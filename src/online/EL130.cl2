00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL130 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/18/95 07:28:16.                 
00007 *                            VMOD=2.055.                          
00008 *                                                                 
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00024 *REMARKS.    TRANSACTION - EX19 - NEW CLAIM SETUP
041002*
041002******************************************************************
041002*                   C H A N G E   L O G
041002*
041002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
041002*-----------------------------------------------------------------
041002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041002* EFFECTIVE    NUMBER
041002*-----------------------------------------------------------------
041002* 041002    2002040200004  SMVA  MAKE SURE CLAIM TYPE IS ALWAYS
041002*                               POPULATED -MOVE PI-LAST-CLAIM-TYPE
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
031405* 031405                   PEMA  ADD PROCESSING FOR NEW CLAM TYP G
050807* 050807    2007020800002  AJRA  AUTO POPULATE I1 ADDRESS FROM CERT
071508* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
012009* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
041309* 041309    2009031600001  AJRA  ADD VIN TO DIAG FOR BENE 55 AND 56
100809* 100809    2009081800004  AJRA  ADD MSG FOR BENE CD 2O - 2V
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020513* 020513  CR2011090100001  PEMA  FORCE ENTRY OF SEX CODE
020613* 020613    2012092400007  AJRA  ADD CAUSAL STATE MESSAGE
042413* 042413    2013042300001  AJRA  FIX SPECIAL MSG WHEN MULTIPLE CERTS
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
040814* 040814    2014030500002  AJRA  ADD ICD CODES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
101917* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
121417* 121417  IR2017121200001  PEMA  Correct assignment of ben period
011118* 011118  CR2016052500002  TANA  Add Message for pre-existing
052918* 052918  CR2018031500002  TANA  Add Message for filing time limit
061418* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
071720* 071720  CR2019112600001  TANA  Remove filing time limit error
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
102901******************************************************************
00025                                                                   
00026      EJECT                                                        
00027  ENVIRONMENT DIVISION.                                            
00028  DATA DIVISION.                                                   
00029  WORKING-STORAGE SECTION.                                         
00030  77  FILLER  PIC X(32)  VALUE '********************************'. 
00031  77  FILLER  PIC X(32)  VALUE '*    EL130 WORKING STORAGE     *'. 
00032  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'. 
052113 77  A1                          PIC S999 COMP-3 VALUE +0.
052113 77  E1                          PIC S999 COMP-3 VALUE +0.
052113 77  s1                          pic s999 comp-3 value +0.
052113 77  s2                          pic s999 comp-3 value +0.
052113 77  P1                          PIC S999 COMP-3 VALUE +0.
052113 77  P2                          PIC S999 COMP-3 VALUE +0.
052113 77  WS-CRIT-PER-RECURRENT       PIC 99    VALUE zeros.
052113 77  WS-MAX-BENEFITS             PIC 99    VALUE ZEROS.
052113 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
052113 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
052113 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
052113 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
052113 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
052113 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
052113     88  ERPDEF-FOUND                 VALUE 'Y'.
052113 77  ws-monthly-benefit          pic s9(11)v99 comp-3 value +0.
052113 77  WS-BENEFITS-PREV-PAID       PIC S9(4)V999 VALUE +0  COMP-3.
       77  ws-dcc-product-code         pic xxx value spaces.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-zero-bens-avail          pic x value ' '.
011118 77  WS-PRE-EXISTING-PER         PIC 99    VALUE ZEROS.
022122 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.


00034      COPY ELCSCTM.
00035                                                                   
00036      COPY ELCSCRTY.
00037      EJECT                                                        
00038  01  WS-DATE-AREA.                                                
00039      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00040      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
00041      05  SAVE-SEND-DT        PIC XX      VALUE LOW-VALUES.        
00042                                                                   
052113 01  ws-dcc-error-line.
052113     05  filler occurs 15.
052113         10  ws-error-no        pic x(4).

00043  01  WS-CRI-SSN-WORK-AREA.                                        
00044      05  WS-CRI-SOC-SEC-NO.                                       
00045          10  WS-SS-REPT-CD       PIC X(5).                        
00046          10  WS-SS-LAST-NAME     PIC X(5).                        
00047          10  WS-SS-INITIAL       PIC X.                           
00048                                                                   
00049      05  WS-CRI-REPORT-CODE.                                      
00050          10  FILLER              PIC X(5).                        
00051          10  WS-CRI-RPT-CD-MOVE  PIC X(5).                        
00052                                                                   
00053      05  WS-CRI-LAST-NAME.                                        
00054          10  WS-CRI-NAME-MOVE    PIC X(5).                        
00055          10  FILLER              PIC X(10).                       
00056                                                                   
090821 01  filler.
090821     05  ws-prev-inc-dt          pic xx value low-values.
090821     05  ws-mob-cert-ind         pic x value ' '.
090821         88  mob-cert        value 'M'.
090821     05  ws-eracct-startbr-ind   pic x  value spaces.
090821         88  eracct-browse-started  value 'Y'.
090821     05  ws-lo-acct-dt           pic xx value low-values.
090821     05  ws-hi-acct-dt           pic xx value low-values.
090821     05  ws-acct-status          pic x value spaces.
090821         88  acct-cancelled          value '3'.
090821     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
090821         88  i-say-STOP            value 'S'.

00057  01  STANDARD-AREAS.                                              
090821     12  er-1679-text        pic x(60) value
090821       '1679-N CONTRACT IS NOT CONTESTABLE'.
090821     12  er-1682-text        pic x(60) value
090821       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
090821     12  er-1683-text        pic x(60) value
090821       '1683-N INC DATE < CERTIFICATE EFF DATE'.
00058      12  SC-ITEM             PIC S9(4)   VALUE +0001      COMP.   
00059      12  GETMAIN-SPACE       PIC X       VALUE SPACES.            
00060      12  MAP-NAME            PIC X(8)    VALUE 'EL130A'.          
00061      12  MAPSET-NAME         PIC X(8)    VALUE 'EL130S'.          
00062      12  TRANS-ID            PIC X(4)    VALUE 'EX19'.            
00063      12  START-TRANS-ID      PIC X(4)    VALUE 'EX58'.            
00064      12  THIS-PGM            PIC X(8)    VALUE 'EL130'.           
00065      12  PGM-NAME            PIC X(8).                            
00066      12  TIME-IN             PIC S9(7).                           
00067      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00068          16  FILLER          PIC X.                               
00069          16  TIME-OUT        PIC 99V99.                           
00070          16  FILLER          PIC X(2).                            
00071      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           
00072      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           
PEMMOD     12  XCTL-114            PIC X(8)    VALUE 'EL114'.           
00073      12  XCTL-126            PIC X(8)    VALUE 'EL126'.           
00074      12  XCTL-127            PIC X(8)    VALUE 'EL127'.           
00075      12  XCTL-727            PIC X(8)    VALUE 'EL727'.           
00076      12  XCTL-131            PIC X(8)    VALUE 'EL131'.           
00077      12  XCTL-132            PIC X(8)    VALUE 'EL132'.           
00078      12  XCTL-141            PIC X(8)    VALUE 'EL141'.           
00079      12  XCTL-150            PIC X(8)    VALUE 'EL150'.           
00080      12  XCTL-157            PIC X(8)    VALUE 'EL157'.           
00081      12  XCTL-650            PIC X(8)    VALUE 'EL650'.           
00082      12  XCTL-659            PIC X(8)    VALUE 'EL659'.           
00083      12  XCTL-6592           PIC X(8)    VALUE 'EL6592'.          
00084      12  XCTL-725            PIC X(8)    VALUE 'EL725'.           
00085      12  LINK-001            PIC X(8)    VALUE 'EL001'.           
00086      12  LINK-004            PIC X(8)    VALUE 'EL004'.           
00087      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
00088      12  LINK-ELRTRM         PIC X(8)    VALUE 'ELRTRM'.          
00089      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.          
00090      12  ELCNTL-DSID         PIC X(8)    VALUE 'ELCNTL'.          
00091      12  ELMSTR-DSID         PIC X(8)    VALUE 'ELMSTR'.          
00092      12  ELMSTR5-DSID        PIC X(8)    VALUE 'ELMSTR5'.         
00093      12  ELTRLR-DSID         PIC X(8)    VALUE 'ELTRLR'.          
00094      12  ERACCT2-DSID        PIC X(8)    VALUE 'ERACCT2'.         
00095      12  ELCERT-DSID         PIC X(8)    VALUE 'ELCERT'.          
00096      12  ELCERT5-DSID        PIC X(8)    VALUE 'ELCERT5'.         
00097      12  ELACTQ-DSID         PIC X(8)    VALUE 'ELACTQ'.          
00098      12  ELARCH-DSID         PIC X(8)    VALUE 'ELARCH'.          
00099      12  ELBENE-DSID         PIC X(8)    VALUE 'ELBENE'.          
00100      12  ERREIN-DSID         PIC X(8)    VALUE 'ERREIN'.          
CIDMOD     12  DLYACTV-DSID        PIC X(8)    VALUE 'DLYACTV'. 
050807     12  ERMAIL-DSID         PIC X(8)    VALUE 'ERMAIL'.
041309     12  ELCRTT-DSID         PIC X(8)    VALUE 'ELCRTT'.
00101      12  FILE-ID             PIC X(8).                            
CIDMOD     12  DLYACTV-LENGTH      PIC S9(04)    COMP     VALUE +25.    
00102      12  ELACTQ-LENGTH       PIC S9(04)    COMP     VALUE +60.    
00103      12  ELARCH-LENGTH       PIC S9(04)    COMP     VALUE +90.    
00104      12  ELCERT-LENGTH       PIC S9(04)    COMP     VALUE +450.   
00105      12  ELMSTR-LENGTH       PIC S9(04)    COMP     VALUE +350.   
00106      12  ELTRLR-LENGTH       PIC S9(04)    COMP     VALUE +200.   
00107      12  ELMSTR-GENERIC-LENGTH PIC S9(4)   VALUE +9      COMP.    
050807     12  ERMAIL-LENGTH       PIC S9(04)    COMP     VALUE +374.
052113     12  ELCRTT-LENGTH       PIC S9(04)    COMP     VALUE +552.
00108                                                                   
00109  01  TERM-CALCULATION-WORK-AREA     COMP-3.                       
00110      12  M                   PIC S9(7)V99           VALUE ZEROS.  
00111      12  L                   PIC S9(7)V99           VALUE ZEROS.  
00112      12  N                   PIC S9(3)              VALUE ZEROS.  
00113      12  N-STORE             PIC S9(3)              VALUE ZEROS.  
00114      12  NV-STORE            PIC S9(3)              VALUE ZEROS.  
00115      12  I                   PIC S99V9(5)           VALUE ZEROS.  
00116      12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.  
00117      12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.  
00118      12  V                   PIC S9(3)V9(14)        VALUE ZEROS.  
00119      12  R                   PIC S9(3)              VALUE ZEROS.  
00120      12  M1                  PIC S9(7)V99           VALUE ZEROS.  
00121      12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.  
00122      12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.  
00123      12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.  
00124      12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.  
00125      12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.  
00126      12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.  
00127      12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.  
00128      12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.  
00129                                                                   
00130  01  TERM-CALC-WORK-AREA.                                         
00131      12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.  
00132      12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.  
00133      12  WS-TERM             PIC S9(3)              VALUE ZEROS.  
00134      12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.  
00135      12  WS-REMAIN           PIC S99                VALUE ZEROS.  
00136      12  V-EXPONENTS.                                             
00137         14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.   
00138      12  V-EX-ONETIME        PIC 9                  VALUE 1.      
00139                                                                   
00140  01  MISC-WORK-AREAS.                                             
PEMMOD     12  SAVE-BENEFICIARY    PIC X(10)   VALUE LOW-VALUES.
00141      12  FILE-SWITCH         PIC X(4)    VALUE SPACES.            
00142      12  CLAIM-SWITCH        PIC X       VALUE 'N'.               
00143      12  QID.                                                     
00144          16  QID-TERM        PIC X(4).                            
00145          16  FILLER          PIC X(4)    VALUE '130A'.            
00146      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.            
00147      12  SAVE-AT-PRIMARY-KEY PIC X(22)   VALUE SPACES.            
00148      12  WS-GROUP-NO.                                             
00149          16  WS-GROUP-NO-1   PIC X(01)   VALUE SPACES.            
00150          16  WS-GROUP-NO-2-6 PIC X(05)   VALUE SPACES.            
00151                                                                   
00152      12  WS-RESPONSE         PIC S9(8)   COMP.                    
00153          88  WS-RESP-NORMAL              VALUE +00.               
00154          88  WS-RESP-ERROR               VALUE +01.               
052113         88  WS-RESP-NOTFND              VALUE +13.               
052113         88  WS-RESP-DUPKEY              VALUE +15.
052113         88  WS-RESP-NOTOPEN             VALUE +19.
052113         88  WS-RESP-ENDFILE             VALUE +20.
00156                                                                   
00157 ***************************************************************   
00158 *                                                             *   
00159 *    QID-MAP-LENGTH MUST BE ADJUSTED EVERY TIME THAT ANY      *   
00160 *    FIELDS ARE ADDED, DELETED, OR THE SIZE OF ANY FIELD      *   
00161 *    CHANGES WITH IN MAP EL130A.                              *   
00162 *                                                             *   
00163 ***************************************************************   
00164                                                                   
00165      12  QID-MAP-LENGTH            PIC S9(4) VALUE +1084 COMP.    
00166      12  WS-SAVE-CLAIM-MASTER.                                    
00167          16  FILLER                    PIC XX.                    
00168          16  WS-SAVE-CLAIM-KEY.                                   
00169              20  WS-SAVE-COMPANY-CD    PIC X.                     
00170              20  WS-SAVE-CARRIER       PIC X.                     
00171              20  WS-SAVE-CLAIM-NO      PIC X(7).                  
00172              20  WS-SAVE-CERT-NO       PIC X(11).                 
00173          16  FILLER                    PIC X(328).                
00174      12  SAVE-CURRENT-DATE         PIC XX.                        
00175      12  SAVE-CONTROL              PIC X(39).                     
00176      12  SAVE-ERACCT-KEY           PIC X(26).                     
00177      12  SAVE-COUNTER              PIC S9(8)   COMP.              
00178      12  SAVE-METHOD               PIC X.                         
00179      12  SAVE-COMPANY-ID           PIC XXX.                       
00180      12  SAVE-COMPANY-CD           PIC X.                         
00181      12  SAVE-JOURNAL-FILE-ID      PIC S9(4)   COMP.              
00182      12  SAVE-CREDIT-USER          PIC X.                         
00183      12  SAVE-CLAIM-USER           PIC X.                         
00184      12  SAVE-LIFE-OVERRIDE-L1     PIC X.                         
00185      12  SAVE-LIFE-OVERRIDE-L2     PIC XX.                        
00186      12  SAVE-LIFE-OVERRIDE-L6     PIC X(6).                      
00187      12  SAVE-LIFE-OVERRIDE-L12    PIC X(12).                     
00188      12  SAVE-AH-OVERRIDE-L1       PIC X.                         
00189      12  SAVE-AH-OVERRIDE-L2       PIC XX.                        
00190      12  SAVE-AH-OVERRIDE-L6       PIC X(6).                      
00191      12  SAVE-AH-OVERRIDE-L12      PIC X(12).                     
00192      12  FILLER                    PIC X.                         
00193 *    12  SAVE-CLAIM-ACCESS-CONTROL PIC X.                         
00194      12  SAVE-CERT-ACCESS-CONTROL  PIC X.                         
00195      12  SAVE-CARRIER-CONTROL-LEVEL    PIC X.                     
00196      12  SAVE-EL127-TO-EL130-CNTRL.                               
00197          16  SAVE-CERT-SELECT-CNT    PIC S9(04) COMP.             
00198          16  SAVE-CERT-PROCESSED     PIC S9(04) COMP.             
00199          16  SAVE-CERT-CONTROLS-EL127 OCCURS 5 TIMES.             
00200              20  SAVE-EL127-CARRIER  PIC X.                       
00201              20  SAVE-EL127-GROUPING PIC X(6).                    
00202              20  SAVE-EL127-STATE    PIC X(2).                    
00203              20  SAVE-EL127-ACCOUNT  PIC X(10).                   
00204              20  SAVE-EL127-CERT-NO  PIC X(11).                   
00205              20  SAVE-EL127-EFF-DT   PIC XX.                      
00206      12  SAVE-EL659-TO-EL130-CNTRL.                               
00207          16  SAVE-EL659-CARRIER    PIC X.                         
00208          16  SAVE-EL659-GROUPING   PIC X(6).                      
00209          16  SAVE-EL659-STATE      PIC XX.                        
00210          16  SAVE-EL659-ACCOUNT    PIC X(10).                     
00211      12  DIVIDE-QUOT               PIC 999.                       
00212      12  DIVIDE-REM                PIC 999.                       
00213      12  WS-CURRENT-MANUAL-RESERVE PIC S9(5)V99 COMP-3.           
00214      12  WS-ITD-ADDITIONAL-RESERVE PIC S9(5)V99 COMP-3.           
00215      12  WS-ITD-PAID-EXPENSE       PIC S9(3)V99 COMP-3.           
00216      12  WS-ITD-CHARGABLE-EXPENSE  PIC S9(5)V99 COMP-3.           
00217      12  WS-INITIAL-MANUAL-RESERVE PIC S9(5)V99 COMP-3.           
00218      12  WS-RESERVE-CONTROLS       PIC X(9).                      
00219      12  WS-EXPENSE-CONTROLS       PIC X(7).                      
00220      12  WS-REIN-TABLE.                                           
00221          16  WS-REIN-1             PIC X            VALUE SPACE.  
00222          16  WS-REIN-2             PIC X            VALUE SPACE.  
00223          16  WS-REIN-3             PIC X            VALUE SPACE.  
00224      12  WS-BENE-CALC-METHOD       PIC X.                         
           12  ws-lf-joint-indicator     pic x.
           12  ws-ah-joint-indicator     pic x.
00225      12  WS-EARNINGS-CALC          PIC X.                         
00226      12  WS-LF-EARNINGS-CALC       PIC X.                         
00227          88 TEX-REG                VALUE '4'.                     
00228          88 NET-PAY                VALUE '5'.                     
00229      12  WS-AH-EARNINGS-CALC       PIC X.                         
00230      12  WS-SPECIAL-CALC-CD        PIC X(01).                     
00231      12  WS-LF-SPECIAL-CALC-CD     PIC X(01).                     
00232      12  WS-AH-SPECIAL-CALC-CD     PIC X(01).                     
00233      12  WS-DIAGNOSIS-DESCRIPT     PIC X(60).                     
040814     12  WS-ICD-CODE-1             PIC X(8).
040814     12  WS-ICD-CODE-2             PIC X(8).
00234      12  WS-CLAIM-NUMBER.                                         
00235          16  WS-CN-PREFIX.                                        
00236              20  WS-CN-PRF-A PIC X.                               
00237              20  WS-CN-PRF-B PIC X.                               
00238          16  WS-CN-NUMBER    PIC 9(5).                            
00239      12  WS-CLAIM-NUMBER-R1 REDEFINES WS-CLAIM-NUMBER PIC 9(7).   
00240      12  WS-CLAIM-SEQU.                                           
00241          16  FILLER          PIC X    VALUE '('.                  
00242          16  WS-CURRENT-SEQU PIC Z9.                              
00243          16  FILLER          PIC X(4) VALUE ' OF '.               
00244          16  WS-OF-SEQU      PIC Z9.                              
00245          16  FILLER          PIC X    VALUE ')'.                  
00246      12  DATE-WORK.                                               
00247        14  FILLER                PIC XX.                          
00248        14  DATE-MM               PIC XX.                          
00249        14  DATE-DD               PIC XX.                          
00250        14  DATE-YY               PIC XX.                          
00251      12  CURR-DATE REDEFINES DATE-WORK.                           
00252          16  CURR-MM.                                             
00253            18  CURR-M1           PIC 9.                           
00254            18  CURR-M2           PIC 9.                           
00255          16  FILLER              PIC X(5).                        
00256          16  CURR-YEAR           PIC 9.                           
00257      12  NUM-WORK   REDEFINES DATE-WORK   PIC 9(8).               
00258      12  SUB                     PIC 99.                          
00259      12  MY-DATE.                                                 
00260          16  MY-MM               PIC XX.                          
00261          16  FILLER              PIC X VALUE '/'.                 
00262          16  MY-YY               PIC XX.                          
00263                                                                   
00264      12  WS-EDIT-BEN-CODE        PIC XX.                          
00265          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'               
00266                                           '90' THRU '99'.         
00267                                                                   
00268      12  WS-NUM-HOLD             PIC S9(4)     VALUE +0  COMP-3.  
00269      12  WS-BEN-HOLD             PIC XX.                          
00270      12  WS-BEN-ALPHA-HOLD.                                       
00271          15  WS-BEN-ALPHA-2      PIC XX.                          
00272          15  WS-BEN-ALPHA-1      PIC X.                           
00273      12  WS-FORM-HOLD            PIC X(12)  VALUE SPACES.         
00274      12  WS-REC-TYPE             PIC X.                           
00275      12  HOLD-LOAN-BAL           PIC 9(7)V99   VALUE ZEROS.       
00276      12  DEEDIT-FIELD            PIC 9(10)V99  VALUE ZEROS.       
00277      12  ARCH-NUMBER             PIC S9(8)   COMP.                
00278      12  WS-REIN-REC-FOUND-SW    PIC X(01)   VALUE ' '.           
00279          88  REIN-REC-NOT-FOUND              VALUE 'N'.           
00280          88  REIN-REC-FOUND                  VALUE 'Y'.           
00281                                                                   
00282      12  WS-REC-FOUND-SW         PIC X(01)   VALUE ' '.           
00283      12  WS-LETTER-SW            PIC X(01)   VALUE 'N'.           
00284                                                                   
CIDMOD                                                                  
CIDMOD 01  CSO-WORK-FIELDS.                                             
CIDMOD     12  WS-BLANK            PIC X       VALUE ' '.               
050807
050807     12  WS-MA-CONTROL-PRIMARY.
050807         16  WS-MA-COMPANY-CD        PIC  X.
050807         16  WS-MA-CARRIER           PIC  X.
050807         16  WS-MA-GROUPING          PIC  X(6).
050807         16  WS-MA-STATE             PIC  XX.
050807         16  WS-MA-ACCOUNT           PIC  X(10).
050807         16  WS-MA-CERT-EFF-DT       PIC  XX.
050807         16  WS-MA-CERT-NO.
050807             20  WS-MA-CERT-PRIME    PIC  X(10).
050807             20  WS-MA-CERT-SFX      PIC  X.
050807
050807     12  WS-INSURED-ADDR-CNT         PIC  S9(1) VALUE +0.
050807     12  WS-NO-INSURED-ADDRESS       PIC  X(01) VALUE 'N'.
071508     12  WS-SOC-SEC-NUMBER.
071508         16  WS-SOC-SEC-NO       PIC 9(9).
071508         16  WS-SOC-SEC-BLANK    PIC X(2).
071508     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
071508         16  WS-SSN-1-3          PIC 9(3).
071508         16  WS-SSN-DASH1        PIC X(1).
071508         16  WS-SSN-4-5          PIC 9(2).
071508         16  WS-SSN-DASH2        PIC X(1).
071508         16  WS-SSN-6-9          PIC 9(4).
041309
041309     12  WS-DIAG-VIN-MSG.
041309         16  FILLER          PIC X(11)  
041309             VALUE 'VIN NUMBER '. 
041309         16  WS-DIAG-VIN     PIC X(17).
061511
061511     12  WS-VERIFY-NOTE      PIC X(34)
061511         VALUE '2ND BENE SSN VERIFICATION REQUIRED'.
020613
020613     12  WS-CAUSAL-NOTE      PIC X(40)
020613         VALUE 'VERIFY IF DX RELATED TO PRIOR CONDITION '.
032514
032514     12  WS-VFY-SSN-NOTE     PIC X(25)
032514         VALUE 'SSN VERIFICATION REQUIRED'.
061418
061418     12  WS-FILING-NOTE     PIC X(30)
061418         VALUE 'FILING TIME LIMIT EXCEEDED'.
CIDMOD                                                                  
00285  01  MAP-DATE-AREAS.                                              
00286      12  WS-EFFDT            PIC XX      VALUE LOW-VALUES.        
00287      12  WS-EXPIRE           PIC XX      VALUE LOW-VALUES.        
00288      12  WS-ADD-ON-DT        PIC XX      VALUE LOW-VALUES.        
00289      12  WS-BIRTHDT          PIC XX      VALUE LOW-VALUES.        
00290      12  WS-INCUR            PIC XX      VALUE LOW-VALUES.        
00291      12  WS-REPORT           PIC XX      VALUE LOW-VALUES.        
00292      12  WS-ESTEND           PIC XX      VALUE LOW-VALUES.        
00293      12  WS-ACVCNDT          PIC XX      VALUE LOW-VALUES.        
00294      12  WS-LCVCNDT          PIC XX      VALUE LOW-VALUES.        
00295      12  WS-TODAY-DT         PIC XX      VALUE LOW-VALUES.        
00296      12  WS-ENTRYDT          PIC XX      VALUE LOW-VALUES.        
00297      12  WS-MANRSV           PIC S9(6)V99  VALUE ZEROS COMP-3.    
00298      12  WS-ACVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.    
00299      12  WS-ACVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.    
00300      12  WS-LCVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.    
00301      12  WS-LCVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.    
00302      12  WS-APR              PIC S999V9999 VALUE ZEROS COMP-3.    
00303      12  WS-PMTFREQ          PIC 99        VALUE ZEROS COMP-3.    
00304      12  WS-ASSOC-CERT-TOTAL PIC S99       VALUE ZEROS.           
00305          88  NO-ASSOCIATED-CERTS           VALUE ZEROS.           
00306      12  WS-ASSOC-CERT-SEQU  PIC S99       VALUE ZEROS.           
00307      12  WS-READNEXT-SWITCH  PIC S99       VALUE +1.              
00308      12  WS-ASSOCIATED-CERTS PIC S9        VALUE ZEROS COMP-3.    
00309      12  ONE-OR-MIN1         PIC S9        VALUE +1    COMP-3.   
00310                                                                   
00311  01  ACCESS-KEYS.                                                 
00312      12  ELACTQ-KEY.                                              
00313          16  ELACTQ-COMPANY-CD       PIC X.                       
00314          16  ELACTQ-CARRIER          PIC X.                       
00315          16  ELACTQ-CLAIM-NO         PIC X(7).                    
00316          16  ELACTQ-CERT-NO          PIC X(11).                   
00317      12  ELMSTR-KEY.                                              
00318          16  MSTR-COMP-CD            PIC X.                       
00319          16  MSTR-CARRIER            PIC X.                       
00320          16  MSTR-CLAIM-NO           PIC X(7).                    
00321          16  MSTR-CERT-NO.                                        
00322              20  MSTR-CERT-NO-PRIME  PIC X(10).                   
00323              20  MSTR-CERT-NO-SUFX   PIC X.                       
00324      12  SAVE-ELMSTR-KEY.                                         
00325          16  SAVE-COMP-CD            PIC X.                       
00326          16  SAVE-CARRIER            PIC X.                       
00327          16  SAVE-CLAIM-NO           PIC X(7).                    
00328          16  SAVE-CERT-NO.                                        
00329              20  SAVE-CERT-NO-PRIME  PIC X(10).                   
00330              20  SAVE-CERT-NO-SUFX   PIC X.                       
00331      12  ELMSTR5-KEY.                                             
00332          16  MSTR5-COMP-CD           PIC X.                       
00333          16  MSTR5-CERT-NO.                                       
00334              20  MSTR5-CERT-NO-PRIME PIC X(10).                   
00335              20  MSTR5-CERT-NO-SUFX  PIC X.                       
00336      12  INCUR-DTE-DUPE-SW           PIC X VALUE 'N'.             
00337          88  NO-CLAIMS-FOR-CERT            VALUE 'N'.             
00338          88  INCURRED-DATE-MATCH           VALUE 'Y'.             
00339          88  CLAIM-RECORD-FOUND            VALUE 'H'.             
00340      12  ELCNTL-KEY.                                              
00341          16  CNTL-COMP-ID    PIC X(3).                            
00342          16  CNTL-REC-TYPE   PIC X.                               
00343          16  CNTL-ACCESS.                                         
00344              20  FILLER      PIC XX.                              
00345              20  CNTL-BENEFIT.                                    
00346                  24  FILLER  PIC X.                               
00347                  24  CNTL-CARRIER PIC X.                          
00348          16  CNTL-SEQ-NO     PIC S9(4)    COMP.                   
00349      12  ELCERT-KEY.                                              
00350          16  CERT-COMP-CD    PIC X.                               
00351          16  CERT-CARRIER    PIC X.                               
00352          16  CERT-GROUPING   PIC X(6).                            
00353          16  CERT-STATE      PIC X(2).                            
00354          16  CERT-ACCOUNT.                                        
00355              20  CERT-ACCOUNT-PREFIX PIC X(4).                    
00356              20  CERT-ACCOUNT-PRIME  PIC X(6).                    
00357          16  CERT-EFF-DT     PIC XX.                              
00358          16  CERT-CERT-NO.                                        
00359              20  CERT-CERT-NO-PRIME  PIC X(10).                   
00360              20  CERT-CERT-NO-SUFX   PIC X.                       
00361      12  ELCERT-KEY-5.                                            
00362          16  CERT-COMP-CD-5      PIC X.                           
00363          16  CERT-CERT-5.                                         
00364              20  CERT-CERT-5-PRIME   PIC X(10).                   
00365              20  CERT-CERT-5-SUFX    PIC X.                       
00366      12  ERACCT-KEY.                                              
00367          16  ACCT-COMP-CD    PIC X.                               
00368          16  ACCT-CARRIER    PIC X.                               
00369          16  ACCT-GROUPING   PIC X(6).                            
00370          16  ACCT-STATE      PIC X(2).                            
00371          16  ACCT-ACCOUNT    PIC X(10).                           
00372          16  ACCT-EXP-DT     PIC XX.                              
00373      12  ELTRLR-KEY.                                              
00374          16  TRLR-COMP-CD    PIC X.                               
00375          16  TRLR-CARRIER    PIC X.                               
00376          16  TRLR-CLAIM-NO   PIC X(7).                            
00377          16  TRLR-CERT-NO.                                        
00378              20  TRLR-CERT-NO-PRIME  PIC X(10).                   
00379              20  TRLR-CERT-NO-SUFX   PIC X.                       
00380          16  TRLR-SEQ-NO     PIC S9(4)   COMP.                    
00381          16  TRLR-TYPE       PIC X.                               
00382      12  ELBENE-KEY.                                              
00383          16  BENE-COMP-CD    PIC X.                               
00384          16  BENE-RCD-TYPE   PIC X.                               
00385          16  BENE-CODE       PIC X(10).                           
00386      12  ERREIN-KEY.                                              
00387          16  REIN-COMP-CD        PIC X(01).                       
00388          16  REIN-TYPE           PIC X(01).                       
00389          16  REIN-TABLE-CO.                                       
00390              20  REIN-CODE-1     PIC X(01).                       
00391              20  REIN-CODE-2     PIC X(01).                       
00392              20  REIN-CODE-3     PIC X(01).                       
00393          16  FILLER              PIC X(03).                       
041309     12  ELCRTT-KEY.                                              
041309         16  CTRLR-COMP-CD       PIC X.                               
041309         16  CTRLR-CARRIER       PIC X.                               
041309         16  CTRLR-GROUPING      PIC X(6).                            
041309         16  CTRLR-STATE         PIC X(2).                            
041309         16  CTRLR-ACCOUNT       PIC X(10).
041309         16  CTRLR-EFF-DT        PIC XX.                              
041309         16  CTRLR-CERT-NO       PIC X(11).  
041309         16  CTRLR-REC-TYPE      PIC X.

       01  ws-acct-found-sw            pic x value ' '.
           88  acct-found                value 'Y'.
052113 01  ERPDEF-KEY-SAVE             PIC X(18).
052113 01  ERPDEF-KEY.
052113     12  ERPDEF-COMPANY-CD       PIC X.
052113     12  ERPDEF-STATE            PIC XX.
052113     12  ERPDEF-PROD-CD          PIC XXX.
052113     12  F                       PIC X(7).
052113     12  ERPDEF-BEN-TYPE         PIC X.
052113     12  ERPDEF-BEN-CODE         PIC XX.
052113     12  ERPDEF-EXP-DT           PIC XX.

00395  01  ERROR-MESSAGES.                                              
00396      12  ER-0000                 PIC X(4)  VALUE '0000'.          
00397      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00398      12  ER-0019                 PIC X(4)  VALUE '0019'.          
00399      12  ER-0023                 PIC X(4)  VALUE '0023'.          
00400      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00401      12  ER-0033                 PIC X(4)  VALUE '0033'.          
00402      12  ER-0068                 PIC X(4)  VALUE '0068'.          
00403      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00404      12  ER-0194                 PIC X(4)  VALUE '0194'.          
00405      12  ER-0202                 PIC X(4)  VALUE '0202'.          
00406      12  ER-0203                 PIC X(4)  VALUE '0203'.          
00407      12  ER-0204                 PIC X(4)  VALUE '0204'.          
00408      12  ER-0205                 PIC X(4)  VALUE '0205'.          
00409      12  ER-0206                 PIC X(4)  VALUE '0206'.          
00410      12  ER-0207                 PIC X(4)  VALUE '0207'.          
00411      12  ER-0213                 PIC X(4)  VALUE '0213'.          
00412      12  ER-0214                 PIC X(4)  VALUE '0214'.          
00413      12  ER-0217                 PIC X(4)  VALUE '0217'.          
00414      12  ER-0219                 PIC X(4)  VALUE '0219'.          
00415      12  ER-0220                 PIC X(4)  VALUE '0220'.          
00416      12  ER-0221                 PIC X(4)  VALUE '0221'.          
00417      12  ER-0222                 PIC X(4)  VALUE '0222'.          
00418      12  ER-0223                 PIC X(4)  VALUE '0223'.          
00419      12  ER-0224                 PIC X(4)  VALUE '0224'.          
00420      12  ER-0225                 PIC X(4)  VALUE '0225'.          
00421      12  ER-0226                 PIC X(4)  VALUE '0226'.          
00422      12  ER-0227                 PIC X(4)  VALUE '0227'.          
00423      12  ER-0229                 PIC X(4)  VALUE '0229'.          
00424      12  ER-0230                 PIC X(4)  VALUE '0230'.          
00425      12  ER-0231                 PIC X(4)  VALUE '0231'.          
00426      12  ER-0232                 PIC X(4)  VALUE '0232'.          
00427      12  ER-0233                 PIC X(4)  VALUE '0233'.          
00428      12  ER-0234                 PIC X(4)  VALUE '0234'.          
00429      12  ER-0235                 PIC X(4)  VALUE '0235'.          
00430      12  ER-0236                 PIC X(4)  VALUE '0236'.          
00431      12  ER-0237                 PIC X(4)  VALUE '0237'.          
00432      12  ER-0238                 PIC X(4)  VALUE '0238'.          
00433      12  ER-0239                 PIC X(4)  VALUE '0239'.          
00434      12  ER-0240                 PIC X(4)  VALUE '0240'.          
00435      12  ER-0241                 PIC X(4)  VALUE '0241'.          
00436      12  ER-0243                 PIC X(4)  VALUE '0243'.          
00437      12  ER-0244                 PIC X(4)  VALUE '0244'.          
00438      12  ER-0245                 PIC X(4)  VALUE '0245'.          
00439      12  ER-0246                 PIC X(4)  VALUE '0246'.          
00440      12  ER-0247                 PIC X(4)  VALUE '0247'.          
00441      12  ER-0248                 PIC X(4)  VALUE '0248'.          
00442      12  ER-0250                 PIC X(4)  VALUE '0250'.          
00443      12  ER-0251                 PIC X(4)  VALUE '0251'.          
00444      12  ER-0252                 PIC X(4)  VALUE '0252'.          
00445      12  ER-0253                 PIC X(4)  VALUE '0253'.          
00446      12  ER-0254                 PIC X(4)  VALUE '0254'.          
00447      12  ER-0257                 PIC X(4)  VALUE '0257'.          
00448      12  ER-0258                 PIC X(4)  VALUE '0258'.          
00449      12  ER-0260                 PIC X(4)  VALUE '0260'.          
00450      12  ER-0261                 PIC X(4)  VALUE '0261'.          
00451      12  ER-0263                 PIC X(4)  VALUE '0263'.          
00452      12  ER-0264                 PIC X(4)  VALUE '0264'.          
00453      12  ER-0265                 PIC X(4)  VALUE '0265'.          
00454      12  ER-0266                 PIC X(4)  VALUE '0266'.          
00455      12  ER-0267                 PIC X(4)  VALUE '0267'.          
00456      12  ER-0334                 PIC X(4)  VALUE '0334'.          
00457      12  ER-0337                 PIC X(4)  VALUE '0337'.          
00458      12  ER-0412                 PIC X(4)  VALUE '0412'.          
00459      12  ER-0413                 PIC X(4)  VALUE '0413'.          
00460      12  ER-0428                 PIC X(4)  VALUE '0428'.          
00461      12  ER-0429                 PIC X(4)  VALUE '0429'.          
00462      12  ER-0430                 PIC X(4)  VALUE '0430'.          
00463      12  ER-0433                 PIC X(4)  VALUE '0433'.          
00464      12  ER-0434                 PIC X(4)  VALUE '0434'.          
00465      12  ER-0458                 PIC X(4)  VALUE '0458'.          
00466      12  ER-0489                 PIC X(4)  VALUE '0489'.          
00467      12  ER-0509                 PIC X(4)  VALUE '0509'.          
00468      12  ER-0510                 PIC X(4)  VALUE '0510'.          
00469      12  ER-0511                 PIC X(4)  VALUE '0511'.          
00470      12  ER-0512                 PIC X(4)  VALUE '0512'.          
00471      12  ER-0516                 PIC X(4)  VALUE '0516'.          
00472      12  ER-0517                 PIC X(4)  VALUE '0517'.          
00473      12  ER-0518                 PIC X(4)  VALUE '0518'.          
00474      12  ER-0519                 PIC X(4)  VALUE '0519'.          
00475      12  ER-0520                 PIC X(4)  VALUE '0520'.          
00476      12  ER-0521                 PIC X(4)  VALUE '0521'.          
00477      12  ER-0522                 PIC X(4)  VALUE '0522'.          
00478      12  ER-0523                 PIC X(4)  VALUE '0523'.          
00479      12  ER-0544                 PIC X(4)  VALUE '0544'.          
00480      12  ER-0546                 PIC X(4)  VALUE '0546'.          
00481      12  ER-0558                 PIC X(4)  VALUE '0558'.          
00482      12  ER-0560                 PIC X(4)  VALUE '0560'.          
00483      12  ER-0562                 PIC X(4)  VALUE '0562'.          
00484      12  ER-0565                 PIC X(4)  VALUE '0565'.          
00485      12  ER-0569                 PIC X(4)  VALUE '0569'.          
00486      12  ER-0693                 PIC X(4)  VALUE '0693'.          
00487      12  ER-0715                 PIC X(4)  VALUE '0715'.          
00488      12  ER-0840                 PIC X(4)  VALUE '0840'.          
00489      12  ER-0841                 PIC X(4)  VALUE '0841'.          
00490      12  ER-0842                 PIC X(4)  VALUE '0842'.          
00491      12  ER-0843                 PIC X(4)  VALUE '0843'.  
100809     12  ER-0878                 PIC X(4)  VALUE '0878'.
071508     12  ER-0887                 PIC X(4)  VALUE '0887'.        
040814     12  ER-0992                 PIC X(4)  VALUE '0992'.
052113     12  ER-1651                 PIC X(4)  VALUE '1651'.
052113     12  ER-1652                 PIC X(4)  VALUE '1652'.
052113     12  ER-1653                 PIC X(4)  VALUE '1653'.
052113     12  ER-1654                 PIC X(4)  VALUE '1654'.
052113     12  ER-1655                 PIC X(4)  VALUE '1655'.
052113     12  er-1659                 pic x(4)  value '1659'.
052113     12  er-1660                 pic x(4)  value '1660'.
052113     12  er-1661                 pic x(4)  value '1661'.
052113     12  er-1671                 pic x(4)  value '1671'.
052113     12  er-1672                 pic x(4)  value '1672'.
052113     12  er-1673                 pic x(4)  value '1673'.
           12  er-1675                 pic x(4)  value '1675'.
           12  er-1676                 pic x(4)  value '1676'.
011118     12  ER-1677                 PIC X(4)  VALUE '1677'.
101917     12  ER-1679                 PIC X(4)  VALUE '1679'.
090821     12  er-1682                 pic x(4)  value '1682'.
090821     12  er-1683                 pic x(4)  value '1683'.
00492      12  ER-2280                 PIC X(4)  VALUE '2280'.          
00493      12  ER-2370                 PIC X(4)  VALUE '2370'.          
00494      12  ER-2371                 PIC X(4)  VALUE '2371'.          
00495      12  ER-2378                 PIC X(4)  VALUE '2378'.          
00496      12  ER-2380                 PIC X(4)  VALUE '2380'.          
00497      12  ER-2566                 PIC X(4)  VALUE '2566'.          
00498      12  ER-2848                 PIC X(4)  VALUE '2848'.          
050807     12  ER-3548                 PIC X(4)  VALUE '3548'.          
00499      12  ER-7008                 PIC X(4)  VALUE '7008'.          
00500      12  ER-7319                 PIC X(4)  VALUE '7319'.          
00501      12  ER-7321                 PIC X(4)  VALUE '7321'.          
00502      12  ER-7352                 PIC X(4)  VALUE '7352'.          
00503      12  ER-7353                 PIC X(4)  VALUE '7353'.          
052918     12  ER-7572                 PIC X(4)  VALUE '7572'.
061511     12  ER-7575                 PIC X(4)  VALUE '7575'.
020613     12  ER-7577                 PIC X(4)  VALUE '7577'.
032514     12  ER-7582                 PIC X(4)  VALUE '7582'.
00504      12  ER-7634                 PIC X(4)  VALUE '7634'.          
00505      12  ER-7635                 PIC X(4)  VALUE '7635'.          
00506      12  ER-7636                 PIC X(4)  VALUE '7636'.          
00507      12  ER-7637                 PIC X(4)  VALUE '7637'.          
00508      12  ER-7638                 PIC X(4)  VALUE '7638'.          
00509      12  ER-7639                 PIC X(4)  VALUE '7639'.          
00510      12  ER-7640                 PIC X(4)  VALUE '7640'.          
00511      12  ER-7641                 PIC X(4)  VALUE '7641'.          
00512      12  ER-7642                 PIC X(4)  VALUE '7642'.          
00513      12  ER-7643                 PIC X(4)  VALUE '7643'.          
00514      12  ER-7646                 PIC X(4)  VALUE '7646'.          
00515      12  ER-7647                 PIC X(4)  VALUE '7647'.          
00516      12  ER-7648                 PIC X(4)  VALUE '7648'.          
00517      12  ER-7649                 PIC X(4)  VALUE '7649'.          
00518      12  ER-7651                 PIC X(4)  VALUE '7651'.          
00519      12  ER-7670                 PIC X(4)  VALUE '7670'.          
00520      12  ER-7671                 PIC X(4)  VALUE '7671'.          
00521      12  ER-7686                 PIC X(4)  VALUE '7686'.          
00522      12  ER-7688                 PIC X(4)  VALUE '7688'.          
00523      12  ER-7689                 PIC X(4)  VALUE '7689'.          
00524      12  ER-7691                 PIC X(4)  VALUE '7691'.

110921 01  work-flow-pass-area.
110921     05  pa-rec-type             pic x(4).
110921     05  pa-company-id           pic xxx.
110921     05  pa-rest-of-record       pic x(600).

052113     COPY ERCPDEF.

00526      COPY ELCLNKLT.
00527      EJECT                                                        
00528      COPY ELCINTF.
00529      12  PI-REDEF     REDEFINES PI-PROGRAM-WORK-AREA.             
00530          16  PI-MSTR-DT               PIC XX.                     
00531          16  PI-MSTR-WHEN             PIC S9(6)   COMP-3.         
00532          16  PI-TRLR-DT               PIC XX.                     
00533          16  PI-TRLR-WHEN             PIC S9(6)   COMP-3.         
00534          16  PI-LAST-CLAIM            PIC X(7).                   
00535          16  PI-LAST-CARR             PIC X.                      
00536          16  PI-LAST-CERT.                                        
00537              20  PI-LAST-CERT-PRIME   PIC X(10).                  
00538              20  PI-LAST-CERT-SUFX    PIC X.                      
00539          16  PI-LAST-CLAIM-TYPE       PIC X.                      
00540          16  PI-CURSOR                PIC S9(4)   COMP.           
00541          16  PI-SAVE-CERT.                                        
00542              20  PI-SAVE-CERT-PRIME   PIC X(10).                  
00543              20  PI-SAVE-CERT-SUFX    PIC X.                      
00544          16  PI-SAVE-EFFDT            PIC X(2).                   
00545          16  PI-SAVE-ACCOUNT          PIC X(10).                  
00546          16  PI-INCURR-SW             PIC X.                      
00547          16  PI-PRT-OPT               PIC X.                      
00548          16  PI-ALT-PRT               PIC X(4).                   
00549          16  PI-CLAIM-DELETED-SWITCH  PIC X.                      
00550              88  CLAIM-DELETED-BY-EL131   VALUE 'D'.              
00551          16  FILLER                   PIC X(272).                 
00552          16  PI-EL127-TO-EL130-CNTRL.                             
00553              20  PI-CERT-SELECT-CNT          PIC S9(04) COMP.     
00554              20  PI-CERT-PROCESSED           PIC S9(04) COMP.     
00555              20  PI-CERT-CONTROLS-EL127  OCCURS 5 TIMES.          
00556                  24  PI-EL127-CARRIER        PIC X.               
00557                  24  PI-EL127-GROUPING       PIC X(6).            
00558                  24  PI-EL127-STATE          PIC X(2).            
00559                  24  PI-EL127-ACCOUNT        PIC X(10).           
00560                  24  PI-EL127-CERT-NO.                            
00561                      28  PI-EL127-CERT-PRIME PIC X(10).           
00562                      28  PI-EL127-CERT-SUFX  PIC X.               
00563                  24  PI-EL127-EFF-DT         PIC XX.              
00564 *        16  FILLER                          PIC X(50).           
00565          16  FILLER                          PIC X(31).           
00566          16  PI-EL659-TO-EL130-CNTRL.                             
00567              20  PI-EL659-CARRIER            PIC X.               
00568              20  PI-EL659-GROUPING           PIC X(6).            
00569              20  PI-EL659-STATE              PIC XX.              
00570              20  PI-EL659-ACCOUNT            PIC X(10).           
00571                                                                   
00572          16  FILLER                          PIC X(19).           
00573                                                                   
00574          16  PI-SAVE-INCUR-DT                PIC X(02).           
00575          16  PI-LETTER-ERROR-CODE            PIC 9(04).           
061511*         16  FILLER                          PIC X(65).           
061511         16  PI-ST-VFY-2ND-BENE              PIC X.
020613         16  PI-ST-CAUSAL-STATE              PIC X.
052113         16  pi-dcc-max-benefits             pic s999 comp-3.
052113         16  pi-dcc-max-amt                  pic s9(9)v99 comp-3.
052113         16  pi-emi-sub                      pic 99.
052113         16  FILLER                          PIC X(53).
00577                                                                   
00578      EJECT                                                        
00579      COPY EL130S.
00580      EJECT                                                        
00581      COPY ELCDATE.
00582      EJECT                                                        
00583      COPY ELCCALC.
00584      EJECT                                                        
00585      COPY ELCLOGOF.
00586      EJECT                                                        
00587      COPY ELCATTR.
00588      EJECT                                                        
00589      COPY ELCEMIB.
00590      EJECT                                                        
00591      COPY ELCJPFX.
00592          PIC X(512).                                              
00593      EJECT                                                        
00594      COPY ELCAID.
00595                                                                   
00596  01  FILLER    REDEFINES DFHAID.                                  
00597      12  FILLER              PIC X(8).                            
00598      12  PF-VALUES           PIC X       OCCURS 12.               
00599                                                                   
00600      EJECT                                                        
00601  LINKAGE SECTION.
00602                                                                   
00603  01  DFHCOMMAREA             PIC X(1024).                         
00604                                                                   
00605 *01 PARMLIST .                                                    
00606 *    02  FILLER              PIC S9(8)   COMP.                    
00607 *    02  ELMSTR-POINTER      PIC S9(8)   COMP.                    
00608 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
00609 *    02  ELCERT-POINTER      PIC S9(8)   COMP.                    
00610 *    02  ERACCT-POINTER      PIC S9(8)   COMP.                    
00611 *    02  ELTRLR-POINTER      PIC S9(8)   COMP.                    
00612 *    02  ELBENE-POINTER      PIC S9(8)   COMP.                    
00613 *    02  ELACTQ-POINTER      PIC S9(8)   COMP.                    
00614 *    02  ELARCH-POINTER      PIC S9(8)   COMP.                    
00615 *    02  ERREIN-POINTER      PIC S9(8)   COMP.                    
00616      EJECT                                                        
00617      COPY ELCMSTR.
00618      EJECT                                                        
00619      COPY ELCCNTL.
00620      EJECT                                                        
00621      COPY ELCCERT.
00622      EJECT                                                        
00623      COPY ERCACCT.
00624      EJECT                                                        
00625      COPY ELCTRLR.
00626      EJECT                                                        
00627      COPY ELCBENE.
00628      EJECT                                                        
CIDMOD     COPY ELCDAR.
CIDMOD     EJECT                                                        
00629      COPY ELCACTQ.
00630      EJECT                                                        
00631      COPY ELCARCH.
00632      EJECT                                                        
00633      COPY ERCREIN.
00634      EJECT
050807     COPY ERCMAIL.
050807     EJECT
041309     COPY ELCCRTT.
041309     EJECT
00634
00635  PROCEDURE DIVISION.
00636      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00637      MOVE '5'                    TO DC-OPTION-CODE.               
00638      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
00639      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   
00640      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               
00641                                                                   
00642      IF EIBCALEN = ZERO                                           
00643          GO TO 8800-UNAUTHORIZED-ACCESS
041002     END-IF.
00644                                                                   
00645      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 
00646      MOVE EIBTRMID               TO QID-TERM.                     
00647      MOVE +2                     TO EMI-NUMBER-OF-LINES
052113     move spaces                 to emi-error-lines
00648      MOVE '2'                    TO EMI-SWITCH2.                  
00649      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         
00650      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           
00651                                                                   
           EXEC CICS HANDLE CONDITION
00653          QIDERR     (0100-TEST-ENTRY)
00654          MAPFAIL    (8100-SEND-INITIAL-MAP)
00655          PGMIDERR   (9600-PGMID-ERROR)
00656          TERMIDERR  (8820-TERM-ERROR)
00657          TRANSIDERR (8830-TRAN-ERROR)
00658          ERROR      (9990-ABEND)
00659      END-EXEC.
00660                                                                   
00661      IF PI-RETURN-TO-PROGRAM = THIS-PGM  OR                       
00662         PI-CALLING-PROGRAM   = XCTL-6592                          
00663          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM                 
00664      ELSE                                                         
00665          MOVE SPACES             TO RETURNED-FROM
041002     END-IF.
00666                                                                   
00667      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00668          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00669              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00670              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00671              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00672              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00673              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00674              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00675              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00676              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00677          ELSE                                                     
00678              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00679              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00680              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00681              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00682              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00683              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00684              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00685              MOVE SPACES               TO PI-SAVED-PROGRAM-6
041002         END-IF
041002     END-IF.
00686                                                                   
00687      IF EIBTRNID = TRANS-ID                                       
00688          IF EIBAID = DFHCLEAR                                     
00689              GO TO 9400-CLEAR                                     
00690          ELSE                                                     
00691              GO TO 0200-RECEIVE
041002         END-IF
041002     END-IF.
00692                                                                   
00693      IF RETURNED-FROM NOT = SPACES                                
00694          GO TO 0600-RECOVER-TEMP-STORAGE
041002     END-IF.
00695                                                                   
00696      MOVE LOW-VALUES TO EL130AO.                                  
052113     move zeros to pi-dcc-max-amt
052113                   pi-dcc-max-benefits
00697                                                                   
00698      
           EXEC CICS DELETEQ TS
00699           QUEUE   (QID)
00700      END-EXEC.
00701                                                                   
00702  0100-TEST-ENTRY.                                                 
00703      IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727               
00704          GO TO 0650-FROM-EL127                                    
00705      ELSE                                                         
00706          MOVE ZEROS              TO PI-CERT-SELECT-CNT            
00707                                     PI-CERT-PROCESSED             
00708                                     PI-LETTER-ERROR-CODE
041002     END-IF.
00709                                                                   
00710      IF PI-RETURN-TO-PROGRAM = XCTL-132                           
00711          GO TO 0640-FROM-EL132
041002     END-IF.
00712                                                                   
00713      GO TO 8100-SEND-INITIAL-MAP.                                 
00714                                                                   
00715      EJECT                                                        
00716  0200-RECEIVE.                                                    
00717      MOVE LOW-VALUES             TO EL130AI.                      
00718                                                                   
00719      IF PI-PROCESSOR-ID = 'LGXX'                                  
041002*        NEXT SENTENCE
041002         CONTINUE
00721      ELSE                                                         
               EXEC CICS READQ TS
00723              QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00724              INTO    (SECURITY-CONTROL)
00725              LENGTH  (SC-COMM-LENGTH)
00726              ITEM    (SC-ITEM)
00727          END-EXEC
00728          MOVE SC-CLAIMS-DISPLAY (1)   TO  PI-DISPLAY-CAP          
00729          MOVE SC-CLAIMS-UPDATE  (1)   TO  PI-MODIFY-CAP           
00730          IF NOT DISPLAY-CAP                                       
00731              MOVE 'READ'              TO  SM-READ                 
00732              PERFORM 9995-SECURITY-VIOLATION                      
00733              MOVE ER-0070             TO  EMI-ERROR               
00734              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00735              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
00736                                                                   
00737      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00738          MOVE ER-7008 TO EMI-ERROR                                
00739          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00740          MOVE -1 TO MAINTL                                        
00741          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00742                                                                   
00743      
           EXEC CICS RECEIVE
00744           MAP     (MAP-NAME)
00745           MAPSET  (MAPSET-NAME)
00746           INTO    (EL130AI)
00747      END-EXEC.
00748                                                                   
00749      IF NOT PI-NO-CARRIER-SECURITY                                
00750         MOVE +1                  TO CLMCARRL                      
00751         MOVE PI-CARRIER-SECURITY TO CLMCARRI
041002     END-IF.
00752                                                                   
00753      IF ENTERPFL = ZERO                                           
00754          GO TO 0300-CHECK-PFKEYS
041002     END-IF.
00755                                                                   
00756      IF EIBAID NOT = DFHENTER                                     
00757          MOVE ER-0004            TO EMI-ERROR                     
00758          GO TO 0320-INPUT-ERROR
041002     END-IF.
00759                                                                   
00760      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)   
00761          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      
00762      ELSE                                                         
00763          MOVE ER-0029            TO EMI-ERROR                     
00764          GO TO 0320-INPUT-ERROR
041002     END-IF.
00765                                                                   
00766  0300-CHECK-PFKEYS.                                               
00767      IF EIBAID = DFHPF23                                          
00768          GO TO 8810-PF23
041002     END-IF.
00769                                                                   
00770      IF EIBAID = DFHPF24                                          
00771          GO TO 9200-RETURN-MAIN-MENU
041002     END-IF.
00772                                                                   
00773      IF EIBAID = DFHPF12                                          
00774          GO TO 9500-PF12
041002     END-IF.
00775                                                                   
00776      IF EIBAID = DFHPF3                                           
00777          MOVE SPACES              TO MAINTI
00778          MOVE ZEROS               TO MAINTL
00779          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00780          MOVE XCTL-127            TO PGM-NAME
00781          GO TO 9300-XCTL
041002     END-IF.
00782                                                                   
00783      IF EIBAID = DFHPF4                                           
00784          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT          
00785          MOVE XCTL-132            TO PGM-NAME                     
00786          GO TO 9300-XCTL
041002     END-IF.
00787                                                                   
00788      IF EIBAID = DFHPF13                                          
00789          IF PI-ACCESS-TO-BOTH-SYSTEMS                             
00790              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT      
00791              MOVE PI-CARRIER          TO  PI-CR-CARRIER           
00792              MOVE PI-GROUPING         TO  PI-CR-GROUPING          
00793              MOVE PI-STATE            TO  PI-CR-STATE             
00794              MOVE PI-ACCOUNT          TO  PI-CR-ACCOUNT           
00795              MOVE XCTL-650            TO  PGM-NAME                
00796              GO TO 9300-XCTL                                      
00797          ELSE                                                     
00798              MOVE -1                  TO  ENTERPFL                
00799              MOVE ER-2566             TO  EMI-ERROR               
00800              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00801              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
00802                                                                   
00803      IF EIBAID = DFHPF5                                           
00804          IF PI-CERT-PROCESSED LESS THAN PI-CERT-SELECT-CNT
00805              ADD +1 TO PI-CERT-PROCESSED
00806              GO TO 0650-FROM-EL127
00807          ELSE
00808              MOVE ER-7686         TO EMI-ERROR
00809              GO TO 0320-INPUT-ERROR
041002         END-IF
041002     END-IF.
00810                                                                   
00811      IF EIBAID = DFHPF7 OR DFHPF8 OR DFHPF9                       
00812          GO TO 0800-TEST-PREVIOUS-CONTROL
041002     END-IF.
00813                                                                   
00814      IF PI-HAS-CLAS-IC-CRDTCRD                                    
00815          IF EIBAID = DFHPF10
00816              MOVE XCTL-725          TO PGM-NAME
00817              GO TO 9300-XCTL
041002         END-IF
041002     END-IF.
00818                                                                   
00819      IF EIBAID = DFHPF14                                          
00820          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00821          MOVE SPACES              TO PI-EL659-TO-EL130-CNTRL
00822          MOVE XCTL-659            TO PGM-NAME
00823          GO TO 9300-XCTL
041002     END-IF.
00824                                                                   
PEMMOD     IF EIBAID = DFHPF15                                          
PEMMOD         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
PEMMOD         MOVE XCTL-114            TO PGM-NAME
PEMMOD         GO TO 9300-XCTL
041002     END-IF.
PEMMOD                                                                  

052113*    IF EIBAID = DFHPF11
052113*       GO TO 0600-RECOVER-TEMP-STORAGE
052113*    END-IF

00825      IF EIBAID = DFHENTER OR DFHPF6 OR DFHPF11                    
00826          GO TO 0330-EDIT-DATA
041002     END-IF.
00827                                                                   
00828      MOVE ER-0029                TO EMI-ERROR.                    
00829                                                                   
00830  0320-INPUT-ERROR.                                                
00831      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00832                                                                   
00833      MOVE AL-UNBON               TO ENTERPFA.                     
00834                                                                   
00835      IF ENTERPFL = ZERO                                           
00836          MOVE -1                 TO MAINTL                        
00837      ELSE                                                         
00838          MOVE -1                 TO ENTERPFL
041002     END-IF.
00839                                                                   
00840      GO TO 8200-SEND-DATAONLY.                                    
00841                                                                   
00842      EJECT                                                        
00843  0330-EDIT-DATA.                                                  
00844                                                                   
00845      IF MAINTI = 'S'                                              
00846          IF PRTOPTL GREATER THAN 0                                
00847              IF PRTOPTI NOT = 'N' AND 'L'                         
00848                  MOVE AL-UABON   TO  PRTOPTA                      
00849                  MOVE -1         TO  PRTOPTL                      
00850                  MOVE ER-0334    TO  EMI-ERROR                    
00851                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00852                  GO TO 8200-SEND-DATAONLY                         
00853              ELSE                                                 
00854                  IF PRTOPTI = 'L'                                 
00855                      GO TO 0400-CREATE-ELACTQ                     
00856                  ELSE                                             
00857                      GO TO 0480-PRINT-NOW
041002                 END-IF
041002             END-IF
00858          ELSE                                                     
00859              GO TO 1000-SHOW-CLAIM
041002         END-IF
041002     END-IF.
00860                                                                   
00861      IF NOT MODIFY-CAP                                            
00862          MOVE 'UPDATE'       TO  SM-READ                          
00863          PERFORM 9995-SECURITY-VIOLATION                          
00864          MOVE ER-0070        TO  EMI-ERROR                        
00865          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00866          GO TO 8100-SEND-INITIAL-MAP
041002     END-IF.
00867                                                                   
00868      IF MAINTI = 'D'                                              
00869          GO TO 2000-DELETE-CLAIM
041002     END-IF.
00870                                                                   
00871      IF MAINTI = 'A'                                              
00872          GO TO 3000-ADD-CLAIM
041002     END-IF.
00873                                                                   
00874      IF MAINTI = 'I'                                              
00875          GO TO 5000-INCURRED-CHANGE
041002     END-IF.
00876                                                                   
00877      MOVE ER-0023                TO EMI-ERROR.                    
00878      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00879      MOVE -1                     TO MAINTL.                       
00880      MOVE AL-UABON               TO MAINTA.                       
00881                                                                   
00882      GO TO 8200-SEND-DATAONLY.                                    
00883                                                                   
00884      EJECT                                                        
00885  0400-CREATE-ELACTQ.                                              
00886      IF CLMNOI   = PI-LAST-CLAIM      AND                         
00887         CLMCARRI = PI-LAST-CARR       AND                         
00888         CERTNOI  = PI-LAST-CERT-PRIME AND                         
00889         SUFXI    = PI-LAST-CERT-SUFX                              
00890          MOVE PI-LAST-CLAIM      TO PI-CLAIM-NO                   
00891          MOVE PI-LAST-CARR       TO PI-CARRIER                    
00892          MOVE PI-LAST-CERT       TO PI-CERT-NO                    
00893      ELSE                                                         
00894          MOVE ER-0207            TO EMI-ERROR                     
00895          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00896          MOVE -1                 TO MAINTL                        
00897          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00898                                                                   
00899      IF (NOT PI-NO-CARRIER-SECURITY OR                            
00900          NOT PI-NO-ACCOUNT-SECURITY)                              
00901          MOVE AL-UABON      TO  PRTOPTA
00902          MOVE -1            TO  PRTOPTL
00903          MOVE ER-2378       TO  EMI-ERROR
00904          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00905          GO TO 8200-SEND-DATAONLY
041002     END-IF.
00906                                                                   
00907      MOVE PI-COMPANY-CD      TO ELACTQ-COMPANY-CD.                
00908      MOVE PI-LAST-CARR       TO ELACTQ-CARRIER.                   
00909      MOVE PI-LAST-CLAIM      TO ELACTQ-CLAIM-NO.                  
00910      MOVE PI-LAST-CERT       TO ELACTQ-CERT-NO.                   
00911                                                                   
00912      
           EXEC CICS HANDLE CONDITION
00913           NOTFND     (0450-ADD-ELACTQ-RECORD)
00914      END-EXEC.
00915                                                                   
00916      
           EXEC CICS READ
00917           UPDATE
00918           DATASET    (ELACTQ-DSID)
00919           SET        (ADDRESS OF ACTIVITY-QUE)
00920           RIDFLD     (ELACTQ-KEY)
00921      END-EXEC.
00922                                                                   
00923      MOVE '2'         TO AQ-PENDING-STATUS-FLAG.                  
00924      MOVE +130        TO AQ-LAST-UPDATED-BY.                      
00925                                                                   
00926      
           EXEC CICS REWRITE
00927           DATASET     (ELACTQ-DSID)
00928           FROM        (ACTIVITY-QUE)
00929      END-EXEC.
00930                                                                   
00931      GO TO 0470-STATUS-FINISH.                                    
00932                                                                   
00933  0450-ADD-ELACTQ-RECORD.                                          
00934      
           EXEC CICS GETMAIN
00935          SET     (ADDRESS OF ACTIVITY-QUE)
00936          LENGTH  (ELACTQ-LENGTH)
00937          INITIMG (GETMAIN-SPACE)
00938      END-EXEC.
00939                                                                   
00940      MOVE 'AQ'              TO AQ-RECORD-ID.                      
00941      MOVE ELACTQ-KEY        TO AQ-CONTROL-PRIMARY.                
00942      MOVE +0                TO AQ-PAYMENT-COUNTER                 
00943                                AQ-PMT-UNAPPROVED-COUNT.           
00944      MOVE LOW-VALUES        TO AQ-RESEND-DATE                     
00945                                AQ-FOLLOWUP-DATE.                  
00946      MOVE '2'               TO AQ-PENDING-STATUS-FLAG.            
00947      MOVE +130              TO AQ-LAST-UPDATED-BY.                
00948                                                                   
00949      
           EXEC CICS WRITE
00950           DATASET     (ELACTQ-DSID)
00951           FROM        (ACTIVITY-QUE)
00952           RIDFLD      (ELACTQ-KEY)
00953      END-EXEC.
00954                                                                   
00955  0470-STATUS-FINISH.                                              
00956      MOVE AL-UANOF               TO  PRTOPTA                      
00957                                      ALTPRTA.                     
00958                                                                   
00959      MOVE SPACES                 TO  PRTOPTO                      
00960                                      ALTPRTO.                     
00961                                                                   
00962      MOVE ER-0000                TO EMI-ERROR.                    
00963      MOVE -1                     TO MAINTL.                       
00964      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00965      GO TO 8100-SEND-INITIAL-MAP.                                 
00966                                                                   
00967  0480-PRINT-NOW.                                                  
00968      
           EXEC CICS HANDLE CONDITION
00969          TERMIDERR   (8820-TERM-ERROR)
00970          TRANSIDERR  (8830-TRAN-ERROR)
00971      END-EXEC.
00972                                                                   
00973      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                
00974      MOVE '1'                    TO  CNTL-REC-TYPE.               
00975      MOVE SPACES                 TO  CNTL-ACCESS.                 
00976      MOVE +0                     TO  CNTL-SEQ-NO.                 
00977      MOVE 'CNTL'                 TO  FILE-SWITCH.                 
00978                                                                   
00979      PERFORM 7970-READ-CNTL THRU 7970-EXIT.                       
00980                                                                   
00981      IF CF-FORMS-PRINTER-ID = SPACES                              
00982          IF ALTPRTL NOT GREATER THAN 0                            
00983              MOVE ER-0337        TO  EMI-ERROR                    
00984              MOVE -1             TO  ALTPRTL                      
00985              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00986              GO TO 8200-SEND-DATAONLY
041002         END-IF
           END-IF.
00987                                                                   
00988      IF ALTPRTL GREATER THAN 0                                    
00989          MOVE ALTPRTI            TO  CF-FORMS-PRINTER-ID
041002     END-IF.
00990                                                                   
00991      MOVE PI-LAST-CLAIM          TO  PI-CLAIM-NO.                 
00992      MOVE PI-LAST-CARR           TO  PI-CARRIER.                  
00993      MOVE PI-LAST-CERT           TO  PI-CERT-NO.                  
00994                                                                   
00995      
           EXEC CICS START
00996          TRANSID   (START-TRANS-ID)
00997          TERMID    (CF-FORMS-PRINTER-ID)
00998          FROM      (PROGRAM-INTERFACE-BLOCK)
00999          LENGTH    (PI-COMM-LENGTH)
01000      END-EXEC.
01001                                                                   
01002      MOVE AL-UANOF               TO  PRTOPTA                      
01003                                      ALTPRTA.                     
01004                                                                   
01005      MOVE SPACES                 TO  PRTOPTO                      
01006                                      ALTPRTO.                     
01007                                                                   
01008      MOVE ER-0000                TO  EMI-ERROR.                   
01009      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01010      MOVE -1                     TO  MAINTL.                      
01011      GO TO 8100-SEND-INITIAL-MAP.                                 
01012                                                                   
01013      EJECT                                                        
01014  0500-CREATE-TEMP-STORAGE.                                        
01015      MOVE EIBCPOSN               TO PI-CURSOR.                    
01016                                                                   
052113    move emi-line1               to errmsg1o
052113    move emi-line2               to errmsg2o
052113    move emi-sub                 to pi-emi-sub
01017      
           EXEC CICS WRITEQ TS
01018          QUEUE   (QID)
01019          FROM    (EL130AI)
01020          LENGTH  (QID-MAP-LENGTH)
01021      END-EXEC.
01022                                                                   
01023      
           EXEC CICS WRITEQ TS
01024          QUEUE    (QID)
01025          FROM     (PROGRAM-INTERFACE-BLOCK)
01026          LENGTH   (PI-COMM-LENGTH)
01027      END-EXEC.
01028                                                                   
01029  0599-EXIT.                                                       
01030       EXIT.                                                       
01031                                                                   
01032      EJECT                                                        
01033  0600-RECOVER-TEMP-STORAGE.                                       
01034      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.                 
01035      MOVE PI-EL127-TO-EL130-CNTRL                                 
01036                                  TO SAVE-EL127-TO-EL130-CNTRL.    
01037      MOVE PI-EL659-TO-EL130-CNTRL                                 
01038                                  TO SAVE-EL659-TO-EL130-CNTRL.    
01039      MOVE PI-COMPANY-ID          TO SAVE-COMPANY-ID.              
01040      MOVE PI-COMPANY-CD          TO SAVE-COMPANY-CD.              
01041      MOVE PI-JOURNAL-FILE-ID     TO SAVE-JOURNAL-FILE-ID.         
01042      MOVE PI-CREDIT-USER         TO SAVE-CREDIT-USER.             
01043      MOVE PI-CLAIM-USER          TO SAVE-CLAIM-USER.              
01044      MOVE PI-LIFE-OVERRIDE-L1    TO SAVE-LIFE-OVERRIDE-L1.        
01045      MOVE PI-LIFE-OVERRIDE-L2    TO SAVE-LIFE-OVERRIDE-L2.        
01046      MOVE PI-LIFE-OVERRIDE-L6    TO SAVE-LIFE-OVERRIDE-L6.        
01047      MOVE PI-LIFE-OVERRIDE-L12   TO SAVE-LIFE-OVERRIDE-L12.       
01048      MOVE PI-AH-OVERRIDE-L1      TO SAVE-AH-OVERRIDE-L1.          
01049      MOVE PI-AH-OVERRIDE-L2      TO SAVE-AH-OVERRIDE-L2.          
01050      MOVE PI-AH-OVERRIDE-L6      TO SAVE-AH-OVERRIDE-L6.          
01051      MOVE PI-AH-OVERRIDE-L12     TO SAVE-AH-OVERRIDE-L12.         
01052      MOVE PI-CERT-ACCESS-CONTROL TO SAVE-CERT-ACCESS-CONTROL.     
01053      MOVE PI-CARRIER-CONTROL-LEVEL  TO SAVE-CARRIER-CONTROL-LEVEL.
PEMMOD     MOVE PI-PROGRAM-WORK-AREA (1:10)
PEMMOD                                 TO SAVE-BENEFICIARY
01054                                                                   
01055      
           EXEC CICS HANDLE CONDITION
01056          NOTFND  (0660-NOT-FOUND)
01057          QIDERR  (0670-QIDERR)
01058      END-EXEC.
01059                                                                   
01060      
           EXEC CICS READQ TS
01061          QUEUE    (QID)
01062          INTO     (EL130AI)
01063          LENGTH   (QID-MAP-LENGTH)
01064      END-EXEC.
01065                                                                   
01066      IF RETURNED-FROM = XCTL-131                                  
01067         IF CLAIM-DELETED-BY-EL131                                 
01068             MOVE LOW-VALUES      TO CERTNOI                       
01069                                     SUFXI                         
01070             MOVE ZEROS           TO CERTNOL                       
01071                                     SUFXL
041002         END-IF
041002     END-IF.
01072                                                                   
01073      IF RETURNED-FROM = XCTL-131                                  
01074          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA          
01075          MOVE ZEROS              TO PI-CERT-SELECT-CNT            
01076                                     PI-CERT-PROCESSED             
01077      ELSE                                                         
01078          
               EXEC CICS READQ TS
01079               QUEUE   (QID)
01080               INTO    (PROGRAM-INTERFACE-BLOCK)
01081               LENGTH  (PI-COMM-LENGTH)
01082          END-EXEC
041002     END-IF.
                                                
01084      
           EXEC CICS DELETEQ TS
01085          QUEUE   (QID)
01086      END-EXEC.
01087                                                                   
01088      MOVE SAVE-CONTROL           TO PI-CONTROL-IN-PROGRESS.       
01089                                                                   
01090      IF RETURNED-FROM = XCTL-127 OR XCTL-132                      
01091          MOVE SAVE-COMPANY-CD        TO PI-COMPANY-CD
01092          MOVE SAVE-COMPANY-ID        TO PI-COMPANY-ID
01093          MOVE SAVE-JOURNAL-FILE-ID   TO PI-JOURNAL-FILE-ID
01094          MOVE SAVE-CREDIT-USER       TO PI-CREDIT-USER
01095          MOVE SAVE-CLAIM-USER        TO PI-CLAIM-USER
01096          MOVE SAVE-LIFE-OVERRIDE-L1  TO PI-LIFE-OVERRIDE-L1
01097          MOVE SAVE-LIFE-OVERRIDE-L2  TO PI-LIFE-OVERRIDE-L2
01098          MOVE SAVE-LIFE-OVERRIDE-L6  TO PI-LIFE-OVERRIDE-L6
01099          MOVE SAVE-LIFE-OVERRIDE-L12 TO PI-LIFE-OVERRIDE-L12
01100          MOVE SAVE-AH-OVERRIDE-L1    TO PI-AH-OVERRIDE-L1
01101          MOVE SAVE-AH-OVERRIDE-L2    TO PI-AH-OVERRIDE-L2
01102          MOVE SAVE-AH-OVERRIDE-L6    TO PI-AH-OVERRIDE-L6
01103          MOVE SAVE-AH-OVERRIDE-L12   TO PI-AH-OVERRIDE-L12
01104          MOVE SAVE-CERT-ACCESS-CONTROL
01105                                     TO PI-CERT-ACCESS-CONTROL     
01106          MOVE SAVE-CARRIER-CONTROL-LEVEL
01107                                     TO PI-CARRIER-CONTROL-LEVEL
041002     END-IF.
01108                                                                   
052113*    IF EIBAID = DFHPF11
052113*       move errmsg1i            to emi-line1
052113*       move errmsg2i            to emi-line2
052113*       move pi-emi-sub          to emi-sub
052113*       go to 8200-SEND-DATAONLY
052113*    end-if
01109      IF RETURNED-FROM = XCTL-127                                  
01110          MOVE SAVE-EL127-TO-EL130-CNTRL                           
01111                                  TO PI-EL127-TO-EL130-CNTRL
041002     END-IF.
01112                                                                   
01113      IF RETURNED-FROM = XCTL-6592                                 
01114         MOVE SAVE-EL659-TO-EL130-CNTRL                            
01115                                  TO PI-EL659-TO-EL130-CNTRL
041002     END-IF.
01116                                                                   
PEMMOD     IF RETURNED-FROM = XCTL-114
PEMMOD        IF SAVE-BENEFICIARY NOT = SPACES AND LOW-VALUES
PEMMOD           MOVE SAVE-BENEFICIARY TO BENECDI
PEMMOD           MOVE +10              TO BENECDL
PEMMOD           MOVE -1               TO BIRTHDTL
PEMMOD        END-IF
041002     END-IF.

01117      PERFORM 3993-MODIFY-SCREEN-ATTRB THRU 3993-EXIT.             
01118                                                                   
01119      IF MAINTL NOT = ZERO                                         
01120          MOVE AL-UANON           TO MAINTA
041002     END-IF.
01121                                                                   
01122      IF ENTERPFL NOT = ZERO                                       
01123          MOVE AL-UNNON           TO ENTERPFA
041002     END-IF.
01124                                                                   
01125      IF RETURNED-FROM = XCTL-127                                  
01126          GO TO 0650-FROM-EL127
041002     END-IF.
01127                                                                   
01128      IF RETURNED-FROM = XCTL-132                                  
01129          GO TO 0640-FROM-EL132
041002     END-IF.
01130                                                                   
01131      IF RETURNED-FROM = XCTL-650                                  
01132          MOVE AL-SABON           TO PF5A                          
01133          PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT
041002     END-IF.
01134                                                                   
01135      IF RETURNED-FROM = XCTL-6592                                 
01136          GO TO 0630-FROM-EL6592
041002     END-IF.
01137                                                                   
01138      IF MAINTI = 'S' OR 'I'                                       
01139         GO TO 1000-SHOW-CLAIM
041002     END-IF.
01140                                                                   
01141      GO TO 8100-SEND-INITIAL-MAP.                                 
01142                                                                   
01143      EJECT                                                        
01144  0630-FROM-EL6592.                                                
01145      MOVE PI-EL659-CARRIER       TO CRTCARRO.                     
01146      MOVE PI-EL659-GROUPING      TO GROUPO.                       
01147      MOVE PI-EL659-STATE         TO STATEO.                       
01148      MOVE PI-EL659-ACCOUNT       TO ACCOUNTO.                     
01149                                                                   
01150      MOVE AL-UANON               TO CRTCARRA                      
01151                                     GROUPA                        
01152                                     STATEA                        
01153                                     ACCOUNTA.                     
01154      GO TO 8100-SEND-INITIAL-MAP.                                 
01155                                                                   
01156  0640-FROM-EL132.                                                 
01157      IF PI-CLAIM-NO = SPACES                                      
01158          GO TO 0645-NO-CLAIM-SELECTED
041002     END-IF.
01159                                                                   
01160      MOVE LOW-VALUES             TO EL130AI.                      
01161      MOVE 'MSTR'                 TO FILE-SWITCH.                  
01162      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
01163      MOVE PI-CARRIER             TO MSTR-CARRIER.                 
01164      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO                 
01165                                     CLMNOO.                       
01166      MOVE AL-UANON               TO CLMNOA.                       
01167      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 
01168                                                                   
01169      
           EXEC CICS HANDLE CONDITION
01170          NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01171      END-EXEC.
01172                                                                   
01173      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.                      
01174      MOVE 'X'                    TO CLAIM-SWITCH.                 
01175      PERFORM 7910-READ-NINETY THRU 7910-EXIT.                     
01176      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.        
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
01177      PERFORM 7910-READ-ACTV THRU 7910-EXIT.                       
01178      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.                
01179                                                                   
01180      MOVE ER-0213                TO EMI-ERROR.                    
01181      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01182      MOVE PI-CLAIM-NO            TO EMI-TEXT-VARIABLE (1).        
01183      GO TO 8100-SEND-INITIAL-MAP.                                 
01184                                                                   
01185  0645-NO-CLAIM-SELECTED.                                          
01186                                                                   
01187      IF MAINTI = 'S' OR 'I'                                       
01188         GO TO 1000-SHOW-CLAIM
041002     END-IF.
01189                                                                   
01190      GO TO 8100-SEND-INITIAL-MAP.                                 
01191                                                                   
01192  0650-FROM-EL127.                                                 
01193                                                                   
01194      IF PI-CERT-SELECT-CNT GREATER THAN +6                        
01195          MOVE ZEROS              TO PI-CERT-SELECT-CNT            
01196                                     PI-CERT-PROCESSED
041002     END-IF.
01197                                                                   
01198      IF PI-RETURN-TO-PROGRAM = XCTL-727                           
01199          MOVE +1                 TO PI-CERT-SELECT-CNT            
01200                                     PI-CERT-PROCESSED             
01201          MOVE PI-CARRIER         TO PI-EL127-CARRIER (1)          
01202          MOVE PI-GROUPING        TO PI-EL127-GROUPING (1)         
01203          MOVE PI-STATE           TO PI-EL127-STATE   (1)          
01204          MOVE PI-ACCOUNT         TO PI-EL127-ACCOUNT (1)          
01205          MOVE PI-CERT-EFF-DT     TO PI-EL127-EFF-DT  (1)          
01206          MOVE PI-CERT-NO         TO PI-EL127-CERT-NO (1)
041002     END-IF.
01207                                                                   
01208      IF PI-CERT-SELECT-CNT = ZERO                                 
01209          IF CERTNOI NOT = SPACES AND LOW-VALUES                   
01210              MOVE CERTNOI        TO PI-CERT-NO                    
01211              MOVE SUFXI          TO PI-CERT-SFX                   
01212              GO TO 3000-READ-CERT                                 
01213          ELSE                                                     
01214              GO TO 8100-SEND-INITIAL-MAP
041002         END-IF
041002     END-IF.
01215                                                                   
01216      MOVE AL-SABON               TO PF5A.                         
01217                                                                   
01218      IF PI-CERT-PROCESSED = 1                                     
01219          PERFORM 0680-PLUG-SELECTED-CERTS THRU 0680-EXIT
041002     END-IF.
01220                                                                   
01221      PERFORM 0690-HIGHLIGHT-CERTS    THRU 0690-EXIT.              
01222      PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT.              
01223                                                                   
01224      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 
01225      MOVE PI-EL127-CARRIER (PI-CERT-PROCESSED)                    
01226                                  TO CERT-CARRIER                  
01227                                     PI-CARRIER.                   
01228      MOVE PI-EL127-GROUPING(PI-CERT-PROCESSED)                    
01229                                  TO CERT-GROUPING                 
01230                                     PI-GROUPING.                  
01231      MOVE PI-EL127-STATE   (PI-CERT-PROCESSED)                    
01232                                  TO CERT-STATE                    
01233                                     PI-STATE.                     
01234      MOVE PI-EL127-ACCOUNT (PI-CERT-PROCESSED)                    
01235                                  TO CERT-ACCOUNT                  
01236                                     PI-ACCOUNT.                   
01237      MOVE PI-EL127-EFF-DT  (PI-CERT-PROCESSED)                    
01238                                  TO CERT-EFF-DT                   
01239                                     PI-CERT-EFF-DT.               
01240      MOVE PI-EL127-CERT-NO (PI-CERT-PROCESSED)                    
01241                                  TO CERT-CERT-NO                  
01242                                     PI-CERT-NO.                   
01243                                                                   
01244      MOVE 'CERT'                 TO FILE-SWITCH.                  
01245      PERFORM 7940-READ-CERT THRU 7940-EXIT.                       
01246                                                                   
01247      MOVE CERT-CERT-NO           TO PI-SAVE-CERT.                 
01248      MOVE CERT-ACCOUNT           TO PI-SAVE-ACCOUNT.              
01249      MOVE CERT-EFF-DT            TO PI-SAVE-EFFDT.                
01250                                                                   
01251      IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO                        
01252          MOVE PI-COMPANY-CD      TO MSTR5-COMP-CD                 
01253          MOVE CERT-CERT-NO       TO MSTR5-CERT-NO                 
01254          PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT         
01255          IF INCURRED-DATE-MATCH                                   
01256              MOVE ER-7352        TO EMI-ERROR                     
01257              MOVE -1             TO CERTNOL                       
01258              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01259          ELSE                                                     
01260              IF NO-CLAIMS-FOR-CERT                                
01261                  MOVE ER-7353    TO EMI-ERROR                     
01262                  MOVE -1         TO CERTNOL                       
01263                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01264              ELSE                                                 
01265                  MOVE ER-0558    TO EMI-ERROR                     
01266                  MOVE -1         TO CERTNOL                       
01267                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002             END-IF
041002         END-IF
041002     END-IF.
01268                                                                   
01269      MOVE SPACES                 TO PI-PRT-OPT                    
01270                                     PI-ALT-PRT.                   
01271                                                                   
01272      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.                
01273                                                                   
           if (ws-lf-joint-indicator not = 'J')
              and (ws-ah-joint-indicator not = 'J')
              move 'P'                 to INSTYPEO
              MOVE AL-UANON            TO INSTYPEA
           end-if

01274      IF LSTNMEL NOT GREATER ZERO                                  
01275         AND CRTLNMEI NOT = (SPACES AND LOW-VALUES)
01276          MOVE CRTLNMEI           TO  LSTNMEO
01277          MOVE CRTFNMEI           TO  FSTNMEO                      
01278          MOVE CRTINITI           TO  INITO                        
01279          MOVE AL-UANON           TO  LSTNMEA                      
01280                                      FSTNMEA                      
01281                                      INITA
041002     END-IF.
01282                                                                   
01283      IF SSNL GREATER ZERO                                         
01284          GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01285                                                                   
01286      IF CRTSSNI = SPACES OR LOW-VALUES                            
01287          GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01288                                                                   
01289      IF CM-SSN-STATE   = CM-STATE AND                             
01290         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME                         
01291             GO TO 0655-SKIP-SOCIAL-SECURITY
041002     END-IF.
01292                                                                   
01293      MOVE CRTSSNI                TO SSNO                          
01294      MOVE AL-UANON               TO SSNA.                         
01295                                                                   
01296  0655-SKIP-SOCIAL-SECURITY.                                       
01297                                                                   
01298      IF CLMCARRL NOT = ZERO  AND                                  
01299         CLMCARRI NOT = CRTCARRI                                   
01300         MOVE ER-0562             TO EMI-ERROR                     
01301         MOVE AL-UABON            TO CLMCARRA                      
01302         MOVE -1                  TO CLMCARRL                      
01303         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
01304                                                                   
01305      IF CLMCARRL = ZERO                                           
01306         MOVE AL-UANON            TO CLMCARRA                      
01307         MOVE CRTCARRI            TO CLMCARRI
041002     END-IF.
01308                                                                   
01309      MOVE 'A'                    TO MAINTO.                       
01310      MOVE AL-UANON               TO MAINTA.                       
01311                                                                   
01312      IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727               
01313         GO TO 8100-SEND-INITIAL-MAP                               
01314      ELSE                                                         
01315         GO TO 8150-SEND-MAP-CURSOR
041002     END-IF.
01316                                                                   
01317  0660-NOT-FOUND.                                                  
01318      MOVE ER-0214                TO EMI-ERROR.                    
01319      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01320      GO TO 8100-SEND-INITIAL-MAP.                                 
01321                                                                   
01322  0670-QIDERR.                                                     
01323      MOVE ER-0033                TO EMI-ERROR.                    
01324      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01325      GO TO 8100-SEND-INITIAL-MAP.                                 
01326                                                                   
01327      EJECT                                                        
01328  0680-PLUG-SELECTED-CERTS.                                        
01329                                                                   
01330      MOVE AL-SANON               TO BCERT1A                       
01331                                     BSUFX1A                       
01332                                     BCERT2A                       
01333                                     BSUFX2A                       
01334                                     BCERT3A                       
01335                                     BSUFX3A                       
01336                                     BCERT4A                       
01337                                     BSUFX4A                       
01338                                     BCERT5A                       
01339                                     BSUFX5A.                      
01340                                                                   
01341      IF PCERTNOO = SPACES OR LOW-VALUES                           
01342          MOVE PI-EL127-CERT-PRIME (1)                             
01343                                  TO PCERTNOO                      
01344          MOVE PI-EL127-CERT-SUFX  (1)                             
01345                                  TO PSUFXO
041002     END-IF.
01346                                                                   
01347      MOVE PI-EL127-CERT-PRIME (1)                                 
01348                                  TO BCERT1O.                      
01349      MOVE PI-EL127-CERT-SUFX  (1)                                 
01350                                  TO BSUFX1O.                      
01351                                                                   
01352      IF PI-CERT-SELECT-CNT GREATER THAN 1                         
01353          MOVE PI-EL127-CERT-PRIME (2)                             
01354                                  TO BCERT2O                       
01355          MOVE PI-EL127-CERT-SUFX  (2)                             
01356                                  TO BSUFX2O                       
01357      ELSE                                                         
01358          MOVE LOW-VALUES         TO BCERT2O                       
01359                                     BSUFX2O
041002     END-IF.
01360                                                                   
01361      IF PI-CERT-SELECT-CNT GREATER THAN 2                         
01362          MOVE PI-EL127-CERT-PRIME (3)                             
01363                                  TO BCERT3O                       
01364          MOVE PI-EL127-CERT-SUFX  (3)                             
01365                                  TO BSUFX3O                       
01366      ELSE                                                         
01367          MOVE LOW-VALUES         TO BCERT3O                       
01368                                     BSUFX3O
041002     END-IF.
01369                                                                   
01370      IF PI-CERT-SELECT-CNT GREATER THAN 3                         
01371          MOVE PI-EL127-CERT-PRIME (4)                             
01372                                  TO BCERT4O                       
01373          MOVE PI-EL127-CERT-SUFX  (4)                             
01374                                  TO BSUFX4O                       
01375      ELSE                                                         
01376          MOVE LOW-VALUES         TO BCERT4O                       
01377                                     BSUFX4O
041002     END-IF.
01378                                                                   
01379      IF PI-CERT-SELECT-CNT GREATER THAN 4                         
01380          MOVE PI-EL127-CERT-PRIME (5)                             
01381                                  TO BCERT5O                       
01382          MOVE PI-EL127-CERT-SUFX  (5)                             
01383                                  TO BSUFX5O                       
01384      ELSE                                                         
01385          MOVE LOW-VALUES         TO BCERT5O                       
01386                                     BSUFX5O
041002     END-IF.
01387                                                                   
01388  0680-EXIT.                                                       
01389      EXIT.                                                        
01390                                                                   
01391      EJECT                                                        
01392  0690-HIGHLIGHT-CERTS.                                            
01393                                                                   
01394      MOVE AL-UANON               TO PCERTNOA                      
01395                                     PSUFXA.                       
01396                                                                   
01397      IF BCERT1O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND     
01398         BSUFX1O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)         
01399             MOVE AL-SABON        TO BCERT1A                       
01400                                     BSUFX1A                       
01401      ELSE                                                         
01402          MOVE AL-SANON           TO BCERT1A                       
01403                                     BSUFX1A
041002     END-IF.
01404                                                                   
01405      IF BCERT2O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND     
01406         BSUFX2O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)         
01407             MOVE AL-SABON        TO BCERT2A                       
01408                                     BSUFX2A                       
01409      ELSE                                                         
01410          MOVE AL-SANON           TO BCERT2A                       
01411                                     BSUFX2A
041002     END-IF.
01412                                                                   
01413      IF BCERT3O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND     
01414         BSUFX3O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)         
01415             MOVE AL-SABON        TO BCERT3A                       
01416                                     BSUFX3A                       
01417      ELSE                                                         
01418          MOVE AL-SANON           TO BCERT3A                       
01419                                     BSUFX3A
041002     END-IF.
01420                                                                   
01421      IF BCERT4O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND     
01422         BSUFX4O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)         
01423             MOVE AL-SABON        TO BCERT4A                       
01424                                     BSUFX4A                       
01425      ELSE                                                         
01426          MOVE AL-SANON           TO BCERT4A                       
01427                                     BSUFX4A
041002     END-IF.
01428                                                                   
01429      IF BCERT5O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND     
01430         BSUFX5O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)         
01431             MOVE AL-SABON        TO BCERT5A                       
01432                                     BSUFX5A                       
01433      ELSE                                                         
01434          MOVE AL-SANON           TO BCERT5A                       
01435                                     BSUFX5A
041002     END-IF.
01436                                                                   
01437  0690-EXIT.                                                       
01438      EXIT.                                                        
01439                                                                   
01440      EJECT                                                        
01441  0800-TEST-PREVIOUS-CONTROL.                                      
01442      IF CLMNOI   = PI-LAST-CLAIM       AND                        
01443         CLMCARRI = PI-LAST-CARR        AND                        
01444         CERTNOI  = PI-LAST-CERT-PRIME  AND                        
01445         SUFXI    = PI-LAST-CERT-SUFX                              
041002*        NEXT SENTENCE
041002         CONTINUE
01447      ELSE                                                         
01448          MOVE ER-0207            TO EMI-ERROR                     
01449          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01450          MOVE -1                 TO MAINTL                        
01451          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01452                                                                   
01453      PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT.             
01454                                                                   
01455      MOVE PI-LAST-CLAIM          TO PI-CLAIM-NO.                  
01456      MOVE PI-LAST-CARR           TO PI-CARRIER.                   
01457      MOVE PI-LAST-CERT           TO PI-CERT-NO.                   
01458                                                                   
01459      IF EIBAID = DFHPF7                                           
01460          MOVE XCTL-157           TO PGM-NAME
041002     END-IF.
01461                                                                   
01462      IF EIBAID = DFHPF8                                           
01463          MOVE XCTL-141           TO PGM-NAME
041002     END-IF.
01464                                                                   
01465      IF EIBAID = DFHPF9                                           
01466         MOVE SPACES              TO PI-SAVED-PROGRAM-1            
01467                                     PI-SAVED-PROGRAM-2            
01468                                     PI-SAVED-PROGRAM-3            
01469                                     PI-SAVED-PROGRAM-4            
01470                                     PI-SAVED-PROGRAM-5            
01471                                     PI-SAVED-PROGRAM-6            
01472         MOVE XCTL-126            TO PI-RETURN-TO-PROGRAM          
01473         MOVE XCTL-132            TO PI-CALLING-PROGRAM            
01474         MOVE XCTL-150            TO PGM-NAME
041002     END-IF.
01475                                                                   
01476      GO TO 9300-XCTL.                                             
01477                                                                   
01478      EJECT                                                        
01479  1000-SHOW-CLAIM.                                                 
01480                                                                   
01481      IF CLMNOL = ZERO                                             
01482          MOVE -1                 TO CLMNOL                        
01483          MOVE AL-UABON           TO CLMNOA                        
01484          MOVE ER-0202            TO EMI-ERROR                     
01485          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01486      ELSE                                                         
01487          MOVE AL-UANON           TO CLMNOA
041002     END-IF.
01488                                                                   
01489      IF PI-NO-CARRIER-SECURITY                                    
01490         IF CLMCARRL = ZERO                                        
01491              MOVE -1             TO CLMCARRL                      
01492              MOVE AL-UABON       TO CLMCARRA                      
01493              MOVE ER-0194        TO EMI-ERROR                     
01494              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01495          ELSE                                                     
01496              MOVE AL-UANON       TO CLMCARRA
041002         END-IF
041002     END-IF.
01497                                                                   
01498      IF SUFXL = ZERO                                              
01499          MOVE SPACES             TO SUFXI                         
01500          MOVE AL-UANON           TO SUFXA                         
01501      ELSE                                                         
01502          MOVE AL-UANON           TO SUFXA
041002     END-IF.
01503
01504      IF NOT EMI-NO-ERRORS                                         
01505          GO TO 1050-SEND-CHECK
041002     END-IF.
01506                                                                   
01507      
           EXEC CICS HANDLE CONDITION
01508          NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01509      END-EXEC.
01510                                                                   
01511      MOVE 'MSTR'                 TO FILE-SWITCH                   
01512      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
01513                                                                   
01514      MOVE CLMCARRI               TO MSTR-CARRIER.                 
01515      MOVE CLMNOI                 TO MSTR-CLAIM-NO.                
01516                                                                   
01517      IF CERTNOL = ZERO     OR                                     
01518         CERTNOI = SPACES   OR                                     
01519         CERTNOI = LOW-VALUES                                      
01520          PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT
PEMMOD         MOVE AL-SANON        TO CERTNOA
PEMMOD                                 SUFXA
01523      ELSE                                                         
01524          PERFORM 7610-BROWSE-CLAIM-LOOP THRU 7699-EXIT
041002     END-IF.
01525                                                                   
01526      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.           
01527      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.            
01528                                                                   
01529      
           EXEC CICS HANDLE CONDITION
01530          NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01531      END-EXEC.
01532                                                                   
01533      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.                      
01534                                                                   
01535      IF NOT PI-NO-ACCOUNT-SECURITY                                
01536          IF PI-ACCOUNT-SECURITY NOT = CL-CERT-ACCOUNT
01537            MOVE ER-2371          TO EMI-ERROR                     
01538            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01539            MOVE -1               TO MAINTL                        
01540            GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
01541                                                                   
01542      
           EXEC CICS HANDLE CONDITION
01543          NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
01544      END-EXEC.
01545                                                                   
01546      MOVE 'Y'                    TO CLAIM-SWITCH.                 
01547      PERFORM 7910-READ-NINETY THRU 7910-EXIT.                     
01548      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.        
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
01549      PERFORM 7910-READ-ACTV THRU 7910-EXIT.                       
01550                                                                   
01551  1000-SHOW-END-OF-PERFORM.                                        
01552                                                                   
01553      MOVE MSTR-COMP-CD           TO CERT-COMP-CD.                 
01554      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 
01555      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.                
01556      MOVE CL-CERT-STATE          TO CERT-STATE.                   
01557      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 
01558      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.                  
01559      MOVE CL-CERT-NO             TO CERT-CERT-NO.                 
01560                                                                   
01561      MOVE 'CERT'                 TO FILE-SWITCH.                  
01562                                                                   
01563      PERFORM 7940-READ-CERT THRU 7940-EXIT.                       
01564                                                                   
01565      MOVE CL-LAST-MAINT-DT       TO PI-MSTR-DT.                   
01566      MOVE CL-LAST-MAINT-HHMMSS   TO PI-MSTR-WHEN.                 
01567      MOVE AT-RECORDED-DT         TO PI-TRLR-DT.                   
01568      MOVE AT-LAST-MAINT-HHMMSS   TO PI-TRLR-WHEN.                 
01569      MOVE CLMNOI                 TO PI-LAST-CLAIM.                
01570      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.           
01571      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.            
01572      MOVE CLMCARRI               TO PI-LAST-CARR.                 
01573                                                                   
01574      PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.                
01575                                                                   
01576      IF MAINTI = 'I'                                              
01577          MOVE AL-SANOF TO  FSTNMEA   INITA     SEXA               
01578                  BIRTHDTA  LSTNMEA   RELCLMA
01579                  LOANNOA   LOANBALA  BENECDA                      
01580          MOVE AL-SANON TO  SSNA                                   
040814         MOVE SPACES   TO  INCURI    REPORTI               
01582          MOVE 'Y'      TO PI-INCURR-SW                            
01583          MOVE ER-0522  TO EMI-ERROR                               
01584          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01585      ELSE                                                         
01586          MOVE ' '      TO PI-INCURR-SW
041002     END-IF.
01587                                                                   
01588      MOVE -1                     TO MAINTL.                       
01589      MOVE MSTR-CARRIER           TO PI-CARRIER.                   
01590      MOVE CERT-GROUPING          TO PI-GROUPING.                  
01591      MOVE CERT-STATE             TO PI-STATE.                     
01592      MOVE CERT-ACCOUNT           TO PI-ACCOUNT.                   
01593      MOVE MSTR-CLAIM-NO          TO PI-CLAIM-NO.                  
01594      MOVE MSTR-CERT-NO           TO PI-CERT-NO.                   
01595      MOVE CERT-EFF-DT            TO PI-CERT-EFF-DT.               
01596                                                                   
01597      IF RETURNED-FROM NOT = SPACES                                
01598          GO TO 8100-SEND-INITIAL-MAP                              
01599      ELSE                                                         
01600          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01601                                                                   
01602  1025-SHOW-RECORD-NOT-FOUND.                                      
01603                                                                   
01604      IF FILE-SWITCH = 'MSTR'                                      
01605          MOVE ER-0204            TO EMI-ERROR                     
01606      ELSE                                                         
01607          IF FILE-SWITCH = 'TRLR'                                  
01608              MOVE ER-0205        TO EMI-ERROR                     
01609          ELSE                                                     
01610              MOVE ER-0206        TO EMI-ERROR
041002         END-IF
041002     END-IF.
01611                                                                   
01612      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01613                                                                   
01614  1050-SEND-CHECK.                                                 
01615                                                                   
01616      IF RETURNED-FROM NOT = SPACES                                
01617          GO TO 8100-SEND-INITIAL-MAP                              
01618      ELSE                                                         
01619          MOVE -1                 TO MAINTL                        
01620          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01621                                                                   
01623  2000-DELETE-CLAIM.                                               
01624      IF NOT MODIFY-CAP                                            
01625          MOVE ER-0070            TO EMI-ERROR                     
01626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01627          MOVE -1                 TO MAINTL                        
01628          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01629                                                                   
01630      IF CLMNOI   NOT = PI-LAST-CLAIM       OR                     
01631         CLMCARRI NOT = PI-LAST-CARR        OR                     
01632         CERTNOI  NOT = PI-LAST-CERT-PRIME  OR                     
01633         SUFXI    NOT = PI-LAST-CERT-SUFX                          
01634          MOVE ER-0207            TO EMI-ERROR                     
01635          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01636          MOVE -1                 TO MAINTL                        
01637          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01638                                                                   
01639      
           EXEC CICS HANDLE CONDITION
01640          NOTFND   (2050-DELETE-NOT-FOUND)
01641      END-EXEC.
01642                                                                   
01643      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
01644                                                                   
01645      MOVE CLMCARRI               TO MSTR-CARRIER.                 
01646      MOVE CLMNOI                 TO MSTR-CLAIM-NO.                
01647      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.           
01648      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.            
01649                                                                   
01650      MOVE 'MSTR'                 TO FILE-SWITCH.                  
01651      PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.               
01652                                                                   
01653      MOVE 'Y'                    TO CLAIM-SWITCH.                 
01654                                                                   
01655      IF PI-MSTR-DT   NOT = CL-LAST-MAINT-DT  OR                   
01656         PI-MSTR-WHEN NOT = CL-LAST-MAINT-HHMMSS                   
01657          MOVE ER-0068        TO EMI-ERROR
01658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01659          MOVE -1             TO MAINTL
01660          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01661
01662      IF CL-TRAILER-SEQ-CNT NOT = 4095                             
01663          MOVE ER-0569            TO EMI-ERROR                     
01664          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01665          MOVE -1                 TO MAINTL                        
01666          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01667                                                                   
01668      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC                       
01669          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
041002     END-IF.
01670                                                                   
01671      IF CL-PURGED-DT NOT = LOW-VALUES                             
01672          MOVE ER-7691            TO EMI-ERROR                     
01673          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01674          MOVE -1                 TO MAINTL                        
01675          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01676                                                                   
01677      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.                 
01678      MOVE MSTR-CARRIER           TO TRLR-CARRIER.                 
01679      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.                
01680      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.                 
01681      MOVE +0                     TO TRLR-SEQ-NO.                  
01682      MOVE '1'                    TO TRLR-TYPE.                    
01683                                                                   
01684      MOVE 'TRLR'                 TO FILE-SWITCH.                  
01685                                                                   
01686      
           EXEC CICS READ
01687          UPDATE
01688          DATASET  (ELTRLR-DSID)
01689          SET      (ADDRESS OF ACTIVITY-TRAILERS)
01690          RIDFLD   (ELTRLR-KEY)
01691      END-EXEC.
01692                                                                   
01693      IF PI-TRLR-DT   NOT = AT-RECORDED-DT  OR                     
01694         PI-TRLR-WHEN NOT = AT-LAST-MAINT-HHMMSS                   
01695          MOVE ER-0068        TO EMI-ERROR
01696          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01697          MOVE -1             TO MAINTL
01698          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01699                                                                   
01700      
           EXEC CICS DELETE
01701          DATASET   (ELTRLR-DSID)
01702      END-EXEC.
01703                                                                   
01704      MOVE +90                    TO TRLR-SEQ-NO.                  
01705      MOVE '6'                    TO TRLR-TYPE.                    
01706                                                                   
01707      
           EXEC CICS READ
01708          UPDATE
01709          DATASET  (ELTRLR-DSID)
01710          SET      (ADDRESS OF ACTIVITY-TRAILERS)
01711          RIDFLD   (ELTRLR-KEY)
01712      END-EXEC.
01713                                                                   
01714      
           EXEC CICS DELETE
01715          DATASET   (ELTRLR-DSID)
01716      END-EXEC.
01717                                                                   
CIDMOD     MOVE +91                    TO TRLR-SEQ-NO.
CIDMOD     MOVE '6'                    TO TRLR-TYPE.
CIDMOD                                                                  
CIDMOD     
           EXEC CICS READ
CIDMOD         UPDATE
CIDMOD         DATASET  (ELTRLR-DSID)
CIDMOD         SET      (ADDRESS OF ACTIVITY-TRAILERS)
CIDMOD         RIDFLD   (ELTRLR-KEY)
CIDMOD         RESP     (WS-RESPONSE)
CIDMOD     END-EXEC
CIDMOD                                                                  
CIDMOD     IF WS-RESP-NORMAL
              EXEC CICS DELETE
CIDMOD           DATASET   (ELTRLR-DSID)
CIDMOD        END-EXEC
041002     END-IF.
01717                                                                   
01718      MOVE MSTR-COMP-CD           TO CERT-COMP-CD.                 
01719      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 
01720      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.                
01721      MOVE CL-CERT-STATE          TO CERT-STATE.                   
01722      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 
01723      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.                  
01724      MOVE CL-CERT-NO             TO CERT-CERT-NO.                 
01725                                                                   
01726      MOVE 'CERT'                 TO FILE-SWITCH.                  
01727                                                                   
01728      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.                
01729                                                                   
01730      SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT GIVING WS-NUM-HOLD. 
01731                                                                   
01732      IF WS-NUM-HOLD NOT = +0                                      
01733          GO TO 2005-REWRITE-CERT
041002     END-IF.
01734                                                                   
01735      IF NOT CERT-WAS-CREATED                                      
01736          MOVE SPACE              TO CM-CLAIM-INTERFACE-SW         
01737          GO TO 2005-REWRITE-CERT
041002     END-IF.
01738                                                                   
01739      
           EXEC CICS DELETE
01740          DATASET   (ELCERT-DSID)
01741      END-EXEC.
01742                                                                   
01743      GO TO 2010-DELETE-CL.                                        
01744                                                                   
01745  2005-REWRITE-CERT.                                               
01746                                                                   
01747      SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT.                    
01748                                                                   
01749      
           EXEC CICS HANDLE CONDITION
01750           DUPKEY   (2010-DELETE-CL)
01751      END-EXEC.
01752                                                                   
01753      
           EXEC CICS REWRITE
01754          DATASET  (ELCERT-DSID)
01755          FROM     (CERTIFICATE-MASTER)
01756      END-EXEC.
01757                                                                   
01759  2010-DELETE-CL.                                                  
01760                                                                   
01761      
           EXEC CICS DELETE
01762          DATASET  (ELMSTR-DSID)
01763      END-EXEC.
01764                                                                   
01765      MOVE -1                     TO ONE-OR-MIN1.                  
01766      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.                  
01767                                                                   
01768      IF WS-ASSOC-CERT-TOTAL NOT = ZERO                            
01769          MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY                    
01770                                     WS-SAVE-CLAIM-KEY             
01771          MOVE +1                 TO ONE-OR-MIN1                   
01772          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
041002     END-IF.
01773                                                                   
01774      MOVE ER-0000                TO EMI-ERROR.                    
01775      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01776      MOVE LOW-VALUES             TO EL130AO.                      
01777      MOVE -1                     TO MAINTL.                       
01778      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.         
01779      MOVE ZEROS                  TO PI-CERT-SELECT-CNT            
01780                                     PI-CERT-PROCESSED             
01781                                     PI-LETTER-ERROR-CODE.         
01782      GO TO 8100-SEND-INITIAL-MAP.                                 
01783                                                                   
01784  2050-DELETE-NOT-FOUND.

01785      IF FILE-SWITCH = 'MSTR'                                      
01786          MOVE -1                 TO CLMNOL                        
01787          MOVE ER-0204            TO EMI-ERROR                     
01788      ELSE                                                         
01789          IF FILE-SWITCH = 'TRLR'                                  
01790              MOVE -1             TO MAINTL                        
01791              MOVE ER-0205        TO EMI-ERROR                     
01792          ELSE                                                     
01793              MOVE -1             TO CERTNOL                       
01794              MOVE ER-0206        TO EMI-ERROR
041002         END-IF
041002     END-IF.
01795                                                                   
01796      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01797      GO TO 8200-SEND-DATAONLY.                                    
01798                                                                   
01800  3000-ADD-CLAIM.

01801      IF NOT MODIFY-CAP                                            
01802          MOVE ER-0070            TO EMI-ERROR                     
01803          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01804          MOVE -1                 TO MAINTL                        
01805          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01806                                                                   
01807      PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.                 
01808                                                                   
01809      IF CERTMTI = 'S'                                             
01810          IF CERTNOI  = PI-SAVE-CERT-PRIME  AND                    
01811             SUFXI    = PI-SAVE-CERT-SUFX   AND                    
01812             WS-EFFDT = PI-SAVE-EFFDT       AND                    
01813             ACCOUNTI = PI-SAVE-ACCOUNT                            
01814              IF EMI-ERROR = 0                                     
01815                  GO TO 3010-TEST-FOR-ERRORS                       
01816              ELSE                                                 
01817                  MOVE PI-COMPANY-CD    TO  CERT-COMP-CD           
01818                  MOVE PI-CARRIER       TO  CERT-CARRIER           
01819                  MOVE PI-GROUPING      TO  CERT-GROUPING          
01820                  MOVE PI-STATE         TO  CERT-STATE             
01821                  MOVE PI-ACCOUNT       TO  CERT-ACCOUNT           
01822                  MOVE PI-CERT-EFF-DT   TO  CERT-EFF-DT            
01823                  MOVE PI-CERT-NO       TO  CERT-CERT-NO           
01824                  
                       EXEC CICS HANDLE CONDITION
01825                      NOTFND   (3010-TEST-FOR-ERRORS)
01826                  END-EXEC
01827                  PERFORM 7940-READ-CERT THRU 7940-EXIT            
01828                  PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT  
01829                  GO TO 3010-TEST-FOR-ERRORS
041002             END-IF
041002         END-IF
041002     END-IF.
01830                                                                   
01831      IF CERTMTI = 'A'                                             
01832          IF NOT PI-NO-CARRIER-SECURITY OR
01833              NOT PI-NO-ACCOUNT-SECURITY
01834              MOVE ER-2370          TO EMI-ERROR
01835              MOVE -1               TO MAINTL
01836              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01837              GO TO 8200-SEND-DATAONLY
01838          ELSE
01839              PERFORM 6300-REQUIRED-CERT-EDIT THRU 6399-EXIT
01840              GO TO 3010-TEST-FOR-ERRORS
041002         END-IF
041002     END-IF.
01841                                                                   
01842      IF CERTNOL = ZEROS                                           
01843         GO TO 3010-TEST-FOR-ERRORS
041002     END-IF.
01844                                                                   
01845  3000-READ-CERT.                                                  
01846                                                                   
01847      MOVE PI-COMPANY-CD          TO CERT-COMP-CD-5                
01848                                     MSTR5-COMP-CD.                
01849      MOVE CERTNOI                TO CERT-CERT-5-PRIME             
01850                                     MSTR5-CERT-NO-PRIME.          
01851      MOVE SUFXI                  TO CERT-CERT-5-SUFX              
01852                                     MSTR5-CERT-NO-SUFX.           
01853                                                                   
01854      
           EXEC CICS HANDLE CONDITION
01855           DUPKEY   (3008-DUP-ERROR)
01856           NOTFND   (3009-NOT-FOUND)
01857      END-EXEC
01858                                                                   
01859      PERFORM 7980-READ-CERT5 THRU 7980-EXIT.                      
01860                                                                   
01861      IF NOT PI-NO-ACCOUNT-SECURITY                                
01862          IF PI-ACCOUNT-SECURITY NOT = CM-ACCOUNT
01863              MOVE ER-2371          TO EMI-ERROR
01864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01865              MOVE -1               TO MAINTL
01866              GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
01867                                                                   
01868      MOVE CM-CERT-NO             TO PI-SAVE-CERT.                 
01869      MOVE CM-ACCOUNT             TO PI-SAVE-ACCOUNT.              
01870      MOVE CM-CERT-EFF-DT         TO PI-SAVE-EFFDT.                
01871      MOVE CM-COMPANY-CD          TO PI-COMPANY-CD.
01872      MOVE CM-CARRIER             TO PI-CARRIER.
01873      MOVE CM-GROUPING            TO PI-GROUPING.
01874      MOVE CM-STATE               TO PI-STATE.
01875      MOVE CM-ACCOUNT             TO PI-ACCOUNT.
01876      MOVE CM-CERT-EFF-DT         TO PI-CERT-EFF-DT.
01877      MOVE CM-CERT-NO             TO PI-CERT-NO.
01878      PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT.             
01879                                                                   
01880      IF CRTLNMEI NOT = SPACES AND LOW-VALUES                      
01881          IF LSTNMEO = SPACES OR LOW-VALUES                        
01882              MOVE CRTLNMEI       TO  LSTNMEO                      
01883              MOVE AL-UANON       TO  LSTNMEA
041002         END-IF
041002     END-IF.
01884                                                                   
01885      MOVE 'S'                    TO CERTMTO.                      
PEMMOD*    MOVE AL-UANON               TO CERTMTA  CERTNOA  CLMCARRA    
PEMMOD     MOVE AL-SANON               TO CERTNOA.
PEMMOD     MOVE AL-UANON               TO CERTMTA   CLMCARRA            
01887                                  EFFDTA ACCOUNTA STATEA   GROUPA. 
01888      IF EMI-NO-ERRORS                                             
01889          MOVE -1                  TO MAINTL
041002     END-IF.
01890                                                                   
01891      IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO                        
01892          PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
01893          IF INCURRED-DATE-MATCH
01894              MOVE ER-7352         TO EMI-ERROR
01895              MOVE -1              TO CERTNOL
01896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01897          ELSE
01898              IF NO-CLAIMS-FOR-CERT
01899                  MOVE ER-7353     TO EMI-ERROR
01900                  MOVE -1          TO CERTNOL
01901                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01902              ELSE
01903                  MOVE ER-0558     TO EMI-ERROR
01904                  MOVE -1          TO CERTNOL
01905                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01906                  GO TO 3010-TEST-FOR-ERRORS
041002             END-IF
041002         END-IF
041002     END-IF.
01907                                                                   
01908  3007-SEND-SCREEN.

01909      IF RETURNED-FROM = XCTL-132 OR XCTL-127                      
01910          GO TO 8150-SEND-MAP-CURSOR
01911      ELSE                                                         
01912          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01913                                                                   
01914  3008-DUP-ERROR.

01915      MOVE ER-0560                TO EMI-ERROR.                    
01916      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01917      MOVE -1                     TO CERTNOL.                      
01918      GO TO 3007-SEND-SCREEN.                                      
01919                                                                   
01920  3009-NOT-FOUND.

01921      MOVE ER-0214                TO EMI-ERROR.                    
01922      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01923      MOVE -1                     TO CERTNOL.                      
01924      GO TO 3007-SEND-SCREEN.                                      
01925                                                                   
01927  3010-TEST-FOR-ERRORS.
01928                                                                   
01929      PERFORM 6200-EDIT-CERT-DATA           THRU 6200-EXIT.        
070714     perform 6500-get-acct    thru 6500-exit
01930      PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.        

070714*     perform 6500-get-acct    thru 6500-exit
061013
061013     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
061013     move +0                     to e1
061013     move spaces                 to ws-dcc-error-line
           move ' '                    to ws-zero-bens-avail
061013
           if ws-dcc-product-code not = spaces
061013        PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
061013        IF ERPDEF-FOUND
061013           MOVE CM-CERT-EFF-DT   TO DC-BIN-DATE-1
061013           MOVE WS-INCUR         TO DC-BIN-DATE-2
061013           MOVE '1'              TO DC-OPTION-CODE
061013           MOVE +0               TO DC-ELAPSED-MONTHS
061013                                    DC-ELAPSED-DAYS
061013           PERFORM 9700-LINK-DATE-CONVERT
061013                                 THRU 9700-EXIT
061013           IF NO-CONVERSION-ERROR
061013              MOVE DC-ELAPSED-MONTHS
061013                                 TO WS-MONTHS-BETWEEN
022122              IF DC-odd-days-over > 1
061013                 ADD 1 TO WS-MONTHS-BETWEEN
061013              END-IF
061013           ELSE
061013              MOVE ZEROS         TO WS-MONTHS-BETWEEN
061013           END-IF
061013
061013           evaluate true
061013              when (ws-excl-period not = zeros)
061013                 and (ws-months-between <= ws-excl-period)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
                       move 'Y'        to ws-zero-bens-avail
061013                 MOVE ER-1651    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when (ws-cov-ends not = zeros)
061013                 and (ws-months-between > ws-cov-ends)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
                       move 'Y'        to ws-zero-bens-avail
061013                 MOVE ER-1653    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 evaluate true
061013                    when clmtypei = 'L'
061013                       move '  LF  ' to emi-claim-type
061013                    when clmtypei = 'I'
061013                       move '  IU  ' to emi-claim-type
061013                    when clmtypei = 'F'
061013                       move '  FL  ' to emi-claim-type
022122                    when clmtypei = 'H'
022122                       move '  HS  ' to emi-claim-type
022122                    when clmtypei = 'B'
022122                       move '  BR  ' to emi-claim-type
061013                    when other
061013                       move '  AH  ' to emi-claim-type
061013                 end-evaluate
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when (ws-acc-period not = zeros)
061013                 and (ws-months-between <= ws-acc-period)
061013                 MOVE -1         TO MAINTL
061013                 add +1          to e1
061013                 MOVE ER-1652    TO EMI-ERROR
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013                 add +1          to e1
061013                 move er-1655    to emi-error
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              when clmtypei = 'I'
061013                 add +1          to e1
061013                 move er-1661    to emi-error
061013                                    ws-error-no (e1)
061013                 PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013           end-evaluate
011118           IF (WS-PRE-EXISTING-PER NOT = ZEROS)
011118              AND (WS-MONTHS-BETWEEN <= WS-PRE-EXISTING-PER)
011118              MOVE -1         TO MAINTL
011118              ADD +1          TO E1
011118              MOVE ER-1677    TO EMI-ERROR
011118                                 WS-ERROR-NO (E1)
011118              PERFORM 9900-ERROR-FORMAT
011118                              THRU 9900-EXIT
011118           END-IF
061013        END-IF
061013*       perform 3996-read-cert-claim-trailer
061013*                                thru 3996-exit
061013     END-IF

           perform 3996-read-cert-claim-trailer
                                       thru 3996-exit

01932      IF CERTMTI NOT = 'A'                                         
01933          IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
01934              MOVE PI-COMPANY-CD       TO MSTR5-COMP-CD
01935              MOVE CERTNOI             TO MSTR5-CERT-NO-PRIME
01936              MOVE SUFXI               TO MSTR5-CERT-NO-SUFX
01937              PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
01938              IF INCURRED-DATE-MATCH
01939                  MOVE ER-7352         TO EMI-ERROR
01940                  MOVE -1              TO CERTNOL
01941                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01942              ELSE
01943                  IF NO-CLAIMS-FOR-CERT
01944                      MOVE ER-7353     TO EMI-ERROR
01945                      MOVE -1          TO CERTNOL
01946                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01947                  ELSE
01948                      MOVE ER-0558     TO EMI-ERROR
01949                      MOVE -1          TO CERTNOL
01950                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002                 END-IF
041002             END-IF
041002         END-IF
041002     END-IF.
01951                                                                   
01952      IF EIBAID = DFHPF6  AND NOT FORCE-CAP                        
01953          MOVE ER-0433             TO EMI-ERROR
01954          MOVE -1                  TO MAINTL
01955          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01956          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01957                                                                   
01958      IF (EMI-NO-ERRORS)
061013        OR (ZEROS = EMI-FATAL-CTR AND EMI-FORCABLE-CTR)
01959          GO TO 3015-TRY-TO-BUILD
041002     END-IF.
01960                                                                   
01961      IF EIBAID = DFHPF6                                           
01962          IF EMI-FATAL-CTR NOT EQUAL ZERO                          
01963              MOVE ER-0434         TO EMI-ERROR
01964              MOVE -1              TO MAINTL
01965              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01966              GO TO 8200-SEND-DATAONLY
01967          ELSE
01968              MOVE +1              TO INCURL
01969              GO TO 3015-TRY-TO-BUILD
041002         END-IF
041002     END-IF.
01970                                                                   
01971      IF RETURNED-FROM = XCTL-132 OR XCTL-127                      
01972          GO TO 8150-SEND-MAP-CURSOR
01973      ELSE                                                         
01974          GO TO 8200-SEND-DATAONLY
041002     END-IF.
01975                                                                   
01976  3015-TRY-TO-BUILD.

01977      IF CLMNOL EQUAL ZERO                                         
01978          GO TO 3030-BUILD-CONT
041002     END-IF.
01979                                                                   
01980      
           EXEC CICS HANDLE CONDITION
01981          NOTFND   (3030-BUILD-CONT)
01982      END-EXEC.
01983                                                                   
01984      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
01985                                                                   
01986      MOVE CLMCARRI               TO MSTR-CARRIER.                 
01987      MOVE CLMNOI                 TO MSTR-CLAIM-NO.                
01988      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.           
01989      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.            
01990                                                                   
01991      MOVE 'MSTR'                 TO FILE-SWITCH.                  
01992                                                                   
01993      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.                      
01994                                                                   
01995      MOVE ER-0221                TO EMI-ERROR.                    
01996                                                                   
01997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01998      MOVE -1                     TO MAINTL.                       
01999      GO TO 8200-SEND-DATAONLY.                                    
02000                                                                   
02001  3030-BUILD-CONT.

02002      IF (CERTMTI NOT = 'A')
020816        and (pi-company-id not = 'DCC' and 'VPP')
02006          GO TO 3060-BUILD-CONT
041002     END-IF.
02007                                                                   
02008      
           EXEC CICS HANDLE CONDITION
02009          NOTFND   (3033-ACCT-NOT-FOUND)
02010          ENDFILE  (3033-ACCT-NOT-FOUND)
02011      END-EXEC.
02012                                                                   
02013      MOVE SPACES                 TO ERACCT-KEY.                   
02014                                                                   
02015      IF CARR-GROUP-ST-ACCNT-CNTL                                  
02016          MOVE PI-COMPANY-CD      TO ACCT-COMP-CD                  
02017          MOVE CRTCARRI           TO ACCT-CARRIER                  
02018          MOVE GROUPI             TO ACCT-GROUPING                 
02019          MOVE STATEI             TO ACCT-STATE                    
02020          MOVE ACCOUNTI           TO ACCT-ACCOUNT                  
02021          GO TO 3031-MOVE-ACCT-KEY
041002     END-IF.
02022                                                                   
02023      MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.                 
02024      MOVE ACCOUNTI               TO ACCT-ACCOUNT.                 
02025                                                                   
02026      IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'                       
02027          MOVE STATEI             TO ACCT-STATE
041002     END-IF.
02028                                                                   
02029      IF PI-CERT-ACCESS-CONTROL = '2' OR '4'                       
02030          MOVE CRTCARRI           TO ACCT-CARRIER
041002     END-IF.
02031                                                                   
02032  3031-MOVE-ACCT-KEY.

02033      MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.              
02034                                                                   
02035      
           EXEC CICS STARTBR
02036          DATASET     (ERACCT2-DSID)
02037          RIDFLD      (SAVE-ERACCT-KEY)
02038          GENERIC
02039          KEYLENGTH   (20)
02040      END-EXEC.
02041                                                                   
02042      
           EXEC CICS HANDLE CONDITION
02043          ENDFILE   (3032-NOT-IN-DT-RANGE)
02044      END-EXEC.
02045                                                                   
02046  3031-READ-ACCT-LOOP.                                             
02047      
           EXEC CICS READNEXT
02048          DATASET   (ERACCT2-DSID)
02049          SET       (ADDRESS OF ACCOUNT-MASTER)
02050          RIDFLD    (SAVE-ERACCT-KEY)
02051      END-EXEC.
02052                                                                   
02053      IF ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1  OR                  
02054         ACCT-CARRIER  NOT = AM-VG-CARRIER     OR                  
02055         ACCT-GROUPING NOT = AM-VG-GROUPING    OR                  
02056         ACCT-STATE    NOT = AM-VG-STATE       OR                  
02057         ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT                         
02058          GO TO 3033-ACCT-NOT-FOUND
041002     END-IF.
02059                                                                   
02060      IF WS-EFFDT NOT LESS AM-EFFECTIVE-DT  AND                    
02061         WS-EFFDT LESS AM-EXPIRATION-DT                            
02062          MOVE PI-COMPANY-CD    TO CERT-COMP-CD
02063          MOVE AM-CARRIER       TO CERT-CARRIER
02064          MOVE AM-GROUPING      TO CERT-GROUPING
02065          MOVE AM-STATE         TO CERT-STATE
02066          MOVE AM-ACCOUNT       TO CERT-ACCOUNT
02067          MOVE WS-EFFDT         TO CERT-EFF-DT
02068          MOVE CERTNOI          TO CERT-CERT-NO-PRIME
02069          MOVE SUFXI            TO CERT-CERT-NO-SUFX
02070          MOVE AM-REI-TABLE     TO WS-REIN-TABLE
02071          GO TO 3034-CHECK-EMPLOYER-STATEMENT
041002     END-IF.
02072                                                                   
02073      IF WS-EFFDT NOT LESS AM-EXPIRATION-DT                        
02074          GO TO 3031-READ-ACCT-LOOP
041002     END-IF.
02075                                                                   
02076  3032-NOT-IN-DT-RANGE.

02077      MOVE ER-0226                TO EMI-ERROR.                    
02078      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02079      MOVE -1                     TO EFFDTL.                       
02080      GO TO 8200-SEND-DATAONLY.                                    
02081                                                                   
02082  3033-ACCT-NOT-FOUND.

02083      MOVE ER-0226 TO EMI-ERROR.                                   
02084      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02085      MOVE -1                     TO ACCOUNTL.                     
02086      GO TO 8200-SEND-DATAONLY.                                    
02087                                                                   
02088  3034-CHECK-EMPLOYER-STATEMENT.                                   
02089                                                                   
02090      IF AM-EMPLOYER-STMT-USED = '1' OR '2' OR '3' OR 'Y'          
041002*        NEXT SENTENCE
041002         CONTINUE
02092      ELSE                                                         
02093          GO TO  3034-CHECK-ACCOUNT-LIMITS
041002     END-IF.
02094                                                                   
02095      MOVE CERT-EFF-DT            TO  DC-BIN-DATE-1.               
02096      MOVE WS-INCUR               TO  DC-BIN-DATE-2.               
02097      MOVE '1'                    TO  DC-OPTION-CODE.              
02098      MOVE +0                     TO  DC-ELAPSED-MONTHS            
02099                                      DC-ELAPSED-DAYS.             
02100      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
02101                                                                   
02102      IF (AM-EMPLOYER-STMT-USED = '1' AND                          
02103          DC-ELAPSED-DAYS LESS THAN +31)                           
02104                       OR                                          
02105         (AM-EMPLOYER-STMT-USED = '2' AND                          
02106          DC-ELAPSED-DAYS LESS THAN +61)                           
02107                       OR                                          
02108         (AM-EMPLOYER-STMT-USED = '3' AND                          
02109          DC-ELAPSED-DAYS LESS THAN +91)                           
02110             MOVE 'Y'             TO  AM-EMPLOYER-STMT-USED
041002     END-IF.
02111                                                                   
02112  3034-CHECK-ACCOUNT-LIMITS.                                       
02113                                                                   
02117      IF CERTMTI NOT = 'A'                                         
02118          GO TO 3060-BUILD-CONT
041002     END-IF.
02119                                                                   
02120      IF AM-STATUS NOT = '0'                                       
02121          MOVE ER-0225                TO EMI-ERROR
02122          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02123          MOVE -1                     TO ACCOUNTL
041002     END-IF.
02124                                                                   
02125      IF EIBAID = DFHPF6                                           
02126          IF EMI-FATAL-CTR NOT = ZERO                              
02127              MOVE ER-0434         TO EMI-ERROR
02128              MOVE -1              TO MAINTL
02129              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02130              GO TO 8200-SEND-DATAONLY
041002         END-IF
041002     END-IF.
02131
121802*    IF PI-COMPANY-ID NOT = 'CRI'                                 
121802*        GO TO 3035-END-BROWSE
121802*    END-IF.
02201                                                                   
02202  3035-END-BROWSE.                                                 
02203      
           EXEC CICS ENDBR
02204          DATASET    (ERACCT2-DSID)
02205      END-EXEC.
02206                                                                   
02207  3040-CHECK-CERT-ALREADY-ON.                                      
02208      
           EXEC CICS HANDLE CONDITION
02209          NOTFND   (3070-BUILD-CONT)
02210      END-EXEC.
02211                                                                   
02212      PERFORM 7940-READ-CERT THRU 7940-EXIT.
02213                                                                   
02214      MOVE ER-0229                TO EMI-ERROR.                    
02215      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02216      MOVE -1                     TO MAINTL.                       
02217      GO TO 8200-SEND-DATAONLY.                                    
02218                                                                   
02219  3060-BUILD-CONT.

02220      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 
02221      MOVE CRTCARRI               TO CERT-CARRIER.                 
02222      MOVE GROUPI                 TO CERT-GROUPING.                
02223      MOVE STATEI                 TO CERT-STATE.                   
02224      MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.                 
02225      MOVE CERTNOI                TO CERT-CERT-NO-PRIME.           
02226      MOVE SUFXI                  TO CERT-CERT-NO-SUFX.            
02227      MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.                  
02228                                                                   
02229      
           EXEC CICS HANDLE CONDITION
02230          NOTFND  (3065-CERT-NOT-FOUND)
02231      END-EXEC.
02232                                                                   
02233      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.                
02234                                                                   
02235      GO TO 3070-BUILD-CONT.                                       
02236                                                                   
02237  3065-CERT-NOT-FOUND.

02238      MOVE ER-0244                TO EMI-ERROR.                    
02239      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02240      MOVE -1                     TO CERTNOL.                      
02241      GO TO 8200-SEND-DATAONLY.                                    
02242                                                                   
02243  3070-BUILD-CONT.

02244      IF CLMNOL NOT = ZERO                                         
02245          GO TO 3200-BUILD-ZERO-TRAILER
041002     END-IF.
02246                                                                   
02247      MOVE SPACES                 TO ELCNTL-KEY.                   
02248                                                                   
02249      IF CONTROL-IS-ACTUAL-CARRIER                                 
02250          MOVE PI-CARRIER         TO CNTL-CARRIER                  
02251      ELSE                                                         
02252          MOVE PI-CARRIER-CONTROL-LEVEL                            
02253                                  TO CNTL-CARRIER
041002     END-IF.
02254                                                                   
02255      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
02256      MOVE '6'                    TO CNTL-REC-TYPE.                
02257      MOVE +0                     TO CNTL-SEQ-NO.                  
02258                                                                   
02259      
           EXEC CICS HANDLE CONDITION
02260          NOTFND   (3100-CARRIER-NOT-FOUND)
02261      END-EXEC.
02262                                                                   
02263      
           EXEC CICS READ
02264          UPDATE
02265          DATASET   (ELCNTL-DSID)
02266          SET       (ADDRESS OF CONTROL-FILE)
02267          RIDFLD    (ELCNTL-KEY)
02268      END-EXEC.
02269                                                                   
02270      IF CLAIM-NO-MANUAL                                           
02271          GO TO 3120-CLAIM-MUST-BE-INPUT
041002     END-IF.
02272                                                                   
02273      MOVE CF-CLAIM-NO-METHOD     TO SAVE-METHOD.                  
02274      MOVE CF-CLAIM-COUNTER       TO SAVE-COUNTER.                 
02275                                                                   
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
062121           or 'FNL'
041002         CONTINUE
02278      ELSE                                                         
02279          GO TO 3080-ASSIGN-LOOP
041002     END-IF.
02280                                                                   
02281      
           EXEC CICS UNLOCK
02282          DATASET   (ELCNTL-DSID)
02283      END-EXEC.
02284                                                                   
02285      MOVE '1'                    TO CNTL-REC-TYPE.                
02286      MOVE SPACES                 TO CNTL-ACCESS.                  
02287                                                                   
030612     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
02290                                                                   
02291      
           EXEC CICS HANDLE CONDITION
02292          NOTFND   (3110-COMPANY-NOT-FOUND)
02293      END-EXEC.
02294                                                                   
02295      PERFORM 7960-READ-CNTL-UPDATE THRU 7960-EXIT.                
02296                                                                   
02297      MOVE CF-CO-CLAIM-COUNTER    TO SAVE-COUNTER.                 
02298                                                                   
02300  3080-ASSIGN-LOOP.

02301      IF (SAVE-COUNTER = +99999   AND SAVE-METHOD = '2') OR        
02302         (SAVE-COUNTER = +9999999 AND SAVE-METHOD = '3')           
02303          MOVE +0                 TO SAVE-COUNTER
041002     END-IF.
02304                                                                   
02305      ADD +1 TO SAVE-COUNTER.                                      
02306                                                                   
02307      IF SAVE-METHOD  = '3'                                        
02308          MOVE SAVE-COUNTER        TO WS-CLAIM-NUMBER-R1           
02309          GO TO 3090-SET-COMP
041002     END-IF.
02310                                                                   
02311      IF SAVE-METHOD  = '4'                                        
02312          MOVE SAVE-COUNTER       TO WS-CLAIM-NUMBER-R1            
02313          MOVE CLMCARRI           TO WS-CN-PRF-A                   
02314          GO TO 3090-SET-COMP
041002     END-IF.
02315                                                                   
02316      MOVE ZEROS                  TO WS-CLAIM-NUMBER.              
02317      MOVE SAVE-COUNTER           TO WS-CN-NUMBER.                 
02318      MOVE SAVE-DATE              TO CURR-DATE.                    
02319                                                                   
02320      IF CURR-MM LESS '10'                                         
02321          MOVE CURR-M2             TO WS-CN-PRF-B
041002     END-IF.
02322                                                                   
02323      IF CURR-MM = '10'                                            
02324         MOVE 'A'                 TO WS-CN-PRF-B
041002     END-IF.

02325      IF CURR-MM = '11'                                            
02326         MOVE 'B'                 TO WS-CN-PRF-B
041002     END-IF.

02327      IF CURR-MM = '12'                                            
02328         MOVE 'C'                 TO WS-CN-PRF-B
041002     END-IF.
02329                                                                   
02330      MOVE CURR-YEAR              TO WS-CN-PRF-A.                  
02331                                                                   
02338  3090-SET-COMP.                                                   
02339                                                                   
02340      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
02341      MOVE WS-CLAIM-NUMBER        TO MSTR-CLAIM-NO                 
02342                                     CLMNOO.                       
02343      MOVE AL-UANON               TO CLMNOA.                       
02344      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.           
02345      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.            
02346      MOVE CLMCARRI               TO MSTR-CARRIER.                 
02347                                                                   
02348      
           EXEC CICS HANDLE CONDITION
02349          NOTFND  (3200-BUILD-ZERO-TRAILER)
02350      END-EXEC.
02351                                                                   
02352      PERFORM 7950-READ-CLAIM THRU 7950-EXIT.                      
02353                                                                   
02354      GO TO 3080-ASSIGN-LOOP.                                      
02355                                                                   
02356  3100-CARRIER-NOT-FOUND.

02357      MOVE ER-0252                TO EMI-ERROR.                    
02358      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02359      MOVE -1                     TO CLMCARRL.                     
02360      GO TO 8200-SEND-DATAONLY.                                    
02361                                                                   
02362  3110-COMPANY-NOT-FOUND.

02363      MOVE ER-0254                TO EMI-ERROR.                    
02364      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02365      MOVE -1                     TO MAINTL.                       
02366      GO TO 8200-SEND-DATAONLY.                                    
02367                                                                   
02368  3120-CLAIM-MUST-BE-INPUT.

02369      MOVE ER-0264                TO EMI-ERROR.                    
02370      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02371      MOVE -1                     TO CLMNOL.                       
02372      GO TO 8200-SEND-DATAONLY.                                    
02373                                                                   
02375  3200-BUILD-ZERO-TRAILER.                                         
02376      
           EXEC CICS HANDLE CONDITION
02377          NOTFND
02378      END-EXEC.
02379                                                                   
02380      
           EXEC CICS GETMAIN
02381          SET     (ADDRESS OF ACTIVITY-TRAILERS)
02382          LENGTH  (ELTRLR-LENGTH)
02383      END-EXEC.
02384                                                                   
02385      MOVE SPACES                 TO ACTIVITY-TRAILERS.            
02386      MOVE 'AT'                   TO AT-RECORD-ID.                 
02387                                                                   
02388      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
02389      MOVE CLMCARRI               TO AT-CARRIER.                   
02390      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
02391      MOVE CERT-CERT-NO           TO AT-CERT-NO.                   
02392      MOVE +0                     TO AT-SEQUENCE-NO.               
02393      MOVE '1'                    TO AT-TRAILER-TYPE.              
02394                                                                   
02395      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
02396      MOVE '5'                    TO DC-OPTION-CODE.               
02397      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
02398      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT                
02399                                     AT-RESERVES-LAST-MAINT-DT.    
02400                                                                   
02401      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
02402                                     AT-RESERVES-LAST-UPDATED-BY.  
02403                                                                   
02404      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
02405                                                                   
02406      MOVE WS-RESERVE-CONTROLS    TO AT-RESERVE-CONTROLS.
02407                                                                   
02408      MOVE +0                     TO AT-FUTURE-RESERVE             
02409                                     AT-IBNR-RESERVE               
02410                                     AT-PAY-CURRENT-RESERVE        
02411                                     AT-INITIAL-MANUAL-RESERVE     
02412                                     AT-CURRENT-MANUAL-RESERVE     
02413                                     AT-ITD-LIFE-REFUNDS           
02414                                     AT-ITD-AH-REFUNDS             
02415                                     AT-ITD-ADDITIONAL-RESERVE.    
02416                                                                   
02417      MOVE LOW-VALUES             TO AT-LAST-COMPUTED-DT.          
02418                                                                   
02419      IF MANRSVL NOT = ZERO                                        
02420          MOVE WS-MANRSV          TO AT-INITIAL-MANUAL-RESERVE     
02421                                     AT-CURRENT-MANUAL-RESERVE
041002     END-IF.
02422                                                                   
02423      MOVE WS-EXPENSE-CONTROLS    TO AT-EXPENSE-CONTROLS.          
02424                                                                   
02425      MOVE +0                     TO AT-ITD-PAID-EXPENSES          
02426                                     AT-ITD-CHARGEABLE-EXPENSE.    
02427                                                                   
02428      MOVE WS-TODAY-DT            TO AT-OPEN-CLOSE-DATE (1).       
02429      MOVE 'O'                    TO AT-OPEN-CLOSE-TYPE (1).       
02430      MOVE 'NEW'                  TO AT-OPEN-CLOSE-REASON (1).     
02431                                                                   
02432      
           EXEC CICS WRITE
02433          DATASET   (ELTRLR-DSID)
02434          FROM      (ACTIVITY-TRAILERS)
02435          RIDFLD    (AT-CONTROL-PRIMARY)
02436      END-EXEC.
02437                                                                   
02438      MOVE AT-CONTROL-PRIMARY  TO  SAVE-AT-PRIMARY-KEY.            
02439                                                                   
02440      EJECT                 
050807
050807 3250-BUILD-I1-ADDRESS-TRAILER.
050807
050807     MOVE 'N'                    TO WS-NO-INSURED-ADDRESS.
050807     MOVE +0                     TO WS-INSURED-ADDR-CNT.     
050807     MOVE PI-COMPANY-CD          TO WS-MA-COMPANY-CD.                 
050807     MOVE CRTCARRI               TO WS-MA-CARRIER.                 
050807     MOVE GROUPI                 TO WS-MA-GROUPING.                
050807     MOVE STATEI                 TO WS-MA-STATE.                   
050807     MOVE PI-SAVE-ACCOUNT        TO WS-MA-ACCOUNT.                 
050807     MOVE CERTNOI                TO WS-MA-CERT-PRIME.           
050807     MOVE SUFXI                  TO WS-MA-CERT-SFX.            
050807     MOVE PI-SAVE-EFFDT          TO WS-MA-CERT-EFF-DT.                  
050807
050807     EXEC CICS HANDLE CONDITION
050807         NOTFND   (3290-NO-INSURED-ADDRESS)
050807     END-EXEC.
050807
050807     EXEC CICS READ
050807         EQUAL
050807         DATASET   (ERMAIL-DSID)
050807         SET       (ADDRESS OF MAILING-DATA)
050807         RIDFLD    (WS-MA-CONTROL-PRIMARY)
050807     END-EXEC.
050807
050807     IF MA-INSURED-LAST-NAME NOT GREATER THAN SPACES
050807         GO TO 3290-NO-INSURED-ADDRESS
050807     END-IF.
050807
050807     MOVE SPACES                 TO ACTIVITY-TRAILERS.            
050807     MOVE 'AT'                   TO AT-RECORD-ID.                 
050807                                                                  
050807     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
050807     MOVE CLMCARRI               TO AT-CARRIER.                   
050807     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
050807     MOVE CERT-CERT-NO           TO AT-CERT-NO.                   
050807     MOVE +1                     TO AT-SEQUENCE-NO.               
050807     MOVE '5'                    TO AT-TRAILER-TYPE.
050807
050807     MOVE 'I'                    TO AT-ADDRESS-TYPE.
050807     IF MA-INSURED-MIDDLE-INIT > SPACES
050807        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-MIDDLE-INIT DELIMITED BY SIZE
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-LAST-NAME DELIMITED BY SIZE
050807        INTO AT-MAIL-TO-NAME
050807     ELSE
050807        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
050807               ' ' DELIMITED BY SIZE
050807               MA-INSURED-LAST-NAME DELIMITED BY SIZE
050807        INTO AT-MAIL-TO-NAME
050807     END-IF.
050807     MOVE MA-ADDRESS-LINE-1      TO AT-ADDRESS-LINE-1.
050807     MOVE MA-ADDRESS-LINE-2      TO AT-ADDRESS-LINE-2.
050807*    MOVE MA-CITY-STATE          TO AT-CITY-STATE.
           MOVE MA-CITY                TO AT-CITY
           MOVE MA-ADDR-STATE          TO AT-STATE
050807     MOVE MA-ZIP                 TO AT-ZIP
050807     MOVE MA-PHONE-NO            TO AT-PHONE-NO
050807
050807     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT                
050807                                    AT-ADDRESS-LAST-MAINT-DT.    
050807                                                                  
050807     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
050807                                    AT-ADDRESS-LAST-UPDATED-BY.  
050807                                                                  
050807     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
050807
050807     EXEC CICS WRITE
050807         DATASET   (ELTRLR-DSID)
050807         FROM      (ACTIVITY-TRAILERS)
050807         RIDFLD    (AT-CONTROL-PRIMARY)
050807     END-EXEC.
050807
050807     ADD +1                      TO WS-INSURED-ADDR-CNT.
050807     GO TO 3300-BUILD-NINETY-TRAILER.
050807
050807 3290-NO-INSURED-ADDRESS.
050807      MOVE 'Y' TO WS-NO-INSURED-ADDRESS.
050807
02441  3300-BUILD-NINETY-TRAILER.                                       
02442                                                                   
02443      MOVE SPACES                 TO ACTIVITY-TRAILERS.            
02444      MOVE 'AT'                   TO AT-RECORD-ID.                 
02445                                                                   
02446      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
02447      MOVE CLMCARRI               TO AT-CARRIER.                   
02448                                                                   
02449      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
02450      MOVE CERT-CERT-NO           TO AT-CERT-NO.                   
02451      MOVE +90                    TO AT-SEQUENCE-NO.               
02452      MOVE '6'                    TO AT-TRAILER-TYPE.              
02453                                                                   
02454      IF DIAGL NOT = ZERO                                          
02455          MOVE DIAGI              TO AT-INFO-LINE-1
041002     END-IF.
040814
040814     IF ICD1L NOT = ZERO
040814         MOVE ICD1I              TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2L NOT = ZERO
040814         MOVE ICD2I              TO AT-ICD-CODE-2
040814     END-IF.
02456                                                                   
02457      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT                
02458                                     AT-GEN-INFO-LAST-MAINT-DT.    
02459                                                                   
02460      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
02461                                     AT-GEN-INFO-LAST-UPDATED-BY.  
02462      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
02463                                                                   
02464      
           EXEC CICS WRITE
02465          DATASET   (ELTRLR-DSID)
02466          FROM      (ACTIVITY-TRAILERS)
02467          RIDFLD    (AT-CONTROL-PRIMARY)
02468      END-EXEC.
02469                                                                   
02470      EJECT                                                        
02471  3400-CHECK-CERT-MAINT.                                           
02472                                                                   
02473      IF CERTMTI = 'A'                                             
02474          GO TO 3600-BUILD-NEW-CERT
041002     END-IF.
02475                                                                   
02476      IF NO-CLAIM-ATTACHED                                         
02477          MOVE '1'                TO CM-CLAIM-INTERFACE-SW
041002     END-IF.
02478                                                                   
02479      ADD +1                      TO CM-CLAIM-ATTACHED-COUNT.      
02480                                                                   
02481      IF CM-LF-CURRENT-STATUS = '2' OR                             
02482         CM-AH-CURRENT-STATUS = '2'                                
02483          GO TO 3400-REWRITE-CERT
041002     END-IF.
02484                                                                   
02485      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.                      
02486                                                                   
02487  3400-REWRITE-CERT.                                               
02488                                                                   
02489      
           EXEC CICS HANDLE CONDITION
02490          DUPKEY (3410-DUP-KEY)
02491      END-EXEC.
02492                                                                   
02493      
           EXEC CICS REWRITE
02494          DATASET   (ELCERT-DSID)
02495          FROM      (CERTIFICATE-MASTER)
02496      END-EXEC.
02497                                                                   
02498  3410-DUP-KEY.

02499      GO TO 3800-BUILD-NEW-CLAIM.                                  
02500                                                                   
02501  3600-BUILD-NEW-CERT.                                             
02502      
           EXEC CICS GETMAIN
02503          SET      (ADDRESS OF CERTIFICATE-MASTER)
02504          LENGTH   (ELCERT-LENGTH)
02505      END-EXEC.
02506                                                                   
02507      MOVE SPACES                 TO CERTIFICATE-MASTER.           
02508                                                                   
02509      MOVE 'CM'                   TO CM-RECORD-ID.                 
02510                                                                   
02511      MOVE PI-COMPANY-CD          TO CM-COMPANY-CD                 
02512                                     CM-COMPANY-CD-A1              
02513                                     CM-COMPANY-CD-A2              
02514                                     CM-COMPANY-CD-A4              
02515                                     CM-COMPANY-CD-A5.             
02516                                                                   
02517      MOVE CERT-CARRIER           TO CM-CARRIER.                   
02518      MOVE CERT-GROUPING          TO CM-GROUPING.                  
02519      MOVE CERT-STATE             TO CM-STATE.                     
02520      MOVE CERT-ACCOUNT           TO CM-ACCOUNT.                   
02521      MOVE CERT-CERT-NO           TO CM-CERT-NO  CM-CERT-NO-A4.    
02522      MOVE CERT-EFF-DT            TO CM-CERT-EFF-DT.               
02523                                                                   
02524      MOVE ZERO      TO CM-LF-ORIG-TERM       CM-LF-BENEFIT-AMT    
02525                        CM-INSURED-ISSUE-AGE  CM-INSURED-JOINT-AGE 
02526                        CM-LF-BENEFIT-CD      CM-AH-BENEFIT-CD     
02527                        CM-LF-PREMIUM-AMT     CM-LF-REMAINING-AMT  
02528                        CM-LF-ITD-CANCEL-AMT  CM-LF-ITD-DEATH-AMT  
02529                        CM-LIVES              CM-LOAN-TERM         
02530                        CM-AH-ITD-AH-PMT      CM-AH-NSP-PREMIUM-AMT
02531                        CM-AH-DEV-PCT         CM-AH-CRITICAL-PERIOD
02532                        CM-LF-TERM-IN-DAYS    CM-LF-ALT-PREMIUM-AMT
02533                        CM-LF-DEV-PCT         CM-LF-NSP-PREMIUM-AMT
02534                        CM-LF-CRITICAL-PERIOD CM-LF-ALT-BENEFIT-AMT
02535                        CM-LF-PREMIUM-RATE    CM-AH-PREMIUM-RATE   
02536                        CM-LF-ALT-PREMIUM-RATE                     
02537                        CM-AH-ORIG-TERM       CM-AH-BENEFIT-AMT    
02538                        CM-AH-PREMIUM-AMT     CM-AH-ITD-CANCEL-AMT 
02539                        CM-AH-ITD-LUMP-PMT    CM-LOAN-APR          
02540                        CM-LOAN-BALANCE       CM-PAY-FREQUENCY     
02541                        CM-LIFE-COMM-PCT      CM-AH-COMM-PCT.      
02542                                                                   
02543      MOVE LOW-VALUES             TO CM-AH-LOAN-EXPIRE-DT          
02544                                     CM-AH-CANCEL-DT               
02545                                     CM-AH-SETTLEMENT-DT           
02546                                     CM-AH-CANCEL-EXIT-DT          
02547                                     CM-AH-SETTLEMENT-EXIT-DT      
02548                                     CM-LF-LOAN-EXPIRE-DT          
02549                                     CM-LF-CANCEL-DT               
02550                                     CM-LF-DEATH-DT                
02551                                     CM-LF-CANCEL-EXIT-DT          
02552                                     CM-LF-DEATH-EXIT-DT           
02553                                     CM-LAST-ADD-ON-DT             
02554                                     CM-ENTRY-DT.                  
02555                                                                   
02556      MOVE '2'                    TO CM-CLAIM-INTERFACE-SW.        
02557      MOVE +1                     TO CM-CLAIM-ATTACHED-COUNT.      
02558      MOVE LOW-VALUES             TO CM-LAST-MONTH-END.            
02559                                                                   
02560      MOVE CRTLNMEI               TO CM-INSURED-LAST-NAME          
02561                                     CM-PART-LAST-NAME-A2          
02562                                     CM-PART-LAST-NAME-A5.         
02563                                                                   
02564      MOVE CRTFNMEI               TO CM-INSURED-INITIAL1           
02565                                     CM-INSURED-INITIAL1-A2        
02566                                     CM-INSURED-INITIAL1-A5        
02567                                     CM-INSURED-FIRST-NAME.        
02568                                                                   
02569      IF CRTINITL GREATER ZERO                                     
02570          MOVE CRTINITI           TO CM-INSURED-INITIAL2           
02571                                     CM-INSURED-INITIAL2-A2        
02572                                     CM-INSURED-INITIAL2-A5        
02573      ELSE                                                         
02574          MOVE SPACES             TO CM-INSURED-INITIAL2           
02575                                     CM-INSURED-INITIAL2-A2        
02576                                     CM-INSURED-INITIAL2-A5
041002     END-IF.
02577                                                                   
02578      IF CM-INSURED-INITIALS = SPACES                              
02579          MOVE '**'               TO CM-INSURED-INITIALS
041002     END-IF.
02580                                                                   
02581      IF MEMBERL GREATER ZERO                                      
02582          MOVE MEMBERI              TO CM-MEMBER-NO                
02583      ELSE                                                         
02584          MOVE CERT-STATE           TO CM-MEMB-STATE               
02585          MOVE CERT-ACCOUNT-PRIME   TO CM-MEMB-ACCOUNT             
02586          MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A5      
02587          MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A5
041002     END-IF.
02588                                                                   
02589      IF JNTLNMEL GREATER ZERO                                     
02590          MOVE JNTLNMEI           TO CM-JT-LAST-NAME
041002     END-IF.
02591                                                                   
02592      IF JNTFNMEL GREATER ZERO                                     
02593          MOVE JNTFNMEI           TO CM-JT-FIRST-NAME
041002     END-IF.
02594                                                                   
02595      IF JNTINITL GREATER ZERO                                     
02596          MOVE JNTINITI           TO CM-JT-INITIAL
041002     END-IF.
02597                                                                   
02598      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.                      
02599                                                                   
02600      
           EXEC CICS HANDLE CONDITION
02601          DUPKEY   (3690-CERT-WRITTEN)
02602      END-EXEC.
02603                                                                   
02604      
           EXEC CICS WRITE
02605          DATASET   (ELCERT-DSID)
02606          FROM      (CERTIFICATE-MASTER)
02607          RIDFLD    (CM-CONTROL-PRIMARY)
02608      END-EXEC.
02609                                                                   
02610  3690-CERT-WRITTEN.                                               
02611                                                                   
02612      GO TO 3800-BUILD-NEW-CLAIM.                                  
02613                                                                   
02614  3690-EXIT.                                                       
02615       EXIT.                                                       
02616                                                                   
02617      EJECT                                                        
02618  3700-BUILD-CERT.                                                 
02619                                                                   
02620      IF CERTMTI NOT = 'A'                                         
02621          GO TO 3700-FINISH-CERT-BUILD
041002     END-IF.
02622                                                                   
02623      IF CERTMTI = 'A'                                             
02624          IF LCVCDL = ZEROS                                        
02625              MOVE ZEROS          TO CM-LF-BENEFIT-CD              
02626                                     CM-LF-ORIG-TERM               
02627                                     CM-LF-BENEFIT-AMT             
02628                                     CM-LF-PREMIUM-RATE            
02629              GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002         END-IF
041002     END-IF.
02630                                                                   
02631      IF LCVCDL GREATER ZERO                                       
02632          MOVE LCVCDI             TO CM-LF-BENEFIT-CD
041002     END-IF.
02633                                                                   
02634      IF LCVOTRML GREATER ZERO                                     
02635          MOVE LCVOTRMI           TO CM-LF-ORIG-TERM
041002     END-IF.
02636                                                                   
02637      IF LCVBENEL GREATER ZERO                                     
02638          MOVE WS-LCVBENE         TO CM-LF-BENEFIT-AMT
041002     END-IF.
02639                                                                   
02640      IF LCVRATEL GREATER ZERO                                     
02641          MOVE WS-LCVRATE         TO CM-LF-PREMIUM-RATE
041002     END-IF.
02642                                                                   
02643      IF WS-EXPIRE NOT = LOW-VALUES                                
02644          MOVE WS-EXPIRE          TO CM-LF-LOAN-EXPIRE-DT
041002     END-IF.
02645                                                                   
02646      IF LCVFORML GREATER ZERO                                     
02647          MOVE LCVFORMI           TO CM-POLICY-FORM-NO
041002     END-IF.
02648                                                                   
02649      IF LCVCNDTL = ZERO                                           
02650         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02651                                                                   
02652      IF WS-LCVCNDT NOT = LOW-VALUES                               
02653         MOVE '8'                 TO CM-LF-CURRENT-STATUS          
02654         MOVE WS-LCVCNDT          TO CM-LF-CANCEL-DT               
02655         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02656                                                                   
02657      IF CM-LF-CURRENT-STATUS = '7'                                
02658         MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS        
02659         MOVE SPACES              TO CM-LF-STATUS-AT-DEATH         
02660         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT           
02661                                     CM-LF-DEATH-DT                
02662         GO TO 3700-BUILD-AH-COVERAGE-SIDE
041002     END-IF.
02663                                                                   
02664      IF CM-LF-CURRENT-STATUS = '8'                                
02665         MOVE '1'                 TO CM-LF-CURRENT-STATUS          
02666         MOVE LOW-VALUES          TO CM-LF-CANCEL-DT
041002     END-IF.
02667                                                                   
02668  3700-BUILD-AH-COVERAGE-SIDE.                                     
02669                                                                   
02670      IF CERTMTI = 'A'                                             
02671          IF ACVCDL = ZEROS                                        
02672              MOVE ZEROS          TO CM-AH-BENEFIT-CD              
02673                                     CM-AH-ORIG-TERM               
02674                                     CM-AH-BENEFIT-AMT             
02675                                     CM-AH-PREMIUM-RATE            
02676              GO TO 3700-COVERAGE-BUILT
041002         END-IF
041002     END-IF.
02677                                                                   
02678      IF ACVCDL GREATER ZERO                                       
02679          MOVE ACVCDI             TO CM-AH-BENEFIT-CD
041002     END-IF.
02680                                                                   
02681      IF ACVOTRML GREATER ZERO                                     
02682          MOVE ACVOTRMI           TO CM-AH-ORIG-TERM
041002     END-IF.
02683                                                                   
02684      IF ACVBENEL GREATER ZERO                                     
02685          MOVE WS-ACVBENE         TO CM-AH-BENEFIT-AMT
041002     END-IF.
02686                                                                   
02687      IF ACVRATEL GREATER ZERO                                     
02688          MOVE WS-ACVRATE         TO CM-AH-PREMIUM-RATE
041002     END-IF.
02689                                                                   
02690      IF WS-EXPIRE NOT = LOW-VALUES                                
02691          MOVE WS-EXPIRE          TO CM-AH-LOAN-EXPIRE-DT
041002     END-IF.
02692                                                                   
02693      IF ACVFORML GREATER ZERO                                     
02694          MOVE ACVFORMI           TO CM-POLICY-FORM-NO
041002     END-IF.
02695                                                                   
02696      IF ACVCNDTL = ZERO                                           
02697         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02698                                                                   
02699      IF WS-ACVCNDT NOT = LOW-VALUES                               
02700         MOVE '8'                 TO CM-AH-CURRENT-STATUS          
02701         MOVE WS-ACVCNDT          TO CM-AH-CANCEL-DT               
02702         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02703                                                                   
02704      IF CM-AH-CURRENT-STATUS = '6'                                
02705         MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS   
02706         MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT    
02707         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT      
02708         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-DT           
02709         GO TO 3700-COVERAGE-BUILT
041002     END-IF.
02710                                                                   
02711      IF CM-AH-CURRENT-STATUS = '8'                                
02712         MOVE '1'                 TO CM-AH-CURRENT-STATUS          
02713         MOVE LOW-VALUES          TO CM-AH-CANCEL-DT
041002     END-IF.
02714                                                                   
02715  3700-COVERAGE-BUILT.

02716      IF ISSAGEL GREATER ZERO                                      
02717          MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE
041002     END-IF.
02718                                                                   
02719      IF JNTAGEL GREATER ZERO                                      
02720          MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE
041002     END-IF.
02721                                                                   
02722      IF SEXL GREATER ZERO                                         
02723          MOVE SEXI               TO CM-INSURED-SEX
041002     END-IF.
02724                                                                   
02725      IF APRL GREATER ZERO                                         
02726          MOVE WS-APR             TO CM-LOAN-APR
041002     END-IF.
02727                                                                   
02728      IF PMTFREQL GREATER ZERO                                     
02729          MOVE WS-PMTFREQ         TO CM-PAY-FREQUENCY
041002     END-IF.
02730                                                                   
02731      IF INDGRPL GREATER ZERO                                      
02732          MOVE INDGRPI            TO CM-IND-GRP-TYPE
041002     END-IF.
02733                                                                   
02734      IF CERTMTI = 'A'                                             
02735          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
041002     END-IF.
02736                                                                   
02737      IF REINCDL GREATER ZERO                                      
02738          MOVE REINCDI            TO CM-SPECIAL-REIN-CODE          
02739                                     WS-REIN-1                     
02740                                     WS-REIN-2                     
02741                                     WS-REIN-3                     
02742          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
041002     END-IF.
02743                                                                   
02744      IF WS-ADD-ON-DT NOT = LOW-VALUES                             
02748          MOVE WS-ADD-ON-DT   TO CM-LAST-ADD-ON-DT
041002     END-IF.
02749                                                                   
02750      IF CRTSSNL GREATER ZERO                                      
02751          MOVE CRTSSNI            TO CM-SOC-SEC-NO                 
02752      ELSE                                                         
02753          IF SSNL GREATER ZERO                                     
02754              MOVE SSNI           TO CM-SOC-SEC-NO
041002         END-IF
041002     END-IF.
02755                                                                   
02756      IF CRTSSNL = ZERO AND                                        
02757         SSNL    = ZERO                                            
02758          MOVE CM-STATE             TO CM-SSN-STATE                
02759          MOVE CM-ACCOUNT-PRIME     TO CM-SSN-ACCOUNT              
02760          MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A2      
02761          MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A2
041002     END-IF.
02762                                                                   
02770      IF CM-LF-CURRENT-STATUS = SPACES                             
02771          MOVE '1'                TO CM-LF-CURRENT-STATUS
041002     END-IF.
02772                                                                   
02773      IF CM-AH-CURRENT-STATUS = SPACES                             
02774          MOVE '1'                TO CM-AH-CURRENT-STATUS
041002     END-IF.
02775                                                                   
02776      IF CERTMTI = 'A'                                             
02777          MOVE SAVE-BIN-DATE      TO CM-ENTRY-DT
041002     END-IF.
02778                                                                   
02779  3700-FINISH-CERT-BUILD.                                          
02780                                                                   
02781      IF LOANNOL GREATER ZERO                                      
02782          MOVE LOANNOI            TO CM-LOAN-NUMBER
041002     END-IF.
02783                                                                   
02784      IF LOANBALL GREATER ZERO                                     
02785          MOVE HOLD-LOAN-BAL      TO CM-LOAN-BALANCE
041002     END-IF.
02786                                                                   
02787      IF PREMTYPL GREATER ZERO                                     
02788          MOVE PREMTYPI           TO CM-PREMIUM-TYPE
041002     END-IF.
02789                                                                   
02790  3700-EXIT.                                                       
02791       EXIT.                                                       
02792                                                                   
02793      EJECT                                                        
02794  3800-BUILD-NEW-CLAIM.                                            
CIDMOD                                                                  
CIDMOD*************************************************************     
CIDMOD*****           START OF ACTIVITY FILE PROCESSING          **     
CIDMOD*************************************************************     
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'
CIDMOD         PERFORM 6700-OUTPUT-ACTIVITY-RECORD THRU                 
CIDMOD                 6700-EXIT                                        
CIDMOD     END-IF.                                                      
CIDMOD*************************************************************     
CIDMOD*****            END OF ACTIVITY FILE PROCESSING           **     
CIDMOD*************************************************************     
CIDMOD                                                                  
02814                                                                   
02795      
           EXEC CICS GETMAIN
02796          SET     (ADDRESS OF CLAIM-MASTER)
02797          LENGTH  (ELMSTR-LENGTH)
02798      END-EXEC.
02799                                                                   
02800      MOVE SPACES TO CLAIM-MASTER.                                 
02801                                                                   
02802      MOVE LOW-VALUES   TO       CL-INSURED-BIRTH-DT CL-INCURRED-DT
02803          CL-NEXT-FOLLOWUP-DT    CL-REPORTED-DT      CL-LAST-PMT-DT
02804          CL-EST-END-OF-DISAB-DT CL-PAID-THRU-DT   CL-LAST-CLOSE-DT
02805          CL-LAST-REOPEN-DT      CL-PURGED-DT        CL-RESTORED-DT
02806          CL-NEXT-AUTO-PAY-DT    CL-NEXT-RESEND-DT                 
02807          CL-LAST-ADD-ON-DT      CL-FILE-ESTABLISH-DT              
02808          CL-LAST-MAINT-DT       CL-HISTORY-ARCHIVE-DT             
061013         CL-ACTIVITY-MAINT-DT   cl-benefit-expiration-dt
02810                                                                   
02811      MOVE +0 TO CL-LAST-PMT-AMT      CL-TOTAL-PAID-AMT            
02812              CL-NO-OF-PMTS-MADE      CL-NO-OF-DAYS-PAID           
02813              CL-LAST-INC-DT-CHANGE   CL-AUTO-PAY-SEQ              
02814              CL-INSURED-ADDR-CNT     CL-ACCOUNT-ADDR-CNT          
02815              CL-BENIF-ADDR-CNT       CL-EMPLOYER-ADDR-CNT         
02816              CL-DOCTOR-ADDR-CNT      CL-OTHER-1-ADDR-CNT          
02817              CL-OTHER-2-ADDR-CNT     CL-FATAL-ERROR-CNT           
02818              CL-CLAIM-PAYMENT-STATUS CL-LAST-MAINT-HHMMSS         
02819              CL-FORCEABLE-ERROR-CNT.                
050807
050807     MOVE WS-INSURED-ADDR-CNT    TO CL-INSURED-ADDR-CNT.              
02820                                                                   
02821      MOVE ZEROS                  TO CL-ACTIVITY-CODE              
02822                                     CL-LAPSE-REPORT-CODE          
02823                                     CL-LAG-REPORT-CODE
061013                                    CL-CRITICAL-PERIOD
061013*                                   CL-CRIT-PER-RTW-MOS
           move 01                     to cl-benefit-period
02824                                                                   
02825      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.       
02826      MOVE +4095                  TO CL-TRAILER-SEQ-CNT.           
02827                                                                   
02828      IF BENECDL NOT = ZEROS                                       
02829         MOVE BENECDI             TO CL-BENEFICIARY
041002     END-IF.
02830                                                                   
02831      MOVE 'CL'                   TO CL-RECORD-ID.                 
02832      MOVE 'CR'                   TO CL-SYSTEM-IDENTIFIER.         
02833                                                                   
02834      MOVE PI-COMPANY-CD  TO  CL-COMPANY-CD     CL-COMPANY-CD-A1   
02835                              CL-COMPANY-CD-A2  CL-COMPANY-CD-A4   
02836                              CL-COMPANY-CD-A5.                    
02837                                                                   
02838      MOVE CLMCARRI               TO CL-CARRIER.                   
02839                                                                   
02840      IF CLMNOL = ZERO                                             
02841          MOVE WS-CLAIM-NUMBER    TO CL-CLAIM-NO                   
02842      ELSE                                                         
02843          MOVE CLMNOI             TO CL-CLAIM-NO
041002     END-IF.
02844                                                                   
02845      MOVE CERTNOI                TO CL-CERT-PRIME                 
02846                                     CL-CERT-A4-PRIME.             
02847                                                                   
02848      MOVE SUFXI                  TO CL-CERT-SFX                   
02849                                     CL-CERT-A4-SFX.               
02850                                                                   
02851      MOVE PCERTNOI               TO CL-PRIME-CERT-PRIME.          
02852                                                                   
02853      MOVE PSUFXI                 TO CL-PRIME-CERT-SFX.            
02854                                                                   
02855      MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.           
02856                                                                   
02857      MOVE LSTNMEI                TO CL-INSURED-LAST-NAME.         
02858                                                                   
02859      MOVE WS-TODAY-DT            TO CL-FILE-ESTABLISH-DT.         
02860      MOVE ' '                    TO CL-LAST-MAINT-TYPE.           
02861                                                                   
02862      PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.              
02863                                                                   
02864      IF CERTMTI = 'A'                                             
02865          MOVE '2'                TO CL-CERT-ORIGIN                
02866      ELSE                                                         
02867          MOVE '1'                TO CL-CERT-ORIGIN
041002     END-IF.
02868                                                                   
02869      MOVE CERT-CARRIER        TO CL-CERT-CARRIER   PI-CARRIER.    
02870      MOVE CERT-GROUPING       TO CL-CERT-GROUPING  PI-GROUPING.   
02871      MOVE CERT-STATE          TO CL-CERT-STATE     PI-STATE.      
02872      MOVE CERT-ACCOUNT        TO CL-CERT-ACCOUNT   PI-ACCOUNT.    
02873      MOVE WS-EFFDT            TO CL-CERT-EFF-DT    PI-CERT-EFF-DT.
02874                                                                   
02875  3820-REWRITE-CONTROL-FILE.                                       
02876                                                                   
02877      IF CLMNOL GREATER ZERO                                       
02878          GO TO 3830-SEQUENCE-CLAIM-RECORDS
041002     END-IF.
02879                                                                   
02880      IF CF-RECORD-TYPE = '6'                                      
02881          MOVE SAVE-COUNTER    TO  CF-CLAIM-COUNTER                
02882      ELSE                                                         
02883          MOVE SAVE-COUNTER    TO  CF-CO-CLAIM-COUNTER
041002     END-IF.
02884                                                                   
02885      
           EXEC CICS REWRITE
02886          DATASET   (ELCNTL-DSID)
02887          FROM      (CONTROL-FILE)
02888      END-EXEC.
02889                                                                   
02890  3830-SEQUENCE-CLAIM-RECORDS.                                     
02891                                                                   
02892      MOVE CLAIM-MASTER           TO WS-SAVE-CLAIM-MASTER.         
02893      MOVE CL-CONTROL-PRIMARY     TO ELMSTR-KEY.                   
02894      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.                  
02895                                                                   
02896      IF WS-ASSOC-CERT-TOTAL = ZERO                                
02897          MOVE 1                  TO CL-ASSOC-CERT-SEQU            
02898                                     CL-ASSOC-CERT-TOTAL           
02899          GO TO 3840-WRITE-CLAIM-MASTER
041002     END-IF.
02900                                                                   
02901      IF CL-CONTROL-PRIMARY LESS THAN WS-SAVE-CLAIM-KEY            
02902          MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY                    
02903      ELSE                                                         
02904          MOVE WS-SAVE-CLAIM-KEY  TO ELMSTR-KEY
041002     END-IF.
02905                                                                   
02906      MOVE WS-SAVE-CLAIM-MASTER   TO CLAIM-MASTER.                 
02907      MOVE ZERO                   TO CL-ASSOC-CERT-SEQU.           
02908      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.          
02909                                                                   
02910  3840-WRITE-CLAIM-MASTER.                                         
02911                                                                   
02926      PERFORM 6600-CHECK-AUTO-ACTIVITY THRU 6600-EXIT.             
02927      IF WS-REC-FOUND-SW = 'Y'                                     
02928          MOVE 01                 TO  CL-ACTIVITY-CODE             
02929          MOVE SAVE-BIN-DATE      TO  CL-ACTIVITY-MAINT-DT         
02930          MOVE 'ADD '             TO  CL-ACTIVITY-MAINT-TYPE
041002     END-IF.
02931                                                                   
02932      
           EXEC CICS HANDLE CONDITION
02933           DUPKEY    (3850-CLAIM-MSTR-WRITTEN)
02934      END-EXEC.
02935                                                                   
02936 ** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.                  
02937      MOVE CL-CERT-NO             TO CL-CCN-A5.                    

110921     move 'CLMS'                 to pa-rec-type
110921     move pi-company-id          to pa-company-id
110921     move claim-master           to pa-rest-of-record
110921     
110921     EXEC CICS LINK                                               
110921         PROGRAM  ('WF001')
110921         COMMAREA (work-flow-pass-area)
110921         LENGTH   (604)
110921     END-EXEC

           EXEC CICS WRITE
02940          DATASET   (ELMSTR-DSID)
02941          FROM      (CLAIM-MASTER)
02942          RIDFLD    (CL-CONTROL-PRIMARY)
02943      END-EXEC.
02944                                                                    
02945  3850-CLAIM-MSTR-WRITTEN.                                         
02946                                                                   
02947      IF WS-ASSOC-CERT-TOTAL NOT = ZERO                            
02948          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
041002     END-IF.
02949                                                                   
02950      EJECT                                                        
02951  3990-ADD-DONE.                                                   
PEMTST*    MOVE 1                     TO EMI-SWITCH1  EMI-SWITCH-AREA-1 
PEMTST*                                  EMI-SUB      EMI-SWITCH-AREA-2.
PEMTST*    MOVE SPACES                TO EMI-ACTION-SWITCH              
PEMTST*                                  EMI-ERROR-LINES.               
02956                                                                   
02957      IF CLMNOL = ZERO                                             
               move spaces             to emi-claim-no
02958          MOVE ER-0265            TO EMI-ERROR                     
02959          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
02960          MOVE WS-CLAIM-NUMBER    TO EMI-TEXT-VARIABLE (EMI-SUB)         
02961                                     PI-LAST-CLAIM                 
02962      ELSE                                                         
02963          MOVE CLMNOI             TO PI-LAST-CLAIM                 
042413                                    MSTR-CLAIM-NO
02964          MOVE ER-0000            TO EMI-ERROR                     
02965          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
061511
100518     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
032514       AND (PI-ST-VFY-2ND-BENE = 'L' OR 'B')
061511         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
061511         MOVE ER-7575            TO EMI-ERROR
061511         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511     END-IF.
032514
100518     IF CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
032514       AND (PI-ST-VFY-2ND-BENE = 'A' OR 'B')
032514         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
032514         MOVE ER-7582            TO EMI-ERROR
032514         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032514     END-IF.
050807
101917     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
101917        PERFORM 3991-CHECK-2-YEAR-CONTESTABLE THRU 3991-EXIT
101917     END-IF.

050807     IF WS-NO-INSURED-ADDRESS = 'Y'
050807         MOVE ER-3548            TO EMI-ERROR                     
050807         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
050807     END-IF.
020613
020613     IF PI-ST-CAUSAL-STATE = 'B'  OR
020613       (PI-ST-CAUSAL-STATE = 'L' AND 
100518             (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))   OR
020613       (PI-ST-CAUSAL-STATE = 'A' AND
020614              CLMTYPEI = PI-AH-OVERRIDE-L1)
020613         PERFORM 3994B-BUILD-NINETY-FOUR-TRAILER THRU 3994B-EXIT
020613         MOVE ER-7577            TO EMI-ERROR
020613         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020613     END-IF.

020816     if pi-company-id = 'DCC' OR 'VPP'
061013        PERFORM 3994C-BUILD-NINETY-FIVE-TRAILER
061013                                 THRU 3994C-EXIT
061013     end-if

071720*052918IF CLMTYPEI = PI-AH-OVERRIDE-L1
071720*052918  AND PI-COMPANY-ID = 'CID'
071720*052918   SET ELAPSED-BETWEEN-BIN TO TRUE
071720*052918   MOVE ZERO               TO DC-ELAPSED-MONTHS
071720*052918                              DC-ELAPSED-DAYS
071720*052918
071720*052918   MOVE CL-INCURRED-DT  TO DC-BIN-DATE-1
071720*052918   MOVE WS-TODAY-DT TO DC-BIN-DATE-2
071720*052918   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
071720*052918   IF DC-ODD-DAYS-OVER > ZERO
071720*052918      ADD 1 TO DC-ELAPSED-MONTHS
071720*052918   END-IF
071720*052918
071720*052918   IF PI-STATE = 'HI'
071720*052918     AND DC-ELAPSED-MONTHS <= 18
071720*052918      CONTINUE
071720*052918   ELSE
071720*052918      IF DC-ELAPSED-MONTHS > 15
071720*052918         MOVE ER-7572            TO EMI-ERROR
071720*052918         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071720*052918         PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
071720*052918      END-IF
071720*052918   END-IF
071720*052918
071720*052918END-IF.

090821     if clmtypei = 'L' or 'O'
090821        move cm-lf-benefit-cd    to ws-ben-hold
090821        move '4'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-lf-special-calc-cd
090821        end-if
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-hold
090821        move '5'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-ah-special-calc-cd
090821        end-if
090821     end-if
090821
090821     move ' '                    to ws-mob-cert-ind
090821     if pi-company-id = 'CID'
090821        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
090821           and (ws-lf-special-calc-cd = 'O'))
090821                      or
090821           ((clmtypei <> pi-life-override-l1 and 'O')
090821           and (ws-ah-special-calc-cd = 'O'))
090821           set mob-cert to true
090821        end-if
090821     else
090821        if (pi-company-id = 'DCC')
090821           and (cm-carrier = '7')
090821           set mob-cert to true
090821        end-if
090821     end-if
090821 
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821 
090821     if (ws-incur >= ws-hi-acct-dt)
090821        and (acct-cancelled)
090821        and (mob-cert)
090821        MOVE er-1682             TO EMI-ERROR
090821        MOVE -1                  TO INCURL
090821        MOVE AL-UABON            TO INCURA
090821        PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821        PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821     end-if
090821
090821     if ws-incur < cm-cert-eff-dt
090821        MOVE er-1683             TO EMI-ERROR
090821        MOVE -1                  TO INCURL
090821        MOVE AL-UABON            TO INCURA
090821        PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821        PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821     end-if

           .
       3990-continue.

02967      MOVE CLMNOI                     TO  PI-CLAIM-NO.             
02968      MOVE CLMCARRI                   TO  PI-CARRIER.              
02969      MOVE CERTNOI                    TO  PI-CERT-PRIME.           
02970      MOVE SUFXI                      TO  PI-CERT-SFX.             
02971                                                                   
02972      IF WS-LETTER-SW = 'Y'                                        
02973          PERFORM 6600-START-AUTO-LETTER-WRITER THRU 6600-EXIT     
02974          IF W-1523-ERROR-CODE NOT = ZEROS                         
02975              MOVE W-1523-ERROR-CODE  TO  EMI-ERROR                
02976              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
02977                                                                   
061013     perform 3998-update-cert-claim-trailer
061013                                 thru 3998-exit

02978      MOVE CLMTYPEI               TO PI-LAST-CLAIM-TYPE.           
02979      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.           
02980      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.            
02981      MOVE CLMCARRI               TO PI-LAST-CARR.                 
02982                                                                   
02983      IF PRTOPTL GREATER THAN 0                                    
02984          MOVE PRTOPTI            TO PI-PRT-OPT
041002     END-IF.

02985      IF ALTPRTL GREATER THAN 0                                    
02986          MOVE ALTPRTI            TO PI-ALT-PRT
041002     END-IF.
02987                                                                   
02988      IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT        
02989          MOVE LOW-VALUES         TO EL130AI                       
02990          MOVE PI-LAST-CERT-PRIME TO CERTNOO                       
PEMMOD         MOVE AL-SANON           TO CERTNOA                       
02992          MOVE PI-LAST-CERT-SUFX  TO SUFXO                         
PEMMOD         MOVE AL-SANON           TO SUFXA                         
02994          MOVE PI-LAST-CARR       TO CLMCARRO                      
02995          MOVE AL-UANON           TO CLMCARRA                      
02996          MOVE PI-LAST-CLAIM      TO CLMNOO                        
02997          MOVE AL-UANON           TO CLMNOA                        
02998          MOVE PI-LAST-CLAIM-TYPE TO CLMTYPEO                      
02999          MOVE AL-UANON           TO CLMTYPEA                      
03000      ELSE                                                         
03001          PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT              
03002          PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT
041002     END-IF.
03003                                                                   
03004      IF PI-PRT-OPT = 'N' OR 'L'                                   
03005          MOVE PI-PRT-OPT         TO PRTOPTO                       
03006          MOVE 1                  TO PRTOPTL
041002     END-IF.
03007                                                                   
03008      IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES                    
03009          MOVE PI-ALT-PRT         TO ALTPRTO                       
03010          MOVE 4                  TO ALTPRTL
041002     END-IF.
03011                                                                   
03012      IF PRTOPTL GREATER THAN 0                                    
03013          IF PRTOPTI = 'L'                                         
03014              GO TO 0400-CREATE-ELACTQ                             
03015          ELSE                                                     
03016              GO TO 0480-PRINT-NOW
041002         END-IF
041002     END-IF.

061013     move 'S'                    to mainti
061013     PERFORM 0500-CREATE-TEMP-STORAGE
061013                                 THRU 0599-EXIT

03018      IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT        
03019          GO TO 8100-SEND-INITIAL-MAP                              
03020      ELSE                                                         
03021          MOVE LOW-VALUES         TO MAINTO                        
03022          MOVE -1                 TO MAINTL                        
03023          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03024                                                                   
03025      EJECT
101917 3991-CHECK-2-YEAR-CONTESTABLE.
101917     SET ELAPSED-BETWEEN-BIN TO TRUE
101917     MOVE ZERO               TO DC-ELAPSED-MONTHS
101917                                DC-ELAPSED-DAYS
101917
101917     MOVE PI-SAVE-EFFDT  TO DC-BIN-DATE-1.
101917     MOVE CL-INCURRED-DT TO DC-BIN-DATE-2.
101917     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
101917     IF DC-ODD-DAYS-OVER > ZERO
101917        ADD 1 TO DC-ELAPSED-MONTHS
101917     END-IF
101917
101917     IF DC-ELAPSED-MONTHS <= 24
101917        MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
101917        MOVE ZERO           TO DC-ELAPSED-MONTHS
101917                               DC-ELAPSED-DAYS
101917        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
101917        IF DC-ODD-DAYS-OVER > ZERO
101917           ADD 1 TO DC-ELAPSED-MONTHS
101917        END-IF
101917        IF DC-ELAPSED-MONTHS > 24
101917           MOVE ER-1679            TO EMI-ERROR
101917           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
101917           PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
101917        END-IF
101917     END-IF.
101917
101917 3991-EXIT.
101917     EXIT.
101917 3992-BUILD-TRAILER.
101917
101917     MOVE SPACES             TO AT-GENERAL-INFO-TR
101917     INITIALIZE AT-GENERAL-INFO-TR
101917     MOVE ELMSTR-KEY         TO AT-CONTROL-PRIMARY
101917     MOVE 'AT'               TO AT-RECORD-ID
090821     evaluate true
090821        when emi-error = er-1679
090821           move er-1679-text     to at-info-line-1
090821        when emi-error = er-1682
090821           move er-1682-text     to at-info-line-1
090821        when emi-error = er-1683
090821           move er-1683-text     to at-info-line-1
090821     end-evaluate
090821     move +97                to at-sequence-no
061418*     IF EMI-ERROR = ER-7572
061418*        MOVE +97             TO AT-SEQUENCE-NO
061418*     ELSE
101917*        MOVE +96             TO AT-SEQUENCE-NO
061418*     END-IF
101917     MOVE '6'                TO AT-TRAILER-TYPE
101917     MOVE WS-TODAY-DT        TO AT-RECORDED-DT
101917                                AT-GEN-INFO-LAST-MAINT-DT
101917     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
101917                                AT-GEN-INFO-LAST-UPDATED-BY
101917     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS
101917
061418*     IF EMI-ERROR = ER-7572
061418*        MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
061418*     ELSE
101917*        MOVE EMI-LINE2 TO AT-INFO-LINE-1
061418*     END-IF
101917     MOVE SPACES             TO AT-INFO-LINE-2.
101917
101917 3992-WRITE.
101917
101917     EXEC CICS HANDLE CONDITION
101917         DUPREC    (3992-DUPREC)
101917     END-EXEC.
101917
101917     EXEC CICS WRITE
101917          DATASET     ('ELTRLR')
101917          FROM        (ACTIVITY-TRAILERS)
101917          RIDFLD      (AT-CONTROL-PRIMARY)
101917      END-EXEC.
101917
101917     GO TO 3992-EXIT.
101917
101917 3992-DUPREC.
101917     SUBTRACT +1 FROM AT-SEQUENCE-NO.
101917     GO TO 3992-WRITE.
101917
101917 3992-EXIT.
101917     EXIT.

03026  3993-MODIFY-SCREEN-ATTRB.                                        
03027                                                                   
03028      IF CERTMTL GREATER ZERO                                      
03029          MOVE AL-UANON           TO CERTMTA
041002     END-IF.
03030                                                                   
03031      IF EFFDTL GREATER ZERO                                       
03032          MOVE AL-UANON           TO EFFDTA
041002     END-IF.
03033                                                                   
03034      IF ACCOUNTL GREATER ZERO                                     
03035          MOVE AL-UANON           TO ACCOUNTA
041002     END-IF.
03036                                                                   
03037      IF STATEL GREATER ZERO                                       
03038          MOVE AL-UANON           TO STATEA
041002     END-IF.
03039                                                                   
03040      IF CRTCARRL GREATER ZERO                                     
03041          MOVE AL-UANON           TO CRTCARRA
041002     END-IF.
03042                                                                   
03043      IF GROUPL GREATER ZERO                                       
03044          MOVE AL-UANON           TO GROUPA
041002     END-IF.
03045                                                                   
03046      IF CRTLNMEL GREATER ZERO                                     
03047          MOVE AL-UANON           TO CRTLNMEA
041002     END-IF.
03048                                                                   
03049      IF CRTFNMEL GREATER ZERO                                     
03050          MOVE AL-UANON           TO CRTFNMEA
041002     END-IF.
03051                                                                   
03052      IF CRTINITL GREATER ZERO                                     
03053          MOVE AL-UANON           TO CRTINITA
041002     END-IF.
03054                                                                   
03055      IF ISSAGEL GREATER ZERO                                      
03056          MOVE AL-UNNON           TO ISSAGEA
041002     END-IF.
03057                                                                   
03058      IF JNTLNMEL GREATER ZERO                                     
03059          MOVE AL-UANON           TO JNTLNMEA
041002     END-IF.
03060                                                                   
03061      IF JNTFNMEL GREATER ZERO                                     
03062          MOVE AL-UANON           TO JNTFNMEA
041002     END-IF.
03063                                                                   
03064      IF JNTINITL GREATER ZERO                                     
03065          MOVE AL-UANON           TO JNTINITA
041002     END-IF.
03066                                                                   
03067      IF JNTAGEL GREATER ZERO                                      
03068          MOVE AL-UNNON           TO JNTAGEA
041002     END-IF.
03069                                                                   
03070      IF CRTSSNL GREATER ZERO                                      
03071          MOVE AL-UANON           TO CRTSSNA
041002     END-IF.
03072                                                                   
03073      IF ACVDSCRL GREATER ZERO                                     
03074          MOVE AL-SANON           TO ACVDSCRA
041002     END-IF.
03075                                                                   
03076      IF ACVKINDL GREATER ZERO                                     
03077          MOVE AL-SANON           TO ACVKINDA
041002     END-IF.
03078                                                                   
03079      IF ACVCDL GREATER ZERO                                       
03080          MOVE AL-UANON           TO ACVCDA
041002     END-IF.
03081                                                                   
03082      IF ACVOTRML GREATER ZERO                                     
03083          MOVE AL-UNNON           TO ACVOTRMA
041002     END-IF.
03084                                                                   
03085      IF ACVRTRML GREATER ZERO                                     
03086          MOVE AL-UNNON           TO ACVRTRMA
041002     END-IF.
03087                                                                   
03088      IF ACVRATEL GREATER ZERO                                     
03089          MOVE AL-UNNON           TO ACVRATEA
041002     END-IF.
03090                                                                   
03091      IF ACVBENEL GREATER ZERO                                     
03092          MOVE AL-UNNON           TO ACVBENEA
041002     END-IF.
03093                                                                   
03094      IF ACVFORML GREATER ZERO                                     
03095          MOVE AL-UANON           TO ACVFORMA
041002     END-IF.
03096                                                                   
03097      IF ACVCNDTL GREATER ZERO                                     
03098          MOVE AL-UANON           TO ACVCNDTA
041002     END-IF.
03099                                                                   
03100      IF ACVEXITL GREATER ZERO                                     
03101          MOVE AL-UANON           TO ACVEXITA
041002     END-IF.
03102                                                                   
03103      IF ACVSTATL GREATER ZERO                                     
03104          MOVE AL-UANON           TO ACVSTATA
041002     END-IF.
03105                                                                   
03106      IF LCVDSCRL GREATER ZERO                                     
03107          MOVE AL-SANON           TO LCVDSCRA
041002     END-IF.
03108                                                                   
03109      IF LCVKINDL GREATER ZERO                                     
03110          MOVE AL-SANON           TO LCVKINDA
041002     END-IF.
03111                                                                   
03112      IF LCVCDL GREATER ZERO                                       
03113          MOVE AL-UANON           TO LCVCDA
041002     END-IF.
03114                                                                   
03115      IF LCVOTRML GREATER ZERO                                     
03116          MOVE AL-UNNON           TO LCVOTRMA
041002     END-IF.
03117                                                                   
03118      IF LCVRTRML GREATER ZERO                                     
03119          MOVE AL-UNNON           TO LCVRTRMA
041002     END-IF.
03120                                                                   
03121      IF LCVRATEL GREATER ZERO                                     
03122          MOVE AL-UNNON           TO LCVRATEA
041002     END-IF.
03123                                                                   
03124      IF LCVBENEL GREATER ZERO                                     
03125          MOVE AL-UNNON           TO LCVBENEA
041002     END-IF.
03126                                                                   
03127      IF LCVFORML GREATER ZERO                                     
03128          MOVE AL-UANON           TO LCVFORMA
041002     END-IF.
03129                                                                   
03130      IF LCVCNDTL GREATER ZERO                                     
03131          MOVE AL-UANON           TO LCVCNDTA
041002     END-IF.
03132                                                                   
03133      IF LCVEXITL GREATER ZERO                                     
03134          MOVE AL-UANON           TO LCVEXITA
041002     END-IF.
03135                                                                   
03136      IF LCVSTATL GREATER ZERO                                     
03137          MOVE AL-UANON           TO LCVSTATA
041002     END-IF.
03138                                                                   
03139      IF MEMCAPL GREATER ZERO                                      
03140          MOVE AL-UANON           TO MEMCAPA
041002     END-IF.
03141                                                                   
03142      IF APRL GREATER ZERO                                         
03143          MOVE AL-UNNON           TO APRA
041002     END-IF.
03144                                                                   
03145      IF PMTFREQL GREATER ZERO                                     
03146          MOVE AL-UNNON           TO PMTFREQA
041002     END-IF.
03147                                                                   
03148      IF INDGRPL GREATER ZERO                                      
03149          MOVE AL-UANON           TO INDGRPA
041002     END-IF.
03150
03151      IF PREMTYPL GREATER ZERO                                     
03152          MOVE AL-UANON           TO PREMTYPA
041002     END-IF.
03153                                                                   
03154      IF REINCDL GREATER ZERO                                      
03155          MOVE AL-UANON           TO REINCDA
041002     END-IF.
03156                                                                   
03157      IF ADDONDTL GREATER ZERO                                     
03158          MOVE AL-UANON           TO ADDONDTA
041002     END-IF.
03159
03160      IF MEMBERL GREATER ZERO                                      
03161          MOVE AL-UANON           TO MEMBERA
041002     END-IF.
03162                                                                   
03163      IF BCERT1L GREATER ZERO                                      
03164          MOVE AL-SANON           TO BCERT1A                       
03165                                     BSUFX1A
041002     END-IF.
03166                                                                   
03167      IF BCERT2L GREATER ZERO                                      
03168          MOVE AL-SANON           TO BCERT2A                       
03169                                     BSUFX2A
041002     END-IF.
03170                                                                   
03171      IF BCERT3L GREATER ZERO                                      
03172          MOVE AL-SANON           TO BCERT3A                       
03173                                     BSUFX3A
041002     END-IF.
03174                                                                   
03175      IF BCERT4L GREATER ZERO                                      
03176          MOVE AL-SANON           TO BCERT4A                       
03177                                     BSUFX4A
041002     END-IF.
03178                                                                   
03179      IF BCERT5L GREATER ZERO                                      
03180          MOVE AL-SANON           TO BCERT5A                       
03181                                     BSUFX5A
041002     END-IF.
03182                                                                   
03183  3993-MODIFY-CLAIM-ATTRB.                                         
03184                                                                   
03185      IF PCERTNOL GREATER ZERO                                     
03186          MOVE AL-UANON           TO PCERTNOA
041002     END-IF.
03187                                                                   
03188      IF PSUFXL GREATER ZERO                                       
03189          MOVE AL-UANON           TO PSUFXA
041002     END-IF.
03190                                                                   
03191      IF LSTNMEL GREATER ZERO                                      
03192          MOVE AL-UANON           TO LSTNMEA
041002     END-IF.
03193                                                                   
03194      IF FSTNMEL GREATER ZERO                                      
03195          MOVE AL-UANON           TO FSTNMEA
041002     END-IF.
03196                                                                   
03197      IF INITL GREATER ZERO                                        
03198          MOVE AL-UANON           TO INITA
041002     END-IF.

03199                                                                   
03200      IF SEXL GREATER ZERO                                         
03201          MOVE AL-UANON           TO SEXA
041002     END-IF.
03202                                                                   
03206      IF BIRTHDTL GREATER ZERO                                     
03207          MOVE AL-UANON           TO BIRTHDTA
041002     END-IF.
03208                                                                   
03209      IF SSNL GREATER ZERO                                         
03210          MOVE AL-UANON           TO SSNA
041002     END-IF.
03211                                                                   
03212      IF INCURL GREATER ZERO                                       
03213          MOVE AL-UANON           TO INCURA
041002     END-IF.
03214                                                                   
03215      IF REPORTL GREATER ZERO                                      
03216          MOVE AL-UANON           TO REPORTA
041002     END-IF.
03217                                                                   
040714*    IF ESTENDL GREATER ZERO                                      
040714*        MOVE AL-UANON           TO ESTENDA
040714*    END-IF.
03220                                                                   
03221      IF DIAGL GREATER ZERO                                        
03222          MOVE AL-UANON           TO DIAGA
041002     END-IF.
040714*
040814     IF ICD1L GREATER ZERO
040814         MOVE AL-UANON           TO ICD1A
040814     END-IF.
040814
040814     IF ICD2L GREATER ZERO
040814         MOVE AL-UANON           TO ICD2A
040814     END-IF.
03223                                                                   
040714*    IF CAUSEL GREATER ZERO                                       
040714*        MOVE AL-UANON           TO CAUSEA
040714*    END-IF.
03226                                                                   
03227      IF MANRSVL GREATER ZERO                                      
03228          MOVE AL-UNNON           TO MANRSVA
041002     END-IF.
03229                                                                   
03230      IF RELCLML GREATER ZERO                                      
03231          MOVE AL-UANON           TO RELCLMA
041002     END-IF.
03232                                                                   
03233      IF LOANNOL GREATER ZERO                                      
03234          MOVE AL-UANON           TO LOANNOA
041002     END-IF.
03235                                                                   
03236      IF LOANBALL GREATER ZERO                                     
03237          MOVE AL-UNNON           TO LOANBALA
041002     END-IF.
03238                                                                   
03239      IF PROCCDL GREATER ZERO                                      
03240          MOVE AL-UANON           TO PROCCDA
041002     END-IF.
03241                                                                   
03242      IF BENECDL GREATER THAN ZERO                                 
03243          MOVE AL-UANON           TO  BENECDA
041002     END-IF.
03244                                                                   
03245      IF PRICDL GREATER ZERO                                       
03246          MOVE AL-UANON           TO PRICDA
041002     END-IF.
03247                                                                   
03252      IF SUPVL GREATER ZERO                                        
03253          MOVE AL-UANON           TO SUPVA
041002     END-IF.
03254                                                                   
03255      IF FILETOL GREATER ZERO                                      
03256          MOVE AL-UANON           TO FILETOA
041002     END-IF.
03257                                                                   
03258                                                                   
03259  3993-MODIFY-CLAIM-KEY-ATTRB.                                     
03260                                                                   
03261      IF CLMNOL GREATER ZERO                                       
03262          MOVE AL-UANON           TO CLMNOA
041002     END-IF.
03263                                                                   
03264      IF CLMCARRL GREATER ZERO                                     
03265          MOVE AL-UANON           TO CLMCARRA
041002     END-IF.
03266                                                                   
03267      IF CLMTYPEL GREATER ZERO                                     
03268          MOVE AL-UANON           TO CLMTYPEA
041002     END-IF.
03269                                                                   
PEMMOD     IF CERTNOL GREATER ZERO                                      
PEMMOD         MOVE AL-SANON           TO CERTNOA
041002     END-IF.
03272                                                                   
03273      IF SUFXL GREATER ZERO                                        
PEMMOD         MOVE AL-SANON           TO SUFXA
041002     END-IF.
03275                                                                   
03276  3993-EXIT.                                                       
03277      EXIT.                                                        
03278                                                                   
03279      EJECT                                                        
061511 3994-BUILD-NINETY-THREE-TRAILER.                                       
061511                                     
061511     MOVE SPACES                 TO ACTIVITY-TRAILERS.            
061511     MOVE 'AT'                   TO AT-RECORD-ID.                 
061511                                                                  
061511     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
061511     MOVE CLMCARRI               TO AT-CARRIER.                   
061511                                                                  
061511     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
061511     MOVE CERT-CERT-NO           TO AT-CERT-NO.                   
061511     MOVE +93                    TO AT-SEQUENCE-NO.               
061511     MOVE '6'                    TO AT-TRAILER-TYPE. 
061511     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.             
061511                                                                  
100518     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
032514         MOVE WS-VERIFY-NOTE     TO AT-INFO-LINE-1
032514     ELSE
032514         MOVE WS-VFY-SSN-NOTE    TO AT-INFO-LINE-1
032514     END-IF
061511                                                                  
061511     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT                
061511                                    AT-GEN-INFO-LAST-MAINT-DT.    
061511                                                                  
061511     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
061511                                    AT-GEN-INFO-LAST-UPDATED-BY.  
061511     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
061511                                                                  
061511     
061511     EXEC CICS WRITE
061511         DATASET   (ELTRLR-DSID)
061511         FROM      (ACTIVITY-TRAILERS)
061511         RIDFLD    (AT-CONTROL-PRIMARY)
061511     END-EXEC.
061511
061511 3994-EXIT.
061511     EXIT.
061511
020613 3994B-BUILD-NINETY-FOUR-TRAILER.                                       
020613                                     
020613     MOVE SPACES                 TO ACTIVITY-TRAILERS.            
020613     MOVE 'AT'                   TO AT-RECORD-ID.                 
020613                                                                  
020613     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
020613     MOVE CLMCARRI               TO AT-CARRIER.                   
020613                                                                  
020613     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
020613     MOVE CERT-CERT-NO           TO AT-CERT-NO.                   
020613     MOVE +94                    TO AT-SEQUENCE-NO.               
020613     MOVE '6'                    TO AT-TRAILER-TYPE. 
020613     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.             
020613                                                                  
020613     MOVE WS-CAUSAL-NOTE         TO AT-INFO-LINE-1.
020613                                                                  
020613     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT                
020613                                    AT-GEN-INFO-LAST-MAINT-DT.    
020613                                                                  
020613     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
020613                                    AT-GEN-INFO-LAST-UPDATED-BY.  
020613     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
020613                                                                  
020613     
020613     EXEC CICS WRITE
020613         DATASET   (ELTRLR-DSID)
020613         FROM      (ACTIVITY-TRAILERS)
020613         RIDFLD    (AT-CONTROL-PRIMARY)
020613     END-EXEC.
020613
020613 3994B-EXIT.
020613     EXIT.

061013 3994C-BUILD-NINETY-FIVE-TRAILER.
061013
061013     MOVE SPACES                 TO ACTIVITY-TRAILERS
061013     MOVE 'AT'                   TO AT-RECORD-ID
061013
061013     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD
061013     MOVE CLMCARRI               TO AT-CARRIER
061013
061013     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO
061013     MOVE CERT-CERT-NO           TO AT-CERT-NO
061013     MOVE +95                    TO AT-SEQUENCE-NO
061013     MOVE '6'                    TO AT-TRAILER-TYPE
061013     MOVE 'E'                    TO AT-INFO-TRAILER-TYPE
061013
061013     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
061013                                    AT-GEN-INFO-LAST-MAINT-DT
061013
061013     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
061013                                    AT-GEN-INFO-LAST-UPDATED-BY
061013     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
061013
061013     perform varying e1 from +1 by +1 until
061013        ws-error-no (e1) = spaces
061013        move ws-error-no (e1)    to at-note-error-no (e1)
061013     end-perform
061013
061013     if at-info-line-1 not = spaces
061013        EXEC CICS WRITE
061013           DATASET  (ELTRLR-DSID)
061013           FROM     (ACTIVITY-TRAILERS)
061013           RIDFLD   (AT-CONTROL-PRIMARY)
061013        END-EXEC
061013     end-if
061013
061013     .
061013 3994C-EXIT.
061013     EXIT.

03280  3995-BUILD-CLAIM-RECORD.                                         

03282      IF PROCCDL GREATER ZERO                                      
03283          MOVE PROCCDI            TO CL-PROCESSOR-ID               
03284      ELSE                                                         
03285          MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID
041002     END-IF.
03286                                                                   
03287      IF FSTNMEL GREATER ZERO                                      
03288          MOVE FSTNMEI            TO CL-INSURED-1ST-NAME
041002     END-IF.
03289                                                                   
03290      IF INITL GREATER ZERO                                        
03291          MOVE INITI              TO CL-INSURED-MID-INIT
041002     END-IF.
03292                                                                   
03293      IF BIRTHDTL GREATER ZERO                                     
03294          MOVE WS-BIRTHDT         TO CL-INSURED-BIRTH-DT
041002     END-IF.
03295                                                                   
03296      IF SEXL GREATER ZERO                                         
03297          MOVE SEXI               TO CL-INSURED-SEX-CD
041002     END-IF.
03298                                                                   
03302      MOVE 'O'                    TO CL-CLAIM-STATUS.              
03303                                                                   
041002*    IF CLMTYPEL GREATER ZERO
041002     MOVE CLMTYPEI               TO CL-CLAIM-TYPE.
041002*    END-IF.
03306
03307      IF PREMTYPL GREATER ZERO                                     
03308          MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE
041002     END-IF.
03309                                                                   
03310      IF INCURL GREATER ZERO                                       
03311          MOVE WS-INCUR           TO CL-INCURRED-DT
041002     END-IF.
03312                                                                   
020816     IF (PI-COMPANY-ID = 'DCC' OR 'VPP')
061013        AND (ERPDEF-FOUND)
061013        MOVE WS-MAX-BENEFITS     TO CL-CRITICAL-PERIOD
061013*       MOVE WS-CRIT-PER-RECURRENT
061013*                                TO CL-CRIT-PER-RECURRENT
061013*       MOVE WS-CRIT-PER-RTW-MOS TO CL-CRIT-PER-RTW-MOS
              if ws-crit-per-recurrent > 01
                 move zeros            to cl-benefit-period
              end-if
061013     ELSE
100518        IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
061013           MOVE CM-LF-CRITICAL-PERIOD
061013                                 TO CL-CRITICAL-PERIOD
061013        ELSE
061013           MOVE CM-AH-CRITICAL-PERIOD
061013                                 TO CL-CRITICAL-PERIOD
061013        END-IF
061013     END-IF
061013
061013     IF CL-CRITICAL-PERIOD NOT = ZEROS
061013        COMPUTE DC-ELAPSED-MONTHS =
061013           CL-CRITICAL-PERIOD - WS-BENEFITS-PREV-PAID
061013        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
061013        MOVE ZEROS               TO DC-ELAPSED-DAYS
061013        MOVE '6'                 TO DC-OPTION-CODE
061013        PERFORM 9700-LINK-DATE-CONVERT
061013                                 THRU 9700-EXIT        
061013        MOVE DC-BIN-DATE-2       TO CL-BENEFIT-EXPIRATION-DT
061013     END-IF
061013
061013     if instypel > zero
061013        move instypei            to cl-insured-type
061013     end-if
03313      IF REPORTL GREATER ZERO                                      
03314          MOVE WS-REPORT          TO CL-REPORTED-DT
041002     END-IF.
03315                                                                   
03316      IF EIBAID = DFHPF6                                           
03317          IF WS-REPORT = LOW-VALUES  OR   REPORTL = ZEROS          
03318              MOVE WS-TODAY-DT    TO CL-REPORTED-DT                
03319                  IF EMI-FORCABLE-CTR = +1                         
03320                      MOVE ZEROS  TO CL-FORCEABLE-ERROR-CNT
041002                 END-IF
041002         END-IF
041002     END-IF.
03321                                                                   
040814*    IF ESTENDL GREATER ZERO                                      
040814*       MOVE WS-ESTEND           TO CL-EST-END-OF-DISAB-DT
040814*    END-IF.
03324                                                                   
03325      IF ADDONDTL GREATER ZERO                                     
03329          MOVE WS-ADD-ON-DT   TO CL-LAST-ADD-ON-DT
041002     END-IF.
03330                                                                   
040814*    IF CAUSEL GREATER ZERO                                       
040814*        MOVE CAUSEI             TO CL-CAUSE-CD
040814*    END-IF.
03333                                                                   
03334      IF SSNL GREATER ZERO                                         
03335          MOVE SSNI               TO CL-SOC-SEC-NO                 
03336      ELSE                                                         
03337          MOVE CERT-STATE         TO CL-SSN-STATE                  
03338          MOVE CERT-ACCOUNT-PRIME TO CL-SSN-ACCOUNT                
03339          MOVE CL-INSURED-LAST-NAME TO CL-SSN-LN3
041002     END-IF.
03340                                                                   
03341      IF PRICDL GREATER ZERO                                       
03342          MOVE PRICDI             TO CL-PRIORITY-CD
041002     END-IF.
03343                                                                   
03348      IF SUPVL GREATER ZERO                                        
03349          MOVE SUPVI              TO CL-SUPV-ATTN-CD
041002     END-IF.
03350                                                                   
03351      MOVE WS-TODAY-DT            TO CL-LAST-MAINT-DT.             
03352      MOVE CL-PROCESSOR-ID        TO CL-LAST-MAINT-USER.           
03353      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.         
03354                                                                   
03355      IF RELCLML GREATER ZERO                                      
03356          MOVE RELCLMI            TO CL-RELATED-CLAIM-NO
041002     END-IF.
03357                                                                   
03358      IF FILETOL GREATER ZERO                                      
03359          MOVE FILETOI            TO CL-FILE-LOCATION
041002     END-IF.
03360                                                                   
03361  3995-EXIT.                                                       
03362       EXIT.                                                       

061013 3996-read-cert-claim-trailer.
061013
061013     move cm-ah-benefit-amt      to ws-monthly-benefit
061013     if (pi-dcc-max-amt < ws-monthly-benefit)
              and (pi-dcc-max-amt not = zeros)
061013        move pi-dcc-max-amt      to ws-monthly-benefit
061013     end-if
061013
061013     MOVE CM-COMPANY-CD          TO CTRLR-COMP-CD
061013     MOVE CM-CARRIER             TO CTRLR-CARRIER
061013     MOVE CM-GROUPING            TO CTRLR-GROUPING
061013     MOVE CM-STATE               TO CTRLR-STATE 
061013     MOVE CM-ACCOUNT             TO CTRLR-ACCOUNT
061013     MOVE CM-CERT-EFF-DT         TO CTRLR-EFF-DT
061013     MOVE CM-CERT-NO             TO CTRLR-CERT-NO
061013     MOVE 'B'                    TO CTRLR-REC-TYPE
061013     EXEC CICS READ
061013        DATASET  (ELCRTT-DSID)
061013        set     (address of CERTIFICATE-TRAILERS)
061013        RIDFLD   (ELCRTT-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This may need some work here. I think I should accumulate ***
      ***  all the claims with the same claim type?????????          ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
061013     IF WS-RESP-NORMAL
              move zeros to ws-tot-days-paid ws-tot-amt-paid
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = spaces)
                 if cs-claim-type (s1) = clmtypei
                    move er-1659       to ws-error-no (e1)
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s1)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s1)
                 end-if
              end-perform
      *        compute ws-pd-bens rounded = ws-tot-days-paid / 30
      *        if (ws-pd-bens >= pi-dcc-max-benefits)
      *           and (pi-dcc-max-benefits not = zeros)
      *           add +1                to e1
      *           MOVE ER-1660          TO EMI-ERROR
      *                                    ws-error-no (e1)
      *           PERFORM 9900-ERROR-FORMAT
      *                                 THRU 9900-EXIT
      *        end-if
           end-if

061013**       if instypei = 'P'
061013**          move +1 to s1
061013**       else
061013**          MOVE +2 to s1
061013**       end-if
061013*        
061013*        move +1 to s1
061013*        evaluate clmtypei
061013*           when 'A'
061013*              move +1 to s2
061013*           when 'I'
061013*              move +2 to s2
061013*           when 'G'
061013*              move +3 to s2
061013*           when 'L'
061013*              move +4 to s2
061013*           when 'P'
061013*              move +5 to s2
061013*        end-evaluate
061013*        if cs-claim-type (s1 s2) = clmtypei
061013*           if cs-no-of-claims (s1 s2) > zeros
061013*              move er-1659       to ws-error-no (e1)
061013*           end-if
061013*           compute WS-BENEFITS-PREV-PAID =
061013*              cs-total-paid (s1 s2) / ws-monthly-benefit
061013*           if (WS-BENEFITS-PREV-PAID >= pi-dcc-max-benefits)
061013*              and (pi-dcc-max-benefits not = zeros)
061013*              add +1             to e1
061013*              MOVE ER-1660       TO EMI-ERROR
061013*                                    ws-error-no (e1)
061013*              PERFORM 9900-ERROR-FORMAT
061013*                                 THRU 9900-EXIT
061013*           end-if
061013*        end-if
061013*     end-if
061013
061013     .
061013 3996-exit.
061013     exit.
061013
061013 3997-GET-ERPDEF.
061013
           if cm-clp-state = spaces or low-values or zeros
              move cm-state            to cm-clp-state
           end-if
061013     MOVE SPACES                 TO WS-ERPDEF-SW
061013     MOVE ZEROS                  TO WS-MAX-BENEFITS
061013                                    WS-CRIT-PER-RTW-MOS
                                          WS-CRIT-PER-RECURRENT
061013
061013     MOVE PI-COMPANY-CD       TO ERPDEF-KEY
061013     MOVE CM-clp-state        TO ERPDEF-STATE
061013     MOVE ws-dcc-product-code TO ERPDEF-PROD-CD

070714***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
070714***                                                            ***
070714***  If they set up a claim where there is no coverage on the  ***
070714***  addendum with the intention of denying.  We need to do    ***
070714***  some funky stuff here.                                    ***
070714***                                                            ***
070714***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
070714
070714     evaluate true
070714        when (clmtypei = 'L' or 'P')
070714           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714              and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
070714        when (clmtypei not = 'L' and 'P')
070714           and (cm-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when (clmtypei not = 'L' and 'P')
070714           and (cm-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
070714        when (clmtypei = 'L' or 'P')
070714           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate

022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122
022122     move zeros                  to dc-elapsed-months
022122                                    dc-elapsed-days
022122     move low-values to dc-bin-date-1 dc-bin-date-2

061013*    MOVE 'A'                 TO ERPDEF-BEN-TYPE
061013*    MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
061013     MOVE CM-CERT-EFF-DT      TO ERPDEF-EXP-DT
061013     MOVE ERPDEF-KEY          TO ERPDEF-KEY-SAVE
061013
061013     EXEC CICS STARTBR
061013         DATASET  ('ERPDEF')
061013         RIDFLD   (ERPDEF-KEY)
061013         GTEQ
061013         RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF WS-RESP-NORMAL
061013        EXEC CICS READNEXT
061013           DATASET  ('ERPDEF')
061013           INTO     (PRODUCT-MASTER)
061013           RIDFLD   (ERPDEF-KEY)
061013           RESP     (WS-RESPONSE)
061013        END-EXEC
061013
061013        IF WS-RESP-NORMAL
061013           IF (ERPDEF-KEY-SAVE (1:16) =
061013              PD-CONTROL-PRIMARY (1:16))
061013              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013
061013              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
022122                 (A1 > +11)
061013                 OR ((PD-PROD-CODE (A1) = CLMTYPEI)
PEMTST                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013              END-PERFORM
022122              IF A1 < +12
                       SET ERPDEF-FOUND TO TRUE
061013                 move pd-max-amt (a1)
061013                              to pi-dcc-max-amt
061013                 MOVE PD-CRIT-PERIOD (A1)
061013                              TO WS-MAX-BENEFITS
061013                                 pi-dcc-max-benefits
                       if pd-rec-crit-period (a1) not numeric
                          move 01      to pd-rec-crit-period (a1)
                       end-if
061013                 MOVE PD-REC-CRIT-PERIOD (A1)
061013                              TO WS-CRIT-PER-RECURRENT
061013                 IF PD-RTW-MOS (A1) NUMERIC
061013                    MOVE PD-RTW-MOS (A1)
061013                              TO WS-CRIT-PER-RTW-MOS
061013                 ELSE
061013                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
061013                 END-IF
061013                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
061013                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
061013                                 TO WS-EXCL-PERIOD
061013                 END-IF
061013                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
061013                    MOVE PD-COVERAGE-ENDS-MOS (A1)
061013                                 TO WS-COV-ENDS
061013                 END-IF
061013                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
061013                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
061013                                 TO WS-ACC-PERIOD
061013                 END-IF
011118                 IF PD-PRE-EXIST-EXCL-TYPE (A1) NUMERIC
011118                    MOVE PD-PRE-EXIST-EXCL-TYPE (A1)
011118                                 TO WS-PRE-EXISTING-PER
011118                 END-IF
                    else
070714                 MOVE -1         TO MAINTL
                       move er-1673    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    MOVE -1            TO MAINTL
                    move er-1671       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        END-IF
061013     END-IF
061013
061013     .
061013 3997-EXIT.
061013     EXIT.
061013
061013 3998-update-cert-claim-trailer.
061013
061013     EXEC CICS GETMAIN
061013        SET      (ADDRESS OF CERTIFICATE-TRAILERS)
061013        LENGTH   (ELCRTT-LENGTH)
061013        INITIMG  (GETMAIN-SPACE)
061013     END-EXEC
061013
061013     MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
061013     MOVE CM-CARRIER       TO CTRLR-CARRIER
061013     MOVE CM-GROUPING      TO CTRLR-GROUPING
061013     MOVE CM-STATE         TO CTRLR-STATE 
061013     MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
061013     MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
061013     MOVE CM-CERT-NO       TO CTRLR-CERT-NO
061013     MOVE 'B'              TO CTRLR-REC-TYPE
061013     EXEC CICS READ
061013        UPDATE
061013        DATASET  (ELCRTT-DSID)
061013        into     (CERTIFICATE-TRAILERS)
061013        RIDFLD   (ELCRTT-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = spaces)
              end-perform
              if s1 < +25
061013           perform 3999-upd-crt-trlr thru 3999-exit
061013           EXEC CICS REWRITE
061013              DATASET  (ELCRTT-DSID)
061013              from     (CERTIFICATE-TRAILERS)
061013              RESP     (WS-RESPONSE)
061013           END-EXEC
              end-if
061013     else
061013        if ws-resp-notfnd
061013           move 'CS'             to certificate-trailers
061013           move cm-control-primary to cs-control-primary
061013           move 'B'              to cs-trailer-type
061013           perform varying s1 from +1 by +1 until s1 > +24
061013              move zeros         to cs-days-paid (s1)
061013                                    cs-total-paid (s1)
                                          cs-benefit-period (s1)
                                          cs-remaining-bens (s1)
061013           end-perform
                 move +1               to s1
061013           perform 3999-upd-crt-trlr
061013                                 thru 3999-exit
061013           EXEC CICS WRITE
061013              DATASET  (ELCRTT-DSID)
061013              from     (CERTIFICATE-TRAILERS)
061013              RIDFLD   (cs-control-primary)
061013              RESP     (WS-RESPONSE)
061013           END-EXEC
061013        end-if
061013     END-IF
061013
061013     .
061013 3998-exit.
061013     exit.
061013
061013 3999-upd-crt-trlr.

           move clmnoi                 to cs-claim-no       (s1)
061013     move clmtypei               to cs-claim-type     (s1)
           move instypei               to cs-insured-type   (s1)

121417     if ws-crit-per-recurrent > 01
121417        move zeros               to cs-benefit-period(s1)
121417     else
121417        move 01                  to cs-benefit-period(s1)
121417     end-if

121417*    move cl-benefit-period      to cs-benefit-period (s1)
pemtst*    move cl-critical-period     to cs-remaining-bens (s1)
pemtst     move zeros                  to cs-remaining-bens (s1)

061013     .
061013 3999-exit.
061013     exit.

03365  4000-CALCULATE-CERT-TERM.                                        
03366                                                                   
03367      PERFORM 4100-CALC-GROSS-TERM THRU 4200-EXIT.                 
03368                                                                   
03369      MOVE N                      TO WS-TERM.                      
03370                                                                   
03371      IF WS-TERM GREATER 120                                       
03372          MOVE 120                TO WS-TERM
041002     END-IF.
03373                                                                   
03374  4000-CALC-TERM-EXIT.                                             
03375       EXIT.                                                       
03376                                                                   
03377  4100-CALC-GROSS-TERM.                                            
03378      MOVE HOLD-LOAN-BAL           TO L.                           
03379      MOVE WS-ACVBENE              TO M.                           
03380      COMPUTE N = L / M.                                           
03381      IF N LESS 1                                                  
03382          MOVE 1  TO N
041002     END-IF.
03383                                                                   
03384      IF LCVCDL GREATER ZERO                                       
03385          COMPUTE WS-LF-RATE = WS-LCVRATE / +1000                  
03386      ELSE                                                         
03387          MOVE ZEROS               TO WS-LF-RATE
041002     END-IF.
03388                                                                   
03389      IF ACVCDL GREATER ZERO                                       
03390          COMPUTE WS-AH-RATE = WS-ACVRATE / +1000                  
03391      ELSE                                                         
03392          MOVE ZEROS               TO WS-AH-RATE
041002     END-IF.
03393                                                                   
03394      COMPUTE I = WS-APR / +1200.                                  
03395                                                                   
03396  4105-LOOP.                                                       
03397      IF N GREATER 240                                             
03398          GO TO 4200-EXIT
041002     END-IF.
03399                                                                   
03400      PERFORM 4210-CALC-LEFT-RIGHTONE THRU 4220-EXIT.              
03401                                                                   
03402      IF LEFT-TOT-1 GREATER RIGHT-TOT-1                            
03403          ADD 1 TO N                                               
03404          GO TO 4105-LOOP
041002     END-IF.
03405                                                                   
03406  4200-EXIT.                                                       
03407       EXIT.                                                       
03408                                                                   
03409  4210-CALC-LEFT-RIGHTONE.                                         
03410       MOVE L         TO LEFT-TOT-1.                               
03411       SUBTRACT 1 FROM N.                                          
03412       PERFORM 4300-CALC-A-N THRU 4300-EXIT.                       
03413       PERFORM 4350-CALC-IA-N THRU 4400-EXIT.                      
03414       ADD 1 TO N.                                                 
03415                                                                   
03416  4211-CALC-TERM1.                                                 
03417       MOVE N         TO TERM1.                                    
03418       MULTIPLY M BY TERM1.                                        
03419                                                                   
03420  4212-LOOP.                                                       
03421       COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.          
03422       ADD 1 TO A-N.                                               
03423       MULTIPLY A-N BY TERM1.                                      
03424       SUBTRACT 1 FROM A-N.                                        
03425       ADD TERM1 TO LEFT-TOT-1.                                    
03426                                                                   
03427  4213-CALC-TERM2.                                                 
03428       MOVE M         TO TERM2.                                    
03429       COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.          
03430       MULTIPLY IA-N BY TERM2.                                     
03431       SUBTRACT TERM2 FROM LEFT-TOT-1.                             
03432                                                                   
03433  4215-CALC-RIGHTONE.                                              
03434       MOVE M         TO RIGHT-TOT-1.                              
03435       PERFORM 4300-CALC-A-N THRU 4300-EXIT.                       
03436       MULTIPLY A-N BY RIGHT-TOT-1.                                
03437                                                                   
03438  4220-EXIT.                                                       
03439       EXIT.                                                       
03440                                                                   
03441  4300-CALC-A-N.                                                   
03442      IF N LESS 1                                                  
03443          MOVE 0     TO A-N                                        
03444          GO TO 4300-EXIT
041002     END-IF.
03445                                                                   
03446      IF I = 0                                                     
03447          MOVE .00001 TO I
041002     END-IF.
03448                                                                   
03449      ADD 1 TO I.                                                  
03450      DIVIDE I INTO 1 GIVING V.                                    
03451      SUBTRACT 1 FROM I.                                           
03452      PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.                     
03453      SUBTRACT V-EX-N FROM 1 GIVING TERM3.                         
03454      DIVIDE I INTO TERM3 GIVING A-N.                              
03455                                                                   
03456  4300-EXIT.                                                       
03457       EXIT.                                                       
03458                                                                   
03459  4350-CALC-IA-N.                                                  
03460      IF N LESS 1                                                  
03461          MOVE 0      TO IA-N                                      
03462          GO TO 4400-EXIT
041002     END-IF.
03463                                                                   
03464      ADD 1 TO N.                                                  
03465      PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.                     
03466      SUBTRACT 1 FROM N.                                           
03467      MULTIPLY N BY V-EX-N GIVING TERM3.                           
03468      SUBTRACT TERM3 FROM A-N GIVING TERM3.                        
03469      SUBTRACT V FROM 1 GIVING TERM4.                              
03470      DIVIDE TERM4 INTO TERM3 GIVING IA-N.                         
03471                                                                   
03472  4400-EXIT.                                                       
03473       EXIT.                                                       
03474                                                                   
03475  4450-CALC-V-EX-N.                                                
03476      IF N LESS 1                                                  
03477          MOVE 1    TO V-EX-N                                      
03478          GO TO 4490-EXIT
041002     END-IF.
03479                                                                   
03480      MOVE N        TO NV-STORE.                                   
03481                                                                   
03482      IF V-EX-ONETIME = 1  OR                                      
03483         V NOT = V-EXPONENT (1)                                    
03484           PERFORM 4470-BUILD-V-EX-TABLE THRU 4480-EXIT
041002     END-IF.
03485                                                                   
03486  4460-LOOP.                                                       
03487      IF N GREATER 248                                             
03488          MOVE 248 TO N
041002     END-IF.
03489                                                                   
03490      IF N = 1                                                     
03491          MOVE V               TO V-EX-N                           
03492      ELSE
03493          MOVE V-EXPONENT (N)  TO V-EX-N
041002     END-IF.
03494                                                                   
03495      GO TO 4490-EXIT.                                             
03496                                                                   
03497  4470-BUILD-V-EX-TABLE.                                           
03498      MOVE 2      TO N.                                            
03499      MOVE V      TO V-EXPONENT (1)                                
03500                     V-EX-N.                                       
03501                                                                   
03502  4471-LOOP.                                                       
03503      MULTIPLY V BY V-EX-N.                                        
03504      MOVE V-EX-N   TO V-EXPONENT (N).                             
03505                                                                   
03506      ADD 1 TO N.                                                  
03507      IF N LESS 248                                                
03508          GO TO 4471-LOOP
041002     END-IF.
03509                                                                   
03510      MOVE NV-STORE     TO N.                                      
03511      MOVE ZERO         TO V-EX-ONETIME.                           
03512                                                                   
03513  4480-EXIT.                                                       
03514       EXIT.                                                       
03515                                                                   
03516  4490-EXIT.                                                       
03517       EXIT.                                                       
03518                                                                   
03519  4500-CALC-NET-TERM.                                              
03520      MOVE HOLD-LOAN-BAL           TO L.                           
03521      MOVE WS-ACVBENE              TO M.                           
03522      DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.                  
03523                                                                   
03524      IF WS-REMAIN GREATER ZERO                                    
03525          ADD 1 TO N
041002     END-IF.
03526                                                                   
03527      IF N = 0                                                     
03528          MOVE 1 TO N
041002     END-IF.
03529                                                                   
03530  4700-EXIT.                                                       
03531       EXIT.                                                       
03532                                                                   
03533  4999-CALC-TERM-EXIT.                                             
03534      EXIT.                                                        
03535                                                                   
03536      EJECT                                                        
03537  5000-INCURRED-CHANGE.                                            
03538      IF NOT MODIFY-CAP                                            
03539          MOVE ER-0070            TO EMI-ERROR                     
03540          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03541          MOVE -1                 TO MAINTL                        
03542          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03543                                                                   
03544      IF CLMNOI       NOT = PI-LAST-CLAIM       OR                 
03545         CLMCARRI     NOT = PI-LAST-CARR        OR                 
03546         CERTNOI      NOT = PI-LAST-CERT-PRIME  OR                 
03547         SUFXI        NOT = PI-LAST-CERT-SUFX   OR                 
03548         PI-INCURR-SW NOT = 'Y'                                    
03549           GO TO 1000-SHOW-CLAIM
041002     END-IF.
03550                                                                   
03551      PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.                 
03552      PERFORM 6200-EDIT-CERT-DATA  THRU 6200-EXIT.                 
03553      PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.        
03554                                                                   
03555      IF EMI-NO-ERRORS                                             
03556          GO TO 5000-TRY-TO-BUILD
041002     END-IF.
03557                                                                   
03558      IF EIBAID = DFHPF6  AND NOT FORCE-CAP                        
03559         MOVE ER-0433             TO EMI-ERROR                     
03560         MOVE -1                  TO MAINTL                        
03561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03562         GO TO 8200-SEND-DATAONLY
041002     END-IF.
03563                                                                   
03564      IF EIBAID = DFHPF6                                           
03565          IF EMI-FATAL-CTR NOT EQUAL ZERO                          
03566             MOVE ER-0434         TO EMI-ERROR                     
03567             MOVE -1              TO MAINTL                        
03568             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
03569             GO TO 8200-SEND-DATAONLY                              
03570          ELSE                                                     
03571             MOVE +1              TO INCURL                        
03572             GO TO 5000-TRY-TO-BUILD
041002         END-IF
041002     END-IF.
03573                                                                   
03574      GO TO 8200-SEND-DATAONLY.                                    
03575                                                                   
03576  5000-TRY-TO-BUILD.                                               
03577                                                                   
03578      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 
03579                                                                   
03580      MOVE CLMCARRI               TO MSTR-CARRIER.                 
03581                                                                   
03582      MOVE CLMNOI                 TO MSTR-CLAIM-NO.                
03583      MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.           
03584      MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.            
03585                                                                   
03586      PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.               
03587                                                                   
03588      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC                       
03589          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
041002     END-IF.
03590                                                                   
03591      IF CL-PURGED-DT NOT = LOW-VALUES                             
03592          
               EXEC CICS UNLOCK
03593              DATASET   (ELMSTR-DSID)
03594          END-EXEC
03595          MOVE ER-7691            TO EMI-ERROR                     
03596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03597          MOVE -1                 TO MAINTL                        
03598          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03599                                                                   
03600      IF CL-AUTO-PAY-SEQ NOT = ZERO                                
03601         
              EXEC CICS UNLOCK
03602              DATASET   (ELMSTR-DSID)
03603         END-EXEC
03604          MOVE ER-0523             TO EMI-ERROR
03605          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03606          MOVE -1                  TO MAINTL
03607          GO TO 8200-SEND-DATAONLY
041002     END-IF.
03608                                                                   
03609      PERFORM 7900-READ-ACTV-UPDATE THRU 7900-EXIT.                
03610                                                                   
03611      MOVE AT-ITD-PAID-EXPENSES       TO WS-ITD-PAID-EXPENSE.      
03612      MOVE AT-ITD-CHARGEABLE-EXPENSE  TO WS-ITD-CHARGABLE-EXPENSE. 
03613      MOVE AT-INITIAL-MANUAL-RESERVE  TO WS-INITIAL-MANUAL-RESERVE.
03614      MOVE AT-CURRENT-MANUAL-RESERVE  TO WS-CURRENT-MANUAL-RESERVE.
03615      MOVE AT-ITD-ADDITIONAL-RESERVE  TO WS-ITD-ADDITIONAL-RESERVE.
03616                                                                   
03617      IF MANRSVL GREATER ZERO                                      
03618         MOVE WS-MANRSV           TO AT-INITIAL-MANUAL-RESERVE     
03619                                     AT-CURRENT-MANUAL-RESERVE
041002     END-IF.

03620      MOVE 1                      TO SUB.                          
03621                                                                   
03622  5010-LOOP.                                                       
03623      IF SUB = 1                                                   
03624         MOVE WS-TODAY-DT         TO AT-OPEN-CLOSE-DATE   (SUB)    
03625         MOVE 'O'                 TO AT-OPEN-CLOSE-TYPE   (SUB)    
03626         MOVE 'NEW'               TO AT-OPEN-CLOSE-REASON (SUB)    
03627      ELSE                                                         
03628         MOVE LOW-VALUES          TO AT-OPEN-CLOSE-DATE   (SUB)    
03629         MOVE SPACES              TO AT-OPEN-CLOSE-TYPE   (SUB)    
03630                                     AT-OPEN-CLOSE-REASON (SUB)
041002     END-IF.
03631                                                                   
03632      IF SUB NOT = 6                                               
03633         ADD 1                    TO SUB                           
03634         GO TO 5010-LOOP
041002     END-IF.
03635                                                                   
03636  5020-UPDATE.                                                     
03637                                                                   
03638      PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.                 
03639                                                                   
03640      IF DIAGL = ZERO
040814       AND ICD1L = ZERO
040814       AND ICD2L = ZERO                                              
03641          PERFORM 7910-READ-NINETY THRU 7910-EXIT                  
03642          MOVE AT-INFO-LINE-1     TO WS-DIAGNOSIS-DESCRIPT         
040814         MOVE AT-ICD-CODE-1      TO WS-ICD-CODE-1
040814         MOVE AT-ICD-CODE-2      TO WS-ICD-CODE-2
03643          GO TO 5025-BYPASS-UPDATE
041002     END-IF.
03644                                                                   
03645      PERFORM 7900-READ-NINETY-UPDATE THRU 7900-EXIT.              
03646                                                                   
03647      MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.        
040814     IF DIAGL NOT = ZERO
040814         MOVE DIAGI              TO AT-INFO-LINE-1
040814     END-IF.
040814
040814     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
040814     IF ICD1L NOT = ZERO
040814         MOVE ICD1I              TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
040814     IF ICD2L NOT = ZERO
040814         MOVE ICD2I              TO AT-ICD-CODE-2
040814     END-IF.
03649                                                                   
03650      PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.                 
03651                                                                   
03652  5025-BYPASS-UPDATE.                                              
03653                                                                   
03654      
           EXEC CICS GETMAIN
03655          SET      (ADDRESS OF ACTIVITY-TRAILERS)
03656          LENGTH   (ELTRLR-LENGTH)
03657      END-EXEC.
03658                                                                   
03659      MOVE SPACES                 TO ACTIVITY-TRAILERS.            
03660      MOVE 'AT'                   TO AT-RECORD-ID.                 
03661                                                                   
03662      MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.                
03663      MOVE MSTR-CARRIER           TO AT-CARRIER.                   
03664      MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.                  
03665      MOVE MSTR-CERT-NO           TO AT-CERT-NO.                   
03666      MOVE CL-TRAILER-SEQ-CNT     TO AT-TRAILER-CNT-AT-CHG.        
03667      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                          
03668      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.               
03669      MOVE '9'                    TO AT-TRAILER-TYPE.              
03670      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
03671      MOVE '5'                    TO DC-OPTION-CODE.               
03672      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
03673      MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT.               
03674                                                                   
03675      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY.               
03676      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         
03677                                                                   
03678      MOVE CL-INCURRED-DT         TO AT-OLD-INCURRED-DT
090821                                    ws-prev-inc-dt
03679      MOVE CL-REPORTED-DT         TO AT-OLD-REPORTED-DT.           
03680      MOVE CL-FILE-ESTABLISH-DT   TO AT-OLD-ESTABLISHED-DT.        
03681      MOVE CL-TOTAL-PAID-AMT      TO AT-OLD-TOTAL-PAID.            
03682      MOVE CL-NO-OF-DAYS-PAID     TO AT-OLD-DAYS-PAID.             
03683      MOVE CL-NO-OF-PMTS-MADE     TO AT-OLD-NO-OF-PMTS.            
03684      MOVE CL-PAID-THRU-DT        TO AT-OLD-PAID-THRU-DT.          
03685      MOVE CL-LAST-PMT-DT         TO AT-LAST-PMT-MADE-DT.          
03686      MOVE WS-DIAGNOSIS-DESCRIPT  TO AT-OLD-DIAG-DESCRIP.          
03687      MOVE CL-CAUSE-CD            TO AT-OLD-DIAG-CODE.             
040814     MOVE WS-ICD-CODE-1          TO AT-OLD-ICD-CODE-1.
040814     MOVE WS-ICD-CODE-2          TO AT-OLD-ICD-CODE-2.
03688                                                                   
03689      MOVE WS-ITD-PAID-EXPENSE    TO AT-OLD-ITD-PAID-EXPENSE.      
03690      MOVE WS-ITD-CHARGABLE-EXPENSE   TO AT-OLD-CHARGABLE-EXPENSE. 
03691      MOVE WS-INITIAL-MANUAL-RESERVE  TO AT-OLD-INIT-MAN-RESV.     
03692      MOVE WS-CURRENT-MANUAL-RESERVE  TO AT-OLD-CURRENT-MAN-RESV.  
03693      MOVE WS-ITD-ADDITIONAL-RESERVE  TO AT-OLD-ADDL-MAN-RESV.     
03694                                                                   
03695      
           EXEC CICS WRITE
03696          DATASET   (ELTRLR-DSID)
03697          FROM      (ACTIVITY-TRAILERS)
03698          RIDFLD    (AT-CONTROL-PRIMARY)
03699      END-EXEC.
03700                                                                   
03701      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 
03702      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 
03703      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.                
03704      MOVE CL-CERT-STATE          TO CERT-STATE.                   
03705      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 
03706      MOVE CL-CERT-NO             TO CERT-CERT-NO.                 
03707      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.                  
03708                                                                   
03709      
           EXEC CICS HANDLE CONDITION
03710          NOTFND   (5050-CERT-NOT-FOUND)
03711      END-EXEC.
03712                                                                   
03713      PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.                
03714                                                                   
03715      PERFORM 3700-BUILD-CERT THRU 3700-EXIT.                      
03716                                                                   
03717      
           EXEC CICS HANDLE CONDITION
03718           DUPKEY   (5030-CERT-WRITTEN)
03719      END-EXEC.
03720                                                                   
03721      
           EXEC CICS REWRITE
03722          DATASET  (ELCERT-DSID)
03723          FROM     (CERTIFICATE-MASTER)
03724      END-EXEC.
03725                                                                   
03726  5030-CERT-WRITTEN.                                               
03727                                                                   
03728      MOVE LOW-VALUES             TO CL-LAST-PMT-DT                
03729                                     CL-INCURRED-DT                
03730                                     CL-REPORTED-DT                
03731                                     CL-EST-END-OF-DISAB-DT        
03732                                     CL-PAID-THRU-DT.              
03733                                                                   
03734      MOVE ZEROS                  TO CL-TOTAL-PAID-AMT             
03735                                     CL-NO-OF-PMTS-MADE            
03736                                     CL-NO-OF-DAYS-PAID.           
03737                                                                   
03738      IF CLAIM-IS-CLOSED                                           
03739         MOVE WS-TODAY-DT         TO CL-LAST-REOPEN-DT             
03740         MOVE 'O'                 TO CL-CLAIM-STATUS
041002     END-IF.
03741                                                                   
03742      PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.              
03743      MOVE '5'                    TO CL-LAST-MAINT-TYPE.
03744      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.       
03745                                                                   
03746      
           EXEC CICS HANDLE CONDITION
03747           DUPKEY   (5035-CLAIM-WRITTEN)
03748      END-EXEC.
03749                                                                   
03750      
           EXEC CICS REWRITE
03751          DATASET   (ELMSTR-DSID)
03752          FROM      (CLAIM-MASTER)
03753      END-EXEC.
03754                                                                   
03755  5035-CLAIM-WRITTEN.                                              

090821     if clmtypei = 'L' or 'O'
090821        move cm-lf-benefit-cd    to ws-ben-hold
090821        move '4'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-lf-special-calc-cd
090821        end-if
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-hold
090821        move '5'                 to ws-rec-type
090821        perform 7100-read-benefit thru 7199-exit
090821        if ws-ben-alpha-hold <> spaces
090821           move ws-special-calc-cd
090821                                 to ws-ah-special-calc-cd
090821        end-if
090821     end-if
090821 
090821     move ' '                    to ws-mob-cert-ind
090821     if pi-company-id = 'CID'
090821        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
090821           and (ws-lf-special-calc-cd = 'O'))
090821                      or
090821           ((clmtypei <> pi-life-override-l1 and 'O')
090821           and (ws-ah-special-calc-cd = 'O'))
090821           set mob-cert to true
090821        end-if
090821     else
090821        if (pi-company-id = 'DCC')
090821           and (cm-carrier = '7')
090821           set mob-cert to true
090821        end-if
090821     end-if
090821 
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821 
      ** if the prev inc dt >= hi dt then probably
      ** have error already set
           if ws-prev-inc-dt >= ws-hi-acct-dt
              continue
           else
090821        if (ws-incur >= ws-hi-acct-dt)
090821           and (acct-cancelled)
090821           and (mob-cert)
090821           MOVE er-1682          TO EMI-ERROR
090821           MOVE -1               TO INCURL
090821           MOVE AL-UABON         TO INCURA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821        end-if
           end-if
090821
           if ws-prev-inc-dt < cm-cert-eff-dt
              continue
           else
090821        if ws-incur < cm-cert-eff-dt
090821           MOVE er-1683          TO EMI-ERROR
090821           MOVE -1               TO INCURL
090821           MOVE AL-UABON         TO INCURA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 3992-BUILD-TRAILER
090821                                 THRU 3992-EXIT
090821        end-if
           end-if

           .
       5035-continue.
03756                                                                   
03757      MOVE ER-0000                TO EMI-ERROR.                    
03758      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03759                                                                   
03760      MOVE CLMNOI                 TO PI-LAST-CLAIM.                
03761      MOVE CERTNOI                TO PI-LAST-CERT-PRIME.           
03762      MOVE SUFXI                  TO PI-LAST-CERT-SUFX.            
03763      MOVE CLMCARRI               TO PI-LAST-CARR.                 
03764                                                                   
03765      IF PRTOPTL GREATER ZERO                                      
03766          MOVE PRTOPTI            TO PI-PRT-OPT
041002     END-IF.
03767                                                                   
03768      IF ALTPRTL GREATER ZERO                                      
03769          MOVE ALTPRTI            TO PI-ALT-PRT
041002     END-IF.
03770                                                                   
03771      MOVE LOW-VALUES             TO EL130AI.                      
03772                                                                   
03773      MOVE PI-LAST-CERT-PRIME     TO CERTNOO.                      
03774      MOVE PI-LAST-CERT-SUFX      TO SUFXO.                        
03775      MOVE PI-LAST-CARR           TO CLMCARRO.                     
03776      MOVE PI-LAST-CLAIM          TO CLMNOO.                       
03777                                                                   
03778      IF PI-PRT-OPT = 'N' OR 'L'                                   
03779          MOVE PI-PRT-OPT         TO PRTOPTO                       
03780          MOVE 1                  TO PRTOPTL
041002     END-IF.
03781                                                                   
03782      IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES                    
03783          MOVE PI-ALT-PRT         TO ALTPRTO                       
03784          MOVE 4                  TO ALTPRTL
041002     END-IF.
03785                                                                   
03786      MOVE AL-UANON               TO CLMNOA CLMCARRA.
PEMMOD     MOVE AL-SANON               TO CERTNOA SUFXA.                
03788                                                                   
03789      MOVE SPACES                 TO PI-INCURR-SW.                 
03790                                                                   
03791      IF PRTOPTL GREATER ZERO                                      
03792          IF PRTOPTI = 'L'                                         
03793              GO TO 0400-CREATE-ELACTQ                             
03794          ELSE                                                     
03795              GO TO 0480-PRINT-NOW
041002         END-IF
041002     END-IF.
03796                                                                   
03797      GO TO 8100-SEND-INITIAL-MAP.                                 
03798                                                                   
03799  5050-CERT-NOT-FOUND.                                             
03800                                                                   
03801      MOVE ER-0244                TO EMI-ERROR.                    
03802      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03803      MOVE -1                     TO CERTNOL.                      
03804      GO TO 8200-SEND-DATAONLY.                                    
03805                                                                   
03806      EJECT                                                        
03807  5500-BUILD-FORM-TRAILER.                                         
03808 *************************************************************     
03809 *           THIS CODE IS FOR 'CRI' ONLY         04/18/88          
03810 *************************************************************     
03811                                                                   
03812      MOVE WS-TODAY-DT            TO SAVE-SEND-DT.                 
03813                                                                   
03814      IF  WS-BEN-ALPHA-2 = '30'                                    
03815          MOVE WS-TODAY-DT        TO DC-BIN-DATE-1                 
03816          MOVE '6'                TO DC-OPTION-CODE                
03817          IF CL-CARRIER = 'C'                                      
03818              MOVE +14            TO DC-ELAPSED-DAYS               
03819              MOVE +0             TO DC-ELAPSED-MONTHS             
03820              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
03821              MOVE DC-BIN-DATE-2  TO SAVE-SEND-DT                  
03822          ELSE                                                     
03823              MOVE +21            TO DC-ELAPSED-DAYS               
03824              MOVE +0             TO DC-ELAPSED-MONTHS             
03825              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
03826              MOVE DC-BIN-DATE-2      TO  SAVE-SEND-DT
041002         END-IF
041002     END-IF.
03827
03828      
           EXEC CICS GETMAIN
03829           SET      (ADDRESS OF ACTIVITY-TRAILERS)
03830           LENGTH   (ELTRLR-LENGTH)
03831           INITIMG  (GETMAIN-SPACE)
03832      END-EXEC.
03833                                                                   
03834      MOVE SAVE-AT-PRIMARY-KEY    TO AT-CONTROL-PRIMARY.           
03835      MOVE 'AT'                   TO AT-RECORD-ID.                 
03836                                                                   
03837      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.                         
03838      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.               
03839      MOVE 'A'                    TO AT-TRAILER-TYPE.              
03840      MOVE WS-TODAY-DT            TO AT-RECORDED-DT                
03841                                     AT-FORM-LAST-MAINT-DT.        
03842      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
03843                                     AT-FORM-LAST-UPDATED-BY.
03844      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
03845      MOVE SAVE-SEND-DT           TO AT-FORM-SEND-ON-DT.
03846      MOVE LOW-VALUES             TO AT-FORM-FOLLOW-UP-DT
03847                                     AT-FORM-RE-SEND-DT            
03848                                     AT-FORM-ANSWERED-DT           
03849                                     AT-EMP-FORM-ANSWERED-DT       
03850                                     AT-PHY-FORM-ANSWERED-DT       
03851                                     AT-EMP-FORM-SEND-ON-DT        
03852                                     AT-FORM-PRINTED-DT            
03853                                     AT-FORM-REPRINT-DT.           
03854      MOVE '1'                    TO AT-FORM-TYPE.                 
03855                                                                   
03856      MOVE SPACES                 TO AT-INSTRUCT-LN-1              
03857                                     AT-INSTRUCT-LN-2              
03858                                     AT-INSTRUCT-LN-3.             
03859                                                                   
03860      MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.          
03861      MOVE 'I'                    TO AT-FORM-ADDRESS.              
03862                                                                   
03863      MOVE SAVE-SEND-DT           TO AT-PHY-FORM-SEND-ON-DT.
03864                                                                   
03865      IF AM-EMPLOYER-STMT-USED = 'Y'                               
03866         MOVE SAVE-SEND-DT       TO AT-EMP-FORM-SEND-ON-DT
041002     END-IF.
03867                                                                   
03868      
           EXEC CICS WRITE
03869           DATASET  (ELTRLR-DSID)
03870           FROM     (ACTIVITY-TRAILERS)
03871           RIDFLD   (AT-CONTROL-PRIMARY)
03872      END-EXEC.
03873                                                                   
03874  5599-EXIT.                                                       
03875      EJECT                                                        
03876 *************************************************************     
03877 *           THIS CODE IS FOR 'CRI' ONLY         04/18/88          
03878 *************************************************************     
03879                                                                   
03880  5700-BUILD-ARCHIVE-HEADER.                                       
03881                                                                   
03882      MOVE '1'                    TO CNTL-REC-TYPE.                
03883      MOVE ZEROS                  TO CNTL-SEQ-NO.                  
03884      MOVE SPACES                 TO CNTL-ACCESS.                  
03885                                                                   
03886      
           EXEC CICS READ
03887           DATASET  (ELCNTL-DSID)
03888           SET      (ADDRESS OF CONTROL-FILE)
03889           RIDFLD   (ELCNTL-KEY)
03890           UPDATE
03891      END-EXEC.
03892                                                                   
03893      ADD 1 TO CF-CO-ARCHIVE-COUNTER.                              
03894                                                                   
03895      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.                  
03896                                                                   
03897      
           EXEC CICS REWRITE
03898           FROM     (CONTROL-FILE)
03899           DATASET  (ELCNTL-DSID)
03900      END-EXEC.
03901                                                                   
03902      
           EXEC CICS HANDLE CONDITION
03903           NOTOPEN  (9990-ABEND)
03904           DUPKEY   (5799-EXIT)
03905      END-EXEC.
03906                                                                   
03907      
           EXEC CICS GETMAIN
03908           SET     (ADDRESS OF LETTER-ARCHIVE)
03909           LENGTH  (ELARCH-LENGTH)
03910      END-EXEC.
03911                                                                   
03912      MOVE SPACES                 TO LETTER-ARCHIVE.               
03913      MOVE 'LA'                   TO LA-RECORD-ID.                 
03914      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                 
03915                                     LA-ARCHIVE-NO-A1.             
03916      MOVE '4'                    TO LA-RECORD-TYPE                
03917                                     LA-RECORD-TYPE-A1.            
03918      MOVE ZEROS                  TO LA-LINE-SEQ-NO                
03919                                     LA-LINE-SEQ-NO-A1.            
03920      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD                 
03921                                     LA-COMPANY-CD-A1.             
03922      MOVE CL-CARRIER             TO LA4-CARRIER.                  
03923      MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.                 
03924      MOVE CL-CERT-NO             TO LA4-CERT-NO.                  
03925      MOVE CL-CERT-STATE          TO LA4-STATE.                    
03926      MOVE  ZEROS                 TO LA4-NO-OF-COPIES.             
03927      MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD.             
03928      MOVE WS-TODAY-DT            TO LA4-CREATION-DT.              
03929      MOVE LOW-VALUES             TO LA4-INITIAL-PRINT-DATE        
03930                                     LA4-RESEND-PRINT-DATE         
03931                                     LA4-FORM-REM-PRINT-DT         
03932                                     LA4-RESEND-DATE.              
03933      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.            
03934      MOVE '1'                    TO LA4-FORM-TYPE.                
03935                                                                   
03936      
           EXEC CICS WRITE
03937           DATASET  (ELARCH-DSID)
03938           FROM     (LETTER-ARCHIVE)
03939           RIDFLD   (LA-CONTROL-PRIMARY)
03940      END-EXEC.
03941                                                                   
03942  5799-EXIT.                                                       
03943      EJECT                                                        
03944  6000-EDIT-CLAIM-DATA.                                            
03945                                                                   
03946      MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.          
03947      MOVE '2'                    TO DC-OPTION-CODE.               
03948      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
03949      MOVE DC-BIN-DATE-1          TO WS-TODAY-DT.                  
03950                                                                   
01119      IF MAINTL NOT = ZERO                                         
01120          MOVE AL-UANON           TO MAINTA
041002     END-IF
01121                                                                   
03951      IF CLMNOL GREATER ZERO                                       
03952          IF CLMNOI = SPACES OR LOW-VALUES                         
03953              MOVE -1             TO CLMNOL                        
03954              MOVE AL-UABON       TO CLMNOA                        
03955              MOVE ER-7688        TO EMI-ERROR                     
03956              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
03957          ELSE                                                     
03958              MOVE AL-UANON       TO CLMNOA
041002         END-IF
041002     END-IF.
03959                                                                   
PEMMOD     IF CERTNOL = ZERO                                            
PEMMOD         MOVE -1                 TO CERTNOL                       
PEMMOD         MOVE AL-SABON           TO CERTNOA                       
PEMMOD         MOVE ER-0203            TO EMI-ERROR                     
PEMMOD         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
PEMMOD     ELSE                                                         
PEMMOD         MOVE AL-SANON           TO CERTNOA
041002     END-IF.
03967                                                                   
PEMMOD     IF SUFXL = ZERO                                              
PEMMOD         MOVE SPACES             TO SUFXI                         
PEMMOD         MOVE AL-SANON           TO SUFXA                         
PEMMOD     ELSE                                                         
PEMMOD         MOVE AL-SANON           TO SUFXA
041002     END-IF.
03973                                                                   
03974      IF CLMCARRL = ZERO                                           
03975          MOVE -1                 TO CLMCARRL                      
03976          MOVE ER-0194            TO EMI-ERROR                     
03977          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03978      ELSE                                                         
03979          MOVE CLMCARRI           TO PI-CARRIER                    
03980          IF PI-NO-CARRIER-SECURITY                                
03981              MOVE AL-UANON       TO CLMCARRA
041002         END-IF
041002     END-IF.
03982                                                                   
03983      IF CLMTYPEL > ZERO                                     
020816        IF ((PI-COMPANY-ID = 'DCC' OR 'VPP')
100518           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
052614              AND PI-AH-OVERRIDE-L1 AND 'I' AND 'G' AND 'F'
022122              AND 'B' AND 'H'))
012009                        OR
062121           ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
012009           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
012009              AND PI-AH-OVERRIDE-L1))
03986               MOVE -1             TO CLMTYPEL                      
03987               MOVE AL-UABON       TO CLMTYPEA                      
03988               MOVE ER-7634        TO EMI-ERROR                     
03989               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
03990         ELSE                                                     
03991            MOVE AL-UANON       TO CLMTYPEA
041002        END-IF
03992      ELSE                                                         
03993         IF MAINTI = 'A'                                          
03994            MOVE -1             TO CLMTYPEL                      
03995            MOVE AL-UABON       TO CLMTYPEA                      
03996            MOVE ER-0546        TO EMI-ERROR                     
03997            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
041002     END-IF

           if instypel > ZERO                                     
              if instypei = 'P' OR 'C'
                 move al-uanon         to instypea
              else
                 MOVE -1               TO INSTYPEL                      
                 MOVE AL-UABON         TO INSTYPEA                      
                 MOVE ER-1654          TO EMI-ERROR                     
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT             
              END-IF
           ELSE                                                         
              IF MAINTI = 'A'                                          
                 MOVE -1               TO INSTYPEL
                 MOVE AL-UABON         TO INSTYPEA
                 MOVE ER-1654          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           if (instypel <> zeros)
              and (emi-error not = er-1654)
              and (instypei = 'C')
              and (crtfnmei = fstnmei)
              move er-1675             to emi-error
              MOVE -1                  TO INSTYPEL
              MOVE AL-UABON            TO INSTYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
          end-if

           if (instypel <> zeros)
              and (emi-error not = er-1654)
              and (instypei = 'P')
              and (crtfnmei <> fstnmei)
              move er-1676             to emi-error
              MOVE -1                  TO INSTYPEL
              MOVE AL-UABON            TO INSTYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
          end-if

03999      IF PCERTNOL = ZERO                                           
04000         MOVE CERTNOI             TO PCERTNOI                      
04001         MOVE CERTNOL             TO PCERTNOL                      
04002         MOVE AL-UANON            TO PCERTNOA                      
04003      ELSE                                                         
04004         MOVE AL-UANON            TO PCERTNOA
041002     END-IF.
04005                                                                   
04006      IF PSUFXL = ZERO                                             
04007         MOVE SUFXI               TO PSUFXI                        
04008         MOVE SUFXL               TO PSUFXL                        
04009         MOVE AL-UANON            TO PSUFXA                        
04010      ELSE                                                         
04011         MOVE AL-UANON            TO PSUFXA
041002     END-IF.
04012                                                                   
04013      IF MAINTI = 'I'                                              
04014          GO TO 6020-EDIT
041002     END-IF.
04015                                                                   
04016      IF LSTNMEL = ZERO AND CERTMTL NOT = ZEROS                    
04017          MOVE -1                 TO LSTNMEL                       
04018          MOVE ER-0236            TO EMI-ERROR                     
04019          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
04020      ELSE                                                         
04021          MOVE AL-UANON           TO LSTNMEA
041002     END-IF.

           if fstnmel not = zeros
              move al-uanon            to fstnmea
           end-if
020513     IF SEXL > ZEROS
020513        IF SEXI = 'M' OR 'F'
                 move al-uanon         to sexa
020513           CONTINUE
020513        ELSE
020513           MOVE -1               TO SEXL
020513           MOVE AL-UABON         TO SEXA
020513           MOVE ER-0219          TO EMI-ERROR
020513           PERFORM 9900-ERROR-FORMAT
020513                                 THRU 9900-EXIT
020513        END-IF
020513     ELSE
020513        IF MAINTI = 'A'
020513           MOVE -1               TO SEXL
020513           MOVE AL-UABON         TO SEXA
020513           MOVE ER-0219          TO EMI-ERROR
020513           PERFORM 9900-ERROR-FORMAT
020513                                 THRU 9900-EXIT
020513        END-IF
020513     END-IF

071508     IF SSNL GREATER THAN ZERO
071508         MOVE SSNI TO WS-SOC-SEC-NUMBER
071508         IF WS-SOC-SEC-NO NUMERIC AND 
071508           (WS-SOC-SEC-BLANK = SPACES OR LOW-VALUES)
071508             NEXT SENTENCE
071508         ELSE
071508            IF WS-SSN-1-3 NUMERIC AND WS-SSN-4-5 NUMERIC AND
071508               WS-SSN-6-9 NUMERIC AND WS-SSN-DASH1 = '-' AND
071508               WS-SSN-DASH2 = '-'
071508                 NEXT SENTENCE
071508            ELSE
071508                 MOVE ER-0887        TO  EMI-ERROR
071508                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071508                 MOVE AL-UABON       TO  SSNA
071508                 MOVE -1             TO  SSNL
071508            END-IF
071508        END-IF
071508     END-IF.
071508
04031      IF BIRTHDTL = ZERO                                           
04032          GO TO 6020-EDIT
041002     END-IF.
04033                                                                   
04034      IF BIRTHDTL GREATER ZERO                                     
04035          MOVE BIRTHDTI           TO DATE-WORK                     
04036          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
04037          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY            
04038          MOVE '4'                TO DC-OPTION-CODE                
04039          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
04040          IF DATE-CONVERSION-ERROR                                 
04041              MOVE AL-UABON       TO BIRTHDTA                      
04042              MOVE -1             TO BIRTHDTL                      
04043              MOVE ER-0220        TO EMI-ERROR                     
04044              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04045              GO TO 6020-EDIT
041002         END-IF
041002     END-IF.
04046                                                                   
04047 ******************************************************************
04048 **   IF CALCULATED BIRTH DATE GREATER THAN TODAYS DATE          **
04049 **   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **
04050 **   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **
04051 ******************************************************************
04052                                                                   
04053      IF DC-BIN-DATE-1 GREATER THAN SAVE-BIN-DATE                  
04054          MOVE BIRTHDTI           TO  DATE-WORK                    
04055          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
04056          MOVE NUM-WORK           TO  DC-GREG-DATE-1-MDY           
04057          MOVE '4'                TO  DC-OPTION-CODE               
04058          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT        
04059          MOVE +0                 TO  DC-ELAPSED-MONTHS            
04060                                      DC-ELAPSED-DAYS              
04061          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
04062          IF DATE-CONVERSION-ERROR                                 
04063              MOVE AL-UABON       TO  BIRTHDTA                     
04064              MOVE -1             TO  BIRTHDTL                     
04065              MOVE ER-0220        TO  EMI-ERROR                    
04066              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04067              GO TO 6020-EDIT
041002         END-IF
041002     END-IF.
04068                                                                   
04069      MOVE AL-UANON               TO  BIRTHDTA.                    
04070      MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHDTI.                    
04071      MOVE DC-BIN-DATE-1          TO  WS-BIRTHDT.                  
04072      MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.       
04073                                                                   
04074  6020-EDIT.

04075      IF INCURL GREATER ZERO                                       
04076          MOVE INCURI             TO DATE-WORK                     
04077          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
04078          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY            
04079          MOVE '4'                TO DC-OPTION-CODE                
04080          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
04081          IF DATE-CONVERSION-ERROR                                 
04082              MOVE AL-UABON       TO INCURA                        
04083              MOVE -1             TO INCURL                        
04084              MOVE ER-0222        TO EMI-ERROR                     
04085              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04086          ELSE                                                     
04087              MOVE AL-UANON       TO INCURA                        
04088              MOVE DC-GREG-DATE-1-EDIT  TO INCURI                  
04089              MOVE DC-BIN-DATE-1  TO WS-INCUR                      
04090                                     PI-SAVE-INCUR-DT              
04091              IF WS-INCUR GREATER THAN WS-TODAY-DT
04092                  MOVE ER-0511     TO EMI-ERROR
04093                  MOVE -1          TO INCURL
04094                  MOVE AL-UABON    TO INCURA
04095                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04096              ELSE                                                 
041002*                NEXT SENTENCE
041002                 CONTINUE
041002             END-IF
               END-IF
04098      ELSE
04099          MOVE ER-0516         TO EMI-ERROR
04100          MOVE AL-UABON        TO INCURA
04101          MOVE -1              TO INCURL
04102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04103                                                                   
04104      IF REPORTL GREATER ZERO                                      
04105          MOVE REPORTI            TO DATE-WORK                     
04106          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
04107          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY            
04108          MOVE '4'                TO DC-OPTION-CODE                
04109          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
04110          IF DATE-CONVERSION-ERROR                                 
04111              MOVE AL-UABON       TO REPORTA                       
04112              MOVE -1             TO REPORTL                       
04113              MOVE ER-0223        TO EMI-ERROR                     
04114              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04115          ELSE                                                     
04116              MOVE AL-UANON       TO REPORTA                       
04117              MOVE DC-GREG-DATE-1-EDIT  TO REPORTI                 
04118              MOVE DC-BIN-DATE-1  TO WS-REPORT                     
04119              IF WS-REPORT GREATER THAN WS-TODAY-DT                
04120                  MOVE ER-0512     TO EMI-ERROR
04121                  MOVE -1          TO REPORTL
04122                  MOVE AL-UABON    TO REPORTA
04123                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04124              ELSE                                                 
041002*                NEXT SENTENCE
041002                 CONTINUE
041002             END-IF
041002         END-IF
04126      ELSE
04127          MOVE ER-0517         TO EMI-ERROR
04128          MOVE -1              TO REPORTL
04129          MOVE AL-UABON        TO REPORTA
04130          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04131                                                                   
04132      IF WS-REPORT NOT = LOW-VALUES                                
04133          IF WS-INCUR GREATER THAN WS-REPORT
04134              MOVE AL-UABON       TO REPORTA                       
04135              MOVE -1             TO REPORTL                       
04136              MOVE ER-0509        TO EMI-ERROR                     
04137              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04138                                                                   
040814     IF ICD1L GREATER THAN ZERO
040814         IF (ICD1I GREATER THAN SPACES) AND 
040814            (ICD1I(4:1) NOT = '.')
040814             IF ICD1I(4:1) = ' '
040814                 MOVE '.' TO ICD1I(4:1)
040814             ELSE
040814                 IF ICD1I(8:1) = ' '
040814                     MOVE ICD1I(7:1) TO ICD1I(8:1)
040814                     MOVE ICD1I(6:1) TO ICD1I(7:1)
040814                     MOVE ICD1I(5:1) TO ICD1I(6:1)
040814                     MOVE ICD1I(4:1) TO ICD1I(5:1)
040814                     MOVE '.'        TO ICD1I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD1I GREATER THAN SPACES) AND 
040814            (ICD1I(1:1) NOT > ' ' OR
040814             ICD1I(2:1) NOT > ' ' OR 
040814             ICD1I(3:1) NOT > ' ' OR 
040814             ICD1I(4:1) NOT = '.')
040814              MOVE ER-0992        TO  EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE AL-UABON       TO ICD1A
040814              MOVE -1             TO ICD1L
040814         END-IF
040814     END-IF.
040814
040814     IF ICD2L GREATER THAN ZERO
040814         IF (ICD2I GREATER THAN SPACES) AND 
040814            (ICD2I(4:1) NOT = '.')
040814             IF ICD2I(4:1) = ' '
040814                 MOVE '.' TO ICD2I(4:1)
040814             ELSE
040814                 IF ICD2I(8:1) = ' '
040814                     MOVE ICD2I(7:1) TO ICD2I(8:1)
040814                     MOVE ICD2I(6:1) TO ICD2I(7:1)
040814                     MOVE ICD2I(5:1) TO ICD2I(6:1)
040814                     MOVE ICD2I(4:1) TO ICD2I(5:1)
040814                     MOVE '.'        TO ICD2I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD2I GREATER THAN SPACES) AND 
040814            (ICD2I(1:1) NOT > ' ' OR
040814             ICD2I(2:1) NOT > ' ' OR 
040814             ICD2I(3:1) NOT > ' ' OR 
040814             ICD2I(4:1) NOT = '.')
040814              MOVE ER-0992        TO  EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE AL-UABON       TO ICD2A
040814              MOVE -1             TO ICD2L
040814         END-IF
040814     END-IF
040814
040814*    IF ESTENDL GREATER ZERO                                      
040814*        MOVE ESTENDI            TO DATE-WORK                     
040814*        PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
040814*        MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY            
040814*        MOVE '4'                TO DC-OPTION-CODE                
040814*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
040814*        IF DATE-CONVERSION-ERROR                                 
040814*            MOVE AL-UABON       TO ESTENDA                       
040814*            MOVE -1             TO ESTENDL                       
040814*            MOVE ER-0224        TO EMI-ERROR                     
040814*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
040814*        ELSE                                                     
040814*            MOVE AL-UANON       TO ESTENDA                       
040814*            MOVE DC-GREG-DATE-1-EDIT  TO ESTENDI                 
040814*            MOVE DC-BIN-DATE-1  TO WS-ESTEND                     
040814*            IF CLMTYPEI = PI-LIFE-OVERRIDE-L1                    
040814*               MOVE AL-UABON    TO ESTENDA                       
040814*               MOVE -1          TO ESTENDL                       
040814*               MOVE ER-0544     TO EMI-ERROR                     
040814*               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*            END-IF
040814*        END-IF
040814*    END-IF.
040814*                                                                 
040814*    IF WS-ESTEND NOT = LOW-VALUES                                
040814*        IF WS-INCUR GREATER THAN WS-ESTEND
040814*            MOVE AL-UABON       TO ESTENDA                       
040814*            MOVE -1             TO ESTENDL                       
040814*            MOVE ER-0510        TO EMI-ERROR                     
040814*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814*        END-IF
040814*    END-IF.
04166                                                                   
04167      IF PRTOPTL GREATER ZERO                                      
04168          IF PRTOPTI NOT = 'N' AND 'L'                             
04169              MOVE AL-UABON       TO PRTOPTA                       
04170              MOVE -1             TO PRTOPTL                       
04171              MOVE ER-0334        TO EMI-ERROR                     
04172              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04173                                                                   
04174      IF MANRSVL GREATER ZERO                                      
               EXEC CICS BIF DEEDIT
04176              FIELD   (MANRSVI)
04177              LENGTH  (9)
04178          END-EXEC
04179          IF MANRSVI NOT NUMERIC                                   
04180              MOVE AL-UNBON       TO MANRSVA                       
04181              MOVE -1             TO MANRSVL                       
04182              MOVE ER-0489        TO EMI-ERROR                     
04183              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04184          ELSE                                                     
04185              MOVE AL-UNNON       TO MANRSVA                       
04186              MOVE MANRSVI        TO WS-MANRSV  MANRSVO
041002         END-IF
041002     END-IF.
04187                                                                   
04188      MOVE SPACES                 TO ELCNTL-KEY.                   
04189                                                                   
04190      IF CONTROL-IS-ACTUAL-CARRIER                                 
04191          MOVE PI-CARRIER         TO CNTL-CARRIER                  
04192      ELSE                                                         
04193          MOVE PI-CARRIER-CONTROL-LEVEL TO CNTL-CARRIER
041002     END-IF.
04194                                                                   
04195      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
04196      MOVE '6'                    TO CNTL-REC-TYPE.                
04197      MOVE +0                     TO CNTL-SEQ-NO.                  
04198                                                                   
04199      
           EXEC CICS HANDLE CONDITION
04200          NOTFND   (3100-CARRIER-NOT-FOUND)
04201      END-EXEC.
04202                                                                   
04203      PERFORM 7970-READ-CNTL THRU 7970-EXIT.                       
04204                                                                   
04205      IF NOT CF-MANUAL-RESERVES-USED AND                           
04206            WS-MANRSV GREATER ZERO                                 
04207          MOVE ER-0518             TO EMI-ERROR
04208          MOVE -1                  TO MANRSVL
04209          MOVE AL-UNBON            TO MANRSVA
04210          MOVE ZEROS               TO WS-MANRSV
04211          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04212                                                                   
04213      MOVE CF-RESERVE-CONTROLS    TO WS-RESERVE-CONTROLS.          
04214      MOVE CF-EXPENSE-CONTROLS    TO WS-EXPENSE-CONTROLS.          
04215                                                                   
04216      IF SUPVL GREATER ZERO                                        
04217          IF SUPVI NOT = ' ' AND 'Y' AND 'N'                       
04218              MOVE -1             TO SUPVL                         
04219              MOVE AL-UABON       TO SUPVA                         
04220              MOVE ER-0230        TO EMI-ERROR                     
04221              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04222          ELSE                                                     
04223              MOVE AL-UANON       TO SUPVA
041002         END-IF
041002     END-IF.
04224                                                                   
04225      IF PROCCDL = ZERO                                            
04226         GO TO 6050-CONTINUE-EDITS
041002     END-IF.
04227                                                                   
04228      MOVE SPACES                 TO ELCNTL-KEY.                   
04229      MOVE PROCCDI                TO CNTL-ACCESS.                  
04230                                                                   
04231      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
04232      MOVE '2'                    TO CNTL-REC-TYPE.                
04233      MOVE +0                     TO CNTL-SEQ-NO.                  
04234                                                                   
04235      
           EXEC CICS HANDLE CONDITION
04236          NOTFND   (6040-NO-PROCESSOR-RECORD)
04237      END-EXEC.
04238                                                                   
04239      PERFORM 7970-READ-CNTL THRU 7970-EXIT.                       
04240                                                                   
04241      GO TO 6050-CONTINUE-EDITS.                                   
04242                                                                   
04243  6040-NO-PROCESSOR-RECORD.

04244      MOVE ER-0019                TO EMI-ERROR.
04245      MOVE -1                     TO PROCCDL.
04246      MOVE AL-UABON               TO PROCCDA.
04247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04248                                                                   
04249  6050-CONTINUE-EDITS.                                             
04250      IF (BENECDL = ZERO) OR (BENECDI = SPACES OR LOW-VALUES)      
04251            GO TO 6070-CONTINUE-EDITS
041002     END-IF.
04252                                                                   
04253      MOVE BENECDI                TO BENE-CODE.                    
04254      MOVE 'B'                    TO BENE-RCD-TYPE.                
04255      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.                 
04256                                                                   
04257      
           EXEC CICS HANDLE CONDITION
04258          NOTFND   (6060-NO-BENEFICIARY-RECORD)
04259      END-EXEC.
04260                                                                   
04261      EXEC CICS READ
04262           DATASET   (ELBENE-DSID)                                    CL*33
04263           SET       (ADDRESS OF BENEFICIARY-MASTER)                  CL*52
04264           RIDFLD    (ELBENE-KEY)
04265      END-EXEC.
04266                                                                   
04267      GO TO 6070-CONTINUE-EDITS.                                   
04268                                                                   
04269  6060-NO-BENEFICIARY-RECORD.

04270      MOVE ER-0565                TO EMI-ERROR.                    
04271      MOVE -1                     TO BENECDL.                      
04272      MOVE AL-UABON               TO BENECDA.                      
04273      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04274                                                                   
04275                                                                   
04276  6070-CONTINUE-EDITS.                                             
04277      IF CERTMTL GREATER ZERO                                      
04278          IF CERTMTI NOT = 'A' AND 'S'                             
04279              MOVE -1             TO CERTMTL                       
04280              MOVE AL-UABON       TO CERTMTA                       
04281              MOVE ER-0267        TO EMI-ERROR                     
04282              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04283          ELSE                                                     
04284              MOVE AL-UANON       TO CERTMTA
041002         END-IF
04285      ELSE                                                         
04286          GO TO 6000-EXIT
041002     END-IF.
04287                                                                   
04288      IF EFFDTL GREATER ZERO                                       
04289          MOVE EFFDTI             TO DATE-WORK                     
04290          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT                  
04291          MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY            
04292          MOVE '4'                TO DC-OPTION-CODE                
04293          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
04294          IF DATE-CONVERSION-ERROR                                 
04295              MOVE AL-UABON       TO EFFDTA                        
04296              MOVE -1             TO EFFDTL                        
04297              MOVE ER-0231        TO EMI-ERROR                     
04298              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04299          ELSE                                                     
04300              MOVE AL-UANON       TO EFFDTA                        
04301              MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI                  
04302              MOVE DC-BIN-DATE-1  TO WS-EFFDT
041002         END-IF
04303      ELSE                                                         
04304          MOVE ER-0231            TO EMI-ERROR                     
04305          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
04306          MOVE -1                 TO EFFDTL                        
04307          MOVE AL-UABON           TO EFFDTA
041002     END-IF.
04308                                                                   
04309      IF ACCOUNTL = ZERO                                           
04310          MOVE ER-0232            TO EMI-ERROR                     
04311          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
04312          MOVE AL-UABON           TO ACCOUNTA                      
04313          MOVE -1                 TO ACCOUNTL                      
04314      ELSE                                                         
04315          MOVE AL-UANON           TO ACCOUNTA
041002     END-IF.
04316                                                                   
04317      IF (PI-CERT-ACCESS-CONTROL = ' ' OR '1' OR '2')  AND         
04318         (STATEL = ZERO)                                           
04319          MOVE ER-0233        TO EMI-ERROR
04320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04321          MOVE AL-UABON       TO STATEA
04322          MOVE -1             TO STATEL
04323      ELSE
04324          MOVE AL-UANON       TO STATEA
041002     END-IF.
04325                                                                   
04326      IF (PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4') AND          
04327         (CRTCARRL = ZERO)                                         
04328          MOVE ER-0234        TO EMI-ERROR
04329          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04330          MOVE AL-UABON       TO CRTCARRA
04331          MOVE -1             TO CRTCARRL
04332      ELSE
04333          MOVE AL-UANON       TO CRTCARRA
041002     END-IF.
04334                                                                   
04335      IF PI-CERT-ACCESS-CONTROL = '1' AND                          
04336         GROUPL = ZERO                                             
04337          MOVE ER-0235            TO EMI-ERROR                     
04338          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
04339          MOVE AL-UABON           TO GROUPA                        
04340          MOVE -1                 TO GROUPL                        
04341      ELSE                                                         
04342          MOVE AL-UANON           TO GROUPA
041002     END-IF.
04343                                                                   
04344      IF CRTLNMEL = ZERO                                           
04345          IF LSTNMEI = SPACES OR LOW-VALUES                        
04346              MOVE ER-0236            TO  EMI-ERROR                
04347              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04348              MOVE AL-UABON           TO  CRTLNMEA                 
04349              MOVE -1                 TO  CRTLNMEL                 
04350          ELSE                                                     
04351              MOVE LSTNMEI            TO  CRTLNMEI                 
04352              MOVE AL-UANON           TO  CRTLNMEA
041002         END-IF
04353      ELSE                                                         
04354          MOVE AL-UANON               TO  CRTLNMEA
041002     END-IF.
04355                                                                   
04356      IF CRTFNMEL = ZERO                                           
04357          IF FSTNMEI = SPACES OR LOW-VALUES                        
041002*            NEXT SENTENCE
041002             CONTINUE
04359          ELSE                                                     
04360              MOVE FSTNMEI            TO  CRTFNMEI                 
04361              MOVE AL-UANON           TO  CRTFNMEA
041002         END-IF
041002     END-IF.
04362                                                                   
04363      IF CRTINITL = ZERO                                           
04364          IF INITI = SPACES OR LOW-VALUES                          
041002*            NEXT SENTENCE
041002             CONTINUE
04366          ELSE                                                     
04367              MOVE INITI              TO  CRTINITI                 
04368              MOVE AL-UANON           TO  CRTINITA
041002         END-IF
041002     END-IF.
04369                                                                   
04370  6000-EXIT.                                                       
04371      EXIT.                                                        
04372                                                                   
04373      EJECT                                                        
04374  6200-EDIT-CERT-DATA.                                             

04375      IF ISSAGEL GREATER ZERO                                      
04376          IF ISSAGEI NOT NUMERIC                                   
04377              MOVE ER-0237        TO EMI-ERROR                     
04378              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04379              MOVE -1             TO ISSAGEL                       
04380              MOVE AL-UNBON       TO ISSAGEA                       
04381          ELSE                                                     
04382              MOVE AL-UNNON       TO ISSAGEA
041002         END-IF
041002     END-IF.
04383                                                                   
04384      IF JNTAGEL GREATER ZERO                                      
04385          IF JNTAGEI NOT NUMERIC                                   
04386              MOVE ER-0238        TO EMI-ERROR                     
04387              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04388              MOVE -1             TO JNTAGEL                       
04389              MOVE AL-UNBON       TO JNTAGEA                       
04390          ELSE                                                     
04391              MOVE AL-UNNON       TO JNTAGEA
041002         END-IF
041002     END-IF.
04392                                                                   
04393      IF LCVCDL GREATER ZERO                                       
04394          MOVE LCVCDI             TO WS-EDIT-BEN-CODE              
04395          IF INVALID-BENEFIT-CODE                                  
04396              MOVE ER-7635        TO EMI-ERROR                     
04397              MOVE -1             TO LCVCDL                        
04398              MOVE AL-UABON       TO LCVCDA                        
04399              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04400          ELSE                                                     
04401              MOVE LCVCDI         TO WS-BEN-HOLD                   
04402              MOVE '4'            TO WS-REC-TYPE                   
04403              PERFORM 7100-READ-BENEFIT THRU 7199-EXIT             
04404              IF WS-BEN-ALPHA-HOLD = SPACES                        
04405                 MOVE ER-7635     TO EMI-ERROR                     
04406                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          
04407                 MOVE -1          TO LCVCDL                        
04408                 MOVE AL-UABON    TO LCVCDA                        
04409              ELSE                                                 
04410                 MOVE WS-BEN-ALPHA-HOLD  TO LCVKINDO               
04411                 MOVE WS-EARNINGS-CALC   TO WS-LF-EARNINGS-CALC    
04412                 MOVE WS-SPECIAL-CALC-CD TO WS-LF-SPECIAL-CALC-CD  
04413                 MOVE AL-UANON           TO LCVCDA
041002             END-IF
041002         END-IF
041002     END-IF.
04414                                                                   
04415      IF ACVCDL GREATER ZERO                                       
04416          MOVE ACVCDI             TO WS-EDIT-BEN-CODE              
04417          IF INVALID-BENEFIT-CODE                                  
04418              MOVE ER-7635        TO EMI-ERROR                     
04419              MOVE -1             TO ACVCDL                        
04420              MOVE AL-UABON       TO ACVCDA                        
04421              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04422          ELSE                                                     
04423              MOVE ACVCDI         TO WS-BEN-HOLD                   
04424              MOVE '5'            TO WS-REC-TYPE                   
04425              PERFORM 7100-READ-BENEFIT THRU 7199-EXIT             
04426              IF WS-BEN-ALPHA-HOLD = SPACES                        
04427                 MOVE ER-7635     TO EMI-ERROR                     
04428                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          
04429                 MOVE -1          TO ACVCDL                        
04430                 MOVE AL-UABON    TO ACVCDA                        
04431              ELSE                                                 
04432                 MOVE WS-BEN-ALPHA-HOLD  TO ACVKINDO               
04433                 MOVE WS-EARNINGS-CALC   TO WS-AH-EARNINGS-CALC    
04434                 MOVE WS-SPECIAL-CALC-CD TO WS-AH-SPECIAL-CALC-CD  
04435                 MOVE AL-UANON           TO ACVCDA
041002             END-IF
041002         END-IF
041002     END-IF.
04436                                                                   
04437      IF LCVOTRML GREATER ZERO                                     
04438          IF LCVOTRMI NOT NUMERIC                                  
04439              MOVE ER-7636        TO EMI-ERROR                     
04440              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04441              MOVE -1             TO LCVOTRML                      
04442              MOVE AL-UNBON       TO LCVOTRMA                      
04443          ELSE                                                     
04444              MOVE AL-UNNON       TO LCVOTRMA
041002         END-IF
041002     END-IF.
04445                                                                   
04446      IF ACVOTRML GREATER ZERO                                     
04447          IF ACVOTRMI NOT NUMERIC                                  
04448              MOVE ER-7636        TO EMI-ERROR                     
04449              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04450              MOVE -1             TO ACVOTRML                      
04451              MOVE AL-UNBON       TO ACVOTRMA                      
04452          ELSE                                                     
04453              MOVE AL-UNNON       TO ACVOTRMA
041002         END-IF
041002     END-IF.
04454                                                                   
04455      IF LCVRATEL GREATER ZERO                                     
               EXEC CICS BIF DEEDIT
04457              FIELD    (LCVRATEI)
04458              LENGTH   (06)
04459          END-EXEC
04460          IF LCVRATEI NOT NUMERIC                                  
04461              MOVE AL-UNBON       TO LCVRATEA                      
04462              MOVE -1             TO LCVRATEL                      
04463              MOVE ER-2280        TO EMI-ERROR                     
04464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04465          ELSE                                                     
04466              MOVE AL-UNNON       TO LCVRATEA                      
04467              MOVE LCVRATEI       TO WS-LCVRATE LCVRATEO
041002         END-IF
041002     END-IF.
04478                                                                   
04479      IF ACVRATEL GREATER ZERO                                     
               EXEC CICS BIF DEEDIT
04481              FIELD    (ACVRATEI)
04482              LENGTH   (06)
04483          END-EXEC
04484          IF ACVRATEI NOT NUMERIC                                  
04485              MOVE AL-UNBON       TO ACVRATEA                      
04486              MOVE -1             TO ACVRATEL                      
04487              MOVE ER-2280        TO EMI-ERROR                     
04488              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04489          ELSE                                                     
04490              MOVE AL-UNNON       TO ACVRATEA                      
04491              MOVE ACVRATEI       TO WS-ACVRATE ACVRATEO
041002         END-IF
041002     END-IF.
04502                                                                   
04503      IF LCVBENEL GREATER ZERO                                     
               EXEC CICS BIF DEEDIT
04505              FIELD    (LCVBENEI)
04506              LENGTH   (11)
04507          END-EXEC
04508          IF LCVBENEI NOT NUMERIC                                  
04509              MOVE AL-UNBON       TO LCVBENEA                      
04510              MOVE -1             TO LCVBENEL                      
04511              MOVE ER-7637        TO EMI-ERROR                     
04512              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04513          ELSE                                                     
04514              MOVE AL-UNNON       TO LCVBENEA                      
04515              MOVE LCVBENEI       TO WS-LCVBENE LCVBENEO
041002         END-IF
041002     END-IF.
04516                                                                   
04517      IF ACVBENEL GREATER ZERO                                     
               EXEC CICS BIF DEEDIT
04519              FIELD    (ACVBENEI)
04520              LENGTH   (11)
04521          END-EXEC
04522          IF ACVBENEI NOT NUMERIC                                  
04523              MOVE AL-UNBON       TO ACVBENEA                      
04524              MOVE -1             TO ACVBENEL                      
04525              MOVE ER-7637        TO EMI-ERROR                     
04526              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04527          ELSE                                                     
04528              MOVE AL-UNNON       TO ACVBENEA                      
04529              MOVE ACVBENEI       TO WS-ACVBENE ACVBENEO
041002         END-IF
041002     END-IF.
04543                                                                   
04544      IF LCVCNDTL GREATER ZERO                                     
04545          IF LCVCNDTI NOT = SPACES
04546              MOVE LCVCNDTI        TO DATE-WORK
04547              PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04548              MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04549              MOVE '4'             TO DC-OPTION-CODE
04550              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04551              IF DATE-CONVERSION-ERROR
04552                  MOVE AL-UABON    TO LCVCNDTA
04553                  MOVE -1          TO LCVCNDTL
04554                  MOVE ER-7638     TO EMI-ERROR
04555                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04556              ELSE
04557                  MOVE AL-UANON    TO LCVCNDTA
04558                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTI
04559                  MOVE DC-BIN-DATE-1 TO WS-LCVCNDT
041002             END-IF
041002         END-IF
041002     END-IF.
04560                                                                   
04561      IF ACVCNDTL GREATER ZERO                                     
04562          IF ACVCNDTI NOT = SPACES
04563              MOVE ACVCNDTI        TO DATE-WORK
04564              PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04565              MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04566              MOVE '4'             TO DC-OPTION-CODE
04567              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04568              IF DATE-CONVERSION-ERROR
04569                  MOVE AL-UABON    TO ACVCNDTA
04570                  MOVE -1          TO ACVCNDTL
04571                  MOVE ER-7638     TO EMI-ERROR
04572                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04573              ELSE
04574                  MOVE AL-UANON    TO ACVCNDTA
04575                  MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTI
04576                  MOVE DC-BIN-DATE-1 TO WS-ACVCNDT
041002             END-IF
041002         END-IF
041002     END-IF.
04577                                                                   
04578      IF ADDONDTL GREATER ZERO                                     
04579          MOVE ADDONDTI        TO DATE-WORK
04580          PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
04581          MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
04582          MOVE '4'             TO DC-OPTION-CODE
04583          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
04584          IF DATE-CONVERSION-ERROR
04585              MOVE AL-UABON    TO ADDONDTA
04586              MOVE -1          TO ADDONDTL
04587              MOVE ER-7651     TO EMI-ERROR
04588              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04589          ELSE
04590              MOVE AL-UANON    TO ADDONDTA
04591              MOVE DC-GREG-DATE-1-EDIT TO ADDONDTI
04592              MOVE DC-BIN-DATE-1 TO WS-ADD-ON-DT
041002         END-IF
041002     END-IF.
04602                                                                   
04603      IF APRL GREATER ZERO                                         
               EXEC CICS BIF DEEDIT
04605              FIELD    (APRI)
04606              LENGTH   (8)
04607          END-EXEC
04608          IF APRI NOT NUMERIC                                      
04609              MOVE AL-UNBON       TO APRA                          
04610              MOVE -1             TO APRL                          
04611              MOVE ER-0248        TO EMI-ERROR                     
04612              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04613          ELSE                                                     
04614              MOVE AL-UNNON       TO APRA                          
04615              MOVE APRI           TO WS-APR APRO
041002         END-IF
041002     END-IF.
04626                                                                   
04627      IF PMTFREQL GREATER ZERO                                     
               EXEC CICS BIF DEEDIT
04629              FIELD   (PMTFREQI)
04630              LENGTH  (2)
04631          END-EXEC
04632          IF PMTFREQI NOT NUMERIC                                  
04633              MOVE -1             TO PMTFREQL                      
04634              MOVE AL-UNBON       TO PMTFREQA                      
04635              MOVE ER-0258        TO EMI-ERROR                     
04636              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04637          ELSE                                                     
04638              MOVE AL-UNNON       TO PMTFREQA                      
04639              MOVE PMTFREQI       TO WS-PMTFREQ  PMTFREQO
041002         END-IF
041002     END-IF.
04640                                                                   
04641      IF INDGRPL GREATER ZERO                                      
04642          IF INDGRPI = 'I' OR 'G'                                  
04643              MOVE AL-UANON       TO INDGRPA                       
04644          ELSE                                                     
04645              MOVE -1 TO INDGRPL                                   
04646              MOVE AL-UABON       TO INDGRPA                       
04647              MOVE ER-0260        TO EMI-ERROR                     
04648              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04649                                                                   
04650      IF LOANBALL GREATER ZERO                                     
04651          MOVE LOANBALI           TO DEEDIT-FIELD                  
               EXEC CICS BIF DEEDIT
04653              FIELD   (DEEDIT-FIELD)
04654              LENGTH  (12)
04655          END-EXEC
04656          IF DEEDIT-FIELD NOT NUMERIC                              
04657              MOVE AL-UNBON       TO LOANBALA                      
04658              MOVE -1             TO LOANBALL                      
04659              MOVE ER-0247        TO EMI-ERROR                     
04660              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04661          ELSE                                                     
04662              MOVE AL-UNNON       TO LOANBALA                      
04663              MOVE DEEDIT-FIELD   TO HOLD-LOAN-BAL  LOANBALO
041002         END-IF
041002     END-IF.
04674                                                                   
04675      IF PREMTYPL GREATER ZERO                                     
04676          IF (PREMTYPI = '1' OR '2' OR '3')                        
04677              MOVE AL-UANON       TO PREMTYPA                      
04678          ELSE                                                     
04679              MOVE -1             TO PREMTYPL                      
04680              MOVE AL-UABON       TO PREMTYPA                      
04681              MOVE ER-0227        TO EMI-ERROR                     
04682              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04683                                                                   
04684      IF REINCDL GREATER ZERO                                      
04685          MOVE LOW-VALUES         TO  ERREIN-KEY                   
04686          MOVE PI-COMPANY-CD      TO  REIN-COMP-CD                 
04687          MOVE REINCDI            TO  REIN-CODE-1                  
04688                                      REIN-CODE-2                  
04689                                      REIN-CODE-3                  
04690          MOVE 'A'                TO  REIN-TYPE                    
04691          PERFORM 7992-READ-REIN THRU 7992-EXIT                    
04692          IF REIN-REC-NOT-FOUND                                    
04693              MOVE ER-0266        TO  EMI-ERROR                    
04694              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04695              MOVE -1             TO  REINCDL                      
04696              MOVE AL-UABON       TO  REINCDA                      
04697          ELSE                                                     
04698              MOVE AL-UANON       TO  REINCDA
041002         END-IF
041002     END-IF.
04699                                                                   
04700      IF CERTMTI NOT = 'A'                                         
04701         GO TO 6200-EXIT
041002     END-IF.
04702                                                                   
04728      IF PREMTYPI = '1'                                            
04729          IF LCVCDL GREATER ZERO                                   
04730              IF WS-LF-SPECIAL-CALC-CD = 'O'                       
04731                  MOVE ER-0841        TO  EMI-ERROR                
04732                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
04733                  MOVE -1             TO  PREMTYPL                 
04734                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
               END-IF
041002     END-IF.
04735                                                                   
04736      IF PREMTYPI = '1'                                            
04737          IF ACVCDL GREATER ZERO                                   
04738              IF WS-AH-SPECIAL-CALC-CD = 'O'                       
04739                  MOVE ER-0841        TO  EMI-ERROR                
04740                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
04741                  MOVE -1             TO  PREMTYPL                 
04742                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
               END-IF
041002     END-IF.
04743                                                                   
04744      IF PREMTYPI = '2'                                            
04745          IF LCVCDL GREATER ZERO
04746              IF (PI-COMPANY-ID = 'HAN') AND
04747                 (WS-LF-SPECIAL-CALC-CD = 'O' OR 'M')              
04748                         OR                                        
04749                  WS-LF-SPECIAL-CALC-CD = 'O'                      
041002*                NEXT SENTENCE
04750                  CONTINUE
04751              ELSE
04752                  MOVE ER-0840        TO  EMI-ERROR
04753                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04754                  MOVE -1             TO  PREMTYPL
04755                  MOVE AL-UABON       TO  PREMTYPA
04756              END-IF
041002         END-IF
041002     END-IF.
04756                                                                   
04757      IF PREMTYPI = '2'                                            
04758          IF ACVCDL GREATER ZERO                                   
04759              IF (PI-COMPANY-ID = 'HAN')  AND                      
04760                 (WS-AH-SPECIAL-CALC-CD = 'O' OR 'M')              
04761                         OR                                        
04762                  WS-AH-SPECIAL-CALC-CD = 'O'                      
041002*                NEXT SENTENCE
041002                 CONTINUE
04764              ELSE
04765                  MOVE ER-0840        TO  EMI-ERROR
04766                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04767                  MOVE -1             TO  PREMTYPL
04768                  MOVE AL-UABON       TO  PREMTYPA
041002             END-IF
041002         END-IF
041002     END-IF.
04769                                                                   
04788      IF LCVCDL = ZEROS                                            
04789         GO TO 6200-EXIT
041002     END-IF.
04790
04791      IF PMTFREQL NOT = ZEROS  AND NOT TEX-REG                     
04792          IF PMTFREQI NOT = ZEROS                                  
04793              MOVE ER-0428        TO EMI-ERROR                     
04794              MOVE -1             TO PMTFREQL                      
04795              MOVE AL-UNBON       TO PMTFREQA                      
04796              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
04797              GO TO 6200-CHECK-APR
041002         END-IF
041002     END-IF.
04798                                                                   
04799      IF PMTFREQL = ZEROS AND TEX-REG                              
04800          MOVE ER-0429            TO EMI-ERROR                     
04801          MOVE -1                 TO PMTFREQL                      
04802          MOVE AL-UNBON           TO PMTFREQA                      
04803          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
04804          GO TO 6200-CHECK-APR
041002     END-IF.
04805                                                                   
04806      IF LCVOTRMI NOT NUMERIC                                      
04807          GO TO 6200-CHECK-APR
041002     END-IF.
04808                                                                   
04809      IF WS-PMTFREQ NOT = ZERO                                     
04810          DIVIDE WS-PMTFREQ INTO LCVOTRMI                          
04811          GIVING DIVIDE-QUOT                                       
04812          REMAINDER DIVIDE-REM                                     
04813          IF DIVIDE-REM GREATER ZERO
04814              MOVE ER-0430            TO EMI-ERROR
04815              MOVE -1                 TO PMTFREQL
04816              MOVE AL-UNBON           TO PMTFREQA
04817              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04818
04819  6200-CHECK-APR.                                                  
04820      IF NET-PAY AND APRL = ZEROS                                  
04821         MOVE ER-0257             TO EMI-ERROR                     
04822         MOVE -1                  TO APRL                          
04823         MOVE AL-UABON            TO APRA                          
04824         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04825
04826  6200-EXIT.                                                       
04827      EXIT.                                                        
04828                                                                   
04829      EJECT                                                        
04830  6300-REQUIRED-CERT-EDIT.                                         
04831                                                                   
052614     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
04833         GO TO 6300-REQUIRED-AH-EDIT
041002     END-IF
04834                                                                   
04835      IF LCVCDL = ZERO                                             
04836         MOVE ER-7639             TO EMI-ERROR                     
04837         MOVE -1                  TO LCVCDL                        
04838         MOVE AL-UABON            TO LCVCDA                        
04839         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04840                                                                   
04841      IF LCVOTRML = ZERO                                           
04842         MOVE ER-7670             TO EMI-ERROR                     
04843         MOVE -1                  TO LCVOTRML                      
04844         MOVE AL-UNBON            TO LCVOTRMA                      
04845         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04846                                                                   
04847      IF LCVBENEL = ZERO                                           
04848         MOVE ER-7671             TO EMI-ERROR                     
04849         MOVE -1                  TO LCVBENEL                      
04850         MOVE AL-UNBON            TO LCVBENEA                      
04851         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04852                                                                   
04853      GO TO 6399-EXIT.                                             
04854                                                                   
04855  6300-REQUIRED-AH-EDIT.                                           
04856                                                                   
04857      IF ACVCDL = ZERO                                             
04858         MOVE ER-7639             TO EMI-ERROR                     
04859         MOVE -1                  TO ACVCDL                        
04860         MOVE AL-UABON            TO ACVCDA                        
04861         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04862                                                                   
04863      IF ACVOTRML = ZERO                                           
04864          MOVE ER-7670             TO EMI-ERROR                    
04865          MOVE -1                  TO ACVOTRML                     
04866          MOVE AL-UNBON            TO ACVOTRMA                     
04867          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04868                                                                   
04869      IF ACVBENEL = ZERO                                           
04870         MOVE ER-7671             TO EMI-ERROR                     
04871         MOVE -1                  TO ACVBENEL                      
04872         MOVE AL-UNBON            TO ACVBENEA                      
04873         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04874                                                                   
04875  6399-EXIT.                                                       
04876       EXIT.                                                       
04877                                                                   
04878      EJECT                                                        
04879  6400-TEST-CLAIM-REASONABILITY.                                   

04881      IF WS-INCUR = LOW-VALUES                                     
04882         GO TO 6499-EXIT
041002     END-IF.
04883                                                                   
04884      IF WS-EFFDT NOT = LOW-VALUES                                 
04885          IF WS-INCUR LESS WS-EFFDT
04886              MOVE ER-0458          TO EMI-ERROR
04887              MOVE -1               TO INCURL
04888              MOVE AL-UABON         TO INCURA
04889              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002         END-IF
041002     END-IF.
04890                                                                   
04891      IF CERTMTI = 'A'                                             
04892         GO TO 6460-TEST-NEW-CERT
041002     END-IF.
04893                                                                   
04894      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 
04895      MOVE CRTCARRI               TO CERT-CARRIER.                 
04896      MOVE GROUPI                 TO CERT-GROUPING.                
04897      MOVE STATEI                 TO CERT-STATE.                   
04898      MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.                 
04899      MOVE PI-SAVE-CERT           TO CERT-CERT-NO.                 
04900      MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.                  
04901                                                                   
04902      
           EXEC CICS HANDLE CONDITION
04903           DUPKEY   (6499-EXIT)
04904           NOTFND   (6499-EXIT)
04905      END-EXEC.
04906                                                                   
04907      PERFORM 7940-READ-CERT THRU 7940-EXIT.                       
04908                                                                   
121802     IF (CLMTYPEI = PI-AH-OVERRIDE-L1  OR
052614        'I' OR 'G' OR 'F'
022122         OR 'B' OR 'H')
04910         MOVE CM-AH-ORIG-TERM     TO DC-ELAPSED-MONTHS             
04911      ELSE                                                         
04912         MOVE CM-LF-ORIG-TERM     TO DC-ELAPSED-MONTHS
041002     END-IF.
04913                                                                   
04914      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
04915      MOVE '6'                    TO DC-OPTION-CODE.               
04916      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
04917                                                                   
04918      IF (WS-INCUR GREATER THAN DC-BIN-DATE-2)
070714        and (dc-elapsed-months > zeros)
04919         MOVE ER-0519             TO EMI-ERROR                     
04920         MOVE -1                  TO INCURL                        
04921         MOVE AL-UABON            TO INCURA                        
04922         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04923                                                                   
04924      IF CLMCARRI NOT = CRTCARRI                                   
04925         MOVE ER-0562             TO EMI-ERROR                     
04926         MOVE AL-UABON            TO CLMCARRA                      
04927         MOVE -1                  TO CLMCARRL                      
04928         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
04929                                                                   
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
052614      IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122          OR 'B' OR 'H') AND
04931         CM-AH-BENEFIT-CD = '00')
                        OR
100518        ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
04933         CM-LF-BENEFIT-CD = '00')                                   
04934          MOVE ER-0521         TO EMI-ERROR
04935          MOVE AL-UABON        TO CLMTYPEA
04936          MOVE -1              TO CLMTYPEL
04937          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002      END-IF
070714     end-if

031405*    IF CLMTYPEI = 'I' AND
031405*       CM-AH-CRITICAL-PERIOD = +0
031405*        MOVE ER-0521         TO EMI-ERROR
031405*        MOVE AL-UABON        TO CLMTYPEA
031405*        MOVE -1              TO CLMTYPEL
031405*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031405*    END-IF
04938                                                                   
121802*    IF PI-COMPANY-ID = 'CRI'                                     
121802*        IF MAINTI = 'A'                                          
121802*            IF CERT-PEND-ISSUE-ERROR  OR                         
121802*               CERT-PEND-ISSUE-RETURNED                          
121802*                MOVE -1                 TO CERTNOL               
121802*                MOVE AL-UABON           TO CERTNOA               
121802*                MOVE ER-7640            TO EMI-ERROR             
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            END-IF
121802*        END-IF
121802*    END-IF.
04947                                                                   
04948      IF (CM-AH-CANCEL-DT  NOT  =  LOW-VALUES AND                  
052614         (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122          OR 'B' OR 'H'))
04950          IF WS-INCUR GREATER THAN CM-AH-CANCEL-DT                 
04951              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04952
04953      IF (CM-LF-CANCEL-DT  NOT  =  LOW-VALUES AND                  
100518         (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))
04955          IF WS-INCUR GREATER THAN CM-LF-CANCEL-DT
04956              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04957                                                                   
04958      IF CM-LF-DEATH-DT NOT = LOW-VALUES                           
04959          IF WS-INCUR GREATER THAN CM-LF-DEATH-DT
04960              GO TO 6450-DATE-ERROR
041002         END-IF
041002     END-IF.
04961                                                                   
04962      GO TO 6499-EXIT.                                             
04963                                                                   
04964  6450-DATE-ERROR.                                                 
04965      MOVE ER-0520                TO EMI-ERROR.
04966      MOVE -1                     TO INCURL.
04967      MOVE AL-UABON               TO INCURA.
04968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04969                                                                   
121802*    IF (PI-COMPANY-ID = 'FIM' OR 'LGX') AND                      
121802*       (EMI-SEVERITY-SAVE = 'X')                                 
121802*        ADD 1                TO EMI-FORCABLE-CTR
121802*        SUBTRACT 1 FROM EMI-FATAL-CTR
121802*    END-IF.
04974                                                                   
04975      GO TO 6499-EXIT.                                             
04976                                                                   
04977      EJECT                                                        
04978  6460-TEST-NEW-CERT.                                              
04979                                                                   
04980      IF NOT EMI-NO-ERRORS                                         
04981         GO TO 6499-EXIT
041002     END-IF.
04982                                                                   
04983      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.            
04984                                                                   
052614     IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122          OR 'F' OR 'B' OR 'H') AND
04986          ACVOTRML GREATER ZERO)                                    
04987         MOVE ACVOTRMI       TO DC-ELAPSED-MONTHS
041002     END-IF.
04988                                                                   
100518     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
04990          LCVOTRML GREATER ZERO                                    
04991         MOVE LCVOTRMI       TO DC-ELAPSED-MONTHS
041002     END-IF.
04992                                                                   
04993      MOVE WS-EFFDT               TO DC-BIN-DATE-1.
04994                                                                   
121802*    IF PI-COMPANY-ID = 'HAN'                                     
121802*        IF ADDONDTL GREATER ZERO                                 
121802*            MOVE WS-ADD-ON-DT   TO  DC-BIN-DATE-1                
121802*            IF CLMTYPEI = 'A'                                    
121802*                IF ACVOTRMI NOT = ZEROS                          
121802*                    COMPUTE DC-ELAPSED-MONTHS = (ACVOTRMI - 1)   
121802*                ELSE                                             
041002*                    NEXT SENTENCE
121802*                    CONTINUE
121802*                END-IF
121802*            ELSE                                                 
121802*                IF LCVOTRMI NOT = ZEROS                          
121802*                    COMPUTE DC-ELAPSED-MONTHS = (LCVOTRMI - 1)
121802*                END-IF
121802*            END-IF
121802*        END-IF
121802*    END-IF.
05006                                                                   
05007      MOVE '6'                    TO DC-OPTION-CODE.
05008      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
05009                                                                   
05010      IF NO-CONVERSION-ERROR                                       
05011          MOVE DC-BIN-DATE-2      TO WS-EXPIRE
041002     END-IF.
05012                                                                   
05013      IF WS-INCUR GREATER THAN WS-EXPIRE                           
05014         MOVE ER-0519             TO EMI-ERROR                     
05015         MOVE -1                  TO INCURL                        
05016         MOVE AL-UABON            TO INCURA                        
05017         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
05018                                                                   
05019      IF CLMCARRI NOT = CRTCARRI                                   
05020         MOVE ER-0562             TO EMI-ERROR                     
05021         MOVE AL-UABON            TO CLMCARRA                      
05022         MOVE -1                  TO CLMCARRL                      
05023         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002     END-IF.
05024                                                                   
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
052614        IF (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122            OR 'B' OR 'H')
05026            and (ACVCDL = ZEROS)                                           
05027            MOVE ER-0521          TO EMI-ERROR                     
05028            MOVE AL-UABON         TO CLMTYPEA                      
05029            MOVE -1               TO CLMTYPEL                      
05030            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
070714     end-if
05031                                                                   
020816     if (pi-company-id = 'DCC' OR 'VPP')
070714        and (ws-dcc-product-code <> spaces)
070714        continue
070714     else
100518        IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
05033            and (LCVCDL = ZEROS)
05034            MOVE ER-0521          TO EMI-ERROR                     
05035            MOVE AL-UABON         TO CLMTYPEA                      
05036            MOVE -1               TO CLMTYPEL                      
05037            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041002        END-IF
070714     end-if
05038                                                                   
100518     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
05040          IF LCVCNDTL GREATER ZERO                                 
05041              IF WS-INCUR GREATER THAN WS-LCVCNDT                  
05042                  GO TO 6450-DATE-ERROR
041002             END-IF
041002         END-IF
041002     END-IF.
05043                                                                   
052614     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
05045          IF ACVCNDTL GREATER ZERO                                 
05046              IF WS-INCUR GREATER THAN WS-ACVCNDT                  
05047                  GO TO 6450-DATE-ERROR
041002             END-IF
041002         END-IF
041002     END-IF.
05048                                                                   
05049  6499-EXIT.                                                       
05050      EXIT.                                                        

061013 6500-get-acct.
061013
061013     MOVE SPACES                 TO ERACCT-KEY
061013                                    ws-dcc-product-code
                                          ws-acct-found-sw
061013
061013     IF CARR-GROUP-ST-ACCNT-CNTL
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013        MOVE CRTCARRI            TO ACCT-CARRIER
061013        MOVE GROUPI              TO ACCT-GROUPING
061013        MOVE STATEI              TO ACCT-STATE
061013        MOVE ACCOUNTI            TO ACCT-ACCOUNT
061013        GO TO 6500-startbr
061013     END-IF
061013
061013     MOVE PI-COMPANY-CD          TO ACCT-COMP-CD
061013     MOVE ACCOUNTI               TO ACCT-ACCOUNT
061013
061013     IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'
061013        MOVE STATEI             TO ACCT-STATE
061013     END-IF
061013
061013     IF PI-CERT-ACCESS-CONTROL = '2' OR '4'
061013        MOVE CRTCARRI           TO ACCT-CARRIER
061013     END-IF
061013
061013     .
061013 6500-startbr.
061013
061013     MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY
061013
061013     EXEC CICS STARTBR
061013        DATASET     (ERACCT2-DSID)
061013        RIDFLD      (SAVE-ERACCT-KEY)
061013        GENERIC
061013        KEYLENGTH   (20)
061013        resp        (ws-response)
061013     END-EXEC
061013
061013     if not ws-resp-normal
061013        go to 6500-exit
061013     end-if
061013
061013     .                                                                        
061013 6500-readnext.
061013     
061013     EXEC CICS READNEXT
061013        DATASET   (ERACCT2-DSID)
061013        SET       (ADDRESS OF ACCOUNT-MASTER)
061013        RIDFLD    (SAVE-ERACCT-KEY)
061013     END-EXEC
061013
061013     IF (ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1)
061013        or (ACCT-CARRIER  NOT = AM-VG-CARRIER )
061013        or (ACCT-GROUPING NOT = AM-VG-GROUPING)
061013        or (ACCT-STATE    NOT = AM-VG-STATE   )
061013        or (ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT )
061013        go to 6500-endbr
061013     END-IF
061013
061013     IF (WS-EFFDT >= AM-EFFECTIVE-DT)
061013        and (WS-EFFDT < AM-EXPIRATION-DT)
              set acct-found to true
061013        MOVE AM-dcc-product-code to ws-dcc-product-code
061013        GO TO 6500-endbr
061013     END-IF
061013                                                                  
061013     IF WS-EFFDT >= AM-EXPIRATION-DT                        
061013         GO TO 6500-readnext
061013     END-IF
061013
061013     .                                                                        
061013 6500-endbr.
061013     
061013     EXEC CICS ENDBR
061013         DATASET    (ERACCT2-DSID)
061013     END-EXEC
061013
061013     .
061013 6500-exit.
061013     exit.

05053  6600-CHECK-AUTO-ACTIVITY.                                        
05054                                                                   
           EXEC CICS HANDLE CONDITION
05056          NOTFND   (6600-NOT-FOUND)
05057      END-EXEC.
05058                                                                   
05059      MOVE PI-COMPANY-ID              TO  CNTL-COMP-ID.            
05060      MOVE 'T'                        TO  CNTL-REC-TYPE.           
05061      MOVE SPACES                     TO  CNTL-ACCESS.             
05062      MOVE +0                         TO  CNTL-SEQ-NO.             
05063                                                                   
05064      PERFORM 7970-READ-CNTL THRU 7970-EXIT.                       
05065                                                                   
05066      IF CF-SYS-ACTIVE-SW (1) = 'N' OR ' '                         
05067          MOVE 'N'                    TO  WS-REC-FOUND-SW          
05068                                          WS-LETTER-SW             
05069          GO TO 6600-EXIT
041002     END-IF.
05070                                                                   
05071      IF CF-SYS-LETTER-ID (1) = SPACES OR LOW-VALUES               
05072          MOVE 'N'                     TO  WS-LETTER-SW            
05073      ELSE                                                         
05074          MOVE 'Y'                     TO  WS-LETTER-SW
041002     END-IF.
05075                                                                   
05076      MOVE 'Y'                         TO  WS-REC-FOUND-SW.        
05077      GO TO 6600-EXIT.                                             
05078                                                                   
05079  6600-START-AUTO-LETTER-WRITER.                                   
05080                                                                   
05081      MOVE LOW-VALUES                  TO  W-1523-LINKDATA.        
05082      MOVE PROGRAM-INTERFACE-BLOCK     TO  W-1523-COMMON-PI-DATA.  
05083                                                                   
05084      MOVE CF-SYS-LETTER-ID (1)        TO  W-1523-FORM-NUMBER.     
05085                                                                   
05086      IF CF-SYS-RESEND-DAYS (1) NOT = ZEROS                        
05087          MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1           
05088          MOVE '6'                     TO  DC-OPTION-CODE          
05089          MOVE CF-SYS-RESEND-DAYS (1)  TO  DC-ELAPSED-DAYS         
05090          MOVE +0                      TO  DC-ELAPSED-MONTHS       
05091          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05092          IF NO-CONVERSION-ERROR                                   
05093              MOVE DC-BIN-DATE-2       TO  W-1523-RESEND-DATE      
05094          ELSE                                                     
05095              MOVE LOW-VALUES          TO  W-1523-RESEND-DATE
041002         END-IF
041002     END-IF.
05096                                                                   
05097      IF CF-SYS-FOLLOW-UP-DAYS (1) NOT = ZEROS                     
05098          MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1           
05099          MOVE '6'                     TO  DC-OPTION-CODE          
05100          MOVE CF-SYS-FOLLOW-UP-DAYS (1)   TO  DC-ELAPSED-DAYS     
05101          MOVE +0                      TO  DC-ELAPSED-MONTHS       
05102          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05103          IF NO-CONVERSION-ERROR                                   
05104              MOVE DC-BIN-DATE-2       TO  W-1523-FOLLOW-UP-DATE   
05105          ELSE                                                     
05106              MOVE LOW-VALUES          TO  W-1523-FOLLOW-UP-DATE
041002         END-IF
041002     END-IF.
05107                                                                   
05108      
           EXEC CICS LINK
05109          PROGRAM    (LINK-1523)
05110          COMMAREA   (W-1523-LINKDATA)
05111          LENGTH     (W-1523-COMM-LENGTH)
05112      END-EXEC.
05113                                                                   
05114      GO TO 6600-EXIT.                                             
05115                                                                   
05116  6600-NOT-FOUND.                                                  
05117      MOVE 'N'                         TO  WS-REC-FOUND-SW.        
05118                                                                   
05119  6600-EXIT.                                                       
05120      EXIT.                                                        
05121                                                                   
CIDMOD/                                                                 
CIDMOD*************************************************************     
CIDMOD*****        START OF ACTIVITY FILE OUTPUT PROCESSING      **     
CIDMOD*************************************************************     
CIDMOD                                                                  
CIDMOD 6700-OUTPUT-ACTIVITY-RECORD.                                     
CIDMOD                                                                  
CIDMOD     
           EXEC CICS GETMAIN
CIDMOD         SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD         LENGTH (DLYACTV-LENGTH)
CIDMOD         INITIMG (WS-BLANK)
CIDMOD     END-EXEC.
CIDMOD                                                                  
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.        
CIDMOD     MOVE PI-COMPANY-CD          TO DA-COMP-CD.                   
CIDMOD     MOVE CLMCARRI               TO DA-CARRIER.                   
CIDMOD     IF CLMNOL EQUAL ZERO                                         
CIDMOD         MOVE WS-CLAIM-NUMBER    TO DA-CLAIM-NO                   
CIDMOD     ELSE                                                         
CIDMOD         MOVE CLMNOI             TO DA-CLAIM-NO                   
CIDMOD     END-IF.
CIDMOD     MOVE CERTNOI                TO DA-CERT-PRIME.                
CIDMOD     MOVE SUFXI                  TO DA-CERT-SFX.                  
CIDMOD     MOVE +0                     TO DA-TRAILER-SEQ-NO.            
CIDMOD     MOVE 'A'                    TO DA-RECORD-TYPE.               
CIDMOD     
           EXEC CICS HANDLE CONDITION
CIDMOD         NOTOPEN (6700-NOTOPEN-ERROR)
CIDMOD         DUPREC (6700-EXIT)
CIDMOD     END-EXEC.
CIDMOD     
           EXEC CICS WRITE
CIDMOD         DATASET (DLYACTV-DSID)
CIDMOD         RIDFLD (DA-KEY)
CIDMOD         FROM (DAILY-ACTIVITY-RECORD)
CIDMOD         LENGTH (DLYACTV-LENGTH)
CIDMOD     END-EXEC.
CIDMOD     GO TO 6700-EXIT.                                             
CIDMOD                                                                  
CIDMOD 6700-NOTOPEN-ERROR.                                              
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                    
CIDMOD     MOVE -1                     TO MAINTL.                       
CIDMOD     MOVE AL-UANON               TO MAINTA.                       
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU                               
CIDMOD             9900-EXIT.                                           
CIDMOD     GO TO 8200-SEND-DATAONLY.                                    
CIDMOD                                                                  
CIDMOD 6700-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD*************************************************************     
CIDMOD*****         END OF ACTIVITY FILE OUTPUT PROCESSING        *     
CIDMOD*************************************************************     
CIDMOD/                                                                 
05123  6990-DEEDIT-DATE.                                                
05124      
           EXEC CICS BIF DEEDIT
05125          FIELD   (DATE-WORK)
05126          LENGTH  (8)
05127      END-EXEC.
05128  6990-EXIT.                                                       
05129      EXIT.                                                        
05130                                                                   
05131      EJECT                                                        
05132  7000-BUILD-OUTPUT-MAP.                                           
05133                                                                   
05134      IF CLAIM-SWITCH = 'N'                                        
05135          GO TO 7050-BUILD-MAP-CERT-DATA
041002     END-IF.
05136                                                                   
05137      IF CL-ASSOC-CERT-TOTAL = +0 OR +1                            
05138          MOVE SPACES                 TO SEQUO                     
05139          MOVE AL-SABOF               TO SEQUA                     
05140      ELSE                                                         
05141          MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU           
05142          MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU                
05143          MOVE WS-CLAIM-SEQU          TO SEQUO                     
05144          MOVE AL-SABON               TO SEQUA
041002     END-IF.
05145                                                                   
05146      MOVE CL-CARRIER             TO CLMCARRO.                     
05147      MOVE CL-CLAIM-TYPE          TO CLMTYPEO.                     
05148      MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.                     
05149      MOVE CL-PRIME-CERT-SFX      TO PSUFXO.                       
05150      MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.                      
05151      MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.                      
05152      MOVE CL-INSURED-MID-INIT    TO INITO.                        
05153      MOVE CL-INSURED-SEX-CD      TO SEXO.
           move cl-insured-type        to instypeo
05154 *    MOVE CL-INSURED-OCC-CD      TO OCCCDO.                       
05155                                                                   
05156      MOVE AL-UANON               TO CLMCARRA                      
05157                                     CLMTYPEA                      
05158                                     PCERTNOA                      
05159                                     PSUFXA                        
05160                                     LSTNMEA                       
05161                                     FSTNMEA                       
05162                                     INITA                         
05163                                     SEXA                          
05165                                                                   
05166      MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.           
05167                                                                   
05168      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES                      
05169          MOVE ' '                 TO DC-OPTION-CODE               
05170          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1                
05171          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05172          IF NOT DATE-CONVERSION-ERROR                             
05173              MOVE DC-GREG-DATE-1-EDIT TO BIRTHDTO                 
05174              MOVE AL-UANON            TO BIRTHDTA
041002         END-IF
041002     END-IF.
05175                                                                   
05176      MOVE CL-SOC-SEC-NO          TO SSNO.                         
05177      MOVE AL-UANON               TO SSNA.                         
05178                                                                   
05179      IF CL-INCURRED-DT NOT = LOW-VALUES                           
05180          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 
05181          MOVE ' '                TO DC-OPTION-CODE                
05182          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05183          IF NOT DATE-CONVERSION-ERROR                             
05184              MOVE DC-GREG-DATE-1-EDIT TO INCURO                   
05185              MOVE AL-UANON            TO INCURA
041002         END-IF
041002     END-IF.
05186                                                                   
05187      IF CL-REPORTED-DT NOT = LOW-VALUES                           
05188          MOVE ' '                TO DC-OPTION-CODE                
05189          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1                 
05190          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05191          IF NOT DATE-CONVERSION-ERROR                             
05192              MOVE DC-GREG-DATE-1-EDIT TO REPORTO                  
05193              MOVE AL-UANON            TO REPORTA
041002         END-IF
041002     END-IF.
05194
040814*    IF CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES                   
040814*        MOVE ' '                 TO DC-OPTION-CODE               
040814*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1             
040814*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
040814*        IF NOT DATE-CONVERSION-ERROR                             
040814*            MOVE DC-GREG-DATE-1-EDIT TO ESTENDO                  
040814*            MOVE AL-UANON            TO ESTENDA
040814*        END-IF
040814*    END-IF.
05202                                                                   
05203      MOVE WS-DIAGNOSIS-DESCRIPT  TO DIAGO.                        
040814     MOVE WS-ICD-CODE-1          TO ICD1O.
040814     MOVE WS-ICD-CODE-2          TO ICD2O.
040814*    MOVE CL-CAUSE-CD            TO CAUSEO.                       
05205      MOVE AT-CURRENT-MANUAL-RESERVE TO MANRSVO.                   
05206      MOVE CL-RELATED-CLAIM-NO    TO RELCLMO.                      
05207      MOVE CL-PROCESSOR-ID        TO PROCCDO.                      
05208      MOVE CL-PRIORITY-CD         TO PRICDO.                       
05209      MOVE AL-UANON               TO DIAGA                         
040814                                    ICD1A
040814                                    ICD2A
040814*                                   CAUSEA                        
05211                                     RELCLMA                       
05212                                     PROCCDA                       
05213                                     PRICDA.                       
05214      MOVE AL-UNNON               TO MANRSVA.                      
05215                                                                   
121802*    IF PI-COMPANY-ID  = 'ACC'  OR  'FDL' OR 'LGX'                
121802*        MOVE CL-PRODUCT-CD          TO PRODCDO                   
121802*        MOVE AL-UANON               TO PRODCDA
121802*    END-IF.
05219                                                                   
05220      MOVE CL-SUPV-ATTN-CD        TO SUPVO.                        
05221      MOVE CL-FILE-LOCATION       TO FILETOO.                      
05222      MOVE CL-BENEFICIARY         TO BENECDO.                      
05223      MOVE AL-UANON               TO SUPVA                         
05224                                     FILETOA                       
05225                                     BENECDA.                      
05226                                                                   
05227      IF CLAIM-SWITCH = 'X'                                        
05228          GO TO 7099-EXIT
041002     END-IF.
05229                                                                   
05230      EJECT                                                        
05231  7050-BUILD-MAP-CERT-DATA.                                        
05232      MOVE CM-CERT-PRIME          TO CERTNOO.                      
05233      MOVE CM-CERT-SFX            TO SUFXO.                        
PEMMOD     MOVE AL-SANON               TO CERTNOA                       
PEMMOD                                    SUFXA.                        
05236                                                                   
05237      IF CM-CERT-EFF-DT NOT = LOW-VALUES                           
05238          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1                 
05239          MOVE ' '                TO DC-OPTION-CODE                
05240          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05241          IF NOT DATE-CONVERSION-ERROR                             
05242              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
041002         END-IF
041002     END-IF.
05243                                                                   
05244      MOVE CM-ACCOUNT             TO ACCOUNTO.                     
05245      MOVE CM-STATE               TO STATEO.                       
05246      MOVE CM-CARRIER             TO CRTCARRO.                     
05247      MOVE CM-GROUPING            TO GROUPO.                       
05248      MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.                     
05249      MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.                     
05250      MOVE CM-INSURED-INITIAL2    TO CRTINITO.                     
05251      MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.                      
05252      MOVE CM-JT-LAST-NAME        TO JNTLNMEO.                     
05253      MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.                     
05254      MOVE CM-JT-INITIAL          TO JNTINITO.                     
05255      MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.                      
05256      MOVE AL-UANON               TO ACCOUNTA                      
05257                                     STATEA                        
05258                                     CRTCARRA                      
05259                                     GROUPA                        
05260                                     CRTLNMEA                      
05261                                     CRTFNMEA                      
05262                                     CRTINITA                      
05263                                     JNTLNMEA                      
05264                                     JNTFNMEA                      
05265                                     JNTINITA.                     
05266      MOVE AL-UNNON               TO ISSAGEA                       
05267                                     JNTAGEA.                      
05268                                                                   
05269      IF CM-SSN-STATE   = CM-STATE AND                             
05270         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME                         
041002*        NEXT SENTENCE
041002         CONTINUE
05272      ELSE                                                         
05273          MOVE CM-SOC-SEC-NO      TO CRTSSNO
041002     END-IF.
05274
05275      MOVE AL-UANON               TO CRTSSNA.                      
05276                                                                   
05277      MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.          
05278      MOVE '2'                    TO DC-OPTION-CODE.               
05279      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
05280      MOVE DC-BIN-DATE-1          TO SAVE-CURRENT-DATE.            
05281                                                                   
05282 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***             
05283      MOVE SPACES                 TO ELCNTL-KEY.                   
05284      MOVE CM-STATE               TO CNTL-ACCESS.                  
05285      MOVE '3'                    TO CNTL-REC-TYPE.                
05286      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
05287      MOVE ZEROS                  TO CNTL-SEQ-NO.                  
05288                                                                   
05289      
           EXEC CICS READ
05290          DATASET  (ELCNTL-DSID)
05291          SET      (ADDRESS OF CONTROL-FILE)
05292          RIDFLD   (ELCNTL-KEY)
05293          RESP     (WS-RESPONSE)
05294      END-EXEC.
05295                                                                   
05296      IF WS-RESP-NOTFND                                            
05297           MOVE ER-2848           TO EMI-ERROR                     
05298           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
05299           GO TO 8100-SEND-INITIAL-MAP
041002     END-IF.
05300                                                                   
05301      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                 
061511     MOVE CF-ST-VFY-2ND-BENE TO PI-ST-VFY-2ND-BENE.
020613     MOVE CF-ST-CAUSAL-STATE TO PI-ST-CAUSAL-STATE.
05302      EJECT                                                        
05303      IF CM-LF-BENEFIT-CD = '00'                                   
05304          MOVE ZEROS              TO LCVOTRMO                      
05305                                     LCVRTRMO                      
05306                                     LCVRATEO                      
05307                                     LCVBENEO                      
05308          MOVE SPACES             TO LCVDSCRO                      
05309                                     LCVCDO                        
05310                                     LCVKINDO                      
05311                                     LCVFORMO                      
05312                                     LCVCNDTO                      
05313                                     LCVEXITO                      
05314                                     LCVSTATO                      
05315          IF CLMTYPEL GREATER THAN ZERO AND                        
100518            CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
05317                 MOVE ER-0521     TO EMI-ERROR                     
05318                 MOVE AL-UABON    TO CLMTYPEA                      
05319                 MOVE -1          TO CLMTYPEL                      
05320                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          
05321                 GO TO 7060-FILL-AH-COVERAGE                       
05322          ELSE                                                     
05323              GO TO 7060-FILL-AH-COVERAGE
041002         END-IF
041002     END-IF.
05324                                                                   
05325      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.                     
05326      MOVE AL-UANON               TO LCVDSCRA.                     
05327      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-HOLD.                  
05328      MOVE '4'                    TO WS-REC-TYPE.                  
05329      PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.                    
05330      MOVE WS-BEN-ALPHA-HOLD      TO LCVKINDO.                     
05331      MOVE CM-LF-BENEFIT-CD       TO LCVCDO.                       
05332      MOVE AL-UANON               TO LCVCDA.                       
05333      MOVE CM-LF-ORIG-TERM        TO LCVOTRMO                      
05334                                     CP-ORIGINAL-TERM.             
05335      MOVE AL-UNNON               TO LCVOTRMA.                     
05336      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               
05337      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.            
05338      MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.              
05339      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.       
05340      MOVE '4'                    TO CP-REM-TERM-METHOD.           
05341      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                
05342                                                                   
05343      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                   
05344      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.                     
05345                                                                   
05346      IF CM-LF-PREMIUM-RATE NUMERIC                                
05347          MOVE CM-LF-PREMIUM-RATE TO LCVRATEO                      
05348      ELSE                                                         
05349          MOVE ZEROS              TO LCVRATEO
041002     END-IF.
05350                                                                   
05351      MOVE AL-UNNON               TO LCVRATEA.                     
05352                                                                   
05353      IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC                         
05354          MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT
041002     END-IF.
05355                                                                   
05356      COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.
05357                                                                   
05358      MOVE AL-UNNON               TO LCVBENEA.                     
05359                                                                   
05360      IF CM-POLICY-FORM-NO NOT = SPACES                            
05361          MOVE CM-POLICY-FORM-NO  TO LCVFORMO                      
05362      ELSE                                                         
05363          MOVE WS-FORM-HOLD       TO LCVFORMO
041002     END-IF.
05364                                                                   
05365      MOVE SPACES                 TO WS-FORM-HOLD.                 
05366      MOVE 'L'                    TO WS-REC-TYPE.                  
05367      PERFORM 7400-READ-STATE THRU 7499-EXIT.                      
05368                                                                   
05369      IF WS-FORM-HOLD NOT = SPACES                                 
05370          MOVE WS-FORM-HOLD       TO LCVFORMO                      
05371          MOVE AL-UANON           TO LCVFORMA
041002     END-IF.
05372                                                                   
05373      IF CM-LF-CURRENT-STATUS = '8'                                
05374          IF CM-LF-CANCEL-DT NOT = LOW-VALUES
05375              MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
05376              MOVE ' '             TO DC-OPTION-CODE
05377              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05378              IF NOT DATE-CONVERSION-ERROR
05379                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
05380                  MOVE AL-UANON            TO LCVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05381                                                                   
05382      IF CM-LF-CURRENT-STATUS = '7'                                
05383          IF CM-LF-DEATH-DT NOT = LOW-VALUES                       
05384              MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1                 
05385              MOVE ' '            TO DC-OPTION-CODE                
05386              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
05387              IF NOT DATE-CONVERSION-ERROR                         
05388                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO             
05389                  MOVE AL-UANON   TO LCVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05390                                                                   
05391      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES                      
05392          IF CM-LF-DEATH-EXIT-DT NOT = SPACES                      
05393              MOVE ' '            TO DC-OPTION-CODE                
05394              MOVE CM-LF-DEATH-EXIT-DT                             
05395                                  TO DC-BIN-DATE-1                 
05396              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
05397              IF NOT DATE-CONVERSION-ERROR                         
05398                  MOVE DC-GREG-DATE-1-EDIT                         
05399                                  TO LCVEXITO
041002             END-IF
041002         END-IF
041002     END-IF.
05400                                                                   
05401      IF CM-LF-CURRENT-STATUS = '1' OR = '4'                       
05402          IF CP-REMAINING-TERM-3 = ZEROS                           
05403              MOVE 'EXPIRED'      TO LCVSTATO                      
05404          ELSE                                                     
05405              MOVE 'ACTIVE'       TO LCVSTATO
041002         END-IF
041002     END-IF.
05406                                                                   
05407      IF CM-LF-CURRENT-STATUS = '2'                                
05408         MOVE 'PEND  '            TO LCVSTATO
041002     END-IF.

05409      IF CM-LF-CURRENT-STATUS = '3'                                
05410         MOVE 'RESTORE'           TO LCVSTATO
041002     END-IF.

05411      IF CM-LF-CURRENT-STATUS = '5'                                
05412         MOVE 'REISSUE'           TO LCVSTATO
041002     END-IF.

05413      IF CM-LF-CURRENT-STATUS = '6'                                
05414         MOVE 'LMP DIS'           TO LCVSTATO
041002     END-IF.

05415      IF CM-LF-CURRENT-STATUS = '7'                                
05416         MOVE 'DEATH  '           TO LCVSTATO
041002     END-IF.

05417      IF CM-LF-CURRENT-STATUS = '8'                                
05418         MOVE 'CANCEL '           TO LCVSTATO
041002     END-IF.

05419      IF CM-LF-CURRENT-STATUS = '9'                                
05420         MOVE 'RE-ONLY'           TO LCVSTATO
041002     END-IF.

05421      IF CM-LF-CURRENT-STATUS = 'V'                                
05422         MOVE 'VOID   '           TO LCVSTATO
041002     END-IF.

05423      IF CM-LF-CURRENT-STATUS = 'D'                                
05424         MOVE 'DECLINE'           TO LCVSTATO
041002     END-IF.
100809
100809     IF PI-COMPANY-ID = 'CID'
100809         IF CM-LF-BENEFIT-CD >= '2O' AND 
100809            CM-LF-BENEFIT-CD <= '2V'
100809             MOVE ER-0878       TO EMI-ERROR                     
100809             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100809         END-IF
100809     END-IF.

05425                                                                   
05426      EJECT                                                        
05427  7060-FILL-AH-COVERAGE.                                           
05428                                                                   
05429      IF CM-AH-BENEFIT-CD = '00'                                   
05430          MOVE ZEROS              TO ACVOTRMO                      
05431                                     ACVRTRMO                      
05432                                     ACVRATEO                      
05433                                     ACVBENEO                      
05434          MOVE SPACES             TO ACVDSCRO                      
05435                                     ACVCDO                        
05436                                     ACVKINDO                      
05437                                     ACVFORMO                      
05438                                     ACVCNDTO                      
05439                                     ACVEXITO                      
05440                                     ACVSTATO                      
05441          IF (CLMTYPEL GREATER THAN ZERO AND                        
052614             (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122              OR 'B' OR 'H'))
05443              MOVE ER-0521     TO EMI-ERROR
05444              MOVE AL-UABON    TO CLMTYPEA
05445              MOVE -1          TO CLMTYPEL
05446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05447              GO TO 7070-BUILD-MAP-FINISH
05448          ELSE                                                     
05449              GO TO 7070-BUILD-MAP-FINISH
041002         END-IF
041002     END-IF.
05450                                                                   
05451      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.                     
05452      MOVE CM-AH-BENEFIT-CD       TO ACVCDO WS-BEN-HOLD.           
05453      MOVE AL-UANON               TO ACVDSCRA                      
05454                                     ACVCDA.                       
05455      MOVE '5'                    TO WS-REC-TYPE.                  
05456      PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.                    
05457      MOVE WS-BEN-ALPHA-HOLD      TO ACVKINDO.                     
05458                                                                   
05459      MOVE CM-AH-ORIG-TERM        TO ACVOTRMO                      
05460                                     CP-ORIGINAL-TERM.             
05461      MOVE AL-UNNON               TO ACVOTRMA.                     
05462                                                                   
05463      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               
05464      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.            
05465      MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.              
05466      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.       
05467      MOVE '4'                    TO CP-REM-TERM-METHOD.           
05468      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                
05469                                                                   
05470      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                   
05471      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.                     
05472                                                                   
05473      IF CM-AH-PREMIUM-RATE NUMERIC                                
05474          MOVE CM-AH-PREMIUM-RATE TO ACVRATEO                      
05475      ELSE                                                         
05476          MOVE ZEROS              TO ACVRATEO
041002     END-IF.
05477                                                                   
05478      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO.                     
05479      MOVE AL-UNNON               TO ACVRATEA                      
05480                                     ACVBENEA.                     
05481                                                                   
05482      IF CM-POLICY-FORM-NO NOT = SPACES                            
05483          MOVE CM-POLICY-FORM-NO  TO ACVFORMO                      
05484      ELSE                                                         
05485          MOVE WS-FORM-HOLD       TO ACVFORMO                      
05486          MOVE SPACES TO WS-FORM-HOLD                              
05487          MOVE 'A'                TO WS-REC-TYPE                   
05488          PERFORM 7400-READ-STATE THRU 7499-EXIT                   
05489          IF WS-FORM-HOLD NOT = SPACES                             
05490              MOVE WS-FORM-HOLD   TO ACVFORMO
041002         END-IF
041002     END-IF.
05491                                                                   
05492      MOVE AL-UANON               TO ACVFORMA.                     
05493                                                                   
05494      IF CM-AH-CURRENT-STATUS = '8'                                
05495          IF CM-AH-CANCEL-DT NOT = LOW-VALUES
05496              MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
05497              MOVE ' '             TO DC-OPTION-CODE
05498              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05499              IF NOT DATE-CONVERSION-ERROR
05500                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO              
05501                 MOVE AL-UANON    TO ACVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05502                                                                   
05503      IF CM-AH-CURRENT-STATUS = '6' OR '7'                         
05504          IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
05505              MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
05506              MOVE ' '             TO DC-OPTION-CODE
05507              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05508              IF NOT DATE-CONVERSION-ERROR
05509                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO              
05510                 MOVE AL-UANON            TO ACVCNDTA
041002             END-IF
041002         END-IF
041002     END-IF.
05511                                                                   
05512      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES                 
05513          MOVE ' '                TO DC-OPTION-CODE                
05514          MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1           
05515          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
05516          IF NOT DATE-CONVERSION-ERROR                             
05517              MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO
041002         END-IF
041002     END-IF.
05518                                                                   
05519      IF CM-AH-CURRENT-STATUS = '1' OR = '4'                       
05520          IF CP-REMAINING-TERM-3 = ZEROS                           
05521              MOVE 'EXPIRED'      TO ACVSTATO                      
05522          ELSE                                                     
05523              MOVE 'ACTIVE'       TO ACVSTATO
041002         END-IF
041002     END-IF.
05524                                                                   
05525      IF CM-AH-CURRENT-STATUS = '2'                                
05526          MOVE 'PEND  '           TO ACVSTATO
041002     END-IF.

05527      IF CM-AH-CURRENT-STATUS = '3'                                
05528          MOVE 'RESTORE'          TO ACVSTATO
041002     END-IF.

05529      IF CM-AH-CURRENT-STATUS = '5'                                
05530          MOVE 'REISSUE'          TO ACVSTATO
041002     END-IF.

05531      IF CM-AH-CURRENT-STATUS = '6'                                
05532          MOVE 'LMP DIS'          TO ACVSTATO
041002     END-IF.

05533      IF CM-AH-CURRENT-STATUS = '7'                                
05534          MOVE 'DEATH  '          TO ACVSTATO
041002     END-IF.

05535      IF CM-AH-CURRENT-STATUS = '8'                                
05536          MOVE 'CANCEL '          TO ACVSTATO
041002     END-IF.

05537      IF CM-AH-CURRENT-STATUS = '9'                                
05538          MOVE 'RE-ONLY'          TO ACVSTATO
041002     END-IF.

05539      IF CM-AH-CURRENT-STATUS = 'V'                                
05540          MOVE 'VOID   '          TO ACVSTATO
041002     END-IF.

05541      IF CM-AH-CURRENT-STATUS = 'D'                                
05542          MOVE 'DECLINE'          TO ACVSTATO
041002     END-IF.
041309
020816     IF PI-COMPANY-ID = 'DCC' OR 'VPP'
041309        IF CM-ACCOUNT (9:2) = 'BI'
041309           MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
041309           MOVE CM-CARRIER       TO CTRLR-CARRIER
041309           MOVE CM-GROUPING      TO CTRLR-GROUPING
041309           MOVE CM-STATE         TO CTRLR-STATE 
041309           MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
041309           MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
041309           MOVE CM-CERT-NO       TO CTRLR-CERT-NO
041309           MOVE 'C'              TO CTRLR-REC-TYPE
041309           EXEC CICS READ
041309             DATASET  (ELCRTT-DSID)
041309             SET      (ADDRESS OF CERTIFICATE-TRAILERS)
041309             RIDFLD   (ELCRTT-KEY)
041309             RESP     (WS-RESPONSE)
041309           END-EXEC
041309           IF WS-RESP-NORMAL
041309               MOVE CS-VIN-NUMBER   TO WS-DIAG-VIN
041309               MOVE WS-DIAG-VIN-MSG TO DIAGO
041309               MOVE AL-UANON        TO DIAGA
041309           END-IF
041309        END-IF
041309     END-IF.
041309
05544      EJECT                                                        
05545  7070-BUILD-MAP-FINISH.                                           
05546      MOVE CM-LOAN-APR            TO APRO.                         
05547      MOVE CM-PAY-FREQUENCY       TO PMTFREQO.                     
05548      MOVE CM-IND-GRP-TYPE        TO INDGRPO.                      
05549      MOVE CM-PREMIUM-TYPE        TO PREMTYPO.
05550      MOVE AL-UNNON               TO APRA
05551                                     PMTFREQA.                     
05552      MOVE AL-UANON               TO INDGRPA                       
05553                                     PREMTYPA.                     
05554                                                                   
05555      IF (CM-SPECIAL-REIN-CODE NOT = SPACES AND LOW-VALUES)        
05556          MOVE CM-SPECIAL-REIN-CODE   TO REINCDO                   
05557          MOVE AL-UANON               TO REINCDA
041002     END-IF.
05558                                                                   
05559      IF CM-LAST-ADD-ON-DT = SPACES OR LOW-VALUES                  
041002*        NEXT SENTENCE
041002         CONTINUE
05561      ELSE                                                         
05562          MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1
05563          MOVE ' '                TO DC-OPTION-CODE
05564          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
05565          IF NOT DATE-CONVERSION-ERROR
05566              MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO
05567              MOVE AL-UANON            TO ADDONDTA
041002         END-IF
041002     END-IF.

05568                                                                   
05569      IF CLAIM-SWITCH = 'N'                                        
05570          IF SEXL = ZEROS                                          
05571              MOVE CM-INSURED-SEX TO SEXO                          
020513*            MOVE AL-UANON       TO SEXA
041002         END-IF
041002     END-IF.

05573                                                                   
05574      IF CM-MEMB-ACCOUNT NOT = CM-ACCOUNT-PRIME                    
05575          MOVE CM-MEMBER-NO       TO MEMBERO
05576          MOVE AL-UANON           TO MEMBERA
041002     END-IF.
05577                                                                   
05578      IF MAINTI = 'A'                                              
05579          IF (LOANNOL = ZEROS OR                                   
05580              EIBAID = DFHPF5)                                     
05581              MOVE CM-LOAN-NUMBER TO LOANNOO                       
05582              MOVE AL-UANON       TO LOANNOA                       
05583          ELSE                                                     
041002*            NEXT SENTENCE
041002             CONTINUE
041002         END-IF
05585      ELSE                                                         
05586          MOVE CM-LOAN-NUMBER     TO LOANNOO                       
05587          MOVE AL-UANON           TO LOANNOA
041002     END-IF.
05588                                                                   
05589      IF MAINTI = 'A'                                              
05590          IF (LOANBALL = ZEROS OR                                  
05591              EIBAID = DFHPF5)                                     
05592              MOVE CM-LOAN-BALANCE TO LOANBALO                     
05593              MOVE AL-UNNON       TO LOANBALA                      
05594          ELSE                                                     
041002*            NEXT SENTENCE
041002             CONTINUE
041002         END-IF
05596      ELSE                                                         
05597          MOVE CM-LOAN-BALANCE    TO LOANBALO                      
05598          MOVE AL-UNNON           TO LOANBALA
041002     END-IF.
05599
05600      IF CM-PREMIUM-TYPE NOT = '3'                                 
05601          IF NOT CERT-WAS-CREATED-FOR-CLAIM                        
05602              PERFORM 7200-PROTECT-CERT-FIELDS                     
05603          ELSE                                                     
05604              PERFORM 7300-RESET-KEY-ATTRBS
041002         END-IF
05605      ELSE                                                         
05606          PERFORM 7300-RESET-KEY-ATTRBS
041002     END-IF.
05607
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'         
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE                                                         
05611          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.
121802*    END-IF.
05612                                                                   
05613  7099-EXIT.                                                       
05614      EXIT.                                                        
05615                                                                   
05616      EJECT                                                        
05617  7100-READ-BENEFIT.                                               
05618      
           EXEC CICS HANDLE CONDITION
05619           NOTFND   (7120-NOT-FOUND)
05620      END-EXEC.
05621                                                                   
05622      MOVE SPACES                 TO ELCNTL-KEY.                   
05623      MOVE WS-BEN-HOLD            TO CNTL-BENEFIT.                 
05624      MOVE WS-REC-TYPE            TO CNTL-REC-TYPE.                
05625      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
05626      MOVE ZEROS                  TO CNTL-SEQ-NO.                  
05627                                                                   
05628  7105-READ-FILE.                                                  
05629      PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT.                  
05630                                                                   
05631      IF PI-COMPANY-ID NOT = CF-COMPANY-ID  OR                     
05632         WS-REC-TYPE   NOT = CF-RECORD-TYPE                        
05633           GO TO 7120-NOT-FOUND
041002     END-IF.
05634                                                                   
05635      MOVE 1                      TO SUB.                          
05636  7110-LOOP.                                                       
05637      IF SUB = 9                                                   
05638          GO TO 7120-NOT-FOUND
041002     END-IF.
05639                                                                   
05640      IF WS-BEN-HOLD NOT = CF-BENEFIT-CODE (SUB)                   
05641          ADD 1                   TO SUB                           
05642          GO TO 7110-LOOP
041002     END-IF.
05643                                                                   
           if ws-rec-type = '4'
              move cf-joint-indicator (sub)
                                       to ws-lf-joint-indicator
           else
              move cf-joint-indicator (sub)
                                       to ws-ah-joint-indicator
           end-if
05644      MOVE CF-BENEFIT-ALPHA (SUB)    TO WS-BEN-ALPHA-HOLD.         
05645      MOVE CF-CO-EARNINGS-CALC (SUB) TO WS-EARNINGS-CALC.          
05646      MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.        
05647                                                                   
05648      GO TO 7199-EXIT.                                             
05649                                                                   
05650  7120-NOT-FOUND.                                                  
05651      MOVE SPACES                 TO WS-BEN-ALPHA-HOLD             
05652                                     WS-FORM-HOLD.                 
05653                                                                   
05654  7199-EXIT.                                                       
05655       EXIT.                                                       
05656                                                                   
05657      EJECT                                                        
05658  7200-PROTECT-CERT-FIELDS.                                        
05659      MOVE AL-SANON           TO CERTMTA EFFDTA ACCOUNTA STATEA    
05660                                 CRTCARRA GROUPA CRTLNMEA CRTFNMEA 
05661                                 LCVFORMA ACVFORMA JNTLNMEA        
05662                                 JNTFNMEA JNTINITA.                
05663                                                                   
05664      IF CM-LF-CURRENT-STATUS = '2' OR                             
05665         CM-AH-CURRENT-STATUS = '2'                                
05666          MOVE AL-SANON           TO LOANNOA LOANBALA PREMTYPA
041002     END-IF.
05667                                                                   
05668      MOVE AL-SANOF TO                                             
05669             CRTINITA   ISSAGEA    JNTAGEA    CRTSSNA   ADDONDTA   
05670             LCVDSCRA   LCVCDA     LCVOTRMA   LCVBENEA  LCVCNDTA   
05671             ACVDSCRA   ACVCDA     ACVOTRMA   ACVBENEA  ACVCNDTA   
05672             APRA       PMTFREQA   INDGRPA    MEMBERA   REINCDA    
05673             LCVRATEA   ACVRATEA.                                  
05674                                                                   
05675      MOVE 'S'                    TO CERTMTI.                      
05676                                                                   
05677  7300-RESET-KEY-ATTRBS.                                           
05678      MOVE AL-UANON           TO CERTMTA EFFDTA ACCOUNTA STATEA    
05679                                 CRTCARRA GROUPA CRTLNMEA CRTFNMEA 
05680                                 LCVFORMA ACVFORMA JNTLNMEA        
05681                                 JNTFNMEA JNTINITA.                
05682                                                                   
05683      IF NOT CERT-WAS-CREATED-FOR-CLAIM                            
05684          MOVE AL-SANOF           TO REINCDA
041002     END-IF.
05685                                                                   
05686      MOVE 'S'                    TO CERTMTI.                      
05687                                                                   
05688      EJECT                                                        
05689  7400-READ-STATE.                                                 
05690      
           EXEC CICS HANDLE CONDITION
05691           NOTFND   (7499-EXIT)
05692      END-EXEC.
05693                                                                   
05694      MOVE SPACES                 TO ELCNTL-KEY.                   
05695      MOVE CM-STATE               TO CNTL-ACCESS.                  
05696      MOVE '3'                    TO CNTL-REC-TYPE.                
05697      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
05698      MOVE ZEROS                  TO CNTL-SEQ-NO.                  
05699                                                                   
05700      PERFORM 7970-READ-CNTL THRU 7970-EXIT.                       
05701                                                                   
05702      MOVE 1                      TO SUB.                          
05703                                                                   
05704  7410-LOOP.                                                       
05705      IF SUB = 21                                                  
05706         GO TO 7499-EXIT
041002     END-IF.
05707                                                                   
05708 *    IF (WS-BEN-HOLD  = CF-ST-BENEFIT-CD (SUB))  AND              
05709 *       (WS-REC-TYPE  = CF-ST-BENEFIT-KIND (SUB))                 
05710 *        MOVE CF-ST-FORM-NO (SUB) TO WS-FORM-HOLD                 
05711 *        GO TO 7499-EXIT.                                         
05712                                                                   
05713      ADD 1                       TO SUB.                          
05714      GO TO 7410-LOOP.                                             
05715                                                                   
05716  7499-EXIT.                                                       
05717       EXIT.                                                       
05718                                                                   
05719      EJECT                                                        
05720  7500-BROWSE-FOR-DUPLICATE.                                       
05721                                                                   
05722      MOVE 'N'                    TO INCUR-DTE-DUPE-SW.            
05723                                                                   
05724      
           EXEC CICS HANDLE CONDITION
05725          NOTFND   (7500-EXIT)
05726          DUPKEY   (7500-DUPLICATE-KEY)
05727          ENDFILE  (7500-EXIT)
05728      END-EXEC.
05729                                                                   
05730      
           EXEC CICS STARTBR
05731          DATASET    (ELMSTR5-DSID)
05732          RIDFLD     (ELMSTR5-KEY)
05733      END-EXEC.
05734                                                                   
05735      
           EXEC CICS HANDLE CONDITION
05736          ENDFILE  (7500-END-BROWSE)
05737      END-EXEC.
05738                                                                   
05739  7500-READ-CLAIM-LOOP.                                            
05740                                                                   
05741      
           EXEC CICS READNEXT
05742          DATASET   (ELMSTR5-DSID)
05743          SET       (ADDRESS OF CLAIM-MASTER)
05744          RIDFLD    (ELMSTR5-KEY)
05745      END-EXEC.
05746                                                                   
05747  7500-DUPLICATE-KEY.                                              
05748                                                                   
05749      IF CL-COMPANY-CD     NOT = CM-COMPANY-CD  OR                 
05750         CL-CERT-CARRIER   NOT = CM-CARRIER     OR                 
05751         CL-CERT-GROUPING  NOT = CM-GROUPING    OR                 
05752         CL-CERT-STATE     NOT = CM-STATE       OR                 
05753         CL-CERT-ACCOUNT   NOT = CM-ACCOUNT     OR                 
05754         CL-CERT-EFF-DT    NOT = CM-CERT-EFF-DT OR                 
05755         CL-CERT-NO        NOT = CM-CERT-NO                        
05756            GO TO 7500-CHECK-MATCH-COUNT
041002     END-IF.
05757                                                                   
05758      MOVE 'H'                    TO INCUR-DTE-DUPE-SW.            
05759                                                                   
05760      IF CL-INCURRED-DT = PI-SAVE-INCUR-DT                         
05761         MOVE 'Y'                 TO INCUR-DTE-DUPE-SW             
05762         GO TO 7500-END-BROWSE                                     
05763      ELSE                                                         
05764         GO TO 7500-READ-CLAIM-LOOP
041002     END-IF.
05765                                                                   
05766  7500-CHECK-MATCH-COUNT.                                          
05767                                                                   
05768      IF CL-COMPANY-CD = CM-COMPANY-CD AND                         
05769         CL-CERT-NO    = CM-CERT-NO                                
05770           GO TO 7500-READ-CLAIM-LOOP
041002     END-IF.
05771                                                                   
05772  7500-END-BROWSE.                                                 
05773                                                                   
05774      
           EXEC CICS ENDBR
05775          DATASET   (ELMSTR5-DSID)
05776      END-EXEC.
05777                                                                   
05778  7500-EXIT.                                                       
05779      EXIT.                                                        
05780                                                                   
05781      EJECT                                                        
05782  7550-FALL-THRU-PARA.                                             
05783                                                                   
05784      
           EXEC CICS DUMP
05785           DUMPCODE ('E130')
05786           TASK
05787      END-EXEC.
05788                                                                   
05789      
           EXEC CICS RETURN
05790      END-EXEC.
05791                                                                   
05792  7600-BROWSE-CLAIM.                                               
05793                                                                   
05794      
           EXEC CICS READ
05795          DATASET(ELMSTR-DSID)
05796          SET    (ADDRESS OF CLAIM-MASTER)
05797          RIDFLD (ELMSTR-KEY)
05798          GENERIC
05799          EQUAL
05800          KEYLENGTH(ELMSTR-GENERIC-LENGTH)
05801      END-EXEC.
05802                                                                   
05803      MOVE CL-CERT-PRIME          TO CERTNOI.                      
05804      MOVE CL-CERT-SFX            TO SUFXI.                        
05805                                                                   
05806  7610-BROWSE-CLAIM-LOOP.                                          
05807                                                                   
05808      MOVE LOW-VALUES             TO BCERT1O                       
05809                                     BSUFX1O                       
05810                                     BCERT2O                       
05811                                     BSUFX2O                       
05812                                     BCERT3O                       
05813                                     BSUFX3O                       
05814                                     BCERT4O                       
05815                                     BSUFX4O                       
05816                                     BCERT5O                       
05817                                     BSUFX5O.                      
05818                                                                   
05819      MOVE ELMSTR-KEY             TO SAVE-ELMSTR-KEY.              
05820                                                                   
05821      
           EXEC CICS HANDLE CONDITION
05822          ENDFILE  (7630-END-BROWSE)
05823      END-EXEC.
05824                                                                   
05825      
           EXEC CICS STARTBR
05826          DATASET    (ELMSTR-DSID)
05827          RIDFLD     (ELMSTR-KEY)
05828      END-EXEC.
05829                                                                   
05830      MOVE +1                     TO WS-ASSOCIATED-CERTS.          
05831                                                                   
05832  7620-READ-CLAIM-LOOP.                                            
05833                                                                   
05834      
           EXEC CICS READNEXT
05835          DATASET   (ELMSTR-DSID)
05836          SET       (ADDRESS OF CLAIM-MASTER)
05837          RIDFLD    (ELMSTR-KEY)
05838      END-EXEC.
05839                                                                   
05840      IF CL-COMPANY-CD  NOT = PI-COMPANY-CD  OR                    
05841         CL-CARRIER     NOT = CLMCARRI       OR                    
05842         CL-CLAIM-NO    NOT = CLMNOI                               
05843           GO TO 7630-END-BROWSE
041002     END-IF.
05844                                                                   
05845      IF WS-ASSOCIATED-CERTS = 1                                   
05846          MOVE CL-CERT-PRIME      TO BCERT1O                       
05847          MOVE CL-CERT-SFX        TO BSUFX1O
041002     END-IF.
05848                                                                   
05849      IF WS-ASSOCIATED-CERTS = 2                                   
05850          MOVE CL-CERT-PRIME      TO BCERT2O                       
05851          MOVE CL-CERT-SFX        TO BSUFX2O
041002     END-IF.
05852                                                                   
05853      IF WS-ASSOCIATED-CERTS = 3                                   
05854          MOVE CL-CERT-PRIME      TO BCERT3O                       
05855          MOVE CL-CERT-SFX        TO BSUFX3O
041002     END-IF.
05856                                                                   
05857      IF WS-ASSOCIATED-CERTS = 4                                   
05858          MOVE CL-CERT-PRIME      TO BCERT4O                       
05859          MOVE CL-CERT-SFX        TO BSUFX4O
041002     END-IF.
05860                                                                   
05861      IF WS-ASSOCIATED-CERTS = 5                                   
05862          MOVE CL-CERT-PRIME      TO BCERT5O                       
05863          MOVE CL-CERT-SFX        TO BSUFX5O                       
05864      ELSE                                                         
05865          ADD +1                  TO WS-ASSOCIATED-CERTS           
05866          GO TO 7620-READ-CLAIM-LOOP
041002     END-IF.
05867                                                                   
05868  7630-END-BROWSE.                                                 
05869                                                                   
05870      
           EXEC CICS ENDBR
05871          DATASET   (ELMSTR-DSID)
05872      END-EXEC.
05873                                                                   
05874  7640-HIGHLIGHT-CERT-DISPLAYED.                                   
05875                                                                   
05876      MOVE SAVE-ELMSTR-KEY        TO ELMSTR-KEY.                   
05877      MOVE AL-SANON               TO BCERT1A                       
05878                                     BSUFX1A                       
05879                                     BCERT2A                       
05880                                     BSUFX2A                       
05881                                     BCERT3A                       
05882                                     BSUFX3A                       
05883                                     BCERT4A                       
05884                                     BSUFX4A                       
05885                                     BCERT5A                       
05886                                     BSUFX5A.                      
05887                                                                   
05888      IF BCERT1O = CERTNOI AND                                     
05889         BSUFX1O = SUFXI                                           
05890             MOVE AL-SABON        TO BCERT1A                       
05891                                     BSUFX1A
041002     END-IF.
05892                                                                   
05893      IF BCERT2O = CERTNOI AND                                     
05894         BSUFX2O = SUFXI                                           
05895             MOVE AL-SABON        TO BCERT2A                       
05896                                     BSUFX2A
041002     END-IF.
05897                                                                   
05898      IF BCERT3O = CERTNOI AND                                     
05899         BSUFX3O = SUFXI                                           
05900             MOVE AL-SABON        TO BCERT3A                       
05901                                     BSUFX3A
041002     END-IF.
05902                                                                   
05903      IF BCERT4O = CERTNOI AND                                     
05904         BSUFX4O = SUFXI                                           
05905             MOVE AL-SABON        TO BCERT4A                       
05906                                     BSUFX4A
041002     END-IF.
05907                                                                   
05908      IF BCERT5O = CERTNOI AND                                     
05909         BSUFX5O = SUFXI                                           
05910             MOVE AL-SABON        TO BCERT5A                       
05911                                     BSUFX5A
041002     END-IF.
05912                                                                   
05913  7699-EXIT.                                                       
05914      EXIT.                                                        
05915                                                                   
05916      EJECT                                                        
05917  7700-CHECK-SEQUENCE.                                             
05918                                                                   
05919      
           EXEC CICS HANDLE CONDITION
05920          ENDFILE  (7799-EXIT)
05921          NOTFND   (7799-EXIT)
05922      END-EXEC.
05923                                                                   
05924      
           EXEC CICS READ
05925          DATASET(ELMSTR-DSID)
05926          SET    (ADDRESS OF CLAIM-MASTER)
05927          RIDFLD (ELMSTR-KEY)
05928          GENERIC
05929          EQUAL
05930          KEYLENGTH(ELMSTR-GENERIC-LENGTH)
05931      END-EXEC.
05932                                                                   
05933      COMPUTE WS-ASSOC-CERT-TOTAL =                                
05934              CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.                   
05935                                                                   
05936      GO TO 7799-EXIT.                                             
05937                                                                   
05938  7710-RESEQUENCE-CLAIMS.                                          
05939                                                                   
05940      
           EXEC CICS HANDLE CONDITION
05941          ENDFILE  (7790-END-BROWSE)
05942      END-EXEC.
05943                                                                   
05944      
           EXEC CICS STARTBR
05945          DATASET    (ELMSTR-DSID)
05946          RIDFLD     (ELMSTR-KEY)
05947      END-EXEC.
05948                                                                   
05949      ADD +1 TO WS-ASSOC-CERT-SEQU                                 
05950                WS-READNEXT-SWITCH.                                
05951                                                                   
05952  7720-READ-CLAIM-LOOP.                                            
05953                                                                   
05954      
           EXEC CICS READNEXT
05955          DATASET   (ELMSTR-DSID)
05956          SET       (ADDRESS OF CLAIM-MASTER)
05957          RIDFLD    (ELMSTR-KEY)
05958      END-EXEC.
05959                                                                   
05960      IF WS-READNEXT-SWITCH = 1                                    
05961          ADD 1 TO WS-READNEXT-SWITCH                              
05962          GO TO 7720-READ-CLAIM-LOOP
041002     END-IF.
05963                                                                   
05964  7730-END-BROWSE.                                                 
05965                                                                   
05966      
           EXEC CICS ENDBR
05967          DATASET   (ELMSTR-DSID)
05968      END-EXEC.
05969                                                                   
05970  7740-READ-CLAIM-UPDATE.                                          
05971                                                                   
05972      IF CL-COMPANY-CD NOT = WS-SAVE-COMPANY-CD OR                 
05973         CL-CARRIER    NOT = WS-SAVE-CARRIER    OR                 
05974         CL-CLAIM-NO   NOT = WS-SAVE-CLAIM-NO                      
05975           GO TO 7799-EXIT                                         
05976      ELSE
05977           MOVE ZERO              TO WS-READNEXT-SWITCH
041002     END-IF.
05978                                                                   
05979      
           EXEC CICS READ
05980          DATASET(ELMSTR-DSID)
05981          SET    (ADDRESS OF CLAIM-MASTER)
05982          RIDFLD (ELMSTR-KEY)
05983          UPDATE
05984      END-EXEC.
05985                                                                   
05986      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.          
05987      MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.           
05988                                                                   
05989      
           EXEC CICS REWRITE
05990           DATASET     (ELMSTR-DSID)
05991           FROM        (CLAIM-MASTER)
05992      END-EXEC.
05993                                                                   
05994      GO TO 7710-RESEQUENCE-CLAIMS.                                
05995                                                                   
05996  7790-END-BROWSE.                                                 
05997                                                                   
05998      
           EXEC CICS ENDBR
05999          DATASET   (ELMSTR-DSID)
06000      END-EXEC.
06001                                                                   
06002  7799-EXIT.                                                       
06003      EXIT.                                                        
06004                                                                   
06005      EJECT                                                        
06006 ************************************************                  
06007 *  I/O REQUESTS AGAINST ACTIVITY TRAILER FILE  *                  
06008 ************************************************                  
06009                                                                   
06010  7900-READ-ACTV-UPDATE.                                           
06011      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.                 
06012      MOVE MSTR-CARRIER           TO TRLR-CARRIER.                 
06013      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.                
06014      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.                 
06015      MOVE +0                     TO TRLR-SEQ-NO.                  
06016      MOVE '1'                    TO TRLR-TYPE.                    
06017      GO TO 7900-READ-TRAILER-UPDATE.                              
06018                                                                   
06019  7900-READ-NINETY-UPDATE.                                         
06020      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.                 
06021      MOVE MSTR-CARRIER           TO TRLR-CARRIER.                 
06022      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.                
06023      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.                 
06024      MOVE +90                    TO TRLR-SEQ-NO.                  
06025      MOVE '6'                    TO TRLR-TYPE.                    
06026                                                                   
06027  7900-READ-TRAILER-UPDATE.                                        
06028      
           EXEC CICS READ  UPDATE
06029          DATASET    (ELTRLR-DSID)
06030          SET        (ADDRESS OF ACTIVITY-TRAILERS)
06031          RIDFLD     (ELTRLR-KEY)
06032      END-EXEC.
06033                                                                   
06034  7900-EXIT.                                                       
06035       EXIT.                                                       
06036                                                                   
06037  7910-READ-ACTV.                                                  
06038      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.                 
06039      MOVE MSTR-CARRIER           TO TRLR-CARRIER.                 
06040      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.                
06041      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.                 
06042      MOVE +0                     TO TRLR-SEQ-NO.                  
06043      MOVE '1'                    TO TRLR-TYPE.                    
06044      GO TO 7910-READ-TRAILER.                                     
06045                                                                   
06046  7910-READ-NINETY.                                                
06047      MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.                 
06048      MOVE MSTR-CARRIER           TO TRLR-CARRIER.                 
06049      MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.                
06050      MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.                 
06051      MOVE +90                    TO TRLR-SEQ-NO.                  
06052      MOVE '6'                    TO TRLR-TYPE.                    
06053                                                                   
06054  7910-READ-TRAILER.                                               
06055      MOVE 'TRLR'                 TO FILE-SWITCH.                  
06056                                                                   
06057      
           EXEC CICS READ
06058          DATASET   (ELTRLR-DSID)
06059          SET       (ADDRESS OF ACTIVITY-TRAILERS)
06060          RIDFLD    (ELTRLR-KEY)
06061      END-EXEC.
06062                                                                   
06063  7910-EXIT.                                                       
06064       EXIT.                                                       
06065                                                                   
06066  7915-REWRITE-TRAILER.                                            
06067      
           EXEC CICS REWRITE
06068          DATASET   (ELTRLR-DSID)
06069          FROM      (ACTIVITY-TRAILERS)
06070      END-EXEC.
06071                                                                   
06072  7915-EXIT.                                                       
06073       EXIT.                                                       
06074                                                                   
06075      EJECT                                                        
06076 ********************************************                      
06077 *  I/O REQUESTS AGAINST CLAIM MASTER FILE  *                      
06078 ********************************************                      
06079                                                                   
06080  7920-READ-CLAIM-UPDATE.                                          
06081                                                                   
06082      
           EXEC CICS READ
06083          UPDATE
06084          DATASET   (ELMSTR-DSID)
06085          SET       (ADDRESS OF CLAIM-MASTER)
06086          RIDFLD    (ELMSTR-KEY)
06087      END-EXEC.
06088                                                                   
06089  7920-EXIT.                                                       
06090       EXIT.                                                       
06091                                                                   
06154  7930-READ-CERT-UPDATE.                                           
06155      
           EXEC CICS READ
06156          DATASET  (ELCERT-DSID)
06157          SET      (ADDRESS OF CERTIFICATE-MASTER)
06158          RIDFLD   (ELCERT-KEY)
06159          UPDATE
06160      END-EXEC.
06161                                                                   
06162  7930-EXIT.                                                       
06163       EXIT.                                                       
06164                                                                   
06165  7940-READ-CERT.                                                  
06166      
           EXEC CICS READ
06167          DATASET  (ELCERT-DSID)
06168          SET      (ADDRESS OF CERTIFICATE-MASTER)
06169          RIDFLD   (ELCERT-KEY)
06170      END-EXEC.
06171                                                                   
06172  7940-EXIT.                                                       
06173       EXIT.                                                       

06092  7950-READ-CLAIM.                                                 
06093      
           EXEC CICS READ
06094          DATASET   (ELMSTR-DSID)
06095          SET       (ADDRESS OF CLAIM-MASTER)
06096          RIDFLD    (ELMSTR-KEY)
06097      END-EXEC.
06098                                                                   
06099  7950-EXIT.                                                       
06100       EXIT.                                                       
06101                                                                   
06102      EJECT                                                        
06103 ***************************************                           
06104 *  I/O REQUESTS AGAINST CONTROL FILE  *                           
06105 ***************************************                           
06106                                                                   
06107  7960-READ-CNTL-UPDATE.                                           
06108      
           EXEC CICS READ
06109          UPDATE
06110          DATASET  (ELCNTL-DSID)
06111          SET      (ADDRESS OF CONTROL-FILE)
06112          RIDFLD   (ELCNTL-KEY)
06113      END-EXEC.
06114                                                                   
06115  7960-EXIT.                                                       
06116       EXIT.                                                       
06117                                                                   
06118  7970-READ-CNTL.                                                  
06119      
           EXEC CICS READ
06120          DATASET  (ELCNTL-DSID)
06121          SET      (ADDRESS OF CONTROL-FILE)
06122          RIDFLD   (ELCNTL-KEY)
06123      END-EXEC.
06124                                                                   
06125  7970-EXIT.                                                       
06126       EXIT.                                                       
06127                                                                   
06128  7975-READ-CNTL-GTEQ.                                             
06129      
           EXEC CICS READ
06130           DATASET   (ELCNTL-DSID)
06131           SET       (ADDRESS OF CONTROL-FILE)
06132           RIDFLD    (ELCNTL-KEY)
06133           GTEQ
06134       END-EXEC.
06135                                                                   
06136  7975-EXIT.                                                       
06137       EXIT.                                                       
06138                                                                   
06139      EJECT                                                        
06140 **************************************************                
06141 *  I/O REQUESTS AGAINST CERTIFICATE MASTER FILE  *                
06142 **************************************************                
06143                                                                   
06144  7980-READ-CERT5.                                                 
06145       
            EXEC CICS READ
06146           DATASET  (ELCERT5-DSID)
06147           SET      (ADDRESS OF CERTIFICATE-MASTER)
06148           RIDFLD   (ELCERT-KEY-5)
06149       END-EXEC.
06150                                                                   
06151  7980-EXIT.                                                       
06152       EXIT.                                                       
06153                                                                   
090821 7990-get-lo-hi-acct-dates.

090821     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUPING
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-EXP-DT
090821     MOVE ERACCT-KEY          TO SAVE-ERACCT-KEY
090821
090821     move spaces              to ws-i-say-stop-ind
090821                                 ws-eracct-startbr-ind
090821                                 ws-acct-status
090821
090821     EXEC CICS STARTBR
090821          DATASET    ('ERACCT')
090821          RIDFLD     (ERACCT-KEY)
090821          GTEQ
090821          resp       (ws-response)
090821     END-EXEC

090821     if ws-resp-normal
090821        set eracct-browse-started to true
090821     end-if
090821
090821     perform until i-say-stop
090821        EXEC CICS READNEXT
090821           DATASET ('ERACCT')
090821           RIDFLD  (ERACCT-KEY)
090821           SET     (ADDRESS OF ACCOUNT-MASTER)
090821           resp    (WS-RESPONSE)
090821        END-EXEC
090821
090821        IF WS-RESP-NORMAL
090821           AND save-eracct-key(1:20) =
090821                       AM-CONTROL-PRIMARY (1:20)
090821           if ws-lo-acct-dt = low-values
090821              move am-effective-dt
090821                                 to ws-lo-acct-dt
090821           end-if
090821           if am-expiration-dt > ws-hi-acct-dt
090821              move am-expiration-dt
090821                                 to ws-hi-acct-dt
090821           end-if
090821           move am-status        to ws-acct-status
090821        else
090821           set i-say-stop to true
090821        end-if
090821     end-perform
090821
090821     if eracct-browse-started
090821        exec cics endbr
090821           dataset('ERACCT')
090821        end-exec
090821     end-if
090821
090821     .
090821 7990-exit.
090821     exit.

06176 **************************************************                
06177 *  I/O REQUESTS AGAINST BENEFICIARY MASTER FILE  *                
06178 **************************************************                
06179                                                                   
06180  7991-READ-BENE.                                                  
06181      
           EXEC CICS READ
06182           DATASET   (ELBENE-DSID)
06183           SET       (ADDRESS OF BENEFICIARY-MASTER)
06184           RIDFLD    (ELBENE-KEY)
06185       END-EXEC.
06186                                                                   
06187  7991-EXIT.                                                       
06188       EXIT.                                                       
06189                                                                   
06190      EJECT                                                        
06191 **************************************************                
06192 *  I/O REQUESTS AGAINST REINSURANCE MASTER FILE  *                
06193 **************************************************                
06194                                                                   
06195  7992-READ-REIN.                                                  
06196                                                                   
06197      
           EXEC CICS HANDLE CONDITION
06198          NOTFND   (7992-NOT-FOUND)
06199      END-EXEC.
06200                                                                   
06201      
           EXEC CICS READ
06202          DATASET   (ERREIN-DSID)
06203          SET       (ADDRESS OF REINSURANCE-RECORD)
06204          RIDFLD    (ERREIN-KEY)
06205      END-EXEC.
06206                                                                   
06207      MOVE 'Y'                       TO  WS-REIN-REC-FOUND-SW.     
06208                                                                   
06209      GO TO 7992-EXIT.                                             
06210                                                                   
06211  7992-NOT-FOUND.                                                  
06212      MOVE 'N'                       TO  WS-REIN-REC-FOUND-SW.     
06213                                                                   
06214  7992-EXIT.                                                       
06215      EXIT.                                                        
06216                                                                   
06217      EJECT                                                        
06218  8000-LOAD-ERROR-MESSAGES.                                        
06219      MOVE SPACES                 TO ERRMSG1O  ERRMSG2O.           
06220                                                                   
06221      IF EMI-NO-ERRORS                                             
06222          GO TO 8000-EXIT
041002     END-IF.
06223                                                                   
061013     perform varying e1 from +1 by +1 until e1 > +3
061013        if emi-error-number (e1) = '1651' or '1652' or '1653'
061013                                or '1655' or '1660'
061013           move 'W'              to emi-severity (e1)
061013        end-if
061013     end-perform

06224      IF EMI-NUMBER-OF-LINES = 1                                   
06225          MOVE EMI-LINE1          TO ERRMSG1O                      
06226          GO TO 8000-EXIT
041002     END-IF.
06227                                                                   
06228      IF EMI-NUMBER-OF-LINES = 2                                   
06229           MOVE EMI-LINE1         TO ERRMSG1O                      
06230           MOVE EMI-LINE2         TO ERRMSG2O                      
06231           GO TO 8000-EXIT
041002     END-IF.
06232                                                                   
06233      MOVE EMI-LINE1              TO ERRMSG1O.                     
06234                                                                   
06235  8000-EXIT.                                                       
06236      EXIT.                                                        
06237                                                                   
06238  8100-SEND-INITIAL-MAP.                                           
06239      MOVE SAVE-DATE              TO RUNDTEO.                      
06240      MOVE EIBTIME                TO TIME-IN.                      
06241      MOVE TIME-OUT               TO RUNTIMEO.                     
           IF EIBAID = DFHPF11                               
              MOVE 'Y'                 TO EMI-ROLL-SWITCH    
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
           end-if

PEMMOD     IF RETURNED-FROM = XCTL-114
PEMMOD        CONTINUE
PEMMOD     ELSE
PEMMOD        MOVE -1                  TO MAINTL                        
PEMMOD     END-IF.
06243                                                                   
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'             
06245          MOVE SPACES             TO PRODO.                         
06246          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06247                                                                   
06248      MOVE PI-COMPANY-ID          TO COMPO.                        
06249      MOVE AL-PABOF               TO COMPA.                        
06250      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.                      
06251      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.                     
06252      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.                     
06253                                                                   
06254      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             
06255                                                                   
06256      IF NOT PI-NO-CARRIER-SECURITY                                
06257         MOVE PI-CARRIER-SECURITY TO CLMCARRO                      
06258         MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06259                                                                   
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.

121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'         
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE                                                         
06263          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06264                                                                   
06265      
           EXEC CICS SEND
06266          MAP     (MAP-NAME)
06267          MAPSET  (MAPSET-NAME)
06268          FROM    (EL130AO)
06269          ERASE
06270          CURSOR
06271      END-EXEC.
06272                                                                   
06273      GO TO 9100-RETURN-TRAN.                                      
06274                                                                   
06275  8150-SEND-MAP-CURSOR.                                            
06276      MOVE SAVE-DATE              TO RUNDTEO.                      
06277      MOVE EIBTIME                TO TIME-IN.                      
06278      MOVE TIME-OUT               TO RUNTIMEO.                     
           IF EIBAID = DFHPF11                               
              MOVE 'Y'                 TO EMI-ROLL-SWITCH    
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
           end-if

06279      MOVE PI-COMPANY-ID          TO COMPO.                        
06280      MOVE AL-PABOF               TO COMPA                         
06281      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.                      
06282      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.                     
06283      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.                     
06284                                                                   
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'             
06286          MOVE SPACES             TO PRODO.                         
06287          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06288                                                                   
06289      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             
06290                                                                   
06291      IF NOT PI-NO-CARRIER-SECURITY                                
06292          MOVE PI-CARRIER-SECURITY TO CLMCARRO                     
06293          MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06294                                                                   
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.

121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'         
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE                                                         
06298          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06299                                                                   
06300      
           EXEC CICS SEND
06301          MAP      (MAP-NAME)
06302          MAPSET   (MAPSET-NAME)
06303          FROM     (EL130AO)
06304          CURSOR   (PI-CURSOR)
06305          ERASE
06306      END-EXEC.
06307                                                                   
06308      GO TO 9100-RETURN-TRAN.                                      
06309                                                                   
06310  8200-SEND-DATAONLY.                                              
061013     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             
06316                                                                   
           IF EIBAID = DFHPF11                               
              MOVE 'Y'                 TO EMI-ROLL-SWITCH    
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              go to 8100-send-initial-map
           end-if

06311 *    IF EIBAID = DFHPF11                                          
06312 *       MOVE 'Y'                 TO EMI-ROLL-SWITCH               
061013*       move pi-last-claim       to emi-claim-no
06313 *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013*       go to 8100-send-initial-map
041002*    END-IF.
           .
06317  8200-BYPASS-ERROR.                                               
06318      MOVE SAVE-DATE              TO RUNDTEO.                      
06319      MOVE EIBTIME                TO TIME-IN.                      
06320      MOVE TIME-OUT               TO RUNTIMEO.                     
06321      MOVE PI-COMPANY-ID          TO COMPO.                        
06322      MOVE AL-PABOF               TO COMPA                         
06323      MOVE PI-MEMBER-CAPTION      TO MEMCAPO.                      
06324                                                                   
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'             
06326          MOVE SPACES             TO PRODO.                        
06327          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06328                                                                   
06329      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.                     
06330      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.                     
06331                                                                   
PEMMOD     IF PI-PROCESSOR-ID = 'PEMA'
PEMMOD        MOVE AL-UANON            TO CERTNOA
PEMMOD                                    SUFXA
PEMMOD     END-IF.

06332      IF NOT PI-NO-CARRIER-SECURITY                                
06333         MOVE PI-CARRIER-SECURITY TO CLMCARRO                      
06334         MOVE AL-SABOF            TO CLMCARRA
041002     END-IF.
06335                                                                   
121802*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'         
041002*        NEXT SENTENCE
121802*        CONTINUE
121802*    ELSE                                                         
06339          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
121802*    END-IF.
06340                                                                   
121802*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL'                       
06342          MOVE AL-SANOF           TO PRODCDA.
121802*    END-IF.
06343                                                                   
06344      
           EXEC CICS SEND
06345          MAP      (MAP-NAME)
06346          MAPSET   (MAPSET-NAME)
06347          FROM     (EL130AO)
06348          DATAONLY
06349          CURSOR
06350      END-EXEC.
06351                                                                   
06352      GO TO 9100-RETURN-TRAN.                                      
06353                                                                   
06354  8300-SEND-TEXT.                                                  
06355      
           EXEC CICS SEND TEXT
06356          FROM     (LOGOFF-TEXT)
06357          LENGTH   (LOGOFF-LENGTH)
06358          ERASE
06359          FREEKB
06360      END-EXEC.
06361                                                                   
06362      
           EXEC CICS RETURN
06363      END-EXEC.
06364                                                                   
06365  8800-UNAUTHORIZED-ACCESS.                                        
06366      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
06367      GO TO 8300-SEND-TEXT.                                        
06368                                                                   
06369  8810-PF23.                                                       
06370      MOVE EIBAID TO PI-ENTRY-CD-1.                                
06371      MOVE XCTL-005               TO PGM-NAME.                     
06372      GO TO 9300-XCTL.                                             
06373                                                                   
06374  8820-TERM-ERROR.                                                 
06375      MOVE ER-0412                TO EMI-ERROR.                    
06376      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06377      MOVE -1                     TO MAINTL.                       
06378      GO TO 8200-SEND-DATAONLY.                                    
06379                                                                   
06380  8830-TRAN-ERROR.                                                 
06381      MOVE ER-0413                TO EMI-ERROR.                    
06382      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
06383      MOVE -1                     TO MAINTL.                       
06384      GO TO 8200-SEND-DATAONLY.                                    
06385                                                                   
06386  9000-RETURN-CICS.                                                
06387      
           EXEC CICS RETURN
06388      END-EXEC.
06389                                                                   
06390  9100-RETURN-TRAN.                                                
06391      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
06392      MOVE '130A'                 TO PI-CURRENT-SCREEN-NO.         
06393                                                                   
06394      
           EXEC CICS RETURN
06395          TRANSID   (TRANS-ID)
06396          COMMAREA  (PROGRAM-INTERFACE-BLOCK)
06397          LENGTH    (PI-COMM-LENGTH)
06398      END-EXEC.
06399                                                                   
06400  9200-RETURN-MAIN-MENU.                                           
06401      MOVE XCTL-126               TO PGM-NAME.                     
06402      GO TO 9300-XCTL.                                             
06403                                                                   
06404  9300-XCTL.                                                       
06405      
           EXEC CICS XCTL
06406          PROGRAM   (PGM-NAME)
06407          COMMAREA  (PROGRAM-INTERFACE-BLOCK)
06408          LENGTH    (PI-COMM-LENGTH)
06409      END-EXEC.
06410                                                                   
06411  9400-CLEAR.                                                      
06412      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
06413      GO TO 9300-XCTL.                                             
06414                                                                   
06415  9500-PF12.                                                       
06416      MOVE XCTL-010               TO PGM-NAME.                     
06417      GO TO 9300-XCTL.                                             
06418                                                                   
06419  9600-PGMID-ERROR.                                                
06420      
           EXEC CICS HANDLE CONDITION
06421          PGMIDERR  (8300-SEND-TEXT)
06422      END-EXEC.
06423                                                                   
06424      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
06425      MOVE ' '                    TO PI-ENTRY-CD-1.                
06426      MOVE XCTL-005               TO PGM-NAME.                     
06427      MOVE PGM-NAME               TO LOGOFF-PGM.                   
06428      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
06429      GO TO 9300-XCTL.                                             
06430                                                                   
06431  9700-LINK-DATE-CONVERT.                                          
06432      MOVE LINK-ELDATCV           TO PGM-NAME.                     
06433      
           EXEC CICS LINK
06434          PROGRAM    (PGM-NAME)
06435          COMMAREA   (DATE-CONVERSION-DATA)
06436          LENGTH     (DC-COMM-LENGTH)
06437      END-EXEC.
06438                                                                   
06439  9700-EXIT.                                                       
06440      EXIT.                                                        
06441                                                                   
06442  9800-LINK-REM-TERM.                                              
06443      MOVE LINK-ELRTRM            TO PGM-NAME.                     
06444      
           EXEC CICS LINK
06445          PROGRAM    (PGM-NAME)
06446          COMMAREA   (CALCULATION-PASS-AREA)
06447          LENGTH     (CP-COMM-LENGTH)
06448      END-EXEC.
06449                                                                   
06450  9800-EXIT.                                                       
06451      EXIT.                                                        
06452                                                                   
06453  9900-ERROR-FORMAT.                                               
      *    if emi-error = 1651 or 1652 or 1653 or 1655
      *       display ' 9900 error ' emi-error
      *       display emi-message-area (1)
      *       display emi-message-area (2)
      *       display emi-message-area (3)
      *    end-if

06454      IF NOT EMI-ERRORS-COMPLETE                                   
06455          MOVE LINK-001           TO PGM-NAME                      
               EXEC CICS LINK
06457              PROGRAM   (PGM-NAME)
06458              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
06459              LENGTH    (EMI-COMM-LENGTH)
06460          END-EXEC
061013         if emi-error = 1651 or 1652 or 1653 or 1655 or 1660
061013            subtract 1 from emi-fatal-ctr
061013         end-if
041002     END-IF.
06461                                                                   
           .
06462  9900-EXIT.                                                       
06463      EXIT.                                                        
06464                                                                   
06465  9990-ABEND.                                                      
06466      MOVE -1                     TO MAINTL.                       
06467      MOVE LINK-004               TO PGM-NAME.                     
06468      MOVE DFHEIBLK               TO EMI-LINE1.                    
06469      
           EXEC CICS LINK
06470          PROGRAM   (PGM-NAME)
06471          COMMAREA  (EMI-LINE1)
06472          LENGTH    (72)
06473      END-EXEC.
06474                                                                   
06475      MOVE EMI-LINE1              TO ERRMSG2O.                     
06476                                                                   
06477      GO TO 8200-BYPASS-ERROR.                                     
06478                                                                   
06479                                                                   
06480      GOBACK.
06481                                                                   
06482  9995-SECURITY-VIOLATION.                                         
06483               COPY ELCSCTP.
06484                                                                   
06485  9995-EXIT.                                                       
06486      EXIT.                                                        
06487                                                                   
