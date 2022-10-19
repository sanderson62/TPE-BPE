00001  ID DIVISION.                                                     
00002                                                                   
00003  PROGRAM-ID.                 EL106.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/12/96 09:41:00.                 
00007 *                            VMOD=2.026.                          
00008 *                                                                 
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
00023 *REMARKS.    TRANSACTION- EX10 - STATE MAINTENANCE                
100108******************************************************************
100108*                   C H A N G E   L O G
100108*
100108* Changes are marked by the Change Effective date.
100108*-----------------------------------------------------------------
100108*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100108* EFFECTIVE    NUMBER
100108*-----------------------------------------------------------------
100108* 100108    2008022800002  AJRA  ADD CHECK NUMBER TO STATE FOR AK
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH CLAIM
011211* 011211    2010030900001  AJRA  ADD OPTION 4 AND 5 TO REFUND IND
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
031512* 031512    2011120900003  AJRA  ADD AHL COMPANY CODE
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
102717* 102717  CR2017062000003  PEMA  MOVE COM CAP STUFF TO EL1062
100108******************************************************************
00024                                                                   
00025      EJECT                                                        
00026  ENVIRONMENT DIVISION.                                            
00027  DATA DIVISION.                                                   
00028  WORKING-STORAGE SECTION.                                         
00029  77  FILLER  PIC X(32)  VALUE '********************************'. 
00030  77  FILLER  PIC X(32)  VALUE '*    EL106 WORKING STORAGE     *'. 
00031  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.026 ***********'. 
00032                                                                   
00033                                  COPY ELCSCTM.
00034                                  COPY ELCSCRTY.
092308                                 COPY MPCSCRT.
00035                                                                   
00036  01  WS-DATE-AREA.                                                
00037      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00038      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
00039                                                                   
00040  01  WS.                                                          
092308     05  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +29.
092308     05  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
00041      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1500.          
00042      05  WS-MAP-LENGTH       PIC S9(4) COMP VALUE +500.           
00043      05  RETURNED-FROM       PIC X(8)    VALUE SPACES.            
00044      05  QID.                                                     
00045          16  QID-TERM        PIC X(4).                            
00046          16  FILLER          PIC X(4)    VALUE '106A'.            
00047                                                                   
00048      05  WS-ST-TOL-PREM-PCT  PIC S9V9(4)    VALUE +0.             
00049      05  WS-ST-TOL-REF-PCT   PIC S9V9(4)    VALUE +0.             
00050      05  WS-ST-OVR-SHT-PCT   PIC S9V9(4)    VALUE +0.             
PEMMOD     05  WS-ST-LF-PREM-TAX   PIC S9V9(4)    VALUE +0.             
PEMMOD     05  WS-ST-AH-PREM-TAX-I PIC S9V9(4)    VALUE +0.             
PEMMOD     05  WS-ST-AH-PREM-TAX-G PIC S9V9(4)    VALUE +0.             
00051      05  WS-ST-LF-EXP-PCT    PIC S9(3)V9(4) VALUE +0.             
00052      05  WS-ST-AH-EXP-PCT    PIC S9(3)V9(4) VALUE +0.             
00053      05  WS-ST-TARGET-LOSS-RATIO                                  
00054                              PIC S9V9(4)    VALUE +0.             
00055      05  WS-ST-CALC-INTEREST PIC S9V9(4)    VALUE +0.             
00060      05  WS-RESIDENT-TAX     PIC S9V9(4)    VALUE +0.             
00061      05  WS-IRATE            PIC S9V9(4)    VALUE +0.             
00062      05  WS-IRATE1           PIC S9V9(4)    VALUE +0.             
00063      05  WS-IRATE2           PIC S9V9(4)    VALUE +0.             
00064      05  WS-IRATE3           PIC S9V9(4)    VALUE +0.             
00065      05  WS-FREE-LOOK-DAYS   PIC S9(3)      VALUE +0.             
00066                                                                   
00067  01  STANDARD-AREAS.                                              
00068      12  MAP-NAME            PIC X(8)    VALUE 'EL106A'.          
00069      12  MAPSET-NAME         PIC X(8)    VALUE 'EL106S'.          
00070      12  TRANS-ID            PIC X(4)    VALUE 'EX10'.            
00071      12  PGM-NAME            PIC X(8).                            
00072      12  TIME-IN             PIC S9(7).                           
00073      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00074          16  FILLER          PIC X.                               
00075          16  TIME-OUT        PIC 99V99.                           
00076          16  FILLER          PIC XX.                              
00077      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           
00078      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           
00079      12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.           
00080      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           
00081      12  XCTL-EL6565         PIC X(8)    VALUE 'EL6565'.          
00082      12  XCTL-EL6507         PIC X(8)    VALUE 'EL6507'.          
00083      12  XCTL-EL1061         PIC X(8)    VALUE 'EL1061'.          
102717     12  XCTL-EL1062         PIC X(8)    VALUE 'EL1062'.
00084      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.           
00085      12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.           
00086      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
00087      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
00088      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
00089      12  THIS-PGM            PIC X(8)    VALUE 'EL106'.           
00090      12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.          
00091      12  ELLETR-ID           PIC X(8)    VALUE 'ELLETR'.          
00092      12  SUB                 PIC S9(4)   COMP.                    
00093      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
00094      12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.               
00095          88  FIRST-TIME                  VALUE 'Y'.               
00096      12  WS-DISPLAY-SW       PIC X       VALUE 'N'.               
00097          88  RETURN-DISPLAY              VALUE 'Y'.               
00098                                                                   
00099  01  ACCESS-KEYS.                                                 
00100      12  ELCNTL-KEY.                                              
00101          16  CK-COMP-ID      PIC X(3).                            
00102          16  FILLER          PIC X       VALUE '3'.               
00103          16  CK-STATE-CD     PIC X(4)    VALUE SPACES.            
00104          16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.        
00105      12  W-WORKING-TEXT-KEY.                                      
00106          16  W-TEXT-COMPANY-CD                                    
00107                              PIC X.                               
00108          16  W-TEXT-FORM-NO  PIC X(12).                           
00109          16  W-TEXT-LINE-SEQ PIC S9(4)   COMP.                    
00110                                                                   
00111  01  ERROR-MESSAGES.                                              
00112      12  ER-0000                 PIC X(4)  VALUE '0000'.          
00113      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00114      12  ER-0013                 PIC X(4)  VALUE '0013'.          
00115      12  ER-0023                 PIC X(4)  VALUE '0023'.          
00116      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00117      12  ER-0042                 PIC X(4)  VALUE '0042'.          
00118      12  ER-0050                 PIC X(4)  VALUE '0050'.          
00119      12  ER-0068                 PIC X(4)  VALUE '0068'.          
00120      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00121      12  ER-0141                 PIC X(4)  VALUE '0141'.          
00122      12  ER-0144                 PIC X(4)  VALUE '0144'.          
00123      12  ER-0145                 PIC X(4)  VALUE '0145'.          
00124      12  ER-0146                 PIC X(4)  VALUE '0146'.          
00125      12  ER-0147                 PIC X(4)  VALUE '0147'.          
00126      12  ER-0148                 PIC X(4)  VALUE '0148'.          
00127      12  ER-0149                 PIC X(4)  VALUE '0149'.          
00128      12  ER-0150                 PIC X(4)  VALUE '0150'.          
00129      12  ER-0151                 PIC X(4)  VALUE '0151'.          
00130      12  ER-0152                 PIC X(4)  VALUE '0152'.          
00131      12  ER-0153                 PIC X(4)  VALUE '0153'.          
00132      12  ER-0159                 PIC X(4)  VALUE '0159'.          
00133      12  ER-0160                 PIC X(4)  VALUE '0160'.          
00134      12  ER-0161                 PIC X(4)  VALUE '0161'.          
CIDMOD     12  ER-0582                 PIC X(4)  VALUE '0582'.
00135      12  ER-0805                 PIC X(4)  VALUE '0805'.          
00136      12  ER-1614                 PIC X(4)  VALUE '1614'.
102717     12  er-1964                 pic x(4)  value '1964'.
00137      12  ER-2009                 PIC X(4)  VALUE '2009'.          
00138      12  ER-2010                 PIC X(4)  VALUE '2010'.          
00139      12  ER-2012                 PIC X(4)  VALUE '2012'.          
00140      12  ER-2014                 PIC X(4)  VALUE '2014'.          
00141      12  ER-2024                 PIC X(4)  VALUE '2024'.          
00142      12  ER-2028                 PIC X(4)  VALUE '2028'.          
00143      12  ER-2032                 PIC X(4)  VALUE '2032'.          
00144      12  ER-2033                 PIC X(4)  VALUE '2033'.          
PEMMOD     12  ER-2082                 PIC X(4)  VALUE '2082'.          
PEMMOD     12  ER-2084                 PIC X(4)  VALUE '2084'.          
00145      12  ER-2137                 PIC X(4)  VALUE '2137'.          
00146      12  ER-2298                 PIC X(4)  VALUE '2298'.          
00147      12  ER-2299                 PIC X(4)  VALUE '2299'.          
00148      12  ER-3030                 PIC X(4)  VALUE '3030'.          
00149      12  ER-3031                 PIC X(4)  VALUE '3031'.          
00150      12  ER-3032                 PIC X(4)  VALUE '3032'.          
00151      12  ER-3033                 PIC X(4)  VALUE '3033'.          
00152      12  ER-3034                 PIC X(4)  VALUE '3034'.          
00153      12  ER-3035                 PIC X(4)  VALUE '3035'.          
011410     12  ER-3036                 PIC X(4)  VALUE '3036'.
061511     12  ER-3040                 PIC X(4)  VALUE '3040'.
102717     12  er-3064                 pic x(4)  value '3064'.
00154      12  ER-7008                 PIC X(4)  VALUE '7008'.          
00155      12  ER-7346                 PIC X(4)  VALUE '7346'.          
00156      12  ER-7531                 PIC X(4)  VALUE '7531'.          
00157      12  ER-7532                 PIC X(4)  VALUE '7532'.          
00158      12  ER-7536                 PIC X(4)  VALUE '7536'. 
012913     12  ER-7578                 PIC X(4)  VALUE '7578'.         
032514     12  ER-7581                 PIC X(4)  VALUE '7581'.
00159      12  ER-7717                 PIC X(4)  VALUE '7717'.          
00160      12  ER-7735                 PIC X(4)  VALUE '7735'.          
00161      12  ER-8159                 PIC X(4)  VALUE '8159'.          
00162      12  ER-9074                 PIC X(4)  VALUE '9074'.          
092308     12  ER-9097                 PIC X(4)  VALUE '9097'.
00163      12  ER-9447                 PIC X(4)  VALUE '9447'.          
00164      12  ER-9448                 PIC X(4)  VALUE '9448'.          
00165      12  ER-9478                 PIC X(4)  VALUE '9478'.          
PEMMOD     12  ER-9999                 PIC X(4)  VALUE '9999'.          
00166      EJECT                                                        
00167      COPY ELCDATE.                                                
00168      EJECT                                                        
00169      COPY ELCLOGOF.                                               
00170      EJECT                                                        
00171      COPY ELCATTR.                                                
00172      EJECT                                                        
00173      COPY ELCEMIB.                                                
00174      EJECT                                                        
00175      COPY ELCJPFX.                                                
00176              PIC X(530).                                          
00177      EJECT                                                        
00178      COPY ELCINTF.                                                
00179      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
00180          16  FILLER              PIC X(101).                      
00181          16  PI-WS-STATE         PIC XX.                          
00182          16  PI-WS-CLASS         PIC XX.                          
00183          16  PI-WS-DEV           PIC XXX.                         
00184          16  PI-WS-TYPE          PIC X.                           
00185          16  PI-WS-PLAN          PIC XX.                          
00186          16  PI-PREV-STATE       PIC X(4).                        
00187          16  FILLER              PIC X(525).                      
00188                                                                   
00189      EJECT                                                        
00190      COPY ELCAID.                                                 
00191  01  FILLER REDEFINES DFHAID.                                     
00192      12  FILLER              PIC X(8).                            
00193      12  PF-VALUES           PIC X       OCCURS 2.                
00194                                                                   
00195      EJECT                                                        
00196      COPY EL106S.                                                 
00197      EJECT                                                        
00198  LINKAGE SECTION.                                                 
00199  01  DFHCOMMAREA             PIC X(1500).                         
00200 *01 PARMLIST .                                                    
00201 *    02  FILLER              PIC S9(8)   COMP.                    
00202 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
00203 *    02  ELLETR-POINTER      PIC S9(8)   COMP.                    
00204      EJECT                                                        
00205      COPY ELCCNTL.                                                
00206      EJECT                                                        
00207      COPY ELCTEXT.                                                
00208      EJECT                                                        
00209  PROCEDURE DIVISION.                                              
00210                                                                   
00211      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00212      MOVE '5'                   TO DC-OPTION-CODE.                
00213      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
00214      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
00215      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
00216                                                                   
00217      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.       
00218      MOVE 2                     TO EMI-NUMBER-OF-LINES            
00219                                    EMI-SWITCH2.                   
00220                                                                   
00221      MOVE EIBTRMID              TO QID-TERM.                      
00222                                                                   
00223      IF EIBCALEN = 0                                              
00224          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00225                                                                   
00226      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           
00227          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM                 
00228        ELSE                                                       
00229          MOVE SPACES             TO RETURNED-FROM.                
00230                                                                   
00231      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00232          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00233              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00234              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00235              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00236              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00237              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00238              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00239              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00240              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
092308             PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00241          ELSE                                                     
00242              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00243              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00244              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00245              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00246              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00247              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00248              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00249              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
00250                                                                   
00251      MOVE 'N' TO WS-DISPLAY-SW.                                   
00252                                                                   
00253      EXEC CICS HANDLE CONDITION                                   
00254          DUPREC  (8850-DUPREC)                                    
00255          NOTOPEN (8870-NOTOPEN)                                   
00256          NOTFND  (8880-NOT-FOUND)                                 
00257          PGMIDERR(9600-PGMID-ERROR)                               
00258          ERROR   (9990-ABEND)                                     
00259      END-EXEC.                                                    
00260                                                                   
00261      IF EIBTRNID NOT = TRANS-ID                                   
00262        IF RETURNED-FROM = XCTL-EL6565                             
00263            MOVE LOW-VALUES           TO EL106AO                   
00264            PERFORM 6500-RECOVER-TEMP-STORAGE THRU 6500-EXIT       
00265            GO TO 8100-SEND-INITIAL-MAP                            
00266         ELSE                                                      
102717       IF RETURNED-FROM = XCTL-EL1061 or XCTL-EL1062
00268            MOVE LOW-VALUES           TO EL106AO                   
00269            IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES              
00270                GO TO 8100-SEND-INITIAL-MAP                        
00271            ELSE                                                   
00272                MOVE PI-WS-STATE TO STCDI                          
00273                MOVE 'Y' TO WS-DISPLAY-SW                          
00274                GO TO 1000-SHOW-STATE                              
00275         ELSE                                                      
00276        IF PI-RETURN-TO-PROGRAM = XCTL-EL6507                      
00277            MOVE LOW-VALUES           TO EL106AO                   
00278            IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES              
00279                GO TO 8100-SEND-INITIAL-MAP                        
00280            ELSE                                                   
00281                MOVE PI-WS-STATE TO STCDI                          
00282                MOVE 'Y' TO WS-DISPLAY-SW                          
00283                GO TO 1000-SHOW-STATE                              
00284         ELSE                                                      
00285          MOVE LOW-VALUES           TO EL106AO                     
00286          GO TO 8100-SEND-INITIAL-MAP.                             
00287                                                                   
00288      IF EIBAID = DFHCLEAR                                         
00289          GO TO 9400-CLEAR.                                        
00290                                                                   
00291      IF NOT DISPLAY-CAP                                    
00292          MOVE 'READ'         TO SM-READ                           
00293          PERFORM 9995-SECURITY-VIOLATION                          
00294          MOVE ER-0070        TO EMI-ERROR                         
00295          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00296          GO TO 8100-SEND-INITIAL-MAP.                             
00297                                                                   
00298      EJECT                                                        
00299  0200-RECEIVE.                                                    
00300      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00301          MOVE LOW-VALUES TO EL106AI                               
00302          MOVE ER-7008    TO EMI-ERROR                             
00303          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00304          MOVE -1 TO MAINTL                                        
00305          GO TO 8200-SEND-DATAONLY.                                
00306                                                                   
00307      EXEC CICS RECEIVE                                            
00308          MAP   (MAP-NAME)                                         
00309          MAPSET(MAPSET-NAME)                                      
00310          INTO  (EL106AI)                                          
00311      END-EXEC.                                                    
00312                                                                   
00313      IF ENTERPFL = 0                                              
00314          GO TO 0300-CHECK-PFKEYS.                                 
00315                                                                   
00316      IF EIBAID NOT = DFHENTER                                     
00317          MOVE ER-0004 TO EMI-ERROR                                
00318          GO TO 0320-INPUT-ERROR.                                  
00319                                                                   
00320      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)   
00321          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      
00322      ELSE                                                         
00323          MOVE ER-0029 TO EMI-ERROR                                
00324          GO TO 0320-INPUT-ERROR.                                  
00325                                                                   
00326  0300-CHECK-PFKEYS.                                               
00327      IF EIBAID = DFHPF23                                          
00328          GO TO 8810-PF23.                                         
00329                                                                   
00330      IF EIBAID = DFHPF24                                          
00331          GO TO 9200-RETURN-MAIN-MENU.                             
00332                                                                   
00333      IF EIBAID = DFHPF12                                          
00334          GO TO 9500-PF12.                                         
00335                                                                   
00336      IF MAINTL NOT = 0  AND                                       
00337         EIBAID NOT = DFHENTER                                     
00338           MOVE ER-0050 TO EMI-ERROR                               
00339           GO TO 0320-INPUT-ERROR.                                 
00340                                                                   
00341      IF EIBAID = DFHPF1                                           
00342          GO TO 5000-FIND-NEXT-STATE.                              
00343                                                                   
00344      IF EIBAID = DFHPF2                                           
00345          GO TO 5500-FIND-PREV-STATE.                              
00346                                                                   
00347      IF EIBAID = DFHPF3                                           
00348          IF NOT MORTGAGE-SESSION                                  
00349              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')        
00350                  MOVE ER-0029        TO  EMI-ERROR                
00351                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00352                  MOVE -1             TO  ENTERPFL                 
00353                  MOVE AL-UNBON       TO  ENTERPFA                 
00354                  GO TO 8200-SEND-DATAONLY                         
00355              ELSE                                                 
00356                  MOVE PI-PREV-STATE  TO  PI-WS-STATE              
00357                  MOVE SPACES         TO  PI-WS-CLASS              
00358                                          PI-WS-DEV                
00359                                          PI-WS-TYPE               
00360                                          PI-WS-PLAN               
00361                  PERFORM 6400-CREATE-TEMP-STORAGE THRU 6400-EXIT  
00362                  MOVE XCTL-EL6565    TO  PGM-NAME                 
00363                  GO TO 9300-XCTL                                  
00364          ELSE                                                     
00365              MOVE ER-7536 TO EMI-ERROR                            
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00367              MOVE AL-UNBON TO ENTERPFA                            
00368              IF ENTERPFL = 0                                      
00369                  MOVE -1 TO MAINTL                                
00370                  GO TO 8200-SEND-DATAONLY                         
00371              ELSE                                                 
00372                  MOVE -1 TO ENTERPFL                              
00373                  GO TO 8200-SEND-DATAONLY.                        
00374                                                                   
00375      IF EIBAID = DFHPF4                                           
00376          IF NOT MORTGAGE-SESSION                                  
00377              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')        
00378                  MOVE ER-0029        TO  EMI-ERROR                
00379                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00380                  MOVE -1             TO  ENTERPFL                 
00381                  MOVE AL-UNBON       TO  ENTERPFA                 
00382                  GO TO 8200-SEND-DATAONLY                         
00383              ELSE                                                 
00384                  MOVE PI-PREV-STATE  TO  PI-WS-STATE              
00385                  MOVE SPACES         TO  PI-WS-CLASS              
00386                                          PI-WS-DEV                
00387                                          PI-WS-TYPE               
00388                                          PI-WS-PLAN               
00389                  MOVE XCTL-EL1061    TO  PGM-NAME                 
00390                  GO TO 9300-XCTL                                  
00391          ELSE                                                     
00392              MOVE ER-7536 TO EMI-ERROR                            
00393              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00394              MOVE AL-UNBON TO ENTERPFA                            
00395              IF ENTERPFL = 0                                      
00396                  MOVE -1 TO MAINTL                                
00397                  GO TO 8200-SEND-DATAONLY                         
00398              ELSE                                                 
00399                  MOVE -1 TO ENTERPFL                              
00400                  GO TO 8200-SEND-DATAONLY.                        

102717     IF EIBAID = DFHPF5
102717        MOVE PI-PREV-STATE       TO PI-WS-STATE
102717        MOVE SPACES              TO PI-WS-CLASS
102717                                    PI-WS-DEV
102717                                    PI-WS-TYPE
102717                                    PI-WS-PLAN
102717        MOVE XCTL-EL1062         TO PGM-NAME
102717        GO TO 9300-XCTL
102717     END-IF

00402      IF EIBAID = DFHENTER                                         
00403          GO TO 0330-EDIT-DATA.                                    
00404                                                                   
00405      MOVE ER-0029 TO EMI-ERROR.                                   
00406  0320-INPUT-ERROR.                                                
00407      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00408      MOVE AL-UNBON TO ENTERPFA.                                   
00409                                                                   
00410      IF ENTERPFL = 0                                              
00411          MOVE -1 TO MAINTL                                        
00412      ELSE                                                         
00413          MOVE -1 TO ENTERPFL.                                     
00414                                                                   
00415      GO TO 8200-SEND-DATAONLY.                                    
00416                                                                   
00417      EJECT                                                        
00418  0330-EDIT-DATA.                                                  
00419      IF STCDL = ZERO AND                                          
00420         STABRL = ZERO                                             
00421          MOVE ER-0144       TO EMI-ERROR                          
00422          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00423          MOVE -1            TO STCDL                              
00424          MOVE AL-UABON      TO STCDA                              
00425          GO TO 8200-SEND-DATAONLY.                                
00426                                                                   
00427      IF STCDL = ZERO AND                                          
00428         STABRL NOT = ZERO                                         
00429          PERFORM 0500-GET-STATE-CD THRU 0600-EXIT.                
00430                                                                   
00431      IF MAINTI = 'S'                                              
00432          GO TO 1000-SHOW-STATE.                                   
00433                                                                   
00434      IF MODIFY-CAP                                         
00435         NEXT SENTENCE                                             
00436        ELSE                                                       
00437         IF MAINTI = 'A' OR 'C' OR 'D'                             
00438          MOVE 'UPDATE'       TO SM-READ                           
00439          PERFORM 9995-SECURITY-VIOLATION                          
00440          MOVE ER-0070        TO EMI-ERROR                         
00441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00442          MOVE LOW-VALUES     TO EL106AO                           
00443          GO TO 8100-SEND-INITIAL-MAP.                             
00444                                                                   
00445      IF MAINTI = 'C'                                              
00446          GO TO 2000-CHANGE-STATE.                                 
00447                                                                   
00448      IF MAINTI = 'A'                                              
00449          GO TO 3000-ADD-STATE.                                    
00450                                                                   
00451      IF MAINTI = 'D'                                              
00452          GO TO 4000-DELETE-STATE.                                 
00453                                                                   
00454      MOVE ER-0023 TO EMI-ERROR.                                   
00455      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00456      MOVE -1       TO MAINTL.                                     
00457      MOVE AL-UABON TO MAINTA.                                     
00458      GO TO 8200-SEND-DATAONLY.                                    
00459                                                                   
00460      EJECT                                                        
00461  0500-GET-STATE-CD.                                               
00462      MOVE PI-COMPANY-ID  TO CK-COMP-ID.                           
00463      MOVE LOW-VALUES     TO CK-STATE-CD.                          
00464      MOVE +0             TO CK-SEQ.                               
00465                                                                   
00466      EXEC CICS HANDLE CONDITION                                   
00467          ENDFILE(8880-NOT-FOUND)                                  
00468          NOTFND (8880-NOT-FOUND)                                  
00469      END-EXEC.                                                    
00470                                                                   
00471      EXEC CICS STARTBR                                            
00472          DATASET  (ELCNTL-ID)                                     
00473          RIDFLD   (ELCNTL-KEY)                                    
00474      END-EXEC.                                                    
00475                                                                   
00476  0510-GET-NEXT-CD.                                                
00477      EXEC CICS READNEXT                                           
00478          DATASET(ELCNTL-ID)                                       
00479          SET    (ADDRESS OF CONTROL-FILE)                         
00480          RIDFLD (ELCNTL-KEY)                                      
00481      END-EXEC.                                                    
00482                                                                   
00483      IF CF-COMPANY-ID NOT = PI-COMPANY-ID                         
00484          EXEC CICS ENDBR                                          
00485              DATASET  (ELCNTL-ID)                                 
00486          END-EXEC                                                 
00487          GO TO 8880-NOT-FOUND.                                    
00488                                                                   
00489      IF CF-RECORD-TYPE NOT = '3'                                  
00490          GO TO 0510-GET-NEXT-CD.                                  
00491                                                                   
00492      IF CF-STATE-ABBREVIATION = STABRI                            
00493          NEXT SENTENCE                                            
00494        ELSE                                                       
00495          GO TO 0510-GET-NEXT-CD.                                  
00496                                                                   
00497      MOVE CF-STATE-CODE      TO STCDI.                            
00498      MOVE AL-UANON           TO STCDA.                            
00499      MOVE +2                 TO STCDL.                            
00500                                                                   
00501      EXEC CICS ENDBR                                              
00502          DATASET  (ELCNTL-ID)                                     
00503      END-EXEC.                                                    
00504                                                                   
00505  0600-EXIT.                                                       
00506       EXIT.                                                       
00507      EJECT                                                        
00508  1000-SHOW-STATE.                                                 
00509      IF RETURN-DISPLAY                                            
00510          EXEC CICS HANDLE CONDITION                               
00511              NOTFND  (8100-SEND-INITIAL-MAP)                      
00512          END-EXEC.                                                
00513                                                                   
00514      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            
00515      MOVE STCDI         TO CK-STATE-CD                            
00516                            PI-WS-STATE.                           
00517                                                                   
00518      EXEC CICS READ                                               
00519          DATASET(ELCNTL-ID)                                       
00520          SET    (ADDRESS OF CONTROL-FILE)                         
00521          RIDFLD (ELCNTL-KEY)                                      
00522      END-EXEC.                                                    
00523                                                                   
00524      GO TO 7000-BUILD-OUTPUT-MAP.                                 
00525                                                                   
00526      EJECT                                                        
00527  2000-CHANGE-STATE.                                               
00528      IF STCDI NOT = PI-PREV-STATE                                 
00529          MOVE ER-0145 TO EMI-ERROR                                
00530          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00531          MOVE -1       TO STCDL                                   
00532          MOVE AL-UABON TO STCDA                                   
00533          GO TO 8200-SEND-DATAONLY.                                
00534                                                                   
00535      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.                 
00536                                                                   
00537      IF NOT EMI-NO-ERRORS                                         
00538          GO TO 8200-SEND-DATAONLY.                                
00539                                                                   
00540      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            
00541      MOVE STCDI         TO CK-STATE-CD                            
00542                            PI-WS-STATE.                           
00543                                                                   
00544      EXEC CICS READ                                               
00545          UPDATE                                                   
00546          DATASET(ELCNTL-ID)                                       
00547          SET    (ADDRESS OF CONTROL-FILE)                         
00548          RIDFLD (ELCNTL-KEY)                                      
00549      END-EXEC.                                                    
00550                                                                   
00551      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR             
00552         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS               
00553          EXEC CICS UNLOCK                                         
00554              DATASET(ELCNTL-ID)                                   
00555          END-EXEC                                                 
00556          MOVE ER-0068 TO EMI-ERROR                                
00557          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00558          GO TO 1000-SHOW-STATE.                                   

102717     if cf-st-ga-comm-cap-sl not numeric
102717        move zeros               to cf-st-ga-comm-cap-sl
102717     end-if
102717     if cf-st-ga-comm-cap-jl not numeric
102717        move zeros               to cf-st-ga-comm-cap-jl
102717     end-if
102717     if cf-st-ga-comm-cap-sa not numeric
102717        move zeros               to cf-st-ga-comm-cap-sa
102717     end-if
102717     if cf-st-ga-comm-cap-ja not numeric
102717        move zeros               to cf-st-ga-comm-cap-ja
102717     end-if
102717
102717     if cf-st-tot-comm-cap-sl not numeric
102717        move zeros               to cf-st-tot-comm-cap-sl
102717     end-if
102717     if cf-st-tot-comm-cap-jl not numeric
102717        move zeros               to cf-st-tot-comm-cap-jl
102717     end-if
102717     if cf-st-tot-comm-cap-sa not numeric
102717        move zeros               to cf-st-tot-comm-cap-sa
102717     end-if
102717     if cf-st-tot-comm-cap-ja not numeric
102717        move zeros               to cf-st-tot-comm-cap-ja
102717     end-if

102717     if (cf-commission-cap-required = 'Y')
                  or
              (ccreqi = 'Y')
              display ' found YYY'
102717        if (cf-st-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-ja = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-ja = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-ja = zeros or spaces
                      or low-values)
                      display ' about to unlock '
102717           EXEC CICS UNLOCK                                         
102717              DATASET(ELCNTL-ID)                                   
102717           END-EXEC                                                 
102717           MOVE ER-1964          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717           GO TO 1000-SHOW-STATE
102717        end-if
102717     end-if

00560      MOVE 'B'                    TO JP-RECORD-TYPE.               
00561      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
00562      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              
00563                                                                   
00564      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY.                    
00565      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS.                
00566      MOVE EIBDATE         TO DC-JULIAN-YYDDD.                     
00567      MOVE '5'             TO DC-OPTION-CODE.                      
00568      MOVE LINK-ELDATCV    TO PGM-NAME.                            
00569                                                                   
00570      EXEC CICS LINK                                               
00571          PROGRAM (PGM-NAME)                                       
00572          COMMAREA(DATE-CONVERSION-DATA)                           
00573          LENGTH  (DC-COMM-LENGTH)                                 
00574      END-EXEC.                                                    
00575                                                                   
00576      IF DATE-CONVERSION-ERROR                                     
00577          MOVE LOW-VALUES          TO CF-LAST-MAINT-DT             
00578      ELSE                                                         
00579          MOVE DC-BIN-DATE-1       TO CF-LAST-MAINT-DT.            
00580                                                                   
00581      IF STABRL NOT = ZEROS                                        
00582          MOVE STABRI              TO CF-STATE-ABBREVIATION.       
00583                                                                   
00584      IF STNAMEL NOT = ZEROS                                       
00585          MOVE STNAMEI             TO CF-STATE-NAME.               
00586                                                                   
00587      IF LFEXPL NOT = ZEROS                                        
00588          MOVE WS-ST-LF-EXP-PCT    TO CF-ST-LF-EXP-PCT.            
00589                                                                   
00590      IF AHEXPL NOT = ZEROS                                        
00591          MOVE WS-ST-AH-EXP-PCT    TO CF-ST-AH-EXP-PCT.            
00592                                                                   
00593      IF QUOTCALL NOT = ZEROS                                      
00594          MOVE QUOTCALI            TO CF-ST-TOL-CLAIM.             
00595                                                                   
00596      IF PREMTOLL NOT = ZEROS                                      
00597          MOVE PREMTOLI            TO CF-ST-TOL-PREM.              
00598                                                                   
00599      IF REFTOLL NOT = ZEROS                                       
00600          MOVE REFTOLI             TO CF-ST-TOL-REFUND.            
00601                                                                   
00602      IF OVSAMTL > 0                                               
00603          MOVE OVSAMTI TO CF-ST-OVR-SHT-AMT                        
00604      END-IF.                                                      
00605                                                                   
00606      IF PRMPCTL NOT = ZEROS                                       
00607          MOVE WS-ST-TOL-PREM-PCT  TO CF-ST-TOL-PREM-PCT.          
00608                                                                   
00609      IF REFPCTL NOT = ZEROS                                       
00610          MOVE  WS-ST-TOL-REF-PCT  TO CF-ST-TOL-REF-PCT.           
00611                                                                   
00612      IF OVSPCTL > +0                                              
00613          MOVE WS-ST-OVR-SHT-PCT TO CF-ST-OVR-SHT-PCT              
00614      END-IF.                                                      
00615                                                                   
00616      IF CLREJECL NOT = ZEROS                                      
00617          MOVE CLREJECI            TO CF-ST-CLAIM-REJECT-SW.       
00618                                                                   
00619      IF ISSREJL NOT = ZEROS                                       
00620          MOVE ISSREJI             TO CF-ST-PREM-REJECT-SW.        

040915     IF AGTSIGL NOT = ZEROS
040915        MOVE AGTSIGI             TO CF-ST-AGENT-SIG-EDIT
040915     END-IF

070115     IF NETONLYL NOT = ZEROS
070115        MOVE NETONLYI            TO CF-ST-NET-ONLY-STATE
070115     END-IF

102717     if ccreql <> zeros
102717        move ccreqi              to cf-commission-cap-required
102717     end-if

00622      IF REFREJL NOT = ZEROS                                       
00623          MOVE REFREJI             TO CF-ST-REF-REJECT-SW.         
00624                                                                   
00625      IF REFMINL NOT = ZEROS                                       
00626          MOVE REFMINI             TO CF-ST-REFUND-MIN.            
00627                                                                   
00628      IF REFDAY1L NOT = ZEROS                                      
00629          MOVE REFDAY1I            TO CF-ST-REFUND-DAYS-FIRST.     
00630                                                                   
00631      IF REFDAYSL NOT = ZEROS                                      
00632          MOVE REFDAYSI            TO CF-ST-REFUND-DAYS-SUBSEQ.    
00633                                                                   
00634      IF SPLPMTL NOT = ZEROS                                       
00635          MOVE SPLPMTI             TO CF-ST-SPLIT-PAYMENT.         
00636                                                                   
00637      IF EXTDAYSL NOT = ZEROS                                      
00638          MOVE EXTDAYSI            TO CF-ST-FST-PMT-DAYS-MAX.      
00639                                                                   
PEMMOD*    IF INTDAYSL NOT = ZEROS                                      
PEMMOD*        MOVE INTDAYSI            TO CF-ST-NO-DAYS-ELAPSED.       
00642                                                                   
00643      IF EXTCHGL NOT = ZEROS                                       
00644          MOVE EXTCHGI             TO CF-ST-FST-PMT-DAYS-CHG.      
00645                                                                   
CIDMOD     IF REMTERML NOT = ZEROS
CIDMOD        MOVE REMTERMI             TO CF-ST-RT-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFREDL NOT = ZEROS
PEMMOD        MOVE LFREDI               TO CF-ST-RF-LR-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFLEVL NOT = ZEROS
PEMMOD        MOVE LFLEVI               TO CF-ST-RF-LL-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFNETL NOT = ZEROS
PEMMOD        MOVE LFNETI               TO CF-ST-RF-LN-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHAHL NOT = ZEROS
PEMMOD        MOVE AHAHI                TO CF-ST-RF-AH-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHCPL NOT = ZEROS
PEMMOD        MOVE AHCPI                TO CF-ST-RF-CP-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFTAXL > +0                                               
PEMMOD        MOVE WS-ST-LF-PREM-TAX   TO CF-ST-LF-PREM-TAX             
PEMMOD     END-IF                                                       
PEMMOD                                                                  
PEMMOD     IF AHITAXL > +0                                              
PEMMOD        MOVE WS-ST-AH-PREM-TAX-I TO CF-ST-AH-PREM-TAX-I           
PEMMOD     END-IF                                                       
PEMMOD                                                                  
PEMMOD     IF AHGTAXL > +0                                              
PEMMOD        MOVE WS-ST-AH-PREM-TAX-G TO CF-ST-AH-PREM-TAX-G           
PEMMOD     END-IF                                                       
PEMMOD                                                                  
022415     IF (XINTL NOT = ZEROS)
022415        and (xinti numeric)
022415        MOVE XINTO               TO CF-ST-EXTRA-INTEREST-PERIODS
022415     END-IF
022415     IF (XPMTSL NOT = ZEROS)
022415        and (xpmtsi numeric)
022415        MOVE XPMTSO              TO CF-ST-EXTRA-PAYMENTS
022415     END-IF
011410     IF REFCLML NOT = ZEROS
011410         MOVE REFCLMI             TO CF-ST-REF-AH-DEATH-IND
011410     END-IF.
011410
061511     IF VFYBENEL NOT = ZEROS
061511         MOVE VFYBENEI            TO CF-ST-VFY-2ND-BENE
061511     END-IF.
061511 
012913     IF CAUSALL NOT = ZEROS
012913         MOVE CAUSALI             TO CF-ST-CAUSAL-STATE
012913     END-IF.
012913
00646      IF STUEL NOT = ZEROS                                         
00647          MOVE STUEI               TO CF-ST-CALL-UNEARNED.         
00648                                                                   
00649      IF STCNTLL NOT = ZEROS                                       
00650          MOVE STCNTLI            TO CF-ST-CALL-RPT-CNTL.          
00651                                                                   
00652      IF STDEVL NOT = ZEROS                                        
00653          MOVE STDEVI             TO CF-ST-CALL-RATE-DEV.          
00654                                                                   
00655      MOVE REPLAWI                TO CF-REPLACEMENT-LAW-SW.        
00656      MOVE REPLETRI               TO CF-REPLACEMENT-LETTER.        
00657                                                                   
00660 *    MOVE STATII                 TO CF-ST-STAT-DATE-FROM.         
00685                                                                   
00686      IF WS-IRATE NUMERIC AND                                      
00687         WS-IRATE NOT = ZEROS                                      
00688          MOVE WS-IRATE           TO CF-ST-STAT-INTEREST           
00689       ELSE                                                        
00690          MOVE ZEROS              TO CF-ST-STAT-INTEREST.          
00691                                                                   
00692      IF WS-IRATE1 NUMERIC AND                                     
00693         WS-IRATE1 NOT = ZEROS                                     
00694          MOVE WS-IRATE1          TO CF-ST-STAT-INTEREST-1         
00695       ELSE                                                        
00696          MOVE ZEROS              TO CF-ST-STAT-INTEREST-1.        
00697                                                                   
00698      IF WS-IRATE2 NUMERIC AND                                     
00699         WS-IRATE2 NOT = ZEROS                                     
00700          MOVE WS-IRATE2          TO CF-ST-STAT-INTEREST-2         
00701       ELSE                                                        
00702          MOVE ZEROS              TO CF-ST-STAT-INTEREST-2.        
00703                                                                   
00704      IF WS-IRATE3 NUMERIC AND                                     
00705         WS-IRATE3 NOT = ZEROS                                     
00706          MOVE WS-IRATE3          TO CF-ST-STAT-INTEREST-3         
00707       ELSE                                                        
00708          MOVE ZEROS              TO CF-ST-STAT-INTEREST-3.        
00709                                                                   
00710      IF WS-RESIDENT-TAX NUMERIC AND                               
00711         WS-RESIDENT-TAX NOT = ZEROS                               
00712          MOVE WS-RESIDENT-TAX    TO CF-ST-RES-TAX-PCT             
00713       ELSE                                                        
00714          MOVE ZEROS              TO CF-ST-RES-TAX-PCT.            
00715                                                                   
00716      IF WS-FREE-LOOK-DAYS NUMERIC                                 
00717              AND                                                  
00718         WS-FREE-LOOK-DAYS NOT EQUAL ZEROS                         
00719          MOVE WS-FREE-LOOK-DAYS  TO CF-ST-FREE-LOOK-PERIOD        
00720       ELSE                                                        
00721          MOVE ZEROS              TO CF-ST-FREE-LOOK-PERIOD.       
00722                                                                   
00723      IF  WS-ST-TARGET-LOSS-RATIO NUMERIC                          
00724              AND                                                  
00725          WS-ST-TARGET-LOSS-RATIO NOT EQUAL ZEROS                  
00726          MOVE WS-ST-TARGET-LOSS-RATIO                             
00727                                  TO CF-ST-TARGET-LOSS-RATIO       
00728      ELSE                                                         
00729          MOVE ZEROS              TO CF-ST-TARGET-LOSS-RATIO.      
00730                                                                   
00731      IF  WS-ST-CALC-INTEREST NUMERIC                              
00732              AND                                                  
00733          WS-ST-CALC-INTEREST NOT EQUAL ZEROS                      
00734          MOVE WS-ST-CALC-INTEREST                                 
00735                                  TO CF-ST-CALC-INTEREST           
00736      ELSE                                                         
00737          MOVE ZEROS              TO CF-ST-CALC-INTEREST.          
00738                                                                   
00739      MOVE 'C'                    TO JP-RECORD-TYPE.               
00740      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
00741      EXEC CICS REWRITE                                            
00742          DATASET(ELCNTL-ID)                                       
00743          FROM   (CONTROL-FILE)                                    
00744      END-EXEC.                                                    
00745                                                                   
00746      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              
00747      MOVE ER-0000 TO EMI-ERROR.                                   
00748      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00749      MOVE LOW-VALUES  TO EL106AO.                                 
00750      MOVE -1          TO MAINTL.                                  
00751      MOVE SPACES      TO PI-PREV-STATE.                           
00752      MOVE CK-STATE-CD TO STCDO.                                   
00753      MOVE AL-UANON    TO STCDA.                                   
00754      GO TO 1000-SHOW-STATE.                                       
00755                                                                   
00756      EJECT                                                        
00757  3000-ADD-STATE.                                                  
00758      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.                 
00759                                                                   
00760      IF NOT EMI-NO-ERRORS                                         
00761          GO TO 8200-SEND-DATAONLY.                                
00762                                                                   
00763      EXEC CICS GETMAIN                                            
00764          SET    (ADDRESS OF CONTROL-FILE)                         
00765          LENGTH (750)                                             
00766          INITIMG(GETMAIN-SPACE)                                   
00767      END-EXEC.                                                    
00768                                                                   
00769      MOVE 'CF'          TO CF-RECORD-ID.                          
00770      MOVE PI-COMPANY-ID TO CF-COMPANY-ID.                         
00771      MOVE '3'           TO CF-RECORD-TYPE.                        
00772      MOVE STCDI         TO CF-ACCESS-OF-STATE.                    
00773      MOVE +0            TO CF-SEQUENCE-NO.                        
00774                                                                   
00775      MOVE EIBDATE       TO DC-JULIAN-YYDDD.                       
00776      MOVE '5'           TO DC-OPTION-CODE.                        
00777      MOVE LINK-ELDATCV  TO PGM-NAME.                              
00778      EXEC CICS LINK                                               
00779          PROGRAM (PGM-NAME)                                       
00780          COMMAREA(DATE-CONVERSION-DATA)                           
00781          LENGTH  (DC-COMM-LENGTH)                                 
00782      END-EXEC.                                                    
00783                                                                   
00784      IF DATE-CONVERSION-ERROR                                     
00785          MOVE LOW-VALUES    TO CF-LAST-MAINT-DT                   
00786        ELSE                                                       
00787          MOVE DC-BIN-DATE-1 TO CF-LAST-MAINT-DT.                  
00788                                                                   
00789      MOVE PI-PROCESSOR-ID   TO CF-LAST-MAINT-BY.                  
00790      MOVE EIBTIME           TO CF-LAST-MAINT-HHMMSS.              
00791                                                                   
00792      MOVE STNAMEI           TO CF-STATE-NAME.                     
00793      MOVE STABRI            TO CF-STATE-ABBREVIATION.             
00794                                                                   
00795      IF LFEXPL = ZEROS                                            
00796          MOVE ZEROS               TO CF-ST-LF-EXP-PCT             
00797        ELSE                                                       
00798          MOVE WS-ST-LF-EXP-PCT    TO CF-ST-LF-EXP-PCT.            
00799                                                                   
00800      IF AHEXPL = ZEROS                                            
00801          MOVE ZEROS               TO CF-ST-AH-EXP-PCT             
00802        ELSE                                                       
00803          MOVE WS-ST-AH-EXP-PCT    TO CF-ST-AH-EXP-PCT.            
00804                                                                   
00805      IF QUOTCALL = ZEROS                                          
00806          MOVE ZEROS               TO CF-ST-TOL-CLAIM              
00807        ELSE                                                       
00808          MOVE QUOTCALI            TO CF-ST-TOL-CLAIM.             
00809                                                                   
00810      IF PREMTOLL = ZEROS                                          
00811          MOVE ZEROS               TO CF-ST-TOL-PREM               
00812        ELSE                                                       
00813          MOVE PREMTOLI            TO CF-ST-TOL-PREM.              
00814                                                                   
00815      IF OVSAMTL = ZEROS                                           
00816          MOVE ZEROS              TO CF-ST-OVR-SHT-AMT             
00817        ELSE                                                       
00818          MOVE OVSAMTI            TO CF-ST-OVR-SHT-AMT.            
00819                                                                   
00820      IF REFTOLL = ZEROS                                           
00821          MOVE ZEROS              TO CF-ST-TOL-REFUND              
00822        ELSE                                                       
00823          MOVE REFTOLI            TO CF-ST-TOL-REFUND.             
00824                                                                   
00825      IF PRMPCTL = ZEROS                                           
00826          MOVE ZEROS              TO CF-ST-TOL-PREM-PCT            
00827        ELSE                                                       
00828          MOVE WS-ST-TOL-PREM-PCT TO CF-ST-TOL-PREM-PCT.           
00829                                                                   
00830      IF OVSPCTL = ZEROS                                           
00831          MOVE ZEROS              TO CF-ST-OVR-SHT-PCT             
00832        ELSE                                                       
00833          MOVE WS-ST-OVR-SHT-PCT  TO CF-ST-OVR-SHT-PCT.            
00834                                                                   
00835      IF REFPCTL = ZEROS                                           
00836          MOVE ZEROS              TO CF-ST-TOL-REF-PCT             
00837        ELSE                                                       
00838          MOVE WS-ST-TOL-REF-PCT  TO CF-ST-TOL-REF-PCT.            
00839                                                                   
00840      IF CLREJECL = ZEROS                                          
00841          MOVE SPACE              TO CF-ST-CLAIM-REJECT-SW         
00842        ELSE                                                       
00843          MOVE CLREJECI           TO CF-ST-CLAIM-REJECT-SW.        
00844                                                                   
00845      IF ISSREJL = ZEROS                                           
00846          MOVE SPACE              TO CF-ST-PREM-REJECT-SW          
00847        ELSE                                                       
00848          MOVE ISSREJI            TO CF-ST-PREM-REJECT-SW.         

040915     IF AGTSIGL = ZEROS
040915        MOVE SPACES              TO CF-ST-AGENT-SIG-EDIT
040915     ELSE
040915        MOVE AGTSIGI             TO CF-ST-AGENT-SIG-EDIT
040915     END-IF

070115     IF NETONLYL = ZEROS
070115        MOVE SPACES              TO CF-ST-NET-ONLY-STATE
070115     ELSE
070115        MOVE NETONLYI            TO CF-ST-NET-ONLY-STATE
070115     END-IF


102717     if ccreql = zeros
102717        MOVE SPACES              TO CF-COMMISSION-CAP-REQUIRED
102717     else
102717        move ccreqi              to cf-commission-cap-required
102717     end-if

00850      IF REFREJL = ZEROS                                           
00851          MOVE SPACE              TO CF-ST-REF-REJECT-SW           
00852        ELSE                                                       
00853          MOVE REFREJI            TO CF-ST-REF-REJECT-SW.          
00854                                                                   
00855      IF REFMINL = ZEROS                                           
00856          MOVE ZEROS              TO CF-ST-REFUND-MIN              
00857        ELSE                                                       
00858          MOVE REFMINI            TO CF-ST-REFUND-MIN.             
00859                                                                   
00860      IF REFDAY1L  = ZERO                                          
00861          MOVE ZEROS              TO CF-ST-REFUND-DAYS-FIRST       
00862        ELSE                                                       
00863          MOVE REFDAY1I           TO CF-ST-REFUND-DAYS-FIRST.      
00864                                                                   
00865      IF REFDAYSL = ZERO                                           
00866          MOVE ZEROS              TO CF-ST-REFUND-DAYS-SUBSEQ      
00867        ELSE                                                       
00868          MOVE REFDAYSI           TO CF-ST-REFUND-DAYS-SUBSEQ.     
00869                                                                   
00870      IF SPLPMTL = ZERO                                            
00871          MOVE 'N'                TO CF-ST-SPLIT-PAYMENT           
00872      ELSE                                                         
00873          MOVE SPLPMTI            TO CF-ST-SPLIT-PAYMENT.          
00874                                                                   
00875      IF EXTDAYSL = ZERO                                           
00876          MOVE ZEROS              TO CF-ST-FST-PMT-DAYS-MAX        
00877        ELSE                                                       
00878          MOVE EXTDAYSI           TO CF-ST-FST-PMT-DAYS-MAX.       
00879                                                                   
PEMMOD*    IF INTDAYSL = ZERO                                           
PEMMOD*        MOVE ZEROS              TO CF-ST-NO-DAYS-ELAPSED         
PEMMOD*      ELSE                                                       
PEMMOD*        MOVE INTDAYSI           TO CF-ST-NO-DAYS-ELAPSED.        
00884                                                                   
00885      IF EXTCHGL = ZEROS                                           
00886          MOVE SPACES             TO CF-ST-FST-PMT-DAYS-CHG        
00887        ELSE                                                       
00888          MOVE EXTCHGI            TO CF-ST-FST-PMT-DAYS-CHG.       
00889                                                                   
CIDMOD     IF REMTERML = ZEROS
CIDMOD        MOVE SPACES              TO CF-ST-RT-CALC
CIDMOD     ELSE
CIDMOD        MOVE REMTERMI            TO CF-ST-RT-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFTAXL = ZEROS                                            
PEMMOD        MOVE ZEROS               TO CF-ST-LF-PREM-TAX             
PEMMOD     ELSE                                                         
PEMMOD        MOVE WS-ST-LF-PREM-TAX   TO CF-ST-LF-PREM-TAX             
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHITAXL = ZEROS                                           
PEMMOD        MOVE ZEROS               TO CF-ST-AH-PREM-TAX-I           
PEMMOD     ELSE                                                         
PEMMOD        MOVE WS-ST-AH-PREM-TAX-I TO CF-ST-AH-PREM-TAX-I           
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHGTAXL = ZEROS                                           
PEMMOD        MOVE ZEROS               TO CF-ST-AH-PREM-TAX-G           
PEMMOD     ELSE                                                         
PEMMOD        MOVE WS-ST-AH-PREM-TAX-G TO CF-ST-AH-PREM-TAX-G           
PEMMOD     END-IF
PEMMOD                                                                  
00890      IF STUEL = ZEROS                                             
00891          MOVE SPACES             TO CF-ST-CALL-UNEARNED           
00892        ELSE                                                       
00893          MOVE STUEI              TO CF-ST-CALL-UNEARNED.          
00894                                                                   
00895      IF STCNTLL = ZEROS                                           
00896          MOVE SPACES             TO CF-ST-CALL-RPT-CNTL           
00897        ELSE                                                       
00898          MOVE STCNTLI            TO CF-ST-CALL-RPT-CNTL.          
00899                                                                   
00900      IF STDEVL = ZEROS                                            
00901          MOVE SPACES             TO CF-ST-CALL-RATE-DEV           
00902        ELSE                                                       
00903          MOVE STDEVI             TO CF-ST-CALL-RATE-DEV.          
00904                                                                   
00905      IF  WS-ST-TARGET-LOSS-RATIO NUMERIC                          
00906              AND                                                  
00907          WS-ST-TARGET-LOSS-RATIO NOT EQUAL ZEROS                  
00908          MOVE WS-ST-TARGET-LOSS-RATIO                             
00909                                  TO CF-ST-TARGET-LOSS-RATIO       
00910      ELSE                                                         
00911          MOVE ZEROS              TO CF-ST-TARGET-LOSS-RATIO.      
00912                                                                   
00913      IF  WS-ST-CALC-INTEREST NUMERIC                              
00914              AND                                                  
00915          WS-ST-CALC-INTEREST NOT EQUAL ZEROS                      
00916          MOVE WS-ST-CALC-INTEREST                                 
00917                                  TO CF-ST-CALC-INTEREST           
00918      ELSE                                                         
00919          MOVE ZEROS              TO CF-ST-CALC-INTEREST.          
00920                                                                   
00921      MOVE REPLAWI                TO CF-REPLACEMENT-LAW-SW.        
00922      MOVE REPLETRI               TO CF-REPLACEMENT-LETTER.        
00923                                                                   
00926 *    MOVE STATII                 TO CF-ST-STAT-DATE-FROM.         
00927                                                                   
00948      IF RESTAXL = ZEROS                                           
00949          MOVE ZEROS              TO CF-ST-RES-TAX-PCT             
00950        ELSE                                                       
00951          MOVE WS-RESIDENT-TAX    TO CF-ST-RES-TAX-PCT.            
00952                                                                   
00953      IF FREELKL = ZEROS                                           
00954          MOVE ZEROS              TO CF-ST-FREE-LOOK-PERIOD        
00955        ELSE                                                       
00956          MOVE WS-FREE-LOOK-DAYS  TO CF-ST-FREE-LOOK-PERIOD.       
00957                                                                   
00958 *    IF STATRL = ZEROS                                            
00959          MOVE ZEROS              TO CF-ST-STAT-INTEREST           
00960 *      ELSE                                                       
00961 *        MOVE WS-IRATE           TO CF-ST-STAT-INTEREST.          
00962                                                                   
00963 *    IF STATR1L = ZEROS                                           
00964          MOVE ZEROS              TO CF-ST-STAT-INTEREST-1         
00965 *      ELSE                                                       
00966 *        MOVE WS-IRATE1          TO CF-ST-STAT-INTEREST-1.        
00967                                                                   
00968 *    IF STATR2L = ZEROS                                           
00969          MOVE ZEROS              TO CF-ST-STAT-INTEREST-2         
00970 *      ELSE                                                       
00971 *        MOVE WS-IRATE2          TO CF-ST-STAT-INTEREST-2.        
00972                                                                   
00973 *    IF STATR3L = ZEROS                                           
00974          MOVE ZEROS              TO CF-ST-STAT-INTEREST-3         
00975 *      ELSE                                                       
00976 *        MOVE WS-IRATE3          TO CF-ST-STAT-INTEREST-3.        
00977                                                                   
00978      PERFORM 6100-INITIALIZE-BENEFIT-CNTL THRU 6199-EXIT          
00979              VARYING SUB FROM +1 BY +1                            
00980              UNTIL SUB GREATER THAN +50.                          
00981                                                                   
00982      MOVE 'A'                    TO JP-RECORD-TYPE.               
00983      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
00984                                                                   
00985      EXEC CICS WRITE                                              
00986          FROM   (CONTROL-FILE)                                    
00987          DATASET(ELCNTL-ID)                                       
00988          RIDFLD (CF-CONTROL-PRIMARY)                              
00989      END-EXEC.                                                    
00990                                                                   
00991      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.              
00992      MOVE ER-0000 TO EMI-ERROR.                                   
00993      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00994      MOVE LOW-VALUES  TO EL106AO.                                 
00995      MOVE -1          TO MAINTL.                                  
00996      MOVE SPACES      TO PI-PREV-STATE.                           
00997      MOVE CK-STATE-CD TO STCDO.                                   
00998      MOVE AL-UANON    TO STCDA.                                   
00999      GO TO 8100-SEND-INITIAL-MAP.                                 
01000                                                                   
01001      EJECT                                                        
01002  4000-DELETE-STATE.                                               
01003      IF STCDI NOT = PI-PREV-STATE                                 
01004          MOVE ER-0145  TO EMI-ERROR                               
01005          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01006          MOVE -1       TO STCDL                                   
01007          MOVE AL-UABON TO STCDA                                   
01008          GO TO 8200-SEND-DATAONLY.                                
01009                                                                   
01010      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            
01011      MOVE STCDI         TO CK-STATE-CD.                           
01012      EXEC CICS READ                                               
01013          UPDATE                                                   
01014          DATASET(ELCNTL-ID)                                       
01015          SET    (ADDRESS OF CONTROL-FILE)                         
01016          RIDFLD (ELCNTL-KEY)                                      
01017      END-EXEC.                                                    
01018                                                                   
01019      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR             
01020         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS               
01021          EXEC CICS UNLOCK                                         
01022              DATASET(ELCNTL-ID)                                   
01023          END-EXEC                                                 
01024          MOVE ER-0068 TO EMI-ERROR                                
01025          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01026          GO TO 1000-SHOW-STATE.                                   
01027                                                                   
01028      MOVE 'D'                    TO JP-RECORD-TYPE.               
01029      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
01030      EXEC CICS DELETE                                             
01031          DATASET(ELCNTL-ID)                                       
01032      END-EXEC.                                                    
01033                                                                   
01034      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT               
01035      MOVE ER-0000 TO EMI-ERROR.                                   
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01037      MOVE LOW-VALUES  TO EL106AO.                                 
01038      MOVE -1          TO MAINTL.                                  
01039      MOVE SPACES      TO PI-PREV-STATE                            
01040      MOVE CK-STATE-CD TO STCDO.                                   
01041      MOVE AL-UANON    TO STCDA.                                   
01042      GO TO 8100-SEND-INITIAL-MAP.                                 
01043                                                                   
01044      EJECT                                                        
01045  5000-FIND-NEXT-STATE.                                            
01046      MOVE PI-COMPANY-ID  TO CK-COMP-ID.                           
01047                                                                   
01048      IF STCDL = 0                                                 
01049          MOVE LOW-VALUES TO CK-STATE-CD                           
01050          MOVE +0         TO CK-SEQ                                
01051      ELSE                                                         
01052          MOVE STCDI      TO CK-STATE-CD                           
01053          MOVE +1         TO CK-SEQ.                               
01054                                                                   
01055      MOVE SPACES TO PI-PREV-STATE.                                
01056                                                                   
01057      EXEC CICS HANDLE CONDITION                                   
01058          NOTFND (8860-ENDFILE)                                    
01059      END-EXEC.                                                    
01060                                                                   
01061      EXEC CICS READ                                               
01062          DATASET(ELCNTL-ID)                                       
01063          SET    (ADDRESS OF CONTROL-FILE)                         
01064          RIDFLD (ELCNTL-KEY)                                      
01065          GTEQ                                                     
01066      END-EXEC.                                                    
01067                                                                   
01068      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    
01069         CF-RECORD-TYPE NOT = '3'                                  
01070          GO TO 8860-ENDFILE.                                      
01071                                                                   
01072      IF STCDL = 0                                                 
01073          MOVE ER-0146 TO EMI-ERROR                                
01074          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
01075                                                                   
01076      GO TO 7000-BUILD-OUTPUT-MAP.                                 
01077                                                                   
01078      EJECT                                                        
01079  5500-FIND-PREV-STATE.                                            
01080      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.                  
01081      MOVE PI-PREV-STATE          TO  CK-STATE-CD.                 
01082                                                                   
01083      IF STCDL GREATER +0                                          
01084          MOVE STCDI              TO  CK-STATE-CD.                 
01085                                                                   
01086      MOVE SPACES                 TO  PI-PREV-STATE.               
01087                                                                   
01088      EXEC CICS HANDLE CONDITION                                   
01089          NOTFND(8860-ENDFILE)                                     
01090      END-EXEC.                                                    
01091                                                                   
01092      EXEC CICS STARTBR                                            
01093          DATASET  (ELCNTL-ID)                                     
01094          RIDFLD   (ELCNTL-KEY)                                    
01095      END-EXEC.                                                    
01096                                                                   
01097  5600-READ-PREV-STATE-RECORD.                                     
01098      EXEC CICS READPREV                                           
01099          DATASET  (ELCNTL-ID)                                     
01100          SET      (ADDRESS OF CONTROL-FILE)                       
01101          RIDFLD   (ELCNTL-KEY)                                    
01102      END-EXEC.                                                    
01103                                                                   
01104      IF FIRST-TIME                                                
01105          MOVE 'N'                TO  WS-FIRST-TIME-SW             
01106          GO TO 5600-READ-PREV-STATE-RECORD.                       
01107                                                                   
01108      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR                    
01109         CF-RECORD-TYPE NOT = '3'                                  
01110          GO TO 8860-ENDFILE.                                      
01111                                                                   
01112      IF STCDL = 0                                                 
01113          MOVE ER-0146 TO EMI-ERROR                                
01114          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
01115                                                                   
01116      GO TO 7000-BUILD-OUTPUT-MAP.                                 
01117                                                                   
01118      EJECT                                                        
01119  6000-EDIT-INPUT-DATA.                                            
01120      IF STABRL = ZEROS AND MAINTI = 'A'                           
01121         MOVE -1  TO STABRL                                        
01122         MOVE ER-0152 TO EMI-ERROR                                 
01123         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01124                                                                   
01125      IF STNAMEL = ZEROS AND MAINTI = 'A'                          
01126         MOVE -1   TO STNAMEL                                      
01127         MOVE ER-0153 TO EMI-ERROR                                 
01128         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01129                                                                   
01130      IF LFEXPL NOT = ZEROS                                        
01131         EXEC CICS BIF                                             
01132              DEEDIT                                               
01133              FIELD (LFEXPI)                                       
01134              LENGTH(7)                                            
01135         END-EXEC                                                  
01136       IF LFEXPI NUMERIC                                           
01137             MOVE AL-UNNON        TO LFEXPA                        
01138             MOVE LFEXPI          TO WS-ST-LF-EXP-PCT              
01139         ELSE                                                      
01140             MOVE ER-7531         TO EMI-ERROR                     
01141             MOVE -1              TO LFEXPL                        
01142             MOVE AL-UNBON        TO LFEXPA                        
01143             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01144                                                                   
01145      IF AHEXPL NOT = ZEROS                                        
01146         EXEC CICS BIF                                             
01147              DEEDIT                                               
01148              FIELD (AHEXPI)                                       
01149              LENGTH(7)                                            
01150         END-EXEC                                                  
01151       IF AHEXPI NUMERIC                                           
01152             MOVE AL-UNNON        TO AHEXPA                        
01153             MOVE AHEXPI          TO WS-ST-AH-EXP-PCT              
01154         ELSE                                                      
01155             MOVE ER-7531         TO EMI-ERROR                     
01156             MOVE -1              TO AHEXPL                        
01157             MOVE AL-UNBON        TO AHEXPA                        
01158             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01159                                                                   
01160      IF QUOTCALL NOT = ZEROS                                      
01161         EXEC CICS BIF                                             
01162              DEEDIT                                               
01163              FIELD (QUOTCALI)                                     
01164              LENGTH(6)                                            
01165         END-EXEC                                                  
01166         IF QUOTCALI NOT NUMERIC                                   
01167            MOVE -1               TO QUOTCALL                      
01168            MOVE AL-UNBON         TO QUOTCALA                      
01169            MOVE ER-2009          TO EMI-ERROR                     
01170            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01171         ELSE                                                      
01172         IF QUOTCALI GREATER THAN 9999                             
01173            MOVE -1               TO QUOTCALL                      
01174            MOVE AL-UNBON         TO QUOTCALA                      
01175            MOVE ER-2009          TO EMI-ERROR                     
01176            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01177                                                                   
01178      IF OVSAMTL NOT = ZEROS                                       
01179         EXEC CICS BIF                                             
01180              DEEDIT                                               
01181              FIELD (OVSAMTI)                                      
01182              LENGTH(6)                                            
01183         END-EXEC                                                  
01184         IF OVSAMTI  NOT NUMERIC                                   
01185            MOVE -1               TO OVSAMTL                       
01186            MOVE AL-UNBON         TO OVSAMTA                       
01187            MOVE ER-2010          TO EMI-ERROR                     
01188            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01189         ELSE                                                      
01190         IF OVSAMTI  GREATER THAN 9999                             
01191            MOVE -1               TO OVSAMTL                       
01192            MOVE AL-UNBON         TO OVSAMTA                       
01193            MOVE ER-2010          TO EMI-ERROR                     
01194            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01195                                                                   
01196      IF PREMTOLL NOT = ZEROS                                      
01197         EXEC CICS BIF                                             
01198              DEEDIT                                               
01199              FIELD (PREMTOLI)                                     
01200              LENGTH(6)                                            
01201         END-EXEC                                                  
01202         IF PREMTOLI NOT NUMERIC                                   
01203            MOVE -1               TO PREMTOLL                      
01204            MOVE AL-UNBON         TO PREMTOLA                      
01205            MOVE ER-2010          TO EMI-ERROR                     
01206            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01207         ELSE                                                      
01208         IF PREMTOLI GREATER THAN 9999                             
01209            MOVE -1               TO PREMTOLL                      
01210            MOVE AL-UNBON         TO PREMTOLA                      
01211            MOVE ER-2010          TO EMI-ERROR                     
01212            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01213                                                                   
01214      IF REFTOLL NOT = ZEROS                                       
01215         EXEC CICS BIF                                             
01216              DEEDIT                                               
01217              FIELD(REFTOLI)                                       
01218              LENGTH(6)                                            
01219         END-EXEC                                                  
01220         IF REFTOLI NOT NUMERIC                                    
01221            MOVE -1               TO REFTOLL                       
01222            MOVE AL-UNBON         TO REFTOLA                       
01223            MOVE ER-2014          TO EMI-ERROR                     
01224            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01225         ELSE                                                      
01226         IF REFTOLI GREATER THAN 9999                              
01227            MOVE -1               TO REFTOLL                       
01228            MOVE AL-UNBON         TO REFTOLA                       
01229            MOVE ER-2014          TO EMI-ERROR                     
01230            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01231                                                                   
01232      IF CLREJECL NOT = ZEROS                                      
01233         IF CLREJECI = SPACES OR '1'                               
01234            NEXT SENTENCE                                          
01235           ELSE                                                    
01236            MOVE -1               TO CLREJECL                      
01237            MOVE AL-UABON         TO CLREJECA                      
01238            MOVE ER-2024          TO EMI-ERROR                     
01239            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01240                                                                   
01241      IF ISSREJL NOT = ZEROS                                       
01242         IF ISSREJI = SPACES OR '1'                                
01243            NEXT SENTENCE                                          
01244           ELSE                                                    
01245            MOVE -1               TO ISSREJL                       
01246            MOVE AL-UABON         TO ISSREJA                       
01247            MOVE ER-2012          TO EMI-ERROR                     
01248            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              

040915     IF AGTSIGL NOT = ZEROS
040915        IF AGTSIGI = ' ' OR 'Y' OR 'N'
040915           CONTINUE
040915        ELSE
040915           MOVE -1               TO AGTSIGL
040915           MOVE AL-UABON         TO AGTSIGA
040915           MOVE ER-9999          TO EMI-ERROR
040915           PERFORM 9900-ERROR-FORMAT
040915                                 THRU 9900-EXIT
040915        END-IF
040915     END-IF

070115     IF NETONLYL NOT = ZEROS
070115        IF NETONLYI = ' ' OR 'Y' OR 'N'
070115           CONTINUE
070115        ELSE
070115           MOVE -1               TO NETONLYL
070115           MOVE AL-UABON         TO NETONLYA
070115           MOVE ER-9999          TO EMI-ERROR
070115           PERFORM 9900-ERROR-FORMAT
070115                                 THRU 9900-EXIT
070115        END-IF
070115     END-IF

102717     IF CCREQL NOT = ZEROS
102717        IF CCREQI = ' ' OR 'Y' OR 'N'
102717           CONTINUE
102717        ELSE
102717           MOVE -1               TO CCREQL
102717           MOVE AL-UABON         TO CCREQA
102717           MOVE ER-3064          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        END-IF
102717     END-IF

01250      IF REFREJL NOT = ZEROS                                       
01251         IF REFREJI = SPACES OR '1'                                
01252            NEXT SENTENCE                                          
01253           ELSE                                                    
01254            MOVE -1               TO REFREJL                       
01255            MOVE AL-UABON         TO REFREJA                       
01256            MOVE ER-2028          TO EMI-ERROR                     
01257            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01258                                                                   
01259      IF REFMINL NOT = ZEROS                                       
01260         EXEC CICS BIF                                             
01261              DEEDIT                                               
01262              FIELD (REFMINI)                                      
01263              LENGTH(6)                                            
01264         END-EXEC                                                  
01265         IF REFMINI NOT NUMERIC                                    
01266            MOVE -1               TO REFMINL                       
01267            MOVE AL-UNBON         TO REFMINA                       
01268            MOVE ER-3030          TO EMI-ERROR                     
01269            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01270                                                                   
01271      IF REFDAY1L NOT = ZERO                                       
01272         IF REFDAY1I LESS '00' OR GREATER '31'                     
01273             MOVE -1               TO REFDAY1L                     
01274             MOVE AL-UNBON         TO REFDAY1A                     
01275             MOVE ER-3031          TO EMI-ERROR                    
01276             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01277                                                                   
01278      IF REFDAYSL NOT = ZERO                                       
01279         IF REFDAYSI LESS '00' OR GREATER '31'                     
01280             MOVE -1               TO REFDAYSL                     
01281             MOVE AL-UNBON         TO REFDAYSA                     
01282             MOVE ER-3031          TO EMI-ERROR                    
01283             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01284                                                                   
01285      IF SPLPMTL NOT = ZERO                                        
01286         IF (SPLPMTI IS EQUAL TO 'Y' OR 'N' OR ' ')                
01287             NEXT SENTENCE                                         
01288         ELSE                                                      
01289             MOVE -1               TO SPLPMTL                      
01290             MOVE AL-UABON         TO SPLPMTA                      
01291             MOVE ER-0805          TO EMI-ERROR                    
01292             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01293                                                                   
01294      IF EXTDAYSL NOT = ZERO                                       
01295         IF EXTDAYSI NOT NUMERIC                                   
01296             MOVE -1               TO EXTDAYSL                     
01297             MOVE AL-UNBON         TO EXTDAYSA                     
01298             MOVE ER-3032          TO EMI-ERROR                    
01299             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01300                                                                   
PEMMOD*    IF INTDAYSL NOT = ZERO                                       
PEMMOD*       IF INTDAYSI NOT NUMERIC                                   
PEMMOD*           MOVE -1               TO INTDAYSL                     
PEMMOD*           MOVE AL-UNBON         TO INTDAYSA                     
PEMMOD*           MOVE ER-0141          TO EMI-ERROR                    
PEMMOD*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01307                                                                   
01308      IF EXTCHGL NOT = ZEROS                                       
01309         IF EXTCHGI = SPACES OR '1' OR '2' OR '3'                  
01310            NEXT SENTENCE                                          
01311           ELSE                                                    
01312            MOVE -1               TO EXTCHGL                       
01313            MOVE AL-UABON         TO EXTCHGA                       
01314            MOVE ER-3033          TO EMI-ERROR                     
01315            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01316                                                                   
CIDMOD     IF REMTERML NOT = ZEROS
CIDMOD        IF (REMTERMI = ' ') OR
CIDMOD           (REMTERMI > '0' AND < '8')
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE -1               TO REMTERML
CIDMOD           MOVE AL-UABON         TO REMTERMA
CIDMOD           MOVE ER-2298          TO EMI-ERROR
CIDMOD           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFREDL NOT = ZEROS
PEMMOD        IF LFREDI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFREDL
PEMMOD           MOVE AL-UABON         TO LFREDA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFLEVL NOT = ZEROS
PEMMOD        IF LFLEVI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFLEVL
PEMMOD           MOVE AL-UABON         TO LFLEVA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFNETL NOT = ZEROS
PEMMOD        IF LFNETI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFNETL
PEMMOD           MOVE AL-UABON         TO LFNETA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHAHL  NOT = ZEROS
PEMMOD        IF AHAHI  = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO AHAHL
PEMMOD           MOVE AL-UABON         TO AHAHA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHCPL  NOT = ZEROS
PEMMOD        IF AHCPI  = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO AHCPL
PEMMOD           MOVE AL-UABON         TO AHCPA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
01317      IF STUEL NOT = ZEROS                                         
01318         IF STUEI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'         
01319            NEXT SENTENCE                                          
01320           ELSE                                                    
01321            MOVE -1               TO STUEL                         
01322            MOVE AL-UABON         TO STUEA                         
01323            MOVE ER-3034          TO EMI-ERROR                     
01324            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01325                                                                   
01326      IF STCNTLL NOT = ZEROS                                       
01327         IF STCNTLI = SPACES                                       
01328            NEXT SENTENCE                                          
01329         ELSE                                                      
01330            IF STCNTLI = '1' OR '2' OR '3' OR '4' OR               
01331                         '5' OR '6' OR '7' OR '8' OR               
01332                         '9' OR 'A' OR 'B' OR 'X'                  
01333               NEXT SENTENCE                                       
01334            ELSE                                                   
01335               MOVE -1               TO STCNTLL                    
01336               MOVE AL-UABON         TO STCNTLA                    
01337               MOVE ER-3035          TO EMI-ERROR                  
01338               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
01339                                                                   
01340      IF  REPLAWL GREATER THAN ZEROS                               
01341          IF  REPLAWI = 'Y'                                        
01342              MOVE REPLAWI        TO REPLAWO                       
01343              MOVE AL-UANON       TO REPLAWA                       
01344          ELSE                                                     
01345              IF  REPLAWI = 'N' OR SPACES OR LOW-VALUES            
01346                  MOVE 'N'        TO REPLAWO                       
01347                  MOVE AL-UANON   TO REPLAWA                       
01348              ELSE                                                 
01349                  MOVE ER-9074    TO EMI-ERROR                     
01350                  MOVE -1         TO REPLAWL                       
01351                  MOVE AL-UABON   TO REPLAWA                       
01352                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
01353                                                                   
01354      IF  REPLETRL GREATER THAN +0                                 
01355          IF  REPLAWI = 'Y'                                        
01356              IF  REPLETRI GREATER THAN SPACES                     
01357                  MOVE SPACES     TO W-WORKING-TEXT-KEY            
01358                  MOVE PI-COMPANY-CD                               
01359                                  TO W-TEXT-COMPANY-CD             
01360                  MOVE REPLETRI   TO W-TEXT-FORM-NO                
01361                  MOVE +1         TO W-TEXT-LINE-SEQ               
01362                  EXEC CICS HANDLE CONDITION                       
01363                      NOTFND   (6010-LETR-NOT-FOUND)               
01364                      ENDFILE  (6010-LETR-NOT-FOUND)               
01365                      NOTOPEN  (8890-LETR-NOT-OPEN)                
01366                  END-EXEC                                         
01367                  EXEC CICS READ                                   
01368                      SET     (ADDRESS OF TEXT-FILES)              
01369                      DATASET (ELLETR-ID)                          
01370                      RIDFLD  (W-WORKING-TEXT-KEY)                 
01371                  END-EXEC                                         
01372                  MOVE AL-UANON   TO REPLETRA                      
01373              ELSE                                                 
01374                  MOVE ER-9448    TO EMI-ERROR                     
01375                  MOVE -1         TO REPLETRL                      
01376                  MOVE AL-UABON   TO REPLETRA                      
01377                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01378          ELSE                                                     
01379              IF  REPLETRI EQUAL SPACES OR LOW-VALUES              
01380                  MOVE LOW-VALUES TO REPLETRO                      
01381                  MOVE AL-UANON   TO REPLETRA                      
01382              ELSE                                                 
01383                  MOVE ER-9478    TO EMI-ERROR                     
01384                  MOVE -1         TO REPLAWL                       
01385                  MOVE AL-UABON   TO REPLAWA                       
01386                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
01387      ELSE                                                         
01388          IF  REPLAWI = 'Y'                                        
01389              MOVE ER-9448        TO EMI-ERROR                     
01390              MOVE -1             TO REPLETRL                      
01391              MOVE AL-UABON       TO REPLETRA                      
01392              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01407                                                                   
01408 *    IF STATIL GREATER ZEROS                                      
01409 *        IF STATII = 'I' OR 'R'                                   
01410 *            MOVE STATII         TO STATIO                        
01411 *            MOVE AL-UANON       TO STATIA                        
01412 *         ELSE                                                    
01413 *            IF STATII = SPACES OR LOW-VALUES                     
01414 *                MOVE ' '        TO STATIO                        
01415 *                MOVE AL-UANON   TO STATIA                        
01416 *        ELSE                                                     
01417 *            MOVE ER-7346    TO EMI-ERROR                         
01418 *            MOVE -1         TO STATIL                            
01419 *            MOVE AL-UABON   TO STATIA                            
01420 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            

01486      IF RESTAXL GREATER ZEROS                                     
01487         EXEC CICS BIF                                             
01488              DEEDIT                                               
01489              FIELD (RESTAXI)                                      
01490              LENGTH(6)                                            
01491         END-EXEC                                                  
01492       IF RESTAXI NUMERIC                                          
01493             MOVE AL-UNNON        TO RESTAXA                       
01494             MOVE RESTAXI         TO WS-RESIDENT-TAX               
01495                                     RESTAXO                       
01496         ELSE                                                      
01497             MOVE ER-1614         TO EMI-ERROR                     
01498             MOVE -1              TO RESTAXL                       
01499             MOVE AL-UNBON        TO RESTAXA                       
01500             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01501                                                                   
01502      IF FREELKI  NUMERIC                                          
01503             MOVE AL-UNNON        TO FREELKA                       
01504             MOVE FREELKI         TO WS-FREE-LOOK-DAYS             
01505                                     FREELKO                       
01506         ELSE                                                      
01507             MOVE -1               TO FREELKL                      
01508             MOVE AL-UNBON         TO FREELKA                      
01509             MOVE ER-8159          TO EMI-ERROR                    
01510             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01511                                                                   
01512 *    IF STATRL GREATER ZEROS                                      
01513 *       EXEC CICS BIF                                             
01514 *            DEEDIT                                               
01515 *            FIELD (STATRI)                                       
01516 *            LENGTH(6)                                            
01517 *       END-EXEC                                                  
01518 *     IF STATRI NUMERIC                                           
01519 *           MOVE AL-UNNON        TO STATRA                        
01520 *           MOVE STATRI          TO WS-IRATE                      
01521 *                                   STATRO                        
01522 *       ELSE                                                      
01523 *           MOVE ER-1614         TO EMI-ERROR                     
01524 *           MOVE -1              TO STATRL                        
01525 *           MOVE AL-UNBON        TO STATRA                        
01526 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01527                                                                   
01528 *    IF STATR1L GREATER ZEROS                                     
01529 *       EXEC CICS BIF                                             
01530 *            DEEDIT                                               
01531 *            FIELD (STATR1I)                                      
01532 *            LENGTH(6)                                            
01533 *       END-EXEC                                                  
01534 *     IF STATR1I NUMERIC                                          
01535 *           MOVE AL-UNNON        TO STATR1A                       
01536 *           MOVE STATR1I         TO WS-IRATE1                     
01537 *                                   STATR1O                       
01538 *       ELSE                                                      
01539 *           MOVE ER-1614         TO EMI-ERROR                     
01540 *           MOVE -1              TO STATR1L                       
01541 *           MOVE AL-UNBON        TO STATR1A                       
01542 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01543 *                                                                 
01544 *    IF STATR2L GREATER ZEROS                                     
01545 *       EXEC CICS BIF                                             
01546 *            DEEDIT                                               
01547 *            FIELD (STATR2I)                                      
01548 *            LENGTH(6)                                            
01549 *       END-EXEC                                                  
01550 *     IF STATR2I NUMERIC                                          
01551 *           MOVE AL-UNNON        TO STATR2A                       
01552 *           MOVE STATR2I         TO WS-IRATE2                     
01553 *                                   STATR2O                       
01554 *       ELSE                                                      
01555 *           MOVE ER-1614         TO EMI-ERROR                     
01556 *           MOVE -1              TO STATR2L                       
01557 *           MOVE AL-UNBON        TO STATR2A                       
01558 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01559 *                                                                 
01560 *    IF STATR3L GREATER ZEROS                                     
01561 *       EXEC CICS BIF                                             
01562 *            DEEDIT                                               
01563 *            FIELD (STATR3I)                                      
01564 *            LENGTH(6)                                            
01565 *       END-EXEC                                                  
01566 *     IF STATR3I NUMERIC                                          
01567 *           MOVE AL-UNNON        TO STATR3A                       
01568 *           MOVE STATR3I         TO WS-IRATE3                     
01569 *                                   STATR3O                       
01570 *       ELSE                                                      
01571 *           MOVE ER-1614         TO EMI-ERROR                     
01572 *           MOVE -1              TO STATR3L                       
01573 *           MOVE AL-UNBON        TO STATR3A                       
01574 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01575                                                                   
01576      GO TO 6020-CONTINUE.                                         
01577                                                                   
01578  6010-LETR-NOT-FOUND.                                             
01579      MOVE ER-9447                TO EMI-ERROR.                    
01580      MOVE -1                     TO REPLETRL.                     
01581      MOVE AL-UABON               TO REPLETRA.                     
01582      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01583                                                                   
01584  6020-CONTINUE.                                                   
01585                                                                   
01586      IF  TARRATL NOT = ZEROS                                      
01587          EXEC CICS BIF                                            
01588               DEEDIT                                              
01589               FIELD (TARRATI)                                     
01590               LENGTH (6)                                          
01591          END-EXEC                                                 
01592                                                                   
01593          IF  TARRATI NUMERIC                                      
01594              MOVE AL-UNNON       TO TARRATA                       
01595              MOVE TARRATI        TO WS-ST-TARGET-LOSS-RATIO       
01596                                     TARRATO                       
01597          ELSE                                                     
01598              MOVE ER-7717        TO EMI-ERROR                     
01599              MOVE -1             TO TARRATL                       
01600              MOVE AL-UNBON       TO TARRATA                       
01601              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01602                                                                   
01603      IF  CALCINTL NOT = ZEROS                                     
01604          EXEC CICS BIF                                            
01605               DEEDIT                                              
01606               FIELD (CALCINTI)                                    
01607               LENGTH (6)                                          
01608          END-EXEC                                                 
01609                                                                   
01610          IF  CALCINTI NUMERIC                                     
01611              MOVE AL-UNNON       TO CALCINTA                      
01612              MOVE CALCINTI       TO WS-ST-CALC-INTEREST           
01613                                     CALCINTO                      
01614          ELSE                                                     
01615              MOVE ER-7735        TO EMI-ERROR                     
01616              MOVE -1             TO CALCINTL                      
01617              MOVE AL-UNBON       TO CALCINTA                      
01618              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
01619                                                                   
01620      IF PRMPCTL NOT = ZEROS                                       
01621         EXEC CICS BIF                                             
01622              DEEDIT                                               
01623              FIELD (PRMPCTI)                                      
01624              LENGTH(5)                                            
01625         END-EXEC                                                  
01626       IF PRMPCTI NUMERIC                                          
01627             MOVE AL-UNNON        TO PRMPCTA                       
01628             MOVE PRMPCTI         TO WS-ST-TOL-PREM-PCT            
01629         ELSE                                                      
01630             MOVE ER-7532         TO EMI-ERROR                     
01631             MOVE -1              TO PRMPCTL                       
01632             MOVE AL-UNBON        TO PRMPCTA                       
01633             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01634                                                                   
01635      IF OVSPCTL NOT = ZEROS                                       
01636         EXEC CICS BIF                                             
01637              DEEDIT                                               
01638              FIELD (OVSPCTI)                                      
01639              LENGTH(5)                                            
01640         END-EXEC                                                  
01641       IF OVSPCTI NUMERIC                                          
01642             MOVE AL-UNNON        TO OVSPCTA                       
01643             MOVE OVSPCTI         TO WS-ST-OVR-SHT-PCT             
01644         ELSE                                                      
01645             MOVE ER-7532         TO EMI-ERROR                     
01646             MOVE -1              TO OVSPCTL                       
01647             MOVE AL-UNBON        TO OVSPCTA                       
01648             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01649                                                                   
01650      IF REFPCTL NOT = ZEROS                                       
01651         EXEC CICS BIF                                             
01652              DEEDIT                                               
01653              FIELD (REFPCTI)                                      
01654              LENGTH(5)                                            
01655         END-EXEC                                                  
01656       IF REFPCTI NUMERIC                                          
01657             MOVE AL-UNNON        TO REFPCTA                       
01658             MOVE REFPCTI         TO WS-ST-TOL-REF-PCT             
01659         ELSE                                                      
01660             MOVE ER-7532         TO EMI-ERROR                     
01661             MOVE -1              TO REFPCTL                       
01662             MOVE AL-UNBON        TO REFPCTA                       
01663             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
01664                                                                   
PEMMOD     IF LFTAXL NOT = ZEROS
PEMMOD        EXEC CICS BIF
PEMMOD             DEEDIT
PEMMOD             FIELD (LFTAXI)
PEMMOD             LENGTH(5)
PEMMOD        END-EXEC
PEMMOD        IF LFTAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO LFTAXA
PEMMOD           MOVE LFTAXI           TO WS-ST-LF-PREM-TAX
PEMMOD        ELSE
PEMMOD           MOVE ER-2082          TO EMI-ERROR
PEMMOD           MOVE -1               TO LFTAXL
PEMMOD           MOVE AL-UNBON         TO LFTAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHITAXL NOT = ZEROS
PEMMOD        EXEC CICS BIF
PEMMOD             DEEDIT
PEMMOD             FIELD (AHITAXI)
PEMMOD             LENGTH(5)
PEMMOD        END-EXEC
PEMMOD        IF AHITAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO AHITAXA
PEMMOD           MOVE AHITAXI          TO WS-ST-AH-PREM-TAX-I
PEMMOD        ELSE
PEMMOD           MOVE ER-2084          TO EMI-ERROR
PEMMOD           MOVE -1               TO AHITAXL
PEMMOD           MOVE AL-UNBON         TO AHITAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHGTAXL NOT = ZEROS
PEMMOD        EXEC CICS BIF
PEMMOD             DEEDIT
PEMMOD             FIELD (AHGTAXI)
PEMMOD             LENGTH(5)
PEMMOD        END-EXEC
PEMMOD        IF AHGTAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO AHGTAXA
PEMMOD           MOVE AHGTAXI          TO WS-ST-AH-PREM-TAX-G
PEMMOD        ELSE
PEMMOD           MOVE ER-2084          TO EMI-ERROR
PEMMOD           MOVE -1               TO AHGTAXL
PEMMOD           MOVE AL-UNBON         TO AHGTAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
011410
011410     IF REFCLML NOT = ZEROS                                       
011211        IF REFCLMI = SPACES OR '1' OR '2' OR '3' OR '4' OR '5'                  
011410           NEXT SENTENCE                                          
011410        ELSE                                                    
011410           MOVE -1               TO REFCLML                       
011410           MOVE AL-UABON         TO REFCLMA                       
011410           MOVE ER-3036          TO EMI-ERROR                     
011410           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011410        END-IF
011410     END-IF.
011410                                                                   
061511     IF VFYBENEL NOT = ZEROS
032514        IF VFYBENEI = SPACES OR 'A' OR 'L' OR 'B'
061511           NEXT SENTENCE
061511        ELSE
061511           MOVE -1               TO VFYBENEL
061511           MOVE AL-UABON         TO VFYBENEA
032514           MOVE ER-7581          TO EMI-ERROR
061511           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511        END-IF
061511     END-IF.
061511
012913     IF CAUSALL NOT = ZEROS
012913        IF CAUSALI = SPACES OR 'A' OR 'L' OR 'B'
012913           NEXT SENTENCE
012913        ELSE
012913           MOVE -1               TO CAUSALL
012913           MOVE AL-UABON         TO CAUSALA
012913           MOVE ER-7578          TO EMI-ERROR
012913           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
012913        END-IF
012913     END-IF.
012913
PEMMOD     .                                                            
01665  6099-EXIT.                                                       
01666      EXIT.                                                        
01667                                                                   
01668      EJECT                                                        
01669  6100-INITIALIZE-BENEFIT-CNTL.                                    
01670                                                                   
01671      MOVE SPACES   TO   CF-ST-BENEFIT-CD         (SUB)            
01672                         CF-ST-BENEFIT-KIND       (SUB)            
01673                         CF-ST-REM-TERM-CALC      (SUB)            
01674                         CF-ST-REFUND-CALC        (SUB)            
01675                         CF-ST-EARNING-CALC       (SUB)            
01676                         CF-ST-OVRD-EARNINGS-CALC (SUB).           
01677                                                                   
01678  6199-EXIT.                                                       
01679      EXIT.                                                        
01680                                                                   
01681      EJECT                                                        
01682  6400-CREATE-TEMP-STORAGE.                                        
01683      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.             
01684                                                                   
01685      EXEC CICS WRITEQ TS                                          
01686          QUEUE   (QID)                                            
01687          FROM    (EL106AO)                                        
01688          LENGTH  (WS-MAP-LENGTH)                                  
01689      END-EXEC.                                                    
01690                                                                   
01691  6400-EXIT.                                                       
01692       EXIT.                                                       
01693                                                                   
01694  6500-RECOVER-TEMP-STORAGE.                                       
01695      EXEC CICS READQ TS                                           
01696          QUEUE   (QID)                                            
01697          INTO    (EL106AO)                                        
01698          LENGTH  (WS-MAP-LENGTH)                                  
01699      END-EXEC.                                                    
01700                                                                   
01701      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.             
01702                                                                   
01703  6500-EXIT.                                                       
01704       EXIT.                                                       
01705                                                                   
01706  6600-DELETE-TEMP-STORAGE.                                        
01707      EXEC CICS HANDLE CONDITION                                   
01708          QIDERR  (6600-EXIT)                                      
01709      END-EXEC.                                                    
01710                                                                   
01711      EXEC CICS DELETEQ TS                                         
01712          QUEUE  (QID)                                             
01713      END-EXEC.                                                    
01714                                                                   
01715  6600-EXIT.                                                       
01716       EXIT.                                                       
01717                                                                   
01718      EJECT                                                        
01719 ***************************************************************   
01720 *                                                             *   
01721 *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *   
01722 *                                                             *   
01723 ***************************************************************   
01724  7000-BUILD-OUTPUT-MAP.                                           
01725      MOVE LOW-VALUES            TO EL106AO.                       
01726      MOVE CF-STATE-CODE         TO STCDO                          
01727                                    PI-WS-STATE.                   
01728      MOVE CF-ST-TOL-CLAIM       TO QUOTCALO.                      
01729      MOVE CF-ST-TOL-PREM        TO PREMTOLO.                      
01730      MOVE CF-ST-TOL-REFUND      TO REFTOLO.                       
01731      MOVE CF-ST-CLAIM-REJECT-SW TO CLREJECO.                      
01732      MOVE CF-ST-PREM-REJECT-SW  TO ISSREJO.                       

040915     if cf-st-agent-sig-edit = 'Y'
040915        move 'Y'                to agtsigo
040915     else
040915        move 'N'                to agtsigo
040915     end-if

070115     if cf-st-net-only-state = 'Y'
070115        move 'Y'                to netonlyo
070115     else
070115        move 'N'                to netonlyo
070115     end-if
102717     if cf-commission-cap-required = 'Y'
102717        move 'Y'                to ccreqo
102717     else
102717        move 'N'                to ccreqo
102717     end-if
01733      MOVE CF-ST-REF-REJECT-SW   TO REFREJO.                       
01734      MOVE CF-ST-FST-PMT-DAYS-CHG                                  
01735                                 TO EXTCHGO.                       
CIDMOD     MOVE CF-ST-RT-CALC         TO REMTERMO
PEMMOD     MOVE CF-ST-RF-LR-CALC      TO LFREDO
PEMMOD     MOVE CF-ST-RF-LL-CALC      TO LFLEVO
PEMMOD     MOVE CF-ST-RF-LN-CALC      TO LFNETO
PEMMOD     MOVE CF-ST-RF-AH-CALC      TO AHAHO
PEMMOD     MOVE CF-ST-RF-CP-CALC      TO AHCPO
01736      MOVE CF-ST-CALL-UNEARNED   TO STUEO.                         
01737      MOVE CF-ST-CALL-RPT-CNTL   TO STCNTLO.                       
01738      MOVE CF-ST-CALL-RATE-DEV   TO STDEVO.                        
01739      MOVE CF-STATE-ABBREVIATION TO STABRO.                        
01740      MOVE CF-STATE-NAME         TO STNAMEO.                       
011410     MOVE CF-ST-REF-AH-DEATH-IND TO REFCLMO.
061511     MOVE CF-ST-VFY-2ND-BENE    TO VFYBENEO.
012913     MOVE CF-ST-CAUSAL-STATE    TO CAUSALO.
100108     IF  CF-ST-CHECK-COUNTER NUMERIC
100108        MOVE CF-ST-CHECK-COUNTER TO STCHKNOO
100108     ELSE
100108        MOVE ZEROS             TO STCHKNOO
100108     END-IF.

022415     IF CF-ST-EXTRA-INTEREST-PERIODS NOT NUMERIC
022415        MOVE ZEROS               TO CF-ST-EXTRA-INTEREST-PERIODS
022415     END-IF
022415     IF CF-ST-EXTRA-PAYMENTS NOT NUMERIC
022415        MOVE ZEROS               TO CF-ST-EXTRA-PAYMENTS
022415     END-IF
           MOVE CF-ST-EXTRA-INTEREST-PERIODS
                                       TO XINTO
           MOVE CF-ST-EXTRA-PAYMENTS   TO XPMTSO
01742      IF CF-ST-LF-EXP-PCT      NUMERIC                             
01743          IF CF-ST-LF-EXP-PCT NOT = ZEROS                          
01744              MOVE CF-ST-LF-EXP-PCT  TO LFEXPO.                    
01745                                                                   
01746      IF CF-ST-AH-EXP-PCT      NUMERIC                             
01747          IF CF-ST-AH-EXP-PCT NOT = ZEROS                          
01748              MOVE CF-ST-AH-EXP-PCT  TO AHEXPO.                    
01749                                                                   
01750      IF CF-ST-TOL-PREM-PCT    NUMERIC                             
01751          IF CF-ST-TOL-PREM-PCT NOT = ZEROS                        
01752              MOVE CF-ST-TOL-PREM-PCT TO PRMPCTO.                  
01753                                                                   
01754      IF CF-ST-OVR-SHT-AMT NUMERIC                                 
01755         IF CF-ST-OVR-SHT-AMT > ZEROS                              
01756            MOVE CF-ST-OVR-SHT-AMT TO OVSAMTO                      
01757         END-IF                                                    
01758      END-IF.                                                      
01759                                                                   
01760      IF CF-ST-OVR-SHT-PCT NUMERIC                                 
01761          IF CF-ST-OVR-SHT-PCT > ZEROS                             
01762              MOVE CF-ST-OVR-SHT-PCT TO OVSPCTO                    
01763          END-IF                                                   
01764      END-IF.                                                      
01765                                                                   
01766      IF CF-ST-TOL-REF-PCT     NUMERIC                             
01767          IF CF-ST-TOL-REF-PCT NOT = ZEROS                         
01768              MOVE CF-ST-TOL-REF-PCT TO REFPCTO.                   
01769                                                                   
01770      IF CF-ST-REFUND-MIN      NUMERIC                             
01771          IF CF-ST-REFUND-MIN NOT = ZEROS                          
01772              MOVE CF-ST-REFUND-MIN  TO REFMINO.                   
01773                                                                   
01774      IF CF-ST-REFUND-DAYS-FIRST NUMERIC                           
01775        IF CF-ST-REFUND-DAYS-FIRST NOT = ZEROS                     
01776          MOVE CF-ST-REFUND-DAYS-FIRST                             
01777                                 TO REFDAY1O.                      
01778                                                                   
01779      IF CF-ST-REFUND-DAYS-SUBSEQ NUMERIC                          
01780        IF CF-ST-REFUND-DAYS-SUBSEQ NOT = ZEROS                    
01781          MOVE CF-ST-REFUND-DAYS-SUBSEQ                            
01782                                 TO REFDAYSO.                      
01783                                                                   
01784      IF CF-ST-SPLIT-PAYMENT IS EQUAL TO ' '                       
01785          MOVE 'N'               TO SPLPMTO                        
01786      ELSE                                                         
01787          MOVE CF-ST-SPLIT-PAYMENT                                 
01788                                 TO SPLPMTO.                       
01789                                                                   
01790      IF CF-ST-FST-PMT-DAYS-MAX NUMERIC                            
01791        IF CF-ST-FST-PMT-DAYS-MAX NOT = ZEROS                      
01792          MOVE CF-ST-FST-PMT-DAYS-MAX                              
01793                                 TO EXTDAYSO.                      
01794                                                                   
PEMMOD     IF CF-ST-LF-PREM-TAX NUMERIC                                 
PEMMOD        IF CF-ST-LF-PREM-TAX > ZEROS                              
PEMMOD           MOVE CF-ST-LF-PREM-TAX                                 
PEMMOD                                 TO LFTAXO                        
PEMMOD        END-IF                                                    
PEMMOD     END-IF                                                       
PEMMOD                                                                  
PEMMOD     IF CF-ST-AH-PREM-TAX-I NUMERIC                               
PEMMOD        IF CF-ST-AH-PREM-TAX-I > ZEROS                            
PEMMOD           MOVE CF-ST-AH-PREM-TAX-I                               
PEMMOD                                 TO AHITAXO                       
PEMMOD        END-IF                                                    
PEMMOD     END-IF                                                       
PEMMOD                                                                  
PEMMOD     IF CF-ST-AH-PREM-TAX-G NUMERIC                               
PEMMOD        IF CF-ST-AH-PREM-TAX-G > ZEROS                            
PEMMOD           MOVE CF-ST-AH-PREM-TAX-G                               
PEMMOD                                 TO AHGTAXO                       
PEMMOD        END-IF                                                    
PEMMOD     END-IF                                                       
PEMMOD                                                                  
PEMMOD*    IF PI-COMPANY-ID = 'NCL'                                     
PEMMOD*    IF CF-ST-NO-DAYS-ELAPSED NUMERIC                             
PEMMOD*      IF CF-ST-NO-DAYS-ELAPSED NOT = ZEROS                       
PEMMOD*        MOVE CF-ST-NO-DAYS-ELAPSED                               
PEMMOD*                               TO INTDAYSO.                      
01800                                                                   
01801      IF  CF-REPLACEMENT-LAW-SW NOT EQUAL 'Y'                      
01802              AND                                                  
01803          CF-REPLACEMENT-LAW-SW NOT EQUAL 'N'                      
01804          MOVE 'N'               TO REPLAWO                        
01805          MOVE LOW-VALUES        TO REPLETRO                       
01806      ELSE                                                         
01807          MOVE CF-REPLACEMENT-LAW-SW                               
01808                                 TO REPLAWO                        
01809                                                                   
01810          IF  CF-REPLACEMENT-LETTER GREATER THAN SPACES            
01811              MOVE CF-REPLACEMENT-LETTER                           
01812                                 TO REPLETRO                       
01813          ELSE                                                     
01814              MOVE LOW-VALUES    TO REPLETRO.                      
01815                                                                   
01816      IF  CF-ST-TARGET-LOSS-RATIO NUMERIC                          
01817          IF  CF-ST-TARGET-LOSS-RATIO NOT = ZEROS                  
01818              MOVE CF-ST-TARGET-LOSS-RATIO                         
01819                                 TO TARRATO                        
01820          ELSE                                                     
01821              MOVE ZEROS         TO TARRATO                        
01822      ELSE                                                         
01823          MOVE ZEROS             TO TARRATO.                       
01824                                                                   
01825      IF  CF-ST-CALC-INTEREST NUMERIC                              
01826          IF  CF-ST-CALC-INTEREST NOT = ZEROS                      
01827              MOVE CF-ST-CALC-INTEREST                             
01828                                 TO CALCINTO                       
01829          ELSE                                                     
01830              MOVE ZEROS         TO CALCINTO                       
01831      ELSE                                                         
01832          MOVE ZEROS             TO CALCINTO.                      
01869                                                                   
01870      IF  CF-ST-RES-TAX-PCT NUMERIC                                
01871          IF  CF-ST-RES-TAX-PCT NOT = ZEROS                        
01872              MOVE CF-ST-RES-TAX-PCT                               
01873                                 TO RESTAXO                        
01874          ELSE                                                     
01875              MOVE ZEROS         TO RESTAXO                        
01876      ELSE                                                         
01877          MOVE ZEROS             TO RESTAXO.                       
01878                                                                   
01879      IF CF-ST-FREE-LOOK-PERIOD NUMERIC                            
01880         IF CF-ST-FREE-LOOK-PERIOD NOT = ZEROS                     
01881              MOVE CF-ST-FREE-LOOK-PERIOD                          
01882                                 TO FREELKO                        
01883         ELSE                                                      
01884              MOVE ZEROS         TO FREELKO                        
01885      ELSE                                                         
01886          MOVE ZEROS             TO FREELKO.                       
01887                                                                   
01888 *    IF PI-COMPANY-ID = 'NCL'                                     
01889 *    IF  CF-ST-STAT-INTEREST NUMERIC                              
01890 *        IF  CF-ST-STAT-INTEREST NOT = ZEROS                      
01891 *            MOVE CF-ST-STAT-INTEREST                             
01892 *                               TO STATRO                         
01893 *        ELSE                                                     
01894 *            MOVE ZEROS         TO STATRO                         
01895 *    ELSE                                                         
01896 *        MOVE ZEROS             TO STATRO.                        
01897                                                                   
01898 *    IF PI-COMPANY-ID = 'NCL'                                     
01899 *    IF  CF-ST-STAT-INTEREST-1 NUMERIC                            
01900 *        IF  CF-ST-STAT-INTEREST-1 NOT = ZEROS                    
01901 *            MOVE CF-ST-STAT-INTEREST-1                           
01902 *                               TO STATR1O                        
01903 *        ELSE                                                     
01904 *            MOVE ZEROS         TO STATR1O                        
01905 *    ELSE                                                         
01906 *        MOVE ZEROS             TO STATR1O.                       
01907                                                                   
01908 *    IF PI-COMPANY-ID = 'NCL'                                     
01909 *    IF  CF-ST-STAT-INTEREST-2 NUMERIC                            
01910 *        IF  CF-ST-STAT-INTEREST-2 NOT = ZEROS                    
01911 *            MOVE CF-ST-STAT-INTEREST-2                           
01912 *                               TO STATR2O                        
01913 *        ELSE                                                     
01914 *            MOVE ZEROS         TO STATR2O                        
01915 *    ELSE                                                         
01916 *        MOVE ZEROS             TO STATR2O.                       
01917                                                                   
01918 *    IF PI-COMPANY-ID = 'NCL'                                     
01919 *    IF  CF-ST-STAT-INTEREST-3 NUMERIC                            
01920 *        IF  CF-ST-STAT-INTEREST-3 NOT = ZEROS                    
01921 *            MOVE CF-ST-STAT-INTEREST-3                           
01922 *                               TO STATR3O                        
01923 *        ELSE                                                     
01924 *            MOVE ZEROS         TO STATR3O                        
01925 *    ELSE                                                         
01926 *        MOVE ZEROS             TO STATR3O.                       
01927                                                                   
PEMMOD*    MOVE CF-ST-STAT-DATE-FROM  TO STATIO.                        
01930                                                                   
01931      MOVE CF-LAST-MAINT-BY      TO LSTUSRO.                       
01932      MOVE ' '                   TO DC-OPTION-CODE.                
01933      MOVE CF-LAST-MAINT-DT      TO DC-BIN-DATE-1.                 
01934      MOVE LINK-ELDATCV          TO PGM-NAME.                      
01935      EXEC CICS LINK                                               
01936          PROGRAM (PGM-NAME)                                       
01937          COMMAREA(DATE-CONVERSION-DATA)                           
01938          LENGTH  (DC-COMM-LENGTH)                                 
01939      END-EXEC.                                                    
01940                                                                   
01941      IF DATE-CONVERSION-ERROR                                     
01942          MOVE ZEROS              TO LSTDTEO                       
01943      ELSE                                                         
01944          MOVE DC-GREG-DATE-1-EDIT                                 
01945                                  TO LSTDTEO.                      
01946                                                                   
01947      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN.                      
01948      MOVE TIME-OUT               TO LSTTIMEO.                     
01949      MOVE -1                     TO MAINTL.                       
01950      MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.                 
01951      MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             
01952      MOVE CF-STATE-CODE          TO PI-PREV-STATE.                
01953      MOVE AL-UANOF               TO MAINTA.                       
01954      MOVE AL-UANON               TO STCDA                         
01955                                   STNAMEA                         
01956                                   STABRA                          
01957                                   CLREJECA ISSREJA REFREJA
040915                                  AGTSIGA netonlya
01958                                   EXTCHGA STUEA STCNTLA STDEVA    
CIDMOD                                  REMTERMA CCREQA
PEMMOD                                  LFREDA LFLEVA LFNETA
022415                                  XINTA  XPMTSA
PEMMOD                                  AHAHA  AHCPA
01959                                   REPLAWA REPLETRA SPLPMTA.

PEMMOD*    IF PI-COMPANY-ID = 'NCL'                                     
PEMMOD*        MOVE AL-UANON           TO STATIA.                       
01963                                                                   
01964      MOVE AL-UNNON               TO LFEXPA AHEXPA                 
01965                                   QUOTCALA PREMTOLA REFTOLA       
01966                                   OVSAMTA  OVSPCTA                
PEMMOD                                  LFTAXA   AHITAXA  AHGTAXA
01967                                   PRMPCTA  REFPCTA                
01968                                   REFDAY1A REFDAYSA               
01969                                   REFMINA EXTDAYSA                
01970                                   TARRATA                         
01971                                   CALCINTA                        
01972                                   RESTAXA FREELKA.
01974                                                                   
PEMMOD*    IF PI-COMPANY-ID = 'NCL'                                     
PEMMOD*        MOVE AL-UNNON           TO STATRA STATR1A STATR2A        
PEMMOD*                                                  STATR3A        
PEMMOD*                                                  INTDAYSA.      
01979                                                                   
01980      IF RETURN-DISPLAY                                            
01981          GO TO 8100-SEND-INITIAL-MAP                              
01982      ELSE                                                         
01983          GO TO 8200-SEND-DATAONLY.                                
01984                                                                   
01985      EJECT                                                        
01986  8100-SEND-INITIAL-MAP.                                           
031512     IF (PI-COMPANY-ID = 'CID' OR 'AHL')
031512       AND PI-WS-STATE = 'AK'
100108        MOVE AL-SANON TO STCHKHDA STCHKNOA
100108     ELSE
100108        MOVE AL-SADOF TO STCHKHDA STCHKNOA
100108     END-IF.
01987      MOVE SAVE-DATE              TO RUNDTEO.                      
01988      MOVE EIBTIME                TO TIME-IN.                      
01989      MOVE TIME-OUT               TO RUNTIMEO.                     
01990      MOVE -1                     TO MAINTL.                       
01991      MOVE PI-LIFE-OVERRIDE-L6    TO LEXPLBLO.                     
01992      MOVE PI-AH-OVERRIDE-L6      TO AEXPLBLO.                     
01993      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
01994      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     
01995                                                                   
PEMMOD*    IF PI-COMPANY-ID = 'NCL'                                     
PEMMOD*       MOVE AL-SANON            TO STATZA                        
01998 *                                   INTDAYZA                      
01999 *                                   STATRZA                       
02000 *                                   RNG1A                         
02001 *                                   RNG2A                         
02002 *                                   RNG3A                         
02003 *       MOVE AL-UANON            TO STATIA                        
02004 *                                   STATRA                        
02005 *                                   STATR1A                       
02006 *                                   STATR2A                       
02007 *                                   STATR3A                       
PEMMOD*       MOVE AL-UNNON            TO INTDAYSA.                     
02009                                                                   
02010      EXEC CICS SEND                                               
02011          MAP   (MAP-NAME)                                         
02012          MAPSET(MAPSET-NAME)                                      
02013          FROM  (EL106AO)                                          
02014          ERASE                                                    
02015          CURSOR                                                   
02016      END-EXEC.                                                    
02017                                                                   
02018      GO TO 9100-RETURN-TRAN.                                      
02019      EJECT                                                        
02020  8200-SEND-DATAONLY.                                              
100108                
031512     IF (PI-COMPANY-ID = 'CID' OR 'AHL')
031512       AND PI-WS-STATE = 'AK'
100108        MOVE AL-SANON TO STCHKHDA STCHKNOA
100108     ELSE
100108        MOVE AL-SADOF TO STCHKHDA STCHKNOA
100108     END-IF.
100108
02021      MOVE SAVE-DATE      TO RUNDTEO.                              
02022      MOVE EIBTIME        TO TIME-IN.                              
02023      MOVE TIME-OUT       TO RUNTIMEO.                             
02024                                                                   
02025      IF QUOTCALL NOT = ZEROS                                      
02026         IF QUOTCALI NUMERIC                                       
02027            MOVE QUOTCALI TO QUOTCALO.                             
02028                                                                   
02029      IF PREMTOLL NOT = ZEROS                                      
02030         IF PREMTOLI NUMERIC                                       
02031            MOVE PREMTOLI TO PREMTOLO.                             
02032                                                                   
02033      IF OVSAMTL NOT = ZEROS                                       
02034         IF OVSAMTI NUMERIC                                        
02035            MOVE OVSAMTI TO OVSAMTO.                               
02036                                                                   
02037      IF REFTOLL NOT = ZEROS                                       
02038         IF REFTOLI NUMERIC                                        
02039            MOVE REFTOLI TO REFTOLO.                               
02040                                                                   
02041      IF PRMPCTL NOT = ZEROS                                       
02042         IF PRMPCTI NUMERIC                                        
02043            MOVE PRMPCTI TO PRMPCTO.                               
02044                                                                   
02045      IF OVSPCTL NOT = ZEROS                                       
02046         IF OVSPCTI NUMERIC                                        
02047            MOVE OVSPCTI TO OVSPCTO.                               
02048                                                                   
PEMMOD     IF LFTAXL NOT = ZEROS                                        
PEMMOD        IF LFTAXI NUMERIC                                         
PEMMOD           MOVE LFTAXI           TO LFTAXO                        
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHITAXL NOT = ZEROS                                       
PEMMOD        IF AHITAXI NUMERIC                                        
PEMMOD           MOVE AHITAXI          TO AHITAXO                       
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD                                                                  
PEMMOD     IF AHGTAXL NOT = ZEROS                                       
PEMMOD        IF AHGTAXI NUMERIC                                        
PEMMOD           MOVE AHGTAXI          TO AHGTAXO                       
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD                                                                  
02049      IF REFPCTL NOT = ZEROS                                       
02050         IF REFPCTI NUMERIC                                        
02051            MOVE REFPCTI TO REFPCTO.                               
02052                                                                   
02053      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
02054      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     
02055                                                                   
02056      EXEC CICS SEND                                               
02057          MAP   (MAP-NAME)                                         
02058          MAPSET(MAPSET-NAME)                                      
02059          FROM  (EL106AO)                                          
02060          DATAONLY                                                 
02061          ERASEAUP                                                 
02062          CURSOR                                                   
02063      END-EXEC.                                                    
02064                                                                   
02065      GO TO 9100-RETURN-TRAN.                                      
02066      EJECT                                                        
02067  8300-SEND-TEXT.                                                  
02068      EXEC CICS SEND TEXT                                          
02069          FROM  (LOGOFF-TEXT)                                      
02070          LENGTH(LOGOFF-LENGTH)                                    
02071          ERASE                                                    
02072          FREEKB                                                   
02073      END-EXEC.                                                    
02074                                                                   
02075      EXEC CICS RETURN                                             
02076      END-EXEC.                                                    
02077                                                                   
02078      EJECT                                                        
02079  8400-LOG-JOURNAL-RECORD.                                         
02080      IF PI-JOURNAL-FILE-ID = 0                                    
02081          GO TO 8400-EXIT.                                         
02082                                                                   
02083      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   
02084      MOVE ELCNTL-ID              TO JP-FILE-ID.                   
02085      MOVE THIS-PGM               TO JP-PROGRAM-ID.                
pemuni*    EXEC CICS JOURNAL
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)
pemuni*         JTYPEID('EL')
pemuni*         FROM   (JOURNAL-RECORD)
pemuni*         LENGTH (773)
pemuni*    END-EXEC.
02092                                                                   
02093  8400-EXIT.                                                       
02094      EXIT.                                                        
02095                                                                   
02096  8800-UNAUTHORIZED-ACCESS.                                        
02097      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             
02098      GO TO 8300-SEND-TEXT.                                        
02099                                                                   
02100  8810-PF23.                                                       
02101      MOVE EIBAID   TO PI-ENTRY-CD-1.                              
02102      MOVE XCTL-EL005 TO PGM-NAME.                                 
02103      GO TO 9300-XCTL.                                             
02104                                                                   
02105  8850-DUPREC.                                                     
02106      MOVE ER-0147 TO EMI-ERROR.                                   
02107      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02108      MOVE -1       TO STCDL.                                      
02109      MOVE AL-UABON TO STCDA.                                      
02110      GO TO 8200-SEND-DATAONLY.                                    
02111                                                                   
02112  8860-ENDFILE.                                                    
02113      MOVE LOW-VALUES TO EL106AO.                                  
02114      MOVE ER-0148    TO EMI-ERROR.                                
02115      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02116      MOVE -1 TO MAINTL.                                           
100108     MOVE SPACES TO PI-WS-STATE.                                           
02117      GO TO 8100-SEND-INITIAL-MAP.                                 
02118                                                                   
02119  8870-NOTOPEN.                                                    
02120      MOVE ER-0042 TO EMI-ERROR.                                   
02121      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02122      MOVE -1 TO MAINTL.                                           
02123      GO TO 8200-SEND-DATAONLY.                                    
02124                                                                   
02125  8880-NOT-FOUND.                                                  
02126      MOVE ER-0149 TO EMI-ERROR.                                   
02127      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02128      MOVE -1       TO STCDL.                                      
02129      MOVE AL-UABON TO STCDA.                                      
02130      GO TO 8200-SEND-DATAONLY.                                    
02131                                                                   
02132  8890-LETR-NOT-OPEN.                                              
02133      MOVE ER-0013                TO EMI-ERROR.                    
02134      MOVE -1                     TO REPLETRL.                     
02135      MOVE AL-UABON               TO REPLETRA.                     
02136      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02137      GO TO 8200-SEND-DATAONLY.                                    
02138                                                                   
02139  9000-RETURN-CICS.                                                
02140      EXEC CICS RETURN                                             
02141      END-EXEC.                                                    
02142                                                                   
02143  9100-RETURN-TRAN.                                                
02144      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
02145      MOVE '106A'                 TO PI-CURRENT-SCREEN-NO.         
02146                                                                   
02147      EXEC CICS RETURN                                             
02148          TRANSID (TRANS-ID)                                       
02149          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
02150          LENGTH  (WS-COMM-LENGTH)                                 
02151      END-EXEC.                                                    
02152                                                                   
02153  9200-RETURN-MAIN-MENU.                                           
02154      IF  CREDIT-SESSION                                           
02155          MOVE XCTL-EL626           TO PGM-NAME                    
02156      ELSE                                                         
02157      IF  CLAIM-SESSION                                            
02158          MOVE XCTL-EL126           TO PGM-NAME                    
02159      ELSE                                                         
02160      IF  MORTGAGE-SESSION                                         
02161          MOVE XCTL-EM626           TO PGM-NAME                    
02162      ELSE                                                         
02163      IF  GENERAL-LEDGER-SESSION                                   
02164          MOVE XCTL-GL800           TO PGM-NAME.                   
02165                                                                   
02166      GO TO 9300-XCTL.                                             
02167                                                                   
02168  9300-XCTL.                                                       
02169      EXEC CICS XCTL                                               
02170          PROGRAM (PGM-NAME)                                       
02171          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
02172          LENGTH  (WS-COMM-LENGTH)                                 
02173      END-EXEC.                                                    
02174                                                                   
02175  9400-CLEAR.                                                      
02176      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
02177      GO TO 9300-XCTL.                                             
02178                                                                   
02179  9500-PF12.                                                       
02180      MOVE XCTL-EL010 TO PGM-NAME.                                 
02181      GO TO 9300-XCTL.                                             
02182                                                                   
02183  9600-PGMID-ERROR.                                                
02184      EXEC CICS HANDLE CONDITION                                   
02185          PGMIDERR(8300-SEND-TEXT)                                 
02186      END-EXEC.                                                    
02187                                                                   
02188      MOVE PGM-NAME      TO PI-CALLING-PROGRAM.                    
02189      MOVE ' '           TO PI-ENTRY-CD-1.                         
02190      MOVE XCTL-EL005    TO PGM-NAME.                              
02191      MOVE PGM-NAME      TO LOGOFF-PGM.                            
02192      MOVE PGMIDERR-MSG  TO LOGOFF-FILL.                           
02193      GO TO 9300-XCTL.                                             
02194                                                                   
02195  9700-LINK-DATE-CONVERT.                                          
02196      EXEC CICS LINK                                               
02197          PROGRAM    ('ELDATCV')                                   
02198          COMMAREA   (DATE-CONVERSION-DATA)                        
02199          LENGTH     (DC-COMM-LENGTH)                              
02200      END-EXEC.                                                    
02201                                                                   
02202  9700-EXIT.                                                       
02203      EXIT.                                                        
02204                                                                   
02205  9900-ERROR-FORMAT.                                               
02206      IF NOT EMI-ERRORS-COMPLETE                                   
02207          MOVE LINK-EL001 TO PGM-NAME                              
02208          EXEC CICS LINK                                           
02209              PROGRAM (PGM-NAME)                                   
02210              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              
02211              LENGTH  (EMI-COMM-LENGTH)                            
02212          END-EXEC.                                                
02213                                                                   
02214  9900-EXIT.                                                       
02215      EXIT.                                                        
02216                                                                   
092308 9910-INITIALIZE-SECURITY.                                        
      ******************************************************************
      *                                                                *
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
      *                                                                *
      ******************************************************************
                                                                        
           IF  PI-PROCESSOR-ID NOT = 'LGXX'                             
               IF  MORTGAGE-SESSION                                     
                   MOVE '125E'             TO SC-QUID-SYSTEM            
                   MOVE EIBTRMID           TO SC-QUID-TERMINAL          
                                                                        
                   EXEC CICS READQ TS                                   
                       QUEUE  (SC-QUID-KEY)                             
                       INTO   (SECURITY-CONTROL-E)                      
                       LENGTH (SC-COMM-LENGTH-E)                        
                       ITEM   (SC-ITEM)                                 
                   END-EXEC                                             
                                                                        
                   MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                
                                           TO PI-DISPLAY-CAP            
                   MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                 
                                           TO PI-MODIFY-CAP             
                                                                        
                   IF  NOT DISPLAY-CAP                                  
                       MOVE 'READ'         TO SM-READ                   
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT   
                       MOVE ER-9097        TO EMI-ERROR                 
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8100-SEND-INITIAL-MAP                      
                   ELSE                                                 
                       GO TO 9910-EXIT                                  
               ELSE                                                     
                   EXEC CICS  READQ TS                                  
                       QUEUE   (PI-SECURITY-TEMP-STORE-ID)              
                       INTO    (SECURITY-CONTROL)                       
                       LENGTH  (SC-COMM-LENGTH)                         
                       ITEM    (SC-ITEM-CL-CR)                          
                       END-EXEC                                         
                                                                        
                   MOVE SC-CREDIT-DISPLAY (29)                          
                                       TO PI-DISPLAY-CAP                
                   MOVE SC-CREDIT-UPDATE  (29)                          
                                       TO PI-MODIFY-CAP                 
                                                                        
                   IF  NOT DISPLAY-CAP                                  
                       MOVE 'READ'     TO SM-READ                       
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT   
                       MOVE ER-0070    TO  EMI-ERROR                    
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8100-SEND-INITIAL-MAP.                     
                                                                        
092308 9910-EXIT.                                                       
           EXIT.                                                        
                                                                        
02217  9990-ABEND.                                                      
02218      MOVE LINK-EL004             TO PGM-NAME.                     
02219      MOVE DFHEIBLK               TO EMI-LINE1.                    
02220      EXEC CICS LINK                                               
02221          PROGRAM   (PGM-NAME)                                     
02222          COMMAREA  (EMI-LINE1)                                    
02223          LENGTH    (72)                                           
02224      END-EXEC.                                                    
02225                                                                   
02226      GO TO 8200-SEND-DATAONLY.                                    
02227                                                                   
02228  9995-SECURITY-VIOLATION.                                         
02229             COPY ELCSCTP.                                         
02230                                                                   
02231  9995-EXIT.                                                       
02232       EXIT.                                                       
02233                                                                   
