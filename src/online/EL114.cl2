00001  ID DIVISION.                                                     
00002                                                                   
00003  PROGRAM-ID.                 EL114.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/20/95 15:43:46.                 
00007 *                            VMOD=2.009.                          
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
00023 *REMARKS.                                                         
00024                                                                   
00025 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED   
00026 *    FOR THE BENEFICIARY CONTROL RECORDS.                         
00027                                                                   
00028 *    SCREENS     - EL114A - BENEFICIARY MAINTENANCE               
00029                                                                   
00030 *    ENTERED BY  - EL101 - MAINTENANCE MENU                       
00031                                                                   
00032 *    EXIT TO     - EL101 - MAINTENANCE MENU                       
00033                                                                   
00034 *    INPUT FILE  - ELBENE -              - BENEFICIARY RECORDS    
00035                                                                   
00036 *    OUTPUT FILE - ELBENE -              - BENEFICIARY RECORDS    
00037                                                                   
00038 *    COMMAREA    - PASSED                                         
00039                                                                   
00040 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     
00041 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE 
00042 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00043 *                  ENTRIES (XCTL FROM CICS VIA EX39) THE SCREEN   
00044 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   
00045 *                  MAINTENANCE TYPE INDICATED.                    
081312******************************************************************
081312*                   C H A N G E   L O G
081312*
081312* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081312*-----------------------------------------------------------------
081312*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081312* EFFECTIVE    NUMBER
081312*-----------------------------------------------------------------
081312* 081312  IR2012081000001  AJRA  EDIT FOR BLANK BENEFICARY NUMBER 
082317* 082317  CR2017082100003  PEMA  ADD SUB TYPE to elbene
032019* 032019  CR2019011400002  PEMA  ADD ACH processing
081312******************************************************************
00046                                                                   
00047      EJECT                                                        
00048  ENVIRONMENT DIVISION.                                            
00049  DATA DIVISION.                                                   
00050  WORKING-STORAGE SECTION.                                         
00051  77  FILLER  PIC X(32)  VALUE '********************************'. 
00052  77  FILLER  PIC X(32)  VALUE '*    EL114 WORKING STORAGE     *'. 
00053  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.009 *********'. 
00054                                                                   
00055  77  BENE-IDX                PIC S9(04) VALUE +0  COMP.           
00056  77  MAP-MAXIMUM             PIC S9(04) VALUE +8  COMP.           
00057  77  FIRST-READ-PREV-SW      PIC X(01)  VALUE SPACES.             
00058      88  FIRST-READ-PREV                VALUE 'Y'.                
00059                                                                   
00060  01  ACCESS-KEYS.                                                 
00061      12  ELBENE-KEY.                                              
00062          16  ELBENE-COMPANY-CD   PIC X.                           
00063          16  ELBENE-RECORD-TYPE  PIC X.                           
00064          16  ELBENE-BENEFICIARY  PIC X(10).                       
00065      12  ELBENE2-KEY.                                             
00066          16  ELBENE2-COMPANY-CD  PIC X.                           
00067          16  ELBENE2-RECORD-TYPE PIC X.                           
00068          16  ELBENE2-BENE-NAME   PIC X(30).                       
00069          16  ELBENE2-PRIME-ALT   PIC X(10).                       
00070                                                                   
00071  01  WS-DATE-AREA.                                                
00072      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00073      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
00074                                                                   
00075  01  MISC-WORK-AREAS.
032019     12  ws-email-sw             pic 9 value 9.
032019         88  valid-email           value 0.
032019         88  warn-email            value 1.
032019         88  invalid-email         value 2.
032019     12  e1                      pic s999 comp-3 value +0.
032019     12  e2                      pic s999 comp-3 value +0.
032019     12  ws-tally-counter        pic s999 comp-3 value +0.
032019     12  ws-work-email-address   pic x(35).
00076                                                                   
00077      12  WS-ABENEI-10.                                            
00078          16  FILLER              PIC X(4) VALUE ZEROS.            
00079          16  WS-ABENEI           PIC X(6).                        
00080                                                                   
00081      12  WS-ZIP-CODE.                                             
00082          16  WS-ZIP-1            PIC X.                           
00083              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.           
00084          16  WS-ZIP-2-3          PIC XX.                          
00085          16  WS-ZIP-4            PIC X.                           
00086          16  WS-ZIP-5            PIC X.                           
00087          16  WS-ZIP-6            PIC X.                           
00088          16  FILLER              PIC X(4).                        
00089      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                     
00090          16  WS-ZIP-AM-1-CODE    PIC X(5).                        
00091          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                        
00092          16  FILLER              PIC X.                           
00093      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                     
00094          16  WS-ZIP-AM-2-CODE    PIC X(5).                        
00095          16  WS-ZIP-AM-2-DASH    PIC X.                           
00096          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                        
00097      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                    
00098          16  WS-ZIP-CAN-1-POST1  PIC XXX.                         
00099          16  WS-ZIP-CAN-1-POST2  PIC XXX.                         
00100          16  FILLER              PIC X(4).                        
00101      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                    
00102          16  WS-ZIP-CAN-2-POST1  PIC XXX.                         
00103          16  FILLER              PIC X.                           
00104          16  WS-ZIP-CAN-2-POST2  PIC XXX.                         
00105          16  FILLER              PIC XXX.                         
00106                                                                   
00107  01  STANDARD-AREAS.                                              
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  FILE-ID-ELCNTL          PIC X(8)    VALUE 'ELCNTL'.          
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
00108      12  SC-ITEM             PIC S9(4) COMP VALUE +1.             
00109      12  TRANS-ID            PIC X(4)    VALUE 'EX39'.            
00110      12  PGM-NAME            PIC X(8).                            
00111      12  TIME-IN             PIC S9(7).                           
00112      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00113          16  FILLER          PIC X.                               
00114          16  TIME-OUT        PIC 99V99.                           
00115          16  FILLER          PIC XX.                              
00116      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           
00117      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           
00118      12  XCTL-126            PIC X(8)    VALUE 'EL126'.           
00119      12  XCTL-400            PIC X(8)    VALUE 'EL400'.           
00120      12  XCTL-800            PIC X(8)    VALUE 'EL800'.           
00121      12  LINK-001            PIC X(8)    VALUE 'EL001'.           
00122      12  LINK-004            PIC X(8)    VALUE 'EL004'.           
00123      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
00124      12  THIS-PGM            PIC X(8)    VALUE 'EL114'.           
00125      12  ELBENE-FILEID       PIC X(8)    VALUE 'ELBENE'.          
00126      12  ELBENE2-FILEID      PIC X(8)    VALUE 'ELBENE2'.         
00127      12  SUB                 PIC 99.                              
00128      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
00129      12  APHONE-LENGTH       PIC S9(4)   VALUE +12 COMP.          
00130      12  MAPSET-NAME         PIC X(8)    VALUE 'EL114S'.          
00131      12  MAP-NAME-A          PIC X(8)    VALUE 'EL114A'.          
00132      12  MAP-NAME-B          PIC X(8)    VALUE 'EL114B'.          
00133      12  WS-MAP-NAME         PIC X(8)    VALUE 'EL114A'.          
00134      12  WS-PF-KEY           PIC 99      VALUE ZEROS.             
00135                                                                   
00136  01  WS-ANALYZE-NAME.                                             
00137      12  ONE                  PIC S9(04) VALUE +1 COMP.           
00138      12  TWO                  PIC S9(04) VALUE +2 COMP.           
00139      12  EIGHT                PIC S9(04) VALUE +8 COMP.           
00140      12  NINE                 PIC S9(04) VALUE +9 COMP.           
00141      12  NAME-CNT                                 PIC S9(4) COMP. 
00142      12  SCREEN-CNT                               PIC S9(4) COMP. 
00143      12  WS-INPUT-NAME                            PIC X(30).      
00144      12  WS-R-INPUT-NAME  REDEFINES  WS-INPUT-NAME.               
00145          16  WS-INPUT-CHAR  OCCURS 30 TIMES       PIC X.          
00146                                                                   
00147      12  WS-READ-NAME                             PIC X(30).      
00148      12  WS-R-READ-NAME  REDEFINES  WS-READ-NAME.                 
00149          16  WS-READ-CHAR   OCCURS 30 TIMES       PIC X.          
00150      12  NAME-IDX                                 PIC S9(4) COMP. 
00151                                                                   
00152  01  MAP-ENTRY-NUMBERS.                                           
00153      12  FILLER                  PIC X(3)  VALUE ' 1)'.           
00154      12  FILLER                  PIC X(3)  VALUE ' 2)'.           
00155      12  FILLER                  PIC X(3)  VALUE ' 3)'.           
00156      12  FILLER                  PIC X(3)  VALUE ' 4)'.           
00157      12  FILLER                  PIC X(3)  VALUE ' 5)'.           
00158      12  FILLER                  PIC X(3)  VALUE ' 6)'.           
00159      12  FILLER                  PIC X(3)  VALUE ' 7)'.           
00160      12  FILLER                  PIC X(3)  VALUE ' 8)'.           
00161                                                                   
00162  01  MAP-LINE-TABLE  REDEFINES  MAP-ENTRY-NUMBERS.                
00163      12  TBL-LINE-NUMBER  OCCURS 8 TIMES  PIC X(3).               
00164                                                                   
00165      EJECT                                                        
00166                                                                   
00167                              COPY ELCSCTM.                        
00168                              COPY ELCSCRTY.                       
00169                                                                   
00170      EJECT                                                        
00171  01  ERROR-MESSAGES.                                              
00172      12  ER-0000                 PIC X(4)  VALUE '0000'.          
00173      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00174      12  ER-0006                 PIC X(4)  VALUE '0006'.          
00175      12  ER-0008                 PIC X(4)  VALUE '0008'.          
00176      12  ER-0023                 PIC X(4)  VALUE '0023'.          
00177      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00178      12  ER-0042                 PIC X(4)  VALUE '0042'.          
00179      12  ER-0050                 PIC X(4)  VALUE '0050'.          
00180      12  ER-0052                 PIC X(4)  VALUE '0052'.          
00181      12  ER-0053                 PIC X(4)  VALUE '0053'.          
00182      12  ER-0068                 PIC X(4)  VALUE '0068'.          
00183      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00184      12  ER-0130                 PIC X(4)  VALUE '0130'.          
00185      12  ER-0131                 PIC X(4)  VALUE '0131'.          
00186      12  ER-0132                 PIC X(4)  VALUE '0132'.          
00187      12  ER-0138                 PIC X(4)  VALUE '0138'.          
00188      12  ER-0145                 PIC X(4)  VALUE '0145'.          
00189      12  ER-0147                 PIC X(4)  VALUE '0147'.          
00190      12  ER-0148                 PIC X(4)  VALUE '0148'.          
00191      12  ER-0149                 PIC X(4)  VALUE '0149'.          
00192      12  ER-0200                 PIC X(4)  VALUE '0200'.          
00193      12  ER-0565                 PIC X(4)  VALUE '0565'.          
00194      12  ER-0566                 PIC X(4)  VALUE '0566'.          
00195      12  ER-0658                 PIC X(4)  VALUE '0658'.          
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  er-2565                 pic x(4)  value '2565'.
00196      12  ER-7008                 PIC X(4)  VALUE '7008'.          
00197      12  ER-7220                 PIC X(4)  VALUE '7220'.
032019     12  ER-7663                 PIC X(4)  VALUE '7663'.
032019     12  ER-7664                 PIC X(4)  VALUE '7664'.
032019     12  ER-7665                 PIC X(4)  VALUE '7665'.
032019     12  ER-7666                 PIC X(4)  VALUE '7666'.
032019     12  ER-7667                 PIC X(4)  VALUE '7667'.
081312     12  ER-7668                 PIC X(4)  VALUE '7668'.
00198      12  ER-7672                 PIC X(4)  VALUE '7672'.          
00199      12  ER-7673                 PIC X(4)  VALUE '7673'.          
00200      12  ER-7674                 PIC X(4)  VALUE '7674'.          
00201      12  ER-7675                 PIC X(4)  VALUE '7675'.          
00202      12  ER-7676                 PIC X(4)  VALUE '7676'.          
00203      12  ER-7677                 PIC X(4)  VALUE '7677'.          
00204      12  ER-7678                 PIC X(4)  VALUE '7678'.          
00205      12  ER-7679                 PIC X(4)  VALUE '7679'.          
00206      12  ER-8124                 PIC X(4)  VALUE '8124'.          
00207      12  ER-8125                 PIC X(4)  VALUE '8125'.          
00208      EJECT                                                        
00209                              COPY ELCDATE.                        
00210      EJECT                                                        
00211                              COPY ELCLOGOF.                       
00212      EJECT                                                        
00213                              COPY ELCATTR.                        
00214      EJECT                                                        
00215                              COPY ELCEMIB.                        
00216      EJECT                                                        
00217                              COPY ELCINTF.                        
00218      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
00219          16  PI-MAX-SELECTION             PIC S9(04) COMP.        
00220          16  PI-KEY-LENGTH                PIC S9(04) COMP.        
00221          16  PI-NAME-LENGTH               PIC S9(04) COMP.        
00222          16  PI-PREV-DIRECTION            PIC X(1).               
00223              88  PI-PREV-FORWARD                   VALUE 'F'.     
00224              88  PI-PREV-BACKWARD                  VALUE 'B'.     
00225          16  PI-PREV-MAP-NAME             PIC X(8).               
00226          16  PI-PREV-BENEFICIARY          PIC X(10).              
00227          16  PI-LOW-BENE-NAME             PIC X(30).              
00228          16  PI-LOW-BENE-PRIME            PIC X(10).              
00229          16  PI-HIGH-BENE-NAME            PIC X(30).              
00230          16  PI-HIGH-BENE-PRIME           PIC X(10).
013017         16  pi-approval-level            pic x.
00231          16  FILLER                       PIC X(534).             
00232                                                                   
00233      EJECT                                                        
00234                              COPY ELCAID.                         
00235  01  FILLER    REDEFINES DFHAID.                                  
00236      12  FILLER              PIC X(8).                            
00237      12  PF-VALUES           PIC X       OCCURS 2.                
00238                                                                   
00239      EJECT                                                        
00240                                  COPY EL114S.                         
00241                                                                   
00242      EJECT                                                        
00243  01  MAP-REDEFINITION  REDEFINES  EL114BI.                        
00244      16  FILLER                        PIC X(73).                 
00245      16  MAP-SELECTION-DISPLAY OCCURS 8 TIMES.                    
00246          20  FILLER                    PIC X(03).                 
00247          20  MAP-LINE-NUMBER           PIC X(03).                 
00248          20  FILLER                    PIC X(03).                 
00249          20  MAP-BENE-NAME             PIC X(30).                 
00250          20  FILLER                    PIC X(03).                 
00251          20  MAP-BENE-CITY             PIC X(30).                 
00252          20  FILLER                    PIC X(03).                 
00253          20  MAP-BENE-CNTL             PIC X(10).                 
00254                                                                   
00255      EJECT                                                        
00256  LINKAGE SECTION.                                                 
00257                                                                   
00258  01  DFHCOMMAREA             PIC X(1024).                         
00259 *01 PARMLIST .                                                    
00260 *    02  FILLER              PIC S9(8)   COMP.                    
00261 *    02  ELBENE-POINTER      PIC S9(8)   COMP.                    
00262                                                                   
00263      EJECT                                                        
00264                                 COPY ELCBENE.
                                      COPY ELCCNTL.
00265                                                                   
00266      EJECT                                                        
00267  PROCEDURE DIVISION.                                              
00268      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00269      MOVE '5'                   TO DC-OPTION-CODE.                
00270      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
00271      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
00272      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
00273                                                                   
00274      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 
00275      IF EIBCALEN EQUAL 0                                          
00276          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00277                                                                   
00278      EXEC CICS HANDLE CONDITION                                   
00279          DUPREC  (8850-DUPREC)                                    
00280          NOTOPEN (8870-NOTOPEN)                                   
00281          NOTFND  (8880-NOT-FOUND)                                 
00282          PGMIDERR(9600-PGMID-ERROR)                               
00283          ERROR   (9990-ABEND)                                     
00284      END-EXEC.                                                    
00285                                                                   
00286      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM                     
00287          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM               
00288              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00289              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00290              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00291              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00292              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00293              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00294              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00295              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00296          ELSE                                                     
00297              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00298              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00299              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00300              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00301              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00302              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00303              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00304              MOVE SPACES               TO PI-SAVED-PROGRAM-6      
00305              MOVE PI-PREV-BENEFICIARY  TO ABENEI                  
00306      ELSE                                                         
00307          GO TO 0200-RECEIVE.                                      
00308                                                                   
00309      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        

013017     MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID
013017     MOVE '2'                    TO ELCNTL-REC-TYPE
013017     MOVE +0                     TO ELCNTL-SEQ
013017     MOVE PI-PROCESSOR-ID        TO ELCNTL-ACCESS

013017     EXEC CICS READ
013017          DATASET  (FILE-ID-ELCNTL)
013017          SET      (ADDRESS OF CONTROL-FILE)
013017          RIDFLD   (ELCNTL-KEY)
013017     END-EXEC

013017     MOVE CF-APPROVAL-LEVEL      TO PI-APPROVAL-LEVEL

00311      MOVE LOW-VALUES             TO  EL114AO.                     
00312      MOVE MAP-NAME-A             TO  PI-PREV-MAP-NAME.            
00313      MOVE -1                     TO  AMAINTL.                     
00314      GO TO 8100-SEND-INITIAL-MAP.                                 
00315                                                                   
00316      EJECT                                                        
00317  0200-RECEIVE.                                                    
00318                                                                   
00319      IF EIBAID NOT EQUAL DFHCLEAR                                 
00320          GO TO 0205-SCREEN-NOT-CLEARED.                           
00321                                                                   
00322      IF PI-PREV-MAP-NAME EQUAL MAP-NAME-A                         
00323          GO TO 9400-CLEAR.                                        
00324                                                                   
00325      MOVE LOW-VALUES             TO EL114BO.                      
00326      MOVE MAP-NAME-A             TO WS-MAP-NAME                   
00327                                     PI-PREV-MAP-NAME.             
00328      MOVE SPACES                 TO PI-PREV-DIRECTION.            
00329                                                                   
00330      IF PI-PREV-BENEFICIARY NOT EQUAL SPACES AND LOW-VALUES       
00331          MOVE PI-PREV-BENEFICIARY TO ABENEI                       
00332          GO TO 1000-SHOW-BENEFICIARY                              
00333      ELSE                                                         
00334          MOVE -1                 TO AMAINTL                       
00335          GO TO 8100-SEND-INITIAL-MAP.                             
00336                                                                   
00337  0205-SCREEN-NOT-CLEARED.                                         
00338                                                                   
00339      IF PI-PROCESSOR-ID EQUAL 'LGXX'                              
00340          GO TO 0210-RECEIVE.                                      
00341                                                                   
00342      EXEC CICS READQ TS                                           
00343          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       
00344          INTO   (SECURITY-CONTROL)                                
00345          LENGTH (SC-COMM-LENGTH)                                  
00346          ITEM   (SC-ITEM)                                         
00347      END-EXEC.                                                    
00348                                                                   
00349      MOVE SC-CLAIMS-DISPLAY (4)  TO PI-DISPLAY-CAP.               
00350      MOVE SC-CLAIMS-UPDATE  (4)  TO PI-MODIFY-CAP.                
00351                                                                   
00352      IF NOT DISPLAY-CAP                                           
00353          MOVE 'READ'             TO SM-READ                       
00354          PERFORM 9995-SECURITY-VIOLATION                          
00355          MOVE ER-0070            TO EMI-ERROR                     
00356          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00357          MOVE MAP-NAME-A         TO WS-MAP-NAME                   
00358          MOVE -1                 TO AMAINTL                       
00359          GO TO 8100-SEND-INITIAL-MAP.                             
00360                                                                   
00361  0210-RECEIVE.                                                    
00362                                                                   
00363      MOVE PI-PREV-MAP-NAME       TO WS-MAP-NAME.                  
00364                                                                   
00365      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   
00366          IF WS-MAP-NAME EQUAL MAP-NAME-A                          
00367              MOVE LOW-VALUES     TO EL114AI                       
00368              MOVE ER-7008        TO EMI-ERROR                     
00369              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00370              MOVE -1             TO AMAINTL                       
00371              GO TO 8200-SEND-DATAONLY                             
00372          ELSE                                                     
00373              MOVE LOW-VALUES     TO EL114BI                       
00374              MOVE ER-7008        TO EMI-ERROR                     
00375              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00376              MOVE -1             TO BSELL                         
00377              GO TO 8200-SEND-DATAONLY.                            
00378                                                                   
00379      EXEC CICS RECEIVE                                            
00380          MAP   (WS-MAP-NAME)                                      
00381          MAPSET(MAPSET-NAME)                                      
00382          INTO  (EL114AI)                                          
00383      END-EXEC.                                                    
00384                                                                   
00385      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
00386          IF APFKL EQUAL 0                                         
00387              GO TO 0300-CHECK-PFKEYS                              
00388          ELSE                                                     
00389              MOVE APFKI          TO WS-PF-KEY.                    
00390                                                                   
00391      IF WS-MAP-NAME EQUAL MAP-NAME-B                              
00392          IF BPFKL EQUAL 0                                         
00393              GO TO 0310-CHECK-PFKEYS                              
00394          ELSE                                                     
00395              MOVE BPFKI          TO WS-PF-KEY.                    
00396                                                                   
00397      IF EIBAID NOT EQUAL DFHENTER                                 
00398          MOVE ER-0004            TO EMI-ERROR                     
00399          GO TO 0320-INPUT-ERROR.                                  
00400                                                                   
00401      IF WS-PF-KEY GREATER 0 AND LESS 25                           
00402          MOVE PF-VALUES (WS-PF-KEY)                               
00403                                  TO EIBAID                        
00404      ELSE                                                         
00405          MOVE ER-0029            TO EMI-ERROR                     
00406          GO TO 0320-INPUT-ERROR.                                  
00407                                                                   
00408      IF WS-MAP-NAME EQUAL MAP-NAME-B                              
00409          GO TO 0310-CHECK-PFKEYS.                                 
00410                                                                   
00411  0300-CHECK-PFKEYS.                                               
00412                                                                   
00413      IF EIBAID EQUAL DFHPF23                                      
00414          GO TO 8810-PF23.                                         
00415                                                                   
00416      IF EIBAID EQUAL DFHPF24                                      
00417          GO TO 9200-RETURN-MAIN-MENU.                             
00418                                                                   
00419      IF EIBAID EQUAL DFHPF12                                      
00420          GO TO 9500-PF12.                                         
00421                                                                   
00422      IF (AMAINTL NOT EQUAL 0) AND (EIBAID NOT EQUAL DFHENTER)     
00423          MOVE ER-0050            TO EMI-ERROR                     
00424          GO TO 0320-INPUT-ERROR.                                  
00425                                                                   
00426      IF EIBAID EQUAL DFHPF1                                       
00427          GO TO 5000-FIND-NEXT-BENEFICIARY.                        
00428                                                                   
00429      IF EIBAID EQUAL DFHPF2                                       
00430          GO TO 5100-FIND-PREV-BENEFICIARY.                        
00431                                                                   
00432      IF EIBAID EQUAL DFHPF3                                       
00433          GO TO 0500-LOOKUP-BENEFICIARY.                           
00434                                                                   
PEMMOD     IF EIBAID = DFHPF4
PEMMOD        MOVE ABENEI              TO PI-PROGRAM-WORK-AREA (1:10)
PEMMOD        GO TO 9400-CLEAR
PEMMOD     END-IF
PEMMOD
00435      IF EIBAID EQUAL DFHENTER                                     
00436          GO TO 0330-EDIT-DATA-MAP-A.                              
00437                                                                   
00438      MOVE ER-0029                TO EMI-ERROR.                    
00439      GO TO 0320-INPUT-ERROR.                                      
00440                                                                   
00441  0310-CHECK-PFKEYS.                                               
00442                                                                   
00443      IF EIBAID EQUAL DFHPF23                                      
00444          GO TO 8810-PF23.                                         
00445                                                                   
00446      IF EIBAID EQUAL DFHPF24                                      
00447          GO TO 9200-RETURN-MAIN-MENU.                             
00448                                                                   
00449      IF EIBAID EQUAL DFHPF12                                      
00450          GO TO 9500-PF12.                                         
00451                                                                   
00452      IF EIBAID EQUAL DFHPF1                                       
00453          GO TO 5200-ROLL-NEXT-BENEFICIARY.                        
00454                                                                   
00455      IF EIBAID EQUAL DFHPF2                                       
00456          GO TO 5300-ROLL-PREV-BENEFICIARY.                        
00457                                                                   
PEMMOD*    IF EIBAID EQUAL DFHENTER                                     
PEMMOD     IF EIBAID EQUAL DFHENTER OR DFHPF3                           
00459          GO TO 0340-EDIT-DATA-MAP-B.                              
00460                                                                   
00461      MOVE ER-0029                TO EMI-ERROR.                    
00462                                                                   
00463  0320-INPUT-ERROR.                                                
00464                                                                   
00465      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00466                                                                   
00467      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
00468          IF APFKL EQUAL 0                                         
00469              MOVE -1             TO AMAINTL                       
00470          ELSE                                                     
00471              MOVE -1             TO APFKL.                        
00472                                                                   
00473      IF WS-MAP-NAME EQUAL MAP-NAME-B                              
00474          IF BPFKL EQUAL 0                                         
00475              MOVE -1             TO BSELL                         
00476          ELSE                                                     
00477              MOVE -1             TO BPFKL.                        
00478                                                                   
00479      GO TO 8200-SEND-DATAONLY.                                    
00480                                                                   
00481      EJECT                                                        
00482  0330-EDIT-DATA-MAP-A.                                            
00483                                                                   
00484      IF ABENEL EQUAL ZERO                                         
00485          MOVE ER-0565            TO EMI-ERROR                     
00486          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00487          MOVE -1                 TO ABENEL                        
00488          MOVE AL-UABON           TO ABENEA                        
00489          MOVE AL-UANON           TO AMAINTA                       
00490          GO TO 8200-SEND-DATAONLY.                                
00491                                                                   
00492      IF PI-COMPANY-ID = 'DMD'                                     
00493        IF ABENEL = +6                                             
00494          MOVE ABENEI (1:6)       TO WS-ABENEI                     
00495          MOVE WS-ABENEI-10       TO ABENEI.                       
00496                                                                   
00497      IF AMAINTI EQUAL 'S'                                         
00498          GO TO 1000-SHOW-BENEFICIARY.                             
00499                                                                   
00500      IF AMAINTI EQUAL 'A' OR 'C' OR 'D'                           
00501         IF NOT MODIFY-CAP                                         
00502             PERFORM 9995-SECURITY-VIOLATION                       
00503             MOVE ER-0070         TO EMI-ERROR                     
00504             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
00505             MOVE LOW-VALUES      TO EL114AO                       
00506             MOVE -1              TO AMAINTL                       
00507             GO TO 8100-SEND-INITIAL-MAP.                          
00508                                                                   
00509      IF AMAINTI EQUAL 'C'                                         
00510          GO TO 2000-CHANGE-BENEFICIARY.                           
00511                                                                   
00512      IF AMAINTI EQUAL 'A'                                         
00513          GO TO 3000-ADD-BENEFICIARY.                              
00514                                                                   
00515      IF AMAINTI EQUAL 'D'                                         
00516          GO TO 4000-DELETE-BENEFICIARY.                           
00517                                                                   
00518      MOVE ER-0023                TO EMI-ERROR.                    
00519      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00520      MOVE -1                     TO AMAINTL.                      
00521      MOVE AL-UABON               TO AMAINTA.                      
00522      GO TO 8200-SEND-DATAONLY.                                    
00523                                                                   
00524      EJECT                                                        
00525  0340-EDIT-DATA-MAP-B.                                            
00526                                                                   
00527      IF BSELL EQUAL ZERO                                          
00528          MOVE ER-0200            TO EMI-ERROR                     
00529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00530          MOVE -1                 TO BSELL                         
00531          MOVE AL-UNBON           TO BSELA                         
00532          GO TO 8200-SEND-DATAONLY.                                
00533                                                                   
00534      IF BSELI GREATER THAN PI-MAX-SELECTION                       
00535          MOVE ER-0200            TO EMI-ERROR                     
00536          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00537          MOVE -1                 TO BSELL                         
00538          MOVE AL-UNBON           TO BSELA                         
00539          GO TO 8200-SEND-DATAONLY.                                
00540                                                                   
00541      MOVE BSELI                  TO BENE-IDX.                     
00542      MOVE MAP-BENE-CNTL (BENE-IDX)                                
00543                                  TO PI-PREV-BENEFICIARY.          
00544      MOVE LOW-VALUES             TO EL114AI.                      
00545      MOVE MAP-NAME-A             TO WS-MAP-NAME                   
00546                                     PI-PREV-MAP-NAME.             
00547      MOVE PI-PREV-BENEFICIARY    TO ABENEI.                       
PEMMOD
PEMMOD     IF EIBAID = DFHPF3
PEMMOD        MOVE PI-PREV-BENEFICIARY TO PI-PROGRAM-WORK-AREA (1:10)
PEMMOD        GO TO 9400-CLEAR
PEMMOD     END-IF
PEMMOD
00548      GO TO 1000-SHOW-BENEFICIARY.                                 
00549                                                                   
00550      EJECT                                                        
00551  0500-LOOKUP-BENEFICIARY.                                         
00552                                                                   
00553      IF ABENAMEL EQUAL ZERO                                       
00554          MOVE ER-7677            TO EMI-ERROR                     
00555          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00556          MOVE -1                 TO ABENAMEL                      
00557          MOVE AL-UABON           TO ABENEA                        
00558          GO TO 8200-SEND-DATAONLY.                                
00559                                                                   
00560      IF ABENAMEI EQUAL SPACES OR LOW-VALUES                       
00561          MOVE ER-7677            TO EMI-ERROR                     
00562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00563          MOVE -1                 TO ABENAMEL                      
00564          MOVE AL-UABON           TO ABENEA                        
00565          GO TO 8200-SEND-DATAONLY.                                
00566                                                                   
00567      MOVE ABENEI                 TO PI-PREV-BENEFICIARY.          
00568      MOVE TWO                    TO PI-KEY-LENGTH.                
00569                                                                   
00570      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.           
00571      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.          
00572      MOVE ABENAMEI               TO ELBENE2-BENE-NAME             
00573                                     WS-INPUT-NAME.                
00574                                                                   
00575      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT                    
00576          VARYING NAME-IDX FROM ABENAMEL BY -1                     
00577           UNTIL WS-INPUT-CHAR (NAME-IDX) GREATER THAN SPACES.     
00578                                                                   
00579      MOVE NAME-IDX               TO PI-NAME-LENGTH                
00580      ADD NAME-IDX                TO PI-KEY-LENGTH.                
00581                                                                   
00582      EXEC CICS HANDLE CONDITION                                   
00583          NOTFND (0510-BENEFICIARY-NOTFND)                         
00584      END-EXEC.                                                    
00585                                                                   
00586      EXEC CICS READ                                               
00587          DATASET(ELBENE2-FILEID)                                  
00588          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
00589          RIDFLD (ELBENE2-KEY)                                     
00590          GENERIC                                                  
00591          EQUAL                                                    
00592          KEYLENGTH(PI-KEY-LENGTH)                                 
00593      END-EXEC.                                                    
00594                                                                   
00595      MOVE BE-CONTROL-BY-NAME     TO ELBENE2-KEY.                  
00596                                                                   
00597      GO TO 5210-BENE-KEY-BUILT.                                   
00598                                                                   
00599  0510-BENEFICIARY-NOTFND.                                         
00600                                                                   
00601      MOVE ER-7678                TO EMI-ERROR.                    
00602      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00603      MOVE -1                     TO ABENAMEL.                     
00604      MOVE AL-UABON               TO ABENAMEA.                     
00605      GO TO 8200-SEND-DATAONLY.                                    
00606                                                                   
00607      EJECT                                                        
00608  1000-SHOW-BENEFICIARY.                                           
00609      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.            
00610      MOVE 'B'                    TO ELBENE-RECORD-TYPE.           
00611      MOVE ABENEI                 TO ELBENE-BENEFICIARY.           
00612                                                                   
00613      EXEC CICS READ                                               
00614          DATASET(ELBENE-FILEID)                                   
00615          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
00616          RIDFLD (ELBENE-KEY)                                      
00617      END-EXEC.                                                    
00618                                                                   
00619      GO TO 7000-BUILD-OUTPUT-MAP.                                 
00620                                                                   
00621      EJECT                                                        
00622  2000-CHANGE-BENEFICIARY.                                         
00623      IF ABENEI NOT EQUAL PI-PREV-BENEFICIARY                      
00624          MOVE ER-0138            TO EMI-ERROR                     
00625          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00626          MOVE -1                 TO ABENEL                        
00627          MOVE AL-UABON           TO ABENEA                        
00628          GO TO 8200-SEND-DATAONLY.                                
00629                                                                   
00630      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 
00631                                                                   
00632      IF NOT EMI-NO-ERRORS                                         
00633          GO TO 8200-SEND-DATAONLY.                                
00634                                                                   
00635      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD             
00636      MOVE 'B'                    TO ELBENE-RECORD-TYPE.           
00637      MOVE ABENEI                 TO ELBENE-BENEFICIARY            
00638                                                                   
00639      EXEC CICS READ                                               
00640          UPDATE                                                   
00641          DATASET(ELBENE-FILEID)                                   
00642          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
00643          RIDFLD(ELBENE-KEY)                                       
00644      END-EXEC.                                                    
00645                                                                   
00646      IF BE-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR            
00647         BE-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS           
00648          EXEC CICS UNLOCK                                         
00649              DATASET(ELBENE-FILEID)                               
00650          END-EXEC                                                 
00651          MOVE ER-0068 TO EMI-ERROR                                
00652          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00653          GO TO 1000-SHOW-BENEFICIARY.                             
00654                                                                   
00655      MOVE PI-PROCESSOR-ID        TO BE-LAST-MAINT-BY.             
00656      MOVE EIBTIME                TO BE-LAST-MAINT-HHMMSS.         
00657      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00658      MOVE '5'                    TO DC-OPTION-CODE.               
00659      MOVE LINK-ELDATCV           TO PGM-NAME.                     
00660                                                                   
00661      EXEC CICS LINK                                               
00662          PROGRAM (PGM-NAME)                                       
00663          COMMAREA(DATE-CONVERSION-DATA)                           
00664          LENGTH  (DC-COMM-LENGTH)                                 
00665      END-EXEC.                                                    
00666                                                                   
00667      IF DATE-CONVERSION-ERROR                                     
00668          MOVE LOW-VALUES         TO BE-LAST-MAINT-DT              
00669      ELSE                                                         
00670          MOVE DC-BIN-DATE-1      TO BE-LAST-MAINT-DT.             
00671                                                                   
00672      IF ABENAMEL NOT EQUAL ZEROS                                  
00673         MOVE ABENAMEI            TO BE-MAIL-TO-NAME               
00674                                     BE-MAIL-TO-NAME-A1.           
00675                                                                   
00676      IF AADDR1L  NOT EQUAL ZEROS                                  
00677         MOVE AADDR1I             TO BE-ADDRESS-LINE-1.            
00678                                                                   
00679      IF AADDR2L  NOT EQUAL ZEROS                                  
00680         MOVE AADDR2I             TO BE-ADDRESS-LINE-2.            
00681                                                                   
00682      IF AADDR3L  NOT EQUAL ZEROS                                  
00683         MOVE AADDR3I             TO BE-ADDRESS-LINE-3.            
00684                                                                   
           IF ACITYL NOT = 0
              MOVE ACITYI              TO BE-CITY
           END-IF
           IF ASTATEL NOT = 0
              MOVE ASTATEI             TO BE-STATE
           END-IF
00685 *    IF ACITYSTL NOT EQUAL ZEROS                                  
00686 *       MOVE ACITYSTI            TO BE-CITY-STATE.                
00687                                                                   
00688      IF AZIPCDEL = ZEROS                                          
00689          GO TO 2000-CHANGE-CONTINUE.                              
00690                                                                   
00691      MOVE SPACES                      TO BE-ZIP-CODE.             
00692      MOVE AZIPCDEI                    TO WS-ZIP-CODE.             
00693                                                                   
00694      IF WS-CANADIAN-ZIP                                           
00695          IF WS-ZIP-4 = SPACE  OR  '-'                             
00696              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-1          
00697              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-2          
00698          ELSE                                                     
00699              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-1          
00700              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-2          
00701      ELSE                                                         
00702          IF WS-ZIP-6 = SPACE  OR  '-'                             
00703              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME             
00704              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS4             
00705          ELSE                                                     
00706              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME             
00707              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS4.            
00708                                                                   
00709  2000-CHANGE-CONTINUE.                                            
00710                                                                   
00711      IF APHONEL GREATER THAN ZERO                                 
00712          EXEC CICS BIF DEEDIT                                     
00713              FIELD   (APHONEI)                                    
00714              LENGTH  (APHONE-LENGTH)                              
00715          END-EXEC                                                 
00716          MOVE APHONEI            TO BE-PHONE-NO.                  
00717                                                                   
00718      IF GRPCHKL GREATER THAN +0                                   
00719         MOVE GRPCHKI             TO BE-GROUP-CHECKS-Y-N.          
00720                                                                   
00721      IF ACORRESL GREATER THAN +0                                  
00722         MOVE ACORRESI            TO  BE-MAIL-TO-NAME2.            
00723                                                                   
00724      IF ACADDR1L GREATER THAN +0                                  
00725         MOVE  ACADDR1I           TO  BE-ADDRESS-LINE-12.          
00726                                                                   
00727      IF ACADDR2L GREATER THAN +0                                  
00728         MOVE  ACADDR2I           TO  BE-ADDRESS-LINE-22.          
00729                                                                   
00730      IF ACADDR3L GREATER THAN +0                                  
00731         MOVE ACADDR3I            TO  BE-ADDRESS-LINE-32.          
00732                                                                   
00733      IF ACCITYL GREATER THAN +0                                  
00734         MOVE ACCITYI            TO  BE-CITY2.

00733      IF ACSTATEL GREATER THAN +0                                  
00734         MOVE ACSTATEI            TO  BE-STATE2.

00736      IF ACZPCDEL = ZEROS                                          
00737          GO TO 2001-CHANGE-CONTINUE.                              
00738                                                                   
00739      MOVE SPACES                      TO BE-ZIP-CODE2.            
00740      MOVE ACZPCDEI                    TO WS-ZIP-CODE.             
00741                                                                   
00742      IF WS-CANADIAN-ZIP                                           
00743          IF WS-ZIP-4 = SPACE  OR  '-'                             
00744              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-12         
00745              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-22         
00746          ELSE                                                     
00747              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-12         
00748              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-22         
00749      ELSE                                                         
00750          IF WS-ZIP-6 = SPACE  OR  '-'                             
00751              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME2            
00752              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS42            
00753          ELSE                                                     
00754              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME2            
00755              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS42.           
00756                                                                   
00757  2001-CHANGE-CONTINUE.                                            
00758      IF ACPHONEL GREATER THAN ZERO                                
00759          EXEC CICS BIF DEEDIT                                     
00760              FIELD   (ACPHONEI)                                   
00761              LENGTH  (APHONE-LENGTH)                              
00762          END-EXEC                                                 
00763         MOVE ACPHONEI            TO  BE-PHONE-NO2.                
00764                                                                   
00781      IF AFAXNOL   GREATER THAN +0                                 
00782          EXEC CICS BIF DEEDIT                                     
00783              FIELD   (AFAXNOI)                                    
00784              LENGTH  (APHONE-LENGTH)                              
00785          END-EXEC                                                 
00786         MOVE AFAXNOI             TO  BE-BSR-FAX-NUM.              
00787                                                                   
032019*    IF ACBSOTL   GREATER THAN +0                                 
032019*       MOVE ACBSOTI             TO  BE-OUTPUT-TYPE.              

           IF AACHYNL > +0
              MOVE AACHYNI             TO BE-ACH-YES-OR-NO
           END-IF

           IF (AABANOL > +0)
              and (pi-approval-level = '4' or '5')
              MOVE AABANOI             TO BE-ACH-ABA-ROUTING-NUMBER
           END-IF

           IF AACCTNOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AACCTNOI            TO BE-ACH-BANK-ACCOUNT-NUMBER
           END-IF

032019     if aacheynl > +0
032019        move aacheyni            to be-ach-email-yn
032019     end-if
032019
032019     if aemaill > +0
032019        move aemaili             to be-ach-email-addr
032019     end-if

           IF (ASUBTYPL > +0)
              and (pi-approval-level = '4' or '5')
              MOVE ASUBTYPI            TO BE-ACH-SUB-TYPE
           END-IF

00791      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                
00792          IF ACARRL GREATER THAN +0                                
00793              MOVE ACARRI         TO  BE-CARRIER.                  
00794                                                                   
00795      EXEC CICS REWRITE                                            
00796          DATASET(ELBENE-FILEID)                                   
00797          FROM(BENEFICIARY-MASTER)                                 
00798      END-EXEC.                                                    
00799                                                                   
00800      MOVE ER-0000                TO EMI-ERROR.                    
00801      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00802      MOVE LOW-VALUES             TO EL114AO.                      
00803      MOVE -1                     TO AMAINTL.                      
00804      MOVE SPACES                 TO PI-PREV-BENEFICIARY.          
00805      MOVE ELBENE-BENEFICIARY     TO ABENEI.                       
00806      GO TO 1000-SHOW-BENEFICIARY.                                 
00807                                                                   
00808      EJECT                                                        
00809  3000-ADD-BENEFICIARY.                                            
00810                                                                   
00811      PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 
00812                                                                   
00813      IF NOT EMI-NO-ERRORS                                         
00814          GO TO 8200-SEND-DATAONLY.                                
00815                                                                   
00816      EXEC CICS GETMAIN                                            
00817          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
00818          LENGTH (500)                                             
00819          INITIMG(GETMAIN-SPACE)                                   
00820      END-EXEC.                                                    
00821                                                                   
00822      MOVE 'BE'                   TO BE-RECORD-ID.                 
00823      MOVE 'B'                    TO BE-RECORD-TYPE                
00824                                     BE-RECORD-TYPE-A1             
00825                                     ELBENE-RECORD-TYPE.           
00826      MOVE PI-COMPANY-CD          TO BE-COMPANY-CD                 
00827                                     BE-COMPANY-CD-A1              
00828                                     ELBENE-COMPANY-CD.            
00829      MOVE ABENEI                 TO BE-BENEFICIARY                
00830                                     BE-ALTERNATE-PRIME-A1         
00831                                     ELBENE-BENEFICIARY.           
00832      MOVE ABENAMEI               TO BE-MAIL-TO-NAME               
00833                                     BE-MAIL-TO-NAME-A1.           
00834      MOVE AADDR1I                TO BE-ADDRESS-LINE-1.            
00835                                                                   
00836      IF AADDR2L GREATER THAN ZERO                                 
00837          MOVE AADDR2I            TO BE-ADDRESS-LINE-2.            
00838                                                                   
00839      IF AADDR3L  NOT EQUAL ZEROS                                  
00840         MOVE AADDR3I             TO BE-ADDRESS-LINE-3.            
00841                                                                   
00842      MOVE ACITYI                 TO BE-CITY.
           MOVE ASTATEI                TO BE-STATE
00843                                                                   
00844      IF AZIPCDEL = ZEROS                                          
00845          GO TO 3000-ADD-CONTINUE.                                 
00846                                                                   
00847      MOVE SPACES                      TO BE-ZIP-CODE.             
00848      MOVE AZIPCDEI                    TO WS-ZIP-CODE.             
00849                                                                   
00850      IF WS-CANADIAN-ZIP                                           
00851          IF WS-ZIP-4 = SPACE  OR  '-'                             
00852              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-1          
00853              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-2          
00854          ELSE                                                     
00855              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-1          
00856              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-2          
00857      ELSE                                                         
00858          IF WS-ZIP-6 = SPACE  OR  '-'                             
00859              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME             
00860              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS4             
00861          ELSE                                                     
00862              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME             
00863              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS4.            
00864                                                                   
00865  3000-ADD-CONTINUE.                                               
00866                                                                   
00867      IF APHONEL GREATER THAN ZERO                                 
00868          EXEC CICS BIF DEEDIT                                     
00869              FIELD   (APHONEI)                                    
00870              LENGTH  (APHONE-LENGTH)                              
00871          END-EXEC                                                 
00872          MOVE APHONEI            TO BE-PHONE-NO                   
00873      ELSE                                                         
00874          MOVE ZEROS              TO BE-PHONE-NO.                  
00875                                                                   
00876      IF GRPCHKL GREATER THAN +0                                   
00877         MOVE GRPCHKI             TO BE-GROUP-CHECKS-Y-N.          
00878                                                                   
00879      IF ACORRESL GREATER THAN +0                                  
00880         MOVE ACORRESI            TO  BE-MAIL-TO-NAME2.            
00881                                                                   
00882      IF ACADDR1L GREATER THAN +0                                  
00883         MOVE  ACADDR1I           TO  BE-ADDRESS-LINE-12.          
00884                                                                   
00885      IF ACADDR2L GREATER THAN +0                                  
00886         MOVE  ACADDR2I           TO  BE-ADDRESS-LINE-22.          
00887                                                                   
00888      IF ACADDR3L GREATER THAN +0                                  
00889         MOVE ACADDR3I            TO  BE-ADDRESS-LINE-32.          
00890                                                                   
00891      IF ACCITYL GREATER THAN +0                                  
00892         MOVE ACCITYI            TO  BE-CITY2.

00891      IF ACSTATEL GREATER THAN +0                                  
00892         MOVE ACSTATEI            TO  BE-STATE2.

00894      IF ACZPCDEL = ZEROS                                          
00895          GO TO 3001-ADD-CONTINUE.                                 
00896                                                                   
00897      MOVE SPACES                      TO BE-ZIP-CODE2.            
00898      MOVE ACZPCDEI                    TO WS-ZIP-CODE.             
00899                                                                   
00900      IF WS-CANADIAN-ZIP                                           
00901          IF WS-ZIP-4 = SPACE  OR  '-'                             
00902              MOVE WS-ZIP-CAN-2-POST1  TO BE-CAN-POSTAL-12         
00903              MOVE WS-ZIP-CAN-2-POST2  TO BE-CAN-POSTAL-22         
00904          ELSE                                                     
00905              MOVE WS-ZIP-CAN-1-POST1  TO BE-CAN-POSTAL-12         
00906              MOVE WS-ZIP-CAN-1-POST2  TO BE-CAN-POSTAL-22         
00907      ELSE                                                         
00908          IF WS-ZIP-6 = SPACE  OR  '-'                             
00909              MOVE WS-ZIP-AM-2-CODE    TO BE-ZIP-PRIME2            
00910              MOVE WS-ZIP-AM-2-PLUS4   TO BE-ZIP-PLUS42            
00911          ELSE                                                     
00912              MOVE WS-ZIP-AM-1-CODE    TO BE-ZIP-PRIME2            
00913              MOVE WS-ZIP-AM-1-PLUS4   TO BE-ZIP-PLUS42.           
00914                                                                   
00915  3001-ADD-CONTINUE.                                               
00916      IF ACPHONEL GREATER THAN ZERO                                
00917          EXEC CICS BIF DEEDIT                                     
00918              FIELD   (ACPHONEI)                                   
00919              LENGTH  (APHONE-LENGTH)                              
00920          END-EXEC                                                 
00921          MOVE ACPHONEI           TO  BE-PHONE-NO2                 
00922      ELSE                                                         
00923          MOVE ZEROS              TO  BE-PHONE-NO2.                
00924                                                                   
00943      IF AFAXNOL   GREATER THAN +0                                 
00944          EXEC CICS BIF DEEDIT                                     
00945              FIELD   (AFAXNOI)                                    
00946              LENGTH  (APHONE-LENGTH)                              
00947          END-EXEC                                                 
00948         MOVE AFAXNOI             TO  BE-BSR-FAX-NUM               
00949      ELSE                                                         
00950         MOVE ZEROS               TO  BE-BSR-FAX-NUM.              
00951                                                                   
032019*    IF ACBSOTL   GREATER THAN +0                                 
032019*       MOVE ACBSOTI             TO  BE-OUTPUT-TYPE.              

           IF AACHYNL > +0
              MOVE AACHYNI             TO BE-ACH-YES-OR-NO
           END-IF

           IF AABANOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AABANOI             TO BE-ACH-ABA-ROUTING-NUMBER
           END-IF

           IF AACCTNOL > +0
              and (pi-approval-level = '4' or '5')
              MOVE AACCTNOI            to BE-ACH-BANK-ACCOUNT-NUMBER
           END-IF

032019     if aacheynl > +0
032019        move aacheyni            to be-ach-email-yn
032019     end-if
032019
032019     if aemaill > +0
032019        move aemaili             to be-ach-email-addr
032019     end-if


00955      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                
00956          IF ACARRL GREATER THAN +0                                
00957              MOVE ACARRI         TO BE-CARRIER.                   
00958                                                                   
00959      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00960      MOVE '5'                    TO DC-OPTION-CODE.               
00961      MOVE LINK-ELDATCV           TO PGM-NAME.                     
00962                                                                   
00963      EXEC CICS LINK                                               
00964          PROGRAM (PGM-NAME)                                       
00965          COMMAREA(DATE-CONVERSION-DATA)                           
00966          LENGTH  (DC-COMM-LENGTH)                                 
00967      END-EXEC.                                                    
00968                                                                   
00969      IF DATE-CONVERSION-ERROR                                     
00970          MOVE LOW-VALUES         TO BE-LAST-MAINT-DT              
00971      ELSE                                                         
00972          MOVE DC-BIN-DATE-1      TO BE-LAST-MAINT-DT.             
00973                                                                   
00974      MOVE PI-PROCESSOR-ID        TO BE-LAST-MAINT-BY.             
00975      MOVE EIBTIME                TO BE-LAST-MAINT-HHMMSS.         
00976                                                                   
00977  3005-WRITE-ELBENE-FILE.                                          
00978                                                                   
00979      EXEC CICS WRITE                                              
00980          FROM   (BENEFICIARY-MASTER)                              
00981          DATASET(ELBENE-FILEID)                                   
00982          RIDFLD (ELBENE-KEY)                                      
00983      END-EXEC.                                                    
00984                                                                   
00985      MOVE ER-0000                TO EMI-ERROR.                    
00986      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00987      MOVE LOW-VALUES             TO EL114AO.                      
00988      MOVE -1                     TO AMAINTL.                      
00989      MOVE SPACES                 TO PI-PREV-BENEFICIARY.          
00990      MOVE ELBENE-BENEFICIARY     TO ABENEI.                       
00991      GO TO 1000-SHOW-BENEFICIARY.                                 
00992                                                                   
00993      EJECT                                                        
00994  4000-DELETE-BENEFICIARY.                                         
00995                                                                   
00996      IF ABENEI NOT EQUAL PI-PREV-BENEFICIARY                      
00997          MOVE ER-0138            TO EMI-ERROR                     
00998          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00999          MOVE -1                 TO ABENEL                        
01000          MOVE AL-UABON           TO ABENEA                        
01001          GO TO 8200-SEND-DATAONLY.                                
01002                                                                   
01003      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.            
01004      MOVE 'B'                    TO ELBENE-RECORD-TYPE.           
01005      MOVE ABENEI                 TO ELBENE-BENEFICIARY.           
01006                                                                   
01007      EXEC CICS READ                                               
01008          UPDATE                                                   
01009          DATASET(ELBENE-FILEID)                                   
01010          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01011          RIDFLD (ELBENE-KEY)                                      
01012      END-EXEC.                                                    
01013                                                                   
01014      IF BE-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR            
01015         BE-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS           
01016          EXEC CICS UNLOCK                                         
01017              DATASET(ELBENE-FILEID)                               
01018              END-EXEC                                             
01019          MOVE ER-0068            TO EMI-ERROR                     
01020          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01021          GO TO 1000-SHOW-BENEFICIARY.                             
01022                                                                   
01023      EXEC CICS DELETE                                             
01024          DATASET(ELBENE-FILEID)                                   
01025          END-EXEC.                                                
01026                                                                   
01027      MOVE ER-0000                TO EMI-ERROR.                    
01028      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01029      MOVE LOW-VALUES             TO EL114AO.                      
01030      MOVE -1                     TO AMAINTL.                      
01031      MOVE SPACES                 TO PI-PREV-BENEFICIARY           
01032      GO TO 8100-SEND-INITIAL-MAP.                                 
01033                                                                   
01034      EJECT                                                        
01035  5000-FIND-NEXT-BENEFICIARY.                                      
01036                                                                   
01037      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.            
01038      MOVE 'B'                    TO ELBENE-RECORD-TYPE.           
01039                                                                   
01040      IF ABENEL EQUAL 0                                            
01041          MOVE LOW-VALUES         TO ELBENE-BENEFICIARY            
01042      ELSE                                                         
01043          MOVE ABENEI             TO ELBENE-BENEFICIARY.           
01044                                                                   
01045      EXEC CICS HANDLE CONDITION                                   
01046          ENDFILE (5000-UNSUCCESSFUL-SEARCH)                       
01047      END-EXEC.                                                    
01048                                                                   
01049      EXEC CICS STARTBR                                            
01050          DATASET (ELBENE-FILEID)                                  
01051          RIDFLD  (ELBENE-KEY)                                     
01052          GTEQ                                                     
01053      END-EXEC.                                                    
01054                                                                   
01055  5000-READNEXT-LOOP.                                              
01056      EXEC CICS READNEXT                                           
01057          DATASET(ELBENE-FILEID)                                   
01058          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01059          RIDFLD (ELBENE-KEY)                                      
01060      END-EXEC.                                                    
01061                                                                   
01062      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
01063          PERFORM 5000-END-BROWSE                                  
01064          GO TO 8860-ENDFILE.                                      
01065                                                                   
01066      IF BE-RECORD-TYPE NOT EQUAL 'B'                              
01067          PERFORM 5000-END-BROWSE                                  
01068          GO TO 8860-ENDFILE.                                      
01069                                                                   
01070      IF ELBENE-BENEFICIARY EQUAL PI-PREV-BENEFICIARY              
01071          GO TO 5000-READNEXT-LOOP.                                
01072                                                                   
01073      MOVE ELBENE-BENEFICIARY     TO ABENEI                        
01074                                     PI-PREV-BENEFICIARY.          
01075                                                                   
01076      IF ABENEL EQUAL 0                                            
01077          MOVE ER-7220            TO EMI-ERROR                     
01078          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
01079                                                                   
01080      GO TO 7000-BUILD-OUTPUT-MAP.                                 
01081                                                                   
01082  5000-END-BROWSE.                                                 
01083                                                                   
01084      EXEC CICS ENDBR                                              
01085          DATASET (ELBENE-FILEID)                                  
01086      END-EXEC.                                                    
01087                                                                   
01088  5000-UNSUCCESSFUL-SEARCH.                                        
01089                                                                   
01090      PERFORM 5000-END-BROWSE.                                     
01091      GO TO 8860-ENDFILE.                                          
01092                                                                   
01093      EJECT                                                        
01094  5100-FIND-PREV-BENEFICIARY.                                      
01095                                                                   
01096      MOVE 'Y'                    TO FIRST-READ-PREV-SW.           
01097      MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD.            
01098      MOVE 'B'                    TO ELBENE-RECORD-TYPE.           
01099      MOVE PI-PREV-BENEFICIARY    TO ELBENE-BENEFICIARY.           
01100                                                                   
01101      IF ABENEL GREATER 0                                          
01102          MOVE ABENEI             TO ELBENE-BENEFICIARY.           
01103                                                                   
01104      MOVE LOW-VALUES             TO PI-PREV-BENEFICIARY.          
01105                                                                   
01106      EXEC CICS HANDLE CONDITION                                   
01107          ENDFILE (5100-UNSUCCESSFUL-SEARCH)                       
01108      END-EXEC.                                                    
01109                                                                   
01110      EXEC CICS STARTBR                                            
01111          DATASET (ELBENE-FILEID)                                  
01112          RIDFLD  (ELBENE-KEY)                                     
01113          GTEQ                                                     
01114      END-EXEC.                                                    
01115                                                                   
01116  5100-READPREV-LOOP.                                              
01117      EXEC CICS READPREV                                           
01118          DATASET(ELBENE-FILEID)                                   
01119          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01120          RIDFLD (ELBENE-KEY)                                      
01121      END-EXEC.                                                    
01122                                                                   
01123      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
01124          PERFORM 5100-END-BROWSE                                  
01125          GO TO 8860-ENDFILE.                                      
01126                                                                   
01127      IF BE-RECORD-TYPE NOT EQUAL 'B'                              
01128          PERFORM 5100-END-BROWSE                                  
01129          GO TO 8860-ENDFILE.                                      
01130                                                                   
01131      IF FIRST-READ-PREV                                           
01132          MOVE 'N'                TO FIRST-READ-PREV-SW            
01133          GO TO 5100-READPREV-LOOP.                                
01134                                                                   
01135      MOVE ELBENE-BENEFICIARY     TO ABENEI.                       
01136      MOVE BE-BENEFICIARY         TO PI-PREV-BENEFICIARY.          
01137      GO TO 7000-BUILD-OUTPUT-MAP.                                 
01138                                                                   
01139  5100-END-BROWSE.                                                 
01140      EXEC CICS ENDBR                                              
01141          DATASET (ELBENE-FILEID)                                  
01142      END-EXEC.                                                    
01143                                                                   
01144                                                                   
01145  5100-UNSUCCESSFUL-SEARCH.                                        
01146                                                                   
01147      PERFORM 5100-END-BROWSE.                                     
01148      GO TO 8860-ENDFILE.                                          
01149                                                                   
01150      EJECT                                                        
01151  5200-ROLL-NEXT-BENEFICIARY.                                      
01152                                                                   
01153      IF PI-MAX-SELECTION LESS THAN MAP-MAXIMUM AND                
01154          PI-PREV-FORWARD                                          
01155              MOVE ER-0130        TO EMI-ERROR                     
01156              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01157              MOVE -1             TO BPFKL                         
01158              MOVE AL-UABON       TO BPFKA                         
01159              GO TO 8200-SEND-DATAONLY.                            
01160                                                                   
01161      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.           
01162      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.          
01163      MOVE PI-HIGH-BENE-NAME      TO ELBENE2-BENE-NAME             
01164                                     WS-INPUT-NAME.                
01165      MOVE PI-HIGH-BENE-PRIME     TO ELBENE2-PRIME-ALT.            
01166                                                                   
01167  5210-BENE-KEY-BUILT.                                             
01168                                                                   
01169      MOVE LOW-VALUES             TO EL114BO.                      
01170      MOVE ONE                    TO NAME-CNT.                     
01171                                                                   
01172      EXEC CICS HANDLE CONDITION                                   
01173          ENDFILE (5230-END-BROWSE)                                
01174      END-EXEC.                                                    
01175                                                                   
01176      EXEC CICS STARTBR                                            
01177          DATASET (ELBENE2-FILEID)                                 
01178          RIDFLD  (ELBENE2-KEY)                                    
01179          GTEQ                                                     
01180      END-EXEC.                                                    
01181                                                                   
01182  5220-READNEXT-LOOP.                                              
01183                                                                   
01184      EXEC CICS READNEXT                                           
01185          DATASET(ELBENE2-FILEID)                                  
01186          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01187          RIDFLD (ELBENE2-KEY)                                     
01188      END-EXEC.                                                    
01189                                                                   
01190      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
01191          GO TO 5230-END-BROWSE.                                   
01192                                                                   
01193      IF BE-RECORD-TYPE NOT EQUAL 'B'                              
01194          GO TO 5230-END-BROWSE.                                   
01195                                                                   
01196      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.                 
01197                                                                   
01198      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT                    
01199          VARYING NAME-IDX FROM ONE BY ONE                         
01200              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR        
01201                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL               
01202                      WS-READ-CHAR (NAME-IDX).                     
01203                                                                   
01204      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH                  
01205          GO TO 5230-END-BROWSE.                                   
01206                                                                   
01207      IF NAME-CNT EQUAL ONE                                        
01208          MOVE BE-MAIL-TO-NAME-A1 TO PI-LOW-BENE-NAME              
01209          MOVE BE-BENEFICIARY     TO PI-LOW-BENE-PRIME.            
01210                                                                   
01211      MOVE TBL-LINE-NUMBER (NAME-CNT)                              
01212                                  TO MAP-LINE-NUMBER (NAME-CNT).   
01213      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)      
01214                                     PI-HIGH-BENE-NAME.            
01215      MOVE BE-BENEFICIARY         TO PI-HIGH-BENE-PRIME            
01216                                     MAP-BENE-CNTL (NAME-CNT).     
01217      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).     
01218      MOVE NAME-CNT               TO PI-MAX-SELECTION.             
01219      ADD ONE                     TO NAME-CNT.                     
01220                                                                   
01221      IF NAME-CNT LESS THAN NINE                                   
01222          GO TO 5220-READNEXT-LOOP.                                
01223                                                                   
01224  5230-END-BROWSE.                                                 
01225                                                                   
01226      EXEC CICS ENDBR                                              
01227          DATASET (ELBENE2-FILEID)                                 
01228      END-EXEC.                                                    
01229                                                                   
01230      IF NAME-CNT LESS THAN EIGHT                                  
01231          MOVE ER-0130            TO EMI-ERROR                     
01232          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
01233                                                                   
01234      MOVE 'F'                    TO PI-PREV-DIRECTION.            
01235      MOVE MAP-NAME-B             TO WS-MAP-NAME                   
01236                                     PI-PREV-MAP-NAME.             
01237      MOVE -1                     TO BSELL.                        
01238      GO TO 8100-SEND-INITIAL-MAP.                                 
01239                                                                   
01240      EJECT                                                        
01241  5300-ROLL-PREV-BENEFICIARY.                                      
01242                                                                   
01243      IF PI-MAX-SELECTION LESS THAN MAP-MAXIMUM AND                
01244          PI-PREV-BACKWARD                                         
01245              MOVE ER-0130        TO EMI-ERROR                     
01246              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
01247              MOVE -1             TO BPFKL                         
01248              MOVE AL-UABON       TO BPFKA                         
01249              GO TO 8200-SEND-DATAONLY.                            
01250                                                                   
01251      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.           
01252      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.          
01253                                                                   
01254      MOVE PI-LOW-BENE-NAME       TO ELBENE2-BENE-NAME             
01255                                     WS-INPUT-NAME.                
01256      MOVE PI-LOW-BENE-PRIME      TO ELBENE2-PRIME-ALT.            
01257                                                                   
01258  5310-BENE-KEY-BUILT.                                             
01259                                                                   
01260      MOVE LOW-VALUES             TO EL114BO.                      
01261      MOVE EIGHT                  TO NAME-CNT.                     
01262                                                                   
01263      EXEC CICS HANDLE CONDITION                                   
01264          ENDFILE (5330-END-BROWSE)                                
01265      END-EXEC.                                                    
01266                                                                   
01267      EXEC CICS STARTBR                                            
01268          DATASET (ELBENE2-FILEID)                                 
01269          RIDFLD  (ELBENE2-KEY)                                    
01270          GTEQ                                                     
01271      END-EXEC.                                                    
01272                                                                   
01273  5320-READPREV-LOOP.                                              
01274                                                                   
01275      EXEC CICS READPREV                                           
01276          DATASET(ELBENE2-FILEID)                                  
01277          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01278          RIDFLD (ELBENE2-KEY)                                     
01279      END-EXEC.                                                    
01280                                                                   
01281      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
01282          GO TO 5330-END-BROWSE.                                   
01283                                                                   
01284      IF BE-RECORD-TYPE NOT EQUAL 'B'                              
01285          GO TO 5330-END-BROWSE.                                   
01286                                                                   
01287      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.                 
01288                                                                   
01289      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT                    
01290          VARYING NAME-IDX FROM ONE BY ONE                         
01291              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR        
01292                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL               
01293                      WS-READ-CHAR (NAME-IDX).                     
01294                                                                   
01295      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH                  
01296          GO TO 5330-END-BROWSE.                                   
01297                                                                   
01298      IF NAME-CNT EQUAL EIGHT                                      
01299          MOVE BE-MAIL-TO-NAME-A1 TO PI-HIGH-BENE-NAME             
01300          MOVE BE-BENEFICIARY     TO PI-HIGH-BENE-PRIME.           
01301                                                                   
01302      MOVE TBL-LINE-NUMBER (NAME-CNT)                              
01303                                  TO MAP-LINE-NUMBER (NAME-CNT).   
01304      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)      
01305                                     PI-LOW-BENE-NAME.             
01306      MOVE BE-BENEFICIARY         TO PI-LOW-BENE-PRIME             
01307                                     MAP-BENE-CNTL (NAME-CNT).     
01308      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).     
01309      SUBTRACT ONE              FROM NAME-CNT.                     
01310                                                                   
01311      IF NAME-CNT GREATER THAN +0                                  
01312          GO TO 5320-READPREV-LOOP.                                
01313                                                                   
01314  5330-END-BROWSE.                                                 
01315                                                                   
01316      EXEC CICS ENDBR                                              
01317          DATASET (ELBENE2-FILEID)                                 
01318      END-EXEC.                                                    
01319                                                                   
01320      IF NAME-CNT NOT EQUAL +0                                     
01321          ADD +1                  TO NAME-CNT                      
01322              PERFORM 5400-REARRANGE-SCREEN THRU 5400-EXIT         
01323                  VARYING SCREEN-CNT FROM ONE BY ONE               
01324                      UNTIL NAME-CNT GREATER THAN EIGHT            
01325      ELSE                                                         
01326          MOVE EIGHT              TO PI-MAX-SELECTION.             
01327                                                                   
01328      IF PI-MAX-SELECTION LESS THAN EIGHT                          
01329          MOVE PI-MAX-SELECTION   TO SCREEN-CNT                    
01330          PERFORM 5500-FILL-REMAINING-SLOTS THRU 5550-EXIT         
01331          MOVE ER-0130            TO EMI-ERROR                     
01332          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
01333                                                                   
01334                                                                   
01335      MOVE 'B'                    TO PI-PREV-DIRECTION.            
01336      MOVE MAP-NAME-B             TO WS-MAP-NAME                   
01337                                     PI-PREV-MAP-NAME.             
01338      MOVE -1                     TO BSELL.                        
01339      GO TO 8100-SEND-INITIAL-MAP.                                 
01340                                                                   
01341      EJECT                                                        
01342  5400-REARRANGE-SCREEN.                                           
01343                                                                   
01344      MOVE TBL-LINE-NUMBER (SCREEN-CNT)                            
01345                                  TO MAP-LINE-NUMBER (SCREEN-CNT). 
01346      MOVE MAP-BENE-NAME (NAME-CNT)                                
01347                                  TO MAP-BENE-NAME (SCREEN-CNT).   
01348      MOVE MAP-BENE-CITY (NAME-CNT)                                
01349                                  TO MAP-BENE-CITY (SCREEN-CNT).   
01350      MOVE MAP-BENE-CNTL (NAME-CNT)                                
01351                                  TO MAP-BENE-CNTL (SCREEN-CNT).   
01352                                                                   
01353      MOVE SPACES                 TO MAP-LINE-NUMBER (NAME-CNT)    
01354                                     MAP-BENE-NAME   (NAME-CNT)    
01355                                     MAP-BENE-CITY   (NAME-CNT)    
01356                                     MAP-BENE-CNTL   (NAME-CNT).   
01357                                                                   
01358      ADD +1                      TO NAME-CNT.                     
01359      MOVE SCREEN-CNT             TO PI-MAX-SELECTION.             
01360                                                                   
01361  5400-EXIT.                                                       
01362      EXIT.                                                        
01363                                                                   
01364      EJECT                                                        
01365  5500-FILL-REMAINING-SLOTS.                                       
01366                                                                   
01367      MOVE PI-COMPANY-CD          TO ELBENE2-COMPANY-CD.           
01368      MOVE 'B'                    TO ELBENE2-RECORD-TYPE.          
01369      MOVE MAP-BENE-NAME (SCREEN-CNT)                              
01370                                  TO ELBENE2-BENE-NAME             
01371                                     WS-INPUT-NAME.                
01372      MOVE MAP-BENE-CNTL (SCREEN-CNT)                              
01373                                  TO ELBENE2-PRIME-ALT.            
01374                                                                   
01375      MOVE SCREEN-CNT             TO NAME-CNT.                     
01376                                                                   
01377  5510-BENE-KEY-BUILT.                                             
01378                                                                   
01379      EXEC CICS HANDLE CONDITION                                   
01380          ENDFILE (5530-END-BROWSE)                                
01381      END-EXEC.                                                    
01382                                                                   
01383      EXEC CICS STARTBR                                            
01384          DATASET (ELBENE2-FILEID)                                 
01385          RIDFLD  (ELBENE2-KEY)                                    
01386          GTEQ                                                     
01387      END-EXEC.                                                    
01388                                                                   
01389  5520-READNEXT-LOOP.                                              
01390                                                                   
01391      EXEC CICS READNEXT                                           
01392          DATASET(ELBENE2-FILEID)                                  
01393          SET    (ADDRESS OF BENEFICIARY-MASTER)                   
01394          RIDFLD (ELBENE2-KEY)                                     
01395      END-EXEC.                                                    
01396                                                                   
01397      IF BE-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
01398          GO TO 5530-END-BROWSE.                                   
01399                                                                   
01400      IF BE-RECORD-TYPE NOT EQUAL 'B'                              
01401          GO TO 5530-END-BROWSE.                                   
01402                                                                   
01403      MOVE BE-MAIL-TO-NAME-A1     TO WS-READ-NAME.                 
01404                                                                   
01405      PERFORM 9999-DUMMY-ROUTINE THRU 9999-EXIT                    
01406          VARYING NAME-IDX FROM ONE BY ONE                         
01407              UNTIL NAME-IDX GREATER THAN PI-NAME-LENGTH OR        
01408                  WS-INPUT-CHAR (NAME-IDX) NOT EQUAL               
01409                      WS-READ-CHAR (NAME-IDX).                     
01410                                                                   
01411      IF NAME-IDX NOT GREATER THAN PI-NAME-LENGTH                  
01412          GO TO 5530-END-BROWSE.                                   
01413                                                                   
01414      MOVE TBL-LINE-NUMBER (NAME-CNT)                              
01415                                  TO MAP-LINE-NUMBER (NAME-CNT).   
01416      MOVE BE-MAIL-TO-NAME-A1     TO MAP-BENE-NAME (NAME-CNT)      
01417                                     PI-HIGH-BENE-NAME.            
01418      MOVE BE-BENEFICIARY         TO PI-HIGH-BENE-PRIME            
01419                                     MAP-BENE-CNTL (NAME-CNT).     
01420      MOVE BE-CITY-STATE          TO MAP-BENE-CITY (NAME-CNT).     
01421      ADD ONE                     TO NAME-CNT.                     
01422                                                                   
01423      IF NAME-CNT LESS THAN NINE                                   
01424          GO TO 5520-READNEXT-LOOP.                                
01425                                                                   
01426  5530-END-BROWSE.                                                 
01427                                                                   
01428      EXEC CICS ENDBR                                              
01429          DATASET (ELBENE2-FILEID)                                 
01430      END-EXEC.                                                    
01431                                                                   
01432  5550-EXIT.                                                       
01433      EXIT.                                                        
01434                                                                   
01435      EJECT                                                        
01436  6000-EDIT-INPUT-DATA.
081312
081312     IF ABENEI EQUAL SPACES
081312        MOVE -1                  TO ABENEL
081312        MOVE ER-7668             TO EMI-ERROR
081312        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
081312     END-IF.
081312
01437      IF ABENAMEL EQUAL ZEROS                                      
01438         MOVE -1                  TO ABENAMEL                      
01439         MOVE ER-7672             TO EMI-ERROR                     
01440         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01441                                                                   
01442      IF AADDR1L EQUAL ZEROS                                       
01443         MOVE -1                  TO AADDR1L                       
01444         MOVE ER-7673             TO EMI-ERROR                     
01445         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
01446                                                                   
01447      IF APHONEL GREATER THAN ZERO                                 
01448          EXEC CICS BIF DEEDIT                                     
01449              FIELD  (APHONEI)                                     
01450              LENGTH (APHONE-LENGTH)                               
01451          END-EXEC                                                 
01452          IF APHONEI NUMERIC                                       
01453              MOVE AL-UNNON       TO APHONEA                       
01454              MOVE APHONEI        TO APHONEO                       
01455              INSPECT APHONEO CONVERTING SPACES TO '-'             
01456          ELSE                                                     
01457              MOVE ER-0053        TO EMI-ERROR                     
01458              PERFORM 9900-ERROR-FORMAT                            
01459              MOVE AL-UNBON       TO APHONEA                       
01460              MOVE -1             TO APHONEL.                      
01461                                                                   
01462      IF ACITYL EQUAL ZEROS                                      
01463         MOVE -1                  TO ACITYL
01464         MOVE AL-UABON            TO ACITYA
01465         MOVE ER-7674             TO EMI-ERROR
01466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01467      ELSE
01468         MOVE AL-UANON            TO ACITYA.
01469                                                                   
01462      IF (ASTATEL = +0)
              AND (AMAINTI = 'A')
01463         MOVE -1                  TO ASTATEL
01464         MOVE AL-UABON            TO ASTATEA
01465         MOVE ER-7674             TO EMI-ERROR
01466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01467      ELSE
01468         MOVE AL-UANON            TO ASTATEA.
01469                                                                   
           IF ASTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE ASTATEI             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 MOVE AL-UANON         TO ASTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO ASTATEL
                 MOVE AL-UABON         TO ASTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

01470      IF AZIPCDEL EQUAL ZEROS                                      
01471         MOVE -1                  TO AZIPCDEL                      
01472         MOVE AL-UABON            TO AZIPCDEA                      
01473         MOVE ER-7676             TO EMI-ERROR                     
01474         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01475      ELSE                                                         
01476         MOVE AL-UANON            TO AZIPCDEA.                     
01477                                                                   
01478      IF GRPCHKL GREATER THAN +0                                   
01479         IF GRPCHKI EQUAL ' ' OR 'Y' OR 'N'                        
01480            MOVE AL-UANON               TO GRPCHKA                 
01481         ELSE                                                      
01482            MOVE ER-0658                TO EMI-ERROR               
01483            MOVE -1                     TO GRPCHKL                 
01484            MOVE AL-UABON               TO GRPCHKA                 
01485            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01486                                                                   
01487      IF ACORRESL GREATER THAN +0                                  
01488         MOVE AL-UANON                  TO ACORRESA.               
01489                                                                   
01490      IF ACADDR1L GREATER THAN +0                                  
01491         MOVE AL-UANON                  TO  ACADDR1A.              
01492                                                                   
01493      IF ACADDR2L GREATER THAN +0                                  
01494         MOVE AL-UANON                  TO  ACADDR2A.              
01495                                                                   
01496      IF ACADDR3L GREATER THAN +0                                  
01497         MOVE AL-UANON                  TO ACADDR3A.               
01498                                                                   
01499      IF ACCITYL GREATER THAN +0                                  
01500         MOVE AL-UANON                  TO ACCITYA.

           IF ACSTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE ACSTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 MOVE AL-UANON         TO ACSTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO ACSTATEL
                 MOVE AL-UABON         TO ACSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

01502      IF ACZPCDEL GREATER THAN +0                                  
01503         MOVE AL-UANON                  TO ACZPCDEA.               
01504                                                                   
01505      IF ACPHONEL GREATER THAN ZERO                                
01506          EXEC CICS BIF DEEDIT                                     
01507              FIELD  (ACPHONEI)                                    
01508              LENGTH (APHONE-LENGTH)                               
01509          END-EXEC                                                 
01510          IF ACPHONEI NUMERIC                                      
01511              MOVE AL-UNNON       TO ACPHONEA                      
01512              MOVE ACPHONEI       TO ACPHONEO                      
01513              INSPECT ACPHONEO CONVERTING SPACES TO '-'            
01514          ELSE                                                     
01515              MOVE ER-0053        TO EMI-ERROR                     
01516              PERFORM 9900-ERROR-FORMAT                            
01517              MOVE AL-UNBON       TO ACPHONEA                      
01518              MOVE -1             TO ACPHONEL.                     
01519                                                                   
01549                                                                   
01550      IF AFAXNOL   GREATER THAN +0                                 
01551          EXEC CICS BIF DEEDIT                                     
01552              FIELD  (AFAXNOI)                                     
01553              LENGTH (APHONE-LENGTH)                               
01554          END-EXEC                                                 
01555          IF AFAXNOI NUMERIC                                       
01556              MOVE AL-UNNON       TO AFAXNOA                       
01557              MOVE AFAXNOI        TO AFAXNOO                       
01558              INSPECT AFAXNOO CONVERTING SPACES TO '-'             
01559          ELSE                                                     
01560              MOVE ER-0053        TO EMI-ERROR                     
01561              PERFORM 9900-ERROR-FORMAT                            
01562              MOVE AL-UNBON       TO AFAXNOA                       
01563              MOVE -1             TO AFAXNOL.                      
01564                                                                   
01565      IF ACBSOTL   GREATER THAN +0                                 
01566         IF ACBSOTI EQUAL ' ' OR 'F' OR 'P'                        
01567            MOVE AL-UANON               TO ACBSOTA                 
01568         ELSE                                                      
01569            MOVE ER-8125                TO EMI-ERROR               
01570            MOVE -1                     TO ACBSOTL                 
01571            MOVE AL-UABON               TO ACBSOTA                 
01572            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              

           IF AACHYNL > +0
              IF AACHYNI = ' ' OR 'Y' OR 'N'
                 MOVE AL-UANON         TO AACHYNA
              ELSE
032019           MOVE ER-7665          TO EMI-ERROR
                 MOVE AL-UABON         TO AACHYNA
                 MOVE -1               TO AACHYNL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

032019     IF AACHEYNL > +0
032019        IF AACHEYNI = ' ' OR 'Y' OR 'N'
032019           MOVE AL-UANON         TO AACHEYNA
032019        ELSE
032019           MOVE ER-7664          TO EMI-ERROR
032019           MOVE AL-UABON         TO AACHEYNA
032019           MOVE -1               TO AACHEYNL
032019           PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019        END-IF
032019     END-IF

           if asubtypl > +0
              move al-uanon            to asubtypa
           end-if

032019     if aemaill > +0
032019        move al-uanon            to aemaila
032019        move aemaili             to ws-work-email-address
032019        if ws-work-email-address not = spaces
032019           perform 6100-check-email-reasonability
032019                                 thru 6100-exit
032019           if invalid-email
032019              move er-7667       to emi-error
032019              MOVE AL-UABON      TO Aemaila
032019              MOVE -1            TO Aemaill 
032019              PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019           end-if
032019        end-if
032019     end-if

032019     if aacheynl > +0
032019        and aacheyni = 'Y'
032019        and (aemaili = spaces or aemaill = +0)
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill 
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
              
           IF AABANOL > +0
              MOVE AL-UANON            TO AABANOA
           END-IF

           IF AACCTNOL > +0
032019        if pi-approval-level = '4' or '5'
032019           MOVE AL-UANON         TO aacctnoa
032019        else
032019           move er-2565          to emi-error
032019           move -1               to aacctnol
032019           MOVE AL-UABON         TO aacctnoa                      
032019           PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019        end-if
           END-IF

032019     if amainti = 'A'
032019        go to 6000-exit
032019     end-if
032019
032019     display ' made it to edit change ' abenei
032019     MOVE PI-COMPANY-CD          TO ELBENE-COMPANY-CD
032019     MOVE 'B'                    TO ELBENE-RECORD-TYPE
032019     MOVE ABENEI                 TO ELBENE-BENEFICIARY
032019
032019     EXEC CICS READ                                               
032019         DATASET(ELBENE-FILEID)                                   
032019         SET    (ADDRESS OF BENEFICIARY-MASTER)                   
032019         RIDFLD (ELBENE-KEY)
032019         resp   (ws-response)
032019     END-EXEC
032019
032019     if not resp-normal
032019        go to 6000-exit
032019     end-if
032019
032019     if (aacheyni = 'Y')
032019        and ((aemaili = spaces or low-values)
032019                        and
032019        (be-ach-email-addr = spaces or low-values))
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill 
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
032019
032019     if (aacheynl = zeros)
032019        and (BE-ACH-EMAIL-YN = 'Y')
032019        and (aemaili = spaces or low-values)
032019        move er-7663             to emi-error
032019        MOVE AL-UABON            TO Aemaila
032019        MOVE -1                  TO Aemaill 
032019        PERFORM 9900-ERROR-FORMAT
032019                                 THRU 9900-EXIT
032019     end-if
032019
           .
01574  6000-EXIT.                                                       
01575      EXIT.                                                        

032019 6100-check-email-reasonability.
032019
032019     move zeros                  to ws-tally-counter
032019     move 9                      to ws-email-sw
032019
032019     inspect ws-work-email-address
032019        tallying ws-tally-counter for all '@'
032019     if ws-tally-counter not = +1
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019     perform varying e1 from +35 by -1 until
032019        ws-work-email-address(e1:1) not = spaces
032019     end-perform
032019     if e1 < +5
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019     if ws-work-email-address (e1 - 3:4) not = '.COM' and
032019        '.ORG' and '.NET'
032019        set invalid-email to true
032019        go to 6100-exit
032019     end-if
032019
032019     set valid-email to true
032019
032019     .
032019 6100-exit.
032019     exit.

01578  7000-BUILD-OUTPUT-MAP.                                           
01579                                                                   
01580      MOVE LOW-VALUES             TO EL114AO.                      
01581      MOVE BE-BENEFICIARY         TO ABENEO.                       
01582      MOVE BE-MAIL-TO-NAME        TO ABENAMEO.                     
01583      MOVE BE-ADDRESS-LINE-1      TO AADDR1O.                      
01584      MOVE BE-ADDRESS-LINE-2      TO AADDR2O.                      
01585      MOVE BE-ADDRESS-LINE-3      TO AADDR3O.                      
01586                                                                   
01587      IF BE-PHONE-NO NOT NUMERIC                                   
01588          MOVE ZEROS              TO BE-PHONE-NO.                  
01589                                                                   
01590      MOVE BE-PHONE-NO            TO APHONEO.                      
01591                                                                   
01592      INSPECT APHONEO CONVERTING SPACES TO '-'.                    
01593                                                                   
01594      MOVE BE-CITY                TO ACITYO
           MOVE BE-STATE               TO ASTATEO
01595                                                                   
01596      MOVE SPACES                 TO WS-ZIP-CODE.                  
01597      IF BE-CANADIAN-POST-CODE                                     
01598          MOVE BE-CAN-POSTAL-1    TO WS-ZIP-CAN-2-POST1            
01599          MOVE BE-CAN-POSTAL-2    TO WS-ZIP-CAN-2-POST2            
01600      ELSE                                                         
01601          MOVE BE-ZIP-PRIME       TO WS-ZIP-AM-2-CODE              
01602          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                 
01603              MOVE '-'            TO WS-ZIP-AM-2-DASH              
01604              MOVE BE-ZIP-PLUS4   TO WS-ZIP-AM-2-PLUS4.            
01605                                                                   
01606      MOVE WS-ZIP-CODE            TO AZIPCDEO.                     
01607                                                                   
01608      MOVE BE-LAST-MAINT-BY       TO ALUBYO.                       
01609      MOVE ' '                    TO DC-OPTION-CODE.               
01610      MOVE BE-LAST-MAINT-DT       TO DC-BIN-DATE-1.                
01611      MOVE LINK-ELDATCV           TO PGM-NAME.                     
01612      EXEC CICS LINK                                               
01613          PROGRAM (PGM-NAME)                                       
01614          COMMAREA(DATE-CONVERSION-DATA)                           
01615          LENGTH  (DC-COMM-LENGTH)                                 
01616      END-EXEC.                                                    
01617                                                                   
01618      IF DATE-CONVERSION-ERROR                                     
01619          MOVE ZEROS              TO ALUDATEO                      
01620      ELSE                                                         
01621          MOVE DC-GREG-DATE-1-EDIT TO ALUDATEO.                    
01622                                                                   
01623      MOVE BE-LAST-MAINT-HHMMSS   TO ALUTIMEO.                     
01624      INSPECT ALUTIMEI CONVERTING SPACES TO '.'.                   
01625      MOVE -1                     TO AMAINTL.                      
01626      MOVE BE-LAST-MAINT-BY       TO PI-UPDATE-BY.                 
01627      MOVE BE-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             
01628      MOVE BE-BENEFICIARY         TO PI-PREV-BENEFICIARY.          
01629      MOVE AL-UANOF               TO AMAINTA.                      
01630      MOVE AL-UANON               TO ABENEA.                       
01631                                                                   
01632      IF BE-GROUP-CHECKS-Y-N EQUAL 'Y'                             
01633         MOVE 'Y'                 TO GRPCHKO                       
01634      ELSE                                                         
01635         MOVE 'N'                 TO GRPCHKO.                      
01636                                                                   
01637      MOVE BE-MAIL-TO-NAME2       TO ACORRESO.                     
01638      MOVE BE-ADDRESS-LINE-12     TO ACADDR1O.                     
01639      MOVE BE-ADDRESS-LINE-22     TO ACADDR2O.                     
01640      MOVE BE-ADDRESS-LINE-32     TO ACADDR3O.                     
01641      MOVE BE-CITY2               TO ACCITYO
01641      MOVE BE-STATE2              TO ACSTATEO
01642                                                                   
01643      MOVE SPACES                 TO WS-ZIP-CODE.                  
01644      IF BE-CANADIAN-POST-CODE2                                    
01645          MOVE BE-CAN-POSTAL-12   TO WS-ZIP-CAN-2-POST1            
01646          MOVE BE-CAN-POSTAL-22   TO WS-ZIP-CAN-2-POST2            
01647      ELSE                                                         
01648          MOVE BE-ZIP-PRIME2      TO WS-ZIP-AM-2-CODE              
01649          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS                  
01650              MOVE '-'            TO WS-ZIP-AM-2-DASH              
01651              MOVE BE-ZIP-PLUS42  TO WS-ZIP-AM-2-PLUS4.            
01652                                                                   
01653      MOVE WS-ZIP-CODE            TO ACZPCDEO.                     
01654                                                                   
01655      IF BE-PHONE-NO2 NOT NUMERIC                                  
01656          MOVE ZEROS              TO BE-PHONE-NO2.                 
01657      MOVE BE-PHONE-NO2           TO ACPHONEO.                     
01658      INSPECT ACPHONEO CONVERTING SPACES TO '-'.                   
01659                                                                   
032019*    IF BE-BSR-PHONE-NUM NOT NUMERIC                              
032019*       MOVE ZEROS               TO BE-BSR-PHONE-NUM.             
01668                                                                   
01669      IF BE-BSR-FAX-NUM NOT NUMERIC                                
01670         MOVE ZEROS               TO BE-BSR-FAX-NUM.               
01671      MOVE BE-BSR-FAX-NUM         TO AFAXNOO.                      
01672      INSPECT AFAXNOO CONVERTING SPACES TO '-'.                    
01673                                                                   
032019*    MOVE BE-OUTPUT-TYPE         TO ACBSOTO.                      

           IF BE-ACH-YES-OR-NO = 'Y'
              MOVE 'Y'                 TO AACHYNO
           ELSE
              MOVE 'N'                 TO AACHYNO
           END-IF

032019     if be-ach-email-yn = 'Y'
032019        move 'Y'                 to aacheyno
032019     else
032019        move 'N'                 to aacheyno
032019     end-if
032019
032019     if be-ach-email-addr <> spaces
032019        move be-ach-email-addr   to aemailo
032019     end-if

           move be-ach-sub-type        to asubtypo
           MOVE BE-ACH-ABA-ROUTING-NUMBER
                                       TO AABANOO
032019     move be-ach-email-addr      to aemailo
           if pi-approval-level = '4' or '5'
              MOVE BE-ACH-BANK-ACCOUNT-NUMBER
                                       TO AACCTNOO
           else
              if be-ach-bank-account-number not = spaces
                 move '********************'
                                       to aacctnoo
              end-if
           end-if
01676      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                
01677          MOVE BE-CARRIER         TO ACARRO.                       
01678                                                                   
01679      EJECT                                                        
01680  8100-SEND-INITIAL-MAP.                                           
01681                                                                   
01682      IF WS-MAP-NAME IS EQUAL TO MAP-NAME-A                        
01683          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')            
01684              NEXT SENTENCE                                        
01685          ELSE                                                     
01686              MOVE AL-PADOF         TO ACARRA                      
01687              MOVE SPACES           TO ACARHDGO                    
01688                                       ACARRO.                     
01689                                                                   
01690      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
01691          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                     
01692          MOVE EIBTIME              TO TIME-IN                     
01693          MOVE SAVE-DATE            TO ADATEO                      
01694          MOVE TIME-OUT             TO ATIMEO                      
01695          MOVE PI-COMPANY-ID        TO ACOMPO                      
01696      ELSE                                                         
01697          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O                     
01698          MOVE EIBTIME              TO TIME-IN                     
01699          MOVE SAVE-DATE            TO BDATEO                      
01700          MOVE TIME-OUT             TO BTIMEO                      
01701          MOVE PI-COMPANY-ID        TO BCOMPO.                     
01702                                                                   

013017     if ws-map-name = map-name-a
              if pi-approval-level = '4' or '5'
                 continue
              else
                 move al-panof         to aachyna
                                          aabanoa
                                          asubtypa
                 move al-padof         to aacctnoa
              end-if
           end-if

01703      EXEC CICS SEND                                               
01704          MAP   (WS-MAP-NAME)                                      
01705          MAPSET(MAPSET-NAME)                                      
01706          FROM  (EL114BO)                                          
01707          ERASE                                                    
01708          CURSOR                                                   
01709      END-EXEC.                                                    
01710                                                                   
01711      GO TO 9100-RETURN-TRAN.                                      
01712                                                                   
01713      EJECT                                                        
01714  8200-SEND-DATAONLY.                                              
01715                                                                   
01716      IF WS-MAP-NAME IS EQUAL TO MAP-NAME-A                        
01717          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')            
01718              NEXT SENTENCE                                        
01719          ELSE                                                     
01720              MOVE AL-PADOF         TO ACARRA                      
01721              MOVE SPACES           TO ACARHDGO                    
01722                                       ACARRO.                     
01723                                                                   
01724      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
01725          MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O                     
01726          MOVE EIBTIME              TO TIME-IN                     
01727          MOVE SAVE-DATE            TO ADATEO                      
01728          MOVE TIME-OUT             TO ATIMEO                      
01729          MOVE PI-COMPANY-ID        TO ACOMPO                      
01730      ELSE                                                         
01731          MOVE EMI-MESSAGE-AREA (1) TO BEMSG1O                     
01732          MOVE EIBTIME              TO TIME-IN                     
01733          MOVE SAVE-DATE            TO BDATEO                      
01734          MOVE TIME-OUT             TO BTIMEO                      
01735          MOVE PI-COMPANY-ID        TO BCOMPO.                     
01736                                                                   
01737      EXEC CICS SEND                                               
01738          MAP   (WS-MAP-NAME)                                      
01739          MAPSET(MAPSET-NAME)                                      
01740          FROM  (EL114BO)                                          
01741          DATAONLY                                                 
01742          ERASEAUP                                                 
01743          CURSOR                                                   
01744      END-EXEC.                                                    
01745                                                                   
01746      GO TO 9100-RETURN-TRAN.                                      
01747      EJECT                                                        
01748  8300-SEND-TEXT.                                                  
01749      EXEC CICS SEND TEXT                                          
01750          FROM  (LOGOFF-TEXT)                                      
01751          LENGTH(LOGOFF-LENGTH)                                    
01752          ERASE                                                    
01753          FREEKB                                                   
01754      END-EXEC.                                                    
01755                                                                   
01756      EXEC CICS RETURN                                             
01757          END-EXEC.                                                
01758                                                                   
01759      EJECT                                                        
01760  8800-UNAUTHORIZED-ACCESS.                                        
01761      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
01762      GO TO 8300-SEND-TEXT.                                        
01763                                                                   
01764  8810-PF23.                                                       
01765      MOVE EIBAID                 TO PI-ENTRY-CD-1.                
01766      MOVE XCTL-005               TO PGM-NAME.                     
01767      GO TO 9300-XCTL.                                             
01768                                                                   
01769  8850-DUPREC.                                                     
01770      MOVE ER-0132                TO EMI-ERROR.                    
01771      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01772      MOVE -1                     TO ABENEL.                       
01773      MOVE AL-UABON               TO ABENEA.                       
01774      GO TO 8100-SEND-INITIAL-MAP.                                 
01775                                                                   
01776  8860-ENDFILE.                                                    
01777                                                                   
01778      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
01779          MOVE -1                 TO AMAINTL                       
01780      ELSE                                                         
01781          MOVE -1                 TO BPFKL.                        
01782                                                                   
01783      MOVE ER-0130                TO EMI-ERROR.                    
01784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01785      GO TO 8200-SEND-DATAONLY.                                    
01786                                                                   
01787  8870-NOTOPEN.                                                    
01788                                                                   
01789      IF WS-MAP-NAME EQUAL MAP-NAME-A                              
01790          MOVE LOW-VALUES         TO EL114AO                       
01791          MOVE -1                 TO AMAINTL                       
01792      ELSE                                                         
01793          MOVE LOW-VALUES         TO EL114BO                       
01794          MOVE -1                 TO BPFKL.                        
01795                                                                   
01796      MOVE ER-7675                TO EMI-ERROR.                    
01797      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01798      GO TO 8100-SEND-INITIAL-MAP.                                 
01799                                                                   
01800  8880-NOT-FOUND.                                                  
01801      MOVE ER-0565                TO EMI-ERROR.                    
01802      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01803      MOVE -1                     TO ABENEL.                       
01804      MOVE AL-UABON               TO ABENEA.                       
01805      MOVE AL-UANON               TO AMAINTA.                      
01806      GO TO 8100-SEND-INITIAL-MAP.                                 
01807                                                                   
01808  9000-RETURN-CICS.                                                
01809      EXEC CICS RETURN                                             
01810      END-EXEC.                                                    
01811                                                                   
01812      EJECT                                                        
01813  9100-RETURN-TRAN.                                                
01814      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
01815      MOVE '114A' TO PI-CURRENT-SCREEN-NO.                         
01816                                                                   
01817      EXEC CICS RETURN                                             
01818          TRANSID (TRANS-ID)                                       
01819          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
01820          LENGTH  (PI-COMM-LENGTH)                                 
01821      END-EXEC.                                                    
01822                                                                   
01823  9200-RETURN-MAIN-MENU.                                           
01824      MOVE XCTL-126               TO PGM-NAME.                     
01825                                                                   
01826      IF PI-SESSION-IN-PROGRESS EQUAL '4'                          
01827          MOVE XCTL-400           TO PGM-NAME.                     
01828                                                                   
01829      IF PI-SESSION-IN-PROGRESS EQUAL '5'                          
01830          MOVE XCTL-800           TO PGM-NAME.                     
01831                                                                   
01832      GO TO 9300-XCTL.                                             
01833                                                                   
01834  9300-XCTL.                                                       
01835      EXEC CICS XCTL                                               
01836          PROGRAM (PGM-NAME)                                       
01837          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
01838          LENGTH  (PI-COMM-LENGTH)                                 
01839      END-EXEC.                                                    
01840                                                                   
01841  9400-CLEAR.                                                      
01842      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
01843      GO TO 9300-XCTL.                                             
01844                                                                   
01845  9500-PF12.                                                       
01846      MOVE XCTL-010               TO PGM-NAME.                     
01847      GO TO 9300-XCTL.                                             
01848                                                                   
01849  9600-PGMID-ERROR.                                                
01850      EXEC CICS HANDLE CONDITION                                   
01851          PGMIDERR(8300-SEND-TEXT)                                 
01852      END-EXEC.                                                    
01853                                                                   
01854      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
01855      MOVE ' '                    TO PI-ENTRY-CD-1.                
01856      MOVE XCTL-005               TO PGM-NAME.                     
01857      MOVE PGM-NAME               TO LOGOFF-PGM.                   
01858      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
01859      GO TO 9300-XCTL.                                             
01860                                                                   
01861      EJECT                                                        
01862  9700-LINK-DATE-CONVERT.                                          
01863      EXEC CICS LINK                                               
01864          PROGRAM    ('ELDATCV')                                   
01865          COMMAREA   (DATE-CONVERSION-DATA)                        
01866          LENGTH     (DC-COMM-LENGTH)                              
01867      END-EXEC.                                                    
01868                                                                   
01869  9700-EXIT.                                                       
01870      EXIT.                                                        
01871                                                                   
01872                                                                   
01873                                                                   
01874  9900-ERROR-FORMAT.                                               
01875      IF NOT EMI-ERRORS-COMPLETE                                   
01876          MOVE LINK-001           TO PGM-NAME                      
01877          EXEC CICS LINK                                           
01878              PROGRAM(PGM-NAME)                                    
01879              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              
01880              LENGTH(EMI-COMM-LENGTH)                              
01881          END-EXEC.                                                
01882                                                                   
01883  9900-EXIT.                                                       
01884      EXIT.                                                        
01885                                                                   
01886                                                                   
01887                                                                   
01888                                                                   
01889  9990-ABEND.                                                      
01890      MOVE LINK-004               TO PGM-NAME.                     
01891      MOVE DFHEIBLK               TO EMI-LINE1.                    
01892      EXEC CICS LINK                                               
01893          PROGRAM   (PGM-NAME)                                     
01894          COMMAREA  (EMI-LINE1)                                    
01895          LENGTH    (72)                                           
01896      END-EXEC.                                                    
01897                                                                   
01898      GO TO 8100-SEND-INITIAL-MAP.                                 
01899                                                                   
01900      GOBACK.                                                      
01901                                                                   
01902      EJECT                                                        
01903  9995-SECURITY-VIOLATION.                                         
01904                              COPY ELCSCTP.                        
01905                                                                   
01906  9995-EXIT.                                                       
01907      EXIT.                                                        
01908                                                                   
01909  9999-DUMMY-ROUTINE.                                              
01910                                                                   
01911  9999-EXIT.                                                       
01912      EXIT.                                                        
01913                                                                   
