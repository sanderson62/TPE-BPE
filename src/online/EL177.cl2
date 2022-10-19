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
121903*           2002100700001  SMVA  ADD ACCT STATE AND CLM TYPE FOR PRINTING
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
LGC007                             COPY ELCINTF.                        
LGC007            COPY ELC176PI.                                        
                                                                        
               16  PI-TEMP-STORAGE-ITEM    PICTURE S9(4)                
                                           COMPUTATIONAL                
                                           SYNCHRONIZED.                
                                                                        
           EJECT                                                        
LGC007                            COPY ELC176W2.                        
                                                                        
           EJECT                                                        
LGC007                            COPY ELC176W1.    
                                                                        
           EJECT                                                        
LGC007                            COPY ELC176W3.   
                                                                        
           EJECT                                                        
LGC007                            COPY ELCCSOCL.                         
                                                                        
           EJECT                                                        
LGC007                            COPY ELCDATE.                           
                                                                        
LGC007*01  S-WORK-AREA COPY ELPRTCVD.                                   
LGC007*                       COPY ELPRTCVD.                            
                                                                        
CSOMOD 01  CSO-DRAFT-420C.                                              
CSODJN     05  CSO-DRAFT-KEY           PIC X(19).                       
011105     05  FILLER                  PIC X(1235).                      
CSOMOD 01  CSO-DRAFT-420C-RED REDEFINES CSO-DRAFT-420C.                 
CSOMOD     COPY MICR420C.                                               
CSOMOD 01  CSO-RESP                    PIC S9(5) COMP.                  
CSOMOD 01  CSO-ZIP                     PIC Z(9).                        
LGC007     EJECT                                                        
DMBMOD                     COPY ELCCPA.                                 
           EJECT                                                        
       LINKAGE SECTION.                                                 
                                                                        
LGC007 01  DFHCOMMAREA                 PIC X(1024).             
                                                                        
           EJECT                                                        
       PROCEDURE DIVISION.                                              
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               QIDERR  (5000-MAIN-LOGIC)                                
               ITEMERR (5000-MAIN-LOGIC)                                
               ERROR   (9990-ERROR) END-EXEC.                           
                                                                        
           EJECT                                                        
       0010-MAIN-LOGIC.                                                 
                                                                        
                                                                        
           EXEC CICS RETRIEVE                                           
               INTO   (PROGRAM-INTERFACE-BLOCK)                         
               LENGTH (PI-COMM-LENGTH) END-EXEC                         
                                                                        
           MOVE +1                     TO  PI-TEMP-STORAGE-ITEM.        
                                                                        

       0100-MAIN-LOGIC.                                                 

                                                                        
           EXEC CICS READQ TS                                           
               INTO   (CHECK-PASS-AREA)
               QUEUE  (PI-TEMP-STORAGE-KEY)                             
               ITEM   (PI-TEMP-STORAGE-ITEM)                            
               LENGTH (WS-TS-LENGTH)                                    
CIDMOD     END-EXEC
                                                                        
           IF WS-TS-LENGTH NOT GREATER THAN +1                          
               EXEC CICS DELETEQ TS                                     
                   QUEUE (PI-TEMP-STORAGE-KEY) END-EXEC                 
               EXEC CICS RETURN                                         
                   END-EXEC.                                            
                                                                        
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

CSOMOD     EXEC CICS WRITE FILE('MICRDRFT') FROM(CSO-DRAFT-420C)        
121903        LENGTH(1254) RESP(CSO-RESP) RIDFLD(CSO-DRAFT-KEY)         
CSOMOD        END-EXEC.                                                 
CSOMOD     GO TO 0100-MAIN-LOGIC.                                       
           EJECT                                                        
                                                                        
       5000-MAIN-LOGIC.

           EXEC CICS DELAY
               INTERVAL (WS-DELAY-INTERVAL) 
           END-EXEC

           GO TO 0100-MAIN-LOGIC.

       8500-DATE-CONVERSION SECTION.                                    
                                                                        
                                                                        
           EXEC CICS LINK                                               
LGC007         PROGRAM  ('ELDATCV')                                     
               COMMAREA (DATE-CONVERSION-DATA)                          
               LENGTH   (DC-COMM-LENGTH) END-EXEC.                      
                                                                        
                                                                        
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
LGC007 SPELL-DOLLAR SECTION. COPY ELC176P1.                             
                                                                        
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
JGEND      EXEC CICS LINK                                               
LGC007         PROGRAM  ('EL004')                                       
JGEND          COMMAREA (DFHEIBLK)                                      
JGEND          LENGTH   (64) END-EXEC.                                  
JGEND                                                                   
JGEND                                                                   
JGEND  9990-EXIT.                                                       
JGEND                                                                   
JGEND      EXIT.                                                        
JGEND                                                                   
JGEND      SKIP3                                                        
JGEND  9999-LAST-PARAGRAPH SECTION.                                     
JGEND                                                                   
JGEND      GOBACK.                                                      
JGEND                                                                   
