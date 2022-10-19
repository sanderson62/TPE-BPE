      *TITLE 'CID CLAIM INTERFACE'
                                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB164.
      *AUTHOR.        DAN DRYDEN.
      *DATE-WRITTEN.  MAY, 1998.
                                                                        
      ***************************************************************** 
      *                      H I S T O R Y                            * 
      ***************************************************************** 
      * NAME    DATE    DESCRIPTION                                   * 
      * ---- ---------- --------------------------------------------- * 
      * DANA 01/01/1999 CR#1998011500013 FREEDOM SYSTEM INSTALL
CSODJN* DJNA 04/01/2000 CR#2000030100009 DRAFT NUMBER EXPANSION
CSODAN* DANA 08/29/2000 CR#2000082900005 DRAFT RECORD LENGTH CHANGE
021601* DANA 02/16/2001 MEMO FROM TPT -  CHECK DR-NOTE-CODE FOR 01
030702* SMVA 03/07/2002 2001112600001    REMOVE PLAN CODE TABLE
030702*                                DEPENDENCIES
103002* PEMA 10/30/2002                  ADD PROCESSING FOR DCC
122205* 122205    2005033100001  PEMA  ADD PROCESSING FOR CSI
112906* 112906  CR2006111300003  PEMA  ADD PROCESSING FOR KY
091808* 091808    2008022800002  AJRA  ADD PAYEE STATE TO OUTPUT FOR 
091808*                                ALASKA CHECK PROCESSING.
101708* 101708    2008050500001  AJRA  ADD PROCESSING FOR CCC
022812* 022812    2011110200001  AJRA  ADD PROCESSING FOR AHL
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
021215* 021215  IR2015020900001  PEMA  CORRECTLY ID LPAC,CCC,CSI
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
040616* 040616  CR2016032400001  PEMA  MORE CARRIER 7 CHANGES
111616* 111616  CR2016092200001  PEMA  Remove CSI processing
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
022017* 022017 CR2017022000001   PEMA  DCC DRAFTS TO CHECKS
      *****************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT DAILY-DRAFTS                                          
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT WORK-FILE
               ASSIGN TO SYS011
pemuni         organization is line sequential.

101708     SELECT CCC-WORK-FILE
101708         ASSIGN TO SYS013
101708         organization is line sequential.
101708
103002     SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  DAILY-DRAFTS                                                 
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
CSODJN**** RECORD CONTAINS 1194 CHARACTERS                              
           BLOCK CONTAINS 0 RECORDS.                                    
           COPY CLO420.                                                 
                                                                        
       FD  WORK-FILE                                                    
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  WORK-RECORD.                                                 
           COPY FNC022.                                                 

101708 FD  CCC-WORK-FILE                                                    
101708     LABEL RECORDS ARE STANDARD                                   
101708     RECORDING MODE IS F                                          
101708     BLOCK CONTAINS 0 RECORDS.                                    
101708 01  CCC-WORK-RECORD             PIC X(250).
101708
103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  
                                                                        
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
103002     05  WS-ZERO                 PIC S9(3)  COMP-3  VALUE +0.
103002     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.    
103002     05  PGM-SUB                 PIC S9(4) COMP VALUE +310.
103002     05  WS-RETURN-CODE          PIC S999  COMP-3 VALUE +0.
           05  DUMP                PIC X         VALUE ' '.             
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC XX        VALUE '00'.            
               88  EOF                           VALUE '10'.            
           05  DATE-SW             PIC X         VALUE ' '.             
               88  VALID-DATE                    VALUE 'V'.             
           05  WS-ZIP              PIC ZZZZ99999.                       
           05  WS-DESCRIPTION.                                          
               10  WS-DESC-TYPE      PIC XX.                            
111714         10  ws-check-ind      PIC X.                             
               10  WS-DESC-KON       PIC X(5).                          
               10  WS-DESC-DATE      PIC X(4).                          
               10  WS-DESC-DR-STATUS PIC X.                             
               10  WS-DESC-HORC      PIC X(3).                          
               10  WS-DESC-ASRC      PIC X(3).                          
               10  WS-DESC-PLAN      PIC X(6).                          
091808*               10  FILLER            PIC X(5).
091808         10  FILLER            PIC X(3).
091808         10  WS-DESC-PAYEE-ST  PIC X(2).


030702 01  SYSTEM-DATE.
030702     05  SYS-MO         PIC 9(2).
030702     05  SYS-DA         PIC 9(2).
030702     05  SYS-CCYY       PIC 9(4).

030702* FUNCTION-DATE COPYBOOK
030702                                      COPY ELCFUNDT.

030702* STATE EDIT TABLE
030702 COPY FNC018.

103002     COPY ELCDTECX.                                               
103002     COPY ELCDTEVR.                                               

       LINKAGE SECTION.
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH      PIC S9(4)   COMP.                       
           05  CYCLE-DATE       PIC X(8).                               
                                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION USING PARM.                                   
      *                                                                 
103002*************************************************************     
103002                                 COPY ELCDTERX.                   
103002*************************************************************     
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT
                                                                        
           PERFORM 1000-PROCESS      THRU 1000-EXIT
               UNTIL EOF
 
083002     CLOSE DAILY-DRAFTS
083002           WORK-FILE

122205     IF DTE-CLIENT = 'DCC'
111616        CLOSE CCC-WORK-FILE
122205     END-IF

030702     GOBACK.
                                                                        
      *                                                                 
       1000-PROCESS.                                                    
      *                                                                 
           READ DAILY-DRAFTS                                            
               AT END GO TO 1000-EXIT
           END-READ

           IF PR-POLICY-TYPE NOT = 'CR'                                 
               GO TO 1000-EXIT
           END-IF
                                                                        
           MOVE SPACES                 TO WORK-RECORD
                                                                        
103002     MOVE 'CLAIMS'               TO FX-SOURCE-CODE                          
103002     MOVE '60'                   TO FX-TRAN-TYPE                            
103002
122205     EVALUATE TRUE
022812        WHEN DTE-CLIENT = 'AHL'
022812           MOVE 'AHL CLAIMS'     TO FX-SYSTEM
022812           MOVE '02'             TO FX-DIVISION
022812        WHEN DTE-CLIENT = 'FNL'
022812           MOVE 'FNL CLAIMS'     TO FX-SYSTEM
022812           MOVE '02'             TO FX-DIVISION
122205        WHEN DTE-CLIENT = 'CID'
122205           MOVE 'CID CLAIMS'     TO FX-SYSTEM
122205           MOVE '02'             TO FX-DIVISION
111616*       WHEN (DTE-CLIENT = 'DCC')
111616*            and (cr-carrier = '3' or '4')
111616*          MOVE 'CSIDCCCLMS'     TO FX-SYSTEM
111616*          MOVE '11'             TO FX-DIVISION
101708        WHEN (DTE-CLIENT = 'DCC')
111616             and (cr-carrier = '3' or '4' or '5' or '6')
101708           MOVE 'CCCDCCCLMS'     TO FX-SYSTEM
101708           MOVE '50'             TO FX-DIVISION
040616        WHEN (DTE-CLIENT = 'DCC')
040616             and (cr-carrier = '7')
040616           MOVE 'CCCDCCCLMS'     TO FX-SYSTEM
040616           MOVE '5C'             TO FX-DIVISION
122205        WHEN DTE-CLIENT = 'DCC'  *> default car 1,2,9
122205           MOVE 'LPACCLAIMS'     TO FX-SYSTEM
122205           MOVE '11'             TO FX-DIVISION
122205        WHEN DTE-CLIENT = 'VPP'
122205           MOVE 'VPA CLAIMS'     TO FX-SYSTEM
122205           MOVE '5V'             TO FX-DIVISION
122205     END-EVALUATE


030702***** SUB-TYPE 2 = REFUND AND SUB-TYPE 1 = PAYMENT
030702***** DR-NOTE-CODE '01' IS A NEGATIVE AMOUNT PAID AND A PAYMENT
030702***** ORIGIN OF '3', EVALUATED IN PGM LGCIFCE
021601     IF (DR-DRAFT-STATUS = 'S')  OR  (DR-NOTE-CODE = 01)          
              MOVE '02' TO FX-SUB-TYPE                                  
              MULTIPLY DR-AMOUNT-PAID BY -1 GIVING FX-AMOUNT            
           ELSE                                                         
              MOVE '01' TO FX-SUB-TYPE                                  
              MOVE DR-AMOUNT-PAID TO FX-AMOUNT                          
           END-IF                                                       
                                                                        
022812     IF (DTE-CLIENT = 'CID' OR 'AHL')
021215        and (cr-carrier = '8')
112906        MOVE '04'                TO FX-SUB-TYPE
112906     END-IF

           MOVE PR-PLAN-NUMBER      TO FX-PLAN-CODE
           MOVE CYCLE-DATE          TO FX-POSTING-DATE                  
           MOVE PR-POLICY-NUMBER    TO FX-POLICY-NO                     
           MOVE PR-PO-CITY          TO FX-CITY                          
           MOVE 'Y'                 TO FX-LOC-CODE                      
           MOVE PR-COLL-AGT-STATE   TO FX-STATE                         
                                                                        
           MOVE PR-PO-ZIP TO WS-ZIP                                     
           IF WS-ZIP(1:4) = '0000'                                      
              MOVE WS-ZIP(5:5)      TO FX-ZIP-CODE                      
           ELSE                                                         
              MOVE WS-ZIP           TO FX-ZIP-CODE                      
           END-IF                                                       
                                                                        
022812     IF DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL'
103002        MOVE 'S'                 TO FX-FY-REN                        
103002     ELSE
103002        IF DTE-CLIENT = 'DCC'
103002           MOVE ' '              TO FX-FY-REN
103002        END-IF
103002     END-IF

           MOVE PR-SELL-AGT-NO      TO FX-AGENT-01                      
           MOVE ' '                 TO FX-DISTR                         
           MOVE ' '                 TO FX-SOURCE-ACCT                   
           MOVE ' '                 TO FX-REFERENCE                     
           MOVE DR-CLAIM-NUMBER     TO FX-CLAIM-NO                      

           IF DTE-CLIENT = 'DCC'
              MOVE DR-RECORD-KEY (3:2) TO FX-DRAFT-NO (1:2)
              MOVE '00'                TO FX-DRAFT-NO (3:2)
              MOVE DR-RECORD-KEY (5:6) TO FX-DRAFT-NO (5:6)
           ELSE
111714***=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=****
111714***                                                           ****
111714*** The draft number may be in the proper format from         ****
111714*** program LGCIFCE, but let's reformat it anyway.            ****
111714*** Got a problem with that?  hehehehehe                      ****
111714***                                                           ****
111714***=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=****
111714        if cr-last-paid > 20150430
111714           move zeros               to fx-draft-no
111714           move dr-record-key (3:7) to fx-draft-no (4:7)
111714        else
111714           MOVE DR-RECORD-KEY(1:10) TO FX-DRAFT-NO
111714        end-if
           END-IF

           MOVE DR-DRAFT-STATUS     TO FX-DRAFT-STATUS                  
                                                                        
           IF DR-TAX-TYPE = 'E' OR 'S'                                  
              MOVE DR-TAX-TYPE      TO FX-TAX-ID(1:1)                   
              MOVE DR-TAX-NO        TO FX-TAX-ID(2:9)                   
           ELSE                                                         
              MOVE SPACES           TO FX-TAX-ID                        
           END-IF
042817     move cr-exc-ben-type     to fx-distr
                                                                        
           MOVE SPACES                   TO WS-DESCRIPTION              
           MOVE PR-POLICY-TYPE           TO WS-DESC-TYPE                
022017     if ((dte-client = 'CID' or 'AHL')
022017        and (cr-last-paid < 20150501))
022017                  or
022017        ((dte-client = 'DCC')
022017        and (cr-last-paid < 20170523))
022017           move 'D'              to ws-check-ind
022017     else
013017        if cr-ach-payment = 'Y'
013017           move 'A'              to ws-check-ind
013017        else
022017           move 'C'              to ws-check-ind
013017        end-if
022017     end-if
           MOVE 'CLAIM'                  TO WS-DESC-KON                 
           MOVE CR-DATE-INCURRED(5:2)    TO WS-DESC-DATE(1:2)           
           MOVE CR-DATE-INCURRED(3:2)    TO WS-DESC-DATE(3:2)           
           MOVE DR-DRAFT-STATUS          TO WS-DESC-DR-STATUS           
           MOVE PR-PLAN-NUMBER           TO WS-DESC-PLAN
091808**** PUT PAYEE STATE IN LAST 2 POSITIONS OF DESC
091808     MOVE DR-PAYEE-STATE           TO WS-DESC-PAYEE-ST
           MOVE WS-DESCRIPTION           TO FX-DESCRIPTION

030702**** NO LONGER READING PLAN CODE TABLE WITHIN CID LOGIC SYSTEM
030702**** THE FOLLOWING CALL USED TO BRING BACK PLAN CODE TABLE DATA
030702*    CALL 'FNB160' USING WORK-RECORD

030702**** SEARCH PULLED FROM FNB160
030702     IF FX-STATE NOT= SPACES
030702         SEARCH ALL STATE-TABLE
030702             AT END DISPLAY 'INVALID STATE CODE: ' FX-STATE
030702             WHEN ST-STATE (ST-INDEX) = FX-STATE
030702             MOVE ST-ALT-STATE (ST-INDEX) TO FX-STATE
030702         END-SEARCH
030702     END-IF

030702     MOVE SYSTEM-DATE              TO FX-JOURNAL-DATE

030702**** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
030702**** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
030702     MOVE '*'                      TO WORK-RECORD(250:1)

PEMTST*    DISPLAY ' FX-DRAFT-NO = ' FX-DRAFT-NO (2:1)
PEMTST*    DISPLAY ' DR-RECORD-KEY 4 1 = ' DR-RECORD-KEY (4:1)
PEMTST*    DISPLAY ' CR-CARRIER = ' CR-CARRIER
111616     IF (DTE-CLIENT = 'DCC')
111616        AND (cr-carrier = '3' or '4' or '5' OR '6' or '7')
111616        WRITE CCC-WORK-RECORD FROM WORK-RECORD
111616     ELSE  *> default carrier 1, 2, 9
111616        WRITE WORK-RECORD
111616     END-IF

           .
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       0000-HOUSEKEEPING.                                               
      *                                                                 
           IF PARM-LENGTH = +0                                          
               DISPLAY 'PARAMETER DATE IS MISSING'                      
               ADD +1 TO FORCE-DUMP                                     
           ELSE                                                         
               CALL 'DATEEDIT' USING CYCLE-DATE,  DATE-SW               
               IF NOT VALID-DATE                                        
                   DISPLAY 'INVALID PARAMETER DATE: ' CYCLE-DATE        
                   ADD +1 TO FORCE-DUMP
               END-IF
           END-IF

030702     MOVE FUNCTION CURRENT-DATE
030702                                 TO FUNCTION-DATE
030702     MOVE WS-FN-MO               TO SYS-MO
030702     MOVE WS-FN-DA               TO SYS-DA
030702     MOVE WS-FN-CCYR             TO SYS-CCYY


           OPEN  INPUT DAILY-DRAFTS                                     
                OUTPUT WORK-FILE

122205     IF DTE-CLIENT = 'DCC'
111616        OPEN OUTPUT CCC-WORK-FILE
122205     END-IF

           .
       0000-EXIT.                                                       
            EXIT.                                                       
103002 ABEND-PGM SECTION.                                               00026280
103002     DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    00026290
103002 ABEND-EXIT.                                                      00026300
                                                                        
