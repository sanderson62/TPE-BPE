       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRATV7.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELtrlr           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS at-CONTROL-PRIMARY
                                   FILE STATUS IS ELtrlr-FILE-STATUS.
       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ELtrlr.

                                   COPY ELCTRLR.

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-eltrlr             VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FIX           PIC 9(9) VALUE ZEROS.
       
       01  WS-MISC.
           05  ws-work-seq             pic x.
           05  ws-num-seq redefines ws-work-seq pic 9.
           05  WS-SAVE-ELtrlr          PIC X(207) VALUE LOW-VALUES.
           05  ELtrlr-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
       

       PROCEDURE DIVISION.

       0000-MAIN.
       
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-eltrlr

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' TRLR RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' TRLR RECORDS WRITTEN ' TRLR-RECS-OUT
           DISPLAY ' TRLR RECORDS FIXED   ' TRLR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-TRLR.
       
           if payment-tr and 
              online-manual-pmt and
              AT-check-written-dt = low-values and
              at-void-dt = low-values and
              at-payment-approval-sw <> 'A'
              and at-recorded-dt < x'a29f'              
       	      display 'Claim # ' at-claim-no '  Cert # ' at-cert-no
       	         '   Approval sw added ' 
       	         
       	      ADD 1 TO TRLR-RECS-fix
       	      move 'A' to at-payment-approval-sw
              
              PERFORM 0300-REWRITE-TRLR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ eltrlr NEXT RECORD

           IF eltrlr-FILE-STATUS = '10' OR '23'
              SET END-OF-eltrlr        TO TRUE
           ELSE
              IF eltrlr-FILE-STATUS NOT = '00'
                 DISPLAY 'eltrlr READ NEXT ' eltrlr-FILE-STATUS
                 SET END-OF-eltrlr     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-eltrlr
              ADD 1 TO trlr-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-REWRITE-TRLR.

           REWRITE ACTIVITY-TRAILERS
           IF ELTRLR-FILE-STATUS = '00'
              ADD 1 TO TRLR-RECS-OUT
           ELSE
              DISPLAY ' ERROR - ELTRLR - REWRITE ' ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN I-O eltrlr

           IF eltrlr-FILE-STATUS = '00' 
              continue
           ELSE
              DISPLAY 'eltrlr open err  ' eltrlr-FILE-STATUS
              perform abend-pgm
           END-IF


           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE eltrlr

           .

       0500-EXIT.
           EXIT.
       0550-START-eltrlr.

           MOVE LOW-VALUES             TO at-CONTROL-PRIMARY
           MOVE X'04'                  TO at-COMPANY-CD

           START eltrlr KEY IS NOT < at-CONTROL-PRIMARY

           IF eltrlr-FILE-STATUS = '10' OR '23'
              SET END-OF-eltrlr        TO TRUE
           ELSE
              IF eltrlr-FILE-STATUS NOT = '00'
                 DISPLAY 'eltrlr START     ' eltrlr-FILE-STATUS
                 SET END-OF-eltrlr     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-eltrlr   THRU 0550-EXIT
           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

       ABEND-PGM.
          CALL 'ABORTME'.
          
       