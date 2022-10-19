      *****************************************************************
      *                                                               *
      * Copyright (c) 2018 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. SOCK09.
      *
      *AUTHOR.    Cowtown.
      *           Colleyville, TEXAS.
      
      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program acts as a Socket Server and expects a key    ***
      ***  and an Action indicator. This program works in            ***
      ***  conjunction with the verification of claim history on     ***
      ***  new pending issues.                                       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 013018  CR2017062000002  PEMA  NEW PROGRAM
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
      *-----------------------------------------------------------------
      
       environment division.
       data division.
       working-storage section.
      *
      * program buffers
      *
       77 ws-send-msg-size           pic s9(8) comp value 48000.
       77 ws-recv-msg-size           pic s9(8) comp value 19200.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(48000) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77 WS-COMP-ID                   PIC XXX VALUE 'CID'.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77 a1                           PIC S999 COMP-3 VALUE +0.
       77  c1                          pic s999 value +0 comp-3.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77  save-bin-date               pic xx value low-values.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERPNDB-SW                PIC X VALUE ' '.
           88  END-OF-ERPNDB                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-manager-id               pic x value 'N'.
       77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
       77  note-count                  pic s999 comp-3 value +0.
       77  ws-build-note-sw            pic x value ' '.
           88  finished-with-notes      value 'Y'.
       77  ws-ercnot-sw                pic x  value spaces.
           88  ercnot-startbr            value 'Y'.
       77  ws-notes-name               pic x(35) value spaces.
       77  n1                          pic s999 comp-3 value +0.
       77  n2                          pic s999 comp-3 value +0.
       01  ws-note-line                pic x(252) value spaces.
       01  ws-work-note                pic x(252) value spaces.
       01  ws-slice                    pic x(65)  value spaces.
       01  ws-note-line1               pic x(65)  value spaces.
       01  ws-note-line2               pic x(65)  value spaces.
       01  ws-note-line3               pic x(65)  value spaces.
       01  ws-note-line4               pic x(65)  value spaces.
       01  cert-note-records-holder.
           05  cert-note-record occurs 300.
               10  filler              pic x(48).
               10  cnr-rest            pic x(102).
      
      
      
      
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
      
       01  ws-cf-key.
           05  ws-cf-comp-id           pic xxx.
           05  ws-cf-rec-type          pic x.
           05  ws-cf-access-cd         pic x(4).
           05  ws-cf-seq-no            pic s9(4) comp value +0.

       01  WS-CERT-KEY.
           05  WS-CK-COMPANY-CD        PIC X.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUPING          PIC X(6).
           05  WS-CK-STATE             PIC XX.
           05  WS-CK-ACCOUNT           PIC X(10).
           05  WS-CK-CERT-EFF-DT       PIC XX.
           05  WS-CK-CERT-NO           PIC X(11).

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.
           05  WS-CS-CARRIER           PIC X.
           05  WS-CS-GROUPING          PIC X(6).
           05  WS-CS-STATE             PIC XX.
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-CERT-EFF-DT       PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.
      
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.
           05  WS-CM-CARRIER           PIC X.
           05  WS-CM-GROUPING          PIC X(6).
           05  WS-CM-STATE             PIC XX.
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-CERT-EFF-DT       PIC XX.
           05  WS-CM-CERT-NO           PIC X(11).
      
       01  WS-CZ-KEY.
           05  WS-CZ-COMPANY-CD        PIC X.
           05  WS-CZ-CARRIER           PIC X.
           05  WS-CZ-GROUP             PIC X(6).
           05  WS-CZ-STATE             PIC XX.
           05  WS-CZ-ACCOUNT           PIC X(10).
           05  WS-CZ-EFF-DT            PIC XX.
           05  WS-CZ-CERT-NO           PIC X(11).
           05  WS-CZ-REC-TYPE          PIC X.
           05  ws-cz-note-seq          pic s9(4) comp.
      
       01  WS-PB-KEY.
           05  WS-PB-COMPANY-CD        PIC X.
           05  WS-PB-BATCH-NO          PIC X(6).
           05  WS-PB-SEQ-NO            PIC S9(4) COMP.
           05  WS-PB-CHG-SEQ-NO        PIC S9(4) COMP.
      
       01  filler.
           05  reason-table.
               10  filler             pic x(28) value '1Life Coveraage'.
               10  filler             pic x(28) value '2Disability Cover
      -        'age'.
               10  filler             pic x(28) value '3Life or Disabili
      -        'ty Coverage'.
           05  filler redefines reason-table occurs 3.
               10  ws-reason-code      pic x.
               10  ws-reason-value     pic x(27).

       01  p1                          pic s999 comp-3 value +0.
       01  ws-return-stuff.
           05  ws-pend-status          pic x.
      
       01  WS-CID-NO                   PIC X(8).
       01  ws-new-status               pic x value spaces.
      
       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
      
                                        copy ELCCERT.
                                        COPY ERCPNDB.
                                        COPY ELCCRTT.
                                        copy ELCCNTL.
                                        copy ERCCNOT.
                                        COPY ELCDATE.
      
       01 soc-CLIENT-IN-DATA.
          05  soc-client-action        pic x.
          05  soc-client-comp-id       pic xxx.
          05  soc-CLIENT-CAR           PIC X.
          05  soc-CLIENT-ST            PIC XX.
          05  soc-CLIENT-ACT           PIC X(10).
          05  soc-CLIENT-EFF-DT        PIC X(10).
          05  soc-Client-cert-no       pic x(11).
          05  soc-client-proc-id       pic x(4).
          05  soc-cert-notes           pic x(252).

       01  filler                      pic x(100) value low-values.      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      
      **********  SOCK09   ************
         05 CLIENT-IN-DATA           pic x(36).
      **********  SOCK09   ************
      
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
      
       procedure division.
      *
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
           display 'SOCK09:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK09:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK09:socket name      =', lstn-name ' '
              lstn-subname
      
           exec cics
              asktime
           end-exec
      
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           perform 9700-DATE-LINK   thru 9700-exit
      
           IF DATE-CONVERSION-ERROR
              display ' error converting eibdate '
              MOVE LOW-VALUES          TO save-bin-date
           ELSE
              MOVE DC-BIN-DATE-1       TO save-bin-date
           end-if
      
           perform 0000-init-contact   thru 0000-exit
           perform 0010-receive        thru 0010-exit
           perform 0020-build-key      thru 0020-exit
           perform 0050-read-elcert    thru 0050-exit
      
           evaluate true
      ****  All no matches
              when soc-client-action = 'I'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  These are the co borrowers with no claims
      ***   All we do is add a cert note
              when soc-client-action = 'X'
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims researching
              when soc-client-action = 'W'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  No matches from Claims
              when soc-client-action = 'K'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims marked as Eligible
              when soc-client-action = 'A'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims marked as partially eligible
              when soc-client-action = 'B'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims marked as not eligible
              when soc-client-action = 'C'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims marked as not eligible - open paid claim
              when soc-client-action = 'D'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  Claims marked as part/not elig - rewrite
              when soc-client-action = 'E'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit

      ****  Claims marked as ND CERT - SPECIAL
              when soc-client-action = 'F'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit

      ****  Claims marked as SPECIAL OTHER
              when soc-client-action = 'G'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit

      ****  Undo previous action
              when soc-client-action = 'U'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
      ****  FUTURE
              when soc-client-action = 'P'
                 move soc-client-action
                                       to ws-new-status
                 perform 0200-update-status
                                       thru 0200-exit
                 perform 0250-process-cert-notes
                                       thru 0250-exit
              when other
                 display ' Invalid Action ' soc-client-action
           end-evaluate
      
           .
       0000-end.

           perform 1000-send-buffer    thru 1000-exit
           perform 1100-close-socket   thru 1100-exit
           goback
      
           .
       0000-init-contact.
      
           move spaces                 to ws-return-stuff
           if client-in-data (1:8) = 'SOCKET09'
              continue
           else
              display ' Unknown origin ' client-in-data
              go to 1100-close-socket
           end-if
      
           move 'SOCKET09READY'        to ws-send-buf
           move +25                    to ws-send-msg-size
      
           display 'SOCK09:sequence number  =', ws-seq-num.
           display 'SOCK09:send buffer      =', ws-send-buf(1:25).
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags
      
           if return-code <= zero
              display 'SOCK09:send error ' return-code
              go to 1000-socket-error
           end-if

           .
       0000-exit.
           exit.
      
       0010-receive.
      
           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.
      
           if return-code < zero
              display 'SOCK09:recv error ' return-code
              go to 1000-socket-error
           end-if
      
           if return-code = zero
              display 'SOCK09:client disconnected',
              go to 1000-socket-error
           end-if
      
           display 'SOCK09:Good recv  '
           display 'SOCK09:return code      = ', return-code
           display 'SOCK09:receive buffer   = ', ws-recv-buf(1:300)
      
           move +256                   to ws-send-msg-size
      
           move ws-recv-buf (1:300)    to soc-client-in-data
      
           move soc-client-comp-id     to ws-comp-id
           evaluate true
              when ws-comp-id = 'DCC'
                 move X'05'            to ws-comp-cd
              when ws-comp-id = 'AHL'
                 MOVE X'06'            TO ws-comp-cd
              when ws-comp-id = 'VPP'
                 move X'07'            to ws-comp-cd
062121        when ws-comp-id = 'FNL'
062121           move X'08'            to ws-comp-cd
              when other
                 move X'04'            to ws-comp-cd
           end-evaluate
      
           .
       0010-exit.
           exit.
       0020-build-key.

           move ws-comp-cd             to ws-ck-company-cd
           move soc-client-car         to ws-ck-carrier
           move '000000'               to ws-ck-grouping
           move soc-client-st          to ws-ck-state
           move soc-client-act         to ws-ck-account
           string
              soc-client-eff-dt(7:4)
              soc-client-eff-dt(1:2)
              soc-client-eff-dt(4:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if not no-conversion-error
              display ' error datecnvt eff dt ' soc-client-eff-dt ' '
              dc-error-code ' ' soc-client-cert-no
           else
              move dc-bin-date-1       to ws-ck-cert-eff-dt
           end-if
           move soc-client-cert-no     to ws-ck-cert-no

           .
       0020-exit.
           exit.

       0050-read-elcert.

           move ws-cert-key            to ws-cm-key
      
           exec cics read
              dataset   ('ELCERT')
              ridfld    (ws-cm-key)
              into      (certificate-master)
              resp      (ws-response)
           end-exec
      
           if not resp-normal
              display ' Certificate record not found ' ws-ck-cert-no
              move '8'                 to ws-return-stuff
              go to 0000-end
           end-if

      *    if soc-client-PriCo = 'C'
      *       string
      *          cm-jt-first-name delimited by ' '
      *          " " delimited by size
      *          cm-jt-initial delimited by ' '
      *          " "
      *          cm-jt-last-name delimited by size
      *          into ws-notes-name
      *       end-string
      *    else
      *       string
      *          cm-insured-first-name delimited by ' '
      *          " " delimited by size
      *          cm-insured-initial2 delimited by ' '
      *          " " 
      *          cm-insured-last-name delimited by size
      *          into ws-notes-name
      *       end-string
      *    end-if

           .
       0050-exit.
           exit.

       0100-get-status.
      
           move ws-cert-key            to ws-cs-key
           move 'C'                    to ws-cs-trlr-type
      
           EXEC CICS READ
                INTO    (certificate-trailers)
                DATASET ('ELCRTT')
                RIDFLD  (WS-CS-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC
      
           if resp-normal
              move cs-claim-verification-status
                                       to ws-pend-status
           else
              display ' no elcrtt ' ws-cs-cert-no
              move ' '                 to ws-pend-status
           end-if
      
           .
       0100-exit.
           exit.

       0200-update-status.

           move ws-cert-key            to ws-cs-key
           move 'C'                    to ws-cs-trlr-type
      
           if ws-new-status = 'U'
              move 'W'                 to ws-new-status
           end-if
      
           EXEC CICS READ UPDATE
              INTO    (certificate-trailers)
              DATASET ('ELCRTT')
              RIDFLD  (WS-CS-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC
      
           if resp-normal
              move ws-new-status       to cs-claim-verification-status
              EXEC CICS REWRITE
                 DATASET ('ELCRTT')
                 FROM    (certificate-trailers)
                 RESP    (WS-RESPONSE)
              END-EXEC
              if resp-normal
                 move '0'              to ws-return-stuff
              else
                 move '8'              to ws-return-stuff
              end-if
           else
              move 'CS'                to certificate-trailers
              move ws-cs-key           to cs-control-primary
              move ws-new-status       to cs-claim-verification-status
              EXEC CICS WRITE
                 DATASET ('ELCRTT')
                 FROM    (certificate-trailers)
                 RIDFLD  (WS-CS-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
              if resp-normal
                 move '0'              to ws-return-stuff
              else
                 move '8'              to ws-return-stuff
              end-if
           end-if

           .
       0200-exit.
           exit.
      
       0250-process-cert-notes.
      
           move spaces                 to cert-note-records-holder
                                          ws-build-note-sw
                                          ws-ercnot-sw
           move cm-control-primary    (1:33)
                                       to ws-cz-key
           move '1'                    to ws-cz-rec-type
           move +0                     to ws-cz-note-seq
                                          c1
      
           EXEC CICS STARTBR
                DATASET    ('ERCNOT')
                RIDFLD     (WS-CZ-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
              set ercnot-startbr to true
              perform until finished-with-notes
                 EXEC CICS READNEXT
                    DATASET    ('ERCNOT')
                    RIDFLD     (WS-CZ-KEY)
                    INTO       (cert-note-file)
                    resp       (ws-response)
                 end-exec
                 if (resp-normal)
                    and (cz-control-primary (1:33) =
                       cm-control-primary (1:33))
                    if cz-record-type = '1'
                       add +1 to c1
                       move cert-note-file
                                       to cert-note-record (c1)
                    end-if
                 else
                    set finished-with-notes to true
                 end-if
              end-perform
           end-if
      
           if ercnot-startbr
              exec cics endbr
                 dataset    ('ERCNOT')
              end-exec
           end-if
           move c1                     to note-count
      
           if c1 = +0
              perform 0251-add-note    thru 0251-exit
              perform 0254-update-cert thru 0254-exit
           else
              perform 0252-delete-cert-notes
                                       thru 0252-exit
              if resp-normal
                 perform 0251-add-note thru 0251-exit
                 if resp-normal
                    perform 0253-put-back-cert-notes
                                       thru 0253-exit
                    if resp-normal
                       move 0 to ws-return-stuff
                    else
                       move 8 to ws-return-stuff
                       display ' something wrong with put back '
                          pb-cert-no
                    end-if
                 else
                    display ' something went wrong with adding note '
                       pb-cert-no
                 end-if
              else
                 display ' something went wrong with generic delete '
                    pb-cert-no
              end-if
           end-if
      
           .
       0250-exit.
           exit.

       0260-get-slice.

             move +63                  to n1
             perform varying n1 from n1 by -1 until
                (ws-work-note (n1:1) = spaces)
                or (n1 < 1)
             end-perform
             if n1 > +1
                move ws-work-note (1:n1 - 1) to ws-slice
             end-if

           .
       0260-exit.
           exit.

       0251-add-note.

           move 'CZ'                   to cert-note-file
           move cm-control-primary    (1:33)
                                       to cz-control-primary
           move '1'                    to cz-record-type
           move +20                    to cz-note-sequence
           move save-bin-date          to cz-last-maint-dt
           move eibtime                to cz-last-maint-hhmmss
           move soc-client-proc-id     to cz-last-maint-user

           move soc-cert-notes         to ws-note-line

           if ws-note-line = spaces
              go to 0251-exit
      *       go to 0251-continue
           end-if

           move spaces                 to ws-note-line1
                                          ws-note-line2
                                          ws-note-line3
                                          ws-note-line4

           perform varying n1 from +252 by -1 until
              ws-note-line (n1:1) <> spaces and low-values
           end-perform
           move n1 to n2

           if n1 < +64
              move ws-note-line        to ws-note-line1
              go to 0251-continue
           end-if

           move ws-note-line           to ws-work-note
           perform 0260-get-slice      thru 0260-exit
           move ws-slice               to ws-note-line1
           compute n2 = n2 - n1
           move ws-note-line(n1 + 1:n2) to ws-note-line
           if n2 > 63
              move ws-note-line        to ws-work-note
              perform 0260-get-slice   thru 0260-exit
              move ws-slice            to ws-note-line2
           else
              move ws-note-line
                                       to ws-note-line2
              go to 0251-continue
           end-if

           compute n2 = n2 - n1
           move ws-note-line(n1 + 1:n2) to ws-note-line
           if n2 > 63
              move ws-note-line        to ws-work-note
              perform 0260-get-slice   thru 0260-exit
              move ws-slice            to ws-note-line3
           else
              move ws-note-line
                                       to ws-note-line3
              go to 0251-continue
           end-if
           
           compute n2 = n2 - n1
           move ws-note-line(n1 + 1:n2) to ws-note-line
           if n2 > 63
              move ws-note-line        to ws-work-note
              perform 0260-get-slice   thru 0260-exit
              move ws-slice            to ws-note-line4
           else
              move ws-note-line
                                       to ws-note-line4
              go to 0251-continue
           end-if

      *    perform varying n1 from +252 by -1 until
      *       ws-note-line (n1:1) <> spaces and low-values
      *    end-perform
      *    if n1 < +64
      *       move ws-note-line        to cz-note
      *       go to 0251-continue
      *    end-if
      *
      *   if n1 < +127
      *      move n1                   to n2
      *      move +63                  to n1
      *      perform varying n1 from n1 by -1 until
      *         (ws-note-line (n1:1) = spaces)
      *         or (n1 < 1)
      *      end-perform
      *      if n1 > +1
      *         move ws-note-line (1:n1)
      *                                to cz-note
      *         perform 0255-write-cert-note
      *                                thru 0255-exit
      *         move ws-note-line(n1 + 1:n2 - n1) to cz-note
      *         add +1                 to cz-note-sequence
      *      end-if
      *      go to 0251-continue
      *   end-if
      *
      *   if n1 < +190
      *      move ws-note-line (1:63)  to cz-note
      *      perform 0255-write-cert-note
      *                                thru 0255-exit
      *      add +1                    to cz-note-sequence
      *      move ws-note-line (64:63) to cz-note
      *      perform 0255-write-cert-note
      *                                thru 0255-exit
      *      add +1                    to cz-note-sequence
      *      move ws-note-line (127:63) to cz-note
      *      go to 0251-continue
      *   end-if
      *
      *   move ws-note-line (1:63)     to cz-note
      *   perform 0255-write-cert-note thru 0255-exit
      *   add +1                       to cz-note-sequence
      *   move ws-note-line (64:63)    to cz-note
      *   perform 0255-write-cert-note thru 0255-exit
      *   add +1                       to cz-note-sequence
      *   move ws-note-line (127:63)   to cz-note
      *   perform 0255-write-cert-note thru 0255-exit
      *   add +1                       to cz-note-sequence
      *   move ws-note-line (160:63)   to cz-note

      *     evaluate true
      *        when ws-new-status = 'I'
      *          move 'Claim History Verification script ran.  No matche
      *-       's found.'               to cz-note
      *
      *        when ws-new-status = 'W'
      *          move 'Referred to Claims to research if prior claim his
      *-       'tory impacts eligibility.'
      *                                 to cz-note
      *
      *        when ws-new-status = 'K'
      *           string
      *              'No prior claim history exists on '
      *              ws-notes-name delimited by size into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'A'
      *           string
      *              'Prior claim history does not impact '
      *              'eligibility for '
      *              ws-notes-name delimited by size into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'B'
      *           string
      *              'Endorsement required based on prior '
      *              'claim history for '
      *              ws-notes-name delimited by '   '
      *              ', who is NOT eligible for '
      *              soc-client-reason delimited by low-values
      *              into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'C'
      *           string
      *              ws-notes-name delimited by '   '
      *              ' is not eligible for coverage based on prior '
      *              'claim history' delimited by size
      *              into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'D'
      *           string
      *              ws-notes-name delimited by '   '
      *              ' is not eligible for '
      *              soc-client-reason delimited by low-values
      *              ' coverage based on open claim'
      *                 delimited by size
      *              into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'E'
      *           string
      *              ws-notes-name delimited by '   '
      *              ' is not eligible for '
      *              soc-client-reason delimited by low-values
      *              ' coverage based on prior claim history. Special '
      *              'handling required due to rewrite. '
      *              'Refer to Manager.'
      *                 delimited by size
      *              into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'F'
      *           string
      *              ws-notes-name delimited by '   '
      *              ' is not eligible for '
      *              soc-client-reason delimited by low-values
      *              ' coverage based on prior claim history. Special '
      *              'handling required due to ND contract. '
      *              'Refer to Manager.'
      *                 delimited by size
      *              into ws-note-line
      *           end-string
      *
      *        when ws-new-status = 'G'
      *           string
      *              ws-notes-name delimited by '   '
      *              ' is not eligible for '
      *              soc-client-reason delimited by low-values
      *              ' coverage based on prior claim history. Special '
      *              'handling required. '
      *              'Refer to Manager.'
      *                 delimited by size
      *              into ws-note-line
      *           end-string
      *
      *     end-evaluate

      *    evaluate true
      *       when ws-new-status = 'I'
      *          move 'Claim History Verification script ran.  No matche
      *       's found.'               to cz-note
      *       when ws-new-status = 'A'
      *          move 'No Prior claim history. Okay to continue to proce
      *        'ss'                    to cz-note
      *       when ws-new-status = 'B'
      *          string
      *             ws-notes-name delimited by '   '
      *             ' is not eligible for ' delimited by size
      *             soc-client-reason delimited by low-values
      *             ' based on prior claim history' delimited by '  '
      *             into ws-note-line
      *          end-string
      *       when ws-new-status = 'C'
      *          string
      *             ws-notes-name delimited by '   '
      *             ' is not eligible for coverage based on prior '
      *             'claim history' delimited by size
      *             into ws-note-line
      *          end-string
      *       when ws-new-status = 'D'
      *          string
      *             ws-notes-name delimited by '   '
      *             ' is not eligible for coverage based on open '
      *             'paid claim ' delimited by size
      *             into ws-note-line
      *          end-string
      *    end-evaluate

           .
       0251-continue.

           if ws-note-line1 not = spaces
              move ws-note-line1       to cz-note
              perform 0255-write-cert-note
                                       thru 0255-exit
           end-if

           if ws-note-line2 not = spaces
              move ws-note-line2       to cz-note
              add +1 to cz-note-sequence
              perform 0255-write-cert-note
                                       thru 0255-exit
           end-if

           if ws-note-line3 not = spaces
              move ws-note-line3       to cz-note
              add +1 to cz-note-sequence
              perform 0255-write-cert-note
                                       thru 0255-exit
           end-if

           if ws-note-line4 not = spaces
              move ws-note-line4       to cz-note
              add +1 to cz-note-sequence
              perform 0255-write-cert-note
                                       thru 0255-exit
           end-if

           .
       0251-exit.
           exit.
      
       0252-delete-cert-notes.

           move cm-control-primary    (1:33)
                                       to ws-cz-key
           move '1'                    to ws-cz-rec-type
           move +0                     to ws-cz-note-seq
           exec cics delete
              dataset    ('ERCNOT')
              keylength  (ws-cert-note-generic-key-len)
              ridfld     (ws-cz-key (1:34))
              generic
              resp       (ws-response)
           end-exec

           .
       0252-exit.
           exit.
      
       0253-put-back-cert-notes.
      
           perform varying c1 from +1 by +1 until
              c1 > note-count
              move cert-note-record (c1)
                                       to cert-note-file
              add +23                  to cz-note-sequence
              exec cics write
                 dataset ('ERCNOT')
                 FROM    (cert-note-file)
                 ridfld  (cz-control-primary)
                 resp    (ws-response)
              end-exec
              if not resp-normal
                 display ' error-ercnot-write subsequ ' ws-response ' '
                    cz-control-primary (2:33)
                 move +999             to c1
              end-if
           end-perform
      
          .
       0253-exit.
           exit.
      
       0254-update-cert.
      
           move cm-control-primary    (1:33)
                                       to ws-cm-key
      
           exec cics read
              update
              dataset   ('ELCERT')
              ridfld    (ws-cm-key)
              into      (certificate-master)
              resp      (ws-response)
           end-exec
      
           if resp-normal
      
              evaluate cm-note-sw
                 when ' '
                    move '1'           to cm-note-sw
                 when '2'
                    move '3'           to cm-note-sw
                 when '4'
                    move '5'           to cm-note-sw
                 when '6'
                    move '7'           to cm-note-sw
              end-evaluate
      
              exec cics rewrite
                 dataset   ('ELCERT')
                 from      (certificate-master)
                 resp      (ws-response)
              end-exec
      
              if resp-normal
                 go to 0254-exit
              end-if
           end-if
      
           move 8              to ws-return-stuff
      
           .
       0254-exit.
           exit.
      
       0255-write-cert-note.

           exec cics write
              dataset   ('ERCNOT')
              from      (cert-note-file)
              ridfld    (cz-control-primary)
              resp      (ws-response)
           end-exec
           if not resp-normal
              display ' error-ercnot-write 1 ' ws-response ' '
                 cz-control-primary (2:33)
              move 8                   to ws-return-stuff
              go to 0255-exit
           else
              move 0                   to ws-return-stuff
           end-if

           .
       0255-exit.
           exit.
      
       1000-send-buffer.
      
           move ws-return-stuff        to ws-send-buf
           display 'SOCK09:About to send      '
           display 'SOCK09:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size
      
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
      
           if return-code <= zero
              display 'SOCK09:send error ',
              go to 1000-socket-error
           end-if
           go to 1000-exit
      
           .
       1000-socket-error.
           if ws-seq-num <> 0
              display "SOCK09:did not complete"
           end-if
      
           .
       1000-exit.
           exit.
      
       1100-close-socket.
      
      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK09:done'
           exec cics return end-exec.
           goback
      
           .
       1100-exit.
           exit.
      
       9700-DATE-LINK.
      
           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.
      
       9700-EXIT.
            EXIT.
      
