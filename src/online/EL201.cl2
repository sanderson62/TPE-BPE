      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2014 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. EL201.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  Post check number and check written date to refund checks ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

011714******************************************************************
011714*                   C H A N G E   L O G
011714*
011714* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011714*-----------------------------------------------------------------
011714*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011714* EFFECTIVE    NUMBER
011714*-----------------------------------------------------------------
011714* 011714  CR2013053000001  PEMA  NEW PROGRAM
022014* 022014  IR2014022000001  PEMA  chg process when file is empty
082014* 082014  IR2014081400001  PEMA  bypass manual request checks
082515* 082014  IR2015082100001  PEMA  bypass more manual request checks
092215* 092215  IR2015092200001  PEMA  modify determination of manual checks
091615* 091615  CR2015082000001  PEMA  Add Endorsement check processing
020816* 020816  IR2016020500001  PEMA  Add env to unikix cmd for cid1p
042517* 042517  IR2017042500001  PEMA  Reduce number of print lines per page
060717* 060717  CR2017032900002  PEMA  CHANGE TEST DB
030921* 030921  CR2019012500003  PEMA  Connect to sdv-db01.cso.local
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
032422* 032422  IR2022031600001  PEMA  Bypass previously processed records.

       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CPS-WRITTEN-IN ASSIGN TO dynamic ws-written-in
          FILE STATUS IS WRITTEN-STATUS
                                  ORGANIZATION IS LINE SEQUENTIAL.

       SELECT CPS-WRITTEN-RPT ASSIGN TO dynamic ws-written-rpt
          FILE STATUS IS WRITRPT-STATUS
                                  ORGANIZATION IS LINE SEQUENTIAL.

       data division.
       FILE SECTION.

       FD  CPS-WRITTEN-IN
           BLOCK CONTAINS 0
           RECORDING MODE F.
       01  CPS-WRITTEN-IN-REC          pic x(200).

       FD  CPS-WRITTEN-RPT
           BLOCK CONTAINS 0
           RECORDING MODE F.
       01  CPS-WRITTEN-RPT-REC         pic x(132).

       working-storage section.

       77  ws-process-sw               pic x value ' '.
           88  process-cashed            value 'C'.
           88  process-written           value 'W'.
       77  ws-writ-cnt                 pic 9(5) value zeros.
       77  ws-tot-writ-amt             pic s9(9)v99 comp-3 value +0.
       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
082014 77  i1                          pic s999 comp-3 value +0.
082014 77  o1                          pic s999 comp-3 value +0.
       77  ws-bin-cashed-dt            pic xx  value low-values.
       77  ws-bin-check-dt             pic xx  value low-values.
       77  ws-page-cntr                pic 999  value zeros.
       77  ws-line-cntr                pic 999  value 070.
042517 77  ws-max-lines                pic 999  value 055.
       77  ws-erchek-sw                pic x    value spaces.
           88  erchek-found              value 'Y'.
       77  ws-table-sw                 pic x    value spaces.
           88  table-found               value 'Y'.
       77  ws-continue-sw              pic x  value 'Y'.
           88  ok-to-continue            value 'Y'.
           88  not-ok-to-continue        value 'N'.
       77  ws-in-recs                  pic 9(5) value zeros.
       77  ws-eof-sw                   pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
032422 77  ws-bypass-ind               pic x value spaces.
032422     88  bypass-rec               value 'Y'.

       01  ws-work-dynamic.
           05  ws-work-dir             pic x(27) value spaces.
           05  ws-work-env             pic x(8)  value spaces.
           05  ws-work-comp-id         pic xxx   value spaces.
           05  ws-work-file-in         pic x(16) value spaces.
           05  ws-work-rpt-out         pic x(13) value spaces.
           05  ws-work-job-name        pic x(8)  value spaces.

       01  ws-written-in               pic x(58).
       01  ws-written-rpt              pic x(58).

091615 01  P pointer.
091615 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
091615 01  var-ptr pointer.
091615 01  env-var-len                 pic 9(4)  binary.
091615 01  rc                          pic 9(9)  binary.
091615
091615 01  WS-KIXSYS.
091615     05  WS-KIX-FIL1             PIC X(10).
091615     05  WS-KIX-APPS             PIC X(10).
091615     05  WS-KIX-ENV              PIC X(10).
091615     05  WS-KIX-MYENV            PIC X(10).
091615     05  WS-KIX-SYS              PIC X(10).
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-key-stuff.
           05  ws-check-key            pic 9(7).
           05  ws-check-no             pic x(7).
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic 999.

       01  sqlcmd                      pic x(1024).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-checkno              pic s9(4) comp value +0.
       01  daily-check-request-rec.
           05  db-checkkey             pic x(7).
           05  db-compid               pic xxx.
           05  db-carrier              pic x.
           05  db-grouping             pic x(6).
           05  db-state                pic xx.
           05  db-account              pic x(10).
           05  db-effdate              pic x(24).
           05  db-certificate          pic x(10).
           05  db-cert-sfx             pic x.
           05  db-seq-no               pic x(7).
           05  db-type                 pic x(7).
           05  db-amount               pic x(10).
           05  db-amount-n redefines
               db-amount               pic 9(7).99.
           05  db-checkno              pic x(15).
           05  db-checkdate            pic x(10).
           05  db-checkstatus          pic 9(5).
           05  db-releasebatch         pic 9(5).
           05  db-releasedt            pic x(10).
           05  db-releaseby            pic x(4).
           05  db-payeename1           pic x(30).
           05  db-payeename2           pic x(30).
           05  db-payeeaddr1           pic x(30).
           05  db-payeeaddr2           pic x(30).
           05  db-payeecity            pic x(30).
           05  db-payeest              pic xx.
           05  db-payeezip             pic x(10).
           05  db-fincar               pic x.
           05  db-fingrp               pic x(6).
           05  db-finresp              pic x(10).
           05  db-finacct              pic x(10).
           05  db-preparer             pic x(4).
           05  db-app-status           pic x(9).
           05  dp-app-status-n redefines db-app-status
                                       pic 9(9).
           05  db-app-by               pic x(20).
           05  db-app-date             pic x(30).
           05  db-app-batch            pic x(10).
           05  db-return-to            pic x(30).
091615     05  db-check-sub-type       pic x.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  FILE-KEYS.
           12  ELCNTL-KEY.                                              
               16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.    
               16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.    
               16 ELCNTL-ACCESS.                                        
                   20 ELCNTL-STATE         PIC XX      VALUE SPACES.    
                   20  FILLER              PIC X       VALUE SPACES.    
                   20 ELCNTL-CARRIER       PIC X       VALUE SPACES.    
               16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.

       01  f.
           05  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
           05  written-status          pic xx.
           05  writrpt-status          pic xx.
           05  ws-comp-id              pic xxx.
           05  ws-comp-cd              pic x.

082014 01  ws-raw-record. 
082014     05  raw-check-no            pic x(20).
082014     05  raw-check-dt            pic x(20).
082014     05  raw-check-amt           pic x(15).
082014     05  raw-vendor-id           pic x(20).
082014     05  raw-user-defined        pic x(30).
082014     05  raw-voucher-ref         pic x(35).

       01  ws-written-rec.
           05  wsw-check-no            pic x(20).
           05  wsw-check-dt            pic x(20).
           05  wsw-check-amt           pic x(15).
           05  wsw-vendor-id           pic x(20).
           05  wsw-user-defined        pic x(30).
           05  wsw-voucher-ref         pic x(35).

       01  filler.
           05  wsw-check-key           pic x(7).
           05  wsw-check-key-num redefines wsw-check-key
                                       pic 9(7).

       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.

       01  ws-work-amt-alpha           pic x(10).
       01  ws-work-amt-num redefines ws-work-amt-alpha
                                       pic 9(8)v99.
       01  ws-qry-string               pic x(80) value spaces.
       01  ws-qrystr-len               pic s9(8) comp value +60.
       01  ws-seq-alpha                pic x(5).
       01  ws-seq-no-num redefines ws-seq-alpha
                                       pic 9(5).

                                       copy ERCCHEK.
                                       copy ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCCNTL.
       01  ws-heading1.
           05  ws-head1                pic x(120)  value '              
      -     '                                   DAILY CHECK RECONCILIATI
      -     'ON                                            '.
           05  ws-h1-rpt-id            pic x(6)  value 'EL201A'.

       01  ws-heading2.
           05  filler                  pic x(51) value spaces.
           05  WS-H2-CLIENT-NAME       PIC X(59) VALUE SPACES.
           05  WS-H2-DATE              PIC X(10).
           05  ws-h2-time              pic x(8).

       01  ws-heading3.
           05  filler                  pic x(53) value spaces.
           05  WS-H3-DATE              PIC X(67) VALUE SPACES.
           05  FILLER                  PIC X(5)  VALUE 'PAGE '.
           05  WS-H3-PAGE              PIC ZZ,ZZ9.                  

       01  ws-heading4.
           05  filler                  pic x(117)   value ' CAR  GROUP  
      -        'ST   ACCOUNT    EFF DTE       CERT NO    TYPE   CHECK NO
      -        '  CHECK CASHED DT   PREPARER   AMOUNT    MESSAGE'.

       01  ws-hd1-written.
           05  filler                  pic x(120)  value '              
      -     '                                   DAILY WRITTEN CHECKS REP
      -     'ORT                                           '.
       01  ws-hd4-written.
           05  filler                  pic x(117)   value ' CAR  GROUP  
      -        'ST   ACCOUNT    EFF DTE       CERT NO    TYPE   CHECK NO
      -        '  CHECK WRITTEN DT  PREPARER   AMOUNT    MESSAGE'.

       01  ws-detail1.
           05  filler                  pic xx value '  '.
           05  ws-d1-carr              pic x(4) value spaces.
           05  ws-d1-grp               pic x(7) value spaces.
           05  ws-d1-state             pic x(4) value spaces.
           05  ws-d1-account           pic x(12) value spaces.
           05  ws-d1-eff-dt            pic x(12) value spaces.
           05  ws-d1-cert-no           pic x(14) value spaces.
           05  ws-d1-chk-type          pic x(5) value spaces.
           05  ws-d1-chk-no            pic x(12) value spaces.
           05  f                       pic xx value spaces.
           05  ws-d1-writ-cash-dt      pic x(16) value spaces.
           05  ws-d1-preparer          pic x(8) value spaces.
           05  ws-d1-chk-amt-a         pic x(10) value spaces.
           05  ws-d1-chk-amt redefines
               ws-d1-chk-amt-a         pic $$$,$$9.99.
           05  filler                  pic xx value spaces.
           05  ws-d1-message           pic x(20) value spaces.

       01  ws-detail2.
           05  filler                  pic x(12) value '    PAYEE - '.
           05  ws-d2-addr-line         pic x(120) value spaces.

       01  ws-total1.
           05  filler                  pic x(5) value '     '.
           05  ws-t1-cnt               pic zzzz9  value zeros.
           05  ws-t1-chks              pic x(24)  value spaces.
           05  ws-t1-amount            pic $$,$$$,$$9.99 value zeros.

       01  w-doctoken                  pic x(16).
       01 output-data.
          05  filler                   pic x(6) value "TITLE=".
          05  output-title             pic x(26) value 'Check Posting'.
          05  filler                   pic x(8) value "&COMPID=".
          05  out-comp-id              pic xxx.
          05  filler                   pic x(5) value "&MSG=".
          05  output-msg               pic x(50).


       01  tran-data-line1             pic x(80) value spaces.
       01  tran-data-line2             pic x(80) value spaces.

      * 01  TRAN-DATA-LINE1             PIC X(80)    VALUE
pemtst**    'cd /apps/prod/cid1p/jcl'.
pemtst*      'cd /apps/test/ahltst/jcl'.
      * 01  TRAN-DATA-LINE2.
      *     05  filler                  pic x(10) value 'unikixjob '.
      *     05  tdl2-job                pic x(8) value 'cilg201b'.
pemtst**    05  filler                  pic x(9) value ' -k cid1p'.
pemtst*     05  filler                  pic x(10) value ' -k ahltst'.
      *     05  filler                  pic x(50) value spaces.

       LINKAGE SECTION.

       01  VAR                         PIC X(30).
       procedure division.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  Even though this program is using WEB Servies we don't    ***
      ***  have any form fields to browse through, we are getting    ***
      ***  the information we need through the variables that are    ***
      ***  included in the URL. These variables are retrieved by     ***
      ***  the           exec cics web extract                       ***
      ***                   querystring  (data-value)                ***
      ***                   querystrlen  (data-value)                ***
      ***                end-exec                                    ***
      ***  BTW, you must provide the length it isn't passed to you.  ***
      ***  Allow for extra on the length or you will get a length    ***
      ***  error instead of just truncation.                         ***
      ***                                                            ***
      ***  This program is expecting                                 ***
      *** http://slunikix:7001/cics/cwba/EL201?comp=CID&file=written***
pemtst*** http://slunikix:7003/cics/cwba/EL201?comp=CID&file=written***
091615*** http://logictest:6007/cics/cwba/EL201?comp=CID&file=written***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           display ' Entering program EL201 '

091615     set P to address of KIXSYS
091615     CALL "getenv" using by value P returning var-ptr
091615     if var-ptr = null then
091615        display ' kixsys not set '
091615     else
091615        set address of var to var-ptr
091615        move 0 to env-var-len
091615        inspect var tallying env-var-len
091615          for characters before X'00' 
091615        unstring var (1:env-var-len) delimited by '/'
091615           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
091615              WS-KIX-SYS
091615        end-unstring
091615     end-if

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE

           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-greg-date-1-alpha to ws-h3-date
              move dc-greg-date-1-edit to ws-h2-date
           else
              display ' error current dt invalid ' dc-error-code
           end-if

           string ws-fn-hours   ':'
                  ws-fn-minutes ':'
                  ws-fn-seconds
              delimited by size into ws-h2-time
           end-string

           exec cics web extract
              querystring  (ws-qry-string)
              querystrlen  (ws-qrystr-len)
           end-exec

           move ws-qry-string (6:3)    to ws-comp-id
                                          out-comp-id

           move 'Successful Run   '    to output-msg
           move ws-comp-id             to elcntl-key
           move '1'                    to elcntl-rec-type
           move +0                     to elcntl-seq
           exec cics read
              dataset  ('ELCNTL')
              into     (control-file)
              ridfld   (elcntl-key)
              resp     (ws-response)
           end-exec

           if resp-normal
              move cf-cl-mail-to-name  to ws-h2-client-name
              move cf-company-cd       to ws-comp-cd
           else
              move 'Company Rec not found ' to ws-h2-client-name
                                               output-msg
              display ' error elcntl read ' ws-response ' '
                 elcntl-key (1:8)
              go to 0000-return
           end-if

           evaluate true
              when ws-qry-string (15:7) = 'written' or 'WRITTEN'
                 set process-written      to true
                 perform 2000-process-written
                                       thru 2000-exit
              when other
                 move 'Invalid File ' to output-msg
           end-evaluate

           if connected-to-db
              EXEC SQL
                  commit work release
              END-EXEC
              if sqlcode not = 0
                 move ' Failed to Commit DB '
                                       to output-msg
                 display "Error: commit release "
                 display ' sql return code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
              end-if
           end-if

           .
       0000-return.

           perform 1030-send-form      thru 1030-exit

           exec cics
              return
           end-exec

           .
       1000-CONNECT-DB.

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              move 'Failed to connect to DB'
                                       to output-msg
              go to 0000-return
           else
              set connected-to-db to true
           end-if

           .
       1000-EXIT.
           EXIT.

       1020-read-erchek.

           move ws-comp-cd             to ch-company-cd
           move db-carrier             to ch-carrier
           move db-grouping            to ch-grouping
           move db-state               to ch-state
           move db-account             to ch-account
           move db-certificate         to ch-cert-prime
           move db-cert-sfx            to ch-cert-sfx

           string db-effdate (1:4)
                  db-effdate (6:2)
                  db-effdate (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ch-cert-eff-dt
           else
              display ' error cvtdte eff dt ' db-effdate ' '
                 dc-error-code
           end-if
           move zeros                  to ws-seq-alpha
           move +5                     to s2
           perform varying s1 from +5 by -1 until s1 < +1
              if db-seq-no (s1:1) numeric
                 move db-seq-no (s1:1) to ws-seq-alpha (s2:1)
                 subtract +1 from s2
              end-if
           end-perform

           move ws-seq-no-num          to ch-sequence-no

pemtst*    display ' dte   **' db-effdate '**'
pemtst*    display ' carrier ' ch-carrier
pemtst*    display ' group   ' ch-grouping
pemtst*    display ' state   ' ch-state
pemtst*    display ' acct    ' ch-account
pemtst*    display ' cert  **' ch-cert-no '**'
pemtst*    display ' seq     ' ch-sequence-no

           exec cics read
pemtst        update
              dataset    ('ERCHEK')
              into       (check-records)
              ridfld     (ch-control-primary)
              resp       (ws-response)
           end-exec

           .
       1020-exit.
           exit.

       1030-send-form.

           exec cics document create
              doctoken   (w-doctoken)
              template   ('WCHECKS')
              symbollist (output-data)
              listlength (length of output-data)
           end-exec

           exec cics web send
              doctoken(w-doctoken)
           end-exec

           .
       1030-exit.
           exit.

       1050-disconnect.

           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' ' to ws-connect-sw
           end-if

           .
       1050-exit.
           exit.

       2000-process-written.

           perform 2010-open-files     thru 2010-exit
           perform 2020-init           thru 2020-exit
           perform 2050-process-input  thru 2050-exit until
              end-of-input
           if ws-writ-cnt > zeros
              move ws-writ-cnt         to ws-t1-cnt
              move ' Written '         to ws-t1-chks
              move ws-tot-writ-amt     to ws-t1-amount
           else
              perform 4010-write-headings
                                       thru 4010-exit
              move spaces              to ws-total1
              string ' No checks for '
                 ws-work-comp-id 
                 ' today. ' delimited by size into ws-total1
              end-string
           end-if
           move spaces                 to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit
           move ws-total1              to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit
           perform 2500-close-files    thru 2500-exit

           perform 5000-submit-job     thru 5000-exit

           .
       2000-exit.
           exit.

       2010-open-files.

           if ws-kix-myenv = 'cid1p'
030921        move 'SDVDB01_ChkApprv'  to svr
030921        move 'appuser'           to usr
030921        move 'appuser@cso'       to pass

              move '/data/seqfiles/'   to ws-work-dir
              move 'cd /apps/prod/cid1p/jcl'
                                       to tran-data-line1
020816        move ws-kix-myenv        to ws-work-env
           else
030921        move 'HOVTSTDB01_ChkApprv' to svr
030921        move 'appuser'           to usr
030921        move 'appuser@cso'       to pass

              move ws-kix-myenv        to ws-work-env
              string
                 '/data/test/' delimited by size
                 ws-work-env   delimited by space
                 '/seqfiles/'  delimited by size
                    into ws-work-dir
              end-string
              string
                 'cd /apps/test/' delimited by size
                 ws-work-env      delimited by space
                 '/jcl'           delimited by size
                    into tran-data-line1
              end-string
           end-if

           move function upper-case(ws-comp-id)
                                       to ws-work-comp-id
           evaluate true
              when ws-work-comp-id = 'AHL'
                 move 'miscpymtsahl.csv'
                                       to ws-work-file-in
                 move 'ahlg201b'       to ws-work-job-name
062121        when ws-work-comp-id = 'FNL'
062121           move 'miscpymtsfnl.csv'
062121                                 to ws-work-file-in
062121           move 'fllg201b'       to ws-work-job-name
              when other
                 move 'miscpymts.csv'  to ws-work-file-in
                 move 'cilg201b'       to ws-work-job-name
           end-evaluate

           move spaces                 to ws-written-in
                                          ws-written-rpt

           string
              ws-work-dir delimited by space
              ws-work-file-in delimited by space
                 into ws-written-in
           end-string
           move 'miscpymts.rpt'        to ws-work-rpt-out
           string
              ws-work-dir delimited by space
              ws-work-comp-id delimited by size
              ws-work-rpt-out delimited by size
                 into ws-written-rpt
           end-string
           string
              'unikixjob '     delimited by size
              ws-work-job-name delimited by size
              ' -k '           delimited by size
              ws-work-env      delimited by space
                 into tran-data-line2
           end-string

      *     display ' svr    ' svr
      *     display ' user   ' usr
      *     display ' pw     ' pass
      *     display ' input  ' ws-written-in
      *     display ' output ' ws-written-rpt
      *     display '        '
      *     display ' line 1 ' tran-data-line1
      *     display ' line 2 ' tran-data-line2

           open input CPS-WRITTEN-IN
               output CPS-WRITTEN-RPT

           if written-status not = '00'
              display 'error written open ' written-status
              move ' Invalid or missing file '
                                       to output-msg
              go to 0000-return
           end-if

           .
       2010-exit.
           exit.

       2020-init.

           move spaces                 to ws-detail1
           move ws-hd1-written         to ws-head1
           move 'EL201B'               to ws-h1-rpt-id
           move ws-hd4-written         to ws-heading4
           move 'Posting of Written Checks '
                                       to output-title
      *    if ws-comp-id = 'AHL'
      *       move 'ahlg201b'          to tdl2-job
      *    else
      *       move 'cilg201b'          to tdl2-job
      *    end-if

           if not connected-to-db
              perform 1000-connect-db  thru 1000-exit
           end-if

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      **     I do 2 reads to bypass the header record                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           perform 2040-read-input     thru 2040-exit
           perform 2040-read-input     thru 2040-exit

           .
       2020-exit.
           exit.

       2040-read-input.

           read CPS-WRITTEN-IN at end
              set end-of-input   to true
           end-read

           if not end-of-input
              add 1 to ws-in-recs
           end-if

           .
       2040-exit.
           exit.

       2050-process-input.

082014     move spaces                 to ws-raw-record
082014                                    ws-written-rec
032422                                    ws-bypass-ind

           unstring cps-written-in-rec
              delimited by ',' into
082014           raw-check-no      
082014           raw-check-dt
082014           raw-check-amt
082014           raw-vendor-id     
082014           raw-user-defined  
082014           raw-voucher-ref
           end-unstring

pemtst*    display ' chk no    **' wsw-check-no '**'
pemtst*    display ' chk dt    **' wsw-check-dt '**'
pemtst*    display ' chk amt   **' wsw-check-amt '**'
pemtst*    display ' vend id   **' wsw-vendor-id '**'
pemtst*    display ' user def  **' wsw-user-defined '**'
pemtst*    display ' vouch ref **' wsw-voucher-ref '**'

082014***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
082014***                                                            ***
082014***    Every now and then I get bogus records with quotes      ***
082014***  so I am just going to remove them.  These are normally    ***
082014***  a result of a manual process that should not be included  ***
082014***  in this file.                                             ***
082014***                                                            ***
082014***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +20
082014        if raw-check-no (i1:1) <> '"'
082014           move raw-check-no (i1:1)
082014                                 to wsw-check-no (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +20
082014        if raw-check-dt (i1:1) <> '"'
082014           move raw-check-dt (i1:1)
082014                                 to wsw-check-dt (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +15
082014        if raw-check-amt (i1:1) <> '"'
082014           move raw-check-amt (i1:1)
082014                                 to wsw-check-amt (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +20
082014        if raw-vendor-id (i1:1) <> '"'
082014           move raw-vendor-id (i1:1)
082014                                 to wsw-vendor-id (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +30
082014        if raw-user-defined (i1:1) <> '"'
082014           move raw-user-defined (i1:1)
082014                                 to wsw-user-defined (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform
082014
082014     move +1                     to o1
082014     perform varying i1 from +1 by +1 until i1 > +35
082014        if raw-voucher-ref (i1:1) <> '"'
082014           move raw-voucher-ref (i1:1)
082014                                 to wsw-voucher-ref (o1:1)
082014           add +1 to o1
082014        end-if
082014     end-perform

           if (wsw-check-no = spaces)
              and (wsw-check-dt = spaces)
              and (wsw-check-amt = spaces)
              display ' empty record ' cps-written-in-rec
              go to 2050-read
           end-if
      
           string wsw-check-dt (1:4)
                  wsw-check-dt (6:2)
                  wsw-check-dt (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 9700-date-convert   thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-check-dt
           else
              display ' error cvtdte chek dt ' wsw-check-dt ' '
                 dc-error-code
           end-if

082014     perform varying s1 from +1 by +1 until
082014        (s1 > +28)
082515        or (wsw-voucher-ref (s1:6) = 'REF DI' or 'REFUND' or
082515          'CANCEL')
082014     end-perform
082014     if s1 < +29
082515        move 'Bypass- Manual Entry'    to ws-d1-message
082014        move ' '                 to ws-table-sw
082014        go to 2050-continue
082014     end-if

082515*    if wsw-user-defined (1:3) not numeric
082515*       move 'Bypass- Manual Entry'
082515*                                to ws-d1-message
082515*       move ' '                 to ws-table-sw
082515*       go to 2050-continue
082515*    end-if

092215     perform varying s1 from +1 by +1 until
092215        (s1 > +30)
092215        or (wsw-user-defined (s1:1) = ' ')
092215     end-perform
092215     if (s1 < +12)
092215        and (s1 > +8)
092215        continue
092215     else
092215        move 'Bypass- Manual Entry'
092215                                 to ws-d1-message
092215        move ' '                 to ws-table-sw
092215        go to 2050-continue
092215     end-if

           move spaces                 to wsw-check-key
           perform varying s1 from +30 by -1 until
              (s1 < +1)
              or (wsw-user-defined (s1:1) numeric)
           end-perform
           if s1 > +7
              move wsw-user-defined (s1 - 6:7)
                                       to wsw-check-key
           else
              move 'Invalid Input CK-KEY'    to ws-d1-message
              go to 2050-continue
           end-if

pemtst*    go to 2050-read

           move ' '                    to ws-erchek-sw
                                          ws-table-sw

           perform 2200-get-tbl-row    thru 2200-exit
           if sqlcode = 0
              set table-found          to true
              perform 1020-read-erchek thru 1020-exit
              if resp-normal
                 set erchek-found to true
                 perform 2310-upd-erchek
                                       thru 2310-exit
032422           perform 2320-upd-table
032422                                 thru 2320-exit
              else
                 display ' erchek not fnd ' ch-carrier ' ' ch-state ' '
                    ch-account ' ' db-effdate (1:10) ' ' db-certificate
                    ' ' ws-response
                 move ' chk rec not found ' to ws-d1-message
              end-if
           else
              move ' tbl row not found ' to ws-d1-message
           end-if

           .
       2050-continue.

           perform 2400-build-print-line thru 2400-exit
           .
       2050-read.
           perform 2040-read-input     thru 2040-exit

           .
       2050-exit.
           exit.

       2200-get-tbl-row.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  I'm only expecting one row so no cursor is declared       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           move ws-comp-id             to ws-compid
           move wsw-check-key-num      to ws-check-key
           move 1                      to ws-type

           exec sql
              SELECT
                 CheckKey,
                 Company,
                 CertCarrier,
                 CertGroup,
                 CertState,
                 CertAccount,
                 CertEffDate,
                 CertNumber,
                 CertNumberSuf,
                 CheckSeqNbr,
                 CheckType,
                 CheckAmount,
091615           Preparer,
091615           CheckSubType
              INTO
                 :db-checkkey,
                 :db-compid,
                 :db-carrier,
                 :db-grouping,
                 :db-state,
                 :db-account,
                 :db-effdate,
                 :db-certificate,
                 :db-cert-sfx,
                 :db-seq-no,
                 :db-type,
                 :db-amount,
091615           :db-preparer,
091615           :db-check-sub-type
              FROM
                 ChkApp_Check
              WHERE
                 (CheckKey    = :ws-check-key)
                 and (Company = :ws-compid)
           end-exec

           if sqlcode not = 0
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       2200-exit.
           exit.

       2310-upd-erchek.

032422     if (ch-check-written-dt <> low-values and spaces)
032422        and (ch-check-no <> low-values and spaces)
032422        exec cics unlock
032422           dataset  ('ERCHEK')
032422        end-exec
032422        move 'Bypass, Possible Dup'
032422                                 to ws-d1-message
032422        set bypass-rec to true
032422        go to 2310-exit
032422     end-if

           move ws-bin-check-dt        to ch-check-written-dt
           if wsw-check-no (1:1) = '"'
              move wsw-check-no (5:7)  to ch-check-no
           else
              move wsw-check-no (4:7)  to ch-check-no
           end-if

pemtst*    move zeros                  to ws-response
pemtst     exec cics rewrite
pemtst        dataset     ('ERCHEK')
pemtst        from        (check-records)
pemtst        resp        (ws-response)
pemtst     end-exec

           if resp-normal
              add 1 to ws-writ-cnt
              compute ws-tot-writ-amt =
                 ws-tot-writ-amt + ch-amount-paid
              move 'Successful Post '  to ws-d1-message
           else
              move '*** ERROR - NOT POSTED ***' TO ws-d1-message
           end-if

           .
       2310-exit.
           exit.

       2320-upd-table.

           string wsw-check-dt (6:2) '/'
                  wsw-check-dt (9:2) '/'
                  wsw-check-dt (1:4)
              delimited by size into db-checkdate

           move ch-check-no            to db-checkno
032422     if bypass-rec
032422        go to 2320-exit
032422     end-if

pemtst*    move zeros to sqlcode
pemtst     EXEC SQL
pemtst        UPDATE
                 ChkApp_Check
pemtst        SET
                 CheckNumber = :db-checkno,
pemtst           CheckDate = :db-checkdate
pemtst        WHERE
                 CheckKey = :db-checkkey
pemtst     END-EXEC

           if sqlcode not = 0
              move 'Table not updated ' to ws-d1-message
              display "Error: cannot update table   "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       2320-exit.
           exit.

       2400-build-print-line.

           if table-found
              move db-carrier          to ws-d1-carr
              move db-grouping         to ws-d1-grp
              move db-state            to ws-d1-state
              move db-account          to ws-d1-account
              string
                 db-effdate (6:2)   '/'
                 db-effdate (9:2)   '/'
                 db-effdate (1:4)
                 delimited by size into ws-d1-eff-dt
              end-string

              move db-certificate      to ws-d1-cert-no
              move db-cert-sfx         to ws-d1-cert-no (11:1)
091615        if db-check-sub-type = '2'
091615           move 'COR'            to ws-d1-chk-type
091615        else
091615           move 'REF'            to ws-d1-chk-type
091615        end-if
              move +10                 to s2
              move zeros               to ws-work-amt-alpha
              perform varying s1 from +10 by -1 until s1 < +1
                 if db-amount (s1:1) numeric
                    move db-amount (s1:1)
                                       to ws-work-amt-alpha (s2:1)
                    subtract +1 from s2
                 end-if
              end-perform
              move ws-work-amt-num     to ws-d1-chk-amt
              move db-preparer         to ws-d1-preparer
              move db-checkno          to ws-d1-chk-no
              move db-checkdate        to ws-d1-writ-cash-dt
           else
              move wsw-check-no        to ws-d1-chk-no
           end-if              

           perform 4000-write-report   thru 4000-exit
           move spaces                 to ws-detail1

           .
       2400-exit.
           exit.

       2500-close-files.

           close cps-written-in cps-written-rpt

           .
       2500-exit.
           exit.

       4000-write-report.

           if ws-line-cntr > ws-max-lines
              move zeros               to ws-line-cntr
              perform 4010-write-headings thru 4010-exit
           end-if

           move ws-detail1             to cps-written-rpt-rec

           perform 4020-write-a-line   thru 4020-exit

           .
       4000-exit.
           exit.
       4010-write-headings.

           add 1 to ws-page-cntr
           move ws-page-cntr           to ws-h3-page

           move ws-heading1            to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move ws-heading2            to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move ws-heading3            to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move spaces                 to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move spaces                 to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move ws-heading4            to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           move spaces                 to cps-written-rpt-rec
           perform 4020-write-a-line   thru 4020-exit

           .
       4010-exit.
           exit.

       4020-write-a-line.

           write cps-written-rpt-rec

           add 1 to ws-line-cntr

           .
       4020-exit.
           exit.

       5000-submit-job.

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

           .
       5000-exit.
           exit.

       9700-DATE-CONVERT.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
