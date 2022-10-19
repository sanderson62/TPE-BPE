       identification division.
       program-id. EL055.
052020******************************************************************
052020*                   C H A N G E   L O G
052020*
052020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052020*-----------------------------------------------------------------
052020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052020* EFFECTIVE    NUMBER
052020*-----------------------------------------------------------------
052020* 052020  CR2019111300001  PEMA  New program for 'K' transaction
072320* 072320  IR2020072100002  PEMA  Allow deletion of dup pend issue
072920* 072920  IR2020072900001  PEMA  Skip err if cert notfnd
052020******************************************************************
       environment division.

       data division.

       working-storage section.

       77  s1                          pic s999 comp-3 value +0.
       77  ns1                         pic s9(5) comp-3 value +0.
       77  nscntr                      pic s999 comp-3 value +0.

       77  ws-eof-sw                   pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-error-sw                 pic x  value spaces.
           88  error-found               value 'Y'.
       77  ws-string-len               pic s999 comp-3 value zeros.
       77  ws-startbr-ind              pic x value spaces.
       77  ws-NS-startbr-ind           pic x value spaces.
072320 77  ws-bypass-ind               pic x value spaces.
072320     88  bypass-copy-files         value 'Y'.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  ws-new-greg-eff-date        pic x(10).
       01  ws-old-greg-eff-date        pic x(10).
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

                                       copy ERCPNDB.
                                       copy ELCCERT.
                                       copy ELCCRTO.
                                       copy ELCCRTT.
                                       copy ERCARCH.
                                       copy NSCASEXTR.
                                       copy ERCCHEK.
                                       copy ERCCNOT.
                                       copy ERCNOTE.
                                       copy ERCMAIL.
                                       copy ERCENDR.
                                       copy ERCENDT replacing
                 ENDORSEMENT-RECORD by ENDT-RECORD
                 LEADING ==EN-== BY ==ET-==.
                 

       01  ws-dummy-area               pic x(1000).
       01  ws-misc.
           05  ws-old-pndb-key.
               10  opk-company-cd      pic x.
               10  opk-carrier         pic x.
               10  opk-group           pic x(6).
               10  opk-state           pic xx.
               10  opk-account         pic x(10).
               10  opk-eff-dt          pic xx.
               10  opk-cert-no         pic x(11).
               10  opk-seq-no          pic xx.
               10  opk-rec-type        pic x.
               
       01  ws-disp-code                pic s9(11).


       01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
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

       01  erpndb-ridfld               pic x(11).

       01  erpndb2-ridfld              pic x(36).

       01  elcert-ridfld               pic x(33).

       01  elcrto-ridfld.
           05  filler                  pic x(33).
           05  elcrto-rec-type         pic x.
           05  elcrto-seq-no           pic s9(4) comp.

       01  nsasextr-ridfld.
           05  filler                  pic x(5).
           05  nsasextr-seq-no         pic s9(4) comp.

       01  elcrtt-ridfld.
           05  filler                  pic x(33).
           05  elcrtt-rec-type         pic x.

       01  erarch-ridfld               pic x(5).

       01  erarch2-ridfld.  *>  pic x(37).
           05  erarch2-company-cd      pic x.
           05  erarch2-cert-no         pic x(11).
           05  erarch2-carrier         pic x.
           05  erarch2-group           pic x(6).
           05  erarch2-state           pic xx.
           05  erarch2-account         pic x(10).
           05  erarch2-eff-dt          pic xx.
           05  erarch2-arch-no         pic s9(8) comp.

       01  erchek-ridfld.
           05  filler                  pic x(33).
           05  erchek-seq-no           pic s9(4) comp.

       01  ercnot-ridfld.
           05  filler                  pic x(33).
           05  ercnot-rec-type         pic x.
           05  ercnot-seq-no           pic s9(4) comp.

       01  ernote-ridfld               pic x(33).

       01  ermail-ridfld               pic x(33).

       01  erendr-ridfld.
           05  filler                  pic x(33).
           05  erendr-seq-no           pic s9(4) comp.
           05  erendr-rec-type         pic x.

       01  erendt-ridfld.
           05  filler                  pic x(33).
           05  erendt-rec-type         pic x.
           05  erendt-seq-no           pic s9(4) comp.

       01  msg-error.
           05  filler                  pic x(08) value ' error- '.
           05  me-response             pic 9(5)  value zeros.
           05  filler                  pic xx value spaces.
           05  me-response2            pic 9(5)  value zeros.
           05  filler                  pic x(5)  value spaces.
           05  me-command              pic x(20) value spaces.

       01  WS-PASS-AREA.
           05  PA-ERPNDB-KEY           PIC X(11).
           05  pa-new-pndb-key.
               10  filler              pic x(20).
               10  pa-new-eff-dt       pic xx.
               10  pa-new-cert-no      pic x(11).
               10  filler              pic xxx.
           05  pa-return-area.
               10  ra-status-code      pic 9(4).
               10  ra-message          pic x(205).

                                       COPY ELCDATE.

       LINKAGE SECTION.
       
       01  DFHCOMMAREA                 PIC X(256).

       01  var  pic x(30).

       procedure division using dfhcommarea.

           display ' entering program EL055'

           perform 0010-init           thru 0010-exit

           perform 0100-check-validity thru 0100-exit
           perform 0200-convert-key    thru 0200-exit

           perform 0050-bld-return-area thru 0050-exit

           .
       0000-return.

           move ws-pass-area           to dfhcommarea

           exec cics return
           end-exec

           GOBACK

           .
       0010-init.

           move dfhcommarea            to ws-pass-AREA

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

           .
       0010-exit.
           exit.

       0050-bld-return-area.

           move zeros                  to ra-status-code
           move 'SUCCESS'              to ra-message

           .           
       0050-exit.
           exit.

       0100-check-validity.
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure Key to change exists on ERPNDB              ****
      ****                                                          ****
      **** ======================================================== ****
           move PA-ERPNDB-KEY          to erpndb-ridfld
           exec cics read
              dataset    ('ERPNDB')
              ridfld     (erpndb-ridfld)
              into       (pending-business)
              resp       (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb read error '
                                       to ra-message
              display 'error-erpndb-read ' ws-response
              go to 0000-return
           end-if

           move pb-control-by-account  to ws-old-pndb-key


           move pb-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              string
                 dc-cymd-cen
                 dc-cymd-year   '-'
                 dc-cymd-month  '-'
                 dc-cymd-day delimited by size
                    into ws-old-greg-eff-date
              end-string
           else
              display ' Bad eff date convert ' pb-cert-no
              go to 0000-return
           end-if

      ***  Might as well convert the new effective dt while we're at it

           move pa-new-eff-dt          to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              string
                 dc-cymd-cen
                 dc-cymd-year   '-'
                 dc-cymd-month  '-'
                 dc-cymd-day delimited by size
                    into ws-new-greg-eff-date
              end-string
           else
              display ' Bad new eff date convert ' pb-cert-no
              go to 0000-return
           end-if
           
      **** ======================================================== ****

      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure Key to change exists on ELCERT              ****
      ****                                                          ****
      **** ======================================================== ****
072320     move spaces                 to ws-bypass-ind

           move ws-old-pndb-key(1:33)  to elcert-ridfld
           exec cics read
              dataset    ('ELCERT')
              ridfld     (elcert-ridfld)
              into       (certificate-master)
              resp       (ws-response)
           end-exec

072920     if ws-response = 13
072920        set bypass-copy-files to true
072920        go to 0100-check-erpndb2
072920     end-if

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert read error '
                                       to ra-message
              display 'error-elcert-read ' ws-response
              go to 0000-return
           end-if

072320     if cert-added-batch
072320        set bypass-copy-files to true
072320     end-if

           if (cert-and-claim-online)
              or (cert-was-created-for-claim)
072320        or (cm-claim-attached-count > zeros)
              move +99                 to ra-status-code
              move 'Claim Exists '     to ra-message
              go to 0000-return
           end-if

      **** ======================================================== ****
072920     .
072920 0100-check-erpndb2.
      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure new key does NOT exist on ERPNDB2           ****
      ****                                                          ****
      **** ======================================================== ****
           move PA-new-pndb-key        to erpndb2-ridfld
           exec cics read
              dataset    ('ERPNDB2')
              ridfld     (erpndb2-ridfld)
              into       (ws-dummy-area)
              resp       (ws-response)
           end-exec

           if (not RESP-NOTFND)
              and (not resp-endfile)
              move +88                 to ra-status-code
              move ' pndb already exists '
                                       to ra-message
              display 'error-erpndb2-read ' ws-response
              go to 0000-return
           end-if

      **** ======================================================== ****

      **** ======================================================== ****
      ****                                                          ****
      ****    Make sure new key does NOT exist on ELCERT            ****
      ****                                                          ****
      **** ======================================================== ****

           move PA-new-pndb-key(1:33)  to elcert-ridfld
           exec cics read
              dataset    ('ELCERT')
              ridfld     (elcert-ridfld)
              into       (ws-dummy-area)
              resp       (ws-response)
           end-exec

           if (not RESP-NOTFND)
              and (not resp-endfile)
              display ' setting status to 88 '
              move +88                 to ra-status-code
              move ' elcert already exist '
                                       to ra-message
              display 'error-elcert-read-exists ' ws-response
              go to 0000-return
           end-if

      **** ======================================================== ****

      **** ======================================================== ****
      ****                                                          ****
      ****    Right here, 1)rewrite erpndb with new key, 2)delete   ****
      ****  elcert.                                                 ****
      ****                                                          ****
      **** ======================================================== ****

      ** 1)  Rewrite erpndb with new key
           move PA-ERPNDB-KEY          to erpndb-ridfld
           exec cics read update
              dataset    ('ERPNDB')
              ridfld     (erpndb-ridfld)
              into       (pending-business)
              resp       (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb read update error '
                                       to ra-message
              display 'error-erpndb-read update ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to pb-cert-eff-dt
           move pa-new-cert-no         to pb-cert-no

           exec cics rewrite
              dataset  ('ERPNDB')
              from     (pending-business)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erpndb rewrite error '
                                       to ra-message
              display 'error-erpndb-rewrite ' ws-response
              go to 0000-return
           end-if

           .
       0100-check-for-claim.

072320     if bypass-copy-files
072320        go to 0000-return
072320     end-if
              
           move ws-old-pndb-key(1:33)  to elcert-ridfld
           exec cics read update
              dataset    ('ELCERT')
              ridfld     (elcert-ridfld)
              into       (certificate-master)
              resp       (ws-response)
           end-exec

           if (cert-added-batch)
              or (cert-and-claim-online)
              or (cert-was-created-for-claim)
              continue
           else
              go to 0100-delete-elcert
           end-if

           if (no-claim-attached)
              or (cert-added-batch)
072320        exec cics unlock
072320           dataset   ('ELCERT')
072320           resp      (ws-response)
072320        end-exec
           else
              move spaces              to cm-credit-interface-sw-1
              move '2'                 to cm-claim-interface-sw
              exec cics rewrite
                 dataset   ('ELCERT')
                 from      (certificate-master)
                 resp      (ws-response)
              end-exec
           end-if

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert rewrite error clm '
                                       to ra-message
              display 'error-elcert-rewrite ' ws-response
              go to 0000-return
           end-if

           go to 0100-write-new-elcert

           .
       0100-delete-elcert.

      ** 2)  Delete ELCERT (old key)
           move ws-old-pndb-key(1:33)  to elcert-ridfld
           exec cics delete
              dataset  ('ELCERT')
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcert delete error '
                                       to ra-message
              display 'error-elcert-delete  ' ws-response
              go to 0000-return
           end-if

           .
       0100-write-new-elcert.
      **  This is commented out because I want EL6311 to write 
      **    the new ELCERT record.

      ** 3)  Write new ELCERT record
      **  Not now. Let el6311 write the new one??
      **   move pa-new-eff-dt          to cm-cert-eff-dt
      **   move pa-new-cert-no         to cm-cert-no
      **
      **   exec cics write
      **      dataset  ('ELCERT')
      **      from     (certificate-master)
      **      ridfld   (cm-control-primary)
      **      resp     (ws-response)
      **   end-exec
      **
      **   if not resp-normal
      **      move ws-response         to ra-status-code
      **      move ' elcert write  error '
      **                               to ra-message
      **      display 'error-elcert-write   ' ws-response
      **      go to 0000-return
      **   end-if

      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ELCRTO records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to elcrto-ridfld(1:33)
           move opk-rec-type           to elcrto-rec-type
           move +0                     to elcrto-seq-no

           exec cics startbr
              dataset    ('ELCRTO')
              ridfld     (elcrto-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-elcrto
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrto error startbr '
                                       to ra-message
                 display 'error-elcrto-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-elcrto-loop.

           exec cics readnext
              dataset    ('ELCRTO')
              ridfld     (elcrto-ridfld)
              into       (original-certificate)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (elcrto-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-elcrto
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrto error readnext '
                                       to ra-message
                 display 'error-elcrto-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           if opk-rec-type <> oc-record-type
              go to 0100-elcrto-loop
           end-if

      ***  delete and add with new key right here!!!

           exec cics delete
              dataset  ('ELCRTO')
              ridfld   (elcrto-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrto delete error '
                                       to ra-message
              display 'error-elcrto-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to oc-cert-eff-dt
           move pa-new-cert-no         to oc-cert-no

           exec cics write
              dataset  ('ELCRTO')
              from     (original-certificate)
              ridfld   (oc-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrto write  error '
                                       to ra-message
              display 'error-elcrto-write   ' ws-response
              go to 0000-return
           end-if
           
           go to 0100-elcrto-loop

           .
       0100-end-elcrto.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ELCRTO')
              end-exec
           end-if

            .
       0100-elcrtt.

      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ELCRTT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to elcrtt-ridfld(1:33)
           move ' '                    to elcrtt-rec-type

           exec cics startbr
              dataset    ('ELCRTT')
              ridfld     (elcrtt-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-elcrtt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrtt error startbr '
                                       to ra-message
                 display 'error-elcrtt-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-elcrtt-loop.

           exec cics readnext
              dataset    ('ELCRTT')
              ridfld     (elcrtt-ridfld)
              into       (certificate-trailers)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (elcrtt-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-elcrtt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' elcrtt error readnext '
                                       to ra-message
                 display 'error-elcrtt-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

      ***  delete and add with new key right here!!!
           exec cics delete
              dataset  ('ELCRTT')
              ridfld   (elcrtt-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrtt delete error '
                                       to ra-message
              display 'error-elcrtt-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to cs-cert-eff-dt
           move pa-new-cert-no         to cs-cert-no

           exec cics write
              dataset  ('ELCRTT')
              from     (certificate-trailers)
              ridfld   (cs-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' elcrtt write  error '
                                       to ra-message
              display 'error-elcrtt-write   ' ws-response
              go to 0000-return
           end-if

           go to 0100-elcrtt-loop

           .
       0100-end-elcrtt.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ELCRTT')
              end-exec
           end-if

           .
       0100-erarch.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERARCH records                              ****
      ****                                                          ****
      **** ======================================================== ****

           move ' '                    to ws-startbr-ind
           move opk-company-cd         to erarch2-company-cd
           move opk-cert-no            to erarch2-cert-no
           move ws-old-pndb-key(1:21)  to erarch2-ridfld(13:21)
           move +0                     to erarch2-arch-no

           exec cics startbr
              dataset    ('ERARCH2')
              ridfld     (erarch2-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-erarch
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erarch2 error startbr '
                                       to ra-message
                 display 'error-erarch2-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-erarch-loop.

           exec cics readnext
              dataset    ('ERARCH2')
              ridfld     (erarch2-ridfld)
              into       (letter-archive)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (erarch2-company-cd <> opk-company-cd)
              or (erarch2-cert-no <> opk-cert-no)
              go to 0100-end-erarch
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erarch2 error readnext '
                                       to ra-message
                 display 'error-erarch2-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           if (opk-carrier <> la-carrier-a2)
              or (opk-group <> la-grouping-a2)
              or (opk-state <> la-state-a2)
              or (opk-account <> la-account-a2)
              or (opk-eff-dt <> la-effect-date-a2)
              go to 0100-erarch-loop
           end-if

      ***  Do rewrites to base cluster right here!!!

           move la-control-primary     to erarch-ridfld
                                          nsasextr-ridfld
           exec cics read update
              dataset  ('ERARCH')
              into     (letter-archive)
              ridfld   (erarch-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erarch read update error '
                                       to ra-message
              display 'error-erarch-read update ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to la-effect-date-a2
           move pa-new-cert-no         to la-cert-no-a2

           exec cics rewrite
              dataset  ('ERARCH')
              from     (letter-archive)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erarch write  error '
                                       to ra-message
              display 'error-erarch-write   ' ws-response
              go to 0000-return
           end-if

           perform 0500-process-nsasextr thru 0500-exit

           go to 0100-erarch-loop

           .
       0100-end-erarch.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ERARCH2')
              end-exec
           end-if

           .
       0100-erchek.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERCHEK records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erchek-ridfld(1:33)
           move +0                     to erchek-seq-no

           exec cics startbr
              dataset    ('ERCHEK')
              ridfld     (erchek-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-erchek
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erchek error startbr '
                                       to ra-message
                 display 'error-erchek-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-erchek-loop.

           exec cics readnext
              dataset    ('ERCHEK')
              ridfld     (erchek-ridfld)
              into       (check-records)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (erchek-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erchek
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erchek error readnext '
                                       to ra-message
                 display 'error-erchek-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

      ***  delete and add with new key right here!!!

           exec cics delete
              dataset  ('ERCHEK')
              ridfld   (erchek-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erchek delete error '
                                       to ra-message
              display 'error-erchek-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to ch-cert-eff-dt
           move pa-new-cert-no         to ch-cert-no

           exec cics write
              dataset  ('ERCHEK')
              from     (check-records)
              ridfld   (ch-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erchek write  error '
                                       to ra-message
              display 'error-erchek-write   ' ws-response
              go to 0000-return
           end-if
           
           go to 0100-erchek-loop

           .
       0100-end-erchek.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ERCHEK')
              end-exec
           end-if

           .
       0100-ercnot.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERCNOT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to ercnot-ridfld(1:33)
           move opk-rec-type           to ercnot-rec-type
           move +0                     to ercnot-seq-no

           exec cics startbr
              dataset    ('ERCNOT')
              ridfld     (ercnot-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-ercnot
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ercnot error startbr '
                                       to ra-message
                 display 'error-ercnot-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-ercnot-loop.

           exec cics readnext
              dataset    ('ERCNOT')
              ridfld     (ercnot-ridfld)
              into       (cert-note-file)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (ercnot-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-ercnot
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ercnot error readnext '
                                       to ra-message
                 display 'error-ercnot-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           if cz-record-type <> '1'  *>  cert note
              go to 0100-ercnot-loop
           end-if

      ***  delete and add with new key right here!!!

           exec cics delete
              dataset  ('ERCNOT')
              ridfld   (ercnot-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ercnot delete error '
                                       to ra-message
              display 'error-ercnot-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to cz-cert-eff-dt
           move pa-new-cert-no         to cz-cert-no

           exec cics write
              dataset  ('ERCNOT')
              from     (cert-note-file)
              ridfld   (cz-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ercnot write  error '
                                       to ra-message
              display 'error-ercnot-write   ' ws-response
              go to 0000-return
           end-if
           
           go to 0100-ercnot-loop

           .
       0100-end-ercnot.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ERCNOT')
              end-exec
           end-if

            .
       0100-ernote.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERNOTE record.                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ws-old-pndb-key(1:33)  to ernote-ridfld

           exec cics read
              dataset    ('ERNOTE')
              ridfld     (ernote-ridfld)
              into       (certificate-note)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              go to 0100-end-ernote
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ernote error readnext '
                                       to ra-message
                 display 'error-ernote-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

      ***  delete and add with new key right here!!!


      ** 1)  Delete ERNOTE (old key)

           exec cics delete
              dataset  ('ERNOTE')
              ridfld   (ernote-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ernote delete error '
                                       to ra-message
              display 'error-ernote-delete  ' ws-response
              go to 0000-return
           end-if

      ** 2)  Write new ERNOTE record
           move pa-new-eff-dt          to cn-cert-eff-dt
           move pa-new-cert-no         to cn-cert-no

           exec cics write
              dataset  ('ERNOTE')
              from     (certificate-note)
              ridfld   (cn-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ernote write  error '
                                       to ra-message
              display 'error-ernote-write   ' ws-response
              go to 0000-return
           end-if

           .
       0100-end-ernote.

           .
       0100-ermail.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERMAIL record.                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ws-old-pndb-key(1:33)  to ermail-ridfld

           exec cics read
              dataset    ('ERMAIL')
              ridfld     (ermail-ridfld)
              into       (mailing-data)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              go to 0100-end-ermail
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' ermail error readnext '
                                       to ra-message
                 display 'error-ermail-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

      ***  delete and add with new key right here!!!

      ** 1)  Delete ERMAIL (old key)

           exec cics delete
              dataset  ('ERMAIL')
              ridfld   (ermail-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ermail delete error '
                                       to ra-message
              display 'error-ermail-delete  ' ws-response
              go to 0000-return
           end-if

      ** 2)  Write new ERMAIL record
           move pa-new-eff-dt          to ma-cert-eff-dt
           move pa-new-cert-no         to ma-cert-no

           exec cics write
              dataset  ('ERMAIL')
              from     (mailing-data)
              ridfld   (ma-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' ermail write  error '
                                       to ra-message
              display 'error-ermail-write   ' ws-response
              go to 0000-return
           end-if

           .
       0100-end-ermail.

           .
       0100-erendr.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERENDR records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erendr-ridfld(1:33)
           move +0                     to erendr-seq-no
           move ' '                    to erendr-rec-type

           exec cics startbr
              dataset    ('ERENDR')
              ridfld     (erendr-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-erendr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendr error startbr '
                                       to ra-message
                 display 'error-erendr-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-erendr-loop.

           exec cics readnext
              dataset    ('ERENDR')
              ridfld     (erendr-ridfld)
              into       (endorsement-record)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (erendr-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erendr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendr error readnext '
                                       to ra-message
                 display 'error-erendr-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           if en-rec-type <> '1'  *>  Issue record
              go to 0100-erendr-loop
           end-if

      ***  delete and add with new key right here!!!

           exec cics delete
              dataset  ('ERENDR')
              ridfld   (erendr-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendr delete error '
                                       to ra-message
              display 'error-erendr-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to en-cert-eff-dt
           move pa-new-cert-no         to en-cert-no

           exec cics write
              dataset  ('ERENDR')
              from     (ENDORSEMENT-RECORD)
              ridfld   (en-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendr write  error '
                                       to ra-message
              display 'error-erendr-write   ' ws-response
              go to 0000-return
           end-if
           
           go to 0100-erendr-loop

           .
       0100-end-erendr.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ERENDR')
              end-exec
           end-if

           .
       0100-erendt.
      **** ======================================================== ****
      ****                                                          ****
      ****    Check for ERENDT records                              ****
      ****                                                          ****
      **** ======================================================== ****
           move ' '                    to ws-startbr-ind
           move ws-old-pndb-key(1:33)  to erendt-ridfld(1:33)
           move ' '                    to erendt-rec-type
           move +0                     to erendt-seq-no

           exec cics startbr
              dataset    ('ERENDT')
              ridfld     (erendt-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0100-end-erendt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendt error startbr '
                                       to ra-message
                 display 'error-erendt-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-startbr-ind

           .
       0100-erendt-loop.

           exec cics readnext
              dataset    ('ERENDT')
              ridfld     (erendt-ridfld)
              into       (endt-record)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (erendt-ridfld(1:33) <> ws-old-pndb-key(1:33))
              go to 0100-end-erendt
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' erendt error readnext '
                                       to ra-message
                 display 'error-erendt-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           if et-rec-type <> 'I'  *>  Issue record
              go to 0100-erendt-loop
           end-if

      ***  delete and add with new key right here!!!

           exec cics delete
              dataset  ('ERENDT')
              ridfld   (erendt-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendt delete error '
                                       to ra-message
              display 'error-erendt-delete  ' ws-response
              go to 0000-return
           end-if

           move pa-new-eff-dt          to et-cert-eff-dt
           move pa-new-cert-no         to et-cert-no

           exec cics write
              dataset  ('ERENDT')
              from     (ENDT-RECORD)
              ridfld   (et-control-primary)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' erendt write  error '
                                       to ra-message
              display 'error-erendt-write   ' ws-response
              go to 0000-return
           end-if

           go to 0100-erendt-loop

           .
       0100-end-erendt.

           if ws-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('ERENDT')
              end-exec
           end-if

           .
       0100-exit.
           exit.

       0200-convert-key.


           .
       0200-exit.
           exit.

       0500-process-nsasextr.

      **** ======================================================== ****
      ****                                                          ****
      ****    Check for NSASEXTR records                            ****
      ****                                                          ****
      **** ======================================================== ****

           move ' '                    to ws-NS-startbr-ind
           move +0                     to nsasextr-seq-no

           exec cics startbr
              dataset    ('NSASEXTR')
              ridfld     (nsasextr-ridfld)
              gteq
              resp       (ws-response)
           end-exec

           if RESP-NOTFND or resp-endfile
              go to 0500-end-nsasextr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' nsasextr error startbr '
                                       to ra-message
                 display 'error-nsasextr-startbr ' ws-response
                 go to 0000-return
              end-if
           end-if
           move 'Y'                    to ws-NS-startbr-ind

           .
       0500-nsasextr-loop.

           exec cics readnext
              dataset    ('NSASEXTR')
              ridfld     (nsasextr-ridfld)
              into       (nsas-extract-record)
              resp       (ws-response)
           end-exec

           if (RESP-NOTFND)
              or (resp-endfile)
              or (nsas-company-cd <> opk-company-cd)
              or (nsas-archive-no <> la-archive-no)
              go to 0500-end-nsasextr
           else
              if not resp-normal
                 move ws-response      to ra-status-code
                 move ' nsasextr error readnext '
                                       to ra-message
                 display 'error-nsasextr-readnext ' ws-response
                 go to 0000-return
              end-if
           end-if

           move nsas-control-primary   to nsasextr-ridfld

           exec cics read update
              dataset  ('NSASEXTR')
              into     (nsas-extract-record)
              ridfld   (nsasextr-ridfld)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' nsasextr read update error '
                                       to ra-message
              display 'error-nsasextr-read update ' ws-response
              go to 0000-return
           end-if

           move zeros                  to nscntr
           perform varying ns1 from +1 by +1 until
              nscntr = +10
              if nsas-letter-variables(ns1:1) = '~'
                 compute nscntr = nscntr + 1
              end-if
           end-perform

           if nscntr = +10
              if (nsas-letter-variables(ns1:10) =
                    ws-old-greg-eff-date)
                 and (nsas-letter-variables(ns1:10) <>
                    ws-new-greg-eff-date)
                 move ws-new-greg-eff-date
                                       to nsas-letter-variables(ns1:10)
              end-if
              perform varying ns1 from ns1 by +1
                 until (nsas-letter-variables(ns1:1) = '~')
                    or (ns1 > +4000)
              end-perform
              compute ns1 = ns1 + 1
              if ns1 < +4000
                 if (nsas-letter-variables(ns1:11) =
                       opk-cert-no)
                    and (nsas-letter-variables(ns1:11) <>
                       pa-new-cert-no)
                    move pa-new-cert-no to
                       nsas-letter-variables(ns1:11)
                 end-if
              end-if
           end-if

           exec cics rewrite
              dataset  ('NSASEXTR')
              from     (nsas-extract-record)
              resp     (ws-response)
           end-exec

           if not resp-normal
              move ws-response         to ra-status-code
              move ' nsasextr write  error '
                                       to ra-message
              display 'error-nsasextr-write   ' ws-response
              go to 0000-return
           end-if

           go to 0500-nsasextr-loop

           .
       0500-end-nsasextr.

           if ws-NS-startbr-ind = 'Y'
              exec cics endbr
                 dataset     ('NSASEXTR')
              end-exec
           end-if

           .
       0500-exit.
           exit.

       8500-DATE-CONVERT.
           EXEC CICS LINK
               PROGRAM  ('ELDATCV')
               COMMAREA (DATE-CONVERSION-DATA)
               LENGTH   (DC-COMM-LENGTH)
           END-EXEC

           .
       8500-EXIT.
           EXIT.
                                            