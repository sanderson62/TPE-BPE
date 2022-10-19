000100 IDENTIFICATION DIVISION.                                         00000010
000200 PROGRAM-ID. CIIMNET.                                             00000011
000300 AUTHOR.  C. S. O.                                                00000012
000400          OMAHA, NE.                                              00000013
000500                                                                  00000014
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
122002******************************************************************
000600 ENVIRONMENT DIVISION.                                            00000015
000700 CONFIGURATION SECTION.                                           00000016
000800 SPECIAL-NAMES.                                                   00000017
000900     C01 IS NEW-PAGE.                                             00000018
001000                                                                  00000019
001100 INPUT-OUTPUT SECTION.                                            00000025
001200 FILE-CONTROL.                                                    00000026
001300                                                                  00000047
001400                                                                  00000048
001500     SELECT CLAIM-FILE       ASSIGN TO SYS011-UT-FBA1-S-CLMFILE.  00000049
001600                                                                  00000050
001700     SELECT PNDB-FILE        ASSIGN TO SYS012-UT-FBA1-S-PNDBFILE. 00000051
001800                                                                  00000052
001900     SELECT CRTC-FILE        ASSIGN TO SYS013-UT-FBA1-S-CRTCFILE. 00000053
002000                                                                  00000054
002100     SELECT HDR-FILE         ASSIGN TO HDRFILE
                   organization is line sequential.
002200                                                                  00000056
002300     SELECT DATE-KARD        ASSIGN TO SYS006-UR-2540R-S-SYS006.  00000057
002400                                                                  00000058
002500     SELECT  DISPLAY-PRT     ASSIGN TO SYS022-UR-1403-S-SYS022.   00000059
002600                                                                  00000060
002700     SELECT  ERROR-PRT       ASSIGN TO SYS023-UR-1403-S-SYS023.   00000061
110303
110303     SELECT DISK-DATE        ASSIGN TO SYS019.
002800                                                                  00000062
002900******************************************************************00000063
003000   EJECT                                                          00000064
003100 DATA DIVISION.                                                   00000065
003200 FILE SECTION.                                                    00000066
003300******************************************************************00000067
003400                                                                  00000068
003500 FD  DISPLAY-PRT                                                  00000069
003600     LABEL RECORDS ARE STANDARD                                   00000070
003700     RECORD CONTAINS 81 CHARACTERS                                00000071
003800     BLOCK CONTAINS 0 RECORDS                                     00000072
003900     DATA RECORD IS DISPLAY-REC.                                  00000073
004000                                                                  00000074
004100 01  DISPLAY-REC.                                                 00000075
004200     12  DISPLAY-CC              PIC X.                           00000076
004300     12  DISPLAY-INFO.                                            00000077
004400         15  DISPLAY-INFO-11     PIC X(11).                       00000078
004500         15  FILLER              PIC X.                           00000079
004600         15  DISPLAY-INFO-OTHERS PIC X(68).                       00000080
004700                                                                  00000081
004800******************************************************************00000082
004900                                                                  00000083
005000 FD  ERROR-PRT                                                    00000084
005100     LABEL RECORDS ARE STANDARD                                   00000085
005200     RECORD CONTAINS 81 CHARACTERS                                00000086
005300     BLOCK CONTAINS 0 RECORDS                                     00000087
005400     DATA RECORD IS ERR-REC.                                      00000088
005500                                                                  00000089
005600 01  ERR-REC.                                                     00000090
005700     12  ERR-CC              PIC X.                               00000091
005800     12  ERR-INFO            PIC X(80).                           00000092
005900                                                                  00000093
006000******************************************************************00000094
006100                                                                  00000095
006200 FD  DATE-KARD                                                    00000096
006300     LABEL RECORDS ARE STANDARD                                   00000097
006400     RECORD CONTAINS 80 CHARACTERS                                00000098
006500     DATA RECORD IS DATE-CARD-REC.                                00000099
006600                                                                  00000100
006700 01  DATE-CARD-REC            PIC X(80).                          00000101
006800******************************************************************00000102
006900                                                                  00000103
007000 FD  CLAIM-FILE                                                   00000104
007100     LABEL RECORDS ARE STANDARD                                   00000105
007200     RECORD CONTAINS 350 CHARACTERS                               00000106
007300     DATA RECORD IS CLAIM-REC.                                    00000107
007400                                                                  00000108
007500 01  CLAIM-REC                       PIC X(350).                  00000109
007600******************************************************************00000110
007700                                                                  00000111
007800 FD  PNDB-FILE                                                    00000112
007900     LABEL RECORDS ARE STANDARD                                   00000113
008000     RECORD CONTAINS 585 CHARACTERS                               00000114
008100     DATA RECORD IS PNDB-REC.                                     00000115
008200                                                                  00000116
008300 01  PNDB-REC                       PIC X(1056).                  00000117
008400******************************************************************00000118
008500                                                                  00000119
008600 FD  CRTC-FILE                                                    00000120
008700     LABEL RECORDS ARE STANDARD                                   00000121
008800     RECORD CONTAINS 300 CHARACTERS                               00000122
008900     DATA RECORD IS CRTC-REC.                                     00000123
009000                                                                  00000124
009100 01  CRTC-REC                            PIC X(300).              00000125
009200******************************************************************00000126
009300                                                                  00000127
009400 FD  HDR-FILE                                                     00000128
009500     LABEL RECORDS ARE STANDARD                                   00000129
009600     BLOCK CONTAINS 0 RECORDS.                                    00000130
009700                                                                  00000131
009800 01  HDR-REC                 PIC X(80).
009900                                                                  00000133
110303 FD  DISK-DATE                                                    
110303                             COPY ELCDTEFD.                       
110303 EJECT                                                            
010200                                                                  00000136
010300 WORKING-STORAGE SECTION.                                         00000137
010400                                                                  00000138
010500 01  FILLER        PIC X(24) VALUE 'CIIMNET WORKING STORAGE '.    00000139
010600******************************************************************00000140
010700                                                                  00000141
010800 77  ERROR-CODE              PIC X(90)  VALUE SPACES.             00000142
010900 77  ERR-FLAG                PIC X      VALUE 'N'.                00000143
011000 77  SUB-IN                  PIC 99     VALUE ZEROS.              00000144
011100 77  SUB-OUT                 PIC 99     VALUE ZEROS.              00000145
011200 77  SPACE-SUB               PIC 99     VALUE ZEROS.              00000146
011300 77  E-SUB                   PIC 99     VALUE ZEROS.              00000147
011400 77  SAVE-BIN-DATE           PIC XX     VALUE LOW-VALUES.         00000148
011500 77  ELMSTR-FILE-STATUS      PIC XX     VALUE ZERO.               00000149
011600 77  CLAIM-CNTR              PIC 9(09)  VALUE ZERO.               00000150
011700 77  CLAIM-HDR               PIC 9(09)  VALUE ZERO.               00000151
011800 77  PNDB-CNTR               PIC 9(09)  VALUE ZERO.               00000152
011900 77  PNDB-HDR                PIC 9(09)  VALUE ZERO.               00000153
012000 77  CRTC-CNTR               PIC 9(09)  VALUE ZERO.               00000154
012100 77  CRTC-HDR                PIC 9(09)  VALUE ZERO.               00000155
012200 77  TOT-HDR                 PIC 9(09)  VALUE ZERO.               00000156
012300 77  ERR-CNTR                PIC 9(09)  VALUE ZERO.               00000157
012400 77  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.             00000158
012500 77  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZEROS.              00000159
012600 77  ERR-HEAD-SW             PIC X       VALUE 'Y'.               00000160
012700 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               00000161
012800 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             00000162
012900 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             00000163
013000 77  ERR-LINE-CNT            PIC 99      VALUE ZEROS.             00000164
110303 77  PGM-SUB                 PIC S999 COMP  VALUE +521.
110303 77  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.

011304 01  FILLER.
011304     05  WS-DCC-ERR-RPT-TITLE PIC X(26)
011304                              VALUE 'DCC TO IMNET ERROR REPORT '.
011304     05  WS-DCC-RPT-TITLE     PIC X(24)
011304                              VALUE 'DCC TO IMNET LOAD REPORT'.

013100                                                                  00000165
013200******************************************************************00000166
013300                                                                  00000167
013400 01  ERR-HD-1.                                                    00000168
013500     12  FILLER      PIC X     VALUE SPACES.                      00000169
013600     12  FILLER      PIC X(21) VALUE SPACES.                      00000170
011304     12  ERR-HD-1-TITLE      PIC X(26) 
                                   VALUE 'CID TO IMNET ERROR REPORT '. 
013800     12  FILLER      PIC X(20) VALUE SPACES.                      00000172
013900     12  FILLER      PIC X(07) VALUE 'CIIMNET'.                   00000173
014000     12  FILLER      PIC X(03) VALUE SPACES.                      00000174
014100                                                                  00001351
014200******************************************************************00001352
014300                                                                  00001353
014400 01  DISPLAY-HD-1.                                                00001354
014500     12  FILLER      PIC X     VALUE SPACES.                      00001355
014600     12  FILLER      PIC X(21) VALUE SPACES.                      00001356
011304     12  DISPLAY-HD-1-TITLE  PIC X(24) 
                                   VALUE 'CID TO IMNET LOAD REPORT'.  
014800     12  FILLER      PIC X(20) VALUE SPACES.                      00001358
014900     12  FILLER      PIC X(07) VALUE 'CIIMNET'.                   00001359
015000     12  FILLER      PIC X(05) VALUE SPACES.                      00001360
015100                                                                  00001361
015200******************************************************************00001362
015300                                                                  00001363
015400 01  DISPLAY-HD-2.                                                00001364
015500     12  FILLER      PIC X     VALUE SPACES.                      00001365
015600     12  FILLER      PIC X(24) VALUE SPACES.                      00001366
015700     12  FILLER      PIC X(11) VALUE 'RUN DATE = '.               00001367
015800     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      00001368
015900     12  FILLER      PIC X(22) VALUE SPACES.                      00001369
016000                                                                  00001370
016100******************************************************************00001371
016200                                                                  00001372
016300 01  ERR-LINE.                                                    00001373
016400     05  ERR-LINE-05.                                             00001374
016500         10  ERR-LINE-CC         PIC X.                           00001375
016600         10  ERR-LINE-REC        PIC X(79).                       00001376
016700                                                                  00001377
016800     05  ERR-LINE-ALT  REDEFINES  ERR-LINE-05.                    00001378
016900         10  ERR-CC-ALT          PIC X.                           00001379
017000         10  ERR-FLD-1           PIC X(35).                       00001380
017100         10  ERR-FLD-2           PIC X(44).                       00001381
017200                                                                  00001442
017300                                                                  00001443
017400******************************************************************00001444
017500                                                                  00001445
017600 01  DISPLAY-LINE.                                                00001446
017700     05  DISPLAY-LINE-05.                                         00001447
017800         10  DIS-CC              PIC X.                           00001448
017900         10  DIS-LINE-REC        PIC X(79).                       00001449
018000                                                                  00001450
018100     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            00001451
018200         10  DIS-CC-ALT          PIC X.                           00001452
018300         10  DIS-FLD-1           PIC X(35).                       00001453
018400         10  DIS-FLD-2           PIC X(44).                       00001454
018500                                                                  00001455
018600******************************************************************00001456
018700                                                                  00001457
018800 01  CC-STATUS-CODE.                                              00001458
018900     05  CC-STATUS-1         PIC X      VALUE ZERO.               00001459
019000     05  CC-STATUS-2         PIC X      VALUE ZERO.               00001460
019100******************************************************************00001461
019200                                                                  00001462
019300 01  PB-STATUS-CODE.                                              00001463
019400     05  PB-STATUS-1         PIC X      VALUE ZERO.               00001464
019500     05  PB-STATUS-2         PIC X      VALUE ZERO.               00001465
019600******************************************************************00001466
019700                                                                  00001467
019800 01  SAVE-CMP-CODE           PIC S9(4) COMP   VALUE +04.          00001468
019900 01  FILLER        REDEFINES      SAVE-CMP-CODE.                  00001469
020000     05 FILLER               PIC X.                               00001470
020100     05 SAVE-CMP-CD          PIC X.                               00001471
020200******************************************************************00001472
020300                                                                  00001473
020400 01  CRT-NUM.                                                     00001474
020500     05 CRT-2                PIC X(02)   VALUE ZEROS.             00001475
020600     05 CRT-7                PIC X(07)   VALUE ZEROS.             00001476
020700     05 CRT-1                PIC X(01)   VALUE ZEROS.             00001477
020800******************************************************************00001478
020900                                                                  00001479
021000 01  EDIT-CRT-NUM.                                                00001480
021100     05 E-CRT-3              PIC X(03).                           00001481
021200     05 E-CRT-7              PIC X(07).                           00001482
021300                                                                  00001483
021400 01  EDIT-CRT-NUM-N REDEFINES  EDIT-CRT-NUM.                      00001484
021500     05 E-CRT-NUMERIC        PIC 9(10).                           00001485
021600******************************************************************00001486
021700                                                                  00001487
021800 01  CL-NUMBER.                                                   00001488
021900     05 CL-FIRST-3           PIC X(03)   VALUE ZEROS.             00001489
022000     05 CL-LAST-7.                                                00001490
022100         10 CL-LAST-2        PIC X(02)   VALUE ZEROS.             00001491
022200         10 CL-LAST-5        PIC X(05)   VALUE ZEROS.             00001492
022300******************************************************************00001493
022400                                                                  00001494
022500 01  NAME-FLD-IN-X                     PIC X(35).                 00001495
022600                                                                  00001496
022700 01  NAME-FLD-IN  REDEFINES NAME-FLD-IN-X.                        00001497
022800     05 FLD-POS-IN   OCCURS 35 TIMES   PIC X.                     00001498
022900******************************************************************00001499
023000                                                                  00001500
023100 01  NAME-FLD-OUT-X                    PIC X(35).                 00001501
023200                                                                  00001502
023300 01  NAME-FLD-OUT  REDEFINES NAME-FLD-OUT-X.                      00001503
023400     05 FLD-POS-OUT  OCCURS 35 TIMES   PIC X.                     00001504
023500******************************************************************00001505
023600                                                                  00001506
023700 01  ML-NAME.                                                     00001507
023800     05 ML-LAST-NAME         PIC X(15)   VALUE SPACES.            00001508
023900     05 ML-FIRST             PIC X(10)   VALUE SPACES.            00001509
024000     05 ML-INIT              PIC X(01)   VALUE SPACES.            00001510
024100     05 FILLER               PIC X(09)   VALUE SPACES.            00001511
024200******************************************************************00001512
024300                                                                  00001513
024400 01  CL-NAME.                                                     00001514
024500     05 CL-LAST-NAME         PIC X(15)      VALUE SPACES.         00001515
024600     05 CL-FIRST             PIC X(13)      VALUE SPACES.         00001516
024700     05 CL-INIT              PIC X(01)      VALUE SPACES.         00001517
024800     05 FILLER               PIC X(06)      VALUE SPACES.         00001518
024900******************************************************************00001519
025000                                                                  00001520
025100 01  CM-NAME.                                                     00001521
025200     05 CM-LAST-NAME         PIC X(15)      VALUE SPACES.         00001522
025300     05 CM-INITS.                                                 00001523
025400        10 CM-FIRST-INIT     PIC X(01)      VALUE SPACES.         00001524
025500        10 CM-SECOND-INIT    PIC X(01)      VALUE SPACES.         00001525
025600     05 FILLER               PIC X(18)      VALUE SPACES.         00001526
025700******************************************************************00001527
025800                                                                  00001528
025900 01  WS-GROUP.                                                    00001529
026000     05 WS-HDR-COUNT         PIC 9(7)       VALUE 0.              00001530
026100     05 WS-EXTR-COUNT        PIC 9(7)       VALUE 0.              00001531
026200******************************************************************00001532
026300                                                                  00001533
026400 01  WS-ISSUE-DATE.                                               00001534
026500     05  WS-ISS-MO          PIC X(02)    VALUE SPACES.            00001535
026600     05  WS-ISS-DA          PIC X(02)    VALUE SPACES.            00001536
026700     05  WS-ISS-YR          PIC X(02)    VALUE SPACES.            00001537
026800******************************************************************00001538
026900                                                                  00001539
027000 01  OVERRIDE-DATE-IN.                                            00001540
027100     05  OVERRIDE-DATE.                                           00001541
027200         10  OVERRIDE-MO         PIC X(02)    VALUE SPACES.       00001542
027300         10  FILLER              PIC X(01)    VALUE '/'.          00001543
027400         10  OVERRIDE-DA         PIC X(02)    VALUE SPACES.       00001544
027500         10  FILLER              PIC X(01)    VALUE '/'.          00001545
027600         10  OVERRIDE-YR         PIC X(02)    VALUE SPACES.       00001546
027700     05  FILLER                  PIC X(17)    VALUE               00001547
027800                                 ' PREV CYC DATE = '.             00001548
027900     05  P-C-DATE                PIC X(08)    VALUE SPACES.       00001549
028000     05  FILLER                  PIC X(47)    VALUE SPACES.       00001550
028100******************************************************************00001551
028200                                                                  00001552
028300 01  WS-CURR-DATE.                                                00001553
028400     05  WS-CURR-MO          PIC X(02)    VALUE SPACES.           00001554
028500     05  FILLER              PIC X(01)    VALUE '/'.              00001555
028600     05  WS-CURR-DA          PIC X(02)    VALUE SPACES.           00001556
028700     05  FILLER              PIC X(01)    VALUE '/'.              00001557
028800     05  WS-CURR-YR          PIC X(02)    VALUE SPACES.           00001558
028900******************************************************************00001559
029000                                                                  00001560
029100 01  WS-CYC-DATE.                                                 00001561
029200     05  WS-CYC-MO           PIC X(02)    VALUE SPACES.           00001562
029300     05  FILLER              PIC X(01)    VALUE '/'.              00001563
029400     05  WS-CYC-DA           PIC X(02)    VALUE SPACES.           00001564
029500     05  FILLER              PIC X(01)    VALUE '/'.              00001565
029600     05  WS-CYC-YR           PIC X(02)    VALUE SPACES.           00001566
029700******************************************************************00001567
       01  WS-WORK-DATE            PIC 9(11)    VALUE ZEROS.
       01  FILLER REDEFINES WS-WORK-DATE.
           10  FILLER              PIC XXX.
           10  WS-WORK-CCYY        PIC X(4).
           10  WS-WORK-MM          PIC XX.
           10  WS-WORK-DD          PIC XX.

CIDVAO 01  DATE-IN.                                                     00001568
CIDVAO     05  WORK-DATE-IN.
CIDVAO         10   DATE-YY-IN     PIC 99       VALUE ZEROES.
CIDVAO         10   DATE-MM-IN     PIC 99       VALUE ZEROES.
CIDVAO         10   DATE-DD-IN     PIC 99       VALUE ZEROES.
CIDVAO***********************************************************
029900 01  WORK-DATE.                                                   00001569
030000     05  WORK-CENT           PIC X(02)    VALUE SPACES.           00001570
030100     05  WORK-YR             PIC X(02)    VALUE SPACES.           00001571
030200******************************************************************00001572
030300                                                                  00001573
030400 01  WS-HDR-REC.                                                  00001574
030500     05  H-11.                                                    00001575
030600         10  H-TRANS-CODE        PIC X        VALUE SPACES.       00001576
030700         10  H-POLICY-NUM        PIC X(10)    VALUE SPACES.       00001577
030800     05  H-OTHERS.                                                00001578
030900         10  H-INS-NAME          PIC X(35)    VALUE SPACES.       00001579
031000         10  H-ISS-STATE         PIC XX       VALUE SPACES.       00001580
031100         10  H-DOB.                                               00001581
031200             15  H-YR.                                            00001582
031300                 20  H-CENTURY   PIC XX       VALUE SPACES.       00001583
031400                 20  H-YEAR      PIC XX       VALUE SPACES.       00001584
031500             15  H-MONTH         PIC XX       VALUE SPACES.       00001585
031600             15  H-DAY           PIC XX       VALUE SPACES.       00001586
031700         10  H-IMNET-DT.                                          00001587
031800             20  IMNET-YR        PIC XXXX     VALUE SPACES.       00001588
031900             20  IMNET-MO        PIC XX       VALUE SPACES.       00001589
032000             20  IMNET-DA        PIC XX       VALUE SPACES.       00001590
032100         10  H-STATUS            PIC X        VALUE SPACES.       00001591
               10  H-ACCOUNT           PIC X(10)    VALUE SPACES.
032200         10  H-FILLER            PIC X(5)     VALUE SPACES.       00001592
032300                                                                  00001593
032400******************************************************************00001594
032500                                                                  00001595
032600 01  EDIT-LINE               PIC X(80).                           00001596
032700 01  FILLER   REDEFINES   EDIT-LINE.                              00001597
032800     05  EDIT-POS  OCCURS 80 TIMES PIC X.                         00001598
032900                                                                  00001599
032900                                                                  00001599
033000                                                                  00001600
033100 01  CLAIM-RECORD.                                                00001601
033200     12  CL-RECORD-ID                PIC XX.                      00001602
033300         88  VALID-CL-ID         VALUE 'CL'.                      00001603
033400                                                                  00001604
033500     12  CL-CONTROL-PRIMARY.                                      00001605
033600         16  CL-COMPANY-CD           PIC X.                       00001606
033700         16  CL-CARRIER              PIC X.                       00001607
033800         16  CL-CLAIM-NO             PIC X(7).                    00001608
033900         16  CL-CERT-NO.                                          00001609
034000             20  CL-CERT-PRIME       PIC X(10).                   00001610
034100             20  CL-CERT-SFX         PIC X.                       00001611
034200                                                                  00001612
034300     12  CL-CONTROL-BY-NAME.                                      00001613
034400         16  CL-COMPANY-CD-A1        PIC X.                       00001614
034500         16  CL-INSURED-LAST-NAME    PIC X(15).                   00001615
034600         16  CL-INSURED-NAME.                                     00001616
034700             20  CL-INSURED-1ST-NAME PIC X(12).                   00001617
034800             20  CL-INSURED-MID-INIT PIC X.                       00001618
034900                                                                  00001619
035000     12  CL-CONTROL-BY-SSN.                                       00001620
035100         16  CL-COMPANY-CD-A2        PIC X.                       00001621
035200         16  CL-SOC-SEC-NO.                                       00001622
035300             20  CL-SSN-STATE        PIC XX.                      00001623
035400             20  CL-SSN-ACCOUNT      PIC X(6).                    00001624
035500             20  CL-SSN-LN3          PIC X(3).                    00001625
035600                                                                  00001626
035700     12  CL-CONTROL-BY-CERT-NO.                                   00001627
035800         16  CL-COMPANY-CD-A4        PIC X.                       00001628
035900         16  CL-CERT-NO-A4.                                       00001629
036000             20  CL-CERT-A4-PRIME    PIC X(10).                   00001630
036100             20  CL-CERT-A4-SFX      PIC X.                       00001631
TSTMOD                                                                  00001619
T35000     12  CL-CONTROL-BY-CCN.                                       00001620
T35100         16  CL-COMPANY-CD-A5        PIC X.                       00001621
T35200         16  CL-CCN-A5.                                           00001622
T35300             20  CL-CCN              PIC X(20).                   00001623
TSTMOD                                                                  00001632
036300     12  CL-INSURED-PROFILE-DATA.                                 00001633
036400         16  CL-INSURED-BIRTH-DT     PIC XX.                      00001634
036500         16  CL-INSURED-SEX-CD       PIC X.                       00001635
036600             88  INSURED-IS-MALE        VALUE 'M'.                00001636
036700             88  INSURED-IS-FEMALE      VALUE 'F'.                00001637
036800             88  INSURED-SEX-UNKNOWN    VALUE ' '.                00001638
036900         16  CL-INSURED-OCC-CD       PIC X(6).                    00001639
037000         16  FILLER                  PIC X(05).                   00001640
037100                                                                  00001641
037200     12  CL-PROCESSING-INFO.                                      00001642
037300         16  CL-PROCESSOR-ID         PIC X(4).                    00001643
037400         16  CL-CLAIM-STATUS         PIC X.                       00001644
037500             88  CLAIM-IS-OPEN          VALUE 'O'.                00001645
037600             88  CLAIM-IS-CLOSED        VALUE 'C'.                00001646
037700         16  CL-CLAIM-TYPE           PIC X.                       00001647
037800*            88  AH-CLAIM               VALUE 'A'.                00001648
037900*            88  LIFE-CLAIM             VALUE 'L'.                00001649
038000*            88  PROPERTY-CLAIM         VALUE 'P'.                00001650
038100*            88  UNEMPLOYMENT-CLAIM     VALUE 'U'.                00001651
038200         16  CL-CLAIM-PREM-TYPE      PIC X.                       00001652
038300             88  SINGLE-PREMIUM         VALUE '1'.                00001653
038400             88  O-B-COVERAGE           VALUE '2'.                00001654
038500             88  OPEN-END-COVERAGE      VALUE '3'.                00001655
038600         16  CL-INCURRED-DT          PIC XX.                      00001656
038700         16  CL-REPORTED-DT          PIC XX.                      00001657
038800         16  CL-FILE-ESTABLISH-DT    PIC XX.                      00001658
038900         16  CL-EST-END-OF-DISAB-DT  PIC XX.                      00001659
039000         16  CL-LAST-PMT-DT          PIC XX.                      00001660
039100         16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.        00001661
039200         16  CL-PAID-THRU-DT         PIC XX.                      00001662
039300         16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.        00001663
039400         16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.        00001664
039500         16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.          00001665
039600         16  CL-PMT-CALC-METHOD      PIC X.                       00001666
039700             88  CL-360-DAY-YR          VALUE '1'.                00001667
039800             88  CL-365-DAY-YR          VALUE '2'.                00001668
039900             88  CL-FULL-MONTHS         VALUE '3'.                00001669
040000         16  CL-CAUSE-CD             PIC X(6).                    00001670
040100                                                                  00001671
040200         16  CL-PRIME-CERT-NO.                                    00001672
040300             20  CL-PRIME-CERT-PRIME PIC X(10).                   00001673
040400             20  CL-PRIME-CERT-SFX   PIC X.                       00001674
040500                                                                  00001675
040600         16  CL-SYSTEM-IDENTIFIER    PIC XX.                      00001676
040700             88  CL-CREDIT-CLAIM        VALUE 'CR'.               00001677
040800             88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.               00001678
040900                                                                  00001679
041000         16  CL-MICROFILM-NO         PIC X(10).                   00001680
041100         16  CL-PROG-FORM-TYPE       PIC X.                       00001681
041200         16  CL-LAST-ADD-ON-DT       PIC XX.                      00001682
041300                                                                  00001683
041400         16  CL-LAST-REOPEN-DT       PIC XX.                      00001684
041500         16  CL-LAST-CLOSE-DT        PIC XX.                      00001685
041600         16  CL-LAST-CLOSE-REASON    PIC X.                       00001686
041700             88  FINAL-PAID             VALUE '1'.                00001687
041800             88  CLAIM-DENIED           VALUE '2'.                00001688
041900             88  AUTO-CLOSE             VALUE '3'.                00001689
042000             88  MANUAL-CLOSE           VALUE '4'.                00001690
042100         16  CL-ASSOC-CERT-SEQU      PIC S99.                     00001691
042200         16  CL-ASSOC-CERT-TOTAL     PIC S99.                     00001692
042300         16  CL-CLAIM-PAYMENT-STATUS PIC 9.                       00001693
042400             88  PAYMENT-IN-PREP        VALUE 1 THRU 9.           00001694
042500         16  FILLER                  PIC X(05).                   00001695
042600                                                                  00001696
042700     12  CL-CERTIFICATE-DATA.                                     00001697
042800         16  CL-CERT-ORIGIN          PIC X.                       00001698
042900             88  CERT-WAS-ONLINE        VALUE '1'.                00001699
043000             88  CERT-WAS-CREATED       VALUE '2'.                00001700
043100             88  COVERAGE-WAS-ADDED     VALUE '3'.                00001701
043200         16  CL-CERT-KEY-DATA.                                    00001702
043300             20  CL-CERT-CARRIER     PIC X.                       00001703
043400             20  CL-CERT-GROUPING    PIC X(6).                    00001704
043500             20  CL-CERT-STATE       PIC XX.                      00001705
043600             20  CL-CERT-ACCOUNT.                                 00001706
043700                 24  CL-CERT-ACCOUNT-PREFIX PIC X(4).             00001707
043800                 24  CL-CERT-ACCOUNT-PRIME  PIC X(6).             00001708
043900             20  CL-CERT-EFF-DT      PIC XX.                      00001709
044000                                                                  00001710
044100     12  CL-STATUS-CONTROLS.                                      00001711
044200         16  CL-PRIORITY-CD          PIC X.                       00001712
044300             88  HIGHEST-PRIORITY       VALUE '9'.                00001713
044400         16  CL-SUPV-ATTN-CD         PIC X.                       00001714
044500             88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.            00001715
044600             88  SUPV-IS-REQUIRED       VALUE 'Y'.                00001716
044700         16  CL-PURGED-DT            PIC XX.                      00001717
044800         16  CL-RESTORED-DT          PIC XX.                      00001718
044900         16  CL-NEXT-AUTO-PAY-DT     PIC XX.                      00001719
045000         16  CL-NEXT-RESEND-DT       PIC XX.                      00001720
045100         16  CL-NEXT-FOLLOWUP-DT     PIC XX.                      00001721
045200         16  FILLER                  PIC XX.                      00001722
045300         16  CL-LAST-MAINT-DT        PIC XX.                      00001723
045400         16  CL-LAST-MAINT-USER      PIC X(4).                    00001724
045500         16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.        00001725
045600         16  CL-LAST-MAINT-TYPE      PIC X.                       00001726
045700             88  CLAIM-SET-UP           VALUE ' '.                00001727
045800             88  PAYMENT-MADE           VALUE '1'.                00001728
045900             88  LETTER-SENT            VALUE '2'.                00001729
046000             88  MASTER-WAS-ALTERED     VALUE '3'.                00001730
046100             88  MASTER-WAS-RESTORED    VALUE '4'.                00001731
046200             88  INCURRED-DATE-CHANGED  VALUE '5'.                00001732
046300             88  FILE-CONVERTED         VALUE '6'.                00001733
046400         16  CL-RELATED-CLAIM-NO     PIC X(7).                    00001734
046500         16  CL-HISTORY-ARCHIVE-DT   PIC XX.                      00001735
046600         16  CL-BENEFICIARY          PIC X(10).                   00001736
046700         16  FILLER                  PIC X(10).                   00001737
046800                                                                  00001738
046900     12  CL-TRAILER-CONTROLS.                                     00001739
047000         16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.          00001740
047100             88  CL-1ST-TRL-AVAIL       VALUE +4095.              00001741
047200             88  CL-LAST-TRL-AVAIL      VALUE +100.               00001742
047300             88  CL-RESV-EXP-HIST-TRLR  VALUE +0.                 00001743
047400         16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.          00001744
047500         16  FILLER                  PIC XX.                      00001745
047600         16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.          00001746
047700         16  CL-ADDRESS-TRAILER-CNT.                              00001747
047800             20  CL-INSURED-ADDR-CNT  PIC S9(1).                  00001748
047900                 88  NO-INSURED-AVAILABLE    VALUE ZERO.          00001749
048000             20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).                  00001750
048100                 88  ACCOUNT-IS-ONLINE       VALUE ZERO.          00001751
048200             20  CL-BENIF-ADDR-CNT    PIC S9(1).                  00001752
048300                 88  BENEFICIARY-IS-ONLINE   VALUE ZERO.          00001753
048400             20  CL-EMPLOYER-ADDR-CNT PIC S9(1).                  00001754
048500                 88  NO-EMPLOY-AVAILABLE     VALUE ZERO.          00001755
048600             20  CL-DOCTOR-ADDR-CNT   PIC S9(1).                  00001756
048700                 88  NO-DOCTOR-AVAILABLE     VALUE ZERO.          00001757
048800             20  CL-OTHER-1-ADDR-CNT  PIC S9(1).                  00001758
048900                 88  NO-OTHER-1-ADDRESSES    VALUE ZERO.          00001759
049000             20  CL-OTHER-2-ADDR-CNT  PIC S9(1).                  00001760
049100                 88  NO-OTHER-2-ADDRESSES    VALUE ZERO.          00001761
049200                                                                  00001762
049300     12  CL-CV-REFERENCE-NO.                                      00001763
049400         16  CL-CV-REFNO-PRIME       PIC X(18).                   00001764
049500         16  CL-CV-REFNO-SFX         PIC X(02).                   00001765
049600                                                                  00001766
049700     12  CL-FILE-LOCATION            PIC X(4).                    00001767
049800                                                                  00001768
049900     12  CL-PROCESS-ERRORS.                                       00001769
050000         16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.          00001770
050100             88  NO-FATAL-ERRORS        VALUE ZERO.               00001771
050200         16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.          00001772
050300             88  NO-FORCABLE-ERRORS     VALUE ZERO.               00001773
050400                                                                  00001774
050500     12  CL-PRODUCT-CD               PIC X.                       00001775
050600                                                                  00001776
050700     12  CL-CURRENT-KEY-DATA.                                     00001777
050800         16  CL-CURRENT-CARRIER      PIC X(01).                   00001778
050900         16  CL-CURRENT-GROUPING     PIC X(06).                   00001779
051000         16  CL-CURRENT-STATE        PIC X(02).                   00001780
051100         16  CL-CURRENT-ACCOUNT      PIC X(10).                   00001781
051200                                                                  00001782
051300     12  CL-ASSOCIATES               PIC X(01).                   00001783
051400         88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.                00001784
051500         88  CL-ASSOC-INTERFACE         VALUE 'I'.                00001785
051600         88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.                00001786
051700         88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.                00001787
051800                                                                  00001788
051900     12  CL-ACTIVITY-CODE            PIC 9(02).                   00001789
052000     12  CL-ACTIVITY-MAINT-DT        PIC X(02).                   00001790
052100     12  CL-ACTIVITY-MAINT-TYPE      PIC X(04).                   00001791
052200                                                                  00001792
052300     12  CL-LAPSE-REPORT-CODE        PIC 9(01).                   00001793
052400     12  CL-LAG-REPORT-CODE          PIC 9(01).                   00001794
052500     12  CL-LOAN-TYPE                PIC X(02).                   00001795
052600     12  CL-LEGAL-STATE              PIC X(02).                   00001796
052700                                                                  00001797
052800     12  FILLER                      PIC X(05).                   00001798
052900     12  CL-YESNOSW                  PIC X.                       00001799
053000     12  FILLER                      PIC X(04).                   00001800
053100******************************************************************00001801

                                       COPY ECSCRT01.

053200 01  PNDB-RECORD.                                                 00001802
053300     12  PB-RECORD-ID                     PIC XX.                 00001803
053400         88  VALID-PB-ID                        VALUE 'PB'.       00001804
053500                                                                  00001805
053600     12  PB-CONTROL-PRIMARY.                                      00001806
053700         16  PB-COMPANY-CD                PIC X.                  00001807
053800         16  PB-ENTRY-BATCH               PIC X(6).               00001808
053900         16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.     00001809
054000         16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.     00001810
054100                                                                  00001811
054200     12  PB-CONTROL-BY-ACCOUNT.                                   00001812
054300         16  PB-COMPANY-CD-A1             PIC X.                  00001813
054400         16  PB-CARRIER                   PIC X.                  00001814
054500         16  PB-GROUPING.                                         00001815
054600             20  PB-GROUPING-PREFIX       PIC XXX.                00001816
054700             20  PB-GROUPING-PRIME        PIC XXX.                00001817
054800         16  PB-STATE                     PIC XX.                 00001818
054900         16  PB-ACCOUNT.                                          00001819
055000             20  PB-ACCOUNT-PREFIX        PIC X(4).               00001820
055100             20  PB-ACCOUNT-PRIME         PIC X(6).               00001821
055200         16  PB-CERT-EFF-DT               PIC XX.                 00001822
055300         16  PB-CERT-NO.                                          00001823
055400             20  PB-CERT-PRIME            PIC X(10).              00001824
055500             20  PB-CERT-SFX              PIC X.                  00001825
055600         16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.     00001826
055700                                                                  00001827
055800         16  PB-RECORD-TYPE               PIC X.                  00001828
055900             88  PB-MAILING-DATA                VALUE '0'.        00001829
056000             88  PB-ISSUE                       VALUE '1'.        00001830
056100             88  PB-CANCELLATION                VALUE '2'.        00001831
056200             88  PB-BATCH-TRAILER               VALUE '9'.        00001832
056300                                                                  00001833
056400     12  PB-CONTROL-BY-ORIG-BATCH.                                00001834
056500         16  PB-ORIGINAL-COMPANY-CD       PIC X.                  00001835
056600         16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).               00001836
056700         16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.     00001837
056800         16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.     00001838
056900                                                                  00001839
057000     12  PB-CONTROL-BY-CSR.                                       00001840
057100         16  PB-CSR-COMPANY-CD            PIC X.                  00001841
057200         16  PB-CSR-ID                    PIC X(4).               00001842
057300         16  PB-CSR-ENTRY-BATCH           PIC X(6).               00001843
057400         16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.     00001844
057500         16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.     00001845
057600******************************************************************00001846
057700*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *00001847
057800******************************************************************00001848
057900                                                                  00001849
058000     12  PB-LAST-MAINT-DT                 PIC XX.                 00001850
058100     12  PB-LAST-MAINT-BY                 PIC X(4).               00001851
058200     12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.   00001852
058300                                                                  00001853
058400     12  PB-RECORD-BODY                   PIC X(375).             00001854
058500                                                                  00001855
058600     12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.              00001856
058700         16  PB-CERT-ORIGIN               PIC X.                  00001857
058800             88  CLASIC-CREATED-CERT         VALUE '1'.           00001858
058900         16  PB-I-NAME.                                           00001859
059000             20  PB-I-INSURED-LAST-NAME   PIC X(15).              00001860
059100             20  PB-I-INSURED-FIRST-NAME.                         00001861
059200                 24  PB-I-INSURED-1ST-INIT PIC X.                 00001862
059300                 24  FILLER                PIC X(9).              00001863
059400             20  PB-I-INSURED-MIDDLE-INIT PIC X.                  00001864
059500         16  PB-I-AGE                     PIC S99   COMP-3.       00001865
059600         16  PB-I-JOINT-AGE               PIC S99   COMP-3.       00001866
059700         16  PB-I-BIRTHDAY                PIC XX.                 00001867
059800         16  PB-I-INSURED-SEX             PIC X.                  00001868
059900             88  PB-SEX-MALE     VALUE 'M'.                       00001869
060000             88  PB-SEX-FEMALE   VALUE 'F'.                       00001870
060100                                                                  00001871
060200         16  PB-I-LF-TERM                 PIC S999   COMP-3.      00001872
060300         16  PB-I-AH-TERM                 PIC S999   COMP-3.      00001873
060400         16  PB-I-LOAN-TERM               PIC S999   COMP-3.      00001874
060500         16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.      00001875
060600         16  PB-I-SKIP-CODE               PIC X.                  00001876
060700             88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.         00001877
060800             88  PB-SKIP-JULY              VALUE '1'.             00001878
060900             88  PB-SKIP-AUGUST            VALUE '2'.             00001879
061000             88  PB-SKIP-SEPTEMBER         VALUE '3'.             00001880
061100             88  PB-SKIP-JULY-AUG          VALUE '4'.             00001881
061200             88  PB-SKIP-AUG-SEPT          VALUE '5'.             00001882
061300             88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.             00001883
061400             88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.             00001884
061500             88  PB-SKIP-JUNE              VALUE '8'.             00001885
061600             88  PB-SKIP-JUNE-JULY         VALUE '9'.             00001886
061700             88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.             00001887
061800             88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.             00001888
061900         16  PB-I-TERM-TYPE               PIC X.                  00001889
062000             88  PB-PAID-MONTHLY           VALUE ' ' 'M'.         00001890
062100             88  PB-PAID-WEEKLY            VALUE 'W'.             00001891
062200             88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.             00001892
062300             88  PB-PAID-BI-WEEKLY         VALUE 'B'.             00001893
062400             88  PB-PAID-13-YEARLY         VALUE 'T'.             00001894
062500         16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.      00001895
062600         16  PB-I-POLICY-FORM-NO          PIC X(12).              00001896
062700         16  PB-I-DATA-ENTRY-SW           PIC X.                  00001897
062800             88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.         00001898
062900             88  PB-EXT-DAYS-PROCESSING    VALUE '2'.             00001899
063000             88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.             00001900
063100             88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.             00001901
063200         16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.   00001902
063300         16  FILLER                       PIC X(4).               00001903
063400         16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.  00001904
063500                                                                  00001905
063600         16  PB-I-LIFE-BENEFIT-CD         PIC XX.                 00001906
063700             88  PB-VALID-LIFE               VALUE '01' THRU '89'.00001907
063800             88  PB-INVALID-LIFE             VALUE '  ' '00'      00001908
063900                                                   '90' THRU '99'.00001909
064000         16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD  00001910
064100                                          PIC XX.                 00001911
064200         16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.  00001912
064300         16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.  00001913
064400         16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.  00001914
064500         16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.  00001915
064600         16  PB-I-LF-CALC-FLAG            PIC X.                  00001916
064700             88 PB-COMP-LF-PREM               VALUE '?'.          00001917
064800         16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.  00001918
064900         16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.  00001919
065000         16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.  00001920
065100         16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.  00001921
065200         16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.  00001922
065300         16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.  00001923
065400         16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.  00001924
065500         16  PB-I-LF-ABBR                 PIC XXX.                00001925
065600         16  PB-I-LF-INPUT-CD             PIC XX.                 00001926
065700                                                                  00001927
065800         16  PB-I-AH-BENEFIT-CD           PIC XX.                 00001928
065900             88  PB-VALID-AH                 VALUE '01' THRU '89'.00001929
066000             88  PB-INVALID-AH               VALUE '  ' '00'      00001930
066100                                                   '90' THRU '99'.00001931
066200         16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.  00001932
066300         16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.  00001933
066400         16  PB-I-AH-CALC-FLAG            PIC X.                  00001934
066500             88 PB-COMP-AH-PREM                  VALUE '?'.       00001935
066600         16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.  00001936
066700         16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.  00001937
066800         16  PB-I-AH-POLICY-FEE           PIC S9(3)V99   COMP-3.  00001938
066900         16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.  00001939
067000         16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.  00001940
067100         16  PB-I-AH-ABBR                 PIC XXX.                00001941
067200         16  PB-I-AH-INPUT-CD             PIC XXX.                00001942
067300                                                                  00001943
067400         16  PB-I-SPECIAL-REIN-CODE       PIC X.                  00001944
067500         16  PB-I-REIN-TABLE              PIC XXX.                00001945
067600         16  PB-I-BUSINESS-TYPE           PIC 99.                 00001946
067700         16  PB-I-INDV-GRP-CD             PIC X.                  00001947
067800         16  PB-I-MORT-CODE.                                      00001948
067900             20  PB-I-TABLE               PIC X.                  00001949
068000             20  PB-I-INTEREST            PIC XX.                 00001950
068100             20  PB-I-MORT-TYP            PIC X.                  00001951
068200         16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.  00001952
068300         16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.  00001953
068400         16  FILLER                       PIC X(4).               00001954
068500         16  PB-I-INDV-GRP-OVRD           PIC X.                  00001955
068600         16  PB-I-RATE-CLASS-OVRD         PIC XX.                 00001956
068700         16  PB-I-SIG-SW                  PIC X.                  00001957
068800             88  PB-POLICY-SIGNED             VALUE 'Y'.          00001958
068900         16  PB-I-RATE-CLASS              PIC XX.                 00001959
069000         16  PB-I-RATE-DEVIATION-LF       PIC XXX.                00001960
069100         16  PB-I-RATE-DEVIATION-AH       PIC XXX.                00001961
069200         16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.  00001962
069300         16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.  00001963
069400         16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.  00001964
069500         16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.  00001965
069600         16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.  00001966
069700         16  PB-I-BENEFIT-TYPE            PIC XXX.                00001967
069800         16  PB-I-OB-FLAG                 PIC X.                  00001968
069900             88  PB-I-OB                      VALUE 'B'.          00001969
070000             88  PB-I-SUMMARY                 VALUE 'Z'.          00001970
070100         16  PB-I-ENTRY-STATUS            PIC X.                  00001971
070200             88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'   00001972
070300                                              'M' '5' '9' '2'.    00001973
070400             88  PB-I-NORMAL-ENTRY            VALUE '1'.          00001974
070500             88  PB-I-POLICY-PENDING          VALUE '2'.          00001975
070600             88  PB-I-CONVERSION-ENTRY        VALUE '4'.          00001976
070700             88  PB-I-POLICY-IS-REISSUE       VALUE '5'.          00001977
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.          00001977
070800             88  PB-I-REIN-ONLY               VALUE '9'.          00001978
070900             88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.          00001979
071000             88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.          00001980
071100             88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.          00001981
071200             88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.          00001982
071300         16  PB-I-INT-CODE                PIC X.                  00001983
071400             88  PB-ADD-ON-INTEREST           VALUE 'A'.          00001984
071500             88  PB-SIMPLE-INTEREST           VALUE 'S'.          00001985
071600         16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3. 00001986
071700         16  PB-I-SOC-SEC-NO              PIC X(11).              00001987
071800         16  PB-I-MEMBER-NO               PIC X(12).              00001988
071900         16  PB-I-CURR-SEQ                PIC S9(4)       COMP.   00001989
072000         16  PB-I-LOAN-OFFICER            PIC XXX.                00001990
072100         16  PB-I-LF-EXPIRE-DT            PIC XX.                 00001991
072200         16  PB-I-AH-EXPIRE-DT            PIC XX.                 00001992
072300         16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3. 00001993
072400         16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3. 00001994
072500         16  PB-I-LIFE-INDICATOR          PIC X.                  00001995
072600             88  PB-I-JOINT-COVERAGE         VALUE 'J'.           00001996
072700         16  PB-I-LIVES                   PIC S9(3)       COMP-3. 00001997
072800         16  PB-I-MAIL-ADDRS-SW           PIC X.                  00001998
072900             88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.           00001999
073000             88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.           00002000
073100         16  PB-I-1ST-PMT-DT              PIC XX.                 00002001
073200         16  PB-I-JOINT-INSURED.                                  00002002
073300             20 PB-I-JOINT-LAST-NAME      PIC X(15).              00002003
073400             20 PB-I-JOINT-FIRST-NAME.                            00002004
073500                24  PB-I-JOINT-FIRST-INIT PIC X.                  00002005
073600                24  FILLER                PIC X(9).               00002006
073700             20 PB-I-JOINT-MIDDLE-INIT    PIC X.                  00002007
073800         16  PB-I-BENEFICIARY-NAME        PIC X(25).              00002008
073900         16  PB-I-LAST-ADD-ON-DT          PIC XX.                 00002009
074000         16  PB-I-REFERENCE               PIC X(12).              00002010
074100         16  PB-I-UNDERWRITING-STATUS     PIC X.                  00002011
074200             88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.      00002012
074300             88  PB-I-POLICY-DECLINED         VALUE 'D'.          00002013
074400             88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.          00002014
074500         16  FILLER                       PIC X(39).              00002015
074600                                                                  00002016
074700     12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.             00002017
074800         16  PB-C-LF-CANCEL-VOID-SW       PIC X.                  00002018
074900             88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.          00002019
075000         16  PB-C-CANCEL-ORIGIN           PIC X.                  00002020
075100             88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.           00002021
075200         16  PB-C-LF-CANCEL-DT            PIC XX.                 00002022
075300         16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3. 00002023
075400         16  PB-C-LF-CALC-REQ             PIC X.                  00002024
075500             88 PB-COMP-LF-CANCEL            VALUE '?'.           00002025
075600         16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3. 00002026
075700         16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3. 00002027
075800         16  PB-C-AH-CANCEL-VOID-SW       PIC X.                  00002028
075900             88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.          00002029
076000         16  PB-C-AH-CANCEL-DT            PIC XX.                 00002030
076100         16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3. 00002031
076200         16  PB-C-AH-CALC-REQ             PIC X.                  00002032
076300             88 PB-COMP-AH-CANCEL            VALUE '?'.           00002033
076400         16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3. 00002034
076500         16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3. 00002035
076600         16  PB-C-LAST-NAME               PIC X(15).              00002036
076700         16  PB-C-REFUND-SW               PIC X.                  00002037
076800             88  PB-C-REFUND-CREATED          VALUE 'Y'.          00002038
076900             88  PB-C-REFUND-REQUESTED        VALUE 'R'.          00002039
077000         16  PB-C-LIVES                   PIC S9(3)       COMP-3. 00002040
077100         16  PB-C-PAYEE-CODE              PIC X(6).               00002041
077200         16  PB-C-LF-REFUND-OVERRIDE      PIC X.                  00002042
077300         16  PB-C-AH-REFUND-OVERRIDE      PIC X.                  00002043
077400         16  PB-C-LF-COMM-CHARGEBACK      PIC X(01).              00002044
077500         16  PB-C-AH-COMM-CHARGEBACK      PIC X(01).              00002045
077600         16  PB-C-REFERENCE               PIC X(12).              00002046
077700         16  FILLER                       PIC X(30).              00002047
077800         16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.  00002048
077900         16  PB-CANCELED-CERT-DATA.                               00002049
078000             20  PB-CI-INSURED-NAME.                              00002050
078100                 24  PB-CI-LAST-NAME      PIC X(15).              00002051
078200                 24  PB-CI-INITIALS       PIC XX.                 00002052
078300             20  PB-CI-INSURED-AGE        PIC S99         COMP-3. 00002053
078400             20  PB-CI-INSURED-SEX        PIC X.                  00002054
078500             20  PB-CI-LF-TERM            PIC S999        COMP-3. 00002055
078600             20  PB-CI-LF-BENEFIT-CD      PIC XX.                 00002056
078700             20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3. 00002057
078800             20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3. 00002058
078900             20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3. 00002059
079000             20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3. 00002060
079100             20  PB-CI-AH-TERM            PIC S999        COMP-3. 00002061
079200             20  PB-CI-AH-BENEFIT-CD      PIC XX.                 00002062
079300             20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3. 00002063
079400             20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3. 00002064
079500             20  PB-CI-RATE-CLASS         PIC XX.                 00002065
079600             20  PB-CI-RATE-DEV-LF        PIC XXX.                00002066
079700             20  PB-CI-RATE-DEV-AH        PIC XXX.                00002067
079800             20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3. 00002068
079900             20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3. 00002069
080000             20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3. 00002070
080100             20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3. 00002071
080200             20  PB-CI-LF-ABBR            PIC X(3).               00002072
080300             20  PB-CI-AH-ABBR            PIC X(3).               00002073
080400             20  PB-CI-OB-FLAG            PIC X.                  00002074
080500                 88  PB-CI-OB                VALUE 'B'.           00002075
080600             20  PB-CI-LF-POLICY-STATUS   PIC X.                  00002076
080700                 88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'00002077
122002                                           'M' '4' '5' '9' '2'.   00002078
080900                 88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.   00002079
081000                 88  PB-CI-LF-POLICY-PENDING         VALUE '2'.   00002080
081100                 88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.   00002081
081200                 88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.   00002082
081300                 88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.   00002083
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.   00002083
081400                 88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.   00002084
081500                 88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.   00002085
081600                 88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.   00002086
081700                 88  PB-CI-LF-REIN-ONLY              VALUE '9'.   00002087
081800                 88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.   00002088
081900                 88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.   00002089
082000             20  PB-CI-AH-POLICY-STATUS   PIC X.                  00002090
082100                 88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'00002091
082200                                           'M' '4' '5' '9' '2'.   00002092
082300                 88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.   00002093
082400                 88  PB-CI-AH-POLICY-PENDING         VALUE '2'.   00002094
082500                 88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.   00002095
082600                 88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.   00002096
082700                 88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.   00002097
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.   00002097
082800                 88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.   00002098
082900                 88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.   00002099
083000                 88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.   00002100
083100                 88  PB-CI-AH-REIN-ONLY              VALUE '9'.   00002101
083200                 88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.   00002102
083300                 88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.   00002103
083400             20  PB-CI-PAY-FREQUENCY      PIC 99.                 00002104
083500             20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3. 00002105
083600             20  PB-CI-SOC-SEC-NO         PIC X(11).              00002106
083700             20  PB-CI-MEMBER-NO          PIC X(12).              00002107
083800             20  PB-CI-INT-CODE           PIC X.                  00002108
083900                 88  PB-CI-ADD-ON                  VALUE 'A'.     00002109
084000                 88  PB-CI-SIMPLE                  VALUE 'S'.     00002110
084100             20  PB-CI-LOAN-TERM          PIC S999        COMP-3. 00002111
084200             20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).               00002112
084300             20  PB-CI-COMP-EXCP-SW       PIC X.                  00002113
084400                 88  PB-CI-NO-COMP-EXCP            VALUE ' '.     00002114
084500                 88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.     00002115
084600             20  PB-CI-ENTRY-STATUS       PIC X.                  00002116
084700             20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.   00002117
084800             20  PB-CI-AH-PAID-THRU-DT    PIC XX.                 00002118
084900             20  PB-CI-AH-SETTLEMENT-DT   PIC XX.                 00002119
085000             20  PB-CI-DEATH-DT           PIC XX.                 00002120
085100             20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.                 00002121
085200             20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.                 00002122
085300             20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3. 00002123
085400             20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3. 00002124
085500             20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.               00002125
085600             20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.               00002126
085700             20  PB-CI-ENTRY-DT              PIC XX.              00002127
085800             20  PB-CI-ENTRY-BATCH           PIC X(6).            00002128
085900             20  PB-CI-LF-EXPIRE-DT          PIC XX.              00002129
086000             20  PB-CI-AH-EXPIRE-DT          PIC XX.              00002130
086100             20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3. 00002131
086200             20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3. 00002132
086300             20  PB-CI-LOAN-OFFICER          PIC XXX.             00002133
086400             20  PB-CI-LIVES                 PIC S9(3)    COMP-3. 00002134
086500             20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3. 00002135
086600             20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3. 00002136
086700             20  PB-CI-INDV-GRP-CD           PIC X.               00002137
086800             20  PB-CI-BENEFICIARY-NAME      PIC X(25).           00002138
086900             20  PB-CI-NOTE-SW               PIC X.               00002139
087000         16  FILLER                       PIC X(43).              00002140
087100                                                                  00002141
087200     12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.              00002142
087300         16  FILLER                       PIC X(10).              00002143
087400         16  PB-M-INSURED-LAST-NAME       PIC X(15).              00002144
087500         16  PB-M-INSURED-FIRST-NAME      PIC X(10).              00002145
087600         16  PB-M-INSURED-MID-INIT        PIC X.                  00002146
087700         16  PB-M-INSURED-AGE             PIC 99.                 00002147
087800         16  PB-M-INSURED-BIRTHDAY        PIC XX.                 00002148
087900         16  PB-M-INSURED-SEX             PIC X.                  00002149
088000         16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).              00002150
088100         16  PB-M-INSURED-ADDRESS-1       PIC X(30).              00002151
088200         16  PB-M-INSURED-ADDRESS-2       PIC X(30).              00002152
088300         16  PB-M-INSURED-CITY-STATE      PIC X(30).              00002153
088400         16  PB-M-INSURED-ZIP-CODE.                               00002154
088500             20  PB-M-INSURED-ZIP-PRIME.                          00002155
088600                 24  PB-M-INSURED-ZIP-1   PIC X.                  00002156
088700                     88  PB-M-CANADIAN-POST-CODE                  00002157
088800                                             VALUE 'A' THRU 'Z'.  00002158
088900                 24  FILLER               PIC X(4).               00002159
089000             20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).               00002160
089100         16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES                 00002161
089200                                        PB-M-INSURED-ZIP-CODE.    00002162
089300             20  PM-M-INS-CAN-POST1       PIC XXX.                00002163
089400             20  PM-M-INS-CAN-POST2       PIC XXX.                00002164
089500             20  FILLER                   PIC XXX.                00002165
089600         16  PB-M-INSURED-PHONE-NO        PIC 9(10).              00002166
089700         16  FILLER                       PIC X(194).             00002167
089800                                                                  00002168
089900     12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.              00002169
090000         16  FILLER                       PIC X(10).              00002170
090100         16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.    00002171
090200         16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.    00002172
090300         16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    00002173
090400         16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.    00002174
090500         16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.    00002175
090600         16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    00002176
090700         16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.    00002177
090800         16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.    00002178
090900         16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    00002179
091000         16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.    00002180
091100         16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.    00002181
091200         16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.    00002182
091300         16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.    00002183
091400         16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.    00002184
091500         16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.    00002185
091600         16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.    00002186
091700         16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.      00002187
091800         16  PB-ACCOUNT-NAME              PIC X(30).              00002188
091900         16  PB-PREM-REF-RPT-FLAG         PIC X.                  00002189
092000         16  PB-REFERENCE                 PIC X(12).              00002190
092100         16  PB-B-RECEIVED-DT             PIC XX.                 00002191
092200         16  FILLER                       PIC X(214).             00002192
092300                                                                  00002193
092400     12  PB-RECORD-STATUS.                                        00002194
092500         16  PB-CREDIT-SELECT-DT          PIC XX.                 00002195
092600         16  PB-CREDIT-ACCEPT-DT          PIC XX.                 00002196
092700         16  PB-BILLED-DT                 PIC XX.                 00002197
092800         16  PB-BILLING-STATUS            PIC X.                  00002198
092900             88  PB-ENTRY-REVERSED            VALUE 'R'.          00002199
093000             88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.          00002200
093100             88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.          00002201
093200         16  PB-RECORD-BILL               PIC X.                  00002202
093300             88  PB-RECORD-ON-HOLD            VALUE 'H'.          00002203
093400             88  PB-RECORD-RETURNED           VALUE 'R'.          00002204
093500             88  PB-RECORD-ENDORSED           VALUE 'E'.          00002205
093600             88  PB-OVERRIDE-LIFE             VALUE 'L'.          00002206
093700             88  PB-OVERRIDE-AH               VALUE 'A'.          00002207
093800             88  PB-OVERRIDE-BOTH             VALUE 'B'.          00002208
093900         16  PB-BATCH-ENTRY               PIC X.                  00002209
094000             88  PB-POLICY-IS-DECLINED        VALUE 'D'.          00002210
094100             88  PB-REIN-ONLY-CERT            VALUE 'R'.          00002211
094200             88  PB-REISSUED-CERT             VALUE 'E'.          00002212
122002             88  PB-MONTHLY-CERT              VALUE 'M'.          00002212
094300             88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.          00002213
094400             88  PB-NEEDS-UNDERWRITING        VALUE 'U'.          00002214
094500             88  PB-POLICY-IS-VOIDED          VALUE 'V'.          00002215
094600         16  PB-FORCE-CODE                PIC X.                  00002216
094700             88  PB-FORCE-OFF                 VALUE ' ' '0'.      00002217
094800             88  PB-ISSUE-FORCE               VALUE 'A'.          00002218
094900             88  PB-CANCEL-FORCE              VALUE '8'.          00002219
095000             88  PB-ALL-ISSUE-FORCED          VALUE 'A'.          00002220
095100             88  PB-ALL-CANCEL-FORCED         VALUE '8'.          00002221
095200             88  PB-CANCEL-DATE-FORCED        VALUE 'D'.          00002222
095300             88  PB-ISSUE-DATE-FORCED         VALUE 'D'.          00002223
095400         16  PB-FATAL-FLAG                PIC X.                  00002224
095500             88  PB-FATAL-ERRORS              VALUE 'X'.          00002225
095600         16  PB-FORCE-ER-CD               PIC X.                  00002226
095700             88  PB-FORCE-ERRORS              VALUE 'F'.          00002227
095800             88  PB-UNFORCED-ERRORS           VALUE 'X'.          00002228
095900         16  PB-WARN-ER-CD                PIC X.                  00002229
096000             88  PB-WARNING-ERRORS            VALUE 'W'.          00002230
096100         16  FILLER                       PIC X.                  00002231
096200         16  PB-OUT-BAL-CD                PIC X.                  00002232
096300             88  PB-OUT-OF-BAL                VALUE 'O'.          00002233
096400         16  PB-LIFE-OVERRIDE-L1          PIC X.                  00002234
096500         16  PB-AH-OVERRIDE-L1            PIC X.                  00002235
096600         16  PB-INPUT-DT                  PIC XX.                 00002236
096700         16  PB-INPUT-BY                  PIC X(4).               00002237
096800         16  PB-CHG-COUNT                 PIC 9(3)        COMP-3. 00002238
096900         16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3. 00002239
097000         16  PB-TOLERANCE-REJECT-SW       PIC X.                  00002240
097100         16  PB-LF-EARNING-METHOD         PIC X.                  00002241
097200         16  PB-AH-EARNING-METHOD         PIC X.                  00002242
097300         16  PB-LF-TERM-CALC-METHOD       PIC X.                  00002243
097400         16  PB-AH-TERM-CALC-METHOD       PIC X.                  00002244
097500         16  PB-REIN-CD                   PIC XXX.                00002245
097600         16  PB-LF-REFUND-TYPE            PIC X.                  00002246
097700         16  PB-AH-REFUND-TYPE            PIC X.                  00002247
097800         16  PB-ACCT-EFF-DT               PIC XX.                 00002248
097900         16  PB-ACCT-EXP-DT               PIC XX.                 00002249
098000         16  PB-COMPANY-ID                PIC X(3).               00002250
098100         16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.   00002251
098200         16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.   00002252
098300         16  PB-SV-CARRIER                PIC X.                  00002253
098400         16  PB-SV-GROUPING               PIC X(6).               00002254
098500         16  PB-SV-STATE                  PIC XX.                 00002255
098600         16  PB-CONFIRMATION-REPT-DT      PIC XX.                 00002256
098700         16  PB-GA-BILLING-INFO.                                  00002257
098800             20  PB-GA-BILL-DT OCCURS 5 TIMES                     00002258
098900                                          PIC XX.                 00002259
099000         16  PB-SV-REMIT-TO  REDEFINES                            00002260
099100             PB-GA-BILLING-INFO           PIC X(10).              00002261
099200         16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.       00002262
099300                                                                  00002263
LGC128*        16  FILLER                       PIC X(27).              00002264
LGC128         16  FILLER                       PIC X(26).              00002265
LGC128         16  IMNET-BYPASS-SW              PIC X(01).              00002266
099500                                                                  00002267
099600******************************************************************00002268
099700*                COMMON EDIT ERRORS                              *00002269
099800******************************************************************00002270
099900                                                                  00002271
100000     12  PB-COMMON-ERRORS.                                        00002272
100100         16  PB-COMMON-ERROR    OCCURS 10 TIMES                   00002273
100200                                           PIC S9(4)     COMP.    00002274
100300                                                                  00002275
100400******************************************************************00002276
100500 01  CRTC-RECORD.                                                 00002277
100600     12  CC-RECORD-ID                     PIC XX.                 00002278
100700         88  VALID-CC-ID                        VALUE 'CC'.       00002279
100800                                                                  00002280
100900     12  CC-CONTROL-PRIMARY.                                      00002281
101000         16  CC-COMPANY-CD                PIC X.                  00002282
101100         16  CC-CARRIER                   PIC X.                  00002283
101200         16  CC-GROUPING                  PIC X(6).               00002284
101300         16  CC-STATE                     PIC XX.                 00002285
101400         16  CC-ACCOUNT                   PIC X(10).              00002286
101500         16  CC-CERT-EFF-DT               PIC XX.                 00002287
101600         16  CC-CERT-NO.                                          00002288
101700             20  CC-CERT-PRIME            PIC X(10).              00002289
101800             20  CC-CERT-SFX              PIC X.                  00002290
101900                                                                  00002291
102000         16  CC-FILE-SEQ-NO               PIC S9(8)     COMP.     00002292
102100                                                                  00002293
102200     12  CC-LAST-MAINT-DT                 PIC XX.                 00002294
102300     12  CC-LAST-MAINT-BY                 PIC X(4).               00002295
102400     12  CC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.   00002296
102500                                                                  00002297
102600     12  CC-RECORD-BODY                   PIC X(191).             00002298
102700                                                                  00002299
102800     12  CC-CHANGES-RECORD   REDEFINES CC-RECORD-BODY.            00002300
102900         16  FILLER                       PIC X(10).              00002301
103000         16  CC-NAME.                                             00002302
103100             20  CC-INSURED-LAST-NAME     PIC X(15).              00002303
103200             20  CC-INSURED-FIRST-NAME.                           00002304
103300                 24  CC-INSURED-1ST-INIT  PIC X.                  00002305
103400                 24  FILLER               PIC X(9).               00002306
103500             20  CC-INSURED-INITIAL2      PIC XX.                 00002307
103600         16  CC-AGE                       PIC S99.                00002308
103700         16  CC-INSURED-JOINT-AGE         PIC S99.                00002309
103800         16  CC-BIRTHDAY                  PIC XX.                 00002310
103900         16  CC-INSURED-SEX               PIC X.                  00002311
104000             88  CC-SEX-MALE                 VALUE 'M'.           00002312
104100             88  CC-SEX-FEMALE               VALUE 'F'.           00002313
104200         16  CC-JOINT-INSURED-NAME.                               00002314
104300             20  CC-JT-LAST-NAME          PIC X(15).              00002315
104400             20  CC-JT-FIRST-NAME.                                00002316
104500                 24  CC-JT-1ST-INIT       PIC X.                  00002317
104600                 24  FILLER               PIC X(9).               00002318
104700             20  CC-JT-INITIAL            PIC X.                  00002319
104800         16  CC-LOAN-APR                  PIC 9(3)V9(4).          00002320
104900         16  CC-PAY-FREQUENCY             PIC S99.                00002321
105000         16  CC-BENEFICIARY               PIC X(25).              00002322
105100         16  CC-POLICY-FORM-NO            PIC X(12).              00002323
105200         16  CC-PREMIUM-TYPE              PIC X.                  00002324
105300         16  CC-IND-GRP-TYPE              PIC X.                  00002325
105400         16  CC-LOAN-NUMBER               PIC X(8).               00002326
105500         16  CC-LOAN-BALANCE              PIC S9(7)V99  COMP-3.   00002327
105600         16  CC-LOAN-OFFICER              PIC XXX.                00002328
105700         16  CC-USER-FIELD                PIC X.                  00002329
105800         16  CC-SOC-SEC-NO                PIC X(11).              00002330
105900         16  CC-MEMBER-NO                 PIC X(12).              00002331
106000         16  CC-LF-ORIG-TERM              PIC S9(3).              00002332
106100         16  CC-AH-ORIG-TERM              PIC S9(3).              00002333
106200         16  CC-LIVES                     PIC S9(3).              00002334
106300         16  CC-CLAIM-DEDUCT-WITHHELD     PIC S9(5)V99  COMP-3.   00002335
106400         16  CC-CANCEL-DEDUCT-WITHHELD    PIC S9(5)V99  COMP-3.   00002336
106500         16  FILLER                       PIC X(16).              00002337
106600     12  CC-RECORD-STATUS.                                        00002338
106700         16  CC-CREDIT-SELECT-DT          PIC XX.                 00002339
106800         16  CC-CREDIT-ACCEPT-DT          PIC XX.                 00002340
106900         16  FILLER                       PIC X(15).              00002341
107000         16  CC-INPUT-DT                  PIC XX.                 00002342
107100     12  CC-ERROR-FLAGS.                                          00002343
107200         16  CC-STANDARD-ERRORS.                                  00002344
107300             20  CC-ERROR-FLAGS   OCCURS 10 TIMES PIC X.          00002345
107400         16  CC-TRANSACTION-ERRORS.                               00002346
107500             20  CC-ERROR-FLAGS   OCCURS 10 TIMES PIC X.          00002347
107600     12  FILLER                               PIC X(19).          00002348
107700******************************************************************00002349
107900**09/16/98 - COMMENTED OUT 01 LEVEL FOR DATE-CONVERSION-DATA.     00002351
CIDVAO*            THE 01 LEVEL IS NOW DEFINED IN COPYBOOK ELCDATE.
CIDVAO*
CIDVAO                                 COPY ELCDATE.

110303                                 COPY ELCDTECX.

110303                                 COPY ELCDTEVR.
108200******************************************************************00002354
108300     EJECT                                                        00002355
108400                                                                  00002356
108500******************************************************************00002357
108600                                                                  00002358
108700 PROCEDURE DIVISION.                                              00002359
108800 0000-MAINLINE SECTION.                                           00002360
108900                                                                  00002361
110303                             COPY ELCDTERX.                       
109700                                                                  00002369
109800     OPEN  INPUT  DATE-KARD                                       00002370
109900          OUTPUT  DISPLAY-PRT                                     00002371
110000                  ERROR-PRT.                                      00002372

CIDVAO**9/16/98: COBOL II DATE CHANGE.
CIDVAO**
CIDVAO     ACCEPT  DATE-IN          FROM  DATE.
CIDVAO     MOVE    DATE-MM-IN       TO    WS-CURR-MO
CIDVAO                                    WS-CYC-MO.
CIDVAO     MOVE    DATE-DD-IN       TO    WS-CURR-DA
CIDVAO                                    WS-CYC-DA.
CIDVAO     MOVE    DATE-YY-IN       TO    WS-CURR-YR
CIDVAO                                    WS-CYC-YR.
CIDVAO****
TSTMOD     DISPLAY 'WS-CURR-DATE = '  WS-CURR-DATE.                     00002374
TSTMOD     DISPLAY 'WS-CYC-DATE  = '  WS-CYC-DATE.                      00002375
110200**** MOVE CURRENT-DATE  TO  WS-CURR-DATE.                         00002374
110300**** MOVE CURRENT-DATE  TO  WS-CYC-DATE.                          00002375
110400     MOVE WS-CURR-DATE  TO  DIS-DATE.                             00002376
110500                                                                  00002377
011304     IF DTE-CLIENT = 'DCC' 
011304         MOVE WS-DCC-ERR-RPT-TITLE TO ERR-HD-1-TITLE
011304         MOVE WS-DCC-RPT-TITLE     TO DISPLAY-HD-1-TITLE
011304     END-IF.

110600     MOVE  ZEROS  TO  DIS-LINE-CNT.                               00002378
110700     PERFORM 8600-DISPLAY-PRT THRU                                00002379
110800          8600-DISPLAY-EXIT.                                      00002380
110900                                                                  00002381
111000     MOVE  ZEROS  TO  ERR-LINE-CNT.                               00002382
111100     PERFORM 8600-ERR-PRT THRU                                    00002383
111200          8600-ERR-EXIT.                                          00002384
111300                                                                  00002385
111400                                                                  00002386
111500     READ  DATE-KARD  INTO  OVERRIDE-DATE-IN                      00002387
111600        AT END                                                    00002388
111700           CLOSE DATE-KARD.                                       00002389
111800                                                                  00002390
111900     DISPLAY 'OVERRIDE-DATE-IN = ' OVERRIDE-DATE-IN.              00002391
112000     DISPLAY ' '.                                                 00002392
112100     PERFORM 0500-START  THRU 0590-EXIT.                          00002393
112200                                                                  00002394
112300     MOVE    '**************************************************' 00002395
112400        TO  DIS-LINE-REC.                                         00002396
112500     PERFORM 8600-DISPLAY-PRT THRU                                00002397
112600          8600-DISPLAY-EXIT.                                      00002398
112700                                                                  00002399
112800     MOVE    '    '                                               00002400
112900        TO  DIS-LINE-REC.                                         00002401
113000     PERFORM 8600-DISPLAY-PRT THRU                                00002402
113100          8600-DISPLAY-EXIT.                                      00002403
113200                                                                  00002404
113300     MOVE    '      REQUESTED DATE IS = '                         00002405
113400        TO  DIS-FLD-1.                                            00002406
113500     MOVE    WS-CURR-DATE                                         00002407
113600        TO  DIS-FLD-2.                                            00002408
113700     PERFORM 8600-DISPLAY-PRT THRU                                00002409
113800          8600-DISPLAY-EXIT.                                      00002410
113900                                                                  00002411
114000     MOVE   SPACES                                                00002412
114100        TO  DIS-LINE-REC.                                         00002413
114200     PERFORM 8600-DISPLAY-PRT THRU                                00002414
114300          8600-DISPLAY-EXIT.                                      00002415
114400                                                                  00002416
114500     MOVE    '**************************************************' 00002417
114600        TO  DIS-LINE-REC.                                         00002418
114700     PERFORM 8600-DISPLAY-PRT THRU                                00002419
114800          8600-DISPLAY-EXIT.                                      00002420
114900                                                                  00002421
115000     MOVE    SPACES                                               00002422
115100        TO  DIS-LINE-REC.                                         00002423
115200     PERFORM 8600-DISPLAY-PRT THRU                                00002424
115300          8600-DISPLAY-EXIT.                                      00002425
115400                                                                  00002426
115500                                                                  00002427
115600     OPEN INPUT   CLAIM-FILE                                      00002428
115700          OUTPUT  HDR-FILE.                                       00002429
115800                                                                  00002430
115900     MOVE    '**************************************************' 00002431
116000        TO  DIS-LINE-REC.                                         00002432
116100     PERFORM 8600-DISPLAY-PRT THRU                                00002433
116200          8600-DISPLAY-EXIT.                                      00002434
116300                                                                  00002435
116400     MOVE    '    '                                               00002436
116500        TO  DIS-LINE-REC.                                         00002437
116600     PERFORM 8600-DISPLAY-PRT THRU                                00002438
116700          8600-DISPLAY-EXIT.                                      00002439
116800                                                                  00002440
116900     MOVE    '  HDR FILE OPENED '                                 00002441
117000        TO  DIS-LINE-REC.                                         00002442
117100     PERFORM 8600-DISPLAY-PRT THRU                                00002443
117200          8600-DISPLAY-EXIT.                                      00002444
117300                                                                  00002445
117400     MOVE    '    '                                               00002446
117500        TO  DIS-LINE-REC.                                         00002447
117600     PERFORM 8600-DISPLAY-PRT THRU                                00002448
117700          8600-DISPLAY-EXIT.                                      00002449
117800                                                                  00002450
117900     MOVE    'CLAIM FILE OPENED '                                 00002451
118000        TO  DIS-LINE-REC.                                         00002452
118100     PERFORM 8600-DISPLAY-PRT THRU                                00002453
118200          8600-DISPLAY-EXIT.                                      00002454
118300                                                                  00002455
118400     MOVE    '    '                                               00002456
118500        TO  DIS-LINE-REC.                                         00002457
118600     PERFORM 8600-DISPLAY-PRT THRU                                00002458
118700          8600-DISPLAY-EXIT.                                      00002459
118800                                                                  00002460
118900     MOVE    '**************************************************' 00002461
119000        TO  DIS-LINE-REC.                                         00002462
119100     PERFORM 8600-DISPLAY-PRT THRU                                00002463
119200          8600-DISPLAY-EXIT.                                      00002464
119300                                                                  00002465
119400     MOVE    '    '                                               00002466
119500        TO  DIS-LINE-REC.                                         00002467
119600     PERFORM 8600-DISPLAY-PRT THRU                                00002468
119700          8600-DISPLAY-EXIT.                                      00002469
119800                                                                  00002470
119900                                                                  00002471
120000     PERFORM 1000-PROCESS-CLAIM THRU 1900-EXIT.                   00002472
120100                                                                  00002473
120200     CLOSE        CLAIM-FILE.                                     00002474
120300                                                                  00002475
120400                                                                  00002476
120500     MOVE    ' CRTC FILE OPENED '                                 00002477
120600        TO  DIS-LINE-REC.                                         00002478
120700     PERFORM 8600-DISPLAY-PRT THRU                                00002479
120800          8600-DISPLAY-EXIT.                                      00002480
120900                                                                  00002481
121000     MOVE    '    '                                               00002482
121100        TO  DIS-LINE-REC.                                         00002483
121200     PERFORM 8600-DISPLAY-PRT THRU                                00002484
121300          8600-DISPLAY-EXIT.                                      00002485
121400                                                                  00002486
121500     MOVE    '**************************************************' 00002487
121600        TO  DIS-LINE-REC.                                         00002488
121700     PERFORM 8600-DISPLAY-PRT THRU                                00002489
121800          8600-DISPLAY-EXIT.                                      00002490
121900                                                                  00002491
122000     MOVE    '    '                                               00002492
122100        TO  DIS-LINE-REC.                                         00002493
122200     PERFORM 8600-DISPLAY-PRT THRU                                00002494
122300          8600-DISPLAY-EXIT.                                      00002495
122400                                                                  00002496
122500                                                                  00002497
122600     OPEN INPUT   CRTC-FILE.                                      00002498
122700                                                                  00002499
122800     PERFORM 3000-PROCESS-CRTC  THRU 3900-EXIT.                   00002500
122900                                                                  00002501
123000     CLOSE        CRTC-FILE.                                      00002502
123100                                                                  00002503
123200     MOVE    '**************************************************' 00002504
123300        TO  DIS-LINE-REC.                                         00002505
123400     PERFORM 8600-DISPLAY-PRT THRU                                00002506
123500          8600-DISPLAY-EXIT.                                      00002507
123600                                                                  00002508
123700     MOVE    '    '                                               00002509
123800        TO  DIS-LINE-REC.                                         00002510
123900     PERFORM 8600-DISPLAY-PRT THRU                                00002511
124000          8600-DISPLAY-EXIT.                                      00002512
124100                                                                  00002513
124200     MOVE    ' CRTC FILE CLOSED '                                 00002514
124300        TO  DIS-LINE-REC.                                         00002515
124400     PERFORM 8600-DISPLAY-PRT THRU                                00002516
124500          8600-DISPLAY-EXIT.                                      00002517
124600                                                                  00002518
124700     MOVE    '    '                                               00002519
124800        TO  DIS-LINE-REC.                                         00002520
124900     PERFORM 8600-DISPLAY-PRT THRU                                00002521
125000          8600-DISPLAY-EXIT.                                      00002522
125100                                                                  00002523
125200     MOVE    ' PNDB FILE OPENED '                                 00002524
125300        TO  DIS-LINE-REC.                                         00002525
125400     PERFORM 8600-DISPLAY-PRT THRU                                00002526
125500          8600-DISPLAY-EXIT.                                      00002527
125600                                                                  00002528
125700     MOVE    '    '                                               00002529
125800        TO  DIS-LINE-REC.                                         00002530
125900     PERFORM 8600-DISPLAY-PRT THRU                                00002531
126000          8600-DISPLAY-EXIT.                                      00002532
126100                                                                  00002533
126200     MOVE    '**************************************************' 00002534
126300        TO  DIS-LINE-REC.                                         00002535
126400     PERFORM 8600-DISPLAY-PRT THRU                                00002536
126500          8600-DISPLAY-EXIT.                                      00002537
126600                                                                  00002538
126700     MOVE    '    '                                               00002539
126800        TO  DIS-LINE-REC.                                         00002540
126900     PERFORM 8600-DISPLAY-PRT THRU                                00002541
127000          8600-DISPLAY-EXIT.                                      00002542
127100                                                                  00002543
127200                                                                  00002544
127300     OPEN INPUT   PNDB-FILE.                                      00002545
127400                                                                  00002546
127500     PERFORM 4000-PROCESS-PNDB  THRU 4900-EXIT.                   00002547
127600                                                                  00002548
127700     CLOSE        PNDB-FILE.                                      00002549
127800     CLOSE        HDR-FILE.                                       00002550
127900                                                                  00002551
128000     MOVE    '**************************************************' 00002552
128100        TO  DIS-LINE-REC.                                         00002553
128200     PERFORM 8600-DISPLAY-PRT THRU                                00002554
128300          8600-DISPLAY-EXIT.                                      00002555
128400                                                                  00002556
128500     MOVE    '    '                                               00002557
128600        TO  DIS-LINE-REC.                                         00002558
128700     PERFORM 8600-DISPLAY-PRT THRU                                00002559
128800          8600-DISPLAY-EXIT.                                      00002560
128900                                                                  00002561
129000     MOVE    '   PNDB FILE CLOSED '                               00002562
129100        TO  DIS-LINE-REC.                                         00002563
129200     PERFORM 8600-DISPLAY-PRT THRU                                00002564
129300          8600-DISPLAY-EXIT.                                      00002565
129400                                                                  00002566
129500     MOVE    '    '                                               00002567
129600        TO  DIS-LINE-REC.                                         00002568
129700     PERFORM 8600-DISPLAY-PRT THRU                                00002569
129800          8600-DISPLAY-EXIT.                                      00002570
129900                                                                  00002571
130000     MOVE    '    HDR FILE CLOSED '                               00002572
130100        TO  DIS-LINE-REC.                                         00002573
130200     PERFORM 8600-DISPLAY-PRT THRU                                00002574
130300          8600-DISPLAY-EXIT.                                      00002575
130400                                                                  00002576
130500     MOVE    '    '                                               00002577
130600        TO  DIS-LINE-REC.                                         00002578
130700     PERFORM 8600-DISPLAY-PRT THRU                                00002579
130800          8600-DISPLAY-EXIT.                                      00002580
130900                                                                  00002581
131000     MOVE    'DISPLAY FILE CLOSED '                               00002582
131100        TO  DIS-LINE-REC.                                         00002583
131200     PERFORM 8600-DISPLAY-PRT THRU                                00002584
131300          8600-DISPLAY-EXIT.                                      00002585
131400                                                                  00002586
131500     MOVE    '    '                                               00002587
131600        TO  DIS-LINE-REC.                                         00002588
131700     PERFORM 8600-DISPLAY-PRT THRU                                00002589
131800          8600-DISPLAY-EXIT.                                      00002590
131900                                                                  00002591
132000     MOVE    '**************************************************' 00002592
132100        TO  DIS-LINE-REC.                                         00002593
132200     PERFORM 8600-DISPLAY-PRT THRU                                00002594
132300          8600-DISPLAY-EXIT.                                      00002595
132400                                                                  00002596
132500     MOVE    '    '                                               00002597
132600        TO  DIS-LINE-REC.                                         00002598
132700     PERFORM 8600-DISPLAY-PRT THRU                                00002599
132800          8600-DISPLAY-EXIT.                                      00002600
132900                                                                  00002601
133000                                                                  00002602
133100                                                                  00002603
133200     PERFORM 6000-FINISH-PROC THRU 6900-EXIT.                     00002604
133300                                                                  00002605
133400     CLOSE        ERROR-PRT                                       00002606
133500                  DISPLAY-PRT.                                    00002607
133600                                                                  00002608
133700     DISPLAY 'END OF PROGRAM:  CIIMNET'.                          00002609
133800                                                                  00002610
133900     GOBACK.                                                      00002611
134000                                                                  00002612
134100******************************************************************00002613
134200                                                                  00002614
134300 0500-START.                                                      00002615
134400                                                                  00002616
CIDVAO**9/16/98: COBOLE II DATE CHANGE.
CIDVAO**
CIDVAO     ACCEPT  DATE-IN          FROM  DATE.
CIDVAO     MOVE    DATE-MM-IN       TO    WS-CURR-MO.
CIDVAO     MOVE    DATE-DD-IN       TO    WS-CURR-DA.
CIDVAO     MOVE    DATE-YY-IN       TO    WS-CURR-YR.
CIDVAO**
134500**   MOVE CURRENT-DATE  TO  WS-CURR-DATE.                         00002617
134600     MOVE WS-CURR-DATE  TO  DIS-DATE.                             00002618
134700     MOVE P-C-DATE      TO  WS-CYC-DATE.                          00002619
134800                                                                  00002620
134900     IF   OVERRIDE-MO  >  '00'                                    00002621
135000        MOVE OVERRIDE-DATE TO  WS-CURR-DATE                       00002622
135100                               DC-GREG-DATE-1-EDIT                00002623
135200                               WS-CYC-DATE                        00002624
135300        GO  TO  0500-DATE-SET.                                    00002625
135400                                                                  00002626
135500     MOVE WS-CYC-DATE   TO                                        00002627
135600                            DC-GREG-DATE-1-EDIT.                  00002628
135700                                                                  00002629
135800 0500-DATE-SET.                                                   00002630
135900                                                                  00002631
136000     DISPLAY '0500-CURR-DATE   = ' WS-CURR-DATE.                  00002632
136100     DISPLAY ' '.                                                 00002633
136200                                                                  00002634
136300     DISPLAY '0500-CYCLE-DATE  = ' WS-CYC-DATE.                   00002635
136400     DISPLAY ' '.                                                 00002636
136500                                                                  00002637
136600     MOVE '2'           TO  DC-OPTION-CODE.                       00002638
136700     PERFORM 8500-DATE-CONVERSION.                                00002639
136800                                                                  00002640
136900     MOVE  DC-BIN-DATE-1  TO  SAVE-BIN-DATE.                      00002641
137000                                                                  00002642
137100* N O T E:                    M                                   00002643
137200*   SAVE-BIN-DATE IS THE DATE USED TO SELECT DAILY CHANGES.       00002644
137300                                                                  00002645
137400     MOVE  'N'  TO  ERR-FLAG.                                     00002646
137500                                                                  00002647
137600 0590-EXIT.                                                       00002648
137700     EXIT.                                                        00002649
137800                                                                  00002650
137900                                                                  00002651
138000******************************************************************00002652
138100                                                                  00002653
138200 1000-PROCESS-CLAIM.                                              00002654
138300*1010-READ-CLAIM-FILE-NEXT.                                       00002655
138400                                                                  00002656
138500*    DISPLAY '1010-READ-CLAIM-FILE-NEXT ENTERED'.                 00002657
138600                                                                  00002658
138700     READ CLAIM-FILE  INTO  CLAIM-RECORD                          00002659
138800       AT  END                                                    00002660
138900          GO  TO  1900-EXIT.                                      00002661
139000                                                                  00002662
110303     IF CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303        CONTINUE
110303     ELSE
110303        GO TO 1000-PROCESS-CLAIM
110303     END-IF
139100     ADD  1   TO  CLAIM-CNTR.                                     00002663
139200                                                                  00002664
139300*    IF CL-LAST-MAINT-DT   <  SAVE-BIN-DATE                       00002665
139600*       GO TO 1000-PROCESS-CLAIM.                                 00002666
139700                                                                  00002667
           IF CL-CLAIM-NO (1:2) NOT = 'SA'
              GO TO 1000-PROCESS-CLAIM
           END-IF
           .

139800 1020-PROCESS-CLAIM-FILE.                                         00002668
139900                                                                  00002669
140000     MOVE CL-LAST-MAINT-DT TO                                     00002670
140100                            DC-BIN-DATE-1.                        00002671
140200                                                                  00002672
140300     MOVE ' '           TO  DC-OPTION-CODE.                       00002673
140400     PERFORM 8500-DATE-CONVERSION.                                00002674
140500                                                                  00002675
140600     MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002676
140800                                                                  00002678
140900     IF  WS-ISS-YR LESS THAN '20'                                 00002679
141000         MOVE '20' TO WORK-CENT                                   00002680
141100       ELSE                                                       00002681
141200         MOVE '19' TO WORK-CENT.                                  00002682
141300                                                                  00002683
141400     MOVE WS-ISS-YR TO WORK-YR.                                   00002684
141500                                                                  00002685
141600     MOVE  WORK-DATE  TO IMNET-YR.                                00002686
141700                                                                  00002687
141800     MOVE WS-ISS-MO  TO IMNET-MO.                                 00002688
141900                                                                  00002689
142000     MOVE WS-ISS-DA  TO IMNET-DA.                                 00002690
142100                                                                  00002691
142200     MOVE CL-CERT-STATE TO H-ISS-STATE.                           00002692
           MOVE CL-CERT-ACCOUNT        TO H-ACCOUNT
142300                                                                  00002693
142400     MOVE   CL-CLAIM-NO  TO  CL-LAST-7.                           00002694
142500     MOVE   ZEROS        TO  CL-FIRST-3.                          00002695
142600     MOVE   ZEROS        TO  CL-LAST-2.                           00002696
142700     MOVE   CL-NUMBER    TO  H-POLICY-NUM.                        00002697
142800                                                                  00002698
142900     MOVE   CL-CLAIM-STATUS  TO  H-STATUS.                        00002699
143000                                                                  00002700
143100     MOVE CL-INCURRED-DT TO  DC-BIN-DATE-1.                       00002701
143200     MOVE ' '            TO  DC-OPTION-CODE.                      00002702
143300     PERFORM 8500-DATE-CONVERSION.                                00002703
143400                                                                  00002704
143500     MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002705
143600                                                                  00002706
143700     IF  WS-ISS-YR LESS THAN '20'                                 00002707
143800         MOVE '20' TO WORK-CENT                                   00002708
143900       ELSE                                                       00002709
144000         MOVE '19' TO WORK-CENT.                                  00002710
144100                                                                  00002711
144200     MOVE WS-ISS-YR TO  WORK-YR.                                  00002712
144300                                                                  00002713
144400     MOVE WORK-DATE TO  H-YR.                                     00002714
144500     MOVE WS-ISS-MO TO  H-MONTH.                                  00002715
144600     MOVE WS-ISS-DA TO  H-DAY.                                    00002716
144700                                                                  00002717
144800     MOVE CL-INSURED-LAST-NAME TO CL-LAST-NAME.                   00002718
144900     MOVE CL-INSURED-1ST-NAME  TO CL-FIRST.                       00002719
145000     MOVE CL-INSURED-MID-INIT  TO CL-INIT.                        00002720
145100                                                                  00002721
145200     MOVE CL-NAME              TO H-INS-NAME.                     00002722
145300                                                                  00002723
145400     PERFORM  8000-FORMAT-NAME THRU 8400-EXIT.                    00002724
145500                                                                  00002725
145600     IF  ERR-FLAG = 'Y'                                           00002726
145700        MOVE  'N'  TO   ERR-FLAG                                  00002727
145800          GO TO 1000-PROCESS-CLAIM.                               00002728
145900                                                                  00002729
146000     MOVE 'A'       TO  H-TRANS-CODE.                             00002730
146100                                                                  00002731
146200     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002732
146300                                                                  00002733
146400     IF  ERR-FLAG = 'Y'                                           00002734
146500        MOVE  'N'  TO   ERR-FLAG                                  00002735
146600          GO TO 1000-PROCESS-CLAIM.                               00002736
146700                                                                  00002737
146800     PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                   00002738
146900     ADD  1   TO  CLAIM-HDR.                                      00002739
147000                                                                  00002740
147100     MOVE 'U'       TO  H-TRANS-CODE.                             00002741
147200                                                                  00002742
147300     PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                   00002743
147400     ADD  1   TO  CLAIM-HDR.                                      00002744
147500                                                                  00002745
147600     MOVE SPACES TO WS-HDR-REC.                                   00002746
147700                                                                  00002747
147800     GO TO 1000-PROCESS-CLAIM.                                    00002748
147900                                                                  00002749
148000 1900-EXIT.                                                       00002750
148100     EXIT.                                                        00002751
148200                                                                  00002752
148300                                                                  00002753
148400******************************************************************00002754
148500                                                                  00002755
148600 3000-PROCESS-CRTC.                                               00002756
148700                                                                  00002757
148800*    DISPLAY '3000-PROCESS-CRTC  ENTERED'.                        00002758
148900                                                                  00002759
149000 3010-READ-CRTC-FILE-NEXT.                                        00002760
149100                                                                  00002761
149200     READ CRTC-FILE  INTO  CRTC-RECORD                            00002762
149300       AT  END                                                    00002763
149400          GO TO 3900-EXIT.                                        00002764
149500                                                                  00002765
110303     IF CC-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303        CONTINUE
110303     ELSE
110303        GO TO 3000-PROCESS-CRTC
110303     END-IF
149600     ADD  1   TO  CRTC-CNTR.                                      00002766
149700                                                                  00002767
149800     IF CC-LAST-MAINT-DT  <  SAVE-BIN-DATE                        00002768
150100        GO TO 3010-READ-CRTC-FILE-NEXT.                           00002769
150200                                                                  00002772
150300 3020-PROCESS-CRTC-FILE.                                          00002773
150400                                                                  00002774
150500     MOVE CC-LAST-MAINT-DT TO                                     00002775
150600                            DC-BIN-DATE-1.                        00002776
150700                                                                  00002777
150800     MOVE ' '           TO  DC-OPTION-CODE.                       00002778
150900     PERFORM 8500-DATE-CONVERSION.                                00002779
151000                                                                  00002780
151100     MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002781
151200                                                                  00002782
151300                                                                  00002783
151400     IF  WS-ISS-YR LESS THAN '20'                                 00002784
151500         MOVE '20' TO WORK-CENT                                   00002785
151600       ELSE                                                       00002786
151700         MOVE '19' TO WORK-CENT.                                  00002787
151800                                                                  00002788
151900     MOVE WS-ISS-YR             TO WORK-YR.                       00002789
152000                                                                  00002790
152100     MOVE  WORK-DATE             TO IMNET-YR.                     00002791
152200                                                                  00002792
152300     MOVE WS-ISS-MO             TO IMNET-MO.                      00002793
152400                                                                  00002794
152500     MOVE WS-ISS-DA             TO IMNET-DA.                      00002795
152600                                                                  00002796
152700     MOVE CC-STATE               TO H-ISS-STATE.                  00002797
           MOVE CC-ACCOUNT             TO H-ACCOUNT
152800                                                                  00002798
152900     MOVE   CC-CERT-PRIME        TO  H-POLICY-NUM.                00002799
153000                                                                  00002800
153100     MOVE  'A' TO  H-STATUS.                                      00002801
153200                                                                  00002802
153300     MOVE CC-CERT-EFF-DT         TO  DC-BIN-DATE-1.               00002803
153400     MOVE ' '                    TO  DC-OPTION-CODE.              00002804
153500     PERFORM 8500-DATE-CONVERSION.                                00002805
153600                                                                  00002806
153700     MOVE DC-GREG-DATE-1-MDY     TO  WS-ISSUE-DATE.               00002807
153800                                                                  00002808
153900     IF  WS-ISS-YR LESS THAN '20'                                 00002809
154000         MOVE '20' TO WORK-CENT                                   00002810
154100       ELSE                                                       00002811
154200         MOVE '19' TO WORK-CENT.                                  00002812
154300                                                                  00002813
154400     MOVE WS-ISS-YR TO  WORK-YR.                                  00002814
154500                                                                  00002815
154600     MOVE WORK-DATE TO  H-YR.                                     00002816
154700     MOVE WS-ISS-MO TO  H-MONTH.                                  00002817
154800     MOVE WS-ISS-DA TO  H-DAY.                                    00002818
154900                                                                  00002819
155000     MOVE SPACES               TO H-INS-NAME.                     00002820
155100     MOVE CC-INSURED-LAST-NAME TO CM-LAST-NAME.                   00002821
155200     MOVE CC-INSURED-1ST-INIT  TO CM-FIRST-INIT.                  00002822
155300     MOVE CC-INSURED-INITIAL2  TO CM-SECOND-INIT.                 00002823
155400                                                                  00002824
155500     MOVE CM-NAME              TO H-INS-NAME.                     00002825
155600                                                                  00002826
155700     PERFORM  8000-FORMAT-NAME THRU 8400-EXIT.                    00002827
155800                                                                  00002828
155900     IF  ERR-FLAG = 'Y'                                           00002829
156000        MOVE  'N'  TO   ERR-FLAG                                  00002830
156100          GO TO 3010-READ-CRTC-FILE-NEXT.                         00002831
156200                                                                  00002832
156300                                                                  00002833
156400      MOVE 'A'       TO  H-TRANS-CODE.                            00002834
156500                                                                  00002835
156600     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002836
156700                                                                  00002837
156800     IF  ERR-FLAG = 'Y'                                           00002838
156900        MOVE  'N'  TO   ERR-FLAG                                  00002839
157000          GO TO 3010-READ-CRTC-FILE-NEXT.                         00002840
157100                                                                  00002841
157200      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002842
157300      ADD  1   TO  CRTC-HDR.                                      00002843
157400                                                                  00002844
157500      MOVE 'U'       TO  H-TRANS-CODE.                            00002845
157600                                                                  00002846
157700      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002847
157800      ADD  1   TO  CRTC-HDR.                                      00002848
157900                                                                  00002849
158000      MOVE SPACES TO WS-HDR-REC.                                  00002850
158100                                                                  00002851
158200      GO TO 3010-READ-CRTC-FILE-NEXT.                             00002852
158300                                                                  00002853
158400 3900-EXIT.                                                       00002854
158500     EXIT.                                                        00002855
158600                                                                  00002856
158700******************************************************************00002857
158800                                                                  00002858
158900 4000-PROCESS-PNDB.                                               00002859
159000                                                                  00002860
159100*    DISPLAY '4000-PROCESS-PNDB  ENTERED'.                        00002861
159200                                                                  00002862
159300 4010-READ-PNDB-FILE-NEXT.                                        00002863
159400                                                                  00002864
159500     READ PNDB-FILE  INTO  CERTIFICATE-RECORD                     00002865
159600       AT END                                                     00002866
159700         GO TO 4900-EXIT.                                         00002867
159800                                                                  00002868
           IF CR-CERT-NO = '1400202274 ' OR '1900070370 '
                        OR '1707000577 ' OR '1707000551 '
                        OR '1707000500 '
              CONTINUE
           ELSE
              GO TO 4000-PROCESS-PNDB
           END-IF
           
110303*    IF PB-COMPANY-CD = DTE-CLASIC-COMPANY-CD
110303*       CONTINUE
110303*    ELSE
110303*       GO TO 4000-PROCESS-PNDB
110303*    END-IF
159900     ADD  1   TO  PNDB-CNTR.                                      00002869
160800                                                                  00002879
160900 4020-PROCESS-PNDB-FILE.                                          00002880
161000                                                                  00002881
           MOVE CR-ENTRY-DATE          TO WS-WORK-DATE
           
161100*    MOVE PB-LAST-MAINT-DT TO                                     00002882
161200*                           DC-BIN-DATE-1.                        00002883
161300*                                                                 00002884
161400*    MOVE ' '           TO  DC-OPTION-CODE.                       00002885
161500*    PERFORM 8500-DATE-CONVERSION.                                00002886
161600*                                                                 00002887
161700*    MOVE DC-GREG-DATE-1-MDY  TO  WS-ISSUE-DATE.                  00002888
161800                                                                  00002889
161900                                                                  00002890
162000*    IF  WS-ISS-YR LESS THAN '20'                                 00002891
162100*        MOVE '20' TO WORK-CENT                                   00002892
162200*      ELSE                                                       00002893
162300*        MOVE '19' TO WORK-CENT.                                  00002894
           MOVE WS-WORK-CCYY           TO WORK-DATE
162400                                                                  00002895
162500*    MOVE WS-ISS-YR             TO WORK-YR.                       00002896
162600                                                                  00002897
162700     MOVE  WORK-DATE             TO IMNET-YR.                     00002898
162800                                                                  00002899
162900     MOVE WS-WORK-MM            TO IMNET-MO.                      00002900
163000                                                                  00002901
163100     MOVE WS-WORK-DD            TO IMNET-DA.                      00002902
163200                                                                  00002903
163300     MOVE CR-STATE               TO H-ISS-STATE.                  00002904
           MOVE CR-ACCOUNT             TO H-ACCOUNT
163400                                                                  00002905
163500     MOVE  CR-CERT        TO  H-POLICY-NUM.                       00002906
164200                                                                  00002914
164300 4025-CONTINUE.                                                   00002915
164400                                                                  00002916
164500     MOVE  'A' TO  H-STATUS.                                      00002917
164600                                                                  00002918
164700*    MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               00002919
164800*    MOVE ' '                    TO  DC-OPTION-CODE.              00002920
164900*    PERFORM 8500-DATE-CONVERSION.                                00002921
165000                                                                  00002922
165100*    MOVE DC-GREG-DATE-1-MDY     TO  WS-ISSUE-DATE.               00002923
165200                                                                  00002924
           MOVE CR-DT                  TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO WORK-DATE
           MOVE WORK-DATE              TO H-YR
           MOVE WS-WORK-MM             TO H-MONTH
           MOVE WS-WORK-DD             TO H-DAY

165300*    IF  WS-ISS-YR LESS THAN '20'                                 00002925
165400*        MOVE '20' TO WORK-CENT                                   00002926
165500*      ELSE                                                       00002927
165600*        MOVE '19' TO WORK-CENT.                                  00002928
165700                                                                  00002929
165800*    MOVE WS-ISS-YR TO  WORK-YR.                                  00002930
165900                                                                  00002931
166000*    MOVE WORK-DATE TO  H-YR.                                     00002932
166100*    MOVE WS-ISS-MO TO  H-MONTH.                                  00002933
166200*    MOVE WS-ISS-DA TO  H-DAY.                                    00002934
166300                                                                  00002935
166500     MOVE SPACES                    TO H-INS-NAME
166600     MOVE CR-LNAME                  TO CM-LAST-NAME
166700     MOVE CR-1ST-INITIAL            TO CM-FIRST-INIT
166800     MOVE CR-INIT                   TO CM-SECOND-INIT
166900     MOVE CM-NAME                   TO H-INS-NAME
167000     PERFORM  8000-FORMAT-NAME THRU 8400-EXIT
168900     .
169000 4030-CONTINUE.                                                   00002962
169100                                                                  00002963
169700      MOVE 'A'       TO  H-TRANS-CODE.                            00002969
169800                                                                  00002970
169900     PERFORM  7000-EDITS  THRU   7900-EXIT.                       00002971
170000                                                                  00002972
170100     IF  ERR-FLAG = 'Y'                                           00002973
170200        MOVE  'N'  TO   ERR-FLAG                                  00002974
170300          GO TO 4010-READ-PNDB-FILE-NEXT.                         00002975
170400                                                                  00002976
170500      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002977
170600      ADD  1   TO  PNDB-HDR.                                      00002978
170700                                                                  00002979
170800      MOVE 'U' TO  H-TRANS-CODE.                                  00002980
170900                                                                  00002981
171000      PERFORM  5000-WRITE-HDR  THRU   5900-EXIT.                  00002982
171100      ADD  1   TO  PNDB-HDR.                                      00002983
171200                                                                  00002984
171300      MOVE SPACES TO WS-HDR-REC.                                  00002985
171400                                                                  00002986
171500      GO TO 4010-READ-PNDB-FILE-NEXT.                             00002987
171600                                                                  00002988
171700 4900-EXIT.                                                       00002989
171800     EXIT.                                                        00002990
171900                                                                  00002991
172000                                                                  00002992
172100******************************************************************00002993
172200                                                                  00002994
172300 5000-WRITE-HDR.                                                  00002995
172400                                                                  00002996
172500     WRITE  HDR-REC  FROM  WS-HDR-REC.                            00002997
172600                                                                  00002998
172700     ADD  1   TO  TOT-HDR.                                        00002999
172800                                                                  00003000
172900                                                                  00003001
173000                                                                  00003002
173100 5900-EXIT.                                                       00003003
173200     EXIT.                                                        00003004
173300                                                                  00003005
173400                                                                  00003006
173500******************************************************************00003007
173600                                                                  00003008
173700 6000-FINISH-PROC.                                                00003009
173800                                                                  00003010
173900     MOVE    SPACES                                               00003011
174000        TO  DIS-LINE-REC.                                         00003012
174100     PERFORM 8600-DISPLAY-PRT THRU                                00003013
174200          8600-DISPLAY-EXIT.                                      00003014
174300                                                                  00003015
174400     MOVE    '     CLAIM FILE RECORDS READ = '                    00003016
174500        TO  DIS-FLD-1.                                            00003017
174600     MOVE         CLAIM-CNTR                                      00003018
174700        TO  DIS-FLD-2.                                            00003019
174800     PERFORM 8600-DISPLAY-PRT THRU                                00003020
174900          8600-DISPLAY-EXIT.                                      00003021
175000                                                                  00003022
175100     MOVE    SPACES                                               00003023
175200        TO  DIS-LINE-REC.                                         00003024
175300     PERFORM 8600-DISPLAY-PRT THRU                                00003025
175400          8600-DISPLAY-EXIT.                                      00003026
175500                                                                  00003027
175600     MOVE    'CLAIM HEADER RECORDS WRITTEN = '                    00003028
175700        TO  DIS-FLD-1.                                            00003029
175800     MOVE    CLAIM-HDR                                            00003030
175900        TO  DIS-FLD-2.                                            00003031
176000     PERFORM 8600-DISPLAY-PRT THRU                                00003032
176100          8600-DISPLAY-EXIT.                                      00003033
176200                                                                  00003034
176300     MOVE    SPACES                                               00003035
176400        TO  DIS-LINE-REC.                                         00003036
176500     PERFORM 8600-DISPLAY-PRT THRU                                00003037
176600          8600-DISPLAY-EXIT.                                      00003038
176700                                                                  00003039
176800     MOVE    '      CRTC FILE RECORDS READ = '                    00003040
176900        TO  DIS-FLD-1.                                            00003041
177000     MOVE    CRTC-CNTR                                            00003042
177100        TO  DIS-FLD-2.                                            00003043
177200     PERFORM 8600-DISPLAY-PRT THRU                                00003044
177300          8600-DISPLAY-EXIT.                                      00003045
177400                                                                  00003046
177500     MOVE    SPACES                                               00003047
177600        TO  DIS-LINE-REC.                                         00003048
177700     PERFORM 8600-DISPLAY-PRT THRU                                00003049
177800          8600-DISPLAY-EXIT.                                      00003050
177900                                                                  00003051
178000     MOVE    ' CRTC HEADER RECORDS WRITTEN = '                    00003052
178100        TO  DIS-FLD-1.                                            00003053
178200     MOVE    CRTC-HDR                                             00003054
178300        TO  DIS-FLD-2.                                            00003055
178400     PERFORM 8600-DISPLAY-PRT THRU                                00003056
178500          8600-DISPLAY-EXIT.                                      00003057
178600                                                                  00003058
178700     MOVE    SPACES                                               00003059
178800        TO  DIS-LINE-REC.                                         00003060
178900     PERFORM 8600-DISPLAY-PRT THRU                                00003061
179000          8600-DISPLAY-EXIT.                                      00003062
179100                                                                  00003063
179200     MOVE    '      PNDB FILE RECORDS READ = '                    00003064
179300        TO  DIS-FLD-1.                                            00003065
179400     MOVE    PNDB-CNTR                                            00003066
179500        TO  DIS-FLD-2.                                            00003067
179600     PERFORM 8600-DISPLAY-PRT THRU                                00003068
179700          8600-DISPLAY-EXIT.                                      00003069
179800                                                                  00003070
179900     MOVE    SPACES                                               00003071
180000        TO  DIS-LINE-REC.                                         00003072
180100     PERFORM 8600-DISPLAY-PRT THRU                                00003073
180200          8600-DISPLAY-EXIT.                                      00003074
180300                                                                  00003075
180400     MOVE    ' PNDB HEADER RECORDS WRITTEN = '                    00003076
180500        TO  DIS-FLD-1.                                            00003077
180600     MOVE    PNDB-HDR                                             00003078
180700        TO  DIS-FLD-2.                                            00003079
180800     PERFORM 8600-DISPLAY-PRT THRU                                00003080
180900          8600-DISPLAY-EXIT.                                      00003081
181000                                                                  00003082
181100     MOVE    SPACES                                               00003083
181200        TO  DIS-LINE-REC.                                         00003084
181300     PERFORM 8600-DISPLAY-PRT THRU                                00003085
181400          8600-DISPLAY-EXIT.                                      00003086
181500                                                                  00003087
181600     MOVE    'TOTAL HEADER RECORDS WRITTEN = '                    00003088
181700        TO  DIS-FLD-1.                                            00003089
181800     MOVE    TOT-HDR                                              00003090
181900        TO  DIS-FLD-2.                                            00003091
182000     PERFORM 8600-DISPLAY-PRT THRU                                00003092
182100          8600-DISPLAY-EXIT.                                      00003093
182200                                                                  00003094
182300     MOVE    SPACES                                               00003095
182400        TO  DIS-LINE-REC.                                         00003096
182500     PERFORM 8600-DISPLAY-PRT THRU                                00003097
182600          8600-DISPLAY-EXIT.                                      00003098
182700                                                                  00003099
182800     MOVE    '      ERROR RECORDS BYPASSED = '                    00003100
182900        TO  DIS-FLD-1.                                            00003101
183000     MOVE    ERR-CNTR                                             00003102
183100        TO  DIS-FLD-2.                                            00003103
183200     PERFORM 8600-DISPLAY-PRT THRU                                00003104
183300          8600-DISPLAY-EXIT.                                      00003105
183400                                                                  00003106
183500     MOVE    SPACES                                               00003107
183600        TO  DIS-LINE-REC.                                         00003108
183700     PERFORM 8600-DISPLAY-PRT THRU                                00003109
183800          8600-DISPLAY-EXIT.                                      00003110
183900                                                                  00003111
184000                                                                  00003112
184100 6900-EXIT.                                                       00003113
184200     EXIT.                                                        00003114
184300                                                                  00003115
184400                                                                  00003116
184500******************************************************************00003117
184600                                                                  00003118
184700 7000-EDITS.                                                      00003119
184800                                                                  00003120
184900     MOVE SPACES TO H-FILLER.                                     00003121
185000                                                                  00003122
185100     MOVE WS-HDR-REC TO EDIT-LINE.                                00003123
185200                                                                  00003124
185300     MOVE ZEROS TO E-SUB.                                         00003125
185400     MOVE SPACES TO ERROR-CODE.                                   00003126
185500                                                                  00003127
185600     ADD  1     TO E-SUB.                                         00003128
185700                                                                  00003129
185800     IF   E-SUB GREATER THAN 80                                   00003130
185900          GO TO 7800-EXIT.                                        00003131
186000                                                                  00003132
186100     IF   EDIT-POS (E-SUB)  =  ' '                                00003133
186200       MOVE 'POS 1 (TRANS CODE) IS BLANK '   TO ERROR-CODE        00003134
186300          GO TO 7100-ERROR.                                       00003135
186400                                                                  00003136
186500 7010-LOOP.                                                       00003137
186600                                                                  00003138
186700     MOVE  H-POLICY-NUM  TO  EDIT-CRT-NUM.                        00003139
186800                                                                  00003140
186900     IF  E-CRT-NUMERIC IS NUMERIC                                 00003141
187000       ADD  11 TO E-SUB                                           00003142
187100         GO TO 7030-PROCESS.                                      00003143
187200                                                                  00003144
187300     MOVE 'CERT NUMBER NOT ALL NUMERIC '   TO ERROR-CODE.         00003145
187400     GO TO 7100-ERROR.                                            00003146
187500                                                                  00003147
187600 7020-LOOP.                                                       00003148
187700                                                                  00003149
187800     ADD  1     TO E-SUB.                                         00003150
187900     IF   E-SUB GREATER THAN 80                                   00003151
188000          GO TO 7800-EXIT.                                        00003152
188100                                                                  00003153
188200 7030-PROCESS.                                                    00003154
188300                                                                  00003155
188400     IF   EDIT-POS (E-SUB)  =  ' '                                00003156
188500          GO TO 7020-LOOP.                                        00003157
188600                                                                  00003158
188700     IF   EDIT-POS (E-SUB)  =  ZERO OR '9'                        00003159
188800          GO TO 7020-LOOP.                                        00003160
CIDVAO****
CIDVAO     IF   EDIT-POS (E-SUB)  >  '0' AND < '9'
CIDVAO          GO TO 7020-LOOP.
CIDVAO****
189000**** IF   EDIT-POS (E-SUB)  ( > ZERO AND < '9')                   00003162
189100****      GO TO 7020-LOOP.                                        00003163
189200                                                                  00003164
189300     IF   EDIT-POS (E-SUB)  =  'A'  OR 'Z'                        00003165
189400          GO TO 7020-LOOP.                                        00003166
CIDVAO****
CIDVAO     IF   EDIT-POS (E-SUB)  >   'A'  AND  < 'Z'
CIDVAO          GO TO 7020-LOOP.
CIDVAO****
189600**** IF   EDIT-POS (E-SUB)  ( > 'A'  AND  < 'Z')                  00003168
189700****      GO TO 7020-LOOP.                                        00003169
189800                                                                  00003170
189900     IF   EDIT-POS (E-SUB)  =  ','  OR '-' OR '.' OR ','          00003171
190000                               OR ''''                            00003172
190100          GO TO 7020-LOOP.                                        00003173
190200                                                                  00003174
190300     MOVE 'INVALID CHARACTER IN NAME FIELD '   TO ERROR-CODE.     00003175
190400                                                                  00003176
190500 7100-ERROR.                                                      00003177
190600                                                                  00003178
190700     MOVE    WS-HDR-REC                                           00003179
190800        TO  ERR-LINE-REC.                                         00003180
190900     PERFORM 8600-ERR-PRT THRU                                    00003181
191000          8600-ERR-EXIT.                                          00003182
191100                                                                  00003183
191200     MOVE    'ERR REC TYPE = '                                    00003184
191300        TO  ERR-FLD-1.                                            00003185
191400     MOVE    PB-RECORD-TYPE                                       00003186
191500        TO  ERR-FLD-2.                                            00003187
191600     PERFORM 8600-ERR-PRT THRU                                    00003188
191700          8600-ERR-EXIT.                                          00003189
191800                                                                  00003190
191900     MOVE    'ERROR CAUSE  = '                                    00003191
192000        TO  ERR-FLD-1.                                            00003192
192100     MOVE    ERROR-CODE                                           00003193
192200        TO  ERR-FLD-2.                                            00003194
192300     PERFORM 8600-ERR-PRT THRU                                    00003195
192400          8600-ERR-EXIT.                                          00003196
192500                                                                  00003197
192600     MOVE    'ERROR - - '                                         00003198
192700        TO  ERR-FLD-1.                                            00003199
192800     MOVE    WS-HDR-REC                                           00003200
192900        TO  ERR-FLD-2.                                            00003201
193000     PERFORM 8600-ERR-PRT THRU                                    00003202
193100          8600-ERR-EXIT.                                          00003203
193200                                                                  00003204
193300     ADD  1  TO ERR-CNTR                                          00003205
193400     MOVE 'Y' TO ERR-FLAG                                         00003206
193500     MOVE   SPACES                                                00003207
193600        TO  ERR-LINE-REC.                                         00003208
193700     PERFORM 8600-ERR-PRT THRU                                    00003209
193800          8600-ERR-EXIT.                                          00003210
193900                                                                  00003211
194000     GO TO 7900-EXIT.                                             00003212
194100                                                                  00003213
194200 7800-EXIT.                                                       00003214
194300                                                                  00003215
194400*    MOVE    WS-HDR-REC                                           00003216
194500*       TO  DIS-LINE-REC.                                         00003217
194600     MOVE   H-11         TO DISPLAY-INFO-11.                      00003218
194700     MOVE   H-OTHERS     TO DISPLAY-INFO-OTHERS.                  00003219
194800     MOVE   DISPLAY-INFO                                          00003220
194900        TO  DIS-LINE-REC.                                         00003221
195000     PERFORM 8600-DISPLAY-PRT THRU                                00003222
195100          8600-DISPLAY-EXIT.                                      00003223
195200                                                                  00003224
195300     MOVE    SPACES                                               00003225
195400        TO  DIS-LINE-REC.                                         00003226
195500     PERFORM 8600-DISPLAY-PRT THRU                                00003227
195600          8600-DISPLAY-EXIT.                                      00003228
195700                                                                  00003229
195800 7900-EXIT.                                                       00003230
195900     EXIT.                                                        00003231
196000                                                                  00003232
196100******************************************************************00003233
196200                                                                  00003234
196300 8000-FORMAT-NAME.                                                00003235
196400                                                                  00003236
196500     MOVE  SPACES      TO  NAME-FLD-IN.                           00003237
196600     MOVE  H-INS-NAME  TO  NAME-FLD-IN.                           00003238
196700     MOVE  ZEROS TO SPACE-SUB                                     00003239
196800                    SUB-IN                                        00003240
196900                    SUB-OUT.                                      00003241
197000                                                                  00003242
197100 8000-L-NAME.                                                     00003243
197200                                                                  00003244
197300     ADD  1  TO  SUB-IN.                                          00003245
197400     IF   SUB-IN GREATER THAN  15                                 00003246
197500          MOVE  15     TO  SUB-IN                                 00003247
197600          ADD  1  TO  SUB-OUT                                     00003248
197700          MOVE ',' TO  FLD-POS-OUT (SUB-OUT)                      00003249
197800          GO  TO  8000-F-NAME.                                    00003250
197900                                                                  00003251
198000     IF   FLD-POS-IN (SUB-IN) = SPACES                            00003252
198100          ADD  1  TO  SPACE-SUB                                   00003253
980729        ELSE                                                      00003253
980729          MOVE ZERO  TO  SPACE-SUB.                               00003253
198200                                                                  00003254
198300     IF   SPACE-SUB GREATER THAN  1                               00003255
198400          MOVE  ZEROS  TO  SPACE-SUB                              00003256
198500          MOVE  15     TO  SUB-IN                                 00003257
198600          MOVE ',' TO  FLD-POS-OUT (SUB-OUT)                      00003258
198700          GO  TO  8000-F-NAME.                                    00003259
198800                                                                  00003260
198900     ADD  1  TO  SUB-OUT.                                         00003261
199000                                                                  00003262
199100     MOVE  FLD-POS-IN (SUB-IN)                                    00003263
199200       TO  FLD-POS-OUT (SUB-OUT).                                 00003264
199300                                                                  00003265
199400     GO  TO  8000-L-NAME.                                         00003266
199500                                                                  00003267
199600 8000-F-NAME.                                                     00003268
199700                                                                  00003269
199800     ADD  1  TO  SUB-IN.                                          00003270
199900     IF   SUB-IN GREATER THAN  35                                 00003271
200000          GO  TO  8000-RESET-HDR-NAME.                            00003272
200100                                                                  00003273
200200     ADD  1  TO  SUB-OUT.                                         00003274
200300                                                                  00003275
200400     MOVE  FLD-POS-IN (SUB-IN)                                    00003276
200500       TO  FLD-POS-OUT (SUB-OUT).                                 00003277
200600                                                                  00003278
200700     GO  TO  8000-F-NAME.                                         00003279
200800                                                                  00003280
200900 8000-RESET-HDR-NAME.                                             00003281
201000                                                                  00003282
201100     IF   SUB-OUT GREATER THAN  35                                00003283
201200          GO  TO  8000-MOVE-HDR-NAME.                             00003284
201300                                                                  00003285
201400     ADD  1  TO  SUB-OUT.                                         00003286
201500                                                                  00003287
201600     MOVE ' '  TO  FLD-POS-OUT (SUB-OUT).                         00003288
201700                                                                  00003289
201800     GO TO  8000-RESET-HDR-NAME.                                  00003290
201900                                                                  00003291
202000 8000-MOVE-HDR-NAME.                                              00003292
202100                                                                  00003293
202200     MOVE  NAME-FLD-OUT                                           00003294
202300          TO  H-INS-NAME.                                         00003295
202400                                                                  00003296
202500     IF  H-INS-NAME =  SPACES OR ','                              00003297
202600         NEXT  SENTENCE                                           00003298
202700       ELSE                                                       00003299
202800         GO  TO  8400-EXIT.                                       00003300
202900                                                                  00003301
203000     MOVE    '* * * * * * * * * * * * * * * * * * * * * *'        00003302
203100        TO  ERR-LINE-REC.                                         00003303
203200     PERFORM 8600-ERR-PRT THRU                                    00003304
203300          8600-ERR-EXIT.                                          00003305
203400                                                                  00003306
203500     MOVE    WS-HDR-REC                                           00003307
203600        TO  ERR-LINE-REC.                                         00003308
203700     PERFORM 8600-ERR-PRT THRU                                    00003309
203800          8600-ERR-EXIT.                                          00003310
203900                                                                  00003311
204000     MOVE    'NO INSURED NAME '                                   00003312
204100        TO  ERR-LINE-REC.                                         00003313
204200     PERFORM 8600-ERR-PRT THRU                                    00003314
204300          8600-ERR-EXIT.                                          00003315
204400                                                                  00003316
204500     MOVE    'ERROR - - '                                         00003317
204600        TO  ERR-FLD-1.                                            00003318
204700     MOVE    WS-HDR-REC                                           00003319
204800        TO  ERR-FLD-2.                                            00003320
204900     PERFORM 8600-ERR-PRT THRU                                    00003321
205000          8600-ERR-EXIT.                                          00003322
205100                                                                  00003323
205200     ADD  1  TO ERR-CNTR.                                         00003324
205300     MOVE 'Y' TO ERR-FLAG.                                        00003325
205400     MOVE   SPACES                                                00003326
205500        TO  ERR-LINE-REC.                                         00003327
205600     PERFORM 8600-ERR-PRT THRU                                    00003328
205700          8600-ERR-EXIT.                                          00003329
205800                                                                  00003330
205900 8400-EXIT.                                                       00003331
206000     EXIT.                                                        00003332
206100                                                                  00003333
206200******************************************************************00003334
206300                                                                  00003335
206400 8500-DATE-CONVERSION.                                            00003336
206500                                                                  00003337
206600     CALL  'ELDATCX'  USING  DATE-CONVERSION-DATA.                00003338
206700                                                                  00003339
206800 8500-EXIT.                                                       00003340
206900     EXIT.                                                        00003341
207000                                                                  00003342
207100******************************************************************00003343
207200                                                                  00003344
207300 8600-DISPLAY-PRT.                                                00003345
207400                                                                  00003346
207500     IF  DIS-HEAD-SW =  'Y'                                       00003347
207600       MOVE 'N' TO  DIS-HEAD-SW                                   00003348
207700         PERFORM 8600-DISPLAY-HD THRU                             00003349
207800             8600-HD-EXIT                                         00003350
207900           GO TO 8600-DISPLAY-EXIT.                               00003351
208000                                                                  00003352
208100     IF  DIS-LINE-CNT GREATER THAN 77                             00003353
208200         PERFORM 8600-DISPLAY-HD THRU                             00003354
208300             8600-HD-EXIT.                                        00003355
208400                                                                  00003356
208500     ADD +1 TO DIS-LINE-CNT.                                      00003357
208600     MOVE   DISPLAY-LINE TO DISPLAY-INFO.                         00003358
208700     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003359
208800     MOVE   SPACES TO DISPLAY-REC.                                00003360
208900     MOVE   SPACES TO DISPLAY-LINE.                               00003361
209000                                                                  00003362
209100                                                                  00003363
209200 8600-DISPLAY-EXIT.                                               00003364
209300     EXIT.                                                        00003365
209400                                                                  00003366
209500 8600-DISPLAY-HD.                                                 00003367
209600                                                                  00003368
209700     MOVE ZEROS TO DIS-LINE-CNT.                                  00003369
209800     MOVE  DISPLAY-HD-1 TO DISPLAY-INFO.                          00003370
209900     WRITE DISPLAY-REC AFTER ADVANCING NEW-PAGE.                  00003371
210000     ADD  +1  TO DIS-LINE-CNT.                                    00003372
210100     MOVE   SPACES TO DISPLAY-REC.                                00003373
210200     ADD +1 TO DIS-LINE-CNT.                                      00003374
210300     MOVE   DISPLAY-HD-2 TO DISPLAY-INFO.                         00003375
210400     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003376
210500     ADD +1 TO DIS-LINE-CNT.                                      00003377
210600     MOVE   SPACES TO DISPLAY-REC.                                00003378
210700     WRITE  DISPLAY-REC AFTER ADVANCING 1 LINE.                   00003379
210800                                                                  00003380
210900 8600-HD-EXIT.                                                    00003381
211000     EXIT.                                                        00003382
211100 EJECT                                                            00003383
211200                                                                  00003384
211300******************************************************************00003385
211400                                                                  00003386
211500 8600-ERR-PRT.                                                    00003387
211600                                                                  00003388
211700     IF  ERR-HEAD-SW =  'Y'                                       00003389
211800       MOVE 'N' TO  ERR-HEAD-SW                                   00003390
211900         PERFORM 8600-ERR-HD THRU                                 00003391
212000             8600-ERR-HD-EXIT                                     00003392
212100           GO TO 8600-ERR-EXIT.                                   00003393
212200                                                                  00003394
212300     IF  ERR-LINE-CNT GREATER THAN 77                             00003395
212400         PERFORM 8600-ERR-HD THRU                                 00003396
212500             8600-ERR-HD-EXIT.                                    00003397
212600                                                                  00003398
212700     ADD +1 TO ERR-LINE-CNT.                                      00003399
212800     MOVE   ERR-LINE  TO ERR-INFO.                                00003400
212900     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003401
213000     MOVE   SPACES TO ERR-REC.                                    00003402
213100     MOVE   SPACES TO ERR-LINE.                                   00003403
213200                                                                  00003404
213300                                                                  00003405
213400 8600-ERR-EXIT.                                                   00003406
213500     EXIT.                                                        00003407
213600                                                                  00003408
213700 8600-ERR-HD.                                                     00003409
213800                                                                  00003410
213900     MOVE ZEROS TO ERR-LINE-CNT.                                  00003411
214000     MOVE  ERR-HD-1 TO ERR-INFO.                                  00003412
214100     WRITE ERR-REC AFTER ADVANCING NEW-PAGE.                      00003413
214200     ADD  +1  TO ERR-LINE-CNT.                                    00003414
214300     MOVE   SPACES TO ERR-REC.                                    00003415
214400     ADD +1 TO ERR-LINE-CNT.                                      00003416
214500     MOVE   DISPLAY-HD-2 TO ERR-INFO.                             00003417
214600     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003418
214700     ADD +1 TO ERR-LINE-CNT.                                      00003419
214800     MOVE   SPACES TO ERR-REC.                                    00003420
214900     WRITE  ERR-REC AFTER ADVANCING 1 LINE.                       00003421
215000                                                                  00003422
215100 8600-ERR-HD-EXIT.                                                00003423
215200      EXIT.                                                       00003424
215300                                                                  00003425
215400 EJECT                                                            00003426
215500                                                                  00003427
215600******************************************************************00003428
215700                                                                  00003429
215800 ABEND-PGM.                                                       00003430
215900                                                                  00003431
216000     DISPLAY '   ABEND MSG IS = ' WS-ABEND-MESSAGE.               00003432
216100     DISPLAY 'ABEND STATUS IS = ' WS-ABEND-FILE-STATUS.           00003433
216200                                                                  00003434
216300     DISPLAY 'PROGRAM PROBLEMS - JOB WILL ABEND VIA "GOBACK"'.    00003435
216400                                                                  00003436
216500     GOBACK.                                                      00003437
216600                                                                  00003438
216700******************************************************************00003439
216800                                                                  00003440
