       IDENTIFICATION DIVISION.
       PROGRAM-ID.  AGEB16.
       AUTHOR.       J. CONDON.

      *REMARKS.
      *    THIS SUB-PROGRAM CALLS AND PASSES PARAMETERS TO THE IBM BAR CODE
      *    ROUTINE (eansrc), WHICH IS CONTAINED IN barcode.c (A COPY OF THE 
      *    ORGINAL CODE)


051707******************************************************************
051707*                   C H A N G E   L O G
051707*
051707* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051707*-----------------------------------------------------------------
051707*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051707* EFFECTIVE    NUMBER
051707*-----------------------------------------------------------------
051707* 051707  CR2006082200001  PEMA  REMOVE FORCED LENGTH OF 28
051707******************************************************************
       ENVIRONMENT DIVISION.


       DATA DIVISION.


       WORKING-STORAGE SECTION.

       copy "ctypes.cpy".

       01  preloadobj   procedure-pointer.
       01  preloaderr   procedure-pointer.

       01  PARM-FLDS.
           05  PARM-BC-TYPE                PIC X(1).
           05  PARM-BC-IN                  PIC X(50).
           05  PARM-BC-OUT                 PIC X(128).
       01 PARM-BC-LEN short.
       01 PARM-BC-RC  short.

       LINKAGE SECTION.
       01  PARM-LIST.
          05  BC-LENGTH                    short.
          05  BC-CODE-IN                   PIC X(28).
          05  BC-CODE-OUT                  PIC X(128).


       PROCEDURE DIVISION USING PARM-LIST.

       set preloadobj to entry "barcode".
       set preloaderr to entry "doesnotexit".

       if preloadobj = preloaderr
           display "unable to load barcode"
           stop run
       end-if.

       MOVE 'C'        TO PARM-BC-TYPE.
       MOVE BC-CODE-IN TO PARM-BC-IN.
051707 MOVE BC-LENGTH  TO PARM-BC-LEN
051707*MOVE +28        TO PARM-BC-LEN.

      **** for testing
      *      MOVE '0011600000000000277700001' TO PARM-BC-IN.

      *DISPLAY 'Calling BarCode'.

       CALL "eansrc" USING
           BY REFERENCE PARM-BC-TYPE,
           BY REFERENCE PARM-BC-IN,
           BY REFERENCE PARM-BC-LEN,
           BY REFERENCE PARM-BC-OUT,
           RETURNING PARM-BC-RC.


       MOVE PARM-BC-LEN TO BC-LENGTH.
       MOVE PARM-BC-OUT TO BC-CODE-OUT.
      *DISPLAY 'RC was: ' PARM-BC-RC.
      *DISPLAY 'Got ' PARM-BC-LEN ' bytes back'.
       GOBACK.
