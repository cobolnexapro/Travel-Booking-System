       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TBSANAREPT.
       AUTHOR.        IBM-I-BATCH-EXPERT.
       DATE-WRITTEN.  2026-06-21.
      *================================================================*
      * TRAVEL BOOKING SYSTEM (TBS) - ANALYTICAL EXECUTIVE REPORT      *
      *================================================================*
      * SECURITY CLASSIFICATION: CONFIDENTIAL / MANAGEMENT DECI-SUPPORT *
      * DESCRIPTION:                                                   *
      * PROCESSES FLIGHT BOOKING METRICS TO COMPUTE REVENUE PER        *
      * AVAILABLE SEAT MILE (RASM), PASSENGER LOAD FACTORS (PLF), AND  *
      * MULTI-LEVEL ROUTE PROFITABILITY REPORTING WITH CONTROL BREAKS. *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * SORT INPUT WORK FILE FOR CONTROL BREAKS
           SELECT FLIGHT-DATA-FILE ASSIGN TO DATABASE-FLTMETRIC.
           SELECT SORT-WORK-FILE   ASSIGN TO PRD-SORT-WORK.
           
           SELECT EXECUTIVE-REPORT-FILE ASSIGN TO PRINTER-QPRINT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-PRT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FLIGHT-DATA-FILE.
       01  FD-FLIGHT-RECORD.
           05  FD-REGION-ID          PIC X(05).
           05  FD-ROUTE-ID           PIC X(10).
           05  FD-FLIGHT-NUMBER      PIC X(06).
           05  FD-TOTAL-REVENUE      PIC 9(07)V99.
           05  FD-SEATS-AVAILABLE    PIC 9(04).
           05  FD-SEATS-BOOKED       PIC 9(04).
           05  FD-STAGE-LENGTH-MILES PIC 9(05).

       SD  SORT-WORK-FILE.
       01  SW-SORT-RECORD.
           05  SW-REGION-ID          PIC X(05).
           05  SW-ROUTE-ID           PIC X(10).
           05  SW-FLIGHT-NUMBER      PIC X(06).
           05  SW-TOTAL-REVENUE      PIC 9(07)V99.
           05  SW-SEATS-AVAILABLE    PIC 9(04).
           05  SW-SEATS-BOOKED       PIC 9(04).
           05  SW-STAGE-LENGTH-MILES PIC 9(05).

       FD  EXECUTIVE-REPORT-FILE.
       01  PRT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS-FIELDS.
           05  WS-PRT-STATUS         PIC X(02) VALUE "00".

       01  WS-CONTROL-BREAK-KEYS.
           05  PREV-REGION-ID        PIC X(05) VALUE SPACES.
           05  PREV-ROUTE-ID         PIC X(10) VALUE SPACES.

       01  WS-FLAGS.
           05  FS-EOF-SORT           PIC X(01) VALUE 'N'.
               88  END-OF-SORT-FILE  VALUE 'Y'.

      * ANALYTICAL WORKING STORAGE VARIABLES
       01  WS-ANALYTICAL-CALCS.
           05  WS-ASM                PIC 9(10)V99 VALUE ZERO.
           05  WS-RASM               PIC 9(03)V9999 VALUE ZERO.
           05  WS-PLF                PIC 9(03)V99 VALUE ZERO.

      * MULTI-LEVEL CONTROL BREAK ACCUMULATORS
       01  WS-ROUTE-ACCUMULATORS.
           05  RT-REVENUE            PIC 9(09)V99 VALUE ZERO.
           05  RT-SEATS-AVAIL        PIC 9(06)    VALUE ZERO.
           05  RT-SEATS-BOOKED       PIC 9(06)    VALUE ZERO.
           05  RT-ASM                PIC 9(12)V99 VALUE ZERO.

       01  WS-REGION-ACCUMULATORS.
           05  REG-REVENUE           PIC 9(11)V99 VALUE ZERO.
           05  REG-SEATS-AVAIL       PIC 9(08)    VALUE ZERO.
           05  REG-SEATS-BOOKED       PIC 9(08)    VALUE ZERO.
           05  REG-ASM               PIC 9(14)V99 VALUE ZERO.

       01  WS-SYSTEM-ACCUMULATORS.
           05  SYS-REVENUE           PIC 9(13)V99 VALUE ZERO.
           05  SYS-SEATS-AVAIL       PIC 9(10)    VALUE ZERO.
           05  SYS-SEATS-BOOKED       PIC 9(10)    VALUE ZERO.
           05  SYS-ASM               PIC 9(16)V99 VALUE ZERO.

      * REPORTING LAYOUT STRINGS
       01  RPT-PAGE-HEADER-1.
           05  FILLER      PIC X(45) VALUE "RUN DATE: 2026-06-21".
           05  FILLER      PIC X(42) VALUE "TRAVEL BOOKING SYSTEM (TBS)" .
           05  FILLER      PIC X(45) VALUE "PAGE: 0001".
           
       01  RPT-PAGE-HEADER-2.
           05  FILLER      PIC X(132) VALUE 
           "                     EXECUTIVE AIRLINE SYSTEM PERFORMANCE REPORT                    ".

       01  RPT-COLUMN-HEADER-1.
           05  FILLER      PIC X(35) VALUE "REGION ROUTE      FLIGHT  REVENUE  ".
           05  FILLER      PIC X(50) VALUE " CAP    BKG   LOAD %   STAGE-MILES   ASM         ".
           05  FILLER      PIC X(47) VALUE "RASM ($)".

       01  RPT-COLUMN-HEADER-2.
           05  FILLER      PIC X(132) VALUE 
           "------------------------------------------------------------------------------------------------------------------------------------".

       01  RPT-DETAIL-LINE.
           05  DET-REG     PIC X(06).
           05  DET-RTE     PIC X(11).
           05  DET-FLT     PIC X(08).
           05  DET-REV     PIC $,$$$,$$9.99.
           05  FILLER      PIC X(02) VALUE SPACES.
           05  DET-CAP     PIC Z,ZZ9.
           05  FILLER      PIC X(02) VALUE SPACES.
           05  DET-BKG     PIC Z,ZZ9.
           05  FILLER      PIC X(03) VALUE SPACES.
           05  DET-PLF     PIC ZZ9.99.
           05  FILLER      PIC X(04) VALUE "%   ".
           05  DET-DIST    PIC ZZ,ZZ9.
           05  FILLER      PIC X(02) VALUE SPACES.
           05  DET-ASM     PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC X(02) VALUE SPACES.
           05  DET-RASM    PIC ZZ9.9999.

       01  RPT-BREAK-LINE.
           05  FILLER      PIC X(132) VALUE 
           "                                     --------- ----- ----- ------               --------------             --------".

       01  RPT-TOTAL-LINE.
           05  DET-LABEL   PIC X(25).
           05  TOT-REV     PIC $$,$$$,$$9.99.
           05  FILLER      PIC X(01) VALUE SPACES.
           05  TOT-CAP     PIC ZZ,ZZ9.
           05  FILLER      PIC X(01) VALUE SPACES.
           05  TOT-BKG     PIC ZZ,ZZ9.
           05  FILLER      PIC X(03) VALUE SPACES.
           05  TOT-PLF     PIC ZZ9.99.
           05  FILLER      PIC X(11) VALUE "%          ".
           05  TOT-ASM     PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC X(01) VALUE SPACES.
           05  TOT-RASM    PIC ZZ9.9999.

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SW-REGION-ID
                                SW-ROUTE-ID
               INPUT PROCEDURE  1000-INPUT-PROCEDURE
               OUTPUT PROCEDURE 2000-OUTPUT-PROCEDURE.
               
           IF WS-PRT-STATUS NOT = "00"
               DISPLAY "ERROR GENERATING PRINTER OVERFLOW SPOOL FILE"
           END-IF.
           STOP RUN.

       1000-INPUT-PROCEDURE.
           OPEN INPUT FLIGHT-DATA-FILE
           PERFORM UNTIL   FALSE
               READ FLIGHT-DATA-FILE
                   AT END EXIT PERFORM
               END-READ
               MOVE CORRESPONDING FD-FLIGHT-RECORD TO SW-SORT-RECORD
               RELEASE SW-SORT-RECORD
           END-PERFORM
           CLOSE FLIGHT-DATA-FILE.

       2000-OUTPUT-PROCEDURE.
           OPEN OUTPUT EXECUTIVE-REPORT-FILE
           PERFORM 2100-WRITE-HEADERS
           
           PERFORM 2200-READ-SORT-RECORD
           IF NOT END-OF-SORT-FILE
               MOVE SW-REGION-ID TO PREV-REGION-ID
               MOVE SW-ROUTE-ID  TO PREV-ROUTE-ID
           END-IF
           
           PERFORM UNTIL END-OF-SORT-FILE
               IF SW-REGION-ID NOT = PREV-REGION-ID
                   PERFORM 3100-ROUTE-BREAK
                   PERFORM 3200-REGION-BREAK
               ELSE
                   IF SW-ROUTE-ID NOT = PREV-ROUTE-ID
                       PERFORM 3100-ROUTE-BREAK
                   END-IF
               END-IF
               PERFORM 2300-PROCESS-DETAIL
               PERFORM 2200-READ-SORT-RECORD
           END-PERFORM
           
           PERFORM 3100-ROUTE-BREAK
           PERFORM 3200-REGION-BREAK
           PERFORM 3300-SYSTEM-FINAL-BREAK
           CLOSE EXECUTIVE-REPORT-FILE.

       2200-READ-SORT-RECORD.
           RETURN SORT-WORK-FILE
               AT END MOVE 'Y' TO FS-EOF-SORT
           END-RETURN.

       2100-WRITE-HEADERS.
           WRITE PRT-RECORD FROM RPT-PAGE-HEADER-1
           WRITE PRT-RECORD FROM RPT-PAGE-HEADER-2
           WRITE PRT-RECORD FROM RPT-COLUMN-HEADER-1
           WRITE PRT-RECORD FROM RPT-COLUMN-HEADER-2.

       2300-PROCESS-DETAIL.
      * INDIVIDUAL ENGINE ANALYTICS
           COMPUTE WS-ASM = SW-SEATS-AVAILABLE * SW-STAGE-LENGTH-MILES
           IF WS-ASM > 0
               COMPUTE WS-RASM = SW-TOTAL-REVENUE / WS-ASM
           ELSE
               MOVE ZERO TO WS-RASM
           END-IF
           
           IF SW-SEATS-AVAILABLE > 0
               COMPUTE WS-PLF = (SW-SEATS-BOOKED / SW-SEATS-AVAILABLE) * 100
           ELSE
               MOVE ZERO TO WS-PLF
           END-IF

      * MOVE DETAIL OUTLINES
           MOVE SW-REGION-ID          TO DET-REG
           MOVE SW-ROUTE-ID           TO DET-RTE
           MOVE SW-FLIGHT-NUMBER      TO DET-FLT
           MOVE SW-TOTAL-REVENUE      TO DET-REV
           MOVE SW-SEATS-AVAILABLE    TO DET-CAP
           MOVE SW-SEATS-BOOKED       TO DET-BKG
           MOVE WS-PLF                TO DET-PLF
           MOVE SW-STAGE-LENGTH-MILES TO DET-DIST
           MOVE WS-ASM                TO DET-ASM
           MOVE WS-RASM               TO DET-RASM
           
           WRITE PRT-RECORD FROM RPT-DETAIL-LINE

      * ACCUMULATE MULTI-LEVEL BREAK METRICS
           ADD SW-TOTAL-REVENUE   TO RT-REVENUE   REG-REVENUE   SYS-REVENUE
           ADD SW-SEATS-AVAILABLE TO RT-SEATS-AVAIL REG-SEATS-AVAIL SYS-SEATS-AVAIL
           ADD SW-SEATS-BOOKED    TO RT-SEATS-BOOKED REG-SEATS-BOOKED SYS-SEATS-BOOKED
           ADD WS-ASM             TO RT-ASM       REG-ASM       SYS-ASM.

       3100-ROUTE-BREAK.
           WRITE PRT-RECORD FROM RPT-BREAK-LINE
           MOVE "  * ROUTE TOTALS:" TO DET-LABEL
           MOVE RT-REVENUE         TO TOT-REV
           MOVE RT-SEATS-AVAIL     TO TOT-CAP
           MOVE RT-SEATS-BOOKED    TO TOT-BKG
           MOVE RT-ASM             TO TOT-ASM
           
           IF RT-SEATS-AVAIL > 0
               COMPUTE TOT-PLF = (RT-SEATS-BOOKED / RT-SEATS-AVAIL) * 100
           ELSE
               MOVE ZERO TO TOT-PLF
           END-IF
           
           IF RT-ASM > 0
               COMPUTE TOT-RASM = RT-REVENUE / RT-ASM
           ELSE
               MOVE ZERO TO TOT-RASM
           END-IF
           
           WRITE PRT-RECORD FROM RPT-TOTAL-LINE
           MOVE SPACES TO PRT-RECORD
           WRITE PRT-RECORD
           
      * RESET ACCUMULATORS FOR INTERMEDIATE CONTROL BREAK
           MOVE ZERO TO RT-REVENUE RT-SEATS-AVAIL RT-SEATS-BOOKED RT-ASM
           MOVE SW-ROUTE-ID TO PREV-ROUTE-ID.

       3200-REGION-BREAK.
           WRITE PRT-RECORD FROM RPT-BREAK-LINE
           MOVE " ** REGION TOTALS:" TO DET-LABEL
           MOVE REG-REVENUE        TO TOT-REV
           MOVE REG-SEATS-AVAIL    TO TOT-CAP
           MOVE REG-SEATS-BOOKED   TO TOT-BKG
           MOVE REG-ASM            TO TOT-ASM
           
           IF REG-SEATS-AVAIL > 0
               COMPUTE TOT-PLF = (REG-SEATS-BOOKED / REG-SEATS-AVAIL) * 100
           ELSE
               MOVE ZERO TO TOT-PLF
           END-IF
           
           IF REG-ASM > 0
               COMPUTE TOT-RASM = REG-REVENUE / REG-ASM
           ELSE
               MOVE ZERO TO TOT-RASM
           END-IF
           
           WRITE PRT-RECORD FROM RPT-TOTAL-LINE
           MOVE SPACES TO PRT-RECORD
           WRITE PRT-RECORD
           
      * RESET ACCUMULATORS FOR MAJOR CONTROL BREAK
           MOVE ZERO TO REG-REVENUE REG-SEATS-AVAIL REG-SEATS-BOOKED REG-ASM
           MOVE SW-REGION-ID TO PREV-REGION-ID.

       3300-SYSTEM-FINAL-BREAK.
           WRITE PRT-RECORD FROM RPT-BREAK-LINE
           MOVE "*** SYSTEM TOTALS:" TO DET-LABEL
           MOVE SYS-REVENUE        TO TOT-REV
           MOVE SYS-SEATS-AVAIL    TO TOT-CAP
           MOVE SYS-SEATS-BOOKED   TO TOT-BKG
           MOVE SYS-ASM            TO TOT-ASM
           
           IF SYS-SEATS-AVAIL > 0
               COMPUTE TOT-PLF = (SYS-SEATS-BOOKED / SYS-SEATS-AVAIL) * 100
           ELSE
               MOVE ZERO TO TOT-PLF
           END-IF
           
           IF SYS-ASM > 0
               COMPUTE TOT-RASM = SYS-REVENUE / SYS-ASM
           ELSE
               MOVE ZERO TO TOT-RASM
           END-IF
           
           WRITE PRT-RECORD FROM RPT-TOTAL-LINE.
