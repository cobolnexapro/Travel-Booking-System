       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TBSEODBATCH.
       AUTHOR.        IBM-I-BATCH-EXPERT.
       DATE-WRITTEN.  2026-06-21.
      *================================================================*
      * TRAVEL BOOKING SYSTEM (TBS) - END OF DAY RECONCILIATION        *
      *================================================================*
      * SECURITY CLASSIFICATION: CONFIDENTIAL / PROPRIETARY            *
      * DESCRIPTION:                                                   *
      * SCANS RESERVATIONS, IDENTIFIES EXPIRED FLIGHTS, TRANSFORMS   *
      * UNCONFIRMED OR LAPSED RESERVATIONS INTO NO-SHOW STATUS,     *
      * AND EMITS COBOL AUDIT JOURNAL RECORDS FOR ACCOUNTABILITY.    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.
       SPECIAL-NAMES.
           LOCAL-DATA AREA IS LDA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESERVATION-FILE ASSIGN TO DATABASE-RESVFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS RESV-KEY
               FILE STATUS  IS WS-RESV-STATUS.

           SELECT FLIGHT-FILE ASSIGN TO DATABASE-FLTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS FLT-KEY
               FILE STATUS  IS WS-FLT-STATUS.

           SELECT AUDIT-LOG-FILE ASSIGN TO DATABASE-AUDITLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  RESERVATION-FILE.
       01  RESV-RECORD.
           05  RESV-KEY.
               10  RESV-FLIGHT-ID    PIC X(10).
               10  RESV-PASSENGER-ID PIC X(10).
           05  RESV-STATUS           PIC X(01).
               88  RESV-ACTIVE       VALUE 'A'.
               88  RESV-CONFIRMED    VALUE 'C'.
               88  RESV-NO-SHOW      VALUE 'N'.
               88  RESV-CANCELLED    VALUE 'X'.
           05  RESV-BOOKING-DATE     PIC X(08).
           05  RESV-FARE-PAID        PIC 9(05)V99.
           05  RESV-SEAT-NO          PIC X(04).

       FD  FLIGHT-FILE.
       01  FLT-RECORD.
           05  FLT-KEY.
               10  FLT-ID            PIC X(10).
           05  FLT-DEPART-DATE       PIC X(08).
           05  FLT-STATUS            PIC X(01).
               88  FLT-SCHEDULED     VALUE 'S'.
               88  FLT-BOARDING      VALUE 'B'.
               88  FLT-DEPARTED      VALUE 'D'.
               88  FLT-COMPLETED     VALUE 'C'.
           05  FLT-CAPACITY          PIC 9(03).
           05  FLT-BOOKED-SEATS      PIC 9(03).

       FD  AUDIT-LOG-FILE.
       01  AUDIT-RECORD.
           05  AUD-TIMESTAMP         PIC X(14).
           05  AUD-RESV-FLIGHT-ID    PIC X(10).
           05  AUD-RESV-PASSENGER-ID PIC X(10).
           05  AUD-PREV-STATUS       PIC X(01).
           05  AUD-NEW-STATUS        PIC X(01).
           05  AUD-REASON            PIC X(30).

       WORKING-STORAGE SECTION.
      * FILE STATUS FIELDS
       01  WS-FILE-STATUS-FIELDS.
           05  WS-RESV-STATUS        PIC X(02).
           05  WS-FLT-STATUS         PIC X(02).
           05  WS-AUDIT-STATUS       PIC X(02).
           
      * SYSTEM DATE FIELDS
       01  WS-SYSTEM-DATE.
           05  WS-CURRENT-YEAR       PIC X(04).
           05  WS-CURRENT-MONTH      PIC X(02).
           05  WS-CURRENT-DAY        PIC X(02).
       01  WS-COMPARE-DATE           PIC X(08).

      * COUNTERS AND TOTALS FOR SUMMARY TABLE
       01  WS-RECONCILIATION-COUNTERS.
           05  WS-TOT-RECORDS-READ   PIC 9(06) VALUE ZERO.
           05  WS-TOT-EXPIRED        PIC 9(06) VALUE ZERO.
           05  WS-TOT-NO-SHOWS       PIC 9(06) VALUE ZERO.
           05  WS-TOT-ACTIVE-KEPT    PIC 9(06) VALUE ZERO.
           05  WS-REVENUE-LOST       PIC 9(07)V99 VALUE ZERO.

      * POINTERS AND FLAGS
       01  WS-FLAGS.
           05  FS-EOF-RESV           PIC X(01) VALUE 'N'.
               88  END-OF-RESV-FILE  VALUE 'Y'.

      * DISPLAY SUMMARY FORMATTING
       01  WS-SUMMARY-HDR.
           05  FILLER                PIC X(45) VALUE 
               "=============================================".
       01  WS-SUMMARY-TITLE.
           05  FILLER                PIC X(45) VALUE 
               "   TBS EOD BATCH RECONCILIATION SUMMARY      ".
       01  WS-LINE-READ.
           05  FILLER                PIC X(25) VALUE "TOTAL RECORDS READ:      ".
           05  DET-READ              PIC ZZZ,ZZ9.
       01  WS-LINE-EXPIRED.
           05  FILLER                PIC X(25) VALUE "TOTAL EXPIRED FLIGHTS:   ".
           05  DET-EXPIRED           PIC ZZZ,ZZ9.
       01  WS-LINE-NOSHOW.
           05  FILLER                PIC X(25) VALUE "AUTOMATED NO-SHOWS SET:  ".
           05  DET-NOSHOW            PIC ZZZ,ZZ9.
       01  WS-LINE-KEPT.
           05  FILLER                PIC X(25) VALUE "ACTIVE RECORDS RETAINED: ".
           05  DET-KEPT              PIC ZZZ,ZZ9.
       01  WS-LINE-REV.
           05  FILLER                PIC X(25) VALUE "TOTAL REVENUE FORFEITED: ".
           05  DET-REV               PIC $,$$$,$$9.99.

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-RESERVATIONS
               UNTIL END-OF-RESV-FILE
           PERFORM 3000-TERMINATION
           STOP RUN.

       1000-INITIALIZATION.
           ACCEPT WS-SYSTEM-DATE FROM DATE YYYYMMDD
           STRING WS-CURRENT-YEAR WS-CURRENT-MONTH WS-CURRENT-DAY
               DELIMITED BY SIZE INTO WS-COMPARE-DATE
           
           OPEN INPUT  FLIGHT-FILE
           OPEN I-O    RESERVATION-FILE
           OPEN OUTPUT AUDIT-LOG-FILE
           
           IF WS-RESV-STATUS NOT = "00" OR WS-FLT-STATUS NOT = "00"
               DISPLAY "CRITICAL: FILE OPEN FAILURE. SYSTEM ABORTING."
               MOVE "99" TO WS-RESV-STATUS
               STOP RUN
           END-IF.

           PERFORM 1100-READ-NEXT-RESV.

       1100-READ-NEXT-RESV.
           READ RESERVATION-FILE NEXT
               AT END MOVE 'Y' TO FS-EOF-RESV
           END-READ
           IF NOT END-OF-RESV-FILE
               ADD 1 TO WS-TOT-RECORDS-READ
           END-IF.

       2000-PROCESS-RESERVATIONS.
      * LINK TO FLIGHT-FILE TO VERIFY DEPARTURE DATE
           MOVE RESV-FLIGHT-ID TO FLT-ID
           READ FLIGHT-FILE INVALID KEY
               DISPLAY "WARNING: FLIGHT NOT FOUND FOR RESV: " RESV-KEY
               PERFORM 1100-READ-NEXT-RESV
               EXIT PARAGRAPH
           END-READ.

      * RECONCILIATION LOGIC: IF FLIGHT DEPARTED OR DATE PAST
           IF FLT-DEPART-DATE < WS-COMPARE-DATE OR FLT-DEPARTED OR FLT-COMPLETED
               ADD 1 TO WS-TOT-EXPIRED
               IF RESV-ACTIVE OR RESV-CONFIRMED
                   PERFORM 2100-TRIGGER-NO-SHOW
               ELSE
                   ADD 1 TO WS-TOT-ACTIVE-KEPT
               END-IF
           ELSE
               ADD 1 TO WS-TOT-ACTIVE-KEPT
           END-IF.

           PERFORM 1100-READ-NEXT-RESV.

       2100-TRIGGER-NO-SHOW.
           MOVE RESV-STATUS TO AUD-PREV-STATUS
           MOVE 'N' TO RESV-STATUS
           REWRITE RESV-RECORD
               INVALID KEY 
                   DISPLAY "ERROR WRITING REWRITE FOR KEY: " RESV-KEY
           END-REWRITE
           
           ADD 1 TO WS-TOT-NO-SHOWS
           ADD RESV-FARE-PAID TO WS-REVENUE-LOST
           
           PERFORM 2200-WRITE-AUDIT-LOG.

       2200-WRITE-AUDIT-LOG.
           ACCEPT AUD-TIMESTAMP FROM TIME
           MOVE RESV-FLIGHT-ID    TO AUD-RESV-FLIGHT-ID
           MOVE RESV-PASSENGER-ID TO AUD-RESV-PASSENGER-ID
           MOVE 'N'               TO AUD-NEW-STATUS
           MOVE "AUTO-EOD NO SHOW FORFEIT" TO AUD-REASON
           WRITE AUDIT-RECORD
               INVALID KEY DISPLAY "AUDIT WRITE FAILED".

       3000-TERMINATION.
           CLOSE RESERVATION-FILE
                 FLIGHT-FILE
                 AUDIT-LOG-FILE
                 
           MOVE WS-TOT-RECORDS-READ TO DET-READ
           MOVE WS-TOT-EXPIRED      TO DET-EXPIRED
           MOVE WS-TOT-NO-SHOWS     TO DET-NOSHOW
           MOVE WS-TOT-ACTIVE-KEPT  TO DET-KEPT
           MOVE WS-REVENUE-LOST     TO DET-REV

           DISPLAY WS-SUMMARY-HDR
           DISPLAY WS-SUMMARY-TITLE
           DISPLAY WS-SUMMARY-HDR
           DISPLAY WS-LINE-READ
           DISPLAY WS-LINE-EXPIRED
           DISPLAY WS-LINE-NOSHOW
           DISPLAY WS-LINE-KEPT
           DISPLAY WS-LINE-REV
           DISPLAY WS-SUMMARY-HDR.
