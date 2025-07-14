       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-VALIDATOR.
       AUTHOR. [YOUR NAME].

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSIN ASSIGN TO "TRANSIN"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSOUT ASSIGN TO "TRANSOUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSIN.
       01  TRANSACTION-INPUT       PIC X(80).

       FD  TRANSOUT.
       01  TRANSACTION-OUTPUT      PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-TOTAL-COUNT      PIC 9(5) VALUE 0.
           05  WS-VALID-COUNT      PIC 9(5) VALUE 0.
           05  WS-INVALID-COUNT    PIC 9(5) VALUE 0.

       01  WS-TRANSACTION-RECORD.
           05  WS-TXN-ID           PIC X(6).
           05  FILLER              PIC X VALUE ','.
           05  WS-TXN-TYPE         PIC X(10).
           05  FILLER              PIC X VALUE ','.
           05  WS-ACCOUNT-NUM      PIC X(5).
           05  FILLER              PIC X VALUE ','.
           05  WS-AMOUNT           PIC X(10).
           05  FILLER              PIC X VALUE ','.
           05  WS-DATE             PIC X(8).

       01  WS-EOF-FLAG             PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "BATCH-VALIDATOR: Starting transaction validation..."

           OPEN INPUT TRANSIN
           OPEN OUTPUT TRANSOUT

           PERFORM PROCESS-TRANSACTIONS UNTIL WS-EOF-FLAG = 'Y'

           CLOSE TRANSIN
           CLOSE TRANSOUT

           PERFORM DISPLAY-STATISTICS

           IF WS-INVALID-COUNT > 0
               DISPLAY "BATCH-VALIDATOR: Invalid transactions found!"
               STOP RUN RETURNING 4
           ELSE
               DISPLAY "BATCH-VALIDATOR: All transactions valid!"
               STOP RUN RETURNING 0
           END-IF.

       PROCESS-TRANSACTIONS.
           READ TRANSIN INTO TRANSACTION-INPUT
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-TOTAL-COUNT
                   PERFORM VALIDATE-TRANSACTION
           END-READ.

       VALIDATE-TRANSACTION.
           MOVE TRANSACTION-INPUT TO WS-TRANSACTION-RECORD

           IF WS-TXN-TYPE = 'DEPOSIT'
               OR WS-TXN-TYPE = 'WITHDRAWAL'
               OR WS-TXN-TYPE = 'TRANSFER'
               ADD 1 TO WS-VALID-COUNT
               WRITE TRANSACTION-OUTPUT FROM TRANSACTION-INPUT
               DISPLAY "✓ VALID: ", TRANSACTION-INPUT
           ELSE
               ADD 1 TO WS-INVALID-COUNT
               DISPLAY "✗ INVALID: ", TRANSACTION-INPUT
           END-IF.

       DISPLAY-STATISTICS.
           DISPLAY "BATCH-VALIDATOR: Validation completed"
           DISPLAY "BATCH-VALIDATOR: Total transactions: ", WS-TOTAL-COUNT
           DISPLAY "BATCH-VALIDATOR: Valid transactions: ", WS-VALID-COUNT
           DISPLAY "BATCH-VALIDATOR: Invalid transactions: ", WS-INVALID-COUNT.
