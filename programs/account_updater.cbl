       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-UPDATE.
       AUTHOR. STUDENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO "ACCOUNTS"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSIN ASSIGN TO "TRANSIN"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS.
       01  ACCOUNT-RECORD          PIC X(80).
       
       FD  TRANSIN.
       01  TRANSACTION-RECORD      PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-UPDATE-COUNT         PIC 9(5) VALUE 0.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ACCOUNT-UPDATE: Starting account updates..."
           
           OPEN INPUT ACCOUNTS
           OPEN INPUT TRANSIN
           
           PERFORM PROCESS-UPDATES UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE ACCOUNTS
           CLOSE TRANSIN
           
           DISPLAY "ACCOUNT-UPDATE: Updates completed"
           DISPLAY "ACCOUNT-UPDATE: Accounts updated: " WS-UPDATE-COUNT
           
           STOP RUN.
       
       PROCESS-UPDATES.
           READ TRANSIN INTO TRANSACTION-RECORD
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-UPDATE-COUNT
                   DISPLAY "PROCESSING: " TRANSACTION-RECORD
           END-READ.
