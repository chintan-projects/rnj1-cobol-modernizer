      ******************************************************************
      * Program Name : CBACT04C
      * Application  : CardDemo
      * Type         : BATCH COBOL Program
      * Function     : Credit Card Interest Calculator
      *                Calculates monthly interest on outstanding balances
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBACT04C.
       AUTHOR.        AWS MAINFRAME MODERNIZATION TEAM.
       DATE-WRITTEN.  2022-03-15.
       DATE-COMPILED.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE
               ASSIGN TO ACCTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ACCT-ID
               FILE STATUS IS WS-ACCT-STATUS.
      
           SELECT INTEREST-REPORT
               ASSIGN TO INTRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  ACCOUNT-FILE
           RECORD CONTAINS 300 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  ACCOUNT-RECORD.
           05 ACCT-ID                   PIC X(11).
           05 ACCT-HOLDER-NAME          PIC X(50).
           05 ACCT-CREDIT-LIMIT         PIC 9(7)V99.
           05 ACCT-CURRENT-BALANCE      PIC S9(7)V99.
           05 ACCT-MINIMUM-PAYMENT      PIC 9(5)V99.
           05 ACCT-INTEREST-RATE        PIC 9(2)V9(4).
           05 ACCT-LAST-PAYMENT-DATE    PIC X(10).
           05 ACCT-LAST-PAYMENT-AMT     PIC 9(7)V99.
           05 ACCT-STATUS               PIC X(01).
              88 ACCT-ACTIVE            VALUE 'A'.
              88 ACCT-CLOSED            VALUE 'C'.
              88 ACCT-SUSPENDED         VALUE 'S'.
           05 ACCT-OPEN-DATE            PIC X(10).
           05 FILLER                    PIC X(192).
      
       FD  INTEREST-REPORT
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-LINE                  PIC X(132).
      
       WORKING-STORAGE SECTION.
      
       01  WS-FILE-STATUS.
           05 WS-ACCT-STATUS            PIC XX.
           05 WS-RPT-STATUS             PIC XX.
      
       01  WS-COUNTERS.
           05 WS-RECORDS-READ           PIC 9(7) VALUE 0.
           05 WS-RECORDS-PROCESSED      PIC 9(7) VALUE 0.
           05 WS-RECORDS-SKIPPED        PIC 9(7) VALUE 0.
           05 WS-TOTAL-INTEREST         PIC S9(11)V99 VALUE 0.
      
       01  WS-CALCULATION-FIELDS.
           05 WS-MONTHLY-RATE           PIC 9V9(6).
           05 WS-INTEREST-AMOUNT        PIC S9(7)V99.
           05 WS-NEW-BALANCE            PIC S9(9)V99.
           05 WS-DAYS-IN-MONTH          PIC 99 VALUE 30.
      
       01  WS-DATE-FIELDS.
           05 WS-CURRENT-DATE.
              10 WS-CURR-YEAR           PIC 9(4).
              10 WS-CURR-MONTH          PIC 9(2).
              10 WS-CURR-DAY            PIC 9(2).
           05 WS-PROCESSING-DATE        PIC X(10).
      
       01  WS-REPORT-HEADER.
           05 FILLER                    PIC X(40) 
              VALUE 'CARDDEMO MONTHLY INTEREST CALCULATION'.
           05 FILLER                    PIC X(20) VALUE SPACES.
           05 FILLER                    PIC X(06) VALUE 'DATE: '.
           05 WS-HDR-DATE               PIC X(10).
           05 FILLER                    PIC X(56) VALUE SPACES.
      
       01  WS-REPORT-DETAIL.
           05 WS-DTL-ACCT-ID            PIC X(11).
           05 FILLER                    PIC X(02) VALUE SPACES.
           05 WS-DTL-NAME               PIC X(25).
           05 FILLER                    PIC X(02) VALUE SPACES.
           05 WS-DTL-BALANCE            PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                    PIC X(02) VALUE SPACES.
           05 WS-DTL-RATE               PIC Z9.9999.
           05 FILLER                    PIC X(02) VALUE SPACES.
           05 WS-DTL-INTEREST           PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                    PIC X(02) VALUE SPACES.
           05 WS-DTL-NEW-BAL            PIC Z,ZZZ,ZZ9.99-.
           05 FILLER                    PIC X(30) VALUE SPACES.
      
       01  WS-REPORT-TOTAL.
           05 FILLER                    PIC X(50) 
              VALUE 'TOTAL INTEREST CALCULATED:'.
           05 WS-TOT-INTEREST           PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05 FILLER                    PIC X(65) VALUE SPACES.
      
       01  WS-FLAGS.
           05 WS-EOF-FLAG               PIC X VALUE 'N'.
              88 END-OF-FILE            VALUE 'Y'.
              88 NOT-END-OF-FILE        VALUE 'N'.
      
       PROCEDURE DIVISION.
      
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNTS
              UNTIL END-OF-FILE
           PERFORM 3000-FINALIZE
           STOP RUN.
      
       1000-INITIALIZE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
              DELIMITED BY SIZE INTO WS-PROCESSING-DATE
           END-STRING
           
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING ACCOUNT FILE: ' WS-ACCT-STATUS
              STOP RUN
           END-IF
           
           OPEN OUTPUT INTEREST-REPORT
           IF WS-RPT-STATUS NOT = '00'
              DISPLAY 'ERROR OPENING REPORT FILE: ' WS-RPT-STATUS
              STOP RUN
           END-IF
           
           MOVE WS-PROCESSING-DATE TO WS-HDR-DATE
           WRITE REPORT-LINE FROM WS-REPORT-HEADER
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           PERFORM 1100-READ-ACCOUNT.
      
       1100-READ-ACCOUNT.
           READ ACCOUNT-FILE
              AT END
                 SET END-OF-FILE TO TRUE
              NOT AT END
                 ADD 1 TO WS-RECORDS-READ
           END-READ.
      
       2000-PROCESS-ACCOUNTS.
           IF NOT END-OF-FILE
              EVALUATE TRUE
                 WHEN ACCT-ACTIVE
                    PERFORM 2100-CALCULATE-INTEREST
                 WHEN ACCT-CLOSED
                    ADD 1 TO WS-RECORDS-SKIPPED
                 WHEN ACCT-SUSPENDED
                    ADD 1 TO WS-RECORDS-SKIPPED
                 WHEN OTHER
                    ADD 1 TO WS-RECORDS-SKIPPED
              END-EVALUATE
              PERFORM 1100-READ-ACCOUNT
           END-IF.
      
       2100-CALCULATE-INTEREST.
      *    Convert annual rate to monthly rate
           COMPUTE WS-MONTHLY-RATE = 
              ACCT-INTEREST-RATE / 12
           END-COMPUTE
           
      *    Calculate interest only on positive balances
           IF ACCT-CURRENT-BALANCE > 0
              COMPUTE WS-INTEREST-AMOUNT = 
                 ACCT-CURRENT-BALANCE * WS-MONTHLY-RATE
              END-COMPUTE
              
      *       Apply minimum interest threshold of $0.50
              IF WS-INTEREST-AMOUNT < 0.50
                 MOVE 0.50 TO WS-INTEREST-AMOUNT
              END-IF
              
              COMPUTE WS-NEW-BALANCE = 
                 ACCT-CURRENT-BALANCE + WS-INTEREST-AMOUNT
              END-COMPUTE
              
      *       Check if new balance exceeds credit limit
              IF WS-NEW-BALANCE > ACCT-CREDIT-LIMIT
                 DISPLAY 'WARNING: ACCOUNT ' ACCT-ID 
                    ' EXCEEDS CREDIT LIMIT'
              END-IF
              
              ADD WS-INTEREST-AMOUNT TO WS-TOTAL-INTEREST
              ADD 1 TO WS-RECORDS-PROCESSED
              
              PERFORM 2200-WRITE-DETAIL
           ELSE
              ADD 1 TO WS-RECORDS-SKIPPED
           END-IF.
      
       2200-WRITE-DETAIL.
           MOVE ACCT-ID TO WS-DTL-ACCT-ID
           MOVE ACCT-HOLDER-NAME(1:25) TO WS-DTL-NAME
           MOVE ACCT-CURRENT-BALANCE TO WS-DTL-BALANCE
           MOVE ACCT-INTEREST-RATE TO WS-DTL-RATE
           MOVE WS-INTEREST-AMOUNT TO WS-DTL-INTEREST
           MOVE WS-NEW-BALANCE TO WS-DTL-NEW-BAL
           
           WRITE REPORT-LINE FROM WS-REPORT-DETAIL.
      
       3000-FINALIZE.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE WS-TOTAL-INTEREST TO WS-TOT-INTEREST
           WRITE REPORT-LINE FROM WS-REPORT-TOTAL
           
           CLOSE ACCOUNT-FILE
           CLOSE INTEREST-REPORT
           
           DISPLAY 'PROCESSING COMPLETE'
           DISPLAY 'RECORDS READ:      ' WS-RECORDS-READ
           DISPLAY 'RECORDS PROCESSED: ' WS-RECORDS-PROCESSED
           DISPLAY 'RECORDS SKIPPED:   ' WS-RECORDS-SKIPPED
           DISPLAY 'TOTAL INTEREST:    ' WS-TOTAL-INTEREST.
