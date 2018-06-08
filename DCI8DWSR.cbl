       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DWSR.
       AUTHOR. AMAR AL-ADIL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE  PIC X(60) VALUE SPACES.

       01 ACCT-REQ.
           05 REQ-ACCTNO        PIC X(5).
           05 FILLER            PIC X(100).

       01 ACCT-RESP.
           05 RESP-STATUS.
               10 RESP-S-CODE     PIC 9.
               10 RESP-S-MESSAGE  PIC X(60).
           05 RESP-ACCOUNT-DATA.
               10 RESP-AD-ACCTNO   PIC X(5).
               10 RESP-AD-FNAME    PIC X(15).
               10 RESP-AD-SNAME    PIC X(15).
               10 RESP-AD-STATUS   PIC X.
               10 RESP-AD-LIMIT    PIC X(8).

       77 REQ-ACCTNO-LEN        COMP  PIC  S9(4).

       01 WS-CHAR  PIC X(1).
           88 WS-CHAR-VALID
                   value "A" THRU "Z",
                         "0" THRU "9".

       77 COUNTER PIC S9(4) COMP
           VALUE 0.

       77 WS-ACCT-TOTAL-LEN         PIC 9
           VALUE 0.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(105).

       PROCEDURE DIVISION.

       000-START-LOGIC.

           EXEC CICS HANDLE CONDITION
                NOTFND(300-ACCTFILE-NOTFND)
           END-EXEC.

           MOVE DFHCOMMAREA TO ACCT-REQ.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.


       200-MAIN-LOGIC.
           MOVE LOW-VALUES TO ACCT-RESP.

           COMPUTE REQ-ACCTNO-LEN = ( FUNCTION LENGTH(REQ-ACCTNO) )

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > REQ-ACCTNO-LEN)

                MOVE REQ-ACCTNO(COUNTER:1)
                TO WS-CHAR

                IF (WS-CHAR-VALID) THEN
                    ADD 1 TO WS-ACCT-TOTAL-LEN
                END-IF

           END-PERFORM.


           IF WS-ACCT-TOTAL-LEN < 5 THEN

                MOVE 'ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG'
                TO RESP-S-MESSAGE

                MOVE 1 TO RESP-S-CODE

                MOVE LOW-VALUES TO RESP-AD-ACCTNO, RESP-AD-FNAME,
                    RESP-AD-SNAME, RESP-AD-LIMIT, RESP-AD-STATUS

                MOVE ACCT-RESP TO DFHCOMMAREA

                GO TO 999-EXIT

           ELSE IF REQ-ACCTNO IS NOT NUMERIC

                MOVE 'ACCOUNT NUMBERS MUST BE NUMERIC'
                TO RESP-S-MESSAGE

                MOVE 1 TO RESP-S-CODE

                MOVE LOW-VALUES TO RESP-AD-ACCTNO, RESP-AD-FNAME,
                    RESP-AD-SNAME, RESP-AD-LIMIT, RESP-AD-STATUS

                MOVE ACCT-RESP TO DFHCOMMAREA

                GO TO 999-EXIT

           ELSE

      * TODO: ATTEMPT TO FIND AN ACCOUND RECORD IN ACCTFILE
      *       FROM USER INPUT ACCOUNTNO

                MOVE REQ-ACCTNO TO ACCTNO
                EXEC CICS READ
                     FILE('ACCTFILE')
                     INTO(ACCTREC)
                     LENGTH(ACCTREC-LEN)
                     RIDFLD(ACCTKEY)
                END-EXEC

      * TODO: MOVE VALUES FROM ACCTREC TO O FIELDS
                MOVE 0 TO RESP-S-CODE
                MOVE "SUCCESS" TO RESP-S-MESSAGE
                MOVE ACCTNO TO RESP-AD-ACCTNO
                MOVE FNAME TO RESP-AD-FNAME
                MOVE SNAME TO RESP-AD-SNAME
                MOVE CRLIMIT TO RESP-AD-LIMIT
                MOVE STAT TO RESP-AD-STATUS

                MOVE ACCT-RESP TO DFHCOMMAREA

                GO TO 999-EXIT

           END-IF.

       300-ACCTFILE-NOTFND.

           MOVE LOW-VALUES TO RESP-AD-ACCTNO, RESP-AD-FNAME,
                RESP-AD-SNAME, RESP-AD-LIMIT, RESP-AD-STATUS

           MOVE "Account not found." TO RESP-S-MESSAGE.
           MOVE 2 TO RESP-S-CODE.

           MOVE ACCT-RESP TO DFHCOMMAREA.

           GO TO 999-EXIT.

       999-EXIT.

           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DWSR.
