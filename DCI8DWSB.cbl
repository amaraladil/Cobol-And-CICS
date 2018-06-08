       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DWSB.
       AUTHOR. AMAR AL-ADIL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE  PIC X(60) VALUE SPACES.

       01 ACCT-REQ.
           05 SEARCH-NAME    PIC X(15).
           05 AL-SPACE       PIC X(400).

       01 ACCT-RESP.
           05 RESP-STATUS.
               10 RESP-CODE             PIC X.
               10 RESP-MSG              PIC X(60).
           05 RESP-ACCT-LIST.
               10 RESP-COUNT            PIC X(2).
               10 RESP-DEPENDS          PIC X(2).
               10 RESP-ACCTS OCCURS 0 TO 10 TIMES
                                DEPENDING ON WS-COUNT.
                   15 RESP-ACCTNO       PIC X(5).
                   15 RESP-FNAME        PIC X(15).
                   15 RESP-SNAME        PIC X(15).

       01 WS-RESPONSE            PIC S9(4) COMP.


       77 WS-LINE-SUB            PIC 9(2).
       77 WS-LINES               PIC 9(2) VALUE 10.

       77 WS-COUNT               PIC 9(2) VALUE 0.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(415).

       PROCEDURE DIVISION.

       000-START-LOGIC.

           EXEC CICS HANDLE CONDITION
                NOTFND(300-ACCTFILE-NOTFND)
           END-EXEC.

           EXEC CICS IGNORE CONDITION
                DUPKEY
           END-EXEC.

           MOVE DFHCOMMAREA TO ACCT-REQ.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.


       200-MAIN-LOGIC.
           MOVE LOW-VALUES TO ACCT-RESP.
           MOVE SEARCH-NAME TO SNAME.

           EXEC CICS STARTBR
                FILE('ACCTNAME')
                RIDFLD(SNAME)
           END-EXEC.

           MOVE 1 TO WS-LINE-SUB.

           PERFORM 250-BRWS-FORWARD
                VARYING WS-LINE-SUB
                FROM 1 BY 1
                UNTIL WS-LINE-SUB > WS-LINES.

           EXEC CICS ENDBR
                FILE('ACCTNAME')
           END-EXEC.

           MOVE "Success" TO RESP-MSG.
           MOVE 0 TO RESP-CODE.
           MOVE WS-COUNT TO RESP-COUNT RESP-DEPENDS.

           MOVE ACCT-RESP TO DFHCOMMAREA.

           GO TO 999-EXIT.

       250-BRWS-FORWARD.

           EXEC CICS READNEXT
                FILE('ACCTNAME')
                INTO(ACCTREC)
                RIDFLD(SNAME)
                LENGTH(ACCTREC-LEN)
                RESP(WS-RESPONSE)
           END-EXEC.

           IF WS-RESPONSE = DFHRESP(ENDFILE) THEN
      *         END OF FILE, NO DATA FOUND
                CONTINUE
           ELSE
      *         MOVE DATA TO THE SCREEN WHEN WE HAVE A READ LINE
                MOVE ACCTNO TO RESP-ACCTNO(WS-LINE-SUB)
                MOVE FNAME TO RESP-FNAME(WS-LINE-SUB)
                MOVE SNAME TO RESP-SNAME(WS-LINE-SUB)

                ADD 1 TO WS-COUNT

           END-IF.

       300-ACCTFILE-NOTFND.

           MOVE "Account not found." TO RESP-MSG.
           MOVE 2 TO RESP-CODE.
           MOVE WS-COUNT TO RESP-COUNT RESP-DEPENDS.

           MOVE ACCT-RESP TO DFHCOMMAREA.

           GO TO 999-EXIT.

       999-EXIT.

           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DWSB.
