       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGE.
       AUTHOR. ANDREW MAYNE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-CHAR  PIC X(1).
           88 WS-CHAR-VALID
                   value "A" THRU "Z".

       01 WS-ALPHANUM  PIC X(1).
           88 WS-ALPHANUM-VALID
                   value "A" THRU "Z",
                        "0" THRU "9".

       77 COUNTER PIC S9(4) COMP
           VALUE 0.

       01 WS-FNAME  PIC X(1).
           88 WS-FNAME-VALID
                   value "A" THRU "I",
                   "J" THRU "R",
                   "S" THRU "Z".

       01 WS-FNAME-VAL-COUNT  PIC 99
           VALUE 0.


       01 WS-SNAME  PIC X(1).
           88 WS-SNAME-VALID
                   value "A" THRU "I",
                   "J" THRU "R",
                   "S" THRU "Z".

       01 WS-SNAME-VAL-COUNT  PIC 99
           VALUE 0.

       01 WS-TITLE-COUNT      PIC 9
           VALUE 0.

       01 WS-ADDR1-COUNT      PIC 99
           VALUE 0.

       01 WS-ADDR2-COUNT      PIC 99
           VALUE 0.

       77 WS-CRLIMIT PIC 9(8).

       01 WS-ACCTDATA.
       COPY 'ACCTDATA'.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(156).

       PROCEDURE DIVISION.

       100-MAIN-LOGIC.

           MOVE DFHCOMMAREA TO WS-ACCTDATA.

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > AD-FNAMEL)

                MOVE AD-FNAME(COUNTER:1)
                TO WS-CHAR

                IF (WS-CHAR-VALID) THEN
                    ADD 1 TO WS-FNAME-VAL-COUNT
                END-IF

           END-PERFORM.

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > AD-SNAMEL)

                MOVE AD-SNAME(COUNTER:1)
                TO WS-CHAR

                IF (WS-CHAR-VALID) THEN
                    ADD 1 TO WS-SNAME-VAL-COUNT
                END-IF

           END-PERFORM.

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > AD-ADDR1L)

                MOVE AD-ADDR1(COUNTER:1)
                TO WS-CHAR

                IF (WS-CHAR-VALID) THEN
                    ADD 1 TO WS-ADDR1-COUNT
                END-IF

           END-PERFORM.

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > AD-ADDR2L)

                MOVE AD-ADDR2(COUNTER:1)
                TO WS-CHAR

                IF (WS-CHAR-VALID) THEN
                    ADD 1 TO WS-ADDR2-COUNT
                END-IF

           END-PERFORM.

           PERFORM VARYING COUNTER
                    FROM 1 BY 1
                    UNTIL (COUNTER > AD-TITLEL)

                MOVE AD-TITLE(COUNTER:1)
                TO WS-ALPHANUM

                IF (WS-ALPHANUM NOT = SPACE) THEN
                    ADD 1 TO WS-TITLE-COUNT
                END-IF

           END-PERFORM.


           MOVE AD-FNAME(1:1) TO WS-FNAME.
           MOVE AD-SNAME(1:1) TO WS-SNAME.
           MOVE AD-CRLIMIT TO WS-CRLIMIT.

           IF AD-ACCTNOL < 5 THEN

                MOVE 'ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG'
                TO AD-MESSAGE

                MOVE -1 TO AD-ACCTNOL

           ELSE IF AD-ACCTNO IS NOT NUMERIC

                MOVE 'ACCOUNT NUMBER MUST BE NUMERIC'
                TO AD-MESSAGE

                MOVE -1 TO AD-ACCTNOL

           ELSE IF WS-TITLE-COUNT > 0 AND
                (AD-TITLE NOT = 'M' AND
                AD-TITLE NOT = 'MS' AND
                AD-TITLE NOT = 'MR' AND
                AD-TITLE NOT = 'MRS' AND
                AD-TITLE NOT = 'DR' AND
                AD-TITLE NOT = 'CPTN') THEN

                MOVE
                'TITLE SHOULD EITHER EMPTY OR: M, MR, MRS, DR, CPTN'
                TO AD-MESSAGE

                MOVE -1 TO AD-TITLEL

           ELSE IF AD-FNAMEL = 0 THEN

                MOVE 'FIRST NAME SHOULD NOT BE EMPTY'
                TO AD-MESSAGE

                MOVE -1 TO AD-FNAMEL

           ELSE IF (NOT WS-FNAME-VALID) THEN

                MOVE 'FIRST NAMES FIRST CHARACTER MUST BE LETTER'
                TO AD-MESSAGE

                MOVE -1 TO AD-FNAMEL

           ELSE IF WS-FNAME-VAL-COUNT < 4 THEN

                MOVE 'FIRST NAME SHOULD HAVE ATLEAST 4 LETTERS '
                TO AD-MESSAGE

                MOVE -1 TO AD-FNAMEL

           ELSE IF AD-SNAMEL = 0 THEN

                MOVE 'SURNAME SHOULD NOT BE EMPTY'
                TO AD-MESSAGE

                MOVE -1 TO AD-SNAMEL

           ELSE IF (NOT WS-SNAME-VALID) THEN

                MOVE 'SURNAMES FIRST CHARACTER MUST BE LETTER'
                TO AD-MESSAGE

                MOVE -1 TO AD-SNAMEL

           ELSE IF WS-SNAME-VAL-COUNT < 4 THEN

                MOVE 'SURNAME SHOULD HAVE ATLEAST 4 LETTERS'
                TO AD-MESSAGE

                MOVE -1 TO AD-SNAMEL

           ELSE IF AD-ADDR1 EQUAL LOW-VALUES OR
                    AD-ADDR1 EQUAL SPACES THEN

                MOVE 'ADDRESS 1 SHOULD NOT BE EMPTY'
                TO AD-MESSAGE

                MOVE -1 TO AD-ADDR1L

           ELSE IF WS-ADDR1-COUNT < 4 THEN

                MOVE 'ADDRESS 1 SHOULD HAVE 4 LETTERS'
                TO AD-MESSAGE

                MOVE -1 TO AD-ADDR1L

           ELSE IF AD-ADDR2 EQUAL LOW-VALUES OR
                    AD-ADDR2 EQUAL SPACES THEN

                MOVE 'ADDRESS 2 SHOULD NOT EMPTY'
                TO AD-MESSAGE

                MOVE -1 TO AD-ADDR2L

           ELSE IF WS-ADDR2-COUNT < 4 THEN

                MOVE 'ADDRESS 2 SHOULD HAVE 4 LETTERS'
                TO AD-MESSAGE

                MOVE -1 TO AD-ADDR2L

           ELSE IF AD-STAT NOT = 'A' AND
                AD-STAT NOT = 'B' AND
                AD-STAT NOT = 'X' AND
                AD-STAT NOT = 'Z' THEN

                MOVE 'STATUS MUST BE EITHER A, B, X OR Z'
                TO AD-MESSAGE

                MOVE -1 TO AD-STATL

           ELSE IF AD-CRLIMIT EQUAL LOW-VALUES OR
                    AD-CRLIMIT EQUAL SPACES THEN

                MOVE 'LIMIT MUST BE ENTERED'
                TO AD-MESSAGE

                MOVE -1 TO AD-CRLIMITL

           ELSE IF AD-CRLIMIT IS NOT NUMERIC THEN

                MOVE 'LIMIT MUST BE NUMERIC'
                TO AD-MESSAGE

                MOVE -1 TO AD-CRLIMITL

           ELSE IF AD-STAT IS EQUAL TO 'A' THEN
                IF (WS-CRLIMIT < 5000 OR
                WS-CRLIMIT > 99999999) THEN

                    MOVE 'STATUS A: RANGE BETWEEN 5000 & 99999999'
                    TO AD-MESSAGE

                    MOVE -1 TO AD-CRLIMITL


                END-IF

           ELSE IF AD-STAT IS EQUAL TO 'B' THEN
                IF (WS-CRLIMIT < 1000 OR
                    WS-CRLIMIT > 1000000) THEN

                    MOVE 'STATUS B: RANGE BETWEEN 1000 & 1000000'
                    TO AD-MESSAGE

                    MOVE -1 TO AD-CRLIMITL
                END-IF

           ELSE IF AD-STAT IS EQUAL TO 'X' THEN
                IF (WS-CRLIMIT < 100000 OR
                    WS-CRLIMIT > 99999999) THEN

                    MOVE
                    'STATUS X: RANGE BETWEEN 100000 & 99999999'
                    TO AD-MESSAGE

                    MOVE -1 TO AD-CRLIMITL
                END-IF

           ELSE IF AD-STAT IS EQUAL TO 'Z' THEN
                IF (WS-CRLIMIT < 100 OR
                    WS-CRLIMIT > 4999) THEN

                    MOVE 'STATUS Z: RANGE BETWEEN 100 & 4999'
                    TO AD-MESSAGE

                    MOVE -1 TO AD-CRLIMITL

                END-IF


           ELSE
                MOVE LOW-VALUES TO AD-MESSAGE
                            END-IF
                            END-IF
                            END-IF
                        END-IF
                        END-IF
                        END-IF
                        END-IF
                        END-IF
                        END-IF
                    END-IF
                    END-IF
                    END-IF
                    END-IF
                END-IF
                END-IF
                END-IF
           END-IF
           END-IF
           END-IF
           END-IF.

           MOVE WS-ACCTDATA TO DFHCOMMAREA.
           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DPGE.
