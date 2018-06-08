       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGM.
       AUTHOR. Amar Al-Adil.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY 'DCI8DMM'.

       01 WS-COMMAREA           PIC X(3)
           VALUE SPACES.

       01 WS-COMMAREA-LENGTH    PIC S9(4) COMP
           VALUE 3.


       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF EIBCALEN = 3 THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
           END-EXEC.

           EXEC CICS HANDLE AID
                PF1(300-CHOICE-1)
                PF2(400-CHOICE-2)
                PF3(500-CHOICE-3)
                PF4(600-CHOICE-4)
                PF9(700-CHOICE-9)
           END-EXEC.


           EXEC CICS RECEIVE
                MAP('MENU')
                MAPSET('DCI8DMM')
           END-EXEC.


           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MENUO.
           EXEC CICS SEND
               MAP('MENU')
               MAPSET('DCI8DMM')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D0')
           END-EXEC.

       200-MAIN-LOGIC.

           IF CHOICEI = LOW-VALUES OR CHOICEI = SPACES THEN

                MOVE LOW-VALUES TO MENUO
                MOVE 'YOU MUST ENTER A NUMBER' TO MSGO
                EXEC CICS SEND MAP('MENU') MAPSET('DCI8DMM') END-EXEC
                EXEC CICS RETURN TRANSID('I8D0') END-EXEC

           ELSE IF CHOICEI IS NOT NUMERIC
                MOVE LOW-VALUES TO MENUO
                MOVE 'YOU ENTERED NON NUMERIC CHOICE' TO MSGO
                EXEC CICS SEND MAP('MENU') MAPSET('DCI8DMM') END-EXEC
                EXEC CICS RETURN TRANSID('I8D0') END-EXEC

           ELSE IF CHOICEI IS EQUAL TO '1'
                GO TO 300-CHOICE-1
           ELSE IF CHOICEI IS EQUAL TO '2'
                GO TO 400-CHOICE-2
           ELSE IF CHOICEI IS EQUAL TO '3'
                GO TO 500-CHOICE-3
           ELSE IF CHOICEI IS EQUAL TO '4'
                GO TO 600-CHOICE-4
           ELSE IF CHOICEI IS EQUAL TO '9'
                GO TO 700-CHOICE-9
           ELSE
                GO TO 999-SEND-ERROR-MSG
                                    END-IF
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                END-IF
           END-IF.

       300-CHOICE-1.

           MOVE LOW-VALUES TO MENUO.
           EXEC CICS XCTL
                PROGRAM('DCI8DPGC')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       400-CHOICE-2.

           MOVE LOW-VALUES TO MENUO.
           EXEC CICS XCTL
                PROGRAM('DCI8DPGR')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       500-CHOICE-3.
      *    same as above, different msg
           MOVE LOW-VALUES TO MENUO.
           EXEC CICS XCTL
                PROGRAM('DCI8DPGU')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       600-CHOICE-4.
      *    same as above, different msg
           MOVE LOW-VALUES TO MENUO.
           EXEC CICS XCTL
                PROGRAM('DCI8DPGB')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       700-CHOICE-9.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'YOU ENTERED 9 - PROGRAM ENDING' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCI8DMM') END-EXEC.
           EXEC CICS RETURN END-EXEC.

       999-SEND-ERROR-MSG.

           MOVE LOW-VALUES TO MENUO.
           MOVE 'YOU ENTERED NONE OF THE CHOICES' TO MSGO.
           EXEC CICS SEND MAP('MENU') MAPSET('DCI8DMM') END-EXEC.
           EXEC CICS RETURN TRANSID('I8D0') END-EXEC.

       999-EXIT.

           EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.
           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DPGM.
