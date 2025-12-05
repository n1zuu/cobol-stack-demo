      ******************************************************************
      * Author: Vince Jevy B. Tapdasan
      * Date: November 30, 2025
      * Purpose: To demonstrate the Stack data structure in COBOL using
      *          standard arrays (non-dynamic).
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACK-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-CHOICE PIC X(2) VALUE SPACES.
       77 WS-CONTINUE PIC X VALUE 'Y'.
       77 WS-INPUT-DATA PIC X(20).
       77 WS-ENTER PIC X.

       *> Stack variables
       77 STACK-MAX PIC 9(3) VALUE 10.
       77 STACK-TOP PIC 9(3) VALUE 0.
       01 STACK-ARR.
          05 STACK-ELEM OCCURS 10 TIMES PIC X(20).

       77 WS-STATUS PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-CONTINUE = 'N'
               PERFORM DISPLAY-MENU
               PERFORM GET-CHOICE
               PERFORM PROCESS-CHOICE

               IF WS-CONTINUE NOT = 'N'
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                       WITH NO ADVANCING
                   ACCEPT WS-ENTER
                   DISPLAY " "
               END-IF
           END-PERFORM

           DISPLAY "Thank you for using the Stack program!"
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "================================"
           DISPLAY "    STACK OPERATIONS MENU"
           DISPLAY "================================"
           DISPLAY "1. PUSH - Add element to stack"
           DISPLAY "2. POP  - Remove element from stack"
           DISPLAY "3. PEEK - View top element"
           DISPLAY "4. EXIT - Quit program"
           DISPLAY "================================"
           DISPLAY "Enter your choice (1-4): " WITH NO ADVANCING.

       GET-CHOICE.
           ACCEPT WS-CHOICE.

       PROCESS-CHOICE.
           EVALUATE WS-CHOICE
               WHEN "1"
                   PERFORM PUSH-OPERATION
               WHEN "2"
                   PERFORM POP-OPERATION
               WHEN "3"
                   PERFORM PEEK-OPERATION
               WHEN "4"
                   DISPLAY " "
                   DISPLAY "Exiting program..."
                   MOVE 'N' TO WS-CONTINUE
                   EXIT PARAGRAPH
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "Invalid choice! Please select 1-4."
           END-EVALUATE.

       PUSH-OPERATION.
           DISPLAY " "
           DISPLAY "Enter data to push (max 20 chars): "
               WITH NO ADVANCING
           ACCEPT WS-INPUT-DATA

           PERFORM DO-PUSH

           DISPLAY "Status: " WS-STATUS
           IF WS-STATUS = "OK"
               DISPLAY "Successfully pushed: " WS-INPUT-DATA
           END-IF.

       POP-OPERATION.
           DISPLAY " "
           PERFORM DO-POP

           DISPLAY "Status: " WS-STATUS
           IF WS-STATUS = "OK"
               DISPLAY "Popped element: " WS-INPUT-DATA
           END-IF.

       PEEK-OPERATION.
           DISPLAY " "
           PERFORM DO-PEEK

           DISPLAY "Status: " WS-STATUS
           IF WS-STATUS = "OK"
               DISPLAY "Top element: " WS-INPUT-DATA
           END-IF.

       STACK-SECTION SECTION.
       DO-PUSH.
           IF STACK-TOP = STACK-MAX
               MOVE "STACK FULL" TO WS-STATUS
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO STACK-TOP
           MOVE WS-INPUT-DATA TO STACK-ELEM(STACK-TOP)
           MOVE "OK" TO WS-STATUS
           .

       DO-POP.
           IF STACK-TOP = 0
               MOVE "STACK EMPTY" TO WS-STATUS
               MOVE SPACES TO WS-INPUT-DATA
               EXIT PARAGRAPH
           END-IF

           MOVE STACK-ELEM(STACK-TOP) TO WS-INPUT-DATA
           SUBTRACT 1 FROM STACK-TOP
           MOVE "OK" TO WS-STATUS
           .

       DO-PEEK.
           IF STACK-TOP = 0
               MOVE "STACK EMPTY" TO WS-STATUS
               MOVE SPACES TO WS-INPUT-DATA
               EXIT PARAGRAPH
           END-IF

           MOVE STACK-ELEM(STACK-TOP) TO WS-INPUT-DATA
           MOVE "OK" TO WS-STATUS
           .
