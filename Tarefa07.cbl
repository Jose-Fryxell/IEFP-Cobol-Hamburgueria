      ******************************************************************
      * Author: JOSE SERRA
      * Date: 08-03-2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA07.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ****** NUM PEDIDO ************************************************
       77 NUM-PEDIDO                   PIC 99      VALUE 0.
       77 MENU                         PIC 9       VALUE 0.
         88 DESVALIDAR-MENU                           VALUES 7 THRU 8.
      ****** PRECO *****************************************************
       77 PRECO                        PIC 9999V99  VALUE 0.
       77 PRECO-PEQUENO                PIC 999V99  VALUE 0.
       77 PRECO-MEDIO                  PIC 999V99  VALUE 0.
       77 PRECO-GRANDE                 PIC 999V99  VALUE 0.
       77 PRECO-SUPER                  PIC 999V99  VALUE 0.
       77 PRECO-BATATAS                PIC 999V99  VALUE 0.
       77 PRECO-SALADA                 PIC 999V99  VALUE 0.
       77 PRECO-SAIDA                  PIC ZZZ9.99 VALUE SPACES.
      ****** DATA & HORA ***********************************************
       01 DATA-ATUAL.
           05 ANO                      PIC 9999.
           05 MES                      PIC 99.
           05 DIA                      PIC 99.
           05 HORA                     PIC 99.
           05 MINUTOS                  PIC 99.
      ******************************************************************
       77 MENU-PEQUENO                 PIC 99      VALUE 0.
       77 MENU-MEDIO                   PIC 99      VALUE 0.
       77 MENU-GRANDE                  PIC 99      VALUE 0.
       77 MENU-SUPER                   PIC 99      VALUE 0.
       77 MENU-BATATAS                 PIC 99      VALUE 0.
       77 MENU-SALADA                  PIC 99      VALUE 0.
      ******************************************************************
       77 LT                           PIC 99       VALUE 8.
       77 LTPQN                        PIC 99.
       77 LTMD                         PIC 99.
       77 LTGRD                        PIC 99.
       77 LTSPR                        PIC 99.
       77 LTBTT                        PIC 99.
       77 LTSLD                        PIC 99.
      ******************************************************************
       77 LT2                          PIC 99.
       77 LT3                          PIC 99.
       77 LT4                          PIC 99.
       77 LT5                          PIC 99.
      ******************************************************************
       77 REPETIR                      PIC A.
           88 VALIDAR-REPETIR          VALUES "S","s","N","n".
           88 SIM                      VALUES "S","s".
       SCREEN SECTION.
       01 CLS BLANK SCREEN.
      ******************************************************************
       01 JANELA.
      ****** LOGO ******************************************************
           05 LINE 02 COL 02 FOREGROUND-COLOR 6 VALUE
           "  _  _               _                                     "
           & " _           .-""""""""-.".
           05 LINE 03 COL 02 FOREGROUND-COLOR 6 VALUE
           " | || | __ _  _ __  | |__  _  _  _ _  __ _  _  _  ___  _ _ "
           & "(_) __ _    /' .  '. \".
           05 LINE 04 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT VALUE
           " | __ |/ _` || '  \ | '_ \| || || '_|/ _` || || |/ -_)| '_|"
           & "| |/ _` |  (`-:...:.-')".
           05 LINE 05 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT VALUE
           " |_||_|\__,_||_|_|_||_.__/ \_,_||_|  \__, | \_,_|\___||_|  "
           & "|_|\__,_|   ;-......-;".
           05 LINE 06 COL 02 FOREGROUND-COLOR 6 VALUE
           "                                     |___/                 "
           & "             '------'".
      ****** DATA HORA N PEDIDO ****************************************
           05 LINE 03 COL 86 FOREGROUND-COLOR 3 VALUE "Data:".
           05 LINE 04 COL 86 FOREGROUND-COLOR 3 VALUE "Hora:".
           05 LINE 05 COL 86 FOREGROUND-COLOR 3 VALUE "Num. Pedido:".
      ****** MENU ******************************************************
           05 LINE 08 COL 71 FOREGROUND-COLOR 3 VALUE
           "*-N-----------Menus-------Preco-*".
           05 LINE 09 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 1 Hamburguer Pequeno     5.15 |".
           05 LINE 10 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 2 Hamburguer Medio       6.05 |".
           05 LINE 11 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 3 Hamburguer Grande      7.10 |".
           05 LINE 12 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 4 Hamburguer Super       8.20 |".
           05 LINE 13 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 5 Batatas                4.50 |".
           05 LINE 14 COL 71 FOREGROUND-COLOR 3 VALUE
           "| 6 Salada                 5.00 |".
           05 LINE 15 COL 71 FOREGROUND-COLOR 3 VALUE
           "*-------------------------------*".
      ******************************************************************
           05 LINE 08 COL 73 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "N".
           05 LINE 08 COL 85 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "Menus".
           05 LINE 08 COL 97 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "Preco".

           05 LINE 09 COL 73 HIGHLIGHT VALUE "1".
           05 LINE 09 COL 98 HIGHLIGHT VALUE "5.15".
           05 LINE 10 COL 73 HIGHLIGHT VALUE "2".
           05 LINE 10 COL 98 HIGHLIGHT VALUE "6.05".
           05 LINE 11 COL 73 HIGHLIGHT VALUE "3".
           05 LINE 11 COL 98 HIGHLIGHT VALUE "7.10".
           05 LINE 12 COL 73 HIGHLIGHT VALUE "4".
           05 LINE 12 COL 98 HIGHLIGHT VALUE "8.20".
           05 LINE 13 COL 73 HIGHLIGHT VALUE "5".
           05 LINE 13 COL 98 HIGHLIGHT VALUE "4.50".
           05 LINE 14 COL 73 HIGHLIGHT VALUE "6".
           05 LINE 14 COL 98 HIGHLIGHT VALUE "5.00".

      ****** FATURA ****************************************************
           05 LINE 08 COL 03 FOREGROUND-COLOR 3 VALUE
           "*------------------QTD----Preco-*".
           05 LINE 09 COL 03 FOREGROUND-COLOR 3 VALUE
           "| -                 --     -.-- |".
           05 LINE 10 COL 03 FOREGROUND-COLOR 3 VALUE
           "+-------------------------------+".
           05 LINE 11 COL 03 FOREGROUND-COLOR 3 VALUE
           "| Total                    -.-- |".
           05 LINE 12 COL 03 FOREGROUND-COLOR 3 VALUE
           "*-------------------------------*".
      ******************************************************************
           05 LINE 08 COL 22 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "QTD".
           05 LINE 08 COL 29 FOREGROUND-COLOR 3 HIGHLIGHT VALUE "Preco".
           05 LINE 11 COL 05 HIGHLIGHT VALUE "Total".
      ******************************************************************
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY CLS.
           DISPLAY JANELA.
      ****** DATA & HORA ***********************************************
           MOVE FUNCTION CURRENT-DATE TO DATA-ATUAL.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT 0392.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTOS)
           HIGHLIGHT AT 0492.
      ****** RESET *****************************************************
           IF (NUM-PEDIDO = 99) THEN
               MOVE 0 TO NUM-PEDIDO
           END-IF.
           ADD 1 TO NUM-PEDIDO.
           DISPLAY NUM-PEDIDO HIGHLIGHT AT 0599.
           MOVE 0 TO PRECO, MENU-PEQUENO, MENU-MEDIO, MENU-GRANDE,
           MENU-SUPER, MENU-BATATAS, MENU-SALADA.
           MOVE 8 TO LT.
           MOVE 10 TO LT2.
           MOVE 11 TO LT4.
      ******************************************************************
       SOLICITAR.
           DISPLAY "Bem-vindo!" HIGHLIGHT AT 0838.
           DISPLAY "Por favor digite o numero dos"
           FOREGROUND-COLOR 3 HIGHLIGHT AT 1038.
           DISPLAY "menus que deseja pedir:"
           FOREGROUND-COLOR 3 HIGHLIGHT AT 1138.

           ACCEPT MENU HIGHLIGHT AT 1162.
           IF ((MENU = 0 AND PRECO = 0) OR (DESVALIDAR-MENU) OR
               (MENU = 9 AND PRECO = 0)) THEN
               IF (PRECO = 0) THEN
                   DISPLAY "Por favor digite um"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1338
                   DISPLAY "menu valido."
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1438
               END-IF
               IF (PRECO > 0) THEN
                   DISPLAY "Por favor digite um"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1638
                   DISPLAY "menu valido."
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1738
               END-IF
           ELSE
               IF (PRECO > 0) THEN
                   DISPLAY "Digite 0 para terminar."
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                   DISPLAY "Digite 9 para repetir."
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   DISPLAY "                   " AT 1638
                   DISPLAY "            " AT 1738
               END-IF
               IF (PRECO = 0) THEN
                   DISPLAY "                   " AT 1338
                   DISPLAY "            " AT 1438
               END-IF
           END-IF.

           EVALUATE MENU
               WHEN 1
                   IF (LT > 8 AND MENU-PEQUENO = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-PEQUENO = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTPQN = LT
                   END-IF
                   IF (MENU-PEQUENO = 99) THEN
                       MOVE 0 TO MENU-PEQUENO
                       COMPUTE PRECO = PRECO - (99 * 5.15)
                       DISPLAY " " AT LINE LTPQN COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-PEQUENO
                   DISPLAY "Hamburguer Peq."
                   AT LINE LTPQN COL 05 HIGHLIGHT
                   COMPUTE PRECO-PEQUENO = 5.15 * MENU-PEQUENO
                   ADD 5.15 TO PRECO
                   MOVE PRECO-PEQUENO TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTPQN COL 27 HIGHLIGHT
                   DISPLAY MENU-PEQUENO
                   AT LINE LTPQN COL 23 HIGHLIGHT
                   IF (MENU-PEQUENO < 10) THEN
                       DISPLAY "x" AT LINE LTPQN COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTPQN COL 22 HIGHLIGHT
                   END-IF
               WHEN 2
                   IF (LT > 8 AND MENU-MEDIO = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-MEDIO = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTMD = LT
                   END-IF
                   IF (MENU-MEDIO = 99) THEN
                       MOVE 0 TO MENU-MEDIO
                       COMPUTE PRECO = PRECO - (99 * 6.05)
                       DISPLAY " " AT LINE LTMD COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-MEDIO
                   DISPLAY "Hamburguer Med."
                   AT LINE LTMD COL 05 HIGHLIGHT
                   COMPUTE PRECO-MEDIO = 6.05 * MENU-MEDIO
                   ADD 6.05 TO PRECO
                   MOVE PRECO-MEDIO TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTMD COL 27 HIGHLIGHT
                   DISPLAY MENU-MEDIO
                   AT LINE LTMD COL 23 HIGHLIGHT
                   IF (MENU-MEDIO < 10) THEN
                       DISPLAY "x" AT LINE LTMD COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTMD COL 22 HIGHLIGHT
                   END-IF
               WHEN 3
                   IF (LT > 8 AND MENU-GRANDE = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-GRANDE = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTGRD = LT
                   END-IF
                   IF (MENU-GRANDE = 99) THEN
                       MOVE 0 TO MENU-GRANDE
                       COMPUTE PRECO = PRECO - (99 * 7.10)
                       DISPLAY " " AT LINE LTGRD COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-GRANDE
                   DISPLAY "Hamburguer Grd."
                   AT LINE LTGRD COL 05 HIGHLIGHT
                   COMPUTE PRECO-GRANDE = 7.10 * MENU-GRANDE
                   ADD 7.10 TO PRECO
                   MOVE PRECO-GRANDE TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTGRD COL 27 HIGHLIGHT
                   DISPLAY MENU-GRANDE
                   AT LINE LTGRD COL 23 HIGHLIGHT
                   IF (MENU-GRANDE < 10) THEN
                       DISPLAY "x" AT LINE LTGRD COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTGRD COL 22 HIGHLIGHT
                   END-IF
               WHEN 4
                   IF (LT > 8 AND MENU-SUPER = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-SUPER = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTSPR = LT
                   END-IF
                   IF (MENU-SUPER = 99) THEN
                       MOVE 0 TO MENU-SUPER
                       COMPUTE PRECO = PRECO - (99 * 8.20)
                       DISPLAY " " AT LINE LTSPR COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-SUPER
                   DISPLAY "Hamburguer Sup."
                   AT LINE LTSPR COL 05 HIGHLIGHT
                   COMPUTE PRECO-SUPER = 8.20 * MENU-SUPER
                   ADD 8.20 TO PRECO
                   MOVE PRECO-SUPER TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTSPR COL 27 HIGHLIGHT
                   DISPLAY MENU-SUPER
                   AT LINE LTSPR COL 23 HIGHLIGHT
                   IF (MENU-SUPER < 10) THEN
                       DISPLAY "x" AT LINE LTSPR COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTSPR COL 22 HIGHLIGHT
                   END-IF
               WHEN 5
                   IF (LT > 8 AND MENU-BATATAS = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-BATATAS = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTBTT = LT
                   END-IF
                   IF (MENU-BATATAS = 99) THEN
                       MOVE 0 TO MENU-BATATAS
                       COMPUTE PRECO = PRECO - (99 * 4.50)
                       DISPLAY " " AT LINE LTBTT COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-BATATAS
                   DISPLAY "Batatas"
                   AT LINE LTBTT COL 05 HIGHLIGHT
                   COMPUTE PRECO-BATATAS = 4.50 * MENU-BATATAS
                   ADD 4.50 TO PRECO
                   MOVE PRECO-BATATAS TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTBTT COL 27 HIGHLIGHT
                   DISPLAY MENU-BATATAS
                   AT LINE LTBTT COL 23 HIGHLIGHT
                   IF (MENU-BATATAS < 10) THEN
                       DISPLAY "x" AT LINE LTBTT COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTBTT COL 22 HIGHLIGHT
                   END-IF
               WHEN 6
                   IF (LT > 8 AND MENU-SALADA = 0) THEN
                       PERFORM EXTENDER-TABELA
                   END-IF
                   IF (MENU-SALADA = 0) THEN
                       ADD 1 TO LT
                       COMPUTE LTSLD = LT
                   END-IF
                   IF (MENU-SALADA = 99) THEN
                       MOVE 0 TO MENU-SALADA
                       COMPUTE PRECO = PRECO - (99 * 5.00)
                       DISPLAY " " AT LINE LTSLD COL 22
                   END-IF
                   IF (PRECO = 0) THEN
                       DISPLAY "Digite 0 para terminar."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1338
                       DISPLAY "Digite 9 para repetir."
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 1438
                   END-IF
                   ADD 1 TO MENU-SALADA
                   DISPLAY "Salada"
                   AT LINE LTSLD COL 05 HIGHLIGHT
                   COMPUTE PRECO-SALADA = 5.00 * MENU-SALADA
                   ADD 5.00 TO PRECO
                   MOVE PRECO-SALADA TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA
                   AT LINE LTSLD COL 27 HIGHLIGHT
                   DISPLAY MENU-SALADA
                   AT LINE LTSLD COL 23 HIGHLIGHT
                   IF (MENU-SALADA < 10) THEN
                       DISPLAY "x" AT LINE LTSLD COL 23 HIGHLIGHT
                   ELSE
                       DISPLAY "x" AT LINE LTSLD COL 22 HIGHLIGHT
                   END-IF
           END-EVALUATE.

           PERFORM SOLICITAR UNTIL((MENU = 0 AND PRECO > 0) OR
                                   (MENU = 9 AND PRECO > 0)).

           EVALUATE MENU
               WHEN 9
                   SUBTRACT 1 FROM NUM-PEDIDO
                   GO INICIO
               WHEN 0
                   MOVE PRECO TO PRECO-SAIDA
                   DISPLAY PRECO-SAIDA HIGHLIGHT AT LINE LT4 COL 27
           END-EVALUATE.
      ****** LIMPAR ****************************************************
           DISPLAY "                               " AT 1038.
           DISPLAY "                               " AT 1138.
           DISPLAY "                               " AT 1338.
           DISPLAY "                               " AT 1438.


           DISPLAY "Deseja realizar um novo pedido?"
           FOREGROUND-COLOR 3 HIGHLIGHT AT 0838.
           DISPLAY "(S/N)" FOREGROUND-COLOR 3 HIGHLIGHT AT 0938.
           DISPLAY "N" FOREGROUND-COLOR 4 HIGHLIGHT AT 0941.
           DISPLAY "S" FOREGROUND-COLOR 2 HIGHLIGHT AT 0939.
      *    DISPLAY PRECO-SAIDA AT 3001.
       REPETIR-PROGRAMA.
           ACCEPT REPETIR AT 0944 HIGHLIGHT.
           IF (NOT VALIDAR-REPETIR) THEN
               DISPLAY "Por favor digite:"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 1138
               DISPLAY "'S' para 'Sim'"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 1338
               DISPLAY "'N' para 'Nao'"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 1438
               GO REPETIR-PROGRAMA
           END-IF.
           IF (SIM) THEN
               GO INICIO
           END-IF.
           STOP RUN.
       EXTENDER-TABELA.
           COMPUTE LT3 = LT2 + 1.
           COMPUTE LT4 = LT2 + 2.
           COMPUTE LT5 = LT2 + 3.
           DISPLAY "|                               |"
           FOREGROUND-COLOR 3 AT LINE LT2 COL 03.
           DISPLAY "+-------------------------------+"
           FOREGROUND-COLOR 3 AT LINE LT3 COL 03.
           DISPLAY "| Total                    -.-- |"
           FOREGROUND-COLOR 3 AT LINE LT4 COL 03.
           DISPLAY "*-------------------------------*"
           FOREGROUND-COLOR 3 AT LINE LT5 COL 03.
           DISPLAY "Total" HIGHLIGHT AT LINE LT4 COL 05.
           ADD 1 TO LT2.
       END PROGRAM TAREFA07.
