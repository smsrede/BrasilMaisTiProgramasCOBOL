      *================================================================* 
       IDENTIFICATION                               DIVISION.
      ******************************************************************
       PROGRAM-ID.                                    PRIMEIRO-PROGRAMA.
       AUTHOR.     SAMUEL MENEZES.
      ******************************************************************
      *>   DIVISOES E SESSOES ENTRE A COLUNA 8 E 11  ÁREA AxSAMUEL.SA <*
      *>   não tem sessões                                            <*
      *>   DATA: 01/02/2017                                           <*   
      *>   OBJETVO: ESSE PROGRM RECEBE UM VALOR E IMPRIM NA TELA      <* 
      *>                                                              <*
      ******************************************************************
      ************************FIM DOCUMENTACAO**************************
      *================================================================*
       ENVIRONMENT                                  DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
      *QUALIFICARA MAPEAR EQUIPAMENTOS E ARQUIVOS E TEM DUAS SESSÕES 
         CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *        SOURCE-COMPUTER.
      *        OBJECT-COMPUTER.
               SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*     
         INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*     
               FILE-CONTROL.
      *             SELECT XPT0 ASSIGN TO XYZ.
      *         I-O-CONTROL.             
      *================================================================*  
       DATA                                         DIVISION.
      *    VARIAVEL CONTANTES E TUDO MAIS SOBRE DADOS 
      *================================================================*
      *----------------------------------------------------------------* 
         FILE                             SECTION.
      *----------------------------------------------------------------*     
         WORKING-STORAGE                  SECTION.
      *    VARIAVEIS
      *    Estrutura: <Nivel de variavel> < identificador> <tidpo (tamanho)>
      *    NIVEL: 77 (PARA COMEÇAR)
      *    ID: 30 CARAC MAX 33, SEM ESPACO, LETRA A-Z, NUMERO 0-9, "-" 
      *    MAS O "-" NÃO COMECA OU TERMINA
      *    NAO PODE SER PALAVRA RESERVADA
      *    PRECISA DE AO MENOS UM CARAC ALFABETICO
      *    TIPOS: ALFABETICOS, NUMERICOS E ALFANUMERICOS
      *    PIC OU PICTURE - PALAVRAAS QUE DEFIENM O TIPO E TAMANHO DA VARIAVEL, CAMPO
      *    PARA DEFINIR A VARIÁVEL COMO NUMERICA USE O NUMERO 9 APOS - PIC AO MAXIMO DE 18 DIGITOS DE TAMANHO     
      *    aLFABETICA REPRESENTADO PELA LETRA A
      *     ALFANUMERICO REPRESENTADO PELA LETRA X
            
      *    COM VARIAVEIS COMPOSTAS VAMOS CRIAR REGISTROS  
      *----------------------------------------------------------------*      
           77 WRK-NOME      PIC X(25) VALUE 'SAMUEL MENEZES DE SOUZA'.
      *         'DE SOUZA'.
       
           77 NUMEROX                       PIC 9(18).
           77 NUMEROX-SINAL                 PIC S9(18).
      *    PARA A VARIAVEL APRESENTAR SINAL (COMO NEGATIVOS POR EXEMPLO)
           77 NUMEROX-DECIMAL               PIC 9(02)V9(02).
      *    FLOAT, DOUBLE 10,05 DUAS CASAS ANTES E DEPOIS DA VIRGULA 
      *    OU PIC 9(02)V9(02). OU PIC 9(05)V99. OU PIC 999V99. OU 9(07)V99.

           77 NOMEX                         PIC A(20).
           77 NOMENUMERO                    PIC X(30).
      *    OS NIVEIS DEFIINEM CAMPOS DE REGISTROS OU AREAS AUXILIARES
      *    01, 66, 77, 78, 88
      *    SOMENTE OS NIVEIS 01 E 77 SAO ACEITAVEIS NA COL A 8-11
      *    01 - NIVEL DE GRUPO - USADO PARA REGISTRO, SAO ALFA NUMERICOS
      *    DENTRO DOS ITENS DE GRUPO TEMOS ITENS ELEMENTARES QUE PODEM
      *    RECEBER VALORES DE 02 A 49
      *    CLIENTE EH UM ITEM DE GRUPO E O QUE ESTA DENTRO SAO ITENS
      *    SAO ITENS ELEMENTARES
      *    CLIENTE EH ALFANUMERICO MAS OS INTERNOS PEDEM ID DE TIPO
      *    PARA INICIAR VAR NO COBOL - VALUE -
      *    QUANDO O VALOR E DO TIPO NUMERICO NÃO FICA ENTRE ASPAS 
       01 ALUNO.
               03 NOME-ALUNO.
                   05 PRIMEIRO-NOME       PIC X(30) VALUE SPACES.
                   05 SOBRENOME           PIC X(30) VALUE SPACES.  
               03 RG       PIC X(11) VALUE SPACES.
               03 CPF      PIC 9(11) VALUE 11111111111.
               03 DATANASC.
                   05 DIA PIC 9(02).
                   05 MES PIC 9(02).
                   05 ANO PIC 9(04).
               03 SALARIO      PIC 9(10)V99.
               03 SALARIO-EDIT PIC Z.ZZZ.ZZZ.ZZ9,99.
               03 ENDERECO PIC X(50) VALUE SPACES.
               03 DATANASC-EDTI PIC 99/99/9999.
               77 SENHA PIC X(6).
               77 CEP PIC   9(8).
               77 NOME PIC  A(20) VALUE SPACES.
       
      *    COMO OS CAMPOS SAO ALFANUMERICOS NAO É PRECISO DECLARAR TIPO
      *    OS NIVEIS VAO DE 01 - 99 MAS SO PODEMOS USAR COMO ITENS
      *    ELEMENTARES 02 - 49
      *    OS NIVEIS ESPECIFICOS VAO DE 50 A 99
      *    77 - VARIAVEIS AUX INDEPENDENTES NUNCA ITEM DE GRUPO     
           
           
      *----------------------------------------------------------------*
      *  LINKAGE                          SECTION.
      *   VARIAVEIS E COMUNIDACAO ENTRE PROGRAMAS OU ACESSO A MODULO
      *----------------------------------------------------------------*
      *  SCREEN                           SECTION.
      *   DEFINICOES DE TELAS
      *----------------------------------------------------------------* 
      *================================================================* 
       PROCEDURE                                    DIVISION.
      *    IMPLEMENTAMOS A LOGICA
      *    TEM SECTIONS, MAS, NÓS É QUE VAMOS CRIAR
      *    CRIAREMOS AS ROTINAS. COBOL É PROCEDURAL     
      *================================================================* 
      *  MAIN-PROCEDURE.
      *A..AB                                                           B   
123456*89012345678901234567890123456789012345678901234567890123456789012(73)
      *AREA A: INICIO DE DIV SEC PARAGR F
      *    AREA B: QUASE TODOS OS COMANDO COBOL E NIVEIS DE VARIAVEL
      *                                                                 3456789(80) - CODIGO DE INTERPRETACAO DO PROGRAMA NAO INTERPRETADO PELO CObol
      *        DISPLAY 'Hello world'.
      *         DISPLAY 'Hello world 2'.
      *         DISPLAY WRK-NOME.
               
      *        DISPLAY NOME.
      *        DISPLAY CLIENTE.
      *        DISPLAY DATANASC.
       
       FIM.
               DISPLAY "OLA ALUNOS" AT 0435.
               DISPLAY "DIGITE O NOME DO ALUNO" AT 0701.
               ACCEPT NOME-ALUNO AT 0901.
               DISPLAY NOME-ALUNO AT 1001.
               DISPLAY "DIGITE A DATA" AT 1101.
               ACCEPT DATANASC  AT 1201.
               MOVE DATANASC TO DATANASC-EDTI.
               DISPLAY DATANASC-EDTI  AT 1401.
               DISPLAY "DIGITE O DATA EDITADA C MASCARA"  AT 1501.
               ACCEPT DATANASC-EDTI at 1601.
               DISPLAY DATANASC-EDTI 1701.
               DISPLAY "DIGITE O SALARIO"  AT 1801.
               ACCEPT SALARIO  AT 1901.
               DISPLAY SALARIO  AT 2001.
               DISPLAY "DIGITE SALARIO C MASCARA"  AT 2101.
               ACCEPT SALARIO-EDIT  AT 2201.
               DISPLAY SALARIO-EDIT  AT 2301.
               display ERASE AT 0101.
               display "TESTE" AT 1111.
               accept NOME-ALUNO at 1010.
               
               DISPLAY "DIGITE SUA SENHA: " AT 4050.
               ACCEPT SENHA WITH NO-ECHO AT 4050.
               
               ACCEPT CEP WITH AUTO-SKIP.
               ACCEPT NOME AT 5050 ON ESCAPE GO TO FIM.
               
               
               STOP RUN.
      *        STOP RUN MUITO USADO EM PROGRAMAS BAT
      *        END PROGRAM                   PRIMEIRO-PROGRAMA.
