      $set sourceformat"free"

      *>----Divisão de identificação do programa
       identification division.
       program-id. "lista11ex3indexado".
       author. "Lourieni Gonçalves".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.



      *>----Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.

           select arqAluno assign to "arqAlunoIndexed.dat"
           organization is indexed
           access mode is dynamic
           lock mode is automatic
           record key is fd-cod
           file status is ws-fs-arqAluno.

       i-o-control.

      *>----Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqAluno.
       01 fd-alunos.
           05  fd-aluno                            pic x(25).
           05  fd-cod                              pic 9(03).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-notas.
               10  fd-nota1                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  fd-nota2                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  fd-nota3                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  fd-nota4                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".



      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqAluno                          pic  9(02).

       01  ws-alunos.
           05  ws-aluno                            pic x(25).
           05  ws-cod                              pic 9(03).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-notas.
               10  ws-nota1                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  ws-nota2                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  ws-nota3                       pic 9(02)v99.
               10 filler                          pic x(01) value "-".
               10  ws-nota4                       pic 9(02)v99.

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).




       77 ws-sair                                  pic  x(01).
          88  ws-fechar-programa                   value "N" "n".
          88  ws-voltar-tela                       value "V" "v".

       77  ws-menu                                 pic  x(02).
       77  ws-menu-cad                             pic  x(02).


      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

      *>Declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.

           open i-o arqAluno  *> 'open i-o' abre o arquivo para leitura e escrita
           if ws-fs-arqAluno  <> 00
           and ws-fs-arqAluno <> 05 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqAluno                   to ws-msn-erro-cod
               move "Erro ao abrir arq. arqAluno"    to ws-msn-erro-text
               perform finaliza-anormal
           end-if
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until ws-fechar-programa
               move space to ws-sair
               display "Digite 'Ca' p/ cadastrar aluno "
               display "Digite 'Cn' p/ cadastrar nota"
               display "Digite 'Ci' p/ consulta indexada"
               display "Digite 'Cs' p/ consulta sequencial"
               display "Digite 'Dc' p/ deletar cadastro"
               display "Digite 'Ac' p/ alterar cadastro"
               display "Digite 'An' p/ alterar notas"

               accept ws-menu

               evaluate ws-menu
                   when = "Ca"
                       perform cadastrar-aluno
                   when = "Cn"
                       perform cadastrar-notas

                   when = "Ci"
                       perform consultar-cadastro

                   when = "Cs"
                       perform consultar-cad-sequencial-next


                   when = "Dc"
                       perform deletar-cadastro

                    when = "Ac"
                       perform alterar-cadastro

                    when = "An"
                       perform alterar-notas

                   when other
                       display "opcao invalida"
               end-evaluate


           end-perform


           .
       processamento-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  Rotina de cadastro de alunos / escreve no arquivo
      *>------------------------------------------------------------------------
       cadastrar-aluno section.

           perform until ws-voltar-tela
               display "Cod     :"
               accept  ws-cod

               display "Nome    :"
               accept  ws-aluno

               display "Endereco:"
               accept  ws-endereco

               display "Mae     :"
               accept  ws-mae

               display "Pai     :"
               accept  ws-pai

               display "Telefone:"
               accept  ws-telefone


      *>------------------------------------------------------------------
      *>   Salvar dados no arquivo
      *>------------------------------------------------------------------


               write fd-alunos      from ws-alunos


               if ws-fs-arqAluno <> 0 then
                   move 7                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao escrever arq. arqTemp "    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if


               display "Deseja cadastrar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair


           end-perform


           .
       cadastrar-aluno-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   Rotina de cadastro de notas
      *>------------------------------------------------------------------------
       cadastrar-notas section.
           perform until ws-voltar-tela

               display "Informe o codigo do aluno:"
               accept  ws-cod

               display "Informe a primeira nota  :"
               accept  ws-nota1

               display "Informe a segunda nota   :"
               accept  ws-nota2

               display "Informe a terceira nota  :"
               accept  ws-nota3

               display "Informe a quarta nota    :"
               accept  ws-nota4



      *>------------------------------------------------------------------
      *>   Salvar dados no arquivo
      *>------------------------------------------------------------------


               move ws-cod       to fd-cod

               read arqAluno

               move ws-notas     to fd-notas

               rewrite fd-alunos

               if ws-fs-arqAluno <> 0 then
                   move 7                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao escrever arq. arqAluno"    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if


               display "Deseja cadastrar mais uma nota? 'S' ou 'V'oltar"
               accept ws-sair


           end-perform


           .
       cadastrar-notas-exit.
           exit.



      *>------------------------------------------------------------------------
      *>   Rotina de consulta de alunos - lê o arquivo de forma indexada
      *>------------------------------------------------------------------------
       consultar-cadastro section.


           display "informe o codigo do aluno"
           accept ws-cod

           move ws-cod   to fd-cod
           read arqAluno
           if  ws-fs-arqAluno   <> 0
           and ws-fs-arqaluno   <> 10 then
               if ws-fs-arqAluno = 23 then
                   display "Aluno não cadastrado"
               else
                   move 2                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao ler arq. arqAluno"         to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if

           move  fd-alunos   to  ws-alunos

           display "Cod     :"   ws-cod

           display "Nome    :"   ws-aluno

           display "Endereco:"   ws-endereco

           display "Mae     :"   ws-mae

           display "Pai     :"   ws-pai

           display "Telefone:"   ws-telefone

           display "Notas   :"   ws-notas




           .
       consultar-cadastro-exit.
           exit.
      *>------------------------------------------------------------------------
      *>   Rotina de consulta de alunos  - lê o arquivo de forma sequencial
      *>------------------------------------------------------------------------
       consultar-cad-sequencial-prev section.


           perform until ws-voltar-tela

               read arqAluno previous
               if  ws-fs-arqAluno <> 0  then
                  if ws-fs-arqAluno = 10 then
                      perform consultar-cad-sequencial-next
                  else
                      move 4                                   to ws-msn-erro-ofsset
                      move ws-fs-arqAluno                      to ws-msn-erro-cod
                      move "Erro ao ler arq. arqAluno "        to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move  fd-alunos       to  ws-alunos

               display "Cod     :"   ws-cod

               display "Nome    :"   ws-aluno

               display "Endereco:"   ws-endereco

               display "Mae     :"   ws-mae

               display "Pai     :"   ws-pai

               display "Telefone:"   ws-telefone

               display "Notas   :"   ws-notas


               display "Deseja consultar mais um aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform


           .
       consultar-cad-seq-prev-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   Rotina de consulta de aluno lendo o arquivo de forma sequencial
      *>------------------------------------------------------------------------
       consultar-cad-sequencial-next section.

           perform consultar-cadastro

           perform until ws-voltar-tela

               read arqAluno next
               if  ws-fs-arqAluno <> 0  then
                   if ws-fs-arqAluno = 10 then
                       perform consultar-cad-sequencial-prev
                   else
                       move 3                                   to ws-msn-erro-ofsset
                       move ws-fs-arqAluno                      to ws-msn-erro-cod
                       move "Erro ao ler arq. arqAluno"         to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if

               if   ws-sair <> "v"
               and  ws-sair <> "V" then
                   move  fd-alunos     to  ws-alunos


                   display "Cod     :"   ws-cod

                   display "Nome    :"   ws-aluno

                   display "Endereco:"   ws-endereco

                   display "Mae     :"   ws-mae

                   display "Pai     :"   ws-pai

                   display "Telefone:"   ws-telefone

                   display "Notas   :"   ws-notas

                   display "Deseja consultar mais um aluno? 'S' ou 'V'oltar"
                   accept ws-sair

               end-if



           end-perform


           .
       consultar-cad-seq-next-exit.
           exit.

      *>------------------------------------------------------------------------
      *>   Rotina de apagar dados do registro do arquivo
      *>------------------------------------------------------------------------
       deletar-cadastro section.

           display "informe o codigo do aluno"
           accept ws-cod

           move ws-cod to fd-cod
           delete arqAluno
           if  ws-fs-arqAluno = 0 then
               display "Aluno " ws-aluno  " removido com sucesso!"
           else
               if ws-fs-arqAluno = 23 then
                   display "Aluno informado nao esta cadastrado!"
               else
                   move 5                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao apagar arq. arqTemp "      to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           end-if
           .
       deletar-cadastro-exit.
           exit.


      *>------------------------------------------------------------------------
      *>   Rotina de alteração de dados do registro do arquivo
      *>------------------------------------------------------------------------
       alterar-cadastro section.
           perform until ws-voltar-tela
               display erase
               display "Informe o codigo do aluno:"
               accept  ws-cod
               display "O que voce deseja alterar?"
               display "Digite '1' p/ alterar o nome"
               display "Digite '2' p/ alterar o endereco"
               display "Digite '3' p/ alterar o nome da mae"
               display "Digite '4' p/ alterar o nome do pai"
               display "Digite '5' p/ alterar o telefone"

               accept ws-menu-cad

               move ws-cod   to fd-cod

               read arqAluno

               if   ws-menu-cad = 1 then

                   display "Nome       :"
                   accept ws-aluno
                   move   ws-aluno     to fd-aluno

               end-if

               if   ws-menu-cad = 2 then

                   display "Endereco   :"
                   accept ws-endereco
                   move   ws-endereco  to fd-endereco

               end-if

               if   ws-menu-cad = 3 then

                   display "Nome da Mae:"
                   accept ws-mae
                   move   ws-mae       to fd-mae

               end-if

               if   ws-menu-cad = 4 then

                   display "Nome do Pai:"
                   accept ws-pai
                   move   ws-pai       to fd-pai

               end-if

               if   ws-menu-cad = 5 then


                   display "Telefone  :"
                   accept ws-telefone
                   move   ws-telefone  to fd-telefone

               end-if

               rewrite fd-alunos

               if  ws-fs-arqAluno = 0 then
                   display "Dado do aluno " ws-aluno " alterado com sucesso!"
               else
                   move 6                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao alterar arq. arqAluno"     to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

               display "Deseja alterar mais um aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform
           .
       alterar-cadastro-exit.
       exit.

      *>------------------------------------------------------------------------
      *>   Rotina de alteração de dados do registro do arquivo
      *>------------------------------------------------------------------------
       alterar-notas section.
           perform until ws-voltar-tela

               display "Informe o codigo do aluno:"
               accept  ws-cod
               display "Informe o numero da nota que voce deseja alterar:"
               display "Digite '1' p/ alterar a nota1"
               display "Digite '2' p/ alterar a nota2"
               display "Digite '3' p/ alterar a nota3"
               display "Digite '4' p/ alterar a nota4"

               accept ws-menu-cad

               move ws-cod   to fd-cod

               read arqAluno

               if   ws-menu-cad = 1 then

                   display "Nota1:"
                   accept ws-nota1
                   move   ws-nota1     to fd-nota1

               end-if

               if   ws-menu-cad = 2 then

                   display "Nota2:"
                   accept ws-nota2
                   move   ws-nota2     to fd-nota2

               end-if

               if   ws-menu-cad = 3 then

                   display "Nota3:"
                   accept ws-nota3
                   move   ws-nota3     to fd-nota3

               end-if

               if   ws-menu-cad = 4 then

                   display "Nota4:"
                   accept ws-nota4
                   move   ws-nota4     to fd-nota4

               end-if


               rewrite fd-alunos

               if  ws-fs-arqAluno = 0 then
                   display "Nota do aluno " ws-aluno " alterada com sucesso!"
               else
                   move 6                                   to ws-msn-erro-ofsset
                   move ws-fs-arqAluno                      to ws-msn-erro-cod
                   move "Erro ao alterar arq. arqAluno"     to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
               display "Deseja alterar mais uma nota? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform
           .
       alterar-notas-exit.
       exit.





      *>------------------------------------------------------------------------
      *>   Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.

           display erase
           display ws-msn-erro.
           stop run
           .
       finaliza-anormal-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqAluno
           if ws-fs-arqAluno <> 0 then
               move 8                                to ws-msn-erro-ofsset
               move ws-fs-arqAluno                   to ws-msn-erro-cod
               move "Erro ao fechar arq. arqAluno "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           stop run
           .
       finaliza-exit.
           exit.













