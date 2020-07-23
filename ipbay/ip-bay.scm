;*************************************   
;*                                   *   
;*          Projecto ip-Bay          *   
;*                                   *   
;*************************************   
;
;*************************************
;*            Grupo nº14             *
;*      N50230 Rodrigo Correia       *
;*      N51394 Filipa Andrade        *
;*************************************

(require-library "functio.ss")
;*****************
;*  NOSSA LISTA  *
;*****************
;sendo que a representação escolhida para os tipos de informação foi a lista,
;foram criadas as seguintes funções de manipulação de listas para que, se um 
;dia quisermos mudar a representação teremos apenas de mudar estas funções 
;adaptando-as ao novo tipo
;deve-se ter em conta que o tipo lista já existe no scheme. Para o utilizar 
;é necessária a biblioteca "functio.ss". O que aqui definimos é o tipo escolhido
;como representação interna de todas as estruturas do projecto. Por isso talvez 
;devesse estar dentro do ip-Bay mas optámos por colocar este TAI cá fora pois
;pode ser que um dia o utilizemos noutros programas.

;***************
;* constructor *
;***************
;cria uma lista vazia
(define tipo-vazio (list))

;cria uma lista com os argumentos recebidos
(define (tipo-escolhido . args)
  args)

;*****************
;* modificadores *
;*****************
;insere um elemento numa lista
(define (insere el lst)
  (cons el lst))

;insere um elemento no fim de uma lista
(define (insere-fim el lst)
  (junta-listas lst (list el)))

;junta duas listas
(define (junta-listas l1 l2)
  (if (null? l1)
      l2
      (insere (first l1) (junta-listas (rest l1) l2))))

;cria um par
(define (cria-par a b)
  (cons a b))

;remove um elemento de uma lista
(define (remover el lista)
  (remove el lista))

;modificam os argumentos de uma lista - do segundo ao quinto, que foram os necessários
(define (set-segundo! lst el)
  (set-car! (cdr lst) el))
(define (set-terceiro! lst el)
  (set-car! (rest (rest lst)) el))
(define (set-quarto! lst el)
  (set-car! (cdr (cdr (cdr lst))) el))
(define (set-quinto! lst el)
  (set-car! (cdr (cdr (cdr (cdr lst)))) el))
(define (set-sexto! lst el)
  (set-car! (cdr (cdr (cdr (cdr (cdr lst))))) el))


;**************
;* selectores *
;**************
;devolve o resto de uma lista
(define (resto lst)
  (rest lst))

;seleccionam os argumentos do primeiro ao sétimo de uma lista
(define (primeiro lista)
  (list-ref lista 0))
(define (segundo lista)
  (list-ref lista 1))
(define (terceiro lista)
  (list-ref lista 2))
(define (quarto lista)
  (list-ref lista 3))
(define (quinto lista)
  (list-ref lista 4))
(define (sexto lista)
  (list-ref lista 5))
(define (setimo lista)
  (list-ref lista 6))

;devolve o ultimo elemento da lista
(define (ultimo lst)
  (if (null? lst)
      (display "a lista não tem elementos")
      (let ((ultimo-el (first lst)))
        (if (null? (rest lst))
            ultimo-el
            (ultimo (rest lst))))))


;****************
;* reconhecedor *
;****************
;vê se o comando é do tipo escolhido - lista
(define (tipo-escolhido? arg)
  (list? arg))

;verifica se o argumento é uma lista vazia
(define (tipo-vazio? arg)
  (null? arg))

;*********
;* teste *
;*********
;devolve verdadeiro se as duas listas recebdidas forem iguais
(define (listas=? l1 l2)
  (equal? l1 l2))

;*******************************
;* procedimentos de alto nível *
;*******************************
;devolve o comprimento da lista
(define (comprimento lista)
  (length lista))

;verificam se o comprimento da lista é o desejado
(define (comprimento-1 l)
  (= (comprimento l) 1))

(define (comprimento-2 l)
  (= (comprimento l) 2))

(define (comprimento-3 l)
  (= (comprimento l) 3))

(define (comprimento-4 l)
  (= (comprimento l) 4))

(define (comprimento-5 l)
  (= (comprimento l) 5))

(define (comprimento-6 l)
  (= (comprimento l) 6))

(define (comprimento-7 l)
  (= (comprimento l) 7))

;****************************
;* PROCEDIMENTOS AUXILIARES *
;****************************
;dá o símbolo de elemento uma lista
(define (da-simbolo arg)
  (primeiro (resto arg)))

;verifica se os dois argumentos são iguais
(define (iguais? a b)
  (eq? a b))

;avalia um comando
(define (executa comando)
  (eval comando))

;estes últimos 3 são específicos do ip-bay
;procedimento utilizado para que introduzir os zeros à esquerda
(define (display-zero x)
  (if (<= x 9)
      (begin
        (display "0")
        (display x))
      (display x)))

;procedimento para imprimir aspas
(define (escreve-aspas)
  (display "\""))

;imprime 5 espaços no ecran
(define (escreve-espacos)
  (display "     "))



;**********************************
;* TIPOS ABSTRACTOS DE INFORMAÇÃO *
;**********************************
;estes foram os TAIS implementados para usar no projecto ip-Bay mas que também
;podem ser utilizados noutros programas pois não têm nenhuma dependência do ip-Bay
;todos estes tais utilizam o TAI nossa lista definido acima

;***************
;   TAI HORA   *
;***************
;tipo abstracto de informação que representa a hora
;***************
;* constructor *
;***************
;cria uma hora a partir de dois inteiros válidos para hora e minuto
(define (cria-hora h m)
  (if (hora? (tipo-escolhido h m))
      (tipo-escolhido h m)))

;**************
;* selectores *
;**************
;selecionam partes da estrutura hora
(define (hora h)
  (primeiro h))

(define (minuto h)
  (segundo h))

;****************
;* reconhecedor *
;****************
;devolve verdadeiro se o argumento for do tipo hora
(define (hora? arg)
  (define (ishora? x)
    (and (integer? x)
         (>= x 0)
         (<= x 23)))
  (define (minuto? x)
    (and (integer? x)
         (>= x 0)
         (<= x 59)))
  (and (tipo-escolhido? arg)
       (= 2 (length arg))
       (ishora? (primeiro arg))
       (minuto? (segundo arg))))

;*********
;* teste *
;*********
;devolve verdadeiro se duas horas são iguais
(define (horas=? h1 h2)
  (and (hora? h1)
       (hora? h2)
       (and (= (hora h1)(hora h2))
            (= (minuto h1)(minuto h2)))))

;**************************
;* transformador de saída *
;**************************
;escreve no ecran a hora recebida na forma "hora:min"
(define (escreve-hora h)
  (if (hora? h)
      (begin
        (display-zero (hora h))
        (display ":")
        (display-zero (minuto h)))))


;****************
;    TAI DATA   *
;****************
;tipo abstracto de informação para data
;***************
;* constructor *
;***************
;cria uma data a partir de 3 inteiros válidos para ano, mês e dia
(define (cria-data a m d)
  (if (data? (tipo-escolhido a m d))
      (tipo-escolhido a m d)))

;**************
;* selectores *
;**************
;seleccionam as partes constituintes do tipo data
(define (ano d)
  (primeiro d))

(define (mes d)
  (segundo d))

(define (dia d)
  (terceiro d))

;****************
;* reconhecedor *
;****************
;devolve verdadeiro se o argumento recebido for do tipo data
(define (data? arg)
  (and (tipo-escolhido? arg)
       (comprimento-3 arg)
       (ano? (primeiro arg))
       (mes? (segundo arg))
       (dia? (primeiro arg)(segundo arg)(terceiro arg))))

;verifica se o argumento é um ano
(define (ano? x)
    (and (integer? x)
         (> x 0)))

;verifica se o argumento é um mês
(define (mes? x)
  (and (integer? x)
       (>= x 1)
       (<= x 12)))

;verifica se o argumento é um dia de acordo com o ano e o mês
(define (dia? a m d)
  (and (integer? d)(>= d 1)(<= d 31)
       (cond ((and (= d 29)(= m 2)(bissexto? a)))
             ((and (or (= d 29)(= d 30))(= m 2)) #f)
             ((and (= d 31) (or (= m 2)(= m 4)(= m 6)(= m 9)(= m 11))) #f)
             (else #t))))

;devolve verdadeiro se o ano é bissexto
(define (bissexto? x)
  (or (and (= 0 (remainder x 4))
           (not (= 0 (remainder x 100))))
      (= 0 (remainder x 400))))

;*********
;* teste *
;*********
;devolve verdadeiro se duas datas forem iguais
(define (datas=? d1 d2)
  (and (data? d1)(data? d2)
       (and (= (ano d1)(ano d2))
            (=(mes d1)(mes d2))
            (=(dia d1)(dia d2)))))

;**************************
;* transformador de saída *
;**************************
;escreve a data no ecran da seguinte forma "ano-mes-dia"
(define (escreve-data d)
  (if (data? d)
      (begin
        (display (ano d))
        (display "-")
        (display-zero (mes d))
        (display "-")
        (display-zero (dia d)))))


;****************
;   TAI TEMPO   *
;****************
;tipo abstracto de informação para o tempo. Este será constituído 
;por data e uma hora e será representado como uma lista que contém 
;as duas listas que representam a data e a hora
;***************
;* constructor *
;***************
;cria um tempo a partir de uma data e hora válidas
(define (cria-tempo dat h)
  (if (and (data? dat)
           (hora? h))
      (tipo-escolhido dat h)))

;**************
;* selectores *
;**************
;selecciona os constituintes do tempo, quer a data e a hora quer
;os interiores a estes dois
(define (data-t tempo)
  (primeiro tempo))

(define (hora-t tempo)
  (segundo tempo))

(define (ano-t tempo)
  (ano (data-t tempo)))

(define (mes-t tempo)
  (mes (data-t tempo)))

(define (dia-t tempo)
  (dia (data-t tempo)))

(define (hora-in tempo)
  (hora (hora-t tempo)))

(define (minuto-in tempo)
  (minuto (hora-t tempo)))

;****************
;* reconhecedor *
;****************
;devolve verdadeiro se o argumento recebido for do tipo tempo
(define (tempo? arg)
  (and (tipo-escolhido? arg)
       (comprimento-2 arg)
       (tipo-escolhido? (primeiro arg))
       (comprimento-3 (primeiro arg))
       (tipo-escolhido? (segundo arg))
       (comprimento-2 (segundo arg))
       (and (data? (primeiro arg))
            (hora? (segundo arg)))))

;*********
;* teste *
;*********
;devolve verdadeiro se os dois tempos recebidos forem iguais
(define (tempos=? t1 t2)
  (and (tempo? t1)
       (tempo? t2)
       (and (= (ano t1)(ano t2))
            (= (mes t1)(mes t2))
            (= (dia t1)(dia t2))
            (= (hora-in t1)(hora-in t2))
            (= (minuto-in t1)(minuto-in t2)))))

;devolve verdadeiro se o primeiro tempo recebido é posterior ao segundo
(define (depois? t1 t2)
  (and (tempo? t1)
       (tempo? t2)
       (cond ((> (ano-t t1) (ano-t t2)))
             ((< (ano-t t1) (ano-t t2)) #f)
             ((> (mes-t t1) (mes-t t2)))
             ((< (mes-t t1) (mes-t t2)) #f)
             ((> (dia-t t1) (dia-t t2)))
             ((< (dia-t t1) (dia-t t2)) #f)
             ((> (hora-in t1) (hora-in t2)))
             ((< (hora-in t1) (hora-in t2)) #f)
             ((> (minuto-in t1) (minuto-in t2)))
             ((< (minuto-in t1) (minuto-in t2)) #f)
             (else #t))))

;**************************
;* transformador de saída *
;**************************
;escreve o tempo no ecran na forma "data hora"
(define (escreve-tempo t)
  (if (tempo? t)
      (begin
        (escreve-data (data-t t))
        (display " ")
        (escreve-hora (hora-t t))
        (newline))))

;escreve o tempo que existe entre os dois tempos recebidos
(define (escreve-tempo-que-falta t1 t2)
  (if (and (tempo? t1)
           (tempo? t2))
      (if (not (depois? t1 t2))
          (begin
            (display (abs (- (ano-t t1) (ano-t t2))))
            (display "a")
            (display " ")
            (display (abs (- (mes-t t1) (mes-t t2))))
            (display "m")
            (display " ")
            (display (abs (- (dia-t t1) (dia-t t2))))
            (display "d")
            (display " ")
            (display (abs (- (hora-in t1) (hora-in t2))))
            (display "h")
            (display " ")
            (display (abs (- (minuto-in t1) (minuto-in t2))))
            (display "m")))))
          
      
;*******************************
;* procedimentos de alto nível *
;*******************************
;devolve o tempo que existe entre os dois tempos recebidos


;*****************
; TAI UTILIZADOR *
;*****************
;tipo abstracto de informação para utilizador - lista de 5 argumentos
;nick - símbolo, nome - string, morada - string, email - string e 
;password - símbolo
;**************
; constructor *
;**************
;cria um novo utilizador se os argumentos recebidos forem válidos
(define (cria-utilizador nick nome morada mail pass)
  (if (utilizador? (tipo-escolhido nick nome morada mail pass))
      (tipo-escolhido nick nome morada mail pass)))

;**************
;* Selectores *
;**************
;acedem a partes constituintes do utilizador
(define (da-nick args)
  (primeiro args))

(define (da-nome args)
  (segundo args))

(define (da-morada args)
  (terceiro args))

(define (da-mail args)
  (quarto args))

(define (da-pass args)
  (quinto args))

;******************
;* reconhecedores *
;******************
(define (nick? arg)
  (symbol? arg))

(define (nome? arg)
  (string? arg))

(define (morada? arg)
  (string? arg))

(define (email? arg)
  (string? arg))

(define (password? arg)
  (symbol? arg))

;verifica se um utilizador é válido
(define (utilizador? arg)
  (and (tipo-escolhido? arg)
       (comprimento-5 arg)
       (and (nick? (da-nick arg))
            (nome? (da-nome arg))
            (morada? (da-morada arg))
            (email? (da-mail arg))
            (password? (da-pass arg)))))

;*****************
;* modificadores *
;*****************
;modificam partes do utilizador
(define (muda-nome novo-nome ut)
  (set-segundo! ut novo-nome))

(define (muda-morada nova-morada ut)
  (set-terceiro! ut nova-morada))

(define (muda-email novo-mail ut)
  (set-quarto! ut novo-mail))

(define (muda-pass nova-pass ut)
  (set-quinto! ut nova-pass))

;**********
;* testes *
;**********
;verifica se dois utilizadores têm o mesmo nick
(define (nicks=? u1 u2)
  (if (and (utilizador? u1)
           (utilizador? u2))
      (iguais? (da-nick u1)
               (da-nick u2))))

;verifica se dois utilizadores são iguais
(define (utilizadores=? u1 u2)
  (if (and (utilizador? u1)
           (utilizador? u2))
      (equal? u1 u2)))

;**************************
;* transformador de saída *
;**************************
;escreve para o ecran as informações do utilizador. A flag vai servir para no ip-bay
;se poder decidir se se escreve a password ou não de acordo com o utilizador activo,
;talvez não devesse estar aqui mas não encontrámos outra maneira melhor na altura
(define (escreve-utilizador ut flag)
  (if (utilizador? ut)
      (begin
        (display "Nome: ") 
        (escreve-aspas) 
        (display (da-nome ut))
        (escreve-aspas)
        (newline)
        (display "Morada: ")
        (escreve-aspas) 
        (display (da-morada ut))
        (escreve-aspas)
        (newline)
        (display "Email: ")
        (escreve-aspas) 
        (display (da-mail ut))
        (escreve-aspas)
        (newline)
        ;se a flag for igual a 1, significa que o utilizador activo é igual ao utilizador
        ;inserido, podendo por isso mostrar a password
        (if (= flag 1)
            (begin
              (display "Password: ") 
              (display (da-pass ut))
              (newline))))))


;*****************
;* TAI CATEGORIA *
;*****************
;tipo abstracto de informação que representa a categoria
;***************
;* constructor *
;***************
;cria uma nova categoria
(define (cria-categoria arg)
  arg)

;**************
;* selectores *
;**************
;não há selectores pois a categoria é apenas um símbolo - não temp partes

;****************
;* reconhecedor *
;****************
;devolve verdadeiro se o argumento for do tipo categoria
(define (categoria? arg)
  (symbol? arg))

;*********
;* teste *
;*********
;devolve verdadeiro se as duas recebidas forem iguais
(define (categorias=? cat1 cat2)
  (if (and (categoria? cat1)
           (categoria? cat2))
      (iguais? cat1 cat2)))

;**************************
;* transformador de saída *
;**************************
;imprime a categoria no ecran
(define (escreve-categoria cat)
  (if (categoria? cat)
      (display cat)))

;*******************
;*   TAI PRODUTO   *
;*******************
;tipo abstracto de informação para o produto - lista de 7 argumentos
;nome do produto - símbolo, descreição - string, categoria - categoria
;base de licitacao - real positivo, tempo de permanência em licitação -
;tempo, estado - símbolo e dono - símbolo é o nome do utilizador
;***************
;* constructor *
;***************
;cria um novo produto
(define (cria-produto-tai nome desc cat base tempo estado ut)
  (if (produto? (tipo-escolhido nome desc cat base tempo estado ut))
      (tipo-escolhido nome desc cat base tempo estado ut)))

;**************
;* selectores *
;**************
;seleccionam partes constituintes do produto. 
(define (nome-p produto)
  (primeiro produto))

(define (descricao-p produto)
  (segundo produto))

(define (categoria-p produto)
  (terceiro produto))

(define (base-p produto)
  (quarto produto))

(define (tempo-p produto)
  (quinto produto))

(define (estado-p produto)
  (sexto produto))

(define (dono-p produto)
  (setimo produto))

;******************
;* reconhecedores *
;******************
;verificam as especificidades do produto
(define (nome-p? nome)
  (symbol? nome))

(define (descricao-p? desc)
  (string? desc))

;usa o reconhecedor do tai categoria
(define (categoria-p? cat)
  (categoria? cat))

(define (base-p? base)
  (and (real? base)
       (> base 0)))

;usa o reconhecedor do tai tempo
(define (tempo-p? temp)
  (tempo? temp))

(define (estado-p? est)
  (symbol? est))

(define (dono-p? dono)
  (symbol? dono))

;devolve verdadeiro se o argumento recebido for um produto
(define (produto? arg)
  (and (tipo-escolhido? arg)
       (comprimento-7 arg)
       (and (nome-p? (nome-p arg))
            (descricao-p? (descricao-p arg))
            (categoria-p? (categoria-p arg))
            (base-p? (base-p arg))
            (tempo-p? (tempo-p arg))
            (estado-p? (estado-p arg))
            (dono-p? (dono-p arg)))))

;*********
;* teste *
;*********
;verifica se dois produtos têm o mesmo nome
(define (nome-p=? p1 p2)
  (if (and (produto? p1)
           (produto? p2))
      (iguais? (nome-p p1)
               (nome-p p2))))

;devolve verdadeiro se os dois produtos recebidos forem iguais
(define (produtos=? p1 p2)
  (if (and (produto? p1)
           (produto? p2))
      (equal? p1 p2)))

;**************************
;* transformador de saída *
;**************************
;imprime no ecran o produto de acordo com as especificações apresentadas no enunciado
(define (escreve-produto arg)
  (if (produto? arg)
      (begin
        (display "Descrição: ")
        (escreve-aspas)
        (display (descricao-p arg))
        (escreve-aspas)
        (newline)
        (display "Dono: ")
        (display (dono-p arg))
        (newline)
        (display "Estado: ")
        (display (estado-p arg))
        (newline))))


;*********************
;*   TAI LICITACAO   *
;*********************
;tipo abstracto de informação para a licitação - lista de 4 argumentos
;nome do produto licitado - símbolo, valor da licitação - real positivo,
;tempo da licitação - tempo e licitante - símbolo
;***************
;* constructor *
;***************
;cria uma nova licitaçao
(define (cria-licitacao nome-l valor tempo-l ut-l)
  (if (licitacao? (tipo-escolhido nome-l valor tempo-l ut-l))
      (tipo-escolhido nome-l valor tempo-l ut-l)))

;**************
;* selectores *
;**************
;seleccionam partes constituintes da licitação. 
(define (nome-produto-l licitacao)
  (primeiro licitacao))

(define (valor-l licitacao)
  (segundo licitacao))

(define (tempo-l licitacao)
  (terceiro licitacao))

(define (utilizador-l licitacao)
  (quarto licitacao))


;******************
;* reconhecedores *
;******************
;verificam as especificidades da licitação
(define (nome-produto-l? nome)
  (symbol? nome))

(define (valor-l? valor)
  (and (real? valor)
       (> valor 0)))

(define (tempo-l? t)
  (tempo? t))

(define (utilizador-l? ut)
  (symbol? ut))

;devolve verdadeiro se o argumento recebido for uma licitação
(define (licitacao? arg)
  (and (tipo-escolhido? arg)
       (comprimento-4 arg)
       (and (nome-produto-l? (nome-produto-l arg))
            (valor-l? (valor-l arg))
            (tempo-l? (tempo-l arg))
            (utilizador-l? (utilizador-l arg)))))
                            
;*********
;* teste *
;*********
;devolve verdadeiro se as duas licitações recebidas forem iguais
(define (licitacoes=? l1 l2)
  (equal? l1 l2))

;**************************
;* transformador de saída *
;**************************
;imprime no ecran a licitação de acordo com as especificações apresentadas no enunciado
(define (escreve-licitacao arg)
  (if (licitacao? arg)
      (begin
        (display (nome-produto-l arg))
        (escreve-espacos)
        (display (utilizador-l arg))
        (escreve-espacos)
        (display (valor-l arg))
        (escreve-espacos)
        (display (tempo-l arg))
        (newline))))

;********************
;*   TAI MENSAGEM   *
;********************
;tipo abstracto de informação para a mensagem - lista de 5 argumentos
;nome do produto - símbolo, nick do comprador - símbol, nick do vendedor 
;- símbolo, valor da compra - real positivo ou 0.00 se não houve compra 
;e o tempo associado à acção  - tempo, tempo da licitação que o permitiu 
;comprar ou tempo da criação do produto
;***************
;* constructor *
;***************
;cria uma nova mensagem
(define (cria-mensagem nome-prod-m comprador-m vendedor-m valor-m tempo-m)
  (if (mensagem? (tipo-escolhido nome-prod-m comprador-m vendedor-m valor-m tempo-m))
      (tipo-escolhido nome-prod-m comprador-m vendedor-m valor-m tempo-m)))

;**************
;* selectores *
;**************
;seleccionam partes constituintes da mensagem. 
(define (nome-prod-m msg)
  (primeiro msg))

(define (comprador-m msg)
  (segundo msg))

(define (vendedor-m msg)
  (terceiro msg))

(define (valor-m msg)
  (quarto msg))

(define (tempo-m msg)
  (quinto msg))

;******************
;* reconhecedores *
;******************
;verificam as especificidades da licitação
(define (nome-prod-m? nome)
  (symbol? nome))

(define (comprador-m? ut)
  (symbol? ut))

(define (vendedor-m? ut)
  (symbol? ut))

(define (valor-m? valor)
  (and (real? valor)
       (>= valor 0)))

(define (tempo-m? t)
  (tempo? t))

;devolve verdadeiro se o argumento recebido for uma mensagem
(define (mensagem? arg)
  (and (tipo-escolhido? arg)
       (comprimento-5 arg)
       (and (nome-prod-m? (nome-prod-m arg))
            (comprador-m? (comprador-m arg))
            (vendedor-m? (vendedor-m arg))
            (valor-m? (valor-m arg))
            (tempo-m? (tempo-m arg)))))
                            
;*********
;* teste *
;*********
;devolve verdadeiro se as duas mensagens recebidas forem iguais
(define (mensagens=? m1 m2)
  (equal? m1 m2))

;**************************
;* transformador de saída *
;**************************
;imprime no ecran a mensagem de acordo com as especificações apresentadas no enunciado
(define (escreve-mensagem arg flag)
  (if (mensagem? arg)
      (begin
        (display (nome-prod-m arg))
        (escreve-espacos)
        (if (= flag 1)
            (if (= (valor-m arg) 0)
                (display (vendedor-m arg))
                (display (comprador-m arg))))
        (escreve-espacos)
        (display (valor-m arg))
        (escreve-espacos)
        (escreve-tempo (tempo-m arg))
        (newline))))


;****************************
;*   TAI PROCESSO PRODUTO   *
;****************************
;tipo abstracto de informação para o processor produto - lista de 5 
;argumentos: tipo de acção - símbolo nome do produto - símbolo, nick 
;do utilizador - símbolo, valor da compra - real positivo ou 0.00 se
;não houve compra e o tempo associado à acção - tempo
;*****************
;* constructores *
;*****************
;cria um novo processo produto
(define (cria-pp nome-prod-pp utilizador-pp valor-pp tempo-pp flag)
  (if (= flag 1)
      ;é venda
      (if (processo-produto? 
           (insere 'venda (tipo-escolhido nome-prod-pp utilizador-pp valor-pp tempo-pp)))
          (insere 'venda (tipo-escolhido nome-prod-pp utilizador-pp valor-pp tempo-pp)))
      (if (processo-produto? 
           (insere 'compra (tipo-escolhido nome-prod-pp utilizador-pp valor-pp tempo-pp)))
          (insere 'compra (tipo-escolhido nome-prod-pp utilizador-pp valor-pp tempo-pp)))))

;**************
;* selectores *
;**************
;seleccionam partes constituintes da processo-produto. 
(define (accao-pp pp)
  (primeiro pp))

(define (nome-prod-pp pp)
  (segundo pp))

(define (utilizador-pp pp)
  (terceiro pp))

(define (valor-pp pp)
  (quarto pp))

(define (tempo-pp pp)
  (quinto pp))

;******************
;* reconhecedores *
;******************
;verificam as especificidades da licitação
(define (accao-pp? ac)
  (symbol? ac))

(define (nome-prod-pp? nome)
  (symbol? nome))

(define (utilizador-pp? ut)
  (symbol? ut))

(define (valor-pp? valor)
  (and (real? valor)
       (>= valor 0)))

(define (tempo-pp? t)
  (tempo? t))

;devolve verdadeiro se o argumento recebido for uma processo-produto
(define (processo-produto? arg)
  (and (tipo-escolhido? arg)
       (comprimento-5 arg)
       (and (accao-pp? (accao-pp arg))
            (nome-prod-pp? (nome-prod-pp arg))
            (utilizador-pp? (utilizador-pp arg))
            (valor-pp? (valor-pp arg))
            (tempo-pp? (tempo-pp arg)))))
                            
;*********
;* teste *
;*********
;devolve verdadeiro se os dois processos produto recebidos forem iguais
(define (pp=? pp1 pp2)
  (equal? pp1 pp2))

;**************************
;* transformador de saída *
;**************************
;imprime no ecran o processo-produto de acordo com as especificações apresentadas no enunciado
(define (escreve-processo-produto-normal arg t-actual t-accao)
  (if (processo-produto? arg)
      (if (iguais? (accao-pp arg) 'venda)
          (begin
            (display "(")
            (display (utilizador-pp arg))
            (display " ")
            (display (valor-pp arg))
            (display " ")
            (escreve-tempo-que-falta t-actual t-accao)
            (display ")")
            (newline))
          (begin
            (escreve-tempo (tempo-pp arg))
            (escreve-espacos)
            (display (utilizador-pp arg))
            (escreve-espacos)
            (display (valor-pp arg))
            (newline)))))
          
(define (escreve-processo-produto-terminado arg)
  (if (processo-produto? arg)
      (if (iguais? (accao-pp arg) 'venda)
          (begin
            (display "(")
            (display (utilizador-pp arg))
            (display " ")
            (display "0.00")
            (display " ")
            (escreve-aspas)
            (display "Licitação terminada")
            (escreve-aspas)
            (display ")")
            (newline))
          (begin
            (escreve-tempo (tempo-pp arg))
            (escreve-espacos)
            (display (utilizador-pp arg))
            (escreve-espacos)
            (display (valor-pp arg))
            (newline)))))


;*************************
;*************************
;**   CICLO PRINCIPAL   **
;*************************
;*************************
;ciclo principal do sistema ip-Bay que lê os comandos inseridos pelo utilizador
;e associa-os aos respectivos procedimentos

(define (iniciar-ip-Bay)
  
  (define (iniciar-aux)
    
    ;imprime a prompt inicial de acordo com o utilizador activo
    (prompt-inicial)
    
    ;vamos guardar os comandos introduzidos pelo utilizador na variável comando
    (let ((comando (read)))
      
      ;testes para verificar a validade dos comandos de acordo com o utilizador activo
      (if (and (tipo-escolhido? comando) (not (tipo-vazio? comando)))
          
          (let ((comando-principal (primeiro comando))
                (argumentos-comando (resto comando)))
            
            ;comandos permitidos quando nenhum utilizador registado está activo
            (cond ((utilizador-nenhum?)
                   (cond ((iguais? comando-principal 'entrar)
                          (entrar))
                         ((iguais? comando-principal 'entra-utilizador)
                          (entra-utilizador argumentos-comando))
                         ((iguais? comando-principal 'terminar-ip-Bay)
                          (terminar-ip-Bay))
                         (else (mensagem-comando-desconhecido))))
                  
                  ;comandos permitidos ao administrador do sistema
                  ((utilizador-administrador?)
                   (cond ((iguais? comando-principal 'inicia-tempo)
                          (inicia-tempo argumentos-comando))
                         ((iguais? comando-principal 'visualiza-tempo)
                          (visualiza-tempo))
                         ((iguais? comando-principal 'passa-tempo)
                          (passa-tempo argumentos-comando))
                         ((iguais? comando-principal 'sair)
                          (sair))
                         ((iguais? comando-principal 'inserir-categoria)
                          (inserir-categoria argumentos-comando))
                         ((iguais? comando-principal 'categorias-disponiveis)
                          (categorias-disponiveis))
                         ((iguais? comando-principal 'remover-categoria)
                          (remover-categoria argumentos-comando))
                         ((iguais? comando-principal 'utilizadores-registados)
                          (utilizadores-registados))
                         ((iguais? comando-principal 'dados-produto)
                          (dados-produto argumentos-comando))
                         ((iguais? comando-principal 'produtos-em-licitacao)
                          (produtos-em-licitacao))
                         ((iguais? comando-principal 'produtos-vendidos)
                          (produtos-vendidos))
                         ((iguais? comando-principal 'produtos-nao-vendidos)
                          (produtos-nao-vendidos))
                         ((iguais? comando-principal 'processo-produto)
                          (processo-produto argumentos-comando))
                         (else (mensagem-comando-desconhecido))))
                  
                  ;comandos permitidos ao utilizador anónimo
                  ((utilizador-anonimo?)
                   (cond ((iguais? comando-principal 'regista)
                          (regista argumentos-comando))
                         ((iguais? comando-principal 'utilizadores-registados)
                          (utilizadores-registados))
                         ((iguais? comando-principal 'sair)
                          (sair))
                         ((iguais? comando-principal 'utilizadores-registados)
                          (utilizadores-registados))
                         ((iguais? comando-principal 'dados-produto)
                          (dados-produto argumentos-comando))
                         ((iguais? comando-principal 'produtos-em-licitacao)
                          (produtos-em-licitacao))
                         ((iguais? comando-principal 'produtos-vendidos)
                          (produtos-vendidos))
                         ((iguais? comando-principal 'produtos-nao-vendidos)
                          (produtos-nao-vendidos))
                         ((iguais? comando-principal 'processo-produto)
                          (processo-produto argumentos-comando))
                         (else (mensagem-comando-desconhecido))))
                  
                  ;comandos permitidos a todos os utilizadores registados
                  (else
                   (cond ((iguais? comando-principal 'sair)
                          (sair))
                         ((iguais? comando-principal 'visualiza-tempo)
                          (visualiza-tempo))
                         ((iguais? comando-principal 'passa-tempo)
                          (passa-tempo argumentos-comando))
                         ((iguais? comando-principal 'nome)
                          (nome argumentos-comando))
                         ((iguais? comando-principal 'morada)
                          (morada argumentos-comando))
                         ((iguais? comando-principal 'email)
                          (email argumentos-comando))
                         ((iguais? comando-principal 'password)
                          (password argumentos-comando))
                         ((iguais? comando-principal 'dados-utilizador)
                          (dados-utilizador argumentos-comando))
                         ((iguais? comando-principal 'utilizadores-registados)
                          (utilizadores-registados))
                         ((iguais? comando-principal 'categorias-disponiveis)
                          (categorias-disponiveis))
                         ((iguais? comando-principal 'cria-produto)
                          (cria-produto argumentos-comando))
                         ((iguais? comando-principal 'dados-produto)
                          (dados-produto argumentos-comando))
                         ((iguais? comando-principal 'produtos-em-licitacao)
                          (produtos-em-licitacao))
                         ((iguais? comando-principal 'produtos-vendidos)
                          (produtos-vendidos))
                         ((iguais? comando-principal 'produtos-nao-vendidos)
                          (produtos-nao-vendidos))
                         ((iguais? comando-principal 'licita)
                          (licita argumentos-comando))
                         ((iguais? comando-principal 'processo-produto)
                          (processo-produto argumentos-comando))
                         ;não foi implementado em tempo útil
                         ;((iguais? comando-principal 'processo-utilizador)
                         ; (processo-utilizador argumentos-comando
                         (else (mensagem-comando-desconhecido))))))
          
          (mensagem-comando-desconhecido))
      ;se a flag final ainda não tiver sido modificada continuamos no ciclo
      (if (= final 0)
          (iniciar-aux))))
  
  ;*********************
  ;*   OBJECTO TEMPO   *
  ;*********************
  ;objecto com a variável tempo guardada que regista o tempo actual do sistema
  ;e métodos que a ela acedem ou a modificam. Vai utilizar três dos tipos de 
  ;informação definidos: a hora, a data e o tempo
  (define (objecto-tempo tempo)
    
    ;*******************************
    ;*   MÉTODOS DO OBJECTO TEMPO  *
    ;*******************************
    ;de seguida são apresentados os métodos do objecto tempo. Estes métodos 
    ;são os únicos que têm acesso à variável tempo actual aqui guardada
    ;********************
    ;*   INICIA-TEMPO   *
    ;********************
    ;coloca na variável tempo o argumento do tipo tempo recebido
    (define (inicia-tempo-aux t)
      (if (comprimento-5 t)
          (let ((novo-tempo (cria-tempo (cria-data (primeiro t)
                                                   (segundo t)
                                                   (terceiro t))
                                        (cria-hora (quarto t)
                                                   (quinto t)))))
            (if (tempo? novo-tempo)
                (set! tempo novo-tempo)
                (mensagem-tempo-invalido)))
          (mensagem-tempo-invalido)))
    
    ;*********************
    ;*  VISUALIZA-TEMPO  *
    ;*********************
    ;imprime o tempo actual guardado na variável tempo no ecran
    (define (visualiza-tempo-aux)
      (if (tipo-vazio? tempo)
          (mensagem-tempo-nao-inicializado)
          (escreve-tempo tempo)))
    
    ;*******************
    ;*   PASSA-TEMPO   *
    ;*******************
    ;faz avançar o tempo actual os minutos que receber
    (define (passa-tempo-aux2 min)
      ;constantes usadas na definição do procedimento passa-tempo
      (define minutos-de-ano 525600)
      (define minutos-ano-bissexto 527040)
      (define minutos-4-anos 2103840)
      (define minutos-mes-31 44640)
      (define minutos-mes-30 43200)
      (define minutos-mes-29 41760)
      (define minutos-mes-28 40320)
      (define minutos-dia 1440)
      (define minutos-hora 60)
      ;verifica se o mês recebido tem 31 ou 30 dias
      (define (mes-31-dias? mes)
        (or (= mes 1)(= mes 3)(= mes 5)(= mes 7)(= mes 8)(= mes 10)(= mes 12)))
      (define (mes-30-dias? mes)
        (or (= mes 4)(= mes 6)(= mes 9)(= mes 11)))
      ;verifica se o dia recebido é o último de acordo com o mês e ano recebidos
      (define (ultimo-dia-mes? ano mes dia)
        (cond ((and (mes-31-dias? mes)(= dia 31)))
              ((and (mes-30-dias? mes)(= dia 30)))
              ((and (bissexto? ano)
                    (= mes 2)
                    (= dia 29)))
              ((and (not (bissexto? ano))
                    (= mes 2)
                    (= dia 28)))
              (else #f)))
      
      (define (passa-tempo-aux3 minutos ano-aux mes-aux dia-aux hora-aux min-aux)
        (define (passa-tempo-aux minutos)
          
          (cond ((>= (- minutos minutos-4-anos) 0)
                 (set! ano-aux (+ ano-aux 4))
                 (passa-tempo-aux (- minutos minutos-4-anos)))
                
                ((or (and (bissexto? ano-aux)
                          (<= mes-aux 2)
                          (and (not (= dia-aux 29))
                               (= mes-aux 2))
                          (>= (- minutos minutos-ano-bissexto) 0))
                     (and (bissexto? (+ ano-aux 1))
                          (>= (- minutos minutos-ano-bissexto) 0)))
                 (set! ano-aux (+ ano-aux 1))
                 (passa-tempo-aux (- minutos minutos-ano-bissexto)))
                
                ((or (and (bissexto? ano-aux)
                          (>= (- minutos minutos-de-ano) 0))
                     (and (bissexto? (+ ano-aux 1))
                          (<= mes-aux 2)
                          (and (not (= dia-aux 29))
                               (= mes-aux 2))
                          (>= (- minutos minutos-de-ano) 0))
                     (>= (- minutos minutos-de-ano) 0))
                 (set! ano-aux (+ ano-aux 1))
                 (passa-tempo-aux (- minutos minutos-de-ano)))
                
                ((and (= mes-aux 12)
                      (ultimo-dia-mes? ano-aux mes-aux dia-aux)
                      (>= (- minutos minutos-dia) 0))
                 (set! dia-aux 1)
                 (set! mes-aux 1)
                 (set! ano-aux (+ ano-aux 1))
                 (passa-tempo-aux (- minutos minutos-dia)))
                
                ((and (ultimo-dia-mes? ano-aux mes-aux dia-aux)
                      (>= (- minutos minutos-dia) 0))
                 (set! dia-aux 1)
                 (set! mes-aux (+ mes-aux 1))
                 (passa-tempo-aux (- minutos minutos-dia)))
                
                ((and (= mes-aux 1)
                      (= dia-aux 30)
                      (>= (- minutos minutos-mes-31) 0))
                 (set! mes-aux 3)
                 (if (bissexto? ano-aux)
                     (set! dia-aux 1)
                     (set! dia-aux 2))
                 (passa-tempo-aux (- minutos minutos-mes-31)))
                
                ((and (mes-31-dias? mes-aux)
                      (>= (- minutos minutos-mes-31) 0)
                      (not (= mes-aux 12)))
                 (set! mes-aux (+ mes-aux 1))
                 (passa-tempo-aux (- minutos minutos-mes-31)))
                
                ((and (= mes-aux 12)
                      (>= (- minutos minutos-mes-31) 0))
                 (set! mes-aux 1)
                 (set! ano-aux (+ ano-aux 1))
                 (passa-tempo-aux (- minutos minutos-mes-31)))
                
                ((and (mes-30-dias? mes-aux)
                      (>= (- minutos minutos-mes-30) 0))
                 (set! mes-aux (+ mes-aux 1))
                 (passa-tempo-aux (- minutos minutos-mes-30)))
                
                ((and (= mes-aux 2)
                      (bissexto? ano-aux)
                      (>= (- minutos minutos-mes-29) 0))
                 (set! mes-aux (+ mes-aux 1))
                 (passa-tempo-aux (- minutos minutos-mes-29)))
                
                ((and (= mes-aux 2)
                      (not (bissexto? ano-aux))
                      (>= (- minutos minutos-mes-28) 0))
                 (set! mes-aux (+ mes-aux 1))
                 (passa-tempo-aux (- minutos minutos-mes-28)))
                
                ((>= (- minutos minutos-dia) 0)
                 (set! dia-aux (+ dia-aux 1))
                 (passa-tempo-aux (- minutos minutos-dia)))
                
                ((and (= hora-aux 23)
                      (>= (- minutos minutos-hora) 0))
                 (set! dia-aux (+ dia-aux 1))
                 (set! hora-aux 0)
                 (passa-tempo-aux (- minutos minutos-hora)))
                
                ((>= (- minutos minutos-hora) 0)
                 (set! hora-aux (+ hora-aux 1))
                 (passa-tempo-aux (- minutos minutos-hora)))
                
                ((< (+ minutos min-aux) 60)
                 (set! min-aux (+ min-aux minutos))
                 (set! tempo (tipo-escolhido (tipo-escolhido ano-aux mes-aux dia-aux)
                                             (tipo-escolhido hora-aux min-aux))))
                
                ((= min-aux 59)
                 (set! min-aux 0)
                 (if (= hora-aux 23)
                     (begin
                       (set! hora-aux 0)
                       (if (ultimo-dia-mes? ano-aux mes-aux dia-aux)
                           (begin 
                             (set! dia-aux 1)
                             (if (= mes-aux 12)
                                 (begin
                                   (set! ano-aux (+ ano-aux 1))
                                   (set! mes-aux 1))
                                 (set! mes-aux (+ mes-aux 1))))
                           (set! dia-aux (+ dia-aux 1))))
                     (set! hora-aux (+ hora-aux 1)))
                 (passa-tempo-aux (- minutos 1)))
                
                ((not (= minutos 0))
                 (set! min-aux (+ min-aux 1))
                 (passa-tempo-aux (- minutos 1)))
                
                ((= minutos 0)
                 (set! tempo (tipo-escolhido (tipo-escolhido ano-aux mes-aux dia-aux)
                                             (tipo-escolhido hora-aux min-aux))))
                
                (else (display "deu um erro qualquer"))))
        (passa-tempo-aux minutos))
      
      (if (tipo-vazio? tempo)
          (mensagem-tempo-nao-inicializado)
          (cond ((and (integer? min)
                      (< min 10000000000)
                      (> min 0))
                 (let ((ano-aux (ano-t tempo))
                       (mes-aux (mes-t tempo))
                       (dia-aux (dia-t tempo))
                       (hora-aux (hora-in tempo))
                       (min-aux (minuto-in tempo))
                       (minutos min))
                   (passa-tempo-aux3 min ano-aux mes-aux dia-aux hora-aux min-aux)
                   ((produto-ip-bay 've))))
                (else (numero-minutos-invalido)))))
    
    ;*****************
    ;* DEVOLVE TEMPO *
    ;*****************
    ;devolve a variável tempo guardada para que possa ser utilizada por outros objectos
    (define (devolve-tempo)
      tempo)
    
    
    ;***********************
    ;*  ESCOLHA DO MÉTODO  *
    ;***********************
    ;verificamos aqui qual o método a chamar entre os definidos.
    
    (lambda (tipo)
      (cond ((iguais? tipo 'p) passa-tempo-aux2)
            ((iguais? tipo 'v) visualiza-tempo-aux)
            ((iguais? tipo 'i) inicia-tempo-aux)
            ((iguais? tipo 'd) devolve-tempo)
            (else (mensagem-comando-desconhecido)))))
  
  
  ;****************************
  ;*   OBJECTO UTILIZADORES   *
  ;****************************
  ;objecto com duas variáveis guardadas: a lista de utilizadores
  ;registados e a lista de nick's, e métodos que a elas acedem ou 
  ;as modificam.
  (define (objecto-utilizador lista-utilizadores lista-nicks)
    
    
    ;**********************
    ;* MÉTODOS DO OBJECTO *
    ;**********************
    ;de seguida são apresentados os métodos do objecto utilizador. Estes 
    ;métodos são os únicos que têm acesso às variáveis aqui guardadas. Aqueles
    ;que tiverem caixa à volta do nome são os utilizador "directamente" pelos
    ;utilizadores do sistema
    
    ;devolve o utilizador referente ao nick introduzido
    (define (da-utilizador arg)
      (define (da-utilizador-aux lst)
        (if (iguais? arg (da-nick (primeiro lst)))
            (primeiro lst)
            (da-utilizador-aux (resto lst))))
      (if (nick-existente? arg)
          (da-utilizador-aux lista-utilizadores)
          (display "da-utilizador: o utilizador não existe.")))
    
    ;devolve verdadeiro o utilizador recebido existe na lista
    (define (utilizador-existente? ut)
      (define (utilizador-existente-aux lst)
        (cond ((tipo-vazio? lst) #f)
              ((nicks=? ut (primeiro lst)))
              (else (utilizador-existente-aux (resto lst)))))
      (utilizador-existente-aux lista-utilizadores))
    
    ;devolve verdadeiro se o nick recebido existe na lista de nicks
    (define (nick-existente? nick)
      (define (nick-existente-aux lst)
        (cond ((tipo-vazio? lst) #f)
              ((iguais? nick (primeiro lst)))
              (else (nick-existente-aux (resto lst)))))
      (nick-existente-aux lista-nicks))
    
    
    ;***********
    ;* REGISTA *
    ;***********
    ;regista um utilizador no sistema desde que sejam recebidos argumentos
    ;válidos
    (define (regista-aux args)
      (define (registar args)
        (let ((utilizador-a-registar (cria-utilizador (ultimo (primeiro args))
                                                      (segundo args)
                                                      (terceiro args)
                                                      (quarto args)
                                                      (ultimo (quinto args)))))
          (if (utilizador? utilizador-a-registar)
              (let ((nick-a-registar (da-nick utilizador-a-registar)))
                (if (nick-existente? nick-a-registar)
                    (mensagem-utilizador-ja-existe nick-a-registar)
                    (begin
                      (set! lista-utilizadores 
                            (insere utilizador-a-registar lista-utilizadores))
                      (set! lista-nicks (insere nick-a-registar lista-nicks))
                      (mensagem-utilizador-registado-com-sucesso nick-a-registar))))
              (mensagem-utilizador-invalido))))
      (if (comprimento-5 args)
          (registar args)
          (mensagem-utilizador-invalido)))
    
    ;********************
    ;* ENTRA UTILIZADOR *
    ;********************
    ;entra no "ambiente" do utilizador após a introdução da password correcta
    ;caso o utilizador não exista devolve um erro
    (define (entra-aux n)
      (define (pede-pass-adm vezes)
        (display "Password: ")
        (let ((comando (read)))
          (if (iguais? comando 'ip-admin)
              (set! utilizador-activo 'administrador)
              (begin
                (mensagem-password-incorrecta)
                (set! vezes (+ vezes 1))
                (if (< vezes 3)
                    (pede-pass-adm vezes))))))
      
      (define (pede-pass-ut nick vezes)
        (display "Password: ")
        (let ((comando (read)))
          (if (iguais? comando (da-pass (da-utilizador nick)))
              (begin
                (set! utilizador-activo nick)
                ((produto-ip-bay 'vm) utilizador-activo))
              (begin
                (mensagem-password-incorrecta)
                (set! vezes (+ vezes 1))
                (if (< vezes 3)
                    (pede-pass-ut nick vezes))))))
      
      (let ((nick (ultimo (ultimo n))))
        (if (iguais? nick 'administrador)
            (pede-pass-adm 0)
            (if (nick-existente? nick)
                (pede-pass-ut nick 0)
                (mensagem-utilizador-nao-existe nick)))))
    
    ;********************
    ;* DADOS UTILIZADOR *
    ;********************
    ;devolve os dados do utilizador que tem nick inserido
    (define (dados-aux arg)
      (if (not (comprimento-1 arg))
          (numero-argumentos-invalido)
          (let ((nick (ultimo (ultimo arg))))
            (if (not (nick-existente? nick))
                (mensagem-utilizador-nao-existente)
                (let ((utilizador-inserido (da-utilizador nick)))
                  (if (iguais? utilizador-activo nick)
                      ;se o utilizador inserido for o mesmo que o utilizador 
                      ;activo também aparece a password daí flag=1
                      (escreve-utilizador utilizador-inserido 1)
                      ;o utilizador inserido não é igual ao utilizador activo 
                      ;por isso não aparece a password daí flag=0
                      (escreve-utilizador utilizador-inserido 0)))))))
    
    ;***************************
    ;* UTILIZADORES REGISTADOS *
    ;***************************
    ;devolve a lista de utilizadores registados no ip-Bay
    (define (util-aux)
      (if (tipo-vazio? lista-nicks)
          (mensagem-nao-ha-utilizadores-registados)
          (begin
            (display lista-nicks)
            (newline))))
    
    ;********
    ;* NOME *
    ;********
    ;altera o nome do utilizador para o novo nome por ele inserido
    (define (nome-aux arg)
      (if (nome? arg)
          (muda-nome arg (da-utilizador utilizador-activo))
          (mensagem-argumentos-invalidos)))
    
    ;**********
    ;* MORADA *
    ;**********
    ;altera a morada do utilizador para a nova morada por ele inserido
    (define (morada-aux arg)
      (if (morada? arg)
          (muda-morada arg (da-utilizador utilizador-activo))
          (mensagem-argumentos-invalidos)))
    
    ;*********
    ;* EMAIL *
    ;*********
    ;altera o email do utilizador para o novo email por ele inserido
    (define (email-aux arg)
      (if (email? arg)
          (muda-email arg (da-utilizador utilizador-activo))
          (mensagem-argumentos-invalidos)))
    
    ;************
    ;* PASSWORD *
    ;************
    ;altera a password do utilizador para a nova password por ele inserido
    (define (pass-aux arg)
      (let ((pass-nova (ultimo arg)))
        (if (password? pass-nova)
            (muda-pass pass-nova (da-utilizador utilizador-activo))
            (mensagem-argumentos-invalidos))))
    
    
    ;***********************
    ;*  ESCOLHA DO MÉTODO  *
    ;***********************
    ;verificamos aqui qual o método a chamar entre os definidos.
    (lambda (tipo)
      (cond ((iguais? tipo 'r) regista-aux)
            ((iguais? tipo 'u) util-aux)
            ((iguais? tipo 'd) dados-aux)
            ((iguais? tipo 'n) nome-aux)
            ((iguais? tipo 'm) morada-aux)
            ((iguais? tipo 'e) email-aux)
            ((iguais? tipo 'p) pass-aux)
            ((iguais? tipo 'eu) entra-aux)
            (else (mensagem-comando-desconhecido)))))
  
  
  ;****************************
  ;*   OBJECTO CATEGORIA      *
  ;****************************
  ;objecto com uma variável guardada: a lista de categorias
  ;e métodos que a ela acedem ou a modificam
  (define (objecto-categoria lista-categorias)
    
    
    ;**********************
    ;* MÉTODOS DO OBJECTO *
    ;**********************
    ;de seguida são apresentados os métodos do objecto categoria. Estes métodos 
    ;são os únicos que têm acesso à variável lista-categorias aqui guardada
    
    ;este é um selector da lista, devolve-nos a primeira categoria da lista
    (define (da-categoria lista)
      (primeiro lista))
    
    ;devolve verdadeiro se a categoria recebida existir na lista de categorias
    (define (categoria-existente? arg)
      (define (existe-aux lst)
        (cond ((tipo-vazio? lst) #f)
              ((categorias=? arg (da-categoria lst)))
              (else (existe-aux (resto lst)))))
      (existe-aux lista-categorias))
    
    ;***************
    ;* modificador *
    ;***************
    ;modifica a lista removendo um elemento
    (define (remove-cat cat lista)
      (remq cat lista))
    
    ;imprime a lista de categorias no ecran na forma (cat1 cat2 ...)
    (define (escreve-lista-categorias lst)
      (display lst)
      (newline))
    
    ;****************************
    ;* DEVOLVE LISTA CATEGORIAS *
    ;****************************
    ;retorna a lista para poder ser usada por outros objectos
    (define (lista-categorias-aux)
      lista-categorias)
    
    ;**************************
    ;* CATEGORIAS DISPONIVEIS *
    ;**************************
    ;devolve a lista de categorias, se não existirem categorias devolve
    ;uma mensagem informativa do facto
    (define (categorias-disp-aux)
      (if (tipo-vazio? lista-categorias)
          (mensagem-nao-ha-categorias)
          (escreve-lista-categorias lista-categorias)))
    
    ;*********************
    ;* INSERIR CATEGORIA *
    ;*********************
    ;insere uma ou mais categorias na lista de categoria
    (define (inserir-aux args)  
      (define (inserir-cat nova-categoria cont)
        (if (> cont 0)
            (if (and (tipo-escolhido? (da-categoria nova-categoria))
                     (categoria? (da-simbolo (da-categoria nova-categoria))))
                (let ((categoria-a-inserir (da-simbolo (da-categoria nova-categoria))))
                  (if (categoria-existente? categoria-a-inserir)
                      (begin
                        (mensagem-categoria-existente categoria-a-inserir)
                        (inserir-cat (resto nova-categoria) (- cont 1)))
                      (begin
                        (set! lista-categorias (insere categoria-a-inserir lista-categorias))
                        (inserir-cat (resto nova-categoria) (- cont 1)))))
                (inserir-cat (resto nova-categoria) (- cont 1)))))
      (if (tipo-vazio? args)
          (mensagem-deve-introduzir-categorias)
          (inserir-cat (cria-categoria args) (comprimento args))))
    
    ;*********************
    ;* REMOVER CATEGORIA *
    ;*********************
    ;remove uma ou mais categorias da lista de categorias
    (define (remover-aux args)
      (define (rem-aux cat)
        (if (not (tipo-vazio? cat))
            (let ((categoria-a-remover (da-simbolo (primeiro cat))))
              (if (categoria-existente? categoria-a-remover)
                  (begin
                    (set! lista-categorias (remove-cat categoria-a-remover lista-categorias))
                    (rem-aux (resto cat)))
                  (begin
                    (mensagem-categoria-nao-existente categoria-a-remover)
                    (rem-aux (resto cat)))))))
      (if (tipo-vazio? args)
          (mensagem-deve-introduzir-categorias)
          (rem-aux args)))
    
    
    ;***********************
    ;*  ESCOLHA DO MÉTODO  *
    ;***********************
    ;verificamos aqui qual o método a chamar entre os definidos.
    
    (lambda (tipo)
      (cond ((iguais? tipo 'i) inserir-aux)
            ((iguais? tipo 'c) categorias-disp-aux)
            ((iguais? tipo 'r) remover-aux)
            ((iguais? tipo 'l) lista-categorias-aux)
            ((iguais? tipo 'e) categoria-existente?)
            (else (mensagem-comando-desconhecido)))))
  
  
  ;***********************
  ;*   OBJECTO PRODUTO   *
  ;***********************
  ;objecto que vai guardar uma lista de produtos e uma lista de licitações
  (define (objecto-produto lista-produtos lista-licitacoes 
                           lista-mensagens lista-processo-produto)
    
    
    ;*****************************
    ; MÉTODOS DO OBJECTO PRODUTO *
    ;*****************************
    ;devolve verdadeiro se o produto com o nome recebido já existe 
    ;na lista de produtos com o estado em licitação
    (define (produto-existente? nome)
      (define (produto-existente?-aux lst)
        (cond ((tipo-vazio? lst) #f)
              ((iguais? nome (nome-p (primeiro lst))))
              (else (produto-existente?-aux (resto lst)))))
      (produto-existente?-aux lista-produtos))
    
    ;devolve verdadeiro se a licitação for válida ou seja, o seu valor é superior
    ;ao valor da última licitação ao produto
    ;se o produto tiver licitacoes ve o valor da primeira que encontra - porque é 
    ;o maior se nao tiver ve o valor da base de licitacao
    (define (ultima-licitacao lst nome-prod)
      (cond ((tipo-vazio? lst) 
             (base-p (da-produto nome-prod)))
            ((iguais? nome-prod (nome-produto-l (primeiro lst)))
             (valor-l (primeiro lst)))
            (else (ultima-licitacao (resto lst) nome-prod))))
    
    (define (tem-licitacoes? nome)
      (define (tem-licitacoes?-aux lst)
        (cond ((tipo-vazio? lst) #f)
              ((iguais? nome (nome-produto-l (primeiro lst))))
              (else (tem-licitacoes?-aux (resto lst)))))
      (tem-licitacoes?-aux lista-licitacoes))
    
    (define (licitacao-valida? lic prod)
      (cond ((> (valor-l lic) 
                (ultima-licitacao lista-licitacoes prod)))
            (else #f)))
    
    (define (da-licitacao nome)
      (define (da-licitacao-aux lst)
        (if (iguais? nome (nome-produto-l (primeiro lst)))
            (primeiro lst)
            (da-licitacao-aux (resto lst))))
      (if (tem-licitacoes? nome)
          (da-licitacao-aux lista-licitacoes)
          (display "o produto não tem licitacoes")))
    
    
    ;****************
    ;* CRIA PRODUTO *
    ;****************
    ;acrescenta um novo produto à lista de produtos, se for válido acrescentando-lhe
    ;o dono - utilizador-activo e o estado - em licitação
    (define (cria-produto-aux prod)
      (if (comprimento-5 prod)
          (let ((faz-tempo (executa (quinto prod))))
            (let ((produto-a-inserir (cria-produto-tai (ultimo (primeiro prod))
                                                       (segundo prod)
                                                       (ultimo (terceiro prod))
                                                       (quarto prod)
                                                       faz-tempo
                                                       estado 
                                                       utilizador-activo)))
            (if (and (produto? produto-a-inserir)
                     ((categoria-ip-bay 'e) (categoria-p produto-a-inserir))
                     (depois? (tempo-p produto-a-inserir) ((tempo-ip-bay 'd))))
                (let ((nome-prod-a-inserir (nome-p produto-a-inserir)))
                  (if (not (produto-existente? nome-prod-a-inserir))
                      (let ((processo-produto-a-inserir 
                             (cria-pp 
                              nome-prod-a-inserir
                              utilizador-activo
                              (base-p produto-a-inserir)
                              (tempo-p produto-a-inserir)
                              1)))
                        (set! lista-produtos (insere produto-a-inserir lista-produtos))
                        (set! lista-processo-produto 
                              (insere processo-produto-a-inserir lista-processo-produto)))
                      (mensagem-produto-ja-existe nome-prod-a-inserir)))
                (mensagem-argumentos-invalidos))))
          (mensagem-argumentos-invalidos)))
    
    ;devolve o produto com o nome recebido
    (define (da-produto nome)
      (define (da-produto-aux lst)
        (if (tipo-vazio? lst)
            (mensagem-produto-inexistente nome)
            (let ((produto (primeiro lst)))
              (if (iguais? nome (nome-p produto))
                  produto
                  (da-produto-aux (resto lst))))))
      (da-produto-aux lista-produtos))
    
    ;*****************
    ;* DADOS PRODUTO *
    ;*****************
    ;imprime no ecran os seguintes dados do produto: descrição, dono e estado
    (define (dados-produto-aux nome)
      (if (produto-existente? nome)
          (escreve-produto (da-produto nome))
          (mensagem-produto-inexistente nome)))
    
    ;*************************
    ;* PRODUTOS EM LICITAÇÃO *
    ;*************************
    ;imprime no ecran uma lista que contém os nomes dos produtos em licitação
    (define (produtos-em-licitacao-aux)
      (define (prod-lic-aux lst lista-em-licitacao)
        (if (tipo-vazio? lst)
            (begin
              (display lista-em-licitacao)
              (newline))
            (if (iguais? 'em-licitacao (sexto (primeiro lst)))
                (begin
                  (set! lista-em-licitacao 
                        (insere (nome-p (primeiro lst)) lista-em-licitacao))
                  (prod-lic-aux (resto lst) lista-em-licitacao))
                (prod-lic-aux (resto lst) lista-em-licitacao))))
      (let ((lista-produtos-em-licitacao (list)))
        (prod-lic-aux lista-produtos lista-produtos-em-licitacao)))
    
    ;*********************
    ;* PRODUTOS VENDIDOS *
    ;*********************
    ;imprime no ecran uma lista que contém os nomes dos produtos vendidos
    (define (produtos-vendidos-aux)
      (define (prod-vendidos-aux lst lista-vendidos)
        (if (tipo-vazio? lst)
            (begin
              (display lista-vendidos)
              (newline))
            (if (iguais? 'vendido (sexto (primeiro lst)))
                (begin
                  (set! lista-vendidos
                        (insere (nome-p (primeiro lst)) lista-vendidos))
                  (prod-vendidos-aux (resto lst) lista-vendidos))
                (prod-vendidos-aux (resto lst) lista-vendidos))))
      (let ((lista-produtos-vendidos (list)))
        (prod-vendidos-aux lista-produtos lista-produtos-vendidos)))
    
    ;*************************
    ;* PRODUTOS NÃO VENDIDOS *
    ;*************************
    ;imprime no ecran uma lista que contém os nomes dos produtos não vendidos
    (define (produtos-nao-vendidos-aux)
      (define (prod-nao-vendidos-aux lst lista-nao-vendidos)
        (if (tipo-vazio? lst)
            (begin
              (display lista-nao-vendidos)
              (newline))
            (if (iguais? 'nao-vendido (sexto (primeiro lst)))
                (begin
                  (set! lista-nao-vendidos
                        (insere (nome-p (primeiro lst)) lista-nao-vendidos))
                  (prod-nao-vendidos-aux (resto lst) lista-nao-vendidos))
                (prod-nao-vendidos-aux (resto lst) lista-nao-vendidos))))
      (let ((lista-produtos-nao-vendidos (list)))
        (prod-nao-vendidos-aux lista-produtos lista-produtos-nao-vendidos)))
    
    ;**********
    ;* LICITA *
    ;**********
    ;faz uma licitação a um produto, verifica se ele existe e se a licitação é
    ;válida, acrescenta os campos da licitação e coloca-a na lista de licitações
    (define (licita-aux lic)
      (if (comprimento-2 lic)
          (let ((licitacao-a-inserir (cria-licitacao (ultimo (primeiro lic))
                                                     (segundo lic)
                                                     ((tempo-ip-bay 'd))
                                                     utilizador-activo)))
            (let ((nome-prod-lic-a-inserir (nome-produto-l licitacao-a-inserir)))
              (if (produto-existente? nome-prod-lic-a-inserir)
                    (if (licitacao-valida? licitacao-a-inserir nome-prod-lic-a-inserir)
                        (let ((pp-a-inserir (cria-pp nome-prod-lic-a-inserir
                                                     utilizador-activo
                                                     (valor-l licitacao-a-inserir)
                                                     (tempo-l licitacao-a-inserir)
                                                     0)))
                          (set! lista-licitacoes (insere licitacao-a-inserir lista-licitacoes))
                          (set! lista-processo-produto 
                                (insere pp-a-inserir lista-processo-produto)))
                        (mensagem-licitacao-invalida))
                    (mensagem-produto-inexistente nome-prod-lic-a-inserir))))
          (mensagem-argumentos-invalidos)))
    
    
    ;**********************************
    ;* VERIFICA PRODUTOS EM LICITAÇÃO *
    ;**********************************
    ;este método é chamado pelo passa-tempo - após actualizar o tempo, para verificar
    ;se existem produtos que já terminaram o seu processo de licitação
    (define (verifica-produtos-em-licitacao)
      (define (verifica-aux lst)
        (if (not (tipo-vazio? lst))
            (let ((produto-a-verificar (primeiro lst)))
              (let ((nome-a-usar (nome-p produto-a-verificar)))
                (if (and (iguais? 'em-licitacao (estado-p produto-a-verificar))
                         (depois? ((tempo-ip-bay 'd)) (tempo-p produto-a-verificar)))
                    (if (tem-licitacoes? nome-a-usar)
                        (begin
                          (set-sexto! produto-a-verificar 'vendido)
                          (let ((mensagem-nova
                                 (cria-mensagem nome-a-usar
                                                (utilizador-l (da-licitacao nome-a-usar))
                                                (dono-p produto-a-verificar)
                                                (valor-l (da-licitacao nome-a-usar))
                                                (tempo-l (da-licitacao nome-a-usar)))))
                            (escreve-mensagem mensagem-nova 1)
                            (set! lista-mensagens (insere mensagem-nova lista-mensagens)))
                          (verifica-aux (resto lst)))
                        (begin
                          (set-sexto! produto-a-verificar 'nao-vendido)
                          (let ((mensagem-nova
                                 (cria-mensagem nome-a-usar
                                                'nenhum
                                                (dono-p produto-a-verificar)
                                                0.0
                                                (tempo-p produto-a-verificar))))
                            (escreve-mensagem mensagem-nova 1)
                            (set! lista-mensagens (insere mensagem-nova lista-mensagens)))
                          (verifica-aux (resto lst))))
                      (verifica-aux (resto lst)))))))
      (if (not (tipo-vazio? lista-produtos))
          (verifica-aux lista-produtos)))
    
    
    ;**********************
    ;* VERIFICA MENSAGENS *
    ;**********************
    ;quando um utilizador entra no sistema chama este procedimento que irá
    ;verificar se existem mensagens a escrever no ecran para o utilizador
    (define (verifica-mensagens ut)
      (define (verifica-msgs-aux lst)
        (if (not (tipo-vazio? lst))
            (let ((mensagem-a-verificar (primeiro lst)))
              (if (e-comprador? ut mensagem-a-verificar)
                  (begin
                    (escreve-mensagem mensagem-a-verificar 0)
                    (set-segundo! mensagem-a-verificar 'nenhum)
                    (verifica-msgs-aux (resto lst)))
                  (if (e-vendedor? ut mensagem-a-verificar)
                      (begin
                        (escreve-mensagem mensagem-a-verificar 0)
                        (set-terceiro! mensagem-a-verificar 'nenhum)
                        (verifica-msgs-aux (resto lst)))
                      (verifica-msgs-aux (resto lst)))))))
      (if (not (tipo-vazio? lista-mensagens))
          (verifica-msgs-aux lista-mensagens)))
    
    (define (e-comprador? ut msg)
      (cond ((iguais? ut (comprador-m msg)))
            (else #f)))
    
    (define (e-vendedor? ut msg)
      (cond ((iguais? ut (vendedor-m msg)))
            (else #f)))
    
    ;********************
    ;* PROCESSO PRODUTO *
    ;********************
    ;escreve no ecran o processo do produto, ou seja, todas as acções 
    ;sobre ele tomadas até à data
    (define (processo-produto-aux nome-prod)
      (define (pp-aux lst)
        (cond ((tipo-vazio? lst))
              ((iguais? (ultimo (ultimo nome-prod)) (nome-prod-pp (primeiro lst)))
               ;se a licitação já tiver terminado faz isto
               (if (depois? ((tempo-ip-bay 'd)) (tempo-p (da-produto 
                                                          (ultimo (ultimo nome-prod)))))
                   (begin
                     (escreve-processo-produto-terminado 
                      (primeiro lst))
                     (pp-aux (resto lst)))
                   (begin
                     (escreve-processo-produto-normal 
                      (primeiro lst)
                      ((tempo-ip-bay 'd))
                      (tempo-p (da-produto (ultimo (ultimo nome-prod)))))
                     (pp-aux (resto lst)))))
              (else (pp-aux (resto lst)))))
      (pp-aux lista-processo-produto))
                
               
    ;***********************
    ;*  ESCOLHA DO MÉTODO  *
    ;***********************
    ;verificamos aqui qual o método a chamar entre os definidos.
    (lambda (tipo)
      (cond ((iguais? tipo 'c) cria-produto-aux)
            ((iguais? tipo 'd) dados-produto-aux)
            ((iguais? tipo 'l) produtos-em-licitacao-aux)
            ((iguais? tipo 'v) produtos-vendidos-aux)
            ((iguais? tipo 'n) produtos-nao-vendidos-aux)
            ((iguais? tipo 'pp) processo-produto-aux)
            ((iguais? tipo 'i) licita-aux)
            ((iguais? tipo 've) verifica-produtos-em-licitacao)
            ((iguais? tipo 'vm) verifica-mensagens)
            (else (display "comando desconhecido")))))
  
  ;**************************************
  ;*  VARIÁVEIS QUE GUARDAM INFORMAÇÃO  *
  ;**************************************
  ;variáveis e constantes que guardam informação
  ;determina o final do programa
  (define final 0)
  
  ;variável que guarda o utilizador activo. É inicializada com o utilizador 'nenhum
  (define utilizador-activo 'nenhum)
    
  ;constante que define o estado inicial com que são rotulados os produtos
  (define estado 'em-licitacao)
      
  ;definimos tempo-ip-bay como sendo um objecto que guarda a variável tempo
  ;e tem os métodos definidos pelo objecto-tempo: inicia-tempo, passa-tempo e
  ;visualiza-tempo.
  ;como o tempo ainda não foi inicializado, a variável tempo do tempo-ip-bay
  ;é inicializada como uma lista vazia.
  (define tempo-ip-bay (objecto-tempo (list)))
  
  ;lista de utilizadores
  ;como ainda não existem utilizadores as listas inicialmente estão vazias
  (define utilizador-ip-bay (objecto-utilizador (list) (list)))
  
  ;lista de categorias
  ;como inicialmente não existem categorias definidas a lista é vazia
  (define categoria-ip-bay (objecto-categoria (list)))
  
  ;variáveis guardadas pelo objecto produto actual, uma lista de produtos postos em
  ;venda e uma lista das licitações aos produtos
  (define produto-ip-bay (objecto-produto (list) (list) (list) (list)))
  
  
  ;**********************************
  ;* COMANDOS DO UTILIZADOR NENHUM  *
  ;**********************************
  
  (define (entrar)
    (set! utilizador-activo 'anonimo))
  
  (define (entra-utilizador utilizador)
    (if (tipo-vazio? utilizador)
        (mensagem-deve-introduzir-utilizador)
        ((utilizador-ip-bay 'eu) utilizador)))
  
  (define (terminar-ip-Bay)
    (set! final 1)
    (mensagem-final))
  
  ;***********************************
  ;* COMANDOS DO UTILIZADOR ANÓNIMO  *
  ;***********************************
  
  (define (regista utilizador)
    ((utilizador-ip-bay 'r) utilizador))
  
  ;*****************************************
  ;* COMANDOS DO UTILIZADOR ADMINISTRADOR  *
  ;*****************************************
  ;métodos que podem ser chamados pelo administrador. São sempre invocados os 
  ;métodos do objecto onde está guardada a variável tempo.
  
  (define (inicia-tempo args)
    ((tempo-ip-bay 'i) args))
  
  (define (inserir-categoria args)
    ((categoria-ip-bay 'i) args))
  
  (define (remover-categoria args)
    ((categoria-ip-bay 'r) args))
  
  
  ;*****************************************
  ;* COMANDOS DOS UTILIZADORES REGISTADOS  *
  ;*****************************************
  ;alguns destes comandos também se aplicam ao administrador quando ele apenas se
  ;apresenta como um utilizador registado normal e não faz uso dos seus comados
  ;específicos
  
  ;terminar execução de utilizador - também se aplica ao utilizador anónimo
  (define (sair)
    (set! utilizador-activo 'nenhum))
  
  (define (visualiza-tempo)
    ((tempo-ip-bay 'v)))
  
  (define (passa-tempo minutos)
    ((tempo-ip-bay 'p) (primeiro minutos)))
  
  ;este comando também se aplica ao utilizador anónimo
  (define (utilizadores-registados)
    ((utilizador-ip-bay 'u)))
  
  (define (nome novo-nome)
    (if (tipo-vazio? novo-nome)
        (mensagem-deve-introduzir-nome)
        ((utilizador-ip-bay 'n) (primeiro novo-nome))))
  
  (define (morada nova-morada)
    (if (tipo-vazio? nova-morada)
        (mensagem-deve-introduzir-morada)
        ((utilizador-ip-bay 'm) (primeiro nova-morada))))
  
  (define (email novo-email)
    (if (tipo-vazio? novo-email)
        (mensagem-deve-introduzir-email)
        ((utilizador-ip-bay 'e) (primeiro novo-email))))
  
  (define (password nova-pass)
    (if (tipo-vazio? nova-pass)
        (mensagem-deve-introduzir-pass)
        ((utilizador-ip-bay 'p) (primeiro nova-pass))))
  
  (define (dados-utilizador utilizador)
    ((utilizador-ip-bay 'd) utilizador))
  
  (define (categorias-disponiveis)
    ((categoria-ip-bay 'c)))
  
  (define (cria-produto dados)
    ((produto-ip-bay 'c) dados))
  
  (define (dados-produto nome)
    ((produto-ip-bay 'd) (ultimo (ultimo nome))))
  
  (define (produtos-em-licitacao)
    ((produto-ip-bay 'l)))
  
  (define (produtos-vendidos)
    ((produto-ip-bay 'v)))
  
  (define (produtos-nao-vendidos)
    ((produto-ip-bay 'n)))
            
  (define (licita dados)
    ((produto-ip-bay 'i) dados))           
  
  (define (processo-produto nome)
    ((produto-ip-bay 'pp) nome))
     
  ;  (define (processo-utilizador nome)
  ;    ((produto-ip-bay 'pu) nome
  
  
  ;**************************************
  ;*  verificação do utilizador activo  *
  ;**************************************
  ;funções que verificam que tipo de utilizador está activo
  (define (utilizador-nenhum?)
    (iguais? utilizador-activo 'nenhum))
  
  (define (utilizador-anonimo?)
    (iguais? utilizador-activo 'anonimo))
  
  (define (utilizador-administrador?)
    (iguais? utilizador-activo 'administrador))
  
  (define (utilizador-registado?)
    (and (not (utilizador-nenhum?))
         (not (utilizador-anonimo?))
         (not (utilizador-administrador?))))
  
  
  ;***************
  ;*  MENSAGENS  *
  ;***************
  ;mensagens que informam o utilizador do que se passa
  ;mensagem de boas vindas
  (define (mensagem-inicial)
    (display "Seja Bem-Vindo(a) ao Sistema de Leilões Ip-Bay")
    (newline))
  
  ;mensagem final
  (define (mensagem-final)
    (display "Até à Próxima."))
  
  ;mensagem impressa quando o utilizador dá um comando desconhecido
  (define (mensagem-comando-desconhecido)
    (display "ip-Bay: Erro! Comando desconhecido.")
    (newline))
  
  ;tempo inserido é inválido
  (define (mensagem-tempo-invalido)
    (display "ip-Bay: Erro! O tempo inserido não é válido.")
    (newline))
  
  (define (mensagem-produto-existente produto)
    (display "ip-Bay: Erro! O produto ")
    (display (nome-p produto))
    (display " já existe."))
  
  (define (mensagem-produto-inexistente nome-produto)
    (display "ip-Bay: Erro! O produto ")
    (display nome-produto)
    (display " não existe.")
    (newline))
  
  (define (mensagem-tempo-nao-inicializado)
    (display "ip-Bay: Info! Ainda não foi inicializado o tempo.")
    (newline))
  
  (define (numero-minutos-invalido)
    (display "ip-Bay: Erro! Número de minutos inválido.")
    (newline))
  
  (define (mensagem-utilizador-invalido)
    (display "ip-Bay: Erro! O utilizador inserido não é válido.")
    (newline))
  
  (define (mensagem-utilizador-ja-existe nick)
    (display "ip-Bay: Erro! O utilizador ")
    (display nick)
    (display " já existe no sistema.")
    (newline))
  
  (define (mensagem-utilizador-nao-existe nick)
    (display "ip-Bay: Erro! O utilizador ")
    (display nick)
    (display " não existe no sistema.")
    (newline))
  
  (define (mensagem-utilizador-registado-com-sucesso nick)
    (display "ip-Bay: O utilizador ")
    (display nick)
    (display " foi registado com sucesso.")
    (newline))
  
  (define (mensagem-utilizador-nao-existente)
    (display "ip-Bay: Erro! Utilizador não existente.")
    (newline))
  
  (define (numero-argumentos-invalido)
    (display "ip-Bay: Erro! O número de argumentos que inseriu não é válido.")
    (newline))
  
  (define (mensagem-nao-ha-utilizadores-registados)
    (display "ip-Bay: Info! Não existem utilizadores registados.")
    (newline))
  
  (define (mensagem-argumentos-invalidos)
    (display "ip-Bay: Erro! Argumento(s) inválido(s).")
    (newline))
  
  (define (mensagem-password-incorrecta)
    (display "ip-Bay: Erro! Password incorrecta.")
    (newline))
  
  (define (mensagem-nao-ha-categorias)
    (display "ip-Bay: Info! Não existem categorias.")
    (newline))
  
  (define (mensagem-deve-introduzir-categorias)
    (display "ip-Bay: Erro! Deve introduzir uma categoria.")
    (newline))
  
  (define (mensagem-categoria-existente cat)
    (display "ip-Bay: Erro! A categoria ")
    (display cat)
    (display " já existe.")
    (newline))
  
  (define (mensagem-categoria-nao-existente cat)
    (display "ip-Bay: Erro! A categoria ")
    (display cat)
    (display " não existe.")
    (newline))
  
  (define (mensagem-produto-ja-existe prod)
    (display "ip-Bay: Erro! O produto ")
    (display prod)
    (display " já existe no sistema.")
    (newline))
  
  (define (mensagem-deve-introduzir-utilizador)
    (display "ip-Bay: Erro! Deve introduzir um utilizador.")
    (newline))
  
  (define (mensagem-deve-introduzir-nome)
    (display "ip-Bay: Erro! Deve introduzir o novo nome.")
    (newline))
  
  (define (mensagem-deve-introduzir-morada)
    (display "ip-Bay: Erro! Deve introduzir a nova morada.")
    (newline))
  
  (define (mensagem-deve-introduzir-email)
    (display "ip-Bay: Erro! Deve introduzir o novo email.")
    (newline))
  
  (define (mensagem-deve-introduzir-pass)
    (display "ip-Bay: Erro! Deve introduzir a nova password.")
    (newline))
  
  (define (mensagem-licitacao-invalida)
    (display "ip-Bay: Erro! A licitação não é válida.")
    (newline))
  
  ;************
  ;*  PROMPT  *
  ;************
  ;prompt inicial: imprime a prompt de acordo com o utilizador activo
  (define (prompt-inicial)
    (cond ((utilizador-nenhum?)
           (display " > "))
          ((utilizador-anonimo?)
           (display "anonimo > "))
          (else
           (display utilizador-activo)
           (display " > "))))
  
  
  ;****************************************
  ;* FUNÇÕES DE INICIALIZAÇÃO DO SISTEMA  *
  ;****************************************
  ;funções a realizar uma só vez aquando da entrada no sistema
  (mensagem-inicial)
  (iniciar-aux))

;(iniciar-ip-Bay)

;==================================================== 
(define (descobriu-um-erro) 
  (display "ERRO!!!!!") 
  (newline) 
  (exit 0)) 

(define (run filename-input filename-output) 
  (let ((input (open-input-file filename-input)) 
        (output (open-output-file filename-output 'replace))) 
    (current-input-port input) 
    (current-output-port output) 
    (current-error-port output) 
    (error-escape-handler descobriu-um-erro) 
    (iniciar-ip-Bay) 
    (close-input-port input) 
    (close-output-port output))) 
;;===================================================== 
(run "teste-input.scm" "teste-output.scm")