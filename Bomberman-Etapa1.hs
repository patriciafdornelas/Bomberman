import Data.Array ( Array, array )

--data Presente = Patins | Arremesso

data Jogador = Jogador
data Celula = Pedra | Bomba | Presente | Grama | Parede  | Patins | Arremesso deriving (Show,Eq,Read)
type Linha = (Int,Int)


--Cria uma matriz que se refere ao tabuleiro e adiciona parede ao redor do tabuleiro

tabuleiro ::  Array Linha [Celula]
tabuleiro = array ((1,1),(8,8)) [((1,1),opc4),((1,2),opc4),((1,3),opc4),((1,4),opc4),((1,5),opc4),((1,6),opc4),((1,7),opc4),((1,8),opc4),
                  ((2,1),opc4),((2,2),opc5),((2,3),opc5),((2,4),opc3),((2,5),opc2),((2,6),opc3),((2,7),opc1),((2,8),opc4),
                  ((3,1),opc4),((3,2),opc5),((3,3),opc1),((3,4),opc3),((3,5),opc3),((3,6),opc2),((3,7),opc3),((3,8),opc4),
                  ((4,1),opc4),((4,2),opc2),((4,3),opc2),((4,4),opc3),((4,5),opc3),((4,6),opc1),((4,7),opc3),((4,8),opc4),
                  ((5,1),opc4),((5,2),opc3),((5,3),opc3),((5,4),opc2),((5,5),opc1),((5,6),opc3),((5,7),opc1),((5,8),opc4),
                  ((6,1),opc4),((6,2),opc1),((6,3),opc3),((6,4),opc1),((6,5),opc3),((6,6),opc3),((6,7),opc2),((6,8),opc4),
                  ((7,1),opc4),((7,2),opc3),((7,3),opc1),((7,4),opc3),((7,5),opc2),((7,6),opc2),((7,7),opc3),((7,8),opc4),
                  ((8,1),opc4),((8,2),opc4),((8,3),opc4),((8,4),opc4),((8,5),opc4),((8,6),opc4),((8,7),opc4),((8,8),opc4)]
                  where 
                     opc1 = [Grama, Patins, Pedra]
                     opc2 = [Grama, Arremesso, Pedra]
                     opc3 = [Grama, Pedra]
                     opc4 = [Parede]
                     opc5 = [Grama]

type Localização = (Int,Int)
data Direcao = N | S | L | O  
type Capacidades = (String, Int)

--jogador quando inicia o jogo

jogador :: (Localização,Direcao,Capacidades)
jogador = ((2,2),L,("Nada",0))

-- Movimentação de um jogador em qualquer dos sentidos

direcao :: String
direcao = "NSLO"


move:: Char -> Array Linha [Celula]
move x
   |(elem x direcao == True) && (x == 'N') = array ((1,1),(8,8)) [((i+1,j),[Grama])|i<-[1..8],j<-[1..8]]  --para ir pro norte (_+1,continua a mesma)
   |(elem x direcao == True) && (x == 'S') = array ((1,1),(8,8)) [((i-1,j),[Grama])|i<-[1..8],j<-[1..8]]  --para ir pro sul (_-1, continua a mesma)
   |(elem x direcao == True) && (x == 'L') = array ((1,1),(8,8)) [((i,j+1),[Grama])|i<-[1..8],j<-[1..8]]  --para ir pro leste(continua a mesma, _+1)
   |(elem x direcao == True) && (x == 'O') = array ((1,1),(8,8)) [((i,j-1),[Grama])|i<-[1..8],j<-[1..8]]  --para ir pro oeste(continua a mesma,_-1)

-- tabuleiro ::  Array Linha [Celula]
-- tabuleiro = array ((1,1),(8,8)) [((i,j),if (i == 1) || (j == 1) || (i == 8) || (j == 8) || ((i == 3)&&(j == 3)) || ((i == 3)&&(j == 6))
--    || ((i == 6)&&(j == 3)) || ((i == 6)&&(j == 6)) then [Parede] 
--    else if ((i == 2)&&(j == 2)) || ((i == 2)&&(j == 3)) || ((i == 3)&&(j == 2)) then [Grama] else [Pedra])|i<-[1..8],j<-[1..8]]

-- tabuleiro ::  Array Linha [Celula]
-- tabuleiro = array ((1,1),(8,8)) [((i,j),[Pedra])|i<-[1..8],j<-[1..8]]

-- Função que receba um tabuleiro e uma instrução de movimentação de um jogador e retorne um novo tabuleiro, com o jogador na nova posição.
-- Jogador só pode se deslocar para célula adjacente que não tenha pedra ou bomba
-- Pode ser impossível ao jogador se deslocar
-- Ao se deslocar para uma célula vazia, cai no buraco
-- Ao se deslocar para uma célula com um presente, o coleta
-- Coleta de presente
-- Ao coletar um presente, incrementa a posição correspondente ao presente nas suas capacidades
-- Arremesso
-- Se estiver adjacente, olhando para uma bomba, e tiver a capacidade "arremesso", o jogador arremessa a bomba uma distância proporcional à capacidade.
-- Teste de direção
-- Explosão

-- Direção
-- Capacidade
-- Eliminação de presentes
-- Eliminação de pedra
-- Eliminação de jogador
-- Detecção de fim de jogo



