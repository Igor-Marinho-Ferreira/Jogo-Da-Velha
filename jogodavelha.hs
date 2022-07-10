import Control.Exception
import System.IO
import System.IO.Error
import System.Process


-- definição dos tipos dos dados
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Tabela = [Char]
data Jogador = Jogador Nome Pontuacao
			deriving (Show, Read)


-- função que recebe uma String e retorna uma IO String
getString :: String -> IO String
getString str = do
		putStr str
		res <- getLine
		return res


-- função que inicia o programa
inicio :: IO ()
inicio = do
		{catch (ler_arquivo) tratar_erro;}
		where
			-- tenta ler o arquivo
			ler_arquivo = do
			{
				arq <- openFile "dados.txt" ReadMode; 
				dados <- hGetLine arq; -- ler o conteúdo do arquivo
				hClose arq;
				menu (read dados); -- passa os dados para a função menu
				return ()
			}
			tratar_erro erro = if isDoesNotExistError erro then do
			{
				-- se o arquivo NÃO existir, então cria o arquivo
				arq <- openFile "dados.txt" WriteMode; 
				hPutStrLn arq "[]"; -- escreve uma lista vazia no arquivo
				hClose arq; -- 
				menu []; -- passa uma lista vazia para o menu
				return ()
			}
			else
				ioError erro


-- função que exibe o Menu
menu :: Jogadores -> IO Jogadores
menu dados = do
		system "cls" 
		putStrLn "-------------------------------- Jogo da Velha --------------------------------"
		putStrLn "\nDigite 1 para cadastrar jogador"
		putStrLn "Digite 2 para jogar"
		putStrLn "Digite 3 para visualizar o ranking"
		putStrLn "Digite 0 para sair"
		putStr "Opção: "
		op <- getChar
		getChar 
		executarOpcao dados op


-- função para manipular a opção escolhida pelo usuário
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '0' = do
				putStrLn ("\nBye! https://github.com/Igor-Marinho-Ferreira\n")
				return dados
executarOpcao dados _ = do
				putStrLn ("\nOpção inválida! Tente novamente...")
				putStr "\nPressione <Enter> para voltar ao menu..."
				getChar
				menu dados


-- função responsável pelo cadastro de jogadores
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
				nome <- getString "\nDigite um nome de usuário: "
				if (existeJogador dados nome) then do
					putStrLn "\nEsse nome já existe, escolha outro."
					putStr "\nPressione <Enter> para continuar..."
					getChar
					menu dados
				else do
					arq <- openFile "dados.txt" WriteMode -- abre o arquivo para escrita
					hPutStrLn arq (show ((Jogador nome 0):dados))
					hClose arq -- fecha o arquivo
					putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso.")
					putStr "\nPressione <Enter> para continuar..."
					getChar
					menu ((Jogador nome 0):dados) -- retorna a nova lista para o menu


-- função que verifica se um jogador existe (o nome do jogador é único)
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
		| (n == nome) = True
		| otherwise = existeJogador xs nome