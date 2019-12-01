module To_Do where 


main:: IO ()
main = do
   putStrLn "Lista de Tarefas"
   let initialList = []
   interactWithUser initialList
   putStrLn "Obrigado por me complementar com as suas tarefas. I achieved hapiness...Kiss kiss!!!"

type Nota = String
type Notas = [Nota]

data Comandos
  = Sair
  | MostrarNotas
  | AdicionarNota String
  | Feito Int
  | Ajuda

parseComandos :: String -> Either String Comandos
parseComandos line = case words line of 
   ["sair"] -> Right Sair
   ["tarefas"] -> Right MostrarNotas
   "adicionar" : "-" : n -> Right (AdicionarNota (unwords n))
   ["ajuda"] -> Right Ajuda
   _ -> Left "Esse comando não existe meu caro"


interactWithUser :: Notas -> IO ()
interactWithUser ns = do
   line <- getLine
   case parseComandos line of
    Right Ajuda -> do
     putStrLn "Comandos: sair, tarefas, adicionar - <o que desejar adicionar...>, feito <índice da nota>"
     interactWithUser ns
   
    Right MostrarNotas -> do
     putStrLn "A sua lista de tarefas é a seguinte:"
     putStrLn (displayNotas ns)
     interactWithUser ns

    Right (AdicionarNota n) -> do
     let novasNotas =  addNota n ns
     putStrLn "Tarefa adicionada"
     interactWithUser novasNotas

    Right Sair -> do
     putStrLn "Adeusinho"
     pure ()

    Right (Feito index) -> do
     let resultado = removerNota index ns
     case resultado of
      Left errMsg -> do
       putStrLn ("Error:" ++ errMsg)
       interactWithUser ns
      Right novasNotas -> do
       putStrLn "Tarefa feita."
       interactWithUser novasNotas

    Left errMsg -> do
     putStrLn ("Error:" ++ errMsg)
     interactWithUser ns


addNota:: Nota -> Notas -> Notas
addNota n ns = n : ns

displayNotas :: Notas -> String
displayNotas ns =
    let 
     mostrarNota index n = show index ++ " - " ++ n
     reversedList = reverse ns
     mostrarNotasList = zipWith mostrarNota [1..] reversedList
    in
     unlines mostrarNotasList

removerNota :: Int -> Notas -> Either String Notas 
removerNota reverseIndex allNotas =
   impl (length allNotas - reverseIndex) allNotas
  where 
   impl index ns = 
    case (index, ns) of
     (0, n : rest) -> 
      Right rest 
     (x, []) -> 
      Left "Indice não existente"
     (x, n : rest ) -> 
      case impl (x - 1) rest of
       Right novasNotas -> 
        Right (n: novasNotas)
       Left errMsg -> 
        Left errMsg 

