import System.IO
import Data.Char
import Data.List
import Data.Tuple

type Doc = String
type Line = String
type Word = String

main = do
    putStrLn "Name a file that you want to read: "
    hFlush stdout
    fileName <- getLine
    content <- readFile fileName
    let contentFiltrado = trata content
    let textLines = lines contentFiltrado
    let numLines = zip [1..length textLines] textLines
    let allWords = word' numLines
    let conca = inverte (concLista allWords)
    let ordenado = sortLs conca
    let amalgado = inverte (amalgamate ordenado)
    let removido = remFinal amalgado
    putStrLn  (imprimir removido)
    

-- Transforma a string de linhas em strings menores de palavras
word' []= []
word' ((a,b):xs)= zip (map (\x -> a) [1..]) (words b) : word' xs

-- Realiza o tratamento nas strings
trata [] = []
trata (x:xs) |  isLetter x || isSpace x == True = toLower x : trata xs
             |  isLetter x || isSpace x == False = trata xs

-- Ordena segundo a tabela ASCII
sortLs xs = sort xs

-- Inverte as duplas da lista
inverte [] = []
inverte ((a,b):xs) = (b,a):inverte xs

-- Concatena as listas dentro da lista
concLista [] = []
concLista (x:xs) = x++concLista xs 

-- 
amalgamate []=[]
amalgamate ((a,b):xs) = (a,(b:indice a xs)): amalgamate (removeElemento a xs)


-- Remove palavras repetidas da lista
removeElemento _ [] = []
removeElemento p ((a,b):xs) | p==a = removeElemento p xs
                        | otherwise = (a,b):removeElemento p xs

-- Cria uma lista das linhas em que uma palavra repete
indice _ [] = []
indice n ((a,b):xs) | n==a = b: indice n xs 
                    | otherwise = indice n xs


-- removeNum [] = [] 
-- removeNum ((a,b):xs) = (((head a):filter (\x -> x /= (head a) ) (tail a)),b) : removeNum xs
removeNum [] = [] 
removeNum (x:xs) = x:filter (\w -> w /= x) (removeNum xs)


remFinal [] = []
remFinal ((a,b):xs) = ((removeNum a),b) : remFinal xs

imprimir [] = []
imprimir ((a,b):xs) ="A palavra: "++b++" aparece na(s) linha(s): "++show(a)++"\n" ++ imprimir xs