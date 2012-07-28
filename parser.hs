module Parser
	 where
import Template
import Text.ParserCombinators.Parsec hiding (State)
import Data.Maybe
import Data.List(nub)
import Language.Haskell.TH
import Control.Monad.State(modify, State, execState)
import Logic
import Data.List.Split(splitOn)
import Data.Maybe
import Text.Printf
import qualified Data.Map.Strict as Map

data Expr = Expr String [String] deriving Show

data Chunk = Part String | KV String String | K String | KF String Expr deriving Show


--add_bi_rule i1 i2 cmd o = modify $ (:) (create_rule cmd [(fp i1),(fp i2)]  (fp o))

--add_rule i cmd o  = modify $ (:) (create_rule cmd [(fp i)]  (fp o))
--build_rules :: State ([Rule]) () -> [Rule]
--build_rules f = execState f []



fileName :: GenParser Char st [Chunk]
fileName = do 
             result <- many chunk
       	     eof
       	     return result

chunk = part  <|> try kvPair <|> try kfPair <|> try keyOnly

part = do 
		s <-  many1 (noneOf "(),\n=$")
		return (Part s)

kvPair =
	do k <- key 
	   _ <- char '=' 
	   v <- value
	   return (KV  k v)

kfPair =
    do k <- key
       _ <- char '='
       f <- expr
       return (KF k f)

expr = 
    do
        _ <- char '('
        fun <- many1 (noneOf "()\n ")    
        params <- many key
        _ <- char ')'    
        return (Expr fun params)


keyOnly = do
	s <- key
	return (K s)

key :: GenParser Char st String
key = do _ <- spaces
	 d <- dollar
	 many1 (noneOf "=,\n$-/_() ")

value :: GenParser Char st String
value = do
       _ <- spaces
       many1 (noneOf "=,\n$-/_() ")
        

dollar = char '$'
eq :: GenParser Char st Char
eq = char '='


parseFileName :: String -> Either ParseError [Chunk]
parseFileName input = parse fileName "(unknown)" input

get_chunks s = 
       case parseFileName s  of
            (Left err) -> error (show err)
            (Right r) -> r 

get_params s =
    let chunks = case parseFileName s  of
            (Left err) -> error (show err)
            (Right r) -> r in 
    let collectParam chunk = case chunk of 
                        K k -> Just k
                        KV k _ -> Just k
                        KF k _ -> Just k
                        _ -> Nothing in
    -- nub == sort | uniq 
    let params = nub $ mapMaybe collectParam chunks in
    params

my_concat = (++)
appEE f p1 p2 = 
    (AppE (AppE f p1) p2)

{- create_printf "%s %s" [a, b] == printf "%s %s a b -}
create_printf::[Chunk]->Exp
create_printf chunks =  join $ map toE chunks
    where 
        toE (Part s) = (LitE (stringL s))
        toE (K s) = showE (VarE (mkName s))
      
        join = foldl (\acc e -> appEE concatE acc e)  empty 
        empty = (LitE (stringL "" ))
        concatE = (VarE (mkName "my_concat"))
        {-
        apply f (Part s) = (AppE f ()
        printfStub = printfE printfP  -- printf "%s %s"
        printfE = (AppE (VarE (mkName "printf")))
        printfP = (LitE (stringL format ))
        -}
        --params = map  showE $ map VarE $ map mkName l
        showE = (AppE (VarE (mkName "show")))

input_file  s = 
        return $ (LamE varps body)
        --return $ [ FunD ( mkName n) [ ( Clause varps (NormalB body) []) ] ]
        where   
            varps = (map (\n -> VarP n) names)
            body = (AppE (ConE (mkName "InputFile")) (create_printf (chunks)))
            chunks = get_chunks s
            names = map mkName params
            params = get_params s

rule::[DepGraph]->File->a ->Exp
rule deps output c = (AppE (AppE (AppE (ConE (mkName "GeneratedFile")) depsE) outputE) cmdE)
    where
        depsE = undefined
        outputE = undefined
        cmdE = undefined
  {-       
  GeneratedFile deps myOutput cmd
  where
    cmd = grep_command (napi_log y m d)
    deps = [napi_log y m d]
    myOutput = "user-" ++ show y ++ "-" ++ show m ++ "-" ++ show d
 -}
    -- p_0 <- newName "p"
        -- Prelude Language.Haskell.TH> runQ [| \p -> head p |] >>= print
        -- LamE [VarP p_0] (AppE (VarE GHC.List.head) (VarE p_0))
    --    return (LamE [VarP p_0] (AppE (VarE (  mkName fun_name)) (VarE p_0)))

--main = do
 --   print $ input_file "$year $month $day"
