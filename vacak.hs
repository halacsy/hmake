type Ablak = String -> String
instance Show Ablak where
	show f = "function"
a::Ablak
a=  (\s -> s ++ "hello")

main = do
	print a
