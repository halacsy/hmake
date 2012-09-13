module Schema where
data Typ = I | S | L | Bool | T Schema | B Schema deriving (Show, Eq)
type Name = Maybe String
type NamedT = (Name, Typ)
type Schema = [NamedT]

