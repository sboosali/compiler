module Lexer where
import IO
import AST


numeric = ['0'..'9']
alphanumeric = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
alphabetic = ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
whitespace = " \t\r\n"

startsAN str = head str `elem` alphanumeric

tokenize :: String -> [Token]
tokenize = reverse . parse' []

parse' :: [Token] -> String -> [Token]
parse' acc []             = acc
parse' acc ('/':'*':rest) = parseComment 1 acc rest
parse' acc ('*':'/':rest) = error "saw \"*/\" without \"/*\"\n"

parse' acc ('\"':rest)    = parseString [] acc rest

parse' acc ('-':'>':rest) = parse' (TokenArrow : acc) rest
parse' acc ('<':'=':rest) = parse' (TokenLEQ : acc) rest
parse' acc ('>':'=':rest) = parse' (TokenGEQ : acc) rest
parse' acc (':':'=':rest) = parse' (TokenAssign : acc) rest
parse' acc ('<':'>':rest) = parse' (TokenNeq : acc) rest
parse' acc ('+':rest)     = parse' (TokenPlus : acc) rest
parse' acc ('-':rest)     = parse' (TokenMinus : acc) rest
parse' acc ('*':rest)     = parse' (TokenTimes : acc) rest
parse' acc ('/':rest)     = parse' (TokenDivide : acc) rest
parse' acc ('=':rest)     = parse' (TokenEq : acc) rest
parse' acc ('<':rest)     = parse' (TokenLT : acc) rest
parse' acc ('>':rest)     = parse' (TokenGT : acc) rest
parse' acc ('&':rest)     = parse' (TokenAnd : acc) rest
parse' acc ('|':rest)     = parse' (TokenOr : acc) rest
parse' acc ('(':rest)     = parse' (TokenLParen : acc) rest
parse' acc (')':rest)     = parse' (TokenRParen : acc) rest
parse' acc ('[':rest)     = parse' (TokenLBracket : acc) rest
parse' acc (']':rest)     = parse' (TokenRBracket : acc) rest
parse' acc ('{':rest)     = parse' (TokenLBrace : acc) rest
parse' acc ('}':rest)     = parse' (TokenRBrace : acc) rest
parse' acc (';':rest)     = parse' (TokenSemicolon : acc) rest
parse' acc (':':rest)     = parse' (TokenColon : acc) rest
parse' acc ('.':rest)     = parse' (TokenDot : acc) rest
parse' acc (',':rest)     = parse' (TokenComma : acc) rest

parse' acc s@('t':'y':'p':'e':rest) | null rest     = TokenType : acc
                                    | startsAN rest = parseId "" acc s
                                    | otherwise     = parse' (TokenType : acc) rest

parse' acc s@('a':'r':'r':'a':'y':rest) | null rest     = TokenArray : acc
                                        | startsAN rest = parseId "" acc s
                                        | otherwise     = parse' (TokenArray : acc) rest
  
parse' acc s@('o':'f':rest) | null rest     = TokenOf : acc
                            | startsAN rest = parseId "" acc s
                            | otherwise     = parse' (TokenOf : acc) rest

parse' acc s@('v':'a':'r':rest) | null rest     = TokenVar : acc
                                | startsAN rest = parseId "" acc s
                                | otherwise     = parse' (TokenVar : acc) rest

parse' acc s@('f':'u':'n':'c':'t':'i':'o':'n':rest) | null rest     = TokenFunction : acc
                                                    | startsAN rest = parseId "" acc s
                                                    | otherwise     = parse' (TokenFunction : acc) rest

parse' acc s@('n':'i':'l':rest) | null rest     = TokenNil : acc
                                | startsAN rest = parseId "" acc s
                                | otherwise     = parse' (TokenNil : acc) rest

parse' acc s@('b':'r':'e':'a':'k':rest) | null rest     = TokenBreak : acc
                                        | startsAN rest = parseId "" acc s
                                        | otherwise     = parse' (TokenBreak : acc) rest

parse' acc s@('i':'f':rest) | null rest     = TokenIf : acc
                            | startsAN rest = parseId "" acc s
                            | otherwise     = parse' (TokenIf : acc) rest

parse' acc s@('t':'h':'e':'n':rest) | null rest     = TokenThen : acc
                                    | startsAN rest = parseId "" acc s
                                    | otherwise     = parse' (TokenThen : acc) rest

parse' acc s@('e':'l':'s':'e':rest) | null rest     = TokenElse : acc
                                    | startsAN rest = parseId "" acc s
                                    | otherwise     = parse' (TokenElse : acc) rest

parse' acc s@('f':'o':'r':rest) | null rest     = TokenFor : acc
                                | startsAN rest = parseId "" acc s
                                | otherwise     = parse' (TokenFor : acc) rest

parse' acc s@('t':'o':rest) | null rest     = TokenTo : acc
                            | startsAN rest = parseId "" acc s
                            | otherwise     = parse' (TokenTo : acc) rest

parse' acc s@('d':'o':rest) | null rest     = TokenDo : acc
                            | startsAN rest = parseId "" acc s
                            | otherwise     = parse' (TokenDo : acc) rest

parse' acc s@('l':'e':'t':rest) | null rest     = TokenLet : acc
                                | startsAN rest = parseId "" acc s
                                | otherwise     = parse' (TokenLet : acc) rest

parse' acc s@('i':'n':rest) | null rest     = TokenIn : acc
                            | startsAN rest = parseId "" acc s
                            | otherwise     = parse' (TokenIn : acc) rest

parse' acc s@('e':'n':'d':rest) | null rest     = TokenEnd : acc
                                | startsAN rest = parseId "" acc s
                                | otherwise     = parse' (TokenEnd : acc) rest

parse' acc (' ':rest)                             = parse' acc rest
parse' acc ('\t':rest)                            = parse' acc rest
parse' acc ('\r':rest)                            = parse' acc rest
parse' acc ('\n':rest)                            = parse' acc rest

parse' acc (c:rest) | c `elem` numeric    = parseNumber [c] acc rest
                    | c `elem` alphabetic = parseId [c] acc rest

parse' acc (c:rest) = error $ "invalid character \'" ++ [c] ++ "\'"


parseNumber :: String -> [Token] -> String -> [Token]
parseNumber digits acc (digit:rest) 
    | digit `elem` numeric = parseNumber (digit : digits) acc rest 
    | digit `elem` alphabetic
        = error "num cannot be followed by a letter or '_'"
    | otherwise = parse' ((TokenInteger $ read $ reverse digits) : acc) (digit : rest)
parseNumber digits acc [] = parse' ((TokenInteger $ read $ reverse digits) : acc) []


parseId :: String -> [Token] -> String -> [Token]
parseId chars acc (char:rest) 
    | char `elem` alphanumeric = parseId (char : chars) acc rest 
    | otherwise = parse' ((TokenID $ reverse chars) : acc) (char : rest)
parseId chars acc [] = parse' ((TokenID $ reverse chars) : acc) []


parseString :: [Char] -> [Token] -> String -> [Token]
parseString chars acc ('\"':rest) = parse' ((TokenString $ reverse chars) : acc) rest

parseString chars acc ('\\':'n':rest)   = parseString ('\n':chars) acc rest
parseString chars acc ('\\':'t':rest)   = parseString ('\t':chars) acc rest
parseString chars acc ('\\':'r':rest)   = parseString ('\r':chars) acc rest
parseString chars acc ('\\':'\"':rest)  = parseString ('\"':chars) acc rest
parseString chars acc ('\\':'\\':rest)  = parseString ('\\':chars) acc rest

parseString chars acc ('\\':rest) = eatWhitespace chars acc rest
parseString chars acc (char:rest) = parseString (char:chars) acc rest
parseString chars acc [] = error "missing \'\"\'"


eatWhitespace chars acc ('\\':rest) = parseString chars acc rest
eatWhitespace chars acc (' ':rest)  = eatWhitespace chars acc rest
eatWhitespace chars acc ('\n':rest) = eatWhitespace chars acc rest
eatWhitespace chars acc ('\t':rest) = eatWhitespace chars acc rest
eatWhitespace chars acc ('\r':rest) = eatWhitespace chars acc rest
eatWhitespace chars acc (c:rest)    = error $ '\'':c:"\' is not a valid escape character or whitespace"


parseComment :: Int -> [Token] -> String -> [Token]
parseComment 0 acc rest           = parse' acc rest
parseComment n acc ('/':'*':rest) = parseComment (n + 1) acc rest
parseComment n acc ('*':'/':rest) = parseComment (n - 1) acc rest
parseComment n acc (_:rest)       = parseComment n acc rest
parseComment _ _   []             = error "dangling \"/*\"\n"