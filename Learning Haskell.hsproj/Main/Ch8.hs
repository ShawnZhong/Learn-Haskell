module Ch8 where
  
import Data.Char
import ShapeGraphics



-- Lexing
data Token = PlusTok | TimesTok | OpenTok | CloseTok | IntTok Int deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr 
lexer ('(' : restStr) = OpenTok  : lexer restStr 
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (chr : restStr) 
  | isSpace chr       = lexer restStr
lexer str@(chr : _) 
  | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr where
     -- break splits a list into the leading elements for which
     -- the predicate evaluates to False
     (digitStr, restStr) = break (not . isDigit) str
     
     -- local function to convert a string to an integer value
     stringToInt :: String -> Int
     stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
lexer (chr : restString) = error ("lexer: unexpected character: '" ++ (show chr) ++ "'")










data Expr
  = IntLit Int          -- integer constants, leaves of the expression tree
  | Add    Expr Expr    -- addition node
  | Mult   Expr Expr    -- multiplication node
  deriving (Show)


-- Parsing integer literals
parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (IntTok n : restTokens) = Just (IntLit n, restTokens)
parseInt tokens = Nothing



-- Parsing products
parseProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseProdOrInt tokens
  = case parseInt tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result     -- could be 'Nothing' or a valid expression
      


-- Parsing sums
parseSumOrProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrInt tokens
  = case parseProdOrInt tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseProdOrInt restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result    -- could be 'Nothing' or a valid expression
      






-- Parsing parentheses
parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLit n,   restTokens)
parseIntOrParenExpr (OpenTok : restTokens1)
  = case parseSumOrProdOrIntOrParenExpr restTokens1 of
       Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
       Just _  -> Nothing -- no closing paren
       Nothing -> Nothing
parseIntOrParenExpr tokens
  = Nothing
      
parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parseIntOrParenExpr tokens of
      Just (expr1, (TimesTok : restTokens1)) -> 
          case parseProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result   
              
parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusTok : restTokens1)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens1 of
            Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
            Nothing                   -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr    
    _               -> error "Could not parse input"
    








-- Evaluator







eval :: Expr -> Int
eval (IntLit n) = n
eval (Add expr1 expr2)
  = eval expr1 + eval expr2
eval (Mult expr1 expr2)
  = eval expr1 * eval expr2  












































-- Auxilliary code for tree rendering
--

renderExpr :: Expr -> Picture
renderExpr expr = pic
  where 
    (_, _, pic) = renderExpr' expr

    renderExpr' :: Expr -> (Float, Float, Picture)
    renderExpr' (Add expr1 expr2) 
      = renderMS expr1 expr2 "Add  "
    renderExpr' (Mult expr1 expr2) 
      = renderMS expr1 expr2 "Mult "

    renderExpr' (IntLit x) 
      = borderedTextBox str
        where str = "IntLit " ++  show x

    renderMS expr1 expr2 str 
      = (treeWidth1 + treeWidth2 + dist, 
         treeWidth1 + dist/2,
         movePicture (Vector (treeWidth1 + dist/2 - nodeWidth/2) 0) pic ++
         movePicture (Vector 0 levelHeight) pic1 ++
         movePicture (Vector (dist + treeWidth1) levelHeight)  pic2 ++
         [Path [Point (treeWidth1 + dist/2 - nodeWidth/2)  nodeHeight, Point ancor1 levelHeight] grey Solid]  ++
         [Path [Point (treeWidth1 + dist/2 +nodeWidth/2)  nodeHeight, Point (ancor2 + dist + treeWidth1) levelHeight] grey Solid]
         )
      where
        (treeWidth1, ancor1, pic1) = renderExpr' expr1
        (treeWidth2, ancor2, pic2) = renderExpr' expr2
        (nodeWidth, _, pic) = borderedTextBox str
         
    borderedTextBox :: String -> (Float, Float, Picture)    
    borderedTextBox str 
      = (nodeWidth,
         nodeWidth/2, 
         [TextBox (Point (chrSize/5) (0.8*nodeHeight)) grey str chrSize, 
            Polygon [Point 0 0, Point 0 nodeHeight, 
                     Point nodeWidth nodeHeight, Point nodeWidth 0, 
                     Point 0 0] 
                    black Solid NoFill])
      where
        strLen    = fromIntegral $ length $ str
        nodeWidth = strLen * chrSize/1.6
    dist     = 40
    levelHeight = 100
    chrSize  = 20 -- point size
    nodeHeight = 1.5 * chrSize