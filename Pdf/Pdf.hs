module PDF () where
--  ( Document

  -- document part
--  , createDocument -- :: Size -> 

---------------------------------------------------------------------
-- the different datatypes

data Command 
  = Td Int Int
  | Tf Font Int
  | TL Int
  | Quote String
  | Tj String
  | Tc Int
  | Tw Int
  | Tr Int
  | Cm Float Float Float Float 

type Commands = [Command]

type Page  = (Int, Int, Commands)
type Pages = [Page]

type Font = String

--type Pos = (Int, Int)
--type Size = (Int, Int)

--type Area = (Block, Pos)

--type Page = (Size, [Area])

---------------------------------------------------------------------
-- exported functions

-- document --
--createPdf :: [Page] -> String

-- page --
--createPage :: Size -> Page

-- area --
--position :: Block -> Pos -> Area
--position b p = (b, p)

-- block --
--textBlock :: [Attr] -> [String] -> Block
--textBlock a t = TextBlock a t

--graphicsBlock :: [Attr] -> [Graphic] -> Block
--graphicsBlock a g = GraphicsBlock a g

---------------------------------------------------------------------
-- tests

createPdf :: Pages -> String
createPdf []            = " "
createPdf ((_,_,Tf f s):cs) = "Tf " ++ (lookupFont f) ++ " " ++ (show s)
createPdf ((_,_,Tf f s):cs) = "Tf " ++ (lookupFont f) ++ " " ++ (show s)
createPdf ((_,_,Tf f s):cs) = "Tf " ++ (lookupFont f) ++ " " ++ (show s)

createPage :: (Int, Int) -> Commands -> Page
createPage (w,h) c = (w,h,c)

lookupFont :: String -> String
lookupFont "Helvetica" = "/F0"
lookupFont _           = "/F1"


font :: String -> Command
font f = Tf f undefined

size :: Int -> Command
size s = Tf undefined s

move :: Int -> Int -> Command
move x y = Td x y

textln :: String -> Command
textln s = Quote s

text :: String -> Command
text s = Tj s

test = createPdf 
  [ createPage (595, 842)
    [ size 15
	, move 50 150
	, textln "Testar PDF..."
	, text "Eller?"
	]
  ]
