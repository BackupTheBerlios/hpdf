module Pdf 
  -- document part
  ( createPdf  -- :: PageSize -> [Page] -> String
  , createPage -- :: [Area] -> Page
  , pageSize   -- :: PageSize -> (Int, Int, Int, Int)
  
  -- area part
  , position -- :: Block -> Double -> Double -> Area
  , rotate   -- :: Double -> Area -> Area
  , scale    -- :: Double -> Double -> Area -> Area
  
  -- block part
  , textBlock     -- :: [Command] -> Block
  , graphicsBlock -- :: [Command] -> Block
  , over          -- :: Block -> Block -> Block
  , under         -- :: Block -> Block -> Block
  , rightAlign    -- :: Block -> Block
  
  -- commands part
  , setFont   -- :: String -> Int -> Command 
  , write     -- :: String -> Command
  , writeLn   -- :: String -> Command
  , line      -- :: Int -> Int -> Int -> Int -> Command
  , lineTo    -- :: Int -> Int -> Command

  -- data constructors
  , A3        -- :: PageSize
  , A4        -- :: PageSize
  , Letter    -- :: PageSize
  , Helvetica -- :: Font
  , Courier   -- :: Font
  , Times     -- :: Font
  , Arial     -- :: Font
  ) where

---------------------------------------------------------------------
-- the datatypes

data Command 
  = Text TextCommand
  | Graph GraphCommand 
  | Comm CommonCommand

data TextCommand
  = Tf String Int
  | Quote String
  | Tj String
  | G Int
  | Tz Int
  | T'
  | BT
  | ET

data GraphCommand
  = Line Int Int Int Int
  | LineTo Int Int 
  | Stroke

data CommonCommand
  = QSave
  | QLoad
  | QLoadSave
  | Cm Double Double Double Double Double Double

data Font 
  = Helvetica
  | Courier
  | Times
  | Arial

data Block = Block [Command]
type Area = (Double, Double, Block)
type Page = [Area]
data PageSize 
  = A3
  | A4
  | Letter
type Object = String
  
---------------------------------------------------------------------
-- exported functions

-- document --
createPdf :: PageSize -> [Page] -> String
createPdf size ps = objString ++ (trailer objects xrefOffset) 
 where 
  (objString, objects, xrefOffset) = arrangeObjects (static ++ pages)
  static = [ objDictionary 1 ["/Type/Catalog", "/Pages 2 0 R"]
           , objDictionary 2 ["/Type/Pages", "/Count " ++ (show (length ps))
                             , "/Kids [" ++ kids ++ "]"]
    	   , objDictionary 3 ["/ProcSet [/PDF/Text]", "/Font <</F0 4 0 R /F1 5 0 R>>"]
           , objDictionary 4 ["/Type/Font", "/Subtype/Type1"
                             , "/Encoding/WinAnsiEncoding"
                             , "/BaseFont/Helvetica", "/Name/F0" ]
           , objDictionary 5 ["/Type/Font", "/Subtype/Type1"
                             , "/Encoding/WinAnsiEncoding"
                             , "/BaseFont/Courier", "/Name/F1" ]
           , objDictionary 6 ["/Type/Font", "/Subtype/Type1"
                             , "/Encoding/WinAnsiEncoding"
                             , "/BaseFont/Arial", "/Name/F1" ]
           , objDictionary 7 ["/Type/Font", "/Subtype/Type1"
                             , "/Encoding/WinAnsiEncoding"
                             , "/BaseFont/Times", "/Name/F1" ]
           ]
  pages  = translatePages (pageSize size) 8 ps
  kids   = unwords (take (length ps) (map (\n -> (show (n)) ++ " 0 R") [8,11..]))
  
  trailer :: Int -> Int -> String
  trailer n off = "trailer\n<<\n\t/Size " ++ (show n) ++
    "\n\t/Root 1 0 R\n>>\nstartxref\n" ++
	(show off) ++ "\n%%EOF"
  

createPage :: [Area] -> Page
createPage = id

pageSize :: PageSize -> (Int, Int, Int, Int)
pageSize A3     = (0, 0, 842, 1190)
pageSize A4     = (0, 0, 595, 842)
pageSize Letter = (0, 0, 612, 792)
pageSize _  = error "Unknown pagesize"

-- area --
position :: Block -> Double -> Double -> Area
position b x y = (x, y, b)

rotate :: Double -> Area -> Area
rotate deg (x, y, (Block b)) = (x, y, Block (rot:b))
 where
  rad = deg * 3.14 / 180.0
  rot = Comm (Cm (cos rad) (-sin rad) (sin rad) (cos rad) 0 0)

scale :: Double -> Double -> Area -> Area
scale w h (x,y,(Block b)) = (x,y, Block (Comm (Cm w 0 0 h 0 0):b))

-- block --
textBlock :: [Command] -> Block
textBlock cs = Block (Text BT:(filter (\b -> 
  case b of 
    Text _    -> True
    otherwise -> False
  ) cs) ++ [Text ET])

graphicsBlock :: [Command] -> Block
graphicsBlock cs = Block ((filter (\b -> 
  case b of 
    Graph _   -> True
    otherwise -> False
  ) cs) ++ [Graph Stroke])

over :: Block -> Block -> Block
over (Block b) (Block b') = Block (b++(Text (Quote ""):b'))

under :: Block -> Block -> Block
under b b' = over b' b

rightAlign :: Block -> Block
rightAlign (Block [])     = Block []
rightAlign (Block (b:bs)) = case b of
  Text (Tj s)    -> Block ((shifted s) ++ rest)
  Text (Quote s) -> Block ((shifted s) ++ (Text T':rest))
  otherwise      -> Block (b:rest)
 where
  Block rest = rightAlign (Block bs)
  shifted s  = 
	( Text (G 1) 
    : Text (Tz (-100)) 
	: Text (Tj s)
    : Text (Tz 100)
    : Text (G 0)
    : [Text (Tj s)]
	)

-- commands --
setFont :: Font -> Int -> Command 
setFont Helvetica size = Text (Tf "/F0" size)
setFont Courier size   = Text (Tf "/F1" size)
setFont Arial size     = Text (Tf "/F2" size)
setFont Times size     = Text (Tf "/F3" size)


write :: String -> Command
write s = Text (Tj s)

writeLn :: String -> Command
writeLn s = Text (Quote s)

line :: Int -> Int -> Int -> Int -> Command
line x1 y1 x2 y2 = Graph (Line x1 y1 x2 y2)

lineTo :: Int -> Int -> Command
lineTo x y = Graph (LineTo x y)

---------------------------------------------------------------------
-- local helper functions

-- creates an object 
obj :: Int -> [String] -> Object
obj n ss = (show n) ++ " 0 obj\n" ++ (unlines (map ('\t':) ss)) ++ "endobj"


-- creates an object containing a dictionary
objDictionary :: Int -> [String] -> Object
objDictionary n ss = 
  (show n) ++ " 0 obj\n<<\n" ++
  (unlines (map ('\t':) ss)) ++ ">>\nendobj"


-- creates an object containing a stream
objStream :: Int -> [String] -> (Object, Int)
objStream n ss = 
  ((show n) ++ " 0 obj\n\t<< /Length " ++ (show (n+1)) ++ 
   " 0 R >>\nstream\n" ++ ss' ++ "endstream\nendobj"
  , length ss')
 where 
  ss' = unlines (map ('\t':) ss)


-- the pdf-header
header :: String
header = "%PDF-1.3\n%הדֿׂ"


-- concats all objects and add a xref at the end
arrangeObjects :: [Object] -> (String, Int, Int)
arrangeObjects os = (content ++ xref, objLen, 2 + length content)
 where
  content   = header ++ concat objects 
  xref      = "\nxref\n0 " ++ (show objLen) ++ "\n" ++ refs
  objLen    = 1 + length objects
  objects   = map ('\n':) os
  refs      = unlines ("0000000000 65535 f ":(map makeRef positions))
  makeRef   = (\p -> (reverse (offset p)) ++ " 00000 n ")
  positions = calcPos ((length header) + 2) (map ((1 +) . length) os)
  offset    = (\p -> take 10 ((reverse (show p))++(concat (repeat "0"))))
  
  calcPos :: Int -> [Int] -> [Int]
  calcPos _ []         = []
  calcPos start (x:xs) = (start):(calcPos (start+x) xs)


-- translates all pages to objects
translatePages :: (Int, Int, Int, Int) -> Int -> [Page] -> [Object]
translatePages s@(x,y,w,h) n ps = concat (map makePageObjs pageCommands)
 where 
  pageCommands = zip [n, (n+3)..] (map (translatePage s) ps)

  makePageObjs :: (Int, [Command]) -> [Object]
  makePageObjs (n,cs) = 
    [ objDictionary n ["/Type/Page", "/Parent 2 0 R", "/Resources 3 0 R" 
	        , "/Mediabox [" ++ (show x) ++ " " ++ (show y) ++ 
			" " ++ (show w) ++ " " ++ (show h) ++ "]", "/Contents [" ++ 
			(show (n+1)) ++ " 0 R]" ]
	, content
	, obj (n+2) [show (len)]
	]
   where
    (content, len) = objStream (n+1) (translate cs)
 

-- combines all areas in a page
translatePage :: (Int, Int, Int, Int) -> Page -> [Command]
translatePage s ps = Comm QSave : (concat (map (translateArea s) ps))


-- creates a list of commands from an area
translateArea :: (Int, Int, Int, Int) -> Area -> [Command]
translateArea (sx,sy,sw,sh) = 
  (\(x,y,Block b) -> 
      (
		Comm QLoadSave :
		Comm (Cm 1 0 0 1 (x-(fromInt sx)) ((fromInt sh)-(y-(fromInt sy)))) :
		b
	  ))


-- translates commands to text
translate :: [Command] -> [String]
translate []             = []
translate (Text (Tf f s):cs)  = (f ++ " " ++ (show s) ++ " Tf") : ((show s) ++ " TL") : (translate cs)
translate (Text (Quote s):cs) = ("(" ++ (escape s) ++ ")Tj") : "T*" : (translate cs)
translate (Text (Tj s):cs)    = ("(" ++ (escape s) ++ ")Tj") : (translate cs)
translate (Text (Tz z):cs)    = ((show z) ++ " Tz") : (translate cs)
translate (Text (G g):cs)     = ((show g) ++ " g") : (translate cs)
translate (Text T':cs)        = "T*" : (translate cs)
translate (Text BT:cs)        = "BT" : (translate cs)
translate (Text ET:cs)        = "ET" : (translate cs)

translate (Graph (Line x1 y1 x2 y2):cs) 
  = ((show x1) ++ " " ++ (show y1) ++ " m")
  : ((show x2) ++ " " ++ (show y1) ++ " l")
  : (translate cs)
translate (Graph (LineTo x y):cs) = ((show x) ++ " " ++ (show y) ++ " l") : (translate cs)
translate (Graph Stroke:cs)       = "S" : (translate cs)

translate (Comm QSave:cs)     = "q" : (translate cs)
translate (Comm QLoad:cs)     = "Q" : (translate cs)
translate (Comm QLoadSave:cs) = "Q" : "q" : (translate cs)
translate (Comm (Cm a b c d e f):cs) = 
  ((show a) ++ " " ++ 
   (show b) ++ " " ++ 
   (show c) ++ " " ++ 
   (show d) ++ " " ++ 
   (show e) ++ " " ++ 
   (show f) ++ " cm") : (translate cs)
	

-- escape ( ) and \ characters with an extra \
escape :: String -> String
escape []        = []
escape ('(':ss)  = '\\':'(':(escape ss)
escape (')':ss)  = '\\':')':(escape ss)
escape ('\\':ss) = '\\':'\\': (escape ss)
escape (s:ss)    = s:(escape ss)
