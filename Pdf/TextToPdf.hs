import Pdf
import System (getArgs)
import List (elemIndex)

pSize = A4

main :: IO ()
main = 
  do args <- getArgs
     case ((length args) >= 2) of
       False -> putStr ("Usage: TextToPdf <textfile> <pdffile> [options]\n" ++
	     " options:\n" ++
		 "\t-f fontSize - The size of the font used\n" ++
		 "\t-w width    - The maximum number of characters per line\n" ++
		 "\t-h height   - The number of lines per page\n" ++
		 "\t-t title    - The title of each page\n" ++
		 "\t               (default: \"TextToPdf: filename - page \")"
		 )
       True  -> 
         do let textfile = head args
            let pdffile  = head (tail args)
            let fontSize = read (option 'f' "12" args)::Int
            let width    = read (option 'w' "78" args)::Int
            let height   = read (option 'h' "62" args)::Int
            let title    = option 't' ("TextToPdf: " ++ textfile ++ " - page ") args
            text <- readFile textfile
            let pages = textToPages text fontSize width height title
            writeFile pdffile (createPdf pSize pages)
            return ()
 where 
  option :: Char -> String -> [String] -> String
  option c def args = case elemIndex ('-':[c]) args of
    Just n  -> (args ++ (repeat def))!!(n+1)
    Nothing -> def

textToPages text fSize verChars horChars title = pages
 where
  textLines = concat (map (split verChars) (lines text))
  textPages = split (horChars) textLines
  pages     = map makePage (zip textPages [1..])
  makePage = (\(s, n) -> createPage 
    [ position (textBlock ((setFont Courier fSize) : map writeLn s)) 20 750
	, position (graphicsBlock [line 10 765 585 765]) 0 0
	, position (textBlock 
	  [setFont Helvetica 10, write (title ++ (show n))])
	  30 770 
	])

  split :: Int -> [a] -> [[a]]
  split _ [] = []
  split n s  = first : (split n second)
   where 
    (first, second) = splitAt n s

