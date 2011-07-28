module WXMaxima where

import Bugs

import Control.Monad.State

import Data.List
import Utils
import Directory


-- Zipper On lists
data LstZipPlus a = LZP { lzp'left  :: [a] ,
                          lzp'elem  ::  a  ,
                          lzp'right :: [a]
                        }
  deriving (Eq,Ord,Show,Read)

type LstZip a = Maybe (LstZipPlus a)

data WXMT a = WXMT { unWXMT :: State (LstZip Entry, String) a}

instance Monad WXMT where
  return         = WXMT . return
  (WXMT m) >>= f = WXMT $ m >>= unWXMT . f



-- wxMaxima Printing
print_input_cell :: String -> [String]
print_input_cell s =
     [ "/* [wxMaxima: input   start ] */"
     , s ++ ";"
     , "/* [wxMaxima: input   end   ] */"
     , ""
     ]

print_comment_cell :: String -> [String]
print_comment_cell s =
     [ "/* [wxMaxima: comment start ]"
     , s
     , "   [wxMaxima: comment end   ] */"
     , ""
     ]

print_pair :: (Show a, Show b) => (a, b) -> String
print_pair (i,j) = "[ " ++ (show i) ++ " , " ++ (show j) ++ " ]"

print_table :: (Show a, Show b) => [(a, b)] -> String
print_table l = "[" ++ (aux l)
  where
    aux []      = " ]"
    aux [x]     = "\n   " ++ (print_pair x) ++ "\n ]"
    aux (x : l) = "\n   " ++ (print_pair x) ++ "," ++ (aux l)

print_entry :: Entry -> [String]
print_entry (Entry c sql n t) =
     (print_comment_cell $ "Computation for kernel " ++ c ++ "\n" ++ sql)
  ++ (print_input_cell $ table ++ " : " ++ (print_table t))
  ++ (print_input_cell $ theta ++ " : log_param_of_table(" ++ table  ++ ")")
  ++ (print_input_cell $ chi_square ++ " : chi_square_of_table(" ++ theta ++ "[1] , " ++ table ++ ")")
  ++ (print_input_cell $ pblog_expected ++ " : pblog_expected_table(" ++ theta ++ "[1]," ++ table ++ ")")
  ++ (print_input_cell $ df ++ " : degree_of_freedom(" ++ table ++ ")")
  ++ (print_input_cell $ pvalue ++ " : p_value_chi_square(" ++ chi_square ++ "," ++ df ++ ")")
  where
     table          = "table_"              ++ n
     theta          = "theta_"              ++ n
     chi_square     = "chi_square_"         ++ n
     pblog_expected = "pblog_expected_"     ++ n
     df             = "degree_of_freedom_"  ++ n
     pvalue         = "p_value_chi_square_" ++ n

print_entries :: [Entry] -> [String]
print_entries = concatMap print_entry

extract_version :: String -> String
extract_version name =
	(sepPred (== '_') ((sepPred (== '-') name) !! 1)) !! 0

print_tex_entry :: Entry -> String
print_tex_entry (Entry c sql n t) =
        "if "++theta++"[1] = und\n"
        ++ "then printf (stream, \""++ version ++" & "
        ++ "\\\\textcolor{Red}{NA} & \\\\textcolor{Red}{NA} & "
        ++ "\\\\textcolor{Red}{$~4d$} & \\\\textcolor{Red}{NA} \\\\\\\\~%\", "++ df ++")\n"
	++ "elseif "++ pvalue ++ " > 0.05\n"
        ++ "then printf (stream, \""++ version ++" & $~{~,3,,,'0f~}$ & $~,3,,,'0f$ & $~4d$ & $~5,3,,,'0f$ \\\\\\\\~%\", "
	++ theta ++ ", "
	++ chi_square ++ ", "
--	++ pblog_expected ++ ", "
	++ df ++ ", "
	++ pvalue ++")"
        ++ "else printf (stream, \""++ version ++" & "
        ++ "\\\\textcolor{Red}{$~{~,3,,,'0f~}$} & "
        ++ "\\\\textcolor{Red}{$~,3,,,'0f$} & "
        ++ "\\\\textcolor{Red}{$~4d$} & "
        ++ "\\\\textcolor{Red}{$~5,3,,,'0f$} \\\\\\\\~%\", "
	++ theta ++ ", "
	++ chi_square ++ ", "
--	++ pblog_expected ++ ", "
	++ df ++ ", "
	++ pvalue ++")"
	where
                 version        = extract_version c
                 theta          = "theta_"              ++ n
                 chi_square     = "chi_square_"         ++ n
--               pblog_expected = "pblog_expected_"     ++ n
                 df             = "degree_of_freedom_"  ++ n
                 pvalue         = "p_value_chi_square_" ++ n

print_tex :: String -> [Entry] -> [String]
print_tex output entries =
	  print_input_cell $ concat $ intersperse ";\n"
                           (  ["stream : openw(\"" ++ output ++ "\")"]
			   ++ ["printf (stream, \"% Version & $\\\\theta$ & $\\\\chi^2$ & Degree of freedom & $\\\\chi^2$ p-value \\\\\\\\~%\")"]
			   ++ (map print_tex_entry entries)
			   ++ ["close(stream)"]
			   )

-- Maxima Batch file manipulation

wxmaxima_begining = [ "/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"
                     , "/* [ Created with wxMaxima version 0.8.5 ] */"
                     , ""
                    ]

wxmaxima_ending =   [ "/* Maxima can't load/batch files which end with a comment! */"
                    , "\"Created with wxMaxima\"$"
                    ]

wxmaximize s   = wxmaxima_begining ++ s ++ wxmaxima_ending


wxunmaximize :: [String] -> [String]
wxunmaximize s = aux $ drop 2 $ s
  where
    aux l = if length l <= 2
              then []
              else (head l) : (aux $ tail l)




getLibrary :: [Char] -> IO [String]
getLibrary libdir = do l  <- getDirectoryContents libdir
                       ls <- mapM doone $ sort $ filter (isSuffixOf ".wxm") l
                       return $ concat $ ls
  where
    doone filename = do s <- readFile $ libdir ++ "/" ++ filename
                        return $ wxunmaximize $ normalizeWXMlines $ lines s

output_tables :: String -> Maybe String -> [Entry] -> IO String
output_tables output lib entries =
	      do plib <- case lib of
              	               	   Nothing -> return []
                               	   Just fp -> getLibrary fp
                 return $ concat $
                        intersperse "\n" $
                        normalizeWXMlines $
                        wxmaximize $
                        (plib
			++ (print_entries entries)
			++ (print_tex (output++".tex") entries))


-- Summery




-- Utils

normalizeWXMlines :: [String] -> [String]
normalizeWXMlines = check . (dropWhile p)
  where
    pc c = c == ' ' || c == '\t'
    p    = all pc

    check [] = []
    check l  = map snd $ filter perase $ zip toeraise l
      where
        mask         = map p l
        toeraise     = map (uncurry (&&)) $ zip mask (tail mask ++ [True])
        perase (b,_) = not b

normalizeWXMstr :: String -> String
normalizeWXMstr = concat . (intersperse "\n") . normalizeWXMlines . lines
