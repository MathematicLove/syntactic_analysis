{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( UI, defaultConfig, startGUI, Window
  , attr, element, getBody, get, on, domEvent, runFunction, set, ffi, (#)
  )
import Graphics.UI.Threepenny ((#+))

import Data.Char (isSpace)
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Lib
  ( Token(..)
  , lexer
  , buildAssignMap
  , tokenLexeme
  , executeProgram
  , generateProgram
  , stripComments
  )

tokenType :: Token -> String
tokenType t =
  case t of
    TWhile        -> "ключевое слово"
    TDo           -> "ключевое слово"
    TKeyword kw   -> case kw of
                       "String" -> "ключевое слово"
                       "Monad"  -> "ключевое слово"
                       "Maybe"  -> "монада"
                       "Just"   -> "монада"
                       "Nothing"-> "монада"
                       "IO"     -> "монада"
                       "Either" -> "монада"
                       "Left"   -> "монада"
                       "Right"  -> "монада"
                       "fmap"   -> "функтор"
                       "map"    -> "функтор"
                       _        -> "ключевое слово"
    TIdentifier _ -> "идентификатор"
    TReal _       -> "константа"
    TAssign       -> "знак присваивания"
    TOp op        -> case op of
                       "+"   -> "оператор присваивания"
                       "<>"  -> "функтор"
                       "<$>" -> "функтор"
                       ">>=" -> "стрелка Клейсли"
                       "."   -> "функтор"
                       _     -> "оператор сравнения"
    TSemicolon    -> "разделитель"
    TString _     -> "строковая константа"
    TComment _    -> "комментарий"
    TLambda _     -> "лямбда выражение"

type RowState = (Int, Int, Map.Map String Int)

processTokens
  :: [(String,String)]
  -> [Token]
  -> RowState
  -> ([(String, String, String)], RowState)
processTokens _ [] st = ([], st)
processTokens assigns [tk] st =
  let (row, st') = tokenToRowWithLookahead assigns tk Nothing st
  in ([row], st')
processTokens assigns (tk:rest@(nextTk:_)) st =
  let (row, st')   = tokenToRowWithLookahead assigns tk (Just nextTk) st
      (rows, st'') = processTokens assigns rest st'
  in (row : rows, st'')

tokenToRowWithLookahead
  :: [(String,String)]
  -> Token
  -> Maybe Token
  -> RowState
  -> ((String, String, String), RowState)
tokenToRowWithLookahead assigns tok maybeNext (kwCount, semiCount, idMap) =
  let lexemeStr = tokenLexeme tok
      typeStr   = tokenType tok
  in case tok of
       TWhile ->
         ((lexemeStr, typeStr, "X" ++ show kwCount), (kwCount + 1, semiCount, idMap))
       TDo ->
         ((lexemeStr, typeStr, "X" ++ show kwCount), (kwCount + 1, semiCount, idMap))
       TKeyword kw
         | kw `elem` ["String","Monad","Maybe","Just","Nothing","IO","Either","Left","Right"] ->
             ((lexemeStr, typeStr, "X" ++ show kwCount), (kwCount + 1, semiCount, idMap))
       TLambda _ ->
         ((lexemeStr, typeStr, "X" ++ show kwCount), (kwCount + 1, semiCount, idMap))

       TOp _ ->
         ((lexemeStr, typeStr, "-"), (kwCount, semiCount, idMap))
       TAssign ->
         ((lexemeStr, typeStr, "-"), (kwCount, semiCount, idMap))
       TSemicolon ->
         ((lexemeStr, typeStr, "-" ), (kwCount, semiCount, idMap))

       TKeyword "fmap" ->
         ((lexemeStr, typeStr, "-"), (kwCount, semiCount, idMap))
       TKeyword "map" ->
         ((lexemeStr, typeStr, "-"), (kwCount, semiCount, idMap))
       TKeyword _ ->
         ((lexemeStr, typeStr, "ключевое слово"), (kwCount, semiCount, idMap))

       TIdentifier s ->
         let occ        = Map.findWithDefault 0 s idMap
             newIdMap   = Map.insertWith (+) s 1 idMap
             newVal     = s ++ " : " ++ show (occ + 1)  
         in ((lexemeStr, typeStr, newVal), (kwCount, semiCount, newIdMap))

       TReal r ->
         ((lexemeStr, typeStr, show r), (kwCount, semiCount, idMap))

       TString s ->
         ((lexemeStr, typeStr, s), (kwCount, semiCount, idMap))

       TComment s ->
         ((lexemeStr, typeStr, s), (kwCount, semiCount, idMap))

separateComments :: [Token] -> ([Token],[Token])
separateComments [] = ([],[])
separateComments (x:xs) =
  let (com, noncom) = separateComments xs
  in case x of
       TComment _ -> (x : com, noncom)
       _          -> (com, x : noncom)

setup :: Window -> UI ()
setup window = do
  inputBox <- UI.textarea
    # set (attr "rows") "10"
    # set (attr "cols") "50"
    # set (attr "id") "inputBoxId"

  fileInput <- UI.input
    # set UI.type_ "file"
    # set (attr "accept") ".txt"
    # set (attr "id") "fileInputId"

  recognizeButton <- UI.button #+ [UI.string "Распознать"]
  generateButton  <- UI.button #+ [UI.string "Сгенерировать"]
  clearButton     <- UI.button #+ [UI.string "Очистить"]
  solveButton     <- UI.button #+ [UI.string "Решить пример"]
  resetButton     <- UI.button #+ [UI.string "Сброс"]

  outputDiv <- UI.div

  getBody window #+ 
    [ UI.column
      [ element inputBox
      , element recognizeButton
      , element generateButton
      , element clearButton
      , element solveButton
      , element resetButton
      , element outputDiv
      , element fileInput
      ]
    ]

  on (domEvent "change") fileInput $ \_ -> do
    runFunction $ ffi
      "var f = document.getElementById('fileInputId').files[0]; \
      \if (f) { \
      \  var reader = new FileReader(); \
      \  reader.onload = function(e){ \
      \    document.getElementById('inputBoxId').value = e.target.result; \
      \  }; \
      \  reader.readAsText(f); \
      \}"
    return ()

  on UI.click recognizeButton $ \_ -> do
    txt <- get UI.value inputBox
    if all isSpace txt
      then element outputDiv # set UI.text "Введите текст или нажмите «Сгенерировать»"
      else do
        let (tokens, errors) = lexer txt
        if not (null errors)
          then element outputDiv # set UI.text ("Ошибки:\n" ++ unlines errors)
          else do
            let assigns               = buildAssignMap tokens
            let (comments, nonComms) = separateComments tokens
            let (rows, _)            = processTokens assigns nonComms (1, 1, Map.empty)
            tableMain <- buildMainTable rows
            tableComm <- buildCommentTable comments
            element outputDiv # set UI.children [tableMain, tableComm]

  on UI.click generateButton $ \_ -> do
    code <- liftIO generateProgram
    element inputBox # set UI.value code

  on UI.click clearButton $ \_ -> do
    txt <- get UI.value inputBox
    if all isSpace txt
      then element outputDiv # set UI.text "И так пусто..."
      else do
        element inputBox # set UI.value ""
        element outputDiv # set UI.text ""

  on UI.click solveButton $ \_ -> do
    txt <- get UI.value inputBox
    case executeProgram txt of
      Left err -> element outputDiv # set UI.text ("Ошибка выполнения: " ++ err)
      Right ok -> element outputDiv # set UI.text ("Результат выполнения: " ++ ok)

  on UI.click resetButton $ \_ -> do
    element inputBox # set UI.value ""
    element outputDiv # set UI.text "Сброшено"

buildMainTable :: [(String,String,String)] -> UI UI.Element
buildMainTable rows = do
  table <- UI.table
    # set UI.style [("border-collapse","collapse"),("margin","8px 0")]
  hdr <- mkRow True ["Лексема", "Тип лексемы", "Значение"]
  rws <- mapM (\(a,b,c) -> mkRow False [a,b,c]) rows
  element table #+ (pure hdr : map pure rws)

buildCommentTable :: [Token] -> UI UI.Element
buildCommentTable comms = do
  cap <- UI.h3 #+ [UI.string "Не компилируемые:"]
  tbl <- UI.table
    # set UI.style [("border-collapse","collapse"),("margin","8px 0")]
  hdr  <- mkRow True ["Тип", "Комментарий"]
  rows <- mapM (\(TComment s) -> do
                  let (tp, bod) = parseComment s
                  mkRow False [tp, bod]
               ) comms
  element tbl #+ (pure hdr : map pure rows)
  container <- UI.div
  element container # set UI.children [cap, tbl]

parseComment :: String -> (String, String)
parseComment s
  | take 2 s == "//" = ("//", drop 2 s)
  | take 2 s == "--"  = ("--",  drop 2 s)
  | take 2 s == "/*"  =
      let x = drop 2 s
      in if take 2 (reverse x) == "*/"
           then ("/* */", reverse (drop 2 (reverse x)))
           else ("/* */", x)
  | otherwise = ("комментарий", s)

mkRow :: Bool -> [String] -> UI UI.Element
mkRow isHdr cols = do
  tds <- mapM (mkCell isHdr) cols
  UI.tr #+ map pure tds

mkCell :: Bool -> String -> UI UI.Element
mkCell hdr s =
  if hdr
    then UI.th
         # set UI.style [("border","1px solid black"),("padding","4px")]
         #+ [UI.string s]
    else UI.td
         # set UI.style [("border","1px solid black"),("padding","4px")]
         #+ [UI.string s]

main :: IO ()
main = startGUI defaultConfig { UI.jsPort = Just 8023 } setup
