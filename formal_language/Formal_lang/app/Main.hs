{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Char (isSpace)
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
    TKeyword kw ->
      case kw of
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
    TOp op ->
      case op of
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

tokenToRow :: [(String,String)] -> Token -> Int -> ((String,String,String), Int)
tokenToRow assigns tok kwCount =
  let lexemeStr = tokenLexeme tok
      typeStr   = tokenType tok

      valStr =
        case tok of
          TWhile -> "X" ++ show kwCount
          TDo    -> "X" ++ show kwCount
          TKeyword kw
            | kw `elem` ["String","Monad","Maybe","Just","Nothing","IO","Either","Left","Right"]
              -> "X" ++ show kwCount
          TLambda _ ->
            "X" ++ show kwCount
          TOp op ->
            "-"
          TAssign    -> "-"
          TSemicolon -> "-"
          TKeyword "fmap" -> "-"
          TKeyword "map"  -> "-"
          TIdentifier s ->
            case lookup s assigns of
              Just v  -> s ++ " : " ++ v
              Nothing -> s ++ " : не инициализирован"
          TReal r -> show r
          _ -> lexemeStr

      newCount =
        case tok of
          TWhile -> kwCount + 1
          TDo    -> kwCount + 1
          TKeyword kw
            | kw `elem` ["String","Monad","Maybe","Just","Nothing","IO","Either","Left","Right"]
              -> kwCount + 1
          TLambda _ -> kwCount + 1
          _         -> kwCount

   in ((lexemeStr, typeStr, valStr), newCount)

separateComments :: [Token] -> ([Token],[Token])
separateComments [] = ([],[])
separateComments (x:xs) =
  let (com,noncom) = separateComments xs
  in case x of
       TComment _ -> (x:com, noncom)
       _          -> (com, x:noncom)

setup :: Window -> UI ()
setup window = do
  return window # set title "HAAASKEEEEEELLLLLLL"

  inputBox <- UI.textarea
                # set (attr "rows") "10"
                # set (attr "cols") "50"

  outputDiv <- UI.div

  recognizeButton <- UI.button #+ [string "Распознать"]
  generateButton  <- UI.button #+ [string "Сгенерировать"]
  clearButton     <- UI.button #+ [string "Очистить"]
  solveButton     <- UI.button #+ [string "Решить пример"]
  resetButton     <- UI.button #+ [string "Сброс"]

  getBody window #+ [ UI.column
                      [ element inputBox
                      , element recognizeButton
                      , element generateButton
                      , element clearButton
                      , element solveButton
                      , element resetButton
                      , element outputDiv
                      ]
                    ]

  on UI.click recognizeButton $ \_ -> do
    txt <- get value inputBox
    if all isSpace txt
      then element outputDiv # set text "Введите текст или нажмите «Сгенерировать»"
      else do
        let (tokens, errors) = lexer txt
        if not (null errors)
          then element outputDiv # set text ("Ошибки:\n" ++ unlines errors)
          else do
            let assigns = buildAssignMap tokens
            let (comments, nonComments) = separateComments tokens
            let (rows, _) = foldl
                              (\(acc,counter) tk ->
                                  let (row3, newC) = tokenToRow assigns tk counter
                                  in (acc ++ [row3], newC)
                              )
                              ([], 1)
                              nonComments
            tableMain <- buildMainTable rows
            tableComm <- buildCommentTable comments
            element outputDiv # set children [tableMain, tableComm]

  on UI.click generateButton $ \_ -> do
    code <- liftIO generateProgram
    element inputBox # set value code

  on UI.click clearButton $ \_ -> do
    txt <- get value inputBox
    if all isSpace txt
      then element outputDiv # set text "И так пусто..."
      else do
        element inputBox  # set value ""
        element outputDiv # set text ""

  on UI.click solveButton $ \_ -> do
    txt <- get value inputBox
    case executeProgram txt of
      Left err -> element outputDiv # set text ("Ошибка выполнения: " ++ err)
      Right ok -> element outputDiv # set text ("Результат выполнения: " ++ ok)

  on UI.click resetButton $ \_ -> do
    element inputBox  # set value ""
    element outputDiv # set text "Сброшено"

buildMainTable :: [(String,String,String)] -> UI Element
buildMainTable rows = do
  table <- UI.table
            # set UI.style [("border-collapse","collapse"),("margin","8px 0")]
  hdr   <- mkRow True ["Лексема","Тип лексемы","Значение"]
  rws   <- mapM (\(a,b,c) -> mkRow False [a,b,c]) rows
  element table #+ (pure hdr : map pure rws)

buildCommentTable :: [Token] -> UI Element
buildCommentTable comms = do
  cap  <- UI.h3 #+ [string "Не компилируемые:"]
  tbl  <- UI.table
            # set UI.style [("border-collapse","collapse"),("margin","8px 0")]
  hdr  <- mkRow True ["Тип","Комментарий"]
  rows <- mapM (\(TComment s)-> do
                  let (tp,bod) = parseComment s
                  mkRow False [tp,bod]
               ) comms
  element tbl #+ (pure hdr : map pure rows)
  container <- UI.div
  element container # set children [cap,tbl]

parseComment :: String -> (String,String)
parseComment s
  | take 2 s == "//" = ("//", drop 2 s)
  | take 2 s == "--" = ("--", drop 2 s)
  | take 2 s == "/*" =
      let x = drop 2 s
      in if take 2 (reverse x) == "*/"
           then ("/* */", reverse (drop 2 (reverse x)))
           else ("/* */", x)
  | otherwise = ("комментарий", s)

mkRow :: Bool -> [String] -> UI Element
mkRow isHdr cols = do
  tds <- mapM (mkCell isHdr) cols
  UI.tr #+ map pure tds

mkCell :: Bool -> String -> UI Element
mkCell hdr s =
  if hdr
    then UI.th
         # set UI.style [("border","1px solid black"),("padding","4px")]
         #+ [string s]
    else UI.td
         # set UI.style [("border","1px solid black"),("padding","4px")]
         #+ [string s]

main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023 } setup
