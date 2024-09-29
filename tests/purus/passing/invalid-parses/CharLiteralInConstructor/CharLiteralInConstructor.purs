module CharLiteralInConstructor where

-- Terrible way to represent tic tac toe
data TicTacToe = TicTacToe Char Char Char
                           Char Char Char
                           Char Char Char

isEmptyGame :: TicTacToe -> Boolean
isEmptyGame (TicTacToe '_' '_' '_'
                       '_' '_' '_'
                       '_' '_' '_') = True
isEmptyGame _        = False
