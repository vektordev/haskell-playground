module AiHeader where

import System.Random

data ParameterState = RandomGen GameState

data GameState = TicTacToeState [String] Int | SCALState [String] Int
