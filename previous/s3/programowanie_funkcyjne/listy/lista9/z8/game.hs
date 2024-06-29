{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import Control.Monad
import Data.List
import Data.Maybe
data Score = AWins | BWins | Draw


type family Board (m :: * -> *) :: *
type family PlayerA (m :: * -> *) :: *
type family PlayerB (m :: * -> *) :: *


class Monad m => TwoPlayerGame m where
    moveA :: (Board m) -> m (PlayerA m)
    moveB :: (Board m) -> m (PlayerB m)

data Row = R1 | R2 | R3 
    deriving( Eq)

data Col = C1 | C2 | C3
    deriving( Eq)

type Moves = (Row, Col)
type BoardState = ([Moves], [Moves])

type instance PlayerA m = Moves 
type instance PlayerB m = Moves 
type instance Board m = BoardState

startingBoard = ([], [])

validMove :: BoardState -> Moves -> Bool
validMove (ba, bb) m = 
    notElem m ba && notElem m bb

makeMoveA :: BoardState -> Moves -> BoardState
makeMoveA (ba, bb) m = (m : ba, bb)

makeMoveB :: BoardState -> Moves -> BoardState
makeMoveB (ba, bb) m = (ba, m : bb)

results :: BoardState -> Maybe Score
results board = Nothing


gameA :: TwoPlayerGame m => BoardState -> m Score
gameA board = do 
    m <- moveA board 
    case validMove board m of 
        False -> return BWins
        True -> case results (makeMoveA board m) of 
            Nothing  -> gameB (makeMoveA board m) 
            Just res -> return res
        


gameB :: TwoPlayerGame m => BoardState -> m Score
gameB board = do 
    m <- moveB board
    case validMove board m of 
        False -> return AWins
        True -> 
            case results (makeMoveB board m) of 
                Nothing  -> gameA (makeMoveB board m) 
                Just res -> return res


game :: TwoPlayerGame m => m Score
game = gameA  startingBoard
    

    







