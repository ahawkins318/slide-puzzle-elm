module Messages exposing (..)

import String
import Direction exposing (Direction)

-- handle all Maybe / Err / uncertainty here so that our Update module can deal with clean inputs 
-- (i.e. Direction vs Maybe Direction, NewSize Int vs NewSize String) 

type Msg = 
     NewSize Int |                   -- user entered a (valid) new model size 
     KeyPressed Direction |          -- user pressed a (valid) key (arrow key)
     RandomDirs (List Direction) |   -- random directions have been generated for shuffling
     Noop                            -- something happened that wasn't valid - don't need to do anything to the model


constructSizeMsg : String -> Msg
constructSizeMsg size = case String.toInt size of
   Err msg -> Noop
   Ok val -> NewSize val


constructKeyMsg : Int -> Msg
constructKeyMsg code = case (Direction.fromKeycode code) of
   Nothing -> Noop
   Just direction -> KeyPressed direction


constructRandomDirsMsg : List Int -> Msg
constructRandomDirsMsg dirs = RandomDirs (List.filterMap (\n -> Direction.fromKeycode n) dirs)