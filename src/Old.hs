module Old
( oldMain ) where

import qualified Data.Map.Strict as M

import Code
import Display
import Lst
import Rec
import Rel
import Tuple
import Util
import Value

p x = putStr ("" ++ nDisplayValue x ++ "")

oldMain = do
  let card id name ty points = recFromList [("id", I id), ("name", S name), ("type", S ty), ("points", I points)]
      cards = relFromList [card 0 "9/11" "lifestyle" 4, card 1 "ghost" "battle" (-3), card 2 "steely dan" "lifestyle" 20]
      drawr ord id = recFromList [("ord", I ord), ("card_id", I id)]
      draw = relFromList [drawr 0 2, drawr 1 1, drawr 2 0]
      handr playerId cardId = recFromList [("player_id", I playerId), ("card_id", I cardId)]
      hands = relFromList [
        handr 0 0, handr 0 1,
        handr 1 1, handr 1 2,
        handr 2 1, handr 2 0, handr 2 1 ]
  -- p cards
  -- p draw
  -- p hands
  let dat = recFromList [("cards", cards)]
  let dat2 = recFromList [("cards", cards), ("ha", I 13)]
  let code = recFromList [("fib", fibEvaled)]
        where fib = Lam "x" (If (App (App (Prim "==") (Var "x")) (Const (I 0)))
                            (Const (I 1))
                            (App (App (Prim "*") (Var "x"))
                                 (App (Var "fib")
                                      (App (App (Prim "-") (Var "x")) (Const (I 1))))))
              fibEvaled = eval M.empty M.empty fib
  {-
    data Request = Request (Call a) (a -> Response a)
    data Call a = Call fun:String args:[Value]
    data Response a = Response (Maybe Write) (Maybe Request)
    data Write = forall a. Write (Node a) a
  -}
  let callCalltype = TCtor "Call" [TI]
  let callType = TAdt "Call" [callCalltype]
  let callbackType = TFun
  let requestRequestType = TCtor "Request" [callType, callbackType]
  let requestType = TAdt "Request" [requestRequestType]
  let maybeJustType = TCtor "Just" [TI]
  let maybeNoneType = TCtor "None" []
  let maybeType = TAdt "Maybe" [maybeJustType, maybeNoneType]
  let repsonseResponseType = TCtor "Response" [maybeType, maybeType]
  let w = recFromList [("code", code), ("data", dat)]
  p w
