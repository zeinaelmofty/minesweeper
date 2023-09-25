type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show , Eq)
up:: MyState -> MyState
up (S (x,y) cell st state)=if x-1<0 then Null 
                            else (S (x-1,y) cell "up" (S (x,y) cell st state))
down:: MyState -> MyState
down (S (x,y) cell st state)=if x+1>3 then Null 
                            else (S (x+1,y) cell "down" (S (x,y) cell st state))

left:: MyState -> MyState
left (S (x,y) cell st state)=if y-1<0 then Null 
                            else (S (x,y-1) cell "left" (S (x,y) cell st state))
right:: MyState -> MyState
right (S (x,y) cell st state)=if y+1>3 then Null 
                            else (S (x,y+1) cell "right" (S (x,y) cell st state))
collect:: MyState -> MyState
collect (S (x,y) [(x1,y1),(x2,y2)] st state)| x==x1 && y==y1 = S (x,y) [(x2,y2)] "collect" (S (x,y) [(x1,y1),(x2,y2)] st state)
                                            | x==x2 && y==y2 = S (x,y) [(x1,y1)] "collect" (S (x,y) [(x1,y1),(x2,y2)] st state)
                                            | otherwise = (S (x,y) [(x1,y1),(x2,y2)] st state)
collect (S (x,y) [(x1,y1)] st state)| x==x1 && y==y1 = S (x,y) [] "collect" (S (x,y) [(x1,y1)] st state)     
                                    | otherwise = (S (x,y) [(x1,y1)] st state)                  
nextMyStates::MyState->[MyState]
nextMyStates (S cell xs st state)= (checkCollect (S cell xs st state)) ++ (checkUp (S cell xs st state))++ (checkDown (S cell xs st state))++ (checkLeft (S cell xs st state)) ++(checkRight (S cell xs st state))++[]

checkUp::MyState->[MyState]
checkUp (S (x,y) cell st state)= if up (S (x,y) cell st state) == Null then []
                    else [up (S (x,y) cell st state)]
checkDown::MyState->[MyState]
checkDown (S (x,y) cell st state)= if down (S (x,y) cell st state) == Null then []
                    else [down (S (x,y) cell st state)]
checkLeft::MyState->[MyState]
checkLeft (S (x,y) cell st state)= if left (S (x,y) cell st state) == Null then []
                    else [left (S (x,y) cell st state)]
checkRight::MyState->[MyState]
checkRight (S (x,y) cell st state)= if right (S (x,y) cell st state) == Null then []
                    else [right (S (x,y) cell st state)]
checkCollect::MyState->[MyState]
checkCollect (S (x,y) cell st state)= if collect (S (x,y) cell st state) == Null then []
                    else [collect(S (x,y) cell st state)]
isGoal::MyState->Bool
isGoal (S cell xs st state)= if xs==[] then True
                             else False
search::[MyState]->MyState
search (s:xs)= if isGoal (s) then s
               else search (xs++(nextMyStates s)) 

constructSolution:: MyState ->[String]
constructSolution Null = []
constructSolution (S cell xs string state)=if string=="" then constructSolution state 
                                           else (constructSolution state) ++ [string]
solve :: Cell->[Cell]->[String]
solve start (x:xs)= constructSolution (search [(S start (x:xs) "" Null)])
