module FlowChar where

data Expr = Num Double
          | Var String
          | Op String [Expr]
            deriving (Show)

data Command = Goto Int
            |  Assign String Expr
            |  If Expr Int Int
            |  Print Expr
            |  Return Expr

data Program = Read [String] [Command]

nth (c:cs) 1 = c
nth (c:cs) n = nth cs (n-1)

lookupVar _ ([],[]) = 0
lookupVar x (n:ns,v:vs) = if x == n 
                         then v 
                         else lookupVar x (ns,vs)

-- If the variable is not in the list, add it to the list
update ([],[]) x w = ([x],[w])
update (n:ns,v:vs) x w = if n == x 
                             -- If the variable is already in the list, update its value
                             then (n:ns,w:vs)
                             -- Keep looking for the variable in the list
                             else let (ns',vs') = update (ns,vs) x w 
                             -- Add the variable we dropped to the list
                                  in (n:ns',v:vs')
-- Eval Expr
eval (Num x) _ = x
eval (Var x) env = lookupVar x env
eval (Op oper [e1,e2]) env = 
  case oper of
    "+" -> left + right
    "-" -> left - right
    "*" -> left * right
    "/" -> left / right
    "=" -> if left == right then 1 else 0
    "<" -> if left < right then 1 else 0
    ">" -> if left > right then 1 else 0
  where left = eval e1 env 
        right = eval e2 env

-- Run Command
run line (Goto n) env pgm = run n (nth pgm n) env pgm
run line (Assign var value) env pgm = run (line+1) (nth pgm (line+1)) env' pgm
                                      where env' = update env var (eval value env)
run line (If cond true false) env pgm = if eval cond env == 1 
                                  then run true (nth pgm true) env pgm
                                  else run false (nth pgm false) env pgm
run line (Print value) env pgm = print (eval value env) >> run (line+1) (nth pgm (line+1)) env pgm

run line (Return value) env pgm = return (eval value env) 


interpret pgm args =
  let Read vars cmds = pgm
      (c1:_) = cmds
      store = (vars,args)
  in run 1 c1 store cmds

-- Example

pgm = Read ["x","y"]
           [
            If (Op "=" [Var "x",Var "y"]) 7 2,
            If (Op "<" [Var "x",Var "y"]) 5 3,
            Assign "x" (Op "-" [Var "x",Var "y"]),
            Goto 1,
            Assign "y" (Op "-" [Var "y",Var "x"]),
            Goto 1,
            Return (Var "x")
           ]

while = Read ["x"]
             [
              If (Op ">" [Var "x",Num 0]) 2 5,
              Print $ Var "x",
              Assign "x" (Op "-" [Var "x",Num 1]),
              Goto 1,
              Return (Num 0)
             ]

main = interpret while [10]