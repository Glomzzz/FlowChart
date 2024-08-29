# FlowChart
A FlowChart language interpreter written in Haskell

# Example

```haskell
while = Read ["x"]
             [
              If (Op ">" [Var "x",Num 0]) 2 5,
              Print $ Var "x",
              Assign "x" (Op "-" [Var "x",Num 1]),
              Goto 1,
              Return (Num 0)
             ]

main = interpret while [10]
```