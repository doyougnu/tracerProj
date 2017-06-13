# tracerProj

## What
This project's purpose is to implement the WHILE language, with the ability to trace evaluation, and perform program slicing on any program in the language. Any slice, on a well-formed program should return a well-formed program. Slicing is, and was a stretch goal from the original project which was just to implement the WHILE language and perform tracing.

## Where
All code, besides Main.hs, is located in src/, Main.hs is located in app/

## How to run
From the root directory (directory with .cabal file) you should just be able to run stack ghci and start inputting commands

### Available Commands
There are several functions available at the top level, these are:

1. help
2. programs
3. printProgram
4. slice
5. sliceAndEval
6. sliceTraceEval
7. printRunProg
8. printEvalProg
9. printExecProg

The help function will print out a list of available functions in the ghci session. The programs function will print out all programs in the Programs/ directory. The printProgram function will, when given the filename for a program, print out that program. Slice takes a filename and a variable and performs a slicing on that program for that variable, then prints out the result. SliceAndEval does the same thing as slice, but evalualates the resultant program. SliceTraceEval slices a program, then traces the evaluation of the resultant program. All of the slicing programs have P-variants that allow you to run a program _without_ the assumption that the program is in the Programs/ directory. If you choose to use any of these (sliceP, sliceAndEvalP etc.) then you must specify the path to the program in its entirety. Lastly, I've included functions that run the program and either print the final value, state or both. These follow the same pattern as most monad libraries where printRunProg outputs both and printEvalProg outputs the value.

### Examples
Here are just some examples to refer to:

```haskell
λ> help
Available Functions: 
  1. Programs        -- List all available pre-made programs in the Program/ directory
  2. printProgram    -- Print a given program
  3. slice           -- Slice a program on a given variable
  4. sliceAndEval    -- Slice a program, then evaluate the resultant program
  5. sliceTraceEval  -- Slice a program, and evaluate the result while tracing it
  6. sliceP          -- Same as 3, but assumes the program is not in the Program/ directory
  7. sliceAndEvalP   -- Same as 4, but makes the same assumption as 6
  8. sliceTraceEvalP -- Same as 5, makes same assumptions as 6, 7
  9. printRunProg    -- Run the program, print the final value and final state
  10. printEvalProg  -- Run the program, print the final value
  11. printExecProg  -- Run the program, print the final state
  12. printRunProgP  -- see P-variants above
  13. printEvalProgP -- see P-variants above
  14. printExecProgP  -- see P-variants above
  
  
λ> printProgram "IF1"
  let aa = 0
  let bb = 100
  If (aa < bb) {
      bb - 10
      let aa = ~aa
  } else {
      let aa = 1000}
    
λ> slice "IF1" "aa"
  let aa = 0
  let bb = 100
  If (aa < bb) {
      let aa = ~aa
  } else {
      let aa = 1000
  }
    
λ> sliceTraceEval "IF1" "aa"
 let aa = 0 State: fromList []
 => fromList [("aa",0)]

 let bb = 100 State: fromList [("aa",0)]
 => fromList [("aa",0),("bb",100)]

 let aa = ~aa State: fromList [("aa",0),("bb",100)]
 => fromList [("aa",0),("bb",100)]
```

### When things go wrong
Under normal circumstances you should not receive an Exception of any sort. If you evaluate a program and it returns a Nothing, then that means the program was poorly formed and could not be evaluated. Programs that return a "Just " terminate successfully but do not have any return value. If you do encounter any errors these should be parsing errors. I would like to stress that semi-colons are hard to miss and please use my pre-made programs as a reference for the syntax.
