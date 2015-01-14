#load "DesignTree.fs"
#load "Ast.fs"
#load "Translation.fs"
#load "SavePs.fs"

open Ast
open DesignTree
open Translation
open SavePs


let Stm1 =
  While
    (Apply ("<>",[ContOf (Var "n"); Int 0]),
     Seq
       [Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
        Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))])

let Stm2 =
  Block
    ([VarDec ("n",Int 4); VarDec ("y",Int 1)],
     Seq
       [While
          (Apply ("<>",[ContOf (Var "n"); Int 0]),
           Seq
             [Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
              Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))])])

let Stm3 =
  Block
    ([VarDec ("n",Int 4); VarDec ("y",Int 1)],
     Seq
       [While
          (Apply ("<>",[ContOf (Var "n"); Int 0]),
           Seq
             [Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
              Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))])])

let Stm4 =
  Block
    ([VarDec ("n",Int 4); VarDec ("y",Int 1)],
     Seq
       [While
          (Apply ("<>",[ContOf (Var "n"); Int 0]),
           Seq
             [PrintLn (Apply ("toString",[ContOf (Var "n")]));
              PrintLn (Apply ("toString",[ContOf (Var "y")]));
              Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
              Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))]);
        PrintLn (String "Result is: ");
        PrintLn (Apply ("toString",[ContOf (Var "n")]));
        PrintLn (Apply ("toString",[ContOf (Var "y")]))])

let Stm5 =
  Block
    ([VarDec ("x",Int 4); VarDec ("output",Int 1);
      Procedure
        ("fac",[Var "n"; Var "y"],
         While
           (Apply ("<>",[ContOf (Var "n"); Int 0]),
            Seq
              [PrintLn (Apply ("toString",[ContOf (Var "n")]));
               PrintLn (Apply ("toString",[ContOf (Var "y")]));
               Asg (Var "y",Apply ("*",[ContOf (Var "n"); ContOf (Var "y")]));
               Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))]))],
     Seq
       [Call ("fac",[Var "x"; Var "output"]);
        PrintLn (Apply ("toString",[ContOf (Var "x")]));
        PrintLn (Apply ("toString",[ContOf (Var "output")]))])

let Stm6 =
  Block
    ([VarDec ("x",Int 4); VarDec ("output",Int 1);
      RecProcedure
        ("fac",[Var "n"],
         IfElse
           (Apply ("=",[ContOf (Var "n"); Int 0]),Seq [Ret (Int 1)],
            Seq
              [Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]));
               Ret
                 (Apply
                    ("*",
                     [Apply ("+",[ContOf (Var "n"); Int 1]);
                      Apply ("fac",[Var "n"])]))]))],
     Seq
       [Asg (Var "output",Apply ("fac",[Var "x"]));
        PrintLn (Apply ("toString",[ContOf (Var "x")]));
        PrintLn (Apply ("toString",[ContOf (Var "output")]))])

let Stm7 =
  Block
    ([VarDec ("x",Int 4); VarDec ("output",Int 1);
      RecProcedure
        ("fac",[Var "n"; Var "o"],
         IfThen
           (Apply ("<>",[ContOf (Var "n"); Int 0]),
            Seq
              [Asg (Var "o",Apply ("*",[ContOf (Var "n"); ContOf (Var "o")]));
               Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]));
               Call ("fac",[Var "n"; Var "o"])]))],
     Seq
       [Call ("fac",[Var "x"; Var "output"]);
        PrintLn (Apply ("toString",[ContOf (Var "x")]));
        PrintLn (Apply ("toString",[ContOf (Var "output")]))])

let Stm8 =
  Block
    ([VarDec ("rng",Int 100); VarDec ("len",Int 10);
      VarArr ("a",ContOf (Var "len"),Int 2);
      VarDec ("sum",Apply ("sumArray",[Var "a"]))],
     Seq [PrintLn (Apply ("toString",[ContOf (Var "sum")]))])

let Stm9 =
  Block
    ([VarDec ("rng",Int 100); VarDec ("len",Int 10);
      VarArr ("a",ContOf (Var "len"),Int 1); VarDec ("sum",Int 0)],
     Seq
       [Asg (Var "a",Apply ("randomArray",[Var "rng"; Var "len"]));
        Call ("printArray",[ContOf (Var "a")]);
        Asg (Var "sum",Apply ("sumArray",[ContOf (Var "a")]));
        PrintLn (String "Sum:");
        PrintLn (Apply ("toString",[ContOf (Var "sum")]))])

let Stm10 =
  Block
    ([VarDec ("len",Int 10); VarArr ("a",ContOf (Var "len"),Int 0);
      Procedure
        ("getFactArr",[Var "n"],
         Block
           ([VarArr ("result",ContOf (Var "n"),Int 0); VarDec ("temp",Int 0);
             VarDec ("nn",Int 0);
             RecProcedure
               ("fac",[Var "x"],
                IfElse
                  (Apply ("=",[ContOf (Var "x"); Int 0]),Seq [Ret (Int 1)],
                   Seq
                     [Asg (Var "x",Apply ("-",[ContOf (Var "x"); Int 1]));
                      Ret
                        (Apply
                           ("*",
                            [Apply ("+",[ContOf (Var "x"); Int 1]);
                             Apply ("fac",[Var "x"])]))]))],
            Seq
              [Asg (Var "nn",ContOf (Var "n"));
               While
                 (Apply ("<>",[ContOf (Var "nn"); Int 0]),
                  Seq
                    [Asg (Var "temp",ContOf (Var "nn"));
                     Asg (Var "nn",Apply ("-",[ContOf (Var "nn"); Int 1]));
                     AsgArr
                       ("result",ContOf (Var "nn"),Apply ("fac",[Var "temp"]))]);
               Ret (Var "result")]))],
     Seq
       [Asg (Var "a",Apply ("getFactArr",[Var "len"]));
        Call ("printArray",[Var "a"])])

let Stm11 =
  Block
    ([VarDec ("v",Int 10); VarDec ("a",Int 0);
      Procedure
        ("sumOfConsecutiveNumbers",[Var "n"],
         Block
           ([VarDec ("result",Int 0)],
            Seq
              [While
                 (Apply ("<>",[ContOf (Var "n"); Int 0]),
                  Seq
                    [Asg
                       (Var "result",
                        Apply ("+",[ContOf (Var "result"); ContOf (Var "n")]));
                     Asg (Var "n",Apply ("-",[ContOf (Var "n"); Int 1]))]);
               Ret (Var "result")]))],
     Seq
       [Asg (Var "a",Apply ("sumOfConsecutiveNumbers",[Const "v"]));
        PrintLn (String "sum of numbers: ");
        While
          (Apply ("<>",[ContOf (Var "v"); Int 0]),
           Seq
             [PrintLn (Apply ("toString",[ContOf (Var "v")]));
              Asg (Var "v",Apply ("-",[ContOf (Var "v"); Int 1]))]);
        PrintLn (String "is ");
        PrintLn (Apply ("toString",[ContOf (Var "a")]))])
       
let testTime stm =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let translated = translateStm Stm10
    stopWatch.Stop()
    let transTime = stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()
    stopWatch.Start()

    let designTree = design translated
    stopWatch.Stop()
    let designTime = stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()
    stopWatch.Start()

    createPostScriptText designTree |> ignore
    stopWatch.Stop()
    let postScriptTime = stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Reset()
    stopWatch.Start()

    createPostScriptTextSlow designTree |> ignore
    stopWatch.Stop()
    let postScriptTimeSlow = stopWatch.Elapsed.TotalMilliseconds
    (transTime, designTime, postScriptTime, postScriptTimeSlow)

let labels = ("Translation", "Design", "PostScript", "StringConcat") 
testTime Stm1 |> ignore
let time1 = testTime Stm1
let time2 = testTime Stm2
let time3 = testTime Stm3
let time4 = testTime Stm4
let time5 = testTime Stm5
let time6 = testTime Stm6
let time7 = testTime Stm7
let time8 = testTime Stm8
let time9 = testTime Stm9
let time10 = testTime Stm10
let time11 = testTime Stm11

