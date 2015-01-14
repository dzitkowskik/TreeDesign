module SavePs

open DesignTree

open System
open System.Net
open System.IO
open System.Text

let simpleTree = Node("root", [Node("chld1",[Node("leaf1",[]); Node("leaf2",[])]);Node("chld2",[])])

let width = 2500
let height = 1000

let ws = width.ToString()
let hs = height.ToString()
let wsh = (width/2).ToString()

let header =
    let sb = StringBuilder()
    sb.AppendLine("%!") |> ignore
    sb.AppendLine("<</PageSize["+ws+" "+hs+"]/ImagingBBox null>> setpagedevice") |> ignore
    sb.AppendLine("1 1 scale") |> ignore
    sb.AppendLine(wsh+" "+hs+" translate") |> ignore
    sb.AppendLine("newpath") |> ignore
    sb.AppendLine("/Times-Roman findfont 10 scalefont setfont") |> ignore
    sb.ToString()

let footer = "showpage"

let getLabelText label =  "(" + label ") dup stringwidth pop 2 div neg 0 rmoveto show"

let scale = 50.0

let downMove (delta:float) pos = match pos with | (x,y) -> (x,y-delta)
let horizMove (delta:float) pos = match pos with | (x,y) -> (x+(scale*delta),y)

let moveStr = function | (a,b) -> (int a).ToString()+" "+(int b).ToString()+" moveto"
let lineStr = function | (a,b) -> (int a).ToString()+" "+(int b).ToString()+" lineto"
let putLabel label = "("+label.ToString()+") dup stringwidth pop 2 div neg 0 rmoveto show"

let printLabel cursor label (sb:StringBuilder) = 
    let cursorTop = downMove 10.0 cursor
    let cursorBot = downMove 2.0 cursorTop
    sb.AppendLine( moveStr cursorTop) |> ignore
    sb.AppendLine( putLabel label) |> ignore
    sb.AppendLine( moveStr cursorBot) |> ignore
    cursorBot

let printLabelSlow cursor label s =
    let cursorTop = downMove 10.0 cursor
    let cursorBot = downMove 2.0 cursorTop
    let text = String.concat "" [s; 
        moveStr cursorTop; 
        putLabel label; 
        moveStr cursorBot]
    (cursorBot, text)

let printLine cursor pos (sb:StringBuilder) =
    let cursor1 = downMove 30.0 cursor
    let cursor2 = horizMove pos cursor1
    let cursor3 = downMove 30.0 cursor2
    sb.AppendLine( moveStr cursor) |> ignore
    sb.AppendLine( lineStr cursor1) |> ignore
    sb.AppendLine( moveStr cursor1) |> ignore
    sb.AppendLine( lineStr cursor2) |> ignore
    sb.AppendLine( moveStr cursor2) |> ignore
    sb.AppendLine( lineStr cursor3) |> ignore
    sb.AppendLine( moveStr cursor3) |> ignore
    sb.AppendLine( "stroke" ) |> ignore
    cursor3 

let printLineSlow cursor pos s =
    let cursor1 = downMove 30.0 cursor
    let cursor2 = horizMove pos cursor1
    let cursor3 = downMove 30.0 cursor2
    let text = String.concat "" [s; 
        moveStr cursor; 
        lineStr cursor1; 
        moveStr cursor1;
        lineStr cursor2; 
        moveStr cursor2;
        lineStr cursor3; 
        moveStr cursor3;
        "stroke"]
    (cursor3, text)

let createPostScriptText designTree =
    let sb = StringBuilder()
    sb.AppendLine(header) |> ignore
    let cursor = (0.0,0.0)
    sb.AppendLine(moveStr cursor) |> ignore
    let rec drawTree tree cursor = 
        match tree with 
        | Node((label,position),subtrees) ->
            let cursor1 = printLabel cursor label sb    
            List.forall(fun d -> 
                match d with 
                | Node((label, position), _) ->
                    let cursor2 = printLine cursor1 position sb
                    drawTree d cursor2) subtrees
    let u = drawTree designTree cursor
    sb.Append(footer) |> ignore
    sb.ToString()

let createPostScriptTextSlow designTree =
    let cursor = (0.0,0.0)
    let text = String.concat "" [header; moveStr cursor]
    let rec drawTree tree cursor = 
        match tree with 
        | Node((label,position),subtrees) ->
            let (cursor1, temp) = printLabelSlow cursor label text
            List.fold(fun acc d -> 
                match d with 
                | Node((label, position), _) ->
                    let (cursor2, temp2) = printLineSlow cursor1 position temp
                    String.concat "" [temp2; drawTree d cursor2]) "" subtrees
    let text2 = drawTree designTree cursor
    String.concat "" [text2; footer]
   
let sd = __SOURCE_DIRECTORY__

let createPostScript designTree =
    let text = createPostScriptText designTree
    let filePath = Path.GetFullPath(sd+"/test.ps")
    File.WriteAllText(filePath, text)
