open System

type Tree =
    | Node of string * Tree * Tree
    | Empty

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(ZOV, left, right) ->
        if value < ZOV then Node(ZOV, insert value left, right)
        elif value > ZOV then Node(ZOV, left, insert value right)
        else tree

let rec inputTree tree =

    printf "Введите строку (или 'exit' для завершения): "
    match Console.ReadLine() with
    | "exit" -> tree
    | input -> inputTree(insert input tree)

let randomTree size =
    let randomString() =
        let chars = "abcdefghijklmnopqrstuvwxyz123456789"
        let length = Random().Next(3, 8)
        String.init length (fun _ -> chars.[Random().Next(chars.Length)].ToString())
    let rec generate n tree =
        if n = 0 then tree
        else generate (n - 1) (insert (randomString()) tree)
    generate size Empty

let printTree tree =
    let rec printTreeHelper tree depth =
        match tree with
        | Empty -> ()
        | Node(value, left, right) ->
            printTreeHelper right (depth + 4)
            printfn "%s%s" (String.replicate depth " ") value
            printTreeHelper left (depth + 4)
    printTreeHelper tree 0

//Задание 1
let rec mapTree tree =
    let shiftChar c = char (int c + 1)
    let shiftString s = String.map shiftChar s
    match tree with
    | Empty -> Empty
    | Node(value, left, right) -> Node(shiftString value, mapTree left, mapTree right)

let rec foldTree tree acc =
    match tree with
    | Empty -> acc
    | Node(_, Empty, Empty) -> acc
    | Node(value, left, right) ->
        let acc = foldTree left acc
        let acc = foldTree right acc
        match left, right with
        | Node(_, Empty, Empty), Node(_, Empty, Empty) -> value :: acc
        | _ -> acc

let rec selectTreeInput() =
    printfn "\nВыберите способ заполнения дерева:"
    printfn "1 - Вручную"
    printfn "2 - Случайные строки"
    printfn "0 - Назад"
    let treeInput = Empty
    match Console.ReadLine() with
    | "1" -> Some(inputTree treeInput)
    | "2" ->
        printf "Количество элементов: "
        
        match Int32.TryParse(Console.ReadLine()) with
        | true, count when count > 0 ->
            let tree = randomTree count
            printfn "\nИсходное дерево:"
            printTree tree 
            Some(tree)
        | _ -> 
            printfn "Ошибка: введите число > 0!"
            selectTreeInput()
    | "0" -> None
    | _ ->
        printfn "Ошибка выбора!"
        selectTreeInput()

and mainMenu() =
    printfn "\nВыберите задачу:"
    printfn "1 - Преобразовать дерево (map)"
    printfn "2 - Найти узлы с двумя листьями"
    printfn "0 - Выход"

    match Console.ReadLine() with
    | "1" ->
        match selectTreeInput() with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree 
            let mappedTree = mapTree tree
            printfn "\nПосле преобразования:"
            printTree mappedTree 
        | None -> ()
        mainMenu()
    | "2" ->
        match selectTreeInput() with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree 
            let nodes = foldTree tree []
            printfn "\nУзлы с двумя листьями: %A" nodes
        | None -> ()
        mainMenu()
    | "0" -> printfn "Выход."
    | _ -> 
        printfn "Ошибка!"
        mainMenu()

mainMenu()