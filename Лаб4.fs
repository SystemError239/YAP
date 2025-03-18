open System

type Tree =
    | Node of string * Tree * Tree
    | Empty

//Вставка в дерево
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(ZOV, left, right) ->
        if value < ZOV then Node(ZOV, insert value left, right)
        elif value > ZOV then Node(ZOV, left, insert value right)
        else tree

//Ввод дерева
let rec inputTree tree =
    printf "Введите строку (или 'exit' для завершения): "
    match Console.ReadLine() with
    | "exit" -> tree
    | input -> inputTree(insert input tree)
//Рандомное заполнение дерева
let randomTree size =
    let randomString() =
        let chars = "abcdefghijklmnopqrstuvwxyz123456789"
        let length = Random().Next(3, 8)
        String.init length (fun _ -> chars.[Random().Next(chars.Length)].ToString())
    let rec generate n tree =
        if n = 0 then tree
        else generate (n - 1) (insert (randomString()) tree)
    generate size Empty
//Вывод дерева
let printTree tree =
    let rec printTreeOtstup tree otstup =
        match tree with
        | Empty -> ()
        | Node(value, left, right) ->
            printTreeOtstup right (otstup + 4)
            printfn "%s%s" (String.replicate otstup " ") value
            printTreeOtstup left (otstup + 4)
    printTreeOtstup tree 0

//Задание 1
let rec Task1 tree =
    let shiftChar c = char (int c + 1)
    let shiftString s = String.map shiftChar s
    match tree with
    | Empty -> Empty
    | Node(value, left, right) -> Node(shiftString value, Task1 left, Task1 right)

//Задание 2
let rec Task2 tree acc =
    match tree with
    | Empty -> acc
    | Node(_, Empty, Empty) -> acc
    | Node(value, left, right) ->
        let acc = Task2 left acc
        let acc = Task2 right acc
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
            let mappedTree = Task1 tree
            printfn "\nПосле преобразования:"
            printTree mappedTree 
        | None -> ()
        mainMenu()
    | "2" ->
        match selectTreeInput() with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree 
            let nodes = Task2 tree []
            printfn "\nУзлы с двумя листьями: %A" nodes
        | None -> ()
        mainMenu()
    | "0" -> printfn "Выход."
    | _ -> 
        printfn "Ошибка!"
        mainMenu()
open System

type Tree =
    | Node of string * Tree * Tree
    | Empty

// Вставка в дерево
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(ZOV, left, right) ->
        if value < ZOV then Node(ZOV, insert value left, right)
        elif value > ZOV then Node(ZOV, left, insert value right)
        else tree

// Ввод дерева
let rec inputTree tree =
    printf "Введите строку (или 'exit' для завершения): "
    match Console.ReadLine() with
    | "exit" -> tree
    | input -> inputTree(insert input tree)

// Рандомное заполнение дерева
let randomTree size =
    let randomString() =
        let chars = "abcdefghijklmnopqrstuvwxyz123456789"
        let length = Random().Next(3, 8)
        String.init length (fun _ -> chars.[Random().Next(chars.Length)].ToString())
    let rec generate n tree =
        if n = 0 then tree
        else generate (n - 1) (insert (randomString()) tree)
    generate size Empty

// Вывод дерева
let printTree tree =
    let rec printTreeOtstup tree otstup =
        match tree with
        | Empty -> ()
        | Node(value, left, right) ->
            printTreeOtstup right (otstup + 4)
            printfn "%s%s" (String.replicate otstup " ") value
            printTreeOtstup left (otstup + 4)
    printTreeOtstup tree 0

// Задание 1 (map через fold)
let mapTree f tree =
    let rec foldTree tree acc =
        match tree with
        | Empty -> acc
        | Node(value, left, right) ->
            let newLeft = foldTree left Empty
            let newRight = foldTree right Empty
            Node(f value, newLeft, newRight)
    foldTree tree Empty

let shiftChar c = char (int c + 1)
let shiftString s = String.map shiftChar s
let Task1 tree = mapTree shiftString tree

// Задание 2 (поиск узлов с двумя листьями через fold)
let Task2 tree =
    let folder node acc =
        match node with
        | Node(value, Node(_, Empty, Empty), Node(_, Empty, Empty)) -> value :: acc
        | _ -> acc
    
    let rec foldTree tree acc =
        match tree with
        | Empty -> acc
        | Node(_, left, right) ->
            let acc = foldTree left acc
            let acc = foldTree right acc
            folder tree acc
    
    foldTree tree []

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
            let mappedTree = Task1 tree
            printfn "\nПосле преобразования:"
            printTree mappedTree 
        | None -> ()
        mainMenu()
    | "2" ->
        match selectTreeInput() with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree 
            let nodes = Task2 tree
            printfn "\nУзлы с двумя листьями: %A" nodes
        | None -> ()
        mainMenu()
    | "0" -> printfn "Выход."
    | _ -> 
        printfn "Ошибка!"
        mainMenu()

mainMenu()
open System

type Tree =
    | Node of string * Tree * Tree
    | Empty

// Вставка в дерево
let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then Node(v, insert value left, right)
        elif value > v then Node(v, left, insert value right)
        else tree

// Ввод дерева вручную
let rec inputTree tree =
    printf "Введите строку (или 'exit' для завершения): "
    match Console.ReadLine() with
    | "exit" -> tree
    | input -> inputTree (insert input tree)

// Случайное заполнение дерева
let randomTree size =
    let randomString () =
        let chars = "abcdefghijklmnopqrstuvwxyz123456789"
        let length = Random().Next(3, 8)
        String.init length (fun _ -> chars.[Random().Next(chars.Length)].ToString())
    let rec generate n tree =
        if n = 0 then tree
        else generate (n - 1) (insert (randomString()) tree)
    generate size Empty

// Вывод дерева (справа налево)
let printTree tree =
    let rec printTreeIndented tree indent =
        match tree with
        | Empty -> ()
        | Node(value, left, right) ->
            printTreeIndented right (indent + 4)
            printfn "%s%s" (String.replicate indent " ") value
            printTreeIndented left (indent + 4)
    printTreeIndented tree 0

// Функция map для преобразования дерева
let rec mapTree f tree =
    match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(f value, mapTree f left, mapTree f right)

// Функция для преобразования символов в строке
let shiftChar c = char (int c + 1)
let shiftString s = String.map shiftChar s

// Применение map к дереву
let task1 tree = mapTree shiftString tree

// Функция fold для поиска узлов с двумя листьями
let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node(_, Empty, Empty) -> acc
    | Node(value, left, right) ->
        let acc = foldTree f acc left
        let acc = foldTree f acc right
        f acc tree value

// Функция поиска узлов с двумя листьями
let findNodesWithTwoLeaves acc tree value =
    match tree with
    | Node(_, Node(_, Empty, Empty), Node(_, Empty, Empty)) -> value :: acc
    | _ -> acc

let task2 tree = foldTree findNodesWithTwoLeaves [] tree

// Меню выбора способа ввода дерева
let rec selectTreeInput () =
    printfn "\nВыберите способ заполнения дерева:"
    printfn "1 - Вручную"
    printfn "2 - Случайные строки"
    printfn "0 - Назад"
    match Console.ReadLine() with
    | "1" -> Some(inputTree Empty)
    | "2" ->
        printf "Количество элементов: "
        match Int32.TryParse(Console.ReadLine()) with
        | true, count when count > 0 ->
            let tree = randomTree count
            printfn "\nИсходное дерево:"
            printTree tree
            Some tree
        | _ ->
            printfn "Ошибка: введите число > 0!"
            selectTreeInput()
    | "0" -> None
    | _ ->
        printfn "Ошибка выбора!"
        selectTreeInput()

// Главное меню
let rec mainMenu () =
    printfn "\nВыберите задачу:"
    printfn "1 - Преобразовать дерево (map)"
    printfn "2 - Найти узлы с двумя листьями (fold)"
    printfn "0 - Выход"

    match Console.ReadLine() with
    | "1" ->
        match selectTreeInput () with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree
            let mappedTree = task1 tree
            printfn "\nПосле преобразования:"
            printTree mappedTree
        | None -> ()
        mainMenu()
    | "2" ->
        match selectTreeInput () with
        | Some tree ->
            printfn "\nИсходное дерево:"
            printTree tree
            let nodes = task2 tree
            printfn "\nУзлы с двумя листьями: %A" nodes
        | None -> ()
        mainMenu()
    | "0" -> printfn "Выход."
    | _ ->
        printfn "Ошибка!"
        mainMenu()

mainMenu()


    
