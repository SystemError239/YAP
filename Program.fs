open System
open System.Globalization

// Функция для ввода чисел (натуральных и вещественных)
let rec inputNumbers () =
    printf "Введите числа через пробел (или 'exit' для выхода): "
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "exit" then None
    else
        let parsedNumbers =
            input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun s -> 
                match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, n -> Some n
                | _ -> None)
        
        if parsedNumbers.Length > 0 then Some (parsedNumbers |> Array.toList)
        else
            printfn "Ошибка: введите хотя бы одно число!"
            inputNumbers()

// Функция нахождения противоположных чисел
let oppositeNumbers numbers = numbers |> List.map (fun x -> -x)

// Рекурсивная функция подсчета цифр в натуральном числе
let rec countDigits (n: double) count =
    if n < 10.0 then count + 1
    else countDigits (n / 10.0) (count + 1)

// Основные операции над списками
let addElements lst elements = lst @ elements
let removeElement lst element = List.filter ((<>) element) lst
let findElement lst element = List.tryFindIndex ((=) element) lst
let concatenateLists lst1 lst2 = lst1 @ lst2
let getElementByIndex lst index =
    if index >= 0 && index < List.length lst then Some (List.item index lst) else None

// Функция для работы со списком
let rec listMenu currentList =
    printfn "Текущий список: %A" currentList
    printfn "Выберите операцию:"
    printfn "1 - Добавить элементы"
    printfn "2 - Удалить элемент"
    printfn "3 - Найти элемент"
    printfn "4 - Сцепить с другим списком"
    printfn "5 - Получить элемент по индексу"
    printfn "0 - Назад в главное меню"
    
    match Console.ReadLine() with
    | "1" ->
        match inputNumbers() with
        | Some newElements -> listMenu (addElements currentList newElements)
        | None -> listMenu currentList
    | "2" ->
        printf "Введите число для удаления: "
        match inputNumbers() with
        | Some [n] -> listMenu (removeElement currentList n)
        | Some _ ->
            printfn "Ошибка: необходимо ввести только одно число для удаления!"
            listMenu currentList
        | None ->
            printfn "Ошибка: некорректный ввод"
            listMenu currentList
    | "3" ->
        printf "Введите число для поиска: "
        match System.Double.TryParse(Console.ReadLine()) with
        | true, n ->
            match findElement currentList n with
            | Some idx -> printfn "Число найдено на позиции %d" idx
            | None -> printfn "Число не найдено"
            listMenu currentList
        | _ ->
            printfn "Ошибка: некорректный ввод"
            listMenu currentList
    | "4" ->
        //printf "Введите второй список чисел через пробел: "
        match inputNumbers() with
        | Some newList -> listMenu (concatenateLists currentList newList)
        | None -> listMenu currentList
    | "5" ->
        printf "Введите индекс: "
        match System.Int32.TryParse(Console.ReadLine()) with
        | true, i ->
            match getElementByIndex currentList i with
            | Some x -> printfn "Элемент: %A" x
            | None -> printfn "Ошибка: индекс вне диапазона"
            listMenu currentList
        | _ ->
            printfn "Ошибка: некорректный ввод"
            listMenu currentList
    | "0" -> main()
    | _ ->
        printfn "Ошибка: неверный выбор"
        listMenu currentList

// Главная функция
and main () =
    printfn "Выберите задачу:"
    printfn "1 - Список противоположных чисел"
    printfn "2 - Количество цифр в числе"
    printfn "3 - Операции над списками"
    printfn "0 - Выход"
    match Console.ReadLine() with
    | "1" ->
        match inputNumbers() with
        | Some numbers ->
            printfn "Противоположные числа: %A" (oppositeNumbers numbers)
            main()
        | None -> main()
    | "2" ->
        printf "Введите натуральное число (только одно): "
        match inputNumbers() with
        | Some [n] when n > 0.0 && floor n = n ->
            printfn "Количество цифр: %d" (countDigits n 0)
            main()
        | Some _ ->
            printfn "Ошибка: введите только одно натуральное число!"
            main()
        | None ->
            printfn "Ошибка: некорректный ввод!"
            main()
    | "3" ->
        printf "Для работы со списком надо ввести его стартовые значения\n"
        match inputNumbers() with
        | Some initialList -> listMenu initialList
        | None -> main()
    | "0" -> printfn "Выход"
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

// Запуск программы
main()
