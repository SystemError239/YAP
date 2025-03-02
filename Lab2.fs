open System

// Функция для ввода натуральных чисел с проверкой
let rec inputNaturalNumbers () =
    printf "Введите натуральные числа через пробел (или 'exit' для выхода): "
    let input = Console.ReadLine().Trim()
    if input.ToLower() = "exit" then None
    else
        let invalidInput = input.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                               |> Array.exists (fun s -> match System.Int32.TryParse(s) with | true, n when n > 0 -> false | _ -> true)
        if invalidInput then
            printfn "Ошибка: ввод должен содержать только натуральные числа! Попробуйте снова."
            inputNaturalNumbers()
        else
            let parsedNumbers =
                input.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.choose (fun s -> match System.Int32.TryParse(s) with | true, n -> Some n | _ -> None)
            Some (parsedNumbers |> Array.toList)

// Функция генерации случайного списка натуральных чисел
let random = System.Random()
let createRandomNumberList (size: int) = List.init size (fun _ -> random.Next(1, 100))

// Функция получения списка максимальных цифр
let getMaxDigitsList (numbers: int list) =
    numbers |> List.map (fun n -> n.ToString().ToCharArray() |> Array.map (fun c -> int c - int '0') |> Array.max)

// Функция подсчета суммарной длины строк
let getTotalStringLength (strings: string list) =
    strings |> List.fold (fun acc s -> acc + s.Length) 0

// Меню задачи 1
let rec taskMenu () =
    printfn "Выберите способ ввода списка:"
    printfn "1 - Ручной ввод"
    printfn "2 - Рандомный список"
    printfn "0 - Назад в главное меню"
    match Console.ReadLine() with
    | "1" ->
        match inputNaturalNumbers() with
        | Some numbers ->
            let maxDigits = getMaxDigitsList numbers
            printfn "Список максимальных цифр: %A" maxDigits
            taskMenu()
        | None -> main()
    | "2" ->
        printf "Введите размер списка: "
        match System.Int32.TryParse(Console.ReadLine()) with
        | true, size when size > 0 ->
            let randomList = createRandomNumberList size
            let maxDigits = getMaxDigitsList randomList
            printfn "Сгенерированный список: %A" randomList
            printfn "Список максимальных цифр: %A" maxDigits
            taskMenu()
        | _ ->
            printfn "Ошибка: введите положительное целое число для размера списка!"
            taskMenu()
    | "0" -> main()
    | _ ->
        printfn "Ошибка: неверный выбор"
        taskMenu()

// Главное меню
and main () =
    printfn "Главное меню:"
    printfn "1 - Задача 1: Найти максимальные цифры"
    printfn "2 - Задача 2: Найти суммарную длину строк"
    printfn "0 - Выход"
    match Console.ReadLine() with
    | "1" -> taskMenu()
    | "2" ->
        let rec inputStrings () =
            printfn "Введите строки через пробел (или 'exit' для выхода): "
            let input = Console.ReadLine().Trim()
            if input.ToLower() = "exit" then main()
            else
                let strings = input.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
                let totalLength = getTotalStringLength strings
                printfn "Суммарная длина строк: %d" totalLength
                main()
        inputStrings()
    | "0" -> printfn "Выход из программы"
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

main()
