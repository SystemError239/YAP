open System

//ввод натуральных чисел с проверкой
let rec InputNatural () =
    printf "Введите натуральные числа через пробел (или 'exit' для выхода): "
    let input = Console.ReadLine().Trim()
    if input.ToLower() = "exit" then None
    else
        let invalidInput = input.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                               |> Array.exists (fun s -> match System.Int32.TryParse(s) with | true, n when n > 0 -> false | _ -> true)
        if invalidInput then
            printfn "Ошибка: ввод должен содержать только натуральные числа! Попробуйте снова."
            InputNatural()
        else
            let parsedNumbers =
                input.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.choose (fun s -> match System.Int32.TryParse(s) with | true, n -> Some n | _ -> None)
            Some (parsedNumbers |> Array.toList)

//генерация случайного списка натуральных чисел
let random = System.Random()
let RandList (size: int) = List.init size (fun _ -> random.Next(1, 100))

//список максимальных цифр
let MaxDigitsList (numbers: int list) =
    let rec findMaxDigit n maxDigit =
        if n = 0 then maxDigit
        else
            let digit = n % 10
            let newMax = if digit > maxDigit then digit else maxDigit
            findMaxDigit (n / 10) newMax
    
    numbers |> List.map (fun n -> findMaxDigit n 0)


//подсчет суммарной длины строк
let StrLenght (strings: string list) =
    strings |> List.fold (fun acc s -> acc + s.Length) 0


let rec taskMenu () =
    printfn "Выберите способ ввода списка:"
    printfn "1 - Ручной ввод"
    printfn "2 - Рандомный список"
    printfn "0 - Назад в главное меню"
    match Console.ReadLine() with
    | "1" ->
        match InputNatural() with
        | Some numbers ->
            let maxDigits = MaxDigitsList numbers
            printfn "Список максимальных цифр: %A" maxDigits
            taskMenu()
        | None -> main()
    | "2" ->
        let rec inputRandomSize () =
            printf "Введите размер списка (или 'exit' для выхода): "
            let input = Console.ReadLine().Trim()
            if input.ToLower() = "exit" then main()
            else
                match System.Int32.TryParse(input) with
                | true, size when size > 0 ->
                    let randomList = RandList size
                    let maxDigits = MaxDigitsList randomList
                    printfn "Сгенерированный список: %A" randomList
                    printfn "Список максимальных цифр: %A" maxDigits
                    taskMenu()
                | _ ->
                    printfn "Ошибка: введите положительное целое число для размера списка!"
                    inputRandomSize()
        inputRandomSize()
    | "0" -> main()
    | _ ->
        printfn "Ошибка: неверный выбор"
        taskMenu()

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
                let totalLength = StrLenght strings
                printfn "Суммарная длина строк: %d" totalLength
                main()
        inputStrings()
    | "0" -> printfn "Выход из программы"
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

main()
