open System

//ввод натуральных чисел с проверкой
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

//генерация случайного списка строк
let createRandomStringList (size: int) =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "
    let randomString () =
        let length = random.Next(3, 10)
        String.init length (fun _ -> chars.[random.Next(chars.Length)].ToString())
    List.init size (fun _ -> randomString())

//генерация случайного списка натуральных чисел
let random = System.Random()
let createRandomNumberList (size: int) = List.init size (fun _ -> random.Next(1, 100))

//получение списка максимальных цифр
let getMaxDigitsList (numbers: int list) =
    let rec findMaxDigit n maxDigit =
        if n = 0 then maxDigit
        else
            let digit = n % 10
            let newMax = if digit > maxDigit then digit else maxDigit
            findMaxDigit (n / 10) newMax
    
    numbers |> List.map (fun n -> findMaxDigit n 0)

//подсчет суммарной длины строк 
let allStrLenght (strings: string list) =
    strings |> List.fold (fun acc s -> acc + s.Length) 0

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
        let rec inputRandomSize () =
            printf "Введите размер списка (или 'exit' для выхода): "
            let input = Console.ReadLine().Trim()
            if input.ToLower() = "exit" then main()
            else
                match System.Int32.TryParse(input) with
                | true, size when size > 0 ->
                    let randomList = createRandomNumberList size
                    let maxDigits = getMaxDigitsList randomList
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
        let rec stringInputMenu () =
            printfn "Выберите способ ввода строк:"
            printfn "1 - Ручной ввод"
            printfn "2 - Рандомный список строк"
            printfn "0 - Назад в главное меню"
            match Console.ReadLine() with
            | "1" ->
                let rec manualStringInput count collected =
                    if count = 0 then
                        collected
                    else
                        printf "Введите строку ('exit' для выхода): "
                        let input = Console.ReadLine()
                        if input.ToLower().Trim() = "exit" then []
                        else manualStringInput (count - 1) (collected @ [input])
                
                printf "Введите количество строк: "
                match System.Int32.TryParse(Console.ReadLine()) with
                | true, size when size > 0 ->
                    let strings = manualStringInput size []
                    if strings.Length > 0 then
                        let totalLength = allStrLenght strings
                        printfn "Суммарная длина строк : %d" totalLength
                    main()
                | _ ->
                    printfn "Ошибка: введите положительное целое число!"
                    stringInputMenu()
            | "2" ->
                printf "Введите размер списка: "
                match System.Int32.TryParse(Console.ReadLine()) with
                | true, size when size > 0 ->
                    let randomStrings = createRandomStringList size
                    printfn "Сгенерированный список строк: %A" randomStrings
                    let totalLength = allStrLenght randomStrings
                    printfn "Суммарная длина строк : %d" totalLength
                    main()
                | _ ->
                    printfn "Ошибка: введите положительное целое число для размера списка!"
                    stringInputMenu()
            | "0" -> main()
            | _ ->
                printfn "Ошибка: неверный выбор"
                stringInputMenu()
        stringInputMenu()
    | "0" -> printfn "Выход из программы"
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

main()
