open System
open System.IO
//ввод натуральных чисел
let rec inputNaturalNumbers count collected =
    if count = 0 then
        Some collected
    else
        printf "Введите натуральное число или 'exit' для выхода: "
        let input = Console.ReadLine().Trim()
        if input.ToLower() = "exit" then None
        else
            match System.Int32.TryParse(input) with
            | true, n when n > 0 -> inputNaturalNumbers (count - 1) (collected @ [n])
            | _ ->
                printfn "Ошибка: введите положительное целое число!"
                inputNaturalNumbers count collected

//чтение чисел из файла
let readNumbersFromFile (path: string) =
    seq {
        use reader = new System.IO.StreamReader(path)
        while not reader.EndOfStream do
            let line = reader.ReadLine().Trim()
            match System.Int32.TryParse(line) with
            | true, n when n > 0 -> yield n
            | _ -> printfn "Пропуск некорректной строки: %s" line
    }

//получение списка максимальных цифр
let getMaxDigitsList (numbers: seq<int>) =
    let rec findMaxDigit n maxDigit =
        if n = 0 then maxDigit
        else
            let digit = n % 10
            let newMax = if digit > maxDigit then digit else maxDigit
            findMaxDigit (n / 10) newMax
    numbers |> Seq.map (fun n -> findMaxDigit n 0)

//подсчет суммарной длины строк
let getTotalStringLength (strings: seq<string>) =
    strings |> Seq.fold (fun acc s -> acc + s.Length) 0

//чтение строк из файла
let readStringsFromFile (path: string) =
    seq {
        use reader = new System.IO.StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

//чтение файлов в каталоге и поиск первого по алфавиту
let findFirstFileAlphabetically (directoryPath: string) =
    try
        Directory.GetFiles(directoryPath)
        |> Seq.ofArray
        |> Seq.sort
        |> Seq.tryHead
    with
    | :? DirectoryNotFoundException ->
        printfn "Ошибка: каталог не найден."
        None
    | ex ->
        printfn "Ошибка: %s" ex.Message
        None
//задача 1
let rec task1Menu () =
    printfn "Выберите способ ввода списка:"
    printfn "1 - Ручной ввод"
    printfn "2 - Ввод из файла"
    printfn "0 - Назад в главное меню"
    match Console.ReadLine() with
    | "1" ->
        printf "Введите количество чисел: "
        match System.Int32.TryParse(Console.ReadLine()) with
        | true, count when count > 0 ->
            match inputNaturalNumbers count [] with
            | Some numbers when numbers.Length > 0 ->
                let maxDigits = getMaxDigitsList (Seq.ofList numbers)
                printfn "Список максимальных цифр: %A" (Seq.toList maxDigits)
            | None -> printfn "Выход в главное меню."
            task1Menu()
        | _ ->
            printfn "Ошибка: введите положительное целое число!"
            task1Menu()
    | "2" ->
        let path = "numbers.txt" //путь к файлу
        let numbers = readNumbersFromFile path
        let maxDigits = getMaxDigitsList numbers
        printfn "Список максимальных цифр из файла: %A" (Seq.toList maxDigits)
        task1Menu()
    | "0" -> main()
    | _ ->
        printfn "Ошибка: неверный выбор"
        task1Menu()

//задача 2
and task2Menu () =
    printfn "Выберите способ ввода строк:"
    printfn "1 - Ручной ввод"
    printfn "2 - Ввод из файла"
    printfn "0 - Назад в главное меню"
    match Console.ReadLine() with
    | "1" ->
        printf "Введите количество строк: "
        match System.Int32.TryParse(Console.ReadLine()) with
        | true, count when count > 0 ->
            let rec manualStringInput count collected =
                if count = 0 then collected
                else
                    printf "Введите строку или 'exit' для выхода: "
                    let input = Console.ReadLine()
                    if input.ToLower().Trim() = "exit" then Seq.empty
                    else manualStringInput (count - 1) (Seq.append collected (seq { yield input }))
            let strings = manualStringInput count Seq.empty
            let totalLength = getTotalStringLength strings
            printfn "Суммарная длина строк: %d" totalLength
            task2Menu()
        | _ ->
            printfn "Ошибка: введите положительное целое число!"
            task2Menu()
    | "2" ->
        let path = "strings.txt" //путь к файлу
        let strings = readStringsFromFile path
        let totalLength = getTotalStringLength strings
        printfn "Суммарная длина строк из файла: %d" totalLength
        task2Menu()
    | "0" -> main()
    | _ ->
        printfn "Ошибка: неверный выбор"
        task2Menu()

//задача 3
and task3Menu () =
    let directoryPath = "C:/Users/Тамара/Desktop/Test" //путь к файлу
    match findFirstFileAlphabetically directoryPath with
    | Some file -> printfn "Первый по алфавиту файл: %s" (Path.GetFileName(file))
    | None -> printfn "Файл не найден или произошла ошибка."
    main()

//главное меню
and main () =
    printfn "Главное меню:"
    printfn "1 - Задача 1: Найти максимальные цифры"
    printfn "2 - Задача 2: Найти суммарную длину строк"
    printfn "3 - Задача 3: Найти первый файл по алфавиту"
    printfn "0 - Выход"
    match Console.ReadLine() with
    | "1" -> task1Menu()
    | "2" -> task2Menu()
    | "3" -> task3Menu()
    | "0" -> printfn "Выход из программы"
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

main()
