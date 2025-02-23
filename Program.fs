open System
open System.Globalization

// Функция для ввода чисел (натуральных и вещественных)
let rec inputNumbers () =
    printf "Введите числа через пробел (или 'exit' для выхода): "
    let input = Console.ReadLine()
    if input.Trim().ToLower() = "exit" then None
    else
        // Разделяем введенную строку на отдельные элементы и пытаемся перевести их в числа
        let parsedNumbers =
            input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun s -> 
                match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, n -> Some n  // Если успешенo, добавляем в список
                | _ -> None)          // Если нет, игнорируем ввод
        
        if parsedNumbers.Length > 0 then Some (parsedNumbers |> Array.toList) // Если есть числа, возвращаем список
        else
            printfn "Ошибка: введите хотя бы одно число!" // Ошибка при пустом вводе
            inputNumbers()

// Функция нахождения противоположных чисел
let oppositeNumbers numbers = numbers |> List.map (fun x -> (-1.0)*x)

// Подсчет цифр в натуральном числе
let rec countDigits (n: double) count =
    if n < 10.0 then count + 1
    else countDigits (n / 10.0) (count + 1)

// Основные операции над списками
let addElements lst elements = lst @ elements // Добавление элементов в список
let removeElement lst element = List.filter ((<>) element) lst // Удаление элемента из списка
let findElement lst element = List.tryFindIndex ((=) element) lst // Поиск индекса элемента
let List221 lst1 lst2 = lst1 @ lst2 // Слияние двух списков
let getElementByIndex lst index =
    if index >= 0 && index < List.length lst then Some (List.item index lst) else None // Получение элемента по индексу

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
    | "1" -> // Добавление новых элементов в список
        match inputNumbers() with
        | Some newElements ->
            listMenu (addElements currentList newElements)
        | None -> listMenu currentList
    | "2" -> // Удаление элемента из списка
        printf "Введите число для удаления: "
        match System.Double.TryParse(Console.ReadLine()) with
        | true, n ->
            listMenu (removeElement currentList n)
        | _ ->
            printfn "Ошибка: некорректный ввод"
            listMenu currentList
    | "3" -> // Поиск элемента в списке
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
    | "4" -> // Слияние списка с другим
        printf "Введите второй список чисел через пробел: "
        match inputNumbers() with
        | Some newList ->
            listMenu (List221 currentList newList)
        | None -> listMenu currentList
    | "5" -> // Получение элемента по индексу
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
    | "0" -> main() // Возвращение в главное меню
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
    | "1" -> // Получение списка противоположных чисел
        match inputNumbers() with
        | Some numbers ->
            printfn "Противоположные числа: %A" (oppositeNumbers numbers)
            main()
        | None -> main()
    | "2" -> // Подсчет цифр в натуральном числе
        printf "Введите натуральное число: "
        match System.Double.TryParse(Console.ReadLine()) with
        | true, n when n > 0.0 && floor n = n ->
            printfn "Количество цифр: %d" (countDigits n 0)
            main()
        | _ ->
            printfn "Ошибка: введите натуральное число!"
            main()
    | "3" -> // Переход к операциям над списками
        match inputNumbers() with
        | Some initialList -> listMenu initialList
        | None -> main()
    | "0" -> printfn "Выход" // Выход из программы
    | _ ->
        printfn "Ошибка: неверный выбор"
        main()

// Запуск программы
main()
