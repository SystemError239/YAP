:- initialization(main).

%запуск программы
main :-
    write('Введите натуральное число: '),
    read(N),
    nl,
    write('Делители числа '), write(N), write(': '), nl,
    find_divisors(N, 1),
    nl,
    main.  % Повторный запуск для нового ввода

%поиск и вывод всех делителей числа
find_divisors(N, D) :-
    (D > N -> true  % Базовый случай завершения рекурсии
    ; (N mod D =:= 0 -> write(D), write(' ') ; true),
      D1 is D + 1,
      find_divisors(N, D1)).
