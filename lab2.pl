:- initialization(main).

%запуск программы
main :-
    write('Введите список целых чисел: '),
    read(List),
    sum_even(List, Sum),
    write('Сумма чётных элементов списка: '), write(Sum), nl,
    main.

%сумма чётных элементов списка
sum_even([], 0).  %если список пуст

sum_even([H|T], Sum) :-
    H mod 2 =:= 0,  %если H четное
    sum_even(T, Sum1),  %рекурсивно считаем сумму для хвоста
    Sum is H + Sum1.  %прибавляем H к сумме хвоста

sum_even([_|T], Sum) :-
    sum_even(T, Sum).  %если H нечетное
