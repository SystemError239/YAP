:- initialization(main).

main :-
    write('Введите первое множество (список без повторов): '),
    read(Set1),
    write('Введите второе множество (список без повторов): '),
    read(Set2),
    de_morgan_union(Set1, Set2, Result1, Result2),
    write('Результат (левая часть): '), write(Result1), nl,
    write('Результат (правая часть): '), write(Result2), nl,
    (Result1 == Result2 -> write('Закон де Моргана подтверждён.') ; write('Ошибка! Закон не подтверждён.')), nl,
    main.
%нахождение дополнения множества относительно универсального множества
complement(Universe, Set, Complement) :-
    findall(X, (member(X, Universe), \+ member(X, Set)), Complement).

%объединение множеств
union([], Set, Set).
union([H|T], Set, Union) :-
    member(H, Set), !, union(T, Set, Union).
union([H|T], Set, [H|Union]) :-
    union(T, Set, Union).

% пересечение множеств
intersection([], _, []).
intersection([H|T], Set, [H|Intersection]) :-
    member(H, Set), !, intersection(T, Set, Intersection).
intersection([_|T], Set, Intersection) :-
    intersection(T, Set, Intersection).

de_morgan_union(Set1, Set2, Left, Right) :-
    Universe = [0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f], %универсальное множество
    union(Set1, Set2, UnionSet),%объединение в левой части
    complement(Universe, UnionSet, Left),%дополнение в левой части
    complement(Universe, Set1, Comp1),%дополнение первого множества
    complement(Universe, Set2, Comp2),%дополнение второго множества
    intersection(Comp1, Comp2, Right).%пересечение множеств
