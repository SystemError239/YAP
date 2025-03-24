:- initialization(main).

% Определяем возможные кандидаты для каждой роли
expedition(Bio, Hyd, Syn, Rad, Mech, Doc) :-
    member(Bio, [e, g]),
    member(Hyd, [b, f]),
    member(Syn, [f, g]),
    member(Rad, [c, d]),
    member(Mech, [c, h]),
    member(Doc, [a, d]),

    % Проверка уникальности каждого участника
    all_different([Bio, Hyd, Syn, Rad, Mech, Doc]),

    % Условия
    (Hyd = f -> member(b, [Bio, Syn, Rad, Mech, Doc]) ; true),  % F не может ехать без B
    (Doc = d -> member(c, [Bio, Hyd, Syn, Rad, Mech, Doc]),
                 member(h, [Bio, Hyd, Syn, Rad, Mech, Doc]) ; true),  % D не может без C и H
    \+ (Rad = c, member(g, [Bio, Hyd, Syn, Mech, Doc])),  % C и G не могут быть вместе
    \+ (Doc = a, member(b, [Bio, Hyd, Syn, Rad, Mech, Doc])).  % A и B не могут быть вместе

% Проверка, что все выбранные участники различны
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

% Вывод всех возможных составов экспедиции
print_expeditions :-
    findall([Bio, Hyd, Syn, Rad, Mech, Doc], expedition(Bio, Hyd, Syn, Rad, Mech, Doc), Teams),
    print_results(Teams).

% Вывод списка команд
print_results([]) :-
    write('Нет возможных составов экспедиции.'), nl.
print_results([H|T]) :-
    write('Возможный состав: '), write(H), nl,
    print_results(T).

% Главный предикат, запускающий программу автоматически
main :-
    print_expeditions,
    write('Нажмите Enter для выхода...'), nl,
    read_line_to_string(user_input, _).
