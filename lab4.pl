:- initialization(main).

% ���������� ��������� ��������� ��� ������ ����
expedition(Bio, Hyd, Syn, Rad, Mech, Doc) :-
    member(Bio, [e, g]),
    member(Hyd, [b, f]),
    member(Syn, [f, g]),
    member(Rad, [c, d]),
    member(Mech, [c, h]),
    member(Doc, [a, d]),

    % �������� ������������ ������� ���������
    all_different([Bio, Hyd, Syn, Rad, Mech, Doc]),

    % �������
    (Hyd = f -> member(b, [Bio, Syn, Rad, Mech, Doc]) ; true),  % F �� ����� ����� ��� B
    (Doc = d -> member(c, [Bio, Hyd, Syn, Rad, Mech, Doc]),
                 member(h, [Bio, Hyd, Syn, Rad, Mech, Doc]) ; true),  % D �� ����� ��� C � H
    \+ (Rad = c, member(g, [Bio, Hyd, Syn, Mech, Doc])),  % C � G �� ����� ���� ������
    \+ (Doc = a, member(b, [Bio, Hyd, Syn, Rad, Mech, Doc])).  % A � B �� ����� ���� ������

% ��������, ��� ��� ��������� ��������� ��������
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

% ����� ���� ��������� �������� ����������
print_expeditions :-
    findall([Bio, Hyd, Syn, Rad, Mech, Doc], expedition(Bio, Hyd, Syn, Rad, Mech, Doc), Teams),
    print_results(Teams).

% ����� ������ ������
print_results([]) :-
    write('��� ��������� �������� ����������.'), nl.
print_results([H|T]) :-
    write('��������� ������: '), write(H), nl,
    print_results(T).

% ������� ��������, ����������� ��������� �������������
main :-
    print_expeditions,
    write('������� Enter ��� ������...'), nl,
    read_line_to_string(user_input, _).
