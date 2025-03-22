:- initialization(main).

% ������� �������� ��� ������� ���������
main :-
    write('������� ����������� �����: '),
    read(N),
    nl,
    write('�������� ����� '), write(N), write(': '), nl,
    find_divisors(N, 1),
    nl,
    main.  % ��������� ������ ��� ������ �����

% �������� ��� ������ � ������ ���� ��������� �����
find_divisors(N, D) :-
    (D > N -> true  % ������� ������ ���������� ��������
    ; (N mod D =:= 0 -> write(D), write(' ') ; true),
      D1 is D + 1,
      find_divisors(N, D1)).
