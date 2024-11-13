:- dynamic books_available/0, book/2.

interact(piece_of_paper) :-
    write('Oh my proportions.'), nl,
    write('The higher your values the closer we get to perfection...'), nl,
    write('Like Fibonacci!'), nl.

assert_books_in_range :-
    forall(between(1, 200, X),  % X goes from 1 to 200
           forall(between(1, 200, Y),  % Y goes from 1 to 200
                  assertz(book(X, Y)))).

books :-
    assertz(books_available),
    assert_books_in_range.

book(X, Y) :-
    books_available,
    (
    X > 200; Y > 200 ->  write('There are no books at such indexes!'), nl ; 
    (X =:= 89, Y =:= 144) ->  write('GGGGG!'), nl ;
    write('BAAAD'), nl
    ).

clear_books :-
    retractall(book(_, _)),
    retract(books_available),
    true.

init :-
    retractall(book(_, _)),
    retract(books_available),
    true.

