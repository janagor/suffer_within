:- dynamic books_available/0, books/2, book_found/0.

init_books :-
    retractall(books_available),
    retractall(book_found).

books :-
    ( \+ books_available, \+ book_found -> 
        assert(books_available)
    ;
       true
    ).

book(X, Y) :-
    ( \+ books_available; book_found ->
        true
    ;
      ( X =< 0 ; X > 200 ; Y =< 0 ; Y > 200 ) ->
        write('There are no books at such indexes!'), nl
    ;
      ( X =:= 89, Y =:= 144 ) ->
        write('GGGGG!'), nl, clear_books, assert(book_found)
    ;
        write('BAAAD'), nl
    ).

clear_books :-
    retractall(books_available),
    write('Books cleared.'), nl.


