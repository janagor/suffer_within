:- dynamic throws_counter/1, random_angle/1, throw_rock/1, rock_shot_under_x/1.
:- dynamic throw_enabled/0, bird_killed/0, key_found/0, toggle_throw/0.

popup_message :-
    write('While trying your best to shoot the key you got lost yourself...'), nl,
    write('As you always do... You did not see the bird flying by window...'), nl,
    write('The rock hit it and you could only see it dropping...'), nl.

init_rock_shot_under_x :-
    assert(rock_shot_under_x(0)).

init_random_angle :-
    random_between(0, 90, X),
    assert(random_angle(X)).

init_throws_counter :-
    assert(throws_counter(0)).

init :-
    init_rock_shot_under_x,
    init_random_angle,
    init_throws_counter,
    assert(toggle_throw),
    retractall(throw_enabled),
    retractall(bird_killed),
    retractall(key_found).

increment_counter :-
    throws_counter(TC),
    NewTC is TC + 1,
    retract(throws_counter(TC)),
    assert(throws_counter(NewTC)),
    (NewTC =:= 7 -> (popup_message, assert(bird_killed)) ; true).  % Show popup message only, no restriction on throws

check_shot(X) :-
    random_angle(RA),
    (
        X =:= RA -> write('You got it!'),assert(key_found), assert(bird_killed), retract(throw_rock);
        X < RA -> write('Too low...');
        X > RA -> write('Too high...')
    ),
    nl.

shot(Angle) :-
    ( throw_enabled, \+ key_found ->
        check_shot(Angle),
        increment_counter,
        nl
    ;
        write('You cannot throw anything.'), nl
    ).

toggle_throw :-
    ( throw_enabled, \+ key_found  ->
        retract(throw_enabled), write('Throwing disabled.'), nl
    ;
        assert(throw_enabled), write('Throwing enabled.'), nl
    ).

is_killed :-
(bird_killed -> write('yes') ; write('no')).
