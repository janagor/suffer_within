:- dynamic throws_counter/1, random_angle/1, throw_rock/1, rock_shot_under_x/1.

init_rock_shot_under_x :-
    assert(rock_shot_under_x(0)).

init_random_angle :-
    random_between(0, 90, X),
    assert(random_angle(X)).

init_throws_counter :-
    assert(throws_counter(1)).

init :-
    init_rock_shot_under_x,
    init_random_angle,
    init_throws_counter.

increment_counter :-
    throws_counter(TC),
    NewX is TC + 1,
    retract(throws_counter(TC)),
    assert(throws_counter(NewX)).

check_shot(X) :-
    random_angle(RA),
    (
        X =:= RA -> write('You got it!'), retract(throw_rock);
        X < RA -> write('Too low...');
        X > RA -> write('Too high...')
    ),
    nl.

throw_rock(Angle) :-
    check_shot(Angle),
    increment_counter,
    nl.
