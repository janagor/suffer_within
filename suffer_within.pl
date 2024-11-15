/* Suffer within - game by Jakub Antas, Jan Górski, Patryk Zdziech
   Load this file with your swipl interpreter and start the game by using command:  start.   */

:- dynamic at/2, i_am_at/1, interactable_at/2, used/1, on/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(interactable_at(_, _)), retractall(used(_)).
:- retractall(on()).


/* This will tell us where we currently are. */

i_am_at(garden).




/* These paths say how we can move around */

path(garden, n, hall) :- at(golden_key, eq).
path(hall, s, garden).

path(garden, e, glasshouse).
path(glasshouse, w, garden) :- retract(shot_enabled), write('Cannot Shot now!!!!!!!!!!!!!!!').

path(hall, w, library).
path(library, e, hall).

path(hall, e, banquet_hall).
path(banquet_hall, w, hall).

path(banquet_hall, e, closet).
path(closet, w, banquet_hall).

path(hall, n, low_security_prison) :- used(rune1), used(rune2).
path(low_security_prison, s, hall).

path(low_security_prison, w, gluttony_cell).
path(gluttony_cell, e, low_security_prison).

path(low_security_prison, e, lust_cell).
path(lust_cell, w, low_security_prison).

path(low_security_prison, n, medium_security_prison).
path(medium_security_prison, s, low_security_prison).

path(medium_security_prison, w, envy_cell).
path(envy_cell, e, medium_security_prison).

path(medium_security_prison, e, sloth_cell).
path(sloth_cell, w, medium_security_prison).

path(medium_security_prison, n, high_security_prison).
path(high_security_prison, s, medium_security_prison).

path(high_security_prison, w, generator_room).
path(generator_room, e, high_security_prison) :- on(generator).
path(generator_room, e, high_security_prison) :- used(unlit_torch).

path(high_security_prison, e, wrath_cell).
path(wrath_cell, w, high_security_prison).

path(high_security_prison, n, lab) :- at(magnetic_card, eq), on(generator).




/*This command tell us what we can use from our equipment in a particular location */

usable :-
	i_am_at(Place),
	at(X, eq),
	usable_at(X, Place),
	write('Use available: '), write(X),
	nl, fail.



/* These tell us how to use particular items */

use(rune1) :-
	i_am_at(hall),
	usable_at(rune1, hall),
	at(rune1, eq),
	retract(at(rune1, eq)),
	assert(used(rune1)),
	write('You have put rune1 into the gate.'), nl,
	write('Something moved...'),
	nl, !.

use(rune2) :-
	i_am_at(hall),
        usable_at(rune2, hall),
	at(rune2, eq),
	retract(at(rune2, eq)),
	assert(used(rune2)),
	write('You have put rune2 into the gate'),nl,
	write('Something moved...'),
	nl, !.

use(unlit_torch) :-
	i_am_at(generator_room),
	usable_at(unlit_torch, generator_room),
	at(unlit_torch, eq),
	retract(at(unlit_torch, eq)),
	assert(used(unlit_torch)),
	write('Now you can go into the darkness'),
	nl, !.


use(Object) :-
	at(Object, eq),
	usable_at(Object, _),
	write('You can use this object but not here!'),
	nl, !.

use(Object) :-
	at(Object, eq),
	write('You cant use this object'),
	nl, !.

use(_) :-
	write('You dont have the object you are trying to use!'),
	nl,!.
	




/* These tell us where we can use items */

usable_at(rune1, hall).
usable_at(rune2, hall).
usable_at(unlit_torch, generator_room).




/* These tell us where object are located */


at(unlit_torch, sloth_cell).




/* These tell us the state of some objects */

on(generator).




/* This command tell us what we can interact with */

interactable :-
	i_am_at(Place),
	interactable_at(X, Place),
	write('Interaction available: '), write(X),
	nl, fail.




/* These tell us what happens when we interact with object */

interact(slingshot) :-
	i_am_at(glasshouse),
	interactable_at(slingshot, glasshouse),
	write('Now you can shoot the skeleton.'), nl,
	write('To make a shot enter `shot()` with the number corresponding to '), nl,
	write('an angle you want to shoot with. '), nl,
	write('The angle should be a number between 0 and 90. '), nl,
	write('Example: shot(45).'),
	assert(shot_enabled),
	nl,!.

interact(piece_of_paper) :-
	i_am_at(library),
	interactable_at(piece_of_paper, library),
	write('The only readable characters are some numbers: [tutaj wstawic numery]'),
	nl, !.

interact(window) :-
	i_am_at(library),
	interactable_at(window, library),
	on(window),
	retract(on(window)),
	write('You close the window. It becomes quiet.'),
	nl, !.

interact(window) :-
	i_am_at(library),
	interactable_at(window, library),
	assert(on(window)),
	write('You open the window. You can hear the wind blowing from outside.'),
	nl, !.

interact(book) :-
	i_am_at(library),
	interactable_at(book, library),
	assert(at(rune1, library)),
	retract(interactable_at(book, library)),
	write('There was something inside of this book'), nl,
	write('but when you opened the book it fell on the ground'),
	nl, !.

interact(generator) :-
	i_am_at(generator_room),
	interactable_at(generator, generator_room),
	on(generator),
	retract(on(generator)),
	assert(at(magnetic_card, lust_cell)),
	write('The generator is turned off now.'),
	nl, !.

interact(generator) :-
	i_am_at(generator_room),
	interactable_at(generator, generator_room),
	assert(on(generator)),
	retract(at(magnetic_card, lust_cell)),
	write('The generator is turned on now.'),
	nl, !.

interact(painting) :-
	i_am_at(banquet_hall),
	interactable_at(painting, banquet_hall),
	write('In the painting: A thick fog blankets a dark, twisted forest.'), nl,
	write('Gnarled trees loom like dark silhouettes, their branches'), nl,
	write('reaching out like claws. In the distance, a decaying'), nl,
	write('mansion stands, its broken windows resembling'), nl,
	write('hollow eyes. The faint glow of a flickering lantern'), nl,
	write('casts an eerie light on a narrow, winding path'), nl,
	write('leading toward the mansion, swallowed by the thick darkness.'),
	nl, !.

interact(chandelier) :-
	i_am_at(banquet_hall),
	interactable_at(chandelier, banquet_hall),
	write('The broken chandelier swings, its shattered crystals casting'), nl,
	write('twisted shadows in the dim light – better not walk beneath it.'),
	nl, !.

interact(skeletons) :-
	i_am_at(banquet_hall),
	interactable_at(skeletons, banquet_hall),
	write('Decayed skeletons are scattered around. Some sit in chairs,'), nl,
	write('others lie on the floor, their bones twisted and lifeless.'), nl,
	write('Flickering light casts eerie shadows on their empty eye sockets.'), 
	nl, !.

interact(crystal_ball) :-
	i_am_at(closet),
	interactable_at(crystal_ball, closet),
	retract(interactable_at(crystal_ball, closet)),
	assert(at(rune2, closet)),
	write('You picked up the crystal ball, but it slipped'), nl,
	write('from your hands and fell to the ground, shattering.'), nl,
	write('Something strange fell out of it.'),
	nl, !.

interact(floor) :-
	i_am_at(closet),
	interactable_at(floor, closet),
	write('There is such a mess in here...'),
	nl, !.

interact(magnet) :-
	i_am_at(lust_cell),
	interactable_at(magnet, lust_cell),
	write('Such a strong magnet. Looks like its powered by electicity.'),
	nl, !.

interact(tapestry) :-
	i_am_at(lust_cell),
	interactable_at(tapestry, lust_cell),
	write('The tapestry depicts a man and a woman, naked, passionately kissing.'),
	nl, !.

interact(broken_mirrors) :-
	i_am_at(lust_cell),
	interactable_at(broken_mirrors, lust_cell),
	write('There is a lot of broken mirrors laying on the ground.'),nl,
	write('Looks like a person here cared about his looks a lot...'),
	nl, !.

interact(rotting_food) :-
	i_am_at(gluttony_cell),
	interactable_at(rotting_food, gluttony_cell),
	write('A lot of stinky, fat, rotting food. It smells horribly...'),
	nl, !.

interact(floor) :-
	i_am_at(gluttony_cell),
	interactable_at(floor, gluttony_cell),
	write('Floor is really sticky. Somebody spilled a lot of wine in there.'),
	nl, !.

interact(chest) :-
	i_am_at(gluttony_cell),
	interactable_at(chest, gluttony_cell),
	write('A chest full of bones and some kind of meat. I hope'), nl,
	write('its not a human...'),
	nl, !.

interact(paper) :-
	i_am_at(sloth_cell),
	interactable_at(paper, sloth_cell),
	write('He was too lazy to stand up and throw them into the bin...'),
	nl, !.

interact(written_paper) :-
	i_am_at(sloth_cell),
	interactable_at(written_paper, sloth_cell),
	write('"I dont understand why they keep me here. All I'), nl,
	write('ever wanted was a happy life, nothing more. I'), nl,
	write('never asked for this... for any of this. The fear...'), nl,
	write('its overwhelming. I feel trapped, like a caged'), nl,
	write('animal. The things that keep me here, I dont'), nl,
	write('even think they are fully human. They speak in a'), nl,
	write('strange way, like their words are not their own,'), nl,
	write('and their eyes... they are empty, lifeless. No'), nl,
	write('emotion. Just cold, heartless beings. Their cruelty'), nl,
	write('knows no bounds. They dont care, they never'), nl,
	write('cared. I cant escape, no matter how hard I try. I fear'), nl,
	write('they have already taken something from me,'), nl,
	write('something inside. I dont know how much longer'), nl,
	write('I can endure this. Please, someone... anyone... I'), nl,
	write('just want to be free..."'),
	nl, !.



interact(blanket) :-
	i_am_at(envy_cell),
	interactable_at(blanket, envy_cell),
	write('A terribly torn blanket—someone must have suffered greatly here.'),
	nl, !.

interact(notebook) :-
	i_am_at(envy_cell),
	interactable_at(notebook, envy_cell),
	write('"I cant take this anymore. Its driving me insane.'), nl,
	write('These bastards keep forcing us to drink some'), nl,
	write('kind of potions, and every time, I feel worse.'), nl,
	write('My body is burning, my head is spinning, and I cant'), nl,
	write('stop shaking. Its like theyre doing this on purpose.'), nl,
	write('My cellmate—he mentioned something about'), nl,
	write('them testing their potions on us, trying to find a cure'), nl,
	write('for immortality. I dont even know what to believe anymore.'), nl,
	write('Theyve labeled me as envious. Envious? I was just talking'), nl,
	write('to my coworkers wife, for Gods sake! Its'), nl,
	write('all a misunderstanding, a fucking mistake. I swear'), nl,
	write('to God, I wasnt doing anything wrong. But now'), nl,
	write('they have me here, locked up in this hellhole,'), nl,
	write('torturing me with their drugs and lies.'), nl,
	write('Im so fucking scared. What are they doing to us?'), nl,
	write('I just want out of here. I dont deserve this. Please...'), nl,
	write('please, someone, just let us go............"'), 
	nl, !.



interact(scratches) :-
	i_am_at(wrath_cell),
	interactable_at(scratches, wrath_cell),
	write('Scratches cover most of the wall surfaces, looking as if someone made them with their fingernails.'),
	nl, !.
	
interact(chain) :-
	i_am_at(wrath_cell),
	interactable_at(chain, wrath_cell),
	write('The chain is covered in a large amount of dried'), nl,
	write('blood, as if someone had been hung here by their'), nl,
	write('hands, struggling violently and injuring themselves.'), 
	nl, !.

interact(chair) :-
	i_am_at(wrath_cell),
	interactable_at(chair, wrath_cell),
	write('The chair is barely holding together; someone must have thrown it around a lot.'),
	nl, !.

interact(lit_torch) :-
	i_am_at(generator_room),
	interactable_at(lit_torch, generator_room),
	write('The torch is burning but fixed to the wall.'), nl,
	write('You could definitely use it to set something on fire.'),
	nl, !.

interact(doors) :-
	i_am_at(high_security_prison),
	interactable_at(doors, high_security_prison),
	write('The doors are very sturdy, and next to them, you can see a magnetic card reader.'),
	nl, !.

interact(r1tual) :-
	i_am_at(lab),
	interactable_at(r1tual, lab),
	write('Y0u b3c0m3 1mm0rt@l, but @t th3 c0st 0f n3v3r'), nl,
	write('b3ing @bl3 t0 l34v3 th1s h0us3. T3rr0r1f1c v01c3s'), nl,
	write('p0ss3ss y0u, dr1v1ng y0u t0 m@dness.'), nl,
	write('Y0u w1ll n3v3r kn0w p3@c3 @g@1n...'), nl,write(''),nl,
	write('1/3 Madness Ending. Use command:  halt.     to leave.'),
	retractall(at(_, _)), retractall(i_am_at(_)), retractall(interactable_at(_, _)), retractall(used(_)),
	nl, !.

interact(h3lp) :-
	i_am_at(lab),
	interactable_at(h3lp, lab),
	write('Y0u m@n@g3 t0 fr33 th3 p30pl3 1n th3 gr33n l1qu1d,'), nl,
	write('@nd th3Y r34v3@l th31r trU3 f0rm. Th3y'), nl,
	write('@r3 n0t hum@ns, but s0m3th1ng m0r3 d@rk @nd'), nl,
	write('anc13nt. Th3y b3g1n t0 c0rrupt y0ur m1nd,'), nl,
	write('wh1sp3r1ng 1n y0ur 34rs, thr34t3n1ng t0 d3stROY 4ll'), nl,
	write('th@t y0u kn0w. Y0u r3@l1z3 th@t y0u h@v3'), nl,
	write('f@ll3n 1nt0 th31r tr@p, @nd th@t n0t 3v3n y0u'), nl,
	write(' c@nn0t 3sc@p3 th31r p0w3r. Y0ur b0dY 1s st1ll'), nl,
	write('y0urs, but y0ur s0ul 1s n0w th31rs.'), nl,nl,
	write('2/3 Trap Ending. Use command:  halt.     to leave.'),
	retractall(at(_, _)), retractall(i_am_at(_)), retractall(interactable_at(_, _)), retractall(used(_)),
	nl, !.

interact(a1sl3) :-
	i_am_at(lab),
	interactable_at(a1sl3),
	write('Running through the maze'),
	nl, !.
	
interact(Object) :-
	interactable_at(Object, _),
	write('You can interact with it but not here!'),
	nl, !.

interact(_) :-
	write('You cant interact with it.'),
	nl, !.
	
	


/* These facts tell us where we can interact with individual objects */

interactable_at(slingshot, glasshouse).
interactable_at(piece_of_paper, library).
interactable_at(window, library). 
interactable_at(book, library).
interactable_at(generator, generator_room).
interactable_at(painting, banquet_hall).
interactable_at(chandelier, banquet_hall).
interactable_at(skeletons, banquet_hall).
interactable_at(crystal_ball, closet).
interactable_at(floor, closet).
interactable_at(magnet, lust_cell).
interactable_at(tapestry, lust_cell).
interactable_at(broken_mirrors, lust_cell).
interactable_at(rotting_food, gluttony_cell).
interactable_at(floor, gluttony_cell).
interactable_at(chest, gluttony_cell).
interactable_at(paper, sloth_cell).
interactable_at(written_paper, sloth_cell).
interactable_at(blanket, envy_cell).
interactable_at(notebook, envy_cell).
interactable_at(scratches, wrath_cell).
interactable_at(chain, wrath_cell).
interactable_at(chair, wrath_cell).
interactable_at(lit_torch, generator_room).
interactable_at(doors, high_security_prison).
interactable_at(r1tual, lab).
interactable_at(h3lp, lab).
interactable_at(a1sl3, lab).




/* These rules describe how picking objects works like */

take(X) :-
        at(X, eq),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(at(X, eq)),
        write('OK.'),
        nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.




/* These rules describe how putting down objects works like */

drop(X) :-
        at(X, eq),
        i_am_at(Place),
        retract(at(X, eq)),
        assert(at(X, Place)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You aren''t holding it!'),
        nl.




/* These rules defines calls how to go to some direction */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).




/* This rule tells how to move in a given direction. */


go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(n) :-
	i_am_at(hall),
	write('It looks like to pass through these gates, the 2 missing shards need to be found.'),
	nl, !.

go(e) :-
	i_am_at(generator_room),
	write('You cant go out, its too dark out there!'),
	nl, !.


go(n) :-
	i_am_at(high_security_prison),
	on(generator),
	write('You need to have a magnetic card to go through.'),
	nl, !.

go(n) :-
	i_am_at(high_security_prison),
	write('In order for the door to work, the generator must be running.'),
	nl, !.


go(s) :-
	i_am_at(lab),
	write('n0 turn1ng b@ck :}'),
	nl, !.

go(_) :-
        write('You can''t go that way.').




/* This rule tells how looking around us works */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.




/* This rule runs a loop which tell us all the objects that are next to us */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).




/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('use(Object).             -- to use an object from your eq'), nl,
	write('interact(Object)         -- to interact with a particular object from your location'), nl,
        write('look.                    -- to look around you again (description of location + takable objects)'), nl,
        write('usable.                  -- to see what you can use from your eq in your location'), nl,
	write('interactable.            -- to see with what you can interact in your location'), nl,
	write('instructions.            -- to see this message again.'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        nl.




/* This rule starts the game and tells us where we are and what we see */

start :-
        instructions,
	init, %%
        look.




/* These rules describe various locations which are in the game */

describe(garden) :-
        write('You are in the garden.The garden is overgrown, with thorny'), nl,
        write('bushes and dead leaves scattered across the narrow paths.'), nl,
        write('A faint mist hangs in the air, making everything feel quiet'), nl,
        write('and strange. To the north stands a large, dark mansion.'), nl,
        write('Its stone walls are cracked, covered in ivy, and the windows'), nl,
        write('are pitch black, as if hiding secrets inside.'), nl,
        write('To the east, there’s an old greenhouse with broken'), nl,
        write('and foggy glass panels. Inside, twisted plants and vines'), nl,
        write('press against the glass, and a faint smell of earth and decay'), nl,
        write('drifts out, hinting at something forgotten within.'), nl.

describe(glasshouse) :-   
	write('You are in the glasshouse. Shattered glass'), nl,
	write('and broken panes lie scattered on the ground.'), nl,
        write('Vines crawl over everything, choking what'), nl,
        write('remains of the plants. High above, a skeleton'), nl,
        write('swings from a fraying rope, its empty eyes'), nl,
	write('staring into nothing. Between its teeth'), nl,
  	write('glimmers a faint, glowing object.'), nl,
	write('There has to be a way to get it...'), nl,
	write('The air is thick with decay,'), nl,
 	write('and the silence feels suffocating, broken only'), nl,
	write('by the creaking of the hanging bones.'), nl.


describe(hall) :-         
	write('You stand at the entrance of the mansion,'), nl,
	write('the hall before you looming in darkness.'), nl,
	write('The air is thick with the smell of age and decay,'), nl,
	write('and the floor creaks under your feet as you step'), nl,
	write('forward. Before you stand enormous doors,'), nl,
	write('their surfaces covered in strange, ancient symbols.'), nl,
	write('The carvings twist and writhe as if alive,'), nl,
	write('but something is wrong – two pieces of the door'), nl,
	write('are missing, leaving jagged gaps that seem to pulse'), nl,
	write('with a faint, unnatural glow. The emptiness between the'), nl,
	write('gaps feels wrong, as if something is waiting,'), nl,
	write('something that has been sealed away for a long time.'), nl,
	write('The silence is oppressive, and the feeling that you'), nl,
	write('are being watched from the shadows is impossible to shake.'), nl.

describe(library) :-      
	write('You are in the library.'), nl,
	write('The library is cold and dim, filled with shelves of'), nl,
	write('forgotten, weathered books. The air is thick with the'), nl,
	write('smell of dust and dampness. A broken window lets'), nl,
	write('in faint light, casting long shadows on the creaky'), nl,
	write('floor. By the window stands a desk, its surface'), nl,
	write('covered in dust. On it lies a torn scrap of paper,'), nl,
	write('scribbled with frantic, barely legible writing. Nearby,'), nl,
	write('a rusted clock stands still, its hands frozen in'), nl,
	write('time. In the corner, a spider''s web stretches'), nl,
	write('between the bookshelves. The room feels heavy with'), nl,
	write('unspoken secrets, as if the books themselves are'), nl,
	write('hiding dark stories waiting to be uncovered.'), nl.


describe(banquet_hall) :- 
	write('You are in the banquet hall.'), nl,
	write('The banquet hall is vast, filled with the stench of decay.'), nl,
	write('Long tables are covered in the bodies of the'), nl,
	write('dead, some still seated, others slumped on'), nl,
	write('the floor. Ashtrays overflow with cigarette butts, their'), nl,
	write('ashes still smoldering. Faded paintings hang'), nl,
	write('crookedly on the walls, and a broken chandelier casts'), nl,
	write('fractured shadows above. The silence is heavy,'), nl,
	write('disturbed only by the soft creak of the wooden floor'), nl,
	write('beneath your feet.'), nl.
	

describe(closet) :-       
	write('You are in the closet.'), nl,
	write('The closet is cramped and cluttered, filled with shelves of scattered items.'), nl,
	write('Old boxes, forgotten dishes, rusted tools, and scraps of fabric cover every surface.'), nl,
	write('On the floor, a pile of unsorted books lies forgotten, while an old dusty trunk sits.'), nl,
	write('In the corner stands a strange crystal ball, inside which something unsettling moves.'), nl,
	write('Faint wisps of mist swirl within, occasionally forming shapes as if something is trying to escape.'), nl,
	write('The air smells of dampness and mildew, and the darkness seems to swallow every corner.'), nl,
	write('The place holds more secrets than it lets on, hidden beneath layers of dust and shadow.'), nl.

	

describe(low_security_prison) :-          
	write('You are in the low security prison.'), nl,
	write('The room is dimly lit, with two small cells on either'), nl,
	write('side, their rusted barred doors creaking. The air is'), nl,
	write('stale, filled with the scent of mold and cold stone.'), nl,
	write('Ahead, through open metal doors, lies another'), nl,
	write('similar room, and the sound of dripping water'), nl,
	write('echoes through the silence. The oppressive stillness'), nl,
	write('makes the space feel even more confining.'), nl.

describe(gluttony_cell) :-
	write('You are in the glutony cell.'), nl,
	write('The room reeks of spoiled food and decay. Cracked'), nl,
	write('plates and tarnished cutlery are scattered'), nl,
	write('around, some half-buried in mold. A rusted bedframe'), nl,
	write('lies under piles of rotting food, and the floor is'), nl,
	write('sticky with spilled wine and spoiled meat. In the'), nl,
	write('corner, a chest overflows with scraps of fabric,'), nl,
	write('bones, and remnants of feasts. The air is thick'), nl,
	write('with the stench of excess and decay, a grim reminder'), nl,
	write('of gluttony’s punishment.'), nl.


describe(lust_cell) :-
	write('You are in the lust cell.'), nl,
	write('The room is dim and heavy with the scent of'), nl,
	write('perfume and stale sweat. Faded tapestries hang on the'), nl,
	write('walls, depicting intimate scenes. A narrow bed with'), nl,
	write('torn silk sheets sits in the center, surrounded by'), nl,
	write('broken mirrors reflecting fragmented images. In the'), nl,
	write('corner, an old vanity with twisted jewelry and'), nl,
	write('faded love letters adds to the oppressive atmosphere,'), nl,
	write('filled with the remnants of unchecked desire.'), nl,
	write('In the corner, there is a magnetic card held by a large electric magnet.'), nl,
	write('There is no way to take it while this magnet is powered on'), nl.


describe(medium_security_prison) :-
	write('You are in the medium security prison.'), nl,
	write('The room is cold, with faded grey stone walls. Two'), nl,
	write('empty cells stand on either side, their bars casting'), nl,
	write('long shadows. A metal door leads to another similar'), nl,
	write('room, behind heavy iron gates. The floor is'), nl,
	write('cracked and stained, and dim light from a flickering'), nl,
	write('bulb casts eerie shadows across the space,'), nl,
	write('giving the room a heavy, oppressive feeling.'), nl.


describe(envy_cell) :-
	write('You are in the envy cell.'), nl,
	write('The room is cold and empty, with cracked walls'), nl,
	write('closing in. A narrow cot sits against one wall,'), nl,
	write('covered by a thin, tattered blanket and a notebook on it. A small,'), nl,
	write('tarnished mirror on the opposite wall distorts the'), nl,
	write('reflection. The air feels thick with bitterness, and'), nl,
	write('a small window offers only a sliver of the outside'), nl,
	write('world. The floor is covered in a layer of dust,'), nl,
	write('untouched, as if no one has ever truly rested here.'), nl.

describe(sloth_cell) :-
	write('You are in the sloth cell.'), nl,
	write('The room is dim and neglected, with a sagging'), nl,
	write('bed in the corner. Empty containers and crumpled'), nl,
	write('papers litter the floor. A cracked lamp barely'), nl,
	write('lights the space, and the air is stale, untouched by'), nl,
	write('effort or care.'), nl.


describe(high_security_prison) :-
	write('You are in the high security prison.'), nl,
	write('The room is stark and oppressive, with an atmosphere'), nl,
	write('of cold metal and concrete. To the right, a cell'), nl,
	write('stands behind thick, reinforced bars, making it clear'), nl,
	write('this is a place of high security. To the left, heavy'), nl,
	write('steel doors loom, their solid form unyielding and'), nl,
	write('intimidating. In front of you, there is another set of'), nl,
	write('doors, even more imposing, with a magnetic card'), nl,
	write('reader beside them, suggesting a high-tech lock'), nl,
	write('system. Everything in this room exudes a sense'), nl,
	write('of being locked down, impenetrable, and designed'), nl,
	write('to keep any potential threat contained.'), nl.


describe(generator_room) :-
	write('You are in the generator room'), nl,
	write('The room vibrates with the hum of a massive generator'), nl,
	write('at its center, filling the air with the scent of'), nl,
	write('oil and metal. Pipes and wires snake along the walls,'), nl,
	write('some hissing softly. The floor is stained, and the'), nl,
	write('flickering lights cast unsettling shadows, making'), nl,
	write('the room feel both powerful and dangerous.'), nl.


describe(wrath_cell) :-
	write('You are in the wrath_cell'), nl,
	write('The cell is dark and cramped, with the walls'), nl,
	write('seemingly closing in. Scratches and dents mar the'), nl,
	write('surfaces, as if something—or someone—has been'), nl,
	write('violently thrashing. In one corner, a broken chair'), nl,
	write('lies half-smashed, and shattered glass glistens on'), nl,
	write('the floor. The air is thick with a sense of rage that'), nl,
	write('never left, a heaviness that hangs in the oppressive'), nl,
	write('silence. A rusted, bloodstained chain dangles'), nl,
	write('from the ceiling, suggesting past violence, while the'), nl,
	write('faint smell of sweat and anger lingers in the air.'), nl.


describe(lab) :-
	write('Y0u ar3 1n th3 1ab0r@t0ry...'), nl,
	write('Th3 d00r cl0s3s b3hind y0u...'), nl,
	write('Gr33n l1qu1d f1lls th3 c0mp@rtm3nts w1th h@lf-c0nsci0us b0d13s.'), nl,
	write('M@ny ch3m1c@l @pp@r@tus3s @r3 3v3rYwh3r3.'), nl,
	write('@t th3 3nd, @ r3d c@rp3t c0v3rs th3 fl00r, w1th'), nl,
	write('@ c1rcl3 0f fl@m3s @nd @n 0p3n b00k.'), nl, write(''),nl,
	write('P!CK: '), nl,write(''),nl,
	write('1. Y0u r34d wh@ts 1n th3 b00k @nd p3rf0rm th3 r1tual,'), nl,
	write('b3c0m1ng 1mm0rt@l, but y0u c@nn0t 3v3r'), nl,
	write('l3@v3 th1s h0us3 @g@1n.'), nl,write(''),nl,
	write('2. Y0u br34k th3 c0mp@rtm3nts @nd try t0 s@v3 th3s3 p30pl3,'), nl,
	write('but y0u d0nt kn0w wh@t th3Y r34llY @r3.'), nl,write(''),nl,
	write('3. Y0U RUN F0R Y0UR L1F3 THR0UGH TH3 A1SL3'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% part including shooting game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic shots_counter/1, random_angle/1, shot_rock/1.
:- dynamic shot_enabled/0, bird_killed/0, key_found/0.

popup_message :-
    write('While trying your best to shoot the key you got lost yourself...'), nl,
    write('As you always do... You did not see the bird flying by window...'), nl,
    write('The rock hit it and you could only see it dropping...'), nl.

init_random_angle :-
    random_between(0, 90, X),
    assert(random_angle(X)).

init :-
    init_random_angle,
    assert(shots_counter(0)),
    retractall(shot_enabled),
    retractall(bird_killed),
    retractall(key_found).

increment_counter :-
    shots_counter(TC),
    NewTC is TC + 1,
    retract(shots_counter(TC)),
    assert(shots_counter(NewTC)),
    (NewTC =:= 7 -> (popup_message, assert(bird_killed)) ; true).

check_shot(X) :-
    random_angle(RA),
    (
        X =:= RA -> retract(interactable_at(slingshot, glasshouse)),
		assert(at(golden_key, glasshouse)),
		write('You shot a skeleton and a key from his mouth fell on the ground.'),
		assert(key_found),
		assert(bird_killed),
		retract(shot_rock);
        X < RA -> write('Too low...');
        X > RA -> write('Too high...')
    ),
    nl.

shot(Angle) :-
    ( shot_enabled, \+ key_found ->
        check_shot(Angle),
	increment_counter,
	nl
    ;
        write('You cannot shot anything.'), nl
    ).

is_killed :-
	(bird_killed -> write('yes') ; write('no')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% end of part including shooting game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
