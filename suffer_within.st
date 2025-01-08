Object subclass: Location [
    | name description interactables items north south east west |

"innit"
    Location class >> name: aName description: aDescription interactables: anInteractables items: anItems [
        ^self new
            name: aName;
            description: aDescription;
            interactables: anInteractables;
            items: anItems.
    ]

"setters"
    name: aName [name := aName.]
    description: aDescription [description := aDescription.]
    interactables: anInteractables [interactables := anInteractables.]
    items: anItems [items := anItems.]
    north: aNorth [north := aNorth.]
    south: aSouth [south := aSouth.]
    east: anEast [east := anEast.]
    west: aWest [west := aWest.]
    
"getters"
    name [^name]
    description [^description]
    interactables [^interactables]
    items [^items]
    north [^north]
    south [^south]
    east [^east]
    west [^west]
].

Object subclass: GuessingGame [
    | target attempts |

    GuessingGame class >> playGuessingGame [
        "Create a new instance of the game and start the guessing loop"
        | game |
        game := self new.
        game initialize.
        game welcome.
        game generateRandomTarget.
        ^game guessingLoop
    ]
    
    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    GuessingGame >> initialize [
        attempts := 0.
    ]

    GuessingGame >> welcome [
        Transcript show: 'Welcome to the slingshot game! Guess a number between 0 and 90.'; cr.
    ]

GuessingGame >> generateRandomTarget [
    "Generate a random number between 0 and 90"
    target := (1 to: 89) atRandom
]


    GuessingGame >> guessingLoop [
        | guess |
        Transcript show: 'Enter your guess: '; flush.
        guess := self readCommand asInteger.
        attempts := attempts + 1.
        (guess < target) ifTrue: [
            Transcript show: 'Too low!'; cr.
            ^self guessingLoop
        ].
        (guess > target) ifTrue: [
            Transcript show: 'Too high!'; cr.
            ^self guessingLoop
        ].
        Transcript show: 'Congratulations! You guessed the number!'; cr.
        ^(attempts >= 10)
    ]

]


Object subclass: Game [
    | 
    "Locations:"
    garden hall glasshouse library banquetHall closet lowSecurityPrison gluttonyCell
    lustCell mediumSecurityPrison envyCell slothCell highSecurityPrison generatorRoom wrathCell lab 
    "Flags:"
    isHallOpen isBirdKilled isGoldenKeyTaken isCrystalBallBroken isLaboratoryOpen isPrisonOpen 
    hasPlayedNumberPair isRune2Taken isTorchLit isGeneratorOn hasPlayedSlingshot isWindowOpen
    "State:"
    isGameOver location dinamicInteractables inventory itemMap
    |

    introductionText := #('' '' ).
    instructionsText := #('Enter command (n/s/e/w to move, `interact <item>`, `pickup <item>`, or `q` to quit): ' '' ).
 
    Game class >> new [
        | g | g := super new.
        g init.
        ^g
    ]

    init [
    
        "Flags"
        
            isTorchLit := false.
            isGeneratorOn := true.
            isHallOpen := false.
            isGoldenKeyTaken := false.
            isLaboratoryOpen := false.
            isCrystalBallBroken := false.
            isRune2Taken := false.
            hasPlayedNumberPair := false.
            isPrisonOpen := false.
            isBirdKilled := false.
            hasPlayedSlingshot := false.
            isWindowOpen := false.

        "Locarions:"
        garden := Location name: 'Garden'
              description: #(
              'The garden is overgrown, with thorny'
              'bushes and dead leaves scattered across the narrow paths.'
              'A faint mist hangs in the air, making everything feel quiet'
              'and strange. To the north stands a large, dark mansion.'
              'Its stone walls are cracked, covered in ivy, and the windows'
              'are pitch black, as if hiding secrets inside.'
              'To the east, there`s an old greenhouse with broken'
              'and foggy glass panels. Inside, twisted plants and vines'
              'press against the glass, and a faint smell of earth and decay'
              'drifts out, hinting at something forgotten within.')
              interactables: #()
              items: #().
        hall := Location name: 'Hall'
              description: #(
              'You stand at the entrance of the mansion,'
              'the hall before you looming in darkness.'
              'The air is thick with the smell of age and decay,'
              'and the floor creaks under your feet as you step'
              'forward. Before you stand enormous doors,'
              'their surfaces covered in strange, ancient symbols.'
              'The carvings twist and writhe as if alive,'
              'but something is wrong – two pieces of the door'
              'are missing, leaving jagged gaps that seem to pulse'
              'with a faint, unnatural glow. The emptiness between the'
              'gaps feels wrong, as if something is waiting,'
              'something that has been sealed away for a long time.'
              'The silence is oppressive, and the feeling that you'
              'are being watched from the shadows is impossible to shake.')
              interactables: #()
              items: #().
        glasshouse := Location name: 'Glasshouse'
              description: #(
              'Shattered glass'
              'and broken panes lie scattered on the ground.'
              'Vines crawl over everything, choking what'
              'remains of the plants. High above, a skeleton'
              'swings from a fraying rope, its empty eyes'
              'staring into nothing. Between its teeth'
              'glimmers a faint, glowing object.'
              'There has to be a way to get it...'
              'The air is thick with decay,'
              'and the silence feels suffocating, broken only'
              'by the creaking of the hanging bones.')
              interactables: #('slingshot') asSet
              items: #() asSet.
        library := Location name: 'Library'
              description: #(
              'The library is cold and dim, filled with shelves of'
              'forgotten, weathered books. The air is thick with the'
              'smell of dust and dampness. A broken window lets'
              'in faint light, casting long shadows on the creaky'
              'floor. By the window stands a desk, its surface'
              'covered in dust. On it lies a torn scrap of paper,'
              'scribbled with frantic, barely legible writing. Nearby,'
              'a rusted clock stands still, its hands frozen in'
              'time. In the corner, a spider''s web stretches'
              'between the bookshelves. The room feels heavy with'
              'unspoken secrets, as if the books themselves are'
              'hiding dark stories waiting to be uncovered.')
              interactables: #('piece_of_paper' 'window' 'book') asSet
              items: #() asSet. 
        banquetHall := Location name: 'Banquet Hall'
              description: #(
              'The banquet hall is vast, filled with the stench of decay.'
              'Long tables are covered in the bodies of the'
              'dead, some still seated, others slumped on'
              'the floor. Ashtrays overflow with cigarette butts, their'
              'ashes still smoldering. Faded paintings hang'
              'crookedly on the walls, and a broken chandelier casts'
              'fractured shadows above. The silence is heavy,'
              'disturbed only by the soft creak of the wooden floor'
              'beneath your feet.')
              interactables: #('painting' 'chandelier' 'skeletons') asSet
              items: #() asSet.
        closet := Location name: 'Closet'
              description: #(
              'The closet is cramped and cluttered, filled with shelves of scattered items.'
              'Old boxes, forgotten dishes, rusted tools, and scraps of fabric cover every surface.'
              'On the floor, a pile of unsorted books lies forgotten, while an old dusty trunk sits.'
              'In the corner stands a strange crystal ball, inside which something unsettling moves.'
              'Faint wisps of mist swirl within, occasionally forming shapes as if something is trying to escape.'
              'The air smells of dampness and mildew, and the darkness seems to swallow every corner.'
              'The place holds more secrets than it lets on, hidden beneath layers of dust and shadow.')
              interactables: #('floor' 'crystal_ball') asSet
              items: #() asSet.
        lowSecurityPrison := Location name: 'Low Security Prison'
              description: #(
              'The room is dimly lit, with two small cells on either'
              'side, their rusted barred doors creaking. The air is'
              'stale, filled with the scent of mold and cold stone.'
              'Ahead, through open metal doors, lies another'
              'similar room, and the sound of dripping water'
              'echoes through the silence. The oppressive stillness'
              'makes the space feel even more confining.')
              interactables: #() asSet
              items: #() asSet.
        gluttonyCell := Location name: 'Gluttony Cell'
              description: #(
              'The room reeks of spoiled food and decay. Cracked'
              'plates and tarnished cutlery are scattered'
              'around, some half-buried in mold. A rusted bedframe'
              'lies under piles of rotting food, and the floor is'
              'sticky with spilled wine and spoiled meat. In the'
              'corner, a chest overflows with scraps of fabric,'
              'bones, and remnants of feasts. The air is thick'
              'with the stench of excess and decay, a grim reminder'
              'of gluttony’s punishment.')
              interactables: #('rotting_food' 'floor' 'chest') asSet
              items: #() asSet.
        lustCell := Location name: 'Lust Cell'
              description: #(
              'The room is dim and heavy with the scent of'
              'perfume and stale sweat. Faded tapestries hang on the'
              'walls, depicting intimate scenes. A narrow bed with'
              'torn silk sheets sits in the center, surrounded by'
              'broken mirrors reflecting fragmented images. In the'
              'corner, an old vanity with twisted jewelry and'
              'faded love letters adds to the oppressive atmosphere,'
              'filled with the remnants of unchecked desire.'
              'In the corner, there is a magnetic card held by a large electric magnet.'
              'There is no way to take it while this magnet is powered on')
              interactables: #('magnet' 'tapestry' 'broken_mirrors') asSet
              items: #() asSet.
        mediumSecurityPrison := Location name: 'Medium Security Prison'
              description: #(
              'The room is cold, with faded grey stone walls. Two'
              'empty cells stand on either side, their bars casting'
              'long shadows. A metal door leads to another similar'
              'room, behind heavy iron gates. The floor is'
              'cracked and stained, and dim light from a flickering'
              'bulb casts eerie shadows across the space,'
              'giving the room a heavy, oppressive feeling.')
              interactables: #() asSet
              items: #() asSet.
        envyCell := Location name: 'Envy Cell'
              description: #(
              'The room is cold and empty, with cracked walls'
              'closing in. A narrow cot sits against one wall,'
              'covered by a thin, tattered blanket and a notebook on it. A small,'
              'tarnished mirror on the opposite wall distorts the'
              'reflection. The air feels thick with bitterness, and'
              'a small window offers only a sliver of the outside'
              'world. The floor is covered in a layer of dust,'
              'untouched, as if no one has ever truly rested here.')
              interactables: #('blanket' 'notebook') asSet
              items: #() asSet.
        slothCell := Location name: 'Sloth Cell'
              description:  #(
              'The room is dim and neglected, with a sagging'
              'bed in the corner. Empty containers and crumpled'
              'papers litter the floor. A cracked lamp barely'
              'lights the space, and the air is stale, untouched by'
              'effort or care.')
              interactables: #('paper' 'written_paper') asSet "unlit torch?"
              items: #('unlit_torch') asSet.
        highSecurityPrison := Location name: 'High Security Prison'
              description: #(
              'The room is stark and oppressive, with an atmosphere'
              'of cold metal and concrete. To the right, a cell'
              'stands behind thick, reinforced bars, making it clear'
              'this is a place of high security. To the left, heavy'
              'steel doors loom, their solid form unyielding and'
              'intimidating. In front of you, there is another set of'
              'doors, even more imposing, with a magnetic card'
              'reader beside them, suggesting a high-tech lock'
              'system. Everything in this room exudes a sense'
              'of being locked down, impenetrable, and designed'
              'to keep any potential threat contained.')
              interactables: #('doors') asSet
              items: #() asSet. 
        generatorRoom := Location name: 'Generator Room'
              description:  #(
              'The room vibrates with the hum of a massive generator'
              'at its center, filling the air with the scent of'
              'oil and metal. Pipes and wires snake along the walls,'
              'some hissing softly. The floor is stained, and the'
              'flickering lights cast unsettling shadows, making'
              'the room feel both powerful and dangerous.')
              interactables: #('generator' 'lit_torch') asSet
              items: #() asSet.
        wrathCell := Location name: 'Wrath Cell'
              description: #(
              'The cell is dark and cramped, with the walls'
              'seemingly closing in. Scratches and dents mar the'
              'surfaces, as if something—or someone—has been'
              'violently thrashing. In one corner, a broken chair'
              'lies half-smashed, and shattered glass glistens on'
              'the floor. The air is thick with a sense of rage that'
              'never left, a heaviness that hangs in the oppressive'
              'silence. A rusted, bloodstained chain dangles'
              'from the ceiling, suggesting past violence, while the'
              'faint smell of sweat and anger lingers in the air.')
              interactables: #('scratches' 'chain' 'chair') asSet
              items: #() asSet.
        lab := Location name: 'Lab'
              description: #(
              'Th3 d00r cl0s3s b3hind y0u...'
              'Gr33n l1qu1d f1lls th3 c0mp@rtm3nts w1th h@lf-c0nsci0us b0d13s.'
              'M@ny ch3m1c@l @pp@r@tus3s @r3 3v3rYwh3r3.'
              '@t th3 3nd, @ r3d c@rp3t c0v3rs th3 fl00r, w1th'
              '@ c1rcl3 0f fl@m3s @nd @n 0p3n b00k.'
              ''
              'P!CK: '
              ''
              '1. Y0u r34d wh@ts 1n th3 b00k @nd p3rf0rm th3 r1tual,'
              'b3c0m1ng 1mm0rt@l, but y0u c@nn0t 3v3r'
              'l3@v3 th1s h0us3 @g@1n.'
              ''
              '2. Y0u br34k th3 c0mp@rtm3nts @nd try t0 s@v3 th3s3 p30pl3,'
              'but y0u d0nt kn0w wh@t th3Y r34llY @r3.'
              ''
              '3. You noticed a chirping bird that flew out through a hole in the wall.'
              'You decide to escape through it too.'
              'Y0U RUN F0R Y0UR L1F3 THR0UGH TH3 A1SL3') asSet
              interactables: #('r1tual' 'h3lp' 'a1sl3') asSet
              items: #() asSet.
        
        "garden    north: hall. Requires golden_key"
        garden    east: glasshouse.
        "hall      north:  lowSecurityPrison.  Requires rune1 and rune2 to be used"
        hall      south:  garden.
        hall      east:  banquetHall.
        hall      west:  library.
        glasshouse west:  garden.
        library   east:  hall.
        banquetHall east:  closet.
        banquetHall west:  hall.
        closet west:  banquetHall.
        lowSecurityPrison      north:  mediumSecurityPrison.
        lowSecurityPrison      south:  hall.
        lowSecurityPrison      east:  lustCell.
        lowSecurityPrison      west:  gluttonyCell.
        gluttonyCell  east:  lowSecurityPrison.
        lustCell west:  lowSecurityPrison.
        mediumSecurityPrison     north:  highSecurityPrison.
        mediumSecurityPrison     south:  lowSecurityPrison.
        mediumSecurityPrison     east:  slothCell.
        mediumSecurityPrison     west:  envyCell.
        envyCell east:  mediumSecurityPrison.
        slothCell west:  mediumSecurityPrison.
        "highSecurityPrison      north:  lab.  Requires magnetic_card"
        highSecurityPrison      south:  mediumSecurityPrison.
        highSecurityPrison      east:  wrathCell.
        highSecurityPrison      west:  generatorRoom. 
        "generatorRoom east:  highSecurityPrison.  Requires generator to be on or lit_torch to be used"
        wrathCell west:  highSecurityPrison.
              
        "State:"
        isGameOver := false.
        location := garden.
        inventory := #() asSet.
        
        "Create a map of item actions"
        itemMap := Dictionary new.
    
        itemMap at: 'slingshot' put: [ 
        hasPlayedSlingshot := GuessingGame playGuessingGame.
        hasPlayedSlingshot ifTrue: [
                self printLines: #('You took too many shots... The bird is killed!').
                isBirdKilled := true.
              lab description: #(
              'Th3 d00r cl0s3s b3hind y0u...'
              'Gr33n l1qu1d f1lls th3 c0mp@rtm3nts w1th h@lf-c0nsci0us b0d13s.'
              'M@ny ch3m1c@l @pp@r@tus3s @r3 3v3rYwh3r3.'
              '@t th3 3nd, @ r3d c@rp3t c0v3rs th3 fl00r, w1th'
              '@ c1rcl3 0f fl@m3s @nd @n 0p3n b00k.'
              ''
              'P!CK: '
              ''
              '1. Y0u r34d wh@ts 1n th3 b00k @nd p3rf0rm th3 r1tual,'
              'b3c0m1ng 1mm0rt@l, but y0u c@nn0t 3v3r'
              'l3@v3 th1s h0us3 @g@1n.'
              ''
              '2. Y0u br34k th3 c0mp@rtm3nts @nd try t0 s@v3 th3s3 p30pl3,'
              'but y0u d0nt kn0w wh@t th3Y r34llY @r3.').
              lab interactables: #('r1tual' 'h3lp') asSet.
            ]
            ifFalse: [
                self printLines: #('Golden key falls to the ground').
                glasshouse items add: 'golden_key'.
                glasshouse interactables remove: 'slingshot'.
            ].
        ].
        itemMap at: 'book' put: [ self printLines: #('There are a lot of books in the library.'
  'They are indexed with two numbers: row and column from 0 to 200.'
  'You can search any of them.'
  'Enter two numbers separated by a space:'). 
  self readCommand = '89 144' ifTrue: [
                self printLines: #('Congratulations! You guessed the correct numbers!' 'a rune appears in library').
                library items add: 'rune1'.
                library interactables remove: 'book'.
                ]
            ifFalse: [
                self printLines: #('Wrong try again').
            ].
  "Play" ].
        itemMap at: 'piece_of_paper' put: [ self printLines: #('I am a sequence where each number grows'
          '"By adding the two that came before it, it shows'
          'Find the pair just under 200s crest'
          '"The penultimate and ultimate in my test'). ].
        itemMap at: 'window' put: [ 
            isWindowOpen ifTrue: [
                self printLines: #('You close the window. It becomes quiet.').
                isWindowOpen := false
                ]
            ifFalse: [
                self printLines: #('You open the window. You can hear the wind blowing from outside.').
                isWindowOpen := true
            ]
        ].
        itemMap at: 'painting' put: [ self printLines: #(          'In the painting: A thick fog blankets a dark, twisted forest'
          'Gnarled trees loom like dark silhouettes, their branches'
          'reaching out like claws. In the distance, a decaying'
          'mansion stands, its broken windows resembling'
          'hollow eyes. The faint glow of a flickering lantern'
          'casts an eerie light on a narrow, winding path'
          'leading toward the mansion, swallowed by the thick darkness.'). ].
        itemMap at: 'chandelier' put: [ self printLines: #('The broken chandelier swings, its shattered crystals casting'
          'twisted shadows in the dim light – better not walk beneath it.'). ].
        itemMap at: 'skeletons' put: [ self printLines: #('Decayed skeletons are scattered around. Some sit in chairs,'
          'others lie on the floor, their bones twisted and lifeless.'
          'Flickering light casts eerie shadows on their empty eye sockets.'). ].
        itemMap at: 'crystal_ball' put: [ self printLines: #(          'You picked up the crystal ball, but it slipped'
          'from your hands and fell to the ground, shattering.'
          'Something strange fell out of it.'). 
            closet items add: 'rune2'.
            closet interactables remove: 'crystal_ball'.].
        itemMap at: 'floor' put: [ self printLines: #('There is such a mess in here...'). ].
        itemMap at: 'magnetic_card' put: [ highSecurityPrison north: lab. ].
        itemMap at: 'golden_key' put: [ garden north: hall. ].
        itemMap at: 'rune1' put: [ (inventory includes: 'rune2') ifTrue: [hall north:  lowSecurityPrison.]. ].
        itemMap at: 'rune2' put: [ (inventory includes: 'rune2') ifTrue: [hall north:  lowSecurityPrison.]. ].
        itemMap at: 'rotting_food' put: [ self printLines: #('A lot of stinky, fat, rotting food. It smells horribly...'). ].
        itemMap at: 'dirty_floor' put: [ self printLines: #('Floor is really sticky. Somebody spilled a lot of wine in there.'). ].
        itemMap at: 'chest' put: [ self printLines: #('A chest full of bones and some kind of meat. I hope'
          'its not a human...'). ].
        itemMap at: 'magnet' put: [ self printLines: #('Such a strong magnet. Looks like its powered by electicity.'). ].
        itemMap at: 'tapestry' put: [ self printLines: #('The tapestry depicts a man and a woman, naked, passionately kissing.'). ].
        itemMap at: 'broken_mirrors' put: [ self printLines: #('There is a lot of broken mirrors laying on the ground.'
          'Looks like a person here cared about his looks a lot...'). ].
        itemMap at: 'blanket' put: [ self printLines: #('A terribly torn blanket—someone must have suffered greatly here.'). ].
        itemMap at: 'notebook' put: [ self printLines: #('I cant take this anymore. Its driving me insane.'
          'These bastards keep forcing us to drink some'
          'kind of potions, and every time, I feel worse.'
          'My body is burning, my head is spinning, and I cant'
          'stop shaking. Its like theyre doing this on purpose.'
          'My cellmate—he mentioned something about'
          'them testing their potions on us, trying to find a cure'
          'for immortality. I dont even know what to believe anymore.'
          'Theyve labeled me as envious. Envious? I was just talking'
          'to my coworkers wife, for Gods sake! Its'
          'all a misunderstanding, a fucking mistake. I swear'
          'to God, I wasnt doing anything wrong. But now'
          'they have me here, locked up in this hellhole,'
          'torturing me with their drugs and lies.'
          'Im so fucking scared. What are they doing to us?'
          'I just want out of here. I dont deserve this. Please...'
          'please, someone, just let us go............'). ].
        itemMap at: 'paper' put: [ self printLines: #('He was too lazy to stand up and throw them into the bin...'). ].
        itemMap at: 'written_paper' put: [ self printLines: #('I dont understand why they keep me here. All I'
          'ever wanted was a happy life, nothing more. I'
          'never asked for this... for any of this. The fear...'
          'its overwhelming. I feel trapped, like a caged'
          'animal. The things that keep me here, I dont'
          'even think they are fully human. They speak in a'
          'strange way, like their words are not their own,'
          'and their eyes... they are empty, lifeless. No'
          'emotion. Just cold, heartless beings. Their cruelty'
          'knows no bounds. They dont care, they never'
          'cared. I cant escape, no matter how hard I try. I fear'
          'they have already taken something from me,'
          'something inside. I dont know how much longer'
          'I can endure this. Please, someone... anyone... I'
          'just want to be free...'). ].
        itemMap at: 'lit_torch' put: [ 
        (inventory includes: 'unlit_torch') ifTrue: [
                self printLines: #('You take the unlit torch from your inventory and light it up.').
                inventory remove: 'unlit_torch'.
                inventory add: 'lit_torch'.
                generatorRoom east:  highSecurityPrison.
                ]
            ifFalse: [
                self printLines: #('The torch is burning but fixed to the wall.'
              'You could definitely use it to set something on fire.').

            ]
        ].
        itemMap at: 'chain' put: [ self printLines: #('The chain is covered in a large amount of dried'
          'blood, as if someone had been hung here by their'
          'hands, struggling violently and injuring themselves.'). ].
        itemMap at: 'generator' put: [ 
            isGeneratorOn ifTrue: [
                self printLines: #('Power off.').
                isGeneratorOn := false.
                (inventory includes: 'magnetic_card') ifFalse: [ lustCell items add: 'magnetic_card'].
                (inventory includes: 'lit_torch') ifFalse: [ generatorRoom east:  nil.].
                ]
            ifFalse: [
                self printLines: #('Power on.').
                isGeneratorOn := true.
                lustCell items remove: 'magnetic_card' ifAbsent: [].
                generatorRoom east:  highSecurityPrison.
            ]
        ].
        itemMap at: 'chair' put: [ self printLines: #('The chair is barely holding together; someone must have thrown it around a lot.'). ].
        itemMap at: 'r1tual' put: [ self printLines: #( 'Y0u b3c0m3 1mm0rt@l, but @t th3 c0st 0f n3v3r'
          'b3ing @bl3 t0 l34v3 th1s h0us3. T3rr0r1f1c v01c3s'
          'p0ss3ss y0u, dr1v1ng y0u t0 m@dness.'
          'Y0u w1ll n3v3r kn0w p3@c3 @g@1n...'
          '1/3 Madness Ending. Use command:  q     to leave.'
          ''). ].
        itemMap at: 'h3lp' put: [ self printLines: #('Y0u m@n@g3 t0 fr33 th3 p30pl3 1n th3 gr33n l1qu1d,'
          '@nd th3Y r34v3@l th31r trU3 f0rm. Th3y'
          '@r3 n0t hum@ns, but s0m3th1ng m0r3 d@rk @nd'
          'anc13nt. Th3y b3g1n t0 c0rrupt y0ur m1nd,'
          'wh1sp3r1ng 1n y0ur 34rs, thr34t3n1ng t0 d3stROY 4ll'
          'th@t y0u kn0w. Y0u r3@l1z3 th@t y0u h@v3'
          'f@ll3n 1nt0 th31r tr@p, @nd th@t n0t 3v3n y0u'
          'c@nn0t 3sc@p3 th31r p0w3r. Y0ur b0dY 1s st1ll'
          'y0urs, but y0ur s0ul 1s n0w th31rs.'
          '2/3 Trap Ending. Use command:  q     to leave.'). ].
        itemMap at: 'a1sl3' put: [ self printLines: #(          'You escaped through the hole, following the bird.'
          'It led you outside the building, and after what'
          'you saw inside, you decided to get as far away from'
          'there as possible. A long journey lies ahead, with'
          'many questions waiting to be answered—who you'
          'are and how you ended up in that terrifying'
          'place. But you remain hopeful.'
          '3/3 Good Ending. Use command:  q     to leave.'). ].
        
        ]

    "Print strings from array in separate lines."
    printLines: anArray [anArray do: [ :string | Transcript show: string; cr ]]
    
    "Print the description of a location."
    printLocationDescription: aLocation [
    Transcript show: aLocation name; cr.
    self printLines: aLocation description.
    self printLines: #(' ').
    self printLines: #('#Interactibles here:').
    self printLines: aLocation interactables.
    self printLines: #('#Inventory:').
    self printLines: inventory.
    self printLines: #('#Items here:').
    self printLines: aLocation items.
    self printLines: #(' ').
    ]
    
     "Move in a direction if possible."
    move: direction [
        | newLocation |
        direction = 'north' ifTrue: [ newLocation := location north ].
        direction = 'south' ifTrue: [ newLocation := location south ].
        direction = 'east' ifTrue: [ newLocation := location east ].
        direction = 'west' ifTrue: [ newLocation := location west ].

        newLocation isNil ifTrue: [self printLines: #('You cannot go there.' '')]
        ifFalse: [location := newLocation. self printLocationDescription: location].
        
        "newLocation isNil ifFalse: [location := newLocation. printLocationDescription location]."
    ]
    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    "Run the game."
run [
    | cmd commandMap |
    
    self printLocationDescription: location.
    self printLines: instructionsText.

    "Create a map of commands to corresponding actions"
    commandMap := Dictionary new.

    commandMap at: 'instructions' put: [ self printLines: instructionsText. ].
    commandMap at: 'i' put: [ self printLines: instructionsText. ].
    commandMap at: 'quit' put: [ isGameOver := true ].
    commandMap at: 'q' put: [ isGameOver := true ].
    commandMap at: 'n' put: [ self move: 'north'. ].
    commandMap at: 's' put: [ self move: 'south'. ].
    commandMap at: 'e' put: [ self move: 'east'. ].
    commandMap at: 'w' put: [ self move: 'west'. ].
    commandMap at: 'north' put: [ self move: 'north'. ].
    commandMap at: 'south' put: [ self move: 'south'. ].
    commandMap at: 'east' put: [ self move: 'east'. ].
    commandMap at: 'west' put: [ self move: 'west'. ].
    commandMap at: 'description' put: [self printLocationDescription: location].
    commandMap at: 'd' put: [self printLocationDescription: location].
    
    commandMap at: 'adminplayslingshot' put: [(itemMap at: 'slingshot') value.].
    commandMap at: 'adminskipslingshot' put: [glasshouse items add: 'golden_key'. glasshouse interactables remove: 'slingshot'.].
    commandMap at: 'admintorch' put: [inventory add: 'lit_torch'. generatorRoom east:  highSecurityPrison.].
    commandMap at: 'adminrunes' put: [inventory add: 'rune2'. inventory add: 'rune1'. hall north:  lowSecurityPrison.].
    commandMap at: 'admincard' put: [inventory add: 'magnetic_card'. highSecurityPrison north: lab.].
    commandMap at: 'adminkey' put: [inventory add: 'golden_key'. garden    north: hall.].
    commandMap at: 'adminunlockall' put: [
    generatorRoom east:  highSecurityPrison. hall north:  lowSecurityPrison. highSecurityPrison north: lab. garden north: hall.].
    
    

"Game loop."
[isGameOver] whileFalse: [
    | cmd action prefix |
    cmd := self readCommand.
    
    "Check if the command is in the dictionary and execute the corresponding action"
    action := commandMap at: cmd ifAbsent: [ nil ].
    action ifNotNil: [ 
        action value. "Execute the action"
    ] ifNil: [
        "Handle 'interact' commands"
        prefix := 'interact '.
        (cmd size >= prefix size and: [(cmd copyFrom: 1 to: prefix size) = prefix]) ifTrue: [
            cmd := cmd copyFrom: (prefix size + 1) to: cmd size. "Extract item after 'interact '"
            ((location interactables) includes: cmd) ifTrue: [
                action := itemMap at: cmd ifAbsent: [ nil ].
                action ifNotNil: [ 
                    action value.
                ] ifNil: [
                    self printLines: #('Unknown interact.' '')
                ].
            ] ifFalse: [ 
                self printLines: #('Unknown interact.' '')
            ].
        ] ifFalse: [
            "Handle 'pickup' commands"
            prefix := 'pickup '.
            (cmd size >= prefix size and: [(cmd copyFrom: 1 to: prefix size) = prefix]) ifTrue: [
                cmd := cmd copyFrom: (prefix size + 1) to: cmd size. "Extract item after 'pickup '"
                ((location items) includes: cmd) ifTrue: [
                    self printLines: #('You picked up something' ).
                    inventory add: cmd.
                    location items remove: cmd.
                    action := itemMap at: cmd ifAbsent: [ nil ].
                    action ifNotNil: [ 
                        action value.
                    ] ifNil: [
                        self printLines: #('')
                    ].
                ] ifFalse: [ 
                self printLines: #('Unknown pickup.' '')
                ].
            ] ifFalse: [
                self printLines: #('Unknown command.' ' ').
                self printLines: instructionsText.
            ].
        ].
    ].
]


]

]

Game new run.