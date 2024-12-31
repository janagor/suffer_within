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

Object subclass: Game [
    | 
    "Locations:"
    garden hall glasshouse library banquetHall closet lowSecurityPrison gluttonyCell
    lustCell mediumSecurityPrison envyCell slothCell highSecurityPrison generatorRoom wrathCell lab 
    "Flags:"
    isHallOpen isBirdKilled isGoldenKeyTaken isCrystalBallBroken isLaboratoryOpen isPrisonOpen 
    hasPlayedNumberPair isRune2Taken isTorchLit isGeneratorOn hasPlayedSlingshot isWindowOpen
    "State:"
    isGameOver location dinamicInteractables inventory 
    |

    introductionText := #('Enter command (n/s/e/w to move, `interact <item>`, `pickup <item>`, or `q` to quit): ' '' ).
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
              interactables: #('slingshot')
              items: #().
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
              interactables: #('piece_of_paper' 'window' 'book')
              items: #().
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
              interactables: #('painting' 'chandelier' 'skeletons')
              items: #().
        closet := Location name: 'Closet'
              description: #(
              'The closet is cramped and cluttered, filled with shelves of scattered items.'
              'Old boxes, forgotten dishes, rusted tools, and scraps of fabric cover every surface.'
              'On the floor, a pile of unsorted books lies forgotten, while an old dusty trunk sits.'
              'In the corner stands a strange crystal ball, inside which something unsettling moves.'
              'Faint wisps of mist swirl within, occasionally forming shapes as if something is trying to escape.'
              'The air smells of dampness and mildew, and the darkness seems to swallow every corner.'
              'The place holds more secrets than it lets on, hidden beneath layers of dust and shadow.')
              interactables: #('crystal ball' 'floor')
              items: #().
        lowSecurityPrison := Location name: 'Low Security Prison'
              description: #(
              'The room is dimly lit, with two small cells on either'
              'side, their rusted barred doors creaking. The air is'
              'stale, filled with the scent of mold and cold stone.'
              'Ahead, through open metal doors, lies another'
              'similar room, and the sound of dripping water'
              'echoes through the silence. The oppressive stillness'
              'makes the space feel even more confining.')
              interactables: #()
              items: #().
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
              interactables: #('rotting food''floor''chest')
              items: #().
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
              interactables: #('magnet''tapestry''broken mirrors')
              items: #().
        mediumSecurityPrison := Location name: 'Medium Security Prison'
              description: #(
              'The room is cold, with faded grey stone walls. Two'
              'empty cells stand on either side, their bars casting'
              'long shadows. A metal door leads to another similar'
              'room, behind heavy iron gates. The floor is'
              'cracked and stained, and dim light from a flickering'
              'bulb casts eerie shadows across the space,'
              'giving the room a heavy, oppressive feeling.')
              interactables: #()
              items: #().
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
              interactables: #('blanket''notebook')
              items: #().
        slothCell := Location name: 'Sloth Cell'
              description:  #(
              'The room is dim and neglected, with a sagging'
              'bed in the corner. Empty containers and crumpled'
              'papers litter the floor. A cracked lamp barely'
              'lights the space, and the air is stale, untouched by'
              'effort or care.')
              interactables: #('paper''written paper')
              items: #().
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
              interactables: #('doors')
              items: #(). 
        generatorRoom := Location name: 'Generator Room'
              description:  #(
              'The room vibrates with the hum of a massive generator'
              'at its center, filling the air with the scent of'
              'oil and metal. Pipes and wires snake along the walls,'
              'some hissing softly. The floor is stained, and the'
              'flickering lights cast unsettling shadows, making'
              'the room feel both powerful and dangerous.')
              interactables: #('generator''lit torch')
              items: #().
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
              interactables: #('scratches''chain''chair')
              items: #().
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
              '3. Y0U RUN F0R Y0UR L1F3 THR0UGH TH3 A1SL3')
              interactables: #('r1tual''h3lp''a1sl3')
              items: #().
        
        garden    north: hall. "Requires golden_key"
        garden    east: glasshouse.
        hall      north:  lowSecurityPrison. " Requires rune1 and rune2 to be used"
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
        highSecurityPrison      north:  lab. " Requires magnetic_card and generator to be on"
        highSecurityPrison      south:  mediumSecurityPrison.
        highSecurityPrison      east:  wrathCell.
        highSecurityPrison      west:  generatorRoom. " Requires generator to be on or unlit_torch to be used"
        generatorRoom east:  highSecurityPrison.
        wrathCell west:  highSecurityPrison.
              
        "State:"
        isGameOver := false.
        location := garden.
        
        
        ]

    "Print strings from array in separate lines."
    printLines: anArray [anArray do: [ :string | Transcript show: string; cr ]]
    "Print the description of a location."
    printLocationDescription: aLocation [self printLines: aLocation description.]
    
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

    self printLines: introductionText.
    self printLines: instructionsText.

    "Create a map of commands to corresponding actions"
    commandMap := Dictionary new.

    commandMap at: 'instructions' put: [ self printLines: instructionsText. ].
    commandMap at: 'quit' put: [ isGameOver := true ].
    commandMap at: 'q' put: [ isGameOver := true ].
    commandMap at: 'test' put: [ | d | d:= garden east. self printLocationDescription: d ].
    commandMap at: 'n' put: [ self move: 'north'. ].
    commandMap at: 's' put: [ self move: 'south'. ].
    commandMap at: 'e' put: [ self move: 'east'. ].
    commandMap at: 'w' put: [ self move: 'west'. ].
    commandMap at: 'd' put: [self printLocationDescription: location].

    "Game loop."
    [isGameOver] whileFalse: [
        cmd := self readCommand.
        "Check if the command is in the dictionary and execute the corresponding action"
        commandMap at: cmd ifPresent: [ :action | action value ].
        "If the command is not found in the map, print unknown command"
        (commandMap at: cmd ifAbsent: [ nil ]) isNil ifTrue: [
            self printLines: #('Unknown command.' '')
        ]
    ]
]

]

Game new run.