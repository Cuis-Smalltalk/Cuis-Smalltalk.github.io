## BorderedRectMorph

BorderedRectMorph introduce borders to RectangleLikeMorph. Borders have the instanceVariables borderWidth and borderColor. BorderedRectMorph subclasses can use a variety of border styles: simple, inset, raised BorderedRectMorph new borderColor: Color red; borderWidth: 10; openInWorld. BorderedRectMorph new borderColor: Color white; openInWorld

### Methods
#### BorderedRectMorph>>#isOpaqueMorph

Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:) but is not an opaque rectangle covering bounds MUST answer false to this message


<details>
	<summary>See more</summary>
	
	isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	borderWidth > 0 ifTrue: [
		borderColor mightBeTranslucent ifTrue: [
			^false ]].
	^true
</details>

#### BorderedRectMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.
	"initialize the receiver state related to border"
	borderColor _ self defaultBorderColor.
	borderWidth _ self defaultBorderWidth
</details>

#### BorderedRectMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #BorderedRectMorph or: [ super is: aSymbol ]
</details>

#### BorderedRectMorph>>#borderColor: aColor

<details>
	<summary>See more</summary>
	
	borderColor: aColor
	borderColor = aColor ifFalse: [
		borderColor _ aColor.
		self redrawNeeded]
</details>

#### BorderedRectMorph>>#borderWidth

<details>
	<summary>See more</summary>
	
	borderWidth
	^ borderWidth
</details>

#### BorderedRectMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"If you redefine this method in a subclass, please take a look at the comment at #isOpaqueMorph"
	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor
</details>

#### BorderedRectMorph>>#defaultBorderColor

answer the default border color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color gray`
</details>

#### BorderedRectMorph>>#borderColor

<details>
	<summary>See more</summary>
	
	borderColor
	^ borderColor
</details>

#### BorderedRectMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 2
</details>

#### BorderedRectMorph>>#borderWidth: anInteger

<details>
	<summary>See more</summary>
	
	borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth _ anInteger max: 0.
		self redrawNeeded ]
</details>

## DraggingGuideMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DraggingGuideMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	extent _ `0@0`.
</details>

#### DraggingGuideMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol

	^aSymbol == #DraggingGuideMorph or: [ super is: aSymbol ]
</details>

#### DraggingGuideMorph>>#justDroppedInto: newOwnerMorph event: anEvent

This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph


<details>
	<summary>See more</summary>
	
	justDroppedInto: newOwnerMorph event: anEvent 
	
	self delete.
	anEvent hand redrawNeeded.
</details>

#### DraggingGuideMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor

	^Color transparent
</details>

## HandMorph

The cursor may be thought of as the HandMorph. The hand's submorphs hold anything being carried by dragging. There is some minimal support for multiple hands in the same world. grabMorphData is a dictionary of Morph->{prevOwner,prevPos} for morphs being carried.

### Methods
#### HandMorph>>#startDropEventDispatch: aDropEvent

<details>
	<summary>See more</summary>
	
	startDropEventDispatch: aDropEvent

	owner dispatchEvent: aDropEvent localPosition: aDropEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent
</details>

#### HandMorph>>#releaseKeyboardFocus

Release the current keyboard focus unconditionally


<details>
	<summary>See more</summary>
	
	releaseKeyboardFocus
	"Release the current keyboard focus unconditionally"
	self newKeyboardFocus: nil.

</details>

#### HandMorph>>#generateWindowEvent: evtBuf

Generate the appropriate window event for the given raw event buffer


<details>
	<summary>See more</summary>
	
	generateWindowEvent: evtBuf 
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time localMillisecondClock].
	evt windowAction: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt
</details>

#### HandMorph>>#activatePreviousWindow

<details>
	<summary>See more</summary>
	
	activatePreviousWindow

	self previousFocusWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: false ]]
</details>

#### HandMorph>>#removePendingBalloonFor: aMorph

Get rid of pending balloon help.


<details>
	<summary>See more</summary>
	
	removePendingBalloonFor: aMorph
	"Get rid of pending balloon help."
	self removeAlarm: #spawnBalloonFor:.
	self deleteBalloonTarget: aMorph.
</details>

#### HandMorph>>#nextFocusMorph

Or nil


<details>
	<summary>See more</summary>
	
	nextFocusMorph
	"Or nil"

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboard and: [ m isReallyVisible ]]
</details>

#### HandMorph>>#startWindowEventDispatch: aWindowEvent

<details>
	<summary>See more</summary>
	
	startWindowEventDispatch: aWindowEvent

	owner dispatchEvent: aWindowEvent localPosition: aWindowEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent
</details>

#### HandMorph>>#dropMorphs: anEvent

Drop the morphs at the hands position


<details>
	<summary>See more</summary>
	
	dropMorphs: anEvent
	"Drop the morphs at the hands position"
	self submorphsReverseDo:[:m|
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ]
</details>

#### HandMorph>>#keyboardFocus

<details>
	<summary>See more</summary>
	
	keyboardFocus

	keyboardFocus ifNotNil: [
		keyboardFocus world
			ifNil: [ keyboardFocus _ nil ]].
	^ keyboardFocus
</details>

#### HandMorph>>#previousFocusWindow

Or nil


<details>
	<summary>See more</summary>
	
	previousFocusWindow
	"Or nil"

	^ (SystemWindow topWindow ifNil: [ self world ])
		previousMorphThat: [ :m |
			(m is: #SystemWindow) and: [ m isReallyVisible ]]
</details>

#### HandMorph>>#forgetGrabMorphDataFor: aMorph

<details>
	<summary>See more</summary>
	
	forgetGrabMorphDataFor: aMorph

	grabMorphData 
		removeKey: aMorph 
		ifAbsent: [ "no error" ]
</details>

#### HandMorph>>#startEventDispatch: aMorphicEvent

<details>
	<summary>See more</summary>
	
	startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent startDispatchFrom: self
</details>

#### HandMorph>>#mouseTrailFrom: currentBuf

Current event, a mouse event buffer, is about to be processed. If there are other similar mouse events queued up, then drop them from the queue, and report the positions inbetween.


<details>
	<summary>See more</summary>
	
	mouseTrailFrom: currentBuf 
	"Current event, a mouse event buffer, is about to be processed.  If there are other similar mouse events queued up, then drop them from the queue, and report the positions inbetween."

	| nextEvent trail |
	trail := WriteStream on: (Array new: 1).
	trail nextPut: currentBuf third @ currentBuf fourth.
	[(nextEvent := Sensor peekEvent) isNil] whileFalse: 
			[nextEvent first = currentBuf first 
				ifFalse: [^trail contents	"different event type"].
			nextEvent fifth = currentBuf fifth 
				ifFalse: [^trail contents	"buttons changed"].
			nextEvent sixth = currentBuf sixth 
				ifFalse: [^trail contents	"modifiers changed"].
			"nextEvent is similar.  Remove it from the queue, and check the next."
			nextEvent := Sensor nextEvent.
			trail nextPut: nextEvent third @ nextEvent fourth].
	^trail contents
</details>

#### HandMorph>>#waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel

<details>
	<summary>See more</summary>
	
	waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel
	
	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2
</details>

#### HandMorph>>#startKeyboardDispatch: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement _ self keyboardFocus ifNil: [ self world ].
	focusedElement handleFocusEvent: aKeyboardEvent.
	
	self mouseOverHandler processMouseOver: self lastMouseEvent
</details>

#### HandMorph>>#startMouseDispatch: aMouseEvent

<details>
	<summary>See more</summary>
	
	startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [self dropMorphs: aMouseEvent ]]
				ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent localPosition: aMouseEvent eventPosition ]]]].
		self mouseOverHandler processMouseOver: self lastMouseEvent
</details>

#### HandMorph>>#newKeyboardFocus: aMorphOrNil

Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled.


<details>
	<summary>See more</summary>
	
	newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	oldFocus _ keyboardFocus.
	keyboardFocus _ aMorphOrNil.
	oldFocus ifNotNil: [oldFocus == aMorphOrNil ifFalse: [oldFocus keyboardFocusChange: false]].
	aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true].

</details>

#### HandMorph>>#fullDrawHandOn: aCanvas

A HandMorph has an unusual drawing requirement: The hand itself (i.e., the cursor) appears in front of its submorphs The illusion is that the hand plucks up morphs and carries them above the world.


<details>
	<summary>See more</summary>
	
	fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
</details>

#### HandMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask defaultCursor extent.
	damageRecorder _ DamageRecorder new.
	grabMorphData _ IdentityDictionary new.
	self initForEvents.
</details>

#### HandMorph>>#releaseCachedState

Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'.


<details>
	<summary>See more</summary>
	
	releaseCachedState
	| oo |
	super releaseCachedState.
	oo _ owner.
	self removeAllMorphs.
	self initialize.	"nuke everything"
	self privateOwner: oo.
	self releaseAllFoci.
</details>

#### HandMorph>>#rememberGrabMorphDataFor: aMorph

<details>
	<summary>See more</summary>
	
	rememberGrabMorphDataFor: aMorph

	grabMorphData
		at: aMorph 
		put: { aMorph owner. aMorph morphPositionInWorld. }
</details>

#### HandMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #HandMorph or: [ super is: aSymbol ]
</details>

#### HandMorph>>#deleteBalloonTarget: aMorph

Delete any existing balloon help. This is now done unconditionally, whether or not the morph supplied is the same as the current balloon target


<details>
	<summary>See more</summary>
	
	deleteBalloonTarget: aMorph
	"Delete any existing balloon help.  This is now done unconditionally, whether or not the morph supplied is the same as the current balloon target"
	
	self balloonHelp: nil

"	| h |
	h _ self balloonHelp ifNil: [^ self].
	h balloonOwner == aMorph ifTrue: [self balloonHelp: nil]"
</details>

#### HandMorph>>#previousFocusMorph

Or nil


<details>
	<summary>See more</summary>
	
	previousFocusMorph
	"Or nil"

	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m | 
			m handlesKeyboard and: [ m isReallyVisible ]]
</details>

#### HandMorph>>#keyboardFocusPrevious

<details>
	<summary>See more</summary>
	
	keyboardFocusPrevious

	| m |
	m _ self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m ]
</details>

#### HandMorph>>#flushEvents

Flush any events that may be pending


<details>
	<summary>See more</summary>
	
	flushEvents
	"Flush any events that may be pending"
	self flag: #arNote. "Remove it and fix senders"
	Sensor flushEvents.
</details>

#### HandMorph>>#nextFocusWindow

Or nil


<details>
	<summary>See more</summary>
	
	nextFocusWindow
	"Or nil"

	^(SystemWindow topWindow ifNil: [ self world ])
		nextMorphThat: [ :m | 
			(m is: #SystemWindow) and: [ m isReallyVisible ]]
</details>

#### HandMorph>>#grabMorph: aMorph delta: delta

Grab the given morph (i.e., add it to this hand and remove it from its current owner).


<details>
	<summary>See more</summary>
	
	grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		h target == aMorph ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		grabMorphData
			at: aMorph 
			put: { formerOwner. aMorph morphPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.
	aMorph justGrabbedFrom: formerOwner
</details>

#### HandMorph>>#lastMouseEvent

<details>
	<summary>See more</summary>
	
	lastMouseEvent
	^ lastMouseEvent
</details>

#### HandMorph>>#generateDropFilesEvent: evtBuf

Generate the appropriate mouse event for the given raw event buffer


<details>
	<summary>See more</summary>
	
	generateDropFilesEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| position stamp numberOfFiles dragType |
	
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time localMillisecondClock].
	dragType := evtBuf third.
	position := evtBuf fourth @ evtBuf fifth.
	numberOfFiles := evtBuf seventh.
	
	^ dragType = 4 ifTrue: [ DropFilesEvent at: position with: numberOfFiles from: self].

</details>

#### HandMorph>>#mouseOverHandler

<details>
	<summary>See more</summary>
	
	mouseOverHandler
	^mouseOverHandler ifNil:[mouseOverHandler _ MouseOverHandler new].
</details>

#### HandMorph>>#generateKeyboardEvent: evtBuf

Generate the appropriate mouse event for the given raw event buffer


<details>
	<summary>See more</summary>
	
	generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time localMillisecondClock ].
	(evtBuf sixth <= 0 or: [ (keyValue _ Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth) isNil ]) ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	Sensor peekEvent ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and: [ nxt third > 255 ]) ifTrue: [
			keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
			Sensor
				nextEvent;
				nextEvent;
				nextEvent ].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"
		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and: [ evtBuf third = 13 ]) and: [
			nxt fourth = EventSensor eventKeyChar and: [ nxt third = 10 ]]) ifTrue: [ Sensor nextEvent
			"print " ]].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType _ EventSensor eventKeyChar ]
			ifFalse: [type _ #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [
			"Control key pressed"
			keyValue < 27 ifTrue: [
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [
					"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [
						"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96
								"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue _ 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]]].
	buttons _ modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection _ nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection _ #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection _ #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [Preferences ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection _ #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection _ #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setType: #mouseScroll
			position: self morphPosition
			direction: mouseScrollDirection
			buttons: buttons
			hand: self
			stamp: stamp ].
	^ KeyboardEvent new
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp
</details>

#### HandMorph>>#balloonHelp

Return the balloon morph associated with this hand


<details>
	<summary>See more</summary>
	
	balloonHelp
	"Return the balloon morph associated with this hand"
	^self valueOfProperty: #balloonHelpMorph
</details>

#### HandMorph>>#waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods.


<details>
	<summary>See more</summary>
	
	waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2
</details>

#### HandMorph>>#spawnBalloonFor: aMorph

<details>
	<summary>See more</summary>
	
	spawnBalloonFor: aMorph
	aMorph showBalloon: aMorph balloonText hand: self.
</details>

#### HandMorph>>#noticeMouseOver: aMorph event: anEvent

<details>
	<summary>See more</summary>
	
	noticeMouseOver: aMorph event: anEvent
	mouseOverHandler ifNil:[^self].
	mouseOverHandler noticeMouseOver: aMorph event: anEvent.
</details>

#### HandMorph>>#needsToBeDrawn

Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden.


<details>
	<summary>See more</summary>
	
	needsToBeDrawn
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty and there are any visible submorphs. If using the software cursor, ensure that the hardware cursor is hidden."
	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor from the display."
	"Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!"
	(savedPatch notNil or: [
		submorphs anySatisfy: [ :ea |
			ea visible ]]) ifTrue: [
		"using the software cursor; hide the hardware one"
		Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifFalse: [ (Cursor cursorAt: #blankCursor) activateCursor ].
		^ true ].
	^ false.
</details>

#### HandMorph>>#waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel

Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods.


<details>
	<summary>See more</summary>
	
	waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil 
			dblClick: dblClkSel
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false
</details>

#### HandMorph>>#grabMorphDataFor: aMorph

<details>
	<summary>See more</summary>
	
	grabMorphDataFor: aMorph

	^ grabMorphData at: aMorph ifAbsent: [ { nil. nil. } ]
</details>

#### HandMorph>>#restoreSavedPatchOn: aCanvas

Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor.


<details>
	<summary>See more</summary>
	
	restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas
			image: savedPatch
			at: savedPatch offset.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self
			invalidateDisplayRect: (self morphPosition extent: extent)
			from: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].
</details>

#### HandMorph>>#releaseMouseFocus

Release the current mouse focus unconditionally.


<details>
	<summary>See more</summary>
	
	releaseMouseFocus
	"Release the current mouse focus unconditionally."
	self newMouseFocus: nil.
</details>

#### HandMorph>>#redrawNeeded

Report that the area occupied by this morph should be redrawn.


<details>
	<summary>See more</summary>
	
	redrawNeeded

	hasChanged _ true
</details>

#### HandMorph>>#obtainHalo: aHalo

Used for transfering halos between hands


<details>
	<summary>See more</summary>
	
	obtainHalo: aHalo
	"Used for transfering halos between hands"
	| formerOwner |
	self halo == aHalo ifTrue:[^self].
	"Find former owner"
	formerOwner _ self world hands detect: [ :h | h halo == aHalo] ifNone: nil.
	formerOwner ifNotNil:[formerOwner releaseHalo: aHalo].
	self halo: aHalo
</details>

#### HandMorph>>#grabMorph: aMorph

Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand.


<details>
	<summary>See more</summary>
	
	grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	^self grabMorph: aMorph moveUnderHand: false
</details>

#### HandMorph>>#halo: newHalo

Set halo associated with this hand


<details>
	<summary>See more</summary>
	
	halo: newHalo
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo _ self halo.
	(oldHalo isNil or:[oldHalo == newHalo]) ifFalse:[oldHalo delete].
	newHalo
		ifNil:[self removeProperty: #halo]
		ifNotNil:[self setProperty: #halo toValue: newHalo]
</details>

#### HandMorph>>#hasChanged

Return true if this hand has changed, either because it has moved or because some morph it is holding has changed.


<details>
	<summary>See more</summary>
	
	hasChanged
	"Return true if this hand has changed, either because it has moved or because some morph it is holding has changed."

	^ hasChanged ifNil: [ true ]

</details>

#### HandMorph>>#savePatchFrom: aCanvas appendDamageTo: aStream

Save the part of the given canvas under this hand as a Form and return its bounding rectangle.


<details>
	<summary>See more</summary>
	
	savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self morphFullBoundsInWorld.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds intersect: (`0@0` extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds merge: prevFullBounds)].
	prevFullBounds _ fullBounds
</details>

#### HandMorph>>#attachMorphBeside: aMorph

Position the given morph beside this hand, then grab it.


<details>
	<summary>See more</summary>
	
	attachMorphBeside: aMorph

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	^self grabMorph: aMorph delta: (self morphWidth)@0
</details>

#### HandMorph>>#halo

Return the halo associated with this hand, if any


<details>
	<summary>See more</summary>
	
	halo
	"Return the halo associated with this hand, if any"
	^self valueOfProperty: #halo
</details>

#### HandMorph>>#dontWaitForMoreClicks

Reset the double-click detection state to normal (i.e., not waiting for a double-click).


<details>
	<summary>See more</summary>
	
	dontWaitForMoreClicks
	"Reset the double-click detection state to normal (i.e., not waiting for a double-click)."

	mouseClickState _ nil.
</details>

#### HandMorph>>#grabMorph: aMorph moveUnderHand: moveUnderHand

Grab the given morph (i.e., add it to this hand and remove it from its current owner). If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand.


<details>
	<summary>See more</summary>
	
	grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed delta |
	"#aboutToGrab: and #aboutToBeGrabbedBy: might mess wildly with our morphs.
	If we need it, prepare delta before that happens"
	moveUnderHand ifFalse: [
		delta _ aMorph morphPositionInWorld - self morphPositionInWorld ].
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o |
		grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	(moveUnderHand or: [ (grabbed morphExtent >= aMorph morphExtent) not ])
		ifTrue: [ delta _ (grabbed morphExtent // 2) negated ].
	^ self
		grabMorph: grabbed
		delta: delta
</details>

#### HandMorph>>#mouseFocus

<details>
	<summary>See more</summary>
	
	mouseFocus

	mouseFocus ifNotNil: [
		mouseFocus world
			ifNil: [ mouseFocus _ nil ]].
	^mouseFocus
</details>

#### HandMorph>>#initForEvents

<details>
	<summary>See more</summary>
	
	initForEvents
	mouseOverHandler _ nil.
	lastMouseEvent _ MouseEvent new setType: #mouseMove position: `0@0` buttons: 0 hand: self.
	lastMouseEventTime _ Time localMillisecondClock.
	lastEventBuffer _ {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks
</details>

#### HandMorph>>#newMouseFocus: aMorphOrNil

Make the given morph the new mouse focus, canceling the previous mouse focus if any. If the argument is nil, the current mouse focus is cancelled.


<details>
	<summary>See more</summary>
	
	newMouseFocus: aMorphOrNil
	"Make the given morph the new mouse focus, canceling the previous mouse focus if any. If the argument is nil, the current mouse focus is cancelled."
	mouseFocus _ aMorphOrNil.

</details>

#### HandMorph>>#generateMouseEvent: evtBuf

Generate the appropriate mouse event for the given raw event buffer


<details>
	<summary>See more</summary>
	
	generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| pos buttons modifiers type trail stamp oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time localMillisecondClock ].
	pos := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: [
			lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: [
			lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: [
			trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent new 
				setType: type
				position: trail last
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent new 
		setType: type
		position: pos
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp
</details>

#### HandMorph>>#dropMorph: aMorph event: aMouseEvent

Drop the given morph which was carried by the hand


<details>
	<summary>See more</summary>
	
	dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent _ DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	owner dispatchEvent: dropEvent localPosition: dropEvent eventPosition.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent
</details>

#### HandMorph>>#shouldControlEmulateAltFor: keyValue

At least on Linux Windows, command key is usually ctrl, not alt. But not for arrow keys! ctrl-left ~~ alt-left


<details>
	<summary>See more</summary>
	
	shouldControlEmulateAltFor: keyValue
	"At least on Linux Windows, command key is usually ctrl, not alt.
	But not for arrow keys! ctrl-left ~~ alt-left"
	^ keyValue > 32
</details>

#### HandMorph>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp
</details>

#### HandMorph>>#balloonHelp: aBalloonMorph

Return the balloon morph associated with this hand


<details>
	<summary>See more</summary>
	
	balloonHelp: aBalloonMorph
	"Return the balloon morph associated with this hand"
	| oldHelp |
	oldHelp _ self balloonHelp.
	oldHelp ifNotNil:[oldHelp delete].
	aBalloonMorph
		ifNil:[self removeProperty: #balloonHelpMorph]
		ifNotNil:[self setProperty: #balloonHelpMorph toValue: aBalloonMorph]
</details>

#### HandMorph>>#activateNextWindow

<details>
	<summary>See more</summary>
	
	activateNextWindow

	self nextFocusWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: true ]]
</details>

#### HandMorph>>#releaseMouseFocus: aMorph

If the given morph had the mouse focus before, release it


<details>
	<summary>See more</summary>
	
	releaseMouseFocus: aMorph
	"If the given morph had the mouse focus before, release it"
	self mouseFocus == aMorph ifTrue:[self releaseMouseFocus].
</details>

#### HandMorph>>#releaseHalo: aHalo

Used for transfering halos between hands


<details>
	<summary>See more</summary>
	
	releaseHalo: aHalo
	"Used for transfering halos between hands"
	self removeProperty: #halo
</details>

#### HandMorph>>#startDropFilesEventDispatch: aDropFilesEvent

<details>
	<summary>See more</summary>
	
	startDropFilesEventDispatch: aDropFilesEvent

	owner dispatchEvent: aDropFilesEvent localPosition: aDropFilesEvent eventPosition.
	self mouseOverHandler processMouseOver: lastMouseEvent
</details>

#### HandMorph>>#processEventQueue

Process user input events from the local input devices.


<details>
	<summary>See more</summary>
	
	processEventQueue
	"Process user input events from the local input devices."

	| evt evtBuf type hadAny mcs |
	mcs _ mouseClickState.
	hadAny := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt
			ifNil: [ 
				"I have to consume all eventTypeDragDropFiles of type 2 quicky, that is why
				I check if it was an eventTypeDragDropFiles to continue in the loop - Hernan"
				type ~= EventSensor eventTypeDragDropFiles ifTrue: [^hadAny]]
			ifNotNil: [
				"Finally, handle it"
				self startEventDispatch: evt.
				hadAny := true.
				"For better user feedback, return immediately after a mouse event has been processed."
				evt isMouse ifTrue: [ ^hadAny ]]].
	"note: if we come here we didn't have any mouse events"
	mcs 
		ifNotNil: [ 
			"No mouse events during this cycle. Make sure click states time out accordingly"
			mcs
				handleEvent: (self lastMouseEvent asMouseMove: (Time localMillisecondClock - self lastMouseEventTime max: 0))
				from: self ].
	^hadAny
</details>

#### HandMorph>>#triggerBalloonFor: aMorph after: timeOut

Trigger balloon help after the given time out for some morph


<details>
	<summary>See more</summary>
	
	triggerBalloonFor: aMorph after: timeOut
	"Trigger balloon help after the given time out for some morph"
	self addAlarm: #spawnBalloonFor: with: aMorph after: timeOut.
</details>

#### HandMorph>>#createEventFrom: eventBuffer ofType: type

<details>
	<summary>See more</summary>
	
	createEventFrom: eventBuffer ofType: type

	type = EventSensor eventTypeMouse ifTrue: [ ^self generateMouseEvent: eventBuffer ].
	type = EventSensor eventTypeKeyboard ifTrue: [ ^self generateKeyboardEvent: eventBuffer ].
	type = EventSensor eventTypeWindow ifTrue: [ ^self generateWindowEvent: eventBuffer ].
	type = EventSensor eventTypeDragDropFiles ifTrue: [ ^self generateDropFilesEvent: eventBuffer ].
		
	"All other events are ignored"
	^nil 
</details>

#### HandMorph>>#keyboardFocusNext

<details>
	<summary>See more</summary>
	
	keyboardFocusNext

	| m |
	m _ self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m ]
</details>

#### HandMorph>>#removeHaloFromClick: anEvent on: aMorph

<details>
	<summary>See more</summary>
	
	removeHaloFromClick: anEvent on: aMorph
	| halo |
	halo _ self halo ifNil:[^self].
	(halo target hasOwner: self) ifTrue:[^self].
	(halo staysUpWhenMouseIsDownIn: aMorph) ifFalse:[
		halo delete.
		self removeProperty: #halo.
	].
</details>

#### HandMorph>>#attachMorph: aMorph

Position the center of the given morph under this hand, then grab it. This method is used to grab far away or newly created morphs.


<details>
	<summary>See more</summary>
	
	attachMorph: aMorph
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."

	^self grabMorph: aMorph moveUnderHand: true
</details>

#### HandMorph>>#drawOn: aCanvas

Draw the hand itself (i.e., the cursor).


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	aCanvas
		stencil: (Cursor cursorAt: #moveCursor)
		at: `0 @ 0`
		color: Color black.
</details>

#### HandMorph>>#pasteMorph

<details>
	<summary>See more</summary>
	
	pasteMorph

	| pastee |
	pastee _ Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee
</details>

#### HandMorph>>#lastMouseEventTime

<details>
	<summary>See more</summary>
	
	lastMouseEventTime
	^ lastMouseEventTime
</details>

#### HandMorph>>#waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel

juntar los 3? documentar mejor? uno solo completo y shortcuts?


<details>
	<summary>See more</summary>
	
	waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel
"juntar los 3? documentar mejor? uno solo completo y shortcuts?"
	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: nil 
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false
</details>

#### HandMorph>>#releaseAllFoci

<details>
	<summary>See more</summary>
	
	releaseAllFoci
	mouseFocus _ nil.
	keyboardFocus _ nil.

</details>

#### HandMorph>>#releaseKeyboardFocus: aMorph

If the given morph had the keyboard focus before, release it


<details>
	<summary>See more</summary>
	
	releaseKeyboardFocus: aMorph
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus ifNotNil: [
		keyboardFocus withAllOwnersDo: [ :outerOwner |
			outerOwner == aMorph ifTrue: [self releaseKeyboardFocus]]]
</details>

## Morph

A Morph (from the Greek "shape" or "form") is an interactive graphical object. General information on the Morphic system can be found at http://wiki.squeak.org/squeak/morph. Morphs exist in a tree, rooted at a World (generally a PasteUpMorph). The morphs owned by a morph are its submorphs. Morphs are drawn recursively; if a Morph has no owner it never gets drawn. To hide a Morph and its submorphs, set its #visible property to false using the #visible: method. Structure: instance var Type Description owner Morph My parent Morph, or nil for the top-level Morph, which is a or nil world, typically a PasteUpMorph. submorphs Array My child Morphs. location GeometryTransformation Specifies position (and possibly, angle of rotation and scale change) inside owner See comment at GeometryTransformation extension MorphExtension Allows extra properties to be stored without adding a or nil storage burden to all morphs.

### Methods
#### Morph>>#hide

<details>
	<summary>See more</summary>
	
	hide
	owner ifNil: [^ self].
	self visible: false
</details>

#### Morph>>#processKeystroke: aKeyboardEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyStroke: aKeyboardEvent
</details>

#### Morph>>#transferHalo: event from: formerHaloOwner

Progressively transfer the halo to the next likely recipient


<details>
	<summary>See more</summary>
	
	transferHalo: event from: formerHaloOwner
	"Progressively transfer the halo to the next likely recipient"
	| eventLocalPos |

	formerHaloOwner == self
		ifFalse: [ ^self addHalo: event from: formerHaloOwner ].

	eventLocalPos _ self internalizeFromWorld: event eventPosition.
	event shiftPressed ifTrue: [
		"Pass it outwards"
		owner ifNotNil: [ ^owner transferHalo: event from: formerHaloOwner ].
		"We're at the top level; just keep it on ourselves"
		^self ].

	self submorphsDo: [ :m |
		(m wantsHalo and: [ m fullContainsPoint: (m internalize: eventLocalPos) ])
			ifTrue: [ ^m transferHalo: event from: formerHaloOwner ]].
	"We're at the bottom most level; just keep halo on ourselves"
</details>

#### Morph>>#privateDelete

Remove the receiver as a submorph of its owner


<details>
	<summary>See more</summary>
	
	privateDelete
	"Remove the receiver as a submorph of its owner"
	owner ifNotNil:[owner removeMorph: self].
</details>

#### Morph>>#allOwnersDo: aBlock

Evaluate aBlock with all owners of the receiver


<details>
	<summary>See more</summary>
	
	allOwnersDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]
</details>

#### Morph>>#processMouseMove: aMouseMoveEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"System level event handling."

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]
</details>

#### Morph>>#addToggleItemsToHaloMenu: aMenu

Add standard true/false-checkbox items to the memu


<details>
	<summary>See more</summary>
	
	addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(
		(stickinessString toggleStickiness 'whether I should be resistant to a drag done by mousing down on me')
		(lockedString lockUnlockMorph 'when "locked", I am inert to all user interactions')
	) do: [ :trip | 
			(aMenu addUpdating: trip first action: trip second)
				setBalloonText: trip third ]
</details>

#### Morph>>#dropFiles: aDropFilesEvent

I do nothing, subclasses should redefine if they handle this event - Hernan


<details>
	<summary>See more</summary>
	
	dropFiles: aDropFilesEvent

	"I do nothing, subclasses should redefine if they handle this event - Hernan"
</details>

#### Morph>>#collapse

If taskbar not visible, just hide.


<details>
	<summary>See more</summary>
	
	collapse
	"If taskbar not visible, just hide."

	self taskbar
		ifNotNil: [ :tb | tb aboutToCollapse: self ].
	self hide
</details>

#### Morph>>#visible

Answer if I am visible -- default is true


<details>
	<summary>See more</summary>
	
	visible
	"Answer if I am visible -- default is true"

	^ self valueOfProperty: #visible ifAbsent: [ true ]
</details>

#### Morph>>#lockedString

Answer the string to be shown in a menu to represent the 'locked' status


<details>
	<summary>See more</summary>
	
	lockedString
	"Answer the string to be shown in a menu to represent the 
	'locked' status"
	^ (self isLocked
		ifTrue: ['<on>']
		ifFalse: ['<off>']), 'be locked'
</details>

#### Morph>>#submorphs

<details>
	<summary>See more</summary>
	
	submorphs

	^ submorphs copy
</details>

#### Morph>>#mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 2 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
</details>

#### Morph>>#someSubmorphPositionOrExtentChanged

Our extent, or some submorph changed. Must layout submorphs again.


<details>
	<summary>See more</summary>
	
	someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	layoutNeeded _ true
</details>

#### Morph>>#morphContainsPoint: aLocalPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate! (or finish Morphic 3!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint
</details>

#### Morph>>#addStandardHaloMenuItemsTo: aMenu hand: aHandMorph

Add standard halo items to the menu


<details>
	<summary>See more</summary>
	
	addStandardHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu"

	| unlockables |

	self isWorldMorph ifTrue:
		[^ self addWorldHaloMenuItemsTo: aMenu hand: aHandMorph].

	aMenu add: 'send to back' action: #goBehind.
	aMenu add: 'bring to front' action: #comeToFront.
	self addEmbeddingMenuItemsTo: aMenu hand: aHandMorph.
	aMenu addLine.

	self addColorMenuItems: aMenu hand: aHandMorph.
	self addHaloActionsTo: aMenu.
	aMenu addLine.
	self addToggleItemsToHaloMenu: aMenu.
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.
	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[:m | m isLocked].
	unlockables size = 1 ifTrue:
		[aMenu
			add: ('unlock "{1}"' format: (unlockables first printStringLimitedTo: 40))
			action: #unlockContents].
	unlockables size > 1 ifTrue:
		[aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.

</details>

#### Morph>>#wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph

Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)


<details>
	<summary>See more</summary>
	
	wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	(#(addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].

	Preferences selectiveHalos ifFalse: [
		^true ].

	(#(#addDismissHandle: ) includes: aSelector)
		ifTrue: [ ^ self resistsRemoval not ].
	(#(#addDragHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToBrownDragEasily ].
	(#(#addGrowHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToResizeEasily ].
	(#(#addRotateHandle: ) includes: aSelector)
		ifTrue: [ ^ self okayToRotateEasily ].
	(#(#addRecolorHandle: ) includes: aSelector)
		ifTrue: [ ^ self wantsRecolorHandle ].
	^ true
</details>

#### Morph>>#maybeDuplicateMorph

Maybe duplicate the morph


<details>
	<summary>See more</summary>
	
	maybeDuplicateMorph
	"Maybe duplicate the morph"

	self okayToDuplicate ifTrue:
		[self duplicate openInHand]
</details>

#### Morph>>#processUnknownEvent: aMorphicEvent localPosition: localEventPosition

An event of an unknown type was sent to the receiver. What shall we do?!


<details>
	<summary>See more</summary>
	
	processUnknownEvent: aMorphicEvent localPosition: localEventPosition
	"An event of an unknown type was sent to the receiver. What shall we do?!"

	Smalltalk beep. 
	aMorphicEvent printString displayAt: `0@0`.
	aMorphicEvent wasHandled: true
</details>

#### Morph>>#windowEvent: anEvent

Host window event


<details>
	<summary>See more</summary>
	
	windowEvent: anEvent
	"Host window event"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'windowEvent:' 
		ifPresentDo: [ :handler | handler value: anEvent ]
</details>

#### Morph>>#processDropFiles: aDropFilesEvent localPosition: localEventPosition

Handle a dropping file.


<details>
	<summary>See more</summary>
	
	processDropFiles: aDropFilesEvent localPosition: localEventPosition
	"Handle a dropping file."

	aDropFilesEvent wasHandled ifTrue: [ ^self ]. 
	
	aDropFilesEvent wasHandled: true.
	self dropFiles: aDropFilesEvent
</details>

#### Morph>>#hasMouseFocus

<details>
	<summary>See more</summary>
	
	hasMouseFocus

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h mouseFocus == self ]].
	^ false
</details>

#### Morph>>#addCustomHaloMenuItems: aMenu hand: aHandMorph

Add morph-specific items to the given menu which was invoked by the given hand from the halo. To get started, we defer to the counterpart method used with the option-menu, but in time we can have separate menu choices for halo-menus and for option-menus


<details>
	<summary>See more</summary>
	
	addCustomHaloMenuItems: aMenu hand: aHandMorph
	"Add morph-specific items to the given menu which was invoked by the given hand from the halo.  To get started, we defer to the counterpart method used with the option-menu, but in time we can have separate menu choices for halo-menus and for option-menus"

	self addCustomMenuItems: aMenu hand: aHandMorph
</details>

#### Morph>>#storeDataOn: aDataStream

Let all Morphs be written out. All owners are weak references. They only go out if the owner is in the tree being written.


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."
	| cntInstVars cntIndexedVars ti localInstVars |

	"block my owner unless he is written out by someone else"
	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	localInstVars _ Morph instVarNames.
	ti _ 1.  
	((localInstVars at: ti) = 'owner') & (Morph superclass == Object) ifFalse:
			[self error: 'this method is out of date'].
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: ti-1 do:
		[:i | aDataStream nextPut: (self instVarAt: i)].
	aDataStream nextPutWeak: owner.	"owner only written if in our tree"
	ti+1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].
	1 to: cntIndexedVars do:
		[:i | aDataStream nextPut: (self basicAt: i)]
</details>

#### Morph>>#root

Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph.


<details>
	<summary>See more</summary>
	
	root
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [ owner isWorldMorph or: [ owner is: #HandMorph ] ]) ifTrue: [ ^self ].
	^owner root
</details>

#### Morph>>#goBehind

<details>
	<summary>See more</summary>
	
	goBehind

	owner addMorphBack: self.

</details>

#### Morph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 1 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Up:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
</details>

#### Morph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"

	owner _ nil.
	submorphs _ #().
	location _ MorphicTranslation new.
	layoutNeeded _ false
</details>

#### Morph>>#shouldGetStepsFrom: aWorld

<details>
	<summary>See more</summary>
	
	shouldGetStepsFrom: aWorld

	^self world == aWorld
</details>

#### Morph>>#wantsRecolorHandle

Answer whether the receiver would like a recoloring halo handle to be put up. Since this handle also presently affords access to the property-sheet, it is presently always allowed, even though SketchMorphs don't like regular recoloring


<details>
	<summary>See more</summary>
	
	wantsRecolorHandle
	"Answer whether the receiver would like a recoloring halo handle to be put up.  Since this handle also presently affords access to the property-sheet, it is presently always allowed, even though SketchMorphs don't like regular recoloring"

	^ true
	

</details>

#### Morph>>#morphFullBoundsInWorld

Morphs should know nothing about absolute coordinates...


<details>
	<summary>See more</summary>
	
	morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas displayFullBoundsInWorldOf: self ]
</details>

#### Morph>>#isKnownFailing

<details>
	<summary>See more</summary>
	
	isKnownFailing
	| w |
	w _ self world.
	^w notNil and: [ w isKnownFailing: self ]
</details>

#### Morph>>#activateWindowAndSendTopToBack: aBoolean

<details>
	<summary>See more</summary>
	
	activateWindowAndSendTopToBack: aBoolean

	self owningWindow ifNotNil: [ :w |
		w isTopWindow ifFalse: [
			w activateAndSendTopToBack: aBoolean]]
</details>

#### Morph>>#morphBoundsInWorld: newBounds

remove senders and implementors


<details>
	<summary>See more</summary>
	
	morphBoundsInWorld: newBounds
	| oldExtent newExtent |

	"remove senders and implementors"
	self flag: #jmvVer2.

	oldExtent _ self morphExtentInWorld.
	newExtent _ newBounds extent.
	"Moving stuff around is most likely the most common operation.
	Optimize it"
	oldExtent = newExtent ifTrue: [
		^self morphPositionInWorld: newBounds topLeft ].
	(oldExtent dotProduct: oldExtent) <= (newExtent dotProduct: newExtent) ifTrue:[
		"We're growing. First move then resize."
		self morphPositionInWorld: newBounds topLeft; morphExtent: newExtent.
	] ifFalse: [
		"We're shrinking. First resize then move."
		self morphExtent: newExtent; morphPositionInWorld: newBounds topLeft.
	].
</details>

#### Morph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME. Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^false
</details>

#### Morph>>#unclippedSubmorphsReverseDo: aBlock

<details>
	<summary>See more</summary>
	
	unclippedSubmorphsReverseDo: aBlock
	| lastClippedIndex |
	lastClippedIndex _ submorphs size.
	self clipsLastSubmorph ifTrue: [
		lastClippedIndex _ lastClippedIndex - 1 ].
	lastClippedIndex to: 1 by: -1 do: [ :index |
		aBlock value: (submorphs at: index) ]
</details>

#### Morph>>#morphPosition

Answer our position inside our owner, in owner's coordinates.


<details>
	<summary>See more</summary>
	
	morphPosition
	"Answer our position inside our owner, in owner's coordinates."

	^ location translation
</details>

#### Morph>>#stopStepping

Stop getting sent the 'step' message.


<details>
	<summary>See more</summary>
	
	stopStepping
	"Stop getting sent the 'step' message."

	self world ifNotNil: [ :w |
		w stopSteppingMorph: self ]
</details>

#### Morph>>#adoptWidgetsColor: paneColor

<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: paneColor
	self submorphsDo: [ :m| m adoptWidgetsColor: paneColor]
</details>

#### Morph>>#taskbar

<details>
	<summary>See more</summary>
	
	taskbar
	^self world ifNotNil: [ :w | w taskbar ]
</details>

#### Morph>>#processKeyDown: aKeyboardEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processKeyDown: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
	"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyDown: aKeyboardEvent
</details>

#### Morph>>#addAddHandMenuItemsForHalo: aMenu hand: aHandMorph

The former charter of this method was to add halo menu items that pertained specifically to the hand. Over time this charter has withered, and most morphs reimplement this method simply to add their morph-specific menu items. So in the latest round, all other implementors in the standard image have been removed. However, this is left here as a hook for the benefit of existing code in client uses.


<details>
	<summary>See more</summary>
	
	addAddHandMenuItemsForHalo: aMenu hand: aHandMorph
	"The former charter of this method was to add halo menu items that pertained specifically to the hand.  Over time this charter has withered, and most morphs reimplement this method simply to add their morph-specific menu items.  So in the latest round, all other implementors in the standard image have been removed.  However, this is left here as a hook for the benefit of existing code in client uses."


</details>

#### Morph>>#previewing: aBoolean

<details>
	<summary>See more</summary>
	
	previewing: aBoolean

	self setProperty: #previewing toValue: aBoolean
</details>

#### Morph>>#name

Answer the value of morphName


<details>
	<summary>See more</summary>
	
	name
	"Answer the value of morphName"
	^ self valueOfProperty: #morphName
</details>

#### Morph>>#findSubmorphBinary: aBlock

Use binary search for finding a specific submorph of the receiver. Caller must be certain that the ordering holds for the submorphs.


<details>
	<summary>See more</summary>
	
	findSubmorphBinary: aBlock
	"Use binary search for finding a specific submorph of the receiver. Caller must be certain that the ordering holds for the submorphs."
	^submorphs findBinary: aBlock do: [ :found | found ] ifNone: [ :a :b | ]
</details>

#### Morph>>#label

Answer the name to show in a list of windows-and-morphs to represent the receiver


<details>
	<summary>See more</summary>
	
	label
	"Answer the name to show in a list of windows-and-morphs to represent the receiver"

	^ self class name
</details>

#### Morph>>#click: aMouseButtonEvent localPosition: localEventPosition

Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self 
		valueOfProperty: #'click:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]
</details>

#### Morph>>#isOpaqueMorph

Just answer false in the general case, to simplify submorphs. See the implementation and comment in BorderedMorph. and see also senders. If the answer is true, there is an optimization in world draw


<details>
	<summary>See more</summary>
	
	isOpaqueMorph
	"Just answer false in the general case, to simplify submorphs.
	See the implementation and comment in BorderedMorph. and see also senders.
	If the answer is true, there is an optimization in world draw"
	^false
</details>

#### Morph>>#unlockOneSubpart

<details>
	<summary>See more</summary>
	
	unlockOneSubpart
	| unlockables aMenu reply |
	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size <= 1 ifTrue: [^ self unlockContents].
	aMenu _ SelectionMenu labelList: (unlockables collect: [:m | m printStringLimitedTo: 40]) selections: unlockables.
	reply _ aMenu startUpWithCaption: 'Who should be be unlocked?'.
	reply ifNil: [^ self].
	reply unlock
</details>

#### Morph>>#redrawNeeded

Report that the area occupied by this morph should be redrawn.


<details>
	<summary>See more</summary>
	
	redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Include submorphs if we don't clip!
	Think about it. We don't to know about a specific display rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"

	self morphBoundsInWorld ifNotNil: [ :r |
		self invalidateDisplayRect: r from: nil.
		"Expensive in many cases..."
		self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]]
</details>

#### Morph>>#morphExtentInWorld: newExtent

world coordinates Ignored by morphs that are not resizeable


<details>
	<summary>See more</summary>
	
	morphExtentInWorld: newExtent
	"world coordinates
	Ignored by morphs that are not resizeable"
</details>

#### Morph>>#visibleBeforePreview: aBoolean

<details>
	<summary>See more</summary>
	
	visibleBeforePreview: aBoolean

	self setProperty: #visibleBeforePreview toValue: self visible
</details>

#### Morph>>#morphWidth

Ensure everybody wants owner's coordinates!


<details>
	<summary>See more</summary>
	
	morphWidth

"Ensure everybody wants owner's coordinates!"
	self flag: #jmvVer2.
	^ self morphExtent x
</details>

#### Morph>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseDown: We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseMove:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseMoveEvent value: localEventPosition ]
</details>

#### Morph>>#processMouseDown: aMouseButtonEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		^self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ]
</details>

#### Morph>>#allMorphsDo: aBlock

Evaluate the given block for all morphs in this composite morph (including the receiver).


<details>
	<summary>See more</summary>
	
	allMorphsDo: aBlock 
	"Evaluate the given block for all morphs in this composite morph (including the receiver)."

	submorphs do: [:m | m allMorphsDo: aBlock].
	aBlock value: self
</details>

#### Morph>>#nextMorph

Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world). Do a DFS (Depth First Seach) over the morphs tree


<details>
	<summary>See more</summary>
	
	nextMorph
	"Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world).
	Do a DFS (Depth First Seach) over the morphs tree"
	
	submorphs isEmpty
		ifFalse: [ ^submorphs first ].

	^self nextMorphPart2
</details>

#### Morph>>#location

<details>
	<summary>See more</summary>
	
	location
	^location
</details>

#### Morph>>#color

<details>
	<summary>See more</summary>
	
	color

	^ `Color blue`
</details>

#### Morph>>#handlesMouseStillDown: evt

Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp


<details>
	<summary>See more</summary>
	
	handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown: messages between #mouseDown: and #mouseUp"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseStillDown:'
</details>

#### Morph>>#duplicate

Make and return a duplicate of the receiver


<details>
	<summary>See more</summary>
	
	duplicate
	"Make and return a duplicate of the receiver"

	| newMorph |
	self okayToDuplicate ifFalse: [^ self].
	newMorph _ self copy.

	newMorph wantsSteps ifTrue: [ newMorph startStepping ].

	^ newMorph
</details>

#### Morph>>#privateAddMorph: aMorph atIndex: index position: aPoint

<details>
	<summary>See more</summary>
	
	privateAddMorph: aMorph atIndex: index position: aPoint

	| oldIndex myWorld itsWorld oldOwner |
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue: [
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
		aMorph privatePosition: aPoint.
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		aMorph privatePosition: aPoint.
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]
</details>

#### Morph>>#addHalo

Invoke a halo programatically (e.g., not from a meta gesture)


<details>
	<summary>See more</summary>
	
	addHalo
	"Invoke a halo programatically (e.g., not from a meta gesture)"
	^self addHalo: nil
</details>

#### Morph>>#openInHand

Attach the receiver to the current hand in the current morphic world


<details>
	<summary>See more</summary>
	
	openInHand
	"Attach the receiver to the current hand in the current morphic world"

	self runningWorld activeHand attachMorph: self
</details>

#### Morph>>#noHelpString

<details>
	<summary>See more</summary>
	
	noHelpString
	^ 'Help not yet supplied'
</details>

#### Morph>>#processWindowEvent: aWindowEvent localPosition: localEventPosition

Handle an event concerning our host window


<details>
	<summary>See more</summary>
	
	processWindowEvent: aWindowEvent localPosition: localEventPosition
	"Handle an event concerning our host window"

	aWindowEvent wasHandled ifTrue: [^self]. "not interested"
	(self wantsWindowEvent: aWindowEvent) ifFalse: [^self].
	aWindowEvent wasHandled: true.
	self windowEvent: aWindowEvent.

</details>

#### Morph>>#hasModel

<details>
	<summary>See more</summary>
	
	hasModel
	^false
</details>

#### Morph>>#previousMorph

Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world), in 'backward' direction. Do a DFS (Depth First Seach) over the morphs tree


<details>
	<summary>See more</summary>
	
	previousMorph
	"Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world),
	in 'backward'  direction.
	Do a DFS (Depth First Seach) over the morphs tree"
	
	| prev |
	owner ifNotNil: [
		prev _ owner submorphInFrontOf: self.
		prev ifNotNil: [ ^prev veryLastLeaf ].
		^owner ].
	^self veryLastLeaf
</details>

#### Morph>>#show

Make sure this morph is on-stage.


<details>
	<summary>See more</summary>
	
	show
	"Make sure this morph is on-stage."
	self visible: true
</details>

#### Morph>>#addMorphBack: aMorph

Usually prefer alternatives specifying also position: Besides, it is usually better to set aMorph extent or any other attributes before adding it to some owner. All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware.


<details>
	<summary>See more</summary>
	
	addMorphBack: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph atIndex: submorphs size+1
</details>

#### Morph>>#comeToFrontAndAddHalo

<details>
	<summary>See more</summary>
	
	comeToFrontAndAddHalo
	self show.
	self comeToFront.
	self addHalo
</details>

#### Morph>>#mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition

Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph.


<details>
	<summary>See more</summary>
	
	mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Special gestures (cmd-mouse on the Macintosh; Alt-mouse on Windows and Unix) allow a mouse-sensitive morph to be moved or bring up a halo for the morph."
	| h doNotDrag |
	h _ aMouseButtonEvent hand halo.
	"Prevent wrap around halo transfers originating from throwing the event back in"
	doNotDrag _ false.
	h ifNotNil:[
		(h target == self) ifTrue: [ doNotDrag _ true].
		(h target hasOwner: self) ifTrue: [ doNotDrag _ true].
		(self hasOwner: h target) ifTrue: [ doNotDrag _ true]].

	"cmd-drag on flexed morphs works better this way"
	h _ self addHalo: aMouseButtonEvent.
	doNotDrag ifTrue: [ ^self ].
	"Initiate drag transition if requested"
	"good gesture. implement it"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: h
		event: aMouseButtonEvent
		clkSel: nil
		dblClkSel: nil.
	"Pass focus explicitly here"
	aMouseButtonEvent hand newMouseFocus: h.
</details>

#### Morph>>#rotateBy: radians

Change the scale of this morph. Argument is an angle.


<details>
	<summary>See more</summary>
	
	rotateBy: radians
	"Change the scale of this morph. Argument is an angle."
	self redrawNeeded.
	location _ location rotatedBy: radians.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### Morph>>#nextMorphThat: aBlock

Keep looking for some morph that satisfies aBlock. Answer nil if none


<details>
	<summary>See more</summary>
	
	nextMorphThat: aBlock
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate _ self.
	[
		candidate _ candidate nextMorph.
		satisfies _ aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]
</details>

#### Morph>>#addMorph: aMorph position: aPoint

<details>
	<summary>See more</summary>
	
	addMorph: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint
</details>

#### Morph>>#processMouseUp: aMouseButtonEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		^self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		^self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopStepping: #processMouseStillDown
</details>

#### Morph>>#scaleBy: scaleFactor

Change the scale of this morph. Argument is a factor.


<details>
	<summary>See more</summary>
	
	scaleBy: scaleFactor
	"Change the scale of this morph. Argument is a factor."
	self redrawNeeded.
	location _ location scaledBy: scaleFactor.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### Morph>>#privateRemove: aMorph

Private! Should only be used by methods that maintain the ower/submorph invariant.


<details>
	<summary>See more</summary>
	
	privateRemove: aMorph
	"Private! Should only be used by methods that maintain the ower/submorph invariant."
	submorphs _ submorphs copyWithout: aMorph.
</details>

#### Morph>>#addMorphBack: aMorph position: aPoint

<details>
	<summary>See more</summary>
	
	addMorphBack: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: submorphs size+1 position: aPoint
</details>

#### Morph>>#addHalo: evt from: formerHaloOwner

Transfer a halo from the former halo owner to the receiver


<details>
	<summary>See more</summary>
	
	addHalo: evt from: formerHaloOwner
	"Transfer a halo from the former halo owner to the receiver"
	^self addHalo: evt
</details>

#### Morph>>#resistsRemoval

Answer whether the receiver is marked as resisting removal


<details>
	<summary>See more</summary>
	
	resistsRemoval
	"Answer whether the receiver is marked as resisting removal"

	^ false
</details>

#### Morph>>#previousMorphThat: aBlock

Keep looking for some morph that satisfies aBlock. Answer nil if none


<details>
	<summary>See more</summary>
	
	previousMorphThat: aBlock
	"Keep looking for some morph that satisfies aBlock.
	Answer nil if none"

	| candidate satisfies |
	candidate _ self.
	[
		candidate _ candidate previousMorph.
		satisfies _ aBlock value: candidate.
		satisfies not
			"But stop after a whole turn."
			and: [ candidate ~~ self ] ] whileTrue.

	^satisfies ifTrue: [ candidate ]
</details>

#### Morph>>#beginPreview

<details>
	<summary>See more</summary>
	
	beginPreview

	(self visible and: [self atFront]) ifFalse: [
		self visibleBeforePreview: self visible.
		self morphBehindBeforePreview: (self owner submorphBehind: self).
		self previewing: true.
		self showAndComeToFront. ]
</details>

#### Morph>>#externalizeToWorld: aPoint

aPoint is in own coordinates. Answer is in world coordinates. BUT there is no well defined World!


<details>
	<summary>See more</summary>
	
	externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!"
	| inOwners |
	self flag: #jmvVer2.

	inOwners _ self externalize: aPoint.
	^owner
		ifNotNil: [ owner externalizeToWorld: inOwners ]
		ifNil: [ inOwners ]
</details>

#### Morph>>#resumeAfterDrawError

<details>
	<summary>See more</summary>
	
	resumeAfterDrawError

	self drawingFailsNot.
	self redrawNeeded
</details>

#### Morph>>#processMouseStillDown

Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages


<details>
	<summary>See more</summary>
	
	processMouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	self hasMouseFocus
		ifFalse: [
			^self stopStepping: #processMouseStillDown ].
	self mouseStillDown
</details>

#### Morph>>#removeAlarm: aSelector

Remove the given alarm


<details>
	<summary>See more</summary>
	
	removeAlarm: aSelector
	"Remove the given alarm"
	| scheduler |
	scheduler _ self alarmScheduler.
	scheduler ifNotNil:[scheduler removeAlarm: aSelector for: self].
</details>

#### Morph>>#layoutSpecOrNil

Layout specific. Return the layout spec describing where the receiver should appear in a proportional layout. Answer nil if none!


<details>
	<summary>See more</summary>
	
	layoutSpecOrNil
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout.
	Answer nil if none!"

	^ layoutSpec 
</details>

#### Morph>>#beSticky

make the receiver sticky


<details>
	<summary>See more</summary>
	
	beSticky
	"make the receiver sticky"
	self sticky: true
</details>

#### Morph>>#addMorphFront: aMorph position: aPoint

<details>
	<summary>See more</summary>
	
	addMorphFront: aMorph position: aPoint
	^self privateAddMorph: aMorph atIndex: 1 position: aPoint
</details>

#### Morph>>#invalidateLocalRect: localRectangle

los senders que quieran el rect completo, llamar a #morphBoundsInWorld Los que quieran un pedacito, traducirlo hacia arriba (el canvas no me ayuda) y no tengo la currentTransformation. hacerlo paso a paso


<details>
	<summary>See more</summary>
	
	invalidateLocalRect: localRectangle
"los senders que quieran el rect completo, llamar a #morphBoundsInWorld
Los que quieran un pedacito, traducirlo hacia arriba (el canvas no me ayuda) y no tengo la currentTransformation. hacerlo paso a paso"

	self invalidateDisplayRect: (self externalizeDisplayBounds: localRectangle) from: nil
</details>

#### Morph>>#processMouseEnter: aMouseEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseEnter: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	aMouseEvent isDraggingEvent ifTrue: [
		^self].
	self wantsBalloon ifTrue: [
		aMouseEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEvent) ifTrue: [
		aMouseEvent wasHandled: true.
		self mouseEnter: aMouseEvent ]
</details>

#### Morph>>#morphExtentInWorld

eventually, remove.


<details>
	<summary>See more</summary>
	
	morphExtentInWorld
	"eventually, remove."
	self flag: #jmvVer2.
	^self externalizeDistanceToWorld: self morphExtent
</details>

#### Morph>>#morphBoundsInWorld

Morphs should know nothing about absolute coordinates...


<details>
	<summary>See more</summary>
	
	morphBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas ifNotNil: [ :c | c displayBoundsInWorldOf: self ]]
</details>

#### Morph>>#externalizeDisplayBounds: r

All senders of #displayBoundsOfTransformOf: should be rethought...


<details>
	<summary>See more</summary>
	
	externalizeDisplayBounds: r

	| inOwners |
	"All senders of #displayBoundsOfTransformOf: should be rethought..."
	self flag: #jmvVer2.

	inOwners _ location displayBoundsOfTransformOf: r.
	^owner
		ifNotNil: [ owner externalizeDisplayBounds: inOwners ]
		ifNil: [ inOwners ]
</details>

#### Morph>>#morphExtent: aPoint

In our own coordinates! Ignored by morphs that are not resizeable


<details>
	<summary>See more</summary>
	
	morphExtent: aPoint
	"In our own coordinates!
	Ignored by morphs that are not resizeable"
</details>

#### Morph>>#wantsHalo

<details>
	<summary>See more</summary>
	
	wantsHalo
	^self visible
</details>

#### Morph>>#name: anObject

Set the morphName property


<details>
	<summary>See more</summary>
	
	name: anObject
	"Set the morphName property"
	self setProperty: #morphName toValue: anObject
</details>

#### Morph>>#addAllMorphs: aCollection

<details>
	<summary>See more</summary>
	
	addAllMorphs: aCollection
	^self privateAddAllMorphs: aCollection atIndex: submorphs size
</details>

#### Morph>>#hasProperty: aSymbol

Answer whether the receiver has the property named aSymbol


<details>
	<summary>See more</summary>
	
	hasProperty: aSymbol 
	"Answer whether the receiver has the property named aSymbol"
	properties ifNil: [ ^false ].
	^properties includesKey: aSymbol
</details>

#### Morph>>#comeToFront

<details>
	<summary>See more</summary>
	
	comeToFront

	self atFront ifFalse: [owner addMorphFront: self]
</details>

#### Morph>>#doubleClick: aMouseButtonEvent localPosition: localEventPosition

Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	doubleClick: aMouseButtonEvent localPosition: localEventPosition
	"Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'doubleClick:localPosition:'
		ifPresentDo: [ :handler | handler value:  aMouseButtonEvent value: localEventPosition ]
</details>

#### Morph>>#submorphsReverseDo: aBlock

<details>
	<summary>See more</summary>
	
	submorphsReverseDo: aBlock

	submorphs reverseDo: aBlock.
</details>

#### Morph>>#balloonText

Answer balloon help text or nil, if no help is available. NB: subclasses may override such that they programatically construct the text, for economy's sake, such as model phrases in a Viewer


<details>
	<summary>See more</summary>
	
	balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	^ self valueOfProperty: #balloonText ifAbsent: [ nil ]
</details>

#### Morph>>#addAllMorphs: aCollection after: anotherMorph

<details>
	<summary>See more</summary>
	
	addAllMorphs: aCollection after: anotherMorph
	^self privateAddAllMorphs: aCollection 
			atIndex: (submorphs indexOf: anotherMorph ifAbsent: [submorphs size])
</details>

#### Morph>>#referencePosition: aPoint

a rather ugly way to say #center: . Just for consistency with #referencePosition


<details>
	<summary>See more</summary>
	
	referencePosition: aPoint
	"a rather ugly way to say #center: . Just for consistency with #referencePosition"
	"remove some day"
	self flag: #jmvVer2.
	self morphPositionInWorld: aPoint - (self morphExtentInWorld // 2)
</details>

#### Morph>>#valueOfProperty: aSymbol

Answer the value of the receiver's property named aSymbol. If property is not present, answer nil.


<details>
	<summary>See more</summary>
	
	valueOfProperty: aSymbol
	"Answer the value of the receiver's property named aSymbol.
	If property is not present, answer nil."

	^ properties
		  ifNotNil: [ properties at: aSymbol ifAbsent: nil ]
</details>

#### Morph>>#morphPosition: aPoint

Change the position of this morph. Argument is in owner's coordinates.


<details>
	<summary>See more</summary>
	
	morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."
	(location isTranslation: aPoint) ifTrue: [ "Null change"
		^ self ].
	"Invalidate the rectangle at the old position..."
	self redrawNeeded.
	location _ location withTranslation: aPoint.
	"... and the new position"
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
</details>

#### Morph>>#duplicateMorph: evt

Make and return a duplicate of the receiver's argument


<details>
	<summary>See more</summary>
	
	duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"

	^self duplicate
</details>

#### Morph>>#inspectOwnerChain

<details>
	<summary>See more</summary>
	
	inspectOwnerChain
	self ownerChain inspectWithLabel: 'Owner chain for ', self printString
</details>

#### Morph>>#stopStepping: aSelector

Stop getting sent the given message.


<details>
	<summary>See more</summary>
	
	stopStepping: aSelector
	"Stop getting sent the given message."

	self world ifNotNil: [ :w |
		 w stopStepping: self selector: aSelector ]
</details>

#### Morph>>#allowsSubmorphDrag

Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsSubmorphDrag'
</details>

#### Morph>>#releaseCachedState

Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'.


<details>
	<summary>See more</summary>
	
	releaseCachedState
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'."
</details>

#### Morph>>#owner

Returns the owner of this morph, which may be nil.


<details>
	<summary>See more</summary>
	
	owner
	"Returns the owner of this morph, which may be nil."

	^ owner
</details>

#### Morph>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `1@1`
</details>

#### Morph>>#morphBounds

<details>
	<summary>See more</summary>
	
	morphBounds
	^ self morphPosition extent: self morphExtent
</details>

#### Morph>>#morphPositionInWorld: newPositionInWorld

Change the position of this morph.


<details>
	<summary>See more</summary>
	
	morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (AffineTransformation) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location _ location withTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### Morph>>#allowsMorphDrop

Answer whether we accept dropping morphs. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsMorphDrop'
</details>

#### Morph>>#focusKeyboardFor: aKeyboardEvent

If aKeyboardEvent ctrl-tab or shift-ctrl-tab use it to navigate keyboard focus. Warning: This doesn't work on Windows... the event is not sent


<details>
	<summary>See more</summary>
	
	focusKeyboardFor: aKeyboardEvent

	"If aKeyboardEvent ctrl-tab or shift-ctrl-tab use it to navigate keyboard focus.
	Warning: This doesn't work on Windows... the event is not sent"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent controlKeyPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ]
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ].
			^ true ].
	"On Windows use at least some keystroke to navigate morphs... even shift-Tab that should navigate backwards"
"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent shiftPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent hand keyboardFocusNext.
			^ true ].
"

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false
</details>

#### Morph>>#startSteppingStepTime: stepTime

Start stepping the receiver


<details>
	<summary>See more</summary>
	
	startSteppingStepTime: stepTime
	"Start stepping the receiver"

	self startStepping: #stepAt: in: 0 stepTime: stepTime
</details>

#### Morph>>#isOwnedByWorld

<details>
	<summary>See more</summary>
	
	isOwnedByWorld
	^owner is: #PasteUpMorph
</details>

#### Morph>>#keyUp: anEvent

Handle a key up event. The default response is to do nothing.


<details>
	<summary>See more</summary>
	
	keyUp: anEvent
	"Handle a key up event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyUp:'
		ifPresentDo: [ :handler | handler value: anEvent ]
</details>

#### Morph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseButton messages ?
	- #mouseButton1Down:localPosition:
	- #mouseButton1Up:localPosition:
	- #mouseButton2Down:localPosition:
	- #mouseButton2Up:localPosition:
	- #mouseButton3Down:localPosition:
	- #mouseButton3Up:localPosition:
	- #mouseMove:localPosition:
	- #mouseButton2Activity
	
	NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true.
	
	Implementors could query the argument, and only answer true for (for example) button 2 up only."

	"Use a property test to allow individual instances to dynamically specify this."
	^ self hasProperty: #'handlesMouseDown:'
</details>

#### Morph>>#addCustomMenuItems: aCustomMenu hand: aHandMorph

Add morph-specific items to the given menu which was invoked by the given hand. This method provides is invoked both from the halo-menu and from the control-menu regimes.


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: aCustomMenu hand: aHandMorph
	"Add morph-specific items to the given menu which was invoked by the given hand.  This method provides is invoked both from the halo-menu and from the control-menu regimes."

</details>

#### Morph>>#submorphsSatisfying: aBlock

<details>
	<summary>See more</summary>
	
	submorphsSatisfying: aBlock
	^ submorphs select: [:m | (aBlock value: m) == true]
</details>

#### Morph>>#mouseHover: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled. We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseHover: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event.
	This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled.
	We can query aMouseMoveEvent to know about pressed mouse buttons."
	"Allow instances to dynamically use properties for handling common events."
	self
		valueOfProperty: #mouseHover:localPosition:
		ifPresentDo: [ :handler |
			handler
				value: aMouseMoveEvent
				value: localEventPosition ].
</details>

#### Morph>>#submorphsBehind: aMorph do: aBlock

<details>
	<summary>See more</summary>
	
	submorphsBehind: aMorph do: aBlock
	| behind |
	behind _ false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind _ true]
						ifFalse: [behind ifTrue: [aBlock value: m]]].

</details>

#### Morph>>#mouseScroll: aMouseScrollEvent localPosition: localEventPosition

Handle a mouse scroll event. This message will only be sent to Morphs that answer true to #handlesMouseScroll: We can query aMouseScrollEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseScroll: aMouseScrollEvent localPosition: localEventPosition
	"Handle a mouse scroll event.
	This message will only be sent to Morphs that answer true to #handlesMouseScroll:
	We can query aMouseScrollEvent to know about pressed mouse buttons."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseScroll:localPosition: 
		ifPresentDo: [ :handler | handler value: aMouseScrollEvent value: localEventPosition ]
</details>

#### Morph>>#morphPositionInWorld

<details>
	<summary>See more</summary>
	
	morphPositionInWorld

	self flag: #jmvVer2.
	"Most likely we don't want to use global coordinates...
	In fact, we could be in many frames of reference at the same time...
	This method makes no sense at all!"

	^self externalizeToWorld: self morphTopLeft 
</details>

#### Morph>>#addMorphFrontFromWorldPosition: aMorph

<details>
	<summary>See more</summary>
	
	addMorphFrontFromWorldPosition: aMorph

	| positionInWorld |
	self flag: #jmv. 	"Not really pretty..."
	positionInWorld _ aMorph morphPositionInWorld.
	self addMorphFront: aMorph.
	aMorph morphPositionInWorld: positionInWorld
</details>

#### Morph>>#submorphCount

<details>
	<summary>See more</summary>
	
	submorphCount

	^ submorphs size
</details>

#### Morph>>#submorphsDo: aBlock

<details>
	<summary>See more</summary>
	
	submorphsDo: aBlock

	submorphs do: aBlock.
</details>

#### Morph>>#exportAsJPEG

Export the receiver's image as a JPEG


<details>
	<summary>See more</summary>
	
	exportAsJPEG
	"Export the receiver's image as a JPEG"

	| fName |
	fName _ FillInTheBlankMorph request: 'Please enter the name' initialAnswer: (self printStringLimitedTo: 20),'.jpeg'.
	fName isEmpty ifTrue: [^ self].
	(self imageForm: 32) writeJPEGfileNamed: fName
</details>

#### Morph>>#changeColor

Change the color of the receiver -- triggered, e.g. from a menu


<details>
	<summary>See more</summary>
	
	changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #color:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'
</details>

#### Morph>>#clippedSubmorph

<details>
	<summary>See more</summary>
	
	clippedSubmorph
	| i |
	^(self clipsLastSubmorph and: [
		i _ submorphs size.
		i ~= 0]) ifTrue: [
			submorphs at: i ]
</details>

#### Morph>>#fullContainsPoint: aPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	fullContainsPoint: aPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	(self morphContainsPoint: aPoint) ifTrue: [ ^ true ].  "quick acceptance"
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m fullContainsPoint: (m internalize: aPoint)) ifTrue: [ ^ true ]].
	^ false
</details>

#### Morph>>#containsPoint: aLocalPoint event: anEvent

Return true if aPoint is considered to be inside the receiver for the given event. The default implementation treats locked children as integral part of their owners.


<details>
	<summary>See more</summary>
	
	containsPoint: aLocalPoint event: anEvent
	"Return true if aPoint is considered to be inside the receiver for the given event.
	The default implementation treats locked children as integral part of their owners."
	
	"Should this method be called #fullContainsPoint:event: ?
	Should it be merged with #fullContainsPoint: ?
	"
	self visible ifFalse: [ ^false ].
	(self morphContainsPoint: aLocalPoint) ifTrue: [ ^true ].
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m isLocked and: [ m fullContainsPoint: (m internalize: aLocalPoint) ])
			ifTrue: [ ^true ]].
	^false
</details>

#### Morph>>#mouseDownOnHelpHandle: anEvent

The mouse went down in the show-balloon handle


<details>
	<summary>See more</summary>
	
	mouseDownOnHelpHandle: anEvent
	"The mouse went down in the show-balloon handle"
	
	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str _ self balloonText.
	str ifNil: [str _ self noHelpString].
	self showBalloon: str hand: anEvent hand.

</details>

#### Morph>>#externalizeDistanceToWorld: aPoint

aPoint is in own coordinates. Answer is in world coordinates. BUT there is no well defined World!


<details>
	<summary>See more</summary>
	
	externalizeDistanceToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!"
	| inOwners |
	self flag: #jmvVer2.

	inOwners _ self externalizeDistance: aPoint.
	^owner
		ifNotNil: [ owner externalizeDistanceToWorld: inOwners ]
		ifNil: [ inOwners ]
</details>

#### Morph>>#lock

<details>
	<summary>See more</summary>
	
	lock
	self lock: true
</details>

#### Morph>>#addCopyItemsTo: aMenu

Add copy-like items to the halo menu


<details>
	<summary>See more</summary>
	
	addCopyItemsTo: aMenu 
	"Add copy-like items to the halo menu"

	aMenu add: 'copy to clipboard (c)' action: #copyToClipboard:
</details>

#### Morph>>#showBalloon: msgString hand: aHand

Pop up a balloon containing the given string, first removing any existing BalloonMorphs in the world.


<details>
	<summary>See more</summary>
	
	showBalloon: msgString hand: aHand
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w balloon h |
	(w _ self world) ifNil: [^ self].
	h _ aHand.
	h ifNil:[
		h _ w activeHand].
	balloon _ HoverHelpMorph contents: msgString.
	
	"Do it in a while. In some cases, processing the event that might have triggered us might also remove any Help Balloon"
	UISupervisor whenUIinSafeState: [
		balloon popUpForHand: h ]
</details>

#### Morph>>#processMouseScroll: aMouseEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	processMouseScroll: aMouseEvent localPosition: localEventPosition
	((self handlesMouseScroll: aMouseEvent) and: [ aMouseEvent wasHandled not ]) ifTrue: [
		self
			mouseScroll: aMouseEvent
			localPosition: localEventPosition.
		aMouseEvent wasHandled: true ].
</details>

#### Morph>>#layoutSpec

Layout specific. Return the layout spec describing where the receiver should appear in a proportional layout


<details>
	<summary>See more</summary>
	
	layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec := LayoutSpec keepMorphExtent.
	layoutSpec morph: self.

	^ layoutSpec 
</details>

#### Morph>>#resumeAfterStepError

Resume stepping after an error has occured.


<details>
	<summary>See more</summary>
	
	resumeAfterStepError
	"Resume stepping after an error has occured."
	self startStepping. "Will #step"
	self removeProperty:#errorOnStep. "Will remove prop only if #step was okay"

</details>

#### Morph>>#ownerChain

Answer a list of objects representing the receiver and all of its owners. The first element is the receiver, and the last one is typically the world in which the receiver resides


<details>
	<summary>See more</summary>
	
	ownerChain
	"Answer a list of objects representing the receiver and all of its owners.   The first element is the receiver, and the last one is typically the world in which the receiver resides"

	| c next |
	c := OrderedCollection with: self.
	next := self.
	[(next := next owner) notNil] whileTrue: [c add: next].
	^c asArray
</details>

#### Morph>>#halo

<details>
	<summary>See more</summary>
	
	halo

	self world ifNotNil: [ :w |
		w haloMorphs do: [ :h |
			h target == self ifTrue: [^ h]]].
	^ nil
</details>

#### Morph>>#allowsFilesDrop

Answer whether we accept dropping files. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsFilesDrop
	"Answer whether we accept dropping files. By default answer false."

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'allowsFilesDrop'
</details>

#### Morph>>#hasSubmorphs

<details>
	<summary>See more</summary>
	
	hasSubmorphs
	^submorphs size ~= 0
</details>

#### Morph>>#removedMorph: aMorph

Notify the receiver that aMorph was just removed from its children


<details>
	<summary>See more</summary>
	
	removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"

</details>

#### Morph>>#keyboardFocusChange: aBoolean

The message is sent to a morph when its keyboard focus change. The given argument indicates that the receiver is gaining keyboard focus (versus losing) the keyboard focus. Morphs that accept keystrokes should change their appearance in some way when they are the current keyboard focus. This default implementation does nothing. Only morphs that answer true to #handlesKeyboard will ever get keyboard focus!


<details>
	<summary>See more</summary>
	
	keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus change. The given argument indicates that the receiver is gaining keyboard focus (versus losing) the keyboard focus. Morphs that accept keystrokes should change their appearance in some way when they are the current keyboard focus. This default implementation does nothing.
	
	Only morphs that answer true to #handlesKeyboard will ever get keyboard focus!"
</details>

#### Morph>>#addMorph: newMorph behind: aMorph

Add a morph to the list of submorphs behind the specified morph


<details>
	<summary>See more</summary>
	
	addMorph: newMorph behind: aMorph
	"Add a morph to the list of submorphs behind the specified morph"
	^self privateAddMorph: newMorph atIndex: (submorphs indexOf: aMorph) + 1.

</details>

#### Morph>>#expand

<details>
	<summary>See more</summary>
	
	expand
	
	self show.
	self comeToFront
</details>

#### Morph>>#isInWorld

Return true if this morph is in a world.


<details>
	<summary>See more</summary>
	
	isInWorld
	"Return true if this morph is in a world."

	^self world notNil
</details>

#### Morph>>#valueOfProperty: aSymbol ifAbsent: aBlock

if the receiver possesses a property of the given name, answer its value. If not then evaluate aBlock and answer the result of this block evaluation


<details>
	<summary>See more</summary>
	
	valueOfProperty: aSymbol ifAbsent: aBlock
	"if the receiver possesses a property of the given name, answer
	its value. If not then evaluate aBlock and answer the result of
	this block evaluation"
	^ properties
		ifNil: [ aBlock value ]
		ifNotNil: [ properties at: aSymbol ifAbsent: aBlock ]
</details>

#### Morph>>#firstSubmorph

<details>
	<summary>See more</summary>
	
	firstSubmorph
	^submorphs first
</details>

#### Morph>>#morphId

Non zero. Zero id means no Morph.


<details>
	<summary>See more</summary>
	
	morphId
	"Non zero. Zero id means no Morph."
	id isNil ifTrue: [
		LastMorphId isNil ifTrue: [ LastMorphId _ 0 ].
		LastMorphId _ LastMorphId + 1.
		id _ LastMorphId ].
	^id
</details>

#### Morph>>#processMouseOver: aMouseEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseOver: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	 self hasMouseFocus ifTrue: [
		"Got this directly through #handleFocusEvent: so check explicitly"
		(self containsPoint: localEventPosition event: aMouseEvent) ifFalse: [
			^self ]].
	aMouseEvent hand noticeMouseOver: self event: aMouseEvent.
	"Open question: should any unhandled mouse move events be filtered out? (i.e. should mouseHover:localPosition: be called when a mouse button is pressed but the morph doesn't have mouse button handlers?  Essentially, what are the limits of what is considered 'hovering'?"
	(self handlesMouseHover and: [aMouseEvent wasHandled not]) ifTrue: [
		self
			mouseHover: aMouseEvent
			localPosition: localEventPosition ].
</details>

#### Morph>>#addHalo: evt

<details>
	<summary>See more</summary>
	
	addHalo: evt
	| halo |
	halo _ HaloMorph new.
	halo popUpFor: self event: evt.
	halo morphBoundsInWorld: self worldBoundsForHalo.
	^halo
</details>

#### Morph>>#addOptionalHandlesTo: aHalo box: box

<details>
	<summary>See more</summary>
	
	addOptionalHandlesTo: aHalo box: box
	
</details>

#### Morph>>#addedMorph: aMorph

Notify the receiver that the given morph was just added.


<details>
	<summary>See more</summary>
	
	addedMorph: aMorph
	"Notify the receiver that the given morph was just added."

</details>

#### Morph>>#removeAllMorphsIn: aCollection

greatly speeds up the removal of *lots* of submorphs


<details>
	<summary>See more</summary>
	
	removeAllMorphsIn: aCollection
	"greatly speeds up the removal of *lots* of submorphs"
	| set |
	aCollection isEmpty ifTrue: [ ^self ].
	set _ IdentitySet new: aCollection size * 4 // 3.
	aCollection do: [ :each | each owner == self ifTrue: [ set add: each ]].
	set isEmpty ifTrue: [ ^self ].
	self redrawNeeded.
	set do: [ :m | m privateOwner: nil ].
	submorphs _ submorphs reject: [ :each | set includes: each].
	set do: [ :m | self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged
</details>

#### Morph>>#handlesMouseHover

Do I want to receive unhandled mouseMove events when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseHover
	"Do I want to receive unhandled mouseMove events when the button is up and the hand is empty?  The default response is false."
	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #handlesMouseHover.
</details>

#### Morph>>#layoutSpec: aLayoutSpec

Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout


<details>
	<summary>See more</summary>
	
	layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	aLayoutSpec 
		useMorphWidth;
		useMorphHeight.
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### Morph>>#drawingFailsNot

<details>
	<summary>See more</summary>
	
	drawingFailsNot
	self world removeKnownFailing: self
</details>

#### Morph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged
	"Preferred fonts scale a number of window relations.
	Let morphs which rely on this updte themselves.
	
	Note that the fontPreferenceChanged message is typically
	sent to the current world.  As a PasteUpMorph iinherits from me
	the code below works fine for this."
	
	"I  do nothing myself but my submorphs may."
	
	self submorphsDo: [ :m | m fontPreferenceChanged. ]
</details>

#### Morph>>#deleteBalloon

If I am showing a balloon, delete it.


<details>
	<summary>See more</summary>
	
	deleteBalloon
	"If I am showing a balloon, delete it."
	| w |
	w _ self world ifNil:[^self].
	w deleteBalloonTarget: self.
</details>

#### Morph>>#morphBehindBeforePreview

<details>
	<summary>See more</summary>
	
	morphBehindBeforePreview

	^self valueOfProperty: #morphBehindBeforePreview
</details>

#### Morph>>#morphTopLeft

By default, morphs occupy a rectangle specified by #morphTopLef and #morphExtent


<details>
	<summary>See more</summary>
	
	morphTopLeft
	"By default, morphs occupy a rectangle specified by #morphTopLef and #morphExtent"
	^`0@0`
</details>

#### Morph>>#mouseStillDownThreshold

Return the number of milliseconds after which mouseStillDown should be sent


<details>
	<summary>See more</summary>
	
	mouseStillDownThreshold
	"Return the number of milliseconds after which mouseStillDown should be sent"
	^200
</details>

#### Morph>>#alarmScheduler

Return the scheduler being responsible for triggering alarms


<details>
	<summary>See more</summary>
	
	alarmScheduler
	"Return the scheduler being responsible for triggering alarms"
	^self world
</details>

#### Morph>>#handlesMouseOver: aMorphicEvent

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseOver: aMorphicEvent
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?  The default response is false." 

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesMouseOver:'
</details>

#### Morph>>#openInWorld: aWorld

Add this morph to the requested World.


<details>
	<summary>See more</summary>
	
	openInWorld: aWorld
	"Add this morph to the requested World."
	(location = MorphicTranslation new)
		ifTrue: [ aWorld addMorph: self position: `50@50` ]
		ifFalse: [ aWorld addMorph: self ]
</details>

#### Morph>>#world

<details>
	<summary>See more</summary>
	
	world
	^owner
		ifNotNil: [ owner world ]
</details>

#### Morph>>#addHaloActionsTo: aMenu

Add items to aMenu representing actions requestable via halo


<details>
	<summary>See more</summary>
	
	addHaloActionsTo: aMenu 
	"Add items to aMenu representing actions requestable via halo"

	| subMenu |
	subMenu := MenuMorph new defaultTarget: self.
	subMenu addTitle: (self printStringLimitedTo: 40).
	subMenu addStayUpIcons.
	subMenu addLine.
	(subMenu add: 'delete' action: #dismissViaHalo)
		setBalloonText: 'Delete this object -- warning -- can be destructive!' .
	self maybeAddCollapseItemTo: subMenu.
	(subMenu add: 'grab' action: #openInHand)
		setBalloonText: 'Pick this object up -- warning, since this removes it from its container, it can have adverse effects.' .
	subMenu addLine.
	(subMenu add: 'resize' action: #resizeFromMenu)
		setBalloonText: 'Change the size of this object'.
	(subMenu add: 'duplicate' action: #maybeDuplicateMorph)
		setBalloonText: 'Hand me a copy of this object'.
	(subMenu
		add: 'set color'
		target: self
		action: #changeColor)
			setBalloonText: 'Change the color of this object'.
	(subMenu
		add: 'inspect'
		target: self
		action: #inspect)
			setBalloonText: 'Open an Inspector on this object'.
	aMenu add: 'halo actions...' subMenu: subMenu
</details>

#### Morph>>#rejectDropMorphEvent: dropEvent

The receiver has been rejected, and must be put back somewhere. If the original owner and position are known, use them, else Just keep it in the hand


<details>
	<summary>See more</summary>
	
	rejectDropMorphEvent: dropEvent
	"The receiver has been rejected, and must be put back somewhere.
	 If the original owner and position are known, use them, 
	else Just keep it in the hand"

	((dropEvent formerOwner isNil) or: [ dropEvent formerPosition isNil ])
		ifTrue: [ dropEvent hand grabMorph: self.
		]
		ifFalse: [ dropEvent formerOwner addMorph: self.
				   self morphPositionInWorld: dropEvent formerPosition.
		]
		
</details>

#### Morph>>#internalize: aPoint

aPoint is in owner's coordinates. Answer is in own coordinates.


<details>
	<summary>See more</summary>
	
	internalize: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizePosition: aPoint
</details>

#### Morph>>#acceptDroppingMorph: aMorph event: evt

This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: message. This default implementation just adds the given morph to the receiver.


<details>
	<summary>See more</summary>
	
	acceptDroppingMorph: aMorph event: evt
	"This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: message. This default implementation just adds the given morph to the receiver."
	
	self addMorph: aMorph
</details>

#### Morph>>#mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition

Ignored. Theoretically we should never get here since control is transferred to the halo on #mouseButton3Down: but subclasses may implement this differently.


<details>
	<summary>See more</summary>
	
	mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition
	"Ignored. Theoretically we should never get here since control is transferred to the halo on #mouseButton3Down: but subclasses may implement this differently."
</details>

#### Morph>>#visible: aBoolean

set the 'visible' attribute of the receiver to aBoolean


<details>
	<summary>See more</summary>
	
	visible: aBoolean
	"set the 'visible' attribute of the receiver to aBoolean"

	self visible == aBoolean
		ifTrue: [ ^ self ].
	aBoolean ifFalse: [
		self redrawNeeded ].
	self setProperty: #visible toValue: aBoolean.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	aBoolean ifTrue: [
		self redrawNeeded]
</details>

#### Morph>>#addMorph: aMorph

Usually prefer alternatives specifying also position: Besides, it is usually better to set aMorph extent or any other attributes before adding it to some owner. All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	self addMorphFront: aMorph
</details>

#### Morph>>#internalizeDistanceFromWorld: aPoint

aPoint is a delta in World coordinates. Answer is in own coordinates.


<details>
	<summary>See more</summary>
	
	internalizeDistanceFromWorld: aPoint
	"aPoint is a delta in World coordinates. Answer is in own coordinates."
	| inOwners |
	self flag: #jmvVer2.
	inOwners _ owner
		ifNotNil: [ owner internalizeDistanceFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalizeDistance: inOwners
</details>

#### Morph>>#extentBorder

This is the number of pixels to add to internal minimum to calculate my minimumExtent. I don;t have to do anything here. This is the default for my subclasses


<details>
	<summary>See more</summary>
	
	extentBorder
	"This is the number of pixels to add to internal minimum to calculate
	my minimumExtent.  I don;t have to do anything here. 
	This is the default for my subclasses"
	
	^ 0
	

</details>

#### Morph>>#step

<details>
	<summary>See more</summary>
	
	step
	^ self
</details>

#### Morph>>#buildDebugMenu: aHand

Answer a debugging menu for the receiver. The hand argument is seemingly historical and plays no role presently


<details>
	<summary>See more</summary>
	
	buildDebugMenu: aHand 
	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	self isKnownFailing
		ifTrue: [
			aMenu add: 'start drawing again' action: #resumeAfterDrawError.
			aMenu addLine].
	(self hasProperty: #errorOnStep) 
		ifTrue: [
			aMenu add: 'start stepping again' action: #resumeAfterStepError.
			aMenu addLine].
	aMenu add: 'inspect morph' action: #inspect.
	aMenu add: 'inspect owner chain' action: #inspectOwnerChain.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'inspect model'
				target: self model
				action: #inspect].
	aMenu 
		add: 'explore morph'
		target: self
		action: #explore.
	aMenu 
		add: 'copy to clipboard (c)'
		target: self
		action: #copyToClipboard.
	aMenu addLine.
	aMenu 
		add: 'browse morph class'
		target: self
		action: #browseClassHierarchy.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'browse model class'
				target: self model
				action: #browseClassHierarchy].
	aMenu addLine.
	aMenu
		add: 'edit balloon help' action: #editBalloonHelpText.
	^aMenu
</details>

#### Morph>>#privatePosition: aPoint

Change the position of this morph. Argument is in owner's coordinates.


<details>
	<summary>See more</summary>
	
	privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	location _ location withTranslation: aPoint
</details>

#### Morph>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld _ self world ifNil: [ self runningWorld ].
	"Terminate genie recognition focus"
	"I encountered a case where the hand was nil, so I put in a little 
	protection - raa "
	" This happens when we are in an MVC project and open
	  a morphic window. - BG "
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocus: self;
			releaseMouseFocus: self ]].
	owner ifNotNil:[ self privateDelete].
</details>

#### Morph>>#veryLastLeaf

Answer the last submorph, recursively, i.e. the very last leaf of the morph tree


<details>
	<summary>See more</summary>
	
	veryLastLeaf
	"Answer the last submorph, recursively, i.e. the very last leaf of the morph tree"

	^submorphs size = 0
		ifTrue: [ self ]
		ifFalse: [ submorphs last veryLastLeaf ]
</details>

#### Morph>>#externalize: aPoint

aPoint is in own coordinates. Answer is in owner's coordinates.


<details>
	<summary>See more</summary>
	
	externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	"Must include scale and rotation!"
	self flag: #jmvVer2.
	^ location externalizePosition: aPoint
</details>

#### Morph>>#removeMorph: aMorph

Remove the given morph from my submorphs


<details>
	<summary>See more</summary>
	
	removeMorph: aMorph
	"Remove the given morph from my submorphs"
	| aWorld |
	aMorph owner == self ifFalse:[^self].
	aWorld := self world.
	aWorld ifNotNil: [
		aMorph redrawNeeded ].
	self privateRemove: aMorph.
	aMorph privateOwner: nil.
	self removedMorph: aMorph.
	self someSubmorphPositionOrExtentChanged.
</details>

#### Morph>>#printOn: aStream

Add the identity of the receiver to a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Add the identity of the receiver to a stream"
	aStream isText
		ifTrue: [
			aStream
				withAttribute: (TextAnchor new anchoredFormOrMorph: (owner ifNil: [self] ifNotNil: [self imageForm: 32@32 depth: 32]))
				do: [ aStream nextPut: $* ].
			^ self].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [ :x | aStream nextPutAll: x asString]
</details>

#### Morph>>#keyStroke: aKeyboardEvent

Handle a keystroke event.


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
		
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler | handler value: aKeyboardEvent ]
</details>

#### Morph>>#addMorph: newMorph inFrontOf: aMorph

Add a morph to the list of submorphs in front of the specified morph


<details>
	<summary>See more</summary>
	
	addMorph: newMorph inFrontOf: aMorph
	"Add a morph to the list of submorphs in front of the specified morph"
	^self privateAddMorph: newMorph atIndex: ((submorphs indexOf: aMorph) max: 1).
</details>

#### Morph>>#wantsBalloon

Answer true if receiver wants to show a balloon help text is a few moments.


<details>
	<summary>See more</summary>
	
	wantsBalloon
	"Answer true if receiver wants to show a balloon help text is a few moments."

	^ (self balloonText notNil) and: [Preferences balloonHelpEnabled]
</details>

#### Morph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime
	"Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second."

	^ 1000
</details>

#### Morph>>#removeAllMorphs

<details>
	<summary>See more</summary>
	
	removeAllMorphs
	| oldMorphs |
	submorphs isEmpty ifTrue: [ ^self ].
	self redrawNeeded.
	submorphs do: [ :m |
		m privateOwner: nil ].
	oldMorphs _ submorphs.
	submorphs _ #().
	oldMorphs do: [ :m |
		self removedMorph: m ].
	self someSubmorphPositionOrExtentChanged
</details>

#### Morph>>#dismissViaHalo

The user has clicked in the delete halo-handle. This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example.


<details>
	<summary>See more</summary>
	
	dismissViaHalo
	"The user has clicked in the delete halo-handle.  This provides a hook in case some concomitant action should be taken, or if the particular morph is not one which should be put in the trash can, for example."

	^ self dismissMorph
</details>

#### Morph>>#activateWindow

<details>
	<summary>See more</summary>
	
	activateWindow

	self activateWindowAndSendTopToBack: false
</details>

#### Morph>>#aboutToBeGrabbedBy: aHand

The receiver is being grabbed by a hand. Perform necessary adjustments (if any) and return the actual morph that should be added to the hand. Answer nil to reject the drag.


<details>
	<summary>See more</summary>
	
	aboutToBeGrabbedBy: aHand
	"The receiver is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^self "Grab me"
</details>

#### Morph>>#mouseButton2Activity

This method may be redefined, for example, to open a pop-up menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	"This method may be redefined, for example, to open a pop-up menu"
</details>

#### Morph>>#rotationDegrees

Default implementation.


<details>
	<summary>See more</summary>
	
	rotationDegrees
	"Default implementation."

	^ 0.0

</details>

#### Morph>>#isSticky

Answer whether the receiver is Sticky. A morph that is made sticky can not be easily grabbed with the hand.


<details>
	<summary>See more</summary>
	
	isSticky
	"Answer whether the receiver is Sticky.
	A morph that is made sticky can not be easily grabbed with the hand."

	^ self valueOfProperty: #sticky ifAbsent: [ false ]
</details>

#### Morph>>#morphBounds: aRectangle

<details>
	<summary>See more</summary>
	
	morphBounds: aRectangle
	self morphPosition: aRectangle topLeft.
	self morphExtent: aRectangle extent
</details>

#### Morph>>#processDropMorph: aDropEvent localPosition: localEventPosition

Handle a dropping morph.


<details>
	<summary>See more</summary>
	
	processDropMorph: aDropEvent localPosition: localEventPosition
	"Handle a dropping morph."
	| aMorph |
	
	aDropEvent wasHandled ifTrue: [ ^self ]. "Do it just once, for one drop destination"
	
	aMorph _ aDropEvent contents.
	aDropEvent wasHandled: true.
	self acceptDroppingMorph: aMorph event: aDropEvent.
	aMorph justDroppedInto: self event: aDropEvent
</details>

#### Morph>>#hasKeyboardFocus

<details>
	<summary>See more</summary>
	
	hasKeyboardFocus

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			^ h keyboardFocus == self ]].
	^ false
</details>

#### Morph>>#minimumLayoutExtent

This returns the minimum extent that the morph may be shrunk to, when resizing LayoutMorphs or when adjusting a LayoutAdjustingMorph. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumLayoutExtent
	"This returns the minimum extent that the morph may be shrunk to, when resizing LayoutMorphs or when adjusting a LayoutAdjustingMorph. 
	It is expressed in the morph own coordinates, like morphExtent."

	| minExtent |
	minExtent _ self minimumExtent.
	^ layoutSpec
		ifNil: [ minExtent ]
		ifNotNil: [ minExtent max: layoutSpec minimumLayoutWidth @ layoutSpec minimumLayoutHeight ]
</details>

#### Morph>>#worldBoundsForHalo

Answer the rectangle to be used as the inner dimension of my halos. Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle.


<details>
	<summary>See more</summary>
	
	worldBoundsForHalo
	"Answer the rectangle to be used as the inner dimension of my halos.
	Allow for showing either bounds or fullBounds, and compensate for the optional bounds rectangle."

	^ Preferences haloEnclosesFullBounds
		ifFalse: [ self morphBoundsInWorld ]
		ifTrue: [ self morphFullBoundsInWorld ]
</details>

#### Morph>>#lastSubmorph

<details>
	<summary>See more</summary>
	
	lastSubmorph
	^submorphs last
</details>

#### Morph>>#privateAddAllMorphs: aCollection atIndex: index

Private. Add aCollection of morphs to the receiver


<details>
	<summary>See more</summary>
	
	privateAddAllMorphs: aCollection atIndex: index
	"Private. Add aCollection of morphs to the receiver"
	| myWorld itsWorld otherSubmorphs |
	myWorld _ self world.
	otherSubmorphs _ submorphs copyWithoutAll: aCollection.
	(index between: 0 and: otherSubmorphs size)
		ifFalse: [^ self error: 'index out of range'].
	index = 0
		ifTrue:[	submorphs _ aCollection asArray, otherSubmorphs]
		ifFalse:[	index = otherSubmorphs size
			ifTrue:[	submorphs _ otherSubmorphs, aCollection]
			ifFalse:[	submorphs _ otherSubmorphs copyReplaceFrom: index + 1 to: index with: aCollection ]].
	aCollection do: [:m | | itsOwner |
		itsOwner _ m owner.
		itsOwner ifNotNil: [
			itsWorld _ m world.
			(itsWorld == myWorld) ifFalse: [
				itsWorld ifNotNil: [m redrawNeeded]].
			(itsOwner ~~ self) ifTrue: [
				m owner privateRemove: m.
				m owner removedMorph: m ]].
		m privateOwner: self.
		myWorld ifNotNil: [m redrawNeeded].
		(myWorld == itsWorld) ifFalse: [m intoWorld: myWorld].
		itsOwner == self ifFalse: [
			self addedMorph: m.
			m noteNewOwner: self ].
	].
	self someSubmorphPositionOrExtentChanged
</details>

#### Morph>>#toggleStickiness

togle the receiver's Stickiness


<details>
	<summary>See more</summary>
	
	toggleStickiness
	"togle the receiver's Stickiness"
	self sticky: self isSticky not
</details>

#### Morph>>#editBalloonHelpText

Modify the receiver's balloon help text.


<details>
	<summary>See more</summary>
	
	editBalloonHelpText
	"Modify the receiver's balloon help text."

	self editBalloonHelpContent: self balloonText
</details>

#### Morph>>#addPossiblyUncoveredAreasIn: aRectangle to: aCollection

Answer an array of rectangles encompassing those areas in aRectangle not completely covered by self. All areas that might possibly be uncovered must be included.


<details>
	<summary>See more</summary>
	
	addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."
	(self isOrthoRectangularMorph and: [ self isOpaqueMorph ]) ifTrue: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :r |  aCollection add: r ].
		^self ].
	aCollection add: aRectangle
</details>

#### Morph>>#buildHandleMenu: aHand

Build the morph menu for the given morph's halo's menu handle. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. This method allows the morph to decide which items should be included in the hand's section of the menu.


<details>
	<summary>See more</summary>
	
	buildHandleMenu: aHand
	"Build the morph menu for the given morph's halo's menu handle. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. This method allows the morph to decide which items should be included in the hand's section of the menu."

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu addStayUpIcons.
	menu addLine.
	self addStandardHaloMenuItemsTo: menu hand: aHand.
	menu defaultTarget: aHand.
	self addAddHandMenuItemsForHalo: menu  hand: aHand.
	menu defaultTarget: self.
	self addCustomHaloMenuItems: menu hand: aHand.
	menu defaultTarget: aHand.
	^ menu

</details>

#### Morph>>#externalizeDistance: aPoint

aPoint is in own coordinates. Answer is in owner's coordinates.


<details>
	<summary>See more</summary>
	
	externalizeDistance: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."
	^ location externalizeDelta: aPoint
</details>

#### Morph>>#withAllOwnersDo: aBlock

Evaluate aBlock with the receiver and all of its owners


<details>
	<summary>See more</summary>
	
	withAllOwnersDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock value: self.
	owner ifNotNil: [ owner withAllOwnersDo: aBlock ]
</details>

#### Morph>>#dispatchEvent: aMorphicEvent localPosition: localPosition

This is the central entry for dispatching events in morphic. Given some event, find the right receiver and let him handle it. localPosition is in our coordinates.


<details>
	<summary>See more</summary>
	
	dispatchEvent: aMorphicEvent localPosition: localPosition
	"This is the central entry for dispatching events in morphic. Given some event, find the right receiver and let him handle it.
	localPosition is in our coordinates."

	^ (self rejectsEvent: aMorphicEvent)
		ifTrue: [ #rejected ]
		ifFalse: [ aMorphicEvent dispatchWith: self localPosition: localPosition ]
</details>

#### Morph>>#icon

<details>
	<summary>See more</summary>
	
	icon
	^ (self imageForm: 400@300 depth: 32)
		ifNil: [ Theme current morphsIcon ]
		ifNotNil: [ :form | form icon ]
</details>

#### Morph>>#layoutSubmorphsIfNeeded

Return self. Recompute the layout if necessary.


<details>
	<summary>See more</summary>
	
	layoutSubmorphsIfNeeded
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	layoutNeeded ifTrue: [
		self layoutSubmorphs ].
</details>

#### Morph>>#rotation: radians scale: scale

Change the scale of this morph. Arguments are an angle and a scale.


<details>
	<summary>See more</summary>
	
	rotation: radians scale: scale
	"Change the scale of this morph. Arguments are an angle and a scale."
	self redrawNeeded.
	location _ location withRotation: radians scale: scale.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### Morph>>#morphAlign: aPoint with: anotherPoint

<details>
	<summary>See more</summary>
	
	morphAlign: aPoint with: anotherPoint
	^ self morphPosition: self morphPosition + anotherPoint - aPoint
</details>

#### Morph>>#fullReleaseCachedState

Release the cached state of the receiver and its full submorph tree.


<details>
	<summary>See more</summary>
	
	fullReleaseCachedState
	"Release the cached state of the receiver and its full submorph tree."

	self allMorphsDo: [:m | m releaseCachedState].

</details>

#### Morph>>#whenUIinSafeState: evaluableObject

<details>
	<summary>See more</summary>
	
	whenUIinSafeState: evaluableObject
	self world
		ifNotNil: [ :w | w whenUIinSafeState: evaluableObject ]
		ifNil: evaluableObject
</details>

#### Morph>>#lockUnlockMorph

If the receiver is locked, unlock it; if unlocked, lock it


<details>
	<summary>See more</summary>
	
	lockUnlockMorph
	"If the receiver is locked, unlock it; if unlocked, lock it"

	self isLocked ifTrue: [self unlock] ifFalse: [self lock]
</details>

#### Morph>>#mouseStillDownStepRate

At what rate do I want to receive #mouseStillDown notifications?


<details>
	<summary>See more</summary>
	
	mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?"
	^1
</details>

#### Morph>>#submorphsInFrontOf: aMorph do: aBlock

<details>
	<summary>See more</summary>
	
	submorphsInFrontOf: aMorph do: aBlock
	| behind |
	behind _ false.
	submorphs do:
		[:m | m == aMorph ifTrue: [behind _ true]
						ifFalse: [behind ifFalse: [aBlock value: m]]].

</details>

#### Morph>>#embeddedInMorphicWindowLabeled: labelString

<details>
	<summary>See more</summary>
	
	embeddedInMorphicWindowLabeled: labelString 
	| window |
	window := SystemWindow new.
	window setLabel: labelString.
	window layoutMorph addMorph: self proportionalHeight: 1.
	^window
</details>

#### Morph>>#mouseStillDown

Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages


<details>
	<summary>See more</summary>
	
	mouseStillDown
	"Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages"
	
	self "Allow instances to use this"
		valueOfProperty: #'mouseStillDown
'		ifPresentDo: [ :handler | handler value ]
</details>

#### Morph>>#setBalloonText: stringTextOrSymbol

Set receiver's balloon help text. Pass nil to remove the help.


<details>
	<summary>See more</summary>
	
	setBalloonText: stringTextOrSymbol
	"Set receiver's balloon help text. Pass nil to remove the help."

	stringTextOrSymbol
		ifNil: [ self removeProperty: #balloonText ]
		ifNotNil: [
			self
				setProperty: #balloonText
				toValue: stringTextOrSymbol string ].
</details>

#### Morph>>#copyToClipboard: evt

<details>
	<summary>See more</summary>
	
	copyToClipboard: evt
	self copyToClipboard
</details>

#### Morph>>#exportAsBMP

<details>
	<summary>See more</summary>
	
	exportAsBMP
	| fName |
	fName _ FillInTheBlankMorph request:'Please enter the name' initialAnswer: (self printStringLimitedTo: 20),'.bmp'.
	fName isEmpty ifTrue:[^self].
	(self imageForm: 32) writeBMPfileNamed: fName.
</details>

#### Morph>>#lock: aBoolean

change the receiver's lock property


<details>
	<summary>See more</summary>
	
	lock: aBoolean
	"change the receiver's lock property"

	self setProperty: #locked toValue: aBoolean
</details>

#### Morph>>#removeProperty: aSymbol

removes the property named aSymbol if it exists


<details>
	<summary>See more</summary>
	
	removeProperty: aSymbol
	"removes the property named aSymbol if it exists"

	properties ifNil: [ ^ self ].
	properties
		removeKey: aSymbol
		ifAbsent: nil.
	properties isEmpty ifTrue: [ properties := nil ]
</details>

#### Morph>>#invalidateDisplayRect: damageRect from: aMorph

warning. Senders are using global coordinates. Redesign!


<details>
	<summary>See more</summary>
	
	invalidateDisplayRect: damageRect from: aMorph

	| clippedRect b |

	"warning. Senders are using global coordinates. Redesign!"
	"local now!!!!!"
	self flag: #jmvVer2.	"ok?"

	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	aMorph ifNotNil: [
	 	aMorph == self clippedSubmorph
			ifTrue: [
				b _ self morphBoundsInWorld.
				b ifNil: [ ^self ].
				clippedRect _ damageRect intersect: b ]].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect from: self ]
</details>

#### Morph>>#intoWorld: aWorld

The receiver has just appeared in a new world. Note: * aWorld can be nil (due to optimizations in other places) * owner is already set * owner's submorphs may not include receiver yet. Important: Keep this method fast - it is run whenever morphs are added.


<details>
	<summary>See more</summary>
	
	intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	aWorld ifNil: [ ^self ].
	self wantsSteps ifTrue: [ self startStepping ].
	self submorphsDo: [ :m | m intoWorld: aWorld ]
</details>

#### Morph>>#dismissMorph

<details>
	<summary>See more</summary>
	
	dismissMorph
	| w |
	w _ self world ifNil: [ ^self ].
	w deleteAllHalos; stopSteppingMorph: self.
	self delete
</details>

#### Morph>>#morphHeight

Ensure everybody wants owner's coordinates!


<details>
	<summary>See more</summary>
	
	morphHeight

"Ensure everybody wants owner's coordinates!"
	self flag: #jmvVer2.
	^ self morphExtent y
</details>

#### Morph>>#stickinessString

Answer the string to be shown in a menu to represent the stickiness status


<details>
	<summary>See more</summary>
	
	stickinessString
	"Answer the string to be shown in a menu to represent the  
	stickiness status"
	^ (self isSticky
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'resist being picked up'
</details>

#### Morph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'mouseButton1Down:localPosition:' 
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]
</details>

#### Morph>>#privateOwner: aMorph

Private! Should only be used by methods that maintain the ower/submorph invariant.


<details>
	<summary>See more</summary>
	
	privateOwner: aMorph
	"Private! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner _ owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition _ self morphPositionInWorld ].
	owner _ aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location _ location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!"
					]]
</details>

#### Morph>>#startStepping: aSelector stepTime: stepTime

Start stepping the receiver


<details>
	<summary>See more</summary>
	
	startStepping: aSelector stepTime: stepTime
	"Start stepping the receiver"

	self startStepping: aSelector in: 0 stepTime: stepTime
</details>

#### Morph>>#addEmbeddingMenuItemsTo: aMenu hand: aHandMorph

<details>
	<summary>See more</summary>
	
	addEmbeddingMenuItemsTo: aMenu hand: aHandMorph
	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self potentialEmbeddingTargets reverseDo: [:m | 
		menu 
			add: m class name asString 
			target: m 
			action: #addMorphFrontFromWorldPosition: 
			argumentList: {self}].
	aMenu ifNotNil:[
		menu submorphCount > 0 
			ifTrue:[aMenu add:'embed into' subMenu: menu].
	].
	^menu
</details>

#### Morph>>#handleFocusEvent: aMorphicEvent

Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand.


<details>
	<summary>See more</summary>
	
	handleFocusEvent: aMorphicEvent
	"Handle the given event. This message is sent if the receiver currently has the focus and is therefore receiving events directly from some hand."

	^aMorphicEvent sentTo: self localPosition: (self internalizeFromWorld: aMorphicEvent eventPosition)
</details>

#### Morph>>#maybeAddCollapseItemTo: aMenu

If appropriate, add a collapse item to the given menu


<details>
	<summary>See more</summary>
	
	maybeAddCollapseItemTo: aMenu
	"If appropriate, add a collapse item to the given menu"

	owner ifNotNil: [
		owner isWorldMorph ifTrue: [
			aMenu add: 'collapse' target: self action: #collapse ]]
</details>

#### Morph>>#replaceSubmorph: oldMorph by: newMorph

<details>
	<summary>See more</summary>
	
	replaceSubmorph: oldMorph by: newMorph
	| index |
	oldMorph stopStepping.
	index _ submorphs indexOf: oldMorph.
	oldMorph privateDelete.
	self privateAddMorph: newMorph atIndex: index
</details>

#### Morph>>#objectForDataStream: refStrm

I am being written out on an object file


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm 
	"I am being written out on an object file"
	self prepareToBeSaved.	"Amen"
	^self
</details>

#### Morph>>#addColorMenuItems: aMenu hand: aHand

Add the items for changing the current color of the Morph


<details>
	<summary>See more</summary>
	
	addColorMenuItems: aMenu hand: aHand 
	"Add the items for changing the current color of the Morph"

	aMenu add: 'change color...' action: #changeColor
</details>

#### Morph>>#openInWorld

<details>
	<summary>See more</summary>
	
	openInWorld

	self runningWorld
		ifNil: [ UISupervisor whenUIinSafeState: [ self openInWorld ]]
		ifNotNil: [ :w | self openInWorld: w ]
</details>

#### Morph>>#noteNewOwner: aMorph

I have just been added as a submorph of aMorph


<details>
	<summary>See more</summary>
	
	noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"
</details>

#### Morph>>#disregardUnacceptedEdits

Return true if this view either has no text changes or does not care.


<details>
	<summary>See more</summary>
	
	disregardUnacceptedEdits
	"Return true if this view either has no text changes or does not care."

	submorphs do: [ :m | m disregardUnacceptedEdits ].
	^ true
</details>

#### Morph>>#referencePosition

Return the current reference position of the receiver


<details>
	<summary>See more</summary>
	
	referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	^self morphExtentInWorld // 2 + self morphPositionInWorld
</details>

#### Morph>>#flashWith: aColor

<details>
	<summary>See more</summary>
	
	flashWith: aColor

	self morphBoundsInWorld ifNotNil: [ :r | Display flash: r with: aColor ]
</details>

#### Morph>>#handlesMouseScroll: aMouseScrollEvent

<details>
	<summary>See more</summary>
	
	handlesMouseScroll: aMouseScrollEvent
	^ self hasProperty: #'handlesMouseScroll:'
</details>

#### Morph>>#isCollapsed

<details>
	<summary>See more</summary>
	
	isCollapsed

	^ self visible not
</details>

#### Morph>>#owningWindow

Return the first enclosing morph that is a kind of Window, or nil if none


<details>
	<summary>See more</summary>
	
	owningWindow
	"Return the first enclosing morph that is a kind of Window, or nil if none"

	^ self firstOwnerSuchThat: [ :m | m is: #SystemWindow ]
</details>

#### Morph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: `Color blue`
</details>

#### Morph>>#clipsLastSubmorph

Drawing specific. If this property is set, clip the receiver's last submorph to the receiver's shape


<details>
	<summary>See more</summary>
	
	clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ false
</details>

#### Morph>>#addDebuggingItemsTo: aMenu hand: aHandMorph

<details>
	<summary>See more</summary>
	
	addDebuggingItemsTo: aMenu hand: aHandMorph
	aMenu add: 'debug...' subMenu:  (self buildDebugMenu: aHandMorph)
</details>

#### Morph>>#okayToDuplicate

Formerly this protocol was used to guard against awkward situations when there were anonymous scripts in the etoy system. Nowadays we just always allow duplication


<details>
	<summary>See more</summary>
	
	okayToDuplicate
	"Formerly this protocol was used to guard against awkward situations when there were anonymous scripts in the etoy system.  Nowadays we just always allow duplication"

	^ true
</details>

#### Morph>>#flash

Do nothing.


<details>
	<summary>See more</summary>
	
	flash

	self morphBoundsInWorld ifNotNil: [ :r |
		Display flash: r ]
</details>

#### Morph>>#refreshWorld

<details>
	<summary>See more</summary>
	
	refreshWorld
	| aWorld |
	(aWorld _ self world) ifNotNil: [aWorld displayWorldSafely]

</details>

#### Morph>>#toggleCollapseOrShow

If collapsed, show me. If visible, collapse me.


<details>
	<summary>See more</summary>
	
	toggleCollapseOrShow
	"If collapsed, show me.
	If visible, collapse me."

	(self visible and: [self atFront])
		ifTrue:  [ self collapse ]
		ifFalse: [ self showAndComeToFront ]
</details>

#### Morph>>#visibleBeforePreview

<details>
	<summary>See more</summary>
	
	visibleBeforePreview

	^self valueOfProperty: #visibleBeforePreview
</details>

#### Morph>>#addTitleForHaloMenu: aMenu

<details>
	<summary>See more</summary>
	
	addTitleForHaloMenu: aMenu
	aMenu addTitle: (self printStringLimitedTo: 40)
</details>

#### Morph>>#internalizeDistance: aPoint

aPoint is in owner's coordinates. Answer is in own coordinates.


<details>
	<summary>See more</summary>
	
	internalizeDistance: aPoint
	"aPoint is in owner's coordinates. Answer is in own coordinates."
	^ location internalizeDelta: aPoint
</details>

#### Morph>>#endPreview

<details>
	<summary>See more</summary>
	
	endPreview
	
	self previewing ifTrue: [
		self visible: self visibleBeforePreview.
		self owner addMorph: self inFrontOf: self morphBehindBeforePreview.
		self previewing: false. ]
</details>

#### Morph>>#internalizeFromWorld: aPoint

aPoint is in World coordinates. Answer is in own coordinates.


<details>
	<summary>See more</summary>
	
	internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	| inOwners |
	self flag: #jmvVer2.
	inOwners _ owner
		ifNotNil: [ owner internalizeFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalize: inOwners
</details>

#### Morph>>#findDeepSubmorphThat: block1 ifAbsent: block2

<details>
	<summary>See more</summary>
	
	findDeepSubmorphThat: block1 ifAbsent: block2 
	self
		allMorphsDo: [:m | (block1 value: m)
				== true ifTrue: [^ m]].
	^ block2 value
</details>

#### Morph>>#setProperty: aSymbol toValue: anObject

change the receiver's property named aSymbol to anObject


<details>
	<summary>See more</summary>
	
	setProperty: aSymbol toValue: anObject
	"change the receiver's property named aSymbol to anObject"

	"the properties dictionary never has nil as value.
	Asking for a nil value is the same as removing the property."

	anObject ifNil: [^ self removeProperty: aSymbol].
	properties ifNil: [ properties := IdentityDictionary new ].
	properties at: aSymbol put: anObject
</details>

#### Morph>>#aboutToGrab: submorph

submorph is being grabbed by a hand. Perform necessary adjustments (if any) and return the actual morph that should be added to the hand. Answer nil to reject the drag.


<details>
	<summary>See more</summary>
	
	aboutToGrab: submorph
	"submorph is being grabbed by a hand.
	Perform necessary adjustments (if any) and return the actual morph
	that should be added to the hand.
	Answer nil to reject the drag."

	^submorph "Grab it"
</details>

#### Morph>>#copy

Answer a copy of the Morph. Copy is not in the World or any other owner.


<details>
	<summary>See more</summary>
	
	copy
	"Answer a copy of the Morph.
	Copy is not in the World or any other owner."
	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)
</details>

#### Morph>>#submorphBehind: aMorph

<details>
	<summary>See more</summary>
	
	submorphBehind: aMorph

	self submorphsBehind: aMorph do: [ :m | ^m ].
	^nil
</details>

#### Morph>>#atFront

<details>
	<summary>See more</summary>
	
	atFront
	
	^owner firstSubmorph == self
</details>

#### Morph>>#privateSubmorphs

<details>
	<summary>See more</summary>
	
	privateSubmorphs
	^submorphs
</details>

#### Morph>>#mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 2 down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"
	self mouseButton2Activity
</details>

#### Morph>>#addAlarm: aSelector withArguments: args after: delayTime

Add an alarm (that is an action to be executed once) with the given set of parameters


<details>
	<summary>See more</summary>
	
	addAlarm: aSelector withArguments: args after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"

	self alarmScheduler ifNotNil: [ :scheduler |
		scheduler
			addAlarm: aSelector
			withArguments: args
			for: self
			at: Time localMillisecondClock + delayTime ]
</details>

#### Morph>>#handlesKeyboard

Return true if the receiver wishes to handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard
	"Return true if the receiver wishes to handle keyboard events"

	"Use a property test to allow individual instances to specify this."
	^ self hasProperty: #'handlesKeyboard'
</details>

#### Morph>>#okayToRotateEasily

Answer whether it is appropriate for a rotation handle to be shown for the receiver. This is a hook -- at present nobody declines.


<details>
	<summary>See more</summary>
	
	okayToRotateEasily
	"Answer whether it is appropriate for a rotation handle to be shown for the receiver.  This is a hook -- at present nobody declines."

	^ true
</details>

#### Morph>>#valueOfProperty: aSymbol ifPresentDo: aBlock

If the receiver has a property of the given name, evaluate aBlock on behalf of the value of that property


<details>
	<summary>See more</summary>
	
	valueOfProperty: aSymbol ifPresentDo: aBlock
	"If the receiver has a property of the given name, evaluate
	aBlock on behalf of the value of that property"

	| value |
	properties ifNil: [^ self ].
	value := self valueOfProperty: aSymbol ifAbsent: [^self].
	^aBlock value: value
</details>

#### Morph>>#editBalloonHelpContent: aString

<details>
	<summary>See more</summary>
	
	editBalloonHelpContent: aString
	| reply |
	reply _ FillInTheBlankMorph
		request: 'Edit the balloon help text for ' , (self printStringLimitedTo: 40)
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString]).
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]
</details>

#### Morph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Morph or: [ super is: aSymbol ]
</details>

#### Morph>>#rejectsEvent: anEvent

Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it.


<details>
	<summary>See more</summary>
	
	rejectsEvent: anEvent
	"Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it."

	^ self isLocked or: [ self visible not ]
</details>

#### Morph>>#mouseEnter: evt

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseEnter: 
		ifPresentDo: [ :handler | handler value: evt ]
</details>

#### Morph>>#okayToResizeEasily

Answer whether it is appropriate to have the receiver be easily resized by the user from the halo


<details>
	<summary>See more</summary>
	
	okayToResizeEasily
	"Answer whether it is appropriate to have the receiver be easily resized by the user from the halo"

	^ true

	"This one was too jarring, not that it didn't most of the time do the right  thing but because some of the time it didn't, such as in a holder.  If we pursue this path, the test needs to be airtight, obviously...
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"
</details>

#### Morph>>#minItemWidth

<details>
	<summary>See more</summary>
	
	minItemWidth
	^self morphWidth
</details>

#### Morph>>#layoutSubmorphs

Compute a new layout of submorphs based on the given layout bounds.


<details>
	<summary>See more</summary>
	
	layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."
	"Only specific subclasses do layout. They redefine this method.
	Remember to call super, or set layoutNeeded ivar to false!"

	layoutNeeded _ false
</details>

#### Morph>>#processKeyUp: aKeyboardEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processKeyUp: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue: [^self].
	self handlesKeyboard ifFalse: [^self].
	aKeyboardEvent wasHandled: true.
	^self keyUp: aKeyboardEvent
</details>

#### Morph>>#addHandlesTo: aHaloMorph box: box

Add halo handles to the halo. Apply the halo filter if appropriate


<details>
	<summary>See more</summary>
	
	addHandlesTo: aHaloMorph box: box
	"Add halo handles to the halo.  Apply the halo filter if appropriate"

	Preferences haloSpecifications do: [ :aSpec |
		(self
			wantsHaloHandleWithSelector: aSpec addHandleSelector
			inHalo: aHaloMorph) ifTrue: [
		aHaloMorph
			perform: aSpec addHandleSelector
			with: aSpec ]].
	aHaloMorph target
		addOptionalHandlesTo: aHaloMorph
		box: box
</details>

#### Morph>>#inATwoWayScrollPane

Answer a two-way scroll pane that allows the user to scroll the receiver in either direction. It will have permanent scroll bars unless you take some special action.


<details>
	<summary>See more</summary>
	
	inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."
	"
	(EllipseMorph new morphExtent: 500@270) inATwoWayScrollPane openInHand
	"

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget morphExtent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	^widget
</details>

#### Morph>>#okayToBrownDragEasily

Answer whether it it okay for the receiver to be brown-dragged easily -- i.e. repositioned within its container without extracting it. At present this is just a hook -- nobody declines.


<details>
	<summary>See more</summary>
	
	okayToBrownDragEasily
	"Answer whether it it okay for the receiver to be brown-dragged easily -- i.e. repositioned within its container without extracting it.  At present this is just a hook -- nobody declines."

	^ true



"
	^ (self topRendererOrSelf owner isKindOf: PasteUpMorph) and:
		[self layoutPolicy isNil]"
</details>

#### Morph>>#privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent

<details>
	<summary>See more</summary>
	
	privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent
	| foundHandler |
	foundHandler _ false.
	(self ownerChain allButFirst anySatisfy: [ :anOwner |
		anOwner isWorldMorph not and: [ anOwner handlesMouseScroll: aMouseScrollEvent ]]) ifTrue: [ foundHandler _ true ].
	^ foundHandler.
</details>

#### Morph>>#startStepping: aSelector in: millisecs stepTime: stepTime

Start stepping the receiver


<details>
	<summary>See more</summary>
	
	startStepping: aSelector in: millisecs stepTime: stepTime
	"Start stepping the receiver"

	self world ifNotNil: [ :w |
		w
			startStepping: self
			at: Time localMillisecondClock+millisecs
			selector: aSelector
			stepTime: stepTime.
		"?"
		"self redrawNeeded" ]
</details>

#### Morph>>#canDiscardEdits

Return true if this view either has no text changes or does not care.


<details>
	<summary>See more</summary>
	
	canDiscardEdits
	"Return true if this view either has no text changes or does not care."

	submorphs do: [ :m | m canDiscardEdits ifFalse: [ ^false ]].
	^ true
</details>

#### Morph>>#unlockContents

<details>
	<summary>See more</summary>
	
	unlockContents
	self submorphsDo:
		[:m | m unlock]
</details>

#### Morph>>#copyForClipboard

Some subclasses might need specific behavior...


<details>
	<summary>See more</summary>
	
	copyForClipboard
	"Some subclasses might need specific behavior..."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self copy
</details>

#### Morph>>#isReallyVisible

Answer true only if all the owner chain is visible (i.e. if we are really visible!)


<details>
	<summary>See more</summary>
	
	isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!)"
	^self visible and: [ owner isReallyVisible ]
</details>

#### Morph>>#startStepping: aSelector

Start getting sent the requested message at each Morphic step. See sample code snippet at Morph>>stepAt:


<details>
	<summary>See more</summary>
	
	startStepping: aSelector
	"Start getting sent the requested message at each Morphic step.
	See sample code snippet at Morph>>stepAt:"

	self startStepping: aSelector stepTime: nil
</details>

#### Morph>>#keyDown: aMorphicEvent

Handle a key down event. The default response is to do nothing.


<details>
	<summary>See more</summary>
	
	keyDown: aMorphicEvent
	"Handle a key down event. The default response is to do nothing."
	
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyDown:' 
		ifPresentDo: [ :handler | handler value: aMorphicEvent ]
</details>

#### Morph>>#showAndComeToFront

Make me visible if not, set me on top of all other sibling morphs.


<details>
	<summary>See more</summary>
	
	showAndComeToFront
	"Make me visible if not, set me on top of all other sibling morphs."
	self show; comeToFront
</details>

#### Morph>>#drawingFails

<details>
	<summary>See more</summary>
	
	drawingFails
	self world addKnownFailing: self
</details>

#### Morph>>#endPreviewAndToggleCollapseOrShow

<details>
	<summary>See more</summary>
	
	endPreviewAndToggleCollapseOrShow

	self endPreview.
	self toggleCollapseOrShow.
</details>

#### Morph>>#isLocked

Answer whether the receiver is Locked. The idea is that a locked morph behaves as if it wasn't a separate object, but just part of the #drawOn: method in the owner. Locked morphs receive no events.


<details>
	<summary>See more</summary>
	
	isLocked
	"Answer whether the receiver is Locked.
	The idea is that a locked morph behaves as if it wasn't a separate object,
	but just part of the #drawOn: method in the owner. Locked morphs receive no events."

	^ self valueOfProperty: #locked ifAbsent: [ false ]
</details>

#### Morph>>#allOwnersReverseDo: aBlock

Evaluate aBlock with all owners of the receiver


<details>
	<summary>See more</summary>
	
	allOwnersReverseDo: aBlock
	"Evaluate aBlock with all owners of the receiver"
	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ]
</details>

#### Morph>>#clearId

<details>
	<summary>See more</summary>
	
	clearId
	id _ nil.
</details>

#### Morph>>#isOwnedByHand

<details>
	<summary>See more</summary>
	
	isOwnedByHand
	^owner is: #HandMorph
</details>

#### Morph>>#prepareToBeSaved

Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session.


<details>
	<summary>See more</summary>
	
	prepareToBeSaved
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState
</details>

#### Morph>>#stepAt: millisecondSinceLast

Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The desired time between steps is specified by this morph's answer to the stepTime message. The millisecondSinceLast parameter gives the time elapsed since the previous step.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m _ RectangleLikeMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t _ 0.
	m when: #morphicStep evaluate: [ :delta |
		t _ t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast
</details>

#### Morph>>#startStepping

Start getting sent the 'step' message.


<details>
	<summary>See more</summary>
	
	startStepping
	"Start getting sent the 'step' message."
	"The #stepAt: message, or any message requiring an argument will be 
	called with the current millisecond timer value if no argument value is specified"

	self startStepping: #stepAt:
</details>

#### Morph>>#addExportMenuItems: aMenu hand: aHandMorph

Add export items to the menu


<details>
	<summary>See more</summary>
	
	addExportMenuItems: aMenu hand: aHandMorph
	"Add export items to the menu"

	aMenu ifNotNil: [
		| aSubMenu |
		aSubMenu _ MenuMorph new defaultTarget: self.
		aSubMenu add: 'BMP file' action: #exportAsBMP.
		aSubMenu add: 'JPEG file' action: #exportAsJPEG.
		aMenu add: 'export...' subMenu: aSubMenu]

</details>

#### Morph>>#justGrabbedFrom: formerOwner

The receiver was just grabbed from its former owner and is now attached to the hand.


<details>
	<summary>See more</summary>
	
	justGrabbedFrom: formerOwner
	"The receiver was just grabbed from its former owner and is now attached to the hand."
</details>

#### Morph>>#justDroppedInto: newOwnerMorph event: anEvent

This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph


<details>
	<summary>See more</summary>
	
	justDroppedInto: newOwnerMorph event: anEvent 
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	newOwnerMorph activateWindow
</details>

#### Morph>>#showBalloon: msgString

Pop up a balloon containing the given string, first removing any existing BalloonMorphs in the world.


<details>
	<summary>See more</summary>
	
	showBalloon: msgString
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."
	| w |
	self showBalloon: msgString hand: ((w _ self world) ifNotNil: [ w activeHand ])
</details>

#### Morph>>#privateAddMorph: aMorph atIndex: index

<details>
	<summary>See more</summary>
	
	privateAddMorph: aMorph atIndex: index

	| oldIndex myWorld itsWorld oldOwner |
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld _ self world.
	oldOwner _ aMorph owner.
	(oldOwner == self and: [(oldIndex _ submorphs indexOf: aMorph) > 0]) ifTrue:[
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue:[
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-2 with: submorphs startingAt: oldIndex+1.
			submorphs at: index-1 put: aMorph.
		] ifFalse:[
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do:[:i|
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
	] ifFalse:[
		"adding a new morph"
		oldOwner ifNotNil:[
			itsWorld _ aMorph world.
			itsWorld ifNotNil: [aMorph redrawNeeded].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs _ submorphs copyReplaceFrom: index to: index-1 with: (Array with: aMorph).
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil:[aMorph redrawNeeded].
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ].

</details>

#### Morph>>#sticky: aBoolean

change the receiver's sticky property


<details>
	<summary>See more</summary>
	
	sticky: aBoolean
	"change the receiver's sticky property"

	self setProperty: #sticky toValue: aBoolean
</details>

#### Morph>>#addMorphFront: aMorph

Usually prefer alternatives specifying also position: Besides, it is usually better to set aMorph extent or any other attributes before adding it to some owner. All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware.


<details>
	<summary>See more</summary>
	
	addMorphFront: aMorph
	"Usually prefer alternatives specifying also position:
	Besides, it is usually better to set aMorph extent or any other attributes before adding it
	to some owner.
	All this avoids screen redraws, giving a slightly more responsive UI in slowish hardware."

	^self privateAddMorph: aMorph atIndex: 1
</details>

#### Morph>>#submorphInFrontOf: aMorph

<details>
	<summary>See more</summary>
	
	submorphInFrontOf: aMorph

	^submorphs before: aMorph
</details>

#### Morph>>#withAllOwnersReverseDo: aBlock

Evaluate aBlock with the receiver and all of its owners


<details>
	<summary>See more</summary>
	
	withAllOwnersReverseDo: aBlock
	"Evaluate aBlock with the receiver and all of its owners"
	owner ifNotNil: [ owner withAllOwnersReverseDo: aBlock ].
	aBlock value: self
</details>

#### Morph>>#nextMorphPart2

<details>
	<summary>See more</summary>
	
	nextMorphPart2
	
	| next |
	owner ifNotNil: [
		next _ owner submorphBehind: self.
		next ifNotNil: [ ^ next ].
		
		^ owner nextMorphPart2 ].

	^ self
	
</details>

#### Morph>>#processMouseLeave: anEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseLeave: anEvent localPosition: localEventPosition
	"System level event handling."
	anEvent hand removePendingBalloonFor: self.
	anEvent isDraggingEvent ifTrue: [
		^self].
	(self handlesMouseOver: anEvent) ifTrue: [
		anEvent wasHandled: true.
		self mouseLeave: anEvent ]
</details>

#### Morph>>#morphLocalBounds

<details>
	<summary>See more</summary>
	
	morphLocalBounds

	^self morphTopLeft extent: self morphExtent
</details>

#### Morph>>#unlock

<details>
	<summary>See more</summary>
	
	unlock
	self lock: false
</details>

#### Morph>>#firstOwnerSuchThat: conditionBlock

<details>
	<summary>See more</summary>
	
	firstOwnerSuchThat: conditionBlock

	self allOwnersDo: [:m | (conditionBlock value: m) ifTrue: [^ m]].
	^ nil

</details>

#### Morph>>#submorphsDrawingOutsideReverseDo: aBlock

Might be redefined in subclasses that know that its submorphs are never outside itself


<details>
	<summary>See more</summary>
	
	submorphsDrawingOutsideReverseDo: aBlock
	"Might be redefined in subclasses that know that its submorphs are never outside itself"
	self unclippedSubmorphsReverseDo: aBlock
</details>

#### Morph>>#isWorldMorph

<details>
	<summary>See more</summary>
	
	isWorldMorph

	^ false
</details>

#### Morph>>#rotationDegrees: degrees

<details>
	<summary>See more</summary>
	
	rotationDegrees: degrees
	self flag: #jmvVer2.
	self showBalloon: 'General rotation is currently disabled.'
</details>

#### Morph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^false
</details>

#### Morph>>#morphBehindBeforePreview: aMorph

<details>
	<summary>See more</summary>
	
	morphBehindBeforePreview: aMorph

	self setProperty: #morphBehindBeforePreview toValue: aMorph
</details>

#### Morph>>#imageForm: depth

<details>
	<summary>See more</summary>
	
	imageForm: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: (self morphPosition extent: self morphExtent).
	canvas fullDraw: self.
	^ canvas form
</details>

#### Morph>>#hasOwner: aMorph

Return true if the receiver has aMorph in its owner chain


<details>
	<summary>See more</summary>
	
	hasOwner: aMorph
	"Return true if the receiver has aMorph in its owner chain"
	aMorph ifNil:[^true].
	self allOwnersDo:[:m| m = aMorph ifTrue:[^true]].
	^false
</details>

#### Morph>>#dragEvent: aMouseEvent localPosition: aPoint

<details>
	<summary>See more</summary>
	
	dragEvent: aMouseEvent localPosition: aPoint

	aMouseEvent hand grabMorph: self
</details>

#### Morph>>#wantsDroppedMorph: aMorph event: evt

Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.


<details>
	<summary>See more</summary>
	
	wantsDroppedMorph: aMorph event: evt
	"Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self."

	^true
</details>

#### Morph>>#resizeMorph

<details>
	<summary>See more</summary>
	
	resizeMorph
	| handle |
	handle _ HandleMorph new 
				forEachPointDo: [ :newPoint | self morphExtent: newPoint - self morphPositionInWorld].
	self runningWorld activeHand attachMorph: handle.
	handle startStepping
</details>

#### Morph>>#balloonHelpDelayTime

Return the number of milliseconds before a balloon help should be put up on the receiver. The balloon help will only be put up if the receiver responds to #wantsBalloon by returning true.


<details>
	<summary>See more</summary>
	
	balloonHelpDelayTime
	"Return the number of milliseconds before a balloon help should be put up on the receiver. The balloon help will only be put up if the receiver responds to #wantsBalloon by returning true."
	^800
</details>

#### Morph>>#addAlarm: aSelector with: arg1 after: delayTime

Add an alarm (that is an action to be executed once) with the given set of parameters


<details>
	<summary>See more</summary>
	
	addAlarm: aSelector with: arg1 after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: (Array with: arg1) after: delayTime
</details>

#### Morph>>#morphExtent

In our own coordinates!


<details>
	<summary>See more</summary>
	
	morphExtent
	"In our own coordinates!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^`50 @ 40`
</details>

#### Morph>>#previewing

<details>
	<summary>See more</summary>
	
	previewing

	^(self valueOfProperty: #previewing) = true
</details>

#### Morph>>#addAlarm: aSelector after: delayTime

Add an alarm (that is an action to be executed once) with the given set of parameters


<details>
	<summary>See more</summary>
	
	addAlarm: aSelector after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: nil after: delayTime
</details>

#### Morph>>#wantsToBeDroppedInto: aMorph

Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other.


<details>
	<summary>See more</summary>
	
	wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph. This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out whether they like each other."
	^true
</details>

#### Morph>>#resizeFromMenu

Commence an interaction that will resize the receiver


<details>
	<summary>See more</summary>
	
	resizeFromMenu
	"Commence an interaction that will resize the receiver"

	self resizeMorph
</details>

#### Morph>>#imageForm: extent depth: depth

<details>
	<summary>See more</summary>
	
	imageForm: extent depth: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: (self morphPosition extent: (self morphExtent min: extent)).
	canvas fullDraw: self.
	^ canvas form
</details>

#### Morph>>#potentialEmbeddingTargets

Return the potential targets for embedding the receiver


<details>
	<summary>See more</summary>
	
	potentialEmbeddingTargets
	"Return the potential targets for embedding the receiver"
	| myRect myWorld |
	owner ifNil:[^#()].
	myWorld := owner world ifNil:[^#()].
	myRect := self morphBoundsInWorld.
	^myWorld submorphs select: [ :m |
		m isReallyVisible
		and: [ m isLocked not
			and: [(m morphBoundsInWorld intersects: myRect)
				and: [(m ~= self)
					and: [(m isKindOf: HaloMorph) not]]]]
		]
</details>

#### Morph>>#mouseLeave: evt

Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."
	Preferences focusFollowsMouse
		ifTrue: [evt hand releaseKeyboardFocus: self].
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #mouseLeave: 
		ifPresentDo: [ :handler | handler value: evt ].
</details>

#### Morph>>#removeHalo

<details>
	<summary>See more</summary>
	
	removeHalo
	| h |
	h _ self halo.
	h ifNotNil: [h delete]
</details>

## MouseClickState

MouseClickState is a simple class managing the distinction between clicks, double clicks, and drag operations. It has been factored out of HandMorph due to the many instVars. Instance variables: clickClient <Morph> The client wishing to receive #click:, #dblClick:, or #drag messages clickState <Symbol> The internal state of handling the last event (#firstClickDown, #firstClickUp, #firstClickTimedOut) firstClickDown <MorphicEvent> The #mouseDown event after which the client wished to receive #click: or similar messages clickSelector <Symbol> The selector to use for sending #click: messages dblClickSelector <Symbol> The selector to use for sending #doubleClick: messages tripleClickSelector <Symbol> The selector to use for sending #tripleClick: messages

### Methods
#### MouseClickState>>#lastClickLocalPosition

<details>
	<summary>See more</summary>
	
	lastClickLocalPosition

	^clickClient internalizeFromWorld: lastClickDown eventPosition
</details>

#### MouseClickState>>#didDoubleClickAndHalf

<details>
	<summary>See more</summary>
	
	didDoubleClickAndHalf

	doubleClickAndHalfDone ifFalse: [
		dblClickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: dblClickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickAndHalfDone _ true ]
</details>

#### MouseClickState>>#didTripleClick

<details>
	<summary>See more</summary>
	
	didTripleClick

	tripleClickSelector ifNotNil: [
		clickClient perform: tripleClickSelector with: lastClickDown with: self lastClickLocalPosition]
</details>

#### MouseClickState>>#client: aMorph drag: aDragSelector click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

<details>
	<summary>See more</summary>
	
	client: aMorph drag: aDragSelector click: aClickSelector clickAndHalf: aClickAndHalfSelector dblClick: aDblClickSelector dblClickAndHalf: aDblClickAndHalfSelector tripleClick: aTripleClickSelector event: firstClickEvent sendMouseButton2Activity: aBoolean

	clickClient _ aMorph.
	dragSelector _ aDragSelector.
	clickSelector _ aClickSelector.
	clickAndHalfSelector _ aClickAndHalfSelector.
	dblClickSelector _ aDblClickSelector.
	dblClickAndHalfSelector _ aDblClickAndHalfSelector.
	tripleClickSelector _ aTripleClickSelector.
	sendMouseButton2Activity _ aBoolean.
	buttonDownCount _ 1.
	buttonUpCount _ 0.
	dragDone _ false.
	clickDone _ false.
	clickAndHalfDone _ false.
	doubleClickDone _ false.
	doubleClickAndHalfDone _ false.
	lastClickDown _ firstClickEvent
</details>

#### MouseClickState>>#didClickAndHalf

<details>
	<summary>See more</summary>
	
	didClickAndHalf

	clickAndHalfDone ifFalse: [
		clickAndHalfSelector ifNotNil: [
			"Focus was lost at buttonUp. Set it again."
			lastClickDown hand newMouseFocus: clickClient.
			clickClient perform: clickAndHalfSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickAndHalfDone _ true ]
</details>

#### MouseClickState>>#didClick

<details>
	<summary>See more</summary>
	
	didClick
	clickDone ifFalse: [
		clickSelector ifNotNil: [
			clickClient perform: clickSelector with: lastClickDown with: self lastClickLocalPosition ].
		clickDone _ true ]
</details>

#### MouseClickState>>#didDrag

<details>
	<summary>See more</summary>
	
	didDrag
	dragDone ifFalse: [
		dragSelector ifNotNil: [
			clickClient perform: dragSelector with: lastClickDown with: self lastClickLocalPosition ].
		dragDone _ true ]
</details>

#### MouseClickState>>#handleEvent: aMouseEvent from: aHand

Process the given mouse event to detect a click, double-click, or drag. Return true if the event should be processed by the sender, false if it shouldn't. NOTE: This method heavily relies on getting *all* mouse button events.


<details>
	<summary>See more</summary>
	
	handleEvent: aMouseEvent from: aHand
	"Process the given mouse event to detect a click, double-click, or drag.
	Return true if the event should be processed by the sender, false if it shouldn't.
	NOTE: This method heavily relies on getting *all* mouse button events."

	| timedOut distance |
	timedOut _ (aMouseEvent timeStamp - lastClickDown timeStamp) > self class doubleClickTimeout.
	distance _ (aMouseEvent eventPosition - lastClickDown eventPosition) r.
	"Real action dispatch might be done after the triggering event, for example, because of waiting for timeout.
	So, count the button downs and ups(clicks), to be processed, maybe later, maybe in a mouseMove..."
	aMouseEvent isMouseDown ifTrue: [
		lastClickDown _ aMouseEvent.
		buttonDownCount _ buttonDownCount + 1 ].
	aMouseEvent isMouseUp ifTrue: [
		buttonUpCount _ buttonUpCount + 1 ].

	"Simulate button 2 if timeout during first click (i.e. tap & hold). Useful for opening menus on pen computers."
	(buttonDownCount = 1 and: [ buttonUpCount = 0]) ifTrue: [
		(timedOut and: [ sendMouseButton2Activity and: [ distance = 0]]) ifTrue: [
			aHand dontWaitForMoreClicks.
			clickClient mouseButton2Activity.
			^ false ].
		"If we have already moved, then it won't be a double or triple click... why wait?"
		(timedOut or: [distance > 0]) ifTrue: [
			aHand dontWaitForMoreClicks.
			dragSelector
				ifNotNil: [ self didDrag ]
				ifNil: [ self didClick ].
			^ false ]].

	"If we're over triple click, or timed out, or mouse moved, don't allow more clicks."
	(buttonDownCount = 4 or: [ timedOut or: [ distance > 0 ]]) ifTrue: [
		aHand dontWaitForMoreClicks.
		^ false ].

	"Simple click."
	(buttonDownCount = 1 and: [ buttonUpCount = 1 ]) ifTrue: [
		self didClick ].

	"Click & hold"
	(buttonDownCount = 2 and: [ buttonUpCount = 1]) ifTrue: [
		self didClickAndHalf ].

	"Double click."
	(buttonDownCount = 2 and: [ buttonUpCount = 2]) ifTrue: [
		self didDoubleClick ].

	"Double click & hold."
	(buttonDownCount = 3 and: [ buttonUpCount = 2]) ifTrue: [
		self didDoubleClickAndHalf ].

	"Triple click"
	(buttonDownCount = 3 and: [ buttonUpCount = 3]) ifTrue: [
		self didTripleClick ].

	"This means: if a mouseDown, then don't further process this event (so we can turn it into a double or triple click on next buttonUp)"
	^ aMouseEvent isMouseDown
</details>

#### MouseClickState>>#didDoubleClick

<details>
	<summary>See more</summary>
	
	didDoubleClick

	doubleClickDone ifFalse: [
		dblClickSelector ifNotNil: [
			clickClient perform: dblClickSelector with: lastClickDown with: self lastClickLocalPosition ].
		doubleClickDone _ true ]
</details>

## RectangleLikeMorph

Hierarchy for morphs that are rectangle like. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape. Subclasses can add things like 'roundedCornerRadious' or such.

### Methods
#### RectangleLikeMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ `50@40`.
	color _ self defaultColor
</details>

#### RectangleLikeMorph>>#privateExtent: aPoint

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint
	"Answer whether extent was actually changed.
	If some subclass may reject the update, answer false in those cases."

	| newExtent |
	newExtent _ aPoint max: self minimumExtent.
	^extent = newExtent
		ifFalse: [ extent _ newExtent ]; not
</details>

#### RectangleLikeMorph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^true
</details>

#### RectangleLikeMorph>>#morphExtentInWorld: newExtent

world coordinates


<details>
	<summary>See more</summary>
	
	morphExtentInWorld: newExtent
	"world coordinates"
	self flag: #jmvVer2.
	self morphExtent: (self internalizeDistanceFromWorld: newExtent)
</details>

#### RectangleLikeMorph>>#morphWidth

Ensure everybody wants our coordinates!


<details>
	<summary>See more</summary>
	
	morphWidth

"Ensure everybody wants our coordinates!"
	self flag: #jmvVer2.
	^ extent x
</details>

#### RectangleLikeMorph>>#morphExtent: newExtent

assume it is always in our coordinates!


<details>
	<summary>See more</summary>
	
	morphExtent: newExtent
	"assume it is always in our coordinates!"
	| oldBoundsInWorld |
	self flag: #jmvVer2.
	extent = newExtent ifFalse: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld _ self morphBoundsInWorld.
		(self privateExtent: newExtent) ifTrue: [
			"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
			oldBoundsInWorld ifNotNil: [
				self invalidateDisplayRect: oldBoundsInWorld from: nil ].
			self someSubmorphPositionOrExtentChanged.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ]]
</details>

#### RectangleLikeMorph>>#color

<details>
	<summary>See more</summary>
	
	color

	^ color
</details>

#### RectangleLikeMorph>>#layoutSpec: aLayoutSpec

Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout


<details>
	<summary>See more</summary>
	
	layoutSpec: aLayoutSpec
	"Layout specific. Set the layout spec describing where the receiver should appear in a proportional layout"
	self layoutSpec == aLayoutSpec ifTrue: [ ^self ].
	aLayoutSpec morph: self.
	layoutSpec := aLayoutSpec.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]
</details>

#### RectangleLikeMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ `Color orange`
</details>

#### RectangleLikeMorph>>#morphPosition: newPos extent: newExtent

Change the position of this morph. Argument is in owner's coordinates.


<details>
	<summary>See more</summary>
	
	morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent or position has really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent or position has really changed."
		oldBoundsInWorld ifNotNil: [
			self invalidateDisplayRect: oldBoundsInWorld from: nil ].
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]
</details>

#### RectangleLikeMorph>>#morphExtent

In our own coordinates!


<details>
	<summary>See more</summary>
	
	morphExtent
	"In our own coordinates!"
"Quizas eventualmente borrar este tambien? (no se usa mucho...)"
	self flag: #jmvVer2.
	^ extent
</details>

#### RectangleLikeMorph>>#morphWidth: aNumber

Ensure everybody wants our coordinates!


<details>
	<summary>See more</summary>
	
	morphWidth: aNumber

"Ensure everybody wants our coordinates!"
	self flag: #jmvVer2.
	self morphExtent: aNumber@extent y
</details>

#### RectangleLikeMorph>>#color: aColor

Set the receiver's color.


<details>
	<summary>See more</summary>
	
	color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color _ aColor.
		self redrawNeeded ]
</details>

#### RectangleLikeMorph>>#morphHeight

Ensure everybody wants our coordinates!


<details>
	<summary>See more</summary>
	
	morphHeight

"Ensure everybody wants our coordinates!"
	self flag: #jmvVer2.
	^ extent y
</details>

#### RectangleLikeMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: self color
</details>

#### RectangleLikeMorph>>#morphHeight: aNumber

Ensure everybody wants our coordinates!


<details>
	<summary>See more</summary>
	
	morphHeight: aNumber

"Ensure everybody wants our coordinates!"
	self flag: #jmvVer2.
	self morphExtent: extent x@aNumber
</details>

