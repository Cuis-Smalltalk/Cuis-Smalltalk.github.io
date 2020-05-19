## DropEvent

I remember data for drop events so the Morphs involved in the drop don't have to.

### Methods
#### DropEvent>>#formerPosition

<details>
	<summary>See more</summary>
	
	formerPosition
	
	^ formerPosition 
</details>

#### DropEvent>>#eventPosition

<details>
	<summary>See more</summary>
	
	eventPosition
	^position
</details>

#### DropEvent>>#wasHandled: aBool

This is ugly, and means that events are copied in many places...


<details>
	<summary>See more</summary>
	
	wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool
</details>

#### DropEvent>>#setPosition: pos contents: aMorph hand: aHand formerOwner: oldOwner formerPosition: oldPos

<details>
	<summary>See more</summary>
	
	setPosition: pos contents: aMorph hand: aHand formerOwner: oldOwner formerPosition: oldPos
	position _ pos.
	contents _ aMorph.
	source _ aHand.
	formerOwner := oldOwner.
	formerPosition := oldPos.
	wasHandled _ false.
</details>

#### DropEvent>>#wasHandled

Return true if this event was handled. May be ignored for some types of events.


<details>
	<summary>See more</summary>
	
	wasHandled
	^wasHandled
</details>

#### DropEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startDropEventDispatch: self
</details>

#### DropEvent>>#isDropEvent

<details>
	<summary>See more</summary>
	
	isDropEvent
	^true
</details>

#### DropEvent>>#formerOwner

<details>
	<summary>See more</summary>
	
	formerOwner
	
	^ formerOwner 
</details>

#### DropEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Drop is done on the innermost target that accepts it.


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."
	| eventPositionInChild dropped |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
			^self ]].

	(aMorph allowsMorphDrop and: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [
			"Do a symmetric check if both morphs like each other"
			dropped _ self contents.
			((aMorph wantsDroppedMorph: dropped event: self)	"I want her"
				and: [dropped wantsToBeDroppedInto: aMorph])		"she wants me"
					ifTrue: [
						^ self sentTo: aMorph localPosition: positionInAMorph ]].
	^#rejected
</details>

#### DropEvent>>#contents

<details>
	<summary>See more</summary>
	
	contents
	^contents
</details>

#### DropEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into aMorph


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropMorph: self localPosition: positionInAMorph
</details>

## DropFilesEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DropFilesEvent>>#primDropRequestFileHandle: dropIndex

Primitive. Return the (read-only) file handle for some file that was just dropped onto Squeak. Fail if dropIndex is out of range or the primitive is not supported.


<details>
	<summary>See more</summary>
	
	primDropRequestFileHandle: dropIndex
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileHandle' module:'DropPlugin'>
	^nil
</details>

#### DropFilesEvent>>#eventPosition

<details>
	<summary>See more</summary>
	
	eventPosition

	^position
</details>

#### DropFilesEvent>>#numberOfFiles

<details>
	<summary>See more</summary>
	
	numberOfFiles

	^numberOfFiles
</details>

#### DropFilesEvent>>#isDropEvent

<details>
	<summary>See more</summary>
	
	isDropEvent

	^true
</details>

#### DropFilesEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Drop is done on the innermost target that accepts it.


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Drop is done on the innermost target that accepts it."

	| eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph) ifFalse: [ ^#rejected ].

	"Go looking if any of our submorphs wants it"
	aMorph submorphsDo: [ :eachChild |
		eventPositionInChild _ eachChild internalize: positionInAMorph.
		(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [ ^self ]].

	(aMorph allowsFilesDrop and: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [^ self sentTo: aMorph localPosition: positionInAMorph ].
	
	^#rejected
</details>

#### DropFilesEvent>>#initializeAt: aPosition with: aNumberOfFiles from: aHand

<details>
	<summary>See more</summary>
	
	initializeAt: aPosition with: aNumberOfFiles from: aHand

	position := aPosition.
	numberOfFiles := aNumberOfFiles.
	source := aHand.
	wasHandled := false.
</details>

#### DropFilesEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into aMorph


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^aMorph processDropFiles: self localPosition: positionInAMorph
</details>

#### DropFilesEvent>>#primDropRequestFileName: dropIndex

Primitive. Return the file name for some file that was just dropped onto Squeak. Fail if dropIndex is out of range or the primitive is not supported.


<details>
	<summary>See more</summary>
	
	primDropRequestFileName: dropIndex
	"Primitive. Return the file name for some file that was just dropped onto Squeak.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileName' module:'DropPlugin'>
	^nil
</details>

#### DropFilesEvent>>#wasHandled

Return true if this event was handled. May be ignored for some types of events.


<details>
	<summary>See more</summary>
	
	wasHandled

	^wasHandled
</details>

#### DropFilesEvent>>#wasHandled: aBool

This is ugly, and means that events are copied in many places...


<details>
	<summary>See more</summary>
	
	wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool
</details>

#### DropFilesEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startDropFilesEventDispatch: self
</details>

#### DropFilesEvent>>#fileNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | fileName |
		fileName := self primDropRequestFileName: fileNumber.
		fileName ifNotNil: aBlock ]
</details>

## KeyboardEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### KeyboardEvent>>#isBackspace

Returns true if the pressed key is a backspace key. In Text Editors, pressing backspace usually means to delete the character before the cursor position


<details>
	<summary>See more</summary>
	
	isBackspace
	"Returns true if the pressed key is a backspace key. In Text Editors, pressing backspace usually means to delete the character before the cursor position"
	^ keyValue = 8
</details>

#### KeyboardEvent>>#isEnd

<details>
	<summary>See more</summary>
	
	isEnd
	
	^ keyValue = 4
</details>

#### KeyboardEvent>>#isAlphaNumeric

<details>
	<summary>See more</summary>
	
	isAlphaNumeric

	^ self keyCharacter isAlphaNumeric
</details>

#### KeyboardEvent>>#closeCurrentWindowOf: aMorph

<details>
	<summary>See more</summary>
	
	closeCurrentWindowOf: aMorph

	aMorph owningWindow ifNotNil: [ :w |
		(w morphContainsPoint: (w internalizeFromWorld: self eventPosition))
			ifTrue: [ w delete. ] ]
</details>

#### KeyboardEvent>>#isKeyUp

<details>
	<summary>See more</summary>
	
	isKeyUp
	^ type == #keyUp
</details>

#### KeyboardEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into anObject


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	type == #keystroke ifTrue: [
		self isFindClassShortcut
			ifTrue: [ ^ BrowserWindow findClass].
		self isCloseWindowShortcut
			ifTrue: [ ^ self closeCurrentWindowOf: aMorph ].
		^ aMorph
			processKeystroke: self
			localPosition: positionInAMorph ].
	type == #keyDown ifTrue: [
		^ aMorph
			processKeyDown: self
			localPosition: positionInAMorph ].
	type == #keyUp ifTrue: [ 
		^ aMorph
			processKeyUp: self
			localPosition: positionInAMorph ].
	^ super
		sentTo: aMorph
		localPosition: positionInAMorph.
</details>

#### KeyboardEvent>>#= aMorphicEvent

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^false ].

	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	keyValue = aMorphicEvent keyValue ifFalse: [ ^ false ].
	^ true
</details>

#### KeyboardEvent>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^buttons hash bitXor: keyValue hash

</details>

#### KeyboardEvent>>#isReturnKey

Answer true if the return key (called Enter in many PC keyboards) was pressed


<details>
	<summary>See more</summary>
	
	isReturnKey
	"Answer true if the return key (called Enter in many PC keyboards) was pressed"

	^keyValue = InputSensor returnKey
</details>

#### KeyboardEvent>>#isKeystroke

<details>
	<summary>See more</summary>
	
	isKeystroke
	^ type == #keystroke
</details>

#### KeyboardEvent>>#isColon

<details>
	<summary>See more</summary>
	
	isColon

	^ self keyCharacter = $:.
</details>

#### KeyboardEvent>>#isPageDown

<details>
	<summary>See more</summary>
	
	isPageDown

	^ keyValue = 12
</details>

#### KeyboardEvent>>#isTab

<details>
	<summary>See more</summary>
	
	isTab

	^self keyCharacter = Character tab.
</details>

#### KeyboardEvent>>#isCtrlSpace

<details>
	<summary>See more</summary>
	
	isCtrlSpace
	
	^ (self controlKeyPressed or: [ self rawMacOptionKeyPressed ]) and: [ self isSpace ]
</details>

#### KeyboardEvent>>#isFindClassShortcut

<details>
	<summary>See more</summary>
	
	isFindClassShortcut

	^ self shiftPressed and: [ self isReturnKey ]
</details>

#### KeyboardEvent>>#setType: aSymbol buttons: anInteger position: pos keyValue: aValue hand: aHand stamp: stamp

<details>
	<summary>See more</summary>
	
	setType: aSymbol buttons: anInteger position: pos keyValue: aValue hand: aHand stamp: stamp
	type _ aSymbol.
	buttons _ anInteger.
	position _ pos.
	keyValue _ aValue.
	source _ aHand.
	wasHandled _ false.
	timeStamp _ stamp.
</details>

#### KeyboardEvent>>#isQuesitonMark

<details>
	<summary>See more</summary>
	
	isQuesitonMark
	
	^ self keyCharacter = $? 
</details>

#### KeyboardEvent>>#isMouseMove

<details>
	<summary>See more</summary>
	
	isMouseMove
	^false
</details>

#### KeyboardEvent>>#isPageUp

<details>
	<summary>See more</summary>
	
	isPageUp

	^ keyValue = 11
</details>

#### KeyboardEvent>>#isCloseWindowShortcut

<details>
	<summary>See more</summary>
	
	isCloseWindowShortcut

	^ (self commandAltKeyPressed or: [ self controlKeyPressed ])
		and: [self keyCharacter = $w]
</details>

#### KeyboardEvent>>#isSpace

<details>
	<summary>See more</summary>
	
	isSpace

	^ #(0 32 160) includes: keyValue.
</details>

#### KeyboardEvent>>#isArrowUp

<details>
	<summary>See more</summary>
	
	isArrowUp

	^ keyValue = 30
</details>

#### KeyboardEvent>>#keyValue

Answer the ascii value for this keystroke. This is defined only for keystroke events.


<details>
	<summary>See more</summary>
	
	keyValue
	"Answer the ascii value for this keystroke. This is defined only for keystroke events."

	^ keyValue
</details>

#### KeyboardEvent>>#isCmdAltLessThan

Answer true if the user pressed cmd/Alt + $< We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!) Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed... Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !


<details>
	<summary>See more</summary>
	
	isCmdAltLessThan
	"Answer true if the user pressed cmd/Alt + $<
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char _ self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char = $< ].

	shifted _ self shiftPressed.
	
	"For Mac keyboard layouts where $< is unshifted (for example, German and Spanish)"
	(shifted not and: [ char = $< ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $< is shift + $, (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $,"
	(shifted and: [ char = $, ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $< then..."
	^false
</details>

#### KeyboardEvent>>#isCmdAltGreaterThan

Answer true if the user pressed cmd/Alt + $> We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!) Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed... Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !


<details>
	<summary>See more</summary>
	
	isCmdAltGreaterThan
	"Answer true if the user pressed cmd/Alt + $>
	We need this special method because of platform differences, especially the weird way the Mac VM reports keystrokes if cmd or control are held down (the character answered is usually the unshifted one, even if shift is pressed, depending on keyboard layout!)
	Note: On the Mac, this code handles US, German and Spanish layouts. Add more as needed...
	Tweak both #isCmdAltLessThan and #isCmdAltGreaterThan !"
	| char shifted |

	self commandAltKeyPressed ifFalse: [ ^false ].

	char _ self keyCharacter.
	"Do the easy test for non macOS"
	Smalltalk platformName = 'Mac OS' ifFalse: [
		^char = $> ].

	shifted _ self shiftPressed.
	
	"For Mac keyboard layouts where $> is unshifted (Is there such a layout?)"
	(shifted not and: [ char = $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $. (for example US).
	As cmd is pressed, the Mac VM gives us the unshifted character, i.e. $."
	(shifted and: [ char = $. ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<.
	In this case, in the German layout, the Mac VM gives us $> (surprising!)"
	(shifted and: [ char = $> ])
		ifTrue: [ ^true ].

	"For Mac keyboard layouts where $> is shift + $<
	In the Spanish layout, as cmd is pressed, the Mac VM gives us the unshifted character, i.e. $<"
	(shifted and: [ char = $< ])
		ifTrue: [ ^true ].

	"We guess it is not cmd + $> then..."
	^false
</details>

#### KeyboardEvent>>#isArrowDown

<details>
	<summary>See more</summary>
	
	isArrowDown

	^ keyValue = 31
</details>

#### KeyboardEvent>>#isArrowLeft

<details>
	<summary>See more</summary>
	
	isArrowLeft
	
	^keyValue = 28
</details>

#### KeyboardEvent>>#isHome

<details>
	<summary>See more</summary>
	
	isHome
	
	^ keyValue = 1
</details>

#### KeyboardEvent>>#isEsc

<details>
	<summary>See more</summary>
	
	isEsc

	^ keyValue = 27
</details>

#### KeyboardEvent>>#keyCharacter

Answer the character corresponding this keystroke. This is defined only for keystroke events.


<details>
	<summary>See more</summary>
	
	keyCharacter
	"Answer the character corresponding this keystroke. This is defined only for keystroke events."

	^ keyValue asCharacter
</details>

#### KeyboardEvent>>#isArrowRight

<details>
	<summary>See more</summary>
	
	isArrowRight
	
	^keyValue = 29 
</details>

#### KeyboardEvent>>#isKeyboard

<details>
	<summary>See more</summary>
	
	isKeyboard
	^true
</details>

#### KeyboardEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startKeyboardDispatch: self
</details>

#### KeyboardEvent>>#isKeyDown

<details>
	<summary>See more</summary>
	
	isKeyDown
	^ type == #keyDown
</details>

#### KeyboardEvent>>#isDelete

Returns true on the delete key, which is not the same as the backspace key. In Text Editors, it usually means to delete the character after the cursor


<details>
	<summary>See more</summary>
	
	isDelete
	"Returns true on the delete key, which is not the same as the backspace key. In Text Editors, it usually means to delete the character after the cursor"
	^keyValue = 127	
</details>

## MorphicAlarm

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MorphicAlarm>>#scheduledTime: msecs

Set the time (in milliseconds) that the receiver is scheduled to be executed


<details>
	<summary>See more</summary>
	
	scheduledTime: msecs
	"Set the time (in milliseconds) that the receiver is scheduled to be executed"
	scheduledTime _ msecs
</details>

#### MorphicAlarm>>#scheduledTime

Return the time (in milliseconds) that the receiver is scheduled to be executed


<details>
	<summary>See more</summary>
	
	scheduledTime
	"Return the time (in milliseconds) that the receiver is scheduled to be executed"
	^scheduledTime
</details>

#### MorphicAlarm>>#valueAtTime: millisecondClock

<details>
	<summary>See more</summary>
	
	valueAtTime: millisecondClock

	|  nArgs |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondClock]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondClock) ]
</details>

## MorphicEvent

This class represents the base for all Morphic events. Instance variables: stamp <Integer> The millisecond clock time stamp (based on Time millisecondClock) source <Hand | nil> If non-nil the hand that generated the event.

### Methods
#### MorphicEvent>>#isMouse

<details>
	<summary>See more</summary>
	
	isMouse
	^false
</details>

#### MorphicEvent>>#isDraggingEvent

<details>
	<summary>See more</summary>
	
	isDraggingEvent
	^false
</details>

#### MorphicEvent>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #MorphicEvent or: [ super is: aSymbol ]
</details>

#### MorphicEvent>>#hand

Return the source that generated the event


<details>
	<summary>See more</summary>
	
	hand
	"Return the source that generated the event"
	^source
</details>

#### MorphicEvent>>#isWindowEvent

<details>
	<summary>See more</summary>
	
	isWindowEvent
	^false
</details>

#### MorphicEvent>>#isDropEvent

<details>
	<summary>See more</summary>
	
	isDropEvent
	^false
</details>

#### MorphicEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event.


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Dispatch me. The event will be passed to the front-most visible submorph that contains the position wrt. to the event."
	| handledByInner eventPositionInChild |

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	"Check for being inside the receiver"
	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ])
		ifTrue: [ ^ self sentTo: aMorph localPosition: positionInAMorph ].

	^ #rejected
</details>

#### MorphicEvent>>#isMouseOver

<details>
	<summary>See more</summary>
	
	isMouseOver
	^false
</details>

#### MorphicEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into aMorph


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	^ aMorph processUnknownEvent: self localPosition: positionInAMorph
</details>

#### MorphicEvent>>#isKeystroke

<details>
	<summary>See more</summary>
	
	isKeystroke
	^false
</details>

#### MorphicEvent>>#setTimeStamp: stamp

<details>
	<summary>See more</summary>
	
	setTimeStamp: stamp
	timeStamp := stamp.
</details>

#### MorphicEvent>>#wasHandled: aBool

Determine if this event was handled. May be ignored for some types of events.


<details>
	<summary>See more</summary>
	
	wasHandled: aBool
	"Determine if this event was handled. May be ignored for some types of events."
</details>

#### MorphicEvent>>#wasHandled

Return true if this event was handled. May be ignored for some types of events.


<details>
	<summary>See more</summary>
	
	wasHandled
	"Return true if this event was handled. May be ignored for some types of events."
	^false
</details>

#### MorphicEvent>>#isKeyboard

<details>
	<summary>See more</summary>
	
	isKeyboard
	^false
</details>

#### MorphicEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"
	"An event of an unknown type was sent. What shall we do?!"

	Smalltalk beep. 
	self printString displayAt: `0@0`.
	self wasHandled: true
</details>

#### MorphicEvent>>#timeStamp

Return the millisecond clock value at which the event was generated


<details>
	<summary>See more</summary>
	
	timeStamp
	"Return the millisecond clock value at which the event was generated"
	^timeStamp ifNil:[timeStamp _ Time localMillisecondClock]
</details>

## MouseButtonEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MouseButtonEvent>>#mouseButton3Changed

Answer true if the mouseButton3 has changed. Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or meta+click on Linux. It is also emulated here with shift-ctrl-click on any platform. The check for button change (instead of button press) is specially useful on buttonUp events.


<details>
	<summary>See more</summary>
	
	mouseButton3Changed
	"Answer true if the mouseButton3 has changed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or meta+click on Linux.
	It is also emulated here with shift-ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into3 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton3
</details>

#### MouseButtonEvent>>#mouseButton2Changed

Answer true if the mouseButton2 has changed. Reported by the VM for right mouse button or option+click on the Mac. It is also emulated here with ctrl-click on any platform. The check for button change (instead of button press) is specially useful on buttonUp events.


<details>
	<summary>See more</summary>
	
	mouseButton2Changed
	"Answer true if the mouseButton2 has changed.
	Reported by the VM for right mouse button or option+click on the Mac.
	It is also emulated here with ctrl-click on any platform.
	The check for button change (instead of button press) is specially useful on buttonUp events."

	(self turnMouseButton1Into2 and: [ whichButton anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ whichButton anyMask: InputSensor mouseButton2
</details>

#### MouseButtonEvent>>#mouseButton1Changed

Answer true if the mouseButton1 has changed. Reported by the VM for the single/first mouse button, usually the one at the left. But if they are combined with modifier keys, it is might button 2 or 3. See mouseButton1Changed and mouseButton3Changed. The check for button change (instead of button press) is specially useful on buttonUp events. See also #mouseButton1Pressed


<details>
	<summary>See more</summary>
	
	mouseButton1Changed
	"Answer true if the mouseButton1 has changed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton1Changed and mouseButton3Changed.
	The check for button change (instead of button press) is specially useful on buttonUp events.
	See also #mouseButton1Pressed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ whichButton anyMask: InputSensor mouseButton1
</details>

#### MouseButtonEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Find the appropriate receiver for the event and let it handle it. Default rules: * The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event. * When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is. * When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed. * If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	| aMorphHandlesIt grabAMorph handledByInner eventPositionInChild |
	"Only for MouseDown"
	self isMouseDown ifFalse: [
		^super dispatchWith: aMorph localPosition: positionInAMorph ].

	"Try to get out quickly"
	(aMorph fullContainsPoint: positionInAMorph)
		ifFalse: [ ^#rejected ].

	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	grabAMorph _ false.
	self mouseButton3Pressed
		ifTrue: [
			(eventHandler isNil or: [ eventHandler isWorldMorph or: [
					self shiftPressed or: [ aMorph is: #HaloMorph ]]])
				ifTrue: [
					eventHandler _ aMorph.
					aMorphHandlesIt _ true ]]
		ifFalse: [
			(aMorph handlesMouseDown: self) ifTrue: [
				eventHandler _ aMorph.
				aMorphHandlesIt _ true ].
			"If button 1, and both aMorph and the owner allows grabbing with the hand (to initiate drag & drop), so be it."
			self mouseButton1Pressed ifTrue: [
				aMorph owner ifNotNil: [ :o |
					(o allowsSubmorphDrag and: [ aMorph isSticky not ]) ifTrue: [
						grabAMorph _ true ]]]].

	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild dispatchEvent: self localPosition: eventPositionInChild) == #rejected ifFalse: [
				"Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].

	(handledByInner or: [ aMorph containsPoint: positionInAMorph event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt
			ifTrue: [ ^self sentTo: aMorph localPosition: positionInAMorph ]
			ifFalse: [
				(grabAMorph and: [ handledByInner not ]) ifTrue: [
					self hand
						waitForClicksOrDrag: aMorph event: self
						dragSel: (Preferences clickGrabsMorphs ifFalse: [#dragEvent:localPosition:])
						clkSel: (Preferences clickGrabsMorphs ifTrue: [#dragEvent:localPosition:]).
					"false ifTrue: [ self hand grabMorph: aMorph ]."
					Preferences clickGrabsMorphs ifFalse: [aMorph activateWindow].
					self wasHandled: true.
					^self ]]].

	handledByInner ifTrue: [ ^self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected
</details>

#### MouseButtonEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into anObject


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		^aMorph processMouseDown: self localPosition: positionInAMorph ].
	type == #mouseUp ifTrue: [
		^aMorph processMouseUp: self localPosition: positionInAMorph ].
	^super sentTo: aMorph localPosition: positionInAMorph
</details>

#### MouseButtonEvent>>#setType: evtType position: evtPos which: button buttons: evtButtons hand: evtHand stamp: stamp

<details>
	<summary>See more</summary>
	
	setType: evtType position: evtPos which: button buttons: evtButtons hand: evtHand stamp: stamp
	type _ evtType.
	position _ evtPos.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
	whichButton _ button.
	timeStamp _ stamp.
</details>

## MouseEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MouseEvent>>#isMouse

<details>
	<summary>See more</summary>
	
	isMouse
	^true
</details>

#### MouseEvent>>#isMouseUp

<details>
	<summary>See more</summary>
	
	isMouseUp
	^ type == #mouseUp
</details>

#### MouseEvent>>#mouseButton3Pressed

Answer true if the mouseButton3 is being pressed. Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or win/meta+click on Windows and Linux. It is also emulated here with shift-ctrl-click on any platform.


<details>
	<summary>See more</summary>
	
	mouseButton3Pressed
	"Answer true if the mouseButton3 is being pressed.
	Reported by the VM for center (wheel) mouse button or cmd+click on the Mac or win/meta+click on Windows and Linux.
	It is also emulated here with shift-ctrl-click on any platform."

	(self turnMouseButton1Into3 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton3
</details>

#### MouseEvent>>#isMouseEnter

<details>
	<summary>See more</summary>
	
	isMouseEnter
	^ type == #mouseEnter
</details>

#### MouseEvent>>#isMouseOver

<details>
	<summary>See more</summary>
	
	isMouseOver
	^type == #mouseOver
</details>

#### MouseEvent>>#isMouseDown

<details>
	<summary>See more</summary>
	
	isMouseDown
	^ type == #mouseDown
</details>

#### MouseEvent>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ type hash bitXor: (position hash bitXor: buttons hash)
</details>

#### MouseEvent>>#asMouseMove: deltaTime

Convert the receiver into a mouse move. adjust timestamp by the provided delta


<details>
	<summary>See more</summary>
	
	asMouseMove: deltaTime
	"Convert the receiver into a mouse move. adjust timestamp by the provided delta"

	^ MouseMoveEvent new
		setType: #mouseMove
		position: position
		buttons: buttons
		hand: source
		stamp: timeStamp + deltaTime
</details>

#### MouseEvent>>#= aMorphicEvent

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	type = aMorphicEvent eventType ifFalse: [ ^ false ].
	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true
</details>

#### MouseEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into aMorph


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into aMorph"

	type == #mouseOver ifTrue: [
		^aMorph processMouseOver: self localPosition: positionInAMorph ].
	type == #mouseEnter ifTrue: [
		^ aMorph processMouseEnter: self localPosition: positionInAMorph ].
	type == #mouseLeave ifTrue: [
		^aMorph processMouseLeave: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph
</details>

#### MouseEvent>>#asMouseEnter

<details>
	<summary>See more</summary>
	
	asMouseEnter

	^self copy setType: #mouseEnter
</details>

#### MouseEvent>>#setType: evtType position: evtPos buttons: evtButtons hand: evtHand

<details>
	<summary>See more</summary>
	
	setType: evtType position: evtPos buttons: evtButtons hand: evtHand
	type _ evtType.
	position _ evtPos.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
</details>

#### MouseEvent>>#isMouseMove

<details>
	<summary>See more</summary>
	
	isMouseMove
	^ type == #mouseMove
</details>

#### MouseEvent>>#isDraggingEvent

<details>
	<summary>See more</summary>
	
	isDraggingEvent
	source ifNil:[^false].
	source hasSubmorphs ifTrue:[^true].
	self anyButtonPressed ifTrue:[^true].
	^false
</details>

#### MouseEvent>>#turnMouseButton1Into2

Answer true if modifier keys are such that button 1 should be considered as button 2. ctrl - click -> right click


<details>
	<summary>See more</summary>
	
	turnMouseButton1Into2
	"Answer true if modifier keys are such that button 1 should be considered as button 2.
	ctrl - click -> right click
	"

	(self controlKeyPressed and: [self shiftPressed not]) ifTrue: [ ^ true ].
	^ false
</details>

#### MouseEvent>>#asMouseLeave

<details>
	<summary>See more</summary>
	
	asMouseLeave

	^self copy setType: #mouseLeave
</details>

#### MouseEvent>>#turnMouseButton1Into3

Answer true if modifier keys are such that button 1 should be considered as button 3. ctrl - shift - click -> center click alt -> click -> center click (effective only on Windows, the vm on Mac already reports center click, and on Linux right click)


<details>
	<summary>See more</summary>
	
	turnMouseButton1Into3
	"Answer true if modifier keys are such that button 1 should be considered as button 3.
	ctrl - shift - click -> center click
	alt -> click -> center click (effective only on Windows,
						the vm on Mac already reports center click, and on Linux right click)
	"

	(self controlKeyPressed and: [self shiftPressed]) ifTrue: [ ^ true ].
	self commandAltKeyPressed ifTrue: [ ^ true ].
	^ false
</details>

#### MouseEvent>>#isMouseScroll

<details>
	<summary>See more</summary>
	
	isMouseScroll
	^ type == #mouseScroll
</details>

#### MouseEvent>>#isMouseLeave

<details>
	<summary>See more</summary>
	
	isMouseLeave
	^ type == #mouseLeave
</details>

#### MouseEvent>>#isMove

<details>
	<summary>See more</summary>
	
	isMove
	^false
</details>

#### MouseEvent>>#mouseButton2Pressed

Answer true if the mouseButton2 is being pressed. Reported by the VM for right mouse button or option+click on the Mac, ctrl-click on Windows, or ctrl-click or alt-click on Linux. It is also emulated here with ctrl-click on any platform.


<details>
	<summary>See more</summary>
	
	mouseButton2Pressed
	"Answer true if the mouseButton2 is being pressed.
	Reported by the VM for right mouse button or option+click on the Mac, ctrl-click on Windows, or ctrl-click or alt-click on Linux.
	It is also emulated here with ctrl-click on any platform."

	(self turnMouseButton1Into2 and: [ buttons anyMask: InputSensor mouseButton1 ])
		ifTrue: [ ^ true ].
	^ buttons anyMask: InputSensor mouseButton2
</details>

#### MouseEvent>>#asMouseOver

Convert the receiver into a mouse over event


<details>
	<summary>See more</summary>
	
	asMouseOver
	"Convert the receiver into a mouse over event"
	^MouseEvent new setType: #mouseOver position: position buttons: buttons hand: source
</details>

#### MouseEvent>>#mouseButton1Pressed

Answer true if the mouseButton1 is being pressed. Reported by the VM for the single/first mouse button, usually the one at the left. But if they are combined with modifier keys, it is might button 2 or 3. See mouseButton2Pressed and mouseButton3Pressed. See also #mouseButton1Changed


<details>
	<summary>See more</summary>
	
	mouseButton1Pressed
	"Answer true if the mouseButton1 is being pressed.
	Reported by the VM for the single/first mouse button, usually the one at the left.
	But if they are combined with modifier keys, it is might button 2 or 3.
		See mouseButton2Pressed and mouseButton3Pressed.
	See also #mouseButton1Changed"

	self turnMouseButton1Into2 ifTrue: [ ^ false ].
	self turnMouseButton1Into3 ifTrue: [ ^ false ].
	^ buttons anyMask: InputSensor mouseButton1
</details>

#### MouseEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startMouseDispatch: self
</details>

#### MouseEvent>>#eventType

<details>
	<summary>See more</summary>
	
	eventType
	^type
</details>

#### MouseEvent>>#setType: aSymbol

For quick conversion between event types


<details>
	<summary>See more</summary>
	
	setType: aSymbol
	"For quick conversion between event types"
	type _ aSymbol.
</details>

#### MouseEvent>>#anyButtonPressed

Answer true if any mouse button is being pressed.


<details>
	<summary>See more</summary>
	
	anyButtonPressed
	"Answer true if any mouse button is being pressed."

	^ buttons anyMask: InputSensor anyMouseButton
</details>

## MouseMoveEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MouseMoveEvent>>#isMove

<details>
	<summary>See more</summary>
	
	isMove
	^true
</details>

#### MouseMoveEvent>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ position hash bitXor: buttons hash
</details>

#### MouseMoveEvent>>#setType: evtType position: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

<details>
	<summary>See more</summary>
	
	setType: evtType position: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type _ evtType.
	position _ evtEnd.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
	timeStamp _ stamp
</details>

#### MouseMoveEvent>>#= aMorphicEvent

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true
</details>

#### MouseMoveEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into anObject


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		^aMorph processMouseMove: self localPosition: positionInAMorph ].
	^ super sentTo: aMorph localPosition: positionInAMorph
</details>

## MouseOverHandler

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MouseOverHandler>>#noticeMouseOver: aMorph event: anEvent

Remember that the mouse is currently over some morph


<details>
	<summary>See more</summary>
	
	noticeMouseOver: aMorph event: anEvent
	"Remember that the mouse is currently over some morph"
	leftMorphs ifNil: [ ^self ].		"Might happen if you halt during layout."
	(leftMorphs includes: aMorph) 
		ifTrue:[leftMorphs remove: aMorph]
		ifFalse:[enteredMorphs nextPut: aMorph].
	overMorphs nextPut: aMorph.

</details>

#### MouseOverHandler>>#processMouseOver: aMouseEvent

Re-establish the z-order for all morphs wrt the given event


<details>
	<summary>See more</summary>
	
	processMouseOver: aMouseEvent 
	"Re-establish the z-order for all morphs wrt the given event"

	| hand focus evt p |
	hand := aMouseEvent hand.
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	"Now go looking for eventual mouse overs"
	hand startEventDispatch: aMouseEvent asMouseOver.
	"Get out early if there's no change"
	(leftMorphs isNil or: [			"Should never happen, but it could if you halt during layout."
		(leftMorphs isEmpty and: [enteredMorphs position = 0])])
		ifTrue: [^leftMorphs := enteredMorphs := overMorphs := nil].
	focus := hand mouseFocus.
	"Send #mouseLeave as appropriate"
	evt := aMouseEvent asMouseLeave.
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m | 
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					p _ m internalizeFromWorld: evt eventPosition.
					evt sentTo: m localPosition: p ]
				ifFalse: [overMorphs nextPut: m]].
	"Send #mouseEnter as appropriate"
	evt := aMouseEvent asMouseEnter.
	enteredMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	enteredMorphs := enteredMorphs contents.
	enteredMorphs reverseDo: [ :m | 
			(m == focus or: [m hasOwner: focus]) 
				ifTrue: [
					p _ m internalizeFromWorld: evt eventPosition.
					evt sentTo: m localPosition: p ]].
	"And remember the over list"
	overMorphs ifNil: [
			"inform: was called in handleEvent:"
			^leftMorphs := enteredMorphs := overMorphs := nil].
	mouseOverMorphs := overMorphs contents.
	leftMorphs := enteredMorphs := overMorphs := nil
</details>

#### MouseOverHandler>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	mouseOverMorphs _ #().
</details>

## MouseScrollEvent

A MouseScrollEvent can be any type of secondary pointer movement (typically via a scroll wheel on a traditional mouse or a gesture on a trackpad). Currently, events are extracted from KeyboardEvents (which is how the VM currently communicates things like scroll wheel events via ctl+arrow up/down). It is also possible to generate these events with a keyboard, pressing ctrl-down or ctrl-up. Given this, we also added ctrl-left and ctrl-right, that can only be generated with a keyboard, to control horizontal scroll.

### Methods
#### MouseScrollEvent>>#direction

<details>
	<summary>See more</summary>
	
	direction
	^ direction 
</details>

#### MouseScrollEvent>>#setType: evtType position: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp

<details>
	<summary>See more</summary>
	
	setType: evtType position: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp
	type _ evtType.
	position _ evtPos.
	buttons _ evtButtons.
	source _ evtHand.
	wasHandled _ false.
	direction _ evtDir.
	timeStamp _ stamp.
</details>

#### MouseScrollEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Find the appropriate receiver for the event and let it handle it. Default rules: * The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event. * When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is. * When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed. * If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Find the appropriate receiver for the event and let it handle it. Default rules:
	* The top-most chain of visible, unlocked morphs containing the event position will get a chance to handle the event.
	* When travelling down the hierarchy a prospective handler for the event is installed. This prospective handler can be used by submorphs wishing to handle the mouse down for negotiating who the receiver is.
	* When travelling up, the prospective handler is always executed. The handler needs to check if the event was handled before as well as checking if somebody else's handler has been installed.
	* If another handler has been installed but the event was not handled it means that somebody up in the hierarchy wants to handle the event.
	"
	"Try to get out quickly"
	| aMorphHandlesIt handledByInner eventPositionInChild focus|
	focus := self hand keyboardFocus.
	"FIXME - this works in all tested cases but one: when the window directly under the mouse doesn't have keyboard focus (i.e. a Transcript window)"
	((aMorph fullContainsPoint: positionInAMorph) and: [(aMorph = focus) or: [focus notNil and: [aMorph notNil and: [focus hasOwner: aMorph]]]]) ifFalse: [ ^ #rejected ].
	"Install the prospective handler for the receiver"
	aMorphHandlesIt _ false.
	(aMorph handlesMouseScroll: self) ifTrue: [
		eventHandler _ aMorph.
		aMorphHandlesIt _ true ].
	"Now give submorphs a chance to handle the event"
	handledByInner _ false.
	aMorph submorphsDo: [ :eachChild |
		handledByInner ifFalse: [
			eventPositionInChild _ eachChild internalize: positionInAMorph.
			(eachChild
				dispatchEvent: self
				localPosition: eventPositionInChild) == #rejected ifFalse: [ "Some child did contain the point so aMorph is part of the top-most chain."
				handledByInner _ true ]]].
	(handledByInner or: [
		aMorph
			containsPoint: positionInAMorph
			event: self ]) ifTrue: [
		"aMorph is in the top-most unlocked, visible morph in the chain."
		aMorphHandlesIt ifTrue: [ ^ self
				sentTo: aMorph
				localPosition: positionInAMorph ]].
	handledByInner ifTrue: [ ^ self ].
	"Mouse was not on aMorph nor any of its children"
	^ #rejected.
</details>

#### MouseScrollEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into anObject


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"
	^ aMorph
		processMouseScroll: self
		localPosition: positionInAMorph.
</details>

#### MouseScrollEvent>>#= aMorphicEvent

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class == aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	direction = aMorphicEvent direction ifFalse: [ ^ false ].
	^ true
</details>

#### MouseScrollEvent>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ position hash bitXor: (buttons hash bitXor: direction hash)
</details>

## StepMessage

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### StepMessage>>#valueAtTime: millisecondClock

<details>
	<summary>See more</summary>
	
	valueAtTime: millisecondClock

	|  nArgs millisecondsSinceLast |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
	lastEvaluationTime ifNil: [ lastEvaluationTime _ millisecondClock ].
	millisecondsSinceLast _ millisecondClock - lastEvaluationTime.
	lastEvaluationTime _ millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondsSinceLast]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]
</details>

#### StepMessage>>#stepTime: aNumber

Set the step time for this message. If nil, the receiver of the message will be asked for its #stepTime.


<details>
	<summary>See more</summary>
	
	stepTime: aNumber
	"Set the step time for this message. If nil, the receiver of the message will be asked for its #stepTime."
	stepTime _ aNumber
</details>

#### StepMessage>>#rescheduleAfter: millisecondTimer

Schedule next run


<details>
	<summary>See more</summary>
	
	rescheduleAfter: millisecondTimer
	"Schedule next run"
	scheduledTime _ scheduledTime + self stepTime max: millisecondTimer + 1
</details>

#### StepMessage>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: receiver;
		space;
		print: selector;
		space;
		print: scheduledTime;
		nextPut: $).
</details>

#### StepMessage>>#stepTime

Return the step time for this message. If nil, the receiver of the message will be asked for its #stepTime.


<details>
	<summary>See more</summary>
	
	stepTime
	"Return the step time for this message. If nil, the receiver of the message will be asked for its #stepTime."
	^stepTime ifNil: [ receiver stepTime ]
</details>

## UserInputEvent

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UserInputEvent>>#shiftPressed

Answer true if the shift key on the keyboard was being held down when this event occurred.


<details>
	<summary>See more</summary>
	
	shiftPressed
	"Answer true if the shift key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor shiftKey
</details>

#### UserInputEvent>>#macOptionKeyPressed

Answer whether the option key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific.


<details>
	<summary>See more</summary>
	
	macOptionKeyPressed
	"Answer whether the option key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific."

	self notify: 'Portability note:
MorphicEvent>>macOptionKeyPressed is not portable.
Please use MorphicEvent>>mouseButton2Pressed instead!'.

	^ self rawMacOptionKeyPressed
</details>

#### UserInputEvent>>#anyModifierKeyPressed

ignore, however, the shift keys 'cause that's not REALLY a command key


<details>
	<summary>See more</summary>
	
	anyModifierKeyPressed
	"ignore, however, the shift keys 'cause that's not REALLY a command key "

	^ self buttons anyMask: InputSensor anyModifierKey
</details>

#### UserInputEvent>>#eventPosition

<details>
	<summary>See more</summary>
	
	eventPosition
	^position
</details>

#### UserInputEvent>>#wasHandled

Return true if this event was handled. May be ignored for some types of events.


<details>
	<summary>See more</summary>
	
	wasHandled
	^wasHandled
</details>

#### UserInputEvent>>#wasHandled: aBool

This is ugly, and means that events are copied in many places...


<details>
	<summary>See more</summary>
	
	wasHandled: aBool

	"This is ugly, and means that events are copied in many places..."
	self flag: #jmvVer.

	wasHandled _ aBool
</details>

#### UserInputEvent>>#commandAltKeyPressed

Answer true if the command (Mac) / alt(Windows) key on the keyboard was being held down when this event occurred.


<details>
	<summary>See more</summary>
	
	commandAltKeyPressed
	"Answer true if the command (Mac) / alt(Windows) key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor commandAltKey
</details>

#### UserInputEvent>>#buttons

Return the a word encoding the mouse and modifier buttons for this event.


<details>
	<summary>See more</summary>
	
	buttons
	"Return the a word encoding the mouse and modifier buttons for this event."

	^ buttons
</details>

#### UserInputEvent>>#controlKeyPressed

Answer true if the control/ctrl key on the keyboard was being held down when this event occurred.


<details>
	<summary>See more</summary>
	
	controlKeyPressed
	"Answer true if the control/ctrl key on the keyboard was being held down when this event occurred."

	^ buttons anyMask: InputSensor controlKey
</details>

#### UserInputEvent>>#rawMacOptionKeyPressed

Answer whether the option/alt key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific. Please do not confuse with the alt key on Windows and Linux, that is called 'command' on the mac. See #commandAltKeyPressed


<details>
	<summary>See more</summary>
	
	rawMacOptionKeyPressed
	"Answer whether the option/alt key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific. Please do not confuse with the alt key on Windows and Linux, that is called 'command' on the mac. See #commandAltKeyPressed"

	^ buttons anyMask: InputSensor macOptionKey
</details>

## WindowEvent

I'm an event related to the host window, only dispatched to the World.

### Methods
#### WindowEvent>>#windowAction: aValue

<details>
	<summary>See more</summary>
	
	windowAction: aValue

	action _ aValue
</details>

#### WindowEvent>>#rectangle: aValue

<details>
	<summary>See more</summary>
	
	rectangle: aValue
	rectangle := aValue.
</details>

#### WindowEvent>>#eventPosition

Answer something...


<details>
	<summary>See more</summary>
	
	eventPosition
	"Answer something..."
	^rectangle topLeft
</details>

#### WindowEvent>>#windowEventType

This should match the definitions in sq.h


<details>
	<summary>See more</summary>
	
	windowEventType
	"This should match the definitions in sq.h"
	^#(
		windowMetricChange
		windowClose
		windowIconise
		windowActivated
		windowPaint
	) at: action ifAbsent: [#windowEventUnknown]
</details>

#### WindowEvent>>#startDispatchFrom: aHand

double dispatch the event dispatch


<details>
	<summary>See more</summary>
	
	startDispatchFrom: aHand
	"double dispatch the event dispatch"

	aHand startWindowEventDispatch: self
</details>

#### WindowEvent>>#isWindowEvent

<details>
	<summary>See more</summary>
	
	isWindowEvent
	^true
</details>

#### WindowEvent>>#dispatchWith: aMorph localPosition: positionInAMorph

Host window events do not have a position and are only dispatched to the World


<details>
	<summary>See more</summary>
	
	dispatchWith: aMorph localPosition: positionInAMorph
	"Host window events do not have a position and are only dispatched to the World"

	aMorph isWorldMorph ifFalse: [ ^#rejected ].
	self wasHandled ifTrue: [ ^self ].
	^ self sentTo: aMorph localPosition: positionInAMorph
</details>

#### WindowEvent>>#rectangle

<details>
	<summary>See more</summary>
	
	rectangle
	^rectangle
</details>

#### WindowEvent>>#sentTo: aMorph localPosition: positionInAMorph

Dispatch the receiver into anObject


<details>
	<summary>See more</summary>
	
	sentTo: aMorph localPosition: positionInAMorph
	"Dispatch the receiver into anObject"

	^ aMorph processWindowEvent: self localPosition: positionInAMorph
</details>

