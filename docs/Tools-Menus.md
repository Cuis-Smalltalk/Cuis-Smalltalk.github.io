## PopUpMenu

I represent a list of items. My instances are presented on the display screen in a rectangular area. The user points to an item, pressing a mouse button; the item is highlighted. When the button is released, the highlighted item indicates the selection.

### Methods
#### PopUpMenu>>#labels: aString lines: anArray icons: iconCollection

<details>
	<summary>See more</summary>
	
	labels: aString lines: anArray icons: iconCollection

	labelString _ aString.
	lineArray _ anArray.
	icons _ iconCollection 

</details>

#### PopUpMenu>>#frameHeight

Designed to avoid the entire frame computation (includes MVC form), since the menu may well end up being displayed in Morphic anyway.


<details>
	<summary>See more</summary>
	
	frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * Preferences standardMenuFont lineSpacing) + 4 "border width"
</details>

#### PopUpMenu>>#lineArray

<details>
	<summary>See more</summary>
	
	lineArray

	^ lineArray
</details>

#### PopUpMenu>>#labelString

<details>
	<summary>See more</summary>
	
	labelString

	^ labelString
</details>

#### PopUpMenu>>#labels: aString lines: anArray

<details>
	<summary>See more</summary>
	
	labels: aString lines: anArray

	self labels: aString lines: anArray icons: #()
</details>

#### PopUpMenu>>#iconAt: aPosition

<details>
	<summary>See more</summary>
	
	iconAt: aPosition

	^icons at: aPosition ifAbsent: [ nil ]
</details>

#### PopUpMenu>>#startUpMenu

Display and make a selection from the receiver as long as the button is pressed. Answer the current selection.


<details>
	<summary>See more</summary>
	
	startUpMenu
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil
</details>

#### PopUpMenu>>#startUpWithCaption: captionOrNil

Display the menu, slightly offset from the cursor, so that a slight tweak is required to confirm any action.


<details>
	<summary>See more</summary>
	
	startUpWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self startUpWithCaption: captionOrNil at: Sensor mousePoint allowKeyboard: Preferences menuKeyboardControl
</details>

#### PopUpMenu>>#startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean

This menu is too big to fit comfortably on the screen. Break it up into smaller chunks, and manage the relative indices. Inspired by a special-case solution by Reinier van Loon. The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)


<details>
	<summary>See more</summary>
	
	startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // Preferences standardMenuFont lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]
</details>

#### PopUpMenu>>#startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean

Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released, Answer the index of the current selection, or zero if the mouse is not released over any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard.


<details>
	<summary>See more</summary>
	
	startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean 
	"Display the menu, with caption if supplied. Wait for the mouse button to go down, then track the selection as long as the button is pressed. When the button is released,
	Answer the index of the current selection, or zero if the mouse is not released over  any menu item. Location specifies the desired topLeft of the menu body rectangle. The final argument indicates whether the menu should seize the keyboard focus in order to allow the user to navigate it via the keyboard."

	| maxHeight |
	maxHeight := Display height * 3 // 4.
	self frameHeight > maxHeight 
		ifTrue: [
			^self 
				startUpSegmented: maxHeight
				withCaption: captionOrNil
				at: location
				allowKeyboard: aBoolean].
	^(MVCMenuMorph from: self title: captionOrNil) 
		invokeAt: location
		allowKeyboard: aBoolean
</details>

## SelectionMenu

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SelectionMenu>>#selections: selectionArray

<details>
	<summary>See more</summary>
	
	selections: selectionArray
	selections _ selectionArray
</details>

#### SelectionMenu>>#startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean

Overridden to return value returned by manageMarker. The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)


<details>
	<summary>See more</summary>
	
	startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean
	"Overridden to return value returned by manageMarker.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

	| index |
	index _ super startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
	selections ifNil: [ ^index ].  "If there are no selections defined, show the super class' behavior."
	index between: 1 and: selections size :: ifFalse: [ ^nil ].
	^ selections at: index
</details>

## Switch

I represent a selection setting and actions to take depending on a change in the setting. An instance has three attributes: state, which is either on or off; on action; and off action. The on and off actions are blocks of code that execute whenever the instance changes state.

### Methods
#### Switch>>#turnOn

Set the state of the receiver to 'on'. If the state of the receiver was previously 'off', then 'self change' is sent and the receiver's on action is executed.


<details>
	<summary>See more</summary>
	
	turnOn
	"Set the state of the receiver to 'on'. If the state of the receiver was 
	previously 'off', then 'self change' is sent and the receiver's on action is 
	executed."

	self isOff
		ifTrue: 
			[on _ true.
			self changed: self.
			self doAction: onAction]
</details>

#### Switch>>#isOn

Answer whether the receiver is set on or not.


<details>
	<summary>See more</summary>
	
	isOn
	"Answer whether the receiver is set on or not."

	^on
</details>

#### Switch>>#initializeOff

<details>
	<summary>See more</summary>
	
	initializeOff

	on _ false. 
	onAction _ nil.
	offAction _ nil
</details>

#### Switch>>#turnOff

Set the state of the receiver to 'off'. If the state of the receiver was previously 'on', then 'self change' is sent and the receiver's off action is executed.


<details>
	<summary>See more</summary>
	
	turnOff
	"Set the state of the receiver to 'off'. If the state of the receiver was 
	previously 'on', then 'self change' is sent and the receiver's off action is 
	executed."

	self isOn
		ifTrue: 
			[on _ false.
			self changed: self.
			self doAction: offAction]
</details>

#### Switch>>#doAction: anAction

Execute anAction if it is non-nil.


<details>
	<summary>See more</summary>
	
	doAction: anAction 
	"Execute anAction if it is non-nil."

	anAction
		ifNotNil: [anAction value]
</details>

#### Switch>>#set

Set the state of the receiver to 'on'. If the state of the receiver was previously 'off', then 'self change' is sent. The receiver's on action is NOT executed.


<details>
	<summary>See more</summary>
	
	set
	"Set the state of the receiver to 'on'. If the state of the receiver was 
	previously 'off', then 'self change' is sent. The receiver's on action is 
	NOT executed."

	self isOff
		ifTrue: 
			[on _ true.
			self changed: self]
</details>

#### Switch>>#offAction: anAction

Set the off action of the receiver to anAction.


<details>
	<summary>See more</summary>
	
	offAction: anAction 
	"Set the off action of the receiver to anAction."

	offAction _ anAction
</details>

#### Switch>>#onAction: anAction

Set the on action of the receiver to anAction.


<details>
	<summary>See more</summary>
	
	onAction: anAction 
	"Set the on action of the receiver to anAction."

	onAction _ anAction
</details>

#### Switch>>#switch

Change the state of the receiver from 'on' to 'off' or from 'off' to 'on' (see Switch|turnOn, Switch|turnOff).


<details>
	<summary>See more</summary>
	
	switch
	"Change the state of the receiver from 'on' to 'off' or from 'off' to 'on' (see 
	Switch|turnOn, Switch|turnOff)."

	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]
</details>

#### Switch>>#initializeOn

<details>
	<summary>See more</summary>
	
	initializeOn

	on _ true. 
	onAction _ nil.
	offAction _ nil
</details>

#### Switch>>#clear

Set the state of the receiver to 'off'. If the state of the receiver was previously 'on', then 'self change' is sent. The receiver's off action is NOT executed.


<details>
	<summary>See more</summary>
	
	clear
	"Set the state of the receiver to 'off'. If the state of the receiver was 
	previously 'on', then 'self change' is sent. The receiver's off action is 
	NOT executed."

	self isOn
		ifTrue: 
			[on _ false.
			self changed: self]
</details>

#### Switch>>#isOff

Answer whether the receiver is set off or not.


<details>
	<summary>See more</summary>
	
	isOff
	"Answer whether the receiver is set off or not."

	^on not
</details>

