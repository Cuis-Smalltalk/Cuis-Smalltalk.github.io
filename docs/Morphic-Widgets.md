## ClassNameRequestMorph

Allows use of the SmalltalkCompleter to find class names

### Methods
#### ClassNameRequestMorph>>#classOfThisContext

<details>
	<summary>See more</summary>
	
	classOfThisContext

	^ MethodContext
</details>

#### ClassNameRequestMorph>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### ClassNameRequestMorph>>#computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### ClassNameRequestMorph>>#classOfWorkspaceVarNamed: aName

<details>
	<summary>See more</summary>
	
	classOfWorkspaceVarNamed: aName

	^ nil
</details>

#### ClassNameRequestMorph>>#computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### ClassNameRequestMorph>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	
	^ClassNameCompleter 
</details>

#### ClassNameRequestMorph>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

## FillInTheBlankMorph

A simple dialog with an entry field and accept / cancel buttons.

### Methods
#### FillInTheBlankMorph>>#response: aText

Sent when text pane accepts.


<details>
	<summary>See more</summary>
	
	response: aText
	"Sent when text pane accepts."

	response _ aText asString.
	done _ true.
	^ true

</details>

#### FillInTheBlankMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	extent _ `20@10` * self sizeUnit.
	responseUponCancel _ ''
</details>

#### FillInTheBlankMorph>>#getUserResponse

Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels.


<details>
	<summary>See more</summary>
	
	getUserResponse
	"Wait for the user to accept or cancel, and answer the result string. Answers the empty string if the user cancels."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop."

	| w delay |
	w _ self world.
	w isNil ifTrue: [^ response].
	done _ false.
	textPane focusText.
	delay _ Delay forMilliseconds: 10.
	[done] whileFalse: [ w doOneMinimalCycleNow. delay wait. ].
	self delete.
	w doOneMinimalCycleNow.
	^ response

</details>

#### FillInTheBlankMorph>>#responseUponCancel: anObject

<details>
	<summary>See more</summary>
	
	responseUponCancel: anObject
	responseUponCancel _ anObject

</details>

#### FillInTheBlankMorph>>#setQuery: queryString initialAnswer: initialAnswer acceptOnCR: acceptBoolean

<details>
	<summary>See more</summary>
	
	setQuery: queryString initialAnswer: initialAnswer acceptOnCR: acceptBoolean 
	response := initialAnswer.
	done := false.
	self removeAllMorphs.
	self createQueryTextMorph: queryString.
	self createAcceptButton.
	self createCancelButton.
	textPane := self createTextPaneAcceptOnCR: acceptBoolean
</details>

#### FillInTheBlankMorph>>#acceptClicked

Sent by the accept button.


<details>
	<summary>See more</summary>
	
	acceptClicked
	"Sent by the accept button."

	textPane textMorph acceptContents
</details>

#### FillInTheBlankMorph>>#cancelClicked

Sent by the cancel button.


<details>
	<summary>See more</summary>
	
	cancelClicked
	"Sent by the cancel button."

	response _ responseUponCancel.
	done _ true.

</details>

#### FillInTheBlankMorph>>#createAcceptButton

create the [accept] button


<details>
	<summary>See more</summary>
	
	createAcceptButton
	"create the [accept] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current acceptButton;
		 label: 'Accept';
		 action: #acceptClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `2@7.5` * self sizeUnit // 1.
	^ result
</details>

#### FillInTheBlankMorph>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete

	self breakDependents.
	super delete.
</details>

#### FillInTheBlankMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current menu
</details>

#### FillInTheBlankMorph>>#createQueryTextMorph: queryString

create the queryTextMorph


<details>
	<summary>See more</summary>
	
	createQueryTextMorph: queryString 
	"create the queryTextMorph"
	| result |
	result _ StringMorph new contents: queryString.
	result lock.
	result morphExtent: `24@2` * self sizeUnit.
	self addMorph: result position: `2@0.5` * self sizeUnit // 1.
	^ result
</details>

#### FillInTheBlankMorph>>#emptyTextDisplayMessage: aString

<details>
	<summary>See more</summary>
	
	emptyTextDisplayMessage: aString
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.
</details>

#### FillInTheBlankMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	Theme current roundWindowCorners
		ifTrue: [
			aCanvas
				roundRect: self morphLocalBounds
				color: color
				radius: Theme current roundedWindowRadius ]
		ifFalse: [ super drawOn: aCanvas ]
</details>

#### FillInTheBlankMorph>>#selectionInterval

<details>
	<summary>See more</summary>
	
	selectionInterval
	^ 1 to: response size

</details>

#### FillInTheBlankMorph>>#createCancelButton

create the [cancel] button


<details>
	<summary>See more</summary>
	
	createCancelButton
	"create the [cancel] button"
	| result |
	result _ PluggableButtonMorph new
		 model: self;
		 color: Theme current cancelButton;
		 label: 'Cancel';
		 action: #cancelClicked.
	result morphExtent: `6@2` * self sizeUnit.
	self addMorph: result position: `12@7.5` * self sizeUnit // 1.
	^ result
</details>

#### FillInTheBlankMorph>>#createTextPaneAcceptOnCR: acceptBoolean

create the textPane


<details>
	<summary>See more</summary>
	
	createTextPaneAcceptOnCR: acceptBoolean
	"create the textPane"
	| result |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	result _ (TextModelMorph
		textProvider: self
		textGetter: #response
		textSetter: #response:
		selectionGetter: #selectionInterval) emptyTextDisplayMessage: 'Enter response'.
	self
		valueOfProperty: #emptyTextDisplayMessage
		ifPresentDo: [ :msg |
			result emptyTextDisplayMessage: msg ].
	result
		hasUnacceptedEdits: true;
		acceptOnCR: acceptBoolean;
		escAction: [ self cancelClicked ];
		morphExtent: `18 @ 5` * self sizeUnit.
	self
		addMorph: result
		position: `1 @ 2` * self sizeUnit.
	^ result.
</details>

#### FillInTheBlankMorph>>#response

<details>
	<summary>See more</summary>
	
	response

	^ response

</details>

#### FillInTheBlankMorph>>#sizeUnit

<details>
	<summary>See more</summary>
	
	sizeUnit
	^AbstractFont default lineSpacing
</details>

## FontPicker

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## HandleMorph

A HandleMorph provides mouse-up control behavior.

### Methods
#### HandleMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.
	extent _ `12@12`
</details>

#### HandleMorph>>#stepAt: millisecondSinceLast

Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The desired time between steps is specified by this morph's answer to the stepTime message. The millisecondSinceLast parameter gives the time elapsed since the previous step.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	pointBlock value: self morphBoundsInWorld center
</details>

#### HandleMorph>>#keyStroke: aKeyboardEvent

Check for cursor keys


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent
	"Check for cursor keys"
	| keyValue |
	(owner is: #HandMorph) ifFalse: [ ^self ].
	keyValue _ aKeyboardEvent keyValue.
	keyValue = 28 ifTrue: [ ^self morphPosition: self morphPosition - `1@0` ].
	keyValue = 29 ifTrue: [ ^self morphPosition: self morphPosition + `1@0` ].
	keyValue = 30 ifTrue: [ ^self morphPosition: self morphPosition - `0@1` ].
	keyValue = 31 ifTrue: [ ^self morphPosition: self morphPosition + `0@1` ].
	"Special case for return"
	aKeyboardEvent isReturnKey ifTrue:[
		"Drop the receiver and be done"
	self flag: #arNote. "Probably unnecessary"
		owner releaseKeyboardFocus: self.
		self delete ]
</details>

#### HandleMorph>>#forEachPointDo: aBlock

<details>
	<summary>See more</summary>
	
	forEachPointDo: aBlock
	pointBlock _ aBlock
</details>

#### HandleMorph>>#stepTime

Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!


<details>
	<summary>See more</summary>
	
	stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!"
	^ 20
</details>

## HoverHelpMorph

A balloon with text used for the display of explanatory information.

### Methods
#### HoverHelpMorph>>#contents: aString

<details>
	<summary>See more</summary>
	
	contents: aString
	contents _ aString.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 9999999@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8
</details>

#### HoverHelpMorph>>#popUpForHand: aHand

Pop up the receiver as balloon help for the given hand


<details>
	<summary>See more</summary>
	
	popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [ ^self ].
	x _ aHand morphPosition x - 20.
	y _ aHand morphPosition y + 20.
	x + self morphWidth > aHand world morphWidth ifTrue: [
		x _ aHand world morphWidth - self morphWidth ].
	y + self morphHeight > aHand world morphHeight ifTrue: [
		y _ aHand morphPosition y - self morphHeight - 12 ].
	newPos _ x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self
</details>

#### HoverHelpMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor

	^ `Color r: 1.0 g: 1.0 b: 0.7`
</details>

#### HoverHelpMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	| r |
	r _ self morphLocalBounds.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		textComposition: textComposition
		bounds: (r insetBy: 4)
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false)
</details>

## ProgressBarMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ProgressBarMorph>>#progressValue

<details>
	<summary>See more</summary>
	
	progressValue
	^value
</details>

#### ProgressBarMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	progressColor _ `Color gray`.
	value _ 0.0
</details>

#### ProgressBarMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ `Color white`
</details>

#### ProgressBarMorph>>#progressValue: aValue

<details>
	<summary>See more</summary>
	
	progressValue: aValue
	value _ aValue.
	self redrawNeeded
</details>

#### ProgressBarMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	| twoBorders |
	super drawOn: aCanvas.
	twoBorders _ borderWidth + borderWidth.
	aCanvas
		fillRectangle: (borderWidth @ borderWidth extent: extent x * value @ extent y - twoBorders)
		color: progressColor
</details>

#### ProgressBarMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1
</details>

## ProgressMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ProgressMorph>>#subLabel: aString

<details>
	<summary>See more</summary>
	
	subLabel: aString
	self whenUIinSafeState: [
		subLabelMorph contents: aString.
		self updatePositionAndExtent. ]
</details>

#### ProgressMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	self separation: 0.
	labelMorph _ StringMorph contents: '' font: AbstractFont default.
	subLabelMorph _ StringMorph contents: '' font: AbstractFont default.
	progress _ ProgressBarMorph new.
	progress morphExtent: 200 @ AbstractFont default lineSpacing.
	self addMorphFront: labelMorph.
	self addMorphFront: subLabelMorph.
	self addMorph: progress fixedHeight: AbstractFont default lineSpacing
</details>

#### ProgressMorph>>#incrDone: incrDone

<details>
	<summary>See more</summary>
	
	incrDone: incrDone
	self done: self done + incrDone
</details>

#### ProgressMorph>>#label: aString subLabel: otherString

<details>
	<summary>See more</summary>
	
	label: aString subLabel: otherString
	self whenUIinSafeState: [
		labelMorph contents: aString.
		subLabelMorph contents: otherString.
		self updatePositionAndExtent. ]
</details>

#### ProgressMorph>>#updatePositionAndExtent

<details>
	<summary>See more</summary>
	
	updatePositionAndExtent
	| w newExtent |
	w _ ((labelMorph measureContents x max: subLabelMorph measureContents x) max: 200) + 18.
	w _ w min: Display extent x.
	newExtent _ w > extent x
		ifTrue: [ w@(labelMorph morphHeight + subLabelMorph morphHeight + progress morphHeight + 10) ]
		ifFalse: [ extent ].
	self world
		ifNotNil: [ :world | self morphPosition: world morphExtent - newExtent // 2 extent: newExtent ]
		ifNil: [ self morphExtent: newExtent ]
</details>

#### ProgressMorph>>#done: amountDone

<details>
	<summary>See more</summary>
	
	done: amountDone
	progress progressValue: ((amountDone min: 1.0) max: 0.0)
</details>

#### ProgressMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ `Color veryLightGray`
</details>

#### ProgressMorph>>#done

<details>
	<summary>See more</summary>
	
	done
	^progress progressValue
</details>

#### ProgressMorph>>#label: aString

<details>
	<summary>See more</summary>
	
	label: aString
	self label: aString subLabel: ''
</details>

#### ProgressMorph>>#openInWorld: aWorld

This msg and its callees result in the window being activeOnlyOnTop


<details>
	<summary>See more</summary>
	
	openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	aWorld addMorph: self.
	self updatePositionAndExtent.
	labelMorph fitContents.
	subLabelMorph fitContents.
	layoutNeeded _ true.
</details>

## TranscriptMorph

Provide a view for integrating Transcript into the Morphic World.

### Methods
#### TranscriptMorph>>#editContents

<details>
	<summary>See more</summary>
	
	editContents
	workspace _ Workspace new.
	workspace shouldStyle ifTrue: [ workspace toggleStyling ].
	workspace contents: Transcript contents.
	lastUnfinishedEntrySize _ Transcript unfinishedEntrySize.
	workspaceWindow _ workspace openLabel: 'Workspace - Transcript contents'.
	lastIncludedIndex _ Transcript lastIndex.
</details>

#### TranscriptMorph>>#clearInternal

<details>
	<summary>See more</summary>
	
	clearInternal
	Transcript clearInternal
</details>

#### TranscriptMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	doImmediateUpdates _ true
</details>

#### TranscriptMorph>>#privateExtent: aPoint

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			(form isNil or: [ form extent ~= aPoint ]) ifTrue: [
				form _ Form extent: aPoint depth: Display depth ]]; yourself
</details>

#### TranscriptMorph>>#doImmediateUpdates

<details>
	<summary>See more</summary>
	
	doImmediateUpdates
	doImmediateUpdates _ true.
	Transcript 
		showOnDisplay: doImmediateUpdates
</details>

#### TranscriptMorph>>#getMenu

Set up the menu to apply to the receiver


<details>
	<summary>See more</summary>
	
	getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu add: 'Only update in the regular Morphic cycle' 	action: #doRegularUpdates ]
		ifFalse: [ aMenu add: 'Immediately show each entry'	 				action: #doImmediateUpdates ].
	aMenu
		addLine;
		add: 'Workspace with Contents' 					action: #editContents;
		addLine;
		add: 'Clear Transcript' 							action: #clearInternal;
		add: 'Clear Transcript File' 						action: #clearFile;
		add: 'Clear Transcript Stdout' 					action: #clearStdout;
		add: 'Clear Both' 									action: #clearAll;
		addLine.
	Transcript logsToFile
		ifTrue: [ aMenu add: 'Stop logging to File'		action: #dontLogToFile ]
		ifFalse: [ aMenu add: 'Start logging to File'	action: #logToFile ].
	aMenu addLine.
	Transcript logsToStdout
		ifTrue:  [ aMenu add: 'Stop logging to Stdout'	action: #dontLogToStdout ]
		ifFalse: [ aMenu add: 'Start logging to Stdout'	action: #logToStdout  ].
	
	^ aMenu
</details>

#### TranscriptMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^ true
</details>

#### TranscriptMorph>>#doRegularUpdates

<details>
	<summary>See more</summary>
	
	doRegularUpdates
	doImmediateUpdates _ false.
	Transcript 
		showOnDisplay: doImmediateUpdates
</details>

#### TranscriptMorph>>#logToStdout

<details>
	<summary>See more</summary>
	
	logToStdout

	Transcript logToStdout: true
</details>

#### TranscriptMorph>>#dontLogToFile

<details>
	<summary>See more</summary>
	
	dontLogToFile
	Transcript logToFile: false
</details>

#### TranscriptMorph>>#clearFile

<details>
	<summary>See more</summary>
	
	clearFile
	Transcript clearFile
</details>

#### TranscriptMorph>>#logToFile

<details>
	<summary>See more</summary>
	
	logToFile
	Transcript logToFile: true
</details>

#### TranscriptMorph>>#clearStdout

<details>
	<summary>See more</summary>
	
	clearStdout

	Transcript clearStdout
</details>

#### TranscriptMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	Transcript
		showOnDisplay: true;
		displayOn: form in: self morphLocalBounds.
	aCanvas image: form at: self morphTopLeft.
	Transcript
		bounds: self morphBoundsInWorld;
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace
</details>

#### TranscriptMorph>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity

	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]
</details>

#### TranscriptMorph>>#updateWorkspace

<details>
	<summary>See more</summary>
	
	updateWorkspace
	| newContents newLastIndex newLastUnfinishedEntrySize |
	workspaceWindow ifNotNil: [
		workspaceWindow owner notNil
			ifTrue: [
				newLastIndex _ Transcript lastIndex.
				newLastIndex = lastIncludedIndex ifFalse: [
					newContents _ Transcript contentsStartingAt: lastIncludedIndex+1.
					newLastUnfinishedEntrySize _ Transcript unfinishedEntrySize.
					 lastUnfinishedEntrySize  > 0 ifTrue: [
						newContents _ newContents copyFrom: lastUnfinishedEntrySize+1 to: newContents size.].
					workspace actualContents: workspace actualContents, newContents.
					lastUnfinishedEntrySize _ newLastUnfinishedEntrySize ].
					lastIncludedIndex _ newLastIndex. ]
			ifFalse: [ workspace _ lastIncludedIndex _ nil ]]
</details>

#### TranscriptMorph>>#clearAll

<details>
	<summary>See more</summary>
	
	clearAll
	Transcript clearAll
</details>

#### TranscriptMorph>>#dontLogToStdout

<details>
	<summary>See more</summary>
	
	dontLogToStdout

	Transcript logToStdout: false
</details>

## UpdatingStringMorph

UpdatingStringMorph new target: [self runningWorld activeHand morphPosition asString]; getSelector: #value; stepTime: 10; openInWorld

### Methods
#### UpdatingStringMorph>>#getSelector: aSymbol

<details>
	<summary>See more</summary>
	
	getSelector: aSymbol
	getSelector _ aSymbol
</details>

#### UpdatingStringMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	target _ self.
	getSelector _ #contents.
	stepTime _ 50
</details>

#### UpdatingStringMorph>>#showPrintStringFor: anObject

<details>
	<summary>See more</summary>
	
	showPrintStringFor: anObject

	self contents: anObject printString
</details>

#### UpdatingStringMorph>>#stepTime: aNumber

<details>
	<summary>See more</summary>
	
	stepTime: aNumber
	stepTime _ aNumber
</details>

#### UpdatingStringMorph>>#stepAt: millisecondSinceLast

Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The desired time between steps is specified by this morph's answer to the stepTime message. The millisecondSinceLast parameter gives the time elapsed since the previous step.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	self contents: (target perform: getSelector) asString
</details>

#### UpdatingStringMorph>>#fitContents

Don't shrink each time contents change. Might shrink during layout


<details>
	<summary>See more</summary>
	
	fitContents
	"Don't shrink each time contents change.
	Might shrink during layout"
	self morphExtent: (extent max: self measureContents)
</details>

#### UpdatingStringMorph>>#target: anObject

<details>
	<summary>See more</summary>
	
	target: anObject
	target _ anObject
</details>

#### UpdatingStringMorph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime

	^stepTime
</details>

#### UpdatingStringMorph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run"

	^true
</details>

