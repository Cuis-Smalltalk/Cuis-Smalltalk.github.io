## DropFilesAction

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DropFilesAction>>#performService: aService

<details>
	<summary>See more</summary>
	
	performService: aService

	aService 
		argumentProvider: self;
		performService 

</details>

#### DropFilesAction>>#selectedFileEntry

<details>
	<summary>See more</summary>
	
	selectedFileEntry
	
	^selectedFileEntry
</details>

#### DropFilesAction>>#value

<details>
	<summary>See more</summary>
	
	value

	stopHereBlock := [ ^self ].
	dropFilesEvent fileNamesDo: [ :fileName | self fileNamedDropped: fileName ]
		

</details>

#### DropFilesAction>>#initializeFor: aDropFilesEvent

<details>
	<summary>See more</summary>
	
	initializeFor: aDropFilesEvent 

	dropFilesEvent := aDropFilesEvent.
	shouldAskToStop := aDropFilesEvent numberOfFiles > 1
</details>

#### DropFilesAction>>#fileNamedDropped: aFileName

<details>
	<summary>See more</summary>
	
	fileNamedDropped: aFileName 

	| options selectionIndex menu |
	
	selectedFileEntry := aFileName asFileEntry.
	options := FileList itemsForFileEntry: selectedFileEntry.
	options isEmpty ifTrue: [ ^self inform: 'No action found for ', selectedFileEntry name ].
	menu := self createMenuFor: options.
		
	selectionIndex := menu startUpWithCaption: 'Select action for ', selectedFileEntry name.
	
	selectionIndex = 0 ifTrue: [ ^self ].
	(options isInBounds: selectionIndex) ifTrue: [ ^self performService: (options at: selectionIndex) ].
	"The only available option is 'stop here'. This could change if #createMenuFor: changes - Hernan"
	stopHereBlock value
	

</details>

#### DropFilesAction>>#createMenuFor: options

options is a small collection, no problem to collect twice - Hernan


<details>
	<summary>See more</summary>
	
	createMenuFor: options

	| icons lines labels |

	"options is a small collection, no problem to collect twice - Hernan"	
	labels := options collect: [ :option | option label ].
	icons := options collect: [ :option | option iconSpec ].

	shouldAskToStop 
		ifTrue: [
			lines := Array with: labels size.
			labels add: 'stop here'.
			icons add: #cancelIcon ]
		ifFalse: [ lines := #() ].
		
	^PopUpMenu labelArray: labels lines: lines icons: icons
</details>

#### DropFilesAction>>#fullName

<details>
	<summary>See more</summary>
	
	fullName
	
	^selectedFileEntry name 
</details>

## PasteUpMorph

A World, the entire Smalltalk screen, is a PasteUpMorph. A World responds true to isWorld. A PasteUpMorph that is a world, builds its menu in HandMorph buildWorldMenu. worldState If I am also a World, keeps the hands, damageRecorder, stepList etc.

### Methods
#### PasteUpMorph>>#backgroundImage

<details>
	<summary>See more</summary>
	
	backgroundImage
	^backgroundImage
</details>

#### PasteUpMorph>>#acceptDroppingMorph: aMorph event: evt

The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied


<details>
	<summary>See more</summary>
	
	acceptDroppingMorph: aMorph event: evt 
	"The supplied morph, known to be acceptable to the receiver, is now to be assimilated; the precipitating event is supplied"

	self isWorldMorph 
		ifTrue: [	
			"Add the given morph to this world and start stepping it if it wants to be."
			self addMorphFront: aMorph.
			(aMorph morphFullBoundsInWorld intersects: self viewBox) 
				ifFalse: [
					Smalltalk beep.
					aMorph morphPosition: extent // 2]]
		ifFalse: [super acceptDroppingMorph: aMorph event: evt].
	aMorph submorphsDo: [ :m | (m is: #HaloMorph) ifTrue: [ m delete ]]
</details>

#### PasteUpMorph>>#wantsWindowEvent: anEvent

<details>
	<summary>See more</summary>
	
	wantsWindowEvent: anEvent
	^self isWorldMorph or: [self windowEventHandler notNil]
</details>

#### PasteUpMorph>>#findATranscript: evt

Locate a transcript, open it, and bring it to the front. Create one if necessary


<details>
	<summary>See more</summary>
	
	findATranscript: evt
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript
</details>

#### PasteUpMorph>>#dropFiles: aDropFilesEvent

I do nothing, subclasses should redefine if they handle this event - Hernan


<details>
	<summary>See more</summary>
	
	dropFiles: aDropFilesEvent

	(DropFilesAction for: aDropFilesEvent) value
</details>

#### PasteUpMorph>>#addMorph: aMorph centeredNear: aPoint

Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self morphBoundsInWorld.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.
</details>

#### PasteUpMorph>>#privateExtent: newExtent

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: newExtent

	^ (super privateExtent: newExtent)
		ifTrue: [
			self buildMagnifiedBackgroundImage.
			worldState ifNotNil: [
				worldState clearCanvas ]];
		yourself
</details>

#### PasteUpMorph>>#doOneCycleNow

see the comment in WorldState >> doOneCycleNow Only used for a few tests.


<details>
	<summary>See more</summary>
	
	doOneCycleNow
	"see the comment in WorldState >> doOneCycleNow
	Only used for a few tests."
	worldState doOneCycleNow
</details>

#### PasteUpMorph>>#startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime

<details>
	<summary>See more</summary>
	
	startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime
	worldState startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTime.
</details>

#### PasteUpMorph>>#deleteAllHalos

<details>
	<summary>See more</summary>
	
	deleteAllHalos
	self haloMorphs do:
		[ :m | m delete]
</details>

#### PasteUpMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.8
		g: 1.0
		b: 0.6`
</details>

#### PasteUpMorph>>#wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph

Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)


<details>
	<summary>See more</summary>
	
	wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"
	(#(addHelpHandle: addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].
	
	self isWorldMorph ifFalse: [
		^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph ].

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector
</details>

#### PasteUpMorph>>#printOn: aStream

Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	self isWorldMorph
		ifTrue: [aStream nextPutAll: ' [world]']
		ifFalse: [super printOn: aStream]
</details>

#### PasteUpMorph>>#keyStroke: aKeyboardEvent

Handle a keystroke event.


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ BrowserWindow openBrowser ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent
</details>

#### PasteUpMorph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime

	(self isWorldMorph and: [owner notNil]) ifTrue: [
		^1
	].
	^super stepTime
</details>

#### PasteUpMorph>>#windowEvent: aMorphicEvent

Host window event


<details>
	<summary>See more</summary>
	
	windowEvent: aMorphicEvent
	self windowEventHandler
		ifNotNil: [^self windowEventHandler windowEvent: aMorphicEvent].

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]

</details>

#### PasteUpMorph>>#mouseButton2Activity

This method may be redefined, for example, to open a pop-up menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity

	^self invokeWorldMenu
</details>

#### PasteUpMorph>>#restoreAll

Expand all windows to their previous state


<details>
	<summary>See more</summary>
	
	restoreAll
	"Expand all windows to their previous state"
	taskbar
		ifNotNil: [ :tb | tb restoreAll ]
</details>

#### PasteUpMorph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME. Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message. jmv: Not really sure. Sub-world stepping needs some review.


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message.
	jmv: Not really sure. Sub-world stepping needs some review."

	^true
</details>

#### PasteUpMorph>>#clearCanvas

<details>
	<summary>See more</summary>
	
	clearCanvas
	worldState clearCanvas
</details>

#### PasteUpMorph>>#deleteBalloonTarget: aMorph

Delete the balloon help targeting the given morph


<details>
	<summary>See more</summary>
	
	deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"
	self handsDo:[:h| h deleteBalloonTarget: aMorph].
</details>

#### PasteUpMorph>>#displayWorldSafely

<details>
	<summary>See more</summary>
	
	displayWorldSafely

	worldState displayWorldSafely

</details>

#### PasteUpMorph>>#firstHand

<details>
	<summary>See more</summary>
	
	firstHand

	^ worldState hands first
</details>

#### PasteUpMorph>>#taskbar

<details>
	<summary>See more</summary>
	
	taskbar
	^taskbar
</details>

#### PasteUpMorph>>#tearDownDesktop

<details>
	<summary>See more</summary>
	
	tearDownDesktop
	self whenUIinSafeState: [
		self hideTaskbar.
		submorphs 
			do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]]].
</details>

#### PasteUpMorph>>#removeAllKnownFailing

<details>
	<summary>See more</summary>
	
	removeAllKnownFailing
	worldState removeAllKnownFailing.
	self redrawNeeded
</details>

#### PasteUpMorph>>#closeUnchangedWindows

Present a menu of window titles for all windows with changes, and activate the one that gets chosen.


<details>
	<summary>See more</summary>
	
	closeUnchangedWindows
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(SelectionMenu confirm: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]
</details>

#### PasteUpMorph>>#handsReverseDo: aBlock

<details>
	<summary>See more</summary>
	
	handsReverseDo: aBlock

	^ worldState ifNotNil: [ worldState handsReverseDo: aBlock ]
</details>

#### PasteUpMorph>>#setCanvas: aCanvas

<details>
	<summary>See more</summary>
	
	setCanvas: aCanvas

	^ worldState ifNotNil: [ worldState setCanvas: aCanvas ]
</details>

#### PasteUpMorph>>#click: aMouseButtonEvent localPosition: localEventPosition

Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	click: aMouseButtonEvent localPosition: localEventPosition
	^self whenUIinSafeState: [self mouseButton2Activity]
</details>

#### PasteUpMorph>>#whenUIinSafeState: evaluableObject

Please call UISupervisor whenUIinSafeState: evaluableObject


<details>
	<summary>See more</summary>
	
	whenUIinSafeState: evaluableObject
	"Please call
	UISupervisor whenUIinSafeState: evaluableObject
	"
	WorldState addDeferredUIMessage: evaluableObject 
</details>

#### PasteUpMorph>>#redrawNeeded

Report that the area occupied by this morph should be redrawn.


<details>
	<summary>See more</summary>
	
	redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

        self == self world 
                ifTrue: [worldState doFullRepaint]
                ifFalse: [super redrawNeeded]

</details>

#### PasteUpMorph>>#allMorphsDo: aBlock

Enumerate all morphs in the world, including those held in hands.


<details>
	<summary>See more</summary>
	
	allMorphsDo: aBlock
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	self isWorldMorph
		ifTrue: [worldState handsReverseDo: [:h | h allMorphsDo: aBlock]].

</details>

#### PasteUpMorph>>#allNonWindowRelatedSubmorphs

Answer all non-window submorphs that are not flap-related


<details>
	<summary>See more</summary>
	
	allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]
</details>

#### PasteUpMorph>>#buildMagnifiedBackgroundImage

<details>
	<summary>See more</summary>
	
	buildMagnifiedBackgroundImage
	| image old |
	old _ backgroundImage.
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [ 
			[image _ Form fromBinaryStream: backgroundImageData readStream.
			backgroundImage _ image magnifyTo: extent.
			self canvas ifNotNil: [ :c |
				(backgroundImage depth = 32 and: [ c depth < 32 ]) ifTrue: [
					backgroundImage _ backgroundImage orderedDither32To16 ]]
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
		].
	old == backgroundImage ifFalse: [
		self redrawNeeded ]
</details>

#### PasteUpMorph>>#taskbarDeleted

<details>
	<summary>See more</summary>
	
	taskbarDeleted
	taskbar _ nil
</details>

#### PasteUpMorph>>#removeAlarm: aSelector for: aTarget

Remove the alarm with the given selector


<details>
	<summary>See more</summary>
	
	removeAlarm: aSelector for: aTarget
	"Remove the alarm with the given selector"
	worldState removeAlarm: aSelector for: aTarget
</details>

#### PasteUpMorph>>#invalidateDisplayRect: damageRect from: aMorph

Clip damage reports to my bounds, since drawing is clipped to my bounds.


<details>
	<summary>See more</summary>
	
	invalidateDisplayRect: damageRect from: aMorph
        "Clip damage reports to my bounds, since drawing is clipped to my bounds."

        self == self world 
                ifTrue: [ worldState recordDamagedRect: (damageRect intersect: self morphLocalBounds ) ]
                ifFalse: [ super invalidateDisplayRect: damageRect from: aMorph ]
</details>

#### PasteUpMorph>>#runProcess

<details>
	<summary>See more</summary>
	
	runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	
	^ process
</details>

#### PasteUpMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil
</details>

#### PasteUpMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1
</details>

#### PasteUpMorph>>#hands

<details>
	<summary>See more</summary>
	
	hands

	^ worldState hands
</details>

#### PasteUpMorph>>#activeHand

Answer the currently active hand, if any...


<details>
	<summary>See more</summary>
	
	activeHand
	"Answer the currently active hand, if any..."
	^worldState
		ifNotNil: [ :ws | ws activeHand ]
		ifNil: [ self world ifNotNil: [ :w | w activeHand ]]
</details>

#### PasteUpMorph>>#findDirtyWindows: evt

Present a menu of window titles for all windows with changes, and activate the one that gets chosen.


<details>
	<summary>See more</summary>
	
	findDirtyWindows: evt
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits not ]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]
</details>

#### PasteUpMorph>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp
</details>

#### PasteUpMorph>>#hideTaskbar

<details>
	<summary>See more</summary>
	
	hideTaskbar
	taskbar ifNotNil: [
		taskbar delete.
		taskbar _ nil ]
</details>

#### PasteUpMorph>>#doOneMinimalCycleNow

see the comment in WorldState >> doOneMinimalCycleNow


<details>
	<summary>See more</summary>
	
	doOneMinimalCycleNow
	"see the comment in WorldState >> doOneMinimalCycleNow"

	worldState doOneMinimalCycleNow
</details>

#### PasteUpMorph>>#backgroundImageData: aByteArray

| filename | filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'. filename _ 'bg/free-desktop-wallpaper.jpg'. filename _ 'bg/jellyfish-thumb.jpg'. filename _ 'bg/splash_by_beefpepsi.jpg'. filename _ 'bg/gray ocean and pier.jpg'. filename _ 'bg/newyork.jpg'. filename _ 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'. filename _ 'bg/desktop-wallpaper-tropical-1280x1024.jpg'. filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'. self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.


<details>
	<summary>See more</summary>
	
	backgroundImageData: aByteArray
	"
	| filename |
	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename _ 'bg/free-desktop-wallpaper.jpg'.
	filename _ 'bg/jellyfish-thumb.jpg'.
	filename _ 'bg/splash_by_beefpepsi.jpg'.
	filename _ 'bg/gray ocean and pier.jpg'.
	filename _ 'bg/newyork.jpg'.
	filename _ 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename _ 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.
	"
	backgroundImageData _ aByteArray.
	self buildMagnifiedBackgroundImage
</details>

#### PasteUpMorph>>#privateOuterDisplayWorld

<details>
	<summary>See more</summary>
	
	privateOuterDisplayWorld

	worldState displayWorldAndSubmorphs: submorphs

</details>

#### PasteUpMorph>>#mainLoop

<details>
	<summary>See more</summary>
	
	mainLoop

	
	self clearWaitDelay.
	self clearCanvas.
	[
		self doOneCycle.
		Processor yield.
		true ]
			whileTrue: []
</details>

#### PasteUpMorph>>#showTaskbar

<details>
	<summary>See more</summary>
	
	showTaskbar

	taskbar ifNil: [
		taskbar _ TaskbarMorph newRow.
		taskbar openInWorld: self ]
</details>

#### PasteUpMorph>>#drawOn: aCanvas

draw background image.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]
</details>

#### PasteUpMorph>>#defaultBorderColor

answer the default border color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color
		r: 0.861
		g: 1.0
		b: 0.722`
</details>

#### PasteUpMorph>>#externalizeToWorld: aPoint

aPoint is in own coordinates. Answer is in world coordinates.


<details>
	<summary>See more</summary>
	
	externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."
	^self isWorldMorph
		ifTrue: [ aPoint ]
		ifFalse: [ super externalizeToWorld: aPoint ]
</details>

#### PasteUpMorph>>#addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime

Add a new alarm with the given set of parameters


<details>
	<summary>See more</summary>
	
	addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"
	worldState addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime.
</details>

#### PasteUpMorph>>#restoreDisplay

<details>
	<summary>See more</summary>
	
	restoreDisplay
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded
</details>

#### PasteUpMorph>>#removeKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	removeKnownFailing: aMorph
	worldState removeKnownFailing: aMorph
</details>

#### PasteUpMorph>>#internalizeFromWorld: aPoint

aPoint is in World coordinates. Answer is in own coordinates.


<details>
	<summary>See more</summary>
	
	internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."
	^self isWorldMorph
		ifTrue: [ aPoint ]
		ifFalse: [ super internalizeFromWorld: aPoint ]
</details>

#### PasteUpMorph>>#externalizeDisplayBounds: r

All senders of #displayBoundsOfTransformOf: should be rethought...


<details>
	<summary>See more</summary>
	
	externalizeDisplayBounds: r

	^self isWorldMorph
		ifTrue: [ r ]
		ifFalse: [ super externalizeDisplayBounds: r ]
</details>

#### PasteUpMorph>>#clearWaitDelay

<details>
	<summary>See more</summary>
	
	clearWaitDelay
	worldState clearWaitDelay
</details>

#### PasteUpMorph>>#fullRepaintNeeded

<details>
	<summary>See more</summary>
	
	fullRepaintNeeded
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]
</details>

#### PasteUpMorph>>#allowsSubmorphDrag

Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false. Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false.


<details>
	<summary>See more</summary>
	
	allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true
</details>

#### PasteUpMorph>>#releaseCachedState

Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'.


<details>
	<summary>See more</summary>
	
	releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.
	self isWorldMorph ifTrue: [
		worldState cleanseStepList.
		worldState clearCanvas ]
</details>

#### PasteUpMorph>>#handlesKeyboard

Return true if the receiver wishes to handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard

	^ true 
</details>

#### PasteUpMorph>>#findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock

Locate a window satisfying a block, open it, and bring it to the front. Create one if necessary, by using the makeBlock


<details>
	<summary>See more</summary>
	
	findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock
	"Locate a window satisfying a block, open it, and bring it to the front.  Create one if necessary, by using the makeBlock"
	| aWindow |
	submorphs do: [ :aMorph |
		(((aWindow _ aMorph) is: #SystemWindow) and: [ qualifyingBlock value: aWindow ]) ifTrue: [
			aWindow isCollapsed ifTrue: [ aWindow expand ].
			aWindow activateAndForceLabelToShow.
			^ self ]].
	"None found, so create one"
	makeBlock value.
</details>

#### PasteUpMorph>>#collapseNonWindows

<details>
	<summary>See more</summary>
	
	collapseNonWindows
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]
</details>

#### PasteUpMorph>>#displayWorld

<details>
	<summary>See more</summary>
	
	displayWorld

	self world privateOuterDisplayWorld

</details>

#### PasteUpMorph>>#invokeWorldMenu

Put up the world menu, triggered by the passed-in event. Perhaps a good place to disable it if needed


<details>
	<summary>See more</summary>
	
	invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self
</details>

#### PasteUpMorph>>#fillRects: rectangleList

For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work.


<details>
	<summary>See more</summary>
	
	fillRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait
</details>

#### PasteUpMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #PasteUpMorph or: [ super is: aSymbol ]
</details>

#### PasteUpMorph>>#allowsMorphDrop

Answer whether we accept dropping morphs. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true
</details>

#### PasteUpMorph>>#windowEventHandler

This is a class variable so it is global to all projects and does not get saved


<details>
	<summary>See more</summary>
	
	windowEventHandler
	"This is a class variable so it is global to all projects and does not get saved"
	^WindowEventHandler
</details>

#### PasteUpMorph>>#haloMorphs

<details>
	<summary>See more</summary>
	
	haloMorphs
	^ self hands collect:[:h| h halo] thenSelect:[:halo| halo notNil]
</details>

#### PasteUpMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^true
</details>

#### PasteUpMorph>>#addCustomMenuItems: menu hand: aHandMorph

Add morph-specific menu itemns to the menu for the hand


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: menu hand: aHandMorph 
	"Add morph-specific menu itemns to the menu for the hand"

	super addCustomMenuItems: menu hand: aHandMorph.
	self isWorldMorph 
		ifTrue: [
			menu 
				add: 'desktop menu...'
				target: self
				action: #invokeWorldMenu].
	menu addLine
</details>

#### PasteUpMorph>>#color: aColor

Set the receiver's color.


<details>
	<summary>See more</summary>
	
	color: aColor
	super color: aColor.
	self backgroundImageData: nil
</details>

#### PasteUpMorph>>#isReallyVisible

Answer true only if all the owner chain is visible (i.e. if we are really visible!)


<details>
	<summary>See more</summary>
	
	isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!)"
	^self visible and: [ self isWorldMorph or: [ owner isReallyVisible ]]
</details>

#### PasteUpMorph>>#morphPositionInWorld

<details>
	<summary>See more</summary>
	
	morphPositionInWorld

	self flag: #jmvVer2. "Solo para evitar los warning por falta de owner... pensar despues este caso"
	self isWorldMorph ifTrue: [ ^ `0@0` ].
	^ super morphPositionInWorld
</details>

#### PasteUpMorph>>#stopSteppingMorph: aMorph

Remove the given morph from the step list.


<details>
	<summary>See more</summary>
	
	stopSteppingMorph: aMorph
	"Remove the given morph from the step list."

	worldState stopSteppingMorph: aMorph

</details>

#### PasteUpMorph>>#findDirtyBrowsers: evt

Present a menu of window titles for browsers with changes, and activate the one that gets chosen.


<details>
	<summary>See more</summary>
	
	findDirtyBrowsers: evt
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model is: #CodeProvider) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]
</details>

#### PasteUpMorph>>#findWindow: evt

Present a menu names of windows and naked morphs, and activate the one that gets chosen. Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo.


<details>
	<summary>See more</summary>
	
	findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self
</details>

#### PasteUpMorph>>#canvas

<details>
	<summary>See more</summary>
	
	canvas

	^ worldState ifNotNil: [ worldState canvas ]
</details>

#### PasteUpMorph>>#findATranscript

Locate a transcript, open it, and bring it to the front. Create one if necessary


<details>
	<summary>See more</summary>
	
	findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ TranscriptWindow openTranscript ]
</details>

#### PasteUpMorph>>#worldState: aWorldState

| w | w _ self runningWorld. w worldState: (w instVarNamed: 'worldState')


<details>
	<summary>See more</summary>
	
	worldState: aWorldState
	"
	| w |
	w _ self runningWorld.
	w worldState: (w instVarNamed: 'worldState')
	"
	worldState _ aWorldState.
	worldState world: self
</details>

#### PasteUpMorph>>#deleteNonWindows

<details>
	<summary>See more</summary>
	
	deleteNonWindows
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]
</details>

#### PasteUpMorph>>#collapseAll

Collapse all windows


<details>
	<summary>See more</summary>
	
	collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse.  self displayWorld].
	self collapseNonWindows
</details>

#### PasteUpMorph>>#findAChangeSorter: evt

Locate a change sorter, open it, and bring it to the front. Create one if necessary


<details>
	<summary>See more</summary>
	
	findAChangeSorter: evt
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]
</details>

#### PasteUpMorph>>#stopStepping: aMorph selector: aSelector

Remove the given morph from the step list.


<details>
	<summary>See more</summary>
	
	stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."

	worldState stopStepping: aMorph selector: aSelector

</details>

#### PasteUpMorph>>#bringWindowsFullOnscreen

Make ever SystemWindow on the desktop be totally on-screen, whenever possible.


<details>
	<summary>See more</summary>
	
	bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]
</details>

#### PasteUpMorph>>#allowsFilesDrop

Answer whether we accept dropping files. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsFilesDrop

	^ true
</details>

#### PasteUpMorph>>#handsDo: aBlock

<details>
	<summary>See more</summary>
	
	handsDo: aBlock

	^ worldState ifNotNil: [ worldState handsDo: aBlock ]
</details>

#### PasteUpMorph>>#isWorldMorph

<details>
	<summary>See more</summary>
	
	isWorldMorph

	^ worldState notNil
</details>

#### PasteUpMorph>>#doOneCycle

see the comment in WorldState >> doOneCycle


<details>
	<summary>See more</summary>
	
	doOneCycle
	"see the comment in WorldState >> doOneCycle"

	worldState doOneCycle
</details>

#### PasteUpMorph>>#removedMorph: aMorph

Notify the receiver that aMorph was just removed from its children


<details>
	<summary>See more</summary>
	
	removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]
</details>

#### PasteUpMorph>>#findAFileList: evt

Locate a file list, open it, and bring it to the front. Create one if necessary, respecting the Preference.


<details>
	<summary>See more</summary>
	
	findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileListWindow openFileList ]
</details>

#### PasteUpMorph>>#addedMorph: aMorph

Notify the receiver that the given morph was just added.


<details>
	<summary>See more</summary>
	
	addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]
</details>

#### PasteUpMorph>>#addKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	addKnownFailing: aMorph
	worldState addKnownFailing: aMorph
</details>

#### PasteUpMorph>>#recreateDefaultDesktop

<details>
	<summary>See more</summary>
	
	recreateDefaultDesktop
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		TranscriptWindow openTranscript
			morphPosition: 5 @ 290;
			morphExtent: 990 @ 400.
		editor _ TextEditor openTextEditor
			morphPosition: 456 @ 10;
			morphExtent: 900 @ 680.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		self showTaskbar.
	].
</details>

#### PasteUpMorph>>#addWorldHaloMenuItemsTo: aMenu hand: aHandMorph

Add standard halo items to the menu, given that the receiver is a World


<details>
	<summary>See more</summary>
	
	addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first printStringLimitedTo: 40})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.

</details>

#### PasteUpMorph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged
	self recreateDefaultDesktop.
	self submorphsDo: [ :m |
		m morphExtent: (m morphExtent max: m minimumExtent).
		m fontPreferenceChanged ]
</details>

#### PasteUpMorph>>#isKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	isKnownFailing: aMorph
	^worldState isKnownFailing: aMorph
</details>

#### PasteUpMorph>>#findAMessageNamesWindow: evt

Locate a MessageNames tool, open it, and bring it to the front. Create one if necessary


<details>
	<summary>See more</summary>
	
	findAMessageNamesWindow: evt
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]
</details>

#### PasteUpMorph>>#world

<details>
	<summary>See more</summary>
	
	world
	worldState ifNil: [^super world].
	^self
</details>

#### PasteUpMorph>>#viewBox

<details>
	<summary>See more</summary>
	
	viewBox

	^ worldState
		ifNotNil: [
			 self morphLocalBounds ]
		ifNil: [
			self world viewBox ]
</details>

## WorldState

The state of a Morphic world. Used as the Model of a PasteUpMorph (World) Display. hands -- Array of HandMorphs (typically only one) representing the Cursor and its event queue. viewBox -- Physical screen size. (Display boundingBox) canvas -- BitBlitCanvas on the DisplayScreen. (Display getCanvas) damageRecorder -- the DamageRecorder for the Display stepList -- a Heap of StepMessage. Each morph which wants stepping has a StepMessage here. See comment in #Morph>>stepAt: lastStepTime -- 'now' as last sampled at step. (Time localMillisecondClock) lastStepMessage -- nil or a selector lastCycleTime -- 'now' as last sampled (Time localMillisecondClock) alarms -- a Heap of MorphicAlarm. See comment in #Morph>>addAlarm:after: An _alarm_ is an action to be done once, vs a _step_ which is cycled. lastAlarm -- 'now' as sampled at last alarm (Time localMillisecondClock). remoteServer -- nil (??Unused??) drawingFailureMorphs -- a WeakIdentitySet of Morphs with drawing failures waitDelay -- a Delay (set in #WorldState>>doOneCycleFor:) pause -- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate. (set in #WorldState>>doOneCycleFor:) lastCycleHadAnyEvent -- a boolean (set in #WorldState>>doOneCycleFor:) activeHand -- a HandMorph world -- a PasteUpMorph

### Methods
#### WorldState>>#addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime

Add a new alarm with the given set of parameters


<details>
	<summary>See more</summary>
	
	addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)
</details>

#### WorldState>>#doFullRepaint

<details>
	<summary>See more</summary>
	
	doFullRepaint

	damageRecorder doFullRepaint

</details>

#### WorldState>>#removeKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	removeKnownFailing: aMorph
	drawingFailingMorphs remove: aMorph
</details>

#### WorldState>>#doOneCycleNow

Immediately do one cycle of the interaction loop.


<details>
	<summary>See more</summary>
	
	doOneCycleNow
	"Immediately do one cycle of the interaction loop."
	"See #eventTickler"
	| hadAnyEvent |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAnyEvent.
</details>

#### WorldState>>#runLocalStepMethods: nowTime

Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world.


<details>
	<summary>See more</summary>
	
	runLocalStepMethods: nowTime
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ stepList first scheduledTime <= nowTime ]] 
		whileTrue: [
			stepMessage _ stepList first.
			(stepMessage receiver shouldGetStepsFrom: world)
				ifFalse: [ stepList removeFirst ]
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					stepMessage rescheduleAfter: nowTime.
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort.
					]
		]
</details>

#### WorldState>>#clearWaitDelay

<details>
	<summary>See more</summary>
	
	clearWaitDelay
	waitDelay ifNotNil: [
		waitDelay unschedule.
		waitDelay _ nil ].
	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.
</details>

#### WorldState>>#adjustAlarmTimes: nowTime

Adjust the alarm times after some clock weirdness (such as image-startup etc)


<details>
	<summary>See more</summary>
	
	adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]
</details>

#### WorldState>>#startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil

Add the given morph to the step list


<details>
	<summary>See more</summary>
	
	startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: stepTimeOrNil)
</details>

#### WorldState>>#selectHandsToDrawForDamage: damageList

Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle.


<details>
	<summary>See more</summary>
	
	selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result hBnds |
	result _ OrderedCollection new.
	hands do: [:h |
		h needsToBeDrawn ifTrue: [
			h hasChanged
				ifTrue: [result add: h]
				ifFalse: [
					hBnds _ h morphFullBoundsInWorld.
					hBnds _ hBnds origin corner: hBnds corner.
					(damageList detect: [:r | r intersects: hBnds] ifNone: nil)
						ifNotNil: [result add: h]]]].
	^ result

</details>

#### WorldState>>#stepListSortBlock

<details>
	<summary>See more</summary>
	
	stepListSortBlock

	^ [ :stepMsg1 :stepMsg2 |
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime ]
</details>

#### WorldState>>#displayWorldAndSubmorphs: submorphs

Update this world's display.


<details>
	<summary>See more</summary>
	
	displayWorldAndSubmorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasSubmorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: world viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	Display forceDamageToScreen: allDamage.

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].
</details>

#### WorldState>>#cleanseStepList

Remove morphs from the step list that are not in this World.


<details>
	<summary>See more</summary>
	
	cleanseStepList
	"Remove morphs from the step list that are not in this World."

	| deletions |
	deletions _ OrderedCollection new.
	stepList do: [ :entry |
		entry receiver world == world ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		stepList remove: entry ].

	deletions _ OrderedCollection new.
	alarms do: [ :entry |
		((entry receiver is: #Morph) and: [ entry receiver world == world ]) ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		alarms remove: entry ]
</details>

#### WorldState>>#simpleDrawInvalidAreasSubmorphs: submorphs

mover todo esto al canvas, no? Y ver que hacer con los argumentos, etc.... Toda esta bananarama!


<details>
	<summary>See more</summary>
	
	simpleDrawInvalidAreasSubmorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.

	"Draw World"
	rectsToRepair do: [ :r |
		world drawOn: (canvas newClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair
</details>

#### WorldState>>#alarmSortBlock

<details>
	<summary>See more</summary>
	
	alarmSortBlock

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]
</details>

#### WorldState>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	activeHand _ HandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	alarms _ Heap sortBlock: self alarmSortBlock.
	lastAlarmTime _ 0.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false
</details>

#### WorldState>>#recordDamagedRect: damageRect

<details>
	<summary>See more</summary>
	
	recordDamagedRect: damageRect

	damageRecorder ifNotNil: [damageRecorder recordInvalidRect: damageRect]

</details>

#### WorldState>>#clearCanvas

<details>
	<summary>See more</summary>
	
	clearCanvas
	canvas _ nil.
	damageRecorder _ DamageRecorder new.
	damageRecorder doFullRepaint
</details>

#### WorldState>>#runStepMethods

Perform periodic activity inbetween event cycles


<details>
	<summary>See more</summary>
	
	runStepMethods
	"Perform periodic activity inbetween event cycles"
	| queue readyToProcess |

	queue _ self class deferredUIMessages.
	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!"
	readyToProcess _ queue size.
	readyToProcess timesRepeat: [
		queue nextOrNil ifNotNil: [ :block |
			block value
		]
	].
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."

</details>

#### WorldState>>#displayWorldSafely

Update this world's display and keep track of errors during draw methods.


<details>
	<summary>See more</summary>
	
	displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[world displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx _ thisContext.
		[
			errCtx _ errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and: [ (errCtx receiver is: #Morph) not ]] 
				whileTrue:[errCtx _ errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph _ errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph isKnownFailing
		] whileTrue.
		errMorph drawingFails.
		self clearCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]
</details>

#### WorldState>>#world: aPasteUpMorph

<details>
	<summary>See more</summary>
	
	world: aPasteUpMorph

	world _ aPasteUpMorph
</details>

#### WorldState>>#removeAllKnownFailing

<details>
	<summary>See more</summary>
	
	removeAllKnownFailing
	drawingFailingMorphs _ WeakIdentitySet new.
</details>

#### WorldState>>#handsReverseDo: aBlock

<details>
	<summary>See more</summary>
	
	handsReverseDo: aBlock

	^ hands reverseDo: aBlock
</details>

#### WorldState>>#handleFatalDrawingError: errMsg

Handle a fatal drawing error.


<details>
	<summary>See more</summary>
	
	handleFatalDrawingError: errMsg
	"Handle a fatal drawing error."

	self primitiveError: 
		'Fatal Morphic drawing error', String newLineString,
		errMsg.

	"Hm... we should jump into a 'safe' worldState here, but how do we find it?!"
</details>

#### WorldState>>#stopSteppingMorph: aMorph

Remove the given morph from the step list.


<details>
	<summary>See more</summary>
	
	stopSteppingMorph: aMorph
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])
</details>

#### WorldState>>#setCanvas: aMorphicCanvas

<details>
	<summary>See more</summary>
	
	setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas into: world.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].
	damageRecorder doFullRepaint
</details>

#### WorldState>>#canvas

<details>
	<summary>See more</summary>
	
	canvas

	^ canvas
</details>

#### WorldState>>#checkIfUpdateNeeded

<details>
	<summary>See more</summary>
	
	checkIfUpdateNeeded

	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h hasChanged and: [h needsToBeDrawn]) ifTrue: [^true]].
	^false  "display is already up-to-date"

</details>

#### WorldState>>#drawInvalidAreasSubmorphs: submorphs

Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn.


<details>
	<summary>See more</summary>
	
	drawInvalidAreasSubmorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasSubmorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.

	"The idea here is to iterate morphs front to back, i.e. first the ones on top.
	For each morph, record the rectangles it affects. And if a morph is opaque, remove the area behind it from the list of rectangles to be repaired.
	This means, for example, that morphs completely covered might even not be redrawn.
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors.
	See 'A reverse painter's algorithm' at en.wikipedia.org/wiki/Painter's_algorithm"
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ world fillRects: initialRectsToRepair ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			world drawOn: (canvas newClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!
		See WorldState class >> #experiment1"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			world fillRects: xrects ].
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas newClipRect: r) fullDraw: m.
		].
"		(canvas newClipRect: rr) fullDraw: m"
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair
</details>

#### WorldState>>#tryDeferredUpdatingAndSetCanvas

If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false..


<details>
	<summary>See more</summary>
	
	tryDeferredUpdatingAndSetCanvas
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		world morphPosition: `0@0` extent: Display extent.
		self setCanvas: Display getCanvas.
	].
	^ true
</details>

#### WorldState>>#removeAlarm: aSelector for: aTarget

Remove the alarm with the given selector


<details>
	<summary>See more</summary>
	
	removeAlarm: aSelector for: aTarget 
	"Remove the alarm with the given selector"

	| alarm |
	alarm _ alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]
</details>

#### WorldState>>#stopStepping: aMorph selector: aSelector

Remove the given morph from the step list.


<details>
	<summary>See more</summary>
	
	stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])
</details>

#### WorldState>>#ensureNonDisplayCanvas

<details>
	<summary>See more</summary>
	
	ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= world morphExtent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (BitBltCanvas withExtent: world morphExtent depth: Display depth)]
</details>

#### WorldState>>#handsDo: aBlock

<details>
	<summary>See more</summary>
	
	handsDo: aBlock

	^ hands do: aBlock
</details>

#### WorldState>>#hands

<details>
	<summary>See more</summary>
	
	hands

	^ hands
</details>

#### WorldState>>#activeHand

Answer a hand for the morphic world that is the current UI focus. This is the UI root animated by the active Process. This method could answer nil, if not in an UI process!


<details>
	<summary>See more</summary>
	
	activeHand
	^activeHand
</details>

#### WorldState>>#doOneCycle

Do one cycle of the interaction loop. This method is called repeatedly when the world is running. Make for low cpu usage if the ui is inactive, but quick response when ui is in use. However, after some inactivity, there will be a larger delay before the ui gets responsive again.


<details>
	<summary>See more</summary>
	
	doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent | self class deferredUIMessages isEmpty not
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < 200				"No events processed? Start saving CPU! But never make the user wait more than 200ms for being responsive again."
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0
		ifFalse: [ Processor yield ]
		ifTrue: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow
</details>

#### WorldState>>#doOneMinimalCycleNow

Immediately do one cycle of the interaction loop. Only repair display and process events. For modal menus and such.


<details>
	<summary>See more</summary>
	
	doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldSafely.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue
</details>

#### WorldState>>#addKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	addKnownFailing: aMorph
	drawingFailingMorphs add: aMorph
</details>

#### WorldState>>#triggerAlarmsBefore: nowTime

Trigger all pending alarms that are to be executed before nowTime.


<details>
	<summary>See more</summary>
	
	triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."

	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	[ alarms notEmpty and: [ alarms first scheduledTime < nowTime ]]
		whileTrue: [ alarms removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime
</details>

#### WorldState>>#isKnownFailing: aMorph

<details>
	<summary>See more</summary>
	
	isKnownFailing: aMorph
	^drawingFailingMorphs includes: aMorph
</details>

