## HaloHandleMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### HaloHandleMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse up event.


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event."
	self send: mouseUpSelector withEvent: aMouseButtonEvent
</details>

#### HaloHandleMorph>>#mouseMoveSelector: aSymbol

<details>
	<summary>See more</summary>
	
	mouseMoveSelector: aSymbol
	mouseMoveSelector _ aSymbol
</details>

#### HaloHandleMorph>>#mouseDownSelector: aSymbol

<details>
	<summary>See more</summary>
	
	mouseDownSelector: aSymbol
	mouseDownSelector _ aSymbol
</details>

#### HaloHandleMorph>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Handle a mouse move event."
	aMouseMoveEvent anyButtonPressed ifTrue: [
		self send: mouseMoveSelector withEvent: aMouseMoveEvent ]
</details>

#### HaloHandleMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	
	^ mouseDownSelector notNil | mouseMoveSelector notNil | mouseUpSelector notNil
</details>

#### HaloHandleMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	aCanvas
		image: (self class circleForm: extent)
		multipliedBy: color
		at: self morphTopLeft 
</details>

#### HaloHandleMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	self send: mouseDownSelector withEvent: aMouseButtonEvent
</details>

#### HaloHandleMorph>>#mouseUpSelector: aSymbol

<details>
	<summary>See more</summary>
	
	mouseUpSelector: aSymbol
	mouseUpSelector _ aSymbol
</details>

#### HaloHandleMorph>>#send: selector withEvent: aMorphicEvent

<details>
	<summary>See more</summary>
	
	send: selector withEvent: aMorphicEvent
	| arity |
	owner ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	arity _ selector numArgs.
	arity = 0 ifTrue: [ ^ owner perform: selector ].
	arity = 1 ifTrue: [
		^ owner
			perform: selector
			with: aMorphicEvent ].
	arity = 2 ifTrue: [
		^ owner
			perform: selector
			with: aMorphicEvent
			with: self ].
	self error: 'Event handling selectors must be Symbols and take 0-2 arguments'
</details>

## HaloMorph

This morph provides a halo of handles for its target morph. Dragging, duplicating, rotating, and resizing to be done by mousing down on the appropriate handle. There are also handles for help and for a menu of infrequently used operations.

### Methods
#### HaloMorph>>#maybeDoDup: evt with: dupHandle

<details>
	<summary>See more</summary>
	
	maybeDoDup: evt with: dupHandle
	evt hand obtainHalo: self.
	^ target okayToDuplicate ifTrue:
		[self doDup: evt with: dupHandle]
</details>

#### HaloMorph>>#setDismissColor: evt with: dismissHandle

Called on mouseStillDown in the dismiss handle; set the color appropriately.


<details>
	<summary>See more</summary>
	
	setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse _  (dismissHandle morphContainsPoint:  (dismissHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [ `Color red muchLighter` ]
		ifTrue: [ `Color lightGray` ].
	dismissHandle color: colorToUse
</details>

#### HaloMorph>>#transferHalo: event localPosition: localEventPosition

Transfer the halo to the next likely recipient


<details>
	<summary>See more</summary>
	
	transferHalo: event localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil: [ ^self delete ].
	target transferHalo: event from: target.
</details>

#### HaloMorph>>#wantsHalo

<details>
	<summary>See more</summary>
	
	wantsHalo
	^false
</details>

#### HaloMorph>>#doDrag: evt with: dragHandle

<details>
	<summary>See more</summary>
	
	doDrag: evt with: dragHandle
	| thePoint |
	evt hand obtainHalo: self.
	thePoint _ evt eventPosition - positionOffset.
	target morphPositionInWorld: thePoint.
	self morphPositionInWorld: thePoint.
</details>

#### HaloMorph>>#doGrow: evt with: growHandle

Called while the mouse is down in the grow handle


<details>
	<summary>See more</summary>
	
	doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	growHandle morphPositionInWorld: evt eventPosition - (growHandle morphExtent // 2)
</details>

#### HaloMorph>>#step

<details>
	<summary>See more</summary>
	
	step
	(target isNil or: [target isInWorld not]) ifTrue: [self delete]
</details>

#### HaloMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.6
		g: 0.8
		b: 1.0`
</details>

#### HaloMorph>>#doGrab: evt with: grabHandle

Ask hand to grab my target.


<details>
	<summary>See more</summary>
	
	doGrab: evt with: grabHandle
	"Ask hand to grab my target."

	evt hand
		obtainHalo: self;
		grabMorph: target
</details>

#### HaloMorph>>#popUpFor: aMorph event: aMorphicEvent

This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame.


<details>
	<summary>See more</summary>
	
	popUpFor: aMorph event: aMorphicEvent
	"This message is sent by morphs that explicitly request the halo on a button click. Note: anEvent is in aMorphs coordinate frame."

	| hand anEvent |
	self flag: #workAround.	"We should really have some event/hand here..."
	anEvent _ aMorphicEvent
				ifNil: [
					hand _ aMorph world activeHand.
					hand ifNil: [ hand _ aMorph world firstHand ]. 
					hand lastMouseEvent ]
				ifNotNil: [
					hand _ aMorphicEvent hand.
					aMorphicEvent ].
	hand halo: self.
	hand world addMorphFront: self.
	self target: aMorph.
	positionOffset _ anEvent eventPosition - aMorph morphPositionInWorld
</details>

#### HaloMorph>>#morphContainsPoint: aLocalPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We behave as if we were a rectangle. I.e., we want (specifically mouse button) events that happen inside our bounds"
	^ self morphLocalBounds containsPoint: aLocalPoint
</details>

#### HaloMorph>>#addHelpHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addHelpHandle: haloSpec
	(self addHandle: haloSpec)
		mouseDownSelector: #mouseDownOnHelpHandle:;
		mouseMoveSelector: #deleteBalloon
</details>

#### HaloMorph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime
	^ 100
</details>

#### HaloMorph>>#addFontSizeHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addFontSizeHandle: haloSpec

	(target is: #InnerTextMorph) ifTrue: [
		(self addHandle: haloSpec) mouseDownSelector: #chooseFont]
</details>

#### HaloMorph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME. Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message


<details>
	<summary>See more</summary>
	
	wantsSteps
	^ true
</details>

#### HaloMorph>>#endInteraction

Clean up after a user interaction with the a halo control


<details>
	<summary>See more</summary>
	
	endInteraction
	"Clean up after a user interaction with the a halo control"

	(target isInWorld not or: [owner isNil]) ifTrue: [^self].
	self isInWorld 
		ifTrue: [
			"make sure handles show in front"
			self comeToFront.
			self addHandles]
</details>

#### HaloMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.
	""
	growingOrRotating _ false
</details>

#### HaloMorph>>#removeAllHandlesBut: h

Remove all handles except h.


<details>
	<summary>See more</summary>
	
	removeAllHandlesBut: h
	"Remove all handles except h."
	submorphs copy do:
		[:m | m == h ifFalse: [m delete]].

</details>

#### HaloMorph>>#mouseDownInCollapseHandle: evt with: collapseHandle

The mouse went down in the collapse handle; collapse the morph


<details>
	<summary>See more</summary>
	
	mouseDownInCollapseHandle: evt with: collapseHandle
	"The mouse went down in the collapse handle; collapse the morph"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self setDismissColor: evt with: collapseHandle
</details>

#### HaloMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #HaloMorph or: [ super is: aSymbol ]
</details>

#### HaloMorph>>#rejectsEvent: anEvent

Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it.


<details>
	<summary>See more</summary>
	
	rejectsEvent: anEvent
	"Return true to reject the given event. Rejecting an event means neither the receiver nor any of it's submorphs will be given any chance to handle it."
	(super rejectsEvent: anEvent) ifTrue: [^true].
	anEvent isDropEvent ifTrue: [^true]. "never attempt to drop on halos"
	^false
</details>

#### HaloMorph>>#doMenu: evt with: menuHandle

Ask hand to invoke the halo menu for my inner target.


<details>
	<summary>See more</summary>
	
	doMenu: evt with: menuHandle
	"Ask hand to invoke the halo menu for my inner target."

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	menu _ target buildHandleMenu: evt hand.
	target addTitleForHaloMenu: menu.
	menu popUpInWorld: self world.

</details>

#### HaloMorph>>#addNameString: aString

Add a name display centered beneath the bottom of the outer rectangle. Return the handle.


<details>
	<summary>See more</summary>
	
	addNameString: aString 
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| nameMorph namePosition nameBackground |
	nameBackground _ RectangleLikeMorph new
		color: ((target is: #SystemWindow) ifTrue: [target windowColor] ifFalse: [`Color lightBlue alpha: 0.9`]).
	nameMorph _ StringMorph contents: aString.
	nameMorph color: `Color black`.
	nameBackground morphExtent: nameMorph morphExtent + 4.
	namePosition _ haloBox width - nameMorph morphWidth // 2 @ (haloBox height).
	self addMorph: nameBackground position: namePosition - 2.
	self addMorph: nameMorph position: namePosition.
	^nameMorph
</details>

#### HaloMorph>>#maybeCollapse: evt with: collapseHandle

Ask hand to collapse my target if mouse comes up in it.


<details>
	<summary>See more</summary>
	
	maybeCollapse: evt with: collapseHandle 
	"Ask hand to collapse my target if mouse comes up in it."

	evt hand obtainHalo: self.
	self delete.
	(collapseHandle morphContainsPoint: (collapseHandle internalizeFromWorld: evt eventPosition)) 
		ifFalse: [
			target addHalo: evt ]
		ifTrue: [
			target collapse ]
</details>

#### HaloMorph>>#target

<details>
	<summary>See more</summary>
	
	target

	^ target

</details>

#### HaloMorph>>#addCollapseHandle: handleSpec

Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand.


<details>
	<summary>See more</summary>
	
	addCollapseHandle: handleSpec
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	target owner
		ifNil: [ ^self ]	"nil happens, amazingly"
		ifNotNil: [ :to |
			(to isWorldMorph or: [ to is: #HandMorph ])
				ifFalse: [ ^self ]].
		
	(self addHandle: handleSpec)
		mouseDownSelector: #mouseDownInCollapseHandle:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeCollapse:with:
</details>

#### HaloMorph>>#chooseEmphasisOrAlignment

<details>
	<summary>See more</summary>
	
	chooseEmphasisOrAlignment
	target chooseEmphasisOrAlignment
</details>

#### HaloMorph>>#addDebugHandle: handleSpec

<details>
	<summary>See more</summary>
	
	addDebugHandle: handleSpec

	Preferences debugHaloHandle ifTrue: [
		(self addHandle: handleSpec)
			mouseDownSelector: #doDebug:with: ]

</details>

#### HaloMorph>>#maybeDismiss: evt with: dismissHandle

Ask hand to dismiss my target if mouse comes up in it.


<details>
	<summary>See more</summary>
	
	maybeDismiss: evt with: dismissHandle
	"Ask hand to dismiss my target if mouse comes up in it."

	evt hand obtainHalo: self.
	(dismissHandle morphContainsPoint: (dismissHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [
			self delete.
			target addHalo: evt]
		ifTrue: [
			target resistsRemoval ifTrue: [
				(PopUpMenu
					confirm: 'Really throw this away'
					trueChoice: 'Yes'
					falseChoice: 'Um, no, let me reconsider') ifFalse: [^ self]].

			self delete.
			target dismissViaHalo]
</details>

#### HaloMorph>>#addRecolorHandle: haloSpec

Add a recolor handle to the receiver, if appropriate


<details>
	<summary>See more</summary>
	
	addRecolorHandle: haloSpec
	"Add a recolor handle to the receiver, if appropriate"

	(self addHandle: haloSpec) mouseUpSelector: #doRecolor:with:
</details>

#### HaloMorph>>#positionIn: aBox horizontalPlacement: horiz verticalPlacement: vert

<details>
	<summary>See more</summary>
	
	positionIn: aBox horizontalPlacement: horiz verticalPlacement: vert
	| xCoord yCoord |

	horiz == #left
		ifTrue:	[xCoord _ aBox left].
	horiz == #leftCenter
		ifTrue:	[xCoord _ aBox left + (aBox width // 4)].
	horiz == #center
		ifTrue:	[xCoord _ (aBox left + aBox right) // 2].
	horiz == #rightCenter
		ifTrue:	[xCoord _ aBox left + ((3 * aBox width) // 4)].
	horiz == #right
		ifTrue:	[xCoord _ aBox right].

	vert == #top
		ifTrue:	[yCoord _ aBox top].
	vert == #topCenter
		ifTrue:	[yCoord _ aBox top + (aBox height // 4)].
	vert == #center
		ifTrue:	[yCoord _ (aBox top + aBox bottom) // 2].
	vert == #bottomCenter
		ifTrue:	[yCoord _ aBox top + ((3 * aBox height) // 4)].
	vert == #bottom
		ifTrue:	[yCoord _ aBox bottom].

	^ xCoord asInteger @ yCoord asInteger
</details>

#### HaloMorph>>#startRot: evt with: rotHandle

Initialize rotation of my target if it is rotatable. Launch a command object to represent the action


<details>
	<summary>See more</summary>
	
	startRot: evt with: rotHandle
	"Initialize rotation of my target if it is rotatable.  Launch a command object to represent the action"

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	growingOrRotating _ true.

	self removeAllHandlesBut: rotHandle.  "remove all other handles"
	angleOffset _ evt eventPosition - target referencePosition.
	angleOffset _ Point
			r: angleOffset r
			degrees: angleOffset degrees - target rotationDegrees


</details>

#### HaloMorph>>#containsPoint: aLocalPoint event: aMorphicEvent

Return true if aPoint is considered to be inside the receiver for the given event. The default implementation treats locked children as integral part of their owners.


<details>
	<summary>See more</summary>
	
	containsPoint: aLocalPoint event: aMorphicEvent

	self visible ifFalse: [ ^false ].

	"mouseButton3 events are handled by the halo"
	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].

	^false
</details>

#### HaloMorph>>#mouseDownOnHelpHandle: anEvent

The mouse went down in the show-balloon handle


<details>
	<summary>See more</summary>
	
	mouseDownOnHelpHandle: anEvent
	target mouseDownOnHelpHandle: anEvent
</details>

#### HaloMorph>>#addGrowHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addGrowHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startGrow:with:;
		mouseMoveSelector: #doGrow:with:
</details>

#### HaloMorph>>#addFontEmphHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addFontEmphHandle: haloSpec

	(target is: #InnerTextMorph) ifTrue: [
		(self addHandle: haloSpec) mouseDownSelector: #chooseEmphasisOrAlignment ]
</details>

#### HaloMorph>>#addHandles

<details>
	<summary>See more</summary>
	
	addHandles

	self removeAllMorphs.  "remove old handles, if any"
	self morphBoundsInWorld: target worldBoundsForHalo.  "update my size"
	haloBox _ self basicBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	growingOrRotating _ false.
	self redrawNeeded
</details>

#### HaloMorph>>#chooseFont

<details>
	<summary>See more</summary>
	
	chooseFont
	target chooseFont
</details>

#### HaloMorph>>#basicBox

<details>
	<summary>See more</summary>
	
	basicBox
	| aBox minSide anExtent w |
	minSide _ 4 * self class handleSize.
	anExtent _ ((extent x + self class handleSize + 8) max: minSide) @
				((extent y + self class handleSize + 8) max: minSide).
	aBox _ Rectangle center: self morphBoundsInWorld center extent: anExtent.
	w _ self world ifNil: [ target world ].
	^ w
		ifNil:
			[ aBox ]
		ifNotNil:
			[ aBox intersect: (w viewBox insetBy: `8@8`) ]
</details>

#### HaloMorph>>#startDrag: evt with: dragHandle

Drag my target without removing it from its owner.


<details>
	<summary>See more</summary>
	
	startDrag: evt with: dragHandle
	"Drag my target without removing it from its owner."

	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	positionOffset _ dragHandle referencePosition - target morphPositionInWorld
</details>

#### HaloMorph>>#doDebug: evt with: menuHandle

Ask hand to invoke the a debugging menu for my inner target. If shift key is down, immediately put up an inspector on the inner target


<details>
	<summary>See more</summary>
	
	doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self world displayWorld.
	evt shiftPressed ifTrue: [
		self delete.
		^ target inspect].

	menu _ target buildDebugMenu: evt hand.
	menu addTitle: (target printStringLimitedTo: 40).
	menu popUpInWorld: self world
</details>

#### HaloMorph>>#addGrabHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addGrabHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #doGrab:with:
</details>

#### HaloMorph>>#startGrow: evt with: growHandle

Initialize resizing of my target. Launch a command representing it, to support Undo


<details>
	<summary>See more</summary>
	
	startGrow: evt with: growHandle
	"Initialize resizing of my target.  Launch a command representing it, to support Undo"

	| botRt |
	evt hand obtainHalo: self.	"Make sure the event's hand correlates with the receiver"
	self removeAllHandlesBut: growHandle.  "remove all other handles"
	botRt _ target morphPositionInWorld + target morphExtentInWorld.
	positionOffset _ (self world viewBox containsPoint: botRt)
		ifTrue: [evt eventPosition - botRt]
		ifFalse: [`0@0`]
</details>

#### HaloMorph>>#addDupHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addDupHandle: haloSpec
	(self addHandle: haloSpec) mouseDownSelector:#doDup:with:
</details>

#### HaloMorph>>#doDup: evt with: dupHandle

Ask hand to duplicate my target.


<details>
	<summary>See more</summary>
	
	doDup: evt with: dupHandle 
	"Ask hand to duplicate my target."

	target _ target duplicateMorph: evt.
	evt hand
		obtainHalo: self;
		grabMorph: target
</details>

#### HaloMorph>>#mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition

Transfer the halo to the next likely recipient


<details>
	<summary>See more</summary>
	
	mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition
	"Transfer the halo to the next likely recipient"
	target ifNil:[^self delete].
	aMouseButtonEvent hand obtainHalo: self.
	positionOffset _ aMouseButtonEvent eventPosition - target morphPositionInWorld.
	"wait for click to transfer halo"
	aMouseButtonEvent hand 
		waitForClicksOrDrag: self 
		event: aMouseButtonEvent
		clkSel: #transferHalo:localPosition:
		dblClkSel: nil
</details>

#### HaloMorph>>#addRotateHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addRotateHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startRot:with:;
		mouseMoveSelector: #doRot:with:
</details>

#### HaloMorph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	^false
</details>

#### HaloMorph>>#staysUpWhenMouseIsDownIn: aMorph

<details>
	<summary>See more</summary>
	
	staysUpWhenMouseIsDownIn: aMorph
	^ ((aMorph == target) or: [aMorph hasOwner: self])
</details>

#### HaloMorph>>#target: aMorph

<details>
	<summary>See more</summary>
	
	target: aMorph

	target _ aMorph.
	target ifNotNil: [ self addHandles ]
</details>

#### HaloMorph>>#addDismissHandle: handleSpec

<details>
	<summary>See more</summary>
	
	addDismissHandle: handleSpec

	(self addHandle: handleSpec)
		mouseDownSelector: #setDismissColor:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeDismiss:with:
</details>

#### HaloMorph>>#deleteBalloon

If I am showing a balloon, delete it.


<details>
	<summary>See more</summary>
	
	deleteBalloon
	target deleteBalloon
</details>

#### HaloMorph>>#addMenuHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addMenuHandle: haloSpec

	(self addHandle: haloSpec) mouseDownSelector: #doMenu:with:
</details>

#### HaloMorph>>#addDragHandle: haloSpec

<details>
	<summary>See more</summary>
	
	addDragHandle: haloSpec

	(self addHandle: haloSpec)
		mouseDownSelector: #startDrag:with:;
		mouseMoveSelector: #doDrag:with:
</details>

#### HaloMorph>>#doRot: evt with: rotHandle

Update the rotation of my target if it is rotatable. Keep the relevant command object up to date.


<details>
	<summary>See more</summary>
	
	doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable.  Keep the relevant command object up to date."

	| degrees |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
	degrees _ (evt eventPosition - target referencePosition) degrees.
	degrees _ degrees - angleOffset degrees.
	degrees _ degrees detentBy: 10.0 atMultiplesOf: 90.0 snap: false.
	degrees = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	self removeAllHandlesBut: rotHandle.

	target rotationDegrees: degrees.

	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2)
</details>

#### HaloMorph>>#addHandle: handleSpec

Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient. Return the handle.


<details>
	<summary>See more</summary>
	
	addHandle: handleSpec
	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."

	| handle aPoint colorToUse form icon |
	aPoint _ self 
				positionIn: haloBox
				horizontalPlacement: handleSpec horizontalPlacement
				verticalPlacement: handleSpec verticalPlacement.
	colorToUse _ Color colorFrom: handleSpec color.
	handle _ HaloHandleMorph new color: colorToUse.
	self addMorph: handle.
	handle morphBoundsInWorld: (Rectangle center: aPoint extent: self class handleSize asPoint).
	handleSpec iconSymbol ifNotNil: [ :iconName |
			form _ self class icons at: iconName ifAbsent: [self class perform: iconName].
			form ifNotNil: [
				icon _ ImageMorph new
					image: form;
					color: colorToUse makeForegroundColor;
					lock.
				handle addMorphFront: icon position: `0@0` ]].
	handle mouseUpSelector: #endInteraction.
	handle setBalloonText: handleSpec hoverHelp.
	^ handle
</details>

#### HaloMorph>>#drawOn: aCanvas

Draw this morph only if it has no target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas 
	"Draw this morph only if it has no target."

	target ifNil: [^super drawOn: aCanvas]
</details>

#### HaloMorph>>#doRecolor: evt with: aHandle

The mouse went down in the 'recolor' halo handle. Allow the user to change the color of the innerTarget


<details>
	<summary>See more</summary>
	
	doRecolor: evt with: aHandle
	"The mouse went down in the 'recolor' halo handle.  Allow the user to change the color of the innerTarget"

	evt hand obtainHalo: self.
	(aHandle morphContainsPoint: (aHandle internalizeFromWorld: evt eventPosition))
		ifFalse: [  "only do it if mouse still in handle on mouse up"
			self delete.
			target addHalo: evt]
		ifTrue: [
			target changeColor]
</details>

## HaloSpec

Sets spec's for how handles are layed out in a halo.

### Methods
#### HaloSpec>>#verticalPlacement

<details>
	<summary>See more</summary>
	
	verticalPlacement
	^ verticalPlacement
</details>

#### HaloSpec>>#printOn: aStream

Add a textual printout representing the receiver to a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Add a textual printout representing the receiver to a stream"

	super printOn: aStream.
	aStream nextPutAll: ' (', addHandleSelector asString, ' ', iconSymbol asString, ')'
</details>

#### HaloSpec>>#horizontalPlacement

<details>
	<summary>See more</summary>
	
	horizontalPlacement
	^ horizontalPlacement
</details>

#### HaloSpec>>#addHandleSelector

<details>
	<summary>See more</summary>
	
	addHandleSelector
	^ addHandleSelector
</details>

#### HaloSpec>>#horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel hoverHelp: aString

<details>
	<summary>See more</summary>
	
	horizontalPlacement: hp verticalPlacement: vp color: col iconSymbol: is addHandleSelector: sel hoverHelp: aString
	horizontalPlacement _ hp.
	verticalPlacement _ vp.
	color _ col.
	iconSymbol _ is asSymbol.
	addHandleSelector _ sel.
	hoverHelp _ aString
</details>

#### HaloSpec>>#iconSymbol

<details>
	<summary>See more</summary>
	
	iconSymbol
	^ iconSymbol
</details>

#### HaloSpec>>#color

<details>
	<summary>See more</summary>
	
	color
	^ color
</details>

#### HaloSpec>>#hoverHelp

<details>
	<summary>See more</summary>
	
	hoverHelp
	^hoverHelp
</details>

