## TaskbarMorph

A simple task bar written for Cuis. dashBoard contains views/controls viewBox contains graphic buttons of "iconized" windows/morphs. scale allows 1x 2x 4x tarkbar height. [scale= 1,2,4]

### Methods
#### TaskbarMorph>>#isSticky

answer whether the receiver is Sticky


<details>
	<summary>See more</summary>
	
	isSticky
	"answer whether the receiver is Sticky"
	^true
</details>

#### TaskbarMorph>>#defaultHeight

<details>
	<summary>See more</summary>
	
	defaultHeight

	^ Preferences windowTitleFont lineSpacing * 2 * self scale
</details>

#### TaskbarMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	viewBox _ LayoutMorph newRow color: self defaultColor.
	self
		addMorph: UpdatingStringMorph initializedInstance
		layoutSpec:  (LayoutSpec morphWidthProportionalHeight: 0.5).
	self
		addMorph: viewBox 
		layoutSpec: (LayoutSpec
			proportionalWidth: 1.0
			proportionalHeight: 1.0 
			minorDirectionPadding: #right).
	viewBox separation: self defaultHeight // 8

</details>

#### TaskbarMorph>>#wasOpened: aMorph

aMorph was added to the world. Add button for aMorph if appropriate (see #taskbarIncludesAllWindows)


<details>
	<summary>See more</summary>
	
	wasOpened: aMorph
	"aMorph was added to the world. Add button for aMorph if appropriate (see #taskbarIncludesAllWindows)"

	self addButtonFor: aMorph
</details>

#### TaskbarMorph>>#aboutToCollapse: aMorph

Add a button for aMorph if not already there (see #taskbarIncludesAllWindows)


<details>
	<summary>See more</summary>
	
	aboutToCollapse: aMorph
	"Add a button for aMorph if not already there (see #taskbarIncludesAllWindows)"

	(self buttonFor: aMorph) ifNil: [
		self addButtonFor: aMorph ]
</details>

#### TaskbarMorph>>#removeButtonFor: aMorph

<details>
	<summary>See more</summary>
	
	removeButtonFor: aMorph

	(self buttonFor: aMorph) ifNotNil: [ :b |
		b delete ]
</details>

#### TaskbarMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #TaskbarMorph or: [ super is: aSymbol ]
</details>

#### TaskbarMorph>>#scaleNormal

<details>
	<summary>See more</summary>
	
	scaleNormal

	self scale: 1
</details>

#### TaskbarMorph>>#scale

<details>
	<summary>See more</summary>
	
	scale

	 ^ scale ifNil: [ self defaultScale ] ifNotNil: [ scale ]
</details>

#### TaskbarMorph>>#scaleX2

<details>
	<summary>See more</summary>
	
	scaleX2

	self scale: 2
</details>

#### TaskbarMorph>>#noteNewOwner: aMorph

I have just been added as a submorph of aMorph


<details>
	<summary>See more</summary>
	
	noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"
	super noteNewOwner: aMorph.
	aMorph submorphsDo: [ :m |
		self addButtonFor: m ].
	self notifyDisplayResize
</details>

#### TaskbarMorph>>#screenSizeChanged

Respond to change in screen size by repositioning self to bottom of screen


<details>
	<summary>See more</summary>
	
	screenSizeChanged
	"Respond to change in screen size by repositioning self to bottom of screen"
	
"	Transcript newLine; print: 'Taskbar screenSizeChanged'.
"
	| y e |
	UISupervisor whenUIinSafeState: [
		self world ifNotNil: [ :w |
			y _ w morphExtent y - self defaultHeight.
			e _ self internalizeDistance: w morphExtent x @ self defaultHeight.
			self morphPosition: 0@y extent: e ]]
</details>

#### TaskbarMorph>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete

	| w |
	self restoreAll.
	super delete.
	w _ self world ifNil: [ self runningWorld ].
	Display removeActionsWithReceiver: self.
	w ifNotNil: [ w taskbarDeleted ]
</details>

#### TaskbarMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent

	^ true
</details>

#### TaskbarMorph>>#scaleX4

<details>
	<summary>See more</summary>
	
	scaleX4

	self scale: 4
</details>

#### TaskbarMorph>>#addButtonFor: aMorph

<details>
	<summary>See more</summary>
	
	addButtonFor: aMorph

	| button |
	aMorph == self ifFalse: [
		button _ HoverableButtonMorph
			model: aMorph
			stateGetter: nil
			action: #endPreviewAndToggleCollapseOrShow
			onMouseEnterSend: #beginPreview
			onMouseLeaveSend: #endPreview.
		button
			color: self color;
			icon: (aMorph imageForm: 400@300 depth: 32);
			setBalloonText: #label.
		button icon: button magnifiedIcon.
		viewBox
			addMorph: button
			fixedWidth: self defaultHeight ]
</details>

#### TaskbarMorph>>#mouseButton2Activity

This method may be redefined, for example, to open a pop-up menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu
		addLine;
		add: 'Normal Height' action: #scaleNormal;
		add: 'Scale x 2' action: #scaleX2;
		add: 'Scale x 4' action: #scaleX4.
	menu popUpInWorld
</details>

#### TaskbarMorph>>#defaultScale

<details>
	<summary>See more</summary>
	
	defaultScale

	 ^ 1
</details>

#### TaskbarMorph>>#scale: anInteger

<details>
	<summary>See more</summary>
	
	scale: anInteger

	(anInteger between: 1 and: 4) ifFalse: [ self error: 'scale should be 1 2 or 4' ].
	scale := anInteger.
	self screenSizeChanged. "rescale self"
	viewBox ifNotNil: [ "rescale buttons"
		viewBox submorphs do: [ :button | 
			button layoutSpec fixedWidth: self defaultHeight
		]
	]
</details>

#### TaskbarMorph>>#wasDeleted: aMorph

aMorph was deleted. Remove button for aMorph


<details>
	<summary>See more</summary>
	
	wasDeleted: aMorph
	"aMorph was deleted. Remove button for aMorph"

	self removeButtonFor: aMorph
</details>

#### TaskbarMorph>>#restoreAll

<details>
	<summary>See more</summary>
	
	restoreAll
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model showAndComeToFront ]	]
</details>

#### TaskbarMorph>>#notifyDisplayResize

<details>
	<summary>See more</summary>
	
	notifyDisplayResize
	Display
		when: #screenSizeChanged
		send: #screenSizeChanged
		to: self.
	self screenSizeChanged
</details>

#### TaskbarMorph>>#buttonFor: aMorph

<details>
	<summary>See more</summary>
	
	buttonFor: aMorph
	
	viewBox ifNotNil: [
		viewBox submorphs do: [ :button | 
			button model == aMorph
				ifTrue: [ ^button ]]
	].
	^nil
</details>

