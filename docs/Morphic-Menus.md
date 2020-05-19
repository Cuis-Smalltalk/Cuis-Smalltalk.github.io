## DynamicMenuBuilder

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DynamicMenuBuilder>>#changeOptions

<details>
	<summary>See more</summary>
	
	changeOptions

	optionsChanger value: items
</details>

#### DynamicMenuBuilder>>#addGroupedMenuOptionsToMenu

<details>
	<summary>See more</summary>
	
	addGroupedMenuOptionsToMenu
	
	groups := items groupBy: [ :item | item at: #itemGroup ].
	groups keys asSortedCollection 
		do: [ :group | self addMenuOptionsOfGroup: group ]
		separatedBy: [ self addGroupSeparation ].

</details>

#### DynamicMenuBuilder>>#classesProvidingMenuOptions

<details>
	<summary>See more</summary>
	
	classesProvidingMenuOptions

	^(Smalltalk allClassesImplementing: menuOptionsSelector) select: [ :aClass | aClass isMeta ]
</details>

#### DynamicMenuBuilder>>#addMenuOptionsOfGroup: group

<details>
	<summary>See more</summary>
	
	addMenuOptionsOfGroup: group

	| groupMenuOptions |
	
	groupMenuOptions := (groups at: group) asSortedCollection: [ :leftItem :rightItem | (leftItem at: #itemOrder) < (rightItem at: #itemOrder) ].
	menu addItemsFromDictionaries: groupMenuOptions.
</details>

#### DynamicMenuBuilder>>#collectMenuOptions

<details>
	<summary>See more</summary>
	
	collectMenuOptions

	items := OrderedCollection new.
	self classesProvidingMenuOptions do: [ :aClass | items addAll: (aClass soleInstance perform: menuOptionsSelector) ].
	
</details>

#### DynamicMenuBuilder>>#addGroupSeparation

<details>
	<summary>See more</summary>
	
	addGroupSeparation 

	menu addLine
	
</details>

#### DynamicMenuBuilder>>#build

<details>
	<summary>See more</summary>
	
	build

	self 
		createMenu;
	 	collectMenuOptions;
		changeOptions;
		addGroupedMenuOptionsToMenu.		
			
	^ menu.
</details>

#### DynamicMenuBuilder>>#initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

<details>
	<summary>See more</summary>
	
	initializeTitled: aTitle targeting: aDefaultTarget collectingMenuOptionsWith: aMenuOptionsSelector changingThemWith: anOptionsChangerBlock

	title := aTitle.
	defaultTarget := aDefaultTarget.
	menuOptionsSelector := aMenuOptionsSelector.
	optionsChanger := anOptionsChangerBlock 
</details>

#### DynamicMenuBuilder>>#hasTitle

<details>
	<summary>See more</summary>
	
	hasTitle

	^title ~= self class noTitle 
</details>

#### DynamicMenuBuilder>>#createMenu

<details>
	<summary>See more</summary>
	
	createMenu

	menu := self hasTitle ifTrue: [ MenuMorph entitled: title] ifFalse: [ MenuMorph new ].
	menu defaultTarget: defaultTarget
</details>

## MVCMenuMorph

I simulate the MVC menu classes PopUpMenu, SelectionMenu, and CustomMenu when running in a Morphic world. I am also used to implement Utilities>informUser:during:.

### Methods
#### MVCMenuMorph>>#selectMVCItem: item

Called by the MenuItemMorph that the user selects. Record the selection and set the done flag to end this interaction.


<details>
	<summary>See more</summary>
	
	selectMVCItem: item
	"Called by the MenuItemMorph that the user selects.
	Record the selection and set the done flag to end this interaction."

	mvcSelection _ item.
	done _ true.

</details>

#### MVCMenuMorph>>#invokeAt: aPoint allowKeyboard: aBoolean

Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu.


<details>
	<summary>See more</summary>
	
	invokeAt: aPoint allowKeyboard: aBoolean
	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."
	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 
	|actHand w oldFocus delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self
		popUpAt: aPoint
		forHand: actHand
		allowKeyboard: aBoolean.
	done _ false.
	delay _ Delay forMilliseconds: 10.
	[ done not and: [self isInWorld] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ mvcSelection 
</details>

#### MVCMenuMorph>>#cancelValue: selectionOrNil

Set the value to be returned if the user cancels without making a selection.


<details>
	<summary>See more</summary>
	
	cancelValue: selectionOrNil
	"Set the value to be returned if the user cancels without making a selection."

	mvcSelection _ selectionOrNil.

</details>

## MenuItemMorph

I represent an item in a menu. Instance variables: isEnabled <Boolean> True if the menu item can be executed. subMenu <MenuMorph | nil> The submenu to activate automatically when the user mouses over the item. isSelected <Boolean> True if the item is currently selected. target <Object> The target of the associated action. selector <Symbol> The associated action. arguments <Array> The arguments for the associated action. icon <Form | nil> An optional icon form to be displayed to my left. If I have a dynamic marker, created by strings like <yes> or <no> in my contents, it will be installed as a submorph.

### Methods
#### MenuItemMorph>>#invokeWithEvent: evt

Perform the action associated with the given menu item.


<details>
	<summary>See more</summary>
	
	invokeWithEvent: evt
	"Perform the action associated with the given menu item."

	| selArgCount w |
	self isEnabled ifFalse: [^ self].
	owner ifNotNil: [
		self flag: #workAround. "The tile system invokes menus straightforwardly so the menu might not be in the world."
		(w _ self world) ifNotNil:[
			owner deleteIfPopUp: evt.
			"Repair damage before invoking the action for better feedback"
			w displayWorldSafely]].
	selector ifNil: [ ^self ].
	(selArgCount _ selector numArgs) = 0
		ifTrue: [
			target perform: selector]
		ifFalse: [
			selArgCount = arguments size
				ifTrue: [target perform: selector withArguments: arguments]
				ifFalse: [target perform: selector withArguments: (arguments copyWith: evt)]]
</details>

#### MenuItemMorph>>#deselect

<details>
	<summary>See more</summary>
	
	deselect

	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil: [ owner activeSubmenu: nil ]]
</details>

#### MenuItemMorph>>#processMouseEnter: aMouseEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseEnter: aMouseEvent localPosition: localEventPosition
	"System level event handling."
	"Do #mouseEnter: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	self wantsBalloon ifTrue: [
		aMouseEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	(self handlesMouseOver: aMouseEvent) ifTrue:[
		aMouseEvent wasHandled: true.
		self mouseEnter: aMouseEvent ]
</details>

#### MenuItemMorph>>#contents: aString

<details>
	<summary>See more</summary>
	
	contents: aString
	^self contentsWithMarkers: aString inverse: false
</details>

#### MenuItemMorph>>#hasSubMenu

Return true if the receiver has a submenu


<details>
	<summary>See more</summary>
	
	hasSubMenu
	"Return true if the receiver has a submenu"
	^subMenu notNil
</details>

#### MenuItemMorph>>#activateSubmenu: evt

Activate our submenu; e.g., pass control to it


<details>
	<summary>See more</summary>
	
	activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil: [ ^false ]. "not applicable"
	(subMenu morphContainsPoint: (subMenu internalizeFromWorld: evt eventPosition)) ifFalse:[^false].
	subMenu activate: evt.
	^true
</details>

#### MenuItemMorph>>#isEnabled

<details>
	<summary>See more</summary>
	
	isEnabled

	^ isEnabled

</details>

#### MenuItemMorph>>#processMouseLeave: aMouseEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processMouseLeave: aMouseEvent localPosition: localEventPosition
	"System level event handling."

	"Do #mouseLeave: even if button down (others, like LayoutAdjustingMorph need the default behavior)"
	aMouseEvent hand removePendingBalloonFor: self.
	(self handlesMouseOver: aMouseEvent) ifTrue: [
		aMouseEvent wasHandled: true.
		self mouseLeave: aMouseEvent ]
</details>

#### MenuItemMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. Menu items get activated when the mouse is over them.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		^ super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self
</details>

#### MenuItemMorph>>#aboutToBeGrabbedBy: aHand

Don't allow the receiver to act outside a Menu


<details>
	<summary>See more</summary>
	
	aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu |
	self isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: self.
	menu stayUp.
	^ menu
</details>

#### MenuItemMorph>>#duplicateMorph: evt

Don't allow the receiver to act outside a Menu


<details>
	<summary>See more</summary>
	
	duplicateMorph: evt
	"Don't allow the receiver to act outside a Menu"
	| dup menu |
	dup _ self duplicate isSelected: false.
	menu _ MenuMorph new defaultTarget: nil.
	menu addMorphFront: dup.
	menu stayUp.
	^ menu
</details>

#### MenuItemMorph>>#isSelected: aBoolean

<details>
	<summary>See more</summary>
	
	isSelected: aBoolean

	isSelected _ aBoolean.
	self redrawNeeded
</details>

#### MenuItemMorph>>#hasIcon

Answer whether the receiver has an icon.


<details>
	<summary>See more</summary>
	
	hasIcon
	"Answer whether the receiver has an icon."
	^ icon notNil
</details>

#### MenuItemMorph>>#contentsWithMarkers: aString inverse: inverse

Set the menu item entry. Parse aString for embedded markers.


<details>
	<summary>See more</summary>
	
	contentsWithMarkers: aString inverse: inverse 
	"Set the menu item entry. Parse aString for embedded markers."

	| markerIndex marker |
	contentString _ nil.	"get rid of old"
	self removeAllMorphs.	"get rid of old markers if updating"
	icon _ nil.
	(aString notEmpty and: [aString first = $<]) 
		ifFalse: [^super contents: aString].
	markerIndex := aString indexOf: $>.
	markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	contentString _ aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).
	"And set the marker"
	marker := ImageMorph new image: marker.
	self addMorphFront: marker position: `0@2`
</details>

#### MenuItemMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu.


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event. Menu items get activated when the mouse is over them. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."
	owner hasMouseFocus ifFalse: [ ^self ].
	"This will happen if the menu has toggles in it. (for instance, the 'show...' button)
	Update the look, refresh the world and wait a bit,
	to give the user some visual feedback"
	contentString ifNotNil: [
		self contentsWithMarkers: contentString inverse: true.
		self refreshWorld.
		(Delay forMilliseconds: 200) wait].
	self deselect.
	self invokeWithEvent: aMouseButtonEvent
</details>

#### MenuItemMorph>>#hasMarker

Answer whether the receiver has a marker morph.


<details>
	<summary>See more</summary>
	
	hasMarker
	"Answer whether the receiver has a marker morph."
	^ submorphs isEmpty not
</details>

#### MenuItemMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.
	""
	extent _ `10@10`.
	contents _ ''.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
	font _ Preferences standardMenuFont
</details>

#### MenuItemMorph>>#target: anObject selector: aSymbol arguments: aCollection

<details>
	<summary>See more</summary>
	
	target: anObject selector: aSymbol arguments: aCollection

	target _ anObject.
	selector _ aSymbol.
	arguments _ aCollection
</details>

#### MenuItemMorph>>#activateOwnerMenu: evt

Activate our owner menu; e.g., pass control to it


<details>
	<summary>See more</summary>
	
	activateOwnerMenu: evt
	"Activate our owner menu; e.g., pass control to it"
	owner ifNil: [ ^false ]. "not applicable"
	(owner morphContainsPoint: (owner internalizeFromWorld: evt eventPosition))
		ifFalse: [ ^false ].
	owner activate: evt.
	^true
</details>

#### MenuItemMorph>>#subMenu: aMenuMorph

<details>
	<summary>See more</summary>
	
	subMenu: aMenuMorph
	subMenu _ aMenuMorph.
	self redrawNeeded
</details>

#### MenuItemMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #MenuItemMorph or: [ super is: aSymbol ]
</details>

#### MenuItemMorph>>#isEnabled: aBoolean

<details>
	<summary>See more</summary>
	
	isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled _ aBoolean.
	self color: (aBoolean ifTrue: [`Color black`] ifFalse: [`Color gray`]).

</details>

#### MenuItemMorph>>#subMenu

<details>
	<summary>See more</summary>
	
	subMenu

	^ subMenu

</details>

#### MenuItemMorph>>#offImage

Return the form to be used for indicating an '<off>' marker


<details>
	<summary>See more</summary>
	
	offImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 32.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: `(Color gray: 0.9)`
			borderWidth: 1 borderColor: `Color black`.
	^form
</details>

#### MenuItemMorph>>#mouseEnter: evt

The mouse entered the receiver


<details>
	<summary>See more</summary>
	
	mouseEnter: evt
	"The mouse entered the receiver"
	owner ifNil: [ ^self ].
	owner selectItem: self
</details>

#### MenuItemMorph>>#minItemWidth

<details>
	<summary>See more</summary>
	
	minItemWidth
	| fontToUse iconWidth subMenuWidth markerWidth |
	fontToUse _ self fontToUse.
	subMenuWidth _ self hasSubMenu
				ifFalse: [0]
				ifTrue: [10].
	iconWidth _ icon
				ifNotNil: [icon width * 12//10]
				ifNil: [0].
	markerWidth _ self hasMarker
		ifTrue: [ submorphs first morphWidth + 8 ]
		ifFalse: [ 0 ].
	^ (fontToUse widthOfString: contents)
		+ subMenuWidth + iconWidth + markerWidth.
</details>

#### MenuItemMorph>>#setBlankIcon

change the the receiver's icon


<details>
	<summary>See more</summary>
	
	setBlankIcon
	"change the the receiver's icon"
	icon := Theme current blankIcon
</details>

#### MenuItemMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent

	^ true
</details>

#### MenuItemMorph>>#iconSeparation

<details>
	<summary>See more</summary>
	
	iconSeparation
	^5
</details>

#### MenuItemMorph>>#onImage

Return the form to be used for indicating an '<off>' marker


<details>
	<summary>See more</summary>
	
	onImage
	"Return the form to be used for indicating an '<off>' marker"
	| form |
	form _ Form extent: (self fontToUse ascent-2) asPoint depth: 32.
	form getCanvas
		frameAndFillRectangle: form boundingBox fillColor: `Color gray: 0.8`
			borderWidth: 1 borderColor: `Color black`;
		fillRectangle: (form boundingBox insetBy: 2) color: `Color black`.
	^form
</details>

#### MenuItemMorph>>#handlesMouseOver: anEvent

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseOver: anEvent
	^true
</details>

#### MenuItemMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas 
	| stringColor leftEdge |

	stringColor _ color.
	isSelected & isEnabled
		ifTrue: [
			aCanvas fillRectangle: self morphLocalBounds color: Theme current menuHighlight].
	leftEdge _ 0.

	self hasMarker ifTrue: [
		leftEdge _ leftEdge + submorphs first morphWidth + 8 ].

	icon ifNotNil: [
		| iconForm w h factor magnifiedExtent magnifiedIcon |
		iconForm _ isEnabled ifTrue: [ icon ] ifFalse: [ icon asGrayScaleAndTransparent ].
		magnifiedIcon _ iconForm.
		w _ iconForm width.
		h _ iconForm height.
		w*h = 0 ifFalse: [
			factor _ extent y * 0.8 / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (iconForm extent * factor) rounded.
				magnifiedIcon _ iconForm magnifyTo: magnifiedExtent ]].
		aCanvas image: magnifiedIcon at: leftEdge+1 @ (magnifiedIcon height *1//10).
		leftEdge _ magnifiedIcon width *12//10 + leftEdge].

	aCanvas
		drawString: contents
		at: leftEdge @ 1
		font: self fontToUse
		color: stringColor.
	subMenu ifNotNil: [
		aCanvas
			image: self class subMenuMarker
			at: extent x - 8 @ (extent y - self class subMenuMarker height // 2) ]
</details>

#### MenuItemMorph>>#measureContents

<details>
	<summary>See more</summary>
	
	measureContents
	| e |
	e _ super measureContents.
	^e y > 12
		ifTrue: [e+`2@2`]
		ifFalse: [e+`2@1`]
</details>

#### MenuItemMorph>>#setIcon: symbolOrFormOrNil

Argument can be a Form, a Symbol (to be sent to Theme current) or nil.


<details>
	<summary>See more</summary>
	
	setIcon: symbolOrFormOrNil
	"Argument can be a Form, a Symbol (to be sent to Theme current) or nil."

	icon _ symbolOrFormOrNil isSymbol
		ifTrue: [Theme current perform: symbolOrFormOrNil]
		ifFalse: [ symbolOrFormOrNil ]
</details>

#### MenuItemMorph>>#deleteIfPopUp: evt

Recurse up for nested pop ups


<details>
	<summary>See more</summary>
	
	deleteIfPopUp: evt
	"Recurse up for nested pop ups"
	owner ifNotNil:[owner deleteIfPopUp: evt].
</details>

#### MenuItemMorph>>#select

<details>
	<summary>See more</summary>
	
	select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: (Array with: self morphBoundsInWorld topRight + `10@0`
											with: self morphBoundsInWorld topLeft)
			from: self.
		subMenu selectItem: nil ]
</details>

## MenuLineMorph

Just a line for separating items in menus.

### Methods
#### MenuLineMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ `50 @ 2`
</details>

#### MenuLineMorph>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent

	^`10@2`
</details>

#### MenuLineMorph>>#minItemWidth

<details>
	<summary>See more</summary>
	
	minItemWidth
	^ 0

</details>

#### MenuLineMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas 
	| baseColor |
	baseColor _ owner color.
	aCanvas
		fillRectangle: (self morphTopLeft corner: extent x @ (extent y / 2))
		color: baseColor twiceDarker.
			
	aCanvas
		fillRectangle: (0 @ (extent y / 2) corner: extent)
		color: baseColor twiceLighter
</details>

## MenuMorph

Instance variables: defaultTarget <Object> The default target for creating menu items selectedItem <MenuItemMorph> The currently selected item in the receiver stayUp <Boolean> True if the receiver should stay up after clicks

### Methods
#### MenuMorph>>#addStayUpIcons

<details>
	<summary>See more</summary>
	
	addStayUpIcons
	| closeButton pinButton w buttonHW |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Preferences windowTitleFont pointSize.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton icon: Theme current closeIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton icon: Theme current pushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedWidth: buttonHW//3;
			addMorph: pinButton fixedWidth: buttonHW).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons
</details>

#### MenuMorph>>#addUpdating: wordingSelector target: target action: aSymbol argumentList: argList

Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument. In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target, Answer the item added.


<details>
	<summary>See more</summary>
	
	addUpdating: wordingSelector target: target action: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  In this variant, the wording of the menu item is obtained by sending the wordingSelector to the target,  Answer the item added."

	| item |
	item _ UpdatingMenuItemMorph new
		target: target selector: aSymbol arguments: argList asArray;
		wordingProvider: target wordingSelector: wordingSelector.
	self addMorphBack: item.
	^ item
</details>

#### MenuMorph>>#defaultTarget: anObject

Set the default target for adding menu items.


<details>
	<summary>See more</summary>
	
	defaultTarget: anObject
	"Set the default target for adding menu items."

	defaultTarget _ anObject.

</details>

#### MenuMorph>>#popUpForHand: hand in: aWorld

Present this menu under control of the given hand.


<details>
	<summary>See more</summary>
	
	popUpForHand: hand in: aWorld
	| p |
	"Present this menu under control of the given hand."

	p _ hand morphPosition truncated.
	^self popUpAt: p forHand: hand in: aWorld

</details>

#### MenuMorph>>#addUpdating: aWordingSelector action: aSymbol

<details>
	<summary>See more</summary>
	
	addUpdating: aWordingSelector action: aSymbol

	^self addUpdating: aWordingSelector target: defaultTarget action: aSymbol argumentList: #()
</details>

#### MenuMorph>>#activate: evt

Receiver should be activated; e.g., so that control passes correctly.


<details>
	<summary>See more</summary>
	
	activate: evt
	"Receiver should be activated; e.g., so that control passes correctly."
	evt hand newMouseFocus: self.
</details>

#### MenuMorph>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	activeSubMenu ifNotNil:[activeSubMenu delete].
	^super delete
</details>

#### MenuMorph>>#displayAt: aPoint during: aBlock

Add this menu to the Morphic world during the execution of the given block.


<details>
	<summary>See more</summary>
	
	displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world ifNotNil: [ w displayWorld ].  "show myself"
		].
	aBlock value.
	self delete
</details>

#### MenuMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ Theme current menu
</details>

#### MenuMorph>>#mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse up event. Note: This might be sent from a modal shell.


<details>
	<summary>See more</summary>
	
	mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		"mejor en un lugar que no dependa de esto... es una porqueria"
		aMouseButtonEvent hand newMouseFocus: self ]
</details>

#### MenuMorph>>#keyStroke: aKeyboardEvent

Handle a keystroke event.


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent 
	| matchString char asc selectable |
	char := aKeyboardEvent keyCharacter.
	asc := char numericValue.
	aKeyboardEvent isReturnKey
		ifTrue: [
			selectedItem ifNotNil: [
					selectedItem hasSubMenu 
						ifTrue: [
							aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
							^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: [
							"self delete."
							^selectedItem invokeWithEvent: aKeyboardEvent]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: aKeyboardEvent].
			^self].
	asc = 27 
		ifTrue: [
			"escape key"
			self
				valueOfProperty: #matchString
				ifPresentDo: [ :str | 
					str isEmpty 
						ifFalse: [
							"If filtered, first ESC removes filter"
							self setProperty: #matchString toValue: String new.
							self selectItem: nil.
							^self displayFiltered: aKeyboardEvent]].
			"If a stand-alone menu, just delete it"
			^self delete].
	(asc = 28 or: [asc = 29]) 
		ifTrue: [
			"left or right arrow key"
			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: [
					aKeyboardEvent hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: aKeyboardEvent.
					^aKeyboardEvent hand newKeyboardFocus: selectedItem subMenu]
				ifFalse: [^ self]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: aKeyboardEvent].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: aKeyboardEvent].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: aKeyboardEvent].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: aKeyboardEvent].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsent: [String new].
	matchString := char = Character backspace 
				ifTrue: [
					matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: aKeyboardEvent keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: aKeyboardEvent
</details>

#### MenuMorph>>#displayFiltered: evt

<details>
	<summary>See more</summary>
	
	displayFiltered: evt
	| matchStr allItems isMatch matches feedbackMorph |
	matchStr _ self valueOfProperty: #matchString.
	allItems _ self submorphs select: [ :m |
		m is: #MenuItemMorph ].
	matches _ allItems select: [ :m |
		isMatch _ matchStr isEmpty or: [
			m contents
				includesSubstring: matchStr
				caseSensitive: false ].
		m isEnabled: isMatch.
		isMatch ].
	feedbackMorph _ self valueOfProperty: #feedbackMorph.
	feedbackMorph ifNil: [
		feedbackMorph _ StringMorph new color: `Color veryDarkGray`.
		self addMorphBack: feedbackMorph lock position: `0@ -20`.
		self
			setProperty: #feedbackMorph
			toValue: feedbackMorph ].
	feedbackMorph contents: '<' , matchStr , '>'.
	matchStr isEmpty ifTrue: [
		feedbackMorph delete.
		self removeProperty: #feedbackMorph ].
	matches notEmpty ifTrue: [
		self selectItem: matches first ]
</details>

#### MenuMorph>>#addItemsFromDictionaries: dataForMenuDicts

A menu constructor utility that uses Dictionaries with elements: #label - the name that displays in the menu #object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target. #selector - the selector called on object when the menu item is selected #arguments - optional collection of arguments passed to the selector #balloonText - optional 'tool tip' style help text #icon- optional icon selector or Form note, nil elements will add a line.


<details>
	<summary>See more</summary>
	
	addItemsFromDictionaries: dataForMenuDicts
	"A menu constructor utility that uses Dictionaries with elements:
		#label - the name that displays in the menu
		#object - the target object. If nil, use defaultTarget. If a Symbol, send it as message to defaultTarget to get real target.
		#selector - the selector called on object when the menu item is selected
		#arguments - optional collection of arguments passed to the selector
		#balloonText - optional 'tool tip' style help text
		#icon-	optional icon selector or Form

	note, nil elements will add a line."
	| item wantsIcons |
	wantsIcons _ Preferences wantsMenuIcons.
	dataForMenuDicts do: [ :dict |
		dict
			ifNil: [ self addLine ]
			ifNotNil: [ | realTarget |
				realTarget _ dict at: #object ifAbsent: [defaultTarget].
				realTarget isSymbol ifTrue: [ realTarget _ defaultTarget perform: realTarget ].
				item _ (dict at: #label) isSymbol
					ifTrue: [
						self
							addUpdating: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ]
					ifFalse: [
						self
							add: (dict at: #label)
							target: realTarget
							action: (dict at: #selector)
							argumentList:
								(dict
									at: #arguments
									ifAbsent: [ #() ]) ].
				wantsIcons ifTrue: [
					dict
						at: #icon
						ifPresent: [ :symbolOrFormOrNil |
							item setIcon: symbolOrFormOrNil ]].
				dict
					at: #balloonText
					ifPresent: [ :balloonText |
						item setBalloonText: balloonText ].
			]]
</details>

#### MenuMorph>>#selectItem: aMenuItem

<details>
	<summary>See more</summary>
	
	selectItem: aMenuItem
	selectedItem ifNotNil: [ selectedItem deselect ].
	selectedItem _ aMenuItem.
	selectedItem ifNotNil: [ selectedItem select ]
</details>

#### MenuMorph>>#add: aString target: target action: aSymbol argument: arg icon: symbolOrFormOrNil

<details>
	<summary>See more</summary>
	
	add: aString target: target action: aSymbol argument: arg icon: symbolOrFormOrNil
	
	^(self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }) 
			setIcon: symbolOrFormOrNil;
			yourself

</details>

#### MenuMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse up event. Note: This might be sent from a modal shell.


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse up event.
	Note: This might be sent from a modal shell."
	(self fullContainsPoint: localEventPosition) ifFalse:[
		"Mouse up outside. Release eventual focus and delete if pop up."
		aMouseButtonEvent hand ifNotNil: [ :h | h releaseMouseFocus: self ].
		^ self deleteIfPopUp: aMouseButtonEvent ].
	stayUp ifFalse: [
		"Still in pop-up transition; keep focus"
		aMouseButtonEvent hand newMouseFocus: self ]
</details>

#### MenuMorph>>#popUpAt: aPoint forHand: hand allowKeyboard: aBoolean

Present this menu at the given point under control of the given hand.


<details>
	<summary>See more</summary>
	
	popUpAt: aPoint forHand: hand allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	| evt |
	self items isEmpty ifTrue: [^self].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph ]) 
		do: [ :m | m updateContents].
	self runningWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ].
	evt _ hand lastMouseEvent.
	(evt isKeyboard or: [ evt isMouse and: [ evt anyButtonPressed not ]]) 
		ifTrue: [
			"Select first item if button not down"
			self moveSelectionDown: 1 event: evt ]
</details>

#### MenuMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ `40@10`.
	defaultTarget _ nil.
	selectedItem _ nil.
	stayUp _ false.
	popUpOwner _ nil
</details>

#### MenuMorph>>#handlesKeyboard

Answer whether the receiver handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard
	"Answer whether the receiver handle keyboard events"

	^self visible
</details>

#### MenuMorph>>#addUpdating: aWordingSelector target: aTarget action: aSymbol

<details>
	<summary>See more</summary>
	
	addUpdating: aWordingSelector target: aTarget action: aSymbol

	^self addUpdating: aWordingSelector target: aTarget action: aSymbol argumentList: #()
</details>

#### MenuMorph>>#stayUp

<details>
	<summary>See more</summary>
	
	stayUp

	^ self stayUp: true

</details>

#### MenuMorph>>#add: aString action: aSymbol

Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object.


<details>
	<summary>See more</summary>
	
	add: aString action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #()
</details>

#### MenuMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^true
</details>

#### MenuMorph>>#fitInWorld

Note: items may not be laid out yet (I found them all to be at 0@0), so we have to add up heights of items above the selected item.


<details>
	<summary>See more</summary>
	
	fitInWorld
	"Note: items may not be laid out yet (I found them all to be at 0@0),  
	so we have to add up heights of items above the selected item."

	| delta |
	"If it doesn't fit, show it to the left, not to the right of the hand."
	self morphBoundsInWorld right > owner world morphBoundsInWorld right
		ifTrue: [
			self morphPosition: ((self morphPosition x + 10 - extent x) @ self morphPosition y) ].

	"Make sure that the menu fits in the world."
	delta _ self morphBoundsInWorld amountToTranslateWithin:
		(owner world morphBoundsInWorld withHeight:
			((owner world morphExtentInWorld y) max: (self morphPosition y) + 1)).
	delta = `0 @ 0` ifFalse: [ self morphPosition: self morphPosition + delta ]
</details>

#### MenuMorph>>#addServices: services for: served extraLines: linesArray

<details>
	<summary>See more</summary>
	
	addServices: services for: served extraLines: linesArray

	services withIndexDo: [:service :i |
		service addServiceFor: served toMenu: self.
		submorphs last setBalloonText: service description.
		(linesArray includes: i) | service useLineAfter 
			ifTrue: [self addLine]].

</details>

#### MenuMorph>>#activeSubmenu: aSubmenu

<details>
	<summary>See more</summary>
	
	activeSubmenu: aSubmenu
	activeSubMenu ifNotNil:[activeSubMenu delete].
	activeSubMenu _ aSubmenu.
</details>

#### MenuMorph>>#add: aString action: aSymbol balloonText: stringOrText

Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object.


<details>
	<summary>See more</summary>
	
	add: aString action: aSymbol balloonText: stringOrText
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setBalloonText: stringOrText
</details>

#### MenuMorph>>#add: aString subMenu: aMenuMorph

Append the given submenu with the given label.


<details>
	<summary>See more</summary>
	
	add: aString subMenu: aMenuMorph
	"Append the given submenu with the given label."

	| item |
	item _ MenuItemMorph new.
	item
		contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item.
	^item
</details>

#### MenuMorph>>#invokeModal: allowKeyboardControl

Invoke this menu and don't return until the user has chosen a value. If the allowKeyboarControl boolean is true, permit keyboard control of the menu See senders of this method for finding out how to use modal menu morphs.


<details>
	<summary>See more</summary>
	
	invokeModal: allowKeyboardControl
	"Invoke this menu and don't return until the user has chosen a value.  If the allowKeyboarControl boolean is true, permit keyboard control of the menu
	See senders of this method for finding out how to use modal menu morphs."
	| w oldFocus actHand delay |
	w _ self runningWorld.
	actHand _ w activeHand.
	oldFocus _ actHand keyboardFocus.
	w doOneMinimalCycleNow.
	self	
		popUpAt: actHand morphPosition
		forHand: actHand 
		allowKeyboard: allowKeyboardControl.
	self isModalInvokationDone: false.
	delay _ Delay forMilliseconds: 10.
	[ self isInWorld and: [self isModalInvokationDone not] ] whileTrue: [ w doOneMinimalCycleNow. delay wait ].
	self delete.
	oldFocus ifNotNil: [ actHand newKeyboardFocus: oldFocus ].
	^ self modalSelection
</details>

#### MenuMorph>>#add: aString target: aTarget action: aSymbol

<details>
	<summary>See more</summary>
	
	add: aString target: aTarget action: aSymbol
	^self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #()
</details>

#### MenuMorph>>#add: aString target: target action: aSymbol argumentList: argList

Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument. Answer the appended menu item.


<details>
	<summary>See more</summary>
	
	add: aString target: target action: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument.  Answer the appended menu item."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target selector: aSymbol arguments: argList asArray.
	self addMorphBack: item.
	^ item
</details>

#### MenuMorph>>#moveSelectionDown: direction event: evt

Move the current selection up or down by one, presumably under keyboard control. direction = +/-1


<details>
	<summary>See more</summary>
	
	moveSelectionDown: direction event: evt
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index m |
	index _ (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | m _ submorphs atWrap: index.
		((m is: #MenuItemMorph) and: [m isEnabled]) ifTrue: [
			^ self selectItem: m ].
		"Keep looking for an enabled item"
		index _ index + direction sign].
	^ self selectItem: nil
</details>

#### MenuMorph>>#informUserAt: aPoint during: aBlock

Add this menu to the Morphic world during the execution of the given block.


<details>
	<summary>See more</summary>
	
	informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]
</details>

#### MenuMorph>>#isModalInvokationDone: aBool

<details>
	<summary>See more</summary>
	
	isModalInvokationDone: aBool
	self setProperty: #isModalInvokationDone toValue: aBool

</details>

#### MenuMorph>>#add: aString target: aTarget action: aSymbol icon: symbolOrFormOrNil

<details>
	<summary>See more</summary>
	
	add: aString target: aTarget action: aSymbol icon: symbolOrFormOrNil
	^(self add: aString
		target: aTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil
</details>

#### MenuMorph>>#selectedItem

<details>
	<summary>See more</summary>
	
	selectedItem
	^selectedItem
</details>

#### MenuMorph>>#justDroppedInto: newOwnerMorph event: evt

This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph


<details>
	<summary>See more</summary>
	
	justDroppedInto: newOwnerMorph event: evt
	| halo |
	super justDroppedInto: newOwnerMorph event: evt.
	halo _ evt hand halo.
	(halo notNil and:[halo target hasOwner: self]) ifTrue: [
		"Grabbed single menu item"
		self addHalo: evt ].
	stayUp ifFalse: [ evt hand newMouseFocus: self ]
</details>

#### MenuMorph>>#modalSelection

<details>
	<summary>See more</summary>
	
	modalSelection
	^self valueOfProperty: #modalSelection ifAbsent: nil
</details>

#### MenuMorph>>#popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem

Present this menu at the given point under control of the given hand. Used mostly for submenus.


<details>
	<summary>See more</summary>
	
	popUpAdjacentTo: rightOrLeftPointInWorld from: sourceItem
	"Present this menu at the given point under control of the given hand.
	Used mostly for submenus."

	| delta tryToPlace selectedOffset |
	popUpOwner _ sourceItem.
	selectedOffset _ (selectedItem ifNil: [ self items first ]) morphPosition.
	sourceItem world addMorphFront: self.

	tryToPlace _ [ :where :mustFit |
		self morphPosition: where - selectedOffset.
		delta _ self morphFullBoundsInWorld
			amountToTranslateWithin: sourceItem world morphBoundsInWorld.
		(delta x = 0 | mustFit) ifTrue: [
			delta = `0@0` ifFalse: [ self morphPosition: self morphPosition + delta ].
			^ self]].
	tryToPlace 
		value: rightOrLeftPointInWorld first value: false;
		value: rightOrLeftPointInWorld last - (extent x @ 0) value: false;
		value: rightOrLeftPointInWorld first value: true
</details>

#### MenuMorph>>#add: aString action: aSymbol icon: symbolOrFormOrNil

Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object.


<details>
	<summary>See more</summary>
	
	add: aString action: aSymbol icon: symbolOrFormOrNil
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	^(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil
</details>

#### MenuMorph>>#items

<details>
	<summary>See more</summary>
	
	items

	^ submorphs select: [:m | m is: #MenuItemMorph]

</details>

#### MenuMorph>>#addBlankIconsIfNecessary

If any of my items have an icon, ensure that all do by using anIcon for those that don't


<details>
	<summary>See more</summary>
	
	addBlankIconsIfNecessary
	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"

	| withIcons withoutIcons |
	withIcons _ Set new.
	withoutIcons _ Set new.
	self items do: [ :item |
		item hasIcon | item hasMarker
			ifTrue: [ withIcons add: item ]
			ifFalse: [ withoutIcons add: item ].
		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary ]].
"	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ]."
	withoutIcons do: [ :item | item setBlankIcon ]
</details>

#### MenuMorph>>#popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean

Present this menu at the given point under control of the given hand.


<details>
	<summary>See more</summary>
	
	popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: aBoolean 
	"Present this menu at the given point under control of the given hand."

	self items isEmpty ifTrue: [ ^self ].
	self addBlankIconsIfNecessary.
	(self submorphs select: [ :m | m is: #UpdatingMenuItemMorph]) 
		do: [ :m | m updateContents].
	aWorld addMorphFront: self position: aPoint - `2 @ 8`.
	self fitInWorld.
	"Acquire focus for valid pop up behavior"
	hand newMouseFocus: self.
	aBoolean ifTrue: [ hand newKeyboardFocus: self ]
</details>

#### MenuMorph>>#intoWorld: aWorld

The receiver has just appeared in a new world. Note: * aWorld can be nil (due to optimizations in other places) * owner is already set * owner's submorphs may not include receiver yet. Important: Keep this method fast - it is run whenever morphs are added.


<details>
	<summary>See more</summary>
	
	intoWorld: aWorld
	"The receiver has just appeared in a new world. Note:
		* aWorld can be nil (due to optimizations in other places)
		* owner is already set
		* owner's submorphs may not include receiver yet.
	Important: Keep this method fast - it is run whenever morphs are added."
	super intoWorld: aWorld.
	self adjustSubmorphsLayout
</details>

#### MenuMorph>>#invokeModal

Invoke this menu and don't return until the user has chosen a value. See example below on how to use modal menu morphs.


<details>
	<summary>See more</summary>
	
	invokeModal
	"Invoke this menu and don't return until the user has chosen a value.
	See example below on how to use modal menu morphs."
	^ self invokeModal: Preferences menuKeyboardControl

	"
	| menu sub entry |
	menu _ MenuMorph new.
	1 to: 3 do: [:i |
		entry _ 'Line', i printString.
		sub _ MenuMorph new.
		menu add: entry subMenu: sub.
		#('Item A' 'Item B' 'Item C')  do:[:subEntry|
			sub add: subEntry target: menu 
				action: #modalSelection: argument: {entry. subEntry}]].
	menu invokeModal.
	"
</details>

#### MenuMorph>>#add: aString action: aSymbol icon: symbolOrFormOrNil enabled: aBoolean

Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object.


<details>
	<summary>See more</summary>
	
	add: aString action: aSymbol icon: symbolOrFormOrNil enabled: aBoolean
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	(self add: aString
		target: defaultTarget
		action: aSymbol
		argumentList: #())
			setIcon: symbolOrFormOrNil;
			isEnabled: aBoolean
</details>

#### MenuMorph>>#popUpAt: aPoint forHand: hand in: aWorld

Present this menu at the given point under control of the given hand. Allow keyboard input into the menu.


<details>
	<summary>See more</summary>
	
	popUpAt: aPoint forHand: hand in: aWorld
	"Present this menu at the given point under control of the given hand.  Allow keyboard input into the menu."

	^ self popUpAt: aPoint forHand: hand in: aWorld allowKeyboard: Preferences menuKeyboardControl
</details>

#### MenuMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."
	(stayUp or: [ self fullContainsPoint:localEventPosition ]) 
		ifFalse: [ ^self deleteIfPopUp: aMouseButtonEvent ]. "click outside"

	"Grab the menu and drag it to some other place
	This is reimplemented here because we handle the event, and if the following lines are commented, a menu can't be grabbed with the hand. This is not nice and shouldn't be needed"
	self isSticky ifTrue: [ ^self ].
	aMouseButtonEvent hand grabMorph: self
</details>

#### MenuMorph>>#popUpInWorld: aWorld

Present this menu under control of the given hand.


<details>
	<summary>See more</summary>
	
	popUpInWorld: aWorld
	"Present this menu under control of the given hand."
	"Needed if not the real world but an inner PasteUpMorph"
	| positionInWorld |
	positionInWorld _ aWorld internalizeFromWorld: aWorld activeHand morphPosition.
	^self
		popUpAt: positionInWorld
		forHand: aWorld activeHand
		in: aWorld

</details>

#### MenuMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	^ (Theme current roundWindowCorners or: [Theme current minimalWindows])
		ifTrue: [0]
		ifFalse: [Preferences menuBorderWidth]
</details>

#### MenuMorph>>#labels: labelList lines: linesArray selections: selectionsArray

This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:.


<details>
	<summary>See more</summary>
	
	labels: labelList lines: linesArray selections: selectionsArray
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."
	"Labels can be either a sting with embedded crs, or a collection of strings."

	| labelArray |
	labelArray _ (labelList isMemberOf: String)
		ifTrue: [ labelList lines ]
		ifFalse: [ labelList ].
	1 to: labelArray size do: [ :i |
		self add: (labelArray at: i) action: (selectionsArray at: i).
		(linesArray includes: i) ifTrue: [ self addLine ]]
</details>

#### MenuMorph>>#keyboardFocusChange: aBoolean

Notify change due to green border for keyboard focus


<details>
	<summary>See more</summary>
	
	keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	aBoolean ifFalse: [self deleteIfPopUp: nil].
	self redrawNeeded
</details>

#### MenuMorph>>#handleFocusEvent: aMorphicEvent

Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children.


<details>
	<summary>See more</summary>
	
	handleFocusEvent: aMorphicEvent
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	| eventPositionInOurCoordinates |
	eventPositionInOurCoordinates _ self internalizeFromWorld: aMorphicEvent eventPosition.

	self dispatchEvent: aMorphicEvent localPosition: eventPositionInOurCoordinates.

	"Need to handle keyboard input if we have the focus."
	aMorphicEvent isKeyboard ifTrue: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(aMorphicEvent isMouseOver or: [aMorphicEvent isMouse not]) ifTrue: [ ^self ].
	"What remains are mouse buttons and moves"
	aMorphicEvent isMove ifFalse: [ ^ aMorphicEvent sentTo: self localPosition: eventPositionInOurCoordinates ]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: aMorphicEvent) ifTrue: [^self]].
	"Note: The following does not traverse upwards but it's the best I can do for now"
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: aMorphicEvent) ifTrue: [^self]].
</details>

#### MenuMorph>>#isModalInvokationDone

<details>
	<summary>See more</summary>
	
	isModalInvokationDone
	^self valueOfProperty: #isModalInvokationDone ifAbsent:[false]
</details>

#### MenuMorph>>#modalSelection: anObject

<details>
	<summary>See more</summary>
	
	modalSelection: anObject
	self setProperty: #modalSelection toValue: anObject.
	self isModalInvokationDone: true
</details>

#### MenuMorph>>#addLine

Append a divider line to this menu. Suppress duplicate lines.


<details>
	<summary>See more</summary>
	
	addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	submorphs isEmpty ifTrue: [^ self].
	(self lastSubmorph class == MenuLineMorph)
		ifFalse: [self addMorphBack: MenuLineMorph new].

</details>

#### MenuMorph>>#adjustSubmorphsLayout

Enlarge the width of submorphs as needed so all of them are have the same width, and no less than #minWidth. Also adjust their vertical position. Finally, set our own extent.


<details>
	<summary>See more</summary>
	
	adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w _ w + 1.
	p _ `5 @ 5`.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5
</details>

#### MenuMorph>>#lastItem

<details>
	<summary>See more</summary>
	
	lastItem
	^ submorphs reverse
		detect: [ :m | m is: #MenuItemMorph ]
		ifNone: [ submorphs last ]
</details>

#### MenuMorph>>#wantsToBeDroppedInto: aMorph

Return true if it's okay to drop the receiver into aMorph. A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object.


<details>
	<summary>See more</summary>
	
	wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.  A single-item MenuMorph is in effect a button rather than a menu, and as such should not be reluctant to be dropped into another object."

	^ (aMorph isWorldMorph or: [submorphs size = 1]) or:
		[Preferences systemWindowEmbedOK]
</details>

#### MenuMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphLocalBounds color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]
</details>

#### MenuMorph>>#addTitle: aString

Add a title line at the top of this menu Make aString its initial contents. If aSelector is not nil, then periodically obtain fresh values for its contents by sending aSelector to aTarget..


<details>
	<summary>See more</summary>
	
	addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s pp w |
	
	titleMorph _ RectangleLikeMorph new.
	titleMorph color: Theme current menuTitleBar.
	pp _ `8@2`.
	aString asString linesDo: [ :line |
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		titleMorph addMorphBack: s position: pp.
		pp _ pp + (0@(s morphHeight+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each morphWidth ].
	titleMorph morphExtent: (w + 16) @ (pp y).
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]
</details>

#### MenuMorph>>#removeStayUpBox

<details>
	<summary>See more</summary>
	
	removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box _ submorphs first submorphs first.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]
</details>

#### MenuMorph>>#stayUp: aBoolean

<details>
	<summary>See more</summary>
	
	stayUp: aBoolean

	stayUp _ aBoolean.
	aBoolean ifTrue: [ self removeStayUpBox ].
</details>

#### MenuMorph>>#add: aString target: target action: aSymbol argument: arg

Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument.


<details>
	<summary>See more</summary>
	
	add: aString target: target action: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	^self add: aString
		target: target
		action: aSymbol
		argumentList: { arg }
</details>

#### MenuMorph>>#popUpInWorld

Present this menu in the current World


<details>
	<summary>See more</summary>
	
	popUpInWorld
	"Present this menu in the current World"

	^ self popUpInWorld: self runningWorld
</details>

#### MenuMorph>>#deleteIfPopUp: evt

Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu.


<details>
	<summary>See more</summary>
	
	deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [ self delete ].
	popUpOwner ifNotNil: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt ].
	evt ifNotNil: [ evt hand ifNotNil: [ :h | h releaseMouseFocus: self ]]
</details>

## TheWorldMenu

Instances of TheWorldMenu serve to present the primary Squeak menu obtained by clicking on open desktop.

### Methods
#### TheWorldMenu>>#garbageCollect

Do a garbage collection, and report results to the user.


<details>
	<summary>See more</summary>
	
	garbageCollect
	"Do a garbage collection, and report results to the user."

	Utilities garbageCollectAndReport
</details>

#### TheWorldMenu>>#startMessageTally

Tally on all the processes in the system, and not only the UI


<details>
	<summary>See more</summary>
	
	startMessageTally
	"Tally on all the processes in the system, and not only the UI"
	
	| d |
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		[
			d _ Delay forMilliseconds: 100.
			AndreasSystemProfiler spyAllOn: [
				[Sensor peekMousePt y > 10] whileTrue: [d wait]]
			] forkAt: Processor userInterruptPriority
		]
</details>

#### TheWorldMenu>>#doPopUp: aMenu

<details>
	<summary>See more</summary>
	
	doPopUp: aMenu

	aMenu popUpForHand: myHand in: myWorld.

</details>

#### TheWorldMenu>>#openCommentGuide

In ClassCommentBrowser.pck


<details>
	<summary>See more</summary>
	
	openCommentGuide
	"In ClassCommentBrowser.pck"
	Smalltalk at: #CommentGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Class Comment Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #'ClassCommentBrowser'] 
	].
	Smalltalk at: #CommentGuide ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package ClassCommentBrowser.pck.st', String newLineString, 'Feature require: #''ClassCommentBrowser'' '
</details>

#### TheWorldMenu>>#pvtAlphaSplitListDepth

Split factor. A higher number results in fewer items in each submenu


<details>
	<summary>See more</summary>
	
	pvtAlphaSplitListDepth
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 4
</details>

#### TheWorldMenu>>#openMenu

Build the open window menu for the world.


<details>
	<summary>See more</summary>
	
	openMenu
	"Build the open window menu for the world."

	^(DynamicMenuBuilder buildTitled: 'Open...' targeting: self collectingMenuOptionsWith: #worldMenuForOpenGroup) 
		addStayUpIcons;
		yourself
	
</details>

#### TheWorldMenu>>#vmStatistics

Open a string view on a report of vm statistics


<details>
	<summary>See more</summary>
	
	vmStatistics
	"Open a string view on a report of vm statistics"

	(TextModel new contents: Utilities vmStatisticsReportString)
		openLabel: 'VM Statistics'
</details>

#### TheWorldMenu>>#newMorphOfClass: morphClass event: evt

Attach a new morph of the given class to the invoking hand.


<details>
	<summary>See more</summary>
	
	newMorphOfClass: morphClass event: evt
	"Attach a new morph of the given class to the invoking hand."

	| m |
	m _ morphClass initializedInstance.
	evt hand attachMorph: m
</details>

#### TheWorldMenu>>#windowsMenu

Build the windows menu for the world.


<details>
	<summary>See more</summary>
	
	windowsMenu
        "Build the windows menu for the world."

	^ (self menu: 'Windows')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Find Window'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findWindow:.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Presents a list of all windows; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Browsers...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyBrowsers:.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			{
				#label 			-> 		'Find changed Windows...'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findDirtyWindows:.
				#icon 			-> 		#newWindowIcon.
				#balloonText 	-> 		'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Find a Transcript'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findATranscript:.
				#icon 			-> 		#printerIcon.
				#balloonText 	-> 		'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a FileList'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAFileList:.
				#icon 			-> 		#systemFileManagerIcon.
				#balloonText 	-> 		'Brings an open fileList  to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find a Change Sorter'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAChangeSorter:.
				#icon 			-> 		#changesIcon.
				#balloonText 	-> 		'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			{
				#label 			-> 		'Find Message Names'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#findAMessageNamesWindow:.
				#icon 			-> 		#inspectIcon.
				#balloonText 	-> 		'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Tile open windows'.
				#object            ->       TileResizeMorph.
				#selector 		-> 		#tileOpenWindows.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'Tile open windows'.
			} asDictionary.
			{
				#label 			-> 		'Collapse all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#collapseAll.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'Reduce all open windows to collapsed forms that only show titles.'
			} asDictionary.
			{
				#label 			-> 		'Restore all Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#restoreAll.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'Restore all collapsed windows back to their previous forms.'
			} asDictionary.
			{
				#label 			-> 		'Close top Window'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#closeTopWindow.
				#icon 			-> 		#closeIcon.
				#balloonText 	-> 		'Close the topmost window if possible.'
			} asDictionary.
			{
				#label 			-> 		'Send top Window to back'.
				#object 			-> 		SystemWindow.
				#selector 		-> 		#sendTopWindowToBack.
				#icon 			-> 		#goBottomIcon.
				#balloonText 	-> 		'Make the topmost window become the backmost one, and activate the window just beneath it.'
			} asDictionary.
			{
				#label 			-> 		'Move Windows onscreen'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#bringWindowsFullOnscreen.
				#icon 			-> 		#displayIcon.
				#balloonText 	-> 		'Make all windows fully visible on the screen'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Delete unchanged Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#closeUnchangedWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all windows that do not have unsaved text edits.'
			} asDictionary.
			{
				#label 			-> 		'Delete non Windows'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#deleteNonWindows.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all non-window morphs lying on the world.'
			} asDictionary.
			{
				#label 			-> 		'Delete Both of the Above'.
				#selector 		-> 		#cleanUpWorld.
				#icon 			-> 		#warningIcon.
				#balloonText 	-> 		'Deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'
			} asDictionary.
		}`
</details>

#### TheWorldMenu>>#openChangesMenu

Build the changes menu for the world.


<details>
	<summary>See more</summary>
	
	openChangesMenu
	"Build the changes menu for the world."

	self doPopUp: self changesMenu
</details>

#### TheWorldMenu>>#changesMenu

Build the changes menu for the world.


<details>
	<summary>See more</summary>
	
	changesMenu
        "Build the changes menu for the world."

	^ (self menu: 'Changes...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Change Sorter'.
				#object 			-> 		ChangeSorterWindow.
				#selector 		-> 		#openChangeSorter.
				#icon 			-> 		#halfRefreshIcon.
				#balloonText 	-> 		'Open a 3-paned changed-set viewing tool'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Install New Updates'.
				#object 			-> 		ChangeSet.
				#selector 		-> 		#installNewUpdates.
				#icon 			-> 		#updateIcon.
				#balloonText 	-> 		'Install in the current image the new updates available
in directory named ./CoreUpdates
or in directory named ../Cuis-Smalltalk-Dev/CoreUpdates'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Browse my Changes'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#browseMyChanges.
				#icon 			-> 		#editFindReplaceIcon.
				#balloonText 	-> 		'Browse all of my changes since the last time #condenseSources was run.'.
			} asDictionary.
			{
				#label 			-> 		'Recently logged Changes...'.
				#object 			-> 		ChangeList.
				#selector 		-> 		#browseRecentLog.
				#icon 			-> 		#clockIcon.
				#balloonText 	-> 		'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Save World as morph file'.
				#selector 		-> 		#saveWorldInFile.
				#icon 			-> 		#morphsIcon.
				#balloonText 	-> 		'Save a file that, when reloaded, reconstitutes the current World.'.
			} asDictionary.
		}`
</details>

#### TheWorldMenu>>#openOpenMenu

<details>
	<summary>See more</summary>
	
	openOpenMenu

	self doPopUp: self openMenu
</details>

#### TheWorldMenu>>#debugMenu

<details>
	<summary>See more</summary>
	
	debugMenu

	^ (self menu: 'Debug...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#inspect.
				#icon 			-> 		#inspectIcon.
			} asDictionary.
			{
				#label 			-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#explore.
				#icon 			-> 		#exploreIcon.
			} asDictionary.
			{
				#label 			-> 		'MessageTally all Processes'.
				#selector 		-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#removeAllKnownFailing.
				#icon 			-> 		#graphicsIcon.
			} asDictionary.
			{
				#label 			-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#resumeAfterStepError.
				#icon 			-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#label 			-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 		-> 		#closeAllDebuggers.
				#icon 			-> 		#closeIcon.
			} asDictionary.
		}`
</details>

#### TheWorldMenu>>#pvtNewMenuForSplitLists: splitLists

<details>
	<summary>See more</summary>
	
	pvtNewMenuForSplitLists: splitLists
| firstChar lastChar menu subMenu |
menu := MenuMorph new defaultTarget: self.
	1
		to: splitLists size
		do: [ :i |
			firstChar := i = 1
				ifTrue: [ $A ]
				ifFalse: [ 
					(splitLists at: i) first name first: 3 ].
			lastChar := i = splitLists size
				ifTrue: [ $Z ]
				ifFalse: [ 
					(splitLists at: i) last name first: 3 ].
			subMenu := MenuMorph new.
			(splitLists at: i) do: [ :cl |
				subMenu
					add: cl name
					target: self
					action: #newMorphOfClass:event:
					argument: cl ].
			menu
				add: firstChar asString , ' - ' , lastChar asString
				subMenu: subMenu ].
	^ menu.
</details>

#### TheWorldMenu>>#openPreferencesMenu

Build and show the preferences menu for the world.


<details>
	<summary>See more</summary>
	
	openPreferencesMenu
	"Build and show the preferences menu for the world."

	self doPopUp: self preferencesMenu
</details>

#### TheWorldMenu>>#cleanUpWorld

<details>
	<summary>See more</summary>
	
	cleanUpWorld
	(SelectionMenu confirm: 'This will remove all windows except those
containing unsubmitted text edits, and will
also remove all non-window morphs (other
than flaps) found on the desktop.  Are you
sure you want to do this?') ifFalse: [ ^ self ].
	myWorld allNonWindowRelatedSubmorphs do: [ :m |
		m delete ].
	(SystemWindow
		windowsIn: myWorld
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]
</details>

#### TheWorldMenu>>#helpMenu

Build the help menu for the world.


<details>
	<summary>See more</summary>
	
	helpMenu
	"Build the help menu for the world."

	^ (self menu: 'Help...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'About this System...'.
				#object 			-> 		Smalltalk.
				#selector 		-> 		#aboutThisSystem.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'current version information.'
			} asDictionary.
			{
				#label 			-> 		'Terse Guide to Cuis'.
				#selector 		-> 		#openTerseGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'explore Cuis Smalltalk'
			} asDictionary.
			{
				#label 			-> 		'Class Comment Browser'.
				#selector 		-> 		#openCommentGuide.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'search & explore Cuis Class Comments'
			} asDictionary.
			{
				#label 			-> 		'Code management in Cuis'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCodeManagementInCuis.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'Features are kept in Packages.'
			} asDictionary.
			{
				#label 			-> 		'Using GitHub to host Cuis packages'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openCuisAndGitHub.
				#icon 			->			#helpIcon.
				#balloonText 	-> 		'GitHub usage pattern.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Editor keyboard shortcuts'.
				#object 			-> 		SmalltalkEditor.
				#selector 		-> 		#openHelp.
				#icon 			-> 		#keyboardShortcutsIcon.
				#balloonText 	-> 		'summary of keyboard shortcuts in editors for Smalltalk code.'
			} asDictionary.
			{
				#label 			-> 		'Useful Expressions'.
				#object 			-> 		Utilities.
				#selector 		-> 		#openUsefulExpressions.
				#icon 			-> 		#chatIcon.
				#balloonText 	-> 		'a window full of useful expressions.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'VM Statistics'.
				#selector 		-> 		#vmStatistics.
				#icon 			-> 		#systemMonitorIcon.
				#balloonText 	-> 		'obtain some intriguing data about the vm.'
			} asDictionary.
			{
				#label 			-> 		'Space Left'.
				#selector 		-> 		#garbageCollect.
				#icon 			-> 		#removableMediaIcon.
				#balloonText 	-> 		'perform a full garbage-collection and report how many bytes of space remain in the image.'
			} asDictionary.
		}`
</details>

#### TheWorldMenu>>#preferencesMenu

Build the preferences menu for the world.


<details>
	<summary>See more</summary>
	
	preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 		-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 		-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Font Sizes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->			'Set font...'.
				#object 			-> 		FontPicker.
				#selector 		-> 		#promptUserAndSetDefaultInstallIfNecessaryWithFamilies:.
				#arguments 		-> 		{FontPicker familyNames}.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current font family.'
			} asDictionary.
			{
				#label			->			'Load extra fonts'.
				#object 			-> 		FontFamily.
				#selector 		-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 		-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 		-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 		-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 		-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}
</details>

#### TheWorldMenu>>#splitNewMorphList: list depth: d

<details>
	<summary>See more</summary>
	
	splitNewMorphList: list depth: d
	| middle c prev next out |
	d <= 0 ifTrue: [ ^ Array with: list ].
	middle := list size // 2 + 1.
	c := (list at: middle) name first: 3.
	prev := middle - 1.
	[
	prev > 0 and: [ ((list at: prev) name first: 3) = c ]] whileTrue: [ prev := prev - 1 ].
	next := middle + 1.
	[
	next <= list size and: [ ((list at: next) name first: 3) = c ]] whileTrue: [ next := next + 1 ].
	"Choose the better cluster"
	middle := middle - prev < (next - middle)
		ifTrue: [ prev + 1 ]
		ifFalse: [ next ].
	middle = 1 ifTrue: [ middle := next ].
	middle >= list size ifTrue: [ middle := prev + 1 ].
	(middle = 1 or: [ middle >= list size ]) ifTrue: [ ^ Array with: list ].
	out := WriteStream on: Array new.
	out nextPutAll:
		(self
			splitNewMorphList:
				(list
					copyFrom: 1
					to: middle - 1)
			depth: d - 1).
	out nextPutAll:
		(self
			splitNewMorphList:
				(list
					copyFrom: middle
					to: list size)
			depth: d - 1).
	^ out contents.
</details>

#### TheWorldMenu>>#pvtMaxItemsPerCategorySubMenu

If the number of items exceeds this value, split the category submenu into sub-submenus


<details>
	<summary>See more</summary>
	
	pvtMaxItemsPerCategorySubMenu
	"If the number of items exceeds this value, split the category submenu into sub-submenus"
	^ 15.
</details>

#### TheWorldMenu>>#buildWorldMenu

Build the menu that is put up when the screen-desktop is clicked on


<details>
	<summary>See more</summary>
	
	buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #worldMenuOptions)
		addStayUpIcons;
		yourself
	
	
</details>

#### TheWorldMenu>>#newMorph

The user requested 'new morph' from the world menu. Put up a menu that allows many ways of obtaining new morphs. If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used.


<details>
	<summary>See more</summary>
	
	newMorph
	"The user requested 'new morph' from the world menu.  Put up a menu that allows many ways of obtaining new morphs.  If the preference #classicNewMorphMenu is true, the full form of yore is used; otherwise, a much shortened form is used."
	| menu catDict |
	menu _ self menu: 'Add a new morph'.
	menu
		
			add: 'From Clipboard'
			target: myHand
			action: #pasteMorph;
		
			add: 'From Alphabetical List'
			subMenu: self alphabeticalMorphMenu.
	menu addLine.
	"Add 'Classic' New Morph menu"
	catDict _ Dictionary new.
	Morph allSubclassesDo: [ :eaSubclass |
		eaSubclass includeInNewMorphMenu ifTrue: [ | category |
			(eaSubclass respondsTo: #categoryInNewMorphMenu)
				ifTrue: [ category _ eaSubclass categoryInNewMorphMenu ]
				ifFalse: [ category _ 'Unknown' ].
			(catDict includesKey: category)
				ifTrue: [ (catDict at: category) add: eaSubclass ]
				ifFalse: [
					catDict
						at: category
						put: (OrderedCollection with: eaSubclass) ]]].
	catDict keys sort do: [ :categ | | morphsInCat |
		morphsInCat _ (catDict at: categ) asArray sort: [ :c1 :c2 |
			c1 name < c2 name ].
		morphsInCat size > self pvtMaxItemsPerCategorySubMenu
			ifTrue: [
				menu
					add: categ
					subMenu:
						(self pvtNewMenuForSplitLists:
							(self
								splitNewMorphList: morphsInCat
								depth: self pvtCategorySplitListDepth )) ]
			ifFalse: [ | subMenu |
				subMenu _ MenuMorph new.
				morphsInCat do: [ :cl |
					subMenu
						add: cl name
						target: self
						action: #newMorphOfClass:event:
						argument: cl ].
				menu
					add: categ
					subMenu: subMenu ]].
	self doPopUp: menu.
</details>

#### TheWorldMenu>>#openDebugMenu

<details>
	<summary>See more</summary>
	
	openDebugMenu

	self doPopUp: self debugMenu
</details>

#### TheWorldMenu>>#menu: titleString

Create a menu with the given title, ready for filling


<details>
	<summary>See more</summary>
	
	menu: titleString
	"Create a menu with the given title, ready for filling"

	| menu |
	(menu _ MenuMorph entitled: titleString) 
		defaultTarget: self; 
		addStayUpIcons.
	^ menu

</details>

#### TheWorldMenu>>#fullScreenOn

<details>
	<summary>See more</summary>
	
	fullScreenOn

	Display fullScreenMode: true.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreDisplay
</details>

#### TheWorldMenu>>#quitSession

<details>
	<summary>See more</summary>
	
	quitSession
	Smalltalk 
		snapshot: (self confirm: 'Save the current image before quitting?' orCancel: [^ self])
		andQuit: true
		clearAllClassState: false
</details>

#### TheWorldMenu>>#openHelpMenu

Build and show the help menu for the world.


<details>
	<summary>See more</summary>
	
	openHelpMenu
	"Build and show the help menu for the world."

	self doPopUp: self helpMenu
</details>

#### TheWorldMenu>>#myWorld

<details>
	<summary>See more</summary>
	
	myWorld
	^ myWorld
</details>

#### TheWorldMenu>>#saveWorldInFile

Save the world's submorphs, model, and stepList in a file.


<details>
	<summary>See more</summary>
	
	saveWorldInFile
	"Save the world's submorphs, model, and stepList in a file.  "

	| fileName |
	fileName _ FillInTheBlankMorph request: 'File name for this morph?'.
	fileName isEmpty ifTrue: [^ self].  "abort"

	"Save only model, stepList, submorphs in this world"
	myWorld submorphsDo: [ :m |
		m allMorphsDo: [ :subM | subM prepareToBeSaved ]].	"Amen"

	(fileName, '.morph') asFileEntry writeStreamDo: [ :fileStream |
		fileStream fileOutObject: myWorld ]
</details>

#### TheWorldMenu>>#world: aWorld hand: aHand

<details>
	<summary>See more</summary>
	
	world: aWorld hand: aHand

	myWorld _ aWorld.
	myHand _ aHand.
</details>

#### TheWorldMenu>>#openTerseGuide

In TerseGuide.pck


<details>
	<summary>See more</summary>
	
	openTerseGuide
	"In TerseGuide.pck"
	Smalltalk at: #TerseGuideWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Terse Guide is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #'TerseGuide'] 
	].
	Smalltalk at: #TerseGuideWindow ifPresent: [ :cls | ^cls openTerseGuide ].
	self inform: 'Please install optional package TerseGuide.pck.st', String newLineString, 'Feature require: #''TerseGuide'' '
</details>

#### TheWorldMenu>>#saveAndQuit

<details>
	<summary>See more</summary>
	
	saveAndQuit

	Smalltalk snapshot: true andQuit: true clearAllClassState: false
</details>

#### TheWorldMenu>>#pvtCategorySplitListDepth

Split factor. A higher number results in fewer items in each submenu


<details>
	<summary>See more</summary>
	
	pvtCategorySplitListDepth
	"Split factor.  A higher number results in fewer items in each submenu"
	^ 2.
</details>

#### TheWorldMenu>>#alphabeticalMorphMenu

<details>
	<summary>See more</summary>
	
	alphabeticalMorphMenu
	| list splitLists |
	list := Morph withAllSubclasses select: [ :m |
		m includeInNewMorphMenu ].
	list := list asArray sort: [ :c1 :c2 |
		c1 name < c2 name ].
	splitLists := self
		splitNewMorphList: list
		depth: self pvtAlphaSplitListDepth .
	^ self pvtNewMenuForSplitLists: splitLists 

</details>

#### TheWorldMenu>>#openWindowsMenu

Build the windows menu for the world.


<details>
	<summary>See more</summary>
	
	openWindowsMenu
	"Build the windows menu for the world."

	self doPopUp: self windowsMenu
</details>

#### TheWorldMenu>>#fullScreenOff

<details>
	<summary>See more</summary>
	
	fullScreenOff

	Display fullScreenMode: false.
	DisplayScreen checkForNewScreenSize.
	myWorld restoreDisplay
</details>

## UpdatingMenuItemMorph

A menu item whose textual label and whose enablement are updatable. The wordingProvider provides the current wording, upon being being sent the wordingSelector. The item can also dynamically update whether or not it should be enabled; to do this, give it an enablementSelector, which is also sent to the wordingProvider..

### Methods
#### UpdatingMenuItemMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #UpdatingMenuItemMorph or: [ super is: aSymbol ]
</details>

#### UpdatingMenuItemMorph>>#stepAt: millisecondSinceLast

Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The desired time between steps is specified by this morph's answer to the stepTime message. The millisecondSinceLast parameter gives the time elapsed since the previous step.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	super stepAt: millisecondSinceLast.
	self updateContents
</details>

#### UpdatingMenuItemMorph>>#updateContents

Update the receiver's contents


<details>
	<summary>See more</summary>
	
	updateContents
	"Update the receiver's contents"

	| newString nArgs |
	((wordingProvider isNil) or: [wordingSelector isNil]) ifFalse: [
		nArgs _ wordingSelector numArgs.
		newString _ nArgs = 0
			ifTrue: [
				wordingProvider perform: wordingSelector]
			ifFalse: [
				nArgs = arguments size ifTrue: [
					wordingProvider perform: wordingSelector withArguments: arguments]].
		newString = (contentString ifNil: [ contents ])
			ifFalse: [
				self contents: newString ]]
</details>

#### UpdatingMenuItemMorph>>#wordingProvider: aProvider wordingSelector: aSelector

<details>
	<summary>See more</summary>
	
	wordingProvider: aProvider wordingSelector: aSelector
	wordingProvider _ aProvider.
	wordingSelector _ aSelector
</details>

#### UpdatingMenuItemMorph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime
	^ 1200
</details>

#### UpdatingMenuItemMorph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME. Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^true
</details>

