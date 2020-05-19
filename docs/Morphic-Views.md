## BrowserCommentTextMorph

I am a TextModelMorph that knows enough to make myself invisible when necessary.

### Methods
#### BrowserCommentTextMorph>>#showPane

<details>
	<summary>See more</summary>
	
	showPane

	self show.
	proportionalHeight ifNotNil: [
		self layoutSpec proportionalHeight: proportionalHeight ].
	separator ifNotNil: [
		separatorHeight ifNotNil: [
			separator layoutSpec fixedHeight: separatorHeight ].
		separator show ]
</details>

#### BrowserCommentTextMorph>>#hidePane

<details>
	<summary>See more</summary>
	
	hidePane

	self hide.
	separator visible ifTrue: [
		proportionalHeight _ self layoutSpec proportionaLayoutlHeight.
		separatorHeight _ separator layoutSpec fixedOrMinimumLayoutHeight ].
	separator layoutSpec fixedHeight: 0.
	self layoutSpec proportionalHeight: 0.
	separator ifNotNil: [ separator hide ]
</details>

#### BrowserCommentTextMorph>>#separator: aMorph

<details>
	<summary>See more</summary>
	
	separator: aMorph
	separator _ aMorph
</details>

#### BrowserCommentTextMorph>>#update: anAspect

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: anAspect
	super update: anAspect.
	anAspect == #editSelection ifFalse: [ ^self ].
	model textProvider isEditingExistingClass
		ifTrue: [ self showPane ]
		ifFalse: [ self hidePane ]
</details>

## DraggeableButtonMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DraggeableButtonMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	isPressed _ false.
	mouseIsOver _ false.
	actWhen == #buttonUp
		ifTrue: [ self performAction ].
	self redrawNeeded
</details>

#### DraggeableButtonMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.

	grabSelector _ nil.
	dragSelector _ nil.
</details>

#### DraggeableButtonMorph>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseDown: We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition

	dragSelector ifNotNil: [
		model perform: dragSelector with: localEventPosition ]
</details>

#### DraggeableButtonMorph>>#dragSelector: aSymbol

<details>
	<summary>See more</summary>
	
	dragSelector: aSymbol
	dragSelector _ aSymbol
</details>

#### DraggeableButtonMorph>>#grabSelector: aSymbol

<details>
	<summary>See more</summary>
	
	grabSelector: aSymbol
	grabSelector _ aSymbol
</details>

#### DraggeableButtonMorph>>#isRoundButton

<details>
	<summary>See more</summary>
	
	isRoundButton
	^false
</details>

#### DraggeableButtonMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Inform the model that this button has been released.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Inform the model that this button has been released. "
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	grabSelector ifNotNil: [
		model perform: grabSelector with: localEventPosition ]
</details>

## HierarchicalListMorph

Display a hierarchical list of items. Each item should be wrapped with a ListItemWrapper. For a simple example, look at submorphsExample. For beefier examples, look at ObjectExplorer or FileList2.

### Methods
#### HierarchicalListMorph>>#autoExpand: trueOrFalse

<details>
	<summary>See more</summary>
	
	autoExpand: trueOrFalse

	autoExpand _ trueOrFalse
</details>

#### HierarchicalListMorph>>#expandedForm

<details>
	<summary>See more</summary>
	
	expandedForm

	^BitBltCanvas arrowOfDirection: #down size: 13
</details>

#### HierarchicalListMorph>>#processMouseMove: aMouseMoveEvent localPosition: localEventPosition

Reimplemented because we really want #mouseMove when a morph is dragged around


<details>
	<summary>See more</summary>
	
	processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]
</details>

#### HierarchicalListMorph>>#selection: item

Called to set a new selection. Updates both model and view.


<details>
	<summary>See more</summary>
	
	selection: item
	"Called to set a new selection.
	Updates both model and view."
	"Assumes scroller submorphs is exactly our list.
	Note: MAY NOT work right if list includes repeated items"

	self selectionIndex: (self indexForItem: item)
</details>

#### HierarchicalListMorph>>#numSelectionsInView

<details>
	<summary>See more</summary>
	
	numSelectionsInView
	^ self viewableHeight // self listItemHeight
</details>

#### HierarchicalListMorph>>#expandAllAsPer: aBlock

<details>
	<summary>See more</summary>
	
	expandAllAsPer: aBlock

	scroller submorphs isEmpty ifTrue: [^self].
	scroller submorphs first beFullyExpandedAsPer: aBlock.
	scroller adjustExtent.
	self setScrollDeltas
</details>

#### HierarchicalListMorph>>#getMenu

Answer the menu for this view


<details>
	<summary>See more</summary>
	
	getMenu
	"Answer the menu for this view"

	menuGetter ifNil: [^ nil].
	(menuGetter is: #MessageSend) ifTrue: [
		^menuGetter value ].
	menuGetter numArgs = 0 ifTrue: [
		^ mainView perform: menuGetter ].
	^ self error: 'The menuGetter has an unsupported number of arguments'
</details>

#### HierarchicalListMorph>>#scrollSelectionIntoView

Scroll my text into view if necessary and return true, else return false


<details>
	<summary>See more</summary>
	
	scrollSelectionIntoView

	selectedMorph ifNotNil: [
		self flag: #jmvVer2.	"traducir mejor el rectangulo..."
		self scrollToShow: ((scroller externalize: selectedMorph morphPosition) extent: selectedMorph morphExtent) ]
</details>

#### HierarchicalListMorph>>#doubleClick: aMouseButtonEvent localPosition: localEventPosition

Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	doubleClick: aMouseButtonEvent localPosition: localEventPosition

	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	^ self model perform: doubleClickSelector
</details>

#### HierarchicalListMorph>>#keyStroke: aKeyboardEvent

Process potential command keys


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args |
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	keystrokeActionSelector ifNil: [^self].
	(args _ keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aKeyboardEvent keyCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aKeyboardEvent keyCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'
</details>

#### HierarchicalListMorph>>#innerMorphClass

<details>
	<summary>See more</summary>
	
	innerMorphClass
	^InnerHierarchicalListMorph
</details>

#### HierarchicalListMorph>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	self highlightedMorph: nil.
	super mouseButton2Activity
</details>

#### HierarchicalListMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	highlightedMorph ifNil: [ ^self ].
	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect and: [ highlightedMorph == selectedMorph ])
		ifTrue: [ self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: highlightedMorph ].
	self highlightedMorph: nil
</details>

#### HierarchicalListMorph>>#list: aCollection

<details>
	<summary>See more</summary>
	
	list: aCollection

	| wereExpanded morphList |
	wereExpanded _ self currentlyExpanded.
	scroller removeAllMorphs.
	(aCollection isNil or: [aCollection isEmpty]) ifTrue: [^ self selectedMorph: nil].
	morphList _ OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aCollection 
		allowSorting: false
		withExpandedItems: wereExpanded
		atLevel: 0.
	self insertNewMorphs: morphList.
</details>

#### HierarchicalListMorph>>#handlesKeyboard

Return true if the receiver wishes to handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard

	^self visible
</details>

#### HierarchicalListMorph>>#notExpandedForm

<details>
	<summary>See more</summary>
	
	notExpandedForm

	^BitBltCanvas arrowOfDirection: #right size: 13
</details>

#### HierarchicalListMorph>>#mouseEnter: event

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]
</details>

#### HierarchicalListMorph>>#doubleClickSelector: aSymbol

<details>
	<summary>See more</summary>
	
	doubleClickSelector: aSymbol
	doubleClickSelector _ aSymbol
</details>

#### HierarchicalListMorph>>#maximumSelection

<details>
	<summary>See more</summary>
	
	maximumSelection

	^ scroller submorphs size

</details>

#### HierarchicalListMorph>>#addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent

<details>
	<summary>See more</summary>
	
	addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent

	| priorMorph newCollection firstAddition |
	priorMorph _ nil.
	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	firstAddition _ nil.
	newCollection do: [:item | 
		priorMorph _ self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: newIndent.
		firstAddition ifNil: [firstAddition _ priorMorph].
		morphList add: priorMorph.
		((item hasEquivalentIn: expandedItems) or: [priorMorph isExpanded]) ifTrue: [
			priorMorph beExpanded.
			priorMorph 
				addChildrenForList: self 
				addingTo: morphList
				withExpandedItems: expandedItems.
		].
	].
	^firstAddition
	

</details>

#### HierarchicalListMorph>>#toggleExpandedState: aMorph

<details>
	<summary>See more</summary>
	
	toggleExpandedState: aMorph
	aMorph toggleExpandedState.
	scroller adjustExtent.
	self setScrollDeltas
</details>

#### HierarchicalListMorph>>#navigateToBottom

<details>
	<summary>See more</summary>
	
	navigateToBottom
	
	self changeSelectionTo: self maximumSelection
</details>

#### HierarchicalListMorph>>#scrollDeltaHeight

Return the increment in pixels which this pane should be scrolled (normally a subclass responsibility).


<details>
	<summary>See more</summary>
	
	scrollDeltaHeight
	scroller hasSubmorphs ifFalse: [ ^1].
	^ scroller firstSubmorph morphHeight
</details>

#### HierarchicalListMorph>>#arrowKey: aKeyboardEvent

Handle a keyboard navigation event. Answer nil if not handled.


<details>
	<summary>See more</summary>
	
	arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil
</details>

#### HierarchicalListMorph>>#navigateDown

move down, wrapping to top if needed


<details>
	<summary>See more</summary>
	
	navigateDown
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection _ self visualSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection _ self minimumSelection ].
	self changeSelectionTo: nextSelection
</details>

#### HierarchicalListMorph>>#noteRemovalOfAll: aCollection

<details>
	<summary>See more</summary>
	
	noteRemovalOfAll: aCollection

	scroller removeAllMorphsIn: aCollection.
	(aCollection includes: selectedMorph) ifTrue: [ self setSelectedMorph: nil ]
</details>

#### HierarchicalListMorph>>#autoExpand

<details>
	<summary>See more</summary>
	
	autoExpand

	^autoExpand ifNil: [false]
</details>

#### HierarchicalListMorph>>#addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean

<details>
	<summary>See more</summary>
	
	addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean

	| priorMorph morphList newCollection |
	priorMorph _ nil.
	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	morphList _ OrderedCollection new.
	newCollection do: [:item | 
		priorMorph _ self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
	].
	scroller addAllMorphs: morphList after: parentMorph.
	^morphList
	

</details>

#### HierarchicalListMorph>>#insertNewMorphs: morphList

<details>
	<summary>See more</summary>
	
	insertNewMorphs: morphList

	scroller addAllMorphs: morphList.
	scroller adjustExtent.
	self setScrollDeltas.
	self privateVisualSelection: self getCurrentSelectionItem
</details>

#### HierarchicalListMorph>>#privateVisualSelectionIndex: idx

Called internally to select the index-th item. Does not update model


<details>
	<summary>See more</summary>
	
	privateVisualSelectionIndex: idx
	"Called internally to select the index-th item.
	Does not update model"
	self selectedMorph: (self listMorphAt: idx).
	self scrollSelectionIntoView
</details>

#### HierarchicalListMorph>>#navigateOnePageUp

<details>
	<summary>See more</summary>
	
	navigateOnePageUp

	self changeSelectionTo: (self minimumSelection max: self visualSelectionIndex - self numSelectionsInView)
</details>

#### HierarchicalListMorph>>#navigateOnePageDown

<details>
	<summary>See more</summary>
	
	navigateOnePageDown

	self changeSelectionTo: (self visualSelectionIndex + self numSelectionsInView min: self maximumSelection)
</details>

#### HierarchicalListMorph>>#expandAll

<details>
	<summary>See more</summary>
	
	expandAll

	^self expandAllAsPer: [:each | true]
</details>

#### HierarchicalListMorph>>#itemFromPoint: aPoint

Return the list element (morph) at the given point or nil if outside


<details>
	<summary>See more</summary>
	
	itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	^scroller itemFromPoint: (scroller internalize: aPoint)
</details>

#### HierarchicalListMorph>>#autoDeselect: trueOrFalse

<details>
	<summary>See more</summary>
	
	autoDeselect: trueOrFalse

	autoDeselect _ trueOrFalse
</details>

#### HierarchicalListMorph>>#navigateUp

move up, wrapping to bottom if needed


<details>
	<summary>See more</summary>
	
	navigateUp
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection _ self visualSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection _ self maximumSelection ].
	self changeSelectionTo: nextSelection
</details>

#### HierarchicalListMorph>>#minimumSelection

<details>
	<summary>See more</summary>
	
	minimumSelection
	^ 1
</details>

#### HierarchicalListMorph>>#indentingItemClass

<details>
	<summary>See more</summary>
	
	indentingItemClass
	
	^IndentingListItemMorph
</details>

#### HierarchicalListMorph>>#selectedMorph: aMorph

<details>
	<summary>See more</summary>
	
	selectedMorph: aMorph

	selectedMorph ifNotNil: [
		selectedMorph isSelected: false ].
	selectedMorph _ aMorph.
	selectedMorph ifNotNil: [
		selectedMorph isSelected: true ]
</details>

#### HierarchicalListMorph>>#getList

Answer the list to be displayed.


<details>
	<summary>See more</summary>
	
	getList 
	"Answer the list to be displayed."

	^(model perform: (getListSelector ifNil: [^#()])) ifNil: [#()]


</details>

#### HierarchicalListMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
</details>

#### HierarchicalListMorph>>#getCurrentSelectionItem

<details>
	<summary>See more</summary>
	
	getCurrentSelectionItem

	^model perform: (getSelectionSelector ifNil: [^nil])
	
</details>

#### HierarchicalListMorph>>#highlightedMorph: aMorph

<details>
	<summary>See more</summary>
	
	highlightedMorph: aMorph

	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: false ].
	highlightedMorph _ aMorph.
	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: true ]
</details>

#### HierarchicalListMorph>>#update: aSymbol

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aSymbol
	super update: aSymbol.
	aSymbol == getSelectionSelector 
		ifTrue: [
			self privateVisualSelection: self getCurrentSelectionItem.
			^self ].
	aSymbol == getListSelector 
		ifTrue: [
			self list: self getList.
			^self ].

	"Indeed not pretty"
	( aSymbol notEmpty and: [aSymbol first == #openPath]) 
		ifTrue: [
			^(scroller submorphs at: 1 ifAbsent: [^self]) 
				openPath: aSymbol allButFirst adaptor: #asString compare: #=]
</details>

#### HierarchicalListMorph>>#keyboardFocusChange: aBoolean

The message is sent to a morph when its keyboard focus changes. The given argument indicates that the receiver is gaining (versus losing) the keyboard focus. In this case, all we need to do is to redraw focus feedback


<details>
	<summary>See more</summary>
	
	keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	self redrawNeeded
</details>

#### HierarchicalListMorph>>#toggleExpandedState: aMorph event: event

self setSelectedMorph: aMorph.


<details>
	<summary>See more</summary>
	
	toggleExpandedState: aMorph event: event

	"self setSelectedMorph: aMorph."
	((self autoExpand or: [event shiftPressed]) and: [aMorph isExpanded not])
		ifTrue: [aMorph beFullyExpanded]
		ifFalse: [aMorph toggleExpandedState].
	scroller adjustExtent.
	self setScrollDeltas
</details>

#### HierarchicalListMorph>>#indexForItem: item

<details>
	<summary>See more</summary>
	
	indexForItem: item
	| i |
	item ifNil: [
		^ 0 ].
	i _ scroller submorphs findFirst: [ :m | m complexContents == item ].
	i > 0 ifTrue: [
		^ i ].
	i _ scroller submorphs findFirst: [ :m | m withoutListWrapper = item withoutListWrapper ].
	^ i
</details>

#### HierarchicalListMorph>>#navigateToTop

<details>
	<summary>See more</summary>
	
	navigateToTop
	
	self changeSelectionTo: self minimumSelection
</details>

#### HierarchicalListMorph>>#setSelectedMorph: aMorph

<details>
	<summary>See more</summary>
	
	setSelectedMorph: aMorph
	setSelectionSelector ifNil: [ ^ false ].
	model 
		perform: setSelectionSelector
		with: aMorph complexContents	."leave last wrapper in place"
	^ true

 
</details>

#### HierarchicalListMorph>>#navigateRight

<details>
	<summary>See more</summary>
	
	navigateRight
	| oldSelection nextSelection |
	oldSelection _ self visualSelectionIndex.
	nextSelection _ oldSelection.
	selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView ]
				ifFalse: [ nextSelection := oldSelection + 1 ]].
	self changeSelectionTo: nextSelection
</details>

#### HierarchicalListMorph>>#privateVisualSelection: item

Called internally to set a new selection. Does not update model


<details>
	<summary>See more</summary>
	
	privateVisualSelection: item
	"Called internally to set a new selection.
	Does not update model"

	self privateVisualSelectionIndex: (self indexForItem: item)
</details>

#### HierarchicalListMorph>>#listMorphAt:  idx

Called internally to select the index-th item.


<details>
	<summary>See more</summary>
	
	listMorphAt:  idx
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index _ idx min: scroller submorphs size max: 0.
	theMorph _ index = 0 ifTrue: [ nil ] ifFalse: [ scroller submorphs at: index ].
	^theMorph
</details>

#### HierarchicalListMorph>>#setSelectionIndex: idx

Change the model's selected item index to be anInteger.


<details>
	<summary>See more</summary>
	
	setSelectionIndex: idx
	"Change the model's selected item index to be anInteger."

	^self setSelectedMorph: (self listMorphAt: idx)
</details>

#### HierarchicalListMorph>>#scrollDeltaWidth

A guess -- assume that the width of a char is approx 1/2 the height of the font


<details>
	<summary>See more</summary>
	
	scrollDeltaWidth
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2



</details>

#### HierarchicalListMorph>>#selectedMorph

<details>
	<summary>See more</summary>
	
	selectedMorph
	^selectedMorph
</details>

#### HierarchicalListMorph>>#listItemHeight

This should be cleaned up. The list should get spaced by this parameter.


<details>
	<summary>See more</summary>
	
	listItemHeight
	"This should be cleaned up.  The list should get spaced by this parameter."
	^ 12
</details>

#### HierarchicalListMorph>>#model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

<details>
	<summary>See more</summary>
	
	model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	self model: anObject.
	getListSelector _ getListSel.
	getSelectionSelector _ getSelectionSel.
	setSelectionSelector _ setSelectionSel.
	mainView _ aMainView.
	menuGetter _ getMenuSel.
	keystrokeActionSelector _ keyActionSel.
	autoDeselect _ true.
	self list: self getList.
</details>

#### HierarchicalListMorph>>#visualSelectionIndex

<details>
	<summary>See more</summary>
	
	visualSelectionIndex
	^scroller submorphs indexOf: selectedMorph
</details>

#### HierarchicalListMorph>>#changeSelectionTo: nextSelection

<details>
	<summary>See more</summary>
	
	changeSelectionTo: nextSelection

	nextSelection = self visualSelectionIndex ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ]].
</details>

#### HierarchicalListMorph>>#currentlyExpanded

<details>
	<summary>See more</summary>
	
	currentlyExpanded

	^(scroller submorphs select: [ :each | each isExpanded]) collect: [ :each |
		each complexContents
	].
	
</details>

#### HierarchicalListMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle 
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ]
</details>

#### HierarchicalListMorph>>#navigateLeft

<details>
	<summary>See more</summary>
	
	navigateLeft
	| oldSelection nextSelection |
	oldSelection _ self visualSelectionIndex.
	nextSelection _ oldSelection.
	selectedMorph ifNotNil: [
		selectedMorph isExpanded
			ifTrue: [
				self toggleExpandedState: selectedMorph.
				self scrollSelectionIntoView ]
			ifFalse: [
				oldSelection > self minimumSelection ifTrue: [
					nextSelection _ (oldSelection-1 to: 1 by: -1) 
						detect: [ :i | ( scroller submorphs at: i) indentLevel < selectedMorph indentLevel ] 
						ifNone: [ oldSelection ]].
				]].
	self changeSelectionTo: nextSelection
</details>

#### HierarchicalListMorph>>#selectionIndex: anInteger

Public. Call to set selection. Usually, view is updated from model updates. If model updating fails (no model index setter defined) then just update visuals.


<details>
	<summary>See more</summary>
	
	selectionIndex: anInteger
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]
</details>

## HoverableButtonMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### HoverableButtonMorph>>#model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector

<details>
	<summary>See more</summary>
	
	model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector

	super model: anObject stateGetter: getStateSel action: actionSel label: nil.
	mouseEnterSelector _ aMouseEnterSelector.
	mouseLeaveSelector _ aMouseLeaveSelector.
</details>

#### HoverableButtonMorph>>#mouseEnter: event

The mouse entered the receiver


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	
	mouseEnterSelector ifNotNil: [ model perform: mouseEnterSelector ].
	^super mouseEnter: event
</details>

#### HoverableButtonMorph>>#mouseLeave: event

The mouse has left the area of the receiver


<details>
	<summary>See more</summary>
	
	mouseLeave: event
	
	mouseLeaveSelector ifNotNil: [ model perform: mouseLeaveSelector ].
	^super mouseLeave: event
</details>

## IndentingListItemMorph

An IndentingListItemMorph is a StringMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph. It will also display lines around the toggle if the #showLinesInHierarchyViews Preference is set. Instance variables: indentLevel <SmallInteger> the indent level, from 0 at the root and increasing by 1 at each level of the hierarchy. isExpanded <Boolean> true if this item is expanded (showing its children) complexContents <ListItemWrapper> an adapter wrapping my represented item that can answer its children, etc. firstChild <IndentingListItemMorph|nil> my first child, or nil if none container <SimpleHierarchicalListMorph> my container nextSibling <IndentingListItemMorph|nil> the next item in the linked list of siblings, or nil if none. icon a 16 x 16 form or nil Contributed by Bob Arning as part of the ObjectExplorer package. Don't blame him if it's not perfect. We wanted to get it out for people to play with.

### Methods
#### IndentingListItemMorph>>#withoutListWrapper

<details>
	<summary>See more</summary>
	
	withoutListWrapper

	^complexContents withoutListWrapper
</details>

#### IndentingListItemMorph>>#beFullyExpanded

<details>
	<summary>See more</summary>
	
	beFullyExpanded

	self beFullyExpandedAsPer: [:each | true]
</details>

#### IndentingListItemMorph>>#firstChild

<details>
	<summary>See more</summary>
	
	firstChild

	^firstChild
</details>

#### IndentingListItemMorph>>#isExpanded

<details>
	<summary>See more</summary>
	
	isExpanded

	^isExpanded
</details>

#### IndentingListItemMorph>>#initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

<details>
	<summary>See more</summary>
	
	initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	| o |
	container _ hostList.
	complexContents _ anObject.
	self initWithContents: anObject asString font: Preferences standardListFont emphasis: nil.
	indentLevel _ 0.
	isExpanded _ false.
 	nextSibling _ firstChild _ nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	o _ anObject withoutListWrapper.
	icon _ o ifNotNil: [ (o respondsTo: #icon) ifTrue: [ o icon ] ].
	indentLevel _ newLevel.

</details>

#### IndentingListItemMorph>>#beExpanded

<details>
	<summary>See more</summary>
	
	beExpanded

	isExpanded _ true
</details>

#### IndentingListItemMorph>>#indentLevel

<details>
	<summary>See more</summary>
	
	indentLevel

	^indentLevel
</details>

#### IndentingListItemMorph>>#toggleExpandedState

<details>
	<summary>See more</summary>
	
	toggleExpandedState
	| newChildren toDelete c |
	isExpanded _ isExpanded not.
	toDelete _ OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode |
			aNode recursiveAddTo: toDelete ]].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ]) ifFalse: [
		firstChild _ nil.
"	 	nextSibling _ firstChild _ nil."
		^ self redrawNeeded ].
	(c _ complexContents contents) isEmpty ifTrue: [ ^ self redrawNeeded ].
	newChildren _ container
		addSubmorphsAfter: self
		fromCollection: c
		allowSorting: true.
	firstChild _ newChildren first
</details>

#### IndentingListItemMorph>>#balloonText

Answer balloon help text or nil, if no help is available. NB: subclasses may override such that they programatically construct the text, for economy's sake, such as model phrases in a Viewer


<details>
	<summary>See more</summary>
	
	balloonText

	^complexContents balloonText ifNil: [super balloonText]
</details>

#### IndentingListItemMorph>>#isSelected: aBoolean

<details>
	<summary>See more</summary>
	
	isSelected: aBoolean

	isSelected _ aBoolean.
	isHighlighted _ false.
	self redrawNeeded
</details>

#### IndentingListItemMorph>>#toggleRectangle

<details>
	<summary>See more</summary>
	
	toggleRectangle

	^(12*indentLevel @ 0) extent: 12@extent y
</details>

#### IndentingListItemMorph>>#desiredWidth

<details>
	<summary>See more</summary>
	
	desiredWidth

	^(indentLevel * 12) + 12 + (self font widthOfStringOrText: self contents) + 10
</details>

#### IndentingListItemMorph>>#isSoleItem

<details>
	<summary>See more</summary>
	
	isSoleItem
	^self isFirstItem and: [ owner submorphs size = 1 ]
</details>

#### IndentingListItemMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	indentLevel _ 0.
	isExpanded _ false.
	isSelected _ false.
	isHighlighted _ false.
	icon _ nil
</details>

#### IndentingListItemMorph>>#hasToggle

<details>
	<summary>See more</summary>
	
	hasToggle
	^ complexContents hasContents
</details>

#### IndentingListItemMorph>>#inToggleArea: aPoint

<details>
	<summary>See more</summary>
	
	inToggleArea: aPoint

	^self toggleRectangle containsPoint: aPoint
</details>

#### IndentingListItemMorph>>#beFullyExpandedAsPer: aBlock

<details>
	<summary>See more</summary>
	
	beFullyExpandedAsPer: aBlock

	| allChildren |
	allChildren _ OrderedCollection new: 10.
	self recursiveAddTo: allChildren.
	allChildren do:
		[:each |
			| shouldExpandEach |
			shouldExpandEach := each canExpand and: [aBlock value: each].
			shouldExpandEach ~= each isExpanded ifTrue:
				[
					each toggleExpandedState.
					each beFullyExpandedAsPer: aBlock
				]
		]
</details>

#### IndentingListItemMorph>>#recursiveAddTo: aCollection

<details>
	<summary>See more</summary>
	
	recursiveAddTo: aCollection

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection].
	].
	aCollection add: self
	
</details>

#### IndentingListItemMorph>>#nextSibling: anotherMorph

<details>
	<summary>See more</summary>
	
	nextSibling: anotherMorph

	nextSibling _ anotherMorph
</details>

#### IndentingListItemMorph>>#mouseEnter: event

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	isHighlighted _ true.
	self redrawNeeded.
	^super mouseEnter: event
</details>

#### IndentingListItemMorph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged

	super fontPreferenceChanged.
	self font: Preferences standardListFont.
</details>

#### IndentingListItemMorph>>#addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems

<details>
	<summary>See more</summary>
	
	addChildrenForList: hostList addingTo: morphList withExpandedItems: expandedItems

	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode | aNode delete].
	].
	firstChild _ nil.
	complexContents hasContents ifFalse: [^self].
	firstChild _ hostList 
		addMorphsTo: morphList
		from: complexContents contents 
		allowSorting: true
		withExpandedItems: expandedItems
		atLevel: indentLevel + 1.
	
</details>

#### IndentingListItemMorph>>#complexContents

<details>
	<summary>See more</summary>
	
	complexContents

	^complexContents
</details>

#### IndentingListItemMorph>>#isFirstItem

<details>
	<summary>See more</summary>
	
	isFirstItem
	^owner submorphs first == self
</details>

#### IndentingListItemMorph>>#withSiblingsDo: aBlock

<details>
	<summary>See more</summary>
	
	withSiblingsDo: aBlock

	| node |
	node _ self.
	[node isNil] whileFalse: [
		aBlock value: node.
		node _ node nextSibling
	].
</details>

#### IndentingListItemMorph>>#isHighlighted: aBoolean

<details>
	<summary>See more</summary>
	
	isHighlighted: aBoolean

	isHighlighted _ aBoolean.
	self redrawNeeded
</details>

#### IndentingListItemMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	| tRect colorToUse sLeft aForm centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
			
	tRect _ self toggleRectangle.
	aForm _ isExpanded 
		ifTrue: [ container expandedForm ]
		ifFalse: [ container notExpandedForm ].
	centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.

	complexContents hasContents ifTrue: [
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		at: sLeft@0
		font: self fontToUse
		color: colorToUse
</details>

#### IndentingListItemMorph>>#openPath: anArray adaptor: aSymbol compare: comparison

<details>
	<summary>See more</summary>
	
	openPath: anArray adaptor: aSymbol compare: comparison

	anArray isEmpty ifTrue: [ ^container setSelectedMorph: nil ].
	self withSiblingsDo: [ :each | 
		(anArray first isNil or: [
			(each complexContents perform: aSymbol)
				perform: comparison
					with: (anArray first perform: aSymbol) ]) ifTrue: [
			each isExpanded ifFalse: [
				each toggleExpandedState.
				owner adjustExtent.
				container setScrollDeltas ].
			each redrawNeeded.
			anArray size = 1 ifTrue: [
				^container setSelectedMorph: each ].
			each firstChild ifNil: [^container setSelectedMorph: nil ].
			^each firstChild openPath: anArray allButFirst adaptor: aSymbol compare: comparison ]].
	^container setSelectedMorph: nil
</details>

#### IndentingListItemMorph>>#mouseLeave: event

Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseLeave: event
	isHighlighted _ false.
	self redrawNeeded.
	^super mouseEnter: event
</details>

#### IndentingListItemMorph>>#handlesMouseOver: event

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseOver: event
	^ true
</details>

#### IndentingListItemMorph>>#canExpand

<details>
	<summary>See more</summary>
	
	canExpand

	^complexContents hasContents
</details>

#### IndentingListItemMorph>>#nextSibling

<details>
	<summary>See more</summary>
	
	nextSibling

	^nextSibling
</details>

## InnerHierarchicalListMorph

The morph that displays the list in a HierarchicalListMorph.

### Methods
#### InnerHierarchicalListMorph>>#drawLinesFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor

<details>
	<summary>See more</summary>
	
	drawLinesFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor 
	| hasToggle |
	hasToggle _ anIndentingListItemMorph hasToggle.
	"Draw line from toggle to text"
	self
		drawLineToggleToTextFor: anIndentingListItemMorph
		on: aCanvas
		lineColor: lineColor
		hasToggle: hasToggle.

	"Draw the line from toggle to the nextSibling's toggle"
	anIndentingListItemMorph nextSibling ifNotNil: [
		self
			drawLinesToNextSiblingFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor
			hasToggle: hasToggle ].

	"If it have children and am expanded, draw a line to its first child"
	(anIndentingListItemMorph firstChild notNil and: [
			anIndentingListItemMorph isExpanded ]) ifTrue: [
		self
			drawLinesToFirstChildFor: anIndentingListItemMorph
			on: aCanvas
			lineColor: lineColor]
</details>

#### InnerHierarchicalListMorph>>#itemFromPoint: aPoint

Return the list element (morph) at the given point or nil if outside


<details>
	<summary>See more</summary>
	
	itemFromPoint: aPoint
	"Return the list element (morph) at the given point or nil if outside"
	| ptY last |
	self hasSubmorphs ifFalse: [ ^nil ].
	(aPoint > self morphTopLeft and: [ aPoint < extent ]) ifFalse: [ ^nil ].
	ptY _ aPoint y.
	"note: following assumes that submorphs are vertical, non-overlapping, and ordered"
	self firstSubmorph morphPosition y > ptY ifTrue: [ ^nil ].
	last _ self lastSubmorph.
	last morphPosition y + last morphHeight < ptY ifTrue: [ ^nil ].
	"now use binary search"
	^self 
		findSubmorphBinary: [ :m |
			(m morphPosition y <= ptY and: [ m morphPosition y + m morphHeight >= ptY ])
				ifTrue: [ 0 ] "found"
				ifFalse: [ m morphPosition y + (m morphHeight // 2) > ptY ifTrue: [-1] ifFalse: [1]]]
</details>

#### InnerHierarchicalListMorph>>#adjustExtent

And reposition submorphs


<details>
	<summary>See more</summary>
	
	adjustExtent
	"And reposition submorphs"
	| w h y |
	"make all items wide, so selection indicator is wide too"
	w _ self desiredWidth.
	y _ 0.
	self submorphsDo: [ :m |
		h _ m morphHeight.
		m morphPosition: 0@y extent: w@h.
		y _ y + h ].
	self morphExtent: w@y
</details>

#### InnerHierarchicalListMorph>>#drawLinesOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawLinesOn: aCanvas 
	| lColor bottomY topY tx clipRect |
	lColor _ Theme current line.
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	topY _ (tx internalizePosition: clipRect topLeft) y min: (tx internalizePosition: clipRect topRight) y.
	bottomY _ (tx internalizePosition: clipRect bottomLeft) y max: (tx internalizePosition: clipRect bottomRight) y.
	self submorphs do: [ :submorph | 
		(submorph isExpanded or: [
			(submorph morphPosition y between: topY and: bottomY) or: [
				submorph nextSibling notNil and: [
					submorph nextSibling morphPosition y between: topY and: bottomY ] ] ])
		ifTrue: [
			self
				drawLinesFor: submorph
				on: aCanvas
				lineColor: lColor ]]
	
</details>

#### InnerHierarchicalListMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	Preferences showLinesInHierarchyViews ifTrue:[
		self drawLinesOn: aCanvas ]
</details>

#### InnerHierarchicalListMorph>>#drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle

If I am not the only item in my container, draw the line between: - my toggle (if any) or my left edge (if no toggle) - and my text left edge


<details>
	<summary>See more</summary>
	
	drawLineToggleToTextFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	"If I am not the only item in my container, draw the line between:
		- my toggle (if any) or my left edge (if no toggle)
		- and my text left edge"

	| aMorphCenter hLineY hLineLeft rect right |
	anIndentingListItemMorph isSoleItem ifTrue: [ ^ self ].
	rect _ anIndentingListItemMorph toggleRectangle.
	aMorphCenter _ anIndentingListItemMorph externalize: rect center.
	right _ (anIndentingListItemMorph externalize: rect rightCenter) x.
	hLineY _ aMorphCenter y.
	hasToggle
		ifTrue: [ hLineLeft _ right - 3 ]
		ifFalse: [ hLineLeft _ aMorphCenter x - 1 ].
	"Draw line from toggle to text"
	aCanvas
		line: hLineLeft @ hLineY
		to: right + 0 @ hLineY
		width: 1
		color: lineColor
</details>

#### InnerHierarchicalListMorph>>#drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle

<details>
	<summary>See more</summary>
	
	drawLinesToNextSiblingFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor hasToggle: hasToggle
	| vLineX aMorphCenter vLineTop vLineBottom nextSibCenter nextSibling |

	nextSibling _ anIndentingListItemMorph nextSibling.
	nextSibCenter _ nextSibling externalize: nextSibling toggleRectangle center.

	aMorphCenter _ anIndentingListItemMorph externalize:
		 anIndentingListItemMorph toggleRectangle center.
	vLineX _ aMorphCenter x - 1.
	hasToggle
		ifTrue: [ vLineTop _ aMorphCenter y + 5 ]
		ifFalse: [ vLineTop _ aMorphCenter y ].
	nextSibling hasToggle
		ifTrue: [ vLineBottom _ nextSibCenter y - 7 ]
		ifFalse: [ vLineBottom _ nextSibCenter y ].
	"Draw line from me to next sibling"
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @ vLineBottom
		width: 1
		color: lineColor
</details>

#### InnerHierarchicalListMorph>>#drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor

Draw line from me to next sibling


<details>
	<summary>See more</summary>
	
	drawLinesToFirstChildFor: anIndentingListItemMorph on: aCanvas lineColor: lineColor
	"Draw line from me to next sibling"

	| vLineX vLineTop vLineBottom childCenter firstChild |
	"child in the drawn tree. it is acually our submorph"
	firstChild _ anIndentingListItemMorph firstChild.
	childCenter _ firstChild externalize: firstChild toggleRectangle center.
	vLineX _ childCenter x - 1.
	vLineTop _ (anIndentingListItemMorph
		externalize: anIndentingListItemMorph morphExtent) y.
	firstChild hasToggle
		ifTrue: [ vLineBottom _ childCenter y - 7 ]
		ifFalse: [ vLineBottom _ childCenter y ].
	aCanvas
		line: vLineX @ vLineTop
		to: vLineX @vLineBottom
		width: 1
		color: lineColor
</details>

#### InnerHierarchicalListMorph>>#desiredWidth

<details>
	<summary>See more</summary>
	
	desiredWidth
	^submorphs inject: owner viewableWidth into: [ :previousValue :each |
		previousValue max: each desiredWidth ]
</details>

## InnerListMorph

The morph that displays the list in a PluggableListMorph.

### Methods
#### InnerListMorph>>#drawHighlightOn: aCanvas

Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'


<details>
	<summary>See more</summary>
	
	drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: highlightedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: ((Theme current listHighlightFocused: owner hasKeyboardFocus) alpha: 0.3)
</details>

#### InnerListMorph>>#flashRow: aRow

<details>
	<summary>See more</summary>
	
	flashRow: aRow

	self world ifNotNil: [ :world | world canvas ifNotNil: [ :canvas | 
		Display flash: (canvas externalizeDisplayBounds: (self drawBoundsForRow: aRow) from: self) ]].
	

</details>

#### InnerListMorph>>#draw: item atRow: row on: canvas

display the given item at row row


<details>
	<summary>See more</summary>
	
	draw: item atRow: row on: canvas
	"display the given item at row row"
	| f c |
	(item is: #Text)
		ifTrue: [
			f _ font emphasized: (item emphasisAt: 1).
			c _ (item colorAt: 1) ifNil: [Theme current text]]
		ifFalse: [ 
			f _ font.
			c _  Theme current text].
	canvas
		drawString: item
		at: 0 @ (self drawYForRow: row)
		font: f
		color: c
</details>

#### InnerListMorph>>#selectedRow

return the currently selected row, or nil if none is selected


<details>
	<summary>See more</summary>
	
	selectedRow
	"return the currently selected row, or nil if none is selected"
	^selectedRow
</details>

#### InnerListMorph>>#bottomVisibleRowForCanvas: aCanvas

return the bottom visible row in aCanvas's clip rectangle


<details>
	<summary>See more</summary>
	
	bottomVisibleRowForCanvas: aCanvas
	"return the bottom visible row in aCanvas's clip rectangle"
	| tx clipRect |
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	^ (self rowAtLocation: (tx internalizePosition: clipRect bottomLeft))
		max: (self rowAtLocation: (tx internalizePosition: clipRect bottomRight))
</details>

#### InnerListMorph>>#drawBoundsForRow: row

calculate the bounds that row should be drawn at. This might be outside our bounds!


<details>
	<summary>See more</summary>
	
	drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!"

	self flag: #jmvVer2.
	"revisar senders"
	^ 0 @ (self drawYForRow: row) extent: extent x @ font lineSpacing
</details>

#### InnerListMorph>>#drawSelectionOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)
</details>

#### InnerListMorph>>#getListItem: index

grab a list item directly from the model


<details>
	<summary>See more</summary>
	
	getListItem: index
	"grab a list item directly from the model"
	^owner getListItem: index
</details>

#### InnerListMorph>>#widthToDisplayItem: item

<details>
	<summary>See more</summary>
	
	widthToDisplayItem: item
	^self font widthOfStringOrText: item
	
</details>

#### InnerListMorph>>#item: index

return the index-th item, using the 'listItems' cache


<details>
	<summary>See more</summary>
	
	item: index
	"return the index-th item, using the 'listItems' cache"
	(index between: 1 and: listItems size)
		ifFalse: [ "there should have been an update, but there wasn't!"  ^self getListItem: index].
	(listItems at: index) ifNil: [ 
		listItems at: index put: (self getListItem: index). ].
	^listItems at: index
</details>

#### InnerListMorph>>#desiredWidth

Ok, this is a bit messed up. We need to return the width of the widest item in the list. If we grab every item in the list, it defeats the purpose of LazyListMorph. If we don't, then we don't know the size. This is a compromise -- if the list is less then 30 items, we grab them all. If not, we grab currently visible ones, until we've checked itemsToCheck of them, then take the max width out of that 'sampling', then double it. If you know a better way, please chime in.


<details>
	<summary>See more</summary>
	
	desiredWidth
"Ok, this is a bit messed up. We need to return the width of the widest item in the list. If we grab every item in the list, it defeats the purpose of LazyListMorph. If we don't, then we don't know the size. This is a compromise -- if the list is less then 30 items, we grab them all. If not, we grab currently visible ones, until we've checked itemsToCheck of them, then take the max width out of that 'sampling', then double it. If you know a better way, please chime in."

	| maxW count itemsToCheck item |

	itemsToCheck _ 30.
	maxW _ owner viewableWidth.
	count _ 0.
	listItems do: [ :each |
		each ifNotNil: [
			maxW _ maxW max: (self widthToDisplayItem: each contents)]].

	(count < itemsToCheck) ifTrue: [
		1 to: listItems size do: [:i | 
			(listItems at: i) ifNil: [ 
				item _ self item: i.
				maxW _ maxW max: (self widthToDisplayItem: item contents).
				((count _ count + 1) > itemsToCheck) ifTrue: [ ^maxW * 2 ]]]].

	^maxW 

</details>

#### InnerListMorph>>#highlightedRow: n

<details>
	<summary>See more</summary>
	
	highlightedRow: n
	highlightedRow _ n.
	self redrawNeeded
</details>

#### InnerListMorph>>#drawYForRow: row

calculate the vertical position that row should be drawn at. This might be outside our bounds!


<details>
	<summary>See more</summary>
	
	drawYForRow: row
	"calculate the vertical position that row should be drawn at.  This might be outside our bounds!"
	^ row - 1 * font lineSpacing
</details>

#### InnerListMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	self color: `Color black`.
	font _ Preferences standardListFont.
	listItems _ #().
	selectedRow _ nil.
	highlightedRow _ nil
</details>

#### InnerListMorph>>#drawBackgroundForMulti: row on: aCanvas

shade the background darker, if this row is selected


<details>
	<summary>See more</summary>
	
	drawBackgroundForMulti: row on: aCanvas
	| selectionDrawBounds c |
	"shade the background darker, if this row is selected"
	selectionDrawBounds _ self drawBoundsForRow: row.
	selectionDrawBounds _ selectionDrawBounds intersect: self morphLocalBounds.
	c _ (selectedRow notNil and: [ row = selectedRow])
		ifTrue: [ Theme current listHighlightFocused: owner hasKeyboardFocus ]
		ifFalse: [ Theme current listMultiHighlightFocused: owner hasKeyboardFocus ].
	aCanvas fillRectangle: selectionDrawBounds color: c
</details>

#### InnerListMorph>>#listChanged

set newList to be the list of strings to display


<details>
	<summary>See more</summary>
	
	listChanged
	"set newList to be the list of strings to display"
	listItems _ Array new: self getListSize withAll: nil.
	selectedRow _ nil.
	self adjustExtent
</details>

#### InnerListMorph>>#rowAtLocation: aPoint

return the number of the row at aPoint


<details>
	<summary>See more</summary>
	
	rowAtLocation: aPoint
	"return the number of the row at aPoint"

	listItems isEmpty ifTrue: [ ^0 ].
	^aPoint y // font lineSpacing + 1 min: listItems size max: 1
</details>

#### InnerListMorph>>#font

return the font used for drawing. The response is never nil


<details>
	<summary>See more</summary>
	
	font
	"return the font used for drawing.  The response is never nil"
	^font
</details>

#### InnerListMorph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged

	super fontPreferenceChanged.
	self font: Preferences standardListFont.
</details>

#### InnerListMorph>>#topVisibleRowForCanvas: aCanvas

return the top visible row in aCanvas's clip rectangle


<details>
	<summary>See more</summary>
	
	topVisibleRowForCanvas: aCanvas
	"return the top visible row in aCanvas's clip rectangle"
	| tx clipRect |
	tx _ aCanvas currentTransformation.
	clipRect _ aCanvas clipRect.
	^ (self rowAtLocation: (tx internalizePosition: clipRect topLeft))
		min: (self rowAtLocation: (tx internalizePosition: clipRect topRight))
</details>

#### InnerListMorph>>#noSelection

<details>
	<summary>See more</summary>
	
	noSelection
	selectedRow _ nil.
	highlightedRow _ nil
</details>

#### InnerListMorph>>#adjustExtent

Adjust our height to match the underlying list, but make it wider if neccesary to fill the available width in our PluggableListMorph (this is needed to make the selection indicator no narrower than the list)


<details>
	<summary>See more</summary>
	
	adjustExtent
	"Adjust our height to match the underlying list,
	but make it wider if neccesary to fill the available width in our PluggableListMorph
	(this is needed to make the selection indicator no narrower than the list)"
	self morphExtent:
		self desiredWidth @ ((listItems size max: 1) * font lineSpacing)

</details>

#### InnerListMorph>>#font: newFont

<details>
	<summary>See more</summary>
	
	font: newFont
	font _ newFont ifNil: [ AbstractFont default ].
	self adjustExtent
</details>

#### InnerListMorph>>#rowAtLocation: aPoint ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	rowAtLocation: aPoint ifNone: aNoneBlock

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // font lineSpacing + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: aNoneBlock
</details>

#### InnerListMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	listItems size = 0 ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) displayStringOrText atRow: row on: aCanvas ]
</details>

#### InnerListMorph>>#getListSize

return the number of items in the list


<details>
	<summary>See more</summary>
	
	getListSize
	"return the number of items in the list"
	^owner getListSize
</details>

#### InnerListMorph>>#selectedRow: index

select the index-th row. if nil, remove the current selection


<details>
	<summary>See more</summary>
	
	selectedRow: index
	"select the index-th row.  if nil, remove the current selection"
	selectedRow _ index.
	highlightedRow _ nil.
	self redrawNeeded
</details>

## InnerPluggableMorph

The morph that displays the list in a PluggableScrollPane

### Methods
#### InnerPluggableMorph>>#adjustExtent

<details>
	<summary>See more</summary>
	
	adjustExtent
	self flag: #jmvVer2.	"Do it just adding submorph extents!"
	"
	self submorphBounds ifNotNil: [ :r |
		self morphExtent: r bottomRight ]
	"
</details>

## InnerTextMorph

InnerTextMorphs support display of text with emphasis. They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text. They are 'bare' in the sense that they can not clip contents to some window, or scroll it by themselves. Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter. Comment about Shout specifics: ----------------------------------------- Instances of me are usually created using my #on:text:accept:readSelection:menu: class method. In order to colour the text, I use an instance of SHTextStylerST80, which I store in my 'styler' instance variable. When my setText: method is called, I use my styler to ... a) optionally set all assignments to ansi or leftArrow. b) Colour my text (immediately, if the text is less than 4096 chars in length, or in a backgroundProcess otherwise) When my text is changed, my hasUnacceptedEdits: method is called with true, and I ask my styler to re-colour my text. This is performed in a background process so that typing remains responsive regardless of the length of the text. Just before my styler is about to format/style the text, I send #stylerAboutToStyle: to my model. This gives my model a chance to veto the styling (by answering false), or to initialize the styler with information it needs in order to parse the text correctly (e.g. the class to which a method belongs, or the workspace in which I am contained). My styler informs me that it has finished styling by triggering the #shoutStyled event which I handle. I then update the textAttributes of my text and refresh the display. My 'unstyledAcceptText' instance variable is used in conjunction with my #acceptTextInModel and #correctFrom:to:with: methods to ensure that when my text is modified during a method compilation (removing unused vars etc), I do not lose those changes.

### Methods
#### InnerTextMorph>>#processKeystroke: aKeyboardEvent localPosition: localEventPosition

System level event handling.


<details>
	<summary>See more</summary>
	
	processKeystroke: aKeyboardEvent localPosition: localEventPosition
	"System level event handling."
		"localEventPosition?????"

	aKeyboardEvent wasHandled ifTrue:[^self].
	self handlesKeyboard ifFalse: [^ self].
	aKeyboardEvent wasHandled: true.
	self keyStroke: aKeyboardEvent
</details>

#### InnerTextMorph>>#contents: stringOrText

<details>
	<summary>See more</summary>
	
	contents: stringOrText
	^ self contentsAsIs: stringOrText
</details>

#### InnerTextMorph>>#textColor: aColor

<details>
	<summary>See more</summary>
	
	textColor: aColor

	color = aColor ifTrue: [^ self].
	color _ aColor.
	self redrawNeeded
</details>

#### InnerTextMorph>>#escAction

Return the action to perform when user presses <Esc> key


<details>
	<summary>See more</summary>
	
	escAction

	"Return the action to perform when user presses <Esc> key"

	^self valueOfProperty: #escAction
</details>

#### InnerTextMorph>>#installEditorAndTextComposition

Install an editor for my textComposition. Install also the textComposition.


<details>
	<summary>See more</summary>
	
	installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ model editorClass new morph: self.
	e model: model.
	tc _ TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc ].
	tc
		setModel: model;
		extentForComposing: self extentForComposing.
	e textComposition: tc.
	tc editor: e.
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		(Preferences syntaxHighlightingAsYouType ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass
</details>

#### InnerTextMorph>>#privateExtent: aPoint

Resist changing the extent if no wordwrap. this should be checked.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint
	| newExtent |

	"Resist changing the extent if no wordwrap. this should be checked."
	wrapFlag ifFalse: [ ^ false ].
	
	"Just update width. Height is set by ourselves. See #fit"
	newExtent _ aPoint x truncated @ extent y.

	^ (super privateExtent: newExtent)
		ifTrue: [
			self resetTextComposition.
			self editor recomputeSelection.	
			self updateFromTextComposition ]; yourself
</details>

#### InnerTextMorph>>#fit

Adjust my bounds to fit the text. Required after the text changes, or if wrapFlag is true and the user attempts to change the extent.


<details>
	<summary>See more</summary>
	
	fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: AbstractFont default lineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y
</details>

#### InnerTextMorph>>#processMouseMove: aMouseMoveEvent localPosition: localEventPosition

Re-implemented to allow for mouse-up move events


<details>
	<summary>See more</summary>
	
	processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Re-implemented to allow for mouse-up move events"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1]
</details>

#### InnerTextMorph>>#acceptOnCR: trueOrFalse

<details>
	<summary>See more</summary>
	
	acceptOnCR: trueOrFalse
	acceptOnCR _ trueOrFalse
</details>

#### InnerTextMorph>>#getMenu

<details>
	<summary>See more</summary>
	
	getMenu

	^self editor getMenu
</details>

#### InnerTextMorph>>#scrollSelectionIntoView

<details>
	<summary>See more</summary>
	
	scrollSelectionIntoView

	(owner is: #ScrollPane) ifTrue: [
		owner scrollSelectionIntoView ]
</details>

#### InnerTextMorph>>#defaultColor

Return the default fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"Return the default fill style for the receiver"
	^ Theme current text
</details>

#### InnerTextMorph>>#textColor

<details>
	<summary>See more</summary>
	
	textColor

	^ color
</details>

#### InnerTextMorph>>#someSubmorphPositionOrExtentChanged

Our extent, or some submorph changed. Must layout submorphs again.


<details>
	<summary>See more</summary>
	
	someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	textComposition ifNotNil: [ 
		textComposition composeAll.
		self fit.
		self selectionChanged ]
</details>

#### InnerTextMorph>>#doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor doubleClickAndHalf ].
	owner scrollSelectionIntoView
</details>

#### InnerTextMorph>>#keyStroke: aKeyboardEvent

Handle a keystroke event.


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	autoCompleter 
		ifNil: [ self processKeyStroke: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeyStroke: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].
			
	super keyStroke: aKeyboardEvent
</details>

#### InnerTextMorph>>#wrapFlag: aBoolean

Change whether contents are wrapped to the container.


<details>
	<summary>See more</summary>
	
	wrapFlag: aBoolean
	"Change whether contents are wrapped to the container."

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag _ aBoolean.

	"Compose my text to fit my bounds."
	self resetTextComposition.
	self editor recomputeSelection.	
	self updateFromTextComposition 
</details>

#### InnerTextMorph>>#wrapOnOff

<details>
	<summary>See more</summary>
	
	wrapOnOff
	self wrapFlag: wrapFlag not
</details>

#### InnerTextMorph>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]
</details>

#### InnerTextMorph>>#textComposition

textComposition instantiation is lazy -- create it only when needed


<details>
	<summary>See more</summary>
	
	textComposition
	"textComposition instantiation is lazy -- create it only when needed"
	textComposition ifNil: [ self installEditorAndTextComposition ].
	^textComposition
</details>

#### InnerTextMorph>>#onBlinkCursor

Blink the cursor


<details>
	<summary>See more</summary>
	
	onBlinkCursor
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textComposition lastTextCursorRect ifNotNil: [ :r | self invalidateLocalRect: r].
</details>

#### InnerTextMorph>>#askBeforeDiscardingEdits: aBoolean

Set the flag that determines whether the user should be asked before discarding unaccepted edits.


<details>
	<summary>See more</summary>
	
	askBeforeDiscardingEdits: aBoolean
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	askBeforeDiscardingEdits _ aBoolean
</details>

#### InnerTextMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self pauseBlinking.
	self handleInteraction: [ editor mouseButton1Up: aMouseButtonEvent  localPosition: localEventPosition ].
	owner scrollSelectionIntoView
</details>

#### InnerTextMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	wrapFlag _ true.
	acceptOnCR _ false.
	hasUnacceptedEdits _ false.
	hasEditingConflicts _ false.
	askBeforeDiscardingEdits _ true
</details>

#### InnerTextMorph>>#handlesKeyboard

Return true if the receiver wishes to handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard

	^self visible
</details>

#### InnerTextMorph>>#acceptOnCR

Answer whether the receiver wants to accept when the Return key is hit


<details>
	<summary>See more</summary>
	
	acceptOnCR
	"Answer whether the receiver wants to accept when the Return key is hit"

	^ acceptOnCR == true
</details>

#### InnerTextMorph>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent

	^(9@(AbstractFont default lineSpacing+2))
</details>

#### InnerTextMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #InnerTextMorph or: [ super is: aSymbol ]
</details>

#### InnerTextMorph>>#releaseCachedState

Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'.


<details>
	<summary>See more</summary>
	
	releaseCachedState

	super releaseCachedState.
	self releaseEditorAndTextComposition.

</details>

#### InnerTextMorph>>#clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self handleInteraction: [
		editor clickAndHalf ].
	owner scrollSelectionIntoView
</details>

#### InnerTextMorph>>#formatAndStyleIfNeeded

Apply both formatting (changes to the characters in the text, such as preferred assignment operators), and styling (TextAttributes to make Smalltalk code easier to understand)


<details>
	<summary>See more</summary>
	
	formatAndStyleIfNeeded
	"Apply both formatting (changes to the characters in the text, such as
	preferred assignment operators), and styling (TextAttributes to make
	Smalltalk code easier to understand)"

	model formatAndStyleIfNeededWith: styler
</details>

#### InnerTextMorph>>#pauseBlinking

Show a solid cursor (non blinking) for a short while


<details>
	<summary>See more</summary>
	
	pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	textComposition ifNotNil: [
		self showsBlinkingCursor ifTrue: [
			"Show cursor right now if needed"
			textComposition showTextCursor ifFalse: [
				textComposition showTextCursor: true ]]]
</details>

#### InnerTextMorph>>#crAction

Return the action to perform when user presses <Return> key


<details>
	<summary>See more</summary>
	
	crAction
	"Return the action to perform when user presses <Return> key"
	^self valueOfProperty: #crAction
</details>

#### InnerTextMorph>>#enterClickableRegion: aMorphicEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	enterClickableRegion: aMorphicEvent localPosition: localEventPosition

</details>

#### InnerTextMorph>>#enableEditing

<details>
	<summary>See more</summary>
	
	enableEditing
	
	self removeProperty: #disablesEditing
</details>

#### InnerTextMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^ true
</details>

#### InnerTextMorph>>#hasUnacceptedEdits: aBoolean

Set the hasUnacceptedEdits flag to the given value.


<details>
	<summary>See more</summary>
	
	hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean.
		owner redrawNeeded].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	"shout:  re-style the text iff aBoolean is true
	Do not apply any formatting (i.e. changes to the characters in the text),
	just styling (i.e. TextAttributes)"
	aBoolean ifTrue: [
		self formatAndStyleIfNeeded ]
</details>

#### InnerTextMorph>>#addCustomMenuItems: aCustomMenu hand: aHandMorph

Add text-related menu items to the menu


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: aCustomMenu hand: aHandMorph 
	"Add text-related menu items to the menu"

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu 
		addUpdating: #wrapString
		target: self
		action: #wrapOnOff
</details>

#### InnerTextMorph>>#disablesEditing

<details>
	<summary>See more</summary>
	
	disablesEditing

	^self hasProperty: #disablesEditing
</details>

#### InnerTextMorph>>#hasUnacceptedEdits

<details>
	<summary>See more</summary>
	
	hasUnacceptedEdits
	^hasUnacceptedEdits
</details>

#### InnerTextMorph>>#isWrapped

<details>
	<summary>See more</summary>
	
	isWrapped
	
	^wrapFlag
</details>

#### InnerTextMorph>>#hasEditingConflicts

Return true if a conflicting edit to the same code (typically) is known to have occurred after the current contents started getting edited


<details>
	<summary>See more</summary>
	
	hasEditingConflicts
	"Return true if a conflicting edit to the same code (typically) is known to have occurred after the current contents started getting edited"

	^ hasEditingConflicts
</details>

#### InnerTextMorph>>#canDiscardEdits

Return true if this view either has no text changes or does not care.


<details>
	<summary>See more</summary>
	
	canDiscardEdits
	"Return true if this view either has no text changes or does not care."
	^ (hasUnacceptedEdits & askBeforeDiscardingEdits) not
</details>

#### InnerTextMorph>>#addMorphFrontFromWorldPosition: aMorph

Overridden for more specific re-layout and positioning


<details>
	<summary>See more</summary>
	
	addMorphFrontFromWorldPosition: aMorph
	"Overridden for more specific re-layout and positioning"
	| positionInWorld |
	positionInWorld _ aMorph morphPositionInWorld.
	^self anchorMorph: aMorph at: positionInWorld
</details>

#### InnerTextMorph>>#chooseEmphasisOrAlignment

<details>
	<summary>See more</summary>
	
	chooseEmphasisOrAlignment
	self editor changeEmphasisOrAlignment.
	self updateFromTextComposition
</details>

#### InnerTextMorph>>#editor

Return my current editor, or install a new one.


<details>
	<summary>See more</summary>
	
	editor
	"Return my current editor, or install a new one."
	editor ifNil: [ self installEditorAndTextComposition ].
	^editor
</details>

#### InnerTextMorph>>#contentsAsIs: stringOrText

Accept new text contents with line breaks only as in the text. Fit my width and height to the result.


<details>
	<summary>See more</summary>
	
	contentsAsIs: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	wrapFlag _ false.
	model basicActualContents: stringOrText
</details>

#### InnerTextMorph>>#stylerClass: aTextStylerClass

<details>
	<summary>See more</summary>
	
	stylerClass: aTextStylerClass
	styler class == aTextStylerClass ifFalse: [
		styler _ aTextStylerClass ifNotNil: [
			aTextStylerClass new ].
		styler ifNotNil: [
			model ifNotNil: [
				styler textModel: model ]]]
</details>

#### InnerTextMorph>>#possiblyChanged

<details>
	<summary>See more</summary>
	
	possiblyChanged
	| embeddedMorphs |
	embeddedMorphs _ model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil ]].
	embeddedMorphs do: [ :each|
		each owner == self ifFalse: [
			self addMorphFront: each.
			each hide "Show it only when properly located"]].
	owner possiblyChanged
</details>

#### InnerTextMorph>>#mutex

<details>
	<summary>See more</summary>
	
	mutex
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex
</details>

#### InnerTextMorph>>#anchorMorph: aMorph at: aPoint

<details>
	<summary>See more</summary>
	
	anchorMorph: aMorph at: aPoint
	| relPt |
	aMorph owner == self ifTrue: [ self removeMorph: aMorph ].
	self addMorphFront: aMorph.
	relPt _ aPoint - self morphPositionInWorld.
	editor insertMorph: aMorph at: relPt.
	self fit.
</details>

#### InnerTextMorph>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseDown: We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent mouseButton1Pressed ifFalse: [
		^ self enterClickableRegion: aMouseMoveEvent localPosition: localEventPosition ].
	self handleInteraction: [
		editor mouseMove: aMouseMoveEvent localPosition: localEventPosition].
	owner scrollSelectionIntoView
</details>

#### InnerTextMorph>>#acceptContents

The message is sent when the user hits return or Cmd-S. Accept the current contents and end editing.


<details>
	<summary>See more</summary>
	
	acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar scrollValue.
	
	hasUnacceptedEdits ifFalse: [ self flash. ^true ].
	hasEditingConflicts ifTrue: [
		self confirmAcceptAnyway ifFalse: [self flash. ^false]].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			UISupervisor whenUIinSafeState: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar internalScrollValue: prevScrollValue].
			true]
		ifFalse: [ false ]
</details>

#### InnerTextMorph>>#hasEditingConflicts: aBoolean

<details>
	<summary>See more</summary>
	
	hasEditingConflicts: aBoolean

	hasEditingConflicts _ aBoolean
</details>

#### InnerTextMorph>>#stylerStyled

<details>
	<summary>See more</summary>
	
	stylerStyled

	self textComposition composeAll.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	self scrollSelectionIntoView
</details>

#### InnerTextMorph>>#model: aTextModel wrappedTo: width

Accept new text contents. Lay it out, wrapping to width. Then fit my height to the result.


<details>
	<summary>See more</summary>
	
	model: aTextModel wrappedTo: width
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	| newExtent |
	wrapFlag _ true.
	newExtent _ width truncated@extent y.
	extent = newExtent ifFalse: [
		self redrawNeeded.
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
	self model: aTextModel
</details>

#### InnerTextMorph>>#chooseFont

<details>
	<summary>See more</summary>
	
	chooseFont
	self editor offerFontMenu.
	self updateFromTextComposition.
</details>

#### InnerTextMorph>>#startBlinking

And show the cursor


<details>
	<summary>See more</summary>
	
	startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	textComposition ifNotNil: [ textComposition showTextCursor: true ].
	self startStepping: #onBlinkCursor stepTime: 500
</details>

#### InnerTextMorph>>#handleInteraction: interactionBlock

Perform the changes in interactionBlock, noting any change in selection and possibly a change in the composition


<details>
	<summary>See more</summary>
	
	handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the composition"

	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	self updateFromTextComposition
</details>

#### InnerTextMorph>>#selectAll

Tell my editor to select all the text


<details>
	<summary>See more</summary>
	
	selectAll
	"Tell my editor to select all the text"

	self editor selectAll.
	self redrawNeeded
</details>

#### InnerTextMorph>>#model: aTextModel

<details>
	<summary>See more</summary>
	
	model: aTextModel
	model _ aTextModel.
	styler ifNotNil: [ styler textModel: model ].
	self releaseEditorAndTextComposition.	"So the model is properly set on the editor and the text composition"
</details>

#### InnerTextMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Make this TextMorph be the keyboard input focus, if it isn't already, and repond to the text selection gesture.


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	"If we don't focus, Get focus, and do nothing else (the user will need to click again to do further interaction)"
	self hasKeyboardFocus ifFalse: [
		^aMouseButtonEvent hand newKeyboardFocus: self].

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
</details>

#### InnerTextMorph>>#autoCompleterClass: aTextCompleterClass

<details>
	<summary>See more</summary>
	
	autoCompleterClass: aTextCompleterClass
	autoCompleter class == aTextCompleterClass ifFalse: [
		autoCompleter _ aTextCompleterClass ifNotNil: [
			aTextCompleterClass withModel: model ].
		autoCompleter ifNotNil: [
			autoCompleter textMorph: self ]]
</details>

#### InnerTextMorph>>#removedMorph: aMorph

Notify the receiver that aMorph was just removed from its children


<details>
	<summary>See more</summary>
	
	removedMorph: aMorph

	editor removeMorph: aMorph.
	self fit.
	super removedMorph: aMorph
</details>

#### InnerTextMorph>>#stopBlinking

And do not show cursor anymore.


<details>
	<summary>See more</summary>
	
	stopBlinking
	"And do not show cursor anymore."
	self stopStepping: #onBlinkCursor.
	textComposition ifNotNil: [
		textComposition showTextCursor: false ]
</details>

#### InnerTextMorph>>#keyboardFocusChange: aBoolean

The message is sent to a morph when its keyboard focus changes. The given argument indicates that the receiver is gaining (versus losing) the keyboard focus. In this case, all we need to do is to redraw border feedback


<details>
	<summary>See more</summary>
	
	keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self showsBlinkingCursor ifTrue: [
				editor hasSelection ifFalse: [
					self startBlinking ]]]
		ifFalse: [ self stopBlinking ].
	"Selection might be shown differently when focused"
	owner
		ifNotNil: [ owner redrawNeeded ]
		ifNil: [ self redrawNeeded ]		"Or at least redraw us"
</details>

#### InnerTextMorph>>#showsBlinkingCursor

<details>
	<summary>See more</summary>
	
	showsBlinkingCursor

	^self handlesKeyboard and: [ self disablesEditing not ]
</details>

#### InnerTextMorph>>#escAction: aBlock

Sets the action to perform when user presses <Esc> key


<details>
	<summary>See more</summary>
	
	escAction: aBlock
	
	"Sets the action to perform when user presses <Esc> key"
	
	^self setProperty: #escAction toValue: aBlock 
</details>

#### InnerTextMorph>>#crAction: aBlock

Sets the action to perform when user presses <Return> key


<details>
	<summary>See more</summary>
	
	crAction: aBlock
	"Sets the action to perform when user presses <Return> key"
	^self setProperty: #crAction toValue: aBlock 
</details>

#### InnerTextMorph>>#releaseEditorAndTextComposition

Editor and TextComposition instantiation is lazy -- they will be created only when needed


<details>
	<summary>See more</summary>
	
	releaseEditorAndTextComposition
	"Editor and TextComposition instantiation is lazy -- they will be created only when needed"

	editor _ nil.
	textComposition _ nil
</details>

#### InnerTextMorph>>#extentForComposing

<details>
	<summary>See more</summary>
	
	extentForComposing
	self flag: #jmvVer2.	"like #extent ..."
	^wrapFlag
		ifTrue: [ extent x @ 9999999 ]
		ifFalse: [ 9999999@9999999 ]
</details>

#### InnerTextMorph>>#disregardUnacceptedEdits

Return true if this view either has no text changes or does not care.


<details>
	<summary>See more</summary>
	
	disregardUnacceptedEdits

	^ self hasUnacceptedEdits: false
</details>

#### InnerTextMorph>>#resetTextComposition

<details>
	<summary>See more</summary>
	
	resetTextComposition
	textComposition ifNotNil: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		editor storeSelectionInComposition ].
	self fit.
	self selectionChanged.
</details>

#### InnerTextMorph>>#processKeyStroke: evt

<details>
	<summary>See more</summary>
	
	processKeyStroke: evt
	| action |

	(acceptOnCR and: [evt isReturnKey]) ifTrue: [^ self acceptContents].

	self pauseBlinking.
	
	"Return - check for special action"
	evt isReturnKey ifTrue: [	
		action _ self crAction.
		action ifNotNil: [ ^action value]].
	
	"Esc - check for special action"
	evt isEsc ifTrue: [			
		action _ self escAction.
		action ifNotNil: [ ^action value]].
	
	self handleInteraction: [ editor processKeyStroke: evt ].
	self scrollSelectionIntoView
</details>

#### InnerTextMorph>>#adjustExtent

This is just a suggestion. If we do wordwrap, the width will be honored. But the height is whatever is appropriate for the contents! See #fit


<details>
	<summary>See more</summary>
	
	adjustExtent
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents! See #fit"
	self morphExtent: owner viewableExtent
</details>

#### InnerTextMorph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged

	super fontPreferenceChanged.
	hasUnacceptedEdits ifFalse: [
		model refetch ].
	self updateFromTextComposition.
</details>

#### InnerTextMorph>>#disableEditing

<details>
	<summary>See more</summary>
	
	disableEditing
	self setProperty: #disablesEditing toValue: true.
	self stopBlinking
</details>

#### InnerTextMorph>>#debugDrawLineRectsOn: aCanvas

Shows where text line rectangles are


<details>
	<summary>See more</summary>
	
	debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self textComposition lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: `Color brown` ]

</details>

#### InnerTextMorph>>#wrapString

Answer the string to put in a menu that will invite the user to switch word wrap mode


<details>
	<summary>See more</summary>
	
	wrapString
	"Answer the string to put in a menu that will invite the user to 
	switch word wrap mode"
	^ (wrapFlag
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'text wrap to bounds'
</details>

#### InnerTextMorph>>#drawOn: aCanvas

Draw the receiver on a canvas


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: self morphLocalBounds
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)
</details>

#### InnerTextMorph>>#confirmAcceptAnyway

<details>
	<summary>See more</summary>
	
	confirmAcceptAnyway

	^ self confirm: 
'Caution! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?'
</details>

#### InnerTextMorph>>#selectionChanged

<details>
	<summary>See more</summary>
	
	selectionChanged

	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	editor hasSelection
		ifTrue: [ self stopBlinking ]
		ifFalse: [ self hasKeyboardFocus ifTrue: [self startBlinking ]]
</details>

#### InnerTextMorph>>#updateFromTextComposition

A change has taken place in my textComposition, as a result of editing and I must be updated.


<details>
	<summary>See more</summary>
	
	updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		editor storeSelectionInComposition.
		self fit ].

	owner
		updateScrollBarsBounds;
		setScrollDeltas
</details>

#### InnerTextMorph>>#flash

Do nothing.


<details>
	<summary>See more</summary>
	
	flash
	^ owner flash
</details>

## PluggableButtonMorph

A PluggableButtonMorph is a combination of an indicator for a boolean value stored in its model and an action button. The action of a button is often, but not always, to toggle the boolean value that it shows. Its pluggable selectors are: getStateSelector fetch a boolean value from the model actionSelector invoke this button's action on the model getLabelSelector fetch this button's lable from the model getMenuSelector fetch a pop-up menu for this button from the model Any of the above selectors can be nil, meaning that the model does not supply behavior for the given action, and the default behavior should be used. For example, if getStateSelector is nil, then this button shows the state of a read-only boolean that is always false. The model informs its view(s) of changes by sending #changed: to itself with getStateSelector as a parameter. The view tells the model when the button is pressed by sending actionSelector. If the actionSelector takes one or more arguments, then the following are relevant: arguments A list of arguments to provide when the actionSelector is called. argumentsProvider The object that is sent the argumentSelector to obtain arguments, if dynamic argumentsSelector The message sent to the argumentProvider to obtain the arguments.

### Methods
#### PluggableButtonMorph>>#updateDownButtonImage

update the receiver's as a downButton. put a new image inside


<details>
	<summary>See more</summary>
	
	updateDownButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #down size: ScrollBar scrollbarThickness.
	self iconName: #drawDownIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded
</details>

#### PluggableButtonMorph>>#mouseStillDownStepRate

At what rate do I want to receive #mouseStillDown notifications?


<details>
	<summary>See more</summary>
	
	mouseStillDownStepRate
	"At what rate do I want to receive #mouseStillDown notifications?"
	^40
</details>

#### PluggableButtonMorph>>#privateExtent: aPoint

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			magnifiedIcon _ nil ]; yourself
</details>

#### PluggableButtonMorph>>#performAction

Inform the model that this button has been pressed.


<details>
	<summary>See more</summary>
	
	performAction
	"Inform the model that this button has been pressed. "

	actionSelector ifNotNil: [
		model perform: actionSelector ]
</details>

#### PluggableButtonMorph>>#iconName: aSymbol

<details>
	<summary>See more</summary>
	
	iconName: aSymbol
	iconName _ aSymbol
</details>

#### PluggableButtonMorph>>#mouseStillDown

Acting when down (instead of waiting until releasing the button) also means that the button actin is repeated if the button is kept pressed. See #handlesMouseStillDown:


<details>
	<summary>See more</summary>
	
	mouseStillDown
	"Acting when down (instead of waiting until releasing the button)
	also means that the button actin is repeated if the button is kept pressed.
	See #handlesMouseStillDown:"
	self performAction
</details>

#### PluggableButtonMorph>>#updateRightButtonImage

update the receiver's as a downButton. put a new image inside


<details>
	<summary>See more</summary>
	
	updateRightButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #right size: ScrollBar scrollbarThickness.
	self iconName: #drawRightIcon.
	actionSelector _ #scrollDown.
	self
		roundButtonStyle: false;
		redrawNeeded
</details>

#### PluggableButtonMorph>>#actWhen: condition

Accepts symbols: #buttonDown, #buttonStillDown or #buttonUp (default)


<details>
	<summary>See more</summary>
	
	actWhen: condition
	"Accepts symbols:  #buttonDown, #buttonStillDown or #buttonUp (default)"
	actWhen _ condition
</details>

#### PluggableButtonMorph>>#iconName

<details>
	<summary>See more</summary>
	
	iconName
	^iconName
</details>

#### PluggableButtonMorph>>#icon: aForm

<details>
	<summary>See more</summary>
	
	icon: aForm
	icon _ aForm.
	magnifiedIcon _ nil
</details>

#### PluggableButtonMorph>>#handlesMouseStillDown: evt

Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp


<details>
	<summary>See more</summary>
	
	handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	"Acting when down (instead of waiting until releasing the button)
	also means that the button action is repeated if the button is kept pressed"
	^actWhen == #buttonStillDown
</details>

#### PluggableButtonMorph>>#morphContainsPoint: aLocalPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	morphContainsPoint: aLocalPoint

	| iconOrigin |
	(self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin _ extent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]
</details>

#### PluggableButtonMorph>>#action: aSymbol

Set actionSelector to be the action defined by aSymbol.


<details>
	<summary>See more</summary>
	
	action: aSymbol 
	"Set actionSelector to be the action defined by aSymbol."

	actionSelector _ aSymbol.

</details>

#### PluggableButtonMorph>>#magnifiedIcon

<details>
	<summary>See more</summary>
	
	magnifiedIcon
	| factor magnifiedExtent w h |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon _ icon.
		w _ icon width.
		h _ icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * extent x / w min: 1.0 * extent y / h.
			factor = 1.0 ifFalse: [
				magnifiedExtent _ (icon extent * factor) rounded.
				magnifiedIcon _ icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon
</details>

#### PluggableButtonMorph>>#label: aString

Label this button with the given string.


<details>
	<summary>See more</summary>
	
	label: aString
	"Label this button with the given string."

	self label: aString font: nil
</details>

#### PluggableButtonMorph>>#drawEmbossedLabelOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f lineSpacing // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]
</details>

#### PluggableButtonMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ true.
	self redrawNeeded.
	(actWhen == #buttonDown or: [ actWhen == #buttonStillDown ])
		ifTrue: [
			self performAction ]
		ifFalse: [
			"Don't make multi-click slower if we act on button down, just do multiple actions"
			aMouseButtonEvent hand
				waitForClicksOrDragOrSimulatedMouseButton2: self
				event: aMouseButtonEvent
				clkSel: nil
				clkNHalf: nil
				dblClkSel: #doubleClick:localPosition:
				dblClkNHalfSel: nil
				tripleClkSel: nil ]
</details>

#### PluggableButtonMorph>>#update: aSymbol

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aSymbol

	super update: aSymbol.
	aSymbol = getStateSelector ifTrue: [
		self redrawNeeded ]
</details>

#### PluggableButtonMorph>>#getModelState

Answer the result of sending the receiver's model the getStateSelector message.


<details>
	<summary>See more</summary>
	
	getModelState
	"Answer the result of sending the receiver's model the getStateSelector message."

	^ getStateSelector 
		ifNil: [false]
		ifNotNil: [model perform: getStateSelector]
</details>

#### PluggableButtonMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	^ Theme current minimalWindows ifTrue: [0] ifFalse: [1]
</details>

#### PluggableButtonMorph>>#actionSelector

Answer the receiver's actionSelector


<details>
	<summary>See more</summary>
	
	actionSelector
	"Answer the receiver's actionSelector"

	^ actionSelector
</details>

#### PluggableButtonMorph>>#model: anObject

Set my model and make me me a dependent of the given object.


<details>
	<summary>See more</summary>
	
	model: anObject
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeDependent: self].
	getStateSelector ifNotNil: [
		anObject ifNotNil: [anObject addDependent: self]].
	model _ anObject
</details>

#### PluggableButtonMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	isPressed _ false.
	mouseIsOver _ false.
	(actWhen == #buttonUp and: [ self morphContainsPoint: localEventPosition ])
		ifTrue: [ self performAction ].
	self redrawNeeded
</details>

#### PluggableButtonMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle _ nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model _ nil.
	getStateSelector _ nil.
	actionSelector _ nil.
	isPressed _ false.
	mouseIsOver _ false.
	actWhen _ #buttonUp.
	extent _  `20 @ 15`
</details>

#### PluggableButtonMorph>>#mouseIsOver

<details>
	<summary>See more</summary>
	
	mouseIsOver
	^mouseIsOver
</details>

#### PluggableButtonMorph>>#updateUpButtonImage

update the receiver's as a upButton. put a new image inside


<details>
	<summary>See more</summary>
	
	updateUpButtonImage
	"update the receiver's as a upButton. put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #up size: ScrollBar scrollbarThickness.
	self iconName: #drawUpIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded
</details>

#### PluggableButtonMorph>>#drawRegularLabelOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]
</details>

#### PluggableButtonMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #PluggableButtonMorph or: [ super is: aSymbol ]
</details>

#### PluggableButtonMorph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^self isRoundButton not
</details>

#### PluggableButtonMorph>>#model: anObject stateGetter: getStateSel action: actionSel label: aString

<details>
	<summary>See more</summary>
	
	model: anObject stateGetter: getStateSel action: actionSel label: aString

	getStateSelector _ getStateSel.
	actionSelector _ actionSel.
	self model: anObject.
	self label: aString
</details>

#### PluggableButtonMorph>>#mouseEnter: event

The mouse entered the receiver


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	"The mouse entered the receiver"
	mouseIsOver _ true.
	self redrawNeeded.
	^super mouseEnter: event
</details>

#### PluggableButtonMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true
</details>

#### PluggableButtonMorph>>#isPressed

<details>
	<summary>See more</summary>
	
	isPressed
	^isPressed | self getModelState
</details>

#### PluggableButtonMorph>>#adoptWidgetsColor: paneColor

<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	self color: (Theme current buttonColorFrom: paneColor)
</details>

#### PluggableButtonMorph>>#roundButtonStyle: aBooleanOrNil

<details>
	<summary>See more</summary>
	
	roundButtonStyle: aBooleanOrNil
	roundButtonStyle _ aBooleanOrNil
</details>

#### PluggableButtonMorph>>#draw3DLookOn: aCanvas

<details>
	<summary>See more</summary>
	
	draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphLocalBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas
</details>

#### PluggableButtonMorph>>#updateLeftButtonImage

update the receiver's as a downButton. put a new image inside


<details>
	<summary>See more</summary>
	
	updateLeftButtonImage
	"update the receiver's as a downButton.  put a new image inside"

	icon _ BitBltCanvas arrowOfDirection: #left size: ScrollBar scrollbarThickness.
	self iconName: #drawLeftIcon.
	actionSelector _ #scrollUp.
	self
		roundButtonStyle: false;
		redrawNeeded
</details>

#### PluggableButtonMorph>>#drawRoundGradientLookOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ self morphLocalBounds insetBy: `1@3`.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ self morphLocalBounds insetBy: `1@3`.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]
</details>

#### PluggableButtonMorph>>#isRoundButton

<details>
	<summary>See more</summary>
	
	isRoundButton
	^roundButtonStyle ifNil: [ Theme current roundButtons ]
</details>

#### PluggableButtonMorph>>#iconColor

<details>
	<summary>See more</summary>
	
	iconColor

	^ self isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			self mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].
</details>

#### PluggableButtonMorph>>#handlesMouseOver: evt

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?


<details>
	<summary>See more</summary>
	
	handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^true
</details>

#### PluggableButtonMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	self isRoundButton
		ifTrue: [
			aCanvas drawButtonIconFromCurrentMorph ifFalse: [
				self drawRoundGradientLookOn: aCanvas ]]
		ifFalse: [
			self draw3DLookOn: aCanvas.
			aCanvas drawButtonIconFromCurrentMorph ]
</details>

#### PluggableButtonMorph>>#label: aStringOrNil font: aFontOrNil

Label this button with the given string.


<details>
	<summary>See more</summary>
	
	label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label _ aStringOrNil.
	font _ aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			extent := (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ]
</details>

#### PluggableButtonMorph>>#mouseLeave: event

The mouse has left the area of the receiver


<details>
	<summary>See more</summary>
	
	mouseLeave: event
	"The mouse has left the area of the receiver"
	mouseIsOver _ false.
	self redrawNeeded.
	^super mouseLeave: event
</details>

#### PluggableButtonMorph>>#fontToUse

<details>
	<summary>See more</summary>
	
	fontToUse
	| fontToUse |
	fontToUse := font ifNil: [Preferences standardButtonFont].
	"
	Could add emphasis...
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [fontToUse]
		ifFalse: [fontToUse emphasized: emphasis]
	"
	^fontToUse
</details>

## PluggableListMorph

... When a PluggableListMorph is in focus, type in a letter (or several letters quickly) to go to the next item that begins with that letter. Special keys (up, down, home, etc.) are also supported.

### Methods
#### PluggableListMorph>>#acceptDroppingMorph: aMorph event: dropEvent

This message is sent when a morph is dropped onto a morph that has agreed to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: message. This default implementation just adds the given morph to the receiver.


<details>
	<summary>See more</summary>
	
	acceptDroppingMorph: aMorph event: dropEvent

	| localPosition row |
	
	localPosition _ self internalizeFromWorld: dropEvent eventPosition.
	row _ self rowAtLocation: localPosition ifNone: [ ^self acceptDroppingMorph: aMorph outsideListWithEvent: dropEvent ].
	
	self acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent.
	
	
</details>

#### PluggableListMorph>>#makeItemsDraggable

<details>
	<summary>See more</summary>
	
	makeItemsDraggable

	self setProperty: #draggableItems toValue: true
</details>

#### PluggableListMorph>>#processMouseMove: aMouseMoveEvent localPosition: localEventPosition

Reimplemented because we really want #mouseMove when a morph is dragged around


<details>
	<summary>See more</summary>
	
	processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	self listMorph highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: []]).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]
</details>

#### PluggableListMorph>>#acceptDroppingMorph: aMorph outsideListWithEvent: dropEvent

<details>
	<summary>See more</summary>
	
	acceptDroppingMorph: aMorph outsideListWithEvent: dropEvent

	| dropActionSelector dropSelectorArgument |
	
	dropActionSelector _ self valueOfProperty: #dropOutsideListActionSelector.
	dropSelectorArgument _ aMorph
		valueOfProperty: #dropSelectorArgument
		ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
	model perform: dropActionSelector with: dropSelectorArgument.
	
</details>

#### PluggableListMorph>>#selection: item

Public. Call to set selection. Usually, view is updated from model updates. If model updating fails (no model index setter defined) then just update visuals.


<details>
	<summary>See more</summary>
	
	selection: item
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	self selectionIndex: (list indexOf: item)
</details>

#### PluggableListMorph>>#numSelectionsInView

<details>
	<summary>See more</summary>
	
	numSelectionsInView
	^ self viewableHeight // self listItemHeight
</details>

#### PluggableListMorph>>#getMenu

Answer the menu for this view


<details>
	<summary>See more</summary>
	
	getMenu
	"Answer the menu for this view"

	menuGetter ifNil: [^ nil].
	(menuGetter is: #MessageSend) ifTrue: [
		^menuGetter value ].
	menuGetter numArgs = 0 ifTrue: [
		^ mainView perform: menuGetter ].
	^ self error: 'The menuGetter has an unsupported number of arguments'
</details>

#### PluggableListMorph>>#scrollSelectionIntoView

make sure that the current selection is visible


<details>
	<summary>See more</summary>
	
	scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			self flag: #jmvVer2.
			r _ self listMorph drawBoundsForRow: row.
			r _ ((self listMorph externalize: r origin) extent: r extent).
			self scrollToShow: r ]
</details>

#### PluggableListMorph>>#doubleClick: aMouseButtonEvent localPosition: localEventPosition

Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index _ self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ self model perform: doubleClickSelector
</details>

#### PluggableListMorph>>#keyStroke: aKeyboardEvent

Process keys


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent 
	"Process keys"
	| aCharacter |
	(self focusKeyboardFor: aKeyboardEvent) ifTrue: [ ^ self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [ ^ self ].
	
	aKeyboardEvent isEsc ifTrue: [ " escape key" ^ self mouseButton2Activity ].
	aKeyboardEvent isDelete ifTrue: [ "delete key" ^ self deleteAction ].
	aKeyboardEvent isBackspace ifTrue: [ "backspace key" ^ self deleteAction ].
	
	aCharacter _ aKeyboardEvent keyCharacter.
	
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter
</details>

#### PluggableListMorph>>#innerMorphClass

<details>
	<summary>See more</summary>
	
	innerMorphClass
	^InnerListMorph
</details>

#### PluggableListMorph>>#copySelectionToClipboard

Copy my selected item to the clipboard as a string


<details>
	<summary>See more</summary>
	
	copySelectionToClipboard
	"Copy my selected item to the clipboard as a string"

	self visualSelection 
		ifNotNil: [ :sel |
			Clipboard storeObject: sel asString ]
		ifNil: [
			self flash ]
</details>

#### PluggableListMorph>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	self listMorph highlightedRow: nil.
	super mouseButton2Activity
</details>

#### PluggableListMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

The mouse came up within the list; take appropriate action


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].  "work-around the no-mans-land bug"
	"No change if model is locked"
	(autoDeselect and: [ row == self visualSelectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self setSelectionIndex: 0 ]]
		ifFalse: [ self setSelectionIndex: row ]
</details>

#### PluggableListMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	scroller morphWidth: extent x.
</details>

#### PluggableListMorph>>#handlesKeyboard

Return true if the receiver wishes to handle keyboard events


<details>
	<summary>See more</summary>
	
	handlesKeyboard

	^self visible
</details>

#### PluggableListMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #PluggableListMorph or: [ super is: aSymbol ]
</details>

#### PluggableListMorph>>#visualSelection

<details>
	<summary>See more</summary>
	
	visualSelection 
	self visualSelectionIndex = 0 ifTrue: [ ^nil ].
	list ifNotNil: [ ^list at: self visualSelectionIndex ].
	^ self getListItem: self visualSelectionIndex
</details>

#### PluggableListMorph>>#mouseEnter: event

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self ]
</details>

#### PluggableListMorph>>#doubleClickSelector: aSymbol

<details>
	<summary>See more</summary>
	
	doubleClickSelector: aSymbol
	doubleClickSelector _ aSymbol
</details>

#### PluggableListMorph>>#maximumSelection

<details>
	<summary>See more</summary>
	
	maximumSelection
	^ self getListSize
</details>

#### PluggableListMorph>>#allowsMorphDrop

Answer whether we accept dropping morphs. By default answer false.


<details>
	<summary>See more</summary>
	
	allowsMorphDrop

	^self hasProperty: #allowsMorphDrop
</details>

#### PluggableListMorph>>#rowAtLocation: aPoint

Return the row at the given point or 0 if outside


<details>
	<summary>See more</summary>
	
	rowAtLocation: aPoint
	"Return the row at the given point or 0 if outside"

	| m |
	m _ self listMorph.
	^m rowAtLocation: (m internalize: aPoint)
</details>

#### PluggableListMorph>>#addCustomMenuItems:  aMenu hand: aHandMorph

Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo.


<details>
	<summary>See more</summary>
	
	addCustomMenuItems:  aMenu hand: aHandMorph
	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'copy list to clipboard' target: self action: #copyListToClipboard.
	aMenu add: 'copy selection to clipboard' target: self action: #copySelectionToClipboard
</details>

#### PluggableListMorph>>#navigateToBottom

<details>
	<summary>See more</summary>
	
	navigateToBottom
	
	self changeSelectionTo: self maximumSelection
</details>

#### PluggableListMorph>>#scrollDeltaHeight

Return the increment in pixels which this pane should be scrolled.


<details>
	<summary>See more</summary>
	
	scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."
	^ self font lineSpacing
</details>

#### PluggableListMorph>>#itemsAreDraggable

<details>
	<summary>See more</summary>
	
	itemsAreDraggable
	
	^self hasProperty: #draggableItems
</details>

#### PluggableListMorph>>#rejectDrops

<details>
	<summary>See more</summary>
	
	rejectDrops

	self removeProperty: #allowsMorphDrop.
	self removeProperty: #acceptedDragSource.
	self removeProperty: #dropActionSelector.
	self removeProperty: #dropOutsideListActionSelector
</details>

#### PluggableListMorph>>#font: aFontOrNil

<details>
	<summary>See more</summary>
	
	font: aFontOrNil
	self listMorph font: aFontOrNil.

</details>

#### PluggableListMorph>>#arrowKey: aKeyboardEvent

Handle a keyboard navigation event. Answer nil if not handled.


<details>
	<summary>See more</summary>
	
	arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil
</details>

#### PluggableListMorph>>#navigateDown

move down, wrapping to top if needed


<details>
	<summary>See more</summary>
	
	navigateDown
	"move down, wrapping to top if needed"
	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex + 1.
	nextSelection > self maximumSelection ifTrue: [ nextSelection _ self minimumSelection ].
	self changeSelectionTo: nextSelection
</details>

#### PluggableListMorph>>#getListSize

return the current number of items in the displayed list


<details>
	<summary>See more</summary>
	
	getListSize
	"return the current number of items in the displayed list"
	^list ifNotNil: [ list size ] ifNil: [ 0 ]
</details>

#### PluggableListMorph>>#keystrokeAction: aChar

<details>
	<summary>See more</summary>
	
	keystrokeAction: aChar 
	| args |
	keystrokeActionSelector ifNil: [^false].
	args _ keystrokeActionSelector numArgs.
	args = 1 ifTrue: [
		mainView perform: keystrokeActionSelector with: aChar.
		^true ].
	args = 2 
		ifTrue: [
			mainView 
				perform: keystrokeActionSelector
				with: aChar
				with: self.
			^true ].
	^self error: 'keystrokeActionSelector must be a 1- or 2-keyword symbol'
</details>

#### PluggableListMorph>>#navigateOnePageUp

<details>
	<summary>See more</summary>
	
	navigateOnePageUp

	self changeSelectionTo: (self minimumSelection max: self getCurrentSelectionIndex - self numSelectionsInView)
</details>

#### PluggableListMorph>>#copyListToClipboard

Copy my items to the clipboard as a multi-line string


<details>
	<summary>See more</summary>
	
	copyListToClipboard
	"Copy my items to the clipboard as a multi-line string"

	| stream |
	stream _ WriteStream on: (String new: list size * 40).
	list
		do: [:ea | stream nextPutAll: ea asString]
		separatedBy: [ stream newLine ].
	Clipboard storeObject: stream contents
</details>

#### PluggableListMorph>>#privateVisualSelectionIndex: index

Called internally to select the index-th item. Does not update model


<details>
	<summary>See more</summary>
	
	privateVisualSelectionIndex: index
	"Called internally to select the index-th item.
	Does not update model"
	| row |
	row _ index ifNil: [ 0 ].
	row _ row min: self getListSize.  "make sure we don't select past the end"
	self listMorph selectedRow: row.
	self scrollSelectionIntoView
</details>

#### PluggableListMorph>>#keyboardSearch: aChar

<details>
	<summary>See more</summary>
	
	keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		(w okToChangeDueTo: self) ifFalse: [^ self]].
	nextSelection _ list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self setSelectionIndex: nextSelection
</details>

#### PluggableListMorph>>#flashRow: aRow

<details>
	<summary>See more</summary>
	
	flashRow: aRow

	^self listMorph flashRow: aRow.
</details>

#### PluggableListMorph>>#acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent

<details>
	<summary>See more</summary>
	
	acceptDroppingMorph: aMorph atRow: row withEvent: dropEvent

	| args dropActionSelector |

	self flashRow: row.

	dropActionSelector _ self valueOfProperty: #dropActionSelector.
	args _ dropActionSelector numArgs.
	args = 1 ifTrue: [ ^model perform: dropActionSelector with: row].
	args = 2 ifTrue: [ | dropSelectorArgument |
		dropSelectorArgument _ aMorph
			valueOfProperty: #dropSelectorArgument
			ifAbsent: [self error: 'aMorph is missing dropSelectorArgument property'].
		^model perform: dropActionSelector with: row with: dropSelectorArgument ].
	
	self error: 'dropActionSelector must be a 1- or 2-keyword symbol'
	
	
</details>

#### PluggableListMorph>>#navigateOnePageDown

<details>
	<summary>See more</summary>
	
	navigateOnePageDown

	self changeSelectionTo: (self getCurrentSelectionIndex + self numSelectionsInView min: self maximumSelection)
</details>

#### PluggableListMorph>>#autoDeselect: trueOrFalse

Enable/disable autoDeselect (see class comment)


<details>
	<summary>See more</summary>
	
	autoDeselect: trueOrFalse
	"Enable/disable autoDeselect (see class comment)"
	autoDeselect _ trueOrFalse.
</details>

#### PluggableListMorph>>#getCurrentSelectionIndex

Answer the index of the current selection.


<details>
	<summary>See more</summary>
	
	getCurrentSelectionIndex
	"Answer the index of the current selection."

	getIndexSelector ifNil: [^0].
	^model perform: getIndexSelector
</details>

#### PluggableListMorph>>#navigateUp

move up, wrapping to bottom if needed


<details>
	<summary>See more</summary>
	
	navigateUp
	"move up, wrapping to bottom if needed"
	| nextSelection |
	nextSelection _ self getCurrentSelectionIndex - 1.
	nextSelection < self minimumSelection ifTrue: [ nextSelection _ self maximumSelection ].
	self changeSelectionTo: nextSelection
</details>

#### PluggableListMorph>>#getListItem: index

get the index-th item in the displayed list


<details>
	<summary>See more</summary>
	
	getListItem: index
	"get the index-th item in the displayed list"
	^list at: index
</details>

#### PluggableListMorph>>#minimumSelection

<details>
	<summary>See more</summary>
	
	minimumSelection
	^ 1
</details>

#### PluggableListMorph>>#leftSibling: aListMorphToTheLeft rightSibling: aListMorphToTheRight

<details>
	<summary>See more</summary>
	
	leftSibling: aListMorphToTheLeft rightSibling: aListMorphToTheRight

	self leftSibling: aListMorphToTheLeft.
	self rightSibling: aListMorphToTheRight.
</details>

#### PluggableListMorph>>#getList

Answer the list to be displayed. Caches the returned list in the 'list' ivar


<details>
	<summary>See more</summary>
	
	getList
	"Answer the list to be displayed.  Caches the returned list in the 'list' ivar"
	getListSelector
		ifNil: [^ #()].
	list _ model perform: getListSelector.
	list
		ifNil: [^ #()].
	list _ list collect: [ :item | item displayStringOrText ].
	^ list
</details>

#### PluggableListMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self 
		rowAtLocation: localEventPosition
		ifNone:  [^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view.
	Model update will be done on mouse button up, so this feedback will be visible before that."
	self listMorph highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: (self itemsAreDraggable ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])
</details>

#### PluggableListMorph>>#update: aSymbol

Refer to the comment in View|update:.


<details>
	<summary>See more</summary>
	
	update: aSymbol 
	"Refer to the comment in View|update:."

	super update: aSymbol.
	aSymbol == getListSelector ifTrue: [
		self updateList.
		^ self].
	aSymbol == getIndexSelector ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex ]
</details>

#### PluggableListMorph>>#initForKeystrokes

<details>
	<summary>See more</summary>
	
	initForKeystrokes
	lastKeystrokeTime _ 0.
	lastKeystrokes _ ''
</details>

#### PluggableListMorph>>#gainFocusFrom: aHand

<details>
	<summary>See more</summary>
	
	gainFocusFrom: aHand

	aHand newKeyboardFocus: self.
	self getCurrentSelectionIndex = 0 ifTrue: [ self selectionIndex: 1 ].
</details>

#### PluggableListMorph>>#keyboardFocusChange: aBoolean

The message is sent to a morph when its keyboard focus changes. The given argument indicates that the receiver is gaining (versus losing) the keyboard focus. In this case, all we need to do is to redraw focus feedback


<details>
	<summary>See more</summary>
	
	keyboardFocusChange: aBoolean
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw focus feedback"

	self redrawNeeded
</details>

#### PluggableListMorph>>#navigateToTop

<details>
	<summary>See more</summary>
	
	navigateToTop
	
	self changeSelectionTo: self minimumSelection
</details>

#### PluggableListMorph>>#rightSibling: aListMorphToTheRight

<details>
	<summary>See more</summary>
	
	rightSibling: aListMorphToTheRight

	rightSibling _ aListMorphToTheRight
</details>

#### PluggableListMorph>>#itemSelectedAmongMultiple: index

return whether the index-th row is selected. Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany


<details>
	<summary>See more</summary>
	
	itemSelectedAmongMultiple: index
	"return whether the index-th row is selected.  Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany"
	^false
</details>

#### PluggableListMorph>>#navigateRight

<details>
	<summary>See more</summary>
	
	navigateRight

	rightSibling ifNotNil: [ rightSibling gainFocusFrom: self activeHand ]
</details>

#### PluggableListMorph>>#privateVisualSelection: item

Called internally to set a new selection. Does not update model


<details>
	<summary>See more</summary>
	
	privateVisualSelection: item
	"Called internally to set a new selection.
	Does not update model"

	self privateVisualSelectionIndex: (list indexOf: item)
</details>

#### PluggableListMorph>>#makeItemsUndraggable

<details>
	<summary>See more</summary>
	
	makeItemsUndraggable

	self removeProperty: #draggableItems
</details>

#### PluggableListMorph>>#leftSibling: aListMorphToTheLeft

<details>
	<summary>See more</summary>
	
	leftSibling: aListMorphToTheLeft

	leftSibling _ aListMorphToTheLeft
</details>

#### PluggableListMorph>>#updateList

the list has changed -- update from the model


<details>
	<summary>See more</summary>
	
	updateList
	| index |
	"the list has changed -- update from the model"
	self getList.
	self listMorph listChanged.
	self setScrollDeltas.
	index _ self getCurrentSelectionIndex.
	self privateVisualSelectionIndex: index
</details>

#### PluggableListMorph>>#setSelectionIndex: anInteger

Change the model's selected item index to be anInteger.


<details>
	<summary>See more</summary>
	
	setSelectionIndex: anInteger
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil: [
		model perform: setIndexSelector with: anInteger.
		self update: getIndexSelector.
		^ true ].
	^ false
</details>

#### PluggableListMorph>>#scrollDeltaWidth

A guess -- assume that the width of a char is approx 1/2 the height of the font


<details>
	<summary>See more</summary>
	
	scrollDeltaWidth
"A guess -- assume that the width of a char is approx 1/2 the height of the font"
	^ self scrollDeltaHeight // 2


</details>

#### PluggableListMorph>>#dragEvent: aMouseEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem |
	self listMorph highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.

	dragged _ DraggingGuideMorph new.
	dragged addMorph: (StringMorph contents: listItem).
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.
</details>

#### PluggableListMorph>>#font

<details>
	<summary>See more</summary>
	
	font

	^ self listMorph font

</details>

#### PluggableListMorph>>#wantsDroppedMorph: aMorph event: evt

Return true if the receiver wishes to accept the given morph, which is being dropped by a hand in response to the given event. Note that for a successful drop operation both parties need to agree. The symmetric check is done automatically via aMorph wantsToBeDroppedInto: self.


<details>
	<summary>See more</summary>
	
	wantsDroppedMorph: aMorph event: evt

	^(aMorph is: #DraggingGuideMorph)
		and: [ (aMorph valueOfProperty: #dragSource) = (self valueOfProperty: #acceptedDragSource) ]
</details>

#### PluggableListMorph>>#listItemHeight

This should be cleaned up. The list should get spaced by this parameter.


<details>
	<summary>See more</summary>
	
	listItemHeight
	"This should be cleaned up.  The list should get spaced by this parameter."
	^ 12
</details>

#### PluggableListMorph>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged

	super fontPreferenceChanged.
	self font: Preferences standardListFont.
</details>

#### PluggableListMorph>>#model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

<details>
	<summary>See more</summary>
	
	model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	self model: anObject.
	getListSelector _ getListSel.
	getIndexSelector _ getSelectionSel.
	setIndexSelector _ setSelectionSel.
	mainView _ aMainView.
	menuGetter _ getMenuSel.
	keystrokeActionSelector _ keyActionSel.
	autoDeselect _ true.
	self
		updateList;
		initForKeystrokes

</details>

#### PluggableListMorph>>#visualSelectionIndex

return the index we have currently selected, or 0 if none


<details>
	<summary>See more</summary>
	
	visualSelectionIndex
	"return the index we have currently selected, or 0 if none"
	^self listMorph selectedRow ifNil: [ 0 ]
</details>

#### PluggableListMorph>>#acceptDropsFrom: aMorph performing: aDropActionSelector whenOutsideList: aDropOutsideListActionSelector

<details>
	<summary>See more</summary>
	
	acceptDropsFrom: aMorph performing: aDropActionSelector whenOutsideList: aDropOutsideListActionSelector 

	(aDropActionSelector numArgs between: 1 and: 2) ifFalse: [ self error: 'dropActionSelector must be a 1- or 2-keyword symbol' ].
	aDropOutsideListActionSelector numArgs = 1 ifFalse: [ self error: 'dropOutsideListActionSelector must be a 1-keyword symbol' ].

	self setProperty: #allowsMorphDrop toValue: true.
	self setProperty: #acceptedDragSource toValue: aMorph.
	self setProperty: #dropActionSelector toValue: aDropActionSelector.
	self setProperty: #dropOutsideListActionSelector toValue: aDropOutsideListActionSelector 
</details>

#### PluggableListMorph>>#changeSelectionTo: nextSelection

<details>
	<summary>See more</summary>
	
	changeSelectionTo: nextSelection

	nextSelection = self getCurrentSelectionIndex ifFalse: [
		| window |
		window _ self owningWindow.
		(window isNil or: [ window okToChangeDueTo: self ]) ifTrue: [
			"No change if model is locked"
			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.
			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		]
	]
</details>

#### PluggableListMorph>>#deleteAction

Handles deleting action, which happens when the user presses backspace or delete key within me


<details>
	<summary>See more</summary>
	
	deleteAction
	"Handles deleting action, which happens when the user presses backspace or delete key within me"
	| deleteActionBlock |
	deleteActionBlock _ self valueOfProperty: #deleteAction  ifAbsent: [ nil ].
	deleteActionBlock isNil
		ifTrue: [ self flash ]
		ifFalse: [ deleteActionBlock value ].
	^self
</details>

#### PluggableListMorph>>#rowAtLocation: aPoint ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	rowAtLocation: aPoint ifNone: aNoneBlock 
	
	| listMorph |
	
	listMorph _ self listMorph.
	^listMorph rowAtLocation: (listMorph internalize: aPoint) ifNone: aNoneBlock
</details>

#### PluggableListMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	super drawOn: aCanvas.

	(drawKeyboardFocusIndicator and: [ self hasKeyboardFocus ]) ifTrue: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: Preferences focusIndicatorWidth
			color: Theme current focusIndicator ].
</details>

#### PluggableListMorph>>#verifyContents

Verify the contents of the receiver, reconstituting if necessary. Called whenever window is reactivated, to react to possible structural changes.


<details>
	<summary>See more</summary>
	
	verifyContents
	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes."

	| newList oldList |
	oldList _ list ifNil: [ #() ].
	newList _ self getList.
	oldList = newList ifFalse: [
		self updateList ]
</details>

#### PluggableListMorph>>#listMorph

<details>
	<summary>See more</summary>
	
	listMorph
self flag: #jmvVer.
"Podemos reemplazar los senders locales por accesos directos (el doble encapsulamiento es tonto) una vez que quede definido el shape de la clase!"
	^scroller
</details>

#### PluggableListMorph>>#mouseLeave: event

Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseLeave: event
	super mouseLeave: event.
	self listMorph highlightedRow: nil
</details>

#### PluggableListMorph>>#navigateLeft

<details>
	<summary>See more</summary>
	
	navigateLeft

	leftSibling ifNotNil: [ leftSibling gainFocusFrom: self activeHand ]
</details>

#### PluggableListMorph>>#selectionIndex: anInteger

Public. Call to set selection. Usually, view is updated from model updates. If model updating fails (no model index setter defined) then just update visuals.


<details>
	<summary>See more</summary>
	
	selectionIndex: anInteger
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]
</details>

## PluggableListMorphByItem

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PluggableListMorphByItem>>#setSelectionIndex: anInteger

Change the model's selected item to be the one at the given index.


<details>
	<summary>See more</summary>
	
	setSelectionIndex: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item _ itemList at: anInteger ifAbsent: [ nil ].
		model perform: setIndexSelector with: item.
		self update: getIndexSelector.
		^ true ].
	^false
	
</details>

#### PluggableListMorphByItem>>#getCurrentSelectionIndex

Answer the index of the current selection.


<details>
	<summary>See more</summary>
	
	getCurrentSelectionIndex
	"Answer the index of the current selection."
	| item |
	getIndexSelector ifNil: [^ 0].
	item _ model perform: getIndexSelector.
	^ list findFirst: [ :x | x = item]

</details>

#### PluggableListMorphByItem>>#getList

cache the raw items in itemList


<details>
	<summary>See more</summary>
	
	getList
	"cache the raw items in itemList"
	itemList := getListSelector ifNil: [ #() ] ifNotNil: [ model perform: getListSelector ].
	^super getList
</details>

## PluggableListMorphOfMany

A variant of its superclass that allows multiple items to be selected simultaneously. There is still a distinguished element which is selected, but each other element in the list may be flagged on or off.

### Methods
#### PluggableListMorphOfMany>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

The mouse came up within the list; take appropriate action


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	dragOnOrOff _ nil.  "So improperly started drags will have not effect"
	dragStartRow _ nil
</details>

#### PluggableListMorphOfMany>>#itemSelectedAmongMultiple: index

return whether the index-th row is selected. Always false in PluggableListMorph, but sometimes true in PluggableListMorphOfMany


<details>
	<summary>See more</summary>
	
	itemSelectedAmongMultiple: index
	^self listSelectionAt: index
</details>

#### PluggableListMorphOfMany>>#listSelectionAt: index put: value

<details>
	<summary>See more</summary>
	
	listSelectionAt: index put: value
	setSelectionListSelector ifNil:[^false].
	^model perform: setSelectionListSelector with: index with: value
</details>

#### PluggableListMorphOfMany>>#model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

setup a whole load of pluggability options


<details>
	<summary>See more</summary>
	
	model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel
	"setup a whole load of pluggability options"
	getSelectionListSelector _ getListSel.
	setSelectionListSelector _ setListSel.
	self 
		model: anObject
		listGetter: listSel
		indexGetter: getSelectionSel
		indexSetter: setSelectionSel
		mainView: aMainView
		menuGetter: getMenuSel
		keystrokeAction: keyActionSel
</details>

#### PluggableListMorphOfMany>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

The mouse has moved, as characterized by the event provided. Adjust the scrollbar, and alter the selection as appropriate


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar scrollValue > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: `0@0`) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar scrollValue < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self setSelectionIndex: row ]
		ifFalse: [ self setSelectionIndex: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]
</details>

#### PluggableListMorphOfMany>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	self listMorph highlightedRow: row.

	w _ self owningWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

		"Different from PluggableListMorph. There, we highlight on mouse down, and act on mouse up.
		Here, we act on mouse down, because we support dragging of selection, so mouse up will
		only happen after user is finished dragging. In order to get the highlight visible for the user,
		update the model on next world cycle."
		UISupervisor whenUIinSafeState: [
			"Set or clear new primary selection (listIndex)"
			dragOnOrOff
				ifTrue: [self setSelectionIndex: row]
				ifFalse: [self setSelectionIndex: 0].
			"Need to restore the old one, due to how model works, and set new one."
			oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
			self listSelectionAt: row put: dragOnOrOff ].
	].
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil
</details>

#### PluggableListMorphOfMany>>#update: aSymbol

Refer to the comment in View|update:.


<details>
	<summary>See more</summary>
	
	update: aSymbol 
	super update: aSymbol.
	aSymbol == #allSelections ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		self redrawNeeded]
</details>

#### PluggableListMorphOfMany>>#listSelectionAt: index

<details>
	<summary>See more</summary>
	
	listSelectionAt: index
	getSelectionListSelector ifNil:[^false].
	^model perform: getSelectionListSelector with: index
</details>

## PluggableMorph

PluggableMorph are used to represent structures with state and behavior as well as graphical structure. A PluggableMorph is usually the root of a morphic tree depicting its appearance. The tree is constructed concretely by adding its consituent morphs to a world. When a part is named in a world, it is given a new slot in the model. When a part is sensitized, it is named, and a set of mouse-driven methods is also generated in the model. These may be edited to induce particular behavior. When a variable is added through the morphic world, it is given a slot in the model, along with a set of access methods. In addition for public variables (and this is the default for now), methods are generated and called in any outer model in which this model gets embedded, thus propagating variable changes outward.

### Methods
#### PluggableMorph>>#releaseCachedState

Release cached state of the receiver


<details>
	<summary>See more</summary>
	
	releaseCachedState
	"Release cached state of the receiver"

	(model ~~ self and: [model respondsTo: #releaseCachedState]) ifTrue:
		[model releaseCachedState].
	super releaseCachedState
</details>

#### PluggableMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ `200@100`
</details>

#### PluggableMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color lightGray`
</details>

#### PluggableMorph>>#model

<details>
	<summary>See more</summary>
	
	model 
	^ model
</details>

#### PluggableMorph>>#balloonText

Answer balloon help text or nil, if no help is available. NB: subclasses may override such that they programatically construct the text, for economy's sake, such as model phrases in a Viewer


<details>
	<summary>See more</summary>
	
	balloonText
	"Answer balloon help text or nil, if no help is available.
	NB: subclasses may override such that they programatically
	construct the text, for economy's sake, such as model phrases in
	a Viewer"

	| balloonText |
	balloonText _ super balloonText.
	balloonText isSymbol ifTrue: [ ^model perform: balloonText ].
	^ balloonText
</details>

#### PluggableMorph>>#model: anObject

Set my model and make me me a dependent of the given object.


<details>
	<summary>See more</summary>
	
	model: anObject
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeActionsWithReceiver: self].
	anObject ifNotNil: [anObject addDependent: self].
	model _ anObject
</details>

#### PluggableMorph>>#hasModel

<details>
	<summary>See more</summary>
	
	hasModel
	^true
</details>

#### PluggableMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 0
</details>

## PluggableScrollPane

The scroller (a transform) of a scrollPane is driven by the scrollBar. The scroll values vary from 0.0 to 1.0.

### Methods
#### PluggableScrollPane>>#viewableArea

<details>
	<summary>See more</summary>
	
	viewableArea
	^ self viewableAreaTopLeft corner: self viewableAreaRight @ self viewableAreaBottom
</details>

#### PluggableScrollPane>>#privateExtent: aPoint

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			"Now reset widget sizes"
			scroller adjustExtent.
			self updateScrollBarsBounds.
			self setScrollDeltas.
			self scrollSelectionIntoView ]; yourself
</details>

#### PluggableScrollPane>>#mightNeedHorizontalScrollBar

If not sure, answer true. Only answer false when the horizontal scrollbar will not be needed, regardless of contents, for example, when showing wrapped text. This method is called for deciding if a vertical scroll bar is needed. Therefore if the need of an horizontal scrollbar might depend on the vertical scrollbar being there (and taking space) or not, just answer true.


<details>
	<summary>See more</summary>
	
	mightNeedHorizontalScrollBar
	"If not sure, answer true. Only answer false when the horizontal scrollbar will not be needed, regardless of contents, for example, when showing wrapped text.
	This method is called for deciding if a vertical scroll bar is needed. Therefore if the need of an horizontal scrollbar might depend on the vertical scrollbar being there (and taking space) or not, just answer true."
	^true
</details>

#### PluggableScrollPane>>#scrollToShow: aRectangle

scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space. This means that 0@0 is scrolling all the way top and all the way left


<details>
	<summary>See more</summary>
	
	scrollToShow: aRectangle
	"scroll to include as much of aRectangle as possible, where aRectangle is in the scroller's local space.
	This means that 0@0 is scrolling all the way top and all the way left"
	| deltaY |
	deltaY _ (aRectangle amountToTranslateWithin: (`0@0` extent: self viewableExtent)) y.
	deltaY ~= 0 ifTrue: [
		self scrollBy: 0@deltaY ]
</details>

#### PluggableScrollPane>>#hIsScrollbarShowing

Return true if a horizontal scroll bar is currently showing


<details>
	<summary>See more</summary>
	
	hIsScrollbarShowing
	"Return true if a horizontal scroll bar is currently showing"

	^hScrollBar visible
</details>

#### PluggableScrollPane>>#scrollSelectionIntoView

Scroll my text into view if necessary and return true, else return false


<details>
	<summary>See more</summary>
	
	scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

</details>

#### PluggableScrollPane>>#scrollBy: delta

Move the contents in the direction delta.


<details>
	<summary>See more</summary>
	
	scrollBy: delta
	"Move the contents in the direction delta."

	| newYoffset yRange xRange newXoffset |
	
	"Set the offset on the scroller"
	yRange _ self vLeftoverScrollRange.
	xRange _ self hLeftoverScrollRange.
	newYoffset _ self scrollerOffset y - delta y min: yRange max: 0.
	newXoffset _ self scrollerOffset x - delta x min: xRange max: 0.
	
	self scrollerOffset: newXoffset@newYoffset.

	"Update the scrollBars"
	scrollBar scrollValue: (yRange ifNotZero: [newYoffset asFloat / yRange]).
	hScrollBar scrollValue: (xRange ifNotZero: [newXoffset asFloat / xRange])
</details>

#### PluggableScrollPane>>#someSubmorphPositionOrExtentChanged

Our extent, or some submorph changed. Must layout submorphs again.


<details>
	<summary>See more</summary>
	
	someSubmorphPositionOrExtentChanged
	"Our extent, or some submorph changed. Must layout submorphs again."

	super someSubmorphPositionOrExtentChanged.
	self updateScrollBarsBounds
</details>

#### PluggableScrollPane>>#doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

Some subclasses might do something


<details>
	<summary>See more</summary>
	
	doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition
	"Some subclasses might do something"
</details>

#### PluggableScrollPane>>#hShowScrollBar

<details>
	<summary>See more</summary>
	
	hShowScrollBar

	hScrollBar show.
	scroller adjustExtent
</details>

#### PluggableScrollPane>>#vHideScrollBar

<details>
	<summary>See more</summary>
	
	vHideScrollBar
	scrollBar hide.
	scroller adjustExtent
</details>

#### PluggableScrollPane>>#keyStroke: aKeyboardEvent

Handle a keystroke event.


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent

	( self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	super keyStroke: aKeyboardEvent.
	scroller keyStroke: aKeyboardEvent
</details>

#### PluggableScrollPane>>#innerMorphClass

<details>
	<summary>See more</summary>
	
	innerMorphClass
	^InnerPluggableMorph
</details>

#### PluggableScrollPane>>#scrollBarClass

<details>
	<summary>See more</summary>
	
	scrollBarClass
	^ScrollBar
</details>

#### PluggableScrollPane>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	"Invoke the menu"
	self getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.
		"menu invokeModal" ]
</details>

#### PluggableScrollPane>>#hScrollBarValue: scrollValue

<details>
	<summary>See more</summary>
	
	hScrollBarValue: scrollValue

	| x |
	self hIsScrollbarShowing ifFalse: [
		^self scrollerOffset: 0@self scrollerOffset y ].
	(x _ self hLeftoverScrollRange * scrollValue) <= 0
		ifTrue: [ x _ 0 ].
	self scrollerOffset: x@self scrollerOffset y
</details>

#### PluggableScrollPane>>#hSetScrollDelta

Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range.


<details>
	<summary>See more</summary>
	
	hSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta w |
	
	delta _ self scrollDeltaWidth * 1.0.		"avoid Fraction arithmetic"
	range _ self hLeftoverScrollRange.
	range = 0 ifTrue: [
		^hScrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	w _ self viewableWidth * 1.0.		"avoid Fraction arithmetic"
	hScrollBar scrollDelta: delta / range pageDelta: w - delta / range.
	hScrollBar interval: w / self hTotalScrollRange.
	hScrollBar internalScrollValue: hScrollBar scrollValue
</details>

#### PluggableScrollPane>>#viewableAreaBottom

<details>
	<summary>See more</summary>
	
	viewableAreaBottom
	^ self focusIndicatorBottom - self xtraBorder
</details>

#### PluggableScrollPane>>#viewableAreaLeft

<details>
	<summary>See more</summary>
	
	viewableAreaLeft
	^ self focusIndicatorLeft + self xtraBorder
</details>

#### PluggableScrollPane>>#viewableAreaRight

<details>
	<summary>See more</summary>
	
	viewableAreaRight
	^ self focusIndicatorRight - self xtraBorder
</details>

#### PluggableScrollPane>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	super mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	scroller mouseButton1Up: aMouseButtonEvent localPosition: eventPositionLocalToScroller
</details>

#### PluggableScrollPane>>#vIsScrollbarShowing

Return true if a vertical scroll bar is currently showing


<details>
	<summary>See more</summary>
	
	vIsScrollbarShowing
	"Return true if a vertical scroll bar is currently showing"

	^scrollBar visible
</details>

#### PluggableScrollPane>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	
	"initialize the state of the receiver"
	super initialize.
	hideScrollBars _ false.

	"initialize the receiver's scrollBars"
	scrollBar _ self scrollBarClass new model: self setValueSelector: #vScrollBarValue:.
	hScrollBar _ self scrollBarClass new model: self setValueSelector: #hScrollBarValue:.
	drawKeyboardFocusIndicator _ true.

	scroller _ self innerMorphClass new.
	self addMorph: scroller.
	self scrollerOffset: `0@ 0`.
	self addMorph: scrollBar.
	self addMorph: hScrollBar
</details>

#### PluggableScrollPane>>#minimumExtent

Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit


<details>
	<summary>See more</summary>
	
	minimumExtent
	| minW minH |
	"Figure out the minimum extent for this pane so that either content, or at least required scrollbars, will fit"
	minW _ self xtraBorder * 2 + scroller morphWidth min: self scrollBarClass scrollbarThickness * 2.
	self vIsScrollbarShowing
		ifTrue: [
			minW _ minW + self scrollBarClass scrollbarThickness].
	minH _ self xtraBorder * 2 + scroller morphHeight.
	self hIsScrollbarShowing
		ifTrue: [
			minH _ minH + self scrollBarClass scrollbarThickness].
	minH _ minH min: self scrollBarClass scrollbarThickness * 2.
	^ (minW + (borderWidth * 2)) @ (minH + (borderWidth * 2))
</details>

#### PluggableScrollPane>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #ScrollPane or: [ super is: aSymbol ]
</details>

#### PluggableScrollPane>>#hideScrollBarsIndefinitely

<details>
	<summary>See more</summary>
	
	hideScrollBarsIndefinitely

	hideScrollBars _ true.
	self vHideScrollBar.
	self hHideScrollBar.
</details>

#### PluggableScrollPane>>#vShowScrollBar

<details>
	<summary>See more</summary>
	
	vShowScrollBar

	scrollBar show.
	scroller adjustExtent
</details>

#### PluggableScrollPane>>#addToScroller: aMorph

<details>
	<summary>See more</summary>
	
	addToScroller: aMorph

	scroller
		addMorph: aMorph position: `0@0`;
		morphExtent: aMorph morphExtent
</details>

#### PluggableScrollPane>>#hideOrShowScrollBars

Assume for a moment we don't need an horizontal scrollbar


<details>
	<summary>See more</summary>
	
	hideOrShowScrollBars

	"Assume for a moment we don't need an horizontal scrollbar"
	self hHideScrollBar.

	"Add or remove vertical scrollbar, asuming for a monent there's no horizontal scrollbar,
	to determine need of horizontal scrollbar..."
	self vIsScrollbarNeeded
		ifTrue: [ self vShowScrollBar ]
		ifFalse: [ self vHideScrollBar ].

	"If we need an horizontal scrollbar, add it."
	self hIsScrollbarNeeded ifTrue: [
		self hShowScrollBar.

		"If horizontal scrollbar is needed, maybe vertical scrollbar will be needed too (even if we previously thoutht it wouldn't be needed)."	
		"Note that there is no chance of modifying the need of horizontal scrollbar: it was already needed. Therefore, there is no circularity here."
		self vIsScrollbarNeeded  ifTrue: [
			self vShowScrollBar ]].

	"Ensure that if no scrollbars are needed, whole contents are visible"
	self vIsScrollbarShowing ifFalse: [
		scrollBar internalScrollValue: 0 ].
	self hIsScrollbarShowing ifFalse: [
		hScrollBar internalScrollValue: 0 ].

	self updateScrollBarsBounds
</details>

#### PluggableScrollPane>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^ true
</details>

#### PluggableScrollPane>>#focusIndicatorTop

<details>
	<summary>See more</summary>
	
	focusIndicatorTop
	^ borderWidth
</details>

#### PluggableScrollPane>>#adoptWidgetsColor: paneColor

<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: paneColor
	color = self defaultColor ifTrue: [
		color _ Theme current paneBackgroundFrom: paneColor.
	].
	super adoptWidgetsColor: paneColor.
	scrollBar adoptWidgetsColor: paneColor.
	hScrollBar adoptWidgetsColor: paneColor
</details>

#### PluggableScrollPane>>#scrollDeltaHeight

Return the increment in pixels which this pane should be scrolled (normally a subclass responsibility).


<details>
	<summary>See more</summary>
	
	scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled (normally a subclass responsibility)."
	^ 10

</details>

#### PluggableScrollPane>>#xtraBorder

Answer the width of an extra white border to look nicer


<details>
	<summary>See more</summary>
	
	xtraBorder
	"Answer the width of an extra white border to look nicer"
	^3
</details>

#### PluggableScrollPane>>#viewableAreaTop

<details>
	<summary>See more</summary>
	
	viewableAreaTop
	^ self focusIndicatorTop + self xtraBorder
</details>

#### PluggableScrollPane>>#focusIndicatorExtent

<details>
	<summary>See more</summary>
	
	focusIndicatorExtent
	^ self focusIndicatorRectangle extent
</details>

#### PluggableScrollPane>>#mouseScroll: aMouseEvent localPosition: localEventPosition

Handle a mouse scroll event. This message will only be sent to Morphs that answer true to #handlesMouseScroll: We can query aMouseScrollEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 		[  scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 	-> 		[  hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[  hScrollBar scrollDown: 1 ] }
</details>

#### PluggableScrollPane>>#focusIndicatorBottom

<details>
	<summary>See more</summary>
	
	focusIndicatorBottom
	^ self hIsScrollbarShowing
		ifTrue: [ extent y - borderWidth - self scrollBarClass scrollbarThickness ]
		ifFalse: [ extent y - borderWidth ]
</details>

#### PluggableScrollPane>>#focusIndicatorRectangle

<details>
	<summary>See more</summary>
	
	focusIndicatorRectangle

	^ self focusIndicatorLeft @ self focusIndicatorTop corner: self focusIndicatorRight @ self focusIndicatorBottom
</details>

#### PluggableScrollPane>>#hTotalScrollRange

Return the width extent of the receiver's scrollable area


<details>
	<summary>See more</summary>
	
	hTotalScrollRange
	"Return the width extent of the receiver's scrollable area"
	^scroller morphWidth
</details>

#### PluggableScrollPane>>#viewableWidth

Viewable width. Leave room for vertical scrollbar if present


<details>
	<summary>See more</summary>
	
	viewableWidth
	"Viewable width.
	Leave room for vertical scrollbar if present"

	^ self viewableAreaRight - self viewableAreaLeft
</details>

#### PluggableScrollPane>>#hHideScrollBar

<details>
	<summary>See more</summary>
	
	hHideScrollBar
	hScrollBar hide.
	scroller adjustExtent
</details>

#### PluggableScrollPane>>#vLeftoverScrollRange

Return the entire scrolling range minus the currently viewed area.


<details>
	<summary>See more</summary>
	
	vLeftoverScrollRange
	"Return the entire scrolling range minus the currently viewed area."

	^ (self vTotalScrollRange - self viewableHeight) max: 0
</details>

#### PluggableScrollPane>>#hScrollBarWidth

Return the width of the horizontal scrollbar


<details>
	<summary>See more</summary>
	
	hScrollBarWidth
	"Return the width of the horizontal scrollbar"

	^ self focusIndicatorRight - self focusIndicatorLeft
</details>

#### PluggableScrollPane>>#viewableAreaTopLeft

<details>
	<summary>See more</summary>
	
	viewableAreaTopLeft
	^ self viewableAreaLeft @ self viewableAreaTop
</details>

#### PluggableScrollPane>>#hLeftoverScrollRange

Return the entire scrolling range minus the currently viewed area.


<details>
	<summary>See more</summary>
	
	hLeftoverScrollRange
	"Return the entire scrolling range minus the currently viewed area."

	^ (self hTotalScrollRange - self viewableWidth) max: 0
</details>

#### PluggableScrollPane>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseDown: We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller
</details>

#### PluggableScrollPane>>#hIsScrollbarNeeded

Return whether the horz scrollbar is needed


<details>
	<summary>See more</summary>
	
	hIsScrollbarNeeded
	"Return whether the horz scrollbar is needed"

	self mightNeedHorizontalScrollBar ifFalse: [ ^false ].
	
	"Don't show it if we were told not to."
	hideScrollBars ifTrue: [ ^false ].

	^self hLeftoverScrollRange > 0
</details>

#### PluggableScrollPane>>#focusIndicatorLeft

<details>
	<summary>See more</summary>
	
	focusIndicatorLeft
	^ borderWidth
</details>

#### PluggableScrollPane>>#vScrollBarHeight

<details>
	<summary>See more</summary>
	
	vScrollBarHeight
	^extent y - (2 * borderWidth)
</details>

#### PluggableScrollPane>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
</details>

#### PluggableScrollPane>>#setScrollDeltas

Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range.


<details>
	<summary>See more</summary>
	
	setScrollDeltas
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
self flag: #jmvVer.	"Revisar senders. Que no se haga al dopelin"
	self hideOrShowScrollBars.
	self vIsScrollbarShowing ifTrue: [ self vSetScrollDelta ].
	self hIsScrollbarShowing ifTrue: [ self hSetScrollDelta ]
</details>

#### PluggableScrollPane>>#vSetScrollDelta

Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range.


<details>
	<summary>See more</summary>
	
	vSetScrollDelta
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."
	| range delta h |
	
	delta _ self scrollDeltaHeight * 1.0.	"avoid Fraction arithmetic"
	range _ self vLeftoverScrollRange.
	range = 0 ifTrue: [
		^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0; internalScrollValue: 0 ].

	"Set up for one line (for arrow scrolling), or a full pane less one line (for paging)."
	h _ self viewableHeight * 1.0. 		"avoid Fraction arithmetic"
	scrollBar scrollDelta: delta / range pageDelta: h - delta / range.
	scrollBar interval: h / self vTotalScrollRange.
	scrollBar internalScrollValue: scrollBar scrollValue
</details>

#### PluggableScrollPane>>#focusIndicatorRight

<details>
	<summary>See more</summary>
	
	focusIndicatorRight
	^ self vIsScrollbarShowing
		ifTrue: [ extent x - borderWidth - self scrollBarClass scrollbarThickness ]
		ifFalse: [ extent x - borderWidth ]
</details>

#### PluggableScrollPane>>#borderWidth: anInteger

<details>
	<summary>See more</summary>
	
	borderWidth: anInteger
	super borderWidth: anInteger.
	self updateScrollBarsBounds
</details>

#### PluggableScrollPane>>#drawKeyboardFocusIndicator: aBoolean

<details>
	<summary>See more</summary>
	
	drawKeyboardFocusIndicator: aBoolean

	drawKeyboardFocusIndicator _ aBoolean
</details>

#### PluggableScrollPane>>#viewableExtent

<details>
	<summary>See more</summary>
	
	viewableExtent

	^ self viewableWidth @ self viewableHeight
</details>

#### PluggableScrollPane>>#updateScrollBarsBounds

<details>
	<summary>See more</summary>
	
	updateScrollBarsBounds
	
	| t |
	hideScrollBars ifTrue: [^self].
	t _ self scrollBarClass scrollbarThickness.
	scrollBar
		morphPosition: extent x - t - borderWidth @ borderWidth
		extent: t @ self vScrollBarHeight.
	hScrollBar
		morphPosition: borderWidth @ (extent y - t - borderWidth)
		extent: self hScrollBarWidth@t
</details>

#### PluggableScrollPane>>#vTotalScrollRange

Return the height extent of the receiver's scrollable area


<details>
	<summary>See more</summary>
	
	vTotalScrollRange
	"Return the height extent of the receiver's scrollable area"
	^scroller morphHeight
</details>

#### PluggableScrollPane>>#verticalScrollBar

<details>
	<summary>See more</summary>
	
	verticalScrollBar
	^scrollBar
</details>

#### PluggableScrollPane>>#vIsScrollbarNeeded

Return whether the vertical scrollbar is needed


<details>
	<summary>See more</summary>
	
	vIsScrollbarNeeded
	"Return whether the vertical scrollbar is needed"
	
	"Don't show it if we were told not to."
	hideScrollBars ifTrue: [ ^false ].

	^self vLeftoverScrollRange > 0
</details>

#### PluggableScrollPane>>#scrollDeltaWidth

Return the increment in pixels which this pane should be scrolled (normally a subclass responsibility).


<details>
	<summary>See more</summary>
	
	scrollDeltaWidth
	"Return the increment in pixels which this pane should be scrolled (normally a subclass responsibility)."
	
	^10

</details>

#### PluggableScrollPane>>#scrollerOffset: newOffset

<details>
	<summary>See more</summary>
	
	scrollerOffset: newOffset

	scroller morphPosition: self viewableAreaTopLeft - newOffset
</details>

#### PluggableScrollPane>>#handlesMouseScroll: aMouseScrollEvent

Only accept if we can actually do something useful with the event (i.e. not scrolling up when already at the top or down when already at the bottom) or if my owner chain doesn't want it


<details>
	<summary>See more</summary>
	
	handlesMouseScroll: aMouseScrollEvent
	"Only accept if we can actually do something useful with the event (i.e. not scrolling up when already at the top or down when already at the bottom) or if my owner chain doesn't want it"

	(aMouseScrollEvent direction = #up and: [ scrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #down and: [ scrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #left and: [ hScrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #right and: [ hScrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	"Even if I don't want it, one of my owners might. (i.e. nested scroll panes)  If my owners don't want it, accept the event to make sure that morphs behind me doesn't get the event."
	(self privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent)
		ifFalse: [ ^ true ].
	^ false
</details>

#### PluggableScrollPane>>#vScrollBarValue: scrollValue

<details>
	<summary>See more</summary>
	
	vScrollBarValue: scrollValue

	self scrollerOffset: (self scrollerOffset x @ (self vLeftoverScrollRange * scrollValue) rounded)

</details>

#### PluggableScrollPane>>#scroller

<details>
	<summary>See more</summary>
	
	scroller
^scroller
</details>

#### PluggableScrollPane>>#handlesMouseOver: evt

subclasses implement #mouseEnter: and/or #mouseLeave:


<details>
	<summary>See more</summary>
	
	handlesMouseOver: evt
	"subclasses implement #mouseEnter: and/or #mouseLeave:"
	^true
</details>

#### PluggableScrollPane>>#scrollerOffset

<details>
	<summary>See more</summary>
	
	scrollerOffset

	^ scroller morphPosition negated + self viewableAreaTopLeft
</details>

#### PluggableScrollPane>>#clipsLastSubmorph

Drawing specific. If this property is set, clip the receiver's last submorph to the receiver's shape


<details>
	<summary>See more</summary>
	
	clipsLastSubmorph
	"Drawing specific. If this property is set, clip the receiver's  
	last submorph to the receiver's shape"
	^ true
</details>

#### PluggableScrollPane>>#viewableHeight

Viewable height. Leave room for horizontal scrollbar if present


<details>
	<summary>See more</summary>
	
	viewableHeight
	"Viewable height.
	Leave room for horizontal scrollbar if present"

	^ self viewableAreaBottom - self viewableAreaTop 
</details>

## ResizeMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ResizeMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	self selectTo: localEventPosition.
	outlineMorph delete.
	action ifNotNil: [
		action value.
		self delete]
</details>

#### ResizeMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ `400@300`.
	color _ `Color white`.
	grid _ `8@6`.
	gridLineWidth _ 2.
	gridColor _ `Color black`.
	selectionColor _ `Color red`
</details>

#### ResizeMorph>>#updateOutlineMorph

<details>
	<summary>See more</summary>
	
	updateOutlineMorph
	| rectangle |
	rectangle _ self selectionRectangle: Display extent.
	outlineMorph
		morphPosition: rectangle origin extent: rectangle extent;
		show
</details>

#### ResizeMorph>>#mouseMove: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseDown: We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseButtonEvent localPosition: localEventPosition
	self selectTo: localEventPosition
</details>

#### ResizeMorph>>#selectTo: localEventPosition

<details>
	<summary>See more</summary>
	
	selectTo: localEventPosition
	| newTo |
	newTo _ self toGridPoint: localEventPosition.
	newTo ~= to ifTrue: [
		to _ newTo.
		self redrawNeeded.
		self updateOutlineMorph]
</details>

#### ResizeMorph>>#toGridPoint: aPoint

<details>
	<summary>See more</summary>
	
	toGridPoint: aPoint
	^(aPoint min: extent - 1) // (extent // grid)
</details>

#### ResizeMorph>>#grid: aPoint

<details>
	<summary>See more</summary>
	
	grid: aPoint
	grid _ aPoint
</details>

#### ResizeMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	^true
</details>

#### ResizeMorph>>#printOn: aStream

Add the identity of the receiver to a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream space; print: from; space; print: to
</details>

#### ResizeMorph>>#selectionRectangle: aRectangle

<details>
	<summary>See more</summary>
	
	selectionRectangle: aRectangle
	^(from corner: to + 1) scaledBy: aRectangle // grid
</details>

#### ResizeMorph>>#action: aBlock

<details>
	<summary>See more</summary>
	
	action: aBlock
	action _ aBlock
</details>

#### ResizeMorph>>#drawGridOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawGridOn: aCanvas
	0 to: grid x do: [:i |
		| x |
		x _ i * (extent x - gridLineWidth) / grid x.
		aCanvas line: x @ 0 to: x @ (extent y - 2) width: gridLineWidth color: gridColor].
	0 to: grid y do: [:i |
		| y |
		y _ i * (extent y - gridLineWidth) / grid y.
		aCanvas line: 0 @ y to: (extent x - 2) @ y width: gridLineWidth color: gridColor]
</details>

#### ResizeMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	super drawOn: aCanvas.
	from ifNotNil: [aCanvas fillRectangle: (self selectionRectangle: extent) color: selectionColor].
	self drawGridOn: aCanvas
</details>

#### ResizeMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	from _ self toGridPoint: localEventPosition.
	outlineMorph _ BorderedRectMorph new
		borderColor: `Color black`;
		color: `Color transparent`;
		openInWorld;
		hide.
	self selectTo: localEventPosition
</details>

## SystemWindow

SystemWindow is the Morphic implementation of Window: a labelled container for rectangular views, with iconic facilities for close, collapse/expand, and resizing. The attribute onlyActiveOnTop, if set to true (and any call to activate will set this), determines that only the top member of a collection of such windows on the screen shall be active. To be not active means that a mouse click in any region will only result in bringing the window to the top and then making it active.

### Methods
#### SystemWindow>>#resizeFull

<details>
	<summary>See more</summary>
	
	resizeFull
	self resize: Display boundingBox
</details>

#### SystemWindow>>#windowBottomRight: aPoint

aPoint is an X@Y coordinate pair in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowBottomRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	self morphExtent: aPoint - self morphPosition
</details>

#### SystemWindow>>#labelHeight

Answer the height for the window label.


<details>
	<summary>See more</summary>
	
	labelHeight
	"Answer the height for the window label."
	Theme current minimalWindows ifTrue: [^ 0].
	^ Preferences windowTitleFont lineSpacing+1
</details>

#### SystemWindow>>#drawClassicFrameOn: aCanvas color: titleColor

Window border encompasses title area. No round corners. No title gradient.


<details>
	<summary>See more</summary>
	
	drawClassicFrameOn: aCanvas color: titleColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: titleColor
</details>

#### SystemWindow>>#closeBoxHit

The user clicked on the close-box control in the window title. For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down.


<details>
	<summary>See more</summary>
	
	closeBoxHit
	"The user clicked on the close-box control in the window title.  For Mac users only, the Mac convention of option-click-on-close-box is obeyed if the mac option key is down."

	Preferences dismissAllOnOptionClose ifTrue:
		[Sensor rawMacOptionKeyPressed ifTrue:
			[^ self world closeUnchangedWindows]].
	self delete

</details>

#### SystemWindow>>#visible: aBoolean

set the 'visible' attribute of the receiver to aBoolean


<details>
	<summary>See more</summary>
	
	visible: aBoolean
	super visible: aBoolean.
	aBoolean ifTrue: [
		self activate ]
</details>

#### SystemWindow>>#addMorph: aMorph

Add a submorph to our client area.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph 
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph
</details>

#### SystemWindow>>#fullScreen

Zoom Window to Full World size with possible DeskMargins


<details>
	<summary>See more</summary>
	
	fullScreen
	"Zoom Window to Full World size with possible DeskMargins"

	"SystemWindow fullScreen"

	| left right possibleBounds |
	(self hasProperty: #originalBounds)
		ifFalse: [ "Expand"
			self setProperty: #originalBounds toValue: self morphBoundsInWorld.
			left := right := 0.
			possibleBounds := (RealEstateAgent maximumUsableAreaInWorld: self world) 
				insetBy: (left @ 0 corner: right @ 0).
			possibleBounds := possibleBounds insetBy: Theme current fullScreenDeskMargin
		] 
		ifTrue: [ "Contract"
			possibleBounds := self valueOfProperty: #originalBounds.
			self removeProperty: #originalBounds.
		].
	self morphPosition: possibleBounds topLeft extent: possibleBounds extent
</details>

#### SystemWindow>>#resize

<details>
	<summary>See more</summary>
	
	resize
	| resizeMorph |
	resizeMorph _ ResizeMorph new morphExtent: `200@150`.
	resizeMorph action: [self resize: (resizeMorph selectionRectangle: Display extent)].
	resizeMorph morphPosition: self world activeHand morphPosition.
	resizeMorph openInWorld
</details>

#### SystemWindow>>#titleBarButtonsExtent

answer the extent to use for close & other title bar buttons. the label height is used to be proportional to the fonts preferences


<details>
	<summary>See more</summary>
	
	titleBarButtonsExtent
	"answer the extent to use for close & other title bar buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^`0@0`].
	e _ Preferences windowTitleFont pointSize.
	^e@e
</details>

#### SystemWindow>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	| thisWorld |
	self okToChange ifFalse: [^self].
	thisWorld _ self world.
	SystemWindow noteTopWindowIn: thisWorld but: self.
	self sendToBack.
	self removeHalo.
	super delete.
	self model: nil.
	Theme current windowClosed: self
</details>

#### SystemWindow>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color white`
</details>

#### SystemWindow>>#windowLeft: aNumber

aNumber is an X coordinate in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowLeft: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ aNumber @ p y.
	e _ extent x + p x - aNumber @ extent y.
	self morphPosition: newP extent: e
</details>

#### SystemWindow>>#printOn: aStream

Add the identity of the receiver to a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	aStream nextPutAll: labelString asString
</details>

#### SystemWindow>>#addTileResizerMenuTo: aMenu

We can look at preferences here to decide what too do


<details>
	<summary>See more</summary>
	
	addTileResizerMenuTo: aMenu
	"We can look at preferences here to decide what too do"
	(Preferences tileResizerInWindowMenu) ifFalse: [
		aMenu add: 'resize full' 		action: #resizeFull icon: #resizeFullIcon;
		add: 'resize top' 				action: #resizeTop icon: #resizeTopIcon;
		add: 'resize left' 				action: #resizeLeft icon: #resizeLeftIcon;
		add: 'resize bottom' 			action: #resizeBottom icon: #resizeBottomIcon;
		add: 'resize right' 				action: #resizeRight icon: #resizeRightIcon;
		add: 'resize top left' 			action: #resizeTopLeft icon: #resizeTopLeftIcon;
		add: 'resize top right' 		action: #resizeTopRight icon: #resizeTopRightIcon;
		add: 'resize bottom left' 		action: #resizeBottomLeft icon: #resizeBottomLeftIcon;
		add: 'resize bottom right' 	action: #resizeBottomRight icon: #resizeBottomRightIcon]
	ifTrue: [ |resizeMorph|
		"Use embedded resize morph"
		resizeMorph _ TileResizeMorph new
							selectionColor: (self widgetsColor adjustSaturation: -0.2 brightness: 0.25) ;
							action: [:resize | |resizeMsg|
								resizeMsg _ ('resize', resize asString capitalized) asSymbol.
								self perform: resizeMsg.
								aMenu delete];
							yourself.
		aMenu addMorphBack: resizeMorph].
	^aMenu.
</details>

#### SystemWindow>>#createMenuButton

<details>
	<summary>See more</summary>
	
	createMenuButton
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		icon: Theme current windowMenuIcon;
		iconName: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: self titleBarButtonsExtent
</details>

#### SystemWindow>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime
	^ 200 "milliseconds"
</details>

#### SystemWindow>>#sendToBack

<details>
	<summary>See more</summary>
	
	sendToBack
	| thisWorld |
	thisWorld _ self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorphBack: self ]
</details>

#### SystemWindow>>#wantsSteps

Return true if the model wants its view to be stepped. For an open system window, we give the model to offer an opinion


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the model wants its view to be stepped.  For an open system window, we give the model to offer an opinion"

	^ model wantsSteps
</details>

#### SystemWindow>>#initialize

Initialize a system window. Add label, stripes, etc., if desired


<details>
	<summary>See more</summary>
	
	initialize
	"Initialize a system window. Add label, stripes, etc., if desired"

	super initialize.
	labelString ifNil: [ labelString _ 'Untitled Window'].
	
	self initializeLabelArea.
	extent _ `300 @ 200`.

	adjusters _ Dictionary new.
	adjusters at: #topAdjuster put: WindowEdgeAdjustingMorph forTop.
	adjusters at: #bottomAdjuster put: WindowEdgeAdjustingMorph forBottom.
	adjusters at: #leftAdjuster put: WindowEdgeAdjustingMorph forLeft.
	adjusters at: #rightAdjuster put: WindowEdgeAdjustingMorph forRight.
	adjusters at: #topLeftAdjuster put: WindowEdgeAdjustingMorph forTopLeft.
	adjusters at: #bottomLeftAdjuster put: WindowEdgeAdjustingMorph forBottomLeft.
	adjusters at: #topRightAdjuster put: WindowEdgeAdjustingMorph forTopRight.
	adjusters at: #bottomRightAdjuster put: WindowEdgeAdjustingMorph forBottomRight.
	adjusters do: [ :m |
		self addMorphFront: m ].

	"by default"
	self beColumn
</details>

#### SystemWindow>>#makeMeVisible

<details>
	<summary>See more</summary>
	
	makeMeVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self labelHeight)]) ifTrue: [
		^ self "OK -- at least my top left is visible"].

	"window not on screen (probably due to reframe) -- move it now"
	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: extent world: self world) topLeft
</details>

#### SystemWindow>>#makeSecondTopmost

<details>
	<summary>See more</summary>
	
	makeSecondTopmost
	| thisWorld |
	thisWorld _ self world.
	(SystemWindow noteTopWindowIn: thisWorld but: self)
		ifNotNil: [ :nextWindow |
			thisWorld addMorph: self behind: nextWindow ]
</details>

#### SystemWindow>>#defaultButtonPaneHeight

Answer the user's preferred default height for new button panes.


<details>
	<summary>See more</summary>
	
	defaultButtonPaneHeight
	"Answer the user's preferred default height for new button panes."

	^ Theme current buttonPaneHeight
</details>

#### SystemWindow>>#addPossiblyUncoveredAreasIn: aRectangle to: aCollection

Answer an array of rectangles encompassing those areas in aRectangle not completely covered by self. These are the areas that might require further drawing (of morphs below us) All areas that might possibly be uncovered must be included.


<details>
	<summary>See more</summary>
	
	addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self. These are the areas that might require further drawing (of morphs below us)
	All areas that might possibly be uncovered must be included."
	 | bounds radious |
	color mightBeTranslucent ifTrue: [
		aCollection add: aRectangle.
		^self ].

	bounds _ self morphBoundsInWorld.
	bounds ifNil: [
		aCollection add: aRectangle.
		^self ].

	"Solid rectangle.
	This will be the fastest in many cases. So, please disable rounded corners if on slow hardware!"
	Theme current roundWindowCorners ifFalse: [
		aRectangle areasOutside: bounds do: [ :rect |  aCollection add: rect ].
		^self ].

	"The solid rectangle does not include the corners.
	Report a couple of rows (top and bottom) or columns (left and right) as uncovered areas.
	We could also try to be more careful and answer each rounded corner...
	Right now, report top and bottom rows as uncovered areas"
	radious _ Theme current roundedWindowRadius.
	aRectangle areasOutside: (bounds insetBy: 0@radious) do: [ :rect |  aCollection add: rect ]
</details>

#### SystemWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	"Some default"
	^model class windowColor
</details>

#### SystemWindow>>#buttonColor

<details>
	<summary>See more</summary>
	
	buttonColor

	^Theme current buttonColorFrom: self windowColor
</details>

#### SystemWindow>>#label

Answer the name to show in a list of windows-and-morphs to represent the receiver


<details>
	<summary>See more</summary>
	
	label
	^ labelString
</details>

#### SystemWindow>>#resizeBottomRight

<details>
	<summary>See more</summary>
	
	resizeBottomRight
	self resize: (Display boundingBox center corner: Display boundingBox corner)
</details>

#### SystemWindow>>#layoutMorph

<details>
	<summary>See more</summary>
	
	layoutMorph
	^layoutMorph
</details>

#### SystemWindow>>#isOpaqueMorph

Not really used, as we also reimplement #addPossiblyUncoveredAreasIn:to:


<details>
	<summary>See more</summary>
	
	isOpaqueMorph
	"Not really used, as we also reimplement #addPossiblyUncoveredAreasIn:to:"
	^(Theme current roundWindowCorners or: [ color mightBeTranslucent ]) not
</details>

#### SystemWindow>>#windowTop: aNumber

aNumber is an X coordinate in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowTop: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ p x @ aNumber.
	e _ extent x @ (extent y + p y - aNumber).
	self morphPosition: newP extent: e
</details>

#### SystemWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent
	^ RealEstateAgent standardWindowExtent
</details>

#### SystemWindow>>#resizeLeft

<details>
	<summary>See more</summary>
	
	resizeLeft
	self resize: (Display boundingBox right: Display width // 2)
</details>

#### SystemWindow>>#buildWindowMenu

<details>
	<summary>See more</summary>
	
	buildWindowMenu

	| aMenu |

	aMenu _ MenuMorph new defaultTarget: self.

	aMenu 
		add: 'change title...' 			action: #relabel 						icon: #saveAsIcon;
		add: 'window color...' 			action: #setWindowColor 			icon: #graphicsIcon.
	
	self hasSaveAs
		ifTrue: [ aMenu add: 'Save as ...' action: #saveContents icon: #saveAsIcon ].
		
	aMenu
		addLine.
		
	self addWindowControlTo: aMenu.
	self addTileResizerMenuTo: aMenu.

	^ aMenu
</details>

#### SystemWindow>>#saveContents

Prompts the user for a file name and saves the contents to the file


<details>
	<summary>See more</summary>
	
	saveContents
	"Prompts the user for a file name and saves the contents to the file"
	| fileName |
	self hasSaveAs ifFalse: [^self].
	
	fileName _ FillInTheBlankMorph request: 'Filename'.
	
	fileName isEmptyOrNil
		ifTrue: [ self notifyUserWith: 'Contents not saved']
		ifFalse: [ self saveContentsTo: fileName ].
</details>

#### SystemWindow>>#relabel

<details>
	<summary>See more</summary>
	
	relabel
	| newLabel |
	newLabel _ FillInTheBlankMorph 
		request: 'New title for this window'
		initialAnswer: labelString.
	newLabel isEmpty ifTrue: [^self].
	self setLabel: newLabel
</details>

#### SystemWindow>>#activateAndSendTopToBack: aBoolean

Bring me to the front and make me able to respond to mouse and keyboard


<details>
	<summary>See more</summary>
	
	activateAndSendTopToBack: aBoolean
	"Bring me to the front and make me able to respond to mouse and keyboard"

	| oldTop |
	owner 
		ifNil: [^self	"avoid spurious activate when drop in trash"].
	oldTop _ TopWindow.
	TopWindow _ self.

	oldTop ifNotNil: [
		oldTop redrawNeeded.
		aBoolean ifTrue: [
			| bottomWindow |
			bottomWindow _ oldTop owner submorphs reverse detect: [:one | one is: #SystemWindow].
			oldTop owner addMorph: oldTop behind: bottomWindow]].

	owner firstSubmorph == self 
		ifFalse: [
			"Bring me to the top if not already"
			owner addMorphFront: self].
	self redrawNeeded.

	"Set keyboard focus"
	self world ifNotNil: [ :w |
		w activeHand newKeyboardFocus: self submorphToFocusKeyboard ]
</details>

#### SystemWindow>>#update: aSymbol

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aSymbol
	super update: aSymbol.
	aSymbol == #relabel
		ifTrue: [ model ifNotNil: [ self setLabel: model labelString ]]
</details>

#### SystemWindow>>#resizeBottomLeft

<details>
	<summary>See more</summary>
	
	resizeBottomLeft
	self resize: (Display boundingBox leftCenter corner: Display boundingBox bottomCenter)
</details>

#### SystemWindow>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	Theme current minimalWindows ifTrue: [^ 1].
	^ Theme current roundWindowCorners
		ifTrue: [ 3 ]
		ifFalse: [ 2 ]
</details>

#### SystemWindow>>#submorphToFocusKeyboard

Might answer nil


<details>
	<summary>See more</summary>
	
	submorphToFocusKeyboard

	"Might answer nil"
	^self nextMorphThat: [ :m |  m handlesKeyboard and: [ m isReallyVisible ]]
</details>

#### SystemWindow>>#resizeTop

<details>
	<summary>See more</summary>
	
	resizeTop
	self resize: (Display boundingBox bottom: Display height // 2)
</details>

#### SystemWindow>>#openInWorld

Ensure all widgets have proper colors before opening


<details>
	<summary>See more</summary>
	
	openInWorld
	"Ensure all widgets have proper colors before opening"
	self widgetsColor: self windowColor.
	super openInWorld.
	Theme current windowOpen: self
</details>

#### SystemWindow>>#drawLabelOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawLabelOn: aCanvas

	| x0 y0 f w availableW l |
	f _ Preferences windowTitleFont.
	x0 _  f lineSpacing * 4 + 14.
	y0 _ 2+3.
	y0 _ f lineSpacing - f ascent // 2.
	availableW _ extent x - x0.
	l _ labelString.
	w _ f widthOfString: l.
	[ w > availableW ] whileTrue: [
		l _ l squeezedTo: (1.0 * l size * availableW / w) truncated.
		l isEmpty ifTrue: [ ^self ].
		w _ f widthOfString: l ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles
</details>

#### SystemWindow>>#widgetsColor: aColor

aColor will be used for titles, borders, etc. A variation of it, #paneColorFrom:, will be used for panes background


<details>
	<summary>See more</summary>
	
	widgetsColor: aColor
	"aColor will be used for titles, borders, etc.
	A variation of it, #paneColorFrom:, will be used for panes background"

	borderColor _ aColor.
	self color: self textBackgroundColor.
	self adoptWidgetsColor: borderColor
</details>

#### SystemWindow>>#addMorph: aMorph layoutSpec: aLayoutSpec

Add a submorph to our client area.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph to our client area."

	layoutMorph addMorph: aMorph layoutSpec: aLayoutSpec
</details>

#### SystemWindow>>#resizeRight

<details>
	<summary>See more</summary>
	
	resizeRight
	self resize: (Display boundingBox left: Display width // 2)
</details>

#### SystemWindow>>#owningWindow

Return the first enclosing morph that is a kind of Window, or nil if none


<details>
	<summary>See more</summary>
	
	owningWindow
	"Return the first enclosing morph that is a kind of Window, or nil if none"

	^ self
</details>

#### SystemWindow>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	| titleColor roundCorners |

	titleColor _ self widgetsColor.
	self isTopWindow
		ifTrue: [ titleColor _ titleColor lighter ].

	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: titleColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: titleColor ].
	Theme current minimalWindows
		ifFalse: [
			labelString ifNotNil: [self drawLabelOn: aCanvas]]
</details>

#### SystemWindow>>#setWindowColor: incomingColor

<details>
	<summary>See more</summary>
	
	setWindowColor: incomingColor
	| existingColor aColor |
	incomingColor ifNil: [^ self].  "it happens"
	aColor _ incomingColor asNontranslucentColor.
	aColor = `Color black` ifTrue: [^ self].
	existingColor _ self widgetsColor.
	existingColor ifNil: [^ Smalltalk beep].
	self widgetsColor: aColor.
	self redrawNeeded
</details>

#### SystemWindow>>#widgetsColor

<details>
	<summary>See more</summary>
	
	widgetsColor

	^borderColor
</details>

#### SystemWindow>>#beRow

<details>
	<summary>See more</summary>
	
	beRow
	layoutMorph
		ifNotNil: [ layoutMorph beRow ]
		ifNil: [
			layoutMorph _ LayoutMorph newRow.
			self addMorphFront: layoutMorph ]
</details>

#### SystemWindow>>#beColumn

<details>
	<summary>See more</summary>
	
	beColumn
	layoutMorph
		ifNotNil: [ layoutMorph beColumn ]
		ifNil: [
			layoutMorph _ LayoutMorph newColumn.
			self addMorphFront: layoutMorph ]
</details>

#### SystemWindow>>#resizeTopRight

<details>
	<summary>See more</summary>
	
	resizeTopRight
	self resize: (Display boundingBox topCenter corner: Display boundingBox rightCenter)
</details>

#### SystemWindow>>#resize: boundingBox

<details>
	<summary>See more</summary>
	
	resize: boundingBox
	(self hasProperty: #originalBounds) ifFalse: [
		self setProperty: #originalBounds toValue: self morphBoundsInWorld].
	self morphPosition: boundingBox origin extent: boundingBox extent
</details>

#### SystemWindow>>#windowRight: aNumber

aNumber is an X coordinate in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowRight: aNumber
	"aNumber is an X coordinate in the owner's coordinate system"

	self morphWidth: aNumber - self morphPosition x
</details>

#### SystemWindow>>#windowTopLeft: newPosition

aPoint is an X@Y coordinate pair in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowTopLeft: newPosition
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e |
	e _ extent + self morphPosition - newPosition.
	self morphPosition: newPosition extent: e
</details>

#### SystemWindow>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent

	^layoutMorph minimumExtent + (borderWidth * 2) + (0@self labelHeight) max: self titleBarButtonsExtent x * 6 @ 0
</details>

#### SystemWindow>>#setLabel: aString

<details>
	<summary>See more</summary>
	
	setLabel: aString

	labelString _ aString.
	self invalidateTitleArea
</details>

#### SystemWindow>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #SystemWindow or: [ super is: aSymbol ]
</details>

#### SystemWindow>>#offerWindowMenu

<details>
	<summary>See more</summary>
	
	offerWindowMenu

	self buildWindowMenu popUpInWorld: self world
</details>

#### SystemWindow>>#wantsStepsWhenCollapsed

Default is not to bother updating collapsed windows


<details>
	<summary>See more</summary>
	
	wantsStepsWhenCollapsed
	"Default is not to bother updating collapsed windows"

	^ false
</details>

#### SystemWindow>>#layoutSubmorphs

Compute a new layout of submorphs based on the given layout bounds.


<details>
	<summary>See more</summary>
	
	layoutSubmorphs
	"Compute a new layout of submorphs based on the given layout bounds."

	| h thickness w cornerExtent wh ww pos |
	thickness _ 4.
	cornerExtent _ 20.
	ww _ extent x.
	wh _ extent y.
	w _ ww - cornerExtent - cornerExtent.
	h _ wh - cornerExtent - cornerExtent.
	(adjusters at: #topAdjuster) morphPosition: cornerExtent@0 extent: w@thickness.
	(adjusters at: #bottomAdjuster) morphPosition: cornerExtent@(wh-thickness) extent: w@thickness.
	(adjusters at: #leftAdjuster) morphPosition: 0@cornerExtent extent: thickness@h.
	(adjusters at: #rightAdjuster) morphPosition: ww-thickness@cornerExtent extent: thickness@h.
	(adjusters at: #topLeftAdjuster) morphPosition: `0@0` extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomLeftAdjuster) morphPosition: 0@(wh-cornerExtent) extent: cornerExtent@cornerExtent.
	(adjusters at: #topRightAdjuster) morphPosition: ww-cornerExtent@0 extent: cornerExtent@cornerExtent.
	(adjusters at: #bottomRightAdjuster) morphPosition: ww@wh-cornerExtent extent: cornerExtent@cornerExtent.

	layoutMorph ifNotNil: [
		pos _ borderWidth @ (borderWidth + self labelHeight).
		layoutMorph
			morphPosition: pos
			extent: extent - pos - borderWidth ].
	
	layoutNeeded _ false
</details>

#### SystemWindow>>#resizeTopLeft

<details>
	<summary>See more</summary>
	
	resizeTopLeft
	self resize: (Display boundingBox origin corner: Display boundingBox center)
</details>

#### SystemWindow>>#canDiscardEditsOf: aMorphWithChanges

<details>
	<summary>See more</summary>
	
	canDiscardEditsOf: aMorphWithChanges 

	| okToLooseChanges |
	
	aMorphWithChanges canDiscardEdits ifTrue: [ ^true ].
	
	okToLooseChanges _ self isItOkToLooseChanges.
	okToLooseChanges ifTrue: [ aMorphWithChanges disregardUnacceptedEdits ].
	
	^okToLooseChanges

		
</details>

#### SystemWindow>>#addCustomMenuItems: aCustomMenu hand: aHandMorph

Add morph-specific items to the given menu which was invoked by the given hand. This method provides is invoked both from the halo-menu and from the control-menu regimes.


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
"template..."
	aCustomMenu addLine.
	aCustomMenu add: 'edit label...' action: #relabel.

</details>

#### SystemWindow>>#isItOkToLooseChanges

<details>
	<summary>See more</summary>
	
	isItOkToLooseChanges
	
	^ self confirm:
'Changes have not been saved.
Is it OK to cancel those changes?'.
</details>

#### SystemWindow>>#rescaleButtons

boxExtent changed. Update my buttons.


<details>
	<summary>See more</summary>
	
	rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonPos buttonExtent  buttonDelta|
	buttonExtent := self titleBarButtonsExtent.
	buttonPos _ self labelHeight + borderWidth - buttonExtent // 2 * (1@1).
	buttonDelta _ buttonExtent x *14//10.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos _ buttonPos + (buttonDelta@0).
		].
	]
</details>

#### SystemWindow>>#hasSaveAs

Returns true if the window has a model which can be saved to a file


<details>
	<summary>See more</summary>
	
	hasSaveAs
	"Returns true if the window has a model which can be saved to a file"
	^model is: #canSaveContents
</details>

#### SystemWindow>>#windowBottomLeft: aPoint

aPoint is an X@Y coordinate pair in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowBottomLeft: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ aPoint x @ p y.
	e _ extent x + p x - aPoint x @ (aPoint y - self morphPosition y).
	self morphPosition: newP extent: e
</details>

#### SystemWindow>>#changeColor

Change the color of the receiver -- triggered, e.g. from a menu. This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu


<details>
	<summary>See more</summary>
	
	changeColor
	"Change the color of the receiver -- triggered, e.g. from a menu.  This variant allows the recolor triggered from the window's halo recolor handle to have the same result as choosing change-window-color from the window-title menu"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self color;
		putUpFor: self near: self morphFullBoundsInWorld"
	self flag: #jmvVer2.
	self showBalloon: 'Interactive color change is currently disabled. Please use #color:'
</details>

#### SystemWindow>>#notifyUserWith: message

Notifies the user with a message and an 'ok' button


<details>
	<summary>See more</summary>
	
	notifyUserWith: message
	"Notifies the user with a message and an 'ok' button"
	| morph |
	morph _ MenuMorph new.
	morph addTitle: message.
	morph add: 'Ok' action: nil.
	morph openInWorld.
</details>

#### SystemWindow>>#initialFrameIn: aWorld

<details>
	<summary>See more</summary>
	
	initialFrameIn: aWorld
	^RealEstateAgent initialFrameFor: self world: aWorld
</details>

#### SystemWindow>>#drawRoundedFrameOn: aCanvas color: widgetsColor

Title area is not inside window borders


<details>
	<summary>See more</summary>
	
	drawRoundedFrameOn: aCanvas color: widgetsColor
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	aCanvas
		windowFrame: self morphLocalBounds
		color: widgetsColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: self labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: color
</details>

#### SystemWindow>>#addWindowControlTo: aMenu

<details>
	<summary>See more</summary>
	
	addWindowControlTo: aMenu

	aMenu
		add: 'send to back' 				action: #sendToBack 				icon: #goBottomIcon;
		add: 'make next-to-topmost' 	action: #makeSecondTopmost 	icon: #goUpIcon;
		addLine;
		add: (self isSticky ifTrue: [ 'make draggable' ] ifFalse: [ 'make undraggable' ])
											action: #toggleStickiness 			icon: (self isSticky ifFalse: [#pushPinIcon]);
		addLine;
		add: 'close' 						action: #delete 						icon: #closeIcon;
		add: 'collapse' 					action: #collapse 					icon: #collapseIcon;
		add: 'expand / contract' 		action: #expandBoxHit 				icon: #expandIcon;
		addLine;
		add: 'resize...' 					action: #resize.
		
	^aMenu
</details>

#### SystemWindow>>#makeMeFullyVisible

<details>
	<summary>See more</summary>
	
	makeMeFullyVisible 

	self world morphExtent > `0@0` ifFalse: [^ self].

	(self morphPosition >= `0@0` and: [ self morphPosition < (self world morphExtent-self morphExtent)]) ifTrue: [
		^ self "OK -- visible"].

	self morphPosition: (RealEstateAgent initialFrameFor: self initialExtent: self morphExtentInWorld world: self world) topLeft
</details>

#### SystemWindow>>#stepAt: millisecondSinceLast

If the receiver is not collapsed, step it, after first stepping the model.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [ self wantsStepsWhenCollapsed ]) ifTrue: [
		model ifNotNil: [ model stepAt: millisecondSinceLast ].
		super stepAt: millisecondSinceLast ]
</details>

#### SystemWindow>>#okToChange

<details>
	<summary>See more</summary>
	
	okToChange

	^self canDiscardEditsOf: self
</details>

#### SystemWindow>>#justDroppedInto: newOwnerMorph event: anEvent

This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph


<details>
	<summary>See more</summary>
	
	justDroppedInto: newOwnerMorph event: anEvent

	TopWindow ~~ self ifTrue: [ self activate ].
	^super justDroppedInto: newOwnerMorph event: anEvent
</details>

#### SystemWindow>>#windowBottom: aNumber

aNumber is an Y coordinate in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowBottom: aNumber
	"aNumber is an Y coordinate in the owner's coordinate system"

	self morphHeight: aNumber - self morphPosition y
</details>

#### SystemWindow>>#createCloseButton

<details>
	<summary>See more</summary>
	
	createCloseButton
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		icon: Theme current closeIcon;
		iconName: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: self titleBarButtonsExtent
</details>

#### SystemWindow>>#activateAndForceLabelToShow

<details>
	<summary>See more</summary>
	
	activateAndForceLabelToShow
	self activate.
	self morphPosition y < 0 ifTrue: [
		self morphPosition: (self morphPosition x @ 0)]
</details>

#### SystemWindow>>#windowTopRight: aPoint

aPoint is an X@Y coordinate pair in the owner's coordinate system


<details>
	<summary>See more</summary>
	
	windowTopRight: aPoint
	"aPoint is an X@Y coordinate pair in the owner's coordinate system"

	| e newP p |
	p _ self morphPosition.
	newP _ p x @ aPoint y.
	e _ (aPoint x - self morphPosition x) @ (extent y + p y - aPoint y).
	self morphPosition: newP extent: e
</details>

#### SystemWindow>>#invalidateTitleArea

not really pretty... also invalidating the top border, regardless of it being above or below the title area (Different themes use various looks, this covers them all)


<details>
	<summary>See more</summary>
	
	invalidateTitleArea

	"not really pretty... also invalidating the top border, regardless of it being above or below the title area
	(Different themes use various looks, this covers them all)"
	self invalidateLocalRect: (self morphTopLeft extent: extent x @ (self labelHeight + borderWidth))
</details>

#### SystemWindow>>#setWindowColor

Allow the user to select a new basic color for the window


<details>
	<summary>See more</summary>
	
	setWindowColor
	"Allow the user to select a new basic color for the window"

	"ColorPickerMorph new
		choseModalityFromPreference;
		sourceHand: self world activeHand;
		target: self;
		selector: #setWindowColor:;
		originalColor: self widgetsColor;
		putUpFor: self near: self morphFullBoundsInWorld"
</details>

#### SystemWindow>>#resizeBottom

<details>
	<summary>See more</summary>
	
	resizeBottom
	self resize: (Display boundingBox top: Display height // 2)
</details>

#### SystemWindow>>#initializeLabelArea

Initialize the label area (titlebar) for the window.


<details>
	<summary>See more</summary>
	
	initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft _ self labelHeight + borderWidth - self titleBarButtonsExtent // 2 * (1@1).
	spacing _ self titleBarButtonsExtent x *14//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft
</details>

#### SystemWindow>>#submorphsDrawingOutsideReverseDo: aBlock

All our submorphs are inside us


<details>
	<summary>See more</summary>
	
	submorphsDrawingOutsideReverseDo: aBlock
	"All our submorphs are inside us"
</details>

#### SystemWindow>>#activate

Bring me to the front and make me able to respond to mouse and keyboard


<details>
	<summary>See more</summary>
	
	activate
	"Bring me to the front and make me able to respond to mouse and keyboard"

	self activateAndSendTopToBack: false
</details>

#### SystemWindow>>#textBackgroundColor

<details>
	<summary>See more</summary>
	
	textBackgroundColor

	^Theme current paneBackgroundFrom: self widgetsColor
</details>

#### SystemWindow>>#fontPreferenceChanged

Preferred fonts scale a number of window relations. Let morphs which rely on this updte themselves. Note that the fontPreferenceChanged message is typically sent to the current world. As a PasteUpMorph iinherits from me the code below works fine for this.


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged

	super fontPreferenceChanged.
	self rescaleButtons.
	self model ifNotNil: [ :m | m changed: #actualContents ].
	self redrawNeeded.

</details>

#### SystemWindow>>#createCollapseButton

<details>
	<summary>See more</summary>
	
	createCollapseButton
	^(PluggableButtonMorph model: self action: #collapse)
		icon: Theme current collapseIcon;
		iconName: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: self titleBarButtonsExtent
</details>

#### SystemWindow>>#isTopWindow

<details>
	<summary>See more</summary>
	
	isTopWindow

	^ self == TopWindow
</details>

#### SystemWindow>>#wantsToBeDroppedInto: aMorph

Return true if it's okay to drop the receiver into aMorph


<details>
	<summary>See more</summary>
	
	wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph"
	^aMorph isWorldMorph or:[Preferences systemWindowEmbedOK]
</details>

#### SystemWindow>>#okToChangeDueTo: aMorph

<details>
	<summary>See more</summary>
	
	okToChangeDueTo: aMorph
	
	^self okToChange
</details>

#### SystemWindow>>#expandBoxHit

The full screen expand box has been hit


<details>
	<summary>See more</summary>
	
	expandBoxHit
	"The full screen expand box has been hit"

	self fullScreen
</details>

#### SystemWindow>>#openInWorld: aWorld

This msg and its callees result in the window being activeOnlyOnTop


<details>
	<summary>See more</summary>
	
	openInWorld: aWorld
	"This msg and its callees result in the window being activeOnlyOnTop"
	| frameRect |
	frameRect _ self initialFrameIn: aWorld.
	self morphExtent: frameRect extent.
	aWorld addMorph: self position: frameRect topLeft.
	"Do it deferred. Was needed for text cursor to start blinking if (Preferences disable: #focusFollowsMouse) "
	UISupervisor whenUIinSafeState: [ self activate ]
</details>

#### SystemWindow>>#saveContentsTo: fileName

Saves the contents to the given filename


<details>
	<summary>See more</summary>
	
	saveContentsTo: fileName
	"Saves the contents to the given filename"
	| stream |
	self hasSaveAs ifFalse: [^self].
	
	stream _ StandardFileStream new.
	stream open: fileName forWrite: true.
	
	model saveOn: stream as: 'text/plain'.
	
	stream closed ifFalse: [stream close].
	self notifyUserWith: 'Contents saved'.
</details>

#### SystemWindow>>#createExpandButton

<details>
	<summary>See more</summary>
	
	createExpandButton
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		icon: Theme current expandIcon;
		iconName: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: self titleBarButtonsExtent
</details>

## TextModelMorph

The main morph for text editors

### Methods
#### TextModelMorph>>#selectString

<details>
	<summary>See more</summary>
	
	selectString

	self editor
		setSearch: model autoSelectString;
		findAndReplaceMany: true 
</details>

#### TextModelMorph>>#updateClearUserEdits

Quite ugly


<details>
	<summary>See more</summary>
	
	updateClearUserEdits
	
	"Quite ugly"
	^self hasUnacceptedEdits: false

</details>

#### TextModelMorph>>#acceptOnCR: aBoolean

<details>
	<summary>See more</summary>
	
	acceptOnCR: aBoolean
	self textMorph acceptOnCR: aBoolean
</details>

#### TextModelMorph>>#mightNeedHorizontalScrollBar

If not sure, answer true. Only answer false when the horizontal scrollbar will not be needed, regardless of contents, for example, when showing wrapped text. This method is called for deciding if a vertical scroll bar is needed. Therefore if the need of an horizontal scrollbar might depend on the vertical scrollbar being there (and taking space) or not, just answer true.


<details>
	<summary>See more</summary>
	
	mightNeedHorizontalScrollBar

	self textMorph isWrapped ifTrue: [ ^false ].

	^super mightNeedHorizontalScrollBar

</details>

#### TextModelMorph>>#setTextColor: aColor

Set the color of my text to the given color


<details>
	<summary>See more</summary>
	
	setTextColor: aColor
	"Set the color of my text to the given color"

	self textMorph color: aColor
</details>

#### TextModelMorph>>#updateRefetched

#actualContents also signalled when #refetched is signalled. No need to repeat what's done there.


<details>
	<summary>See more</summary>
	
	updateRefetched 

	"#actualContents also signalled when #refetched is signalled.
	No need to repeat what's done there."
	self setSelection: model getSelection.
	self hasUnacceptedEdits: false.
	
	^self 
</details>

#### TextModelMorph>>#scrollSelectionIntoView

Scroll my text into view if necessary and return true, else return false


<details>
	<summary>See more</summary>
	
	scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"

	| delta |
	delta _ self textMorph morphPosition.
	self editor pointIndex > 1
		ifTrue: [
			self scrollToShow: (self editor pointBlock translatedBy: delta) ]
		ifFalse: [
			self scrollToShow: (self editor selectionRectangle translatedBy: delta) ]
</details>

#### TextModelMorph>>#doubleClick: aMouseButtonEvent localPosition: localEventPosition

Handle a double-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing.


<details>
	<summary>See more</summary>
	
	doubleClick: aMouseButtonEvent localPosition: localEventPosition

	self textMorph doubleClick: aMouseButtonEvent localPosition: localEventPosition
</details>

#### TextModelMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current textPane
</details>

#### TextModelMorph>>#doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

Some subclasses might do something


<details>
	<summary>See more</summary>
	
	doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self textMorph doubleClickAndHalf: aMouseButtonEvent localPosition: localEventPosition
</details>

#### TextModelMorph>>#keyStroke: aKeyboardEvent

A keystroke was hit while the receiver had keyboard focus. Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler


<details>
	<summary>See more</summary>
	
	keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	self textMorph keyStroke: aKeyboardEvent
</details>

#### TextModelMorph>>#innerMorphClass

<details>
	<summary>See more</summary>
	
	innerMorphClass
	^InnerTextMorph
</details>

#### TextModelMorph>>#wrapFlag: aBoolean

<details>
	<summary>See more</summary>
	
	wrapFlag: aBoolean
	self textMorph wrapFlag: aBoolean
</details>

#### TextModelMorph>>#mouseButton2Activity

Invoke the menu


<details>
	<summary>See more</summary>
	
	mouseButton2Activity
	self textMorph mouseButton2Activity
</details>

#### TextModelMorph>>#updateAutoSelect

<details>
	<summary>See more</summary>
	
	updateAutoSelect 

	TextEditor abandonChangeText.	"no replacement!"
	self selectMessage
		ifFalse: [ self selectString ].

	self textMorph updateFromTextComposition.
	^self scrollSelectionIntoView
</details>

#### TextModelMorph>>#askBeforeDiscardingEdits: aBoolean

Set the flag that determines whether the user should be asked before discarding unaccepted edits.


<details>
	<summary>See more</summary>
	
	askBeforeDiscardingEdits: aBoolean
	"Set the flag that determines whether the user should be asked before discarding unaccepted edits."

	self textMorph askBeforeDiscardingEdits: aBoolean
</details>

#### TextModelMorph>>#enableEditing

<details>
	<summary>See more</summary>
	
	enableEditing

	self textMorph enableEditing
</details>

#### TextModelMorph>>#focusText

<details>
	<summary>See more</summary>
	
	focusText

	self world activeHand newKeyboardFocus: self textMorph
</details>

#### TextModelMorph>>#clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	clickAndHalf: aMouseButtonEvent localPosition: localEventPosition

	self textMorph clickAndHalf: aMouseButtonEvent localPosition: localEventPosition
</details>

#### TextModelMorph>>#mouseEnter: event

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: event
	super mouseEnter: event.
	Preferences focusFollowsMouse
		ifTrue: [ event hand newKeyboardFocus: self textMorph ]
</details>

#### TextModelMorph>>#hasUnacceptedEdits: aBoolean

Set the hasUnacceptedEdits flag in my morph.


<details>
	<summary>See more</summary>
	
	hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag in my morph."
	self textMorph hasUnacceptedEdits: aBoolean
</details>

#### TextModelMorph>>#updateAcceptedContents

<details>
	<summary>See more</summary>
	
	updateAcceptedContents

	self textMorph hasUnacceptedEdits ifTrue: [
		self textMorph hasEditingConflicts: true.
		^self redrawNeeded ].
	model refetch.
	"#actualContents also signalled in #refetch. No need to repeat what's done there."
	^self 
</details>

#### TextModelMorph>>#scrollDeltaHeight

Return the increment in pixels which this pane should be scrolled.


<details>
	<summary>See more</summary>
	
	scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."

	^ AbstractFont default lineSpacing
</details>

#### TextModelMorph>>#nextTokenFrom: start direction: dir

<details>
	<summary>See more</summary>
	
	nextTokenFrom: start direction: dir
	^ self editor nextTokenFrom: start direction: dir
</details>

#### TextModelMorph>>#canDiscardEdits

Return true if this view either has no text changes or does not care.


<details>
	<summary>See more</summary>
	
	canDiscardEdits
	"Return true if this view either has no text changes or does not care."

	^ self textMorph canDiscardEdits
</details>

#### TextModelMorph>>#selectionInterval

<details>
	<summary>See more</summary>
	
	selectionInterval
	^ self editor selectionInterval
</details>

#### TextModelMorph>>#editor

<details>
	<summary>See more</summary>
	
	editor
	^self textMorph editor
</details>

#### TextModelMorph>>#updateShoutStyled

<details>
	<summary>See more</summary>
	
	updateShoutStyled

	self textMorph stylerStyled.
	^self redrawNeeded 
</details>

#### TextModelMorph>>#possiblyChanged

A hook for notificating possible interested parties Not used in base system


<details>
	<summary>See more</summary>
	
	possiblyChanged
	"A hook for notificating possible interested parties
	Not used in base system"
</details>

#### TextModelMorph>>#updateActualContents

Some day, it would be nice to keep objects and update them instead of throwing them away all the time for no good reason...


<details>
	<summary>See more</summary>
	
	updateActualContents 

	"Some day, it would be nice to keep objects and update them
	instead of throwing them away all the time for no good reason..."
	self textMorph
		releaseEditorAndTextComposition;
		installEditorAndTextComposition;
		formatAndStyleIfNeeded.
	self setScrollDeltas.
	self redrawNeeded.
	^self 
</details>

#### TextModelMorph>>#correctFrom: start to: stop with: aString

<details>
	<summary>See more</summary>
	
	correctFrom: start to: stop with: aString

	^ self editor correctFrom: start to: stop with: aString
</details>

#### TextModelMorph>>#innerHeight: aNumber

Adjust height and scrollbar to the new contents height. Nothing to do here: TextModelMorph height does not depend on contents height.


<details>
	<summary>See more</summary>
	
	innerHeight: aNumber
	"Adjust height and scrollbar to the new contents height.
	Nothing to do here: TextModelMorph height does not depend on contents height."
</details>

#### TextModelMorph>>#selectAll

Tell my textMorph's editor to select all


<details>
	<summary>See more</summary>
	
	selectAll
	"Tell my textMorph's editor to select all"

	self textMorph selectAll
</details>

#### TextModelMorph>>#update: aSymbol

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aSymbol

	super update: aSymbol.
	aSymbol ifNil: [^self].

	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [ ^self updateActualContents ].
	aSymbol == #acceptedContents ifTrue: [ ^self updateAcceptedContents ].
	aSymbol == #refetched ifTrue: [ ^self updateRefetched ].
	aSymbol == #initialSelection ifTrue: [ ^self updateInitialSelection ].
	aSymbol == #autoSelect ifTrue: [ ^self updateAutoSelect ].
	aSymbol == #clearUserEdits ifTrue: [ ^self updateClearUserEdits ].
	aSymbol == #shoutStyled ifTrue: [ ^self updateShoutStyled ].

</details>

#### TextModelMorph>>#model: aTextModel

Set my model and make me me a dependent of the given object.


<details>
	<summary>See more</summary>
	
	model: aTextModel

	super model: aTextModel.
	self textMorph model: model wrappedTo: self viewableWidth.
	model refetch.
	self setScrollDeltas
</details>

#### TextModelMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |
	eventPositionLocalToScroller _ localEventPosition - scroller morphPosition.
	scroller mouseButton1Down: aMouseButtonEvent localPosition: eventPositionLocalToScroller.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil
</details>

#### TextModelMorph>>#selectInvisiblyFrom: start to: stop

<details>
	<summary>See more</summary>
	
	selectInvisiblyFrom: start to: stop
	^ self editor selectInvisiblyFrom: start to: stop
</details>

#### TextModelMorph>>#escAction: aBlock

<details>
	<summary>See more</summary>
	
	escAction: aBlock
	
	self textMorph escAction: aBlock
</details>

#### TextModelMorph>>#selectMessage

<details>
	<summary>See more</summary>
	
	selectMessage

	| messageSendsRanges |

	messageSendsRanges := model textProvider messageSendsRangesOf: model autoSelectString.
	^ messageSendsRanges notEmpty
		ifTrue: [ self editor messageSendsRanges: messageSendsRanges ]; yourself
</details>

#### TextModelMorph>>#notify: aString at: anInteger in: aStream

<details>
	<summary>See more</summary>
	
	notify: aString at: anInteger in: aStream
	^ self editor notify: aString at: anInteger in: aStream
</details>

#### TextModelMorph>>#updateInitialSelection

<details>
	<summary>See more</summary>
	
	updateInitialSelection

	^self 
		setSelection: model getSelection; 
		redrawNeeded
</details>

#### TextModelMorph>>#textMorph

<details>
	<summary>See more</summary>
	
	textMorph
self flag: #jmvVer.
"Podemos reemplazar los senders locales por accesos directos (el doble encapsulamiento es tonto) una vez que quede definido el shape de la clase!
Y la variable deberia ser innerMorph o algo asi...
Y el getter para callers externos tambien deberia ser #innerMorph"
	^ scroller
</details>

#### TextModelMorph>>#disableEditing

<details>
	<summary>See more</summary>
	
	disableEditing
	self textMorph disableEditing
</details>

#### TextModelMorph>>#wantsFrameAdornments

<details>
	<summary>See more</summary>
	
	wantsFrameAdornments
	^model wantsFrameAdornments
</details>

#### TextModelMorph>>#selectFrom: start to: stop

<details>
	<summary>See more</summary>
	
	selectFrom: start to: stop
	^ self editor selectFrom: start to: stop
</details>

#### TextModelMorph>>#emptyTextDisplayMessage: aString

<details>
	<summary>See more</summary>
	
	emptyTextDisplayMessage: aString
	self
		setProperty: #emptyTextDisplayMessage
		toValue: aString.
</details>

#### TextModelMorph>>#text

The returned object should be treated as read-only, and never modified


<details>
	<summary>See more</summary>
	
	text
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
</details>

#### TextModelMorph>>#drawOn: aCanvas

Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	"Include a thin red inset border for unaccepted edits, or, if the unaccepted edits are known to conflict with a change made somewhere else to the same method (typically), put a thick red frame"
	| bw bc |
	self flag: #todo.
	"Integrate this method with the Theme system. --cbr"
	super drawOn: aCanvas.
	bw _ Preferences focusIndicatorWidth.
	bc _ nil.
	self wantsFrameAdornments ifTrue: [
		model refusesToAccept
			ifTrue: [ "Put up feedback showing that code cannot be submitted in this state"
				bc _ Color tan ]
			ifFalse: [
				self textMorph hasEditingConflicts
					ifTrue: [
						bw _ 3.
						bc _ Color red ]
					ifFalse: [
						self textMorph hasUnacceptedEdits ifTrue: [ bc _ Color red ]]]].
	(drawKeyboardFocusIndicator and: [ self textMorph hasKeyboardFocus ])
		ifTrue: [ bc ifNil: [ bc _ Theme current focusIndicator ]]
		ifFalse: [
			bc ifNotNil: [
				bc _ bc
					alphaMixed: 0.4
					with: Color white ]].
	bc ifNotNil: [
		aCanvas
			frameRectangle: self focusIndicatorRectangle
			borderWidth: bw
			color: bc ].
	model actualContents isEmpty ifTrue: [
		self
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas  clippingByCurrentMorphDo: [
					aCanvas
						drawString: msg
						at: self viewableAreaTopLeft
						font: nil
						color: Theme current textEmptyDisplayMessage ]]].
</details>

#### TextModelMorph>>#setSelection: sel

<details>
	<summary>See more</summary>
	
	setSelection: sel

	sel == #all
		ifTrue: [ self editor selectAll ]
		ifFalse: [
			sel
				ifNil: [ self editor selectFrom:1 to: 0 ]
				ifNotNil: [ self editor selectFrom: sel first to: sel last ]].
	self scrollSelectionIntoView
</details>

## TileResizeMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TileResizeMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition.
	action ifNotNil: [
		action value: selectedResize.
		self delete]
</details>

#### TileResizeMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	extent _ 120@60.
	color _ Color white.
	selectionColor _ Color lightYellow .
	self borderColor: Color black.
	self borderWidth: 1.
</details>

#### TileResizeMorph>>#resizeAtPoint: aPoint

<details>
	<summary>See more</summary>
	
	resizeAtPoint: aPoint
	
	|region|
		
	region _ (aPoint min: extent - 1) // (extent // 3).
	
	^ region caseOf: {
		[`0@0`] -> [#topLeft].
		[`1@0`] -> [#top].
		[`2@0`] -> [#topRight].
		[`0@1`] -> [#left].
		[`1@1`] -> [#full].
		[`2@1`] -> [#right].
		[`0@2`] -> [#bottomLeft].
		[`1@2`] -> [#bottom].
		[`2@2`] -> [#bottomRight].
	} otherwise: [nil]
</details>

#### TileResizeMorph>>#handlesMouseHover

Do I want to receive unhandled mouseMove events when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseHover
	^ true
</details>

#### TileResizeMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent

	^ true
</details>

#### TileResizeMorph>>#mouseHover: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse move event. This message will only be sent to Morphs that answer true to #handlesMouseHover for events that have not been previously handled. We can query aMouseMoveEvent to know about pressed mouse buttons.


<details>
	<summary>See more</summary>
	
	mouseHover: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition
</details>

#### TileResizeMorph>>#selectionRectangle: region

<details>
	<summary>See more</summary>
	
	selectionRectangle: region

	^ region caseOf: {
		[#topLeft] -> [`0@0` corner: (extent // 2)].
		[#top] -> [`0@0` corner: (extent x@(extent y // 2))].
		[#topRight] -> [(extent x // 2)@0 corner: (extent x@(extent y // 2))].
		[#left] -> [`0@0` corner: (extent x // 2)@extent y].
		[#full] -> [`0@0` corner: extent].
		[#right] -> [(extent x // 2)@0 corner: extent].
		[#bottomLeft] -> [0@(extent y // 2) corner: (extent x // 2)@extent y].
		[#bottomRight] -> [(extent x // 2)@(extent y // 2) corner: extent].
		[#bottom] -> [0@(extent y // 2) corner: extent].
	}
</details>

#### TileResizeMorph>>#action: aBlock

<details>
	<summary>See more</summary>
	
	action: aBlock
	action _ aBlock
</details>

#### TileResizeMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	super drawOn: aCanvas.
	
	selectedResize ifNil: [^ self].
	
	aCanvas fillRectangle: (self selectionRectangle: selectedResize) 
			color: selectionColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple
			baseColorForBorder: borderColor.
</details>

#### TileResizeMorph>>#selectResize: localEventPosition

<details>
	<summary>See more</summary>
	
	selectResize: localEventPosition

	| newResize |
	
	newResize _ self resizeAtPoint: localEventPosition.
	newResize ~= selectedResize ifTrue: [
		selectedResize _ newResize.
		self redrawNeeded]
</details>

#### TileResizeMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	self selectResize: localEventPosition
</details>

#### TileResizeMorph>>#selectionColor: aColor

<details>
	<summary>See more</summary>
	
	selectionColor: aColor

	selectionColor _ aColor
</details>

