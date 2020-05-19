## AutoCompleter

A TextMorph (instance of BareTextMorph) can have an autocompleter in the same way it might have a styler. My instances implement autocompletion.

### Methods
#### AutoCompleter>>#entryCount

<details>
	<summary>See more</summary>
	
	entryCount
	^ entries size
</details>

#### AutoCompleter>>#selectedEntry

<details>
	<summary>See more</summary>
	
	selectedEntry

	^self entries at: menuMorph selected
</details>

#### AutoCompleter>>#shouldOpenMorphWhenNoPrefixFor: currentChar

<details>
	<summary>See more</summary>
	
	shouldOpenMorphWhenNoPrefixFor: currentChar 
	
	^currentChar isAlphaNumeric 
		or: [ currentChar isRightBracket
		or: [ currentChar = $) 
		or: [ currentChar = $; ]]]
</details>

#### AutoCompleter>>#handleKeystrokeBefore: kbEvent

I return a boolean. true when I have handled the event and no futher processing is needed by the caller.


<details>
	<summary>See more</summary>
	
	handleKeystrokeBefore: kbEvent
	
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	
	| shouldOpenMorph |
	  
	shouldOpenMorph _ self shouldOpenMorph.
	
	"Stuff to do if the menu is not open"
	menuMorph ifNil: [ ^ self openCompletionMenuFor: kbEvent if: shouldOpenMorph ].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.
	
	kbEvent isEsc ifTrue: [ self closeMenu. ^ true].
	kbEvent isBackspace ifTrue: [ shouldOpenMorph ifFalse: [ self closeMenu ]. ^ false].
	kbEvent isHome ifTrue: [ menuMorph goHome. ^ true ].
	kbEvent isEnd ifTrue: [ menuMorph goToEnd. ^ true].
	kbEvent isQuesitonMark ifTrue: [ menuMorph help. ^true].
	kbEvent isArrowUp ifTrue: [ menuMorph goUp. ^ true].
	kbEvent isArrowDown ifTrue: [ menuMorph goDown. ^ true].
	kbEvent isPageUp ifTrue: [ menuMorph goPageUp. ^ true].
	kbEvent isPageDown ifTrue: [ menuMorph goPageDown. ^ true].
	
	self canShowSelectorDocumentation 
		ifTrue: [ 
			kbEvent isArrowRight ifTrue: [ menuMorph showSelectorDocumentation. ^ true ].
			kbEvent isArrowLeft ifTrue: [ menuMorph hideSelectorDocumentation. ^ true ]]
		ifFalse: [
			"If it is showing identifiers I eat the right arrow key because the user is used to it when
			showing selectors,  so to avoid an unexpected behavior I do nothing with it -Hernan"
			kbEvent isArrowRight ifTrue: [ ^ true ]].
					
	(self shouldInsertSelected: kbEvent) ifTrue: [ self insertSelected ifTrue: [^ true]].
	(self shouldCloseMenu: kbEvent) ifTrue: [ self closeMenu ]. 
	 
	^false
</details>

#### AutoCompleter>>#shouldOpenMorphWhenPrefixAt: currentPos and: currentChar

<details>
	<summary>See more</summary>
	
	shouldOpenMorphWhenPrefixAt: currentPos and: currentChar 
	
	^ model textSize >= currentPos and: [ currentChar isAlphaNumeric or: [ currentChar == $: ]] 


	
</details>

#### AutoCompleter>>#isPossibleInvalidEntry: anEntry

<details>
	<summary>See more</summary>
	
	isPossibleInvalidEntry: anEntry

	^false
</details>

#### AutoCompleter>>#canSelect: anEntry

<details>
	<summary>See more</summary>
	
	canSelect: anEntry

	^true
</details>

#### AutoCompleter>>#openCompletionMenuFor: kbEvent if: shouldOpenMorph

Ctrl-Space or Tab for open


<details>
	<summary>See more</summary>
	
	openCompletionMenuFor: kbEvent if: shouldOpenMorph

	"Ctrl-Space or Tab for open"
	"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"
	(kbEvent isCtrlSpace or: [self isTab: kbEvent and: shouldOpenMorph]) ifTrue: [ self openCompletionMenu. ^ true].
		
	"Auto-open - currently deactivated"
"	(ctrl not & cmd not & alphanum) ifTrue: [ self openCompletionMenu ]."
	
	^ false
</details>

#### AutoCompleter>>#shouldCloseMenu: kbEvent

All keys but the alphanumeric chars (without command and control ) and the backspace key do close the menu


<details>
	<summary>See more</summary>
	
	shouldCloseMenu: kbEvent

	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"

	^ ((kbEvent controlKeyPressed not and: [ kbEvent commandAltKeyPressed not ]) and: [ kbEvent isAlphaNumeric or: [ kbEvent isColon ]]) not

</details>

#### AutoCompleter>>#computeEntries

This default implementation might be redefined of needed.


<details>
	<summary>See more</summary>
	
	computeEntries
	"This default implementation might be redefined of needed."
	| prefixStart prefixStop string |
	string _ model actualContents string.
	prefixStop _ position.
	prefixStart _ position.
	[ prefixStart > 0 and: [ (string at: prefixStart) isSeparator not ]] whileTrue: [
		prefixStart _ prefixStart - 1 ].
	prefix _ string copyFrom: prefixStart+1 to: prefixStop.
	entries _ Array streamContents: [ :strm |
		self addEntriesTo: strm ]
</details>

#### AutoCompleter>>#closeMenu

<details>
	<summary>See more</summary>
	
	closeMenu
	menuMorph ifNotNil: [
		menuMorph delete.
		menuMorph _ nil ]
</details>

#### AutoCompleter>>#shouldInsertSelected: kbEvent

<details>
	<summary>See more</summary>
	
	shouldInsertSelected: kbEvent
	
	^ kbEvent isReturnKey 
		or: [ (kbEvent isSpace and: [ kbEvent controlKeyPressed or: [ kbEvent rawMacOptionKeyPressed ]]) 
		or: [ kbEvent isTab]]
</details>

#### AutoCompleter>>#isTab: kbEvent and: shouldOpenMoprh

<details>
	<summary>See more</summary>
	
	isTab: kbEvent and: shouldOpenMoprh
	
	^ self opensWithTab 
		and: [ kbEvent isTab 
		and: [ shouldOpenMoprh ]]
</details>

#### AutoCompleter>>#canShowSelectorDocumentation

<details>
	<summary>See more</summary>
	
	canShowSelectorDocumentation

	self subclassResponsibility
</details>

#### AutoCompleter>>#textMorph: aBareTextMorph

<details>
	<summary>See more</summary>
	
	textMorph: aBareTextMorph
	textMorph _ aBareTextMorph
</details>

#### AutoCompleter>>#shouldOpenMorph

<details>
	<summary>See more</summary>
	
	shouldOpenMorph

	| currentPos currentChar |
	
	textMorph editor hasSelection ifTrue: [ ^ false ].
	
	currentPos _ textMorph editor startIndex-1.
	currentPos <= 0 ifTrue: [ ^ false ].
	currentChar _ model actualContents at: currentPos.
	
	^ currentChar = Character space
		ifTrue: [ self shouldOpenMorphWhenNoPrefixAt: currentPos-1 ]
		ifFalse: [ self shouldOpenMorphWhenPrefixAt: currentPos and: currentChar ].
</details>

#### AutoCompleter>>#canSelectEntryAt: anIndex

<details>
	<summary>See more</summary>
	
	canSelectEntryAt: anIndex

	^self canSelect: (self entryAt: anIndex)
</details>

#### AutoCompleter>>#openCompletionMenu

<details>
	<summary>See more</summary>
	
	openCompletionMenu

	| theEditor |

	theEditor _ textMorph editor.
	position _ theEditor startIndex - 1.
	self closeMenu.
	self computeEntries.
	entries notEmpty
		ifTrue: [ | startIndex characterBlock cursorIndex |
			cursorIndex := theEditor pointIndex.
			startIndex := (theEditor text at: cursorIndex-1) = Character space 
				ifTrue: [ cursorIndex ]
				ifFalse: [ theEditor previousWordStart: (cursorIndex >  theEditor text size ifTrue: [ cursorIndex-1 ] ifFalse: [ cursorIndex ])].
			characterBlock := theEditor characterBlockForIndex: startIndex.
			menuMorph _  AutoCompleterMorph 
				completer: self
				position: characterBlock bottomLeft + textMorph morphPositionInWorld ].

</details>

#### AutoCompleter>>#opensWithTab

Returns wheter should open the auto completer when pressing Tab or not


<details>
	<summary>See more</summary>
	
	opensWithTab
	
	"Returns wheter should open the auto completer when pressing Tab or not"
	
	^false
</details>

#### AutoCompleter>>#selectedEntryFormatted

<details>
	<summary>See more</summary>
	
	selectedEntryFormatted

	^(self entries at: menuMorph selected), ' '
</details>

#### AutoCompleter>>#menuClosed

<details>
	<summary>See more</summary>
	
	menuClosed
	menuMorph _ nil
</details>

#### AutoCompleter>>#insertSelected

<details>
	<summary>See more</summary>
	
	insertSelected
	
	| entry editor selEnd str |
	
	textMorph ifNil: [ ^false ].
	
	entry _ self selectedEntryFormatted.
	(self canSelect: entry) ifFalse: [ ^true ].
	
	editor _ textMorph editor.
	str _ model actualContents string.
	selEnd _ position.
	[selEnd < str size and: [ (str at: selEnd+1) tokenish ]] whileTrue: [ selEnd _ selEnd + 1 ].
	(selEnd < str size and: [ (str at: selEnd+1) = $ ]) ifTrue: [ selEnd _ selEnd + 1].
	editor selectFrom: position-prefix size+1 to: selEnd.
	editor
		replaceSelectionWith: entry;
		deselectAndPlaceCursorAt: position - prefix size + 1 + (self newCursorPosition: entry).
	textMorph redrawNeeded.
	menuMorph delete.
	menuMorph _ nil.
	
	^ true
</details>

#### AutoCompleter>>#handleKeystrokeAfter: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	handleKeystrokeAfter: aKeyboardEvent
	| newPos |
	menuMorph ifNil: [^self].
	newPos _ textMorph editor startIndex-1.
	newPos = position ifTrue: [^self].
	newPos < position
		ifTrue: [
			prefix _ prefix copyFrom: 1 to: prefix size+(newPos-position).
			position _ newPos ]
		ifFalse: [
			position _ position + 1.
			prefix _ prefix copyWith: (model actualContents at: position) ].
	self computeEntries.
	entries notEmpty
		ifTrue: [ menuMorph resetMenu ]
		ifFalse: [ self closeMenu ]
</details>

#### AutoCompleter>>#setModel: aTextModel

<details>
	<summary>See more</summary>
	
	setModel: aTextModel
	model _ aTextModel
</details>

#### AutoCompleter>>#textMorph

<details>
	<summary>See more</summary>
	
	textMorph

	^textMorph
</details>

#### AutoCompleter>>#shouldOpenMorphWhenNoPrefixAt: currentPos

<details>
	<summary>See more</summary>
	
	shouldOpenMorphWhenNoPrefixAt: currentPos 
	
	^  model textSize >= currentPos 
		and: [ currentPos > 0
		and: [ self shouldOpenMorphWhenNoPrefixFor: (model actualContents at: currentPos) ]]
</details>

#### AutoCompleter>>#entries

<details>
	<summary>See more</summary>
	
	entries
	^entries
</details>

#### AutoCompleter>>#entryAt: anIndex

<details>
	<summary>See more</summary>
	
	entryAt: anIndex
	
	^self entries at: anIndex
</details>

#### AutoCompleter>>#autoCompletionAround: aBlock keyStroke: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	autoCompletionAround: aBlock keyStroke: aKeyboardEvent

	(self handleKeystrokeBefore: aKeyboardEvent)
		ifTrue: [^ self].
	aBlock value.
	"Narrow the completion with any of the keys"
	self handleKeystrokeAfter: aKeyboardEvent
</details>

#### AutoCompleter>>#newCursorPosition: anEntry

<details>
	<summary>See more</summary>
	
	newCursorPosition: anEntry
	^anEntry size
</details>

## AutoCompleterMorph

I show the possible completions in a menu like appearance. The user may choose an entry from my list and complete the word he was typing in the editor. I'm showed with the Tab key and will be deleted when with ESC key or when a successful completion occurs. The following keystrokes are supported: Ctrl-Space or Tab: Open a new morph. Tab requires at least one character in front of the cursor. When already open complete the selected entry. Esc: Close me Arrow Up: Move one entry up. Arrow Down: Move one entry down Return: (like Ctrl-Space and Tab): Complete with the selected item and close the morph any letter or digit: Narrow the completion further

### Methods
#### AutoCompleterMorph>>#help

<details>
	<summary>See more</summary>
	
	help

	TextModel new contents: AutoCompleter helpText; openLabel: 'uCompletion Keyboard Help'
</details>

#### AutoCompleterMorph>>#previousSelectableEntryIndexFrom: anIndex

<details>
	<summary>See more</summary>
	
	previousSelectableEntryIndexFrom: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: false
</details>

#### AutoCompleterMorph>>#goPageUp

<details>
	<summary>See more</summary>
	
	goPageUp
	
	| oldEntry newEntry nextEntry |
	
	oldEntry _ self selected.
	newEntry _ oldEntry.
	[nextEntry _ self previousSelectableEntryIndexFrom: newEntry.
	oldEntry > nextEntry and: [oldEntry - nextEntry <= self itemsPerPage]]
		whileTrue: [newEntry _ nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness

<details>
	<summary>See more</summary>
	
	drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness
	
	| height top bottom |

	height _ extent y - (2 * scrollbarThickness).
	top _ (1.0 * self firstVisible-1 / self entryCount * height) ceiling + 1 + scrollbarThickness-1.
	bottom _ (1.0 * self lastVisible / self entryCount * height) floor + 1 + scrollbarThickness -1.
	aCanvas
		fillRectangle: (extent x - scrollbarThickness+2@top corner:  extent x-2 @ bottom)
		color: `Color veryLightGray` 
</details>

#### AutoCompleterMorph>>#stillActive

<details>
	<summary>See more</summary>
	
	stillActive
	lastActivity := Time localMillisecondClock
</details>

#### AutoCompleterMorph>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete

	selectorDocumentation ifNotNil: [ 
		selectorDocumentation delete.
		selectorDocumentation := nil ].
	
	^super delete 
</details>

#### AutoCompleterMorph>>#lastSelectableEntryIndex

<details>
	<summary>See more</summary>
	
	lastSelectableEntryIndex
	
	^self previousSelectableEntryIndexFrom: 1
</details>

#### AutoCompleterMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ Theme current paneBackgroundFrom: self defaultBorderColor
</details>

#### AutoCompleterMorph>>#goToEnd

<details>
	<summary>See more</summary>
	
	goToEnd
	
	self selected: self lastSelectableEntryIndex.
	self lastVisible: self selected.
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#stepTime

Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second.


<details>
	<summary>See more</summary>
	
	stepTime 
	^ 100
</details>

#### AutoCompleterMorph>>#updateColor

<details>
	<summary>See more</summary>
	
	updateColor

	| remaining alpha |

	remaining := (self timeout - self timeOfLastActivity).
	remaining < 1000 
		ifTrue: [
			alpha _ remaining / 1000.0.
			self color: (self color alpha: alpha).
			self borderColor: (borderColor alpha: alpha) ]
		ifFalse: [ self setDefaultColors ]
		
			
</details>

#### AutoCompleterMorph>>#drawScrollBarOn: aCanvas

<details>
	<summary>See more</summary>
	
	drawScrollBarOn: aCanvas

	| scrollbarThickness width |

	width _ extent x-2.
	self entryCount > self itemsPerPage  ifTrue: [
		scrollbarThickness _ ScrollBar scrollbarThickness.
		width _ width - scrollbarThickness.

		self drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness.
		self drawUpArrowOn: aCanvas thickness: scrollbarThickness.
		self drawDownArrowOn: aCanvas thickness: scrollbarThickness.
		self drawScrollBarMovementBarOn: aCanvas thickness: scrollbarThickness ].

	^width

</details>

#### AutoCompleterMorph>>#wantsSteps

Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME. Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!) don't need to answer true to this message"

	^true
</details>

#### AutoCompleterMorph>>#calculateWidth

<details>
	<summary>See more</summary>
	
	calculateWidth

	| width font |

	width _ 120.
	font _ self class listFont.

	1
		to: self entryCount
		do: [ :index | width _ width max: (font widthOfString: (completer entries at: index) asString)].

	self entryCount > self itemsPerPage ifTrue: [ width _ width + ScrollBar scrollbarThickness ].

	^ width 
</details>

#### AutoCompleterMorph>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse button 1 up event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self morphContainsPoint: localEventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goUp ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ ^self stillActive; goDown ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]
</details>

#### AutoCompleterMorph>>#selectorDocumentationTextForAllI: aMethodsCollection

<details>
	<summary>See more</summary>
	
	selectorDocumentationTextForAllI: aMethodsCollection

	| selectorDocumentationText implementors methodDocumentationSeparator |
	
	selectorDocumentationText := Text new.
	methodDocumentationSeparator := self methodDocumentationSeparator.		
	implementors := aMethodsCollection asSortedCollection: [ :leftMethod :rightMethod | leftMethod methodClass classDepth <  rightMethod methodClass classDepth ].
	
	implementors 
		do: [ :implementor | selectorDocumentationText := selectorDocumentationText append: (completer documentationOf: implementor)]
		separatedBy: [ selectorDocumentationText := selectorDocumentationText append: methodDocumentationSeparator ].
		
	^ selectorDocumentationText
</details>

#### AutoCompleterMorph>>#drawUpArrowOn: aCanvas thickness: scrollbarThickness

<details>
	<summary>See more</summary>
	
	drawUpArrowOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		image: (BitBltCanvas arrowOfDirection: #up size: scrollbarThickness)
		at: self upButtonPosition.

</details>

#### AutoCompleterMorph>>#selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses

<details>
	<summary>See more</summary>
	
	selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses
	
	| methodsToShow |
	
	methodsToShow := selectorsClasses 
		inject: IdentitySet new
		into: [ :methods :aClass | 
			(aClass lookupSelector: selectedEntry) ifNotNil: [ :method | methods add: method ].
			methods ].
			
	^self selectorDocumentationTextForAllI: methodsToShow 
</details>

#### AutoCompleterMorph>>#selectorDocumentationText

<details>
	<summary>See more</summary>
	
	selectorDocumentationText

	| selectedEntry selectorsClasses |
	
	selectedEntry := completer selectedEntry.
	(completer isCategoryEntry: selectedEntry) ifTrue: [ ^'' ].
	selectedEntry := selectedEntry asSymbol.
	selectorsClasses := completer selectorsClasses.
	 
	^ selectorsClasses isEmpty 
		ifTrue: [ self selectorDocumentationTextForAllImplementorsOf: selectedEntry ]
		ifFalse: [ self selectorDocumentationTextOf: selectedEntry forAll: selectorsClasses ].
	
</details>

#### AutoCompleterMorph>>#firstSelectableEntryIndex

<details>
	<summary>See more</summary>
	
	firstSelectableEntryIndex
	
	^self nextSelectableEntryIndexFrom: 0
</details>

#### AutoCompleterMorph>>#timeout

<details>
	<summary>See more</summary>
	
	timeout
	^ 5000
</details>

#### AutoCompleterMorph>>#goDown

<details>
	<summary>See more</summary>
	
	goDown
	
	self selected: (self nextSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self lastVisible: self selected].
	
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#timeOfLastActivity

<details>
	<summary>See more</summary>
	
	timeOfLastActivity
	^ (Time localMillisecondClock - self lastActivity)
</details>

#### AutoCompleterMorph>>#nextSelectableEntryIndexFromAndIncluding: anIndex

<details>
	<summary>See more</summary>
	
	nextSelectableEntryIndexFromAndIncluding: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex - 1
</details>

#### AutoCompleterMorph>>#mouseEnter: evt

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: evt
	self activeHand newKeyboardFocus: completer textMorph.
	^ super mouseEnter: evt
</details>

#### AutoCompleterMorph>>#crPressedOnSelectorDocumentation

<details>
	<summary>See more</summary>
	
	crPressedOnSelectorDocumentation
	
	self hideSelectorDocumentation. 
	self activeHand newKeyboardFocus: completer textMorph 
</details>

#### AutoCompleterMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent

	^ true
</details>

#### AutoCompleterMorph>>#setDefaultColors

<details>
	<summary>See more</summary>
	
	setDefaultColors

	self color: self defaultColor.
	self borderColor: self defaultBorderColor 
</details>

#### AutoCompleterMorph>>#calculateItemsPerPage

<details>
	<summary>See more</summary>
	
	calculateItemsPerPage

	| itemsThatCanFit |

	itemsThatCanFit _ (DisplayScreen actualScreenSize y - originalPosition y - 2) // self itemHeight.
	itemsPerPage _ (itemsThatCanFit min: self maxItemsPerPage) min: self entryCount.


</details>

#### AutoCompleterMorph>>#goUp

<details>
	<summary>See more</summary>
	
	goUp

	(self selected = 0 and: [self firstVisible = 1]) ifTrue: [^ self].
	
	self selected: (self previousSelectableEntryIndexFrom: self selected).
	(self selected between: self firstVisible and: self lastVisible)
		ifFalse: [self firstVisible: self selected].
		
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#itemsPerPage

<details>
	<summary>See more</summary>
	
	itemsPerPage

	^itemsPerPage
</details>

#### AutoCompleterMorph>>#firstVisible: anIndex

<details>
	<summary>See more</summary>
	
	firstVisible: anIndex
	
	firstVisible _ anIndex
		min: self entryCount - self itemsPerPage + 1
		max: 1.
</details>

#### AutoCompleterMorph>>#isXOutOfScreen: aLocation with: anExtent

<details>
	<summary>See more</summary>
	
	isXOutOfScreen: aLocation with: anExtent
	
	^aLocation x + anExtent x > DisplayScreen actualScreenSize x
</details>

#### AutoCompleterMorph>>#itemHeight

cached to minimise recalculation


<details>
	<summary>See more</summary>
	
	itemHeight
	"cached to minimise recalculation"
	^ itemHeight ifNil: [itemHeight := self class itemHeight]
</details>

#### AutoCompleterMorph>>#selectorDocumentation

<details>
	<summary>See more</summary>
	
	selectorDocumentation

	selectorDocumentation ifNil: [ self initializeSelectorDocumentation ].
	^selectorDocumentation 
</details>

#### AutoCompleterMorph>>#goHome

<details>
	<summary>See more</summary>
	
	goHome

	self selected: self firstSelectableEntryIndex.
	self firstVisible: 1.
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#drawContainingRectangle: aCanvas

<details>
	<summary>See more</summary>
	
	drawContainingRectangle: aCanvas
	
	aCanvas frameAndFillRectangle: self morphLocalBounds fillColor: self color borderWidth: borderWidth borderColor: borderColor.

</details>

#### AutoCompleterMorph>>#selectorDefaultDocumentationLocation

<details>
	<summary>See more</summary>
	
	selectorDefaultDocumentationLocation

	| relativeSelected |

	relativeSelected := (self selected - self firstVisible) min: self itemsPerPage - 1 max: 0.

	^location externalizePosition: extent x@(relativeSelected * self itemHeight + 1).

	
</details>

#### AutoCompleterMorph>>#setCompleter: anAutoCompleter position: aPoint

<details>
	<summary>See more</summary>
	
	setCompleter: anAutoCompleter position: aPoint 

	completer _ anAutoCompleter.
	
	originalPosition := aPoint.
	
	self resetMenu.
	self openInWorld
</details>

#### AutoCompleterMorph>>#downButtonPosition

<details>
	<summary>See more</summary>
	
	downButtonPosition

	^ `0@0` + (extent - ScrollBar scrollbarThickness)
</details>

#### AutoCompleterMorph>>#lastVisible

<details>
	<summary>See more</summary>
	
	lastVisible
	
	^self firstVisible + self itemsPerPage - 1 min: self entryCount
</details>

#### AutoCompleterMorph>>#initializeSelectorDocumentation

<details>
	<summary>See more</summary>
	
	initializeSelectorDocumentation 
 
	selectorDocumentation := TextModelMorph withText: ''.
	selectorDocumentation textMorph
		acceptOnCR: false;
		crAction: [ self crPressedOnSelectorDocumentation ].

	selectorDocumentation
		wrapFlag: true;
		borderColor: Color black;
		borderWidth: 1;
		disableEditing;
		openInWorld 
	
</details>

#### AutoCompleterMorph>>#entryCount

<details>
	<summary>See more</summary>
	
	entryCount
	
	^completer entryCount
</details>

#### AutoCompleterMorph>>#stepAt: millisecondSinceLast

Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The desired time between steps is specified by this morph's answer to the stepTime message. The millisecondSinceLast parameter gives the time elapsed since the previous step.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	self isShowingSelectorDocumentation ifTrue: [ ^self ].
	
	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]
</details>

#### AutoCompleterMorph>>#lastVisible: anIndex

<details>
	<summary>See more</summary>
	
	lastVisible: anIndex
	
	self firstVisible: anIndex - self itemsPerPage + 1.
</details>

#### AutoCompleterMorph>>#adjust: aLocation ifOutOfScreenWith: anExtent xOffset: xOffset yOffset: yOffset

<details>
	<summary>See more</summary>
	
	adjust: aLocation ifOutOfScreenWith: anExtent xOffset: xOffset yOffset: yOffset

	| adjustedLocationX adjustedLocationY |

	adjustedLocationX := (self isXOutOfScreen: aLocation with: anExtent)
		ifTrue: [ aLocation x - anExtent x - xOffset ]
		ifFalse: [ aLocation x ].

	adjustedLocationX < 0 ifTrue: [ adjustedLocationX := aLocation x ].

	adjustedLocationY := (self isYOutOfScreen: aLocation with: anExtent)
		ifTrue: [ aLocation y - anExtent y - yOffset ]
		ifFalse: [ aLocation y ].

	^adjustedLocationX @ adjustedLocationY
	
</details>

#### AutoCompleterMorph>>#wrapIndex: anIndex by: aSize

<details>
	<summary>See more</summary>
	
	wrapIndex: anIndex by: aSize
	
	^anIndex - 1 \\ aSize + 1
</details>

#### AutoCompleterMorph>>#selected

Answer the value of selected


<details>
	<summary>See more</summary>
	
	selected
	"Answer the value of selected"
	selected ifNil: [ self selected: self firstVisible ].
	^ selected
</details>

#### AutoCompleterMorph>>#selected: aNumber

Set the value of selected


<details>
	<summary>See more</summary>
	
	selected: aNumber 

	"Set the value of selected"

	((aNumber between: 1 and: self entryCount) and: [ aNumber ~= selected ])
		ifTrue: [ 
			selected _ aNumber.
			self isShowingSelectorDocumentation ifTrue: [ self showSelectorDocumentation ]]
</details>

#### AutoCompleterMorph>>#goPageDown

<details>
	<summary>See more</summary>
	
	goPageDown
	
	| oldEntry newEntry nextEntry |
	
	oldEntry _ self selected.
	newEntry _ oldEntry.
	[nextEntry _ self nextSelectableEntryIndexFrom: newEntry.
	nextEntry > oldEntry and: [nextEntry - oldEntry <= self itemsPerPage]]
		whileTrue: [newEntry _ nextEntry].
	
	self selected: newEntry.
	self firstVisible: newEntry.
	
	self redrawNeeded.
</details>

#### AutoCompleterMorph>>#drawItemsOn: aCanvas width: width

<details>
	<summary>See more</summary>
	
	drawItemsOn: aCanvas width: width

	| itemTop |

	itemTop _ 1.
	self firstVisible
		to: self lastVisible
		do: [ :index |
			self drawItemOf: index on: aCanvas width: width top: itemTop.
			itemTop _ itemTop + self itemHeight ].
</details>

#### AutoCompleterMorph>>#colorOf: entry

<details>
	<summary>See more</summary>
	
	colorOf: entry

	^(completer isPossibleInvalidEntry: entry) 
		ifTrue: [ `Color blue` ] 
		ifFalse: [ Theme current text ]
	
</details>

#### AutoCompleterMorph>>#selectorDocumentationExtent

<details>
	<summary>See more</summary>
	
	selectorDocumentationExtent

	^`600@250`
</details>

#### AutoCompleterMorph>>#isShowingSelectorDocumentation

<details>
	<summary>See more</summary>
	
	isShowingSelectorDocumentation

	^selectorDocumentation notNil and: [ selectorDocumentation visible ]
</details>

#### AutoCompleterMorph>>#methodDocumentationSeparator

<details>
	<summary>See more</summary>
	
	methodDocumentationSeparator
	
	^ Text 
		string: String newLineString, '------------------------------------------------------------------------------------------------', String newLineString 
		attribute: TextColor black.
</details>

#### AutoCompleterMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1
</details>

#### AutoCompleterMorph>>#resetMenu

<details>
	<summary>See more</summary>
	
	resetMenu

	| width newExtent |
	self hideSelectorDocumentation.

	self calculateItemsPerPage.
	
	self firstVisible: 1.
	self selected: self firstSelectableEntryIndex.
	
	width _ self calculateWidth.
	newExtent _ width + 4 @ (self itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded 
</details>

#### AutoCompleterMorph>>#drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness

<details>
	<summary>See more</summary>
	
	drawScrollBarRectangleOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		frameRectangle: (extent x - scrollbarThickness@0 extent: scrollbarThickness @ extent y)
		borderWidth: 1
		color: borderColor.

</details>

#### AutoCompleterMorph>>#maxItemsPerPage

<details>
	<summary>See more</summary>
	
	maxItemsPerPage

	^13
</details>

#### AutoCompleterMorph>>#canSelectEntryAt: anIndex

<details>
	<summary>See more</summary>
	
	canSelectEntryAt: anIndex
	
	^completer canSelectEntryAt: anIndex
</details>

#### AutoCompleterMorph>>#firstVisible

<details>
	<summary>See more</summary>
	
	firstVisible

	^firstVisible min: self entryCount
</details>

#### AutoCompleterMorph>>#isYOutOfScreen: aLocation with: anExtent

<details>
	<summary>See more</summary>
	
	isYOutOfScreen: aLocation with: anExtent
	
	^aLocation y + anExtent y > DisplayScreen actualScreenSize y
</details>

#### AutoCompleterMorph>>#nextSelectableEntryIndexFrom: anIndex goingForwards: goingForwardsBoolean

<details>
	<summary>See more</summary>
	
	nextSelectableEntryIndexFrom: anIndex goingForwards: goingForwardsBoolean
	
	| direction indicesFromAnIndex |
	
	direction _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	indicesFromAnIndex _ (1 to: self entryCount)
		collect: [ :offset | self wrapIndex: anIndex + (offset*direction) by: self entryCount ].
	
	^indicesFromAnIndex
		detect: [ :index | self canSelectEntryAt: index ]
		ifNone: [self error: 'there are no selectable entries']
</details>

#### AutoCompleterMorph>>#showSelectorDocumentation

<details>
	<summary>See more</summary>
	
	showSelectorDocumentation

	| selectorDocumentationLocation selectorDocumentationExtent |
	
	selectorDocumentationLocation := self selectorDefaultDocumentationLocation.
	selectorDocumentationExtent := self selectorDocumentationExtent.
	selectorDocumentationLocation := self adjust: selectorDocumentationLocation ifOutOfScreenWith: selectorDocumentationExtent xOffset: extent x yOffset: self itemHeight negated.
	
	self setDefaultColors.
	
	self selectorDocumentation 
		model: (TextModel withText: self selectorDocumentationText);
		morphPosition: selectorDocumentationLocation extent: selectorDocumentationExtent;
		wrapFlag: false;
		show.
		
	
</details>

#### AutoCompleterMorph>>#upButtonPosition

<details>
	<summary>See more</summary>
	
	upButtonPosition

	^extent x - ScrollBar scrollbarThickness@0
</details>

#### AutoCompleterMorph>>#drawDownArrowOn: aCanvas thickness: scrollbarThickness

<details>
	<summary>See more</summary>
	
	drawDownArrowOn: aCanvas thickness: scrollbarThickness
	
	aCanvas
		image: (BitBltCanvas arrowOfDirection: #down size: scrollbarThickness)
		at: self downButtonPosition.

</details>

#### AutoCompleterMorph>>#drawItemOf: index on: aCanvas width: width top: itemTop

<details>
	<summary>See more</summary>
	
	drawItemOf: index on: aCanvas width: width top: itemTop

	| rectangle entry |
	
	rectangle _ 1@itemTop extent: width@self itemHeight.
	index = self selected ifTrue: [ aCanvas fillRectangle: rectangle color: (Theme current listHighlightFocused: true) ].

	entry _ completer entries at: index.
	aCanvas
		drawString: entry asString
		at: rectangle topLeft
		font: self class listFont
		color: (self colorOf: entry).
	
</details>

#### AutoCompleterMorph>>#selectorDocumentationTextForAllImplementorsOf: selectedEntry

<details>
	<summary>See more</summary>
	
	selectorDocumentationTextForAllImplementorsOf: selectedEntry

	^ self selectorDocumentationTextForAllI: (Smalltalk allImplementorsOf: selectedEntry).

	
</details>

#### AutoCompleterMorph>>#hideSelectorDocumentation

<details>
	<summary>See more</summary>
	
	hideSelectorDocumentation

	selectorDocumentation ifNotNil: [ selectorDocumentation hide ].
	self stillActive 
</details>

#### AutoCompleterMorph>>#nextSelectableEntryIndexFrom: anIndex

<details>
	<summary>See more</summary>
	
	nextSelectableEntryIndexFrom: anIndex
	
	^self nextSelectableEntryIndexFrom: anIndex goingForwards: true
</details>

#### AutoCompleterMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	
	| width |
	
	self drawContainingRectangle: aCanvas.
	width _ self drawScrollBarOn: aCanvas.
	self drawItemsOn: aCanvas width: width 

</details>

#### AutoCompleterMorph>>#lastActivity

<details>
	<summary>See more</summary>
	
	lastActivity 
	lastActivity ifNil: [self stillActive].
	^ lastActivity 
</details>

#### AutoCompleterMorph>>#defaultBorderColor

answer the default border color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderColor
	^ `Color gray`
</details>

#### AutoCompleterMorph>>#handlesMouseOver: evt

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?


<details>
	<summary>See more</summary>
	
	handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^true
</details>

## AutoCompleterSelectorsCollector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### AutoCompleterSelectorsCollector>>#selectKeywordSelectorsWhile: aClosure

<details>
	<summary>See more</summary>
	
	selectKeywordSelectorsWhile: aClosure 

	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword ] while: aClosure 

</details>

#### AutoCompleterSelectorsCollector>>#addSelectorsOfAll: classes upTo: aSuperclass

<details>
	<summary>See more</summary>
	
	addSelectorsOfAll: classes upTo: aSuperclass

	classes do: [ :aClass | 
		otherClasses := classes copyWithout: aClass.
		self addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclass ]
</details>

#### AutoCompleterSelectorsCollector>>#selectSelectorsThatSatisfy: aSelectorsSelectionCondition while: aClosure

<details>
	<summary>See more</summary>
	
	selectSelectorsThatSatisfy: aSelectorsSelectionCondition while: aClosure
	
	| currentSelectorsSelectionCondition |
	
	currentSelectorsSelectionCondition := selectorSelectionCondition.
	[ selectorSelectionCondition := aSelectorsSelectionCondition.
	aClosure value ] ensure: [ selectorSelectionCondition := currentSelectorsSelectionCondition ].
</details>

#### AutoCompleterSelectorsCollector>>#addKeywordSelectorsOfAll: classes upTo: aSuperclass

<details>
	<summary>See more</summary>
	
	addKeywordSelectorsOfAll: classes upTo: aSuperclass

	self selectKeywordSelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclass ]
</details>

#### AutoCompleterSelectorsCollector>>#addLeftCategories

<details>
	<summary>See more</summary>
	
	addLeftCategories
	
	categoriesWithSelectors keysAndValuesDo: [ :aCategory :selectors | self addCategory: aCategory with: selectors ].
	
</details>

#### AutoCompleterSelectorsCollector>>#hasReachSelectorsLimit

<details>
	<summary>See more</summary>
	
	hasReachSelectorsLimit
	
	^addedSelectorsFastSet size >= selectorsLimit
</details>

#### AutoCompleterSelectorsCollector>>#categoryEntryFor: aCategory

<details>
	<summary>See more</summary>
	
	categoryEntryFor: aCategory
	
	^self class categoryEntryHeader, aCategory 
</details>

#### AutoCompleterSelectorsCollector>>#prioritizedCategories

<details>
	<summary>See more</summary>
	
	prioritizedCategories 
	
	^{Categorizer instanceCreation}
</details>

#### AutoCompleterSelectorsCollector>>#addUnaryAndBinarySelectorsOf: aClass

<details>
	<summary>See more</summary>
	
	addUnaryAndBinarySelectorsOf: aClass 
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass ].
	
</details>

#### AutoCompleterSelectorsCollector>>#addKeywordSelectorsOf: aClass upTo: aSuperclassToExclude

<details>
	<summary>See more</summary>
	
	addKeywordSelectorsOf: aClass upTo: aSuperclassToExclude

	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ]
</details>

#### AutoCompleterSelectorsCollector>>#selectUnaryAndBinarySelectorsWhile: aClosure

<details>
	<summary>See more</summary>
	
	selectUnaryAndBinarySelectorsWhile: aClosure
	
	self selectSelectorsThatSatisfy: [ :aSelector | aSelector isKeyword not ] while: aClosure 

</details>

#### AutoCompleterSelectorsCollector>>#addCategory: aCategory with: selectors

<details>
	<summary>See more</summary>
	
	addCategory: aCategory with: selectors 

	entriesToShow 
		add: (self categoryEntryFor: aCategory); 
		addAll: selectors
	   
</details>

#### AutoCompleterSelectorsCollector>>#initializeFor: aPrefix withSelectorsLimitedTo: aLimit

<details>
	<summary>See more</summary>
	
	initializeFor: aPrefix withSelectorsLimitedTo: aLimit 

	prefix := aPrefix.
	categoriesWithSelectors := OrderedDictionary new.
	addedSelectorsFastSet := IdentitySet new.
	possibleInvalidSelectors := IdentitySet new.
	selectorsLimit := aLimit.
	selectorSelectionCondition := [ :aSelector | true ]
</details>

#### AutoCompleterSelectorsCollector>>#addCategoriesAndSelectorsOf: aClass

<details>
	<summary>See more</summary>
	
	addCategoriesAndSelectorsOf: aClass

	| classOrganization |
	
	classOrganization := aClass organization.
	
	classOrganization categories do: [ :aCategory | 
		self hasReachSelectorsLimit ifTrue: [ ^ self ].
		self addCategory: aCategory of: classOrganization ]
</details>

#### AutoCompleterSelectorsCollector>>#entriesToShow

<details>
	<summary>See more</summary>
	
	entriesToShow
	
	self hasCollectedOnlyOneSelector
		ifTrue: [ entriesToShow := categoriesWithSelectors anyOne ]
		ifFalse: [ 
			entriesToShow := OrderedCollection new.
			self 
				addPrioritizedCategories;
				addLeftCategories ].
						  			
	^entriesToShow 
	
	   
</details>

#### AutoCompleterSelectorsCollector>>#hasCollectedOnlyOneSelector

<details>
	<summary>See more</summary>
	
	hasCollectedOnlyOneSelector
	
	^ categoriesWithSelectors size = 1 and: [ categoriesWithSelectors anyOne size = 1 ]
</details>

#### AutoCompleterSelectorsCollector>>#addPrioritizedCategories

<details>
	<summary>See more</summary>
	
	addPrioritizedCategories

	self prioritizedCategories do: [ :aCategory | self addPrioritizedCategory: aCategory ]
</details>

#### AutoCompleterSelectorsCollector>>#addedSelectors: selectors

<details>
	<summary>See more</summary>
	
	addedSelectors: selectors

	addedSelectorsFastSet addAll: selectors 
</details>

#### AutoCompleterSelectorsCollector>>#addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclassToExclude

<details>
	<summary>See more</summary>
	
	addSelectorsMarkingPossibleInvalidOnesOf: aClass upTo: aSuperclassToExclude

	| currentClass |
	
	currentClass := aClass.
	
	[ currentClass ~= aSuperclassToExclude and: [ currentClass notNil ] and: [ self hasReachSelectorsLimit not ] ] whileTrue: [ 
		self addCategoriesAndSelectorsOf: currentClass.
		currentClass := currentClass superclass].
	 

</details>

#### AutoCompleterSelectorsCollector>>#addPrioritizedCategory: aCategory

<details>
	<summary>See more</summary>
	
	addPrioritizedCategory: aCategory

	categoriesWithSelectors 
		at: aCategory 
		ifPresent: [ :selectors | 
			self addCategory: aCategory with: selectors.
			categoriesWithSelectors removeKey: aCategory ].

</details>

#### AutoCompleterSelectorsCollector>>#addUnaryAndBinarySelectorsOfAll: classes upTo: aSuperclassToExclude

<details>
	<summary>See more</summary>
	
	addUnaryAndBinarySelectorsOfAll: classes upTo: aSuperclassToExclude
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOfAll: classes upTo: aSuperclassToExclude ].
	
</details>

#### AutoCompleterSelectorsCollector>>#addSelectorsOf: aClass upTo: aSuperclassToExclude

<details>
	<summary>See more</summary>
	
	addSelectorsOf: aClass upTo: aSuperclassToExclude

	self addSelectorsOfAll: (Array with: aClass) upTo: aSuperclassToExclude 
</details>

#### AutoCompleterSelectorsCollector>>#prefixedSelectorsOf: aCategory in: aClassOrganization

<details>
	<summary>See more</summary>
	
	prefixedSelectorsOf: aCategory in: aClassOrganization
	
	^ (aClassOrganization listAtCategoryNamed: aCategory) 
		select: [ :aSelector |  
			(aSelector beginsWith: prefix) 
				and: [ (selectorSelectionCondition value: aSelector) 
				and: [ (addedSelectorsFastSet includes: aSelector) not ]]].

</details>

#### AutoCompleterSelectorsCollector>>#addCategory: aCategory of: classOrganization

<details>
	<summary>See more</summary>
	
	addCategory: aCategory of: classOrganization

	| categorySelectors selectedSelectors |

	selectedSelectors := self prefixedSelectorsOf: aCategory in: classOrganization.
	selectedSelectors isEmpty ifFalse: [ 
		categorySelectors := categoriesWithSelectors at: aCategory ifAbsentPut: [ OrderedCollection new ].
		categorySelectors addAll: selectedSelectors.
		self addedSelectors: selectedSelectors.
		self addToPossibleInvalidIfCorrespond: selectedSelectors ]
</details>

#### AutoCompleterSelectorsCollector>>#addKeywordSelectorsOf: aClass

<details>
	<summary>See more</summary>
	
	addKeywordSelectorsOf: aClass 
	
	self selectKeywordSelectorsWhile: [ self addSelectorsOf: aClass ]
</details>

#### AutoCompleterSelectorsCollector>>#addToPossibleInvalidIfCorrespond: selectedSelectors

<details>
	<summary>See more</summary>
	
	addToPossibleInvalidIfCorrespond: selectedSelectors

	selectedSelectors do: [ :aSelector |
		(otherClasses allSatisfy: [ :otherClass | otherClass canUnderstand: aSelector ]) ifFalse: [ possibleInvalidSelectors add: aSelector ]]
</details>

#### AutoCompleterSelectorsCollector>>#possibleInvalidSelectors

<details>
	<summary>See more</summary>
	
	possibleInvalidSelectors
	
	^possibleInvalidSelectors
</details>

#### AutoCompleterSelectorsCollector>>#addSelectorsOf: aClass

<details>
	<summary>See more</summary>
	
	addSelectorsOf: aClass

	self addSelectorsOf: aClass upTo: nil
</details>

#### AutoCompleterSelectorsCollector>>#addUnaryAndBinarySelectorsOf: aClass upTo: aSuperclassToExclude

<details>
	<summary>See more</summary>
	
	addUnaryAndBinarySelectorsOf: aClass upTo: aSuperclassToExclude
	
	self selectUnaryAndBinarySelectorsWhile: [ self addSelectorsOf: aClass upTo: aSuperclassToExclude ].
	
</details>

## ClassNameCompleter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ClassNameCompleter>>#opensWithTab

Returns wheter should open the auto completer when pressing Tab or not


<details>
	<summary>See more</summary>
	
	opensWithTab

	^true
</details>

#### ClassNameCompleter>>#computeEntries

This default implementation might be redefined of needed.


<details>
	<summary>See more</summary>
	
	computeEntries

	prefix _ model actualContents string.
	entries _ (Smalltalk classNames select: [ :aClassName | aClassName beginsWith: prefix ]) sort.
	
</details>

#### ClassNameCompleter>>#canShowSelectorDocumentation

<details>
	<summary>See more</summary>
	
	canShowSelectorDocumentation

	^false
</details>

## DynamicTypingSmalltalkCompleter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DynamicTypingSmalltalkCompleter>>#documentationOf: aMethod

<details>
	<summary>See more</summary>
	
	documentationOf: aMethod

	^aMethod dynamicTypingAutoCompleterDocumentation
</details>

#### DynamicTypingSmalltalkCompleter>>#computeMessageEntriesOfEnclosedExpressionReturnAt: aRange

<details>
	<summary>See more</summary>
	
	computeMessageEntriesOfEnclosedExpressionReturnAt: aRange   

	self computeMessageEntriesForUnknowClass 
	

</details>

#### DynamicTypingSmalltalkCompleter>>#computeMessageEntriesOfCascadeReceiverAt: aRange

<details>
	<summary>See more</summary>
	
	computeMessageEntriesOfCascadeReceiverAt: aRange   
	
	self computeMessageEntriesForUnknowClass
	

</details>

#### DynamicTypingSmalltalkCompleter>>#computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange

<details>
	<summary>See more</summary>
	
	computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange   
	
	self computeMessageEntriesForUnknowClass 
	
</details>

## SmalltalkCompleter

An autocompleter specialized in Smalltalk code

### Methods
#### SmalltalkCompleter>>#lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate

<details>
	<summary>See more</summary>
	
	lookForBinarySelectorAfter: aStopToken startingAt: anIndex with: aCandidate

	| currentRange currentIndex |
	
	currentIndex := anIndex.
	[ currentRange := allRanges at: currentIndex.
	currentRange rangeType ~= aStopToken and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex > 1
		ifTrue: [ allRanges at: currentIndex - 1 ]
		ifFalse: [ aCandidate ].

</details>

#### SmalltalkCompleter>>#canComputeMessageEntriesFor: prevRange

<details>
	<summary>See more</summary>
	
	canComputeMessageEntriesFor: prevRange 

	^ prevRange rangeType notNil 
</details>

#### SmalltalkCompleter>>#computeMessageEntriesOfEnclosedExpressionReturnAt: aRange

<details>
	<summary>See more</summary>
	
	computeMessageEntriesOfEnclosedExpressionReturnAt: aRange   

	self subclassResponsibility 
</details>

#### SmalltalkCompleter>>#computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection

<details>
	<summary>See more</summary>
	
	computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: aCollection

	| selectorsToShow |
	selectorsToShow _ OrderedCollection new.
	
	self class protected: [
		Selectors forPrefix: prefix keysAndValuesDo: [ :selector :lastUsedTime |
				selectorsToShow := self add: selector and: lastUsedTime to: selectorsToShow.
				(Object canUnderstand: selector) ifFalse: [ aCollection add: selector ]]].
				
	selectorsToShow size < EntriesLimit ifTrue: [ selectorsToShow _  self sortByLastUsedTime: selectorsToShow ].
	
	" To see the timestamps in the menu (need to tweak #insertCompletion: to activate. Right now, just for debugging)
	entries _ selectorsToShow collect: [ :ary | ary first, '(', ((DateAndTime fromString: '01/01/1996 00:00') + ary second minutes) printString,')' ]
	"
	^ selectorsToShow collect: [ :selectorAndTime | selectorAndTime first ]
	

</details>

#### SmalltalkCompleter>>#changePositionTo: newPosition

<details>
	<summary>See more</summary>
	
	changePositionTo: newPosition 
	
	position _ newPosition
</details>

#### SmalltalkCompleter>>#isPossibleInvalidEntry: anEntry

<details>
	<summary>See more</summary>
	
	isPossibleInvalidEntry: anEntry

	^possibleInvalidSelectors includes: anEntry 
</details>

#### SmalltalkCompleter>>#lookForBinarySelectorAfterArrayStartStartingAt: anIndex with: aCandidate

<details>
	<summary>See more</summary>
	
	lookForBinarySelectorAfterArrayStartStartingAt: anIndex with: aCandidate
 
	| foundRange foundRangeIndex |
	
	foundRange := self lookForBinarySelectorAfter: #arrayStart startingAt: anIndex with: aCandidate.
	
	^foundRange rangeType = #symbol
		ifTrue: [ 
			foundRangeIndex := allRanges indexOf: foundRange.
			allRanges at: foundRangeIndex - 1 ifAbsent: [ aCandidate ]]
		ifFalse: [ aCandidate ]
</details>

#### SmalltalkCompleter>>#computeMessageEntriesForUnknowClass

<details>
	<summary>See more</summary>
	
	computeMessageEntriesForUnknowClass

	selectorsClasses _ #().
	possibleInvalidSelectors _ IdentitySet new.
	canShowSelectorDocumentation _ true.
	entries _ self computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: possibleInvalidSelectors.
</details>

#### SmalltalkCompleter>>#lookForNoUnaryMessageSend

<details>
	<summary>See more</summary>
	
	lookForNoUnaryMessageSend
		
	| currentIndex currentRangeType |
	
	currentIndex := allRanges size.
	[ currentRangeType := (allRanges at: currentIndex) rangeType.
	currentRangeType = #unary and: [ currentIndex > 1 ]] whileTrue: [ currentIndex := currentIndex - 1 ].

	^currentIndex
</details>

#### SmalltalkCompleter>>#computeEntries

This default implementation might be redefined of needed.


<details>
	<summary>See more</summary>
	
	computeEntries

	| allSource contextClass specificModel range |

	allSource _ model actualContents string.
	specificModel _ self textProviderOrModel.
	contextClass _ self selectedClassOrMetaClassIn: specificModel. 
	allRanges _ self parse: allSource in: contextClass and: specificModel.
	"For debugging porpouses: 
	allRanges collect: [ :r | r rangeType ]
	"
	range _ allRanges lastIfEmpty: [ ^entries _ #() ].
	possibleInvalidSelectors _ #().
	
	range end = position
		ifTrue: [ self computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel ]
	 	ifFalse: [ self computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel ].

	
</details>

#### SmalltalkCompleter>>#parse: allSource in: contextClass and: specificModel

<details>
	<summary>See more</summary>
	
	parse: allSource in: contextClass and: specificModel
	
	| isMethod |
	
	parser _ SHParserST80 new.
	parser
		workspace: ((specificModel is: #providesBindings) ifTrue: [specificModel]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position);
		allSource: allSource.
		
	isMethod := (specificModel is: #Browser)
		ifTrue: [ specificModel isEditingClass not ]
		ifFalse: [ specificModel is: #CodeProvider ].
	parser parse: isMethod.
	
	^ parser rangesWithoutExcessCode.

</details>

#### SmalltalkCompleter>>#computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel

<details>
	<summary>See more</summary>
	
	computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel 
	
	| lastRange |
	
	allRanges removeLast.
	lastRange _ allRanges lastIfEmpty: [ SHRange nilObject ].
	possibleBinarySendRange _ self lookForBinarySendRange.
	
	^ (self canComputeMessageEntriesFor: lastRange) 
		ifTrue: [ self computeMessageEntriesFor: allSource at: lastRange in: contextClass and: specificModel ]
		ifFalse: [ self computeMessageEntriesForUnknowClass ]

</details>

#### SmalltalkCompleter>>#canShowSelectorDocumentation

<details>
	<summary>See more</summary>
	
	canShowSelectorDocumentation

	^canShowSelectorDocumentation 
</details>

#### SmalltalkCompleter>>#add: selector and: lastUsedTime whenNotFullTo: selectorsToShow

<details>
	<summary>See more</summary>
	
	add: selector and: lastUsedTime whenNotFullTo: selectorsToShow
			
	selectorsToShow add: { selector . lastUsedTime }.
	
	^selectorsToShow size = EntriesLimit 
		ifTrue: [ self sortByLastUsedTime: selectorsToShow ]
		ifFalse: [ selectorsToShow ]
	
</details>

#### SmalltalkCompleter>>#selectedEntryFormatted

<details>
	<summary>See more</summary>
	
	selectedEntryFormatted

	^(self entries at: menuMorph selected) separateKeywords
</details>

#### SmalltalkCompleter>>#computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel

<details>
	<summary>See more</summary>
	
	computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel  
	
	| id rangeType |

	canShowSelectorDocumentation _ true.
	id _ allSource copyFrom: range start to: range end.
	rangeType _ range rangeType.

	rangeType == #globalVar
		ifTrue: [ ^self computeMessageEntriesForClass: (Smalltalk at: id asSymbol) class ].
	rangeType == #self
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass ].
	rangeType == #super
		ifTrue: [ ^self computeMessageEntriesForClass: contextClass superclass ].
	rangeType == #true 
		ifTrue: [ ^self computeMessageEntriesForClass: True ].
	rangeType == #false
		ifTrue: [ ^self computeMessageEntriesForClass: False ].
	rangeType == #nil
		ifTrue: [ ^self computeMessageEntriesForClass: UndefinedObject ].
	rangeType == #character
		ifTrue: [ ^self computeMessageEntriesForClass: id first class ].
	rangeType == #number
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #string
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #symbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #stringSymbol
		ifTrue: [ ^self computeMessageEntriesForClass: (self classOfLiteral: id in: contextClass) ].
	rangeType == #instVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofInstVarNamed: id ].
	rangeType == #methodArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #tempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofTempVarNamed: id ].
	rangeType == #blockArg
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockArgNamed: id ].
	rangeType == #blockTempVar
		ifTrue: [ ^specificModel computeMessageEntriesIn: self ofBlockTempVarNamed: id ].
	rangeType == #workspaceVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (specificModel classOfWorkspaceVarNamed: id) ].
	rangeType == #thisContext
		ifTrue: [ ^self computeMessageEntriesForClass: (specificModel classOfThisContext) ]. 
	rangeType == #classVar
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	rangeType == #poolConstant
		ifTrue: [ ^self computeMessageEntriesForClassOrNil: (self classOfLocalBindingNamed: id in: contextClass) ].
	(rangeType beginsWith: #blockEnd)
		ifTrue: [ ^self computeMessageEntriesForClass: BlockClosure ].
	rangeType == #arrayEnd
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	(rangeType beginsWith: #rightBrace)
		ifTrue: [ ^self computeMessageEntriesForClass: Array ].
	rangeType == #unary
		ifTrue: [ ^self computeEntriesOfUnaryMessageReturnNamed: id at: range ].
	(rangeType beginsWith: #rightParenthesis)
		ifTrue: [ ^self computeMessageEntriesOfEnclosedExpressionReturnAt: range ].
	rangeType == #cascadeSeparator
		ifTrue: [ ^self computeMessageEntriesOfCascadeReceiverAt: range ]. 

	self computeMessageEntriesForUnknowClass 
	
	
</details>

#### SmalltalkCompleter>>#selectorsClasses

<details>
	<summary>See more</summary>
	
	selectorsClasses

	^selectorsClasses 
</details>

#### SmalltalkCompleter>>#computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange

<details>
	<summary>See more</summary>
	
	computeEntriesOfUnaryMessageReturnNamed: aSelector at: aRange   
	
	self subclassResponsibility 
</details>

#### SmalltalkCompleter>>#classOfLiteral: aLiteral in: aClass

<details>
	<summary>See more</summary>
	
	classOfLiteral: aLiteral in: aClass 
	
	| compilerClass |
	
	compilerClass := aClass ifNil: [ Compiler ] ifNotNil: [ aClass compilerClass ].
	
	^ (compilerClass evaluate: aLiteral) class 
</details>

#### SmalltalkCompleter>>#computeMessageEntriesWithoutBinaryMessageForClass: aClass

<details>
	<summary>See more</summary>
	
	computeMessageEntriesWithoutBinaryMessageForClass: aClass

	selectorsClasses := Array with: aClass.
	
	entries := self selectorsOf: aClass beginningWith: prefix.
</details>

#### SmalltalkCompleter>>#computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel

<details>
	<summary>See more</summary>
	
	computeMessageEntriesWithEmptyPrefixFor: allSource at: range in: contextClass and: specificModel 
	
	possibleBinarySendRange _ self lookForBinarySendRange.
	prefix _ ''.

	self computeMessageEntriesFor: allSource at: range in: contextClass and: specificModel .
</details>

#### SmalltalkCompleter>>#possibleInvalidSelectors

<details>
	<summary>See more</summary>
	
	possibleInvalidSelectors
	
	^ possibleInvalidSelectors
</details>

#### SmalltalkCompleter>>#isCategoryEntry: anEntry

<details>
	<summary>See more</summary>
	
	isCategoryEntry: anEntry

	^anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader
</details>

#### SmalltalkCompleter>>#selectorsOf: aClass beginningWith: aPrefix

<details>
	<summary>See more</summary>
	
	selectorsOf: aClass beginningWith: aPrefix

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addSelectorsOf: aClass;
		entriesToShow
</details>

#### SmalltalkCompleter>>#computeIdentifierEntries

<details>
	<summary>See more</summary>
	
	computeIdentifierEntries
	
	canShowSelectorDocumentation _ false.
	entries _ self computeIdentifierEntriesBeginningWith: prefix.
</details>

#### SmalltalkCompleter>>#classOfLocalBindingNamed: aName in: aClass

<details>
	<summary>See more</summary>
	
	classOfLocalBindingNamed: aName in: aClass 
	
	^  (aClass localBindingOf: aName) ifNotNil: [ :aBinding | aBinding value class ]
</details>

#### SmalltalkCompleter>>#canSelect: anEntry

<details>
	<summary>See more</summary>
	
	canSelect: anEntry

	^ (self isCategoryEntry: anEntry) not
</details>

#### SmalltalkCompleter>>#computeMessageEntriesForClassOrNil: aClassOrNil

<details>
	<summary>See more</summary>
	
	computeMessageEntriesForClassOrNil: aClassOrNil  

	aClassOrNil 
		ifNil: [ self computeMessageEntriesForUnknowClass ]
		ifNotNil: [ self computeMessageEntriesForClass: aClassOrNil ].
	

</details>

#### SmalltalkCompleter>>#add: selector and: lastUsedTime to: selectorsToShow

<details>
	<summary>See more</summary>
	
	add: selector and: lastUsedTime to: selectorsToShow

	^ selectorsToShow size < EntriesLimit
		ifTrue: [ self add: selector and: lastUsedTime whenNotFullTo: selectorsToShow ]
		ifFalse: [ self add: selector and: lastUsedTime whenFullTo: selectorsToShow ]	
</details>

#### SmalltalkCompleter>>#ifEmptyEntriesShowAllPrefixedSelectors

<details>
	<summary>See more</summary>
	
	ifEmptyEntriesShowAllPrefixedSelectors

	entries isEmpty ifTrue: [ self computeMessageEntriesForUnknowClass ] 
</details>

#### SmalltalkCompleter>>#documentationOf: aMethod

<details>
	<summary>See more</summary>
	
	documentationOf: aMethod

	self subclassResponsibility 
</details>

#### SmalltalkCompleter>>#unaryAndBinarySelectorsOf: aClass beginningWith: aPrefix

<details>
	<summary>See more</summary>
	
	unaryAndBinarySelectorsOf: aClass beginningWith: aPrefix 

	^ (AutoCompleterSelectorsCollector for: aPrefix) 
		addUnaryAndBinarySelectorsOf: aClass;
		entriesToShow
</details>

#### SmalltalkCompleter>>#opensWithTab

Returns wheter should open the auto completer when pressing Tab or not


<details>
	<summary>See more</summary>
	
	opensWithTab

	^true
</details>

#### SmalltalkCompleter>>#computeMessageEntriesOfCascadeReceiverAt: aRange

<details>
	<summary>See more</summary>
	
	computeMessageEntriesOfCascadeReceiverAt: aRange   
	
	self subclassResponsibility 
</details>

#### SmalltalkCompleter>>#computeMessageEntriesWithBinaryMessageForClass: aClass

<details>
	<summary>See more</summary>
	
	computeMessageEntriesWithBinaryMessageForClass: aClass 

	selectorsClasses := Array with: aClass.
	
	entries := self unaryAndBinarySelectorsOf: aClass beginningWith: prefix.
		
</details>

#### SmalltalkCompleter>>#insert: selector and: lastUsedTime at: insertionIndex to: selectorsToShow

<details>
	<summary>See more</summary>
	
	insert: selector and: lastUsedTime at: insertionIndex to: selectorsToShow

	insertionIndex <= EntriesLimit ifTrue: [ selectorsToShow insert: { selector . lastUsedTime } shiftingRightAt: insertionIndex ].
	
	^selectorsToShow 
</details>

#### SmalltalkCompleter>>#computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel

<details>
	<summary>See more</summary>
	
	computeEntriesOfMessageOrIdentifiersFor: allSource at: range in: contextClass and: specificModel
	
	prefix _ allSource copyFrom: range start to: range end.
	(parser isMessage: range rangeType) ifTrue: [ 
		^self computeMessageEntriesWhenSendinMessageFor: allSource in: contextClass and: specificModel ].	
	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [ ^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #() 
</details>

#### SmalltalkCompleter>>#selectedClassOrMetaClassIn: specificModel

<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClassIn: specificModel

	(specificModel is: #CodeProvider) ifTrue: [ ^ specificModel selectedClassOrMetaClass ].
	
	"I can not use #selectedClassOrMetaClass becuase it changes with the selection but when compiling to evaluate it assumes object as receiver - Hernan"
	^ (specificModel isKindOf: Inspector) ifTrue: [ specificModel object class ] 
</details>

#### SmalltalkCompleter>>#isPreviousMessageSendBinary

<details>
	<summary>See more</summary>
	
	isPreviousMessageSendBinary

	^possibleBinarySendRange notNil and: [ possibleBinarySendRange rangeType = #binary ].

	
</details>

#### SmalltalkCompleter>>#computeMessageEntriesForClass: aClass

<details>
	<summary>See more</summary>
	
	computeMessageEntriesForClass: aClass  

	self isPreviousMessageSendBinary
		ifTrue: [ self computeMessageEntriesWithBinaryMessageForClass: aClass ]
		ifFalse: [ self computeMessageEntriesWithoutBinaryMessageForClass: aClass ].
		
	self ifEmptyEntriesShowAllPrefixedSelectors
</details>

#### SmalltalkCompleter>>#lookForBinarySendRange

<details>
	<summary>See more</summary>
	
	lookForBinarySendRange

	| penultimate currentIndex currentRangeType |

	currentIndex := self lookForNoUnaryMessageSend.
	penultimate := allRanges at: currentIndex - 1 ifAbsent: [ SHRange nilObject ].

	currentRangeType := (allRanges at: currentIndex) rangeType.
	currentRangeType = #rightParenthesis ifTrue: [ 
		^self lookForBinarySelectorAfter: #leftParenthesis startingAt: currentIndex with: penultimate ].	
	currentRangeType = #rightBrace ifTrue: [
		^self lookForBinarySelectorAfter: #leftBrace startingAt: currentIndex with: penultimate ].	
	currentRangeType = #blockEnd ifTrue: [
		^self lookForBinarySelectorAfter: #blockStart startingAt: currentIndex with: penultimate ].	
	currentRangeType = #arrayEnd ifTrue: [
		^self lookForBinarySelectorAfterArrayStartStartingAt: currentIndex with: penultimate ].	

	^({#'$'. #symbol} includes: penultimate rangeType)
		ifTrue: [ allRanges at: currentIndex - 2 ifAbsent: [ SHRange nilObject ] ]
		ifFalse: [ penultimate ]
</details>

#### SmalltalkCompleter>>#textProviderOrModel

<details>
	<summary>See more</summary>
	
	textProviderOrModel

	^ (model is: #hasTextProvider) ifTrue: [ model textProvider ] ifFalse: [ model ].
</details>

#### SmalltalkCompleter>>#add: selector and: lastUsedTime whenFullTo: selectorsToShow

<details>
	<summary>See more</summary>
	
	add: selector and: lastUsedTime whenFullTo: selectorsToShow

	selectorsToShow
		findBinaryIndex: [ :selectorAndTime | selectorAndTime second < lastUsedTime ifTrue: [ -1 ] ifFalse: [ 1 ]]
		do: [ :found | ]
		ifNone: [ :leftBound :rightBound | self insert: selector and: lastUsedTime at: rightBound to: selectorsToShow ].
		
	^selectorsToShow 

</details>

#### SmalltalkCompleter>>#sortByLastUsedTime: selectorsToShow

<details>
	<summary>See more</summary>
	
	sortByLastUsedTime: selectorsToShow

	^selectorsToShow asArray sort: [ :leftSelectorAndTime :rightSelectorAndTime | leftSelectorAndTime second > rightSelectorAndTime second ]
</details>

#### SmalltalkCompleter>>#computeIdentifierEntriesBeginningWith: aPrefix

Use an aux Set to avoid duplicates, but keep the order given.


<details>
	<summary>See more</summary>
	
	computeIdentifierEntriesBeginningWith: aPrefix
	"Use an aux Set to avoid duplicates, but keep the order given."

	| entriesSet lastTitle candidates |
	entriesSet _ Set new.
	lastTitle _ nil.
	
	candidates _ Array streamContents: [ :strm |
		parser namesBeginningWith: aPrefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle _ kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		^ Array with: entriesSet anyOne ]
		ifFalse: [ ^ candidates ]
</details>

#### SmalltalkCompleter>>#newCursorPosition: anEntry

<details>
	<summary>See more</summary>
	
	newCursorPosition: anEntry

	^anEntry indexOf: $ 
</details>

