## BrowserWindow

A morphic window for Browser models. See category 'GUI building'. (BrowserWindow new model: Browser new; buildMorphicWindow) openInWorld instead of Browser openBrowser ---- No dejar en clases de modelo comportamiento que se haya implementado aca!!!! el unico protocolo repetido deberia ser delegacion

### Methods
#### BrowserWindow>>#buildNoSysCatMorphicWindow

A Browser without the class categories list


<details>
	<summary>See more</summary>
	
	buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classList _ self buildMorphicClassList.
	classColumn _ self buildMorphicClassColumnWith: classList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection
</details>

#### BrowserWindow>>#changeKeywordOrder

<details>
	<summary>See more</summary>
	
	changeKeywordOrder

	model selectedMessageName ifNotNil: [ :oldSelector |
		ChangeKeywordsSelectorOrderApplier createAndValueHandlingExceptions: [
			ChangeKeywordsSelectorOrderApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]].
</details>

#### BrowserWindow>>#addExtraMenu2ItemsTo: optoins

The shifted selector-list menu is being built; some menu items are appropriate only for certain kinds of browsers, and this gives a hook for them to be added as approrpiate. If any is added here, a line should be added first -- browse reimplementors of this message for examples.


<details>
	<summary>See more</summary>
	
	addExtraMenu2ItemsTo: optoins
	"The shifted selector-list menu is being built; some menu items are appropriate only for certain kinds of browsers, and this gives a hook for them to be added as approrpiate.  If any is added here, a line should be added first -- browse reimplementors of this message for examples."
</details>

#### BrowserWindow>>#addParameter

<details>
	<summary>See more</summary>
	
	addParameter

	model selectedMessageName ifNotNil: [ :oldSelector | 
		RefactoringApplier addParameterApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ]
</details>

#### BrowserWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations comment separator |
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator.
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 2.0;
		addMorph: separator fixedHeight: 4;
		addMorph: comment proportionalHeight: 2.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
</details>

#### BrowserWindow>>#pushDownSelector

<details>
	<summary>See more</summary>
	
	pushDownSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushDownMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].
</details>

#### BrowserWindow>>#messageListMenu2

Fill aMenu with the items appropriate when the shift key is held down


<details>
	<summary>See more</summary>
	
	messageListMenu2
	"Fill aMenu with the items appropriate when the shift key is held down"

	^DynamicMenuBuilder 
		buildTitled: 'Message List' 
		targeting: self 
		collectingMenuOptionsWith: #messageListMenu2Options 
		changingThemWith: [ :options |
			self addExtraMenu2ItemsTo: options.
			model canShowMultipleMessageCategories ifTrue: [	
				options add: `{ 
					#itemGroup 		-> 		40.
					#itemOrder 		-> 		45.		
					#label 			-> 		'show category (C)'.
					#object 			-> 		#model.
					#selector 		-> 		#showHomeCategory.
					#icon 			-> 		#packageIcon
				} asDictionary` ]].
		
	
</details>

#### BrowserWindow>>#buildMorphicClassColumnWith: classList

<details>
	<summary>See more</summary>
	
	buildMorphicClassColumnWith: classList

	| column |
	
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: (Theme current minimalWindows ifTrue: [AbstractFont default lineSpacing + 4] ifFalse: [AbstractFont default lineSpacing *2-4]).
		
	^column
</details>

#### BrowserWindow>>#classListMenu

Set up the menu to apply to the receiver's class list, honoring the #shifted boolean


<details>
	<summary>See more</summary>
	
	classListMenu
	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"

	^DynamicMenuBuilder buildTitled: 'Class List' targeting: self collectingMenuOptionsWith: #classListMenuOptions.
</details>

#### BrowserWindow>>#buildMorphicClassList

<details>
	<summary>See more</summary>
	
	buildMorphicClassList

	^PluggableListMorph
		model: model
		listGetter: #classList
		indexGetter: #classListIndex
		indexSetter: #classListIndex:
		mainView: self
		menuGetter: #classListMenu
		keystrokeAction: #classListKey:from:
</details>

#### BrowserWindow>>#pushDownInstanceVariable

<details>
	<summary>See more</summary>
	
	pushDownInstanceVariable

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushDownInstanceVariableApplier on: model at: aClass ) value].
</details>

#### BrowserWindow>>#classListMenu2

Set up the menu to apply to the receiver's class list when the shift key is down


<details>
	<summary>See more</summary>
	
	classListMenu2
	"Set up the menu to apply to the receiver's class list when the shift key is down"

	^DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #classListMenu2Options.
	
</details>

#### BrowserWindow>>#systemCategoryMenu

<details>
	<summary>See more</summary>
	
	systemCategoryMenu

	^DynamicMenuBuilder buildTitled: 'Class category' targeting: self collectingMenuOptionsWith: #systemCategoryMenuOptions.
</details>

#### BrowserWindow>>#systemCatSingletonKey: aChar from: aView

<details>
	<summary>See more</summary>
	
	systemCatSingletonKey: aChar from: aView
	^ self messageListKey: aChar from: aView
</details>

#### BrowserWindow>>#classRefactoringMenu

<details>
	<summary>See more</summary>
	
	classRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #classRefactoringMenuOptions.
</details>

#### BrowserWindow>>#editSelectionChanged

<details>
	<summary>See more</summary>
	
	editSelectionChanged
	
	self isEditSelectionNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]
</details>

#### BrowserWindow>>#systemCatListKey: aChar from: view

Respond to a Command key. I am a model with a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	systemCatListKey: aChar from: view

	aChar == $r ifTrue: [^ model recent ].
	
	^super systemCatListKey: aChar from: view
</details>

#### BrowserWindow>>#pushUpSelector

<details>
	<summary>See more</summary>
	
	pushUpSelector

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(PushUpMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].
</details>

#### BrowserWindow>>#buildMorphicWindow

Create a pluggable version of all the morphs for a Browser in Morphic


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"

	| upperPanes messageCatList messageList systemCatList classList |

	systemCatList _ self buildMorphicSystemCatList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	systemCatList rightSibling: classList.
	classList leftSibling: systemCatList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	classList makeItemsDraggable.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
		
	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: systemCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList) proportionalWidth: 0.2;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.2;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection
</details>

#### BrowserWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current browser
</details>

#### BrowserWindow>>#enableCodePaneEditing

<details>
	<summary>See more</summary>
	
	enableCodePaneEditing

	codePane ifNotNil: [ codePane enableEditing ]
</details>

#### BrowserWindow>>#openClassRefactoringMenu

<details>
	<summary>See more</summary>
	
	openClassRefactoringMenu

	^self classRefactoringMenu popUpInWorld
</details>

#### BrowserWindow>>#moveToInstanceOrClassMethod

<details>
	<summary>See more</summary>
	
	moveToInstanceOrClassMethod

	model selectedMessageName ifNotNil: [ :selectedSelector |
		(MoveToInstanceOrClassMethodApplier on: model for: model selectedClassOrMetaClass>>selectedSelector) value ].
</details>

#### BrowserWindow>>#createClassButton

<details>
	<summary>See more</summary>
	
	createClassButton

	| classSwitch |
	
	classSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classMessagesIndicated
		action: #indicateClassMessages.
	
	classSwitch
		label: 'class';
		setBalloonText: 'show class methods'.
		
	^classSwitch
</details>

#### BrowserWindow>>#messageCategoryMenu

<details>
	<summary>See more</summary>
	
	messageCategoryMenu

	^DynamicMenuBuilder buildTitled: 'Message Category' targeting: self collectingMenuOptionsWith: #messageCategoryMenuOptions.

</details>

#### BrowserWindow>>#buildMorphicSystemCatList

<details>
	<summary>See more</summary>
	
	buildMorphicSystemCatList

	^PluggableListMorph
		model: model
		listGetter: #systemCategoryList
		indexGetter: #systemCategoryListIndex
		indexSetter: #systemCategoryListIndex:
		mainView: self
		menuGetter: #systemCategoryMenu
		keystrokeAction: #systemCatListKey:from:
</details>

#### BrowserWindow>>#buildMorphicSwitches

<details>
	<summary>See more</summary>
	
	buildMorphicSwitches

	| instanceSwitch commentSwitch classSwitch row buttonColor |
	
	instanceSwitch _ self createInstanceButton.
	commentSwitch _ self createCommentButton.
	classSwitch _ self createClassButton.
	
	row _ LayoutMorph newRow.
	row
		doAdoptWidgetsColor;
		addMorph: instanceSwitch proportionalWidth: 0.45;
		addMorph: commentSwitch proportionalWidth: 0.22;
		addMorph: classSwitch proportionalWidth: 0.33.
	buttonColor _ self buttonColor.
	row color: buttonColor.

	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m |  m color: buttonColor ].
		
	^row
</details>

#### BrowserWindow>>#createInstanceButton

<details>
	<summary>See more</summary>
	
	createInstanceButton

	| instanceSwitch |
	
	instanceSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #instanceMessagesIndicated
		action: #indicateInstanceMessages.

	instanceSwitch 
		label: 'instance';
		setBalloonText: 'show instance methods'.

	^instanceSwitch 
</details>

#### BrowserWindow>>#classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

Do nothing here. Subclasses should implement if necessary - Hernan


<details>
	<summary>See more</summary>
	
	classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	| currentSelectedClass |
	
	self canDiscardEdits ifTrue: [
		self model selectedSystemCategoryName = aCategoryName ifTrue: [
			currentSelectedClass := self model selectedClass.
			currentSelectedClass ifNil: [ 
				self model originalSelectedClassName = oldClassName ifTrue: [ 
					currentSelectedClass := aClass ]].
			
		 	self model changed: #classList.
			self model selectClass: currentSelectedClass ]]
</details>

#### BrowserWindow>>#removeAllUnreferencedInstVar

<details>
	<summary>See more</summary>
	
	removeAllUnreferencedInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveAllUnreferencedInstanceVariablesApplier on: model for: aClass) value ].
</details>

#### BrowserWindow>>#pushUpInstanceVariable

<details>
	<summary>See more</summary>
	
	pushUpInstanceVariable

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(PushUpInstanceVariableApplier on: model at: aClass ) value].
</details>

#### BrowserWindow>>#openMessageRefactoringMenu

<details>
	<summary>See more</summary>
	
	openMessageRefactoringMenu

	^self messageRefactoringMenu popUpInWorld
</details>

#### BrowserWindow>>#renameSelector

<details>
	<summary>See more</summary>
	
	renameSelector

	model selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ]
</details>

#### BrowserWindow>>#renameInstVar

<details>
	<summary>See more</summary>
	
	renameInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RenameInstanceVariableApplier on: model at: aClass) value ].
</details>

#### BrowserWindow>>#isEditSelectionNone

<details>
	<summary>See more</summary>
	
	isEditSelectionNone
		
	^ model editSelection = #none
</details>

#### BrowserWindow>>#buildMorphicMessageCatList

<details>
	<summary>See more</summary>
	
	buildMorphicMessageCatList

	^PluggableListMorph
		model: model
		listGetter: #messageCategoryList
		indexGetter: #messageCategoryListIndex
		indexSetter: #messageCategoryListIndex:
		mainView: self
		menuGetter: #messageCategoryMenu
		keystrokeAction: #messageCatListKey:from:
</details>

#### BrowserWindow>>#update: anEvent

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: anEvent
	super update: anEvent.
	anEvent = #editSelection ifTrue: [self editSelectionChanged ] 
</details>

#### BrowserWindow>>#buildMorphicCodePane

Construct the pane that shows the code. Respect the Preference for standardCodeFont.


<details>
	<summary>See more</summary>
	
	buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."

	codePane _ super buildMorphicCodePane.
	^codePane
</details>

#### BrowserWindow>>#buildMorphicMessageList

Build a morphic message list, with #messageList as its list-getter


<details>
	<summary>See more</summary>
	
	buildMorphicMessageList
	"Build a morphic message list, with #messageList as its list-getter"

	^PluggableListMorph
		model: model
		listGetter: #messageList
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:
		mainView: self
		menuGetter: #messageListMenu
		keystrokeAction: #messageListKey:from:
</details>

#### BrowserWindow>>#removeInstVar

<details>
	<summary>See more</summary>
	
	removeInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(RemoveInstanceVariableApplier on: model for: aClass) value ].
</details>

#### BrowserWindow>>#openSystemCategoryBrowser

Create and schedule a new system category browser with initial textual contents set to aString.


<details>
	<summary>See more</summary>
	
	openSystemCategoryBrowser
	"Create and schedule a new system category browser with initial textual 
	contents set to aString."

	model systemCategoryBrowser ifNotNil: [ :newBrowser |	
			BrowserWindow
				openNoSysCat: newBrowser
				label: 'Classes in category ', newBrowser selectedSystemCategoryName ]
</details>

#### BrowserWindow>>#disableCodePaneEditing

<details>
	<summary>See more</summary>
	
	disableCodePaneEditing

	codePane ifNotNil: [ codePane disableEditing ]
</details>

#### BrowserWindow>>#findClass

<details>
	<summary>See more</summary>
	
	findClass

	| scopedClassNames |
	
	scopedClassNames _ model potentialClassNames asOrderedCollection.
	
	self class findClassFrom: scopedClassNames ifFound: [:foundClass |
		model selectCategoryForClass: foundClass.
		model selectClass: foundClass ]
</details>

#### BrowserWindow>>#addInstVar

<details>
	<summary>See more</summary>
	
	addInstVar

	model selectedClassOrMetaClass ifNotNil: [ :aClass |
		(AddInstanceVariableApplier on: model for: aClass) value ].
</details>

#### BrowserWindow>>#buildMorphicCommentPane

Construct the pane that shows the class comment.


<details>
	<summary>See more</summary>
	
	buildMorphicCommentPane
	"Construct the pane that shows the class comment."
	 ^ (BrowserCommentTextMorph
		textProvider: model
		textGetter: #classCommentText
		textSetter: #newClassComment:) emptyTextDisplayMessage: 'Please enter a comment for this class'.
</details>

#### BrowserWindow>>#messageRefactoringMenu

<details>
	<summary>See more</summary>
	
	messageRefactoringMenu

	^DynamicMenuBuilder buildTitled: 'Refactorings' targeting: self collectingMenuOptionsWith: #messsageRefactoringMenuOptions.
</details>

#### BrowserWindow>>#messageListMenu

Answer the message-list menu


<details>
	<summary>See more</summary>
	
	messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	^DynamicMenuBuilder buildTitled: 'Message List' targeting: self collectingMenuOptionsWith: #messageListMenuOptions.
	
</details>

#### BrowserWindow>>#createCommentButton

<details>
	<summary>See more</summary>
	
	createCommentButton

	|  commentSwitch  |
	
	commentSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classCommentIndicated
		action: #plusButtonHit.
				
	commentSwitch 
		label: '?';
		setBalloonText: 'show class comment'.
		
	^commentSwitch 
</details>

#### BrowserWindow>>#removeParameter

<details>
	<summary>See more</summary>
	
	removeParameter

	model selectedMessageName ifNotNil: [ :oldSelector |
		RefactoringApplier removeParameterApplier createAndValueHandlingExceptionsOn: model for: oldSelector in: model selectedClassOrMetaClass ].
		
		
</details>

#### BrowserWindow>>#browseAllClasses

Create and schedule a new browser on all classes alphabetically.


<details>
	<summary>See more</summary>
	
	browseAllClasses
	"Create and schedule a new browser on all classes alphabetically."
	| newBrowser |
	newBrowser _ HierarchyBrowser new initAlphabeticListing.
	HierarchyBrowserWindow openNoSysCat: newBrowser label: 'All Classes Alphabetically'
</details>

#### BrowserWindow>>#systemCatSingletonMenu

<details>
	<summary>See more</summary>
	
	systemCatSingletonMenu

	^DynamicMenuBuilder buildTitled: 'Class category' targeting: self collectingMenuOptionsWith: #systemCatSingletonMenuOptions.

</details>

## ChangeListWindow

Morphic view for ChangeList models. See category 'GUI building'.

### Methods
#### ChangeListWindow>>#optionalButtonRow

Answer a row of buttons to occur in a tool pane


<details>
	<summary>See more</summary>
	
	optionalButtonRow
	"Answer a row of buttons to occur in a tool pane"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph 
					model: model
					stateGetter: nil
					action: tuple third.
		button color: buttonColor.
		button label: tuple second asString.
		buttons add: button.
		button setBalloonText: tuple fourth].
	buttons add: self lineDiffButton.
	widths add: 14.
	buttons add: self wordDiffButton.
	widths add: 16.
	model wantsPrettyDiffOption ifTrue: [
		buttons add:  self prettyLineDiffButton.
		widths add: 21.
		buttons add:  self prettyWordDiffButton.
		widths add: 23 ].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row
</details>

#### ChangeListWindow>>#optionalModelButtonTuples

<details>
	<summary>See more</summary>
	
	optionalModelButtonTuples

	^#(
		(11		'select all' 			selectAll				'select all entries')
		(14		'deselect all'		deselectAll			'deselect all entries')
		(19		'file in selections' 	fileInSelections		'file in all selected entries')
	)
</details>

#### ChangeListWindow>>#browseVersions

Create and schedule a Versions Browser, showing all versions of the currently selected message. Answer the browser or nil.


<details>
	<summary>See more</summary>
	
	browseVersions

	| browser change |
	change _ model xtraVersion.
	change ifNil: [ ^self ].
	browser _ super browseVersions.
	browser ifNotNil: [ browser addedChangeRecord: change ].
	^browser
</details>

#### ChangeListWindow>>#listMenu

Fill aMenu up so that it comprises the primary changelist-browser menu


<details>
	<summary>See more</summary>
	
	listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Change List'.
	aMenu addStayUpIcons.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fileIn selections'.
				#object 			-> 		#model.
				#selector 		-> 		#fileInSelections.
				#balloonText 	-> 		'import the selected items into the image'.
			} asDictionary.
			{
				#label 			-> 		'fileOut selections...	'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutSelections.
				#balloonText 	-> 		'create a new file containing the selected items'.
			} asDictionary.
			{
				#label 			-> 		'fileOut current version of selections...'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutCurrentVersionsOfSelections.
				#balloonText 	-> 		'create a new file containing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'compare to current'.
				#object 			-> 		#model.
				#selector 		-> 		#compareToCurrentVersion.
				#balloonText 	-> 		'open a separate window which shows the text differences between the on-file version and the in-image version.'.
			} asDictionary.
			{
				#label 			-> 		'toggle diffing (D)'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleDiffing.
				#balloonText 	-> 		'start or stop showing diffs in the code pane.'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select new methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectNewMethods.
				#balloonText 	-> 		'select methods in the file that do not currently exist in the image'.
			} asDictionary.
			{
				#label 			-> 		'select changes for absent classes'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForAbsentClasses.
				#balloonText 	-> 		'select methods in the file for classes that are not defined in the image'.
			} asDictionary.
			{
				#label 			-> 		'select all changes for this class'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAllForThisClass.
				#balloonText 	-> 		'select all methods in the file that belong to the currently-selected class'.
			} asDictionary.
			{
				#label 			-> 		'select unchanged methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectUnchangedMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions are the same as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods equivalent to current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectEquivalentMethods.
				#balloonText 	-> 		'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'.
			} asDictionary.
			{
				#label 			-> 		'select methods older than current'.
				#object 			-> 		#model.
				#selector 		-> 		#selectMethodsOlderThanCurrent.
				#balloonText 	-> 		'select methods in the file that are older than the one currently in the image'.
			} asDictionary.
			{
				#label 			-> 		'select removals of sent methods'.
				#object 			-> 		#model.
				#selector 		-> 		#selectRemovalsOfSent.
				#balloonText 	-> 		'select all method removals of methods that have some sender in the image'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'select all (a)'.
				#object 			-> 		#model.
				#selector 		-> 		#selectAll.
				#balloonText 	-> 		'select all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'deselect all'.
				#object 			-> 		#model.
				#selector 		-> 		#deselectAll.
				#balloonText 	-> 		'deselect all the items in the list'.
			} asDictionary.
			{
				#label 			-> 		'invert selections'.
				#object 			-> 		#model.
				#selector 		-> 		#invertSelections.
				#balloonText 	-> 		'select every item that is not currently selected, and deselect every item that *is* currently selected'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse class and method'.
				#selector 		-> 		#browseMethodFull.
				#balloonText 	-> 		'open a full browser showing the selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse all versions of single selection'.
				#selector 		-> 		#browseVersions.
				#balloonText 	-> 		'open a version browser showing the versions of the currently selected method'.
			} asDictionary.
			{
				#label 			-> 		'browse current versions of selections'.
				#selector 		-> 		#browseCurrentVersionsOfSelections.
				#balloonText 	-> 		'open a message-list browser showing the current (in-image) counterparts of the selected methods'.
			} asDictionary.
			{
				#label 			-> 		'destroy current methods of selections'.
				#object 			-> 		#model.
				#selector 		-> 		#destroyCurrentCodeOfSelections.
				#balloonText 	-> 		'remove (*destroy*) the in-image counterparts of all selected methods'.
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove doIts'.
				#object 			-> 		#model.
				#selector 		-> 		#removeDoIts.
				#balloonText 	-> 		'remove all items that are doIts rather than definitions'.
			} asDictionary.
			{
				#label 			-> 		'remove older versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeOlderMethodVersions.
				#balloonText 	-> 		'remove all but the most recent versions of methods in the list'.
			} asDictionary.
			{
				#label 			-> 		'remove up-to-date versions'.
				#object 			-> 		#model.
				#selector 		-> 		#removeUpToDate.
				#balloonText 	-> 		'remove all items whose code is the same as the counterpart in-image code'.
			} asDictionary.
			{
				#label 			-> 		'remove empty class comments'.
				#object 			-> 		#model.
				#selector 		-> 		#removeEmptyClassComments.
				#balloonText 	-> 		'remove all empty class comments'.
			} asDictionary.
			{
				#label 			-> 		'remove selected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeSelections.
				#balloonText 	-> 		'remove the selected items from the change-list'.
			} asDictionary.
			{
				#label 			-> 		'remove unselected items'.
				#object 			-> 		#model.
				#selector 		-> 		#removeNonSelections.
				#balloonText 	-> 		'remove all the items not currently selected from the change-list'.
			} asDictionary.
		}`.
	^ aMenu
</details>

#### ChangeListWindow>>#buildMorphicWindow

Open a morphic view for the messageSet, whose label is labelString. The listView may be either single or multiple selection type


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"
	| listPane |
	listPane _ PluggableListMorphOfMany
		model: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	
	self layoutMorph
		addMorph: listPane proportionalHeight: 0.4;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6
</details>

#### ChangeListWindow>>#browseCurrentVersionsOfSelections

Opens a message-list browser on the current in-memory versions of all methods that are currently seleted


<details>
	<summary>See more</summary>
	
	browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ model currentVersionsOfSelections.

	aList size = 0 ifTrue: [^ self inform: 'no selected methods have in-memory counterparts'].
	MessageSetWindow
		openMessageList: aList 
		label: 'Current versions of selected methods in ', model file localName
</details>

#### ChangeListWindow>>#changeListKey: aChar from: view

Respond to a Command key in the list pane.


<details>
	<summary>See more</summary>
	
	changeListKey: aChar from: view
	"Respond to a Command key in the list pane."

	aChar == $D ifTrue: [^ model toggleDiffing].
	aChar == $a ifTrue: [^ model selectAll]
</details>

#### ChangeListWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current changeList
</details>

#### ChangeListWindow>>#buildMorphicCodePane

Construct the pane that shows the code. Respect the Preference for standardCodeFont.


<details>
	<summary>See more</summary>
	
	buildMorphicCodePane

	^(TextModelMorph
		textProvider: model
		textGetter: #acceptedContents) emptyTextDisplayMessage: 'Selection detail (no change selected?)'
</details>

## ChangeSorterWindow

Morphic view for ChangeSorter models. See category 'GUI building'.

### Methods
#### ChangeSorterWindow>>#messageMenu

Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter


<details>
	<summary>See more</summary>
	
	messageMenu
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'message list'.
	aMenu addStayUpIcons.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete method from changeSet (d)'.
				#object 			-> 		#model.
				#selector 		-> 		#forget.
				#icon 			-> 		#warningIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'remove method from system (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeMessage.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse method (O)'.
				#selector 		-> 		#openSingleMessageBrowser.
				#icon 			-> 		#scriptIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'fileOut'.
				#object 			-> 		#model.
				#selector 		-> 		#fileOutMessage.
				#icon 			-> 		#fileOutIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'senders of... (n)'.
				#selector 		-> 		#browseSendersOfMessages.
				#icon 			-> 		#mailForwardIcon
			} asDictionary.
			{
				#label 			-> 		'implementors of... (m)'.
				#selector 		-> 		#browseMessages.
				#icon 			-> 		#developmentIcon
			} asDictionary.
			{
				#label 			-> 		'inheritance (i)'.
				#selector 		-> 		#methodHierarchy.
				#icon 			-> 		#goDownIcon
			} asDictionary.
			{
				#label 			-> 		'versions (v)'.
				#selector 		-> 		#browseVersions.
				#icon 			-> 		#clockIcon
			} asDictionary.
		}`.
	^ aMenu
</details>

#### ChangeSorterWindow>>#classListKey: aChar from: view

Respond to a Command key in the class-list pane.


<details>
	<summary>See more</summary>
	
	classListKey: aChar from: view
	"Respond to a Command key in the class-list pane."

	aChar == $x ifTrue: [^ model removeClass].
	aChar == $d ifTrue: [^ model forgetClass]. 

	^ self messageListKey: aChar from: view "picks up b,h,p"
</details>

#### ChangeSorterWindow>>#browseMethodConflicts

Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such.


<details>
	<summary>See more</summary>
	
	browseMethodConflicts
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |

	aList _ model methodConflicts.
	aList isEmpty ifTrue: [
		^ self inform: 'No other change set has changes for any method in this change set.' ].

	MessageSetWindow
		open: (MessageSet messageList: aList)
		label: 'Methods in "', model changeSet name, '" that are also in other change sets (', aList size printString, ')'
</details>

#### ChangeSorterWindow>>#browseVersions

Create and schedule a changelist browser on the versions of the selected message.


<details>
	<summary>See more</summary>
	
	browseVersions
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	| class selector method category pair sourcePointer |

	(selector _ model selectedMessageName) ifNil: [^ self].
	class _ model selectedClassOrMetaClass.
	(class includesSelector: selector)
		ifTrue: [method _ class compiledMethodAt: selector.
				category _ class whichCategoryIncludesSelector: selector.
				sourcePointer _ nil]
		ifFalse: [pair _ model methodInfoFromRemoval: {class name. selector}.
				pair ifNil: [^ nil].
				sourcePointer _ pair first.
				method _ CompiledMethod toReturnSelf setSourcePointer: sourcePointer.
				category _ pair last].
	VersionsBrowserWindow
		browseVersionsOf: method
		class: model selectedClass meta: class isMeta
		category: category selector: selector
		lostMethodPointer: sourcePointer
</details>

#### ChangeSorterWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent
	^`540@300` * Preferences standardCodeFont lineSpacing // 14
</details>

#### ChangeSorterWindow>>#changeSetListKey: aChar from: view

Respond to a Command key. I am a model with a listView that has a list of changeSets.


<details>
	<summary>See more</summary>
	
	changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	aChar == $D ifTrue: [^ model toggleDiffing]. 
	aChar == $o ifTrue: [^ model fileOutAndRemove].
	aChar == $k ifTrue: [^ model fileOutAndKeep].
	aChar == $r ifTrue: [^ model rename].
	aChar == $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view
</details>

#### ChangeSorterWindow>>#buildMorphicWindow

Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0.


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| dirtyFlags changeSetList classList messageList upperPanes backColor labelBackground |
	backColor _ self textBackgroundColor.
	labelBackground _ Theme current background.
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	dirtyFlags _ PluggableListMorph
		model: model
		listGetter: #changeSetDirtyFlags
		indexGetter: nil
		indexSetter: nil.
	dirtyFlags color: backColor.
	dirtyFlags _ LayoutMorph newColumn
		color: Theme current background;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	changeSetList _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.
	changeSetList color: backColor.
	changeSetList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.
	classList color: backColor.
	classList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: changeSetList proportionalWidth: 0.47;
		addAdjusterAndMorph: classList proportionalWidth: 0.4.

	messageList _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.
	messageList color: backColor.
	messageList _ LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString
</details>

#### ChangeSorterWindow>>#changeSetMenu

Set up aMenu to hold commands for the change-set-list pane. This could be for a single or double changeSorter


<details>
	<summary>See more</summary>
	
	changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu isForBaseSystem |
	isForBaseSystem _ model changeSet isForBaseSystem.
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'File out and remove (o)' 			action: #fileOutAndRemove 	icon: #fileOutIcon 	enabled: isForBaseSystem.
	aMenu add: 'File out and keep (k)' 				action: #fileOutAndKeep 		icon: #fileOutIcon.
	aMenu addLine.

	aMenu add: 'Rename change set (r)' 			action: #rename 					icon: #saveAsIcon 	enabled: isForBaseSystem.
	aMenu add: 'Destroy change set (x)' 			action: #remove 					icon: #warningIcon 	enabled: isForBaseSystem.
	aMenu addLine.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'Edit preamble (p)' 		action: #addPreamble 			icon: #textEditorIcon.
				aMenu add: 'Remove preamble' 	action: #removePreamble 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add preamble (p)' 	action: #addPreamble 			icon: #listAddIcon ].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'Edit postscript...' 		action: #editPostscript 			icon: #textEditorIcon .
				aMenu add: 'Remove postscript' 	action: #removePostscript 		icon: #listRemoveIcon ]
			ifFalse: [
				aMenu add: 'Add postscript...' 		action: #editPostscript 			icon: #listAddIcon ].
	].
	aMenu addLine.

	"CONFLICTS SECTION"
	(aMenu add: 'conflicts with other change sets' 	target: self 	action: #browseMethodConflicts 	icon: #emblemImportantIcon)
		setBalloonText: 'Browse all methods that occur both in this change set and in at least one other change set.'.
	aMenu addLine.

	"CHECKS SECTION"
	(aMenu add: 'trim history' 						action: #trimHistory 				icon: #clockIcon 		enabled: isForBaseSystem)
		setBalloonText: ' Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes. ',
				'NOTE: can cause confusion if later filed in over an earlier version of these changes'.

	(aMenu add: 'view affected class categories' action: #viewAffectedClassCategories icon: #packageIcon)
		setBalloonText: ' Show class categories affected by any contained change'.

	^ aMenu
</details>

#### ChangeSorterWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current changeSorter
</details>

#### ChangeSorterWindow>>#classListMenu

Fill aMenu with items appropriate for the class list


<details>
	<summary>See more</summary>
	
	classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'delete class from change set (d)'.
				#object 			-> 		#model.
				#selector 		-> 		#forgetClass.
				#icon 			-> 		#warningIcon
			} asDictionary.
			{
				#label 			-> 		'remove class from system (x)'.
				#object 			-> 		#model.
				#selector 		-> 		#removeClass.
				#icon 			-> 		#deleteIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse full (b)'.
				#selector 		-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.
			{
				#label 			-> 		'browse protocol (p)'.
				#selector 		-> 		#browseFullProtocol.
				#icon 			-> 		#spreadsheetIcon
			} asDictionary.
			nil.
			{
				#label 			-> 		'inst var refs...'.
				#selector 		-> 		#browseInstVarRefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'inst var defs...'.
				#selector 		-> 		#browseInstVarDefs.
				#icon 			-> 		#instanceIcon
			} asDictionary.
			{
				#label 			-> 		'class var refs...'.
				#selector 		-> 		#browseClassVarRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class vars'.
				#selector 		-> 		#browseClassVariables.
				#icon 			-> 		#classIcon
			} asDictionary.
			{
				#label 			-> 		'class refs (N)'.
				#selector 		-> 		#browseClassRefs.
				#icon 			-> 		#classIcon
			} asDictionary.
		}`.
	^ aMenu
</details>

#### ChangeSorterWindow>>#messageListKey: aChar from: view

Respond to a Command key in the message-list pane.


<details>
	<summary>See more</summary>
	
	messageListKey: aChar from: view
	"Respond to a Command key in the message-list pane."

	aChar == $d ifTrue: [^ model forget].
	super messageListKey: aChar from: view
</details>

## CodeFileBrowserWindow

Morphic view for CodeFileBrowser models. See category 'GUI building'.

### Methods
#### CodeFileBrowserWindow>>#classListKey: aChar from: view

Respond to a Command key. I am a model with a list of classes and a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	classListKey: aChar from: view
	aChar == $b ifTrue: [^ self browseMethodFull].
	aChar == $N ifTrue: [^ self browseClassRefs].
	self codeFileListKey: aChar from: view
</details>

#### CodeFileBrowserWindow>>#browseMethodFull

Create and schedule a full Browser and then select the current class and message.


<details>
	<summary>See more</summary>
	
	browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass _ model selectedClassOrMetaClass) ifNotNil: [
		BrowserWindow fullOnClass: myClass realClass selector: model selectedMessageName]
</details>

#### CodeFileBrowserWindow>>#messageCategoryMenu

<details>
	<summary>See more</summary>
	
	messageCategoryMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: model.
	"All the options are for the model."
	aMenu addTitle: 'Message Category'.
	itemColl _ OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #selector -> #fileInMessageCategories. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #selector -> #fileOutMessageCategories. #icon -> #fileOutIcon} asDictionary. 
			nil. 
			{#label -> 'reorganize'. #selector -> #editMessageCategories. #icon -> #sendReceiveIcon} asDictionary. 
			nil. 
			{#label -> 'add item...'. #selector -> #addCategory. #icon -> #newIcon} asDictionary. 
			nil. 
			{#label -> 'rename...'. #selector -> #renameCategory. #icon -> #saveAsIcon} asDictionary. 
			{#label -> 'remove'. #selector -> #removeMessageCategory. #icon -> #listRemoveIcon} asDictionary
		}.
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		itemColl addAll:
			{
				nil. 
				{#label -> 'remove existing'. #selector -> #removeUnmodifiedMethods. #icon -> #deleteIcon} asDictionary
			} ].
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.
</details>

#### CodeFileBrowserWindow>>#browseVersions

Create and schedule a message set browser on all versions of the currently selected message selector.


<details>
	<summary>See more</summary>
	
	browseVersions
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	| class selector |
	(selector _ model selectedMessageName) ifNotNil: [
		class _ model selectedClassOrMetaClass.
		(class exists and: [class realClass includesSelector: selector]) ifTrue: [
			VersionsBrowserWindow
				browseVersionsOf: (class realClass compiledMethodAt: selector)
				class: class realClass theNonMetaClass
				meta: class realClass isMeta
				category: model selectedMessageCategoryName
				selector: selector]]
</details>

#### CodeFileBrowserWindow>>#findClass

<details>
	<summary>See more</summary>
	
	findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	pattern _ (FillInTheBlankMorph request: 'Class Name?') asLowercase.
	pattern isEmpty ifTrue: [^ self].
	classNames := Set new.
	classNames addAll: model caseCodeSource classDictionary keys.
	classNames := classNames asArray select: 
		[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
	classNames isEmpty ifTrue: [^ self].
	index _ classNames size = 1
				ifTrue:	[1]
				ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
	index = 0 ifTrue: [^ self].
	foundCodeFile := nil.
	foundClass := nil.
		(model caseCodeSource classDictionary includesKey: (classNames at: index)) ifTrue:[
			foundClass := model caseCodeSource classDictionary at: (classNames at: index).
			foundCodeFile := model caseCodeSource ].
	foundClass ifNotNil: [
	 	model systemCategoryListIndex: (model systemCategoryList indexOf: foundCodeFile name asSymbol).
		model classListIndex: (model classList indexOf: foundClass name) ]
</details>

#### CodeFileBrowserWindow>>#buildMorphicWindow

Create a pluggable version of all the views for a Browser, using Morphic widgets.


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model systemCategoryListIndex: 1.
	sysCatList _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.
	sysCatList rightSibling: clsList.
	clsList leftSibling: sysCatList rightSibling: msgCatList.
	msgCatList leftSibling: clsList rightSibling: msgList.
	msgList leftSibling: msgCatList.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection
</details>

#### CodeFileBrowserWindow>>#codeFileListMenu

<details>
	<summary>See more</summary>
	
	codeFileListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Code File'.
	itemColl _ OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'find class... (f)'. #selector -> #findClass} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileIn} asDictionary ].
	itemColl add:
		{#label -> 'fileOut'. #object -> #model. #selector -> #fileOut} asDictionary.
	self model caseCodeSource isLiveSmalltalkImage ifFalse: [
		itemColl add:
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedClasses} asDictionary ].
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.
</details>

#### CodeFileBrowserWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current fileContentsBrowser
</details>

#### CodeFileBrowserWindow>>#messageListMenu

Answer the message-list menu


<details>
	<summary>See more</summary>
	
	messageListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Message List'.
	itemColl _ OrderedCollection new.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl add:
			{#label -> 'fileIn'. #object -> #model. #selector -> #fileInMessage. #icon -> #updateIcon} asDictionary ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutMessage. #icon -> #fileOutIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'senders (n)'. #selector -> #browseSenders. #icon -> #mailForwardIcon} asDictionary. 
				{#label -> 'implementors (m)'. #selector -> #browseImplementors. #icon -> #developmentIcon} asDictionary. 
				{#label -> 'method inheritance (h)'. #selector -> #methodHierarchy. #icon -> #goDownIcon} asDictionary. 
				{#label -> 'versions (v)'. #selector -> #browseVersions. #icon -> #clockIcon} asDictionary
			} ].
	itemColl addAll:
		{
			nil. 
			{#label -> 'remove method (x)'. #object -> #model. #selector -> #removeMessage. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.
</details>

#### CodeFileBrowserWindow>>#classListMenu

Set up the menu to apply to the receiver's class list, honoring the #shifted boolean


<details>
	<summary>See more</summary>
	
	classListMenu
	| aMenu itemColl |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	itemColl _ OrderedCollection new.
	itemColl addAll:
		{
			{#label -> 'definition'. #object -> #model. #selector -> #editClass. #icon -> #editFindReplaceIcon} asDictionary. 
			{#label -> 'comment'. #object -> #model. #selector -> #editComment. #icon -> #editFindReplaceIcon} asDictionary. 
			nil
		}.
	self model baseCodeSource isLiveSmalltalkImage ifTrue: [
		itemColl addAll:
			{
				{#label -> 'browse full (b)'. #selector -> #browseMethodFull. #icon -> #editFindReplaceIcon} asDictionary. 
				{#label -> 'class refs (N)'. #selector -> #browseClassRefs. #icon -> #classIcon} asDictionary. 
				nil. 
				{#label -> 'fileIn'. #object -> #model. #selector -> #fileInClass. #icon -> #updateIcon} asDictionary
			} ].
	itemColl addAll:
		{
			{#label -> 'fileOut'. #object -> #model. #selector -> #fileOutClass. #icon -> #fileOutIcon} asDictionary. 
			nil. 
			{#label -> 'rename...'. #object -> #model. #selector -> #renameClass. #icon -> #saveAsIcon} asDictionary. 
			{#label -> 'remove'. #object -> #model. #selector -> #removeClass. #icon -> #listRemoveIcon} asDictionary. 
			nil. 
			{#label -> 'remove existing'. #object -> #model. #selector -> #removeUnmodifiedCategories. #icon -> #deleteIcon} asDictionary
		}.
	aMenu addItemsFromDictionaries: itemColl.
	^ aMenu.
</details>

#### CodeFileBrowserWindow>>#methodHierarchy

Create and schedule a method browser on the hierarchy of implementors.


<details>
	<summary>See more</summary>
	
	methodHierarchy
	(model selectedClassOrMetaClass isNil or:
		[model selectedClassOrMetaClass hasDefinition])
			ifFalse: [super methodHierarchy]
</details>

#### CodeFileBrowserWindow>>#codeFileListKey: aChar from: view

<details>
	<summary>See more</summary>
	
	codeFileListKey: aChar from: view

	aChar == $f ifTrue: [^ self findClass]
</details>

#### CodeFileBrowserWindow>>#optionalButtonTuples

Answer a tuple buttons, in the format: button label selector to send help message


<details>
	<summary>See more</summary>
	
	optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (self model baseCodeSource isLiveSmalltalkImage and: [ self model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples ]
		ifFalse: [ "For non-standard browser configurations assume most of the default buttons are invalid"
			#(
				#(10 'show...' #offerWhatToShowMenu 'menu of what to show in lower pane' )
			) ].
</details>

#### CodeFileBrowserWindow>>#messageListKey: aChar from: view

Respond to a Command key. I am a model with a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	messageListKey: aChar from: view
	aChar == $b ifTrue: [^ self browseMethodFull].
	super messageListKey: aChar from: view
</details>

## CodePackageListWindow

Morphic view for CodePackageList models. See category 'GUI building'.

### Methods
#### CodePackageListWindow>>#deletePackage

<details>
	<summary>See more</summary>
	
	deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet existingOrNewChangeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash asString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current
</details>

#### CodePackageListWindow>>#browseChanges

<details>
	<summary>See more</summary>
	
	browseChanges
	| current |
	current _ model selection.
	current ifNil: [ ^self ].

	ChangeSorterWindow
		open: (SingleSetChangeSorter new 
			myChangeSet: (ChangeSet existingOrNewChangeSetForPackage: current))
		label: nil
</details>

#### CodePackageListWindow>>#createPackage

<details>
	<summary>See more</summary>
	
	createPackage

	| pkName |
	pkName _ FillInTheBlankMorph request: 'Name for new package?'.
	pkName ifNotEmpty: [
		CodePackage
			named: pkName
			createIfAbsent: true
			registerIfNew: true ]
</details>

#### CodePackageListWindow>>#addRequirement

Ask user for a FeatureRequirement based on loaded packages


<details>
	<summary>See more</summary>
	
	addRequirement
	"Ask user for a FeatureRequirement based on loaded packages"
	
	| current currentName packageNames reqiredNames selectionNames choices selection cuisBaseName req selectedName |
	current _ model selection.
	current ifNil: [ ^self ].

	packageNames := model packages collect: [ :pak | pak packageName ].
	currentName := current packageName.
	reqiredNames := current requires collect: [ :r | r name ].
	selectionNames := packageNames select: [ :name |
		((name = currentName) or: [reqiredNames includes: name]) not ].
	cuisBaseName := Feature baseSystemFeature name.
	choices := OrderedCollection with: #CANCEL.
	(reqiredNames includes: cuisBaseName)
		ifFalse: [ choices add: cuisBaseName ].		
	choices addAll: selectionNames.
	choices size = 1 ifTrue: [
		^ PopUpMenu inform: 'All loaded packages are already required, as is Cuis base system' ].
	selection := PopUpMenu
		withCaption: 'Choose package to require' 
		chooseFrom: choices.
	selection <= 1
		ifTrue: [ ^ self ]  "1 -> Cance, 0 -> Clicked outside the menu"
		ifFalse: [
			selectedName := choices at: selection.
			req := (selectedName = cuisBaseName)
				ifTrue: [ Feature baseSystemFeature requirementOfMe ]
				ifFalse: [ (CodePackage installedPackages at: selectedName) requirementOfMe].
			current featureSpec requires: req.
			current hasUnsavedChanges: true.
			self changed: #requirement ]
</details>

#### CodePackageListWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent

	^`540@400` * Preferences standardCodeFont lineSpacing // 14
</details>

#### CodePackageListWindow>>#buildMorphicWindow

CodePackageListWindow open: CodePackageList new


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	" 
	CodePackageListWindow open: CodePackageList new
	"
	| dirtyFlags names fileNames upperRow  description summary backColor labelBackground |
	backColor := self textBackgroundColor.	
	labelBackground := Theme current background.
	
	dirtyFlags := PluggableListMorph
		model: model 
		listGetter: #packageDirtyFlags
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	dirtyFlags color: backColor.
	dirtyFlags := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Unsaved?');
		addMorphUseAll: dirtyFlags.

	names := PluggableListMorph
		model: model 
		listGetter: #packageNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	names color: backColor.
	names := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' Package Name');
		addMorphUseAll: names.

	fileNames := PluggableListMorph
		model: model 
		listGetter: #packageFullNames
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	fileNames color: backColor.
	fileNames := LayoutMorph newColumn
		color: labelBackground;
		addMorph: (RectangleLikeMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (StringMorph new contents: ' File Name');
		addMorphUseAll: fileNames.

	upperRow := LayoutMorph newRow.
	upperRow
		addMorph: dirtyFlags proportionalWidth: 0.13;
		addAdjusterAndMorph: names proportionalWidth: 0.27;
		addAdjusterAndMorph: fileNames proportionalWidth: 0.6.
		
	description := (TextModelMorph
		textProvider: model
		textGetter: #description 
		textSetter: #description:) emptyTextDisplayMessage: 'Please enter a description for this package'.

	summary := (TextModelMorph
		textProvider: model
		textGetter: #summary) emptyTextDisplayMessage: 'Package summary (No package selected?)'.

	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.6;
		addAdjusterAndMorph: self buildButtonPane fixedHeight: Theme current buttonPaneHeight;
		addAdjusterAndMorph: summary fixedHeight: 60;
		addAdjusterAndMorph: description proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildRequirementsPane proportionalHeight: 0.15.
	self setLabel: 'Installed Packages'
</details>

#### CodePackageListWindow>>#fontPreferenceChanged

System fonts have changed; rebuild myself


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged
	"System fonts have changed; rebuild myself"
	self layoutMorph removeAllMorphs.
	super fontPreferenceChanged.
	self buildMorphicWindow.

</details>

#### CodePackageListWindow>>#browse

<details>
	<summary>See more</summary>
	
	browse

	| current browser |
	current _ model selection.
	current ifNil: [ ^self ].

	browser _ SinglePackageBrowser new.
	browser package: current.
	BrowserWindow open: browser label: browser labelString
</details>

#### CodePackageListWindow>>#buildRequirementsPane

editReqButton


<details>
	<summary>See more</summary>
	
	buildRequirementsPane

	| requirements deleteReqButton "editReqButton" reqLayout buttonLayout updateReqButton |
	requirements := PluggableListMorph
		model: (PackageRequirementsList fromCodePackageList: model)
		listGetter: #requirementsStrings
		indexGetter: #selectionIndex
		indexSetter: #selectionIndex:.
	requirements color: Theme current textPane.
		
	deleteReqButton := PluggableButtonMorph 
							model: requirements model
							action: #deleteSelectedRequirement 
							label: 'delete'.
	deleteReqButton color: self widgetsColor.
	updateReqButton _ PluggableButtonMorph 
							model: requirements model
							action: #updateSelectedRequirement 
							label: 'update'.
	updateReqButton color: self widgetsColor.
							
	buttonLayout := LayoutMorph newRow.
	buttonLayout 
		addMorph: deleteReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						minorDirectionPadding: #top);
		color: self widgetsColor quiteWhiter;
		addMorph: updateReqButton 
		layoutSpec: (LayoutSpec 
						proportionalWidth: 1.0 
						proportionalHeight: 1.0
						minorDirectionPadding: #top);
		color: self widgetsColor quiteWhiter.
		
	model when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	self when: #changed: send: #updateRequirementsFromPackageList to: requirements model.
	requirements model when: #changed: send: #verifyContents to: requirements.
	self when: #changed: send: #verifyContents to: requirements.
	
	reqLayout := LayoutMorph newRow.
	^ reqLayout 
		doAdoptWidgetsColor;
		addMorph: requirements 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.8
							proportionalHeight: 1.0 
							minorDirectionPadding: #left);
		addMorph: buttonLayout 
			layoutSpec: (LayoutSpec 
							proportionalWidth: 0.2 
							proportionalHeight: 1.0 
							minorDirectionPadding: #right);
		color: `Color transparent`;
		yourself
		
</details>

#### CodePackageListWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current packageList
</details>

#### CodePackageListWindow>>#buildButtonPane

<details>
	<summary>See more</summary>
	
	buildButtonPane

	| saveButton createButton deleteButton browseChangesButton browseButton addReqButton buttonRow |
	saveButton := PluggableButtonMorph 
							model: model 
							action: #save 
							label: 'save'.
	createButton := PluggableButtonMorph 
							model: self 
							action: #createPackage 
							label: 'new'.
	deleteButton := PluggableButtonMorph 
							model: self 
							action: #deletePackage 
							label: 'delete/merge'.
	browseChangesButton := PluggableButtonMorph 
							model: self 
							action: #browseChanges 
							label: 'changes'.
	browseButton := PluggableButtonMorph 
							model: self 
							action: #browse 
							label: 'browse'.
	addReqButton := PluggableButtonMorph 
							model: self 
							action: #addRequirement 
							label: 'add requirement'.
	buttonRow := LayoutMorph newRow.
	buttonRow
		doAdoptWidgetsColor;
		color: self widgetsColor quiteWhiter;
		addMorph: saveButton proportionalWidth: 0.6;
		addMorph: createButton proportionalWidth: 0.6;
		addMorph: deleteButton proportionalWidth: 0.6;
		addMorph: browseChangesButton proportionalWidth: 0.6;
		addMorph: browseButton proportionalWidth: 0.6;
		addMorph: addReqButton proportionalWidth: 0.6.
	buttonRow submorphsDo: [ :button | button  color: self widgetsColor ].
	^ buttonRow 
</details>

## CodeWindow

A hierarchy of morphic views, parallel to the CodeProvider models hierarchy. See category 'GUI building'.

### Methods
#### CodeWindow>>#sendQuery: querySelector to: queryPerformer

Apply a query to the primary selector associated with the current context. If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument.


<details>
	<summary>See more</summary>
	
	sendQuery: querySelector to: queryPerformer
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."

	| aSelector aString |
	aSelector _ model selectedMessageName ifNil: [
		aString _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
		^ aString isEmptyOrNil ifFalse: [
			(Symbol hasInterned: aString ifTrue: [ :aSymbol |
				queryPerformer perform: querySelector with: aSymbol])
					ifFalse: [ self inform: 'no such selector' ]]].

	queryPerformer perform: querySelector with: aSelector
</details>

#### CodeWindow>>#browseHierarchy

Create and schedule a new hierarchy browser on the currently selected class or meta.


<details>
	<summary>See more</summary>
	
	browseHierarchy
	"Create and schedule a new hierarchy browser on the currently selected class or meta."

	model hierarchyBrowser ifNotNil: [ :newBrowser |
		HierarchyBrowserWindow
			openNoSysCat: newBrowser
			label: newBrowser labelString.
		newBrowser assureSelectionsShow ]
</details>

#### CodeWindow>>#prettyWordDiffButton

Return a checkbox that lets the user decide whether prettyDiffs should be shown or not


<details>
	<summary>See more</summary>
	
	prettyWordDiffButton
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyWordDiffs
		action: #togglePrettyWordDiffing)
			label: 'wordPrettyDiffs';
			setBalloonText: self prettyWordDiffButtonHelp
</details>

#### CodeWindow>>#wordDiffButton

Return a checkbox that lets the user decide whether regular diffs should be shown or not


<details>
	<summary>See more</summary>
	
	wordDiffButton
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingWordDiffs
		action: #toggleWordDiffing)
			label: 'wordDiffs';
			setBalloonText: self wordDiffButtonHelp
</details>

#### CodeWindow>>#lineDiffButtonHelp

<details>
	<summary>See more</summary>
	
	lineDiffButtonHelp
	^'Show code differences between the file-based method and the in-memory version, line by line.'
</details>

#### CodeWindow>>#decorateForInheritance

Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to.


<details>
	<summary>See more</summary>
	
	decorateForInheritance
	"Check to see if the currently-viewed method has a super send or an override, and if so, change screen feedback, unless the #decorateBrowserButtons says not to."

	| cm aColor aButton flags buttonColor |
	(aButton _ self inheritanceButton) ifNil: [^ self].
	buttonColor _ self buttonColor.

	Preferences decorateBrowserButtons
		ifFalse: [ ^aButton color: buttonColor ].
	cm _ model currentCompiledMethod.
	(cm is: #CompiledMethod)
		ifFalse: [ ^aButton color: buttonColor ].

	flags _ 0.
	model isThisAnOverride ifTrue: [ flags _ flags bitOr: 4 ].
	cm sendsToSuper ifTrue: [ flags _ flags bitOr: 2 ].
	model isThereAnOverride ifTrue: [ flags _ flags bitOr: 1 ].
	aColor _ {

		"This is NOTan override. There is no super implementation."
		buttonColor.							"no sends to super. there is not override in any subclass"
		`Color tan`.							"no sends to super. there is an override in some subclass"
		`Color red`.							"sends to super. there is no override in any subclass. Error: no super to call (or calls super with a different message)"
		`Color red`.							"sends to super. there is  an override in some subclass. Error: no super to call (or calls super with a different message)"

		"This is an override. There is some super implementation"
		`Color red muchLighter`.			"doesn't have sub; has super but doesn't call it"
		`Color r: 0.94 g: 0.823 b: 0.673`.		"has sub; has super but doesn't call it"
		`Color green muchLighter`.			"doesn't have sub; has super and callsl it"
		`Color blue muchLighter`.			"has sub; has super and callsl it"

	} at: flags + 1.
	Theme current useUniformColors
		ifTrue: [
			aButton color: (self buttonColor mixed: 0.8 with: aColor) ]
		ifFalse: [
			aButton color: aColor ]
</details>

#### CodeWindow>>#offerWhatToShowMenu

Offer a menu governing what to show


<details>
	<summary>See more</summary>
	
	offerWhatToShowMenu
	"Offer a menu governing what to show"

	 | aMenu  |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'What to show'.
	aMenu addStayUpIcons.
	self addContentsTogglesTo: aMenu.
	aMenu popUpInWorld 
</details>

#### CodeWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
</details>

#### CodeWindow>>#findMethodInChangeSets

Find and open a changeSet containing the current method.


<details>
	<summary>See more</summary>
	
	findMethodInChangeSets
	"Find and open a changeSet containing the current method."

	| aName |
	(aName _ model selectedMessageName) ifNotNil: [
		ChangeSorterWindow
			browseChangeSetsWithClass: model selectedClassOrMetaClass
			selector: aName ]
</details>

#### CodeWindow>>#showUnreferencedClassVars

Search for all class variables known to the selected class, and put up a list of those that have no references anywhere in the system. The search includes superclasses, so that you don't need to navigate your way to the class that defines each class variable in order to determine whether it is unreferenced


<details>
	<summary>See more</summary>
	
	showUnreferencedClassVars
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"
	| cls aList |
	(cls _ model selectedClass)
		ifNil: [^ self].
	aList _ cls allUnreferencedClassVariables.
	aList size = 0
		ifTrue: [^ self inform: 'There are no unreferenced
class variables in
' , cls name].
	Transcript newLine; nextPutAll: 'Unreferenced class variable(s) in ', cls name; newLine.
	aList do: [:el | Transcript tab; nextPutAll: el; newLine].
	(SelectionMenu labels: aList selections: aList)
		startUpWithCaption: 'Unreferenced
class variables in 
' , cls name
</details>

#### CodeWindow>>#browseMessages

Present a menu of all messages sent by the currently selected message. Open a message set browser of all implementors of the message chosen.


<details>
	<summary>See more</summary>
	
	browseMessages
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen."

	self getSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk
</details>

#### CodeWindow>>#lineDiffButton

Return a checkbox that lets the user decide whether regular diffs should be shown or not


<details>
	<summary>See more</summary>
	
	lineDiffButton
	"Return a checkbox that lets the user decide whether regular diffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingLineDiffs
		action: #toggleLineDiffing)
			label: 'lineDiffs';
			setBalloonText: self lineDiffButtonHelp
</details>

#### CodeWindow>>#buttonWithSelector: aSelector

If receiver has a control button with the given action selector answer it, else answer nil. morphic only at this point


<details>
	<summary>See more</summary>
	
	buttonWithSelector: aSelector 
	"If receiver has a control button with the given action selector answer it, else answer nil.  morphic only at this point"

	^self 
		findDeepSubmorphThat: [ :m |
			(m is: #PluggableButtonMorph) and: [ m actionSelector == aSelector ]]
		ifAbsent: [ ^nil ]
</details>

#### CodeWindow>>#selectMessageAndEvaluate: aBlock

Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector. If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any


<details>
	<summary>See more</summary>
	
	selectMessageAndEvaluate: aBlock
	"Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector.  If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any"

	| selector method messages |
	(selector _ model selectedMessageName) ifNil: [^ self].
	method _ (model selectedClassOrMetaClass ifNil: [^ self])
		compiledMethodAt: selector
		ifAbsent: nil.
	(method isNil or: [(messages _ method messages) size = 0])
		 ifTrue: [^ aBlock value: selector].
	(messages size = 1 and: [messages includes: selector])
		ifTrue:
			[^ aBlock value: selector].  "If only one item, there is no choice"

	Smalltalk
		showMenuOf: messages
		withFirstItem: selector
		ifChosenDo: aBlock
</details>

#### CodeWindow>>#browseClassVariables

<details>
	<summary>See more</summary>
	
	browseClassVariables

	model selectedClass ifNotNil: [ :cls | 
		cls classPool inspectWithLabel: 'Class Variables in ', cls name]
</details>

#### CodeWindow>>#messageCatListKey: aChar from: view

<details>
	<summary>See more</summary>
	
	messageCatListKey: aChar from: view

	aChar == $o ifTrue: [^ model fileOutMessageCategories ].
	aChar == $t ifTrue: [^ model runMessageCategoryTests ].
	aChar == $x ifTrue: [^ model removeMessageCategory ].
	aChar == $R ifTrue: [ ^model renameCategory ].
	aChar == $n ifTrue: [^model addCategory ].
	aChar == $e ifTrue: [^model removeEmptyCategories ].
	aChar == $c ifTrue: [^model categorizeAllUncategorizedMethods ].
</details>

#### CodeWindow>>#systemCatListKey: aChar from: view

Respond to a Command key. I am a model with a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	systemCatListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also have a listView that has a list of methods.  The view knows how to get the list and selection."

	aChar == $f ifTrue: [ ^ self findClass ].
	aChar == $x ifTrue: [ ^ model removeSystemCategory ].
	aChar == $t ifTrue: [ ^ model runSystemCategoryTests ].
	aChar == $a ifTrue: [ ^ model addSystemCategory ].
	aChar == $A ifTrue: [ ^ model alphabetizeSystemCategories ].
	aChar == $b ifTrue: [ ^ self openSystemCategoryBrowser ].
	aChar == $B ifTrue: [ ^ self browseAllClasses ].
	aChar == $o ifTrue: [ ^ model fileOutSystemCategory ].
	aChar == $u ifTrue: [ ^ model updateSystemCategories ].
	aChar == $R ifTrue: [ ^ model renameSystemCategory ].
	
	^ self classListKey: aChar from: view
</details>

#### CodeWindow>>#browseLocalSendersOfMessages

Present a menu of the currently selected message, as well as all messages sent by it. Open a message set browser of all implementors of the message chosen in or below the selected class


<details>
	<summary>See more</summary>
	
	browseLocalSendersOfMessages
	"Present a menu of the currently selected message, as well as all
	messages sent by it.  Open a message set browser of all implementors
	of the message chosen in or below the selected class"

	self getSelectorAndSendQuery: #browseAllCallsOn:localTo:
		to: Smalltalk
		with: { model selectedClass }
</details>

#### CodeWindow>>#inspectSubInstances

Inspect all instances of the selected class and all its subclasses


<details>
	<summary>See more</summary>
	
	inspectSubInstances
	"Inspect all instances of the selected class and all its subclasses"

	model selectedClassOrMetaClass ifNotNil: [ :c |
		c theNonMetaClass inspectSubInstances]
</details>

#### CodeWindow>>#initializeNotificationActions

Avoid double registration


<details>
	<summary>See more</summary>
	
	initializeNotificationActions

	"Avoid double registration"
	self 
		removeNotificationActions;
		registerNotificationActionsIfModelNotNil 

</details>

#### CodeWindow>>#browseInstVarRefs

<details>
	<summary>See more</summary>
	
	browseInstVarRefs

	model selectedClassOrMetaClass ifNotNil: [ :cls |
		Smalltalk browseInstVarRefs: cls ]
</details>

#### CodeWindow>>#offerClassListMenu

Offer the shifted class-list menu.


<details>
	<summary>See more</summary>
	
	offerClassListMenu
	"Offer the shifted class-list menu."

	^ self classListMenu popUpInWorld
</details>

#### CodeWindow>>#updateListsAndCode

All code windows receive this message on any code change in the system


<details>
	<summary>See more</summary>
	
	updateListsAndCode
	"All code windows receive this message on any code change in the system"
	self canDiscardEdits ifTrue: [
		self allMorphsDo: [ :m | (m is: #PluggableListMorph) ifTrue: [ m verifyContents ]].
		model updateIfNeeded ]
</details>

#### CodeWindow>>#registerNotificationActions

<details>
	<summary>See more</summary>
	
	registerNotificationActions

	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #updateListsAndCode to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self 
</details>

#### CodeWindow>>#browseAllMessages

Create and schedule a message set browser on all implementors of all the messages sent by the current method.


<details>
	<summary>See more</summary>
	
	browseAllMessages
	"Create and schedule a message set browser on all implementors of all the messages sent by the current method."

	| aClass method filteredList |
	model selectedMessageName ifNotNil: [ :aName |
		method _ (aClass _ model selectedClassOrMetaClass) compiledMethodAt: aName.
		filteredList _ method messages reject: 
			[:each | #(new initialize = ) includes: each].
		Smalltalk browseAllImplementorsOfList: filteredList asArray sort
			 title: 'All messages sent in ', aClass name, '.', aName]
</details>

#### CodeWindow>>#removeNotificationActions

<details>
	<summary>See more</summary>
	
	removeNotificationActions

	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.

</details>

#### CodeWindow>>#browseFullProtocol

Create and schedule a new protocol browser on the currently selected class or meta.


<details>
	<summary>See more</summary>
	
	browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."

	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new on: classOrMetaclass.
		label _ 'Entire protocol of: ', classOrMetaclass name.
		ProtocolBrowserWindow open: aPBrowser label: label ]
</details>

#### CodeWindow>>#prettyLineDiffButtonHelp

<details>
	<summary>See more</summary>
	
	prettyLineDiffButtonHelp
	^'Show pretty-printed code differences between the file-based method and the in-memory version, line by line.'
</details>

#### CodeWindow>>#browseSendersOfMessages

Present a menu of the currently selected message, as well as all messages sent by it. Open a message set browser of all senders of the selector chosen.


<details>
	<summary>See more</summary>
	
	browseSendersOfMessages
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self getSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk
</details>

#### CodeWindow>>#optionalButtonTuples

Answer a tuple buttons, in the format: button label selector to send help message


<details>
	<summary>See more</summary>
	
	optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList |

	aList _ #(
	(10	'browse'			browseMethodFull				'view this method in a browser')
	(11	'senders' 			browseSendersOfMessages	'browse senders of...')
	(16	'implementors'		browseMessages				'browse implementors of...')
	(12	'versions'			browseVersions					'browse versions')), 

	(Preferences decorateBrowserButtons
		ifTrue:
			[{#(13	'inheritance'		methodHierarchy 	'browse method inheritance
green: sends to super
tan: has override(s)
mauve: both of the above
pink: is an override but doesn''t call super
pinkish tan: has override(s), also is an override but doesn''t call super' )}]
		ifFalse:
			[{#(13	'inheritance'		methodHierarchy	'browse method inheritance')}]),

	#(
	(12	'hierarchy'			browseHierarchy				'browse class hierarchy')
	(10	'inst vars'			browseInstVarRefs				'inst var refs...')
	(11	'class vars'			browseClassVarRefs			'class var refs...')
	(10	'show...'				offerWhatToShowMenu		'menu of what to show in lower pane')).

	^ aList
</details>

#### CodeWindow>>#messageListKey: aChar from: view

Respond to a Command key. I am a model with a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar == $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar == $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar == $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar == $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar == $N ifTrue: [^ self browseClassRefs].
	aChar == $i ifTrue: [^ self methodHierarchy].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].

	sel
		ifNotNil: [
			"The following require a method selection"
			aChar == $o ifTrue: [^ model fileOutMessage].
			aChar == $c ifTrue: [^ model copySelector].
			aChar == $v ifTrue: [^ self browseVersions].
			aChar == $C ifTrue: [^ model showHomeCategory].
			aChar == $O ifTrue: [^ self openSingleMessageBrowser].
			aChar == $x ifTrue: [^ model removeMessage].
			aChar == $t ifTrue: [^ model runMethodTest].
			aChar == $r ifTrue: [^ model debugMethodTest].
			aChar == $R ifTrue: [^ self renameSelector].
			aChar == $U ifTrue: [^ self addParameter ].
			aChar == $I ifTrue: [^ self removeParameter ]]
		ifNil: [
			aChar == $R ifTrue: [^ model renameClass]]
</details>

#### CodeWindow>>#prettyWordDiffButtonHelp

<details>
	<summary>See more</summary>
	
	prettyWordDiffButtonHelp
	^'Show pretty-printed code differences between the file-based method and the in-memory version, word by word.'
</details>

#### CodeWindow>>#browseInstVarDefs

<details>
	<summary>See more</summary>
	
	browseInstVarDefs 

	model selectedClassOrMetaClass ifNotNil: [ :cls |
		Smalltalk browseInstVarDefs: cls ]
</details>

#### CodeWindow>>#useSelector: incomingSelector orGetSelectorAndSendQuery: querySelector to: queryPerformer

If incomingSelector is not nil, use it, else obtain a selector from user type-in. Using the determined selector, send the query to the performer provided.


<details>
	<summary>See more</summary>
	
	useSelector: incomingSelector orGetSelectorAndSendQuery: querySelector to: queryPerformer
	"If incomingSelector is not nil, use it, else obtain a selector from user type-in.   Using the determined selector, send the query to the performer provided."

	| aSelector |
	incomingSelector
		ifNotNil: [
			queryPerformer perform: querySelector with: incomingSelector]
		ifNil: [
			aSelector _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
			aSelector isEmptyOrNil ifFalse: [
				(Symbol hasInterned: aSelector ifTrue: [ :aSymbol |
					queryPerformer perform: querySelector with: aSymbol])
						ifFalse: [ self inform: 'no such selector']]]
</details>

#### CodeWindow>>#decorateButtons

Change screen feedback for any buttons in the UI of the receiver that may wish it. Initially, it is only the Inheritance button that is decorated, but one can imagine others.


<details>
	<summary>See more</summary>
	
	decorateButtons
	"Change screen feedback for any buttons in the UI of the receiver that may wish it.  Initially, it is only the Inheritance button that is decorated, but one can imagine others."

	self decorateForInheritance
</details>

#### CodeWindow>>#classListKey: aChar from: view

Respond to a Command key. I am a model with a list of classes and a code pane, and I also have a listView that has a list of methods. The view knows how to get the list and selection.


<details>
	<summary>See more</summary>
	
	classListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of classes and a 
	code pane, and I also have a listView that has a list of methods.  The 
	view knows how to get the list and selection."

	aChar == $r ifTrue: [^ model recent].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $x ifTrue: [^ model removeClass].
	aChar == $t ifTrue: [^ model runClassTests ].
	
	^ self messageListKey: aChar from: view
</details>

#### CodeWindow>>#getSelectorAndSendQuery: querySelector to: queryPerformer

Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained as its argument. If no message is currently selected, then obtain a method name from a user type-in


<details>
	<summary>See more</summary>
	
	getSelectorAndSendQuery: querySelector to: queryPerformer
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained as its argument.  If no message is currently selected, then obtain a method name from a user type-in"

	self getSelectorAndSendQuery: querySelector to: queryPerformer with: { }
</details>

#### CodeWindow>>#openMessageListMenu2

Offer the additional selector-list menu


<details>
	<summary>See more</summary>
	
	openMessageListMenu2
	"Offer the additional selector-list menu"

	^ self messageListMenu2 popUpInWorld
</details>

#### CodeWindow>>#makeSampleInstance

<details>
	<summary>See more</summary>
	
	makeSampleInstance
	| aClass nonMetaClass anInstance |
	(aClass _ model selectedClassOrMetaClass) ifNil: [^ self].
	nonMetaClass _ aClass theNonMetaClass.
	anInstance _ nonMetaClass initializedInstance.
	(anInstance isNil and: [nonMetaClass ~~ UndefinedObject]) ifTrue: 
		[^ self inform: 'Sorry, cannot make an instance of ', nonMetaClass name].
	(anInstance is: #Morph)
		ifTrue: [ anInstance openInHand ]
		ifFalse: [ anInstance inspectWithLabel: 'An instance of ', nonMetaClass name ]
</details>

#### CodeWindow>>#classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

Do nothing here. Subclasses should implement if necessary - Hernan


<details>
	<summary>See more</summary>
	
	classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	"Do nothing here. Subclasses should implement if necessary - Hernan"
</details>

#### CodeWindow>>#inspectInstances

Inspect all instances of the selected class.


<details>
	<summary>See more</summary>
	
	inspectInstances
	"Inspect all instances of the selected class."

	model selectedClassOrMetaClass ifNotNil: [ :c |
		c theNonMetaClass inspectAllInstances]
</details>

#### CodeWindow>>#defaultAnnotationPaneHeight

Answer the receiver's preferred default height for new annotation panes.


<details>
	<summary>See more</summary>
	
	defaultAnnotationPaneHeight
	"Answer the receiver's preferred default height for new annotation panes."

	^ AbstractFont default lineSpacing * 2 + 8
</details>

#### CodeWindow>>#activateAndSendTopToBack: aBoolean

Bring me to the front and make me able to respond to mouse and keyboard


<details>
	<summary>See more</summary>
	
	activateAndSendTopToBack: aBoolean
	super activateAndSendTopToBack: aBoolean.
	self decorateButtons
</details>

#### CodeWindow>>#getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs

Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments. If no message is currently selected, then obtain a method name from a user type-in


<details>
	<summary>See more</summary>
	
	getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm _ WriteStream on: (array _ Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName ifNil: [ | selector |
		selector _ FillInTheBlankMorph request: 'Type selector:' initialAnswer: 'flag:'.
		^ selector isEmptyOrNil ifFalse: [
			(Symbol hasInterned: selector
				ifTrue: [ :aSymbol |
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array])
				ifFalse: [ self inform: 'no such selector']
		]
	].

	self selectMessageAndEvaluate: [:selector |
		array at: 1 put: selector.
		queryPerformer perform: querySelector withArguments: array
	]
</details>

#### CodeWindow>>#model: anObject

Set my model and make me me a dependent of the given object.


<details>
	<summary>See more</summary>
	
	model: anObject
	"Set my model and make me me a dependent of the given object."

	super model: anObject.

	self initializeNotificationActions
</details>

#### CodeWindow>>#browseLocalImplementors

Present a menu of all messages sent by the currently selected message. Open a message set browser of all implementors of the message chosen in or below the selected class. Do nothing if no message is chosen.


<details>
	<summary>See more</summary>
	
	browseLocalImplementors
	"Present a menu of all messages sent by the currently selected message. 
	Open a message set browser of all implementors of the message chosen in or below
	the selected class.
	Do nothing if no message is chosen."
	self getSelectorAndSendQuery: #browseAllImplementorsOf:localTo:
		to: Smalltalk
		with: { model selectedClass }
</details>

#### CodeWindow>>#buildMorphicCodePane

Construct the pane that shows the code. Respect the Preference for standardCodeFont.


<details>
	<summary>See more</summary>
	
	buildMorphicCodePane
	"Construct the pane that shows the code.
	Respect the Preference for standardCodeFont."
	^ (TextModelMorph
		textProvider: model
		textGetter: #acceptedContents
		textSetter: #contents:notifying:
		selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Smalltalk code (nothing selected?)'
</details>

#### CodeWindow>>#showUnreferencedInstVars

Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system. The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced


<details>
	<summary>See more</summary>
	
	showUnreferencedInstVars
	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"

	| cls aList |
	(cls _ model selectedClassOrMetaClass) ifNil: [^ self].
	aList _ cls allUnreferencedInstanceVariables.
	aList size = 0 ifTrue: [^ self inform: 'There are no unreferenced
instance variables in
', cls name].
	Transcript newLine; nextPutAll: 'Unreferenced instance variable(s) in ', cls name; newLine.
	aList do: [ :el | Transcript tab; nextPutAll: el; newLine ].
	(SelectionMenu labels: aList selections: aList) startUpWithCaption: 'Unreferenced
instance variables in 
', cls name
</details>

#### CodeWindow>>#prettyLineDiffButton

Return a checkbox that lets the user decide whether prettyDiffs should be shown or not


<details>
	<summary>See more</summary>
	
	prettyLineDiffButton
	"Return a checkbox that lets the user decide whether prettyDiffs should be shown or not"
	^ (PluggableButtonMorph
		model: model
		stateGetter: #showingPrettyLineDiffs
		action: #togglePrettyLineDiffing)
			label: 'linePrettyDiffs';
			setBalloonText: self prettyLineDiffButtonHelp
</details>

#### CodeWindow>>#browseMethodFull

Create and schedule a full Browser and then select the current class and message.


<details>
	<summary>See more</summary>
	
	browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		BrowserWindow fullOnClass: myClass selector: model selectedMessageName]
</details>

#### CodeWindow>>#browseImplementors

Create and schedule a message set browser on all implementors of the currently selected message selector. Do nothing if no message is selected.


<details>
	<summary>See more</summary>
	
	browseImplementors
	"Create and schedule a message set browser on all implementors of the currently selected message selector. Do nothing if no message is selected."

	model selectedMessageName ifNotNil: [ :messageName |
		Smalltalk browseAllImplementorsOf: messageName]
</details>

#### CodeWindow>>#optionalButtonRow

Answer a row of control buttons


<details>
	<summary>See more</summary>
	
	optionalButtonRow
	"Answer a row of control buttons"

	| row buttons widths buttonColor |
	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalButtonTuples do: [ :tuple | | button |
		widths add: tuple first.
		button _ PluggableButtonMorph 
					model: self
					stateGetter: nil
					action: tuple third.
		button color: buttonColor.
		button label: tuple second asString.
		tuple size > 3 ifTrue: [button setBalloonText: tuple fourth].
		buttons add: button ].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row
</details>

#### CodeWindow>>#inheritanceButton

If receiver has an Inheritance button, answer it, else answer nil. morphic only at this point


<details>
	<summary>See more</summary>
	
	inheritanceButton
	"If receiver has an Inheritance button, answer it, else answer nil.  morphic only at this point"

	^ self buttonWithSelector: #methodHierarchy
</details>

#### CodeWindow>>#browseClassRefs

<details>
	<summary>See more</summary>
	
	browseClassRefs

	model selectedClass ifNotNil: [ :cls | 
		Smalltalk browseAllCallsOnClass: cls theNonMetaClass]
</details>

#### CodeWindow>>#browseVersions

Create and schedule a Versions Browser, showing all versions of the currently selected message. Answer the browser or nil.


<details>
	<summary>See more</summary>
	
	browseVersions
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."
	| selector class |
	selector _ model selectedMessageName.
	(selector isNil or: [ MessageSet isPseudoSelector: selector ]) ifTrue: [
		^ VersionsBrowserWindow
			browseCommentOf: model selectedClass ].
	class _ model selectedClassOrMetaClass.
	^ VersionsBrowserWindow
		browseVersionsOf: (class compiledMethodAt: selector)
		class: model selectedClass
		meta: class isMeta
		category: (class organization categoryOfElement: selector)
		selector: selector
</details>

#### CodeWindow>>#browseSenders

Create and schedule a message set browser on all senders of the currently selected message selector. Of there is no message currently selected, offer a type-in


<details>
	<summary>See more</summary>
	
	browseSenders
	"Create and schedule a message set browser on all senders of the currently selected message selector.  Of there is no message currently selected, offer a type-in"

	self sendQuery: #browseAllCallsOn: to: Smalltalk
</details>

#### CodeWindow>>#addContentsTogglesTo: aMenu

Add updating menu toggles governing contents to aMenu.


<details>
	<summary>See more</summary>
	
	addContentsTogglesTo: aMenu
	"Add updating menu toggles governing contents to aMenu."

	model contentsSymbolQuints do: [ :aQuint |
			aQuint == #-
				ifTrue: [
					aMenu addLine]
				ifFalse: [
					(aMenu addUpdating: aQuint third target: model action: aQuint second)
						setBalloonText: aQuint fifth ]]
</details>

#### CodeWindow>>#browseClassVarRefs

<details>
	<summary>See more</summary>
	
	browseClassVarRefs 

	model selectedClass ifNotNil: [ :cls |
		cls browseClassVarRefs ]
</details>

#### CodeWindow>>#buildMorphicAnnotationsPane

<details>
	<summary>See more</summary>
	
	buildMorphicAnnotationsPane

	| aTextMorph |
	aTextMorph _ (TextModelMorph
		textProvider: model
		textGetter: #annotation) emptyTextDisplayMessage: 'Class or method annotation (not selected?)'.
	model when: #annotationChanged send: #refetch to: aTextMorph model.
	model when: #decorateButtons send: #decorateButtons to: self.
	aTextMorph
		askBeforeDiscardingEdits: false;
		hideScrollBarsIndefinitely.
	^aTextMorph
</details>

#### CodeWindow>>#browseProtocol

Create and schedule a new protocol browser on the currently selected class or meta.


<details>
	<summary>See more</summary>
	
	browseProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."
	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new onSubProtocolOf: classOrMetaclass.
		label _'Sub-protocol of: ', classOrMetaclass name.
		ProtocolBrowserWindow open: aPBrowser label: label ]
</details>

#### CodeWindow>>#openSingleMessageBrowser

Create and schedule a message list browser populated only by the currently selected message


<details>
	<summary>See more</summary>
	
	openSingleMessageBrowser
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName _ model selectedMessageName) ifNil: [^ self].

	mr _ MethodReference new
		setStandardClass: model selectedClassOrMetaClass
		methodSymbol: msgName.

	Smalltalk 
		browseMessageList: (Array with: mr)
		name: mr stringVersion
		autoSelect: nil
</details>

#### CodeWindow>>#wordDiffButtonHelp

<details>
	<summary>See more</summary>
	
	wordDiffButtonHelp
	^'Show code differences between the file-based method and the in-memory version, word by word.'
</details>

#### CodeWindow>>#methodHierarchy

Create and schedule a method browser on the hierarchy of implementors.


<details>
	<summary>See more</summary>
	
	methodHierarchy
	"Create and schedule a method browser on the hierarchy of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass _ model selectedClassOrMetaClass.
	sel _ model selectedMessageName.
	aClass ifNil: [ ^ self ].
	sel ifNil: [ ^ self ].
	aClassNonMeta _ aClass theNonMetaClass.
	isMeta _ aClassNonMeta ~~ aClass.
	list _ OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta _ isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	Smalltalk
		browseMessageList: list
		name: 'Inheritance of ' , sel.
</details>

#### CodeWindow>>#openMessageListMenu

Offer the unshifted selector-list menu


<details>
	<summary>See more</summary>
	
	openMessageListMenu
	"Offer the unshifted selector-list menu"

	^ self messageListMenu popUpInWorld
</details>

#### CodeWindow>>#browseUnusedMethods

<details>
	<summary>See more</summary>
	
	browseUnusedMethods
	| list |
	list _ model unusedMethods ifNil: [ ^self ].
	Smalltalk browseMessageList: list name: 'Unsent Methods in ', model selectedClass name
</details>

#### CodeWindow>>#offerClassListMenu2

Offer the shifted class-list menu.


<details>
	<summary>See more</summary>
	
	offerClassListMenu2
	"Offer the shifted class-list menu."

	^ self classListMenu2 popUpInWorld
</details>

#### CodeWindow>>#registerNotificationActionsIfModelNotNil

model set to nil on delete


<details>
	<summary>See more</summary>
	
	registerNotificationActionsIfModelNotNil 

	"model set to nil on delete"
	model ifNotNil: [ self registerNotificationActions ] 
</details>

## DebuggerWindow

Morphic view for Debugger models. See category 'GUI building'.

### Methods
#### DebuggerWindow>>#stepIntoBlock

<details>
	<summary>See more</summary>
	
	stepIntoBlock

	^ self ifOkToChangeCodePaneDo: [ model stepIntoBlock ]
</details>

#### DebuggerWindow>>#customButtonRow

Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane


<details>
	<summary>See more</summary>
	
	customButtonRow
	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'customButtonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"

	| button buttons row buttonColor |
	
	buttons _ OrderedCollection new.
	buttonColor _ self buttonColor.
	"button with target = self"
	button _ PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #proceed.
	button color: buttonColor.
	button label: 'Proceed'.
	button setBalloonText: 'close the debugger and proceed.'.
	buttons add: button.
	"buttons with model target"
	self customButtonSpecs do: [ :tuple |
		button _ PluggableButtonMorph 
					model: self
					stateGetter: nil
					action: tuple second.
		button color: buttonColor.
		button label: tuple first asString.
		tuple size > 2 ifTrue: [button setBalloonText: tuple third].
		buttons add: button].

	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons.
	^row
</details>

#### DebuggerWindow>>#receiverClassHierarchy

<details>
	<summary>See more</summary>
	
	receiverClassHierarchy

	HierarchyBrowserWindow
		onClass: model receiverInspector selectedClassOrMetaClass
		selector: nil
</details>

#### DebuggerWindow>>#browseHierarchyIn: anInspector

Create and schedule a class list browser on the receiver's hierarchy.


<details>
	<summary>See more</summary>
	
	browseHierarchyIn: anInspector
	"Create and schedule a class list browser on the receiver's hierarchy."

	HierarchyBrowserWindow
		onClass: anInspector selectedClassOrMetaClass
		selector: nil
</details>

#### DebuggerWindow>>#returnValue

<details>
	<summary>See more</summary>
	
	returnValue
	| expression |
	expression _ FillInTheBlankMorph request: 'Enter expression for return value:'.
	model returnValue: expression
</details>

#### DebuggerWindow>>#exploreContextSelection

<details>
	<summary>See more</summary>
	
	exploreContextSelection

	model contextVariablesInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model contextVariablesInspector selection explore
</details>

#### DebuggerWindow>>#proceed

<details>
	<summary>See more</summary>
	
	proceed

	^ self ifOkToChangeCodePaneDo: [ model proceed ]
</details>

#### DebuggerWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent
	^ RealEstateAgent standardWindowExtent * 3 // 2
</details>

#### DebuggerWindow>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	model ifNotNil: [ model windowIsClosing ].
	super delete
</details>

#### DebuggerWindow>>#restart

<details>
	<summary>See more</summary>
	
	restart 
	
	^ self ifOkToChangeCodePaneDo: [ model restart ]
</details>

#### DebuggerWindow>>#contextStackMenu

Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided


<details>
	<summary>See more</summary>
	
	contextStackMenu
	"Set up the menu appropriately for the context-stack-list, either shifted or unshifted as per the parameter provided"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'fullStack (f)'.
				#selector 		-> 		#fullStack
			} asDictionary.
			{
				#label 			-> 		'restart (r)'.
				#selector 		-> 		#restart
			} asDictionary.
			{
				#label 			-> 		'proceed (p)'.
				#selector 		-> 		#proceed
			} asDictionary.
			{
				#label 			-> 		'step (t)'.
				#selector 		-> 		#doStep
			} asDictionary.
			{
				#label 			-> 		'step through (T)'.
				#selector 		-> 		#stepIntoBlock
			} asDictionary.
			{
				#label 			-> 		'send (e)'.
				#selector 		-> 		#send
			} asDictionary.
			{
				#label 			-> 		'where (w)'.
				#selector 		-> 		#where
			} asDictionary.
			{
				#label 			-> 		'peel to first like this'.
				#selector 		-> 		#peelToFirst
			} asDictionary.
			nil.
			{
				#label 			-> 		'return entered value'.
				#selector 		-> 		#returnValue
			} asDictionary.
			{
				#label 			-> 		'toggle break on entry'.
				#object 			-> 		#model.
				#selector 		-> 		#toggleBreakOnEntry
			} asDictionary.
		}`.
	^aMenu
</details>

#### DebuggerWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	Preferences optionalButtons ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
</details>

#### DebuggerWindow>>#contextStackKey: aChar from: view

Respond to a keystroke in the context list


<details>
	<summary>See more</summary>
	
	contextStackKey: aChar from: view
	"Respond to a keystroke in the context list"

	aChar == $e ifTrue: [^ self send].
	aChar == $t ifTrue: [^ self doStep].
	aChar == $T ifTrue: [^ self stepIntoBlock].
	aChar == $p ifTrue: [^ self proceed].
	aChar == $r ifTrue: [^ self restart].
	aChar == $f ifTrue: [^ self fullStack].
	aChar == $w ifTrue: [^ self where].

	^ self messageListKey: aChar from: view
</details>

#### DebuggerWindow>>#inspectSelectionIn: anInspector

Create and schedule an Inspector on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	inspectSelectionIn: anInspector
	"Create and schedule an Inspector on the receiver's model's currently selected object."

	^ anInspector selection inspect
</details>

#### DebuggerWindow>>#createMethod

<details>
	<summary>See more</summary>
	
	createMethod

	^ self ifOkToChangeCodePaneDo: [ model createMethod ]
</details>

#### DebuggerWindow>>#send

<details>
	<summary>See more</summary>
	
	send

	^ self ifOkToChangeCodePaneDo: [ model send ]
</details>

#### DebuggerWindow>>#customButtonSpecs

Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger.


<details>
	<summary>See more</summary>
	
	customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send					'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Full Stack'	fullStack			'show full stack')
		('Where'		where				'select current pc range')
		('Create'		createMethod		'create method'))
</details>

#### DebuggerWindow>>#buildMorphicCodePane

Construct the pane that shows the code. Respect the Preference for standardCodeFont.


<details>
	<summary>See more</summary>
	
	buildMorphicCodePane
	
	codePane _ super buildMorphicCodePane.
	^codePane
</details>

#### DebuggerWindow>>#browseFullProtocolIn: anInspector

Spawn a window showing full protocol for the receiver's selection


<details>
	<summary>See more</summary>
	
	browseFullProtocolIn: anInspector
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow
		openFullProtocolForClass: anInspector selectedClassOrMetaClass
</details>

#### DebuggerWindow>>#exploreSelectionIn: anInspector

Create and schedule an Explorer on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	exploreSelectionIn: anInspector
	"Create and schedule an Explorer on the receiver's model's currently selected object."

	^ anInspector selection explore
</details>

#### DebuggerWindow>>#fullStack

<details>
	<summary>See more</summary>
	
	fullStack

	^ self ifOkToChangeCodePaneDo: [ model fullStack ]
</details>

#### DebuggerWindow>>#exploreReceiverSelection

<details>
	<summary>See more</summary>
	
	exploreReceiverSelection
	model receiverInspector selectionIndex = 0 ifTrue: [^ self flash].
	^ model receiverInspector  selection explore
</details>

#### DebuggerWindow>>#ifOkToChangeCodePaneDo: aBlock

<details>
	<summary>See more</summary>
	
	ifOkToChangeCodePaneDo: aBlock

	^self okToChangeCodePane ifTrue: aBlock 

</details>

#### DebuggerWindow>>#contextClassHierarchy

<details>
	<summary>See more</summary>
	
	contextClassHierarchy

	HierarchyBrowserWindow
		onClass: model contextVariablesInspector selectedClassOrMetaClass
		selector: nil
</details>

#### DebuggerWindow>>#inspectorKey: aChar from: view

Respond to a Command key issued while the cursor is over my field list


<details>
	<summary>See more</summary>
	
	inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	aChar == $i ifTrue: [^ self inspectSelectionIn: view model ].
	aChar == $I ifTrue: [^ self exploreSelectionIn: view model ].
	aChar == $b ifTrue:[^ self browseMethodFullIn: view model ].
	aChar == $h ifTrue:[^ self browseHierarchyIn: view model].
	aChar == $p ifTrue: [^ self browseFullProtocolIn: view model].
	aChar == $N ifTrue: [^ self browseClassRefsIn: view model]
</details>

#### DebuggerWindow>>#okToChangeCodePane

<details>
	<summary>See more</summary>
	
	okToChangeCodePane

	| okToLooseChanges |
	
	okToLooseChanges _ self canDiscardEditsOf: codePane.
	okToLooseChanges ifTrue: [
		receiverInspectorText disregardUnacceptedEdits.
		contextVariableInspectorText disregardUnacceptedEdits ].
	
	^okToLooseChanges 
</details>

#### DebuggerWindow>>#receiverFieldListMenu

Arm the supplied menu with items for the field-list of the receiver


<details>
	<summary>See more</summary>
	
	receiverFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreReceiverSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#receiverClassHierarchy
			} asDictionary.
		}`.
	^ aMenu
</details>

#### DebuggerWindow>>#buildMorphicWindow

Open a full morphic debugger with the given label


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Open a full morphic debugger with the given label"

	| bottomMorph |

	stackList _ PluggableListMorph
		model: model 
		listGetter: #contextStackList
		indexGetter: #contextStackIndex
		indexSetter: #toggleContextStackIndex:
		mainView: self
		menuGetter: #contextStackMenu
		keystrokeAction: #contextStackKey:from:.

	receiverInspector _ PluggableListMorph
			model: model receiverInspector
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #receiverFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	receiverInspector doubleClickSelector: #inspectSelection.
	receiverInspectorText _ (TextModelMorph
			textProvider: model receiverInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Receiver scope'.
	contextVariableInspector _ PluggableListMorph
			model: model contextVariablesInspector 
			listGetter: #fieldList
			indexGetter: #selectionIndex 
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #contextFieldListMenu
			keystrokeAction: #inspectorKey:from:.
	contextVariableInspector doubleClickSelector: #inspectSelection.
	contextVariableInspectorText _ (TextModelMorph
			textProvider: model contextVariablesInspector
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection) emptyTextDisplayMessage: 'Context scope'.

	bottomMorph _ LayoutMorph newRow.
	bottomMorph
		addMorph: receiverInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: receiverInspectorText proportionalWidth: 0.3;
		addAdjusterAndMorph: contextVariableInspector proportionalWidth: 0.2;
		addAdjusterAndMorph: contextVariableInspectorText proportionalWidth: 0.3.

	self layoutMorph
		addMorph: stackList proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55;
		addAdjusterAndMorph: bottomMorph proportionalHeight: 0.2
</details>

#### DebuggerWindow>>#browseMethodFullIn: anInspector

Create and schedule a full Browser and then select the current class and message.


<details>
	<summary>See more</summary>
	
	browseMethodFullIn: anInspector
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass _ anInspector selectedClassOrMetaClass) ifNotNil: [
		BrowserWindow fullOnClass: myClass selector: nil]
</details>

#### DebuggerWindow>>#browseClassRefsIn: anInspector

<details>
	<summary>See more</summary>
	
	browseClassRefsIn: anInspector

	anInspector selectedClass ifNotNil: [ :cls | 
		Smalltalk browseAllCallsOnClass: cls theNonMetaClass]
</details>

#### DebuggerWindow>>#okToChangeReceiverInspectorText

<details>
	<summary>See more</summary>
	
	okToChangeReceiverInspectorText
	
	^self canDiscardEditsOf: receiverInspectorText
</details>

#### DebuggerWindow>>#okToChangeContextVariableInspectorText

<details>
	<summary>See more</summary>
	
	okToChangeContextVariableInspectorText
	
	^self canDiscardEditsOf: contextVariableInspectorText
</details>

#### DebuggerWindow>>#closeView

<details>
	<summary>See more</summary>
	
	closeView
	self model: nil.
	self delete
</details>

#### DebuggerWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current debugger
</details>

#### DebuggerWindow>>#doStep

<details>
	<summary>See more</summary>
	
	doStep

	^ self ifOkToChangeCodePaneDo: [ model doStep ]
</details>

#### DebuggerWindow>>#registerNotificationActions

<details>
	<summary>See more</summary>
	
	registerNotificationActions

	super registerNotificationActions.
	model when: #closeViews send: #closeView to: self 
</details>

#### DebuggerWindow>>#contextFieldListMenu

Arm the supplied menu with items for the field-list of the receiver


<details>
	<summary>See more</summary>
	
	contextFieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'explore (I)'.
				#selector 		-> 		#exploreContextSelection
			} asDictionary.
			nil.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 		-> 		#contextClassHierarchy
			} asDictionary.
		}`.
	^ aMenu
</details>

#### DebuggerWindow>>#okToChangeDueTo: aMorph

<details>
	<summary>See more</summary>
	
	okToChangeDueTo: aMorph

	aMorph = stackList ifTrue: [ ^self okToChangeCodePane ].
	aMorph = receiverInspector ifTrue: [ ^self okToChangeReceiverInspectorText ].
	aMorph = contextVariableInspector ifTrue: [ ^self okToChangeContextVariableInspectorText ].
	
	^super okToChangeDueTo: aMorph 
	
	
	
</details>

#### DebuggerWindow>>#optionalButtonTuples

No [show...] button in debugger


<details>
	<summary>See more</summary>
	
	optionalButtonTuples
	"No [show...] button in debugger"
	^super optionalButtonTuples reject: [ :tuple |
		tuple third = #offerWhatToShowMenu ]
</details>

#### DebuggerWindow>>#where

<details>
	<summary>See more</summary>
	
	where

	^ self ifOkToChangeCodePaneDo: [ model where ]
</details>

## FileListWindow

Morphic view for FileList models. See category 'GUI building'.

### Methods
#### FileListWindow>>#morphicDirectoryTreePane

<details>
	<summary>See more</summary>
	
	morphicDirectoryTreePane

	^(HierarchicalListMorph
		model: model
		listGetter: #initialDirectoryList
		indexGetter: #currentDirectorySelected
		indexSetter: #setSelectedDirectoryTo:
		mainView: self
		menuGetter: #volumeMenu
		keystrokeAction: nil)
			autoDeselect: false;
			yourself
</details>

#### FileListWindow>>#buttonToTriggerIn: aFileList for: service

Answer a button that will trigger the receiver service in a file list


<details>
	<summary>See more</summary>
	
	buttonToTriggerIn: aFileList for: service
	"Answer a button that will trigger the receiver service in a file list"

	| aButton |
	service argumentProvider: aFileList.
	aButton := PluggableButtonMorph 
				model: service
				stateGetter: nil
				action: #performService.
	aButton label: service buttonLabel.
	aButton color: self buttonColor.
	aButton setBalloonText: service description.
	^aButton
</details>

#### FileListWindow>>#optionalButtonRow

Answer the button row associated with a file list


<details>
	<summary>See more</summary>
	
	optionalButtonRow
	"Answer the button row associated with a file list"

	| row buttonColor |
	row _ LayoutMorph newRow.
	buttonColor _ self buttonColor.
	row setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	row doAdoptWidgetsColor.
	row color: buttonColor.
	self updateButtonRow: row.
	^row
</details>

#### FileListWindow>>#noFileSelectedMenu

<details>
	<summary>See more</summary>
	
	noFileSelectedMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	^ aMenu
		addServices: model itemsForNoFile 
		for: model
		extraLines: #()
</details>

#### FileListWindow>>#updateButtonRow: row

Dynamically update the contents of the button row, if any.


<details>
	<summary>See more</summary>
	
	updateButtonRow: row
	"Dynamically update the contents of the button row, if any."

	| buttons |
	row removeAllMorphs.
	buttons _ OrderedCollection new.
	model universalButtonServices do: [ :service |
		buttons add: (self buttonToTriggerIn: model for: service) ].
	model dynamicButtonServices do: [ :service | 
		buttons add: (self buttonToTriggerIn: model for: service).
		service when: #fileListChanged send: #updateFileList to: model ].
	row addMorphs: buttons
</details>

#### FileListWindow>>#fileListKey: aChar from: aView

<details>
	<summary>See more</summary>
	
	fileListKey: aChar from: aView

	aChar == $x ifTrue: [ ^ aView model deleteFile ].
	aChar == $R ifTrue: [ ^ aView model renameFile ].
	aChar == $n ifTrue: [ ^ aView model addNewFile ].
	aChar == $N ifTrue: [ ^ aView model addNewDirectory ].
</details>

#### FileListWindow>>#fileListMenu

<details>
	<summary>See more</summary>
	
	fileListMenu
	^model fileName
		ifNil: [ self noFileSelectedMenu ]
		ifNotNil: [ self fileSelectedMenu ]
</details>

#### FileListWindow>>#buildMorphicWindow

<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	| middleRow upperRow |
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: self morphicPatternPane proportionalWidth: 0.3;
		addAdjusterAndMorph: self optionalButtonRow proportionalWidth: 0.7.
	middleRow _ LayoutMorph newRow.
	middleRow
		addMorph: self morphicDirectoryTreePane proportionalWidth: 0.3;
		addAdjusterAndMorph: self morphicFileListPane proportionalWidth: 0.7.
	self layoutMorph
		addMorph: upperRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterAndMorph: middleRow proportionalHeight: 0.4;
		addAdjusterAndMorph: self morphicFileContentsPane proportionalHeight: 0.6.
	self setLabel: model directory pathName.
	model postOpen
</details>

#### FileListWindow>>#morphicFileContentsPane

<details>
	<summary>See more</summary>
	
	morphicFileContentsPane

	^TextModelMorph
		textProvider: model
		textGetter: #acceptedContents 
		textSetter: #put:
		selectionGetter: #contentsSelection
</details>

#### FileListWindow>>#volumeMenu

<details>
	<summary>See more</summary>
	
	volumeMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon.
	^ aMenu
</details>

#### FileListWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current fileList
</details>

#### FileListWindow>>#fileSelectedMenu

<details>
	<summary>See more</summary>
	
	fileSelectedMenu

	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 n1 n2 n3 services aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	itemsPart1 _ model itemsForAnyFile1.
	itemsPart2 _ model itemsForFileEntry: model selectedFileEntry.
	itemsPart3 _ model itemsForAnyFile2.
	itemsPart4 _ model itemsForNoFile.
	n1 _ itemsPart1 size.
	n2 _ n1 + itemsPart2 size.
	n3 _ n2 + itemsPart3 size.
	services _ itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc | svc when: #fileListChanged send: #updateFileList to: model ].
	^ aMenu 
		addServices: services 
		for: model
		extraLines:{ n1 . n2 . n3 }

</details>

#### FileListWindow>>#morphicPatternPane

<details>
	<summary>See more</summary>
	
	morphicPatternPane

	^TextModelMorph
		textProvider: model
		textGetter: #pattern 
		textSetter: #pattern:
</details>

#### FileListWindow>>#updateButtonRow

<details>
	<summary>See more</summary>
	
	updateButtonRow
	| row |
	row _ self 
		findDeepSubmorphThat: [:m | m hasProperty: #buttonRow]
		ifAbsent: [^self].
		
	self updateButtonRow: row
</details>

#### FileListWindow>>#morphicFileListPane

<details>
	<summary>See more</summary>
	
	morphicFileListPane

	^PluggableListMorph
		model: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: 
		mainView: self
		menuGetter: #fileListMenu
		keystrokeAction: #fileListKey:from:
</details>

## HierarchyBrowserWindow

Morphic view for HierarchyBrowser models. See category 'GUI building'.

### Methods
#### HierarchyBrowserWindow>>#systemCatSingletonKey: aChar from: aView

<details>
	<summary>See more</summary>
	
	systemCatSingletonKey: aChar from: aView
	^ self systemCatListKey: aChar from: aView
</details>

## InspectorWindow

A Window built specifically to be the view on an Inspector model object. See category 'GUI building'.

### Methods
#### InspectorWindow>>#addCollectionSpecificMenuOptionsTo: aMenu

<details>
	<summary>See more</summary>
	
	addCollectionSpecificMenuOptionsTo: aMenu

	| object |
	object _ model object.
	(object is: #Dictionary) ifTrue: [
		aMenu addItemsFromDictionaries: self menuOptionsForDictionary ]
	ifFalse: [ (object is: #Set) ifTrue: [
		aMenu addItemsFromDictionaries: self menuOptionsForSet ]]
</details>

#### InspectorWindow>>#fieldListMenu

Arm the supplied menu with items for the field-list of the receiver


<details>
	<summary>See more</summary>
	
	fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addItemsFromDictionaries: self basicMenuOptions.
	self addCollectionSpecificMenuOptionsTo: aMenu.
	aMenu addItemsFromDictionaries: self menuOptionsForBrowsing.
	^ aMenu
</details>

#### InspectorWindow>>#browseHierarchy

Create and schedule a class list browser on the receiver's hierarchy.


<details>
	<summary>See more</summary>
	
	browseHierarchy
	"Create and schedule a class list browser on the receiver's hierarchy."

	HierarchyBrowserWindow
		onClass: model selectedClassOrMetaClass
		selector: nil
</details>

#### InspectorWindow>>#openWeightExplorer

Create and schedule a Weight Explorer on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	openWeightExplorer
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model selection
</details>

#### InspectorWindow>>#classDefinitionChangedFrom: oldClass to: newClass

<details>
	<summary>See more</summary>
	
	classDefinitionChangedFrom: oldClass to: newClass

	model ifNotNil: [ model object class = newClass ifTrue: [ model changed: #fieldList ]]
</details>

#### InspectorWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent

	^`600@325` * Preferences standardCodeFont lineSpacing // 14
</details>

#### InspectorWindow>>#openReferencesFinder

Create and schedule a References Finder on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."

	^ReferenceFinder openExplorerOn: model selection
</details>

#### InspectorWindow>>#sendersOfSelectedKey

<details>
	<summary>See more</summary>
	
	sendersOfSelectedKey
	| key |
	key _ model selectedKey.
	key isString ifFalse: [ ^self ].
	Smalltalk browseAllCallsOn: key
</details>

#### InspectorWindow>>#basicMenuOptions

<details>
	<summary>See more</summary>
	
	basicMenuOptions

	^ `{
		{
			#label 			-> 		'inspect (i)'.
			#selector 		-> 		#inspectSelection.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'explore (I)'.
			#selector 		-> 		#exploreSelection.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'copy to clipboard (c)'.
			#selector 		-> 		#copySelectionToClipboard.
			#icon 			-> 		#copyIcon
		} asDictionary.
		{
			#label 			-> 		'basic inspect'.
			#selector 		-> 		#inspectBasic.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'references finder'.
			#selector 		-> 		#openReferencesFinder.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'weight explorer'.
			#selector 		-> 		#openWeightExplorer.
			#icon 			-> 		#exploreIcon
		} asDictionary.
	}`
</details>

#### InspectorWindow>>#inspectSelection

Create and schedule an Inspector on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	inspectSelection
	"Create and schedule an Inspector on the receiver's model's currently selected object."

	^ model selection inspect
</details>

#### InspectorWindow>>#copySelectionToClipboard

For example, for pasting a reference in a Workspace


<details>
	<summary>See more</summary>
	
	copySelectionToClipboard
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model selection
</details>

#### InspectorWindow>>#model: aModel

Set my model and make me me a dependent of the given object.


<details>
	<summary>See more</summary>
	
	model: aModel

	super model: aModel.
	model ifNotNil: [ 
		SystemChangeNotifier uniqueInstance
			when: #classDefinitionChanged send: #classDefinitionChangedFrom:to: to: self ]

</details>

#### InspectorWindow>>#buildEvaluatorText

<details>
	<summary>See more</summary>
	
	buildEvaluatorText

	| evaluatorText |
	
	evaluatorText _ (TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression'.
	
	^evaluatorText 
</details>

#### InspectorWindow>>#buildList

<details>
	<summary>See more</summary>
	
	buildList

	| list |
	
	list _ PluggableListMorph
			model: model 
			listGetter: #fieldList
			indexGetter: #selectionIndex
			indexSetter: #toggleIndex:
			mainView: self
			menuGetter: #fieldListMenu
			keystrokeAction: #inspectorKey:from:.
	list doubleClickSelector: #inspectSelection.
	
	^list
</details>

#### InspectorWindow>>#browseMethodFull

Create and schedule a full Browser and then select the current class and message.


<details>
	<summary>See more</summary>
	
	browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass _ model selectedClassOrMetaClass) ifNotNil: [
		BrowserWindow fullOnClass: myClass selector: nil]
</details>

#### InspectorWindow>>#menuOptionsForDictionary

<details>
	<summary>See more</summary>
	
	menuOptionsForDictionary

	^ `{
		nil.
		{
			#label 			-> 		'inspect key'.
			#selector 		-> 		#inspectSelectedKey.
			#icon 			-> 		#findIcon
		} asDictionary.
		{
			#label 			-> 		'senders of this key'.
			#selector 		-> 		#sendersOfSelectedKey.
			#icon 			-> 		#mailForwardIcon
		} asDictionary.
		{
			#label 			-> 		'add key'.
			#selector 		-> 		#addEntry.
			#icon 			-> 		#listAddIcon
		} asDictionary.
		{
			#label 			-> 		'rename key'.
			#selector 		-> 		#renameEntry.
			#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 		-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`
</details>

#### InspectorWindow>>#menuOptionsForSet

<details>
	<summary>See more</summary>
	
	menuOptionsForSet

	^ `{
		nil.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 		-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`
</details>

#### InspectorWindow>>#browseClassRefs

<details>
	<summary>See more</summary>
	
	browseClassRefs

	model selectedClass ifNotNil: [ :cls | 
		Smalltalk browseAllCallsOnClass: cls theNonMetaClass]
</details>

#### InspectorWindow>>#renameEntry

<details>
	<summary>See more</summary>
	
	renameEntry
	| string newKey |

	string _ FillInTheBlankMorph request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		 initialAnswer: model selectedKey printString.

	string = '' ifTrue: [
		^self ].

	newKey _ Compiler evaluate: string.

	model renameEntryTo: newKey
</details>

#### InspectorWindow>>#addEntry

<details>
	<summary>See more</summary>
	
	addEntry

	| newKey string |
	string _ FillInTheBlankMorph request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'.
	newKey _ Compiler evaluate: string.
	model addEntry: newKey
</details>

#### InspectorWindow>>#menuOptionsForBrowsing

<details>
	<summary>See more</summary>
	
	menuOptionsForBrowsing

	^ `{
		nil.
		{
			#label 			-> 		'browse full (b)'.
			#selector 		-> 		#browseMethodFull.
			#icon 			-> 		#editFindReplaceIcon
		} asDictionary.
		{
			#label 			-> 		'browse hierarchy (h)'.
			#selector 		-> 		#browseHierarchy.
			#icon 			-> 		#goTopIcon
		} asDictionary.
		{
			#label 			-> 		'browse protocol (p)'.
			#selector 		-> 		#browseFullProtocol.
			#icon 			-> 		#spreadsheetIcon
		} asDictionary.
	}`
</details>

#### InspectorWindow>>#inspectorKey: aChar from: view

Respond to a Command key issued while the cursor is over my field list


<details>
	<summary>See more</summary>
	
	inspectorKey: aChar from: view
	"Respond to a Command key issued while the cursor is over my field list"

	aChar == $i ifTrue: [^ self inspectSelection].
	aChar == $I ifTrue: [^ self exploreSelection].
	aChar == $b ifTrue:[^ self browseMethodFull].
	aChar == $h ifTrue:[^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].
	aChar == $N ifTrue: [^ self browseClassRefs]
</details>

#### InspectorWindow>>#inspectSelectedKey

<details>
	<summary>See more</summary>
	
	inspectSelectedKey

	^ model selectedKey inspect
</details>

#### InspectorWindow>>#buildMorphicWindow

Inspector openOn: SystemOrganization


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	" Inspector openOn: SystemOrganization "
	| contentsText list upperRow evaluatorText label |

	"Build widgets. We'll assemble them below."
	list _ self buildList.
	contentsText _ self buildContentsText.
	evaluatorText _ self buildEvaluatorText.
	
	"Upper row has two widgets, side by side: the list of variables at the left side, and the variable contents pane at the right side."
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list proportionalWidth: 0.3;
		addAdjusterAndMorph: contentsText proportionalWidth: 0.7.
	"Inspector Window has two rows: At the top, the one we just bult. Below it, the evaluation pane."
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.7;
		addAdjusterAndMorph: evaluatorText proportionalHeight: 0.3.
	"Set label"
	label _ [model object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: model object class printString, ' (printing failed)'].
	(label includesSubString: model object  class name)
		ifFalse: [label _ model object  class name, ': ', label].
	self setLabel: label
</details>

#### InspectorWindow>>#inspectBasic

Create and schedule a basic Inspector on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	inspectBasic
	"Create and schedule a basic Inspector on the receiver's model's currently selected object."

	^ model selection basicInspect
</details>

#### InspectorWindow>>#browseFullProtocol

Spawn a window showing full protocol for the receiver's selection


<details>
	<summary>See more</summary>
	
	browseFullProtocol
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClassOrMetaClass
</details>

#### InspectorWindow>>#buildContentsText

<details>
	<summary>See more</summary>
	
	buildContentsText

	| contentsText |

	contentsText _ TextModelMorph
			textProvider: model
			textGetter: #acceptedContents 
			textSetter: #accept:
			selectionGetter: #contentsSelection.
			
	^contentsText
</details>

#### InspectorWindow>>#exploreSelection

Create and schedule an Explorer on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	exploreSelection
	"Create and schedule an Explorer on the receiver's model's currently selected object."

	^ model selection explore
</details>

## MessageNamesWindow

Morphic view for MessageNames models. See category 'GUI building'.

### Methods
#### MessageNamesWindow>>#submorphToFocusKeyboard

Might answer nil


<details>
	<summary>See more</summary>
	
	submorphToFocusKeyboard
	^textMorph textMorph
</details>

#### MessageNamesWindow>>#buildMorphicWindow

Answer a morphic window with the given initial search string, nil if none


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Answer a morphic window with the given initial search string, nil if none"
	"MessageNames openMessageNames"
	| selectorListView firstRow searchButton secondRow |
	textMorph _ (TextModelMorph
		textProvider: model
		textGetter: #searchString
		textSetter: #searchString:
		selectionGetter: #contentsSelection)
		setBalloonText: 'See MessageNames class comment for search string options';
		emptyTextDisplayMessage: 'Type here, then hit Search'.
	textMorph askBeforeDiscardingEdits: false.
	textMorph acceptOnCR: true.
	textMorph hideScrollBarsIndefinitely.
	searchButton _ PluggableButtonMorph new
		model: textMorph textMorph;
		label: 'Search';
		action: #acceptContents.
	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.
	firstRow _ LayoutMorph newRow.
	firstRow color: self windowColor.
	firstRow
		doAdoptWidgetsColor;
		
			addMorph: searchButton
			proportionalWidth: 0.25;
		
			addMorph: textMorph
			proportionalWidth: 0.75.
	selectorListView _ PluggableListMorph
		model: model
		listGetter: #selectorList
		indexGetter: #selectorListIndex
		indexSetter: #selectorListIndex:
		mainView: self
		menuGetter: #selectorListMenu
		keystrokeAction: #selectorListKey:from:.
	secondRow _ LayoutMorph newRow.
	secondRow
		
			addMorph: selectorListView
			proportionalWidth: 0.5;
		
			addAdjusterAndMorph: self buildMorphicMessageList
			proportionalWidth: 0.5.
	self layoutMorph
		
			addMorph: firstRow
			fixedHeight: self defaultButtonPaneHeight + 4;
		
			addAdjusterAndMorph: secondRow
			proportionalHeight: 0.5;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.5.
	model changed: #editSelection.
</details>

#### MessageNamesWindow>>#selectorListKey: aChar from: view

Respond to a Command key in the message-list pane.


<details>
	<summary>See more</summary>
	
	selectorListKey: aChar from: view
	"Respond to a Command key in the message-list pane."
	aChar == $n ifTrue: [ ^ self browseSenders ].
	aChar == $b ifTrue: [ ^ self browseMethodFull ].
	aChar == $m ifTrue: [ ^ self browseImplementors ].
</details>

#### MessageNamesWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current messageNames
</details>

#### MessageNamesWindow>>#selectorListMenu

Answer the menu associated with the selectorList


<details>
	<summary>See more</summary>
	
	selectorListMenu
	"Answer the menu associated with the selectorList"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	(aMenu add: 'senders (n)' action: #browseSenders icon: #mailForwardIcon)
		setBalloonText: 'browse senders of the chosen selector'.
	^ aMenu
</details>

## MessageSetWindow

Morphic view for MessageSet models. See category 'GUI building'.

### Methods
#### MessageSetWindow>>#addExtraMenu2ItemsTo: options

The shifted selector-list menu is being built. Add items specific to MessageSet


<details>
	<summary>See more</summary>
	
	addExtraMenu2ItemsTo: options
	"The shifted selector-list menu is being built.  Add items specific to MessageSet"

	model growable ifTrue: [
		options add: `{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		31.		
			#label 			-> 		'remove from this browser'.
			#object 			-> 		#model.
			#selector 		-> 		#removeMessageFromBrowser.
			#icon 			-> 		#listRemoveIcon 
		} asDictionary`.
		options add:`{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		32.		
			#label 			-> 		'filter message list...'.
			#selector 		-> 		#filterMessageList.
			#icon 			-> 		#findIcon
		} asDictionary`	].
	
	options add: `{
			#itemGroup 		-> 		40.
			#itemOrder 		-> 		33.		
			#label 			-> 		'sort by date'.
			#object 			-> 		#model.	
			#selector 		-> 		#sortByDate.
			#icon 			-> 		#dateIcon
		} asDictionary`.


</details>

#### MessageSetWindow>>#buildMorphicWindow

Answer a morphic window with the given label that can display the receiver


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection
</details>

#### MessageSetWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations
</details>

#### MessageSetWindow>>#activateAndSendTopToBack: aBoolean

Bring me to the front and make me able to respond to mouse and keyboard


<details>
	<summary>See more</summary>
	
	activateAndSendTopToBack: aBoolean
	super activateAndSendTopToBack: aBoolean.
	(model messageListIndex = 0 and: [ model messageList notEmpty ])
		ifTrue: [	
			model messageListIndex: 1 ]
</details>

#### MessageSetWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current messageSet
</details>

#### MessageSetWindow>>#filterMessageList

Allow the user to refine the list of messages.


<details>
	<summary>See more</summary>
	
	filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [ ^self inform: 'this is not a propitious filtering situation' ].

	aMenu _ MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		add: 'unsent messages' action: #filterToUnsentMessages balloonText: 'filter to show only messages that have no senders';
		addLine;
		add: 'messages that send...' action: #filterToSendersOf balloonText: 'filter to show only messages that send a selector I specify';
		add: 'messages that do not send...' action: #filterToNotSendersOf balloonText: 'filter to show only messages that do not send a selector I specify';
		addLine;
		add: 'messages whose selector is...' action: #filterToImplementorsOf balloonText: 'filter to show only messages with a given selector I specify';
		add: 'messages whose selector is NOT...' action: #filterToNotImplementorsOf balloonText: 'filter to show only messages whose selector is NOT a seletor I specify';
		addLine;
		add: 'messages in any change set' action: #filterToAnyChangeSet balloonText: 'filter to show only messages that occur in at least one change set';
		add: 'messages not in any change set' action: #filterToNotAnyChangeSet balloonText: 'filter to show only messages that do not occur in any change set in the system';
		addLine;
		add: 'messages authored by me' action: #filterToCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp has my initials';
		add: 'messages not authored by me' action: #filterToNotCurrentAuthor balloonText: 'filter to show only messages whose authoring stamp does not have my initials';
		addLine;
		add: 'messages logged in .changes file' action: #filterToMessagesInChangesFile balloonText: 'filter to show only messages whose latest source code is logged in the .changes file';
		add: 'messages only in .sources file' action: #filterToMessagesInSourcesFile balloonText: 'filter to show only messages whose latest source code is logged in the .sources file';
		addLine;
		add: 'messages with prior versions' action: #filterToMessagesWithPriorVersions balloonText: 'filter to show only messages that have at least one prior version';
		add: 'messages without prior versions' action: #filterToMessagesWithoutPriorVersions balloonText: 'filter to show only messages that have no prior versions';
		addLine;
		add: 'uncommented messages' action: #filterToUncommentedMethods balloonText: 'filter to show only messages that do not have comments at the beginning';
		add: 'commented messages' action: #filterToCommentedMethods balloonText: 'filter to show only messages that have comments at the beginning'.
	aMenu popUpInWorld: self world
</details>

## ObjectExplorerWindow

Morphic view for ObjectExplorer models. See category 'GUI building'.

### Methods
#### ObjectExplorerWindow>>#explorerKey: aChar from: view

<details>
	<summary>See more</summary>
	
	explorerKey: aChar from: view

	model getCurrentSelection ifNotNil: [

		aChar == $i ifTrue: [^ self inspectSelection].
		aChar == $I ifTrue: [^ self exploreSelection].

		aChar == $b ifTrue: [^ self browseMethodFull].
		aChar == $h ifTrue: [^ self browseHierarchy].
		aChar == $p ifTrue: [^ self browseFullProtocol].
		aChar == $N ifTrue: [^ self browseClassRefs]]
</details>

#### ObjectExplorerWindow>>#browseHierarchy

Create and schedule a class list browser on the receiver's hierarchy.


<details>
	<summary>See more</summary>
	
	browseHierarchy
	"Create and schedule a class list browser on the receiver's hierarchy."

	HierarchyBrowserWindow
		onClass: model selectedClass
		selector: nil
</details>

#### ObjectExplorerWindow>>#rescan

<details>
	<summary>See more</summary>
	
	rescan

	| objectsToSkip |
	objectsToSkip := self objectsToSkipDuringRescan.
	model noteNewSelection: nil.
	self model rescanSkipping: objectsToSkip.
	self expandAll
</details>

#### ObjectExplorerWindow>>#browseMethodFull

Create and schedule a full Browser and then select the current class and message.


<details>
	<summary>See more</summary>
	
	browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass _ model selectedClass) ifNotNil: [
		BrowserWindow fullOnClass: myClass selector: nil]
</details>

#### ObjectExplorerWindow>>#expandAll

<details>
	<summary>See more</summary>
	
	expandAll

	| firstSelection list |
	list _ model getList.
	firstSelection _ list isEmpty ifFalse: [ list first ].
	model noteNewSelection: firstSelection.
	listMorph expandAll
</details>

#### ObjectExplorerWindow>>#openWeightExplorer

Create and schedule a Weight Explorer on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	openWeightExplorer
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model object
</details>

#### ObjectExplorerWindow>>#browseClassRefs

<details>
	<summary>See more</summary>
	
	browseClassRefs
	Smalltalk browseAllCallsOnClass: model object class theNonMetaClass
</details>

#### ObjectExplorerWindow>>#stepAt: millisecondSinceLast

If the receiver is not collapsed, step it, after first stepping the model.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	super stepAt: millisecondSinceLast.	
	model monitorList isEmpty ifTrue: [
		self stopStepping ]
</details>

#### ObjectExplorerWindow>>#stopMonitoring

<details>
	<summary>See more</summary>
	
	stopMonitoring

	model stopMonitoring.
	self stopStepping
</details>

#### ObjectExplorerWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent

	^`300@500` * Preferences standardCodeFont lineSpacing // 14
</details>

#### ObjectExplorerWindow>>#openReferencesFinder

Create and schedule a References Finder on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	openReferencesFinder
	"Create and schedule a References Finder on the receiver's model's currently selected object."
	
	ReferenceFinder openExplorerOn: model object
</details>

#### ObjectExplorerWindow>>#buildMorphicWindow

<details>
	<summary>See more</summary>
	
	buildMorphicWindow

	| textMorph |
	listMorph _ HierarchicalListMorph
			model: model
			listGetter: #getList
			indexGetter: #getCurrentSelection
			indexSetter: #noteNewSelection:
			mainView: self
			menuGetter: #genericMenu
			keystrokeAction: #explorerKey:from:.
	listMorph autoDeselect: false.
	listMorph doubleClickSelector: #inspectSelection.
	textMorph _ ((TextModelMorph textProvider: model)
			askBeforeDiscardingEdits: false;
			emptyTextDisplayMessage: 'Smalltalk expression (self is selected item)').
	self layoutMorph
		addMorph: listMorph proportionalHeight: 0.8;
		addAdjusterAndMorph: textMorph proportionalHeight: 0.2.
	self setLabel: (model rootObject printStringLimitedTo: 64)
</details>

#### ObjectExplorerWindow>>#objectsToSkipDuringRescan

Avoid observing side effects of the reference finder itself


<details>
	<summary>See more</summary>
	
	objectsToSkipDuringRescan
	"Avoid observing side effects of the reference finder itself"

	| answer |
	answer := OrderedCollection
		with: self
		with: self model
		with: self model getCurrentSelection.
	self allMorphsDo: [:each | answer add: each].
	^answer
</details>

#### ObjectExplorerWindow>>#inspectBasic

Create and schedule a basic Inspector on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	inspectBasic
	"Create and schedule a basic Inspector on the receiver's model's currently selected object."

	^ model object basicInspect
</details>

#### ObjectExplorerWindow>>#inspectSelection

Create and schedule an Inspector on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	inspectSelection
	"Create and schedule an Inspector on the receiver's model's currently selected object."

	^ model object inspect
</details>

#### ObjectExplorerWindow>>#browseFullProtocol

Spawn a window showing full protocol for the receiver's selection


<details>
	<summary>See more</summary>
	
	browseFullProtocol
	"Spawn a window showing full protocol for the receiver's selection"

	ProtocolBrowserWindow openFullProtocolForClass: model selectedClass
</details>

#### ObjectExplorerWindow>>#copySelectionToClipboard

For example, for pasting a reference in a Workspace


<details>
	<summary>See more</summary>
	
	copySelectionToClipboard
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model object
</details>

#### ObjectExplorerWindow>>#genericMenu

Borrow a menu from my inspector


<details>
	<summary>See more</summary>
	
	genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				action: #yourself]
		ifNotNil: [
			aMenu
				addItemsFromDictionaries: `{
					{
						#label 			-> 		'inspect (i)'.
						#selector 		-> 		#inspectSelection.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'explore (I)'.
						#selector 		-> 		#exploreSelection.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'copy to clipboard (c)'.
						#selector 		-> 		#copySelectionToClipboard.
						#icon 			-> 		#copyIcon
					} asDictionary.
					{
						#label 			-> 		'basic inspect'.
						#selector 		-> 		#inspectBasic.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'references finder'.
						#selector 		-> 		#openReferencesFinder.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'weight explorer'.
						#selector 		-> 		#openWeightExplorer.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					nil.
					{
						#label 			-> 		'browse full (b)'.
						#selector 		-> 		#browseMethodFull.
						#icon 			-> 		#editFindReplaceIcon
					} asDictionary.
					{
						#label 			-> 		'browse hierarchy (h)'.
						#selector 		-> 		#browseHierarchy.
						#icon 			-> 		#goTopIcon
					} asDictionary.
					{
						#label 			-> 		'browse protocol (p)'.
						#selector 		-> 		#browseFullProtocol.
						#icon 			-> 		#spreadsheetIcon
					} asDictionary.
				}`.
			aMenu addLine;
				add: 'monitor changes'
				target: self
				action: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					action: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				action: #stopMonitoring ].
	^ aMenu
</details>

#### ObjectExplorerWindow>>#exploreSelection

Create and schedule an Explorer on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	exploreSelection
	"Create and schedule an Explorer on the receiver's model's currently selected object."

	^ model object explore
</details>

#### ObjectExplorerWindow>>#monitor: anObjectExplorerWrapper

Start stepping and watching the given wrapper for changes.


<details>
	<summary>See more</summary>
	
	monitor: anObjectExplorerWrapper
	"Start stepping and watching the given wrapper for changes."

	anObjectExplorerWrapper ifNotNil: [
		model monitorList at: anObjectExplorerWrapper put: anObjectExplorerWrapper asString.
		self startSteppingStepTime: 200 ]
</details>

## PreDebugWindow

Morphic view for Debugger models. See category 'GUI building'.

### Methods
#### PreDebugWindow>>#debug

Open a full DebuggerView.


<details>
	<summary>See more</summary>
	
	debug
	"Open a full DebuggerView."
	| m |
	m _ model.
	self closeView.
	m openFullMorphicLabel: self label
</details>

#### PreDebugWindow>>#proceed

Proceed execution of the receiver's model, starting after the expression at which an interruption occurred.


<details>
	<summary>See more</summary>
	
	proceed
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	| m |
	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		m _ model.
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self closeView.
		"Resume instead"
		m resumeProcess ]
</details>

#### PreDebugWindow>>#initialExtent

<details>
	<summary>See more</summary>
	
	initialExtent
	^ `640 @ 320` * Preferences standardCodeFont lineSpacing // 14
</details>

#### PreDebugWindow>>#buttonRowForPreDebugWindow: aDebugger

<details>
	<summary>See more</summary>
	
	buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	aDebugger shouldBeAbleToCreateMethod ifTrue: [ triads add: { 'Create'. #createMethod. 'create the missing method' }].
	
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: `Color transparent`.
	row separation: 1.
	row addMorphs: buttons.
	^row
</details>

#### PreDebugWindow>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	model ifNotNil: [ model windowIsClosing ].
	super delete
</details>

#### PreDebugWindow>>#abandon

abandon the debugger from its pre-debug notifier


<details>
	<summary>See more</summary>
	
	abandon
	"abandon the debugger from its pre-debug notifier"
	self delete
</details>

#### PreDebugWindow>>#buildMorphicWindowMessage: messageString

<details>
	<summary>See more</summary>
	
	buildMorphicWindowMessage: messageString
	| notifyPane |
	model expandStack.
	messageString
		ifNil: [
			notifyPane _ PluggableListMorph
				model: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: #openFullAt:
				mainView: self
				menuGetter: nil
				keystrokeAction: nil ]
		ifNotNil: [
			notifyPane _ TextModelMorph withText: messageString.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8
</details>

#### PreDebugWindow>>#closeView

<details>
	<summary>See more</summary>
	
	closeView
	self model: nil.
	self delete
</details>

#### PreDebugWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current debugger
</details>

#### PreDebugWindow>>#registerNotificationActions

<details>
	<summary>See more</summary>
	
	registerNotificationActions

	super registerNotificationActions.
	model when: #closeViews send: #closeView to: self 
</details>

#### PreDebugWindow>>#createMethod

Should only be called when this Debugger was created in response to a MessageNotUnderstood exception. Create a stub for the method that was missing and proceed into it.


<details>
	<summary>See more</summary>
	
	createMethod
	"Should only be called when this Debugger was created in response to a
	MessageNotUnderstood exception. Create a stub for the method that was
	missing and proceed into it."
	
	model createMethod.
	self debug

</details>

#### PreDebugWindow>>#preDebugButtonSpec

<details>
	<summary>See more</summary>
	
	preDebugButtonSpec

	^{
		{'Proceed'.		#proceed. 	'continue execution' }.
		{'Abandon'.		#abandon. 	'abandon this execution by closing this window' }.
		{'Debug'.		#debug.		'bring up a debugger' }
	}
</details>

## ProcessBrowserWindow

Morphic view for ProcessBrowser models. See category 'GUI building'.

### Methods
#### ProcessBrowserWindow>>#exploreContext

<details>
	<summary>See more</summary>
	
	exploreContext
	model selectedContext explore
</details>

#### ProcessBrowserWindow>>#inspectProcess

<details>
	<summary>See more</summary>
	
	inspectProcess
	model selectedProcess inspect
</details>

#### ProcessBrowserWindow>>#inspectContext

<details>
	<summary>See more</summary>
	
	inspectContext
	model selectedContext inspect
</details>

#### ProcessBrowserWindow>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	model stopCPUWatcher.
	super delete
</details>

#### ProcessBrowserWindow>>#openReferencesFinder

Create and schedule a References Finder on the receiver's model's currently selected object.


<details>
	<summary>See more</summary>
	
	openReferencesFinder

	"Create and schedule a References Finder on the receiver's model's currently selected object."
	model selectedProcess ifNotNil: [ :pr | ReferenceFinder openExplorerOn: pr]
</details>

#### ProcessBrowserWindow>>#processListKey: aKey from: aView

<details>
	<summary>See more</summary>
	
	processListKey: aKey from: aView 
	^ aKey caseOf: {
		[$i] -> [self inspectProcess].
		[$I] -> [self exploreProcess].
		[$P] -> [self openReferencesFinder].
		[$t] -> [model terminateProcess].
		[$r] -> [model resumeProcess].
		[$s] -> [model suspendProcess].
		[$d] -> [self debugProcess].
		[$p] -> [self changePriority].
		[$f] -> [self findContext].
		[$g] -> [model nextContext].
		[$a] -> [self toggleAutoUpdate].
		[$u] -> [model updateProcessList].
		[$S] -> [model signalSemaphore].
		[$k] -> [model moreStack]}
		 otherwise: []
</details>

#### ProcessBrowserWindow>>#stackListMenu

<details>
	<summary>See more</summary>
	
	stackListMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model selectedContext
		ifNil: [^ aMenu].
	aMenu
		add: 'inspect context (c)' 		action: #inspectContext icon: #inspectIcon;
		add: 'explore context (C)' 		action: #exploreContext icon: #exploreIcon;
		add: 'inspect receiver (i)' 		action: #inspectReceiver icon: #inspectIcon;
		add: 'explore receiver (I)' 		action: #exploreReceiver icon: #exploreIcon;
		add: 'browse (b)' 				action: #browseContext icon: #editFindReplaceIcon.
	^aMenu
</details>

#### ProcessBrowserWindow>>#startStackSizeWatcher

<details>
	<summary>See more</summary>
	
	startStackSizeWatcher

	model startStackSizeWatcher
</details>

#### ProcessBrowserWindow>>#stepTime

Update 5 times per second.


<details>
	<summary>See more</summary>
	
	stepTime
	"Update 5 times per second."
	^ 200
</details>

#### ProcessBrowserWindow>>#processListMenu

<details>
	<summary>See more</summary>
	
	processListMenu
	| menu rules |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules _ model class rulesFor: model selectedProcess.
			menu
				add: 'inspect (i)'				action: #inspectProcess 				icon: #inspectIcon;
				add: 'explore (I)'				action: #exploreProcess 				icon: #exploreIcon;
				add: 'references finder'	action: #openReferencesFinder 		icon: #exploreIcon.
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess icon: #cancelIcon.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess icon: #mediaPlaybackStartIcon]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess icon: #chatIcon]].
			rules second
				ifTrue: [
					menu
						add: 'change priority (p)'	action: #changePriority 	icon: #systemMonitorIcon;
						add: 'debug (d)'				action: #debugProcess 	icon: #debugIcon ].
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore icon: #haloHelpIcon ].
			menu add: 'full stack (k)' target: model action: #moreStack icon: #systemIcon.
			menu addLine].

	menu
		add: 'find context... (f)'					action: #findContext 	icon: #findIcon;
		add: 'find again (g)'	target: model 	action: #nextContext 	icon: #systemIcon.
	menu addLine.

	isStepping
		ifTrue: [ 	menu add: 'turn off auto-update (a)' 	action: #toggleAutoUpdate icon: #cancelIcon ]
		ifFalse: [ 	menu add: 'turn on auto-update (a)' 	action: #toggleAutoUpdate icon: #updateIcon ].
		
	menu add: 'update list (u)' target: model action: #updateProcessList icon: #updateIcon.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher icon: #inspectIcon ].
	StackSizeWatcher isWatching
			ifTrue: [ menu add: 'stop StackSizeWatcher' action: #stopStackSizeWatcher icon: #inspectIcon ]
			ifFalse: [ menu add: 'start StackSizeWatcher' action: #startStackSizeWatcher  icon: #inspectIcon ].

	^ menu
</details>

#### ProcessBrowserWindow>>#browseContext

<details>
	<summary>See more</summary>
	
	browseContext
	model selectedContext
		ifNil: [^ self].
	BrowserWindow fullOnClass: model selectedClass selector: model selectedSelector
</details>

#### ProcessBrowserWindow>>#stopStackSizeWatcher

<details>
	<summary>See more</summary>
	
	stopStackSizeWatcher
	model stopStackSizeWatcher
</details>

#### ProcessBrowserWindow>>#toggleAutoUpdate

<details>
	<summary>See more</summary>
	
	toggleAutoUpdate
	isStepping
		ifTrue: [ self stopAutoUpdate ]
		ifFalse: [ self startAutoUpdate ].

</details>

#### ProcessBrowserWindow>>#initialize

Initialize a system window. Add label, stripes, etc., if desired


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	isStepping _ false.
</details>

#### ProcessBrowserWindow>>#startAutoUpdate

<details>
	<summary>See more</summary>
	
	startAutoUpdate
	self startStepping.
	isStepping _ true
</details>

#### ProcessBrowserWindow>>#stackListKey: aKey from: aView

<details>
	<summary>See more</summary>
	
	stackListKey: aKey from: aView 
	^ aKey caseOf: {
		[$c] -> [self inspectContext].
		[$C] -> [self exploreContext].
		[$i] -> [self inspectReceiver].
		[$I] -> [self exploreReceiver].
		[$b] -> [self browseContext]}
		 otherwise: []
</details>

#### ProcessBrowserWindow>>#stopAutoUpdate

<details>
	<summary>See more</summary>
	
	stopAutoUpdate
	self stopStepping.
	isStepping _ false
</details>

#### ProcessBrowserWindow>>#openInWorld

Ensure all widgets have proper colors before opening


<details>
	<summary>See more</summary>
	
	openInWorld
	
	super openInWorld.
	self startAutoUpdate.
	self startCPUWatcher
</details>

#### ProcessBrowserWindow>>#stopCPUWatcher

<details>
	<summary>See more</summary>
	
	stopCPUWatcher
	model stopCPUWatcher
</details>

#### ProcessBrowserWindow>>#buildMorphicWindow

Create a pluggable version of me, answer a window


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Create a pluggable version of me, answer a window"
	| aTextMorph list1 list2 upperRow |
	list1 _ PluggableListMorph
				model: model
				listGetter: #processNameList
				indexGetter: #processListIndex
				indexSetter: #processListIndex:
				mainView: self
				menuGetter: #processListMenu
				keystrokeAction: #processListKey:from:.
	list2 _ PluggableListMorph
				model: model
				listGetter: #stackNameList
				indexGetter: #stackListIndex
				indexSetter: #stackListIndex:
				mainView: self
				menuGetter: #stackListMenu
				keystrokeAction: #stackListKey:from:.
	upperRow _ LayoutMorph newRow.
	upperRow
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.
	aTextMorph _ (TextModelMorph
				textProvider: model
				textGetter: #selectedMethod) emptyTextDisplayMessage: 'Method source (not selected?)'.
	aTextMorph askBeforeDiscardingEdits: false.
	self layoutMorph
		addMorph: upperRow proportionalHeight: 0.5;
		addAdjusterAndMorph: aTextMorph proportionalHeight: 0.5.
	self setLabel: 'Process Browser'
</details>

#### ProcessBrowserWindow>>#inspectReceiver

<details>
	<summary>See more</summary>
	
	inspectReceiver
	model selectedContext
		ifNotNil: [ :context | context receiver inspect]
</details>

#### ProcessBrowserWindow>>#debugProcess

<details>
	<summary>See more</summary>
	
	debugProcess
	| rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t debug ' , model selectedProcess name.
			^ self].
	model class debugProcess: model selectedProcess.
</details>

#### ProcessBrowserWindow>>#findContext

<details>
	<summary>See more</summary>
	
	findContext
	| searchString |
	searchString _ FillInTheBlankMorph request: 'Enter a string to search for in the process stack lists' initialAnswer: model searchString.
	model findContext: searchString
</details>

#### ProcessBrowserWindow>>#changePriority

<details>
	<summary>See more</summary>
	
	changePriority
	| str newPriority rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	str _ FillInTheBlankMorph request: 'New priority' initialAnswer: model selectedProcess priority asString.
	newPriority _ str asNumber asInteger.
	newPriority
		ifNil: [^ self].
	(newPriority < 1
			or: [newPriority > Processor highestPriority])
		ifTrue: [PopUpMenu inform: 'Bad priority'.
			^ self].
	model class setProcess: model selectedProcess toPriority: newPriority.
	model updateProcessList
</details>

#### ProcessBrowserWindow>>#exploreReceiver

<details>
	<summary>See more</summary>
	
	exploreReceiver
	model selectedContext
		ifNotNil: [
			BrowserWindow
				fullOnClass: model selectedClass
				selector: model selectedSelector ]
</details>

#### ProcessBrowserWindow>>#startCPUWatcher

<details>
	<summary>See more</summary>
	
	startCPUWatcher

	model startCPUWatcher
</details>

#### ProcessBrowserWindow>>#exploreProcess

<details>
	<summary>See more</summary>
	
	exploreProcess
	model selectedProcess explore
</details>

## ProtocolBrowserWindow

A view of the messages available to a class from itself upward through the class hierarchy. The viewed protocol can be pruned by selecting a superclass in the class hierchy pane.

### Methods
#### ProtocolBrowserWindow>>#buildSelectiveClassList

Define the class hierarchy list pane


<details>
	<summary>See more</summary>
	
	buildSelectiveClassList
	"Define the class hierarchy list pane"

	^PluggableListMorph
		model: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:
</details>

#### ProtocolBrowserWindow>>#buildMorphicWindow

Answer a morphic window that can display the receiver with a class hierarchy


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Answer a morphic window that can display the receiver with a class hierarchy"
	| topRow |

	topRow _ LayoutMorph newRow.
	topRow 
		addMorph: self buildSelectiveClassList proportionalWidth: 0.3;
		addAdjusterMorph;
		addMorph: self buildMorphicMessageList proportionalWidth: 0.7.
	self layoutMorph
		addMorph: topRow proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection
</details>

#### ProtocolBrowserWindow>>#update: aSymbol

Respond to events of the Dependency Mechanism


<details>
	<summary>See more</summary>
	
	update: aSymbol
	"Respond to events of the Dependency Mechanism"
	
	super update: aSymbol.
	aSymbol == #relabel
		ifTrue: [self setLabel: model labelString]
</details>

## TestRunnerWindow

Morphic view for TestRunner models. See category 'GUI building'.

### Methods
#### TestRunnerWindow>>#buildPassFailText

<details>
	<summary>See more</summary>
	
	buildPassFailText
	passFailText _ (TextModelMorph
		textProvider: model
		textGetter: #passFail) emptyTextDisplayMessage: 'Pass/Fail summary (no results to display)'.
	passFailText hideScrollBarsIndefinitely.
	^ passFailText
</details>

#### TestRunnerWindow>>#refreshTests

<details>
	<summary>See more</summary>
	
	refreshTests

	model refreshTests.
	self refreshWindow
</details>

#### TestRunnerWindow>>#refreshWindow

<details>
	<summary>See more</summary>
	
	refreshWindow
	| pc |
	pc _ self widgetsColor.
	passFailText color: pc.
	detailsText color: pc.
	model refreshTR
</details>

#### TestRunnerWindow>>#buildRefreshButton

<details>
	<summary>See more</summary>
	
	buildRefreshButton
	| refreshButton |
	refreshButton := PluggableButtonMorph 
				model: self
				stateGetter: nil
				action: #refreshTests
				label: 'Refresh'.
	refreshButton
		color: self runButtonColor.
	^refreshButton
</details>

#### TestRunnerWindow>>#buildUpperControls

<details>
	<summary>See more</summary>
	
	buildUpperControls
	| refreshButton filterButton stopButton runOneButton runButton runProfiledButton row column1 column2 column3 theTestsList |

	refreshButton _ self buildRefreshButton.
	filterButton _ self buildFilterButton.
	stopButton _ self buildStopButton.
	column1 _ LayoutMorph newColumn.
	column1 doAdoptWidgetsColor.
	column1 addMorphs: { refreshButton . filterButton . stopButton }.

	theTestsList _ PluggableListMorphOfMany
				model: model
				listGetter: #tests
				primarySelectionGetter: #selectedSuite
				primarySelectionSetter: #selectedSuite:
				listSelectionGetter: #listSelectionAt:
				listSelectionSetter: #listSelectionAt:put:
				mainView: self
				menuGetter: #listMenu
				keystrokeAction: nil.
	theTestsList autoDeselect: false.
	theTestsList color: self textBackgroundColor.
	column2 _ LayoutMorph newColumn.
	column2
		addMorph: theTestsList proportionalHeight: 1;
		addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight.

	runOneButton _ self buildRunOneButton.
	runButton _ self buildRunButton.
	runProfiledButton := self buildRunProfiledButton.	
	column3 _ LayoutMorph newColumn.
	column3 doAdoptWidgetsColor.
	column3 addMorphs: { runOneButton . runButton . runProfiledButton }.
	
	row _ LayoutMorph newRow.
	row
		addMorph: column1 proportionalWidth: 0.1;
		addMorph: column2 proportionalWidth: 0.7;
		addMorph: column3 proportionalWidth: 0.2.

	^row


</details>

#### TestRunnerWindow>>#buildRunButton

<details>
	<summary>See more</summary>
	
	buildRunButton
	| runButton |
	runButton := PluggableButtonMorph 
				model: model
				stateGetter: #runButtonState
				action: #runTests
				label: 'Run all selected'.
	runButton
		color: self runButtonColor.
	^runButton
</details>

#### TestRunnerWindow>>#optionalButtonRow

<details>
	<summary>See more</summary>
	
	optionalButtonRow
	| row button buttons widths buttonColor |

	buttons _ OrderedCollection new.
	widths _ OrderedCollection new.
	buttonColor _ self buttonColor.
	self optionalModelButtonTuples do: [ :tuple | 
		widths add: tuple first.
		button _ PluggableButtonMorph 
			model: model
			stateGetter: nil
			action: tuple third.
		button color: buttonColor.
		button
			label: tuple second.
		buttons add: button].
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.
	row addMorphs: buttons widthProportionalTo: widths.
	^row
</details>

#### TestRunnerWindow>>#optionalModelButtonTuples

<details>
	<summary>See more</summary>
	
	optionalModelButtonTuples
	^#(
		#(20	'select all' 				#selectAll) 
		#(20	'deselect all' 			#deselectAll) 
		#(20	'toggle selections' 		#invertSelections)
	)
</details>

#### TestRunnerWindow>>#buildErrorsList

<details>
	<summary>See more</summary>
	
	buildErrorsList
	^ PluggableListMorph
		model: model
		listGetter: #errorsList
		indexGetter: #selectedErrorTest
		indexSetter: #debugErrorTest:
		mainView: self
		menuGetter: nil
		keystrokeAction: nil
</details>

#### TestRunnerWindow>>#updatePartColors: aColor

<details>
	<summary>See more</summary>
	
	updatePartColors: aColor

	passFailText color: aColor.
	 detailsText color: aColor
</details>

#### TestRunnerWindow>>#buildRunProfiledButton

<details>
	<summary>See more</summary>
	
	buildRunProfiledButton
	| runProfiledButton |
	runProfiledButton := PluggableButtonMorph 
				model: model
				stateGetter: #runButtonState
				action: #runProfiledTests
				label: 'Run Profiled'.
	runProfiledButton
		color: self runButtonColor.
	^runProfiledButton
</details>

#### TestRunnerWindow>>#listMenu

<details>
	<summary>See more</summary>
	
	listMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Test Cases'.
	aMenu add: 'select all' target: model action: #selectAll icon: #selectAllIcon.
	aMenu add: 'deselect all' target: model action: #deselectAll icon: #selectAllIcon.
	aMenu add: 'toggle selections' target: model action: #invertSelections icon: #switchIcon.
	aMenu add: 'filter' target: model action: #setFilter icon: #findIcon.
	model runButtonState ifTrue: [
		aMenu add: 'stop' target: model action: #terminateRun icon: #cancelIcon ].
	model selectedSuite > 0 ifTrue: [ | cls |
		cls _ (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
		cls _ cls asSymbol.
		cls _ (Smalltalk at: cls ifAbsent: nil).
		cls ifNotNil: [
			aMenu addLine.
			(aMenu add: 'browse' target: self action: #browse: argument: cls)
				setIcon: #editFindReplaceIcon.
		].
	].
	aMenu addLine.
	aMenu add: 'log to Transcript' target: model action: #showResult icon: #printerIcon.
	^aMenu
</details>

#### TestRunnerWindow>>#updateColors

<details>
	<summary>See more</summary>
	
	updateColors
	| aTestResult theColor |
	theColor _ self widgetsColor.
	model ifNotNil: [
		model runButtonState ifFalse: [
			aTestResult _ model result.
			theColor _ aTestResult errors size + aTestResult failures size = 0
				ifTrue: [ Theme current successColor ]
				ifFalse: [
					aTestResult errors size > 0
						ifTrue: [ Theme current errorColor ]
						ifFalse: [ Theme current failureColor ]]].
		self updatePartColors: theColor ]
</details>

#### TestRunnerWindow>>#buildMorphicWindow

<details>
	<summary>See more</summary>
	
	buildMorphicWindow

	self layoutMorph
		addMorph: self buildUpperControls proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.75.
	self setLabel: 'SUnit Test Runner'.
	self refreshWindow.
	self morphExtent: `460 @ 400`
</details>

#### TestRunnerWindow>>#buildStopButton

<details>
	<summary>See more</summary>
	
	buildStopButton
	| stopButton |
	stopButton := PluggableButtonMorph 
				model: model
				stateGetter: #stopButtonState
				action: #terminateRun
				label: 'Stop'.
	stopButton
		color: self runButtonColor.
	^stopButton
</details>

#### TestRunnerWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes
	| failuresList errorsList column |
	progress _ ProgressMorph label: '' subLabel: ''.
	self buildPassFailText.
	self buildDetailsText.
	failuresList _ self buildFailuresList.
	errorsList _ self buildErrorsList.
	column _ LayoutMorph newColumn.
	column
		addMorph: progress fixedHeight: 50;
		addAdjusterAndMorph: passFailText fixedHeight: 26;
		addAdjusterAndMorph: detailsText fixedHeight: 26;
		addAdjusterAndMorph: failuresList proportionalHeight: 0.6;
		addAdjusterAndMorph: errorsList proportionalHeight: 0.6.
	^ column
</details>

#### TestRunnerWindow>>#browse: aClass

<details>
	<summary>See more</summary>
	
	browse: aClass
	BrowserWindow fullOnClass: aClass selector: nil
</details>

#### TestRunnerWindow>>#buildRunOneButton

<details>
	<summary>See more</summary>
	
	buildRunOneButton
	| runOneButton |
	runOneButton := PluggableButtonMorph 
				model: model
				stateGetter: #runButtonState
				action: #runOneTest
				label: 'Run last selected'.
	runOneButton
		color: self runButtonColor.
	^runOneButton
</details>

#### TestRunnerWindow>>#buildFilterButton

<details>
	<summary>See more</summary>
	
	buildFilterButton
	| filterButton |
	filterButton := PluggableButtonMorph 
				model: model
				stateGetter: nil
				action: #setFilter
				label: 'Filter'.
	filterButton color: self runButtonColor.
	^filterButton
</details>

#### TestRunnerWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current testRunner
</details>

#### TestRunnerWindow>>#update: aSymbol

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aSymbol
	super update: aSymbol.
	aSymbol == #runTests ifTrue: [
		self updateColors ].
	aSymbol == #progress ifTrue: [
		model ifNotNil: [
			progress label: 'Test progress' subLabel: model progressLabel.
			progress done: model completedFraction asFloat ]]
</details>

#### TestRunnerWindow>>#buildFailuresList

<details>
	<summary>See more</summary>
	
	buildFailuresList
	^ PluggableListMorph
		model: model
		listGetter: #failuresList
		indexGetter: #selectedFailureTest
		indexSetter: #debugFailureTest:
		mainView: self
		menuGetter: nil
		keystrokeAction: nil
</details>

#### TestRunnerWindow>>#buildDetailsText

<details>
	<summary>See more</summary>
	
	buildDetailsText
	detailsText _ (TextModelMorph
		textProvider: model
		textGetter: #details) emptyTextDisplayMessage: 'Test run details (no results to display)'.
	detailsText hideScrollBarsIndefinitely.
	^detailsText
</details>

#### TestRunnerWindow>>#runButtonColor

<details>
	<summary>See more</summary>
	
	runButtonColor
	^ `Color green lighter duller`
</details>

## TranscriptWindow

Morphic view for Transcript. See category 'GUI building'.

### Methods
#### TranscriptWindow>>#visible: aBoolean

set the 'visible' attribute of the receiver to aBoolean


<details>
	<summary>See more</summary>
	
	visible: aBoolean
	super visible: aBoolean.
	Transcript showOnDisplay: aBoolean
</details>

#### TranscriptWindow>>#delete

Remove the receiver as a submorph of its owner and make its new owner be nil.


<details>
	<summary>See more</summary>
	
	delete
	Transcript showOnDisplay: false.
	super delete
</details>

#### TranscriptWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^Theme current transcript
</details>

## VersionsBrowserWindow

Morphic view for VersionsBrowser models. See category 'GUI building'.

### Methods
#### VersionsBrowserWindow>>#prettyWordDiffButtonHelp

<details>
	<summary>See more</summary>
	
	prettyWordDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, word by word.'
</details>

#### VersionsBrowserWindow>>#optionalModelButtonTuples

<details>
	<summary>See more</summary>
	
	optionalModelButtonTuples

	^#(
		(25
		'compare to current'
		compareToCurrentVersion
		'opens a separate window which shows the text differences between the selected version and the current version')

		(10
		'revert'
		fileInSelections
		'reverts the method to the version selected')

		(7
		'help'
		offerVersionsHelp
		'further explanation about use of Versions browsers')
	)
</details>

#### VersionsBrowserWindow>>#classCommentVersionsMenu

<details>
	<summary>See more</summary>
	
	classCommentVersionsMenu
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.				"all commands are implemented by the model, not the view"
	aMenu addTitle: 'versions'.
	aMenu addStayUpIcons.
	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'compare to current'.
			#selector 		-> 			#compareToCurrentVersion.
			#balloonText 	-> 			'compare selected version to the current version'
		} asDictionary.
		{
			#label 			-> 			'revert to selected version'.
			#selector 		-> 			#fileInSelections.
			#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu
</details>

#### VersionsBrowserWindow>>#methodVersionsMenu

Fill aMenu with menu items appropriate to the receiver


<details>
	<summary>See more</summary>
	
	methodVersionsMenu
	"Fill aMenu with menu items appropriate to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Versions'.
	aMenu addStayUpIcons.
	model listIndex > 0 ifTrue:[
		(model list size > 1 ) ifTrue: [
			aMenu addItemsFromDictionaries: `{
				{
					#label 			-> 			'compare to current'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToCurrentVersion.
					#balloonText 	-> 			'compare selected version to the current version'
				} asDictionary.
				{
					#label 			-> 			'compare to version...'.
					#object 			-> 			#model.
					#selector 		-> 			#compareToOtherVersion.
					#balloonText 	-> 			'compare selected version to another selected version'
				} asDictionary.
			}` ].
		"Note: Revert to selected should be visible for lists of length one for having the ability to revert to an accidentally deleted method"
		aMenu addItemsFromDictionaries: `{
			{
				#label 			-> 			'revert to selected version'.
				#object 			-> 			#model.
				#selector 		-> 			#fileInSelections.
				#balloonText 	-> 			'resubmit the selected version, so that it becomes the current version'
			} asDictionary.
		}` ].

	aMenu addItemsFromDictionaries: `{
		{
			#label 			-> 			'edit current method (O)'.
			#selector 		-> 			#openSingleMessageBrowser.
			#balloonText 	-> 			'open a single-message browser on the current version of this method'
		} asDictionary.
		nil.
		{
			#label 			-> 			'toggle diffing (D)'.
			#object 			-> 			#model.
			#selector 		-> 			#toggleDiffing.
			#balloonText 	-> 			'toggle whether or not diffs should be shown here'
		} asDictionary.
		{
			#label 			-> 			'update list'.
			#object 			-> 			#model.
			#selector 		-> 			#reformulateList.
			#balloonText 	-> 			'reformulate the list of versions, in case it somehow got out of synch with reality'
		} asDictionary.
		nil.
		{
			#label 			-> 			'senders (n)'.
			#selector 		-> 			#browseSenders.
			#balloonText 	-> 			'browse all senders of this selector'
		} asDictionary.
		{
			#label 			-> 			'implementors (m)'.
			#selector 		-> 			#browseImplementors.
			#balloonText 	-> 			'browse all implementors of this selector'
		} asDictionary.
		nil.
		{
			#label 			-> 			'help...'.
			#object 			-> 			#model.
			#selector 		-> 			#offerVersionsHelp.
			#balloonText 	-> 			'provide an explanation of the use of this tool'
		} asDictionary.
	}`.
	^aMenu
</details>

#### VersionsBrowserWindow>>#listMenu

Fill aMenu up so that it comprises the primary changelist-browser menu


<details>
	<summary>See more</summary>
	
	listMenu
	^model classCommentIndicated
		ifTrue: [ self classCommentVersionsMenu ]
		ifFalse: [ self methodVersionsMenu]
</details>

#### VersionsBrowserWindow>>#lineDiffButtonHelp

<details>
	<summary>See more</summary>
	
	lineDiffButtonHelp
	^'Show code differences from the previous version, line by line.'
</details>

#### VersionsBrowserWindow>>#buildMorphicWindow

Open a morphic view for the messageSet, whose label is labelString. The listView may be either single or multiple selection type


<details>
	<summary>See more</summary>
	
	buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"
	| listPane |
	listPane _ PluggableListMorph
		model: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	
	self layoutMorph
		addMorph: listPane proportionalHeight: 0.4;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.6
</details>

#### VersionsBrowserWindow>>#changeListKey: aChar from: view

Respond to a Command key in the list pane. of the versions browser


<details>
	<summary>See more</summary>
	
	changeListKey: aChar from: view
	"Respond to a Command key in the list pane. of the versions browser"

	^ self messageListKey: aChar from: view
</details>

#### VersionsBrowserWindow>>#wordDiffButtonHelp

<details>
	<summary>See more</summary>
	
	wordDiffButtonHelp
	^'Show code differences from the previous version, word by word.'
</details>

#### VersionsBrowserWindow>>#updateListsAndCode

All code windows receive this message on any code change in the system


<details>
	<summary>See more</summary>
	
	updateListsAndCode
	"All code windows receive this message on any code change in the system"
	model updateIfNeeded
</details>

#### VersionsBrowserWindow>>#windowColor

Some default


<details>
	<summary>See more</summary>
	
	windowColor
	^ Theme current versionsBrowser
</details>

#### VersionsBrowserWindow>>#prettyLineDiffButtonHelp

<details>
	<summary>See more</summary>
	
	prettyLineDiffButtonHelp
	^'Show pretty-printed code differences from the previous version, line by line.'
</details>

## WorkspaceWindow

Morphic view for Workspace models. See category 'GUI building'.

### Methods
#### WorkspaceWindow>>#offerWindowMenu

<details>
	<summary>See more</summary>
	
	offerWindowMenu
	| aMenu |
	aMenu _ self buildWindowMenu.
	aMenu addLine.
	aMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings
		icon: #warningIcon.
	aMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling.
	aMenu popUpInWorld: self world
</details>

#### WorkspaceWindow>>#addCustomMenuItems: aCustomMenu hand: aHandMorph

Add morph-specific items to the given menu which was invoked by the given hand. This method provides is invoked both from the halo-menu and from the control-menu regimes.


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu
		add: 'reset variables'
		target: model
		action: #initializeBindings.
	aCustomMenu
		addUpdating: #mustDeclareVariableWording
		target: model
		action: #toggleVariableDeclarationMode.
	aCustomMenu
		addUpdating: #toggleStylingLabel
		target: model
		action: #toggleStyling
</details>

