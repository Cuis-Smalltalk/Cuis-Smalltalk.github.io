## Browser

I represent a query path into the class descriptions, the software of the system.

### Methods
#### Browser>>#newSystemCategoryNameIfNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	newSystemCategoryNameIfNone: aNoneBlock

	| newName |
	
	newName _ self
		request: 'Please type new category name'
		initialAnswer: 'Category-Name'.
	
	^newName isEmpty
		ifTrue: aNoneBlock 
		ifFalse: [newName asSymbol].
</details>

#### Browser>>#isEditingNewClass

<details>
	<summary>See more</summary>
	
	isEditingNewClass

	^editSelection == #newClass 
</details>

#### Browser>>#fileOutSystemCategory

Print a description of each class in the selected category onto a file whose name is the category name followed by .st.


<details>
	<summary>See more</summary>
	
	fileOutSystemCategory
	"Print a description of each class in the selected category onto a file 
	whose name is the category name followed by .st."

	selectedSystemCategory
		ifNotNil: [ systemOrganizer fileOutCategory: selectedSystemCategory ]
</details>

#### Browser>>#metaClassIndicated

Answer the boolean flag that indicates which of the method dictionaries, class or metaclass.


<details>
	<summary>See more</summary>
	
	metaClassIndicated
	"Answer the boolean flag that indicates which of the method dictionaries, 
	class or metaclass."

	^metaClassIndicated
</details>

#### Browser>>#explainSpecial: string

Answer a string explaining the code pane selection if it is displaying one of the special edit functions.


<details>
	<summary>See more</summary>
	
	explainSpecial: string 
	"Answer a string explaining the code pane selection if it is displaying 
	one of the special edit functions."

	| classes whole lits reply |
	self isEditingClass
		ifTrue: 
			["Selector parts in class definition"
			string last == $: ifFalse: [^nil].
			lits _ Array with:
				#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:.
			(whole _ lits detect: [:each | (each keywords
					detect: [:frag | frag = string] ifNone: nil) notNil]
						ifNone: nil) notNil
				ifTrue: [reply _ '"' , string , ' is one part of the message selector ' , whole , '.']
				ifFalse: [^nil].
			classes _ Smalltalk allClassesImplementing: whole.
			classes _ 'these classes ' , classes printString.
			^reply , '  It is defined in ' , classes , '."
Smalltalk browseAllImplementorsOf: #' , whole].

	editSelection == #hierarchy
		ifTrue: 
			["Instance variables in subclasses"
			classes _ self selectedClassOrMetaClass allSubclasses.
			classes _ classes detect: [:each | (each instVarNames
						detect: [:name | name = string] ifNone: nil) notNil]
					ifNone: [^nil].
			classes _ classes printString.
			^'"is an instance variable in class ' , classes , '."
' , classes , ' browseAllAccessesTo: ''' , string , '''.'].
	editSelection == #editSystemCategories ifTrue: [^nil].
	editSelection == #editMessageCategories ifTrue: [^nil].
	^nil
</details>

#### Browser>>#request: prompt initialAnswer: initialAnswer

<details>
	<summary>See more</summary>
	
	request: prompt initialAnswer: initialAnswer

	^ FillInTheBlankMorph
		request: prompt
		initialAnswer: initialAnswer

</details>

#### Browser>>#systemOrganizer: aSystemOrganizer

Initialize the receiver as a perspective on the system organizer, aSystemOrganizer. Typically there is only one--the system variable SystemOrganization.


<details>
	<summary>See more</summary>
	
	systemOrganizer: aSystemOrganizer
	"Initialize the receiver as a perspective on the system organizer, 
	aSystemOrganizer. Typically there is only one--the system variable 
	SystemOrganization."

	systemOrganizer _ aSystemOrganizer.
	selectedSystemCategory _ nil.
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	metaClassIndicated _ false.
	self setClassOrganizer.
	self editSelection: #none.
</details>

#### Browser>>#classDefinitionText

return the text to display for the definition of the currently selected class


<details>
	<summary>See more</summary>
	
	classDefinitionText
	"return the text to display for the definition of the currently selected class"
	
	^self selectedClassOrMetaClass 
		ifNil: [''] 
 		ifNotNil: [ :theClass | theClass definition]
</details>

#### Browser>>#selectedClass

Answer the class that is currently selected. Answer nil if no selection exists.


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	| name |
	(name _ self selectedClassName) ifNil: [^ nil].
	^ Smalltalk at: name ifAbsent: nil
</details>

#### Browser>>#moveSystemCategoryDown

<details>
	<summary>See more</summary>
	
	moveSystemCategoryDown
	selectedSystemCategory ifNil: [^ self].
	self systemCategoryListIndex: 
		(systemOrganizer moveCategoryDown: selectedSystemCategory).
	self changed: #systemCategoryList
</details>

#### Browser>>#editSelection

<details>
	<summary>See more</summary>
	
	editSelection
	^editSelection
</details>

#### Browser>>#runSystemCategoryTests

<details>
	<summary>See more</summary>
	
	runSystemCategoryTests
	
	selectedSystemCategory ifNotNil: [ | suite |
		suite _ TestSuite forSystemCategoryNamed: selectedSystemCategory using: systemOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	
</details>

#### Browser>>#indexIsOne: value

When used as a singleton list, can't change it


<details>
	<summary>See more</summary>
	
	indexIsOne: value
	"When used as a singleton list, can't change it"

	^ self
</details>

#### Browser>>#rawMessageCategoryList

<details>
	<summary>See more</summary>
	
	rawMessageCategoryList

	^ selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ self classOrMetaClassOrganizer categories ]
</details>

#### Browser>>#doItReceiver

This class's classPool has been jimmied to be the classPool of the class being browsed. A doIt in the code pane will let the user see the value of the class variables.


<details>
	<summary>See more</summary>
	
	doItReceiver
	"This class's classPool has been jimmied to be the classPool of the class 
	being browsed. A doIt in the code pane will let the user see the value of 
	the class variables."

	^ self selectedClass
</details>

#### Browser>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	self initializeListClassesHierachically
</details>

#### Browser>>#labelString

<details>
	<summary>See more</summary>
	
	labelString
	^self selectedClass ifNil: [ self defaultBrowserTitle ]
		ifNotNil: [ self defaultBrowserTitle, ': ', self selectedClass printString ].

</details>

#### Browser>>#createInstVarAccessors

Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class


<details>
	<summary>See more</summary>
	
	createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage _ aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ].
			(aClass canUnderstand: (setter _ aName , ':') asSymbol) ifFalse: [
				newMessage _ setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' ' ,
					(Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
						ifTrue: [ '_' ]
						ifFalse: [ ':=' ]) , ' anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]
</details>

#### Browser>>#alphabetizeSystemCategories

<details>
	<summary>See more</summary>
	
	alphabetizeSystemCategories

	systemOrganizer sortCategories.
	self systemCategoryListIndex: 0.
	self changed: #systemCategoryList.

</details>

#### Browser>>#systemCategorySingleton

<details>
	<summary>See more</summary>
	
	systemCategorySingleton

	^ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [Array with: selectedSystemCategory]
</details>

#### Browser>>#selectedSystemCategoryName

Answer the name of the selected system category or nil.


<details>
	<summary>See more</summary>
	
	selectedSystemCategoryName
	"Answer the name of the selected system category or nil."

	^selectedSystemCategory
</details>

#### Browser>>#categorizeUnderCategoryAt: aSystemCategoryIndex class: aClassName

<details>
	<summary>See more</summary>
	
	categorizeUnderCategoryAt: aSystemCategoryIndex class: aClassName 

	systemOrganizer classify: aClassName withBlanksTrimmed asSymbol under: (self systemCategoryList at: aSystemCategoryIndex).
	self changed: #classList
</details>

#### Browser>>#renameCategory

Prompt for a new category name and add it before the current selection, or at the end if no current selection


<details>
	<summary>See more</summary>
	
	renameCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	| oldIndex oldName newName |
	selectedClassName ifNil: [^ self].
	selectedMessageCategory ifNil: [ ^self ].
	oldIndex _ self messageCategoryListIndex.
	oldName _ self selectedMessageCategoryName.
	newName _ self
		request: 'Please type new category name'
		initialAnswer: oldName.
	newName isEmpty
		ifTrue: [^ self]
		ifFalse: [newName _ newName asSymbol].
	newName = oldName ifTrue: [^ self].
	self classOrMetaClassOrganizer
		renameCategory: oldName
		toBe: newName.
	self classListIndex: self classListIndex.
	self messageCategoryListIndex: oldIndex.
	self changed: #messageCategoryList
</details>

#### Browser>>#isEditingClass

<details>
	<summary>See more</summary>
	
	isEditingClass

	^self isEditingExistingClass or: [ self isEditingNewClass ]
</details>

#### Browser>>#contents: input notifying: aRequestor

The retrieved information has changed and its source must now be updated. The information can be a variety of things, depending on the list selections (such as templates for class or message definition, methods) or the user menu commands (such as definition, comment, hierarchy). Answer the result of updating the source.


<details>
	<summary>See more</summary>
	
	contents: input notifying: aRequestor
	"The retrieved information has changed and its source must now be
	 updated. The information can be a variety of things, depending on
	 the list selections (such as templates for class or message definition,
	 methods) or the user menu commands (such as definition, comment,
	 hierarchy).  Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.
	editSelection == #editSystemCategories ifTrue: [ ^ self changeSystemCategories: aString ].
	self isEditingClass ifTrue: [
		[
			self defineClass: aString notifying: aRequestor 
		] on: RecompilationFailure do: [ :ex |
			self inform: ex messageText.
			^ false].
		^ true].
	editSelection == #editComment
		ifTrue: [
			theClass _ self selectedClass.
			theClass
				ifNil: [
					self inform: 'You must select a class
before giving it a comment.'.
					^ false].
			theClass comment: aText stamp: Utilities changeStamp.
			self changed: #classCommentText.
			^ true].
	editSelection == #hierarchy ifTrue: [ ^ true ].
	editSelection == #editMessageCategories ifTrue: [ ^ self changeMessageCategories: aString ].
	editSelection == #editMessage | (editSelection == #newMessage)
		ifTrue: [
			^ self okayToAccept
				ifFalse:[ false ]
				ifTrue: [
					(self compileMessage: aText notifying: aRequestor)
						ifTrue: [ self triggerEvent: #annotationChanged ];
						yourself ]].
	editSelection == #none
		ifTrue: [
			self inform: 'This text cannot be accepted
in this part of the browser.'.
			^ false].
	self error: 'unacceptable accept'
</details>

#### Browser>>#hierarchicalClassList

classNames are an arbitrary collection of classNames of the system. Reorder those class names so that they are sorted and indended by inheritance


<details>
	<summary>See more</summary>
	
	hierarchicalClassList

	"classNames are an arbitrary collection of classNames of the system.
	Reorder those class names so that they are sorted and indended by inheritance"

	| classes |

	"Creating the hierarchy is *really slow* for the full class list. Skip it for now."
	selectedSystemCategory = SystemOrganizer allCategory ifTrue: [^ self defaultClassList].		
	classes := self defaultClassList collect: [:sym | Smalltalk classNamed: sym].
	
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: classes)
		on: OrderedCollection new
		indent: ''.
</details>

#### Browser>>#moveAllToOtherSystemCategory

If a class category is selected, prompt user for category to move to, create a Confirmer so the user can verify that all the classes in current category should be moved to the selected category.


<details>
	<summary>See more</summary>
	
	moveAllToOtherSystemCategory
	"If a class category is selected, prompt user for category to move to,
	create a Confirmer so the user can verify that all the classes in current category
 	should be moved to the selected category."
	| newSystemCategory |
	selectedSystemCategory ifNil: [ ^ self ].
	newSystemCategory _ Smalltalk systemCategoryFromUserWithPrompt: 'Move classes to System Category...'.
	(newSystemCategory notNil and: [
		self classList size > 0 and: [ self confirm: 'Are you sure you want to
move classes from ' , selectedSystemCategory , ' 
to ' , newSystemCategory , '?' ]]) ifTrue: [
		"Safer this way (#classList will be a collection of strings with spaces and who knows what in the future.  So let's just get the classes we need directly)"
		(SystemOrganization classesAt: selectedSystemCategory) do: [ :eaClass |
			eaClass category: newSystemCategory ].
		self changed: #systemCategoryList ].
</details>

#### Browser>>#safelyRemoveClass

<details>
	<summary>See more</summary>
	
	safelyRemoveClass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(SafelyRemoveClassApplier on: self of: aBehavior theNonMetaClass) value ].
</details>

#### Browser>>#runMessageCategoryTests

<details>
	<summary>See more</summary>
	
	runMessageCategoryTests
	
	selectedMessageCategory ifNotNil: [ | selectedClass suite |
		selectedClass _ Smalltalk classNamed: selectedClassName.
		suite _ TestSuite forMessageCategoryNamed: selectedMessageCategory of: selectedClass categorizedWith: classOrganizer.
		(ProgressiveTestRunner for: suite) value ]
	
	
</details>

#### Browser>>#classCommentIndicated

Answer true iff we're viewing the class comment.


<details>
	<summary>See more</summary>
	
	classCommentIndicated
	"Answer true iff we're viewing the class comment."

	^ editSelection == #editComment 

</details>

#### Browser>>#removeMessage

If a message is selected, create a Confirmer so the user can verify that the currently selected message should be removed from the system. If so, remove it. If the Preference 'confirmMethodRemoves' is set to false, the confirmer is bypassed.


<details>
	<summary>See more</summary>
	
	removeMessage
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so,  
	remove it. If the Preference 'confirmMethodRemoves' is set to false, the 
	confirmer is bypassed."
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName _ self selectedMessageName.
	confirmation _ Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: self selectedMessageName.
	self reformulateList.
	self changed: #messageList.
	self setClassOrganizer.
	"In case organization not cached"
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]
</details>

#### Browser>>#selectedClassName

Answer the name of the current class. Answer nil if no selection exists.


<details>
	<summary>See more</summary>
	
	selectedClassName
	"Answer the name of the current class. Answer nil if no selection exists."

	^selectedClassName ifNotNil: [ 
		"I send #defaultClassList and no #classList because when showing classes hierarchically we should remove spaces to see
		if class name is in the list and that consumes more time - Hernan"
		(self defaultClassList includes: selectedClassName) ifTrue: [ selectedClassName ]]
</details>

#### Browser>>#selectedClassOrMetaClass

Answer the selected class or metaclass.


<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	"Answer the selected class or metaclass."

	| cls |
	self metaClassIndicated
		ifTrue: [^ (cls _ self selectedClass) ifNil: [nil] ifNotNil: [cls class]]
		ifFalse: [^ self selectedClass]
</details>

#### Browser>>#setOriginalCategoryIndexForCurrentMethod

private - Set the message category index for the currently selected method. Note: This should only be called when somebody tries to save a method that they are modifying while ALL is selected.


<details>
	<summary>See more</summary>
	
	setOriginalCategoryIndexForCurrentMethod
	"private - Set the message category index for the currently selected method. 
	 
	 Note:  This should only be called when somebody tries to save  
	 a method that they are modifying while ALL is selected."

	selectedMessageCategory _ self categoryOfCurrentMethod
	
</details>

#### Browser>>#recent

Let the user select from a list of recently visited classes. 11/96 stp. 12/96 di: use class name, not classes themselves. : dont fall into debugger in empty case


<details>
	<summary>See more</summary>
	
	recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ self class recentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUpMenu.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self selectClass: class
</details>

#### Browser>>#flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars

Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc.


<details>
	<summary>See more</summary>
	
	flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars

	"Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc."

	| plusIndent |

	plusIndent := indentChars.
	classHierarchy do: [:assoc |
		| class childs |
		class := assoc key.
		col add: indent , class name.
		childs := assoc value.
		self
			flattenHierarchyTree: childs
			on: col
			indent: indent , plusIndent
			by: indentChars].
	^ col
</details>

#### Browser>>#messageList

Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range. Otherwise, answer an empty Array If messageCategoryListIndex is found to be larger than the number of categories (it happens!), it is reset to zero.


<details>
	<summary>See more</summary>
	
	messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!), it is reset to zero."
	| classOrMetaClassOrganizer answer |
	classOrMetaClassOrganizer _ self classOrMetaClassOrganizer.
	classOrMetaClassOrganizer isNil ifTrue: [ ^#() ].
	(selectedMessageCategory isNil or: [ selectedMessageCategory == ClassOrganizer allCategory ]) ifTrue: [
		^ classOrMetaClassOrganizer allMethodSelectors].
	answer _ classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory.	
	answer isNil ifTrue: [
		selectedMessageCategory _ nil.
		answer _ #() ].
	^answer
</details>

#### Browser>>#renameClass

<details>
	<summary>See more</summary>
	
	renameClass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(RenameClassApplier for: aBehavior theNonMetaClass) value ].
</details>

#### Browser>>#defineMessageFrom: aString notifying: aRequestor

Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise.


<details>
	<summary>See more</summary>
	
	defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	 self metaClassIndicated ifTrue: [
		selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString.
		((self selectedClassOrMetaClass includesSelector: selector) not
			and: [Metaclass isScarySelector: selector])
			ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
					(self confirm: (selector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
					ifFalse: [^nil]]].
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector
</details>

#### Browser>>#copyClass

<details>
	<summary>See more</summary>
	
	copyClass
	| originalClass originalName copysName  newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	copysName _ self request: 'Please type new class name' initialAnswer: originalName.
	copysName = '' ifTrue: [^ self].  " Cancel returns '' "
	copysName _ copysName asSymbol.
	copysName = originalName ifTrue: [^ self].
	(Smalltalk includesKey: copysName)
		ifTrue: [^ self error: copysName , ' already exists'].
	newDefinition := originalClass definition
		copyReplaceAll: originalName printString
		with: copysName printString.
	newClass _ Compiler evaluate: newDefinition logged: true.
	newMetaDefinition := originalClass class definition
		copyReplaceAll: originalClass class name
		with: newClass class name.
	Compiler evaluate: newMetaDefinition logged: true.
	newClass copyAllCategoriesFrom: originalClass.
	newClass class copyAllCategoriesFrom: originalClass class.
	originalClass hasComment ifTrue: [
		newClass comment: originalClass comment ].			
	self classListIndex: 0.
	self changed: #classList
</details>

#### Browser>>#messageCategoryListIndex

Answer the index of the selected message category.


<details>
	<summary>See more</summary>
	
	messageCategoryListIndex
	"Answer the index of the selected message category."


	selectedMessageCategory ifNil: [ ^0 ].
	^self messageCategoryList indexOf: selectedMessageCategory
</details>

#### Browser>>#removeSystemCategory

If a class category is selected, create a Confirmer so the user can verify that the currently selected class category and all of its classes should be removed from the system. If so, remove it.


<details>
	<summary>See more</summary>
	
	removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	selectedSystemCategory ifNil: [^ self].
	(self classList size = 0
		or: [self confirm: 'Are you sure you want to
remove this system category 
and all its classes?'])
		ifTrue: [
			systemOrganizer removeSystemCategory: selectedSystemCategory.
			self systemCategoryListIndex: 0.
			self changed: #systemCategoryList ]
</details>

#### Browser>>#toggleBreakOnEntry

Install or uninstall a halt-on-entry breakpoint


<details>
	<summary>See more</summary>
	
	toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[BreakpointManager unInstall: selectedMethod]
		ifFalse:
			[BreakpointManager 
				installInClass: self selectedClassOrMetaClass
				selector: self selectedMessageName].
	self changed: #messageList
</details>

#### Browser>>#createHierarchyTreeOf: col

Create a tree from a flat collection of classes


<details>
	<summary>See more</summary>
	
	createHierarchyTreeOf: col

	"Create a tree from a flat collection of classes"

	| transformed |

	transformed := col collect: [:ea | 
		| childs indexes |
		childs := col select: [:class | class superclass = ea].
		indexes := childs collect: [:child | col indexOf: child].
		Association key: ea value: indexes].
	transformed copy do: [:ea |
		ea value: (ea value collect: [:idx | 
			| val |
			val := transformed at: idx.
			transformed at: idx put: nil.
			val])].
	^ transformed select: [:ea | ea notNil].

</details>

#### Browser>>#changeMessageCategories: aString

The characters in aString represent an edited version of the the message categories for the selected class. Update this information in the system and inform any dependents that the categories have been changed. This message is invoked because the user had issued the categories command and edited the message categories. Then the user issued the accept command.


<details>
	<summary>See more</summary>
	
	changeMessageCategories: aString 
	"The characters in aString represent an edited version of the the message 
	categories for the selected class. Update this information in the system 
	and inform any dependents that the categories have been changed. This 
	message is invoked because the user had issued the categories command 
	and edited the message categories. Then the user issued the accept 
	command."

	self classOrMetaClassOrganizer changeFromString: aString.
	self changed: #clearUserEdits.
	self editClass.
	self classListIndex: self classListIndex.
	^ true
</details>

#### Browser>>#systemCategoryList

Answer the class categories modelled by the receiver.


<details>
	<summary>See more</summary>
	
	systemCategoryList
	"Answer the class categories modelled by the receiver."

	^systemOrganizer categories
</details>

#### Browser>>#messageCategoryListIndex: anInteger

Set the selected message category to be the one indexed by anInteger.


<details>
	<summary>See more</summary>
	
	messageCategoryListIndex: anInteger
	"Set the selected message category to be the one indexed by anInteger."

	| index messageCategoryList |
	
	messageCategoryList _ self messageCategoryList.
	index _ messageCategoryList ifInBounds: anInteger ifNot: 0.

	selectedMessageCategory _ index = 0 ifFalse: [messageCategoryList at: index ].
	selectedMessage _ nil.
	self changed: #messageCategorySelectionChanged.
	self changed: #messageCategoryListIndex. "update my selection"
	self changed: #messageList.
	self editSelection: (index > 0
		ifTrue: [#newMessage]
		ifFalse: [self classListIndex > 0
			ifTrue: [#editClass]
			ifFalse: [#newClass]]).
	self acceptedContentsChanged.
</details>

#### Browser>>#enableListClassesAlphabetically

<details>
	<summary>See more</summary>
	
	enableListClassesAlphabetically

	self listClassesHierarchically: false
</details>

#### Browser>>#messageListIndex

Answer the index of the selected message selector into the currently selected message category.


<details>
	<summary>See more</summary>
	
	messageListIndex
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^self messageList indexOf: selectedMessage
</details>

#### Browser>>#instanceMessagesIndicated

Answer whether the messages to be presented should come from the class.


<details>
	<summary>See more</summary>
	
	instanceMessagesIndicated
	"Answer whether the messages to be presented should come from the 
	class."

	^metaClassIndicated not and: [self classCommentIndicated not]
</details>

#### Browser>>#updateSystemCategories

The class categories were changed in another browser. The receiver must reorganize its lists based on these changes.


<details>
	<summary>See more</summary>
	
	updateSystemCategories
	"The class categories were changed in another browser. The receiver must 
	reorganize its lists based on these changes."

	self changed: #systemCategoryList
</details>

#### Browser>>#removeClass

Remove the selected class from the system, at interactive user request. Make certain the user really wants to do this, since it is not reversible. Answer true if removal actually happened.


<details>
	<summary>See more</summary>
	
	removeClass

	self safelyRemoveClass 
</details>

#### Browser>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	| type |
	
	self isModeStyleable ifFalse: [^false].
	type _ self editSelection.
	(#(newMessage editMessage editClass newClass) includes: type) ifFalse:[^false].
	anSHTextStyler classOrMetaClass: (type = #editClass ifFalse:[self selectedClassOrMetaClass]).
	^true
</details>

#### Browser>>#categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aSelectorToCategorize

<details>
	<summary>See more</summary>
	
	categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector _ self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]
</details>

#### Browser>>#renameGlobal

<details>
	<summary>See more</summary>
	
	renameGlobal
	
	(RenameGlobalApplier on: self for: '') value
</details>

#### Browser>>#editSelection: aSelection

Set the editSelection as requested.


<details>
	<summary>See more</summary>
	
	editSelection: aSelection
	"Set the editSelection as requested."

	editSelection _ aSelection.
	self changed: #editSelection.
</details>

#### Browser>>#editClass

Retrieve the description of the class definition.


<details>
	<summary>See more</summary>
	
	editClass
	"Retrieve the description of the class definition."

	selectedClassName ifNil: [^ self].
	self messageCategoryListIndex: 0.
	self editSelection: #editClass.
	self acceptedContentsChanged.
	self changed: #classCommentText.

</details>

#### Browser>>#classListIndexOf: aClassNameToFind

Answer the index of the aClassName selection.


<details>
	<summary>See more</summary>
	
	classListIndexOf: aClassNameToFind

	"Answer the index of the aClassName selection."

	aClassNameToFind ifNil: [ ^0 ].
	^self classList findFirst: [ :showingClassName |
		"Works regardless of currently showing hierarchically or alphabetically."
		showingClassName afterBlanksEndsWith: aClassNameToFind  ]
</details>

#### Browser>>#classOrMetaClassOrganizer

Answer the class organizer for the metaclass or class, depending on which (instance or class) is indicated.


<details>
	<summary>See more</summary>
	
	classOrMetaClassOrganizer
	"Answer the class organizer for the metaclass or class, depending on 
	which (instance or class) is indicated."

	self metaClassIndicated
		ifTrue: [^metaClassOrganizer]
		ifFalse: [^classOrganizer]
</details>

#### Browser>>#defineClass: defString notifying: aRequestor

The receiver's textual content is a request to define a new class. The source code is defString. If any errors occur in compilation, notify aRequestor.


<details>
	<summary>See more</summary>
	
	defineClass: defString notifying: aRequestor  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryList.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]
</details>

#### Browser>>#addSystemCategory

Prompt for a new category name and add it before the current selection, or at the end if no current selection


<details>
	<summary>See more</summary>
	
	addSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| oldIndex newName |
	
	oldIndex _ self systemCategoryListIndex.
	newName _ self newSystemCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newName
		before: selectedSystemCategory.
	self systemCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self systemCategoryList size]
			ifFalse: [oldIndex]).
	self changed: #systemCategoryList.
</details>

#### Browser>>#fileOutClass

Print a description of the selected class onto a file whose name is the category name followed by .st.


<details>
	<summary>See more</summary>
	
	fileOutClass
	"Print a description of the selected class onto a file whose name is the 
	category name followed by .st."

	selectedClassName ifNotNil: [ self selectedClass fileOut ]
</details>

#### Browser>>#systemCategoryListIndex

Answer the index of the selected class category.


<details>
	<summary>See more</summary>
	
	systemCategoryListIndex
	"Answer the index of the selected class category."

	systemOrganizer ifNil: [ ^0 ].
	selectedSystemCategory ifNil: [ ^0 ].
	^self systemCategoryList indexOf: selectedSystemCategory
</details>

#### Browser>>#systemCategoryListIndex: anInteger

Set the selected system category index to be anInteger. Update all other selections to be deselected.


<details>
	<summary>See more</summary>
	
	systemCategoryListIndex: anInteger
	"Set the selected system category index to be anInteger. Update all other 
	selections to be deselected."
	
	| index systemCategoryList |
	
	systemCategoryList _ self systemCategoryList.
	index _ systemCategoryList ifInBounds: anInteger ifNot: 0.
	
	selectedSystemCategory _ index = 0 ifFalse: [ systemCategoryList at: index ].
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection: ( index = 0 ifTrue: [#none] ifFalse: [#newClass]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #systemCategorySelectionChanged.
	self changed: #systemCategoryListIndex.	"update my selection"
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged
</details>

#### Browser>>#indexIsOne

When used as a singleton list, index is always one


<details>
	<summary>See more</summary>
	
	indexIsOne
	"When used as a singleton list, index is always one"
	^ 1
</details>

#### Browser>>#classCommentText

return the text to display for the comment of the currently selected class


<details>
	<summary>See more</summary>
	
	classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass _ self selectedClassOrMetaClass.
	^ Text
		initialFont: Preferences standardCodeFont
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass comment ]
				ifFalse: [ '' ]).
</details>

#### Browser>>#classListIndex: anInteger

Set anInteger to be the index of the current class selection.


<details>
	<summary>See more</summary>
	
	classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList |

	classList _ self classList.
	canSelectClass _ classList isInBounds: anInteger.
	selectedClassName _ canSelectClass ifTrue: [	| newClassName |
		newClassName := classList at: anInteger ifAbsent: [ nil ].
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	selectedMessage _ nil.
	
	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent _ self class recentClasses.
		className _ self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory _ nil].
		
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged
</details>

#### Browser>>#canShowMultipleMessageCategories

Answer whether the receiver is capable of showing multiple message categories


<details>
	<summary>See more</summary>
	
	canShowMultipleMessageCategories
	"Answer whether the receiver is capable of showing multiple message categories"

	^ true
</details>

#### Browser>>#indicateClassMessages

Indicate that the message selection should come from the metaclass messages.


<details>
	<summary>See more</summary>
	
	indicateClassMessages
	"Indicate that the message selection should come from the metaclass 
	messages."

	self metaClassIndicated: true
</details>

#### Browser>>#methodCategoryChanged

<details>
	<summary>See more</summary>
	
	methodCategoryChanged
	self changed: #messageCategoryList.
	self changed: #messageList.
	self triggerEvent: #annotationChanged.
	self reformulateList
</details>

#### Browser>>#selectClass: classNotMeta

<details>
	<summary>See more</summary>
	
	selectClass: classNotMeta

	self classListIndex: (self classListIndexOf: classNotMeta name)
</details>

#### Browser>>#makeNewSubclass

<details>
	<summary>See more</summary>
	
	makeNewSubclass

	self selectedClassOrMetaClass ifNil: [^ self].
	self editSelection: #newClass.
	self acceptedContentsChanged
</details>

#### Browser>>#defaultClassList

Answer an array of the class names of the selected category. Answer an empty array if no selection exists.


<details>
	<summary>See more</summary>
	
	defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [systemOrganizer listAtCategoryNamed: selectedSystemCategory]
</details>

#### Browser>>#removeMessageCategory

If a message category is selected, create a Confirmer so the user can verify that the currently selected message category should be removed from the system. If so, remove it.


<details>
	<summary>See more</summary>
	
	removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [^ self].
	messageCategoryName _ self selectedMessageCategoryName.
	(self messageList size = 0
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?'])
		ifTrue: [
			self selectedClassOrMetaClass removeCategory: messageCategoryName.
			self messageCategoryListIndex: 0.
			self changed: #classSelectionChanged].
	self changed: #messageCategoryList.

</details>

#### Browser>>#addCategory

Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection


<details>
	<summary>See more</summary>
	
	addCategory
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"

	| oldIndex newName |

	selectedClassName ifNil: [ ^self ].
	
	oldIndex _ self messageCategoryListIndex.
	newName _ self newMethodCategoryNameIfNone: [ ^self ].
	
	self classOrMetaClassOrganizer
		addCategory: newName
		before: selectedMessageCategory.
	self changed: #messageCategoryList.
	self messageCategoryListIndex:
		(oldIndex = 0
			ifTrue: [self classOrMetaClassOrganizer categories size + 1]
			ifFalse: [oldIndex]).
	self changed: #messageCategoryList.
	

</details>

#### Browser>>#compileMessage: aText notifying: aRequestor

Compile the code that was accepted by the user, placing the compiled method into an appropriate message category. Return true if the compilation succeeded, else false.


<details>
	<summary>See more</summary>
	
	compileMessage: aText notifying: aRequestor
	"Compile the code that was accepted by the user, placing the compiled method into an appropriate message category.  Return true if the compilation succeeded, else false."

	| fallBackCategory fallBackMessage originalSelectorName result |

	self selectedMessageCategoryName ifNil: [
		self selectOriginalCategoryForCurrentMethod 	
			ifFalse: [ "Select the '--all--' category"
				self messageCategoryListIndex: 1]]. 


	self selectedMessageCategoryName asSymbol == ClassOrganizer allCategory
		ifTrue: [
			"User tried to save a method while the ALL category was selected"
			fallBackCategory _ selectedMessageCategory.
			fallBackMessage _ selectedMessage.
			editSelection == #newMessage
				ifTrue: [
					"Select the 'as yet unclassified' category"
					selectedMessageCategory _ nil.
					(result _ self defineMessageFrom: aText notifying: aRequestor)
						ifNil: [
							"Compilation failure:  reselect the original category & method"
							selectedMessageCategory _ fallBackCategory.
							selectedMessage _ fallBackMessage ]
						ifNotNil: [
							self setSelector: result]]
				ifFalse: [
					originalSelectorName _ self selectedMessageName.
					self setOriginalCategoryIndexForCurrentMethod.
					selectedMessage _ fallBackMessage _ originalSelectorName.
					(result _ self defineMessageFrom: aText notifying: aRequestor)
						ifNotNil: [
							self setSelector: result]
						ifNil: [
							"Compilation failure:  reselect the original category & method"
							selectedMessageCategory _ fallBackCategory.
							selectedMessage _ fallBackMessage.
							^ result notNil]].
			self changed: #messageCategoryList.
			^ result notNil]
		ifFalse: [
			"User tried to save a method while the ALL category was NOT selected"
			^ (self defineMessageFrom: aText notifying: aRequestor) notNil ]
</details>

#### Browser>>#classList

<details>
	<summary>See more</summary>
	
	classList

	^ self listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self defaultClassList].
</details>

#### Browser>>#changeSystemCategories: aString

Update the class categories by parsing the argument aString.


<details>
	<summary>See more</summary>
	
	changeSystemCategories: aString 
	"Update the class categories by parsing the argument aString."

	systemOrganizer changeFromString: aString.
	self changed: #systemCategoryList.
	^ true
</details>

#### Browser>>#insertSuperclass

<details>
	<summary>See more</summary>
	
	insertSuperclass

	self selectedClassOrMetaClass ifNotNil: [ :aBehavior |
		(InsertSuperclassApplier on: self for: aBehavior theNonMetaClass) value ].
</details>

#### Browser>>#removeEmptyCategories

<details>
	<summary>See more</summary>
	
	removeEmptyCategories
	self selectedClassOrMetaClass organization removeEmptyCategories.
	self changed: #messageCategoryList

</details>

#### Browser>>#setSelector: aSymbol

Make the receiver point at the given selector, in the currently chosen class


<details>
	<summary>See more</summary>
	
	setSelector: aSymbol
	"Make the receiver point at the given selector, in the currently chosen class"

	| aClass messageCatIndex |
	aSymbol ifNil: [^ self].
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ self].
	messageCatIndex _ aClass organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: messageCatIndex + 1.
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex:
			((aClass organization listAtCategoryNumber: messageCatIndex)
					indexOf: aSymbol)
</details>

#### Browser>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol

	^ aSymbol == #Browser or: [ super is: aSymbol ]
</details>

#### Browser>>#isEditingExistingClass

<details>
	<summary>See more</summary>
	
	isEditingExistingClass

	^editSelection == #editClass
</details>

#### Browser>>#selectOriginalCategoryForCurrentMethod

private - Select the message category for the current method. Note: This should only be called when somebody tries to save a method that they are modifying while ALL is selected. Returns: true on success, false on failure.


<details>
	<summary>See more</summary>
	
	selectOriginalCategoryForCurrentMethod
	"private - Select the message category for the current method. 
	 
	 Note:  This should only be called when somebody tries to save  
	 a method that they are modifying while ALL is selected. 
	 
	 Returns: true on success, false on failure."
	| aSymbol selectorName |
	aSymbol _ self categoryOfCurrentMethod.
	selectorName _ self selectedMessageName.
	(aSymbol notNil and: [aSymbol ~~ ClassOrganizer allCategory])
		ifTrue: [
			selectedMessageCategory _ aSymbol.
			selectedMessage _ selectorName.
			self changed: #messageCategorySelectionChanged.
			self changed: #messageCategoryListIndex.	"update my selection"
			self changed: #messageList.
			self changed: #messageListIndex.
			^ true].
	^ false
</details>

#### Browser>>#categoryOfCurrentMethod

Determine the method category associated with the receiver at the current moment, or nil if none


<details>
	<summary>See more</summary>
	
	categoryOfCurrentMethod
	"Determine the method category associated with the receiver at the current moment, or nil if none"

	| category |
	^ super categoryOfCurrentMethod ifNil: [
		category _ selectedMessageCategory.
		category == ClassOrganizer allCategory
			ifTrue: [nil]
			ifFalse: [category]]
</details>

#### Browser>>#debugMethodTest

<details>
	<summary>See more</summary>
	
	debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ifCanNot: [ PopUpMenu inform: TestCase canNotDebugMethodErrorDescription ]]

</details>

#### Browser>>#initializeListClassesHierachically

<details>
	<summary>See more</summary>
	
	initializeListClassesHierachically

	listClassesHierarchically _ self class listClassesHierarchically 
</details>

#### Browser>>#metaClassIndicated: trueOrFalse

Indicate whether browsing instance or class messages.


<details>
	<summary>See more</summary>
	
	metaClassIndicated: trueOrFalse 
	"Indicate whether browsing instance or class messages."

	metaClassIndicated _ trueOrFalse.
	self setClassOrganizer.
	selectedSystemCategory ifNotNil: [
		self editSelection: (selectedClassName
			ifNil: [metaClassIndicated
				ifTrue: [#none]
				ifFalse: [#newClass]]
			ifNotNil: [#editClass])].
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self changed: #classSelectionChanged.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged
</details>

#### Browser>>#removeMessageFromBrowser

Our list speaks the truth and can't have arbitrary things removed


<details>
	<summary>See more</summary>
	
	removeMessageFromBrowser
	"Our list speaks the truth and can't have arbitrary things removed"

	^ self changed: #flash
</details>

#### Browser>>#hierarchy

Display the inheritance hierarchy of the receiver's selected class.


<details>
	<summary>See more</summary>
	
	hierarchy
	"Display the inheritance hierarchy of the receiver's selected class."

	selectedClassName ifNil: [^ self].
	self messageCategoryListIndex: 0.
	self editSelection: #hierarchy.
	self changed: #editComment.
	self acceptedContentsChanged.
	^ self
</details>

#### Browser>>#setClass: aBehavior selector: aSymbol

Set the state of a new, uninitialized Browser.


<details>
	<summary>See more</summary>
	
	setClass: aBehavior selector: aSymbol
	"Set the state of a new, uninitialized Browser."

	| isMeta aClass messageCatIndex |
	aBehavior ifNil: [^ self].
	aBehavior isMeta
		ifTrue: [
			isMeta _ true.
			aClass _ aBehavior soleInstance]
		ifFalse: [
			isMeta _ false.
			aClass _ aBehavior].
	self selectCategoryForClass: aClass.
	self selectClass: aClass.
	self metaClassIndicated: isMeta.
	aSymbol ifNil: [^ self].
	messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
	self messageCategoryListIndex: (messageCatIndex > 0
		ifTrue: [messageCatIndex + 1]
		ifFalse: [0]).
	messageCatIndex = 0 ifTrue: [^ self].
	self messageListIndex: (
		(aBehavior organization listAtCategoryNumber: messageCatIndex)
			indexOf: aSymbol).
</details>

#### Browser>>#alphabetizeMessageCategories

<details>
	<summary>See more</summary>
	
	alphabetizeMessageCategories
	selectedClassName ifNil: [^ false].
	self classOrMetaClassOrganizer sortCategories.
	self changed: #clearUserEdits.
	self editClass.
	self classListIndex: self classListIndex.
	^ true
</details>

#### Browser>>#originalSelectedClassName

Returns the selectedClassName no matter if it exits or not. It is used for refreshing the browser when renaming a class - Hernan


<details>
	<summary>See more</summary>
	
	originalSelectedClassName
	"Returns the selectedClassName no matter if it exits or not.
	It is used for refreshing the browser when renaming a class - Hernan"

	^selectedClassName
</details>

#### Browser>>#setClassOrganizer

Install whatever organization is appropriate


<details>
	<summary>See more</summary>
	
	setClassOrganizer

	"Install whatever organization is appropriate"

	| theClass |

	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	selectedClassName ifNil: [^ self].	
	theClass := self selectedClass ifNil: [ ^self ].
	classOrganizer _ theClass organization.
	metaClassOrganizer _ theClass class organization.
</details>

#### Browser>>#categorizeAllUncategorizedMethods

Categorize methods by looking in parent classes for a method category.


<details>
	<summary>See more</summary>
	
	categorizeAllUncategorizedMethods
	"Categorize methods by looking in parent classes for a method category."

	| organizer organizers |
	organizer _ self classOrMetaClassOrganizer.
	organizers _ self selectedClassOrMetaClass withAllSuperclasses collect: [:ea | ea organization].
	(organizer listAtCategoryNamed: ClassOrganizer default) do: [:sel | | found |
		found _ (organizers collect: [ :org | org categoryOfElement: sel])
			detect: [:ea | ea ~= ClassOrganizer default and: [ ea notNil]]
			ifNone: nil.
		found ifNotNil: [organizer classify: sel under: found]].

	self changed: #messageCategoryList
</details>

#### Browser>>#selectedMessageName

Answer the message selector of the currently selected message, if any. Answer nil otherwise.


<details>
	<summary>See more</summary>
	
	selectedMessageName
	"Answer the message selector of the currently selected message, if any. 
	Answer nil otherwise."
	^selectedMessage
</details>

#### Browser>>#moveSystemCategoryTop

<details>
	<summary>See more</summary>
	
	moveSystemCategoryTop
	selectedSystemCategory ifNil: [^ self].
	systemOrganizer moveCategoryTop: selectedSystemCategory.
	self systemCategoryListIndex: 1.
	self changed: #systemCategoryList
</details>

#### Browser>>#categorizeUnderNewCategorySelector: aSelectorToCategorize

<details>
	<summary>See more</summary>
	
	categorizeUnderNewCategorySelector: aSelectorToCategorize

	self selectedClassOrMetaClass ifNotNil: [ :class | | newCategory |
		newCategory _ self newMethodCategoryNameIfNone: [ ^self ].
		class organization 
			addCategory: newCategory;
			classify: aSelectorToCategorize under: newCategory suppressIfDefault: false.
			
		self changed: #messageCategoryList.
		self changed: #messageList]
</details>

#### Browser>>#plusButtonHit

Cycle among definition, comment, and hierachy


<details>
	<summary>See more</summary>
	
	plusButtonHit
	"Cycle among definition, comment, and hierachy"

	editSelection == #editComment
		ifTrue: [
			self hierarchy.
			^ self].
	editSelection == #hierarchy
		ifTrue: [
			self editSelection: #editClass.
			selectedClassName ifNil: [ ^self ].
			self changed: #editComment.
			self acceptedContentsChanged.
			^ self].
	self editComment.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
</details>

#### Browser>>#moveSystemCategoryUp

<details>
	<summary>See more</summary>
	
	moveSystemCategoryUp
	selectedSystemCategory ifNil: [^ self].
	self systemCategoryListIndex: 
		(systemOrganizer moveCategoryUp: selectedSystemCategory).
	self changed: #systemCategoryList
</details>

#### Browser>>#runMethodTest

<details>
	<summary>See more</summary>
	
	runMethodTest
	
	| suite |
	
	suite _ TestSuite forCompiledMethod: currentCompiledMethod.
	(ProgressiveTestRunner for: suite) value 
	
</details>

#### Browser>>#systemCategoryBrowser

Create a new system category browser with initial textual contents set to aString.


<details>
	<summary>See more</summary>
	
	systemCategoryBrowser
	"Create a new system category browser with initial textual 
	contents set to aString."

	| newBrowser |
	selectedSystemCategory
		ifNotNil: [
			newBrowser _ Browser new.
			newBrowser systemCategoryListIndex: self systemCategoryListIndex.
			newBrowser setClass: self selectedClassOrMetaClass selector: self selectedMessageName.
			^newBrowser].
	^nil
</details>

#### Browser>>#annotation

Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver.


<details>
	<summary>See more</summary>
	
	annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |
	(aClass _ self selectedClassOrMetaClass)
		ifNil: [^ ''].
	self editSelection == #editComment
		ifTrue: [^ self annotationForSelector: #Comment ofClass: aClass].
	self isEditingExistingClass 
		ifTrue: [^ self annotationForSelector: #Definition ofClass: aClass].
	(aSelector _ self selectedMessageName)
		ifNil: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass
</details>

#### Browser>>#messageCategoryList

Answer the selected category of messages.


<details>
	<summary>See more</summary>
	
	messageCategoryList
	"Answer the selected category of messages."
	^selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ (Array with: ClassOrganizer allCategory), self rawMessageCategoryList ]
</details>

#### Browser>>#selectedMessageName: aSelector

Make the given selector be the selected message name


<details>
	<summary>See more</summary>
	
	selectedMessageName: aSelector
	"Make the given selector be the selected message name"

	| anIndex |
	anIndex _ self messageList indexOf: aSelector.
	anIndex > 0 ifTrue:
		[self messageListIndex: anIndex]
</details>

#### Browser>>#defaultBrowserTitle

<details>
	<summary>See more</summary>
	
	defaultBrowserTitle
	^ 'System Browser'
</details>

#### Browser>>#editMessageCategories

Indicate to the receiver and its dependents that the message categories of the selected class have been changed.


<details>
	<summary>See more</summary>
	
	editMessageCategories
	"Indicate to the receiver and its dependents that the message categories of 
	the selected class have been changed."

	selectedClassName ifNotNil: [
			self messageCategoryListIndex: 0.
			self editSelection: #editMessageCategories.
			self changed: #editMessageCategories.
			self acceptedContentsChanged ]
</details>

#### Browser>>#enableListClassesHierarchically

<details>
	<summary>See more</summary>
	
	enableListClassesHierarchically

	self listClassesHierarchically: true
</details>

#### Browser>>#categorizeUnderNewCategoryClass: aClassName

<details>
	<summary>See more</summary>
	
	categorizeUnderNewCategoryClass: aClassName

	| newCategory |
	
	newCategory _ self newSystemCategoryNameIfNone: [ ^self ].
	
	systemOrganizer
		addCategory: newCategory;
		classify: aClassName withBlanksTrimmed asSymbol under: newCategory.
		
	self changed: #systemCategoryList.
	self changed: #classList.
</details>

#### Browser>>#selectedMessage

Answer a copy of the source code for the selected message.


<details>
	<summary>See more</summary>
	
	selectedMessage
	"Answer a copy of the source code for the selected message."

	| class selector method |
	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	method _ class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod _ method.

	^ (self showingDocumentation
		ifFalse: [ self sourceStringPrettifiedAndDiffed ]
		ifTrue: [ self commentContents ])
			copy
</details>

#### Browser>>#potentialClassNames

Answer the names of all the classes that could be viewed in this browser. This hook is provided so that HierarchyBrowsers can indicate their restricted subset. For generic Browsers, the entire list of classes known to Smalltalk is provided, though of course that really only is accurate in the case of full system browsers.


<details>
	<summary>See more</summary>
	
	potentialClassNames
	"Answer the names of all the classes that could be viewed in this browser.  This hook is provided so that HierarchyBrowsers can indicate their restricted subset.  For generic Browsers, the entire list of classes known to Smalltalk is provided, though of course that really only is accurate in the case of full system browsers."

	^ Smalltalk classNames
</details>

#### Browser>>#classListIndex

Answer the index of the current class selection.


<details>
	<summary>See more</summary>
	
	classListIndex
	"Answer the index of the current class selection."

	^self classListIndexOf: selectedClassName 
</details>

#### Browser>>#flattenHierarchyTree: classHierarchy on: col indent: indent

<details>
	<summary>See more</summary>
	
	flattenHierarchyTree: classHierarchy on: col indent: indent

	^ self
		flattenHierarchyTree: classHierarchy
		on: col
		indent: indent
		by: '  '.
</details>

#### Browser>>#classMessagesIndicated

Answer whether the messages to be presented should come from the metaclass.


<details>
	<summary>See more</summary>
	
	classMessagesIndicated
	"Answer whether the messages to be presented should come from the metaclass."

	^ self metaClassIndicated and: [self classCommentIndicated not]
</details>

#### Browser>>#listClassesHierarchically

I check for nil to support migration on already opened browser when the change is loaded in image - Hernan


<details>
	<summary>See more</summary>
	
	listClassesHierarchically 

	"I check for nil to support migration on already opened browser when the change is loaded in image - Hernan"
	^listClassesHierarchically ifNil: [ self initializeListClassesHierachically]
</details>

#### Browser>>#selectCategoryForClass: theClass

<details>
	<summary>See more</summary>
	
	selectCategoryForClass: theClass

	self systemCategoryListIndex: (self systemCategoryList indexOf: theClass category)

</details>

#### Browser>>#newMethodCategoryNameIfNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	newMethodCategoryNameIfNone: aNoneBlock

	| labels lines menuIndex newName reject |
	
	labels _ OrderedCollection with: 'new...'.
	reject _ Set new.
	reject
		addAll: self selectedClassOrMetaClass organization categories;
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection new.
	self selectedClassOrMetaClass allSuperclasses do: [:cls | | cats |
		cls = Object ifFalse: [
			cats _ cls organization categories reject:
				 [:cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: cats asArray sort.
				reject addAll: cats]]].
	newName _ (labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: 'Add Category'.
		menuIndex = 0 ifTrue: [^ aNoneBlock value].
		menuIndex = 1])
			ifTrue: [
				self request: 'Please type new category name'
					initialAnswer: 'category name']
			ifFalse: [
				labels at: menuIndex].
	
	^ newName isEmpty
		ifTrue: aNoneBlock
		ifFalse: [newName asSymbol].

</details>

#### Browser>>#editComment

Retrieve the description of the class comment.


<details>
	<summary>See more</summary>
	
	editComment
	"Retrieve the description of the class comment."

	selectedClassName ifNil: [ ^self ].
	self messageCategoryListIndex: 0.
	metaClassIndicated _ false.
	self editSelection: #editComment.
	self changed: #classSelectionChanged.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self acceptedContentsChanged

</details>

#### Browser>>#contentsSelection

Return the interval of text in the code pane to select when I set the pane's contents


<details>
	<summary>See more</summary>
	
	contentsSelection
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^(selectedMessageCategory notNil and: [ selectedMessage isNil ])
		ifTrue: [ #all ]	"entire empty method template"
	
	"or null selection"
</details>

#### Browser>>#selectedMessageCategoryName

Answer the name of the selected message category, if any. Answer nil otherwise.


<details>
	<summary>See more</summary>
	
	selectedMessageCategoryName
	"Answer the name of the selected message category, if any. Answer nil 
	otherwise."

	^selectedMessageCategory
</details>

#### Browser>>#editSystemCategories

Retrieve the description of the class categories of the system organizer.


<details>
	<summary>See more</summary>
	
	editSystemCategories
	"Retrieve the description of the class categories of the system organizer."

	self systemCategoryListIndex: 0.
	self editSelection: #editSystemCategories.
	self changed: #editSystemCategories.
	self acceptedContentsChanged
</details>

#### Browser>>#moveSystemCategoryBottom

<details>
	<summary>See more</summary>
	
	moveSystemCategoryBottom
	selectedSystemCategory ifNil: [^ self].
	self systemCategoryListIndex:
		(systemOrganizer moveCategoryBottom: selectedSystemCategory).
	self changed: #systemCategoryList
</details>

#### Browser>>#indicateInstanceMessages

Indicate that the message selection should come from the class (instance) messages.


<details>
	<summary>See more</summary>
	
	indicateInstanceMessages
	"Indicate that the message selection should come from the class (instance) 
	messages."

	self metaClassIndicated: false
</details>

#### Browser>>#newClassComment: aText

The user has just entered aText. It may be all red (a side-effect of replacing the default comment), so remove the color if it is.


<details>
	<summary>See more</summary>
	
	newClassComment: aText
	"The user has just entered aText.
	It may be all red (a side-effect of replacing the default comment), so remove the color if it is."
	| theClass |
	theClass _ self selectedClassOrMetaClass theNonMetaClass.
	theClass ifNotNil: [
		theClass classComment: aText asString ].
	self changed: #classCommentText.
	^ true
</details>

#### Browser>>#showHomeCategory

Show the home category of the selected method. This is only really useful if one is in a tool that supports the showing of categories. Thus, it's good in browsers and hierarchy browsers but not in message-list browsers


<details>
	<summary>See more</summary>
	
	showHomeCategory
	"Show the home category of the selected method.  This is only really useful if one is in a tool that supports the showing of categories.  Thus, it's good in browsers and hierarchy browsers but not in message-list browsers"

	| aSelector |
	(aSelector _ self selectedMessageName) ifNotNil: [
		self selectOriginalCategoryForCurrentMethod.
		self selectedMessageName: aSelector]
</details>

#### Browser>>#fileOutMessageCategories

Print a description of the selected message category of the selected class onto an external file.


<details>
	<summary>See more</summary>
	
	fileOutMessageCategories
	"Print a description of the selected message category of the selected class 
	onto an external file."

	selectedMessageCategory ifNotNil: [
		self selectedClassOrMetaClass fileOutCategory:
			self selectedMessageCategoryName ]
</details>

#### Browser>>#listClassesHierarchically: aBoolean

<details>
	<summary>See more</summary>
	
	listClassesHierarchically: aBoolean

	listClassesHierarchically := aBoolean.
	
	self changed: #classList.
	self changed: #classListIndex.

</details>

#### Browser>>#isEditingMethod

<details>
	<summary>See more</summary>
	
	isEditingMethod

	^editSelection = #editMessage or: [ editSelection = #newMessage ]

</details>

#### Browser>>#runClassTests

<details>
	<summary>See more</summary>
	
	runClassTests
	
	self selectedClassName ifNotNil: [ :aClassName | | selectedClass |
		selectedClass _ Smalltalk classNamed: aClassName.
		(ProgressiveTestRunner for: (TestSuite forClass: selectedClass)) value ]
</details>

#### Browser>>#acceptedStringOrText

Depending on the current selection, different information is retrieved. Answer a string description of that information. This information is the method of the currently selected class and message.


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."

	| comment theClass latestCompiledMethod |
	latestCompiledMethod _ currentCompiledMethod.
	currentCompiledMethod _ nil.

	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	self isEditingNewClass 
		ifTrue: [^ (theClass _ self selectedClass)
			ifNil: [
				Class template: selectedSystemCategory]
			ifNotNil: [
				Class templateForSubclassOf: theClass category: selectedSystemCategory]].
	self isEditingExistingClass 
		ifTrue: [^ self classDefinitionText ].
	editSelection == #editComment 
		ifTrue: [
			(theClass _ self selectedClass) ifNil: [^ ''].
			comment _ theClass comment.
			currentCompiledMethod _ theClass organization commentRemoteStr.
			^ comment size = 0
				ifTrue: ['This class has not yet been commented.']
				ifFalse: [comment]].
	editSelection == #hierarchy 
		ifTrue: [^ self selectedClassOrMetaClass printHierarchy].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [
			^ (theClass _ self selectedClassOrMetaClass) 
				ifNil: ['']
				ifNotNil: [theClass sourceCodeTemplate]].
	editSelection == #editMessage
		ifTrue: [
			self showingByteCodes ifTrue: [^ self selectedBytecodes].
			currentCompiledMethod _ latestCompiledMethod.
			^ self selectedMessage].

	self error: 'Browser internal error: unknown edit selection.'
</details>

#### Browser>>#messageListIndex: anInteger

Set the selected message selector to be the one indexed by anInteger.


<details>
	<summary>See more</summary>
	
	messageListIndex: anInteger
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList _ self messageList.
	index _ messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage _ index = 0 ifFalse: [ (messageList at: index) string ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self acceptedContentsChanged
</details>

#### Browser>>#reformulateList

If the receiver has a way of reformulating its message list, here is a chance for it to do so


<details>
	<summary>See more</summary>
	
	reformulateList
	"If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	self messageListIndex: 0
</details>

#### Browser>>#renameSystemCategory

Prompt for a new category name and add it before the current selection, or at the end if no current selection 21-Mar-2012 jmv Note: This is not recorded appropriately in change sets. The easiest solution is to trigger #classRecategorized for all classes in the category. But this is not a real solution, as the resulting changeset would not do a rename, but create a new category (that would go to the bottom) with all the classes. In the meantime, disable the menu entry. This is not so important after all.


<details>
	<summary>See more</summary>
	
	renameSystemCategory
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection
	
	21-Mar-2012 jmv Note: This is not recorded appropriately in change sets.
	The easiest solution is to trigger #classRecategorized for all classes in the category.
	But this is not a real solution, as the resulting changeset would not do a rename,
	but create a new category (that would go to the bottom) with all the classes.
	
	In the meantime, disable the menu entry. This is not so important after all.
	"
	| oldIndex oldName newName |
	selectedSystemCategory ifNil: [ ^ self].  "no selection"
	oldIndex _ self systemCategoryListIndex.
	oldName _ selectedSystemCategory.
	newName _ self
		request: 'Please type new category name'
		initialAnswer: oldName.
	newName isEmpty
		ifTrue: [^ self]
		ifFalse: [newName _ newName asSymbol].
	oldName = newName ifTrue: [^ self].
	systemOrganizer
		renameCategory: oldName
		toBe: newName.
	self systemCategoryListIndex: oldIndex.
	self changed: #systemCategoryList.
</details>

## ClassListBrowser

A ClassListBrowser displays the code for an arbitrary list of classes. ClassListBrowser example1. "all classes that have the string 'Pluggable' in their names" ClassListBrowser example2. "all classes whose names start with the letter S" ClassListBrowser example3. "all variable classes" ClassListBrowser example4. "all classes with more than 100 methods" ClassListBrowser example5. "all classes that lack class comments" ClassListBrowser example6. "all classes that have class instance variables" ClassListBrowser new initForClassesNamed: #(Browser Boolean) title: 'Browser and Boolean!'.

### Methods
#### ClassListBrowser>>#labelString

Answer the label strilng to use on the browser


<details>
	<summary>See more</summary>
	
	labelString
	"Answer the label strilng to use on the browser"

	^ defaultTitle ifNil: [super labelString]
</details>

#### ClassListBrowser>>#initForClassesNamed: nameList title: aTitle

Initialize the receiver for the class-name-list and title provided


<details>
	<summary>See more</summary>
	
	initForClassesNamed: nameList title: aTitle
	"Initialize the receiver for the class-name-list and title provided"

	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ false.
	defaultTitle _ aTitle.
	classList _ nameList copy
</details>

## HierarchyBrowser

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### HierarchyBrowser>>#changed: sym

Receiver changed. The change is denoted by the argument aParameter. Usually the argument is a Symbol that is part of the dependent's change protocol. Inform all of the dependents.


<details>
	<summary>See more</summary>
	
	changed: sym
	sym == #classList ifTrue: [self updateAfterClassChange].
	super changed: sym
</details>

#### HierarchyBrowser>>#classListIndex: newIndex

Cause system organization to reflect appropriate category


<details>
	<summary>See more</summary>
	
	classListIndex: newIndex

	"Cause system organization to reflect appropriate category"

	| newClassName ind i |

	(classList isInBounds: newIndex) ifTrue: [
		newClassName _ (classList at: newIndex) withoutLeadingBlanks.
		i _ systemOrganizer numberOfCategoryOfElement: newClassName.
		selectedSystemCategory _ i = 0 ifFalse: [ self systemCategoryList at: i]].
	ind _ super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ ind
</details>

#### HierarchyBrowser>>#initAlphabeticListing

<details>
	<summary>See more</summary>
	
	initAlphabeticListing
	| tab stab index |
	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ false.
	classList _ Smalltalk classNames.
</details>

#### HierarchyBrowser>>#systemCategorySingleton

<details>
	<summary>See more</summary>
	
	systemCategorySingleton

	| cls |
	cls _ self selectedClass.
	^ cls ifNil: [Array new]
		ifNotNil: [Array with: cls category]
</details>

#### HierarchyBrowser>>#assureSelectionsShow

This is a workaround for the fact that a hierarchy browser, when launched, often does not show the selected class. (jmv) Is this really needed?


<details>
	<summary>See more</summary>
	
	assureSelectionsShow
	"This is a workaround for the fact that a hierarchy browser, when launched, often does not show the selected class.
	(jmv) Is this really needed?"

	| saveCatIndex saveMsgIndex |
	saveCatIndex _ self messageCategoryListIndex.
	saveMsgIndex _ self messageListIndex.
	self classListIndex: self classListIndex.
	self messageCategoryListIndex: saveCatIndex.
	self messageListIndex: saveMsgIndex
</details>

#### HierarchyBrowser>>#defaultBrowserTitle

<details>
	<summary>See more</summary>
	
	defaultBrowserTitle
	^ 'Hierarchy Browser'
</details>

#### HierarchyBrowser>>#selectClass: classNotMeta

<details>
	<summary>See more</summary>
	
	selectClass: classNotMeta
	
	self classListIndex: (self classListIndexOf: classNotMeta name)
</details>

#### HierarchyBrowser>>#removeSystemCategory

If a class category is selected, create a Confirmer so the user can verify that the currently selected class category and all of its classes should be removed from the system. If so, remove it.


<details>
	<summary>See more</summary>
	
	removeSystemCategory
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	self inform: 'Use a normal Browser, in which you can see 
the entire category you are trying to remove.'
</details>

#### HierarchyBrowser>>#potentialClassNames

Answer the names of all the classes that could be viewed in this browser


<details>
	<summary>See more</summary>
	
	potentialClassNames
	
	"Answer the names of all the classes that could be viewed in this browser"
	
	^ self classList collect: [:aName | aName withoutLeadingBlanks ]
</details>

#### HierarchyBrowser>>#selectedClassName

Answer the name of the class currently selected. di bug fix for the case where name cannot be found -- return nil rather than halt


<details>
	<summary>See more</summary>
	
	selectedClassName
	"Answer the name of the class currently selected.   di
	  bug fix for the case where name cannot be found -- return nil rather than halt"

	| aName |
	
	aName _ super selectedClassName.
	^ aName ifNotNil: [aName withoutLeadingBlanks asSymbol]
</details>

#### HierarchyBrowser>>#initHierarchyForClass: aClassOrMetaClass

<details>
	<summary>See more</summary>
	
	initHierarchyForClass: aClassOrMetaClass
	| tab stab index nonMetaClass |
	centralClass _ aClassOrMetaClass.
	nonMetaClass _ aClassOrMetaClass theNonMetaClass.
	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ aClassOrMetaClass isMeta.
	classList _ OrderedCollection new.
	tab _ ''.
	nonMetaClass allSuperclasses reverseDo: 
		[:aClass | 
		classList add: tab , aClass name.
		tab _ tab , '  '].
	index _ classList size + 1.
	nonMetaClass allSubclassesWithLevelDo:
		[:aClass :level |
		stab _ ''.  1 to: level do: [:i | stab _ stab , '  '].
		classList add: tab , stab , aClass name]
	 	startingLevel: 0.
	self classListIndex: index
</details>

#### HierarchyBrowser>>#classList

<details>
	<summary>See more</summary>
	
	classList

	classList _ classList select: [:each | Smalltalk includesKey: each withoutLeadingBlanks asSymbol].
	^ classList
</details>

#### HierarchyBrowser>>#updateAfterClassChange

It is possible that some the classes comprising the hierarchy have changed, so reinitialize the entire browser.


<details>
	<summary>See more</summary>
	
	updateAfterClassChange
	"It is possible that some the classes comprising the hierarchy have changed, so reinitialize the entire browser."

	(centralClass notNil and: [centralClass isObsolete not])
		ifTrue: [self initHierarchyForClass: centralClass]
</details>

## MessageNames

Search for message names. There are several special characters that alter how searchString is interpreted: $; - separate several search criteria (like 'editorClassFor:;contentsSelection') $* - matches a string pattern rather than just a simple string match (i.e. 'set*text') $# - matches a single character (for example, 'ini###lize'

### Methods
#### MessageNames>>#contentsSelection

Return the interval of text in the search pane to select when I set the pane's contents


<details>
	<summary>See more</summary>
	
	contentsSelection
	"Return the interval of text in the search pane to select when I set the pane's contents"

	^ #all 		"all of it"
</details>

#### MessageNames>>#selectorListIndex

Answer the selectorListIndex


<details>
	<summary>See more</summary>
	
	selectorListIndex
	"Answer the selectorListIndex"

	selectedSelector ifNil: [ ^ 0 ].
	^self selectorList indexOf: selectedSelector
</details>

#### MessageNames>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	searchString _ ''
</details>

#### MessageNames>>#labelString

<details>
	<summary>See more</summary>
	
	labelString
	^ 'Message names containing "', searchString asLowercase, '"'
</details>

#### MessageNames>>#searchString

Answer the current searchString


<details>
	<summary>See more</summary>
	
	searchString
	"Answer the current searchString"

	^ searchString
</details>

#### MessageNames>>#computeSelectorListFromSearchString

Compute selector list from search string


<details>
	<summary>See more</summary>
	
	computeSelectorListFromSearchString
	"Compute selector list from search string"
	| sorted |
	sorted _ SortedCollection sortBlock: [ :x :y |
		x asLowercase <= y asLowercase ].
	(searchString findBetweenSubStrs: ';') do: [ :selPat | | raw n m |
		selPat size > 0 ifTrue: [
			(selPat first = $" and: [
				selPat last = $" and: [ selPat size > 2 ]])
				ifTrue: [
					Symbol
						hasInterned:
							(selPat
								copyFrom: 2
								to: selPat size - 1)
						ifTrue: [ :sym |
							sorted add: sym ]]
				ifFalse: [
					raw _ ((n _ selPat occurrencesOf: $*) > 0 | ((m _ selPat occurrencesOf: $#) > 0) and: [ selPat size > (n + m) ])
						ifTrue: [ Symbol selectorsMatching: selPat ]
						ifFalse: [ Symbol selectorsContaining: selPat ].
					sorted addAll: raw ]]].
	selectorList _ sorted asArray  select: [ :sym |
		(Smalltalk isThereAnImplementorOf: sym) or: [ Smalltalk isThereAReferenceTo: sym ]].
	^ selectorList.
</details>

#### MessageNames>>#messageList

Answer the receiver's message list, computing it if necessary. The way to force a recomputation is to set the messageList to nil


<details>
	<summary>See more</summary>
	
	messageList
	"Answer the receiver's message list, computing it if necessary. The way 
	to force a recomputation is to set the messageList to nil"
	messageList
		ifNil: [
			messageList _ selectedSelector
				ifNil: [#()]
				ifNotNil: [
					Smalltalk allImplementorsOf: selectedSelector].
			self initializeMessageList: messageList.
			self messageListIndex: (messageList size > 0
				ifTrue: [1]
				ifFalse: [0])
			].
	^ messageList
</details>

#### MessageNames>>#selectorList

Answer the selectorList


<details>
	<summary>See more</summary>
	
	selectorList
	"Answer the selectorList"

	selectorList ifNil: [
		self computeSelectorListFromSearchString.
		selectedSelector _ selectorList isEmpty ifFalse: [ selectorList first ].
		messageList _ nil ].
	^ selectorList
</details>

#### MessageNames>>#searchString: aString

Take what the user typed and find all selectors containing it


<details>
	<summary>See more</summary>
	
	searchString: aString
	"Take what the user typed and find all selectors containing it"

	searchString _ aString asString.
	self changed: #relabel.
	selectorList _ nil.
	self changed: #selectorList.
	self changed: #messageList.
	^ true
</details>

#### MessageNames>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	textGetter = #searchString ifTrue: [
		^SmalltalkEditor ].
	^super editorClassFor: textGetter
</details>

#### MessageNames>>#selectorListIndex: anInteger

Set the selectorListIndex as specified, and propagate consequences


<details>
	<summary>See more</summary>
	
	selectorListIndex: anInteger 
	"Set the selectorListIndex as specified, and propagate consequences"

	selectedSelector _ anInteger = 0 ifFalse: [ self selectorList at: anInteger ].
	anInteger = 0
		ifTrue: [^ self].
	messageList _ nil.
	self changed: #selectorListIndex.
	self changed: #messageList
</details>

#### MessageNames>>#selectedMessageName

Answer the name of the currently selected message.


<details>
	<summary>See more</summary>
	
	selectedMessageName
	"Answer the name of the currently selected message."
	^selectedSelector
</details>

## MessageSet

Note: In Squeak, this class is not subclass of Browser anymore. See http://lists.squeakfoundation.org/pipermail/packages/2011-June/004764.html . It might make sense to do the same in Cuis. I represent a query path of the retrieval result of making a query about methods in the system. The result is a set of methods, denoted by a message selector and the class in which the method was found. As a TextProvider, the string I represent is the source code of the currently selected method. I am typically viewed in a Message Set Browser consisting of a MessageListView and a BrowserCodeView. Ivar messageList is a collection of MethodReferences. Ivar selectedMessage is not as symbol (as in superclasses) but the selected MethodReference.

### Methods
#### MessageSet>>#filterToUnsentMessages

Filter the receiver's list down to only those items which have no senders


<details>
	<summary>See more</summary>
	
	filterToUnsentMessages
	"Filter the receiver's list down to only those items which have no  
	senders"
	self filterFrom: [ :aClass :aSelector |
		(Smalltalk isThereAReferenceTo: aSelector) not ].
</details>

#### MessageSet>>#selectedClassOrMetaClass

Answer the currently selected class (or metaclass).


<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	"Answer the currently selected class (or metaclass)."

	^selectedMessage ifNotNil: [ :methodReference | methodReference actualClass ]
</details>

#### MessageSet>>#deleteFromMessageList: aMethodReference

Delete the given message from the receiver's message list


<details>
	<summary>See more</summary>
	
	deleteFromMessageList: aMethodReference
	"Delete the given message from the receiver's message list"

	messageList _ messageList copyWithout: aMethodReference
</details>

#### MessageSet>>#acceptedContentsChanged

<details>
	<summary>See more</summary>
	
	acceptedContentsChanged
	super acceptedContentsChanged.
	autoSelectString ifNotNil: [
		self changed: #autoSelect]
</details>

#### MessageSet>>#filterToMessagesInChangesFile

Filter down only to messages whose source code risides in the Changes file. This allows one to ignore long-standing methods that live in the .sources file.


<details>
	<summary>See more</summary>
	
	filterToMessagesInChangesFile
	"Filter down only to messages whose source code risides in the Changes file.  This allows one to ignore long-standing methods that live in the .sources file."

	| cm |
	self filterFrom:
		[:aClass :aSelector |
			aClass notNil and: [aSelector notNil and:
				[(self class isPseudoSelector: aSelector) not and:
					[(cm _ aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex ~= 1]]]]]
</details>

#### MessageSet>>#canShowMultipleMessageCategories

Answer whether the receiver is capable of showing multiple message categories


<details>
	<summary>See more</summary>
	
	canShowMultipleMessageCategories
	"Answer whether the receiver is capable of showing multiple message categories"

	^ false
</details>

#### MessageSet>>#filterToCurrentAuthor

Filter down only to messages with my initials as most recent author


<details>
	<summary>See more</summary>
	
	filterToCurrentAuthor
	"Filter down only to messages with my initials as most recent author"

	| myInitials aMethod aTimeStamp |
	(myInitials _ Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod _ aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp _ aMethod timeStamp) notNil and:
						[aTimeStamp beginsWith: myInitials]]]]
</details>

#### MessageSet>>#addMethodReference: aMethodReference

Append a aMethodReference to the list. Select the new item.


<details>
	<summary>See more</summary>
	
	addMethodReference: aMethodReference
	"Append a aMethodReference to the list.  Select the new item."

	messageList add: aMethodReference.
	self changed: #messageList.
	self messageListIndex: messageList size
</details>

#### MessageSet>>#messageList

Answer the current list of messages.


<details>
	<summary>See more</summary>
	
	messageList
	"Answer the current list of messages."

	^messageList
</details>

#### MessageSet>>#growable

Answer whether the receiver is capable of growing/shrinking dynamically


<details>
	<summary>See more</summary>
	
	growable
	"Answer whether the receiver is capable of growing/shrinking dynamically"

	^ true
</details>

#### MessageSet>>#removeMessageFromBrowserKeepingLabel

<details>
	<summary>See more</summary>
	
	removeMessageFromBrowserKeepingLabel

	selectedMessage ifNil: [ ^nil ].
	self deleteFromMessageList: self selection.
	self reformulateList.

</details>

#### MessageSet>>#messageSendsRangesOf: aSelector

<details>
	<summary>See more</summary>
	
	messageSendsRangesOf: aSelector

	^ selectedMessage 
		ifNil: [ #() ]
		ifNotNil: [selectedMessage messageSendsRangesOf: aSelector ]
</details>

#### MessageSet>>#methodCategoryChanged

<details>
	<summary>See more</summary>
	
	methodCategoryChanged
	self triggerEvent: #annotationChanged
</details>

#### MessageSet>>#reformulateListNoting: newSelector

A method has possibly been submitted for the receiver with newSelector as its selector; If the receiver has a way of reformulating its message list, here is a chance for it to do so


<details>
	<summary>See more</summary>
	
	reformulateListNoting: newSelector
	"A method has possibly been submitted for the receiver with newSelector as its selector; If the receiver has a way of reformulating its message list, here is a chance for it to do so"

	^ self reformulateList
</details>

#### MessageSet>>#selectedMessage

Answer the source method for the currently selected message.


<details>
	<summary>See more</summary>
	
	selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment ].
		selector == #Definition ifTrue: [
			^ class definition ].
		selector == #Hierarchy ifTrue: [
			^ class printHierarchy ]].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed
</details>

#### MessageSet>>#filterToCommentedMethods

Filter the receiver's list down to only those items which have comments


<details>
	<summary>See more</summary>
	
	filterToCommentedMethods
	"Filter the receiver's list down to only those items which have comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil not]]
</details>

#### MessageSet>>#filterToNotSendersOf

Filter the receiver's list down to only those items which do not send a given selector


<details>
	<summary>See more</summary>
	
	filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector"

	| aFragment inputWithBlanksTrimmed aMethod |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					(aMethod _ aClass compiledMethodAt: aSelector) isNil or:
						[(aMethod hasLiteralThorough: aSymbol) not]]]
</details>

#### MessageSet>>#metaClassIndicated

Answer the boolean flag that indicates whether this is a class method.


<details>
	<summary>See more</summary>
	
	metaClassIndicated
	"Answer the boolean flag that indicates whether
	this is a class method."

	^ self selectedClassOrMetaClass isMeta
</details>

#### MessageSet>>#filterToSendersOf

Filter the receiver's list down to only those items which send a given selector


<details>
	<summary>See more</summary>
	
	filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector"

	| aFragment inputWithBlanksTrimmed aMethod |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					(aMethod _ aClass compiledMethodAt: aSelector) notNil and:
						[aMethod hasLiteralThorough: aSymbol]]]


</details>

#### MessageSet>>#addMethodReference: aMethodReferenceToAdd ifIncluded: aBlockClosure

<details>
	<summary>See more</summary>
	
	addMethodReference: aMethodReferenceToAdd ifIncluded: aBlockClosure

	(messageList includes: aMethodReferenceToAdd)
		ifTrue: aBlockClosure
		ifFalse: [ self addMethodReference: aMethodReferenceToAdd ]
</details>

#### MessageSet>>#filterToNotAnyChangeSet

Filter down only to messages present in NO change set


<details>
	<summary>See more</summary>
	
	filterToNotAnyChangeSet
	"Filter down only to messages present in NO change set"

	self filterFrom: [ :aClass :aSelector |
		(ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector) not ]
</details>

#### MessageSet>>#selectedClass

Return the base class for the current selection. 1/17/96 sw fixed up so that it doesn't fall into a debugger in a msg browser that has no message selected


<details>
	<summary>See more</summary>
	
	selectedClass 
	"Return the base class for the current selection.  1/17/96 sw fixed up so that it doesn't fall into a debugger in a msg browser that has no message selected"

	^ self selectedClassOrMetaClass
		ifNotNil: [ :c | c theNonMetaClass]
</details>

#### MessageSet>>#initializeMessageList: anArray

<details>
	<summary>See more</summary>
	
	initializeMessageList: anArray

	messageList _ anArray.
	messageList isEmpty
		ifTrue: [ selectedMessage _ nil ]
		ifFalse: [ self messageListIndex: 1 ]
</details>

#### MessageSet>>#filterToMessagesWithoutPriorVersions

Filter down only to messages which have no prior version stored


<details>
	<summary>See more</summary>
	
	filterToMessagesWithoutPriorVersions
	"Filter down only to messages which have no prior version stored"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) <= 1]]]
</details>

#### MessageSet>>#filterToMessagesWithPriorVersions

Filter down only to messages which have at least one prior version


<details>
	<summary>See more</summary>
	
	filterToMessagesWithPriorVersions
	"Filter down only to messages which have at least one prior version"

	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:
				[(self class isPseudoSelector: aSelector) not and:
					[(VersionsBrowser versionCountForSelector: aSelector class: aClass) > 1]]]
</details>

#### MessageSet>>#selectedMessageCategoryName

Answer the name of the selected message category or nil.


<details>
	<summary>See more</summary>
	
	selectedMessageCategoryName 
	"Answer the name of the selected message category or nil."
	"Ver si esta implementacion loca se usa. Si no se usa, eliminar todas (intentarlo!)"
	selectedMessage ifNil: [ ^nil ].
	^ self selectedClassOrMetaClass organization categoryOfElement: self selectedMessageName
</details>

#### MessageSet>>#labelString

<details>
	<summary>See more</summary>
	
	labelString
	^'Filtered: [',
		messageList size printString,
		']'.
</details>

#### MessageSet>>#autoSelectString

Return the string to be highlighted when making new selections


<details>
	<summary>See more</summary>
	
	autoSelectString
	"Return the string to be highlighted when making new selections"
	^ autoSelectString
</details>

#### MessageSet>>#filterFrom: aBlock

Filter the receiver's list down to only those items that satisfy aBlock, which takes a class an a selector as its arguments.


<details>
	<summary>See more</summary>
	
	filterFrom: aBlock
	"Filter the receiver's list down to only those items that satisfy aBlock, which takes a class an a selector as its arguments."
	
	self setFilteredList: (messageList select: [ :methodReference |
		aBlock
			value: methodReference actualClass
			value: methodReference methodSymbol ])
</details>

#### MessageSet>>#autoSelectString: aString

Set the string to be highlighted when making new selections


<details>
	<summary>See more</summary>
	
	autoSelectString: aString
	"Set the string to be highlighted when making new selections"
	autoSelectString _ aString
</details>

#### MessageSet>>#filterToNotImplementorsOf

Filter the receiver's list down to only those items whose selector is NOT one solicited from the user.


<details>
	<summary>See more</summary>
	
	filterToNotImplementorsOf
	"Filter the receiver's list down to only those items whose selector is NOT one solicited from the user."

	| aFragment inputWithBlanksTrimmed |

	aFragment _ FillInTheBlankMorph request: 'type selector: ' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					aSelector ~~ aSymbol]]
</details>

#### MessageSet>>#setFilteredList: newList

Establish newList as the new list if appropriate, and adjust the window title accordingly; if the new list is of the same size as the old, warn and do nothing


<details>
	<summary>See more</summary>
	
	setFilteredList: newList
	"Establish newList as the new list if appropriate, and adjust the window title accordingly; if the new list is of the same size as the old, warn and do nothing"

	newList size = 0
		ifTrue: [
			^ self inform: 'Nothing would be left in the list if you did that'].
	newList size = messageList size
		ifTrue: [
			^ self inform: 'That leaves the list unchanged'].
	self initializeMessageList: newList.
	self changed: #relabel
</details>

#### MessageSet>>#filterToMessagesInSourcesFile

Filter down only to messages whose source code resides in the .sources file.


<details>
	<summary>See more</summary>
	
	filterToMessagesInSourcesFile
	"Filter down only to messages whose source code resides in the .sources file."

	| cm |
	self filterFrom: [:aClass :aSelector |
		(aClass notNil and: [aSelector notNil]) and:
			[(self class isPseudoSelector: aSelector) not and:
				[(cm _ aClass compiledMethodAt: aSelector ifAbsent: nil) notNil and:
					[cm fileIndex = 1]]]]
</details>

#### MessageSet>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true
</details>

#### MessageSet>>#filterToUncommentedMethods

Filter the receiver's list down to only those items which lack comments


<details>
	<summary>See more</summary>
	
	filterToUncommentedMethods
	"Filter the receiver's list down to only those items which lack comments"

	self filterFrom:
		[:aClass :aSelector |
			(aClass selectors includes: aSelector) and:
						[(aClass firstPrecodeCommentFor: aSelector) isEmptyOrNil]]
</details>

#### MessageSet>>#removeMessageFromBrowser

Remove the selected message from the browser.


<details>
	<summary>See more</summary>
	
	removeMessageFromBrowser
	"Remove the selected message from the browser."

	self removeMessageFromBrowserKeepingLabel.
	self changed: #relabel
</details>

#### MessageSet>>#filterToNotCurrentAuthor

Filter down only to messages not stamped with my initials


<details>
	<summary>See more</summary>
	
	filterToNotCurrentAuthor
	"Filter down only to messages not stamped with my initials"

	| myInitials aMethod aTimeStamp |
	(myInitials _ Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod _ aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp _ aMethod timeStamp) isNil or:
						[(aTimeStamp beginsWith: myInitials) not]]]]
</details>

#### MessageSet>>#contents: aString notifying: aRequestor

Compile the code in aString. Notify aRequestor of any syntax errors. Answer false if the compilation fails. Otherwise, if the compilation created a new method, deselect the current selection. Then answer true.


<details>
	<summary>See more</summary>
	
	contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true
</details>

#### MessageSet>>#acceptedStringOrText

Answer the contents of the receiver


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"Answer the contents of the receiver"
	^selectedMessage
		ifNil: [
			currentCompiledMethod _ nil.
			'']
		ifNotNil: [
			self showingByteCodes
				ifTrue: [ self selectedBytecodes ]
				ifFalse: [ self selectedMessage ]]
</details>

#### MessageSet>>#sortByDate

Sort the message-list by date of time-stamp


<details>
	<summary>See more</summary>
	
	sortByDate
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs _ messageList collect: [ :aRef |
		aDate _ aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSubmitted]
			ifFalse: [
				aCompiledMethod _ aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateMethodLastSubmitted]].
		aRef -> (aDate ifNil: [Date fromString: '01/01/1996'])].  "The dawn of Squeak history"
	inOrder _ assocs asArray sort: [ :a :b | a value < b value].

	messageList _ inOrder collect: [:assoc | assoc key].
	self changed: #messageList
</details>

#### MessageSet>>#removeMessage

Remove the selected message from the system. 1/15/96 sw


<details>
	<summary>See more</summary>
	
	removeMessage
	"Remove the selected message from the system. 1/15/96 sw"
	| messageName confirmation |
	selectedMessage ifNil: [ ^self ].
	messageName _ self selectedMessageName.
	confirmation _ Smalltalk confirmRemovalOf: messageName on: self selectedClassOrMetaClass.
	confirmation = 3
		ifTrue: [^ self].
	self selectedClassOrMetaClass removeSelector: messageName.
	self deleteFromMessageList: self selection.
	self reformulateList.
	confirmation = 2
		ifTrue: [Smalltalk browseAllCallsOn: messageName]
</details>

#### MessageSet>>#messageListIndex: anInteger

Set the index of the selected item to be anInteger.


<details>
	<summary>See more</summary>
	
	messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."

	| list |
	
	list _ self messageList.
	selectedMessage _ (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged
</details>

#### MessageSet>>#filterToAnyChangeSet

Filter down only to messages present in ANY change set


<details>
	<summary>See more</summary>
	
	filterToAnyChangeSet
	"Filter down only to messages present in ANY change set"

	self filterFrom: [ :aClass :aSelector |
		ChangeSet doesAnyChangeSetHaveClass: aClass andSelector: aSelector ]
</details>

#### MessageSet>>#selection

Answer the item in the list that is currently selected, or nil if no selection is present


<details>
	<summary>See more</summary>
	
	selection
	"Answer the item in the list that is currently selected, or nil if no selection is present"

	^ selectedMessage
</details>

#### MessageSet>>#filterToImplementorsOf

Filter the receiver's list down to only those items with a given selector


<details>
	<summary>See more</summary>
	
	filterToImplementorsOf
	"Filter the receiver's list down to only those items with a given selector"

	| aFragment inputWithBlanksTrimmed |

	aFragment _ FillInTheBlankMorph request: 'type selector:' initialAnswer: ''.
	aFragment  isEmptyOrNil ifTrue: [^ self].
	inputWithBlanksTrimmed _ aFragment withBlanksTrimmed.
	Symbol hasInterned: inputWithBlanksTrimmed ifTrue:
		[:aSymbol | 
			self filterFrom:
				[:aClass :aSelector |
					aSelector == aSymbol]]
</details>

#### MessageSet>>#reformulateList

The receiver's messageList has been changed; rebuild it


<details>
	<summary>See more</summary>
	
	reformulateList
	"The receiver's messageList has been changed; rebuild it"
	super reformulateList.
	self initializeMessageList: messageList.
	self changed: #messageList.
	self changed: #messageListIndex.
	self acceptedContentsChanged
</details>

#### MessageSet>>#selectedMessageName

Answer the name of the currently selected message.


<details>
	<summary>See more</summary>
	
	selectedMessageName
	"Answer the name of the currently selected message."

	^selectedMessage ifNotNil: [ :methodReference | methodReference methodSymbol ]
</details>

## MethodReference

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MethodReference>>#classIsMeta

<details>
	<summary>See more</summary>
	
	classIsMeta

	^classIsMeta
</details>

#### MethodReference>>#prefixStringVersionWith: aString

<details>
	<summary>See more</summary>
	
	prefixStringVersionWith: aString

	stringVersion := aString, stringVersion 
</details>

#### MethodReference>>#messageSendsRangesOf: aSentSelector

<details>
	<summary>See more</summary>
	
	messageSendsRangesOf: aSentSelector

	| compiledMethod |
	
	compiledMethod := self compiledMethodIfAbsent: [ ^#() ].
	^compiledMethod messageSendsRangesOf: aSentSelector 
</details>

#### MethodReference>>#selector

<details>
	<summary>See more</summary>
	
	selector

	^methodSymbol
</details>

#### MethodReference>>#methodClass

<details>
	<summary>See more</summary>
	
	methodClass

	^self actualClass 
</details>

#### MethodReference>>#hasVariableBindingTo: aClass

<details>
	<summary>See more</summary>
	
	hasVariableBindingTo: aClass

	^(self compiledMethodIfAbsent: [ ^false ]) hasVariableBindingTo: aClass

</details>

#### MethodReference>>#= anotherMethodReference

Answer whether the receiver and the argument represent the same object.


<details>
	<summary>See more</summary>
	
	= anotherMethodReference 
	"Answer whether the receiver and the argument represent the 
	same object."
	self == anotherMethodReference ifTrue: [ ^ true ].
	^ self species == anotherMethodReference species
		and: [self classSymbol == anotherMethodReference classSymbol]
		and: [self classIsMeta == anotherMethodReference classIsMeta]
		and: [self methodSymbol == anotherMethodReference methodSymbol]
</details>

#### MethodReference>>#isValid

Answer whether the receiver represents a current selector or Comment


<details>
	<summary>See more</summary>
	
	isValid
	"Answer whether the receiver represents a current selector or Comment"

	| aClass |
	(aClass _ self actualClass) ifNil: [^ false].
	^ (aClass includesSelector: methodSymbol) or:
		[methodSymbol == #Comment]
</details>

#### MethodReference>>#hash

Answer a SmallInteger whose value is related to the receiver's identity.


<details>
	<summary>See more</summary>
	
	hash
	"Answer a SmallInteger whose value is related to the receiver's  
	identity."
	^ (self species hash bitXor: self classSymbol hash)
		bitXor: self methodSymbol hash
</details>

#### MethodReference>>#actualClassIfAbsent: anAbsentBlock

<details>
	<summary>See more</summary>
	
	actualClassIfAbsent: anAbsentBlock 

	^Smalltalk 
		at: classSymbol 
		ifPresent: [ :actualClass |
			classIsMeta
				ifTrue: [ actualClass class ]
				ifFalse: [ actualClass ] ]
		ifAbsent: anAbsentBlock 

</details>

#### MethodReference>>#printOn: aStream

Print the receiver on a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Print the receiver on a stream"

	super printOn: aStream.
	aStream space.
	self printClassAndSelectorOn: aStream
</details>

#### MethodReference>>#sourceCodeIfAbsent: aBlock

<details>
	<summary>See more</summary>
	
	sourceCodeIfAbsent: aBlock

	| actualClass |

	actualClass := self actualClassIfAbsent: [ ^aBlock value ].
	^actualClass sourceCodeAt: methodSymbol ifAbsent: aBlock
</details>

#### MethodReference>>#setClass: aClass methodSymbol: methodSym stringVersion: aString

<details>
	<summary>See more</summary>
	
	setClass: aClass methodSymbol: methodSym stringVersion: aString 

	classSymbol _ aClass theNonMetaClass name asSymbol.
	classIsMeta _ aClass isMeta.
	methodSymbol _ methodSym asSymbol.
	stringVersion _ aString.
</details>

#### MethodReference>>#stringVersion

<details>
	<summary>See more</summary>
	
	stringVersion

	^stringVersion
</details>

#### MethodReference>>#category

<details>
	<summary>See more</summary>
	
	category
	^ self actualClass organization categoryOfElement: methodSymbol
</details>

#### MethodReference>>#printClassAndSelectorOn: aStream

<details>
	<summary>See more</summary>
	
	printClassAndSelectorOn: aStream
		
	aStream nextPutAll: classSymbol.
	classIsMeta ifTrue: [ aStream nextPutAll: ' class' ].
	aStream
		nextPutAll: '>>#';
		nextPutAll: methodSymbol
</details>

#### MethodReference>>#methodSymbol

<details>
	<summary>See more</summary>
	
	methodSymbol

	^methodSymbol
</details>

#### MethodReference>>#updateReferencesTo: oldClassName toBe: newClassName

<details>
	<summary>See more</summary>
	
	updateReferencesTo: oldClassName toBe: newClassName

	| src ts cm |
	src := self actualClass sourceCodeAt: methodSymbol.
	src := src copyReplaceAll: oldClassName with: newClassName.
	cm := self actualClass compiledMethodAt: methodSymbol.
	ts := cm timeStamp.
	ts = '' ifTrue: [ts := nil].
	self actualClass 
		compile: src
		classified: ClassOrganizer default
		withStamp: ts 
		notifying: nil.

	^true

</details>

#### MethodReference>>#actualClass

<details>
	<summary>See more</summary>
	
	actualClass 

	^self actualClassIfAbsent: [ nil ]
</details>

#### MethodReference>>#setStandardClass: aClass methodSymbol: methodSym

<details>
	<summary>See more</summary>
	
	setStandardClass: aClass methodSymbol: methodSym

	classSymbol _ aClass theNonMetaClass name asSymbol.
	classIsMeta _ aClass isMeta.
	methodSymbol _ methodSym asSymbol.
	stringVersion _ aClass name , ' ' , methodSym.
</details>

#### MethodReference>>#dynamicTypingAutoCompleterDocumentation

<details>
	<summary>See more</summary>
	
	dynamicTypingAutoCompleterDocumentation

	^self compiledMethod dynamicTypingAutoCompleterDocumentation 
</details>

#### MethodReference>>#setClassSymbol: classSym classIsMeta: isMeta methodSymbol: methodSym stringVersion: aString

<details>
	<summary>See more</summary>
	
	setClassSymbol: classSym classIsMeta: isMeta methodSymbol: methodSym stringVersion: aString 

	classSymbol _ classSym asSymbol.
	classIsMeta _ isMeta.
	methodSymbol _ methodSym asSymbol.
	stringVersion _ aString
</details>

#### MethodReference>>#classSymbol

<details>
	<summary>See more</summary>
	
	classSymbol

	^classSymbol
</details>

#### MethodReference>>#compiledMethod

<details>
	<summary>See more</summary>
	
	compiledMethod
	^ self actualClass compiledMethodAt: methodSymbol
</details>

#### MethodReference>>#sourceCode

<details>
	<summary>See more</summary>
	
	sourceCode

	^ (self actualClassIfAbsent: [ self error: self class classDoesNotExistErrorMessage ]) 
		sourceCodeAt: methodSymbol
</details>

#### MethodReference>>#referencesParameterAt: parameterIndex

<details>
	<summary>See more</summary>
	
	referencesParameterAt: parameterIndex

	^(self compiledMethodIfAbsent: [ ^false ]) referencesParameterAt: parameterIndex 
</details>

#### MethodReference>>#displayStringOrText

To be used in the UI


<details>
	<summary>See more</summary>
	
	displayStringOrText
	"To be used in the UI"

	^stringVersion
</details>

#### MethodReference>>#<= anotherMethodReference

<details>
	<summary>See more</summary>
	
	<= anotherMethodReference

	classSymbol < anotherMethodReference classSymbol ifTrue: [^true].
	classSymbol > anotherMethodReference classSymbol ifTrue: [^false].
	classIsMeta == anotherMethodReference classIsMeta ifFalse: [^classIsMeta not].
	^methodSymbol <= anotherMethodReference methodSymbol

</details>

#### MethodReference>>#compiledMethodIfAbsent: ifAbsentBlock

<details>
	<summary>See more</summary>
	
	compiledMethodIfAbsent: ifAbsentBlock

	^ self actualClass
		ifNil: ifAbsentBlock
		ifNotNil: [ :aClass | aClass compiledMethodAt: methodSymbol ifAbsent: ifAbsentBlock ] 
</details>

## ProtocolBrowser

An instance of ProtocolBrowser shows the methods a class understands--inherited or implemented at this level--as a "flattened" list.

### Methods
#### ProtocolBrowser>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	exclude _ OrderedCollection new
</details>

#### ProtocolBrowser>>#growable

Answer whether the receiver is subject to manual additions and deletions


<details>
	<summary>See more</summary>
	
	growable
	"Answer whether the receiver is subject to manual additions and deletions"

	^ false
</details>

#### ProtocolBrowser>>#labelString

Answer the string for the window title


<details>
	<summary>See more</summary>
	
	labelString
	"Answer the string for the window title"
	
	^ 'Protocol for: ', baseClass name, ' up to: ', (selectedName ifNil: [ ProtoObject name asString ])
</details>

#### ProtocolBrowser>>#selectiveClassListIndex: anObject

Set the value of selectiveClassListIndex


<details>
	<summary>See more</summary>
	
	selectiveClassListIndex: anObject
	"Set the value of selectiveClassListIndex"

	selectiveClassListIndex _ anObject.
	self protocolFor: selectiveClassListIndex

</details>

#### ProtocolBrowser>>#hierarchyForClass: aClass

Set the class hierarchy for the list pane


<details>
	<summary>See more</summary>
	
	hierarchyForClass: aClass
	"Set the class hierarchy for the list pane"
	| tab |

	selectiveClassList _ OrderedCollection new.
	tab _ ''.
	aClass withAllSuperclasses reverse do: [:ea | 
		selectiveClassList add: tab , ea name.
		tab _ tab , '  '].
	self classListIndex: 0
</details>

#### ProtocolBrowser>>#onSubProtocolOf: aClass

Initialize with the entire protocol for the class, aClass, but excluding those inherited from Object.


<details>
	<summary>See more</summary>
	
	onSubProtocolOf: aClass
	"Initialize with the entire protocol for the class, aClass,
		but excluding those inherited from Object."
	| selectors |
	selectors _ Set new.
	aClass withAllSuperclasses do: [ :each |
		(each == Object or: [ each == ProtoObject ]) ifFalse: [ selectors addAll: each selectors ]].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass.
</details>

#### ProtocolBrowser>>#selectiveClassList

Answer the value of selectiveClassList


<details>
	<summary>See more</summary>
	
	selectiveClassList
	"Answer the value of selectiveClassList"

	^ selectiveClassList
</details>

#### ProtocolBrowser>>#initListFrom: selectorCollection highlighting: aClass

Make up the messageList with items from aClass in boldface.


<details>
	<summary>See more</summary>
	
	initListFrom: selectorCollection highlighting: aClass 
	"Make up the messageList with items from aClass in boldface."
	| defClass item |

	messageList _ OrderedCollection new.
	selectorCollection do: [ :selector |  
		defClass _ aClass whichClassIncludesSelector: selector.
		item _ selector, '     (' , defClass name , ')'.
		defClass == aClass ifTrue: [item _ item asText allBold].
		messageList add: (
			MethodReference new
				setClass: defClass 
				methodSymbol: selector 
				stringVersion: item)].
	self hierarchyForClass: (baseClass _ aClass)
</details>

#### ProtocolBrowser>>#protocolFor: anIndex

Change the listed protocol


<details>
	<summary>See more</summary>
	
	protocolFor: anIndex
	"Change the listed protocol"

	exclude _ OrderedCollection new.
	anIndex > 0
		ifTrue: [
			selectedName _ (selectiveClassList at: anIndex) withBlanksTrimmed.
			(1 to: anIndex - 1) do: [:ix |
				exclude addLast: (selectiveClassList at: ix) withBlanksTrimmed]]
		ifFalse: [
			selectedName _ nil.
			].
	self on: baseClass.
	self changed: #relabel
</details>

#### ProtocolBrowser>>#selectiveClassListIndex

Answer the value of selectiveClassListIndex


<details>
	<summary>See more</summary>
	
	selectiveClassListIndex
	"Answer the value of selectiveClassListIndex"

	selectiveClassListIndex ifNil: [selectiveClassListIndex _ 0].
	^ selectiveClassListIndex
</details>

#### ProtocolBrowser>>#on: aClass

Initialize the protocol for the class, aClass.


<details>
	<summary>See more</summary>
	
	on: aClass
	"Initialize the protocol for the class, aClass."
	"Optionally, the upper part of the protocol is excluded."
	| selectors |
	
	selectors _ Set new.
	aClass withAllSuperclasses do: [ :each |
		(exclude includes: each name) ifFalse: [selectors addAll: each selectors]].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass
</details>

