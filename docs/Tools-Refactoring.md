## AddInstanceVariable

I can add a new instance variable to a class. Input parameters are: * name of the new variable * class to add that variable

### Methods
#### AddInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	classToRefactor addInstVarName: newVariable.
	
</details>

#### AddInstanceVariable>>#classToRefactor

<details>
	<summary>See more</summary>
	
	classToRefactor

	^classToRefactor 
</details>

#### AddInstanceVariable>>#initializeNamed: aNewVariable to: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeNamed: aNewVariable to: aClassToRefactor 
	
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor 
</details>

#### AddInstanceVariable>>#newVariable

<details>
	<summary>See more</summary>
	
	newVariable
	
	^newVariable 
</details>

## AddInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### AddInstanceVariableApplier>>#newVariableNameLabel

<details>
	<summary>See more</summary>
	
	newVariableNameLabel
	
	^'Enter new variable name:'
</details>

#### AddInstanceVariableApplier>>#askNewVariableName

<details>
	<summary>See more</summary>
	
	askNewVariableName
		
	newInstanceVariable := self request: self newVariableNameLabel. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed 
</details>

#### AddInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^AddInstanceVariable named: newInstanceVariable to: classToRefactor.
	
</details>

#### AddInstanceVariableApplier>>#initializeOn: aBrowser for: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
</details>

#### AddInstanceVariableApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askNewVariableName
</details>

#### AddInstanceVariableApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser
</details>

#### AddInstanceVariableApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser
		
	browser acceptedContentsChanged
</details>

## AddParameter

I am a refactoring that adds a new parameter to a given selector (that has to be a unary or keyword). The input is the following: * the new parameter name * the selector that is going to be modified * the position of the new parameter in the selector * the keyword for the new parameter * the default value for senders of this message * the collection of implementors affected by the change * the collection of senders affected by the change

### Methods
#### AddParameter>>#firstNoSeparatorIndexIn: sourceCode startingFrom: aStartingPosition

Looks going back for the first no separator char. See #test24AddingParameterToSendersTakeCaresOfSeparators It assumes that there is always going to be a no separator wich holds due to how aStartPosition is obtained - Hernan


<details>
	<summary>See more</summary>
	
	firstNoSeparatorIndexIn: sourceCode startingFrom: aStartingPosition

	"Looks going back for the first no separator char. See #test24AddingParameterToSendersTakeCaresOfSeparators
	It assumes that there is always going to be a no separator wich holds due to how aStartPosition is obtained - Hernan"
	
	^self firstNot: [ :aChar | aChar isSeparator ] indexIn: sourceCode startingFrom: aStartingPosition 
</details>

#### AddParameter>>#firstNot: aBlock indexIn: sourceCode startingFrom: aStartingPosition

<details>
	<summary>See more</summary>
	
	firstNot: aBlock indexIn: sourceCode startingFrom: aStartingPosition
	
	| noSeparatorIndex |
	
	noSeparatorIndex := aStartingPosition.
	[ noSeparatorIndex > 0 and: [ aBlock value: (sourceCode at: noSeparatorIndex) ]] whileTrue: [ noSeparatorIndex := noSeparatorIndex - 1 ].
	
	^noSeparatorIndex 
		
</details>

#### AddParameter>>#implementorNewSourceCodeOf: anImplementor

<details>
	<summary>See more</summary>
	
	implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource insertionPoint |

	implementorMethodNode := anImplementor methodNode.
	insertionPoint := isAddingLast 
		ifTrue: [ implementorMethodNode selectorLastPosition ]
		ifFalse: [ (implementorMethodNode selectorKeywordPositionAt: index) first - 1].

	originalSource := anImplementor sourceCode.
	insertionPoint := self firstNoLineSeparatorIndexIn: originalSource startingFrom: insertionPoint.
	
	newSource := String streamContents: [ :newSourceStream |
		newSourceStream 
			nextPutAll: (originalSource copyFrom: 1 to: insertionPoint);
			nextPutAll: implementorTrailingString;
			nextPutAll: (originalSource copyFrom: insertionPoint+1 to: originalSource size) ].
		
	^newSource
</details>

#### AddParameter>>#addKeywordRangesUsing: insertionPoints to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addKeywordRangesUsing: insertionPoints to: rangesToKeywords

	insertionPoints do: [ :aPosition | 
		rangesToKeywords add: (aPosition to: aPosition-1) -> senderTrailingString ]

</details>

#### AddParameter>>#firstNoLineSeparatorIndexIn: sourceCode startingFrom: aStartingPosition

<details>
	<summary>See more</summary>
	
	firstNoLineSeparatorIndexIn: sourceCode startingFrom: aStartingPosition
	
	^self firstNot: [ :aChar | aChar isLineSeparator ] indexIn: sourceCode startingFrom: aStartingPosition 
</details>

#### AddParameter>>#addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| insertionPoints |
	
	isAddingLast 
		ifTrue: [ 
			insertionPoints := self messageSendLastPositionIn: aMethodNode. 
			self addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords ]
		ifFalse: [ 
			insertionPoints := self messageSendKeywordPositionsIn: aMethodNode.
			self addKeywordRangesUsing: insertionPoints to: rangesToKeywords ]
</details>

#### AddParameter>>#messageSendKeywordPositionsIn: aMethodNode

<details>
	<summary>See more</summary>
	
	messageSendKeywordPositionsIn: aMethodNode
	
	^aMethodNode messageSendKeywordPositionsAt: index of: oldSelector ifAbsent: [ #()].
</details>

#### AddParameter>>#initializedNamed: aNewParameter 
	at: anIndex
	addingLast: anIsAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector  
	addingToImplementors: anImplementorTrailingString 
	addingToSenders: aSenderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

<details>
	<summary>See more</summary>
	
	initializedNamed: aNewParameter 
	at: anIndex
	addingLast: anIsAddingLast
	initializedWith: aNewParameterValue 
	to: anOldSelector 
	implementing: aNewSelector  
	addingToImplementors: anImplementorTrailingString 
	addingToSenders: aSenderTrailingString
	implementors: implementorsCollection 
	senders: sendersCollection

	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.
	
	newParameter := aNewParameter.
	newParameterValue := aNewParameterValue.
	implementorTrailingString := anImplementorTrailingString.
	senderTrailingString := aSenderTrailingString.
	
	index := anIndex.
	isAddingLast := anIsAddingLast 
</details>

#### AddParameter>>#addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addKeywordRangesForLastPositionOf: aMethodNode using: insertionPoints to: rangesToKeywords 
	
	| originalSourceCode |
	
	originalSourceCode := aMethodNode sourceText.
	insertionPoints do: [ :aPosition | | newPosition |
		newPosition := self firstNoSeparatorIndexIn: originalSourceCode startingFrom: aPosition.
		rangesToKeywords add: ((newPosition+1) to: newPosition) -> senderTrailingString ]
</details>

#### AddParameter>>#messageSendLastPositionIn: aMethodNode

<details>
	<summary>See more</summary>
	
	messageSendLastPositionIn: aMethodNode
	
	^aMethodNode messageSendLastPositionsOf: oldSelector ifAbsent: [ #() ].
</details>

## AddParameterApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### AddParameterApplier>>#addAsLastParameterLabel

<details>
	<summary>See more</summary>
	
	addAsLastParameterLabel

	^ 'Add as last parameter'
</details>

#### AddParameterApplier>>#askInsertionIndexUsingKeywords

<details>
	<summary>See more</summary>
	
	askInsertionIndexUsingKeywords

	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword to add parameter before'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	
</details>

#### AddParameterApplier>>#askInsertionIndexIfNecessary

<details>
	<summary>See more</summary>
	
	askInsertionIndexIfNecessary

	oldSelector isKeyword ifTrue: [ self askInsertionIndex ].
	
</details>

#### AddParameterApplier>>#askNewParameterValue

<details>
	<summary>See more</summary>
	
	askNewParameterValue
		
	| enteredString |
	
	enteredString := self request: 'Enter parameter value for senders'.
	newParameterValue := enteredString withBlanksTrimmed.
	self refactoringClass assertNewParameterValueIsNotEmpty: newParameterValue.
	self refactoringClass assertNewParameterValueIsValid: newParameterValue.

</details>

#### AddParameterApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
	
	oldSelector isUnary ifTrue: [ ^self createRefactoringForUnarySelector].
	oldSelector isKeyword ifTrue: [ ^self createRefactoringForKeywordSelector ].
	
	self error: 'oldSelector should be unary or keyword!'
</details>

#### AddParameterApplier>>#askNewKeyword

<details>
	<summary>See more</summary>
	
	askNewKeyword

 	| enteredString |
	
	enteredString := (self request: 'Enter keyword for new parameter') withBlanksTrimmed.
	(enteredString endsWith: ':') ifFalse: [ enteredString := enteredString, ':' ].
	newKeyword := enteredString asSymbol.
	self refactoringClass assertIsValidKeywordForNewParameter: newKeyword
</details>

#### AddParameterApplier>>#askNewKeywordIfNecessary

<details>
	<summary>See more</summary>
	
	askNewKeywordIfNecessary
		
	oldSelector isKeyword ifTrue: [self askNewKeyword]
</details>

#### AddParameterApplier>>#askNewParameter

<details>
	<summary>See more</summary>
	
	askNewParameter
		
	| enteredString |
	
	enteredString := self request: 'Enter new parameter name'.
	newParameter := enteredString withBlanksTrimmed.
	self refactoringClass assertIsValidParameterName: newParameter

</details>

#### AddParameterApplier>>#createRefactoringForUnarySelector

<details>
	<summary>See more</summary>
	
	createRefactoringForUnarySelector
	
	^self refactoringClass 
		named: newParameter
		initializedWith: newParameterValue 
		toUnarySelector: oldSelector 
		implementors: implementors 
		senders: senders 
</details>

#### AddParameterApplier>>#createRefactoringForKeywordSelector

<details>
	<summary>See more</summary>
	
	createRefactoringForKeywordSelector
	
	^self refactoringClass 
		named: newParameter
		at: parameterIndex 
		initializedWith: newParameterValue 
		using: newKeyword 
		toKeywordSelector: oldSelector 
		implementors: implementors 
		senders: senders 
</details>

#### AddParameterApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self
		askNewParameter;
		askNewParameterValue;
		askInsertionIndexIfNecessary;
		askNewKeywordIfNecessary
		
</details>

#### AddParameterApplier>>#askInsertionIndex

See RemoveParameterApplier#askParameterToRemove to understand why I ask for the index using the keywords when no method is found - Hernan


<details>
	<summary>See more</summary>
	
	askInsertionIndex

	| methodNode originalMethod parameterNames |

	"See RemoveParameterApplier#askParameterToRemove to understand why I ask for the index using
	the keywords when no method is found - Hernan"
	originalMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askInsertionIndexUsingKeywords ].

	methodNode := originalMethod methodNode.
	parameterNames := methodNode argumentNames.
	parameterNames add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Add parameter before?'.
	parameterIndex = 0 ifTrue: [self endRequest ].

		
</details>

#### AddParameterApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^AddParameter
</details>

## ArgumentDeclarationCounter

I am responsible for counting the times an argument name appears in different block nodes across a method node.

### Methods
#### ArgumentDeclarationCounter>>#count

<details>
	<summary>See more</summary>
	
	count

	^counter
</details>

#### ArgumentDeclarationCounter>>#declaresSoughtArgument: aParseNode

<details>
	<summary>See more</summary>
	
	declaresSoughtArgument: aParseNode

	^aParseNode hasLocallyArgumentNamed: argumentName 
	
	
</details>

#### ArgumentDeclarationCounter>>#initializeFor: anArgumentName

<details>
	<summary>See more</summary>
	
	initializeFor: anArgumentName

	argumentName := anArgumentName.
	counter := 0
</details>

#### ArgumentDeclarationCounter>>#visitBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode

	self visitPotentiallyDeclaringParseNode: aBlockNode.
	super visitBlockNode: aBlockNode
</details>

#### ArgumentDeclarationCounter>>#visitMethodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNode: aMethodNode

	self visitPotentiallyDeclaringParseNode: aMethodNode.
	super visitMethodNode: aMethodNode
</details>

#### ArgumentDeclarationCounter>>#visitPotentiallyDeclaringParseNode: aParseNode

<details>
	<summary>See more</summary>
	
	visitPotentiallyDeclaringParseNode: aParseNode

	(self declaresSoughtArgument: aParseNode) ifTrue: [ counter := counter + 1 ]
</details>

## BlockNodeParentsFinder

I find the BlockNode parents of a BlockNode in the ParseNode tree starting from a ParseNode supplied to #parentsIn:.

### Methods
#### BlockNodeParentsFinder>>#parentsIn: aParseNode

<details>
	<summary>See more</summary>
	
	parentsIn: aParseNode

	aParseNode accept: self.
	^parents
</details>

#### BlockNodeParentsFinder>>#initializeFor: aSelectedBlockNode

<details>
	<summary>See more</summary>
	
	initializeFor: aSelectedBlockNode

	selectedBlockNode := aSelectedBlockNode.
	parents := OrderedCollection new.
	found := false.
</details>

#### BlockNodeParentsFinder>>#visitBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode

	found ifFalse: [
		aBlockNode = selectedBlockNode
			ifTrue: [ found := true ]
			ifFalse: [
				parents add: aBlockNode.
				super visitBlockNode: aBlockNode.
				found ifFalse: [ parents removeLast ]
			]
	]


	
</details>

## CanNotRefactorDueToReferencesError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### CanNotRefactorDueToReferencesError>>#references

<details>
	<summary>See more</summary>
	
	references

	^references copy
</details>

#### CanNotRefactorDueToReferencesError>>#referencee

<details>
	<summary>See more</summary>
	
	referencee

	^referencee 
</details>

#### CanNotRefactorDueToReferencesError>>#numberOfReferences

<details>
	<summary>See more</summary>
	
	numberOfReferences
	
	^references size
</details>

#### CanNotRefactorDueToReferencesError>>#initializeWith: aMessageText references: aCollectionOfReferences to: aReferencee

<details>
	<summary>See more</summary>
	
	initializeWith: aMessageText references: aCollectionOfReferences to: aReferencee

	self messageText: aMessageText.
	references := aCollectionOfReferences.
	referencee := aReferencee 
</details>

#### CanNotRefactorDueToReferencesError>>#anyReference

<details>
	<summary>See more</summary>
	
	anyReference
	
	^references anyOne 
</details>

## ChangeKeywordsSelectorOrder

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeKeywordsSelectorOrder>>#addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

<details>
	<summary>See more</summary>
	
	addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index).
	rangesToNewKeywords add:
		(currentImplementorMethodNode parameterDefinitionPositionAt: index) ->
		(currentImplementorMethodNode argumentNames at: (changedOrder at: index))
</details>

#### ChangeKeywordsSelectorOrder>>#initializeChangedOrder: aChangeOrder

<details>
	<summary>See more</summary>
	
	initializeChangedOrder: aChangeOrder

	changedOrder := aChangeOrder 
</details>

## ChangeKeywordsSelectorOrderApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeKeywordsSelectorOrderApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^ChangeKeywordsSelectorOrder 
</details>

## ChangeSelector

I am a refactoring abstract class whose purpose is to change a given selector; either by renaming it or changing arguments (adding, removing, change order)

### Methods
#### ChangeSelector>>#compileNewImplementorOf: anImplementor

<details>
	<summary>See more</summary>
	
	compileNewImplementorOf: anImplementor

	| implementorClassification newSourceCode |

	newSourceCode := self implementorNewSourceCodeOf: anImplementor.
	implementorClassification := anImplementor methodClass organization categoryOfElement: oldSelector.

	anImplementor methodClass
		compile: newSourceCode
		classified: implementorClassification.

</details>

#### ChangeSelector>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	self 
		createNewImplementors;
		renameSenders;
		removeOldImplementors.
		
	^changes

</details>

#### ChangeSelector>>#removeOldImplementor: anImplementor

<details>
	<summary>See more</summary>
	
	removeOldImplementor: anImplementor 

	anImplementor methodClass removeSelector: anImplementor selector.
	changes add: anImplementor methodReference
</details>

#### ChangeSelector>>#add: oldSelectorLiteralRanges to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	add: oldSelectorLiteralRanges to: rangesToKeywords

	oldSelectorLiteralRanges do: [ :oldSelectorLiteralRange | 
		rangesToKeywords add: (oldSelectorLiteralRange first + 1 to: oldSelectorLiteralRange last) -> newSelector ].
	

	
</details>

#### ChangeSelector>>#createNewImplementors

<details>
	<summary>See more</summary>
	
	createNewImplementors

	implementors
		select: [ :anImplementor | anImplementor isValid ]
	 	thenDo: [:anImplementor | self createNewImplementorOf: anImplementor ]
	
</details>

#### ChangeSelector>>#implementorNewSourceCodeOf: anImplementor

<details>
	<summary>See more</summary>
	
	implementorNewSourceCodeOf: anImplementor

	self subclassResponsibility 
</details>

#### ChangeSelector>>#renameSendersIn: aMethod

<details>
	<summary>See more</summary>
	
	renameSendersIn: aMethod

	| newSource rangesToNewStrings |
	
	rangesToNewStrings := self rangesToKeywordsOf: aMethod.
	newSource := aMethod sourceCode copyReplacing: rangesToNewStrings.
	aMethod methodClass compile: newSource.
	
	changes add: (MethodReference class: aMethod methodClass selector: aMethod selector)
</details>

#### ChangeSelector>>#removeOldImplementors

<details>
	<summary>See more</summary>
	
	removeOldImplementors
	
	implementors do: [:anImplementor | self removeOldImplementor: anImplementor ]
</details>

#### ChangeSelector>>#addToSendersIfOldSelectorIsSentIn: newImplementor

<details>
	<summary>See more</summary>
	
	addToSendersIfOldSelectorIsSentIn: newImplementor
	
	(newImplementor sendsOrRefersTo: oldSelector) ifTrue: [ senders add: newImplementor ]. 
	
</details>

#### ChangeSelector>>#renameSenders

<details>
	<summary>See more</summary>
	
	renameSenders
	
	senders do: [ :aSender | self renameSendersIn: aSender ].
	
</details>

#### ChangeSelector>>#addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	self subclassResponsibility 
</details>

#### ChangeSelector>>#addRangesForLiteralOf: methodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addRangesForLiteralOf: methodNode to: rangesToKeywords
	
	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsForLiteralNode: oldSelector ifAbsent: [ ^#() ].
	self add: oldSelectorLiteralRanges to: rangesToKeywords.
	
</details>

#### ChangeSelector>>#createNewImplementorOf: anImplementor

<details>
	<summary>See more</summary>
	
	createNewImplementorOf: anImplementor

	| newImplementor |

	self compileNewImplementorOf: anImplementor.
	newImplementor := anImplementor methodClass compiledMethodAt: newSelector.
	self addToSendersIfOldSelectorIsSentIn: newImplementor.
	
	changes add: newImplementor methodReference 
	
	
</details>

#### ChangeSelector>>#sendersSize

<details>
	<summary>See more</summary>
	
	sendersSize
	
	^senders size
</details>

#### ChangeSelector>>#rangesToKeywordsOf: aMethod

<details>
	<summary>See more</summary>
	
	rangesToKeywordsOf: aMethod
	
	| methodNode rangesToKeywords |
	
	methodNode := aMethod methodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right | left key first < right key first ].
	
	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.
	
	^rangesToKeywords	
</details>

#### ChangeSelector>>#initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

<details>
	<summary>See more</summary>
	
	initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	
	oldSelector := anOldSelector.
	newSelector := aNewSelector.
	implementors := aCollectionOfImplementors.
	"I have to make a copy of senders because it can change with recursive implementors - Hernan"
	senders := aCollectionOfSenders asOrderedCollection.
	
	changes := Set new
	
</details>

#### ChangeSelector>>#implementorsSize

<details>
	<summary>See more</summary>
	
	implementorsSize
	
	^implementors size
</details>

#### ChangeSelector>>#newSelector

<details>
	<summary>See more</summary>
	
	newSelector
	
	^newSelector 
</details>

#### ChangeSelector>>#addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords

	| oldSelectorLiteralRanges |
	
	oldSelectorLiteralRanges := methodNode positionsInLiteralArrayOf: oldSelector.
	self add: oldSelectorLiteralRanges to: rangesToKeywords 
</details>

## ChangeSelectorApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	self subclassResponsibility 
</details>

#### ChangeSelectorApplier>>#createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor

<details>
	<summary>See more</summary>
	
	createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor 
		
	implementors := Array with: anImplementor.
	senders := #().
	shouldShowChanges := false.
	
	self createAndApplyRefactoring 
</details>

#### ChangeSelectorApplier>>#openChangeSelectorSendersStepWindow

<details>
	<summary>See more</summary>
	
	openChangeSelectorSendersStepWindow

	ChangeSelectorSendersStepWindow openFrom: self 
</details>

#### ChangeSelectorApplier>>#initializeImplementorsAndSenders

<details>
	<summary>See more</summary>
	
	initializeImplementorsAndSenders

	implementors := self createImplementors.
	senders := self createSenders
</details>

#### ChangeSelectorApplier>>#value

<details>
	<summary>See more</summary>
	
	value

	requestExitBlock := [ ^self ].
		
	self requestRefactoringParametersHandlingRefactoringExceptions.
	
	self 
		ifHasNoSendersAndOneImplementor: [ :anImplementor | self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor ]
		ifNot: [ self askForImplementosAndSenders ]
</details>

#### ChangeSelectorApplier>>#wizardEnded

<details>
	<summary>See more</summary>
	
	wizardEnded

	requestExitBlock := [ ^self ].
	
	self 
		closeBrowser;
		createAndApplyRefactoring.
</details>

#### ChangeSelectorApplier>>#implementorsAndSendersInCategoryAndHierarchy

<details>
	<summary>See more</summary>
	
	implementorsAndSendersInCategoryAndHierarchy

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inCategoriesAndHierarchyOf: selectedClass
		organizedBy: SystemOrganization 
</details>

#### ChangeSelectorApplier>>#senders

<details>
	<summary>See more</summary>
	
	senders
	
	^senders 
</details>

#### ChangeSelectorApplier>>#ifHasNoSendersAndOneImplementor: trueBlock ifNot: falseBlock

<details>
	<summary>See more</summary>
	
	ifHasNoSendersAndOneImplementor: trueBlock ifNot: falseBlock

	| allImplementors |
	
	allImplementors := Smalltalk allImplementorsOf: oldSelector.
	
	"I could try to see if there is one sender and that that sender is in the same method beeing renamed. That could
	mean that it is a recursive call but I should also see if the receiver is self to be sure because if it is other 'type' of 
	object the rename could not be safe. To complex for a small posibility - Hernan"
	(allImplementors size = 1 and: [ (Smalltalk allCallsOn: oldSelector) isEmpty ]) 
		ifTrue: [ trueBlock value: allImplementors anyOne compiledMethod ]
		ifFalse: falseBlock
</details>

#### ChangeSelectorApplier>>#closeBrowser

<details>
	<summary>See more</summary>
	
	closeBrowser
	
	wizardStepWindow delete.
	
</details>

#### ChangeSelectorApplier>>#implementorsAndSendersForHierarchy

<details>
	<summary>See more</summary>
	
	implementorsAndSendersForHierarchy

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: selectedClass
</details>

#### ChangeSelectorApplier>>#askForImplementosAndSenders

<details>
	<summary>See more</summary>
	
	askForImplementosAndSenders

	self 
		askScope;
		initializeImplementorsAndSenders;
		calculateImplementorsAndSenders;
		startWizard 
</details>

#### ChangeSelectorApplier>>#showChangesInMessageSetWindow

<details>
	<summary>See more</summary>
	
	showChangesInMessageSetWindow

	self messageSetWindowClass openMessageList: changes asSortedCollection label: 'Changed methods' 
</details>

#### ChangeSelectorApplier>>#senders: sendersCollection

<details>
	<summary>See more</summary>
	
	senders: sendersCollection
 
	senders := sendersCollection 
</details>

#### ChangeSelectorApplier>>#messageSetWindowClass

<details>
	<summary>See more</summary>
	
	messageSetWindowClass
		
	^MessageSetWindow 
	
</details>

#### ChangeSelectorApplier>>#oldSelector

<details>
	<summary>See more</summary>
	
	oldSelector
	
	^oldSelector 
</details>

#### ChangeSelectorApplier>>#convertSendersToCompiledMethods

<details>
	<summary>See more</summary>
	
	convertSendersToCompiledMethods

	senders := senders collect: [ :aMethodReference | aMethodReference compiledMethod ]
</details>

#### ChangeSelectorApplier>>#implementorsAndSendersInCategory

<details>
	<summary>See more</summary>
	
	implementorsAndSendersInCategory

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		inCategory: selectedClass category
		organizedBy: SystemOrganization
</details>

#### ChangeSelectorApplier>>#initializeOn: aBrowser for: aSelector in: aSelectedClass

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aSelector in: aSelectedClass

	oldSelector := aSelector.
	selectedClass := aSelectedClass.
	browser := aBrowser.
	shouldShowChanges := true.
</details>

#### ChangeSelectorApplier>>#createSenders

<details>
	<summary>See more</summary>
	
	createSenders

	^Set new

</details>

#### ChangeSelectorApplier>>#wizardStepWindow: aWizarStepWindow

<details>
	<summary>See more</summary>
	
	wizardStepWindow: aWizarStepWindow 
	
	wizardStepWindow := aWizarStepWindow 
</details>

#### ChangeSelectorApplier>>#doNotShowChanges

<details>
	<summary>See more</summary>
	
	doNotShowChanges

	shouldShowChanges := false
</details>

#### ChangeSelectorApplier>>#implementorsAndSendersInSystem

<details>
	<summary>See more</summary>
	
	implementorsAndSendersInSystem
		
	^self refactoringClass addImplementorsOf: oldSelector to: implementors andSendersTo: senders inSystem: Smalltalk 
</details>

#### ChangeSelectorApplier>>#implementorsAndSendersForClass

<details>
	<summary>See more</summary>
	
	implementorsAndSendersForClass

	^self refactoringClass
		addImplementorsOf: oldSelector
		to: implementors
		andSendersTo: senders
		forClassAndMetaOf: selectedClass
</details>

#### ChangeSelectorApplier>>#calculateImplementorsAndSenders

<details>
	<summary>See more</summary>
	
	calculateImplementorsAndSenders
		
	scopeChoice = 1 ifTrue: [ ^self implementorsAndSendersForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndSendersForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndSendersInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndSendersInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndSendersInSystem ].
	
	self error: 'Unknown scope option' 
		
		
</details>

#### ChangeSelectorApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges
		
	self showChangesInMessageSetWindow
</details>

#### ChangeSelectorApplier>>#scopeOptionLabels

<details>
	<summary>See more</summary>
	
	scopeOptionLabels
	
	^{'In Class'. 'In Hierarchy'. 'In Category'. 'In Hierarchy and its Categories'. 'In System'}.
</details>

#### ChangeSelectorApplier>>#startWizard

<details>
	<summary>See more</summary>
	
	startWizard
		
	ChangeSelectorImplementorsStepWindow openFrom: self
</details>

#### ChangeSelectorApplier>>#implementors

<details>
	<summary>See more</summary>
	
	implementors
	
	^implementors 
</details>

#### ChangeSelectorApplier>>#informChangesToBrowser

If the selected message is not the same as the oldSelector, that is the selector being renamed, then it implies that we are renaming a selector sent in the source code of the selected message then I don't have to change the selected message in the browser - Hernan


<details>
	<summary>See more</summary>
	
	informChangesToBrowser

	"If the selected message is not the same as the oldSelector, that is the selector being renamed, 
	then it implies that we are renaming a selector sent in the source code of the selected message then
	I don't have to change the selected message in the browser - Hernan"
	browser selectedMessageName = oldSelector ifTrue: [ 
		browser setSelector: refactoring newSelector ]
</details>

#### ChangeSelectorApplier>>#implementors: implementorsCollection

<details>
	<summary>See more</summary>
	
	implementors: implementorsCollection

	implementors := implementorsCollection 
</details>

#### ChangeSelectorApplier>>#createImplementors

<details>
	<summary>See more</summary>
	
	createImplementors

	^IdentitySet new.
</details>

#### ChangeSelectorApplier>>#askScope

<details>
	<summary>See more</summary>
	
	askScope

	| scopeMenu |
	
	scopeMenu := PopUpMenu labelArray: self scopeOptionLabels.
	scopeChoice := scopeMenu startUpWithCaption: 'Select Refactoring Scope'.
	scopeChoice = 0 ifTrue: [ self endRequest ].
	
</details>

#### ChangeSelectorApplier>>#createAndApplyRefactoring

<details>
	<summary>See more</summary>
	
	createAndApplyRefactoring

	self convertSendersToCompiledMethods.
	
	self 
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		informChangesToBrowser.
			
	shouldShowChanges ifTrue: [ self showChanges ]
	
			
</details>

## ChangeSelectorImplementorsStepWindow

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorImplementorsStepWindow>>#addButtonsTo: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButtonsTo: row color: buttonColor

	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeSendersButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

</details>

#### ChangeSelectorImplementorsStepWindow>>#createSeeSendersButton

<details>
	<summary>See more</summary>
	
	createSeeSendersButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeSenders
		label: 'See Senders'.


</details>

#### ChangeSelectorImplementorsStepWindow>>#add

<details>
	<summary>See more</summary>
	
	add
		
	self 
		do: [ :classOfImplementorToAdd | self addImplementorIn: classOfImplementorToAdd ]
		withEnteredClassLabeled:  'Class that implements ', self oldSelector 
</details>

#### ChangeSelectorImplementorsStepWindow>>#addImplementorIn: classOfImplementorToAdd

<details>
	<summary>See more</summary>
	
	addImplementorIn: classOfImplementorToAdd 

	| implementorToAdd |
	
	implementorToAdd := classOfImplementorToAdd 
		compiledMethodAt: self oldSelector 
		ifAbsent: [ ^self inform: classOfImplementorToAdd doesNotImplement: self oldSelector ].
		
	self addToList: implementorToAdd
</details>

#### ChangeSelectorImplementorsStepWindow>>#seeSenders

<details>
	<summary>See more</summary>
	
	seeSenders

	self changeImplementors.
	self delete.	
	
	"Necesary indirection to support actual senders in LiveTyping - Hernan"
	applier openChangeSelectorSendersStepWindow
</details>

#### ChangeSelectorImplementorsStepWindow>>#refactor

<details>
	<summary>See more</summary>
	
	refactor

	self changeImplementors.
	super refactor 
</details>

#### ChangeSelectorImplementorsStepWindow>>#changeImplementors

<details>
	<summary>See more</summary>
	
	changeImplementors
	
	applier implementors: (self compiledMethodsFrom: model messageList).
	
</details>

## ChangeSelectorKeepingParameters

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorKeepingParameters>>#initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

<details>
	<summary>See more</summary>
	
	initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.

	newSelectorKeywords := newSelector keywords.
	
</details>

#### ChangeSelectorKeepingParameters>>#implementorNewSourceCodeOf: anImplementor

<details>
	<summary>See more</summary>
	
	implementorNewSourceCodeOf: anImplementor

	|  newSource rangesToNewKeywords |

	rangesToNewKeywords := OrderedCollection new.
	currentImplementorMethodNode := anImplementor methodNode.

	currentImplementorMethodNode selectorKeywordsPositions withIndexDo: [ :aKeywordRange :index |
		self addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords ].

	newSource := anImplementor sourceCode copyReplacing: rangesToNewKeywords.
	^newSource
</details>

#### ChangeSelectorKeepingParameters>>#addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	| oldSelectorKeywordsRanges |

	oldSelectorKeywordsRanges := self messageSendSelectorKeywordPositionsIn: aMethodNode.
	self addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords.
	
	
</details>

#### ChangeSelectorKeepingParameters>>#addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

<details>
	<summary>See more</summary>
	
	addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	self subclassResponsibility 
</details>

#### ChangeSelectorKeepingParameters>>#addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addRangesOf: oldSelectorKeywordsRanges to: rangesToKeywords

	oldSelectorKeywordsRanges do: [ :aMessageSendSelectorRanges |
		aMessageSendSelectorRanges withIndexDo: [ :aRange :index | rangesToKeywords add: aRange -> (newSelectorKeywords at: index) ]].
	
	
	
</details>

#### ChangeSelectorKeepingParameters>>#messageSendSelectorKeywordPositionsIn: aMethodNode

<details>
	<summary>See more</summary>
	
	messageSendSelectorKeywordPositionsIn: aMethodNode
	
	^aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].
</details>

## ChangeSelectorKeepingParametersApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorKeepingParametersApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	
</details>

#### ChangeSelectorKeepingParametersApplier>>#assertCanRenameSelector

<details>
	<summary>See more</summary>
	
	assertCanRenameSelector

	self refactoringClass assertIsValidToRenameFrom: oldSelector to: newSelector.
	
</details>

#### ChangeSelectorKeepingParametersApplier>>#askNewSelector

<details>
	<summary>See more</summary>
	
	askNewSelector

	| enteredString |

	enteredString := self request: 'Enter new selector:' initialAnswer: oldSelector.
	newSelector := enteredString withBlanksTrimmed asSymbol.


</details>

#### ChangeSelectorKeepingParametersApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self
		askNewSelector;
		assertCanRenameSelector
		
</details>

## ChangeSelectorSendersStepWindow

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorSendersStepWindow>>#addButtonsTo: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButtonsTo: row color: buttonColor
	
	self addButton: self createRemoveButton to: row color: buttonColor.
	self addButton: self createAddButton to: row color: buttonColor.
	self addButton: self createSeeImplementorsButton to: row color: buttonColor.
	self addButton: self createRefactorButton to: row color: buttonColor.
	self addButton: self createJustRefactorButton to: row color: buttonColor.
	self addButton: self createCancelButton to: row color: buttonColor.

</details>

#### ChangeSelectorSendersStepWindow>>#changeSenders

<details>
	<summary>See more</summary>
	
	changeSenders
	
	applier senders: model messageList
	
</details>

#### ChangeSelectorSendersStepWindow>>#add

<details>
	<summary>See more</summary>
	
	add
	
	self 
		do: [ :classOfSenderToAdd | self askAndAddSenderOf: classOfSenderToAdd ]
		withEnteredClassLabeled: 'Class that sends #', self oldSelector
		
</details>

#### ChangeSelectorSendersStepWindow>>#createSeeImplementorsButton

<details>
	<summary>See more</summary>
	
	createSeeImplementorsButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'.

</details>

#### ChangeSelectorSendersStepWindow>>#seeImplementors

<details>
	<summary>See more</summary>
	
	seeImplementors
	
	self changeSenders.
	self delete.
	
	ChangeSelectorImplementorsStepWindow openFrom: applier
</details>

#### ChangeSelectorSendersStepWindow>>#refactor

<details>
	<summary>See more</summary>
	
	refactor

	self changeSenders.
	super refactor 
</details>

#### ChangeSelectorSendersStepWindow>>#askAndAddSenderOf: classOfSenderToAdd

<details>
	<summary>See more</summary>
	
	askAndAddSenderOf: classOfSenderToAdd 
		
	| senderSelector senderToAdd |
	
	senderSelector := FillInTheBlankMorph request: 'Selector of sender of #', self oldSelector onCancel: [^self ].
	senderToAdd := classOfSenderToAdd 
		compiledMethodAt: senderSelector asSymbol
		ifAbsent: [ ^self inform: classOfSenderToAdd doesNotImplement: senderSelector asSymbol].
		
	(senderToAdd sendsOrRefersTo: self oldSelector) ifFalse: [ ^self inform: senderToAdd classAndSelector, ' does not refer to #', self oldSelector ].
	
	self addToList: senderToAdd 
</details>

## ChangeSelectorWizardStepWindow

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ChangeSelectorWizardStepWindow>>#createRefactorButton

<details>
	<summary>See more</summary>
	
	createRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'
</details>

#### ChangeSelectorWizardStepWindow>>#addButtonsTo: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButtonsTo: row color: buttonColor
	
	self subclassResponsibility 
</details>

#### ChangeSelectorWizardStepWindow>>#createJustRefactorButton

<details>
	<summary>See more</summary>
	
	createJustRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #justRefactor
		label: 'Just Refactor!'
</details>

#### ChangeSelectorWizardStepWindow>>#createRemoveButton

<details>
	<summary>See more</summary>
	
	createRemoveButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: #isMessageSelected
		action: #remove
		label: 'Remove'.

</details>

#### ChangeSelectorWizardStepWindow>>#oldSelector

<details>
	<summary>See more</summary>
	
	oldSelector
	
	^applier oldSelector 
</details>

#### ChangeSelectorWizardStepWindow>>#add

<details>
	<summary>See more</summary>
	
	add

	self subclassResponsibility 
</details>

#### ChangeSelectorWizardStepWindow>>#inform: aClass doesNotImplement: aSelector

<details>
	<summary>See more</summary>
	
	inform: aClass doesNotImplement: aSelector

 	self inform: aClass name, ' does not implement #', aSelector 
</details>

#### ChangeSelectorWizardStepWindow>>#compiledMethodsFrom: methodReferences

If the method is not implemented, I leave the not implemented reference because actual senders of it should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan


<details>
	<summary>See more</summary>
	
	compiledMethodsFrom: methodReferences

	"If the method is not implemented, I leave the not implemented reference because actual senders of it
	should be renamed. This is important for LiveTyping Actual Scope Refactorings - Hernan"
	^ methodReferences collect: [:aMethodReference |
		aMethodReference compiledMethodIfAbsent: [ aMethodReference ]]
</details>

#### ChangeSelectorWizardStepWindow>>#withClassNamed: aName do: aBlock

<details>
	<summary>See more</summary>
	
	withClassNamed: aName do: aBlock

	| trimmedNamed |
	
	trimmedNamed := aName withBlanksTrimmed.
	
	(Smalltalk classNamed: trimmedNamed asSymbol)
		ifNotNil: aBlock
		ifNil: [ self inform: 'Class ', trimmedNamed , ' does not exist' ].

</details>

#### ChangeSelectorWizardStepWindow>>#justRefactor

<details>
	<summary>See more</summary>
	
	justRefactor
	
	applier doNotShowChanges.
	self refactor.
</details>

#### ChangeSelectorWizardStepWindow>>#do: aBlock withEnteredClassLabeled: aLabel

<details>
	<summary>See more</summary>
	
	do: aBlock withEnteredClassLabeled: aLabel
		
	| className |
	
	className := ClassNameRequestMorph request: aLabel onCancel: [ ^self ].
	^self withClassNamed: className do: aBlock
</details>

#### ChangeSelectorWizardStepWindow>>#isMessageSelected

<details>
	<summary>See more</summary>
	
	isMessageSelected

	^model isNil ifTrue: [ false ] ifFalse: [ model selection notNil ]
</details>

#### ChangeSelectorWizardStepWindow>>#addToList: aMethod

<details>
	<summary>See more</summary>
	
	addToList: aMethod

	model addMethodReference: aMethod methodReference ifIncluded: [ self inform: 'Method already in list' ]
</details>

#### ChangeSelectorWizardStepWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes

	| codeAndButtons  |
	
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons 
</details>

#### ChangeSelectorWizardStepWindow>>#refactor

<details>
	<summary>See more</summary>
	
	refactor
	
	applier wizardStepWindow: self. 	
	applier wizardEnded.
	
</details>

#### ChangeSelectorWizardStepWindow>>#addButton: button to: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
</details>

#### ChangeSelectorWizardStepWindow>>#buttonsRow

<details>
	<summary>See more</summary>
	
	buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	
</details>

#### ChangeSelectorWizardStepWindow>>#createAddButton

<details>
	<summary>See more</summary>
	
	createAddButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #add
		label: 'Add'.

</details>

#### ChangeSelectorWizardStepWindow>>#createCancelButton

<details>
	<summary>See more</summary>
	
	createCancelButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #delete
		label: 'Cancel'.

</details>

#### ChangeSelectorWizardStepWindow>>#initializeFrom: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	initializeFrom: aChangeSelectorApplier

	applier := aChangeSelectorApplier 
</details>

#### ChangeSelectorWizardStepWindow>>#remove

<details>
	<summary>See more</summary>
	
	remove
	
	model removeMessageFromBrowserKeepingLabel
</details>

## ClassReferencesCollector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ClassReferencesCollector>>#referencedAsClass

<details>
	<summary>See more</summary>
	
	referencedAsClass
	
	^referencedAsClass
</details>

#### ClassReferencesCollector>>#initializeCollectionFor: withAllSubclasses

<details>
	<summary>See more</summary>
	
	initializeCollectionFor: withAllSubclasses
	
	withAllSubclassesNames := withAllSubclasses collect: [:aClass | aClass name ].
	referencesToClass := OrderedCollection new.
	referencedAsClass := OrderedCollection new.
	referencesToName := OrderedCollection new.
	referencedAsName := OrderedCollection new
</details>

#### ClassReferencesCollector>>#initializeOf: aClassToLookForReferences

<details>
	<summary>See more</summary>
	
	initializeOf: aClassToLookForReferences

	classToLookForReferences := aClassToLookForReferences 
</details>

#### ClassReferencesCollector>>#value

<details>
	<summary>See more</summary>
	
	value

	| withAllSubclasses |
	
	withAllSubclasses := classToLookForReferences withAllSubclasses.
	self initializeCollectionFor: withAllSubclasses.
		
	withAllSubclasses do: [ :aClass | self collectReferencesOf: aClass ].
		
	
</details>

#### ClassReferencesCollector>>#hasReferencesToClass

<details>
	<summary>See more</summary>
	
	hasReferencesToClass
	
	^referencesToClass notEmpty
</details>

#### ClassReferencesCollector>>#hasReferencesToName

<details>
	<summary>See more</summary>
	
	hasReferencesToName
	
	^referencesToName notEmpty 
</details>

#### ClassReferencesCollector>>#add: aClass asReferencedToClassWith: referencesToVariableBinding

<details>
	<summary>See more</summary>
	
	add: aClass asReferencedToClassWith: referencesToVariableBinding
	
	referencedAsClass add: aClass.
	referencesToClass addAll: referencesToVariableBinding 
</details>

#### ClassReferencesCollector>>#collectReferencesOf: aClass

<details>
	<summary>See more</summary>
	
	collectReferencesOf: aClass 
	
	| allReferences referencesOutsideHierarchy referencesToVariableBinding referencesToClassName |
	
	allReferences := aClass allCallsOn.
	referencesOutsideHierarchy := allReferences reject: [ :aReference | withAllSubclassesNames includes: aReference classSymbol ].
	referencesToVariableBinding := referencesOutsideHierarchy select: [ :aReference | aReference hasVariableBindingTo: aClass ].
	referencesToClassName := referencesOutsideHierarchy difference: referencesToVariableBinding.
	
	referencesToVariableBinding notEmpty ifTrue: [ self add: aClass asReferencedToClassWith: referencesToVariableBinding ].	
	referencesToClassName notEmpty ifTrue: [ self add: aClass asReferencedToNameWith: referencesToClassName ].
	
</details>

#### ClassReferencesCollector>>#referencedAsName

<details>
	<summary>See more</summary>
	
	referencedAsName
	
	^referencedAsName
</details>

#### ClassReferencesCollector>>#referencesToClass

<details>
	<summary>See more</summary>
	
	referencesToClass
	
	^referencesToClass
</details>

#### ClassReferencesCollector>>#referencesToName

<details>
	<summary>See more</summary>
	
	referencesToName
	
	^referencesToName
</details>

#### ClassReferencesCollector>>#add: aClass asReferencedToNameWith: referencesToClassName

<details>
	<summary>See more</summary>
	
	add: aClass asReferencedToNameWith: referencesToClassName

	referencedAsName add: aClass.
	referencesToName addAll: referencesToClassName 
	
</details>

## ExtractMethod

I am a refactoring that extracts a selected piece of code to a separate method. The input is the following: * interval of code to extract (from index - to index) * the CompiledMethod where this change applies * the new method selector + argument names (instance of Message) * the category name for the new method Many conditions have to be satisfied for this refactoring to be made, I delegate into ExtractMethodExpressionValidation and ExtractMethodNewSelectorPrecondition some of these checks. Refer to the class comment of those classes for more information.

### Methods
#### ExtractMethod>>#startingMethodIdentation

<details>
	<summary>See more</summary>
	
	startingMethodIdentation

	^ String lfString , String lfString , String tab
</details>

#### ExtractMethod>>#newMessageString

<details>
	<summary>See more</summary>
	
	newMessageString

	^ newMessage fullName
</details>

#### ExtractMethod>>#updatedSourceCodeOfExistingMethod

<details>
	<summary>See more</summary>
	
	updatedSourceCodeOfExistingMethod

	^ existingMethod sourceCode
		copyReplaceFrom: intervalToExtract first
		to: intervalToExtract last
		with: self callingExpression
</details>

#### ExtractMethod>>#apply

<details>
	<summary>See more</summary>
	
	apply

	self
		defineExtractedMethod;
		changeExistingMethod
</details>

#### ExtractMethod>>#callingExpression

<details>
	<summary>See more</summary>
	
	callingExpression

	| expression |
	expression _ 'self ', self newMessageString.
	
	^ self shouldBeEnclosedWithParens
		ifTrue: [ '(' , expression , ')' ]
		ifFalse: [ expression ]
</details>

#### ExtractMethod>>#sourceClass

<details>
	<summary>See more</summary>
	
	sourceClass

	^ existingMethod methodClass
</details>

#### ExtractMethod>>#changeExistingMethod

<details>
	<summary>See more</summary>
	
	changeExistingMethod

	self sourceClass
		compile: self updatedSourceCodeOfExistingMethod
		classified: existingMethod category
</details>

#### ExtractMethod>>#initializeExtractedSourceCode

<details>
	<summary>See more</summary>
	
	initializeExtractedSourceCode

	extractedSourceCode _ existingMethod sourceCode
		copyFrom: intervalToExtract first
		to: intervalToExtract last
</details>

#### ExtractMethod>>#returnCharacterIfNeeded

<details>
	<summary>See more</summary>
	
	returnCharacterIfNeeded

	| extractedMethodNode |
	extractedMethodNode _ Parser parse: extractedSourceCode class: self sourceClass noPattern: true.
	^ (extractedMethodNode numberOfStatements > 1 or: [ extractedMethodNode hasTemporaryVariables ])
		ifTrue: [ '' ] ifFalse: [ '^ ' ]
</details>

#### ExtractMethod>>#initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage in: aCategory

<details>
	<summary>See more</summary>
	
	initializeFrom: anIntervalToExtract of: aMethodToExtractCodeFrom to: aNewMessage in: aCategory 

	intervalToExtract _ anIntervalToExtract.
	existingMethod _ aMethodToExtractCodeFrom.
	newMessage _ aNewMessage.
	categoryOfNewSelector _ aCategory.
	self initializeExtractedSourceCode.
</details>

#### ExtractMethod>>#newMethodSourceCode

<details>
	<summary>See more</summary>
	
	newMethodSourceCode

	^ self newMessageString
	, self startingMethodIdentation
	, self returnCharacterIfNeeded
	, extractedSourceCode
</details>

#### ExtractMethod>>#shouldBeEnclosedWithParens

<details>
	<summary>See more</summary>
	
	shouldBeEnclosedWithParens

	| initialNode finalNode parseNodesInCommon methodNode initialNodeAncestors finalNodeAncestors insideMessageNodeExpressions |

	methodNode _ existingMethod methodNode.
	initialNodeAncestors  _ methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors _ methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	parseNodesInCommon _ initialNodeAncestors intersection: finalNodeAncestors.

	initialNode _ (parseNodesInCommon at: 1 ifAbsent: [ ^ false ]) key.
	finalNode _ (parseNodesInCommon at: 2 ifAbsent: [ ^ false ]) key.
	insideMessageNodeExpressions _ initialNode isMessageNode and: [ finalNode isMessageNode ].
	
	^ insideMessageNodeExpressions
		and: [ initialNode precedence < newMessage selector precedence ]
		and: [ initialNode precedence <= finalNode precedence ]
</details>

#### ExtractMethod>>#defineExtractedMethod

<details>
	<summary>See more</summary>
	
	defineExtractedMethod

 	self sourceClass
		compile: self newMethodSourceCode
		classified: categoryOfNewSelector
</details>

## ExtractMethodApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ExtractMethodApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^ ExtractMethod
</details>

#### ExtractMethodApplier>>#validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords

<details>
	<summary>See more</summary>
	
	validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords

	newSelectorKeywords size = parseNodesToParameterize size
		ifFalse: [ self refactoringClass signalExtractMethodWithWrongNumberOfArgumentsError ]
</details>

#### ExtractMethodApplier>>#initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom

<details>
	<summary>See more</summary>
	
	initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom

	intervalToExtract _ anIntervalToExtract.
	methodToExtractCodeFrom _ aMethodToExtractCodeFrom.
	newMessageArguments _ Dictionary new
</details>

#### ExtractMethodApplier>>#saveUnarySelector: userAnswer

<details>
	<summary>See more</summary>
	
	saveUnarySelector: userAnswer

	^ newSelector _ userAnswer asSymbol
</details>

#### ExtractMethodApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^ self refactoringClass
		fromInterval: intervalToExtract
		of: methodToExtractCodeFrom
		to: self buildNewMessage
		categorizedAs: methodToExtractCodeFrom category
</details>

#### ExtractMethodApplier>>#formatAsMethodArgument: aMethodArgumentName

<details>
	<summary>See more</summary>
	
	formatAsMethodArgument: aMethodArgumentName

	^ Text
		string: aMethodArgumentName
		attributes: (SHTextStylerST80 attributesFor: #methodArg)
</details>

#### ExtractMethodApplier>>#selectorTokensOf: userAnswer

this selects the pieces of strings before each $:


<details>
	<summary>See more</summary>
	
	selectorTokensOf: userAnswer
	"this selects the pieces of strings before each $:"

	^ (userAnswer findTokens: ':') allButLast
		collect: [ :tok | (tok findTokens: Character separators) last , ':' ]
</details>

#### ExtractMethodApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	| parseNodesToParameterize initialAnswer userAnswer |
	parseNodesToParameterize _ self parseNodesToParameterize.
	initialAnswer _ self buildInitialSelectorAnswer: parseNodesToParameterize.
	userAnswer _ self request: 'New method name:' initialAnswer: initialAnswer.

	parseNodesToParameterize
		ifEmpty: [ self saveUnarySelector: userAnswer ]
		ifNotEmpty: [ self saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize ]
</details>

#### ExtractMethodApplier>>#formatAsKeyword: aKeyword

<details>
	<summary>See more</summary>
	
	formatAsKeyword: aKeyword

	^ Text
		string: aKeyword
		attributes: (SHTextStylerST80 attributesFor: #patternKeyword)
</details>

#### ExtractMethodApplier>>#saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer

<details>
	<summary>See more</summary>
	
	saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer

	| newSelectorKeywords |
	newSelectorKeywords _ self selectorTokensOf: userAnswer.
	self validateRequiredParameters: parseNodesToParameterize haveACorrespondingKeywordIn: newSelectorKeywords.
	parseNodesToParameterize withIndexDo: [ :parseNode :index |
		newMessageArguments at: (newSelectorKeywords at: index) put: parseNode ]
</details>

#### ExtractMethodApplier>>#newMessageArgumentNames

<details>
	<summary>See more</summary>
	
	newMessageArgumentNames

	^ newMessageArguments values collect: [ :parseNode | parseNode name ]
</details>

#### ExtractMethodApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges
</details>

#### ExtractMethodApplier>>#buildInitialSelectorAnswer: parseNodesToParameterize

builds a selector with the shape of #m1 if unary, or #m1: something m2: else if it has args


<details>
	<summary>See more</summary>
	
	buildInitialSelectorAnswer: parseNodesToParameterize
	"builds a selector with the shape of #m1 if unary, or #m1: something m2: else if it has args"

	^ parseNodesToParameterize
		ifEmpty: [ self formatAsKeyword: 'm1' ]
		ifNotEmpty: [ parseNodesToParameterize
			inject: ''
			into: [ :partialSelector :parseNode |
				| currentKeyword |
				currentKeyword _ 'm' , (parseNodesToParameterize indexOf: parseNode) asString , ': '.
				partialSelector
				, (self formatAsKeyword: currentKeyword)
				, (self formatAsMethodArgument: parseNode name)
				, String newLineString ] ]
</details>

#### ExtractMethodApplier>>#parseNodesToParameterize

<details>
	<summary>See more</summary>
	
	parseNodesToParameterize

	^ ExtractMethodParametersDetector
		valueFor: methodToExtractCodeFrom methodNode
		at: intervalToExtract
</details>

#### ExtractMethodApplier>>#saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize

<details>
	<summary>See more</summary>
	
	saveBinaryOrKeywordSelector: userAnswer withArguments: parseNodesToParameterize

	self saveMessageArgumentsForEach: parseNodesToParameterize using: userAnswer.
	newSelector _ ('' join: (self selectorTokensOf: userAnswer)) asSymbol.
</details>

#### ExtractMethodApplier>>#buildNewMessage

<details>
	<summary>See more</summary>
	
	buildNewMessage

	^ Message
		selector: newSelector
		arguments: self newMessageArgumentNames
</details>

## ExtractMethodExpressionValidation

I check if an expression selected for extract method can be actually extracted. Many conditions have to happen: * we are selecting from the beginning of an AST node to an end of an AST node * it is a smalltalk expression, a single statement or a sequence of complete statements * the expression does not contain returns, or temporary variable assignments

### Methods
#### ExtractMethodExpressionValidation>>#intervalCoversCompleteAstNodes

<details>
	<summary>See more</summary>
	
	intervalCoversCompleteAstNodes 

	^ (self trimmed: (initialNode value first to: finalNode value last)) = intervalToExtract
</details>

#### ExtractMethodExpressionValidation>>#isNotLeftSideOfAssignment

<details>
	<summary>See more</summary>
	
	isNotLeftSideOfAssignment 

	^ (self startAndEndParseNodesAreTheSame and: [ self isLeftSideOfAssignment ]) not
</details>

#### ExtractMethodExpressionValidation>>#initializeFor: anIntervalToExtract of: aMethodUnderValidation

<details>
	<summary>See more</summary>
	
	initializeFor: anIntervalToExtract of: aMethodUnderValidation

	intervalToExtract _ anIntervalToExtract.
	method _ aMethodUnderValidation
</details>

#### ExtractMethodExpressionValidation>>#thereAreNoLocalVariableAssignmentsWithoutDeclaration

<details>
	<summary>See more</summary>
	
	thereAreNoLocalVariableAssignmentsWithoutDeclaration

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode |
			parseNode isAssignmentToTemporary
				and: [ self isNotDeclaredWithinIntervalToExtract: parseNode variable ] ]) not
</details>

#### ExtractMethodExpressionValidation>>#passed

<details>
	<summary>See more</summary>
	
	passed

	methodNode _ method methodNode.
	sourceCode _ method sourceCode.
	initialNodeAncestors  _ methodNode parseNodesPathAt: intervalToExtract first ifAbsent: [ ^ false].
	finalNodeAncestors _ methodNode parseNodesPathAt: intervalToExtract last ifAbsent: [ ^ false ].
	initialNode _ initialNodeAncestors first.
	finalNode _ finalNodeAncestors first.

	^ self intervalCoversCompleteAstNodes
		and: [ self containsValidNodes ]
		and: [ self startAndEndParseNodesAreTheSame
			or: [ self startAndEndNodesShareAParentNode ]
			or: [ self intervalMatchesBeginningOfStatement and: [ self intervalMatchesEndOfStatement ] ] ]
</details>

#### ExtractMethodExpressionValidation>>#intervalMatchesBeginningOfStatement

<details>
	<summary>See more</summary>
	
	intervalMatchesBeginningOfStatement 

	^ (self findSourceRangeOfCloserStatementIn: initialNodeAncestors) first = intervalToExtract first
</details>

#### ExtractMethodExpressionValidation>>#isLeftSideOfAssignment

<details>
	<summary>See more</summary>
	
	isLeftSideOfAssignment

	^ initialNodeAncestors size > 1
		and: [ initialNodeAncestors second key isAssignmentNode ]
		and: [ initialNodeAncestors second key variable = initialNode key ]
</details>

#### ExtractMethodExpressionValidation>>#thereAreNoReturnExpressions

<details>
	<summary>See more</summary>
	
	thereAreNoReturnExpressions 

	^ methodNode
		allParseNodesWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isReturn ]
</details>

#### ExtractMethodExpressionValidation>>#startAndEndParseNodesAreTheSame

<details>
	<summary>See more</summary>
	
	startAndEndParseNodesAreTheSame

	^ initialNode key = finalNode key
</details>

#### ExtractMethodExpressionValidation>>#trimmed: anInterval

<details>
	<summary>See more</summary>
	
	trimmed: anInterval

	^ Refactoring trim: anInterval toMatchExpressionOn: sourceCode
</details>

#### ExtractMethodExpressionValidation>>#isNotATempDeclarationWithUsagesOutOfIntervalToExtract

<details>
	<summary>See more</summary>
	
	isNotATempDeclarationWithUsagesOutOfIntervalToExtract

	initialNode key isTemporariesDeclaration ifFalse: [ ^ true ].
	
	^ (methodNode
		anyParseNodeWithin: (intervalToExtract last to: sourceCode size)
		satisfy: [ :parseNode |
			parseNode isVariableNode
			and: [ initialNode key declaresVariable: parseNode ] ]) not
</details>

#### ExtractMethodExpressionValidation>>#findSourceRangeOfCloserStatementIn: listOfAncestors

<details>
	<summary>See more</summary>
	
	findSourceRangeOfCloserStatementIn: listOfAncestors

	^ (listOfAncestors
		detect: [ :assoc | assoc key isBlockNode ]
		ifFound: [ :assoc | listOfAncestors before: assoc ifNone: [ listOfAncestors last ] ]
		ifNone: [ listOfAncestors last ]) value
</details>

#### ExtractMethodExpressionValidation>>#containsValidNodes

<details>
	<summary>See more</summary>
	
	containsValidNodes

	^ self isNotLeftSideOfAssignment
		and: [ self thereAreNoLocalVariableAssignmentsWithoutDeclaration ]
		and: [ self thereAreNoReturnExpressions ]
		and: [ self isNotATempDeclarationWithUsagesOutOfIntervalToExtract ]
		and: [ self isNotInsideATempDeclaration ]
</details>

#### ExtractMethodExpressionValidation>>#isNotInsideATempDeclaration

<details>
	<summary>See more</summary>
	
	isNotInsideATempDeclaration

	methodNode completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isTemporariesDeclaration
			and: [ sourceRanges anySatisfy: [ :sourceRange |
				sourceRange first < intervalToExtract first and: [ sourceRange last > intervalToExtract last ] ] ])
			ifTrue: [ ^ false ] ].
	^ true
</details>

#### ExtractMethodExpressionValidation>>#intervalMatchesEndOfStatement

<details>
	<summary>See more</summary>
	
	intervalMatchesEndOfStatement

	^ (self findSourceRangeOfCloserStatementIn: finalNodeAncestors) last = intervalToExtract last
</details>

#### ExtractMethodExpressionValidation>>#parseNodesInCommon

<details>
	<summary>See more</summary>
	
	parseNodesInCommon

	^ initialNodeAncestors intersection: finalNodeAncestors
</details>

#### ExtractMethodExpressionValidation>>#isNotDeclaredWithinIntervalToExtract: aVariableNode

<details>
	<summary>See more</summary>
	
	isNotDeclaredWithinIntervalToExtract: aVariableNode

	^ (methodNode
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isTemporariesDeclaration
			and: [ parseNode declaresVariable: aVariableNode ] ]) not
</details>

#### ExtractMethodExpressionValidation>>#startAndEndNodesShareAParentNode

<details>
	<summary>See more</summary>
	
	startAndEndNodesShareAParentNode  

	| parseNodesInCommon |
	parseNodesInCommon _ self parseNodesInCommon.
	
	^ parseNodesInCommon notEmpty and: [
		(self trimmed: parseNodesInCommon first value) = intervalToExtract]



</details>

## ExtractMethodNewSelectorPrecondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ExtractMethodNewSelectorPrecondition>>#signalNewSelectorBeginsWithAnInvalidCharacter

<details>
	<summary>See more</summary>
	
	signalNewSelectorBeginsWithAnInvalidCharacter

	self refactoringError: self class invalidStartingCharacterOfNewSelectorErrorMessage
</details>

#### ExtractMethodNewSelectorPrecondition>>#assertNewSelectorBeginsWithAValidCharacter

<details>
	<summary>See more</summary>
	
	assertNewSelectorBeginsWithAValidCharacter

	selectorToValidate first isValidStartOfIdentifiers 
		ifFalse: [ self signalNewSelectorBeginsWithAnInvalidCharacter ]
</details>

#### ExtractMethodNewSelectorPrecondition>>#assertNewSelectorIsNotEmpty

<details>
	<summary>See more</summary>
	
	assertNewSelectorIsNotEmpty

	selectorToValidate ifEmpty: [ self signalNewSelectorCanNotBeEmptyError ]
</details>

#### ExtractMethodNewSelectorPrecondition>>#value

<details>
	<summary>See more</summary>
	
	value

	self
		assertNewSelectorIsNotEmpty;
		assertNewSelectorDoesNotContainSeparators;
		assertNewSelectorBeginsWithAValidCharacter;
		assertNewSelectorContainsOnlyValidCharacters;
		assertNewSelectorIsNotAlreadyDefinedInTheClass
</details>

#### ExtractMethodNewSelectorPrecondition>>#assertNewSelectorIsNotAlreadyDefinedInTheClass

<details>
	<summary>See more</summary>
	
	assertNewSelectorIsNotAlreadyDefinedInTheClass

	(classToDefineSelector includesSelector: selectorToValidate)
		ifTrue: [ self signalNewSelectorIsAlreadyDefinedInTheClassError ]
</details>

#### ExtractMethodNewSelectorPrecondition>>#signalNewSelectorCanNotBeEmptyError

<details>
	<summary>See more</summary>
	
	signalNewSelectorCanNotBeEmptyError

	self refactoringError: self class newSelectorCanNotBeEmptyErrorMessage
</details>

#### ExtractMethodNewSelectorPrecondition>>#signalNewSelectorCanNotContainSeparatorsError

<details>
	<summary>See more</summary>
	
	signalNewSelectorCanNotContainSeparatorsError

	self refactoringError: self class newSelectorCanNotContainSeparatorsErrorMessage
</details>

#### ExtractMethodNewSelectorPrecondition>>#signalNewSelectorIsAlreadyDefinedInTheClassError

<details>
	<summary>See more</summary>
	
	signalNewSelectorIsAlreadyDefinedInTheClassError

	self refactoringError: self class newSelectorAlreadyDefinedOnTheClassErrorMessage
</details>

#### ExtractMethodNewSelectorPrecondition>>#assertNewSelectorContainsOnlyValidCharacters

<details>
	<summary>See more</summary>
	
	assertNewSelectorContainsOnlyValidCharacters

	selectorToValidate isValidSelector
		ifFalse: [ self signalNewSelectorContainsInvalidCharactersError ]
</details>

#### ExtractMethodNewSelectorPrecondition>>#signalNewSelectorContainsInvalidCharactersError

<details>
	<summary>See more</summary>
	
	signalNewSelectorContainsInvalidCharactersError

	self refactoringError: self class invalidCharacterInsideNewSelectorErrorMessage
</details>

#### ExtractMethodNewSelectorPrecondition>>#assertNewSelectorDoesNotContainSeparators

<details>
	<summary>See more</summary>
	
	assertNewSelectorDoesNotContainSeparators

	(selectorToValidate anySatisfy: [ :character | character isSeparator ])
		ifTrue: [ self signalNewSelectorCanNotContainSeparatorsError ]
</details>

#### ExtractMethodNewSelectorPrecondition>>#initializeFor: aSelectorToValidate on: aClassToDefineSelector

<details>
	<summary>See more</summary>
	
	initializeFor: aSelectorToValidate on: aClassToDefineSelector

	selectorToValidate _ aSelectorToValidate.
	classToDefineSelector _ aClassToDefineSelector
</details>

## ExtractMethodParametersDetector

I am responsible for returning the parse nodes we need to parameterize before performing an extract method refactoring.

### Methods
#### ExtractMethodParametersDetector>>#intervalToExtractIsCoveredByAnyOf: sourceRanges

<details>
	<summary>See more</summary>
	
	intervalToExtractIsCoveredByAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first < intervalToExtract first
			and: [ sourceRange last > intervalToExtract last ] ]
</details>

#### ExtractMethodParametersDetector>>#isNotExtractedAlongWithItsDeclaration: parseNode

<details>
	<summary>See more</summary>
	
	isNotExtractedAlongWithItsDeclaration: parseNode

	^ (methodNodeToRefactor
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNodeInInterval |
			parseNodeInInterval isTemporariesDeclaration
				and: [ parseNode isVariableNode ]
				and: [ parseNodeInInterval declaresVariable: parseNode ] ]) not
</details>

#### ExtractMethodParametersDetector>>#value

<details>
	<summary>See more</summary>
	
	value

	| parseNodesFound |
	parseNodesFound := OrderedCollection new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ parseNodesFound add: parseNode ]
	].
	^ parseNodesFound
</details>

#### ExtractMethodParametersDetector>>#blockNodesEnclosingIntervalToExtract

<details>
	<summary>See more</summary>
	
	blockNodesEnclosingIntervalToExtract

	| nodes |
	nodes := Set new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode and: [ self intervalToExtractIsCoveredByAnyOf: sourceRanges ])
			ifTrue: [ nodes add: parseNode ] ].
	^ nodes
</details>

#### ExtractMethodParametersDetector>>#intervalToExtractIncludesAnyOf: sourceRanges

<details>
	<summary>See more</summary>
	
	intervalToExtractIncludesAnyOf: sourceRanges

	^ sourceRanges anySatisfy: [ :sourceRange |
		intervalToExtract includes: sourceRange first ]
</details>

#### ExtractMethodParametersDetector>>#nodesThatAddVariablesToScope

<details>
	<summary>See more</summary>
	
	nodesThatAddVariablesToScope

	^ (Set with: methodNodeToRefactor)
		addAll: self blockNodesEnclosingIntervalToExtract;
		yourself
</details>

#### ExtractMethodParametersDetector>>#shouldBeParameterized: parseNode appearingIn: sourceRanges

<details>
	<summary>See more</summary>
	
	shouldBeParameterized: parseNode appearingIn: sourceRanges

	^ (self intervalToExtractIncludesAnyOf: sourceRanges)
		and: [ parseNode isTempOrArg ]
		and: [ self definedInOuterScope: parseNode ]
		and: [ self isNotExtractedAlongWithItsDeclaration: parseNode ]
</details>

#### ExtractMethodParametersDetector>>#definedInOuterScope: parseNode

<details>
	<summary>See more</summary>
	
	definedInOuterScope: parseNode

	^ self nodesThatAddVariablesToScope
		anySatisfy: [ :node | node hasLocallyArgumentOrTemporaryNamed: parseNode name ]
</details>

#### ExtractMethodParametersDetector>>#initializeFor: aMethodNodeToRefactor at: anIntervalToExtract

<details>
	<summary>See more</summary>
	
	initializeFor: aMethodNodeToRefactor at: anIntervalToExtract

	methodNodeToRefactor := aMethodNodeToRefactor.
	intervalToExtract := anIntervalToExtract
</details>

## ExtractToTemporary

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ExtractToTemporary>>#hasTemporariesDeclarationBlock

<details>
	<summary>See more</summary>
	
	hasTemporariesDeclarationBlock

	^ parseNodeWithNewVariableScope temporariesDeclaration declarationWritten
</details>

#### ExtractToTemporary>>#positionToInsertNewTemporaryVariableAssignment

<details>
	<summary>See more</summary>
	
	positionToInsertNewTemporaryVariableAssignment

	^ (methodNodeToRefactor singleCompleteSourceRangeOf: self statementNodeIncludingCodeToExtract) first
</details>

#### ExtractToTemporary>>#addNewTemporaryVariableToExistingDeclarationStatement

<details>
	<summary>See more</summary>
	
	addNewTemporaryVariableToExistingDeclarationStatement

	| sourceRangeOfLastTempDeclaration positionOfLastTempDeclaration |
	parseNodeWithNewVariableScope hasTemporaryVariables
		ifTrue: [
			sourceRangeOfLastTempDeclaration := methodNodeToRefactor singleCompleteSourceRangeOf: self lastTemporaryDeclaration.
			positionOfLastTempDeclaration := sourceRangeOfLastTempDeclaration last + 1 ]
		ifFalse: [
			 sourceRangeOfLastTempDeclaration := methodNodeToRefactor singleCompleteSourceRangeOf: parseNodeWithNewVariableScope temporariesDeclaration.
			positionOfLastTempDeclaration := sourceRangeOfLastTempDeclaration last - 1 ].

	self insertAt: positionOfLastTempDeclaration newCodeWith: ' ' , newVariableName
</details>

#### ExtractToTemporary>>#apply

<details>
	<summary>See more</summary>
	
	apply

	self
		replaceExtractedCodeWithNewTemporaryVariable;
		writeAssignmentStatementOfNewTemporaryVariable;
		declareNewTemporaryVariable;
		reflectSourceCodeChanges
</details>

#### ExtractToTemporary>>#initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor declaringTempIn: blockNodeOfNewVariable

<details>
	<summary>See more</summary>
	
	initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor declaringTempIn: blockNodeOfNewVariable

	newVariableName _ aNewVariable.
	intervalToExtract _ anIntervalToExtract.
	methodToRefactor _ aMethodToRefactor.
	methodNodeToRefactor _ methodToRefactor methodNode.
	updatedSourceCode _ aMethodToRefactor sourceCode.
	sourceCodeToExtract _ updatedSourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	parseNodeWithNewVariableScope _ blockNodeOfNewVariable
</details>

#### ExtractToTemporary>>#insertAt: aPositionInSourceCode newCodeWith: sourceCodeContents

<details>
	<summary>See more</summary>
	
	insertAt: aPositionInSourceCode newCodeWith: sourceCodeContents

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: aPositionInSourceCode
		to: aPositionInSourceCode - 1
		with: sourceCodeContents
</details>

#### ExtractToTemporary>>#siblingStatementsOfTemporaryAssignment

<details>
	<summary>See more</summary>
	
	siblingStatementsOfTemporaryAssignment

	^ parseNodeWithNewVariableScope isBlockNode
		ifTrue: [ parseNodeWithNewVariableScope statements ]
		ifFalse: [ parseNodeWithNewVariableScope block statements ]
</details>

#### ExtractToTemporary>>#writeAssignmentStatementOfNewTemporaryVariable

<details>
	<summary>See more</summary>
	
	writeAssignmentStatementOfNewTemporaryVariable

	self
		insertAt: self positionToInsertNewTemporaryVariableAssignment
		newCodeWith: self formattedNewVariableAssignment
</details>

#### ExtractToTemporary>>#lastTemporaryDeclaration

<details>
	<summary>See more</summary>
	
	lastTemporaryDeclaration

	^ parseNodeWithNewVariableScope temporariesDeclaration temporaryDeclarationNodes last
</details>

#### ExtractToTemporary>>#statementNodeIncludingCodeToExtract

<details>
	<summary>See more</summary>
	
	statementNodeIncludingCodeToExtract

	^ self siblingStatementsOfTemporaryAssignment detect: [ :statement |
		(methodNodeToRefactor singleCompleteSourceRangeOf: statement) last >= intervalToExtract last ]
</details>

#### ExtractToTemporary>>#formattedNewVariableAssignment

<details>
	<summary>See more</summary>
	
	formattedNewVariableAssignment

	| newVariableAssignment |
	newVariableAssignment := newVariableName , ' ' , self preferredAssignmentOperator , ' ' , sourceCodeToExtract , '.'.
	^ newVariableAssignment , String newLineString , String tab
</details>

#### ExtractToTemporary>>#insertNewTemporaryDeclarationWithNewVariable

<details>
	<summary>See more</summary>
	
	insertNewTemporaryDeclarationWithNewVariable

	| newVariableDeclaration positionToInsertTempVarDeclaration sourceRangeOfFirstStatement |
	sourceRangeOfFirstStatement := methodNodeToRefactor singleCompleteSourceRangeOf: self siblingStatementsOfTemporaryAssignment first.
	positionToInsertTempVarDeclaration := sourceRangeOfFirstStatement first.
	newVariableDeclaration := '| ' , newVariableName , ' |' , String newLineString , String tab.

	self insertAt: positionToInsertTempVarDeclaration newCodeWith: newVariableDeclaration
</details>

#### ExtractToTemporary>>#declareNewTemporaryVariable

<details>
	<summary>See more</summary>
	
	declareNewTemporaryVariable

	self hasTemporariesDeclarationBlock
		ifTrue: [ self addNewTemporaryVariableToExistingDeclarationStatement ]
		ifFalse: [ self insertNewTemporaryDeclarationWithNewVariable ]
</details>

#### ExtractToTemporary>>#preferredAssignmentOperator

<details>
	<summary>See more</summary>
	
	preferredAssignmentOperator

	^ Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
		ifTrue: [ '_' ]
		ifFalse: [ ':=' ]
</details>

#### ExtractToTemporary>>#replaceExtractedCodeWithNewTemporaryVariable

<details>
	<summary>See more</summary>
	
	replaceExtractedCodeWithNewTemporaryVariable

	updatedSourceCode := updatedSourceCode
		copyReplaceFrom: intervalToExtract first
		to: intervalToExtract last
		with: newVariableName
</details>

#### ExtractToTemporary>>#reflectSourceCodeChanges

<details>
	<summary>See more</summary>
	
	reflectSourceCodeChanges

	self flag: #RNG. "remove the error handler once all the cases are supported"

	[ methodToRefactor methodClass
		compile: updatedSourceCode
		classified: methodToRefactor category ]
	on: SyntaxErrorNotification
	do: [ :syntaxError | self class refactoringError: 'Syntax error: unsupported refactoring case' ]
</details>

## ExtractToTemporaryApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ExtractToTemporaryApplier>>#initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom

<details>
	<summary>See more</summary>
	
	initializeFor: anIntervalToExtract of: aMethodToExtractCodeFrom

	intervalToExtract _ anIntervalToExtract.
	methodToExtractCodeFrom _ aMethodToExtractCodeFrom.
</details>

#### ExtractToTemporaryApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^ self refactoringClass
		named: newVariable
		at: intervalToExtract
		from: methodToExtractCodeFrom
</details>

#### ExtractToTemporaryApplier>>#askNewVariableName

<details>
	<summary>See more</summary>
	
	askNewVariableName

	newVariable := (self request: 'Enter new temp name:' initialAnswer: '') withBlanksTrimmed
</details>

#### ExtractToTemporaryApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askNewVariableName
</details>

#### ExtractToTemporaryApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	
</details>

#### ExtractToTemporaryApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^ ExtractToTemporary
</details>

## InsertSuperclass

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### InsertSuperclass>>#apply

<details>
	<summary>See more</summary>
	
	apply

	| newSuperclass |
	
	newSuperclass := self createSuperclass.
	self changeSuperclassTo: newSuperclass.

	^newSuperclass 
</details>

#### InsertSuperclass>>#createSuperclass

<details>
	<summary>See more</summary>
	
	createSuperclass
	
	^classToRefactor superclass subclass: superclassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: classToRefactor category.
</details>

#### InsertSuperclass>>#initializeTo: aClass named: aSuperclassName

<details>
	<summary>See more</summary>
	
	initializeTo: aClass named: aSuperclassName 

	classToRefactor := aClass.
	superclassName := aSuperclassName.
</details>

#### InsertSuperclass>>#changeSuperclassTo: newSuperclass

<details>
	<summary>See more</summary>
	
	changeSuperclassTo: newSuperclass
	
	newSuperclass subclass: classToRefactor name
		instanceVariableNames: classToRefactor instanceVariablesString 
		classVariableNames: classToRefactor classVariablesString 
		poolDictionaries: classToRefactor sharedPoolsString
		category: classToRefactor category.
</details>

## InsertSuperclassApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### InsertSuperclassApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^InsertSuperclass to: classToRefactor named: newSuperclassName
</details>

#### InsertSuperclassApplier>>#askNewSuperclassName

<details>
	<summary>See more</summary>
	
	askNewSuperclassName
	newSuperclassName _ self
		request: 'Enter new superclass name:'.
	newSuperclassName _ newSuperclassName withBlanksTrimmed asSymbol.
</details>

#### InsertSuperclassApplier>>#initializeOn: aBrowser for: aClass

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aClass
	browser _ aBrowser.
	classToRefactor _ aClass.
</details>

#### InsertSuperclassApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askNewSuperclassName
</details>

#### InsertSuperclassApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges
	
	browser changed: #classList.

</details>

## MoveInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveInstanceVariable>>#initializeNamed: anInstanceVariableToMove from: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeNamed: anInstanceVariableToMove from: aClassToRefactor

	instanceVariableToMove := anInstanceVariableToMove.
	classToRefactor := aClassToRefactor.
</details>

## MoveInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveInstanceVariableApplier>>#initializeOn: aBrowserWindow for: anInstanceVariableName at: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowserWindow for: anInstanceVariableName at: aClassToRefactor 
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	instanceVariableName := anInstanceVariableName
</details>

#### MoveInstanceVariableApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self chooseInstanceVariable
</details>

#### MoveInstanceVariableApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser
</details>

#### MoveInstanceVariableApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser

	browser acceptedContentsChanged.
</details>

#### MoveInstanceVariableApplier>>#chooseInstanceVariable

<details>
	<summary>See more</summary>
	
	chooseInstanceVariable
	
	instanceVariableName ifNotNil: [ ^self ].
		
	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anInstanceVariable | ^instanceVariableName := anInstanceVariable ].
		
	self endRequest 
</details>

## MoveMethod

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveMethod>>#methodCategory

<details>
	<summary>See more</summary>
	
	methodCategory

	^method methodClass organization categoryOfElement: method selector
</details>

#### MoveMethod>>#removeMethod

<details>
	<summary>See more</summary>
	
	removeMethod

	method methodClass removeSelector: method selector.
 
</details>

#### MoveMethod>>#apply

<details>
	<summary>See more</summary>
	
	apply

	self
		moveMethod;
		removeMethod
 
</details>

#### MoveMethod>>#moveMethod

<details>
	<summary>See more</summary>
	
	moveMethod

	self subclassResponsibility 
</details>

#### MoveMethod>>#initializeFor: aMethodToPushup

<details>
	<summary>See more</summary>
	
	initializeFor: aMethodToPushup

	method := aMethodToPushup
</details>

## MoveMethodApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveMethodApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^self refactoringClass for: methodToMove.
</details>

#### MoveMethodApplier>>#initializeOn: aBrowser for: aMethodToMove

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aMethodToMove

	browser := aBrowser.
	methodToMove := aMethodToMove.
</details>

#### MoveMethodApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	
</details>

#### MoveMethodApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser.
</details>

#### MoveMethodApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser

	browser
		reformulateList;
		changed: #messageList;
		setClassOrganizer
</details>

#### MoveMethodApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	self subclassResponsibility 
</details>

## MoveToInstanceOrClassMethod

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveToInstanceOrClassMethod>>#moveMethod

<details>
	<summary>See more</summary>
	
	moveMethod

	| targetClass |

	targetClass := method methodClass isMeta
		ifTrue: [method methodClass soleInstance]
		ifFalse: [method methodClass class].

	targetClass
		compile: method sourceCode
		classified: self methodCategory.

	
</details>

## MoveToInstanceOrClassMethodApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### MoveToInstanceOrClassMethodApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^MoveToInstanceOrClassMethod
</details>

#### MoveToInstanceOrClassMethodApplier>>#methodHasSenders

<details>
	<summary>See more</summary>
	
	methodHasSenders

	^(methodToMove methodClass whichSelectorsReferTo: methodToMove selector) isEmpty not.
</details>

#### MoveToInstanceOrClassMethodApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self methodHasSenders ifTrue: [
		(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ]
	]
</details>

#### MoveToInstanceOrClassMethodApplier>>#confirmationMessageText

<details>
	<summary>See more</summary>
	
	confirmationMessageText
	
	^'This message has senders. Are you sure you want to move it?'
</details>

## NewClassPrecondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### NewClassPrecondition>>#signalNewClassIsUndeclared

<details>
	<summary>See more</summary>
	
	signalNewClassIsUndeclared

	self refactoringError: (self class errorMessageForNewClassIsUndeclared: newClassName).
</details>

#### NewClassPrecondition>>#assertNewClassNameStartsWithRightLetter

<details>
	<summary>See more</summary>
	
	assertNewClassNameStartsWithRightLetter

	newClassName first isUppercase ifFalse: [ self signalNewNameMustStartWithRightLetter]
</details>

#### NewClassPrecondition>>#signalNewClassNameCanNotBeEmpty

<details>
	<summary>See more</summary>
	
	signalNewClassNameCanNotBeEmpty
	
	self refactoringError: self class newClassNameCanNotBeEmptyErrorMessage
</details>

#### NewClassPrecondition>>#value

<details>
	<summary>See more</summary>
	
	value

	self assertNewClassNameIsNotEmpty.
	self assertNewClassNameSymbol.
	self assertNewClassNameStartsWithRightLetter.
	self assertNewClassNameHasNoSeparators.
	self assertNewClassNameDoesNotExistInSystem.
	self assertNewClassNameIsNotDeclaredInUndeclared.


</details>

#### NewClassPrecondition>>#assertNewClassNameIsNotDeclaredInUndeclared

<details>
	<summary>See more</summary>
	
	assertNewClassNameIsNotDeclaredInUndeclared

	(undeclared includesKey: newClassName) ifTrue: [ self signalNewClassIsUndeclared]
</details>

#### NewClassPrecondition>>#assertNewClassNameDoesNotExistInSystem

<details>
	<summary>See more</summary>
	
	assertNewClassNameDoesNotExistInSystem

	system at: newClassName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists]
			ifFalse: [  self signalGlobalAlreadyExists]].
</details>

#### NewClassPrecondition>>#signalGlobalAlreadyExists

<details>
	<summary>See more</summary>
	
	signalGlobalAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newClassName)
</details>

#### NewClassPrecondition>>#assertNewClassNameIsNotEmpty

<details>
	<summary>See more</summary>
	
	assertNewClassNameIsNotEmpty

	newClassName withBlanksTrimmed isEmpty ifTrue: [ self signalNewClassNameCanNotBeEmpty]
</details>

#### NewClassPrecondition>>#initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary

<details>
	<summary>See more</summary>
	
	initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	newClassName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary 
</details>

#### NewClassPrecondition>>#assertNewClassNameSymbol

<details>
	<summary>See more</summary>
	
	assertNewClassNameSymbol

	newClassName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]
</details>

#### NewClassPrecondition>>#signalClassAlreadyExists

<details>
	<summary>See more</summary>
	
	signalClassAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newClassName).
</details>

#### NewClassPrecondition>>#signalNewNameMustStartWithRightLetter

<details>
	<summary>See more</summary>
	
	signalNewNameMustStartWithRightLetter

	self refactoringError: self class newNameMustStartWithRightLetterErrorMessage.
</details>

#### NewClassPrecondition>>#assertNewClassNameHasNoSeparators

<details>
	<summary>See more</summary>
	
	assertNewClassNameHasNoSeparators

	(newClassName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewClassNameCanNotHaveSeparators]
</details>

#### NewClassPrecondition>>#signalNewClassNameCanNotHaveSeparators

<details>
	<summary>See more</summary>
	
	signalNewClassNameCanNotHaveSeparators
	
	self refactoringError: self class newClassNameCanNotHaveSeparatorsErrorMessage 
</details>

#### NewClassPrecondition>>#signalNewNameMustBeSymbol

<details>
	<summary>See more</summary>
	
	signalNewNameMustBeSymbol

	self refactoringError: self class newNameMustBeSymbolErrorMessage.
</details>

## NewGlobalPrecondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### NewGlobalPrecondition>>#initializeFor: aNewName in: aSystem

<details>
	<summary>See more</summary>
	
	initializeFor: aNewName in: aSystem  

	newName := aNewName.
	system := aSystem.

</details>

#### NewGlobalPrecondition>>#signalNewNameCanNotHaveSeparators

<details>
	<summary>See more</summary>
	
	signalNewNameCanNotHaveSeparators
	
	self refactoringError: self class newNameCanNotHaveSeparatorsErrorMessage 
</details>

#### NewGlobalPrecondition>>#signalNewNameCanNotBeEmpty

<details>
	<summary>See more</summary>
	
	signalNewNameCanNotBeEmpty
	
	self refactoringError: self class newNameCanNotBeEmptyErrorMessage
</details>

#### NewGlobalPrecondition>>#value

<details>
	<summary>See more</summary>
	
	value

	self 
		assertNewNameIsNotEmpty;
		assertNewNameIsSymbol;
		assertNewNameHasNoSeparators;
		assertNewNameDoesNotExistInSystem.


</details>

#### NewGlobalPrecondition>>#assertNewNameIsNotEmpty

<details>
	<summary>See more</summary>
	
	assertNewNameIsNotEmpty

	newName withBlanksTrimmed isEmpty ifTrue: [ self signalNewNameCanNotBeEmpty]
</details>

#### NewGlobalPrecondition>>#signalGlobalAlreadyExists

<details>
	<summary>See more</summary>
	
	signalGlobalAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newName)
</details>

#### NewGlobalPrecondition>>#assertNewNameHasNoSeparators

<details>
	<summary>See more</summary>
	
	assertNewNameHasNoSeparators

	(newName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewNameCanNotHaveSeparators ]
</details>

#### NewGlobalPrecondition>>#signalClassAlreadyExists

<details>
	<summary>See more</summary>
	
	signalClassAlreadyExists

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newName).
</details>

#### NewGlobalPrecondition>>#assertNewNameDoesNotExistInSystem

<details>
	<summary>See more</summary>
	
	assertNewNameDoesNotExistInSystem

	system at: newName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists ]
			ifFalse: [ self signalGlobalAlreadyExists ]].
</details>

#### NewGlobalPrecondition>>#signalNewNameMustBeSymbol

<details>
	<summary>See more</summary>
	
	signalNewNameMustBeSymbol

	self refactoringError: self class newNameMustBeSymbolErrorMessage.
</details>

#### NewGlobalPrecondition>>#assertNewNameIsSymbol

<details>
	<summary>See more</summary>
	
	assertNewNameIsSymbol

	newName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]
</details>

## NewInstanceVariablePrecondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### NewInstanceVariablePrecondition>>#methodsDefiningNewVariable

<details>
	<summary>See more</summary>
	
	methodsDefiningNewVariable
	
	| methodsDefiningNewVariableInHierarchy |
	
	methodsDefiningNewVariableInHierarchy := OrderedCollection new.
	
	classToAddInstVar withAllSubclassesDo: [ :class | 
		methodsDefiningNewVariableInHierarchy addAll: (class methodsWithArgumentOrTemporaryNamed: instVarName) ].

	^methodsDefiningNewVariableInHierarchy 
		
	
</details>

#### NewInstanceVariablePrecondition>>#assertIsValidInstanceVariableName

<details>
	<summary>See more</summary>
	
	assertIsValidInstanceVariableName

	| scannedNames |

	scannedNames := Scanner new scanFieldNames: instVarName .
	scannedNames size = 1 ifFalse: [ self signalInvalidInstanceVariable ].
	scannedNames first = instVarName ifFalse: [ self signalInvalidInstanceVariable ].
</details>

#### NewInstanceVariablePrecondition>>#signalInvalidInstanceVariable

<details>
	<summary>See more</summary>
	
	signalInvalidInstanceVariable

	^ self refactoringError: (self class errorMessageForInvalidInstanceVariable: instVarName).
</details>

#### NewInstanceVariablePrecondition>>#initializeOf: anInstanceVariableName for: aClass

<details>
	<summary>See more</summary>
	
	initializeOf: anInstanceVariableName for: aClass

	instVarName := anInstanceVariableName withBlanksTrimmed.
	classToAddInstVar := aClass.
</details>

#### NewInstanceVariablePrecondition>>#value

<details>
	<summary>See more</summary>
	
	value
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefined.

	self assertIsNotDefinedInMethods.
</details>

#### NewInstanceVariablePrecondition>>#signalNewVariable: newVariable willBeHiddenAtAll: methods

<details>
	<summary>See more</summary>
	
	signalNewVariable: newVariable willBeHiddenAtAll: methods

	^ self refactoringError: (self class errorMessageForNewVariable: newVariable willBeHiddenAtAll: methods).
</details>

#### NewInstanceVariablePrecondition>>#assertIsNotAReservedName

<details>
	<summary>See more</summary>
	
	assertIsNotAReservedName
	
	(ClassBuilder reservedNames includes: instVarName) ifTrue: [ self signalNewInstanceVariableCanNotBeAReservedName ]
</details>

#### NewInstanceVariablePrecondition>>#assertIsNotAlreadyDefinedInSuperclasses

<details>
	<summary>See more</summary>
	
	assertIsNotAlreadyDefinedInSuperclasses

	^ (classToAddInstVar classThatDefinesInstanceVariable: instVarName) 
		ifNotNil: [ :definingClass | self signalAlreadyDefinedInAll: {definingClass} ]
		
</details>

#### NewInstanceVariablePrecondition>>#assertIsNotEmpty

<details>
	<summary>See more</summary>
	
	assertIsNotEmpty

	instVarName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty]
</details>

#### NewInstanceVariablePrecondition>>#signalNewInstanceVariableCanNotBeAReservedName

<details>
	<summary>See more</summary>
	
	signalNewInstanceVariableCanNotBeAReservedName

	self refactoringError: (self class errorMessageForNewInstanceVariableCanNotBeAReservedName: instVarName)
</details>

#### NewInstanceVariablePrecondition>>#valueForSuperclass

<details>
	<summary>See more</summary>
	
	valueForSuperclass
	
	self assertIsNotEmpty.
	self assertIsNotAReservedName.
	self assertIsValidInstanceVariableName.
	self assertIsNotAlreadyDefinedInSuperclasses.

	
</details>

#### NewInstanceVariablePrecondition>>#signalNewVariableCanNotBeEmpty

<details>
	<summary>See more</summary>
	
	signalNewVariableCanNotBeEmpty
	
	self refactoringError: self class newVariableCanNotBeEmptyErrorMessage
</details>

#### NewInstanceVariablePrecondition>>#assertIsNotDefinedInMethods

<details>
	<summary>See more</summary>
	
	assertIsNotDefinedInMethods
	
	| methodsDefiningNewVariable |
	
	methodsDefiningNewVariable := self methodsDefiningNewVariable.
	
	methodsDefiningNewVariable notEmpty ifTrue: [ self signalNewVariable: instVarName willBeHiddenAtAll: methodsDefiningNewVariable ].
</details>

#### NewInstanceVariablePrecondition>>#signalAlreadyDefinedInAll: classes

<details>
	<summary>See more</summary>
	
	signalAlreadyDefinedInAll: classes

	^ self refactoringError: (self class errorMessageForNewInstanceVariable: instVarName alreadyDefinedInAll: classes).
</details>

#### NewInstanceVariablePrecondition>>#assertIsNotAlreadyDefined

<details>
	<summary>See more</summary>
	
	assertIsNotAlreadyDefined
	
	^ classToAddInstVar 
		withClassesThatDefineInHierarchyInstanceVariable: instVarName 
		do: [ :definingClasses | self signalAlreadyDefinedInAll: definingClasses ]
		ifNone: [ ].
</details>

## NewTemporaryPrecondition

I am responsible for checking if a new temporary variable can be introduced in a specific block node of a method. If that is not possible, I raise a refactoring error.

### Methods
#### NewTemporaryPrecondition>>#initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode

<details>
	<summary>See more</summary>
	
	initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode

	newTemporaryVariableName _ aNewTemporaryVariableName.
	blockNode _ aBlockNode.
	methodNode _ aMethodNode
</details>

#### NewTemporaryPrecondition>>#assertIsNotDeclaredInParentsOrChildrenScopes

<details>
	<summary>See more</summary>
	
	assertIsNotDeclaredInParentsOrChildrenScopes

	(self isDeclaredInChildrenOfBlockNode or: [ self isDeclaredInParentsOfBlockNode ])
		ifTrue: [ self signalNewTemporaryVariableisAlreadyDefined ]
</details>

#### NewTemporaryPrecondition>>#signalInvalidTemporaryVariable

<details>
	<summary>See more</summary>
	
	signalInvalidTemporaryVariable

	self refactoringError: (self class errorMessageForInvalidTemporaryVariable: newTemporaryVariableName)
</details>

#### NewTemporaryPrecondition>>#signalNewTemporaryVariableisAlreadyDefined

<details>
	<summary>See more</summary>
	
	signalNewTemporaryVariableisAlreadyDefined

	self refactoringError: (
		self class
			errorMessageForNewTemporaryVariable: newTemporaryVariableName
			isAlreadyDefinedIn: methodNode)
</details>

#### NewTemporaryPrecondition>>#assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass

<details>
	<summary>See more</summary>
	
	assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass

	| classDefiningNewVariable |
	
	classDefiningNewVariable _ methodNode methodClass
		whichClassDefinesInstanceVariable: newTemporaryVariableName ifNone: [ ^ self ].
		
	self signalNewVariableCanNotHideInstanceVariableDefinedIn: classDefiningNewVariable
</details>

#### NewTemporaryPrecondition>>#signalNewVariableCanNotHideInstanceVariableDefinedIn: aClass

<details>
	<summary>See more</summary>
	
	signalNewVariableCanNotHideInstanceVariableDefinedIn: aClass

	self refactoringError: (
		self class
			errorMessageFor: newTemporaryVariableName
			canNotBeNamedAsInstanceVariableDefinedIn: aClass)
</details>

#### NewTemporaryPrecondition>>#isDeclaredInAnyOf: someBlockOrMethodNodes

<details>
	<summary>See more</summary>
	
	isDeclaredInAnyOf: someBlockOrMethodNodes

	^ someBlockOrMethodNodes anySatisfy: [ :node | node  hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]
</details>

#### NewTemporaryPrecondition>>#value

<details>
	<summary>See more</summary>
	
	value

	self
		assertIsNotEmpty;
		assertIsValidVariableName;
		assertIsNotAReservedName;
		assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass;
		assertIsNotDeclaredInParentsOrChildrenScopes
</details>

#### NewTemporaryPrecondition>>#assertIsValidVariableName

<details>
	<summary>See more</summary>
	
	assertIsValidVariableName

	| scannedNames |
	scannedNames _ Scanner new scanFieldNames: newTemporaryVariableName.
	scannedNames size = 1 ifFalse: [ self signalInvalidTemporaryVariable ].
	scannedNames first = newTemporaryVariableName ifFalse: [ self signalInvalidTemporaryVariable ].
</details>

#### NewTemporaryPrecondition>>#assertIsNotAReservedName

<details>
	<summary>See more</summary>
	
	assertIsNotAReservedName
	
	(ClassBuilder reservedNames includes: newTemporaryVariableName)
		ifTrue: [ self signalNewTemporaryVariableCanNotBeAReservedName ]
</details>

#### NewTemporaryPrecondition>>#signalNewTemporaryVariableCanNotBeAReservedName

<details>
	<summary>See more</summary>
	
	signalNewTemporaryVariableCanNotBeAReservedName

	self refactoringError: (
		self class errorMessageForNewTemporaryVariableCanNotBeAReservedName: newTemporaryVariableName)
</details>

#### NewTemporaryPrecondition>>#assertIsNotEmpty

<details>
	<summary>See more</summary>
	
	assertIsNotEmpty

	newTemporaryVariableName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty ]
</details>

#### NewTemporaryPrecondition>>#isDeclaredInChildrenOfBlockNode

<details>
	<summary>See more</summary>
	
	isDeclaredInChildrenOfBlockNode

	blockNode nodesDo: [ :node |
		(node isBlockNode and: [ node hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]) ifTrue: [ ^ true ] ].

	^ false
</details>

#### NewTemporaryPrecondition>>#isDeclaredInParentsOfBlockNode

<details>
	<summary>See more</summary>
	
	isDeclaredInParentsOfBlockNode

	| parents |
	parents _ (BlockNodeParentsFinder for: blockNode) parentsIn: methodNode.
	parents add: methodNode.
	^ self isDeclaredInAnyOf: parents
</details>

#### NewTemporaryPrecondition>>#signalNewVariableCanNotBeEmpty

<details>
	<summary>See more</summary>
	
	signalNewVariableCanNotBeEmpty
	
	self refactoringError: self class errorMessageForEmptyTemporaryVariable
</details>

## ParseNodesDeclaringTemporaryVariableVisitor

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ParseNodesDeclaringTemporaryVariableVisitor>>#visitBlockNodeDeclaringTemporary: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNodeDeclaringTemporary: aBlockNode

	self subclassResponsibility.
</details>

#### ParseNodesDeclaringTemporaryVariableVisitor>>#isNodeDeclaringTemporary: aParseNode

<details>
	<summary>See more</summary>
	
	isNodeDeclaringTemporary: aParseNode

	^aParseNode hasLocallyTemporaryNamed: temporaryVariable 
	
</details>

#### ParseNodesDeclaringTemporaryVariableVisitor>>#initializeFor: aTemporaryVariable

<details>
	<summary>See more</summary>
	
	initializeFor: aTemporaryVariable

	temporaryVariable _ aTemporaryVariable.

</details>

#### ParseNodesDeclaringTemporaryVariableVisitor>>#visitBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode

	(self isNodeDeclaringTemporary: aBlockNode) ifTrue: [ 
		self visitBlockNodeDeclaringTemporary: aBlockNode ].
	
	super visitBlockNode: aBlockNode.
</details>

#### ParseNodesDeclaringTemporaryVariableVisitor>>#visitMethodNodeDeclaringTemporary: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNodeDeclaringTemporary: aMethodNode

	self subclassResponsibility.
</details>

#### ParseNodesDeclaringTemporaryVariableVisitor>>#visitMethodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNode: aMethodNode

	(self isNodeDeclaringTemporary: aMethodNode) ifTrue: [
		self visitMethodNodeDeclaringTemporary: aMethodNode ].
	
	super visitMethodNode: aMethodNode.
</details>

## PushDownInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushDownInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply

	classToRefactor removeInstVarName: instanceVariableToMove.
	self pushDownInstanceVariableToAllSubclasses
</details>

#### PushDownInstanceVariable>>#pushDownInstanceVariableToAllSubclasses

<details>
	<summary>See more</summary>
	
	pushDownInstanceVariableToAllSubclasses

	classToRefactor subclassesDo: [ :subClass |
		subClass addInstVarName: instanceVariableToMove ]
</details>

## PushDownInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushDownInstanceVariableApplier>>#selectVariableLabel

<details>
	<summary>See more</summary>
	
	selectVariableLabel
	
	^'Select instance variable to push down'
</details>

#### PushDownInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^PushDownInstanceVariable named: instanceVariableName from: classToRefactor.
	
</details>

## PushDownMethod

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushDownMethod>>#moveMethod

<details>
	<summary>See more</summary>
	
	moveMethod

	| methodCategory sourceCode |

	methodCategory := self methodCategory.
	sourceCode := method sourceCode.

	method methodClass subclassesDo: [:subclass |
		subclass
			compile: sourceCode
			classified: methodCategory.
	].

</details>

## PushDownMethodApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushDownMethodApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^PushDownMethod
</details>

## PushUpInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushUpInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply

	self removeSubclassesInstanceVariable.
	classToRefactor superclass addInstVarName: instanceVariableToMove.
</details>

#### PushUpInstanceVariable>>#removeSubclassesInstanceVariable

<details>
	<summary>See more</summary>
	
	removeSubclassesInstanceVariable

	classToRefactor superclass subclassesDo: [ :subclass |
		(subclass definesInstanceVariableNamed: instanceVariableToMove) ifTrue: [ subclass removeInstVarName: instanceVariableToMove].
	].
</details>

## PushUpInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushUpInstanceVariableApplier>>#selectVariableLabel

<details>
	<summary>See more</summary>
	
	selectVariableLabel
	
	^'Select instance variable to push up'
</details>

#### PushUpInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^PushUpInstanceVariable named: instanceVariableName from: classToRefactor.
	
</details>

## PushUpMethod

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushUpMethod>>#moveMethod

<details>
	<summary>See more</summary>
	
	moveMethod

	method methodClass superclass
		compile: method sourceCode
		classified: self methodCategory
</details>

## PushUpMethodApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PushUpMethodApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^PushUpMethod
</details>

## Refactoring

I am a refactoring, a code transformation preserving behavior, based on some input (provided from the end user through a RefactoringApplier; or provided programmatically). Instances of me have usually only public method, #apply, which does all the work. In case the refactoring cannot be made, or there is a problem during the application of it, I can throw errors using the class message #refactoringError:, or warnings using the class message #refactoringWarning:

### Methods
#### Refactoring>>#apply

<details>
	<summary>See more</summary>
	
	apply

	self subclassResponsibility 
</details>

## RefactoringApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RefactoringApplier>>#request: aLabel initialAnswer: anAnswer onCancel: cancelBlock

<details>
	<summary>See more</summary>
	
	request: aLabel initialAnswer: anAnswer onCancel: cancelBlock

	^FillInTheBlankMorph request: aLabel initialAnswer: anAnswer onCancel: cancelBlock 
</details>

#### RefactoringApplier>>#handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError

<details>
	<summary>See more</summary>
	
	handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError
	
	| options answer question |
	
	options := 
'Browse references
Cancel'.

	question := PopUpMenu labels: options icons: #(mailForwardIcon cancelIcon).
	answer := question startUpWithCaption: aCanNotRefactorDueToReferencesError messageText.
	
	answer = 1 ifTrue: [ self browseReferencesOn: aCanNotRefactorDueToReferencesError ].
	self endRequest.
</details>

#### RefactoringApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	self subclassResponsibility 
</details>

#### RefactoringApplier>>#createRefactoringHandlingRefactoringExceptions

<details>
	<summary>See more</summary>
	
	createRefactoringHandlingRefactoringExceptions

	self valueHandlingRefactoringExceptions: [ refactoring := self createRefactoring ]
	
</details>

#### RefactoringApplier>>#requestRefactoringParametersHandlingRefactoringExceptions

<details>
	<summary>See more</summary>
	
	requestRefactoringParametersHandlingRefactoringExceptions

	self valueHandlingRefactoringExceptions: [ self requestRefactoringParameters ]
	
</details>

#### RefactoringApplier>>#endRequest

<details>
	<summary>See more</summary>
	
	endRequest

	^requestExitBlock value
</details>

#### RefactoringApplier>>#value

<details>
	<summary>See more</summary>
	
	value

	requestExitBlock := [ ^self ].
	
	self 
		requestRefactoringParametersHandlingRefactoringExceptions;
		createRefactoringHandlingRefactoringExceptions;
		applyRefactoring;
		showChanges
	
	
</details>

#### RefactoringApplier>>#handleReferencesWarning: aReferencesWarning

<details>
	<summary>See more</summary>
	
	handleReferencesWarning: aReferencesWarning
	
	| options answer question |
	
	options := 
'Browse references and Cancel
Browse references and Continue
Continue'.

	question := PopUpMenu labels: options icons: #(cancelIcon mailForwardIcon acceptIcon).
	answer := question startUpWithCaption: aReferencesWarning messageText.
	
	answer <= 2 ifTrue: [ self browseReferencesOn: aReferencesWarning ].
	answer = 1 ifTrue: [ self endRequest ].
	aReferencesWarning resume.
</details>

#### RefactoringApplier>>#referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError

<details>
	<summary>See more</summary>
	
	referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError

	^'References to ', aCanNotRefactorDueToReferencesError referencee asString
</details>

#### RefactoringApplier>>#handleRefactoringWarning: aRefactoringWarning

<details>
	<summary>See more</summary>
	
	handleRefactoringWarning: aRefactoringWarning
	
	(self confirm: aRefactoringWarning messageText, '. Continue?')
		ifTrue: [ aRefactoringWarning resume ]
		ifFalse: [ self endRequest]
</details>

#### RefactoringApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self subclassResponsibility 
</details>

#### RefactoringApplier>>#handleRefactoringError: aRefactoringError

<details>
	<summary>See more</summary>
	
	handleRefactoringError: aRefactoringError 

	self inform: aRefactoringError messageText.
	self endRequest 
</details>

#### RefactoringApplier>>#request: aLabel initialAnswer: anAnswer

<details>
	<summary>See more</summary>
	
	request: aLabel initialAnswer: anAnswer

	^self request: aLabel initialAnswer: anAnswer onCancel: requestExitBlock 
</details>

#### RefactoringApplier>>#request: aLabel

<details>
	<summary>See more</summary>
	
	request: aLabel
 
	^self request: aLabel initialAnswer: ''

</details>

#### RefactoringApplier>>#applyRefactoring

<details>
	<summary>See more</summary>
	
	applyRefactoring

	changes := refactoring apply
</details>

#### RefactoringApplier>>#browseReferencesOn: aCanNotRefactorDueToReferencesError

<details>
	<summary>See more</summary>
	
	browseReferencesOn: aCanNotRefactorDueToReferencesError
	
	Smalltalk 
		browseMessageList: aCanNotRefactorDueToReferencesError references 
		name: (self referencesBrowserTitleOn: aCanNotRefactorDueToReferencesError)
		autoSelect: aCanNotRefactorDueToReferencesError referencee asString

</details>

#### RefactoringApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self subclassResponsibility 	
	
</details>

#### RefactoringApplier>>#valueHandlingRefactoringExceptions: aBlock

<details>
	<summary>See more</summary>
	
	valueHandlingRefactoringExceptions: aBlock

	^[[[aBlock
		on: Refactoring referencesWarningClass
		do: [ :aReferencesRefactoringWarning | self handleReferencesWarning: aReferencesRefactoringWarning ]]
		on: Refactoring refactoringWarningClass 
		do: [ :aRefactoringWarning | self handleRefactoringWarning: aRefactoringWarning ]]
		on: Refactoring canNotRefactorDueToReferencesErrorClass
		do: [ :aCanNotRefactorDueToReferencesError | self handleCanNotRefactorDueToReferencesError: aCanNotRefactorDueToReferencesError ]]
		on: Refactoring refactoringErrorClass 
		do: [ :aRefactoringError | self handleRefactoringError: aRefactoringError ]
	
</details>

## RefactoringError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## RefactoringMenues

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## RefactoringPrecondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RefactoringPrecondition>>#refactoringWarning: aMessageText

<details>
	<summary>See more</summary>
	
	refactoringWarning: aMessageText

	^ Refactoring refactoringWarning: aMessageText 
</details>

#### RefactoringPrecondition>>#value

<details>
	<summary>See more</summary>
	
	value

	self subclassResponsibility
</details>

#### RefactoringPrecondition>>#refactoringError: aMessage

<details>
	<summary>See more</summary>
	
	refactoringError: aMessage

	Refactoring refactoringError: aMessage.
</details>

## RefactoringWarning

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## ReferencesRefactoringWarning

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ReferencesRefactoringWarning>>#references

<details>
	<summary>See more</summary>
	
	references
	
	^references
</details>

#### ReferencesRefactoringWarning>>#referencee

<details>
	<summary>See more</summary>
	
	referencee
	
	^primaryReferencee 
</details>

#### ReferencesRefactoringWarning>>#numberOfReferences

<details>
	<summary>See more</summary>
	
	numberOfReferences
	
	^references size
</details>

#### ReferencesRefactoringWarning>>#initializeWith: aMessageText references: aReferences of: aPrimaryReferencee toAll: anAllReferenced

<details>
	<summary>See more</summary>
	
	initializeWith: aMessageText references: aReferences of: aPrimaryReferencee toAll: anAllReferenced

	self messageText: aMessageText.
	references := aReferences.
	primaryReferencee := aPrimaryReferencee.
	allreferenced := anAllReferenced 
</details>

#### ReferencesRefactoringWarning>>#anyReference

<details>
	<summary>See more</summary>
	
	anyReference
	
	^references anyOne 
</details>

## RemoveAllUnreferencedInstanceVariables

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveAllUnreferencedInstanceVariables>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	| variableNamesToRemove |
	
	variableNamesToRemove := classToRefactor unreferencedInstanceVariables.
	variableNamesToRemove do: [ :aVariableName | classToRefactor removeInstVarName: aVariableName ].
	
	^variableNamesToRemove
</details>

#### RemoveAllUnreferencedInstanceVariables>>#initializeFrom: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeFrom: aClassToRefactor

	classToRefactor := aClassToRefactor 
</details>

## RemoveAllUnreferencedInstanceVariablesApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveAllUnreferencedInstanceVariablesApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^RemoveAllUnreferencedInstanceVariables from: classToRefactor 
</details>

#### RemoveAllUnreferencedInstanceVariablesApplier>>#initializeOn: aBrowser for: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
</details>

#### RemoveAllUnreferencedInstanceVariablesApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	
</details>

#### RemoveAllUnreferencedInstanceVariablesApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	| removedInstanceVariablesMessage |
	
	self informChangesToBrowser.
	removedInstanceVariablesMessage := changes isEmpty 
		ifTrue: [ 'No instance variable was removed' ]
		ifFalse: [ changes size = 1
			ifTrue: [ changes first, ' was removed' ]
			ifFalse: [ changes asCommaStringAnd, ' were removed' ]].
			
	self inform: removedInstanceVariablesMessage
</details>

#### RemoveAllUnreferencedInstanceVariablesApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser
		
	browser acceptedContentsChanged
</details>

## RemoveInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveInstanceVariable>>#initializeNamed: aVariable from: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeNamed: aVariable from: aClassToRefactor 

	variableToRemove := aVariable.
	classToRefactor := aClassToRefactor 
</details>

#### RemoveInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	classToRefactor removeInstVarName: variableToRemove 
</details>

## RemoveInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveInstanceVariableApplier>>#selectVariableLabel

<details>
	<summary>See more</summary>
	
	selectVariableLabel
	
	^'Select instance variable to remove'
</details>

#### RemoveInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^RemoveInstanceVariable named: variableToRemove from: classToRefactor 
</details>

#### RemoveInstanceVariableApplier>>#initializeOn: aBrowser for: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: aClassToRefactor 
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
</details>

#### RemoveInstanceVariableApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self chooseInstanceVariable.

	
</details>

#### RemoveInstanceVariableApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser
</details>

#### RemoveInstanceVariableApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser
		
	browser acceptedContentsChanged
</details>

#### RemoveInstanceVariableApplier>>#chooseInstanceVariable

<details>
	<summary>See more</summary>
	
	chooseInstanceVariable

	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :aVariableToRemove | ^variableToRemove := aVariableToRemove ].
	self endRequest 

	
</details>

## RemoveParameter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveParameter>>#writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition

<details>
	<summary>See more</summary>
	
	writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition
	
	| afterParameterPosition |
	
	afterParameterPosition := parameterToRemovePosition last.
	isLastParameter ifFalse: [ afterParameterPosition := self lastSeparatorIndexIn: originalSource startingFrom: afterParameterPosition ].
	
	newSourceStream nextPutAll: (originalSource copyFrom: afterParameterPosition + 1 to: originalSource size) 	
</details>

#### RemoveParameter>>#keywordAndParameterPositionsIn: aMethodNode

<details>
	<summary>See more</summary>
	
	keywordAndParameterPositionsIn: aMethodNode
	
	^aMethodNode messageSendKeywordAndParameterPositionsAt: parameterIndex of: oldSelector ifAbsent: [ #() ].
</details>

#### RemoveParameter>>#implementorNewSourceCodeOf: anImplementor

<details>
	<summary>See more</summary>
	
	implementorNewSourceCodeOf: anImplementor

	| implementorMethodNode newSource originalSource parameterToRemovePosition selectorToRemovePosition |
	
	implementorMethodNode := anImplementor methodNode.
	selectorToRemovePosition := implementorMethodNode selectorKeywordPositionAt: parameterIndex.
	parameterToRemovePosition := implementorMethodNode parameterDefinitionPositionAt: parameterIndex.

	originalSource := anImplementor sourceCode.
	newSource := String streamContents: [ :newSourceStream | 
		self writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition.
		self writeAfterParameterIn: newSourceStream from: originalSource removing: parameterToRemovePosition ].
		
	^newSource
</details>

#### RemoveParameter>>#lastSeparatorIndexIn: senderSourceCode startingFrom: aPosition

<details>
	<summary>See more</summary>
	
	lastSeparatorIndexIn: senderSourceCode startingFrom: aPosition

	| lastPosition senderSourceCodeSize |
	
	lastPosition := aPosition.
	senderSourceCodeSize := senderSourceCode size.
	[ lastPosition := lastPosition + 1.
	lastPosition <= senderSourceCodeSize and: [ (senderSourceCode at: lastPosition) isSeparator ]] whileTrue. 

	^lastPosition - 1
</details>

#### RemoveParameter>>#addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

<details>
	<summary>See more</summary>
	
	addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| keywordAndParameterPositions senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := self keywordAndParameterPositionsIn: aMethodNode.
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition | | lastPosition |
		lastPosition := self lastSeparatorIndexIn: senderSourceCode startingFrom: aKeywordAndParameterPosition last.
		rangesToKeywords add: (aKeywordAndParameterPosition first to: lastPosition) -> senderReplacementString ]
	
</details>

#### RemoveParameter>>#writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition

<details>
	<summary>See more</summary>
	
	writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition

	newSelector isUnary 
		ifTrue: [ newSourceStream nextPutAll: newSelector ]
		ifFalse: [ newSourceStream nextPutAll: (originalSource copyFrom: 1 to: selectorToRemovePosition first - 1) ].

</details>

#### RemoveParameter>>#initializeNamed: aParameterToRemove
	ofKeywordAtIndex: aParameterIndex
	from: anOldSelector 
	creating: aNewSelector 
	implementors: implementorsCollection 
	senders: sendersCollection

<details>
	<summary>See more</summary>
	
	initializeNamed: aParameterToRemove
	ofKeywordAtIndex: aParameterIndex
	from: anOldSelector 
	creating: aNewSelector 
	implementors: implementorsCollection 
	senders: sendersCollection
	
	super initializeFrom: anOldSelector to: aNewSelector implementors: implementorsCollection senders: sendersCollection.

	parameterToRemove := aParameterToRemove.
	parameterIndex := aParameterIndex.
	senderReplacementString := newSelector isUnary ifTrue: [ newSelector asString ] ifFalse: [ '' ].
	isLastParameter := oldSelector numArgs = parameterIndex 
	
</details>

## RemoveParameterApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RemoveParameterApplier>>#selectParameterIndexToRemoveFrom: parameterNames

<details>
	<summary>See more</summary>
	
	selectParameterIndexToRemoveFrom: parameterNames

	| parameterIndex |

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Select parameter to remove'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	^parameterIndex
</details>

#### RemoveParameterApplier>>#selectKeywordIndexToRemoveFrom: keywords

<details>
	<summary>See more</summary>
	
	selectKeywordIndexToRemoveFrom: keywords

	| keywordIndex |

	keywordIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword related to parameter to remove'.
	keywordIndex = 0 ifTrue: [self endRequest ].

	^keywordIndex
</details>

#### RemoveParameterApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^self refactoringClass
		atIndex: parameterToRemoveIndex
		named: parameterToRemoveName
		from: oldSelector
		implementors: implementors
		senders: senders 
</details>

#### RemoveParameterApplier>>#askKeywordToRemove

<details>
	<summary>See more</summary>
	
	askKeywordToRemove

	| keywords |

	keywords := oldSelector keywords.

	keywords size = 1
		ifTrue: [ parameterToRemoveIndex := 1 ]
		ifFalse: [ parameterToRemoveIndex := self selectKeywordIndexToRemoveFrom: keywords ].

	"Because I do not know the parameter name, I'll use this one as explanation - Hernan"
	parameterToRemoveName := 'Parameter related to keyword ', (keywords at: parameterToRemoveIndex) 
</details>

#### RemoveParameterApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askParameterToRemove
		
		
</details>

#### RemoveParameterApplier>>#askParameterToRemove

If the compiled method does not exist it means that the remove is being executed from the editor, in a message send therefore we can not ask for the parameter name unless we look for implementors or use LiveTyping to look for actual implementors, etc. To make it simpler, when we can know the parameter names, we use that. When we can not, we use the keyword names. I tried to used only keyword names but it is not so intuitive. I decided to use two different ways of asking instead of one (asking for keyword names) becuase I think the programmer prefers to see parameter names. It could happen that the selected class implements the message to remove the parameter but that the remove is executed from the editor (not sending to self), in that case the parameters of selected class implementation will be use... it is a rare case and I think it will not confuse the programmer - Hernan


<details>
	<summary>See more</summary>
	
	askParameterToRemove

	| methodNode parameterNames selectedMethod |

	"If the compiled method does not exist it means that the remove is being executed from the
	editor, in a message send therefore we can not ask for the parameter name unless we look for implementors or
	use LiveTyping to look for actual implementors, etc.
	To make it simpler, when we can know the parameter names, we use that. When we can not, we use the keyword
	names. I tried to used only keyword names but it is not so intuitive. I decided to use two different ways of asking
	instead of one (asking for keyword names) becuase I think the programmer prefers to see parameter names.

	It could happen that the selected class implements the message to remove the parameter but that the remove
	is executed from the editor (not sending to self), in that case the parameters of selected class implementation
	will be use... it is a rare case and I think it will not confuse the programmer  - Hernan"

	selectedMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askKeywordToRemove ].

	methodNode := selectedMethod methodNode.
	parameterNames := methodNode argumentNames.

	parameterToRemoveIndex := parameterNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ self selectParameterIndexToRemoveFrom: parameterNames ].

	parameterToRemoveName := parameterNames at: parameterToRemoveIndex.


	
</details>

#### RemoveParameterApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^RemoveParameter
</details>

## RenameClass

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameClass>>#rangesForLiteralVariableOf: methodNode

<details>
	<summary>See more</summary>
	
	rangesForLiteralVariableOf: methodNode
	
	^methodNode positionsForLiteralVariableNode: classToRenameOriginalName ifAbsent: [ #() ]
	
</details>

#### RenameClass>>#referencesToOldClassName

<details>
	<summary>See more</summary>
	
	referencesToOldClassName
	
	^system allCallsOn: classToRenameOriginalName
</details>

#### RenameClass>>#rangesForLiteralOf: methodNode

<details>
	<summary>See more</summary>
	
	rangesForLiteralOf: methodNode
	
	| literalRanges |
	
	literalRanges := methodNode positionsForLiteralNode: classToRenameOriginalName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].
	
	^literalRanges 
</details>

#### RenameClass>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	classToRename safeRenameTo: newClassName.
	^self renameReferences.
	
	
</details>

#### RenameClass>>#renameReference: aReferencingMethod

<details>
	<summary>See more</summary>
	
	renameReference: aReferencingMethod 
	
	| newSource |
	
	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource 
</details>

#### RenameClass>>#initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary

<details>
	<summary>See more</summary>
	
	initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

	classToRename := aClass.
	classToRenameOriginalName := aClass name.
	newClassName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary.
	
	
</details>

#### RenameClass>>#referencesOldClassName: aMethodReference

<details>
	<summary>See more</summary>
	
	referencesOldClassName: aMethodReference

	^self references: aMethodReference classVarNamed: classToRenameOriginalName
</details>

#### RenameClass>>#referencesNewClassName: aMethodReference

<details>
	<summary>See more</summary>
	
	referencesNewClassName: aMethodReference

	^self references: aMethodReference classVarNamed: newClassName 
</details>

#### RenameClass>>#renameReferences

<details>
	<summary>See more</summary>
	
	renameReferences
	
	| references |
	
	references := (self referencesToOldClass asSet, self referencesToOldClassName asSet) asOrderedCollection.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	
	^references
</details>

#### RenameClass>>#references: aMethodReference classVarNamed: aName

<details>
	<summary>See more</summary>
	
	references: aMethodReference classVarNamed: aName

	^aMethodReference actualClass definesClassVariableNamedInHierarchy: aName 
</details>

#### RenameClass>>#newClassName

<details>
	<summary>See more</summary>
	
	newClassName
	
	^newClassName 
</details>

#### RenameClass>>#referencesToOldClass

<details>
	<summary>See more</summary>
	
	referencesToOldClass
	
	^system allCallsOn: newClassName
</details>

#### RenameClass>>#newSourceCodeOf: aCompiledMethod

<details>
	<summary>See more</summary>
	
	newSourceCodeOf: aCompiledMethod 
	
	| newSource |
	
	newSource := aCompiledMethod sourceCode copyReplacing: (self rangesToReplaceOf: aCompiledMethod) with: newClassName.

	^newSource
</details>

#### RenameClass>>#rejectReferencesToClassVariablesFrom: references

<details>
	<summary>See more</summary>
	
	rejectReferencesToClassVariablesFrom: references
	
	^references reject: [ :aMethodReference | (self referencesOldClassName: aMethodReference) or: [ self referencesNewClassName: aMethodReference ] ].
</details>

#### RenameClass>>#rangesToReplaceOf: aCompiledMethod

<details>
	<summary>See more</summary>
	
	rangesToReplaceOf: aCompiledMethod 
	
	| methodNode ranges |
	
	methodNode := aCompiledMethod methodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).
	
	^ranges 
</details>

## RenameClassApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameClassApplier>>#openChangedMethods

<details>
	<summary>See more</summary>
	
	openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references' autoSelect: newClassName ]

</details>

#### RenameClassApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^RenameClass from: classToRename to: newClassName in: Smalltalk undeclared: Undeclared.
	

</details>

#### RenameClassApplier>>#initializeFor: aClass

<details>
	<summary>See more</summary>
	
	initializeFor: aClass

	classToRename := aClass.
	
</details>

#### RenameClassApplier>>#askNewClassName

<details>
	<summary>See more</summary>
	
	askNewClassName

	newClassName := self request: 'Enter new name:' initialAnswer: classToRename name.
	newClassName := newClassName withBlanksTrimmed asSymbol.
	
</details>

#### RenameClassApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askNewClassName
</details>

#### RenameClassApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self openChangedMethods

</details>

## RenameGlobal

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameGlobal>>#referencesOldName: aMethodReference

<details>
	<summary>See more</summary>
	
	referencesOldName: aMethodReference

	^self references: aMethodReference classVarNamed: oldName 
</details>

#### RenameGlobal>>#rangesForLiteralVariableOf: methodNode

<details>
	<summary>See more</summary>
	
	rangesForLiteralVariableOf: methodNode
	
	^methodNode positionsForLiteralVariableNode: oldName ifAbsent: [ #() ]
	
</details>

#### RenameGlobal>>#newName

<details>
	<summary>See more</summary>
	
	newName
	
	^newName 
</details>

#### RenameGlobal>>#referencesToOldName

<details>
	<summary>See more</summary>
	
	referencesToOldName
	
	^system allCallsOn: oldName
</details>

#### RenameGlobal>>#rangesForLiteralOf: methodNode

<details>
	<summary>See more</summary>
	
	rangesForLiteralOf: methodNode
	
	| literalRanges |
	
	literalRanges := methodNode positionsForLiteralNode: oldName ifAbsent: [ #() ].
	literalRanges := literalRanges collect: [ :aRange | aRange first + 1 to: aRange last ].
	
	^literalRanges 
</details>

#### RenameGlobal>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	| renamedReferences |
	
	system at: newName put: (system at: oldName).
	renamedReferences := self renameReferences.
	system removeKey: oldName.
	
	^renamedReferences 
	
</details>

#### RenameGlobal>>#renameReference: aReferencingMethod

<details>
	<summary>See more</summary>
	
	renameReference: aReferencingMethod 
	
	| newSource |
	
	newSource := self newSourceCodeOf: aReferencingMethod.
	aReferencingMethod methodClass compile: newSource 
</details>

#### RenameGlobal>>#renameReferences

<details>
	<summary>See more</summary>
	
	renameReferences
	
	| references |
	
	references := self referencesToOldName.
	references := self rejectReferencesToClassVariablesFrom: references.
	references do: [ :aReference | self renameReference: aReference compiledMethod ].
	
	^references
</details>

#### RenameGlobal>>#references: aMethodReference classVarNamed: aName

<details>
	<summary>See more</summary>
	
	references: aMethodReference classVarNamed: aName

	^aMethodReference actualClass theNonMetaClass definesClassVariableNamedInHierarchy: aName 
</details>

#### RenameGlobal>>#newSourceCodeOf: aCompiledMethod

<details>
	<summary>See more</summary>
	
	newSourceCodeOf: aCompiledMethod 
	
	| newSource |
	
	newSource := aCompiledMethod sourceCode copyReplacing: (self rangesToReplaceOf: aCompiledMethod) with: newName.

	^newSource
</details>

#### RenameGlobal>>#rejectReferencesToClassVariablesFrom: references

<details>
	<summary>See more</summary>
	
	rejectReferencesToClassVariablesFrom: references
	
	^references reject: [ :aMethodReference | self referencesOldName: aMethodReference ].
</details>

#### RenameGlobal>>#rangesToReplaceOf: aCompiledMethod

<details>
	<summary>See more</summary>
	
	rangesToReplaceOf: aCompiledMethod 
	
	| methodNode ranges |
	
	methodNode := aCompiledMethod methodNode.
	ranges := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	ranges addAll: (self rangesForLiteralVariableOf: methodNode).
	ranges addAll: (self rangesForLiteralOf: methodNode).
	
	^ranges 
</details>

#### RenameGlobal>>#initializeFrom: anOldName to: aNewName in: aSystem

<details>
	<summary>See more</summary>
	
	initializeFrom: anOldName to: aNewName in: aSystem  

	oldName := anOldName.
	newName := aNewName.
	system := aSystem.
	
	
</details>

## RenameGlobalApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameGlobalApplier>>#openChangedMethods

<details>
	<summary>See more</summary>
	
	openChangedMethods

	changes ifNotEmpty: [ 
		MessageSetWindow openMessageList: changes label: 'Renamed references' autoSelect: newName ]

</details>

#### RenameGlobalApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^RenameGlobal from: oldName to: newName in: Smalltalk 
</details>

#### RenameGlobalApplier>>#askNewName

<details>
	<summary>See more</summary>
	
	askNewName

	newName := self request: 'Enter new name:' initialAnswer: oldName asString.
	newName := newName withBlanksTrimmed asSymbol.
	
</details>

#### RenameGlobalApplier>>#initializeOn: aBrowser for: anOldName

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser for: anOldName

	browser := aBrowser.
	oldName := anOldName.
	
</details>

#### RenameGlobalApplier>>#askOldName

<details>
	<summary>See more</summary>
	
	askOldName

	oldName := self request: 'Enter global name to rename:' initialAnswer: oldName.
	oldName := oldName withBlanksTrimmed asSymbol.
	
</details>

#### RenameGlobalApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	oldName isEmpty ifTrue: [ self askOldName ].
	self askNewName
</details>

#### RenameGlobalApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self openChangedMethods

</details>

## RenameInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameInstanceVariable>>#newSourceOf: aMethodAndRangesToChange

<details>
	<summary>See more</summary>
	
	newSourceOf: aMethodAndRangesToChange 
	
	| newSource ranges methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	ranges := aMethodAndRangesToChange value.
	newSource := methodToChange sourceCode copyReplacing: ranges with: newVariable.
	
	^newSource
	
</details>

#### RenameInstanceVariable>>#renameReferencesToOldVariableInMethod: aMethodAndRangesToChange

<details>
	<summary>See more</summary>
	
	renameReferencesToOldVariableInMethod: aMethodAndRangesToChange 
	
	| methodToChange |
	
	methodToChange := aMethodAndRangesToChange key.
	methodToChange methodClass compile: (self newSourceOf: aMethodAndRangesToChange).
	renamedReferences add: methodToChange methodReference 
</details>

#### RenameInstanceVariable>>#renameReferencesToOldVariable

<details>
	<summary>See more</summary>
	
	renameReferencesToOldVariable
	
	renamedReferences := OrderedCollection new.
	methodsAndRangesToChange do: [ :aMethodAndRangesToChange | self renameReferencesToOldVariableInMethod: aMethodAndRangesToChange ].
	
</details>

#### RenameInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply

	originalClassToRefactor := classToRefactor copy.
	
	self 
		lookForMethodsReferencingOldVariable;
		changeInstanceVariableName;
		logChange;
		renameReferencesToOldVariable.
		
	^renamedReferences 
		
</details>

#### RenameInstanceVariable>>#lookForMethodsReferencingOldVariable

<details>
	<summary>See more</summary>
	
	lookForMethodsReferencingOldVariable
	
	methodsAndRangesToChange := OrderedCollection new.
	classToRefactor withAllSubclassesDo: [ :aClass |  self lookForMethodsReferencingOldVariableIn: aClass ].

</details>

#### RenameInstanceVariable>>#changeInstanceVariableName

<details>
	<summary>See more</summary>
	
	changeInstanceVariableName

	| instanceVariableNames oldVariableIndex |
	
	instanceVariableNames := classToRefactor instVarNames.
	oldVariableIndex := instanceVariableNames indexOf: oldVariable.
	instanceVariableNames at: oldVariableIndex put: newVariable.
</details>

#### RenameInstanceVariable>>#keepMethodToChangeNamed: aSelector in: aClass

<details>
	<summary>See more</summary>
	
	keepMethodToChangeNamed: aSelector in: aClass 

	| methodToChange rangesToChange |
	
	methodToChange := aClass >> aSelector.
	rangesToChange :=  methodToChange methodNode positionsForInstanceVariable: oldVariable ifAbsent: [ #() ].

	methodsAndRangesToChange add: methodToChange -> rangesToChange 
</details>

#### RenameInstanceVariable>>#logChange

<details>
	<summary>See more</summary>
	
	logChange
		
	Smalltalk
		logChange: classToRefactor definition 
		preamble: classToRefactor definitionPreamble.
		
	ChangeSet
		classDefinitionChangedFrom: originalClassToRefactor to: classToRefactor 
</details>

#### RenameInstanceVariable>>#lookForMethodsReferencingOldVariableIn: aClass

<details>
	<summary>See more</summary>
	
	lookForMethodsReferencingOldVariableIn: aClass

	(aClass whichSelectorsAccess: oldVariable) do: [ :aSelector | self keepMethodToChangeNamed: aSelector in: aClass ].
	
</details>

#### RenameInstanceVariable>>#initializeFrom: anOldvariable to: aNewVariable in: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeFrom: anOldvariable to: aNewVariable in: aClassToRefactor 

	oldVariable := anOldvariable.
	newVariable := aNewVariable.
	classToRefactor := aClassToRefactor 
</details>

## RenameInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameInstanceVariableApplier>>#selectVariableLabel

<details>
	<summary>See more</summary>
	
	selectVariableLabel
	
	^'Select instance variable to rename'
</details>

#### RenameInstanceVariableApplier>>#askNewVariableName

<details>
	<summary>See more</summary>
	
	askNewVariableName
		
	newInstanceVariable := self request: 'Enter new name:' initialAnswer: oldInstanceVariable. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed 
</details>

#### RenameInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^RenameInstanceVariable from: oldInstanceVariable to: newInstanceVariable in: classToRefactor.
	
</details>

#### RenameInstanceVariableApplier>>#initializeOn: aBrowserWindow for: anOldInstanceVariable at: aClassToRefactor

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowserWindow for: anOldInstanceVariable at: aClassToRefactor 
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	oldInstanceVariable := anOldInstanceVariable 
</details>

#### RenameInstanceVariableApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self 
		chooseInstanceVariable;
		askNewVariableName
</details>

#### RenameInstanceVariableApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser
</details>

#### RenameInstanceVariableApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser
	
	browser instanceVariableRenamed 
</details>

#### RenameInstanceVariableApplier>>#chooseInstanceVariable

<details>
	<summary>See more</summary>
	
	chooseInstanceVariable
	
	oldInstanceVariable ifNotNil: [ ^self ].
		
	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anOldInstanceVariable | ^oldInstanceVariable := anOldInstanceVariable ].
	self endRequest 
</details>

## RenameSelector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameSelector>>#addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

<details>
	<summary>See more</summary>
	
	addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index) 	
</details>

## RenameSelectorApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameSelectorApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
	
	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	
</details>

#### RenameSelectorApplier>>#refactoringClass

<details>
	<summary>See more</summary>
	
	refactoringClass

	^RenameSelector 
</details>

## RenameTemporary

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameTemporary>>#apply

<details>
	<summary>See more</summary>
	
	apply

	| newSource ranges |

	ranges := methodNode rangeForNode: oldVariableNode ifAbsent: [ #() ].
	newSource := methodNode sourceText copyReplacing: ranges with: newVariable.

	^ newSource
</details>

#### RenameTemporary>>#initializeFromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

<details>
	<summary>See more</summary>
	
	initializeFromOldVariableNode: anOldVariableNode to: aNewVariable in: aMethodNode

	oldVariableNode := anOldVariableNode.
	newVariable := aNewVariable.
	methodNode := aMethodNode 
</details>

#### RenameTemporary>>#methodNodeAfterApply

<details>
	<summary>See more</summary>
	
	methodNodeAfterApply
	
	^methodNode methodClass methodNodeFor: self apply.
	
	
</details>

## RenameTemporaryApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RenameTemporaryApplier>>#askNewVariableName

<details>
	<summary>See more</summary>
	
	askNewVariableName

	newVariable := (self request: 'Enter new name:' initialAnswer: oldVariableNode name) withBlanksTrimmed 
</details>

#### RenameTemporaryApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^RenameTemporary fromOldVariableNode: oldVariableNode to: newVariable in: methodNode
	
</details>

#### RenameTemporaryApplier>>#initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode

<details>
	<summary>See more</summary>
	
	initializeOn: aSmalltalkEditor for: aTemporaryNode at: aMethodNode

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := aMethodNode.
	oldVariableNode := aTemporaryNode.
	
</details>

#### RenameTemporaryApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	self askNewVariableName
</details>

#### RenameTemporaryApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	smalltalkEditor actualContents: changes.
	smalltalkEditor hasUnacceptedEdits ifFalse: [
		smalltalkEditor 
			hasUnacceptedEdits: true;
			acceptContents ]
	
</details>

## SafelyRemoveClass

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SafelyRemoveClass>>#apply

<details>
	<summary>See more</summary>
	
	apply
	
	self removeWithAllSubclasses: classToRemove.
	
</details>

#### SafelyRemoveClass>>#initializeOf: aClassToSafetelyRemove

<details>
	<summary>See more</summary>
	
	initializeOf: aClassToSafetelyRemove 
	
	classToRemove := aClassToSafetelyRemove 
</details>

#### SafelyRemoveClass>>#removeWithAllSubclasses: aClassToRemove

I have to do 'subclasses do:' and not 'subclassesDo:' because removing a class modifies parent's subclasses collection. #subclasses returns a copy of superclass' subclasses collection -Hernan


<details>
	<summary>See more</summary>
	
	removeWithAllSubclasses: aClassToRemove

	"I have to do 'subclasses do:' and not 'subclassesDo:' because removing a class modifies parent's subclasses collection.
	#subclasses returns a copy of superclass' subclasses collection -Hernan"
	aClassToRemove subclasses do: [ :aSubclassToRemove | self removeWithAllSubclasses: aSubclassToRemove ].
	aClassToRemove removeFromSystem.
	
</details>

## SafelyRemoveClassApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SafelyRemoveClassApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring
		
	^SafelyRemoveClass of: classToRemove 
</details>

#### SafelyRemoveClassApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters

	(self confirm: self confirmationMessageText) ifFalse: [ self endRequest ].

	
</details>

#### SafelyRemoveClassApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	self informChangesToBrowser
</details>

#### SafelyRemoveClassApplier>>#informChangesToBrowser

<details>
	<summary>See more</summary>
	
	informChangesToBrowser

	browser classListIndex: 0
</details>

#### SafelyRemoveClassApplier>>#initializeOn: aBrowser of: aClassToRemove

<details>
	<summary>See more</summary>
	
	initializeOn: aBrowser of: aClassToRemove 
	
	browser := aBrowser.
	classToRemove := aClassToRemove 
</details>

#### SafelyRemoveClassApplier>>#confirmationMessageText

<details>
	<summary>See more</summary>
	
	confirmationMessageText
	
	^'Are you sure you want to remove ', classToRemove name asString, '?'
</details>

## TemporaryToInstanceVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TemporaryToInstanceVariable>>#initializeNamed: aTemporaryVariableName fromMethod: aMethodNode

<details>
	<summary>See more</summary>
	
	initializeNamed: aTemporaryVariableName fromMethod: aMethodNode 
	variable _ aTemporaryVariableName.
	method _ aMethodNode.
</details>

#### TemporaryToInstanceVariable>>#apply

<details>
	<summary>See more</summary>
	
	apply

	| newSourceCode |

	newSourceCode _ self removeTemporary.
	self addInstanceVariable.
	
	^newSourceCode.
</details>

#### TemporaryToInstanceVariable>>#removeTemporary

<details>
	<summary>See more</summary>
	
	removeTemporary

	| remover |
	
	remover _ TemporaryVariableDeclarationRemover in: method for: variable. 
	method accept: remover.
	^remover newSourceCode. 
	
</details>

#### TemporaryToInstanceVariable>>#addInstanceVariable

<details>
	<summary>See more</summary>
	
	addInstanceVariable

	AddInstanceVariable named: variable to: method methodClass :: apply.
</details>

## TemporaryToInstanceVariableApplier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TemporaryToInstanceVariableApplier>>#createRefactoring

<details>
	<summary>See more</summary>
	
	createRefactoring

	^TemporaryToInstanceVariable named: variableName fromMethod: methodNode.
</details>

#### TemporaryToInstanceVariableApplier>>#initializeOn: aSmalltalkEditor for: aTemporaryVariableName

<details>
	<summary>See more</summary>
	
	initializeOn: aSmalltalkEditor for: aTemporaryVariableName

	smalltalkEditor := aSmalltalkEditor.
	classToRefactor := smalltalkEditor codeProvider selectedClassOrMetaClass.
	methodNode := classToRefactor methodNodeFor: smalltalkEditor actualContents string.
	variableName := aTemporaryVariableName 
	
</details>

#### TemporaryToInstanceVariableApplier>>#requestRefactoringParameters

<details>
	<summary>See more</summary>
	
	requestRefactoringParameters
	
</details>

#### TemporaryToInstanceVariableApplier>>#showChanges

<details>
	<summary>See more</summary>
	
	showChanges

	smalltalkEditor actualContents: changes.
	
</details>

## TemporaryVariableDeclarationCounter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TemporaryVariableDeclarationCounter>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	count _ 0.
</details>

#### TemporaryVariableDeclarationCounter>>#visitBlockNodeDeclaringTemporary: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNodeDeclaringTemporary: aBlockNode

	count _ count + 1.
</details>

#### TemporaryVariableDeclarationCounter>>#count

<details>
	<summary>See more</summary>
	
	count

	^count.
</details>

#### TemporaryVariableDeclarationCounter>>#visitMethodNodeDeclaringTemporary: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNodeDeclaringTemporary: aMethodNode

	count _ count + 1.
</details>

## TemporaryVariableDeclarationRemover

I remove declarations of a temporary variable from the children of a ParseNode.

### Methods
#### TemporaryVariableDeclarationRemover>>#sourceTextWithoutTemporaryDeclarationLineFrom: firstIndex to: lastIndex

<details>
	<summary>See more</summary>
	
	sourceTextWithoutTemporaryDeclarationLineFrom: firstIndex to: lastIndex

	^methodNode sourceText copyReplaceFrom: firstIndex to: lastIndex with: ' '.
</details>

#### TemporaryVariableDeclarationRemover>>#methodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	methodNode: aMethodNode

	methodNode _ aMethodNode.
</details>

#### TemporaryVariableDeclarationRemover>>#newSourceCode

<details>
	<summary>See more</summary>
	
	newSourceCode

	^newSourceCode 
</details>

#### TemporaryVariableDeclarationRemover>>#visitBlockNodeDeclaringTemporary: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNodeDeclaringTemporary: aBlockNode

	self 
		visitNodeDeclaringTemporary: aBlockNode 
		withTemporaryDeclarationLineRemover: [ 
			self sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode ]
</details>

#### TemporaryVariableDeclarationRemover>>#sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode
	
	| sourceTextUpToEndTemps endTempsMark startTempsMark |
	
	endTempsMark _ aBlockNode tempsMark.
	sourceTextUpToEndTemps _ methodNode sourceText copyFrom: 1 to: endTempsMark - 1.
	startTempsMark _ sourceTextUpToEndTemps findLastOccurrenceOfString: '|' startingAt: 1.
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark.
			
			
</details>

#### TemporaryVariableDeclarationRemover>>#visitNodeDeclaringTemporary: aParseNode 
	withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

<details>
	<summary>See more</summary>
	
	visitNodeDeclaringTemporary: aParseNode 
	withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

	newSourceCode _ self
		sourceTextWithoutTemporaryFromParseNode: aParseNode
		withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock.
		
	methodNode methodClass compile: newSourceCode.
</details>

#### TemporaryVariableDeclarationRemover>>#sourceTextWithoutTemporaryFromParseNode: aParseNode
withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

<details>
	<summary>See more</summary>
	
	sourceTextWithoutTemporaryFromParseNode: aParseNode
withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock
	
	^aParseNode temporaries size = 1 
		ifTrue: aTemporaryDeclarationLineRemovingBlock value
		ifFalse: [ self sourceTextWithoutTemporaryDeclaration ].
</details>

#### TemporaryVariableDeclarationRemover>>#sourceTextWithoutTemporaryDeclaration

<details>
	<summary>See more</summary>
	
	sourceTextWithoutTemporaryDeclaration

	| temporaryVariablePositions variableDeclarationPosition | 

	temporaryVariablePositions _ methodNode positionsForTemporaryVariable: temporaryVariable ifAbsent: [].
	variableDeclarationPosition _ {temporaryVariablePositions first}.
	^methodNode sourceText copyReplacing: variableDeclarationPosition with: ''
</details>

#### TemporaryVariableDeclarationRemover>>#visitMethodNodeDeclaringTemporary: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNodeDeclaringTemporary: aMethodNode
	
	self 
		visitNodeDeclaringTemporary: aMethodNode 
		withTemporaryDeclarationLineRemover: [ self sourceTextWithoutMethodTemporaryDeclarationLine ]
</details>

#### TemporaryVariableDeclarationRemover>>#sourceTextWithoutMethodTemporaryDeclarationLine

<details>
	<summary>See more</summary>
	
	sourceTextWithoutMethodTemporaryDeclarationLine
	
	| endTempsMark startTempsMark |
	
	startTempsMark _ methodNode sourceText indexOf: $|.
	endTempsMark _ methodNode sourceText indexOf: $| startingAt: startTempsMark + 1.
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark.
</details>

