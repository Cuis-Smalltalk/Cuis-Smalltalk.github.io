## BasicInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### BasicInspector>>#inspect: anObject

Initialize the receiver so that it is inspecting anObject. There is no current selection.


<details>
	<summary>See more</summary>
	
	inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no 
	current selection."

	self initialize.
	object _ anObject.
	selectionIndex _ 0.
	acceptedContentsCache _ ''
</details>

## CompiledMethodInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### CompiledMethodInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList

	| keys |
	keys _ OrderedCollection new.
	keys add: 'self'.
	keys add: 'all bytecodes'.
	keys add: 'header'.
	1 to: object numLiterals do: [ :i |
		keys add: 'literal', i printString ].
	object initialPC to: object size do: [ :i |
		keys add: i printString ].
	^ keys asArray
	
</details>

#### CompiledMethodInspector>>#selectionUnmodifiable

Answer if the current selected variable is unmodifiable via acceptance in the code pane. For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable


<details>
	<summary>See more</summary>
	
	selectionUnmodifiable
	"Answer if the current selected variable is unmodifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^ true
</details>

#### CompiledMethodInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection

	| bytecodeIndex |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object ].
	selectionIndex = 2 ifTrue: [^ object symbolic].
	selectionIndex = 3 ifTrue: [^ object headerDescription].
	selectionIndex <= (object numLiterals + 3) 
		ifTrue: [ ^ object objectAt: selectionIndex - 2 ].
	bytecodeIndex _ selectionIndex - object numLiterals - 3.
	^ object at: object initialPC + bytecodeIndex - 1
</details>

#### CompiledMethodInspector>>#contentsIsString

Hacked so contents empty when deselected


<details>
	<summary>See more</summary>
	
	contentsIsString
	"Hacked so contents empty when deselected"

	^ #(0 2 3) includes: selectionIndex
</details>

## ContextInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ContextInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	"Answer the base field list plus an abbreviated list of indices."

	^ self baseFieldList , (object tempNames collect:[:t| '[',t,']'])
</details>

#### ContextInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object longPrintStringLimitedTo: 20000].
	(selectionIndex - 2) <= object class instSize
		ifTrue: [^ object instVarAt: selectionIndex - 2].
	basicIndex := selectionIndex - 2 - object class instSize.
	^object debuggerMap namedTempAt: basicIndex in: object

</details>

## DictionaryInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DictionaryInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	^ self baseFieldList
		, (keyArray collect: [:key | key printString])
</details>

#### DictionaryInspector>>#numberOfFixedFields

<details>
	<summary>See more</summary>
	
	numberOfFixedFields
	^ 2 + object class instSize
</details>

#### DictionaryInspector>>#selectedKey

Create a browser on all senders of the selected key


<details>
	<summary>See more</summary>
	
	selectedKey
	"Create a browser on all senders of the selected key"
	| i |
	i _ selectionIndex  - self numberOfFixedFields.
	i > 0 ifFalse: [ ^ nil ].
	^keyArray at: i
</details>

#### DictionaryInspector>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	self calculateKeyArray
</details>

#### DictionaryInspector>>#contentsIsString

Hacked so contents empty when deselected


<details>
	<summary>See more</summary>
	
	contentsIsString
	"Hacked so contents empty when deselected"

	^ (selectionIndex = 0)
</details>

#### DictionaryInspector>>#addEntry: aKey

<details>
	<summary>See more</summary>
	
	addEntry: aKey
	object at: aKey put: nil.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: aKey).
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList.
	self update
</details>

#### DictionaryInspector>>#removeSelection

<details>
	<summary>See more</summary>
	
	removeSelection
	selectionIndex = 0 ifTrue: [^ self changed: #flash].
	object removeKey: (keyArray at: selectionIndex - self numberOfFixedFields).
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self calculateKeyArray.
	self changed: #inspectObject.
	self changed: #selectionIndex.
	self changed: #fieldList
</details>

#### DictionaryInspector>>#refreshView

<details>
	<summary>See more</summary>
	
	refreshView
	| i |
	i := selectionIndex.
	self calculateKeyArray.
	selectionIndex := i.
	self changed: #fieldList.
	self acceptedContentsChanged.
</details>

#### DictionaryInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection

	selectionIndex <= (self numberOfFixedFields) ifTrue: [^ super selection].
	^ object at: (keyArray at: selectionIndex - self numberOfFixedFields) ifAbsent: nil
</details>

#### DictionaryInspector>>#calculateKeyArray

Recalculate the KeyArray from the object being inspected


<details>
	<summary>See more</summary>
	
	calculateKeyArray
	"Recalculate the KeyArray from the object being inspected"

	keyArray _ object keysSortedSafely asArray.
	selectionIndex _ 0.

</details>

#### DictionaryInspector>>#renameEntryTo: newKey

<details>
	<summary>See more</summary>
	
	renameEntryTo: newKey
	| value |

	value := object at: (keyArray at: selectionIndex - self numberOfFixedFields).
	object removeKey: (keyArray at: selectionIndex - self numberOfFixedFields).
	object at: newKey put: value.
	self calculateKeyArray.
	selectionIndex := self numberOfFixedFields + (keyArray indexOf: newKey).
	self changed: #selectionIndex.
	self changed: #inspectObject.
	self changed: #fieldList.
	self update
</details>

#### DictionaryInspector>>#replaceSelectionValue: anObject

The receiver has a list of variables of its inspected object. One of these is selected. The value of the selected variable is set to the value, anObject.


<details>
	<summary>See more</summary>
	
	replaceSelectionValue: anObject 
	selectionIndex <= self numberOfFixedFields
		ifTrue: [^ super replaceSelectionValue: anObject].
	^ object
		at: (keyArray at: selectionIndex - self numberOfFixedFields)
		put: anObject
</details>

## Inspector

I represent a query path into the internal representation of an object. As a TextProvider, the string I represent is the value of the currently selected variable of the observed object.

### Methods
#### Inspector>>#i2

This is the number of elements to show at the end of very long arrays


<details>
	<summary>See more</summary>
	
	i2
	"This is the number of elements to show at the end
	of very long arrays"
	^ 30
</details>

#### Inspector>>#bindingNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingNamesDo: aBlock
	object class allInstVarNames do: aBlock
</details>

#### Inspector>>#object

Answer the object being inspected by the receiver.


<details>
	<summary>See more</summary>
	
	object
	"Answer the object being inspected by the receiver."

	^object
</details>

#### Inspector>>#object: anObject

Set anObject to be the object being inspected by the receiver.


<details>
	<summary>See more</summary>
	
	object: anObject 
	"Set anObject to be the object being inspected by the receiver."

	| oldIndex |
	anObject == object
		ifTrue: [self update]
		ifFalse:
			[oldIndex := selectionIndex <= 2 ifTrue: [selectionIndex] ifFalse: [0].
			self inspect: anObject.
			oldIndex := oldIndex min: self fieldList size.
			self changed: #inspectObject.
			oldIndex > 0
				ifTrue: [self toggleIndex: oldIndex].
			self changed: #fieldList.
			self acceptedContentsChanged ]
</details>

#### Inspector>>#update

Reshow contents, assuming selected value may have changed.


<details>
	<summary>See more</summary>
	
	update
	"Reshow contents, assuming selected value may have changed."

	selectionIndex = 0
		ifFalse: [
			self contentsIsString
				ifTrue: [ acceptedContentsCache _ self selection]
				ifFalse: [ acceptedContentsCache _ self selectionPrintString].
			self acceptedContentsChanged.
			self changed: #selectionIndex ]
</details>

#### Inspector>>#stepAt: millisecondSinceLast

See comment at #wantsSteps


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	| newText |
	newText := self contentsIsString
		ifTrue: [self selection]
		ifFalse: ["keep it short to reduce time to compute it"
			self selectionPrintString ].
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]
</details>

#### Inspector>>#selectionPrintString

<details>
	<summary>See more</summary>
	
	selectionPrintString

	^[self selection printTextLimitedTo: 12000]
		on: UnhandledError
		do:
			[:ex |
				ex return:
					(self printStringErrorText
						addAttribute: TextColor red;
						yourself)
			]
</details>

#### Inspector>>#textStylerClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	textStylerClassFor: textGetter

	^SHTextStylerST80
</details>

#### Inspector>>#printStringErrorText

<details>
	<summary>See more</summary>
	
	printStringErrorText
	| nm |
	nm := self selectionIndex < 3
					ifTrue: ['self']
					ifFalse: [self selectedSlotName ifNil: ['??']].
	^ ('<error in printString: evaluate "' , nm , ' printString" to debug>') asText.
</details>

#### Inspector>>#baseFieldList

Answer an Array consisting of 'self' and the instance variable names of the inspected object.


<details>
	<summary>See more</summary>
	
	baseFieldList
	"Answer an Array consisting of 'self'
	and the instance variable names of the inspected object."

	^ (Array with: 'self' with: 'all inst vars')
			, object class allInstVarNames
</details>

#### Inspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	"Answer the base field list plus an abbreviated list of indices."

	object class isVariable ifFalse: [^ self baseFieldList].
	^ self baseFieldList ,
		(object basicSize <= (self i1 + self i2)
			ifTrue: [(1 to: object basicSize)
						collect: [:i | i printString]]
			ifFalse: [(1 to: self i1) , (object basicSize-(self i2-1) to: object basicSize)
						collect: [:i | i printString]])
</details>

#### Inspector>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForClass: (object instVarNamed: aName) class 
</details>

#### Inspector>>#selectedSlotName

<details>
	<summary>See more</summary>
	
	selectedSlotName

	^ self fieldList at: self selectionIndex ifAbsent: nil
</details>

#### Inspector>>#inspectSelection

<details>
	<summary>See more</summary>
	
	inspectSelection
	self selection inspect
</details>

#### Inspector>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do:  aParsingErrorBlock 
</details>

#### Inspector>>#inspect: anObject

Initialize the receiver so that it is inspecting anObject. There is no current selection.


<details>
	<summary>See more</summary>
	
	inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no current selection."
	
	object := anObject. 
	self initialize
</details>

#### Inspector>>#context: ctxt

Set the context of inspection. Currently only used by my subclass ClosureEnvInspector. The inst var is here because we do primitiveChangeClassTo: between subclasses (see inspect:) between different subclasses, but also context could be used as a general concept in all inspectors


<details>
	<summary>See more</summary>
	
	context: ctxt
	"Set the context of inspection. Currently only used by my subclass ClosureEnvInspector. The inst var is here because we do primitiveChangeClassTo: between subclasses (see inspect:) between different subclasses, but also context could be used as a general concept in all inspectors"

	context := ctxt
</details>

#### Inspector>>#selectedClass

Answer the class of the receiver's current selection


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class of the receiver's current selection"

	self selectionUnmodifiable ifTrue: [^ object class].
	^ self selection class
</details>

#### Inspector>>#wantsSteps

Overridden by morphic classes whose instances want to be stepped all the time, or by model classes who want their morphic views to be stepped all the time. Some classes might answer false to this message, and call #startStepping #startSteppingStepTime: #stopStepping as appropriate


<details>
	<summary>See more</summary>
	
	wantsSteps
	^ true
</details>

#### Inspector>>#selectionIndex

The receiver has a list of variables of its inspected object. One of these is selected. Answer the index into the list of the selected variable.


<details>
	<summary>See more</summary>
	
	selectionIndex
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. Answer the index into the list of the selected variable."

	^selectionIndex
</details>

#### Inspector>>#selectionUnmodifiable

Answer if the current selected variable is modifiable via acceptance in the code pane. For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable


<details>
	<summary>See more</summary>
	
	selectionUnmodifiable
	"Answer if the current selected variable is modifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^ selectionIndex <= 2
</details>

#### Inspector>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	acceptedContentsCache _ ''.
	selectionIndex := 0
</details>

#### Inspector>>#toggleIndex: anInteger

The receiver has a list of variables of its inspected object. One of these is selected. If anInteger is the index of this variable, then deselect it. Otherwise, make the variable whose index is anInteger be the selected item.


<details>
	<summary>See more</summary>
	
	toggleIndex: anInteger
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. If anInteger is the index of this variable, then deselect it. 
	Otherwise, make the variable whose index is anInteger be the selected 
	item."

	selectionIndex = anInteger
		ifTrue: [
			"same index, turn off selection"
			selectionIndex _ 0.
			acceptedContentsCache _ '']
		ifFalse: [
			"different index, new selection"
			selectionIndex _ anInteger.
			self contentsIsString
				ifTrue: [ acceptedContentsCache _ self selection]
				ifFalse: [ acceptedContentsCache _ self selectionPrintString]].

	self acceptedContentsChanged.
	self changed: #selectionIndex
</details>

#### Inspector>>#doItReceiver

Answer the object that should be informed of the result of evaluating a text selection.


<details>
	<summary>See more</summary>
	
	doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	^object
</details>

#### Inspector>>#hasBindingOf: aString

<details>
	<summary>See more</summary>
	
	hasBindingOf: aString
	^ object class allInstVarNames includes: aString
</details>

#### Inspector>>#contentsIsString

Hacked so contents empty when deselected and = long printString when item 2


<details>
	<summary>See more</summary>
	
	contentsIsString
	"Hacked so contents empty when deselected and = long printString when item 2"

	^ (selectionIndex = 2) | (selectionIndex = 0)
</details>

#### Inspector>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
</details>

#### Inspector>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer true to allow styling to proceed, or false to veto the styling"

	anSHTextStyler workspace: self.
	^(text = self acceptedContents) not
</details>

#### Inspector>>#i1

This is the max index shown before skipping to the last i2 elements of very long arrays


<details>
	<summary>See more</summary>
	
	i1
	"This is the max index shown before skipping to the 
	last i2 elements of very long arrays"
	^ 500
</details>

#### Inspector>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	^SmalltalkEditor
</details>

#### Inspector>>#acceptedStringOrText

We need our cache not to be modified by user changes


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"We need our cache not to be modified by user changes"
	^acceptedContentsCache copy
</details>

#### Inspector>>#doItContext

Answer the context in which a text selection can be evaluated.


<details>
	<summary>See more</summary>
	
	doItContext
	"Answer the context in which a text selection can be evaluated."

	^nil
</details>

#### Inspector>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	^SmalltalkCompleter
</details>

#### Inspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex index |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object longPrintStringLimitedTo: 20000].
	(selectionIndex - 2) <= object class instSize
		ifTrue: [^ object instVarAt: selectionIndex - 2].
	basicIndex _ selectionIndex - 2 - object class instSize.
	index _ (object basicSize <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [ basicIndex ]
		ifFalse: [ object basicSize - (self i1 + self i2) + basicIndex ].
	^object isString
		ifTrue: [ object at: index ]
		ifFalse: [ object basicAt: index ]
</details>

#### Inspector>>#accept: aString

<details>
	<summary>See more</summary>
	
	accept: aString

	| result |
	result _ self doItReceiver class compilerClass new
				evaluate: (ReadStream on: aString)
				in: self doItContext
				to: self doItReceiver
				notifying: nil	"fix this"
				ifFail:  [^ false].
	acceptedContentsCache _ result printString.
	self replaceSelectionValue: result.	"may put contents back"
	self acceptedContentsChanged.
	^ true
</details>

#### Inspector>>#replaceSelectionValue: anObject

The receiver has a list of variables of its inspected object. One of these is selected. The value of the selected variable is set to the value, anObject.


<details>
	<summary>See more</summary>
	
	replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| basicIndex si instVarIndex |
	selectionIndex <= 2 ifTrue: [
		self toggleIndex: (si := selectionIndex).  
		self toggleIndex: si.
		^ object].
	instVarIndex := selectionIndex - 2.
	instVarIndex > object class instSize
		ifFalse: [^ object instVarAt: instVarIndex put: anObject].
	object class isVariable or: [self error: 'Cannot replace selection'].
	basicIndex := selectionIndex - 2 - object class instSize.
	(object basicSize <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [^object basicAt: basicIndex put: anObject]
		ifFalse: [^object basicAt: object basicSize - (self i1 + self i2) + basicIndex
					put: anObject]
</details>

#### Inspector>>#selectedClassOrMetaClass

<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass

	^ self selectedClass	"I don't know any better"
</details>

## OrderedCollectionInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### OrderedCollectionInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList

	| fieldsHere |
	object isNil ifTrue: [^OrderedCollection new].
	fieldsHere _
		[
			(object size <= (self i1 + self i2)
				ifTrue: [(1 to: object size) collect: [:i | i printString]]
				ifFalse: [(1 to: self i1) , (object size-(self i2-1) to: object size) collect: [:i | i printString]])
		] on: Error do: [:ex | ex return: OrderedCollection new].	
	^self baseFieldList , fieldsHere
"
OrderedCollection new inspect
(OrderedCollection newFrom: #(3 5 7 123)) inspect
(OrderedCollection newFrom: (1 to: 1000)) inspect
"
</details>

#### OrderedCollectionInspector>>#selectedObjectIndex

Answer the index of the inspectee's collection that the current selection refers to.


<details>
	<summary>See more</summary>
	
	selectedObjectIndex
	"Answer the index of the inspectee's collection that the current selection refers to."

	| basicIndex |
	basicIndex _ selectionIndex - 2 - object class instSize.
	^ (object size <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [basicIndex]
		ifFalse: [object size - (self i1 + self i2) + basicIndex]
</details>

#### OrderedCollectionInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."

	(selectionIndex - 2) <= object class instSize
		ifTrue: [^ super selection].
	^ object at: self selectedObjectIndex
</details>

#### OrderedCollectionInspector>>#replaceSelectionValue: anObject

The receiver has a list of variables of its inspected object. One of these is selected. The value of the selected variable is set to the value, anObject.


<details>
	<summary>See more</summary>
	
	replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, anObject."

	(selectionIndex - 2) <= object class instSize
		ifTrue: [^ super replaceSelectionValue: anObject].
	object at: self selectedObjectIndex put: anObject
</details>

## SequenceableCollectionInspector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SequenceableCollectionInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	^self baseFieldList,
		(1 to: object size)
</details>

#### SequenceableCollectionInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex index |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object longPrintStringLimitedTo: 20000].
	basicIndex _ selectionIndex - 2.
	index _ (object size <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [ basicIndex ]
		ifFalse: [ object size - (self i1 + self i2) + basicIndex ].
	^object at: index
</details>

## SetInspector

A verison of the Inspector specialized for inspecting Sets. It displays the elements of the set like elements of an array. Note that the indices, being phyical locations in the hash table, are not meaningful outside of the set.

### Methods
#### SetInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	
	(object isNil or: [ object array isNil]) ifTrue: [^ Set new].
	
	^ self baseFieldList, (object array withIndexCollect: [:each :i | each ifNotNil: [i printString]]) select: [:each | each notNil]
</details>

#### SetInspector>>#removeSelection

<details>
	<summary>See more</summary>
	
	removeSelection
	(selectionIndex <= object class instSize) ifTrue: [^ self changed: #flash].
	object remove: self selection.
	selectionIndex := 0.
	acceptedContentsCache _ ''.
	self changed: #inspectObject.
	self changed: #fieldList.
	self changed: #selectionIndex.
</details>

#### SetInspector>>#selection

The receiver has a list of variables of its inspected object. One of these is selected. Answer the value of the selected variable.


<details>
	<summary>See more</summary>
	
	selection
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object longPrintString].
	(selectionIndex - 2) <= object class instSize
		ifTrue: [^ object instVarAt: selectionIndex - 2].

	^ object array at: self arrayIndexForSelection
</details>

#### SetInspector>>#replaceSelectionValue: anObject

The receiver has a list of variables of its inspected object. One of these is selected. The value of the selected variable is set to the value, anObject.


<details>
	<summary>See more</summary>
	
	replaceSelectionValue: anObject
	^ object array at: self arrayIndexForSelection put: anObject
</details>

#### SetInspector>>#arrayIndexForSelection

<details>
	<summary>See more</summary>
	
	arrayIndexForSelection
	^ (self fieldList at: selectionIndex) asInteger
</details>

## WeakSetInspector

A verison of the SetInspector specialized for inspecting WeakSets. It knows about the flag object used to indicate empty locations in the hash table.

### Methods
#### WeakSetInspector>>#fieldList

Answer the base field list plus an abbreviated list of indices.


<details>
	<summary>See more</summary>
	
	fieldList
	| slotIndices |
	object ifNil: [^ Set new].
	
	"Implementation note: do not use objectArray withIndexCollect: as super
	because this might collect indices in a WeakArray, leading to constantly changing fieldList
	as explained at http://bugs.squeak.org/view.php?id=6812"
	
	slotIndices := (Array new: object size) writeStream.
	object array withIndexDo: [:each :i |
		(each notNil and: [each ~= flagObject]) ifTrue: [slotIndices nextPut: i printString]].
	
	^ self baseFieldList
		, slotIndices contents
</details>

#### WeakSetInspector>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	flagObject := object instVarNamed: 'flag'. 
</details>

