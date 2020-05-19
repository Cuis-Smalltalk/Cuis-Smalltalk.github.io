## BlockLocalTempCounter

I am a support class for the decompiler that is used to find the number of local temps in a block by finding out what the stack offset is at the end of a block.

### Methods
#### BlockLocalTempCounter>>#testTempCountForBlockAt: startPc in: method

Compute the number of local temporaries in a block. If the block begins with a sequence of push: nil bytecodes then some of These could be initializing local temps. We can only reliably disambuguate them from other uses of nil by parsing the stack and seeing what the offset of the stack pointer is at the end of the block.There are short-cuts. The only one we take here is - if there is no sequence of push nils there can be no local temps


<details>
	<summary>See more</summary>
	
	testTempCountForBlockAt: startPc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.There are short-cuts.  The only
	 one we take here is
		- if there is no sequence of push nils there can be no local temps"

	| symbolicLines line prior thePc |
	symbolicLines := Dictionary new.
	method symbolicLinesDo:
		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: startPc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[line := symbolicLines at: scanner pc.
			 prior := stackPointer.
			 thePc := scanner pc.
			 scanner interpretNextInstructionFor: self.
			 Transcript newLine; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line]].
	^stackPointer
</details>

#### BlockLocalTempCounter>>#methodReturnConstant: value

Return Constant bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnConstant: value 
	"Return Constant bytecode."
	self doJoin
</details>

#### BlockLocalTempCounter>>#pushReceiverVariable: offset

Push Contents Of the Receiver's Instance Variable Whose Index is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiverVariable: offset
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#pushConsArrayWithElements: numElements

Push Cons Array of size numElements popping numElements items from the stack into the array bytecode.


<details>
	<summary>See more</summary>
	
	pushConsArrayWithElements: numElements
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."
	stackPointer := stackPointer - numElements + 1
</details>

#### BlockLocalTempCounter>>#doPop

Remove Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doPop
	"Remove Top Of Stack bytecode."
	stackPointer := stackPointer - 1
</details>

#### BlockLocalTempCounter>>#popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Remove Top Of Stack And Store Into Offset of Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."
	stackPointer := stackPointer - 1
</details>

#### BlockLocalTempCounter>>#jump: offset

Unconditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset
	"Unconditional Jump bytecode."
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer.
		 self doJoin]
</details>

#### BlockLocalTempCounter>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Push Closure bytecode. Either compute the end of the block if this is the block we're analysing, or skip it, adjusting the stack as appropriate.


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Push Closure bytecode.  Either compute the end of the block if this is
	 the block we're analysing, or skip it, adjusting the stack as appropriate."
	blockEnd
		ifNil: [blockEnd := scanner pc + blockSize]
		ifNotNil:
			[stackPointer := stackPointer - numCopied + 1.
			 scanner pc: scanner pc + blockSize]
</details>

#### BlockLocalTempCounter>>#pushActiveContext

Push Active Context On Top Of Its Own Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushActiveContext
	"Push Active Context On Top Of Its Own Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#pushNewArrayOfSize: numElements

Push New Array of size numElements bytecode.


<details>
	<summary>See more</summary>
	
	pushNewArrayOfSize: numElements 
	"Push New Array of size numElements bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#jump: offset if: condition

Conditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset if: condition 
	"Conditional Jump bytecode."
	stackPointer := stackPointer - 1.
	offset > 0 ifTrue:
		[joinOffsets at: scanner pc + offset put: stackPointer]
</details>

#### BlockLocalTempCounter>>#pushReceiver

Push Active Context's Receiver on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiver
	"Push Active Context's Receiver on Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#doJoin

<details>
	<summary>See more</summary>
	
	doJoin
	scanner pc < blockEnd ifTrue:
		[stackPointer := joinOffsets at: scanner pc ifAbsent: [scanner followingPc]]

	"the ifAbsent: handles a caseOf:otherwise: where all cases return, which results
	 in the branch around the otherwise being unreached.  e.g. in the following
		jumpTo: L2
	 is unreached.

		| t |
		t caseOf: { [nil] -> [^thisContext method abstractSymbolic] }
		  otherwise: ['Oh no Mr Bill!']

		pushTemp: 0
		pushConstant: nil
		send: #= (1 arg)
		jumpFalseTo: L1
		pushThisContext: 
		send: #method (0 args)
		send: #abstractSymbolic (0 args)
		returnTop
		jumpTo: L2
	L1:
		pushConstant: 'Oh no Mr Bill!'
	L2:
		returnTop"
</details>

#### BlockLocalTempCounter>>#doDup

Duplicate Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doDup
	"Duplicate Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#popIntoTemporaryVariable: offset

Remove Top Of Stack And Store Into Temporary Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoTemporaryVariable: offset 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."
	stackPointer := stackPointer - 1
</details>

#### BlockLocalTempCounter>>#tempCountForBlockAt: pc in: method

Compute the number of local temporaries in a block. If the block begins with a sequence of push: nil bytecodes then some of These could be initializing local temps. We can only reliably disambuguate them from other uses of nil by parsing the stack and seeing what the offset of the stack pointer is at the end of the block. There are short-cuts. The ones we take here are - if there is no sequence of push nils there can be no local temps - we follow forward jumps to shorten the amount of scanning


<details>
	<summary>See more</summary>
	
	tempCountForBlockAt: pc in: method
	"Compute the number of local temporaries in a block.
	 If the block begins with a sequence of push: nil bytecodes then some of
	 These could be initializing local temps.  We can only reliably disambuguate
	 them from other uses of nil by parsing the stack and seeing what the offset
	 of the stack pointer is at the end of the block.

	 There are short-cuts.  The ones we take here are
		- if there is no sequence of push nils there can be no local temps
		- we follow forward jumps to shorten the amount of scanning"
	stackPointer := 0.
	scanner := InstructionStream new method: method pc: pc.
	scanner interpretNextInstructionFor: self.
	blockEnd ifNil:
		[self error: 'pc is not that of a block'].
	scanner nextByte = Encoder pushNilCode ifTrue:
		[joinOffsets := Dictionary new.
		 [scanner pc < blockEnd] whileTrue:
			[scanner interpretNextInstructionFor: self]].
	^stackPointer
</details>

#### BlockLocalTempCounter>>#popIntoLiteralVariable: anAssociation

Remove Top Of Stack And Store Into Literal Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoLiteralVariable: anAssociation 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."
	stackPointer := stackPointer - 1
</details>

#### BlockLocalTempCounter>>#send: selector super: supered numArgs: numberArguments

Send Message With Selector, selector, bytecode. The argument, supered, indicates whether the receiver of the message is specified with 'super' in the source method. The arguments of the message are found in the top numArguments locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: supered numArgs: numberArguments
	"Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	stackPointer := stackPointer - numberArguments
</details>

#### BlockLocalTempCounter>>#pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Push Contents at Offset in Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Push Contents at Offset in Temp Vector bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#pushLiteralVariable: anAssociation

Push Contents Of anAssociation On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushLiteralVariable: anAssociation
	"Push Contents Of anAssociation On Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#blockReturnTop

Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	blockReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	scanner pc < blockEnd ifTrue:
		[self doJoin]
</details>

#### BlockLocalTempCounter>>#pushConstant: value

Push Constant, value, on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushConstant: value
	"Push Constant, value, on Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#methodReturnReceiver

Return Self bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnReceiver
	"Return Self bytecode."
	self doJoin
</details>

#### BlockLocalTempCounter>>#popIntoReceiverVariable: offset

Remove Top Of Stack And Store Into Instance Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoReceiverVariable: offset 
	"Remove Top Of Stack And Store Into Instance Variable bytecode."
	stackPointer := stackPointer - 1
</details>

#### BlockLocalTempCounter>>#pushTemporaryVariable: offset

Push Contents Of Temporary Variable Whose Index Is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushTemporaryVariable: offset
	"Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."
	stackPointer := stackPointer + 1
</details>

#### BlockLocalTempCounter>>#methodReturnTop

Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnTop
	"Return Top Of Stack bytecode."
	stackPointer := stackPointer - 1.
	self doJoin
</details>

## ClassDefinitionNodeAnalyzer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ClassDefinitionNodeAnalyzer>>#isAtClassNameInClassDefinition: anIndex

<details>
	<summary>See more</summary>
	
	isAtClassNameInClassDefinition: anIndex

	^(classDefinitionNode rangeForNode: classCreationMessageNode arguments first ifAbsent: [ ^ false ]) first includes: anIndex
</details>

#### ClassDefinitionNodeAnalyzer>>#isAtSuperclass: anIndex

<details>
	<summary>See more</summary>
	
	isAtSuperclass: anIndex

	^self isClassDefinition and: [ self isAtSuperclassInClassDefinition: anIndex ]
</details>

#### ClassDefinitionNodeAnalyzer>>#is: anIndex atStringParameterNumber: aParameterPosition

<details>
	<summary>See more</summary>
	
	is: anIndex atStringParameterNumber: aParameterPosition

	| parameterRange |
	
	parameterRange := (classDefinitionNode rangeForNode: (classCreationMessageNode arguments at: aParameterPosition) ifAbsent: [ ^ false ]) first.
	
	^anIndex between: parameterRange first + 1 and: parameterRange last - 1
</details>

#### ClassDefinitionNodeAnalyzer>>#isAtSuperclassInClassDefinition: anIndex

<details>
	<summary>See more</summary>
	
	isAtSuperclassInClassDefinition: anIndex

	^(classDefinitionNode rangeForNode: superClassNode ifAbsent: [ ^ false ]) first includes: anIndex 
</details>

#### ClassDefinitionNodeAnalyzer>>#isAtCategory: anIndex

<details>
	<summary>See more</summary>
	
	isAtCategory: anIndex

	^self isClassDefinition and: [ self is: anIndex atStringParameterNumber: self class categoryPosition ]
	
</details>

#### ClassDefinitionNodeAnalyzer>>#superclass

<details>
	<summary>See more</summary>
	
	superclass
	
	^superClassNode key value 
</details>

#### ClassDefinitionNodeAnalyzer>>#initializeFor: aClassDefinitionMethodNode

<details>
	<summary>See more</summary>
	
	initializeFor: aClassDefinitionMethodNode 

	classDefinitionNode := aClassDefinitionMethodNode.
	classCreationMessageNode := classDefinitionNode block statements first expr.
	superClassNode := classCreationMessageNode receiver.

</details>

#### ClassDefinitionNodeAnalyzer>>#instanceVariableNamesPosition

<details>
	<summary>See more</summary>
	
	instanceVariableNamesPosition
	
	^self isClassDefinition ifTrue: [ self class instanceVariableNamesPositionForClassDefinition ] ifFalse: [ self class instanceVariableNamesPositionForMetaclassDefinition ]
</details>

#### ClassDefinitionNodeAnalyzer>>#isClassDefinition

<details>
	<summary>See more</summary>
	
	isClassDefinition
	
	^classDefinitionNode encoder classEncoding isMeta not
</details>

#### ClassDefinitionNodeAnalyzer>>#isAtInstanceVariables: anIndex

<details>
	<summary>See more</summary>
	
	isAtInstanceVariables: anIndex

	^self is: anIndex atStringParameterNumber: self instanceVariableNamesPosition
</details>

#### ClassDefinitionNodeAnalyzer>>#isAtClassName: anIndex

<details>
	<summary>See more</summary>
	
	isAtClassName: anIndex

	^self isClassDefinition and: [ self isAtClassNameInClassDefinition: anIndex ]
	
</details>

## CompiledMethodWithNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### CompiledMethodWithNode>>#method

<details>
	<summary>See more</summary>
	
	method
	^ method
</details>

#### CompiledMethodWithNode>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^ self node selector
</details>

#### CompiledMethodWithNode>>#node

<details>
	<summary>See more</summary>
	
	node
	^ node
</details>

#### CompiledMethodWithNode>>#method: aCompiledMethod

<details>
	<summary>See more</summary>
	
	method: aCompiledMethod
	method := aCompiledMethod
</details>

#### CompiledMethodWithNode>>#node: aMethodNode

<details>
	<summary>See more</summary>
	
	node: aMethodNode
	node := aMethodNode
</details>

## DecompilerConstructor

I construct the node tree for a Decompiler.

### Methods
#### DecompilerConstructor>>#codeConstants

Answer with an array of the objects representing self, true, false, nil, -1, 0, 1, 2.


<details>
	<summary>See more</summary>
	
	codeConstants
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])
</details>

#### DecompilerConstructor>>#codeRemoteTemp: index remoteTemps: tempVector

<details>
	<summary>See more</summary>
	
	codeRemoteTemp: index remoteTemps: tempVector

	^(RemoteTempVectorNode new
		name: 'remoteVar', index printString
		index: index
		type: LdTempType
		scope: 0)
			remoteTemps: tempVector;
			yourself
</details>

#### DecompilerConstructor>>#method: aMethod class: aClass literals: literals

<details>
	<summary>See more</summary>
	
	method: aMethod class: aClass literals: literals

	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals
</details>

#### DecompilerConstructor>>#codeCascadedMessage: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	codeCascadedMessage: selector arguments: arguments

	^self
		codeMessage: nil
		selector: selector
		arguments: arguments
</details>

#### DecompilerConstructor>>#decodeIfNilWithReceiver: receiver selector: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	decodeIfNilWithReceiver: receiver selector: selector arguments: arguments
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
	^ (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3)
		noteSpecialSelector: #ifNil:ifNotNil:
</details>

#### DecompilerConstructor>>#codeMessage: receiver selector: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	codeMessage: receiver selector: selector arguments: arguments
	| symbol |
	symbol := selector key.
	(BraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	(self
		decodeLiteralVariableValueDereferenceWithReceiver: receiver
		selector: symbol
		arguments: arguments) ifNotNil: [:node| ^node].
	(self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [:node| ^node].
	^MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence
</details>

#### DecompilerConstructor>>#codeCascade: receiver messages: messages

<details>
	<summary>See more</summary>
	
	codeCascade: receiver messages: messages

	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [CascadeNode new receiver: receiver messages: messages]
</details>

#### DecompilerConstructor>>#codeArguments: args block: block

<details>
	<summary>See more</summary>
	
	codeArguments: args block: block

	^block arguments: args
</details>

#### DecompilerConstructor>>#codeAnyLitInd: association

<details>
	<summary>See more</summary>
	
	codeAnyLitInd: association

	^VariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType
</details>

#### DecompilerConstructor>>#codeInst: index

<details>
	<summary>See more</summary>
	
	codeInst: index

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1
</details>

#### DecompilerConstructor>>#codeSuper

<details>
	<summary>See more</summary>
	
	codeSuper

	^NodeSuper
</details>

#### DecompilerConstructor>>#codeEmptyBlock

<details>
	<summary>See more</summary>
	
	codeEmptyBlock
	^ BlockNode withJust: NodeNil
</details>

#### DecompilerConstructor>>#codeAnySelector: selector

<details>
	<summary>See more</summary>
	
	codeAnySelector: selector

	^SelectorNode new
		key: selector
		index: 0
		type: SendType
</details>

#### DecompilerConstructor>>#codeAnyLiteral: value

<details>
	<summary>See more</summary>
	
	codeAnyLiteral: value

	^LiteralNode new
		key: value
		index: 0
		type: LdLitType
</details>

#### DecompilerConstructor>>#codeTemp: index named: tempName

<details>
	<summary>See more</summary>
	
	codeTemp: index named: tempName

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0
</details>

#### DecompilerConstructor>>#codeSelector: sel code: code

<details>
	<summary>See more</summary>
	
	codeSelector: sel code: code

	^SelectorNode new key: sel code: code
</details>

#### DecompilerConstructor>>#codeMethod: selector block: block tempVars: vars primitive: primitive class: class

<details>
	<summary>See more</summary>
	
	codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries properties |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	properties := method properties copy.
	(properties at: #onceCache ifAbsent: []) ifNotNil:
		[:onceCache|
		 properties := properties copyWithout: (Association
													key: #onceCache
													value: onceCache)].
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (method encoderClass new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: properties.
	blockNode properties method: blockNode.
	^blockNode
</details>

#### DecompilerConstructor>>#decodeLiteralVariableValueDereferenceWithReceiver: receiver selector: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	decodeLiteralVariableValueDereferenceWithReceiver: receiver selector: selector arguments: arguments
	| varNode |
	(receiver notNil "cascades"
	 and: [receiver isLiteralNode
	 and: [receiver key isVariableBinding]]) ifFalse:
		[^nil].
	varNode := self codeAnyLitInd: receiver key.
	selector = #value ifTrue:
		[^varNode].
	^selector = #value: ifTrue:
		[self codeAssignTo: varNode value: arguments first]
</details>

#### DecompilerConstructor>>#codeThisContext

<details>
	<summary>See more</summary>
	
	codeThisContext

	^NodeThisContext
</details>

#### DecompilerConstructor>>#codeAssignTo: variable value: expression

<details>
	<summary>See more</summary>
	
	codeAssignTo: variable value: expression

	^AssignmentNode new variable: variable value: expression
</details>

#### DecompilerConstructor>>#codeBrace: elements

<details>
	<summary>See more</summary>
	
	codeBrace: elements

	^BraceNode new elements: elements
</details>

#### DecompilerConstructor>>#codeArguments: args temps: temps block: block

<details>
	<summary>See more</summary>
	
	codeArguments: args temps: temps block: block 
	block
		arguments: args;
		temporaries: temps.
	^block
</details>

#### DecompilerConstructor>>#accept: aVisitor

I am not really a ParseNode. Only here to access constants defined in parseNode.


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
</details>

#### DecompilerConstructor>>#codeBlock: statements returns: returns

<details>
	<summary>See more</summary>
	
	codeBlock: statements returns: returns
	^ BlockNode statements: statements returns: returns
</details>

#### DecompilerConstructor>>#codeTemp: index

<details>
	<summary>See more</summary>
	
	codeTemp: index

	^ TempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0
</details>

## LiteralDictionary

A LiteralDictionary, like an IdentityDictionary, has a special test for equality. In this case it is simple equality between objects of like class. This allows equal Float or String literals to be shared without the possibility of erroneously sharing, say, 1 and 1.0

### Methods
#### LiteralDictionary>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [
			element key literalEqual: anObject ])
				ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self error: 'There is no free space in this collection!'
</details>

## OptimizedBlockLocalTempReadBeforeWrittenVisitor

Answer the set of temporary variables that are read before they are written in the visited parse tree. Used by the compiler to detect those block-local temporaries of blocks in optimized loops that require nilling to prevent a value from a previous iteration persisting into a subsequent one.

### Methods
#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#readBeforeWritten

<details>
	<summary>See more</summary>
	
	readBeforeWritten
	^readBeforeWritten ifNil: [IdentitySet new]
</details>

#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	inOptimizedBlock := false
</details>

#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#visitTempVariableNode: aTempVariableNode

<details>
	<summary>See more</summary>
	
	visitTempVariableNode: aTempVariableNode
	(aTempVariableNode isArg
	 or: [written notNil
		and: [written includes: aTempVariableNode]]) ifTrue:
		[^self].
	readBeforeWritten ifNil:
		[readBeforeWritten := IdentitySet new].
	readBeforeWritten add: aTempVariableNode
</details>

#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#visitMessageNode: aMessageNode

<details>
	<summary>See more</summary>
	
	visitMessageNode: aMessageNode
	| savedWritten writtenPostFirstArm |
	(aMessageNode isOptimized
	 and: [#(ifTrue:ifFalse: ifFalse:ifTrue: ifNil:ifNotNil: ifNotNil:ifNil:) includes: aMessageNode selector key]) ifFalse:
		[^super visitMessageNode: aMessageNode].
	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	savedWritten := written copy.
	aMessageNode argumentsInEvaluationOrder
		do: [:argument|
			argument isBlockNode
				ifTrue: [| savedIOB |
					savedIOB := inOptimizedBlock.
					inOptimizedBlock := true.
					[argument accept: self]
						ensure: [inOptimizedBlock := savedIOB]]
				ifFalse: [argument accept: self]]
		separatedBy:
			[writtenPostFirstArm := written.
			 written := savedWritten].
	(written notNil
	 and: [writtenPostFirstArm notNil]) ifTrue:
		[written := written intersection: writtenPostFirstArm]
</details>

#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#visitAssignmentNode: anAssignmentNode

N.B. since assigment happens after the value is evaluated the value is visited first.


<details>
	<summary>See more</summary>
	
	visitAssignmentNode: anAssignmentNode
	anAssignmentNode value accept: self.
	anAssignmentNode variable isTemp
		ifTrue:
			[written ifNil: [written := IdentitySet new].
			 written add: anAssignmentNode variable]
		ifFalse:
			[anAssignmentNode variable accept: self]
</details>

#### OptimizedBlockLocalTempReadBeforeWrittenVisitor>>#visitBlockNode: aBlockNode

If we're in the optimized block in one side of an optimized ifTrue:ifFalse: et al leave it to the enclosing visitMessageNode: activation to handle merging written.


<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode
	| savedWritten |
	"If we're in the optimized block in one side of an optimized ifTrue:ifFalse: et al
	 leave it to the enclosing visitMessageNode: activation to handle merging written."
	inOptimizedBlock ifTrue:
		[^super visitBlockNode: aBlockNode].
	"If we're not then don't update written because without evaluating the guard(s)
	 we can't tell if the block is evaluated or not, and we must avoid false positives."
	savedWritten := written copy.
	super visitBlockNode: aBlockNode.
	written := savedWritten
</details>

## ParseNodeEnumerator

ParseNodeEnumerator implements ParseNode>>nodesDo:. It can be used to enumerate an entire tree via aParseNode accept: (ParseNodeEnumerator ofBlock: aBlock) or selectively, excluding the node and subnodes for which selectBlock answers false, via aParseNode accept: (ParseNodeEnumerator ofBlock: aBlock select: selectBlock) Here's a doIt that generates and compiles the visiting methods: self superclass selectors do: [:s| self compile: (String streamContents: [:str| | arg | arg := 'a', (s allButFirst: 5) allButLast. str nextPutAll: s, ' ', arg; crtab; nextPutAll: '(theSelectBlock isNil or: [theSelectBlock value: '; nextPutAll: arg; nextPutAll: ']) ifFalse:'; crtab; tab: 2; nextPutAll: '[^nil].'; crtab; nextPutAll: 'theBlock value: '; nextPutAll: arg; nextPut: $.; crtab; nextPutAll: '^super '; nextPutAll: s, ' ', arg])]

### Methods
#### ParseNodeEnumerator>>#visitLiteralVariableNode: aLiteralVariableNode

<details>
	<summary>See more</summary>
	
	visitLiteralVariableNode: aLiteralVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aLiteralVariableNode]) ifFalse:
		[^nil].
	theBlock value: aLiteralVariableNode.
	^super visitLiteralVariableNode: aLiteralVariableNode
</details>

#### ParseNodeEnumerator>>#visitTempVariableNode: aTempVariableNode

<details>
	<summary>See more</summary>
	
	visitTempVariableNode: aTempVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aTempVariableNode]) ifFalse:
		[^nil].
	theBlock value: aTempVariableNode.
	^super visitTempVariableNode: aTempVariableNode
</details>

#### ParseNodeEnumerator>>#visitNewArrayNode: aNewArrayNode

<details>
	<summary>See more</summary>
	
	visitNewArrayNode: aNewArrayNode
	(theSelectBlock isNil or: [theSelectBlock value: aNewArrayNode]) ifFalse:
		[^nil].
	theBlock value: aNewArrayNode.
	^super visitNewArrayNode: aNewArrayNode
</details>

#### ParseNodeEnumerator>>#visitInstanceVariableNode: anInstanceVariableNode

<details>
	<summary>See more</summary>
	
	visitInstanceVariableNode: anInstanceVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: anInstanceVariableNode]) ifFalse:
		[^nil].
	theBlock value: anInstanceVariableNode.
	^super visitInstanceVariableNode: anInstanceVariableNode
</details>

#### ParseNodeEnumerator>>#visitLiteralNode: aLiteralNode

<details>
	<summary>See more</summary>
	
	visitLiteralNode: aLiteralNode
	(theSelectBlock isNil or: [theSelectBlock value: aLiteralNode]) ifFalse:
		[^nil].
	theBlock value: aLiteralNode.
	^super visitLiteralNode: aLiteralNode
</details>

#### ParseNodeEnumerator>>#visitCascadeNode: aCascadeNode

<details>
	<summary>See more</summary>
	
	visitCascadeNode: aCascadeNode
	(theSelectBlock isNil or: [theSelectBlock value: aCascadeNode]) ifFalse:
		[^nil].
	theBlock value: aCascadeNode.
	^super visitCascadeNode: aCascadeNode
</details>

#### ParseNodeEnumerator>>#visitMessageNode: aMessageNode

<details>
	<summary>See more</summary>
	
	visitMessageNode: aMessageNode
	(theSelectBlock isNil or: [theSelectBlock value: aMessageNode]) ifFalse:
		[^nil].
	theBlock value: aMessageNode.
	^super visitMessageNode: aMessageNode
</details>

#### ParseNodeEnumerator>>#visitSelectorNode: aSelectorNode

<details>
	<summary>See more</summary>
	
	visitSelectorNode: aSelectorNode
	(theSelectBlock isNil or: [theSelectBlock value: aSelectorNode]) ifFalse:
		[^nil].
	theBlock value: aSelectorNode.
	^super visitSelectorNode: aSelectorNode
</details>

#### ParseNodeEnumerator>>#visitVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	visitVariableNode: aVariableNode
	(theSelectBlock isNil or: [theSelectBlock value: aVariableNode]) ifFalse:
		[^nil].
	theBlock value: aVariableNode.
	^super visitVariableNode: aVariableNode
</details>

#### ParseNodeEnumerator>>#visitRemoteTempVectorNode: aRemoteTempVectorNode

<details>
	<summary>See more</summary>
	
	visitRemoteTempVectorNode: aRemoteTempVectorNode
	(theSelectBlock isNil or: [theSelectBlock value: aRemoteTempVectorNode]) ifFalse:
		[^nil].
	theBlock value: aRemoteTempVectorNode.
	^super visitRemoteTempVectorNode: aRemoteTempVectorNode
</details>

#### ParseNodeEnumerator>>#ofBlock: aBlock select: aSelectBlock

<details>
	<summary>See more</summary>
	
	ofBlock: aBlock select: aSelectBlock
	theBlock := aBlock.
	theSelectBlock := aSelectBlock
</details>

#### ParseNodeEnumerator>>#visitBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode
	(theSelectBlock isNil or: [theSelectBlock value: aBlockNode]) ifFalse:
		[^nil].
	theBlock value: aBlockNode.
	^super visitBlockNode: aBlockNode
</details>

#### ParseNodeEnumerator>>#visitMethodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNode: aMethodNode
	(theSelectBlock isNil or: [theSelectBlock value: aMethodNode]) ifFalse:
		[^nil].
	theBlock value: aMethodNode.
	^super visitMethodNode: aMethodNode
</details>

#### ParseNodeEnumerator>>#visitBraceNode: aBraceNode

<details>
	<summary>See more</summary>
	
	visitBraceNode: aBraceNode
	(theSelectBlock isNil or: [theSelectBlock value: aBraceNode]) ifFalse:
		[^nil].
	theBlock value: aBraceNode.
	^super visitBraceNode: aBraceNode
</details>

#### ParseNodeEnumerator>>#ofBlock: aBlock

<details>
	<summary>See more</summary>
	
	ofBlock: aBlock
	theBlock := aBlock
</details>

#### ParseNodeEnumerator>>#visitAssignmentNode: anAssignmentNode

N.B. since assigment happens after the value is evaluated the value is visited first.


<details>
	<summary>See more</summary>
	
	visitAssignmentNode: anAssignmentNode
	(theSelectBlock isNil or: [theSelectBlock value: anAssignmentNode]) ifFalse:
		[^nil].
	theBlock value: anAssignmentNode.
	^super visitAssignmentNode: anAssignmentNode
</details>

#### ParseNodeEnumerator>>#visitReturnNode: aReturnNode

<details>
	<summary>See more</summary>
	
	visitReturnNode: aReturnNode
	(theSelectBlock isNil or: [theSelectBlock value: aReturnNode]) ifFalse:
		[^nil].
	theBlock value: aReturnNode.
	^super visitReturnNode: aReturnNode
</details>

#### ParseNodeEnumerator>>#visitMessageNodeInCascade: aMessageNodeInCascade

receiver is nil for cascades


<details>
	<summary>See more</summary>
	
	visitMessageNodeInCascade: aMessageNodeInCascade
	(theSelectBlock isNil or: [theSelectBlock value: aMessageNodeInCascade]) ifFalse:
		[^nil].
	theBlock value: aMessageNodeInCascade.
	^super visitMessageNodeInCascade: aMessageNodeInCascade
</details>

## ParseNodeVisitor

I am an abstract superclass for ParseNode visitors that functions as a null visitor. Here's the code that defines my interface: (SystemNavigation default allImplementorsOf: #accept: localTo: ParseNode) do: [:methodReference| methodReference compiledMethod messages do: [:sel| ((sel beginsWith: 'visit') and: [sel numArgs = 1]) ifTrue: [ParseNodeVisitor compile: (String streamContents: [:str| str nextPutAll: sel; space; nextPut: $a. methodReference classSymbol first isVowel ifTrue: [str nextPut: $n]. str nextPutAll: methodReference classSymbol]) classified: 'visiting']]]

### Methods
#### ParseNodeVisitor>>#visitLiteralVariableNode: aLiteralVariableNode

<details>
	<summary>See more</summary>
	
	visitLiteralVariableNode: aLiteralVariableNode
</details>

#### ParseNodeVisitor>>#visitIfNotNil: aParseNode

RNG: this was implemented to support unexpected nil temporary declarations (coming from instances of MethodNode and BlockNode that were living in the image before the new parse nodes were introduced)


<details>
	<summary>See more</summary>
	
	visitIfNotNil: aParseNode
	"RNG: this was implemented to support unexpected nil temporary declarations
	(coming from instances of MethodNode and BlockNode that were living in the image
	before the new parse nodes were introduced)"

	aParseNode ifNotNil: [ aParseNode accept: self ]
</details>

#### ParseNodeVisitor>>#visitTempVariableNode: aTempVariableNode

<details>
	<summary>See more</summary>
	
	visitTempVariableNode: aTempVariableNode
</details>

#### ParseNodeVisitor>>#visitUndeclaredVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	visitUndeclaredVariableNode: aVariableNode
</details>

#### ParseNodeVisitor>>#visitNewArrayNode: aNewArrayNode

<details>
	<summary>See more</summary>
	
	visitNewArrayNode: aNewArrayNode
</details>

#### ParseNodeVisitor>>#visitInstanceVariableNode: anInstanceVariableNode

<details>
	<summary>See more</summary>
	
	visitInstanceVariableNode: anInstanceVariableNode
</details>

#### ParseNodeVisitor>>#visitLiteralNode: aLiteralNode

<details>
	<summary>See more</summary>
	
	visitLiteralNode: aLiteralNode
</details>

#### ParseNodeVisitor>>#visitTemporariesDeclarationNode: aTemporariesDeclarationNode

<details>
	<summary>See more</summary>
	
	visitTemporariesDeclarationNode: aTemporariesDeclarationNode

	aTemporariesDeclarationNode temporaryDeclarationNodesDo:
		[ :temporaryDeclarationNode | temporaryDeclarationNode accept: self ]
</details>

#### ParseNodeVisitor>>#visitCascadeNode: aCascadeNode

<details>
	<summary>See more</summary>
	
	visitCascadeNode: aCascadeNode
	aCascadeNode receiver accept: self.
	aCascadeNode messages do:
		[:message| self visitMessageNodeInCascade: message]
</details>

#### ParseNodeVisitor>>#visitMessageNode: aMessageNode

<details>
	<summary>See more</summary>
	
	visitMessageNode: aMessageNode

	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do: [:argument| argument accept: self]
</details>

#### ParseNodeVisitor>>#visitSelectorNode: aSelectorNode

<details>
	<summary>See more</summary>
	
	visitSelectorNode: aSelectorNode
</details>

#### ParseNodeVisitor>>#visitVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	visitVariableNode: aVariableNode
</details>

#### ParseNodeVisitor>>#visitTemporaryDeclarationNode: aTemporaryDeclarationNode

<details>
	<summary>See more</summary>
	
	visitTemporaryDeclarationNode: aTemporaryDeclarationNode
</details>

#### ParseNodeVisitor>>#visitRemoteTempVectorNode: aRemoteTempVectorNode

<details>
	<summary>See more</summary>
	
	visitRemoteTempVectorNode: aRemoteTempVectorNode
</details>

#### ParseNodeVisitor>>#visitBlockNode: aBlockNode

<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode

	self visitIfNotNil: aBlockNode temporariesDeclaration.
	aBlockNode statementsDo:
		[ :statement| statement accept: self ]
</details>

#### ParseNodeVisitor>>#visitMethodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNode: aMethodNode

	self visitIfNotNil: aMethodNode temporariesDeclaration.
	aMethodNode block accept: self
</details>

#### ParseNodeVisitor>>#visitBraceNode: aBraceNode

<details>
	<summary>See more</summary>
	
	visitBraceNode: aBraceNode

	aBraceNode elementsDo:
		[ :element | element accept: self ]
</details>

#### ParseNodeVisitor>>#visitAssignmentNode: anAssignmentNode

N.B. since assigment happens after the value is evaluated the value is visited first.


<details>
	<summary>See more</summary>
	
	visitAssignmentNode: anAssignmentNode
	"N.B.  since assigment happens after the value is evaluated the value is visited first."
	anAssignmentNode value accept: self.
	anAssignmentNode variable accept: self
</details>

#### ParseNodeVisitor>>#visitReturnNode: aReturnNode

<details>
	<summary>See more</summary>
	
	visitReturnNode: aReturnNode
	aReturnNode expr accept: self
</details>

#### ParseNodeVisitor>>#visitMessageNodeInCascade: aMessageNode

receiver is nil for cascades


<details>
	<summary>See more</summary>
	
	visitMessageNodeInCascade: aMessageNode
	"receiver is nil for cascades"
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]
</details>

## ParseStack

I keep track of the current and high position of the stack that will be needed by code being compiled.

### Methods
#### ParseStack>>#pop: n

<details>
	<summary>See more</summary>
	
	pop: n

	(position := position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']
</details>

#### ParseStack>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	
	super printOn: aStream.
	aStream nextPutAll: ' at '; print: position; nextPutAll: ' of '; print: length
</details>

#### ParseStack>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size

	^length
</details>

#### ParseStack>>#init

<details>
	<summary>See more</summary>
	
	init

	length := position := 0
</details>

#### ParseStack>>#position: n

<details>
	<summary>See more</summary>
	
	position: n 
	(position := n) > length
		ifTrue: [length := position]
</details>

#### ParseStack>>#push: n

<details>
	<summary>See more</summary>
	
	push: n

	(position := position + n) > length 
		ifTrue: [length := position]
</details>

#### ParseStack>>#position

<details>
	<summary>See more</summary>
	
	position

	^position
</details>

## ReparseAfterSourceEditing

A ReparseAfterSourceEditing is a Notification used to restart the syntax parsing phase of a compilation after a change in source code.

### Methods
## UndeclaredVariableReference

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UndeclaredVariableReference>>#varName

<details>
	<summary>See more</summary>
	
	varName
	^varName
</details>

#### UndeclaredVariableReference>>#varEnd: aNumber

<details>
	<summary>See more</summary>
	
	varEnd: aNumber
	varEnd := aNumber
</details>

#### UndeclaredVariableReference>>#parser: aParser

<details>
	<summary>See more</summary>
	
	parser: aParser
	parser := aParser
</details>

#### UndeclaredVariableReference>>#varStart: aNumber

<details>
	<summary>See more</summary>
	
	varStart: aNumber
	varStart := aNumber
</details>

#### UndeclaredVariableReference>>#varName: aString

<details>
	<summary>See more</summary>
	
	varName: aString
	varName := aString
</details>

#### UndeclaredVariableReference>>#parser

<details>
	<summary>See more</summary>
	
	parser
	^parser
</details>

#### UndeclaredVariableReference>>#varEnd

<details>
	<summary>See more</summary>
	
	varEnd
	^varEnd
</details>

#### UndeclaredVariableReference>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction

	^parser correctVariable: varName interval: (varStart to: varEnd)
</details>

#### UndeclaredVariableReference>>#declareTempAndResume

<details>
	<summary>See more</summary>
	
	declareTempAndResume

	parser declareTemp: varName at: #method.
	self resume: varName
</details>

#### UndeclaredVariableReference>>#varStart

<details>
	<summary>See more</summary>
	
	varStart
	^varStart
</details>

## UndeclaredVariableWarning

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UndeclaredVariableWarning>>#selector

<details>
	<summary>See more</summary>
	
	selector

	^selector 
</details>

#### UndeclaredVariableWarning>>#defaultResumeValue

Answer the value that by default should be returned if the exception is resumed


<details>
	<summary>See more</summary>
	
	defaultResumeValue
	"Answer the value that by default should be returned if the exception is resumed"
	^true
</details>

#### UndeclaredVariableWarning>>#defaultAction

The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The user should be notified of the occurrence of an exceptional occurrence and
	 given an option of continuing or aborting the computation. The description of the
	 occurrence should include any text specified as the argument of the #signal: message."
	
	selector
		ifNotNil: [Transcript newLine; nextPutAll: class name, '>>', selector, ' ']
		ifNil: [Transcript newLine ].
	Transcript show: '(' , name , ' is Undeclared) '.
	^true
</details>

#### UndeclaredVariableWarning>>#classImplementingSelector

<details>
	<summary>See more</summary>
	
	classImplementingSelector

	^class 
</details>

#### UndeclaredVariableWarning>>#variableName

<details>
	<summary>See more</summary>
	
	variableName

	^name 
</details>

#### UndeclaredVariableWarning>>#name: aString selector: aSymbolOrNil class: aBehavior

<details>
	<summary>See more</summary>
	
	name: aString selector: aSymbolOrNil class: aBehavior
	name := aString.
	selector := aSymbolOrNil.
	class := aBehavior
</details>

## VariableScopeFinder

A VariableScopeFinder is used to find the minimum enclosing scope of a variable in a method. This is used when auto-declaring temporaries to find the smallest enclosing block in which to declare the temp. Instance Variables theVariable: <VariableNode> theVariable - the varable whose scope is to be determined

### Methods
#### VariableScopeFinder>>#visitLiteralVariableNode: aNode

<details>
	<summary>See more</summary>
	
	visitLiteralVariableNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#visitTempVariableNode: aNode

<details>
	<summary>See more</summary>
	
	visitTempVariableNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#enclosingNodeFor: enumerator of: rootNode

Answer the minimum enclosing root node for aVariabe or nil if none. If the variable is accessed in more than one subnode then the rootNode is the enclosing node, otherwise it is which ever single subnode node that includes it, if any. enumerator applies its argument to all relevant subnodes of rootNode.


<details>
	<summary>See more</summary>
	
	enclosingNodeFor: enumerator of: rootNode
	"Answer the minimum enclosing root node for aVariabe or nil if none.
	 If the variable is accessed in more than one subnode then the rootNode is the
	 enclosing node, otherwise it is which ever single subnode node that includes it, if any.
	 enumerator applies its argument to all relevant subnodes of rootNode."
	| enclosingNodeOrNil |
	enclosingNodeOrNil := nil.
	enumerator value:
		[:subnode|
		(subnode accept: self) ifNotNil:
			[:enclosingNode|
			enclosingNodeOrNil := enclosingNodeOrNil
										ifNil: [enclosingNode]
										ifNotNil: [rootNode]]].
	^enclosingNodeOrNil
</details>

#### VariableScopeFinder>>#visitUndeclaredVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	visitUndeclaredVariableNode: aVariableNode
	^theVariable name = aVariableNode name ifTrue: [theVariable]
</details>

#### VariableScopeFinder>>#visitNewArrayNode: aNode

<details>
	<summary>See more</summary>
	
	visitNewArrayNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#visitInstanceVariableNode: aNode

<details>
	<summary>See more</summary>
	
	visitInstanceVariableNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#visitLiteralNode: aNode

<details>
	<summary>See more</summary>
	
	visitLiteralNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#visitCascadeNode: aCascadeNode

Answer the minimum enclosing node for aVariabe or nil if none. If the variable is accessed in more than one subexpression then aMessageNode is the enclosing node, otherwise it is which ever single node that includes it, if any.


<details>
	<summary>See more</summary>
	
	visitCascadeNode: aCascadeNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aCascadeNode receiver.
							aCascadeNode messages do:
								[:msg| msg argumentsInEvaluationOrder do: aBlock]]
		of: aCascadeNode
</details>

#### VariableScopeFinder>>#visitMessageNode: aMessageNode

Answer the minimum enclosing node for aVariabe or nil if none. If the variable is accessed in more than one subexpression then aMessageNode is the enclosing node, otherwise it is which ever single node that includes it, if any.


<details>
	<summary>See more</summary>
	
	visitMessageNode: aMessageNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aMessageNode is the
	 enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock value: aMessageNode receiver.
							aMessageNode argumentsInEvaluationOrder do: aBlock]
		of: aMessageNode
</details>

#### VariableScopeFinder>>#visitSelectorNode: aNode

<details>
	<summary>See more</summary>
	
	visitSelectorNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#visitVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	visitVariableNode: aVariableNode
	^nil
</details>

#### VariableScopeFinder>>#visitRemoteTempVectorNode: aNode

<details>
	<summary>See more</summary>
	
	visitRemoteTempVectorNode: aNode
	^nil
</details>

#### VariableScopeFinder>>#ofVariable: aVariableNode

<details>
	<summary>See more</summary>
	
	ofVariable: aVariableNode
	theVariable := aVariableNode
</details>

#### VariableScopeFinder>>#visitBlockNode: aBlockNode

Answer the minimum enclosing node for aVariabe or nil if none. If the variable is accessed in more than one statement then aBlockNode is the enclosing node, otherwise it is which ever single block node that includes it, if any.


<details>
	<summary>See more</summary>
	
	visitBlockNode: aBlockNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one statement then aBlockNode is the
	 enclosing node, otherwise it is which ever single block node that includes it, if any."
	^(self enclosingNodeFor: [:aBlock| aBlockNode statementsDo: aBlock] of: aBlockNode) ifNotNil:
		[:aNode|
		aNode isBlockNode ifTrue: [aNode] ifFalse: [aBlockNode]]
</details>

#### VariableScopeFinder>>#visitMethodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	visitMethodNode: aMethodNode
	^aMethodNode block accept: self
</details>

#### VariableScopeFinder>>#visitBraceNode: aBraceNode

Answer the minimum enclosing node for aVariabe or nil if none. If the variable is accessed in more than one subexpression then aBraceNode is the enclosing node, otherwise it is which ever single node that includes it, if any.


<details>
	<summary>See more</summary>
	
	visitBraceNode: aBraceNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then aBraceNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock| aBraceNode elementsDo: aBlock]
		of: aBraceNode
</details>

#### VariableScopeFinder>>#visitAssignmentNode: anAssignmentNode

Answer the minimum enclosing node for aVariabe or nil if none. If the variable is accessed in more than one subexpression then anAssignmentNode is the enclosing node, otherwise it is which ever single node that includes it, if any.


<details>
	<summary>See more</summary>
	
	visitAssignmentNode: anAssignmentNode
	"Answer the minimum enclosing node for aVariabe or nil if none.
	 If the variable is accessed in more than one subexpression then anAssignmentNode
	 is the enclosing node, otherwise it is which ever single node that includes it, if any."
	^self
		enclosingNodeFor: [:aBlock|
							aBlock
								value: anAssignmentNode value;
								value: anAssignmentNode variable]
		of: anAssignmentNode
</details>

#### VariableScopeFinder>>#visitReturnNode: aReturnNode

<details>
	<summary>See more</summary>
	
	visitReturnNode: aReturnNode
	^aReturnNode expr accept: self
</details>

