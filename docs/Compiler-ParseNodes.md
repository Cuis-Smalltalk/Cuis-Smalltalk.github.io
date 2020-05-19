## AssignmentNode

AssignmentNode comment: 'I represent a (var_expr) construct.'

### Methods
#### AssignmentNode>>#emitCodeForEffect: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEffect: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: false encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStorePop: stack encoder: encoder
</details>

#### AssignmentNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' _ '.
	value printOn: aStream indent: level
</details>

#### AssignmentNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder

	variable emitCodeForLoad: stack forValue: true encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStore: stack encoder: encoder
</details>

#### AssignmentNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isAssignmentNode
		and: [ self variable equivalentTo: aParseNode variable ]
		and: [ self value equivalentTo: aParseNode value ]
</details>

#### AssignmentNode>>#isAssignmentToTemporary

<details>
	<summary>See more</summary>
	
	isAssignmentToTemporary

	^ self isAssignmentNode and: [ variable isTemp ]
</details>

#### AssignmentNode>>#expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

<details>
	<summary>See more</summary>
	
	expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

	^ self consolidateAsCollection: (self
		expandRange: (sourceRanges isInterval ifTrue: [ sourceRanges ] ifFalse: [ sourceRanges first ])
		basedOn: (allSourceRanges at: variable))
</details>

#### AssignmentNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	^ value isComplex
</details>

#### AssignmentNode>>#value

<details>
	<summary>See more</summary>
	
	value
	^ value
</details>

#### AssignmentNode>>#variable: aVariable value: expression from: encoder

Case of remote temp vars


<details>
	<summary>See more</summary>
	
	variable: aVariable value: expression from: encoder

	"Case of remote temp vars"
	(aVariable isMemberOf: MessageAsTempNode) ifTrue: [ ^aVariable store: expression from: encoder].
	
	variable := aVariable.
	value := expression.
	
	^self
	

</details>

#### AssignmentNode>>#variable

<details>
	<summary>See more</summary>
	
	variable
	^variable
</details>

#### AssignmentNode>>#analyseTempsWithin: scopeBlock "<BlockNode>"  rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>"  rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"N.B.  since assigment happens _after_ the value is evaluated the value is sent the message _first_."
	value analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools.
	variable beingAssignedToAnalyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools
</details>

#### AssignmentNode>>#toDoIncrement: var

Only meant for Messages or Assignments - else return nil


<details>
	<summary>See more</summary>
	
	toDoIncrement: var
	var = variable ifFalse: [^ nil].
	(value isMemberOf: MessageNode) 
		ifTrue: [^ value toDoIncrement: var]
		ifFalse: [^ nil]
</details>

#### AssignmentNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level 
	variable printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPutAll: ' _ '.
	value printWithClosureAnalysisOn: aStream indent: level
</details>

#### AssignmentNode>>#printOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level precedence: p

	aStream nextPut: $(.
	self printOn: aStream indent: level.
	aStream nextPut: $)
</details>

#### AssignmentNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder

	^(variable sizeCodeForLoad: encoder forValue: true)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStore: encoder)
</details>

#### AssignmentNode>>#variable: aVariable value: expression

<details>
	<summary>See more</summary>
	
	variable: aVariable value: expression

	variable := aVariable.
	value := expression
</details>

#### AssignmentNode>>#isAssignmentNode

<details>
	<summary>See more</summary>
	
	isAssignmentNode
	^true
</details>

#### AssignmentNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitAssignmentNode: self
</details>

#### AssignmentNode>>#variable: aVariable value: expression from: encoder sourceRange: range

<details>
	<summary>See more</summary>
	
	variable: aVariable value: expression from: encoder sourceRange: range

	| realNode |
	
	realNode := self variable: aVariable value: expression from: encoder.
	encoder noteSourceRange: range forNode: realNode.
	
	^realNode
</details>

#### AssignmentNode>>#sizeCodeForEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEffect: encoder

	^(variable sizeCodeForLoad: encoder forValue: false)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStorePop: encoder)
</details>

#### AssignmentNode>>#printWithClosureAnalysisOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level precedence: p

	aStream nextPut: $(.
	self printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPut: $)
</details>

## BlockNode

I represent a bracketed block with 0 or more arguments and 1 or more statements. If I am initialized with no statements, I create one. I have a flag to tell whether my last statement returns a value from the enclosing method. My last three fields remember data needed for code generation. I can emit for value in the usual way, in which case I create a literal method (actually a context remotely copied) to be evaluated by sending it value: at run time. Or I can emit code to be evaluated in line; this only happens at the top level of a method and in conditionals and while-loops, none of which have arguments. We are in the process of refactoring "temporaries" inst var to a "temporariesDeclaration" inst var which is a parse node that contains more than just the temporaries' nodes, it is the node that represents the declaration itself. Refer to the class comment in MethodNode for more information on how to migrate to "temporariesDeclaration".

### Methods
#### BlockNode>>#sizeCodeForClosureValue: encoder

Compute the size for the creation of the block and its code.


<details>
	<summary>See more</summary>
	
	sizeCodeForClosureValue: encoder
	"Compute the size for the creation of the block and its code."
	"If we have the closure bytecodes constructClosureCreationNode: will note
	 the copied values in the copiedValues inst var and answer #pushCopiedValues."
	closureCreationNode := self constructClosureCreationNode: encoder.
	"Remember size of body for emit time so we know the size of the jump around it."
	size := self sizeCodeForEvaluatedClosureValue: encoder.
	^encoder supportsClosureOpcodes
		ifTrue:
			[(copiedValues inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
			 + (encoder sizePushClosureCopyNumCopiedValues: copiedValues size numArgs: arguments size jumpSize: size)
			 + size]
		ifFalse:
			["closureCreationSupportNode is send closureCopy:copiedValues:"
			(closureCreationNode sizeCodeForValue: encoder)
			 + (encoder sizeJumpLong: size)
			 + size]
</details>

#### BlockNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level
	| separateLines |
	aStream nextPut: $[.
	self
		printArgumentsOn: aStream
		indent: level.
	separateLines _ (self
		printTemporaries: temporaries
		on: aStream
		doPrior: [ aStream space ]) or: [arguments notNil and: [arguments notEmpty] ].
	Preferences prettyPrintRectangularBlocks
		ifTrue: [
			"If args+temps > 0 and statements > 1 (or just one complex statement),
			put all statements on separate lines"
			separateLines
				ifTrue: [
					(statements size > 1 or: [
						statements size = 1 and: [ statements first isComplex ]])
							ifTrue: [ aStream newLineTab: (1 max: level) ]
							ifFalse: [ aStream space ] ]
				ifFalse: [
					(statements size = 1 and: [ statements first isComplex not ])
						ifTrue: [ aStream space ]]]
		ifFalse: [
			self isComplex
				ifTrue: [ aStream newLineTab: (1 max: level) ]
				ifFalse: [ aStream space ] ].
	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~= $] ])
		ifTrue: [ aStream space ].
	aStream nextPut: $]
</details>

#### BlockNode>>#arguments

<details>
	<summary>See more</summary>
	
	arguments
	^arguments ifNil: [#()]
</details>

#### BlockNode>>#temporariesDeclaration: aTemporariesDeclarationNode

RNG: after removing all the usages of the temporaries inst var, the last line can be removed


<details>
	<summary>See more</summary>
	
	temporariesDeclaration: aTemporariesDeclarationNode
	"RNG: after removing all the usages of the temporaries inst var, the last line can be removed"

	temporariesDeclaration := aTemporariesDeclarationNode.
	self temporaries: aTemporariesDeclarationNode allDeclaredVariableNodes
</details>

#### BlockNode>>#deoptimize

<details>
	<summary>See more</summary>
	
	deoptimize
	optimized := false.
	optimizedMessageNode := nil
</details>

#### BlockNode>>#emitCodeForClosureValue: stack encoder: encoder

if not supportsClosureOpcodes closureCreationSupportNode is the node for thisContext closureCopy: numArgs [ copiedValues: { values } ]


<details>
	<summary>See more</summary>
	
	emitCodeForClosureValue: stack encoder: encoder
	"if not supportsClosureOpcodes closureCreationSupportNode is the
	 node for thisContext closureCopy: numArgs [ copiedValues: { values } ]"
	encoder supportsClosureOpcodes
		ifTrue:
			[copiedValues do:
				[:copiedValue| copiedValue emitCodeForValue: stack encoder: encoder].
			 closureCreationNode pc: encoder methodStreamPosition + 1.
			 encoder
				genPushClosureCopyNumCopiedValues: copiedValues size
				numArgs: arguments size
				jumpSize: size.
			 stack
				pop: copiedValues size;
				push: 1]
		ifFalse:
			[closureCreationNode emitCodeForValue: stack encoder: encoder.
			 encoder genJumpLong: size]. "Force a two byte jump."
	"Emit the body of the block"
	self emitCodeForEvaluatedClosureValue: stack encoder: encoder
</details>

#### BlockNode>>#addArgument: aTempVariableNode

<details>
	<summary>See more</summary>
	
	addArgument: aTempVariableNode
	temporaries := temporaries copyWith: aTempVariableNode
</details>

#### BlockNode>>#nilReadBeforeWrittenTemps

<details>
	<summary>See more</summary>
	
	nilReadBeforeWrittenTemps
	| visitor readBeforeWritten |
	self accept: (visitor := OptimizedBlockLocalTempReadBeforeWrittenVisitor new).
	readBeforeWritten := visitor readBeforeWritten.
	temporaries reverseDo:
		[:temp|
		((readBeforeWritten includes: temp)
		 and: [temp isRemote not]) ifTrue:
			[statements addFirst: (AssignmentNode new variable: temp value: NodeNil)]]
</details>

#### BlockNode>>#remoteTempNodeName

Answer a useful name for a RemoteTempVectorNode in the receiver.


<details>
	<summary>See more</summary>
	
	remoteTempNodeName
	"Answer a useful name for a RemoteTempVectorNode in the receiver."
	| prefix scope extent |
	prefix := actualScopeIfOptimized ifNil: ['<'] ifNotNil: [ '<...'].
	scope := self.
	[extent := scope blockExtent.
	 extent == nil
	 and: [scope actualScope ~~ scope]] whileTrue:
		[scope := scope actualScope].
	^extent
		ifNil: [prefix, '?-?>']
		ifNotNil:
			[prefix, extent first printString, '-',
				(extent last isZero
					ifTrue: ['?']
					ifFalse: [extent last printString]), '>']
</details>

#### BlockNode>>#hasEquivalentStatementsWith: aCodeNode

<details>
	<summary>See more</summary>
	
	hasEquivalentStatementsWith: aCodeNode

	self statements with: aCodeNode statements do: [ :myStatement :otherCodeNodeStatement |
		(myStatement equivalentTo: otherCodeNodeStatement) ifFalse: [ ^ false ] ].
	^ true
</details>

#### BlockNode>>#startOfLastStatement

<details>
	<summary>See more</summary>
	
	startOfLastStatement
	^startOfLastStatement
</details>

#### BlockNode>>#arguments: argNodes statements: statementsCollection returns: returnBool from: encoder

Compile.


<details>
	<summary>See more</summary>
	
	arguments: argNodes statements: statementsCollection returns: returnBool from: encoder
	"Compile."
	arguments _ argNodes.
	statements _ statementsCollection size > 0
		ifTrue: [ statementsCollection ]
		ifFalse: [ Array with: NodeNil ].
	optimized _ false.
	returns _ returnBool
</details>

#### BlockNode>>#isQuick

<details>
	<summary>See more</summary>
	
	isQuick
	^ statements size = 1
		and: [statements first isVariableReference
				or: [statements first isSpecialConstant]]
</details>

#### BlockNode>>#printArgumentsOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printArgumentsOn: aStream indent: level
	(arguments isNil or: [arguments isEmpty]) ifTrue: [^ self].
	aStream space.
	arguments do:
		[ :arg | aStream nextPut: $:; nextPutAll: arg key; space ].
	aStream nextPut: $|
</details>

#### BlockNode>>#tempsMark: anInteger

Index of the end of the temporaries declarations in the containing MethodNode sourceText


<details>
	<summary>See more</summary>
	
	tempsMark: anInteger
	"Index of the end of the temporaries declarations in the containing MethodNode sourceText"
	tempsMark := anInteger
</details>

#### BlockNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	^self sizeCodeForClosureValue: encoder
</details>

#### BlockNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level
	aStream nextPut: $[; space.
	blockExtent ifNotNil: [aStream print: blockExtent].
	self printWithClosureAnalysisArgumentsOn: aStream indent: level.
	self printWithClosureAnalysisTemporariesOn: aStream indent: level.
	self printWithClosureAnalysisStatementsOn: aStream indent: level.
	aStream space; nextPut: $]
</details>

#### BlockNode>>#actualScope

Answer the actual scope for the receiver. If this is an unoptimized block then it is its actual scope, but if this is an optimized block then the actual scope is some outer block.


<details>
	<summary>See more</summary>
	
	actualScope
	"Answer the actual scope for the receiver.  If this is an unoptimized block then it is its
	 actual scope, but if this is an optimized block then the actual scope is some outer block."
	^actualScopeIfOptimized ifNil: [self]
</details>

#### BlockNode>>#isJustCaseError

<details>
	<summary>See more</summary>
	
	isJustCaseError

	^ statements size = 1 and:
		[statements first
			isMessage: #caseError
			receiver: [:r | r==NodeSelf]
			arguments: nil]
</details>

#### BlockNode>>#statementsDo: aBlock

<details>
	<summary>See more</summary>
	
	statementsDo: aBlock

	statements do: aBlock
</details>

#### BlockNode>>#returnLast

<details>
	<summary>See more</summary>
	
	returnLast

	self returns
		ifFalse: 
			[returns := true.
			statements at: statements size put: statements last asReturnNode]
</details>

#### BlockNode>>#computeCopiedValues: rootNode

<details>
	<summary>See more</summary>
	
	computeCopiedValues: rootNode
	| referencedValues |
	referencedValues := rootNode referencedValuesWithinBlockExtent: blockExtent.
	^(referencedValues reject: [:temp| temp isDefinedWithinBlockExtent: blockExtent])
		asArray sort: ParseNode tempSortBlock
</details>

#### BlockNode>>#noteSourceRangeStart: start end: end encoder: encoder

Note two source ranges for this node. One is for the debugger and is of the last expression, the result of the block. One is for source analysis and is for the entire block.


<details>
	<summary>See more</summary>
	
	noteSourceRangeStart: start end: end encoder: encoder
	"Note two source ranges for this node.  One is for the debugger
	 and is of the last expression, the result of the block.  One is for
	 source analysis and is for the entire block."
	encoder
		noteSourceRange: (start to: end)
		forNode: self closureCreationNode.
	startOfLastStatement
		ifNil:
			[encoder
				noteSourceRange: (start to: end)
				forNode: self]
		ifNotNil:
			[encoder
				noteSourceRange: (startOfLastStatement to: end - 1)
				forNode: self]
</details>

#### BlockNode>>#closureCreationNode

<details>
	<summary>See more</summary>
	
	closureCreationNode
	closureCreationNode ifNil:
		[closureCreationNode := LeafNode new
									key: #closureCreationNode
									code: nil].
	^closureCreationNode
</details>

#### BlockNode>>#returns

<details>
	<summary>See more</summary>
	
	returns

	^returns or: [statements last isReturningIf]
</details>

#### BlockNode>>#isJust: node

<details>
	<summary>See more</summary>
	
	isJust: node

	returns ifTrue: [^false].
	^statements size = 1 and: [statements first == node]
</details>

#### BlockNode>>#tempsMark

Index of the end of the temporaries declarations in the containing MethodNode sourceText


<details>
	<summary>See more</summary>
	
	tempsMark
	"Index of the end of the temporaries declarations in the containing MethodNode sourceText"
	^tempsMark
</details>

#### BlockNode>>#analyseArguments: methodArguments temporaries: methodTemporaries rootNode: rootNode

<MethodNode>


<details>
	<summary>See more</summary>
	
	analyseArguments: methodArguments temporaries: methodTemporaries rootNode: rootNode "<MethodNode>" "^<Sequence of: <TempVarNade>>"
	"Top level entry-point for analysing temps within the hierarchy of blocks in the receiver's method.
	 Answer the (possibly modified) sequence of temp vars.
	 Need to hoist temps out of macro-optimized blocks into their actual blocks.
	 Need to note reads and writes to temps from blocks other than their actual blocks to determine
	 whether blocks can be local (simple slots within a block/method context) or remote (slots in
	 indirection vectors that are shared between contexts by sharing indirection vectors).

	 The algorithm is based on numbering temporary reads and writes and block extents.
	 The index used for numbering starts at zero and is incremented on every block entry
	 and block exit.  So the following
		| a b blk r1 r2 t |
		a := 1. b := 2. t := 0.
		blk := [ | s | s := a + b. t := t + s].
		r1 := blk value.
		b := -100.
		r2 := blk value.
		r1 -> r2 -> t
	is numbered as
		method block 0 to: 6:
		| a b blk r1 r2 t |
		a w@1 := 1. b w@1 := 2. t w@1 := 0.
		blk w@5 := [entry@2 | s |
					 t  w@3 := t r@3 + a r@3 + b r@3
					] exit@4.
		r1 w@5 := blk r@5 value.
		b w@5 := nil.
		r2 w@5 := blk r@5 value.
		r1 r@5 -> r2 r@5 -> t r@5
	So:
		b and blk cannot be copied because for both there exists a write @5 that follows a
			read @4 within block 2 through 4
		t must be remote because there exists a write @3 within block (2 to: 4)
	Complications are introduced by optimized blocks.  In the following temp is written to
	after it is closed over by [ temp ] since the inlined block is executed more than once.
		| temp coll |
		coll := OrderedCollection new.
		1 to: 5 do: [ :index | 
			temp := index. 
			coll add: [ temp ] ].
		self assert: (coll collect: [:ea| ea value]) asArray = #(5 5 5 5 5)
	In the following i is local to the block and must be initialized each time around the loop
	but if the block is inlined it must be declared at method level.
		| col |
		col := OrderedCollection new.
		1 to: 3 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
		self assert: (col collect: [ :each | each value ]) asArray = #(2 3 4)"
	self assert: (arguments isEmpty or: [arguments hasEqualElements: methodArguments]).
	arguments := methodArguments asArray. "won't change"
	self assert: (temporaries isNil or: [temporaries isEmpty or: [temporaries hasEqualElements: methodTemporaries]]).
	temporaries := OrderedCollection withAll: methodTemporaries.

	self assert: optimized not. "the top-level block should not be optimized."
	self analyseTempsWithin: self rootNode: rootNode assignmentPools: Dictionary new.

	"The top-level block needs to reindex temporaries since analysis may have rearranged them.
	 This happens when temps are made remote and/or a remote node is added."
	temporaries withIndexDo:
		[:temp :offsetPlusOne| temp index: arguments size + offsetPlusOne - 1].

	"Answer the (possibly modified) sequence of temps."
	^temporaries asArray
</details>

#### BlockNode>>#temporaries: aCollectionOfTemporaries

Collection of TempVariableNodes


<details>
	<summary>See more</summary>
	
	temporaries: aCollectionOfTemporaries
	"Collection of TempVariableNodes"
	"RNG: deprecated, try to use #temporariesDeclaration: instead"
	
	temporaries := aCollectionOfTemporaries
</details>

#### BlockNode>>#arguments: aCollectionOfArguments

Decompile.


<details>
	<summary>See more</summary>
	
	arguments: aCollectionOfArguments 
	"Decompile."

	arguments := aCollectionOfArguments
</details>

#### BlockNode>>#addRemoteTemp: aTempVariableNode rootNode: rootNode

<MethodNode>


<details>
	<summary>See more</summary>
	
	addRemoteTemp: aTempVariableNode rootNode: rootNode "<MethodNode>"
	"Add aTempVariableNode to my actualScope's sequence of
	 remote temps.  If I am an optimized block then the actual
	 scope is my actualScopeIfOptimized, otherwise it is myself."
	remoteTempNode == nil ifTrue:
		[remoteTempNode := RemoteTempVectorNode new
								name: self remoteTempNodeName
								index: arguments size + temporaries size
								type: LdTempType
								scope: 0.
		 actualScopeIfOptimized
			ifNil:
				[self addTempNode: remoteTempNode.
				 remoteTempNode definingScope: self]
			ifNotNil: [actualScopeIfOptimized addHoistedTemps: { remoteTempNode }]].
	remoteTempNode addRemoteTemp: aTempVariableNode encoder: rootNode encoder.
	"use remove:ifAbsent: because the deferred analysis for optimized
	 loops can result in the temp has already been hoised into the root."
	self removeTempNode: aTempVariableNode ifAbsent: [
		self actualScope removeTempNode: aTempVariableNode ifAbsent: [self shouldNotHappen ]].
	^remoteTempNode
</details>

#### BlockNode>>#reindexingLocalsDo: aBlock encoder: encoderOrNil

Evaluate aBlock wih arguments, temporaries and copiedValues reindexed for their positions within the receiver's block, restoring the correct indices afterwards. If encoder is not nil remember the temps for this block's extent.


<details>
	<summary>See more</summary>
	
	reindexingLocalsDo: aBlock encoder: encoderOrNil
	"Evaluate aBlock wih arguments, temporaries and copiedValues reindexed for
	 their positions within the receiver's block, restoring the correct indices afterwards.
	 If encoder is not nil remember the temps for this block's extent."
	| tempIndices result tempsToReindex |
	self assert: copiedValues notNil.
	tempsToReindex := arguments asArray, copiedValues, temporaries.
	tempIndices := tempsToReindex collect: [:temp| temp index].
	tempsToReindex withIndexDo:
		[:temp :newIndex| temp index: newIndex - 1. self assert: temp index + 1 = newIndex].
	encoderOrNil ifNotNil:
		[encoderOrNil noteBlockExtent: blockExtent hasLocals: tempsToReindex].
	result := aBlock ensure:
				["Horribly pragmatic hack.  The copiedValues will have completely
				  unrelated indices within the closure method and sub-method.
				  Avoiding the effort of rebinding temps in the inner scope simply
				  update the indices to their correct ones during the generation of
				  the closure method and restore the indices immedately there-after."
				 tempsToReindex with: tempIndices do:
					[:temp :oldIndex| temp index: oldIndex. self assert: temp index = oldIndex]].
	^result
</details>

#### BlockNode>>#sizeCodeExceptLast: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeExceptLast: encoder
	| codeSize statement |
	codeSize := 0.
	1 to: statements size - 1 do: [ :i |
		 statement := statements at: i.
		 codeSize := codeSize + (statement sizeCodeForEffect: encoder)].
	^codeSize
</details>

#### BlockNode>>#temporaries

Collection of TempVariableNodes


<details>
	<summary>See more</summary>
	
	temporaries
	"Collection of TempVariableNodes"
	
	"RNG: implementation can be changed after adopting the use of temporariesDeclaration inst var.
	After that, the implementation for this message can be changed to:
	^ temporariesDeclaration allDeclaredVariableNodes
	
	Or we can analyze the senders and change the way we request the temporaries"
	
	^temporaries ifNil: [#()]
</details>

#### BlockNode>>#startOfLastStatement: anInteger

Note the source index of the start of the last full statement. The last full statement is the value answered by a block and hence the expression the debugger should display as the value of the block.


<details>
	<summary>See more</summary>
	
	startOfLastStatement: anInteger
	"Note the source index of the start of the last full statement.  The
	 last full statement is the value answered by a block and hence the
	 expression the debugger should display as the value of the block."
	startOfLastStatement := anInteger
</details>

#### BlockNode>>#statements: statementsCollection returns: returnBool

Decompile.


<details>
	<summary>See more</summary>
	
	statements: statementsCollection returns: returnBool 
	"Decompile."

	| returnLast |
	returnLast := returnBool.
	returns := false.
	statements := 
		(statementsCollection size > 1 
			and: [(statementsCollection at: statementsCollection size - 1) 
					isReturningIf])
				ifTrue: 
					[returnLast := false.
					statementsCollection allButLast]
				ifFalse: [statementsCollection size = 0
						ifTrue: [Array with: NodeNil]
						ifFalse: [statementsCollection]].
	arguments := #().
	temporaries := #().
	optimized := false.
	returnLast ifTrue: [self returnLast]
</details>

#### BlockNode>>#noteOptimizedIn: anOptimizedMessageNode

<details>
	<summary>See more</summary>
	
	noteOptimizedIn: anOptimizedMessageNode
	optimized := true.
	optimizedMessageNode := anOptimizedMessageNode
</details>

#### BlockNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder

	^self emitCodeForClosureValue: stack encoder: encoder
</details>

#### BlockNode>>#addTempNode: aTempVariableNode

Utilities for when we want to add some temporaries.


<details>
	<summary>See more</summary>
	
	addTempNode: aTempVariableNode
	"Utilities for when we want to add some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries add: aTempVariableNode
</details>

#### BlockNode>>#printTemporaries: tempSequence on: aStream doPrior: aBlock

Print any in-scope temporaries. If there are any evaluate aBlock prior to printing. Answer whether any temporaries were printed.


<details>
	<summary>See more</summary>
	
	printTemporaries: tempSequence on: aStream doPrior: aBlock
	"Print any in-scope temporaries.  If there are any evaluate aBlock
	 prior to printing.  Answer whether any temporaries were printed."
	| tempStream seen |
	tempSequence ifNil:
		[^false].
	tempStream := (String new: 16) writeStream.
	"This is for the decompiler which canmot work out which optimized block a particular temp is
	 local to and hence may produce diplicates as in
		expr ifTrue: [| aTemp | ...] ifFalse: [| aTemp | ...]"
	seen := Set new.
	tempSequence do:
		[:tempNode |
		tempNode isIndirectTempVector
			ifTrue:
				[tempNode remoteTemps do:
					[:tempVariableNode|
					 (tempVariableNode scope >= 0
					  and: [(seen includes: tempNode key) not]) ifTrue:
						[tempStream space; nextPutAll: (seen add: tempVariableNode key)]]]
			ifFalse:
				[(tempNode scope >= -1
				  and: ["This is for the decompiler which may create a block arg when converting
						a while into a to:do: but won't remove it form temporaries"
					   tempNode isBlockArg not
				  and: [(seen includes: tempNode key) not]]) ifTrue:
					[tempStream space; nextPutAll: (seen add: tempNode key)]]].
	tempStream position = 0 ifTrue:
		[^false].
	aBlock value.
	aStream nextPut: $|; nextPutAll: tempStream contents; space; nextPut: $|.
	^true
</details>

#### BlockNode>>#printWithClosureAnalysisStatementsOn: aStream indent: levelOrZero

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level := 1 max: levelOrZero.
	comment ifNotNil: [
		self printCommentOn: aStream indent: level.
		aStream newLineTab: level].
	len := shown := statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [shown := 1 max: shown - 1]
		ifFalse: [(len = 1 and: [((statements at: 1) == NodeNil) & (arguments size = 0)])
					ifTrue: [shown := shown - 1]].
	1 to: shown do: 
		[:i | 
		thisStatement := statements at: i.
		thisStatement printWithClosureAnalysisOn: aStream indent: level.
		i < shown ifTrue: [aStream nextPut: $.; newLineTab: level].
		(thisStatement comment notNil and: [thisStatement comment size > 0])
			ifTrue: [
				i = shown ifTrue: [aStream newLineTab: level].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [aStream newLineTab: level]]]
</details>

#### BlockNode>>#block

<details>
	<summary>See more</summary>
	
	block
	^ self
</details>

#### BlockNode>>#printStatementsOn: aStream indent: levelOrZero

<details>
	<summary>See more</summary>
	
	printStatementsOn: aStream indent: levelOrZero
	| len shown thisStatement level |
	level _ 1 max: levelOrZero.
	comment ifNotNil: [
		self printCommentOn: aStream indent: level.
		aStream newLineTab: level].
	statements isNil ifTrue: [^0].
	len _ shown _ statements size.
	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])
		ifTrue: [ shown _ 1 max: shown - 1]
		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."
				(arguments size = 0
				and: [ len >= 1
				and: [ (statements at: len) == NodeNil
				and: [ len = 1
					or: [ len > 1
						and: [(statements at: len - 1) isMessageNode
						and: [(statements at: len - 1) isNilIf ]]]]]])
					ifTrue: [ shown _ shown - 1 ]].
	1 to: shown do: 
		[ :i |
		thisStatement _ statements at: i.
		thisStatement printOn: aStream indent: level.
		i < shown ifTrue: [ aStream nextPut: $.; newLineTab: level ].
		"Add a final period. This helps when pretty-diffing a method and a version of it that adds stuff after the end."
		(i = shown and: [ levelOrZero = 0 ]) ifTrue: [ aStream nextPut: $. ].
		(thisStatement comment notNil and: [ thisStatement comment size > 0 ])
			ifTrue: [
				i = shown ifTrue: [ aStream newLineTab: level ].
				thisStatement printCommentOn: aStream indent: level.
				i < shown ifTrue: [ aStream newLineTab: level ]]].
	^shown
</details>

#### BlockNode>>#emitCodeForEvaluatedValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEvaluatedValue: stack encoder: encoder
	| position |
	position := stack position.
	self emitCodeExceptLast: stack encoder: encoder.
	statements last emitCodeForBlockValue: stack encoder: encoder.
	self assert: stack position - 1 = position
</details>

#### BlockNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	(statements isNil or: [arguments isNil or: [temporaries isNil]]) ifTrue: [^false].
	^ statements size > 1 or:
		[ (statements size = 1 and: [ statements first isComplex ]) or:
			[ arguments size > 0 or: [ temporaries notEmpty ] ] ]
</details>

#### BlockNode>>#nArgsSlot

Private for the Encoder to use in bindArg


<details>
	<summary>See more</summary>
	
	nArgsSlot
	"Private for the Encoder to use in bindArg"
	^nArgsNode
</details>

#### BlockNode>>#sizeCodeForEvaluatedEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEvaluatedEffect: encoder

	^self returns
		ifTrue: [self sizeCodeForEvaluatedValue: encoder]
		ifFalse: [(self sizeCodeExceptLast: encoder)
				+ (statements last sizeCodeForEffect: encoder)]
</details>

#### BlockNode>>#expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

the closure creation node already contains the source ranges including the [ ] and arguments declaration


<details>
	<summary>See more</summary>
	
	expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode
	"the closure creation node already contains the source ranges including the [ ] and arguments declaration"

	^ self consolidateAsCollection: (allSourceRanges at: closureCreationNode)
</details>

#### BlockNode>>#blockExtent

^<Interval>


<details>
	<summary>See more</summary>
	
	blockExtent "^<Interval>"
	^blockExtent
</details>

#### BlockNode>>#printWithClosureAnalysisArgumentsOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisArgumentsOn: aStream indent: level
	arguments size = 0 ifTrue: [^self].
	arguments do: [ :tempNode |
		aStream space; nextPut: $:.
		tempNode printDefinitionForClosureAnalysisOn: aStream].
	aStream nextPut: $|; space.
	"If >0 args and >1 statement, put all statements on separate lines"
	statements size > 1 ifTrue: [
		aStream newLineTab: level]
</details>

#### BlockNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	| effectiveScope blockStart |
	effectiveScope := optimized
						ifTrue: [actualScopeIfOptimized := scopeBlock]
						ifFalse: [self].

	arguments ifNotNil:
		[arguments do: [:temp| temp definingScope: self]].
	temporaries ifNotNil:
		[temporaries do: [:temp| temp definingScope: self]].

	optimized ifFalse: "if optimized this isn't an actual scope"
		[rootNode noteBlockEntry:
			[:entryNumber|
			 blockExtent := (blockStart := entryNumber) to: 0]].

	"Need to enumerate a copy because closure analysis can add a statement
	 via ifHasRemoteTempNodeEnsureInitializationStatementExists:."
	statements copy do:
		[:statement|
		 statement analyseTempsWithin: effectiveScope rootNode: rootNode assignmentPools: assignmentPools].

	optimized
		ifTrue: "if optimized loop need to add nils for any temps read before written"
			[optimizedMessageNode isOptimizedLoop ifTrue:
				[self nilReadBeforeWrittenTemps]]
		ifFalse: "if optimized this isn't an actual scope"
			[rootNode noteBlockExit:
				[:exitNumber|
				 blockExtent := blockStart to: exitNumber]].

	"Now that the analysis is done move any temps that need to be moved."
	self postNumberingProcessTempsWithin: effectiveScope rootNode: rootNode.

	"This is simply a nicety for compiler developers..."
	temporaries do:
		[:temp|
		(temp isIndirectTempVector and: [temp name includes: $?]) ifTrue:
			[temp name: temp definingScope remoteTempNodeName]]
</details>

#### BlockNode>>#isBlockNode

<details>
	<summary>See more</summary>
	
	isBlockNode
	^true
</details>

#### BlockNode>>#removeTempNode: aTempVariableNode ifAbsent: aBlock

Utilities for when we want to remove some temporaries.


<details>
	<summary>See more</summary>
	
	removeTempNode: aTempVariableNode ifAbsent: aBlock
	"Utilities for when we want to remove some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries remove: aTempVariableNode ifAbsent: aBlock
	
</details>

#### BlockNode>>#optimizedBlockHoistTempsInto: scopeBlock

<BlockNode>


<details>
	<summary>See more</summary>
	
	optimizedBlockHoistTempsInto: scopeBlock "<BlockNode>"
	"This is a No-op for all nodes except non-optimized BlockNodes."
	"Let's assume the special > 0 guard in MessageNode>>analyseTempsWithin:forValue:encoder: is correct.
	 Then we can simply hoist our temps up."
	self assert: (arguments isNil or: [arguments size <= 1]).
	(arguments notNil and: [arguments notEmpty]) ifTrue:
		[scopeBlock addHoistedTemps: arguments.
		arguments := #()].
	temporaries notEmpty ifTrue:
		[scopeBlock addHoistedTemps: temporaries.
		temporaries := #()]
</details>

#### BlockNode>>#ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode

If a remoteTempNode has been added ensure a statement exists to initialize it.


<details>
	<summary>See more</summary>
	
	ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode
	"If a remoteTempNode has been added ensure a statement exists to initialize it."
	remoteTempNode ~~ nil ifTrue:
		[(statements notEmpty
		  and: [statements first isAssignmentNode
		  and: [statements first variable isTemp
		  and: [statements first variable isIndirectTempVector]]])
			ifTrue: "If this is a decompiled tree, or if a temporary has been added later in
					the analysis then there already is a temp vector initialization node."
				[(statements first variable ~~ remoteTempNode) ifTrue:
					[statements first variable become: remoteTempNode].
				 statements first value numElements: remoteTempNode remoteTemps size]
			ifFalse:
				[statements addFirst: (remoteTempNode nodeToInitialize: rootNode encoder)]].
</details>

#### BlockNode>>#statements

<details>
	<summary>See more</summary>
	
	statements
	^statements
</details>

#### BlockNode>>#code

<details>
	<summary>See more</summary>
	
	code

	^statements first code
</details>

#### BlockNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isBlockNode
		and: [ self hasEquivalentArgumentsWith: aParseNode ]
		and: [ self hasEquivalentTemporariesDeclarationWith: aParseNode ]
		and: [ self hasEquivalentStatementsWith: aParseNode ]
</details>

#### BlockNode>>#sizeCodeForEvaluatedValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEvaluatedValue: encoder

	^(self sizeCodeExceptLast: encoder)
		+ (statements last sizeCodeForBlockValue: encoder)
</details>

#### BlockNode>>#nArgsSlot: anInteger

Private for the Encoder to use in bindArg


<details>
	<summary>See more</summary>
	
	nArgsSlot: anInteger
	"Private for the Encoder to use in bindArg"
	nArgsNode := anInteger
</details>

#### BlockNode>>#optimized

<details>
	<summary>See more</summary>
	
	optimized
	^optimized
</details>

#### BlockNode>>#emitCodeExceptLast: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeExceptLast: stack encoder: encoder
	| position nextToLast statement |
	position := stack position.
	nextToLast := statements size - 1.
	1 to: nextToLast do: [ :i |
		statement := statements at: i.
		statement emitCodeForEffect: stack encoder: encoder.
		self assert: stack position = position].
</details>

#### BlockNode>>#numberOfArguments

<details>
	<summary>See more</summary>
	
	numberOfArguments

	^arguments size
</details>

#### BlockNode>>#emitCodeForEvaluatedClosureValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEvaluatedClosureValue: stack encoder: encoder
	| position |
	position := stack position.
	stack position: arguments size + temporaries size + copiedValues size.
	encoder genPushNClosureTemps: temporaries size.
	self
		reindexingLocalsDo: [self emitCodeForEvaluatedValue: stack encoder: encoder]
		encoder: encoder.
	self returns ifFalse:
		[encoder genReturnTopToCaller.
		 pc := encoder methodStreamPosition].
	stack position: position
</details>

#### BlockNode>>#printsInNewLine

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	printsInNewLine
	"Used for pretty printing to determine whether to start a new line"

	Preferences prettyPrintRectangularBlocks ifFalse: [ ^false ].
	^super printsInNewLine
</details>

#### BlockNode>>#sizeCodeForEvaluatedClosureValue: encoder

The closure value primitives push the arguments and the copied values. The compiler guarantees that any copied values come before all local temps. So on closure activation we only need to push nils for the remaining temporaries.


<details>
	<summary>See more</summary>
	
	sizeCodeForEvaluatedClosureValue: encoder
	"The closure value primitives push the arguments and the copied values.
	 The compiler guarantees that any copied values come before all local temps.
	 So on closure activation we only need to push nils for the remaining temporaries."
	^(encoder sizePushNClosureTemps: temporaries size)
	+ (self
		reindexingLocalsDo: [self sizeCodeForEvaluatedValue: encoder]
		encoder: nil "don't store temps yet")
	+ (self returns ifTrue: [0] ifFalse: [encoder sizeReturnTopToCaller])
</details>

#### BlockNode>>#constructClosureCreationNode: encoder

<details>
	<summary>See more</summary>
	
	constructClosureCreationNode: encoder
	copiedValues := self computeCopiedValues: encoder rootNode.
	encoder supportsClosureOpcodes ifTrue:
		[^self closureCreationNode].
	"Without the bytecode we can still get by."
	^MessageNode new
		receiver: (encoder encodeVariable: 'thisContext')
		selector: #closureCopy:copiedValues:
		arguments: (Array
						with: (encoder encodeLiteral: arguments size)
						with: (copiedValues isEmpty
								ifTrue: [NodeNil]
								ifFalse: [BraceNode new elements: copiedValues]))
		precedence: 3
		from: encoder
</details>

#### BlockNode>>#temporariesDeclaration

<details>
	<summary>See more</summary>
	
	temporariesDeclaration

	^ temporariesDeclaration
</details>

#### BlockNode>>#postNumberingProcessTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode

<BlockNode>


<details>
	<summary>See more</summary>
	
	postNumberingProcessTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>"
	"A temp can be local (and copied) if it is not written to after it is captured.
	 A temp cannot be local if it is written to remotely.
	 Need to enumerate a copy of the temporaries because any temps becoming remote
	 will be removed from temporaries in analyseClosure: (and a single remote temp node
	 will get added)"
	temporaries copy do:
		[:each|
		each isIndirectTempVector ifFalse:
			[each analyseClosure: rootNode]].

	"If this is an optimized node we need to hoist temporaries up into the relevant block scope."
	optimized ifTrue:
		[self optimizedBlockHoistTempsInto: scopeBlock].

	"Now we may have added a remoteTempNode.  So we need a statement to initialize it."
	self ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode.

	"Now add all arguments and locals to the pool so that copiedValues can be computed during sizing."
	rootNode
		addLocalsToPool: arguments;
		addLocalsToPool: temporaries
</details>

#### BlockNode>>#printWithClosureAnalysisTemporariesOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisTemporariesOn: aStream indent: level

	(temporaries == nil or: [temporaries size = 0]) ifFalse: [
		aStream nextPut: $|.
		temporaries do: [ :tempNode |
			aStream space.
			tempNode printDefinitionForClosureAnalysisOn: aStream].
		aStream nextPutAll: ' | '.
		"If >0 args and >1 statement, put all statements on separate lines"
		statements size > 1 ifTrue: [aStream newLineTab: level]]
</details>

#### BlockNode>>#decompileString

Answer a string description of the parse tree whose root is the receiver.


<details>
	<summary>See more</summary>
	
	decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^ String streamContents: [:strm | self printOn: strm indent: 0]

</details>

#### BlockNode>>#returnNilIfNoOther

<details>
	<summary>See more</summary>
	
	returnNilIfNoOther

	self returns
		ifFalse: 
			[statements last == NodeNil ifFalse: [statements add: NodeNil].
			self returnLast]
</details>

#### BlockNode>>#emitCodeForEvaluatedEffect: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEvaluatedEffect: stack encoder: encoder
	| position |
	position := stack position.
	self returns
		ifTrue: 
			[self emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1]
		ifFalse: 
			[self emitCodeExceptLast: stack encoder: encoder.
			statements last emitCodeForEffect: stack encoder: encoder].
	self assert: stack position = position
</details>

#### BlockNode>>#addHoistedTemps: additionalTemporaries

<SequenceableCollection>


<details>
	<summary>See more</summary>
	
	addHoistedTemps: additionalTemporaries "<SequenceableCollection>"
	additionalTemporaries do:
		[:temp|
		temp definingScope ifNil:
			[temp definingScope: self]].
	temporaries := (temporaries isNil or: [temporaries isEmpty])
					ifTrue: [additionalTemporaries copy]
					ifFalse:
						[temporaries last isIndirectTempVector
							ifTrue: [temporaries allButLast, additionalTemporaries, { temporaries last }]
							ifFalse: [temporaries, additionalTemporaries]]
</details>

#### BlockNode>>#returnSelfIfNoOther: encoder

<details>
	<summary>See more</summary>
	
	returnSelfIfNoOther: encoder

	self returns ifTrue:[^self].
	statements last == NodeSelf ifFalse: [
		statements := statements copyWith: (encoder encodeVariable: 'self').
	].
	self returnLast.

</details>

#### BlockNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitBlockNode: self
</details>

#### BlockNode>>#statements: val

<details>
	<summary>See more</summary>
	
	statements: val
	statements := val
</details>

#### BlockNode>>#firstArgument

<details>
	<summary>See more</summary>
	
	firstArgument
	^ arguments first
</details>

#### BlockNode>>#hasArgumentOrTemporaryNamed: aName

<details>
	<summary>See more</summary>
	
	hasArgumentOrTemporaryNamed: aName

	^ self hasLocallyArgumentOrTemporaryNamed: aName
</details>

#### BlockNode>>#makeTemporariesRemovable

Utilities for when we want to remove some temporaries.


<details>
	<summary>See more</summary>
	
	makeTemporariesRemovable
	"Utilities for when we want to remove some temporaries."
	
	temporaries isArray ifTrue:
		[temporaries := temporaries asOrderedCollection].
</details>

## BraceNode

Used for compiling and decompiling brace constructs. These now compile into either a fast short form for 4 elements or less: Array braceWith: a with: b ... or a long form of indefinfite length: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray. The erstwhile brace assignment form is no longer supported.

### Methods
#### BraceNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level
	| isComplex useLevel |
	useLevel := level.
	isComplex := elements anySatisfy: [ :ea |
		ea isComplex ].
	isComplex ifTrue: [ useLevel := useLevel + 1 ].
	aStream nextPut: ${.
	1
		to: elements size
		do: [ :i |
			isComplex ifTrue: [ aStream newLineTab: (1 max: useLevel) ].
			(elements at: i)
				printOn: aStream
				indent: useLevel.
			i < elements size ifTrue: [ aStream nextPutAll: '. ' ]].
	isComplex ifTrue: [ aStream newLineTab: (1 max: level) ].
	aStream nextPut: $}.
</details>

#### BraceNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[ self elementsDo: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder
</details>

#### BraceNode>>#hasEquivalentElementsTo: aBraceNode

<details>
	<summary>See more</summary>
	
	hasEquivalentElementsTo: aBraceNode

	elements with: aBraceNode elements do: [ :myElement :otherElement |
		(myElement equivalentTo: otherElement) ifFalse: [ ^ false ] ].
	^ true
</details>

#### BraceNode>>#elements: collection sourceLocations: locations

Compile.


<details>
	<summary>See more</summary>
	
	elements: collection sourceLocations: locations
	"Compile."

	elements := collection.
	sourceLocations := locations
</details>

#### BraceNode>>#elements: collection

Decompile.


<details>
	<summary>See more</summary>
	
	elements: collection
	"Decompile."

	elements := collection
</details>

#### BraceNode>>#selectorForShortForm: nElements

<details>
	<summary>See more</summary>
	
	selectorForShortForm: nElements

	nElements > 4 ifTrue: [^ nil].
	^ #(braceWithNone braceWith: braceWith:with:
			braceWith:with:with: braceWith:with:with:with:) at: nElements + 1
</details>

#### BraceNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	^ true.
</details>

#### BraceNode>>#casesReverseDo: aBlock

For each case in reverse order, evaluate aBlock with three arguments: the key block, the value block, and whether it is the last case.


<details>
	<summary>See more</summary>
	
	casesReverseDo: aBlock
	"For each case in reverse order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	(numCases := elements size) to: 1 by: -1 do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]
</details>

#### BraceNode>>#numElements

<details>
	<summary>See more</summary>
	
	numElements

	^ elements size
</details>

#### BraceNode>>#maxElementsForConsArray

Hack; we have no way of knowing how much stack space is available during sizing


<details>
	<summary>See more</summary>
	
	maxElementsForConsArray
	"Hack; we have no way of knowing how much stack space is available during sizing"
	^8
</details>

#### BraceNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"

	self elementsDo: [ :node |
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools ]
</details>

#### BraceNode>>#elements

<details>
	<summary>See more</summary>
	
	elements
	^elements
</details>

#### BraceNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level

	aStream nextPut: ${.
	1 to: elements size do: 
		[:i | (elements at: i) printWithClosureAnalysisOn: aStream indent: level.
		i < elements size ifTrue: [aStream nextPutAll: '. ']].
	aStream nextPut: $}
</details>

#### BraceNode>>#isBraceNode

<details>
	<summary>See more</summary>
	
	isBraceNode

	^ true
</details>

#### BraceNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				MessageNode new
					receiver: (encoder encodeVariable: #Array)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				CascadeNode new
					receiver: (MessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | MessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (MessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder
</details>

#### BraceNode>>#matchBraceStreamReceiver: receiver messages: messages

<details>
	<summary>See more</summary>
	
	matchBraceStreamReceiver: receiver messages: messages

	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])
		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])
		ifFalse: [^ nil "no match"].

	"Appears to be a long form brace construct"
	self elements: (messages allButLast collect:
		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)
					ifFalse: [^ nil "not a brace element"].
		msg arguments first])
</details>

#### BraceNode>>#elementsDo: aBlock

<details>
	<summary>See more</summary>
	
	elementsDo: aBlock

	elements do: aBlock
</details>

#### BraceNode>>#blockAssociationCheck: encoder

If all elements are MessageNodes of the form [block]->[block], and there is at least one element, answer true. Otherwise, notify encoder of an error.


<details>
	<summary>See more</summary>
	
	blockAssociationCheck: encoder
	"If all elements are MessageNodes of the form [block]->[block], and there is at
	 least one element, answer true.
	 Otherwise, notify encoder of an error."

	elements size = 0
		ifTrue: [^encoder notify: 'At least one case required'].
	elements with: sourceLocations do:
			[:x :loc |
			(x 	isMessage: #->
				receiver:
					[:rcvr |
					rcvr isBlockNode and: [rcvr numberOfArguments = 0]]
				arguments:
					[:arg |
					arg isBlockNode and: [arg numberOfArguments = 0]])
			  ifFalse:
				[^encoder notify: 'Association between 0-argument blocks required' at: loc]].
	^true
</details>

#### BraceNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitBraceNode: self
</details>

#### BraceNode>>#casesForwardDo: aBlock

For each case in forward order, evaluate aBlock with three arguments: the key block, the value block, and whether it is the last case.


<details>
	<summary>See more</summary>
	
	casesForwardDo: aBlock
	"For each case in forward order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	1 to: (numCases := elements size) do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]
</details>

#### BraceNode>>#matchBraceWithReceiver: receiver selector: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	matchBraceWithReceiver: receiver selector: selector arguments: arguments

	selector = (self selectorForShortForm: arguments size)
		ifFalse: [^ nil "no match"].

	"Appears to be a short form brace construct"
	self elements: arguments
</details>

#### BraceNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isBraceNode and: [ self hasEquivalentElementsTo: aParseNode ]
</details>

## CascadeNode

The first message has the common receiver, the rest have receiver == nil, which signifies cascading.

### Methods
#### CascadeNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level
	self printOn: aStream indent: level precedence: 0
</details>

#### CascadeNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	receiver emitCodeForValue: stack encoder: encoder.
	1 to: messages size - 1 do: 
		[:i | 
		encoder genDup.
		stack push: 1.
		(messages at: i) emitCodeForValue: stack encoder: encoder.
		encoder genPop.
		stack pop: 1].
	messages last emitCodeForValue: stack encoder: encoder
</details>

#### CascadeNode>>#receiver: receivingObject messages: msgs

Transcript show: 'abc'; cr; show: 'def'


<details>
	<summary>See more</summary>
	
	receiver: receivingObject messages: msgs
	" Transcript show: 'abc'; cr; show: 'def' "

	receiver := receivingObject.
	messages := msgs
</details>

#### CascadeNode>>#expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

<details>
	<summary>See more</summary>
	
	expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ receiver expandRanges: (sourceRanges at: receiver) basedOn: sourceRanges using: sourceCode.
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode
</details>

#### CascadeNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	^ true
</details>

#### CascadeNode>>#receiver

<details>
	<summary>See more</summary>
	
	receiver
	^receiver
</details>

#### CascadeNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	{ receiver }, messages do:
		[:node| node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]
</details>

#### CascadeNode>>#messages

<details>
	<summary>See more</summary>
	
	messages
	^messages
</details>

#### CascadeNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level
	self printWithClosureAnalysisOn: aStream indent: level precedence: 0
</details>

#### CascadeNode>>#printOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level precedence: p
	p > 0 ifTrue: [ aStream nextPut: $( ].
	messages first
		printReceiver: receiver
		on: aStream
		indent: level.
	1
		to: messages size
		do: [ :i | 
			aStream newLineTab: level + 1.
			(messages at: i)
				printOn: aStream
				indent: level+1.
			i < messages size ifTrue: [ aStream nextPut:$; ] ].
	p > 0 ifTrue: [ aStream nextPut: $) ]
</details>

#### CascadeNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	| size |
	size := (receiver sizeCodeForValue: encoder)
			 + (messages size - 1 * (encoder sizeDup + encoder sizePop)).
	messages do: [:aMessage | size := size + (aMessage sizeCodeForValue: encoder)].
	^size
</details>

#### CascadeNode>>#hasEquivalentMessagesWith: aCascadeNode

<details>
	<summary>See more</summary>
	
	hasEquivalentMessagesWith: aCascadeNode

	messages with: aCascadeNode messages do: [ :myMessage :otherNodeMessage |
		(myMessage equivalentTo: otherNodeMessage) ifFalse: [ ^ false ] ].
	^ true
</details>

#### CascadeNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitCascadeNode: self
</details>

#### CascadeNode>>#isCascadeNode

<details>
	<summary>See more</summary>
	
	isCascadeNode

	^ true
</details>

#### CascadeNode>>#printWithClosureAnalysisOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level precedence: p 

	p > 0 ifTrue: [aStream nextPut: $(].
	messages first printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	1 to: messages size do: 
		[:i | (messages at: i) printWithClosureAnalysisOn: aStream indent: level.
		i < messages size ifTrue: 
				[aStream nextPut: $;.
				messages first precedence >= 2 ifTrue: [aStream newLineTab: level + 1]]].
	p > 0 ifTrue: [aStream nextPut: $)]
</details>

#### CascadeNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isCascadeNode
		and: [ receiver equivalentTo: aParseNode receiver ]
		and: [ self hasEquivalentMessagesWith: aParseNode ]
</details>

## CodeNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### CodeNode>>#arguments

<details>
	<summary>See more</summary>
	
	arguments

	self subclassResponsibility 
</details>

#### CodeNode>>#isLocalArgumentOrTemporary: aParseNode

Looks only in this scope - Hernan


<details>
	<summary>See more</summary>
	
	isLocalArgumentOrTemporary: aParseNode

	"Looks only in this scope - Hernan"
	^(self temporaries includes: aParseNode) or: [ self arguments includes: aParseNode ]
	
</details>

#### CodeNode>>#block

<details>
	<summary>See more</summary>
	
	block

	self subclassResponsibility 
</details>

#### CodeNode>>#temporariesDeclaration: aTemporariesDeclarationNode

<details>
	<summary>See more</summary>
	
	temporariesDeclaration: aTemporariesDeclarationNode

	self subclassResponsibility
</details>

#### CodeNode>>#hasTemporaryVariables

<details>
	<summary>See more</summary>
	
	hasTemporaryVariables

	^ self temporariesDeclaration declaresAnyVariable
</details>

#### CodeNode>>#temporariesDeclaration

<details>
	<summary>See more</summary>
	
	temporariesDeclaration

	self subclassResponsibility
</details>

#### CodeNode>>#hasLocallyArgumentOrTemporaryNamed: aVariableName

- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes named aVariableName - hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes temps, arguments, instance variables and pseudo-variables - hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument named aVariableName. - Hernan


<details>
	<summary>See more</summary>
	
	hasLocallyArgumentOrTemporaryNamed: aVariableName

	"- hasArgumentOrTemporaryNamed: returns true if there is a temp or variable in this scope or subscopes 
	   named aVariableName
	- hasLocalName: returns true if there is a variable in the scopeTable name aVariableName. That includes 
	  temps, arguments, instance variables and pseudo-variables
	- hasLocallyArgumentOrTemporaryNamed: returns true if ony this scope defines a temp or argument 
	  named aVariableName. - Hernan" 
	
	^(self hasLocallyArgumentNamed: aVariableName)
		or: [self hasLocallyTemporaryNamed: aVariableName]
</details>

#### CodeNode>>#hasNodeIn: aCollectionOfNodes named: aName

<details>
	<summary>See more</summary>
	
	hasNodeIn: aCollectionOfNodes named: aName

	^aCollectionOfNodes anySatisfy: [ :tempNode | tempNode isNamed: aName ]
</details>

#### CodeNode>>#temporaries: aCollectionOfTemporaries

<details>
	<summary>See more</summary>
	
	temporaries: aCollectionOfTemporaries

	self subclassResponsibility 
</details>

#### CodeNode>>#decompileString

<details>
	<summary>See more</summary>
	
	decompileString

	self subclassResponsibility 
</details>

#### CodeNode>>#hasLocallyArgumentNamed: aVariableName

<details>
	<summary>See more</summary>
	
	hasLocallyArgumentNamed: aVariableName
		
	^self hasNodeIn: self arguments named: aVariableName
</details>

#### CodeNode>>#arguments: aCollectionOfArguments

<details>
	<summary>See more</summary>
	
	arguments: aCollectionOfArguments

	self subclassResponsibility 
</details>

#### CodeNode>>#hasLocallyTemporaryNamed: aVariableName

<details>
	<summary>See more</summary>
	
	hasLocallyTemporaryNamed: aVariableName

	^self hasNodeIn: self temporaries named: aVariableName 
</details>

#### CodeNode>>#temporaries

<details>
	<summary>See more</summary>
	
	temporaries

	self subclassResponsibility 
</details>

#### CodeNode>>#hasEquivalentArgumentsWith: aCodeNode

<details>
	<summary>See more</summary>
	
	hasEquivalentArgumentsWith: aCodeNode

	self arguments with: aCodeNode arguments do: [ :myArgument :otherCodeNodeArgument |
		(myArgument equivalentTo: otherCodeNodeArgument) ifFalse: [ ^ false ] ].
	^ true
</details>

#### CodeNode>>#hasEquivalentTemporariesDeclarationWith: aCodeNode

<details>
	<summary>See more</summary>
	
	hasEquivalentTemporariesDeclarationWith: aCodeNode

	^ (self temporariesDeclaration isNil and: [ aCodeNode temporariesDeclaration isNil ])
		or: [ self temporariesDeclaration equivalentTo: aCodeNode temporariesDeclaration ]
</details>

## InstanceVariableNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### InstanceVariableNode>>#name: varName index: varIndex

<details>
	<summary>See more</summary>
	
	name: varName index: varIndex
	^self name: varName index: varIndex-1 type: LdInstType
</details>

#### InstanceVariableNode>>#isInstanceVariableNode

<details>
	<summary>See more</summary>
	
	isInstanceVariableNode

	^true
</details>

#### InstanceVariableNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	stack push: 1.
	^encoder genPushInstVar: index
</details>

#### InstanceVariableNode>>#emitCodeForStore: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStore: stack encoder: encoder
	encoder genStoreInstVar: index
</details>

#### InstanceVariableNode>>#sizeCodeForStorePop: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	^encoder sizeStorePopInstVar: index
</details>

#### InstanceVariableNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	^encoder sizePushInstVar: index
</details>

#### InstanceVariableNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitInstanceVariableNode: self
</details>

#### InstanceVariableNode>>#sizeCodeForStore: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStore: encoder
	^encoder sizeStoreInstVar: index
</details>

#### InstanceVariableNode>>#emitCodeForStorePop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVar: index.
	stack pop: 1
</details>

## LeafNode

I represent a leaf node of the compiler parse tree. I am abstract. Types (defined in class ParseNode): 1 LdInstType (which uses class VariableNode) 2 LdTempType (which uses class VariableNode) 3 LdLitType (which uses class LiteralNode) 4 LdLitIndType (which uses class VariableNode) 5 SendType (which uses class SelectorNode). Note that Squeak departs slightly from the Blue Book bytecode spec. In order to allow access to more than 63 literals and instance variables, bytecode 132 has been redefined as DoubleExtendedDoAnything: byte2 byte3 Operation (hi 3 bits) (lo 5 bits) 0 nargs lit index Send Literal Message 0-255 1 nargs lit index Super-Send Lit Msg 0-255 2 ignored rcvr index Push Receiver Variable 0-255 3 ignored lit index Push Literal Constant 0-255 4 ignored lit index Push Literal Variable 0-255 5 ignored rcvr index Store Receiver Variable 0-255 6 ignored rcvr index Store-pop Receiver Variable 0-255 7 ignored lit index Store Literal Variable 0-255 This has allowed bytecode 134 also to be redefined as a second extended send that can access literals up to 64 for nargs up to 3 without needing three bytes. It is just like 131, except that the extension byte is aallllll instead of aaalllll, where aaa are bits of argument count, and lll are bits of literal index.

### Methods
#### LeafNode>>#emitCodeForEffect: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEffect: stack encoder: encoder

	^self
</details>

#### LeafNode>>#sizeCodeForEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEffect: encoder

	^0
</details>

#### LeafNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ self class = aParseNode class and: [ self key = aParseNode key ]
</details>

#### LeafNode>>#name: ignored key: object code: byte

<details>
	<summary>See more</summary>
	
	name: ignored key: object code: byte

	key := object.
	code := byte
</details>

#### LeafNode>>#sizeCodeForLoad: encoder forValue: forValue

Default is to do nothing. Subclasses may need to override.


<details>
	<summary>See more</summary>
	
	sizeCodeForLoad: encoder forValue: forValue
	"Default is to do nothing.
	 Subclasses may need to override."
	^0
</details>

#### LeafNode>>#emitCodeForLoad: stack forValue: forValue encoder: encoder

Default is to do nothing. Subclasses may need to override.


<details>
	<summary>See more</summary>
	
	emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Default is to do nothing.
	 Subclasses may need to override."
</details>

#### LeafNode>>#key: object index: i type: type

<details>
	<summary>See more</summary>
	
	key: object index: i type: type

	key := object.
	code := (self code: i type: type).
	index := i
</details>

#### LeafNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self
</details>

#### LeafNode>>#key: object code: byte

<details>
	<summary>See more</summary>
	
	key: object code: byte

	key := object.
	code := byte
</details>

#### LeafNode>>#code: idx type: type

Warning: index would be shadowed


<details>
	<summary>See more</summary>
	
	code: idx type: type
	"Warning: index would be shadowed"
	idx ifNil: [
		^type negated].
	(CodeLimits at: type) > idx 
		ifTrue: [^(CodeBases at: type) + idx].
	^type * 256 + idx
</details>

#### LeafNode>>#key: aKey

<details>
	<summary>See more</summary>
	
	key: aKey

	key := aKey 
</details>

#### LeafNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	self subclassResponsibility
</details>

#### LeafNode>>#reserve: encoder

If this is a yet unused literal of type -code, reserve it.


<details>
	<summary>See more</summary>
	
	reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder litIndex: key) type: 0 - code]
</details>

#### LeafNode>>#key

<details>
	<summary>See more</summary>
	
	key

	^key
</details>

#### LeafNode>>#code

<details>
	<summary>See more</summary>
	
	code

	^ code
</details>

## LiteralNode

I am a parse tree leaf representing a literal string or number.

### Methods
#### LiteralNode>>#isLiteralNode

<details>
	<summary>See more</summary>
	
	isLiteralNode

	^ true
</details>

#### LiteralNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level
	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream
						nextPutAll: '###';
						nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream
						nextPutAll: '##';
						nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ | isComplex |
					isComplex := false.
					key isArray ifTrue: [
						isComplex := key anySatisfy: [ :ea |
							ea isArray ]].
					"Is it complex? (i.e. array of arrays)"
					isComplex
						ifTrue: [
							aStream
								nextPut: $#;
								nextPut: $(.
							key do: [ :ea |
								aStream newLineTab: (1 max: level + 1).
								ea storeOn: aStream ].
							aStream newLineTab: (1 max: level).
							aStream nextPut: $) ]
						ifFalse: [ key storeOn: aStream ]]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $` ]]
</details>

#### LiteralNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	stack push: 1.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 encoder genPushSpecialLiteral: specialLiteral])
		ifFalse:
			[encoder genPushLiteral: index]
</details>

#### LiteralNode>>#name: literal key: object index: i type: type

For compatibility with Encoder>>name:key:class:type:set:


<details>
	<summary>See more</summary>
	
	name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
</details>

#### LiteralNode>>#isConstantNumber

Overridden in LiteralNode


<details>
	<summary>See more</summary>
	
	isConstantNumber
	^ key isNumber
</details>

#### LiteralNode>>#literalValue

<details>
	<summary>See more</summary>
	
	literalValue

	^key
</details>

#### LiteralNode>>#eval

When everything in me is a constant, I can produce a value. This is only used by the Scripting system (TilePadMorph tilesFrom:in:)


<details>
	<summary>See more</summary>
	
	eval
	"When everything in me is a constant, I can produce a value.  This is only used by the Scripting system (TilePadMorph tilesFrom:in:)"

	^ key
</details>

#### LiteralNode>>#isSpecialConstant

<details>
	<summary>See more</summary>
	
	isSpecialConstant
	^ code between: LdTrue and: LdMinus1+3
</details>

#### LiteralNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level

	key isVariableBinding
		ifTrue:
			[key key isNil
				ifTrue:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifFalse:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]
</details>

#### LiteralNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	self reserve: encoder.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder sizePushSpecialLiteral: specialLiteral])
		ifFalse:
			[^encoder sizePushLiteral: index]
</details>

#### LiteralNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitLiteralNode: self
</details>

#### LiteralNode>>#reserve: encoder

If this is a yet unused literal of type -code, reserve it.


<details>
	<summary>See more</summary>
	
	reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue:
		[index := key isVariableBinding "true if sending value[:] to a special binding"
					ifTrue: [encoder sharableLitIndex: key]
					ifFalse: [encoder litIndex: key].
		 code := self code: index type: 0 - code]
</details>

#### LiteralNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isLiteralNode and: [ super equivalentTo: aParseNode ]
</details>

## LiteralVariableNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### LiteralVariableNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	^readNode
		ifNil: [stack push: 1.
			encoder genPushLiteralVar: index]
		ifNotNil: [readNode emitCodeForValue: stack encoder: encoder]
</details>

#### LiteralVariableNode>>#emitCodeForStore: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStore: stack encoder: encoder
	| exprOffset |
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop.
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver.  If they
	 did we could generate much simpler code, e.g.
		encoder genPushLiteral: index.
		stack push: 1.
		writeNode emitCode: stack args: 1 encoder: encoder super: false"
	exprOffset := stack position - 1.
	encoder genPushLiteral: index.
	stack push: 1.
	encoder genPushTempLong: exprOffset.
	stack push: 1.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop
</details>

#### LiteralVariableNode>>#sizeCodeForStorePop: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding])
		ifTrue: [	writeNode := encoder encodeSelector: #value:.
				^ (writeNode sizeCode: encoder args: 1 super: false)
	  			+ encoder sizePop]
		ifFalse: [encoder sizeStorePopLiteralVar: index]
</details>

#### LiteralVariableNode>>#assignmentCheck: encoder at: location

For messageNodes masquerading as variables for the debugger. For now we let this through - ie we allow stores ev into args. Should check against numArgs, though.


<details>
	<summary>See more</summary>
	
	assignmentCheck: encoder at: location
	^(key isVariableBinding and: [key canAssign not])
		ifTrue: [location]
		ifFalse: [-1]
</details>

#### LiteralVariableNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialReadBinding]) 
		ifFalse:
			[^encoder sizePushLiteralVar: index].
	readNode := MessageNode new 
		receiver: (encoder encodeLiteral: key)
		selector: (encoder encodeSelector: #value)
		arguments: #()
		precedence: #value precedence.
	^readNode sizeCodeForValue: encoder
</details>

#### LiteralVariableNode>>#isLiteralVariableNode

<details>
	<summary>See more</summary>
	
	isLiteralVariableNode

	^ true
</details>

#### LiteralVariableNode>>#sizeCodeForLoad: encoder forValue: forValue

Default is to do nothing. Subclasses may need to override.


<details>
	<summary>See more</summary>
	
	sizeCodeForLoad: encoder forValue: forValue
	self reserve: encoder.
	^(key isVariableBinding and: [key isSpecialWriteBinding and: [forValue not]])
		ifTrue: [encoder sizePushLiteral: index]
		ifFalse: [0]
</details>

#### LiteralVariableNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitLiteralVariableNode: self
</details>

#### LiteralVariableNode>>#sizeCodeForStore: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStore: encoder
	self reserve: encoder.
	(key isVariableBinding and: [key isSpecialWriteBinding]) ifFalse:
		[^encoder sizeStoreLiteralVar: index].
	writeNode := encoder encodeSelector: #value:.
	"On entry the stack has only the expression.  Push the binding,
	 duplicate the expression, send #value: and pop."
	^(encoder sizePushLiteral: index)
	  + (encoder sizePushTempLong: 0) "we don't know yet, hence long, sigh..."
	  + (writeNode sizeCode: encoder args: 1 super: false)
	  + encoder sizePop
</details>

#### LiteralVariableNode>>#emitCodeForStorePop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false.
	stack pop: 1.
	encoder genPop
</details>

#### LiteralVariableNode>>#emitCodeForLoad: stack forValue: forValue encoder: encoder

If a normal literal variable (not sending value:), do nothing. If for value (e.g. v := Binding := expr) do nothing; the work will be done in emitCodeForStore:encoder:. If not for value then indeed load. The rest of the work will be done in emitCodeForStorePop:encoder:.


<details>
	<summary>See more</summary>
	
	emitCodeForLoad: stack forValue: forValue encoder: encoder
	"If a normal literal variable (not sending value:), do nothing.
	 If for value (e.g. v := Binding := expr) do nothing; the work will be done in emitCodeForStore:encoder:.
	 If not for value then indeed load.  The rest of the work will be done in  emitCodeForStorePop:encoder:."
	(writeNode isNil or: [forValue]) ifTrue: [^self].
	encoder genPushLiteral: index.
	stack push: 1
</details>

## MaybeContextInstanceVariableNode

This class conspires to arrange that inst var access for contexts is done exclusively using the long-form instance variabl;e access bytecodes. See InstructionStream class>>variablesAndOffsetsDo:. A virtual machine can benefit in performance by organizing method and block activations using a more conventional stack organization than by using first-class activation records (contexts). But such a virtual machine is also cabable of hiding the stack and making it appear as if contexts are still used. This means the system has better performance but still has all the benefits of first-class activation records. To pull this off the VM needs to intercept any and all accesses to context objects so that it can make contexts function as proxy objects for stack frames. Without help from the image such a virtual machine based on an interpreter would have to perform an expensive check on all instance variable accesses to determine if the instance variable was that of a context serving as a proxy for a stack frame. A simple hack is to take advantage of the short and long forms of instance variable access bytecodes. The BlueBook instruction set (and likely any bytecode set evolved from it) has short form bytecodes for fetching and storing the first few bytecodes (BlueBook fetch first 16, store first 8). Contexts typically have at most 6 instance variables. If we arrange to use the long-form bytecodes for all context inst var accesses then we only have to check for context inst var access in long-form bytecodes, and then only if the index is within the context inst var range. This effectively makes the check free because on modern processors checking an index fetched from memory into a register against a constant costs far less than the memry read to fetch the index.

### Methods
#### MaybeContextInstanceVariableNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	stack push: 1.
	^encoder genPushInstVarLong: index
</details>

#### MaybeContextInstanceVariableNode>>#emitCodeForStore: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStore: stack encoder: encoder
	encoder genStoreInstVarLong: index
</details>

#### MaybeContextInstanceVariableNode>>#sizeCodeForStorePop: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	^encoder sizeStorePopInstVarLong: index
</details>

#### MaybeContextInstanceVariableNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	^encoder sizePushInstVarLong: index
</details>

#### MaybeContextInstanceVariableNode>>#sizeCodeForStore: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStore: encoder
	^encoder sizeStoreInstVarLong: index
</details>

#### MaybeContextInstanceVariableNode>>#code

Answer a bogus code to avoid creating quick methods. See MethodNode>>generate:ifQuick:


<details>
	<summary>See more</summary>
	
	code
	"Answer a bogus code to avoid creating quick methods.
	 See MethodNode>>generate:ifQuick:"
	^LoadLong
</details>

#### MaybeContextInstanceVariableNode>>#emitCodeForStorePop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVarLong: index.
	stack pop: 1
</details>

## MessageAsTempNode

This node represents accesses to temporary variables for do-its in the debugger. Since they execute in another context, they must send a message to the original context to access the value of the temporary variable in that context.

### Methods
#### MessageAsTempNode>>#sizeCodeForStorePop: encoder

This node has the form {expr storeAt: offset inTempFrame: homeContext}, where the expr, the block argument, is already on the stack.


<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self sizeCodeForEffect: encoder
</details>

#### MessageAsTempNode>>#store: expr from: encoder

ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment). For assigning into temps of a context being debugged.


<details>
	<summary>See more</summary>
	
	store: expr from: encoder 
	"ctxt tempAt: n -> ctxt tempAt: n put: expr (see Assignment).
	For assigning into temps of a context being debugged."

	selector key ~= #namedTempAt: ifTrue: [^self error: 'cant transform this message'].
	
	^ MessageAsTempNode new
		receiver: receiver
		selector: #namedTempAt:put:
		arguments: (arguments copyWith: expr)
		precedence: precedence
		from: encoder
</details>

#### MessageAsTempNode>>#emitCodeForStorePop: stack encoder: encoder

This node has the form {expr storeAt: offset inTempFrame: homeContext}, where the expr, the block argument, is already on the stack.


<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	"This node has the form {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack."
	^self emitCodeForEffect: stack encoder: encoder
</details>

#### MessageAsTempNode>>#asStorableNode: encoder

This node is a message masquerading as a temporary variable. It currently has the form {homeContext tempAt: offset}. We need to generate code for {expr storeAt: offset inTempFrame: homeContext}, where the expr, the block argument, is already on the stack. This, in turn will get turned into {homeContext tempAt: offset put: expr} at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)


<details>
	<summary>See more</summary>
	
	asStorableNode: encoder
	"This node is a message masquerading as a temporary variable.
	It currently has the form {homeContext tempAt: offset}.
	We need to generate code for {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack.
	This, in turn will get turned into {homeContext tempAt: offset put: expr}
	at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)"
	^ MessageAsTempNode new
		receiver: nil  "suppress code generation for receiver already on stack"
		selector: #storeAt:inTempFrame:
		arguments: (arguments copyWith: receiver)
		precedence: precedence
		from: encoder
</details>

#### MessageAsTempNode>>#code

Allow synthetic temp nodes to be sorted by code


<details>
	<summary>See more</summary>
	
	code
	"Allow synthetic temp nodes to be sorted by code"
	^ arguments first literalValue
</details>

## MessageNode

I represent a receiver and its message. Precedence codes: 1 unary 2 binary 3 keyword 4 other If special>0, I compile special code in-line instead of sending messages with literal methods as remotely copied contexts.

### Methods
#### MessageNode>>#arguments

<details>
	<summary>See more</summary>
	
	arguments
	^arguments
</details>

#### MessageNode>>#originalArguments

<details>
	<summary>See more</summary>
	
	originalArguments

	^ originalArguments
</details>

#### MessageNode>>#receiver

<details>
	<summary>See more</summary>
	
	receiver
	^receiver
</details>

#### MessageNode>>#printRepeatOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printRepeatOn: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array new)
		on: aStream indent: level
</details>

#### MessageNode>>#compare: myArguments with: othersArguments

<details>
	<summary>See more</summary>
	
	compare: myArguments with: othersArguments

	myArguments with: othersArguments do: [ :myArgument :otherArgument |
		(myArgument equivalentTo: otherArgument) ifFalse: [ ^ false ] ].
	^ true
</details>

#### MessageNode>>#keywordPositionAt: anIndex

<details>
	<summary>See more</summary>
	
	keywordPositionAt: anIndex

	^keywordRanges at: anIndex 
</details>

#### MessageNode>>#isMessageNode

<details>
	<summary>See more</summary>
	
	isMessageNode
	^true
</details>

#### MessageNode>>#emitCodeForIf: stack encoder: encoder value: forValue

<details>
	<summary>See more</summary>
	
	emitCodeForIf: stack encoder: encoder value: forValue
	| thenExpr thenSize elseExpr elseSize |
	thenSize := sizes at: 1.
	elseSize := sizes at: 2.
	(forValue not and: [elseSize * thenSize > 0]) ifTrue:
		"Two-armed IFs forEffect share a single pop"
		[^super emitCodeForEffect: stack encoder: encoder].
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
			pc := encoder methodStreamPosition.
			thenExpr emitCodeForEvaluatedValue: stack encoder: encoder.
			stack pop: 1.  "then and else alternate; they don't accumulate"
			thenExpr returns not ifTrue:
				"...not ifTrue: avoids using ifFalse: alone during this compile)"
				"Elide jump over else after a return"
				[self emitCodeForJump: elseSize encoder: encoder].
			elseExpr emitCodeForEvaluatedValue: stack encoder: encoder]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[thenSize > 0
				ifTrue:
					[self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					thenExpr emitCodeForEvaluatedEffect: stack encoder: encoder]
				ifFalse:
					[self emitCodeForBranchOn: true dist: elseSize pop: stack encoder: encoder.
					pc := encoder methodStreamPosition.
					elseExpr emitCodeForEvaluatedEffect: stack encoder: encoder]]
</details>

#### MessageNode>>#printWithClosureAnalysisOn: aStream indent: level

may not need this check anymore - may be fixed by the #receiver: change


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	self printWithClosureAnalysisKeywords: selector key
		 arguments: arguments
		 on: aStream
		 indent: level
</details>

#### MessageNode>>#printOn: strm indent: level precedence: outerPrecedence

<details>
	<summary>See more</summary>
	
	printOn: strm indent: level precedence: outerPrecedence

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printOn: strm indent: level]
</details>

#### MessageNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	| total |
	special > 0 
		ifTrue: 
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: true].
	receiver == NodeSuper
		ifTrue: [selector := selector copy "only necess for splOops"].
	total := selector sizeCode: encoder args: arguments size super: receiver == NodeSuper.
	receiver == nil 
		ifFalse: [total := total + (receiver sizeCodeForValue: encoder)].
	sizes := arguments collect: 
					[:arg | | argSize | 
					argSize := arg sizeCodeForValue: encoder.
					total := total + argSize.
					argSize].
	^total
</details>

#### MessageNode>>#printParenReceiver: rcvr on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printParenReceiver: rcvr on: aStream indent: level
					
	rcvr isBlockNode ifTrue:
		[^rcvr printOn: aStream indent: level].
	aStream nextPut: $(.
	rcvr printOn: aStream indent: level.
	aStream nextPut: $)

</details>

#### MessageNode>>#transformIfTrueIfFalse: encoder

<details>
	<summary>See more</summary>
	
	transformIfTrueIfFalse: encoder
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]]
</details>

#### MessageNode>>#transformRepeat: encoder

answer true if this #repeat message can be optimized


<details>
	<summary>See more</summary>
	
	transformRepeat: encoder
	"answer true if this #repeat message can be optimized"
	
	^(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0)
	   and: [receiver noteOptimizedIn: self.
			true]
</details>

#### MessageNode>>#printWithClosureAnalysisCaseOn: aStream indent: level

receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisCaseOn: aStream indent: level 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printWithClosureAnalysisOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printWithClosureAnalysisOn: aStream indent: level]
		ifFalse: 
			[aStream nextPutAll: '{'; newLineTab: level + 1.
			 braceNode casesForwardDo:
				[:keyNode :valueNode :last | 
				keyNode printWithClosureAnalysisOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode printsInNewLine
					ifTrue: 
						[aStream newLineTab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printWithClosureAnalysisOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 newLineTab: level + 1]]].
	otherwise ifNotNil: [
		aStream newLineTab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise printsInNewLine
					ifTrue: [
						aStream newLineTab: level + 2.
						1]
					ifFalse: [0].
		 otherwise printWithClosureAnalysisOn: aStream indent: level + 1 + extra]
</details>

#### MessageNode>>#transformToDo: encoder

var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2:


<details>
	<summary>See more</summary>
	
	transformToDo: encoder
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| limit increment block initStmt test incStmt limitInit blockVar myRange blockRange limitIsAssignedTo |
	block := arguments last.
	"First check for valid arguments"
	(block notNil
	 and: [block isBlockNode
	 and: [block numberOfArguments = 1
	 and: [block firstArgument isVariableReference "As with debugger remote vars"]]]) ifFalse:
		[^false].
	arguments size = 3
		ifTrue: [increment := arguments at: 2.
				(increment isConstantNumber
				 and: [increment literalValue ~= 0]) ifFalse: [^false]]
		ifFalse: [increment := encoder encodeLiteral: 1].
	(limit := arguments at: 1) isVariableReference ifTrue:
		[limitIsAssignedTo := false.
		 block nodesDo:
			[:node|
			(node isAssignmentNode and: [node variable = limit]) ifTrue:
				[limitIsAssignedTo := true]].
		 limitIsAssignedTo ifTrue:
			[^false]].
	arguments size < 3 ifTrue:   "transform to full form"
		[selector := SelectorNode new key: #to:by:do: code: #macro].

	"Now generate auxiliary structures"
	myRange := encoder rawSourceRanges at: self ifAbsent: [1 to: 0].
	blockRange := encoder rawSourceRanges at: block ifAbsent: [1 to: 0].
	blockVar := block firstArgument.
	initStmt := AssignmentNode new variable: blockVar value: receiver.
	limit isVariableReference | limit isConstantNumber
		ifTrue: [limitInit := nil]
		ifFalse:  "Need to store limit in a var"
			[limit := encoder bindBlockArg: blockVar key, 'LimiT' within: block.
			 limit scope: -2.  "Already done parsing block; flag so it won't print"
			 block addArgument: limit.
			 limitInit := AssignmentNode new
							variable: limit
							value: arguments first].
	test := MessageNode new
				receiver: blockVar
				selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])
				arguments: {limit}
				precedence: precedence
				from: encoder
				sourceRange: (myRange first to: blockRange first).
	incStmt := AssignmentNode new
				variable: blockVar
				value: (MessageNode new
							receiver: blockVar selector: #+
							arguments: {increment}
							precedence: precedence
							from: encoder
							sourceRange: (myRange last to: (myRange last max: blockRange last)))
				from: encoder
				sourceRange: (myRange last to: (myRange last max: blockRange last)).
	arguments := {limit. increment. block. initStmt. test. incStmt. limitInit}.
	block noteOptimizedIn: self.
	^true
</details>

#### MessageNode>>#keywordAndParameterPositionAt: anIndex encodedWith: anEncoder ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	keywordAndParameterPositionAt: anIndex encodedWith: anEncoder ifAbsent: aBlock

	| keywordPosition parameterLastPosition |

	keywordPosition := keywordRanges at: anIndex.
	parameterLastPosition := anIndex = arguments size
		ifTrue: [ (anEncoder rangeForNode: self ifAbsent: aBlock) last ]
		ifFalse: [ (keywordRanges at: anIndex + 1) first - 1].

	^keywordPosition first to: parameterLastPosition
</details>

#### MessageNode>>#printKeywords: key arguments: args on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue: [
		receiver ifNotNil: [ aStream space ].
		aStream nextPutAll: key.
		^ self ].
	keywords _ key keywords.
	doCrTab _ args size > 1.
	1
		to: (args size min: keywords size)
		do: [ :i |
			arg _ args at: i.
			kwd _ keywords at: i.
			doCrTab
				ifTrue: [
					aStream newLineTab: level + 1.
					indent _ 1
					"newline after big args" ]
				ifFalse: [
					receiver ifNotNil: [ aStream space ].
					indent _ 0 ].
			aStream nextPutAll: kwd.
			arg printsInNewLine
				ifTrue: [
					aStream newLineTab: level + indent + 1 ]
				ifFalse: [
					aStream space ].
			arg
				printOn: aStream
				indent: level + 1 + indent
				precedence:
					(precedence = 2
						ifTrue: [ 1 ]
						ifFalse: [ precedence ]) ].
</details>

#### MessageNode>>#toDoFromWhileWithInit: initStmt

Return nil, or a to:do: expression equivalent to this whileTrue:


<details>
	<summary>See more</summary>
	
	toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: (Array with: variable).
	variable scope: -1.
	variable beBlockArg.
	^MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: (Array with: limit with: increment with: toDoBlock)
		precedence: precedence
</details>

#### MessageNode>>#transformBoolean: encoder

<details>
	<summary>See more</summary>
	
	transformBoolean: encoder
	^self
		checkBlock: (arguments at: 1)
		as: 'argument'
		from: encoder
		maxArgs: 0
</details>

#### MessageNode>>#printCaseOn: aStream indent: level

receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]


<details>
	<summary>See more</summary>
	
	printCaseOn: aStream indent: level 
	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"
	| braceNode otherwise extra |
	braceNode := arguments first.
	otherwise := arguments last.
	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:
		[otherwise := nil].
	receiver
		printOn: aStream
		indent: level
		precedence: 3.
	aStream nextPutAll: ' caseOf: '.
	braceNode isVariableReference
		ifTrue: [braceNode printOn: aStream indent: level]
		ifFalse: [
			aStream nextPutAll: '{'; newLineTab: level + 1.
			braceNode casesForwardDo: [ :keyNode :valueNode :last | 
				keyNode printOn: aStream indent: level + 1.
				aStream nextPutAll: ' -> '.
				valueNode printsInNewLine
					ifTrue: [
						aStream newLineTab: level + 2.
						extra := 1]
					ifFalse: [extra := 0].
				valueNode printOn: aStream indent: level + 1 + extra.
				last ifTrue: [aStream nextPut: $}]
					ifFalse: [aStream nextPut: $.;
							 newLineTab: level + 1]]].
	otherwise ifNotNil: [
		aStream newLineTab: level + 1; nextPutAll: ' otherwise: '.
		 extra := otherwise printsInNewLine
					ifTrue: [
						aStream newLineTab: level + 2.
						1]
					ifFalse: [0].
		 otherwise printOn: aStream indent: level + 1 + extra]
</details>

#### MessageNode>>#printWithClosureAnalysisIfNilNotNil: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisIfNilNotNil: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver ifNilReceiver on: aStream indent: level.

	(arguments first isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifNotNil:
				arguments: { arguments second }
				on: aStream indent: level].
	(arguments second isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifNil:
				arguments: { arguments first }
				on: aStream indent: level].
	^self printWithClosureAnalysisKeywords: #ifNil:ifNotNil:
			arguments: arguments
			on: aStream indent: level
</details>

#### MessageNode>>#isNilIf

<details>
	<summary>See more</summary>
	
	isNilIf

	^(special between: 3 and: 4)
	   and: [(arguments first returns or: [arguments first isJust: NodeNil])
	   and: [(arguments last returns or: [arguments last isJust: NodeNil])]]
</details>

#### MessageNode>>#emitCodeForToDo: stack encoder: encoder value: forValue

var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2:


<details>
	<summary>See more</summary>
	
	emitCodeForToDo: stack encoder: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt limitInit test block incStmt blockSize |
	initStmt := arguments at: 4.
	limitInit := arguments at: 7.
	test := arguments at: 5.
	block := arguments at: 3.
	incStmt := arguments at: 6.
	blockSize := sizes at: 1.
	loopSize := sizes at: 2.
	limitInit == nil
		ifFalse: [limitInit emitCodeForEffect: stack encoder: encoder].
		
	"This will return the receiver of to:do: which is the initial value of the loop"
	forValue
		ifTrue: [initStmt emitCodeForValue: stack encoder: encoder.]
		ifFalse: [initStmt emitCodeForEffect: stack encoder: encoder].
	test emitCodeForValue: stack encoder: encoder.
	self emitCodeForBranchOn: false dist: blockSize pop: stack encoder: encoder.
	pc := encoder methodStreamPosition.
	block emitCodeForEvaluatedEffect: stack encoder: encoder.
	incStmt emitCodeForEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
</details>

#### MessageNode>>#transformIfFalseIfTrue: encoder

<details>
	<summary>See more</summary>
	
	transformIfFalseIfTrue: encoder
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]
</details>

#### MessageNode>>#isSelfBasicNewMessageSend

Answer if this ParseNode represents the 'self new'' message send.


<details>
	<summary>See more</summary>
	
	isSelfBasicNewMessageSend
	"Answer if this ParseNode represents the 'self new'' message send."

	^ receiver isSelfPseudoVariable and: [ self selectorSymbol == #basicNew ]
</details>

#### MessageNode>>#originalReceiver

<details>
	<summary>See more</summary>
	
	originalReceiver

	^ originalReceiver
</details>

#### MessageNode>>#emitCodeForValue: stack encoder: encoder

For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly.


<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: true]
		ifFalse: 
			[receiver ~~ nil ifTrue: [receiver emitCodeForValue: stack encoder: encoder].
			arguments do: [:argument | argument emitCodeForValue: stack encoder: encoder].
			pc := encoder methodStreamPosition + 1. "debug pc is first byte of the send, i.e. the next byte".
			selector
				emitCode: stack
				args: arguments size
				encoder: encoder
				super: receiver == NodeSuper]
</details>

#### MessageNode>>#expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

<details>
	<summary>See more</summary>
	
	expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ self isCascade
		ifTrue: [ aSourceRange ] "not expanded because expansion is handled in CascadeNode"
		ifFalse: [ receiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode
</details>

#### MessageNode>>#transformAnd: encoder

<details>
	<summary>See more</summary>
	
	transformAnd: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
</details>

#### MessageNode>>#checkBlock: node as: nodeName from: encoder maxArgs: maxArgs

Answer true if node is a BlockNode with at most maxArgs arguments. This check is required in order to inline some special messages. Notify some undue usage of these special messages.


<details>
	<summary>See more</summary>
	
	checkBlock: node as: nodeName from: encoder maxArgs: maxArgs
	"Answer true if node is a BlockNode with at most maxArgs arguments.
	This check is required in order to inline some special messages.
	Notify some undue usage of these special messages."

	node isBlockNode ifFalse: [ ^false ].
	node numberOfArguments <= maxArgs ifTrue: [ ^true ].
	^encoder notify: '<- ', nodeName , ' of ' , (MacroSelectors at: special) , ' has too many arguments'
</details>

#### MessageNode>>#transform: encoder

<details>
	<summary>See more</summary>
	
	transform: encoder
	special = 0 ifTrue: [^false].
	(self perform: (MacroTransformers at: special) with: encoder)
		ifTrue: 
			[^true]
		ifFalse: 
			[special := 0. ^false]
</details>

#### MessageNode>>#printWithClosureAnalysisWhileOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisWhileOn: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.
	(arguments isEmpty not
	 and: [arguments first isJust: NodeNil]) ifTrue:
			[selector := SelectorNode new
							key:
									(selector key == #whileTrue:
										ifTrue: [#whileTrue]
										ifFalse: [#whileFalse])
							code: #macro.
			arguments := Array new].
	self printWithClosureAnalysisKeywords: selector key arguments: arguments
		on: aStream indent: level
</details>

#### MessageNode>>#receiver: rcvr selector: selNode arguments: args precedence: p

Decompile.


<details>
	<summary>See more</summary>
	
	receiver: rcvr selector: selNode arguments: args precedence: p 
	"Decompile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := selNode key.
	selNode code == #macro
		ifTrue: [self noteSpecialSelector: selNode key]
		ifFalse: [special := 0].
	selector := selNode.
	"self pvtCheckForPvtSelector: encoder"
	"We could test code being decompiled, but the compiler should've checked already. And where to send the complaint?"
</details>

#### MessageNode>>#printWithClosureAnalysisReceiver: rcvr on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisReceiver: rcvr on: aStream indent: level
					
	rcvr ifNil: [^self].

	"Force parens around keyword receiver of kwd message"
	rcvr printWithClosureAnalysisOn: aStream indent: level precedence: precedence
</details>

#### MessageNode>>#emitCodeForRepeat: stack encoder: encoder value: forValue

L1: ... Jmp(L1)


<details>
	<summary>See more</summary>
	
	emitCodeForRepeat: stack encoder: encoder value: forValue 
	" L1: ... Jmp(L1)"
	| loopSize |
	loopSize := sizes at: 1.
	receiver emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]
</details>

#### MessageNode>>#receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range

Compile.


<details>
	<summary>See more</summary>
	
	receiver: rcvr selector: selName arguments: args precedence: p from: encoder sourceRange: range
	"Compile."
	encoder noteSourceRange: range forNode: self.
	^self
		receiver: rcvr
		selector: selName
		arguments: args
		precedence: p
		from: encoder
</details>

#### MessageNode>>#selector: sel

<details>
	<summary>See more</summary>
	
	selector: sel
	selector := sel
</details>

#### MessageNode>>#isReturningIf

<details>
	<summary>See more</summary>
	
	isReturningIf

	^((special between: 3 and: 4) "ifTrue:ifFalse:/ifFalse:ifTrue:"
	    or: [special between: 17 and: 18]) "ifNil:ifNotNil:/ifNotNil:ifNil:"
		and: [arguments first returns and: [arguments last returns]]
</details>

#### MessageNode>>#transformIfNilIfNotNil: encoder

vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we transform the receiver to (var := receiver) which is further transformed to (var := receiver) == nil ifTrue: .... ifFalse: ... This does not allow the block variable to shadow an existing temp, but it's no different from how to:do: is done.


<details>
	<summary>See more</summary>
	
	transformIfNilIfNotNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder maxArgs: 0)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true
</details>

#### MessageNode>>#toDoIncrement: variable

Only meant for Messages or Assignments - else return nil


<details>
	<summary>See more</summary>
	
	toDoIncrement: variable
	(receiver = variable and: [selector key = #+]) 
		ifFalse: [^ nil].
	arguments first isConstantNumber
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]
</details>

#### MessageNode>>#printReceiver: rcvr on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printReceiver: rcvr on: aStream indent: level
					
	rcvr ifNil: [^ self].

	"Force parens around keyword receiver of kwd message"
	rcvr printOn: aStream indent: level precedence: precedence
</details>

#### MessageNode>>#isMessage

<details>
	<summary>See more</summary>
	
	isMessage
	^true
</details>

#### MessageNode>>#receiver: val

14 feb 2001 - removed return arrow


<details>
	<summary>See more</summary>
	
	receiver: val
	"14 feb 2001 - removed return arrow"

	receiver := val
</details>

#### MessageNode>>#sizeCodeForIf: encoder value: forValue

<details>
	<summary>See more</summary>
	
	sizeCodeForIf: encoder value: forValue
	| thenExpr elseExpr branchSize thenSize elseSize |
	thenExpr := arguments at: 1.
	elseExpr := arguments at: 2.
	(forValue
	 or: [(thenExpr isJust: NodeNil)
	 or: [elseExpr isJust: NodeNil]]) not
			"(...not ifTrue: avoids using ifFalse: alone during this compile)"
		ifTrue:  "Two-armed IFs forEffect share a single pop"
			[^super sizeCodeForEffect: encoder].
	forValue
		ifTrue:  "Code all forValue as two-armed"
			[elseSize := elseExpr sizeCodeForEvaluatedValue: encoder.
			thenSize := (thenExpr sizeCodeForEvaluatedValue: encoder)
					+ (thenExpr returns
						ifTrue: [0]  "Elide jump over else after a return"
						ifFalse: [self sizeCode: encoder forJump: elseSize]).
			branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
		ifFalse:  "One arm is empty here (two-arms code forValue)"
			[(elseExpr isJust: NodeNil)
				ifTrue:
					[elseSize := 0.
					thenSize := thenExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: false dist: thenSize]
				ifFalse:
					[thenSize := 0.
					elseSize := elseExpr sizeCodeForEvaluatedEffect: encoder.
					branchSize := self sizeCode: encoder forBranchOn: true dist: elseSize]].
	sizes := Array with: thenSize with: elseSize.
	^(receiver sizeCodeForValue: encoder)
	+ branchSize + thenSize + elseSize
</details>

#### MessageNode>>#sizeCodeForWhile: encoder value: forValue

L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only); justStmt, wholeLoop, justJump.


<details>
	<summary>See more</summary>
	
	sizeCodeForWhile: encoder value: forValue 
	"L1: ... Bfp(L2) ... Jmp(L1) L2: nil (nil for value only);
	justStmt, wholeLoop, justJump."
	| cond stmt stmtSize loopSize branchSize |
	cond := receiver.
	stmt := arguments at: 1.
	"We assume long backward branches are always maximal size branches."
	stmtSize := (stmt sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	branchSize := self
					sizeCode: encoder
					forBranchOn: selector key == #whileFalse:  "Btp for whileFalse"
					dist: stmtSize.
	loopSize := (cond sizeCodeForEvaluatedValue: encoder) + branchSize + stmtSize.
	sizes := Array with: stmtSize with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])
</details>

#### MessageNode>>#ifNilReceiver

assuming this object is the receiver of an ifNil:, what object is being asked about?


<details>
	<summary>See more</summary>
	
	ifNilReceiver

	^receiver
</details>

#### MessageNode>>#transformIfNil: encoder

vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below.


<details>
	<summary>See more</summary>
	
	transformIfNil: encoder

	"vb: Removed the original transformBoolean: which amounds to a test we perform in each of the branches below."
	(MacroSelectors at: special) = #ifNotNil: ifTrue:
		[(self checkBlock: arguments first as: 'ifNotNil arg' from: encoder maxArgs: 1) ifFalse:
			[^false].

		"Transform 'ifNotNil: [stuff]' to 'ifNil: [nil] ifNotNil: [stuff]'.
		Slightly better code and more consistent with decompilation."
		self noteSpecialSelector: #ifNil:ifNotNil:.
		selector := SelectorNode new key: (MacroSelectors at: special) code: #macro.
		arguments := Array
						with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
						with: (arguments first noteOptimizedIn: self).
		(self transform: encoder) ifFalse:
			[self error: 'compiler logic error'].
		^true].
	(self checkBlock: arguments first as: 'ifNil arg' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments first noteOptimizedIn: self.
	^true
</details>

#### MessageNode>>#transformIfFalse: encoder

<details>
	<summary>See more</summary>
	
	transformIfFalse: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]
</details>

#### MessageNode>>#printOn: aStream indent: level

may not need this check anymore - may be fixed by the #receiver: change


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level
	"may not need this check anymore - may be fixed by the #receiver: change"
	special ifNil: [^aStream nextPutAll: '** MessageNode with nil special **'].

	special > 0 ifTrue:
		[^self perform: self macroPrinter with: aStream with: level].

	self printReceiver: receiver on: aStream indent: level.
	selector isForFFICall
		ifTrue:
			[aStream space.
			 selector
				printAsFFICallWithArguments: arguments
				on: aStream
				indent: 0]
		ifFalse:
			[self printKeywords: selector key
				 arguments: arguments
				 on: aStream
				 indent: level]
</details>

#### MessageNode>>#printWithClosureAnalysisToDoOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisToDoOn: aStream indent: level

	| limitNode |
	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	limitNode := (arguments last == nil
				or: [arguments last isAssignmentNode not])
					ifTrue: [arguments first]
					ifFalse: [arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printWithClosureAnalysisKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printWithClosureAnalysisKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]
</details>

#### MessageNode>>#printWithClosureAnalysisIfNil: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisIfNil: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	^self printWithClosureAnalysisKeywords: selector key
		arguments: (Array with: arguments first)
		on: aStream indent: level
</details>

#### MessageNode>>#printIfOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printIfOn: aStream indent: level 
	receiver ifNotNil: 
		[ receiver
			printOn: aStream
			indent: level
			precedence: precedence ].
	(arguments last isJust: NodeNil) ifTrue: [ ^ self
			printKeywords: #ifTrue:
			arguments: (Array with: arguments first)
			on: aStream
			indent: level ].
	(arguments last isJust: NodeFalse) ifTrue: [ ^ self
			printKeywords: #and:
			arguments: (Array with: arguments first)
			on: aStream
			indent: level ].
	(arguments first isJust: NodeNil) ifTrue: [ ^ self
			printKeywords: #ifFalse:
			arguments: (Array with: arguments last)
			on: aStream
			indent: level ].
	(arguments first isJust: NodeTrue) ifTrue: [ ^ self
			printKeywords: #or:
			arguments: (Array with: arguments last)
			on: aStream
			indent: level ].
	self
		printKeywords: #ifTrue:ifFalse:
		arguments: arguments
		on: aStream
		indent: level
</details>

#### MessageNode>>#isMessage: selSymbol receiver: rcvrPred arguments: argsPred

Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred evaluate to true with respect to receiver and the list of arguments. If selSymbol or either predicate is nil, it means 'don't care'. Note that argsPred takes numArgs arguments. All block arguments are ParseNodes.


<details>
	<summary>See more</summary>
	
	isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"Answer whether selector is selSymbol, and the predicates rcvrPred and argsPred
	 evaluate to true with respect to receiver and the list of arguments.  If selSymbol or
	 either predicate is nil, it means 'don't care'.  Note that argsPred takes numArgs
	 arguments.  All block arguments are ParseNodes."

	^(selSymbol isNil or: [selSymbol==selector key]) and:
		[(rcvrPred isNil or: [rcvrPred value: receiver]) and:
			[(argsPred isNil or: [argsPred valueWithArguments: arguments])]]
</details>

#### MessageNode>>#eval

When everything in me is a constant, I can produce a value. This is only used by the Scripting system (TilePadMorph tilesFrom:in:)


<details>
	<summary>See more</summary>
	
	eval
	"When everything in me is a constant, I can produce a value.  This is only used by the Scripting system (TilePadMorph tilesFrom:in:)"

	| rec args |
	receiver isVariableNode ifFalse: [^ #illegal].
	rec := receiver key value.
	args := arguments collect: [:each | each eval].
	^ rec perform: selector key withArguments: args
</details>

#### MessageNode>>#printWithClosureAnalysisKeywords: key arguments: args on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue: [aStream space; nextPutAll: key. ^self].
	keywords := key keywords.
	doCrTab := args size > 2
				or: [{receiver} , args anySatisfy:
						[:thisArg |
						thisArg isBlockNode
						or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]].
	1 to: (args size min: keywords size) do:
		[:i |
		arg := args at: i.
		kwd := keywords at: i.
		doCrTab
			ifTrue: [aStream newLineTab: level+1. indent := 1] "newline after big args"
			ifFalse: [aStream space. indent := 0].
		aStream nextPutAll: kwd; space.
		arg printWithClosureAnalysisOn: aStream
			indent: level + 1 + indent
			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]
</details>

#### MessageNode>>#isSelfNewMessageSend

Answer if this ParseNode represents the 'self new'' message send.


<details>
	<summary>See more</summary>
	
	isSelfNewMessageSend
	"Answer if this ParseNode represents the 'self new'' message send."

	^ receiver isSelfPseudoVariable and: [ self selectorSymbol == #new ]
</details>

#### MessageNode>>#receiverSourceRangesFrom: sourceRanges

we can't just do #at: because sometimes what it is on the source ranges map is not the exact same object than the receiver or the originalReceiver (like when optimizations are made), so we look for an 'equivalent' one (at least for using as a key in the source ranges)


<details>
	<summary>See more</summary>
	
	receiverSourceRangesFrom: sourceRanges
	"we can't just do #at: because sometimes what it is on the source ranges map
	is not the exact same object than the receiver or the originalReceiver
	(like when optimizations are made), so we look for an 'equivalent' one
	(at least for using as a key in the source ranges)"

	^ sourceRanges at: receiver ifAbsent: [
		| parseNodeOfReceiver |
		parseNodeOfReceiver _ sourceRanges keys detect: [ :parseNode |
		(parseNode equivalentTo: receiver) or: [ parseNode equivalentTo: originalReceiver ] ].
		sourceRanges at: parseNodeOfReceiver
	]
</details>

#### MessageNode>>#printWhileOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWhileOn: aStream indent: level
	self printReceiver: receiver on: aStream indent: level.
	self
		printKeywords: originalSelector
		arguments: originalArguments
		on: aStream indent: level
</details>

#### MessageNode>>#argumentsInEvaluationOrder

Answer the receivers arguments in evaluation order. If the receiver is a transformed to:do: node this will undo the misordering done by the transformation.


<details>
	<summary>See more</summary>
	
	argumentsInEvaluationOrder
	"Answer the receivers arguments in evaluation order.
	 If the receiver is a transformed to:do: node this will undo the misordering done by the transformation."
	^(special > 0
	   and: [(MacroTransformers at: special) == #transformToDo:
	   and: [arguments size >= 7]])
		"arguments are in a weid order and may be nil in a transformed to:do: loop.  sigh...
		 c.f. emitCodeForToDo:encoder:value:"
		ifTrue:
			[(arguments at: 7)	"limitInit"
				ifNil: [{	(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]
				ifNotNil: [:limitInit|
						{ limitInit.
						(arguments at: 4).	"initStmt"
						(arguments at: 5).	"test"
						(arguments at: 3).	"block"
						(arguments at: 6) 	"incStmt" }]]
		ifFalse:
			[arguments]
</details>

#### MessageNode>>#emitCodeForIfNil: stack encoder: encoder value: forValue

<details>
	<summary>See more</summary>
	
	emitCodeForIfNil: stack encoder: encoder value: forValue

	| theNode theSize ifNotNilSelector |
	theNode := arguments first.
	theSize := sizes at: 1.
	ifNotNilSelector := #ifNotNil:.
	receiver emitCodeForValue: stack encoder: encoder.
	forValue ifTrue: [encoder genDup. stack push: 1].
	encoder genPushSpecialLiteral: nil. stack push: 1.
	equalNode emitCode: stack args: 1 encoder: encoder.
	self 
		emitCodeForBranchOn: (selector key == ifNotNilSelector)
		dist: theSize 
		pop: stack 
		encoder: encoder.
	pc := encoder methodStreamPosition.
	forValue 
		ifTrue: 
			[encoder genPop. stack pop: 1.
			theNode emitCodeForEvaluatedValue: stack encoder: encoder]	
		ifFalse: [theNode emitCodeForEvaluatedEffect: stack encoder: encoder]
</details>

#### MessageNode>>#cascadeReceiver

Nil out rcvr (to indicate cascade) and return what it had been.


<details>
	<summary>See more</summary>
	
	cascadeReceiver
	"Nil out rcvr (to indicate cascade) and return what it had been."

	| rcvr |
	rcvr := receiver.
	receiver := nil.
	^rcvr
</details>

#### MessageNode>>#isCascade

<details>
	<summary>See more</summary>
	
	isCascade

	^receiver isNil 
</details>

#### MessageNode>>#transformIfTrue: encoder

<details>
	<summary>See more</summary>
	
	transformIfTrue: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
</details>

#### MessageNode>>#printWithClosureAnalysisOn: strm indent: level precedence: outerPrecedence

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: strm indent: level precedence: outerPrecedence

	| parenthesize |
	parenthesize := precedence > outerPrecedence
		or: [outerPrecedence = 3 and: [precedence = 3 "both keywords"]].
	parenthesize
		ifTrue: [strm nextPutAll: '('.
				self printWithClosureAnalysisOn: strm indent: level.
				strm nextPutAll: ')']
		ifFalse: [self printWithClosureAnalysisOn: strm indent: level]
</details>

#### MessageNode>>#printWithClosureAnalysisParenReceiver: rcvr on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisParenReceiver: rcvr on: aStream indent: level
					
	rcvr isBlockNode ifTrue:
		[^rcvr printWithClosureAnalysisOn: aStream indent: level].
	aStream nextPut: $(.
	rcvr printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPut: $)
</details>

#### MessageNode>>#keywordRanges

<details>
	<summary>See more</summary>
	
	keywordRanges

	^keywordRanges
</details>

#### MessageNode>>#receiver: aReceiver selector: aSelector arguments: args precedence: aPrecedence from: anEncoder sourceRange: aSourceRange keywordsRanges: wordsRanges

<details>
	<summary>See more</summary>
	
	receiver: aReceiver selector: aSelector arguments: args precedence: aPrecedence from: anEncoder sourceRange: aSourceRange keywordsRanges: wordsRanges
	
	keywordRanges := wordsRanges.
	
	^self receiver: aReceiver selector: aSelector arguments: args precedence: aPrecedence from: anEncoder sourceRange: aSourceRange 
</details>

#### MessageNode>>#noteSpecialSelector: selectorSymbol

special > 0 denotes specially treated (potentially inlined) messages.


<details>
	<summary>See more</summary>
	
	noteSpecialSelector: selectorSymbol
	"special > 0 denotes specially treated (potentially inlined) messages. "

	special := MacroSelectors indexOf: selectorSymbol.

</details>

#### MessageNode>>#transformIfNotNilIfNil: encoder

vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we transform the receiver to (var := receiver) which is further transformed to (var := receiver) == nil ifTrue: .... ifFalse: ... This does not allow the block variable to shadow an existing temp, but it's no different from how to:do: is done.


<details>
	<summary>See more</summary>
	
	transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	| ifNotNilArg |
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: (Array with: NodeNil)
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true
</details>

#### MessageNode>>#hasEquivalentReceiverWith: aMessageNode

<details>
	<summary>See more</summary>
	
	hasEquivalentReceiverWith: aMessageNode

	^ self isCascade
		ifTrue: [ originalReceiver equivalentTo: aMessageNode originalReceiver ]
		ifFalse: [ receiver equivalentTo: aMessageNode receiver ]
</details>

#### MessageNode>>#ensureCanCascade: encoder

<details>
	<summary>See more</summary>
	
	ensureCanCascade: encoder

	special > 0 ifTrue: [
		special := 0.
		receiver := originalReceiver.
		selector := encoder encodeSelector: originalSelector.
		arguments := originalArguments.
		receiver isBlockNode ifTrue: [receiver deoptimize].
		arguments do:
			[:each|
			each isBlockNode ifTrue:
				[each deoptimize]]]
</details>

#### MessageNode>>#printIfNilNotNil: aStream indent: level

<details>
	<summary>See more</summary>
	
	printIfNilNotNil: aStream indent: level

	self printReceiver: receiver ifNilReceiver on: aStream indent: level.

	(arguments first isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNotNil:
				arguments: { arguments second }
				on: aStream indent: level].
	(arguments second isJust: NodeNil) ifTrue:
		[^ self printKeywords: #ifNil:
				arguments: { arguments first }
				on: aStream indent: level].
	^ self printKeywords: #ifNil:ifNotNil:
			arguments: arguments
			on: aStream indent: level
</details>

#### MessageNode>>#printToDoOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printToDoOn: aStream indent: level

	| limitNode |
	self printReceiver: receiver on: aStream indent: level.

	(arguments last == nil or: [(arguments last isMemberOf: AssignmentNode) not])
		ifTrue: [limitNode := arguments first]
		ifFalse: [limitNode := arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printKeywords: #to:do:
					arguments: (Array with: limitNode with: (arguments at: 3))
					on: aStream indent: level]
		ifFalse: [self printKeywords: selector key
					arguments: (Array with: limitNode) , arguments allButFirst
					on: aStream indent: level]
</details>

#### MessageNode>>#sizeCodeForToDo: encoder value: forValue

var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2:


<details>
	<summary>See more</summary>
	
	sizeCodeForToDo: encoder value: forValue 
	" var := rcvr. L1: [var <= arg1] Bfp(L2) [block body. var := var + inc] Jmp(L1) L2: "
	| loopSize initStmt test block incStmt blockSize initSize limitInit |
	block := arguments at: 3.
	initStmt := arguments at: 4.
	test := arguments at: 5.
	incStmt := arguments at: 6.
	limitInit := arguments at: 7.
	initSize := forValue
		ifTrue: [initStmt sizeCodeForValue: encoder.]
		ifFalse: [initStmt sizeCodeForEffect: encoder].
	limitInit == nil ifFalse:
		[initSize := initSize + (limitInit sizeCodeForEffect: encoder)].
	blockSize := (block sizeCodeForEvaluatedEffect: encoder)
			+ (incStmt sizeCodeForEffect: encoder)
			+ (encoder sizeJumpLong: -1).
	loopSize := (test sizeCodeForValue: encoder)
			+ (self sizeCode: encoder forBranchOn: false dist: blockSize)
			+ blockSize.
	sizes := Array with: blockSize with: loopSize.
	^initSize
	+ loopSize
</details>

#### MessageNode>>#arguments: list

<details>
	<summary>See more</summary>
	
	arguments: list
	arguments := list asArray
</details>

#### MessageNode>>#sizeCodeForEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEffect: encoder

	special > 0 
		ifTrue:
			[encoder noteOptimizedSelector: originalSelector.
			^self perform: (MacroSizers at: special) with: encoder with: false].
	^super sizeCodeForEffect: encoder
</details>

#### MessageNode>>#transformWhile: encoder

<details>
	<summary>See more</summary>
	
	transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder maxArgs: 0) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := SelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]
</details>

#### MessageNode>>#canCascade

<details>
	<summary>See more</summary>
	
	canCascade

	^receiver ~~ NodeSuper
</details>

#### MessageNode>>#isOptimizedLoop

<details>
	<summary>See more</summary>
	
	isOptimizedLoop
	^special > 0
	   and: [#(transformWhile: transformToDo:) includes: (MacroTransformers at: special)]
</details>

#### MessageNode>>#printWithClosureAnalysisIfOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisIfOn: aStream indent: level

	receiver ifNotNil:
		[receiver printWithClosureAnalysisOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifTrue: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printWithClosureAnalysisKeywords: #and: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifFalse: arguments: (Array with: arguments last)
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printWithClosureAnalysisKeywords: #or: arguments: (Array with: arguments last)
					on: aStream indent: level].
	self printWithClosureAnalysisKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level
</details>

#### MessageNode>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^selector
</details>

#### MessageNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	^ (special
			between: 1
			and: 10)
		or: 
		[ arguments size > 1
			or: 
			[ receiver isComplex or: [ arguments anySatisfy: [ : each | each isComplex ] ] ] ]
</details>

#### MessageNode>>#isOptimized

<details>
	<summary>See more</summary>
	
	isOptimized
	^special > 0
</details>

#### MessageNode>>#transformOr: encoder

<details>
	<summary>See more</summary>
	
	transformOr: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeTrue) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]
</details>

#### MessageNode>>#transformCase: encoder

<details>
	<summary>See more</summary>
	
	transformCase: encoder

	| caseNode |
	caseNode := arguments first.
	(caseNode isMemberOf: BraceNode) ifFalse: [^false].
	(caseNode blockAssociationCheck: encoder) ifFalse: [^false].
	(arguments size = 1
	 or: [self checkBlock: arguments last as: 'otherwise arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].
	 caseNode elementsDo:
		[:messageNode |
		messageNode receiver noteOptimizedIn: self.
		messageNode arguments first noteOptimizedIn: self].
	 arguments size = 2 ifTrue:
		[arguments last noteOptimizedIn: self].
	 ^true
</details>

#### MessageNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Assignments within optimized loops are tricky.  Because a loop repeats a
	 write to a temporary in an optimized loop effectively occurs after the loop.
	 To handle this collect the set of temps assigned to in optimized loops and
	 add extra writes after traversing the optimized loop constituents."
	| writtenToTemps |
	self isOptimizedLoop ifTrue:
		[{ receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[assignmentPools at: node put: Set new]]].
	"receiver is nil in cascades"
	receiver == nil ifFalse:
		[receiver analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools].
	arguments do:
		[:node|
		node == nil ifFalse: "last argument of optimized to:do: can be nil"
			[node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]].
	"Add assignments representing subsequent iterations
	 and redo the closure analysis for the written-to temps."
	self isOptimizedLoop ifTrue:
		[writtenToTemps := Set new.
		 { receiver }, arguments do:
			[:node|
			(node notNil and: [node isBlockNode and: [node optimized]]) ifTrue:
				[(assignmentPools removeKey: node) do:
					[:temp|
					temp isBlockArg ifFalse: "ignore added assignments to to:do: loop args"
						[writtenToTemps add: temp.
						 temp addWriteWithin: node at: rootNode locationCounter]]]].
		 writtenToTemps isEmpty ifFalse:
			[(writtenToTemps asSortedCollection: ParseNode tempSortBlock) do:
				[:each| each analyseClosure: rootNode].
			 (writtenToTemps collect: [:each| each definingScope]) do:
				[:blockNode|
				blockNode ifHasRemoteTempNodeEnsureInitializationStatementExists: rootNode]]]
</details>

#### MessageNode>>#printIfNil: aStream indent: level

<details>
	<summary>See more</summary>
	
	printIfNil: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: (Array with: arguments first)
		on: aStream indent: level
</details>

#### MessageNode>>#macroPrinter

<details>
	<summary>See more</summary>
	
	macroPrinter

	special > 0 ifTrue: [^MacroPrinters at: special].
	^nil

</details>

#### MessageNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isMessageNode
		and: [ self hasEquivalentReceiverWith: aParseNode ]
		and: [ self selector equivalentTo: aParseNode selector ]
		and: [ self hasEquivalentArgumentsWith: aParseNode ]
</details>

#### MessageNode>>#precedence

<details>
	<summary>See more</summary>
	
	precedence

	^precedence
</details>

#### MessageNode>>#emitCodeForEffect: stack encoder: encoder

For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly.


<details>
	<summary>See more</summary>
	
	emitCodeForEffect: stack encoder: encoder
	"For #ifTrue:ifFalse: and #whileTrue: / #whileFalse: style messages, the pc is set to the jump instruction, so that mustBeBoolean exceptions can be shown correctly."
	special > 0
		ifTrue: 
			[pc := 0.
			self perform: (MacroEmitters at: special) with: stack with: encoder with: false]
		ifFalse: 
			[super emitCodeForEffect: stack encoder: encoder]
</details>

#### MessageNode>>#receiver: rcvr arguments: args precedence: p

<details>
	<summary>See more</summary>
	
	receiver: rcvr arguments: args precedence: p

	receiver := rcvr.
	originalReceiver := rcvr copy.
	arguments := args asArray.
	originalArguments := arguments copy.
	sizes := Array new: arguments size.
	precedence := p
</details>

#### MessageNode>>#sizeCodeForIfNil: encoder value: forValue

<details>
	<summary>See more</summary>
	
	sizeCodeForIfNil: encoder value: forValue

	| theNode theSize theSelector |
	equalNode := encoder encodeSelector: #==.
	sizes := Array new: 1.
	theNode := arguments first.
	theSelector := #ifNotNil:.
	forValue
		ifTrue:
			[sizes at: 1 put: (theSize := (encoder sizePop + (theNode sizeCodeForEvaluatedValue: encoder))).
			 ^(receiver sizeCodeForValue: encoder)
			 + encoder sizeDup
			 + (encoder sizePushSpecialLiteral: nil)
			 + (equalNode sizeCode: encoder args: 1 super: false)
			 + (self 
					sizeCode: encoder forBranchOn: selector key == theSelector 
					dist: theSize)
			 + theSize]
		ifFalse:
			[sizes at: 1 put: (theSize := (theNode sizeCodeForEvaluatedEffect: encoder)).
			 ^(receiver sizeCodeForValue: encoder)
				+ (encoder sizePushSpecialLiteral: nil)
				+ (equalNode sizeCode: encoder args: 1 super: false)
				+ (self 
					sizeCode: encoder
					forBranchOn: selector key == theSelector 
					dist: theSize)
				+ theSize]
</details>

#### MessageNode>>#emitCodeForCase: stack encoder: encoder value: forValue

<details>
	<summary>See more</summary>
	
	emitCodeForCase: stack encoder: encoder value: forValue

	| braceNode sizeStream allReturn |
	forValue ifFalse:
		[^super emitCodeForEffect: stack encoder: encoder].
	braceNode := arguments first.
	sizeStream := ReadStream on: sizes.
	receiver emitCodeForValue: stack encoder: encoder.
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last | | thenSize elseSize |
		thenSize := sizeStream next.
		elseSize := sizeStream next.
		last ifFalse: [encoder genDup. stack push: 1].
		keyNode emitCodeForEvaluatedValue: stack encoder: encoder.
		equalNode emitCode: stack args: 1 encoder: encoder.
		self emitCodeForBranchOn: false dist: thenSize pop: stack encoder: encoder.
		last ifFalse: [encoder genPop. stack pop: 1].
		valueNode emitCodeForEvaluatedValue: stack encoder: encoder.
		last ifTrue: [stack pop: 1].
		valueNode returns ifFalse:
			[self emitCodeForJump: elseSize encoder: encoder.
			 allReturn := false].
		(last and: [allReturn]) ifTrue:
			[self emitCodeForJump: elseSize encoder: encoder]].
	arguments size = 2
		ifTrue:
			[arguments last emitCodeForEvaluatedValue: stack encoder: encoder] "otherwise: [...]"
		ifFalse:
			[NodeSelf emitCodeForValue: stack encoder: encoder.
			caseErrorNode emitCode: stack args: 0 encoder: encoder]
</details>

#### MessageNode>>#isMessageNamed: aSelector

<details>
	<summary>See more</summary>
	
	isMessageNamed: aSelector

	^aSelector == self selectorSymbol 
</details>

#### MessageNode>>#sizeCodeForRepeat: encoder value: forValue

L1: ... Jmp(L1) nil (nil for value only);


<details>
	<summary>See more</summary>
	
	sizeCodeForRepeat: encoder value: forValue 
	"L1: ... Jmp(L1) nil (nil for value only);"
	| loopSize |
	"We assume long backward branches are always maximal size branches."
	loopSize := (receiver sizeCodeForEvaluatedEffect: encoder) + (encoder sizeJumpLong: -1).
	sizes := Array with: loopSize.
	^loopSize + (forValue ifTrue: [encoder sizePushSpecialLiteral: nil] ifFalse: [0])
</details>

#### MessageNode>>#selectorSymbol

<details>
	<summary>See more</summary>
	
	selectorSymbol

	^selector key
</details>

#### MessageNode>>#toDoLimit: variable

<details>
	<summary>See more</summary>
	
	toDoLimit: variable
	(receiver = variable and: [selector key = #<= or: [selector key = #>=]]) 
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]
</details>

#### MessageNode>>#receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder

Compile.


<details>
	<summary>See more</summary>
	
	receiver: rcvr selector: aSelector arguments: args precedence: p from: encoder 
	"Compile."

	self receiver: rcvr
		arguments: args
		precedence: p.
	originalSelector := aSelector.
	self noteSpecialSelector: aSelector.
	(self transform: encoder)
		ifTrue: 
			[selector isNil ifTrue:
				[selector := SelectorNode new 
								key: (MacroSelectors at: special)
								code: #macro]]
		ifFalse: 
			[selector := encoder encodeSelector: aSelector.
			rcvr == NodeSuper ifTrue: [encoder noteSuper]].
	self pvtCheckForPvtSelector: encoder
</details>

#### MessageNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitMessageNode: self
</details>

#### MessageNode>>#emitCodeForWhile: stack encoder: encoder value: forValue

L1: ... Bfp(L2)|Btp(L2) ... Jmp(L1) L2:


<details>
	<summary>See more</summary>
	
	emitCodeForWhile: stack encoder: encoder value: forValue 
	"L1: ... Bfp(L2)|Btp(L2) ... Jmp(L1) L2: "
	| cond stmt stmtSize loopSize |
	cond := receiver.
	stmt := arguments at: 1.
	stmtSize := sizes at: 1.
	loopSize := sizes at: 2.
	cond emitCodeForEvaluatedValue: stack encoder: encoder.
	self emitCodeForBranchOn: (selector key == #whileFalse:)  "Bfp for whileTrue"
					dist: stmtSize pop: stack encoder: encoder.   "Btp for whileFalse"
	pc := encoder methodStreamPosition.
	stmt emitCodeForEvaluatedEffect: stack encoder: encoder.
	self emitCodeForJump: 0 - loopSize encoder: encoder.
	forValue ifTrue: [encoder genPushSpecialLiteral: nil. stack push: 1]
</details>

#### MessageNode>>#sizeCodeForCase: encoder value: forValue

<details>
	<summary>See more</summary>
	
	sizeCodeForCase: encoder value: forValue

	| braceNode sizeIndex elseSize allReturn |
	forValue not ifTrue:
		[^super sizeCodeForEffect: encoder].
	equalNode := encoder encodeSelector: #=.
	braceNode := arguments first.
	sizes := Array new: 2 * braceNode numElements.
	sizeIndex := sizes size.
	elseSize := arguments size = 2
		ifTrue:
			[arguments last sizeCodeForEvaluatedValue: encoder] "otherwise: [...]"
		ifFalse:
			[caseErrorNode := encoder encodeSelector: #caseError.
			 (NodeSelf sizeCodeForValue: encoder)
			 + (caseErrorNode sizeCode: encoder args: 0 super: false)]. "self caseError"
	"There must be at least one branch around the otherwise/caseError
	  so the decompiler can identify the end of the otherwise/caseError."
	allReturn := true. "assume every case ends with a return"
	braceNode casesForwardDo:
		[:keyNode :valueNode :last |
		valueNode returns ifFalse: [allReturn := false]].
	braceNode casesReverseDo:
		[:keyNode :valueNode :last | | thenSize |
		sizes at: sizeIndex put: elseSize.
		thenSize := valueNode sizeCodeForEvaluatedValue: encoder.
		last ifFalse: [thenSize := thenSize + encoder sizePop].
		valueNode returns ifFalse: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		(last and: [allReturn]) ifTrue: [thenSize := thenSize + (self sizeCode: encoder forJump: elseSize)].
		sizes at: sizeIndex-1 put: thenSize.
		last ifFalse: [elseSize := elseSize + encoder sizeDup].
		elseSize := elseSize
					+ (keyNode sizeCodeForEvaluatedValue: encoder)
					+ (equalNode sizeCode: encoder args: 1 super: false)
					+ (self sizeCode: encoder forBranchOn: false dist: thenSize)
					+ thenSize.
		sizeIndex := sizeIndex - 2].
	^(receiver sizeCodeForValue: encoder) + elseSize
</details>

#### MessageNode>>#hasEquivalentArgumentsWith: aMessageNode

<details>
	<summary>See more</summary>
	
	hasEquivalentArgumentsWith: aMessageNode

	^ self isCascade
		ifTrue: [ self compare: originalArguments with: aMessageNode originalArguments ]
		ifFalse: [ self compare: arguments with: aMessageNode arguments ]
</details>

#### MessageNode>>#pvtCheckForPvtSelector: encoder

If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder.


<details>
	<summary>See more</summary>
	
	pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isInitializePvtSelector ifTrue: [
		(receiver isSelfNewMessageSend or: [receiver isSelfBasicNewMessageSend]) ifFalse: [
			encoder notify: 'Private instance initialization messages may only be sent to ''self new'' or "self basicNew" (by class instance creation methods)']].
	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			encoder notify: 'Private messages may only be sent to self or super']].
</details>

## MethodNode

I am the root of the parse tree.. Instance Variables arguments: <SequenceableCollection> block: <BlockNode> encoder: <BytecodeEncoder> localsPool: <IdentitySet> locationCounter: <Integer> precedence: <Integer> primitive: <Integer> properties: <AdditionalMethodState|nil> selectorOrFalse: <Object> sourceText: <String|Text> temporaries: <SequenceableCollection> temporariesDeclaration: <TemporariesDeclarationNode> arguments - the collection of parsed or decompiled method arguments block - the BlockNode holding the method's statements encoder - the object that comprises the copiler's scope table, literal pool and back-end bytecode generator localsPool - a set used to determine the set of copied values for each block in the method locationCounter - an integer used to mark block scopes for the purposes of the closure transformation. See BlockNode>>#analyseArguments:temporaries:rootNode: precedence - the precedence of the method's selector (see Symbol>>precedence) primitive - if non-zero this is the integer code of the method's primitive properties - the object used to accumulate method properties (a.k.a. pragmas) selectorOrFalse - the method's selector or false if this is a doit sourceText - the source test from which the method was compiled temporaries - the collection of parsed or decompiled method temporaries temporariesDeclaration - an alternative way to represent the temporaries declaration, by using a parse node to represent that; it should eventually replace the need for the "temporaries" instance variable (because the temporaries can be obtained through this object); every read to "temporaries" can be replaced by sending #allDeclaredVariableNodes to this object; right now the Parser initializes both "temporaries" and "temporariesDeclaration" to ease the migration process

### Methods
#### MethodNode>>#tempNames

<details>
	<summary>See more</summary>
	
	tempNames
	^ encoder tempNames
</details>

#### MethodNode>>#ensureClosureAnalysisDone

<details>
	<summary>See more</summary>
	
	ensureClosureAnalysisDone
	block blockExtent ifNil:
		[temporaries := block analyseArguments: arguments temporaries: temporaries rootNode: self]
</details>

#### MethodNode>>#blockExtentsToTempsMap

Answer a Dictionary of blockExtent to temp locations for the current method. This is used by the debugger to locate temp vars in contexts. A temp map entry is a pair of the temp's name and its index, where an index is either an integer for a normal temp or a pair of the index of the indirect temp vector containing the temp and the index of the temp in its indirect temp vector.


<details>
	<summary>See more</summary>
	
	blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := encoder classEncoding parserClass new
						encoderClass: encoder class;
						parse: (sourceText ifNil: [self decompileString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]
</details>

#### MethodNode>>#body

<details>
	<summary>See more</summary>
	
	body
	^block
</details>

#### MethodNode>>#arguments

For transformations etc, not used in compilation


<details>
	<summary>See more</summary>
	
	arguments
	"For transformations etc, not used in compilation"
	^arguments
</details>

#### MethodNode>>#selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim

RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries


<details>
	<summary>See more</summary>
	
	selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim 
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	self 
		selector: selOrFalse
		arguments: args
		precedence: p
		temporaries: temps
		block: blk
		encoder: anEncoder 
		primitive: prim 
		properties: AdditionalMethodState new.
</details>

#### MethodNode>>#selectorLastPosition

If #DoIt selector, returns 0 - Hernan


<details>
	<summary>See more</summary>
	
	selectorLastPosition

	"If #DoIt selector, returns 0 - Hernan"

	^self selector isUnary
		ifTrue: [ selectorKeywordsRanges ifNil: [ 0 ] ifNotNil: [ selectorKeywordsRanges last last ]]
		ifFalse: [
			(encoder 
				rangeForNode: arguments last
				ifAbsent: [ self shouldNotHappenBecause: 'arguments are part of the encoder'  ]) first last ]
</details>

#### MethodNode>>#methodClass

<details>
	<summary>See more</summary>
	
	methodClass

	^ encoder classEncoding
</details>

#### MethodNode>>#primitiveErrorVariableName

Answer the primitive error code temp name, or nil if none.


<details>
	<summary>See more</summary>
	
	primitiveErrorVariableName
	"Answer the primitive error code temp name, or nil if none."
	(primitive isInteger and: [primitive > 0]) ifTrue:
		[properties pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil

	"(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: Parser) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' error: foo module: ''bar''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' module: ''bar'' error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate"
</details>

#### MethodNode>>#messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	^encoder messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: aBlock
	
</details>

#### MethodNode>>#printTemporariesOn: aStream

<details>
	<summary>See more</summary>
	
	printTemporariesOn: aStream
	
	block printTemporaries: temporaries on: aStream doPrior: [aStream newLineTab: 1].

</details>

#### MethodNode>>#temporariesDeclaration: aTemporariesDeclarationNode

RNG: after removing all the usages of the temporaries inst var, the last line can be removed


<details>
	<summary>See more</summary>
	
	temporariesDeclaration: aTemporariesDeclarationNode
	"RNG: after removing all the usages of the temporaries inst var, the last line can be removed"

	temporariesDeclaration := aTemporariesDeclarationNode.
	self temporaries: aTemporariesDeclarationNode allDeclaredVariableNodes
</details>

#### MethodNode>>#selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range

RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated


<details>
	<summary>See more</summary>
	
	selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	selectorKeywordsRanges := range.

	^ self selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict
</details>

#### MethodNode>>#locationCounter

<details>
	<summary>See more</summary>
	
	locationCounter
	^locationCounter
</details>

#### MethodNode>>#numberOfStatements

<details>
	<summary>See more</summary>
	
	numberOfStatements

	^ block statements size
</details>

#### MethodNode>>#generate: trailer

The receiver is the root of a parse tree. Answer a CompiledMethod. The argument, trailer, is arbitrary but is typically either the reference to the source code that is stored with every CompiledMethod, or an encoding of the method's temporary names.


<details>
	<summary>See more</summary>
	
	generate: trailer 
	"The receiver is the root of a parse tree. Answer a CompiledMethod.
	 The argument, trailer, is arbitrary but is typically either the reference
	 to the source code that is stored with every CompiledMethod, or an
	 encoding of the method's temporary names."

	^self generate: trailer using: CompiledMethod
</details>

#### MethodNode>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self 
		printSelectorAndArgumentsOn: aStream;
		printCommentOn: aStream;
		printTemporariesOn: aStream;
		ifPrimitivePrintOn: aStream;
		printPropertiesOn: aStream;
		printPragmasOn: aStream.
		
	aStream newLineTab: 1.
	block printStatementsOn: aStream indent: 0
</details>

#### MethodNode>>#allParseNodesWithin: aSourceCodeInterval satisfy: aCondition

<details>
	<summary>See more</summary>
	
	allParseNodesWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ]
				:: ifTrue: [ ^ false ]]].
	
	^ true
</details>

#### MethodNode>>#parseNodeIncluding: aPosition ifAbsent: aBlockClosure

<details>
	<summary>See more</summary>
	
	parseNodeIncluding: aPosition ifAbsent: aBlockClosure 
	
	^encoder parseNodeIncluding: aPosition ifAbsent: aBlockClosure 

</details>

#### MethodNode>>#messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure

<details>
	<summary>See more</summary>
	
	messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure

	^encoder messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aClosure
</details>

#### MethodNode>>#positionsForLiteralVariableNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForLiteralVariableNode: aName ifAbsent: aBlock

	^encoder positionsForLiteralVariableNode: aName ifAbsent: aBlock
</details>

#### MethodNode>>#preenLocalIfNotNilArg

Try and spot a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block and convert it to expr ifNil: [...] ifNotNil: [:var| ...]. Deal both with the pretty-print case where the block already declares the variable and the decompile case where it does not.


<details>
	<summary>See more</summary>
	
	preenLocalIfNotNilArg
	"Try and spot a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| varsToHide |
	varsToHide := Set new.
	self nodesDo:
		[:node| | variable |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not
		and: [variable isOnlySubnodeOf: node in: self]]]]]]]) ifTrue:
			[node arguments last arguments isEmpty
				ifTrue: [node arguments last arguments: { variable }.
						varsToHide add: variable]
				ifFalse: [self assert: node arguments last arguments asArray =  { variable }].
			 node receiver receiver: node receiver receiver value]].
	varsToHide notEmpty ifTrue:
		[self nodesDo:
			[:node|
			((node == self or: [node isBlockNode])
			and: [node temporaries anySatisfy: [:temp| varsToHide includes: temp]]) ifTrue:
				[node temporaries: (node temporaries reject: [:temp| varsToHide includes: temp])]]]
</details>

#### MethodNode>>#sourceText

<details>
	<summary>See more</summary>
	
	sourceText

	^ sourceText ifNil: [self printString]
</details>

#### MethodNode>>#generate

The receiver is the root of a parse tree. Answer a CompiledMethod.


<details>
	<summary>See more</summary>
	
	generate
	"The receiver is the root of a parse tree. Answer a CompiledMethod."

	^self generate: #(0 0 0 0)
</details>

#### MethodNode>>#generate: trailer using: aCompiledMethodClass ifQuick: methodBlock

<details>
	<summary>See more</summary>
	
	generate: trailer using: aCompiledMethodClass ifQuick: methodBlock
	| v |
	(primitive = 0 and: [arguments size = 0 and: [block isQuick]])
		ifFalse: [^ self].
	v := block code.
	v < 0
		ifTrue: [^ self].
	v = LdSelf
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnSelfTrailerBytes: trailer)].
	(v between: LdTrue and: LdMinus1 + 3)
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnConstant: v - LdSelf trailerBytes: trailer)].
	v < ((CodeBases at: LdInstType) + (CodeLimits at: LdInstType))
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v trailerBytes: trailer)].
	v // 256 = 1
		ifTrue: [^ methodBlock value: (aCompiledMethodClass toReturnField: v \\ 256 trailerBytes: trailer)]
</details>

#### MethodNode>>#selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict

Initialize the receiver with respect to the arguments given.


<details>
	<summary>See more</summary>
	
	selector: selOrFalse arguments: args precedence: p temporariesDeclaration: tempsDeclaration block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."
	"RNG: this is the preferred initializer (with temporariesDeclaration) as opposed to the one with 'temporaries' that is going to be deprecated"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporariesDeclaration _ tempsDeclaration.
	temporaries := tempsDeclaration allDeclaredVariableNodes.
	block := blk.
	primitive := prim.
	properties := propDict.
</details>

#### MethodNode>>#rawSourceRanges

<details>
	<summary>See more</summary>
	
	rawSourceRanges

	^self rawSourceRangesAndMethodDo: [:rawSourceRanges :method| rawSourceRanges]
</details>

#### MethodNode>>#hasGeneratedMethod

<details>
	<summary>See more</summary>
	
	hasGeneratedMethod
	^encoder hasGeneratedMethod
</details>

#### MethodNode>>#selectorKeywordsPositions

<details>
	<summary>See more</summary>
	
	selectorKeywordsPositions

	^selectorKeywordsRanges
</details>

#### MethodNode>>#generate: trailer using: aCompiledMethodClass

The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass. The argument, trailer, is arbitrary but is typically the reference to the source code that is stored with every CompiledMethod.


<details>
	<summary>See more</summary>
	
	generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	^ Smalltalk isSpur 
		ifTrue: [ self generateSpur: trailer using: aCompiledMethodClass ]
		ifFalse: [ self generatePreSpur: trailer using: aCompiledMethodClass ]
</details>

#### MethodNode>>#selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range

RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries


<details>
	<summary>See more</summary>
	
	selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict selectorKeywordsRanges: range
	"RNG: deprecated, use one of the methods that receives a temporariesDeclaration instead of the collection of temporaries"
	
	selectorKeywordsRanges := range.
	
	^self selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
</details>

#### MethodNode>>#rawSourceRangesAndMethodDo: aBinaryBlock

Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver.


<details>
	<summary>See more</summary>
	
	rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
	methNode := encoder classEncoding parserClass new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method
</details>

#### MethodNode>>#selectorAndArgumentsAsString

<details>
	<summary>See more</summary>
	
	selectorAndArgumentsAsString
	
	^String streamContents: [ :aStream | self printSelectorAndArgumentsOn: aStream ]
</details>

#### MethodNode>>#noteBlockExit: aBlock

Evaluate aBlock with the numbering for the block exit.


<details>
	<summary>See more</summary>
	
	noteBlockExit: aBlock
	"Evaluate aBlock with the numbering for the block exit."
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
</details>

#### MethodNode>>#argumentNames

<details>
	<summary>See more</summary>
	
	argumentNames

	^arguments collect: [ :anArgumentNode | anArgumentNode name ]
</details>

#### MethodNode>>#referencedValuesWithinBlockExtent: anInterval

<details>
	<summary>See more</summary>
	
	referencedValuesWithinBlockExtent: anInterval 
	^(localsPool select:
		[:temp|
		 temp isReferencedWithinBlockExtent: anInterval]) collect:
			[:temp|
			temp isRemote ifTrue: [temp remoteNode] ifFalse: [temp]]
</details>

#### MethodNode>>#properties

<details>
	<summary>See more</summary>
	
	properties
	^properties
</details>

#### MethodNode>>#temporaries: aCollectionOfTemporaries

For transformations etc, not used in compilation


<details>
	<summary>See more</summary>
	
	temporaries: aCollectionOfTemporaries
	"For transformations etc, not used in compilation"
	"RNG: deprecated in favor of #temporariesDeclaration:"

	temporaries := aCollectionOfTemporaries
</details>

#### MethodNode>>#messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

	^encoder messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock
</details>

#### MethodNode>>#preen

Preen for pretty-printing and/or decompilation. i.e. post-process to cover up for inadequacies in both algorithms. Currently one case, hiding the assignment to the arg of an inlined block arg to ifNotNil:, (var := expr) ifNil: [...] ifNotNil: [...] => expr ifNil: [...] ifNotNil: [:var| ...].


<details>
	<summary>See more</summary>
	
	preen
	"Preen for pretty-printing and/or decompilation.
	 i.e. post-process to cover up for inadequacies in both algorithms.
	 Currently one case, hiding the assignment to the arg of an inlined block arg to ifNotNil:,
		(var := expr) ifNil: [...] ifNotNil: [...]    =>    expr ifNil: [...] ifNotNil: [:var| ...]."

	self preenLocalIfNotNilArg
</details>

#### MethodNode>>#positionsForLiteralNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForLiteralNode: aName ifAbsent: aBlock

	^encoder positionsForLiteralNode: aName ifAbsent: aBlock

</details>

#### MethodNode>>#arguments: aCollectionOfArguments

For transformations etc, not used in compilation


<details>
	<summary>See more</summary>
	
	arguments: aCollectionOfArguments

	"For transformations etc, not used in compilation"
	arguments := aCollectionOfArguments
</details>

#### MethodNode>>#completeSourceRanges

Returns the 'expanded' version of the source ranges, for instance in message sends it also includes the receiver, and if there are parentheses they are included in the source range as well. Right now used for refactorings.


<details>
	<summary>See more</summary>
	
	completeSourceRanges
	"Returns the 'expanded' version of the source ranges, for instance in message sends it also includes the receiver, and if there are parentheses they are included in the source range as well. Right now used for refactorings."

	^ encoder completeSourceRangesBasedOn: self sourceText
</details>

#### MethodNode>>#classAndSelector

<details>
	<summary>See more</summary>
	
	classAndSelector

	^self methodClass name, '>>', self selector storeString
</details>

#### MethodNode>>#addLocalsToPool: locals

<Set of: TempVariableNode>


<details>
	<summary>See more</summary>
	
	addLocalsToPool: locals "<Set of: TempVariableNode>"
	localsPool isNil ifTrue:
		[localsPool := IdentitySet new].
	localsPool addAll: locals
</details>

#### MethodNode>>#printPrimitiveOn: aStream

Print the primitive on aStream


<details>
	<summary>See more</summary>
	
	printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primDecl |
	primitive = 0 ifTrue:
		[^self].
	primitive = 120 ifTrue: "External call spec"
		[^aStream print: encoder literals first].
	aStream nextPutAll: '<primitive: '.
	primitive = 117
		ifTrue:
			[primDecl := encoder literals at: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>.
	((Smalltalk classNamed: #StackInterpreter) ifNil: [Smalltalk classNamed: #Interpreter]) ifNotNil:
		[:interpreterClass|
		 aStream nextPutAll: ' "', ((interpreterClass classPool at: #PrimitiveTable) at: primitive + 1), '" ']
</details>

#### MethodNode>>#rangeForNode: node ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	rangeForNode: node ifAbsent: aBlock

	^encoder rangeForNode: node ifAbsent: aBlock
</details>

#### MethodNode>>#temporaries

For transformations etc, not used in compilation


<details>
	<summary>See more</summary>
	
	temporaries
	"For transformations etc, not used in compilation"

	"RNG: implementation can be changed after adopting the use of temporariesDeclaration inst var.
	After that, the implementation for this message can be changed to:
	^ temporariesDeclaration allDeclaredVariableNodes
	
	Or we can analyze the senders and change the way we request the temporaries"
	
	^temporaries
</details>

#### MethodNode>>#positionsForTemporaryVariable: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForTemporaryVariable: aName ifAbsent: aBlock

	^encoder positionsForTemporaryVariable: aName ifAbsent: aBlock
</details>

#### MethodNode>>#selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict

Initialize the receiver with respect to the arguments given.


<details>
	<summary>See more</summary>
	
	selector: selOrFalse arguments: args precedence: p temporaries: temps block: blk encoder: anEncoder primitive: prim properties: propDict
	"Initialize the receiver with respect to the arguments given."
	"RNG: its external use is deprecated, in favor of any of the methods that receives a temporariesDeclaration instead of the collection of temporaries"

	encoder := anEncoder.
	selectorOrFalse := selOrFalse.
	precedence := p.
	arguments := args.
	temporaries := temps.
	block := blk.
	primitive := prim.
	properties := propDict.
</details>

#### MethodNode>>#addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition

<details>
	<summary>See more</summary>
	
	addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition

	| insidePosition |

	insidePosition := literalArrayPosition first.
	[ insidePosition < literalArrayPosition last ] whileTrue: [
		insidePosition := self nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition ].
	
</details>

#### MethodNode>>#nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition

<details>
	<summary>See more</summary>
	
	nextPositionAfterAddPositionTo: symbolPositions of: symbolString startingAt: insidePosition

	| symbolStartPosition nextPosition |

	symbolStartPosition := sourceText indexOfSubCollection: symbolString startingAt: insidePosition.

	symbolStartPosition = 0
		ifTrue: [ nextPosition := SmallInteger maxVal ]
		ifFalse: [
			nextPosition := symbolStartPosition + symbolString size.
			(sourceText at: nextPosition) tokenish ifFalse: [ symbolPositions add: (symbolStartPosition to: nextPosition - 1) ]].

	^nextPosition

	
</details>

#### MethodNode>>#block

<details>
	<summary>See more</summary>
	
	block
	^ block
</details>

#### MethodNode>>#generateSpur: trailer using: aCompiledMethodClass

The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass. The argument, trailer, is arbitrary but is typically the reference to the source code that is stored with every CompiledMethod.


<details>
	<summary>See more</summary>
	
	generateSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := aCompiledMethodClass
					createMethod: blkSize
					trailer: trailer
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method
</details>

#### MethodNode>>#selector

Answer the message selector for the method represented by the receiver.


<details>
	<summary>See more</summary>
	
	selector 
	"Answer the message selector for the method represented by the receiver."

	(selectorOrFalse isSymbol)
		ifTrue: [^selectorOrFalse].
	^selectorOrFalse key.

</details>

#### MethodNode>>#generatePreSpur: trailer using: aCompiledMethodClass

The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass. The argument, trailer, is arbitrary but is typically the reference to the source code that is stored with every CompiledMethod.


<details>
	<summary>See more</summary>
	
	generatePreSpur: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	| primErrNode blkSize nLits literals stack method |
	self generate: trailer 
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	encoder supportsClosureOpcodes ifTrue:
		[self ensureClosureAnalysisDone.
		 encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"].
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	method := aCompiledMethodClass
				newBytes: blkSize
				trailerBytes: trailer 
				nArgs: arguments size
				nTemps: (encoder supportsClosureOpcodes
							ifTrue: [| locals |
									locals := arguments,
											  temporaries,
											  (primErrNode
												ifNil: [#()]
												ifNotNil: [{primErrNode}]).
									encoder
										noteBlockExtent: block blockExtent
										hasLocals: locals.
									locals size]
							ifFalse: [encoder maxTemp])
				nStack: 0
				nLits: (nLits := (literals := encoder allLiterals) size)
				primitive: primitive.
	nLits > 255 ifTrue:
		[^self error: 'Too many literals referenced'].
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primErrNode ifNotNil: [primErrNode emitCodeForStore: stack encoder: encoder].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method
</details>

#### MethodNode>>#selectorKeywordPositionAt: anIndex

<details>
	<summary>See more</summary>
	
	selectorKeywordPositionAt: anIndex

	^selectorKeywordsRanges at: anIndex
</details>

#### MethodNode>>#hasLocalNamed: aName

See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan


<details>
	<summary>See more</summary>
	
	hasLocalNamed: aName

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 

	^ encoder hasLocalNamed: aName 
</details>

#### MethodNode>>#printSelectorAndArgumentsOn: aStream

<details>
	<summary>See more</summary>
	
	printSelectorAndArgumentsOn: aStream

	| selectorNode |
	
	selectorNode _ self selectorNode.
	precedence = 1
		ifTrue:
			[selectorNode isForFFICall
				ifTrue: [selectorNode
							printAsFFICallWithArguments: arguments
							on: aStream
							indent: 0]
				ifFalse: [aStream nextPutAll: selectorNode key]]
		ifFalse:
			[selectorNode key keywords withIndexDo:
				[:kwd :i | | arg |
				arg _ arguments at: i.
				i = 1 ifFalse: [ aStream space ].
				aStream nextPutAll: kwd; space; nextPutAll: arg key ]].

</details>

#### MethodNode>>#anyParseNodeWithin: aSourceCodeInterval satisfy: aCondition

<details>
	<summary>See more</summary>
	
	anyParseNodeWithin: aSourceCodeInterval satisfy: aCondition

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(aCondition value: parseNode) ifTrue: [
			sourceRanges anySatisfy: [ :sourceRange | aSourceCodeInterval rangeIncludes: sourceRange first ]
				:: ifTrue: [ ^ true ]]].
	
	^ false
</details>

#### MethodNode>>#tempNodes

<details>
	<summary>See more</summary>
	
	tempNodes

	^encoder tempNodes
</details>

#### MethodNode>>#parameterDefinitionPositionAt: anIndex

<details>
	<summary>See more</summary>
	
	parameterDefinitionPositionAt: anIndex

	^encoder parameterDefinitionPositionFor: (arguments at: anIndex)
</details>

#### MethodNode>>#printWithClosureAnalysisOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream 
	self ensureClosureAnalysisDone.
	precedence = 1
		ifTrue: 
			[(self selector includesSubString: '()/')
				ifTrue: [aStream nextPutAll: (self selector copyUpTo: $)).
						arguments
							do: [:arg| aStream nextPutAll: arg key]
							separatedBy: [aStream nextPutAll: ', '].
						aStream nextPut: $)]
				ifFalse: [aStream nextPutAll: self selector]]  "no node for method selector"
		ifFalse: 
			[self selector keywords with: arguments do: 
				[:kwd :arg | 
				aStream nextPutAll: kwd; space.
				arg printDefinitionForClosureAnalysisOn: aStream.
				aStream space]].
	comment == nil ifFalse: [
			aStream newLineTab: 1.
			 self printCommentOn: aStream indent: 1].
	temporaries size > 0 ifTrue: [
			aStream newLineTab: 1; nextPut: $|.
			temporaries do: [:temp | 
				aStream space.
				temp printDefinitionForClosureAnalysisOn: aStream].
			aStream space; nextPut: $|].
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream newLineTab: 1.
			 self printPrimitiveOn: aStream]].
	self printPropertiesOn: aStream.
	self printPragmasOn: aStream.
	aStream newLineTab: 1.
	block printWithClosureAnalysisStatementsOn: aStream indent: 0
</details>

#### MethodNode>>#encoder

<details>
	<summary>See more</summary>
	
	encoder
	^ encoder
</details>

#### MethodNode>>#removeProperty: aSymbol

<details>
	<summary>See more</summary>
	
	removeProperty: aSymbol
	properties := properties copyWithout: (Association
											key: aSymbol
											value: (properties propertyValueAt: aSymbol))
</details>

#### MethodNode>>#positionsInLiteralArrayOf: aSymbol

<details>
	<summary>See more</summary>
	
	positionsInLiteralArrayOf: aSymbol

	| literalArrayPositions |

	literalArrayPositions := encoder positionsOfLiteralArraysContaining: aSymbol.

	^self positionsOf: aSymbol printString containedIn: literalArrayPositions.


</details>

#### MethodNode>>#noteBlockEntry: aBlock

Evaluate aBlock with the numbering for the block entry.


<details>
	<summary>See more</summary>
	
	noteBlockEntry: aBlock
	"Evaluate aBlock with the numbering for the block entry."
	locationCounter isNil ifTrue:
		[locationCounter := -1].
	aBlock value: locationCounter + 1.
	locationCounter := locationCounter + 2
</details>

#### MethodNode>>#selectorNode

Answer a SelectorNode for the message selector of the method represented by the receiver.


<details>
	<summary>See more</summary>
	
	selectorNode
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: SelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [SelectorNode new key: selectorOrFalse]
</details>

#### MethodNode>>#parseNodesPathAt: aPosition ifAbsent: aBlockClosure

<details>
	<summary>See more</summary>
	
	parseNodesPathAt: aPosition ifAbsent: aBlockClosure

	^ encoder
		parseNodesPathAt: aPosition
		using: self completeSourceRanges
		ifAbsent: aBlockClosure
</details>

#### MethodNode>>#parserClass

Which parser produces this class of parse node


<details>
	<summary>See more</summary>
	
	parserClass
	"Which parser produces this class of parse node"

	^ Parser
</details>

#### MethodNode>>#positionsOf: symbolString containedIn: literalArrayPositions

<details>
	<summary>See more</summary>
	
	positionsOf: symbolString containedIn: literalArrayPositions

	| symbolPositions |

	symbolPositions := OrderedCollection new.

	literalArrayPositions do: [ :literalArrayPosition | self addPositionTo: symbolPositions of: symbolString inside: literalArrayPosition ].

	^symbolPositions





</details>

#### MethodNode>>#ifPrimitivePrintOn: aStream

<details>
	<summary>See more</summary>
	
	ifPrimitivePrintOn: aStream
	
	primitive > 0 ifTrue:
		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"
			[aStream newLineTab: 1.
			 self printPrimitiveOn: aStream]].

</details>

#### MethodNode>>#printPragmasOn: aStream

<details>
	<summary>See more</summary>
	
	printPragmasOn: aStream
	properties ifNil: [^self].
	properties pragmas do: [ :pragma |
		"Primitives are printed in printPrimitiveOn:; skip these"
		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:
			[aStream newLineTab: 1.
			 pragma printOn: aStream]]
</details>

#### MethodNode>>#completeSourceRangesDo: aBinaryBlock

block has to receive parse node and collection of source ranges


<details>
	<summary>See more</summary>
	
	completeSourceRangesDo: aBinaryBlock
	"block has to receive parse node and collection of source ranges"

	^ self completeSourceRanges keysAndValuesDo: aBinaryBlock
</details>

#### MethodNode>>#isMultipleRanges: aRangeOrRanges

<details>
	<summary>See more</summary>
	
	isMultipleRanges: aRangeOrRanges

	^aRangeOrRanges isKindOf: OrderedCollection 
</details>

#### MethodNode>>#singleCompleteSourceRangeOf: requestedParseNode

Returns the source range associated with the requested parse node. Fails if there is no source range, or if there are multiple source ranges.


<details>
	<summary>See more</summary>
	
	singleCompleteSourceRangeOf: requestedParseNode
	"Returns the source range associated with the requested parse node.
	Fails if there is no source range, or if there are multiple source ranges."

	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode equivalentTo: requestedParseNode) ifTrue: [
			sourceRanges size > 1 ifTrue: [ self error: 'there are multiple source ranges for this parse node' ].
			^ sourceRanges first ] ].
	self error: 'could not find source range for this parse node'
</details>

#### MethodNode>>#messageSendLastPositionsOf: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendLastPositionsOf: aSelector ifAbsent: aBlock

	^encoder messageSendLastPositionsOf: aSelector ifAbsent: aBlock
</details>

#### MethodNode>>#selector: symbol

<details>
	<summary>See more</summary>
	
	selector: symbol

	selectorOrFalse := symbol
</details>

#### MethodNode>>#printPropertiesOn: aStream

<details>
	<summary>See more</summary>
	
	printPropertiesOn: aStream
	properties ifNil: [^self].
	properties propertyKeysAndValuesDo:
		[:prop :val|
		aStream newLine; tab; nextPut: $<.
		prop = #on:in:
			ifTrue:
				[prop keywords with: val do:
					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]
			ifFalse:
				[prop = #on
					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 
					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 
		aStream nextPut: $>]
</details>

#### MethodNode>>#generate: trailer ifQuick: methodBlock

<details>
	<summary>See more</summary>
	
	generate: trailer ifQuick: methodBlock
	^self generate: trailer using: CompiledMethod ifQuick: methodBlock
</details>

#### MethodNode>>#temporariesDeclaration

<details>
	<summary>See more</summary>
	
	temporariesDeclaration

	^ temporariesDeclaration
</details>

#### MethodNode>>#decompileString

Answer a string description of the parse tree whose root is the receiver.


<details>
	<summary>See more</summary>
	
	decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^self fullPrintString

</details>

#### MethodNode>>#removeAndRenameLastTempIfErrorCode

<details>
	<summary>See more</summary>
	
	removeAndRenameLastTempIfErrorCode
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 temporaries last
			name: primitiveErrorVariableName
			key: primitiveErrorVariableName
			code: temporaries last code.
		 temporaries removeLast].
</details>

#### MethodNode>>#positionsForInstanceVariable: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForInstanceVariable: aName ifAbsent: aBlock

	^encoder positionsForInstanceVariable: aName ifAbsent: aBlock

</details>

#### MethodNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitMethodNode: self
</details>

#### MethodNode>>#sourceText: stringOrText

<details>
	<summary>See more</summary>
	
	sourceText: stringOrText

	sourceText := stringOrText
</details>

#### MethodNode>>#hasArgumentOrTemporaryNamed: aVariableName

See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan


<details>
	<summary>See more</summary>
	
	hasArgumentOrTemporaryNamed: aVariableName

	"See #hasLocallyArgumentOrTemporaryNamed: comment - Hernan" 
	
	^self tempNames includes: aVariableName
</details>

#### MethodNode>>#printCommentOn: aStream

<details>
	<summary>See more</summary>
	
	printCommentOn: aStream
	
	comment ifNotNil: [
		aStream newLineTab: 1.
		self printCommentOn: aStream indent: 1].
</details>

#### MethodNode>>#withParseNodeIncluding: aPosition do: aBlock ifAbsent: anAbsentBlock

<details>
	<summary>See more</summary>
	
	withParseNodeIncluding: aPosition do: aBlock ifAbsent: anAbsentBlock

	| nodeAndPosition |

	nodeAndPosition :=self parseNodeIncluding: aPosition ifAbsent: [ ^ anAbsentBlock value ].
	^aBlock value: nodeAndPosition key.
</details>

## NewArrayNode

I represent a node for the genPushNewArray: opcode.

### Methods
#### NewArrayNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"This is a no-op except in TempVariableNode"
	^self
</details>

#### NewArrayNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	encoder genPushNewArray: numElements.
	stack push: 1
</details>

#### NewArrayNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitNewArrayNode: self
</details>

#### NewArrayNode>>#numElements: n

<details>
	<summary>See more</summary>
	
	numElements: n
	numElements := n
</details>

#### NewArrayNode>>#numElements

<details>
	<summary>See more</summary>
	
	numElements
	^numElements
</details>

#### NewArrayNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	^encoder sizePushNewArray: numElements
</details>

## ParseNode

This superclass of most compiler/decompiler classes declares common class variables, default messages, and the code emitters for jumps. Some of the class variables are initialized here; the rest are initialized in class VariableNode.

### Methods
#### ParseNode>>#asReturnNode

<details>
	<summary>See more</summary>
	
	asReturnNode

	^ReturnNode new expr: self
</details>

#### ParseNode>>#printOn: aStream indent: anInteger

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop."

	super printOn: aStream
</details>

#### ParseNode>>#isInstanceVariableNode

<details>
	<summary>See more</summary>
	
	isInstanceVariableNode

	^false
</details>

#### ParseNode>>#isLiteralVariableNode

<details>
	<summary>See more</summary>
	
	isLiteralVariableNode

	^ false
</details>

#### ParseNode>>#nodesDo: aBlock

<details>
	<summary>See more</summary>
	
	nodesDo: aBlock
	self accept: (ParseNodeEnumerator ofBlock: aBlock)
</details>

#### ParseNode>>#isAssignmentToTemporary

<details>
	<summary>See more</summary>
	
	isAssignmentToTemporary

	^ false
</details>

#### ParseNode>>#isConstantNumber

Overridden in LiteralNode


<details>
	<summary>See more</summary>
	
	isConstantNumber  "Overridden in LiteralNode"
	^false
</details>

#### ParseNode>>#expandRange: aSourceRange basedOn: sourceRangesOfChildNode

<details>
	<summary>See more</summary>
	
	expandRange: aSourceRange basedOn: sourceRangesOfChildNode

	| intervals |
	intervals _ sourceRangesOfChildNode isInterval
		ifTrue: [ OrderedCollection with: sourceRangesOfChildNode ] ifFalse: [ sourceRangesOfChildNode ].
	intervals withIndexDo: [ :interval :index |
		(interval first > aSourceRange first) ifTrue: [
			^ (aSourceRange first min: (intervals at: index - 1 ifAbsent: [ intervals last ]) first) to: aSourceRange last ] ].
	^ (aSourceRange first min: intervals last first) to: aSourceRange last
</details>

#### ParseNode>>#isMessage: selSymbol receiver: rcvrPred arguments: argsPred

See comment in MessageNode.


<details>
	<summary>See more</summary>
	
	isMessage: selSymbol receiver: rcvrPred arguments: argsPred
	"See comment in MessageNode."

	^false
</details>

#### ParseNode>>#isSelfNewMessageSend

Overridden in MessageNode.


<details>
	<summary>See more</summary>
	
	isSelfNewMessageSend
	"Overridden in  MessageNode."
	^false
</details>

#### ParseNode>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printOn: aStream indent: 0.
	aStream nextPut: $}.
</details>

#### ParseNode>>#isMessageNode

<details>
	<summary>See more</summary>
	
	isMessageNode
	^false
</details>

#### ParseNode>>#printWithClosureAnalysisOn: aStream indent: anInteger

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: anInteger 
	"If control gets here, avoid recursion loop."

	super printWithClosureAnalysisOn: aStream
</details>

#### ParseNode>>#printOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level precedence: p

	self printOn: aStream indent: level
</details>

#### ParseNode>>#isBraceNode

<details>
	<summary>See more</summary>
	
	isBraceNode

	^ false
</details>

#### ParseNode>>#isNilPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isNilPseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#printCommentOn: aStream indent: indent

<details>
	<summary>See more</summary>
	
	printCommentOn: aStream indent: indent 
	| thisComment |
	self comment == nil ifTrue: [^ self].
	1 to: self comment size do: [ :index |
		index > 1 ifTrue: [aStream newLineTab: indent].
		aStream nextPut: $".
		thisComment := self comment at: index.
		self printSingleComment: thisComment
			on: aStream
			indent: indent.
		aStream nextPut: $"]
</details>

#### ParseNode>>#isAssignmentNode

<details>
	<summary>See more</summary>
	
	isAssignmentNode
	^false
</details>

#### ParseNode>>#printWithClosureAnalysis

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysis

	^String streamContents: [:str| self printWithClosureAnalysisOn: str]
</details>

#### ParseNode>>#nodePrintOn: aStrm indent: nn

Show just the sub nodes and the code.


<details>
	<summary>See more</summary>
	
	nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := WriteStream on: (String new: 500)].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString withBlanksCondensed.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; newLine.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isCollection and: [var isSequenceable]) ifTrue: [
			var do: [ :aNode | 
				(aNode respondsTo: #asReturnNode) ifTrue: [
					aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm
</details>

#### ParseNode>>#encodeSelector: selector

<details>
	<summary>See more</summary>
	
	encodeSelector: selector

	^nil
</details>

#### ParseNode>>#printWithClosureAnalysisOn: aStream indent: level precedence: p

<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level precedence: p

	self printWithClosureAnalysisOn: aStream indent: level
</details>

#### ParseNode>>#isReturnSelf

<details>
	<summary>See more</summary>
	
	isReturnSelf

	^false
</details>

#### ParseNode>>#expandIfEnclosed: sourceRange on: sourceCode

takes a source range and a source code and if the source range represents an expression that can be expanded and still is valid, it returns the source range 'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*


<details>
	<summary>See more</summary>
	
	expandIfEnclosed: sourceRange on: sourceCode
	"takes a source range and a source code and if the source range represents an
	expression that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstChar lastChar |
	firstChar _ sourceCode at: sourceRange first - 1 ifAbsent: [ nil ].
	lastChar _ sourceCode at: sourceRange last + 1 ifAbsent: [ nil ].
	^ ((firstChar = $( and: [ lastChar = $) ])
		or: [ firstChar = $` and: [ lastChar = $` ] ])
			ifTrue: [ sourceRange first - 1 to: sourceRange last + 1 ]
			ifFalse: [ sourceRange ]
</details>

#### ParseNode>>#isCascadeNode

<details>
	<summary>See more</summary>
	
	isCascadeNode

	^ false
</details>

#### ParseNode>>#currentValueIn: aContext

<details>
	<summary>See more</summary>
	
	currentValueIn: aContext

	^nil
</details>

#### ParseNode>>#comment: newComment

<details>
	<summary>See more</summary>
	
	comment: newComment

	comment := newComment
</details>

#### ParseNode>>#isFalsePseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isFalsePseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#isUnusedTemp

<details>
	<summary>See more</summary>
	
	isUnusedTemp
	^ false
</details>

#### ParseNode>>#emitCodeForBranchOn: condition dist: dist pop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForBranchOn: condition dist: dist pop: stack encoder: encoder
	stack pop: 1.
	dist = 0 ifTrue: [^encoder genPop].
	condition
		ifTrue: [encoder genBranchPopTrue: dist]
		ifFalse: [encoder genBranchPopFalse: dist]
</details>

#### ParseNode>>#isOnlySubnodeOf: aSubtree "<ParseNode>" in: aParseTree

<ParseNode>


<details>
	<summary>See more</summary>
	
	isOnlySubnodeOf: aSubtree "<ParseNode>" in: aParseTree "<ParseNode>"
	"Answer if the receiver only occurs within aSubtree of aParseTree, not in the rest of aParseTree.
	 Assumes that aSubtree is in fact a subnode of aParseTree."
	| isSubnode |
	isSubnode := false.
	aSubtree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [isSubnode := true]]).
	isSubnode ifFalse:
		[^false].
	aParseTree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [^false]]
							select: [:node| node ~= aSubtree]).
	^true
</details>

#### ParseNode>>#isReturn

<details>
	<summary>See more</summary>
	
	isReturn

	^false
</details>

#### ParseNode>>#isVariableReference

<details>
	<summary>See more</summary>
	
	isVariableReference

	^false
</details>

#### ParseNode>>#isJust: node

<details>
	<summary>See more</summary>
	
	isJust: node
	^false
</details>

#### ParseNode>>#pc: anInteger

Used by encoder source mapping.


<details>
	<summary>See more</summary>
	
	pc: anInteger
	"Used by encoder source mapping."

	pc := anInteger
</details>

#### ParseNode>>#isTruePseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isTruePseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#sizeCodeForBlockValue: encoder

Answer the size for evaluating the last statement in a block


<details>
	<summary>See more</summary>
	
	sizeCodeForBlockValue: encoder
	"Answer the size for evaluating the last statement in a block"
	^self sizeCodeForValue: encoder
</details>

#### ParseNode>>#ensureCanCascade: encoder

<details>
	<summary>See more</summary>
	
	ensureCanCascade: encoder
</details>

#### ParseNode>>#emitCodeForJump: dist encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForJump: dist encoder: encoder

	dist = 0 ifFalse: [encoder genJump: dist]
</details>

#### ParseNode>>#assignmentCheck: encoder at: location

For messageNodes masquerading as variables for the debugger. For now we let this through - ie we allow stores ev into args. Should check against numArgs, though.


<details>
	<summary>See more</summary>
	
	assignmentCheck: encoder at: location
	"For messageNodes masquerading as variables for the debugger.
	For now we let this through - ie we allow stores ev
	into args.  Should check against numArgs, though."
	^ -1
</details>

#### ParseNode>>#printSingleComment: aString on: aStream indent: indent

Print the comment string


<details>
	<summary>See more</summary>
	
	printSingleComment: aString on: aStream indent: indent 
	"Print the comment string"
	
	aStream nextPutAll: aString
</details>

#### ParseNode>>#isSelfBasicNewMessageSend

Overridden in MessageNode.


<details>
	<summary>See more</summary>
	
	isSelfBasicNewMessageSend
	"Overridden in  MessageNode."
	^false
</details>

#### ParseNode>>#isTemporaryDeclaration

<details>
	<summary>See more</summary>
	
	isTemporaryDeclaration

	^ false
</details>

#### ParseNode>>#sizeCodeForEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEffect: encoder

	^(self sizeCodeForValue: encoder) + encoder sizePop
</details>

#### ParseNode>>#sizeCode: encoder forBranchOn: condition dist: dist

<details>
	<summary>See more</summary>
	
	sizeCode: encoder forBranchOn: condition dist: dist
	dist = 0 ifTrue: [^encoder sizePop].
	^condition
		ifTrue: [encoder sizeBranchPopTrue: dist]
		ifFalse: [encoder sizeBranchPopFalse: dist]
</details>

#### ParseNode>>#canCascade

<details>
	<summary>See more</summary>
	
	canCascade

	^false
</details>

#### ParseNode>>#consolidateAsCollection: sourceRanges

<details>
	<summary>See more</summary>
	
	consolidateAsCollection: sourceRanges

	^ sourceRanges isInterval
		ifTrue: [ OrderedCollection with: sourceRanges ]
		ifFalse: [ sourceRanges ]
</details>

#### ParseNode>>#isTemp

<details>
	<summary>See more</summary>
	
	isTemp
	^ false
</details>

#### ParseNode>>#expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

<details>
	<summary>See more</summary>
	
	expandRanges: sourceRanges basedOn: allSourceRanges using: sourceCode

	^ (self consolidateAsCollection: sourceRanges)
		collect: [ :sourceRange | self expandIfEnclosed: sourceRange on: sourceCode ]
</details>

#### ParseNode>>#isComplex

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	isComplex
	"Used for pretty printing to determine whether to start a new line"

	^false
</details>

#### ParseNode>>#isUndefTemp

<details>
	<summary>See more</summary>
	
	isUndefTemp
	^ false
</details>

#### ParseNode>>#isSelfPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isSelfPseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#comment

<details>
	<summary>See more</summary>
	
	comment

	^comment
</details>

#### ParseNode>>#emitCodeForReturn: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForReturn: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genReturnTop
</details>

#### ParseNode>>#isSpecialConstant

<details>
	<summary>See more</summary>
	
	isSpecialConstant
	^ false
</details>

#### ParseNode>>#printWithClosureAnalysisOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printWithClosureAnalysisOn: aStream indent: 0.
	aStream nextPut: $}.
</details>

#### ParseNode>>#emitCodeForBlockValue: stack encoder: encoder

Generate code for evaluating the last statement in a block


<details>
	<summary>See more</summary>
	
	emitCodeForBlockValue: stack encoder: encoder
	"Generate code for evaluating the last statement in a block"
	^self emitCodeForValue: stack encoder: encoder
</details>

#### ParseNode>>#isBlockNode

<details>
	<summary>See more</summary>
	
	isBlockNode
	^false
</details>

#### ParseNode>>#optimizedBlockHoistTempsInto: scopeBlock

<BlockNode>


<details>
	<summary>See more</summary>
	
	optimizedBlockHoistTempsInto: scopeBlock "<BlockNode>" 
	"This is a No-op for all nodes except non-optimized BlockNodes."
	^self
</details>

#### ParseNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ false
</details>

#### ParseNode>>#isLiteralNode

<details>
	<summary>See more</summary>
	
	isLiteralNode

	^ false
</details>

#### ParseNode>>#emitCodeForEffect: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEffect: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genPop.
	stack pop: 1
</details>

#### ParseNode>>#isArg

<details>
	<summary>See more</summary>
	
	isArg

	^false
</details>

#### ParseNode>>#isThisContextPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isThisContextPseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#nowHasRef

Ignored in all but VariableNode


<details>
	<summary>See more</summary>
	
	nowHasRef  "Ignored in all but VariableNode"
</details>

#### ParseNode>>#isMessageNamed: aSelector

<details>
	<summary>See more</summary>
	
	isMessageNamed: aSelector

	^false
</details>

#### ParseNode>>#printsInNewLine

Used for pretty printing to determine whether to start a new line


<details>
	<summary>See more</summary>
	
	printsInNewLine
	"Used for pretty printing to determine whether to start a new line"

	^self isComplex
</details>

#### ParseNode>>#isTemporariesDeclaration

<details>
	<summary>See more</summary>
	
	isTemporariesDeclaration

	^ false
</details>

#### ParseNode>>#isSelectorNode

<details>
	<summary>See more</summary>
	
	isSelectorNode

	^ false
</details>

#### ParseNode>>#isReturningIf

<details>
	<summary>See more</summary>
	
	isReturningIf

	^false
</details>

#### ParseNode>>#isVariableNode

<details>
	<summary>See more</summary>
	
	isVariableNode
	^false
</details>

#### ParseNode>>#nowHasDef

Ignored in all but VariableNode


<details>
	<summary>See more</summary>
	
	nowHasDef  "Ignored in all but VariableNode"
</details>

#### ParseNode>>#pc

Used by encoder source mapping.


<details>
	<summary>See more</summary>
	
	pc
	"Used by encoder source mapping."

	^pc ifNil: [ 0 ]

</details>

#### ParseNode>>#isSuperPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isSuperPseudoVariable	
	"Overridden in VariableNode."
	^false
</details>

#### ParseNode>>#toDoIncrement: ignored

Only meant for Messages or Assignments - else return nil


<details>
	<summary>See more</summary>
	
	toDoIncrement: ignored
	"Only meant for Messages or Assignments - else return nil"
	^ nil
</details>

#### ParseNode>>#isTempOrArg

<details>
	<summary>See more</summary>
	
	isTempOrArg

	^false
</details>

#### ParseNode>>#isMessage

<details>
	<summary>See more</summary>
	
	isMessage
	^false
</details>

#### ParseNode>>#nextWordFrom: aStream setCharacter: aBlock

<details>
	<summary>See more</summary>
	
	nextWordFrom: aStream setCharacter: aBlock

	| outStream char |
	outStream _ WriteStream on: (String new: 16).
	[ (aStream peekFor: Character space) or: [ aStream peekFor: Character tab ]] whileTrue.
	[ aStream atEnd or: [
		char _ aStream next.
		char isSeparator ]]
			whileFalse: [ outStream nextPut: char ].
	aBlock value: char.
	^ outStream contents
</details>

#### ParseNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	"Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:.
	 All such implementations under ParseNode should answer the result of the dispatch, e.g.
		^aVisitor visitBlockNode: self"
	^self subclassResponsibility
</details>

#### ParseNode>>#ifNilReceiver

assuming this object is the receiver of an ifNil:, what object is being asked about?


<details>
	<summary>See more</summary>
	
	ifNilReceiver
	"assuming this object is the receiver of an ifNil:, what object is being asked about?"
	^self
</details>

#### ParseNode>>#sizeCode: encoder forJump: dist

<details>
	<summary>See more</summary>
	
	sizeCode: encoder forJump: dist

	^dist = 0 ifTrue: [0] ifFalse: [encoder sizeJump: dist]
</details>

#### ParseNode>>#sizeCodeForReturn: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForReturn: encoder

	^(self sizeCodeForValue: encoder) + encoder sizeReturnTop
</details>

## RemoteTempVectorNode

I am a node for a vector of remote temps, created to share temps between closures when those temps are written to in closures other than their defining ones.

### Methods
#### RemoteTempVectorNode>>#addRemoteTemp: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	addRemoteTemp: aTempVariableNode encoder: encoder
	remoteTemps isNil ifTrue:
		[remoteTemps := OrderedCollection new].
	remoteTemps addLast: aTempVariableNode.
	aTempVariableNode referenceScopesAndIndicesDo:
		[:scopeBlock "<BlockNode>" :location "<Integer>"|
		 self addReadWithin: scopeBlock at: location].
	encoder supportsClosureOpcodes ifFalse:
		[encoder encodeLiteral: remoteTemps size.
		 readNode := encoder encodeSelector: #at:.
		 writeNode := encoder encodeSelector: #at:put:]
</details>

#### RemoteTempVectorNode>>#sizeCodeForIndexOf: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForIndexOf: aTempVariableNode encoder: encoder
	self assert: encoder supportsClosureOpcodes not.
	^(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode)) sizeCodeForValue: encoder
</details>

#### RemoteTempVectorNode>>#sizeCodeForLoadFor: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForLoadFor: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^0].
	"Need to size the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	^(super sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)
</details>

#### RemoteTempVectorNode>>#printDefinitionForClosureAnalysisOn: aStream

<details>
	<summary>See more</summary>
	
	printDefinitionForClosureAnalysisOn: aStream 
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes ifNotNil: [
		refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asSortedCollection do: [:read| aStream nextPutAll: ' r@'; print: read]].
	remoteTemps
		do: [:rt| rt printDefinitionForClosureAnalysisOn: aStream]
		separatedBy: [aStream nextPut: $,; space].
	aStream nextPut: $}
</details>

#### RemoteTempVectorNode>>#emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^self].
	"Need to generate the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	super emitCodeForValue: stack encoder: encoder.
	self emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder
</details>

#### RemoteTempVectorNode>>#sizeCodeForStoreInto: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStoreInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^writeNode sizeCode: encoder args: 2 super: false
</details>

#### RemoteTempVectorNode>>#referenceScopesAndIndicesDo: aBinaryBlock

Evaluate aBinaryBlock with all read or write scopes and locations. This is used to copy the reference information into RemoteTempVectorNodes


<details>
	<summary>See more</summary>
	
	referenceScopesAndIndicesDo: aBinaryBlock
	self shouldNotImplement
</details>

#### RemoteTempVectorNode>>#isIndirectTempVector

<details>
	<summary>See more</summary>
	
	isIndirectTempVector
	^true
</details>

#### RemoteTempVectorNode>>#scope

Answer scope of temporary variables. Currently only the following distinctions are made: 0 outer level: args and user-declared temps 1 block args and doLimiT temps -1 a block temp that is no longer active -2 a block temp that held limit of to:do: -3 an indirect temp vector


<details>
	<summary>See more</summary>
	
	scope
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:
		-3	an indirect temp vector"
	^-3
</details>

#### RemoteTempVectorNode>>#remoteTemps: anArray

<details>
	<summary>See more</summary>
	
	remoteTemps: anArray
	remoteTemps := anArray.
	anArray do: [:tempNode| tempNode remoteNode: self]
</details>

#### RemoteTempVectorNode>>#nodeToInitialize: encoder

<details>
	<summary>See more</summary>
	
	nodeToInitialize: encoder
	^AssignmentNode new
		variable: self
		value: (encoder supportsClosureOpcodes
					ifTrue: [NewArrayNode new numElements: remoteTemps size]
					ifFalse:
						[MessageNode new
							receiver: (encoder encodeVariable: 'Array')
							selector: #new:
							arguments: (Array with: (encoder encodeLiteral: remoteTemps size))
							precedence: 3
							from: encoder])
</details>

#### RemoteTempVectorNode>>#remoteTemps

<details>
	<summary>See more</summary>
	
	remoteTemps
	^remoteTemps
</details>

#### RemoteTempVectorNode>>#emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genPushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index.
			 stack push: 1]
		ifFalse:
			[self emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder.
			 readNode
				emitCode: stack
				args: 1
				encoder: encoder
				super: false]
</details>

#### RemoteTempVectorNode>>#sizeCodeForValueOf: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValueOf: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizePushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)
	+ (readNode sizeCode: encoder args: 1 super: false)
</details>

#### RemoteTempVectorNode>>#emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[writeNode
				emitCode: stack
				args: 2
				encoder: encoder
				super: false]
</details>

#### RemoteTempVectorNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitRemoteTempVectorNode: self
</details>

#### RemoteTempVectorNode>>#emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder
	self assert: encoder supportsClosureOpcodes not.
	(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode))
		emitCodeForValue: stack encoder: encoder
</details>

#### RemoteTempVectorNode>>#sizeCodeForStorePopInto: aTempVariableNode encoder: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePopInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForStoreInto: aTempVariableNode encoder: encoder)
	+ encoder sizePop
</details>

#### RemoteTempVectorNode>>#emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[self emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder.
			 encoder genPop].
	stack pop: 1
</details>

## ReturnNode

I represent an expression of the form ^expr.

### Methods
#### ReturnNode>>#asReturnNode

<details>
	<summary>See more</summary>
	
	asReturnNode
</details>

#### ReturnNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printOn: aStream indent: level.
	expr printCommentOn: aStream indent: level
</details>

#### ReturnNode>>#expr: e encoder: encoder sourceRange: range

<details>
	<summary>See more</summary>
	
	expr: e encoder: encoder sourceRange: range

	expr := e.
	encoder noteSourceRange: range forNode: self
</details>

#### ReturnNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
</details>

#### ReturnNode>>#isReturnSelf

<details>
	<summary>See more</summary>
	
	isReturnSelf

	^expr == NodeSelf
</details>

#### ReturnNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isReturn and: [ expr equivalentTo: aParseNode expr ]
</details>

#### ReturnNode>>#isReturn

<details>
	<summary>See more</summary>
	
	isReturn

	^true
</details>

#### ReturnNode>>#isImplicitSelfReturnIn: aMethodNode

<details>
	<summary>See more</summary>
	
	isImplicitSelfReturnIn: aMethodNode

	^self isReturnSelf and: [ (aMethodNode encoder rawSourceRanges includesKey: expr) not ]
</details>

#### ReturnNode>>#isVariableReference

<details>
	<summary>See more</summary>
	
	isVariableReference

	^expr isVariableReference
</details>

#### ReturnNode>>#emitCodeForReturn: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForReturn: stack encoder: encoder

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
</details>

#### ReturnNode>>#isSpecialConstant

<details>
	<summary>See more</summary>
	
	isSpecialConstant

	^expr isSpecialConstant
</details>

#### ReturnNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Note we could do this:
		scopeBlock ~~ rootNode block ifTrue:
			[scopeBlock noteNonLocalReturn].
	 and pass up the flag in <BlockNode>>>analyseTempsWithin:rootNode:
	 which may be fast but will also give less information the debugger.
	 For now we consider clean blocks a premature optimization."
	self flag: 'consider clean blocks'.
	expr analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools
</details>

#### ReturnNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level

	aStream nextPutAll: '^ '. "make this a preference??"
	expr printWithClosureAnalysisOn: aStream indent: level.
	expr printCommentOn: aStream indent: level
</details>

#### ReturnNode>>#expr

<details>
	<summary>See more</summary>
	
	expr

	^ expr.

</details>

#### ReturnNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder

	^expr sizeCodeForReturn: encoder
</details>

#### ReturnNode>>#expr: e

<details>
	<summary>See more</summary>
	
	expr: e

	expr := e
</details>

#### ReturnNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitReturnNode: self
</details>

#### ReturnNode>>#sizeCodeForReturn: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForReturn: encoder

	^expr sizeCodeForReturn: encoder
</details>

#### ReturnNode>>#code

<details>
	<summary>See more</summary>
	
	code

	^expr code
</details>

## SelectorNode

I am a parse tree leaf representing a selector.

### Methods
#### SelectorNode>>#name: literal key: object index: i type: type

For compatibility with Encoder>>name:key:class:type:set:


<details>
	<summary>See more</summary>
	
	name: literal key: object index: i type: type
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
</details>

#### SelectorNode>>#emitCodeForEffect: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForEffect: stack encoder: encoder

	self shouldNotImplement
</details>

#### SelectorNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder

	self shouldNotImplement
</details>

#### SelectorNode>>#sizeCode: encoder args: nArgs super: supered

<details>
	<summary>See more</summary>
	
	sizeCode: encoder args: nArgs super: supered
	self reserve: encoder.
	^supered
		ifTrue:
			[code < Send "i.e. its a special selector" ifTrue:
				[code := self code: (index := encoder sharableLitIndex: key) type: 5].
			 encoder sizeSendSuper: index numArgs: nArgs]
		ifFalse:
			[self flag: #yuck. "special selector sends cause this problem"
			 encoder
				sizeSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
</details>

#### SelectorNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])
</details>

#### SelectorNode>>#isPvtSelector

Answer if this selector node is a private message selector.


<details>
	<summary>See more</summary>
	
	isPvtSelector
	"Answer if this selector node is a private message selector."

	^key isPvtSelector
</details>

#### SelectorNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isSelectorNode and: [ super equivalentTo: aParseNode ]
</details>

#### SelectorNode>>#isForFFICall

<details>
	<summary>See more</summary>
	
	isForFFICall
	^key includesSubString: '()/'
</details>

#### SelectorNode>>#isSelectorNode

<details>
	<summary>See more</summary>
	
	isSelectorNode

	^ true
</details>

#### SelectorNode>>#key: aSelector

This is for printing of FFI selectors.


<details>
	<summary>See more</summary>
	
	key: aSelector
	"This is for printing of FFI selectors."
	key := aSelector
</details>

#### SelectorNode>>#isInitializePvtSelector

Answer whether the receiver is a private instance initialization message selector


<details>
	<summary>See more</summary>
	
	isInitializePvtSelector
	"Answer whether the receiver is a private instance initialization message selector"

	^ key isInitializePvtSelector
</details>

#### SelectorNode>>#printAsFFICallWithArguments: aSequence on: aStream indent: level

<details>
	<summary>See more</summary>
	
	printAsFFICallWithArguments: aSequence on: aStream indent: level
	aStream nextPutAll: (key copyUpTo: $)).
	aSequence
		do: [:arg| arg printOn: aStream indent: level]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)
</details>

#### SelectorNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])
</details>

#### SelectorNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder

	self shouldNotImplement
</details>

#### SelectorNode>>#emitCode: stack args: nArgs encoder: encoder super: supered

<details>
	<summary>See more</summary>
	
	emitCode: stack args: nArgs encoder: encoder super: supered
	stack pop: nArgs.
	^supered
		ifTrue:
			[encoder genSendSuper: index numArgs: nArgs]
		ifFalse:
			[encoder
				genSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
</details>

#### SelectorNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitSelectorNode: self
</details>

#### SelectorNode>>#reserve: encoder

If this is a yet unused literal of type -code, reserve it.


<details>
	<summary>See more</summary>
	
	reserve: encoder 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder sharableLitIndex: key) type: 0 - code]
</details>

#### SelectorNode>>#emitCode: stack args: nArgs encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCode: stack args: nArgs encoder: encoder

	self emitCode: stack
		args: nArgs
		encoder: encoder
		super: false
</details>

#### SelectorNode>>#sizeCodeForEffect: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForEffect: encoder

	self shouldNotImplement
</details>

## SpecialSelectorNode

A SpecialSelectorNode is a subclass of SelectorNode that handles the special selectors, a high static and/or dynamic frequency set of selectors that are assigned their own bytecodes. Special selectors both save space in the literal frame and allow an interpreter to implement these sends directly for certain classes of receiver and argument, for example the SmallIntegers, a technique known as static receiver prediction.

### Methods
#### SpecialSelectorNode>>#sizeCode: encoder args: nArgs super: supered

Size a special selector send. A super send of a special selector must be handled like a normal send.


<details>
	<summary>See more</summary>
	
	sizeCode: encoder args: nArgs super: supered
	"Size a special selector send.
	 A super send of a special selector must be handled like a normal send."
	^supered
		ifTrue: [super sizeCode: encoder args: nArgs super: supered]
		ifFalse: [encoder sizeSendSpecial: code numArgs: nArgs]
</details>

#### SpecialSelectorNode>>#emitCode: stack args: nArgs encoder: encoder super: supered

Generate a special selector send. A super send of a special selector must be handled like a normal send.


<details>
	<summary>See more</summary>
	
	emitCode: stack args: nArgs encoder: encoder super: supered
	"Generate a special selector send.
	 A super send of a special selector must be handled like a normal send."
	supered
		ifTrue:
			[super emitCode: stack args: nArgs encoder: encoder super: supered]
		ifFalse:
			[stack pop: nArgs.
			 encoder genSendSpecial: code numArgs: nArgs]
</details>

## TempVariableNode

I am a parse tree leaf representing a temporary variable

### Methods
#### TempVariableNode>>#name: varName index: i type: type scope: level

Only used for initting temporary variables


<details>
	<summary>See more</summary>
	
	name: varName index: i type: type scope: level
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type
</details>

#### TempVariableNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForValueOf: self stack: stack encoder: encoder].
	encoder genPushTemp: index.
	stack push: 1
</details>

#### TempVariableNode>>#emitCodeForStore: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStore: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStoreInto: self stack: stack encoder: encoder].
	encoder genStoreTemp: index
</details>

#### TempVariableNode>>#beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	self addWriteWithin: scopeBlock at: rootNode locationCounter.
	"For analysis of optimized blocks also record the set of temporaries written to
	 within optimized blocks so that additional writes can be added at locations that
	 represent subsequent iterations of the loop. e.g. testInlineBlockCollectionSD1"
	assignmentPools keysAndValuesDo:
		[:outerScopeBlock :set|
		"definingScope can be nil in expr in expr ifNil: [:arg|...] expressions because
		 arg gets its definingScope set when [:arg|...] is analysed."
		outerScopeBlock actualScope
			= (definingScope
				ifNil: [scopeBlock]
				ifNotNil: [definingScope actualScope]) ifTrue:
			[set add: self]]
</details>

#### TempVariableNode>>#addWriteWithin: scopeBlock "<BlockNode>" at: location

<BlockNode>


<details>
	<summary>See more</summary>
	
	addWriteWithin: scopeBlock "<BlockNode>" at: location "<Integer>"
	writingScopes ifNil: [writingScopes := Dictionary new].
	(writingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
</details>

#### TempVariableNode>>#isTemp

<details>
	<summary>See more</summary>
	
	isTemp
	^ true
</details>

#### TempVariableNode>>#remoteNode

<details>
	<summary>See more</summary>
	
	remoteNode
	^remoteNode
</details>

#### TempVariableNode>>#isMethodArg

<details>
	<summary>See more</summary>
	
	isMethodArg
	^#method == argType
</details>

#### TempVariableNode>>#referenceScopesAndIndicesDo: aBinaryBlock

Evaluate aBinaryBlock with all read or write scopes and locations. This is used to copy the reference information into RemoteTempVectorNodes


<details>
	<summary>See more</summary>
	
	referenceScopesAndIndicesDo: aBinaryBlock
	"Evaluate aBinaryBlock with all read or write scopes and locations.
	 This is used to copy the reference information into RemoteTempVectorNodes"
	readingScopes ~~ nil ifTrue:
		[readingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes keysAndValuesDo:
			[:scopeBlock "<BlockNode>" :set "<Set of <Integer>>"|
			set do: [:location| aBinaryBlock value: scopeBlock value: location]]]
</details>

#### TempVariableNode>>#isUndefTemp

<details>
	<summary>See more</summary>
	
	isUndefTemp
	^ hasDefs not
</details>

#### TempVariableNode>>#isIndirectTempVector

<details>
	<summary>See more</summary>
	
	isIndirectTempVector
	^false
</details>

#### TempVariableNode>>#sizeCodeForLoad: encoder forValue: forValue

Default is to do nothing. Subclasses may need to override.


<details>
	<summary>See more</summary>
	
	sizeCodeForLoad: encoder forValue: forValue
	^remoteNode
		ifNil: [0]
		ifNotNil: [remoteNode sizeCodeForLoadFor: self encoder: encoder]
</details>

#### TempVariableNode>>#scope

Answer scope of temporary variables. Currently only the following distinctions are made: 0 outer level: args and user-declared temps 1 block args and doLimiT temps -1 a block temp that is no longer active -2 a block temp that held limit of to:do:


<details>
	<summary>See more</summary>
	
	scope
	"Answer scope of temporary variables.
	 Currently only the following distinctions are made:
		 0	outer level: args and user-declared temps
		 1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	^scope
</details>

#### TempVariableNode>>#emitCodeForStorePop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStorePopInto: self stack: stack encoder: encoder].
	encoder genStorePopTemp: index.
	stack pop: 1
</details>

#### TempVariableNode>>#analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	self addReadWithin: scopeBlock at: rootNode locationCounter
</details>

#### TempVariableNode>>#emitCodeForLoad: stack forValue: forValue encoder: encoder

Do nothing


<details>
	<summary>See more</summary>
	
	emitCodeForLoad: stack forValue: forValue encoder: encoder
	remoteNode ~~ nil ifTrue:
		[remoteNode emitCodeForLoadFor: self stack: stack encoder: encoder]
</details>

#### TempVariableNode>>#index: anInteger

For renumbering temps in the closure compiler.


<details>
	<summary>See more</summary>
	
	index: anInteger
	"For renumbering temps in the closure compiler."
	index := anInteger.
	code := self code: index type: LdTempType
</details>

#### TempVariableNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level
	aStream nextPutAll: name.
	readingScopes ifNotNil: [
		(readingScopes
			inject: Set new
			into: [ :them :reads |
				them addAll: reads.
				them ]) asArray sort do: [ :location |
			aStream
				 space;
				 nextPut: $r;
				 nextPut: $@;
				 print: location ]].
	writingScopes ifNotNil: [
		(writingScopes
			inject: Set new
			into: [ :them :writes |
				them addAll: writes.
				them ]) asArray sort do: [ :location |
			aStream
				 space;
				 nextPut: $w;
				 nextPut: $@;
				 print: location ]].
</details>

#### TempVariableNode>>#definingScope: scopeBlock

<BlockNode>


<details>
	<summary>See more</summary>
	
	definingScope: scopeBlock "<BlockNode>"
	definingScope = scopeBlock ifTrue: [^ self]. "No need to bail"
	definingScope ifNotNil:
		[self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock
</details>

#### TempVariableNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForValueOf: self encoder: encoder].
	self reserve: encoder.
	^encoder sizePushTemp: index
</details>

#### TempVariableNode>>#scope: level

Note scope of temporary variables. Currently only the following distinctions are made: 0 outer level: args and user-declared temps 1 block args and doLimiT temps -1 a block temp that is no longer active -2 a block temp that held limit of to:do:


<details>
	<summary>See more</summary>
	
	scope: level
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope := level
</details>

#### TempVariableNode>>#isRemote

<details>
	<summary>See more</summary>
	
	isRemote
	^remoteNode notNil
</details>

#### TempVariableNode>>#isBlockArg

<details>
	<summary>See more</summary>
	
	isBlockArg
	^#block == argType
</details>

#### TempVariableNode>>#definingScope

<details>
	<summary>See more</summary>
	
	definingScope
	^definingScope
</details>

#### TempVariableNode>>#isDeclaredAtMethodLevel

For the explainer.


<details>
	<summary>See more</summary>
	
	isDeclaredAtMethodLevel
	"For the explainer."
	^scope = 0
</details>

#### TempVariableNode>>#isDefinedWithinBlockExtent: anInterval

<details>
	<summary>See more</summary>
	
	isDefinedWithinBlockExtent: anInterval
	^anInterval rangeIncludes: definingScope actualScope blockExtent first
</details>

#### TempVariableNode>>#isArg

<details>
	<summary>See more</summary>
	
	isArg
	^argType notNil
</details>

#### TempVariableNode>>#nowHasRef

Ignored in all but VariableNode


<details>
	<summary>See more</summary>
	
	nowHasRef
	hasRefs := true
</details>

#### TempVariableNode>>#sizeCodeForStorePop: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStorePopInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStorePopTemp: index
</details>

#### TempVariableNode>>#isUnusedTemp

<details>
	<summary>See more</summary>
	
	isUnusedTemp
	^ hasRefs not
</details>

#### TempVariableNode>>#printDefinitionForClosureAnalysisOn: aStream

<details>
	<summary>See more</summary>
	
	printDefinitionForClosureAnalysisOn: aStream 
	| refs |
	aStream
		nextPut: ${;
		nextPutAll: key.
	definingScope ifNotNil: [definingScope blockExtent ifNotNil: [:be| aStream nextPutAll: ' d@'; print: be first]].
	readingScopes ifNotNil: [
		refs := Set new.
		readingScopes do: [:elems| refs addAll: elems].
		refs asArray sort do: [:read| aStream nextPutAll: ' r@'; print: read]].
	writingScopes ifNotNil: [
		refs := Set new.
		writingScopes do: [:elems| refs addAll: elems].
		refs asArray sort do: [:write| aStream nextPutAll: ' w@'; print: write]].
	aStream nextPut: $}
</details>

#### TempVariableNode>>#addReadWithin: scopeBlock "<BlockNode>" at: location

<BlockNode>


<details>
	<summary>See more</summary>
	
	addReadWithin: scopeBlock "<BlockNode>" at: location "<Integer>"
	readingScopes ifNil: [readingScopes := Dictionary new].
	(readingScopes at: scopeBlock ifAbsentPut: [Set new]) add: location.
	remoteNode ifNotNil:
		[remoteNode addReadWithin: scopeBlock at: location]
</details>

#### TempVariableNode>>#isReferencedWithinBlockExtent: anInterval

<details>
	<summary>See more</summary>
	
	isReferencedWithinBlockExtent: anInterval 
	readingScopes ~~ nil ifTrue:
		[readingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	^false
</details>

#### TempVariableNode>>#nowHasDef

Ignored in all but VariableNode


<details>
	<summary>See more</summary>
	
	nowHasDef
	hasDefs := true
</details>

#### TempVariableNode>>#remoteNode: aRemoteTempVectorNode

<details>
	<summary>See more</summary>
	
	remoteNode: aRemoteTempVectorNode
	remoteNode := aRemoteTempVectorNode
</details>

#### TempVariableNode>>#beBlockArg

<details>
	<summary>See more</summary>
	
	beBlockArg
	argType := #block
</details>

#### TempVariableNode>>#beMethodArg

<details>
	<summary>See more</summary>
	
	beMethodArg
	argType := #method
</details>

#### TempVariableNode>>#isTempOrArg

<details>
	<summary>See more</summary>
	
	isTempOrArg

	^self isTemp or: [ self isArg ]
</details>

#### TempVariableNode>>#assignmentCheck: encoder at: location

For messageNodes masquerading as variables for the debugger. For now we let this through - ie we allow stores ev into args. Should check against numArgs, though.


<details>
	<summary>See more</summary>
	
	assignmentCheck: encoder at: location
	^((self isBlockArg and: [Preferences allowBlockArgumentAssignment not])
	    or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]
</details>

#### TempVariableNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitTempVariableNode: self
</details>

#### TempVariableNode>>#sizeCodeForStore: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStore: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStoreInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStoreTemp: index
</details>

#### TempVariableNode>>#analyseClosure: rootNode

<MethodNode>


<details>
	<summary>See more</summary>
	
	analyseClosure: rootNode "<MethodNode>"
	"Analyse whether the temporary needs to be made remote
	 or not, and answer whether it was made remote.
	 A temp cannot be local if it is written to remotely,
	 or if it is written to after it is closed-over.  An exception
	 is an inlined block argument that appears to be written
	 remotely but is actually local to a block."
	| latestWrite |
	self isBlockArg ifTrue: [^false].
	remoteNode ifNotNil: [^false]. "If already remote, don't remote a second time"
	latestWrite := 0.
	((writingScopes notNil
	 and: [writingScopes associations anySatisfy: [:assoc|
			[:blockScope :refs|
			refs do: [:write| latestWrite := write max: latestWrite].
			"A temp cannot be local if it is written to remotely."
			blockScope actualScope ~~ definingScope actualScope]
				value: assoc key value: assoc value]])
	or: [readingScopes notNil
		and: [readingScopes associations anySatisfy: [:assoc|
				[:blockScope :refs|
				 "A temp cannot be local if it is written to after it is closed-over."
				 blockScope actualScope ~~ definingScope actualScope
				 and: [refs anySatisfy: [:read| read < latestWrite]]]
					value: assoc key value: assoc value]]]) ifTrue:
		[remoteNode := definingScope addRemoteTemp: self rootNode: rootNode.
		 ^true].
	^false
</details>

## TemporariesDeclarationNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TemporariesDeclarationNode>>#printOn: aStream indent: anInteger

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: anInteger

	aStream nextPut: $|; space.
	self printEachTempVarDeclarationOn: aStream.
	aStream nextPut: $|.
</details>

#### TemporariesDeclarationNode>>#allDeclaredVariableNodes

<details>
	<summary>See more</summary>
	
	allDeclaredVariableNodes

	^ tempDeclarationNodes collect: [ :tempDeclaration | tempDeclaration variableNode ]
</details>

#### TemporariesDeclarationNode>>#declaresAnyVariable

<details>
	<summary>See more</summary>
	
	declaresAnyVariable

	^ tempDeclarationNodes notEmpty
</details>

#### TemporariesDeclarationNode>>#isTemporariesDeclaration

<details>
	<summary>See more</summary>
	
	isTemporariesDeclaration

	^ true
</details>

#### TemporariesDeclarationNode>>#printEachTempVarDeclarationOn: aStream

<details>
	<summary>See more</summary>
	
	printEachTempVarDeclarationOn: aStream

	^ tempDeclarationNodes do: [ :tempDeclarationNode |
		aStream nextPutAll: tempDeclarationNode variableName; space]
</details>

#### TemporariesDeclarationNode>>#declaresVariable: aVariableNode

<details>
	<summary>See more</summary>
	
	declaresVariable: aVariableNode

	^ self allDeclaredVariableNodes
		anySatisfy: [ :variableNode | variableNode isNamed: aVariableNode name ]
</details>

#### TemporariesDeclarationNode>>#temporaryDeclarationNodesDo: aBlock

<details>
	<summary>See more</summary>
	
	temporaryDeclarationNodesDo: aBlock

	tempDeclarationNodes do: aBlock
</details>

#### TemporariesDeclarationNode>>#initializeWithAll: aCollectionOfTempDeclarationNodes declarationWritten: aBoolean

<details>
	<summary>See more</summary>
	
	initializeWithAll: aCollectionOfTempDeclarationNodes declarationWritten: aBoolean

	tempDeclarationNodes := aCollectionOfTempDeclarationNodes.
	declarationWritten := aBoolean
</details>

#### TemporariesDeclarationNode>>#initializeWithAll: aCollectionOfTempDeclarationNodes

<details>
	<summary>See more</summary>
	
	initializeWithAll: aCollectionOfTempDeclarationNodes

	tempDeclarationNodes _ aCollectionOfTempDeclarationNodes
</details>

#### TemporariesDeclarationNode>>#declaresSameVariablesThan: aTemporariesDeclarationNode

<details>
	<summary>See more</summary>
	
	declaresSameVariablesThan: aTemporariesDeclarationNode

	self temporaryDeclarationNodes with: aTemporariesDeclarationNode temporaryDeclarationNodes do: [ :myTempDeclaration :otherTempDeclaration |
		(myTempDeclaration equivalentTo: otherTempDeclaration) ifFalse: [ ^ false ] ].
	^ true
</details>

#### TemporariesDeclarationNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor

	^ aVisitor visitTemporariesDeclarationNode: self
</details>

#### TemporariesDeclarationNode>>#temporaryDeclarationNodes

<details>
	<summary>See more</summary>
	
	temporaryDeclarationNodes

	^ tempDeclarationNodes
</details>

#### TemporariesDeclarationNode>>#declarationWritten

<details>
	<summary>See more</summary>
	
	declarationWritten

	^ declarationWritten
</details>

#### TemporariesDeclarationNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isTemporariesDeclaration
		and: [ self declaresSameVariablesThan: aParseNode  ]
</details>

## TemporaryDeclarationNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TemporaryDeclarationNode>>#printOn: aStream indent: anInteger

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: anInteger

	aStream nextPut: $|; space.
	variableNode printOn: aStream indent: anInteger.
	aStream space; nextPut: $|.
</details>

#### TemporaryDeclarationNode>>#initializeVariableNode: aVariableNode

<details>
	<summary>See more</summary>
	
	initializeVariableNode: aVariableNode

	variableNode _ aVariableNode
</details>

#### TemporaryDeclarationNode>>#declaresVariable: aVariableNode

<details>
	<summary>See more</summary>
	
	declaresVariable: aVariableNode

	^ variableNode isNamed: aVariableNode name
</details>

#### TemporaryDeclarationNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor

	^ aVisitor visitTemporaryDeclarationNode: self
</details>

#### TemporaryDeclarationNode>>#variableName

<details>
	<summary>See more</summary>
	
	variableName

	^ self variableNode name
</details>

#### TemporaryDeclarationNode>>#isTemporaryDeclaration

<details>
	<summary>See more</summary>
	
	isTemporaryDeclaration

	^ true
</details>

#### TemporaryDeclarationNode>>#variableNode

<details>
	<summary>See more</summary>
	
	variableNode

	^ variableNode
</details>

#### TemporaryDeclarationNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isTemporaryDeclaration
		and: [ self declaresVariable: aParseNode variableNode ]
</details>

## UndeclaredVariableNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UndeclaredVariableNode>>#tag: anObject

Tag can be whatever one wants it to be; used by Parser to tag undeclared temps with the user's desired declaration level.


<details>
	<summary>See more</summary>
	
	tag: anObject
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	tag := anObject
</details>

#### UndeclaredVariableNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitUndeclaredVariableNode: self
</details>

#### UndeclaredVariableNode>>#tag

Tag can be whatever one wants it to be; used by Parser to tag undeclared temps with the user's desired declaration level.


<details>
	<summary>See more</summary>
	
	tag
	"Tag can be whatever one wants it to be; used by Parser to tag
	  undeclared temps with the user's desired declaration level."

	^tag
</details>

#### UndeclaredVariableNode>>#isUndeclared

<details>
	<summary>See more</summary>
	
	isUndeclared
	^true
</details>

## VariableNode

I am a parse tree leaf representing a variable. Note that my name and key are different for pool variables: the key is the Object Reference.

### Methods
#### VariableNode>>#name: varName key: objRef index: i type: type

Only used for initting global (litInd) variables


<details>
	<summary>See more</summary>
	
	name: varName key: objRef index: i type: type
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)
</details>

#### VariableNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level 

	aStream nextPutAll: name
</details>

#### VariableNode>>#emitCodeForValue: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForValue: stack encoder: encoder
	stack push: 1.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder genPushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder genPushReceiver].
	code = LdThisContext ifTrue:
		[^encoder genPushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder genPushInstVar: index
</details>

#### VariableNode>>#emitCodeForStore: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStore: stack encoder: encoder

	self shouldNotImplement
</details>

#### VariableNode>>#beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools

<BlockNode>


<details>
	<summary>See more</summary>
	
	beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"No-op overridden by TempVariableNode"
</details>

#### VariableNode>>#name: string key: object code: byte

Only used for initting std variables, nil, true, false, self, etc.


<details>
	<summary>See more</summary>
	
	name: string key: object code: byte
	"Only used for initting std variables, nil, true, false, self, etc."
	name := string.
	key := object.
	code := byte
</details>

#### VariableNode>>#isSelfPseudoVariable

Answer if this ParseNode represents the 'self' pseudo-variable.


<details>
	<summary>See more</summary>
	
	isSelfPseudoVariable
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [name = '{{self}}']
</details>

#### VariableNode>>#emitCodeForReturn: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForReturn: stack encoder: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			"short returns"
			 encoder genReturnSpecialLiteral: specialLiteral.
			 stack push: 1 "doesnt seem right".
			 ^self].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue: 
		["short returns"
		 encoder genReturnReceiver.
		 stack push: 1 "doesnt seem right".
		 ^self].
	super emitCodeForReturn: stack encoder: encoder
</details>

#### VariableNode>>#emitCodeForStorePop: stack encoder: encoder

<details>
	<summary>See more</summary>
	
	emitCodeForStorePop: stack encoder: encoder
	self varNodeType ~= 1 ifTrue:
		[self halt].
	encoder genStorePopInstVar: index.
	stack pop: 1
</details>

#### VariableNode>>#name: string

Change name


<details>
	<summary>See more</summary>
	
	name: string
	"Change name"

	name := string
</details>

#### VariableNode>>#variableGetterBlockIn: aContext

<details>
	<summary>See more</summary>
	
	variableGetterBlockIn: aContext

	| temps tempIndex ivars |
	(self varNodeType = 4 and: [self key isVariableBinding]) ifTrue: [
		^[self key value]
	].
	aContext ifNil: [^nil].
	self isSelfPseudoVariable ifTrue: [^[aContext receiver]].
	self varNodeType = 1 ifTrue: [
		ivars := aContext receiver class allInstVarNames.
		tempIndex := ivars indexOf: self name ifAbsent: [^nil].
		^[aContext receiver instVarAt: tempIndex]
	].
	self varNodeType = 2 ifTrue: [
		temps := aContext tempNames.
		tempIndex := temps indexOf: self name ifAbsent: [^nil].
		^[aContext tempAt: tempIndex]
	].
	^nil

</details>

#### VariableNode>>#emitCodeForLoad: stack forValue: forValue encoder: encoder

Do nothing


<details>
	<summary>See more</summary>
	
	emitCodeForLoad: stack forValue: forValue encoder: encoder
	"Do nothing"
</details>

#### VariableNode>>#printWithClosureAnalysisOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream indent: level 

	aStream nextPutAll: name
</details>

#### VariableNode>>#sizeCodeForValue: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForValue: encoder
	self reserve: encoder.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral| "i.e. the pseudo-variables nil true & false"
			 ^encoder sizePushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder sizePushReceiver].
	code = LdThisContext ifTrue:
		[^encoder sizePushThisContext].
	self flag: 'probably superfluous'.
	self halt.
	^encoder sizePushInstVar: index
</details>

#### VariableNode>>#index

If index is nil, this code attempts to reconstruct the index from its encoding in code.


<details>
	<summary>See more</summary>
	
	index
	"If index is nil, this code attempts to reconstruct the index from its encoding in code."
	index ifNotNil:
		[^index].
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[self assert: index = (code \\ 256).
		^code \\ 256].
	code >= (CodeBases at: self varNodeType) ifTrue:
		[self assert: index = (code - (CodeBases at: self varNodeType)).
		^code - (CodeBases at: self varNodeType)].
	self assert: index = (code - self varNodeType).
	^code - self varNodeType
</details>

#### VariableNode>>#nameAndKey: aName

<details>
	<summary>See more</summary>
	
	nameAndKey: aName

	name := key := aName 
</details>

#### VariableNode>>#isNilPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isNilPseudoVariable
	
	^ key = 'nil' or: [name = '{{nil}}']
</details>

#### VariableNode>>#name: varName index: i type: type

Only used for initting instVar refs


<details>
	<summary>See more</summary>
	
	name: varName index: i type: type
	"Only used for initting instVar refs"
	^self name: varName key: varName index: i type: type
</details>

#### VariableNode>>#equivalentTo: aParseNode

<details>
	<summary>See more</summary>
	
	equivalentTo: aParseNode

	^ aParseNode isVariableNode and: [ super equivalentTo: aParseNode ]
</details>

#### VariableNode>>#currentValueIn: aContext

<details>
	<summary>See more</summary>
	
	currentValueIn: aContext

	aContext ifNil: [^nil].
	^((self variableGetterBlockIn: aContext) ifNil: [^nil]) value printString
	


</details>

#### VariableNode>>#isThisContextPseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isThisContextPseudoVariable
	
	^key = 'thisContext' or: [name = '{{thisContext}}']
</details>

#### VariableNode>>#sizeCodeForStorePop: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStorePop: encoder
	self shouldNotImplement
</details>

#### VariableNode>>#isFalsePseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isFalsePseudoVariable
	
	^key = 'false' or: [name = '{{false}}']
</details>

#### VariableNode>>#isNamed: aName

<details>
	<summary>See more</summary>
	
	isNamed: aName

	^ self name = aName
</details>

#### VariableNode>>#isVariableReference

<details>
	<summary>See more</summary>
	
	isVariableReference

	^true
</details>

#### VariableNode>>#fieldOffset

Return temp or instVar offset for this variable


<details>
	<summary>See more</summary>
	
	fieldOffset
	"Return temp or instVar offset for this variable"
	^index ifNil: [code < 256
					ifTrue: [code \\ 16]
					ifFalse: [code \\ 256]]
</details>

#### VariableNode>>#varNodeType

This code attempts to reconstruct the type from its encoding in code. This allows one to test, for instance, (aNode type = LdInstType).


<details>
	<summary>See more</summary>
	
	varNodeType
	"This code attempts to reconstruct the type from its encoding in code.
		This allows one to test, for instance, (aNode type = LdInstType)."
	| type |
	code < 0 ifTrue: [^code negated].
	code >= 256 ifTrue: [^code // 256].
	type := CodeBases findFirst: [:one | code < one].
	^type = 0 ifTrue: [5] ifFalse: [type - 1]
</details>

#### VariableNode>>#isVariableNode

<details>
	<summary>See more</summary>
	
	isVariableNode
	^true
</details>

#### VariableNode>>#isTruePseudoVariable

Overridden in VariableNode.


<details>
	<summary>See more</summary>
	
	isTruePseudoVariable
	
	^ key = 'true' or: [name = '{{true}}']
</details>

#### VariableNode>>#isSuperPseudoVariable

Answer if this ParseNode represents the 'super' pseudo-variable.


<details>
	<summary>See more</summary>
	
	isSuperPseudoVariable
	"Answer if this ParseNode represents the 'super' pseudo-variable."

	^ key = 'super' or: [name = '{{super}}']
</details>

#### VariableNode>>#assignmentCheck: encoder at: location

For messageNodes masquerading as variables for the debugger. For now we let this through - ie we allow stores ev into args. Should check against numArgs, though.


<details>
	<summary>See more</summary>
	
	assignmentCheck: encoder at: location
	^(encoder cantStoreInto: name) ifTrue: [location] ifFalse: [-1]
</details>

#### VariableNode>>#asStorableNode: encoder

<details>
	<summary>See more</summary>
	
	asStorableNode: encoder
	^ self
</details>

#### VariableNode>>#accept: aVisitor

Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:. All such implementations under ParseNode should answer the result of the dispatch, e.g. ^aVisitor visitBlockNode: self


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	^aVisitor visitVariableNode: self
</details>

#### VariableNode>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^ name
</details>

#### VariableNode>>#sizeCodeForStore: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForStore: encoder
	self shouldNotImplement
</details>

#### VariableNode>>#sizeCodeForReturn: encoder

<details>
	<summary>See more</summary>
	
	sizeCodeForReturn: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			 ^encoder sizeReturnSpecialLiteral: specialLiteral].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue:
		[^encoder sizeReturnReceiver].
	^super sizeCodeForReturn: encoder
</details>

#### VariableNode>>#isUndeclared

<details>
	<summary>See more</summary>
	
	isUndeclared
	^false
</details>

