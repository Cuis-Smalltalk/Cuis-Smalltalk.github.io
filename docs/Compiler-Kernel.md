## BytecodeEncoder

I am an abstract superclass for different bytecode set encoders. Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.

### Methods
#### BytecodeEncoder>>#sizeStorePopLiteralVar: literalIndex

<details>
	<summary>See more</summary>
	
	sizeStorePopLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genStorePopLiteralVar: withArguments: {literalIndex}
</details>

#### BytecodeEncoder>>#blockExtentsToTempsMap

Answer a Dictionary of blockExtent to temp locations for the current method. This is used by the debugger to locate temp vars in contexts. A temp map entry is a pair of the temp's name and its index, where an index is either an integer for a normal temp or a pair of the index of the indirect temp vector containing the temp and the index of the temp in its indirect temp vector.


<details>
	<summary>See more</summary>
	
	blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."
	| blockExtentsToTempsMap |
	blockExtentsToLocals ifNil:
		[^nil].
	blockExtentsToTempsMap := Dictionary new.
	blockExtentsToLocals keysAndValuesDo:
		[:blockExtent :locals|
		blockExtentsToTempsMap
			at: blockExtent
			put: (Array streamContents:
					[:strm|		"Do not call it stream, it would be shadowed"
					locals withIndexDo:
						[:local :index|
						local isIndirectTempVector
							ifTrue: [local remoteTemps withIndexDo:
										[:remoteLocal :innerIndex| strm nextPut: { remoteLocal key. { index. innerIndex } }]]
							ifFalse: [strm nextPut: { local key. index }]]])].
	^blockExtentsToTempsMap
</details>

#### BytecodeEncoder>>#bindTemp: name

Declare a temporary; error not if a field or class variable or out-of-scope temp. Read the comment in Encoder>>bindBlockArg:within: and subclass implementations.


<details>
	<summary>See more</summary>
	
	bindTemp: name
	"Declare a temporary; error not if a field or class variable or out-of-scope temp.
	 Read the comment in Encoder>>bindBlockArg:within: and subclass implementations."
	self supportsClosureOpcodes ifFalse:
		[^super bindTemp: name].
	scopeTable at: name ifPresent:
		[:node|
		"When non-interactive raise the error only if it is a duplicate"
		node isTemp
			ifTrue:[node scope >= 0 ifTrue:
						[^self notify: 'Name already used in this method']]
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name
</details>

#### BytecodeEncoder>>#sizeStoreLiteralVar: literalIndex

<details>
	<summary>See more</summary>
	
	sizeStoreLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}
</details>

#### BytecodeEncoder>>#sizePushNClosureTemps: numTemps

<details>
	<summary>See more</summary>
	
	sizePushNClosureTemps: numTemps
	^self sizeOpcodeSelector: #genPushNClosureTemps: withArguments: {numTemps}
</details>

#### BytecodeEncoder>>#rootNode

^<BlockNode>


<details>
	<summary>See more</summary>
	
	rootNode "^<BlockNode>"
	^rootNode
</details>

#### BytecodeEncoder>>#sizePushLiteralVar: literalIndex

<details>
	<summary>See more</summary>
	
	sizePushLiteralVar: literalIndex
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}
</details>

#### BytecodeEncoder>>#outOfRangeError: string index: index range: rangeStart to: rangeEnd

For now...


<details>
	<summary>See more</summary>
	
	outOfRangeError: string index: index range: rangeStart to: rangeEnd
	"For now..."
	^self error: thisContext sender method selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString
</details>

#### BytecodeEncoder>>#sizeBranchPopTrue: distance

<details>
	<summary>See more</summary>
	
	sizeBranchPopTrue: distance
	^self sizeOpcodeSelector: #genBranchPopTrue: withArguments: {distance}
</details>

#### BytecodeEncoder>>#sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize

<details>
	<summary>See more</summary>
	
	sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	^self
		sizeOpcodeSelector: #genPushClosureCopyNumCopiedValues:numArgs:jumpSize:
		withArguments: {numCopied. numArgs. jumpSize}
</details>

#### BytecodeEncoder>>#sizePushLiteral: literalIndex

<details>
	<summary>See more</summary>
	
	sizePushLiteral: literalIndex
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}
</details>

#### BytecodeEncoder>>#sizeSend: selectorLiteralIndex numArgs: nArgs

<details>
	<summary>See more</summary>
	
	sizeSend: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}
</details>

#### BytecodeEncoder>>#sizeJumpLong: distance

<details>
	<summary>See more</summary>
	
	sizeJumpLong: distance
	^self sizeOpcodeSelector: #genJumpLong: withArguments: {distance}
</details>

#### BytecodeEncoder>>#sizeCallPrimitive: primitiveIndex

Only for Spur!


<details>
	<summary>See more</summary>
	
	sizeCallPrimitive: primitiveIndex
	"Only for Spur!"
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}
</details>

#### BytecodeEncoder>>#sizeJump: distance

<details>
	<summary>See more</summary>
	
	sizeJump: distance
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}
</details>

#### BytecodeEncoder>>#rootNode: node

<BlockNode>


<details>
	<summary>See more</summary>
	
	rootNode: node "<BlockNode>"
	rootNode := node
</details>

#### BytecodeEncoder>>#bindBlockArg: name within: aBlockNode

Read the comment in the superclass's method. If we have closures we should check the argument count against the block, not the method. (Note that this isn't entirely adequate either since optimized blocks will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|) are charged against their enclosing block, not themselves)).


<details>
	<summary>See more</summary>
	
	bindBlockArg: name within: aBlockNode
	"Read the comment in the superclass's method.
	 If we have closures we should check the argument
	 count against the block, not the method.

	(Note that this isn't entirely adequate either since optimized blocks
	 will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|)
	 are charged against their enclosing block, not themselves))."
	| nArgs |
	self supportsClosureOpcodes ifFalse:
		[^super bindBlockArg: name within: aBlockNode].
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs  >= 15 ifTrue:
		[^self notify: 'Too many arguments'].
	aBlockNode nArgsSlot: nArgs + 1.
	^(self bindTemp: name)
		beBlockArg;
		nowHasDef;
		nowHasRef;
		yourself
</details>

#### BytecodeEncoder>>#sizeReturnTopToCaller

<details>
	<summary>See more</summary>
	
	sizeReturnTopToCaller
	^self sizeOpcodeSelector: #genReturnTopToCaller withArguments: #()
</details>

#### BytecodeEncoder>>#sizePushNewArray: size

<details>
	<summary>See more</summary>
	
	sizePushNewArray: size
	^self sizeOpcodeSelector: #genPushNewArray: withArguments: {size}
</details>

#### BytecodeEncoder>>#hasGeneratedMethod

<details>
	<summary>See more</summary>
	
	hasGeneratedMethod
	^blockExtentsToLocals notNil
</details>

#### BytecodeEncoder>>#sizeDup

<details>
	<summary>See more</summary>
	
	sizeDup
	^self sizeOpcodeSelector: #genDup withArguments: #()
</details>

#### BytecodeEncoder>>#streamToMethod: aCompiledMethod

<details>
	<summary>See more</summary>
	
	streamToMethod: aCompiledMethod
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1
</details>

#### BytecodeEncoder>>#sizePushSpecialLiteral: specialLiteral

<details>
	<summary>See more</summary>
	
	sizePushSpecialLiteral: specialLiteral
	^self sizeOpcodeSelector: #genPushSpecialLiteral: withArguments: {specialLiteral}
</details>

#### BytecodeEncoder>>#sizePushInstVarLong: instVarIndex

<details>
	<summary>See more</summary>
	
	sizePushInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#supportsClosureOpcodes

Answer if the receiver supports the genPushNewArray:/genPushConsArray: genPushRemoteTemp:inVectorAt: genStoreRemoteTemp:inVectorAt: genStorePopRemoteTemp:inVectorAt: genPushClosureCopyCopiedValues:numArgs:jumpSize: opcodes


<details>
	<summary>See more</summary>
	
	supportsClosureOpcodes
	"Answer if the receiver supports the
		genPushNewArray:/genPushConsArray:
		genPushRemoteTemp:inVectorAt:
		genStoreRemoteTemp:inVectorAt:
		genStorePopRemoteTemp:inVectorAt:
		genPushClosureCopyCopiedValues:numArgs:jumpSize:
	 opcodes"
	^false
</details>

#### BytecodeEncoder>>#sizeReturnSpecialLiteral: specialLiteral

<details>
	<summary>See more</summary>
	
	sizeReturnSpecialLiteral: specialLiteral
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}
</details>

#### BytecodeEncoder>>#sizeStorePopInstVarLong: instVarIndex

<details>
	<summary>See more</summary>
	
	sizeStorePopInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#sizeOpcodeSelector: genSelector withArguments: args

<details>
	<summary>See more</summary>
	
	sizeOpcodeSelector: genSelector withArguments: args
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position
</details>

#### BytecodeEncoder>>#sizeStorePopInstVar: instVarIndex

<details>
	<summary>See more</summary>
	
	sizeStorePopInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#methodStreamPosition

<details>
	<summary>See more</summary>
	
	methodStreamPosition
	^stream position
</details>

#### BytecodeEncoder>>#if: code isSpecialLiteralForPush: aBlock

If code is that of a special literal for push then evaluate aBlock with the special literal The special literals for push are nil true false -1 0 1 & 2 which have special encodings in the blue book bytecode set. Answer whether it was a special literal.


<details>
	<summary>See more</summary>
	
	if: code isSpecialLiteralForPush: aBlock
	"If code is that of a special literal for push then evaluate aBlock with the special literal
	 The special literals for push are nil true false -1 0 1 & 2 which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil + 4)
	    and: [aBlock value: (#(true false nil -1 0 1 2) at: code - LdSelf).
			true]
</details>

#### BytecodeEncoder>>#sizeStoreTemp: tempIndex

<details>
	<summary>See more</summary>
	
	sizeStoreTemp: tempIndex
	^self sizeOpcodeSelector: #genStoreTemp: withArguments: {tempIndex}
</details>

#### BytecodeEncoder>>#sizeReturnTop

<details>
	<summary>See more</summary>
	
	sizeReturnTop
	^self sizeOpcodeSelector: #genReturnTop withArguments: #()
</details>

#### BytecodeEncoder>>#sizeSendSuper: selectorLiteralIndex numArgs: nArgs

<details>
	<summary>See more</summary>
	
	sizeSendSuper: selectorLiteralIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}
</details>

#### BytecodeEncoder>>#sizeStoreInstVar: instVarIndex

<details>
	<summary>See more</summary>
	
	sizeStoreInstVar: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#noteBlockExtent: blockExtent hasLocals: tempNodes

<details>
	<summary>See more</summary>
	
	noteBlockExtent: blockExtent hasLocals: tempNodes
	blockExtentsToLocals ifNil:
		[blockExtentsToLocals := Dictionary new].
	blockExtentsToLocals at: blockExtent put: tempNodes asArray
</details>

#### BytecodeEncoder>>#sizeReturnReceiver

<details>
	<summary>See more</summary>
	
	sizeReturnReceiver
	^self sizeOpcodeSelector: #genReturnReceiver withArguments: #()
</details>

#### BytecodeEncoder>>#sizePop

<details>
	<summary>See more</summary>
	
	sizePop
	^self sizeOpcodeSelector: #genPop withArguments: #()
</details>

#### BytecodeEncoder>>#bindAndJuggle: name

This is used to insert a new temp and reorcder temps on editing. It doesn't really work for closure compilation since we have multiple locations for temps. Simply signal a reparse is necessary.


<details>
	<summary>See more</summary>
	
	bindAndJuggle: name
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal
</details>

#### BytecodeEncoder>>#nextPut: aByte

For sizing make the encoder its own stream and keep track of position with this version of nextPut:


<details>
	<summary>See more</summary>
	
	nextPut: aByte
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1
</details>

#### BytecodeEncoder>>#sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
</details>

#### BytecodeEncoder>>#sizePushTemp: tempIndex

<details>
	<summary>See more</summary>
	
	sizePushTemp: tempIndex
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}
</details>

#### BytecodeEncoder>>#sizePushInstVar: instVarIndex

<details>
	<summary>See more</summary>
	
	sizePushInstVar: instVarIndex
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#bindBlockTemp: name

This shouldn't be used with BytecodeEncoder. Use bindBlockTemp:within: instead.


<details>
	<summary>See more</summary>
	
	bindBlockTemp: name
	"This shouldn't be used with BytecodeEncoder.  Use bindBlockTemp:within: instead."
	self shouldNotImplement
</details>

#### BytecodeEncoder>>#sizePushThisContext

<details>
	<summary>See more</summary>
	
	sizePushThisContext
	^self sizeOpcodeSelector: #genPushThisContext withArguments: #()
</details>

#### BytecodeEncoder>>#if: code isSpecialLiteralForReturn: aBlock

If code is that of a special literal for return then evaluate aBlock with the special literal. The special literals for return are nil true false which have special encodings in the blue book bytecode set. Answer whether it was a special literal.


<details>
	<summary>See more</summary>
	
	if: code isSpecialLiteralForReturn: aBlock
	"If code is that of a special literal for return then evaluate aBlock with the special literal.
	 The special literals for return are nil true false which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil)
	   and: [aBlock value: (#(true false nil) at: code - LdSelf).
			true]
</details>

#### BytecodeEncoder>>#sizeSendSpecial: specialSelectorIndex numArgs: nArgs

<details>
	<summary>See more</summary>
	
	sizeSendSpecial: specialSelectorIndex numArgs: nArgs
	^self sizeOpcodeSelector: #genSendSpecial:numArgs: withArguments: {specialSelectorIndex. nArgs}
</details>

#### BytecodeEncoder>>#sizePushReceiver

<details>
	<summary>See more</summary>
	
	sizePushReceiver
	^self sizeOpcodeSelector: #genPushReceiver withArguments: #()
</details>

#### BytecodeEncoder>>#sizePushConsArray: numElements

<details>
	<summary>See more</summary>
	
	sizePushConsArray: numElements
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}
</details>

#### BytecodeEncoder>>#sizeStoreInstVarLong: instVarIndex

<details>
	<summary>See more</summary>
	
	sizeStoreInstVarLong: instVarIndex
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}
</details>

#### BytecodeEncoder>>#sizePushTempLong: tempIndex

<details>
	<summary>See more</summary>
	
	sizePushTempLong: tempIndex
	^self sizeOpcodeSelector: #genPushTempLong: withArguments: {tempIndex}
</details>

#### BytecodeEncoder>>#bindBlockTemp: name within: aBlockNode

Read the comment in the superclass's bindBlockArg:within: method. If we have closures we should check the argument count against the block, not the method. (Note that this isn't entirely adequate either since optimized blocks will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|) are charged against their enclosing block, not themselves)).


<details>
	<summary>See more</summary>
	
	bindBlockTemp: name within: aBlockNode
	"Read the comment in the superclass's bindBlockArg:within: method.
	 If we have closures we should check the argument
	 count against the block, not the method.

	(Note that this isn't entirely adequate either since optimized blocks
	 will slip through the cracks (their arguments (i.e. ifNotNil: [:expr|)
	 are charged against their enclosing block, not themselves))."
	| nArgs |
	self supportsClosureOpcodes ifFalse:
		[^super bindBlockTemp: name within: aBlockNode].
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= (CompiledMethod fullFrameSize - 1) ifTrue:
		[^self notify: 'Too many temporaries'].
	aBlockNode nArgsSlot: nArgs + 1.
	^self bindTemp: name
</details>

#### BytecodeEncoder>>#sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genPushRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
</details>

#### BytecodeEncoder>>#sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
</details>

#### BytecodeEncoder>>#sizeBranchPopFalse: distance

<details>
	<summary>See more</summary>
	
	sizeBranchPopFalse: distance
	^self sizeOpcodeSelector: #genBranchPopFalse: withArguments: {distance}
</details>

#### BytecodeEncoder>>#sizeStorePopTemp: tempIndex

<details>
	<summary>See more</summary>
	
	sizeStorePopTemp: tempIndex
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}
</details>

#### BytecodeEncoder>>#computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex

Only for Spur!


<details>
	<summary>See more</summary>
	
	computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	"Only for Spur!"
	numArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	numTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	^(CompiledMethod headerFlagForEncoder: self)
	+ (numArgs bitShift: 24)
	+ (numTemps bitShift: 18)
	"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
	+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])
	+ numLits
</details>

## Compiler

The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode. See http://www.whysmalltalk.com/articles/bykov/HitchHiker.htm

### Methods
#### Compiler>>#notify: aString

Refer to the comment in Object|notify:.


<details>
	<summary>See more</summary>
	
	notify: aString 
	"Refer to the comment in Object|notify:."

	^self notify: aString at: sourceStream position + 1
</details>

#### Compiler>>#format: textOrStream in: aClass notifying: aRequestor

Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely. If aBoolean is true, then decorate the resulting text with color and hypertext actions


<details>
	<summary>See more</summary>
	
	format: textOrStream in: aClass notifying: aRequestor
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"

	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode := self format: sourceStream noPattern: false ifFail: [^ nil].
	^aNode decompileString
</details>

#### Compiler>>#compileDoIt: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock

Similar to #compile:in:notifying:ifFail:, but the compiled code is expected to be a do-it expression, with no message pattern.


<details>
	<summary>See more</summary>
	
	compileDoIt: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: false
		doIt: true
		ifFail: failBlock
</details>

#### Compiler>>#evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile

Compiles the sourceStream into a parse tree, then generates code into a method. If aContext is not nil, the text can refer to temporaries in that context (the Debugger uses this). If aRequestor is not nil, then it will receive a notify:at: message before the attempt to evaluate is aborted. Finally, the compiled method is invoked from here via withArgs:executeMethod:, hence the system no longer creates Doit method litter on errors.


<details>
	<summary>See more</summary>
	
	evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method |
	
	class _ (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	methodNode _ self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method _ methodNode generate.
	"I'm not keeping the source nor the methodNode for back compabibility. 
	The SmalltalkEditor sends the message #evaluateMethod:... which already keep the method node
	 for the debugger to show the right source code - Hernan"
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile
</details>

#### Compiler>>#parser: aParser

<details>
	<summary>See more</summary>
	
	parser: aParser

	parser := aParser
</details>

#### Compiler>>#format: aStream noPattern: noPattern ifFail: failBlock

<details>
	<summary>See more</summary>
	
	format: aStream noPattern: noPattern ifFail: failBlock
	^(self parser
		parse: aStream
		class: class
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]) preen
</details>

#### Compiler>>#translate: aStream noPattern: noPattern ifFail: failBlock

<details>
	<summary>See more</summary>
	
	translate: aStream noPattern: noPattern ifFail: failBlock

	^self translate: aStream noPattern: noPattern doIt: noPattern ifFail: failBlock 
</details>

#### Compiler>>#evaluateMethod: method to: receiver logged: doLog profiled: doProfile

See evaluate:in:to:notifying:ifFail:logged:profiled: It does the same but without compiling because it recevies the result of the compilation as the parameter method. self should have compile method


<details>
	<summary>See more</summary>
	
	evaluateMethod: method to: receiver logged: doLog profiled: doProfile
	
	"See evaluate:in:to:notifying:ifFail:logged:profiled:
	It does the same but without compiling because it recevies the result of the compilation 
	as the parameter method. 
	self should have compile method"

	| value toLog itsSelection itsSelectionString |
	
	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: context ].

	"Evaluate now."
	doProfile
		ifTrue: [
			AndreasSystemProfiler spyOn: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value
</details>

#### Compiler>>#parser

<details>
	<summary>See more</summary>
	
	parser

	parser ifNil: [parser := self parserClass new].
	^parser
</details>

#### Compiler>>#translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

<details>
	<summary>See more</summary>
	
	translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self parser
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]
</details>

#### Compiler>>#evaluate: aString in: aContext to: aReceiver

evaluate aString in the given context, and return the result. Anser nil in case of failure


<details>
	<summary>See more</summary>
	
	evaluate: aString in: aContext to: aReceiver
	"evaluate aString in the given context, and return the result.
	Anser nil in case of failure"
	^self
		evaluate: aString
		in: aContext
		to: aReceiver
		notifying: nil
		ifFail: nil
</details>

#### Compiler>>#interactive

this version of the method is necessary to load code from MC else the interactive mode is one. This method is really bad since it links the compiler package with the Tools one. The solution would be to have a real SyntaxError exception belonging to the compiler package and not a subclass of StringHolder - sd Nov 2005


<details>
	<summary>See more</summary>
	
	interactive
	"this version of the method is necessary to load code from MC else the interactive mode is one. 
	This method is really bad since it links the compiler package with the Tools
	one. The solution would be to have a real SyntaxError exception belonging to the 
	compiler package and not a subclass of StringHolder - sd Nov 2005"
	"the code submitted by PlusTools is ideally the one that should be used
	interactive

	      ^requestor ~~ nil "

	^ requestor notNil
</details>

#### Compiler>>#evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock

<details>
	<summary>See more</summary>
	
	evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock

	^ self evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: false profiled: false
</details>

#### Compiler>>#from: textOrStream class: aClass context: aContext notifying: req

<details>
	<summary>See more</summary>
	
	from: textOrStream class: aClass context: aContext notifying: req

	sourceStream _ (textOrStream is: #Stream)
		ifTrue: [ textOrStream ]
		ifFalse: [ ReadStream on: textOrStream asString ].
	class _ aClass.
	context _ aContext.
	requestor _ req
</details>

#### Compiler>>#parserClass

<details>
	<summary>See more</summary>
	
	parserClass

	^parser ifNil: [self class parserClass] ifNotNil: [parser class]
</details>

#### Compiler>>#compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock

Answer a MethodNode for the argument, textOrStream. If the MethodNode can not be created, notify the argument, aRequestor; if aRequestor is nil, evaluate failBlock instead. The MethodNode is the root of a parse tree. It can be told to generate a CompiledMethod to be installed in the method dictionary of the argument, aClass.


<details>
	<summary>See more</summary>
	
	compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock 
	"Answer a MethodNode for the argument, textOrStream. If the 
	MethodNode can not be created, notify the argument, aRequestor; if 
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root 
	of a parse tree. It can be told to generate a CompiledMethod to be 
	installed in the method dictionary of the argument, aClass."
	
	| methodNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	category _ aCategory.
	methodNode _ self translate: sourceStream noPattern: false ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^methodNode
</details>

#### Compiler>>#notify: aString at: location

Refer to the comment in Object|notify:.


<details>
	<summary>See more</summary>
	
	notify: aString at: location
	"Refer to the comment in Object|notify:."

	^requestor == nil
		ifTrue: [SyntaxErrorNotification
					inClass: class
					category: category
					withCode: 
						(sourceStream contents
							copyReplaceFrom: location
							to: location - 1
							with: aString)
					doitFlag: false
					errorMessage: aString
					location: location]
		ifFalse: [requestor
					notify: aString
					at: location
					in: sourceStream]
</details>

#### Compiler>>#compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock

<details>
	<summary>See more</summary>
	
	compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock 
	^self compile: textOrStream in: aClass classified: nil notifying: aRequestor ifFail: failBlock 
</details>

#### Compiler>>#compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock

Similar to #compile:in:notifying:ifFail:, but the compiled code is expected to be a do-it expression, with no message pattern.


<details>
	<summary>See more</summary>
	
	compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: true
		ifFail: failBlock
</details>

## Decompiler

I decompile a method in three phases: Reverser: postfix byte codes -> prefix symbolic codes (nodes and atoms) Parser: prefix symbolic codes -> node tree (same as the compiler) Printer: node tree -> text (done by the nodes) instance vars: constructor <DecompilerConstructor> an auxiliary knowing how to generate Abstract Syntax Tree (node tree) method <CompiledMethod> the method being decompiled instVars <Array of: String> the instance variables of the class implementing method tempVars <String | (OrderedCollection of: String)> hold the names of temporary variables (if known) NOTE: POLYMORPHISM WILL BE RESOLVED IN #initSymbols: constTable <Collection of: ParseNode> parse node associated with byte encoded constants (nil true false 0 1 -1 etc...) stack <OrderedCollection of: (ParseNode | String | Integer) > multipurpose... statements <OrderedCollection of: ParseNode> the statements of the method being decompiled lastPc <Integer> exit <Integer> caseExits <OrderedCollection of: Integer> - stack of exit addresses that have been seen in the branches of caseOf:'s lastJumpPc <Integer> lastReturnPc <Integer> limit <Integer> hasValue <Boolean> blockStackBase <Integer> numLocaltemps <Integer | Symbol> - number of temps local to a block; also a flag indicating decompiling a block blockStartsToTempVars <Dictionary key: Integer value: (OrderedCollection of: String)> tempVarCount <Integer> number of temp vars used by the method lastJumpIfPcStack <OrderedCollection of: Integer> the value of program counter just before the last encountered conditional jumps

### Methods
#### Decompiler>>#pushReceiverVariable: offset

<details>
	<summary>See more</summary>
	
	pushReceiverVariable: offset

	| var |
	(var := instVars at: offset + 1 ifAbsent: []) == nil
		ifTrue:
			["Not set up yet"
			var := constructor codeInst: offset.
			instVars size < (offset + 1) ifTrue: [
				instVars := (Array new: offset + 1)
					replaceFrom: 1 to: instVars size with: instVars; yourself ].
			instVars at: offset + 1 put: var].
	stack addLast: var
</details>

#### Decompiler>>#methodReturnConstant: value

<details>
	<summary>See more</summary>
	
	methodReturnConstant: value

	self pushConstant: value; methodReturnTop
</details>

#### Decompiler>>#doPop

<details>
	<summary>See more</summary>
	
	doPop

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == #CaseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].
</details>

#### Decompiler>>#mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor

<details>
	<summary>See more</summary>
	
	mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys sort
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector|
		tempNameTupleVector isEmpty ifFalse:
			[| subMap numTemps tempVector |
			subMap := Dictionary new.
			"Find how many temp slots there are (direct & indirect temp vectors)
			 and for each indirect temp vector find how big it is."
			tempNameTupleVector do:
				[:tuple|
				tuple last isArray
					ifTrue:
						[subMap at: tuple last first put: tuple last last.
						 numTemps := tuple last first]
					ifFalse:
						[numTemps := tuple last]].
			"create the temp vector for this scope level."
			tempVector := Array new: numTemps.
			"fill it in with any indirect temp vectors"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector at: index put: (Array new: size)].
			"fill it in with temp nodes."
			tempNameTupleVector do:
				[:tuple| | itv |
				tuple last isArray
					ifTrue:
						[itv := tempVector at: tuple last first.
						 itv at: tuple last last
							put: (aDecompilerConstructor
									codeTemp: tuple last last - 1
									named: tuple first)]
					ifFalse:
						[tempVector
							at: tuple last
							put: (aDecompilerConstructor
									codeTemp: tuple last - 1
									named: tuple first)]].
			"replace any indirect temp vectors with proper RemoteTempVectorNodes"
			subMap keysAndValuesDo:
				[:index :size|
				tempVector
					at: index
					put: (aDecompilerConstructor
							codeRemoteTemp: index
							remoteTemps: (tempVector at: index))].
			"and update the entry in the map"
			map at: startpc put: tempVector]].
	^map
</details>

#### Decompiler>>#case: dist

statements = keyStmts #CascadeFlag keyValueBlock ... keyStmts


<details>
	<summary>See more</summary>
	
	case: dist
	"statements = keyStmts #CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add #CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: #CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == #CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns weirdly I guess)"
			stack removeLast. "get rid of #CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == #CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].
</details>

#### Decompiler>>#tempAt: offset

Needed by BraceConstructor<PopIntoTemporaryVariable


<details>
	<summary>See more</summary>
	
	tempAt: offset
	"Needed by BraceConstructor<PopIntoTemporaryVariable"

	^tempVars at: offset + 1
</details>

#### Decompiler>>#withTempNames: tempNames

<Array|String>


<details>
	<summary>See more</summary>
	
	withTempNames: tempNames "<Array|String>"
	"Optionally initialize the temp names to be used when decompiling.
	 For backward-copmpatibility, if tempNames is an Array it is a single
	 vector of temp names, probably for a blue-book-compiled method.
	 If tempNames is a string it is a schematic string that encodes the
	 layout of temp vars in the method and any closures/blocks within it.
	 Decoding encoded tempNames is done in decompile:in:method:using:
	 which has the method from which to derive blockStarts.
	 See e.g. BytecodeEncoder>>schematicTempNamesString for syntax."
	tempVars := tempNames
</details>

#### Decompiler>>#quickMethod

<details>
	<summary>See more</summary>
	
	quickMethod
	| |
	method isReturnSpecial
		ifTrue: [^ constructor codeBlock:
				(Array with: (constTable at: method primitive - 255)) returns: true].
	method isReturnField
		ifTrue: [^ constructor codeBlock:
				(Array with: (constructor codeInst: method returnField)) returns: true].
	self error: 'improper short method'
</details>

#### Decompiler>>#storeIntoTemporaryVariable: offset

<details>
	<summary>See more</summary>
	
	storeIntoTemporaryVariable: offset

	self pushTemporaryVariable: offset; doStore: stack
</details>

#### Decompiler>>#doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize

<details>
	<summary>See more</summary>
	
	doClosureCopyCopiedValues: blockCopiedValues numArgs: numArgs blockSize: blockSize
	| startpc savedTemps savedTempVarCount savedNumLocalTemps
	  jump blockArgs blockTemps blockTempsOffset block |
	savedTemps := tempVars.
	savedTempVarCount := tempVarCount.
	savedNumLocalTemps := numLocalTemps.
	jump := blockSize + (startpc := pc).
	numLocalTemps := BlockLocalTempCounter tempCountForBlockStartingAt: pc in: method.
	blockTempsOffset := numArgs + blockCopiedValues size.
	(blockStartsToTempVars notNil "implies we were intialized with temp names."
	 and: [blockStartsToTempVars includesKey: pc])
		ifTrue:
			[tempVars := blockStartsToTempVars at: pc]
		ifFalse:
			[blockArgs := (1 to: numArgs) collect:
							[:i| (constructor
									codeTemp: i - 1
									named: 'arg', (tempVarCount + i) printString)
								  beBlockArg].
			blockTemps := (1 to: numLocalTemps) collect:
							[:i| constructor
									codeTemp: i + blockTempsOffset - 1
									named: 'temp', (tempVarCount + i + numArgs) printString].
			tempVars := blockArgs, blockCopiedValues, blockTemps].
	numLocalTemps timesRepeat:
		[self interpretNextInstructionFor: self.
		 stack removeLast].
	tempVarCount := tempVarCount + numArgs + numLocalTemps.
	block := self blockTo: jump.
	stack addLast: ((constructor
						codeArguments: (tempVars copyFrom: 1 to: numArgs)
						temps: (tempVars copyFrom: blockTempsOffset + 1 to: blockTempsOffset + numLocalTemps)
						block: block)
							pc: startpc;
							yourself).
	tempVars := savedTemps.
	tempVarCount := savedTempVarCount.
	numLocalTemps := savedNumLocalTemps
</details>

#### Decompiler>>#checkForBlock: receiver selector: selector arguments: arguments

<details>
	<summary>See more</summary>
	
	checkForBlock: receiver selector: selector arguments: arguments
	self assert: selector == #closureCopy:copiedValues:.
	^self checkForClosureCopy: receiver arguments: arguments
</details>

#### Decompiler>>#statementsForCaseTo: end

Decompile the method from pc up to end and return an array of expressions. If at run time this block will leave a value on the stack, set hasValue to true. If the block ends with a jump or return, set exit to the destination of the jump, or the end of the method; otherwise, set exit = end. Leave pc = end. Note that stack initially contains a #CaseFlag which will be removed by a subsequent Pop instruction, so adjust the StackPos accordingly.


<details>
	<summary>See more</summary>
	
	statementsForCaseTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a #CaseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for #CaseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == #CaseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos
</details>

#### Decompiler>>#send: selector super: superFlag numArgs: numArgs

<details>
	<summary>See more</summary>
	
	send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	(#closureCopy:copiedValues: == selector
	  and: [self checkForBlock: rcvr selector: selector arguments: args]) ifFalse:
		[selNode := constructor codeAnySelector: selector.
		rcvr == #CascadeFlag
			ifTrue:
				["May actually be a cascade or an ifNil: for value."
				self willJumpIfFalse
					ifTrue: "= generated by a case macro"
						[selector == #= ifTrue:
							[" = signals a case statement..."
							statements addLast: args first.
							stack addLast: rcvr. "restore #CascadeFlag"
							^ self].
						selector == #== ifTrue:
							[" == signals an ifNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]]
					ifFalse:
						[(self willJumpIfTrue and: [selector == #==]) ifTrue:
							[" == signals an ifNotNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]].
				msgNode := constructor
								codeCascadedMessage: selNode
								arguments: args.
				stack last == #CascadeFlag ifFalse:
					["Last message of a cascade"
					statements addLast: msgNode.
					messages := self popTo: stack removeLast.  "Depth saved by first dup"
					msgNode := constructor
									codeCascade: stack removeLast
									messages: messages]]
			ifFalse:
				[msgNode := constructor
							codeMessage: rcvr
							selector: selNode
							arguments: args].
		stack addLast: msgNode]
</details>

#### Decompiler>>#scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner

<details>
	<summary>See more</summary>
	
	scanBlockScopeFor: refpc from: startpc to: endpc with: scan scanner: scanner
	| bsl maybeBlockSize |
	bsl := BlockStartLocator new.
	scanner pc: startpc.
	[scanner pc <= endpc] whileTrue:
		[refpc = scanner pc ifTrue:
			[scanner pc: startpc.
			 [scanner pc <= endpc] whileTrue:
				[(scan value: scanner firstByte) ifTrue:
					[^endpc].
				 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
					[scanner pc: scanner pc + maybeBlockSize]].
			   ^self].
		 (maybeBlockSize := scanner interpretNextInstructionFor: bsl) isInteger ifTrue:
			[refpc <= (scanner pc + maybeBlockSize)
				ifTrue: [^self scanBlockScopeFor: refpc from: scanner pc to: scanner pc + maybeBlockSize with: scan scanner: scanner]
				ifFalse: [scanner pc: scanner pc + maybeBlockSize]]]
</details>

#### Decompiler>>#blockScopeRefersOnlyOnceToTemp: offset

<details>
	<summary>See more</summary>
	
	blockScopeRefersOnlyOnceToTemp: offset
	| nRefs byteCode extension scanner scan |
	scanner := InstructionStream on: method.
	nRefs := 0.
	scan := offset <= 15
				ifTrue:
					[byteCode := 16 + offset.
					 [:instr |
					  instr = byteCode ifTrue:
						[nRefs := nRefs + 1].
					  nRefs > 1]]
				ifFalse:
					[extension := 64 + offset.
					 [:instr |
					  (instr = 128 and: [scanner followingByte = extension]) ifTrue:
						[nRefs := nRefs + 1].
					   nRefs > 1]].
	self scanBlockScopeFor: pc from: method initialPC to: method endPC with: scan scanner: scanner.
	^nRefs = 1
</details>

#### Decompiler>>#pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	stack addLast: ((tempVars at: tempVectorIndex + 1) remoteTemps at: remoteTempIndex + 1)
</details>

#### Decompiler>>#popTo: oldPos

<details>
	<summary>See more</summary>
	
	popTo: oldPos

	| t |
	t := Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t
</details>

#### Decompiler>>#blockReturnTop

No action needed


<details>
	<summary>See more</summary>
	
	blockReturnTop
	"No action needed"
</details>

#### Decompiler>>#decompile: aSelector in: aClass

See Decompiler|decompile:in:method:. The method is found by looking up the message, aSelector, in the method dictionary of the class, aClass.


<details>
	<summary>See more</summary>
	
	decompile: aSelector in: aClass 
	"See Decompiler|decompile:in:method:. The method is found by looking up 
	the message, aSelector, in the method dictionary of the class, aClass."

	^self
		decompile: aSelector
		in: aClass
		method: (aClass compiledMethodAt: aSelector) methodForDecompile
</details>

#### Decompiler>>#storeIntoLiteralVariable: assoc

<details>
	<summary>See more</summary>
	
	storeIntoLiteralVariable: assoc

	self pushLiteralVariable: assoc; doStore: stack
</details>

#### Decompiler>>#pushConstant: value

<details>
	<summary>See more</summary>
	
	pushConstant: value

	| node |
	node := value == true ifTrue: [constTable at: 2]
		ifFalse: [value == false ifTrue: [constTable at: 3]
		ifFalse: [value == nil ifTrue: [constTable at: 4]
		ifFalse: [constructor codeAnyLiteral: value]]].
	stack addLast: node
</details>

#### Decompiler>>#constructorForMethod: aMethod

<details>
	<summary>See more</summary>
	
	constructorForMethod: aMethod
	^DecompilerConstructor new
</details>

#### Decompiler>>#methodReturnTop

<details>
	<summary>See more</summary>
	
	methodReturnTop
	| last |
	last := stack removeLast "test test" asReturnNode.
	stack size > blockStackBase  "get effect of elided pop before return"
		ifTrue: [statements addLast: stack removeLast].
	exit := pc.
	lastJumpPc := lastReturnPc := lastPc.
	statements addLast: last
</details>

#### Decompiler>>#pushConsArrayWithElements: numElements

<details>
	<summary>See more</summary>
	
	pushConsArrayWithElements: numElements 
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: stack removeLast].
	stack addLast: (constructor codeBrace: array)
</details>

#### Decompiler>>#popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: statements
</details>

#### Decompiler>>#jump: dist

<details>
	<summary>See more</summary>
	
	jump: dist
	| blockBody destPc nextPC |
	destPc := pc + dist.
	(lastJumpIfPcStack isEmpty or: [dist < 0 and: [destPc > lastJumpIfPcStack last]])
		ifTrue:
			["Rule: aBackward jump not crossing a Bfp/Btp must be a repeat"
			nextPC := pc.
			pc := destPc.
			blockBody := self statementsTo: lastPc.
			blockBody size timesRepeat: [statements removeLast].
			pc := nextPC.
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: #repeat
								code: #macro)
					arguments: #()).
			]
		ifFalse:
			[exit := destPc.
			lastJumpPc := lastPc]
</details>

#### Decompiler>>#pushActiveContext

<details>
	<summary>See more</summary>
	
	pushActiveContext

	stack addLast: constructor codeThisContext
</details>

#### Decompiler>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	| copiedValues |

	copiedValues := ((1 to: numCopied) collect: [:ign| stack removeLast]) reversed.
	self doClosureCopyCopiedValues: copiedValues numArgs: numArgs blockSize: blockSize
</details>

#### Decompiler>>#decompile: aSelector in: aClass method: aMethod

Answer a MethodNode that is the root of the parse tree for the argument, aMethod, which is the CompiledMethod associated with the message, aSelector. Variables are determined with respect to the argument, aClass.


<details>
	<summary>See more</summary>
	
	decompile: aSelector in: aClass method: aMethod
	"Answer a MethodNode that is the root of the parse tree for the 
	argument, aMethod, which is the CompiledMethod associated with the 
	message, aSelector. Variables are determined with respect to the 
	argument, aClass."

	^self
		decompile: aSelector
		in: aClass
		method: aMethod
		using: (self constructorForMethod: aMethod)
</details>

#### Decompiler>>#storeIntoReceiverVariable: offset

<details>
	<summary>See more</summary>
	
	storeIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: stack
</details>

#### Decompiler>>#decompile: aSelector in: aClass method: aMethod using: aConstructor

<details>
	<summary>See more</summary>
	
	decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method primitive ~= 0 and: [self skipCallPrimitive; willStore]) ifTrue:
				[pc := pc + (method encoderClass bytecodeSize: self firstByte).
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method primitive > 0 ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen
</details>

#### Decompiler>>#pushNewArrayOfSize: size

<details>
	<summary>See more</summary>
	
	pushNewArrayOfSize: size

	stack addLast: #pushNewArray -> (Array new: size)
</details>

#### Decompiler>>#jump: dist if: condition

<details>
	<summary>See more</summary>
	
	jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack blockBody blockArgs |
	lastJumpIfPcStack addLast: lastPc.
	stack last == #CascadeFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == #IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			["Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by re-decompiling."
			stack := saveStack.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			blockArgs := thenBlock statements = constructor codeEmptyBlock statements
							ifTrue: [#()]
							ifFalse: [{ thenBlock }].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: (blockArgs isEmpty
												ifTrue:
													[sign
														ifTrue: [#whileFalse]
														ifFalse: [#whileTrue]]
												ifFalse:
													[sign
														ifTrue: [#whileFalse:]
														ifFalse: [#whileTrue:]])
								code: #macro)
					arguments: blockArgs).
			pc := elseStart.
			self convertToDoLoop]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: (Array with: thenBlock)]
						ifFalse:
							[constructor
								codeMessage: ifExpr
								selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
								arguments:	(sign
												ifTrue: [{elseBlock. thenBlock}]
												ifFalse: [{thenBlock. elseBlock}])].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast.
</details>

#### Decompiler>>#decompileBlock: aBlock

Decompile aBlock, returning the result as a BlockNode. Show temp names from source if available.


<details>
	<summary>See more</summary>
	
	decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompile)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'
</details>

#### Decompiler>>#blockTo: end

Decompile a range of code as in statementsTo:, but return a block node.


<details>
	<summary>See more</summary>
	
	blockTo: end
	"Decompile a range of code as in statementsTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block
</details>

#### Decompiler>>#pushReceiver

<details>
	<summary>See more</summary>
	
	pushReceiver

	stack addLast: (constTable at: 1)
</details>

#### Decompiler>>#interpretNextInstructionFor: client

Change false here will trace all state in Transcript.


<details>
	<summary>See more</summary>
	
	interpretNextInstructionFor: client

	| code varNames |

"Change false here will trace all state in Transcript."
true ifTrue: [^ super interpretNextInstructionFor: client].

	varNames := self class allInstVarNames.
	code := (self method at: pc) printStringBase: 16.
	Transcript newLine; newLine; print: pc; space;
		nextPutAll: '<' , code, '>'.
	8 to: varNames size do:
		[:i | i <= 10 ifTrue: [Transcript newLine]
				ifFalse: [Transcript space; space].
		Transcript nextPutAll: (varNames at: i);
				nextPutAll: ': '; print: (self instVarAt: i)].
	Transcript endEntry.
	^ super interpretNextInstructionFor: client
</details>

#### Decompiler>>#statementsTo: end

Decompile the method from pc up to end and return an array of expressions. If at run time this block will leave a value on the stack, set hasValue to true. If the block ends with a jump or return, set exit to the destination of the jump, or the end of the method; otherwise, set exit = end. Leave pc = end.


<details>
	<summary>See more</summary>
	
	statementsTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end."

	| blockPos stackPos t |
	blockPos := statements size.
	stackPos := stack size.
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[statements addLast: stack removeLast].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	^self popTo: blockPos
</details>

#### Decompiler>>#doDup

<details>
	<summary>See more</summary>
	
	doDup

	stack last == #CascadeFlag
		ifFalse: [
			"Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: #CascadeFlag].
	stack addLast: #CascadeFlag
</details>

#### Decompiler>>#popIntoTemporaryVariable: offset

<details>
	<summary>See more</summary>
	
	popIntoTemporaryVariable: offset
	| maybeTVTag tempVector start |
	maybeTVTag := stack last.
	((maybeTVTag isMemberOf: Association)
	 and: [maybeTVTag key == #pushNewArray]) ifTrue:
		[blockStartsToTempVars notNil "implies we were intialized with temp names."
			ifTrue: "Use the provided temps"
				[self assert: ((tempVector := tempVars at: offset + 1 ifAbsent: [ParseNode basicNew]) isTemp
							 and: [tempVector isIndirectTempVector
							 and: [tempVector remoteTemps size = maybeTVTag value size]])]
			ifFalse: "Synthesize some remote temps"
				[tempVector := maybeTVTag value.
				 offset + 1 <= tempVars size
					ifTrue:
						[start := 2.
						 tempVector at: 1 put: (tempVars at: offset + 1)]
					ifFalse:
						[tempVars := (Array new: offset + 1)
										replaceFrom: 1
										to: tempVars size
										with: tempVars.
						start := 1].
				 start to: tempVector size do:
					[:i|
					tempVector
						at: i
						put: (constructor
								codeTemp: numLocalTemps + offset + i - 1
								named: 'temp', (tempVarCount + i) printString)].
				tempVars at: offset + 1 put: (constructor codeRemoteTemp: offset + 1 remoteTemps: tempVector)].
		 tempVarCount := tempVarCount + maybeTVTag value size.
		 stack removeLast.
		 ^self].
	self pushTemporaryVariable: offset; doStore: statements
</details>

#### Decompiler>>#popIntoLiteralVariable: value

<details>
	<summary>See more</summary>
	
	popIntoLiteralVariable: value

	self pushLiteralVariable: value; doStore: statements
</details>

#### Decompiler>>#convertToDoLoop

If statements contains the pattern var := startExpr. [var <= limit] whileTrue: [...statements... var := var + incConst] then replace this by startExpr to: limit by: incConst do: [:var | ...statements...]


<details>
	<summary>See more</summary>
	
	convertToDoLoop
	"If statements contains the pattern
		var := startExpr.
		[var <= limit] whileTrue: [...statements... var := var + incConst]
	then replace this by
		startExpr to: limit by: incConst do: [:var | ...statements...]"
	| leaveOnStack initStmt toDoStmt limitStmt |
	leaveOnStack := false.
	(stack notEmpty
	 and: [(stack last == #CaseFlag) not and: [ stack last isAssignmentNode]])
		ifTrue:
			[initStmt := stack last.
			 (toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			 stack removeLast.
			 statements removeLast; addLast: toDoStmt.
			 leaveOnStack := true]
		ifFalse:
			[statements size < 2 ifTrue:
				[^self].
			initStmt := statements at: statements size-1.
			(toDoStmt := statements last toDoFromWhileWithInit: initStmt) ifNil:
				[^self].
			statements removeLast; removeLast; addLast: toDoStmt].
	initStmt variable scope: -1.  "Flag arg as block temp"

	"Attempt further conversion of the pattern
		limitVar := limitExpr.
		startExpr to: limitVar by: incConst do: [:var | ...statements...]
	to
		startExpr to: limitExpr by: incConst do: [:var | ...statements...].
	The complication here is that limitVar := limitExpr's value may be used, in which case it'll
	be statements last, or may not be used, in which case it'll be statements nextToLast."
	statements size < 2 ifTrue:
		[leaveOnStack ifTrue:
			[stack addLast: statements removeLast].
			 ^self].
	limitStmt := statements last.
	((limitStmt isMemberOf: AssignmentNode)
		and: [limitStmt variable isTemp
		and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
			[limitStmt := statements at: statements size-1.
			((limitStmt isMemberOf: AssignmentNode)
				and: [limitStmt variable isTemp
				and: [limitStmt variable == toDoStmt arguments first]]) ifFalse:
					[leaveOnStack ifTrue:
						[stack addLast: statements removeLast].
					^self]].

	(self blockScopeRefersOnlyOnceToTemp: limitStmt variable fieldOffset) ifFalse:
		[^self].
	toDoStmt arguments at: 1 put: limitStmt value.
	limitStmt variable scope: -2.  "Flag limit var so it won't print"
	statements last == limitStmt
		ifTrue: [statements removeLast]
		ifFalse: [statements removeLast; removeLast; addLast: toDoStmt]
</details>

#### Decompiler>>#storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: stack
</details>

#### Decompiler>>#doStore: stackOrBlock

Only called internally, not from InstructionStream. StackOrBlock is stack for store, statements for storePop.


<details>
	<summary>See more</summary>
	
	doStore: stackOrBlock
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == #ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])
</details>

#### Decompiler>>#pushLiteralVariable: assoc

<details>
	<summary>See more</summary>
	
	pushLiteralVariable: assoc

	stack addLast: (constructor codeAnyLitInd: assoc)
</details>

#### Decompiler>>#checkForClosureCopy: receiver arguments: arguments

We just saw a closureCopy:copiedValues: message. Check for and construct a following block.


<details>
	<summary>See more</summary>
	
	checkForClosureCopy: receiver arguments: arguments
	"We just saw a closureCopy:copiedValues: message. Check for and construct a following block."

	| savePc jump |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) notNil ifFalse:
		[pc := savePc.
		 ^nil].
	"Definitely a block"
	self doClosureCopyCopiedValues: arguments last "<BraceNode>" elements
		numArgs: arguments first key
		blockSize: jump.
	^true
</details>

#### Decompiler>>#initSymbols: aClass

<details>
	<summary>See more</summary>
	
	initSymbols: aClass
	| argCount |
	constructor method: method class: aClass literals: method literals.
	constTable _ constructor codeConstants.
	instVars _ Array new: aClass instSize.
	tempVarCount _ method numTemps.
	argCount _ method numArgs.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars _ self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars _ blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps _ tempVars ifNil: [(1 to: tempVarCount) collect: [ :i | 
					(i <= argCount ifTrue: ['arg'] ifFalse: ['temp']), i printString]].
			tempVars _ (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i)
			beMethodArg]
</details>

#### Decompiler>>#methodReturnReceiver

<details>
	<summary>See more</summary>
	
	methodReturnReceiver

	self pushReceiver; methodReturnTop
</details>

#### Decompiler>>#popIntoReceiverVariable: offset

<details>
	<summary>See more</summary>
	
	popIntoReceiverVariable: offset

	self pushReceiverVariable: offset; doStore: statements
</details>

#### Decompiler>>#blockForCaseTo: end

Decompile a range of code as in statementsForCaseTo:, but return a block node.


<details>
	<summary>See more</summary>
	
	blockForCaseTo: end
	"Decompile a range of code as in statementsForCaseTo:, but return a block node."
	| exprs block oldBase |
	oldBase := blockStackBase.
	blockStackBase := stack size.
	exprs := self statementsForCaseTo: end.
	block := constructor codeBlock: exprs returns: lastReturnPc = lastPc.
	blockStackBase := oldBase.
	lastReturnPc := -1.  "So as not to mislead outer calls"
	^block
</details>

#### Decompiler>>#pushTemporaryVariable: offset

<details>
	<summary>See more</summary>
	
	pushTemporaryVariable: offset

	stack addLast: (offset >= tempVars size
		ifTrue:
			["Handle the case of chained LiteralVariableBinding assigments"
			stack at: (offset + 1 - tempVars size)]
		ifFalse:
			["A regular argument or temporary"
			tempVars at: offset + 1])
</details>

## Encoder

I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.

### Methods
#### Encoder>>#warnAboutShadowed: name

<details>
	<summary>See more</summary>
	
	warnAboutShadowed: name

	| msg |
	
	msg _ 'There already exists a variable named ', name, ' '.
	requestor addWarning: msg.
	Transcript newLine; show: msg.
	InMidstOfFileinNotification signal
		ifFalse: [
			requestor interactive
				ifTrue: [
					self notify: msg]
				ifFalse: [
					(RecompilationFailure
						class: class
						messageText: msg, String newLineString, 'Please use a different name') signal ]]
</details>

#### Encoder>>#encodeVariable: name sourceRange: range ifUnknown: action

<details>
	<summary>See more</summary>
	
	encodeVariable: name sourceRange: range ifUnknown: action
	| varNode |
	varNode := scopeTable
					at: name
					ifAbsent: 
						[(self lookupInPools: name 
							ifFound: [:assoc | varNode := self global: assoc name: name])
							ifTrue: [varNode]
							ifFalse: [^action value]].
	range ifNotNil:
		[name first canBeGlobalVarInitial ifTrue:
			[globalSourceRanges addLast: { name. range. false }]].

	(varNode isTemp and: [varNode scope < 0]) ifTrue:
		[^OutOfScopeNotification signal
			ifTrue: [action value]
			ifFalse: [self notify: 'out of scope']].
	^varNode
</details>

#### Encoder>>#floatTemp: node

<details>
	<summary>See more</summary>
	
	floatTemp: node
	(node ~~ (scopeTable at: node name ifAbsent: [])
	or: [node class ~~ TempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1
</details>

#### Encoder>>#newUndeclaredTemp: name

<details>
	<summary>See more</summary>
	
	newUndeclaredTemp: name
	^UndeclaredVariableNode new name: name
</details>

#### Encoder>>#newTemp: name

<details>
	<summary>See more</summary>
	
	newTemp: name

	nTemps := nTemps + 1.
	^ TempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0
</details>

#### Encoder>>#bindTemp: name

Declare a temporary; error not if a field or class variable.


<details>
	<summary>See more</summary>
	
	bindTemp: name 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		node isTemp
			ifTrue:[^self notify:'Name already used in this method']
			ifFalse:[self warnAboutShadowed: name]].
	^self reallyBind: name
</details>

#### Encoder>>#rangesForVariable: aName checkingType: nodeTypeCheckBlock ifAbsent: anAbsentBlock

<details>
	<summary>See more</summary>
	
	rangesForVariable: aName checkingType: nodeTypeCheckBlock ifAbsent: anAbsentBlock

	| variableNode |
	
	variableNode := scopeTable at: aName ifAbsent: [ ^anAbsentBlock value ].
	(nodeTypeCheckBlock value: variableNode) ifFalse: [ ^anAbsentBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: anAbsentBlock 
</details>

#### Encoder>>#rangesForLiteralNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	rangesForLiteralNode: aName ifAbsent: aBlock

	| variableNode |
	
	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock
</details>

#### Encoder>>#literals

Should only be used for decompiling primitives


<details>
	<summary>See more</summary>
	
	literals
	"Should only be used for decompiling primitives"
	^ literalStream contents
</details>

#### Encoder>>#globalSourceRanges

<details>
	<summary>See more</summary>
	
	globalSourceRanges

	^ globalSourceRanges
</details>

#### Encoder>>#messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: absentBlock

<details>
	<summary>See more</summary>
	
	messageSendKeywordPositionsAt: anIndex of: aSelector ifAbsent: absentBlock

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (aMessageSendNode keywordPositionAt: anIndex) first ]
		ifAbsent: absentBlock
	
</details>

#### Encoder>>#positionsOfLiteralArraysContaining: aSymbol

<details>
	<summary>See more</summary>
	
	positionsOfLiteralArraysContaining: aSymbol

	| positions |

	positions := OrderedCollection new.
	litSet keysAndValuesDo: [ :aLiteral :aLiteralNode |
		(aLiteral isArray and: [ aLiteral hasLiteral: aSymbol ]) ifTrue: [ positions addAll: (sourceRanges at: aLiteralNode) ]].

	^positions 
</details>

#### Encoder>>#init: aClass context: aContext notifying: req

<details>
	<summary>See more</summary>
	
	init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: self doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= self doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32
</details>

#### Encoder>>#parseNodeIncluding: aPosition ifAbsent: aBlockClosure

<details>
	<summary>See more</summary>
	
	parseNodeIncluding: aPosition ifAbsent: aBlockClosure

	| smallestRangeSize nodeWithRangeAtPosition |

	smallestRangeSize := SmallInteger maxVal.
	nodeWithRangeAtPosition := nil.

	sourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange | | currentNodeRangeSize |
				currentNodeRangeSize := aRange size.
				currentNodeRangeSize < smallestRangeSize ifTrue: [
					smallestRangeSize := currentNodeRangeSize.
					nodeWithRangeAtPosition := nodeAtRange key -> aRange ]]].

	^ nodeWithRangeAtPosition ifNil: aBlockClosure ifNotNil: [ nodeWithRangeAtPosition ]
			
</details>

#### Encoder>>#messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendKeywordAndParameterPositionsAt: anIndex of: aSelector ifAbsent: aBlock

	| positions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordAndParameterPositionAt: anIndex encodedWith: self ifAbsent: aBlock].

	^ positions isEmpty
		ifTrue: aBlock
		ifFalse: [ positions ]
	
</details>

#### Encoder>>#lookupVariable: name ifAbsent: aBlock

Answer the binding of name in the scope table or aBlock's vaue if none. Do not bind and do not lookup in pools. Used for correction, explanation etc


<details>
	<summary>See more</summary>
	
	lookupVariable: name ifAbsent: aBlock
	"Answer the binding of name in the scope table or aBlock's vaue if none.
	 Do not bind and do not lookup in pools.  Used for correction, explanation etc"
	^scopeTable at: name ifAbsent: aBlock
</details>

#### Encoder>>#rangesForLiteralVariableNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	rangesForLiteralVariableNode: aName ifAbsent: aBlock

	| variableNode |
	
	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].
	
	^sourceRanges at: variableNode ifAbsent: aBlock
</details>

#### Encoder>>#associationForClass

<details>
	<summary>See more</summary>
	
	associationForClass
	| assoc |
	assoc _ Smalltalk associationAt: class name ifAbsent: [ nil ].
	^assoc value == class
		ifTrue: [ assoc ]
		ifFalse: [ Association new value: class ]
</details>

#### Encoder>>#withRangesOf: nodeAtRange including: aPosition do: aBlock

<details>
	<summary>See more</summary>
	
	withRangesOf: nodeAtRange including: aPosition do: aBlock

	| currentNodeRange ranges |

	currentNodeRange := nodeAtRange value.
	ranges := currentNodeRange isInterval ifTrue: [ Array with: currentNodeRange ] ifFalse: [ currentNodeRange ].

	ranges do: [ :aRange | (aRange includes: aPosition) ifTrue: [ aBlock value: aRange ]].

	
</details>

#### Encoder>>#encodeSelector: aSelector

Don't call it 'selector'. It would be shadowed.


<details>
	<summary>See more</summary>
	
	encodeSelector: aSelector
	"Don't call it 'selector'. It would be shadowed."
	^self
		name: aSelector
		key: aSelector
		class: SelectorNode
		type: SendType
		set: selectorSet
</details>

#### Encoder>>#bindBlockArg: name within: aBlockNode

With standard Smalltalk-80 (BlueBook) blocks it used to be legal to use a method temp as a block argument. This shouldn't be the case with the current compiler, which checks for temp names already being used as block arguments. But it is easily fooled by local block temps in optimized blocks, e.g. false ifTrue: [| temp |] ifFalse:[[:temp|]] Rather than fix this we keep the semantics and fix it in the closure compiler.


<details>
	<summary>See more</summary>
	
	bindBlockArg: name within: aBlockNode
	"With standard Smalltalk-80 (BlueBook) blocks it used to be legal to use a
	 method temp as a block argument.  This shouldn't be the case with the
	 current compiler, which checks for temp names already being used as
	 block arguments.  But it is easily fooled by local block temps in optimized
	 blocks, e.g.
		false
			ifTrue: [| temp |]
			ifFalse:[[:temp|]]
	Rather than fix this we keep the semantics and fix it in the closure compiler."
	^self autoBind: name
</details>

#### Encoder>>#global: ref name: name

<details>
	<summary>See more</summary>
	
	global: ref name: name

	^self
		name: name
		key: ref
		class: LiteralVariableNode
		type: LdLitIndType
		set: litIndSet
</details>

#### Encoder>>#bindTemp: aName range: aRange

<details>
	<summary>See more</summary>
	
	bindTemp: aName range: aRange

	^ self addMultiRange: aRange for: (self bindTemp: aName)
</details>

#### Encoder>>#fillDict: dict with: nodeClass mapping: keys to: codeArray

<details>
	<summary>See more</summary>
	
	fillDict: dict with: nodeClass mapping: keys to: codeArray
	| codeStream |
	codeStream := ReadStream on: codeArray.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]
</details>

#### Encoder>>#encodeLiteral: object range: aRange

<details>
	<summary>See more</summary>
	
	encodeLiteral: object range: aRange

	^ self addMultiRange: aRange for: (self encodeLiteral: object)
</details>

#### Encoder>>#classEncoding

This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view.


<details>
	<summary>See more</summary>
	
	classEncoding
	"This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view."
	^ class
</details>

#### Encoder>>#addMultiRange: aRange for: aNode

I'm using an OrderedCollection because ranges are added in order, while parsing the source code. If this constrain is not hold, a SortedCollection should be used - Hernan


<details>
	<summary>See more</summary>
	
	addMultiRange: aRange for: aNode

	| ranges |
	
	"I'm using an OrderedCollection because ranges are added in order, while parsing the source code.
	If this constrain is not hold, a SortedCollection should be used - Hernan"
	ranges := sourceRanges at: aNode ifAbsentPut: [ OrderedCollection new ].
	ranges add: aRange.
	
	^aNode 
</details>

#### Encoder>>#sourceMap

Answer with a sorted set of associations (pc range).


<details>
	<summary>See more</summary>
	
	sourceMap
	"Answer with a sorted set of associations (pc range)."

	^ (sourceRanges keys collect: 
		[:key |  Association key: key pc value: (sourceRanges at: key)])
			asSortedCollection
</details>

#### Encoder>>#rawSourceRanges

<details>
	<summary>See more</summary>
	
	rawSourceRanges

	^ sourceRanges 
</details>

#### Encoder>>#name: name key: key class: leafNodeClass type: type set: dict

<details>
	<summary>See more</summary>
	
	name: name key: key class: leafNodeClass type: type set: dict 
	^dict at: key
		ifAbsentPut: 
			[leafNodeClass new 
				name: name
				key: key
				index: nil
				type: type]
</details>

#### Encoder>>#doItInContextName

<details>
	<summary>See more</summary>
	
	doItInContextName
	^'ThisContext'
</details>

#### Encoder>>#bindBlockArg: name within: aBlockNode range: range

<details>
	<summary>See more</summary>
	
	bindBlockArg: name within: aBlockNode range: range
	
	^self addMultiRange: range for: (self bindBlockArg: name within: aBlockNode)
	

</details>

#### Encoder>>#positionsForLiteralVariableNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForLiteralVariableNode: aName ifAbsent: aBlock

	| variableNode |

	variableNode := litIndSet values detect: [ :aLiteralVariableNode | aLiteralVariableNode name = aName ] ifNone: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock
</details>

#### Encoder>>#release

<details>
	<summary>See more</summary>
	
	release

	requestor := nil
</details>

#### Encoder>>#withMessageSendKeywordPositionsOf: aSelector do: aMessageSendNodeBlock ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	withMessageSendKeywordPositionsOf: aSelector do: aMessageSendNodeBlock ifAbsent: aBlock

	| positions sortedPositions |

	positions := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: aMessageSendNodeBlock.

	positions isEmpty ifTrue: [ ^aBlock value ].
	sortedPositions := positions asSortedCollection.

	^sortedPositions 
</details>

#### Encoder>>#messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendSelectorKeywordPositionsOf: aSelector ifAbsent: aBlock

	| ranges sortedRanges |

	ranges := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordRanges ].

	ranges isEmpty ifTrue: [ ^aBlock value ].
	sortedRanges := ranges asSortedCollection: [ :left :right | left first first < right first first ].

	^sortedRanges 
</details>

#### Encoder>>#completeSourceRangesBasedOn: sourceCode

<details>
	<summary>See more</summary>
	
	completeSourceRangesBasedOn: sourceCode

	| completeSourceRanges |
	completeSourceRanges _ Dictionary new.
	sourceRanges keysAndValuesDo: [ :parseNode :nodeRanges |
		"leaf nodes excluded because they have the same complete source ranges than the block nodes they wrap - Nahuel"
		parseNode class = LeafNode ifFalse: [
			| expandedNodeSourceRanges |
			expandedNodeSourceRanges _ parseNode expandRanges: nodeRanges basedOn: sourceRanges using: sourceCode.
			completeSourceRanges at: parseNode put: expandedNodeSourceRanges ] ].
	^ completeSourceRanges
</details>

#### Encoder>>#unusedTempNames

<details>
	<summary>See more</summary>
	
	unusedTempNames 
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= self doItInContextName ifTrue: [unused add: name]]].
	^ unused
</details>

#### Encoder>>#temps: tempVars literals: lits class: cl

Decompile.


<details>
	<summary>See more</summary>
	
	temps: tempVars literals: lits class: cl 
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	literalStream := ReadStream on: lits.
	literalStream position: lits size.
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32.

</details>

#### Encoder>>#bindBlockTemp: name within: aBlockNode range: range

<details>
	<summary>See more</summary>
	
	bindBlockTemp: name within: aBlockNode range: range

	^self addMultiRange: range for: (self bindBlockTemp: name within: aBlockNode)
	

</details>

#### Encoder>>#fixTemp: name

<details>
	<summary>See more</summary>
	
	fixTemp: name
	| node |
	node := scopeTable at: name ifAbsent: [].
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node
</details>

#### Encoder>>#maxNumLiterals

<details>
	<summary>See more</summary>
	
	maxNumLiterals
	^CompiledMethod maxNumLiterals min: self maxIndexableLiterals
</details>

#### Encoder>>#possibleNamesFor: proposedName

<details>
	<summary>See more</summary>
	
	possibleNamesFor: proposedName
	| results |
	results := class possibleVariablesFor: proposedName continuedFrom: nil.
	^ proposedName correctAgainst: nil continuedFrom: results.

</details>

#### Encoder>>#positionsForLiteralNode: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForLiteralNode: aName ifAbsent: aBlock

	| variableNode |

	variableNode := litSet at: aName ifAbsent: [ ^aBlock value ].

	^sourceRanges at: variableNode ifAbsent: aBlock
</details>

#### Encoder>>#initScopeAndLiteralTables

<details>
	<summary>See more</summary>
	
	initScopeAndLiteralTables

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new: 16.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new
</details>

#### Encoder>>#undeclaredTemps

<details>
	<summary>See more</summary>
	
	undeclaredTemps 
	^(scopeTable select: [:var | var isVariableNode and: [var isUndeclared]]) values
</details>

#### Encoder>>#rangeForNode: node ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	rangeForNode: node ifAbsent: aBlock

	^sourceRanges at: node ifAbsent: aBlock
</details>

#### Encoder>>#noteOptimizedSelector: aSymbol

Register a selector as being optimized. These optimized selectors will later be registered into the literals so that tools can easily browse senders.


<details>
	<summary>See more</summary>
	
	noteOptimizedSelector: aSymbol
	"Register a selector as being optimized.
	These optimized selectors will later be registered into the literals so that tools can easily browse senders."
	optimizedSelectors add: aSymbol
</details>

#### Encoder>>#positionsForTemporaryVariable: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForTemporaryVariable: aName ifAbsent: aBlock

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isTemp ]
		ifAbsent: aBlock 
</details>

#### Encoder>>#messageSendSelectorKeywordRangesOf: aSelector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	messageSendSelectorKeywordRangesOf: aSelector ifAbsent: aBlock
	
	| ranges sortedRanges |
	
	ranges := sourceRanges keys 
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ] 
		thenCollect: [ :aMessageSendNode | aMessageSendNode keywordRanges ].
		
	ranges isEmpty ifTrue: [ ^aBlock value ].
	sortedRanges := ranges asSortedCollection: [ :left :right | left first first < right first first ].
	
	^sortedRanges 
</details>

#### Encoder>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^selector
</details>

#### Encoder>>#bindArg: aName range: aRange

<details>
	<summary>See more</summary>
	
	bindArg: aName range: aRange
 
	^ self addMultiRange: aRange for: (self bindArg: aName)

</details>

#### Encoder>>#undeclared: name

<details>
	<summary>See more</summary>
	
	undeclared: name
	| sym |
	requestor interactive ifTrue: [
		 ^self notify: 'Undeclared'].
	"Allow knowlegeable clients to squash the undeclared warning if they want (e.g.
	 Diffing pretty printers that are simply formatting text).  As this breaks
	 compilation it should only be used by clients that want to discard the result
	 of the compilation.  To squash the warning use e.g.
		[Compiler format: code in: class notifying: nil decorated: false]
			on: UndeclaredVariableWarning
			do: [:ex| ex resume: false]"
	sym := name asSymbol.
	^(UndeclaredVariableWarning new name: name selector: selector class: class) signal
		ifTrue:
			[Undeclared at: sym put: nil.
			self global: (Undeclared associationAt: sym) name: sym]
		ifFalse:
			[self global: (Association key: sym) name: sym]
</details>

#### Encoder>>#noteSourceRange: range forNode: node

<details>
	<summary>See more</summary>
	
	noteSourceRange: range forNode: node

	sourceRanges at: node put: range
</details>

#### Encoder>>#encodeVariable: name ifUnknown: action

<details>
	<summary>See more</summary>
	
	encodeVariable: name ifUnknown: action
	^self encodeVariable: name sourceRange: nil ifUnknown: action
</details>

#### Encoder>>#encodeLiteral: object

<details>
	<summary>See more</summary>
	
	encodeLiteral: object

	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet
</details>

#### Encoder>>#allLiterals

<details>
	<summary>See more</summary>
	
	allLiterals
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 >= self maxNumLiterals
				ifTrue: [self litIndex: optimizedSelectors asArray sort]
				ifFalse: [optimizedSelectors sorted do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents
</details>

#### Encoder>>#tempNodes

<details>
	<summary>See more</summary>
	
	tempNodes 
	| tempNodes |
	tempNodes _ SortedCollection sortBlock: [:n1 :n2 | n1 code <= n2 code].
	scopeTable associationsDo:
		[:assn |
		(assn value is: #Array)
			ifTrue: [assn value do: [:temp| tempNodes add: temp]]
			ifFalse: [assn value isTemp ifTrue: [tempNodes add: assn value]]].
	^tempNodes
	"jmv: perhaps better not include remote temps...
	^ tempNodes reject:
		[ : a |  a isUnusedTemp ]"
</details>

#### Encoder>>#tempsAndBlockArgs

<details>
	<summary>See more</summary>
	
	tempsAndBlockArgs
	| tempNodes |
	tempNodes := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | var |
		var := assn value.
		(var isTemp
		 and: [var isMethodArg not
		 and: [var scope = 0 or: [var scope = -1]]]) ifTrue:
			[tempNodes add: var]].
	^tempNodes
</details>

#### Encoder>>#autoBind: name

Declare a block argument as a temp if not already declared.


<details>
	<summary>See more</summary>
	
	autoBind: name 
	"Declare a block argument as a temp if not already declared."
	| node |
	node := scopeTable 
			at: name
			ifAbsent: 
				[(self lookupInPools: name ifFound: [:assoc | assoc])
					ifTrue: [self warnAboutShadowed: name].
				^ (self reallyBind: name) nowHasDef nowHasRef scope: 1].
	node isTemp
		ifTrue: [node scope >= 0 ifTrue:
					[^ self notify: 'Name already used in this method'].
				node nowHasDef nowHasRef scope: 1]
		ifFalse: [^ self notify: 'Name already used in this class'].
	^node
</details>

#### Encoder>>#requestor: aRequestor

<details>
	<summary>See more</summary>
	
	requestor: aRequestor

	requestor _ aRequestor
</details>

#### Encoder>>#maxTemp

<details>
	<summary>See more</summary>
	
	maxTemp

	^nTemps
</details>

#### Encoder>>#parseNodesPathAt: aPosition using: completeSourceRanges ifAbsent: aBlockClosure

<details>
	<summary>See more</summary>
	
	parseNodesPathAt: aPosition using: completeSourceRanges ifAbsent: aBlockClosure

	| nodesWithRangeAtPosition |
	nodesWithRangeAtPosition _ SortedCollection sortBlock: [ :parseNodeWithSourceRangeOne :parseNodeWithSourceRangeTwo |
		self criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo ].

	completeSourceRanges associationsDo: [ :nodeAtRange |
		self withRangesOf: nodeAtRange including: aPosition do: [ :aRange |
			nodesWithRangeAtPosition add: (nodeAtRange key -> aRange) ] ].

	^ nodesWithRangeAtPosition
		ifEmpty: aBlockClosure
		ifNotEmpty: [ nodesWithRangeAtPosition ]
</details>

#### Encoder>>#maxIndexableLiterals

Answer the maximum number of literals supported by the receiver's bytecode set. This is a nominal value based on the Blue Book bytecode set; subclasses answer a more accurate value.


<details>
	<summary>See more</summary>
	
	maxIndexableLiterals
	"Answer the maximum number of literals supported by the receiver's
	 bytecode set. This is a nominal value based on the Blue Book bytecode
	 set; subclasses answer a more accurate value."
	^63
</details>

#### Encoder>>#notify: string

Put a separate notifier on top of the requestor's window


<details>
	<summary>See more</summary>
	
	notify: string
	"Put a separate notifier on top of the requestor's window"
	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false
</details>

#### Encoder>>#bindAndJuggle: name

<details>
	<summary>See more</summary>
	
	bindAndJuggle: name

	| node nodes first thisCode |
	node := self reallyBind: name.

	"Declared temps must precede block temps for decompiler and debugger to work right"
	nodes := self tempNodes.
	(first := nodes findFirst: [:n | n scope > 0]) > 0 ifTrue:
		[node == nodes last ifFalse: [self error: 'logic error'].
		thisCode := (nodes at: first) code.
		first to: nodes size - 1 do:
			[:i | (nodes at: i) key: (nodes at: i) key
							code: (nodes at: i+1) code].
		nodes last key: nodes last key code: thisCode].
	
	^ node
</details>

#### Encoder>>#cantStoreInto: varName

<details>
	<summary>See more</summary>
	
	cantStoreInto: varName

	^StdVariables includesKey: varName
</details>

#### Encoder>>#litIndex: literal

<details>
	<summary>See more</summary>
	
	litIndex: literal
	| p |
	p := literalStream position.
	p = self maxNumLiterals ifTrue:
		[self notify: 'More than ', self maxNumLiterals printString, ' literals referenced. 
You must split or otherwise simplify this method.
The ', (self maxNumLiterals + 1) printString, 'th literal is: ', literal printString. ^nil].
	"Would like to show where it is in the source code, 
	 but that info is hard to get."
	literalStream nextPut: literal.
	^ p
</details>

#### Encoder>>#messageSendLastPositionsOf: aSelector ifAbsent: absentBlock

<details>
	<summary>See more</summary>
	
	messageSendLastPositionsOf: aSelector ifAbsent: absentBlock

	^self
		withMessageSendKeywordPositionsOf: aSelector 
		do: [ :aMessageSendNode | (sourceRanges at: aMessageSendNode) last ]
		ifAbsent: absentBlock
	
</details>

#### Encoder>>#bindBlockTemp: name

Declare a temporary block variable; complain if it's not a field or class variable.


<details>
	<summary>See more</summary>
	
	bindBlockTemp: name 
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |

	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^ self notify: 'Name already used in this method'].
			node scope: 0]
		ifFalse: [^self notify: 'Name already used in this class'].
	^node

</details>

#### Encoder>>#selector: aSymbol

<details>
	<summary>See more</summary>
	
	selector: aSymbol
	selector := aSymbol
</details>

#### Encoder>>#methodNodeClass

<details>
	<summary>See more</summary>
	
	methodNodeClass
	^MethodNode
</details>

#### Encoder>>#criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo

<details>
	<summary>See more</summary>
	
	criteriaToSortSourceRangeBetween: parseNodeWithSourceRangeOne and: parseNodeWithSourceRangeTwo

	| sourceRangeOne sourceRangeTwo |
	sourceRangeOne _ parseNodeWithSourceRangeOne value.
	sourceRangeTwo _ parseNodeWithSourceRangeTwo value.
	^ sourceRangeOne first > sourceRangeTwo first
		or: [ sourceRangeOne first = sourceRangeTwo first
			and: [ sourceRangeOne last <= sourceRangeTwo last ] ]
</details>

#### Encoder>>#lookupInPools: varName ifFound: assocBlock

<details>
	<summary>See more</summary>
	
	lookupInPools: varName ifFound: assocBlock

	^Symbol
		hasInterned: varName
		ifTrue:
			[:sym|
			(class bindingOf: sym)
				ifNil: [^false]
				ifNotNil: [:assoc| assocBlock value: assoc]]
</details>

#### Encoder>>#parameterDefinitionPositionFor: aParameterNode

<details>
	<summary>See more</summary>
	
	parameterDefinitionPositionFor: aParameterNode

	^ (self rangeForNode: aParameterNode ifAbsent: [ self error: 'invalid parameter node' ]) first
</details>

#### Encoder>>#noteSuper

<details>
	<summary>See more</summary>
	
	noteSuper

	supered := true
</details>

#### Encoder>>#bindArg: name

Declare an argument.


<details>
	<summary>See more</summary>
	
	bindArg: name 
	"Declare an argument."
	| node |
	nTemps >= 15
		ifTrue: [^self notify: 'Too many arguments'].
	node := self bindTemp: name.
	^ node nowHasDef nowHasRef
</details>

#### Encoder>>#sharableLitIndex: literal

Special access prevents multiple entries for post-allocated super send special selectors


<details>
	<summary>See more</summary>
	
	sharableLitIndex: literal
	"Special access prevents multiple entries for post-allocated super send special selectors"
	1 to: literalStream position do:
		[:index|
		(literal literalEqual: (literalStream originalContents at: index)) ifTrue:
			[^index - 1]].
	^self litIndex: literal
</details>

#### Encoder>>#interactive

Answer true if compilation is interactive


<details>
	<summary>See more</summary>
	
	interactive
	"Answer true if compilation is interactive"

	^requestor interactive
</details>

#### Encoder>>#bindBlockTemp: name within: aBlockNode

The BlockContext compiler (the Smalltalk-80 BlueBook compiler) does provide support for ANSI block syntax, but not for ANSI block semantics. Here all temps live at the same level, the method level. The approach taken to two block-local temps in different blocks is to merge them into a single temp. e.g. expr ifTrue: [|temp| self statementOne] ifFalse: [|temp| self statementTwo] is effectvely transformed into | temp | expr ifTrue: [self statementOne] ifFalse: [self statementTwo] and expr do: [:each| | temp | ...]. expr do: [:each| | temp | ...]. is also effectively transformed into | temp | expr do: [:each| ...]. expr do: [:each| ...]. The closure compiler treats the former similarly, but not the latter. The indirection through #bindBlockTemp:within: allows the closure encoder to do this.


<details>
	<summary>See more</summary>
	
	bindBlockTemp: name within: aBlockNode
	"The BlockContext compiler (the Smalltalk-80 BlueBook compiler)
	 does provide support for ANSI block syntax, but not for ANSI block
	 semantics.  Here all temps live at the same level, the method level.
	 The approach taken to two block-local temps in different blocks is to
	 merge them into a single temp.  e.g.
		expr
			ifTrue: [|temp| self statementOne]
			ifFalse: [|temp| self statementTwo]
	 is effectvely transformed into
		| temp |
		expr
			ifTrue: [self statementOne]
			ifFalse: [self statementTwo]
	 and
		expr do: [:each| | temp | ...].
		expr do: [:each| | temp | ...].
	 is also effectively transformed into
		| temp |
		expr do: [:each|  ...].
		expr do: [:each| ...].

	 The closure compiler treats the former similarly, but not the latter.
	 The indirection through #bindBlockTemp:within: allows the closure encoder to do this."
	^self bindBlockTemp: name
</details>

#### Encoder>>#reallyBind: name

<details>
	<summary>See more</summary>
	
	reallyBind: name

	| node |
	node := self newTemp: name.
	scopeTable at: name put: node.
	^node
</details>

#### Encoder>>#positionsForInstanceVariable: aName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	positionsForInstanceVariable: aName ifAbsent: aBlock

	^ self
		rangesForVariable: aName
		checkingType: [ :variableNode | variableNode isInstanceVariableNode ]
		ifAbsent: aBlock 
</details>

#### Encoder>>#encodeVariable: name

<details>
	<summary>See more</summary>
	
	encodeVariable: name
	^ self encodeVariable: name sourceRange: nil ifUnknown: [ self undeclared: name ]
</details>

#### Encoder>>#notify: string at: location

Create and schedule a Notifier with the argument as the message in order to request confirmation before a process can proceed. Subclasses can override this and insert an error message at location within aString.


<details>
	<summary>See more</summary>
	
	notify: string at: location

	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string at: location].
	^false
</details>

#### Encoder>>#accept: aVisitor

I am not really a ParseNode. Only here to access constants defined in parseNode.


<details>
	<summary>See more</summary>
	
	accept: aVisitor
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
</details>

#### Encoder>>#possibleVariablesFor: proposedVariable

<details>
	<summary>See more</summary>
	
	possibleVariablesFor: proposedVariable

	| results |
	results := proposedVariable correctAgainstDictionary: scopeTable
								continuedFrom: nil.
	proposedVariable first canBeGlobalVarInitial ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results.

</details>

#### Encoder>>#tempNames

<details>
	<summary>See more</summary>
	
	tempNames 

	^ self tempNodes collect:
		[:node | (node isMemberOf: MessageAsTempNode)
					ifTrue: [scopeTable keyAtValue: node]
					ifFalse: [node key]]
</details>

#### Encoder>>#bindUndeclaredTemp: name

<details>
	<summary>See more</summary>
	
	bindUndeclaredTemp: name
	^scopeTable at: name put: (self newUndeclaredTemp: name)
</details>

## EncoderForV3PlusClosures

I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book. 0-15 0000iiii Push Receiver Variable #iiii 16-31 0001iiii Push Temporary Location #iiii 32-63 001iiiii Push Literal Constant #iiiii 64-95 010iiiii Push Literal Variable #iiiii 96-103 01100iii Pop and Store Receiver Variable #iii 104-111 01101iii Pop and Store Temporary Location #iii 112-119 01110iii Push (receiver, true, false, nil, -1, 0, 1, 2) [iii] 120-123 011110ii Return (receiver, true, false, nil) [ii] From Message 124-125 0111110i Return Stack Top From (Message, Block) [i] (126-127 unassigned) 128 10000000 jjkkkkkk Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk 129 10000001 jjkkkkkk Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk 130 10000010 jjkkkkkk Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk 131 10000011 jjjkkkkk Send Literal Selector #kkkkk With jjj Arguments 132 10000100 iiijjjjj kkkkkkkk (Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs) 133 10000011 jjjkkkkk Send Literal Selector #kkkkk To Superclass With jjj Arguments 134 10000011 jjjkkkkk Send Literal Selector #kkkkk With jjj Arguments 135 10000111 Pop Stack Top 136 10001000 Duplicate Stack Top 137 10001001 Push Active Context (138-143 unassigned) 144-151 10010iii Jump iii + 1 (i.e., 1 through 8) 152-159 10011iii Pop and Jump 0n False iii +1 (i.e., 1 through 8) 160-167 10100iii jjjjjjjj Jump(iii - 4) *256+jjjjjjjj 168-171 101010ii jjjjjjjj Pop and Jump On True ii *256+jjjjjjjj 172-175 101011ii jjjjjjjj Pop and Jump On False ii *256+jjjjjjjj 176-191 1011iiii Send Arithmetic Message #iiii 192-207 1100iiii Send Special Message #iiii 208-223 1101iiii Send Literal Selector #iiii With No Arguments 224-239 1110iiii Send Literal Selector #iiii With 1 Argument 240-255 1111iiii Send Literal Selector #iiii With 2 Arguments An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation. 138 10001010 jkkkkkkk Push (Array new: kkkkkkk) (j = 0) or Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1) 139 10001011 kkkkkkkk jjjjjjjj Invoke primitive number jjjjjjjjkkkkkkkk 140 10001100 kkkkkkkk jjjjjjjj Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj 141 10001101 kkkkkkkk jjjjjjjj Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj 142 10001110 kkkkkkkk jjjjjjjj Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj 143 10001111 llllkkkk jjjjjjjj iiiiiiii Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii This is an exact duplicate of EncoderForLongFormV3PlusClosures. Could be a trait (or in Newspeak, a Mixin). For now we impose upon you to synchronise any and all changes between these two classes.

### Methods
#### EncoderForV3PlusClosures>>#genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize

143 10001111 llllkkkk jjjjjjjj iiiiiiii Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii


<details>
	<summary>See more</summary>
	
	genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize
	"143 	10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 15]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 15].
	(numArgs < 0 or: [numArgs > 15]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 15].
	stream
		nextPut: 143;
		nextPut: numArgs + (numCopied bitShift: 4);
		nextPut: (jumpSize bitShift: -8);
		nextPut: (jumpSize bitAnd: 16rFF)
</details>

#### EncoderForV3PlusClosures>>#genStoreTemp: tempIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStoreTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
</details>

#### EncoderForV3PlusClosures>>#genPushSpecialLiteral: aLiteral

112-119 01110iii Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]


<details>
	<summary>See more</summary>
	
	genPushSpecialLiteral: aLiteral
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	| index |
	index := #(true false nil -1 0 1 2) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil -1 0 1 2'].
	stream nextPut: index + 112
</details>

#### EncoderForV3PlusClosures>>#genPushNewArray: size

<details>
	<summary>See more</summary>
	
	genPushNewArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 0kkkkkkk 	Pop kkkkkkk into: (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size
</details>

#### EncoderForV3PlusClosures>>#genPushTemp: tempIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 16 ifTrue: 
		["16-31 	0001iiii 	Push Temporary Location #iiii"
		 stream nextPut: 16 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
</details>

#### EncoderForV3PlusClosures>>#genPushConsArray: size

<details>
	<summary>See more</summary>
	
	genPushConsArray: size
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 1kkkkkkk 	Push (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size + 128
</details>

#### EncoderForV3PlusClosures>>#genSendSuper: selectorLiteralIndex numArgs: nArgs

See BlueBook page 596 (with exceptions for 132 & 134)


<details>
	<summary>See more</summary>
	
	genSendSuper: selectorLiteralIndex numArgs: nArgs
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"
		 stream
			nextPut: 133;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: 32 + nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
</details>

#### EncoderForV3PlusClosures>>#genReturnSpecialLiteral: aLiteral

120-123 011110ii Return (receiver, true, false, nil) [ii] From Message


<details>
	<summary>See more</summary>
	
	genReturnSpecialLiteral: aLiteral
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 120 + index
</details>

#### EncoderForV3PlusClosures>>#genStoreLiteralVar: literalIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStoreLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genPop

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPop
	"See BlueBook page 596"
	"135 	10000111 	Pop Stack Top"
	stream nextPut: 135
</details>

#### EncoderForV3PlusClosures>>#hasLocalNamed: aName

<details>
	<summary>See more</summary>
	
	hasLocalNamed: aName

	^ scopeTable includesKey: aName 
</details>

#### EncoderForV3PlusClosures>>#genStorePopTemp: tempIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStorePopTemp: tempIndex
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue: 
		["104-111 	01101iii 	Pop and Store Temporary Location #iii"
		 stream nextPut: 104 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
</details>

#### EncoderForV3PlusClosures>>#genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex

141 10001101 kkkkkkkk jjjjjjjj Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj


<details>
	<summary>See more</summary>
	
	genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"141 	10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 141;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
</details>

#### EncoderForV3PlusClosures>>#genReturnTopToCaller

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genReturnTopToCaller
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 125
</details>

#### EncoderForV3PlusClosures>>#genBranchPopFalse: distance

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genBranchPopFalse: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	(distance > 0 and: [distance < 9]) ifTrue:
		["152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8)"
		 stream nextPut: 152 + distance - 1.
		 ^self].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
</details>

#### EncoderForV3PlusClosures>>#genStorePopLiteralVar: literalIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStorePopLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 self genPop.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genPushThisContext

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushThisContext
	"See BlueBook page 596"
	"137 	10001001 	Push Active Context"
	stream nextPut: 137
</details>

#### EncoderForV3PlusClosures>>#maxIndexableLiterals

This bytecode set can index up to 256 literals.


<details>
	<summary>See more</summary>
	
	maxIndexableLiterals
	"This bytecode set can index up to 256 literals."
	^256
</details>

#### EncoderForV3PlusClosures>>#genPushReceiver

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushReceiver
	"See BlueBook page 596"
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	stream nextPut: 112
</details>

#### EncoderForV3PlusClosures>>#genSendSpecial: specialSelectorIndex numArgs: nArgs

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genSendSpecial: specialSelectorIndex numArgs: nArgs
	"See BlueBook page 596"
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		176-191 	1011iiii 	Send Arithmetic Message #iiii
		192-207 	1100iiii 	Send Special Message #iiii"
	stream nextPut: specialSelectorIndex + 175
</details>

#### EncoderForV3PlusClosures>>#genPushNClosureTemps: numTemps

<details>
	<summary>See more</summary>
	
	genPushNClosureTemps: numTemps
	numTemps timesRepeat: [self genPushSpecialLiteral: nil]
</details>

#### EncoderForV3PlusClosures>>#genStorePopInstVarLong: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStorePopInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 192;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genReturnTop

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genReturnTop
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 124
</details>

#### EncoderForV3PlusClosures>>#genStoreInstVarLong: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStoreInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 160;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genStorePopInstVar: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStorePopInstVar: instVarIndex
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 8 ifTrue:
			["96-103 	01100iii 	Pop and Store Receiver Variable #iii"
			 stream nextPut: 96 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 130;
				nextPut: instVarIndex.
			 ^self]].
	self genStorePopInstVarLong: instVarIndex
</details>

#### EncoderForV3PlusClosures>>#genCallPrimitive: primitiveIndex

Only for Spur!


<details>
	<summary>See more</summary>
	
	genCallPrimitive: primitiveIndex
	"Only for Spur!"
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue:
		[self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)
</details>

#### EncoderForV3PlusClosures>>#genSend: selectorLiteralIndex numArgs: nArgs

See BlueBook page 596 (with exceptions for 132 & 134)


<details>
	<summary>See more</summary>
	
	genSend: selectorLiteralIndex numArgs: nArgs
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		["Special selector sends.
			176-191 	1011iiii 	Send Arithmetic Message #iiii
			192-207 	1100iiii 	Send Special Message #iiii"
		self flag: #yuck.
		 (selectorLiteralIndex negated between: 176 and: 207) ifFalse:
			[^self outOfRangeError: 'special selector code' index: selectorLiteralIndex negated range: 176 to: 207].
		 stream nextPut: selectorLiteralIndex negated.
		 ^self].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue:
		["	208-223 	1101iiii 	Send Literal Selector #iiii With No Arguments
			224-239 	1110iiii 	Send Literal Selector #iiii With 1 Argument
			240-255 	1111iiii 	Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 208 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments"
		 stream
			nextPut: 131;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 64 and: [nArgs < 4]) ifTrue: 
	 	["In Squeak V3
			134 	10000110 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk To Superclass With jjjjjjjj Arguments
		 is replaced by
			134 	10000110 jjkkkkkk 	Send Literal Selector #kkkkkk With jj Arguments"
		 stream
			nextPut: 134;
			nextPut: ((nArgs bitShift: 6) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
</details>

#### EncoderForV3PlusClosures>>#genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex

<details>
	<summary>See more</summary>
	
	genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		["140 	10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
		 stream
			nextPut: 140;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
</details>

#### EncoderForV3PlusClosures>>#genJump: distance

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genJump: distance
	"See BlueBook page 596"
	(distance > 0 and: [distance < 9]) ifTrue:
		["144-151 	10010iii 	Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 144 + distance - 1.
		 ^self].
	"160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	^self genJumpLong: distance
</details>

#### EncoderForV3PlusClosures>>#genDup

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genDup
	"See BlueBook page 596"
	"136 	10001000 	Duplicate Stack Top"
	stream nextPut: 136
</details>

#### EncoderForV3PlusClosures>>#genPushInstVar: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushInstVar: instVarIndex
	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 16 ifTrue:
			["0-15 	0000iiii 	Push Receiver Variable #iiii"
			 stream nextPut: 0 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 128;
				nextPut: instVarIndex.
			 ^self]].
	self genPushInstVarLong: instVarIndex
</details>

#### EncoderForV3PlusClosures>>#genJumpLong: distance

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genJumpLong: distance
	"See BlueBook page 596"
	(distance >= -1024 and: [distance < 1024]) ifTrue:
		["160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
		 stream
			nextPut: 160 + (distance + 1024 bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: -1024 to: 1023
</details>

#### EncoderForV3PlusClosures>>#genPushTempLong: tempIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushTempLong: tempIndex
	"See BlueBook page 596"
	(tempIndex >= 0 and: [tempIndex < 64]) ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
</details>

#### EncoderForV3PlusClosures>>#genBranchPopTrue: distance

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genBranchPopTrue: distance
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	distance < 1024 ifTrue:
		["168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj"
		 stream
			nextPut: 168 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
</details>

#### EncoderForV3PlusClosures>>#genReturnReceiver

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genReturnReceiver
	"See BlueBook page 596"
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	stream nextPut: 120
</details>

#### EncoderForV3PlusClosures>>#supportsClosureOpcodes

Answer if the receiver supports the genPushNewArray:/genPushConsArray: genPushRemoteTemp:inVectorAt: genStoreRemoteTemp:inVectorAt: genStorePopRemoteTemp:inVectorAt: genPushClosureCopyCopiedValues:numArgs:jumpSize: opcodes


<details>
	<summary>See more</summary>
	
	supportsClosureOpcodes
	^true
</details>

#### EncoderForV3PlusClosures>>#genPushInstVarLong: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushInstVarLong: instVarIndex
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 64;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genPushLiteralVar: literalIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushLiteralVar: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["64-95 	010iiiii 	Push Literal Variable #iiiii"
		 stream nextPut: 64 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 128;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
</details>

#### EncoderForV3PlusClosures>>#genStoreInstVar: instVarIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genStoreInstVar: instVarIndex
	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex
</details>

#### EncoderForV3PlusClosures>>#genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex

142 10001110 kkkkkkkk jjjjjjjj Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj


<details>
	<summary>See more</summary>
	
	genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex
	"142 	10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 142;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
</details>

#### EncoderForV3PlusClosures>>#genPushLiteral: literalIndex

See BlueBook page 596


<details>
	<summary>See more</summary>
	
	genPushLiteral: literalIndex
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal Constant #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 128 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 96;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
</details>

## Parser

I parse Smalltalk syntax and create a MethodNode that is the root of the parse tree. I look one token ahead. See http://www.whysmalltalk.com/articles/bykov/HitchHiker.htm

### Methods
#### Parser>>#rangesForRemovableUnusedTempsOf: aMethodNode

<details>
	<summary>See more</summary>
	
	rangesForRemovableUnusedTempsOf: aMethodNode
	
	| tempsToRemove |
	
	tempsToRemove := SortedCollection sortBlock: [ :leftRange :rightRange | leftRange first < rightRange first ].
	
	tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: aMethodNode). 
	aMethodNode nodesDo: [ :node |
		node isBlockNode ifTrue: [
			tempsToRemove addAll: (self rangesForRemovableUnusedTempsInBlockNode: node) ]].
	
	^tempsToRemove
</details>

#### Parser>>#bindTemp: name

<details>
	<summary>See more</summary>
	
	bindTemp: name

	^name
</details>

#### Parser>>#insertWord: anInsertion at: aPosition

<details>
	<summary>See more</summary>
	
	insertWord: anInsertion at: aPosition
	
	^self substituteWord: anInsertion
		wordInterval: (aPosition to: aPosition - 1)
		offset: 0.
</details>

#### Parser>>#noTempsDeclaration

<details>
	<summary>See more</summary>
	
	noTempsDeclaration

	^ TemporariesDeclarationNode empty
</details>

#### Parser>>#substituteWord: correctWord wordInterval: spot offset: o

Substitute the correctSelector into the (presumed interactive) receiver. Update requestorOffset based on the delta size and answer the updated offset.


<details>
	<summary>See more</summary>
	
	substituteWord: correctWord wordInterval: spot offset: o
	"Substitute the correctSelector into the (presumed interactive) receiver.
	 Update requestorOffset based on the delta size and answer the updated offset."

	requestor correctFrom: spot first + o to: spot last + o with: correctWord.
	requestorOffset := requestorOffset + correctWord size - spot size.
	^o + correctWord size - spot size
</details>

#### Parser>>#primitive: aNameString module: aModuleStringOrNil

Create named primitive.


<details>
	<summary>See more</summary>
	
	primitive: aNameString module: aModuleStringOrNil
	"Create named primitive."
	
	^self primitive: aNameString module: aModuleStringOrNil error: nil
</details>

#### Parser>>#performInteractiveChecks: aMethodNode

<details>
	<summary>See more</summary>
	
	performInteractiveChecks: aMethodNode

	self
		declareUndeclaredTemps: aMethodNode;
		removeUnusedTempsOf: aMethodNode 
</details>

#### Parser>>#substituteSelector: selectorParts wordIntervals: spots

Substitute the correctSelector into the (presuamed interactive) receiver.


<details>
	<summary>See more</summary>
	
	substituteSelector: selectorParts wordIntervals: spots
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset := 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset := self substituteWord: word wordInterval: interval offset: offset ]

</details>

#### Parser>>#cascade

{; message} => CascadeNode.


<details>
	<summary>See more</summary>
	
	cascade
	" {; message} => CascadeNode."

	| receiverNode messageNodes |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	receiverNode := parseNode cascadeReceiver.
	messageNodes := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := receiverNode.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			messageNodes addLast: parseNode].
	self createCascadeNodeWith: receiverNode and: messageNodes
</details>

#### Parser>>#declareClassVar: name

<details>
	<summary>See more</summary>
	
	declareClassVar: name
	| sym class |
	sym := name asSymbol.
	class := encoder classEncoding.
	class := class theNonMetaClass.		"not the metaclass"
	class addClassVarName: name.
	"Not needed in Cuis"
	"Smalltalk logChange: class definition."
	^ encoder global: (class classPool associationAt: sym)
			name: sym
</details>

#### Parser>>#blockExpression

[ ({:var} |) (| {temps} |) (statements) ] => BlockNode.


<details>
	<summary>See more</summary>
	
	blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode tempsDeclarationNode variableNodes temporaryBlockVariables start |
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[self argumentNameWithRangeDo: [ :argumentName :range |
			variableNodes addLast: (encoder bindBlockArg: argumentName within: blockNode range: range)]].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	tempsDeclarationNode := self temporaryBlockVariablesFor: blockNode.
	temporaryBlockVariables := tempsDeclarationNode allDeclaredVariableNodes.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporariesDeclaration: tempsDeclarationNode.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]
</details>

#### Parser>>#expression

<details>
	<summary>See more</summary>
	
	expression

	(hereType == #word and: [tokenType == #leftArrow])
		ifTrue: [^ self assignment: self variable].
	hereType == #leftBrace
		ifTrue: [self braceExpression]
		ifFalse: [self primaryExpression ifFalse: [^ false]].
	(self messagePart: 3 repeat: true) ifTrue:
		[
			hereType == #semicolon ifTrue: [self cascade].
			hereType == #colon ifTrue: [self chain]
		].
	^ true
</details>

#### Parser>>#substituteVariable: each atInterval: anInterval

<details>
	<summary>See more</summary>
	
	substituteVariable: each atInterval: anInterval 
	self 
		substituteWord: each
		wordInterval: anInterval
		offset: 0.
	^encoder encodeVariable: each
</details>

#### Parser>>#declareTemp: name at: levelTag

Defer declaring the temp until the parse has completed. This allows the parser to declare the temp in the minimum enclosing block instead of always at method level. See Parser>>declareUndeclaredTemps:


<details>
	<summary>See more</summary>
	
	declareTemp: name at: levelTag
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^(encoder bindUndeclaredTemp: name)
		tag: levelTag;
		yourself
</details>

#### Parser>>#transformVerticalBarAndUpArrowIntoABinarySelector

Transform a vertical bar and or a up arrow into a binary selector. Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector. Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs: - either as an empty temporaries specification, - or as a local temporaries specification in a block of arity > 0. Colon $: can be used as binary, but '::' means Chain.


<details>
	<summary>See more</summary>
	
	transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Colon $: can be used as binary, but '::' means Chain."
	| toMakeBinary |
	toMakeBinary _ #(verticalBar upArrow colon).
	"Special case: '::' is not a binary selector but the Chain operator"
	(hereType = #colon and: [tokenType = #colon]) ifTrue: [^ self ].
	(toMakeBinary identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asString.
	hereType := #binary.
	[(toMakeBinary identityIncludes: tokenType) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asString.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].
</details>

#### Parser>>#init: sourceStream notifying: req failBlock: aBlock

<details>
	<summary>See more</summary>
	
	init: sourceStream notifying: req failBlock: aBlock

	requestor := req.
	failBlock := aBlock.
	requestorOffset := 0.
	"Hey, don't call super with a different selector!"
	self scan: sourceStream.
	prevMark := hereMark := mark.
	self advance
</details>

#### Parser>>#primitive: anIntegerOrString

Create indexed primitive.


<details>
	<summary>See more</summary>
	
	primitive: anIntegerOrString
	"Create indexed primitive."
	
	^self primitive: anIntegerOrString error: nil
</details>

#### Parser>>#createEmptyTempsDeclarationIfNecessaryIn: aBlockNode

Return the new tempsMark for this BlockNode


<details>
	<summary>See more</summary>
	
	createEmptyTempsDeclarationIfNecessaryIn: aBlockNode
	"Return the new tempsMark for this BlockNode"
	| blockTempsMark |
	
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	(self hasNoTempDeclarationPipes: aBlockNode) ifTrue: [ 
		blockTempsMark := self createEmptyTempsDeclarationAfter: blockTempsMark ].
	^blockTempsMark
</details>

#### Parser>>#binaryPattern

<details>
	<summary>See more</summary>
	
	binaryPattern

	^ self advanceWithRangeDo: [ :selectorAsString :selectorRange | | arguments |
		self argumentNameWithRangeDo: [ :argumentName :argumentRange |
		arguments _ Array with: (encoder bindArg: argumentName range: argumentRange).
		{selectorAsString asSymbol. arguments. 2. {selectorRange}}]]
</details>

#### Parser>>#filterSeparatorsToTheLeftOn: currentSource startingAt: anInitialPosition

<details>
	<summary>See more</summary>
	
	filterSeparatorsToTheLeftOn: currentSource startingAt: anInitialPosition

	| start |
	
	start := anInitialPosition. 
	[ (currentSource at: start-1) isSeparator ] whileTrue: [ start := start - 1 ].
	
	^start 
	 	
</details>

#### Parser>>#pragmaSequence

Parse a sequence of method pragmas.


<details>
	<summary>See more</summary>
	
	pragmaSequence
	"Parse a sequence of method pragmas."
	
	[ true ] whileTrue: [
		(self matchToken: #<)
			ifFalse: [ ^ self ].
		self pragmaStatement.
		(self matchToken: #>)
			ifFalse: [ ^ self expected: '>' ] ]
</details>

#### Parser>>#addPragma: aPragma

<details>
	<summary>See more</summary>
	
	addPragma: aPragma
	properties := self properties copyWith: aPragma
</details>

#### Parser>>#temporaryBlockVariablesFor: aBlockNode

Scan and answer temporary block variables.


<details>
	<summary>See more</summary>
	
	temporaryBlockVariablesFor: aBlockNode
	"Scan and answer temporary block variables."

	| tempDeclarationNodes declarationStartIndex |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[aBlockNode tempsMark: prevMark + requestorOffset.
		 ^ self noTempsDeclaration ].

	tempDeclarationNodes _ OrderedCollection new.
	declarationStartIndex _ prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode _ encoder bindBlockTemp: variableName within: aBlockNode range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifFalse:
		[^self expected: 'Vertical bar'].
	aBlockNode tempsMark: prevMark + requestorOffset.
	^ self
		createTempsDeclarationWith: tempDeclarationNodes
		sourceRange: (declarationStartIndex to: prevMark)
</details>

#### Parser>>#pasteTemp: name before: aTempsMark

Return the new position of the tempsMark.


<details>
	<summary>See more</summary>
	
	pasteTemp: name before: aTempsMark
	"Return the new position of the tempsMark."
	
	| insertion theTextString  characterBeforeMark offset |
	
	theTextString := requestor text string.
	insertion := name, ' '.
	characterBeforeMark := theTextString at: aTempsMark - 1 ifAbsent: [$ ].
	characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
	offset := self insertWord: insertion at: aTempsMark.
	
	^aTempsMark + offset.
</details>

#### Parser>>#properties

<details>
	<summary>See more</summary>
	
	properties
	^ properties ifNil: [ properties := AdditionalMethodState new ]
</details>

#### Parser>>#advance

<details>
	<summary>See more</summary>
	
	advance
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := self compensateTwoCharacterLookahead.
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this
</details>

#### Parser>>#endOfLastToken

<details>
	<summary>See more</summary>
	
	endOfLastToken

	^ prevEnd ifNil: [mark]
</details>

#### Parser>>#pragmaPrimitives

<details>
	<summary>See more</summary>
	
	pragmaPrimitives
	| primitives |
	self properties isEmpty ifTrue:
		[^0].
	primitives := properties pragmas select:
					[:pragma|
					self class primitivePragmaSelectors includes: pragma keyword].
	primitives isEmpty ifTrue:
		[^0].
	primitives size > 1 ifTrue:
		[^self notify: 'Ambigous primitives'].
	^self perform: primitives first keyword withArguments: primitives first arguments
</details>

#### Parser>>#temporaries

[ '|' (variable)* '|' ]


<details>
	<summary>See more</summary>
	
	temporaries
	" [ '|' (variable)* '|' ]"
	| tempDeclarationNodes theActualText declarationStartIndex |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue:
			[tempsMark := self interactive
								ifTrue: [requestor selectionInterval first]
								ifFalse: [1].
			^ self noTempsDeclaration ].
		tempsMark := hereMark	"formerly --> prevMark + prevToken".
		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ self noTempsDeclaration ].
	tempDeclarationNodes _ OrderedCollection new.
	declarationStartIndex _ prevMark.
	[hereType == #word] whileTrue: [
		self advanceWithRangeDo: [ :variableName :range |
			| variableNode |
			variableNode _ encoder bindTemp: variableName range: range.
			tempDeclarationNodes addLast: (self createTempDeclarationOf: variableNode sourceRange: range) ] ].
	(self match: #verticalBar) ifTrue: [
		tempsMark := prevMark.
		^ self
			createTempsDeclarationWith: tempDeclarationNodes
			sourceRange: (declarationStartIndex to: prevMark) ].
	^ self expected: 'Vertical bar'
</details>

#### Parser>>#expected: aString

Notify a problem at token 'here'.


<details>
	<summary>See more</summary>
	
	expected: aString 
	"Notify a problem at token 'here'."

	tokenType == #doIt ifTrue: [hereMark := hereMark + 1].
	hereType == #doIt ifTrue: [hereMark := hereMark + 1].
	^ self notify: aString , ' expected' at: hereMark + requestorOffset
</details>

#### Parser>>#declareTempAndPaste: name

Defer declaring the temp until the parse has completed. This allows the parser to declare the temp in the minimum enclosing block instead of always at method level. See Parser>>declareUndeclaredTemps:


<details>
	<summary>See more</summary>
	
	declareTempAndPaste: name
	"Defer declaring the temp until the parse has completed.  This allows
	 the parser to declare the temp in the minimum enclosing block instead
	 of always at method level.  See Parser>>declareUndeclaredTemps:"
	^encoder bindUndeclaredTemp: name
</details>

#### Parser>>#addComment

<details>
	<summary>See more</summary>
	
	addComment

	parseNode ~~ nil
		ifTrue: 
			[parseNode comment: currentComment.
			currentComment := nil]
</details>

#### Parser>>#bindArg: aName range: aRange

<details>
	<summary>See more</summary>
	
	bindArg: aName range: aRange

	^ self bindArg: aName
</details>

#### Parser>>#primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil

Create named primitive with optional error code.


<details>
	<summary>See more</summary>
	
	primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117
</details>

#### Parser>>#keywordPattern

<details>
	<summary>See more</summary>
	
	keywordPattern

	| keywordRanges selector arguments |
	
	selector := WriteStream on: (String new: 32).
	arguments := OrderedCollection new.
	keywordRanges := OrderedCollection new.
	
	[hereType == #keyword] whileTrue:[ 
		self addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments ].
		
	^ {selector contents asSymbol. arguments. 3. keywordRanges}
		
</details>

#### Parser>>#variable

<details>
	<summary>See more</summary>
	
	variable
	
	^self advanceWithRangeDo: [ :variableName :range | | varName result rightRange |
		varName := variableName.
		
		"See ParserTest>>#testRangesAreOkWhenReturningAVariableWithoutSpaceAfterThat
		There is a very difficult bug to fix. It happends when the source code ends with a return of a variable of 
		one char an no other char after that, for example: '^a' 
		In that case the range includes the ^ that is incorrect and makes the rename temporary fail.
		I do this fix becuase changing how the range is calculated is almost imposible due to the coupling 
		and complexity of the parser. This change applies only to variables and therefore it assures no
		unexpected behavior. I'm not cheching for size = 1 because it is redundant - Hernan"
		rightRange := varName size = range size
			ifTrue: [ range ]
			ifFalse: [ range last - varName size + 1 to: range last ].
		
		[result _ encoder encodeVariable: varName sourceRange: rightRange ifUnknown: [ nil ].
		result ifNil: [
			result _ (UndeclaredVariableReference new)
				parser: self;
				varName: varName;
				varStart: rightRange first;
				varEnd: rightRange last;
				signal ].
		result isString ] whileTrue: [ varName _ result].
		encoder addMultiRange: rightRange for: result ].
	
</details>

#### Parser>>#encoder

<details>
	<summary>See more</summary>
	
	encoder
	encoder ifNil:
		[encoder := EncoderForV3PlusClosures new].
	^encoder
</details>

#### Parser>>#unusedTempsOf: aNodeWithTemporaries

<details>
	<summary>See more</summary>
	
	unusedTempsOf: aNodeWithTemporaries

	^aNodeWithTemporaries temporaries select: [ :temporaryNode | temporaryNode isUnusedTemp ]
</details>

#### Parser>>#pragmaStatement

Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives.


<details>
	<summary>See more</summary>
	
	pragmaStatement
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	" This is a ugly hack into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last = $: or: [ selector first isLetter not ])
			ifTrue: [ arguments add: (self pragmaLiteral: selector) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true
</details>

#### Parser>>#parse: sourceStreamOrString class: behavior

<details>
	<summary>See more</summary>
	
	parse: sourceStreamOrString class: behavior

	^ self parse: sourceStreamOrString readStream class: behavior
		noPattern: false context: nil notifying: nil ifFail: [^nil]
</details>

#### Parser>>#externalFunctionDeclaration

Parse the function declaration for a call to an external library.


<details>
	<summary>See more</summary>
	
	externalFunctionDeclaration
	"Parse the function declaration for a call to an external library."
	| descriptorClass callType modifier retType externalName args argType module fn |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil ifTrue:[^self expected:'return type'].
	"Parse function name or index"
	externalName := here.
	(self match: #string) 
		ifTrue:[externalName := externalName asSymbol]
		ifFalse:[(self match:#number) ifFalse:[^self expected:'function name or index']].
	(self match: #leftParenthesis) ifFalse:[^self expected:'argument list'].
	args := WriteStream on: Array new.
	[self match: #rightParenthesis] whileFalse:[
		argType := self externalType: descriptorClass.
		argType == nil ifTrue:[^self expected:'argument'].
		argType isVoid & argType isPointerType not ifFalse:[args nextPut: argType]].
	(self matchToken: 'module:') ifTrue:[
		module := here.
		(self match: #string) ifFalse:[^self expected: 'String'].
		module := module asSymbol].
	Smalltalk at: #ExternalLibraryFunction ifPresent:[:xfn|
		fn := xfn name: externalName 
				module: module 
				callType: callType
				returnType: retType
				argumentTypes: args contents.
		self allocateLiteral: fn].
	(self matchToken: 'error:')
		ifTrue:
			[| errorCodeVariable |
			 errorCodeVariable := here.
			(hereType == #string
			 or: [hereType == #word]) ifFalse:[^self expected: 'error code (a variable or string)'].
			 self advance.
			 self addPragma: (Pragma keyword: #primitive:error: arguments: (Array with: 120 with: errorCodeVariable)).
			 fn ifNotNil: [fn setErrorCodeName: errorCodeVariable]]
		ifFalse:
			[self addPragma: (Pragma keyword: #primitive: arguments: #(120))].
	^true
</details>

#### Parser>>#unaryPattern

<details>
	<summary>See more</summary>
	
	unaryPattern

	^ self advanceWithRangeDo: [ :sel :range | {sel asSymbol. {}. 1. {range}} ]
</details>

#### Parser>>#hasNoTempDeclarationPipes: aBlockNode

Determine if a BlockNode already has the '| |' used to declare temps.


<details>
	<summary>See more</summary>
	
	hasNoTempDeclarationPipes: aBlockNode
	"Determine if a BlockNode already has the '| |' used to declare temps."
	| blockTempsMark sourceCode hasNoTemps |
	
	sourceCode := requestor text string.
	blockTempsMark := aBlockNode tempsMark + requestorOffset.
	hasNoTemps := aBlockNode temporaries isEmpty.
	^hasNoTemps and: [ (self isLastPipeOfEmptyTempsDeclaration: blockTempsMark) not ].
</details>

#### Parser>>#isLastPipeOfEmptyTempsDeclaration: pipeIndex

<details>
	<summary>See more</summary>
	
	isLastPipeOfEmptyTempsDeclaration: pipeIndex

	| indexOfPreviousPipe sourceCode |
	
	sourceCode := requestor text string.
	indexOfPreviousPipe := sourceCode lastIndexOf: $| startingAt: pipeIndex - 1 endingAt: 1 ifAbsent: [ ^false ].
	^(sourceCode at: pipeIndex) = $| and: [
		(sourceCode copyFrom: indexOfPreviousPipe + 1 to: pipeIndex - 1) allSatisfy: [ :char | char isSeparator ]]
</details>

#### Parser>>#declareGlobal: name

<details>
	<summary>See more</summary>
	
	declareGlobal: name
	| sym |
	sym _ name asSymbol.
	^encoder
		global: (Smalltalk
			at: sym put: nil;
			associationAt: sym)
		name: sym
</details>

#### Parser>>#defineClass: className

prompts the user to define a new class, asks for it's category, and lets the users edit further the definition


<details>
	<summary>See more</summary>
	
	defineClass: className
	"prompts the user to define a new class,  
	asks for it's category, and lets the users edit further  
	the definition"
	| sym cat def d2 |
	sym := className asSymbol.
	cat := FillInTheBlankMorph request: 'Enter class category : ' initialAnswer: self encoder classEncoding theNonMetaClass category.
	cat
		ifEmpty: [cat := 'Unknown'].
	def := 'Object subclass: #' , sym , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , cat , ''''.
	d2 := FillInTheBlankMorph request: 'Edit class definition : ' initialAnswer: def.
	d2
		ifEmpty: [d2 := def].
	Compiler evaluate: d2.
	^ encoder
		global: (Smalltalk associationAt: sym)
		name: sym
</details>

#### Parser>>#externalType: descriptorClass

Parse an return an external type


<details>
	<summary>See more</summary>
	
	externalType: descriptorClass
	"Parse an return an external type"
	| xType |
	xType := descriptorClass atomicTypeNamed: here.
	xType == nil ifTrue:["Look up from class scope"
		Symbol hasInterned: here ifTrue:[:sym|
			xType := descriptorClass structTypeNamed: sym]].
	xType == nil ifTrue:[
		"Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	(self matchToken:#*)
		ifTrue:[^xType asPointerType]
		ifFalse:[^xType]
</details>

#### Parser>>#newRangeRemovingTempsDeclarationFrom: currentSource 
	startingAt: start
	to: originalEnd

<details>
	<summary>See more</summary>
	
	newRangeRemovingTempsDeclarationFrom: currentSource 
	startingAt: start
	to: originalEnd
	
	| end |
	
	end := self filterSeparatorsToTheRightOn: currentSource startingAt: originalEnd.
	
	(currentSource at: end+1) = $| ifTrue: [ | possibleNewStart |
		possibleNewStart := (self filterSeparatorsToTheLeftOn: currentSource startingAt: start) - 1.
		(currentSource at: possibleNewStart) = $| ifTrue: [ ^Array with: possibleNewStart with: end + 1 ]]. 
	
	^Array with: start with: end

</details>

#### Parser>>#bindArg: name

<details>
	<summary>See more</summary>
	
	bindArg: name

	^ self bindTemp: name
</details>

#### Parser>>#startOfNextToken

Return starting position in source of next token.


<details>
	<summary>See more</summary>
	
	startOfNextToken
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark
</details>

#### Parser>>#matchToken: thing

Matches the token, not its type.


<details>
	<summary>See more</summary>
	
	matchToken: thing 
	"Matches the token, not its type."

	here = thing ifTrue: [self advance. ^true].
	^false
</details>

#### Parser>>#interactive

Answer true if compilation is interactive


<details>
	<summary>See more</summary>
	
	interactive
	"Answer true if compilation is interactive"

	^ requestor notNil
</details>

#### Parser>>#primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil

Create named primitive with optional error code.


<details>
	<summary>See more</summary>
	
	primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil
	"Create named primitive with optional error code."
	
	^self primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
</details>

#### Parser>>#method: noPattern doIt: doIt context: ctxt

pattern [ | temporaries ] block => MethodNode.


<details>
	<summary>See more</summary>
	
	method: noPattern doIt: doIt context: ctxt 
	" pattern [ | temporaries ] block => MethodNode."

	| sap block primitives tempsDeclarationNode messageComment methodNode |
	
	sap := self pattern: noPattern inContext: ctxt.
	"sap={selector, arguments, precedence}"
	self properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	
	doIt ifFalse: [self pragmaSequence].
	tempsDeclarationNode := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doIt ifFalse: [self pragmaSequence].
	primitives := self pragmaPrimitives.
	
	self statements: #() innerBlock: doIt.
	block := parseNode.
	doIt 
		ifTrue: [block returnLast]
		ifFalse: [block returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	
	methodNode := self newMethodNode comment: messageComment.
	methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporariesDeclaration: tempsDeclarationNode
		block: block
		encoder: encoder
		primitive: primitives
		properties: properties
		selectorKeywordsRanges: (sap at: 4).
		
	self interactive ifTrue: [ self performInteractiveChecks: methodNode ].
	
	^methodNode
</details>

#### Parser>>#argumentName

<details>
	<summary>See more</summary>
	
	argumentName

	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance
</details>

#### Parser>>#queryUndefined

<details>
	<summary>See more</summary>
	
	queryUndefined
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	requestor selectFrom: varStart to: varStart + varName size - 1.
	(UndefinedVariable name: varName) ifFalse: [^ self fail]
</details>

#### Parser>>#declareInstVar: name

Declare an instance variable. Since the variable will get added after any existing inst vars its index is the instSize.


<details>
	<summary>See more</summary>
	
	declareInstVar: name
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarName: name.
	"Not needed in Cuis"
	"Smalltalk logChange: encoder classEncoding definition."
	^InstanceVariableNode new name: name index: encoder classEncoding instSize
		
</details>

#### Parser>>#advanceWithRangeDo: aBlock

<details>
	<summary>See more</summary>
	
	advanceWithRangeDo: aBlock

	| lexema start end |
	
	start := self startOfNextToken + requestorOffset.
	lexema := self advance.
	end _ self endOfLastToken + requestorOffset.
	
	^ aBlock value: lexema value: (start to: end)
</details>

#### Parser>>#removeUnusedTempRange: aRangeToRemove with: delta

<details>
	<summary>See more</summary>
	
	removeUnusedTempRange: aRangeToRemove with: delta

	| currentSource start end newRangeToRemove | 

	currentSource := requestor text. 
	newRangeToRemove := self 
		newRangeRemovingTempsDeclarationFrom: currentSource 
		startingAt: aRangeToRemove first - delta 
		to: aRangeToRemove last - delta.
	start := newRangeToRemove first.
	end := newRangeToRemove last.
		
	requestor correctFrom: start to: end with: ''.
	
	^delta + end - start + 1

</details>

#### Parser>>#chain

{: message} => more messages


<details>
	<summary>See more</summary>
	
	chain
	" {: message} => more messages"

	self advance.

	"The following two lines implement double colon chains"
	hereType == #colon ifFalse:[self expected: 'Chain'].
	self advance.

	(self messagePart: 3 repeat: true) ifFalse: [self expected: 'Chain'].
	hereType == #semicolon ifTrue: [self cascade].
	hereType == #colon ifTrue: [self chain]
</details>

#### Parser>>#parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

Answer a MethodNode for the argument, sourceStream, that is the root of a parse tree. Parsing is done with respect to the argument, class, to find instance, class, and pool variables; and with respect to the argument, ctxt, to find temporary variables. Errors in parsing are reported to the argument, req, if not nil; otherwise aBlock is evaluated. The argument noPattern is a Boolean that is true if the the sourceStream does not contain a method header (i.e., for DoIts).


<details>
	<summary>See more</summary>
	
	parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."
	
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category _ aCategory.
	myStream _ sourceStream.
	[
		repeatNeeded _ false.
		startPosition _ myStream position.
		sourceCode _ myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag _ noPattern.
		failBlock _ aBlock.
		[ methNode _ self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded _ true.
				myStream _ ReadStream on: requestor text string ].
		repeatNeeded
	] whileTrue: [ encoder _ self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode
</details>

#### Parser>>#offEnd: aString

Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!


<details>
	<summary>See more</summary>
	
	offEnd: aString 
	"Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!"

	requestorOffset == nil
		ifTrue: [^ self notify: aString at: mark]
		ifFalse: [^ self notify: aString at: mark + requestorOffset]

</details>

#### Parser>>#canDeclareClassVariable

<details>
	<summary>See more</summary>
	
	canDeclareClassVariable
	^encoder classEncoding ~~ UndefinedObject
</details>

#### Parser>>#parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock

<details>
	<summary>See more</summary>
	
	parse: sourceStream class: class noPattern: noPattern context: aContext notifying: aRequestor ifFail: aBlock 

	^self parse: sourceStream class: class category: nil noPattern: noPattern doIt: noPattern context: aContext notifying: aRequestor ifFail: aBlock 
</details>

#### Parser>>#selectRemovableUnusedTempsFrom: someUnusedTemps

<details>
	<summary>See more</summary>
	
	selectRemovableUnusedTempsFrom: someUnusedTemps

	^someUnusedTemps select: [ :temporaryNode |
		(UnusedVariable name: temporaryNode name) and: [
			temporaryNode isUndefTemp 
				ifTrue: [ true ] 
				ifFalse: [ 
					self inform: 'You''ll first have to remove the\statement where it''s stored into' withNewLines.
					false ]
			]
		].
</details>

#### Parser>>#parse: sourceStreamOrString class: behavior noPattern: aBoolean

<details>
	<summary>See more</summary>
	
	parse: sourceStreamOrString class: behavior noPattern: aBoolean

	^ self
		parse: sourceStreamOrString readStream
		class: behavior
		noPattern: aBoolean
		context: nil
		notifying: nil
		ifFail: [^nil]
</details>

#### Parser>>#primitive: anIntegerOrString error: errorCodeVariableOrNil

Create indexed primitive with optional error code.


<details>
	<summary>See more</summary>
	
	primitive: anIntegerOrString error: errorCodeVariableOrNil
	"Create indexed primitive with optional error code."
	
	^anIntegerOrString isInteger
		ifTrue:
			[errorCodeVariableOrNil ifNotNil:
				[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
			 anIntegerOrString]
		ifFalse:
			[anIntegerOrString isString
				ifTrue: [self primitive: anIntegerOrString module: nil error: errorCodeVariableOrNil]
				ifFalse: [self expected: 'Indexed primitive']]
</details>

#### Parser>>#initPattern: aString notifying: req return: aBlock

<details>
	<summary>See more</summary>
	
	initPattern: aString notifying: req return: aBlock

	| result |
	self
		init: (ReadStream on: aString asString)
		notifying: req
		failBlock: [^nil].
	encoder := self.
	result := aBlock value: (self pattern: false inContext: nil).
	encoder := failBlock := nil.  "break cycles"
	^result
</details>

#### Parser>>#removeUnusedTempsRanges: tempsToRemove

<details>
	<summary>See more</summary>
	
	removeUnusedTempsRanges: tempsToRemove
	
	tempsToRemove inject: 0 into: [ :delta :aRangeToRemove |
		self removeUnusedTempRange: aRangeToRemove with: delta ]
</details>

#### Parser>>#declarationRangesForTemps: someTempNodes

<details>
	<summary>See more</summary>
	
	declarationRangesForTemps: someTempNodes
	
	^someTempNodes collect: [ :temporaryNode | 
		(encoder rangeForNode: temporaryNode ifAbsent: []) first ].
</details>

#### Parser>>#canDeclareInstanceVariable

<details>
	<summary>See more</summary>
	
	canDeclareInstanceVariable
	^encoder classEncoding ~~ UndefinedObject
</details>

#### Parser>>#createTempDeclarationInMethodWith: aTempName

No bars - insert some with CR, tab. Return the new tempsMark.


<details>
	<summary>See more</summary>
	
	createTempDeclarationInMethodWith: aTempName
	"No bars - insert some with CR, tab. Return the new tempsMark."
	
	| characterBeforeMark delta insertion theTextString offset |
	
	theTextString := requestor text string.
	characterBeforeMark := theTextString at: tempsMark - 1 ifAbsent: [$ ].
	insertion := '| ', aTempName, ' |', String newLineString.
	delta := 2.	"the bar and CR"
	characterBeforeMark = Character tab ifTrue: [
		insertion := insertion , String tab.
		delta := delta + 1.	"the tab"
	].
	
	offset := self insertWord: insertion at: tempsMark.

	^tempsMark + offset - delta.
</details>

#### Parser>>#removeUnusedTempsOf: aMethodNode

Scan for unused temp names, and prompt the user about the prospect of removing each one found


<details>
	<summary>See more</summary>
	
	removeUnusedTempsOf: aMethodNode
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	self removeUnusedTempsRanges: (self rangesForRemovableUnusedTempsOf: aMethodNode)
</details>

#### Parser>>#correctVariable: proposedVariable interval: aSpot

Correct the proposedVariable to a known variable, or declare it as a new variable if such action is requested. We support declaring lowercase variables as temps or inst-vars, and uppercase variables as Globals or ClassVars, depending on whether the context is nil (class=UndefinedObject). Spot is the interval within the test stream of the variable. rr 3/4/2004 10:26 : adds the option to define a new class.


<details>
	<summary>See more</summary>
	
	correctVariable: proposedVariable interval: aSpot
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable.
	rr 3/4/2004 10:26 : adds the option to define a new class. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| userSelection action delta spot |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: [
		^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"First check to see if the requestor knows anything about the variable"
	(requestor bindingOf: proposedVariable)
		ifNotNil: [ :binding | ^ encoder global: binding name: proposedVariable].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	userSelection _ requestor selectionInterval.
	delta _ self sourceDelta.
	spot _ aSpot first - delta to: aSpot last - delta.
	requestor selectFrom: spot first to: spot last.

	"Build the menu with alternatives"
	action _ UndeclaredVariable 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.
	^action value
</details>

#### Parser>>#pragmaLiteral: selectorSoFar

Read a pragma literal. As a nicety we allow a variable name (rather than a literal string) as the second argument to primitive:error:


<details>
	<summary>See more</summary>
	
	pragmaLiteral: selectorSoFar
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ (Scanner new scanTokens: self advance) first ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'
</details>

#### Parser>>#match: type

Answer with true if next tokens type matches.


<details>
	<summary>See more</summary>
	
	match: type 
	"Answer with true if next tokens type matches."

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false
</details>

#### Parser>>#correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction

Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated. abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector. Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts.


<details>
	<summary>See more</summary>
	
	correctSelector: proposedKeyword wordIntervals: aSpots exprInterval: expInt ifAbort: abortAction
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection delta adjustedSpots |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [^proposedKeyword asSymbol].
	
	"If the requestor is of an exotic kind (like a telnet server) we might not be allowed to open a PupUpMenu for querying the user"
	"
	((requestor isKindOf: Editor) or: [ requestor is: #Morph ])
		ifFalse: [ ^ proposedKeyword asSymbol ].
	"

	userSelection _ requestor selectionInterval.

	delta := self sourceDelta.
	adjustedSpots := aSpots collect: [ :interval | interval first - delta to: interval last - delta ].
	requestor selectFrom: adjustedSpots first first to: adjustedSpots last last.

	correctSelector _ UnknownSelector name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: adjustedSpots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]
</details>

#### Parser>>#addWarning: aString

ignored by the default compiler.


<details>
	<summary>See more</summary>
	
	addWarning: aString
	"ignored by the default compiler."
</details>

#### Parser>>#newMethodNode

<details>
	<summary>See more</summary>
	
	newMethodNode
	^self encoder methodNodeClass new
</details>

#### Parser>>#fail

<details>
	<summary>See more</summary>
	
	fail

	| exitBlock |
	(encoder == nil or: [encoder == self])
		ifFalse: [encoder release. encoder := nil]. "break cycle"
	exitBlock := failBlock.
	failBlock := nil.
	^exitBlock value
</details>

#### Parser>>#allocateLiteral: lit

<details>
	<summary>See more</summary>
	
	allocateLiteral: lit
	encoder litIndex: lit
</details>

#### Parser>>#pattern: fromDoit inContext: ctxt

unarySelector | binarySelector arg | keyword arg {keyword arg} => {selector, arguments, precedence}.


<details>
	<summary>See more</summary>
	
	pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].
	hereType == #word ifTrue: [^self unaryPattern ].
	self transformVerticalBarAndUpArrowIntoABinarySelector.
	hereType == #binary ifTrue: [^self binaryPattern ].
	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: 'Message pattern'

</details>

#### Parser>>#statements: argNodes innerBlock: inner

<details>
	<summary>See more</summary>
	
	statements: argNodes innerBlock: inner

	^self statements: argNodes innerBlock: inner blockNode: BlockNode new
</details>

#### Parser>>#declareUndeclaredTemps: methodNode

Declare any undeclared temps, declaring them at the smallest enclosing scope.


<details>
	<summary>See more</summary>
	
	declareUndeclaredTemps: methodNode
	"Declare any undeclared temps, declaring them at the smallest enclosing scope."
	| undeclared userSelection blocksToVars |
	(undeclared _ encoder undeclaredTemps) isEmpty ifTrue: [ ^ self ].
	userSelection _ requestor selectionInterval.
	blocksToVars _ IdentityDictionary new.
	undeclared do: [ :var |
		(blocksToVars
			at: (var tag == #method
				ifTrue: [ methodNode block ]
				ifFalse: [ methodNode accept: (VariableScopeFinder new ofVariable: var) ])
			ifAbsentPut: [ SortedCollection new ]) add: var name ].
	(blocksToVars removeKey: methodNode block ifAbsent: nil) ifNotNil: [ :rootVars |
		rootVars do: [ :varName |
			self pasteTempAtMethodLevel: varName ]].
	(blocksToVars keys sort: [ :a :b |
		a tempsMark < b tempsMark ]) do: [ :block | | blockUndeclaredVars |
		blockUndeclaredVars := blocksToVars at: block.
		self declareUndeclaredTemps: blockUndeclaredVars inBlock: block ].
	requestor
		selectInvisiblyFrom: userSelection first
		to: userSelection last + requestorOffset.
	ReparseAfterSourceEditing signal
</details>

#### Parser>>#messagePart: level repeat: repeat

<details>
	<summary>See more</summary>
	
	messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := WriteStream on: (String new: 32).
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [
			
			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].

	parseNode := MessageNode new
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true
</details>

#### Parser>>#createEmptyTempsDeclarationAfter: aDeclarationPosition

Return the position of the end of the declaration.


<details>
	<summary>See more</summary>
	
	createEmptyTempsDeclarationAfter: aDeclarationPosition
	"Return the position of the end of the declaration."
	| offset |
	
	offset := self insertWord: ' | |' at: aDeclarationPosition + 1.
	^aDeclarationPosition + offset
</details>

#### Parser>>#createTempDeclarationOf: variableNode sourceRange: sourceRange

<details>
	<summary>See more</summary>
	
	createTempDeclarationOf: variableNode sourceRange: sourceRange

	| declarationNode |
	declarationNode _ TemporaryDeclarationNode of: variableNode.
	encoder noteSourceRange: sourceRange forNode: declarationNode.
	^ declarationNode
</details>

#### Parser>>#sourceDelta

<details>
	<summary>See more</summary>
	
	sourceDelta

	| userSelectionDelta |
	requestor notNil ifTrue: [
		userSelectionDelta _ requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ].
		encoder selector = Scanner doItSelector ifTrue: [ 
			^ (Scanner selectionDoItSourceCodeHeaderSizeWithContext: false) - userSelectionDelta ].
		encoder selector = Scanner doItInSelector ifTrue: [ 
			^ (Scanner selectionDoItSourceCodeHeaderSizeWithContext: true) - userSelectionDelta ]].
	
	^ 0
</details>

#### Parser>>#assignment: varNode

var ':=' expression => AssignmentNode.


<details>
	<summary>See more</summary>
	
	assignment: varNode
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true
</details>

#### Parser>>#argumentNameWithRangeDo: aBlock

<details>
	<summary>See more</summary>
	
	argumentNameWithRangeDo: aBlock

	hereType == #word ifFalse: [^self expected: 'Argument name'].
	
	^self advanceWithRangeDo: aBlock
</details>

#### Parser>>#rangesForRemovableUnusedTempsInBlockNode: aNodeWithTemporaries

<details>
	<summary>See more</summary>
	
	rangesForRemovableUnusedTempsInBlockNode: aNodeWithTemporaries
	
	| removableTemps unusedTemps |
	
	unusedTemps := self unusedTempsOf: aNodeWithTemporaries.
	removableTemps := self selectRemovableUnusedTempsFrom: unusedTemps.
	
	^self declarationRangesForTemps: removableTemps.
</details>

#### Parser>>#parse: sourceStream class: class noPattern: noPattern notifying: req ifFail: aBlock

<details>
	<summary>See more</summary>
	
	parse: sourceStream class: class noPattern: noPattern notifying: req ifFail: aBlock

	^ self parse: sourceStream class: class noPattern: noPattern context: nil notifying: req ifFail: aBlock
</details>

#### Parser>>#primaryExpression

<details>
	<summary>See more</summary>
	
	primaryExpression 
	hereType == #word 
		ifTrue: [
			parseNode _ self variable.
			
			"
			(parseNode isUndefTemp and: [self interactive] and: [
				((requestor isKindOf: Editor) or: [ requestor is: #Morph ])])
			"
			"If the requestor is of an exotic kind (like a telnet server) we might not be
			 allowed to open a PupUpMenu for querying the user"
				
			(parseNode isUndefTemp and: [ self interactive ])
				ifTrue: [self queryUndefined].
			parseNode nowHasRef.
			^ true].
	hereType == #leftBracket
		ifTrue: [
			self advance.
			self blockExpression.
			^true].
	hereType == #leftBrace
		ifTrue: [
			self braceExpression.
			^true].
	hereType == #leftParenthesis
		ifTrue: [
			self advance.
			self expression ifFalse: [^self expected: 'expression'].
			(self match: #rightParenthesis)
				ifFalse: [^self expected: 'right parenthesis'].
			^true].
	(hereType == #string or: [hereType == #number or: [hereType == #literal]])
		ifTrue: [ 
			parseNode := self advanceWithRangeDo: [ :lexema :range | encoder encodeLiteral: lexema range: range ].
			^true].
	(here == #- and: [tokenType == #number])
		ifTrue: [
			self advanceWithRangeDo: [ :minusChar :minusRange |
				self advanceWithRangeDo: [ :number :numberRange |
					parseNode := encoder encodeLiteral: number negated range: (minusRange first to: numberRange last)]].
			^true].
	^false
</details>

#### Parser>>#encoderClass: anEncoderClass

<details>
	<summary>See more</summary>
	
	encoderClass: anEncoderClass
	encoder ifNotNil: [
		self error: 'encoder already set'].
	encoder := anEncoderClass new
</details>

#### Parser>>#notify: aString

Notify problem at token before 'here'.


<details>
	<summary>See more</summary>
	
	notify: aString 
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset
</details>

#### Parser>>#braceExpression

{ elements } => BraceNode.


<details>
	<summary>See more</summary>
	
	braceExpression
	" { elements } => BraceNode."

	| elements locations loc more sourceRangeStart sourceRangeEnd |

	sourceRangeStart _ hereMark.
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression or right brace'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	sourceRangeEnd _ hereEnd.

	(self match: #rightBrace) ifFalse: [^self expected: 'Period or right brace'].
	encoder noteSourceRange: (sourceRangeStart to: sourceRangeEnd) forNode: parseNode.

	^true
</details>

#### Parser>>#compensateTwoCharacterLookahead

<details>
	<summary>See more</summary>
	
	compensateTwoCharacterLookahead

	^source position - (aheadChar == DoItCharacter ifTrue: [hereChar == DoItCharacter ifTrue: [0] ifFalse: [1]] ifFalse: [2])
	
</details>

#### Parser>>#statements: argNodes innerBlock: inner blockNode: theBlockNode

give initial comment to block, since others trail statements


<details>
	<summary>See more</summary>
	
	statements: argNodes innerBlock: inner blockNode: theBlockNode

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	"Very important. Nil the comment, to avoid printing comments multiple times when pretty-printing"
	currentComment _ nil.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						 stmts size = 0 ifTrue: 
							[stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true
</details>

#### Parser>>#matchReturn

<details>
	<summary>See more</summary>
	
	matchReturn

	^ self match: #upArrow
</details>

#### Parser>>#addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments

<details>
	<summary>See more</summary>
	
	addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments 
		
	self advanceWithRangeDo: [ :keywordAsString :range |
		selector nextPutAll: keywordAsString.
		keywordRanges add: range ].
			
	self argumentNameWithRangeDo: [ :argName :range |
		arguments addLast: (encoder bindArg: argName range: range)]

</details>

#### Parser>>#pasteTempAtMethodLevel: name

<details>
	<summary>See more</summary>
	
	pasteTempAtMethodLevel: name

	| theTextString |
	
	theTextString := requestor text string.
	tempsMark := 
		(theTextString at: tempsMark) = $| 
			ifTrue: [
  				"Paste it before the second vertical bar"
		 		self pasteTemp: name before: tempsMark ] 
			ifFalse: [ self createTempDeclarationInMethodWith: name ].
	
</details>

#### Parser>>#privateReadSelectorFrom: aMethodSource

Answer the message selector for the argument, aMethodSource, which should parse successfully up to the temporary declaration or the end of the method header.


<details>
	<summary>See more</summary>
	
	privateReadSelectorFrom: aMethodSource
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun 4824
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 987 987 987 987 987 987 987 987]'] ] timeToRun  342
	"

	| result |
	self initScannerForTokenization.
	self
		init: (ReadStream on: aMethodSource asString)
		notifying: nil
		failBlock: [ ^nil ].
	encoder _ self.
	result _ self privateReadSelector.
	encoder _ failBlock _ nil.  "break cycles"
	^result
</details>

#### Parser>>#privateReadSelector

<details>
	<summary>See more</summary>
	
	privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: 'Message pattern'
</details>

#### Parser>>#createCascadeNodeWith: receiverNode and: messageNodes

<details>
	<summary>See more</summary>
	
	createCascadeNodeWith: receiverNode and: messageNodes

	| sourceRangeOfFirstMessage |

	parseNode := CascadeNode new receiver: receiverNode messages: messageNodes.
	sourceRangeOfFirstMessage := encoder rawSourceRanges at: messageNodes first.
	sourceRangeOfFirstMessage ifNotNil: [
		| cascadeSourceRangeStart |
		cascadeSourceRangeStart := sourceRangeOfFirstMessage first.
		encoder noteSourceRange: (cascadeSourceRangeStart to: hereMark + 1) forNode: parseNode ]
</details>

#### Parser>>#parseSelector: aString

Answer the message selector for the argument, aString, which should parse successfully up to the temporary declaration or the end of the method header.


<details>
	<summary>See more</summary>
	
	parseSelector: aString 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self initScannerForTokenization.
	^self
		initPattern: aString
		notifying: nil
		return: [:pattern | pattern at: 1]
</details>

#### Parser>>#createTempsDeclarationWith: tempDeclarationNodes sourceRange: sourceRange

<details>
	<summary>See more</summary>
	
	createTempsDeclarationWith: tempDeclarationNodes sourceRange: sourceRange

	|tempsDeclarationNode|
	tempsDeclarationNode _ TemporariesDeclarationNode withAll: tempDeclarationNodes.
	encoder noteSourceRange: sourceRange forNode: tempsDeclarationNode.
	^ tempsDeclarationNode
</details>

#### Parser>>#notify: string at: location

Parser compatible message


<details>
	<summary>See more</summary>
	
	notify: string at: location
	| adjustedLocation |
	adjustedLocation _ location - self sourceDelta.
	requestor
		ifNil: [
			(encoder == self or: [encoder isNil]) ifTrue: [^ self fail  "failure setting up syntax error"].
				SyntaxErrorNotification
					inClass: encoder classEncoding
					category: category
					withCode: 
						(source contents
							copyReplaceFrom: adjustedLocation
							to: adjustedLocation - 1
							with: string , ' ->')
					doitFlag: doitFlag
					errorMessage: string
					location: adjustedLocation]
		ifNotNil: [
			requestor
					notify: string , ' ->'
					at: adjustedLocation
					in: source].
	^self fail
</details>

#### Parser>>#possibleVariablesFor: proposedVariable

<details>
	<summary>See more</summary>
	
	possibleVariablesFor: proposedVariable 
	^encoder possibleVariablesFor: proposedVariable
</details>

#### Parser>>#doitPatternInContext: context

<details>
	<summary>See more</summary>
	
	doitPatternInContext: context

	^context 
		ifNil: [{self class doItSelector. {}. 1. nil }]
		ifNotNil: [{self class doItInSelector. {encoder encodeVariable: encoder doItInContextName}. 3. nil}]
</details>

#### Parser>>#declareUndeclaredTemps: undeclaredTempNodes inBlock: aDeclaringBlockNode

<details>
	<summary>See more</summary>
	
	declareUndeclaredTemps: undeclaredTempNodes inBlock: aDeclaringBlockNode

	| blockTempsMark |
	
	blockTempsMark := self createEmptyTempsDeclarationIfNecessaryIn: aDeclaringBlockNode.
	undeclaredTempNodes do: [ :varName | blockTempsMark := self pasteTemp: varName before: blockTempsMark ]
</details>

#### Parser>>#filterSeparatorsToTheRightOn: currentSource startingAt: anInitialPosition

<details>
	<summary>See more</summary>
	
	filterSeparatorsToTheRightOn: currentSource startingAt: anInitialPosition

	| end |
	
	end := anInitialPosition.	
	[ (currentSource at: end+1) isSeparator ] whileTrue: [ end := end + 1 ].
	
	^end

</details>

## Scanner

I scan a string or text, picking out Smalltalk syntactic tokens. I look one character ahead. I put each token found into the instance variable, token, and its type (a Symbol) into the variable, tokenType. At the end of the input stream, I pretend to see an endless sequence of special characters called doIts. Instance Variables aheadChar: <Character> buffer: <WriteStream> currentComment: <OrderedCollection> hereChar: <Character> mark: <Integer> source: <ReadStream> token: <Symbol|String|NumberCharacter|Boolean|nil> tokenType: <Symbol> typeTable: <Array> aheadChar - the next character in the input stream buffer - a reusable WriteStream on a String which is used for building strings. Shouldn't be used from multiple methods without resetting. currentComment - an OrderedCollection of strings which contain all comments between the current token and the previous token or the beginning of the source. hereChar - the current character mark - the position of the current token in the source stream source - the input stream of characters token - the current token tokenType - the type of the current token. The possible token types are: #binary, #character, #colon, #doIt, #keyword, #leftArrow, #leftBrace, #leftBracket, #leftParenthesis, #literal, #period, #rightBrace, #rightBracket, #rightParenthesis, #semicolon, #string, #upArrow, #verticalBar, #word, #xBinary, #xColon, #xDelimiter, #xDigit, #xDollar, #xDoubleQuote, #xLetter, #xLitQuote, #xSingleQuote, #xUnderscore typeTable - an array that maps each an evaluable tokenType to each character with asciiValue between 0 and 255 See http://www.whysmalltalk.com/articles/bykov/HitchHiker.htm

### Methods
#### Scanner>>#xLitQuote

Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'.


<details>
	<summary>See more</summary>
	
	xLitQuote
	"Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'."
	| start |
	start := mark.
	self step. "litQuote"
	self scanToken.
	tokenType == #leftParenthesis
		ifTrue: [
			self scanToken; scanLitVec.
			mark := start + 1.
			tokenType == #doIt
				ifTrue: [self offEnd: 'Unmatched parenthesis']]
		ifFalse: [
			tokenType == #leftBracket
				ifTrue: [
					self scanToken; scanLitByteVec.
					mark := start + 1.
					tokenType == #doIt
						ifTrue: [self offEnd: 'Unmatched bracket']]
				ifFalse: [
					(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon or: [tokenType == #leftArrow]]])
						ifTrue: [self scanLitWord]
						ifFalse: [tokenType == #string
							ifTrue: [token := token asSymbol]
							ifFalse: [
								(tokenType == #binary or: [ tokenType == #verticalBar ]) 
									ifFalse: [self notify: 'Invalid literal character' at: start + 1]]]]].
	mark := start.
	tokenType := #literal

	"#(Pen)
	#Pen
	#'Pen'
	##Pen
	###Pen
	"
</details>

#### Scanner>>#checkpoint

Return a copy of all changeable state. See revertToCheckpoint:


<details>
	<summary>See more</summary>
	
	checkpoint
	"Return a copy of all changeable state.  See revertToCheckpoint:"

	^ {self copy. source copy. currentComment copy}
</details>

#### Scanner>>#typedScanTokens: textOrString

Answer an Array that has been tokenized with literals mapped to literals, special characters mapped to symbols and variable names and keywords to strings. This methiod accepts _ (underscore) as an assignment token irrespective of whether the system prefers := as the assignment token.


<details>
	<summary>See more</summary>
	
	typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings. This methiod accepts _ (underscore) as an assignment token
	 irrespective of whether the system prefers := as the assignment token."
	| s |
	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asString).
	s := WriteStream on: (Array new: 16).
	[tokenType == #doIt] whileFalse:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
			[self scanToken.
			 token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"
</details>

#### Scanner>>#initScannerForTokenization

Don't raise xIllegal when enocuntering an _


<details>
	<summary>See more</summary>
	
	initScannerForTokenization
	"Don't raise xIllegal when enocuntering an _"
	"Simpler implementation for Cuis"
	isForTokenization _ true.
	ignoreBacktick _ true.
</details>

#### Scanner>>#xUnderscore

Figure out if x _foo (no space between _ and foo) should be a selector or assignment


<details>
	<summary>See more</summary>
	
	xUnderscore
	| type |
	"Figure out if x _foo (no space between _ and foo) 
	should be a selector or assignment"
	((type := self typeTableAt: aheadChar) == #xLetter
		or:[type == #xDigit or:[type == #xUnderscore]]) ifFalse: [
			self step.
			tokenType := #leftArrow.
			^token := #'_'
	].
	^self xLetter
</details>

#### Scanner>>#xBinary

<details>
	<summary>See more</summary>
	
	xBinary

	tokenType _ #binary.
	token _ String streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type _ self typeTableAt: hereChar.
		type == #xBinary and: [hereChar ~= $- or: [aheadChar isDigit not]]
		] whileTrue: [
		stream nextPut: self step]].
	token _ token asSymbol
</details>

#### Scanner>>#xDelimiter

Ignore blanks, etc.


<details>
	<summary>See more</summary>
	
	xDelimiter
	"Ignore blanks, etc."

	self scanToken
</details>

#### Scanner>>#compileBacktickCode

<details>
	<summary>See more</summary>
	
	compileBacktickCode

	| compiler method methodNode |
	
	compiler _ Compiler new.
	methodNode _ compiler compileNoPattern: buffer contents in: UndefinedObject context: nil notifying: nil ifFail: [].
	method _ methodNode generate.
	
	"Grab all messages sent while evaluating literal, so the main method will be shown as senders of them"
	sentInLiterals addAll: method messages.
	method literalsDo: [ :literal | literal isSymbol ifTrue: [ literal = self class doItSelector ifFalse: [sentInLiterals add: literal ]]].
		
	"Evaluate now."
	token _ nil withArgs: #() executeMethod: method 
</details>

#### Scanner>>#step

<details>
	<summary>See more</summary>
	
	step

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := DoItCharacter "doit"]
		ifFalse: [aheadChar := source next].
	^c
</details>

#### Scanner>>#offEnd: aString

Parser overrides this


<details>
	<summary>See more</summary>
	
	offEnd: aString 
	"Parser overrides this"

	^self notify: aString
</details>

#### Scanner>>#scanLitByteVec

Also accept Floats besides bytes! #[1 2 3 255] #[1.0 0.2 1.0] #[1.0 -0.2e-23 1.0e4]


<details>
	<summary>See more</summary>
	
	scanLitByteVec
	"Also accept Floats besides bytes!
	#[1 2 3 255]
	#[1.0 0.2 1.0]
	#[1.0 -0.2e-23 1.0e4]
	"
	| stream |
	stream _ nil.
	[ tokenType == #rightBracket or: [ tokenType == #doIt ] ] whileFalse: [
		(token == #- and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
			self scanToken.
			token _ token negated ].
		((token isInteger and: [ token between: 0 and: 255 ]) or: [token isFloat])
			ifFalse: [ ^ self offEnd: '8-bit integer, floating point number, or right bracket expected' ].
		stream ifNil: [
			stream _ ((token isFloat ifTrue: [Float64Array] ifFalse: [ByteArray]) new: 16) writeStream ].
		stream nextPut: token.
		self scanToken ].
	token _ stream
		ifNotNil: [ stream contents ]
		ifNil: [
			"For back compatibility, if empty, assume ByteArray"
			ByteArray new ]
</details>

#### Scanner>>#xDoubleQuote

Collect a comment.


<details>
	<summary>See more</summary>
	
	xDoubleQuote
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := WriteStream on: (String new: 200).
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar == stopChar and: [source atEnd])
                ifTrue: [^self offEnd: 'Unmatched comment quote'].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken
</details>

#### Scanner>>#scanToken

Skip delimiters fast, there almost always is one.


<details>
	<summary>See more</summary>
	
	scanToken

	"Skip delimiters fast, there almost always is one."
	self skipDelimitersAndBacktickIfNecessary.

	mark := source position - 1.
	(tokenType at: 1) = $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step asSymbol "else just unique the first char"].
	^token
</details>

#### Scanner>>#isBacktickAndShouldIgnoreIt

I compare with true because there are many ways to initialize the scanner and ingoreBacktick could be nil - Hernan


<details>
	<summary>See more</summary>
	
	isBacktickAndShouldIgnoreIt 
	
	"I compare with true because there are many ways to initialize the scanner and ingoreBacktick could be nil - Hernan"
	^ ignoreBacktick == true and: [tokenType = #xBacktick]
</details>

#### Scanner>>#scanForFindSelectorUpTo: terminator

Scanner findSelectorTests


<details>
	<summary>See more</summary>
	
	scanForFindSelectorUpTo: terminator
"
Scanner findSelectorTests
"
    | s |

    s := WriteStream on: (String new: 100).
    [tokenType == terminator or: [tokenType == #doIt]] whileFalse: [
        tokenType caseOf: {
            [#leftParenthesis] -> [self scanToken; scanForFindSelectorUpTo: #rightParenthesis].
            [#leftBracket] -> [self scanToken; scanForFindSelectorUpTo: #rightBracket].
            [#leftBrace] -> [self scanToken; scanForFindSelectorUpTo: #rightBrace].
            [#keyword] -> [s nextPutAll: token].
        } otherwise: [].
        self scanToken
    ].
    ^s contents
</details>

#### Scanner>>#xLetter

Form a word or keyword.


<details>
	<summary>See more</summary>
	
	xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [DoItCharacter "doit"]
							ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents
</details>

#### Scanner>>#xDigit

Form a number.


<details>
	<summary>See more</summary>
	
	xDigit
	"Form a number."

	tokenType := #number.
	(aheadChar == DoItCharacter and: [source atEnd
			and:  [source skip: -1. source next ~~ DoItCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step
</details>

#### Scanner>>#scanFieldNames: stringOrArray

Answer an Array of Strings that are the identifiers in the input string, stringOrArray. If passed an Array, just answer with that Array, i.e., assume it has already been scanned.


<details>
	<summary>See more</summary>
	
	scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: (ReadStream on: stringOrArray asString).
	strm := WriteStream on: (Array new: 10).
	[tokenType == #doIt]
		whileFalse: 
			[tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"
</details>

#### Scanner>>#typeTableAt: aCharacter

<details>
	<summary>See more</summary>
	
	typeTableAt: aCharacter
	^typeTable at: aCharacter numericValue ifAbsent:[#xLetter]
</details>

#### Scanner>>#xDollar

Form a Character literal.


<details>
	<summary>See more</summary>
	
	xDollar
	"Form a Character literal."

	self step. "pass over $"
	token := self step.
	tokenType := #number "really should be Char, but rest of compiler doesn't know"
</details>

#### Scanner>>#isAt: aChar

<details>
	<summary>See more</summary>
	
	isAt: aChar
	
	^ hereChar == aChar and: [aheadChar == aChar ifTrue: [self step. false] ifFalse: [true]]
</details>

#### Scanner>>#notify: string

Refer to the comment in Object|notify:.


<details>
	<summary>See more</summary>
	
	notify: string 
	"Refer to the comment in Object|notify:." 
	self error: string
</details>

#### Scanner>>#xBacktick

Smalltalk code evaluated at compile time as a literal.


<details>
	<summary>See more</summary>
	
	xBacktick
	
	"Smalltalk code evaluated at compile time as a literal."

	self readUpToNext: $` ifNotFound: [^self offEnd: 'Unmatched back quote'].
	self compileBacktickCodeHandlingErrors.
</details>

#### Scanner>>#errorMultibyteCharacter

<details>
	<summary>See more</summary>
	
	errorMultibyteCharacter

	self error: 'multi-byte character is found at unexpected place'.

</details>

#### Scanner>>#skipDelimitersAndBacktickIfNecessary

<details>
	<summary>See more</summary>
	
	skipDelimitersAndBacktickIfNecessary

	[self skipDelimiters.
	self isBacktickAndShouldIgnoreIt ] whileTrue: [self step].  
</details>

#### Scanner>>#skipDelimiters

<details>
	<summary>See more</summary>
	
	skipDelimiters

	[(tokenType := self typeTableAt: hereChar) == #xDelimiter] whileTrue: [self step].  

</details>

#### Scanner>>#xIllegal

An illegal character was encountered


<details>
	<summary>See more</summary>
	
	xIllegal
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar numericValue printString, ' ', hereChar numericValue hex , ')' at: mark
</details>

#### Scanner>>#compileBacktickCodeHandlingErrors

<details>
	<summary>See more</summary>
	
	compileBacktickCodeHandlingErrors

	[[[self compileBacktickCode ] 
		on: SyntaxErrorNotification
		do: [ :ex | self notify: 'Can not compile: ', ex errorMessage at: mark]]
		on: UndeclaredVariableReference
		do: [ :ex | self notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: mark ]]
		on: Error
		do: [ :ex | self notify: 'Can not evaluate code: ', ex description at: mark ].
	
	tokenType _ #literal
</details>

#### Scanner>>#scanAllTokenPositionsInto: aBlock

Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments.


<details>
	<summary>See more</summary>
	
	scanAllTokenPositionsInto: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments."

	| lastMark |
	lastMark := 1.
	[currentComment ifNotNil:
		[currentComment do:
			[:cmnt| | idx |
			 idx := source originalContents indexOfSubCollection: cmnt startingAt: lastMark.
			 (idx > 0 and: [idx < mark]) ifTrue:
				[aBlock value: idx - 1 value: (lastMark := idx + cmnt size)]].
		 currentComment := nil].
	mark ifNotNil:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue:
			[| savedMark |
			 savedMark := mark.
			 self scanToken.
			 token := token negated.
			 mark := savedMark].
		"Compensate for the fact that the parser uses two character lookahead.  Normally we must
		  remove the extra two characters.  But this mustn't happen for the last token at the end of stream."
		 aBlock
			value: mark
			value: self compensateTwoCharacterLookahead ].
	 (tokenType == #rightParenthesis
	  or: [tokenType == #doIt]) ifTrue:
		[^self].
	tokenType == #leftParenthesis
		ifTrue: 
			[self scanToken; scanAllTokenPositionsInto: aBlock]
		ifFalse: 
			[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
				ifTrue: 
					[self scanLitWord.
					 token == #true ifTrue: [token := true].
					 token == #false ifTrue: [token := false].
					 token == #nil ifTrue: [token := nil]]
				ifFalse:
					[(token == #- 
					  and: [(self typeTableAt: hereChar) == #xDigit])
						ifTrue: 
							[self scanToken.
							 token := token negated]]].
		self scanToken ] repeat
</details>

#### Scanner>>#xSingleQuote

String.


<details>
	<summary>See more</summary>
	
	xSingleQuote
	
	"String."

	self readUpToNext: $'  ifNotFound: [^self offEnd: 'Unmatched string quote'].
	tokenType := #string
</details>

#### Scanner>>#xColon

Allow := for assignment


<details>
	<summary>See more</summary>
	
	xColon
	"Allow := for assignment"
	
	aheadChar == $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol
</details>

#### Scanner>>#scanTokens: textOrString

Answer an Array that has been tokenized as though the input text, textOrString, had appeared between the array delimitors #( and ) in a Smalltalk literal expression.


<details>
	<summary>See more</summary>
	
	scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self initScannerForTokenization.
	self scan: (ReadStream on: textOrString asString).
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"
</details>

#### Scanner>>#scanMessageParts: sourceString

Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method. Courtesy of Ted Kaehler, June 1999


<details>
	<summary>See more</summary>
	
	scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: (ReadStream on: sourceString asString).
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll
</details>

#### Scanner>>#scanTokenPositionsIn: textOrString into: aBlock

Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString.


<details>
	<summary>See more</summary>
	
	scanTokenPositionsIn: textOrString into: aBlock
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source := (ReadStream on: textOrString asString).
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]'"
</details>

#### Scanner>>#advance

<details>
	<summary>See more</summary>
	
	advance

	| prevToken |
	prevToken := token.
	self scanToken.
	^prevToken
</details>

#### Scanner>>#scanLitWord

Accumulate keywords and asSymbol the result.


<details>
	<summary>See more</summary>
	
	scanLitWord
	"Accumulate keywords and asSymbol the result."

	token := (String streamContents: [ :stream |
		stream nextPutAll: token.
		[ (self typeTableAt: hereChar) == #xLetter ] whileTrue: [
			self xLetter.
			stream nextPutAll: token ] ]) asSymbol
</details>

#### Scanner>>#readUpToNext: aChar ifNotFound: aNotFoundBlock

<details>
	<summary>See more</summary>
	
	readUpToNext: aChar ifNotFound: aNotFoundBlock

	self step.
	buffer reset.
	
	[self isAt: aChar]
		whileFalse: 
			[buffer nextPut: self step.
			(hereChar == DoItCharacter and: [source atEnd]) ifTrue: [^aNotFoundBlock value ]].
	
	self step.
	token := buffer contents.
	
</details>

#### Scanner>>#scanLitVec

<details>
	<summary>See more</summary>
	
	scanLitVec
	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]] whileFalse:
		[tokenType == #leftParenthesis
			ifTrue: 
				[self scanToken; scanLitVec]
			ifFalse: 
				[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
					ifTrue: 
						[self scanLitWord.
						token == #true ifTrue: [token := true].
						token == #false ifTrue: [token := false].
						token == #nil ifTrue: [token := nil]]
					ifFalse:
						[(token == #- 
						  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
							[self scanToken.
							 token := token negated]]].
		s nextPut: token.
		self scanToken].
	token := s contents
</details>

#### Scanner>>#initScanner

<details>
	<summary>See more</summary>
	
	initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new.
	ignoreBacktick := true
</details>

#### Scanner>>#notify: string at: posiiton

Parser compatible message


<details>
	<summary>See more</summary>
	
	notify: string at: posiiton
	"Parser compatible message"
	 
	^self notify: string 
</details>

#### Scanner>>#ignoreBacktick: aBoolean

<details>
	<summary>See more</summary>
	
	ignoreBacktick: aBoolean

	ignoreBacktick := aBoolean 
</details>

#### Scanner>>#scan: inputStream

Bind the input stream, fill the character buffers and first token buffer.


<details>
	<summary>See more</summary>
	
	scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source := inputStream.
	self step.
	self step.
	self scanToken
</details>

