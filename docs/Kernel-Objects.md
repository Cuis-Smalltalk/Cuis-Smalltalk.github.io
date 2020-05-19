## ActiveModel

Part of update 5005u16-Events.cs: "Change Set: Events Date: 11 February 2002 Author: Rob Withers et al. Published to 3.3a as 4756Events.cs The NuBlue events code. Finally..." The new event model, originally from VisualSmalltalk. This includes #when:send:to: and friends, in Object (generic implementation) and ActiveModel (optimized implementation). It renders the old change/update mechanism in Object and Model as obsolete, although still used.

### Methods
#### ActiveModel>>#updateableActionMap

<details>
	<summary>See more</summary>
	
	updateableActionMap

	actionMap ifNil: [
		actionMap _ IdentityDictionary new ].
	^actionMap
</details>

#### ActiveModel>>#releaseActionMap

<details>
	<summary>See more</summary>
	
	releaseActionMap

    actionMap := nil
</details>

#### ActiveModel>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	actionMap _ nil
</details>

#### ActiveModel>>#actionMap

<details>
	<summary>See more</summary>
	
	actionMap

    ^actionMap
</details>

## Boolean

Boolean is an abstract class defining the protocol for logic testing operations and conditional control structures for the logical values represented by the instances of its subclasses True and False. Boolean redefines #new so no instances of Boolean can be created. It also redefines several messages in the 'copying' protocol to ensure that only one instance of each of its subclasses True (the global true, logical assertion) and False (the global false, logical negation) ever exist in the system.

### Methods
#### Boolean>>#and: block1 and: block2 and: block3 and: block4

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: block1 and: block2 and: block3 and: block4
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
</details>

#### Boolean>>#not

Negation. Answer true if the receiver is false, answer false if the receiver is true.


<details>
	<summary>See more</summary>
	
	not
	"Negation. Answer true if the receiver is false, answer false if the 
	receiver is true."

	self subclassResponsibility
</details>

#### Boolean>>#or: block1 or: block2

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: block1 or: block2
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
</details>

#### Boolean>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	self printOn: aStream
</details>

#### Boolean>>#and: block1 and: block2

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: block1 and: block2
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
</details>

#### Boolean>>#ifTrue: alternativeBlock

If the receiver is false (i.e., the condition is false), then the value is the false alternative, which is nil. Otherwise answer the result of evaluating the argument, alternativeBlock. Create an error notification if the receiver is nonBoolean. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: alternativeBlock 
	"If the receiver is false (i.e., the condition is false), then the value is the 
	false alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."

	self subclassResponsibility
</details>

#### Boolean>>#& aBoolean

Evaluating conjunction. Evaluate the argument. Then answer true if both the receiver and the argument are true.


<details>
	<summary>See more</summary>
	
	& aBoolean 
	"Evaluating conjunction. Evaluate the argument. Then answer true if 
	both the receiver and the argument are true."

	self subclassResponsibility
</details>

#### Boolean>>#eqv: aBoolean

Answer true if the receiver is equivalent to aBoolean.


<details>
	<summary>See more</summary>
	
	eqv: aBoolean 
	"Answer true if the receiver is equivalent to aBoolean."

	^self == aBoolean
</details>

#### Boolean>>#xor: aBoolean

<details>
	<summary>See more</summary>
	
	xor: aBoolean

	self subclassResponsibility
</details>

#### Boolean>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^#Boolean = aSymbol or: [ super is: aSymbol ]
</details>

#### Boolean>>#and: block1 and: block2 and: block3

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: block1 and: block2 and: block3
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
</details>

#### Boolean>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral 
	^ true
</details>

#### Boolean>>#or: alternativeBlock

Nonevaluating disjunction. If the receiver is false, answer the value of the argument, alternativeBlock; otherwise answer true without evaluating the argument.


<details>
	<summary>See more</summary>
	
	or: alternativeBlock 
	"Nonevaluating disjunction. If the receiver is false, answer the value of 
	the argument, alternativeBlock; otherwise answer true without 
	evaluating the argument."

	self subclassResponsibility
</details>

#### Boolean>>#ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock

If the receiver is true (i.e., the condition is true), then answer the value of the argument trueAlternativeBlock. If the receiver is false, answer the result of evaluating the argument falseAlternativeBlock. If the receiver is a nonBoolean then create an error notification. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"If the receiver is true (i.e., the condition is true), then answer the value 
	of the argument trueAlternativeBlock. If the receiver is false, answer the 
	result of evaluating the argument falseAlternativeBlock. If the receiver 
	is a nonBoolean then create an error notification. Execution does not 
	actually reach here because the expression is compiled in-line."

	self subclassResponsibility
</details>

#### Boolean>>#or: block1 or: block2 or: block3 or: block4

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: block1 or: block2 or: block3 or: block4
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
</details>

#### Boolean>>#| aBoolean

Evaluating disjunction (OR). Evaluate the argument. Then answer true if either the receiver or the argument is true.


<details>
	<summary>See more</summary>
	
	| aBoolean 
	"Evaluating disjunction (OR). Evaluate the argument. Then answer true 
	if either the receiver or the argument is true."

	self subclassResponsibility
</details>

#### Boolean>>#and: alternativeBlock

Nonevaluating conjunction. If the receiver is true, answer the value of the argument, alternativeBlock; otherwise answer false without evaluating the argument.


<details>
	<summary>See more</summary>
	
	and: alternativeBlock 
	"Nonevaluating conjunction. If the receiver is true, answer the value of 
	the argument, alternativeBlock; otherwise answer false without 
	evaluating the argument."

	self subclassResponsibility
</details>

#### Boolean>>#or: block1 or: block2 or: block3

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: block1 or: block2 or: block3
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
</details>

#### Boolean>>#ifFalse: alternativeBlock

If the receiver is true (i.e., the condition is true), then the value is the true alternative, which is nil. Otherwise answer the result of evaluating the argument, alternativeBlock. Create an error notification if the receiver is nonBoolean. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifFalse: alternativeBlock 
	"If the receiver is true (i.e., the condition is true), then the value is the 
	true alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."

	self subclassResponsibility
</details>

#### Boolean>>#ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock

Same as ifTrue:ifFalse:.


<details>
	<summary>See more</summary>
	
	ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Same as ifTrue:ifFalse:."

	self subclassResponsibility
</details>

#### Boolean>>#shallowCopy

Receiver has two concrete subclasses, True and False. Only one instance of each should be made, so return self.


<details>
	<summary>See more</summary>
	
	shallowCopy 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."
</details>

## False

False defines the behavior of its single instance, false -- logical negation. Notice how the truth-value checks become direct message sends, without the need for explicit testing. Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no effect.

### Methods
#### False>>#not

Negation -- answer true since the receiver is false.


<details>
	<summary>See more</summary>
	
	not
	"Negation -- answer true since the receiver is false."

	^true
</details>

#### False>>#and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

	^self
</details>

#### False>>#and: aBlock1 and: aBlock2

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2

	^self
</details>

#### False>>#or: aBlock1 or: aBlock2

I sending value to aBlock2 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2

	"I sending value to aBlock2 to optimize the generated byte-code - Hernan"
	^aBlock1 value or: [ aBlock2 value ]

</details>

#### False>>#ifTrue: alternativeBlock

Since the condition is false, answer the value of the false alternative, which is nil. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: alternativeBlock 
	"Since the condition is false, answer the value of the false alternative, 
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."

	^nil
</details>

#### False>>#& alternativeObject

Evaluating conjunction -- answer false since receiver is false.


<details>
	<summary>See more</summary>
	
	& alternativeObject 
	"Evaluating conjunction -- answer false since receiver is false."

	^self
</details>

#### False>>#xor: aBoolean

Posted by Eliot Miranda to squeak-dev on 3/24/2009


<details>
	<summary>See more</summary>
	
	xor: aBoolean
	"Posted by Eliot Miranda to squeak-dev on 3/24/2009"

	^aBoolean
</details>

#### False>>#or: alternativeBlock

Nonevaluating disjunction -- answer value of alternativeBlock.


<details>
	<summary>See more</summary>
	
	or: alternativeBlock 
	"Nonevaluating disjunction -- answer value of alternativeBlock."

	^alternativeBlock value
</details>

#### False>>#and: aBlock1 and: aBlock2 and: aBlock3

Nonevaluating conjunction without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as false, then return false immediately, without evaluating any further blocks. If all return true, then return true.


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2 and: aBlock3

	^self
</details>

#### False>>#ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock

Answer the value of falseAlternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value
</details>

#### False>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream 

	aStream nextPutAll: 'false'
</details>

#### False>>#or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

I sending value to aBlock4 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

	"I sending value to aBlock4 to optimize the generated byte-code - Hernan"
	^aBlock1 value or: [ aBlock2 value or: [ aBlock3 value or: [ aBlock4  value ]]].

</details>

#### False>>#| aBoolean

Evaluating disjunction (OR) -- answer with the argument, aBoolean.


<details>
	<summary>See more</summary>
	
	| aBoolean 
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean
</details>

#### False>>#and: alternativeBlock

Nonevaluating conjunction -- answer with false since the receiver is false.


<details>
	<summary>See more</summary>
	
	and: alternativeBlock 
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self
</details>

#### False>>#orNot: alternativeBlock

<details>
	<summary>See more</summary>
	
	orNot: alternativeBlock

	^alternativeBlock value not
</details>

#### False>>#ifFalse: alternativeBlock

Answer the value of alternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifFalse: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value
</details>

#### False>>#or: aBlock1 or: aBlock2 or: aBlock3

I sending value to aBlock3 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2 or: aBlock3

	"I sending value to aBlock3 to optimize the generated byte-code - Hernan"
	^aBlock1 value or: [ aBlock2 value or: [ aBlock3 value ] ]
</details>

#### False>>#ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock

Answer the value of falseAlternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value
</details>

## Float32SlotsObject

Abstract superclass for objects whose slots are 32 bit Floating Point values, but don't inherit from FloatArray because they are not collections, and collection protocol makes no sense on them.

### Methods
#### Float32SlotsObject>>#slotAt: index put: value

<details>
	<summary>See more</summary>
	
	slotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self slotAt: index put: value asFloat ].
	^value
</details>

#### Float32SlotsObject>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self size * 4
</details>

#### Float32SlotsObject>>#slotAt: index

<details>
	<summary>See more</summary>
	
	slotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
</details>

## MessageSend

Instances of MessageSend encapsulate message sends to objects. Arguments can be either predefined or supplied when the message send is performed. MessageSends are used to implement the #when:send:to: event system. Use #value to perform a message send with its predefined arguments and #valueWithArguments: if additonal arguments have to supplied. Structure: receiver Object -- object receiving the message send selector Symbol -- message selector arguments Array -- bound arguments

### Methods
#### MessageSend>>#collectArguments: anArgArray

Private


<details>
	<summary>See more</summary>
	
	collectArguments: anArgArray
	"Private"

    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [anArgArray]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs copy] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]
</details>

#### MessageSend>>#arguments

<details>
	<summary>See more</summary>
	
	arguments
	^ arguments
</details>

#### MessageSend>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]
</details>

#### MessageSend>>#valueWithArguments: anArray

<details>
	<summary>See more</summary>
	
	valueWithArguments: anArray

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)
</details>

#### MessageSend>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^ selector
</details>

#### MessageSend>>#selector: aSymbol

<details>
	<summary>See more</summary>
	
	selector: aSymbol
	selector _ aSymbol
</details>

#### MessageSend>>#receiver

<details>
	<summary>See more</summary>
	
	receiver
	^ receiver
</details>

#### MessageSend>>#value

Send the message and answer the return value


<details>
	<summary>See more</summary>
	
	value
	"Send the message and answer the return value"

	arguments ifNil: [^ receiver perform: selector].

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: arguments)
</details>

#### MessageSend>>#= anObject

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= anObject

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	^ anObject species == self species 
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]
</details>

#### MessageSend>>#isValid

<details>
	<summary>See more</summary>
	
	isValid
	^true
</details>

#### MessageSend>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ receiver hash bitXor: selector hash
</details>

#### MessageSend>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        receiver printOn: aStream.
        aStream nextPut: $)
</details>

#### MessageSend>>#receiver: anObject

<details>
	<summary>See more</summary>
	
	receiver: anObject
	receiver _ anObject
</details>

#### MessageSend>>#arguments: anArray

<details>
	<summary>See more</summary>
	
	arguments: anArray
	arguments _ anArray
</details>

#### MessageSend>>#asMinimalRepresentation

<details>
	<summary>See more</summary>
	
	asMinimalRepresentation
	^self
</details>

## Object

Object is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass of Object) and its subclasses. Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message sending, and reflection. Also utility messages that all objects should respond to are defined here. Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and layout of certain standard classes. Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed by certain subsystems or applications, or to respond to certain general test messages such as isMorph.

### Methods
#### Object>>#value

<details>
	<summary>See more</summary>
	
	value

	^self
</details>

#### Object>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral
	"Answer whether the receiver has a literal text form recognized by the compiler.
	The literal form must be provided by #storeOn:
	"

	^false
</details>

#### Object>>#executor

Return an object which can act as executor for finalization of the receiver


<details>
	<summary>See more</summary>
	
	executor
	"Return an object which can act as executor for finalization of the receiver"
	^self shallowCopy actAsExecutor
</details>

#### Object>>#isArray

<details>
	<summary>See more</summary>
	
	isArray
	^false
</details>

#### Object>>#isVariableBinding

Return true if I represent a literal variable binding


<details>
	<summary>See more</summary>
	
	isVariableBinding
	"Return true if I represent a literal variable binding"
	^false
	
</details>

#### Object>>#perform: aSymbol with: firstObject with: secondObject

Send the selector, aSymbol, to the receiver with the given arguments. Fail if the number of arguments expected by the selector is not two. Primitive. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)
</details>

#### Object>>#actAsExecutor

Prepare the receiver to act as executor for any resources associated with it


<details>
	<summary>See more</summary>
	
	actAsExecutor
	"Prepare the receiver to act as executor for any resources associated with it"
	self breakDependents
</details>

#### Object>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self class isVariable ifFalse: [self errorNotIndexable].
	^ 0
</details>

#### Object>>#becomeForward: otherObject

Primitive. All variables in the entire system that used to point to the receiver now point to the argument. Fails if either argument is a SmallInteger.


<details>
	<summary>See more</summary>
	
	becomeForward: otherObject 
	"Primitive. All variables in the entire system that used to point
	to the receiver now point to the argument.
	Fails if either argument is a SmallInteger."

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsForwardIdentityTo: { otherObject }
</details>

#### Object>>#storeDataOn: aDataStream

Store myself on a DataStream. Answer self. This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream. NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects. readDataFrom:size: reads back what we write here.


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"Store myself on a DataStream.  Answer self.  This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream.  NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects.  readDataFrom:size: reads back what we write here."
	| cntInstVars cntIndexedVars |

	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].

	"Write fields of a variable length object.  When writing to a dummy 
		stream, don't bother to write the bytes"
	((aDataStream byteStream class == DummyStream) and: [self class isBits]) ifFalse: [
		self class isWeak
			ifTrue: [
				"For weak classes (for example DependentsArray) write the referenced object only
				if referenced from elsewhere in the dumped object graph.
				This means, for instance that if we only dump a model, no dependents are stored, 
				but if we store a view (i.e. a Morph), it is properly handled as a dependent after the object graph is revived."
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPutWeak: (self basicAt: i)]]
			ifFalse: [
				1 to: cntIndexedVars do: [ :i |
					aDataStream nextPut: (self basicAt: i)]]]
</details>

#### Object>>#error: aString

Throw a generic Error exception.


<details>
	<summary>See more</summary>
	
	error: aString 
	"Throw a generic Error exception."

	^Error new signal: aString
</details>

#### Object>>#actionsDo: aBlock

<details>
	<summary>See more</summary>
	
	actionsDo: aBlock

	self actionMap ifNotNil: [ :map |
		map do: aBlock ]
</details>

#### Object>>#handles: exception

This method exists to break an endless loop in Exception>>findHandlerFrom: if the exception is invalid


<details>
	<summary>See more</summary>
	
	handles: exception
	"This method exists to break an endless loop in Exception>>findHandlerFrom: if the exception
is invalid"
	^false
</details>

#### Object>>#addInstanceVarNamed: aName withValue: aValue

Add an instance variable named aName and give it value aValue


<details>
	<summary>See more</summary>
	
	addInstanceVarNamed: aName withValue: aValue
	"Add an instance variable named aName and give it value aValue"
	self class addInstVarName: aName asString.
	self instVarAt: self class instSize put: aValue
</details>

#### Object>>#wantsSteps

Overridden by morphic classes whose instances want to be stepped all the time, or by model classes who want their morphic views to be stepped all the time. Some classes might answer false to this message, and call #startStepping #startSteppingStepTime: #stopStepping as appropriate


<details>
	<summary>See more</summary>
	
	wantsSteps
	"Overridden by morphic classes whose instances want to be stepped all the time,
	or by model classes who want their morphic views to be stepped all the time.
	
	Some classes might answer false to this message, and call
		#startStepping 
		#startSteppingStepTime:
		#stopStepping
	as appropriate"

	^ false
</details>

#### Object>>#runningWorld

Answer a morphic world that is the current UI focus. This is the UI root animated by the active Process. This method could answer nil, if not in an UI process!


<details>
	<summary>See more</summary>
	
	runningWorld
	"Answer a morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!"

	^Processor activeProcess animatedUI
</details>

#### Object>>#with: arg1 with: arg2 with: arg3 executeMethod: compiledMethod

Execute compiledMethod against the receiver and arg1, arg2, & arg3


<details>
	<summary>See more</summary>
	
	with: arg1 with: arg2 with: arg3 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1, arg2, & arg3"

	<primitive: 189>
	^ self withArgs: {arg1. arg2. arg3} executeMethod: compiledMethod
</details>

#### Object>>#primitiveError: aString

This method is called when the error handling results in a recursion in calling on error: or halt or halt:.


<details>
	<summary>See more</summary>
	
	primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ `10@10` extent: (Display extent -20 min: `700@1000`).
	lines _ r height // AbstractFont default lineSpacing.
	emergencyEvaluator _ Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	context _ thisContext sender sender.
	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint
</details>

#### Object>>#when: anEventSelector evaluate: anAction

<details>
	<summary>See more</summary>
	
	when: anEventSelector evaluate: anAction 

	| actions |
	actions := self actionSequenceForEvent: anEventSelector.
	(actions includes: anAction)
		ifTrue: [^ self].
	self 
		setActionSequence: (actions copyWith: anAction)
		forEvent: anEventSelector
</details>

#### Object>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	"self is a shallow copy, subclasses should copy fields as necessary to complete the full copy"

	^ self
</details>

#### Object>>#instVarNamed: aString

Return the value of the instance variable in me with that name. Slow and unclean, but very useful.


<details>
	<summary>See more</summary>
	
	instVarNamed: aString
	"Return the value of the instance variable in me with that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asString)



</details>

#### Object>>#isFraction

Answer true if the receiver is a Fraction.


<details>
	<summary>See more</summary>
	
	isFraction
	"Answer true if the receiver is a Fraction."

	^ false
</details>

#### Object>>#assert: aBlock

Throw an assertion error if aBlock does not evaluates to true.


<details>
	<summary>See more</summary>
	
	assert: aBlock
	"Throw an assertion error if aBlock does not evaluates to true."

	aBlock value ifFalse: [AssertionFailure signal: 'Assertion failed']
</details>

#### Object>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil
</details>

#### Object>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	"Answer a name for the receiver.  This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems.  By default, we let the object just print itself out..  "

	^ self printString
</details>

#### Object>>#halt

This is the typical message to use for inserting breakpoints during debugging. It behaves like halt:, but does not call on halt: in order to avoid putting this message on the stack. Halt is especially useful when the breakpoint message is an arbitrary one.


<details>
	<summary>See more</summary>
	
	halt
	"This is the typical message to use for inserting breakpoints during 
	debugging. It behaves like halt:, but does not call on halt: in order to 
	avoid putting this message on the stack. Halt is especially useful when 
	the breakpoint message is an arbitrary one."

	Halt signal
</details>

#### Object>>#shouldNotHappenBecauseErrorMessage

<details>
	<summary>See more</summary>
	
	shouldNotHappenBecauseErrorMessage

	^self shouldNotHappenErrorMessage, ' because: '
</details>

#### Object>>#inline: inlineFlag

For translation only; noop when running in Smalltalk.


<details>
	<summary>See more</summary>
	
	inline: inlineFlag
	"For translation only; noop when running in Smalltalk."
</details>

#### Object>>#customizeExplorerContents

<details>
	<summary>See more</summary>
	
	customizeExplorerContents

	^ false.

</details>

#### Object>>#withoutListWrapper

<details>
	<summary>See more</summary>
	
	withoutListWrapper

	^self
</details>

#### Object>>#adaptToFraction: rcvr andSend: selector

If no method has been provided for adapting an object to a Fraction, then it may be adequate to simply adapt it to a number.


<details>
	<summary>See more</summary>
	
	adaptToFraction: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Fraction,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
</details>

#### Object>>#inspectWithLabel: aLabel

Create and schedule an Inspector in which the user can examine the receiver's variables.


<details>
	<summary>See more</summary>
	
	inspectWithLabel: aLabel
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	self inspectorClass openOn: self withLabel: aLabel
</details>

#### Object>>#when: anEventSelector
send: aMessageSelector
to: anObject

<details>
	<summary>See more</summary>
	
	when: anEventSelector
send: aMessageSelector
to: anObject
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector)
</details>

#### Object>>#inform: aString

Display a message for the user to read and then dismiss. 6/9/96 sw


<details>
	<summary>See more</summary>
	
	inform: aString
	"Display a message for the user to read and then dismiss. 6/9/96 sw"

	aString isEmptyOrNil ifFalse: [PopUpMenu inform: aString]
</details>

#### Object>>#= anObject

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= anObject 
	"Answer whether the receiver and the argument represent the same 
	object. If = is redefined in any subclass, consider also redefining the 
	message hash."

	^self == anObject
</details>

#### Object>>#copyAs: aSimilarClass

Answer an object of class aSimilarClass that has similar contents to the receiver. Always answer a new object, even if of same class as receiver


<details>
	<summary>See more</summary>
	
	copyAs: aSimilarClass
	"Answer an object of class aSimilarClass that has similar contents to the receiver.
	Always answer a new object, even if of same class as receiver"

	^ aSimilarClass newFrom: self
</details>

#### Object>>#mustBeBooleanIn: context

context is the where the non-boolean error occurred. Rewind context to before jump then raise error.


<details>
	<summary>See more</summary>
	
	mustBeBooleanIn: context
	"context is the where the non-boolean error occurred. Rewind context to before jump then raise error."

	| proceedValue |
	context skipBackBeforeJump.
	proceedValue _ NonBooleanReceiver new
		object: self;
		signal: 'proceed for truth.'.
	^ proceedValue ~~ false
</details>

#### Object>>#fullPrintString

Answer a String whose characters are a description of the receiver.


<details>
	<summary>See more</summary>
	
	fullPrintString
	"Answer a String whose characters are a description of the receiver."

	^String streamContents: [ :s | self printOn: s]
</details>

#### Object>>#shouldNotHappenErrorMessage

<details>
	<summary>See more</summary>
	
	shouldNotHappenErrorMessage

	^'Should not happen'
</details>

#### Object>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Append to the argument aStream a sequence of characters that is an 
	expression whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'

</details>

#### Object>>#instVarNamed: aString put: aValue

Store into the value of the instance variable in me of that name. Slow and unclean, but very useful.


<details>
	<summary>See more</summary>
	
	instVarNamed: aString put: aValue
	"Store into the value of the instance variable in me of that name.  Slow and unclean, but very useful. "

	^ self instVarAt: (self class allInstVarNames indexOf: aString asString) put: aValue

</details>

#### Object>>#removeDependent: anObject

Remove the given object as one of the receiver's dependents.


<details>
	<summary>See more</summary>
	
	removeDependent: anObject
	"Remove the given object as one of the receiver's dependents."

	self 
		removeActionsWithReceiver: anObject
		forEvent: #changed:.
	^ anObject
</details>

#### Object>>#removeActionsForEvent: anEventSelector

<details>
	<summary>See more</summary>
	
	removeActionsForEvent: anEventSelector

	| map |
	map _ self actionMap.
	map ifNotNil: [
		map removeKey: anEventSelector asSymbol ifAbsent: nil.
		map isEmpty
			ifTrue: [ self releaseActionMap ]]
</details>

#### Object>>#longPrintString

Answer a String whose characters are a description of the receiver.


<details>
	<summary>See more</summary>
	
	longPrintString
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str _ String streamContents: [:aStream | self longPrintOn: aStream].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]
</details>

#### Object>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^TextEditor
</details>

#### Object>>#rawBasicAt: index put: value

A verbatim copy of #basicAt:put: To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired. For example, when studying Floats, and understanding FFI, etc.


<details>
	<summary>See more</summary>
	
	rawBasicAt: index put: value 
	"A verbatim copy of #basicAt:put:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#primitiveFail

#primitiveFail may be invoked by certain methods whose code is translated in C. In such a case #primitiveFail and not #primitiveFailed should be invoked. The reason is that this code is translated to C by VMMaker. #primitiveFail is implemented in Interpreter of VMMaker.


<details>
	<summary>See more</summary>
	
	primitiveFail
	"#primitiveFail may be invoked by certain methods whose code is translated in C.
	In such a case #primitiveFail and not #primitiveFailed should be invoked.
	The reason is that this code is translated to C by VMMaker. #primitiveFail is implemented in Interpreter of VMMaker."

	^ self primitiveFailed
</details>

#### Object>>#finalize

Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority.


<details>
	<summary>See more</summary>
	
	finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	^self
</details>

#### Object>>#removeActionsWithReceiver: anObject
forEvent: anEventSelector

<details>
	<summary>See more</summary>
	
	removeActionsWithReceiver: anObject
forEvent: anEventSelector

    self
        removeActionsSatisfying:
            [:anAction |
            anAction receiver == anObject]
        forEvent: anEventSelector
</details>

#### Object>>#disableCode: aBlock

Use this method instead of commenting temporarily disabled code. This way, it will be accessible with senders, references to variables, etc. Besides, you'll avoid problems when the code to disable has comments!


<details>
	<summary>See more</summary>
	
	disableCode: aBlock
	"Use this method instead of commenting temporarily disabled code.
	This way, it will be accessible with senders, references to variables, etc.
	Besides, you'll avoid problems when the code to disable has comments!"
</details>

#### Object>>#setActionSequence: actionSequence
forEvent: anEventSelector

This is a good time to compact the action sequence of old, garbage collected stuff.


<details>
	<summary>See more</summary>
	
	setActionSequence: actionSequence
forEvent: anEventSelector

	| action |
	"This is a good time to compact the action sequence of old, garbage collected stuff."
	action := actionSequence asMinimalRepresentation.
	action
		ifNil: [ self removeActionsForEvent: anEventSelector]
		ifNotNil: [
			self updateableActionMap
				at: anEventSelector asSymbol
				put: action]
</details>

#### Object>>#storeString

Answer a String representation of the receiver from which the receiver can be reconstructed.


<details>
	<summary>See more</summary>
	
	storeString
	"Answer a String representation of the receiver from which the receiver 
	can be reconstructed."

	^ String streamContents: [:s | self storeOn: s]
</details>

#### Object>>#errorSubscriptBounds: index

Create an error notification that an improper integer was used as an index.


<details>
	<summary>See more</summary>
	
	errorSubscriptBounds: index 
	"Create an error notification that an improper integer was used as an index."

	self error: (self errorDescriptionForSubcriptBounds: index)
</details>

#### Object>>#at: index put: value

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: value 
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#respondsTo: aSymbol

Answer whether the method dictionary of the receiver's class contains aSymbol as a message selector.


<details>
	<summary>See more</summary>
	
	respondsTo: aSymbol 
	"Answer whether the method dictionary of the receiver's class contains 
	aSymbol as a message selector."

	^self class canUnderstand: aSymbol
</details>

#### Object>>#triggerEvent: anEventSelector with: anObject

<details>
	<summary>See more</summary>
	
	triggerEvent: anEventSelector with: anObject

    ^self 
		triggerEvent: anEventSelector
		withArguments: {anObject}
</details>

#### Object>>#doesNotUnderstand: aMessage

Handle the fact that there was an attempt to send the given message to the receiver but the receiver does not understand this message (typically sent from the machine when a message is sent to the receiver and no method is defined for that selector). Question: Why is this method different from the one inherited from ProtoObject? Answer (eem): This is intentional. Martin's reply is one half of the issue, that you want to be able to proceed after defining a method in the debugger. The other half is that you want to be able to catch doesNotUnderstand: in an exception handler and proceed with a result, e.g. [nil zork] on: MessageNotUnderstood do: [:ex| ex message selector == #zork ifTrue: [ex resume: #ok]. ex pass] evaluates to #ok. jmv adds: The real difference is what happens if the exception is eventually handled by the default handler (i.e. the debugger is opened). In that case, don't allow the user to proceed.


<details>
	<summary>See more</summary>
	
	doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given
	  message to the receiver but the receiver does not understand
	  this message (typically sent from the machine when a message
	 is sent to the receiver and no method is defined for that selector).
	
	Question: Why is this method different from the one inherited from ProtoObject?
	Answer (eem):
		This is intentional.  Martin's reply is one half of the issue, that you want
		to be able to proceed after defining a method in  the debugger.  The other
		half is that you want to be able to catch doesNotUnderstand: in an exception
		handler and proceed with a result, e.g.
	[nil zork]
		on: MessageNotUnderstood
		do: [:ex|
			ex message selector == #zork ifTrue:
				[ex resume: #ok].
			ex pass]

		evaluates to #ok.
		
	jmv adds:
		The real difference is what happens if the exception is eventually handled by the default handler
		(i.e. the debugger is opened). In that case, don't allow the user to proceed.
	"

	"Testing: 
		(3 activeProcess)
	"

	| exception resumeValue |
	(exception _ MessageNotUnderstood new)
		message: aMessage;
		receiver: self.
	resumeValue _ exception signal.
	^exception reachedDefaultHandler
		ifTrue: [ aMessage sentTo: self ]
		ifFalse: [ resumeValue ]
</details>

#### Object>>#errorDescriptionForSubcriptBounds: index

<details>
	<summary>See more</summary>
	
	errorDescriptionForSubcriptBounds: index

	^'subscript is out of bounds: ' , index printString
</details>

#### Object>>#perform: aSymbol with: firstObject with: secondObject with: thirdObject

Send the selector, aSymbol, to the receiver with the given arguments. Fail if the number of arguments expected by the selector is not three. Primitive. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject)
</details>

#### Object>>#errorImproperStore

Create an error notification that an improper store was attempted.


<details>
	<summary>See more</summary>
	
	errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'
</details>

#### Object>>#adaptToFloat: rcvr andSend: selector

If no method has been provided for adapting an object to a Float, then it may be adequate to simply adapt it to a number.


<details>
	<summary>See more</summary>
	
	adaptToFloat: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Float,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
</details>

#### Object>>#inboundPointers

Answers a collection of all objects in the system that point to myself


<details>
	<summary>See more</summary>
	
	inboundPointers
"Answers a collection of all objects in the system that point to myself"

	^ self inboundPointersExcluding: #()
</details>

#### Object>>#renameTo: newName

If the receiver has an inherent idea about its own name, it should take action here. Any object that might be pointed to in the References dictionary might get this message sent to it upon reload


<details>
	<summary>See more</summary>
	
	renameTo: newName
	"If the receiver has an inherent idea about its own name, it should take action here.  Any object that might be pointed to in the References dictionary might get this message sent to it upon reload"
</details>

#### Object>>#var: varSymbol declareC: declString

For translation only; noop when running in Smalltalk.


<details>
	<summary>See more</summary>
	
	var: varSymbol declareC: declString
	"For translation only; noop when running in Smalltalk."
</details>

#### Object>>#nominallyUnsent: aSelectorSymbol

From within the body of a method which is not formally sent within the system, but which you intend to have remain in the system (for potential manual invocation, or for documentation, or perhaps because it's sent by commented-out-code that you anticipate uncommenting out someday, send this message, with the selector itself as the argument. This will serve two purposes: (1) The method will not be returned by searches for unsent selectors (because it, in a manner of speaking, sends itself). (2) You can locate all such methods by browsing senders of #nominallyUnsent:


<details>
	<summary>See more</summary>
	
	nominallyUnsent: aSelectorSymbol
	"From within the body of a method which is not formally sent within the system, but which you intend to have remain in the system (for potential manual invocation, or for documentation, or perhaps because it's sent by commented-out-code that you anticipate uncommenting out someday, send this message, with the selector itself as the argument.

This will serve two purposes:

	(1)  The method will not be returned by searches for unsent selectors (because it, in a manner of speaking, sends itself).
	(2)	You can locate all such methods by browsing senders of #nominallyUnsent:"
	"(jmv) Correction: send the unsent symbol from elsewhere. If it is sent from whithin itself, it will still appear as unsent."

	false ifTrue: [self flag: #nominallyUnsent:]    "So that this method itself will appear to be sent"

</details>

#### Object>>#instVarAtPrim73: index

Primitive. Answer a fixed variable in an object. The numbering of the variables corresponds to the named instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	instVarAtPrim73: index
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	variables corresponds to the named instance variables. Fail if the index 
	is not an Integer or is not the index of a fixed variable. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize
</details>

#### Object>>#className

Answer a string characterizing the receiver's class, for use in list views for example


<details>
	<summary>See more</summary>
	
	className
	"Answer a string characterizing the receiver's class, for use in list views for example"

	^ self class name asString
</details>

#### Object>>#valueWithPossibleArgument: anArg

<details>
	<summary>See more</summary>
	
	valueWithPossibleArgument: anArg

	^self
</details>

#### Object>>#isKindOf: aClass

Answer whether the class, aClass, is a superclass or class of the receiver.


<details>
	<summary>See more</summary>
	
	isKindOf: aClass 
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]
</details>

#### Object>>#actionSequenceForEvent: anEventSelector

<details>
	<summary>See more</summary>
	
	actionSequenceForEvent: anEventSelector

	^self actionMap
		ifNotNil: [ :map |
			(map
				at: anEventSelector asSymbol
				ifAbsent: [ WeakActionSequence new ])
					asActionSequence ]
		ifNil: [ WeakActionSequence new ]
</details>

#### Object>>#storeAt: offset inTempFrame: aContext

This message had to get sent to an expression already on the stack as a Block argument being accessed by the debugger. Just re-route it to the temp frame.


<details>
	<summary>See more</summary>
	
	storeAt: offset inTempFrame: aContext
	"This message had to get sent to an expression already on the stack
	as a Block argument being accessed by the debugger.
	Just re-route it to the temp frame."
	^ aContext tempAt: offset put: self
</details>

#### Object>>#becomeForward: otherObject copyHash: copyHash

Primitive. All variables in the entire system that used to point to the receiver now point to the argument. If copyHash is true, the argument's identity hash bits will be set to those of the receiver. Fails if either argument is a SmallInteger.


<details>
	<summary>See more</summary>
	
	becomeForward: otherObject copyHash: copyHash
	"Primitive. All variables in the entire system that used to point to the receiver now point to the argument.
	If copyHash is true, the argument's identity hash bits will be set to those of the receiver.
	Fails if either argument is a SmallInteger."

	| newMethod oldMethod selector |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :proces :context |
				self == context receiver ifTrue: [
					selector _ context method selector.
					oldMethod _ self class lookupSelector: selector.
					newMethod _ otherObject class lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self }
		elementsForwardIdentityTo: { otherObject }
		copyHash: copyHash
</details>

#### Object>>#removeActionsWithReceiver: anObject

<details>
	<summary>See more</summary>
	
	removeActionsWithReceiver: anObject

	self actionMap ifNotNil: [ :map |
		map keys do: [ :eachEventSelector |
			self
   				removeActionsSatisfying: [:anAction | anAction receiver == anObject]
				forEvent: eachEventSelector ]]
</details>

#### Object>>#shallowCopy

Answer a copy of the receiver which shares the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Answer a copy of the receiver which shares the receiver's instance variables."
	| class newObject index |
	<primitive: 148>
	class _ self class.
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index).
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index).
			index _ index - 1].
	^ newObject
</details>

#### Object>>#isBehavior

Return true if the receiver is a behavior. Note: Do not override in any class except behavior.


<details>
	<summary>See more</summary>
	
	isBehavior
	"Return true if the receiver is a behavior.
	Note: Do not override in any class except behavior."
	^false
</details>

#### Object>>#perform: selector withPossiblyWrongSizedArguments: argArray

Fix arguments size if needed


<details>
	<summary>See more</summary>
	
	perform: selector withPossiblyWrongSizedArguments: argArray
	"Fix arguments size if needed"

	| arguments numArgs |
	numArgs _ selector numArgs.
	numArgs = argArray size
		ifTrue: [
			arguments _ argArray ]
		ifFalse: [
			arguments _ Array new: numArgs.
			arguments fillWith: argArray ].
	^ self perform: selector withArguments: arguments 
</details>

#### Object>>#removeActionsSatisfying: aOneArgBlock forEvent: anEventSelector

<details>
	<summary>See more</summary>
	
	removeActionsSatisfying: aOneArgBlock forEvent: anEventSelector
	self
		setActionSequence:
			((self actionSequenceForEvent: anEventSelector)
				reject: aOneArgBlock)
		forEvent: anEventSelector
</details>

#### Object>>#isComplex

Answer true if receiver is a Complex number. False by default.


<details>
	<summary>See more</summary>
	
	isComplex
	"Answer true if receiver is a Complex number. False by default."
	^ false
</details>

#### Object>>#executeMethod: compiledMethod

Execute compiledMethod against the receiver with no args


<details>
	<summary>See more</summary>
	
	executeMethod: compiledMethod
	"Execute compiledMethod against the receiver with no args"

	<primitive: 189>
	^ self withArgs: #() executeMethod: compiledMethod
</details>

#### Object>>#triggerEvent: anEventSelector

Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action.


<details>
	<summary>See more</summary>
	
	triggerEvent: anEventSelector
	"Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action."

	^(self actionForEvent: anEventSelector) value
</details>

#### Object>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	May be overridden, and should be overridden in any classes that define = "

	^ self identityHash
</details>

#### Object>>#releaseActionMap

<details>
	<summary>See more</summary>
	
	releaseActionMap

	ActiveModel releaseActionMapFor: self
</details>

#### Object>>#isInterval

<details>
	<summary>See more</summary>
	
	isInterval

	^ false
</details>

#### Object>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [self errorSubscriptBounds: index]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#print

<details>
	<summary>See more</summary>
	
	print
	Transcript show: self printString; newLine
</details>

#### Object>>#addDependent: anObject

Make the given object one of the receiver's dependents.


<details>
	<summary>See more</summary>
	
	addDependent: anObject
	"Make the given object one of the receiver's dependents."

	self
		when: #changed:
		send: #update:
		to: anObject.
	^anObject
</details>

#### Object>>#removeAction: anAction
forEvent: anEventSelector

<details>
	<summary>See more</summary>
	
	removeAction: anAction
forEvent: anEventSelector

    self
        removeActionsSatisfying: [:action | action = anAction]
        forEvent: anEventSelector
</details>

#### Object>>#primitiveFailed

Announce that a primitive has failed and there is no appropriate Smalltalk code to run.


<details>
	<summary>See more</summary>
	
	primitiveFailed
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self primitiveFailed: thisContext sender selector
</details>

#### Object>>#changed: aParameter

Receiver changed. The change is denoted by the argument aParameter. Usually the argument is a Symbol that is part of the dependent's change protocol. Inform all of the dependents.


<details>
	<summary>See more</summary>
	
	changed: aParameter 
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

	self 
		triggerEvent: #changed:
		with: aParameter
</details>

#### Object>>#browseClassHierarchy

<details>
	<summary>See more</summary>
	
	browseClassHierarchy
	| targetClass newBrowser |
	targetClass := self class isMeta
				ifTrue: [self class theNonMetaClass]
				ifFalse: [self class ].
	newBrowser _ HierarchyBrowser for: targetClass.
	HierarchyBrowserWindow openNoSysCat: newBrowser label: nil.
	newBrowser assureSelectionsShow
</details>

#### Object>>#valueWithArguments: aSequenceOfArguments

<details>
	<summary>See more</summary>
	
	valueWithArguments: aSequenceOfArguments

	^self
</details>

#### Object>>#notYetImplemented

<details>
	<summary>See more</summary>
	
	notYetImplemented
	NotYetImplemented signal
</details>

#### Object>>#isPseudoContext

<details>
	<summary>See more</summary>
	
	isPseudoContext
	^false
</details>

#### Object>>#-> anObject

Answer an Association between self and anObject


<details>
	<summary>See more</summary>
	
	-> anObject
	"Answer an Association between self and anObject"

	^Association key: self value: anObject
</details>

#### Object>>#comeFullyUpOnReload: smartRefStream

Normally this read-in object is exactly what we want to store. 7/26/96 tk


<details>
	<summary>See more</summary>
	
	comeFullyUpOnReload: smartRefStream
	"Normally this read-in object is exactly what we want to store. 7/26/96 tk"

	^ self
</details>

#### Object>>#primitiveChangeClassTo: anObject

Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or the receiver is an instance of a compact class and the argument isn't, or when the argument's class is compact and the receiver isn't, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have. Note: The primitive will fail in most cases that you think might work. This is mostly because of a) the difference between compact and non-compact classes, and b) because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for three of the reasons mentioned above. Array is compact, Morph is not (failure #1). Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3). The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use.


<details>
	<summary>See more</summary>
	
	primitiveChangeClassTo: anObject
	"Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or the receiver is an instance of a compact class and the argument isn't, or when the argument's class is compact and the receiver isn't, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have.
	Note: The primitive will fail in most cases that you think might work. This is mostly because of a) the difference between compact and non-compact classes, and b) because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for three of the reasons mentioned above. Array is compact, Morph is not (failure #1). Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3).
	The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use."

	<primitive: 115>
	self primitiveFailed
</details>

#### Object>>#isClosure

<details>
	<summary>See more</summary>
	
	isClosure
	^false
</details>

#### Object>>#convertToCurrentVersion: varDict refStream: smartRefStrm

subclasses should implement if they wish to convert old instances to modern ones


<details>
	<summary>See more</summary>
	
	convertToCurrentVersion: varDict refStream: smartRefStrm

	"subclasses should implement if they wish to convert old instances to modern ones"
</details>

#### Object>>#printString

Answer a String whose characters are a description of the receiver. If you want to print without a character limit, use fullPrintString. This description is to be meaningful for a Smalltalk programmer and usually includes a hint on the class of the object. Usually you should not reimplement this method in subclasses, but #printOn: See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 50000
</details>

#### Object>>#copySameFrom: otherObject

Copy to myself all instance variables named the same in otherObject. This ignores otherObject's control over its own inst vars.


<details>
	<summary>See more</summary>
	
	copySameFrom: otherObject
	"Copy to myself all instance variables named the same in otherObject.
	This ignores otherObject's control over its own inst vars."

	| myInstVars otherInstVars |
	myInstVars := self class allInstVarNames.
	otherInstVars := otherObject class allInstVarNames.
	myInstVars withIndexDo: [:each :index | | match |
		(match := otherInstVars indexOf: each) > 0 ifTrue:
			[self instVarAt: index put: (otherObject instVarAt: match)]].
	1 to: (self basicSize min: otherObject basicSize) do: [:i |
		self basicAt: i put: (otherObject basicAt: i)].

</details>

#### Object>>#bindingOf: aString

<details>
	<summary>See more</summary>
	
	bindingOf: aString
	^nil
</details>

#### Object>>#instVarAt: index put: anObject

Primitive. Store a value into a fixed variable in an object. The numbering of the variables corresponds to the named instance variables, followed by the indexed instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	instVarAt: index put: anObject
	"Primitive. Store a value into a fixed variable in an object. The numbering of the
	 variables corresponds to the named instance variables, followed by the indexed
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed
	 variable. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim74: index put: anObject ].
	self primitiveFailed
</details>

#### Object>>#isNumber

Overridden to return true in Number, natch


<details>
	<summary>See more</summary>
	
	isNumber
	"Overridden to return true in Number, natch"
	^ false
</details>

#### Object>>#longPrintStringLimitedTo: aLimitValue

Answer a String whose characters are a description of the receiver.


<details>
	<summary>See more</summary>
	
	longPrintStringLimitedTo: aLimitValue
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str := String streamContents: [:aStream | self longPrintOn: aStream limitedTo: aLimitValue indent: 0].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]
</details>

#### Object>>#veryDeepCopy

Do a full copy of an object graph. Some classes might chose not to include some ivars. For example, aMorph owner is only included 'wakely'. See #storeDataOn:, and in particular Morph>>storeDataOn: This method is included for comatibility with other Smalltalks and for reference. Each class should decide how to copy its instances. For example, Morph>>copy. Implementing #copy appropriately, and just calling it is better style than calling #veryDeepCopy (except, maybe, in low level, system code)


<details>
	<summary>See more</summary>
	
	veryDeepCopy
	"Do a full copy of an object graph.
	Some classes might chose not to include some ivars.
	For example, aMorph owner is only included 'wakely'.
	See #storeDataOn:, and in particular Morph>>storeDataOn:
	
	This method is included for comatibility with other Smalltalks and for reference.
	Each class should decide how to copy its instances.
	For example, Morph>>copy.
	Implementing #copy appropriately, and just calling it is better style than calling #veryDeepCopy (except, maybe, in low level, system code)"

	^ Object unStream: (ReferenceStream streamedRepresentationOf: self)
</details>

#### Object>>#basicAt: index put: value

Primitive. Assumes receiver is indexable. Store the second argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. Do not override in a subclass. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#adaptToInteger: rcvr andSend: selector

If no method has been provided for adapting an object to a Integer, then it may be adequate to simply adapt it to a number.


<details>
	<summary>See more</summary>
	
	adaptToInteger: rcvr andSend: selector
	"If no method has been provided for adapting an object to a Integer,
	then it may be adequate to simply adapt it to a number."
	^ self adaptToNumber: rcvr andSend: selector
</details>

#### Object>>#literalEqual: other

<details>
	<summary>See more</summary>
	
	literalEqual: other

	^ self class == other class and: [self = other]
</details>

#### Object>>#as: aSimilarClass

Answer an object of class aSimilarClass that has similar contents to the receiver.


<details>
	<summary>See more</summary>
	
	as: aSimilarClass
	"Answer an object of class aSimilarClass that has similar contents to the receiver."

	^ self class == aSimilarClass ifTrue: [self] ifFalse: [aSimilarClass newFrom: self]
</details>

#### Object>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	aStream
		nextPutAll: self class name withArticle
</details>

#### Object>>#subclassResponsibility

This message sets up a framework for the behavior of the class' subclasses. Announce that the subclass should have implemented this message.


<details>
	<summary>See more</summary>
	
	subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass should have overridden ', thisContext sender selector printString
</details>

#### Object>>#shouldBeImplemented

Announce that this message should be implemented


<details>
	<summary>See more</summary>
	
	shouldBeImplemented
	"Announce that this message should be implemented"

	self error: 'This message should be implemented'
</details>

#### Object>>#caseError

Report an error from an in-line or explicit case statement.


<details>
	<summary>See more</summary>
	
	caseError
	"Report an error from an in-line or explicit case statement."

	self error: 'Case not found, and no otherwise clause'
</details>

#### Object>>#retryWithGC: execBlock until: testBlock

Retry execBlock as long as testBlock returns false. Do an incremental GC after the first try, a full GC after the second try.


<details>
	<summary>See more</summary>
	
	retryWithGC: execBlock until: testBlock
	"Retry execBlock as long as testBlock returns false. Do an incremental GC after the first try, a full GC after the second try."
	| blockValue |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.
</details>

#### Object>>#unpin

The VM's garbage collector routinely moves objects as it reclaims and compacts memory. But it can also pin an object so that it will not be moved, which can make it easier to pass objects out through the FFI. Objects are unpinnned when created. This method ensures an object is unpinned, and answers whether it was pinned.


<details>
	<summary>See more</summary>
	
	unpin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is unpinned, and answers whether it was pinned."
	^self setPinned: false
</details>

#### Object>>#actionForEvent: anEventSelector

Answer the action to be evaluated when <anEventSelector> has been triggered.


<details>
	<summary>See more</summary>
	
	actionForEvent: anEventSelector
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	^self actionMap ifNotNil: [ :map |
		map
			at: anEventSelector asSymbol
			ifAbsent: nil]
</details>

#### Object>>#isSymbol

<details>
	<summary>See more</summary>
	
	isSymbol
	^ false 
</details>

#### Object>>#isMethodProperties

<details>
	<summary>See more</summary>
	
	isMethodProperties
	^false
</details>

#### Object>>#~= anObject

Answer whether the receiver and the argument do not represent the same object.


<details>
	<summary>See more</summary>
	
	~= anObject 
	"Answer whether the receiver and the argument do not represent the 
	same object."

	^self = anObject == false
</details>

#### Object>>#errorNonIntegerIndex

Create an error notification that an improper object was used as an index.


<details>
	<summary>See more</summary>
	
	errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'
</details>

#### Object>>#isFloatOrFloatComplex

Overridden to return true in Float and Complex


<details>
	<summary>See more</summary>
	
	isFloatOrFloatComplex
	"Overridden to return true in Float and Complex"
	^ false
</details>

#### Object>>#when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg

<details>
	<summary>See more</summary>
	
	when: anEventSelector
send: aMessageSelector
to: anObject
with: anArg
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: (Array with: anArg))
</details>

#### Object>>#shouldNotHappenBecause: aReason

Used to announce that something that should not happen by design, happened. For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappenBecause: 'The array has one element']. See #shouldNotHappen also


<details>
	<summary>See more</summary>
	
	shouldNotHappenBecause: aReason
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappenBecause: 'The array has one element'].
	See #shouldNotHappen also"
	
	self error: self shouldNotHappenBecauseErrorMessage, aReason
</details>

#### Object>>#isPoint

Overridden to return true in Point.


<details>
	<summary>See more</summary>
	
	isPoint
	"Overridden to return true in Point."

	^ false
</details>

#### Object>>#printText

<details>
	<summary>See more</summary>
	
	printText
	^ Text streamContents: [:aStream| self printOn: aStream]
</details>

#### Object>>#class

Primitive. Answer the object which is the receiver's class. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	class
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	self primitiveFailed
</details>

#### Object>>#perform: aSymbol

Send the unary selector, aSymbol, to the receiver. Fail if the number of arguments expected by the selector is not zero. Primitive. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)
</details>

#### Object>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class
</details>

#### Object>>#complexContents

<details>
	<summary>See more</summary>
	
	complexContents

	^self
</details>

#### Object>>#haltOnce

Halt unless we have already done it once.


<details>
	<summary>See more</summary>
	
	haltOnce
	"Halt unless we have already done it once."
	
	self haltOnceEnabled ifTrue: [
		self clearHaltOnce.
		^ self halt
	]
</details>

#### Object>>#hasActionForEvent: anEventSelector

Answer true if there is an action associated with anEventSelector


<details>
	<summary>See more</summary>
	
	hasActionForEvent: anEventSelector
    "Answer true if there is an action associated with anEventSelector"

    ^(self actionForEvent: anEventSelector) notNil
</details>

#### Object>>#pin

The VM's garbage collector routinely moves objects as it reclaims and compacts memory. But it can also pin an object so that it will not be moved, which can make it easier to pass objects out through the FFI. Objects are unpinnned when created. This method ensures an object is pinned, and answers whether it was already pinned.


<details>
	<summary>See more</summary>
	
	pin
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This method ensures an object is pinned, and answers whether it was already pinned."
	^self setPinned: true
</details>

#### Object>>#isBlock

<details>
	<summary>See more</summary>
	
	isBlock

	^ false
</details>

#### Object>>#mustBeBoolean

Catches attempts to test truth of non-Booleans. This message is sent from the VM. The sending context is rewound to just before the jump causing this exception.


<details>
	<summary>See more</summary>
	
	mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the VM.  The sending context is rewound to just before the jump causing this exception."

	^ self mustBeBooleanIn: thisContext sender
</details>

#### Object>>#when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray

<details>
	<summary>See more</summary>
	
	when: anEventSelector
send: aMessageSelector
to: anObject
withArguments: anArgArray
 
    self
        when: anEventSelector
        evaluate: (WeakMessageSend
            receiver: anObject
            selector: aMessageSelector
		arguments: anArgArray)
</details>

#### Object>>#isFloat

Overridden to return true in Float, natch


<details>
	<summary>See more</summary>
	
	isFloat
	"Overridden to return true in Float, natch"
	^ false
</details>

#### Object>>#argumentNameSufix

<details>
	<summary>See more</summary>
	
	argumentNameSufix

	^self class isMeta ifTrue: ['Class'] ifFalse: [self class name]
</details>

#### Object>>#printAs: streamType limitedTo: limit

Answer an instance of streamType whose characters describe the receiver. If you want to print without a character limit, use fullPrintString.


<details>
	<summary>See more</summary>
	
	printAs: streamType limitedTo: limit
	"Answer an instance of streamType whose characters describe the receiver.
	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString _ streamType streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '[..]'
</details>

#### Object>>#copyToClipboard

<details>
	<summary>See more</summary>
	
	copyToClipboard

	Clipboard storeObject: self
</details>

#### Object>>#inboundPointersExcluding: objectsToExclude

Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts.


<details>
	<summary>See more</summary>
	
	inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts."

	| allObjectsToExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.

	allObjectsToExclude _ { thisContext }, objectsToExclude.
	^Smalltalk allObjects select: [ :object |
		object isInMemory and: [
		((object statePointsTo: self) or: [object class == self]) and: [
		(allObjectsToExclude statePointsTo: object) not ]]
		]
</details>

#### Object>>#with: arg1 with: arg2 with: arg3 with: arg4 executeMethod: compiledMethod

Execute compiledMethod against the receiver and arg1, arg2, arg3, & arg4


<details>
	<summary>See more</summary>
	
	with: arg1 with: arg2 with: arg3 with: arg4 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1, arg2, arg3, & arg4"

	<primitive: 189>
	^ self withArgs: {arg1. arg2. arg3. arg4} executeMethod: compiledMethod
</details>

#### Object>>#printWithClosureAnalysisOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	aStream
		nextPutAll: self class name withArticle
</details>

#### Object>>#setHaltOnce

Turn on the halt once flag.


<details>
	<summary>See more</summary>
	
	setHaltOnce
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: true
</details>

#### Object>>#errorNotIndexable

Create an error notification that the receiver is not indexable.


<details>
	<summary>See more</summary>
	
	errorNotIndexable
	"Create an error notification that the receiver is not indexable."

	self error: ('Instances of {1} are not indexable' format: {self class name})
</details>

#### Object>>#isPinned

Answer if the receiver is pinned. The VM's garbage collector routinely moves objects as it reclaims and compacts memory. But it can also pin an object so that it will not be moved, which can make it easier to pass objects out through the FFI.


<details>
	<summary>See more</summary>
	
	isPinned
	"Answer if the receiver is pinned.  The VM's garbage collector routinely moves
	 objects as it reclaims and compacts memory.  But it can also pin an object so
	 that it will not be moved, which can make it easier to pass objects out through
	 the FFI."
	<primitive: 183 error: ec>
	^self primitiveFailed
</details>

#### Object>>#someObject

Primitive. Answer the first object in the enumeration of all objects.


<details>
	<summary>See more</summary>
	
	someObject
	"Primitive. Answer the first object in the enumeration of all
	 objects."

	<primitive: 138>
	self primitiveFailed.
</details>

#### Object>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ Inspector
</details>

#### Object>>#longPrintOn: aStream

Append to the argument, aStream, the names and values of all of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all 
	of the receiver's instance variables."

	self class allInstVarNames withIndexDo: [ :title :index |
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 print: (self instVarAt: index);
		 newLine]
</details>

#### Object>>#update: aParameter

Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing; a subclass might want to change itself in some way.


<details>
	<summary>See more</summary>
	
	update: aParameter 
	"Receive a change notice from an object of whom the receiver is a 
	dependent. The default behavior is to do nothing; a subclass might want 
	to change itself in some way."

	^self
</details>

#### Object>>#perform: aSymbol with: anObject

Send the selector, aSymbol, to the receiver with anObject as its argument. Fail if the number of arguments expected by the selector is not one. Primitive. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)
</details>

#### Object>>#activeHand

Answer a hand for the morphic world that is the current UI focus. This is the UI root animated by the active Process. This method could answer nil, if not in an UI process!


<details>
	<summary>See more</summary>
	
	activeHand
	"Answer a hand for the morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!"

	^self runningWorld ifNotNil: [ :w | w activeHand ]
</details>

#### Object>>#inspect

Create and schedule an Inspector in which the user can examine the receiver's variables.


<details>
	<summary>See more</summary>
	
	inspect
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	self inspectorClass openOn: self
</details>

#### Object>>#isContext

<details>
	<summary>See more</summary>
	
	isContext

	^false 
</details>

#### Object>>#withArgs: argArray executeMethod: compiledMethod

Execute compiledMethod against the receiver and args in argArray


<details>
	<summary>See more</summary>
	
	withArgs: argArray executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and args in argArray"

	| selector |
	<primitive: 188>
	selector _ Symbol new.
	self class addSelectorSilently: selector withMethod: compiledMethod.
	^ [self perform: selector withArguments: argArray]
		ensure: [self class basicRemoveSelector: selector]
</details>

#### Object>>#objectForDataStream: refStrm

Return an object to store on an external data stream.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
    "Return an object to store on an external data stream."

    ^ self
</details>

#### Object>>#printStringLimitedTo: limit

Answer a String whose characters are a description of the receiver. If you want to print without a character limit, use fullPrintString.


<details>
	<summary>See more</summary>
	
	printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit
</details>

#### Object>>#isString

Overridden to return true in String, natch


<details>
	<summary>See more</summary>
	
	isString
	"Overridden to return true in String, natch"
	^ false
</details>

#### Object>>#with: arg1 with: arg2 executeMethod: compiledMethod

Execute compiledMethod against the receiver and arg1 & arg2


<details>
	<summary>See more</summary>
	
	with: arg1 with: arg2 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1 & arg2"

	<primitive: 189>
	^ self withArgs: {arg1. arg2} executeMethod: compiledMethod
</details>

#### Object>>#instVarAt: index

Primitive. Answer a fixed variable in an object. The numbering of the variables corresponds to the named instance variables, followed by the indexed instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	instVarAt: index
	"Primitive. Answer a fixed variable in an object. The numbering of the variables
	 corresponds to the named instance variables, followed by the indexed instance
	 variables. Fail if the index is not an Integer or is not the index of a fixed variable.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim73: index ].
	self primitiveFailed
</details>

#### Object>>#profilerFriendlyCall: aBlock

In AndreasSystemProfiler, usually primitives are reported as children of the wrong node. The reason is that while the primitive is recorded, it is only taken into account at next suspension point. For long running primitives, and maybe only while profiling, this method provides a workaround. Evaluate and compare AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]]. AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]]. Also see #runProfilerProcess, and this more complex example (you might want to make it use #profilerFriendlyCall:) AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]] Keep in mind there is a performance penaly each time this method is called. Consider doing it only for long-running primitives, or only while profiling code.


<details>
	<summary>See more</summary>
	
	profilerFriendlyCall: aBlock
	"In AndreasSystemProfiler, usually primitives are reported as children of the wrong node.
	The reason is that while the primitive is recorded, it is only taken into account at next suspension point.
	
	For long running primitives, and maybe only while profiling, this method provides a workaround.
	Evaluate and compare

		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	Also see #runProfilerProcess, and this more complex example (you might want to make it use #profilerFriendlyCall:)
		AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]

	Keep in mind there is a performance penaly each time this method is called.
	Consider doing it only for long-running primitives, or only while profiling code.
	"

	"The apparently useless loop is to ensure we have a suspension point in this method, after block evaluation.
	Suspension points are activation of message sends and bytecode back jumps."
	| primResult primWasCalled |
	primWasCalled _ false.
	[ primWasCalled ] whileFalse: [
		primResult _ aBlock value.
		primWasCalled _ true ].
	^primResult
</details>

#### Object>>#confirm: aString orCancel: cancelBlock

Put up a yes/no/cancel menu with caption aString. Answer true if the response is yes, false if no. If cancel is chosen, evaluate cancelBlock. This is a modal question--the user must respond yes or no.


<details>
	<summary>See more</summary>
	
	confirm: aString orCancel: cancelBlock
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	^ PopUpMenu confirm: aString orCancel: cancelBlock
</details>

#### Object>>#finalizationRegistry

Answer the finalization registry associated with the receiver.


<details>
	<summary>See more</summary>
	
	finalizationRegistry
	"Answer the finalization registry associated with the receiver."
	^WeakRegistry default
</details>

#### Object>>#shouldNotImplement

Announce that, although the receiver inherits this message, it should not implement it.


<details>
	<summary>See more</summary>
	
	shouldNotImplement
	"Announce that, although the receiver inherits this message, it should 
	not implement it."

	self error: 'This message is not appropriate for this object'
</details>

#### Object>>#argumentName

<details>
	<summary>See more</summary>
	
	argumentName
	| name |
	name _ self argumentNameSufix.
	^name article, name
</details>

#### Object>>#flash

Do nothing.


<details>
	<summary>See more</summary>
	
	flash
	"Do nothing."

</details>

#### Object>>#haltOnceEnabled

<details>
	<summary>See more</summary>
	
	haltOnceEnabled

	^ Smalltalk
		at: #HaltOnce
		ifAbsent: [false]
</details>

#### Object>>#assert: aBlock description: aStringOrBlock

Throw an assertion error if aBlock does not evaluates to true.


<details>
	<summary>See more</summary>
	
	assert: aBlock description: aStringOrBlock
    "Throw an assertion error if aBlock does not evaluates to true."

    aBlock value 
        ifFalse: [ AssertionFailure signal: aStringOrBlock value ]
</details>

#### Object>>#deprecatedMethod

Warn that this method is deprecated and should not be used


<details>
	<summary>See more</summary>
	
	deprecatedMethod
	"Warn that this method is deprecated and should not be used"

	'========' print.
	thisContext sender print.
	'--------------' print.
	'This method is deprecated. It will be removed from the system. Please change this and any other related senders.' print.
	'--------------' print.
	thisContext sender printStack: 6.
	'========' print.
</details>

#### Object>>#rawBasicAt: index

A verbatim copy of #basicAt: To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired. For example, when studying Floats, and understanding FFI, etc.


<details>
	<summary>See more</summary>
	
	rawBasicAt: index 
	"A verbatim copy of #basicAt:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#shouldNotHappen

Used to announce that something that should not happen by design, happened. For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappen]. See #shouldNotHappenBecause: also


<details>
	<summary>See more</summary>
	
	shouldNotHappen
	"Used to announce that something that should not happen by design, happened. 
	For example: (Array with: 1) at: 1 ifAbsent: [self shouldNotHappen].
	See #shouldNotHappenBecause: also"
	
	self error: self shouldNotHappenErrorMessage
</details>

#### Object>>#with: arg1 executeMethod: compiledMethod

Execute compiledMethod against the receiver and arg1


<details>
	<summary>See more</summary>
	
	with: arg1 executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and arg1"

	<primitive: 189>
	^ self withArgs: {arg1} executeMethod: compiledMethod
</details>

#### Object>>#actionMap

<details>
	<summary>See more</summary>
	
	actionMap

	^ActiveModel actionMapFor: self
</details>

#### Object>>#textStylerClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	textStylerClassFor: textGetter
	"Enable any object to be the textProvider for a PluggableTextModel"
	^nil
</details>

#### Object>>#basicSize

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. Do not override in any subclass. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0	
</details>

#### Object>>#copy

Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy.


<details>
	<summary>See more</summary>
	
	copy
	"Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy."

	^self shallowCopy postCopy
</details>

#### Object>>#revisar

<details>
	<summary>See more</summary>
	
	revisar
	self flag: #jmv
</details>

#### Object>>#triggerEvent: anEventSelector withArguments: anArgumentList

<details>
	<summary>See more</summary>
	
	triggerEvent: anEventSelector withArguments: anArgumentList

	^ (self actionForEvent: anEventSelector)
		valueWithArguments: anArgumentList
</details>

#### Object>>#outboundPointersDo: aBlock

do aBlock for every object I point to, exactly how the garbage collector would. Adapted from PointerFinder >> #followObject:


<details>
	<summary>See more</summary>
	
	outboundPointersDo: aBlock
"do aBlock for every object I point to, exactly how the garbage collector would. Adapted from PointerFinder >> #followObject:"

	aBlock value: self class.
	1 to: self class instSize do: [:i | aBlock value: (self instVarAt: i)].
	1 to: self basicSize do: [:i | aBlock value: (self basicAt: i)].
</details>

#### Object>>#caseOf: aBlockAssociationCollection otherwise: aBlock

The elements of aBlockAssociationCollection are associations between blocks. Answer the evaluated value of the first association in aBlockAssociationCollection whose evaluated key equals the receiver. If no match is found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	caseOf: aBlockAssociationCollection otherwise: aBlock
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, answer the result
	 of evaluating aBlock."

	aBlockAssociationCollection associationsDo:
		[:assoc | (assoc key value = self) ifTrue: [^assoc value value]].
	^ aBlock value

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z otherwise: [0]"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]} otherwise: [0]"
</details>

#### Object>>#isInteger

Overridden to return true in Integer.


<details>
	<summary>See more</summary>
	
	isInteger
	"Overridden to return true in Integer."

	^ false
</details>

#### Object>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false
</details>

#### Object>>#outboundPointers

Answers a list of all objects I am causing not to be garbage-collected


<details>
	<summary>See more</summary>
	
	outboundPointers
"Answers a list of all objects I am causing not to be garbage-collected"

	| collection |
	collection := OrderedCollection new.
	self outboundPointersDo: [:ea | collection add: ea].
	^ collection
</details>

#### Object>>#updateableActionMap

<details>
	<summary>See more</summary>
	
	updateableActionMap

	^ActiveModel updateableActionMapFor: self
</details>

#### Object>>#basicAt: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. Do not override in a subclass. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Object>>#perform: selector withArguments: argArray inSuperclass: lookupClass

NOTE: This is just like perform:withArguments:, except that the message lookup process begins, not with the receivers's class, but with the supplied superclass instead. It will fail if lookupClass cannot be found among the receiver's superclasses. Primitive. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100>
	(selector isMemberOf: Symbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self primitiveFailed
</details>

#### Object>>#copyForClipboard

Some subclasses might need specific behavior...


<details>
	<summary>See more</summary>
	
	copyForClipboard
	"Some subclasses might need specific behavior..."
	^self copy
</details>

#### Object>>#breakDependents

Remove all of the receiver's dependents.


<details>
	<summary>See more</summary>
	
	breakDependents
	"Remove all of the receiver's dependents."

	self removeActionsForEvent: #changed:
</details>

#### Object>>#copyFrom: anotherObject

Copy to myself all instance variables I have in common with anotherObject. This is dangerous because it ignores an object's control over its own inst vars.


<details>
	<summary>See more</summary>
	
	copyFrom: anotherObject
	"Copy to myself all instance variables I have in common with anotherObject.  This is dangerous because it ignores an object's control over its own inst vars.  "

	| mine his |
	<primitive: 168>
	mine _ self class allInstVarNames.
	his _ anotherObject class allInstVarNames.
	1 to: (mine size min: his size) do: [:ind |
		(mine at: ind) = (his at: ind) ifTrue: [
			self instVarAt: ind put: (anotherObject instVarAt: ind)]].
	self class isVariable & anotherObject class isVariable ifTrue: [
		1 to: (self basicSize min: anotherObject basicSize) do: [:ind |
			self basicAt: ind put: (anotherObject basicAt: ind)]].
</details>

#### Object>>#halt: aString

This is the typical message to use for inserting breakpoints during debugging. It creates and schedules a Notifier with the argument, aString, as the label.


<details>
	<summary>See more</summary>
	
	halt: aString 
	"This is the typical message to use for inserting breakpoints during 
	debugging. It creates and schedules a Notifier with the argument, 
	aString, as the label."
	
	Halt new signal: aString
</details>

#### Object>>#readDataFrom: aDataStream size: varsOnDisk

Fill in the fields of self based on the contents of aDataStream. Return self. Read in the instance-variables written by Object>>storeDataOn:. NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it. Allow aDataStream to have fewer inst vars. See SmartRefStream.


<details>
	<summary>See more</summary>
	
	readDataFrom: aDataStream size: varsOnDisk
	"Fill in the fields of self based on the contents of aDataStream.  Return self.
	 Read in the instance-variables written by Object>>storeDataOn:.
	 NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it.
	 Allow aDataStream to have fewer inst vars.  See SmartRefStream."
	| cntInstVars cntIndexedVars |

	cntInstVars _ self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars _ varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars _ 0.
				cntInstVars _ varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	1 to: cntInstVars do:
		[:i | self instVarAt: i put: aDataStream next].
	1 to: cntIndexedVars do:
		[:i | self basicAt: i put: aDataStream next].
	"Total number read MUST be equal to varsOnDisk!"
	^ self	"If we ever return something other than self, fix calls 
			on (super readDataFrom: aDataStream size: anInteger)"
</details>

#### Object>>#stepAt: millisecondSinceLast

See comment at #wantsSteps


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	"See comment at #wantsSteps"
</details>

#### Object>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^self basicSize > 0 or: [self class allInstVarNames isEmpty not]

</details>

#### Object>>#success: aBoolean

For translation only; noop when running in Smalltalk.


<details>
	<summary>See more</summary>
	
	success: aBoolean
	"For translation only; noop when running in Smalltalk."
</details>

#### Object>>#clearHaltOnce

Turn on the halt once flag.


<details>
	<summary>See more</summary>
	
	clearHaltOnce
	"Turn on the halt once flag."
	
	Smalltalk at: #HaltOnce put: false
</details>

#### Object>>#setPinned: aBoolean

The VM's garbage collector routinely moves objects as it reclaims and compacts memory. But it can also pin an object so that it will not be moved, which can make it easier to pass objects out through the FFI. Objects are unpinnned when created. This primitive either pins or unpins an object, and answers if it was already pinned.


<details>
	<summary>See more</summary>
	
	setPinned: aBoolean
	"The VM's garbage collector routinely moves objects as it reclaims and compacts
	 memory. But it can also pin an object so that it will not be moved, which can make
	 it easier to pass objects out through the FFI.  Objects are unpinnned when created.
	 This primitive either pins or unpins an object, and answers if it was already pinned."
	<primitive: 184 error: ec>
	^self primitiveFailed
</details>

#### Object>>#changed

Receiver changed in a general way; inform all the dependents by sending each dependent an update: message.


<details>
	<summary>See more</summary>
	
	changed
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

	self changed: self
</details>

#### Object>>#toFinalizeSend: aSelector to: aFinalizer with: aResourceHandle

When I am finalized (e.g., garbage collected) close the associated resource handle by sending aSelector to the appropriate finalizer (the guy who knows how to get rid of the resource). WARNING: Neither the finalizer nor the resource handle are allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken.


<details>
	<summary>See more</summary>
	
	toFinalizeSend: aSelector to: aFinalizer with: aResourceHandle
	"When I am finalized (e.g., garbage collected) close the associated resource handle by sending aSelector to the appropriate finalizer (the guy who knows how to get rid of the resource).
	WARNING: Neither the finalizer nor the resource handle are allowed to reference me. If they do, then I will NEVER be garbage collected. Since this cannot be validated here, it is up to the client to make sure this invariant is not broken."
	self == aFinalizer ifTrue:[self error: 'I cannot finalize myself'].
	self == aResourceHandle ifTrue:[self error: 'I cannot finalize myself'].
	^self finalizationRegistry add: self executor:
		(ObjectFinalizer new
			receiver: aFinalizer
			selector: aSelector
			argument: aResourceHandle)
</details>

#### Object>>#toggleHaltOnce

<details>
	<summary>See more</summary>
	
	toggleHaltOnce
	self haltOnceEnabled
		ifTrue: [self clearHaltOnce]
		ifFalse: [self setHaltOnce]
</details>

#### Object>>#longPrintOn: aStream limitedTo: sizeLimit indent: indent

Append to the argument, aStream, the names and values of all of the receiver's instance variables. Limit is the length limit for each inst var.


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1));
		 newLine ]
</details>

#### Object>>#break

This is a simple message to use for inserting breakpoints during debugging. The debugger is opened by sending a signal. This gives a chance to restore invariants related to multiple processes.


<details>
	<summary>See more</summary>
	
	break
	"This is a simple message to use for inserting breakpoints during debugging.
	The debugger is opened by sending a signal. This gives a chance to restore
	invariants related to multiple processes."

	BreakPoint signal.

	"nil break."
</details>

#### Object>>#removeActionsSatisfying: aBlock

<details>
	<summary>See more</summary>
	
	removeActionsSatisfying: aBlock

	self actionMap ifNotNil: [ :map |
		map keys do: [ :eachEventSelector |
			self
   				removeActionsSatisfying: aBlock
				forEvent: eachEventSelector ]]
</details>

#### Object>>#isMemberOf: aClass

Answer whether the receiver is an instance of the class, aClass.


<details>
	<summary>See more</summary>
	
	isMemberOf: aClass 
	"Answer whether the receiver is an instance of the class, aClass."

	^self class == aClass
</details>

#### Object>>#notify: aString

Create and schedule a Notifier with the argument as the message in order to request confirmation before a process can proceed.


<details>
	<summary>See more</summary>
	
	notify: aString 
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed."

	Warning signal: aString

	"
	nil notify: 'confirmation message'
	"
</details>

#### Object>>#caseOf: aBlockAssociationCollection

The elements of aBlockAssociationCollection are associations between blocks. Answer the evaluated value of the first association in aBlockAssociationCollection whose evaluated key equals the receiver. If no match is found, report an error.


<details>
	<summary>See more</summary>
	
	caseOf: aBlockAssociationCollection
	"The elements of aBlockAssociationCollection are associations between blocks.
	 Answer the evaluated value of the first association in aBlockAssociationCollection
	 whose evaluated key equals the receiver.  If no match is found, report an error."

	^ self caseOf: aBlockAssociationCollection otherwise: [self caseError]

"| z | z _ {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"| z | z _ {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}. #b caseOf: z"
"The following are compiled in-line:"
"#b caseOf: {[#a]->[1+1]. ['b' asSymbol]->[2+2]. [#c]->[3+3]}"
"#b caseOf: {[#a]->[1+1]. ['d' asSymbol]->[2+2]. [#c]->[3+3]}"
</details>

#### Object>>#asString

Answer a string that represents the receiver. Don't include extra quotes for Strings. This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray, the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired). It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day. In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability. In any case, in your code, if possible, use a more specific method. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	asString
	"Answer a string that represents the receiver.
	Don't include extra quotes for Strings.

	This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray,  the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired).

	It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day.

	In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability.

	In any case, in your code, if possible, use a more specific method.

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printString 
</details>

#### Object>>#yourself

Answer self.


<details>
	<summary>See more</summary>
	
	yourself
	"Answer self."
</details>

#### Object>>#confirm: queryString

Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no.


<details>
	<summary>See more</summary>
	
	confirm: queryString
	"Put up a yes/no menu with caption queryString. Answer true if the 
	response is yes, false if no. This is a modal question--the user must 
	respond yes or no."

	"nil confirm: 'Are you hungry?'"

	^ PopUpMenu confirm: queryString
</details>

#### Object>>#printTextLimitedTo: limit

Answer a Text whose characters are a description of the receiver.


<details>
	<summary>See more</summary>
	
	printTextLimitedTo: limit
	"Answer a Text whose characters are a description of the receiver."

	^self printAs: Text limitedTo: limit
</details>

#### Object>>#perform: selector withArguments: argArray

Send the selector, aSymbol, to the receiver with arguments in argArray. Fail if the number of arguments expected by the selector does not match the size of argArray. Primitive. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	perform: selector withArguments: argArray 
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 84>
	^ self perform: selector withArguments: argArray inSuperclass: self class
</details>

#### Object>>#basicInspect

Create and schedule an Inspector in which the user can examine the receiver's variables. This method should not be overriden.


<details>
	<summary>See more</summary>
	
	basicInspect
	"Create and schedule an Inspector in which the user can examine the 
	receiver's variables. This method should not be overriden."

	BasicInspector openOn: self
</details>

#### Object>>#isCollection

Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:


<details>
	<summary>See more</summary>
	
	isCollection
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^false
</details>

#### Object>>#displayStringOrText

To be used in the UI. Answer might be an instance of Text if appropriate. Don't include extra quotes for Strings. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	displayStringOrText
	"To be used in the UI. Answer might be an instance of Text if appropriate.
	Don't include extra quotes for Strings.
	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^self printString
</details>

#### Object>>#explore

<details>
	<summary>See more</summary>
	
	explore

	ObjectExplorerWindow
		open: (ObjectExplorer new rootObject: self)
		label: nil
</details>

#### Object>>#primitiveFailed: selector

Announce that a primitive has failed and there is no appropriate Smalltalk code to run.


<details>
	<summary>See more</summary>
	
	primitiveFailed: selector
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self error: selector asString, ' failed'
</details>

#### Object>>#notify: aString at: location

Create and schedule a Notifier with the argument as the message in order to request confirmation before a process can proceed. Subclasses can override this and insert an error message at location within aString.


<details>
	<summary>See more</summary>
	
	notify: aString at: location
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed. Subclasses can
	override this and insert an error message at location within aString."

	self notify: aString

	"nil notify: 'confirmation message' at: 12"
</details>

#### Object>>#instVarAtPrim74: anInteger put: anObject

Primitive. Store a value into a fixed variable in the receiver. The numbering of the variables corresponds to the named instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Answer the value stored as the result. Using this message violates the principle that each object has sovereign control over the storing of values into its instance variables. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	instVarAtPrim74: anInteger put: anObject
	"Primitive. Store a value into a fixed variable in the receiver. The 
	numbering of the variables corresponds to the named instance variables. 
	Fail if the index is not an Integer or is not the index of a fixed variable. 
	Answer the value stored as the result. Using this message violates the 
	principle that each object has sovereign control over the storing of 
	values into its instance variables. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject
</details>

## ProtoObject

ProtoObject establishes minimal behavior required of any object in Squeak, even objects that should balk at normal object behavior. Generally these are proxy objects designed to read themselves in from the disk, or to perform some wrapper behavior, before responding to a message. Current examples are ObjectOut and ImageSegmentRootStub, and one could argue that ObjectTracer should also inherit from this class. ProtoObject has no instance variables, nor should any be added.

### Methods
#### ProtoObject>>#ifNotNil: ifNotNilBlock ifNil: nilBlock

If I got here, I am not nil, so evaluate the block ifNotNilBlock


<details>
	<summary>See more</summary>
	
	ifNotNil: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am not nil, so evaluate the block ifNotNilBlock"

	^ ifNotNilBlock valueWithPossibleArgument: self
</details>

#### ProtoObject>>#tryPrimitive: primIndex withArgs: argumentArray

This method is a template that the Smalltalk simulator uses to execute primitives. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	tryPrimitive: primIndex withArgs: argumentArray
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."

	<primitive: 118 error: errorCode>
	^ContextPart primitiveFailTokenFor: errorCode
</details>

#### ProtoObject>>#ifNil: nilBlock

Return self, or evaluate the block if I'm == nil (q.v.)


<details>
	<summary>See more</summary>
	
	ifNil: nilBlock
	"Return self, or evaluate the block if I'm == nil (q.v.)"

	^ self
</details>

#### ProtoObject>>#~~ anObject

Answer whether the receiver and the argument are not the same object (do not have the same object pointer).


<details>
	<summary>See more</summary>
	
	~~ anObject
	"Answer whether the receiver and the argument are not the same object 
	(do not have the same object pointer)."

	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]
</details>

#### ProtoObject>>#== anObject

Primitive. Answer whether the receiver and the argument are the same object (have the same object pointer). Do not redefine the message == in any other class! Essential. No Lookup. Do not override in any subclass. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	== anObject 
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
</details>

#### ProtoObject>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	"Subclasses should redefine this method to perform initializations on instance creation"
</details>

#### ProtoObject>>#nextObject

Primitive. Answer the next object after the receiver in the enumeration of all objects. Return 0 when all objects have been enumerated.


<details>
	<summary>See more</summary>
	
	nextObject
	"Primitive. Answer the next object after the receiver in the 
	enumeration of all objects. Return 0 when all objects have been 
	enumerated."

	<primitive: 139>
	self primitiveFailed.
</details>

#### ProtoObject>>#withArgs: argArray executeMethod: compiledMethod

Execute compiledMethod against the receiver and args in argArray


<details>
	<summary>See more</summary>
	
	withArgs: argArray executeMethod: compiledMethod
	"Execute compiledMethod against the receiver and args in argArray"

	<primitive: 188>
	self primitiveFailed
</details>

#### ProtoObject>>#doesNotUnderstand: aMessage

<details>
	<summary>See more</summary>
	
	doesNotUnderstand: aMessage

	^ MessageNotUnderstood new 
		message: aMessage;
		receiver: self;
		signal
</details>

#### ProtoObject>>#cannotInterpret: aMessage

Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector. Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose.


<details>
	<summary>See more</summary>
	
	cannotInterpret: aMessage 
	 "Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector.  Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose."

"If this is the result of encountering a swap-out stub, then simulating the lookup in Smalltalk should suffice to install the class properly, and the message may be resent."

	(self class lookupSelector: aMessage selector) ifNotNil: [
		"Simulated lookup succeeded -- resend the message."
		^ aMessage sentTo: self].

	"Could not recover by simulated lookup -- it's an error"
	Error signal: 'MethodDictionary fault'.

	"Try again in case an error handler fixed things"
	^ aMessage sentTo: self
</details>

#### ProtoObject>>#statePointsTo: anObject

Answers true if anObject is among my named or indexed instance variables, and false otherwise


<details>
	<summary>See more</summary>
	
	statePointsTo: anObject
	"Answers true if anObject is among my named or indexed instance variables, and false otherwise"

	<primitive: 132>
	1 to: self class instSize do: [ :i |
		(self instVarAt: i) == anObject ifTrue: [ ^ true ]].
	1 to: self basicSize do: [ :i |
		(self basicAt: i) == anObject ifTrue: [ ^ true ]].
	^ false
</details>

#### ProtoObject>>#ifNil: nilBlock ifNotNil: ifNotNilBlock

Evaluate the block, unless I'm == nil (q.v.)


<details>
	<summary>See more</summary>
	
	ifNil: nilBlock ifNotNil: ifNotNilBlock
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock valueWithPossibleArgument: self
</details>

#### ProtoObject>>#notNil

Coerces nil to false and everything else to true.


<details>
	<summary>See more</summary>
	
	notNil
	"Coerces nil to false and everything else to true."

	^true
</details>

#### ProtoObject>>#pointsTo: anObject

Answers true if I hold a reference to anObject, or false otherwise. Or stated another way: Answers true if the garbage collector would fail to collect anObject because I hold a reference to it, or false otherwise


<details>
	<summary>See more</summary>
	
	pointsTo: anObject
"Answers true if I hold a reference to anObject, or false otherwise. Or stated another way:

Answers true if the garbage collector would fail to collect anObject because I hold a reference to it, or false otherwise"

	^ (self statePointsTo: anObject)
		or: [ self class == anObject ]
</details>

#### ProtoObject>>#become: otherObject

Primitive. Swap the object pointers of the receiver and the argument. All variables in the entire system that used to point to the receiver now point to the argument, and vice-versa. Fails if either object is a SmallInteger


<details>
	<summary>See more</summary>
	
	become: otherObject 
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger"

	| selfMethod otherObjectMethod selector contextReceiver |
	self class == otherObject class ifFalse: [
		Processor 
			processesDo: [ :p | ] withStackFramestDo: [ :process :context |
				contextReceiver _ context receiver.
				(self == contextReceiver or: [ otherObject == contextReceiver ]) ifTrue: [
					selector _ context method selector.
					selfMethod _ self class lookupSelector: selector.
					otherObjectMethod _ otherObject class lookupSelector: selector.
					selfMethod = otherObjectMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: contextReceiver class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: thisContext sender.
		].
	{ self } elementsExchangeIdentityWith: { otherObject }
</details>

#### ProtoObject>>#scaledIdentityHash

For identityHash values returned by primitive 75, answer such values times 2^8. Otherwise, match the existing identityHash implementation


<details>
	<summary>See more</summary>
	
	scaledIdentityHash
	"For identityHash values returned by primitive 75, answer
	 such values times 2^8.  Otherwise, match the existing
	 identityHash implementation"

	^self identityHash * 256 "bitShift: 8"
</details>

#### ProtoObject>>#isInMemory

All normal objects are.


<details>
	<summary>See more</summary>
	
	isInMemory
	"All normal objects are."
	^ true
</details>

#### ProtoObject>>#isNil

Coerces nil to true and everything else to false.


<details>
	<summary>See more</summary>
	
	isNil
	"Coerces nil to true and everything else to false."

	^false
</details>

#### ProtoObject>>#flag: aSymbol

Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval. For example, you might put the following line in a number of messages: self flag: #returnHereUrgently Then, to retrieve all such messages, browse all senders of #returnHereUrgently.


<details>
	<summary>See more</summary>
	
	flag: aSymbol
	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
</details>

#### ProtoObject>>#identityHash

Answer a SmallInteger whose value is related to the receiver's identity. This method must not be overridden, except by immediate classes such as SmallInteger, and in Spur systems, Character and SmallFloat64. Primitive. Fails if the receiver is a SmallInteger. Essential. See Object documentation whatIsAPrimitive. Do not override.


<details>
	<summary>See more</summary>
	
	identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by immediate classes such as SmallInteger,
	and in Spur systems, Character and SmallFloat64.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed
</details>

#### ProtoObject>>#ifNotNil: ifNotNilBlock

Evaluate the block, unless I'm == nil (q.v.)


<details>
	<summary>See more</summary>
	
	ifNotNil: ifNotNilBlock
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock valueWithPossibleArgument: self
</details>

#### ProtoObject>>#rehash

Do nothing. Here so sending this to a Set does not have to do a time consuming respondsTo:


<details>
	<summary>See more</summary>
	
	rehash
	"Do nothing.  Here so sending this to a Set does not have to do a time consuming respondsTo:"
</details>

#### ProtoObject>>#nextInstance

Primitive. Answer the next instance after the receiver in the enumeration of all instances of this class. Fails if all instances have been enumerated. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	nextInstance
	"Primitive. Answer the next instance after the receiver in the 
	enumeration of all instances of this class. Fails if all instances have been 
	enumerated. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 78>
	^nil
</details>

## True

True defines the behavior of its single instance, true -- logical assertion. Notice how the truth-value checks become direct message sends, without the need for explicit testing. Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no effect.

### Methods
#### True>>#not

Negation--answer false since the receiver is true.


<details>
	<summary>See more</summary>
	
	not
	"Negation--answer false since the receiver is true."

	^false
</details>

#### True>>#and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

I sending value to aBlock4 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

	"I sending value to aBlock4 to optimize the generated byte-code - Hernan"
	^aBlock1 value and: [ aBlock2 value and: [ aBlock3 value and: [ aBlock4 value ]]] 
</details>

#### True>>#and: aBlock1 and: aBlock2

I sending value to aBlock2 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2

	"I sending value to aBlock2 to optimize the generated byte-code - Hernan"
	^aBlock1 value and: [ aBlock2 value ]
</details>

#### True>>#or: aBlock1 or: aBlock2

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2

	^self
</details>

#### True>>#ifTrue: alternativeBlock

Answer the value of alternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually 
	reach here because the expression is compiled in-line."

	^alternativeBlock value
</details>

#### True>>#& alternativeObject

Evaluating conjunction -- answer alternativeObject since receiver is true.


<details>
	<summary>See more</summary>
	
	& alternativeObject 
	"Evaluating conjunction -- answer alternativeObject since receiver is true."

	^alternativeObject
</details>

#### True>>#xor: aBoolean

Posted by Eliot Miranda to squeak-dev on 3/24/2009


<details>
	<summary>See more</summary>
	
	xor: aBoolean
	"Posted by Eliot Miranda to squeak-dev on 3/24/2009"
	
	^aBoolean not
</details>

#### True>>#or: alternativeBlock

Nonevaluating disjunction -- answer true since the receiver is true.


<details>
	<summary>See more</summary>
	
	or: alternativeBlock 
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self
</details>

#### True>>#and: aBlock1 and: aBlock2 and: aBlock3

I sending value to aBlock3 to optimize the generated byte-code - Hernan


<details>
	<summary>See more</summary>
	
	and: aBlock1 and: aBlock2 and: aBlock3

	"I sending value to aBlock3 to optimize the generated byte-code - Hernan"
	^aBlock1 value and: [ aBlock2 value and: [ aBlock3 value ]]
</details>

#### True>>#ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock

Answer with the value of trueAlternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer with the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value
</details>

#### True>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream 

	aStream nextPutAll: 'true'
</details>

#### True>>#or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

	^self
</details>

#### True>>#| aBoolean

Evaluating disjunction (OR) -- answer true since the receiver is true.


<details>
	<summary>See more</summary>
	
	| aBoolean 
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self
</details>

#### True>>#and: alternativeBlock

Nonevaluating conjunction -- answer the value of alternativeBlock since the receiver is true.


<details>
	<summary>See more</summary>
	
	and: alternativeBlock 
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."

	^alternativeBlock value
</details>

#### True>>#orNot: alternativeBlock

Nonevaluating disjunction -- answer true since the receiver is true.


<details>
	<summary>See more</summary>
	
	orNot: alternativeBlock 
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self
</details>

#### True>>#ifFalse: alternativeBlock

Since the condition is true, the value is the true alternative, which is nil. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifFalse: alternativeBlock 
	"Since the condition is true, the value is the true alternative, which is nil. 
	Execution does not actually reach here because the expression is compiled 
	in-line."

	^nil
</details>

#### True>>#or: aBlock1 or: aBlock2 or: aBlock3

Nonevaluating alternation without deep nesting. The receiver is evaluated, followed by the blocks in order. If any of these evaluates as true, then return true immediately, without evaluating any further blocks. If all return false, then return false.


<details>
	<summary>See more</summary>
	
	or: aBlock1 or: aBlock2 or: aBlock3

	^self
</details>

#### True>>#ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock

Answer the value of trueAlternativeBlock. Execution does not actually reach here because the expression is compiled in-line.


<details>
	<summary>See more</summary>
	
	ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value
</details>

## UndefinedObject

I describe the behavior of my sole instance, nil. nil represents a prior value for variables that have not been initialized, or for results which are meaningless.

### Methods
#### UndefinedObject>>#ifNil: aBlock

A convenient test, in conjunction with Object ifNil:


<details>
	<summary>See more</summary>
	
	ifNil: aBlock
	"A convenient test, in conjunction with Object ifNil:"

	^ aBlock value
</details>

#### UndefinedObject>>#subclasses

Return all the subclasses of nil


<details>
	<summary>See more</summary>
	
	subclasses
	"Return all the subclasses of nil"
	| classList |
	classList _ WriteStream on: Array new.
	self subclassesDo:[:class| classList nextPut: class].
	^classList contents
</details>

#### UndefinedObject>>#subclassesDoGently: aBlock

Evaluate aBlock with all subclasses of nil. Others are not direct subclasses of Class.


<details>
	<summary>See more</summary>
	
	subclassesDoGently: aBlock
	"Evaluate aBlock with all subclasses of nil.  Others are not direct subclasses of Class."

	^ Class subclassesDoGently: [:cl | 
			cl isMeta ifTrue: [aBlock value: cl soleInstance]].
</details>

#### UndefinedObject>>#addSubclass: aClass

Ignored -- necessary to support disjoint class hierarchies


<details>
	<summary>See more</summary>
	
	addSubclass: aClass
	"Ignored -- necessary to support disjoint class hierarchies"
</details>

#### UndefinedObject>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral
	^ true
</details>

#### UndefinedObject>>#notNil

Refer to the comment in Object|notNil.


<details>
	<summary>See more</summary>
	
	notNil 
	"Refer to the comment in Object|notNil."

	^false
</details>

#### UndefinedObject>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Refer to the comment in Object|printOn:." 

	aStream nextPutAll: 'nil'
</details>

#### UndefinedObject>>#suspend

Kills off processes that didn't terminate properly


<details>
	<summary>See more</summary>
	
	suspend
	"Kills off processes that didn't terminate properly"
	"Display reverse; reverse."  "<-- So we can catch the suspend bug"
	Processor terminateActive
</details>

#### UndefinedObject>>#valueWithPossibleArgs: anArray

To provide polymorphism with blocks. Allows nil to be used instead of an empty closure.


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgs: anArray
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
</details>

#### UndefinedObject>>#subclassesDo: aBlock

Evaluate aBlock with all subclasses of nil.


<details>
	<summary>See more</summary>
	
	subclassesDo: aBlock
	"Evaluate aBlock with all subclasses of nil."
	^Class subclassesDo:[:cl| 
		cl isMeta ifTrue:[aBlock value: cl soleInstance]].
</details>

#### UndefinedObject>>#canHandleSignal: exception

When no more handler (on:do:) context left in sender chain this gets called


<details>
	<summary>See more</summary>
	
	canHandleSignal: exception
	"When no more handler (on:do:) context left in sender chain this gets called"

	^ false
</details>

#### UndefinedObject>>#literalScannedAs: scannedLiteral notifying: requestor

<details>
	<summary>See more</summary>
	
	literalScannedAs: scannedLiteral notifying: requestor 
	^ scannedLiteral
</details>

#### UndefinedObject>>#ifNotNil: ifNotNilBlock ifNil: nilBlock

If I got here, I am nil, so evaluate the block nilBlock


<details>
	<summary>See more</summary>
	
	ifNotNil: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am nil, so evaluate the block nilBlock"

	^ nilBlock value
</details>

#### UndefinedObject>>#addDependent: ignored

Refer to the comment in Object|dependents.


<details>
	<summary>See more</summary>
	
	addDependent: ignored 
	"Refer to the comment in Object|dependents."

	self error: 'Nil should not have dependents'
</details>

#### UndefinedObject>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Refer to the comment in Object|storeOn:." 

	aStream nextPutAll: 'nil'
</details>

#### UndefinedObject>>#handleSignal: exception

When no more handler (on:do:) context left in sender chain this gets called. Return from signal with default action.


<details>
	<summary>See more</summary>
	
	handleSignal: exception
	"When no more handler (on:do:) context left in sender chain this gets called.  Return from signal with default action."

	^ exception resumeUnchecked: exception defaultAction
</details>

#### UndefinedObject>>#removeSubclass: aClass

Ignored -- necessary to support disjoint class hierarchies


<details>
	<summary>See more</summary>
	
	removeSubclass: aClass
	"Ignored -- necessary to support disjoint class hierarchies"
</details>

#### UndefinedObject>>#subclass: nameOfClass  
	instanceVariableNames: instVarNames
	classVariableNames: classVarNames
	poolDictionaries: poolDictnames
	category: category

Calling this method is now considered an accident. If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:


<details>
	<summary>See more</summary>
	
	subclass: nameOfClass  
	instanceVariableNames: instVarNames
	classVariableNames: classVarNames
	poolDictionaries: poolDictnames
	category: category
	"Calling this method is now considered an accident.  If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:"
	Transcript show: ('Attempt to create ', nameOfClass, ' as a subclass of nil.  Possibly a class is being loaded before its superclass.'); newLine.
	^ProtoObject
		subclass: nameOfClass
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: poolDictnames
		category: category

</details>

#### UndefinedObject>>#superclassNotValidErrorDescriptionFor: aClass

<details>
	<summary>See more</summary>
	
	superclassNotValidErrorDescriptionFor: aClass 

	^aClass name, ' not in superclasses chain'
</details>

#### UndefinedObject>>#ifNil: nilBlock ifNotNil: ifNotNilBlock

Evaluate the block for nil because I'm == nil


<details>
	<summary>See more</summary>
	
	ifNil: nilBlock ifNotNil: ifNotNilBlock
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value
</details>

#### UndefinedObject>>#valueWithPossibleArgument: anArg

To provide polymorphism with blocks. Allows nil to be used instead of an empty closure.


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgument: anArg
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
</details>

#### UndefinedObject>>#valueWithPossibleArgument: anArg and: secondArg

To provide polymorphism with blocks. Allows nil to be used instead of an empty closure.


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgument: anArg and: secondArg
	"To provide polymorphism with blocks. Allows nil to be used instead of an empty closure."
</details>

#### UndefinedObject>>#typeOfClass

Necessary to support disjoint class hierarchies.


<details>
	<summary>See more</summary>
	
	typeOfClass
	"Necessary to support disjoint class hierarchies."
	^#normal
</details>

#### UndefinedObject>>#allSuperclassesUpTo: aSuperclass

<details>
	<summary>See more</summary>
	
	allSuperclassesUpTo: aSuperclass 

	self error: (self superclassNotValidErrorDescriptionFor: aSuperclass)
</details>

#### UndefinedObject>>#isNil

Refer to the comment in Object|isNil.


<details>
	<summary>See more</summary>
	
	isNil 
	"Refer to the comment in Object|isNil."

	^true
</details>

#### UndefinedObject>>#whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

	^aNoneBlock value
</details>

#### UndefinedObject>>#isEmptyOrNil

Answer whether the receiver contains any elements, or is nil. Useful in numerous situations where one wishes the same reaction to an empty collection or to nil


<details>
	<summary>See more</summary>
	
	isEmptyOrNil
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"
	^ true
</details>

#### UndefinedObject>>#ifNotNil: aBlock

A convenient test, in conjunction with Object ifNotNil:


<details>
	<summary>See more</summary>
	
	ifNotNil: aBlock
	"A convenient test, in conjunction with Object ifNotNil:"

	^ self
</details>

#### UndefinedObject>>#shallowCopy

Only one instance of UndefinedObject should ever be made, so answer with self.


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Only one instance of UndefinedObject should ever be made, so answer 
	with self."
</details>

## WeakActionSequence

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### WeakActionSequence>>#valueWithArguments: anArray startingFrom: startIndex

Do the same as my parent, but make sure that all actions that do not give errors are evaluated before resignaling the ones that gave errors (giving the chance to clients to handle them).


<details>
	<summary>See more</summary>
	
	valueWithArguments: anArray startingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each valueWithArguments: anArray ]
			on: UnhandledError
			do: [:exc | 
				self valueWithArguments: anArray startingFrom: index + 1.
				exc pass]].
	^ answer
</details>

#### WeakActionSequence>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self size < 2 ifTrue: [^super printOn: aStream].
	aStream nextPutAll: '#('.
	self
		do: [:each | each printOn: aStream]
		separatedBy: [aStream newLine].
	aStream nextPut: $)
</details>

#### WeakActionSequence>>#valueWithArguments: anArray

Do the same as my parent, but make sure that all actions that do not give errors are evaluated before resignaling the ones that gave errors (giving the chance to clients to handle them).


<details>
	<summary>See more</summary>
	
	valueWithArguments: anArray 
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	^self valueWithArguments: anArray startingFrom: 1
</details>

#### WeakActionSequence>>#asActionSequence

<details>
	<summary>See more</summary>
	
	asActionSequence

	^self
</details>

#### WeakActionSequence>>#value

Do the same as my parent, but make sure that all actions that do not give errors are evaluated before resignaling the ones that gave errors (giving the chance to clients to handle them).


<details>
	<summary>See more</summary>
	
	value
	"Do the same as my parent, but make sure that all actions that do not  
	give errors are evaluated before resignaling the ones that gave errors  
	(giving the chance to clients to handle them)."

	^self valueStartingFrom: 1
</details>

#### WeakActionSequence>>#asMinimalRepresentation

<details>
	<summary>See more</summary>
	
	asMinimalRepresentation

	| valid |
	valid := self reject: [:e | e isReceiverOrAnyArgumentGarbage ].
	valid size = 0
		ifTrue: [ ^nil ].
	valid size = 1
		ifTrue: [ ^valid first ].
	^valid
</details>

#### WeakActionSequence>>#valueStartingFrom: startIndex

Do the same as my parent, but make sure that all actions that do not give errors are evaluated before resignaling the ones that gave errors (giving the chance to clients to handle them).


<details>
	<summary>See more</summary>
	
	valueStartingFrom: startIndex
	"Do the same as my parent, but make sure that all actions that do not 
	give errors are evaluated before resignaling the ones that gave errors 
	(giving the chance to clients to handle them)."

	| each answer |
	startIndex to: self size do: [:index |
		each := self at: index.
		[ answer := each value ]
			on: UnhandledError
			do: [:exc | 
				self valueStartingFrom: index + 1.
				exc pass]].
	^ answer
</details>

## WeakMessageSend

Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid mesage. A request to value only results in a send if in fact it is valid. See MessageSendComments also. WeakMessageSend is used primarily for event regristration. Unlike MessageSend WeakMessageSend stoes receiver (object receiving the message send) as a the first and only element of its array as opposed to a named ivar. But like MessageSend, it does have selector Symbol -- message selector arguments Array -- bound arguments and it also has shouldBeNil Boolean -- used to ensure array of arguments is not all nils

### Methods
#### WeakMessageSend>>#collectArguments: anArgArray

Private


<details>
	<summary>See more</summary>
	
	collectArguments: anArgArray
	"Private"
    | staticArgs |
    staticArgs := self arguments.
    ^(anArgArray size = staticArgs size)
        ifTrue: [anArgArray]
        ifFalse:
            [(staticArgs isEmpty
                ifTrue: [ staticArgs := Array new: selector numArgs]
                ifFalse: [staticArgs copy] )
                    replaceFrom: 1
                    to: (anArgArray size min: staticArgs size)
                    with: anArgArray
                    startingAt: 1]

</details>

#### WeakMessageSend>>#isReceiverGarbage

Make sure that my receiver hasn't gone away


<details>
	<summary>See more</summary>
	
	isReceiverGarbage
	"Make sure that my receiver hasn't gone away"
	^self receiver isNil

</details>

#### WeakMessageSend>>#arguments

<details>
	<summary>See more</summary>
	
	arguments
	^arguments ifNil: [ Array new ]

</details>

#### WeakMessageSend>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]
</details>

#### WeakMessageSend>>#valueWithArguments: anArray

Safe to use, because they are built before ensureing receiver and args...


<details>
	<summary>See more</summary>
	
	valueWithArguments: anArray
	| argsToUse |
	
	"Safe to use, because they are built before ensureing receiver and args..."
	argsToUse := self collectArguments: anArray.
	^ self withEnsuredReceiverAndArgumentsDo: [ :r :a |
		r
			perform: selector
			withArguments: argsToUse ]
</details>

#### WeakMessageSend>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^selector

</details>

#### WeakMessageSend>>#asActionSequence

<details>
	<summary>See more</summary>
	
	asActionSequence

	^WeakActionSequence with: self
</details>

#### WeakMessageSend>>#receiver

<details>
	<summary>See more</summary>
	
	receiver
	^self at: 1

</details>

#### WeakMessageSend>>#selector: aSymbol

<details>
	<summary>See more</summary>
	
	selector: aSymbol
	selector _ aSymbol

</details>

#### WeakMessageSend>>#value

<details>
	<summary>See more</summary>
	
	value
	^ arguments
		ifNil: [
			self withEnsuredReceiverDo: [ :r | r perform: selector ]]
		ifNotNil: [
			self withEnsuredReceiverAndArgumentsDo: [ :r :a |
				r
					perform: selector
					withArguments: a ]]
</details>

#### WeakMessageSend>>#withEnsuredReceiverDo: aBlock

Grab a real reference to receive. If still there, evaluate aBlock.


<details>
	<summary>See more</summary>
	
	withEnsuredReceiverDo: aBlock
	"Grab a real reference to receive. If still there, evaluate aBlock."
	"Return nil if my receiver has gone away"

	^ self receiver ifNotNil: [ :r | aBlock value: r ]
</details>

#### WeakMessageSend>>#isAnyArgumentGarbage

Make sure that my arguments haven't gone away


<details>
	<summary>See more</summary>
	
	isAnyArgumentGarbage
	"Make sure that my arguments haven't gone away"
	arguments ifNotNil: [
		arguments with: shouldBeNil do: [ :arg :flag |
			(flag not and: [arg isNil])
				ifTrue: [^true]
		]
	].
	^false

</details>

#### WeakMessageSend>>#= anObject

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= anObject

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	"Compare equal to equivalent MessageSend"
	^ (anObject is: #MessageSend)
		and: [self receiver == anObject receiver
		and: [selector == anObject selector
		and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]

</details>

#### WeakMessageSend>>#hash

work like MessageSend>>hash


<details>
	<summary>See more</summary>
	
	hash
	"work like MessageSend>>hash"
	^self receiver hash bitXor: selector hash

</details>

#### WeakMessageSend>>#isReceiverOrAnyArgumentGarbage

Make sure that my receiver hasn't gone away


<details>
	<summary>See more</summary>
	
	isReceiverOrAnyArgumentGarbage
	"Make sure that my receiver hasn't gone away"
	^self isReceiverGarbage 
		or: [self isAnyArgumentGarbage]

</details>

#### WeakMessageSend>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        self receiver printOn: aStream.
        aStream nextPut: $)

</details>

#### WeakMessageSend>>#withEnsuredReceiverAndArgumentsDo: aBlock

Grab real references to receiver and arguments. If they still exist, evaluate aBlock.


<details>
	<summary>See more</summary>
	
	withEnsuredReceiverAndArgumentsDo: aBlock
	"Grab real references to receiver and arguments. If they still exist, evaluate aBlock."

	"Return if my receiver has gone away"
	| r a |
	r := self receiver.
	r ifNil: [ ^nil ].

	
	"Make sure that my arguments haven't gone away"
	a := Array withAll: arguments.
	a with: shouldBeNil do: [ :arg :flag |
		arg ifNil: [ flag ifFalse: [ ^nil ]]
	].

	^aBlock value: r value: a
</details>

#### WeakMessageSend>>#receiver: anObject

<details>
	<summary>See more</summary>
	
	receiver: anObject
	self at: 1 put: anObject

</details>

#### WeakMessageSend>>#arguments: anArray

<details>
	<summary>See more</summary>
	
	arguments: anArray
	arguments _ WeakArray withAll: anArray.
	"no reason this should be a WeakArray"
	shouldBeNil _ Array withAll: (anArray collect: [ :ea | ea isNil ]).

</details>

#### WeakMessageSend>>#asMessageSend

<details>
	<summary>See more</summary>
	
	asMessageSend
	^MessageSend receiver: self receiver selector: selector arguments: (Array withAll: self arguments) 

</details>

#### WeakMessageSend>>#asMinimalRepresentation

<details>
	<summary>See more</summary>
	
	asMinimalRepresentation

	^self isReceiverOrAnyArgumentGarbage
		ifTrue: [ nil ]
		ifFalse: [ self ]
</details>

