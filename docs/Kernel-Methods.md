## AdditionalMethodState

I am class holding state for compiled methods. All my instance variables should be actually part of the CompiledMethod itself, but the current implementation of the VM doesn't allow this. Currently I hold the selector and any pragmas or properties the compiled method has. Pragmas and properties are stored in indexable fields; pragmas as instances of Pragma, properties as instances of Association. I am a reimplementation of much of MethodProperties, but eliminating the explicit properties and pragmas dictionaries. Hence I answer true to isMethodProperties.

### Methods
#### AdditionalMethodState>>#at: aKey put: aValue

Replace the property value or pragma associated with aKey.


<details>
	<summary>See more</summary>
	
	at: aKey put: aValue
	"Replace the property value or pragma associated with aKey."

	| keyAlreadyExists propertyOrPragma "<Association|Pragma>" |
	keyAlreadyExists _ false.
	
	1 to: self basicSize do: [ :i |
		(propertyOrPragma _ self basicAt: i) key == aKey ifTrue: [
			keyAlreadyExists _ true.
			propertyOrPragma isVariableBinding
				ifTrue: [ propertyOrPragma value: aValue ]
				ifFalse: [ self basicAt: i put: aValue ]]].
	
	keyAlreadyExists ifFalse: [
		method propertyValueAt: aKey put: aValue ].
	
	^ aValue
</details>

#### AdditionalMethodState>>#pragmas

Answer the raw messages comprising my pragmas.


<details>
	<summary>See more</summary>
	
	pragmas
	"Answer the raw messages comprising my pragmas."
	| pragmaStream propertyOrPragma "<Association|Message>" |
	pragmaStream := WriteStream on: (Array new: self basicSize).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[pragmaStream nextPut: propertyOrPragma]].
	^pragmaStream contents
</details>

#### AdditionalMethodState>>#hasLiteralSuchThat: aBlock

Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure. This method is only intended for private use by CompiledMethod hasLiteralSuchThat:


<details>
	<summary>See more</summary>
	
	hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [(aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]]]
			ifFalse: [propertyOrPragma hasLiteralSuchThat: aBlock]) ifTrue: [^true]].
	^false
</details>

#### AdditionalMethodState>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^selector
</details>

#### AdditionalMethodState>>#notEmpty

<details>
	<summary>See more</summary>
	
	notEmpty
	^self basicSize > 0
</details>

#### AdditionalMethodState>>#removeKey: aKey ifAbsent: aBlock

Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	removeKey: aKey ifAbsent: aBlock
	"Remove the property with aKey. Answer the value or, if aKey isn't found, answer the result of evaluating aBlock."
	
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma key]
				ifFalse: [propertyOrPragma keyword])
			== aKey ifTrue:
			[^method removeProperty: aKey]].
	^aBlock value
</details>

#### AdditionalMethodState>>#removeKey: aKey

Remove the property with aKey. Answer the property or raise an error if aKey isn't found.


<details>
	<summary>See more</summary>
	
	removeKey: aKey
	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."
	
	^ self removeKey: aKey ifAbsent: [ self error: 'Property not found' ].
</details>

#### AdditionalMethodState>>#includes: aPropertyOrPragma

<Association|Pragma>


<details>
	<summary>See more</summary>
	
	includes: aPropertyOrPragma "<Association|Pragma>"
	"Test if the property or pragma is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) = aPropertyOrPragma ifTrue:
			[^true]].
	^false
</details>

#### AdditionalMethodState>>#at: aKey ifAbsentPut: aBlock

Answer the property value or pragma associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	at: aKey ifAbsentPut: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^method propertyValueAt: aKey put: aBlock value
</details>

#### AdditionalMethodState>>#propertyValueAt: aKey

Answer the property value associated with aKey.


<details>
	<summary>See more</summary>
	
	propertyValueAt: aKey
	"Answer the property value associated with aKey."
	
	^ self propertyValueAt: aKey ifAbsent: [ self error: 'Property not found' ].
</details>

#### AdditionalMethodState>>#hasLiteralThorough: literal

Answer true if any literal in these properties is literal, even if embedded in array structure.


<details>
	<summary>See more</summary>
	
	hasLiteralThorough: literal
	"Answer true if any literal in these properties is literal,
	 even if embedded in array structure."
	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [propertyOrPragma key == literal
					or: [propertyOrPragma value == literal
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteral: literal]]]]
			ifFalse: [propertyOrPragma hasLiteral: literal]) ifTrue:
			[^true]].
	^false
</details>

#### AdditionalMethodState>>#copyWithout: aPropertyOrPragma

<Association|Pragma>


<details>
	<summary>See more</summary>
	
	copyWithout: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which no longer includes aPropertyOrPragma"
	| bs copy offset |
	copy := self class new: (bs := self basicSize) - ((self includes: aPropertyOrPragma)
													ifTrue: [1]
													ifFalse: [0]).
	offset := 0.
	1 to: bs do:
		[:i|
		(self basicAt: i) = aPropertyOrPragma
			ifTrue: [offset := 1]
			ifFalse: [copy basicAt: i - offset put: (self basicAt: i)]].
	^copy
		selector: selector;
		setMethod: method;
		yourself

</details>

#### AdditionalMethodState>>#at: aKey

Answer the property value or pragma associated with aKey.


<details>
	<summary>See more</summary>
	
	at: aKey
	"Answer the property value or pragma associated with aKey."
	
	^self at: aKey ifAbsent: [self error: 'not found']
</details>

#### AdditionalMethodState>>#includesKey: aKey

Test if the property aKey or pragma with selector aKey is present.


<details>
	<summary>See more</summary>
	
	includesKey: aKey
	"Test if the property aKey or pragma with selector aKey is present."

	1 to: self basicSize do:
		[:i |
		(self basicAt: i) key == aKey ifTrue:
			[^true]].
	^false
</details>

#### AdditionalMethodState>>#propertyKeysAndValuesDo: aBlock

Enumerate the receiver with all the keys and values.


<details>
	<summary>See more</summary>
	
	propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[aBlock value: propertyOrPragma key value: propertyOrPragma value]]
</details>

#### AdditionalMethodState>>#method: aMethodNodeOrNil

For decompilation


<details>
	<summary>See more</summary>
	
	method: aMethodNodeOrNil
	"For decompilation"
	method := aMethodNodeOrNil
</details>

#### AdditionalMethodState>>#at: aKey ifAbsent: aBlock

Answer the property value or pragma associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	at: aKey ifAbsent: aBlock
	"Answer the property value or pragma associated with aKey or,
	 if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do:
		[:i |
		(propertyOrPragma := self basicAt: i) key == aKey ifTrue:
			[^propertyOrPragma isVariableBinding
				ifTrue: [propertyOrPragma value]
				ifFalse: [propertyOrPragma]]].
	^aBlock value
</details>

#### AdditionalMethodState>>#isMethodProperties

<details>
	<summary>See more</summary>
	
	isMethodProperties
	^true
</details>

#### AdditionalMethodState>>#includesProperty: aKey

Test if the property aKey is present.


<details>
	<summary>See more</summary>
	
	includesProperty: aKey
	"Test if the property aKey is present."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^true]].
	^false
</details>

#### AdditionalMethodState>>#selector: aSymbol

<details>
	<summary>See more</summary>
	
	selector: aSymbol
	selector := aSymbol
</details>

#### AdditionalMethodState>>#isEmpty

<details>
	<summary>See more</summary>
	
	isEmpty
	^self basicSize = 0
</details>

#### AdditionalMethodState>>#properties

<Association|Pragma>


<details>
	<summary>See more</summary>
	
	properties

	| propertyStream propertyOrPragma "<Association|Pragma>" |
	propertyStream := WriteStream on: (Array new: self basicSize * 2).
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifTrue:
			[propertyStream nextPut: propertyOrPragma key; nextPut: propertyOrPragma value]].
	^IdentityDictionary newFromPairs: propertyStream contents
</details>

#### AdditionalMethodState>>#analogousCodeTo: aMethodProperties

<details>
	<summary>See more</summary>
	
	analogousCodeTo: aMethodProperties
	| bs |
	self class == aMethodProperties class ifFalse:
		[^false].
	(bs := self basicSize) = aMethodProperties basicSize ifFalse:
		[^false].
	1 to: bs do:
		[:i|
		((self basicAt: i) analogousCodeTo: (aMethodProperties basicAt: i)) ifFalse:
			[^false]].
	^true
</details>

#### AdditionalMethodState>>#hasAtLeastTheSamePropertiesAs: aMethodProperties

Answer if the recever has at least the same properties as the argument. N.B. The receiver may have additional properties and still answer true.


<details>
	<summary>See more</summary>
	
	hasAtLeastTheSamePropertiesAs: aMethodProperties
	"Answer if the recever has at least the same properties as the argument.
	 N.B. The receiver may have additional properties and still answer true."
	aMethodProperties keysAndValuesDo:
		[:k :v|
		(v is: #Pragma)
			"ifTrue: [Pragmas have already been checked]"
			ifFalse: [
				(self includes: k->v) ifFalse: [^false]]].
	^true
</details>

#### AdditionalMethodState>>#propertyValueAt: aKey ifAbsent: aBlock

Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	propertyValueAt: aKey ifAbsent: aBlock
	"Answer the property value associated with aKey or, if aKey isn't found, answer the result of evaluating aBlock."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
		 and: [propertyOrPragma key == aKey]) ifTrue:
			[^propertyOrPragma value]].
	^aBlock value
</details>

#### AdditionalMethodState>>#copyWith: aPropertyOrPragma

<Association|Pragma>


<details>
	<summary>See more</summary>
	
	copyWith: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(Association == aPropertyOrPragma class
	 or: [Pragma == aPropertyOrPragma class]) ifFalse:
		[self error: self class name, ' instances should hold only Associations or Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do:
		[:i|
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself

</details>

#### AdditionalMethodState>>#setMethod: aMethod

<Association|Pragma>


<details>
	<summary>See more</summary>
	
	setMethod: aMethod
	| propertyOrPragma "<Association|Pragma>" |
	method := aMethod.
	1 to: self basicSize do:
		[ :i |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[propertyOrPragma setMethod: aMethod]]
</details>

#### AdditionalMethodState>>#keysAndValuesDo: aBlock

Enumerate the receiver with all the keys and values.


<details>
	<summary>See more</summary>
	
	keysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertyOrPragma "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(propertyOrPragma := self basicAt: i) isVariableBinding
			ifTrue: [aBlock value: propertyOrPragma key value: propertyOrPragma value]
			ifFalse: [aBlock value: propertyOrPragma keyword value: propertyOrPragma]]
</details>

## BlockClosure

I am a block closure for Eliot's closure implementation. Not to be confused with the old BlockClosure (they were never part of Cuis anyway). This is a closure converted image. With full closure support, you can finally use recursive blocks like here: | fac | fac := [:n| n > 1 ifTrue:[n * (fac value: n-1)] ifFalse:[1]]. fac value: 5. "120" and close over temps correctly, such as here: (1 to: 10) do:[:i| UISupervisor whenUIinSafeState:[Transcript newLine; show: i]]. Another good example: | fib | fib := [:n| n < 2 ifTrue:[1] ifFalse:[(fib value:n-1) + (fib value:n-2)]]. fib value: 10. "89"

### Methods
#### BlockClosure>>#repeatWithGCIf: testBlock

run the receiver, and if testBlock returns true, garbage collect and run the receiver again


<details>
	<summary>See more</summary>
	
	repeatWithGCIf: testBlock
	| ans |
	"run the receiver, and if testBlock returns true, garbage collect and run the receiver again"
	ans := self value.
	(testBlock value: ans) ifTrue: [ Smalltalk garbageCollect. ans := self value ].
	^ans
</details>

#### BlockClosure>>#hasNonLocalReturn

Answer whether the receiver has a method-return ('^') in its code.


<details>
	<summary>See more</summary>
	
	hasNonLocalReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [:byte | (byte between: 120 and: 124) or: [scanner pc > end]].
	^scanner pc <= end
</details>

#### BlockClosure>>#repeat

Evaluate the receiver repeatedly, ending only if the block explicitly returns.


<details>
	<summary>See more</summary>
	
	repeat
	"Evaluate the receiver repeatedly, ending only if the block explicitly returns."

	[self value. true] whileTrue
</details>

#### BlockClosure>>#copyForSaving

Answer a copy of the receiver suitable for serialization. Notionally, if the receiver's outerContext has been returned from then nothing needs to be done and we can use the receiver. But there's a race condition determining if the receiver has been returned from (it could be executing in a different process). So answer a copy anyway.


<details>
	<summary>See more</summary>
	
	copyForSaving
	"Answer a copy of the receiver suitable for serialization.
	 Notionally, if the receiver's outerContext has been returned from then nothing
	 needs to be done and we can use the receiver. But there's a race condition
	 determining if the receiver has been returned from (it could be executing in a
	 different process). So answer a copy anyway."
	^self copy
</details>

#### BlockClosure>>#value

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the copied values to the activation as its copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	value
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 201>
	| newContext |
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#receiver

<details>
	<summary>See more</summary>
	
	receiver
	^outerContext receiver
</details>

#### BlockClosure>>#whileNotNil: aBlock

Unlike #whileTrue/False: this is not compiled inline.


<details>
	<summary>See more</summary>
	
	whileNotNil: aBlock 
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value == nil] whileFalse: [aBlock value]
</details>

#### BlockClosure>>#numArgs

Answer the number of arguments that must be used to evaluate this block


<details>
	<summary>See more</summary>
	
	numArgs
	"Answer the number of arguments that must be used to evaluate this block"

	^numArgs
</details>

#### BlockClosure>>#asSerializable

<details>
	<summary>See more</summary>
	
	asSerializable
	^SerializableBlockClosure onBlockClosure: self
</details>

#### BlockClosure>>#valueNoContextSwitch

An exact copy of BlockClosure>>value except that this version will not preempt the current process on block activation if a higher-priority process is runnable. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	valueNoContextSwitch
	"An exact copy of BlockClosure>>value except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 221>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed
</details>

#### BlockClosure>>#assert

<details>
	<summary>See more</summary>
	
	assert
	self assert: self
</details>

#### BlockClosure>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream nextPutAll: '[closure] in '.
	outerContext printOn: aStream
</details>

#### BlockClosure>>#whileTrue

Ordinarily compiled in-line, and therefore not overridable. This is in case the message is sent to other than a literal block. Evaluate the receiver, as long as its value is true.


<details>
	<summary>See more</summary>
	
	whileTrue
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the receiver, as long as its value is true."
 
	^ [self value] whileTrue: []
</details>

#### BlockClosure>>#fork

Create and schedule a Process running the code in the receiver.


<details>
	<summary>See more</summary>
	
	fork
	"Create and schedule a Process running the code in the receiver."
	
	"jmv - Do NOT answer the new process.
	
	See http://lists.squeakfoundation.org/pipermail/squeak-dev/2008-February/124960.html
	
	Most times, these methods return before resuming the new process (if priority of new process is less 
	or equal than current). But they might return afterwards.
	
	This means it is very dangerous to use the returned process in code that stores it in some variable 
	and checks for nil to start a new one. If these methods happen to return after the new process is forked,
	chances are the code that starts all this runs again, that variable is nil, and a second process is forked,
	perhaps breaking some shared state. This kind of bug is hard to spot and debug.
	
	Callers wanting the new process object, should call #newProcess, store the answer, and then #resume.
	
	A way to ensure this bug will not ever happen again is just to answer nil"

	self newProcess resume.
	^nil
</details>

#### BlockClosure>>#bench

See how many times I can value in 5 seconds. I'll answer a meaningful description. [ Float pi printString ] bench print. [ 80000 factorial printString ] bench print.


<details>
	<summary>See more</summary>
	
	bench
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description.
	[ Float pi printString ] bench print.
	[ 80000 factorial printString ] bench print.
	"

	| secondsPerRun startTime endTime count run |
	count _ 0.
	run _ true.
	[ (Delay forSeconds: 5) wait. run _ false ] forkAt: Processor timingPriority - 1.
	startTime _ Time localMillisecondClock.
	[ run ] whileTrue: [ self value. count _ count + 1 ].
	endTime _ Time localMillisecondClock.
	secondsPerRun _ (endTime - startTime) / (count * 1000).
	secondsPerRun >= 1
		ifTrue: [
			secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				^String streamContents: [ :strm |
					value printOn: strm fractionDigits: 2.
					strm
						space;
						nextPutAll: unitPrefixSymbol;
						nextPutAll: ' seconds per run']]
			]
		ifFalse: [
			1.0 / secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				^String streamContents: [ :strm |
					value printOn: strm fractionDigits: 2.
					strm
						space;
						nextPutAll: unitPrefixSymbol;
						nextPutAll: ' runs per second' ]]
			]
</details>

#### BlockClosure>>#whileNil: aBlock

Unlike #whileTrue/False: this is not compiled inline.


<details>
	<summary>See more</summary>
	
	whileNil: aBlock 
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value == nil] whileTrue: [ aBlock value ]
</details>

#### BlockClosure>>#whileFalse: aBlock

Ordinarily compiled in-line, and therefore not overridable. This is in case the message is sent to other than a literal block. Evaluate the argument, aBlock, as long as the value of the receiver is false.


<details>
	<summary>See more</summary>
	
	whileFalse: aBlock 
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the argument, aBlock, as long as the value of the receiver is false."

	^ [self value] whileFalse: [aBlock value]
</details>

#### BlockClosure>>#valueWithExit

<details>
	<summary>See more</summary>
	
	valueWithExit 
	  self value: [ ^nil ]
</details>

#### BlockClosure>>#size

Extract this closure's bytecode size (number of bytes) by accessing the closure creation bytecode in the enclosing method.


<details>
	<summary>See more</summary>
	
	size
	"Extract this closure's bytecode size (number of bytes) by accessing the closure
	creation bytecode in the enclosing method."
	
	^ ((self method at: self startpc - 2) bitShift: 8) + (self method at: self startpc - 1)
</details>

#### BlockClosure>>#valueWithPossibleArgs: anArray

Generally, prefer #valueWithPossibleArgument: and #valueWithPossibleArgument:and: for performance.


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgs: anArray
	"Generally, prefer #valueWithPossibleArgument: and #valueWithPossibleArgument:and:
	for performance."
	^ numArgs = 0
		ifTrue: [ self value ]
		ifFalse: [
			self valueWithArguments:
				(numArgs = anArray size
					ifTrue: [ anArray ]
					ifFalse: [
						numArgs > anArray size
							ifTrue: [ anArray , (Array new: numArgs - anArray size) ]
							ifFalse: [
								anArray
									copyFrom: 1
									to: numArgs ]]) ].
</details>

#### BlockClosure>>#durationToRun

Answer the duration taken to execute this block.


<details>
	<summary>See more</summary>
	
	durationToRun

	"Answer the duration taken to execute this block."



	^ Duration milliSeconds: self timeToRun




</details>

#### BlockClosure>>#isCleanClosure

A clean closure is one that doesn't really need the home context because: - It doesn't send messages to self or super - It doesn't access any instance variable - It doesn't access any outer temp - It doesn't do ^ return (return from method to caller) Therefore it doesn't close over a lexical scope, and in this sense they are trivial. They can also be called 'context free', 'clean' or 'simple block'.


<details>
	<summary>See more</summary>
	
	isCleanClosure
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free', 'clean' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!"
	recreated _ [ Compiler evaluate: source. ] on: UndeclaredVariableWarning do: [ :ex | ex return].
	recreated isNil ifTrue: [^false].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			anyClosureDoesMethodReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true
</details>

#### BlockClosure>>#on: exc1 do: block1 on: exc2 do: block2 on: exc3 do: block3

<details>
	<summary>See more</summary>
	
	on: exc1 do: block1 on: exc2 do: block2 on: exc3 do: block3

	^[
		self
			on: exc1
			do: block1 ]
		on: exc2
		do: block2
		on: exc3
		do: block3
</details>

#### BlockClosure>>#grabProcessorOnlyFor: milliseconds

Grab the CPU above most application processes and evaluate, but only for some milliseconds. Return to usual prority after that and finish evaluation if incomplete.


<details>
	<summary>See more</summary>
	
	grabProcessorOnlyFor: milliseconds
	"Grab the CPU above most application processes and evaluate, but only for some milliseconds.
	Return to usual prority after that and finish evaluation if incomplete."

	^self grabProcessorFor: milliseconds onTimeout: nil
</details>

#### BlockClosure>>#handles: anException

This allows a block to be the handling condition of an exception handling. See Exception class>>handles:


<details>
	<summary>See more</summary>
	
	handles: anException

	"This allows a block to be the handling condition of an exception handling.
	See Exception class>>handles:"
	
	^self value: anException 
</details>

#### BlockClosure>>#outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil

<details>
	<summary>See more</summary>
	
	outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil
	outerContext := aContext.
	startpc := aStartpc.
	numArgs := argCount.
	1 to: self numCopiedValues do:
		[:i|
		self at: i put: (anArrayOrNil at: i)]
</details>

#### BlockClosure>>#curried

https://en.wikipedia.org/wiki/Currying [ :a :b | a + b ] value: 1 value: 2 [ :a :b | a + b ] curried value: 1 :: value: 2


<details>
	<summary>See more</summary>
	
	curried
	"
	https://en.wikipedia.org/wiki/Currying
	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] curried value: 1 :: value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ :arg1 | [ self value: arg1 ]]].
		[ 2] -> [[ :arg1 | [ :arg2 | self value: arg1 value: arg2 ]]].
		[ 3] -> [[ :arg1 | [ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]]].
		[ 4] -> [[ :arg1 | [ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]]] }
	otherwise: [ self halt ]
</details>

#### BlockClosure>>#printStack: depth

<details>
	<summary>See more</summary>
	
	printStack: depth
	self print.
	depth > 0 ifTrue: [
		self sender ifNotNil: [ :s | s printStack: depth-1 ]]
</details>

#### BlockClosure>>#valueAt: blockPriority

Evaluate the receiver (block), with another priority as the actual one and restore it afterwards. The caller should be careful with using higher priorities.


<details>
	<summary>See more</summary>
	
	valueAt: blockPriority 
	"Evaluate the receiver (block), with another priority as the actual one 
	and restore it afterwards. The caller should be careful with using 
	higher priorities."
	| activeProcess result outsidePriority |
	activeProcess := Processor activeProcess.
	outsidePriority := activeProcess priority.
	activeProcess priority: blockPriority.
	result := self ensure: [activeProcess priority: outsidePriority].
	"Yield after restoring lower priority to give the preempted processes a  
	chance to run."
	blockPriority > outsidePriority
		ifTrue: [Processor yield].
	^ result
</details>

#### BlockClosure>>#numArgsError: numArgsForInvocation

<details>
	<summary>See more</summary>
	
	numArgsError: numArgsForInvocation

	| printNArgs |
	printNArgs := [:n| n printString, ' argument', (n = 1 ifTrue: [''] ifFalse:['s'])]. 
	self error: 
			'This block accepts ', (printNArgs value: numArgs), 
			', but was called with ', (printNArgs value: numArgsForInvocation), '.'
</details>

#### BlockClosure>>#ensure: aBlock

Evaluate a termination block after evaluating the receiver, regardless of whether the receiver's evaluation completes. N.B. This method is *not* implemented as a primitive. Primitive 198 always fails. The VM uses prim 198 in a context's method as the mark for an ensure:/ifCurtailed: activation.


<details>
	<summary>See more</summary>
	
	ensure: aBlock
	"Evaluate a termination block after evaluating the receiver, regardless of
	 whether the receiver's evaluation completes.  N.B.  This method is *not*
	 implemented as a primitive.  Primitive 198 always fails.  The VM uses prim
	 198 in a context's method as the mark for an ensure:/ifCurtailed: activation."

	| complete returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	complete ifNil:[
		complete := true.
		aBlock value.
	].
	^ returnValue
</details>

#### BlockClosure>>#home

<details>
	<summary>See more</summary>
	
	home
	^outerContext home
</details>

#### BlockClosure>>#valueWithPossibleArgument: anArg

Evaluate the block represented by the receiver. If the block requires one argument, use anArg


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgument: anArg 
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg"

	numArgs = 0 ifTrue: [ ^self value ].
	^self value: anArg
</details>

#### BlockClosure>>#isReceiverOrAnyArgumentGarbage

For use in the when:evaluate: protocol, i.e., foo when: #bar evaluate:[self handleBar]..


<details>
	<summary>See more</summary>
	
	isReceiverOrAnyArgumentGarbage
	"For use in the when:evaluate: protocol, i.e.,
 		foo when: #bar evaluate:[self handleBar].."
	^ false
</details>

#### BlockClosure>>#timeToRun

Answer the number of milliseconds taken to execute this block.


<details>
	<summary>See more</summary>
	
	timeToRun
	"Answer the number of milliseconds taken to execute this block."

	^ Time millisecondsToRun: self

</details>

#### BlockClosure>>#ifError: errorHandlerBlock

Evaluate the block represented by the receiver, and normally return it's value. If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned. The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver).


<details>
	<summary>See more</summary>
	
	ifError: errorHandlerBlock
	"Evaluate the block represented by the receiver, and normally return it's value.  If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned.  The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver)."
	"Examples:
		[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?'].
		[1 / 0] ifError: [:err :rcvr |
			'ZeroDivide' = err
				ifTrue: [Float infinity]
				ifFalse: [self error: err]]
"

	^ self on: Error do: [ :ex |
		errorHandlerBlock valueWithPossibleArgument: ex description and: ex receiver ]
</details>

#### BlockClosure>>#withFirstArg: arg1

https://en.wikipedia.org/wiki/Partial_application [ :a :b | a + b ] value: 1 value: 2 [ :a :b | a + b ] withFirstArg: 1 ([ :a :b | a + b ] withFirstArg: 1) value: 2 ([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2 (([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2) value ([ :a :b | a - b ] withFirstArg: 1) value: 2


<details>
	<summary>See more</summary>
	
	withFirstArg: arg1
	"
	https://en.wikipedia.org/wiki/Partial_application

	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] withFirstArg: 1
	([ :a :b | a + b ] withFirstArg: 1) value: 2
	([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2
	(([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2) value

	([ :a :b | a - b ] withFirstArg: 1) value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ self value: arg1 ]].
		[ 2] -> [[ :arg2 | self value: arg1 value: arg2 ]].
		[ 3] -> [[ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]].
		[ 4] -> [[ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]] }
	otherwise: [ self halt ]
</details>

#### BlockClosure>>#forkAt: priority

Create and schedule a Process running the code in the receiver at the given priority.


<details>
	<summary>See more</summary>
	
	forkAt: priority 
	"Create and schedule a Process running the code in the receiver at the given priority."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	| forkedProcess |
	forkedProcess _ self newProcess.
	forkedProcess priority: priority.
	forkedProcess resume.
	^nil
</details>

#### BlockClosure>>#value: firstArg

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the argument and copied values to the activation as its argument and copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	value: firstArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the argument and copied values to the activation
	 as its argument and copied temps. Primitive. Essential."
	<primitive: 202>
	| newContext |
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#whileFalse

Ordinarily compiled in-line, and therefore not overridable. This is in case the message is sent to other than a literal block. Evaluate the receiver, as long as its value is false.


<details>
	<summary>See more</summary>
	
	whileFalse
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the receiver, as long as its value is false."
 
	^ [self value] whileFalse: []
</details>

#### BlockClosure>>#outerContext

<details>
	<summary>See more</summary>
	
	outerContext
	^outerContext
</details>

#### BlockClosure>>#asMinimalRepresentation

For use in the when:evaluate: protocol, i.e., foo when: #bar evaluate:[self handleBar]. Return the receiver.


<details>
	<summary>See more</summary>
	
	asMinimalRepresentation
	"For use in the when:evaluate: protocol, i.e.,
		foo when: #bar evaluate:[self handleBar].
	Return the receiver."
	^self
</details>

#### BlockClosure>>#forkAndWait

Suspend current process and execute self in new process, when it completes resume current process


<details>
	<summary>See more</summary>
	
	forkAndWait
	"Suspend current process and execute self in new process, when it completes resume current process"

	| semaphore |
	semaphore := Semaphore new.
	[self ensure: [semaphore signal]] fork.
	semaphore wait.

</details>

#### BlockClosure>>#copiedValueAt: i

<details>
	<summary>See more</summary>
	
	copiedValueAt: i
	<primitive: 60>
	^self basicAt: i
</details>

#### BlockClosure>>#isBlock

<details>
	<summary>See more</summary>
	
	isBlock

	^ true
</details>

#### BlockClosure>>#grabProcessorFor: milliseconds onTimeout: timeoutBlock

Evaluate the receiver (block), without the possibility of preemption by regular priority processes. If not finished after milliseconds, restore original priority and evaluate timeoutBlock. Use with care!


<details>
	<summary>See more</summary>
	
	grabProcessorFor: milliseconds onTimeout: timeoutBlock
	"Evaluate the receiver (block), without the possibility of preemption by regular priority processes.
	If not finished after milliseconds, restore original priority and evaluate timeoutBlock.
	Use with care!"
	"Based on #valueUnpreemptively"
	
	| activeProcess oldPriority result done |
	activeProcess _ Processor activeProcess.
	oldPriority _ activeProcess priority.
	done _ false.
	
	activeProcess priority: Processor highIOPriority + Processor lowIOPriority // 2.
	milliseconds ifNotNil: [
		[
			(Delay forMilliseconds: milliseconds) wait.
			done ifFalse: [
				activeProcess
					suspend;
					priority: oldPriority;
					resume.
				timeoutBlock value ]
		] forkAt: Processor highIOPriority named: '#grabProcessorFor:onTimeout:' ].

	result _ self ensure: [
		done _ true.
		activeProcess priority: oldPriority].
	
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result
</details>

#### BlockClosure>>#method

<details>
	<summary>See more</summary>
	
	method
	^outerContext method
</details>

#### BlockClosure>>#asContext

Create a MethodContext that is ready to execute self. Assumes self takes no args (if it does the args will be nil)


<details>
	<summary>See more</summary>
	
	asContext
	"Create a MethodContext that is ready to execute self.  Assumes self takes no args (if it does the args will be nil)"

	^self asContextWithSender: nil
</details>

#### BlockClosure>>#sendsToSuper

Answer whether the receiver sends any message to super.


<details>
	<summary>See more</summary>
	
	sendsToSuper
	"Answer whether the receiver sends any message to super."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		(byte = 16r85 or: [
			byte = 16r84 and: [scanner followingByte between: 16r20 and: 16r3F]])
				or: [scanner pc > end]].
	^scanner pc <= end
</details>

#### BlockClosure>>#on: exc1 do: block1 on: exc2 do: block2

<details>
	<summary>See more</summary>
	
	on: exc1 do: block1 on: exc2 do: block2

	^[
		self
			on: exc1
			do: block1 ]
		on: exc2
		do: block2
</details>

#### BlockClosure>>#startpc

<details>
	<summary>See more</summary>
	
	startpc
	^startpc
</details>

#### BlockClosure>>#value: firstArg value: secondArg value: thirdArg

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the arguments and copied values to the activation as its arguments and copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	value: firstArg value: secondArg value: thirdArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 204>
	| newContext |
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			newContext at: 3 put: thirdArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#value: firstArg value: secondArg

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the arguments and copied values to the activation as its arguments and copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	value: firstArg value: secondArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 203>
	| newContext |
	numArgs ~= 2 ifTrue:
		[self numArgsError: 2].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#newProcessWith: anArray

Answer a Process running the code in the receiver. The receiver's block arguments are bound to the contents of the argument, anArray. The process is not scheduled.


<details>
	<summary>See more</summary>
	
	newProcessWith: anArray 
	"Answer a Process running the code in the receiver. The receiver's block 
	arguments are bound to the contents of the argument, anArray. The 
	process is not scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self valueWithArguments: anArray.
			Processor terminateActive] asContext
		priority: Processor activePriority
</details>

#### BlockClosure>>#valueUnpreemptively

Evaluate the receiver (block), without the possibility of preemption by higher priority processes. Use this facility VERY sparingly!


<details>
	<summary>See more</summary>
	
	valueUnpreemptively
	"Evaluate the receiver (block), without the possibility of preemption by higher priority processes. Use this facility VERY sparingly!"
	"Think about using Block>>valueUninterruptably first, and think about using Semaphore>>critical: before that, and think about redesigning your application even before that! 
	After you've done all that thinking, go right ahead and use it..."
	| activeProcess oldPriority result |
	activeProcess := Processor activeProcess.
	oldPriority := activeProcess priority.
	activeProcess priority: Processor highestPriority.
	result := self ensure: [activeProcess priority: oldPriority].
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result
</details>

#### BlockClosure>>#grabProcessor

Grab the CPU above most application processes and evaluate. Return to usual proprity after that.


<details>
	<summary>See more</summary>
	
	grabProcessor
	"Grab the CPU above most application processes and evaluate.
	Return to usual proprity after that."

	^self grabProcessorFor: nil onTimeout: nil
</details>

#### BlockClosure>>#ifCurtailed: aBlock

Evaluate the receiver with an abnormal termination action. Evaluate aBlock only if execution is unwound during execution of the receiver. If execution of the receiver finishes normally do not evaluate aBlock. N.B. This method is *not* implemented as a primitive. Primitive 198 always fails. The VM uses prim 198 in a context's method as the mark for an ensure:/ifCurtailed: activation.


<details>
	<summary>See more</summary>
	
	ifCurtailed: aBlock
	"Evaluate the receiver with an abnormal termination action.
	 Evaluate aBlock only if execution is unwound during execution
	 of the receiver.  If execution of the receiver finishes normally do
	 not evaluate aBlock.  N.B.  This method is *not* implemented as a
	 primitive.  Primitive 198 always fails.  The VM uses prim 198 in a
	 context's method as the mark for an ensure:/ifCurtailed: activation."
	| complete result |
	<primitive: 198>
	result := self valueNoContextSwitch.
	complete := true.
	^result
</details>

#### BlockClosure>>#forkAt: priority named: name

Create and schedule a Process running the code in the receiver at the given priority and having the given name.


<details>
	<summary>See more</summary>
	
	forkAt: priority named: name
	"Create and schedule a Process running the code in the receiver at the
	given priority and having the given name."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	| forkedProcess |
	forkedProcess := self newProcess.
	forkedProcess priority: priority.
	forkedProcess name: name.
	 forkedProcess resume.
	^nil
</details>

#### BlockClosure>>#valueNoContextSwitch: anArg

An exact copy of BlockClosure>>value: except that this version will not preempt the current process on block activation if a higher-priority process is runnable. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	valueNoContextSwitch: anArg
	"An exact copy of BlockClosure>>value: except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 222>
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed
</details>

#### BlockClosure>>#reentrant

Answer a version of the recever that can be reentered. Closures are reentrant (unlike BlockContect) so simply answer self.


<details>
	<summary>See more</summary>
	
	reentrant
	"Answer a version of the recever that can be reentered.
	 Closures are reentrant (unlike BlockContect) so simply answer self."
	^self
</details>

#### BlockClosure>>#valueWithin: aDuration onTimeout: timeoutBlock

Evaluate the receiver. If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead


<details>
	<summary>See more</summary>
	
	valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | 
			e tag == tag 
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]].
</details>

#### BlockClosure>>#simulateValueWithArguments: anArray caller: aContext

Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity.


<details>
	<summary>See more</summary>
	
	simulateValueWithArguments: anArray caller: aContext
	"Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity."
	| newContext sz |
	newContext := (MethodContext newForMethod: outerContext method)
						setSender: aContext
						receiver: outerContext receiver
						method: outerContext method
						closure: self
						startpc: startpc.
	((newContext objectClass: anArray) ~~ Array
	 or: [numArgs ~= anArray size]) ifTrue:
		[^ContextPart primitiveFailTokenFor: nil].
	sz := self basicSize.
	newContext stackp: sz + numArgs.
	1 to: numArgs do:
		[:i| newContext at: i put: (anArray at: i)].
	1 to: sz do:
		[:i| newContext at: i + numArgs put: (self at: i)].
	^newContext
</details>

#### BlockClosure>>#valueWithArguments: anArray

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the arguments in an anArray and copied values to the activation as its arguments and copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	valueWithArguments: anArray
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments in an anArray and copied values to
	 the activation as its arguments and copied temps. Primitive. Essential."
	<primitive: 206>
	| newContext |
	numArgs ~= anArray size ifTrue:
		[self numArgsError: anArray size].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			1 to: numArgs do:
				[:i| newContext at: i put: (anArray at: i)].
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a textual reference if possible. If not, attempt converting to a serializable object. This might also fail. See #onBlockClosure:


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	"I am about to be written on an object file.
	Write a textual reference if possible. If not, attempt converting to a serializable object.
	This might also fail. See #onBlockClosure:"

	self isCleanClosure ifTrue: [
		^ DiskProxy
			global: #Compiler
			selector: #evaluate:
			args: (Array with: self decompile decompileString) ].
	^self asSerializable
</details>

#### BlockClosure>>#timeToRunWithoutGC

Answer the number of milliseconds taken to execute this block without GC time.


<details>
	<summary>See more</summary>
	
	timeToRunWithoutGC
	"Answer the number of milliseconds taken to execute this block without GC time."

	^(Smalltalk vmParameterAt: 8) + 
		(Smalltalk vmParameterAt: 10) +
		self timeToRun -
		(Smalltalk vmParameterAt: 8) - 
		(Smalltalk vmParameterAt: 10)

</details>

#### BlockClosure>>#numCopiedValues

Answer the number of copied values of the receiver. Since these are stored in the receiver's indexable fields this is the receiver's basic size. Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript.


<details>
	<summary>See more</summary>
	
	numCopiedValues
	"Answer the number of copied values of the receiver.  Since these are
	 stored in the receiver's indexable fields this is the receiver's basic size.
	 Primitive. Answer the number of indexable variables in the receiver. 
	 This value is the same as the largest legal subscript."

	<primitive: 62>
	^self basicSize
</details>

#### BlockClosure>>#argumentCount

Answers the number of arguments needed to evaluate the receiver. For ansi compatibility.


<details>
	<summary>See more</summary>
	
	argumentCount
	"Answers the number of arguments needed to evaluate the receiver.
	For ansi compatibility."
	^ self numArgs
</details>

#### BlockClosure>>#forkNamed: aString

Create and schedule a Process running the code in the receiver and having the given name.


<details>
	<summary>See more</summary>
	
	forkNamed: aString
	"Create and schedule a Process running the code in the receiver and
	having the given name."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	self newProcess name: aString; resume.
	^nil
</details>

#### BlockClosure>>#isDead

Has self finished


<details>
	<summary>See more</summary>
	
	isDead
	"Has self finished"
	^false
</details>

#### BlockClosure>>#newProcess

Answer a Process running the code in the receiver. The process is not scheduled.


<details>
	<summary>See more</summary>
	
	newProcess
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self value.
			Processor terminateActive] asContext
		priority: Processor activePriority
</details>

#### BlockClosure>>#decompile

<details>
	<summary>See more</summary>
	
	decompile
	^Decompiler new decompileBlock: self
</details>

#### BlockClosure>>#on: exception do: handlerAction

Evaluate the receiver in the scope of an exception handler. The following primitive is just a marker used to find the error handling context. See MethodContext>>#isHandlerOrSignalingContext.


<details>
	<summary>See more</summary>
	
	on: exception do: handlerAction
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value
</details>

#### BlockClosure>>#outerContextsDo: aBlock

<details>
	<summary>See more</summary>
	
	outerContextsDo: aBlock

	outerContext outerContextsDo: aBlock
</details>

#### BlockClosure>>#capturedValues

<details>
	<summary>See more</summary>
	
	capturedValues
	| n copiedValues |
	n _ self numCopiedValues.
	copiedValues _ Array new: n.
	1 to: n do: [ :i |
		copiedValues at: i put: (self copiedValueAt: i) ].
	^copiedValues
</details>

#### BlockClosure>>#asContextWithSender: aContext

Inner private support method for evaluation. Do not use unless you know what you're doing.


<details>
	<summary>See more</summary>
	
	asContextWithSender: aContext
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^(MethodContext newForMethod: outerContext method)
		setSender: aContext
		receiver: outerContext receiver
		method: outerContext method
		closure: self
		startpc: startpc;
		privRefresh
</details>

#### BlockClosure>>#valueWithPossibleArgument: anArg and: secondArg

Evaluate the block represented by the receiver. If the block requires one argument, use anArg, if it requires two, use anArg and secondArg. Squeak uses #cull:, #cull:cull:, etc. I (jmv) find that name quite obscure


<details>
	<summary>See more</summary>
	
	valueWithPossibleArgument: anArg and: secondArg
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg, 
	if it requires two, use anArg and secondArg.
	Squeak uses #cull:, #cull:cull:, etc. I (jmv) find that name quite obscure"

	numArgs = 0 ifTrue: [ ^self value ].
	numArgs = 1 ifTrue: [ ^self value: anArg ].
	^self value: anArg value: secondArg
</details>

#### BlockClosure>>#endpc

Determine end of block from the instruction preceding it. Find the instruction by using an MNU handler to capture the instruction message sent by the scanner.


<details>
	<summary>See more</summary>
	
	endpc
	"Determine end of block from the instruction preceding it.
	 Find the instruction by using an MNU handler to capture
	 the instruction message sent by the scanner."
	| myMethod scanner preceedingBytecodeMessage end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: myMethod initialPC.
	[scanner pc < startpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| preceedingBytecodeMessage := ex message]].
	end := preceedingBytecodeMessage arguments last + startpc - 1.
	^end
</details>

#### BlockClosure>>#onDNU: selector do: handleBlock

Catch MessageNotUnderstood exceptions but only those of the given selector (DNU stands for doesNotUnderstand:)


<details>
	<summary>See more</summary>
	
	onDNU: selector do: handleBlock
	"Catch MessageNotUnderstood exceptions but only those of the given selector (DNU stands for doesNotUnderstand:)"

	^ self on: MessageNotUnderstood do: [ :exception |
		exception message selector = selector
			ifTrue: [ handleBlock valueWithPossibleArgument: exception ]
			ifFalse: [ exception pass ]
	  ]
</details>

#### BlockClosure>>#isClosure

<details>
	<summary>See more</summary>
	
	isClosure
	^true
</details>

#### BlockClosure>>#timeProfile

<details>
	<summary>See more</summary>
	
	timeProfile

	^TimeProfileBrowser onBlock: self
</details>

#### BlockClosure>>#value: firstArg value: secondArg value: thirdArg value: fourthArg

Activate the receiver, creating a closure activation (MethodContext) whose closure is the receiver and whose caller is the sender of this message. Supply the arguments and copied values to the activation as its arguments and copied temps. Primitive. Essential.


<details>
	<summary>See more</summary>
	
	value: firstArg value: secondArg value: thirdArg value: fourthArg
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 205>
	| newContext |
	numArgs ~= 4 ifTrue:
		[self numArgsError: 4].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			newContext at: 3 put: thirdArg.
			newContext at: 4 put: fourthArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
</details>

#### BlockClosure>>#whileTrue: aBlock

Ordinarily compiled in-line, and therefore not overridable. This is in case the message is sent to other than a literal block. Evaluate the argument, aBlock, as long as the value of the receiver is true.


<details>
	<summary>See more</summary>
	
	whileTrue: aBlock 
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the argument, aBlock, as long as the value of the receiver is true."

	^ [self value] whileTrue: [aBlock value]
</details>

#### BlockClosure>>#sender

Answer the context that sent the message that created the receiver.


<details>
	<summary>See more</summary>
	
	sender
	"Answer the context that sent the message that created the receiver."

	^outerContext sender
</details>

## BlockStartLocator

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### BlockStartLocator>>#jump: offset

If this jump is around a block answer the size of that block.


<details>
	<summary>See more</summary>
	
	jump: offset
	"If this jump is around a block answer the size of that block."

	nextJumpIsAroundBlock ifTrue:
		[nextJumpIsAroundBlock := false.
		 ^offset]
</details>

#### BlockStartLocator>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	nextJumpIsAroundBlock := false
</details>

#### BlockStartLocator>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Answer the size of the block


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Answer the size of the block"
	^blockSize
</details>

#### BlockStartLocator>>#send: selector super: supered numArgs: numberArguments

Send Message With Selector, selector, bytecode. The argument, supered, indicates whether the receiver of the message is specified with 'super' in the source method. The arguments of the message are found in the top numArguments locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: supered numArgs: numberArguments
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector
</details>

## ClosureExtractor

A ClosureExtractor is a utility class that is used to extract all BlockClosures from a CompiledMethod. It inherits from InstructionClient and understands only one single message, namely that corresponding to the push closure bytecode instruction. Being sent this message, a ClosureExtractor will create a BlockClosure instance and evaluate the block it holds as an instance variable with that closure as parameter.

### Methods
#### ClosureExtractor>>#blockReturnTop

Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	blockReturnTop
	currentContext := currentContext sender
</details>

#### ClosureExtractor>>#scanner

<details>
	<summary>See more</summary>
	
	scanner
	^ scanner
</details>

#### ClosureExtractor>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Create a BlockClosure corresponding to the closure bytecode and execute the action block with it. The created BlockClosure is only a pseudo value, it is not populated with meaningful context and argument information.


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Create a BlockClosure corresponding to the closure bytecode
	 and execute the action block with it. The created BlockClosure is only a pseudo value,
	 it is not populated with meaningful context and argument information."
	| block |
	block := BlockClosure
				outerContext: currentContext
				startpc: scanner pc
				numArgs: numArgs
				copiedValues: (Array new: numCopied)..
	currentContext := block asContextWithSender: currentContext.
	action value: block
</details>

#### ClosureExtractor>>#action: aBlock

<details>
	<summary>See more</summary>
	
	action: aBlock
	action := aBlock
</details>

#### ClosureExtractor>>#scanner: anInstructionStream

<details>
	<summary>See more</summary>
	
	scanner: anInstructionStream
	scanner := anInstructionStream.
	currentContext := MethodContext
							sender: nil
							receiver: self
							method: scanner method
							arguments: (Array new: scanner method numArgs)
</details>

#### ClosureExtractor>>#action

<details>
	<summary>See more</summary>
	
	action
	^ action
</details>

## CompiledMethod

My instances are methods suitable for interpretation by the virtual machine. This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields. The current format of a CompiledMethod is as follows: header (4 bytes) literals (4 bytes each) bytecodes (variable) trailer (variable) The header is a 30-bit integer with the following format: (index 0) 9 bits: main part of primitive number (#primitive) (index 9) 8 bits: number of literals (#numLiterals) (index 17) 1 bit: whether a large frame size is needed (#frameSize) (index 18) 6 bits: number of temporary variables (#numTemps) (index 24) 4 bits: number of arguments to the method (#numArgs) (index 28) 1 bit: high-bit of primitive number (#primitive) (index 29) 1 bit: flag bit, ignored by the VM (#flag) The trailer has two variant formats. In the first variant, the last byte is at least 252 and the last four bytes represent a source pointer into one of the sources files (see #sourcePointer). In the second variant, the last byte is less than 252, and the last several bytes are a compressed version of the names of the method's temporary variables. The number of bytes used for this purpose is the value of the last byte in the method.

### Methods
#### CompiledMethod>>#setSourcePointer: srcPointer

<details>
	<summary>See more</summary>
	
	setSourcePointer: srcPointer
	srcPointer = 0 ifTrue: [
		0 to: 3 do: [ :i |
			self at: self size-i put: 0].
		^self].
	(srcPointer between: 16r1000000 and: 16r4FFFFFF) ifFalse: [self error: 'Source pointer out of range'].
	self at: self size put: (srcPointer bitShift: -24) + 251.
	1 to: 3 do: [:i |
		self at: self size-i put: ((srcPointer bitShift: (i-3)*8) bitAnd: 16rFF)]
</details>

#### CompiledMethod>>#dateMethodLastSubmitted

Answer a Date object indicating when a method was last saved. If there is no date stamp, return nil


<details>
	<summary>See more</summary>
	
	dateMethodLastSubmitted
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSubmitted) dateMethodLastSubmitted
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]
</details>

#### CompiledMethod>>#tempsSubSequenceFrom: tempNamesStream

<details>
	<summary>See more</summary>
	
	tempsSubSequenceFrom: tempNamesStream
	^Array streamContents:
		[:tsss|
		[tempNamesStream skipSeparators.
		 tempNamesStream atEnd
		 or: ['[]()' includes: tempNamesStream peek]] whileFalse:
			[tsss nextPut: (String streamContents:
							[:s|
							[s nextPut: tempNamesStream next.
							 tempNamesStream peek
								ifNil: [true]
								ifNotNil: [:peek| ' []()' includes: peek]] whileFalse])]]

	"thisContext method tempsSubSequenceFrom: 'les temps perdu(sont n''est pas la)' readStream"
	"thisContext method tempsSubSequenceFrom: ('les temps perdu(sont n''est pas la)' readStream skipTo: $(; yourself)"
</details>

#### CompiledMethod>>#primitiveErrorVariableName

Answer the primitive error code temp name, or nil if none.


<details>
	<summary>See more</summary>
	
	primitiveErrorVariableName
	"Answer the primitive error code temp name, or nil if none."
	self primitive > 0 ifTrue:
		[self pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil
</details>

#### CompiledMethod>>#methodClass

answer the class that I am installed in


<details>
	<summary>See more</summary>
	
	methodClass
	"answer the class that I am installed in"
	^(self literalAt: self numLiterals) value.
</details>

#### CompiledMethod>>#isReturnSpecial

Answer whether the receiver is a quick return of self or constant.


<details>
	<summary>See more</summary>
	
	isReturnSpecial
	"Answer whether the receiver is a quick return of self or constant."

	^ self primitive between: 256 and: 263
</details>

#### CompiledMethod>>#xtraBindings

Used for Workspace variables


<details>
	<summary>See more</summary>
	
	xtraBindings
	"Used for Workspace variables"
	^Array streamContents: [ :strm |
		self literalsDo: [ :literal |
			(literal class == Association and: [ literal key isString and: [ literal key first isLowercase ]]) ifTrue: [
				strm nextPut: literal ]]]
</details>

#### CompiledMethod>>#accessesInstanceVariable: aName

<details>
	<summary>See more</summary>
	
	accessesInstanceVariable: aName

	^(self readsInstanceVariable: aName) or: [self writesInstanceVariable: aName].
</details>

#### CompiledMethod>>#indexOfLiteral: literal

Answer the literal index of the argument, literal, or zero if none.


<details>
	<summary>See more</summary>
	
	indexOfLiteral: literal
	"Answer the literal index of the argument, literal, or zero if none."
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do:
		[:index |
		literal == (self objectAt: index) ifTrue: [^index - 1]].
	^0
</details>

#### CompiledMethod>>#methodForDecompile

This is a hook to allow recursive methods like MwMethodWrapper to avoid infinite recursion.


<details>
	<summary>See more</summary>
	
	methodForDecompile
	"This is a hook to allow recursive methods like MwMethodWrapper to avoid infinite recursion."
	^self
</details>

#### CompiledMethod>>#dateSortingValue

Answer an integer that is suitable for chronologically sorting methods. It is the number of whole minutes since 'The dawn of Squeak history'


<details>
	<summary>See more</summary>
	
	dateSortingValue
	"Answer an integer that is suitable for chronologically sorting methods.
	It is the number of whole minutes since 'The dawn of Squeak history'
	"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateSortingValue
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]]
</details>

#### CompiledMethod>>#printOn: aStream

Overrides method inherited from the byte arrayed collection.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Overrides method inherited from the byte arrayed collection."

	aStream nextPut: $(.
	self printClassAndSelectorOn: aStream.
	aStream space; nextPut: $".
	self printNameOn: aStream.
	aStream nextPut: $(; print: self identityHash; nextPut: $); nextPut: $"; nextPut: $)
</details>

#### CompiledMethod>>#defaultSelector

Invent and answer an appropriate message selector (a Symbol) for me, that is, one that will parse with the correct number of arguments.


<details>
	<summary>See more</summary>
	
	defaultSelector 
	
	"Invent and answer an appropriate message selector (a Symbol) for me, 
	that is, one that will parse with the correct number of arguments."
	
	^Scanner doItSelector numArgs: self numArgs
</details>

#### CompiledMethod>>#embeddedBlockClosures

(CompiledMethod >> #embeddedBlockClosures)embeddedBlockClosures


<details>
	<summary>See more</summary>
	
	embeddedBlockClosures
	"
	(CompiledMethod >> #embeddedBlockClosures)embeddedBlockClosures
	"
	| bms extractor scanner |
	bms := OrderedCollection new.
	scanner := self scanner.
	extractor := ClosureExtractor withAction: [ :c | bms add: c ] andScanner: scanner.
	[ scanner pc <= self endPC ] whileTrue: [ scanner interpretNextInstructionFor: extractor ].
	^ bms
</details>

#### CompiledMethod>>#timeStamp

Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available. (CompiledMethod compiledMethodAt: #timeStamp) timeStamp


<details>
	<summary>See more</summary>
	
	timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available.
	(CompiledMethod compiledMethodAt: #timeStamp) timeStamp
	"
	| preamble tokens |
	preamble _ self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens _ Scanner new scanTokens: preamble.
	^CompiledMethod stampFrom: tokens
</details>

#### CompiledMethod>>#literalStrings

<details>
	<summary>See more</summary>
	
	literalStrings
	| litStrs |
	litStrs := OrderedCollection new: self numLiterals.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs
</details>

#### CompiledMethod>>#getPreambleFrom: aFileStream at: position

<details>
	<summary>See more</summary>
	
	getPreambleFrom: aFileStream at: position
	|  writeStream c p |
	writeStream _ String new writeStream.
	p _ position.
	c _ nil.
	aFileStream position: p.
	aFileStream atEnd ifTrue: [ ^ nil ].
	[ p >= 0 and: [ c ~~ $! ]] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		p _ p - 1 ].
	[ p >= 0] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		c == $!
			ifTrue: [^ writeStream contents reverse ]
			ifFalse: [ writeStream nextPut: c ].
		p _ p - 1 ].
	^ nil
</details>

#### CompiledMethod>>#scanVeryLongStore: extension offset: offset

Answer whether the receiver contains a long load with the given offset. Note that the constant +32 is the known difference between a store and a storePop for instVars, and it will always fail on literal variables, but these only use store (followed by pop) anyway.


<details>
	<summary>See more</summary>
	
	scanVeryLongStore: extension offset: offset
	"Answer whether the receiver contains a long load with the given offset.
	Note that the constant +32 is the known difference between a
	store and a storePop for instVars, and it will always fail on literal variables,
	but these only use store (followed by pop) anyway."
	| scanner |
	scanner := InstructionStream on: self.
	^scanner scanFor:
		[:instr | | ext |
		(instr = 132 and: [(ext := scanner followingByte) = extension
											or: ["might be a store/pop into rcvr"
												ext = (extension+32)]])
		and: [scanner thirdByte = offset]]
</details>

#### CompiledMethod>>#category

<details>
	<summary>See more</summary>
	
	category

	^self methodClass organization categoryOfElement: self selector
</details>

#### CompiledMethod>>#isSetterOf: anInstVarName at: anInstVarIndex

<details>
	<summary>See more</summary>
	
	isSetterOf: anInstVarName at: anInstVarIndex
	
	| varIndexCode scanner |
	
	self isQuick ifTrue: [ ^false ].
	self selector = (anInstVarName, ':') ifFalse: [ ^false ].
	
	"I could have use the AST of the method, but parsing the source code could generate errors
	that it is why I decided to check the bytecodes - Hernan"
	varIndexCode := anInstVarIndex - 1.
	scanner := InstructionStream on: self.
	scanner nextByte = 16r10 ifFalse: [ ^false ].
	scanner movePcForward.
	(self writesFieldCode: varIndexCode with: scanner nextByte using: scanner) ifFalse: [ ^false ].
	scanner movePcForward.
	^scanner nextByte = 16r78

	
</details>

#### CompiledMethod>>#linesOfCode

An approximate measure of lines of code. Includes comments, but excludes blank lines.


<details>
	<summary>See more</summary>
	
	linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	| lines |
	lines _ 0.
	self getSource lineIndicesDo: [ :start :endWithoutDelimiters :end |
		endWithoutDelimiters - start > 0 ifTrue: [
			lines _ lines+1 ]].
	^lines
</details>

#### CompiledMethod>>#author

(CompiledMethod compiledMethodAt: #timeStamp) author (BitBlt>>#destRect) author


<details>
	<summary>See more</summary>
	
	author
	"
	(CompiledMethod compiledMethodAt: #timeStamp) author
	(BitBlt>>#destRect) author
	"
	
	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | authorInitials ]
</details>

#### CompiledMethod>>#removeProperty: propName ifAbsent: aBlock

Remove the property propName if it exists. Answer the evaluation of aBlock if the property is missing.


<details>
	<summary>See more</summary>
	
	removeProperty: propName ifAbsent: aBlock
	"Remove the property propName if it exists.
	 Answer the evaluation of aBlock if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^aBlock value].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value
</details>

#### CompiledMethod>>#isReturnSelf

Answer whether the receiver is a quick return of self.


<details>
	<summary>See more</summary>
	
	isReturnSelf
	"Answer whether the receiver is a quick return of self."

	^ self primitive = 256
</details>

#### CompiledMethod>>#zapSourcePointer

clobber the source pointer since it will be wrong


<details>
	<summary>See more</summary>
	
	zapSourcePointer

	"clobber the source pointer since it will be wrong"
	0 to: 3 do: [ :i | self at: self size - i put: 0].

</details>

#### CompiledMethod>>#storeDataOn: aDataStream

Store myself on a DataStream. I am a mixture of objects and raw data bytes. Only use this for blocks. Normal methodDictionaries should not be put out using ReferenceStreams. Their fileOut should be attached to the beginning of the file.


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"Store myself on a DataStream.  I am a mixture of objects and raw data bytes.  Only use this for blocks.  Normal methodDictionaries should not be put out using ReferenceStreams.  Their fileOut should be attached to the beginning of the file."

	| byteLength lits |
	"No inst vars of the normal type"
	byteLength _ self basicSize.
	aDataStream
		beginInstance: self class
		size: byteLength.
	lits _ self numLiterals + 1.	"counting header"
	1 to: lits do:
		[:ii | aDataStream nextPut: (self objectAt: ii)].
	lits*Smalltalk wordSize+1 to: byteLength do:
		[:ii | aDataStream byteStream nextPut: (self basicAt: ii)].
			"write bytes straight through to the file"
</details>

#### CompiledMethod>>#methodNode: aMethodNode

<details>
	<summary>See more</summary>
	
	methodNode: aMethodNode

	self propertyValueAt: #methodNode put: aMethodNode
</details>

#### CompiledMethod>>#readsInstanceVariable: aName

<details>
	<summary>See more</summary>
	
	readsInstanceVariable: aName

	^self readsField: (self methodClass indexOfInstanceVariable: aName) 
</details>

#### CompiledMethod>>#objectAt: index

Primitive. Answer the method header (if index=1) or a literal (if index >1) from the receiver. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	objectAt: index 
	"Primitive. Answer the method header (if index=1) or a literal (if index 
	>1) from the receiver. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed
</details>

#### CompiledMethod>>#getSourceFor: selector in: class

Retrieve or reconstruct the source code for this method.


<details>
	<summary>See more</summary>
	
	getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."

	| flagByte source |

	flagByte := self last.
	
	"If no source pointer..."
	source := flagByte < 252 
		ifTrue: [ nil ]
		ifFalse: [ 
			"Situation normal; read the sourceCode from the file
			An error can happen here if, for example, the changes file has been truncated by an aborted download.  
			The present solution is to ignore the error and fall back on the decompiler.  
			A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  
			Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling 
			into this error case, random source code will get returned."
			[self getSourceFromFile]
				on: Error
				do: [ :ex | ex return: nil]].
		
	"If source code not available, use DoIt source code or if absent decompile blind (no temps)"
	^source ifNil: [ (class decompilerClass new decompile: selector in: class method: self) decompileString ]
</details>

#### CompiledMethod>>#longPrintOn: aStream indent: tabs

List of all the byte codes in a method with a short description of each


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[self isReturnSpecial ifTrue:
			[^ aStream tab: tabs; nextPutAll: 'Quick return ' , 
				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].
		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].

	self primitive = 0 ifFalse: [
		aStream tab: tabs.
		self printPrimitiveOn: aStream.
	].
	(InstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.

</details>

#### CompiledMethod>>#decompilerClass

<details>
	<summary>See more</summary>
	
	decompilerClass
	^self compilerClass decompilerClass
</details>

#### CompiledMethod>>#hasLiteral: literal

Answer whether the receiver references the argument, literal.


<details>
	<summary>See more</summary>
	
	hasLiteral: literal
	"Answer whether the receiver references the argument, literal."
	2 to: self numLiterals - 1 do: "exclude superclass + selector/properties"
		[:index |
		((self objectAt: index) literalEqual: literal) ifTrue: [^true]].
	^false
</details>

#### CompiledMethod>>#removeProperties

<details>
	<summary>See more</summary>
	
	removeProperties
	
	self penultimateLiteral: self selector
</details>

#### CompiledMethod>>#properties

Answer the method properties of the receiver.


<details>
	<summary>See more</summary>
	
	properties

	"Answer the method properties of the receiver."
	
	^self 
		withPropertiesDo: [ :properties | properties ] 
		ifSelector: [ :selector | AdditionalMethodState forMethod: self selector: selector ]
</details>

#### CompiledMethod>>#getSource

<details>
	<summary>See more</summary>
	
	getSource
	^ self getSourceFor: self selector in:self methodClass.
</details>

#### CompiledMethod>>#getSourceFromFile

Read the source code from file, determining source file index and file position from the last 3 bytes of this method.


<details>
	<summary>See more</summary>
	
	getSourceFromFile
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."
	| position |
	(position _ self filePosition) = 0 ifTrue: [^ nil].
	^ (RemoteString newFileNumber: self fileIndex position: position)
			text
</details>

#### CompiledMethod>>#dynamicTypingAutoCompleterDocumentation

<details>
	<summary>See more</summary>
	
	dynamicTypingAutoCompleterDocumentation
	
	^ self 
		autoCompleterDocumentationAppendingToParameter: [ :parameterName | '' ]
		toReturn: [ '' ]
</details>

#### CompiledMethod>>#scanFor: byteOrClosure

Answer whether the receiver contains the argument as a bytecode, if it is a number, or evaluates to true if a block.


<details>
	<summary>See more</summary>
	
	scanFor: byteOrClosure
	"Answer whether the receiver contains the argument as a bytecode,
	 if it is a number, or evaluates to true if a block."
	^ (InstructionStream on: self) scanFor: (byteOrClosure isBlock
													ifTrue: [byteOrClosure]
													ifFalse: [[:instr | instr = byteOrClosure]])
"
Smalltalk browseAllSelect: [:m | m scanFor: 134]
"
</details>

#### CompiledMethod>>#numTemps

Answer the number of temporary variables used by the receiver.


<details>
	<summary>See more</summary>
	
	numTemps
	"Answer the number of temporary variables used by the receiver."
	
	^ (self header bitShift: -18) bitAnd: 16r3F
</details>

#### CompiledMethod>>#selectorAndArgumentsAsString

<details>
	<summary>See more</summary>
	
	selectorAndArgumentsAsString
	
	^self methodNode selectorAndArgumentsAsString 
</details>

#### CompiledMethod>>#printPrimitiveOn: aStream

Print the primitive on aStream


<details>
	<summary>See more</summary>
	
	printPrimitiveOn: aStream
	"Print the primitive on aStream"
	| primIndex primDecl |
	(primIndex := self primitive) = 0 ifTrue:
		[^self].
	primIndex = 120 ifTrue: "External call spec"
		[^aStream print: (self literalAt: 1); newLine].
	aStream nextPutAll: '<primitive: '.
	primIndex = 117
		ifTrue: [
			primDecl := self literalAt: 1.
			(primDecl at: 2) asString printOn: aStream.
			(primDecl at: 1) ifNotNil: [ :moduleName |
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse: [
			aStream print: primIndex].
	self primitiveErrorVariableName ifNotNil: [ :primitiveErrorVariableName |
		aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; newLine
</details>

#### CompiledMethod>>#sendsToSuper

Answer whether the receiver sends any message to super.


<details>
	<summary>See more</summary>
	
	sendsToSuper
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner _ InstructionStream on: self.
	^ scanner scanFor: 
		[:instr |  instr = 16r85 or: [instr = 16r84
						and: [scanner followingByte between: 16r20 and: 16r3F]]]
</details>

#### CompiledMethod>>#setSourcePosition: position inFile: fileIndex

<details>
	<summary>See more</summary>
	
	setSourcePosition: position inFile: fileIndex 
	self setSourcePointer: (SourceFiles sourcePointerFromFileIndex: fileIndex andPosition: position)
</details>

#### CompiledMethod>>#dateAndTime

Answer a DateAndTime object indicating when a method was last saved. If there is no date stamp, return nil


<details>
	<summary>See more</summary>
	
	dateAndTime
	"Answer a DateAndTime object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateAndTime
	"

	^self class timeStamp: self timeStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]
</details>

#### CompiledMethod>>#penultimateLiteral: anObject

Answer the penultimate literal of the receiver, which holds either the receiver's selector or its properties (which will hold the selector).


<details>
	<summary>See more</summary>
	
	penultimateLiteral: anObject
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex put: anObject]
		ifFalse: [self error: 'insufficient literals']
</details>

#### CompiledMethod>>#containsBlockClosures

<details>
	<summary>See more</summary>
	
	containsBlockClosures
	^ self scanner scanFor: [ :bc | bc = 143 "push closure bytecode" ]
</details>

#### CompiledMethod>>#withPropertiesDo: withBlock

<details>
	<summary>See more</summary>
	
	withPropertiesDo: withBlock

	^self withPropertiesDo: withBlock ifSelector: [ :selector | nil ]
</details>

#### CompiledMethod>>#bytecodeSetName

<details>
	<summary>See more</summary>
	
	bytecodeSetName
	^self encoderClass name copyReplaceAll: 'EncoderFor' with: ''
</details>

#### CompiledMethod>>#commentAutoCompleterDocumentationOf: comment

<details>
	<summary>See more</summary>
	
	commentAutoCompleterDocumentationOf: comment
 	
	^ Text 
		string: (String streamContents: [ :stream |				
			stream
				newLine; newLine;
				nextPutAll: comment first ])
		attributes: (SHTextStylerST80 attributesFor: #comment)
</details>

#### CompiledMethod>>#compilerClass

<details>
	<summary>See more</summary>
	
	compilerClass
	^self methodClass 
		ifNil: [Compiler] 
		ifNotNil: [:class | class compilerClass].
</details>

#### CompiledMethod>>#clearFlag

Clear the user-level flag bit


<details>
	<summary>See more</summary>
	
	clearFlag
	"Clear the user-level flag bit"

	self objectAt: 1 put: (self header bitAnd: (1 << 29) bitInvert)
</details>

#### CompiledMethod>>#searchForSelector

search me in all classes, if found, return my selector. Slow!


<details>
	<summary>See more</summary>
	
	searchForSelector
	"search me in all classes, if found, return my selector. Slow!"

	Smalltalk allBehaviorsDo: [:class | 
		(class methodDict keyAtIdentityValue: self ifAbsent: nil) ifNotNil: [ :selector | ^selector]].
	^nil
</details>

#### CompiledMethod>>#sendsOrRefersTo: aSelector

<details>
	<summary>See more</summary>
	
	sendsOrRefersTo: aSelector

	^ (self hasLiteralThorough: aSelector) or: [ self sendsSelector: aSelector ]
</details>

#### CompiledMethod>>#autoCompleterDocumentationAppendingToParameter: aParameterAppendBlock toReturn: aReturnAppendBlock

This message is sent either by the dynamic typing or live typing auto complete. If you do not have live typing installed you will see one sender, do not refactor it! - Hernan


<details>
	<summary>See more</summary>
	
	autoCompleterDocumentationAppendingToParameter: aParameterAppendBlock toReturn: aReturnAppendBlock
	
	"This message is sent either by the dynamic typing or live typing auto complete. If you do not have 
	live typing installed you will see one sender, do not refactor it! - Hernan"
	
	| methodNode text |
 
	text := self receiverTextAutoCompleterDocumentation.
	
	methodNode := self methodNode.
	text := self selectorAutoCompleterDocumentationAppendingTo: text using: methodNode appendingToParameter: aParameterAppendBlock.
	text := text append: aReturnAppendBlock value.
	text := self commentAutoCompleterDocumentationAppendigTo: text using: methodNode.
			
	^text
</details>

#### CompiledMethod>>#trailer

<details>
	<summary>See more</summary>
	
	trailer

	| end trailer |
	end _ self endPC.
	trailer _ ByteArray new: self size - end.
	end + 1 to: self size do: [:i | 
		trailer at: i - end put: (self at: i)].
	^ trailer
</details>

#### CompiledMethod>>#sourceFileStream

Answer the sources file stream with position set at the beginning of my source string


<details>
	<summary>See more</summary>
	
	sourceFileStream 
	"Answer the sources file stream with position set at the beginning of my source string"

	| pos |
	(pos _ self filePosition) = 0 ifTrue: [^ nil].
	^ (RemoteString newFileNumber: self fileIndex position: pos) fileStream
</details>

#### CompiledMethod>>#allLiterals

<details>
	<summary>See more</summary>
	
	allLiterals
	^self literals
</details>

#### CompiledMethod>>#= method

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= method
	| numLits lit1 lit2 firstLitIndex |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits _ self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false  ].

	firstLitIndex _ 1.
	(#(117 120) includes: self primitive) ifTrue: [
		lit1 _ self literalAt: firstLitIndex.
		lit2 _ method literalAt: firstLitIndex.
			lit1 isArray
				ifTrue: [
					(lit2 isArray and: [ lit1 first = lit2 first and: [lit1 second = lit2 second]]) ifFalse: [
						^false ]]
				ifFalse: [ "ExternalLibraryFunction"
					(lit1 analogousCodeTo: lit2) ifFalse: [
						^false ]].
		firstLitIndex _ 2 ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	firstLitIndex to: numLits-2 do: [ :i |
		lit1 _ self literalAt: i.
		lit2 _ method literalAt: i.
		lit1 = lit2 ifFalse: [
			"any other discrepancy is a failure"
			^ false ]].
	^true
</details>

#### CompiledMethod>>#writesFieldCode: varIndexCode with: byteCode using: scanner

<details>
	<summary>See more</summary>
	
	writesFieldCode: varIndexCode with: byteCode using: scanner 

	^byteCode >= 96
		and: [byteCode <= 103
			ifTrue: [byteCode - 96 = varIndexCode]
			ifFalse:
				[(byteCode = 129 or: [byteCode = 130])
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[byteCode = 132
						 and: [(scanner followingByte between: 160 and: 223)
						 and: [scanner thirdByte = varIndexCode]]]]]

</details>

#### CompiledMethod>>#createMethodNode

Creates the parse tree that represents self


<details>
	<summary>See more</summary>
	
	createMethodNode
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail."
	^ (aClass methodNodeFor: source) ifNil: [ self decompile ]
</details>

#### CompiledMethod>>#removeProperty: propName

Remove the property propName if it exists. Do _not_ raise an error if the property is missing.


<details>
	<summary>See more</summary>
	
	removeProperty: propName
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^nil].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value
</details>

#### CompiledMethod>>#usesClosureBytecodes

Answer whether the receiver was compiled using the closure compiler. This is used to help DebuggerMethodMap choose which mechanisms to use to inspect activations of the receiver. This method answers false negatives in that it only identifies methods that use the new BlockClosure bytecodes. But since methods that don't create blocks have essentially the same code when compiled with either compiler this makes little difference.


<details>
	<summary>See more</summary>
	
	usesClosureBytecodes
	"Answer whether the receiver was compiled using the closure compiler.
	 This is used to help DebuggerMethodMap choose which mechanisms to
	 use to inspect activations of the receiver.
	 This method answers false negatives in that it only identifies methods
	 that use the new BlockClosure bytecodes.
	 But since methods that don't create blocks have essentially the same
	 code when compiled with either compiler this makes little difference."

	^(InstructionStream on: self) scanFor: [:instr | instr >= 138 and: [instr <= 143]]
</details>

#### CompiledMethod>>#debuggerMap

<details>
	<summary>See more</summary>
	
	debuggerMap
	^DebuggerMethodMap forMethod: self
</details>

#### CompiledMethod>>#commentAutoCompleterDocumentationAppendigTo: text using: methodNode

<details>
	<summary>See more</summary>
	
	commentAutoCompleterDocumentationAppendigTo: text using: methodNode
	
	| comment |

	comment := methodNode comment.
	^ comment
		ifNil: [ text ] 
		ifNotNil: [ text append: (self commentAutoCompleterDocumentationOf: comment)].
		
	
</details>

#### CompiledMethod>>#numLiterals

Answer the number of literals used by the receiver.


<details>
	<summary>See more</summary>
	
	numLiterals
	"Answer the number of literals used by the receiver."

	^ Smalltalk isSpur
		ifTrue: [ self header bitAnd: 16r7FFF ]
		ifFalse: [ (self header bitShift: -9) bitAnd: 16rFF ]
</details>

#### CompiledMethod>>#literalsDo: aBlock

Evaluate aBlock for each of the literals referenced by the receiver.


<details>
	<summary>See more</summary>
	
	literalsDo: aBlock
	"Evaluate aBlock for each of the literals referenced by the receiver."
	1 to: self numLiterals do:
		[:index |
		aBlock value: (self objectAt: index + 1)]
</details>

#### CompiledMethod>>#printClassAndSelectorOn: aStream

<details>
	<summary>See more</summary>
	
	printClassAndSelectorOn: aStream

	aStream
		print: self methodClass;
		nextPutAll: '>>';
		nextPutAll: self selector storeString
</details>

#### CompiledMethod>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ CompiledMethodInspector
</details>

#### CompiledMethod>>#hasNewPropertyFormat

As of the closure compiler all methods have (or better have) the new format where the penultimate literal is either the method's selector or its properties and the ultimate literal is the class association.


<details>
	<summary>See more</summary>
	
	hasNewPropertyFormat
	"As of the closure compiler all methods have (or better have) the new
	 format where the penultimate literal is either the method's selector
	 or its properties and the ultimate literal is the class association."
	^true
</details>

#### CompiledMethod>>#explorerContents

(CompiledMethod compiledMethodAt: #explorerContents) explore


<details>
	<summary>See more</summary>
	
	explorerContents
	"(CompiledMethod compiledMethodAt: #explorerContents) explore"
	
	^Array streamContents:
		[:s| | tokens |
		tokens := Scanner new scanTokens: (self headerDescription readStream skipTo: $"; upTo: $").
		s nextPut: (ObjectExplorerWrapper
						with: ((0 to: tokens size by: 2) collect:
								[:i| i = 0 ifTrue: [self header] ifFalse: [{tokens at: i - 1. tokens at: i}]])
						name: 'header'
						model: self).
		(1 to: self numLiterals) do:
			[:key|
			s nextPut: (ObjectExplorerWrapper
							with: (self literalAt: key)
							name: ('literal', key printString contractTo: 32)
							model: self)].
		self isQuick
			ifTrue: [s nextPut: (ObjectExplorerWrapper
									with: self symbolic
									name: #symbolic
									model: self)]
			ifFalse:
				[self symbolicLinesDo:
					[:pc :line|
					pc <= 1
						ifTrue:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: 'pragma'
											model: self)]
						ifFalse:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: pc printString
											model: self)]]].
				"should be self numLiterals + 1 * Smalltalk wordSize + 1"
		self endPC + 1
			to: self basicSize
			do: [:key|
				s nextPut: (ObjectExplorerWrapper
								with: (self basicAt: key)
								name: key printString
								model: self)]]
</details>

#### CompiledMethod>>#longPrintOn: aStream

List of all the byte codes in a method with a short description of each


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream
	"List of all the byte codes in a method with a short description of each" 

	self longPrintOn: aStream indent: 0
</details>

#### CompiledMethod>>#propertyKeysAndValuesDo: aBlock

Enumerate the receiver with all the keys and values.


<details>
	<summary>See more</summary>
	
	propertyKeysAndValuesDo: aBlock

	"Enumerate the receiver with all the keys and values."

	self withPropertiesDo: [ :properties | properties propertyKeysAndValuesDo: aBlock]
</details>

#### CompiledMethod>>#pragmaAt: aKey

Answer the pragma with selector aKey, or nil if none.


<details>
	<summary>See more</summary>
	
	pragmaAt: aKey

	"Answer the pragma with selector aKey, or nil if none."

	^self withPropertiesDo: [ :properties | properties at: aKey ifAbsent: nil ] ifSelector: [ :selector | nil ].
	
</details>

#### CompiledMethod>>#isGetterOf: anInstVarName at: anInstVarIndex

<details>
	<summary>See more</summary>
	
	isGetterOf: anInstVarName at: anInstVarIndex
	
	^ self selector = anInstVarName 
		and: [ self isReturnField 
		and: [ self returnField + 1 = anInstVarIndex ]].

</details>

#### CompiledMethod>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' newMethod: '.
	aStream store: self size - self initialPC + 1.
	aStream nextPutAll: ' header: '.
	aStream store: self header.
	aStream nextPut: $).
	noneYet _ self storeElementsFrom: self initialPC to: self endPC on: aStream.
	1 to: self numLiterals do:
		[:index |
		noneYet
			ifTrue: [noneYet _ false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' literalAt: '.
		aStream store: index.
		aStream nextPutAll: ' put: '.
		aStream store: (self literalAt: index)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
</details>

#### CompiledMethod>>#abstractPCForConcretePC: concretePC

Answer the abstractPC matching concretePC.


<details>
	<summary>See more</summary>
	
	abstractPCForConcretePC: concretePC
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	self flag: 'belongs in DebuggerMethodMap?'.
	abstractPC := 1.
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue
</details>

#### CompiledMethod>>#isInstalled

<details>
	<summary>See more</summary>
	
	isInstalled
	self methodClass ifNotNil:
		[ :class |
		self selector ifNotNil:
			[ :selector |
			^self == (class compiledMethodAt: selector ifAbsent: nil)]].
	^false
</details>

#### CompiledMethod>>#needsFrameSize: newFrameSize

Set the largeFrameBit to accomodate the newFrameSize


<details>
	<summary>See more</summary>
	
	needsFrameSize: newFrameSize
	"Set the largeFrameBit to accomodate the newFrameSize"
	| largeFrameBit header |
	largeFrameBit _ 16r20000.
	(self numTemps + newFrameSize) > LargeFrame ifTrue:
		[^ self error: 'Cannot compile -- stack including temps is too deep'].
	header _ self objectAt: 1.
	(header bitAnd: largeFrameBit) ~= 0
		ifTrue: [header _ header - largeFrameBit].
	self objectAt: 1 put: header
 			+ ( ((self numTemps + newFrameSize) > SmallFrame or: [ self primitive = 84 "perform:withArguments:"])
					ifTrue: [largeFrameBit]
					ifFalse: [0])
</details>

#### CompiledMethod>>#objectForDataStream: refStrm

Return an object to store on an external data stream.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	
	self primitive = 117 ifTrue: [self literals first at: 4 put: 0].

</details>

#### CompiledMethod>>#selector: aSelector

Set a method's selector. This is either the penultimate literal, or, if the method has any properties or pragmas, the selector of the MethodProperties stored in the penultimate literal.


<details>
	<summary>See more</summary>
	
	selector: aSelector
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	| numberOfLiterals | 
	
	self 
		withPropertiesDo: [ :properties | properties selector: aSelector ] 
		ifSelector: [ :selector |
			(numberOfLiterals := self numLiterals) < 2 ifTrue: [self error: 'insufficient literals to hold selector'].
			self literalAt: numberOfLiterals - 1 put: aSelector]
</details>

#### CompiledMethod>>#sourceClass

Get my receiver class (method class) from the preamble of my source. Return nil if not found.


<details>
	<summary>See more</summary>
	
	sourceClass
	"Get my receiver class (method class) from the preamble of my source.  Return nil if not found."

	^ [(Compiler evaluate: (self sourceFileStream backChunk "blank"; backChunk "preamble")) theClass] on: Error do: nil
</details>

#### CompiledMethod>>#scanLongStore: extension

Answer whether the receiver contains a long store whose extension is the argument.


<details>
	<summary>See more</summary>
	
	scanLongStore: extension 
	"Answer whether the receiver contains a long store whose extension is 
	the argument."
	| scanner |
	scanner _ InstructionStream on: self.
	^scanner scanFor: 
		[:instr |  (instr = 129 or: [instr = 130]) and: [scanner followingByte = extension]]
</details>

#### CompiledMethod>>#refersToLiteral:aLiteral

<details>
	<summary>See more</summary>
	
	refersToLiteral:aLiteral

	^self hasLiteral: aLiteral.
</details>

#### CompiledMethod>>#decompile

Return the decompiled parse tree that represents self


<details>
	<summary>See more</summary>
	
	decompile
	"Return the decompiled parse tree that represents self"

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].
	^class decompilerClass new decompile: selector in: class method: self
</details>

#### CompiledMethod>>#destroySourcePointer

<details>
	<summary>See more</summary>
	
	destroySourcePointer
	self setSourcePointer: 0
</details>

#### CompiledMethod>>#penultimateLiteral

Answer the penultimate literal of the receiver, which holds either the receiver's selector or its properties (which will hold the selector).


<details>
	<summary>See more</summary>
	
	penultimateLiteral
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	^(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex]
		ifFalse: [nil]
</details>

#### CompiledMethod>>#frameSize

Answer the size of temporary frame needed to run the receiver.


<details>
	<summary>See more</summary>
	
	frameSize
	"Answer the size of temporary frame needed to run the receiver."
	"NOTE:  Versions 2.7 and later use two sizes of contexts."

	(self header noMask: 16r20000)
		ifTrue: [^ SmallFrame]
		ifFalse: [^ LargeFrame]

</details>

#### CompiledMethod>>#properties: aMethodProperties

Set the method-properties of the receiver to aMethodProperties.


<details>
	<summary>See more</summary>
	
	properties: aMethodProperties
	"Set the method-properties of the receiver to aMethodProperties."
	self literalAt: self numLiterals - 1
		put: (aMethodProperties isEmpty
				ifTrue: [aMethodProperties selector]
				ifFalse: [aMethodProperties
							setMethod: self;
							yourself])
</details>

#### CompiledMethod>>#hasReportableSlip

Answer whether the receiver contains anything that should be brought to the attention of the author when filing out. Customize the lists here to suit your preferences. If slips do not get reported in spite of your best efforts here, make certain that the Preference 'checkForSlips' is set to true.


<details>
	<summary>See more</summary>
	
	hasReportableSlip
	"Answer whether the receiver contains anything that should be brought 
	to the attention of the author when filing out. Customize the lists here 
	to suit your preferences. If slips do not get reported in spite of your 
	best efforts here, make certain that the Preference 'checkForSlips' is set 
	to true."

	#(#halt #halt: #hottest #toRemove #personal #urgent  #haltOnce #haltOnce: #haltIf: )
		do: [ :aLit | 
			(self hasLiteral: aLit)
				ifTrue: [^ true]].
	#(#Transcript #AA #BB #CC #DD #EE )
		do: [ :aSymbol | 	| assoc |
			(assoc := Smalltalk
				associationAt: aSymbol
				ifAbsent: nil)
					ifNotNil: [(self hasLiteral: assoc)
						ifTrue: [^ true]]].
	^ false
</details>

#### CompiledMethod>>#messages

Answer a Set of all the message selectors sent by this method.


<details>
	<summary>See more</summary>
	
	messages
	"Answer a Set of all the message selectors sent by this method."

	| scanner aSet |
	aSet _ Set new.
	scanner _ InstructionStream on: self.
	scanner	
		scanFor: 
			[:x | 
			scanner addSelectorTo: aSet.
			false	"keep scanning"].
	^aSet
</details>

#### CompiledMethod>>#checkOKToAdd: size at: filePosition in: fileIndex

Issue several warnings if the end of the changes file is approaching a fixed size limit, and finally halt with an error if the limit is reached.


<details>
	<summary>See more</summary>
	
	checkOKToAdd: size at: filePosition in: fileIndex
	"Issue several warnings if the end of the changes file is approaching
	a fixed size limit, and finally halt with an error if the limit is reached."

	^ SourceFiles checkOKToAdd: size at: filePosition in: fileIndex

</details>

#### CompiledMethod>>#selectorAutoCompleterDocumentationAppendingTo: sourceText using: methodNode appendingToParameter: aParameterAppendBlock

<details>
	<summary>See more</summary>
	
	selectorAutoCompleterDocumentationAppendingTo: sourceText using: methodNode appendingToParameter: aParameterAppendBlock

	| selector text |
	
	selector := methodNode selectorNode key.
	selector isUnary 
		ifTrue: [ text := sourceText append: (Text string: selector attributes: (SHTextStylerST80 attributesFor: #patternKeyword)) ]
		ifFalse: [
			text := sourceText.
			selector keywords 
				with: methodNode argumentNames 
				do: [ :keyword :argumentName |
					text := text append: (Text string: keyword attributes: (SHTextStylerST80 attributesFor: #patternKeyword)).
					text := text append: (Text string: ' ', argumentName, ' ' attributes: (SHTextStylerST80 attributesFor: #methodArg)).
					text := text append: (aParameterAppendBlock value: argumentName) ] 
				separatedBy: [ text := text append: String newLineString, String tab ]].

	^ text

</details>

#### CompiledMethod>>#hasBreakpoint

<details>
	<summary>See more</summary>
	
	hasBreakpoint
	^BreakpointManager methodHasBreakpoint: self
</details>

#### CompiledMethod>>#receiverTextAutoCompleterDocumentation

<details>
	<summary>See more</summary>
	
	receiverTextAutoCompleterDocumentation
	
	| receiverString |
	
	receiverString := String streamContents: [ :stream |
		stream 
			nextPutAll: self methodClass typeName;
			nextPutAll: '>>' ].
	
	^Text string: receiverString attributes: (SHTextStylerST80 attributesFor: #patternKeyword).
		
	
</details>

#### CompiledMethod>>#sendsSelector: aSelector

<details>
	<summary>See more</summary>
	
	sendsSelector: aSelector 
	| scanner |
	scanner := InstructionStream on: self.
	scanner scanFor: 
		[:x | 
		 scanner selectorToSendOrSelf == aSelector ifTrue:
			[^true].
		 false	"keep scanning"].
	^false
</details>

#### CompiledMethod>>#browse

<details>
	<summary>See more</summary>
	
	browse

	BrowserWindow fullOnClass: self methodClass selector: self selector
</details>

#### CompiledMethod>>#writesField: varIndex

Answer whether the receiver stores into the instance variable indexed by the argument.


<details>
	<summary>See more</summary>
	
	writesField: varIndex
	"Answer whether the receiver stores into the instance variable indexed
	 by the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."

	| varIndexCode scanner |
	
	self isQuick ifTrue: [^false].
	
	varIndexCode := varIndex - 1.
	^(scanner := InstructionStream on: self) scanFor: [:byteCode| 
		self writesFieldCode: varIndexCode with: byteCode using: scanner ]

</details>

#### CompiledMethod>>#returnField

Answer the index of the instance variable returned by a quick return method.


<details>
	<summary>See more</summary>
	
	returnField
	"Answer the index of the instance variable returned by a quick return 
	method."
	| prim |
	prim _ self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]
</details>

#### CompiledMethod>>#flag

Answer the user-level flag bit


<details>
	<summary>See more</summary>
	
	flag
	"Answer the user-level flag bit"

	^((self header bitShift: -29) bitAnd: 1) = 1
</details>

#### CompiledMethod>>#pragmas

<details>
	<summary>See more</summary>
	
	pragmas
	
	^self withPropertiesDo: [ :properties | properties pragmas ] ifSelector: [ :selector | #() ]
</details>

#### CompiledMethod>>#filePosition

<details>
	<summary>See more</summary>
	
	filePosition
	^SourceFiles filePositionFromSourcePointer: self sourcePointer
</details>

#### CompiledMethod>>#literals

Answer an Array of the literals referenced by the receiver.


<details>
	<summary>See more</summary>
	
	literals
	"Answer an Array of the literals referenced by the receiver."
	| literals numberLiterals |
	literals _ Array new: (numberLiterals _ self numLiterals).
	1 to: numberLiterals do:
		[:index |
		literals at: index put: (self objectAt: index + 1)].
	^literals
</details>

#### CompiledMethod>>#methodClass: aClass

set the class binding in the last literal to aClass


<details>
	<summary>See more</summary>
	
	methodClass: aClass
	"set the class binding in the last literal to aClass"
	self literalAt: self numLiterals put: aClass binding
</details>

#### CompiledMethod>>#numArgs

Answer the number of arguments the receiver takes.


<details>
	<summary>See more</summary>
	
	numArgs
	"Answer the number of arguments the receiver takes."

	^ (self header bitShift: -24) bitAnd: 16r0F
</details>

#### CompiledMethod>>#initialPC

Answer the program counter for the receiver's first bytecode.


<details>
	<summary>See more</summary>
	
	initialPC
	"Answer the program counter for the receiver's first bytecode."
	^ (self numLiterals + 1) * Smalltalk wordSize + 1
</details>

#### CompiledMethod>>#propertyValueAt: propName put: propValue

Set or add the property with key propName and value propValue. If the receiver does not yet have a method properties create one and replace the selector with it. Otherwise, either relace propValue in the method properties or replace method properties with one containing the new property.


<details>
	<summary>See more</summary>
	
	propertyValueAt: propName put: propValue
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	
	self 
		withPropertiesDo: [:properties | 
			(properties includesProperty: propName) ifTrue: [^properties at: propName put: propValue].
			self penultimateLiteral: (properties
								copyWith: (Association
												key: propName asSymbol
												value: propValue)).
			^propValue ] 
		ifSelector: [ :selector |
			self penultimateLiteral: ((AdditionalMethodState
									selector: selector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
			^propValue].
</details>

#### CompiledMethod>>#isValid

To be polimorphic with MethodReference, important for refactorings - Hernan


<details>
	<summary>See more</summary>
	
	isValid

	"To be polimorphic with MethodReference, important for refactorings - Hernan"
	^true
</details>

#### CompiledMethod>>#putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod overridesMethod: overridenMethod

<details>
	<summary>See more</summary>
	
	putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod overridesMethod: overridenMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod
				overridesMethod: overridenMethod.
			file newLine ]
</details>

#### CompiledMethod>>#isQuick

Answer whether the receiver is a quick return (of self or of an instance variable).


<details>
	<summary>See more</summary>
	
	isQuick
	"Answer whether the receiver is a quick return (of self or of an instance 
	variable)."
	^ self primitive between: 256 and: 519
</details>

#### CompiledMethod>>#hasLiteralThorough: literal

Answer true if any literal in this method is literal, even if embedded in array structure.


<details>
	<summary>See more</summary>
	
	hasLiteralThorough: literal
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralThorough: literal) ifTrue:[^true]].
	
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do: [ :index |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [(lit isVariableBinding and: [lit key == literal])
		 or: [lit isArray and: [lit hasLiteral: literal]]]) ifTrue:
			[^ true]].
	^ false 
</details>

#### CompiledMethod>>#scanner

<details>
	<summary>See more</summary>
	
	scanner

	^ InstructionStream on: self
</details>

#### CompiledMethod>>#scanLongLoad: extension

Answer whether the receiver contains a long load whose extension is the argument.


<details>
	<summary>See more</summary>
	
	scanLongLoad: extension 
	"Answer whether the receiver contains a long load whose extension is the 
	argument."

	| scanner |
	scanner _ InstructionStream on: self.
	^scanner scanFor: [:instr | instr = 128 and: [scanner followingByte = extension]]
</details>

#### CompiledMethod>>#getPreamble

<details>
	<summary>See more</summary>
	
	getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFileEntry readStreamDo: [ :stream |
			preamble _ (self getPreambleFrom: stream at: (0 max: self filePosition)) ifNil: [ '' ].
			]
	] on: FileDoesNotExistException do: [ :ex | preamble _ '' ].
	^ preamble
</details>

#### CompiledMethod>>#outboundPointersDo: aBlock

do aBlock for every object I point to, exactly how the garbage collector would. Adapted from PointerFinder >> #followObject:


<details>
	<summary>See more</summary>
	
	outboundPointersDo: aBlock

	| numLiterals |
	aBlock value: self class.
	numLiterals := self numLiterals.
	1 to: numLiterals do: [:i | aBlock value: (self literalAt: i)]
</details>

#### CompiledMethod>>#header

Answer the word containing the information about the form of the receiver and the form of the context needed to run the receiver.


<details>
	<summary>See more</summary>
	
	header
	"Answer the word containing the information about the form of the 
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1
</details>

#### CompiledMethod>>#literalAt: index

Answer the literal indexed by the argument.


<details>
	<summary>See more</summary>
	
	literalAt: index 
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1
</details>

#### CompiledMethod>>#storeLiteralsOn: aStream forClass: aBehavior

Store the literals referenced by the receiver on aStream, each terminated by a space.


<details>
	<summary>See more</summary>
	
	storeLiteralsOn: aStream forClass: aBehavior
	"Store the literals referenced by the receiver on aStream, each terminated by a space."

	| literal |
	2 to: self numLiterals + 1 do:
		[:index |
		 aBehavior storeLiteral: (self objectAt: index) on: aStream.
		 aStream space]
</details>

#### CompiledMethod>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #CompiledMethod or: [ super is: aSymbol ]
</details>

#### CompiledMethod>>#valueWithReceiver: aReceiver arguments: anArray

<details>
	<summary>See more</summary>
	
	valueWithReceiver: aReceiver arguments: anArray 

	^self class receiver: aReceiver withArguments: anArray executeMethod: self
</details>

#### CompiledMethod>>#copyWithTrailerBytes: bytes

Testing: (CompiledMethod compiledMethodAt: #copyWithTrailerBytes:) tempNamesPut: 'copy end '


<details>
	<summary>See more</summary>
	
	copyWithTrailerBytes: bytes
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start _ self initialPC.
	end _ self endPC.
	copy _ CompiledMethod newMethod: end - start + 1 + bytes size
				header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	1 to: bytes size do: [:i | copy at: end + i put: (bytes at: i)].
	^ copy
</details>

#### CompiledMethod>>#writesInstanceVariable: aName

<details>
	<summary>See more</summary>
	
	writesInstanceVariable: aName

	^self writesField: (self methodClass indexOfInstanceVariable: aName)
</details>

#### CompiledMethod>>#encoderClass

Answer the encoder class that encoded the bytecodes in this method. The sign flag bit is used by the VM to select a bytecode set. This formulation may seem odd but this has to be fast, so no property probe unless needed.


<details>
	<summary>See more</summary>
	
	encoderClass
	"Answer the encoder class that encoded the bytecodes in this method.
	 The sign flag bit is used by the VM to select a bytecode set.  This formulation
	 may seem odd but this has to be fast, so no property probe unless needed."

	^self header >= 0
		ifTrue: 
			[PrimaryBytecodeSetEncoderClass]
		ifFalse:
			[PrimaryBytecodeSetEncoderClass == SecondaryBytecodeSetEncoderClass
				ifTrue: "Support for testing prior to installing another set"
					[(self propertyValueAt: #encoderClass) ifNil: [SecondaryBytecodeSetEncoderClass]]
				ifFalse:
					[SecondaryBytecodeSetEncoderClass]]
</details>

#### CompiledMethod>>#primitive

Answer the primitive index associated with the receiver. Zero indicates that this is not a primitive method.


<details>
	<summary>See more</summary>
	
	primitive
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method."
	| initialPC primBits |
	Smalltalk isSpur
		ifTrue: [
			^(self header anyMask: 65536) "Is the hasPrimitive? flag set?"
				ifTrue: [(self at: (initialPC := self initialPC) + 1) + ((self at: initialPC + 2) bitShift: 8)]
				ifFalse: [0]]
		ifFalse: [
			"We currently allow 10 bits of primitive index, but they are in two places
			for  backward compatibility.  The time to unpack is negligible,
			since the reconstituted full index is stored in the method cache."
			primBits _ self header bitAnd: 16r100001FF.
			^ (primBits bitAnd: 16r1FF) + (primBits bitShift: -19)]
</details>

#### CompiledMethod>>#methodNode

Return the parse tree that represents self


<details>
	<summary>See more</summary>
	
	methodNode

	"Return the parse tree that represents self"
	
	"I do not save the method node in the #methodNode property if it does not
	exist to avoid keeping the method node in memory. 
	The methodNode is saved in the property #methodNode to avoid loosing the source
	code when debugging - Hernan"
	^self propertyValueAt: #methodNode ifAbsent: [ self createMethodNode ]
</details>

#### CompiledMethod>>#sourcePointer

Answer the integer which can be used to find the source file and position for this method. The returned value is either 0 (if no source is stored) or a number between 16r1000000 and 16r4FFFFFF. The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles.


<details>
	<summary>See more</summary>
	
	sourcePointer
	"Answer the integer which can be used to find the source file and position for this method.
	The returned value is either 0 (if no source is stored) or a number between 16r1000000 and 16r4FFFFFF.
	The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles."

	| pos |
	self last < 252 ifTrue: [^ 0  "no source"].
	pos _ self last - 251.
	self size - 1 to: self size - 3 by: -1 do: [:i | pos _ pos * 256 + (self at: i)].
	^pos
</details>

#### CompiledMethod>>#writesRef: literalAssociation

Answer whether the receiver stores into the argument.


<details>
	<summary>See more</summary>
	
	writesRef: literalAssociation 
	"Answer whether the receiver stores into the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor:
		[:b|
		(b = 129 or: [b = 130])
			ifTrue: [scanner followingByte - 192 = litIndex]
			ifFalse:
				[b = 132
				 and: [scanner followingByte >= 224
				 and: [scanner thirdByte = litIndex]]]]
</details>

#### CompiledMethod>>#readsRef: literalAssociation

Answer whether the receiver loads the argument.


<details>
	<summary>See more</summary>
	
	readsRef: literalAssociation 
	"Answer whether the receiver loads the argument."
	"eem 5/24/2008 Rewritten to no longer assume the compler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."
	| litIndex scanner |
	(litIndex := self indexOfLiteral: literalAssociation) = 0 ifTrue:
		[^false].
	litIndex := litIndex - 1.
	^(scanner := InstructionStream on: self) scanFor:
		[:b|
		b >= 64
		and:
			[b <= 95
				ifTrue: [b - 64 = litIndex]
				ifFalse:
					[b = 128
						ifTrue: [scanner followingByte - 192 = litIndex]
						ifFalse:
							[b = 132
							 and: [(scanner followingByte between: 128 and: 159)
							 and: [scanner thirdByte = litIndex]]]]]]
</details>

#### CompiledMethod>>#blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock

Support routine for startpcsToBlockExtents


<details>
	<summary>See more</summary>
	
	blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock
	"Support routine for startpcsToBlockExtents"
	| extentStart blockSizeOrLocator |
	self flag: 'belongs in DebuggerMethodMap'.
	extentStart := numbererBlock value.
	[scanner pc <= endPC] whileTrue:
		[blockSizeOrLocator := scanner interpretNextInstructionFor: BlockStartLocator new.
		 blockSizeOrLocator isInteger ifTrue:
			[self
				blockExtentsInto: aDictionary
				from: scanner pc
				to: scanner pc + blockSizeOrLocator - 1
				scanner: scanner
				numberer: numbererBlock]].
	aDictionary at: initialPC put: (extentStart to: numbererBlock value).
	^aDictionary
</details>

#### CompiledMethod>>#fileIndex

<details>
	<summary>See more</summary>
	
	fileIndex
	^SourceFiles fileIndexFromSourcePointer: self sourcePointer
</details>

#### CompiledMethod>>#objectAt: index put: value

Primitive. Store the value argument into a literal in the receiver. An index of 2 corresponds to the first literal. Fails if the index is less than 2 or greater than the number of literals. Answer the value as the result. Normally only the compiler sends this message, because only the compiler stores values in CompiledMethods. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	objectAt: index put: value 
	"Primitive. Store the value argument into a literal in the receiver. An 
	index of 2 corresponds to the first literal. Fails if the index is less than 2 
	or greater than the number of literals. Answer the value as the result. 
	Normally only the compiler sends this message, because only the 
	compiler stores values in CompiledMethods. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed
</details>

#### CompiledMethod>>#classAndSelector

<details>
	<summary>See more</summary>
	
	classAndSelector

	^String streamContents: [:stream | self printClassAndSelectorOn: stream ]
</details>

#### CompiledMethod>>#messagesSequence

Answer a Set of all the message selectors sent by this method.


<details>
	<summary>See more</summary>
	
	messagesSequence
	"Answer a Set of all the message selectors sent by this method."

	^Array streamContents:
		[:str| | scanner |
		scanner := InstructionStream on: self.
		scanner	scanFor: 
			[:x | | selectorOrSelf |
			(selectorOrSelf := scanner selectorToSendOrSelf) == scanner ifFalse:
				[str nextPut: selectorOrSelf].
			false	"keep scanning"]]
</details>

#### CompiledMethod>>#startpcsToBlockExtents

Answer a Dictionary of startpc to Interval of blockExtent, using the identical numbering scheme described in and orchestrated by BlockNode>>analyseArguments:temporaries:rootNode:. This is used in part to find the temp names for any block in a method, as needed by the debugger. The other half is to recompile the method, obtaining the temp names for each block extent. By indirecting through the blockExtent instead of using the startpc directly we decouple the debugger's access to temp names from the exact bytecode; insulating debugging from minor changes in the compiler (e.g. changes in literal pooling, adding prefix bytecodes, adding inst vars to CompiledMethod in literals towards the end of the literal frame, etc). If the recompilation doesn't produce exactly the same bytecode at exactly the same offset no matter; the blockExtents will be the same.


<details>
	<summary>See more</summary>
	
	startpcsToBlockExtents
	"Answer a Dictionary of startpc to Interval of blockExtent, using the
	 identical numbering scheme described in and orchestrated by
	 BlockNode>>analyseArguments:temporaries:rootNode:.  This is
	 used in part to find the temp names for any block in a method, as
	 needed by the debugger.  The other half is to recompile the method,
	 obtaining the temp names for each block extent.  By indirecting through
	 the blockExtent instead of using the startpc directly we decouple the
	 debugger's access to temp names from the exact bytecode; insulating
	 debugging from minor changes in the compiler (e.g. changes in literal
	 pooling, adding prefix bytecodes, adding inst vars to CompiledMethod
	 in literals towards the end of the literal frame, etc).  If the recompilation
	 doesn't produce exactly the same bytecode at exactly the same offset
	 no matter; the blockExtents will be the same."
	| index |
	self flag: 'belongs in DebuggerMethodMap'.
	index := 0.
	^self
		blockExtentsInto: Dictionary new
		from: self initialPC
		to: self endPC
		scanner: (InstructionStream on: self)
		numberer: [| value | value := index. index := index + 2. value]
</details>

#### CompiledMethod>>#mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString

Decode a schematicTempNamesString that encodes the layout of temp names in a method and any closures/blocks within it, matching keys in keys to vectors of temp names.


<details>
	<summary>See more</summary>
	
	mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek = $(
				ifTrue: [tempSequence addAll: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~= $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAll: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek = $]] whileTrue: [tempNames next].
		tempNames peek = $[ ifTrue:
			[tempNames next]].
	^map
</details>

#### CompiledMethod>>#putSource: sourceStr fromParseNode: methodNode class: class category: catName
	inFile: fileIndex priorMethod: priorMethod

<details>
	<summary>See more</summary>
	
	putSource: sourceStr fromParseNode: methodNode class: class category: catName
	inFile: fileIndex priorMethod: priorMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
		class printCategoryChunk: catName on: file priorMethod: priorMethod.
		file newLine ]
</details>

#### CompiledMethod>>#headerDescription

Answer a description containing the information about the form of the receiver and the form of the context needed to run the receiver.


<details>
	<summary>See more</summary>
	
	headerDescription
	"Answer a description containing the information about the form of the
	 receiver and the form of the context needed to run the receiver."

	^String streamContents: [ :stream |
		stream
			print: self header; newLine;
			nextPutAll: '"primitive: '; print: self primitive; newLine;
			nextPutAll: ' numArgs: '; print: self numArgs; newLine;
			nextPutAll: ' numTemps: '; print: self numTemps; newLine;
			nextPutAll: ' numLiterals: '; print: self numLiterals; newLine;
			nextPutAll: ' frameSize: '; print: self frameSize; newLine;
			nextPutAll: ' bytecodeSet: '; nextPutAll: self bytecodeSetName;
			nextPut: $"; newLine
	]
</details>

#### CompiledMethod>>#hasLiteralSuchThat: litBlock

Answer true if litBlock returns true for any literal in this method, even if embedded in array structure.


<details>
	<summary>See more</summary>
	
	hasLiteralSuchThat: litBlock
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	
	| lit |
	
	self withPropertiesDo: [ :properties | (properties hasLiteralSuchThat: litBlock) ifTrue: [ ^true ]]. 

	2 to: self numLiterals + 1 do: [ :index |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false
</details>

#### CompiledMethod>>#methodClassAssociation: aBinding

sets the association to the class that I am installed in


<details>
	<summary>See more</summary>
	
	methodClassAssociation: aBinding
	"sets the association to the class that I am installed in"
	^self literalAt: self numLiterals put: aBinding
</details>

#### CompiledMethod>>#readDataFrom: aDataStream size: varsOnDisk

Fill in my fields. My header and number of literals are already installed. Must read both objects for the literals and bytes for the bytecodes.


<details>
	<summary>See more</summary>
	
	readDataFrom: aDataStream size: varsOnDisk
	"Fill in my fields.  My header and number of literals are already installed.  Must read both objects for the literals and bytes for the bytecodes."

	self error: 'Must use readMethod'.
</details>

#### CompiledMethod>>#readsField: varIndex

Answer whether the receiver loads the instance variable indexed by the argument.


<details>
	<summary>See more</summary>
	
	readsField: varIndex 
	"Answer whether the receiver loads the instance variable indexed by the 
	 argument."
	"eem 5/24/2008 Rewritten to no longer assume the compiler uses the
	 most compact encoding available (for EncoderForLongFormV3 support)."
	| varIndexCode scanner |
	varIndexCode := varIndex - 1.
	self isReturnField ifTrue: [^self returnField = varIndexCode].
	^(scanner := InstructionStream on: self) scanFor:
		[:b|
		b < 16
			ifTrue: [b = varIndexCode]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 64 and: 95)
						 and: [scanner thirdByte = varIndexCode]]]]]
</details>

#### CompiledMethod>>#endPC

Answer the index of the last bytecode.


<details>
	<summary>See more</summary>
	
	endPC
	"Answer the index of the last bytecode."
	| size flagByte |
	"Can't create a zero-sized CompiledMethod so no need to use last for the errorEmptyCollection check.
	 We can reuse size."
	size := self size.
	flagByte := self at: size.
	flagByte = 0 ifTrue: [
		"If last byte = 0, may be either 0, 0, 0, 0 or just 0"
		size-1 to: size-3 by: -1 do: [ :i |
			i < self initialPC ifTrue: [ ^ i ].
			(self at: i) = 0 ifFalse: [ ^ i ]].
		^size - 4].
	flagByte < 252 ifTrue: [
		"Magic sources (temp names encoded in last few bytes)"
		^flagByte <= 127
			ifTrue: [size - flagByte - 1]
			ifFalse: [size - (flagByte - 128 * 128) - (self at: size - 1) - 2]].
	"Normal 4-byte source pointer"
	^size - 4
</details>

#### CompiledMethod>>#selector

Answer a method's selector. This is either the penultimate literal, or, if the method has any properties or pragmas, the selector of the MethodProperties stored in the penultimate literal.


<details>
	<summary>See more</summary>
	
	selector
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	
	^self 
		withPropertiesDo: [ :properties | properties selector ] 
		ifSelector: [ :selector | selector ]

</details>

#### CompiledMethod>>#withPropertiesDo: withBlock ifSelector: notBlock

<details>
	<summary>See more</summary>
	
	withPropertiesDo: withBlock ifSelector: notBlock

	| penultimalLiteral |
	
	penultimalLiteral := self penultimateLiteral.
	
	^penultimalLiteral isMethodProperties 
		ifTrue: [ withBlock value: penultimalLiteral ]
		ifFalse: [ notBlock value: penultimalLiteral ]
</details>

#### CompiledMethod>>#messageSendsRangesOf: aSentSelector

<details>
	<summary>See more</summary>
	
	messageSendsRangesOf: aSentSelector

	| methodNode ranges |

	methodNode := self methodNode.
	ranges := OrderedCollection new.

	methodNode nodesDo: [ :aParseNode |
		(aParseNode isMessageNamed: aSentSelector) ifTrue: [
			(methodNode rangeForNode: aParseNode ifAbsent: nil) ifNotNil: [ :range |
				ranges add: range ]]].

	^ranges 
</details>

#### CompiledMethod>>#hasVariableBindingTo: aClass

<details>
	<summary>See more</summary>
	
	hasVariableBindingTo: aClass 
	
	self literalsDo: [ :aLiteral |
		(aLiteral isVariableBinding and: [ aLiteral value = aClass ]) ifTrue: [ ^true ]].
	
	^false
</details>

#### CompiledMethod>>#pcPreviousTo: pc

<details>
	<summary>See more</summary>
	
	pcPreviousTo: pc
	| scanner client prevPc |
	self flag: 'belongs in DebuggerMethodMap?'.
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	^prevPc
</details>

#### CompiledMethod>>#hash

CompiledMethod>>#= compares code, i.e. same literals and same bytecode. So we look at the header, methodClass and some bytes between initialPC and endPC, but /not/ the selector because the equal method does not compare selectors. Note that we must override ByteArray>hash which looks at all bytes of the receiver. Using bytes from the pointer part of a COmpiledmethod can lead to a variable hash if and when when the GC moves literals in the receiver. jmv: As I made #= not to compare for methodClass, remove it from here.


<details>
	<summary>See more</summary>
	
	hash
	"CompiledMethod>>#= compares code, i.e. same literals and same bytecode.
	 So we look at the header, methodClass and some bytes between initialPC and endPC,
	 but /not/ the selector because the equal method does not compare selectors.
	 Note that we must override ByteArray>hash which looks at all bytes of the receiver.
	 Using bytes from the pointer part of a COmpiledmethod can lead to a variable hash
	 if and when when the GC moves literals in the receiver.
	jmv: As I made #= not to compare for methodClass, remove it from here."
	| initialPC endPC hash |
	initialPC := self initialPC.
	endPC := self endPC.
	hash := self species hash + self header + initialPC + endPC "+ self methodClass hash" bitAnd: 16rFFFFFFF.
	"sample approximately 20 bytes"
	initialPC to: endPC by: (endPC - initialPC // 20 max: 1) do: [ :i |
		hash := hash + (self at: i)].
	^hash

	"(CompiledMethod>>#hash) hash"
</details>

#### CompiledMethod>>#searchForClass

search me in all classes, if found, return my class. Slow!


<details>
	<summary>See more</summary>
	
	searchForClass
	"search me in all classes, if found, return my class. Slow!"
	Smalltalk allBehaviorsDo: [:class | 
		(class methodDict keyAtIdentityValue: self ifAbsent: nil) ifNotNil: [^class]].
	^nil
</details>

#### CompiledMethod>>#scanForEqSmallConstant

Dan Ingalls' search for arithmetic use of == Answer whether the receiver contains the pattern <expression> == <constant>, where constant is -1, 0, 1, or 2... Smalltalk browseAllSelect: [:m | m scanForEqSmallConstant]


<details>
	<summary>See more</summary>
	
	scanForEqSmallConstant

    "Dan Ingalls' search for arithmetic use of ==
Answer whether the receiver contains the pattern <expression> == <constant>,
where constant is -1, 0, 1, or 2...
	Smalltalk browseAllSelect: [:m | m scanForEqSmallConstant]
"



    | scanner |

    scanner _ InstructionStream on: self.

    ^ scanner scanFor: [:instr | (instr between: 116 and: 119) and: [scanner followingByte = 198]]



"

Smalltalk browseAllSelect: [:m | m scanForEqSmallConstant]

"
</details>

#### CompiledMethod>>#isReturnField

Answer whether the receiver is a quick return of an instance variable.


<details>
	<summary>See more</summary>
	
	isReturnField
	"Answer whether the receiver is a quick return of an instance variable."
	^ self primitive between: 264 and: 519
</details>

#### CompiledMethod>>#parserClass

<details>
	<summary>See more</summary>
	
	parserClass
	^self methodClass 
		ifNil: [Compiler parserClass] 
		ifNotNil: [:class | class parserClass].
</details>

#### CompiledMethod>>#abstractSymbolic

Answer a String that contains a list of all the byte codes in a method with a short description of each, using relative addresses and not including code bytes.


<details>
	<summary>See more</summary>
	
	abstractSymbolic
	"Answer a String that contains a list of all the byte codes in a method with a
	 short description of each, using relative addresses and not including code bytes."

	| aStream |
	aStream := WriteStream on: (String new: 1000).
	self longPrintRelativeOn: aStream indent: 0.
	^aStream contents
</details>

#### CompiledMethod>>#propertyValueAt: propName

<details>
	<summary>See more</summary>
	
	propertyValueAt: propName

	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: nil] 
</details>

#### CompiledMethod>>#messagesDo: aBlock

<details>
	<summary>See more</summary>
	
	messagesDo: aBlock 

	^ self messages do:aBlock.
</details>

#### CompiledMethod>>#literalAt: index put: value

Replace the literal indexed by the first argument with the second argument. Answer the second argument.


<details>
	<summary>See more</summary>
	
	literalAt: index put: value 
	"Replace the literal indexed by the first argument with the second 
	argument. Answer the second argument."

	^self objectAt: index + 1 put: value
</details>

#### CompiledMethod>>#equivalentTo: aCompiledMethod

does not work yet with non-RB parseTrees


<details>
	<summary>See more</summary>
	
	equivalentTo: aCompiledMethod 
	"does not work yet with non-RB parseTrees"
	^ self = aCompiledMethod
		or: [self class == aCompiledMethod class
				and: [self numArgs == aCompiledMethod numArgs
						and: [self decompile = aCompiledMethod decompile]]].
</details>

#### CompiledMethod>>#symbolic

Answer a String that contains a list of all the byte codes in a method with a short description of each.


<details>
	<summary>See more</summary>
	
	symbolic
	"Answer a String that contains a list of all the byte codes in a method 
	with a short description of each."

	| aStream |
	aStream _ WriteStream on: (String new: 1000).
	self longPrintOn: aStream.
	^aStream contents
</details>

#### CompiledMethod>>#longPrintRelativeOn: aStream indent: tabs

List of all the byte codes in a method with a short description of each


<details>
	<summary>See more</summary>
	
	longPrintRelativeOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[^self longPrintOn: aStream indent: tabs].
	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(RelativeInstructionPrinter on: self)
		indent: tabs;
		printCode: false;
		printInstructionsOn: aStream.

</details>

#### CompiledMethod>>#methodClassAssociation

answer the association to the class that I am installed in, or nil if none.


<details>
	<summary>See more</summary>
	
	methodClassAssociation
	"answer the association to the class that I am installed in, or nil if none."
	^self literalAt: self numLiterals
</details>

#### CompiledMethod>>#asString

Convert to a String with Characters for each byte. Fast code uses primitive that avoids character conversion


<details>
	<summary>See more</summary>
	
	asString

	^self printString
</details>

#### CompiledMethod>>#accessorDescriptionOf: anInstVarName at: anInstVarIndex

<details>
	<summary>See more</summary>
	
	accessorDescriptionOf: anInstVarName at: anInstVarIndex
	
	| isReader isWriter |
	
	(self isGetterOf: anInstVarName at: anInstVarIndex) ifTrue: [ ^ 'getter' ].	
	(self isSetterOf: anInstVarName at: anInstVarIndex) ifTrue: [ ^ 'setter' ].
	
	isReader := self readsField: anInstVarIndex.
	isWriter := self writesField: anInstVarIndex.
	
	(isReader and: [ isWriter ]) ifTrue: [ ^ 'write/read' ].
	isReader ifTrue: [ ^ 'read' ].
	isWriter ifTrue: [ ^ 'write' ].
	
	^''
</details>

#### CompiledMethod>>#methodReference

<details>
	<summary>See more</summary>
	
	methodReference
	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^MethodReference class: class selector: selector.
	
</details>

#### CompiledMethod>>#scanVeryLongLoad: extension offset: offset

Answer whether the receiver contains a long load whose extension is the argument.


<details>
	<summary>See more</summary>
	
	scanVeryLongLoad: extension offset: offset
	"Answer whether the receiver contains a long load whose extension is the 
	argument."
	| scanner |
	scanner _ InstructionStream on: self.
	^ scanner scanFor: [:instr | (instr = 132 and: [scanner followingByte = extension])
											and: [scanner thirdByte = offset]]
</details>

#### CompiledMethod>>#flushCache

Tell the interpreter to remove all references to this method from its method lookup cache, if it has one. This primitive must be called whenever a method is defined or removed. NOTE: Only one of two selective flush methods needs to be used. Squeak 2.2 and earlier uses 119 (See Symbol flushCache). Squeak 2.3 and later uses 116 (See CompiledMethod flushCache).


<details>
	<summary>See more</summary>
	
	flushCache
	"Tell the interpreter to remove all references to this method from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of two selective flush methods needs to be used.
	Squeak 2.2 and earlier uses 119 (See Symbol flushCache).
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 116>

</details>

#### CompiledMethod>>#isTestMethod

<details>
	<summary>See more</summary>
	
	isTestMethod

    ^ (self methodClass is: #TestCaseClass) 
		and: [ ((self selector beginsWith: 'test') or: [ (self selector beginsWith: 'should')]) 
		and: [ self numArgs isZero ] ]
</details>

#### CompiledMethod>>#propertyValueAt: propName ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	propertyValueAt: propName ifAbsent: aBlock
	
	^self withPropertiesDo: [ :properties | properties propertyValueAt: propName ifAbsent: aBlock ] ifSelector: [ :selector | aBlock value ]
	
</details>

#### CompiledMethod>>#sourceCode

This method is implemented because getSource is not so intuitive - Hernan


<details>
	<summary>See more</summary>
	
	sourceCode

	"This method is implemented because getSource is not so intuitive - Hernan"
	^self getSource
</details>

#### CompiledMethod>>#decompileString

<details>
	<summary>See more</summary>
	
	decompileString
	^self decompile decompileString
</details>

#### CompiledMethod>>#referencesParameterAt: parameterIndex

<details>
	<summary>See more</summary>
	
	referencesParameterAt: parameterIndex

	| methodNode implementorParameterNodeToRemove parameterRanges |

	methodNode := self methodNode.
	implementorParameterNodeToRemove := methodNode arguments at: parameterIndex.
	parameterRanges := methodNode positionsForTemporaryVariable: implementorParameterNodeToRemove name ifAbsent: [#()].

	^parameterRanges size ~= 1
</details>

#### CompiledMethod>>#putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock

Store the source code for the receiver on an external file. If no sources are available, i.e., SourceFile is nil, do nothing. If the fileIndex is 1, print on *.sources; if it is 2, print on *.changes, in each case, storing a 4-byte source code pointer at the method end.


<details>
	<summary>See more</summary>
	
	putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	Smalltalk assureStartupStampLogged.
	(SourceFiles notNil and: [(file _ SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString _ RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]]
</details>

#### CompiledMethod>>#symbolicLinesDo: aBlock

Evaluate aBlock with each of the lines in the symbolic output.


<details>
	<summary>See more</summary>
	
	symbolicLinesDo: aBlock
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc firstLine |
	aStream := ReadWriteStream on: (String new: 64).
	self isQuick ifTrue:
		[self longPrintOn: aStream.
		 aBlock value: 0 value: aStream contents.
		 ^self].

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(InstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line index |
			line := stream contents allButLast.
			firstLine _ line lines first.
			firstLine size < line size ifTrue: [
				line _ firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]
</details>

#### CompiledMethod>>#hasArgumentOrTemporaryNamed: aVariable

<details>
	<summary>See more</summary>
	
	hasArgumentOrTemporaryNamed: aVariable

	^self methodNode hasArgumentOrTemporaryNamed: aVariable

</details>

## ContextPart

To the instruction parsing ability of InstructionStream I add the actual semantics for execution. The execution state is stored in the indexable fields of my subclasses. This includes temporary variables and a stack of values used in evaluating expressions. The actual semantics of execution can be found in my category "system simulation" and "instruction decode". These methods exactly parallel the operation of the Smalltalk machine itself. The simulator is a group of my methods that do what the Smalltalk interpreter does: execute Smalltalk bytecodes. By adding code to the simulator, you may take statistics on the running of Smalltalk methods. For example, Transcript show: (ContextPart runSimulated: [3 factorial]) printString.

### Methods
#### ContextPart>>#copyTo: aContext

Copy self and my sender chain down to, but not including, aContext. End of copied chain will have nil sender.


<details>
	<summary>See more</summary>
	
	copyTo: aContext
	"Copy self and my sender chain down to, but not including, aContext.  End of copied chain will have nil sender."

	| copy |
	self == aContext ifTrue: [ ^nil ].
	copy := self copy.
	sender ifNotNil: [ copy privSender: (sender copyTo: aContext) ].
	^copy
</details>

#### ContextPart>>#doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments

Simulate a primitive method whose index is primitiveIndex. The simulated receiver and arguments are given as arguments to this message. If successful, push result and return resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes execution needs to be intercepted and simulated to avoid execution running away.


<details>
	<summary>See more</summary>
	
	doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Judicious use of primitive 19 (a null primitive that doesn't do anything) prevents
	 the debugger from entering various run-away activities such as spawning a new
	 process, etc.  Injudicious use results in the debugger not being able to debug
	 interesting code, such as the debugger itself.  hence use primitive 19 with care :-)"
	"SystemNavigation new browseAllSelect: [:m| m primitive = 19]"
	primitiveIndex = 19 ifTrue:
		[Debugger
			openContext: self
			label:'Code simulation error'
			contents: nil].

	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: receiver) includesBehavior: BlockClosure]) ifTrue:
		[((primitiveIndex between: 201 and: 205)			 "BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "BlockClosure>>valueNoContextSwitch[:]"
			[^receiver simulateValueWithArguments: arguments caller: self].
		 primitiveIndex = 206 ifTrue:						"BlockClosure>>valueWithArguments:"
			[^receiver simulateValueWithArguments: arguments first caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[^self send: arguments first to: receiver with: arguments allButFirst super: false].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[^self send: arguments first to: receiver with: (arguments at: 2) lookupIn: (self objectClass: receiver)].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[^self send: arguments first to: receiver with: (arguments at: 2) lookupIn: (arguments at: 3)].

	"Mutex>>primitiveEnterCriticalSection
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSection"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| effective |
		 effective := Processor activeProcess effectiveProcess.
		 "active == effective"
		 value := primitiveIndex = 186
					ifTrue: [receiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [receiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].

	primitiveIndex = 188 ifTrue: "eem 5/27/2008 11:10 Object>>withArgs:executeMethod:"
		[((self objectClass: (arguments at: 1)) == Array
		  and: [(self objectClass: (arguments at: 2)) includesBehavior: CompiledMethod]) ifFalse:
			[^ContextPart primitiveFailTokenFor: #'bad argument'].
		 (arguments at: 2) numArgs = (arguments at: 1) size ifFalse:
			[^ContextPart primitiveFailTokenFor: #'bad number of arguments'].
		 (arguments at: 2) primitive > 0 ifTrue:
			[(arguments at: 2) isQuick ifTrue:
				[^self push: (receiver withArgs: (arguments at: 1) executeMethod: (arguments at: 2))].
			 ^self doPrimitive: (arguments at: 2) primitive method: (arguments at: 2) receiver: receiver args: (arguments at: 1)].
		 ^MethodContext
			sender: self
			receiver: receiver
			method: (arguments at: 2)
			arguments: (arguments at: 1)].

	"Closure primitives"
	(primitiveIndex = 200 and: [self == receiver]) ifTrue:
		"ContextPart>>closureCopy:copiedValues:; simulated to get startpc right"
		[^self push: (BlockClosure
						outerContext: receiver
						startpc: pc + 2
						numArgs: arguments first
						copiedValues: arguments last)].

	primitiveIndex = 118 ifTrue: "[receiver:]tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 3
		  and: [(self objectClass: arguments second) == SmallInteger
		  and: [(self objectClass: arguments last) == Array]]) ifTrue:
			[^self doPrimitive: arguments second method: meth receiver: arguments first args: arguments last].
		 (arguments size = 2
		 and: [(self objectClass: arguments first) == SmallInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^ContextPart primitiveFailTokenFor: nil].
		 ^self doPrimitive: arguments first method: meth receiver: receiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments]
						ifFalse:
							["should use self receiver: receiver tryPrimitive: primitiveIndex withArgs: arguments but this is only in later VMs"
							receiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]
</details>

#### ContextPart>>#nextHandlerContext

<details>
	<summary>See more</summary>
	
	nextHandlerContext

	^ self sender ifNotNil: [ :sndr | sndr findNextHandlerContext ]
</details>

#### ContextPart>>#stack

Answer an Array of the contexts on the receiver's sender chain.


<details>
	<summary>See more</summary>
	
	stack 
	"Answer an Array of the contexts on the receiver's sender chain."

	^self stackOfSize: 9999
</details>

#### ContextPart>>#exceptionClass

<details>
	<summary>See more</summary>
	
	exceptionClass

	^self tempAt: 1
</details>

#### ContextPart>>#arguments

<details>
	<summary>See more</summary>
	
	arguments

	| arguments |

	arguments _ Array new: self selector numArgs.
	1 to: arguments size do: [ :index | arguments at: index put: (self tempAt: index)].

	^arguments.
	
	
</details>

#### ContextPart>>#methodClass

Answer the class in which the receiver's method was found.


<details>
	<summary>See more</summary>
	
	methodClass 
	"Answer the class in which the receiver's method was found."
	
	^self method methodClass ifNil: [self objectClass: self receiver].
</details>

#### ContextPart>>#basicAt: index put: value

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	basicAt: index put: value
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#step

Simulate the execution of the receiver's next bytecode. Answer the context that would be the active context after this bytecode.


<details>
	<summary>See more</summary>
	
	step
	"Simulate the execution of the receiver's next bytecode. Answer the 
	context that would be the active context after this bytecode."

	^self interpretNextInstructionFor: self
</details>

#### ContextPart>>#object: anObject eqeq: anOtherObject

Answer whether the first and second arguments are the same object (have the same object pointer) without sending a message to the first argument. This mimics the action of the VM when it compares two object pointers. Used to simulate the execution machinery by, for example, the debugger. Primitive. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	object: anObject eqeq: anOtherObject 
	"Answer whether the first and second arguments are the same object (have the
	 same object pointer) without sending a message to the first argument.  This
	 mimics the action of the VM when it compares two object pointers.  Used to
	 simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
</details>

#### ContextPart>>#receiver

Answer the receiver of the message that created this context.


<details>
	<summary>See more</summary>
	
	receiver
	"Answer the receiver of the message that created this context."

	self subclassResponsibility
</details>

#### ContextPart>>#top

Answer the top of the receiver's stack.


<details>
	<summary>See more</summary>
	
	top
	"Answer the top of the receiver's stack."

	^self at: stackp
</details>

#### ContextPart>>#restart

Unwind thisContext to self and resume from beginning. Execute unwind blocks when unwinding. ASSUMES self is a sender of thisContext


<details>
	<summary>See more</summary>
	
	restart
	"Unwind thisContext to self and resume from beginning.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: nil to: self].
	self privRefresh.
	ctxt := thisContext.
	[	ctxt := ctxt findNextUnwindContextUpTo: self.
		ctxt isNil
	] whileFalse: [
		(ctxt tempAt: 2) ifNil:[
			ctxt tempAt: 2 put: true.
			unwindBlock := ctxt tempAt: 1.
			thisContext terminateTo: ctxt.
			unwindBlock value].
	].
	thisContext terminateTo: self.
	self jump.

</details>

#### ContextPart>>#stackPtr

For use only by the SystemTracer


<details>
	<summary>See more</summary>
	
	stackPtr  "For use only by the SystemTracer"
	^ stackp
</details>

#### ContextPart>>#push: val

Push val on the receiver's stack.


<details>
	<summary>See more</summary>
	
	push: val 
	"Push val on the receiver's stack."

	self stackp: stackp + 1.
	self at: stackp put: val
</details>

#### ContextPart>>#findSecondToOldestSimilarSender

Search the stack for the second-to-oldest occurance of self's method. Very useful for an infinite recursion. Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.


<details>
	<summary>See more</summary>
	
	findSecondToOldestSimilarSender
	"Search the stack for the second-to-oldest occurance of self's method.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning."

	| sec ctxt bot |
	sec := self.
	ctxt := self.
	[	bot := ctxt findSimilarSender.
		bot isNil
	] whileFalse: [
		sec := ctxt.
		ctxt := bot.
	].
	^ sec

</details>

#### ContextPart>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	| selector class mclass |
	self method
		ifNil: [^ super printOn: aStream].
	class := self receiver class.
	mclass := self methodClass.
	selector := self selector ifNil: [ self method defaultSelector].
	aStream nextPutAll: class name.
	mclass == class 
		ifFalse: 
			[aStream nextPut: $(.
			aStream nextPutAll: mclass name.
			aStream nextPut: $)].
	aStream nextPutAll: '>>'.
	aStream nextPutAll: selector.
	selector == #doesNotUnderstand: ifTrue: [
		aStream space.
		(self tempAt: 1) selector printOn: aStream.
	].

</details>

#### ContextPart>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	size
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed
</details>

#### ContextPart>>#namedTempAt: index put: aValue

Set the value of the temp at index in the receiver's sequence of tempNames. (Note that if the value is a copied value it is also set out along the lexical chain, but alas not in along the lexical chain.).


<details>
	<summary>See more</summary>
	
	namedTempAt: index put: aValue
	"Set the value of the temp at index in the receiver's sequence of tempNames.
	 (Note that if the value is a copied value it is also set out along the lexical chain,
	  but alas not in along the lexical chain.)."
	^self debuggerMap namedTempAt: index put: aValue in: self
</details>

#### ContextPart>>#canHandleSignal: exception

Sent to handler (on:do:) contexts only. If my exception class (first arg) handles exception then return true, otherwise forward this message to the next handler context. If none left, return false (see nil>>canHandleSignal:)


<details>
	<summary>See more</summary>
	
	canHandleSignal: exception
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then return true, otherwise forward this message to the next handler context.  If none left, return false (see nil>>canHandleSignal:)"

	^ (self exceptionClass handles: exception)
		or: [ self nextHandlerContext canHandleSignal: exception ].
</details>

#### ContextPart>>#storeIntoTemporaryVariable: offset

Simulate the action of bytecode that stores the top of the stack into one of my temporary variables.


<details>
	<summary>See more</summary>
	
	storeIntoTemporaryVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into one 
	of my temporary variables."

	self contextForLocalVariables at: offset + 1 put: self top
</details>

#### ContextPart>>#storeDataOn: aDataStream

Contexts are not allowed go to out in DataStreams. They must be included inside an ImageSegment.


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"Contexts are not allowed go to out in DataStreams.  They must be included inside an ImageSegment."

	self error: 'Contexts are not allowed go to out in DataStreams'.
	^ nil
</details>

#### ContextPart>>#cannotReturn: result to: homeContext

The receiver tried to return result to homeContext that no longer exists.


<details>
	<summary>See more</summary>
	
	cannotReturn: result to: homeContext
	"The receiver tried to return result to homeContext that no longer exists."

	^ BlockCannotReturn new
		result: result;
		deadHome: homeContext;
		signal
</details>

#### ContextPart>>#send: selector super: superFlag numArgs: numArgs

Simulate the action of bytecodes that send a message with selector, selector. The argument, superFlag, tells whether the receiver of the message was specified with 'super' in the source method. The arguments of the message are found in the top numArgs locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: superFlag numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, 
	selector. The argument, superFlag, tells whether the receiver of the 
	message was specified with 'super' in the source method. The arguments 
	of the message are found in the top numArgs locations on the stack and 
	the receiver just below them."

	| receiver arguments |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	receiver := self pop.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: receiver with: arguments super: superFlag].
	^self send: selector to: receiver with: arguments super: superFlag
</details>

#### ContextPart>>#isPrimFailToken: anObject

<details>
	<summary>See more</summary>
	
	isPrimFailToken: anObject
	^(self objectClass: anObject) == Array
	  and: [anObject size = 2
	  and: [anObject first == self class primitiveFailToken]]
</details>

#### ContextPart>>#bottomContext

Return the last context (the first context invoked) in my sender chain


<details>
	<summary>See more</summary>
	
	bottomContext
	"Return the last context (the first context invoked) in my sender chain"

	^ self findContextSuchThat: [:c | c sender isNil]
</details>

#### ContextPart>>#home

Answer the context in which the receiver was defined.


<details>
	<summary>See more</summary>
	
	home
	"Answer the context in which the receiver was defined."

	self subclassResponsibility
</details>

#### ContextPart>>#exceptionHandlerBlock

handler context only. access temporaries from BlockClosure>>#on:do:


<details>
	<summary>See more</summary>
	
	exceptionHandlerBlock
	"handler context only. access temporaries from BlockClosure>>#on:do:"

	^self tempAt: 2
</details>

#### ContextPart>>#send: selector to: rcvr with: arguments super: superFlag

Simulate the action of sending a message with selector arguments to rcvr. The argument, superFlag, tells whether the receiver of the message was specified with 'super' in the source method.


<details>
	<summary>See more</summary>
	
	send: selector to: rcvr with: arguments super: superFlag 
	"Simulate the action of sending a message with selector arguments
	 to rcvr. The argument, superFlag, tells whether the receiver of the
	 message was specified with 'super' in the source method."

	^self send: selector
		to: rcvr
		with: arguments
		lookupIn: (superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: rcvr])
</details>

#### ContextPart>>#runUntilErrorOrReturnFrom: aSender

ASSUMES aSender is a sender of self. Execute self's stack until aSender returns or an unhandled exception is raised. Return a pair containing the new top context and a possibly nil exception. The exception is not nil if it was raised before aSender returned and it was not handled. The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it.


<details>
	<summary>See more</summary>
	
	runUntilErrorOrReturnFrom: aSender 
	"ASSUMES aSender is a sender of self.  Execute self's stack until aSender returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSender returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSender that jumps back to thisContext when evaluated.  We also insert an exception handler under aSender that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error ctxt here topContext |
	here _ thisContext.

	"Insert ensure and exception handler contexts under aSender"
	error _ nil.
	ctxt _ aSender insertSender: (ContextPart
		contextOn: UnhandledError do: [:ex |
			error ifNil: [
				error _ ex exception.
				topContext _ thisContext.
				ex resumeUnchecked: here jump]
			ifNotNil: [ex pass]
		]).
	ctxt _ ctxt insertSender: (ContextPart
		contextEnsure: [error ifNil: [
				topContext _ thisContext.
				here jump]
		]).
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead] whileFalse: [topContext _ topContext stepToCallee].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].

</details>

#### ContextPart>>#return: value through: firstUnwindContext

Unwind thisContext to self and return value to self's sender. Execute any unwind blocks while unwinding. ASSUMES self is a sender of thisContext.


<details>
	<summary>See more</summary>
	
	return: value through: firstUnwindContext
	"Unwind thisContext to self and return value to self's sender.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	sender ifNil: [self cannotReturn: value to: sender].
	sender resume: value through: firstUnwindContext
</details>

#### ContextPart>>#findNextHandlerOrSignalingContext

Return the next handler/signaling marked context, answering nil if there is none. Search starts with self and proceeds up to nil.


<details>
	<summary>See more</summary>
	
	findNextHandlerOrSignalingContext
	"Return the next handler/signaling marked context, answering nil if there is none. 
	Search starts with self and proceeds up to nil."

	<primitive: 197>
	| context |
	context := self.
	[ 
	context isHandlerOrSignalingContext
		ifTrue: [ ^ context ].
	(context := context sender) == nil ] whileFalse.
	^ nil
</details>

#### ContextPart>>#isBottomContext

Answer if this is the last context (the first context invoked) in my sender chain


<details>
	<summary>See more</summary>
	
	isBottomContext
	"Answer if this is the last context (the first context invoked) in my sender chain"

	^sender isNil
</details>

#### ContextPart>>#shortErrorReportOn: strm

Write a short error report on the stack (above me) on a stream. For both the error file, and emailing a bug report.


<details>
	<summary>See more</summary>
	
	shortErrorReportOn: strm
	"Write a short error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report. "

	| cnt aContext |
 	strm print: Date today; space; print: Time now; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext notNil and: [(cnt _ cnt + 1) < 20]] whileTrue: [
		strm print: aContext; newLine.  "just class>>selector"	
		aContext _ aContext sender]
</details>

#### ContextPart>>#hasSender: context

Answer whether the receiver is strictly above context on the stack.


<details>
	<summary>See more</summary>
	
	hasSender: context 
	"Answer whether the receiver is strictly above context on the stack."

	| s |
	self == context ifTrue: [^false].
	s _ sender.
	[s == nil]
		whileFalse: 
			[s == context ifTrue: [^true].
			s _ s sender].
	^false
</details>

#### ContextPart>>#push: numObjects fromIndexable: anIndexableCollection

Push the elements of anIndexableCollection onto the receiver's stack. Do not call directly. Called indirectly by {1. 2. 3} constructs.


<details>
	<summary>See more</summary>
	
	push: numObjects fromIndexable: anIndexableCollection
	"Push the elements of anIndexableCollection onto the receiver's stack.
	 Do not call directly.  Called indirectly by {1. 2. 3} constructs."

	1 to: numObjects do:
		[:i | self push: (anIndexableCollection at: i)]
</details>

#### ContextPart>>#method

Answer the method of this context.


<details>
	<summary>See more</summary>
	
	method
	"Answer the method of this context."

	self subclassResponsibility
</details>

#### ContextPart>>#object: anObject instVarAt: anIndex

Primitive. Answer a fixed variable in an object. The numbering of the variables corresponds to the named instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Essential for the debugger. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	object: anObject instVarAt: anIndex
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize
</details>

#### ContextPart>>#longStack

Answer a String showing the top 100 contexts on my sender chain.


<details>
	<summary>See more</summary>
	
	longStack
	"Answer a String showing the top 100 contexts on my sender chain."

	^ String streamContents: [ :strm |
		(self stackOfSize: 100)
			do: [:item | strm print: item; newLine]]
</details>

#### ContextPart>>#resume

Roll back thisContext to self and resume. Execute unwind blocks when rolling back. ASSUMES self is a sender of thisContext


<details>
	<summary>See more</summary>
	
	resume
	"Roll back thisContext to self and resume.  Execute unwind blocks when rolling back.  ASSUMES self is a sender of thisContext"

	self resume: nil
</details>

#### ContextPart>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Simulate the action of a 'closure copy' bytecode whose result is the new BlockClosure for the following code


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Simulate the action of a 'closure copy' bytecode whose result is the
	 new BlockClosure for the following code"
	| copiedValues |
	numCopied > 0
		ifTrue:
			[copiedValues := Array new: numCopied.
			 numCopied to: 1 by: -1 do:
				[:i|
				copiedValues at: i put: self pop]]
		ifFalse:
			[copiedValues := nil].
	self push: (BlockClosure
				outerContext: self
				startpc: pc
				numArgs: numArgs
				copiedValues: copiedValues).
	self jump: blockSize
</details>

#### ContextPart>>#pushActiveContext

Simulate the action of bytecode that pushes the the active context on the top of its own stack.


<details>
	<summary>See more</summary>
	
	pushActiveContext
	"Simulate the action of bytecode that pushes the the active context on the 
	top of its own stack."

	self push: self
</details>

#### ContextPart>>#completeCallee: aContext

Simulate the execution of bytecodes until a return to the receiver.


<details>
	<summary>See more</summary>
	
	completeCallee: aContext
	"Simulate the execution of bytecodes until a return to the receiver."
	| ctxt current ctxt1 |
	ctxt _ aContext.
	[ctxt == current or: [ctxt hasSender: self]]
		whileTrue: 
			[current _ ctxt.
			ctxt1 _ ctxt quickStep.
			ctxt1 ifNil: [self halt].
			ctxt _ ctxt1].
	^self stepToSendOrReturn
</details>

#### ContextPart>>#pushNewArrayOfSize: arraySize

<details>
	<summary>See more</summary>
	
	pushNewArrayOfSize: arraySize 
	self push: (Array new: arraySize)
</details>

#### ContextPart>>#errorReportOn: strm

Write a detailed error report on the stack (above me) on a stream. For both the error file, and emailing a bug report. Suppress any errors while getting printStrings. Limit the length.


<details>
	<summary>See more</summary>
	
	errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; newLine.
	strm newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion asString;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt _ 0.  startPos _ strm position.
	aContext _ self.
	[aContext notNil and: [(cnt _ cnt + 1) < 20]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext _ aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext == nil] whileFalse: [
		cnt _ cnt + 1.
		cnt = 20 ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 60 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext _ aContext sender].

</details>

#### ContextPart>>#swapSender: coroutine

Replace the receiver's sender with coroutine and answer the receiver's previous sender. For use in coroutining.


<details>
	<summary>See more</summary>
	
	swapSender: coroutine 
	"Replace the receiver's sender with coroutine and answer the receiver's 
	previous sender. For use in coroutining."

	| oldSender |
	oldSender := sender.
	sender := coroutine.
	^oldSender
</details>

#### ContextPart>>#resume: value through: firstUnwindCtxt

Unwind thisContext to self and resume with value as result of last send. Execute any unwind blocks while unwinding. ASSUMES self is a sender of thisContext.


<details>
	<summary>See more</summary>
	
	resume: value through: firstUnwindCtxt
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: value to: self].
	ctxt := firstUnwindCtxt.
	[ctxt isNil] whileFalse:
		[(ctxt tempAt: 2) ifNil:
			[ctxt tempAt: 2 put: true.
			 unwindBlock := ctxt tempAt: 1.
			 thisContext terminateTo: ctxt.
			 unwindBlock value].
		 ctxt := ctxt findNextUnwindContextUpTo: self].
	thisContext terminateTo: self.
	^value

</details>

#### ContextPart>>#findSimilarSender

Return the closest sender with the same method, return nil if none found


<details>
	<summary>See more</summary>
	
	findSimilarSender
	"Return the closest sender with the same method, return nil if none found"

	| meth |
	meth := self method.
	^ self sender findContextSuchThat: [:c | c method == meth]
</details>

#### ContextPart>>#client

Answer the client, that is, the object that sent the message that created this context.


<details>
	<summary>See more</summary>
	
	client
	"Answer the client, that is, the object that sent the message that created this context."

	^sender receiver
</details>

#### ContextPart>>#tempAt: index put: value

Store the argument, value, as the temporary variable whose index is the argument, index.


<details>
	<summary>See more</summary>
	
	tempAt: index put: value 
	"Store the argument, value, as the temporary variable whose index is the 
	argument, index."

	self subclassResponsibility
</details>

#### ContextPart>>#contextStack

Answer an Array of the contexts on the receiver's sender chain.


<details>
	<summary>See more</summary>
	
	contextStack 
	"Answer an Array of the contexts on the receiver's sender chain."

	^self stackOfSize: 100000
</details>

#### ContextPart>>#privSender: aContext

<details>
	<summary>See more</summary>
	
	privSender: aContext 

	sender _ aContext
</details>

#### ContextPart>>#singleRelease

Remove information from the receiver in order to break circularities.


<details>
	<summary>See more</summary>
	
	singleRelease
	"Remove information from the receiver in order to break circularities."

	stackp ifNotNil: [1 to: stackp do: [:i | self at: i put: nil]].
	sender _ nil.
	pc _ nil
</details>

#### ContextPart>>#isContext

<details>
	<summary>See more</summary>
	
	isContext
	^true
</details>

#### ContextPart>>#findContextSuchThat: testBlock

Search self and my sender chain for first one that satisfies testBlock. Return nil if none satisfy


<details>
	<summary>See more</summary>
	
	findContextSuchThat: testBlock
	"Search self and my sender chain for first one that satisfies testBlock.  Return nil if none satisfy"

	| ctxt |
	ctxt _ self.
	[ctxt isNil] whileFalse: [
		(testBlock value: ctxt) ifTrue: [^ ctxt].
		ctxt _ ctxt sender.
	].
	^ nil
</details>

#### ContextPart>>#object: anObject basicAt: index put: value

Store the last argument value in the indexable element of the argument anObject indicated by index without sending anObject a message. Fail if the argument index is not an Integer or is out of bounds, or if anObject is not indexable, or if value is an inappropriate value for anObject's indexable slots. This mimics the action of the VM when it indexes an object. Used to simulate the execution machinery by, for example, the debugger. Primitive. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	object: anObject basicAt: index put: value 
	"Store the last argument 
	 value in the indexable element of the argument anObject indicated by index without sending
	 anObject a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable, or if value is an inappropriate value for anObject's indexable slots.
	 This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= (self objectSize: anObject)])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Simulate the action of bytecode that stores the top of the stack at an offset in one of my local variables being used as a remote temp vector.


<details>
	<summary>See more</summary>
	
	storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that stores the top of the stack at
	 an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self top
</details>

#### ContextPart>>#shortStack

Answer a String showing the top ten contexts on my sender chain.


<details>
	<summary>See more</summary>
	
	shortStack
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [ :strm |
		(self stackOfSize: 10)
			do: [:item | strm print: item; newLine]]
</details>

#### ContextPart>>#return: value

Unwind thisContext to self and return value to self's sender. Execute any unwind blocks while unwinding. ASSUMES self is a sender of thisContext


<details>
	<summary>See more</summary>
	
	return: value
	"Unwind thisContext to self and return value to self's sender.  Execute any unwind blocks while unwinding.  ASSUMES self is a sender of thisContext"

	sender ifNil: [self cannotReturn: value to: sender].
	sender resume: value
</details>

#### ContextPart>>#resumeEvaluating: aBlock

Unwind thisContext to self and resume with aBlock value as result of last send. Execute unwind blocks when unwinding. ASSUMES self is a sender of thisContext


<details>
	<summary>See more</summary>
	
	resumeEvaluating: aBlock
	"Unwind thisContext to self and resume with aBlock value as result of last send.  Execute unwind blocks when unwinding.  ASSUMES self is a sender of thisContext"

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: aBlock value to: self].
	ctxt := thisContext.
	[
		ctxt := ctxt findNextUnwindContextUpTo: self.
		ctxt isNil
	] whileFalse: [
		(ctxt tempAt: 2) isNil ifTrue:
			[
				ctxt tempAt: 2 put: true.
				unwindBlock := ctxt tempAt: 1.
				thisContext terminateTo: ctxt.
				unwindBlock value
			]
	].
	thisContext terminateTo: self.
	^aBlock value
</details>

#### ContextPart>>#resume: value

<details>
	<summary>See more</summary>
	
	resume: value

	self resumeEvaluating: [value]
</details>

#### ContextPart>>#stackp: newStackp

Storing into the stack pointer is a potentially dangerous thing. This primitive stores nil into any cells that become accessible as a result, and it performs the entire operation atomically.


<details>
	<summary>See more</summary>
	
	stackp: newStackp
	"Storing into the stack pointer is a potentially dangerous thing.
	This primitive stores nil into any cells that become accessible as a result,
	and it performs the entire operation atomically."
	"Once this primitive is implemented, failure code should cause an error"

	<primitive: 76>
	self error: 'stackp store failure'.
"
	stackp == nil ifTrue: [stackp _ 0].
	newStackp > stackp  'effectively checks that it is a number'
		ifTrue: [oldStackp _ stackp.
				stackp _ newStackp.
				'Nil any newly accessible cells'
				oldStackp + 1 to: stackp do: [:i | self at: i put: nil]]
		ifFalse: [stackp _ newStackp]
"
</details>

#### ContextPart>>#methodReturnReceiver

Simulate the action of a 'return receiver' bytecode. This corresponds to the source expression '^self'.


<details>
	<summary>See more</summary>
	
	methodReturnReceiver
	"Simulate the action of a 'return receiver' bytecode. This corresponds to
	 the source expression '^self'."

	^self return: self receiver from: self methodReturnContext
</details>

#### ContextPart>>#tempsAndValues

Return a string of the temporary variabls and their current values


<details>
	<summary>See more</summary>
	
	tempsAndValues
	"Return a string of the temporary variabls and their current values"
	^self debuggerMap tempsAndValuesForContext: self
</details>

#### ContextPart>>#stepToSendOrReturn

Simulate the execution of bytecodes until either sending a message or returning a value to the receiver (that is, until switching contexts).


<details>
	<summary>See more</summary>
	
	stepToSendOrReturn
	"Simulate the execution of bytecodes until either sending a message or 
	 returning a value to the receiver (that is, until switching contexts)."

	| ctxt |
	[self willSend or: [self willReturn or: [self willReallyStore]]] whileFalse:
		[ctxt := self step.
		 ctxt == self ifFalse:
			[self halt. 
			 "Caused by mustBeBoolean handling"
			 ^ctxt]]
</details>

#### ContextPart>>#send: selector to: rcvr with: arguments lookupIn: lookupClass

Simulate the action of sending a message with selector and arguments to rcvr. The argument, lookupClass, is the class in which to lookup the message. This is the receiver's class for normal messages, but for super messages it will be some specific class related to the source method.


<details>
	<summary>See more</summary>
	
	send: selector to: rcvr with: arguments lookupIn: lookupClass
	"Simulate the action of sending a message with selector and arguments
	 to rcvr. The argument, lookupClass, is the class in which to lookup the
	 message.  This is the receiver's class for normal messages, but for super
	 messages it will be some specific class related to the source method."

	| meth primIndex val ctxt |
	(meth := lookupClass lookupSelector: selector) ifNil:
		[^self send: #doesNotUnderstand:
				to: rcvr
				with: {Message selector: selector arguments: arguments}
				lookupIn: lookupClass].
	(primIndex := meth primitive) > 0 ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		 (self isPrimFailToken: val) ifFalse:
			[^val]].
	(selector == #doesNotUnderstand: and: [lookupClass == ProtoObject]) ifTrue:
		[^self error: 'Simulated message ', arguments first selector, ' not understood'].
	ctxt := MethodContext sender: self receiver: rcvr method: meth arguments: arguments.
	primIndex > 0 ifTrue:
		[ctxt failPrimitiveWith: val].
	^ctxt
</details>

#### ContextPart>>#object: anObject basicAt: index

Answer the value of an indexable element in the argument anObject without sending it a message. Fail if the argument index is not an Integer or is out of bounds, or if anObject is not indexable. This mimics the action of the VM when it indexes an object. Used to simulate the execution machinery by, for example, the debugger. Primitive. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	object: anObject basicAt: index 
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#messageForYourself

<details>
	<summary>See more</summary>
	
	messageForYourself

	^Message selector: self selector arguments: self arguments.
	
</details>

#### ContextPart>>#isHandlerContext

is this context for #on:do:?


<details>
	<summary>See more</summary>
	
	isHandlerContext
	"is this context for #on:do:?"
	^self isHandlerOrSignalingContext and: [ self selector == #on:do: ]
</details>

#### ContextPart>>#object: anObject instVarAt: anIndex put: aValue

Primitive. Store a value into a fixed variable in the argument anObject. The numbering of the variables corresponds to the named instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Answer the value stored as the result. Using this message violates the principle that each object has sovereign control over the storing of values into its instance variables. Essential for the debugger. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	object: anObject instVarAt: anIndex put: aValue 
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable.  Answer the value stored as the result. Using this
	 message violates the  principle that each object has sovereign control
	 over the storing of values into its instance variables. Essential for the
	 debugger. See Object documentation whatIsAPrimitive."

	<primitive: 74>
	"Access beyond fixed fields"
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize put: aValue
</details>

#### ContextPart>>#methodReturnConstant: value

Simulate the action of a 'return constant' bytecode whose value is the argument, value. This corresponds to a source expression like '^0'.


<details>
	<summary>See more</summary>
	
	methodReturnConstant: value
	"Simulate the action of a 'return constant' bytecode whose value is the
	 argument, value. This corresponds to a source expression like '^0'."

	^self return: value from: self methodReturnContext
</details>

#### ContextPart>>#pushReceiverVariable: offset

Simulate the action of bytecode that pushes the contents of the receiver's instance variable whose index is the argument, index, on the top of the stack.


<details>
	<summary>See more</summary>
	
	pushReceiverVariable: offset 
	"Simulate the action of bytecode that pushes the contents of the receiver's 
	instance variable whose index is the argument, index, on the top of the 
	stack."

	self push: (self object: self receiver instVarAt: offset + 1)
</details>

#### ContextPart>>#at: index put: value

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	at: index put: value
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#doPop

Simulate the action of a 'remove top of stack' bytecode.


<details>
	<summary>See more</summary>
	
	doPop
	"Simulate the action of a 'remove top of stack' bytecode."

	self pop
</details>

#### ContextPart>>#terminateTo: previousContext

Terminate all the Contexts between me and previousContext, if previousContext is on my Context stack. Make previousContext my sender.


<details>
	<summary>See more</summary>
	
	terminateTo: previousContext
	"Terminate all the Contexts between me and previousContext, if previousContext is on my Context stack. Make previousContext my sender."

	| currentContext sendingContext |
	<primitive: 196>
	(self hasSender: previousContext) ifTrue: [
		currentContext := sender.
		[currentContext == previousContext] whileFalse: [
			sendingContext := currentContext sender.
			currentContext terminate.
			currentContext := sendingContext]].
	sender := previousContext
</details>

#### ContextPart>>#basicSize

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. Do not override in any subclass. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed
</details>

#### ContextPart>>#stepToCallee

Step to callee or sender


<details>
	<summary>See more</summary>
	
	stepToCallee
	"Step to callee or sender"

	| ctxt |
	ctxt _ self.
	[(ctxt _ ctxt step) == self] whileTrue.
	^ ctxt
</details>

#### ContextPart>>#releaseTo: caller

Remove information from the receiver and the contexts on its sender chain up to caller in order to break circularities.


<details>
	<summary>See more</summary>
	
	releaseTo: caller 
	"Remove information from the receiver and the contexts on its sender 
	chain up to caller in order to break circularities."

	| c s |
	c _ self.
	[c == nil or: [c == caller]]
		whileFalse: 
			[s _ c sender.
			c singleRelease.
			c _ s]
</details>

#### ContextPart>>#isHandlerOrSignalingContext

Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) are marked with primitive 199.


<details>
	<summary>See more</summary>
	
	isHandlerOrSignalingContext
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^false
</details>

#### ContextPart>>#object: anObject perform: selector withArguments: argArray inClass: lookupClass

Send the selector, aSymbol, to anObject with arguments in argArray. Fail if the number of arguments expected by the selector does not match the size of argArray, or if lookupClass cannot be found among the anObject's superclasses. Primitive. Essential for the debugger.


<details>
	<summary>See more</summary>
	
	object: anObject perform: selector withArguments: argArray inClass: lookupClass
	"Send the selector, aSymbol, to anObject with arguments in argArray.
	 Fail if the number of arguments expected by the selector 
	 does not match the size of argArray, or if lookupClass
	 cannot be found among the anObject's superclasses.
	 Primitive. Essential for the debugger."

	<primitive: 100 error: error>
	(selector isSymbol) ifFalse:
		[^self error: 'selector argument must be a Symbol'].
	(argArray isMemberOf: Array) ifFalse:
		[^self error: 'argArray must be an Array'].
	(selector numArgs = argArray size)
		ifFalse: [^self error: 'incorrect number of arguments'].
	((self objectClass: anObject) == lookupClass
	 or: [(self objectClass: anObject) inheritsFrom: lookupClass]) ifFalse:
		[^self error: 'lookupClass is not in anObject''s inheritance chain'].
	self primitiveFailed
</details>

#### ContextPart>>#contextForLocalVariables

Answer the context in which local variables (temporaries) are stored.


<details>
	<summary>See more</summary>
	
	contextForLocalVariables
	"Answer the context in which local variables (temporaries) are stored."

	self subclassResponsibility
</details>

#### ContextPart>>#objectClass: anObject

<details>
	<summary>See more</summary>
	
	objectClass: anObject

	<primitive: 111>
	self primitiveFailed
</details>

#### ContextPart>>#tempAt: index

Answer the value of the temporary variable whose index is the argument, index.


<details>
	<summary>See more</summary>
	
	tempAt: index
	"Answer the value of the temporary variable whose index is the 
	argument, index."

	self subclassResponsibility
</details>

#### ContextPart>>#stackOfSize: limit

Answer an OrderedCollection of the top 'limit' contexts on the receiver's sender chain.


<details>
	<summary>See more</summary>
	
	stackOfSize: limit 
	"Answer an OrderedCollection of the top 'limit' contexts
	 on the receiver's sender chain."

	| stack ctxt |
	stack := OrderedCollection new.
	stack addLast: (ctxt := self).
	[(ctxt := ctxt sender) notNil
	 and: [stack size < limit]] whileTrue:
		[stack addLast: ctxt].
	^stack
</details>

#### ContextPart>>#print: anObject on: aStream

Safely print anObject in the face of direct ProtoObject subclasses.


<details>
	<summary>See more</summary>
	
	print: anObject on: aStream
	"Safely print anObject in the face of direct ProtoObject subclasses."
	| objClass |
	objClass _ self objectClass: anObject.
	(objClass canUnderstand: #printOn:) ifTrue: [
		^anObject printOn: aStream].
	aStream nextPutAll: objClass name withArticle
</details>

#### ContextPart>>#pop

Answer the top of the receiver's stack and remove the top of the stack.


<details>
	<summary>See more</summary>
	
	pop
	"Answer the top of the receiver's stack and remove the top of the stack."
	| val |
	val _ self at: stackp.
	self stackp: stackp - 1.
	^ val
</details>

#### ContextPart>>#handleSignal: exception

Sent to handler (on:do:) contexts only. If my exception class (first arg) handles exception then execute my handle block (second arg), otherwise forward this message to the next handler context. If none left, execute exception's defaultAction (see nil>>handleSignal:).


<details>
	<summary>See more</summary>
	
	handleSignal: exception
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then execute my handle block (second arg), otherwise forward this message to the next handler context.  If none left, execute exception's defaultAction (see nil>>handleSignal:)."

	(self exceptionClass handles: exception)
		ifFalse: [ ^ self nextHandlerContext handleSignal: exception ].
	self evaluateSignal: exception
</details>

#### ContextPart>>#terminate

Make myself unresumable.


<details>
	<summary>See more</summary>
	
	terminate
	"Make myself unresumable."

	sender _ nil.
	pc _ nil.

</details>

#### ContextPart>>#quickSend: selector to: rcvr with: arguments super: superFlag

Send the given selector with arguments in an environment which closely resembles the non-simulating environment, with an interjected unwind-protected block to catch nonlocal returns. Attention: don't get lost! This beautiful method is due to Hans-Martin Mosner. Eliot Miranda merely added the mirror primitive code.


<details>
	<summary>See more</summary>
	
	quickSend: selector to: rcvr with: arguments super: superFlag
	"Send the given selector with arguments in an environment which closely resembles
	 the non-simulating environment, with an interjected unwind-protected block to catch
	 nonlocal returns.  Attention: don't get lost!  This beautiful method is due to
	 Hans-Martin Mosner.  Eliot Miranda merely added the mirror primitive code."
	| oldSender contextToReturnTo result lookupClass |
	contextToReturnTo := self.
	lookupClass := superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: rcvr].
	[oldSender := thisContext sender swapSender: self.
	 result := self object: rcvr perform: selector withArguments: arguments inClass: lookupClass.
	 thisContext sender swapSender: oldSender] ifCurtailed:
		[contextToReturnTo := thisContext sender receiver.	"The block context returning nonlocally"
		contextToReturnTo pc: contextToReturnTo previousPc.	"skip to front of return bytecode causing this unwind"
		contextToReturnTo willReturnTopFromMethod ifTrue:
			"If it was a returnTop, push the value to be returned.
			Otherwise the value is implicit in the bytecode"
			[contextToReturnTo push: (thisContext sender tempAt: 1)].
		thisContext swapSender: thisContext home sender.	"Make this block return to the method's sender"
		contextToReturnTo].
	contextToReturnTo push: result.
	^contextToReturnTo
</details>

#### ContextPart>>#depthBelow: aContext

Answer how many calls there are between this and aContext.


<details>
	<summary>See more</summary>
	
	depthBelow: aContext
	"Answer how many calls there are between this and aContext."

	| this depth |
	this _ self.
	depth _ 0.
	[this == aContext or: [this == nil]]
		whileFalse:
			[this _ this sender.
			depth _ depth + 1].
	^depth
</details>

#### ContextPart>>#methodReturnContext

Answer the context from which an ^-return should return from.


<details>
	<summary>See more</summary>
	
	methodReturnContext
	"Answer the context from which an ^-return should return from."

	self subclassResponsibility
</details>

#### ContextPart>>#printStack: depth

<details>
	<summary>See more</summary>
	
	printStack: depth
	self print.
	depth > 0 ifTrue: [
		self sender ifNotNil: [ :s | s printStack: depth-1 ]]
</details>

#### ContextPart>>#return: value to: sendr

Simulate the return of value to sendr.


<details>
	<summary>See more</summary>
	
	return: value to: sendr 
	"Simulate the return of value to sendr."

	self releaseTo: sendr.
	sendr ifNil: [^ nil].
	^ sendr push: value
</details>

#### ContextPart>>#pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Simulate the action of bytecode that pushes the value at remoteTempIndex in one of my local variables being used as a remote temp vector.


<details>
	<summary>See more</summary>
	
	pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that pushes the value at remoteTempIndex
	 in one of my local variables being used as a remote temp vector."
	self push: (self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1)
</details>

#### ContextPart>>#runSimulated: aBlock contextAtEachStep: block2

Simulate the execution of the argument, aBlock, until it ends. aBlock MUST NOT contain an '^'. Evaluate block2 with the current context prior to each instruction executed. Answer the simulated value of aBlock.


<details>
	<summary>See more</summary>
	
	runSimulated: aBlock contextAtEachStep: block2
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current |
	aBlock hasNonLocalReturn
		ifTrue: [self error: 'simulation of blocks with ^ can run loose'].
	current := aBlock asContext.
	current pushArgs: Array new from: self.
	[current == self]
		whileFalse:
			[block2 value: current.
			current := current step].
	^self pop
</details>

#### ContextPart>>#unwindTo: aContext

<details>
	<summary>See more</summary>
	
	unwindTo: aContext

	| ctx unwindBlock |
	ctx := self.
	[(ctx := ctx findNextUnwindContextUpTo: aContext) isNil] whileFalse: [
		(ctx tempAt: 2) ifNil:[
			ctx tempAt: 2 put: true.
			unwindBlock := ctx tempAt: 1.
			unwindBlock value]
	].

</details>

#### ContextPart>>#methodNode

<details>
	<summary>See more</summary>
	
	methodNode
	^ self method methodNode.
</details>

#### ContextPart>>#basicAt: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	basicAt: index
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#storeIntoLiteralVariable: value

Simulate the action of bytecode that stores the top of the stack into a literal variable of my method.


<details>
	<summary>See more</summary>
	
	storeIntoLiteralVariable: value 
	"Simulate the action of bytecode that stores the top of the stack into a 
	literal variable of my method."

	self object: value instVarAt: self class valueIndex put: self top
</details>

#### ContextPart>>#return

Unwind until my sender is on top


<details>
	<summary>See more</summary>
	
	return
	"Unwind until my sender is on top"

	self return: self receiver
</details>

#### ContextPart>>#namedTempAt: index

Answer the value of the temp at index in the receiver's sequence of tempNames.


<details>
	<summary>See more</summary>
	
	namedTempAt: index
	"Answer the value of the temp at index in the receiver's sequence of tempNames."
	^self debuggerMap namedTempAt: index in: self
</details>

#### ContextPart>>#pushConstant: value

Simulate the action of bytecode that pushes the constant, value, on the top of the stack.


<details>
	<summary>See more</summary>
	
	pushConstant: value 
	"Simulate the action of bytecode that pushes the constant, value, on the 
	top of the stack."

	self push: value
</details>

#### ContextPart>>#secondFromBottom

Return the second from bottom of my sender chain


<details>
	<summary>See more</summary>
	
	secondFromBottom
	"Return the second from bottom of my sender chain"

	self sender ifNil: [^ nil].
	^ self findContextSuchThat: [:c | c sender sender isNil]
</details>

#### ContextPart>>#findNextHandlerContext

Return the next handler marked context, returning nil if there is none. Search starts with self and proceeds up to nil. If context isn't a handler context, it must be a signaling context. When we reach a signaling context we must skip over any handlers that might be on the stack between the signaling context and the handler context for that signal.


<details>
	<summary>See more</summary>
	
	findNextHandlerContext
	"Return the next handler marked context, returning nil if there is none.  Search starts
	with self and proceeds up to nil.  If context isn't a handler context, it must be a signaling
	context.  When we reach a signaling context we must skip over any handlers that might
	be on the stack between the signaling context and the handler context for that signal."

	| context searchStartContext |
	context := self findNextHandlerOrSignalingContext.
	context isNil ifTrue: [^nil].
	context isHandlerContext ifTrue: [^context]. 
	searchStartContext := context exceptionClass topHandlerContext ifNil: [context].
	^searchStartContext nextHandlerContext
</details>

#### ContextPart>>#cut: aContext

Cut aContext and its senders from my sender chain


<details>
	<summary>See more</summary>
	
	cut: aContext
	"Cut aContext and its senders from my sender chain"

	| ctxt callee |
	ctxt _ self.
	[ctxt == aContext] whileFalse: [
		callee _ ctxt.
		ctxt _ ctxt sender.
		ctxt ifNil: [aContext ifNotNil: [self error: 'aContext not a sender']].
	].
	callee privSender: nil.

</details>

#### ContextPart>>#methodReturnTop

Simulate the action of a 'return top of stack' bytecode. This corresponds to source expressions like '^something'.


<details>
	<summary>See more</summary>
	
	methodReturnTop
	"Simulate the action of a 'return top of stack' bytecode. This corresponds
	 to source expressions like '^something'."

	^self return: self pop from: self methodReturnContext
</details>

#### ContextPart>>#activateReturn: aContext value: value

Activate 'aContext return: value' in place of self, so execution will return to aContext's sender


<details>
	<summary>See more</summary>
	
	activateReturn: aContext value: value
	"Activate 'aContext return: value' in place of self, so execution will return to aContext's sender"

	^MethodContext 
		sender: self
		receiver: aContext
		method: MethodContext theReturnMethod
		arguments: {value}
</details>

#### ContextPart>>#hasContext: aContext

Answer whether aContext is me or one of my senders


<details>
	<summary>See more</summary>
	
	hasContext: aContext 
	"Answer whether aContext is me or one of my senders"

	^ (self findContextSuchThat: [:c | c == aContext]) notNil
</details>

#### ContextPart>>#popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Simulate the action of bytecode that removes the top of the stack and stores it into an offset in one of my local variables being used as a remote temp vector.


<details>
	<summary>See more</summary>
	
	popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that removes the top of the stack and  stores
	 it into an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self pop
</details>

#### ContextPart>>#evaluateSignal: exception

The following primitive is just a marker used to find the evaluation context. See MethodContext>>#isHandlerOrSignalingContext.


<details>
	<summary>See more</summary>
	
	evaluateSignal: exception
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception pushHandlerContext: self contextTag.
	value := exception evaluateHandlerBlock: self exceptionHandlerBlock.
	"return from self if not otherwise directed in handle block"
	self return: value
</details>

#### ContextPart>>#jump: distance

Simulate the action of a 'unconditional jump' bytecode whose offset is the argument, distance.


<details>
	<summary>See more</summary>
	
	jump: distance 
	"Simulate the action of a 'unconditional jump' bytecode whose offset is 
	the argument, distance."

	pc _ pc + distance
</details>

#### ContextPart>>#selector

Answer the selector of the method that created the receiver.


<details>
	<summary>See more</summary>
	
	selector
	"Answer the selector of the method that created the receiver."

	^self method selector ifNil: [self method defaultSelector].
</details>

#### ContextPart>>#storeIntoReceiverVariable: offset

Simulate the action of bytecode that stores the top of the stack into an instance variable of my receiver.


<details>
	<summary>See more</summary>
	
	storeIntoReceiverVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into an 
	instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self top
</details>

#### ContextPart>>#return: value from: aSender

For simulation. Roll back self to aSender and return value from it. Execute any unwind blocks on the way. ASSUMES aSender is a sender of self


<details>
	<summary>See more</summary>
	
	return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop ctxt |
	aSender isDead ifTrue: [
		^ self send: #cannotReturn: to: self with: {value} super: false].
	newTop _ aSender sender.
	ctxt _ self findNextUnwindContextUpTo: newTop.
	ctxt ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. ctxt} super: false].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^ newTop

</details>

#### ContextPart>>#depthBelow

<details>
	<summary>See more</summary>
	
	depthBelow

	^self depthBelow: nil
</details>

#### ContextPart>>#closureCopy: numArgs copiedValues: anArray

Distinguish a block of code from its enclosing method by creating a BlockClosure for that block. The compiler inserts into all methods that contain blocks the bytecodes to send the message closureCopy:copiedValues:. Do not use closureCopy:copiedValues: in code that you write! Only the compiler can decide to send the message closureCopy:copiedValues:. Fail if numArgs is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	closureCopy: numArgs copiedValues: anArray
	"Distinguish a block of code from its enclosing method by 
	creating a BlockClosure for that block. The compiler inserts into all 
	methods that contain blocks the bytecodes to send the message 
	closureCopy:copiedValues:. Do not use closureCopy:copiedValues: in code that you write! Only the 
	compiler can decide to send the message closureCopy:copiedValues:. Fail if numArgs is 
	not a SmallInteger. Optional. No Lookup. See Object documentation 
	whatIsAPrimitive."

	<primitive: 200>
	^BlockClosure outerContext: self startpc: pc + 2 numArgs: numArgs copiedValues: anArray
</details>

#### ContextPart>>#jump: distance if: condition

Simulate the action of a 'conditional jump' bytecode whose offset is the argument, distance, and whose condition is the argument, condition.


<details>
	<summary>See more</summary>
	
	jump: distance if: condition 
	"Simulate the action of a 'conditional jump' bytecode whose offset is the 
	argument, distance, and whose condition is the argument, condition."

	| bool |
	bool _ self pop.
	(bool == true or: [bool == false]) ifFalse: [
		^self
			send: #mustBeBooleanIn:
			to: bool
			with: {self}
			super: false].
	(bool eqv: condition) ifTrue: [self jump: distance]
</details>

#### ContextPart>>#insertSender: aContext

Insert aContext and its sender chain between me and my sender. Return new callee of my original sender.


<details>
	<summary>See more</summary>
	
	insertSender: aContext
	"Insert aContext and its sender chain between me and my sender.  Return new callee of my original sender."

	| ctxt |
	ctxt _ aContext bottomContext.
	ctxt privSender: self sender.
	self privSender: aContext.
	^ ctxt
</details>

#### ContextPart>>#jump

Abandon thisContext and resume self instead (using the same current process). You may want to save thisContext's sender before calling this so you can jump back to it. Self MUST BE a top context (ie. a suspended context or a abandoned context that was jumped out of). A top context already has its return value on its stack (see Interpreter>>primitiveSuspend and other suspending primitives). thisContext's sender is converted to a top context (by pushing a nil return value on its stack) so it can be jump back to.


<details>
	<summary>See more</summary>
	
	jump
	"Abandon thisContext and resume self instead (using the same current process).  You may want to save thisContext's sender before calling this so you can jump back to it.
	Self MUST BE a top context (ie. a suspended context or a abandoned context that was jumped out of).  A top context already has its return value on its stack (see Interpreter>>primitiveSuspend and other suspending primitives).
	thisContext's sender is converted to a top context (by pushing a nil return value on its stack) so it can be jump back to."

	| top |
	"Make abandoned context a top context (has return value (nil)) so it can be jumped back to"
	thisContext sender push: nil.

	"Pop self return value then return it to self (since we jump to self by returning to it)"
	stackp = 0 ifTrue: [self stepToSendOrReturn].
	stackp = 0 ifTrue: [self push: nil].  "must be quick return self/constant"
	top _ self pop.
	thisContext privSender: self.
	^ top
</details>

#### ContextPart>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	at: index
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### ContextPart>>#pushReceiver

Simulate the action of bytecode that pushes the active context's receiver on the top of the stack.


<details>
	<summary>See more</summary>
	
	pushReceiver
	"Simulate the action of bytecode that pushes the active context's receiver 
	on the top of the stack."

	self push: self receiver
</details>

#### ContextPart>>#findNextUnwindContextUpTo: aContext

Return the next unwind marked above the receiver, returning nil if there is none. Search proceeds up to but not including aContext.


<details>
	<summary>See more</summary>
	
	findNextUnwindContextUpTo: aContext
	"Return the next unwind marked above the receiver, returning nil if there is none.  Search proceeds up to but not including aContext."

	| ctx |
	<primitive: 195>
	ctx _ self.
		[(ctx _ ctx sender) == nil or: [ctx == aContext]] whileFalse:
		[ ctx isUnwindContext ifTrue: [^ctx]].
	^nil
</details>

#### ContextPart>>#tempsAndValuesLimitedTo: sizeLimit indent: indent

Return a string of the temporary variabls and their current values


<details>
	<summary>See more</summary>
	
	tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variabls and their current values"

	| aStream |
	aStream _ WriteStream on: (String new: 100).
	self tempNames
		withIndexDo: [:title :index |
			indent timesRepeat: [aStream tab].
			aStream nextPutAll: title; nextPut: $:; space; tab.
			aStream nextPutAll: 
				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
			aStream newLine].
	^aStream contents
</details>

#### ContextPart>>#doDup

Simulate the action of a 'duplicate top of stack' bytecode.


<details>
	<summary>See more</summary>
	
	doDup
	"Simulate the action of a 'duplicate top of stack' bytecode."

	self push: self top
</details>

#### ContextPart>>#popIntoTemporaryVariable: offset

Simulate the action of bytecode that removes the top of the stack and stores it into one of my temporary variables.


<details>
	<summary>See more</summary>
	
	popIntoTemporaryVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into one of my temporary variables."

	self contextForLocalVariables at: offset + 1 put: self pop
</details>

#### ContextPart>>#printDetails: strm

Put my class>>selector and arguments and temporaries on the stream. Protect against errors during printing.


<details>
	<summary>See more</summary>
	
	printDetails: strm
	"Put my class>>selector and arguments and temporaries on the stream.  Protect against errors during printing."

	| str |
	self printOn: strm.  

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str := [self tempsAndValuesLimitedTo: 160 indent: 2] ifError: [:err :rcvr | 
						'<<error during printing>>'].
	strm nextPutAll: str.
	strm peekLast isLineSeparator ifFalse: [strm newLine]
</details>

#### ContextPart>>#popIntoLiteralVariable: value

Simulate the action of bytecode that removes the top of the stack and stores it into a literal variable of my method.


<details>
	<summary>See more</summary>
	
	popIntoLiteralVariable: value 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into a literal variable of my method."

	self object: value instVarAt: self class valueIndex put: self pop
</details>

#### ContextPart>>#quickStep

If the next instruction is a send, just perform it. Otherwise, do a normal step.


<details>
	<summary>See more</summary>
	
	quickStep
	"If the next instruction is a send, just perform it.
	Otherwise, do a normal step."

	self willSend ifTrue: [QuickStep _ self].
	^self step
</details>

#### ContextPart>>#tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments

Invoke the named primitive for aCompiledMethod, answering its result, or, if the primiitve fails, answering the error code.


<details>
	<summary>See more</summary>
	
	tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments
	"Invoke the named primitive for aCompiledMethod, answering its result, or,
	 if the primiitve fails, answering the error code."
	<primitive: 218 error: ec>
	ec ifNotNil:
		["If ec is an integer other than -1 there was a problem with primitive 218,
		  not with the external primitive itself.  -1 indicates a generic failure (where
		  ec should be nil) but ec = nil means primitive 218 is not implemented.  So
		  interpret -1 to mean the external primitive failed with a nil error code."
		 ec isInteger ifTrue:
			[ec = -1
				ifTrue: [ec := nil]
				ifFalse: [self primitiveFailed]]].
	^self class primitiveFailTokenFor: ec
</details>

#### ContextPart>>#copyStack

<details>
	<summary>See more</summary>
	
	copyStack

	^ self copyTo: nil
</details>

#### ContextPart>>#objectSize: anObject

Answer the number of indexable variables in the argument anObject without sending it a message. This mimics the action of the VM when it fetches an object's variable size. Used to simulate the execution machinery by, for example, the debugger. Primitive. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	objectSize: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0
</details>

#### ContextPart>>#isDead

Has self finished


<details>
	<summary>See more</summary>
	
	isDead
	"Has self finished"

	^ pc isNil
</details>

#### ContextPart>>#pushLiteralVariable: value

Simulate the action of bytecode that pushes the contents of the literal variable whose index is the argument, index, on the top of the stack.


<details>
	<summary>See more</summary>
	
	pushLiteralVariable: value 
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: (self object: value instVarAt: self class valueIndex)
</details>

#### ContextPart>>#sourceCode

<details>
	<summary>See more</summary>
	
	sourceCode
	^self method getSource.
	
	"Note: The above is a bit safer than
		^ methodClass sourceCodeAt: selector
	which may fail if the receiver's method has been changed in
	the debugger (e.g., the method is no longer in the methodDict
	and thus the above selector is something like #Doit:with:with:with:)
	but the source code is still available."
</details>

#### ContextPart>>#isUnwindContext

<details>
	<summary>See more</summary>
	
	isUnwindContext

	^false
</details>

#### ContextPart>>#popIntoReceiverVariable: offset

Simulate the action of bytecode that removes the top of the stack and stores it into an instance variable of my receiver.


<details>
	<summary>See more</summary>
	
	popIntoReceiverVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into an instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self pop
</details>

#### ContextPart>>#pushTemporaryVariable: offset

Simulate the action of bytecode that pushes the contents of the temporary variable whose index is the argument, index, on the top of the stack.


<details>
	<summary>See more</summary>
	
	pushTemporaryVariable: offset 
	"Simulate the action of bytecode that pushes the contents of the 
	temporary variable whose index is the argument, index, on the top of 
	the stack."

	self push: (self contextForLocalVariables at: offset + 1)
</details>

#### ContextPart>>#sender

Answer the context that sent the message that created the receiver.


<details>
	<summary>See more</summary>
	
	sender
	"Answer the context that sent the message that created the receiver."

	^sender
</details>

#### ContextPart>>#tempNames

Answer a SequenceableCollection of the names of the receiver's temporary variables, which are strings.


<details>
	<summary>See more</summary>
	
	tempNames
	"Answer a SequenceableCollection of the names of the receiver's temporary 
	 variables, which are strings."

	^ self debuggerMap tempNamesForContext: self
</details>

## InstructionClient

My job is to make it easier to implement clients for InstructionStream. See InstVarRefLocator as an example.

### Methods
#### InstructionClient>>#methodReturnConstant: value

Return Constant bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnConstant: value 
	"Return Constant bytecode."

</details>

#### InstructionClient>>#pushReceiverVariable: offset

Push Contents Of the Receiver's Instance Variable Whose Index is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiverVariable: offset
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

</details>

#### InstructionClient>>#pushConsArrayWithElements: numElements

Push Cons Array of size numElements popping numElements items from the stack into the array bytecode.


<details>
	<summary>See more</summary>
	
	pushConsArrayWithElements: numElements
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."

</details>

#### InstructionClient>>#doPop

Remove Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doPop
	"Remove Top Of Stack bytecode."

</details>

#### InstructionClient>>#popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Remove Top Of Stack And Store Into Offset of Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."
</details>

#### InstructionClient>>#jump: offset

Unconditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset
	"Unconditional Jump bytecode."


</details>

#### InstructionClient>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Push Closure bytecode.


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	"Push Closure bytecode."

</details>

#### InstructionClient>>#pushActiveContext

Push Active Context On Top Of Its Own Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushActiveContext
	"Push Active Context On Top Of Its Own Stack bytecode."

</details>

#### InstructionClient>>#storeIntoReceiverVariable: offset

Store Top Of Stack Into Instance Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoReceiverVariable: offset 
	"Store Top Of Stack Into Instance Variable Of Method bytecode."

</details>

#### InstructionClient>>#pushNewArrayOfSize: numElements

Push New Array of size numElements bytecode.


<details>
	<summary>See more</summary>
	
	pushNewArrayOfSize: numElements 
	"Push New Array of size numElements bytecode."

</details>

#### InstructionClient>>#jump: offset if: condition

Conditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset if: condition 
	"Conditional Jump bytecode."


</details>

#### InstructionClient>>#storeIntoTemporaryVariable: offset

Store Top Of Stack Into Temporary Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoTemporaryVariable: offset 
	"Store Top Of Stack Into Temporary Variable Of Method bytecode."

</details>

#### InstructionClient>>#pushReceiver

Push Active Context's Receiver on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiver
	"Push Active Context's Receiver on Top Of Stack bytecode."

</details>

#### InstructionClient>>#doDup

Duplicate Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doDup
	"Duplicate Top Of Stack bytecode."


</details>

#### InstructionClient>>#popIntoTemporaryVariable: offset

Remove Top Of Stack And Store Into Temporary Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoTemporaryVariable: offset 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."

</details>

#### InstructionClient>>#popIntoLiteralVariable: anAssociation

Remove Top Of Stack And Store Into Literal Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoLiteralVariable: anAssociation 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."

</details>

#### InstructionClient>>#send: selector super: supered numArgs: numberArguments

Send Message With Selector, selector, bytecode. The argument, supered, indicates whether the receiver of the message is specified with 'super' in the source method. The arguments of the message are found in the top numArguments locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: supered numArgs: numberArguments
	"Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

</details>

#### InstructionClient>>#pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Push Contents at Offset in Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Push Contents at Offset in Temp Vector bytecode."
</details>

#### InstructionClient>>#storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Store Top Of Stack And Store Into Offset of Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Store Top Of Stack And Store Into Offset of Temp Vector bytecode."
</details>

#### InstructionClient>>#pushLiteralVariable: anAssociation

Push Contents Of anAssociation On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushLiteralVariable: anAssociation
	"Push Contents Of anAssociation On Top Of Stack bytecode."

</details>

#### InstructionClient>>#callPrimitive: pimIndex

V3PlusClosures: 139 10001011 iiiiiiii jjjjjjjj Call Primitive #iiiiiiii + (jjjjjjjj * 256) NewsqueakV4: 249 11111001 iiiiiiii jjjjjjjj Call Primitive #iiiiiiii + (jjjjjjjj * 256) SistaV1: 248 11111000 iiiiiiii mjjjjjjj Call Primitive #iiiiiiii + ( jjjjjjj * 256) m=1 means inlined primitive, no hard return after execution.


<details>
	<summary>See more</summary>
	
	callPrimitive: pimIndex
	"V3PlusClosures:	139 10001011	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 NewsqueakV4:		249 11111001	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 SistaV1:			248 11111000 iiiiiiii mjjjjjjj  Call Primitive #iiiiiiii + ( jjjjjjj * 256)
							m=1 means inlined primitive, no hard return after execution."
</details>

#### InstructionClient>>#blockReturnTop

Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	blockReturnTop
	"Return Top Of Stack bytecode."


</details>

#### InstructionClient>>#storeIntoLiteralVariable: anAssociation

Store Top Of Stack Into Literal Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoLiteralVariable: anAssociation 
	"Store Top Of Stack Into Literal Variable Of Method bytecode."

</details>

#### InstructionClient>>#pushConstant: value

Push Constant, value, on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushConstant: value
	"Push Constant, value, on Top Of Stack bytecode."

</details>

#### InstructionClient>>#methodReturnReceiver

Return Self bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnReceiver
	"Return Self bytecode."

</details>

#### InstructionClient>>#popIntoReceiverVariable: offset

Remove Top Of Stack And Store Into Instance Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoReceiverVariable: offset 
	"Remove Top Of Stack And Store Into Instance Variable bytecode."

</details>

#### InstructionClient>>#pushTemporaryVariable: offset

Push Contents Of Temporary Variable Whose Index Is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushTemporaryVariable: offset
	"Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."

</details>

#### InstructionClient>>#methodReturnTop

Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnTop
	"Return Top Of Stack bytecode."

</details>

## InstructionPrinter

My instances can print the object code of a CompiledMethod in symbolic format. They print into an instance variable, stream, and uses oldPC to determine how many bytes to print in the listing. The variable method is used to hold the method being printed.

### Methods
#### InstructionPrinter>>#method

<details>
	<summary>See more</summary>
	
	method
	^method.
</details>

#### InstructionPrinter>>#methodReturnConstant: value

Print the Return Constant bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnConstant: value 
	"Print the Return Constant bytecode."

	self print: 'return: ' , value printString
</details>

#### InstructionPrinter>>#pushReceiverVariable: offset

Print the Push Contents Of the Receiver's Instance Variable Whose Index is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiverVariable: offset
	"Print the Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

	self print: 'pushRcvr: ' , offset printString
</details>

#### InstructionPrinter>>#pushConsArrayWithElements: numElements

Push Cons Array of size numElements popping numElements items from the stack into the array bytecode.


<details>
	<summary>See more</summary>
	
	pushConsArrayWithElements: numElements 
	self print: 'pop ', numElements printString, ' into (Array new: ', numElements printString, ')'
</details>

#### InstructionPrinter>>#doPop

Print the Remove Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doPop
	"Print the Remove Top Of Stack bytecode."

	self print: 'pop'
</details>

#### InstructionPrinter>>#indent

<details>
	<summary>See more</summary>
	
	indent

	^ indent ifNil: [0]
</details>

#### InstructionPrinter>>#jump: offset

Print the Unconditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset
	"Print the Unconditional Jump bytecode."

	self print: 'jumpTo: ' , (scanner pc + offset) printString.
	indentSpanOfFollowingJump ifTrue:
		[indentSpanOfFollowingJump := false.
		 innerIndents atAll: (scanner pc to: scanner pc + offset - 1) put: (innerIndents at: scanner pc - 1) + 1]
</details>

#### InstructionPrinter>>#popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Remove Top Of Stack And Store Into Offset of Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	self print: 'popIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
</details>

#### InstructionPrinter>>#pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize

Push Closure bytecode.


<details>
	<summary>See more</summary>
	
	pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	self print: 'closureNumCopied: ', numCopied printString
			, ' numArgs: ', numArgs printString
			, ' bytes ', scanner pc printString
			, ' to ', (scanner pc + blockSize - 1) printString.
	innerIndents
		atAll: (scanner pc to: scanner pc + blockSize - 1)
		put: (innerIndents at: scanner pc - 1) + 1
</details>

#### InstructionPrinter>>#pushActiveContext

Print the Push Active Context On Top Of Its Own Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushActiveContext
	"Print the Push Active Context On Top Of Its Own Stack bytecode."

	self print: 'pushThisContext: '
</details>

#### InstructionPrinter>>#storeIntoReceiverVariable: offset

Print the Store Top Of Stack Into Instance Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoReceiverVariable: offset 
	"Print the Store Top Of Stack Into Instance Variable Of Method bytecode."

	self print: 'storeIntoRcvr: ' , offset printString
</details>

#### InstructionPrinter>>#pushNewArrayOfSize: numElements

Push New Array of size numElements bytecode.


<details>
	<summary>See more</summary>
	
	pushNewArrayOfSize: numElements 
	self print: 'push: (Array new: ', numElements printString, ')'
</details>

#### InstructionPrinter>>#jump: offset if: condition

Print the Conditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset if: condition 
	"Print the Conditional Jump bytecode."

	self print: 
		(condition
			ifTrue: ['jumpTrue: ']
			ifFalse: ['jumpFalse: '])
			, (scanner pc + offset) printString
</details>

#### InstructionPrinter>>#indent: numTabs

<details>
	<summary>See more</summary>
	
	indent: numTabs

	indent _ numTabs
</details>

#### InstructionPrinter>>#printInstructionsOn: aStream do: aBlock

Append to the stream, aStream, a description of each bytecode in the instruction stream. Evaluate aBlock with the receiver, the scanner and the stream after each instruction.


<details>
	<summary>See more</summary>
	
	printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream. Evaluate aBlock with the receiver, the scanner and
	 the stream after each instruction."

	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self.
		 aBlock value: self value: scanner value: stream]
</details>

#### InstructionPrinter>>#storeIntoTemporaryVariable: offset

Print the Store Top Of Stack Into Temporary Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoTemporaryVariable: offset 
	"Print the Store Top Of Stack Into Temporary Variable Of Method 
	bytecode."

	self print: 'storeIntoTemp: ' , offset printString
</details>

#### InstructionPrinter>>#method: aMethod

<details>
	<summary>See more</summary>
	
	method: aMethod
	method :=  aMethod.
	printPC := true.
	indentSpanOfFollowingJump := false
</details>

#### InstructionPrinter>>#pushReceiver

Print the Push Active Context's Receiver on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushReceiver
	"Print the Push Active Context's Receiver on Top Of Stack bytecode."

	self print: 'self'
</details>

#### InstructionPrinter>>#doDup

Print the Duplicate Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	doDup
	"Print the Duplicate Top Of Stack bytecode."

	self print: 'dup'
</details>

#### InstructionPrinter>>#popIntoTemporaryVariable: offset

Print the Remove Top Of Stack And Store Into Temporary Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoTemporaryVariable: offset 
	"Print the Remove Top Of Stack And Store Into Temporary Variable 
	bytecode."

	self print: 'popIntoTemp: ' , offset printString
</details>

#### InstructionPrinter>>#popIntoLiteralVariable: anAssociation

Print the Remove Top Of Stack And Store Into Literal Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoLiteralVariable: anAssociation 
	"Print the Remove Top Of Stack And Store Into Literal Variable bytecode."

	self print: 'popIntoLit: ' , anAssociation key
</details>

#### InstructionPrinter>>#send: selector super: supered numArgs: numberArguments

Print the Send Message With Selector, selector, bytecode. The argument, supered, indicates whether the receiver of the message is specified with 'super' in the source method. The arguments of the message are found in the top numArguments locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: supered numArgs: numberArguments
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered ifTrue: ['superSend: '] ifFalse: ['send: ']) , selector.
	indentSpanOfFollowingJump := #closureCopy:copiedValues: = selector
</details>

#### InstructionPrinter>>#pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Push Contents at Offset in Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self print: 'pushTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
</details>

#### InstructionPrinter>>#storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

Store Top Of Stack And Store Into Offset of Temp Vector bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex 
	self print: 'storeIntoTemp: ', remoteTempIndex printString, ' inVectorAt: ', tempVectorIndex printString
</details>

#### InstructionPrinter>>#printPC: aBoolean

<details>
	<summary>See more</summary>
	
	printPC: aBoolean
	printPC := aBoolean
</details>

#### InstructionPrinter>>#pushLiteralVariable: anAssociation

Print the Push Contents Of anAssociation On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushLiteralVariable: anAssociation
	"Print the Push Contents Of anAssociation On Top Of Stack bytecode."

	self print: 'pushLit: ' , anAssociation key
</details>

#### InstructionPrinter>>#callPrimitive: index

Print the callPrimitive bytecode.


<details>
	<summary>See more</summary>
	
	callPrimitive: index
	"Print the callPrimitive bytecode."

	self print: 'callPrimitive: ' , index printString
</details>

#### InstructionPrinter>>#print: instruction

Append to the receiver a description of the bytecode, instruction.


<details>
	<summary>See more</summary>
	
	print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	printPC ifTrue: [stream print: oldPC; space].
	stream tab: (innerIndents at: oldPC).
	stream nextPut: $<.
	oldPC to: scanner pc - 1 do: 
		[:i | 
		code := (method at: i) printStringBase: 16.
		stream nextPut: 
			(code size < 2
				ifTrue: [$0]
				ifFalse: [code at: 1]).
		stream nextPut: code last; space].
	stream skip: -1.
	stream nextPut: $>.
	stream space.
	stream nextPutAll: instruction.
	stream newLine.
	oldPC := scanner pc.
	"(InstructionPrinter compiledMethodAt: #print:) symbolic."

</details>

#### InstructionPrinter>>#blockReturnTop

Print the Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	blockReturnTop
	"Print the Return Top Of Stack bytecode."

	self print: 'blockReturn'
</details>

#### InstructionPrinter>>#storeIntoLiteralVariable: anAssociation

Print the Store Top Of Stack Into Literal Variable Of Method bytecode.


<details>
	<summary>See more</summary>
	
	storeIntoLiteralVariable: anAssociation 
	"Print the Store Top Of Stack Into Literal Variable Of Method bytecode."

	self print: 'storeIntoLit: ' , anAssociation key
</details>

#### InstructionPrinter>>#printInstructionsOn: aStream

Append to the stream, aStream, a description of each bytecode in the instruction stream.


<details>
	<summary>See more</summary>
	
	printInstructionsOn: aStream 
	"Append to the stream, aStream, a description of each bytecode in the
	 instruction stream."
	
	| end |
	stream := aStream.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	innerIndents := Array new: end withAll: 0.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self]
</details>

#### InstructionPrinter>>#methodReturnReceiver

Print the Return Self bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnReceiver
	"Print the Return Self bytecode."

	self print: 'returnSelf'
</details>

#### InstructionPrinter>>#pushConstant: obj

Print the Push Constant, obj, on Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushConstant: obj
	"Print the Push Constant, obj, on Top Of Stack bytecode."

	self print: (String streamContents:
				[:s |
				s nextPutAll: 'pushConstant: '.
				(obj isKindOf: LookupKey)
					ifFalse: [obj printOn: s]
					ifTrue: [obj key
						ifNotNil: [s nextPutAll: '##'; nextPutAll: obj key]
						ifNil: [s nextPutAll: '###'; nextPutAll: obj value soleInstance name]]]).

	(obj is: #CompiledMethod) ifTrue:
		[obj longPrintOn: stream indent: self indent + 2.
		^self].
</details>

#### InstructionPrinter>>#popIntoReceiverVariable: offset

Print the Remove Top Of Stack And Store Into Instance Variable bytecode.


<details>
	<summary>See more</summary>
	
	popIntoReceiverVariable: offset 
	"Print the Remove Top Of Stack And Store Into Instance Variable 
	bytecode."

	self print: 'popIntoRcvr: ' , offset printString
</details>

#### InstructionPrinter>>#pushTemporaryVariable: offset

Print the Push Contents Of Temporary Variable Whose Index Is the argument, offset, On Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	pushTemporaryVariable: offset
	"Print the Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."

	self print: 'pushTemp: ' , offset printString
</details>

#### InstructionPrinter>>#printPC

<details>
	<summary>See more</summary>
	
	printPC
	^printPC
</details>

#### InstructionPrinter>>#methodReturnTop

Print the Return Top Of Stack bytecode.


<details>
	<summary>See more</summary>
	
	methodReturnTop
	"Print the Return Top Of Stack bytecode."

	self print: 'returnTop'
</details>

## InstructionStream

My instances can interpret the byte-encoded Smalltalk instruction set. They maintain a program counter (pc) for streaming through CompiledMethods. My subclasses are Contexts, which inherit this capability. They store the return pointer in the instance variable sender, and the current position in their method in the instance variable pc. For other users, sender can hold a method to be similarly interpreted. The unclean re-use of sender to hold the method was to avoid a trivial subclass for the stand-alone scanning function.

### Methods
#### InstructionStream>>#method

Answer the compiled method that supplies the receiver's bytecodes.


<details>
	<summary>See more</summary>
	
	method
	"Answer the compiled method that supplies the receiver's bytecodes."

	^sender		"method access when used alone (not as part of a context)"
</details>

#### InstructionStream>>#unusedBytecode: client at: targetPC

<details>
	<summary>See more</summary>
	
	unusedBytecode: client at: targetPC
	[client unusedBytecode]
		on: MessageNotUnderstood
		do: [:ex|
			(ex receiver == client
			 and: [ex message selector == #unusedBytecode])
				ifTrue: [self error: 'unusedBytecode']
				ifFalse: [ex pass]]
</details>

#### InstructionStream>>#nextPc: currentByte

Answer the pc of the next bytecode following the current one, given the current bytecode..


<details>
	<summary>See more</summary>
	
	nextPc: currentByte
	"Answer the pc of the next bytecode following the current one, given the current bytecode.."

	^pc + (self method encoderClass bytecodeSize: currentByte)
</details>

#### InstructionStream>>#interpretV3Extension: offset in: method for: client

<details>
	<summary>See more</summary>
	
	interpretV3Extension: offset in: method for: client
	| type offset2 byte2 byte3 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	^self unusedBytecode: client at: pc
</details>

#### InstructionStream>>#interpretNextV3ClosuresInstructionFor: client

Send to the argument, client, a message that specifies the type of the next instruction.


<details>
	<summary>See more</summary>
	
	interpretNextV3ClosuresInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushConstant: (self class specialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnConstant: (self class specialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3ClosuresExtension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			send: (Smalltalk specialSelectorAt: offset + 1)
			super: false
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"
</details>

#### InstructionStream>>#secondByte

Answer the second byte of the current bytecode.


<details>
	<summary>See more</summary>
	
	secondByte
	"Answer the second byte of the current bytecode."

	^self method at: pc + 1
</details>

#### InstructionStream>>#willSend

Answer whether the next bytecode is a message-send.


<details>
	<summary>See more</summary>
	
	willSend
	"Answer whether the next bytecode is a message-send."
	| method |
	method := self method.
	^method encoderClass isSendAt: pc in: method
</details>

#### InstructionStream>>#atEnd

<details>
	<summary>See more</summary>
	
	atEnd

	^ pc > self method endPC
</details>

#### InstructionStream>>#selectorToSendOrSelf

If this instruction is a send, answer the selector, otherwise answer self.


<details>
	<summary>See more</summary>
	
	selectorToSendOrSelf
	"If this instruction is a send, answer the selector, otherwise answer self."

	| method |
	method := self method.
	^method encoderClass selectorToSendOrItselfFor: self in: method at: pc
</details>

#### InstructionStream>>#followingPc

Answer the pc of the following bytecode.


<details>
	<summary>See more</summary>
	
	followingPc
	"Answer the pc of the following bytecode."
	| method |
	method := self method.
	^pc + (method encoderClass bytecodeSize: (method at: pc))
</details>

#### InstructionStream>>#peekInstruction

Return the next bytecode instruction as a message that an InstructionClient would understand. The pc remains unchanged.


<details>
	<summary>See more</summary>
	
	peekInstruction
	"Return the next bytecode instruction as a message that an InstructionClient would understand.  The pc remains unchanged."

	| currentPc instr |
	currentPc _ self pc.
	instr _ self nextInstruction.
	self pc: currentPc.
	^ instr
</details>

#### InstructionStream>>#firstByte

Answer the first byte of the current bytecode.


<details>
	<summary>See more</summary>
	
	firstByte
	"Answer the first byte of the current bytecode."

	^self method at: pc
</details>

#### InstructionStream>>#willReallyStore

Answer whether the bytecode at pc is a store or store-pop into an explicit variable. This eliminates stores into indirect temp vectors, which implement mutable closed-over variables in the the closure implementation, and hence stores into temp vectors are not real stores.


<details>
	<summary>See more</summary>
	
	willReallyStore
	"Answer whether the bytecode at pc is a store or store-pop into an explicit variable.
	 This eliminates stores into indirect temp vectors, which implement mutable closed-over
	 variables in the the closure implementation, and hence stores into temp vectors are not real stores."
	| method |
	method := self method.
	^method encoderClass isNonSyntheticStoreAt: pc in: method for: self
</details>

#### InstructionStream>>#debuggerMap

<details>
	<summary>See more</summary>
	
	debuggerMap
	^self method debuggerMap
</details>

#### InstructionStream>>#willJustPop

Answer whether the bytecode at pc is a pop.


<details>
	<summary>See more</summary>
	
	willJustPop
	"Answer whether the bytecode at pc is a pop."
	| method |
	method := self method.
	^method encoderClass isJustPopAt: pc in: method
</details>

#### InstructionStream>>#addSelectorTo: set

If this instruction is a send, add its selector to set.


<details>
	<summary>See more</summary>
	
	addSelectorTo: set 
	"If this instruction is a send, add its selector to set."

	| selectorOrSelf |
	(selectorOrSelf := self selectorToSendOrSelf) == self ifFalse:
		[set add: selectorOrSelf]
</details>

#### InstructionStream>>#interpretExtension: offset in: method for: client

<details>
	<summary>See more</summary>
	
	interpretExtension: offset in: method for: client
	| type offset2 byte2 byte3 byte4 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	byte2 := method at: pc. pc := pc + 1.
	offset = 10 ifTrue:
		[^byte2 < 128
			ifTrue: [client pushNewArrayOfSize: byte2]
			ifFalse: [client pushConsArrayWithElements: byte2 - 128]].
	offset = 11 ifTrue: [^self error: 'unusedBytecode'].
	byte3 := method at: pc.  pc := pc + 1.
	offset = 12 ifTrue: [^client pushRemoteTemp: byte2 inVectorAt: byte3].
	offset = 13 ifTrue: [^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	offset = 14 ifTrue: [^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"offset = 15"
	byte4 := method at: pc.  pc := pc + 1.
	^client
		pushClosureCopyNumCopiedValues: (byte2 bitShift: -4)
		numArgs: (byte2 bitAnd: 16rF)
		blockSize: (byte3 * 256) + byte4
</details>

#### InstructionStream>>#interpretJump

If the instruction at pc is an unconditional jump, interpret it, advancing the pc, and answering the jump distance. Otherwise answer nil.


<details>
	<summary>See more</summary>
	
	interpretJump
	"If the instruction at pc is an unconditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."
	^self method encoderClass interpretJumpIn: self
</details>

#### InstructionStream>>#willJumpIfFalse

Answer whether the next bytecode is a jump-if-false.


<details>
	<summary>See more</summary>
	
	willJumpIfFalse
	"Answer whether the next bytecode is a jump-if-false."
	| method |
	method := self method.
	^method encoderClass isBranchIfFalseAt: pc in: method
</details>

#### InstructionStream>>#willBlockReturn

Answer whether the next bytecode is a return.


<details>
	<summary>See more</summary>
	
	willBlockReturn
	"Answer whether the next bytecode is a return."
	| method |
	method := self method.
	^method encoderClass isBlockReturnAt: pc in: method
</details>

#### InstructionStream>>#abstractPC

<details>
	<summary>See more</summary>
	
	abstractPC
	^self method abstractPCForConcretePC: pc
</details>

#### InstructionStream>>#previousPc

<details>
	<summary>See more</summary>
	
	previousPc

	^self method pcPreviousTo: pc
</details>

#### InstructionStream>>#interpretNextInstructionFor: client

Send to the argument, client, a message that specifies the type of the next instruction.


<details>
	<summary>See more</summary>
	
	interpretNextInstructionFor: client
	"Send to the argument, client, a message that specifies the type of the next instruction."

	^self method encoderClass interpretNextInstructionFor: client in: self
</details>

#### InstructionStream>>#skipCallPrimitive

If the receiver's method starts with a callPrimitive: bytecode, skip it.


<details>
	<summary>See more</summary>
	
	skipCallPrimitive
	"If the receiver's method starts with a callPrimitive: bytecode, skip it."
	| method encoderClass callPrimitiveCode |
	method := self method.
	encoderClass := method  encoderClass.
	callPrimitiveCode := encoderClass callPrimitiveCode.
	(method byteAt: pc) = callPrimitiveCode ifTrue:
		[pc := pc + (encoderClass bytecodeSize: callPrimitiveCode)]
</details>

#### InstructionStream>>#method: method pc: startpc

<details>
	<summary>See more</summary>
	
	method: method pc: startpc

	sender _ method. 
	"allows this class to stand alone as a method scanner"
	pc _ startpc
</details>

#### InstructionStream>>#willJump

Answer whether the next bytecode is an uncoinditional jump.


<details>
	<summary>See more</summary>
	
	willJump
	"Answer whether the next bytecode is an uncoinditional jump."
	| method |
	method := self method.
	^method encoderClass isJumpAt: pc in: method
</details>

#### InstructionStream>>#willReturn

Answer whether the next bytecode is a return.


<details>
	<summary>See more</summary>
	
	willReturn
	"Answer whether the next bytecode is a return."
	| method |
	method := self method.
	^method encoderClass isReturnAt: pc in: method
</details>

#### InstructionStream>>#interpret

<details>
	<summary>See more</summary>
	
	interpret

	[self atEnd] whileFalse: [self interpretNextInstructionFor: self]
</details>

#### InstructionStream>>#fourthByte

Answer the fourth byte of the current bytecode.


<details>
	<summary>See more</summary>
	
	fourthByte
	"Answer the fourth byte of the current bytecode."

	^self method at: pc + 3
</details>

#### InstructionStream>>#interpretV3ClosuresExtension: offset in: method for: client

<details>
	<summary>See more</summary>
	
	interpretV3ClosuresExtension: offset in: method for: client
	| type offset2 byte2 byte3 byte4 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	byte2 := method at: pc. pc := pc + 1.
	offset = 10 ifTrue:
		[^byte2 < 128
			ifTrue: [client pushNewArrayOfSize: byte2]
			ifFalse: [client pushConsArrayWithElements: byte2 - 128]].
	byte3 := method at: pc.  pc := pc + 1.
	offset = 11 ifTrue: [^client callPrimitive: byte2 + (byte3 bitShift: 8)].
	offset = 12 ifTrue: [^client pushRemoteTemp: byte2 inVectorAt: byte3].
	offset = 13 ifTrue: [^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	offset = 14 ifTrue: [^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"offset = 15"
	byte4 := method at: pc.  pc := pc + 1.
	^client
		pushClosureCopyNumCopiedValues: (byte2 bitShift: -4)
		numArgs: (byte2 bitAnd: 16rF)
		blockSize: (byte3 * 256) + byte4
</details>

#### InstructionStream>>#willStore

Answer whether the bytecode at pc is a store or store-pop.


<details>
	<summary>See more</summary>
	
	willStore
	"Answer whether the bytecode at pc is a store or store-pop."
	| method |
	method := self method.
	^method encoderClass isStoreAt: pc in: method
</details>

#### InstructionStream>>#nextByte

Answer the next bytecode.


<details>
	<summary>See more</summary>
	
	nextByte
	"Answer the next bytecode."

	^self method at: pc
</details>

#### InstructionStream>>#nextInstruction

Return the next bytecode instruction as a message that an InstructionClient would understand. This advances the pc by one instruction.


<details>
	<summary>See more</summary>
	
	nextInstruction
	"Return the next bytecode instruction as a message that an InstructionClient would understand.  This advances the pc by one instruction."

	^ self interpretNextInstructionFor: MessageCatcher new
</details>

#### InstructionStream>>#movePcForward

<details>
	<summary>See more</summary>
	
	movePcForward
	
	pc := self followingPc. 	

</details>

#### InstructionStream>>#pc: n

<details>
	<summary>See more</summary>
	
	pc: n

	pc _ n
</details>

#### InstructionStream>>#followingByte

Answer the next bytecode.


<details>
	<summary>See more</summary>
	
	followingByte
	"Answer the next bytecode."

	^self method at: pc + 1
</details>

#### InstructionStream>>#pc

Answer the index of the next bytecode.


<details>
	<summary>See more</summary>
	
	pc
	"Answer the index of the next bytecode."

	^pc
</details>

#### InstructionStream>>#skipBackBeforeJump

Assuming that the receiver is positioned just after a jump, skip back one or two bytes, depending on the size of the previous jump instruction.


<details>
	<summary>See more</summary>
	
	skipBackBeforeJump
	"Assuming that the receiver is positioned just after a jump, skip back one or two bytes,
	 depending on the size of the previous jump instruction."
	| scanner client prevPc |
	scanner := InstructionStream on: self method.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	scanner pc: prevPc.
	(scanner willJumpIfTrue or: [scanner willJumpIfFalse]) ifFalse:
		[self error: 'Where''s the jump??'].
	self jump: prevPc - pc
</details>

#### InstructionStream>>#scanFor: scanBlock

Check all bytecode instructions with scanBlock, answer true if scanBlock answers true. This can be used to, e.g., check whether a method contains 'push closure' bytecodes like this: aMethod scanFor: [ :b | b = 143 ]


<details>
	<summary>See more</summary>
	
	scanFor: scanBlock
	"Check all bytecode instructions with scanBlock, answer true if scanBlock answers true.
	This can be used to, e.g., check whether a method contains 'push closure' bytecodes like this:
	aMethod scanFor: [ :b | b = 143 ]"

	| method encoderClass end byteCode |
	
	method := self method.
	end := method endPC.
	encoderClass := method encoderClass.
	
	[pc <= end] whileTrue: [
		byteCode := method at: pc.
		(scanBlock value: byteCode) ifTrue: [^true].
		pc := pc + (encoderClass bytecodeSize: byteCode)].
	
	^false
</details>

#### InstructionStream>>#interpretJumpIfCond

If the instruction at pc is a conditional jump, interpret it, advancing the pc, and answering the jump distance. Otherwise answer nil.


<details>
	<summary>See more</summary>
	
	interpretJumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."
	^self method encoderClass interpretJumpIfCondIn: self
</details>

#### InstructionStream>>#willStorePop

Answer whether the bytecode at pc is a store-pop.


<details>
	<summary>See more</summary>
	
	willStorePop
	"Answer whether the bytecode at pc is a store-pop."
	| method |
	method := self method.
	^method encoderClass isStorePopAt: pc in: method
</details>

#### InstructionStream>>#willJumpIfTrue

Answer whether the next bytecode is a jump-if-true.


<details>
	<summary>See more</summary>
	
	willJumpIfTrue
	"Answer whether the next bytecode is a jump-if-true."
	| method |
	method := self method.
	^method encoderClass isBranchIfTrueAt: pc in: method
</details>

#### InstructionStream>>#thirdByte

Answer the third byte of the current bytecode.


<details>
	<summary>See more</summary>
	
	thirdByte
	"Answer the third byte of the current bytecode."

	^self method at: pc + 2
</details>

#### InstructionStream>>#willReturnTopFromMethod

Answer whether the next bytecode is a return stack top from method.


<details>
	<summary>See more</summary>
	
	willReturnTopFromMethod
	"Answer whether the next bytecode is a return stack top from method."
	| method |
	method := self method.
	^method encoderClass isReturnTopFromMethodAt: pc in: method
</details>

#### InstructionStream>>#interpretV3JumpIfCond

If the instruction at pc is a conditional jump, interpret it, advancing the pc, and answering the jump distance. Otherwise answer nil.


<details>
	<summary>See more</summary>
	
	interpretV3JumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."

	"152-159 	10011iii 		Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	 168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	 172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
	| byte |
	byte := self method at: pc.
	(byte between: 152 and: 159) ifTrue:
		[pc := pc + 1.
		 ^byte - 151].
	(byte between: 168 and: 175) ifTrue:
		[pc := pc + 2.
		 ^(byte bitAnd: 3) * 256 + (self method at: pc - 1)].
	^nil
</details>

#### InstructionStream>>#followingBytecode

Answer the bytecode of the following bytecode (different to nextByte).


<details>
	<summary>See more</summary>
	
	followingBytecode
	"Answer the bytecode of the following bytecode (different to nextByte)."

	^self method at: self followingPc
</details>

#### InstructionStream>>#interpretV3Jump

If the instruction at pc is an unconditional jump, interpret it, advancing the pc, and answering the target pc. Otherwise answer nil.


<details>
	<summary>See more</summary>
	
	interpretV3Jump
	"If the instruction at pc is an unconditional jump, interpret it, advancing the pc,
	 and answering the target pc. Otherwise answer nil."

	"144-151 	10010iii 		Jump iii + 1 (i.e., 1 through 8)
	 160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	| byte |
	byte := self method at: pc.
	(byte between: 144 and: 151) ifTrue:
		[pc := pc + 1.
		 ^byte - 143].
	(byte between: 160 and: 167) ifTrue:
		[pc := pc + 2.
		 ^(byte - 164) * 256 + (self method at: pc - 1)].
	^nil
</details>

## Message

I represent a selector and its argument values. Generally, the system does not use instances of Message for efficiency reasons. However, when a message is not understood by its receiver, the interpreter will make up an instance of me in order to capture the information involved in an actual message transmission. This instance is sent it as an argument with the message doesNotUnderstand: to the receiver.

### Methods
#### Message>>#createStubMethodFor: aClass

<details>
	<summary>See more</summary>
	
	createStubMethodFor: aClass

	^ String streamContents: [ :stream |
		self writeMessageNameOn: stream.
		stream newLine; tab.
		self writeShouldBeImplementedOn: stream.
		(self isGetterFor: aClass) ifTrue: [ self addGetterCodeOn: stream ].
		(self isSetterFor: aClass) ifTrue: [ self addSetterCodeOn: stream ].
	]
</details>

#### Message>>#argumentNameAt: anIndex havingNamed: alreadyNamedArguments

<details>
	<summary>See more</summary>
	
	argumentNameAt: anIndex havingNamed: alreadyNamedArguments

	| argumentName |
	
	argumentName _ (self arguments at: anIndex) argumentName.
	[alreadyNamedArguments includes: argumentName] whileTrue: [argumentName _ argumentName, anIndex asString].
	alreadyNamedArguments add: argumentName.

	^argumentName
</details>

#### Message>>#arguments

Answer the arguments of the receiver.


<details>
	<summary>See more</summary>
	
	arguments
	"Answer the arguments of the receiver."

	^args
</details>

#### Message>>#selector

Answer the selector of the receiver.


<details>
	<summary>See more</summary>
	
	selector
	"Answer the selector of the receiver."

	^selector
</details>

#### Message>>#writeMessageNameOn: aStream

<details>
	<summary>See more</summary>
	
	writeMessageNameOn: aStream

	| alreadyNamedArguments |

	alreadyNamedArguments _ Set new.
	self selector keywords withIndexDo: [ :keyword :index |
		aStream nextPutAll: keyword.
		self hasArguments ifTrue: [ self writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments ]].
	
	
</details>

#### Message>>#numArgs

Answer the number of arguments in this message


<details>
	<summary>See more</summary>
	
	numArgs
	"Answer the number of arguments in this message"

	^args size
</details>

#### Message>>#sentTo: receiver

answer the result of sending this message to receiver


<details>
	<summary>See more</summary>
	
	sentTo: receiver
	"answer the result of sending this message to receiver"

	^lookupClass
		ifNil: [receiver perform: selector withArguments: args]
		ifNotNil: [receiver perform: selector withArguments: args inSuperclass: lookupClass]
</details>

#### Message>>#isSetterFor: aClass

<details>
	<summary>See more</summary>
	
	isSetterFor: aClass

	^selector isKeyword and: [ self numArgs = 1 and: [ aClass instVarNames includes: selector allButLast ]]
</details>

#### Message>>#= aMessage

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aMessage

	"Any object is equal to itself"
	self == aMessage ifTrue: [ ^ true ].

	self class == aMessage class ifFalse: [ ^false ].
	selector = aMessage selector ifFalse: [ ^false ].
	lookupClass = aMessage lookupClass ifFalse: [ ^false ].
	^args literalEqual: aMessage arguments
</details>

#### Message>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."
	^selector hash bitXor: args hash
</details>

#### Message>>#printOn: stream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: stream

	args isEmpty ifTrue: [^ stream nextPutAll: selector].
	args with: selector keywords do: [:arg :word |
		stream nextPutAll: word.
		stream nextPutAll: ' ('.
		arg printOn: stream.
		stream nextPutAll: ') '.
	].
	stream skip: -1.

</details>

#### Message>>#writeShouldBeImplementedOn: stream

<details>
	<summary>See more</summary>
	
	writeShouldBeImplementedOn: stream.

	stream				
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.
</details>

#### Message>>#hasArguments

<details>
	<summary>See more</summary>
	
	hasArguments
	^args size > 0
</details>

#### Message>>#lookupClass: aClass

<details>
	<summary>See more</summary>
	
	lookupClass: aClass

	lookupClass _ aClass
</details>

#### Message>>#sendTo: receiver

answer the result of sending this message to receiver


<details>
	<summary>See more</summary>
	
	sendTo: receiver
	"answer the result of sending this message to receiver"

	^ receiver perform: selector withArguments: args
</details>

#### Message>>#pushReceiver

<details>
	<summary>See more</summary>
	
	pushReceiver
</details>

#### Message>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	aStream nextPut: $(;
	 nextPutAll: self class name;
	 nextPutAll: ' selector: ';
	 store: selector;
	 nextPutAll: ' arguments: ';
	 store: args;
	 nextPut: $)
</details>

#### Message>>#setSelector: aSymbol

<details>
	<summary>See more</summary>
	
	setSelector: aSymbol

	selector _ aSymbol.

</details>

#### Message>>#writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments

<details>
	<summary>See more</summary>
	
	writeOn: aStream argumentNameAt: index havingNamed: alreadyNamedArguments

	| argumentName |
	
	argumentName _ self argumentNameAt: index havingNamed: alreadyNamedArguments.
	
	aStream 
		nextPutAll: ' '; 
		nextPutAll: argumentName; 
		space
	
	
</details>

#### Message>>#addSetterCodeOn: stream

<details>
	<summary>See more</summary>
	
	addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: (Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
			ifTrue: [ ' _ ' ]
			ifFalse: [ ' := ' ]);
		nextPutAll: self arguments first argumentName 
</details>

#### Message>>#addGetterCodeOn: stream

<details>
	<summary>See more</summary>
	
	addGetterCodeOn: stream

	stream
		newLine; tab;
		nextPut: $^;
		nextPutAll: selector 
</details>

#### Message>>#lookupClass

<details>
	<summary>See more</summary>
	
	lookupClass

	^ lookupClass
</details>

#### Message>>#argument

Answer the first (presumably sole) argument


<details>
	<summary>See more</summary>
	
	argument
	"Answer the first (presumably sole) argument"

	^args at: 1
</details>

#### Message>>#analogousCodeTo: anObject

For MethodPropertires comparison.


<details>
	<summary>See more</summary>
	
	analogousCodeTo: anObject
	"For MethodPropertires comparison."
	^self class == anObject class
	  and: [selector == anObject selector
	  and: [args = anObject arguments
	  and: [lookupClass == anObject lookupClass]]]
</details>

#### Message>>#setSelector: aSymbol arguments: anArray

<details>
	<summary>See more</summary>
	
	setSelector: aSymbol arguments: anArray

	selector _ aSymbol.
	args _ anArray
</details>

#### Message>>#isGetterFor: aClass

<details>
	<summary>See more</summary>
	
	isGetterFor: aClass

	^selector isUnary and: [ aClass instVarNames includes: selector  ]
</details>

#### Message>>#sends: aSelector

answer whether this message's selector is aSelector


<details>
	<summary>See more</summary>
	
	sends: aSelector
	"answer whether this message's selector is aSelector"

	^selector == aSelector
</details>

#### Message>>#fullName

<details>
	<summary>See more</summary>
	
	fullName

	^ String streamContents: [ :messageStream |
		args
			ifEmpty: [ messageStream nextPutAll: selector ]
			ifNotEmpty: [
				self selector keywords withIndexDo: [ :keyword :index |
					messageStream
						nextPutAll: keyword;
						nextPut:  Character space;
						nextPutAll: (args at: index).
					"add an space unless it's the last keyword"
					index = self selector keywords size ifFalse: [ messageStream nextPut: Character space ]
				]
			].
		]
</details>

## MessageCatcher

Any message sent to me is returned as a Message object. "Message catcher" creates an instance of me.

### Methods
#### MessageCatcher>>#doesNotUnderstand: aMessage

<details>
	<summary>See more</summary>
	
	doesNotUnderstand: aMessage

	accumulator ifNotNil: [accumulator add: aMessage].
	^ aMessage
</details>

#### MessageCatcher>>#privAccumulator: collection

<details>
	<summary>See more</summary>
	
	privAccumulator: collection

	accumulator := collection
</details>

#### MessageCatcher>>#privAccumulator

<details>
	<summary>See more</summary>
	
	privAccumulator

	^ accumulator
</details>

## MethodContext

My instances hold all the dynamic state associated with the execution of either a method activation resulting from a message send or a block activation resulting from a block evaluation. In addition to their inherited state, this includes the receiver (self), the closure for a BlockClosure activation (which is nil for a method activation), a CompiledMethod, and space in the variable part of the context for arguments and temporary variables. MethodContexts, though normal in their variable size, are actually only used in two sizes, small and large, which are determined by the temporary space required by the method being executed. MethodContexts must only be created using the method newForMethod:. Note that it is impossible to determine the real object size of a MethodContext except by asking for the frameSize of its method. Any fields above the stack pointer (stackp) are truly invisible -- even (and especially!) to the garbage collector. Any store into stackp other than by the primitive method stackp: is potentially fatal.

### Methods
#### MethodContext>>#instVarAt: index put: value

Primitive. Store a value into a fixed variable in an object. The numbering of the variables corresponds to the named instance variables, followed by the indexed instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	instVarAt: index put: value
	index = 3 ifTrue: [self stackp: value. ^ value].
	^ super instVarAt: index put: value
</details>

#### MethodContext>>#setSender: s receiver: r method: m closure: c startpc: startpc

Create the receiver's initial state.


<details>
	<summary>See more</summary>
	
	setSender: s receiver: r method: m closure: c startpc: startpc
	"Create the receiver's initial state."

	sender := s.
	receiver := r.
	method := m.
	closureOrNil := c.
	pc := startpc.
	stackp := 0
</details>

#### MethodContext>>#activeHome

If executing closure, search senders for the activation of the original (outermost) method that (indirectly) created my closure (the closureHome). If the closureHome is not found on the sender chain answer nil.


<details>
	<summary>See more</summary>
	
	activeHome
	"If executing closure, search senders for the activation of the original
	 (outermost) method that (indirectly) created my closure (the closureHome).
	 If the closureHome is not found on the sender chain answer nil."

	| methodReturnContext |
	self isExecutingBlock ifFalse: [^self].
	self sender ifNil: [^nil].
	methodReturnContext := self methodReturnContext.
	^self sender findContextSuchThat: [:ctxt | ctxt = methodReturnContext]
</details>

#### MethodContext>>#method

Answer the method of this context.


<details>
	<summary>See more</summary>
	
	method

	^method
</details>

#### MethodContext>>#pushConsArrayWithElements: numElements

<details>
	<summary>See more</summary>
	
	pushConsArrayWithElements: numElements 
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: self pop].
	self push: array
</details>

#### MethodContext>>#startpc

<details>
	<summary>See more</summary>
	
	startpc
	^closureOrNil
		ifNil:	[self method initialPC]
		ifNotNil: [closureOrNil startpc]
</details>

#### MethodContext>>#hasNonLocalReturn

<details>
	<summary>See more</summary>
	
	hasNonLocalReturn
	^closureOrNil hasNonLocalReturn
</details>

#### MethodContext>>#swapReceiver: r

<details>
	<summary>See more</summary>
	
	swapReceiver: r

	receiver := r
</details>

#### MethodContext>>#atEnd

<details>
	<summary>See more</summary>
	
	atEnd
	^ self isExecutingBlock
		ifTrue: [ self closure startpc + self closure size - 1 = self pc ]
		ifFalse: [ self pc >= self method endPC ]
</details>

#### MethodContext>>#isHandlerOrSignalingContext

Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) are marked with primitive 199.


<details>
	<summary>See more</summary>
	
	isHandlerOrSignalingContext
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^method primitive = 199
</details>

#### MethodContext>>#receiver

Refer to the comment in ContextPart|receiver.


<details>
	<summary>See more</summary>
	
	receiver 
	"Refer to the comment in ContextPart|receiver."

	^receiver
</details>

#### MethodContext>>#isExecutingBlock

Is this executing a block versus a method? In the new closure implemetation this is true if closureOrNil is not nil, in which case it should be holding a BlockClosure.


<details>
	<summary>See more</summary>
	
	isExecutingBlock
	"Is this executing a block versus a method?  In the new closure
	 implemetation this is true if closureOrNil is not nil, in which case
	 it should be holding a BlockClosure."

	^closureOrNil isClosure
</details>

#### MethodContext>>#setSender: s receiver: r method: m arguments: args

Create the receiver's initial state.


<details>
	<summary>See more</summary>
	
	setSender: s receiver: r method: m arguments: args 
	"Create the receiver's initial state."

	sender := s.
	receiver := r.
	method := m.
	closureOrNil := nil.
	pc := method initialPC.
	self stackp: method numTemps.
	1 to: args size do: [:i | self at: i put: (args at: i)]
</details>

#### MethodContext>>#closure

<details>
	<summary>See more</summary>
	
	closure
	^closureOrNil
</details>

#### MethodContext>>#contextForLocalVariables

Answer the context in which local variables (temporaries) are stored.


<details>
	<summary>See more</summary>
	
	contextForLocalVariables
	"Answer the context in which local variables (temporaries) are stored."

	^self
</details>

#### MethodContext>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self outerContext
		ifNil: [super printOn: aStream]
		ifNotNil:
			[:outerContext|
			 aStream nextPutAll: '[] in '.
			 outerContext printOn: aStream]
</details>

#### MethodContext>>#privRefreshWith: aCompiledMethod

Reinitialize the receiver as though it had been for a different method. Used by a Debugger when one of the methods to which it refers is recompiled.


<details>
	<summary>See more</summary>
	
	privRefreshWith: aCompiledMethod 
	"Reinitialize the receiver as though it had been for a different method. 
	 Used by a Debugger when one of the methods to which it refers is 
	 recompiled."

	(aCompiledMethod is: #CompiledMethod) ifFalse: [
		self error: 'method can only be set to aCompiledMethod'].
	method := aCompiledMethod.
	self assert: closureOrNil == nil.
	"was: receiverMap := nil."
	self privRefresh
</details>

#### MethodContext>>#tempAt: index

Answer the value of the temporary variable whose index is the argument, index. Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default at: primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	tempAt: index 
	"Answer the value of the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at: primitive to give latitude to the
	 VM in context management."

	<primitive: 210>
	^self at: index
</details>

#### MethodContext>>#tempAt: index put: value

Store the argument, value, as the temporary variable whose index is the argument, index. Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive. Override the default at:put: primitive to give latitude to the VM in context management.


<details>
	<summary>See more</summary>
	
	tempAt: index put: value 
	"Store the argument, value, as the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at:put: primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	^self at: index put: value
</details>

#### MethodContext>>#aboutToReturn: result through: firstUnwindContext

Called from VM when an unwindBlock is found between self and its home. Return to home's sender, executing unwind blocks on the way.


<details>
	<summary>See more</summary>
	
	aboutToReturn: result through: firstUnwindContext 
	"Called from VM when an unwindBlock is found between self and its home.
	 Return to home's sender, executing unwind blocks on the way."

	self methodReturnContext return: result through: firstUnwindContext
</details>

#### MethodContext>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ ContextInspector
</details>

#### MethodContext>>#failPrimitiveWith: maybePrimFailToken

The receiver is a freshly-created context on a primitive method. Skip the callPrimitive: bytecode and store the primitive fail code if there is one and the method consumes it.


<details>
	<summary>See more</summary>
	
	failPrimitiveWith: maybePrimFailToken
	"The receiver is a freshly-created context on a primitive method.  Skip the callPrimitive:
	 bytecode and store the primitive fail code if there is one and the method consumes it."
	self skipCallPrimitive.
	((self isPrimFailToken: maybePrimFailToken)
	  and: [method encoderClass isStoreAt: pc in: method]) ifTrue:
		[self at: stackp put: maybePrimFailToken last]
</details>

#### MethodContext>>#removeSelf

Nil the receiver pointer and answer its former value.


<details>
	<summary>See more</summary>
	
	removeSelf
	"Nil the receiver pointer and answer its former value."

	| tempSelf |
	tempSelf _ receiver.
	receiver _ nil.
	^tempSelf
</details>

#### MethodContext>>#restartWithNewReceiver: obj

<details>
	<summary>See more</summary>
	
	restartWithNewReceiver: obj

	self
		swapReceiver: obj;
		restart
</details>

#### MethodContext>>#privRefresh

Reinitialize the receiver so that it is in the state it was at its creation.


<details>
	<summary>See more</summary>
	
	privRefresh
	"Reinitialize the receiver so that it is in the state it was at its creation."

	closureOrNil
		ifNotNil:
			[pc := closureOrNil startpc.
			self stackp: closureOrNil numArgs + closureOrNil numCopiedValues.
			1 to: closureOrNil numCopiedValues do:
				[:i | self tempAt: closureOrNil numArgs + i put: (closureOrNil at: i)]]
		ifNil:
			[pc := method initialPC.
			self stackp: method numTemps.
			method numArgs+1 to: method numTemps do:
				[:i | self tempAt: i put: nil]]
</details>

#### MethodContext>>#pushArgs: args "<Array>" from: sendr

<Array>


<details>
	<summary>See more</summary>
	
	pushArgs: args "<Array>" from: sendr "<ContextPart>" 
	"Helps simulate action of the value primitive for closures.
	 This is used by ContextPart>>runSimulated:contextAtEachStep:"

	closureOrNil
		ifNil: [self error: 'context needs a closure!']
		ifNotNil:
			["See BlockClosure>>asContextWithSender:"
			 stackp ~= (closureOrNil numArgs + closureOrNil numCopiedValues) ifTrue:
				[self error: 'stack pointer is incorrect!'].].

	1 to: closureOrNil numArgs do:
		[:i| self at: i put: (args at: i)].
	sender := sendr
</details>

#### MethodContext>>#printDetails: strm

Put my class>>selector and instance variables and arguments and temporaries on the stream. Protect against errors during printing.


<details>
	<summary>See more</summary>
	
	printDetails: strm
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe _ '<<error during printing>>'.
	strm nextPutAll: ([receiver printStringLimitedTo: 90] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str _ [(self tempsAndValuesLimitedTo: 80 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos _ strm position.
	[receiver longPrintOn: strm limitedTo: 80 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver printStringLimitedTo: 90] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].
</details>

#### MethodContext>>#methodReturnContext

Answer the context from which an ^-return should return from.


<details>
	<summary>See more</summary>
	
	methodReturnContext
	"Answer the context from which an ^-return should return from."

	closureOrNil
		ifNil: [^ self].
	^closureOrNil outerContext methodReturnContext
</details>

#### MethodContext>>#contextTag

Context tags may be used for referring to contexts instead of contexts themselves as they can be copied and will continue to work in other processes (continuations). By default, we use the context itself to as its tag.


<details>
	<summary>See more</summary>
	
	contextTag
	"Context tags may be used for referring to contexts instead of contexts themselves as they can be copied and will continue to work in other processes (continuations). By default, we use the context itself to as its tag."
	^self
</details>

#### MethodContext>>#cannotReturn: result

<details>
	<summary>See more</summary>
	
	cannotReturn: result
	closureOrNil ifNotNil: [
		^self cannotReturn: result to: sender].
	Debugger
		openContext: thisContext
		label: 'computation has been terminated'
		contents: nil
</details>

#### MethodContext>>#home

Answer the context in which the receiver was defined.


<details>
	<summary>See more</summary>
	
	home 
	"Answer the context in which the receiver was defined."

	closureOrNil
		ifNil: [^ self].
	^closureOrNil outerContext home
</details>

#### MethodContext>>#outerContextsDo: aBlock

Answer the context in which the receiver was defined.


<details>
	<summary>See more</summary>
	
	outerContextsDo: aBlock 
	"Answer the context in which the receiver was defined."

	aBlock value: self.
	closureOrNil
		ifNotNil: [ closureOrNil outerContextsDo: aBlock ].
</details>

#### MethodContext>>#isUnwindContext

is this context for method that is marked?


<details>
	<summary>See more</summary>
	
	isUnwindContext
"is this context for  method that is marked?"
	^method primitive = 198
</details>

#### MethodContext>>#blockReturnTop

Simulate the interpreter's action when a ReturnTopOfStackToCaller bytecode is encountered in the receiver. This should only happen in a closure activation.


<details>
	<summary>See more</summary>
	
	blockReturnTop
	"Simulate the interpreter's action when a ReturnTopOfStackToCaller bytecode is 
	 encountered in the receiver.  This should only happen in a closure activation."
	self assert: closureOrNil isClosure.
	^self return: self pop from: self
</details>

#### MethodContext>>#activeOuterContext

If executing closure, search senders for the activation in which the receiver's closure was created (the receiver's outerContext). If the outerContext is not found on the sender chain answer nil.


<details>
	<summary>See more</summary>
	
	activeOuterContext
	"If executing closure, search senders for the activation in which the receiver's
	 closure was created (the receiver's outerContext).  If the outerContext is not
	 found on the sender chain answer nil."

	| outerContext |
	self isExecutingBlock ifFalse: [^self].
	self sender ifNil: [^nil].
	outerContext := self outerContext.
	^self sender findContextSuchThat: [:ctxt | ctxt = outerContext]
</details>

#### MethodContext>>#receiver: r

<details>
	<summary>See more</summary>
	
	receiver: r

	receiver := r
</details>

#### MethodContext>>#outerContext

Answer the context within which the receiver is nested.


<details>
	<summary>See more</summary>
	
	outerContext
	"Answer the context within which the receiver is nested."

	^ closureOrNil
		ifNotNil: [closureOrNil outerContext]
</details>

#### MethodContext>>#printString

Answer an emphasized string in case of a breakpoint method


<details>
	<summary>See more</summary>
	
	printString
	"Answer an emphasized string in case of a breakpoint method"
	(self method notNil and: [ self method hasBreakpoint ])
		ifFalse: [ ^ super printString ].
	^ super printString , ' [break]' asText allBold
</details>

#### MethodContext>>#asContext

<details>
	<summary>See more</summary>
	
	asContext

	^ self
</details>

## MethodDictionary

I am a special dictionary holding methods. I am just like a normal Dictionary, except that I am implemented differently. Each Class has an instance of MethodDictionary to hold the correspondence between selectors (names of methods) and methods themselves. In a normal Dictionary, the instance variable 'array' holds an array of Associations. Since there are thousands of methods in the system, these Associations waste space. Each MethodDictionary is a variable object, with the list of keys (selector Symbols) in the variable part of the instance. The variable 'array' holds the values, which are CompiledMethods. I also maintain the following invariant: (self basicAt: index) isNil = (array at: index) isNil.

### Methods
#### MethodDictionary>>#grow

Grow the elements array and reinsert the old elements


<details>
	<summary>See more</summary>
	
	grow 
	| newSelf |
	newSelf _ self species new: self basicSize.  "This will double the size"
	1 to: self basicSize do: [ :i |
		(self basicAt: i)
			ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self become: newSelf
</details>

#### MethodDictionary>>#rehash

Smalltalk rehash.


<details>
	<summary>See more</summary>
	
	rehash 
	| newSelf |
	newSelf _ self species new: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	self become: newSelf
</details>

#### MethodDictionary>>#removeAll

This provides a faster way than repeated become. a single become is still in use to prevent system crash.


<details>
	<summary>See more</summary>
	
	removeAll
	"This provides a faster way than repeated become.
	a single become is still in use to prevent system crash."
	
	| newSelf |
	tally = 0 ifTrue: [^self].
	newSelf := self species new: self size.  "This will preserve the capacity"
	self become: newSelf
</details>

#### MethodDictionary>>#at: key ifAbsent: aBlock

Answer the value associated with the key or, if key isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	at: key ifAbsent: aBlock

	| index |
	index _ self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	^ array at: index
</details>

#### MethodDictionary>>#keyAt: index

May be overridden by subclasses so that fixCollisionsFrom: will work


<details>
	<summary>See more</summary>
	
	keyAt: index

	^ self basicAt: index
</details>

#### MethodDictionary>>#at: key put: value

Set the value at key to be value.


<details>
	<summary>See more</summary>
	
	at: key put: value
	"Set the value at key to be value."
	| index |
	index _ self findElementOrNil: key.
	(self basicAt: index)
		ifNil: [ 
			tally _ tally + 1.
			self basicAt: index put: key]
		ifNotNil: [
			(array at: index) flushCache].
	array at: index put: value.
	self fullCheck.
	^ value
</details>

#### MethodDictionary>>#keysDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	keysDo: aBlock

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key |
			aBlock value: key]]
</details>

#### MethodDictionary>>#add: anAssociation

Quite like doing self at: anAssociation key put: anAssociation value but making sure the argument is stored. This method should be used when the argument is stored elsewhere, and its value should be kept in sync with the value stored in me. If the key already exists, and it is desired to keep the existing association, then call #at:put:


<details>
	<summary>See more</summary>
	
	add: anAssociation
	^ self at: anAssociation key put: anAssociation value
</details>

#### MethodDictionary>>#valuesDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	valuesDo: aBlock

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do: [ :i |
		(array at: i) ifNotNil: [ :value |
			aBlock value: value]]
</details>

#### MethodDictionary>>#removeDangerouslyKey: key ifAbsent: aBlock

This is not really dangerous. But if normal removal were done WHILE a MethodDict were being used, the system might crash. So instead we make a copy, then do this operation (which is NOT dangerous in a copy that is not being used), and then use the copy after the removal.


<details>
	<summary>See more</summary>
	
	removeDangerouslyKey: key ifAbsent: aBlock
	"This is not really dangerous.  But if normal removal
	were done WHILE a MethodDict were being used, the
	system might crash.  So instead we make a copy, then do
	this operation (which is NOT dangerous in a copy that is
	not being used), and then use the copy after the removal."

	| index element |
	index _ self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	element _ array at: index.
	array at: index put: nil.
	self basicAt: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^ element
</details>

#### MethodDictionary>>#postCopy

Must copy the associations, or later store will affect both the original and the copy


<details>
	<summary>See more</summary>
	
	postCopy

	array _ array copy
</details>

#### MethodDictionary>>#removeKey: key ifAbsent: errorBlock

The interpreter might be using this MethodDict while this method is running! Therefore we perform the removal in a copy, and then atomically become that copy


<details>
	<summary>See more</summary>
	
	removeKey: key ifAbsent: errorBlock 
	"The interpreter might be using this MethodDict while
	this method is running!  Therefore we perform the removal
	in a copy, and then atomically become that copy"
	| copy |
	copy _ self copy.
	copy removeDangerouslyKey: key ifAbsent: [^ errorBlock value].
	self become: copy
</details>

#### MethodDictionary>>#rehashWithoutBecome

<details>
	<summary>See more</summary>
	
	rehashWithoutBecome
	| newSelf |
	newSelf _ self species new: self size.
	1 to: self basicSize do: [ :i |
		(self basicAt: i) ifNotNil: [ :key |
			newSelf at: key put: (array at: i)]].
	^newSelf
</details>

#### MethodDictionary>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject identityHash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### MethodDictionary>>#keyAtIdentityValue: value ifAbsent: exceptionBlock

Answer the key whose value equals the argument, value. If there is none, answer the result of evaluating exceptionBlock.


<details>
	<summary>See more</summary>
	
	keyAtIdentityValue: value ifAbsent: exceptionBlock
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."

	1 to: self basicSize do:
		[:index |
		value == (array at: index)
			ifTrue: [
				(self basicAt: index)
					ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
</details>

#### MethodDictionary>>#associationsDo: aBlock

Evaluate aBlock for each of the receiver's elements (key/value associations).


<details>
	<summary>See more</summary>
	
	associationsDo: aBlock 

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do: [ :i | 
		(self basicAt: i) ifNotNil: [ :key |
			aBlock value: (
				Association key: key value: (array at: i))]]
</details>

#### MethodDictionary>>#valuesSelect: aCondition

<details>
	<summary>See more</summary>
	
	valuesSelect: aCondition

	| selected |

	selected := OrderedCollection new.
	self valuesDo: [ :aValue | (aCondition value: aValue) ifTrue: [ selected add: aValue ]].

	^selected
</details>

#### MethodDictionary>>#fixCollisionsFrom: start

The element at start has been removed and replaced by nil. This method moves forward from there, relocating any entries that had been placed below due to collisions with this one.


<details>
	<summary>See more</summary>
	
	fixCollisionsFrom: start
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| key index |
	index := start.
	[ (key := self basicAt: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self scanFor: key) = index ifFalse: [
			self swap: index with: newIndex ] ]
</details>

#### MethodDictionary>>#associationAt: key ifAbsent: aBlock

Answer the association with the given key. If key is not found, return the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^(array at: (self scanFor: key)) 
		ifNil: [ aBlock value ]
		ifNotNil: [ :value | key -> value ]
</details>

#### MethodDictionary>>#swap: oneIndex with: otherIndex

May be overridden by subclasses so that fixCollisionsFrom: will work


<details>
	<summary>See more</summary>
	
	swap: oneIndex with: otherIndex
	| element |
	element _ self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	super swap: oneIndex with: otherIndex.

</details>

#### MethodDictionary>>#keyAtValue: value ifAbsent: exceptionBlock

Answer the key whose value equals the argument, value. If there is none, answer the result of evaluating exceptionBlock.


<details>
	<summary>See more</summary>
	
	keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."

	1 to: self basicSize do:
		[:index |
		value = (array at: index)
			ifTrue: [
				(self basicAt: index)
					ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
</details>

#### MethodDictionary>>#keysAndValuesDo: aBlock

Enumerate the receiver with all the keys and values passed to the block


<details>
	<summary>See more</summary>
	
	keysAndValuesDo: aBlock 
	"Enumerate the receiver with all the keys and values passed to the block"

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key |
			aBlock value: key value: (array at: i)]
		]
</details>

#### MethodDictionary>>#methodArray

<details>
	<summary>See more</summary>
	
	methodArray
	^ array
</details>

## Pragma

I represent an occurrence of a pragma in a compiled method. A pragma is a literal message pattern that occurs between angle brackets at the start of a method after any temporaries. A common example is the primitive pragma: <primitive: 123 errorCode: 'errorCode'> but one can add one's own and use them as metadata attached to a method. Because pragmas are messages one can browse senders and implementors and perform them. One can query a method for its pragmas by sending it the pragmas message, which answers an Array of instances of me, one for each pragma in the method. I can provide information about the defining class, method, its selector, as well as the information about the pragma keyword and its arguments. See the two 'accessing' protocols for details. 'accessing-method' provides information about the method the pragma is found in, while 'accessing-pragma' is about the pragma itself. Instances are retrieved using one of the pragma search methods of the 'finding' protocol on the class side. To browse all methods with pragmas in the system evaluate SystemNavigation default browseAllSelect: [:m| m pragmas notEmpty] and to browse all nonprimitive methods with pragmas evaluate SystemNavigation default browseAllSelect: [:m| m primitive isZero and: [m pragmas notEmpty]]

### Methods
#### Pragma>>#method

Answer the compiled-method containing the pragma.


<details>
	<summary>See more</summary>
	
	method
	"Answer the compiled-method containing the pragma."
	
	^ method
</details>

#### Pragma>>#message

Answer the message of the receiving pragma.


<details>
	<summary>See more</summary>
	
	message
	"Answer the message of the receiving pragma."
	
	^ Message selector: self keyword arguments: self arguments. 
</details>

#### Pragma>>#arguments

Answer the arguments of the receiving pragma. For a pragma defined as <key1: val1 key2: val2> this will answer #(val1 val2).


<details>
	<summary>See more</summary>
	
	arguments
	"Answer the arguments of the receiving pragma. For a pragma defined as <key1: val1 key2: val2> this will answer #(val1 val2)."
	
	^ arguments
</details>

#### Pragma>>#hasLiteralSuchThat: aBlock

Answer true if litBlock returns true for any literal in the receiver, even if embedded in further array structure. This method is only intended for private use by CompiledMethod hasLiteralSuchThat:


<details>
	<summary>See more</summary>
	
	hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in the receiver, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	^(aBlock value: keyword)
	   or: [arguments hasLiteralSuchThat: aBlock]
</details>

#### Pragma>>#argumentAt: anInteger

Answer one of the arguments of the pragma.


<details>
	<summary>See more</summary>
	
	argumentAt: anInteger
	"Answer one of the arguments of the pragma."
	
	^ self arguments at: anInteger.
</details>

#### Pragma>>#selector

Answer the selector of the method containing the pragma. Do not confuse this with the selector of the pragma's message pattern.


<details>
	<summary>See more</summary>
	
	selector
	"Answer the selector of the method containing the pragma.
	 Do not confuse this with the selector of the pragma's message pattern."
	
	^method selector
</details>

#### Pragma>>#methodClass

Answer the class of the method containing the pragma.


<details>
	<summary>See more</summary>
	
	methodClass
	"Answer the class of the method containing the pragma."
	
	^ method methodClass
</details>

#### Pragma>>#numArgs

Answer the number of arguments in the pragma.


<details>
	<summary>See more</summary>
	
	numArgs
	"Answer the number of arguments in the pragma."

	^ self arguments size.
</details>

#### Pragma>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Pragma or: [ super is: aSymbol ]
</details>

#### Pragma>>#hasLiteral: aLiteral

<details>
	<summary>See more</summary>
	
	hasLiteral: aLiteral
	^keyword == aLiteral 
	   or: [arguments hasLiteral: aLiteral]
</details>

#### Pragma>>#= anObject

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= anObject 
	^self class == anObject class
	  and: [keyword == anObject keyword
	  and: [arguments = anObject arguments]]
</details>

#### Pragma>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^keyword hash + arguments hash
</details>

#### Pragma>>#analogousCodeTo: anObject

<details>
	<summary>See more</summary>
	
	analogousCodeTo: anObject 
	^self class == anObject class
	  and: [keyword == anObject keyword
	  and: [arguments = anObject arguments]]
</details>

#### Pragma>>#setKeyword: aSymbol

<details>
	<summary>See more</summary>
	
	setKeyword: aSymbol
	keyword := aSymbol
</details>

#### Pragma>>#setArguments: anArray

<details>
	<summary>See more</summary>
	
	setArguments: anArray
	arguments := anArray
</details>

#### Pragma>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream nextPut: $<.
	self keyword precedence = 1
		ifTrue: [ aStream nextPutAll: self keyword ]
		ifFalse: [
			self keyword keywords with: self arguments do: [ :key :arg |
				aStream nextPutAll: key; space; print: arg; space ].
			aStream skip: -1 ].
	aStream nextPut: $>.
</details>

#### Pragma>>#setMethod: aCompiledMethod

<details>
	<summary>See more</summary>
	
	setMethod: aCompiledMethod
	method := aCompiledMethod
</details>

#### Pragma>>#keyword

Answer the keyword of the pragma (the selector of its message pattern). For a pragma defined as <key1: val1 key2: val2> this will answer #key1:key2:.


<details>
	<summary>See more</summary>
	
	keyword
	"Answer the keyword of the pragma (the selector of its message pattern).
	 For a pragma defined as <key1: val1 key2: val2> this will answer #key1:key2:."
	
	^ keyword
</details>

#### Pragma>>#key

Answer the keyword of the pragma (the selector of its message pattern). This accessor provides polymorphism with Associations used for properties.


<details>
	<summary>See more</summary>
	
	key
	"Answer the keyword of the pragma (the selector of its message pattern).
	 This accessor provides polymorphism with Associations used for properties."
	^keyword
</details>

## RelativeInstructionPrinter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### RelativeInstructionPrinter>>#print: instruction

Append to the receiver a description of the bytecode, instruction.


<details>
	<summary>See more</summary>
	
	print: instruction 
	"Append to the receiver a description of the bytecode, instruction." 

	| code |
	stream tab: self indent.
	labelling
		ifTrue: [stream print: oldPC - method initialPC; space]
		ifFalse: [stream tab].
	stream tab: (innerIndents at: oldPC).
	self printCode ifTrue: [
		stream nextPut: $<.
		 oldPC to: scanner pc - 1 do: [ :i |
			code := (method at: i) printStringBase: 16.
			stream
				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);
				nextPut: code last;
				space].
		 stream skip: -1; nextPut: $>; space].
	stream nextPutAll: instruction.
	stream newLine.
	labelling ifFalse: [
		(labels at: scanner pc + 1) ~~ false ifTrue:
			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].
	oldPC := scanner pc
</details>

#### RelativeInstructionPrinter>>#jump: offset if: condition

Print the Conditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset if: condition 
	"Print the Conditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 
				(condition ifTrue: ['jumpTrueBy: '] ifFalse: ['jumpFalseBy: ']), offset printString,
				' to: ', (labelling
							ifTrue: [(scanner pc + offset - method initialPC) printString]
							ifFalse: [labels at: scanner pc + offset])]
		ifFalse:
			[self print: 
				(condition ifTrue: ['jumpTrueTo: '] ifFalse: ['jumpFalseTo: ']), (labels at: scanner pc + offset + 1)]
</details>

#### RelativeInstructionPrinter>>#printInstructionsOn: aStream

Append to the stream, aStream, a description of each bytecode in the instruction stream.


<details>
	<summary>See more</summary>
	
	printInstructionsOn: aStream
	"Append to the stream, aStream, a description of each bytecode in the instruction stream."
	
	| label |
	labelling := true.
	labels := Array new: method size + 1 withAll: false.
	super printInstructionsOn: (String new: 1024) writeStream.
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream
</details>

#### RelativeInstructionPrinter>>#send: selector super: supered numArgs: numArgs

Print the Send Message With Selector, selector, bytecode. The argument, supered, indicates whether the receiver of the message is specified with 'super' in the source method. The arguments of the message are found in the top numArguments locations on the stack and the receiver just below them.


<details>
	<summary>See more</summary>
	
	send: selector super: supered numArgs: numArgs
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered
				ifTrue: ['superSend: ']
				ifFalse: ['send: '])
			, selector storeString
			, (numArgs = 1
				ifTrue: [' (1 arg)']
				ifFalse: [' (', numArgs printString, ' args)'])
</details>

#### RelativeInstructionPrinter>>#printCode

<details>
	<summary>See more</summary>
	
	printCode
	^printCode ~~ false
</details>

#### RelativeInstructionPrinter>>#jump: offset

Print the Unconditional Jump bytecode.


<details>
	<summary>See more</summary>
	
	jump: offset
	"Print the Unconditional Jump bytecode."

	labelling
		ifTrue:
			[labels at: scanner pc + offset + 1 put: true.
			 self print: 'jumpBy: ', offset printString,
				' to: ', (scanner pc + offset - method initialPC) printString]
		ifFalse:
			[self print: 'jumpTo: ', (labels at: scanner pc + offset + 1)]
</details>

#### RelativeInstructionPrinter>>#printInstructionsOn: aStream do: aBlock

Append to the stream, aStream, a description of each bytecode in the instruction stream. Evaluate aBlock with the receiver, the scanner and the stream after each instruction.


<details>
	<summary>See more</summary>
	
	printInstructionsOn: aStream do: aBlock
	"Append to the stream, aStream, a description of each bytecode in the instruction stream.
	  Evaluate aBlock with the receiver, the scanner and the stream after each instruction."
	
	| label |
	labelling := true.
	labels := Array new: method size withAll: false.
	super printInstructionsOn: (String new: 1024) writeStream do: [:ig :no :re|].
	label := 0.
	labels withIndexDo:
		[:bool :index|
		bool ifTrue: [labels at: index put: 'L', (label := label + 1) printString]].
	labelling := false.
	super printInstructionsOn: aStream do: aBlock
</details>

#### RelativeInstructionPrinter>>#printCode: aBoolean

<details>
	<summary>See more</summary>
	
	printCode: aBoolean
	printCode := aBoolean
</details>

