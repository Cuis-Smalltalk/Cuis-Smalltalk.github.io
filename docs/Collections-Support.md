## Association

I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary.

### Methods
#### Association>>#analogousCodeTo: anObject

From: Oscar Nierstrasz <oscar.nierstrasz@gmail.com>Add oscar.nierstrasz@gmail.com to my Address Book Subject: [Pharo-dev] CompiledMethod>>#= is broken The following code breaks: (SortedCollectionTest>>#testDo) = (SortedCollectionTest>>#testStoreOn). because it makes use of AdditionalMethodState>>#analogousCodeTo: which tries to send #analogousCodeTo: to a property which is an Association. It seems to me the solution is to implement Association>>#analogousCodeTo: with an equality test: analogousCodeTo: anObject ^self class == anObject class and: [ self = anObject ] Does this make sense? See: https://pharo.fogbugz.com/f/cases/12077/MNU-in-AdditionalMethodState-analogousCodeTo Oscar Nierstrasz


<details>
	<summary>See more</summary>
	
	analogousCodeTo: anObject
"
From:  	Oscar Nierstrasz <oscar.nierstrasz@gmail.com>Add oscar.nierstrasz@gmail.com to my Address Book
Subject:  	[Pharo-dev] CompiledMethod>>#= is broken

The following code breaks:

(SortedCollectionTest>>#testDo) = (SortedCollectionTest>>#testStoreOn).

because it makes use of AdditionalMethodState>>#analogousCodeTo: which tries to send #analogousCodeTo: to a property which is an Association.

It seems to me the solution is to implement Association>>#analogousCodeTo: with an equality test:

analogousCodeTo: anObject
   ^self class == anObject class
     and: [ self = anObject ]

Does this make sense?

See:

https://pharo.fogbugz.com/f/cases/12077/MNU-in-AdditionalMethodState-analogousCodeTo

Oscar Nierstrasz

"
   ^self class == anObject class
     and: [ self = anObject ]
</details>

#### Association>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^ key hash
</details>

#### Association>>#storeOn: aStream

Store in the format (key->value)


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	"Store in the format (key->value)"
	aStream nextPut: $(.
	key storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)
</details>

#### Association>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		print: self key;
		nextPutAll: ' -> ';
		print: self value
</details>

#### Association>>#value: anObject

Store the argument, anObject, as the value of the receiver.


<details>
	<summary>See more</summary>
	
	value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	value _ anObject
</details>

#### Association>>#literalEqual: otherLiteral

Answer true if the receiver and otherLiteral represent the same literal. Variable bindings are literally equals only if identical. This is how variable sharing works, by preserving identity and changing only the value.


<details>
	<summary>See more</summary>
	
	literalEqual: otherLiteral
	"Answer true if the receiver and otherLiteral represent the same literal.
	Variable bindings are literally equals only if identical.
	This is how variable sharing works, by preserving identity and changing only the value."
	^self == otherLiteral
</details>

#### Association>>#objectForDataStream: refStrm

I am about to be written on an object file. If I am a known global, write a proxy that will hook up with the same resource in the destination system.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  If I am a known global, write a proxy that will hook up with the same resource in the destination system."

	^ (Smalltalk associationAt: key ifAbsent: nil) == self 
		ifTrue: [dp _ DiskProxy global: #Smalltalk selector: #associationOrUndeclaredAt: 
							args: (Array with: key).
			refStrm replace: self with: dp.
			dp]
		ifFalse: [self]
</details>

#### Association>>#isSpecialWriteBinding

Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages


<details>
	<summary>See more</summary>
	
	isSpecialWriteBinding
	"Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages"
	^false
</details>

#### Association>>#value

Answer the value of the receiver.


<details>
	<summary>See more</summary>
	
	value
	"Answer the value of the receiver."

	^value
</details>

#### Association>>#= anAssociation

Compare the receiver with the argument and answer with true if the receiver is equal to the argument. Otherwise answer false.


<details>
	<summary>See more</summary>
	
	= anAssociation

	self == anAssociation ifTrue: [ ^ true ].
	^ super = anAssociation and: [value = anAssociation value]
</details>

#### Association>>#key: aKey value: anObject

Store the arguments as the variables of the receiver.


<details>
	<summary>See more</summary>
	
	key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key _ aKey.
	value _ anObject
</details>

## Link

An instance of me is a simple record of a pointer to another Link. I am an abstract class; my concrete subclasses, for example, Process, can be stored in a LinkedList structure.

### Methods
#### Link>>#nextLink

Answer the link to which the receiver points.


<details>
	<summary>See more</summary>
	
	nextLink
	"Answer the link to which the receiver points."

	^nextLink
</details>

#### Link>>#nextLink: aLink

Store the argument, aLink, as the link to which the receiver refers. Answer aLink.


<details>
	<summary>See more</summary>
	
	nextLink: aLink 
	"Store the argument, aLink, as the link to which the receiver refers. 
	Answer aLink."

	^nextLink _ aLink
</details>

## LookupKey

I represent a key for looking up entries in a data structure. Subclasses of me, such as Association, typically represent dictionary entries.

### Methods
#### LookupKey>>#beReadWriteBindingAnnouncing: aBool

Make the receiver (a global read-write binding) be a read-write binding


<details>
	<summary>See more</summary>
	
	beReadWriteBindingAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-write binding"
	^self beBindingOfType: Association announcing: aBool
</details>

#### LookupKey>>#recompileBindingsAnnouncing: aBool

Make the receiver (a global read-write binding) be a read-only binding


<details>
	<summary>See more</summary>
	
	recompileBindingsAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-only binding"
	aBool ifTrue:[
		Utilities informUserDuring: [ :barBlock |
			(Smalltalk allCallsOn: self) do: [ :mref | 
				barBlock value: 'Recompiling ', mref stringVersion.
				mref actualClass recompile: mref methodSymbol ].
		].
	] ifFalse:[
		(Smalltalk allCallsOn: self) do: [ :mref |
			mref actualClass recompile: mref methodSymbol ]
	]
</details>

#### LookupKey>>#beReadWriteBinding

Make the receiver (a global read-only binding) be a read-write binding


<details>
	<summary>See more</summary>
	
	beReadWriteBinding
	"Make the receiver (a global read-only binding) be a read-write binding"
	^self beReadWriteBindingAnnouncing: true
</details>

#### LookupKey>>#isSpecialReadBinding

Return true if this variable binding is read protected, e.g., should not be accessed primitively but rather by sending #value messages


<details>
	<summary>See more</summary>
	
	isSpecialReadBinding
	"Return true if this variable binding is read protected, e.g., should not be accessed primitively but rather by sending #value messages"
	^false
</details>

#### LookupKey>>#beReadOnlyBindingAnnouncing: aBool

Make the receiver (a global read-write binding) be a read-only binding


<details>
	<summary>See more</summary>
	
	beReadOnlyBindingAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beBindingOfType: ReadOnlyVariableBinding announcing: aBool
</details>

#### LookupKey>>#= aLookupKey

Compare the receiver with the argument and answer with true if the receiver is equal to the argument. Otherwise answer false.


<details>
	<summary>See more</summary>
	
	= aLookupKey

	self == aLookupKey ifTrue: [ ^ true ].
	self species == aLookupKey species
		ifFalse: [ ^false ].

	^ key = aLookupKey key
</details>

#### LookupKey>>#key: anObject

Store the argument, anObject, as the lookup key of the receiver.


<details>
	<summary>See more</summary>
	
	key: anObject 
	"Store the argument, anObject, as the lookup key of the receiver."

	key _ anObject
</details>

#### LookupKey>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^key hash
</details>

#### LookupKey>>#isVariableBinding

Return true if I represent a literal variable binding


<details>
	<summary>See more</summary>
	
	isVariableBinding
	"Return true if I represent a literal variable binding"
	^true
</details>

#### LookupKey>>#beBindingOfType: aClass announcing: aBool

Make the receiver a global binding of the given type


<details>
	<summary>See more</summary>
	
	beBindingOfType: aClass announcing: aBool
	"Make the receiver a global binding of the given type"
	| old new |
	(Smalltalk associationAt: self key) == self
		ifFalse:[^self error:'Not a global variable binding'].
	self class == aClass ifTrue:[^self].
	old _ self.
	new _ aClass key: self key value: self value.
	old become: new.
	"NOTE: Now self == read-only (e.g., the new binding)"
	^self recompileBindingsAnnouncing: aBool
</details>

#### LookupKey>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPut: $)
</details>

#### LookupKey>>#beReadOnlyBinding

Make the receiver (a global read-write binding) be a read-only binding


<details>
	<summary>See more</summary>
	
	beReadOnlyBinding
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beReadOnlyBindingAnnouncing: true
</details>

#### LookupKey>>#< aLookupKey

Refer to the comment in Magnitude|<.


<details>
	<summary>See more</summary>
	
	< aLookupKey 
	"Refer to the comment in Magnitude|<."

	^key < aLookupKey key
</details>

#### LookupKey>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^ self key isString
		ifTrue: [self key]
		ifFalse: [self key printString]
</details>

#### LookupKey>>#canAssign

<details>
	<summary>See more</summary>
	
	canAssign

	^ true
</details>

#### LookupKey>>#key

Answer the lookup key of the receiver.


<details>
	<summary>See more</summary>
	
	key
	"Answer the lookup key of the receiver."

	^key
</details>

## ReadOnlyVariableBinding

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ReadOnlyVariableBinding>>#isSpecialWriteBinding

Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages


<details>
	<summary>See more</summary>
	
	isSpecialWriteBinding
	"Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages"
	^true
</details>

#### ReadOnlyVariableBinding>>#value

<details>
	<summary>See more</summary>
	
	value
	^value
</details>

#### ReadOnlyVariableBinding>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		print: self key;
		nextPutAll: ' ->(ReadOnlyVariableBinding) ';
		print: self value.
</details>

#### ReadOnlyVariableBinding>>#value: aValue

<details>
	<summary>See more</summary>
	
	value: aValue
	(AttemptToWriteReadOnlyGlobal signal: 'Cannot store into read-only bindings') == true ifTrue:[
		value _ aValue.
	].
</details>

#### ReadOnlyVariableBinding>>#canAssign

<details>
	<summary>See more</summary>
	
	canAssign

	^ false
</details>

#### ReadOnlyVariableBinding>>#privateSetKey: aKey value: aValue

<details>
	<summary>See more</summary>
	
	privateSetKey: aKey value: aValue
	key _ aKey.
	value _ aValue
</details>

## WeakKeyAssociation

I am an association holding only weakly on my key.

### Methods
#### WeakKeyAssociation>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^self key hash
</details>

#### WeakKeyAssociation>>#key: aKey

Store the argument, anObject, as the lookup key of the receiver.


<details>
	<summary>See more</summary>
	
	key: aKey
	key := WeakArray with: aKey
</details>

#### WeakKeyAssociation>>#storeOn: aStream

Store in the format (key->value)


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	aStream 
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll:' key: '.
	self key storeOn: aStream.
	aStream nextPutAll: ' value: '.
	self value storeOn: aStream.
	aStream nextPut: $)
</details>

#### WeakKeyAssociation>>#key: aKey value: anObject

Store the arguments as the variables of the receiver.


<details>
	<summary>See more</summary>
	
	key: aKey value: anObject
	key := WeakArray with: aKey.
	value := anObject.
</details>

#### WeakKeyAssociation>>#< aLookupKey

Refer to the comment in Magnitude|<.


<details>
	<summary>See more</summary>
	
	< aLookupKey 
	"Refer to the comment in Magnitude|<."

	^self key < aLookupKey key
</details>

#### WeakKeyAssociation>>#= aLookupKey

Compare the receiver with the argument and answer with true if the receiver is equal to the argument. Otherwise answer false.


<details>
	<summary>See more</summary>
	
	= aLookupKey
	self == aLookupKey ifTrue: [ ^ true ].

	self species == aLookupKey species
		ifFalse: [ ^ false ].

	^self key = aLookupKey key
</details>

#### WeakKeyAssociation>>#key

Answer the lookup key of the receiver.


<details>
	<summary>See more</summary>
	
	key
	^key ifNotNil: [ key at: 1]
</details>

## WeakValueAssociation

I am a lookup key (acting like an association but) holding only weakly on my value.

### Methods
#### WeakValueAssociation>>#value: anObject

Store the argument, anObject, as the value of the receiver.


<details>
	<summary>See more</summary>
	
	value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	self at: 1 put: anObject
</details>

#### WeakValueAssociation>>#key: aKey value: anObject

Store the arguments as the variables of the receiver.


<details>
	<summary>See more</summary>
	
	key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key _ aKey.
	self value: anObject
</details>

#### WeakValueAssociation>>#value

<details>
	<summary>See more</summary>
	
	value
	^self at: 1
</details>

