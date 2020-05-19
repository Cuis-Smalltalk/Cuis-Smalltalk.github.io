## Array

I present an ArrayedCollection whose elements are objects.

### Methods
#### Array>>#storeOn: aStream

Use the literal form if possible.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: 
			[aStream nextPut: $#; nextPut: $(.
			self do: 
				[:element | 
				element printOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]
</details>

#### Array>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Array or: [ super is: aSymbol ]
</details>

#### Array>>#hasLiteralSuchThat: testBlock

Answer true if testBlock returns true for any literal in this array, even if imbedded in further Arrays or CompiledMethods. This method is only intended for private use by CompiledMethod hasLiteralSuchThat:


<details>
	<summary>See more</summary>
	
	hasLiteralSuchThat: testBlock
	"Answer true if testBlock returns true for any literal in this array, even if imbedded in 	further Arrays or CompiledMethods.  This method is only intended for private use by 	CompiledMethod 	hasLiteralSuchThat:"
	| lit |
	1 to: self size do: [:index |
		(testBlock value: (lit := self at: index)) ifTrue: [^ true].
		(lit isArray and: [lit hasLiteralSuchThat: testBlock]) ifTrue: [^ true]].
	^ false
</details>

#### Array>>#isLiteral

Definition from Squeak


<details>
	<summary>See more</summary>
	
	isLiteral
	"Definition from Squeak"
	^ self class == Array and: [
		self allSatisfy: [ :each |
			each isLiteral ]].
</details>

#### Array>>#hasLiteral: literal

Answer true if literal is identical to any literal in this array, even if imbedded in further array structure. This method is only intended for private use by CompiledMethod hasLiteralSymbol:


<details>
	<summary>See more</summary>
	
	hasLiteral: literal
	"Answer true if literal is identical to any literal in this array, even 
	if imbedded in further array structure. This method is only intended 
	for private use by CompiledMethod hasLiteralSymbol:"

	| lit |
	1 to: self size do: 
		[:index | 
		((lit := self at: index) literalEqual: literal) ifTrue: [^true].
		(Array == lit class and: [lit hasLiteral: literal]) ifTrue: [^true]].
	^false
</details>

#### Array>>#identityIncludes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	identityIncludes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self statePointsTo: anObject
</details>

#### Array>>#evalStrings

Allows you to construct literal arrays. #(true false nil '5@6' 'Set new' '''text string''') evalStrings gives an array with true, false, nil, a Point, a Set, and a String instead of just a bunch of Symbols


<details>
	<summary>See more</summary>
	
	evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"

    ^ self collect: [:each |  | it |
        it _ each.
        each == #true ifTrue: [it _ true].
		      each == #false ifTrue: [it _ false].
        each == #nil ifTrue: [it _ nil].
        each class == String ifTrue: [
			it _ Compiler evaluate: each].
        each class == Array ifTrue: [it _ it evalStrings].
        it]
</details>

#### Array>>#literalEqual: other

<details>
	<summary>See more</summary>
	
	literalEqual: other

	self class == other class ifFalse: [^ false].
	self size = other size ifFalse: [^ false].
	self with: other do: [:e1 :e2 |
		(e1 literalEqual: e2) ifFalse: [^ false]].
	^ true
</details>

#### Array>>#isArray

<details>
	<summary>See more</summary>
	
	isArray
	^true
</details>

#### Array>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self == Smalltalk specialObjectsArray
		ifTrue: [
			aStream nextPutAll: 'Smalltalk specialObjectsArray' ]
		ifFalse: [
			aStream nextPut: $#.
			self printElementsOn: aStream ]
</details>

#### Array>>#elementsForwardIdentityTo: otherArray

This primitive performs a bulk mutation, causing all pointers to the elements of the receiver to be replaced by pointers to the corresponding elements of otherArray. The identityHashes remain with the pointers rather than with the objects so that the objects in this array should still be properly indexed in any existing hashed structures after the mutation. Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples.


<details>
	<summary>See more</summary>
	
	elementsForwardIdentityTo: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 The identityHashes remain with the pointers rather than with the objects so that
	 the objects in this array should still be properly indexed in any existing hashed
	 structures after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

	<primitive: 72 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[self error: 'The virtual machine is out-of-date.  Please upgrade.'].
	self primitiveFailed
</details>

#### Array>>#atWrap: index put: anObject

Optimized to go through the primitive if possible


<details>
	<summary>See more</summary>
	
	atWrap: index put: anObject
	"Optimized to go through the primitive if possible"
	<primitive: 61>
	^ self at: index - 1 \\ self size + 1 put: anObject
</details>

#### Array>>#atWrap: index

Optimized to go through the primitive if possible


<details>
	<summary>See more</summary>
	
	atWrap: index 
	"Optimized to go through the primitive if possible"
	<primitive: 60>
	^ self at: index - 1 \\ self size + 1
</details>

#### Array>>#asArray

Answer with the receiver itself. But for subclasses, answer an actual array!


<details>
	<summary>See more</summary>
	
	asArray
	"Answer with the receiver itself.
	But for subclasses, answer an actual array!"

	^self class == Array
		ifTrue: [ self ]
		ifFalse: [ self asNewArray ]
</details>

#### Array>>#elementsForwardIdentityTo: otherArray copyHash: copyHash

This primitive performs a bulk mutation, causing all pointers to the elements of the receiver to be replaced by pointers to the corresponding elements of otherArray. If copyHash is true, the identityHashes remain with the pointers rather than with the objects so that the objects in the receiver should still be properly indexed in any existing hashed structures after the mutation. If copyHash is false, then the hashes of the objects in otherArray remain unchanged. If you know what you're doing this may indeed be what you want. Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples.


<details>
	<summary>See more</summary>
	
	elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 If copyHash is true, the identityHashes remain with the pointers rather than with the
	 objects so that the objects in the receiver should still be properly indexed in any
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes
	 of the objects in otherArray remain unchanged.  If you know what you're doing this
	 may indeed be what you want.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."
	<primitive: 249 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	self primitiveFailed
</details>

#### Array>>#elementsExchangeIdentityWith: otherArray

This primitive performs a bulk mutation, causing all pointers to the elements of the receiver to be replaced by pointers to the corresponding elements of otherArray. At the same time, all pointers to the elements of otherArray are replaced by pointers to the corresponding elements of this array. The identityHashes remain with the pointers rather than with the objects so that objects in hashed structures should still be properly indexed after the mutation. Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples.


<details>
	<summary>See more</summary>
	
	elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of the
	 receiver to be replaced by pointers to the corresponding elements of otherArray.
	 At the same time, all pointers to the elements of otherArray are replaced by
	 pointers to the corresponding elements of this array.  The identityHashes remain
	 with the pointers rather than with the objects so that objects in hashed structures
	 should still be properly indexed after the mutation.

	Warning. This is a dangerous operation and it could lead to a crash if some object in receiver or argument is receiver of a method currently in execution. See #anyReceiverInStackIn: See senders for examples."

	<primitive: 128 error: ec>
	ec == #'bad receiver' ifTrue:
		[^self error: 'receiver must be of class Array'].
	ec == #'bad argument' ifTrue:
		[^self error: (otherArray class == Array
						ifTrue: ['arg must be of class Array']
						ifFalse: ['receiver and argument must have the same size'])].
	ec == #'inappropriate operation' ifTrue:
		[^self error: 'can''t become immediates such as SmallIntegers or Characters'].
	ec == #'no modification' ifTrue:
		[^self error: 'can''t become immutable objects'].
	ec == #'object is pinned' ifTrue:
		[^self error: 'can''t become pinned objects'].
	ec == #'insufficient object memory' ifTrue:
		[| maxRequired |
		 "In Spur, two-way become may involve making each pair of objects into a forwarder into a copy of the other.
		 So if become fails with #'insufficient object memory', garbage collect, and if necessary, grow memory."
		 maxRequired := (self sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0])
						+ (otherArray sum: [:obj | obj class byteSizeOfInstanceOfSize: obj basicSize] ifEmpty: [0]).
		 (Smalltalk garbageCollectMost < maxRequired
		  and: [Smalltalk garbageCollect < maxRequired]) ifTrue:
			[Smalltalk growMemoryByAtLeast: maxRequired].
		 ^self elementsExchangeIdentityWith: otherArray].
	self primitiveFailed
</details>

#### Array>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

## Array2D

My instances are two dimensional arrays, providing basic accessors and some convenience methods. Specialized subclasses provide additional services. See packages LinearAlgebra and SignalProcessing.

### Methods
#### Array2D>>#fillWithArrayOfArrays: anArray

Array2D fromArrayOfArrays: #( #(1 2 0 0 0 0) #(2 4 1 0 0 -4) #(0 1 1 1 0 0) #(0 0 1 1 1 0) #(0 0 0 1 1 1) #(0 1 0 0 1 1) ).


<details>
	<summary>See more</summary>
	
	fillWithArrayOfArrays: anArray
	"Array2D fromArrayOfArrays: #(
		#(1 2 0 0 0 0) 
		#(2 4 1 0 0 -4) 
		#(0 1 1 1 0 0)
		#(0 0 1 1 1 0)
		#(0 0 0 1 1 1)
		#(0 1 0 0 1 1)
	)."

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put: ((anArray at: i) at: j) ] ]
</details>

#### Array2D>>#at: aPointOrIndex put: aNumber

If aPointOrIndex is a Number, the receiver must be a vector (either row or column). Indexes goes from 1 to size (width or height)


<details>
	<summary>See more</summary>
	
	at: aPointOrIndex put: aNumber
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i _ aPointOrIndex y. j _ aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i _ aPointOrIndex. j _ 1 ].
			height = 1 ifTrue: [ i _ 1. j _ aPointOrIndex ]].
	^ self i: i j: j put: aNumber
</details>

#### Array2D>>#withIndexesDo: aBlock

Evaluate aBlock on each element, including i, j indexes also as arguments


<details>
	<summary>See more</summary>
	
	withIndexesDo: aBlock
	"Evaluate aBlock on each element, including i, j indexes also as arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: i value: j value: (self i: i j: j) ] ]
</details>

#### Array2D>>#initHeight: h width: w

<details>
	<summary>See more</summary>
	
	initHeight: h width: w

	height _ h.
	width _ w.
	self initializeElements
</details>

#### Array2D>>#= another

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= another
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	width = another width ifFalse: [ ^false ].
	height = another height ifFalse: [ ^false ].
	^elements = another elements
</details>

#### Array2D>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^elements hash
</details>

#### Array2D>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	1 to: height do: [ :i |
		aStream nextPutAll: '| '.
		1 to: width do: [ :j |
			aStream print: (self i: i j: j).
			aStream nextPut: $  ].
		aStream nextPut: $|; newLine ]
</details>

#### Array2D>>#x: x y: y put: aNumber

Set the value at coordinates x@y. x and y are integers in [1 .. width] and [1 .. height]


<details>
	<summary>See more</summary>
	
	x: x y: y put: aNumber
	"Set the value at coordinates x@y.
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x put: aNumber
</details>

#### Array2D>>#size

The result is a point. So, x is our width and y is our height


<details>
	<summary>See more</summary>
	
	size
	"The result is a point. So, x is our width and y is our height"

	^width @ height
</details>

#### Array2D>>#elements

<details>
	<summary>See more</summary>
	
	elements

	^elements
</details>

#### Array2D>>#do: aBlock

Evaluate aBlock on each element


<details>
	<summary>See more</summary>
	
	do: aBlock
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) ] ]
</details>

#### Array2D>>#x: x y: y

Answer element at column x, row y x and y are integers in [1 .. width] and [1 .. height]


<details>
	<summary>See more</summary>
	
	x: x y: y
	"Answer element at column x, row y
	x and y are integers in [1 .. width] and [1 .. height]"

	^ self i: y j: x
</details>

#### Array2D>>#i: i j: j

Answer element at row i, column j


<details>
	<summary>See more</summary>
	
	i: i j: j

	"Answer element at row i, column j"

	^ elements at: (self elementsIndexForI: i j: j)
</details>

#### Array2D>>#extent

The result is a point. So, x is our width and y is our height


<details>
	<summary>See more</summary>
	
	extent
	"The result is a point. So, x is our width and y is our height"

	^width @ height
</details>

#### Array2D>>#with: otherImage do: aBlock

Evaluate aBlock on each element


<details>
	<summary>See more</summary>
	
	with: otherImage do: aBlock
	"Evaluate aBlock on each element"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			aBlock value: (self i: i j: j) value: (otherImage i: i j: j) ] ]
</details>

#### Array2D>>#at: aPointOrIndex

If aPointOrIndex is a Number, the receiver must be a vector (either row or column). Indexes goes from 1 to size (width or height)


<details>
	<summary>See more</summary>
	
	at: aPointOrIndex
	"If aPointOrIndex is a Number, the receiver must be a vector (either row or column).
	Indexes goes from 1 to size (width or height)"

	| i j |
	aPointOrIndex isPoint
		ifTrue: [ i _ aPointOrIndex y. j _ aPointOrIndex x ]
		ifFalse: [
			width = 1 ifTrue: [ i _ aPointOrIndex. j _ 1 ].
			height = 1 ifTrue: [ i _ 1. j _ aPointOrIndex. ]].
	^ self i: i j: j 
</details>

#### Array2D>>#print

<details>
	<summary>See more</summary>
	
	print
	self printOn: Transcript.
	Transcript newLine
</details>

#### Array2D>>#width

<details>
	<summary>See more</summary>
	
	width

	^width
</details>

#### Array2D>>#fillWith: anArray2D

<details>
	<summary>See more</summary>
	
	fillWith: anArray2D

	1 to: (height min: anArray2D height) do: [ :i |
		1 to: (width min: anArray2D width) do: [ :j |
			self i: i j: j put: (anArray2D i: i j: j) ] ]
</details>

#### Array2D>>#height

<details>
	<summary>See more</summary>
	
	height

	^height
</details>

#### Array2D>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	elements _ elements copy
</details>

#### Array2D>>#isSquare

<details>
	<summary>See more</summary>
	
	isSquare

	^height = width
</details>

#### Array2D>>#wrapI: i j: j put: anObject

<details>
	<summary>See more</summary>
	
	wrapI: i j: j put: anObject

	^ self i: i-1\\height+1 j: j-1\\width+1 put: anObject
</details>

#### Array2D>>#initializeElements

<details>
	<summary>See more</summary>
	
	initializeElements

	elements _ Array new: height * width
</details>

#### Array2D>>#elementsIndexForI: i j: j

Indexes goes from 1 to size (width or height)


<details>
	<summary>See more</summary>
	
	elementsIndexForI: i j: j
	"Indexes goes from 1 to size (width or height)"

	(j between: 1 and: width) ifFalse: [ self errorSubscriptBounds: j@i ].
	^ i-1*width+j
</details>

#### Array2D>>#replaceValues: aBlock

Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments


<details>
	<summary>See more</summary>
	
	replaceValues: aBlock
	"Replace each value with the result of evaluating aBlock, with i, j and the previous value as the arguments"

	1 to: height do: [ :i |
		1 to: width do: [ :j |
			self i: i j: j put:
				(aBlock value: i value: j value: (self i: i j: j)) ] ]
</details>

#### Array2D>>#wrapI: i j: j

<details>
	<summary>See more</summary>
	
	wrapI: i j: j

	^ self i: i-1\\height+1 j: j-1\\width+1
</details>

#### Array2D>>#i: i j: j put: anObject

Store anObject as the element at row i, column j


<details>
	<summary>See more</summary>
	
	i: i j: j put: anObject
	"Store anObject as the element at row i, column j"

	elements at: (self elementsIndexForI: i j: j) put: anObject
</details>

## ByteArray

I represent an ArrayedCollection whose elements are integers between 0 and 255. We have a literal syntax: #[1 2 3 4]

### Methods
#### ByteArray>>#byteAt: index

<details>
	<summary>See more</summary>
	
	byteAt: index
	<primitive: 60>
	^self at: index
</details>

#### ByteArray>>#long64At: index bigEndian: bigEndian

Return a 64-bit signed integer quantity starting from the given byte index.


<details>
	<summary>See more</summary>
	
	long64At: index bigEndian: bigEndian
	"Return a 64-bit signed integer quantity starting from the given byte index."

	| value |
	value := self unsignedLong64At: index bigEndian: bigEndian.
	value digitLength < 8 ifTrue: [ ^value ].
	(value digitAt: 8) < 16r80 ifTrue: [ ^value ].
	^value - 16r10000000000000000
</details>

#### ByteArray>>#floatAt: index bigEndian: bool

Return a 32 bit float starting from the given byte index. We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float.


<details>
	<summary>See more</summary>
	
	floatAt: index bigEndian: bool 
	"Return a 32 bit float starting from the given byte index.
	We answer an instance of Float, i.e. 64 bits, but holding the 32 bit Float."
	| w |
	w := self unsignedLongAt: index bigEndian: bool.
	^ Float fromIEEE32Bit: w
</details>

#### ByteArray>>#bitAt: bitIndex

Answer the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray. Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8.


<details>
	<summary>See more</summary>
	
	bitAt: bitIndex
	"Answer the bit (0 or 1) at a bit index.
	This way, the receiver behaves as a BitArray.
	Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8."
	| bitPosition index |
	index _ bitIndex - 1 // 8 + 1.
	bitPosition _ bitIndex - 1 \\ 8 + 1.
	^ self bitAt: index bitPosition: bitPosition
</details>

#### ByteArray>>#bitAt: index bitPosition: bitPosition

Answer the bit (0 or 1) at byte at index, at bitPosition. The bits are indexed starting at 1 for the least significant bit


<details>
	<summary>See more</summary>
	
	bitAt: index bitPosition: bitPosition
	"Answer the bit (0 or 1) at byte at index, at bitPosition.
	The bits are indexed starting at 1 for the least significant bit"
	^(self at: index) bitAt: bitPosition
</details>

#### ByteArray>>#shortAt: index put: value bigEndian: bigEndian

Store a 16-bit signed integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	shortAt: index put: value bigEndian: bigEndian
	"Store a 16-bit signed integer quantity starting from the given byte index"
	
	| unsignedValue |
	(unsignedValue := value) < 0 ifTrue: [
		unsignedValue := unsignedValue + 16r10000 ].
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (unsignedValue bitShift: -8);
			at: index put: (unsignedValue bitAnd: 16rFF).
		^value ].
	self
		at: index put: (unsignedValue bitShift: -8);
		at: index + 1 put: (unsignedValue bitAnd: 16rFF).
	^value
</details>

#### ByteArray>>#isLiteral

so that #(1 #[1 2 3] 5) prints itself


<details>
	<summary>See more</summary>
	
	isLiteral
	"so that #(1 #[1 2 3] 5) prints itself"
	^self class == ByteArray
</details>

#### ByteArray>>#longAt: index bigEndian: bigEndian

Return a 32-bit integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers.


<details>
	<summary>See more</summary>
	
	longAt: index bigEndian: bigEndian
	"Return a 32-bit integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers."
	
	| byte result |
	bigEndian ifFalse: [
		(byte := self at: index + 3) <= 16r7F ifTrue: [ "Is the result non-negative?"
			byte <= 16r3F ifTrue: [
				^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
			^(LargePositiveInteger new: 4)
				replaceFrom: 1
					to: 4
					with: self
					startingAt: index;
				normalize ].
		"Negative"
		byte >= 16rC0 ifTrue: [
			^-1 - (((((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index) bitXor: 16rFF)) ].
		(result := LargeNegativeInteger new: 4)
			digitAt: 4 put: ((self at: index + 3) bitXor: 16rFF);
			digitAt: 3 put: ((self at: index + 2) bitXor: 16rFF);
			digitAt: 2 put: ((self at: index + 1) bitXor: 16rFF).
		(byte := ((self at: index) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
			^result
				digitAt: 1 put: byte;
				normalize ].
		^result
			digitAt: 1 put: 16rFF;
			- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this." ].
	(byte := self at: index) <= 16r7F ifTrue: [ "Is the result non-negative?"
		byte <= 16r3F ifTrue: [
			^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3) ].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize ].
	"Negative"
	16rC0 <= byte ifTrue: [
		^-1 - (((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitXor: 16rFFFFFF) bitShift: 8) + ((self at: index + 3) bitXor: 16rFF)) ].
	(result := LargeNegativeInteger new: 4)
		digitAt: 4 put: (byte bitXor: 16rFF);
		digitAt: 3 put: ((self at: index + 1) bitXor: 16rFF);
		digitAt: 2 put: ((self at: index + 2) bitXor: 16rFF).
	(byte := ((self at: index + 3) bitXor: 16rFF) + 1) <= 16rFF ifTrue: [
		^result
			digitAt: 1 put: byte;
			normalize ].
	^result 
		digitAt: 1 put: 16rFF;
		- 1 "It's tempting to do the subtraction in a loop to avoid the LargeInteger creation, but it's actually slower than this."
</details>

#### ByteArray>>#= another

Should do something with BitBlt... Same for Bitmap, and any other Words or Byes object! OTOH, String uses our #hashBytes:startingWith: . We could use their equality...


<details>
	<summary>See more</summary>
	
	= another
	"Should do something with BitBlt... Same for Bitmap, and any other Words or Byes object!
	OTOH, String uses our #hashBytes:startingWith: . We could use their equality..."
	self == another ifTrue: [ ^ true ].
	^super = another
</details>

#### ByteArray>>#doubleAt: index put: value bigEndian: bool

Store a 64 bit float starting from the given byte index


<details>
	<summary>See more</summary>
	
	doubleAt: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self unsignedLongAt: index put: w1 bigEndian: bool.
	self unsignedLongAt: index + 4 put: w2 bigEndian: bool.
	^ value
</details>

#### ByteArray>>#hash

#hash is implemented, because #= is implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is implemented, because #= is implemented"
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^self class
		hashBytes: self
		startingWith: self species hash
</details>

#### ByteArray>>#atAllPut: value

Fill the receiver with the given value


<details>
	<summary>See more</summary>
	
	atAllPut: value
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
</details>

#### ByteArray>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self storeOn: aStream
</details>

#### ByteArray>>#unsignedLong64At: index bigEndian: bigEndian

Return a 64-bit unsigned integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers.


<details>
	<summary>See more</summary>
	
	unsignedLong64At: index bigEndian: bigEndian
	"Return a 64-bit unsigned integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers."

	| byte |
	SmallInteger maxVal >  1073741823 ifTrue:
		[bigEndian
			ifTrue: "64-bit SmallIntegers have a 3 bit tag and a sign bit, so the most positive value has 16rF as its top byte."
				[(byte := self at: index) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)) bitShift: 8)
						+ (self at: index + 4) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)]]
			ifFalse:
				[(byte := self at: index + 7) <= 16rF ifTrue:
					[^((((((((byte bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 4)) bitShift: 8)
						+ (self at: index + 3) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)]]].
	bigEndian ifFalse: [
		(byte := self at: index + 7) = 0 ifFalse: [
			^(LargePositiveInteger new: 8)
				replaceFrom: 1 to: 8 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 6) = 0 ifFalse: [
			^(LargePositiveInteger new: 7)
				replaceFrom: 1 to: 7 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 5) = 0 ifFalse: [
			^(LargePositiveInteger new: 6)
				replaceFrom: 1 to: 6 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 4) = 0 ifFalse: [
			^(LargePositiveInteger new: 5)
				replaceFrom: 1 to: 5 with: self startingAt: index;
				normalize ].
		(byte := self at: index + 3) <= 16r3F ifFalse: [
			^(LargePositiveInteger new: 4)
				replaceFrom: 1 to: 4 with: self startingAt: index;
				normalize ].
		^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index) ].
	(byte := self at: index) = 0 ifFalse: [
		^(LargePositiveInteger new: 8)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: (self at: index + 1);
			digitAt: 8 put: byte;
			normalize ].
	(byte := self at: index + 1) = 0 ifFalse: [	
		^(LargePositiveInteger new: 7)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: (self at: index + 2);
			digitAt: 7 put: byte;
			normalize ].
	(byte := self at: index + 2) = 0 ifFalse: [	
		^(LargePositiveInteger new: 6)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: (self at: index + 3);
			digitAt: 6 put: byte;
			normalize ].
	(byte := self at: index + 3) = 0 ifFalse: [	
		^(LargePositiveInteger new: 5)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: (self at: index + 4);
			digitAt: 5 put: byte;
			normalize ].		
	(byte := self at: index + 4) <= 16r3F ifFalse: [
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 7);
			digitAt: 2 put: (self at: index + 6);
			digitAt: 3 put: (self at: index + 5);
			digitAt: 4 put: byte;
			normalize ].
	^(((byte bitShift: 8) + (self at: index + 5) bitShift: 8) + (self at: index + 6) bitShift: 8) + (self at: index + 7)
</details>

#### ByteArray>>#bitAt: index bitPosition: bitPosition put: aBit

Set the bit (0 or 1) at byte at index, at bitPosition. The bit value should be 0 or 1, otherwise raise an Error. The bits are indexed starting at 1 for the least significant bit


<details>
	<summary>See more</summary>
	
	bitAt: index bitPosition: bitPosition put: aBit
	"Set the bit (0 or 1) at byte at index, at bitPosition. 
	The bit value should be 0 or 1, otherwise raise an Error.
	The bits are indexed starting at 1 for the least significant bit"
	self at: index put: ((self at: index) bitAt: bitPosition put: aBit)
</details>

#### ByteArray>>#bitBooleanAt: bitIndex put: aBoolean

Consider the bit at bitIndex as a Boolean value. 0 -> false 1 -> true


<details>
	<summary>See more</summary>
	
	bitBooleanAt: bitIndex put: aBoolean
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	self bitAt: bitIndex put: (aBoolean ifTrue: [1] ifFalse: [0])
</details>

#### ByteArray>>#floatAt: index put: value bigEndian: bool

Store a 32 bit float starting from the given byte index. Round value to a 32bit Float, and store it.


<details>
	<summary>See more</summary>
	
	floatAt: index put: value bigEndian: bool 
	"Store a 32 bit float starting from the given byte index.
	Round value to a 32bit Float, and store it."
	| w |
	w _ value asIEEE32BitWord.
	self unsignedLongAt: index put: w bigEndian: bool.
	^ value
</details>

#### ByteArray>>#unsignedLong64At: index put: value bigEndian: bigEndian

Store a 64-bit unsigned integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	unsignedLong64At: index put: value bigEndian: bigEndian
	"Store a 64-bit unsigned integer quantity starting from the given byte index"
	
	| i j |
	value isLarge ifTrue: [
		i := value digitLength.
		bigEndian ifFalse: [
			self
				replaceFrom: index
					to: index + i - 1
					with: value 
					startingAt: 1;
				replaceFrom: index + i
					to: index + 7
					with: #[0 0 0 0 0 0 0 0]
					startingAt: 1.
			^value ].
		j := index + 8.
		i <= 7 ifTrue: [
			self
				replaceFrom: index
				to: j - i - 1
				with: #[0 0 0 0 0 0 0 0]
				startingAt: 1 ].
		[ 1 <= i ] whileTrue: [
			self at: j - i put: (value digitAt: i).
			i := i - 1 ].
		^value ].
	bigEndian ifFalse: [
		j := index - 1.
		i := value.
		[ 1 <= i ] whileTrue: [
			self at: (j := j + 1) put: (i bitAnd: 16rFF).
			i := i bitShift: -8 ].
		self replaceFrom: j + 1
			to: index + 7
			with: #[0 0 0 0 0 0 0 0]
			startingAt: 1.
		^value ].
	j := index + 8.
	i := value.
	[ 1 <= i ] whileTrue: [
		self at: (j := j - 1) put: (i bitAnd: 16rFF).
		i := i bitShift: -8 ].
	self replaceFrom: index
		to: j - 1
		with: #[0 0 0 0 0 0 0 0]
		startingAt: 1.
	^value
</details>

#### ByteArray>>#asByteArray

Answer a ByteArray whose elements are the elements of the receiver. Optimized implementation.


<details>
	<summary>See more</summary>
	
	asByteArray
	^self
</details>

#### ByteArray>>#bytesPerElement

Number of bytes in each item. This multiplied by (self size)*8 gives the number of bits stored.


<details>
	<summary>See more</summary>
	
	bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 1
</details>

#### ByteArray>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	aStream nextPutAll: '#['.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]
</details>

#### ByteArray>>#indexOf: anInteger startingAt: start

Answer the index of the first occurence of anElement after start within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	indexOf: anInteger startingAt: start

	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	^String indexOfByte: anInteger inString: self startingAt: start
</details>

#### ByteArray>>#asString

Convert to a String with Characters for each byte. Fast code uses primitive that avoids character conversion


<details>
	<summary>See more</summary>
	
	asString
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self
</details>

#### ByteArray>>#unsignedShortAt: index bigEndian: bigEndian

Return a 16-bit unsigned integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	unsignedShortAt: index bigEndian: bigEndian
	"Return a 16-bit unsigned integer quantity starting from the given byte index"

	bigEndian ifFalse: [ ^((self at: index + 1) bitShift: 8) + (self at: index) ].
	^((self at: index) bitShift: 8) + (self at: index + 1)
	
</details>

#### ByteArray>>#unsignedShortAt: index put: value bigEndian: bigEndian

Store a 16-bit unsigned integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	unsignedShortAt: index put: value bigEndian: bigEndian
	"Store a 16-bit unsigned integer quantity starting from the given byte index"
	
	bigEndian ifFalse: [
		self 
			at: index + 1 put: (value bitShift: -8);
			at: index put: (value bitAnd: 16rFF).
		^value ].
	self
		at: index put: (value bitShift: -8);
		at: index+1 put: (value bitAnd: 16rFF).
	^value
</details>

#### ByteArray>>#unsignedLongAt: index put: value bigEndian: bigEndian

Store a 32-bit unsigned integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	unsignedLongAt: index put: value bigEndian: bigEndian
	"Store a 32-bit unsigned integer quantity starting from the given byte index"
	
	value isLarge
		ifTrue: [
			bigEndian ifFalse: [
				self
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index +3 put: (value digitAt: 1) ]
		ifFalse: [
			bigEndian ifFalse: [
				self 
					at: index put: (value bitAnd: 16rFF);
					at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
					at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
					at: index + 3 put: (value bitShift: -24).
				^value ].
			self 
				at: index put: (value bitShift: -24);
				at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
				at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
				at: index + 3 put: (value bitAnd: 16rFF) ].
	^value
</details>

#### ByteArray>>#longAt: index put: value bigEndian: bigEndian

Store a 32-bit signed integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	longAt: index put: value bigEndian: bigEndian
	"Store a 32-bit signed integer quantity starting from the given byte index"
	
	| v v2 |
	value isLarge ifTrue: [
		bigEndian ifFalse: [
			value positive ifTrue: [
				self 
					replaceFrom: index
					to: index + 3
					with: value
					startingAt: 1.
				^value ].
			v := 0.
			[ v <= 3 and: [ (v2 := ((value digitAt: v + 1) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
				self at: index + v put: 0.
				v := v + 1 ].
			self at: index + v put: v2.
			v := v + 1.
			[ v <= 3 ] whileTrue: [
				self at: index + v put: ((value digitAt: (v := v + 1)) bitXor: 16rFF) ].
			^value ].
		value positive ifTrue: [
			self
				at: index put: (value digitAt: 4);
				at: index + 1 put: (value digitAt: 3);
				at: index + 2 put: (value digitAt: 2);
				at: index + 3 put: (value digitAt: 1).
			^value ].
		v := 3.
		[ 0 <= v and: [ (v2 := ((value digitAt: 4 - v) bitXor: 16rFF) + 1) = 16r100 ] ] whileTrue: [
			self at: index + v put: 0.
			v := v - 1 ].
		self at: index + v put: v2.
		[ 0 <= (v := v - 1) ] whileTrue: [
			self at: index + v put: ((value digitAt: 4 - v) bitXor: 16rFF) ].
		^value ].
	v := value bitShift: -24.
	0 <= (v := (v bitAnd: 16r7F) - (v bitAnd: 16r80)) ifFalse: [
		v := v + 16r100 ].
	bigEndian ifFalse: [
		self 
			at: index put: (value bitAnd: 16rFF);
			at: index + 1 put: ((value bitShift: -8) bitAnd: 16rFF);
			at: index + 2 put: ((value bitShift: -16) bitAnd: 16rFF);
			at: index + 3 put: v.
		^value ].
	self
		at: index put: v;
		at: index + 1 put: ((value bitShift: -16) bitAnd: 16rFF);
		at: index + 2 put: ((value bitShift: -8) bitAnd: 16rFF);
		at: index + 3 put: (value bitAnd: 16rFF).
	^value
</details>

#### ByteArray>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self size
</details>

#### ByteArray>>#long64At: index put: value bigEndian: bigEndian

Store a 64-bit signed integer quantity starting from the given byte index.


<details>
	<summary>See more</summary>
	
	long64At: index put: value bigEndian: bigEndian
	"Store a 64-bit signed integer quantity starting from the given byte index."
	
	^self
		unsignedLong64At: index
		put: (value negative
			ifFalse: [ value ]
			ifTrue: [ value + 16r10000000000000000 ])
		bigEndian: bigEndian
</details>

#### ByteArray>>#bitBooleanAt: bitIndex

Consider the bit at bitIndex as a Boolean value. 0 -> false 1 -> true


<details>
	<summary>See more</summary>
	
	bitBooleanAt: bitIndex
	"Consider the bit at bitIndex as a Boolean value.
	0 -> false
	1 -> true"
	^ (self bitAt: bitIndex) = 1
</details>

#### ByteArray>>#bitAt: bitIndex put: aBit

Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8. #[1 0 0 ] bitAt: 1 #[0 1 0 ] bitAt: 9 #[0 0 128 ] bitAt: 24


<details>
	<summary>See more</summary>
	
	bitAt: bitIndex put: aBit
	"Set the bit (0 or 1) at a bit index. This way, the receiver behaves as a BitArray
	Note: There is no error raised if you the possible access extra bits at the end if size is not multiple of 8.
	#[1 0 0 ] bitAt: 1
	#[0 1  0 ] bitAt: 9
	#[0 0 128 ] bitAt: 24
	"
	| bitPosition index |
	index _ bitIndex - 1 // 8 + 1.
	bitPosition _ bitIndex - 1 \\ 8 + 1.
	self bitAt: index bitPosition: bitPosition put: aBit
</details>

#### ByteArray>>#hex

Answer an hexa decimal representation of the receiver #[122 43 213 7] hex => ' 7A2BD507'


<details>
	<summary>See more</summary>
	
	hex
	"Answer an hexa decimal representation of the receiver
	
	        #[122 43 213 7] hex
                => ' 7A2BD507'
	"
	| string v index map |
	map := '0123456789ABCDEF'.
	string := String new: self size * 2. "hex"
	index := 0.
	1 to: self size do:[:i| 
		v := self at: i.
		string at: (index := index + 1) put: (map at: (v bitShift: -4) + 1). 
		string at: (index := index + 1) put: (map at: (v bitAnd: 15) + 1).
	].
	^string
</details>

#### ByteArray>>#shortAt: index bigEndian: bigEndian

Return a 16-bit signed integer quantity starting from the given byte index


<details>
	<summary>See more</summary>
	
	shortAt: index bigEndian: bigEndian
	"Return a 16-bit signed integer quantity starting from the given byte index"

	| result |
	result := bigEndian
		ifFalse: [ ((self at: index + 1) bitShift: 8) + (self at: index) ]
		ifTrue: [ ((self at: index) bitShift: 8) + (self at: index + 1) ].
	result < 16r8000 ifTrue: [ ^result ].
	^result - 16r10000
</details>

#### ByteArray>>#defaultElement

<details>
	<summary>See more</summary>
	
	defaultElement

	^0
</details>

#### ByteArray>>#base64Encoded

Encode the receiver as base64


<details>
	<summary>See more</summary>
	
	base64Encoded
	"Encode the receiver as base64"
	"'Hello World' asByteArray base64Encoded"
	^(Base64MimeConverter mimeEncode: self readStream) contents
</details>

#### ByteArray>>#doubleAt: index bigEndian: bool

Return a 64 bit float starting from the given byte index


<details>
	<summary>See more</summary>
	
	doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new. 
	bool
		ifTrue: [
			dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [
			dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl
</details>

#### ByteArray>>#byteAt: index put: value

<details>
	<summary>See more</summary>
	
	byteAt: index put: value
	<primitive: 61>
	^self at: index put: value
</details>

#### ByteArray>>#readHexFrom: aStream

Initialize the receiver from a hexadecimal string representation ByteArray readHexFrom: '1E1e' ByteArray readHexFrom: '1e1E' ByteArray readHexFrom: '1e1e' Note: lowercase hex digits are supported here, because we are reading strictly bytes. But are not really general, because lowercase e is used to denote scientific notation: 16r1e0 = 1 16r1E0 = 480


<details>
	<summary>See more</summary>
	
	readHexFrom: aStream
	"Initialize the receiver from a hexadecimal string representation
		ByteArray readHexFrom: '1E1e'
		ByteArray readHexFrom: '1e1E'
		ByteArray readHexFrom: '1e1e'
	Note: lowercase hex digits are supported here, because we are reading strictly bytes.
	But are not really general, because lowercase e is used to denote scientific notation:
		16r1e0 = 1
		16r1E0 = 480
	"
	| map v ch value |
	map := '0123456789ABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		 (v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := v bitShift: 4.
		ch := aStream next asUppercase.
		v := (map indexOf: ch) - 1.
		(v between: 0 and: 15) ifFalse: [ ^self error: 'Hex digit expected' ].
		value := value + v.
		self at: i put: value.
	]
</details>

#### ByteArray>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

#### ByteArray>>#unsignedLongAt: index bigEndian: bigEndian

Return a 32-bit unsigned integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers.


<details>
	<summary>See more</summary>
	
	unsignedLongAt: index bigEndian: bigEndian
	"Return a 32-bit unsigned integer quantity starting from the given byte index. Use #normalize where necessary to ensure compatibility with non-30-bit SmallIntegers."
	| byte |
	bigEndian ifTrue:
		[((byte := self at: index) <= 16r3F
		 or: [SmallInteger maxVal >  1073741823]) ifTrue:
			[^(((byte bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 3)].
		^(LargePositiveInteger new: 4)
			digitAt: 1 put: (self at: index + 3);
			digitAt: 2 put: (self at: index + 2);
			digitAt: 3 put: (self at: index + 1);
			digitAt: 4 put: byte;
			normalize].
	((byte := self at: index + 3) <= 16r3F
	 or: [SmallInteger maxVal >  1073741823]) ifTrue:
		[^(((byte bitShift: 8) + (self at: index + 2) bitShift: 8) + (self at: index + 1) bitShift: 8) + (self at: index)].
	^(LargePositiveInteger new: 4)
		replaceFrom: 1 to: 4 with: self startingAt: index;
		normalize
</details>

## ColorArray

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ColorArray>>#bytesPerElement

<details>
	<summary>See more</summary>
	
	bytesPerElement

	^4
</details>

#### ColorArray>>#at: index put: aColor

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: aColor
	^super at: index put: (aColor pixelWordForDepth: 32).
</details>

#### ColorArray>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	^(super at: index) asColorOfDepth: 32
</details>

## Float64Array

FloatArrays store 64bit IEEE floating point numbers, i.e. instances of the Float class. Some support is included for subclasses in the style of Balloon3D-Math. Uses the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits. See #floatAt: and #floatAt:put:

### Methods
#### Float64Array>>#loadFrom: srcObject

<details>
	<summary>See more</summary>
	
	loadFrom: srcObject

	self == srcObject ifTrue: [ ^self ].
	self class == srcObject class
		ifTrue: [ self replaceFrom: 1 to: self size with: srcObject startingAt: 1 ]
		ifFalse: [ self privateLoadFrom: srcObject ]
</details>

#### Float64Array>>#privateLoadFrom: srcObject

Load the receiver from the given source object.


<details>
	<summary>See more</summary>
	
	privateLoadFrom: srcObject
	"Load the receiver from the given source object."
	self error:'Cannot load a ', srcObject class name,' into a ', self class name
</details>

#### Float64Array>>#primSubArray: floatArray

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primSubArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].
</details>

#### Float64Array>>#at: index put: aFloat

Store the argument (e.g., 64 bit Float) at the given index


<details>
	<summary>See more</summary>
	
	at: index put: aFloat
	"Store the argument (e.g., 64 bit Float) at the given index"
	^self floatAt: index put: aFloat
</details>

#### Float64Array>>#primMulScalar: scalarValue

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primMulScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].
</details>

#### Float64Array>>#primAddScalar: scalarValue

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primAddScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) + scalarValue].
</details>

#### Float64Array>>#isLiteral

so that #(1 #[1.0 2 3] 5) prints itself


<details>
	<summary>See more</summary>
	
	isLiteral
	"so that 
	#(1 #[1.0 2 3] 5)
	prints itself"
	^self class == Float64Array
</details>

#### Float64Array>>#dot: aFloatVector

Primitive. Return the dot product of the receiver and the argument. Fail if the argument is not of the same size as the receiver.


<details>
	<summary>See more</summary>
	
	dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result
</details>

#### Float64Array>>#+= anObject

<details>
	<summary>See more</summary>
	
	+= anObject
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]
</details>

#### Float64Array>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self storeOn: aStream
</details>

#### Float64Array>>#floatAt: index

Return the element (e.g., 64 bit Float) at the given index. Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits. Allow subclasses to redefine #at:


<details>
	<summary>See more</summary>
	
	floatAt: index
	"Return the element (e.g., 64 bit Float) at the given index.
	Use the same internal representation as Float. I.e. a Float and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:"
	| answer |

	answer _ Float new.
	answer replaceWordsFrom: 1 to: 2 with: self startingAt: index * 2 - 1.
	^answer
</details>

#### Float64Array>>#-= anObject

<details>
	<summary>See more</summary>
	
	-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]
</details>

#### Float64Array>>#size

Return the number of elements in the receiver


<details>
	<summary>See more</summary>
	
	size
	"Return the number of elements in the receiver"
	^super size // 2
</details>

#### Float64Array>>#primMulArray: floatArray

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primMulArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].
</details>

#### Float64Array>>#negated

Negated value of all elements in the collection


<details>
	<summary>See more</summary>
	
	negated

	^self copy *= -1
</details>

#### Float64Array>>#divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

<details>
	<summary>See more</summary>
	
	divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
</details>

#### Float64Array>>#primAddArray: floatArray

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primAddArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)].
</details>

#### Float64Array>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector
</details>

#### Float64Array>>#at: index

Return the element (e.g., 64 bit Float) at the given index


<details>
	<summary>See more</summary>
	
	at: index
	"Return the element (e.g., 64 bit Float) at the given index"
	^self floatAt: index
</details>

#### Float64Array>>#/ anObject

<details>
	<summary>See more</summary>
	
	/ anObject

	^self copy /= anObject
</details>

#### Float64Array>>#bytesPerElement

<details>
	<summary>See more</summary>
	
	bytesPerElement
	^8
</details>

#### Float64Array>>#squaredLength

Return the squared length of the receiver


<details>
	<summary>See more</summary>
	
	squaredLength
	"Return the squared length of the receiver"
	^self dot: self
</details>

#### Float64Array>>#swapWords

This could call #swapHalvesIn64BitWords:


<details>
	<summary>See more</summary>
	
	swapWords
	"This could call #swapHalvesIn64BitWords:"
	| tmp |
	1 to: self size do: [ :i |
		tmp _ self rawBasicAt: i*2.
		self rawBasicAt: i*2 put: (self rawBasicAt: i*2-1).
		self rawBasicAt: i*2-1 put: tmp ]
</details>

#### Float64Array>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream nextPutAll: '#['.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]
</details>

#### Float64Array>>#length

Return the length of the receiver


<details>
	<summary>See more</summary>
	
	length
	"Return the length of the receiver"
	^self squaredLength sqrt
</details>

#### Float64Array>>#interpolatedValueAt: floatIndex

Do a linear interpolation. Gives usual error if argument outside bounds: #[ 4 5 ] asFloatArray interpolatedValueAt: 0.999 #[ 4 5 ] asFloatArray interpolatedValueAt: 1.0 #[ 4 5 ] asFloatArray interpolatedValueAt: 1.5 #[ 4 5 ] asFloatArray interpolatedValueAt: 2.0 #[ 4 5 ] asFloatArray interpolatedValueAt: 2.000001 #[ 4 5 ] asFloatArray interpolatedValueAt: 3


<details>
	<summary>See more</summary>
	
	interpolatedValueAt: floatIndex
	"Do a linear interpolation.
	Gives usual error if argument outside bounds:
	#[ 4 5 ] asFloatArray interpolatedValueAt: 0.999
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.5
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.000001
	#[ 4 5 ] asFloatArray interpolatedValueAt: 3
	"
	| size index0 index1 weight0 weight1 |

	size _ self size.
	index0 _ floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 _ floatIndex - index0.
	weight0 _ 1.0 - weight1.

	index1 _ (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))
</details>

#### Float64Array>>#primSubScalar: scalarValue

It would be nice to have FloatArrayPlugin or equivalent for Float64Array... <primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>


<details>
	<summary>See more</summary>
	
	primSubScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].
</details>

#### Float64Array>>#*= anObject

<details>
	<summary>See more</summary>
	
	*= anObject
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]
</details>

#### Float64Array>>#normalize

Unsafely normalize the receiver in-place (become a unit vector). Div-by-Zero raised if len 0.


<details>
	<summary>See more</summary>
	
	normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveNormalize' module: 'FloatArrayPlugin'>"
	self /= self length.
</details>

#### Float64Array>>#/= anObject

<details>
	<summary>See more</summary>
	
	/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
</details>

#### Float64Array>>#asIEEE32BitPrecisionFloat

<details>
	<summary>See more</summary>
	
	asIEEE32BitPrecisionFloat
	| answer s |
	self class == Float64Array ifFalse: [
		self error: 'please implement' ].
	s _ self size.
	answer _ FloatArray new: s.
	1 to: s do: [ :i | answer at: i put: (self at: i) ].
	^answer
</details>

#### Float64Array>>#interpolateValues: valuesArray at: x

Interpret self as a domain and valuesArray as a function samples.


<details>
	<summary>See more</summary>
	
	interpolateValues: valuesArray at: x
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]
</details>

#### Float64Array>>#divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

It would be nice to have FloatArrayPlugin or equivalent for Float64Array...


<details>
	<summary>See more</summary>
	
	divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array..."
	1 to: self size do:[:i| | dividend quotient |
		dividend _ self at: i.
		quotient _ 
			scalarValue isZero 
				ifTrue: [
					dividend isZero
						ifTrue: indeterminateBlockOrValue
						ifFalse: zeroDivisionBlockOrValue ]
				ifFalse: [dividend / scalarValue].
		self at: i put: quotient]
</details>

#### Float64Array>>#+ anObject

<details>
	<summary>See more</summary>
	
	+ anObject

	^self copy += anObject
</details>

#### Float64Array>>#- anObject

<details>
	<summary>See more</summary>
	
	- anObject

	^self copy -= anObject
</details>

#### Float64Array>>#defaultElement

Return the default element of the receiver


<details>
	<summary>See more</summary>
	
	defaultElement
	"Return the default element of the receiver"
	^0.0
</details>

#### Float64Array>>#floatAt: index put: aNumber

Store the argument (e.g., 64 bit Float) at the given index Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits. Allow subclasses to redefine #at:put:


<details>
	<summary>See more</summary>
	
	floatAt: index put: aNumber
	"Store the argument (e.g., 64 bit Float) at the given index
	Use the same internal representation as BoxedFloat64. I.e. a BoxedFloat64 and a Float64Array of size 1 hold the same bits.
	Allow subclasses to redefine #at:put:"

	"This breaks with SmallFloat64"
	"self replaceWordsFrom: index * 2 - 1 to: index * 2  with: aFloat asFloat startingAt: 1."

	"Float >>basicAt: acts as if Floats were stored in big endian format. Our instances are in platform endianess."
	| aFloat |
	aFloat _ aNumber asFloat.
	Smalltalk isLittleEndian
		ifTrue: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 2).
			self basicAt: index * 2 put: (aFloat basicAt: 1) ]
		ifFalse: [
			self basicAt: index * 2 - 1 put: (aFloat basicAt: 1).
			self basicAt: index * 2 put: (aFloat basicAt: 2) ].
	^aFloat
</details>

#### Float64Array>>#* anObject

<details>
	<summary>See more</summary>
	
	* anObject

	^self copy *= anObject
</details>

#### Float64Array>>#divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

It would be nice to have FloatArrayPlugin or equivalent for Float64Array...


<details>
	<summary>See more</summary>
	
	divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array..."
	1 to: self size do:[:i| | dividend divisor quotient |
		dividend _ self at: i.
		divisor _ floatArray at: i.
		quotient _ 
			divisor isZero 
				ifTrue: [
					dividend isZero
						ifTrue: indeterminateBlockOrValue
						ifFalse: zeroDivisionBlockOrValue ]
				ifFalse: [dividend / divisor].
		self at: i put: quotient]
</details>

#### Float64Array>>#writeOn: aStream

Store self on the argument, aStream. Write bigEndian / PowerPC order.


<details>
	<summary>See more</summary>
	
	writeOn: aStream
	"Store self on the argument, aStream.
	Write bigEndian / PowerPC order."

	aStream nextUnsignedInt32Put: self size bigEndian: true.
	self do: [ :f |
		aStream nextDouble64Put: f bigEndian: true ]
</details>

#### Float64Array>>#replaceWordsFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceWordsFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	self primitiveFailed
</details>

## FloatArray

FloatArrays store 32bit IEEE floating point numbers.

### Methods
#### FloatArray>>#primSubArray: floatArray

<details>
	<summary>See more</summary>
	
	primSubArray: floatArray

	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].
</details>

#### FloatArray>>#at: index put: value

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: value
	^self floatAt: index put: value
</details>

#### FloatArray>>#primMulScalar: scalarValue

<details>
	<summary>See more</summary>
	
	primMulScalar: scalarValue

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].
</details>

#### FloatArray>>#primAddScalar: scalarValue

<details>
	<summary>See more</summary>
	
	primAddScalar: scalarValue

	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + scalarValue].
</details>

#### FloatArray>>#= another

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	self size > 256 ifTrue: [
		self hashQuick = another hashQuick ifFalse: [ ^false ]].
	^self primitiveEqual: another
</details>

#### FloatArray>>#hash

Subclasses might use other methods. However #hashQuick is suggested for very large collections.


<details>
	<summary>See more</summary>
	
	hash
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ self hashFull
</details>

#### FloatArray>>#dot: aFloatVector

Primitive. Return the dot product of the receiver and the argument. Fail if the argument is not of the same size as the receiver.


<details>
	<summary>See more</summary>
	
	dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result
</details>

#### FloatArray>>#+= anObject

<details>
	<summary>See more</summary>
	
	+= anObject
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]
</details>

#### FloatArray>>#primitiveEqual: aFloatArray

<details>
	<summary>See more</summary>
	
	primitiveEqual: aFloatArray 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aFloatArray class == self class ifFalse: [^ false].
	length _ self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true
</details>

#### FloatArray>>#-= anObject

<details>
	<summary>See more</summary>
	
	-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]
</details>

#### FloatArray>>#primMulArray: floatArray

<details>
	<summary>See more</summary>
	
	primMulArray: floatArray

	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].
</details>

#### FloatArray>>#divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

<details>
	<summary>See more</summary>
	
	divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
</details>

#### FloatArray>>#negated

Negated value of all elements in the collection


<details>
	<summary>See more</summary>
	
	negated

	^self copy *= -1
</details>

#### FloatArray>>#floatAt: index

<details>
	<summary>See more</summary>
	
	floatAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
</details>

#### FloatArray>>#primAddArray: floatArray

<details>
	<summary>See more</summary>
	
	primAddArray: floatArray

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)].
</details>

#### FloatArray>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector
</details>

#### FloatArray>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	^self floatAt: index
</details>

#### FloatArray>>#hashFull

<details>
	<summary>See more</summary>
	
	hashFull
	| hash |
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	hash _ (self species hash + self size hash) hashMultiply.
	1 to: self size do: [:i | hash _ (hash + (self basicAt: i)) hashMultiply].
	^hash
</details>

#### FloatArray>>#primDivScalar: scalarValue

This primitive doesn't fail if argument is zero. It fills result with infinity or nan. For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead. #[1.0 2.0 3.141592 0.0] asFloatArray primDivScalar: 0.0. #[1.0 2.0 3.141592 0.0] asFloatArray / 0.0. #[1.0 2.0 3.141592 0.0] asFloatArray divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200


<details>
	<summary>See more</summary>
	
	primDivScalar: scalarValue
	"This primitive doesn't fail if argument is zero. It fills result with infinity or nan.
	For consistency with division with arrays, and general practice, an exception block or value might be used in public protocol. If needed, call directly this method instead.

	#[1.0 2.0 3.141592 0.0] asFloatArray primDivScalar: 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray / 0.0.
	#[1.0 2.0 3.141592 0.0] asFloatArray divideBy: 0.0 ifDivisorZero: -100 ifBothZero: -200
	"
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].
</details>

#### FloatArray>>#/ anObject

<details>
	<summary>See more</summary>
	
	/ anObject

	^self copy /= anObject
</details>

#### FloatArray>>#interpolatedValueAt: floatIndex

Do a linear interpolation. Gives usual error if argument outside bounds: #[ 4 5 ] asFloatArray interpolatedValueAt: 0.999 #[ 4 5 ] asFloatArray interpolatedValueAt: 1.0 #[ 4 5 ] asFloatArray interpolatedValueAt: 1.5 #[ 4 5 ] asFloatArray interpolatedValueAt: 2.0 #[ 4 5 ] asFloatArray interpolatedValueAt: 2.000001 #[ 4 5 ] asFloatArray interpolatedValueAt: 3


<details>
	<summary>See more</summary>
	
	interpolatedValueAt: floatIndex
	"Do a linear interpolation.
	Gives usual error if argument outside bounds:
	#[ 4 5 ] asFloatArray interpolatedValueAt: 0.999
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.5
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.000001
	#[ 4 5 ] asFloatArray interpolatedValueAt: 3
	"
	| size index0 index1 weight0 weight1 |

	size _ self size.
	index0 _ floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 _ floatIndex - index0.
	weight0 _ 1.0 - weight1.

	index1 _ (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))
</details>

#### FloatArray>>#squaredLength

Return the squared length of the receiver


<details>
	<summary>See more</summary>
	
	squaredLength
	"Return the squared length of the receiver"
	^self dot: self
</details>

#### FloatArray>>#length

Return the length of the receiver


<details>
	<summary>See more</summary>
	
	length
	"Return the length of the receiver"
	^self squaredLength sqrt
</details>

#### FloatArray>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #FloatArray or: [ super is: aSymbol ]
</details>

#### FloatArray>>#primSubScalar: scalarValue

<details>
	<summary>See more</summary>
	
	primSubScalar: scalarValue

	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].
</details>

#### FloatArray>>#*= anObject

<details>
	<summary>See more</summary>
	
	*= anObject
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]
</details>

#### FloatArray>>#normalize

Unsafely normalize the receiver in-place (become a unit vector). Div-by-Zero raised if len 0.


<details>
	<summary>See more</summary>
	
	normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."
	<primitive: 'primitiveNormalize' module: 'FloatArrayPlugin'>
	self /= self length.
</details>

#### FloatArray>>#/= anObject

<details>
	<summary>See more</summary>
	
	/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
</details>

#### FloatArray>>#\\= other

<details>
	<summary>See more</summary>
	
	\\= other

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	].


</details>

#### FloatArray>>#sum

Compute the sum of all the elements in the receiver


<details>
	<summary>See more</summary>
	
	sum

	<primitive: 'primitiveSum' module: 'FloatArrayPlugin'>
	^ super sum
</details>

#### FloatArray>>#adaptToNumber: rcvr andSend: selector

If I am involved in arithmetic with a Number. If possible, convert it to a float and perform the (more efficient) primitive operation.


<details>
	<summary>See more</summary>
	
	adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a Number. If possible,
	convert it to a float and perform the (more efficient) primitive operation."
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated += rcvr].
	selector == #/ ifTrue:[
		"DO NOT USE TRIVIAL CODE
			^self reciprocal * rcvr
		BECAUSE OF GRADUAL UNDERFLOW
		self should: (1.0e-39 / (FloatArray with: 1.0e-39)) first < 2."
			^(self class new: self size withAll: rcvr) / self
		].
	^super adaptToNumber: rcvr andSend: selector
</details>

#### FloatArray>>#interpolateValues: valuesArray at: x

Interpret self as a domain and valuesArray as a function samples.


<details>
	<summary>See more</summary>
	
	interpolateValues: valuesArray at: x
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]
</details>

#### FloatArray>>#divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

This primitive doesn't fail if argument is zeros, just fills with infinity or nan


<details>
	<summary>See more</summary>
	
	divideByScalar: scalarValue ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	"This primitive doesn't fail if argument is zeros, just fills with infinity or nan"
	scalarValue isZero ifFalse: [
		^self primDivScalar: scalarValue ].
	1 to: self size do:[:i| | dividend quotient |
		dividend _ self at: i.
		quotient _ dividend isZero
				ifTrue: indeterminateBlockOrValue
				ifFalse: zeroDivisionBlockOrValue.
		self at: i put: quotient]
</details>

#### FloatArray>>#printElementsOn: aStream

FloatArray elements are answered as 64 bit Float, but are really 32 bit Float. When printing, print them as 32 bit Float.


<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	"FloatArray elements are answered as 64 bit Float, but are really 32 bit Float.
	When printing, print them as 32 bit Float."
	aStream nextPut: $(.
	self do: [ :element |
		element printAsIEEE32BitPrecisionFloatOn: aStream base: 10.
		aStream space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)
</details>

#### FloatArray>>#+ anObject

<details>
	<summary>See more</summary>
	
	+ anObject

	^self copy += anObject
</details>

#### FloatArray>>#- anObject

<details>
	<summary>See more</summary>
	
	- anObject

	^self copy -= anObject
</details>

#### FloatArray>>#primDivArray: floatArray

<details>
	<summary>See more</summary>
	
	primDivArray: floatArray

	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>
	^#primitiveFailure
</details>

#### FloatArray>>#defaultElement

Return the default element of the receiver


<details>
	<summary>See more</summary>
	
	defaultElement
	"Return the default element of the receiver"
	^0.0
</details>

#### FloatArray>>#floatAt: index put: value

<details>
	<summary>See more</summary>
	
	floatAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [self basicAt: index put: value asIEEE32BitWord]
		ifFalse: [self floatAt: index put: value asFloat].
	^value
</details>

#### FloatArray>>#* anObject

<details>
	<summary>See more</summary>
	
	* anObject

	^self copy *= anObject
</details>

#### FloatArray>>#divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

<details>
	<summary>See more</summary>
	
	divideByArray: floatArray ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue

	(self primDivArray: floatArray) == #primitiveFailure ifTrue: [
		1 to: self size do:[:i| | dividend divisor quotient |
			dividend _ self at: i.
			divisor _ floatArray at: i.
			quotient _ 
				divisor isZero 
					ifTrue: [
						dividend isZero
							ifTrue: indeterminateBlockOrValue
							ifFalse: zeroDivisionBlockOrValue ]
					ifFalse: [dividend / divisor].
			self at: i put: quotient]]
</details>

#### FloatArray>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

## IntegerArray

IntegerArrays store 32bit signed Integer values, with values between -16r80000000 and 16r7FFFFFFF. Negative values are stored as 2's complement.

### Methods
#### IntegerArray>>#atAllPut: anInteger

Put anObject at every one of the receiver's indices.


<details>
	<summary>See more</summary>
	
	atAllPut: anInteger
	| word |
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self primFill: word.
</details>

#### IntegerArray>>#at: index put: anInteger

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: anInteger
	^self integerAt: index put: anInteger
</details>

#### IntegerArray>>#defaultElement

Return the default element of the receiver


<details>
	<summary>See more</summary>
	
	defaultElement
	"Return the default element of the receiver"
	^0
</details>

#### IntegerArray>>#integerAt: index

Return the integer at the given index


<details>
	<summary>See more</summary>
	
	integerAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word _ self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]
</details>

#### IntegerArray>>#primFill: aPositiveInteger

Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays.


<details>
	<summary>See more</summary>
	
	primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.
</details>

#### IntegerArray>>#integerAt: index put: anInteger

Store the integer at the given index


<details>
	<summary>See more</summary>
	
	integerAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self  basicAt: index put: word.
	^anInteger
</details>

#### IntegerArray>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	^self integerAt: index
</details>

## RunArray

My instances provide space-efficient storage of data which tends to be constant over long runs of the possible indices. Essentially repeated values are stored singly and then associated with a "run" length that denotes the number of consecutive occurrences of the value. My two important variables are runs An array of how many elements are in each run values An array of what the value is over those elements The variables lastIndex, lastRun and lastOffset cache the last access so that streaming through RunArrays is not an N-squared process. Many complexities of access can be bypassed by using the method RunArray withStartStopAndValueDo:

### Methods
#### RunArray>>#addFirst: value

Add value as the first element of the receiver.


<details>
	<summary>See more</summary>
	
	addFirst: value
	"Add value as the first element of the receiver."
	lastIndex _ nil.  "flush access cache"
	(runs size ~= 0 and: [ self canJoin: values first and: value ])
		ifTrue: [
			runs at: 1 put: runs first + 1]
		ifFalse: [
			runs _ {1}, runs.
			values _ {value}, values]
</details>

#### RunArray>>#mapValues: mapBlock

NOTE: only meaningful to an entire set of runs


<details>
	<summary>See more</summary>
	
	mapValues: mapBlock
	"NOTE: only meaningful to an entire set of runs"
	values _ values collect: mapBlock
</details>

#### RunArray>>#reversed

Answer a copy of the receiver with element order reversed.


<details>
	<summary>See more</summary>
	
	reversed

  ^self class runs: runs reversed values: values reversed
</details>

#### RunArray>>#setRuns: newRuns setValues: newValues

<details>
	<summary>See more</summary>
	
	setRuns: newRuns setValues: newValues
	lastIndex _ nil.  "flush access cache"
	runs _ newRuns asArray.
	values _ newValues asArray.
</details>

#### RunArray>>#addLast: value

Add value as the last element of the receiver.


<details>
	<summary>See more</summary>
	
	addLast: value
	"Add value as the last element of the receiver."
	lastIndex _ nil.		"flush access cache"
	(runs size ~= 0 and: [ self canJoin: values last and: value ])
		ifTrue: [
			runs at: runs size put: runs last + 1 ]
		ifFalse: [
			runs _ runs copyWith: 1.
			values _ values copyWith: value ]
</details>

#### RunArray>>#runLengthAt: index

Answer the length remaining in run beginning at index.


<details>
	<summary>See more</summary>
	
	runLengthAt: index 
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]
</details>

#### RunArray>>#runs

<details>
	<summary>See more</summary>
	
	runs

	^runs
</details>

#### RunArray>>#first

Answer the first element of the receiver. Raise an error if the collection is empty.


<details>
	<summary>See more</summary>
	
	first
	^values at: 1
</details>

#### RunArray>>#= otherArray

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= otherArray 
	self == otherArray ifTrue: [ ^ true ].
	
	self species == otherArray species ifFalse: [^ false].

	"Test if all my elements are equal to those of otherArray"
	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
 	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]
</details>

#### RunArray>>#coalesce

Try to combine adjacent runs


<details>
	<summary>See more</summary>
	
	coalesce
	"Try to combine adjacent runs"
	| ind |
	ind _ 2.
	[ ind > values size ] whileFalse: [
		(self canJoin: (values at: ind-1) and: (values at: ind))
			ifFalse: [ ind _ ind + 1 ]
			ifTrue: [		"two are the same, combine them"
				values _ values copyReplaceFrom: ind to: ind with: #().
				runs at: ind-1 put: (runs at: ind-1) + (runs at: ind).
				runs _ runs copyReplaceFrom: ind to: ind with: #() ]]
</details>

#### RunArray>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self printNameOn: aStream.
	aStream
		nextPutAll: ' runs: ';
		print: runs;
		nextPutAll: ' values: ';
		print: values
</details>

#### RunArray>>#copy

Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy.


<details>
	<summary>See more</summary>
	
	copy

	^self copyFrom: 1 to: self size
</details>

#### RunArray>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size
	| size |
	size _ 0.
	1 to: runs size do: [:i | size _ size + (runs at: i)].
	^size
</details>

#### RunArray>>#withStartStopAndValueDo: aBlock

<details>
	<summary>See more</summary>
	
	withStartStopAndValueDo: aBlock
	| start stop |
	start _ 1.
	runs with: values do:
		[:len : val | stop _ start + len - 1.
		aBlock value: start value: stop value: val.
		start _ stop + 1]
		
</details>

#### RunArray>>#copyFrom: start to: stop

Answer a copy of a subset of the receiver, starting from element at index start until element at index stop.


<details>
	<summary>See more</summary>
	
	copyFrom: start to: stop
	| newRuns run1 run2 offset1 offset2 answer | 
	stop < start ifTrue: [
		answer _ RunArray new.
		answer canJoinMessage: canJoinMessage.
		^answer ].
	self at: start setRunOffsetAndValue: [ :r :o :value1 |
		run1 _ r.
		offset1 _ o. 
		value1 ].
	self at: stop setRunOffsetAndValue: [ :r :o :value2 |
		run2 _ r.
		offset2 _ o.
		value2].
	run1 = run2
		ifTrue: [
			newRuns _ Array with: offset2 - offset1 + 1]
		ifFalse: [
			newRuns _ runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1 ].
	answer _ RunArray runs: newRuns values: (values copyFrom: run1 to: run2).
	answer canJoinMessage: canJoinMessage.
	^answer
</details>

#### RunArray>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index

	self at: index setRunOffsetAndValue: [:run :offset :value | ^value]
</details>

#### RunArray>>#at: index setRunOffsetAndValue: aBlock

Supply all run information to aBlock.


<details>
	<summary>See more</summary>
	
	at: index setRunOffsetAndValue: aBlock 
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit _ runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue:  "cache not loaded, or beyond index - start over"
			[run _ 1.
			offset _ index-1]
		ifFalse:  "cache loaded and before index - start at cache"
			[run _ lastRun.
			offset _ lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset _ offset - (runs at: run).
			run _ run + 1].
	lastIndex _ index.  "Load cache for next access"
	lastRun _ run.
	lastOffset _ offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run _ run - 1.
			offset _ offset + (runs at: run)].
	^aBlock
		value: run	"an index into runs and values"
		value: offset	"zero-based offset from beginning of this run"
		value: (values at: run)	"value for this run"
</details>

#### RunArray>>#runsAndValuesDo: aBlock

Evaluate aBlock with run lengths and values from the receiver


<details>
	<summary>See more</summary>
	
	runsAndValuesDo: aBlock
	"Evaluate aBlock with run lengths and values from the receiver"
	^runs with: values do: aBlock.
</details>

#### RunArray>>#canJoin: aValue and: anotherValue

<details>
	<summary>See more</summary>
	
	canJoin: aValue and: anotherValue
	^ canJoinMessage
		ifNil: [ aValue = anotherValue ]
		ifNotNil: [
			canJoinMessage valueWithArguments: {aValue. anotherValue} ]
</details>

#### RunArray>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)
</details>

#### RunArray>>#add: newObject

Adding to an Interval is not allowed.


<details>
	<summary>See more</summary>
	
	add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement
</details>

#### RunArray>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #RunArray or: [ super is: aSymbol ]
</details>

#### RunArray>>#values

Answer the values in the receiver.


<details>
	<summary>See more</summary>
	
	values
	"Answer the values in the receiver."

	^values
</details>

#### RunArray>>#canJoinMessage: aMessageSend

<details>
	<summary>See more</summary>
	
	canJoinMessage: aMessageSend
	canJoinMessage _ aMessageSend
</details>

#### RunArray>>#copyReplaceFrom: start to: stop with: replacement

Answer a copy of the receiver satisfying the following conditions: + stop is less than start, then this is an insertion; stop should be exactly start-1, + start = 1 means insert before the first character, + start = size+1 means append after last character. + Otherwise, this is a replacement; start and stop have to be within the receiver's bounds.


<details>
	<summary>See more</summary>
	
	copyReplaceFrom: start to: stop with: replacement

	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)
</details>

#### RunArray>>#last

Answer the last element of the receiver. Raise an error if the collection is empty.


<details>
	<summary>See more</summary>
	
	last
	^values at: values size
</details>

#### RunArray>>#find: attribute

Return the first interval over which this attribute applies


<details>
	<summary>See more</summary>
	
	find: attribute
	"Return the first interval over which this attribute applies"
	| begin end |
	begin _ 0.
	self withStartStopAndValueDo: [ :start :stop :attributes |
		(attributes includes: attribute)
			ifTrue: [
				begin = 0 ifTrue: [begin _ start].
				end _ stop]
			ifFalse: [
				begin > 0 ifTrue: [^ begin to: end]]].
	begin > 0 ifTrue: [^ begin to: end].
	^ nil
</details>

#### RunArray>>#rangeOf: attr startingAt: startPos

Answer an interval that gives the range of attr at index position startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos. self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self. Note that an attribute may span several adjancent runs.


<details>
	<summary>See more</summary>
	
	rangeOf: attr startingAt: startPos
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.
Note that an attribute may span several adjancent runs. "

	self at: startPos 
		setRunOffsetAndValue: 
            [:run :offset :value | 
               ^(value includes: attr)
                  ifFalse: [startPos to: startPos - 1]
                  ifTrue:
                    [ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |
                     lastRelevantPosition := startPos - offset + (runs at: run) - 1.
                     firstRelevantPosition := startPos - offset.
                     idxOfCandidateRun := run + 1.
                     [idxOfCandidateRun <= runs size 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun + 1]. 
                     idxOfCandidateRun := run - 1.
                     [idxOfCandidateRun >= 1 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun - 1]. 
 
                    firstRelevantPosition to: lastRelevantPosition]
		  ]
</details>

#### RunArray>>#basicReplaceAttributesFrom: start to: stop with: replacement

Private. Does not enforce invariants. replacement size = (stop-start-1)


<details>
	<summary>See more</summary>
	
	basicReplaceAttributesFrom: start to: stop with: replacement
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	| answer |
	answer _ self
		copyReplaceFrom: start
		to: stop
		with: replacement.
	answer coalesce.
	^ answer
</details>

#### RunArray>>#writeOn: aStream

<details>
	<summary>See more</summary>
	
	writeOn: aStream

	aStream nextUnsignedInt16Put: runs size bigEndian: true.
	1 to: runs size do: [ :x |
		aStream nextUnsignedInt16Put: (runs at: x) bigEndian: true.
		aStream nextUnsignedInt16Put: (values at: x) bigEndian: true ]
</details>

#### RunArray>>#runsFrom: start to: stop do: aBlock

Evaluate aBlock with all existing runs in the range from start to stop


<details>
	<summary>See more</summary>
	
	runsFrom: start to: stop do: aBlock
	"Evaluate aBlock with all existing runs in the range from start to stop"
	| run value index |
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue:[:firstRun :offset :firstValue|
		run _ firstRun.
		value _ firstValue.
		index _ start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run _ run + 1.
			value _ values at: run.
			index _ index + (runs at: run)]].

</details>

#### RunArray>>#, aRunArray

Answer a new RunArray that is a concatenation of the receiver and aRunArray.


<details>
	<summary>See more</summary>
	
	, aRunArray 
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns answer |
	(aRunArray isMemberOf: RunArray)
		ifFalse: [
			new _ self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	runs size = 0 ifTrue: [^aRunArray copy].
	aRunArray runs size = 0 ifTrue: [^self copy].
	(self canJoin: (values at: values size) and: (aRunArray values at: 1))
		ifFalse: [ 
			answer _ RunArray
				runs: runs , aRunArray runs
				values: values , aRunArray values.
			answer canJoinMessage: canJoinMessage.
			^answer ].
	newRuns _ runs
					copyReplaceFrom: runs size
					to: runs size
					with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	answer _ RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values).
	answer canJoinMessage: canJoinMessage.
	^answer
</details>

## RunNotArray

A replacement for RunArray that does not optimize space. Essentially just an array, with a few idiosyncratic methods for compatibility with RunArray. Rationale: When styling Smalltalk text, runs are very short. Space saving is not significant. Maybe 90% if the time is spent building and coaslescing RunArrays. So, don't save space. Save time!

### Methods
#### RunNotArray>>#runsAndValuesDo: aBlock

Evaluate aBlock with run lengths and values from the receiver


<details>
	<summary>See more</summary>
	
	runsAndValuesDo: aBlock
	"Evaluate aBlock with run lengths and values from the receiver"
	self do: [ :attributes |
		aBlock value: 1 value: attributes ]
</details>

#### RunNotArray>>#mapValues: mapBlock

<details>
	<summary>See more</summary>
	
	mapValues: mapBlock
	self withIndexDo: [ :each :i |
		self at: i put: (mapBlock value: each) ]
</details>

#### RunNotArray>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Copied from Array


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Copied from Array"
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

#### RunNotArray>>#find: attribute

Return the first interval over which this attribute applies


<details>
	<summary>See more</summary>
	
	find: attribute
	"Return the first interval over which this attribute applies"

	1 to: self size do: [ :i |
		((self at: i) includes: attribute) ifTrue: [
			^ self rangeOf: attribute startingAt: i ]].
	
	^ nil
</details>

#### RunNotArray>>#rangeOf: attribute startingAt: startPos

Answer an interval that gives the range of attr at index position startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos. self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.


<details>
	<summary>See more</summary>
	
	rangeOf: attribute startingAt: startPos
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self."
	
	| firstRelevantPosition lastRelevantPosition value |
	value _ self at: startPos.
	(value includes: attribute) ifFalse: [
		^ startPos to: startPos - 1].

	lastRelevantPosition := startPos +1.
	[lastRelevantPosition <= self size 
		and: [(self at: lastRelevantPosition) includes: attribute]]
	whileTrue: [
		lastRelevantPosition := lastRelevantPosition + 1 ].

	firstRelevantPosition := startPos -1.
	[firstRelevantPosition >= 1 
		and: [(self at: firstRelevantPosition) includes: attribute]]
	whileTrue: [
		firstRelevantPosition := firstRelevantPosition - 1]. 
 
	^firstRelevantPosition+1 to: lastRelevantPosition-1
</details>

#### RunNotArray>>#values

yep. self


<details>
	<summary>See more</summary>
	
	values
	"yep. self"
</details>

#### RunNotArray>>#canJoinMessage: aMessageSend

Nop. We never try to join.


<details>
	<summary>See more</summary>
	
	canJoinMessage: aMessageSend
	"Nop. We never try to join."
</details>

#### RunNotArray>>#basicReplaceAttributesFrom: start to: stop with: replacement

<details>
	<summary>See more</summary>
	
	basicReplaceAttributesFrom: start to: stop with: replacement

	self replaceFrom: start to: stop with: replacement startingAt: 1 
</details>

#### RunNotArray>>#runLengthAt: i0

({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 1. ({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 2. ({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 3. ({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 4. ({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 5.


<details>
	<summary>See more</summary>
	
	runLengthAt: i0
	"
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 1.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 2.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 3.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 4.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 5.
	"

	| value |
	value _ self at: i0.
	i0+1 to: self size do: [ :i |
		(self at: i) = value ifFalse: [ ^ i-i0 ]].
	^ self size-i0+1
		
</details>

#### RunNotArray>>#at: index

Tolerates index=0 and index=size+1 for copyReplace:


<details>
	<summary>See more</summary>
	
	at: index
	"Tolerates index=0 and index=size+1 for copyReplace: "
	index = 0 ifTrue: [ ^ self at: 1 ].
	index = (self size+1) ifTrue: [ ^ self at: index-1 ].
	^ super at: index
</details>

#### RunNotArray>>#coalesce

Nop. We never combine adjacent runs


<details>
	<summary>See more</summary>
	
	coalesce
	"Nop. We never combine adjacent runs"
</details>

#### RunNotArray>>#runsFrom: start to: stop do: aBlock

<details>
	<summary>See more</summary>
	
	runsFrom: start to: stop do: aBlock
	start to: stop do: [ :i | aBlock value: (self at: i )]
</details>

## WordArray

WordArrays store 32-bit unsigned Integer values, between 0 and 16rFFFFFFFF.

### Methods
#### WordArray>>#bytesPerElement

Number of bytes in each item. This multiplied by (self size)*8 gives the number of bits stored.


<details>
	<summary>See more</summary>
	
	bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 4
</details>

#### WordArray>>#atAllPut: value

Fill the receiver with the given value


<details>
	<summary>See more</summary>
	
	atAllPut: value
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
</details>

#### WordArray>>#bytesAt: index

Answer a ByteArray of 4 elements. The 32-bit word is split in 4 bytes, in little endian format WordArray with: 16rFF32791B :: bytesAt: 1 :: hex


<details>
	<summary>See more</summary>
	
	bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	WordArray with: 16rFF32791B :: bytesAt: 1 :: hex
	"

	| bytes word |
	bytes _ ByteArray new: 4.
	word _ self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes
</details>

#### WordArray>>#defaultElement

Return the default element of the receiver


<details>
	<summary>See more</summary>
	
	defaultElement
	"Return the default element of the receiver"
	^0
</details>

#### WordArray>>#bytesAt: index put: aByteArray

Takes a ByteArray of 4 elements. Store the 32-bit word made with those byes, in little endian format WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex


<details>
	<summary>See more</summary>
	
	bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex
	"

	| word |
	word _ 0.
	4 to: 1 by: -1 do: [ :i | word _ word * 256 + (aByteArray at: i) ].
	self at: index put: word
</details>

#### WordArray>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self size * 4
</details>

#### WordArray>>#replaceFrom: start to: stop with: replacement startingAt: repStart

This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the sequenceable collection, replacementCollection. Answer the receiver. No range checks are performed.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart 
</details>

