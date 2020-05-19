## ArrayedCollection

I am an abstract superclass for collections of elements with the same storage size. My subclasses typically allocate storage as fixed size blocks. Element access is by an integer index, from 1 to collection size. Usual accessors are #at: and #at:put: They are declared as having indexable instances. See: #variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: #variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: #variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: #weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: This means that they don't have instance variables. They are what is usually called an Array: a contiguous area of memory, that stores ObjectPointers (i.e. references to any objects), Bytes or 32-bit Words. Each class is free to use that storarge to encode other kinds of data. See, for example, FloatArray Float64Array or ShortIntegerArray. Therefore the size in bytes of each element is not constrained, although it is fixed for each class. Instance size is fixed at creation. They can not grow or shrink. For these reasons, there are many classes in the system that have Array like acessors (#at: and #at:put: with integer indexes) but are not in the ArrayedCollection hierarchy. These include OrderedCollection, Interval, Text, etc.

### Methods
#### ArrayedCollection>>#bytesPerElement

<details>
	<summary>See more</summary>
	
	bytesPerElement
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ].

</details>

#### ArrayedCollection>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
</details>

#### ArrayedCollection>>#add: newObject

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: newObject

	self shouldNotImplement
</details>

#### ArrayedCollection>>#mergeSortFrom: startIndex to: stopIndex by: aBlock

Sort the given range of indices using the mergesort algorithm. Mergesort is a worst-case O(N log N) sorting algorithm that usually does only half as many comparisons as heapsort or quicksort.


<details>
	<summary>See more</summary>
	
	mergeSortFrom: startIndex to: stopIndex by: aBlock
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra 
	copy of the data is used as temporary storage and successive merge 
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	self assert: [startIndex >= 1 and: [startIndex < stopIndex]]. "bad start index"
	self assert: [stopIndex <= self size]. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex 
		src: self copy 
		dst: self 
		by: aBlock
</details>

#### ArrayedCollection>>#storeElementsFrom: firstIndex to: lastIndex on: aStream

<details>
	<summary>See more</summary>
	
	storeElementsFrom: firstIndex to: lastIndex on: aStream

	| noneYet defaultElement arrayElement |
	noneYet _ true.
	defaultElement _ self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement _ self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet _ false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
</details>

#### ArrayedCollection>>#bytesPerBasicElement

Answer the number of bytes that each of my basic elements requires. In other words: self basicSize * self bytesPerBasicElement should equal the space required on disk by my variable sized representation.


<details>
	<summary>See more</summary>
	
	bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]
</details>

#### ArrayedCollection>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self basicSize * self bytesPerBasicElement

</details>

#### ArrayedCollection>>#isSorted

Return true if the receiver is sorted by the given criterion. Optimization for isSortedBy: [:a :b | a <= b].


<details>
	<summary>See more</summary>
	
	isSorted
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm _ self first.
	2 to: self size do: 
		[:index | 
		elm _ self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm _ elm].
	^ true
</details>

#### ArrayedCollection>>#quickSortFrom: from to: to by: sortBlock

Sort elements i through j of self to be nondescending according to sortBlock using an in-place quicksort with simple median-of-three partitioning with guaranteed O(log(n)) space usage.


<details>
	<summary>See more</summary>
	
	quickSortFrom: from to: to by: sortBlock
	"Sort elements i through j of self to be nondescending according to sortBlock using an in-place quicksort with simple median-of-three partitioning with guaranteed O(log(n)) space usage."

	| i j |
	i := from.
	j := to.
	[
		| k l dij temp ij di dj n |
		"The prefix d means the data at that index."
		"Sort di,dj."
		di := self at: i.
		dj := self at: j.
		(sortBlock ifNil: [ di <= dj ] ifNotNil: [ sortBlock value: di value: dj ]) ifFalse: [
			temp := self at: i. self at: i put: (self at: j); at: j put: temp.
			temp := di. di := dj. dj := temp ].
		(n := j + 1  - i) <= 2 ifTrue: [ ^self ].
		"More than two elements."
		ij := (i + j) // 2.  "ij is the midpoint of i and j."
		dij := self at: ij.  "Sort di,dij,dj.  Make dij be their median."
		(sortBlock ifNil: [ di <= dij ] ifNotNil: [ sortBlock value: di value: dij ])
			ifTrue: [
				(sortBlock ifNil: [ dij <= dj ] ifNotNil: [ sortBlock value: dij value: dj ]) ifFalse: [
					temp := self at: j. self at: j put: (self at: ij); at: ij put: temp.
					dij := dj ] ]
			ifFalse: [
				temp := self at: i. self at: i put: (self at: ij); at: ij put: temp.
				dij := di ].
		n = 3 ifTrue: [ ^self ].
		"More than three elements."
		"Find k>i and l<j such that dk,dij,dl are in reverse order.
		Swap k and l.  Repeat this procedure until k and l pass each other."
		k := i.
		l := j.
		[
			[ k <= (l := l - 1) and: [ 
				sortBlock ifNil: [ dij <= (self at: l) ] ifNotNil: [ sortBlock value: dij value: (self at: l) ] ] ] whileTrue.  "i.e. while dl succeeds dij"
			[ (k := k + 1) <= l and: [
				sortBlock ifNil: [ (self at: k) <= dij ] ifNotNil: [ sortBlock value: (self at: k) value: dij ] ] ] whileTrue.  "i.e. while dij succeeds dk"
			k <= l ] whileTrue: [ temp := self at: k. self at: k put: (self at: l); at: l put: temp. ].
		"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk through dj. Sort the larger segment in this method and call another quicksort for the smaller segment. This ensures O(log(n)) space usage."
		i < l 
			ifFalse: [
				k < j
					ifFalse: [ ^self ]
					ifTrue: [ i := k ] ]
			ifTrue: [
				k < j
					ifFalse: [ j := l ]
					ifTrue: [
						l - i <  (j - k)
							ifTrue: [ 
								self quickSortFrom: i to: l by: sortBlock.
								i := k ]
							ifFalse: [
								self quickSortFrom: k to: j by: sortBlock.
								j := l ] ] ] ] repeat
</details>

#### ArrayedCollection>>#isSortedBy: aBlock

Return true if the receiver is sorted by the given criterion.


<details>
	<summary>See more</summary>
	
	isSortedBy: aBlock
	"Return true if the receiver is sorted by the given criterion."
	| lastEl el |
	self isEmpty ifTrue:[^true].
	lastEl _ self first.
	2 to: self size do: [:i |
		el _ self at: i.
		(aBlock value: lastEl value: el) ifFalse:[^false].
		lastEl _ el].
	^ true

</details>

#### ArrayedCollection>>#size

Primitive. Answer the number of indexable fields in the receiver. This value is the same as the largest legal subscript. Primitive is specified here to override SequenceableCollection size. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size
	"Primitive. Answer the number of indexable fields in the receiver. This
	value is the same as the largest legal subscript. Primitive is specified
	here to override SequenceableCollection size. Essential. See Object
	documentation whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize
</details>

#### ArrayedCollection>>#defaultElement

<details>
	<summary>See more</summary>
	
	defaultElement

	^nil
</details>

#### ArrayedCollection>>#sort

Sort this array into ascending order using the '<=' operator.


<details>
	<summary>See more</summary>
	
	sort
	"Sort this array into ascending order using the '<=' operator."

	self sort: nil
</details>

#### ArrayedCollection>>#insert: anObject shiftingRightAt: anInsertionIndex

Inserts anObject at anInsertionIndex, moving right object between anInsertionIndex and self size, loosing last object. Example: #(0 1 3 4 5) insert: 2 shiftingRightAt: 3 returns: #(0 1 2 3 4)


<details>
	<summary>See more</summary>
	
	insert: anObject shiftingRightAt: anInsertionIndex

	"Inserts anObject at anInsertionIndex, moving right object between anInsertionIndex and self size, 
	loosing last object. Example: 
	#(0 1 3 4 5) insert: 2 shiftingRightAt: 3
	returns:  #(0 1 2 3 4) 
	" 
	| currentIndex |
	
	self assertIsInBounds: anInsertionIndex.
	currentIndex _ self size .
	
	[currentIndex > anInsertionIndex] whileTrue: [
		self at: currentIndex put: (self at: currentIndex-1).
		currentIndex _ currentIndex - 1].
	
	self at: anInsertionIndex put: anObject

</details>

#### ArrayedCollection>>#mergeSortFrom: first to: last src: src dst: dst by: aBlock

Private! Split the range to be sorted in half, sort each half, and merge the two half-ranges into dst.


<details>
	<summary>See more</summary>
	
	mergeSortFrom: first to: last src: src dst: dst by: aBlock
	"Private! Split the range to be sorted in half, sort each half, and merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle _ (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock.

</details>

#### ArrayedCollection>>#sort: aBlock

Sort this array using the given comparision block. The block should take two arguments and return true if the first element should precede the second in the sorted result.


<details>
	<summary>See more</summary>
	
	sort: aBlock
	"Sort this array using the given comparision block. The block should take two arguments and return true if the first element should precede the second in the sorted result."

	self mergeSortFrom: 1 to: self size by: aBlock.

</details>

#### ArrayedCollection>>#restoreEndianness

This word object was just read in from a stream. It was stored in Big Endian (Mac) format. Reverse the byte order if the current machine is Little Endian. We only intend this for non-pointer arrays. Do nothing if I contain pointers.


<details>
	<summary>See more</summary>
	
	restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^ self].

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]
</details>

#### ArrayedCollection>>#writeOn: aStream

Store the array of bits onto the argument, aStream. (leading byte ~= 16r80) identifies this as raw bits (uncompressed). Always store in Big Endian (Mac) byte order. Do the writing at BitBlt speeds. We only intend this for non-pointer arrays. Do nothing if I contain pointers.


<details>
	<summary>See more</summary>
	
	writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	self class isPointers | self class isWords not ifTrue: [^ super writeOn: aStream].
				"super may cause an error, but will not be called."
	aStream nextSignedInt32Put: self basicSize bigEndian: true.
	aStream nextWordsPutAll: self.
</details>

#### ArrayedCollection>>#mergeFirst: first middle: middle last: last into: dst by: aBlock

Private. Merge the sorted ranges [first..middle] and [middle+1..last] of the receiver into the range [first..last] of dst.


<details>
	<summary>See more</summary>
	
	mergeFirst: first middle: middle last: last into: dst by: aBlock
	"Private. Merge the sorted ranges [first..middle] and [middle+1..last] 
	of the receiver into the range [first..last] of dst."

	| i1 i2 val1 val2 out |
	i1 := first.
	i2 := middle + 1.
	val1 := self at: i1.
	val2 := self at: i2.
	out := first - 1.  "will be pre-incremented"

	"select 'lower' half of the elements based on comparator"
	[ (i1 <= middle) and: [ i2 <= last ] ] whileTrue: 	[
		(aBlock 
			ifNil: [ val1 <= val2 ]
			ifNotNil: [ aBlock value: val1 value: val2 ])
				ifTrue: [
					dst at: (out := out + 1) put: val1.
					val1 := self at: (i1 := i1 + 1)]
				ifFalse: [
					dst at: (out := out + 1) put: val2.
					(i2 := i2 + 1) <= last ifTrue: [
						val2 := self at: i2 ] ] ].

	"copy the remaining elements"
	i1 <= middle
		ifTrue: [dst replaceFrom: out + 1 to: last with: self startingAt: i1]
		ifFalse: [dst replaceFrom: out + 1 to: last with: self startingAt: i2]
</details>

## Collection

I am the abstract superclass of all classes that represent a group of elements.

### Methods
#### Collection>>#asIntegerArray

Answer an IntegerArray whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asIntegerArray
	"Answer an IntegerArray whose elements are the elements of the receiver"

	^self as: IntegerArray
</details>

#### Collection>>#detectMax: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Answer the element for which aBlock evaluates to the highest magnitude. If collection empty, return nil. This method might also be called elect:.


<details>
	<summary>See more</summary>
	
	detectMax: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	| maxElement maxValue val |
	self do: [ :each | 
		maxValue
			ifNil: ["first element"
				maxElement _ each.
				maxValue _ aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				(val _ aBlock value: each) > maxValue ifTrue: [
					maxElement _ each.
					maxValue _ val]]].
	^ maxElement
</details>

#### Collection>>#adaptToPoint: rcvr andSend: selector

If I am involved in arithmetic with a scalar, return a Collection of the results of each element combined with the scalar in that expression.


<details>
	<summary>See more</summary>
	
	adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
</details>

#### Collection>>#truncated

<details>
	<summary>See more</summary>
	
	truncated
	^ self collect: [:a | a truncated]
</details>

#### Collection>>#product: aBlock

This is implemented using a variant of the normal inject:into: pattern. The reason for this is that it is not known whether we're in the normal number line, i.e. whether 1 is a good initial value for the product.


<details>
	<summary>See more</summary>
	
	product: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 1 is a good initial value for the product."
	^self collect: aBlock andFold: [ :a :b | a * b ]
</details>

#### Collection>>#notEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	notEmpty
	"Answer whether the receiver contains any elements."

	^ self isEmpty not
</details>

#### Collection>>#errorNotFound: anObject

Actually, this should raise a special Exception not just an error.


<details>
	<summary>See more</summary>
	
	errorNotFound: anObject
	"Actually, this should raise a special Exception not just an error."

	self error: self class objectNotFoundDescription
</details>

#### Collection>>#sum: aBlock ifEmpty: emptyBlockOrValue

This is implemented using a variant of the normal inject:into: pattern. The reason for this is that it is not known whether we're in the normal number line, i.e. whether 0 is a good initial value for the sum. Consider a collection of measurement objects, 0 would be the unitless value and would not be appropriate to add with the unit-ed objects.


<details>
	<summary>See more</summary>
	
	sum: aBlock ifEmpty: emptyBlockOrValue
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ] ifEmpty: emptyBlockOrValue
</details>

#### Collection>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Append a sequence of characters that identify the receiver to aStream."

	self printNameOn: aStream.
	self printElementsOn: aStream
</details>

#### Collection>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size
	"Answer how many elements the receiver contains."

	| tally |
	tally _ 0.
	self do: [:each | tally _ tally + 1].
	^tally
</details>

#### Collection>>#reject: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect into a new collection like the receiver only those elements for which aBlock evaluates to false. Answer the new collection.


<details>
	<summary>See more</summary>
	
	reject: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver only those elements for 
	which aBlock evaluates to false. Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]
</details>

#### Collection>>#truncateTo: quantum

<details>
	<summary>See more</summary>
	
	truncateTo: quantum
	^self collect: [ :each | each truncateTo: quantum ]
</details>

#### Collection>>#sum: aBlock

This is implemented using a variant of the normal inject:into: pattern. The reason for this is that it is not known whether we're in the normal number line, i.e. whether 0 is a good initial value for the sum. Consider a collection of measurement objects, 0 would be the unitless value and would not be appropriate to add with the unit-ed objects.


<details>
	<summary>See more</summary>
	
	sum: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	^self collect: aBlock andFold: [ :a :b | a + b ]
</details>

#### Collection>>#sin

<details>
	<summary>See more</summary>
	
	sin
	^self collect: [:each | each sin]
</details>

#### Collection>>#difference: aCollection

Answer the set theoretic difference of two collections.


<details>
	<summary>See more</summary>
	
	difference: aCollection
	"Answer the set theoretic difference of two collections."

	^ self reject: [:each | aCollection includes: each]
</details>

#### Collection>>#log

<details>
	<summary>See more</summary>
	
	log
	^ self collect: [:each | each log]
</details>

#### Collection>>#noneSatisfy: aBlock

Evaluate aBlock with the elements of the receiver. If aBlock returns false for all elements return true. Otherwise return false


<details>
	<summary>See more</summary>
	
	noneSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for all elements return true.
	Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
</details>

#### Collection>>#range

<details>
	<summary>See more</summary>
	
	range
	^ self max - self min
</details>

#### Collection>>#average: aBlock ifEmpty: emptyBlock

<details>
	<summary>See more</summary>
	
	average: aBlock ifEmpty: emptyBlock 
	
	^ (self sum: aBlock ifEmpty: [ ^emptyBlock value ]) / self size
</details>

#### Collection>>#removeAllFoundIn: aCollection

Remove each element of aCollection which is present in the receiver from the receiver. Answer aCollection. No error is raised if an element isn't found. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	removeAllFoundIn: aCollection 
	"Remove each element of aCollection which is present in the receiver 
	from the receiver. Answer aCollection. No error is raised if an element
	isn't found. ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each ifAbsent: nil].
	^ aCollection
</details>

#### Collection>>#sorted

Return a new sequenceable collection which contains the same elements as self but its elements are sorted


<details>
	<summary>See more</summary>
	
	sorted
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted "

	^self sorted: nil
</details>

#### Collection>>#sorted: aSortBlockOrNil

Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison.


<details>
	<summary>See more</summary>
	
	sorted: aSortBlockOrNil
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison."

	^self asNewArray sort: aSortBlockOrNil
</details>

#### Collection>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."
	"Redefined here just for reference. See inheritance.
	#collect: avoids using #species in String, when there are non-Character objects
	#select: and #copy avoid using it in SortedCollection"

	^ self class
</details>

#### Collection>>#- arg

<details>
	<summary>See more</summary>
	
	- arg

	^ arg adaptToCollection: self andSend: #-
</details>

#### Collection>>#includesSubstringAnywhere: testString

Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring


<details>
	<summary>See more</summary>
	
	includesSubstringAnywhere: testString
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
	self do: [ :element |
		element isString
			ifTrue: [
				(element includesSubString: testString) ifTrue: [^ true]].
		element isCollection
			ifTrue: [
				(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false

"#(first (second third) ((allSentMessages ('Elvis' includes:)))) includesSubstringAnywhere:  'lvi'"
</details>

#### Collection>>#reciprocal

Return the receiver full of reciprocated elements


<details>
	<summary>See more</summary>
	
	reciprocal
	"Return the receiver full of reciprocated elements"
	^ self collect: [:a | a reciprocal]
</details>

#### Collection>>#union: aCollection

Answer the set theoretic union of two collections.


<details>
	<summary>See more</summary>
	
	union: aCollection
	"Answer the set theoretic union of two collections."

	^ self asSet addAll: aCollection; yourself
</details>

#### Collection>>#copyWith: newElement

Answer a new collection with newElement added (as last element if sequenceable).


<details>
	<summary>See more</summary>
	
	copyWith: newElement
	"Answer a new collection with newElement added (as last
	element if sequenceable)."

	^ self copy
		add: newElement;
		yourself
</details>

#### Collection>>#select: selectBlock thenCollect: collectBlock

<details>
	<summary>See more</summary>
	
	select: selectBlock thenCollect: collectBlock
	^ (self select: selectBlock) collect: collectBlock
</details>

#### Collection>>#tan

<details>
	<summary>See more</summary>
	
	tan
	^self collect: [:each | each tan]
</details>

#### Collection>>#ifNotEmpty: aBlock

Evaluate the block unless I'm empty '' ifNotEmpty: [ :c | c, 'coda' ] '' '' ifNotEmpty: [ 'replacement' ] '' 'stuff' ifNotEmpty: [ :c | c, 'coda' ] 'stuffcoda' 'stuff' ifNotEmpty: [ 'replacement' ] 'replacement' |c|c _ OrderedCollection new. c ifNotEmpty: [c add: 'stuff']. c an OrderedCollection() |c|c _ OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection() |c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [c add: 'stuff']. c an OrderedCollection('a' 'stuff') |c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection('a' 'stuff')


<details>
	<summary>See more</summary>
	
	ifNotEmpty: aBlock
	"Evaluate the block unless I'm empty
	'' ifNotEmpty: [ :c | c, 'coda' ] 						 ''
	'' ifNotEmpty: [ 'replacement' ]					 	 ''
	'stuff' ifNotEmpty: [ :c | c, 'coda' ] 					 'stuffcoda'
	'stuff' ifNotEmpty: [ 'replacement' ]				 	 'replacement'
	|c|c _ OrderedCollection new. c ifNotEmpty: [c add: 'stuff']. c			  an OrderedCollection()
	|c|c _ OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff']. c	  an OrderedCollection()
	|c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [c add: 'stuff']. c		  an OrderedCollection('a' 'stuff')
	|c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff']. c   an OrderedCollection('a' 'stuff')
	"

	self isEmpty ifFalse: [^aBlock valueWithPossibleArgument: self]
</details>

#### Collection>>#max: aBlock

Answer the maximum of evaluating aBlock on all the elements of the receiver.


<details>
	<summary>See more</summary>
	
	max: aBlock
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer
</details>

#### Collection>>#remove: oldObject

Remove oldObject from the receiver's elements. Answer oldObject unless no element is equal to oldObject, in which case, raise an error. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: oldObject 
	"Remove oldObject from the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, raise an error.
	ArrayedCollections cannot respond to this message."

	^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]
</details>

#### Collection>>#fold: aTwoArgBlock ifEmpty: emptyBlockOrValue

Evaluate the block with the first two elements of the receiver, then with the result of the first evaluation and the next element, and so on. Answer the result of the final evaluation. If the receiver is empty, raise an error. If the receiver has a single element, answer that element.


<details>
	<summary>See more</summary>
	
	fold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: [ :coll | coll errorEmptyCollection ]
	#() fold: [:a :b | a, ' ', b] ifEmpty: 7
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: emptyBlockOrValue
</details>

#### Collection>>#min

<details>
	<summary>See more</summary>
	
	min
	^ self inject: self anyOne into: [:min :each | min min: each]
</details>

#### Collection>>#emptyCheck

<details>
	<summary>See more</summary>
	
	emptyCheck

	self isEmpty ifTrue: [self errorEmptyCollection]
</details>

#### Collection>>#errorEmptyCollection

<details>
	<summary>See more</summary>
	
	errorEmptyCollection

	self error: self class emptyCollectionDescription
</details>

#### Collection>>#product

Compute the product of all the elements in the receiver


<details>
	<summary>See more</summary>
	
	product
	"Compute the product of all the elements in the receiver"

	^self fold: [ :a :b | a * b] ifEmpty: 1
</details>

#### Collection>>#asSortedCollection

Answer a SortedCollection whose elements are the elements of the receiver. The sort order is the default less than or equal.


<details>
	<summary>See more</summary>
	
	asSortedCollection
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SortedCollection
</details>

#### Collection>>#= other

Default implementation. Usually redefined in subclasses.


<details>
	<summary>See more</summary>
	
	= other
	"Default implementation. Usually redefined in subclasses."

	^self == other
</details>

#### Collection>>#do: aBlock without: anItem

Enumerate all elements in the receiver. Execute aBlock for those elements that are not equal to the given item


<details>
	<summary>See more</summary>
	
	do: aBlock without: anItem
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	^self do:[:el| anItem = el ifFalse:[aBlock value: el]].
</details>

#### Collection>>#asArray

Answer an Array whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asArray
	"Answer an Array whose elements are the elements of the receiver"

	^self asNewArray
</details>

#### Collection>>#raisedTo: arg

<details>
	<summary>See more</summary>
	
	raisedTo: arg

	^ arg adaptToCollection: self andSend: #raisedTo:
</details>

#### Collection>>#addAll: aCollection

Include all the elements of aCollection as the receiver's elements. Answer aCollection.


<details>
	<summary>See more</summary>
	
	addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements. Answer 
	aCollection."

	aCollection do: [:each | self add: each].
	^aCollection
</details>

#### Collection>>#explorerContents

<details>
	<summary>See more</summary>
	
	explorerContents

	^self explorerContentsWithIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]
</details>

#### Collection>>#ceiling

<details>
	<summary>See more</summary>
	
	ceiling
	^ self collect: [:a | a ceiling]
</details>

#### Collection>>#/ arg

<details>
	<summary>See more</summary>
	
	/ arg

	^ arg adaptToCollection: self andSend: #/
</details>

#### Collection>>#reduce: aTwoArgBlock

Apply the argument, binaryBlock cumulatively to the elements of the receiver. For sequenceable collections the elements will be used in order, for unordered collections the order is unspecified.


<details>
	<summary>See more</summary>
	
	reduce: aTwoArgBlock
	"Apply the argument, binaryBlock cumulatively to the elements of the receiver.
	For sequenceable collections the elements will be used in order, for unordered
	collections the order is unspecified."

	^self fold: aTwoArgBlock
</details>

#### Collection>>#inject: thisValue into: binaryBlock

Accumulate a running value associated with evaluating the argument, binaryBlock, with the current value of the argument, thisValue, and the receiver as block arguments. For instance, to sum the numeric elements of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal + next].


<details>
	<summary>See more</summary>
	
	inject: thisValue into: binaryBlock 
	"Accumulate a running value associated with evaluating the argument, 
	binaryBlock, with the current value of the argument, thisValue, and the 
	receiver as block arguments. For instance, to sum the numeric elements 
	of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal + 
	next]."

	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^nextValue
</details>

#### Collection>>#detect: aBlock ifNone: exceptionBlock

Evaluate aBlock with each of the receiver's elements as the argument. Answer the first element for which aBlock evaluates to true. If none evaluate to true, then evaluate the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value
</details>

#### Collection>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet _ false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
</details>

#### Collection>>#asFloatArray

Answer a FloatArray whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asFloatArray
	"Answer a FloatArray whose elements are the elements of the receiver"

	^self as: FloatArray
</details>

#### Collection>>#min: aBlock

Answer the minimum of evaluating aBlock on all the elements of the receiver.


<details>
	<summary>See more</summary>
	
	min: aBlock
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer
</details>

#### Collection>>#printNameOn: aStream

<details>
	<summary>See more</summary>
	
	printNameOn: aStream
	super printOn: aStream
</details>

#### Collection>>#asCommaSeparated: aPrintingBlock on: aStream

<details>
	<summary>See more</summary>
	
	asCommaSeparated: aPrintingBlock on: aStream

	^self as: aPrintingBlock on: aStream delimiter: ', ' last: ' and '
</details>

#### Collection>>#removeAllSuchThat: aBlock

Evaluate aBlock for each element and remove all that elements from the receiver for that aBlock evaluates to true. Use a copy to enumerate collections whose order changes when an element is removed (i.e. Sets).


<details>
	<summary>See more</summary>
	
	removeAllSuchThat: aBlock 
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  Use a copy to enumerate 
	collections whose order changes when an element is removed (i.e. Sets)."

	self copy do: [:each | (aBlock value: each) ifTrue: [self remove: each]]
</details>

#### Collection>>#identityIncludes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	identityIncludes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	self do: [:each | anObject == each ifTrue: [^true]].
	^false
</details>

#### Collection>>#exp

<details>
	<summary>See more</summary>
	
	exp
	^self collect: [:each | each exp]
</details>

#### Collection>>#sum

Compute the sum of all the elements in the receiver


<details>
	<summary>See more</summary>
	
	sum
	"Compute the sum of all the elements in the receiver"

	^self fold: [ :a :b | a + b]
</details>

#### Collection>>#atRandom

Answer a random element of the receiver. Uses a shared random number generator owned by class Collection. If you use this a lot, define your own instance of Random and use #atRandom:. Causes an error if self has no elements.


<details>
	<summary>See more</summary>
	
	atRandom
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ Random withDefaultDo: [ :random |
		self atRandom: random ]

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"
</details>

#### Collection>>#allSatisfy: aBlock

Evaluate aBlock with the elements of the receiver. If aBlock returns false for any element return false. Otherwise return true.


<details>
	<summary>See more</summary>
	
	allSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	self do: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true
</details>

#### Collection>>#contents

<details>
	<summary>See more</summary>
	
	contents
	^ self
</details>

#### Collection>>#asIdentitySet

Answer an IdentitySet whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asIdentitySet
	"Answer an IdentitySet whose elements are the elements of the receiver"

	^self as: IdentitySet
</details>

#### Collection>>#adaptToCollection: rcvr andSend: selector

If I am involved in arithmetic with another Collection, return a Collection of the results of each element combined with the scalar in that expression.


<details>
	<summary>See more</summary>
	
	adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with another Collection, return a Collection of
	the results of each element combined with the scalar in that expression."

	rcvr isSequenceable & self isSequenceable ifFalse:
		[self error: 'Only sequenceable collections may be combined arithmetically'].
	^ rcvr with: self collect:
		[:rcvrElement :myElement | rcvrElement perform: selector with: myElement]
</details>

#### Collection>>#, aCollection

<details>
	<summary>See more</summary>
	
	, aCollection
	^self copy addAll: aCollection; yourself
</details>

#### Collection>>#asCommaStringAnd

<details>
	<summary>See more</summary>
	
	asCommaStringAnd

	^String streamContents: [:stream | self asStringOn: stream delimiter: ', ' last: ' and ']
</details>

#### Collection>>#floor

<details>
	<summary>See more</summary>
	
	floor
	^ self collect: [:a | a floor]
</details>

#### Collection>>#average

<details>
	<summary>See more</summary>
	
	average
	^self mean
</details>

#### Collection>>#sign

<details>
	<summary>See more</summary>
	
	sign
	^self collect: [:each | each sign]
</details>

#### Collection>>#asNewArray

Answer a new Array whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asNewArray
	"Answer a new Array whose elements are the elements of the receiver"

	^self as: Array
</details>

#### Collection>>#collect: aBlock andFold: aTwoArgBlock

Evaluate the block with the first two elements of the receiver, then with the result of the first evaluation and the next element, and so on. Answer the result of the final evaluation. If the receiver is empty, raise an error. If the receiver has a single element, answer that element.


<details>
	<summary>See more</summary>
	
	collect: aBlock andFold: aTwoArgBlock
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b]
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b]
	"

	^ self
		collect: aBlock
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]
</details>

#### Collection>>#as: aPrintingBlock on: aStream delimiter: delimiter last: lastDelimiter

<details>
	<summary>See more</summary>
	
	as: aPrintingBlock on: aStream delimiter: delimiter last: lastDelimiter

	| position selfSize |

	position := 1.
	selfSize := self size.

	self
		do: [:elem |
			position := position + 1.
			aPrintingBlock value: elem ]
		separatedBy: [
			aStream nextPutAll: (position = selfSize ifTrue: [lastDelimiter] ifFalse: [delimiter])]
</details>

#### Collection>>#max

<details>
	<summary>See more</summary>
	
	max
	^ self inject: self anyOne into: [:max :each | max max: each]
</details>

#### Collection>>#asDictionary

Answer a Dictionary. Assume our elements are Associations. { #itemGroup -> 10. #itemOrder -> 30. #label -> 'Browser'. #object -> BrowserWindow. #selector -> #openBrowser. #icon -> #editFindReplaceIcon. #balloonText -> 'A Smalltalk code browser, for studying and modifying the system'. } asDictionary


<details>
	<summary>See more</summary>
	
	asDictionary
	"Answer a Dictionary. Assume our elements are Associations.
	{
			#itemGroup 		-> 	10.
			#itemOrder 		-> 	30.
			#label 			-> 	'Browser'.
			#object 			-> 	BrowserWindow.
			#selector 		-> 	#openBrowser.
			#icon 			-> 	#editFindReplaceIcon.
			#balloonText 	-> 	'A Smalltalk code browser, for studying and modifying the system'.
		} asDictionary
	"

	^ self as: Dictionary
</details>

#### Collection>>#asCharacterSet

Answer a CharacterSet whose elements are the unique elements of the receiver. The receiver should only contain characters


<details>
	<summary>See more</summary>
	
	asCharacterSet
	"Answer a CharacterSet whose elements are the unique elements of the receiver.  The receiver should only contain characters"

	^self as: CharacterSet
</details>

#### Collection>>#asWordArray

Answer a WordArray whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asWordArray
	"Answer a WordArray whose elements are the elements of the receiver"

	^self as: WordArray
</details>

#### Collection>>#ifEmpty: aBlock

Evaluate the block if I'm empty '' ifEmpty: [ :c | c, 'coda' ] 'coda' '' ifEmpty: [ 'replacement' ] 'replacement' 'stuff' ifEmpty: [ :c | c, 'coda' ] 'stuff' 'stuff' ifEmpty: [ 'replacement' ] 'stuff' |c|c _ OrderedCollection new. c ifEmpty: [c add: 'stuff']. c an OrderedCollection('stuff') |c|c _ OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection('stuff') |c|c _ OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff']. c an OrderedCollection('a') |c|c _ OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection('a')


<details>
	<summary>See more</summary>
	
	ifEmpty: aBlock
	"Evaluate the block if I'm empty
	'' ifEmpty: [ :c | c, 'coda' ] 					'coda'
	'' ifEmpty: [ 'replacement' ]					 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] 				'stuff'
	'stuff' ifEmpty: [ 'replacement' ]				'stuff'
	|c|c _ OrderedCollection new. c ifEmpty: [c add: 'stuff']. c			 an OrderedCollection('stuff')
	|c|c _ OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff']. c	 an OrderedCollection('stuff')
	|c|c _ OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff']. c		 an OrderedCollection('a')
	|c|c _ OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff']. c  an OrderedCollection('a')
	"

	 self isEmpty ifTrue: [ ^aBlock valueWithPossibleArgument: self ]
</details>

#### Collection>>#groupBy: keyBlock having: selectBlock

Like in SQL operation - Split the receivers contents into collections of elements for which keyBlock returns the same results, and return those collections allowed by selectBlock.


<details>
	<summary>See more</summary>
	
	groupBy: keyBlock having: selectBlock 
	"Like in SQL operation - Split the receivers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock. "
	
	^ (self groupBy: keyBlock) select: selectBlock
</details>

#### Collection>>#includesAllOf: aCollection

Answer whether all the elements of aCollection are in the receiver.


<details>
	<summary>See more</summary>
	
	includesAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true
</details>

#### Collection>>#squared

<details>
	<summary>See more</summary>
	
	squared
	^ self collect: [:each | each * each]
</details>

#### Collection>>#collect: collectBlock thenSelect: selectBlock

<details>
	<summary>See more</summary>
	
	collect: collectBlock thenSelect: selectBlock
	^ (self collect: collectBlock) select: selectBlock
</details>

#### Collection>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility
</details>

#### Collection>>#asBag

Answer a Bag whose elements are the elements of the receiver.


<details>
	<summary>See more</summary>
	
	asBag
	"Answer a Bag whose elements are the elements of the receiver."

	^ self as: Bag
</details>

#### Collection>>#detectMin: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Answer the element for which aBlock evaluates to the lowest number. If collection empty, return nil.


<details>
	<summary>See more</summary>
	
	detectMin: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the lowest number.
	If collection empty, return nil."

	| minElement minValue val |
	self do: [:each | 
		minValue
			ifNil: ["first element"
				minElement _ each.
				minValue _ aBlock value: each]
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."
			ifNotNil: [
				(val _ aBlock value: each) < minValue ifTrue: [
					minElement _ each.
					minValue _ val]]].
	^ minElement
</details>

#### Collection>>#degreeCos

<details>
	<summary>See more</summary>
	
	degreeCos
	^self collect: [:each | each degreeCos]
</details>

#### Collection>>#asByteArray

<details>
	<summary>See more</summary>
	
	asByteArray

	^self as: ByteArray
</details>

#### Collection>>#occurrencesOf: anObject

Answer how many of the receiver's elements are equal to anObject.


<details>
	<summary>See more</summary>
	
	occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally _ 0.
	self do: [:each | anObject = each ifTrue: [tally _ tally + 1]].
	^tally
</details>

#### Collection>>#arcTan

<details>
	<summary>See more</summary>
	
	arcTan
	^self collect: [:each | each arcTan]
</details>

#### Collection>>#detect: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Answer the first element for which aBlock evaluates to true.


<details>
	<summary>See more</summary>
	
	detect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true."

	^ self detect: aBlock ifNone: [self errorNotFound: aBlock]
</details>

#### Collection>>#flattenTo: flattenedStream

<details>
	<summary>See more</summary>
	
	flattenTo: flattenedStream

	self do: [ :each | 
		each isCollection 
			ifTrue: [ each flattenTo: flattenedStream ] 
			ifFalse: [ flattenedStream nextPut: each ]].
		
	^ flattenedStream 

</details>

#### Collection>>#rounded

<details>
	<summary>See more</summary>
	
	rounded
	^ self collect: [:a | a rounded]
</details>

#### Collection>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Collection = aSymbol or: [ super is: aSymbol ]
</details>

#### Collection>>#errorNotKeyed

<details>
	<summary>See more</summary>
	
	errorNotKeyed

	self error: self class notKeyedDescription

</details>

#### Collection>>#remove: oldObject ifAbsent: anExceptionBlock

Remove oldObject from the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock. Otherwise, answer the argument, oldObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: anExceptionBlock 
	"Remove oldObject from the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. ArrayedCollections cannot 
	respond to this message."

	self subclassResponsibility
</details>

#### Collection>>#intersection: aCollection

Answer the set theoretic intersection of two collections.


<details>
	<summary>See more</summary>
	
	intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	^ self select: [:each | aCollection includes: each]
</details>

#### Collection>>#adaptToNumber: rcvr andSend: selector

If I am involved in arithmetic with a scalar, return a Collection of the results of each element combined with the scalar in that expression.


<details>
	<summary>See more</summary>
	
	adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]
</details>

#### Collection>>#printElementsOn: aStream

<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	aStream nextPut: $(.
	self do: [:element | aStream print: element; space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)
</details>

#### Collection>>#asOrderedCollection

Answer an OrderedCollection whose elements are the elements of the receiver. The order in which elements are added depends on the order in which the receiver enumerates its elements. In the case of unordered collections, the ordering is not necessarily the same for multiple requests for the conversion.


<details>
	<summary>See more</summary>
	
	asOrderedCollection
	"Answer an OrderedCollection whose elements are the elements of the
	receiver. The order in which elements are added depends on the order
	in which the receiver enumerates its elements. In the case of unordered
	collections, the ordering is not necessarily the same for multiple 
	requests for the conversion."

	^ self as: OrderedCollection
</details>

#### Collection>>#sqrt

<details>
	<summary>See more</summary>
	
	sqrt
	^ self collect: [:each | each sqrt]
</details>

#### Collection>>#arcCos

<details>
	<summary>See more</summary>
	
	arcCos
	^self collect: [:each | each arcCos]
</details>

#### Collection>>#asFloat64Array

Answer a Float64Array whose elements are the elements of the receiver


<details>
	<summary>See more</summary>
	
	asFloat64Array
	"Answer a Float64Array whose elements are the elements of the receiver"

	^self as: Float64Array
</details>

#### Collection>>#\\ arg

<details>
	<summary>See more</summary>
	
	\\ arg

	^ arg adaptToCollection: self andSend: #\\
</details>

#### Collection>>#* arg

<details>
	<summary>See more</summary>
	
	* arg

	^ arg adaptToCollection: self andSend: #*
</details>

#### Collection>>#anyOne

Answer any element in the receiver.


<details>
	<summary>See more</summary>
	
	anyOne
	"Answer any element in the receiver."

	self do: [:each | ^ each].
	self errorEmptyCollection
</details>

#### Collection>>#fold: aTwoArgBlock

Evaluate the block with the first two elements of the receiver, then with the result of the first evaluation and the next element, and so on. Answer the result of the final evaluation. If the receiver is empty, raise an error. If the receiver has a single element, answer that element.


<details>
	<summary>See more</summary>
	
	fold: aTwoArgBlock
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b]
	"

	^self
		collect: [ :each | each ]
		andFold: aTwoArgBlock
		ifEmpty: [ self errorEmptyCollection ]
</details>

#### Collection>>#removeAll: aCollection

Remove each element of aCollection from the receiver. If successful for each, answer aCollection. Otherwise create an error notification. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	removeAll: aCollection 
	"Remove each element of aCollection from the receiver. If successful for 
	each, answer aCollection. Otherwise create an error notification.
	ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each].
	^ aCollection
</details>

#### Collection>>#asSet

Answer a Set whose elements are the unique elements of the receiver.


<details>
	<summary>See more</summary>
	
	asSet
	"Answer a Set whose elements are the unique elements of the receiver."

	^ self as: Set
</details>

#### Collection>>#cos

<details>
	<summary>See more</summary>
	
	cos
	^self collect: [:each | each cos]
</details>

#### Collection>>#isSequenceable

<details>
	<summary>See more</summary>
	
	isSequenceable
	^ false
</details>

#### Collection>>#// arg

<details>
	<summary>See more</summary>
	
	// arg

	^ arg adaptToCollection: self andSend: #//
</details>

#### Collection>>#do: elementBlock separatedBy: separatorBlock

Evaluate the elementBlock for all elements in the receiver, and evaluate the separatorBlock between.


<details>
	<summary>See more</summary>
	
	do: elementBlock separatedBy: separatorBlock
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst _ true.
	self do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst _ false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]
</details>

#### Collection>>#abs

Absolute value of all elements in the collection


<details>
	<summary>See more</summary>
	
	abs
	"Absolute value of all elements in the collection"
	^ self collect: [:a | a abs]
</details>

#### Collection>>#errorNoMatch

<details>
	<summary>See more</summary>
	
	errorNoMatch

	self error: self class sizesDoNotMatchDescription
</details>

#### Collection>>#hash

A default hash function for any collection. Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior.


<details>
	<summary>See more</summary>
	
	hash
	"A default hash function for any collection.  Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self do: [ :elem | hash _ hash bitXor: elem hash]].
	^ hash bitXor: self size hash

</details>

#### Collection>>#collect: aBlock andFold: aTwoArgBlock ifEmpty: emptyBlockOrValue

Evaluate the block with the first two elements of the receiver, then with the result of the first evaluation and the next element, and so on. Answer the result of the final evaluation. If the receiver is empty, raise an error. If the receiver has a single element, answer that element.


<details>
	<summary>See more</summary>
	
	collect: aBlock andFold: aTwoArgBlock ifEmpty: emptyBlockOrValue
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b] ifEmpty: nil
	#() collect: [ :each | each ] andFold: [:a :b | a, ' ', b] ifEmpty:nil
	"

	| first eachValue answer |
	first _ true.
	self do: [ :each |
		eachValue _  aBlock value: each.
		first
			ifTrue: [
				first _ false.
				answer _ eachValue ]
			ifFalse: [
				answer _ aTwoArgBlock
					value: answer
					value: eachValue ]].
	first ifTrue: [ answer _ emptyBlockOrValue valueWithPossibleArgument: self ].
	^ answer
</details>

#### Collection>>#includes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:each | each = anObject]
</details>

#### Collection>>#negated

Negated value of all elements in the collection


<details>
	<summary>See more</summary>
	
	negated
	"Negated value of all elements in the collection"
	^ self collect: [:a | a negated]
</details>

#### Collection>>#mean

<details>
	<summary>See more</summary>
	
	mean
	^ self sum / self size
</details>

#### Collection>>#select: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true. Answer the new collection.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for 
	which aBlock evaluates to true. Answer the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection
</details>

#### Collection>>#flatten

<details>
	<summary>See more</summary>
	
	flatten

	^ self species streamContents: [ :flattenedStream | self flattenTo: flattenedStream ]
</details>

#### Collection>>#includesAnyOf: aCollection

Answer whether any element of aCollection is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includesAnyOf: aCollection 
	"Answer whether any element of aCollection is one of the receiver's elements."
	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false
</details>

#### Collection>>#copyWithout: oldElement

Answer a copy of the receiver that does not contain any elements equal to oldElement.


<details>
	<summary>See more</summary>
	
	copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self reject: [:each | each = oldElement]

"Examples:
	'fred the bear' copyWithout: $e
	#(2 3 4 5 5 6) copyWithout: 5
"
</details>

#### Collection>>#add: newObject

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: newObject 
	"Include newObject as one of the receiver's elements. Answer newObject. 
	ArrayedCollections cannot respond to this message."

	self subclassResponsibility
</details>

#### Collection>>#anySatisfy: aBlock

Evaluate aBlock with the elements of the receiver. If aBlock returns true for any element return true. Otherwise return false


<details>
	<summary>See more</summary>
	
	anySatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false"

	self do:[:item | (aBlock value: item) ifTrue: [^ true]].
	^ false
</details>

#### Collection>>#asSortedCollection: aSortBlock

Answer a SortedCollection whose elements are the elements of the receiver. The sort order is defined by the argument, aSortBlock.


<details>
	<summary>See more</summary>
	
	asSortedCollection: aSortBlock 
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection _ SortedCollection new: self size sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection
</details>

#### Collection>>#ln

<details>
	<summary>See more</summary>
	
	ln
	^self collect: [:each | each ln]
</details>

#### Collection>>#detect: aBlock ifFound: foundBlock ifNone: exceptionBlock

foundBlock takes one argument, the found object.


<details>
	<summary>See more</summary>
	
	detect: aBlock ifFound: foundBlock ifNone: exceptionBlock 
	"foundBlock takes one argument, the found object."
	self 
		do: [ :element | (aBlock value: element) ifTrue: [ ^foundBlock value: element ] ].
	^exceptionBlock value
</details>

#### Collection>>#arcSin

<details>
	<summary>See more</summary>
	
	arcSin
	^self collect: [:each | each arcSin]
</details>

#### Collection>>#log2

<details>
	<summary>See more</summary>
	
	log2
	^ self collect: [:each | each log2]
</details>

#### Collection>>#emptyCollectionHash

<details>
	<summary>See more</summary>
	
	emptyCollectionHash
	^self species hash
</details>

#### Collection>>#collect: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection like the receiver. Answer the new collection.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect the resulting values into a collection like the receiver. Answer 
	the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection
</details>

#### Collection>>#count: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Return the number that answered true.


<details>
	<summary>See more</summary>
	
	count: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."

	^self inject: 0 into: [ :prevValue :each |
		(aBlock value: each) ifTrue: [ prevValue + 1 ] ifFalse: [ prevValue ] ]
</details>

#### Collection>>#degreeSin

<details>
	<summary>See more</summary>
	
	degreeSin
	^self collect: [:each | each degreeSin]
</details>

#### Collection>>#isEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether the receiver contains any elements."

	self do: [ :any | ^ false ].
	^true
</details>

#### Collection>>#ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock

Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise '' ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ] 'coda' '' ifNotEmpty: [ 'replacement2' ]ifEmpty: [ 'replacement' ] 'replacement' 'stuff' ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ] 'stuffcoda2' 'stuff' ifNotEmpty: [ 'replacement2' ] ifEmpty: [ 'replacement' ] 'replacement2' |c|c _ OrderedCollection new. c ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c an OrderedCollection('stuff') |c|c _ OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection('stuff') |c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c an OrderedCollection('a' 'stuff2') |c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c an OrderedCollection('a' 'stuff2')


<details>
	<summary>See more</summary>
	
	ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	''  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ] 									 'coda'
	'' ifNotEmpty: [ 'replacement2' ]ifEmpty: [ 'replacement' 	]								 'replacement'
	'stuff'  ifNotEmpty: [ :c | c, 'coda2' ]ifEmpty: [ :c | c, 'coda' ]								 'stuffcoda2'
	'stuff' ifNotEmpty: [ 'replacement2' ] ifEmpty: [ 'replacement' ]							 'replacement2'
	|c|c _ OrderedCollection new. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c			 		 an OrderedCollection('stuff')
	|c|c _ OrderedCollection new. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c	 	 an OrderedCollection('stuff')
	|c|c _ OrderedCollection with: 'a'. c  ifNotEmpty: [c add: 'stuff2']ifEmpty: [c add: 'stuff']. c		 		 an OrderedCollection('a' 'stuff2')
	|c|c _ OrderedCollection with: 'a'. c ifNotEmpty: [ :cc | cc add: 'stuff2'] ifEmpty: [ :cc | cc add: 'stuff']. c 	an OrderedCollection('a' 'stuff2')
	"

	^self ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
</details>

#### Collection>>#ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock

Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise '' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ] 'coda' '' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ] 'replacement' 'stuff' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ] 'stuffcoda2' 'stuff' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ] 'replacement2' |c|c _ OrderedCollection new. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c an OrderedCollection('stuff') |c|c _ OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c an OrderedCollection('stuff') |c|c _ OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c an OrderedCollection('a' 'stuff2') |c|c _ OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c an OrderedCollection('a' 'stuff2')


<details>
	<summary>See more</summary>
	
	ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	'' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ] 								 'coda'
	'' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]								 'replacement'
	'stuff' ifEmpty: [ :c | c, 'coda' ] ifNotEmpty: [ :c | c, 'coda2' ]							 'stuffcoda2'
	'stuff' ifEmpty: [ 'replacement' ] ifNotEmpty: [ 'replacement2' ]						 'replacement2'
	|c|c _ OrderedCollection new. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c						  an OrderedCollection('stuff')
	|c|c _ OrderedCollection new. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c	 		 an OrderedCollection('stuff')
	|c|c _ OrderedCollection with: 'a'. c ifEmpty: [c add: 'stuff'] ifNotEmpty: [c add: 'stuff2']. c		 			 an OrderedCollection('a' 'stuff2')
	|c|c _ OrderedCollection with: 'a'. c ifEmpty: [ :cc | cc add: 'stuff'] ifNotEmpty: [ :cc | cc add: 'stuff2']. c  		 an OrderedCollection('a' 'stuff2')
	"
	^ self isEmpty
		ifTrue: [ emptyBlock valueWithPossibleArgument: self ]
		ifFalse: [ notEmptyBlock valueWithPossibleArgument: self ]
</details>

#### Collection>>#explorerContentsWithIndexCollect: twoArgBlock

<details>
	<summary>See more</summary>
	
	explorerContentsWithIndexCollect: twoArgBlock

	^ self asOrderedCollection withIndexCollect: twoArgBlock

</details>

#### Collection>>#roundTo: quantum

<details>
	<summary>See more</summary>
	
	roundTo: quantum
	^self collect: [ :each | each roundTo: quantum ]
</details>

#### Collection>>#+ arg

<details>
	<summary>See more</summary>
	
	+ arg

	^ arg adaptToCollection: self andSend: #+
</details>

#### Collection>>#isCollection

Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:


<details>
	<summary>See more</summary>
	
	isCollection
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^true
</details>

#### Collection>>#add: newObject withOccurrences: anInteger

Add newObject anInteger times to the receiver. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject withOccurrences: anInteger
	"Add newObject anInteger times to the receiver. Answer newObject."

	anInteger timesRepeat: [self add: newObject].
	^ newObject
</details>

#### Collection>>#associationsDo: aBlock

Evaluate aBlock for each of the receiver's elements (key/value associations). If any non-association is within, the error is not caught now, but later, when a key or value message is sent to it.


<details>
	<summary>See more</summary>
	
	associationsDo: aBlock
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it."

	self do: aBlock
</details>

#### Collection>>#copyWithoutAll: aCollection

Answer a copy of the receiver that does not contain any elements equal to those in aCollection.


<details>
	<summary>See more</summary>
	
	copyWithoutAll: aCollection
	"Answer a copy of the receiver that does not contain any elements 
	equal to those in aCollection."

	^ self reject: [:each | aCollection includes: each]
</details>

#### Collection>>#select: selectBlock thenDo: doBlock

Equivalent to (self select: selectBlock) do: doBlock but avoid creating an extra collection.


<details>
	<summary>See more</summary>
	
	select: selectBlock thenDo: doBlock
	"Equivalent to 
		(self select: selectBlock) do: doBlock
	but avoid creating an extra collection."

	self do: [ :each | (selectBlock value: each) ifTrue: [ doBlock value: each ]]
</details>

#### Collection>>#isEmptyOrNil

Answer whether the receiver contains any elements, or is nil. Useful in numerous situations where one wishes the same reaction to an empty collection or to nil


<details>
	<summary>See more</summary>
	
	isEmptyOrNil
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

	^ self isEmpty
</details>

#### Collection>>#groupBy: keyBlock

<details>
	<summary>See more</summary>
	
	groupBy: keyBlock 

	^ self 
		inject: Dictionary new
		into: [ :groupedCollection :elementToGroup | | group |
			group := groupedCollection at: (keyBlock value: elementToGroup) ifAbsentPut: [ OrderedCollection new ].
			group add: elementToGroup.
			groupedCollection ]
		
</details>

#### Collection>>#average: aBlock

<details>
	<summary>See more</summary>
	
	average: aBlock

	^ self average: aBlock ifEmpty: [ self errorEmptyCollection ]
</details>

#### Collection>>#errorCollectionTooSmall

<details>
	<summary>See more</summary>
	
	errorCollectionTooSmall

	self error: self class collectionTooSmallDescription
</details>

#### Collection>>#asStringOn: aStream delimiter: delimString last: lastDelimString

<details>
	<summary>See more</summary>
	
	asStringOn: aStream delimiter: delimString last: lastDelimString

	self as: [ :elem | aStream nextPutAll: elem asString ] on: aStream delimiter: delimString last: lastDelimString
</details>

## SequenceableCollection

I am an abstract superclass for collections that have a well-defined order associated with their elements. Thus each element is externally-named by integers referred to as indices.

### Methods
#### SequenceableCollection>>#allButLast: n

Answer a copy of the receiver containing all but the last n elements. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	allButLast: n
	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
</details>

#### SequenceableCollection>>#fifth

Answer the fifth element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	fifth
	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 5
</details>

#### SequenceableCollection>>#replace: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into self.


<details>
	<summary>See more</summary>
	
	replace: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into self."

	1 to: self size do: [ :index |
		self at: index put: (aBlock value: (self at: index)) ]
</details>

#### SequenceableCollection>>#quickFindFirst: aBlock

Return the index of my first element for which aBlock evaluates as true. Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search. Result is (in these cases) the same as #findFirst: 1 to: 1000 :: findFirst: [:x | x squared > 123456] 1 to: 1000 :: quickFindFirst: [:x | x squared > 123456] 1 to: 1000 :: findFirst: [:x | x squared > 1234560] 1 to: 1000 :: quickFindFirst: [:x | x squared > 1234560]


<details>
	<summary>See more</summary>
	
	quickFindFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findFirst:
	
	1 to: 1000 :: findFirst: [:x | x squared > 123456]
	1 to: 1000 :: quickFindFirst: [:x | x squared > 123456]
	
	1 to: 1000 :: findFirst: [:x | x squared > 1234560]
	1 to: 1000 :: quickFindFirst: [:x | x squared > 1234560]
	"
	^self
		findBinaryIndex: [ :x | (aBlock value: x) ifTrue: [-1] ifFalse: [1]]
		do: [ :i | ]
		ifNone: [ :i1 :i2 | i2 > self size ifFalse: [i2] ifTrue: [0] ]
</details>

#### SequenceableCollection>>#atRandom: aGenerator

Answer a random element of the receiver. Uses aGenerator which should be kept by the user in a variable and used every time. Use this instead of #atRandom for better uniformity of random numbers because only you use the generator. Causes an error if self has no elements.


<details>
	<summary>See more</summary>
	
	atRandom: aGenerator
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (self size atRandom: aGenerator)
</details>

#### SequenceableCollection>>#reversed

Answer a copy of the receiver with element order reversed.


<details>
	<summary>See more</summary>
	
	reversed
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n _ self size.
	result _ self species new: n.
	src _ n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src _ src - 1))].
	^ result

</details>

#### SequenceableCollection>>#collect: aBlock from: firstIndex to: lastIndex

Refer to the comment in Collection|collect:.


<details>
	<summary>See more</summary>
	
	collect: aBlock from: firstIndex to: lastIndex
	"Refer to the comment in Collection|collect:."

	| size result j |
	size _ lastIndex - firstIndex + 1.
	result _ self species new: size.
	j _ firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j _ j + 1].
	^ result
</details>

#### SequenceableCollection>>#shuffledBy: aGenerator

To answer a mutable collection when receiver is, for example, an Interval.


<details>
	<summary>See more</summary>
	
	shuffledBy: aGenerator
	"To answer a mutable collection when receiver is, for example, an Interval."
	^ (self collect: [ :each | each ]) shuffleBy: aGenerator
</details>

#### SequenceableCollection>>#withNextDo: twoArgBlock

Evaluate the block with each element and the one following it. For the last element, next is nil (1 to: 10) asArray withNextDo: [ :each :next | {each. next} print ] #() withNextDo: [ :a :b | {a. b} print ]


<details>
	<summary>See more</summary>
	
	withNextDo: twoArgBlock
	"Evaluate the block with each element and the one following it.
	For the last element, next is nil
	(1 to: 10) asArray withNextDo: [ :each :next | {each. next} print ]
	#() withNextDo: [ :a :b | {a. b} print ]
	"
	| first previous |
	first _ true.
	self do: [ :each |
		first ifTrue: [
			first _ false ]
		ifFalse: [
			twoArgBlock value: previous value: each ].
		previous _ each ].
	first ifFalse: [
		twoArgBlock value: previous value: nil ]
</details>

#### SequenceableCollection>>#identityIndexOf: anElement ifAbsent: exceptionBlock

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	identityIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value
</details>

#### SequenceableCollection>>#with: otherCollection collect: twoArgBlock

Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result
</details>

#### SequenceableCollection>>#penultimate

Answer the penultimate element of the receiver. Raise an error if the collection is empty or has just one element.


<details>
	<summary>See more</summary>
	
	penultimate
	"Answer the penultimate element of the receiver.
	Raise an error if the collection is empty or has just one element."

	^self penultimateIfAbsent: [self errorCollectionTooSmall].

</details>

#### SequenceableCollection>>#first

Answer the first element of the receiver. Raise an error if the collection is empty.


<details>
	<summary>See more</summary>
	
	first
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self size = 0 ifTrue: [self errorEmptyCollection].
	^ self at: 1
</details>

#### SequenceableCollection>>#asColorArray

<details>
	<summary>See more</summary>
	
	asColorArray

	^self as: ColorArray
</details>

#### SequenceableCollection>>#penultimateIfAbsent: aBlock

<details>
	<summary>See more</summary>
	
	penultimateIfAbsent: aBlock
	
	| size |
	
	size := self size.
	size >= 2 ifTrue: [ ^self at: size-1 ].
	^aBlock value
</details>

#### SequenceableCollection>>#allButLast

Answer a copy of the receiver containing all but the last element. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	allButLast
	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
</details>

#### SequenceableCollection>>#copyAfter: anElement

Answer a copy of the receiver from after the first occurence of anElement up to the end. If no such element exists, answer an empty copy.


<details>
	<summary>See more</summary>
	
	copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self species new: 0])
</details>

#### SequenceableCollection>>#swap: oneIndex with: anotherIndex

Move the element at oneIndex to anotherIndex, and vice-versa.


<details>
	<summary>See more</summary>
	
	swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element _ self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element
</details>

#### SequenceableCollection>>#withIndexDo: elementAndIndexBlock separatedBy: separatorBlock

Evaluate the elementAndIndexBlock for all elements in the receiver, and evaluate the separatorBlock between.


<details>
	<summary>See more</summary>
	
	withIndexDo: elementAndIndexBlock separatedBy: separatorBlock
	"Evaluate the elementAndIndexBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do: [:index |
		index = 1 ifFalse: [separatorBlock value].
		elementAndIndexBlock value: (self at: index) value: index]
</details>

#### SequenceableCollection>>#combinationsAt: j upTo: k in: aCollection after: m upTo: n do: aBlock

Choose k of N items and put in aCollection. j-1 already chosen. Indexes of items are in numerical order to avoid duplication. In this slot, we are allowed to use items in self indexed by m+1 up to n. m is the index used for position j-1.


<details>
	<summary>See more</summary>
	
	combinationsAt: j upTo: k in: aCollection after: m upTo: n do: aBlock
	"Choose k of N items and put in aCollection.  j-1 already chosen.  Indexes of items are in numerical order to avoid duplication.  In this slot, we are allowed to use items in self indexed by m+1 up to n.  m is the index used for position j-1."
	"(1 to: 6) combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	m+1 to: n do: [:index | 
		aCollection at: j put: (self at: index).
		j = k
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self combinationsAt: j + 1 upTo: k in: aCollection after: index upTo: n do: aBlock]]
</details>

#### SequenceableCollection>>#third

Answer the third element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	third
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 3
</details>

#### SequenceableCollection>>#integral

<details>
	<summary>See more</summary>
	
	integral
	| answer |
	answer _ self copy.
	2 to: answer size do: [ :i |
		answer at: i put: (answer at: i) + (answer at: i-1) ].
	^answer
</details>

#### SequenceableCollection>>#at: index ifAbsent: exceptionBlock

Answer the element at my position index. If I do not contain an element at index, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	at: index ifAbsent: exceptionBlock
	"Answer the element at my position index. If I do not contain an element
	at index, answer the result of evaluating the argument, exceptionBlock."

	(self isInBounds: index) ifTrue: [^self at: index].
	^exceptionBlock value
</details>

#### SequenceableCollection>>#indexOf: anElement startingAt: start

Answer the index of the first occurence of anElement after start within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	indexOf: anElement startingAt: start
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer 0."

	^self indexOf: anElement startingAt: start ifAbsent: 0
</details>

#### SequenceableCollection>>#shuffled

<details>
	<summary>See more</summary>
	
	shuffled
	^ Random withDefaultDo: [ :random |
		"To answer a mutable collection when receiver is, for example, an Interval."
		(self collect: [ :each | each ]) shuffleBy: random ]

"Examples:
	($A to: $Z) shuffled
	(1 to: 20) shuffled
	'This is a String' shuffled
"
</details>

#### SequenceableCollection>>#pairsCollect: aBlock

Evaluate aBlock with my elements taken two at a time, and return an Array with the results


<details>
	<summary>See more</summary>
	
	pairsCollect: aBlock 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
</details>

#### SequenceableCollection>>#derivative

<details>
	<summary>See more</summary>
	
	derivative
	| displaced answer |
	displaced _ self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first - self first.	"Some reasonable zero"
	answer _ self copy.
	answer -= displaced.
	^answer
</details>

#### SequenceableCollection>>#allButLastDo: block

<details>
	<summary>See more</summary>
	
	allButLastDo: block

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]
</details>

#### SequenceableCollection>>#copyUpToLast: anElement

Answer a copy of the receiver from index 1 to the last occurrence of anElement, not including anElement.


<details>
	<summary>See more</summary>
	
	copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1
</details>

#### SequenceableCollection>>#from: startIndex to: endIndex put: anObject

Put anObject in all indexes between startIndex and endIndex. Very fast. Faster than to:do: for more than 26 positions. Answer anObject


<details>
	<summary>See more</summary>
	
	from: startIndex to: endIndex put: anObject
	"Put anObject in all indexes between startIndex 
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"

	| written toWrite thisWrite |

	startIndex > endIndex ifTrue: [^self].
	self at: startIndex put: anObject.
	written _ 1.
	toWrite _ endIndex - startIndex + 1.
	[written < toWrite] whileTrue:
		[
			thisWrite _ written min: toWrite - written.
			self 
				replaceFrom: startIndex + written
				to: startIndex + written + thisWrite - 1
				with: self startingAt: startIndex.
			written _ written + thisWrite
		].
	^anObject
</details>

#### SequenceableCollection>>#copyReplaceFrom: start to: stop with: replacementCollection

Answer a copy of the receiver satisfying the following conditions: + stop is less than start, then this is an insertion; stop should be exactly start-1, + start = 1 means insert before the first character, + start = size+1 means append after last character. + Otherwise, this is a replacement; start and stop have to be within the receiver's bounds.


<details>
	<summary>See more</summary>
	
	copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize _ self size - (stop - start + 1) + replacementCollection size.
	endReplacement _ start - 1 + replacementCollection size.
	newSequenceableCollection _ self species new: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection
</details>

#### SequenceableCollection>>#writeStream

<details>
	<summary>See more</summary>
	
	writeStream
	^ WriteStream on: self
</details>

#### SequenceableCollection>>#copyWith: newElement

Answer a copy of the receiver that is 1 bigger than the receiver and has newElement at the last element.


<details>
	<summary>See more</summary>
	
	copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and has 
	newElement at the last element."

	| newIC |
	newIC _ self species new: self size + 1.
	newIC 
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1.
	newIC at: newIC size put: newElement.
	^newIC
</details>

#### SequenceableCollection>>#readStream

<details>
	<summary>See more</summary>
	
	readStream
	^ ReadStream on: self
</details>

#### SequenceableCollection>>#with: otherCollection do: twoArgBlock separatedBy: separatorBlock

<details>
	<summary>See more</summary>
	
	with: otherCollection do: twoArgBlock separatedBy: separatorBlock

	| beforeFirst |

	beforeFirst := true.
	self with: otherCollection do: [ :selfElement :otherCollectionElement |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		twoArgBlock value: selfElement value: otherCollectionElement ].



</details>

#### SequenceableCollection>>#findBinary: aBlock

Search for an element in the receiver using binary search. The argument aBlock is a one-element block returning 0 - if the element is the one searched for <0 - if the search should continue in the first half >0 - if the search should continue in the second half If no matching element is found, raise an error. Examples: #(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]


<details>
	<summary>See more</summary>
	
	findBinary: aBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]
	"
	^self findBinary: aBlock do: [ :found | found ] ifNone: [ self errorNotFound: aBlock ]
</details>

#### SequenceableCollection>>#allButFirst: n

Answer a copy of the receiver containing all but the first n elements. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
</details>

#### SequenceableCollection>>#reverse

<details>
	<summary>See more</summary>
	
	reverse
	^ self reversed
</details>

#### SequenceableCollection>>#with: otherCollection with: thirdCollection do: threeArgBlock

Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection with: thirdCollection do: threeArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	| n |
	n _ self size.
	otherCollection size = n ifFalse: [self error: 'otherCollection must be the same size'].
	thirdCollection size = n ifFalse: [self error: 'thirdCollection must be the same size'].
	1 to: n do: [ :index |
		threeArgBlock
			value: (self at: index)
			value: (otherCollection at: index)
			value: (thirdCollection at: index)]
</details>

#### SequenceableCollection>>#replaceFrom: start to: stop with: replacement startingAt: repStart

This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the sequenceable collection, replacementCollection. Answer the receiver. No range checks are performed.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver 
	starting at index, repStart, in the sequenceable collection, 
	replacementCollection. Answer the receiver. No range checks are 
	performed."

	| index repOff |
	repOff _ repStart - start.
	index _ start - 1.
	[(index _ index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]
</details>

#### SequenceableCollection>>#lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: aBlock

Answer the index of the last occurence of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: aBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: firstIndex by: -1 do: [ :index |
		(self at: index) = anElement ifTrue: [^ aBlock value: index]]
</details>

#### SequenceableCollection>>#lastIfEmpty: aBlock

<details>
	<summary>See more</summary>
	
	lastIfEmpty: aBlock
	
	^self ifEmpty: aBlock ifNotEmpty: [ self at: self size ]
</details>

#### SequenceableCollection>>#customizeExplorerContents

<details>
	<summary>See more</summary>
	
	customizeExplorerContents

	^ true.

</details>

#### SequenceableCollection>>#intervalOfSubCollection: aSubCollectionToFind

<details>
	<summary>See more</summary>
	
	intervalOfSubCollection: aSubCollectionToFind

	| startingIndex |

	startingIndex := self indexOfSubCollection: aSubCollectionToFind startingAt: 1.

	^startingIndex toSelfPlus: aSubCollectionToFind size
</details>

#### SequenceableCollection>>#quickIndexOf: anElement

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	quickIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ anElement < some ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]
</details>

#### SequenceableCollection>>#with: otherCollection with: thirdCollection collect: threeArgBlock

Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| n result |
	n _ self size.
	otherCollection size = n ifFalse: [ self error: 'otherCollection must be the same size' ].
	thirdCollection size = n ifFalse: [ self error: 'thirdCollection must be the same size' ].
	result _ self species new: n.
	1 to: n do: [ :index | 
		result at: index put:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result
</details>

#### SequenceableCollection>>#atPin: index

Return the index'th element of me if possible. Return the first or last element if index is out of bounds.


<details>
	<summary>See more</summary>
	
	atPin: index 
	"Return the index'th element of me if possible.
	Return the first or last element if index is out of bounds."

	index < 1 ifTrue: [^ self first].
	index > self size ifTrue: [^ self last].
	^ self at: index
</details>

#### SequenceableCollection>>#assertIsInBounds: anIndex

<details>
	<summary>See more</summary>
	
	assertIsInBounds: anIndex

	(self isInBounds: anIndex) ifFalse: [ self errorSubscriptBounds: anIndex ]
	
</details>

#### SequenceableCollection>>#after: target ifNone: exceptionBlock

Answer the element after target. Answer the result of evaluating the exceptionBlock if there are no elements after it.


<details>
	<summary>See more</summary>
	
	after: target ifNone: exceptionBlock
	"Answer the element after target.  Answer the result of evaluating
	the exceptionBlock if there are no elements after it."

	| index |
	index _ self indexOf: target.
	^ index = 0
		ifTrue: [self errorNotFound: target]
		ifFalse: [index = self size 
			ifTrue: [exceptionBlock value]
			ifFalse: [self at: index + 1]]
</details>

#### SequenceableCollection>>#= otherCollection

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	"#( 1 2 3) = (1 to: 3) -> false"
	otherCollection isInterval ifTrue: [ ^false ].
	^ self hasEqualElements: otherCollection
</details>

#### SequenceableCollection>>#do: aBlock without: anItem

Enumerate all elements in the receiver. Execute aBlock for those elements that are not equal to the given item


<details>
	<summary>See more</summary>
	
	do: aBlock without: anItem
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]
</details>

#### SequenceableCollection>>#shuffleBy: aRandom

<details>
	<summary>See more</summary>
	
	shuffleBy: aRandom

	self size to: 1 by: -1 do: [ :i |
		self swap: i with: ((1 to: i) atRandom: aRandom) ]
</details>

#### SequenceableCollection>>#+= anObject

<details>
	<summary>See more</summary>
	
	+= anObject
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v + anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) + (anObject at: i)) ]]
</details>

#### SequenceableCollection>>#replaceFrom: start to: stop with: replacement

This destructively replaces elements from start to stop in the receiver. Answer the receiver itself. Use copyReplaceFrom:to:with: for insertion/deletion which may alter the size of the result.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver. 
	Answer the receiver itself. Use copyReplaceFrom:to:with: for 
	insertion/deletion which may alter the size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1
</details>

#### SequenceableCollection>>#isInBounds: anIndex

<details>
	<summary>See more</summary>
	
	isInBounds: anIndex

	^anIndex between: 1 and: self size
</details>

#### SequenceableCollection>>#atWrap: index

Answer the index'th element of the receiver. If index is out of bounds, let it wrap around from the end to the beginning until it is in bounds.


<details>
	<summary>See more</summary>
	
	atWrap: index 
	"Answer the index'th element of the receiver.  If index is out of bounds,
	let it wrap around from the end to the beginning until it is in bounds."

	^ self at: index - 1 \\ self size + 1
</details>

#### SequenceableCollection>>#allButFirstDo: block

<details>
	<summary>See more</summary>
	
	allButFirstDo: block

	2 to: self size do:
		[:index | block value: (self at: index)]
</details>

#### SequenceableCollection>>#with: otherCollection do: twoArgBlock

Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
</details>

#### SequenceableCollection>>#combinations: k atATimeDo: aBlock

Take the items in the receiver, k at a time, and evaluate the block for each combination. Hand in an array of elements of self as the block argument. Each combination only occurs once, and order of the elements does not matter. There are (self size choose: k) combinations. 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].


<details>
	<summary>See more</summary>
	
	combinations: k atATimeDo: aBlock
	"Take the items in the receiver, k at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size choose: k) combinations.

	 'abcde' combinations: 3 atATimeDo: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	k = 0 ifTrue: [aBlock value: #(). ^ self].
	aCollection _ Array new: k.
	self combinationsAt: 1 upTo: k in: aCollection after: 0 upTo: self size do: aBlock
</details>

#### SequenceableCollection>>#copyFrom: start to: stop

Answer a copy of a subset of the receiver, starting from element at index start until element at index stop.


<details>
	<summary>See more</summary>
	
	copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize _ stop - start + 1 max: 0.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start
</details>

#### SequenceableCollection>>#replaceAll: oldObject with: newObject

Replace all occurences of oldObject with newObject


<details>
	<summary>See more</summary>
	
	replaceAll: oldObject with: newObject 
	"Replace all occurences of oldObject with newObject"
	| index |
	index _ self
				indexOf: oldObject
				startingAt: 1
				ifAbsent: [0].
	[index = 0]
		whileFalse: 
			[self at: index put: newObject.
			index _ self
						indexOf: oldObject
						startingAt: index + 1
						ifAbsent: [0]]
</details>

#### SequenceableCollection>>#withPreviousDo: twoArgBlock

Evaluate the block with each element and the one before it. For the first element, previous is nil (1 to: 10) asArray withPreviousDo: [ :each :previous | {previous. each} print ] #() withPreviousDo: [ :a :b | {a. b} print ]


<details>
	<summary>See more</summary>
	
	withPreviousDo: twoArgBlock
	"Evaluate the block with each element and the one before it.
	For the first element, previous is nil
	(1 to: 10) asArray withPreviousDo: [ :each :previous | {previous. each} print ]
	#() withPreviousDo: [ :a :b | {a. b} print ]
	"
	| previous |
	previous _ nil.
	self do: [ :each |
		twoArgBlock value: each value: previous.
		previous _ each ].
</details>

#### SequenceableCollection>>#lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock

Answer the index of the last occurence of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
</details>

#### SequenceableCollection>>#before: target

Answer the receiver's element immediately before target. Raise an error if target is not an element of the receiver, but answer nil if there are no elements before it (i.e. it is the first element).


<details>
	<summary>See more</summary>
	
	before: target
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, 
	but answer nil if there are no elements before it (i.e. it is the first element)."

	^ self before: target ifNone: nil
</details>

#### SequenceableCollection>>#findFirst: aBlock

Return the index of my first element for which aBlock evaluates as true.


<details>
	<summary>See more</summary>
	
	findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	| index currentSize |
	
	index _ 0.
	currentSize _ self size.
	
	[(index _ index + 1) <= currentSize ] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	
	^ 0
</details>

#### SequenceableCollection>>#copyReplaceAll: oldSubstring with: newSubstring

Default is not to do token matching. See also String copyReplaceTokens:with:


<details>
	<summary>See more</summary>
	
	copyReplaceAll: oldSubstring with: newSubstring 
	"Default is not to do token matching.
	See also String copyReplaceTokens:with:"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: false
	"'How now brown cow?' copyReplaceAll: 'ow' with: 'ello'"
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Pile'"
</details>

#### SequenceableCollection>>#printStringWithNewline

Convert to a string with returns between items. Elements are usually strings. Useful for labels for PopUpMenus. #('something' 'there') asStringWithNewline


<details>
	<summary>See more</summary>
	
	printStringWithNewline
	"Convert to a string with returns between items.  Elements are usually strings.
	 Useful for labels for PopUpMenus.
	#('something' 'there') asStringWithNewline
	"
	
	^String streamContents: [ :labelStream |
		self do: [ :each |
			each isString
				ifTrue: [ labelStream nextPutAll: each; newLine ]
				ifFalse: [
					each printOn: labelStream.
					labelStream newLine ]].
		self size > 0 ifTrue: [ labelStream skip: -1 ]]
</details>

#### SequenceableCollection>>#errorOutOfBounds

<details>
	<summary>See more</summary>
	
	errorOutOfBounds

	self error: 'indices are out of bounds'
</details>

#### SequenceableCollection>>#allButFirst

Answer a copy of the receiver containing all but the first element. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	allButFirst
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

	^ self allButFirst: 1
</details>

#### SequenceableCollection>>#@ aCollection

<details>
	<summary>See more</summary>
	
	@ aCollection 
	^ self with: aCollection collect: [:a :b | a @ b]
</details>

#### SequenceableCollection>>#seventh

Answer the seventh element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	seventh
	"Answer the seventh element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 7
</details>

#### SequenceableCollection>>#middle

Answer the middle element of the receiver.


<details>
	<summary>See more</summary>
	
	middle
	"Answer the middle element of the receiver."
	self emptyCheck.
	^ self at: self size // 2 + 1
</details>

#### SequenceableCollection>>#findBinaryIndex: aBlock

Search for an element in the receiver using binary search. The argument aBlock is a one-element block returning 0 - if the element is the one searched for <0 - if the search should continue in the first half >0 - if the search should continue in the second half If no matching element is found, raise an error. Examples: #(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]


<details>
	<summary>See more</summary>
	
	findBinaryIndex: aBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ]
	"
	^self findBinaryIndex: aBlock do: [ :found | found ] ifNone: [ self errorNotFound: aBlock]
</details>

#### SequenceableCollection>>#copyFrom: start count: n

Answer a copy of a subset of the receiver, starting from element at index start and of size n.


<details>
	<summary>See more</summary>
	
	copyFrom: start count: n
	"Answer a copy of a subset of the receiver, starting from element at 
	index start and of size n."

	^(self species new: n)
		replaceFrom: 1
		to: n
		with: self
		startingAt: start
</details>

#### SequenceableCollection>>#indexOf: anElement

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	indexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]
</details>

#### SequenceableCollection>>#hasEqualElements: otherCollection

Answer whether the receiver's size is the same as otherCollection's size, and each of the receiver's elements equal the corresponding element of otherCollection. This should probably replace the current definition of #= .


<details>
	<summary>See more</summary>
	
	hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size _ self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true
</details>

#### SequenceableCollection>>#reverseWith: aSequenceableCollection do: aBlock

Evaluate aBlock with each of the receiver's elements, in reverse order, along with the corresponding element, also in reverse order, from aSequencableCollection.


<details>
	<summary>See more</summary>
	
	reverseWith: aSequenceableCollection do: aBlock 
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  
	corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorNoMatch].
	self size
		to: 1
		by: -1
		do: [:index | aBlock value: (self at: index)
				value: (aSequenceableCollection at: index)]
</details>

#### SequenceableCollection>>#atAll: indexArray putAll: valueArray

Store the elements of valueArray into the slots of this collection selected by indexArray.


<details>
	<summary>See more</summary>
	
	atAll: indexArray putAll: valueArray
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray
</details>

#### SequenceableCollection>>#atLast: indexFromEnd

Return element at indexFromEnd from the last position. atLast: 1, returns the last element


<details>
	<summary>See more</summary>
	
	atLast: indexFromEnd
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']
</details>

#### SequenceableCollection>>#quickFindLast: aBlock

Return the index of my last element for which aBlock evaluates as true. Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search. Result is (in these cases) the same as #findLast: 1 to: 1000 :: findLast: [:x | x squared < 123456] 1 to: 1000 :: quickFindLast: [:x | x squared < 123456] 1 to: 1000 :: findLast: [:x | x squared < -10] 1 to: 1000 :: quickFindLast: [:x | x squared < -10] 1 to: 1000 :: findLast: [:x | x squared < 1234560] 1 to: 1000 :: quickFindLast: [:x | x squared < 1234560]


<details>
	<summary>See more</summary>
	
	quickFindLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true.
	Assumes that receiver is sorted according with aBlock. Then, we are able to use faster binary search.
	Result is (in these cases) the same as #findLast:
	
	1 to: 1000 :: findLast: [:x | x squared < 123456]
	1 to: 1000 :: quickFindLast: [:x | x squared < 123456]
	
	1 to: 1000 :: findLast: [:x | x squared < -10]
	1 to: 1000 :: quickFindLast: [:x | x squared < -10]

	1 to: 1000 :: findLast: [:x | x squared < 1234560]
	1 to: 1000 :: quickFindLast: [:x | x squared < 1234560]
	"
	^self
		findBinaryIndex: [ :x | (aBlock value: x) ifTrue: [1] ifFalse: [-1]]
		do: [ :i | ]
		ifNone: [ :i1 :i2 | i1 ]
</details>

#### SequenceableCollection>>#, otherCollection

Concatenate two Strings or Collections.


<details>
	<summary>See more</summary>
	
	, otherCollection 
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 printStringBase: 16) copyFrom: 4 to: 6) , ' boy!'
"
</details>

#### SequenceableCollection>>#incrementFraction

#(10 12.5 15 20) incrementFraction


<details>
	<summary>See more</summary>
	
	incrementFraction
	"
	#(10 12.5 15 20) incrementFraction
	"
	| displaced answer |
	displaced _ self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first.
	answer _ self copy.
	answer -= displaced.
	^answer / displaced
</details>

#### SequenceableCollection>>#asNewArray

Answer a new Array whose elements are the elements of the receiver. Optimized implementation.


<details>
	<summary>See more</summary>
	
	asNewArray
	"Answer a new Array whose elements are the elements of the receiver.
	Optimized implementation."

	^ Array withAll: self
</details>

#### SequenceableCollection>>#from: start to: stop do: aBlock

Evaluate aBlock for all elements between start and stop (inclusive).


<details>
	<summary>See more</summary>
	
	from: start to: stop do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [:index | aBlock value: (self at: index)]
</details>

#### SequenceableCollection>>#upTo: anObject

Deprecated. Use copyUpTo:


<details>
	<summary>See more</summary>
	
	upTo: anObject
	"Deprecated. Use copyUpTo:"

	^ self copyUpTo: anObject
</details>

#### SequenceableCollection>>#indexOfSubCollection: aSubCollection startingAt: anIndex

Answer the index of the receiver's first element, such that that element equals the first element of aSubCollection, and the next elements equal the rest of the elements of aSubCollection. Begin the search at element anIndex of the receiver. If no such match is found, answer 0.


<details>
	<summary>See more</summary>
	
	indexOfSubCollection: aSubCollection startingAt: anIndex 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal 
	the rest of the elements of aSubCollection. Begin the search at element 
	anIndex of the receiver. If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]
</details>

#### SequenceableCollection>>#ifInBounds: anIndex ifNot: aValuable

<details>
	<summary>See more</summary>
	
	ifInBounds: anIndex ifNot: aValuable

	^(self isInBounds: anIndex) ifTrue: [ anIndex ] ifFalse: aValuable 
</details>

#### SequenceableCollection>>#permutationsStartingAt: anInteger do: aBlock

#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]


<details>
	<summary>See more</summary>
	
	permutationsStartingAt: anInteger do: aBlock
	"#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	anInteger > self size ifTrue: [^self].
	anInteger = self size ifTrue: [^aBlock value: self].
	anInteger to: self size do:
		[:i | self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i]
</details>

#### SequenceableCollection>>#antepenultimateIfAbsent: aBlock

<details>
	<summary>See more</summary>
	
	antepenultimateIfAbsent: aBlock
	
	| size |
	
	size := self size.
	size >= 3 ifTrue: [ ^self at: size - 2 ].
	^aBlock value
</details>

#### SequenceableCollection>>#lastIndexOf: anElement

Answer the index of the last occurence of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	lastIndexOf: anElement
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: [0]
</details>

#### SequenceableCollection>>#do: aBlock

Refer to the comment in Collection >> #do: Note: Subclasses need to redefine either #do: or #size (or both!).


<details>
	<summary>See more</summary>
	
	do: aBlock 
	"Refer to the comment in Collection >> #do:
	Note: Subclasses need to redefine either #do: or #size (or both!).
	"
	1 to: self size do: [ :index |
		aBlock value: (self at: index) ]
</details>

#### SequenceableCollection>>#copyAfterLast: anElement

Answer a copy of the receiver from after the last occurence of anElement up to the end. If no such element exists, answer an empty copy.


<details>
	<summary>See more</summary>
	
	copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self species new: 0])
</details>

#### SequenceableCollection>>#asByteArray

Answer a ByteArray whose elements are the elements of the receiver. Optimized implementation.


<details>
	<summary>See more</summary>
	
	asByteArray
	"Answer a ByteArray whose elements are the elements of the receiver.
	Optimized implementation."

	^ ByteArray withAll: self
</details>

#### SequenceableCollection>>#first: n

Answer the first n elements of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	first: n
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: n
</details>

#### SequenceableCollection>>#groupsOf: n atATimeDo: aBlock

Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end. Allows use of a flattened array for things that naturally group into groups of n. If aBlock has a single argument, pass it an array of n items, otherwise, pass the items as separate arguments. See also pairsDo:


<details>
	<summary>See more</summary>
	
	groupsOf: n atATimeDo: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray args |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].
</details>

#### SequenceableCollection>>#copyUpTo: anElement

Answer all elements up to but not including anObject. If there is no such object, answer a copy of the receiver.


<details>
	<summary>See more</summary>
	
	copyUpTo: anElement 
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1
</details>

#### SequenceableCollection>>#lastIndexOf: anElement ifAbsent: exceptionBlock

Answer the index of the last occurence of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	lastIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
</details>

#### SequenceableCollection>>#indexOf: anElement ifAbsent: exceptionBlock

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	^self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock
</details>

#### SequenceableCollection>>#after: target

Answer the element after target. Raise an error if target is not in the receiver, but answer nil if there are no elements after it.


<details>
	<summary>See more</summary>
	
	after: target
	"Answer the element after target.  Raise an error if target is not
	in the receiver, but answer nil if there are no elements after it."

	^ self after: target ifNone: nil
</details>

#### SequenceableCollection>>#atAll: aCollection put: anObject

Put anObject at every index specified by the elements of aCollection.


<details>
	<summary>See more</summary>
	
	atAll: aCollection put: anObject 
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject
</details>

#### SequenceableCollection>>#checkedAt: index

<details>
	<summary>See more</summary>
	
	checkedAt: index
	index > self size ifTrue: [self error: 'not enough elements'].
	^ self at: index
</details>

#### SequenceableCollection>>#fillWith: aCollection

<details>
	<summary>See more</summary>
	
	fillWith: aCollection

	self replaceFrom: 1 to: (self size min: aCollection size) with: aCollection startingAt: 1 
</details>

#### SequenceableCollection>>#hashQuick

<details>
	<summary>See more</summary>
	
	hashQuick
	| hash size step |

	size _ self size.
	hash _ (self species hash + size hash) hashMultiply.
	step _ size < 64 ifTrue: [1] ifFalse: [size//64].
	1 to: size by: step do: [ :i | | elem |
		elem _ self at: i.
		elem == self ifFalse: [
			hash _ (hash + elem hash) hashMultiply]].
	^hash
</details>

#### SequenceableCollection>>#copyUpThrough: anElement

Answer all elements up to and including anObject. If there is no such object, answer a copy of the receiver.


<details>
	<summary>See more</summary>
	
	copyUpThrough: anElement 
	"Answer all elements up to and including anObject. If there
	is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])
</details>

#### SequenceableCollection>>#remove: oldObject ifAbsent: anExceptionBlock

SequencableCollections cannot implement removing.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: anExceptionBlock 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement
</details>

#### SequenceableCollection>>#second

Answer the second element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	second
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 2
</details>

#### SequenceableCollection>>#findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock

Search for an element in the receiver using binary search. The argument aBlock is a one-element block returning 0 - if the element is the one searched for <0 - if the search should continue in the first half >0 - if the search should continue in the second half If found, evaluate actionBlock with the index as argument If no matching element is found, evaluate exceptionBlock, with the indexes of the 'bounding' elements as arguments. Warning: Might give invalid indexes, see examples below Examples: #(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 11 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 12 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 0.5 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinaryIndex: [ :arg | 25 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ',{a. b} printString) print]


<details>
	<summary>See more</summary>
	
	findBinaryIndex: aBlock do: actionBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the index as argument
	If no matching element is found, evaluate exceptionBlock,
	with the indexes of the  'bounding' elements as arguments.
	Warning: Might give invalid indexes, see examples below
	Examples:
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 11 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 12 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 0.5 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinaryIndex: [ :arg | 25 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ',{a. b} printString) print]
	"
	| index low high test |
	low _ 1.
	high _ self size.
	[
		index _ high + low // 2.
		low > high ] whileFalse: [
		test _ aBlock value: (self at: index).
		test = 0 
			ifTrue: [ ^actionBlock value: index ]
			ifFalse: [ test > 0
				ifTrue: [ low _ index + 1 ]
				ifFalse: [ high _ index - 1 ]]].
	^exceptionBlock valueWithPossibleArgs: {high. low}
</details>

#### SequenceableCollection>>#atWrap: index put: value

Store value into the index'th element of the receiver. If index is out of bounds, let it wrap around from the end to the beginning until it is in bounds. Answer value.


<details>
	<summary>See more</summary>
	
	atWrap: index put: value
	"Store value into the index'th element of the receiver.  If index is out
	of bounds, let it wrap around from the end to the beginning until it 
	is in bounds. Answer value."

	^ self at: index  - 1 \\ self size + 1 put: value
</details>

#### SequenceableCollection>>#with: otherCollection reverseDo: twoArgBlock

Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection reverseDo: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	self size to: 1 by: -1 do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]
</details>

#### SequenceableCollection>>#sixth

Answer the sixth element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	sixth
	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 6
</details>

#### SequenceableCollection>>#indexOfMax

Answer the index of the maximum value in me.


<details>
	<summary>See more</summary>
	
	indexOfMax
	"Answer the index of the maximum value in me."
	
	| answer max e |
	max _ self at: 1.
	answer _ 1.
	2 to: self size do: [ :i |
		e _ self at: i.
		e > max ifTrue: [
			max _ e.
			answer _ i ]].
	^answer
</details>

#### SequenceableCollection>>#polynomialEval: thisX

Treat myself as the coefficients of a polynomial in X. Evaluate it with thisX. First element is the constant and last is the coefficient for the highest power. https://en.wikipedia.org/wiki/Horner's_method


<details>
	<summary>See more</summary>
	
	polynomialEval: thisX
	"Treat myself as the coefficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coefficient for the highest power.
	https://en.wikipedia.org/wiki/Horner's_method"
	"  #(1 2 3) polynomialEval: 2   "   "is 3*X^2 + 2*X + 1 with X = 2"

	| index sum |
	sum := self at: (index := self size).
	[ (index := index - 1) >= 1 ] whileTrue: [
		sum := sum * thisX + (self at: index) ].
	^sum
</details>

#### SequenceableCollection>>#atAll: indexArray

Answer a new collection like the receiver which contains all elements of the receiver at the indices of indexArray.


<details>
	<summary>See more</summary>
	
	atAll: indexArray
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	^ self species streamContents: [ :stream |
		1 to: indexArray size do: [ :index |
			stream nextPut: (self at: (indexArray at: index)) ]]
</details>

#### SequenceableCollection>>#anyOne

Answer any element in the receiver.


<details>
	<summary>See more</summary>
	
	anyOne
	^ self first
</details>

#### SequenceableCollection>>#atLast: indexFromEnd put: obj

Set the element at indexFromEnd from the last position. atLast: 1 put: obj, sets the last element


<details>
	<summary>See more</summary>
	
	atLast: indexFromEnd put: obj
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"

	^ self at: self size + 1 - indexFromEnd put: obj
</details>

#### SequenceableCollection>>#permutationsDo: aBlock

Repeatly value aBlock with a single copy of the receiver. Reorder the copy so that aBlock is presented all (self size factorial) possible permutations. (1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].


<details>
	<summary>See more</summary>
	
	permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self copy permutationsStartingAt: 1 do: aBlock
</details>

#### SequenceableCollection>>#lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex ifAbsent: exceptionBlock

Answer the index of the last occurence of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	self lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: [ :index | ^index ].
	^exceptionBlock value.
</details>

#### SequenceableCollection>>#last: n

Answer the last n elements of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	last: n
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size _ self size.
	^ self copyFrom: size - n + 1 to: size
</details>

#### SequenceableCollection>>#isSequenceable

<details>
	<summary>See more</summary>
	
	isSequenceable
	^ true
</details>

#### SequenceableCollection>>#beginsWith: sequence

Answer true if the receiver starts with the argument collection.


<details>
	<summary>See more</summary>
	
	beginsWith: sequence
	"Answer true if the receiver starts with the argument collection."
	
	| sequenceSize |
	self size < (sequenceSize _ sequence size) ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index) ifFalse: [ ^false ] ].
	^true
</details>

#### SequenceableCollection>>#do: elementBlock separatedBy: separatorBlock

Evaluate the elementBlock for all elements in the receiver, and evaluate the separatorBlock between.


<details>
	<summary>See more</summary>
	
	do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]
</details>

#### SequenceableCollection>>#identityIndexOf: anElement

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	identityIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]
</details>

#### SequenceableCollection>>#indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock

Answer the index of the receiver's first element, such that that element equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at element start of the receiver. If no such match is found, answer the result of evaluating argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first _ sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index _ 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index _ index+1]]].
	^ exceptionBlock value
</details>

#### SequenceableCollection>>#reverseDo: aBlock

Evaluate aBlock with each of the receiver's elements as the argument, starting with the last element and taking each in sequence up to the first. For SequenceableCollections, this is the reverse of the enumeration for do:.


<details>
	<summary>See more</summary>
	
	reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, 
	starting with the last element and taking each in sequence up to the 
	first. For SequenceableCollections, this is the reverse of the enumeration 
	for do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]
</details>

#### SequenceableCollection>>#fourth

Answer the fourth element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	fourth
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 4
</details>

#### SequenceableCollection>>#indexOf: anElement startingAt: start ifAbsent: exceptionBlock

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	start to: self size do:
		[:i | (self at: i) = anElement ifTrue: [^ i]].
	^ exceptionBlock value
</details>

#### SequenceableCollection>>#before: target ifNone: exceptionBlock

Answer the receiver's element immediately before target. Answer the result of evaluating the exceptionBlock if there are no elements before it.


<details>
	<summary>See more</summary>
	
	before: target ifNone: exceptionBlock
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if there are no elements before it."

	| index |
	index _ self indexOf: target.
	^ index = 0
		ifTrue: [self errorNotFound: target]
		ifFalse: [index = 1 
			ifTrue: [exceptionBlock value]
			ifFalse: [self at: index - 1]]
</details>

#### SequenceableCollection>>#do: aBlock displayingProgress: aString

<details>
	<summary>See more</summary>
	
	do: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			self withIndexDo: [ :each :i |
				barBlock value: i.
				aBlock value: each]]
</details>

#### SequenceableCollection>>#hash

Subclasses might use other methods. However #hashQuick is suggested for very large collections.


<details>
	<summary>See more</summary>
	
	hash
	"Subclasses might use other methods.
	However #hashQuick is suggested for very large collections."
	^ self hashQuick
</details>

#### SequenceableCollection>>#atAllPut: anObject

Put anObject at every one of the receiver's indices.


<details>
	<summary>See more</summary>
	
	atAllPut: anObject 
	"Put anObject at every one of the receiver's indices."

	| size |
	(size _ self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]
</details>

#### SequenceableCollection>>#findBinary: aBlock do: actionBlock ifNone: exceptionBlock

Search for an element in the receiver using binary search. The argument aBlock is a one-element block returning 0 - if the element is the one searched for <0 - if the search should continue in the first half >0 - if the search should continue in the second half If found, evaluate actionBlock with the found element as argument If no matching element is found, evaluate exceptionBlock, with the 'bounding' elements (or nil) as arguments. Examples: #(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinary: [ :arg | 12 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinary: [ :arg | 0.5 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ', {a. b} printString) print] #(1 3 5 7 11 15 23) findBinary: [ :arg | 25 - arg ] do: [ :found | found print ] ifNone: [ :a :b | ('between: ',{a. b} printString) print]


<details>
	<summary>See more</summary>
	
	findBinary: aBlock do: actionBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If found, evaluate actionBlock with the found element as argument
	If no matching element is found, evaluate exceptionBlock,
	with the  'bounding' elements (or nil) as arguments.
	Examples:
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 11 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 12 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 0.5 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ', {a. b} printString) print]
		#(1 3 5 7 11 15 23)
			findBinary: [ :arg | 25 - arg ]
			do: [ :found | found print ]
			ifNone: [ :a :b | ('between: ',{a. b} printString) print]
	"
	^ self
		findBinaryIndex: aBlock
		do: [ :foundIndex |
			actionBlock value: (self at: foundIndex) ]
		ifNone: [ :prevIndex :nextIndex |
			exceptionBlock
				value:
					(prevIndex > 0 ifTrue: [ self at: prevIndex ])
				value:
					(nextIndex <= self size ifTrue: [ self at: nextIndex ]) ].
</details>

#### SequenceableCollection>>#findLast: aBlock

Return the index of my last element for which aBlock evaluates as true.


<details>
	<summary>See more</summary>
	
	findLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index _ self size + 1.
	[(index _ index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0
</details>

#### SequenceableCollection>>#includes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: anObject
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0
</details>

#### SequenceableCollection>>#-= anObject

<details>
	<summary>See more</summary>
	
	-= anObject
	^anObject isNumber
		ifTrue: [ self replace: [ :v | v - anObject ]]
		ifFalse: [
			self withIndexDo: [ :v :i |
				self at: i put: ((self at: i) - (anObject at: i)) ]]
</details>

#### SequenceableCollection>>#antepenultimate

<details>
	<summary>See more</summary>
	
	antepenultimate
	
	^self antepenultimateIfAbsent: [ self errorCollectionTooSmall ]
</details>

#### SequenceableCollection>>#select: aBlock

Refer to the comment in Collection>>select: .


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Refer to the comment in Collection>>select: ."
	^self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			(aBlock value: (self at: index))
				ifTrue: [ strm nextPut: (self at: index) ]]]
</details>

#### SequenceableCollection>>#asDigitsToPower: anInteger do: aBlock

Repeatedly value aBlock with a single Array. Adjust the collection so that aBlock is presented all (self size raisedTo: anInteger) possible combinations of the receiver's elements taken as digits of an anInteger long number. (0 to: 1) asDigitsToPower: 4 do: [:each | Transcript newLine; show: each printString].


<details>
	<summary>See more</summary>
	
	asDigitsToPower: anInteger do: aBlock
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number.

	(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript newLine; show: each printString].
	"

	| aCollection |
	aCollection _ Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock
</details>

#### SequenceableCollection>>#withIndexDo: elementAndIndexBlock

Just like with:do: except that the iteration index supplies the second argument to the block.


<details>
	<summary>See more</summary>
	
	withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
</details>

#### SequenceableCollection>>#withIndexCollect: elementAndIndexBlock

Just like with:collect: except that the iteration index supplies the second argument to the block.


<details>
	<summary>See more</summary>
	
	withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result
</details>

#### SequenceableCollection>>#endsWith: sequence

Answer true if the receiver ends with the argument collection.


<details>
	<summary>See more</summary>
	
	endsWith: sequence
	"Answer true if the receiver ends with the argument collection."
	
	| sequenceSize offset |
	sequenceSize _ sequence size.
	(offset _ self size - sequenceSize) < 0 ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :index |
		(sequence at: index) = (self at: index + offset) ifFalse: [ ^false ] ].
	^true
</details>

#### SequenceableCollection>>#pairsDo: aBlock

Evaluate aBlock with my elements taken two at a time. If there's an odd number of items, ignore the last one. Allows use of a flattened array for things that naturally group into pairs. See also pairsCollect:


<details>
	<summary>See more</summary>
	
	pairsDo: aBlock 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript newLine; show: b, ' is number ', a printString]
"
</details>

#### SequenceableCollection>>#collect: aBlock

Refer to the comment in Collection|collect:.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Refer to the comment in Collection|collect:."
	| result |
	result _ self species new: self size.
	1 to: self size do:
		[:index | result at: index put: (aBlock value: (self at: index))].
	^ result
</details>

#### SequenceableCollection>>#ninth

Answer the ninth element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	ninth
	"Answer the ninth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 9
</details>

#### SequenceableCollection>>#asDigitsAt: anInteger in: aCollection do: aBlock

(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]


<details>
	<summary>See more</summary>
	
	asDigitsAt: anInteger in: aCollection do: aBlock
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	self do: 
		[:each | 
		aCollection at: anInteger put: each.
		anInteger = aCollection size 
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self asDigitsAt: anInteger + 1 in: aCollection do: aBlock]].
</details>

#### SequenceableCollection>>#eighth

Answer the eighth element of the receiver. Raise an error if there are not enough elements.


<details>
	<summary>See more</summary>
	
	eighth
	"Answer the eighth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 8
</details>

#### SequenceableCollection>>#last

Answer the last element of the receiver. Raise an error if the collection is empty.


<details>
	<summary>See more</summary>
	
	last
	"Answer the last element of the receiver.
	Raise an error if the collection is empty."

	| size |
	(size _ self size) = 0 ifTrue: [self errorEmptyCollection].
	^ self at: size
</details>

#### SequenceableCollection>>#concatenation

<details>
	<summary>See more</summary>
	
	concatenation
	|result index|

	result _ Array new: (self inject: 0 into: [:sum :each | sum + each size]).
	index _ 0.
	self do: [:each | each do: [:item | result at: (index _ index+1) put: item]].
	^result
</details>

#### SequenceableCollection>>#atLast: indexFromEnd ifAbsent: block

Return element at indexFromEnd from the last position. atLast: 1 ifAbsent: [] returns the last element


<details>
	<summary>See more</summary>
	
	atLast: indexFromEnd ifAbsent: block
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block
</details>

#### SequenceableCollection>>#copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens

Answer a copy of the receiver in which all occurrences of oldSubstring have been replaced by newSubstring. ifTokens (valid for Strings only) specifies that the characters surrounding the recplacement must not be alphanumeric. Bruce Simth, must be incremented by 1 and not newSubstring if ifTokens is true. See example below.


<details>
	<summary>See more</summary>
	
	copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |
	(ifTokens and: [ self isString not ])
		ifTrue: [(self is: #Text) ifFalse: [
			self error: 'Token replacement only valid for Strings']].
	aString _ self.
	startSearch _ 1.
	[(currentIndex _ aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex _ currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isValidInIdentifiers not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isValidInIdentifiers not]]])
			ifTrue: [aString _ aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch _ currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch _ currentIndex + 1]
					ifFalse: [startSearch _ currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "

</details>

#### SequenceableCollection>>#keysAndValuesDo: aBlock

Enumerate the receiver with all the keys and values


<details>
	<summary>See more</summary>
	
	keysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values"
	1 to: self size do:[:i|
		aBlock value: i value: (self at: i).
	].
</details>

