## Heap

Class Heap implements a special data structure commonly referred to as 'heap' [ http://en.wikipedia.org/wiki/Heap_%28data_structure%29 ] A Heap is a kind of binary tree stored in a linear array - see details after the instance variables description. Heaps are good at handling priority queues because: 1) greatest priority element according to the sort block will be stored in first position and thus accessed in O(1) operations 2) worse time for inserting or removing an element is in O(log n) operations, where n is the size of the Heap Insertion/Removal times are more efficient than above upper bound, provided that: a) Elements are only removed at the beginning b) Elements are added with arbitrary sort order. 3) there is no need to fully sort the Heap, which makes it more efficient than a SortedCollection The heap can be fully sorted by sending the message #fullySort. Worse time for fully sorting the Heap is in O(n log n) operations, but this is rarely used a feature. Remind that the Heap does not fully sort the collection if you don't ask. Thus don't expect #do: and other iterators to enumerate elements according to the sortBlock order. Instance variables: array <Array> The data repository tally <Integer> The number of elements in the heap sortBlock <Block|nil> A two-argument block defining the sort order, or nil in which case the default sort order is [:element1 :element2| element1 <= element2] indexUpdateBlock <Block|nil> A two-argument block of the form [:data :index | ... ] which allows an application object to keep track of its index within the heap. Useful for quick heap update when object's sort value changes (for example, when an object in a priority queue has its priority increased by an external event, you don't want to have to search through the whole heap to find the index before fixing the heap). No update occurs if nil. The Heap can be viewed as a binary tree (every node in the tree has at most two children). The root is stored in first slot of internal array. The children are stored in next two slots. The children of children in next four slots. etc... For a node A of index i (1 based), the two children B1 and B2 are thus stored in indices (2*i) and (2*i+1). Of course, the children indices must be less than the tally otherwise they are considered inexistent. The Heap does arrange to preserve the following invariant: For any children B of a node A, A is sorted before B, in other words, (self sort: A before: B) = true This implies that the root is always the first element according to sort order.

### Methods
#### Heap>>#grow

Become larger.


<details>
	<summary>See more</summary>
	
	grow
	"Become larger."
	self growTo: self size + self growSize.
</details>

#### Heap>>#removeAll

<details>
	<summary>See more</summary>
	
	removeAll

	array atAllPut: nil.
	tally _ 0
</details>

#### Heap>>#downHeap: anIndex

Check the heap downwards for correctness starting at anIndex. Everything above (i.e. left of) anIndex is ok.


<details>
	<summary>See more</summary>
	
	downHeap: anIndex
	"Check the heap downwards for correctness starting at anIndex.
	 Everything above (i.e. left of) anIndex is ok."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[ j := j + 1].
		"check if position k is ok"
		(self sorts: value before: (array at: j)) 
			ifTrue:[	"yes -> break loop"
					n := k - 1]
			ifFalse:[	"no -> make room at j by moving j-th element to k-th position"
					array at: k put: (array at: j).
					self updateObjectIndex: k.
					"and try again with j"
					k := j]].
	array at: k put: value.
	self updateObjectIndex: k.
</details>

#### Heap>>#at: index put: newObject

Heaps are accessed with #add: not #at:put:


<details>
	<summary>See more</summary>
	
	at: index put: newObject
	"Heaps are accessed with #add: not #at:put:"
	^self shouldNotImplement
</details>

#### Heap>>#upHeap: anIndex

Check the heap upwards for correctness starting at anIndex. Everything below anIndex is ok.


<details>
	<summary>See more</summary>
	
	upHeap: anIndex
	"Check the heap upwards for correctness starting at anIndex.
	 Everything below anIndex is ok."
	| value k kDiv2 tmp |
	anIndex = 0 ifTrue:[^self].
	k := anIndex.
	value := array at: anIndex.
	[ (k > 1) and:[self sorts: value before: (tmp := array at: (kDiv2 := k bitShift: -1))] ] 
		whileTrue:[
			array at: k put: tmp.
			self updateObjectIndex: k.
			k := kDiv2].
	array at: k put: value.
	self updateObjectIndex: k.
</details>

#### Heap>>#updateObjectIndex: index

If indexUpdateBlock is not nil, notify the object at index of its new position in the heap array.


<details>
	<summary>See more</summary>
	
	updateObjectIndex: index
	"If indexUpdateBlock is not nil, notify the object at index of its new position in the heap array."
	indexUpdateBlock ifNotNil: [
		indexUpdateBlock value: (array at: index) value: index]
</details>

#### Heap>>#removeFirst

Remove the first element from the receiver


<details>
	<summary>See more</summary>
	
	removeFirst
	"Remove the first element from the receiver"
	^self removeAt: 1
</details>

#### Heap>>#trim

Remove any empty slots in the receiver.


<details>
	<summary>See more</summary>
	
	trim
	"Remove any empty slots in the receiver."
	self growTo: self size.
</details>

#### Heap>>#first

Return the first element in the receiver


<details>
	<summary>See more</summary>
	
	first
	"Return the first element in the receiver"
	^array at: 1
</details>

#### Heap>>#= otherCollection

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= otherCollection

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].

	sortBlock = otherCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: otherCollection 
</details>

#### Heap>>#size

Return the number of elements in the receiver


<details>
	<summary>See more</summary>
	
	size
	"Return the number of elements in the receiver"
	^tally
</details>

#### Heap>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument."
	1 to: tally do:[:i| aBlock value: (array at: i)]
</details>

#### Heap>>#privateRemoveAt: index

Remove the element at the given index and make sure the sorting order is okay


<details>
	<summary>See more</summary>
	
	privateRemoveAt: index
	"Remove the element at the given index and make sure the sorting order is okay"
	| removed |
	removed _ array at: index.
	array at: index put: (array at: tally).
	array at: tally put: nil.
	tally _ tally - 1.
	index > tally ifFalse:[
		"Use #downHeapSingle: since only one element has been removed"
		self downHeapSingle: index].
	^removed
</details>

#### Heap>>#select: aBlock

Evaluate aBlock with each of my elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| answer |
	answer _ self species sortBlock: sortBlock.
	self do: [ :each |
		(aBlock value: each)
			ifTrue: [ answer add: each ]].
	^ answer
</details>

#### Heap>>#indexUpdateBlock: aBlockOrNil

<details>
	<summary>See more</summary>
	
	indexUpdateBlock: aBlockOrNil

	indexUpdateBlock := aBlockOrNil.


</details>

#### Heap>>#at: index

Return the element at the given position within the receiver


<details>
	<summary>See more</summary>
	
	at: index
	"Return the element at the given position within the receiver"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^array at: index
</details>

#### Heap>>#downHeapSingle: anIndex

This version is optimized for the case when only one element in the receiver can be at a wrong position. It avoids one comparison at each node when travelling down the heap and checks the heap upwards after the element is at a bottom position. Since the probability for being at the bottom of the heap is much larger than for being somewhere in the middle this version should be faster.


<details>
	<summary>See more</summary>
	
	downHeapSingle: anIndex
	"This version is optimized for the case when only one element in the receiver can be at a wrong position. It avoids one comparison at each node when travelling down the heap and checks the heap upwards after the element is at a bottom position. Since the probability for being at the bottom of the heap is much larger than for being somewhere in the middle this version should be faster."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[	j := j + 1].
		array at: k put: (array at: j).
		self updateObjectIndex: k.
		"and try again with j"
		k := j].
	array at: k put: value.
	self updateObjectIndex: k.
	self upHeap: k
</details>

#### Heap>>#growTo: newSize

Grow to the requested size.


<details>
	<summary>See more</summary>
	
	growTo: newSize
	"Grow to the requested size."
	| newArray |
	newArray _ Array new: (newSize max: tally).
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array _ newArray
</details>

#### Heap>>#sortBlock: aBlock

<details>
	<summary>See more</summary>
	
	sortBlock: aBlock
	sortBlock _ aBlock.
	self reSort
</details>

#### Heap>>#reSort

Resort the entire heap


<details>
	<summary>See more</summary>
	
	reSort
	"Resort the entire heap"
	self isEmpty ifTrue:[^self].
	tally // 2 to: 1 by: -1 do:[:i| self downHeap: i].
</details>

#### Heap>>#add: anObject

Include newObject as one of the receiver's elements. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: anObject
	"Include newObject as one of the receiver's elements. Answer newObject."
	tally = array size ifTrue:[self grow].
	array at: (tally := tally + 1) put: anObject.
	self updateObjectIndex: tally.
	self upHeap: tally.
	^anObject
</details>

#### Heap>>#fullySort

Fully sort the heap. This method preserves the heap invariants and can thus be sent safely


<details>
	<summary>See more</summary>
	
	fullySort
	"Fully sort the heap.
	This method preserves the heap invariants and can thus be sent safely"
	self privateReverseSort.
	1 to: tally // 2 do: [:i | array swap: i with: 1 + tally - i]
</details>

#### Heap>>#remove: oldObject ifAbsent: aBlock

Remove oldObject as one of the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock. Otherwise, answer the argument, oldObject.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: aBlock
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject."
	1 to: tally do:[:i| 
		(array at: i) = oldObject ifTrue:[^self privateRemoveAt: i]].
	^aBlock value
</details>

#### Heap>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	array _ array copy.
	tally _ tally copy
</details>

#### Heap>>#array

<details>
	<summary>See more</summary>
	
	array
	^array
</details>

#### Heap>>#isEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether the receiver contains any elements."
	^tally = 0
</details>

#### Heap>>#removeAt: index

Remove the element at given position


<details>
	<summary>See more</summary>
	
	removeAt: index
	"Remove the element at given position"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^self privateRemoveAt: index
</details>

#### Heap>>#setCollection: aCollection

<details>
	<summary>See more</summary>
	
	setCollection: aCollection
	array _ aCollection.
	tally _ 0.
</details>

#### Heap>>#sortBlock

<details>
	<summary>See more</summary>
	
	sortBlock
	^sortBlock
</details>

#### Heap>>#growSize

Return the size by which the receiver should grow if there are no empty slots left.


<details>
	<summary>See more</summary>
	
	growSize
	"Return the size by which the receiver should grow if there are no empty slots left."
	^array size max: 5
</details>

#### Heap>>#privateReverseSort

Arrange to have the array sorted in reverse order. WARNING: this method breaks the heap invariants. It's up to the sender to restore them afterwards.


<details>
	<summary>See more</summary>
	
	privateReverseSort
	"Arrange to have the array sorted in reverse order.
	WARNING: this method breaks the heap invariants. It's up to the sender to restore them afterwards."
	| oldTally |
	oldTally := tally.
	[tally > 1] whileTrue:
		[array swap: 1 with: tally.
		tally := tally - 1.
		self downHeapSingle: 1].
	tally := oldTally
</details>

#### Heap>>#sorts: element1 before: element2

Return true if element1 should be sorted before element2. This method defines the sort order in the receiver


<details>
	<summary>See more</summary>
	
	sorts: element1 before: element2
	"Return true if element1 should be sorted before element2.
	This method defines the sort order in the receiver"
	^ sortBlock
		ifNil: [element1 <= element2]
		ifNotNil: [sortBlock value: element1 value: element2]
</details>

#### Heap>>#setCollection: aCollection tally: newTally

<details>
	<summary>See more</summary>
	
	setCollection: aCollection tally: newTally
	array _ aCollection.
	tally _ newTally.
</details>

## Interval

I represent a finite arithmetic progression.

### Methods
#### Interval>>#permutationsDo: aBlock

Repeatly value aBlock with a single copy of the receiver. Reorder the copy so that aBlock is presented all (self size factorial) possible permutations. (1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].


<details>
	<summary>See more</summary>
	
	permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations.

	(1 to: 4) permutationsDo: [:each | Transcript newLine; show: each printString].
	"

	self asArray permutationsDo: aBlock

</details>

#### Interval>>#rangeIncludes: aNumber

Return true if the number lies in the interval between start and stop.


<details>
	<summary>See more</summary>
	
	rangeIncludes: aNumber
	"Return true if the number lies in the interval between start and stop."

	^stop >= start
		ifTrue: [ aNumber between: start and: stop ]
		ifFalse: [ aNumber between: stop and: start ]
</details>

#### Interval>>#at: anInteger put: anObject

Storing into an Interval is not allowed.


<details>
	<summary>See more</summary>
	
	at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'
</details>

#### Interval>>#reverseDo: aBlock

Evaluate aBlock for each element of my interval, in reverse order.


<details>
	<summary>See more</summary>
	
	reverseDo: aBlock 
	"Evaluate aBlock for each element of my interval, in reverse order."
	
	count to: 1 by: -1 do: [ :i |
		aBlock value: (self at: i) ]	
</details>

#### Interval>>#first

Refer to the comment in SequenceableCollection|first.


<details>
	<summary>See more</summary>
	
	first 
	"Refer to the comment in SequenceableCollection|first."

	^start
</details>

#### Interval>>#= otherCollection

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= otherCollection

	self == otherCollection ifTrue: [^ true].
	"(1 to: 3) = #( 1 2 3) -> false"
	otherCollection isInterval ifFalse: [ ^false ].
	^ start = otherCollection first
		and: [count = otherCollection size
			and: [self last = otherCollection last]]
</details>

#### Interval>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^ (start hash bitXor: stop hash) bitXor: count hash
</details>

#### Interval>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	| s |
	aStream nextPut: $(;
	 print: start;
	 nextPutAll: ' to: ';
	 print: stop.
	s _ self increment.
	s ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: s].
	aStream nextPut: $)
</details>

#### Interval>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size

	^ count
</details>

#### Interval>>#includes: aNumber

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: aNumber

	| index |
	
	aNumber isNumber ifFalse: [ ^ false ].
	
	^ start = stop 
		ifTrue: [ start = aNumber ]
		ifFalse: [ 
			index := (aNumber - start) / (stop-start) * (count-1) + 1.
			index isInteger and: [ index between: 1 and: count ]]
</details>

#### Interval>>#do: aBlock

Refer to the comment in Collection >> #do: Note: Subclasses need to redefine either #do: or #size (or both!).


<details>
	<summary>See more</summary>
	
	do: aBlock

	1 to: count do: [ :i |
		aBlock value: (self at: i) ]		
</details>

#### Interval>>#isInterval

<details>
	<summary>See more</summary>
	
	isInterval

	^ true
</details>

#### Interval>>#extent

Answer the max - min of the receiver interval.


<details>
	<summary>See more</summary>
	
	extent 
	"Answer the max - min of the receiver interval."
	"(10 to: 50) extent"

	^stop - start
</details>

#### Interval>>#at: i

Answer the anInteger'th element.


<details>
	<summary>See more</summary>
	
	at: i 
	"Answer the anInteger'th element."

	(i >= 1 and: [i <= count])
		ifTrue: [
			count=1 ifTrue: [ ^start ].
			^start + ((stop-start)/(count-1)*(i-1))]
		ifFalse: [self errorSubscriptBounds: i]
</details>

#### Interval>>#storeOn: aStream

This is possible because we know numbers store and print the same.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"This is possible because we know numbers store and print the same."

	self printOn: aStream
</details>

#### Interval>>#add: newObject

Adding to an Interval is not allowed.


<details>
	<summary>See more</summary>
	
	add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement
</details>

#### Interval>>#setFrom: startNumber to: stopNumber count: countInteger

start and stop can be integers, fractions, floats, whatever. Usually stop>start, but stop=start and stop<start are also possible.


<details>
	<summary>See more</summary>
	
	setFrom: startNumber to: stopNumber count: countInteger
	"start and stop can be integers, fractions, floats, whatever.
	Usually stop>start, but stop=start and stop<start are also possible."

	start _ startNumber.
	stop _ stopNumber.
	count _ countInteger
</details>

#### Interval>>#last

Refer to the comment in SequenceableCollection|last.


<details>
	<summary>See more</summary>
	
	last 
	"Refer to the comment in SequenceableCollection|last."

	^ stop
</details>

#### Interval>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species

	^Array
</details>

#### Interval>>#+ number

<details>
	<summary>See more</summary>
	
	+ number

	^ start + number to: stop + number count: count
</details>

#### Interval>>#- number

<details>
	<summary>See more</summary>
	
	- number

	^ start - number to: stop - number count: count
</details>

#### Interval>>#anyOne

This message will fail for an empty Interval, super would not.


<details>
	<summary>See more</summary>
	
	anyOne
 	"This message will fail for an empty Interval, super would not."
 	^self at: 1
</details>

#### Interval>>#remove: newObject

Removing from an Interval is not allowed.


<details>
	<summary>See more</summary>
	
	remove: newObject 
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'
</details>

#### Interval>>#increment

Answer the receiver's interval increment.


<details>
	<summary>See more</summary>
	
	increment
	"Answer the receiver's interval increment."

	^stop = start ifFalse: [stop-start / (count-1)] ifTrue:[1]
</details>

## LinkedList

I represent a collection of links, which are containers for other objects. Using the message sequence addFirst:/removeLast causes the receiver to behave as a stack; using addLast:/removeFirst causes the receiver to behave as a queue.

### Methods
#### LinkedList>>#addFirst: aLink

Add aLink to the beginning of the receiver's list. Answer aLink.


<details>
	<summary>See more</summary>
	
	addFirst: aLink 
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	self isEmpty ifTrue: [lastLink _ aLink].
	aLink nextLink: firstLink.
	firstLink _ aLink.
	^aLink
</details>

#### LinkedList>>#add: aLink

Add aLink to the end of the receiver's list. Answer aLink.


<details>
	<summary>See more</summary>
	
	add: aLink 
	"Add aLink to the end of the receiver's list. Answer aLink."

	^self addLast: aLink
</details>

#### LinkedList>>#at: index ifAbsent: exceptionBlock

Answer the element at my position index. If I do not contain an element at index, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	at: index ifAbsent: exceptionBlock

	| i |
	index < 1 ifTrue: [ ^exceptionBlock value ].
	i _ 0.
	self do: [ :link |
		(i _ i + 1) = index ifTrue: [ ^ link ]].
	^ exceptionBlock value
</details>

#### LinkedList>>#addLast: aLink

Add aLink to the end of the receiver's list. Answer aLink.


<details>
	<summary>See more</summary>
	
	addLast: aLink 
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty
		ifTrue: [firstLink _ aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink _ aLink.
	^aLink
</details>

#### LinkedList>>#removeFirst

Remove the first element and answer it. If the receiver is empty, create an error notification.


<details>
	<summary>See more</summary>
	
	removeFirst
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink _ firstLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [firstLink _ oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink
</details>

#### LinkedList>>#remove: aLink ifAbsent: aBlock

Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	remove: aLink ifAbsent: aBlock  
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink _ aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink _ nil]]
		ifFalse: [tempLink _ firstLink.
				[tempLink ifNil: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink _ tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink _ tempLink]].
	aLink nextLink: nil.
	^aLink
</details>

#### LinkedList>>#removeLast

Remove the receiver's last element and answer it. If the receiver is empty, create an error notification.


<details>
	<summary>See more</summary>
	
	removeLast
	"Remove the receiver's last element and answer it. If the receiver is 
	empty, create an error notification."

	| oldLink aLink |
	self emptyCheck.
	oldLink _ lastLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [aLink _ firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink _ aLink nextLink].
				 aLink nextLink: nil.
				 lastLink _ aLink].
	oldLink nextLink: nil.
	^oldLink
</details>

#### LinkedList>>#isEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	isEmpty

	^firstLink == nil
</details>

#### LinkedList>>#first

Answer the first link. Create an error notification if the receiver is empty.


<details>
	<summary>See more</summary>
	
	first
	"Answer the first link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^firstLink
</details>

#### LinkedList>>#last

Answer the last link. Create an error notification if the receiver is empty.


<details>
	<summary>See more</summary>
	
	last
	"Answer the last link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^lastLink
</details>

#### LinkedList>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species

	^ Array
</details>

#### LinkedList>>#do: aBlock

Refer to the comment in Collection >> #do: Note: Subclasses need to redefine either #do: or #size (or both!).


<details>
	<summary>See more</summary>
	
	do: aBlock

	| aLink |
	aLink _ firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink _ aLink nextLink]
</details>

#### LinkedList>>#add: link before: otherLink

<details>
	<summary>See more</summary>
	
	add: link before: otherLink

	| aLink |
	firstLink == otherLink ifTrue: [^ self addFirst: link].
	aLink _ firstLink.
	[aLink == nil] whileFalse: [
		aLink nextLink == otherLink ifTrue: [
			link nextLink: aLink nextLink.
			aLink nextLink: link.
			^ link
		].
		 aLink _ aLink nextLink.
	].
	^ self errorNotFound: otherLink
</details>

#### LinkedList>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index

	^self at: index ifAbsent: [ self errorSubscriptBounds: index ]
</details>

## OrderedCollection

I represent a collection of objects ordered by the collector.

### Methods
#### OrderedCollection>>#errorNoSuchElement

<details>
	<summary>See more</summary>
	
	errorNoSuchElement

	self error: 'attempt to index non-existent element in an ordered collection'
</details>

#### OrderedCollection>>#removeAll

<details>
	<summary>See more</summary>
	
	removeAll

	array from: firstIndex to: lastIndex put: nil.
	lastIndex _ firstIndex - 1
</details>

#### OrderedCollection>>#at: anInteger put: anObject

Put anObject at element index anInteger. at:put: cannot be used to append, front or back, to an ordered collection; it is used by a knowledgeable client to replace an element.


<details>
	<summary>See more</summary>
	
	at: anInteger put: anObject 
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	index _ anInteger asInteger.
	(index < 1 or: [index + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^array at: index + firstIndex - 1 put: anObject]
</details>

#### OrderedCollection>>#makeRoomAtLast

<details>
	<summary>See more</summary>
	
	makeRoomAtLast

	| size newArray newFirst newLast |
	size _ self size.
	firstIndex - 4 * 5 > size ifTrue:
		[
			newFirst _ (firstIndex bitShift: -5) + 1.
			newFirst < firstIndex ifTrue:
				[
					newLast _ lastIndex - firstIndex + newFirst.
					array
						replaceFrom: newFirst
						to: newLast
						with: array
						startingAt: firstIndex.
					array from: newLast + 1 to: lastIndex put: nil.
					firstIndex _ newFirst.
					lastIndex _ newLast.
					^self
				]
		].
	newArray _ Array new: size + self growSize.
	newLast _ lastIndex - firstIndex + 1.
	newArray replaceFrom: 1 to: newLast with: array startingAt: firstIndex.
	array _ newArray.
	firstIndex _ 1.
	lastIndex _ newLast
</details>

#### OrderedCollection>>#reversed

Answer a copy of the receiver with element order reversed.


<details>
	<summary>See more</summary>
	
	reversed
	"Answer a copy of the receiver with element order reversed.  "
	| newCol |
	newCol _ self species new.
	self reverseDo:
		[:elem | newCol addLast: elem].
	^ newCol

"#(2 3 4 'fred') reversed"
</details>

#### OrderedCollection>>#collect: aBlock from: fromIndex to: toIndex

Override superclass in order to use addLast:, not at:put:.


<details>
	<summary>See more</summary>
	
	collect: aBlock from: fromIndex to: toIndex
	"Override superclass in order to use addLast:, not at:put:."
	| result |
	(fromIndex < 1 or:[toIndex + firstIndex - 1 > lastIndex])
		ifTrue: [^self errorNoSuchElement].
	result _ self species new: toIndex - fromIndex + 1.
	firstIndex + fromIndex - 1 to: firstIndex + toIndex - 1 do:
		[:index | result addLast: (aBlock value: (array at: index))].
	^ result

</details>

#### OrderedCollection>>#asNewArray

Answer a new Array whose elements are the elements of the receiver. Optimized implementation.


<details>
	<summary>See more</summary>
	
	asNewArray
	^ array copyFrom: firstIndex to: lastIndex
</details>

#### OrderedCollection>>#removeFirst

Remove the first element of the receiver and answer it. If the receiver is empty, create an error notification.


<details>
	<summary>See more</summary>
	
	removeFirst
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject _ array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex _ firstIndex + 1.
	^ firstObject
</details>

#### OrderedCollection>>#with: otherCollection collect: twoArgBlock

Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result _ self species new: self size.
	1 to: self size do:
		[:index | result addLast: (twoArgBlock value: (self at: index)
									value: (otherCollection at: index))].
	^ result
</details>

#### OrderedCollection>>#removeLast

Remove the last element of the receiver and answer it. If the receiver is empty, create an error notification.


<details>
	<summary>See more</summary>
	
	removeLast
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject _ array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex _ lastIndex - 1.
	^ lastObject
</details>

#### OrderedCollection>>#addAllLast: aCollection

Add each element of aCollection at the end of me. Answer aCollection.


<details>
	<summary>See more</summary>
	
	addAllLast: aCollection 
	"Add each element of aCollection at the end of me. Answer aCollection."

	^aCollection do: [ :each | self addLast: each ]
</details>

#### OrderedCollection>>#copy

Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy.


<details>
	<summary>See more</summary>
	
	copy

	^self copyFrom: 1 to: self size
</details>

#### OrderedCollection>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size

	^lastIndex - firstIndex + 1
</details>

#### OrderedCollection>>#removeLast: n

Remove last n object into an array with last in last position


<details>
	<summary>See more</summary>
	
	removeLast: n
	"Remove last n object into an array with last in last position"

	| list |
	list _ Array new: n.
	n to: 1 by: -1 do: [:i |
		list at: i put: self removeLast].
	^ list
</details>

#### OrderedCollection>>#do: aBlock

Refer to the comment in Collection >> #do: Note: Subclasses need to redefine either #do: or #size (or both!).


<details>
	<summary>See more</summary>
	
	do: aBlock 

	firstIndex to: lastIndex do: [ :index |
		aBlock value: (array at: index) ]
</details>

#### OrderedCollection>>#removeFirst: n

Remove first n object into an array


<details>
	<summary>See more</summary>
	
	removeFirst: n
	"Remove first n object into an array"

	| list |
	list _ Array new: n.
	1 to: n do: [:i |
		list at: i put: self removeFirst].
	^ list
</details>

#### OrderedCollection>>#addAllFirst: anOrderedCollection

Add each element of anOrderedCollection at the beginning of the receiver. Answer anOrderedCollection.


<details>
	<summary>See more</summary>
	
	addAllFirst: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the 
	receiver. Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection
</details>

#### OrderedCollection>>#remove: oldObject ifAbsent: absentBlock

SequencableCollections cannot implement removing.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: absentBlock

	firstIndex to: lastIndex do: [ :index |
		oldObject = (array at: index)
			ifTrue: [
				self removeIndex: index.
				^ oldObject ]].
	^ absentBlock value
</details>

#### OrderedCollection>>#add: newObject beforeIndex: index

Add the argument, newObject, as an element of the receiver. Put it in the sequence just before index. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject beforeIndex: index 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just before index. Answer newObject."
	(index between: 1 and: self size+1) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index - 1.
	^ newObject
</details>

#### OrderedCollection>>#removeAt: index

<details>
	<summary>See more</summary>
	
	removeAt: index
	| removed |
	removed _ self at: index.
	self removeIndex: index + firstIndex - 1.
	^removed
</details>

#### OrderedCollection>>#copyReplaceFrom: start to: stop with: replacementCollection

Answer a copy of the receiver with replacementCollection's elements in place of the receiver's start'th to stop'th elements. This does not expect a 1-1 map from replacementCollection to the start to stop elements, so it will do an insert or append.


<details>
	<summary>See more</summary>
	
	copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection delta startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	delta _ 0.
	startIndex _ start.
	stopIndex _ stop.
	start < 1
		ifTrue: [startIndex _ stopIndex _ 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex _ stopIndex _ self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [self errorOutOfBounds].
					delta _ stopIndex - startIndex + 1]].
	newOrderedCollection _ 
		self species new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection
</details>

#### OrderedCollection>>#setCollection: anArray

<details>
	<summary>See more</summary>
	
	setCollection: anArray
	array _ anArray.
	self reset
</details>

#### OrderedCollection>>#find: oldObject

Answer an index in the range [firstIndex, lastIndex]


<details>
	<summary>See more</summary>
	
	find: oldObject
	"Answer an index in the range [firstIndex, lastIndex]"

	firstIndex to: lastIndex do: [ :index |
		(array at: index) = oldObject ifTrue: [ ^index ]].
	self errorNotFound: oldObject
</details>

#### OrderedCollection>>#species

aSortedCollection collect: should answer an OrderedCollection


<details>
	<summary>See more</summary>
	
	species
	"aSortedCollection collect: should answer an OrderedCollection"

	^OrderedCollection
</details>

#### OrderedCollection>>#sort

Sort this array into ascending order using the '<=' operator.


<details>
	<summary>See more</summary>
	
	sort
	"Sort this array into ascending order using the '<=' operator."

	self sort: nil
</details>

#### OrderedCollection>>#add: newObject before: oldObject

Add the argument, newObject, as an element of the receiver. Put it in the sequence just preceding oldObject. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject before: oldObject 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just preceding oldObject. Answer newObject."
	
	| index |
	index _ self find: oldObject.
	self insert: newObject before: index.
	^newObject
</details>

#### OrderedCollection>>#copyWith: newElement

Answer a copy of the receiver that is 1 bigger than the receiver and includes the argument, newElement, at the end.


<details>
	<summary>See more</summary>
	
	copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and 
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection _ self copy.
	newCollection add: newElement.
	^newCollection
</details>

#### OrderedCollection>>#sort: aSortBlock

Sort this collection using aSortBlock. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison.


<details>
	<summary>See more</summary>
	
	sort: aSortBlock 
	"Sort this collection using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one.
	If aSortBlock is nil then <= is used for comparison."

	self ifNotEmpty: [
		array
			mergeSortFrom: firstIndex
			to: lastIndex
			by: aSortBlock ]
</details>

#### OrderedCollection>>#addFirst: newObject

Add newObject to the beginning of the receiver. Answer newObject.


<details>
	<summary>See more</summary>
	
	addFirst: newObject 
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex _ firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject
</details>

#### OrderedCollection>>#with: otherCollection with: thirdCollection collect: threeArgBlock

Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection.


<details>
	<summary>See more</summary>
	
	with: otherCollection with: thirdCollection collect: threeArgBlock
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result _ self species new: self size.
	1 to: self size do: [ :index |
		result addLast:
			(threeArgBlock
				value: (self at: index)
				value: (otherCollection at: index)
				value: (thirdCollection at: index) )].
	^ result
</details>

#### OrderedCollection>>#add: newObject after: oldObject

Add the argument, newObject, as an element of the receiver. Put it in the sequence just succeeding oldObject. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject after: oldObject 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just succeeding oldObject. Answer newObject."
	
	| index |
	index _ self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject
</details>

#### OrderedCollection>>#addLast: newObject

Add newObject to the end of the receiver. Answer newObject.


<details>
	<summary>See more</summary>
	
	addLast: newObject 
	"Add newObject to the end of the receiver. Answer newObject."

	lastIndex = array size ifTrue: [self makeRoomAtLast].
	lastIndex _ lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject
</details>

#### OrderedCollection>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^self isEmpty not
</details>

#### OrderedCollection>>#reverseDo: aBlock

Evaluate aBlock with each of the receiver's elements as the argument, starting with the last element and taking each in sequence up to the first. For SequenceableCollections, this is the reverse of the enumeration for do:.


<details>
	<summary>See more</summary>
	
	reverseDo: aBlock 

	lastIndex to: firstIndex by: -1 do: [ :index |
		aBlock value: (array at: index) ]
</details>

#### OrderedCollection>>#makeRoomAtFirst

<details>
	<summary>See more</summary>
	
	makeRoomAtFirst

	| size newArray newFirst newLast lastGap writeGap |
	size _ self size.
	lastGap _ size - lastIndex.
	lastGap - 4 * 5 > size ifTrue:
		[
			newLast _ size - (lastGap bitShift: -5).
			writeGap _ newLast - lastIndex.
			writeGap > 0 ifTrue:
				[
					newFirst _ firstIndex + writeGap.
					newLast to: newFirst by: -1 do:
						[:each | array at: each put: (array at: each - writeGap)].
					array from: firstIndex to: newFirst - 1 put: nil.
					firstIndex _ newFirst.
					lastIndex _ newLast.
					^self
				]
		].
	newLast _ size + self growSize.
	newArray _ Array new: newLast.
	newFirst _ firstIndex + newLast - lastIndex.
	newArray replaceFrom: newFirst to: newLast with: array startingAt: firstIndex.
	array _ newArray.
	firstIndex _ newFirst.
	lastIndex _ newLast
</details>

#### OrderedCollection>>#= otherCollection

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."


	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self == otherCollection ifTrue: [^ true].
	self class == otherCollection class ifFalse: [^ false].
	^ self hasEqualElements: otherCollection
</details>

#### OrderedCollection>>#at: index ifAbsentPut: block

Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)


<details>
	<summary>See more</summary>
	
	at: index ifAbsentPut: block
	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"

	| v |
	index <= self size ifTrue: [
		^ (v _ self at: index)
			ifNotNil: [v]
			ifNil: [self at: index put: block value]
	].
	[self size < index] whileTrue: [self add: nil].
	^ self at: index put: block value
</details>

#### OrderedCollection>>#setContents: anArray

<details>
	<summary>See more</summary>
	
	setContents: anArray
	array _ anArray.
	firstIndex _ 1.
	lastIndex _ array size.
</details>

#### OrderedCollection>>#select: aBlock

Evaluate aBlock with each of my elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection _ self species new.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection
</details>

#### OrderedCollection>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector
</details>

#### OrderedCollection>>#copyFrom: startIndex to: endIndex

Answer a copy of the receiver that contains elements from position startIndex to endIndex.


<details>
	<summary>See more</summary>
	
	copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	| targetCollection |
	endIndex < startIndex ifTrue: [^self species new: 0].
	targetCollection _ self species new: endIndex + 1 - startIndex.
	startIndex to: endIndex do: [:index | targetCollection addLast: (self at: index)].
	^ targetCollection
</details>

#### OrderedCollection>>#at: anInteger

Answer my element at index anInteger. at: is used by a knowledgeable client to access an existing element


<details>
	<summary>See more</summary>
	
	at: anInteger 
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	(anInteger < 1 or: [anInteger + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^ array at: anInteger + firstIndex - 1]
</details>

#### OrderedCollection>>#withIndexCollect: elementAndIndexBlock

Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:.


<details>
	<summary>See more</summary>
	
	withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection _ self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (elementAndIndexBlock
			value: (array at: index)
			value: index - firstIndex + 1)].
	^ newCollection
</details>

#### OrderedCollection>>#add: newObject

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: newObject

	^self addLast: newObject
</details>

#### OrderedCollection>>#collect: aBlock

Evaluate aBlock with each of my elements as the argument. Collect the resulting values into a collection that is like me. Answer the new collection. Override superclass in order to use addLast:, not at:put:.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection _ self species new: self size.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection
</details>

#### OrderedCollection>>#removeAllSuchThat: aBlock

Remove each element of the receiver for which aBlock evaluates to true. The method in Collection is O(N^2), this is O(N).


<details>
	<summary>See more</summary>
	
	removeAllSuchThat: aBlock 
	"Remove each element of the receiver for which aBlock evaluates to true.
	The method in Collection is O(N^2), this is O(N)."

	| n |
	n := firstIndex.
	firstIndex to: lastIndex do: [:index |
	    (aBlock value: (array at: index)) ifFalse: [
			array at: n put: (array at: index).
			n := n + 1]].
	array from: n to: lastIndex put: nil.
	lastIndex := n - 1
</details>

#### OrderedCollection>>#resetTo: index

<details>
	<summary>See more</summary>
	
	resetTo: index
	firstIndex _ index.
	lastIndex _ firstIndex - 1
</details>

#### OrderedCollection>>#growSize

<details>
	<summary>See more</summary>
	
	growSize
	^ array size max: 2
</details>

#### OrderedCollection>>#add: newObject afterIndex: index

Add the argument, newObject, as an element of the receiver. Put it in the sequence just after index. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject afterIndex: index 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just after index. Answer newObject."
	(index between: 0 and: self size) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index.
	^ newObject
</details>

#### OrderedCollection>>#reset

<details>
	<summary>See more</summary>
	
	reset
	firstIndex _ array size // 3 max: 1.
	lastIndex _ firstIndex - 1
</details>

#### OrderedCollection>>#insert: anObject before: spot

Spot is an index in the range [firstIndex, lastIndex]


<details>
	<summary>See more</summary>
	
	insert: anObject before: spot
	"Spot is an index in the range [firstIndex, lastIndex]"

	| delta spotIndex|
	spotIndex _ spot.
	delta _ spotIndex - firstIndex.
	firstIndex = 1 ifTrue: 
		[self makeRoomAtFirst.
		spotIndex _ firstIndex + delta].
	firstIndex _ firstIndex - 1.
	array
		replaceFrom: firstIndex
		to: spotIndex - 2
		with: array
		startingAt: firstIndex + 1.
	array at: spotIndex - 1 put: anObject.
	^anObject
</details>

#### OrderedCollection>>#removeIndex: removedIndex

The removedIndex value is in the range [firstIndex, lastIndex]


<details>
	<summary>See more</summary>
	
	removeIndex: removedIndex
 	"The removedIndex value is in the range [firstIndex, lastIndex]"

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex _ lastIndex - 1
</details>

#### OrderedCollection>>#initializeOfSize: aSize

<details>
	<summary>See more</summary>
	
	initializeOfSize: aSize

	array := Array new: aSize.
	firstIndex := 1.
	lastIndex := aSize.
</details>

## OrderedDictionary

Like Python's OrderedDict

### Methods
#### OrderedDictionary>>#keysDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	orderedKeys do: aBlock 
</details>

#### OrderedDictionary>>#atNewIndex: index put: anAssociation

<details>
	<summary>See more</summary>
	
	atNewIndex: index put: anAssociation

	super atNewIndex: index put: anAssociation.
	orderedKeys add: anAssociation key
</details>

#### OrderedDictionary>>#removeKey: key ifAbsent: aBlock

Remove key (and its associated value) from the receiver. If key is not in the receiver, answer the result of evaluating aBlock. Otherwise, answer the value externally named by key.


<details>
	<summary>See more</summary>
	
	removeKey: key ifAbsent: aBlock 

	super removeKey: key ifAbsent: [
		^ aBlock value ].
	orderedKeys remove: key
</details>

#### OrderedDictionary>>#keysSortedSafely

Answer a sorted Collection containing the receiver's keys. Redefined from Dictionary: for us, propery sorted keys are keys in the order they were added.


<details>
	<summary>See more</summary>
	
	keysSortedSafely
	"Answer a sorted Collection containing the receiver's keys.
	Redefined from Dictionary: for us, propery sorted keys are keys in the order they were added."
	^ orderedKeys
</details>

#### OrderedDictionary>>#associationsDo: aBlock

Evaluate aBlock for each of the receiver's elements (key/value associations).


<details>
	<summary>See more</summary>
	
	associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	orderedKeys do: [ :key |
		aBlock value: (self associationAt: key ifAbsent: nil) ]
</details>

#### OrderedDictionary>>#init: n

Initialize array to an array size of n


<details>
	<summary>See more</summary>
	
	init: n

	super init: n.
	orderedKeys _ OrderedCollection new: n
</details>

#### OrderedDictionary>>#do: aBlock

Evaluate aBlock for each of the receiver's values.


<details>
	<summary>See more</summary>
	
	do: aBlock
	"Evaluate aBlock for each of the receiver's values."

	orderedKeys do: [ :key |
		aBlock value: (self at: key ifAbsent: nil) ]
</details>

## SharedQueue

I provide synchronized communication of arbitrary objects between Processes. An object is sent by sending the message nextPut: and received by sending the message next. If no object has been sent when a next message is sent, the Process requesting the object will be suspended until one is sent.

### Methods
#### SharedQueue>>#nextPut: value

Send value through the receiver. If a Process has been suspended waiting to receive a value through the receiver, allow it to proceed.


<details>
	<summary>See more</summary>
	
	nextPut: value 
	"Send value through the receiver. If a Process has been suspended 
	waiting to receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition _ writePosition + 1].
	readSynch signal.
	^value
</details>

#### SharedQueue>>#nextOrNil

Answer the object that was sent through the receiver first and has not yet been received by anyone. If no object has been sent, answer <nil>.


<details>
	<summary>See more</summary>
	
	nextOrNil
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone. If no object has been sent, answer <nil>."

	^accessProtect critical: [
		| value |
		readPosition >= writePosition ifTrue: [
			value := nil
		] ifFalse: [
			value := contentsArray at: readPosition.
			contentsArray at: readPosition put: nil.
			readPosition := readPosition + 1
		].
		readPosition >= writePosition ifTrue: [readSynch initSignals].
		value
	].
</details>

#### SharedQueue>>#nextOrNilSuchThat: aBlock

Answer the next object that satisfies aBlock, skipping any intermediate objects. If no object has been sent, answer <nil> and leave me intact. NOTA BENE: aBlock MUST NOT contain a non-local return (^).


<details>
	<summary>See more</summary>
	
	nextOrNilSuchThat: aBlock
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no object has been sent, answer <nil> and leave me intact.
	NOTA BENE:  aBlock MUST NOT contain a non-local return (^)."

	^accessProtect critical: [
		| value readPos |
		value := nil.
		readPos := readPosition.
		[readPos < writePosition and: [value isNil]] whileTrue: [
			value := contentsArray at: readPos.
			readPos := readPos + 1.
			(aBlock value: value) ifTrue: [
				readPosition to: readPos - 1 do: [ :j |
					contentsArray at: j put: nil.
				].
				readPosition := readPos.
			] ifFalse: [
				value := nil.
			].
		].
		readPosition >= writePosition ifTrue: [readSynch initSignals].
		value.
	].
"===
q := SharedQueue new.
1 to: 10 do: [ :i | q nextPut: i].
c := OrderedCollection new.
[
	v := q nextOrNilSuchThat: [ :e | e odd].
	v notNil
] whileTrue: [
	c add: {v. q size}
].
{c. q} explore
==="
</details>

#### SharedQueue>>#next

Answer the object that was sent through the receiver first and has not yet been received by anyone. If no object has been sent, suspend the requesting process until one is.


<details>
	<summary>See more</summary>
	
	next
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone. If no object has been sent, suspend the 
	requesting process until one is."

	readSynch wait.
	^accessProtect
		critical: [
			| value |
			readPosition = writePosition
					ifTrue: 
						[self error: 'Error in SharedQueue synchronization'.
						 value := nil]
					ifFalse: 
						[value := contentsArray at: readPosition.
						 contentsArray at: readPosition put: nil.
						 readPosition := readPosition + 1].
			value].
</details>

#### SharedQueue>>#makeRoomAtEnd

<details>
	<summary>See more</summary>
	
	makeRoomAtEnd

	| contentsSize |
	contentsSize := writePosition - readPosition.
	contentsSize * 2 > contentsArray size
		ifTrue: [
			"grow"
			contentsArray := (contentsArray class new: contentsArray size * 2)
				replaceFrom: 1
				to: contentsSize
				with: contentsArray
				startingAt: readPosition;
				yourself ]
		ifFalse: [
			(contentsArray size > 10 and: [ contentsSize * 4 <= contentsArray size ])
				ifTrue: [
					"shrink"
					contentsArray := (contentsArray class new: (contentsSize * 2 max: 10))
						replaceFrom: 1
						to: contentsSize
						with: contentsArray
						startingAt: readPosition;
						yourself ]
				ifFalse: [
					"just move the elements to the front"
					contentsArray
						replaceFrom: 1
						to: contentsSize
						with: contentsArray
						startingAt: readPosition.
					contentsArray
						from: contentsSize + 1
						to: contentsArray size
						put: nil ] ].
	readPosition := 1.
	writePosition := contentsSize + 1
</details>

#### SharedQueue>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	contentsArray := contentsArray copy.
	accessProtect := Semaphore forMutualExclusion.
	readSynch := Semaphore new
</details>

#### SharedQueue>>#isEmpty

Answer whether any objects have been sent through the receiver and not yet received by anyone.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether any objects have been sent through the receiver and 
	not yet received by anyone."

	^readPosition = writePosition
</details>

#### SharedQueue>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	"Print a guesstimate of the size of the queue without aquiring the lock properly"
	aStream nextPut: $(.
	aStream print: writePosition - readPosition.
	aStream nextPut: $).
</details>

#### SharedQueue>>#flushAllSuchThat: aBlock

Remove from the queue all objects that satisfy aBlock.


<details>
	<summary>See more</summary>
	
	flushAllSuchThat: aBlock
	"Remove from the queue all objects that satisfy aBlock."
	^accessProtect critical: [
		| value newReadPos |
		newReadPos := writePosition.
		writePosition-1 to: readPosition by: -1 do:
			[:i | value := contentsArray at: i.
			contentsArray at: i put: nil.
			(aBlock value: value) ifTrue: [
				"We take an element out of the queue, and therefore, we need to decrement 
				the readSynch signals"
				readSynch wait.
			] ifFalse: [
				newReadPos := newReadPos - 1.
				contentsArray at: newReadPos put: value]].
		readPosition := newReadPos.
		value]
</details>

#### SharedQueue>>#size

Answer the number of objects that have been sent through the receiver and not yet received by anyone.


<details>
	<summary>See more</summary>
	
	size
	"Answer the number of objects that have been sent through the
	receiver and not yet received by anyone."

	^writePosition - readPosition
</details>

#### SharedQueue>>#initialize: size

<details>
	<summary>See more</summary>
	
	initialize: size

	contentsArray := Array new: size.
	readPosition := 1.
	writePosition := 1.
	accessProtect := Semaphore forMutualExclusion.
	readSynch := Semaphore new
</details>

#### SharedQueue>>#peek

Answer the object that was sent through the receiver first and has not yet been received by anyone but do not remove it from the receiver. If no object has been sent, return nil


<details>
	<summary>See more</summary>
	
	peek
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone but do not remove it from the receiver. If 
	no object has been sent, return nil"

	^accessProtect
		critical: [
			| value |
			readPosition >= writePosition
					ifTrue: [readPosition := 1.
							writePosition := 1.
							value := nil]
					ifFalse: [value := contentsArray at: readPosition].
			value].
</details>

#### SharedQueue>>#flush

Throw out all pending contents


<details>
	<summary>See more</summary>
	
	flush
	"Throw out all pending contents"
	accessProtect critical: [
		"nil out flushed slots --bf 02/11/2006"
		contentsArray from: readPosition to: writePosition-1 put: nil.
		readPosition := 1.
		writePosition := 1.
		"Reset the read synchronization semaphore"
		readSynch initSignals].
</details>

## SortedCollection

I represent a collection of objects ordered by some property of the objects themselves. The ordering is specified in a BlockClosure.

### Methods
#### SortedCollection>>#sortBlock: aBlock

Make the argument, aBlock, be the criterion for ordering elements of the receiver.


<details>
	<summary>See more</summary>
	
	sortBlock: aBlock 
	"Make the argument, aBlock, be the criterion for ordering elements of the 
	receiver."

	sortBlock _ aBlock.
	self size > 0 ifTrue: [ self reSort ]
</details>

#### SortedCollection>>#addFirst: newObject

Add newObject to the beginning of the receiver. Answer newObject.


<details>
	<summary>See more</summary>
	
	addFirst: newObject
	self shouldNotImplement
</details>

#### SortedCollection>>#reSort

<details>
	<summary>See more</summary>
	
	reSort

	firstIndex < lastIndex ifTrue: [ 
		array quickSortFrom: firstIndex to: lastIndex by: sortBlock ]
</details>

#### SortedCollection>>#add: newObject

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: newObject
	^ super insert: newObject before: (self indexForInserting: newObject)
</details>

#### SortedCollection>>#indexForInserting: newObject

<details>
	<summary>See more</summary>
	
	indexForInserting: newObject

	| index low high |
	low _ firstIndex.
	high _ lastIndex.
	sortBlock
		ifNil: [
			[index _ high + low // 2.  low > high]
				whileFalse: [
					((array at: index) <= newObject)
						ifTrue: [low _ index + 1]
						ifFalse: [high _ index - 1]]]
		ifNotNil: [
			[index _ high + low // 2.  low > high]
				whileFalse: [
					(sortBlock value: (array at: index) value: newObject)
						ifTrue: [low _ index + 1]
						ifFalse: [high _ index - 1]]].
	^low
</details>

#### SortedCollection>>#at: anInteger put: anObject

Put anObject at element index anInteger. at:put: cannot be used to append, front or back, to an ordered collection; it is used by a knowledgeable client to replace an element.


<details>
	<summary>See more</summary>
	
	at: anInteger put: anObject
	self shouldNotImplement
</details>

#### SortedCollection>>#quickIndexOf: anElement

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	quickIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	| sortBlockToUse |
	sortBlockToUse _ sortBlock ifNil: [[:a :b | a <= b ]].
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | 0 ]
</details>

#### SortedCollection>>#indexOf: anElement startingAt: start ifAbsent: exceptionBlock

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	| sortBlockToUse firstCandidate |
	self isEmpty ifTrue: [^ exceptionBlock value].
	sortBlockToUse _ sortBlock ifNil: [[:a :b | a <= b ]].
	"if first element to check is it, answer so."
	firstCandidate _ self at: start.
	firstCandidate = anElement ifTrue: [ ^ start ].
	"if first element to check is already too late, answer 'NotHere' "
	(sortBlockToUse value: anElement value: firstCandidate) ifTrue: [
		^ exceptionBlock value ].
	"Ok. Look for it."
	^self
		findBinaryIndex: [ :some | 
			some = anElement
				ifTrue: [0]
				ifFalse: [ (sortBlockToUse value: anElement value: some) ifTrue: [-1] ifFalse: [1]]]
		do: [ :i | i ]
		ifNone: [ :i1 :i2 | exceptionBlock value ]
</details>

#### SortedCollection>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	array _ array copy.
	firstIndex _ firstIndex copy.
	lastIndex _ lastIndex copy
</details>

#### SortedCollection>>#= aSortedCollection

Answer true if my and aSortedCollection's species are the same, and if our blocks are the same, and if our elements are the same.


<details>
	<summary>See more</summary>
	
	= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	"Any object is equal to itself"
	self == aSortedCollection ifTrue: [ ^ true ].

	"species is not enough. a SortedCollection is never equal to an OrderedCollection"
	self class == aSortedCollection class ifFalse: [ ^ false ].

	sortBlock = aSortedCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: aSortedCollection 
</details>

#### SortedCollection>>#copy

Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not override shallowCopy.


<details>
	<summary>See more</summary>
	
	copy

	^self shallowCopy postCopy
</details>

#### SortedCollection>>#sortBlock

Answer the BlockClosure which is the criterion for sorting elements of the receiver.


<details>
	<summary>See more</summary>
	
	sortBlock
	"Answer the BlockClosure which is the criterion for sorting elements of 
	the receiver."

	^sortBlock
</details>

#### SortedCollection>>#select: aBlock

Evaluate aBlock with each of my elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	"Use self class and not self species. SortedCollection is special,
	the answer for #collect: (an OrderedCollection) is of different kind than
	the answer for #select: or #copy"
	newCollection _ self class sortBlock: sortBlock.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection
</details>

#### SortedCollection>>#addAll: aCollection

optimization


<details>
	<summary>See more</summary>
	
	addAll: aCollection
	"optimization"
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection
</details>

#### SortedCollection>>#insert: anObject before: spot

Spot is an index in the range [firstIndex, lastIndex]


<details>
	<summary>See more</summary>
	
	insert: anObject before: spot
	self shouldNotImplement
</details>

#### SortedCollection>>#defaultSort: i to: j

Sort elements i through j of self to be nondescending according to sortBlock.


<details>
	<summary>See more</summary>
	
	defaultSort: i to: j 
	"Sort elements i through j of self to be nondescending according to
	sortBlock."	"Assume the default sort block ([:x :y | x <= y])."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n _ j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di _ array at: i.
	dj _ array at: j.
	(di <= dj) "i.e., should di precede dj?"
		ifFalse: 
			[array swap: i with: j.
			 tt _ di.
			 di _ dj.
			 dj _ tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij _ (i + j) // 2.  "ij is the midpoint of i and j."
			 dij _ array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (di <= dij) "i.e. should di precede dij?"
			   ifTrue: 
				[(dij <= dj) "i.e., should dij precede dj?"
				  ifFalse: 
					[array swap: j with: ij.
					 dij _ dj]]
			   ifFalse:  "i.e. di should come after dij"
				[array swap: i with: ij.
				 dij _ di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k _ i.
				 l _ j.
				 [[l _ l - 1.  k <= l and: [dij <= (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k _ k + 1.  k <= l and: [(array at: k) <= dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self defaultSort: i to: l.
				self defaultSort: k to: j]]
</details>

