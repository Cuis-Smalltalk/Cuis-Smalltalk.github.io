## Bag

I represent an unordered collection of possibly duplicate elements. I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.

### Methods
#### Bag>>#detect: aBlock ifNone: exceptionBlock

Refer to the comment in Collection|detect:ifNone:.


<details>
	<summary>See more</summary>
	
	detect: aBlock ifNone: exceptionBlock 
	"Refer to the comment in Collection|detect:ifNone:."

	contents keysDo: [:each | (aBlock value: each) ifTrue: [^ each]].
	^ exceptionBlock value
</details>

#### Bag>>#add: newObject

Refer to the comment in Collection|add:.


<details>
	<summary>See more</summary>
	
	add: newObject 
	"Refer to the comment in Collection|add:."

	^self add: newObject withOccurrences: 1
</details>

#### Bag>>#at: index put: anObject

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: anObject

	self errorNotKeyed
</details>

#### Bag>>#asSet

Answer a set with the elements of the receiver


<details>
	<summary>See more</summary>
	
	asSet
	"Answer a set with the elements of the receiver"

	 ^contents keys asSet
</details>

#### Bag>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Bag or: [ super is: aSymbol ]
</details>

#### Bag>>#sortedElements

Answer with a collection of elements with counts, sorted by element.


<details>
	<summary>See more</summary>
	
	sortedElements
	"Answer with a collection of elements with counts, sorted by element."

	| elements |
	elements _ SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^elements
</details>

#### Bag>>#cumulativeCounts

Answer with a collection of cumulative percents covered by elements so far.


<details>
	<summary>See more</summary>
	
	cumulativeCounts
	"Answer with a collection of cumulative percents covered by elements so far."
	| s n |
	s _ self size / 100.0.
	n _ 0.
	^ self sortedCounts asArray collect: [ :a |
		n _ n + a key.
		(n / s roundTo: 0.1) -> a value]
</details>

#### Bag>>#remove: oldObject ifAbsent: exceptionBlock

Refer to the comment in Collection|remove:ifAbsent:.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: exceptionBlock 
	"Refer to the comment in Collection|remove:ifAbsent:."

	| count |
	count _ contents at: oldObject ifAbsent: [^ exceptionBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^ oldObject
</details>

#### Bag>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	contents _ contents copy
</details>

#### Bag>>#= aBag

Two bags are equal if (a) they are the same 'kind' of thing. (b) they have the same size. (c) each element occurs the same number of times in both of them


<details>
	<summary>See more</summary>
	
	= aBag
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them".

	self == aBag ifTrue: [ ^ true ].		"Any object is equal to itself"
	(aBag is: #Bag) ifFalse: [ ^ false ].
	self size = aBag size ifFalse: [ ^ false ].
	contents associationsDo: [ :assoc |
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [ ^ false ]].
	^true
</details>

#### Bag>>#sum

Faster than the superclass implementation when you hold many instances of the same value (which you probably do, otherwise you wouldn't be using a Bag).


<details>
	<summary>See more</summary>
	
	sum
	"Faster than the superclass implementation when you hold many instances of the same value (which you probably do, otherwise you wouldn't be using a Bag)."
	
	| sum first |
	first := true.
	contents keysAndValuesDo: [ :value :count |
		first 
			ifTrue: [ sum := value * count. first := false ]
			ifFalse: [ sum := sum + (value * count) ] ].
	first ifTrue: [ self errorEmptyCollection ].
	^sum
</details>

#### Bag>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size

	^contents inject: 0 into: [ :prevValue :each | prevValue + each ]
</details>

#### Bag>>#includes: anObject

Refer to the comment in Collection|includes:.


<details>
	<summary>See more</summary>
	
	includes: anObject 
	"Refer to the comment in Collection|includes:."

	^contents includesKey: anObject
</details>

#### Bag>>#add: newObject withOccurrences: anInteger

Add the element newObject to the receiver. Do so as though the element were added anInteger number of times. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject withOccurrences: anInteger 
	"Add the element newObject to the receiver. Do so as though the element  
	were added anInteger number of times. Answer newObject."
	contents at: newObject put: (contents at: newObject ifAbsent: [0])
			+ anInteger.
	^ newObject
</details>

#### Bag>>#do: aBlock

Refer to the comment in Collection|do:.


<details>
	<summary>See more</summary>
	
	do: aBlock 
	"Refer to the comment in Collection|do:."

	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]
</details>

#### Bag>>#setContents: aDictionary

<details>
	<summary>See more</summary>
	
	setContents: aDictionary
	contents _ aDictionary
</details>

#### Bag>>#select: aBlock

Refer to the comment in Collection|select:.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Refer to the comment in Collection|select:."

	| newCollection |
	newCollection _ self species new.
	contents associationsDo: [:each |
		(aBlock value: each key)
			ifTrue: [newCollection add: each key withOccurrences: each value]].
	^ newCollection
</details>

#### Bag>>#addAll: aCollection

Refer to the comment in Collection|addAll:.


<details>
	<summary>See more</summary>
	
	addAll: aCollection
	"Refer to the comment in Collection|addAll:."

	(aCollection is: #Bag) ifFalse: [^ super addAll: aCollection].
	aCollection contents associationsDo: [ :each |
		self add: each key withOccurrences: each value].
	^ aCollection
</details>

#### Bag>>#sortedCounts

Answer with a collection of counts with elements, sorted by decreasing count.


<details>
	<summary>See more</summary>
	
	sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing
	count."

	| counts |
	counts _ SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^counts
</details>

#### Bag>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index

	self errorNotKeyed
</details>

#### Bag>>#contents

<details>
	<summary>See more</summary>
	
	contents
	^contents

</details>

#### Bag>>#occurrencesOf: anObject

Refer to the comment in Collection|occurrencesOf:.


<details>
	<summary>See more</summary>
	
	occurrencesOf: anObject 
	"Refer to the comment in Collection|occurrencesOf:."

	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]
</details>

## Dictionary

I represent a set of elements that can be viewed from one of two perspectives: a set of associations, or a container of values that are externally named where the name can be any object that responds to =. The external name is referred to as the key. I inherit many operations from Set.

### Methods
#### Dictionary>>#bindingOf: varName

<details>
	<summary>See more</summary>
	
	bindingOf: varName

	^self associationAt: varName ifAbsent: nil
</details>

#### Dictionary>>#at: key ifPresentAndInMemory: aBlock

Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil.


<details>
	<summary>See more</summary>
	
	at: key ifPresentAndInMemory: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [^ nil].
	v isInMemory ifFalse: [^ nil].
	^ aBlock value: v

</details>

#### Dictionary>>#keys

Answer an Array containing the receiver's keys.


<details>
	<summary>See more</summary>
	
	keys
	"Answer an Array containing the receiver's keys."
	
	^Array streamContents: [:s| self keysDo: [:key| s nextPut: key]] estimatedSize: self size
</details>

#### Dictionary>>#at: key put: anObject

Set the value at key to be anObject. If key is not found, create a new entry for key and set is value to anObject. If key is found, update the existing association. Answer anObject.


<details>
	<summary>See more</summary>
	
	at: key put: anObject
	"Set the value at key to be anObject. 
	If key is not found, create a new entry for key and set is value to anObject.
	If key is found, update the existing association.
	Answer anObject."

	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	assoc
		ifNil: [ self atNewIndex: index put: (Association key: key value: anObject) ]
		ifNotNil: [ assoc value: anObject ].
	^ anObject
</details>

#### Dictionary>>#declare: key from: aDictionary

Add key to the receiver. If key already exists, do nothing. If aDictionary includes key, then remove it from aDictionary and use its association as the element of the receiver. Answer it. If the key wasn't in the receiver or aDictionary, use nil as the value.


<details>
	<summary>See more</summary>
	
	declare: key from: aDictionary
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver. Answer it.
	If the key wasn't in the receiver or aDictionary, use nil as the value."

	| association |
	(self includesKey: key) ifTrue: [^ nil].
	(aDictionary includesKey: key)
		ifTrue: [
			association _ aDictionary associationAt: key.
			self add: association.
			aDictionary removeKey: key.
			^ association ]
		ifFalse: [
			self at: key put: nil.
			^ nil ]
</details>

#### Dictionary>>#removeKey: key ifAbsent: aBlock

Remove key (and its associated value) from the receiver. If key is not in the receiver, answer the result of evaluating aBlock. Otherwise, answer the value externally named by key.


<details>
	<summary>See more</summary>
	
	removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	assoc
		ifNil: [^ aBlock value].
	array at: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value
</details>

#### Dictionary>>#keysSortedSafely

Answer a sorted Collection containing the receiver's keys.


<details>
	<summary>See more</summary>
	
	keysSortedSafely
	"Answer a sorted Collection containing the receiver's keys."
	| sortedKeys |
	sortedKeys _ OrderedCollection new: self size.
	self keysDo: [:each | sortedKeys addLast: each].
	sortedKeys sort:
		[ :x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys
</details>

#### Dictionary>>#removeUnreferencedKeys

Undeclared removeUnreferencedKeys


<details>
	<summary>See more</summary>
	
	removeUnreferencedKeys   "Undeclared removeUnreferencedKeys"

	^ self unreferencedKeys do: [:key | self removeKey: key].
</details>

#### Dictionary>>#removeKey: key

Remove key from the receiver. If key is not in the receiver, notify an error.


<details>
	<summary>See more</summary>
	
	removeKey: key 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: key ifAbsent: [self errorKeyNotFound]
</details>

#### Dictionary>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock

	super do: [:assoc | aBlock value: assoc value]
</details>

#### Dictionary>>#bindingsDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingsDo: aBlock
	^self associationsDo: aBlock
</details>

#### Dictionary>>#keysAndValuesRemove: keyValueBlock

Removes all entries for which keyValueBlock returns true.


<details>
	<summary>See more</summary>
	
	keysAndValuesRemove: keyValueBlock
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals _ OrderedCollection new.
	self associationsDo:
		[:assoc | (keyValueBlock value: assoc key value: assoc value)
			ifTrue: [removals add: assoc key]].
 	removals do:
		[:aKey | self removeKey: aKey]
</details>

#### Dictionary>>#includesKey: key

Answer whether the receiver has a key equal to the argument, key.


<details>
	<summary>See more</summary>
	
	includesKey: key 
	"Answer whether the receiver has a key equal to the argument, key."
	
	self at: key ifAbsent: [^false].
	^true
</details>

#### Dictionary>>#occurrencesOf: anObject

Answer how many of the receiver's elements are equal to anObject.


<details>
	<summary>See more</summary>
	
	occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count _ 0.
	self do: [:each | anObject = each ifTrue: [count _ count + 1]].
	^count
</details>

#### Dictionary>>#at: key ifAbsent: absentBlock

Answer the value associated with the key or, if key isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	at: key ifAbsent: absentBlock
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| assoc |
	assoc _ array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ assoc value
</details>

#### Dictionary>>#valuesDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	valuesDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association value]
</details>

#### Dictionary>>#keyAt: index

May be overridden by subclasses so that fixCollisionsFrom: will work


<details>
	<summary>See more</summary>
	
	keyAt: index
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	| assn |
	assn _ array at: index.
	assn ifNil: [^ nil].
	^ assn key
</details>

#### Dictionary>>#values

Answer a Collection containing the receiver's values.


<details>
	<summary>See more</summary>
	
	values
	"Answer a Collection containing the receiver's values."
	| out |
	out _ WriteStream on: (Array new: self size).
	self valuesDo: [:value | out nextPut: value].
	^ out contents
</details>

#### Dictionary>>#errorValueNotFound

<details>
	<summary>See more</summary>
	
	errorValueNotFound

	self error: 'value not found'
</details>

#### Dictionary>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #Dictionary or: [ super is: aSymbol ]
</details>

#### Dictionary>>#unreferencedKeys

<details>
	<summary>See more</summary>
	
	unreferencedKeys
	| currentClass associations referencedAssociations |
	currentClass := nil.
	associations := self associations asIdentitySet.
	referencedAssociations := IdentitySet new: associations size.
	Smalltalk allSelect: [ :m |
		m methodClass ~~ currentClass ifTrue: [
			currentClass := m methodClass ].
		m literalsDo: [ :l |
			(l isVariableBinding and: [associations includes: l]) ifTrue: [
				referencedAssociations add: l]].
		false ].
	^((associations reject: [:assoc | referencedAssociations includes: assoc]) collect: [:assoc| assoc key]) asSet
</details>

#### Dictionary>>#at: key ifPresent: presentBlock

Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil.


<details>
	<summary>See more</summary>
	
	at: key ifPresent: presentBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [ ^ nil ].
	^ presentBlock value: v

</details>

#### Dictionary>>#associations

Answer a Collection containing the receiver's associations.


<details>
	<summary>See more</summary>
	
	associations
	"Answer a Collection containing the receiver's associations."
	| out |
	out _ WriteStream on: (Array new: self size).
	self associationsDo: [:value | out nextPut: value].
	^ out contents
</details>

#### Dictionary>>#postCopy

Must copy the associations, or later store will affect both the original and the copy


<details>
	<summary>See more</summary>
	
	postCopy
	"Must copy the associations, or later store will affect both the
original and the copy"

	array _ array collect: [:each | each copy]
</details>

#### Dictionary>>#remove: anObject ifAbsent: exceptionBlock

Remove oldObject from the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock. Otherwise, answer the argument, oldObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: anObject ifAbsent: exceptionBlock

	self shouldNotImplement
</details>

#### Dictionary>>#printElementsOn: aStream

<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	aStream nextPut: $(.
	self keysSortedSafely do:
		[:key | aStream print: key; nextPutAll: '->'; print: (self at: key); space].
	aStream nextPut: $)
</details>

#### Dictionary>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### Dictionary>>#keyForIdentity: anObject

If anObject is one of the values of the receive, return its key, else return nil. Contrast #keyAtValue: in which there is only an equality check, here there is an identity check


<details>
	<summary>See more</summary>
	
	keyForIdentity: anObject
	"If anObject is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self associationsDo: [:assoc | assoc value == anObject ifTrue: [^ assoc key]].
	^ nil
</details>

#### Dictionary>>#associationAt: key ifAbsent: aBlock

Answer the association with the given key. If key is not found, return the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index _ self findElementOrNil: key.
	assoc _ array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc
</details>

#### Dictionary>>#associationAt: key

<details>
	<summary>See more</summary>
	
	associationAt: key 
	^ self associationAt: key ifAbsent: [self errorKeyNotFound]
</details>

#### Dictionary>>#keyAtValue: value ifAbsent: exceptionBlock

Answer the key that is the external name for the argument, value. If there is none, answer the result of evaluating exceptionBlock. : Use =, not ==, so stings like 'this' can be found. Note that MethodDictionary continues to use == so it will be fast.


<details>
	<summary>See more</summary>
	
	keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."
 
	self associationsDo: 
		[:association | value = association value ifTrue: [^association key]].
	^exceptionBlock value
</details>

#### Dictionary>>#remove: anObject

Remove oldObject from the receiver's elements. Answer oldObject unless no element is equal to oldObject, in which case, raise an error. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: anObject

	self shouldNotImplement
</details>

#### Dictionary>>#customizeExplorerContents

<details>
	<summary>See more</summary>
	
	customizeExplorerContents

	^ true.

</details>

#### Dictionary>>#keysDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]
</details>

#### Dictionary>>#hasBindingThatBeginsWith: aString

Answer true if the receiver has a key that begins with aString, false otherwise


<details>
	<summary>See more</summary>
	
	hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	
	self keysDo:[:each | 
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false
</details>

#### Dictionary>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^self isEmpty not
</details>

#### Dictionary>>#= aDictionary

Two dictionaries are equal if (a) they are the same 'kind' of thing. (b) they have the same set of keys. (c) for each (common) key, they have the same value


<details>
	<summary>See more</summary>
	
	= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value".

	self == aDictionary ifTrue: [^ true].	"Any object is equal to itself"
	(aDictionary is: #Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true


</details>

#### Dictionary>>#errorKeyNotFound

<details>
	<summary>See more</summary>
	
	errorKeyNotFound

	self error: self class keyNotFoundErrorDescription 
</details>

#### Dictionary>>#keyAtValue: value

Answer the key that is the external name for the argument, value. If there is none, answer nil.


<details>
	<summary>See more</summary>
	
	keyAtValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]
</details>

#### Dictionary>>#keyAtIdentityValue: value ifAbsent: exceptionBlock

Answer the key that is the external name for the argument, value. If there is none, answer the result of evaluating exceptionBlock. Note: There can be multiple keys with the same value. Only one is returned.


<details>
	<summary>See more</summary>
	
	keyAtIdentityValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value
</details>

#### Dictionary>>#noCheckAdd: anObject

Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association. 9/7/96 tk


<details>
	<summary>See more</summary>
	
	noCheckAdd: anObject
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally _ tally + 1
</details>

#### Dictionary>>#includes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: anObject

	self do: [:each | anObject = each ifTrue: [^true]].
	^false
</details>

#### Dictionary>>#hash

Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior.


<details>
	<summary>See more</summary>
	
	hash
	"Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior."

	| hash |
	hash _ self emptyCollectionHash.
	self size <= 10 ifTrue: [
		self associationsDo: [ :association | hash _ hash bitXor: association hash ]].
	^ hash bitXor: self size hash
</details>

#### Dictionary>>#valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary

Support for coordinating class variable and global declarations with variables that have been put in Undeclared so as to redirect all references to the undeclared variable.


<details>
	<summary>See more</summary>
	
	valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary 
	"Support for coordinating class variable and global declarations
	with variables that have been put in Undeclared so as to
	redirect all references to the undeclared variable."

	(aDictionary includesKey: aKey)
		ifTrue: 
			[self atNewIndex: index 
				put: ((aDictionary associationAt: aKey) value: anObject).
			aDictionary removeKey: aKey]
		ifFalse: 
			[self atNewIndex: index put: (Association key: aKey value: anObject)]
</details>

#### Dictionary>>#at: key ifAbsentPut: aBlock

Return the value at the given key. If key is not included in the receiver store the result of evaluating aBlock as new value.


<details>
	<summary>See more</summary>
	
	at: key ifAbsentPut: aBlock 
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."
	^self at: key ifAbsent:[self at: key put: aBlock value]
</details>

#### Dictionary>>#select: aBlock

Evaluate aBlock with each of my values as the argument. Collect into a new dictionary, only those associations for which aBlock evaluates to true.


<details>
	<summary>See more</summary>
	
	select: aBlock 
	"Evaluate aBlock with each of my values as the argument. Collect into a
	new dictionary, only those associations for which aBlock evaluates to
	true."

	| newCollection |
	newCollection _ self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection
</details>

#### Dictionary>>#addAll: aCollection

It should hold Associations, then


<details>
	<summary>See more</summary>
	
	addAll: aCollection

	"It should hold Associations, then"
	(aCollection is: #Dictionary) ifFalse: [
		^super addAll: aCollection ].
	
	aCollection == self ifFalse: [
		aCollection keysAndValuesDo: [:key :value |
			self at: key put: value]].
	
	^aCollection
</details>

#### Dictionary>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ DictionaryInspector
</details>

#### Dictionary>>#at: key

Answer the value associated with the key.


<details>
	<summary>See more</summary>
	
	at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound]
</details>

#### Dictionary>>#rehash

Smalltalk rehash.


<details>
	<summary>See more</summary>
	
	rehash
	"Smalltalk rehash."
	| newSelf |
	newSelf _ self species new: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array _ newSelf array
</details>

#### Dictionary>>#add: anAssociation

Quite like doing self at: anAssociation key put: anAssociation value but making sure the argument is stored. This method should be used when the argument is stored elsewhere, and its value should be kept in sync with the value stored in me. If the key already exists, and it is desired to keep the existing association, then call #at:put:


<details>
	<summary>See more</summary>
	
	add: anAssociation
	"Quite like doing
		self at: anAssociation key put: anAssociation value
	but making sure the argument is stored.
	This method should be used when the argument is stored elsewhere,
	and its value should be kept in sync with the value stored in me.

	If the key already exists, and it is desired to keep the existing association, then call #at:put:"

	| index |
	index _ self findElementOrNil: anAssociation key.
	(array at: index)
		ifNotNil: [ array at: index put: anAssociation ]
		ifNil: [ self atNewIndex: index put: anAssociation ].
	^ anAssociation
</details>

#### Dictionary>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self associationsDo: 
			[:each | 
			noneYet
				ifTrue: [noneYet _ false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
</details>

#### Dictionary>>#emptyCollectionHash

<details>
	<summary>See more</summary>
	
	emptyCollectionHash
	^ Dictionary hash
</details>

#### Dictionary>>#collect: aBlock

Evaluate aBlock with each of my values as the argument. Collect the resulting values into a collection that is like me. Answer with the new collection.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the resulting values into a collection that is like me. Answer with the new collection."
	
	| newCollection |
	newCollection _ self species new: self size.
	self associationsDo: [ :each |
		newCollection at: each key put: (aBlock value: each value) ].
	^newCollection
</details>

#### Dictionary>>#explorerContentsWithIndexCollect: twoArgBlock

<details>
	<summary>See more</summary>
	
	explorerContentsWithIndexCollect: twoArgBlock

	| sortedKeys |
	sortedKeys _ self keys sort: [:x :y |
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	^ sortedKeys collect: [:k | twoArgBlock value: (self at: k) value: k].

</details>

#### Dictionary>>#associationsDo: aBlock

Evaluate aBlock for each of the receiver's elements (key/value associations).


<details>
	<summary>See more</summary>
	
	associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	super do: aBlock
</details>

#### Dictionary>>#keyAtIdentityValue: value

Answer the key that is the external name for the argument, value. If there is none, answer nil. Note: There can be multiple keys with the same value. Only one is returned.


<details>
	<summary>See more</summary>
	
	keyAtIdentityValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound]
</details>

#### Dictionary>>#at: key ifPresent: presentBlock ifAbsent: absentBlock

Answer the value associated with the key or, if key isn't found, answer the result of evaluating aBlock.


<details>
	<summary>See more</summary>
	
	at: key ifPresent: presentBlock ifAbsent: absentBlock
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."
	"
		Smalltalk at: #zork ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'zork absent' print ]
		Smalltalk at: #Number ifPresent: [ :cls | (cls name, ' present') print ] ifAbsent: [ 'Number absent' print ]
	"

	| assoc |
	assoc _ array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ absentBlock value ].
	^ presentBlock value: assoc value
</details>

#### Dictionary>>#keysAndValuesDo: aBlock

<details>
	<summary>See more</summary>
	
	keysAndValuesDo: aBlock
	^self associationsDo:[:assoc|
		aBlock value: assoc key value: assoc value].
</details>

## IdentityBag

Like a Bag, except that items are compared with #== instead of #= . See the comment of IdentitySet for more information.

### Methods
## IdentityDictionary

Like a Dictionary, except that keys are compared with #== instead of #= . See the comment of IdentitySet for more information.

### Methods
#### IdentityDictionary>>#keyAtValue: value ifAbsent: exceptionBlock

Answer the key that is the external name for the argument, value. If there is none, answer the result of evaluating exceptionBlock.


<details>
	<summary>See more</summary>
	
	keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^ association key]].
	^ exceptionBlock value
</details>

#### IdentityDictionary>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

## IdentitySet

The same as a Set, except that items are compared using #== instead of #=. Almost any class named IdentityFoo is the same as Foo except for the way items are compared. In Foo, #= is used, while in IdentityFoo, #== is used. That is, identity collections will treat items as the same only if they have the same identity. For example, note that copies of a string are equal: ('abc' copy) = ('abc' copy) but they are not identitcal: ('abc' copy) == ('abc' copy) A regular Set will only include equal objects once: | aSet | aSet := Set new. aSet add: 'abc' copy. aSet add: 'abc' copy. aSet An IdentitySet will include multiple equal objects if they are not identical: | aSet | aSet := IdentitySet new. aSet add: 'abc' copy. aSet add: 'abc' copy. aSet

### Methods
#### IdentitySet>>#elementForIdentityHash: aNumber

Answer any element matching argument. Answer nil if none found


<details>
	<summary>See more</summary>
	
	elementForIdentityHash: aNumber
	"Answer any element matching argument.
	Answer nil if none found"
	| finish scale index element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	index _ aNumber * scale \\ finish + 1.

	element _ array at: index.
	^element identityHash = aNumber ifTrue: [ element ]
</details>

#### IdentitySet>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

## Set

I represent a set of objects without duplicates. I can hold anything that responds to #hash and #=, except for nil. My instances will automatically grow, if necessary, Note that I rely on #=, not #==. If you want a set using #==, use IdentitySet. Instance structure: array An array whose non-nil elements are the elements of the set, and whose nil elements are empty slots. There is always at least one nil. In fact I try to keep my "load" at 75% or less so that hashing will work well. tally The number of elements in the set. The array size is always greater than this. The core operation is #findElementOrNil:, which either finds the position where an object is stored in array, if it is present, or finds a suitable position holding nil, if its argument is not present in array,

### Methods
#### Set>>#grow

Grow the elements array and reinsert the old elements


<details>
	<summary>See more</summary>
	
	grow
	"Grow the elements array and reinsert the old elements"
	| oldElements |
	oldElements _ array.
	array _ Array new: array size + self growSize.
	tally _ 0.
	oldElements do: [ :each |
		each ifNotNil: [self noCheckAdd: each]]
</details>

#### Set>>#atRandom: aGenerator

Answer a random element of the receiver. Uses aGenerator which should be kept by the user in a variable and used every time. Use this instead of #atRandom for better uniformity of random numbers because only you use the generator. Causes an error if self has no elements.


<details>
	<summary>See more</summary>
	
	atRandom: aGenerator 
	"Answer a random element of the receiver. Uses aGenerator which 
	should be kept by the user in a variable and used every time. Use 
	this instead of #atRandom for better uniformity of random numbers  
	because only you use the generator. Causes an error if self has no  
	elements."
	self emptyCheck.
	[(array atRandom: aGenerator) ifNotNil: [:element| ^ element]] repeat
</details>

#### Set>>#removeAll

<details>
	<summary>See more</summary>
	
	removeAll
	self init: array size
</details>

#### Set>>#like: anObject

Answer an object in the receiver that is equal to anObject, nil if no such object is found. Relies heavily on hash properties


<details>
	<summary>See more</summary>
	
	like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index |

	^(index _ self scanFor: anObject) = 0
		ifFalse: [array at: index]
</details>

#### Set>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^self notEmpty
</details>

#### Set>>#atNewIndex: index put: anObject

<details>
	<summary>See more</summary>
	
	atNewIndex: index put: anObject
	array at: index put: anObject.
	tally _ tally + 1.
	self fullCheck.
	^ anObject
</details>

#### Set>>#= aSet

Default implementation. Usually redefined in subclasses.


<details>
	<summary>See more</summary>
	
	= aSet

	self == aSet ifTrue: [^ true].	"Any object is equal to itself"
	(aSet is: #Set) ifFalse: [^ false].
	(aSet is: #Dictionary) ifTrue: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [ :each | (aSet includes: each) ifFalse: [^ false]].
	^ true
</details>

#### Set>>#noCheckAdd: anObject

<details>
	<summary>See more</summary>
	
	noCheckAdd: anObject
	array at: (self findElementOrNil: anObject) put: anObject.
	tally _ tally + 1
</details>

#### Set>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size
	^ tally
</details>

#### Set>>#init: n

Initialize array to an array size of n


<details>
	<summary>See more</summary>
	
	init: n
	"Initialize array to an array size of n"
	array _ Array new: n.
	tally _ 0
</details>

#### Set>>#includes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: anObject 
	^ (array at: (self findElementOrNil: anObject)) ~~ nil
</details>

#### Set>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock 
	| each |
	tally = 0 ifTrue: [^ self].
	1 to: array size do: [ :index |
		(each _ array at: index) ifNotNil: [aBlock value: each]]
</details>

#### Set>>#swap: oneIndex with: otherIndex

May be overridden by subclasses so that fixCollisionsFrom: will work


<details>
	<summary>See more</summary>
	
	swap: oneIndex with: otherIndex
	"May be overridden by subclasses so that fixCollisionsFrom: will work"

	array swap: oneIndex with: otherIndex

</details>

#### Set>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ SetInspector
</details>

#### Set>>#copyWithout: oldElement

Answer a copy of the receiver that does not contain any elements equal to oldElement.


<details>
	<summary>See more</summary>
	
	copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any elements equal
	to oldElement."
	^ self copy
		remove: oldElement ifAbsent: nil;
		yourself
</details>

#### Set>>#occurrencesOf: anObject

Answer how many of the receiver's elements are equal to anObject.


<details>
	<summary>See more</summary>
	
	occurrencesOf: anObject 
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]
</details>

#### Set>>#rehash

Do nothing. Here so sending this to a Set does not have to do a time consuming respondsTo:


<details>
	<summary>See more</summary>
	
	rehash
	| newSelf |
	newSelf _ self species new: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array _ newSelf array
</details>

#### Set>>#add: newObject

Include newObject as one of the receiver's elements, but only if not already present. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject].
	^ newObject
</details>

#### Set>>#emptyCollectionHash

<details>
	<summary>See more</summary>
	
	emptyCollectionHash
	^ Set hash
</details>

#### Set>>#keyAt: index

May be overridden by subclasses so that fixCollisionsFrom: will work


<details>
	<summary>See more</summary>
	
	keyAt: index
	"May be overridden by subclasses so that fixCollisionsFrom: will work"
	^ array at: index
</details>

#### Set>>#findElementOrNil: anObject

Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found.


<details>
	<summary>See more</summary>
	
	findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."
	| index |
	index _ self scanFor: anObject.
	index > 0 ifTrue: [ ^ index ].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!'.
</details>

#### Set>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #Set or: [ super is: aSymbol ]
</details>

#### Set>>#collect: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection like the receiver. Answer the new collection.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet _ self species new: self size.
	array do: [ :each | each ifNotNil: [ newSet add: (aBlock value: each)]].
	^ newSet
</details>

#### Set>>#remove: oldObject ifAbsent: aBlock

Remove oldObject from the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock. Otherwise, answer the argument, oldObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: aBlock

	| index |
	index _ self findElementOrNil: oldObject.
	(array at: index) ifNil: [ ^ aBlock value ].
	array at: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject
</details>

#### Set>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	array _ array copy
</details>

#### Set>>#fullCheck

Keep array at least 1/4 free for decent hash behavior


<details>
	<summary>See more</summary>
	
	fullCheck
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow]
</details>

#### Set>>#array

<details>
	<summary>See more</summary>
	
	array
	^ array
</details>

#### Set>>#withArray: anArray

private -- for use only in copy


<details>
	<summary>See more</summary>
	
	withArray: anArray
	"private -- for use only in copy"
	array _ anArray
</details>

#### Set>>#growSize

<details>
	<summary>See more</summary>
	
	growSize
	^ array size max: 2
</details>

#### Set>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### Set>>#union: aCollection

Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all.


<details>
	<summary>See more</summary>
	
	union: aCollection
	"Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all."

	^ self copy addAll: aCollection; yourself


</details>

#### Set>>#comeFullyUpOnReload: smartRefStream

Symbols have new hashes in this image.


<details>
	<summary>See more</summary>
	
	comeFullyUpOnReload: smartRefStream
	"Symbols have new hashes in this image."

	self rehash.
	"^ self"

</details>

#### Set>>#add: newObject withOccurrences: anInteger

Add newObject anInteger times to the receiver. Answer newObject.


<details>
	<summary>See more</summary>
	
	add: newObject withOccurrences: anInteger
	^ self add: newObject
</details>

#### Set>>#fixCollisionsFrom: index

The element at index has been removed and replaced by nil. This method moves forward from there, relocating any entries that had been placed below due to collisions with this one


<details>
	<summary>See more</summary>
	
	fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| length oldIndex newIndex element |

	oldIndex _ index.
	length _ array size.
	[oldIndex = length
			ifTrue: [oldIndex _ 1]
			ifFalse: [oldIndex _ oldIndex + 1].
	(element _ self keyAt: oldIndex) == nil]
		whileFalse: 
			[newIndex _ self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]
</details>

