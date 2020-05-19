## WeakArray

WeakArray is an array which holds only weakly on its elements. This means whenever an object is only referenced by instances of WeakArray it will be garbage collected.

### Methods
## WeakIdentityKeyDictionary

This class represents an identity dictionary with weak keys.

### Methods
#### WeakIdentityKeyDictionary>>#scanForNil: anObject

Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot.


<details>
	<summary>See more</summary>
	
	scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."

	| finish scale start |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### WeakIdentityKeyDictionary>>#scanFor: anObject

ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
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

## WeakIdentitySet

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### WeakIdentitySet>>#elementForIdentityHash: aNumber

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
	element == flag ifTrue: [ ^ nil ].
	^element identityHash = aNumber ifTrue: [ element ]
</details>

#### WeakIdentitySet>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| finish scale start element |
	finish _ array size.
	scale _ finish // (Smalltalk maxIdentityHash + 1).
	scale = 0 ifTrue: [scale _ 1].
	start _ anObject identityHash * scale \\ finish + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == flag or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

## WeakKeyDictionary

I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys. See WeakRegistry for an example of use.

### Methods
#### WeakKeyDictionary>>#add: anAssociation

Quite like doing self at: anAssociation key put: anAssociation value but making sure the argument is stored. This method should be used when the argument is stored elsewhere, and its value should be kept in sync with the value stored in me. If the key already exists, and it is desired to keep the existing association, then call #at:put:


<details>
	<summary>See more</summary>
	
	add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
</details>

#### WeakKeyDictionary>>#scanFor: anObject

ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well.


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
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

#### WeakKeyDictionary>>#at: key put: anObject

Set the value at key to be anObject. If key is not found, create a new entry for key and set is value to anObject. Answer anObject.


<details>
	<summary>See more</summary>
	
	at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	key ifNil: [ ^anObject].
	index _ self findElementOrNil: key.
	element _ array at: index.
	element
		ifNil: [self atNewIndex: index put: (WeakKeyAssociation key: key value: anObject)]
		ifNotNil: [element value: anObject].
	^ anObject
</details>

#### WeakKeyDictionary>>#finalizeValues: finiObjects

Remove all associations with key == nil and value is in finiObjects. This method is folded with #rehash for efficiency.


<details>
	<summary>See more</summary>
	
	finalizeValues: finiObjects
	"Remove all associations with key == nil and value is in finiObjects.
	This method is folded with #rehash for efficiency."
	| oldArray assoc newIndex |
	oldArray _ array.
	array _ Array new: oldArray size.
	tally _ 0.
	1 to: array size do:[:i|
		assoc _ oldArray at: i.
		assoc ifNotNil:[
			(assoc key == nil and:[finiObjects includes: assoc value]) ifFalse:[
				newIndex _ self scanForNil: assoc key.
				self atNewIndex: newIndex put: assoc].
		].
	].
</details>

#### WeakKeyDictionary>>#slowSize

Careful! Answer the maximum amount of elements in the receiver, not the exact amount


<details>
	<summary>See more</summary>
	
	slowSize
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object key ifNotNil: [
				count := count + 1 ] ] ].
	^count
</details>

#### WeakKeyDictionary>>#fixCollisionsFrom: oldIndex

The element at index has been removed and replaced by nil.


<details>
	<summary>See more</summary>
	
	fixCollisionsFrom: oldIndex
	"The element at index has been removed and replaced by nil."
	self rehash. "Do it the hard way - we may have any number of nil keys and #rehash deals with them"
</details>

#### WeakKeyDictionary>>#keysDo: aBlock

Evaluate aBlock for each of the receiver's keys.


<details>
	<summary>See more</summary>
	
	keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."
	self associationsDo: [:association | 
		association key ifNotNil:[aBlock value: association key]].
</details>

#### WeakKeyDictionary>>#scanForNil: anObject

Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot.


<details>
	<summary>See more</summary>
	
	scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."
	| start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### WeakKeyDictionary>>#finalizeValues

remove all nil keys and rehash the receiver afterwards


<details>
	<summary>See more</summary>
	
	finalizeValues
	"remove all nil keys and rehash the receiver afterwards"
	| assoc |
	1 to: array size do:[:i|
		assoc _ array at: i.
		(assoc notNil and:[assoc key == nil]) ifTrue:[array at: i put: nil].
	].
	self rehash.
</details>

#### WeakKeyDictionary>>#rehash

Rehash the receiver. Reimplemented to allow for multiple nil keys


<details>
	<summary>See more</summary>
	
	rehash
	"Rehash the receiver. Reimplemented to allow for multiple nil keys"
	| oldArray assoc newIndex |
	oldArray _ array.
	array _ Array new: oldArray size.
	tally _ 0.
	1 to: array size do:[:i|
		assoc _ oldArray at: i.
		assoc ifNotNil:[
			newIndex _ self scanForNil: assoc key.
			self atNewIndex: newIndex put: assoc.
		].
	].
</details>

## WeakRegistry

I am a registry for objects needing finalization. When an object is added the object as well as its executor is stored. When the object is garbage collected, the executor can take the appropriate action for any resources associated with the object. See also: Object executor Object actAsExecutor Object finalize

### Methods
#### WeakRegistry>>#add: anObject

Add anObject to the receiver. Store the object as well as the associated executor.


<details>
	<summary>See more</summary>
	
	add: anObject
	"Add anObject to the receiver. Store the object as well as the associated executor."
	| executor |
	executor := anObject executor.
	self protected:[
		valueDictionary at: anObject put: executor.
	].
	^anObject
</details>

#### WeakRegistry>>#keys

<details>
	<summary>See more</summary>
	
	keys

	^self protected: [ valueDictionary keys ]

</details>

#### WeakRegistry>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species
	^Set
</details>

#### WeakRegistry>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size
	^self protected:[
		valueDictionary size
	].
</details>

#### WeakRegistry>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock
	^self protected:[
		valueDictionary keysDo: aBlock.
	].

</details>

#### WeakRegistry>>#protected: aBlock

Execute aBlock protected by the accessLock


<details>
	<summary>See more</summary>
	
	protected: aBlock
	"Execute aBlock protected by the accessLock"
	^accessLock
		ifNil: [ aBlock value]
		ifNotNil: [ accessLock critical: aBlock ifError:[:msg :rcvr| rcvr error: msg]]
</details>

#### WeakRegistry>>#initialize: n

<details>
	<summary>See more</summary>
	
	initialize: n
	valueDictionary _ WeakIdentityKeyDictionary new: n.
	accessLock _ Semaphore forMutualExclusion
</details>

#### WeakRegistry>>#removeAll

<details>
	<summary>See more</summary>
	
	removeAll

	self protected: [
		valueDictionary removeAll ]
</details>

#### WeakRegistry>>#finalizeValues

Some of our elements may have gone away. Look for those and activate the associated executors.


<details>
	<summary>See more</summary>
	
	finalizeValues
	"Some of our elements may have gone away. Look for those and activate the associated executors."
	| finiObjects |
	finiObjects _ nil.
	"First collect the objects."
	self protected:[
		valueDictionary associationsDo:[:assoc|
			assoc key ifNil: [
				finiObjects 
					ifNil: [ finiObjects := OrderedCollection with: assoc value]
					ifNotNil: [ finiObjects add: assoc value]]
		].
		finiObjects ifNotNil: [ valueDictionary finalizeValues: finiObjects asArray].
	].
	"Then do the finalization"
	finiObjects ifNotNil: [
		finiObjects do:[:each| each finalize]]
</details>

#### WeakRegistry>>#remove: oldObject ifAbsent: exceptionBlock

Remove oldObject as one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: exceptionBlock
	"Remove oldObject as one of the receiver's elements."
	| removedObject |
	oldObject ifNil: [ ^oldObject].
	self protected: [
		removedObject := valueDictionary removeKey: oldObject ifAbsent: nil.
	].
	^removedObject
		ifNil: [ exceptionBlock value]
		ifNotNil: [ removedObject]
</details>

#### WeakRegistry>>#printElementsOn: aStream

<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	accessLock ifNil: [^super printElementsOn: aStream].
	aStream nextPutAll: '(<this WeakRegistry is locked>)'
</details>

#### WeakRegistry>>#add: anObject executor: anExecutor

Add anObject to the receiver. Store the object as well as the associated executor.


<details>
	<summary>See more</summary>
	
	add: anObject executor: anExecutor
	"Add anObject to the receiver. Store the object as well as the associated executor."
	self protected:[
		valueDictionary at: anObject put: anExecutor.
	].
	^anObject
</details>

## WeakSet

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### WeakSet>>#grow

Grow the elements array and reinsert the old elements


<details>
	<summary>See more</summary>
	
	grow
	"Grow the elements array and reinsert the old elements"

	self growTo: array size + self growSize
</details>

#### WeakSet>>#growTo: anInteger

Grow the elements array and reinsert the old elements


<details>
	<summary>See more</summary>
	
	growTo: anInteger
	"Grow the elements array and reinsert the old elements"

	| oldElements |

	oldElements _ array.
	array _ WeakArray new: anInteger.
	array atAllPut: flag.
	tally _ 0.
	oldElements do:
		[:each | (each == flag or: [each == nil]) ifFalse: [self noCheckAdd: each]]
</details>

#### WeakSet>>#add: newObject

Include newObject as one of the receiver's elements, but only if not already present. Answer newObject


<details>
	<summary>See more</summary>
	
	add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject"

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: newObject.
	((array at: index) == flag or: [(array at: index) isNil])
		ifTrue: [self atNewIndex: index put: newObject].
	^newObject
</details>

#### WeakSet>>#slowSize

Careful! Answer the maximum amount of elements in the receiver, not the exact amount


<details>
	<summary>See more</summary>
	
	slowSize
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				count := count + 1 ] ] ].
	^count
</details>

#### WeakSet>>#like: anObject

Answer an object in the receiver that is equal to anObject, nil if no such object is found. Relies heavily on hash properties


<details>
	<summary>See more</summary>
	
	like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index element |

	^(index _ self scanFor: anObject) = 0
		ifFalse: [(element _ array at: index) == flag ifFalse: [element]]
</details>

#### WeakSet>>#do: aBlock after: anElement

<details>
	<summary>See more</summary>
	
	do: aBlock after: anElement
	| each startIndex |

	tally = 0 ifTrue: [^self].
	startIndex _ anElement ifNil: [1] ifNotNil:
		[self findElementOrNil: anElement].
	startIndex + 1 to: array size do:
		[:index |
			((each _ array at: index) == nil or: [each == flag])
				ifFalse: [aBlock value: each]
		]
</details>

#### WeakSet>>#collect: aBlock

Evaluate aBlock with each of the receiver's elements as the argument. Collect the resulting values into a collection like the receiver. Answer the new collection.


<details>
	<summary>See more</summary>
	
	collect: aBlock
	| each newSet |
	newSet _ self species new: self size.
	tally = 0 ifTrue: [^newSet ].
	1 to: array size do:
		[:index |
			((each _ array at: index) == nil or: [each == flag])
				ifFalse: [newSet add: (aBlock value: each)]
		].
	^newSet
</details>

#### WeakSet>>#remove: oldObject ifAbsent: aBlock

Remove oldObject from the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock. Otherwise, answer the argument, oldObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: oldObject ifAbsent: aBlock

	| index |
	index _ self findElementOrNil: oldObject.
	(array at: index) == flag ifTrue: [ ^ aBlock value ].
	array at: index put: flag.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^oldObject
</details>

#### WeakSet>>#printElementsOn: aStream

<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	| oldPos |
	aStream nextPut: $(.
	oldPos _ aStream position.
	self do: [:element | aStream print: element; space].
	aStream position > oldPos ifTrue: [aStream skip: -1 "remove the extra space"].
	aStream nextPut: $)
</details>

#### WeakSet>>#scanFor: anObject

Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements


<details>
	<summary>See more</summary>
	
	scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| element start finish |

	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == flag or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == flag or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
</details>

#### WeakSet>>#size

Careful! Answer the maximum amount of elements in the receiver, not the exact amount


<details>
	<summary>See more</summary>
	
	size
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	^tally
</details>

#### WeakSet>>#init: n

Initialize array to an array size of n


<details>
	<summary>See more</summary>
	
	init: n
	"Initialize array to an array size of n"

	flag _ Object new.
	array _ WeakArray new: n.
	array atAllPut: flag.
	tally _ 0
</details>

#### WeakSet>>#includes: anObject

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: anObject 
	^(array at: (self findElementOrNil: anObject)) ~~ flag
</details>

#### WeakSet>>#do: aBlock

Evaluate aBlock with each of the receiver's elements as the argument.


<details>
	<summary>See more</summary>
	
	do: aBlock
	| each |

	tally = 0 ifTrue: [^self].
	1 to: array size do:
		[:index |
			((each _ array at: index) == nil or: [each == flag])
				ifFalse: [aBlock value: each]
		]
</details>

#### WeakSet>>#fixCollisionsFrom: index

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
	(element _ self keyAt: oldIndex) == flag]
		whileFalse: 
			[newIndex _ self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]
</details>

#### WeakSet>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	^ WeakSetInspector
</details>

#### WeakSet>>#rehash

Do nothing. Here so sending this to a Set does not have to do a time consuming respondsTo:


<details>
	<summary>See more</summary>
	
	rehash
	self growTo: array size
</details>

## WeakValueDictionary

I am a dictionary holding only weakly on my values. Clients may expect to get a nil value for any object they request.

### Methods
#### WeakValueDictionary>>#add: anAssociation

Quite like doing self at: anAssociation key put: anAssociation value but making sure the argument is stored. This method should be used when the argument is stored elsewhere, and its value should be kept in sync with the value stored in me. If the key already exists, and it is desired to keep the existing association, then call #at:put:


<details>
	<summary>See more</summary>
	
	add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
</details>

#### WeakValueDictionary>>#at: key ifAbsentOrNilPut: aBlock

Return the value at the given key. If key is not included in the receiver store the result of evaluating aBlock as new value.


<details>
	<summary>See more</summary>
	
	at: key ifAbsentOrNilPut: aBlock 
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."
	^self at: key ifAbsentOrNil: [self at: key put: aBlock value]
</details>

#### WeakValueDictionary>>#at: key put: anObject

Set the value at key to be anObject. If key is not found, create a new entry for key and set is value to anObject. Answer anObject.


<details>
	<summary>See more</summary>
	
	at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	index _ self findElementOrNil: key.
	element _ array at: index.
	element
		ifNil: [self atNewIndex: index put: (WeakValueAssociation key: key value: anObject)]
		ifNotNil: [element value: anObject].
	^ anObject
</details>

#### WeakValueDictionary>>#at: key ifAbsentOrNil: aBlock

In a WeakValueDictionary, normally nil values (i.e. collected values) are considered the same as if the key/value was never added. Hence, this convenience method.


<details>
	<summary>See more</summary>
	
	at: key ifAbsentOrNil: aBlock
	"In a WeakValueDictionary, normally nil values (i.e. collected values) are considered the same as if the key/value was never added. Hence, this convenience method."
	
	| assoc |
	assoc _ array at: (self findElementOrNil: key).
	assoc ifNil: [ ^ aBlock value ].
	^ assoc value ifNil: [ aBlock value ]
</details>

