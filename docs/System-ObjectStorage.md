## DataStream

This is the save-to-disk facility. A DataStream can store one or more objects in a persistent form. To handle objects with sharing and cycles, you must use a ReferenceStream instead of a DataStream. (Or SmartRefStream.) ReferenceStream is typically faster and produces smaller files because it doesn't repeatedly write the same Symbols. Here is the way to use DataStream and ReferenceStream: rr _ ReferenceStream fileNamed: 'test.obj'. rr nextPut: <your object>. rr close. To get it back: rr _ ReferenceStream fileNamed: 'test.obj'. <your object> _ rr next. rr close. Each object to be stored has two opportunities to control what gets stored. On the high level, objectToStoreOnDataStream allows you to substitute another object on the way out. The low level hook is storeDataOn:. The read-in counterparts to these messages are comeFullyUpOnReload and (class) readDataFrom:size:. See these methods, and the class DiskProxy, for more information about externalizing and internalizing. NOTE: A DataStream should be treated as a write-stream for writing. It is a read-stream for reading. It is not a ReadWriteStream.

### Methods
#### DataStream>>#beginReference: anObject

We’re starting to read anObject. Remember it and its reference position (if we care; ReferenceStream cares). Answer the reference position.


<details>
	<summary>See more</summary>
	
	beginReference: anObject
    "We’re starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0
</details>

#### DataStream>>#writeArray: anArray

PRIVATE -- Write the contents of an Array.


<details>
	<summary>See more</summary>
	
	writeArray: anArray
	"PRIVATE -- Write the contents of an Array."

	byteStream nextUnsignedInt32Put: anArray size bigEndian: true.
	self nextPutAll: anArray.
</details>

#### DataStream>>#readNil

PRIVATE -- Read the contents of an UndefinedObject.


<details>
	<summary>See more</summary>
	
	readNil
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil
</details>

#### DataStream>>#atEnd

Answer true if the stream is at the end.


<details>
	<summary>See more</summary>
	
	atEnd
    "Answer true if the stream is at the end."

    ^ byteStream atEnd
</details>

#### DataStream>>#writeUser: anObject

Write the contents of an arbitrary User instance (and its devoted class).


<details>
	<summary>See more</summary>
	
	writeUser: anObject
    "Write the contents of an arbitrary User instance (and its devoted class)."
    " 7/29/96 tk"

	"If anObject is an instance of a unique user class, will lie and say it has a generic class"
    ^ anObject storeDataOn: self
</details>

#### DataStream>>#readReference

Read the contents of an object reference. (Cf. outputReference:) File is not now positioned at this object.


<details>
	<summary>See more</summary>
	
	readReference
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |

	^ (referencePosition _ (byteStream nextUnsignedInt32BigEndian: true)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"
</details>

#### DataStream>>#readShortRef

Read an object reference from two bytes only. Original object must be in first 65536 bytes of the file. Relative to start of data. vacantRef not a possibility.


<details>
	<summary>See more</summary>
	
	readShortRef
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextUnsignedInt16BigEndian: true)
</details>

#### DataStream>>#objectIfBlocked: anObject

We don't do any blocking


<details>
	<summary>See more</summary>
	
	objectIfBlocked: anObject
	"We don't do any blocking"

	^ anObject
</details>

#### DataStream>>#vacantRef

Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference position' to identify a reference that's not yet filled in. This must be a value that won't be used as an ordinary reference. Cf. outputReference: and readReference. -- NOTE: We could use a different type ID for vacant-refs rather than writing object-references with a magic value. (The type ID and value are overwritten by ordinary object-references when weak refs are fullfilled.)


<details>
	<summary>See more</summary>
	
	vacantRef
	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference
	 position' to identify a reference that's not yet filled in. This must be a
	 value that won't be used as an ordinary reference. Cf. outputReference: and
	 readReference. -- 
	 NOTE: We could use a different type ID for vacant-refs rather than writing
		object-references with a magic value. (The type ID and value are
		overwritten by ordinary object-references when weak refs are fullfilled.)"

	"In 32 bit Cuis it was:"
	"^ SmallInteger maxVal"

	"Use that very same value even if in 64 bit Cuis.
	This means that DataStreams are limited to 1GibiBytes in size."
	^16r3FFFFFFF
</details>

#### DataStream>>#writeFloat: aFloat

PRIVATE -- Write the contents of a Float. We support 8-byte Floats here. Write in bigEndian / PowerPC order.


<details>
	<summary>See more</summary>
	
	writeFloat: aFloat
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here.
	Write in bigEndian / PowerPC order."

	byteStream nextDouble64Put: aFloat bigEndian: true
</details>

#### DataStream>>#size

Answer the stream's size.


<details>
	<summary>See more</summary>
	
	size
    "Answer the stream's size."

    ^ byteStream size
</details>

#### DataStream>>#setStream: aStream

PRIVATE -- Initialization method.


<details>
	<summary>See more</summary>
	
	setStream: aStream
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos _ aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream _ aStream.
</details>

#### DataStream>>#tryToPutReference: anObject typeID: typeID

PRIVATE -- If we support references for type typeID, and if anObject already appears in my output stream, then put a reference to the place where anObject already appears. If we support references for typeID but didn’t already put anObject, then associate the current stream position with anObject in case one wants to nextPut: it again. Return true after putting a reference; false if the object still needs to be put. For DataStream this is trivial. ReferenceStream overrides this.


<details>
	<summary>See more</summary>
	
	tryToPutReference: anObject typeID: typeID
    "PRIVATE -- If we support references for type typeID, and if
       anObject already appears in my output stream, then put a
       reference to the place where anObject already appears. If we
       support references for typeID but didn’t already put anObject,
       then associate the current stream position with anObject in
       case one wants to nextPut: it again.
     Return true after putting a reference; false if the object still
       needs to be put.
     For DataStream this is trivial. ReferenceStream overrides this."

    ^ false
</details>

#### DataStream>>#readFloat

PRIVATE -- Read the contents of a Float. This is the fast way to read a Float. Read bigEndian / PowerPC order.


<details>
	<summary>See more</summary>
	
	readFloat
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	Read bigEndian / PowerPC order."

	^ byteStream nextDouble64BigEndian: true
</details>

#### DataStream>>#rootObject

Return the object at the root of the tree we are filing out.


<details>
	<summary>See more</summary>
	
	rootObject
	"Return the object at the root of the tree we are filing out.  "

	^ topCall
</details>

#### DataStream>>#noteCurrentReference: typeID

PRIVATE -- If we support references for type typeID, remember the current byteStream position so we can add the next object to the ‘objects’ dictionary, and return true. Else return false. This method is here to be overridden by ReferenceStream


<details>
	<summary>See more</summary>
	
	noteCurrentReference: typeID
    "PRIVATE -- If we support references for type typeID, remember
     the current byteStream position so we can add the next object to
     the ‘objects’ dictionary, and return true. Else return false.
     This method is here to be overridden by ReferenceStream"

    ^ false
</details>

#### DataStream>>#objectAt: anInteger

PRIVATE -- Read & return the object at a given stream position. 08:18 tk anInteger is a relative file position.


<details>
	<summary>See more</summary>
	
	objectAt: anInteger
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn _ byteStream position.		"absolute"
	refPosn _ self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject _ self next.

	self setCurrentReference: refPosn.		"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject
</details>

#### DataStream>>#next

Answer the next object in the stream.


<details>
	<summary>See more</summary>
	
	next
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type _ byteStream next.
	type ifNil: [pos _ byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		pos.	"so can see it in debugger"
		^ nil].
	type = 0 ifTrue: [pos _ byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType _ self noteCurrentReference: type.
	selector _ #(readNil readTrue readFalse readInteger	"<-4"
			readStringOld readSymbol readByteArray		"<-7"
			readArray readInstance readReference readBitmap	"<-11"
			readClass readUser readFloat errorRead readShortInst 	"<-16"
			readString readWordArray readWordArrayForSegment 	"<-19"
			readWordLike readMethod 
			readCharacter "<-22") at: type.
	selector = 0 ifTrue: [pos _ byteStream position.	"absolute!!"
			byteStream close. 
			self error: 'file is more recent than this system'. ^ nil].
	anObject _ self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."
	isARefType ifTrue: [self beginReference: anObject].

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject _ anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject
</details>

#### DataStream>>#replace: original with: proxy

We may wish to remember that in some field, the original object is being replaced by the proxy. For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced.


<details>
	<summary>See more</summary>
	
	replace: original with: proxy
	"We may wish to remember that in some field, the original object is being replaced by the proxy.  For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced."

	"do nothing"
</details>

#### DataStream>>#writeByteArray: aByteArray

PRIVATE -- Write the contents of a ByteArray.


<details>
	<summary>See more</summary>
	
	writeByteArray: aByteArray
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextUnsignedInt32Put: aByteArray size bigEndian: true.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.
</details>

#### DataStream>>#writeTrue: aTrue

PRIVATE -- Write the contents of a True.


<details>
	<summary>See more</summary>
	
	writeTrue: aTrue
    "PRIVATE -- Write the contents of a True."
</details>

#### DataStream>>#readWordArrayForSegment

Read the contents of a WordArray ignoring endianness.


<details>
	<summary>See more</summary>
	
	readWordArrayForSegment
	"Read the contents of a WordArray ignoring endianness."
	"Removed WordArrayForSegment when removed ImageSegment"
"	^ WordArrayForSegment newFromStream: byteStream"
	"Size is number of long words."
</details>

#### DataStream>>#readShortInst

Read the contents of an arbitrary instance that has a short header. ASSUMES: readDataFrom:size: sends me beginReference: after it instantiates the new object but before reading nested objects. NOTE: We must restore the current reference position after recursive calls to next. Let the instance, not the class read the data.


<details>
	<summary>See more</summary>
	
	readShortInst
	"Read the contents of an arbitrary instance that has a short header.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize _ (byteStream next) - 1.	"one byte of size"
	refPosn _ self getCurrentReference.
	aSymbol _ self readShortRef.	"class symbol in two bytes of file pos"
	newClass _ Smalltalk at: aSymbol asSymbol.
	anObject _ newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject _ anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
</details>

#### DataStream>>#readString

<details>
	<summary>See more</summary>
	
	readString

	| str |
	byteStream ascii.
	str _ byteStream nextString.
	byteStream binary.
	^ str

</details>

#### DataStream>>#readUser

Reconstruct both the private class and the instance. Still used??


<details>
	<summary>See more</summary>
	
	readUser
	"Reconstruct both the private class and the instance.  Still used??"

	^ self readInstance.		"Will create new unique class"

</details>

#### DataStream>>#rootObject: anObject

Return the object at the root of the tree we are filing out.


<details>
	<summary>See more</summary>
	
	rootObject: anObject
	"Return the object at the root of the tree we are filing out.  "

	topCall _ anObject
</details>

#### DataStream>>#writeString: aString

PRIVATE -- Write the contents of a String.


<details>
	<summary>See more</summary>
	
	writeString: aString
	"PRIVATE -- Write the contents of a String."

	byteStream nextStringPut: aString.
</details>

#### DataStream>>#setCurrentReference: refPosn

PRIVATE -- Set currentReference to refPosn. Noop here. Cf. ReferenceStream.


<details>
	<summary>See more</summary>
	
	setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."
</details>

#### DataStream>>#writeBitmap: aBitmap

PRIVATE -- Write the contents of a Bitmap.


<details>
	<summary>See more</summary>
	
	writeBitmap: aBitmap
	"PRIVATE -- Write the contents of a Bitmap."

	aBitmap writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words."
</details>

#### DataStream>>#flush

Guarantee that any writes to me are actually recorded on disk. -- 11/17/92 jhm


<details>
	<summary>See more</summary>
	
	flush
    "Guarantee that any writes to me are actually recorded on disk. -- 11/17/92 jhm"

    ^ byteStream flush
</details>

#### DataStream>>#errorRead

PRIVATE -- Raise an error because this case of next's perform: shouldn't be called.


<details>
	<summary>See more</summary>
	
	errorRead
    "PRIVATE -- Raise an error because this case of next's perform:
     shouldn't be called."

    self error: 'This should never be called'
</details>

#### DataStream>>#readCharacter

PRIVATE -- Read a Character.


<details>
	<summary>See more</summary>
	
	readCharacter
    "PRIVATE -- Read a Character."

    ^ Character numericValue: (byteStream nextUnsignedInt32BigEndian: true)
</details>

#### DataStream>>#getCurrentReference

PRIVATE -- Return the currentReference posn. Overridden by ReferenceStream.


<details>
	<summary>See more</summary>
	
	getCurrentReference
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0
</details>

#### DataStream>>#readInteger

PRIVATE -- Read the contents of a SmallInteger.


<details>
	<summary>See more</summary>
	
	readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextSignedInt32BigEndian: true	"signed!!!"
</details>

#### DataStream>>#writeFalse: aFalse

PRIVATE -- Write the contents of a False.


<details>
	<summary>See more</summary>
	
	writeFalse: aFalse
    "PRIVATE -- Write the contents of a False."
</details>

#### DataStream>>#beginInstance: aClass size: anInteger

This is for use by storeDataOn: methods. Cf. Object>>storeDataOn:.


<details>
	<summary>See more</summary>
	
	beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextUnsignedInt32Put: anInteger + 1 bigEndian: true.

	self nextPut: aClass name
</details>

#### DataStream>>#setStream: aStream reading: isReading

PRIVATE -- Initialization method.


<details>
	<summary>See more</summary>
	
	setStream: aStream reading: isReading
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos _ aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream _ aStream.
</details>

#### DataStream>>#readByteArray

PRIVATE -- Read the contents of a ByteArray.


<details>
	<summary>See more</summary>
	
	readByteArray
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count _ byteStream nextUnsignedInt32BigEndian: true.
	^ byteStream next: count  "assume stream is in binary mode"

</details>

#### DataStream>>#readMethod

PRIVATE -- Read the contents of an arbitrary instance. ASSUMES: readDataFrom:size: sends me beginReference: after it instantiates the new object but before reading nested objects. NOTE: We must restore the current reference position after recursive calls to next. Let the instance, not the class read the data.


<details>
	<summary>See more</summary>
	
	readMethod
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize refPosn newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	className _ self next.
	newClass _ Smalltalk at: className asSymbol.

	xxHeader _ self next.
		"nArgs _ (xxHeader >> 24) bitAnd: 16rF."
		"nTemps _ (xxHeader >> 18) bitAnd: 16r3F."
		"largeBit _ (xxHeader >> 17) bitAnd: 1."
	nLits _ (xxHeader >> 9) bitAnd: 16rFF.
		"primBits _ ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer _ instSize - (newClass instSize "0") - (nLits + 1 * Smalltalk wordSize).

	newMethod _ newClass 
		newMethod: byteCodeSizePlusTrailer
		header: xxHeader.

	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	self beginReference: newMethod.
	lits _ newMethod numLiterals + 1.	"counting header"
	2 to: lits do:
		[:ii | newMethod objectAt: ii put: self next].
	lits*Smalltalk wordSize+1 to: newMethod basicSize do:
		[:ii | newMethod basicAt: ii put: byteStream next].
			"Get raw bytes directly from the file"
	self setCurrentReference: refPosn.  "before returning to next"
	^ newMethod
</details>

#### DataStream>>#readSymbol

PRIVATE -- Read the contents of a Symbol.


<details>
	<summary>See more</summary>
	
	readSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readString asSymbol
</details>

#### DataStream>>#readArray

PRIVATE -- Read the contents of an Array. We must do beginReference: here after instantiating the Array but before reading its contents, in case the contents reference the Array. beginReference: will be sent again when we return to next, but that's ok as long as we save and restore the current reference position over recursive calls to next.


<details>
	<summary>See more</summary>
	
	readArray
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count _ byteStream nextUnsignedInt32BigEndian: true.

	refPosn _ self beginReference: (array _ Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array
</details>

#### DataStream>>#readFalse

PRIVATE -- Read the contents of a False.


<details>
	<summary>See more</summary>
	
	readFalse
    "PRIVATE -- Read the contents of a False."

    ^ false
</details>

#### DataStream>>#writeClass: aClass

Write out a DiskProxy for the class. It will look up the class's name in Smalltalk in the new sustem. Never write classes or methodDictionaries as objects. For novel classes, front part of file is a fileIn of the new class.


<details>
	<summary>See more</summary>
	
	writeClass: aClass
	"Write out a DiskProxy for the class.  It will look up the class's name in Smalltalk in the new sustem.  Never write classes or methodDictionaries as objects.  For novel classes, front part of file is a fileIn of the new class."

	"This method never executed because objectToStoreOnDataStream returns a DiskProxy.  See DataStream.nextPut:"
    ^ self error: 'Write a DiskProxy instead'
</details>

#### DataStream>>#writeInteger: anInteger

PRIVATE -- Write the contents of a SmallInteger.


<details>
	<summary>See more</summary>
	
	writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextSignedInt32Put: anInteger bigEndian: true	"signed!!!!!"
</details>

#### DataStream>>#next: anInteger

Answer an Array of the next anInteger objects in the stream.


<details>
	<summary>See more</summary>
	
	next: anInteger
    "Answer an Array of the next anInteger objects in the stream."
    | array |

    array _ Array new: anInteger.
    1 to: anInteger do: [:i |
        array at: i put: self next].
    ^ array
</details>

#### DataStream>>#readClass

Should never be executed because a DiskProxy, not a clas comes in.


<details>
	<summary>See more</summary>
	
	readClass
	"Should never be executed because a DiskProxy, not a clas comes in."

	^ self error: 'Classes should be filed in'
</details>

#### DataStream>>#readInstance

PRIVATE -- Read the contents of an arbitrary instance. ASSUMES: readDataFrom:size: sends me beginReference: after it instantiates the new object but before reading nested objects. NOTE: We must restore the current reference position after recursive calls to next. Let the instance, not the class read the data.


<details>
	<summary>See more</summary>
	
	readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	aSymbol _ self next.
	newClass _ Smalltalk at: aSymbol asSymbol.
	anObject _ newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject _ anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
</details>

#### DataStream>>#writeNil: anUndefinedObject

PRIVATE -- Write the contents of an UndefinedObject.


<details>
	<summary>See more</summary>
	
	writeNil: anUndefinedObject
    "PRIVATE -- Write the contents of an UndefinedObject."
</details>

#### DataStream>>#writeCharacter: aCharacter

PRIVATE -- Write a Character.


<details>
	<summary>See more</summary>
	
	writeCharacter: aCharacter
	"PRIVATE -- Write a Character."

	byteStream nextUnsignedInt32Put: aCharacter numericValue bigEndian: true
</details>

#### DataStream>>#writeSymbol: aSymbol

PRIVATE -- Write the contents of a Symbol.


<details>
	<summary>See more</summary>
	
	writeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol
</details>

#### DataStream>>#nextPut: anObject

Write anObject to the receiver stream. Answer anObject.


<details>
	<summary>See more</summary>
	
	nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID _ self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore _ (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID _ self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector _ #(writeNil: writeTrue: writeFalse: writeInteger: 
		writeStringOld: writeSymbol: writeByteArray:
		writeArray: writeInstance: errorWriteReference: writeBitmap:
		writeClass: writeUser: writeFloat: errorWriteReference: == "<-16 short inst" 
		writeString: writeBitmap: writeBitmap: writeWordLike: 
		writeInstance: "CompiledMethod"
		writeCharacter: ) at: typeID.
	self perform: selector with: objectToStore.

	^ anObject


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod and ViewState), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of ViewStates, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."
</details>

#### DataStream>>#readBitmap

PRIVATE -- Read the contents of a Bitmap.


<details>
	<summary>See more</summary>
	
	readBitmap
	"PRIVATE -- Read the contents of a Bitmap."

	^ Bitmap newFromStream: byteStream
	"Note that the reader knows that the size is in long words, but the data is in bytes."
</details>

#### DataStream>>#writeWordLike: aWordArray

Note that we put the class name before the size.


<details>
	<summary>See more</summary>
	
	writeWordLike: aWordArray
	"Note that we put the class name before the size."

	self nextPut: aWordArray class name.
	aWordArray writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words or double-bytes."
</details>

#### DataStream>>#outputReference: referencePosn

PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn.


<details>
	<summary>See more</summary>
	
	outputReference: referencePosn
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextUnsignedInt32Put: referencePosn bigEndian: true	"relative position"
</details>

#### DataStream>>#writeInstance: anObject

PRIVATE -- Write the contents of an arbitrary instance.


<details>
	<summary>See more</summary>
	
	writeInstance: anObject
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self
</details>

#### DataStream>>#readWordLike

Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc).


<details>
	<summary>See more</summary>
	
	readWordLike
	| refPosn aSymbol newClass anObject |
	"Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc)."

	refPosn _ self getCurrentReference.
	aSymbol _ self next.
	newClass _ Smalltalk at: aSymbol asSymbol.
	anObject _ newClass newFromStream: byteStream.
	"Size is number of long words."
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject

</details>

#### DataStream>>#readTrue

PRIVATE -- Read the contents of a True.


<details>
	<summary>See more</summary>
	
	readTrue
    "PRIVATE -- Read the contents of a True."

    ^ true
</details>

#### DataStream>>#readWordArray

PRIVATE -- Read the contents of a WordArray.


<details>
	<summary>See more</summary>
	
	readWordArray
	"PRIVATE -- Read the contents of a WordArray."

	^ WordArray newFromStream: byteStream
	"Size is number of long words."
</details>

#### DataStream>>#byteStream

<details>
	<summary>See more</summary>
	
	byteStream
	^ byteStream
</details>

#### DataStream>>#maybeBeginReference: internalObject

Do nothing. See ReferenceStream|maybeBeginReference:


<details>
	<summary>See more</summary>
	
	maybeBeginReference: internalObject
	"Do nothing.  See ReferenceStream|maybeBeginReference:"

	^ internalObject
</details>

#### DataStream>>#typeIDFor: anObject

Return the typeID for anObject's class. This is where the tangle of objects is clipped to stop everything from going out. Classes can control their instance variables by defining objectToStoreOnDataStream. Any object in blockers is not written out. See ReferenceStream.objectIfBlocked: and DataStream nextPut:. Morphs do not write their owners. See Morph.storeDataOn: Each morph tells itself to 'prepareToBeSaved' before writing out.


<details>
	<summary>See more</summary>
	
	typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ DataStream typeMap at: anObject class ifAbsent: [9 "instance of any normal class"]	
"See DataStream initialize.  nil=1. true=2. false=3. a SmallInteger=4. (a String was 5). a Symbol=6.  a ByteArray=7. an Array=8. other = 9.  a Bitmap=11. a Metaclass=12. a Float=14.  a Rectangle=15. any instance that can have a short header=16.  a String=17 (new format). a WordArray=18."
</details>

#### DataStream>>#writeStringOld: aString

PRIVATE -- Write the contents of a String.


<details>
	<summary>See more</summary>
	
	writeStringOld: aString
	"PRIVATE -- Write the contents of a String."

	| length |
	aString size < 16384 
		ifTrue: [
			(length _ aString size) < 192
				ifTrue: [byteStream nextPut: length]
				ifFalse: 
					[byteStream nextPut: (length // 256 + 192).
					byteStream nextPut: (length \\ 256)].
			aString do: [:char | byteStream nextPut: char numericValue]]
		ifFalse: [self writeByteArray: aString].	"takes more space"
</details>

#### DataStream>>#reset

Reset the stream.


<details>
	<summary>See more</summary>
	
	reset
    "Reset the stream."

    byteStream reset
</details>

#### DataStream>>#errorWriteReference: anInteger

PRIVATE -- Raise an error because this case of nextPut:'s perform: shouldn't be called. -- 11/15/92 jhm


<details>
	<summary>See more</summary>
	
	errorWriteReference: anInteger
    "PRIVATE -- Raise an error because this case of nextPut:'s perform:
     shouldn't be called. -- 11/15/92 jhm"

    self error: 'This should never be called'
</details>

#### DataStream>>#contents

Answer all of the contents of the receiver.


<details>
	<summary>See more</summary>
	
	contents
	^byteStream contents
</details>

## DiskProxy

A DiskProxy is an externalized form of an object to write on a DataStream. It contains a "constructor" message to regenerate the object, in context, when sent a comeFullyUpOnReload: message (i.e. "internalize"). We are now using DiskProxy for shared system objects like StrikeFonts. The idea is to define, for each kind of object that needs special externalization, a class method that will internalize the object by reconstructing it from its defining state. We call this a "constructor" method. Then externalize such an object as a frozen message that invokes this method--a DiskProxy. (Here is the old comment: Constructing a new object is good for any object that (1) can not be externalized simply by snapshotting and reloading its instance variables (like a CompiledMethod or a Picture), or (2) wants to be free to evolve its internal representation without making stored instances obsolete (and dangerous). Snapshotting and reloading an object"s instance variables is a dangerous breach of encapsulation. The internal structure of the class is then free to evolve. All externalized instances will be useful as long as the constructor methods are maintained with the same semantics. There may be several constructor methods for a particular class. This is useful for (1) instances with characteristically different defining state, and (2) newer, evolved forms of an object and its constructors, with the old constructor methods kept around so old data can still be properly loaded.) Create one like this example from class Picture DiskProxy global: #Picture selector: #fromByteArray: args: (Array with: self storage asByteArray) * See also subclass DiskProxyQ that will construct an object in the above manner and then send it a sequence of messages. This may save creating a wide variety of constructor methods. It is also useful because the newly read-in DiskProxyQ can catch messages like #objectContainedIn: (via #doesNotUnderstand:) and add them to the queue of messages to send to the new object. * We may also want a subclass of DiskProxy that evaluates a string expression to compute the receiver of the constructor message. My instance variables: * globalObjectName -- the Symbol name of a global object in the System dictionary (usually a class). * constructorSelector -- the constructor message selector Symbol to send to the global object (perform:withArguments:), typically a variation on newFrom:. * constructorArgs -- the Array of arguments to pass in the constructor message. -- 11/9/92 Jerry Morrison

### Methods
#### DiskProxy>>#global: globalNameSymbol preSelector: aSelector selector: selectorSymbol args: argArray

Initialize self as a DiskProxy constructor with the given globalNameSymbol, selectorSymbol, and argument Array. I will internalize by looking up the global object name in the SystemDictionary (Smalltalk) and sending it this message with these arguments.


<details>
	<summary>See more</summary>
	
	global: globalNameSymbol preSelector: aSelector selector: selectorSymbol args: argArray
	"Initialize self as a DiskProxy constructor with the given
	globalNameSymbol, selectorSymbol, and argument Array.
	I will internalize by looking up the global object name in the
	SystemDictionary (Smalltalk) and sending it this message with
	these arguments."

	globalObjectName _ globalNameSymbol asSymbol.
	preSelector _ aSelector asSymbol.
	constructorSelector _ selectorSymbol asSymbol.
	constructorArgs _ argArray.
</details>

#### DiskProxy>>#global: globalNameSymbol selector: selectorSymbol args: argArray

Initialize self as a DiskProxy constructor with the given globalNameSymbol, selectorSymbol, and argument Array. I will internalize by looking up the global object name in the SystemDictionary (Smalltalk) and sending it this message with these arguments.


<details>
	<summary>See more</summary>
	
	global: globalNameSymbol selector: selectorSymbol args: argArray
	"Initialize self as a DiskProxy constructor with the given
	globalNameSymbol, selectorSymbol, and argument Array.
	I will internalize by looking up the global object name in the
	SystemDictionary (Smalltalk) and sending it this message with
	these arguments."

	(globalNameSymbol beginsWith: 'AnObsolete') ifTrue: [
		self error: 'Trying to write out, ', globalNameSymbol].
	globalObjectName _ globalNameSymbol asSymbol.
	constructorSelector _ selectorSymbol asSymbol.
	constructorArgs _ argArray.
</details>

#### DiskProxy>>#constructorArgs

<details>
	<summary>See more</summary>
	
	constructorArgs
	^ constructorArgs
</details>

#### DiskProxy>>#comeFullyUpOnReload: smartRefStream

Internalize myself into a fully alive object after raw loading from a DataStream. (See my class comment.) DataStream will substitute the object from this eval for the DiskProxy.


<details>
	<summary>See more</summary>
	
	comeFullyUpOnReload: smartRefStream
	"Internalize myself into a fully alive object after raw loading from a DataStream. (See my class comment.)  DataStream will substitute the object from this eval for the DiskProxy."
	| globalObj symbol |

	symbol _ globalObjectName.
	"See if class is mapped to another name"
	(smartRefStream respondsTo: #renamed) ifTrue: [
		symbol _ smartRefStream renamed at: symbol ifAbsent: [symbol]].	"map"
	globalObj _ Smalltalk at: symbol ifAbsent: [
		preSelector == nil & (constructorSelector = #yourself) ifTrue: [
			Transcript newLine; show: symbol, ' is undeclared.'.
			(Undeclared includesKey: symbol) ifTrue: [^ Undeclared at: symbol].
			Undeclared at: symbol put: nil.
			^ nil].
		^ self error: 'Global "', symbol, '" not found'].

	preSelector ifNotNil: [
		Symbol hasInterned: preSelector ifTrue: [:selector |
			globalObj _ globalObj perform: selector]].

	constructorSelector ifNil: [^ globalObj].
	Symbol hasInterned: constructorSelector ifTrue: [:selector |
		^ globalObj perform: selector
				withArguments: constructorArgs].
				"args not checked against Renamed"
	^ nil 	"was not in proper form"
</details>

#### DiskProxy>>#preSelector: aSelector

<details>
	<summary>See more</summary>
	
	preSelector: aSelector

	preSelector _ aSelector
</details>

#### DiskProxy>>#constructorSelector

<details>
	<summary>See more</summary>
	
	constructorSelector
	^ constructorSelector
</details>

#### DiskProxy>>#simpleGlobalOrNil

Return the object I refer to if it is a simple global in Smalltalk.


<details>
	<summary>See more</summary>
	
	simpleGlobalOrNil
	"Return the object I refer to if it is a simple global in Smalltalk."

	preSelector ifNotNil: [^ nil].
	constructorSelector == #yourself ifFalse: [^ nil].
	^ Smalltalk at: globalObjectName ifAbsent: nil

</details>

#### DiskProxy>>#preSelector

<details>
	<summary>See more</summary>
	
	preSelector

	^ preSelector
</details>

#### DiskProxy>>#storeDataOn: aReferenceStream

Besides just storing, get me inserted into references, so structures will know about class DiskProxy.


<details>
	<summary>See more</summary>
	
	storeDataOn: aReferenceStream
	"Besides just storing, get me inserted into references, so structures will know about class DiskProxy."

	super storeDataOn: aReferenceStream.

	"just so instVarInfo: will find it and put it into structures"
"	aReferenceStream references at: self put: #none."
	aReferenceStream addSpecialReference: self
</details>

#### DiskProxy>>#globalObjectName

<details>
	<summary>See more</summary>
	
	globalObjectName
	^ globalObjectName
</details>

## DummyStream

The purpose of this class is to absorb all steam messages and do nothing. This is so ReferenceStream can pretend to write on it while traversing all objects it would normally write. We need to know what those object are. 8/17/96 tk

### Methods
#### DummyStream>>#skip: aNumber

Do nothing.


<details>
	<summary>See more</summary>
	
	skip: aNumber
	"Do nothing."
</details>

#### DummyStream>>#tab

<details>
	<summary>See more</summary>
	
	tab
</details>

#### DummyStream>>#nextPut: aByte

do nothing


<details>
	<summary>See more</summary>
	
	nextPut: aByte
	"do nothing"
</details>

#### DummyStream>>#position: anOffset

Pretend to position wherever the caller says!


<details>
	<summary>See more</summary>
	
	position: anOffset
	"Pretend to position wherever the caller says!"

</details>

#### DummyStream>>#useCharacters

<details>
	<summary>See more</summary>
	
	useCharacters

</details>

#### DummyStream>>#position

Return any random number. Here is where the real lying begins. We are a DummyStream afterall. 8/17/96 tk


<details>
	<summary>See more</summary>
	
	position
	"Return any random number.  Here is where the real lying begins.  We are a DummyStream afterall.  8/17/96 tk"

	^ 47 
</details>

#### DummyStream>>#padToEndIfCantTruncate

Only makes sense for file streams with existing content.


<details>
	<summary>See more</summary>
	
	padToEndIfCantTruncate
	"Only makes sense for file streams with existing content."
</details>

#### DummyStream>>#useBytes

A DummyStream can't switch between bytes and characters, but it doesn't matter. After all, all contents is discarded


<details>
	<summary>See more</summary>
	
	useBytes
	"A DummyStream can't switch between bytes and characters, but it doesn't matter.
	After all, all contents is discarded"
</details>

#### DummyStream>>#originalContents

<details>
	<summary>See more</summary>
	
	originalContents

	^ ''
</details>

#### DummyStream>>#subclassResponsibility

Do nothing. Most messages to class Stream are defined as subclassResponsibility. Just accept them. 8/17/96 tk


<details>
	<summary>See more</summary>
	
	subclassResponsibility
	"Do nothing.  Most messages to class Stream are defined as subclassResponsibility.  Just accept them.  8/17/96 tk"

	"No error.  Just go on."
</details>

#### DummyStream>>#nextPutAll: aByteArray

do nothing


<details>
	<summary>See more</summary>
	
	nextPutAll: aByteArray
	"do nothing"
</details>

#### DummyStream>>#contents

Answer all of the contents of the receiver.


<details>
	<summary>See more</summary>
	
	contents
	^nil
</details>

#### DummyStream>>#newLine

Append a newLine character to the receiver. The Cuis convention is to use lf on output.


<details>
	<summary>See more</summary>
	
	newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."
</details>

#### DummyStream>>#space

<details>
	<summary>See more</summary>
	
	space
</details>

## ReferenceStream

This is a way of serializing a tree of objects into disk file. A ReferenceStream can store one or more objects in a persistent form, including sharing and cycles. Here is the way to use DataStream and ReferenceStream: rr _ ReferenceStream fileNamed: 'test.obj'. rr nextPut: <your object>. rr close. To get it back: rr _ ReferenceStream fileNamed: 'test.obj'. <your object> _ rr next. rr close. ReferenceStreams can now write "weak" references. nextPutWeak: writes a "weak" reference to an object, which refers to that object *if* it also gets written to the stream by a normal nextPut:. A ReferenceStream should be treated as a read-stream *or* as a write-stream, *not* as a read/write-stream. The reference-remembering mechanism would probably do bad things if you tried to read and write from the same ReferenceStream. [TBD] Should we override "close" to do (self forgetReferences)? Instance variables references -- an IdentityDictionary mapping objects already written to their byteStream positions. If asked to write any object a second time, we just write a reference to its stream position. This handles shared objects and reference cycles between objects. To implement "weak references" (for Aliases), the references dictionary also maps objects not (yet?) written to a Collection of byteStream positions with hopeful weak-references to it. If asked to definitely write one of these objects, we'll fixup those weak references. objects -- an IdentityDictionary mapping relative byte stream positions to objects already read in. If asked to follow a reference, we return the object already read. This handles shared objects and reference cycles between objects. currentReference -- the current reference position. Positon relative to the start of object data in this file. (Allows user to cut and paste smalltalk code from the front of the file without effecting the reference values.) This variable is used to help install each new object in "objects" as soon as it's created, **before** we start reading its contents, in case any of its content objects reference it. fwdRefEnds -- A weak reference can be a forward reference, which requires advance-reading the referrent. When we later come to the object, we must get its value from "objects" and not re-read it so refs to it don't become refs to copies. fwdRefEnds remembers the ending byte stream position of advance-read objects. skipping -- true if <what?> If the object is referenced before it is done being created, it might get created twice. Just store the object the moment it is created in the 'objects' dictionary. If at the end, comeFullyUpOnReload returns a different object, some refs will have the temporary object (this is an unlikely case). At the moment, no implementor of comeFullyUpOnReload returns a different object except DiskProxy, and that is OK.

### Methods
#### ReferenceStream>>#beginReference: anObject

Remember anObject as the object we read at the position recorded by noteCurrentReference:. This must be done after instantiating anObject but before reading any of its contents that might (directly or indirectly) refer to it. (It's ok to do this redundantly, which is convenient for #next.) Answer the reference position.


<details>
	<summary>See more</summary>
	
	beginReference: anObject
	"Remember anObject as the object we read at the position recorded by
	 noteCurrentReference:. This must be done after instantiating anObject but
	 before reading any of its contents that might (directly or indirectly) refer to
	 it. (It's ok to do this redundantly, which is convenient for #next.)
	 Answer the reference position."

	objects at: currentReference ifAbsent: [
		objects at: currentReference put: anObject.
		^ currentReference].
	(skipping includes: currentReference) ifFalse: [
		"If reading just to skip it, don't record this copy."
		objects at: currentReference put: anObject
	].
	^ currentReference		"position relative to start of data portion of file"
</details>

#### ReferenceStream>>#blockers

<details>
	<summary>See more</summary>
	
	blockers

	^ blockers
</details>

#### ReferenceStream>>#beginInstance: aClass size: anInteger

This is for use by storeDataOn: methods. Cf. Object>>storeDataOn:.


<details>
	<summary>See more</summary>
	
	beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."
	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.
	In ReferenceStream, class is just 5 bytes for shared symbol.
	SmartRefStream puts out the names and number of class's instances variables for checking.
6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "

	| short ref |
	short _ true.	"All tests for object header that can be written in 4 bytes"
	anInteger <= 254 ifFalse: [short _ false].	"one byte size"
	ref _ references at: aClass name ifAbsent: [short _ false. nil].
	ref isInteger ifFalse: [short _ false].
	short ifTrue: [short _ (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"
	short ifTrue: [
		byteStream skip: -1.
		short _ byteStream next = 9.
		byteStream skip: 0].	"ugly workaround"
	short 
		ifTrue: ["passed all the tests!"
			byteStream skip: -1; nextPut: 16; "type = short header"
				nextPut: anInteger + 1;	"size is short"
				nextUnsignedInt16Put: ref bigEndian: true ]
		ifFalse: [
			"default to normal longer object header"
			byteStream nextUnsignedInt32Put: anInteger + 1 bigEndian: true.
			self nextPut: aClass name].
</details>

#### ReferenceStream>>#getCurrentReference

PRIVATE -- Return the currentReference posn. Always a relative position. So user can cut and paste the Smalltalk source code at the beginning of the file.


<details>
	<summary>See more</summary>
	
	getCurrentReference
    "PRIVATE -- Return the currentReference posn.  Always a relative position.  So user can cut and paste the Smalltalk source code at the beginning of the file."

    ^ currentReference	"relative position"
</details>

#### ReferenceStream>>#setStream: aStream reading: isReading

PRIVATE -- Initialization method.


<details>
	<summary>See more</summary>
	
	setStream: aStream reading: isReading
	"PRIVATE -- Initialization method."

	super setStream: aStream reading: isReading.
	"isReading ifFalse: [  when we are sure"
	references _ IdentityDictionary new: 4096 * 5.
	isReading ifTrue: [
		objects _ IdentityDictionary new: 4096 * 5.
		skipping _ IdentitySet new.
		fwdRefEnds _ IdentityDictionary new].
	blockers ifNil: [blockers _ IdentityDictionary new].	"keep blockers we just passed in"

</details>

#### ReferenceStream>>#references

Values can be: - integer: regular reference. Value is position in the output stream. - #none: special value for DiskProxy. Value is just a marker. - A collection of (still) weak only references. They are turned into regular references when the first regular reference to the key is dumped. See #tryToPutReference:typeID:


<details>
	<summary>See more</summary>
	
	references
	"Values can be:
		- integer: regular reference. Value is position in the output stream.
		- #none: special value for DiskProxy. Value is just a marker.
		- A collection of (still) weak only references. They are turned into regular references when the first regular reference to the key is dumped.
	See #tryToPutReference:typeID:"
	
	"Warning: Methods that need to deal with the internals of dumping weak references, or methods that need to actually modify the references dictionary CAN NOT use this method!"

	"Do not include provisory references created in #nextPutWeak that never became normal references,
	because the referenced object was never added from a call to #nextPut:"
	^ references select: [ :value | value isNumber or: [ value == #none ]]
</details>

#### ReferenceStream>>#next

Answer the next object in the stream. If this object was already read, don't re-read it. File is positioned just before the object.


<details>
	<summary>See more</summary>
	
	next
	"Answer the next object in the stream.  If this object was already read, don't re-read it.  File is positioned just before the object."
	| curPosn skipToPosn haveIt theObject wasSkipping |

	haveIt _ true.
	curPosn _ byteStream position - basePos.
	theObject _ objects at: curPosn ifAbsent: [haveIt _ false].
		"probe in objects is done twice when coming from objectAt:.  This is OK."
	skipToPosn _ fwdRefEnds at: curPosn ifAbsent: nil.
	haveIt ifFalse: [ ^ super next].

	skipToPosn ifNotNil: [
		"Skip over the object and return the already-read-in value."
		byteStream position: skipToPosn + basePos		"make absolute"
	] ifNil: [
		"File is not positioned correctly.  Read object and throw it away."
		wasSkipping _ skipping includes: curPosn.
		skipping add: curPosn.
		"fake _" super next.
		wasSkipping ifFalse: [skipping remove: curPosn ifAbsent: nil].
	].
	^ theObject
		
</details>

#### ReferenceStream>>#replace: original with: proxy

We may wish to remember that in some field, the original object is being replaced by the proxy. For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced.


<details>
	<summary>See more</summary>
	
	replace: original with: proxy
	"We may wish to remember that in some field, the original object is being replaced by the proxy.  For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced."

	blockers at: original put: proxy
</details>

#### ReferenceStream>>#objectAt: anInteger

PRIVATE -- Read & return the object at a given stream position. If we already read it, just get it from the objects dictionary. (Reading it again wouldn't work with cycles or sharing.) If not, go read it and put it in the objects dictionary. NOTE: This resolves a cross-reference in the ReferenceStream: 1. A backward reference to an object already read (the normal case). 2. A forward reference which is a sated weak reference (we record where the object ends so when we get to it normally we can fetch it from 'objects' and skip over it). 3. A backward reference to a 'non-reference type' per the long NOTE in nextPut: (we compensate here--seek back to re-read it and add the object to 'objects' to avoid seeking back to read it any more times). 4. While reading a foward weak reference (case 2), we may recursively hit an ordinary backward reference to an object that we haven't yet read because we temporarily skipped ahead. Such a reference is forward in time so we treat it much like case 2. 11/16-24/92 jhm: Handle forward refs. Cf. class comment and above NOTE. 08:57 tk anInteger is a relative position


<details>
	<summary>See more</summary>
	
	objectAt: anInteger
    "PRIVATE -- Read & return the object at a given stream position.
     If we already read it, just get it from the objects dictionary.
     (Reading it again wouldn't work with cycles or sharing.)
     If not, go read it and put it in the objects dictionary.
     NOTE: This resolves a cross-reference in the ReferenceStream:
       1. A backward reference to an object already read (the normal case).
       2. A forward reference which is a sated weak reference (we record where
          the object ends so when we get to it normally we can fetch it from
          'objects' and skip over it).
       3. A backward reference to a 'non-reference type' per the long NOTE in
          nextPut: (we compensate here--seek back to re-read it and add the object
          to 'objects' to avoid seeking back to read it any more times).
       4. While reading a foward weak reference (case 2), we may recursively hit an
          ordinary backward reference to an object that we haven't yet read because
          we temporarily skipped ahead. Such a reference is forward in time so we
          treat it much like case 2.
     11/16-24/92 jhm: Handle forward refs. Cf. class comment and above NOTE.
	08:57 tk   anInteger is a relative position"
    | savedPosn refPosn anObject |

    ^ objects at: anInteger "relative position.  case 1: It's in 'objects'"
        ifAbsent:   "do like super objectAt:, but remember the fwd-ref-end position"
            [savedPosn _ byteStream position.		"absolute"
            refPosn _ self getCurrentReference.	"relative position"

            byteStream position: anInteger + basePos.	"was relative"
            anObject _ self next.

            (self isAReferenceType: (self typeIDFor: anObject))
                ifTrue:  [fwdRefEnds at: anInteger put: byteStream position - basePos] "cases 2, 4"
                ifFalse: [objects at: anInteger put: anObject]. "case 3"

            self setCurrentReference: refPosn.		"relative position"
            byteStream position: savedPosn.		"absolute"
            anObject]
</details>

#### ReferenceStream>>#isAReferenceType: typeID

Return true iff typeID is one of the classes that can be written as a reference to an instance elsewhere in the stream.


<details>
	<summary>See more</summary>
	
	isAReferenceType: typeID
	"Return true iff typeID is one of the classes that can be written as a reference to an instance elsewhere in the stream."

	"too bad we can't put Booleans in an Array literal"
	^ (DataStream referenceTypes at: typeID) = 1
		"NOTE: If you get a bounds error here, the file probably has bad bits in it.  The most common cause is a file unpacking program that puts linefeeds after carriage returns."
</details>

#### ReferenceStream>>#addSpecialReference: aDiskProxy

See senders. Added to avoid breaking encapsulation (assuming that #references would answer the actual collection)


<details>
	<summary>See more</summary>
	
	addSpecialReference: aDiskProxy
	"See senders. Added to avoid breaking encapsulation (assuming that #references would answer the actual collection)"
	references at: aDiskProxy put: #none
</details>

#### ReferenceStream>>#statisticsOfRefs

Analyze the information in references, the objects being written out


<details>
	<summary>See more</summary>
	
	statisticsOfRefs
	"Analyze the information in references, the objects being written out"

	| parents n kids nm ownerBags tallies owners objParent normalReferences |
	normalReferences _ self references.	"Exclude unrealized weaks"
	parents _ IdentityDictionary new: normalReferences size * 2.
	n _ 0.
	'Finding Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:
		[ :parent | barBlock value: (n _ n+1).
		kids _ parent class isFixed
			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]
			ifFalse: [parent class isBits ifTrue: [Array new]
					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].
		(kids select: [:x | normalReferences includesKey: x])
			do: [:child | parents at: child put: parent]]].
	ownerBags _ Dictionary new.
	tallies _ Bag new.
	n _ 0.
	'Tallying Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"
		[ :obj | barBlock value: (n _ n+1).
		nm _ obj class name.
		tallies add: nm.
		owners _ ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].
		(objParent _ parents at: obj ifAbsent: nil) ifNotNil: [
			owners add: objParent class name]]].
	^ String streamContents: [ :strm | 
		tallies sortedCounts do: [ :assn |
			n _ assn key.  nm _ assn value.
			owners _ ownerBags at: nm.
			strm newLine; nextPutAll: nm; space; print: n.
			owners size > 0 ifTrue: [
				strm newLine; tab; print: owners sortedCounts]]]
</details>

#### ReferenceStream>>#tryToPutReference: anObject typeID: typeID

PRIVATE -- If we support references for type typeID, and if anObject already appears in my output stream, then put a reference to the place where anObject already appears. If we support references for typeID but didn't already put anObject, then associate the current stream position with anObject in case one wants to nextPut: it again. Return true after putting a reference; false if the object still needs to be put. : Added support for weak refs. Split out outputReference:. 08:42 tk references stores relative file positions.


<details>
	<summary>See more</summary>
	
	tryToPutReference: anObject typeID: typeID
	"PRIVATE -- If we support references for type typeID, and if
	   anObject already appears in my output stream, then put a
	   reference to the place where anObject already appears. If we
	   support references for typeID but didn't already put anObject,
	   then associate the current stream position with anObject in
	   case one wants to nextPut: it again.
	 Return true after putting a reference; false if the object still
	   needs to be put.
	 : Added support for weak refs. Split out outputReference:.
	08:42 tk  references stores relative file positions."
	| referencePosn nextPosn |

	"Is it a reference type of object?"
	(self isAReferenceType: typeID) ifFalse: [^ false].

	"Have we heard of and maybe even written anObject before?"
	referencePosn _ references at: anObject ifAbsent:
			["Nope. Remember it and let the sender write it."
			references at: anObject put: (byteStream position - basePos).	"relative"
			^ false].

	"If referencePosn is an Integer, it's the stream position of anObject."
	referencePosn isInteger ifTrue:
		[self outputReference: referencePosn.	"relative"
		^ true].

	referencePosn == #none ifTrue: ["for DiskProxy"
			references at: anObject put: (byteStream position - basePos).	"relative"
			^ false].


	"Else referencePosn is a collection of positions of weak-references to anObject.
	 Make them full references since we're about to really write anObject."
	references at: anObject put: (nextPosn _ byteStream position) - basePos.	"store relative"
	referencePosn do: [:weakRefPosn |
			byteStream position: weakRefPosn + basePos.		"make absolute"
			self outputReference: nextPosn - basePos].	"make relative"
	byteStream position: nextPosn.		"absolute"
	^ false
</details>

#### ReferenceStream>>#blockers: anIdentDict

maps objects -> nil if they should not be written. object -> anotherObject if they need substitution.


<details>
	<summary>See more</summary>
	
	blockers: anIdentDict
	"maps objects -> nil if they should not be written.  object -> anotherObject if they need substitution."

	anIdentDict class == IdentityDictionary ifFalse: [self error: 'must be IdentityDictionary'].
	blockers _ anIdentDict
</details>

#### ReferenceStream>>#nextPutWeak: anObject

Write a weak reference to anObject to the receiver stream. Answer anObject. If anObject is not a reference type of object, then just put it normally. A 'weak' reference means: If anObject gets written this stream via nextPut:, then its weak references will become normal references. Otherwise they'll read back as nil. --


<details>
	<summary>See more</summary>
	
	nextPutWeak: anObject
    "Write a weak reference to anObject to the receiver stream. Answer anObject.
     If anObject is not a reference type of object, then just put it normally.
     A 'weak' reference means: If anObject gets written this stream via nextPut:,
     then its weak references will become normal references. Otherwise they'll
     read back as nil. -- "
    | typeID referencePosn |

    "Is it a reference type of object? If not, just write it normally."
    typeID _ self typeIDFor: anObject.
    (self isAReferenceType: typeID) ifFalse: [^ self nextPut: anObject].

    "Have we heard of and maybe even written anObject before?"
    referencePosn _ references at: anObject ifAbsent: [
			references at: anObject put: OrderedCollection new].

    "If referencePosn is an Integer, it's the stream position of anObject.
     Else it's a collection of hopeful weak-references to anObject."
    referencePosn isInteger ifFalse:
        [referencePosn add: byteStream position - basePos.		"relative"
        referencePosn _ self vacantRef].
    self outputReference: referencePosn.		"relative"

    ^ anObject
</details>

#### ReferenceStream>>#objectIfBlocked: anObject

See if this object is blocked -- not written out and another object substituted.


<details>
	<summary>See more</summary>
	
	objectIfBlocked: anObject
	"See if this object is blocked -- not written out and another object substituted."

	^ blockers at: anObject ifAbsent: [anObject]
</details>

#### ReferenceStream>>#maybeBeginReference: internalObject

See if need to record a reference. In case in the file twice


<details>
	<summary>See more</summary>
	
	maybeBeginReference: internalObject
	"See if need to record a reference.  In case in the file twice"

	(self isAReferenceType: (self typeIDFor: internalObject))
			ifTrue: [self beginReference: internalObject].
			"save the final object and give it out next time."
	^ internalObject
</details>

#### ReferenceStream>>#reset

PRIVATE -- Reset my internal state. 11/15-17/92 jhm: Added transients and fwdRefEnds. 7/11/93 sw: Give substantial initial sizes to avoid huge time spent growing. 9/3/93 sw: monster version for Sasha


<details>
	<summary>See more</summary>
	
	reset
	"PRIVATE -- Reset my internal state.
	   11/15-17/92 jhm: Added transients and fwdRefEnds.
	   7/11/93 sw: Give substantial initial sizes to avoid huge time spent growing.
	   9/3/93 sw: monster version for Sasha"

	super reset.
	references _ IdentityDictionary new: 4096 * 5.
"	objects _ IdentityDictionary new: 4096 * 5.
	fwdRefEnds _ IdentityDictionary new.
"
	blockers ifNil: [blockers _ IdentityDictionary new].
 
</details>

#### ReferenceStream>>#setStream: aStream

PRIVATE -- Initialization method.


<details>
	<summary>See more</summary>
	
	setStream: aStream
	"PRIVATE -- Initialization method."

	super setStream: aStream.
	references _ IdentityDictionary new: 4096 * 5.
	objects _ IdentityDictionary new: 4096 * 5.
	fwdRefEnds _ IdentityDictionary new.
	skipping _ IdentitySet new.
	blockers ifNil: [blockers _ IdentityDictionary new].	"keep blockers we just passed in"

</details>

#### ReferenceStream>>#setCurrentReference: refPosn

PRIVATE -- Set currentReference to refPosn. Always a relative position.


<details>
	<summary>See more</summary>
	
	setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.  Always a relative position."

    currentReference _ refPosn		"relative position"
</details>

#### ReferenceStream>>#noteCurrentReference: typeID

PRIVATE -- If we support references for type typeID, remember the current byteStream position so beginReference: can add the next object to the 'objects' dictionary of reference positions, then return true. Else return false.


<details>
	<summary>See more</summary>
	
	noteCurrentReference: typeID
	"PRIVATE -- If we support references for type typeID, remember
	 the current byteStream position so beginReference: can add the
	 next object to the 'objects' dictionary of reference positions,
	 then return true. Else return false."
	| answer |

	(answer _ self isAReferenceType: typeID)
		ifTrue: [self setCurrentReference: (byteStream position - 1) - basePos "relative"
				"subtract 1 because we already read the object's type ID byte"].
	^ answer
</details>

## SmartRefStream

Ordinary ReferenceStreams assume that the names and order of instance variables is exactly the same when an object file is written and read. SmartRefStream allows object files to be read even after instance variables have changed or the entire class has been renamed. When an object file is written, no one knows how the classes will change in the future. Therefore, all conversion must be done when the file is read. The key is to store enough information in the file about the names of the instance variables of all outgoing classes. SmartRefStream works best with only one tree of objects per file. You can nextPut: more than once, but each object tree gets its own class structure description, which is big. Conversion of old objects is done by a method in each class called (convertToCurrentVersion: varDict refStream: smartRefStrm). At fileOut time, ChangeSet>>checkForConversionMethods creates a prototype of this method (if Preference #conversionMethodsAtFileOut is true). The programmer must edit this method to (1) test if the incoming object needs conversion, (2) put non-nil values into any new inst vars that need them, and (3) save the data of any inst vars that are being deleted. Determining which old version is represented by the incoming object can be done in several ways: noticing that a current inst var is nil when it should have data, noticing that there is an older inst var name in the variable dictionary (varDict), checking kinds of objects in one or more inst vars, or retrieving the classVersion of the incoming object from the ref stream. If a class is renamed, a method goes into SmartRefStream telling the new name. The conversion method of the new class must be prepared to accept instances of the old class also. If no inst var names have changed, the conversion method does nothing. An example: Suppose we change the representation of class Rectangle from ('origin' 'corner') to ('origin' 'extent'). Suppose lots of Rectangle instances are already out on files (in .pr project files, especially). The programmer changes the class definition, modifies all the methods, and filesOut. A series of dialogs appear, asking if instances Rectangle might be in an object file, if 'extent' needs to be non-nil (yes), and if the info in 'corner' needs to be preserved (yes). This method appears: Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm "These variables are automatically stored into the new instance: #('origin'). Test for this particular conversion. Get values using expressions like (varDict at: 'foo')." "New variables: #('extent'). If a non-nil value is needed, please assign it." "These are going away #('corner'). Possibly store their info in some other variable?" "Move your code above the ^ super... Delete extra comments." ^ super convertToCurrentVersion: varDict refStream: smartRefStrm The programmer modifies it to be: Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm (varDict includesKey: 'extent') ifFalse: ["old version!" "Create the new extent, and preserve the info from the old corner" extent _ (varDict at: 'corner') - origin. ]. ^ super convertToCurrentVersion: varDict refStream: smartRefStrm This conversion method stays in the system and is ready to convert the old format of Rectangle whenever one is encountered in an object file. Note that the subclasses of Rectangle, (B3DViewport, CharacterBlock, and Quadrangle) do not need conversion methods. Their instances will be converted by the code in Rectangle. Files written by SmartRefStream are in standard fileout format. You can mix raw objects with code to be filed in. The file starts out in the normal fileOut format. Definitions of new classes on the front. structures Dictionary of (#Rectangle -> #(<classVersionInteger> 'origin' 'corner')). Inst var names are strings. steady Set of Classes who have the same structure now as on the incoming file. Includes classes with same inst vars except for new ones added on the end. reshaped Dictionary of Classes who have a different structure now from the incoming file. Includes those with same inst vars but new version number. (old class name -> method selector to fill in data for version to version) renamed Dictionary of Classes who have a different name. Make an instance of the new class, and send it the conversion call. (old class name symbol -> new class name). renamedConv Dictionary of conversion selector for Classes who have a different name. (old class name symbol -> conversion selector). topCall Tells if next or nextPut: are working on the top object in the tree. nil if outside, the top object if deep inside. See DataStream.typeIDFor: for where the tangle of objects is clipped, so the whole system will not be written on the file. No object that is written on the file is ever a class. All class definitions are filed in. A class may be stored inside an ImageSegment that itself is stored in a SmartRefStream. There is a separate subclass for doing veryDeepCopy (in memory). Currently, any object for which objectToStoreOnDataStream return an object other than self, does this: The new object (a DiskProxy) is traced. When it comes time to go through the fields of the old object, they are not found as keys in references (DiskProxies are there instead). So the old field value is left in the new object. That is OK for StrikeFont, Class, MetaClass, DisplayScreen. But the DiskProxies are evaluated, which takes a lot of time. Some metaclasses are put into the structures table. This is for when a block has a receiver that is a class. See checkFatalReshape:. ImageSegments: A ReferenceStream is used to enumerate objects to put inside an ImageSegment. A SmartRefStream is used to store the ImageSegment. Roots are nil, and the segment is a wordArray. We are encoding the outPointers. Structures contains all classes from both places. --Ted Kaehler and Bob Arning.

### Methods
#### SmartRefStream>>#reshapedClassesIn: outPointers

Look for classes in the outPointer array that have changed shape. Make a fake class for the old shape. Return a dictionary mapping Fake classes to Real classes. Substitute fake classes for real ones in outPointers.


<details>
	<summary>See more</summary>
	
	reshapedClassesIn: outPointers
	"Look for classes in the outPointer array that have changed shape.  Make a fake class for the old shape.  Return a dictionary mapping Fake classes to Real classes.  Substitute fake classes for real ones in outPointers."

	| mapFakeClassesToReal fakeCls originalName |

	self flag: #bobconv.	


	mapFakeClassesToReal _ IdentityDictionary new.
	outPointers withIndexDo: [:outp :ind | 
		outp isBehavior ifTrue: [
			originalName _ renamedConv at: ind ifAbsent: [outp name].
				"in DiskProxy>>comeFullyUpOnReload: we saved the name at the index"
			fakeCls _ self mapClass: outp origName: originalName.
			fakeCls == outp ifFalse: [
				mapFakeClassesToReal at: fakeCls put: outp.
				outPointers at: ind put: fakeCls]]].
	^ mapFakeClassesToReal
</details>

#### SmartRefStream>>#renamed

<details>
	<summary>See more</summary>
	
	renamed

	self flag: #bobconv.	


	^ renamed
</details>

#### SmartRefStream>>#superclasses

<details>
	<summary>See more</summary>
	
	superclasses
	^superclasses
</details>

#### SmartRefStream>>#setStream: aStream reading: isReading

Initialize me.


<details>
	<summary>See more</summary>
	
	setStream: aStream reading: isReading
	"Initialize me. "

	self flag: #bobconv.	

	super setStream: aStream reading: isReading.
	isReading ifFalse: [^ false].
	self initShapeDicts.


</details>

#### SmartRefStream>>#nextPutObjOnly: anObject

Really write three objects: (version, class structure, object). But only when called from the outside. Not in fileOut format. No class definitions will be written for instance-specific classes. Error if find one. (Use nextPut: instead)


<details>
	<summary>See more</summary>
	
	nextPutObjOnly: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  Not in fileOut format.  No class definitions will be written for instance-specific classes.  Error if find one.  (Use nextPut: instead)"

	| info |
	topCall
		ifNil: [
			topCall _ anObject.
			super nextPut: ReferenceStream versionCode.
			'Please wait while objects are counted' displayProgressAt: Sensor mousePoint
				from: 0 to: 10
				during: [ :barBlock |
					info _ self instVarInfo: anObject].
			'Writing an object file' displayProgressAt: Sensor mousePoint
				from: 0 to: objCount*4	"estimate"
				during: [ :barBlock |
					objCount _ 0.
					progressBar _ barBlock.
					super nextPut: info.
					super nextPut: anObject.	"<- the real writing"
					"Class inst vars not written here!"].
			"references is an IDict of every object that got written
			(in case you want to take statistics)"
			"Transcript cr; show: structures keys printString."		"debug"
			topCall _ progressBar _ nil]	"reset it"
		ifNotNil: [
			super nextPut: anObject.
			progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].
</details>

#### SmartRefStream>>#instVarInfo: anObject

Return the object to write on the outgoing file that contains the structure of each class we are about to write out. Must be an Array whose first element is 'class structure'. Its second element is a Dictionary of pairs of the form #Rectangle -> #(<classVersion> 'origin' 'corner').


<details>
	<summary>See more</summary>
	
	instVarInfo: anObject
	"Return the object to write on the outgoing file that contains the structure of each class we are about to write out.  Must be an Array whose first element is 'class structure'.  Its second element is a Dictionary of pairs of the form #Rectangle -> #(<classVersion> 'origin' 'corner').  "

	"Make a pass through the objects, not writing, but recording the classes.  Construct a database of their inst vars and any version info (classVersion)."

	| dummy refs cls newSupers |
	structures _ Dictionary new.
	superclasses _ Dictionary new.
	dummy _ ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	refs _ dummy references.
	objCount _ refs size.		"for progress bar"
		"Note that Dictionary must not change its implementation!  If it does, how do we read this reading information?"
	refs keysDo: [:each | 
		cls _ each class.
		"cls isObsolete ifTrue: [self error: 'Trying to write ', cls name]."
		(cls class ~~ Metaclass) & (cls isObsolete not) ifTrue: [
			structures at: cls name put: false]].
	"Save work by only computing inst vars once for each class"
	newSupers _ Set new.
	structures at: #Point put: false.	"writeRectangle: does not put out class pointer"
	structures at: #Rectangle put: false.
	structures at: #LargePositiveInteger put: false.	"used in slow case of WordArray"
	structures keysDo: [:nm | 
		cls _ (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		cls allSuperclasses do: [:aSuper |
			structures at: aSuper name ifAbsent: [newSupers add: aSuper name]]].
			"Don't modify structures during iteration"
	newSupers do: [:nm | structures at: nm put: 3].	"Get all superclasses into list"
	structures keysDo: [:nm | "Nothing added to classes during loop"
		cls _ (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		structures at: nm put: 
			((Array with: cls classVersion), (cls allInstVarNames)).
		superclasses at: nm ifAbsent: [
				superclasses at: nm put: cls superclass name]].
	^ (Array with: 'class structure' with: structures with: 'superclasses' with: superclasses)
</details>

#### SmartRefStream>>#renamedConv

<details>
	<summary>See more</summary>
	
	renamedConv
	self flag: #bobconv.	


	^ renamedConv
</details>

#### SmartRefStream>>#moreObjects

Return true if there appears to be another object following this one on the file.


<details>
	<summary>See more</summary>
	
	moreObjects
	"Return true if there appears to be another object following this one on the file."

	| byte |
	byteStream atEnd ifTrue: [^ false].	"off end of file"
	(byte _ byteStream peek) ifNil: [^ false].	"off end of file"
	byte = 33 "$! asciiValue" ifTrue: [^ false].
	byte = 0 ifTrue: [^ false].
	^ byte <= DataStream referenceTypes size		"between 1 and 16"
</details>

#### SmartRefStream>>#writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName

The method convertToCurrentVersion:refStream: was not found in newClass. Write a default conversion method for the author to modify. If method exists, append new info into the end.


<details>
	<summary>See more</summary>
	
	writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify.  If method exists, append new info into the end."

	| code newOthers oldOthers copied newCode |

	newOthers _ newList asOrderedCollection "copy".
	oldOthers _ oldList asOrderedCollection "copy".
	copied _ OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code _ WriteStream on: (String new: 500).
	code newLine; newLine; tab; nextPutAll: '"From ', SystemVersion current version, ' [', Smalltalk lastUpdateString;
			nextPutAll: '] on ', Date today printString, '"'; newLine.
	code tab; nextPutAll: '"These variables are automatically stored into the new instance: '.
	code nextPutAll: copied asArray printString; nextPut: $.; newLine.
	code tab; nextPutAll: 'Test for this particular conversion.'; 
		nextPutAll: '  Get values using expressions like (varDict at: ''foo'')."'; newLine; newLine.
	(newOthers size = 0) & (oldOthers size = 0) & (oldName == nil) ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	(newOthers size > 0) ifTrue: [
		code tab; nextPutAll: '"New variables: ', newOthers asArray printString, 
			'.  If a non-nil value is needed, please assign it."'; newLine].
	(oldOthers size > 0) ifTrue: [
		code tab; nextPutAll: '"These are going away ', oldOthers asArray printString, 
			'.  Possibly store their info in some other variable?"'; newLine].
	oldName ifNotNil: [
		code tab; nextPutAll: '"Test for instances of class ', oldName, '.'; newLine.
		code tab; nextPutAll: 'Instance vars with the same name have been moved here."'; newLine.
		].
	code tab; nextPutAll: '"Move your code above the ^ super...  Delete extra comments."'; newLine. 

	(newClass includesSelector: #convertToCurrentVersion:refStream:) 
		ifTrue: ["append to old methods"
			newCode _ (newClass sourceCodeAt: #convertToCurrentVersion:refStream:),
				code contents]
		ifFalse: ["new method"
			newCode _ 'convertToCurrentVersion: varDict refStream: smartRefStrm',
				code contents, 
				'	^ super convertToCurrentVersion: varDict refStream: smartRefStrm'].
	newClass compile: newCode classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."

 
</details>

#### SmartRefStream>>#writeClassRenameMethod: sel was: oldName fromInstVars: oldList

The class coming is unknown. Ask the user for the existing class it maps to. If got one, write a method, and restart the obj fileIn. If none, write a dummy method and get the user to complete it later.


<details>
	<summary>See more</summary>
	
	writeClassRenameMethod: sel was: oldName fromInstVars: oldList 
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := FillInTheBlankMorph request: tell.	"class name"
			choice size = 0 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: 
					[newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := (Array with: answ classVersion) , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ
</details>

#### SmartRefStream>>#mapClass: newClass origName: originalName

See if instances changed shape. If so, make a fake class for the old shape and return it. Remember the original class name.


<details>
	<summary>See more</summary>
	
	mapClass: newClass origName: originalName
	"See if instances changed shape.  If so, make a fake class for the old shape and return it.  Remember the original class name."

	| newName oldInstVars fakeClass |
	newClass isMeta ifTrue: [^ newClass].
	newName _ newClass name.
	(steady includes: newClass) & (newName == originalName) ifTrue: [^ newClass].
		"instances in the segment have the right shape"
	oldInstVars _ structures at: originalName ifAbsent: [
			self error: 'class is not in structures list'].	"Missing in object file"
	fakeClass _ Object subclass: ('Fake37', originalName) asSymbol
		instanceVariableNames: oldInstVars allButFirst
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Obsolete'.
	ChangeSet changeSetForBaseSystem removeClassChanges: fakeClass name.	"reduce clutter"
	^ fakeClass

</details>

#### SmartRefStream>>#readInstanceSize: instSize clsname: className refPosn: refPosn

The common code to read the contents of an arbitrary instance. ASSUMES: readDataFrom:size: sends me beginReference: after it instantiates the new object but before reading nested objects. NOTE: We must restore the current reference position after recursive calls to next. Three cases for files from older versions of the system: 1) Class has not changed shape, read it straight. 2) Class has changed instance variables (or needs fixup). Call a particular method to do it. 3) There is a new class instead. Find it, call a particular method to read. All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change! See setStream:


<details>
	<summary>See more</summary>
	
	readInstanceSize: instSize clsname: className refPosn: refPosn
	"The common code to read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!  See setStream:"
	| anObject newName newClass dict oldInstVars |

	self flag: #bobconv.	

	self setCurrentReference: refPosn.  "remember pos before readDataFrom:size:"
	newName _ renamed at: className ifAbsent: [className].
	newClass _ Smalltalk at: newName.
	(steady includes: newClass) & (newName == className) ifTrue: [
	 	anObject _ newClass isVariable "Create it here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
		anObject _ anObject readDataFrom: self size: instSize.
		self setCurrentReference: refPosn.  "before returning to next"
		^ anObject].
	oldInstVars _ structures at: className ifAbsent: [
			self error: 'class is not in structures list'].	"Missing in object file"
	anObject _ newClass createFrom: self size: instSize version: oldInstVars.
		"only create the instance"
	self beginReference: anObject.
	dict _ self catalogValues: oldInstVars size: instSize.
		"indexed vars as (1 -> val) etc."
	dict at: #ClassName put: className.	"so conversion method can know it"

	"Give each superclass a chance to make its changes"
	self storeInstVarsIn: anObject from: dict.	"ones with the same names"

	anObject _ self applyConversionMethodsTo: anObject className: className varMap: dict.

	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
</details>

#### SmartRefStream>>#storeInstVarsIn: anObject from: dict

For instance variables with the same names, store them in the new instance. Values in variable-length part also. This is NOT the normal inst var transfer! See Object.readDataFrom:size:. This is for when inst var names have changed and some additional conversion is needed. Here we handle the unchanged vars.


<details>
	<summary>See more</summary>
	
	storeInstVarsIn: anObject from: dict
	"For instance variables with the same names, store them in the new instance.  Values in variable-length part also.  This is NOT the normal inst var transfer!  See Object.readDataFrom:size:.  This is for when inst var names have changed and some additional conversion is needed.  Here we handle the unchanged vars.  "

	(anObject class allInstVarNames) withIndexDo: [:varName :index |
		(dict includesKey: varName) ifTrue: [
			anObject instVarAt: index put: (dict at: varName)]].
	"variable part"
	(dict includesKey: #SizeOfVariablePart) ifFalse: [^ anObject].
	1 to: (dict at: #SizeOfVariablePart) do: [:index | 
		anObject basicAt: index put: (dict at: index)].
	^ anObject
</details>

#### SmartRefStream>>#readInstance

Read the contents of an arbitrary instance. ASSUMES: readDataFrom:size: sends me beginReference: after it instantiates the new object but before reading nested objects. NOTE: We must restore the current reference position after recursive calls to next. Three cases for files from older versions of the system: 1) Class has not changed shape, read it straight. 2) Class has changed instance variables (or needs fixup). Call a particular method to do it. 3) There is a new class instead. Find it, call a particular method to read. All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change! See setStream:


<details>
	<summary>See more</summary>
	
	readInstance
	"Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!  See setStream:"
	| instSize className refPosn |

	instSize _ (byteStream nextUnsignedInt32BigEndian: true) - 1.
	refPosn _ self getCurrentReference.
	className _ self next asSymbol.
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn

</details>

#### SmartRefStream>>#setStream: aStream

Initialize me.


<details>
	<summary>See more</summary>
	
	setStream: aStream
	"Initialize me. "

	self flag: #bobconv.	

	super setStream: aStream.
	self initShapeDicts.


</details>

#### SmartRefStream>>#structures

<details>
	<summary>See more</summary>
	
	structures
	^ structures
</details>

#### SmartRefStream>>#writeClassRename: newName was: oldName

Write a method that tells which modern class to map instances to.


<details>
	<summary>See more</summary>
	
	writeClassRename: newName was: oldName
	"Write a method that tells which modern class to map instances to."
	| oldVer sel code |

	oldVer _ self versionSymbol: (structures at: oldName).
	sel _ oldName asString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel _ sel, oldVer.	"i.e. #rectangleoc4"

	code _ WriteStream on: (String new: 500).
	code nextPutAll: sel; newLine.
	code newLine; tab; nextPutAll: '^ ', newName.	"Return new class"

	self class compile: code contents classified: 'conversion'.


</details>

#### SmartRefStream>>#nextPut: anObject

Really write three objects: (version, class structure, object). But only when called from the outside. If any instance-specific classes are present, prepend their source code. byteStream will be in fileOut format. You can see an analysis of which objects are written out by doing: (SmartRefStream statsOfSubObjects: anObject) (SmartRefStream tallyOfSubObjects: anObject) (SmartRefStream subObjects: anObject ofClass: aClass)


<details>
	<summary>See more</summary>
	
	nextPut: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.
	You can see an analysis of which objects are written out by doing: 
	(SmartRefStream statsOfSubObjects: anObject)
	(SmartRefStream tallyOfSubObjects: anObject)
	(SmartRefStream subObjects: anObject ofClass: aClass)"

| info |
topCall
	ifNil: [
		topCall _ anObject.
		'Please wait while objects are counted' 
			displayProgressAt: Sensor mousePoint
			from: 0 to: 10
			during: [ :barBlock | info _ self instVarInfo: anObject].
		byteStream binary.
		'Writing an object file' displayProgressAt: Sensor mousePoint
			from: 0 to: objCount*4	"estimate"
			during: [ :barBlock |
				objCount _ 0.
				progressBar _ barBlock.
				self setStream: byteStream reading: false.
					"set basePos, but keep any class renames"
				super nextPut: ReferenceStream versionCode.
				super nextPut: info.
				super nextPut: anObject.		"<- the real writing"
				].
					"Note: the terminator, $!, is not doubled inside object data"
		"references is an IDict of every object that got written"
		byteStream ascii.
		byteStream nextPutAll: '!'; newLine; newLine.
		byteStream padToEndIfCantTruncate.
		topCall _ progressBar _ nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].

</details>

#### SmartRefStream>>#initShapeDicts

Initialize me.


<details>
	<summary>See more</summary>
	
	initShapeDicts
	"Initialize me. "

	self flag: #bobconv.	

	"These must stay constant.  When structures read in, then things can change."
	steady _ {Array. Dictionary. Association. String. SmallInteger} asSet.

	renamed ifNil: [
		renamed _ Dictionary new.  "(old class name symbol -> new class name)"
		renamedConv _ Dictionary new "(oldClassNameSymbol -> conversionSelectorInNewClass)"
	].
	self initKnownRenames
</details>

#### SmartRefStream>>#mapClass: incoming

See if the old class named nm exists. If so, return it. If not, map it to a new class, and save the mapping in renamed.


<details>
	<summary>See more</summary>
	
	mapClass: incoming
	"See if the old class named nm exists.  If so, return it.  If not, map it to a new class, and save the mapping in renamed.  "

	| cls oldVer sel nm |

	self flag: #bobconv.	


	nm _ renamed at: incoming ifAbsent: [incoming].	"allow pre-mapping around collisions"
	(nm endsWith: ' class') 
		ifFalse: [cls _ Smalltalk at: nm ifAbsent: nil.
			cls ifNotNil: [^ cls]]  	"Known class.  It will know how to translate the instance."
		ifTrue: [cls _ Smalltalk at: nm substrings first asSymbol ifAbsent: nil.
			cls ifNotNil: [^ cls class]]. 	"Known class.  It will know how to translate the instance."
	oldVer _ self versionSymbol: (structures at: nm).
	sel _ nm asString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel _ sel, oldVer.	"i.e. #rectangleoc4"
	Symbol hasInterned: sel ifTrue: [:symb | 
		(self class canUnderstand: sel asSymbol) ifTrue: [
			cls _ self perform: sel asSymbol]].	"This class will take responsibility"
	cls ifNil: [cls _ self writeClassRenameMethod: sel was: nm
					fromInstVars: (structures at: nm).
			   cls class == String ifTrue: [cls _ nil]].
	cls ifNotNil: [renamed at: nm put: cls name].
	^ cls

</details>

#### SmartRefStream>>#checkFatalReshape: setOfClasses

Inform the user if any of these classes were reshaped. A block has a method from the old system whose receiver is of this class. The method's inst var references might be wrong. OK if inst vars were only added.


<details>
	<summary>See more</summary>
	
	checkFatalReshape: setOfClasses
	| suspects oldInstVars newInstVars bad className |
	"Inform the user if any of these classes were reshaped.  A block has a method from the old system whose receiver is of this class.  The method's inst var references might be wrong.  OK if inst vars were only added."

	self flag: #bobconv.	

	setOfClasses isEmpty ifTrue: [^ self].
	suspects _ OrderedCollection new.
	setOfClasses do: [:aClass |
		className _ renamed keyAtValue: aClass name ifAbsent: [aClass name].
		oldInstVars _ (structures at: className ifAbsent: [#(0)]) allButFirst.		"should be there"
		newInstVars _ aClass allInstVarNames.
		oldInstVars size > newInstVars size ifTrue: [bad _ true].
		oldInstVars size = newInstVars size ifTrue: [
			bad _ oldInstVars ~= newInstVars].
		oldInstVars size < newInstVars size ifTrue: [
			bad _ oldInstVars ~= (newInstVars copyFrom: 1 to: oldInstVars size)].
		bad ifTrue: [suspects add: aClass]].

	suspects isEmpty ifFalse: [
		self inform: ('Imported foreign methods will run on instances of:\',
			suspects asArray printString, 
			'\whose shape has changed.  Errors may occur.') withNewLines].
</details>

#### SmartRefStream>>#versionSymbol: instVarList

Create the symbolic code (like a version number) for this class in some older version. First initials of all the inst vars, followed by the class version number. Returns a string, caller makes it into a compound selector.


<details>
	<summary>See more</summary>
	
	versionSymbol: instVarList
	"Create the symbolic code (like a version number) for this class in some older version.  First initials of all the inst vars, followed by the class version number.  Returns a string, caller makes it into a compound selector.  "

	| str |
	str _ instVarList size = 1 ifFalse: [''] ifTrue: ['x'].		"at least one letter"
	2 to: instVarList size do: [:ind |
		str _ str, (instVarList at: ind) first asString].
	str _ str, instVarList first printString.	"the number"
	^ str

" | list | list _ (Array with: Paragraph classVersion), (Paragraph alistInstVarNames).
(SmartRefStream  on: (DummyStream on: nil)) versionSymbol: list
"
</details>

#### SmartRefStream>>#next

Really write three objects: (version, class structure, object). But only when called from the outside.


<details>
	<summary>See more</summary>
	
	next
	"Really write three objects: (version, class structure, object). But only when called from the outside.  "

	| version ss object |
	^ topCall
		ifNil: [ 
			topCall _ #marked.
			version _ super next.
			version class == SmallInteger ifFalse: [^ version].	
				"version number, else just a regular object, not in our format, "
			ss _ super next.
			ss class == Array ifFalse: [^ ss].  "just a regular object"
			(ss at: 1) = 'class structure' ifFalse: [^ ss].
			structures _ ss at: 2.
			superclasses _ (ss size > 3 and: [(ss at: 3) = 'superclasses']) 
				ifTrue: [ss at: 4]		"class name -> superclass name"
				ifFalse: [Dictionary new].
			(self verifyStructure = 'conversion method needed') ifTrue: [^ nil].
			object _ super next.	"all the action here"

			topCall _ nil.	"reset it"
			object]
		ifNotNil: [
			super next]

</details>

#### SmartRefStream>>#superclasses: anObject

<details>
	<summary>See more</summary>
	
	superclasses: anObject
	superclasses _ anObject
</details>

#### SmartRefStream>>#noHeader

Signal that we've already dealt with the version and structure array, and are now reading objects.


<details>
	<summary>See more</summary>
	
	noHeader
	"Signal that we've already dealt with the version and structure array, and are now reading objects."

	topCall _ #marked.

</details>

#### SmartRefStream>>#verifyStructure

Compare the incoming inst var name lists with the existing classes. Prepare tables that will help to restructure those who need it (renamed, reshaped, steady). If all superclasses are recorded in the file, only compare inst vars of this class, not of superclasses. They will get their turn.


<details>
	<summary>See more</summary>
	
	verifyStructure
	"Compare the incoming inst var name lists with the existing classes.  Prepare tables that will help to restructure those who need it (renamed, reshaped, steady).    If all superclasses are recorded in the file, only compare inst vars of this class, not of superclasses.  They will get their turn.  "


	| newClass newList oldList converting |

	self flag: #bobconv.	

	converting _ OrderedCollection new.
	structures keysDo: [:nm "an old className (symbol)" |
		"For missing classes, there needs to be a method in SmartRefStream like 
			#rectangleoc2 that returns the new class."
		newClass _ self mapClass: nm.	   "does (renamed at: nm put: newClass name)"
		newClass class == String ifTrue: [^ newClass].  "error, fileIn needed"
		newList _ (Array with: newClass classVersion), (newClass allInstVarNames).
		oldList _ structures at: nm.
		newList = oldList 
			ifTrue: [steady add: newClass]  "read it in as written"
			ifFalse: [converting add: newClass name]
	].
	false & converting isEmpty not ifTrue: ["debug" 
			self inform: 'These classes are being converted from existing methods:\' withNewLines,
				converting asArray printString].

</details>

#### SmartRefStream>>#catalogValues: instVarList size: varsOnDisk

Create a dictionary of (name -> value) for the inst vars of this reshaped object. Indexed vars as (1 -> val) etc.


<details>
	<summary>See more</summary>
	
	catalogValues: instVarList size: varsOnDisk
	"Create a dictionary of (name -> value) for the inst vars of this reshaped object.  Indexed vars as (1 -> val) etc.  "

	| dict sz |
	dict _ Dictionary new.
	2 to: instVarList size do: [:ind |
		dict at: (instVarList at: ind) put: self next].
	sz _ varsOnDisk - (instVarList size - 1).
	1 to: sz do: [:ii | 
		dict at: ii put: self next].
	"Total number read MUST be equal to varsOnDisk!"
	sz > 0 ifTrue: [dict at: #SizeOfVariablePart put: sz].
	^ dict
</details>

#### SmartRefStream>>#readShortInst

Instance has just one byte of size. Class symbol is encoded in two bytes of file position. See readInstance.


<details>
	<summary>See more</summary>
	
	readShortInst
	"Instance has just one byte of size.  Class symbol is encoded in two bytes of file position.  See readInstance."
	| instSize className refPosn |

	instSize _ (byteStream next) - 1.	"one byte of size"
	refPosn _ self getCurrentReference.
	className _ self readShortRef.	"class symbol in two bytes of file pos"
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn

</details>

#### SmartRefStream>>#writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList

The method convertToCurrentVersion:refStream: was not found in newClass. Write a default conversion method for the author to modify.


<details>
	<summary>See more</summary>
	
	writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify."

	| code newOthers oldOthers copied |

	code _ WriteStream on: (String new: 500).
	code nextPutAll: 'convertToCurrentVersion: varDict refStream: smartRefStrm'; newLine; tab.
	newOthers _ newList asOrderedCollection "copy".
	oldOthers _ oldList asOrderedCollection "copy".
	copied _ OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code nextPutAll: '"These variables are automatically stored into the new instance '.
	code nextPutAll: copied asArray printString; nextPut: $. .
	code newLine; tab; nextPutAll: 'This method is for additional changes.'; 
		nextPutAll: ' Use statements like (foo _ varDict at: ''foo'')."'; newLine; newLine; tab.
	(newOthers size = 0) & (oldOthers size = 0) ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	(newOthers size > 0) ifTrue: [code nextPutAll: '"New variables: ', newOthers asArray printString, '  If a non-nil value is needed, please assign it."\' withNewLines].
	(oldOthers size > 0) ifTrue: [code nextPutAll: '	"These are going away ', oldOthers asArray printString, '.  Possibly store their info in some other variable?"'].

	code newLine; tab.
	code nextPutAll: '^ super convertToCurrentVersion: varDict refStream: smartRefStrm'.
	newClass compile: code contents classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."

 
</details>

#### SmartRefStream>>#structures: anObject

<details>
	<summary>See more</summary>
	
	structures: anObject
	structures _ anObject
</details>

#### SmartRefStream>>#applyConversionMethodsTo: objectIn className: className varMap: varMap

<details>
	<summary>See more</summary>
	
	applyConversionMethodsTo: objectIn className: className varMap: varMap

	| anObject prevObject |

	self flag: #bobconv.	

	anObject _ objectIn.
	[
		prevObject _ anObject.
		anObject _ anObject convertToCurrentVersion: varMap refStream: self.
		prevObject == anObject
	] whileFalse.
	^anObject

</details>

#### SmartRefStream>>#initKnownRenames

Stuff like


<details>
	<summary>See more</summary>
	
	initKnownRenames
	"Stuff like"
	"
	renamed
		at: #FlasherMorph put: #Flasher;
		yourself
	"
</details>

#### SmartRefStream>>#conversionMethodsFor: classList

Each of these needs a conversion method. Hard part is the comment in it. Return a MessageSet.


<details>
	<summary>See more</summary>
	
	conversionMethodsFor: classList
	| oldStruct newStruct list |
	"Each of these needs a conversion method.  Hard part is the comment in it.  Return a MessageSet."

	list _ OrderedCollection new.
	classList do: [:cls |
		oldStruct _ structures at: cls name ifAbsent: [#()].
		newStruct _ (Array with: cls classVersion), (cls allInstVarNames).
		self writeConversionMethodIn: cls fromInstVars: oldStruct to: newStruct 
				renamedFrom: nil.
		list add: cls name, ' convertToCurrentVersion:refStream:'.
		].

	^ MessageSet new initializeMessageList: list.
</details>

#### SmartRefStream>>#convert1: misShapenInst to: goodClass allVarMaps: allVarMaps

Go through the normal instance conversion process and return a modern object.


<details>
	<summary>See more</summary>
	
	convert1: misShapenInst to: goodClass allVarMaps: allVarMaps
	"Go through the normal instance conversion process and return a modern object."

	| className oldInstVars anObject varMap |

	self flag: #bobconv.	

	goodClass isVariable ifTrue: [
		goodClass error: 'shape change for variable class not implemented yet'
	].
	(misShapenInst class name beginsWith: 'Fake37') ifFalse: [self error: 'why mapping?'].
	className _ (misShapenInst class name allButFirst: 6) asSymbol.
	oldInstVars _ structures at: className.
	anObject _ goodClass basicNew.

	varMap _ Dictionary new.	"later, indexed vars as (1 -> val) etc."
	2 to: oldInstVars size do: [:ind |
		varMap at: (oldInstVars at: ind) put: (misShapenInst instVarAt: ind-1)].
	varMap at: #ClassName put: className.	"original"
	varMap at: #NewClassName put: goodClass name.	"new"
	self storeInstVarsIn: anObject from: varMap. 	"ones with the same names"
	allVarMaps at: misShapenInst put: varMap.
	^ anObject

</details>

#### SmartRefStream>>#convert2: partiallyCorrectInst allVarMaps: allVarMaps

Go through the normal instance conversion process and return a modern object.


<details>
	<summary>See more</summary>
	
	convert2: partiallyCorrectInst allVarMaps: allVarMaps
	"Go through the normal instance conversion process and return a modern object."

	| className varMap |

	self flag: #bobconv.	

	varMap _ allVarMaps at: partiallyCorrectInst.
	className _ varMap at: #ClassName.	"original"
	^self applyConversionMethodsTo: partiallyCorrectInst className: className varMap: varMap.


</details>

