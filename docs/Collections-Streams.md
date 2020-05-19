## Base64MimeConverter

This class encodes and decodes data in Base64 format. This is MIME encoding. We translate a whole stream at once, taking a Stream as input and giving one as output. Returns a whole stream for the caller to use. 0 A 17 R 34 i 51 z 1 B 18 S 35 j 52 0 2 C 19 T 36 k 53 1 3 D 20 U 37 l 54 2 4 E 21 V 38 m 55 3 5 F 22 W 39 n 56 4 6 G 23 X 40 o 57 5 7 H 24 Y 41 p 58 6 8 I 25 Z 42 q 59 7 9 J 26 a 43 r 60 8 10 K 27 b 44 s 61 9 11 L 28 c 45 t 62 + 12 M 29 d 46 u 63 / 13 N 30 e 47 v 14 O 31 f 48 w (pad) = 15 P 32 g 49 x 16 Q 33 h 50 y Outbound: bytes are broken into 6 bit chunks, and the 0-63 value is converted to a character. 3 data bytes go into 4 characters. Inbound: Characters are translated in to 0-63 values and shifted into 8 bit bytes. (See: N. Borenstein, Bellcore, N. Freed, Innosoft, Network Working Group, Request for Comments: RFC 1521, September 1993, MIME (Multipurpose Internet Mail Extensions) Part One: Mechanisms for Specifying and Describing the Format of Internet Message Bodies. Sec 6.2) By Ted Kaehler, based on Tim Olson's Base64Filter. In Cuis, we only support bytes as the data stream (not Strings). See https://en.wikipedia.org/wiki/Base64

### Methods
#### Base64MimeConverter>>#mimeDecodeToByteArray

Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full ByteArray of 0-255 values. Reutrn a whole stream for the user to read.


<details>
	<summary>See more</summary>
	
	mimeDecodeToByteArray
	"Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full ByteArray of 0-255 values.  Reutrn a whole stream for the user to read."

	| nibA nibB nibC nibD |
	[mimeStream atEnd] whileFalse: [
		(nibA _ self nextValue) ifNil: [^ dataStream].
		(nibB _ self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibA bitShift: 2) + (nibB bitShift: -4)).
		nibB _ nibB bitAnd: 16rF.
		(nibC _ self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibB bitShift: 4) + (nibC bitShift: -2)).
		nibC _ nibC bitAnd: 16r3.
		(nibD _ self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibC bitShift: 6) + nibD).
		].
	^ dataStream
</details>

#### Base64MimeConverter>>#multiLine: aBool

Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)


<details>
	<summary>See more</summary>
	
	multiLine: aBool
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	multiLine := aBool
</details>

#### Base64MimeConverter>>#dataStream

<details>
	<summary>See more</summary>
	
	dataStream
	^dataStream
</details>

#### Base64MimeConverter>>#dataStream: aByteStream

aByteStream must hold bytes, i.e. integers in [0..255]


<details>
	<summary>See more</summary>
	
	dataStream: aByteStream
	"aByteStream must hold bytes, i.e. integers in [0..255]"

	dataStream _ aByteStream
</details>

#### Base64MimeConverter>>#nextValue

The next six bits of data char from the mimeStream, or nil. Skip all other chars


<details>
	<summary>See more</summary>
	
	nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	FromCharTable ifNil: [ self class initializeTables ].
	[
		raw := mimeStream next.
		raw ifNil: [^ nil].	"end of stream"
		raw == $= ifTrue: [^ nil].
		num := FromCharTable at: raw asciiValue + 1.
		num ifNotNil: [^ num].
		"else ignore space, return, tab, ..."
	] repeat
</details>

#### Base64MimeConverter>>#multiLine

Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)


<details>
	<summary>See more</summary>
	
	multiLine
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	^multiLine
</details>

#### Base64MimeConverter>>#mimeStream: aCharacterStream

aCharacterStream is in base 64 with only a-z,A-Z,0-9,+,/ .


<details>
	<summary>See more</summary>
	
	mimeStream: aCharacterStream
	"aCharacterStream is in base 64 with only a-z,A-Z,0-9,+,/ ."

	mimeStream _ aCharacterStream
</details>

#### Base64MimeConverter>>#mimeStream

<details>
	<summary>See more</summary>
	
	mimeStream
	^mimeStream
</details>

#### Base64MimeConverter>>#mimeEncode

Do conversion reading from dataStream writing to mimeStream


<details>
	<summary>See more</summary>
	
	mimeEncode
	"Do conversion reading from dataStream writing to mimeStream"
	"Convert from data to 6 bit characters."

	| phase1 phase2 byte nib lineLength |
	ToCharTable ifNil: [ self class initializeTables ].
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		(multiLine and:[lineLength >= 70]) ifTrue: [ mimeStream newLine.  lineLength := 0. ].
		data := byte := dataStream next.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + byte.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase2 := true].
		data := ((data bitAnd: 16rF) bitShift: 8) + (byte).
		nib := (data bitAnd: 16rFC0) bitShift: -6.
		mimeStream nextPut: (ToCharTable at: nib+1).
		nib := (data bitAnd: 16r3F).
		mimeStream nextPut: (ToCharTable at: nib+1).

		lineLength := lineLength + 4.].
	phase1 ifTrue: [
		mimeStream skip: -2; nextPut: $=; nextPut: $=.
		^ mimeStream].
	phase2 ifTrue: [
		mimeStream skip: -1; nextPut: $=.
		^ mimeStream]
</details>

## LimitedWriteStream

A LimitedWriteStream is a specialized WriteStream that has a maximum size of the collection it streams over. When this limit is reached a special limitBlock is executed. This can for example be used to "bail out" of lengthy streaming operations before they have finished. For a simple example take a look at the universal Object printString. The message SequenceableCollection class streamContents:limitedTo: creates a LimitedWriteStream. In this case it prevents very large (or possibly recursive) object structures to "overdo" their textual representation.

### Methods
#### LimitedWriteStream>>#nextPut: anObject

Ensure that the limit is not exceeded


<details>
	<summary>See more</summary>
	
	nextPut: anObject 
	"Ensure that the limit is not exceeded"

	^position >= limit
		ifTrue: [limitBlock value]
		ifFalse: [super nextPut: anObject]
</details>

#### LimitedWriteStream>>#nextPutAll: aCollection

Append the elements of aCollection to the sequence of objects accessible by the receiver. Answer aCollection.


<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection

	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > limit ifTrue: [
		super nextPutAll: (aCollection copyFrom: 1 to: (limit - position max: 0)).
		^ limitBlock value.
	].
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10
	].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.
</details>

#### LimitedWriteStream>>#setLimit: sizeLimit limitBlock: aBlock

Limit the numer of elements this stream will write...


<details>
	<summary>See more</summary>
	
	setLimit: sizeLimit limitBlock: aBlock
	"Limit the numer of elements this stream will write..."
	limit _ sizeLimit.
	"Execute this (typically ^ contents) when that limit is exceded"
	limitBlock _ aBlock
</details>

#### LimitedWriteStream>>#pastEndPut: anObject

Grow the collection. Then we put <anObject> at the current write position.


<details>
	<summary>See more</summary>
	
	pastEndPut: anObject
	collection size >= limit ifTrue: [limitBlock value].  "Exceptional return"
	^ super pastEndPut: anObject
</details>

## PositionableStream

I represent an accessor for a sequence of objects (a collection) that are externally named by indices so that the point of access can be repositioned. I am abstract in that I do not implement the messages next and nextPut: which are inherited from my superclass Stream.

### Methods
#### PositionableStream>>#oldBack

Go back one element and return it. Use indirect messages in case I am a StandardFileStream


<details>
	<summary>See more</summary>
	
	oldBack
	"Go back one element and return it.  Use indirect messages in case I am a StandardFileStream"
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #back and #oldBack behavior. (Damien Cassou - 1 August 2007)"
	self position = 0 ifTrue: [self error: 'CantGoBack'].
	self position = 1 ifTrue: [self position: 0.  ^ nil].
	self skip: -2.
	^ self next

</details>

#### PositionableStream>>#contentsOfEntireFile

For non-file streams


<details>
	<summary>See more</summary>
	
	contentsOfEntireFile
	"For non-file streams"
	^ self contents
</details>

#### PositionableStream>>#resetContents

Set the position and limits to 0.


<details>
	<summary>See more</summary>
	
	resetContents
	"Set the position and limits to 0."

	position _ 0.
	readLimit _ 0
</details>

#### PositionableStream>>#position: anInteger

Set the current position for accessing the objects to be anInteger, as long as anInteger is within the bounds of the receiver's contents. If it is not, create an error notification.


<details>
	<summary>See more</summary>
	
	position: anInteger 
	"Set the current position for accessing the objects to be anInteger, as long 
	as anInteger is within the bounds of the receiver's contents. If it is not, 
	create an error notification."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position _ anInteger]
		ifFalse: [self positionError]
</details>

#### PositionableStream>>#positionError

Since I am not necessarily writable, it is up to my subclasses to override position: if expanding the collection is preferrable to giving this error.


<details>
	<summary>See more</summary>
	
	positionError
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'
</details>

#### PositionableStream>>#compileNextChunk

<details>
	<summary>See more</summary>
	
	compileNextChunk
		
	(self peekFor: $!) 
		ifTrue: [ self compileNextChunkWhenStartsWithExclamationMark ]
		ifFalse: [ self compileNextChunkWhenDoesNotStartWithExclamationMark ]
</details>

#### PositionableStream>>#peekFor: anObject

Answer false and do not move over the next element if it is not equal to the argument, anObject, or if the receiver is at the end. Answer true and increment the position for accessing elements, if the next element is equal to anObject.


<details>
	<summary>See more</summary>
	
	peekFor: anObject 
	"Answer false and do not move over the next element if it is not equal to 
	the argument, anObject, or if the receiver is at the end. Answer true 
	and increment the position for accessing elements, if the next element is 
	equal to anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject _ self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position _ position - 1.
	^false
</details>

#### PositionableStream>>#atEnd

Answer whether the receiver can access any more objects.


<details>
	<summary>See more</summary>
	
	atEnd
	"Answer whether the receiver can access any more objects."

	^position >= readLimit
</details>

#### PositionableStream>>#nextChunk

Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character.


<details>
	<summary>See more</summary>
	
	nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator _ $!.
	out _ WriteStream on: (String new: 1000).
	self skipSeparators.
	[(ch _ self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents
</details>

#### PositionableStream>>#notEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	notEmpty
	"Answer whether the receiver contains any elements."

	^ self isEmpty not
</details>

#### PositionableStream>>#upTo: anObject

Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of anObject in the receiver. If anObject is not in the collection, answer the entire rest of the receiver.


<details>
	<summary>See more</summary>
	
	upTo: anObject 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (self collectionSpecies new: 100).
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents
</details>

#### PositionableStream>>#setFrom: newStart to: newStop

<details>
	<summary>See more</summary>
	
	setFrom: newStart to: newStop

	position _ newStart - 1.
	readLimit _ newStop
</details>

#### PositionableStream>>#nextAvailable: aNumber

<details>
	<summary>See more</summary>
	
	nextAvailable: aNumber
	^self next: aNumber
</details>

#### PositionableStream>>#fileInInformingTo: barBlock

<details>
	<summary>See more</summary>
	
	fileInInformingTo: barBlock 
	
	[ self atEnd ] whileFalse: [
		barBlock value: self position.
		self skipSeparators.
		self compileNextChunkHandlingExceptions ]
</details>

#### PositionableStream>>#peek

Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.


<details>
	<summary>See more</summary>
	
	peek
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^nil].
	nextObject _ self next.
	position _ position - 1.
	^nextObject
</details>

#### PositionableStream>>#untilEnd: aBlock displayingProgress: aString

<details>
	<summary>See more</summary>
	
	untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			[ self atEnd ] whileFalse: [
				barBlock value: self position.
				aBlock value ]]
</details>

#### PositionableStream>>#compileNextChunkWhenDoesNotStartWithExclamationMark

<details>
	<summary>See more</summary>
	
	compileNextChunkWhenDoesNotStartWithExclamationMark

	| chunk |
	
	chunk := self nextChunk.
	self checkForPreamble: chunk.
	self evaluate: [ Compiler evaluate: chunk logged: true ] printingErrorWith: chunk
		
</details>

#### PositionableStream>>#compileNextChunkWhenStartsWithExclamationMark

<details>
	<summary>See more</summary>
	
	compileNextChunkWhenStartsWithExclamationMark

	| chunk |
	
	chunk := self nextChunk.
	
	"These are the ones that should do nothing, 
	because next line is a doit that does the stuff
	(or because it is handled elsewhere)"
	
	((chunk beginsWith: 'description:  ') 
		or: [ ChangeList knownFileInPreambles anySatisfy: [ :aPreamble | chunk beginsWith: aPreamble ]])
		ifFalse: [ self evaluate: [ (Compiler evaluate: chunk logged: false) scanFrom: self ] printingErrorWith: chunk ]
</details>

#### PositionableStream>>#nextInto: aCollection startingAt: startIndex

Read the next elements of the receiver into aCollection. Return aCollection or a partial copy if less than aCollection size elements have been read.


<details>
	<summary>See more</summary>
	
	nextInto: aCollection startingAt: startIndex
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.
</details>

#### PositionableStream>>#collectionSpecies

Answer the species of collection into which the receiver can stream


<details>
	<summary>See more</summary>
	
	collectionSpecies
	"Answer the species of collection into which the receiver can stream"
	
	^collection species
</details>

#### PositionableStream>>#skip: n

Skips the next amount objects in the receiver's future sequence values.


<details>
	<summary>See more</summary>
	
	skip: n 
	"Skips the next amount objects in the receiver's future sequence values."

	self position: (self position + (n min: (self contents size - self position)))
</details>

#### PositionableStream>>#padTo: nBytes put: aCharacter

Pad using the argument, aCharacter, to the next boundary of nBytes characters.


<details>
	<summary>See more</summary>
	
	padTo: nBytes put: aCharacter 
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem _ nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	self next: rem put: aCharacter.
</details>

#### PositionableStream>>#upToEnd

Answer a subcollection from the current access position through the last element of the receiver.


<details>
	<summary>See more</summary>
	
	upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream |
	newStream := WriteStream on: (self collectionSpecies new: 100).
	[self atEnd] whileFalse: [ newStream nextPut: self next ].
	^ newStream contents
</details>

#### PositionableStream>>#peekBack

Return the element at the previous position, without changing position. Use indirect messages in case self is a StandardFileStream.


<details>
	<summary>See more</summary>
	
	peekBack
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."

	| element |
	element _ self back.
	self skip: 1.
	^ element
</details>

#### PositionableStream>>#copyMethodChunkFrom: aStream

Copy the next chunk from aStream (must be different from the receiver).


<details>
	<summary>See more</summary>
	
	copyMethodChunkFrom: aStream
	"Copy the next chunk from aStream (must be different from the receiver)."

	self nextChunkPut: aStream nextChunk
</details>

#### PositionableStream>>#fileInAnnouncing: announcement

This is special for reading expressions from text that has been formatted with exclamation delimitors. The expressions are read and passed to the Compiler. Put up a progress report with the given announcement as the title.


<details>
	<summary>See more</summary>
	
	fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Put up a progress report with the given announcement as the title."

	Utilities logsUserChanges: false.

	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | self fileInInformingTo: barBlock ].

	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	
</details>

#### PositionableStream>>#position

Answer the current position of accessing the sequence of objects.


<details>
	<summary>See more</summary>
	
	position
	"Answer the current position of accessing the sequence of objects."

	^position
</details>

#### PositionableStream>>#match: subCollection

Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found. No wildcards, and case does matter.


<details>
	<summary>See more</summary>
	
	match: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	| pattern startMatch |
	pattern _ ReadStream on: subCollection.
	startMatch _ nil.
	[pattern atEnd] whileFalse: 
		[self atEnd ifTrue: [^ false].
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch _ self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch.
						startMatch _ nil]]].
	^ true


</details>

#### PositionableStream>>#nextInto: aCollection

Read the next elements of the receiver into aCollection. Return aCollection or a partial copy if less than aCollection size elements have been read.


<details>
	<summary>See more</summary>
	
	nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
</details>

#### PositionableStream>>#oldPeekBack

Return the element at the previous position, without changing position. Use indirect messages in case self is a StandardFileStream.


<details>
	<summary>See more</summary>
	
	oldPeekBack
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."

	| element |
	element _ self oldBack.
	self skip: 1.
	^ element
</details>

#### PositionableStream>>#nextDelimited: terminator

Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character. For example: 'this '' was a quote'. Start postioned before the initial terminator.


<details>
	<summary>See more</summary>
	
	nextDelimited: terminator
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character.  For example: 'this '' was a quote'. Start postioned before the initial terminator."

	| out ch |
	out _ WriteStream on: (String new: 1000).
	self atEnd ifTrue: [^ ''].
	self next == terminator ifFalse: [self skip: -1].	"absorb initial terminator"
	[(ch _ self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents
</details>

#### PositionableStream>>#isCharacters

Return true if the receiver is a Character stream


<details>
	<summary>See more</summary>
	
	isCharacters
	"Return true if the receiver is a Character stream"
	^collection class == String
</details>

#### PositionableStream>>#upToAll: aCollection

Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream.


<details>
	<summary>See more</summary>
	
	upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos _ self position.
	(self match: aCollection) 
		ifTrue: [endMatch _ self position.
			self position: startPos.
			result _ self next: endMatch - startPos - aCollection size.
			self position: endMatch.
			^ result]
		ifFalse: [self position: startPos.
			^ self upToEnd]
</details>

#### PositionableStream>>#padToNextLongPut: char

Make position be on long word boundary, writing the padding character, char, if necessary.


<details>
	<summary>See more</summary>
	
	padToNextLongPut: char 
	"Make position be on long word boundary, writing the padding 
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]
</details>

#### PositionableStream>>#backUpTo: subCollection

Back up the position to he subCollection. Position must be somewhere within the stream initially. Leave it just after it. Return true if succeeded. No wildcards, and case does matter.


<details>
	<summary>See more</summary>
	
	backUpTo: subCollection
	"Back up the position to he subCollection.  Position must be somewhere within the stream initially.  Leave it just after it.  Return true if succeeded.  No wildcards, and case does matter."
"Example:
	| strm | strm _ ReadStream on: 'zabc abdc'.
	strm setToEnd; backUpTo: 'abc'; position 
"

	| pattern startMatch |
	pattern _ ReadStream on: subCollection reversed.
	startMatch _ nil.
	[pattern atEnd] whileFalse: 
		[self position = 0 ifTrue: [^ false].
		self skip: -1.
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch _ self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch-1.
						startMatch _ nil]].
		self skip: -1].
	self position: startMatch.
	^ true


</details>

#### PositionableStream>>#nextLine

Answer next line (may be empty), or nil if at end


<details>
	<summary>See more</summary>
	
	nextLine
	"Answer next line (may be empty), or nil if at end"

"	self atEnd ifTrue: [^nil].
	^self upTo: Character crCharacter
"
	^self crLfNextLine
</details>

#### PositionableStream>>#next: anInteger putAll: aCollection startingAt: startIndex

Store the next anInteger elements from the given collection.


<details>
	<summary>See more</summary>
	
	next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	(startIndex = 1 and:[anInteger = aCollection size])
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)
</details>

#### PositionableStream>>#next: n into: aCollection startingAt: startIndex

Read n objects into the given collection. Return aCollection or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do: [ :i |
		(obj _ self next) ifNil: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		aCollection at: startIndex+i put: obj].
	^aCollection
</details>

#### PositionableStream>>#crLfNextLine

Answer next line (may be empty), or nil if at end. Support any line ending convention


<details>
	<summary>See more</summary>
	
	crLfNextLine
	"Answer next line (may be empty), or nil if at end.
	Support any line ending convention"

	| answer lineSeparators c |
	self atEnd ifTrue: [^nil].
	lineSeparators _ {Character cr. Character lf}.
	answer _ self upToAny: lineSeparators.
	c _ self peek.
	c = Character cr ifTrue: [self next. c _ self peek].
	c = Character lf ifTrue: [self next].
	^answer
</details>

#### PositionableStream>>#next: n into: aCollection

Read n objects into the given collection. Return aCollection or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	next: n into: aCollection
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1
</details>

#### PositionableStream>>#skipTo: anObject

Set the access position of the receiver to be past the next occurrence of anObject. Answer whether anObject is found.


<details>
	<summary>See more</summary>
	
	skipTo: anObject 
	"Set the access position of the receiver to be past the next occurrence of 
	anObject. Answer whether anObject is found."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false
</details>

#### PositionableStream>>#backChunk

Answer the contents of the receiver back to the previous terminator character. Doubled terminators indicate an embedded terminator character.


<details>
	<summary>See more</summary>
	
	backChunk
	"Answer the contents of the receiver back to the previous terminator character.  Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator _ $!.
	out _ WriteStream on: (String new: 1000).
	[(ch _ self oldBack) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self oldPeekBack == terminator ifTrue: [
				self oldBack.  "skip doubled terminator"
			] ifFalse: [
				^ out contents reversed  "we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents reversed
</details>

#### PositionableStream>>#untilAnySatisfying: aBlock

<details>
	<summary>See more</summary>
	
	untilAnySatisfying: aBlock
	| write c |
	write := collection class new writeStream.
	[
		self atEnd not and: [
			c := self peek.
			(aBlock value: c) not]]
		whileTrue: [write nextPut: self next].
	^write contents
</details>

#### PositionableStream>>#next: anInteger

Answer the next anInteger elements of my collection. Must override because default uses self contents species, which might involve a large collection.


<details>
	<summary>See more</summary>
	
	next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	| newArray |
	newArray := self collectionSpecies new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray
</details>

#### PositionableStream>>#nextWordsInto: aBitmap

Fill the word based buffer from my collection. Stored on stream as Big Endian. Optimized for speed. Read in BigEndian, then restoreEndianness.


<details>
	<summary>See more</summary>
	
	nextWordsInto: aBitmap 
	"Fill the word based buffer from my collection. 
	Stored on stream as Big Endian. Optimized for speed. 
	Read in BigEndian, then restoreEndianness."
	| blt pos source byteSize |
	collection class isBytes
		ifFalse: [^ self next: aBitmap size into: aBitmap startingAt: 1].

	byteSize := aBitmap byteSize.
	"is the test on collection basicSize \\ 4 necessary?"
	((self position bitAnd: 3) = 0 and: [ (collection basicSize bitAnd: 3) = 0])
		ifTrue: [source := collection.
			pos := self position.
			self skip: byteSize]
		ifFalse: ["forced to copy it into a buffer"
			source := self next: byteSize.
			pos := 0].

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt
				toForm: (Form new hackBits: aBitmap))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	"And do whatever the bitmap needs to do to convert from big-endian order."
	aBitmap restoreEndianness.

	^ aBitmap 	"May be WordArray, ColorArray, etc"

</details>

#### PositionableStream>>#upToAny: aCollection

Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If any of these is not in the collection, answer the entire rest of the receiver.


<details>
	<summary>See more</summary>
	
	upToAny: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any objects in the given collection in the receiver. If 
	any of these is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (self collectionSpecies new: 100).
	[self atEnd or: [aCollection includes: (element := self next)]]
		whileFalse: [newStream nextPut: element].
	(aCollection includes: element)
		ifTrue: [self skip: -1].
	^newStream contents
</details>

#### PositionableStream>>#isBinary

Return true if the receiver is a binary byte stream


<details>
	<summary>See more</summary>
	
	isBinary
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray
</details>

#### PositionableStream>>#on: aCollection

<details>
	<summary>See more</summary>
	
	on: aCollection

	collection _ aCollection.
	readLimit _ aCollection size.
	position _ 0.
	self reset
</details>

#### PositionableStream>>#skipSeparators

<details>
	<summary>See more</summary>
	
	skipSeparators
	[self atEnd]
		whileFalse:
		[self next isSeparator ifFalse: [^ self position: self position-1]]
</details>

#### PositionableStream>>#unCommand

If this read stream is at a <, then skip up to just after the next >. For removing html commands.


<details>
	<summary>See more</summary>
	
	unCommand
	"If this read stream is at a <, then skip up to just after the next >.  For removing html commands."
	| char |
	[self peek = $<] whileTrue: ["begin a block"
		[self atEnd == false and: [self next ~= $>]] whileTrue.
		"absorb characters"
		].
 
</details>

#### PositionableStream>>#nextKeyword

<details>
	<summary>See more</summary>
	
	nextKeyword
	^self
		untilAnySatisfying: [:c | (c isValidInIdentifiers or: [c = $:]) not]
</details>

#### PositionableStream>>#setToEnd

Set the position of the receiver to the end of the sequence of objects.


<details>
	<summary>See more</summary>
	
	setToEnd
	"Set the position of the receiver to the end of the sequence of objects."

	position _ readLimit
</details>

#### PositionableStream>>#copyPreamble: preamble from: aStream at: pos

Look for a changeStamp for this method by peeking backward. Write a method preamble, with that stamp if found.


<details>
	<summary>See more</summary>
	
	copyPreamble: preamble from: aStream at: pos 
	"Look for a changeStamp for this method by peeking backward.
	Write a method preamble, with that stamp if found."
	| terminator last50 stamp i |
	terminator := $!.

	"Look back to find stamp in old preamble, such as...
	Polygon methodsFor: 'private' stamp: 'di 6/25/97 21:42' prior: 34957598! "
	aStream position: pos.
	aStream backChunk.	"to beginning of method"
	last50 := aStream backChunk.	"to get preamble"
	aStream position: pos.
	stamp := String new.
	(i := last50 
		findLastOccurrenceOfString: 'stamp:'
		startingAt: 1) > 0 ifTrue: 
		[ stamp := (last50 
			copyFrom: i + 8
			to: last50 size) copyUpTo: $' ].

	"Write the new preamble, with old stamp if any."
	self
		newLine;
		nextPut: terminator.
	self nextChunkPut: (String streamContents: 
			[ :strm | 
			strm nextPutAll: preamble.
			stamp size > 0 ifTrue: 
				[ strm
					nextPutAll: ' stamp: ';
					print: stamp ] ]).
	self newLine
</details>

#### PositionableStream>>#evaluate: aBlock printingErrorWith: chunk

<details>
	<summary>See more</summary>
	
	evaluate: aBlock printingErrorWith: chunk

	aBlock
		on: Error
		do: [ :ex |
			ex print.
			('while evaluating: ', chunk) print.
			ex pass ]
				
</details>

#### PositionableStream>>#next: anInteger putAll: aCollection

Store the next anInteger elements from the given collection.


<details>
	<summary>See more</summary>
	
	next: anInteger putAll: aCollection
	"Store the next anInteger elements from the given collection."
	^self next: anInteger putAll: aCollection startingAt: 1
</details>

#### PositionableStream>>#isEmpty

Answer whether the receiver's contents has no elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether the receiver's contents has no elements."

	"Returns true if both the set of past and future sequence values of
the receiver are empty. Otherwise returns false"

	^ self atEnd and: [position = 0]
</details>

#### PositionableStream>>#originalContents

Answer the receiver's actual contents collection, NOT a copy. 1/29/96 sw


<details>
	<summary>See more</summary>
	
	originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
</details>

#### PositionableStream>>#compileNextChunkHandlingExceptions

<details>
	<summary>See more</summary>
	
	compileNextChunkHandlingExceptions 

	[ self compileNextChunk ]
		on: InMidstOfFileinNotification, UndeclaredVariableWarning, PoolDefinitionNotification 
		do: [ :ex | ex resume: true ]
</details>

#### PositionableStream>>#isText

Return true if the receiver is a Text stream


<details>
	<summary>See more</summary>
	
	isText
	"Return true if the receiver is a Text stream"
	^collection is: #Text
</details>

#### PositionableStream>>#fileIn

This is special for reading expressions from text that has been formatted with exclamation delimitors. The expressions are read and passed to the Compiler.


<details>
	<summary>See more</summary>
	
	fileIn
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler."

	self fileInAnnouncing: 'Reading ' , self name.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ]
</details>

#### PositionableStream>>#last

Return the final element in the receiver


<details>
	<summary>See more</summary>
	
	last
	"Return the final element in the receiver"

	^ collection at: position
</details>

#### PositionableStream>>#isFileStream

<details>
	<summary>See more</summary>
	
	isFileStream
	^false
</details>

#### PositionableStream>>#contents

Answer with a copy of my collection from 1 to readLimit.


<details>
	<summary>See more</summary>
	
	contents
	"Answer with a copy of my collection from 1 to readLimit."

	^collection copyFrom: 1 to: readLimit
</details>

#### PositionableStream>>#reset

Set the receiver's position to the beginning of the sequence of objects.


<details>
	<summary>See more</summary>
	
	reset
	"Set the receiver's position to the beginning of the sequence of objects."

	position _ 0
</details>

#### PositionableStream>>#back

Go back one element and return it.


<details>
	<summary>See more</summary>
	
	back
	"Go back one element and return it."
	self position = 0 ifTrue: [ self error: 'CantGoBack' ].
	self skip: -1.
	^ self peek
</details>

#### PositionableStream>>#checkForPreamble: chunk

As we don't support preambles and postscripts in Packages, assume any preamble or postscript belongs in the BaseSystem. Note: In packages, replace preamble by prerequisites, and postscript by class initialize methods.


<details>
	<summary>See more</summary>
	
	checkForPreamble: chunk
	"As we don't support preambles and postscripts in Packages, assume any preamble or postscript belongs in the BaseSystem.
	Note: In packages, replace preamble by prerequisites, and postscript by class initialize methods."

	| changeSet newPreamble newPostscript |
	(chunk beginsWith: '"Change Set:')
		ifTrue: [
			changeSet _ ChangeSet changeSetForBaseSystem.
			newPreamble _ changeSet preambleString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPreamble |
					oldPreamble, '.', String newLineString, chunk ].
			changeSet preambleString: newPreamble.
			'Preamble added to ChangeSet ', changeSet name.
			].
	(chunk beginsWith: '"Postscript:')
		ifTrue: [
			changeSet _ ChangeSet changeSetForBaseSystem.
			newPostscript _ changeSet postscriptString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPostscript |
					oldPostscript, '.', String newLineString, chunk ].
			changeSet postscriptString: newPostscript.
			'Postscript added to ChangeSet ', changeSet name.
			].
							

</details>

## RWBinaryOrTextStream

Like FileStream in the sense that I can switch between binary and text, as a FileStream does, without recopying the whole collection. Convert to binary upon input and output. Always keep as text internally.

### Methods
#### RWBinaryOrTextStream>>#nextPut: charOrByte

Insert the argument at the next position in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	nextPut: charOrByte

	super nextPut: charOrByte asCharacter
</details>

#### RWBinaryOrTextStream>>#upToEnd

Must override to get class right.


<details>
	<summary>See more</summary>
	
	upToEnd
	"Must override to get class right."
	| newArray |
	newArray _ (isBinary ifTrue: [ByteArray] ifFalse: [String]) new: self size - self position.
	^ self nextInto: newArray
</details>

#### RWBinaryOrTextStream>>#useCharacters

<details>
	<summary>See more</summary>
	
	useCharacters
	isBinary _ false
</details>

#### RWBinaryOrTextStream>>#next

Return the next object in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	next

	| byte |
	^ isBinary 
			ifTrue: [byte _ super next.
				 byte ifNil: [nil] ifNotNil: [byte numericValue]]
			ifFalse: [super next].

</details>

#### RWBinaryOrTextStream>>#next: anInteger putAll: aCollection startingAt: startIndex

Store the next anInteger elements from the given collection.


<details>
	<summary>See more</summary>
	
	next: anInteger putAll: aCollection startingAt: startIndex
	^super next: anInteger putAll: aCollection asString startingAt: startIndex
</details>

#### RWBinaryOrTextStream>>#next: n into: aCollection startingAt: startIndex

Read n objects into the given collection. Return aCollection or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than n elements have been read."
	"Overriden for efficiency"
	| max |
	max _ (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position _ position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]
</details>

#### RWBinaryOrTextStream>>#useBytes

<details>
	<summary>See more</summary>
	
	useBytes
	isBinary _ true
</details>

#### RWBinaryOrTextStream>>#nextPutAll: aCollection

Append the elements of aCollection to the sequence of objects accessible by the receiver. Answer aCollection.


<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection
	^super nextPutAll: aCollection asString
</details>

#### RWBinaryOrTextStream>>#next: anInteger

Answer the next anInteger elements of my collection. Must override to get class right.


<details>
	<summary>See more</summary>
	
	next: anInteger 
	"Answer the next anInteger elements of my collection. Must override to get class right."

	| newArray |
	newArray _ (isBinary ifTrue: [ByteArray] ifFalse: [String]) new: anInteger.
	^ self nextInto: newArray
</details>

#### RWBinaryOrTextStream>>#contents

Answer with a copy of my collection from 1 to readLimit.


<details>
	<summary>See more</summary>
	
	contents
	"Answer with a copy of my collection from 1 to readLimit."

	| newArray |
	isBinary ifFalse: [^ super contents].	"String"
	readLimit _ readLimit max: position.
	newArray _ ByteArray new: readLimit.
	^ newArray replaceFrom: 1
		to: readLimit
		with: collection
		startingAt: 1.
</details>

#### RWBinaryOrTextStream>>#isBinary

Return true if the receiver is a binary byte stream


<details>
	<summary>See more</summary>
	
	isBinary
	^ isBinary
</details>

#### RWBinaryOrTextStream>>#reset

Set the receiver's position to the beginning of the sequence of objects.


<details>
	<summary>See more</summary>
	
	reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary _ false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection _ collection asString ]
</details>

## ReadStream

I represent an accessor for a sequence of objects that can only read objects from the sequence.

### Methods
#### ReadStream>>#localName

<details>
	<summary>See more</summary>
	
	localName
	^'ReadStream'
</details>

#### ReadStream>>#nextPut: anObject

Insert the argument, anObject, as the next object accessible by the receiver. Answer anObject.


<details>
	<summary>See more</summary>
	
	nextPut: anObject

	self shouldNotImplement
</details>

#### ReadStream>>#upToEnd

Answer a subcollection from the current access position through the last element of the receiver.


<details>
	<summary>See more</summary>
	
	upToEnd
	| start |

	start _ position+1.
	position _ collection size.
	^collection copyFrom: start to: position
</details>

#### ReadStream>>#readInto: byteArray startingAt: startIndex count: count

Read n objects into the given collection. Return aCollection or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	readInto: byteArray startingAt: startIndex count: count
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max _ (readLimit - position) min: count.
	byteArray 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position _ position + max.
	^max
</details>

#### ReadStream>>#size

Compatibility with other streams (e.g., FileStream)


<details>
	<summary>See more</summary>
	
	size
	"Compatibility with other streams (e.g., FileStream)"
	^readLimit
</details>

#### ReadStream>>#next: anInteger

Answer the next anInteger elements of my collection. overriden for efficiency


<details>
	<summary>See more</summary>
	
	next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition _ position + anInteger  min:  readLimit.
	ans _ collection copyFrom: position+1 to: endPosition.
	position _ endPosition.
	^ans

</details>

#### ReadStream>>#isReadOnly

To be redefined in subclasses that can't do #nextPut:


<details>
	<summary>See more</summary>
	
	isReadOnly
	^ true
</details>

#### ReadStream>>#next

Answer the next object in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	next
	"Answer the next object in the Stream represented by the receiver."

	^position >= readLimit
		ifFalse: [collection at: (position _ position + 1)]
</details>

#### ReadStream>>#readStream

polymorphic with SequenceableCollection. Return self


<details>
	<summary>See more</summary>
	
	readStream
	"polymorphic with SequenceableCollection.  Return self"

	^ self
</details>

#### ReadStream>>#upTo: anObject

fast version using indexOf:


<details>
	<summary>See more</summary>
	
	upTo: anObject
	"fast version using indexOf:"
	| start end |

	start _ position+1.
	end _ collection indexOf: anObject startingAt: start ifAbsent: [ 0 ].

	"not present--return rest of the collection"	
	end = 0 ifTrue: [ ^self upToEnd ].

	"skip to the end and return the data passed over"
	position _ end.
	^collection copyFrom: start to: (end-1)
</details>

#### ReadStream>>#on: aCollection from: firstIndex to: lastIndex

<details>
	<summary>See more</summary>
	
	on: aCollection from: firstIndex to: lastIndex

	| len |
	collection _ aCollection.
	readLimit _  lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]
</details>

#### ReadStream>>#next: n into: aCollection startingAt: startIndex

Read n objects into the given collection. Return aCollection or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max _ (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position _ position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]
</details>

## ReadWriteStream

I represent an accessor for a sequence of objects. My instances can both read and store objects.

### Methods
#### ReadWriteStream>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash

	self class == ReadWriteStream ifFalse: [^ super hash].
	^ (self position + readLimit + 53) hash
</details>

#### ReadWriteStream>>#fileNameEndsWith: aString

See comment in FileStream fileNameEndsWith:


<details>
	<summary>See more</summary>
	
	fileNameEndsWith: aString
	"See comment in FileStream fileNameEndsWith:"

	^false
</details>

#### ReadWriteStream>>#truncateAtPosition

Truncate the receiver at current position. For example, this should evaluate to true: | s | s _ ReadWriteStream on: Array new. s nextPutAll: 'abcdefg'. s reset. s next; next. s nextPut: $z. s truncateAtPosition. s atEnd


<details>
	<summary>See more</summary>
	
	truncateAtPosition
	"Truncate the receiver at current position.
	For example, this should evaluate to true:
		| s |
		s _ ReadWriteStream on: Array new.
		s nextPutAll: 'abcdefg'.
		s reset.
		s next; next.
		s nextPut: $z.
		s truncateAtPosition.
		s atEnd
	"
	readLimit _ position
</details>

#### ReadWriteStream>>#next: anInteger

Answer the next anInteger elements of my collection. overriden for efficiency


<details>
	<summary>See more</summary>
	
	next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |
	readLimit := readLimit max: position.

	endPosition _ position + anInteger  min:  readLimit.
	ans _ collection copyFrom: position+1 to: endPosition.
	position _ endPosition.
	^ans

</details>

#### ReadWriteStream>>#next

Return the next object in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	next
	"Return the next object in the Stream represented by the receiver."

	"treat me as a FIFO"
	^ position >= readLimit
		ifFalse: [collection at: (position _ position + 1)]
</details>

#### ReadWriteStream>>#readStream

polymorphic with SequenceableCollection. Return self


<details>
	<summary>See more</summary>
	
	readStream
	"polymorphic with SequenceableCollection.  Return self"

	^ self
</details>

#### ReadWriteStream>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^ 'a stream'   "for fileIn compatibility"
</details>

#### ReadWriteStream>>#contents

Answer with a copy of my collection from 1 to readLimit.


<details>
	<summary>See more</summary>
	
	contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: readLimit
</details>

#### ReadWriteStream>>#= other

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= other

	"Any object is equal to itself"
	self == other ifTrue: [ ^ true ].

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ false ].

	^ self position = other position and: [ self contents = other contents ]
</details>

#### ReadWriteStream>>#fileOutObject: theObject

Write a file that has both the source code for the named class and an object as bits. Any instance-specific object will get its class written automatically.


<details>
	<summary>See more</summary>
	
	fileOutObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| srefStream |
	self timeStamp.

	"Append the object's raw data"
	srefStream _ SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"

</details>

## Stream

I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my "contents".

### Methods
#### Stream>>#next: anInteger put: anObject

Make anObject be the next anInteger number of objects accessible by the receiver. Answer anObject.


<details>
	<summary>See more</summary>
	
	next: anInteger put: anObject 
	"Make anObject be the next anInteger number of objects accessible by the 
	receiver. Answer anObject."

	anInteger timesRepeat: [self nextPut: anObject].
	^anObject
</details>

#### Stream>>#localName

<details>
	<summary>See more</summary>
	
	localName
	^'a stream'
</details>

#### Stream>>#nextSignedInt16Put: aNumber bigEndian: bigEndian

Store the given number as a signed, 16-bit integer on this (binary) stream. (16r10000-12345) hex '16rCFC7' (ByteArray streamContents: [ :strm | strm nextSignedInt16Put: -12345 bigEndian: false ]) hex (ByteArray streamContents: [ :strm | strm nextSignedInt16Put: -12345 bigEndian: true ]) hex


<details>
	<summary>See more</summary>
	
	nextSignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 16-bit integer on this (binary) stream.

	(16r10000-12345) hex '16rCFC7'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes shortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#nextUnsignedInt16BigEndian: bigEndian

Answer the next unsigned, 16-bit integer from this (binary) stream. (ByteArray streamContents: [ :strm | strm nextUnsignedInt16Put: 12345 bigEndian: false ]) readStream nextUnsignedInt16BigEndian: false (ByteArray streamContents: [ :strm | strm nextUnsignedInt16Put: 12345 bigEndian: true ]) readStream nextUnsignedInt16BigEndian: true


<details>
	<summary>See more</summary>
	
	nextUnsignedInt16BigEndian: bigEndian
	"Answer the next unsigned, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) 
			readStream nextUnsignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) 
			readStream nextUnsignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes unsignedShortAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#binary

Compatibility. If possible, store and retrieve bytes.


<details>
	<summary>See more</summary>
	
	binary
	"Compatibility. If possible, store and retrieve bytes."
	self useBytes
</details>

#### Stream>>#atEnd

Answer whether the receiver can access any more objects.


<details>
	<summary>See more</summary>
	
	atEnd
	"Answer whether the receiver can access any more objects."

	self subclassResponsibility
</details>

#### Stream>>#nextUnsignedInt32BigEndian: bigEndian

Answer the next unsigned, 32-bit integer from this (binary) stream. (ByteArray streamContents: [ :strm | strm nextUnsignedInt32Put: 123456 bigEndian: false ]) readStream nextUnsignedInt32BigEndian: false (ByteArray streamContents: [ :strm | strm nextUnsignedInt32Put: 123456 bigEndian: true ]) readStream nextUnsignedInt32BigEndian: true


<details>
	<summary>See more</summary>
	
	nextUnsignedInt32BigEndian: bigEndian
	"Answer the next unsigned, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) 
			readStream nextUnsignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) 
			readStream nextUnsignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes unsignedLongAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#openReadOnly

<details>
	<summary>See more</summary>
	
	openReadOnly
	^self
</details>

#### Stream>>#nextSignedInt16BigEndian: bigEndian

Answer the next signed, 16-bit integer from this (binary) stream. (ByteArray streamContents: [ :strm | strm nextSignedInt16Put: -12345 bigEndian: false ]) readStream nextSignedInt16BigEndian: false (ByteArray streamContents: [ :strm | strm nextSignedInt16Put: -12345 bigEndian: true ]) readStream nextSignedInt16BigEndian: true


<details>
	<summary>See more</summary>
	
	nextSignedInt16BigEndian: bigEndian
	"Answer the next  signed, 16-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: false ]) 
			readStream nextSignedInt16BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt16Put: -12345 bigEndian: true ]) 
			readStream nextSignedInt16BigEndian: true
	"
	| bytes |
	bytes _ self next: 2.
	^ bytes shortAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#nextNumber

Answer a number from the (text) stream.


<details>
	<summary>See more</summary>
	
	nextNumber
	"Answer a number from the (text) stream."

	|element|
	[(element := self next) isNil or: [element isDigit or: [element = $- or: [element = $)]]]] whileFalse.
	element ifNil: [^nil].
	self skip: -1.
	element = $) ifTrue: [^nil].
	^Number readFrom: self
</details>

#### Stream>>#closed

Answer true if we have been closed and are no longer usable. Meaningful, for example, for file streams.


<details>
	<summary>See more</summary>
	
	closed
	"Answer true if we have been closed and are no longer usable.
	Meaningful, for example, for file streams."
	^ false
</details>

#### Stream>>#nextDouble64Put: aFloat bigEndian: bigEndian

Store the given number as a 32 bit Float on this (binary) stream. Float pi hex '400921FB54442D18' Float pi negated hex 'C00921FB54442D18' (ByteArray streamContents: [ :strm | strm nextDouble64Put: Float pi bigEndian: false ]) hex (ByteArray streamContents: [ :strm | strm nextDouble64Put: Float pi bigEndian: true ]) hex


<details>
	<summary>See more</summary>
	
	nextDouble64Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 8.
	bytes doubleAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#printOn: stream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: stream

	super printOn: stream.
	stream space.
	self contents printOn: stream.

</details>

#### Stream>>#next: anInteger

Answer the next anInteger number of objects accessible by the receiver.


<details>
	<summary>See more</summary>
	
	next: anInteger 
	"Answer the next anInteger number of objects accessible by the receiver."

	| aCollection |
	aCollection _ OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection
</details>

#### Stream>>#do: aBlock

Evaluate aBlock for each of the objects accessible by receiver.


<details>
	<summary>See more</summary>
	
	do: aBlock 
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]
</details>

#### Stream>>#isReadOnly

To be redefined in subclasses that can't do #nextPut:


<details>
	<summary>See more</summary>
	
	isReadOnly
	"To be redefined in subclasses that can't do #nextPut:"
	^ false
</details>

#### Stream>>#nextFloat32Put: aFloat bigEndian: bigEndian

Store the given number as a 32 bit Float on this (binary) stream. Float pi hex '400921FB54442D18' Float pi negated hex 'C00921FB54442D18' Float pi asIEEE32BitWord hex '16r40490FDB' Float pi negated asIEEE32BitWord hex '16rC0490FDB' (ByteArray streamContents: [ :strm | strm nextFloat32Put: Float pi bigEndian: false ]) hex 'DB0F4940' (ByteArray streamContents: [ :strm | strm nextFloat32Put: Float pi bigEndian: true ]) hex '40490FDB'


<details>
	<summary>See more</summary>
	
	nextFloat32Put: aFloat bigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ]) hex 'DB0F4940'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ]) hex '40490FDB'
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes floatAt: 1 put: aFloat bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#nextSignedInt32Put: aNumber bigEndian: bigEndian

Store the given number as a signed, 32-bit integer on this (binary) stream. (16r100000000-123456) hex '16rFFFE1DC0' (ByteArray streamContents: [ :strm | strm nextSignedInt32Put: -123456 bigEndian: false ]) hex (ByteArray streamContents: [ :strm | strm nextSignedInt32Put: -123456 bigEndian: true ]) hex


<details>
	<summary>See more</summary>
	
	nextSignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a signed, 32-bit integer on this (binary) stream.

	(16r100000000-123456) hex '16rFFFE1DC0'

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes longAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#basicNext

<details>
	<summary>See more</summary>
	
	basicNext

	^ self next
</details>

#### Stream>>#nextUnsignedInt16Put: aNumber bigEndian: bigEndian

Store the given number as a unsigned, 16-bit integer on this (binary) stream. 12345 hex '16r3039' (ByteArray streamContents: [ :strm | strm nextUnsignedInt16Put: 12345 bigEndian: false ]) hex (ByteArray streamContents: [ :strm | strm nextUnsignedInt16Put: 12345 bigEndian: true ]) hex


<details>
	<summary>See more</summary>
	
	nextUnsignedInt16Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 16-bit integer on this (binary) stream.

	12345 hex  '16r3039'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt16Put: 12345 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 2.
	bytes unsignedShortAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#nextPut: anObject

Insert the argument, anObject, as the next object accessible by the receiver. Answer anObject.


<details>
	<summary>See more</summary>
	
	nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	self subclassResponsibility
</details>

#### Stream>>#nextDouble64BigEndian: bigEndian

Store the given number as a 32 bit Float on this (binary) stream. Float pi hex '400921FB54442D18' Float pi negated hex 'C00921FB54442D18' (ByteArray streamContents: [ :strm | strm nextDouble64Put: Float pi bigEndian: false ]) readStream nextDouble64BigEndian: false (ByteArray streamContents: [ :strm | strm nextDouble64Put: Float pi bigEndian: true ]) readStream nextDouble64BigEndian: true


<details>
	<summary>See more</summary>
	
	nextDouble64BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: false ])
			readStream nextDouble64BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextDouble64Put: Float pi bigEndian: true ])
			readStream nextDouble64BigEndian: true
	"
	| bytes |
	bytes _ self next: 8.
	^ bytes doubleAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#ascii

Compatibility. If possible, store and retrieve characters.


<details>
	<summary>See more</summary>
	
	ascii
	"Compatibility. If possible, store and retrieve characters."
	self useCharacters
</details>

#### Stream>>#nextMatchAll: aColl

Answer true if next N objects are the ones in aColl, else false. Advance stream of true, leave as was if false.


<details>
	<summary>See more</summary>
	
	nextMatchAll: aColl
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save _ self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true
</details>

#### Stream>>#upToEnd

answer the remaining elements in the string


<details>
	<summary>See more</summary>
	
	upToEnd
	"answer the remaining elements in the string"
	| elements |
	elements _ OrderedCollection new.
	[ self atEnd ] whileFalse: [ 
		elements add: self next ].
	^elements
</details>

#### Stream>>#nextUnsignedInt32Put: aNumber bigEndian: bigEndian

Store the given number as a unsigned, 32-bit integer on this (binary) stream. 123456 hex '16r1E240' (ByteArray streamContents: [ :strm | strm nextUnsignedInt32Put: 123456 bigEndian: false ]) hex (ByteArray streamContents: [ :strm | strm nextUnsignedInt32Put: 123456 bigEndian: true ]) hex


<details>
	<summary>See more</summary>
	
	nextUnsignedInt32Put: aNumber bigEndian: bigEndian
	"Store the given number as a unsigned, 32-bit integer on this (binary) stream.

	123456 hex '16r1E240'

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: false ]) hex

	(ByteArray streamContents: [ :strm |
		strm nextUnsignedInt32Put: 123456 bigEndian: true ]) hex
	"
	| bytes |
	bytes _ ByteArray new: 4.
	bytes unsignedLongAt: 1 put: aNumber bigEndian: bigEndian.
	self nextPutAll: bytes
</details>

#### Stream>>#is: aSymbol

Return true if the receiver responds to the stream protocol (if that's what's asked)


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Return true if the receiver responds to the stream protocol (if that's what's asked)"
	^ aSymbol == #Stream or: [ super is: aSymbol ]
</details>

#### Stream>>#print: anObject when: aCondition

<details>
	<summary>See more</summary>
	
	print: anObject when: aCondition 

	aCondition ifTrue: [self print: anObject].
</details>

#### Stream>>#close

Only some specific kinds of Streams such as FileStream need this


<details>
	<summary>See more</summary>
	
	close
	"Only some specific kinds of Streams such as FileStream need this"
</details>

#### Stream>>#next

Answer the next object accessible by the receiver.


<details>
	<summary>See more</summary>
	
	next
	"Answer the next object accessible by the receiver."

	self subclassResponsibility
</details>

#### Stream>>#nextFloat32BigEndian: bigEndian

Store the given number as a 32 bit Float on this (binary) stream. Float pi hex '400921FB54442D18' Float pi negated hex 'C00921FB54442D18' Float pi asIEEE32BitWord hex '16r40490FDB' Float pi negated asIEEE32BitWord hex '16rC0490FDB' (ByteArray streamContents: [ :strm | strm nextFloat32Put: Float pi bigEndian: false ]) readStream nextFloat32BigEndian: false (ByteArray streamContents: [ :strm | strm nextFloat32Put: Float pi bigEndian: true ]) readStream nextFloat32BigEndian: true


<details>
	<summary>See more</summary>
	
	nextFloat32BigEndian: bigEndian
	"Store the given number as a 32 bit Float on this (binary) stream.

	Float pi hex '400921FB54442D18'
	Float pi negated hex 'C00921FB54442D18'
	Float pi asIEEE32BitWord hex '16r40490FDB'
	Float pi negated asIEEE32BitWord hex '16rC0490FDB'

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: false ])
			readStream nextFloat32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextFloat32Put: Float pi bigEndian: true ])
			readStream nextFloat32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes floatAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#nextPutAll: aCollection when: aCondition

<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection when: aCondition

	aCondition ifTrue: [ self nextPutAll: aCollection ].
	
</details>

#### Stream>>#nextPutAll: aCollection asCommaSeparated: aPrintBlock

<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection asCommaSeparated: aPrintBlock

	aCollection asCommaSeparated: aPrintBlock on: self
</details>

#### Stream>>#nextSignedInt32BigEndian: bigEndian

Answer the next signed, 32-bit integer from this (binary) stream. (ByteArray streamContents: [ :strm | strm nextSignedInt32Put: -123456 bigEndian: false ]) readStream nextSignedInt32BigEndian: false (ByteArray streamContents: [ :strm | strm nextSignedInt32Put: -123456 bigEndian: true ]) readStream nextSignedInt32BigEndian: true


<details>
	<summary>See more</summary>
	
	nextSignedInt32BigEndian: bigEndian
	"Answer the next signed, 32-bit integer from this (binary) stream.

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: false ]) 
			readStream nextSignedInt32BigEndian: false

	(ByteArray streamContents: [ :strm |
		strm nextSignedInt32Put: -123456 bigEndian: true ]) 
			readStream nextSignedInt32BigEndian: true
	"
	| bytes |
	bytes _ self next: 4.
	^ bytes longAt: 1 bigEndian: bigEndian
</details>

#### Stream>>#print: anObject

<details>
	<summary>See more</summary>
	
	print: anObject
	anObject printOn: self
</details>

#### Stream>>#nextPutAll: aCollection

Append the elements of aCollection to the sequence of objects accessible by the receiver. Answer aCollection.


<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection
</details>

#### Stream>>#nextString

Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length. I expect to be in ascii mode when called (caller puts back to binary).


<details>
	<summary>See more</summary>
	
	nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| aString length |

	"read the length in binary mode"
	self binary.
	length _ self next.		"first byte."
	length >= 192 ifTrue: [length _ length - 192.
		1 to: 3 do: [:ii | length _ length * 256 + self next]].
	aString _ String new: length.

	"read the characters in ASCII mode"
	self ascii.
	self nextInto: aString.
	^aString
</details>

#### Stream>>#nextWordsPutAll: aCollection

Write the argument a word-like object in big endian format on the receiver. May be used to write other than plain word-like objects (such as ColorArray).


<details>
	<summary>See more</summary>
	
	nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextUnsignedInt32Put: (aCollection basicAt: i) bigEndian: true ].
	^aCollection
</details>

#### Stream>>#readOnly

<details>
	<summary>See more</summary>
	
	readOnly
	^self
</details>

#### Stream>>#contents

Answer all of the contents of the receiver.


<details>
	<summary>See more</summary>
	
	contents
	"Answer all of the contents of the receiver."

	self subclassResponsibility
</details>

#### Stream>>#nextStringPut: s

Append the string, s, to the receiver. Only used by DataStream. Max size of 64*256*256*256.


<details>
	<summary>See more</summary>
	
	nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length _ s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s
</details>

#### Stream>>#flush

Do nothing by default


<details>
	<summary>See more</summary>
	
	flush
	"Do nothing by default"
</details>

#### Stream>>#nextPut: anObject when: aCondition

<details>
	<summary>See more</summary>
	
	nextPut: anObject when: aCondition 

	aCondition ifTrue: [ self nextPut: anObject ].
</details>

## Transcripter

Transcripter is a dog-simple scrolling stream with display. It is intended to operate with no support from MVC or color in a minimal, or headless version of Squeak. No attention has been paid to appearance or performance.

### Methods
#### Transcripter>>#endEntry

<details>
	<summary>See more</summary>
	
	endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			`0@0` + (0@(d+AbstractFont default lineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (`4@4` + frame topLeft extent: Display extent)
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen screenUpdateRequired: nil
</details>

#### Transcripter>>#white

<details>
	<summary>See more</summary>
	
	white
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ `Color white`
</details>

#### Transcripter>>#show: anObject

<details>
	<summary>See more</summary>
	
	show: anObject
	self nextPutAll: anObject asString; endEntry
</details>

#### Transcripter>>#initInFrame: rect

<details>
	<summary>See more</summary>
	
	initInFrame: rect
	frame _ rect insetBy: 2.  "Leave room for border"
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: self contents asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll
</details>

#### Transcripter>>#request: prompt

<details>
	<summary>See more</summary>
	
	request: prompt
	| startPos char contents return |
	return _ Character numericValue: InputSensor returnKey.
	self
		newLine;
		show: prompt.
	startPos _ position.
	[
		[ Sensor keyboardPressed ] whileFalse.
		(char _ Sensor keyboard) = return ] whileFalse: [
			char = Character backspace
				ifTrue: [ readLimit _ position _ position - 1 max: startPos ]
				ifFalse: [ self nextPut: char ].
			self endEntry ].
	contents _ self contents.
	^ contents
		copyFrom: startPos + 1
		to: contents size
</details>

#### Transcripter>>#confirm: queryString

Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no.


<details>
	<summary>See more</summary>
	
	confirm: queryString 
	| choice |
	[true]
		whileTrue: 
			[choice _ self request: queryString , '
Please type yes or no followed by return'.
			choice first asUppercase = $Y ifTrue: [^ true].
			choice first asUppercase = $N ifTrue: [^ false]]
</details>

#### Transcripter>>#black

<details>
	<summary>See more</summary>
	
	black
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ `Color black`
</details>

#### Transcripter>>#readEvalPrint

<details>
	<summary>See more</summary>
	
	readEvalPrint
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line _ self request: '>') ] whileFalse: [
		self
			newLine;
			show:
				([ Compiler evaluate: line ] ifError: [ :err :ex |
					err ]) ]
</details>

#### Transcripter>>#clear

<details>
	<summary>See more</summary>
	
	clear
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	self on: (String new: 100); endEntry
</details>

## WriteStream

I represent an accessor for a sequence of objects that can only store objects in the sequence.

### Methods
#### WriteStream>>#tabWhen: aCondition

<details>
	<summary>See more</summary>
	
	tabWhen: aCondition

	aCondition ifTrue: [ self tab ]
</details>

#### WriteStream>>#spaceWhen: aCondition

<details>
	<summary>See more</summary>
	
	spaceWhen: aCondition
 
	aCondition ifTrue: [ self space ]
</details>

#### WriteStream>>#newLineTab: anInteger

Append a newLine character, followed by anInteger tab characters, to the receiver.


<details>
	<summary>See more</summary>
	
	newLineTab: anInteger
	"Append a newLine character, followed by anInteger tab characters, to the receiver."

	self nextPut: Character newLineCharacter.
	anInteger timesRepeat: [self nextPut: Character tab]
</details>

#### WriteStream>>#position: anInteger

Refer to the comment in PositionableStream|position:.


<details>
	<summary>See more</summary>
	
	position: anInteger 
	"Refer to the comment in PositionableStream|position:."

	readLimit _ readLimit max: position.
	super position: anInteger
</details>

#### WriteStream>>#next: anInteger putAll: aCollection startingAt: startIndex

Store the next anInteger elements from the given collection.


<details>
	<summary>See more</summary>
	
	next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."


	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex].

	newEnd _ position + anInteger.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: startIndex.
	position _ newEnd.

	^aCollection
</details>

#### WriteStream>>#store: anObject

Have anObject print on the receiver for purposes of rereading.


<details>
	<summary>See more</summary>
	
	store: anObject 
	"Have anObject print on the receiver for purposes of rereading."

	anObject storeOn: self
</details>

#### WriteStream>>#on: aCollection from: firstIndex to: lastIndex

<details>
	<summary>See more</summary>
	
	on: aCollection from: firstIndex to: lastIndex

	| len |
	collection _ aCollection.
	readLimit _ 
		writeLimit _ lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]
</details>

#### WriteStream>>#nextChunkPut: aString

Append the argument, aString, to the receiver, doubling embedded terminators.


<details>
	<summary>See more</summary>
	
	nextChunkPut: aString
	"Append the argument, aString, to the receiver, doubling embedded terminators."

	| i remainder terminator |
	terminator _ $!.
	remainder _ aString.
	[(i _ remainder indexOf: terminator) = 0] whileFalse:
		[self nextPutAll: (remainder copyFrom: 1 to: i).
		self nextPut: terminator.  "double imbedded terminators"
		remainder _ remainder copyFrom: i+1 to: remainder size].
	self nextPutAll: remainder; nextPut: terminator
</details>

#### WriteStream>>#ensureNoSpace

If there is not one on the end, remove it.


<details>
	<summary>See more</summary>
	
	ensureNoSpace
	"If there is not one on the end, remove it."

	(position > 0 and: [(collection at: position) = Character space]) 
		ifTrue: [self skip: -1].
</details>

#### WriteStream>>#cr

Append a cr character to the receiver. Use this method when you specifically need a cr character. In many cases, it is advisable to call #newLine


<details>
	<summary>See more</summary>
	
	cr
	"Append a cr character to the receiver.
	Use this method when you specifically need a cr character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character cr
</details>

#### WriteStream>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size

	^readLimit _ readLimit max: position
</details>

#### WriteStream>>#timeStamp

Append the current time to the receiver as a String.


<details>
	<summary>See more</summary>
	
	timeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !s"
		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.
	self newLine
</details>

#### WriteStream>>#tab: times when: aCondition

<details>
	<summary>See more</summary>
	
	tab: times when: aCondition

	aCondition ifTrue: [ self tab: times ]
	
	
</details>

#### WriteStream>>#withAttributes: attributes do: streamBlock

<details>
	<summary>See more</summary>
	
	withAttributes: attributes do: streamBlock 
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 _ self position.
	val _ streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val
</details>

#### WriteStream>>#newLine

Append a newLine character to the receiver. The Cuis convention is to use lf on output.


<details>
	<summary>See more</summary>
	
	newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter
</details>

#### WriteStream>>#on: aCollection

<details>
	<summary>See more</summary>
	
	on: aCollection

	super on: aCollection.
	readLimit _ 0.
	writeLimit _ aCollection size
</details>

#### WriteStream>>#with: aCollection

<details>
	<summary>See more</summary>
	
	with: aCollection

	super on: aCollection.
	position _ readLimit _ writeLimit _ aCollection size
</details>

#### WriteStream>>#pastEndPut: anObject

Grow the collection. Then we put <anObject> at the current write position.


<details>
	<summary>See more</summary>
	
	pastEndPut: anObject
	"Grow the collection.
	Then we put <anObject> at the current write position."

	self growTo: collection size + 1.
	collection at: (position _ position + 1) put: anObject
</details>

#### WriteStream>>#tab

Append a tab character to the receiver.


<details>
	<summary>See more</summary>
	
	tab
	"Append a tab character to the receiver."

	self nextPut: Character tab
</details>

#### WriteStream>>#nextPut: anObject

Insert the argument at the next position in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	nextPut: anObject 
	"Insert the argument at the next position in the Stream represented by the receiver."

	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: [
			position _ position + 1.
			^collection at: position put: anObject]
</details>

#### WriteStream>>#growTo: anInteger

Grow the collection by creating a new bigger collection and then copy over the contents from the old one. We grow by doubling the size. anInteger is the required minimal new size of the collection


<details>
	<summary>See more</summary>
	
	growTo: anInteger
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize grownCollection newSize |
	oldSize _ collection size.
     newSize _ anInteger + (oldSize max: 20).
	grownCollection _ collection class ofSize: newSize.
	collection _ grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit _ collection size
</details>

#### WriteStream>>#newLineTab: times when: aCondition

<details>
	<summary>See more</summary>
	
	newLineTab: times when: aCondition

	aCondition ifTrue: [ self newLineTab: times ]

	
</details>

#### WriteStream>>#peekLast

Return that item just put at the end of the stream


<details>
	<summary>See more</summary>
	
	peekLast
	"Return that item just put at the end of the stream"

	^ position > 0 
		ifTrue: [collection at: position]
		ifFalse: [nil]
</details>

#### WriteStream>>#braceArray

This method is used in compilation of brace constructs. It MUST NOT be deleted or altered.


<details>
	<summary>See more</summary>
	
	braceArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ collection
</details>

#### WriteStream>>#next

Answer the next object accessible by the receiver.


<details>
	<summary>See more</summary>
	
	next

	self shouldNotImplement
</details>

#### WriteStream>>#setToEnd

Refer to the comment in PositionableStream|setToEnd.


<details>
	<summary>See more</summary>
	
	setToEnd 
	"Refer to the comment in PositionableStream|setToEnd."

	readLimit _ readLimit max: position.
	super setToEnd.
</details>

#### WriteStream>>#space: anInteger

Append anInteger space characters to the receiver.


<details>
	<summary>See more</summary>
	
	space: anInteger 
	"Append anInteger space characters to the receiver."

	anInteger timesRepeat: [self space]
</details>

#### WriteStream>>#isCompatibleWithContents: aCollection

<details>
	<summary>See more</summary>
	
	isCompatibleWithContents: aCollection

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isString and: [ collection is: #Text])
		ifTrue: [ ^ true ].

	^ false
</details>

#### WriteStream>>#nextPutAllString: aString withAttributes: attributesArray

<details>
	<summary>See more</summary>
	
	nextPutAllString: aString withAttributes: attributesArray

	| newEnd |
	(self isCompatibleWithContents: aString)
		ifFalse: [ ^ self nextPutAll: aString ].

	newEnd _ position + aString size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: aString
		attributes: attributesArray
		startingAt: 1.
	position _ newEnd
</details>

#### WriteStream>>#newLineWhen: aCondition

<details>
	<summary>See more</summary>
	
	newLineWhen: aCondition

	aCondition ifTrue: [ self newLine ]
</details>

#### WriteStream>>#isEmpty

Answer whether the receiver's contents has no elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether the receiver's contents has no elements."

	^position = 0
</details>

#### WriteStream>>#padToEndIfCantTruncate

Only makes sense for file streams with existing content. See inheritance


<details>
	<summary>See more</summary>
	
	padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	See inheritance"
</details>

#### WriteStream>>#nextPutAll: aCollection

Append the elements of aCollection to the sequence of objects accessible by the receiver. Answer aCollection.


<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection

	| newEnd |
	(self isCompatibleWithContents: aCollection)
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.
</details>

#### WriteStream>>#tab: anInteger

Append anInteger tab characters to the receiver.


<details>
	<summary>See more</summary>
	
	tab: anInteger 
	"Append anInteger tab characters to the receiver."

	anInteger timesRepeat: [self tab]
</details>

#### WriteStream>>#lf

Append a lf character to the receiver. Use this method when you specifically need a lf character. In many cases, it is advisable to call #newLine


<details>
	<summary>See more</summary>
	
	lf
	"Append a lf character to the receiver.
	Use this method when you specifically need a lf character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character lf
</details>

#### WriteStream>>#resetToStart

<details>
	<summary>See more</summary>
	
	resetToStart
	readLimit _ position _ 0.
</details>

#### WriteStream>>#braceArray: anArray

This method is used in compilation of brace constructs. It MUST NOT be deleted or altered.


<details>
	<summary>See more</summary>
	
	braceArray: anArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection _ anArray.
	position _ 0.
	readLimit _ 0.
	writeLimit _ anArray size.
</details>

#### WriteStream>>#contents

Answer with a copy of my collection from 1 to readLimit.


<details>
	<summary>See more</summary>
	
	contents

	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: position
</details>

#### WriteStream>>#reset

Refer to the comment in PositionableStream|reset.


<details>
	<summary>See more</summary>
	
	reset 
	"Refer to the comment in PositionableStream|reset."

	readLimit _ readLimit max: position.
	position _ 0
</details>

#### WriteStream>>#ensureASpace

Append a space character to the receiver IFF there is not one on the end.


<details>
	<summary>See more</summary>
	
	ensureASpace
	"Append a space character to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = Character space]) ifTrue: [^self].
	self nextPut: Character space
</details>

#### WriteStream>>#withAttribute: aTextAttribute do: streamBlock

<details>
	<summary>See more</summary>
	
	withAttribute: aTextAttribute do: streamBlock
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 _ self position.
	val _ streamBlock value.
	collection addAttribute: aTextAttribute from: pos1+1 to: self position.
	^ val
</details>

#### WriteStream>>#space: times when: aCondition

<details>
	<summary>See more</summary>
	
	space: times when: aCondition

	aCondition ifTrue: [ self space: times ]
	
	
</details>

#### WriteStream>>#nextPutKeyword: keyword withArg: argValue

Emit a keyword/value pair in the alternate syntax


<details>
	<summary>See more</summary>
	
	nextPutKeyword: keyword withArg: argValue
	"Emit a keyword/value pair in the alternate syntax"

	self nextPutAll: (keyword copyWithout: $:);
		nextPut: $(;
		store: argValue;
		nextPut: $)
</details>

#### WriteStream>>#space

Append a space character to the receiver.


<details>
	<summary>See more</summary>
	
	space
	"Append a space character to the receiver."

	self nextPut: Character space
</details>

