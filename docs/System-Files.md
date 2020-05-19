## CuisSourceFileArray

Cuis source code access mechanism. Uses the range for sourcePointer in CompiledMethods (16r1000000 to 16r4FFFFFF) quite differently than StandardSourceFileArray (the older way, inherited from Squeak). First half is for Sources, second half is for Changes. The actual offset in the file is the sourcePointer minus 16r1000000 (or minus 16r3000000) multiplied by a scaling factor. This scaling factor is (right now) 32, raising the limit to 1Gb. See the class comment at MigratingSourceFileArray to see how to activate this.

### Methods
#### CuisSourceFileArray>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	files _ Array new: 2.
	files at: 1 put: (SourceFiles at: 1).
	files at: 2 put: (SourceFiles at: 2).
	pointerScale _ 32	"To change the pointerScale (and therefore the size limit for Sources and Changes, change this value, and use a MigratingSourceFileArray to create the new Sources and Changes files. See class comment there."
</details>

#### CuisSourceFileArray>>#fileIndexFromSourcePointer: anInteger

Return the index of the source file which contains the source chunk addressed by anInteger


<details>
	<summary>See more</summary>
	
	fileIndexFromSourcePointer: anInteger
	"Return the index of the source file which contains the source chunk addressed by anInteger"
	"No source pointer => no fileIndex"
	anInteger = 0 ifTrue: [ ^0 ].
	^(anInteger - 16r1000000 anyMask: 16r2000000)
		ifTrue: [2]
		ifFalse: [1]
</details>

#### CuisSourceFileArray>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size
	^files size
</details>

#### CuisSourceFileArray>>#at: index put: aFile

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: aFile
	files at: index put: aFile
</details>

#### CuisSourceFileArray>>#checkOKToAdd: size at: filePosition in: fileIndex

Issue several warnings as the end of the changes file approaches its limit, and finally halt with an error when the end is reached.


<details>
	<summary>See more</summary>
	
	checkOKToAdd: size at: filePosition in: fileIndex
	"Issue several warnings as the end of the changes file approaches its limit,
	and finally halt with an error when the end is reached."

	| fileSizeLimit margin |
	fileSizeLimit _ 16r2000000 * pointerScale.
	3 to: 1 by: -1 do:
		[:i | margin := i*100000.
		(filePosition + size + margin) > fileSizeLimit
			ifTrue: [(filePosition + margin) > fileSizeLimit ifFalse:
						[self inform: 'WARNING: your changes file is within
' , margin printString , ' characters of its size limit.
You should take action soon to reduce its size.
You may proceed.']]
			ifFalse: [^ self]].
	(filePosition + size > fileSizeLimit) ifFalse: [^ self].
	self error: 'You have reached the size limit of the changes file.
You must take action now to reduce it.
Close this error.  Do not attempt to proceed.'
</details>

#### CuisSourceFileArray>>#initialize: nFiles

<details>
	<summary>See more</summary>
	
	initialize: nFiles
	files _ Array new: nFiles.
	pointerScale _ 32
</details>

#### CuisSourceFileArray>>#pointerScaleForWriting

<details>
	<summary>See more</summary>
	
	pointerScaleForWriting
	^pointerScale
</details>

#### CuisSourceFileArray>>#filePositionFromSourcePointer: anInteger

Return the position of the source chunk addressed by anInteger


<details>
	<summary>See more</summary>
	
	filePositionFromSourcePointer: anInteger
	"Return the position of the source chunk addressed by anInteger"

	| unscaledPosition |
	"No source pointer => no filePosition"
	anInteger = 0 ifTrue: [ ^0 ].
	unscaledPosition _ (anInteger - 16r1000000) bitAnd: 16r1FFFFFF.
	^unscaledPosition * pointerScale
</details>

#### CuisSourceFileArray>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	^files at: index
</details>

#### CuisSourceFileArray>>#sourcePointerFromFileIndex: index andPosition: position

Return a sourcePointer encoding the given file index and position


<details>
	<summary>See more</summary>
	
	sourcePointerFromFileIndex: index andPosition: position
	"Return a sourcePointer encoding the given file index and position"
	| answer changesFlag |
	((index between: 1 and: 2) and: [position >= 0])
		ifFalse: [self error: 'invalid source code pointer'].
	changesFlag _ index = 2 ifTrue: [ 16r2000000 ] ifFalse: [ 0 ].
	answer _ (position // pointerScale bitOr: changesFlag) + 16r1000000.
	^answer
</details>

## FileStream

I represent a Stream that accesses a FilePage from a File. One use for my instance is to access larger "virtual Strings" than can be stored contiguously in main memory. I restrict the objects stored and retrieved to be Integers or Characters. An end of file pointer terminates reading; it can be extended by writing past it, or the file can be explicitly truncated. To use the file system for most applications, you typically create a FileStream. This is done by sending a message to a FileDirectory (file:, oldFile:, newFile:, rename:newName:) which creates a (sub)instance of me. Accesses to the file are then done via this instance. *** On DOS, files cannot be shortened! *** To overwrite a file with a shorter one, first delete the old file (FileDirectory deleteFilePath: 'Hard Disk:aFolder:dataFolder:foo') or (aFileDirectory deleteFileNamed: 'foo'). Then write your new shorter version. Usually, the active class is StandardFileStream. But regardless of this, it is conventional to reference FileStream in code. This allows replacing StandardFileStream with something else (and not a real file in the host OS file system) if needed.

### Methods
#### FileStream>>#localName

<details>
	<summary>See more</summary>
	
	localName
	self subclassResponsibility
</details>

#### FileStream>>#contentsOfEntireFile

Read all of the contents of the receiver.


<details>
	<summary>See more</summary>
	
	contentsOfEntireFile
	"Read all of the contents of the receiver."

	| s |
	self readOnly.
	self reset.
	s _ self next: self size.
	self close.
	^s
</details>

#### FileStream>>#readWrite

Set this file's mode to read-write.


<details>
	<summary>See more</summary>
	
	readWrite
	"Set this file's mode to read-write."

	self subclassResponsibility

</details>

#### FileStream>>#position: pos

Set the current character position in the file to pos. 1/31/96 sw: made subclassResponsibility


<details>
	<summary>See more</summary>
	
	position: pos
	"Set the current character position in the file to pos.
	 1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#atEnd

Answer true if the current position is >= the end of file position. 1/31/96 sw: subclassResponsibility


<details>
	<summary>See more</summary>
	
	atEnd
	"Answer true if the current position is >= the end of file position.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#useBytes

Set this file to binary mode.


<details>
	<summary>See more</summary>
	
	useBytes
	"Set this file to binary mode."

	self subclassResponsibility

</details>

#### FileStream>>#closed

Answer true if this file is closed.


<details>
	<summary>See more</summary>
	
	closed
	"Answer true if this file is closed."

	self subclassResponsibility

</details>

#### FileStream>>#size

Answer the size of the file in characters. 1/31/96 sw: made subclass responsibility


<details>
	<summary>See more</summary>
	
	size
	"Answer the size of the file in characters.
	 1/31/96 sw: made subclass responsibility"

	self subclassResponsibility
</details>

#### FileStream>>#next: anInteger

Answer the next anInteger elements of my collection. overriden for efficiency


<details>
	<summary>See more</summary>
	
	next: anInteger

	| newCollection howManyRead increment |
	newCollection := self collectionSpecies new: anInteger.
	howManyRead := 0.
	[howManyRead < anInteger] whileTrue:
		[self atEnd ifTrue:
			[(howManyRead + 1) to: anInteger do: [:i | newCollection at: i put: (self next)].
			^newCollection].
		increment := (readLimit - position) min: (anInteger - howManyRead).
		newCollection replaceFrom: (howManyRead + 1)
			to: (howManyRead := howManyRead + increment)
			with: collection
			startingAt: (position + 1).
		position := position + increment].
	^newCollection
</details>

#### FileStream>>#longPrintOn: aStream limitedTo: sizeLimit indent: indent

Do nothing, so it will print short. Called to print the error file. If the error was in a file operation, we can't read the contents of that file. Just print its name instead.


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream limitedTo: sizeLimit indent: indent

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream newLine
</details>

#### FileStream>>#longPrintOn: aStream

Do nothing, so it will print short. Called to print the error file. If the error was in a file operation, we can't read the contents of that file. Just print its name instead.


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream
	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

</details>

#### FileStream>>#skip: n

Set the character position to n characters from the current position. Error if not enough characters left in the file 1/31/96 sw: made subclassResponsibility.


<details>
	<summary>See more</summary>
	
	skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file
	1/31/96 sw: made subclassResponsibility."
 
	self subclassResponsibility
</details>

#### FileStream>>#nextPut: aByte

1/31/96 sw: subclassResponsibility


<details>
	<summary>See more</summary>
	
	nextPut: aByte
	"1/31/96 sw: subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#useCharacters

Set this file to ascii (text) mode.


<details>
	<summary>See more</summary>
	
	useCharacters
	"Set this file to ascii (text) mode."

	self subclassResponsibility

</details>

#### FileStream>>#close

Close this file.


<details>
	<summary>See more</summary>
	
	close
	"Close this file."

	self subclassResponsibility

</details>

#### FileStream>>#next

Return the next object in the Stream represented by the receiver.


<details>
	<summary>See more</summary>
	
	next

	(position >= readLimit and: [self atEnd])
		ifTrue: [^nil]
		ifFalse: [^collection at: (position _ position + 1)]
</details>

#### FileStream>>#setToEnd

Set the current character position to the end of the File. The same as self position: self size. 1/31/96 sw: made subclassResponsibility


<details>
	<summary>See more</summary>
	
	setToEnd
	"Set the current character position to the end of the File. The same as
	self position: self size.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#reopen

Ensure that the receiver is open, re-open it if necessary.


<details>
	<summary>See more</summary>
	
	reopen
	"Ensure that the receiver is open, re-open it if necessary."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	self subclassResponsibility

</details>

#### FileStream>>#position

Answer the current character position in the file. 1/31/96 sw: subclassResponsibility


<details>
	<summary>See more</summary>
	
	position
	"Answer the current character position in the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#nextPutAll: aCollection

1/31/96 sw: made subclass responsibility


<details>
	<summary>See more</summary>
	
	nextPutAll: aCollection
	"1/31/96 sw: made subclass responsibility"

	self subclassResponsibility
</details>

#### FileStream>>#fileIn

Guarantee that the receiver is readOnly before fileIn for efficiency and to eliminate remote sharing conflicts.


<details>
	<summary>See more</summary>
	
	fileIn
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

</details>

#### FileStream>>#isFileStream

<details>
	<summary>See more</summary>
	
	isFileStream
	^true
</details>

#### FileStream>>#readOnly

Set this file's mode to read-only.


<details>
	<summary>See more</summary>
	
	readOnly
	"Set this file's mode to read-only."

	self subclassResponsibility

</details>

#### FileStream>>#contents

Return the contents of the receiver. Do not close or otherwise touch the receiver. Return data in whatever mode the receiver is in (e.g., binary or text).


<details>
	<summary>See more</summary>
	
	contents
	"Return the contents of the receiver. Do not close or otherwise touch the receiver. Return data in whatever mode the receiver is in (e.g., binary or text)."
	| s savePos |
	savePos _ self position.
	self position: 0.
	s _ self next: self size.
	self position: savePos.
	^s
</details>

#### FileStream>>#reset

Set the current character position to the beginning of the file. 1/31/96 sw: subclassResponsibility


<details>
	<summary>See more</summary>
	
	reset
	"Set the current character position to the beginning of the file.
	 1/31/96 sw: subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#name

Answer the name of the file for the page the receiver is streaming over. 1/31/96 sw: made subclassResponsibility


<details>
	<summary>See more</summary>
	
	name
	"Answer the name of the file for the page the receiver is streaming over.  1/31/96 sw: made subclassResponsibility"

	self subclassResponsibility
</details>

#### FileStream>>#truncate: pos

Truncate file to pos


<details>
	<summary>See more</summary>
	
	truncate: pos
	"Truncate file to pos"

	self subclassResponsibility
</details>

#### FileStream>>#flush

When writing, flush the current buffer out to disk.


<details>
	<summary>See more</summary>
	
	flush
	"When writing, flush the current buffer out to disk."

	self subclassResponsibility

</details>

## RemoteString

My instances provide an external file reference to a piece of text. It may be the sourceCode of a method, or the class comments of a class. The changes file or file-in file usually has a chunk that is just the source string of a method: max: aNumber ^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!

### Methods
#### RemoteString>>#fileStream

Answer the file stream with position set at the beginning of my string


<details>
	<summary>See more</summary>
	
	fileStream 
	"Answer the file stream with position set at the beginning of my string"

	| theFile |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^ nil].
	theFile _ SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	^ theFile
</details>

#### RemoteString>>#setSourcePointer: aSourcePointer

<details>
	<summary>See more</summary>
	
	setSourcePointer: aSourcePointer
	sourceFileNumber _ SourceFiles fileIndexFromSourcePointer: aSourcePointer.
	filePositionHi _ SourceFiles filePositionFromSourcePointer: aSourcePointer
</details>

#### RemoteString>>#compilerClass

Answer a compiler class appropriate for source methods of this class.


<details>
	<summary>See more</summary>
	
	compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler
</details>

#### RemoteString>>#string

Answer the receiver's string if remote files are enabled.


<details>
	<summary>See more</summary>
	
	string 
	"Answer the receiver's string if remote files are enabled."
	| theFile answer |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^nil].
	theFile _ SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	answer _ theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]
</details>

#### RemoteString>>#sourceFileNumber

Answer the index of the file on which the string is stored.


<details>
	<summary>See more</summary>
	
	sourceFileNumber
	"Answer the index of the file on which the string is stored."

	^sourceFileNumber
</details>

#### RemoteString>>#checkSum: aString

Construct a checksum of the string. A three byte number represented as Base64 characters.


<details>
	<summary>See more</summary>
	
	checkSum: aString
	"Construct a checksum of the string.  A three byte number represented as Base64 characters."
	| sum shift bytes |
	sum := aString size.
	shift := 0.
	aString do: [:char |
		(shift := shift + 7) > 16 ifTrue: [shift := shift - 17].
			"shift by 7 to keep a change of adjacent chars from xoring to same value"
		sum := sum bitXor: (char asInteger bitShift: shift)
	].
	bytes := ByteArray new: 3.
	sum := sum + 16r10000000000.
	1 to: 3 do: [:ind | bytes at: ind put: (sum digitAt: ind)].
	^bytes base64Encoded
</details>

#### RemoteString>>#string: aString onFileNumber: fileNumber

Store this as my string if source files exist.


<details>
	<summary>See more</summary>
	
	string: aString onFileNumber: fileNumber
	"Store this as my string if source files exist."
	| theFile |
	(SourceFiles at: fileNumber) ifNotNil: [
		theFile _ SourceFiles at: fileNumber.
		theFile setToEnd; newLine.
		self string: aString onFileNumber: fileNumber toFile: theFile]
</details>

#### RemoteString>>#position

Answer the location of the string on a file.


<details>
	<summary>See more</summary>
	
	position 
	"Answer the location of the string on a file."

	^ filePositionHi
</details>

#### RemoteString>>#sourcePointer

<details>
	<summary>See more</summary>
	
	sourcePointer
	sourceFileNumber ifNil: [^ 0].
	^SourceFiles sourcePointerFromFileIndex: sourceFileNumber andPosition: filePositionHi
</details>

#### RemoteString>>#last

<details>
	<summary>See more</summary>
	
	last
	^self string ifNotNil: [ :s | s last ]
</details>

#### RemoteString>>#fileNumber: fileNumber position: position

<details>
	<summary>See more</summary>
	
	fileNumber: fileNumber position: position 

	sourceFileNumber _ fileNumber.
	filePositionHi _ position
</details>

#### RemoteString>>#isRemote

<details>
	<summary>See more</summary>
	
	isRemote
	^true
</details>

#### RemoteString>>#text

In Cuis, all source code is plain Strings


<details>
	<summary>See more</summary>
	
	text 
	"In Cuis, all source code is plain Strings"
	^self string
</details>

#### RemoteString>>#string: aStringOrText onFileNumber: fileNumber toFile: aFileStream

Store this as the receiver's text if source files exist.


<details>
	<summary>See more</summary>
	
	string: aStringOrText onFileNumber: fileNumber toFile: aFileStream
	"Store this as the receiver's text if source files exist."

	| position |
	fileNumber = 0 ifFalse: [
		aFileStream padTo: SourceFiles pointerScaleForWriting put: $  ].
	position _ aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aStringOrText asString
</details>

## StandardFileStream

Provides a simple, platform-independent, interface to a file system. The instance variable rwmode, inherited from class PositionableStream, here is used to hold a Boolean -- true means opened for read-write, false means opened for read-only. 2/12/96 sw I implement a simple read buffering scheme with the variables defined in PositionableStream (which are unused in me otherwise) in the following way: collection <ByteString> or <ByteArray> This is the buffer. position <Integer> The relative position in the buffer. Greater or equal to zero. readLimit <Integer> The number of bytes buffered. Greater or equal to zero. Read buffering is enabled with #enableReadBuffering, disabled with #disableReadBuffering and it is enabled by default. The buffer is filled when a read attempt of an unbuffered absolute position is requested, or when a negative repositioning is made (with #position: with an argument < than the current absolute position) to an absolute position which is not buffered. In the first case, the buffer is positioned to the given absolute position. In the latter case the repositioning is made to the requested absolute position minus fourth of the buffer size. This means that further small negative repositionings won't result in buffer flushing. This is really useful when filing in code. The read buffer is flushed (#flushReadBuffer) whenever a write attempt is made. The buffer state is valid if and only if collection is not nil and position < readLimit.

### Methods
#### StandardFileStream>>#localName

<details>
	<summary>See more</summary>
	
	localName
	^ self fileEntry name
</details>

#### StandardFileStream>>#readWrite

Make this file writable.


<details>
	<summary>See more</summary>
	
	readWrite
	"Make this file writable."

	rwmode _ true.

</details>

#### StandardFileStream>>#position: pos

Set the receiver's position as indicated. 2/12/96 sw


<details>
	<summary>See more</summary>
	
	position: pos
	"Set the receiver's position as indicated.  2/12/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	^self primSetPosition: fileID to: pos
</details>

#### StandardFileStream>>#primCloseNoError: id

Close this file. Don't raise an error if the primitive fails.


<details>
	<summary>See more</summary>
	
	primCloseNoError: id
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>

</details>

#### StandardFileStream>>#peekFor: item

Answer false and do not advance if the next element is not equal to item, or if this stream is at the end. If the next element is equal to item, then advance over it and return true


<details>
	<summary>See more</summary>
	
	peekFor: item 
	"Answer false and do not advance if the next element is not equal to item, or if this stream is at the end.  If the next element is equal to item, then advance over it and return true"
	| next |
	"self atEnd ifTrue: [^ false]. -- SFStream will give nil"
	(next _ self next) ifNil: [^ false].
	item = next ifTrue: [^ true].
	self skip: -1.
	^ false
</details>

#### StandardFileStream>>#atEnd

Answer whether the receiver is at its end.


<details>
	<summary>See more</summary>
	
	atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	^self primAtEnd: fileID
</details>

#### StandardFileStream>>#upTo: delim

Fast version to speed up nextChunk


<details>
	<summary>See more</summary>
	
	upTo: delim 
	"Fast version to speed up nextChunk"
	| pos buffer count |
	collection ifNotNil: [
		(position < readLimit and: [
			(pos := collection indexOf: delim startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^collection copyFrom: position + 1 to: (position := pos) - 1 ] ].
	pos := self position.
	buffer := self next: 2000.
	(count := buffer indexOf: delim) > 0 ifTrue: 
		["Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upTo: delim)
</details>

#### StandardFileStream>>#useBytes

Set this file to binary mode.


<details>
	<summary>See more</summary>
	
	useBytes
	buffer1 := ByteArray new: 1.
	collection ifNotNil: [ collection := collection asByteArray ]
</details>

#### StandardFileStream>>#closed

Answer true if this file is closed.


<details>
	<summary>See more</summary>
	
	closed
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]

</details>

#### StandardFileStream>>#primTruncate: id to: anInteger

Truncate this file to the given position.


<details>
	<summary>See more</summary>
	
	primTruncate: id to: anInteger
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#printOn: aStream

Put a printed version of the receiver onto aStream. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
</details>

#### StandardFileStream>>#primGetPosition: id

Get this files current position.


<details>
	<summary>See more</summary>
	
	primGetPosition: id
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#actAsExecutor

Prepare the receiver to act as executor for any resources associated with it


<details>
	<summary>See more</summary>
	
	actAsExecutor
	super actAsExecutor.
	name := nil.
</details>

#### StandardFileStream>>#peek

Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.


<details>
	<summary>See more</summary>
	
	peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	position < readLimit ifTrue: [
		^collection at: position+1 ].
	self atEnd ifTrue: [^ nil].
	next _ self basicNext.
	self position: self position - 1.
	^ next
</details>

#### StandardFileStream>>#size

Answer the size of the file in characters. 2/12/96 sw


<details>
	<summary>See more</summary>
	
	size
	"Answer the size of the file in characters.  2/12/96 sw"

	^ self primSize: fileID
</details>

#### StandardFileStream>>#fileEntry

<details>
	<summary>See more</summary>
	
	fileEntry
	^ self fullName asFileEntry
</details>

#### StandardFileStream>>#isDirectory

Answer whether the receiver represents a directory. For the post-transition case, uncertain what to do. 2/14/96 sw


<details>
	<summary>See more</summary>
	
	isDirectory
	"Answer whether the receiver represents a directory.  For the post-transition case, uncertain what to do.  2/14/96 sw"
	^ false
</details>

#### StandardFileStream>>#basicNext

Answer the next byte or character (depending on mode) from this file, or nil if at the end of the file.


<details>
	<summary>See more</summary>
	
	basicNext
	"Answer the next byte or character (depending on mode) from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]
</details>

#### StandardFileStream>>#collectionSpecies

Answer the species of collection into which the receiver can stream. This is ByteArray or String, depending on the mode.


<details>
	<summary>See more</summary>
	
	collectionSpecies
	"Answer the species of collection into which the receiver can stream.
	This is ByteArray or String, depending on the mode."
	
	^buffer1 species
</details>

#### StandardFileStream>>#open: fileName forWrite: writeMode

Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode.


<details>
	<summary>See more</summary>
	
	open: fileName forWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ StandardFileStream retryWithGC:[self primOpen: fileName writable: writeMode] 
					until:[:id| id notNil] 
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ String new: 1.
	self enableReadBuffering

</details>

#### StandardFileStream>>#register

<details>
	<summary>See more</summary>
	
	register
	^self class register: self
</details>

#### StandardFileStream>>#skip: n

Set the character position to n characters from the current position. Error if not enough characters left in the file. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.  1/31/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			((newPosition := position + n) >= 0 and: [ newPosition < readLimit ])
				ifTrue: [ 
					position := newPosition.
					^self ] ] ].
	self position: self position + n
</details>

#### StandardFileStream>>#upToEnd

Answer a subcollection from the current access position through the last element of the receiver.


<details>
	<summary>See more</summary>
	
	upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	^self collectionSpecies streamContents: [ :newStream |
		| next |
		[ (next := self next) == nil ] whileFalse: [
			newStream nextPut: next ] ]
</details>

#### StandardFileStream>>#peekLast

Return that item just put at the end of the stream


<details>
	<summary>See more</summary>
	
	peekLast
	"Return that item just put at the end of the stream"

	^ buffer1 size > 0 
		ifTrue: [buffer1 last]
		ifFalse: [nil]

</details>

#### StandardFileStream>>#next

Answer the next byte from this file, or nil if at the end of the file.


<details>
	<summary>See more</summary>
	
	next
	"Answer the next byte from this file, or nil if at the end of the file."

	^ self basicNext
</details>

#### StandardFileStream>>#primSizeNoError: id

Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale.


<details>
	<summary>See more</summary>
	
	primSizeNoError: id
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil

</details>

#### StandardFileStream>>#findStringFromEnd: string

Fast version to find a String in a file starting from the end. Returns the position and also sets the position there. If string is not found 0 is returned and position is unchanged.


<details>
	<summary>See more</summary>
	
	findStringFromEnd: string
	"Fast version to find a String in a file starting from the end.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos |
	oldPos _ self position.
	self setToEnd.
	pos _ self position.
	[ pos _ ((pos - 2000 + string size) max: 0).  "the [+ string size] allows for the case where the end of the search string is at the beginning of the current buffer"
	self position: pos.
	buffer _ self next: 2000.
	(count _ buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count-1.  "use next instead of position:, so that CrLfFileStream can do its magic if it is being used"
			^self position].
	pos = 0] whileFalse.
	"Never found it, and hit beginning of file"
	self position: oldPos.
	^0
</details>

#### StandardFileStream>>#position

Return the receiver's current file position. 2/12/96 sw


<details>
	<summary>See more</summary>
	
	position
	"Return the receiver's current file position.  2/12/96 sw"

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID
</details>

#### StandardFileStream>>#padToEndIfCantTruncate

Only makes sense for file streams with existing content. On file systems that don't support truncating this is needed. If truncating is supported, try that first


<details>
	<summary>See more</summary>
	
	padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [Character space numericValue]
		ifFalse: [Character space ].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad)
</details>

#### StandardFileStream>>#directory

Return the directory containing this file.


<details>
	<summary>See more</summary>
	
	directory
	"Return the directory containing this file."

	^ self fileEntry parent
</details>

#### StandardFileStream>>#nextPutAll: aString

Write all the characters of the given string to this file.


<details>
	<summary>See more</summary>
	
	nextPutAll: aString
	"Write all the characters of the given string to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aString startingAt: 1 count: aString basicSize.
	^ aString

</details>

#### StandardFileStream>>#primRead: id into: byteArray startingAt: startIndex count: count

Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read.


<details>
	<summary>See more</summary>
	
	primRead: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.

</details>

#### StandardFileStream>>#readOnly

Make this file read-only.


<details>
	<summary>See more</summary>
	
	readOnly
	"Make this file read-only."

	rwmode _ false.

</details>

#### StandardFileStream>>#name

Answer this file's full path name.


<details>
	<summary>See more</summary>
	
	name
	"Answer this file's full path name."

	^ name

</details>

#### StandardFileStream>>#fullName

Answer this file's full path name.


<details>
	<summary>See more</summary>
	
	fullName
	"Answer this file's full path name."

	^ name

</details>

#### StandardFileStream>>#crc16

Copied from String>>crc16


<details>
	<summary>See more</summary>
	
	crc16
	"Copied from String>>crc16"
	
	| buffer crc |
	self reset.
	buffer _ String new: 2000.
	crc _ 0.
	[ self atEnd ] whileFalse: [
		buffer _ self nextInto: buffer.
		buffer do: [:c |
			crc _ (crc bitShift: -8) bitXor: (
			 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
				16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
				16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
				16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
				16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
				16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
				16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
				16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
				16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
				16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
				16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
				16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
				16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
				16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
				16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
				16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
				16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
				16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
				16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
				16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
				16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
				16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
				16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
				16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
				16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
				16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
				16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
				16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
				16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
				16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
				16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
				16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c numericValue) bitAnd: 16rFF) + 1) 
		].
	].
	^crc
</details>

#### StandardFileStream>>#primSize: id

Answer the size of this file.


<details>
	<summary>See more</summary>
	
	primSize: id
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#flush

Flush pending changes


<details>
	<summary>See more</summary>
	
	flush
	"Flush pending changes"
	^self primFlush: fileID
</details>

#### StandardFileStream>>#primWrite: id from: stringOrByteArray startingAt: startIndex count: count

Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written.


<details>
	<summary>See more</summary>
	
	primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])
</details>

#### StandardFileStream>>#readInto: byteArray startingAt: startIndex count: count

Read into the given array as specified, and return the count actually transferred. index and count are in units of bytes or longs depending on whether the array is Bitmap, String or ByteArray


<details>
	<summary>See more</summary>
	
	readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"
	
	^(self next: count into: byteArray startingAt: startIndex) size - startIndex + 1

</details>

#### StandardFileStream>>#enableReadBuffering

<details>
	<summary>See more</summary>
	
	enableReadBuffering

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := self collectionSpecies new: 2048 ] ].
	readLimit := position := 0
</details>

#### StandardFileStream>>#ensureOpen

Make sure that this file really is open.


<details>
	<summary>See more</summary>
	
	ensureOpen
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.

</details>

#### StandardFileStream>>#unregister

<details>
	<summary>See more</summary>
	
	unregister
	^self class unregister: self
</details>

#### StandardFileStream>>#next: anInteger putAll: aString startingAt: startIndex

Store the next anInteger elements from the given collection.


<details>
	<summary>See more</summary>
	
	next: anInteger putAll: aString startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	^aString
</details>

#### StandardFileStream>>#openReadOnly

Open the receiver as a read-only file. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	openReadOnly
	"Open the receiver as a read-only file.  1/31/96 sw"

	^ self open: name forWrite: false
</details>

#### StandardFileStream>>#next: n into: aString startingAt: startIndex

Read n bytes into the given string. Return aString or a partial copy if less than n elements have been read.


<details>
	<summary>See more</summary>
	
	next: n into: aString startingAt: startIndex
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
	collection 
		ifNil: [ 
			newN := n.
			newStartIndex := startIndex ]
		ifNotNil: [
			aString class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := n.
					newStartIndex := startIndex ]
				ifTrue: [
					| available |
					(available := readLimit - position) > 0 
						ifFalse: [ available := 0 ]
						ifTrue: [
							| bufferedCount |
							bufferedCount := n min: available.
							aString
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = n ifTrue: [ ^aString ] ].
					newN := n - available.
					newStartIndex := startIndex + available ] ].
	count := self primRead: fileID into: aString
				startingAt: newStartIndex count: newN.
	count = newN
		ifTrue:[ ^aString ]
		ifFalse:[ ^aString copyFrom: 1 to: newStartIndex + count - 1 ]
</details>

#### StandardFileStream>>#next: n

Return a string with the next n characters of the filestream in it. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	next: n
	"Return a string with the next n characters of the filestream in it.  1/31/96 sw"
	^ self nextInto: (self collectionSpecies new: n)
</details>

#### StandardFileStream>>#nextWordsInto: aBitmap

Note: The file primitives automatically adjust for word based objects.


<details>
	<summary>See more</summary>
	
	nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap
</details>

#### StandardFileStream>>#primClose: id

Close this file.


<details>
	<summary>See more</summary>
	
	primClose: id
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#isReadOnly

To be redefined in subclasses that can't do #nextPut:


<details>
	<summary>See more</summary>
	
	isReadOnly

	^ rwmode not

</details>

#### StandardFileStream>>#isBinary

Return true if the receiver is a binary byte stream


<details>
	<summary>See more</summary>
	
	isBinary
	^ buffer1 class == ByteArray
</details>

#### StandardFileStream>>#primOpen: fileName writable: writableFlag

Open a file of the given name, and return the file ID obtained. If writableFlag is true, then if there is none with this name, then create one else prepare to overwrite the existing from the beginning otherwise if the file exists, open it read-only else return nil


<details>
	<summary>See more</summary>
	
	primOpen: fileName writable: writableFlag
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil

</details>

#### StandardFileStream>>#nextPut: char

Write the given byte or character (depending on mode) to this file.


<details>
	<summary>See more</summary>
	
	nextPut: char
	"Write the given byte or character (depending on mode) to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: char.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	^ char

</details>

#### StandardFileStream>>#primSetPosition: id to: anInteger

Set this file to the given position.


<details>
	<summary>See more</summary>
	
	primSetPosition: id to: anInteger
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#useCharacters

opposite of binary


<details>
	<summary>See more</summary>
	
	useCharacters
	"opposite of binary"
	buffer1 := String new: 1.
	collection ifNotNil: [ collection := collection asString ]
</details>

#### StandardFileStream>>#close

Close this file.


<details>
	<summary>See more</summary>
	
	close
	"Close this file."

	fileID ifNotNil: [
		collection ifNotNil: [
			readLimit := position := 0 ].
		self primClose: fileID.
		self unregister.
		fileID := nil].

</details>

#### StandardFileStream>>#setToEnd

Set the position of the receiver to the end of file. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	setToEnd
	"Set the position of the receiver to the end of file.  1/31/96 sw"

	self position: self size
</details>

#### StandardFileStream>>#reopen

Close and reopen this file. The file position is reset to zero.


<details>
	<summary>See more</summary>
	
	reopen
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	| binary |
	binary _ self isBinary.
	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.
	binary ifTrue: [self binary]

</details>

#### StandardFileStream>>#flushReadBuffer

<details>
	<summary>See more</summary>
	
	flushReadBuffer

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]
</details>

#### StandardFileStream>>#primAtEnd: id

Answer true if the file position is at the end of the file.


<details>
	<summary>See more</summary>
	
	primAtEnd: id
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### StandardFileStream>>#primFlush: id

Flush pending changes to the disk


<details>
	<summary>See more</summary>
	
	primFlush: id
	"Flush pending changes to the disk"
	| p |
	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	"In some OS's seeking to 0 and back will do a flush"
	p _ self position.
	self position: 0; position: p
</details>

#### StandardFileStream>>#findString: string

Fast version of #upToAll: to find a String in a file starting from the beginning. Returns the position and also sets the position there. If string is not found 0 is returned and position is unchanged.


<details>
	<summary>See more</summary>
	
	findString: string
	"Fast version of #upToAll: to find a String in a file starting from the beginning.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos sz |
	oldPos _ self position.
	self reset.
	sz _ self size.
	pos _ 0.
	buffer _ String new: 2000.
	[ buffer := self nextInto: buffer.
	(count _ buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count - 1.
			^self position ].
	pos _ ((pos + 2000 - string size) min: sz).
	self position: pos.
	pos = sz] whileFalse.
	"Never found it, and hit end of file"
	self position: oldPos.
	^0
</details>

#### StandardFileStream>>#truncate

Truncate to zero


<details>
	<summary>See more</summary>
	
	truncate
	"Truncate to zero"

	^ self truncate: 0
</details>

#### StandardFileStream>>#reset

Set the current character position to the beginning of the file. 1/31/96 sw: subclassResponsibility


<details>
	<summary>See more</summary>
	
	reset
	self ensureOpen.
	self position: 0.
</details>

#### StandardFileStream>>#truncate: pos

Truncate to this position


<details>
	<summary>See more</summary>
	
	truncate: pos
	"Truncate to this position"

	self position: pos.
	^self primTruncate: fileID to: pos
</details>

#### StandardFileStream>>#disableReadBuffering

<details>
	<summary>See more</summary>
	
	disableReadBuffering

	collection ifNotNil: [
		position < readLimit
			ifTrue: [
				| currentPosition |
				currentPosition := self position.
				collection := readLimit := position := nil.
				self position: currentPosition ]
			ifFalse: [
				collection := readLimit := position := nil ] ]
		
</details>

#### StandardFileStream>>#finalize

Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority.


<details>
	<summary>See more</summary>
	
	finalize
	self primCloseNoError: fileID.
</details>

