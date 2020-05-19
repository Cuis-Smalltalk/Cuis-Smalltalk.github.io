## DirectoryEntry

I represent a single file directory. I implement various directory specific behaviors. You can write data by #at:put: , and read the data by #at:. --- mu 11/6/2006 20:21 -------------- Some examples: DirectoryEntry default DirectoryEntry root DirectoryEntry roots See FileEntry

### Methods
#### DirectoryEntry>>#directoryMatching: pattern

DirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'. DirectoryEntry smalltalkImageDirectory directoryMatching: 'xC*Pack*'.


<details>
	<summary>See more</summary>
	
	directoryMatching: pattern
"
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'C*Pack*'.
	DirectoryEntry smalltalkImageDirectory directoryMatching: 'xC*Pack*'.
"
	self directoriesDo: [ :directory |
		(pattern match: directory name)
			ifTrue: [ ^ directory ]].
	^ nil
</details>

#### DirectoryEntry>>#binaryAt: localFileName put: contents

<details>
	<summary>See more</summary>
	
	binaryAt: localFileName put: contents 
	^self at: localFileName put: contents asByteArray
</details>

#### DirectoryEntry>>#copyTo: filename

<details>
	<summary>See more</summary>
	
	copyTo: filename 
	
	| toDir |
	filename asFileEntry exists ifTrue: [^self error: 'Cannot copy directory to file'].
	
	toDir := filename asDirectoryEntry assureExistence.

	self
		filesDo: [:file | file copyTo: (toDir / file name) pathName].
	
	self
		directoriesDo: [ :dir |
			dir copyTo:  dir pathName ]
</details>

#### DirectoryEntry>>#keys

<details>
	<summary>See more</summary>
	
	keys
	^self files
</details>

#### DirectoryEntry>>#at: localFileName put: contents

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: localFileName put: contents

	(self // localFileName) forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: contents ].
	self invalidateChildren.
	^contents
</details>

#### DirectoryEntry>>#files

<details>
	<summary>See more</summary>
	
	files
	^self children select: [:each | each isFile]
</details>

#### DirectoryEntry>>#latestFileMatches: selectionBlock

<details>
	<summary>See more</summary>
	
	latestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) first
</details>

#### DirectoryEntry>>#regularDirectoriesDo: aBlock

<details>
	<summary>See more</summary>
	
	regularDirectoriesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifFalse: [
			each isRegularDirectory ifTrue: [
				aBlock value: each ]]]
</details>

#### DirectoryEntry>>#isRegularDirectory

hidden convention in Unix


<details>
	<summary>See more</summary>
	
	isRegularDirectory
	"hidden convention in Unix"
	name first = $. ifTrue: [ ^false ].
	"in MacOS, applications are actually directories, but are usually not treated as such"
	self extension = 'app' ifTrue: [ ^false ].
	"in MacOS, .bundle directories, are resource packages"
	self extension = 'bundle' ifTrue: [ ^false ].
	^true
</details>

#### DirectoryEntry>>#isFile

<details>
	<summary>See more</summary>
	
	isFile
	^false
</details>

#### DirectoryEntry>>#removeKey: localFileName ifAbsent: failBlock

<details>
	<summary>See more</summary>
	
	removeKey: localFileName ifAbsent: failBlock
	self fileAccessor deleteFile: (self // localFileName) pathName ifAbsent: [^failBlock value].
	self invalidateChildren.
</details>

#### DirectoryEntry>>#removeKey: localFileName

<details>
	<summary>See more</summary>
	
	removeKey: localFileName 
	self removeKey: localFileName ifAbsent: []
</details>

#### DirectoryEntry>>#delete

<details>
	<summary>See more</summary>
	
	delete
	self fileAccessor deleteDirectory: self pathName.
	self invalidateChildren
</details>

#### DirectoryEntry>>#concatPathComponentsAsDirectory: components

<details>
	<summary>See more</summary>
	
	concatPathComponentsAsDirectory: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry
</details>

#### DirectoryEntry>>#nextNameFor: baseFileName coda: fileNameCoda extension: extension

Assumes a file name includes a version number encoded as '.' followed by digits preceding the file extension. Increment the version number and answer the new file name. If a version number is not found, set the version to 1 and answer a new file name. fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like: someFileName-authorXX.cs someFileName-authorYY.1.cs someFileName-authorZZ.2.cs


<details>
	<summary>See more</summary>
	
	nextNameFor: baseFileName coda: fileNameCoda extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name.
	fileNameCoda is ignored during version number search, but added to the final name. It allows sequences like:
	someFileName-authorXX.cs
	someFileName-authorYY.1.cs
	someFileName-authorZZ.2.cs
	"

	| files splits version candidate |
	files _ self fileNamesMatching: (baseFileName,'*.', extension).
	splits _ files collect: [ :file | self fileAccessor splitNameVersionExtensionFor: file ].
	splits _ splits asArray sort: [ :a :b | (a at: 2) < (b at: 2)].
	splits isEmpty 
			ifTrue: [ version _ 1 ]
			ifFalse: [ version _ (splits last at: 2) + 1 ].
	candidate _ (baseFileName, fileNameCoda, '.', (String streamContents: [ :strm | version printOn: strm length: 3 zeroPadded: true ]), '.', extension) asFileName.
	^ candidate
</details>

#### DirectoryEntry>>#assureExistence

<details>
	<summary>See more</summary>
	
	assureExistence
	self isRoot ifTrue: [^self].
	self parent assureExistenceDirNamed: self name.
	^self
</details>

#### DirectoryEntry>>#allChildrenDo: aBlock

<details>
	<summary>See more</summary>
	
	allChildrenDo: aBlock
	self childrenDo: [ :child | 
		aBlock value: child ].
	self allDirectoriesDo: [ :child |
		child allChildrenDo: aBlock]
</details>

#### DirectoryEntry>>#isDirectory

<details>
	<summary>See more</summary>
	
	isDirectory
	^true
</details>

#### DirectoryEntry>>#fileNames

<details>
	<summary>See more</summary>
	
	fileNames
	^self files collect: [:each | each name]
</details>

#### DirectoryEntry>>#oldestFileMatches: selectionBlock

<details>
	<summary>See more</summary>
	
	oldestFileMatches: selectionBlock
	| entries |
	entries := self filesMatches: selectionBlock.
	entries ifEmpty: [^nil].
	^(entries sort: [:a :b | a modificationTime > b modificationTime]) last
</details>

#### DirectoryEntry>>#includesKey: fileName

<details>
	<summary>See more</summary>
	
	includesKey: fileName
	^self fileNames includes: fileName
</details>

#### DirectoryEntry>>#children

<details>
	<summary>See more</summary>
	
	children
	self invalidateIfOld.
	children ifNil: [self initChildren].
	^children
</details>

#### DirectoryEntry>>#nextNameFor: baseFileName extension: extension

Assumes a file name includes a version number encoded as '.' followed by digits preceding the file extension. Increment the version number and answer the new file name. If a version number is not found, set the version to 1 and answer a new file name


<details>
	<summary>See more</summary>
	
	nextNameFor: baseFileName extension: extension
	"Assumes a file name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, set the version to 1 and answer a new file name"

	^self nextNameFor: baseFileName coda: '' extension: extension
</details>

#### DirectoryEntry>>#allFilesDo: aBlock

<details>
	<summary>See more</summary>
	
	allFilesDo: aBlock
	self childrenDo: 
		[:child | 
		child isFile ifTrue: [aBlock value: child] ifFalse: [child allFilesDo: aBlock]]
</details>

#### DirectoryEntry>>#at: localFileName ifAbsent: block

<details>
	<summary>See more</summary>
	
	at: localFileName ifAbsent: block

	^ [self at: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]
</details>

#### DirectoryEntry>>#initChildren

<details>
	<summary>See more</summary>
	
	initChildren

	self exists ifFalse: [ ^children _ #()] .
	children _ self fileAccessor entriesIn: self.
	^children
</details>

#### DirectoryEntry>>#updateExists

<details>
	<summary>See more</summary>
	
	updateExists
	
	| pathName |
	(self fileAccessor isDriveSupported and: [self pathComponents isEmpty])
		 ifTrue: [^ exists _ self fileAccessor drives includes: self ].

	self isRoot ifTrue: [ ^ exists _ true ].
	
	pathName _ self pathName.
	pathName = self fileAccessor slash ifTrue: [ ^ exists _ true ].

	exists _ self fileAccessor basicDirectoryExists: pathName
</details>

#### DirectoryEntry>>#binaryAt: localFileName

<details>
	<summary>See more</summary>
	
	binaryAt: localFileName 

	^ (self // localFileName) binaryContents
</details>

#### DirectoryEntry>>#childrenDo: aBlock

<details>
	<summary>See more</summary>
	
	childrenDo: aBlock
	^self children do: aBlock
</details>

#### DirectoryEntry>>#rename: newName

<details>
	<summary>See more</summary>
	
	rename: newName
	
	self fileAccessor renameDirectory: self pathName to: (self parent / newName) pathName.
	self name: newName
</details>

#### DirectoryEntry>>#// pathString

Answer an instance of FileEntry. If you want an instance of DirectoryEntry, please call #/


<details>
	<summary>See more</summary>
	
	// pathString
	"Answer an instance of FileEntry.
	If you want an instance of DirectoryEntry, please call #/"
	^self concatPathComponentsAsFile: pathString asString asPathComponents
</details>

#### DirectoryEntry>>#basicRecursiveDelete

<details>
	<summary>See more</summary>
	
	basicRecursiveDelete
	self invalidateChildren.
	self directoriesDo: [:dir | dir basicRecursiveDelete].
	self filesDo: [:file | file delete].
	self delete
</details>

#### DirectoryEntry>>#filesMatches: selectionBlock

<details>
	<summary>See more</summary>
	
	filesMatches: selectionBlock
	^self files select: selectionBlock
</details>

#### DirectoryEntry>>#directoryNames

<details>
	<summary>See more</summary>
	
	directoryNames
	^self directories collect: [:each | each name]
</details>

#### DirectoryEntry>>#directoriesMatches: selectionBlock

<details>
	<summary>See more</summary>
	
	directoriesMatches: selectionBlock
	^self directories select: selectionBlock
</details>

#### DirectoryEntry>>#filesDo: aBlock

<details>
	<summary>See more</summary>
	
	filesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifTrue: [
			aBlock value: each ]]
</details>

#### DirectoryEntry>>#directoryNamesMatching: pat

DirectoryEntry currentDirectory directoryNamesMatching: '*'


<details>
	<summary>See more</summary>
	
	directoryNamesMatching: pat
	"
	DirectoryEntry currentDirectory directoryNamesMatching: '*'
	"

	^ self directoryNames select: [ :each | pat match: each ]
</details>

#### DirectoryEntry>>#invalidate

Assume we know nothing about current state in the File System. This might be because we're just created. Or it might be because there is a chance the File System changed and we don't know current state.


<details>
	<summary>See more</summary>
	
	invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	super invalidate.
	self invalidateChildren
</details>

#### DirectoryEntry>>#at: localFileName

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: localFileName

	^(self // localFileName) textContents
</details>

#### DirectoryEntry>>#/ pathString

Answer an instance of DirectoryEntry. If you want an instance of FileEntry, please call #//


<details>
	<summary>See more</summary>
	
	/ pathString
	"Answer an instance of DirectoryEntry.
	If you want an instance of FileEntry, please call #//"
	^self concatPathComponentsAsDirectory: pathString asString asPathComponents
</details>

#### DirectoryEntry>>#invalidateChildren

<details>
	<summary>See more</summary>
	
	invalidateChildren
	
	children _ nil
</details>

#### DirectoryEntry>>#updateFrom: primitiveArray entryInParent: index

<details>
	<summary>See more</summary>
	
	updateFrom: primitiveArray entryInParent: index
	super updateFrom: primitiveArray entryInParent: index.
	children _ nil.	"lazy initialization"
</details>

#### DirectoryEntry>>#fileNamesMatching: pat

DirectoryEntry currentDirectory fileNamesMatching: '*'


<details>
	<summary>See more</summary>
	
	fileNamesMatching: pat
	"
	DirectoryEntry currentDirectory fileNamesMatching: '*'
	"

	^ self fileNames select: [ :each | pat match: each ]
</details>

#### DirectoryEntry>>#binaryAt: localFileName ifAbsent: block

<details>
	<summary>See more</summary>
	
	binaryAt: localFileName ifAbsent: block

	^ [self binaryAt: localFileName]
		on: FileDoesNotExistException
		do: [:ex | block value]
</details>

#### DirectoryEntry>>#fileMatching: pattern

DirectoryEntry smalltalkImageDirectory fileMatching: '*.image'. DirectoryEntry smalltalkImageDirectory fileMatching: 'x*.image'.


<details>
	<summary>See more</summary>
	
	fileMatching: pattern
"
	DirectoryEntry smalltalkImageDirectory fileMatching: '*.image'.
	DirectoryEntry smalltalkImageDirectory fileMatching: 'x*.image'.
"
	self filesDo: [ :file |
		(pattern match: file name)
			ifTrue: [ ^ file ]].
	^ nil
</details>

#### DirectoryEntry>>#assureExistenceDirNamed: localName

<details>
	<summary>See more</summary>
	
	assureExistenceDirNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"
	(self fileAccessor fileOrDirectoryExists: localName in: self) ifTrue: [^ self]. "exists"

	"otherwise check parent first and then create local dir"
	self parent ifNotNil: [:p | p assureExistenceDirNamed: self name].

	self fileAccessor createDirectory: (self / localName) pathName
</details>

#### DirectoryEntry>>#directories

<details>
	<summary>See more</summary>
	
	directories
	^self children select: [:each | each isFile not]
</details>

#### DirectoryEntry>>#concatPathComponentsAsFile: components

<details>
	<summary>See more</summary>
	
	concatPathComponentsAsFile: components

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FileEntry withPathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry
</details>

#### DirectoryEntry>>#recursiveDelete

<details>
	<summary>See more</summary>
	
	recursiveDelete
	self exists
		ifTrue: [self basicRecursiveDelete]
</details>

#### DirectoryEntry>>#allRegularDirectoriesDo: aBlock

<details>
	<summary>See more</summary>
	
	allRegularDirectoriesDo: aBlock
	self regularDirectoriesDo: [ :child |
		aBlock value: child.
		child allRegularDirectoriesDo: aBlock]
</details>

#### DirectoryEntry>>#directoriesDo: aBlock

<details>
	<summary>See more</summary>
	
	directoriesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifFalse: [
			aBlock value: each ]]
</details>

#### DirectoryEntry>>#allDirectoriesDo: aBlock

<details>
	<summary>See more</summary>
	
	allDirectoriesDo: aBlock
	self directoriesDo: 
		[:child | 
		aBlock value: child.
		child allDirectoriesDo: aBlock]
</details>

#### DirectoryEntry>>#allFilesDo: aBlock matches: selectionBlock

<details>
	<summary>See more</summary>
	
	allFilesDo: aBlock matches: selectionBlock
	self childrenDo: 
		[:child | 
		child isFile
			ifTrue: [(selectionBlock value: child) ifTrue: [aBlock value: child]]
			ifFalse: [child allFilesDo: aBlock matches: selectionBlock]]
</details>

## FileEntry

I represent a single file entry. You can write data by #fileContents: , and read the data by #fileContents. --- mu 11/6/2006 20:21 -------------- See examples class category. See DirectoryEntry. See categories starting with '*fileman-' in String. Smalltalk imageName asFileEntry fileSize Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]

### Methods
#### FileEntry>>#textContents: aString

<details>
	<summary>See more</summary>
	
	textContents: aString
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream to: aString ].
	self invalidate
</details>

#### FileEntry>>#rename: newName

<details>
	<summary>See more</summary>
	
	rename: newName
	
	self fileAccessor rename: self pathName to: (self parent // newName) pathName.
	self name: newName
</details>

#### FileEntry>>#copyTo: filename

<details>
	<summary>See more</summary>
	
	copyTo: filename 
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	targetEntry isDirectory
		ifTrue: [ targetEntry := targetEntry // self name ].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry
</details>

#### FileEntry>>#baseDirectory

The directory this file is located in


<details>
	<summary>See more</summary>
	
	baseDirectory
	"The directory this file is located in"
	^ DirectoryEntry
		withPathComponents: self pathComponents allButLast
		drive: nil.
</details>

#### FileEntry>>#appendContents: aStringOrBytes

<details>
	<summary>See more</summary>
	
	appendContents: aStringOrBytes 
	self
		appendStreamDo: [:str | 
			aStringOrBytes isString
				ifFalse: [str binary].
			str nextPutAll: aStringOrBytes]
</details>

#### FileEntry>>#binaryContents

<details>
	<summary>See more</summary>
	
	binaryContents
	| answer |
	self readStreamDo: [ :stream |
		answer _ stream binary contents ].
	^ answer
</details>

#### FileEntry>>#formContents

<details>
	<summary>See more</summary>
	
	formContents
	^Form fromFileEntry: self
</details>

#### FileEntry>>#isFile

<details>
	<summary>See more</summary>
	
	isFile
	^true
</details>

#### FileEntry>>#delete

<details>
	<summary>See more</summary>
	
	delete
	self fileAccessor deleteFile: self pathName.
	
</details>

#### FileEntry>>#writeStreamDo: blockWithArg

If the file already exists raise FileExistsException. Creates the directory if it doesn't exist.


<details>
	<summary>See more</summary>
	
	writeStreamDo: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self writeStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]
</details>

#### FileEntry>>#fileContents

Default is text mode


<details>
	<summary>See more</summary>
	
	fileContents
	"Default is text mode"
	^self textContents
</details>

#### FileEntry>>#pipe: filterBlock to: filename

<details>
	<summary>See more</summary>
	
	pipe: filterBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	filterBlock value: inStream value: outStream]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry

</details>

#### FileEntry>>#readStreamDo: blockWithArg

Raise FileDoesNotExistException if not found.


<details>
	<summary>See more</summary>
	
	readStreamDo: blockWithArg 
	"Raise FileDoesNotExistException if not found."
	| stream result |
	stream _ self readStream.
	[ result _ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]].
	^ result
</details>

#### FileEntry>>#assureExistence

<details>
	<summary>See more</summary>
	
	assureExistence
	self exists ifTrue: [^self].
	self parent assureExistence.
	self forceWriteStreamDo: [ :stream | ]
</details>

#### FileEntry>>#pipeRepeat: filterBlock to: filename

<details>
	<summary>See more</summary>
	
	pipeRepeat: filterBlock to: filename 
	^self pipeRepeat: filterBlock while: [:in :out | in atEnd not] to: filename 
</details>

#### FileEntry>>#invalidate

Assume we know nothing about current state in the File System. This might be because we're just created. Or it might be because there is a chance the File System changed and we don't know current state.


<details>
	<summary>See more</summary>
	
	invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	super invalidate.
	fileSize _ nil
</details>

#### FileEntry>>#< aStringOrBytes

<details>
	<summary>See more</summary>
	
	< aStringOrBytes 
	self fileContents: aStringOrBytes
</details>

#### FileEntry>>#fileSize

<details>
	<summary>See more</summary>
	
	fileSize

	self invalidateIfOld.

	"Slow version."
	"fileSize ifNil: [self fileAccessor updateEntry: self]."

	"Fast version, that asks just for the size of this file.
	Used if I was not created by reading a direcotry"
	fileSize ifNil: [
		fileSize _ self fileAccessor fileSize: self.
		exists _ fileSize notNil ].

	^fileSize
</details>

#### FileEntry>>#appendStream

Note: You need to eventually close the stream. Usually prefer #appendStreamDo: that closes the file for you.


<details>
	<summary>See more</summary>
	
	appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamDo: that closes the file for you."

	self exists ifFalse: [
		^ self writeStream ].
	^ (self fileAccessor privateWriteableFile: self) setToEnd
</details>

#### FileEntry>>#textContents

<details>
	<summary>See more</summary>
	
	textContents
	| answer |
	self readStreamDo: [ :stream |
		answer _ stream contents ].
	^ answer
</details>

#### FileEntry>>#updateFrom: primitiveArray entryInParent: index

<details>
	<summary>See more</summary>
	
	updateFrom: primitiveArray entryInParent: index
	super updateFrom: primitiveArray entryInParent: index.
	fileSize _ primitiveArray at: 5
</details>

#### FileEntry>>#fileContents: aStringOrBytes

<details>
	<summary>See more</summary>
	
	fileContents: aStringOrBytes 
	aStringOrBytes isString
		ifTrue: [self textContents: aStringOrBytes]
		ifFalse: [self binaryContents: aStringOrBytes]
</details>

#### FileEntry>>#<< aStringOrBytes

<details>
	<summary>See more</summary>
	
	<< aStringOrBytes 
	self appendContents: aStringOrBytes
</details>

#### FileEntry>>#pipeRepeat: filterBlock while: terminateBlock to: filename

<details>
	<summary>See more</summary>
	
	pipeRepeat: filterBlock while: terminateBlock to: filename 
	| nextEntry inStream outStream |
	nextEntry := filename asFileEntry.
	[inStream := self readStream.
	outStream := nextEntry writeStream.
	[terminateBlock value: inStream value: outStream]
		whileTrue: [filterBlock value: inStream value: outStream]]
		ensure: [
			inStream close.
			outStream close].
	^nextEntry
</details>

#### FileEntry>>#forceWriteStreamDo: blockWithArg

If the file already exists, delete it first without asking. Do not raise FileExistsException. Creates the directory if it doesn't exist.


<details>
	<summary>See more</summary>
	
	forceWriteStreamDo: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream _ self forceWriteStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]
</details>

#### FileEntry>>#updateExists

<details>
	<summary>See more</summary>
	
	updateExists

	self fileSize "Updates both"
</details>

#### FileEntry>>#appendStreamDo: blockWithArg

<details>
	<summary>See more</summary>
	
	appendStreamDo: blockWithArg 
	| stream |
	stream _ self appendStream.
	[ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]
</details>

#### FileEntry>>#forceWriteStream

If the file already exists, delete it first without asking. Do not raise FileExistsException. Note: You need to eventually close the stream. Usually prefer #forceWriteStreamDo: that closes the file for you. Creates the directory if it doesn't exist.


<details>
	<summary>See more</summary>
	
	forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateForceNewFile: self
</details>

#### FileEntry>>#writeStream

If the file already exists raise FileExistsException. Note: You need to eventually close the stream. Usually prefer #writeStreamDo: that closes the file for you. Creates the directory if it doesn't exist.


<details>
	<summary>See more</summary>
	
	writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamDo: that closes the file for you.
	Creates the directory if it doesn't exist."

	self invalidate.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor privateNewFile: self
</details>

#### FileEntry>>#primEntryInParent

<details>
	<summary>See more</summary>
	
	primEntryInParent
	^ primEntryInParent
</details>

#### FileEntry>>#readStream

Raise FileDoesNotExistException if not found. Note: You need to eventually close the stream. Usually prefer #readStreamDo: that closes the file for you.


<details>
	<summary>See more</summary>
	
	readStream
	"Raise FileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStreamDo: that closes the file for you."

	^ self fileAccessor privateReadOnlyFile: self
</details>

#### FileEntry>>#binaryContents: aByteArray

<details>
	<summary>See more</summary>
	
	binaryContents: aByteArray
	self forceWriteStreamDo: [ :stream |
		self setContentsOf: stream binary to: aByteArray ].
	self invalidate
</details>

## FileIOAccessor

I am an accessor to the low level file IO. You can extend/rewrite me if you port FileMan to other Smalltalk dialects. --- mu 3/13/2007 11:11

### Methods
#### FileIOAccessor>>#pathNameDelimiter

Given that FileMan supports $/ as path delimiter regardless of the platform, this method is mostly for FileMan's own use, and general usage is discouraged. Just use $/


<details>
	<summary>See more</summary>
	
	pathNameDelimiter
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use $/ "
	^self primPathNameDelimiter
</details>

#### FileIOAccessor>>#slash

Given that FileMan supports $/ as path delimiter regardless of the platform, this method is mostly for FileMan's own use, and general usage is discouraged. Just use '/'


<details>
	<summary>See more</summary>
	
	slash
	"Given that FileMan supports $/ as path delimiter regardless of the platform,
	this method is mostly for FileMan's own use, and general usage is discouraged.
	Just use '/' "
	slash ifNil: [ slash _ self pathNameDelimiter asString ].
	^slash
</details>

#### FileIOAccessor>>#isDriveSupported

<details>
	<summary>See more</summary>
	
	isDriveSupported
	^self onWindows or: [self onMacClassic]
</details>

#### FileIOAccessor>>#deleteFile: fullPathName ifAbsent: failBlock

<details>
	<summary>See more</summary>
	
	deleteFile: fullPathName ifAbsent: failBlock 
	^(self
			try: [self primDeleteFileNamed: fullPathName]
			forFileNamed: fullPathName) 
		ifFalse: [^ failBlock value]
</details>

#### FileIOAccessor>>#privateWriteableFile: aFileEntry

Open the file with the given name in this directory for writing.


<details>
	<summary>See more</summary>
	
	privateWriteableFile: aFileEntry
	"Open the file with the given name in this directory for writing."

	| pathName |
	pathName _ aFileEntry pathName.
	^ (self concreteStreamClass new open: pathName forWrite: true)
		ifNil: [
			"Failed to open the file"
			(FileWriteError fileName: pathName)
				signal: ('File [', pathName, '] open for write failed' ) ]
</details>

#### FileIOAccessor>>#renameDirectory: oldFileFullName to: newFileFullName

<details>
	<summary>See more</summary>
	
	renameDirectory: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asDirectoryEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	newFileFullName asDirectoryEntry exists
		ifTrue: [selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a directory to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [newFileFullName asDirectoryEntry recursiveDelete.
					^ self renameDirectory: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'
</details>

#### FileIOAccessor>>#absolutePathComponentsFor: aString

Not complete, but in most cases it is OK See comment at #isAbsolutePathName


<details>
	<summary>See more</summary>
	
	absolutePathComponentsFor: aString
	"Not complete, but in most cases it is OK
	See comment at #isAbsolutePathName"

	| tokens curDirPathComponents |
	tokens _ aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens asArray ].

	curDirPathComponents _ DirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents _ curDirPathComponents allButLast.
		tokens removeFirst ].

	^ Array streamContents: [ :strm |
		strm nextPutAll: curDirPathComponents.
		tokens do: [ :each |
			each = '.' ifFalse: [ strm nextPut: each ]]]
</details>

#### FileIOAccessor>>#fileOrDirectoryExists: localName in: aDirectoryEntry

<details>
	<summary>See more</summary>
	
	fileOrDirectoryExists: localName in: aDirectoryEntry

	| entryNames |
	entryNames := self entryNamesIn: aDirectoryEntry.

	^self isCaseSensitive 
		ifTrue:[entryNames includes: localName]
		ifFalse:[entryNames anySatisfy: [:name| name sameAs: localName]].
</details>

#### FileIOAccessor>>#fileSize: fileEntry

<details>
	<summary>See more</summary>
	
	fileSize: fileEntry

	| pathName f size |
	pathName _ fileEntry pathName.
	"At least on Linux 64 Cog, opening a directory as a stream and asking #size answers some absurd number: 9223372036854775807"
	(self basicDirectoryExists: pathName) ifTrue: [^ nil ].
	f _ self concreteStreamClass new open: pathName forWrite: false.
	f ifNil: [^ nil].
	size _ f size.
	f close.
	^ size
</details>

#### FileIOAccessor>>#deleteFile: fullPathName

<details>
	<summary>See more</summary>
	
	deleteFile: fullPathName
	^self deleteFile: fullPathName ifAbsent: []
</details>

#### FileIOAccessor>>#rename: oldFileFullName to: newFileFullName

<details>
	<summary>See more</summary>
	
	rename: oldFileFullName to: newFileFullName 
	| selection |
	(self try: [self primRename: oldFileFullName to: newFileFullName]
			forFileNamed: oldFileFullName) ifTrue: [^ self].

	oldFileFullName asFileEntry exists ifFalse: [^ self error: 'Attempt to rename a non-existent file'].
	(newFileFullName asFileEntry exists or: [ newFileFullName asDirectoryEntry exists ])
		ifTrue: [
			selection := (PopUpMenu labels: 'delete old version
cancel')
						startUpWithCaption: 'Trying to rename a file to be
' , newFileFullName , '
and it already exists.'.
			selection = 1
				ifTrue: [self deleteFile: newFileFullName.
					^ self rename: oldFileFullName to: newFileFullName]].
	^ self error: 'Failed to rename file'
</details>

#### FileIOAccessor>>#isCaseSensitive

FileIOAccessor default isCaseSensitive


<details>
	<summary>See more</summary>
	
	isCaseSensitive
	"FileIOAccessor default isCaseSensitive"
	^self onUnix
</details>

#### FileIOAccessor>>#onWindows

<details>
	<summary>See more</summary>
	
	onWindows
	^self pathNameDelimiter = $\
</details>

#### FileIOAccessor>>#drives

Answer a collection of Strings FileIOAccessor default drives


<details>
	<summary>See more</summary>
	
	drives
	"
	Answer a collection of Strings
	FileIOAccessor default drives
	"
	drives _ nil. 		"will change if you mount or unmount drives!"
	drives ifNil: [
		drives _ self onUnix
			ifTrue: [ #() ]
			ifFalse: [ (self entriesIn: nil)]].
	^drives
</details>

#### FileIOAccessor>>#try: execBlock forFileNamed: fullName

If fail, return nil


<details>
	<summary>See more</summary>
	
	try: execBlock forFileNamed: fullName

	"If fail, return nil"

	^ (self concreteStreamClass retryWithGC: execBlock until: [:result | result notNil] forFileNamed: fullName) notNil
</details>

#### FileIOAccessor>>#extensionFor: pathName

In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis. FileIOAccessor default extensionFor: 'writings.txt' FileIOAccessor default extensionFor: 'folder.ext/file' FileIOAccessor default extensionFor: 'optionalstuff.pck.st' FileIOAccessor default extensionFor: 'code.cs.st' FileIOAccessor default extensionFor: 'code.cs'


<details>
	<summary>See more</summary>
	
	extensionFor: pathName 
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.
	
	FileIOAccessor default extensionFor: 'writings.txt'
	FileIOAccessor default extensionFor: 'folder.ext/file'
	FileIOAccessor default extensionFor: 'optionalstuff.pck.st'
	FileIOAccessor default extensionFor: 'code.cs.st'
	FileIOAccessor default extensionFor: 'code.cs'
	"
	| index |
	{ '.cs.st' . '.pck.st' } do: [ :specialExtension |
		(pathName endsWith: specialExtension)
			ifTrue: [ ^specialExtension copyFrom: 2 to: specialExtension size ]].
	index _ pathName
				findLast: [ :c | c = $.].
	^ (index = 0 or: [ pathName indexOfLastPathSeparator > index ])
		ifTrue: ['']
		ifFalse: [pathName copyFrom: index + 1 to: pathName size]
</details>

#### FileIOAccessor>>#primDeleteDirectory: fullPath

Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist.


<details>
	<summary>See more</summary>
	
	primDeleteDirectory: fullPath
	"Delete the directory named by the given path. Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### FileIOAccessor>>#concreteStreamClass

<details>
	<summary>See more</summary>
	
	concreteStreamClass
	^FileStream concreteStream
</details>

#### FileIOAccessor>>#primDeleteFileNamed: aFileName

Delete the file of the given name. Return self if the primitive succeeds, nil otherwise.


<details>
	<summary>See more</summary>
	
	primDeleteFileNamed: aFileName
	"Delete the file of the given name. Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil

</details>

#### FileIOAccessor>>#primLookupEntryIn: fullPath index: index

Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing: <name> <creationTime> <modificationTime> <dirFlag> <fileSize> On MacOS and Windows, the empty string enumerates the mounted volumes/drives. On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory. The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad.


<details>
	<summary>See more</summary>
	
	primLookupEntryIn: fullPath index: index
	"Look up the index-th entry of the directory with the given fully-qualified path (i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	On MacOS and Windows,  the empty string enumerates the mounted volumes/drives.
	
	On Linux, it is equivalent to '.', and lists the contents of DirectoryEntry currentDirectory.

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. The primitive returns nil when index is past the end of the directory. It fails if the given path is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin'>
	^ #badDirectoryPath


</details>

#### FileIOAccessor>>#updateEntry: aFileSystemEntry

If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries.


<details>
	<summary>See more</summary>
	
	updateEntry: aFileSystemEntry
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primLookupEntryIn: aFileSystemEntry parent pathName index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn _ aFileSystemEntry parent pathName.
	index _ 1.
	[
		entryArray _ self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ self].
		entryArray == nil ifTrue: [
			^ self].
		isDirectory _ entryArray at: 4.
		aFileSystemEntry name = (entryArray at: 1) ifTrue: [
			isDirectory == aFileSystemEntry isDirectory ifTrue: [
				aFileSystemEntry updateFrom: entryArray entryInParent: index ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index _ index + 1] repeat
</details>

#### FileIOAccessor>>#createDirectory: fullPathName

<details>
	<summary>See more</summary>
	
	createDirectory: fullPathName
	^self primCreateDirectory: fullPathName
</details>

#### FileIOAccessor>>#basicDirectoryExists: fullPathName

<details>
	<summary>See more</summary>
	
	basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
 	^(result == #badDirectoryPath) not
</details>

#### FileIOAccessor>>#copy: fromFileEntry to: toFileEntry

<details>
	<summary>See more</summary>
	
	copy: fromFileEntry to: toFileEntry 
	| readStr writeStr |
	[readStr := (self privateReadOnlyFile: fromFileEntry) binary.
	writeStr := (self privateForceNewFile: toFileEntry) binary.
	self copyFile: readStr toFile: writeStr]
		ensure: [
			readStr
				ifNotNil: [ :r | r close ].
			writeStr
				ifNotNil: [ :w | w close ]]
</details>

#### FileIOAccessor>>#onUnix

<details>
	<summary>See more</summary>
	
	onUnix
	^self pathNameDelimiter = $/
</details>

#### FileIOAccessor>>#directoryNamesIn: aDirectoryEntry

FileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry


<details>
	<summary>See more</summary>
	
	directoryNamesIn: aDirectoryEntry
	"
	FileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]
</details>

#### FileIOAccessor>>#splitNameVersionExtensionFor: fileName

answer an array with the root name, version # and extension. See comment in senders for more details


<details>
	<summary>See more</summary>
	
	splitNameVersionExtensionFor: fileName
	" answer an array with the root name, version # and extension.
	See comment in senders for more details"

	| baseName version i j |
	self baseNameAndExtensionFor: fileName do: [ :b :extension |
		baseName _ b.
		i := j := baseName findLast: [:c | c isDigit not].
		i = 0
			ifTrue: [version := 0]
			ifFalse: [
				(baseName at: i) = $.
					ifTrue: [
						version := (baseName copyFrom: i+1 to: baseName size) asNumber.
						j := j - 1]
					ifFalse: [version := 0].
				baseName := baseName copyFrom: 1 to: j ].
		^ Array with: baseName with: version with: extension ]
</details>

#### FileIOAccessor>>#primPathNameDelimiter

Return the path delimiter for the underlying platform's file system.


<details>
	<summary>See more</summary>
	
	primPathNameDelimiter
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### FileIOAccessor>>#checkName: aFileName fixErrors: fixing

Check if the file name contains any invalid characters


<details>
	<summary>See more</summary>
	
	checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| badChars hasBadChars |
	badChars _ #( $: $< $> $| $/ $\ $? $* $") asSet.
	hasBadChars _ aFileName includesAnyOf: badChars.
	(hasBadChars and:[fixing not]) ifTrue: [^self error:'Invalid file name'].
	hasBadChars ifFalse:[^ aFileName].
	^ aFileName collect: [ :char |
			(badChars includes: char) 
				ifTrue:[$#] 
				ifFalse:[char]]
</details>

#### FileIOAccessor>>#privateForceNewFile: aFileEntry

Open the file with the given name in this directory for writing. If it already exists, delete it first without asking.


<details>
	<summary>See more</summary>
	
	privateForceNewFile: aFileEntry
	"Open the file with the given name in this directory for writing.  If it already exists, delete it first without asking."

	| pathName |
	pathName _ aFileEntry pathName.
	aFileEntry exists
		ifTrue: [
			self deleteFile: pathName ifAbsent: [
				(CannotDeleteFileException new
					messageText: 'Could not delete the old version of file ' , pathName) signal]].

	^ self privateWriteableFile: aFileEntry
</details>

#### FileIOAccessor>>#containingDirectoryPathOf: pathName

<details>
	<summary>See more</summary>
	
	containingDirectoryPathOf: pathName 

	(((pathName isNil
			or: [pathName isEmpty])
			or: [pathName isPathSeparator])
			or: [pathName isDriveName])
		ifTrue: [^ nil].
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1
</details>

#### FileIOAccessor>>#fileNamesIn: aDirectoryEntry

FileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry


<details>
	<summary>See more</summary>
	
	fileNamesIn: aDirectoryEntry
	"
	FileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^((self entriesIn: aDirectoryEntry)
		reject: [ :each | each isDirectory ])
		collect: [ :each | each name ]
</details>

#### FileIOAccessor>>#onMacOsX

<details>
	<summary>See more</summary>
	
	onMacOsX
	^self onUnix and: [Smalltalk platformName = 'Mac OS']
</details>

#### FileIOAccessor>>#primCreateDirectory: fullPath

Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists.


<details>
	<summary>See more</summary>
	
	primCreateDirectory: fullPath
	"Create a directory named by the given path. Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	self primitiveFailed

</details>

#### FileIOAccessor>>#privateReadOnlyFile: fileEntry

Open the existing file with the given name in this directory for read-only access.


<details>
	<summary>See more</summary>
	
	privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	(self basicDirectoryExists: pathName) ifTrue: [
		"If it is a directory, the it is not a file, and the requested file does not exist."
		^ ((FileDoesNotExistException fileName: pathName) readOnly: true) signal ].
	^(self concreteStreamClass new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]
</details>

#### FileIOAccessor>>#entriesIn: parentEntryOrNil

Warning: Private. Only to be called from within FileMan. Accepts nil as argument, but behavior depends on platform. Windows (nil means root) FileIOAccessor default entriesIn: nil #(C:\ D:\) (FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows) Linux (nil means current dir, like '' and '.') FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory) (FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true (FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media) MacOsX (nil means current dir, like '' and '.') FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources) (FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)


<details>
	<summary>See more</summary>
	
	entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory lookIn |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray _ self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done _ true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				entry name: (entryArray at: 1) parent: parentEntryOrNil.
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index _ index + 1].

	^entries asArray
</details>

#### FileIOAccessor>>#deleteDirectory: fullPathName

<details>
	<summary>See more</summary>
	
	deleteDirectory: fullPathName
	^self primDeleteDirectory: fullPathName
</details>

#### FileIOAccessor>>#primRename: oldFileFullName to: newFileFullName

Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name. Changed to return nil instead of failing ar 3/21/98 18:04


<details>
	<summary>See more</summary>
	
	primRename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.
	Changed to return nil instead of failing ar 3/21/98 18:04"

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil
</details>

#### FileIOAccessor>>#privateNewFile: aFileEntry

Create a new file with the given full pathName.


<details>
	<summary>See more</summary>
	
	privateNewFile: aFileEntry
	"Create a new file with the given full pathName."

	^aFileEntry exists
		ifTrue: [
			(FileExistsException fileName: aFileEntry pathName fileClass: self concreteStreamClass) signal]
		ifFalse: [
			self privateWriteableFile: aFileEntry ]
</details>

#### FileIOAccessor>>#copyFile: fileStream1 toFile: fileStream2

<details>
	<summary>See more</summary>
	
	copyFile: fileStream1 toFile: fileStream2
	| buffer |
	buffer := String new: 50000.
	[fileStream1 atEnd] whileFalse:
		[fileStream2 nextPutAll: (fileStream1 nextInto: buffer)].

</details>

#### FileIOAccessor>>#baseNameAndExtensionFor: pathName do: aBlock

In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.


<details>
	<summary>See more</summary>
	
	baseNameAndExtensionFor: pathName do: aBlock
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	| extension |
	extension _ self extensionFor: pathName.
	extension isEmpty ifTrue: [
		^ aBlock value: pathName value: '' ].
	^ aBlock value: (pathName copyFrom: 1 to: pathName size - extension size - 1) value: extension
</details>

#### FileIOAccessor>>#baseNameFor: pathName

In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis.


<details>
	<summary>See more</summary>
	
	baseNameFor: pathName
	"In original FileMan, accepts only a localName (withouth path separators). Modify it for Cuis for also allowing them, as it is traditional in Squeak / Cuis."
	"Return the given file name without its extension, if any. We have to remember that many (most?) OSs allow extension separators within directory names and so the leaf filename needs to be extracted, trimmed and rejoined. Yuck"
	"The test is 
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim.blam') pathName)
		should end 'foo.bar/blim' (or as appropriate for your platform AND
		
		FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory / 'foo.bar' / 'blim') pathName)
		should be the same and NOT  'foo'
		
		Oh, and
		FileIOAccessor default baseNameFor: 'foo.bar'
		should be 'foo' not '/foo' "

	self baseNameAndExtensionFor: pathName do: [ :baseName :extension |
		^baseName ]
</details>

#### FileIOAccessor>>#onMacClassic

<details>
	<summary>See more</summary>
	
	onMacClassic
	^self pathNameDelimiter = $:
</details>

#### FileIOAccessor>>#entryNamesIn: aDirectoryEntry

FileIOAccessor default entryNamesIn: 'C:\Windows\' asDirectoryEntry


<details>
	<summary>See more</summary>
	
	entryNamesIn: aDirectoryEntry
	"
	FileIOAccessor default entryNamesIn: 'C:\Windows\' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry) collect: [ :each | each name]
</details>

## FileSystemEntry

I represent a single file entry (including directory). You can write data by #fileContents: , and read the data by #fileContents. --- mu 11/6/2006 20:21 -------------- See examples class category. See DirectoryEntry. See categories starting with '*fileman-' in String. Smalltalk imageName asFileEntry fileSize Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]

### Methods
#### FileSystemEntry>>#pathComponents

<details>
	<summary>See more</summary>
	
	pathComponents
	pathComponents ifNil: [pathComponents _ #() ].
	^pathComponents
</details>

#### FileSystemEntry>>#exists

<details>
	<summary>See more</summary>
	
	exists
	self invalidateIfOld.
	exists ifNil: [self updateExists].
	^exists
</details>

#### FileSystemEntry>>#invalidateIfOld

<details>
	<summary>See more</summary>
	
	invalidateIfOld

	lastSync isNil ifTrue: [
		^ self invalidate ].
	(DateAndTime now - lastSync) totalSeconds > 2 ifTrue: [
		self invalidate ]
</details>

#### FileSystemEntry>>#setParent: aDirectoryEntry

<details>
	<summary>See more</summary>
	
	setParent: aDirectoryEntry
	parent := aDirectoryEntry
</details>

#### FileSystemEntry>>#isFile

<details>
	<summary>See more</summary>
	
	isFile
	^false
</details>

#### FileSystemEntry>>#= aFileEntry

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= aFileEntry
	| isCaseSensitive myDrive otherDrive theirs mine |

	self == aFileEntry ifTrue: [ ^ true ].
	self class == aFileEntry class ifFalse: [ ^false ].

	isCaseSensitive _ self fileAccessor isCaseSensitive.

	"Check for drive nil or same."
	myDrive _ self drive.
	otherDrive _ aFileEntry drive.
	isCaseSensitive
		ifTrue: [ self drive = aFileEntry drive ifFalse: [ ^false ]]
		ifFalse: [
			myDrive isNil = otherDrive isNil ifFalse: [ ^false ].		"only one of them is nil"
			myDrive ifNotNil: [											"none is nil"
				(myDrive sameAs: otherDrive) ifFalse: [ ^false ]]].

	"Check for all path components same."
	mine _ self pathComponents.
	theirs _ aFileEntry pathComponents.

	isCaseSensitive ifTrue: [
		^mine = theirs ].

	mine size = theirs size ifFalse: [
		^false ].

	mine with: theirs do: [ :m :t |
		(m sameAs: t) ifFalse: [ ^false ]].

	^ true
</details>

#### FileSystemEntry>>#creationTime

<details>
	<summary>See more</summary>
	
	creationTime
	self invalidateIfOld.
	creationTime ifNil: [self fileAccessor updateEntry: self].
	^creationTime
</details>

#### FileSystemEntry>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^self pathComponents hash
</details>

#### FileSystemEntry>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	self printPathOn: aStream
</details>

#### FileSystemEntry>>#baseName

<details>
	<summary>See more</summary>
	
	baseName
	^self fileAccessor baseNameFor: name
</details>

#### FileSystemEntry>>#name: aString

<details>
	<summary>See more</summary>
	
	name: aString 
	name := aString.
	self pathComponents
		ifNotEmpty: [self pathComponents at: self pathComponents size put: name]
</details>

#### FileSystemEntry>>#pathName

<details>
	<summary>See more</summary>
	
	pathName

	^ String streamContents: [ :stream |
		self printPathOn: stream ]
</details>

#### FileSystemEntry>>#name: aString parent: parentEntryOrNil

<details>
	<summary>See more</summary>
	
	name: aString parent: parentEntryOrNil
	name _ aString.
	parentEntryOrNil
		ifNil: [
			self pathString: aString ]
		ifNotNil: [
			parent _ parentEntryOrNil.
			drive _ parentEntryOrNil drive. "harmless if no drive supported, as in Unix"
			pathComponents _ parentEntryOrNil pathComponents copyWith: name ].
	self invalidate
</details>

#### FileSystemEntry>>#pathString: aString

<details>
	<summary>See more</summary>
	
	pathString: aString
	| tokens |
	tokens _ FileIOAccessor default absolutePathComponentsFor: aString.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			tokens first asDriveName ifNotNil: [ :guessedDriveName |
				^ self pathComponents: (tokens copyFrom: 2 to: tokens size) drive: guessedDriveName ]].
	self pathComponents: tokens drive: nil
</details>

#### FileSystemEntry>>#invalidate

Assume we know nothing about current state in the File System. This might be because we're just created. Or it might be because there is a chance the File System changed and we don't know current state.


<details>
	<summary>See more</summary>
	
	invalidate
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	lastSync _ nil.
	exists _ nil.
	creationTime _ nil.
	modificationTime _ nil.
</details>

#### FileSystemEntry>>#isDirectory

<details>
	<summary>See more</summary>
	
	isDirectory
	^false
</details>

#### FileSystemEntry>>#fileAccessor

<details>
	<summary>See more</summary>
	
	fileAccessor

	^FileIOAccessor default
</details>

#### FileSystemEntry>>#nameWithoutExtension

'writings.txt' asFileEntry nameWithoutExtension 'folder.ext/writings.txt' asFileEntry nameWithoutExtension 'folder.ext/writings' asFileEntry nameWithoutExtension


<details>
	<summary>See more</summary>
	
	nameWithoutExtension
	"
	'writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings.txt' asFileEntry nameWithoutExtension
	'folder.ext/writings' asFileEntry nameWithoutExtension
	"
	^self fileAccessor baseNameFor: name
</details>

#### FileSystemEntry>>#nameVersionExtension

<details>
	<summary>See more</summary>
	
	nameVersionExtension
	^self fileAccessor splitNameVersionExtensionFor: self name
</details>

#### FileSystemEntry>>#ifExists: aBlock

Evaluate a block with receiver as argument if it exists on the file system. If not, do nothing.


<details>
	<summary>See more</summary>
	
	ifExists: aBlock
	"Evaluate a block with receiver as argument if it exists on the file system. If not, do nothing."
	self exists ifTrue: [
		aBlock value: self ]
</details>

#### FileSystemEntry>>#updateFrom: primitiveArray entryInParent: index

<details>
	<summary>See more</summary>
	
	updateFrom: primitiveArray entryInParent: index
	primEntryInParent _ index.
	lastSync _ DateAndTime now.
	exists _ true.
	creationTime _ DateAndTime fromSeconds: (primitiveArray at: 2).
	modificationTime _ DateAndTime fromSeconds: (primitiveArray at: 3)
</details>

#### FileSystemEntry>>#drive

<details>
	<summary>See more</summary>
	
	drive
	self fileAccessor onUnix ifTrue: [^ drive := nil].
	^ drive
</details>

#### FileSystemEntry>>#parent

<details>
	<summary>See more</summary>
	
	parent
	parent ifNil: [parent := self ensureParent].
	^parent
</details>

#### FileSystemEntry>>#extension

<details>
	<summary>See more</summary>
	
	extension
	^self fileAccessor extensionFor: name
</details>

#### FileSystemEntry>>#drive: aString

<details>
	<summary>See more</summary>
	
	drive: aString
	drive := aString
</details>

#### FileSystemEntry>>#pathComponents: tokens drive: driveStringOrNil

<details>
	<summary>See more</summary>
	
	pathComponents: tokens drive: driveStringOrNil
	| firstToken  |
	tokens isEmptyOrNil ifTrue: [ ^pathComponents _ nil ].
	(driveStringOrNil isNil and: [ (firstToken _ tokens first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			pathComponents _ tokens copyFrom: 2 to: tokens size ]
		ifFalse: [
			self drive: driveStringOrNil.
			pathComponents _ tokens ].

	pathComponents ifNotEmpty: [ name _ pathComponents last ].
	self invalidate
</details>

#### FileSystemEntry>>#ensureParent

<details>
	<summary>See more</summary>
	
	ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent _ DirectoryEntry
				withPathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent
</details>

#### FileSystemEntry>>#printPathOn: aStream

<details>
	<summary>See more</summary>
	
	printPathOn: aStream 
	self drive
		ifNotNil: [:d | aStream nextPutAll: d].
	aStream nextPutAll: self fileAccessor slash.
	self pathComponents
		do: [:each | aStream nextPutAll: each]
		separatedBy: [aStream nextPutAll: self fileAccessor slash]
</details>

#### FileSystemEntry>>#isRoot

<details>
	<summary>See more</summary>
	
	isRoot
	^self parent isNil
</details>

#### FileSystemEntry>>#version

<details>
	<summary>See more</summary>
	
	version
	^self nameVersionExtension second
</details>

#### FileSystemEntry>>#setContentsOf: aStream to: aStringOrBytes

<details>
	<summary>See more</summary>
	
	setContentsOf: aStream to: aStringOrBytes

	aStringOrBytes isString
		ifFalse: [ aStream binary].
	aStream nextPutAll: aStringOrBytes
</details>

#### FileSystemEntry>>#modificationTime

<details>
	<summary>See more</summary>
	
	modificationTime
	self invalidateIfOld.
	modificationTime ifNil: [self fileAccessor updateEntry: self].
	^modificationTime
</details>

#### FileSystemEntry>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^name ifNil: [ drive ]
</details>

#### FileSystemEntry>>#parents

<details>
	<summary>See more</summary>
	
	parents
	| ord par |
	par := self parent.
	ord := OrderedCollection with: par.
	[par isRoot] whileFalse: [
		par := par parent.
		ord add: par.
	].
	^ord
</details>

