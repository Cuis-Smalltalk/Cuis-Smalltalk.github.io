## FileList

I am model that can be used to navigate the host file system. By omitting the volume list, file list, and template panes from the view, I can also be used as the model for an editor on an individual file. The FileList provides a dynamic extension mechanism. To extend FileList functionality, tools should implement the following class-side method (look for implementors in the image): #fileReaderServicesForFile:suffix: (appropriate services for given file, takes a file name and a lowercased suffix) This method returns a collection of SimpleServiceEntry instances. These contain a class, a menu label and a method selector having one argument. They may also provide separate button labels and description. The argument to the specified method will be a string representing the full name of a file when one is selected or the file list itself when there is no selected file.

### Methods
#### FileList>>#isFileSmalltalkCode

<details>
	<summary>See more</summary>
	
	isFileSmalltalkCode
	^fileName
		ifNil: [ false ]
		ifNotNil: [ fileName endsWith: '.st' ]
</details>

#### FileList>>#universalButtonServices

Answer a list of services underlying the universal buttons in their initial inception. For the moment, only the sorting buttons are shown.


<details>
	<summary>See more</summary>
	
	universalButtonServices
	"Answer a list of services underlying the universal buttons in their initial inception.  For the moment, only the sorting buttons are shown."

	^ {self serviceSortByName. self serviceSortByDate. self serviceSortBySize}
</details>

#### FileList>>#itemsForNoFile

<details>
	<summary>See more</summary>
	
	itemsForNoFile

	| services |
	services := OrderedCollection new: 6.
	services add: self serviceSortByName.
	services add: self serviceSortBySize.
	services add: (self serviceSortByDate useLineAfter: true).
	services add: self serviceAddNewFile.
	services add: self serviceAddNewDirectory.
	^ services

		
</details>

#### FileList>>#fileListIndex

Answer the index of the currently selected file.


<details>
	<summary>See more</summary>
	
	fileListIndex
	"Answer the index of the currently selected file."

	^ listIndex
</details>

#### FileList>>#initialDirectoryList

<details>
	<summary>See more</summary>
	
	initialDirectoryList

	| dirList |
	dirList _ DirectoryEntry roots collect: [ :each |
		FileDirectoryWrapper with: each name: (each name ifNil: ['/']) model: self].
	dirList isEmpty ifTrue: [
		dirList _ Array with: (FileDirectoryWrapper 
			with: directory
			name: directory localName 
			model: self)].
	^dirList
</details>

#### FileList>>#fileName

<details>
	<summary>See more</summary>
	
	fileName

	^ fileName
</details>

#### FileList>>#serviceDeleteFile

<details>
	<summary>See more</summary>
	
	serviceDeleteFile

	^ SimpleServiceEntry
		provider: self
		label: 'delete (x)'
		selector: #deleteFile
		description: 'delete the seleted item'
		icon: #deleteIcon
</details>

#### FileList>>#sortingByName

<details>
	<summary>See more</summary>
	
	sortingByName
	^ sortMode == #name
</details>

#### FileList>>#serviceRenameFile

<details>
	<summary>See more</summary>
	
	serviceRenameFile

	^ SimpleServiceEntry
		provider: self
		label: 'rename (R)'
		selector: #renameFile
		description: 'rename file'
		icon: #saveAsIcon
</details>

#### FileList>>#readContentsBrief: brevityFlag

Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist.


<details>
	<summary>See more</summary>
	
	readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first50000 |

directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= 2000000]) ifTrue: [
		acceptedContentsCache _ f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	first50000 := f next: 50000.
].
	acceptedContentsCache _
'File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 50000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 50000 characters.' format: {fileName. fileSize. first50000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache
</details>

#### FileList>>#getHex

Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel.


<details>
	<summary>See more</summary>
	
	getHex
	"Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState _ #needToGetBriefHex.
	self acceptedContentsChanged
</details>

#### FileList>>#dynamicButtonServices

Answer services for buttons that may come and go in the button pane, depending on selection


<details>
	<summary>See more</summary>
	
	dynamicButtonServices
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ fileName isEmptyOrNil
		ifTrue:
			[#()]
		ifFalse:
			[ | toReject |
				toReject _ self buttonSelectorsToSuppress.
				(self itemsForFileEntry: self selectedFileEntry) reject:
					[:svc | toReject includes: svc selector]]
</details>

#### FileList>>#deleteDirectory

Remove the currently selected directory


<details>
	<summary>See more</summary>
	
	deleteDirectory
	"Remove the currently selected directory"

	| newSelection |
	directory children isEmpty ifFalse: [ ^self inform:'Directory must be empty' ].
	(self confirm: 'Really delete ' , directory name , '?') ifFalse: [ ^ self ].
	newSelection _ directory parent.
	directory delete.
	self directory: newSelection.
	self updateFileList.
	self updateDirectory.
	self changed: #initialDirectoryList
</details>

#### FileList>>#viewContentsInWorkspace

View the contents of my selected file in a new workspace


<details>
	<summary>See more</summary>
	
	viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"
	
	| aString aName |


	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		aString _ stream next: stream size.
		aName _ stream localName ].

	(Workspace new contents: aString) openLabel: 'Workspace from ', aName
</details>

#### FileList>>#resort: newMode

Re-sort the list of files.


<details>
	<summary>See more</summary>
	
	resort: newMode
	"Re-sort the list of files."

	| name |
	listIndex > 0
		ifTrue: [name _ self fileNameFromFormattedItem: (list at: listIndex)].
	sortAscending _ sortMode = newMode
		ifTrue: [ sortAscending not ]
		ifFalse: [ newMode = #name].
	sortMode _ newMode.
	self pattern: pattern.
	name ifNotNil: [
		fileName _ name.
		listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = name. ].
		self changed: #fileListIndex].
	listIndex = 0 ifTrue: [
		self acceptedContentsChanged ].
	self triggerEvent: #updateButtonRow

</details>

#### FileList>>#listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject

Make the list be those file names which match the patterns.


<details>
	<summary>See more</summary>
	
	listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory childrenDo: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry name]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList
</details>

#### FileList>>#addNew: aString byEvaluating: aBlock

A parameterization of earlier versions of #addNewDirectory and #addNewFile. Fixes the bug in each that pushing the cancel button in the FillInTheBlank dialog gave a walkback.


<details>
	<summary>See more</summary>
	
	addNew: aString byEvaluating: aBlock
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index |
	(response _ FillInTheBlankMorph
						request: ('New {1} Name?' format: {aString})
						initialAnswer: ('{1}Name' format: {aString}))
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	aBlock value: newName.
	self updateFileList.
	index _(1 to:  list size) detect: [ :i |
		(list at: i) includesSubString: newName ] ifNone: [ 0 ].
	self fileListIndex: index
</details>

#### FileList>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	showDirsInFileList _ false
</details>

#### FileList>>#currentDirectorySelected

<details>
	<summary>See more</summary>
	
	currentDirectorySelected
	^ currentDirectorySelected
</details>

#### FileList>>#itemsForAnyFile2

Answer a list of universal services that could apply to any file


<details>
	<summary>See more</summary>
	
	itemsForAnyFile2
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services _ OrderedCollection new.
	(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse: [
		services add: self serviceGetHex ].
	services add: self serviceCopyName.
	services add: self serviceViewContentsInWorkspace.
	^ services
</details>

#### FileList>>#labelString

<details>
	<summary>See more</summary>
	
	labelString

	^ (directory ifNil: [^'[]']) pathName "contractTo: 50"
</details>

#### FileList>>#defaultContents

<details>
	<summary>See more</summary>
	
	defaultContents
	acceptedContentsCache _ list
			ifNil: [String new]
			ifNotNil: [
				String streamContents: [ :s | 
					s nextPutAll: 'NO FILE SELECTED'; newLine.
					s nextPutAll: '  -- Folder Summary --'; newLine.
					list do: [ :item | s nextPutAll: item; newLine]]].
	brevityState _ #FileList.
	^ acceptedContentsCache
</details>

#### FileList>>#fileList

Answer the list of files in the current volume.


<details>
	<summary>See more</summary>
	
	fileList
	"Answer the list of files in the current volume."

	^ list
</details>

#### FileList>>#fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad

entry is a 5-element array of the form: (name creationTime modificationTime dirFlag fileSize)


<details>
	<summary>See more</summary>
	
	fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd _ spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ]
</details>

#### FileList>>#setSelectedDirectoryTo: aFileDirectoryWrapper

<details>
	<summary>See more</summary>
	
	setSelectedDirectoryTo: aFileDirectoryWrapper
	currentDirectorySelected _ aFileDirectoryWrapper.
	self directory: aFileDirectoryWrapper withoutListWrapper.
	brevityState := #FileList.
	self changed: #fileList.
	self acceptedContentsChanged.
	self changed: #currentDirectorySelected
</details>

#### FileList>>#updateFileList

Update my files list with file names in the current directory that match the pattern. The pattern string may have embedded newlines or semicolons; these separate different patterns.


<details>
	<summary>See more</summary>
	
	updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect |
	patterns _ OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $?)
				ifTrue: [ patterns add: (each copyReplaceAll: '?' with: '#')]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].
	"A pattern that starts with $/ is used to reject entries
	Rejecting patterns are applied after selecting patterns."
	patternsThatSelect _ patterns reject: [ :any | any first = $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject _ patterns select: [ :any | any first = $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	list _ self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self triggerEvent: #updateButtonRow
</details>

#### FileList>>#sortingByDate

<details>
	<summary>See more</summary>
	
	sortingByDate
	^ sortMode == #date
</details>

#### FileList>>#directory

<details>
	<summary>See more</summary>
	
	directory

	^ directory
</details>

#### FileList>>#sortByName

<details>
	<summary>See more</summary>
	
	sortByName
	self resort: #name
</details>

#### FileList>>#sortBySize

<details>
	<summary>See more</summary>
	
	sortBySize
	self resort: #size
</details>

#### FileList>>#buttonSelectorsToSuppress

Answer a list of action selectors whose corresponding services we would prefer *not* to have appear in the filelist's button pane; this can be hand-jimmied to suit personal taste.


<details>
	<summary>See more</summary>
	
	buttonSelectorsToSuppress
	"Answer a list of action selectors whose corresponding services we would prefer *not* to have appear in the filelist's button pane; this can be hand-jimmied to suit personal taste."

	^ #(addFileToNewZip: compressFile:)
</details>

#### FileList>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	^SmalltalkCompleter
</details>

#### FileList>>#pattern: textOrStringOrNil

<details>
	<summary>See more</summary>
	
	pattern: textOrStringOrNil

	textOrStringOrNil
		ifNil: [pattern _ '*']
		ifNotNil: [pattern _ textOrStringOrNil asString].
	self updateFileList.
	^ true

</details>

#### FileList>>#serviceAddNewFile

Answer a service entry characterizing the 'add new file' command


<details>
	<summary>See more</summary>
	
	serviceAddNewFile
	"Answer a service entry characterizing the 'add new file' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'add new file (n)' 
		selector: #addNewFile 
		description: 'create a new,. empty file, and add it to the current directory.'
		icon: #newIcon
</details>

#### FileList>>#fullName

Answer the full name for the currently selected file; answer nil if no file is selected.


<details>
	<summary>See more</summary>
	
	fullName
	"Answer the full name for the currently selected file; answer nil if no file is selected."

	^ fileName ifNotNil: [ (directory // fileName) pathName ]
</details>

#### FileList>>#serviceGetHex

<details>
	<summary>See more</summary>
	
	serviceGetHex

	^ SimpleServiceEntry 
		provider: self 
		label: 'view as hex' 
		selector: #getHex
		description: 'view as hex'
		icon: #preferencesDesktopFontIcon
</details>

#### FileList>>#serviceCopyName

<details>
	<summary>See more</summary>
	
	serviceCopyName

	^ SimpleServiceEntry 
		provider: self 
		label: 'copy name to clipboard' 
		selector: #copyName 
		description:'copy name to clipboard' 
		icon: #copyIcon
</details>

#### FileList>>#renameFile

Rename the currently selected file


<details>
	<summary>See more</summary>
	
	renameFile
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	(response _ FillInTheBlankMorph request: 'NewFileName?'
 					initialAnswer: fileName)
		isEmpty ifTrue: [^ self].
	newName _ response asFileName.
	newName = fileName ifTrue: [^ self].
	directory // fileName rename: newName.
	self updateFileList.
	listIndex _ list findFirst: [:item | (self fileNameFromFormattedItem: item) = newName].
	listIndex > 0 ifTrue: [fileName _ newName].
	self changed: #fileListIndex.
	self triggerEvent: #updateButtonRow

</details>

#### FileList>>#folderString

<details>
	<summary>See more</summary>
	
	folderString
	^ ' [...]'
</details>

#### FileList>>#addNewDirectory

<details>
	<summary>See more</summary>
	
	addNewDirectory

	self 
		addNew: 'Directory'
		byEvaluating: [ :newName | (directory / newName) assureExistence ].
	self updateDirectory.
	self changed: #initialDirectoryList
</details>

#### FileList>>#serviceSortByDate

Answer a service for sorting by date


<details>
	<summary>See more</summary>
	
	serviceSortByDate
	"Answer a service for sorting by date"
	| buttonLabel |
	buttonLabel _ sortMode = #date
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - date' ]
				ifFalse: [ '[v] - date' ]]
		ifFalse: [ 'date' ].
	^  (SimpleServiceEntry 
			provider: self 
			label: 'by date' 
			selector: #sortByDate 
			description: 'sort entries by date'
			icon: #sendReceiveIcon)
		extraSelector: #sortingByDate;
		buttonLabel: buttonLabel
</details>

#### FileList>>#selectedFileEntry

Answer the selected file, in the form of a FileEntry. For the various stream-reading services.


<details>
	<summary>See more</summary>
	
	selectedFileEntry
	"Answer the selected file, in the form of a FileEntry. For the various stream-reading services."

	^ directory ifNotNil: [ :dir | dir // fileName ]
</details>

#### FileList>>#itemsForAnyFile1

Answer a list of universal services that could apply to any file


<details>
	<summary>See more</summary>
	
	itemsForAnyFile1
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services _ OrderedCollection new.
	(#(briefHex briefFile needToGetBriefHex needToGetBrief) includes: brevityState) ifTrue: [
		services add: self serviceGet ].
	services add: self serviceRenameFile. 
	services add: self serviceDeleteFile.
	^ services
</details>

#### FileList>>#doesPattern: aPattern allow: entry

<details>
	<summary>See more</summary>
	
	doesPattern: aPattern allow: entry 

	^(aPattern = '*' or: [ aPattern match: entry name ]) and: [
		"Hide Mac resurce forks and folder service stores"
		(entry name = '.DS_Store') not and: [
		('._*' match: entry name) not ]]
</details>

#### FileList>>#deleteFile

Delete the currently selected file


<details>
	<summary>See more</summary>
	
	deleteFile
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	(self confirm: ('Really delete {1}?' format:{fileName})) ifFalse: [^ self].
	(directory // fileName) delete.
	self updateFileList.
	brevityState _ #FileList.
	self get
</details>

#### FileList>>#readContentsHex: brevity

retrieve the contents from the external file unless it is too long. Don't create a file here. Check if exists.


<details>
	<summary>See more</summary>
	
	readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData |

	directory // fileName readStreamDo: [ :stream |
		stream ifNil: [^ 'For some reason, this file cannot be read'].
		((size _ stream size)) > 2000000 & brevity
			ifTrue: [ data _ stream next: 10000. brevityState := #briefHex ]
			ifFalse: [ data _ stream next: size. brevityState := #fullHex ]].

	hexData _ String streamContents: [ :s |
		0 to: data size-1 by: 16 do: [ :loc |
			loc printOn: s base: 16 length: 8 padded: true.
			s
				space;
				nextPut: $(.
			loc printOn: s base: 10 length: 10 padded: true.
			s
				nextPut: $);
				space;
				tab.
			loc+1 to: (loc+16 min: data size) do: [ :i | s nextPutAll: (data at: i) hex; space ].
			s newLine ]].

	^ acceptedContentsCache _ ((size > 2000000) & brevity
		ifTrue: ['File ''{1}'' is {2} bytes long.
You may use the ''get'' command to read the entire file.

Here are the first 10000 characters...
------------------------------------------
{3}
------------------------------------------
... end of the first 10000 characters.' format: {fileName. size. hexData}]
		ifFalse: [hexData])
</details>

#### FileList>>#brevityState

<details>
	<summary>See more</summary>
	
	brevityState
	^brevityState
</details>

#### FileList>>#updateDirectory

directory has been changed externally, by calling directory:. Now change the view to reflect the change.


<details>
	<summary>See more</summary>
	
	updateDirectory
	"directory has been changed externally, by calling directory:.
	Now change the view to reflect the change."
	self changed: #currentDirectorySelected.
	self postOpen
</details>

#### FileList>>#serviceAddNewDirectory

Answer a service entry characterizing the 'add new directory' command


<details>
	<summary>See more</summary>
	
	serviceAddNewDirectory
	"Answer a service entry characterizing the 'add new directory' command"

	^ SimpleServiceEntry
		provider: self
		label: 'add new directory (N)'
		selector: #addNewDirectory
		description: 'adds a new, empty directory (folder)'
		icon: #listAddIcon
</details>

#### FileList>>#serviceViewContentsInWorkspace

Answer a service for viewing the contents of a file in a workspace


<details>
	<summary>See more</summary>
	
	serviceViewContentsInWorkspace
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ SimpleServiceEntry 
		provider: self 
		label: 'workspace with contents' 
		selector: #viewContentsInWorkspace
		description: 'open a new Workspace whose contents are set to the contents of this file'
		icon: #terminalIcon
</details>

#### FileList>>#put: aText

Private - put the supplied text onto the file


<details>
	<summary>See more</summary>
	
	put: aText
	"Private - put the supplied text onto the file"

	| nameUsed type |
	brevityState == #fullFile ifTrue: [
		directory // fileName writeStreamDo: [ :stream |
			nameUsed _ stream name.
			stream nextPutAll: aText asString ].
		fileName = nameUsed
			ifTrue: [ acceptedContentsCache _ aText asString]
			ifFalse: [self updateFileList].		"user chose a different name (instead of overwriting)"
		^ true  "accepted"].

	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type _ 'These'.
	brevityState = #briefFile ifTrue: [type _ 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type _ 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type _ 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type _ 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"

</details>

#### FileList>>#directory: aDirectoryEntry

Set the path of the volume to be displayed.


<details>
	<summary>See more</summary>
	
	directory: aDirectoryEntry
	"Set the path of the volume to be displayed."

	directory _ aDirectoryEntry.

	sortMode ifNil: [
		sortMode _ #date.
		sortAscending _ false ].
	self changed: #relabel.
	self pattern: pattern
</details>

#### FileList>>#addNewFile

<details>
	<summary>See more</summary>
	
	addNewFile

	self 
		addNew: 'File'
		byEvaluating: [ :newName | (directory // newName) assureExistence ]

</details>

#### FileList>>#serviceGet

Answer a service for getting the entire file


<details>
	<summary>See more</summary>
	
	serviceGet
	"Answer a service for getting the entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'get entire file' 
		selector: #get
		description: 'if the file has only been partially read in, because it is very large, read the entire file in at this time.'
		icon: #textEditorIcon
</details>

#### FileList>>#itemsForFileEntry: aFileEntry

Answer a list of services appropriate for a file of the given name


<details>
	<summary>See more</summary>
	
	itemsForFileEntry: aFileEntry
	"Answer a list of services appropriate for a file of the given name"
	^ self class itemsForFileEntry: aFileEntry
</details>

#### FileList>>#get

Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel.


<details>
	<summary>See more</summary>
	
	get
	"Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel."

	brevityState == #briefHex
		ifTrue: [brevityState _ #needToGetFullHex]
		ifFalse: [brevityState _ #needToGetFull].
	self acceptedContentsChanged
</details>

#### FileList>>#sortingBySize

<details>
	<summary>See more</summary>
	
	sortingBySize
	^ sortMode == #size
</details>

#### FileList>>#pattern

<details>
	<summary>See more</summary>
	
	pattern

	^ pattern ifNil: ['*']

</details>

#### FileList>>#fileListIndex: anInteger

Select the file name having the given index, and display its contents.


<details>
	<summary>See more</summary>
	
	fileListIndex: anInteger
	"Select the file name having the given index, and display its contents."

	listIndex _ anInteger.
	fileName _ listIndex = 0
		"open the file selected"
		ifFalse: [
			self fileNameFromFormattedItem: (list at: anInteger)].

	brevityState _ #needToGetBrief.
	self changed: #fileListIndex.
	self acceptedContentsChanged.
	self triggerEvent: #updateButtonRow
</details>

#### FileList>>#sortBlock

Answer block to decide what order to display the directory entries.


<details>
	<summary>See more</summary>
	
	sortBlock
	"Answer block to decide what order to display the directory entries."

	^ [ :xx :yy | | x y |
		(xx isDirectory == yy isDirectory) 
			ifTrue: [  
				sortAscending
					ifTrue: [ x _ xx. y _ yy ]
					ifFalse: [ x _ yy. y _ xx ].
				"sort by user-specified criterion"
				sortMode = #name 
					ifTrue: [(x name compare: y name) <= 2]
					ifFalse: [ sortMode = #date
						ifTrue: [ x modificationTime = y modificationTime
								ifTrue: [ (x name compare: y name) <= 2 ]
								ifFalse: [ x modificationTime <= y modificationTime ] ]
						ifFalse: [ "size"
							x fileSize = y fileSize 
								ifTrue: [ (x name compare: y name) <= 2 ]
								ifFalse: [ x fileSize <= y fileSize ] ] ] ]
			ifFalse: [
				"directories always precede files"
				xx isDirectory ] ]
</details>

#### FileList>>#copyName

<details>
	<summary>See more</summary>
	
	copyName

	listIndex = 0 ifTrue: [^ self].
	Clipboard storeObject: self fullName
</details>

#### FileList>>#serviceSortBySize

Answer a service for sorting by size


<details>
	<summary>See more</summary>
	
	serviceSortBySize
	"Answer a service for sorting by size"
	| buttonLabel |
	buttonLabel _ sortMode = #size
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - size' ]
				ifFalse: [ '[v] - size' ]]
		ifFalse: [ 'size' ].
	^ (SimpleServiceEntry
		provider: self
		label: 'by size'
		selector: #sortBySize
		description: 'sort entries by size'
		icon: #sendReceiveIcon)
			extraSelector: #sortingBySize;
			buttonLabel: buttonLabel.
</details>

#### FileList>>#editorClassFor: textGetter

Accept both on Contents and on Pattern


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	"Accept both on Contents and on Pattern"
	^SmalltalkEditor
</details>

#### FileList>>#acceptedStringOrText

Answer the contents of the file, reading it first if needed.


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"Answer the contents of the file, reading it first if needed."
	"Possible brevityState values:
		FileList,
		fullFile, briefFile, needToGetFull, needToGetBrief,
		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"

	(listIndex = 0) | (brevityState == #FileList) ifTrue: [ ^ self defaultContents ].  "no file selected"
	brevityState == #fullFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #fullHex ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefHex ifTrue: [ ^ acceptedContentsCache ].

	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false ].
	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true ].

	brevityState == #needToGetFull ifTrue:  [^ self readContentsBrief: false ].
	brevityState == #needToGetBrief ifTrue: [ ^ self readContentsBrief: true ].  "default"

	self halt: 'unknown state ' , brevityState printString
</details>

#### FileList>>#sortByDate

<details>
	<summary>See more</summary>
	
	sortByDate
	self resort: #date
</details>

#### FileList>>#fileNameFromFormattedItem: item

Extract fileName and folderString from a formatted fileList item string


<details>
	<summary>See more</summary>
	
	fileNameFromFormattedItem: item
	"Extract fileName and folderString from a formatted fileList item string"

	| from to |
	self sortingByName
		ifTrue: [
			from _ item lastIndexOf: $( ifAbsent: [0].
			to _ item lastIndexOf: $) ifAbsent: [0]]
		ifFalse: [
			from _ item indexOf: $( ifAbsent: [0].
			to _ item indexOf: $) ifAbsent: [0]].
	^ (from * to = 0
		ifTrue: [item]
		ifFalse: [item copyReplaceFrom: from to: to with: '']) withBlanksTrimmed
</details>

#### FileList>>#serviceSortByName

Answer a service for soring by name


<details>
	<summary>See more</summary>
	
	serviceSortByName
	"Answer a service for soring by name"
	| buttonLabel |
	buttonLabel _ sortMode = #name
		ifTrue: [
			sortAscending
				ifTrue: [ '[^] - name' ]
				ifFalse: [ '[v] - name' ]]
		ifFalse: [ 'name' ].
	^ (SimpleServiceEntry 
		provider: self 
		label: 'by name' 
		selector: #sortByName 
		description: 'sort entries by name'
		icon: #sendReceiveIcon)
			extraSelector: #sortingByName;
			buttonLabel: buttonLabel
</details>

#### FileList>>#postOpen

<details>
	<summary>See more</summary>
	
	postOpen

	directory ifNotNil: [
		self changed: #(openPath), {directory drive}, directory pathComponents ]
</details>

## SimpleServiceEntry

I represent a service provider : the service provider label : to be display in a menu selector : to do the service useLineAfter stateSelector : a secondary selector (to be able to query state of the provider for example) description : a description for balloon for example argumentGetter : a selector to get additional arguments with (if selector requres them) buttonLabel : a short label The entire client interface (provided by FileList and other users of the registry) is this (browse #getArgumentsFrom: and the senders of #argumentGetter:): fullName (returns a String with the full filename) dirAndFileName (returns {directory. fileName}) readOnlyStream (returns an open read-only stream)

### Methods
#### SimpleServiceEntry>>#extraSelector

normally should not be used directly


<details>
	<summary>See more</summary>
	
	extraSelector
	"normally should not be used directly"

	^stateSelector
</details>

#### SimpleServiceEntry>>#getArgumentsFrom: aProvider

<details>
	<summary>See more</summary>
	
	getArgumentsFrom: aProvider

	argumentGetter ifNil: [^aProvider selectedFileEntry ].
	^argumentGetter value: aProvider
</details>

#### SimpleServiceEntry>>#description: aString

may be used for balloon or other


<details>
	<summary>See more</summary>
	
	description: aString
	"may be used for balloon or other"

	description := aString

</details>

#### SimpleServiceEntry>>#provider: anObject label: aString selector: aSymbol

basic initialization message


<details>
	<summary>See more</summary>
	
	provider: anObject label: aString selector: aSymbol 
	"basic initialization message"
	provider := anObject.
	label := aString.
	selector := aSymbol.
	stateSelector := #none.
	description := ''
</details>

#### SimpleServiceEntry>>#sortOrder

<details>
	<summary>See more</summary>
	
	sortOrder
	^sortOrder
</details>

#### SimpleServiceEntry>>#selector

normally should not be used directly


<details>
	<summary>See more</summary>
	
	selector
	"normally should not be used directly"

	^selector
</details>

#### SimpleServiceEntry>>#buttonLabel: aString

Set the receiver's buttonLabel, to be used on a button in a tool-pane; this is split out so that a more abbreviated wording can be deployed if desired


<details>
	<summary>See more</summary>
	
	buttonLabel: aString 
	"Set the receiver's buttonLabel, to be used on a button in a 
	tool-pane; this is split out so that a more abbreviated wording 
	can be deployed if desired"
	buttonLabel := aString
</details>

#### SimpleServiceEntry>>#icon: anIcon

<details>
	<summary>See more</summary>
	
	icon: anIcon

	icon := anIcon 
</details>

#### SimpleServiceEntry>>#buttonLabel

Answer the label to be emblazoned on a button representing the service in a file list, for example


<details>
	<summary>See more</summary>
	
	buttonLabel
	"Answer the label to be emblazoned on a button representing the service in a file list, for example"

	^ buttonLabel ifNil: [label]
</details>

#### SimpleServiceEntry>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream nextPutAll: self class name; nextPutAll: ': ('.
	self provider
		ifNotNil: [ aStream nextPutAll: provider printString].
	aStream nextPutAll: ' --- '. 
	self selector
		ifNotNil: [ aStream nextPutAll: selector asString].
	aStream nextPut: $)


</details>

#### SimpleServiceEntry>>#provider

<details>
	<summary>See more</summary>
	
	provider

	^ provider
</details>

#### SimpleServiceEntry>>#addServiceFor: served toMenu: aMenu

<details>
	<summary>See more</summary>
	
	addServiceFor: served toMenu: aMenu
	argumentProvider _ served.
	aMenu 
		add: self label 
		target: self 
		action: #performService
		icon: icon.
	self useLineAfter ifTrue: [ aMenu addLine ].
</details>

#### SimpleServiceEntry>>#triggerFileListChanged

<details>
	<summary>See more</summary>
	
	triggerFileListChanged
	triggerFileListChanged _ true
</details>

#### SimpleServiceEntry>>#argumentGetter: aBlock

<details>
	<summary>See more</summary>
	
	argumentGetter: aBlock

	argumentGetter _ aBlock
</details>

#### SimpleServiceEntry>>#sortOrder: aNumber

<details>
	<summary>See more</summary>
	
	sortOrder: aNumber
	sortOrder _ aNumber
</details>

#### SimpleServiceEntry>>#description

may be used for balloon or other


<details>
	<summary>See more</summary>
	
	description
	"may be used for balloon or other"

	^ description

</details>

#### SimpleServiceEntry>>#argumentProvider: o

<details>
	<summary>See more</summary>
	
	argumentProvider: o
	argumentProvider _ o
</details>

#### SimpleServiceEntry>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	triggerFileListChanged _ false.
	sortOrder _ 1.
	icon _ nil.
</details>

#### SimpleServiceEntry>>#useLineAfter

<details>
	<summary>See more</summary>
	
	useLineAfter

	^ useLineAfter == true
</details>

#### SimpleServiceEntry>>#provider: anObject label: aString selector: aSymbol description: anotherString

basic initialization message


<details>
	<summary>See more</summary>
	
	provider: anObject label: aString selector: aSymbol description: anotherString 
	"basic initialization message"
	self
		provider: anObject
		label: aString
		selector: aSymbol.
	stateSelector := #none.
	description := anotherString
</details>

#### SimpleServiceEntry>>#performService

carry out the service I provide


<details>
	<summary>See more</summary>
	
	performService
	"carry out the service I provide"

	selector numArgs = 0
		ifTrue: [ provider perform: selector ]
		ifFalse: [ provider perform: selector with: (self getArgumentsFrom: argumentProvider) ].
	triggerFileListChanged ifTrue: [
		self triggerEvent: #fileListChanged ]
</details>

#### SimpleServiceEntry>>#useLineAfter: aBoolean

<details>
	<summary>See more</summary>
	
	useLineAfter: aBoolean

	useLineAfter := aBoolean

</details>

#### SimpleServiceEntry>>#requestExtraSelector

send me this message to ask me to perform secondary service


<details>
	<summary>See more</summary>
	
	requestExtraSelector
	"send me this message to ask me to perform secondary service"

	^#performExtraFor:

</details>

#### SimpleServiceEntry>>#extraSelector: aSymbol

<details>
	<summary>See more</summary>
	
	extraSelector: aSymbol

	stateSelector := aSymbol
</details>

#### SimpleServiceEntry>>#performExtraFor: anObject

carry out the extra service I provide


<details>
	<summary>See more</summary>
	
	performExtraFor: anObject
	"carry out the extra service I provide"
	"the stateSelector can be used to ask state of the provider to be reflected in button or other"

	^stateSelector numArgs = 0
		ifTrue: [provider perform: stateSelector]
		ifFalse: [provider perform: stateSelector with: (self getArgumentsFrom: anObject) ]

</details>

#### SimpleServiceEntry>>#iconSpec

<details>
	<summary>See more</summary>
	
	iconSpec

	^icon
</details>

#### SimpleServiceEntry>>#label

<details>
	<summary>See more</summary>
	
	label
	^label
</details>

