## CodeFile

My instances represent a file with Smalltalk code.

### Methods
#### CodeFile>>#classDefinition: string with: chgRec

<details>
	<summary>See more</summary>
	
	classDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.
</details>

#### CodeFile>>#isLiveSmalltalkImage

<details>
	<summary>See more</summary>
	
	isLiveSmalltalkImage
	^ false
</details>

#### CodeFile>>#msgClassComment: string with: chgRec

<details>
	<summary>See more</summary>
	
	msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass theMetaClass classComment: tokens last].

</details>

#### CodeFile>>#renameClass: aPseudoClass to: newName

<details>
	<summary>See more</summary>
	
	renameClass: aPseudoClass to: newName
	| oldName |
	oldName _ aPseudoClass name.
	classes removeKey: oldName.
	classes at: newName put: aPseudoClass.
	aPseudoClass renameTo: newName.
</details>

#### CodeFile>>#fileOutDoits: aStream

<details>
	<summary>See more</summary>
	
	fileOutDoits: aStream
	doIts do:[:chgRec| chgRec fileOutOn: aStream].
</details>

#### CodeFile>>#buildFrom: aStream

<details>
	<summary>See more</summary>
	
	buildFrom: aStream

	| changes |
	
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :barBlock | self buildFrom: changes informingTo: barBlock ].

</details>

#### CodeFile>>#classNamed: className

<details>
	<summary>See more</summary>
	
	classNamed: className
	^ classes at: className
</details>

#### CodeFile>>#classDictionary

<details>
	<summary>See more</summary>
	
	classDictionary
	^classes
</details>

#### CodeFile>>#sourceSystem

<details>
	<summary>See more</summary>
	
	sourceSystem
	^sourceSystem
</details>

#### CodeFile>>#getClass: className

<details>
	<summary>See more</summary>
	
	getClass: className
	| pseudoClass |
	(classes includesKey: className) ifTrue:[
		^classes at: className.
	].
	pseudoClass := PseudoClass new.
	pseudoClass name: className.
	classes at: className put: pseudoClass.
	^pseudoClass.
</details>

#### CodeFile>>#summary

<details>
	<summary>See more</summary>
	
	summary
	| nClasses newClasses oldClasses |
	nClasses := newClasses := oldClasses := 0.
	classes do:[:cls|
		nClasses := nClasses + 1.
		(Smalltalk includesKey: (cls name asSymbol))
			ifTrue: [ oldClasses := oldClasses + 1]
			ifFalse: [ newClasses := newClasses + 1]].
	^nClasses printString,' classes (', newClasses printString, ' new / ', oldClasses printString, ' modified)'
</details>

#### CodeFile>>#possibleSystemSource: chgRec

<details>
	<summary>See more</summary>
	
	possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.
</details>

#### CodeFile>>#buildFrom: changes informingTo: barBlock

<details>
	<summary>See more</summary>
	
	buildFrom: changes informingTo: barBlock 

	changes withIndexDo: [ :changeRecord :anIndex | 
		barBlock value: anIndex.
		changeRecord performOn: self.
	].

</details>

#### CodeFile>>#method: chgRec

See senders of #method


<details>
	<summary>See more</summary>
	
	method: chgRec
	"See senders of #method "
	(self getClass: chgRec changeClassName) methodChange: chgRec
</details>

#### CodeFile>>#description

<details>
	<summary>See more</summary>
	
	description
	^String streamContents:[:s|
		s nextPutAll: 'CodeFile: '.
		s nextPutAll: self fullName; newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ].
		doIts isEmpty ifFalse:[
			s nextPutAll: 'Unresolvable doIts:'; newLine; newLine.
			doIts do: [ :chgRec |
				s
					nextPut:$!;
					nextPutAll: chgRec string;
					nextPut: $!;
					newLine ]]]
</details>

#### CodeFile>>#classComment: chgRec

See senders of #classComment


<details>
	<summary>See more</summary>
	
	classComment: chgRec
	"See senders of #classComment "
	(self getClass: chgRec changeClassName) classComment: chgRec
</details>

#### CodeFile>>#fileInDoits

<details>
	<summary>See more</summary>
	
	fileInDoits
	doIts do:[:chgRec| chgRec fileIn].
</details>

#### CodeFile>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	classes _ Dictionary new.
	classOrder _ OrderedCollection new.
	sourceSystem _ ''.
	doIts _ OrderedCollection new
</details>

#### CodeFile>>#fileOut

<details>
	<summary>See more</summary>
	
	fileOut

	(FillInTheBlankMorph request: 'Enter the file name' initialAnswer:'') asFileEntry writeStreamDo: [ :stream | 
		sourceSystem isEmpty ifFalse:[
			stream nextChunkPut: sourceSystem printString; newLine ].
		self fileOutOn: stream.
		stream newLine; newLine.
		classes do: [ :cls |
			cls needsInitialize ifTrue: [
				stream newLine; nextChunkPut: cls name,' initialize']].
		stream newLine ]
</details>

#### CodeFile>>#doIt: chgRec

See senders of #doIt


<details>
	<summary>See more</summary>
	
	doIt: chgRec
	"See senders of #doIt "
	| string |
	string := chgRec string.
	
	"Method classification spec"
	(string beginsWith: '(''') ifTrue: [
		^ doIts add: chgRec ].
		
	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[^self metaClassDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #methodRemoval: (or similar) change type marker in the files."
	('* removeSelector: *'
		match: string) ifTrue:[^self removedMethod: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classComment change type marker in the files."
	('* comment:*'
		match: string) ifTrue:[^self msgClassComment: string with: chgRec].

	"Don't add these to a CodeFile. They will be added on save if needed."
	('* initialize'
		match: string) ifTrue:[^self]. "Initialization is done based on class>>initialize"

	('''From *'
		match: string) ifTrue:[^self possibleSystemSource: chgRec].
	doIts add: chgRec.
</details>

#### CodeFile>>#organization

<details>
	<summary>See more</summary>
	
	organization
	^ SystemOrganizer defaultList: Array new.
</details>

#### CodeFile>>#fullName: aString

<details>
	<summary>See more</summary>
	
	fullName: aString
	fullName _ aString
</details>

#### CodeFile>>#classDefinition: chgRec

See senders of #classDefinition


<details>
	<summary>See more</summary>
	
	classDefinition: chgRec
	"See senders of #classDefinition "
	| string |
	string := chgRec string.
	^chgRec isMetaClassChange 
		ifTrue: [self metaClassDefinition: string with: chgRec]
		ifFalse: [self classDefinition: string with: chgRec]
</details>

#### CodeFile>>#allMethodReferences

Create an answer a Set with method references for all methods in us


<details>
	<summary>See more</summary>
	
	allMethodReferences
	"Create an answer a Set with method references for all methods in us"
	| answer className metaClass |
	answer _ Set new.
	
	classes do: [ :pseudoClass | 
		className _ pseudoClass name.
		pseudoClass selectors do: [ :selector |
			answer add: 
				(MethodReference new
					setClassSymbol: className
					classIsMeta: false
					methodSymbol: selector
					stringVersion: className, ' ' , selector) ].
		pseudoClass hasMetaclass ifTrue: [
			metaClass _ pseudoClass theMetaClass.
			metaClass selectors do: [ :selector |
				answer add: 
					(MethodReference new
						setClassSymbol: className
						classIsMeta: true
						methodSymbol: selector
						stringVersion: className, ' class ' , selector) ].
				]].
	^answer
</details>

#### CodeFile>>#baseLabel

<details>
	<summary>See more</summary>
	
	baseLabel
	^ 'base'
</details>

#### CodeFile>>#classRemoval: aClassDeletionChangeRecord

<details>
	<summary>See more</summary>
	
	classRemoval: aClassDeletionChangeRecord 
	
	^self classDefinition: aClassDeletionChangeRecord 
</details>

#### CodeFile>>#removeDoIts

<details>
	<summary>See more</summary>
	
	removeDoIts
	doIts := OrderedCollection new.
</details>

#### CodeFile>>#metaClassDefinition: string with: chgRec

<details>
	<summary>See more</summary>
	
	metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass theMetaClass definition: string.
	classOrder add: theClass theMetaClass.
</details>

#### CodeFile>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileInDoits].
	classOrder do:[:cls|
		cls fileInDefinition.
	].
	classes do:[:cls|
		Preferences transcriptLogVerbose ifTrue: [
			Transcript newLine; show:'Filing in ', cls name].
		cls fileInMethods.
		cls hasMetaclass ifTrue:[cls theMetaClass fileInMethods].
	].
	doitsMark = 3 ifTrue: [ self fileInDoits ]
</details>

#### CodeFile>>#askForDoits

<details>
	<summary>See more</summary>
	
	askForDoits
	| menu choice choices |
	choices := #('do not process' 'at the beginning' 'at the end' 'cancel').
	menu _ SelectionMenu selections: choices.
	choice := nil.
	[choices includes: choice] whileFalse: [
		choice _ menu startUpWithCaption: 
'The CodeFile contains unprocessed doIts.
When would like to process those?'].
	^choices indexOf: choice
</details>

#### CodeFile>>#classes

<details>
	<summary>See more</summary>
	
	classes
	^ self classDictionary values
</details>

#### CodeFile>>#removedMethod: string with: chgRec

<details>
	<summary>See more</summary>
	
	removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Scanner new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _ tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class theMetaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec
</details>

#### CodeFile>>#fileOutOn: aStream

<details>
	<summary>See more</summary>
	
	fileOutOn: aStream
	| doitsMark |
	doitsMark := 1.
	doIts isEmpty ifFalse:[doitsMark := self askForDoits].
	doitsMark = 4 ifTrue: [^nil].
	doitsMark = 2 ifTrue:[self fileOutDoits: aStream].
	classOrder do:[:cls|
		cls fileOutDefinitionOn: aStream.
	].
	classes do:[:cls|
		cls fileOutMethodsOn: aStream.
		cls hasMetaclass ifTrue:[cls theMetaClass fileOutMethodsOn: aStream].
	].
	doitsMark = 3 ifTrue:[self fileOutDoits: aStream].
</details>

#### CodeFile>>#preamble: chgRec

See senders of #preamble


<details>
	<summary>See more</summary>
	
	preamble: chgRec
	"See senders of #preamble "
	self doIt: chgRec
</details>

#### CodeFile>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^ self fullName asFileEntry name
</details>

#### CodeFile>>#fullName

<details>
	<summary>See more</summary>
	
	fullName
	^fullName
</details>

#### CodeFile>>#removeClass: aPseudoClass

<details>
	<summary>See more</summary>
	
	removeClass: aPseudoClass
	classes removeKey: aPseudoClass name.
	classOrder copy do: [ :cls |
		cls name = aPseudoClass name ifTrue: [ classOrder remove: cls]]
</details>

## CodeFileBrowser

I am a class browser view on a fileout (either a source file (.st) or change set (.cs.st)). I do not actually load the code into to the system, nor do I alter the classes in the image. Use me to vet code in a comfortable way before loading it into your image. From a FileList, I can be invoked by selecting a source file and selecting the "browse code" menu item from the context (right click) menu. I use PseudoClass, PseudoClassOrganizers, and PseudoMetaclass to model the class structure of the source file.

### Methods
#### CodeFileBrowser>>#didCodeChangeElsewhere

Determine whether the code for the currently selected method and class has been changed somewhere else.


<details>
	<summary>See more</summary>
	
	didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass |
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ false].

	(aClass is: #PseudoClass) ifTrue: [^ false]. "class not installed"
	^super didCodeChangeElsewhere
</details>

#### CodeFileBrowser>>#fileOutClass

Print a description of the selected class onto a file whose name is the category name followed by .st.


<details>
	<summary>See more</summary>
	
	fileOutClass

	self selectedClass fileOut
</details>

#### CodeFileBrowser>>#baseCodeSource: aCodeFile

<details>
	<summary>See more</summary>
	
	baseCodeSource: aCodeFile
	baseCodeSource _ aCodeFile
</details>

#### CodeFileBrowser>>#removeUnmodifiedMethods

<details>
	<summary>See more</summary>
	
	removeUnmodifiedMethods
	| theClass cat |
	theClass := self selectedClassOrMetaClass.
	theClass ifNil: [ ^self].
	cat := self selectedMessageCategoryName.
	cat ifNil: [ ^self].
	theClass removeUnmodifiedMethods: (theClass organization listAtCategoryNamed: cat).
	self reformulateList.
	self changed: #messageList.
</details>

#### CodeFileBrowser>>#annotation

Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver.


<details>
	<summary>See more</summary>
	
	annotation
	^self infoViewContents
</details>

#### CodeFileBrowser>>#contentsSymbolQuints

Answer a list of quintuplets representing information on the alternative views available in the code pane. For the file-contents browser, the choices are restricted to source and the diffing options


<details>
	<summary>See more</summary>
	
	contentsSymbolQuints
	"Answer a list of quintuplets representing information on the alternative views available in the code pane.  For the file-contents browser, the choices are restricted to source and the diffing options"

	^ self sourceAndDiffsQuintsOnly
</details>

#### CodeFileBrowser>>#fileOutMessage

Put a description of the selected message on a file


<details>
	<summary>See more</summary>
	
	fileOutMessage

	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileOutMethod: self selectedMessageName
</details>

#### CodeFileBrowser>>#renameClass

<details>
	<summary>See more</summary>
	
	renameClass
	| oldName newName |
	selectedClassName ifNil: [ ^self ].
	oldName _ self selectedClass name.
	newName _ (self request: 'Please type new class name'
						initialAnswer: oldName) asSymbol.
	(newName isEmpty or:[newName = oldName]) ifTrue: [^ self].
	(caseCodeSource classDictionary includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists in the CodeFile'].
	systemOrganizer classify: newName under: selectedSystemCategory.
	systemOrganizer removeElement: oldName.
	caseCodeSource renameClass: self selectedClass to: newName.
	self changed: #classList.
	self classListIndex: ((systemOrganizer listAtCategoryNamed: selectedSystemCategory) indexOf: newName).

</details>

#### CodeFileBrowser>>#infoViewContents

<details>
	<summary>See more</summary>
	
	infoViewContents
	| theClass selector useLabel |
	useLabel _ self baseCodeSource baseLabel.
	editSelection == #newClass ifTrue: [
		^ caseCodeSource
			ifNil: [ 'No file selected' ]
			ifNotNil: [ caseCodeSource summary ]].
	self selectedClass ifNil: [ ^ '' ].
	theClass _ self pvtBaseClassOrMetaclass.
	editSelection == #editClass ifTrue: [
		^ theClass
			ifNil: [ 'Class not in the ' , useLabel ]
			ifNotNil: [ 'Class exists already in the ' , useLabel ]].
	editSelection == #editMessage ifFalse: [ ^ '' ].
	selector _ self selectedMessageName.
	^ (theClass notNil and: [ theClass includesSelector: selector ])
		ifTrue: [ 'Method already exists' , self extraInfo ]
		ifFalse: [			
			(self classOrMetaClassOrganizer isRemoved: selector)
				ifTrue: [ 'Method not in the ' , useLabel ]
				ifFalse: [ '**NEW** Method not in the ' , useLabel ]]
</details>

#### CodeFileBrowser>>#baseCodeSource

<details>
	<summary>See more</summary>
	
	baseCodeSource
	^ baseCodeSource ifNil: [ Smalltalk ].
</details>

#### CodeFileBrowser>>#messageList

Colorize messages as needed


<details>
	<summary>See more</summary>
	
	messageList
	"Colorize messages as needed"
	^ super messageList collect: [ :eaListItem | | useAttr |
		useAttr _ (self classOrMetaClassOrganizer isRemoved: eaListItem)
			ifTrue: [ TextColor red ]
			ifFalse: [ | baseSrc |
				baseSrc _ self pvtBaseSelectedMessageSourceCodeFor: eaListItem.
				baseSrc
					ifNil: [ TextColor green ]
					ifNotNil: [ | caseSrc |
						caseSrc _ self pvtCaseSelectedMessageSourceCodeFor: eaListItem.
						baseSrc = caseSrc ifFalse: [ TextColor blue ]]].
		useAttr
			ifNil: [ eaListItem ]
			ifNotNil: [ :attr |
				Text
					string: eaListItem
					attribute: attr ]].
</details>

#### CodeFileBrowser>>#fileInClass

<details>
	<summary>See more</summary>
	
	fileInClass

	self selectedClass fileIn
</details>

#### CodeFileBrowser>>#selectedMessage

Answer a copy of the source code for the selected message selector.


<details>
	<summary>See more</summary>
	
	selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ class sourceCodeAt: selector.
	(self classOrMetaClassOrganizer isRemoved: selector) ifTrue: [
		^ Text
			string: answer
			attribute: TextColor red ].
	Preferences browseWithPrettyPrint ifTrue: [
		answer _ class compilerClass new
						format: answer in: class notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ self
			methodDiffFor: answer
			selector: self selectedMessageName ].
	^ answer
</details>

#### CodeFileBrowser>>#pvtCaseSelectedMessageSourceCodeFor: selector

<details>
	<summary>See more</summary>
	
	pvtCaseSelectedMessageSourceCodeFor: selector
	| class  |
	class _ self selectedClassOrMetaClass.
	^ class sourceCodeAt: selector.
</details>

#### CodeFileBrowser>>#changeMessageCategories: aString

The characters in aString represent an edited version of the the message categories for the selected class. Update this information in the system and inform any dependents that the categories have been changed. This message is invoked because the user had issued the categories command and edited the message categories. Then the user issued the accept command.


<details>
	<summary>See more</summary>
	
	changeMessageCategories: aString 
	"The characters in aString represent an edited version of the the message 
	categories for the selected class. Update this information in the system 
	and inform any dependents that the categories have been changed. This 
	message is invoked because the user had issued the categories command 
	and edited the message categories. Then the user issued the accept 
	command."

	self classOrMetaClassOrganizer changeFromString: aString.
	self unlock.
	self editClass.
	self classListIndex: self classListIndex.
	^ true
</details>

#### CodeFileBrowser>>#caseCodeSource: aCodeFile

<details>
	<summary>See more</summary>
	
	caseCodeSource: aCodeFile
	caseCodeSource _ aCodeFile
</details>

#### CodeFileBrowser>>#removeMessageCategory

If a message category is selected, create a Confirmer so the user can verify that the currently selected message category should be removed from the system. If so, remove it.


<details>
	<summary>See more</summary>
	
	removeMessageCategory
	"If a message category is selected, create a Confirmer so the user can 
	verify that the currently selected message category should be removed
 	from the system. If so, remove it."

	| messageCategoryName |
	selectedMessageCategory ifNil: [ ^self ].
	messageCategoryName _ self selectedMessageCategoryName.
	(self messageList size = 0
		or: [self confirm: 'Are you sure you want to
remove this method category 
and all its methods?']) ifFalse: [^ self].
	self selectedClassOrMetaClass removeCategory: messageCategoryName.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList.
</details>

#### CodeFileBrowser>>#fileInMessage

<details>
	<summary>See more</summary>
	
	fileInMessage
	
	self selectedMessageName ifNil: [^self].
	self selectedClassOrMetaClass fileInMethod: self selectedMessageName
</details>

#### CodeFileBrowser>>#classList

Answer an array of the class names of the selected category. Answer an empty array if no selection exists.


<details>
	<summary>See more</summary>
	
	classList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ #() ]
		ifFalse: [ (caseCodeSource classes collect: [:ea| ea name]) sort ]
</details>

#### CodeFileBrowser>>#selectedClass

Answer the class that is currently selected. Answer nil if no selection exists.


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	^self selectedClassName ifNotNil: [ :scn |
		caseCodeSource classNamed: scn ]
</details>

#### CodeFileBrowser>>#pvtBaseClassOrMetaclass

<details>
	<summary>See more</summary>
	
	pvtBaseClassOrMetaclass
	| theClass |
	theClass _ [self baseCodeSource classNamed: self selectedClass name asSymbol] on: Error do: ["Class not found in base?"].
	^ (theClass notNil and: [ self metaClassIndicated ])
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
</details>

#### CodeFileBrowser>>#removeUnmodifiedCategories

<details>
	<summary>See more</summary>
	
	removeUnmodifiedCategories
	| theClass |
	theClass _ self selectedClass.
	theClass ifNil: [^self].
	theClass removeUnmodifiedMethods: theClass selectors.
	theClass theMetaClass removeUnmodifiedMethods: theClass theMetaClass selectors.
	self messageCategoryListIndex: 0.
	self changed: #messageCategoryList
</details>

#### CodeFileBrowser>>#fileOut

<details>
	<summary>See more</summary>
	
	fileOut

	caseCodeSource fileOut
</details>

#### CodeFileBrowser>>#pvtBaseSelectedMessageSourceCodeFor: selector

<details>
	<summary>See more</summary>
	
	pvtBaseSelectedMessageSourceCodeFor: selector
	^ self pvtBaseClassOrMetaclass ifNotNil: [ :theClass |
		(theClass includesSelector: selector) ifTrue: [ theClass sourceCodeAt: selector ]].
</details>

#### CodeFileBrowser>>#contentsSymbol

Answer a symbol indicating what kind of content should be shown for the method


<details>
	<summary>See more</summary>
	
	contentsSymbol
	"Answer a symbol indicating what kind of content should be shown for the method"

	^ contentsSymbol ifNil: [
		contentsSymbol _ self defaultDiffsSymbol ]
</details>

#### CodeFileBrowser>>#methodDiffFor: aString selector: selector

Answer the diff between the current copy of the given class/selector/meta for the string provided


<details>
	<summary>See more</summary>
	
	methodDiffFor: aString selector: selector
	"Answer the diff between the current copy of the given class/selector/meta for the string provided"
	| theClass source |
	source _ ''.
	theClass _ self pvtBaseClassOrMetaclass.
	theClass ifNotNil: [
		(theClass includesSelector: selector) ifTrue: [
			source _ theClass sourceCodeAt: selector ]].
	^ DifferenceFinder
		displayPatchFrom: source
		to: aString
		tryWords: self shouldDiffWords
		prettyPrintedIn:
			(self showingAnyKindOfPrettyDiffs ifTrue: [ theClass ]).
</details>

#### CodeFileBrowser>>#updateInfoView

<details>
	<summary>See more</summary>
	
	updateInfoView

	self changed: #infoViewContents
</details>

#### CodeFileBrowser>>#labelString

Answer the string for the window title


<details>
	<summary>See more</summary>
	
	labelString
	"Answer the string for the window title"

	^ selectedSystemCategory ifNil: ['']
</details>

#### CodeFileBrowser>>#removeUnmodifiedClasses

<details>
	<summary>See more</summary>
	
	removeUnmodifiedClasses
	caseCodeSource isLiveSmalltalkImage
		ifTrue: [ self error: 'Do not perform on a live image!' ]
		ifFalse: [
			caseCodeSource classDictionary copy do: [ :theClass |
				theClass removeAllUnmodified.
				theClass hasChanges ifFalse: [ caseCodeSource removeClass: theClass ]].
			self classListIndex: 0.
			self changed: #classList ].
</details>

#### CodeFileBrowser>>#removeClass

Remove the selected class from the system, at interactive user request. Make certain the user really wants to do this, since it is not reversible. Answer true if removal actually happened.


<details>
	<summary>See more</summary>
	
	removeClass
	| class |
	selectedClassName ifNil: [ ^self ].
	class _ self selectedClass.
	(self confirm:'Are you certain that you
want to delete the class ', class name, '?') ifFalse:[^self].
	caseCodeSource removeClass: class.
	self classListIndex: 0.
	self changed: #classList.
</details>

#### CodeFileBrowser>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true
</details>

#### CodeFileBrowser>>#fileOutMessageCategories

Print a description of the selected message category of the selected class onto an external file.


<details>
	<summary>See more</summary>
	
	fileOutMessageCategories

	self selectedClassOrMetaClass fileOutCategory: self selectedMessageCategoryName
</details>

#### CodeFileBrowser>>#contents: input notifying: aRequestor

The retrieved information has changed and its source must now be updated. The information can be a variety of things, depending on the list selections (such as templates for class or message definition, methods) or the user menu commands (such as definition, comment, hierarchy). Answer the result of updating the source.


<details>
	<summary>See more</summary>
	
	contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| aString aText theClass |
	aString _ input asString.
	aText _ input asText.

	editSelection == #editComment 
		ifTrue: [theClass _ self selectedClass.
				theClass ifNil: [self inform: 'You must select a class
before giving it a comment.'.
				^ false].
				theClass comment: aText. ^ true].
	editSelection == #editMessageCategories 
		ifTrue: [^ self changeMessageCategories: aString].

	self inform:'You cannot change the current selection'.
	^false

</details>

#### CodeFileBrowser>>#fileInMessageCategories

<details>
	<summary>See more</summary>
	
	fileInMessageCategories

	self selectedClassOrMetaClass fileInCategory: self selectedMessageCategoryName
</details>

#### CodeFileBrowser>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn
	caseCodeSource fileIn
</details>

#### CodeFileBrowser>>#selectedBytecodes

Not supported


<details>
	<summary>See more</summary>
	
	selectedBytecodes
	"Not supported"
	^''
</details>

#### CodeFileBrowser>>#modifiedClassDefinition

<details>
	<summary>See more</summary>
	
	modifiedClassDefinition
	| pClass rClass old new |
	pClass := self selectedClassOrMetaClass.
	pClass ifNil: [^''].
	pClass hasDefinition ifFalse: [ ^pClass definition].
	rClass := [self baseCodeSource classNamed: self selectedClass name] on: Error do: ["Missing class"].
	rClass ifNil: [ ^pClass definition].
	self metaClassIndicated ifTrue:[ rClass := rClass class].
	old := rClass definition.
	new := pClass definition.
	^ DifferenceFinder displayPatchFrom: old to: new tryWords: true
</details>

#### CodeFileBrowser>>#acceptedStringOrText

Depending on the current selection, different information is retrieved. Answer a string description of that information. This information is the method of the currently selected class and message.


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	self updateInfoView.
	(editSelection == #newClass and: [ caseCodeSource notNil ])
		ifTrue: [ ^caseCodeSource description ].
	editSelection == #editClass
		ifTrue:[ ^self modifiedClassDefinition ].
	^super acceptedStringOrText
</details>

#### CodeFileBrowser>>#removeMessage

If a message is selected, create a Confirmer so the user can verify that the currently selected message should be removed from the system. If so, remove it. If the Preference 'confirmMethodRemoves' is set to false, the confirmer is bypassed.


<details>
	<summary>See more</summary>
	
	removeMessage
	| messageName |
	selectedMessage ifNil: [ ^self ].
	messageName _ self selectedMessageName.
	(self selectedClass confirmRemovalOf: messageName)
		ifFalse: [^ false].
	self selectedClassOrMetaClass removeMethod: self selectedMessageName.
	self reformulateList.
	self setClassOrganizer.
	"In case organization not cached"
	self changed: #messageList
</details>

#### CodeFileBrowser>>#extraInfo

<details>
	<summary>See more</summary>
	
	extraInfo
	^ (self
		methodDiffFor: (self selectedClassOrMetaClass sourceCodeAt: self selectedMessageName)
		selector: self selectedMessageName)
			hasAnyAttribute
				ifTrue: [' - **MODIFIED**']
				ifFalse: [' - identical']
</details>

#### CodeFileBrowser>>#setClassOrganizer

Install whatever organization is appropriate


<details>
	<summary>See more</summary>
	
	setClassOrganizer
	"Install whatever organization is appropriate"
	| theClass |
	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	selectedClassName ifNil: [ ^self ].
	theClass _ self selectedClass.
	theClass ifNil: [classOrganizer := self baseCodeSource organization.
		metaClassOrganizer := self baseCodeSource organization] ifNotNil: [
	classOrganizer _ theClass organization.
	metaClassOrganizer _ theClass theMetaClass organization.
	
]
</details>

#### CodeFileBrowser>>#caseCodeSource

<details>
	<summary>See more</summary>
	
	caseCodeSource
	^ caseCodeSource ifNil: [ Smalltalk ].
</details>

#### CodeFileBrowser>>#selectedClassOrMetaClass

Answer the selected class or metaclass.


<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	"Answer the selected class or metaclass."

	| cls |
	self metaClassIndicated
		ifTrue: [^ (cls _ self selectedClass) ifNotNil: [cls theMetaClass]]
		ifFalse: [^ self selectedClass]
</details>

## PseudoClass

I provide an inert model of a Class, used by FileContentsBrowser to manipulate filedout code. Instead of a method dictionary or selectors onto CompiledMethods, I have a dictionary ("source") of selectors onto ChangeRecords, which were, in the case of FileContentsBrowser, parsed from a source or change set file.

### Methods
#### PseudoClass>>#bindingOf: varName

<details>
	<summary>See more</summary>
	
	bindingOf: varName
	self exists ifTrue:[
		(self realClass bindingOf: varName) ifNotNil: [ :binding | ^binding].
	].
	^Smalltalk bindingOf: varName asSymbol
</details>

#### PseudoClass>>#removeUnmodifiedMethods: aCollection

<details>
	<summary>See more</summary>
	
	removeUnmodifiedMethods: aCollection
	| stClass |
	self exists ifFalse:[^self].
	stClass := self realClass.
	aCollection do:[:sel|
		(self sourceCodeAt: sel) = (stClass sourceCodeAt: sel ifAbsent:['']) asString ifTrue:[
			self removeMethod: sel.
		].
	].
	self organization removeEmptyCategories.
</details>

#### PseudoClass>>#definition: aString

<details>
	<summary>See more</summary>
	
	definition: aString
	definition := aString
</details>

#### PseudoClass>>#chooseInstVarThenDo: aBlock

<details>
	<summary>See more</summary>
	
	chooseInstVarThenDo: aBlock
</details>

#### PseudoClass>>#allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level

<details>
	<summary>See more</summary>
	
	allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level
	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false
</details>

#### PseudoClass>>#fileOutDefinitionOn: aStream

<details>
	<summary>See more</summary>
	
	fileOutDefinitionOn: aStream
	self hasDefinition ifFalse:[^self].
	aStream nextChunkPut: self definition; newLine.
	self hasComment ifTrue: [
		aStream newLine.
		self organization commentRemoteStr fileOutOn: aStream]
</details>

#### PseudoClass>>#name: anObject

<details>
	<summary>See more</summary>
	
	name: anObject
	name _ anObject
</details>

#### PseudoClass>>#addMethodChange: aChangeRecord

<details>
	<summary>See more</summary>
	
	addMethodChange: aChangeRecord
	| selector |
	selector _ self parserClass selectorFrom: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord
</details>

#### PseudoClass>>#makeSureClassExists: aString

<details>
	<summary>See more</summary>
	
	makeSureClassExists: aString
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent: nil.
	theClass ifNotNil:[^true].
	^self confirm: aString,' does not exist in the system. Use nil instead?'.
</details>

#### PseudoClass>>#superclassName

<details>
	<summary>See more</summary>
	
	superclassName
	^definition copyUpTo: Character space
</details>

#### PseudoClass>>#sourceCodeTemplate

<details>
	<summary>See more</summary>
	
	sourceCodeTemplate
	^''
</details>

#### PseudoClass>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream nextPut:$(; print: name; nextPut:$)
</details>

#### PseudoClass>>#fileInCategory: aCategory

<details>
	<summary>See more</summary>
	
	fileInCategory: aCategory
	^self fileInMethods: (self organization listAtCategoryNamed: aCategory)
</details>

#### PseudoClass>>#fileOutMethodsOn: aStream

<details>
	<summary>See more</summary>
	
	fileOutMethodsOn: aStream
	^self fileOutMethods: self selectors on: aStream.
</details>

#### PseudoClass>>#variablesAndOffsetsDo: aBinaryBlock

NOp


<details>
	<summary>See more</summary>
	
	variablesAndOffsetsDo: aBinaryBlock
	"NOp"
</details>

#### PseudoClass>>#category

<details>
	<summary>See more</summary>
	
	category
	^nil
</details>

#### PseudoClass>>#realClass

<details>
	<summary>See more</summary>
	
	realClass
	^Smalltalk at: self name asSymbol ifAbsent: nil
</details>

#### PseudoClass>>#renameTo: aString

If the receiver has an inherent idea about its own name, it should take action here. Any object that might be pointed to in the References dictionary might get this message sent to it upon reload


<details>
	<summary>See more</summary>
	
	renameTo: aString

	self hasDefinition ifTrue:[
		self isMeta ifTrue:[
			self definition: (self definition
				copyReplaceAll: name,' class'
				with: aString, ' class').
		] ifFalse:[
			self definition: (self definition 
					copyReplaceAll:'ubclass: #',name
					with:'ubclass: #', aString)]].
	name := aString.
	metaClass ifNotNil:[metaClass renameTo: aString].
</details>

#### PseudoClass>>#lastUnderscoreMeansSubscript

Redefine this method on the class side of those classes where you want a_1 to be shown as 'a subscript 1'


<details>
	<summary>See more</summary>
	
	lastUnderscoreMeansSubscript
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false
</details>

#### PseudoClass>>#fileOutMethod: selector

<details>
	<summary>See more</summary>
	
	fileOutMethod: selector

	DirectoryEntry smalltalkImageDirectory // (name,'-', selector asFileName, '.st') writeStreamDo: [ :f |
		self fileOutMethods: (Array with: selector) on: f ]
</details>

#### PseudoClass>>#classComment: aChangeRecord

<details>
	<summary>See more</summary>
	
	classComment: aChangeRecord
	self organization classComment: aChangeRecord
</details>

#### PseudoClass>>#methodChange: aChangeRecord

<details>
	<summary>See more</summary>
	
	methodChange: aChangeRecord
	aChangeRecord isMetaClassChange ifTrue:[
		^self theMetaClass addMethodChange: aChangeRecord.
	] ifFalse:[
		^self addMethodChange: aChangeRecord.
	].

</details>

#### PseudoClass>>#fileOut

<details>
	<summary>See more</summary>
	
	fileOut

	DirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStreamDo: [ :stream |
		self fileOutOn: stream.
		self needsInitialize ifTrue: [
			stream newLine; nextChunkPut: self name,' initialize' ]]
</details>

#### PseudoClass>>#comment: aString

<details>
	<summary>See more</summary>
	
	comment: aString
	self classComment: aString asString
</details>

#### PseudoClass>>#allInstVarNames

<details>
	<summary>See more</summary>
	
	allInstVarNames
	^#()
</details>

#### PseudoClass>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #PseudoClass or: [ super is: aSymbol ]
</details>

#### PseudoClass>>#printHierarchy

<details>
	<summary>See more</summary>
	
	printHierarchy
	
	^'Hierarchy view not supported'
</details>

#### PseudoClass>>#sourceCodeAt: sel

<details>
	<summary>See more</summary>
	
	sourceCodeAt: sel
	^(self sourceCode at: sel) string
</details>

#### PseudoClass>>#fileOutCategory: categoryName

<details>
	<summary>See more</summary>
	
	fileOutCategory: categoryName

	DirectoryEntry smalltalkImageDirectory // (self name,'-',categoryName,'.st') writeStreamDo: [ :f |
		self fileOutMethods: (self organization listAtCategoryNamed: categoryName) on: f ]
</details>

#### PseudoClass>>#fileInMethods

<details>
	<summary>See more</summary>
	
	fileInMethods
	^self fileInMethods: self selectors
</details>

#### PseudoClass>>#removeSelectorIfInBaseSystem: selector

<details>
	<summary>See more</summary>
	
	removeSelectorIfInBaseSystem: selector
	self sourceCode removeKey: selector ifAbsent: [^ self].
	self organization removeElement: selector
</details>

#### PseudoClass>>#removeMethod: selector

<details>
	<summary>See more</summary>
	
	removeMethod: selector
	self organization removeElement: selector.
	self sourceCode removeKey: selector.

</details>

#### PseudoClass>>#isMeta

<details>
	<summary>See more</summary>
	
	isMeta
	^false
</details>

#### PseudoClass>>#stampAt: selector

<details>
	<summary>See more</summary>
	
	stampAt: selector
	| record |
	record _ (self sourceCode at: selector).
	^(record is: #Text)
		ifTrue: [ '']
		ifFalse: [ record stamp ]
</details>

#### PseudoClass>>#instVarNames

<details>
	<summary>See more</summary>
	
	instVarNames
	^ #()
</details>

#### PseudoClass>>#sharedPools

<details>
	<summary>See more</summary>
	
	sharedPools 
	self exists ifFalse: [^ nil].
	^ self realClass sharedPools
</details>

#### PseudoClass>>#includesSelector: aSymbol

<details>
	<summary>See more</summary>
	
	includesSelector: aSymbol
	^ source keys includes: aSymbol.
</details>

#### PseudoClass>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^name
</details>

#### PseudoClass>>#fileOutMethods: aCollection on: aStream

FileOut all methods with selectors taken from aCollection


<details>
	<summary>See more</summary>
	
	fileOutMethods: aCollection on: aStream
	"FileOut all methods with selectors taken from aCollection"
	| categories |
	categories := Dictionary new.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			(categories includesKey: cat) 
				ifFalse:[ categories at: cat put: Set new ].
			(categories at: cat) add: sel].
	].
	categories associationsDo:[:assoc|
		assoc value do: [ :sel |
			aStream newLine.
			(self sourceCode at: sel) fileOutOn: aStream.
		].
	].
</details>

#### PseudoClass>>#whichCategoryIncludesSelector: aSelector

Answer the category of the argument, aSelector, in the organization of the receiver, or answer nil if the receiver does not inlcude this selector.


<details>
	<summary>See more</summary>
	
	whichCategoryIncludesSelector: aSelector 
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	^ self organization categoryOfElement: aSelector
</details>

#### PseudoClass>>#fullName

<details>
	<summary>See more</summary>
	
	fullName
	^self name
</details>

#### PseudoClass>>#confirmRemovalOf: aString

<details>
	<summary>See more</summary>
	
	confirmRemovalOf: aString
	^self confirm:'Remove ',aString,' ?'
</details>

#### PseudoClass>>#sourceCodeAt: sel put: object

<details>
	<summary>See more</summary>
	
	sourceCodeAt: sel put: object
	self sourceCode at: sel put: object
</details>

#### PseudoClass>>#fileInDefinition

<details>
	<summary>See more</summary>
	
	fileInDefinition
	(self makeSureSuperClassExists: self superclassName) ifFalse:[^self].
	self hasDefinition ifTrue:[
		Transcript newLine; show:'Defining ', self name.
		self evaluate: self definition].
	self exists ifFalse:[^self].
	self hasComment ifTrue:[self realClass classComment: self comment].
</details>

#### PseudoClass>>#allCallsOn

<details>
	<summary>See more</summary>
	
	allCallsOn
	^ (self realClass ifNil: [ ^#() ]) allCallsOn
</details>

#### PseudoClass>>#removeAllUnmodified

<details>
	<summary>See more</summary>
	
	removeAllUnmodified
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass comment asString = self comment asString]) ifTrue:[ self classComment: nil].
	metaClass ifNotNil: [ metaClass removeAllUnmodified]
</details>

#### PseudoClass>>#compilerClass

<details>
	<summary>See more</summary>
	
	compilerClass
	^ (Smalltalk at: name ifAbsent: [^ Compiler]) compilerClass
</details>

#### PseudoClass>>#exists

<details>
	<summary>See more</summary>
	
	exists
	^(Smalltalk at: self name asSymbol ifAbsent:[^false]) isKindOf: Behavior
</details>

#### PseudoClass>>#theNonMetaClass

Sent to a class or metaclass, always return the class


<details>
	<summary>See more</summary>
	
	theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^self
</details>

#### PseudoClass>>#removeCategory: selector

<details>
	<summary>See more</summary>
	
	removeCategory: selector
	(self organization listAtCategoryNamed: selector) do:[:sel|
		self organization removeElement: sel.
		self sourceCode removeKey: sel.
	].
	self organization removeCategory: selector.
</details>

#### PseudoClass>>#hasChanges

<details>
	<summary>See more</summary>
	
	hasChanges

	self sourceCode isEmpty ifFalse:[^true].
	self organization hasNoComment ifFalse:[^true].
	definition ifNotNil: [ ^true].
	metaClass ifNotNil: [ ^metaClass hasChanges].
	^false
</details>

#### PseudoClass>>#comment

<details>
	<summary>See more</summary>
	
	comment
	| rStr |
	rStr := self organization commentRemoteStr.
	^rStr
		ifNil: [ self name,' has not been commented in this file']
		ifNotNil: [ rStr string]
</details>

#### PseudoClass>>#makeSureSuperClassExists: aString

<details>
	<summary>See more</summary>
	
	makeSureSuperClassExists: aString
	| theClass |
	theClass := Smalltalk at: (aString asSymbol) ifAbsent: nil.
	theClass ifNotNil:[^true].
	^self confirm: 'The super class ',aString,' does not exist in the system. Use nil instead?'.
</details>

#### PseudoClass>>#evaluate: aString

<details>
	<summary>See more</summary>
	
	evaluate: aString
	^Compiler evaluate: aString for: nil logged: true
</details>

#### PseudoClass>>#removeSelector: aSelector

<details>
	<summary>See more</summary>
	
	removeSelector: aSelector
	| catName |
	catName := self class removedCategoryName.
	self organization addCategory: catName before: self organization categories first.
	self organization classify: aSelector under: catName.
	self sourceCodeAt: aSelector put:'methodWasRemoved' asText.
</details>

#### PseudoClass>>#parserClass

<details>
	<summary>See more</summary>
	
	parserClass

	^ Compiler parserClass
</details>

#### PseudoClass>>#allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean

<details>
	<summary>See more</summary>
	
	allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean
	^ (self realClass ifNil: [ ^self ])
		allSubclassesWithLevelDo: classAndLevelBlock
		startingLevel: level
		sortByCategory: aBoolean
</details>

#### PseudoClass>>#closuresInfoAt: selector

<details>
	<summary>See more</summary>
	
	closuresInfoAt: selector
	^''
</details>

#### PseudoClass>>#hasDefinition

<details>
	<summary>See more</summary>
	
	hasDefinition
	^definition notNil
</details>

#### PseudoClass>>#literalScannedAs: scannedLiteral notifying: requestor

<details>
	<summary>See more</summary>
	
	literalScannedAs: scannedLiteral notifying: requestor 
	^ scannedLiteral
</details>

#### PseudoClass>>#compiledMethodAt: selector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	compiledMethodAt: selector ifAbsent: aBlock
	^aBlock value
</details>

#### PseudoClass>>#fileInDefinitionAndMetaclass

<details>
	<summary>See more</summary>
	
	fileInDefinitionAndMetaclass
	self fileInDefinition.
	metaClass ifNotNil: [ metaClass fileInDefinition ]
</details>

#### PseudoClass>>#fileInMethod: selector

<details>
	<summary>See more</summary>
	
	fileInMethod: selector
	^self fileInMethods: (Array with: selector)
</details>

#### PseudoClass>>#definition

<details>
	<summary>See more</summary>
	
	definition

	^definition ifNil: [
		Text fromString: 'There is no class definition for ', self name, ' in this file'].
</details>

#### PseudoClass>>#organization

<details>
	<summary>See more</summary>
	
	organization
	organization ifNil: [organization _ PseudoClassOrganizer defaultList: #()].

	"Making sure that subject is set correctly. It should not be necessary."
	organization setSubject: self.
	^ organization
</details>

#### PseudoClass>>#classPool

<details>
	<summary>See more</summary>
	
	classPool 
	self exists ifFalse: [^ nil].
	^ self realClass classPool
</details>

#### PseudoClass>>#allSuperclasses

<details>
	<summary>See more</summary>
	
	allSuperclasses
	^ (self realClass ifNil: [ ^#() ]) allSuperclasses
</details>

#### PseudoClass>>#fileInMethods: aCollection

FileIn all methods with selectors taken from aCollection


<details>
	<summary>See more</summary>
	
	fileInMethods: aCollection
	"FileIn all methods with selectors taken from aCollection"
	| theClass |
	self exists ifFalse:[^self classNotDefined].
	theClass := self realClass.
	aCollection do:[:sel|
		| cat |
		cat := self organization categoryOfElement: sel.
		cat = self class removedCategoryName ifFalse:[
			theClass 
				compile: (self sourceCodeAt: sel) 
				classified: cat
				withStamp: (self stampAt: sel)
				notifying: nil.
		].
	].
</details>

#### PseudoClass>>#selectors

<details>
	<summary>See more</summary>
	
	selectors

	^ self sourceCode keys
</details>

#### PseudoClass>>#browseClassVarRefs

<details>
	<summary>See more</summary>
	
	browseClassVarRefs
</details>

#### PseudoClass>>#sourceCode

<details>
	<summary>See more</summary>
	
	sourceCode
	^source ifNil:[source := Dictionary new]
</details>

#### PseudoClass>>#nameExists

<details>
	<summary>See more</summary>
	
	nameExists
	^Smalltalk includesKey: self name asSymbol
</details>

#### PseudoClass>>#fileIn

FileIn the receiver


<details>
	<summary>See more</summary>
	
	fileIn
	"FileIn the receiver"
	self hasDefinition ifTrue:[self fileInDefinition].
	self fileInMethods: self selectors.
	metaClass ifNotNil:[metaClass fileIn].
	self needsInitialize ifTrue:[
		self evaluate: self name,' initialize'.
	].
</details>

#### PseudoClass>>#hasComment

<details>
	<summary>See more</summary>
	
	hasComment
	^self organization commentRemoteStr notNil
</details>

#### PseudoClass>>#needsInitialize

<details>
	<summary>See more</summary>
	
	needsInitialize
	^self hasMetaclass and:[
		self theMetaClass realClass includesSelector: #initialize]
</details>

#### PseudoClass>>#fileOutOn: aStream

FileOut the receiver


<details>
	<summary>See more</summary>
	
	fileOutOn: aStream
	"FileOut the receiver"
	self fileOutDefinitionOn: aStream.
	metaClass ifNotNil:[metaClass fileOutDefinitionOn: aStream].
	self fileOutMethods: self selectors on: aStream.
	metaClass ifNotNil:[metaClass fileOutMethods: metaClass selectors on: aStream].
</details>

#### PseudoClass>>#hasMetaclass

<details>
	<summary>See more</summary>
	
	hasMetaclass
	^metaClass notNil
</details>

#### PseudoClass>>#theMetaClass

<details>
	<summary>See more</summary>
	
	theMetaClass
	^ metaClass ifNil: [ metaClass _ PseudoMetaclass new name: self name ].
</details>

#### PseudoClass>>#classNotDefined

<details>
	<summary>See more</summary>
	
	classNotDefined
	^self inform: self name,' is not defined in the system.
You have to define this class first.'.
</details>

## PseudoClassOrganizer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PseudoClassOrganizer>>#classComment: aChangeRecord

Store the comment, aString, associated with the object that refers to the receiver.


<details>
	<summary>See more</summary>
	
	classComment: aChangeRecord
	classComment := aChangeRecord
</details>

#### PseudoClassOrganizer>>#classComment

Answer the comment associated with the object that refers to the receiver.


<details>
	<summary>See more</summary>
	
	classComment
	"Answer the comment associated with the object that refers to the receiver."
	^classComment ifNil: [ '' ]
</details>

#### PseudoClassOrganizer>>#setDefaultList: anArray

<details>
	<summary>See more</summary>
	
	setDefaultList: anArray
	super setDefaultList: anArray.
	self classComment: nil
</details>

#### PseudoClassOrganizer>>#isRemoved: aSelector

<details>
	<summary>See more</summary>
	
	isRemoved: aSelector
	^(self categoryOfElement: aSelector) = PseudoClass removedCategoryName
</details>

## PseudoMetaclass

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PseudoMetaclass>>#realClass

<details>
	<summary>See more</summary>
	
	realClass
	^super realClass class
</details>

#### PseudoMetaclass>>#isMeta

<details>
	<summary>See more</summary>
	
	isMeta
	^true
</details>

#### PseudoMetaclass>>#fullName

<details>
	<summary>See more</summary>
	
	fullName
	^self name,' class'
</details>

#### PseudoMetaclass>>#theNonMetaClass

Sent to a class or metaclass, always return the class


<details>
	<summary>See more</summary>
	
	theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^self realClass theNonMetaClass
</details>

