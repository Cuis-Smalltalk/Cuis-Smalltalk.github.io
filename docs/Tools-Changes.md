## ChangeList

A ChangeList represents a list of changed methods that reside on a file in fileOut format. The classes and methods in my list are not necessarily in this image! Used as the model for both Version Lists and Changed Methods. It holds three lists: changeList - a list of ChangeRecords list - a list of one-line printable headers listSelections - a list of Booleans (true = selected, false = not selected) multiple OK. listIndex Items that are removed (removeDoits, remove an item) are removed from all three lists. Most recently clicked item is the one showing in the bottom pane.

### Methods
#### ChangeList>>#removeEmptyClassComments

<details>
	<summary>See more</summary>
	
	removeEmptyClassComments

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep |
		keep _ true.
		(chRec changeType == #classComment and: [
				Smalltalk includesKey: chRec changeClassName]) ifTrue: [
			keep _ chRec text notEmpty
		].
		keep ifTrue:[
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList.
		list _ newList.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list
</details>

#### ChangeList>>#selectEquivalentMethods

Selects all method definitions for which there is already an equivalent method in the current image, (meaning that the difference is cosmetic and not in behavior)


<details>
	<summary>See more</summary>
	
	selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and: [
				(class _ change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#scanDoIt

<details>
	<summary>See more</summary>
	
	scanDoIt
	
	| itemPosition command |

	itemPosition _ file position.
	command _ file nextChunk.
	
	command notEmpty ifTrue: [
		self
			addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
			text: 'do it: ' , (command contractTo: 160)]
</details>

#### ChangeList>>#scanFeatureRequirement: tokens

<details>
	<summary>See more</summary>
	
	scanFeatureRequirement: tokens
	
	| feature requirement |
	
	requirement _ FeatureRequirement 
		name: tokens second 
		minVersion: tokens third 
		minRevision: tokens fourth 
		maxVersion: (tokens size > 4 ifTrue: [tokens fifth]).
		
	feature _ FeatureChangeRecord new
		type: #requires
		feature: requirement.
					
	self addItem: feature text: feature string.
</details>

#### ChangeList>>#scanUpTo: stopPosition informing: barBlock

<details>
	<summary>See more</summary>
	
	scanUpTo: stopPosition informing: barBlock
	
	[file position < stopPosition] whileTrue: [ | prevChar |
		barBlock value: file position.
		prevChar _ self notSeparatorChar. 
		"A line starting with $! means a specific ChangeRecord type"
		(file peekFor: $!)
			ifTrue: [ self scanSpecificChangeRecordTypeIfNotAtEnd: prevChar]
			ifFalse: [ self scanDoIt ]]
</details>

#### ChangeList>>#listSelectionAt: index put: value

<details>
	<summary>See more</summary>
	
	listSelectionAt: index put: value

	^ listSelections at: index put: value
</details>

#### ChangeList>>#scanFile: aFile from: startPosition to: stopPosition

<details>
	<summary>See more</summary>
	
	scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.

	'Scanning ', aFile localName
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :barBlock | self scanUpTo: stopPosition informing: barBlock ].
		
	self clearSelections
</details>

#### ChangeList>>#selectSuchThat: aBlock

select all changes for which block returns true


<details>
	<summary>See more</summary>
	
	selectSuchThat: aBlock
	"select all changes for which block returns true"
	listSelections _ changeList collect: aBlock.
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#notSeparatorChar

<details>
	<summary>See more</summary>
	
	notSeparatorChar

	| prevChar |
	
	[file atEnd not and: [file peek isSeparator]] whileTrue: [prevChar _ file next].
	
	^prevChar
</details>

#### ChangeList>>#fileOutCurrentVersionsOfSelections

<details>
	<summary>See more</summary>
	
	fileOutCurrentVersionsOfSelections
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStreamDo: [ :stream |
				stream timeStamp.
				self currentVersionsOfSelections do: [ :methodRef |
					methodRef actualClass
						printMethodChunk: methodRef methodSymbol
						withPreamble: true
						on: stream
						moveSource: false
						toFile: 0 ]]
</details>

#### ChangeList>>#scanAndIgnore: item in: position

<details>
	<summary>See more</summary>
	
	scanAndIgnore: item in: position

	| record |
	
	record _ ChangeRecord new 
		file: file 
		position: position 
		type: #preamble.
		
	self 
		addItem: record 
		text: ('preamble: ' , item contractTo: 160)

</details>

#### ChangeList>>#scanFeatureProvision: tokens

<details>
	<summary>See more</summary>
	
	scanFeatureProvision: tokens

	| feature |
	
	feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: tokens second version: tokens third revision: tokens fourth).
					
	self addItem: feature text: feature string
</details>

#### ChangeList>>#isNewMethod: change

<details>
	<summary>See more</summary>
	
	isNewMethod: change
	| class |
	^change changeType == #method and: [
		((class _ change changeClass) isNil) or: [
			(class includesSelector: change methodSelector) not]]
</details>

#### ChangeList>>#fileInSelections

<details>
	<summary>See more</summary>
	
	fileInSelections 
	| any |
	any _ false.
	listSelections with: changeList do: 
		[:selected :item | selected ifTrue: [any _ true. item fileIn]].
	any ifFalse:
		[self inform: 'nothing selected, so nothing done']
</details>

#### ChangeList>>#file

<details>
	<summary>See more</summary>
	
	file
	^file
</details>

#### ChangeList>>#selectMethodsOlderThanCurrent

Selects all method definitions for which there is some newer counterpart in the current image. This is, select methods that installing would be go back in time


<details>
	<summary>See more</summary>
	
	selectMethodsOlderThanCurrent
	"Selects all method definitions for which there is some newer counterpart in the current image.
	This is, select methods that installing would be go back in time"

	| cm doSelect change class |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		doSelect _ false.
		((change changeType == #method and: [ change isMethodDeletion not ] ) and: [
			((class _ change changeClass) notNil)]) ifTrue: [ | current |
				cm _ class compiledMethodAt: change methodSelector ifAbsent: nil.
				current _ cm ifNotNil: [ cm dateAndTime ].
				current ifNotNil: [ 
					doSelect _ change dateAndTime ifNil: [true] ifNotNil: [ :dateAndTime | dateAndTime < current ]]].
		listSelections at: i put: doSelect ].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#xtraVersion

<details>
	<summary>See more</summary>
	
	xtraVersion
	| change class |
	listIndex = 0
		ifTrue: [^ nil ].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifFalse: [ ^nil ].
	^change
</details>

#### ChangeList>>#selectedClass

<details>
	<summary>See more</summary>
	
	selectedClass
	^(self selectedClassOrMetaClass ifNil: [ ^nil ]) theNonMetaClass 
</details>

#### ChangeList>>#initialize

Initialize a blank ChangeList. Set the contentsSymbol to reflect whether diffs will initally be shown or not


<details>
	<summary>See more</summary>
	
	initialize
	"Initialize a blank ChangeList.  Set the contentsSymbol to reflect whether diffs will initally be shown or not"

	contentsSymbol _ Preferences diffsInChangeList
		ifTrue:
			[self defaultDiffsSymbol]
		ifFalse:
			[#source].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	super initialize
</details>

#### ChangeList>>#selectAllForAbsentClasses

Selects all method definitions for which there is no counterpart method in the current image


<details>
	<summary>See more</summary>
	
	selectAllForAbsentClasses
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put:
			((#(method classComment classDefinition) includes: change changeType)
				and: [ change changeClass isNil ])].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#destroyCurrentCodeOfSelections

Actually remove from the system any in-memory methods with class and selector identical to items current selected. This may seem rather arcane but believe me it has its great uses, when trying to split out code. To use effectively, first file out a change set that you wish to split off. Then open a ChangeList browser on that fileout. Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command. For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!


<details>
	<summary>See more</summary>
	
	destroyCurrentCodeOfSelections
	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!"

	|  aClass aChange aList |
	aList _ OrderedCollection new.
	1 to: changeList size do:
		[:index |
			(listSelections at: index) ifTrue:
				[aChange _ changeList at: index.
				(aChange changeType == #method
					and: [(aClass _ aChange changeClass) notNil
					and: [aClass includesSelector: aChange methodSelector]])
						ifTrue:
							[aList add: {aClass. aChange methodSelector}]]].

	aList size > 0 ifTrue: [
		(self confirm: 'Warning! This will actually remove ', aList size printString,  ' method(s) from the system!') ifFalse: [^ self]].
	aList do: [ :aPair |
		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.
		aPair first removeSelector: aPair second ]
</details>

#### ChangeList>>#removeOlderMethodVersions

Remove older versions of entries from the receiver.


<details>
	<summary>See more</summary>
	
	removeOlderMethodVersions
	"Remove older versions of entries from the receiver."

	| newChangeList newList found |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	found _ OrderedCollection new.
	changeList reverseWith: list do: [ :chRec :strNstamp | | str |
		str _ strNstamp copyUpTo: $;.
		(found includes: str) ifFalse: [
			found add: str.
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList reversed.
		list _ newList reversed.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list
</details>

#### ChangeList>>#classDefinitionRecordFrom: tokens

<details>
	<summary>See more</summary>
	
	classDefinitionRecordFrom: tokens
	
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition _ file position.
	fullClassName _ tokens second.
	isMeta _ fullClassName includesSubString: ' class'.
	className _ isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category _ CompiledMethod classCategoryFrom: tokens.
	stamp _ CompiledMethod stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ChangeRecord new 
		file: file 
		position: itemPosition 
		type: #classDefinition 
		class: className asSymbol 
		category: category 
		meta: isMeta 
		stamp: stamp.
		
	record markAsTest: (classDefinition beginsWith: TestCase name asString).
	
	^record

</details>

#### ChangeList>>#scanClassRenamed: tokens

<details>
	<summary>See more</summary>
	
	scanClassRenamed: tokens

	| record stamp newName previousName preamble |

	preamble _ tokens first.
	previousName _ tokens second.
	newName _ tokens fourth.
	stamp _ CompiledMethod stampFrom: tokens.
	file nextChunk.
	
	record _ ClassRenamedChangeRecord from: previousName to: newName stamp: stamp.
			
	self 
		addItem: record
		text: preamble, previousName, ' - ', newName, '; ', stamp 
</details>

#### ChangeList>>#invertSelections

Invert the selectedness of each item in the changelist


<details>
	<summary>See more</summary>
	
	invertSelections
	"Invert the selectedness of each item in the changelist"

	listSelections _ listSelections collect: [ :ea | ea not].
	listIndex _ 0.
	self changed: #allSelections.
	self acceptedContentsChanged
</details>

#### ChangeList>>#compareToCurrentVersion

If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text


<details>
	<summary>See more</summary>
	
	compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change class s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifTrue: [
			s1 _ (class sourceCodeAt: change methodSelector) asString.
			s2 _ change string.
			s1 = s2
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords _ self shouldDiffWords.
			differDesc _ diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: s1 to: s2
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: Preferences standardCodeFont))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]
</details>

#### ChangeList>>#currentVersionsOfSelections

Opens a message-list browser on the current in-memory versions of all methods that are currently seleted


<details>
	<summary>See more</summary>
	
	currentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ OrderedCollection new.
	1 to: changeList size do: [ :i |
		(listSelections at: i) ifTrue: [
			| aClass aChange |
			aChange _ changeList at: i.
			(aChange changeType == #method
				and: [(aClass _ aChange changeClass) notNil
				and: [aClass includesSelector: aChange methodSelector]])
					ifTrue: [
						aList add: (
							MethodReference new
								setStandardClass: aClass  
								methodSymbol: aChange methodSelector
						)
					]]].
	^ aList
</details>

#### ChangeList>>#removeSelections

Remove the selected items from the receiver. 9/18/96 sw


<details>
	<summary>See more</summary>
	
	removeSelections
	"Remove the selected items from the receiver.  9/18/96 sw"

	| newChangeList newList |

	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	1 to: changeList size do: [ :i |
		(listSelections at: i) ifFalse: [
			newChangeList add: (changeList at: i).
			newList add: (list at: i)]].
	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list

	
</details>

#### ChangeList>>#currentChange

return the current change being viewed, or nil if none


<details>
	<summary>See more</summary>
	
	currentChange
	"return the current change being viewed, or nil if none"

	^ listIndex = 0
		ifFalse: [ changeList at: listIndex ]
</details>

#### ChangeList>>#scanClassRemoval: tokens

<details>
	<summary>See more</summary>
	
	scanClassRemoval: tokens

	| doItOnlyIfInBaseSystem removeType classDefinition className description record stamp |
	
	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #classMoveToSomePackage:.
	stamp _ CompiledMethod stampFrom: tokens.
	classDefinition _ file nextChunk.
	
	record _ ClassDeletionChangeRecord 
			className: className
			definition: classDefinition 
			doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
			stamp: stamp.

	description _ doItOnlyIfInBaseSystem 
			ifTrue: ['clase move to some package: '] 
			ifFalse: ['class removal: '].
			
	self 
		addItem: record
		text: description, className, '; ', stamp 
</details>

#### ChangeList>>#diffedVersionContents

Answer diffed version contents, maybe pretty maybe not


<details>
	<summary>See more</summary>
	
	diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change _ changeList at: listIndex.
	later _ change text.
	class _ change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier _ (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])
</details>

#### ChangeList>>#scanCategory: category class: class meta: meta stamp: stamp prior: aPriorMethod overrides: anOverridenMethod

<details>
	<summary>See more</summary>
	
	scanCategory: category class: class meta: meta stamp: stamp prior: aPriorMethod overrides: anOverridenMethod

	| itemPosition method |

	[
		itemPosition _ file position.
		method _ file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new 
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp
					prior: aPriorMethod
					overrides: anOverridenMethod)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]
</details>

#### ChangeList>>#selectedMessageName

<details>
	<summary>See more</summary>
	
	selectedMessageName
	| c |
	^ (c _ self currentChange) ifNotNil: [c methodSelector]
</details>

#### ChangeList>>#selectedClassOrMetaClass

<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	| c |
	^ (c _ self currentChange) ifNotNil: [c changeClass]
</details>

#### ChangeList>>#deselectAll

Deselect all items in the list pane, and clear the code pane


<details>
	<summary>See more</summary>
	
	deselectAll 
	"Deselect all items in the list pane, and clear the code pane"

	listIndex _ 0.
	listSelections atAllPut: false.
	self changed: #allSelections.
	self acceptedContentsChanged
</details>

#### ChangeList>>#toggleListIndex: newListIndex

<details>
	<summary>See more</summary>
	
	toggleListIndex: newListIndex

	listIndex ~= 0 ifTrue: [listSelections at: listIndex put: false].
	newListIndex ~= 0 ifTrue: [listSelections at: newListIndex put: true].
	listIndex _ newListIndex.
	self changed: #listIndex.
	self acceptedContentsChanged
</details>

#### ChangeList>>#wantsPrettyDiffOption

Answer whether pretty-diffs are meaningful for this tool


<details>
	<summary>See more</summary>
	
	wantsPrettyDiffOption
	"Answer whether pretty-diffs are meaningful for this tool"

	^ true
</details>

#### ChangeList>>#removeNonSelections

Remove the unselected items from the receiver.


<details>
	<summary>See more</summary>
	
	removeNonSelections
	"Remove the unselected items from the receiver."

	| newChangeList newList |

	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	1 to: changeList size do: [ :i | 
		(listSelections at: i) ifTrue: [
			newChangeList add: (changeList at: i).
			newList add: (list at: i)]].
	newChangeList size = 0 ifTrue: [
		^ self inform: 'That would remove everything.
Why would you want to do that?'].

	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list

	
</details>

#### ChangeList>>#itemIsRecognized: item

<details>
	<summary>See more</summary>
	
	itemIsRecognized: item

	^ self class knownPreambles anySatisfy: [ :preamble | item includesSubString: preamble ] 
</details>

#### ChangeList>>#contentsSymbolQuints

Answer a list of quintuplets representing information on the alternative views available in the code pane


<details>
	<summary>See more</summary>
	
	contentsSymbolQuints
	"Answer a list of quintuplets representing information on the alternative views available in the code pane"

	^ self sourceAndDiffsQuintsOnly
</details>

#### ChangeList>>#annotation

Answer the string to be shown in an annotation pane. Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact.


<details>
	<summary>See more</summary>
	
	annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ ''].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					String streamContents: [ :strm | | sel |
						sel _ change methodSelector.
						strm
							nextPutAll: change changeClassName;
							nextPutAll: ' >> ';
							nextPutAll: sel;
							nextPutAll: ' is not present in the system. It has '.
							count _ Smalltalk numberOfImplementorsOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 implementor']
								ifFalse: [count printOn: strm. strm nextPutAll: ' implementors' ].
							strm nextPutAll: ' and '.
							count _ Smalltalk numberOfSendersOf: sel.
							count = 1
								ifTrue: [strm nextPutAll: '1 sender.']
								ifFalse: [count printOn: strm. strm nextPutAll: ' senders.' ].
						]
					]
				ifFalse: [
					'current version: ', super annotation]]
</details>

#### ChangeList>>#contentsDiffedFromCurrent

Answer the contents diffed forward from current (in-memory) method version


<details>
	<summary>See more</summary>
	
	contentsDiffedFromCurrent
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange _ changeList at: listIndex.
	((aChange changeType == #method 
			and: [(aClass _ aChange changeClass) notNil]) 
			and: [aClass includesSelector: aChange methodSelector]) ifTrue: [
		aSelector _ aChange methodSelector.
		(aClass notNil and: [aClass includesSelector: aSelector])
			ifFalse: [ ^aChange text copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange text
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange changeType == #classDefinition and: [
			name _ aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass _ Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass _ aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	(aChange changeType == #classComment
			and: [(aClass _ aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass comment to: aChange text tryWords: self shouldDiffWords].
	^(changeList at: listIndex) text
</details>

#### ChangeList>>#removeUpToDate

Remove all up to date version of entries from the receiver


<details>
	<summary>See more</summary>
	
	removeUpToDate
	"Remove all up to date version of entries from the receiver"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.
	changeList with: list do: [ :chRec :strNstamp | | keep cls name |
		keep _ chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
		keep ifTrue: [
			(cls _ chRec changeClass) ifNotNil: [ | sel str |
				str _ chRec string.
				sel _ chRec methodSelector.
				keep _ chRec isMethodDeletion
					ifTrue: [cls includesSelector: sel]
					ifFalse: [(cls sourceCodeAt: sel ifAbsent: nil)  ~= str]]].
		(chRec changeType == #classComment and: [
				name _ chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls _ Smalltalk at: name.
			keep _ cls organization classComment ~= chRec text ].
		(chRec changeType == #classDefinition and: [
				name _ chRec changeClassName.
				Smalltalk includesKey: name]) ifTrue: [
			cls _ Smalltalk at: name.
			chRec isMetaClassChange ifTrue: [ cls _ cls class ].
			keep _ cls definition ~= chRec text ].
		keep ifTrue: [
			newChangeList add: chRec.
			newList add: strNstamp]].
	newChangeList size < changeList size ifTrue: [
		changeList _ newChangeList.
		list _ newList.
		listIndex _ 0.
		self clearSelections ].
	self changed: #list
</details>

#### ChangeList>>#changeList

<details>
	<summary>See more</summary>
	
	changeList
	^ changeList
</details>

#### ChangeList>>#scanClassComment: tokens

<details>
	<summary>See more</summary>
	
	scanClassComment: tokens

	| className stamp record |
	
	className _ tokens first.
	stamp _ tokens third.
	record _ ChangeRecord new 
			file: file 
			position: file position 
			type: #classComment 
			class: className 
			category: nil 
			meta: false 
			stamp: stamp.
			
	self 
		addItem: record
		text: 'class comment for ' , className, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
		
	file nextChunk.

</details>

#### ChangeList>>#scanSpecificChangeRecordType

Scan anything that involves more than one chunk


<details>
	<summary>See more</summary>
	
	scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition _ file position.
	item _ file nextChunk.

	(self itemIsRecognized: item) ifFalse: [
		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition ].

	tokens _ Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition:
			ifTrue: [ ^ self scanClassDefinition: tokens ].
		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ])
			ifTrue: [ ^ self scanClassRemoval: tokens ].
		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ])
			ifTrue: [ ^ self scanMethodRemoval: tokens ].
		(secondToken == #methodsFor: or: [ tokens third == #methodsFor: ])
			ifTrue: [ ^ self scanMethodDefinition: tokens ].
		secondToken == #commentStamp:
			ifTrue: [ ^ self scanClassComment: tokens ].
		firstToken == #provides:
			ifTrue: [ ^ self scanFeatureProvision: tokens ].
		firstToken == #requires:
			ifTrue: [ ^ self scanFeatureRequirement: tokens ].
		firstToken == #classRenamed:
			ifTrue: [ ^ self scanClassRenamed: tokens ].
		]
</details>

#### ChangeList>>#selectUnchangedMethods

Selects all method definitions for which there is already a method in the current image, whose source is exactly the same. 9/18/96 sw


<details>
	<summary>See more</summary>
	
	selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	| change class |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and:
				[(class _ change changeClass) notNil]) and:
					[(class includesSelector: change methodSelector) and:
						[change string = (class sourceCodeAt: change methodSelector) asString ]])].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#selectRemovalsOfSent

Selects all method removal for sent methods


<details>
	<summary>See more</summary>
	
	selectRemovalsOfSent
	"Selects all method removal for sent methods"

	1 to: changeList size do: [ :i | | change |
		change _ changeList at: i.
		listSelections at: i put:
			(change isDoIt and: [
				change string includesSubString: 'removeSelector: #' ] and: [
					Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]) ].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#selectAll

<details>
	<summary>See more</summary>
	
	selectAll
	listIndex _ 0.
	listSelections atAllPut: true.
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#scanMethodDefinition: tokens

<details>
	<summary>See more</summary>
	
	scanMethodDefinition: tokens

	| stamp className priorMethod overridenMethod |
	
	className _ tokens first.
	stamp _ CompiledMethod stampFrom: tokens.
	priorMethod _ CompiledMethod priorReferenceFrom: tokens.
	overridenMethod _ CompiledMethod overridenMethodReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorMethod overrides: overridenMethod ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorMethod overrides: overridenMethod ].
	
	self error: 'Unsupported method definition' 

</details>

#### ChangeList>>#listSelectionAt: index

<details>
	<summary>See more</summary>
	
	listSelectionAt: index
	^ listSelections at: index
</details>

#### ChangeList>>#showsVersions

<details>
	<summary>See more</summary>
	
	showsVersions
	^ false
</details>

#### ChangeList>>#undiffedContents

<details>
	<summary>See more</summary>
	
	undiffedContents
	^ listIndex = 0
		ifTrue: ['']
		ifFalse: [(changeList at: listIndex) text]
</details>

#### ChangeList>>#scanMethodRemoval: tokens

<details>
	<summary>See more</summary>
	
	scanMethodRemoval: tokens

	| doItOnlyIfInBaseSystem removeType isMeta sourceCode methodReference className description record selector stamp |

	removeType _ tokens first.
	className _ tokens second.
	doItOnlyIfInBaseSystem _ removeType == #methodMoveToSomePackage:.

	sourceCode _ file nextChunk.
	isMeta _ tokens third == #class.	
	selector _ isMeta ifTrue: [ tokens fourth ] ifFalse: [ tokens third ].
	stamp _ CompiledMethod stampFrom: tokens.

	methodReference _ MethodReference new
		setClassSymbol: className
		classIsMeta: isMeta
		methodSymbol: selector
		stringVersion: className, (isMeta ifTrue: [' class '] ifFalse: [' ']), selector,'; ', stamp.
		
	record _ MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem
		source: sourceCode 
		stamp: stamp.
	
	description _ doItOnlyIfInBaseSystem 
		ifTrue: ['method move to some package: '] 
		ifFalse: ['method removal: '].
		
	self
		addItem: record
		text: description, methodReference stringVersion
</details>

#### ChangeList>>#contentsSymbol

Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source. A nil value in the contentsSymbol slot will be set to #source by this method


<details>
	<summary>See more</summary>
	
	contentsSymbol

	self currentChange ifNotNil: [ :change |
		(self isNewMethod: change) ifTrue: [ ^#source ]].
	^super contentsSymbol
</details>

#### ChangeList>>#addItem: item text: text

<details>
	<summary>See more</summary>
	
	addItem: item text: text

	changeList addLast: item.
	list addLast: (text collect: [ :x | x isLineSeparator ifTrue: [$/] ifFalse: [x]])
</details>

#### ChangeList>>#scanSpecificChangeRecordTypeIfNotAtEnd: prevChar

<details>
	<summary>See more</summary>
	
	scanSpecificChangeRecordTypeIfNotAtEnd: prevChar

	(prevChar notNil and: [ prevChar isLineSeparator ]) ifTrue: [self scanSpecificChangeRecordType]
</details>

#### ChangeList>>#selectNewMethods

Selects all method definitions for which there is no counterpart method in the current image


<details>
	<summary>See more</summary>
	
	selectNewMethods
	"Selects all method definitions for which there is no counterpart method in the current image"

	| change |
	1 to: changeList size do: [ :i | 
		change _ changeList at: i.
		listSelections at: i put: (self isNewMethod: change)].
	self changed: #allSelections.
	self changed: #annotation
</details>

#### ChangeList>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				anSHTextStyler classOrMetaClass: cl.
				^true ]]].
	^false
</details>

#### ChangeList>>#removeDoIts

Remove doits from the receiver, other than initializes. 1/26/96 sw


<details>
	<summary>See more</summary>
	
	removeDoIts
	"Remove doits from the receiver, other than initializes. 1/26/96 sw"

	| newChangeList newList |
	newChangeList _ OrderedCollection new.
	newList _ OrderedCollection new.

	changeList with: list do: [ :chRec :str |
		(chRec isDoIt not or: [str endsWith: 'initialize'])
			ifTrue: [
				newChangeList add: chRec.
				newList add: str]].
	newChangeList size < changeList size
		ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	self changed: #list.

	
</details>

#### ChangeList>>#fileOutSelections

<details>
	<summary>See more</summary>
	
	fileOutSelections 
	
	(FillInTheBlankMorph
		request: 'Enter file name'
		initialAnswer: 'Filename.st'
		onCancel: [^nil])

			asFileEntry writeStreamDo: [ :stream |
				stream timeStamp.
				listSelections with: changeList do: [ :selected :item |
					selected ifTrue: [
						item fileOutOn: stream ]]]
</details>

#### ChangeList>>#clearSelections

<details>
	<summary>See more</summary>
	
	clearSelections
	listSelections _ Array new: list size withAll: false
</details>

#### ChangeList>>#acceptedStringOrText

Answer the contents string, obeying diffing directives if needed


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"Answer the contents string, obeying diffing directives if needed"

	^ self showingAnyKindOfDiffs
		ifFalse: [
			self undiffedContents]
		ifTrue: [
			self showsVersions
				ifTrue: [
					self diffedVersionContents]
				ifFalse: [
					self contentsDiffedFromCurrent]]
</details>

#### ChangeList>>#list

<details>
	<summary>See more</summary>
	
	list
	^ list
</details>

#### ChangeList>>#selectAllForThisClass

<details>
	<summary>See more</summary>
	
	selectAllForThisClass
	| name |
	self currentChange ifNil: [ ^self ].
	name _ self currentChange changeClassName.
	name ifNil: [ ^self ].
	^self selectSuchThat: [ :change |
		change changeClassName = name ].
</details>

#### ChangeList>>#listIndex

<details>
	<summary>See more</summary>
	
	listIndex
	^ listIndex
</details>

#### ChangeList>>#scanClassDefinition: tokens

<details>
	<summary>See more</summary>
	
	scanClassDefinition: tokens

	| record |
	
	record _ self classDefinitionRecordFrom: tokens.
						
	self addItem: record text: 'classDefinition: ', record changeClassName.
	
</details>

## ChangeListElement

I am the superclass of elements of a ChangeList. My subinstances are built from fileout files.

### Methods
#### ChangeListElement>>#isMethodDeletion

<details>
	<summary>See more</summary>
	
	isMethodDeletion
	^false
</details>

#### ChangeListElement>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn

	self subclassResponsibility 
</details>

#### ChangeListElement>>#isClassDeletion

<details>
	<summary>See more</summary>
	
	isClassDeletion
	^false
</details>

#### ChangeListElement>>#text

In Cuis, all source code is plain Strings


<details>
	<summary>See more</summary>
	
	text
	"In Cuis, all source code is plain Strings"
	^self string
</details>

#### ChangeListElement>>#performOn: aCodeFile

<details>
	<summary>See more</summary>
	
	performOn: aCodeFile

	 ^aCodeFile perform: (self changeType copyWith: $:) asSymbol with: self 
</details>

#### ChangeListElement>>#fileOutOn: aFileStream

<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream

	self subclassResponsibility 
</details>

#### ChangeListElement>>#isDoIt

<details>
	<summary>See more</summary>
	
	isDoIt

	^false
</details>

## ChangeListWithFileInErrors

This class is used to keep errors when filing in changes. I could have use ChangeList directly, selecting changes with errors, then removing them, etc., but it had some problems and that solution is more a hack. So, instances of this class will keep errors when filing in a change, and it allows the posibility to show the change with the error in a change list window. A doit change that signaled a MessageNotUnderstood is assume to not be an error becuase those kinds of things are evaluations in specific contexts that will obiously generate errors. All doits with errors could be assume not to be errors, but I limited to MNU type of errors to avoid filtering errors that should be shown.

### Methods
#### ChangeListWithFileInErrors>>#fileInAllKeepingErrors

<details>
	<summary>See more</summary>
	
	fileInAllKeepingErrors

	errors := Dictionary new.
	changeList do: [ :change | self fileInKeepingError: change ].

</details>

#### ChangeListWithFileInErrors>>#initialize

Initialize a blank ChangeList. Set the contentsSymbol to reflect whether diffs will initally be shown or not


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	errors := Dictionary new.
</details>

#### ChangeListWithFileInErrors>>#fileInKeepingError: change

<details>
	<summary>See more</summary>
	
	fileInKeepingError: change
	
	[ change fileIn ]
		on: Error 
		do: [ :anError | (self hasToKeep: anError for: change) ifTrue: [ errors at: change put: anError ]]
</details>

#### ChangeListWithFileInErrors>>#hasToKeep: anError for: change

<details>
	<summary>See more</summary>
	
	hasToKeep: anError for: change

	^(change isDoIt and: [ anError isKindOf: MessageNotUnderstood ]) not
</details>

#### ChangeListWithFileInErrors>>#hasFileInErrors

<details>
	<summary>See more</summary>
	
	hasFileInErrors

	^errors notEmpty
</details>

#### ChangeListWithFileInErrors>>#failedFileInChangesLabel

<details>
	<summary>See more</summary>
	
	failedFileInChangesLabel

	^'Changes that failed to file in'
</details>

#### ChangeListWithFileInErrors>>#ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList

<details>
	<summary>See more</summary>
	
	ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList 
	
	(self wasFiledInWithError: aChange) ifTrue: [ 
		newChangeList add: aChange.
		newList add: ((list at: anIndex) contractTo: 40), ' | ', (errors at: aChange) printString ]
</details>

#### ChangeListWithFileInErrors>>#removeSucessfullyFiledInChanges

<details>
	<summary>See more</summary>
	
	removeSucessfullyFiledInChanges

	| newChangeList newList |

	newChangeList := OrderedCollection new.
	newList := OrderedCollection new.

	changeList withIndexDo: [ :aChange :anIndex | self ifFiledInWithErrorAdd: aChange at: anIndex to: newChangeList and: newList ].
			
	changeList _ newChangeList.
	list _ newList.
	listIndex _ 0.
	self clearSelections.
	self changed: #list.
</details>

#### ChangeListWithFileInErrors>>#showChangesWithFileInErrors

<details>
	<summary>See more</summary>
	
	showChangesWithFileInErrors

	self removeSucessfullyFiledInChanges.
	ChangeListWindow open: self label: self failedFileInChangesLabel


</details>

#### ChangeListWithFileInErrors>>#wasFiledInWithError: aChange

<details>
	<summary>See more</summary>
	
	wasFiledInWithError: aChange
	
	^errors includesKey: aChange
</details>

## ChangeRecord

A ChangeRecord represents a change recorded on a file in fileOut format. It includes a type (more needs to be done here), and additional information for certain types such as method defs which need class and category.

### Methods
#### ChangeRecord>>#prior

<details>
	<summary>See more</summary>
	
	prior

	^prior
</details>

#### ChangeRecord>>#file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior

<details>
	<summary>See more</summary>
	
	file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior

	self file: aFile position: aPosition type: aType.
	class _ aClassName.
	category _ aClassCategory.
	meta _ isMeta.
	stamp _ aStamp.
	prior _ aPrior.
</details>

#### ChangeRecord>>#overridesASuperclassMethod

<details>
	<summary>See more</summary>
	
	overridesASuperclassMethod
	^ overrides notNil 
</details>

#### ChangeRecord>>#compilerClass

<details>
	<summary>See more</summary>
	
	compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass
</details>

#### ChangeRecord>>#string

The file is usually closed. But if it happens to be open, leave it like that.


<details>
	<summary>See more</summary>
	
	string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose _ file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string _ file nextChunk.
	mustOpenAndClose ifTrue: [
		file close].
	^ string
</details>

#### ChangeRecord>>#file: aFile position: aPosition type: aType

<details>
	<summary>See more</summary>
	
	file: aFile position: aPosition type: aType

	file _ aFile.
	position _ aPosition.
	type _ aType.
	
	self markAsTest: false.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"
</details>

#### ChangeRecord>>#changeClassName

<details>
	<summary>See more</summary>
	
	changeClassName
	^class
</details>

#### ChangeRecord>>#markAsTest: aBoolean

<details>
	<summary>See more</summary>
	
	markAsTest: aBoolean

	isTest := aBoolean 
</details>

#### ChangeRecord>>#stamp: threePartString

<details>
	<summary>See more</summary>
	
	stamp: threePartString

	stamp _ threePartString
</details>

#### ChangeRecord>>#fileName

<details>
	<summary>See more</summary>
	
	fileName
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]
</details>

#### ChangeRecord>>#file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior overrides: anOverridenMethod

<details>
	<summary>See more</summary>
	
	file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior overrides: anOverridenMethod

	self file: aFile position: aPosition type: aType.
	class _ aClassName.
	category _ aClassCategory.
	meta _ isMeta.
	stamp _ aStamp.
	prior _ aPrior.
	overrides _ anOverridenMethod.
</details>

#### ChangeRecord>>#methodSelector

<details>
	<summary>See more</summary>
	
	methodSelector
	^type == #method ifTrue:
		[(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string]
</details>

#### ChangeRecord>>#changeClass

<details>
	<summary>See more</summary>
	
	changeClass
	| methodClassName methodClass |
	(#(method classComment classDefinition) includes: type) ifFalse: [ ^ nil ].
	methodClassName _ class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass _ Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]
</details>

#### ChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp
	^ stamp
</details>

#### ChangeRecord>>#fileIn

File the receiver in. If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it.


<details>
	<summary>See more</summary>
	
	fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	self isDoIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]
</details>

#### ChangeRecord>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	super printOn: aStream.
	aStream 
		nextPutAll: ' - type: ';
		nextPutAll: type 
</details>

#### ChangeRecord>>#category

<details>
	<summary>See more</summary>
	
	category
	^category
</details>

#### ChangeRecord>>#isTestClassChange

<details>
	<summary>See more</summary>
	
	isTestClassChange

	^ isTest
</details>

#### ChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType
	^ type
</details>

#### ChangeRecord>>#isMetaClassChange

<details>
	<summary>See more</summary>
	
	isMetaClassChange
	^meta
</details>

#### ChangeRecord>>#fileOutOn: aFileStream

File the receiver out on the given file stream


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue: [
			aFileStream newLine; nextPut: $!.
			aString _  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil: [
				aString _ aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine
</details>

#### ChangeRecord>>#file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp

<details>
	<summary>See more</summary>
	
	file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp

	self 
		file: aFile 
		position: aPosition 
		type: aType 
		class: aClassName 
		category: aClassCategory 
		meta: isMeta 
		stamp: aStamp 
		prior: nil

</details>

#### ChangeRecord>>#isDoIt

<details>
	<summary>See more</summary>
	
	isDoIt

	^type = #doIt
</details>

#### ChangeRecord>>#dateAndTime

<details>
	<summary>See more</summary>
	
	dateAndTime

	^CompiledMethod timeStamp: self stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]
</details>

## ChangeSet

ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a "fileOut"). There are separate ChangeSets for caturing changes done to the Cuis base system and changes done to Packages. Usually the ChangeSets for Packages are not important, and ignored, because Packages are saved on pck.st files. Change sets for the Cuis base system are the standard way of capturing changes to Cuis, and are used to evolve Cuis itself. name - a String used to name the changeSet, and thus any associated project or fileOut. preamble and postscript: two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet. changeRecords - Dictionary {class name -> a ClassChangeRecord}. These classChangeRecords (qv) remember all of the system changes. structures - Dictionary {#Rectangle -> #(<classVersionInteger> 'origin' 'corner')}. Of the names of the instances variables before any changes for all classes in classChanges, and all of their superclasses. In the same format used in SmartRefStream. Inst var names are strings. superclasses - Dictionary {#Rectangle -> #Object}. Of all classes in classChanges, and all of their superclasses. Structures and superclasses save the instance variable names of this class and all of its superclasses. Later we can tell how it changed and write a conversion method. The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp). NOTE: It should be fairly simple, by adding a bit more information to the classChangeRecords, to reconstruct the information now stored in 'structures' and 'superclasses'. This would be a welcome simplification.

### Methods
#### ChangeSet>>#aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self noteRenameClass: aClass as: newClassName
</details>

#### ChangeSet>>#classDefinitionChangedFrom: oldClass to: newClass

<details>
	<summary>See more</summary>
	
	classDefinitionChangedFrom: oldClass to: newClass

	(newClass hasChangedComparedTo: oldClass) ifTrue: [ 
		self noteChangeClass: newClass from: oldClass ]
</details>

#### ChangeSet>>#movedClassRecords

<details>
	<summary>See more</summary>
	
	movedClassRecords

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassMoveToOtherPackage ]
</details>

#### ChangeSet>>#fileOutMovedClassRecord: aRemovedClassRecord on: stream

<details>
	<summary>See more</summary>
	
	fileOutMovedClassRecord: aRemovedClassRecord on: stream

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: true on: stream

</details>

#### ChangeSet>>#hasUnsavedChanges

<details>
	<summary>See more</summary>
	
	hasUnsavedChanges

	^hasUnsavedChanges
</details>

#### ChangeSet>>#printOn: aStream

2/7/96 sw: provide the receiver's name in the printout


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"2/7/96 sw: provide the receiver's name in the printout"
	super printOn: aStream.
	aStream nextPutAll: ' named ', self name
</details>

#### ChangeSet>>#atClass: class includes: changeType

<details>
	<summary>See more</summary>
	
	atClass: class includes: changeType

	^(changeRecords at: class name ifAbsent: [^false])
		includesChangeType: changeType
</details>

#### ChangeSet>>#hasPreamble

<details>
	<summary>See more</summary>
	
	hasPreamble
	^ preamble notNil
</details>

#### ChangeSet>>#methodsWithAnyInitialsOtherThan: myInits

Return a collection of method refs whose author appears to be different from the given one, even historically


<details>
	<summary>See more</summary>
	
	methodsWithAnyInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one, even historically"
	| slips |
	slips _ Set new.
	self changedClasses do: [:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					(aClass changeRecordsAt: mAssoc key) do: [ :chg | | aTimeStamp |
						aTimeStamp _ chg stamp.
						(aTimeStamp notNil and: [(aTimeStamp beginsWith: myInits) not])
							ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]]].
	^ slips
</details>

#### ChangeSet>>#methodsWithInitialsOtherThan: myInits

Return a collection of method refs whose author appears to be different from the given one


<details>
	<summary>See more</summary>
	
	methodsWithInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
					method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | aTimeStamp |
						((aTimeStamp _ method timeStamp) notNil and: [
							(aTimeStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"
</details>

#### ChangeSet>>#methodInfoFromRemoval: classAndSelector

<details>
	<summary>See more</summary>
	
	methodInfoFromRemoval: classAndSelector

	^ (self changeRecorderFor: classAndSelector first)
		infoFromRemoval: classAndSelector last
</details>

#### ChangeSet>>#author

<details>
	<summary>See more</summary>
	
	author
	| author |
	self assurePreambleExists.
	author _ self preambleString lineNumber: 3.
	author _ author copyFrom: 8 to: author size. "Strip the 'Author:' prefix. Ugly ugly."	
	^author withBlanksTrimmed.
	
</details>

#### ChangeSet>>#removeClassChanges: classOrClassName

Remove all memory of changes associated with this class


<details>
	<summary>See more</summary>
	
	removeClassChanges: classOrClassName
	"Remove all memory of changes associated with this class"
	| cname |
	cname _ classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	changeRecords removeKey: cname ifAbsent: nil.
	self noteClassForgotten: cname
</details>

#### ChangeSet>>#noteRenameClass: class as: newName

Include indication that a class has been renamed.


<details>
	<summary>See more</summary>
	
	noteRenameClass: class as: newName 
	"Include indication that a class has been renamed."

	| recorder |
	(recorder _ self changeRecorderFor: class)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: class name.
	self noteClassStructure: class.

	recorder _ changeRecords at: class class name ifAbsent: [nil].
	recorder ifNotNil: [
		changeRecords at: (newName, ' class') put: recorder.
		changeRecords removeKey: class class name.
		recorder noteNewName: newName , ' class' ].

	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#hasUnsavedChanges: aBoolean

<details>
	<summary>See more</summary>
	
	hasUnsavedChanges: aBoolean

	hasUnsavedChanges _ aBoolean.
	self triggerEvent: #dirtyFlagChanged
</details>

#### ChangeSet>>#initialize

Initialize the receiver to be empty.


<details>
	<summary>See more</summary>
	
	initialize 
	"Initialize the receiver to be empty."

	name ifNil:
		[^ self error: 'All changeSets must be registered, as in ChangeSorter newChangeSet'].
	self clear.

</details>

#### ChangeSet>>#methodChangesAtClass: className

Return an old-style dictionary of method change types.


<details>
	<summary>See more</summary>
	
	methodChangesAtClass: className
	"Return an old-style dictionary of method change types."

	^(changeRecords at: className ifAbsent: [^ Dictionary new])
		methodChangeTypes
</details>

#### ChangeSet>>#preambleString: aString

Establish aString as the new contents of the preamble.


<details>
	<summary>See more</summary>
	
	preambleString: aString
	"Establish aString as the new contents of the preamble.  "

	preamble _ TextModel withText: aString.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#removedClassRecords

<details>
	<summary>See more</summary>
	
	removedClassRecords

	^ changeRecords values select: [ :aChangeRecord | aChangeRecord isClassRemoval]
</details>

#### ChangeSet>>#isMoribund

Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter. 2/7/96 sw


<details>
	<summary>See more</summary>
	
	isMoribund
	"Answer whether the receiver is obsolete and about to die; part of an effort to get such guys cleared out from the change sorter.  2/7/96 sw"

	^ name == nil 
</details>

#### ChangeSet>>#methodsWithoutComments

Return a collection representing methods in the receiver which have no precode comments


<details>
	<summary>See more</summary>
	
	methodsWithoutComments
	"Return a collection representing methods in the receiver which have no precode comments"

	| slips |
	slips _ OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | (#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse:
					[(aClass selectors includes:  mAssoc key) ifTrue:
						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutComments) name: 'methods lacking comments'
	"
</details>

#### ChangeSet>>#sortedMovedClassesToOtherPackage

<details>
	<summary>See more</summary>
	
	sortedMovedClassesToOtherPackage

	^ self sortedClassRecords: self movedClassRecords
</details>

#### ChangeSet>>#isWithClass: aClass

<details>
	<summary>See more</summary>
	
	isWithClass: aClass 
	
	^changeRecords includesKey: aClass name
</details>

#### ChangeSet>>#fileOutMethodChangesFor: class on: stream

Write out all the method changes for this class.


<details>
	<summary>See more</summary>
	
	fileOutMethodChangesFor: class on: stream
	"Write out all the method changes for this class."

	| changes |
	changes _ Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		(mAssoc value == #remove
			or: [ mAssoc value == #addedThenRemoved
				or: [ mAssoc value == #add
					or: [ mAssoc value == #movedToOtherPackage ]]])
			ifFalse: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]
</details>

#### ChangeSet>>#classRemoved: aClass fromCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classRemoved: aClass fromCategory: aCategoryName

	self noteRemovalOf: aClass fromCategory: aCategoryName
</details>

#### ChangeSet>>#fileOutMethodsAdditionsOf: classList on: stream

<details>
	<summary>See more</summary>
	
	fileOutMethodsAdditionsOf: classList on: stream

	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ]
</details>

#### ChangeSet>>#classAdded: aClass inCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classAdded: aClass inCategory: aCategoryName

	self noteAddClass: aClass
</details>

#### ChangeSet>>#fileOutClassDefinitionsOf: classList on: stream

<details>
	<summary>See more</summary>
	
	fileOutClassDefinitionsOf: classList on: stream

	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ]
</details>

#### ChangeSet>>#fileOutMethodRemovalsFor: class on: stream

Write out removals and initialization for this class.


<details>
	<summary>See more</summary>
	
	fileOutMethodRemovalsFor: class on: stream 
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(remove addedThenRemoved) 
		movedToOtherPackage: false
		for: class 
		on: stream
		
</details>

#### ChangeSet>>#noteChangeClass: class from: oldClass

Remember that a class definition has been changed. Record the original structure, so that a conversion method can be built.


<details>
	<summary>See more</summary>
	
	noteChangeClass: class from: oldClass
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	class wantsChangeSetLogging ifFalse: [^ self].
	class isMeta 
		ifFalse: [self atClass: class add: #change]	"normal"
		ifTrue: [((self classChangeAt: class theNonMetaClass name) includes: #add) 
			ifTrue: [self atClass: class add: #add] 	"When a class is defined, the metaclass
				is not recorded, even though it was added.  A further change is
				really just part of the original add."
			ifFalse: [self atClass: class add: #change]].
	self addCoherency: class name.
	(self changeRecorderFor: class) notePriorDefinition: oldClass.
	self noteClassStructure: oldClass.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#name

The name of this changeSet. 2/7/96 sw: If name is nil, we've got garbage. Help to identify.


<details>
	<summary>See more</summary>
	
	name
	"The name of this changeSet.
	 2/7/96 sw: If name is nil, we've got garbage.  Help to identify."

	^ name
		ifNil: ['<no name -- garbage?>']
		ifNotNil: [name]
</details>

#### ChangeSet>>#fileOutMethodRemovalOf: selector for: class movedToOtherPackage: moved on: stream stamp: stamp

<details>
	<summary>See more</summary>
	
	fileOutMethodRemovalOf: selector for: class movedToOtherPackage: moved on: stream stamp: stamp 

	| methodReference changeRecord |
	
	methodReference := MethodReference class: class selector: selector.
	
	changeRecord := MethodDeletionChangeRecord 
		methodReference: methodReference
		doItOnlyIfInBaseSystem: moved
		source: (methodReference sourceCodeIfAbsent: [ 'Missing' ]) 
		stamp: stamp.
		
	changeRecord fileOutOn: stream 
</details>

#### ChangeSet>>#fileOutMovedClassesOn: stream

<details>
	<summary>See more</summary>
	
	fileOutMovedClassesOn: stream

	^ self sortedMovedClassesToOtherPackage do: [ :aMovedClassRecord |
		self fileOutMovedClassRecord: aMovedClassRecord on: stream ]
</details>

#### ChangeSet>>#askRenames: renamed addTo: msgSet using: smart

Go through the renamed classes. Ask the user if it could be in a project. Add a method in SmartRefStream, and a conversion method in the new class.


<details>
	<summary>See more</summary>
	
	askRenames: renamed addTo: msgSet using: smart
	| list |
	"Go through the renamed classes.  Ask the user if it could be in a project.  Add a method in SmartRefStream, and a conversion method in the new class."

	list _ OrderedCollection new.
	renamed do: [ :cls | | rec |
		rec _ changeRecords at: cls name.
		rec priorName ifNotNil: [
			| ans |
			ans _ PopUpMenu withCaption: 'You renamed class ', rec priorName, 
				' to be ', rec thisName,
				'.\Could an instance of ', rec priorName, 
				' be in a project on someone''s disk?'
			chooseFrom: #('Yes, write code to convert those instances'
				'No, no instances are in projects').
			ans = 1 ifTrue: [
				| oldStruct newStruct  |
				oldStruct _ structures at: rec priorName ifAbsent: nil.
				newStruct _ (Array with: cls classVersion), (cls allInstVarNames).
				oldStruct ifNotNil: [
					smart writeConversionMethodIn: cls fromInstVars: oldStruct 
							to: newStruct renamedFrom: rec priorName.
					smart writeClassRename: cls name was: rec priorName.
					list add: cls name, ' convertToCurrentVersion:refStream:']]
			ifFalse: [structures removeKey: rec priorName ifAbsent: nil]]].
	list isEmpty ifTrue: [^ msgSet].
	msgSet messageList ifNil: [msgSet initializeMessageList: list]
		ifNotNil: [list do: [:item | msgSet addMethodReference: item]].
	^ msgSet
</details>

#### ChangeSet>>#noteAddClass: class

Include indication that a new class was created.


<details>
	<summary>See more</summary>
	
	noteAddClass: class
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

<details>
	<summary>See more</summary>
	
	methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		noteNewMethod: newMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: oldMethod
</details>

#### ChangeSet>>#noteRemoveSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info

Include indication that a method has been forgotten. info is a pair of the source code pointer and message category for the method that was removed.


<details>
	<summary>See more</summary>
	
	noteRemoveSelector: selector class: class priorMethod: priorMethod lastMethodInfo: info
	"Include indication that a method has been forgotten.
	info is a pair of the source code pointer and message category
	for the method that was removed."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: info.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#isForBaseSystem: aBoolean

<details>
	<summary>See more</summary>
	
	isForBaseSystem: aBoolean

	isForBaseSystem _ aBoolean
</details>

#### ChangeSet>>#isForBaseSystem

<details>
	<summary>See more</summary>
	
	isForBaseSystem

	^isForBaseSystem
</details>

#### ChangeSet>>#checkForSlips

Return a collection of method refs with possible debugging code in them.


<details>
	<summary>See more</summary>
	
	checkForSlips
	"Return a collection of method refs with possible debugging code in them."
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc |  | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					method hasReportableSlip
						ifTrue: [slips add: (MethodReference method: method)]]]]].
	^ slips
</details>

#### ChangeSet>>#hasPostscript

<details>
	<summary>See more</summary>
	
	hasPostscript
	^ postscript notNil
</details>

#### ChangeSet>>#noteRemovalOf: class fromCategory: aCategoryName

The class is about to be removed from the system. Adjust the receiver to reflect that fact.


<details>
	<summary>See more</summary>
	
	noteRemovalOf: class fromCategory: aCategoryName 
	"The class is about to be removed from the system.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class) noteRemoved: class fromCategory: aCategoryName.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#classRecategorized: aClass from: oldCategory to: newCategory

<details>
	<summary>See more</summary>
	
	classRecategorized: aClass from: oldCategory to: newCategory

	self noteRecategorizationOfClass: aClass
</details>

#### ChangeSet>>#fileOutRemovedAndMovedMethodsOf: classList on: stream

<details>
	<summary>See more</summary>
	
	fileOutRemovedAndMovedMethodsOf: classList on: stream

	^ classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream ]
</details>

#### ChangeSet>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"try to write reference for me"
	^ DiskProxy 
		global: #ChangeSet
		selector: #existingOrNewChangeSetNamed:forBaseSystem:
		args: (Array with: self name with: self isForBaseSystem)
"===
	refStrm replace: self with: nil.
	^ nil
==="

</details>

#### ChangeSet>>#oldNameFor: class

<details>
	<summary>See more</summary>
	
	oldNameFor: class

	^ (changeRecords at: class name) priorName
</details>

#### ChangeSet>>#changeRecorderFor: classOrClassName

<details>
	<summary>See more</summary>
	
	changeRecorderFor: classOrClassName

	| cname |
	cname _ classOrClassName isString
		ifTrue: [ classOrClassName ]
		ifFalse: [ classOrClassName name ].

	"Later this will init the changeRecords so according to whether they should be revertable."
	^ changeRecords at: cname
			ifAbsent: [
				^ changeRecords at: cname
					put: (ClassChangeRecord new initFor: cname revertable: false)]
</details>

#### ChangeSet>>#fileOutMethodAdditionsFor: class on: stream

Write out all the method changes for this class.


<details>
	<summary>See more</summary>
	
	fileOutMethodAdditionsFor: class on: stream
	"Write out all the method changes for this class."

	| changes |
	changes _ Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		mAssoc value == #add
			ifTrue: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]
</details>

#### ChangeSet>>#clear

Reset the receiver to be empty.


<details>
	<summary>See more</summary>
	
	clear 
	"Reset the receiver to be empty.  "

	changeRecords _ Dictionary new.
	preamble _ nil.
	postscript _ nil.
	self hasUnsavedChanges: false.
	self isForBaseSystem: true 	"Not a great default, but at least some Boolean"
</details>

#### ChangeSet>>#noteCommentClass: class

Include indication that a class comment has been changed.


<details>
	<summary>See more</summary>
	
	noteCommentClass: class 
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #comment.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#addCoherency: className

SqR! 19980923: If I recreate the class then don't remove it


<details>
	<summary>See more</summary>
	
	addCoherency: className
	"SqR! 19980923: If I recreate the class then don't remove it"

	(self changeRecorderFor: className)
		checkCoherence.
"
	classRemoves remove: className ifAbsent: [].
	(classChanges includesKey: className) ifTrue:
		[(classChanges at: className) remove: #remove ifAbsent: []]
"
</details>

#### ChangeSet>>#methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

<details>
	<summary>See more</summary>
	
	methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	self
		noteRemoveSelector: aSymbol
		class: aClass
		priorMethod: aMethod
		lastMethodInfo: {aMethod sourcePointer. protocol}
</details>

#### ChangeSet>>#noteReorganizeClass: class

Include indication that a class was reorganized.


<details>
	<summary>See more</summary>
	
	noteReorganizeClass: class 
	"Include indication that a class was reorganized."

	self atClass: class add: #reorganize.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#fileOutOn: stream

Write out all the changes the receiver knows about


<details>
	<summary>See more</summary>
	
	fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	
	self isEmpty ifTrue: [ self inform: 'Warning: no changes to file out' ].
	
	classList _ Array streamContents: [ :strm |
		Smalltalk hierarchySorted: self changedClasses do: [ :cls | strm nextPut: cls ]].

	self fileOutClassDefinitionsOf: classList on: stream.
	self fileOutMethodsAdditionsOf: classList on: stream.
	self fileOutMethodsChangesOf: stream on: classList.
	self fileOutRemovedAndMovedMethodsOf: classList on: stream.
	self fileOutClassInitializationsOf: classList on: stream.
	self fileOutRemovedClassesOn: stream.
	self fileOutMovedClassesOn: stream.

</details>

#### ChangeSet>>#fileOutClassDeletionFrom: aClassChangeRecord doItOnlyIfInBaseSystem: aDoIt on: stream

<details>
	<summary>See more</summary>
	
	fileOutClassDeletionFrom: aClassChangeRecord doItOnlyIfInBaseSystem: aDoIt on: stream

	| record |
	
	record := ClassDeletionChangeRecord 
		className: aClassChangeRecord thisName 
		definition: aClassChangeRecord priorDefinition 
		doItOnlyIfInBaseSystem: aDoIt 
		stamp: aClassChangeRecord stamp.
		
	record fileOutOn: stream
	

</details>

#### ChangeSet>>#changedClassCategories

<details>
	<summary>See more</summary>
	
	changedClassCategories

	| answer |
	answer _ Dictionary new.
	self changedClasses do: [ :cls | 
		(answer at: cls category ifAbsentPut: Set new)
			add: cls ].
	^ answer
</details>

#### ChangeSet>>#sortedClassRecords: classRecords

<details>
	<summary>See more</summary>
	
	sortedClassRecords: classRecords

	^ classRecords sort: [:left :rigth | left thisName < rigth thisName ]
</details>

#### ChangeSet>>#noteMethodMoveToOtherPackage: selector forClass: class

<details>
	<summary>See more</summary>
	
	noteMethodMoveToOtherPackage: selector forClass: class

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteMethodMoveToOtherPackage: selector.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#fileOutRemovedClassRecord: aRemovedClassRecord on: stream

<details>
	<summary>See more</summary>
	
	fileOutRemovedClassRecord: aRemovedClassRecord on: stream

	self fileOutClassDeletionFrom: aRemovedClassRecord doItOnlyIfInBaseSystem: false on: stream

</details>

#### ChangeSet>>#askRemovedInstVars: classList

Ask the author whether these newly removed inst vars need to have their info saved


<details>
	<summary>See more</summary>
	
	askRemovedInstVars: classList
	| pairList pairClasses index pls |
	"Ask the author whether these newly removed inst vars need to have their info saved"

	pairList _ OrderedCollection new.
	pairClasses _ OrderedCollection new.
	"Class version numbers:  If it must change, something big happened.  Do need a conversion method then.  Ignore them here."
	classList do: [ :cls |
		| newStruct oldStruct |
		newStruct _ (cls allInstVarNames).
		oldStruct _ (structures at: cls name ifAbsent: [#(0), newStruct]) allButFirst.
		oldStruct do: [:instVarName |
			(newStruct includes: instVarName) ifFalse: [
				pairList add: cls name, ' ', instVarName.
				pairClasses add: cls]]].

	pairList isEmpty ifTrue: [^ #()].
	[
		index _ PopUpMenu withCaption: 'These instance variables were removed.
When an old project comes in, instance variables 
that have been removed will lose their contents.
Click on items to remove them from the list.
Click on any whose value is unimportant and need not be saved.'
			chooseFrom: pairList, #('all of these need a conversion method'
						'all of these have old values that can be erased').
		(index <= (pls _ pairList size)) & (index > 0) ifTrue: [
			pairList removeAt: index.
			pairClasses removeAt: index].
		index = (pls + 2) ifTrue: ["all are OK" ^ #()].
		pairList isEmpty | (index = (pls + 1))  "all need conversion, exit"] whileFalse.

	^ pairClasses asSet asArray	"non redundant"
</details>

#### ChangeSet>>#fileOutPostscriptOn: stream

If the receiver has a postscript, put it out onto the stream.


<details>
	<summary>See more</summary>
	
	fileOutPostscriptOn: stream 
	"If the receiver has a postscript, put it out onto the stream.  "

	| aString |
	aString _ self postscriptString.
	(aString notNil and: [ aString size > 0])
		ifTrue: [
			stream nextChunkPut: aString "surroundedBySingleQuotes".
			stream newLine; newLine]
</details>

#### ChangeSet>>#changedClasses

Answer an OrderedCollection of changed or edited classes. Does not include removed classes. Sort alphabetically by name.


<details>
	<summary>See more</summary>
	
	changedClasses
	"Answer an OrderedCollection of changed or edited classes.
	Does not include removed classes.  Sort alphabetically by name."

	"Much faster to sort names first, then convert back to classes.  Because metaclasses reconstruct their name at every comparison in the sorted collection.
	8/91 sw chgd to filter out non-existent classes (triggered by problems with class-renames"

	^ self changedClassNames
		collect: [:className | Smalltalk classNamed: className]
		thenSelect: [:aClass | aClass notNil]
</details>

#### ChangeSet>>#codePackage: aCodePackage

<details>
	<summary>See more</summary>
	
	codePackage: aCodePackage
	codePackage _ aCodePackage.
	self isForBaseSystem: false
</details>

#### ChangeSet>>#changedMessageListAugmented

Even added classes have all messages in changedMessageList.


<details>
	<summary>See more</summary>
	
	changedMessageListAugmented
	"Even added classes have all messages in changedMessageList."
	^ self changedMessageList asArray
</details>

#### ChangeSet>>#changedMessageList

Used by a message set browser to access the list view information.


<details>
	<summary>See more</summary>
	
	changedMessageList
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList _ OrderedCollection new.
	changeRecords associationsDo: [ :clAssoc | | classNameInFull classNameInParts |
		classNameInFull _ clAssoc key asString.
		classNameInParts _ classNameInFull findTokens: ' '.

		(clAssoc value allChangeTypes includes: #comment) ifTrue: [
			messageList add:
				(MethodReference new
					setClassSymbol: classNameInParts first asSymbol
					classIsMeta: false 
					methodSymbol: #Comment 
					stringVersion: classNameInFull, ' Comment')].

		clAssoc value methodChangeTypes associationsDo: [ :mAssoc |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				messageList add:
					(MethodReference new
						setClassSymbol: classNameInParts first asSymbol
						classIsMeta: classNameInParts size > 1 
						methodSymbol: mAssoc key 
						stringVersion: classNameInFull, ' ' , mAssoc key)]]].
	^ messageList sorted
</details>

#### ChangeSet>>#removePostscript

<details>
	<summary>See more</summary>
	
	removePostscript
	postscript _ nil
</details>

#### ChangeSet>>#fileOutRemovedClassesOn: stream

<details>
	<summary>See more</summary>
	
	fileOutRemovedClassesOn: stream

	^ self sortedRemovedClassRecords do: [ :aRemovedClassRecord | 
		self fileOutRemovedClassRecord: aRemovedClassRecord on: stream ]
</details>

#### ChangeSet>>#name: anObject

<details>
	<summary>See more</summary>
	
	name: anObject
	name _ anObject
</details>

#### ChangeSet>>#atSelector: selector class: class

<details>
	<summary>See more</summary>
	
	atSelector: selector class: class

	^ (changeRecords at: class name ifAbsent: [^ #none])
		atSelector: selector ifAbsent: [^ #none]
</details>

#### ChangeSet>>#askAddedInstVars: classList

Ask the author whether these newly added inst vars need to be non-nil


<details>
	<summary>See more</summary>
	
	askAddedInstVars: classList
	| pairList pairClasses index pls |
	"Ask the author whether these newly added inst vars need to be non-nil"

	pairList _ OrderedCollection new.
	pairClasses _ OrderedCollection new.
	"Class version numbers:  If it must change, something big happened.  Do need a conversion method then.  Ignore them here."
	classList do: [ :cls |
		| newStruct oldStruct |
		newStruct _ (cls allInstVarNames).
		oldStruct _ (structures at: cls name ifAbsent: [#(0), newStruct]) allButFirst.
		newStruct do: [ :instVarName |
			(oldStruct includes: instVarName) ifFalse: [
				pairList add: cls name, ' ', instVarName.
				pairClasses add: cls]]].

	pairList isEmpty ifTrue: [^ #()].
	[
		index _ PopUpMenu withCaption: 'These instance variables were added.
When an old project comes in, newly added 
instance variables will have the value nil.
Click on items to remove them from the list.
Click on any for which nil is an OK value.'
			chooseFrom: pairList, #('all of these need a non-nil value'
						'all of these are OK with a nil value').
		(index <= (pls _ pairList size)) & (index > 0) ifTrue: [
			pairList removeAt: index.
			pairClasses removeAt: index].
		index = (pls + 2) ifTrue: ["all are OK" ^ #()].
		pairList isEmpty | (index = (pls + 1)) "all need conversion, exit"] whileFalse.

	^ pairClasses asSet asArray	"non redundant"
</details>

#### ChangeSet>>#methodChanges

<details>
	<summary>See more</summary>
	
	methodChanges

	| methodChangeDict |
	methodChangeDict _ Dictionary new.
	changeRecords associationsDo: [ :assn | | changeTypes |
		changeTypes _ assn value methodChangeTypes.
		changeTypes isEmpty ifFalse: [methodChangeDict at: assn key put: changeTypes]].
	^ methodChangeDict
</details>

#### ChangeSet>>#fileOutMethodsChangesOf: stream on: classList

<details>
	<summary>See more</summary>
	
	fileOutMethodsChangesOf: stream on: classList

	^ classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ]
</details>

#### ChangeSet>>#assurePreambleExists

Make sure there is a TextModel holding the preamble; if it's found to have reverted to empty contents, put up the template


<details>
	<summary>See more</summary>
	
	assurePreambleExists
	"Make sure there is a TextModel holding the preamble; if it's found to have reverted to empty contents, put up the template"

	(preamble == nil or: [preamble actualContents isEmptyOrNil])
		ifTrue: [
			preamble _ TextModel withText: self preambleTemplate.	
			self hasUnsavedChanges: true ]
</details>

#### ChangeSet>>#noteRecategorizationOfClass: aClass

Remember that a class definition has been changed. Record the original structure, so that a conversion method can be built.


<details>
	<summary>See more</summary>
	
	noteRecategorizationOfClass: aClass
	"Remember that a class definition has been changed.  Record the original structure, so that a conversion method can be built."

	aClass wantsChangeSetLogging ifFalse: [^ self].
	self atClass: aClass add: #change.
	self addCoherency: aClass name.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#classChangeAt: className

Return what we know about class changes to this class.


<details>
	<summary>See more</summary>
	
	classChangeAt: className
	"Return what we know about class changes to this class."

	^ (changeRecords at: className ifAbsent: [^ Set new])
		allChangeTypes
</details>

#### ChangeSet>>#fileOutClassInitializationsOf: classList on: stream

<details>
	<summary>See more</summary>
	
	fileOutClassInitializationsOf: classList on: stream

	^ classList do: [ :aClass |
		self fileOutPSFor: aClass on: stream ]
</details>

#### ChangeSet>>#fatDefForClass: class

<details>
	<summary>See more</summary>
	
	fatDefForClass: class

	| newDef oldDef oldStrm newStrm outStrm oldVars newVars addedVars |
	newDef _ class definition.
	oldDef _ (self changeRecorderFor: class) priorDefinition.
	oldDef ifNil: [^ newDef].
	oldDef = newDef ifTrue: [^ newDef].

	oldStrm _ ReadStream on: oldDef.
	newStrm _ ReadStream on: newDef.
	outStrm _ WriteStream on: (String new: newDef size * 2).

	"Merge inst vars from old and new defs..."
	oldStrm upToAll: 'instanceVariableNames'; upTo: $'.
	outStrm 
		nextPutAll: (newStrm upToAll: 'instanceVariableNames'); 
		nextPutAll: 'instanceVariableNames:'.
	newStrm peek = $: ifTrue: [newStrm next].	"may or may not be there, but already written"
	outStrm
		nextPutAll: (newStrm upTo: $'); nextPut: $'.
	oldVars _ (oldStrm upTo: $') findTokens: Character separators.
	newVars _ (newStrm upTo: $') findTokens: Character separators.
	addedVars _ oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
	oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
	outStrm nextPut: $'.

	class isMeta ifFalse:
		["Merge class vars from old and new defs..."
		oldStrm upToAll: 'classVariableNames:'; upTo: $'.
		outStrm nextPutAll: (newStrm upToAll: 'classVariableNames:'); nextPutAll: 'classVariableNames:';
			nextPutAll: (newStrm upTo: $'); nextPut: $'.
		oldVars _ (oldStrm upTo: $') findTokens: Character separators.
		newVars _ (newStrm upTo: $') findTokens: Character separators.
		addedVars _ oldVars asSet addAll: newVars; removeAll: oldVars; asOrderedCollection.
		oldVars , addedVars do: [:var | outStrm nextPutAll: var; space].
		outStrm nextPut: $'].

	outStrm nextPutAll: newStrm upToEnd.
	^ outStrm contents

</details>

#### ChangeSet>>#fileOut

File out the receiver, to a file whose name is a function of the change-set name and either of the date & time or chosen to have a unique numeric tag, depending on the preference 'changeSetVersionNumbers'


<details>
	<summary>See more</summary>
	
	fileOut
	"File out the receiver, to a file whose name is a function of the  
	change-set name and either of the date & time or chosen to have a  
	unique numeric tag, depending on the preference  
	'changeSetVersionNumbers'"
	| slips nameToUse |
	nameToUse _ self name.
	nameToUse _ nameToUse copyReplaceAll: 'AuthorName' with: Utilities authorName asUnaccented asCamelCase.
	nameToUse _ Preferences changeSetVersionNumbers
				ifTrue: [
					DirectoryEntry currentDirectory
						nextNameFor: nameToUse coda: '-', Utilities authorInitials
						extension: 'cs.st' ]
				ifFalse: [ (nameToUse , '.' , Utilities dateTimeSuffix , '.cs.st') asFileName ].
		
	nameToUse asFileEntry writeStreamDo: [ :stream |
		stream timeStamp.
		self fileOutPreambleOn: stream.
		self fileOutOn: stream.
		self fileOutPostscriptOn: stream ].
	
	self hasUnsavedChanges: false.
	Preferences checkForSlips
		ifFalse: [^ self].
	slips _ self checkForSlips.
	(slips size > 0
			and: [(PopUpMenu withCaption: 'Methods in this fileOut have halts
or references to the Transcript
or other ''slips'' in them.
Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')
					= 2])
		ifTrue: [ Smalltalk browseMessageList: slips name: 'Possible slips in ' , name ]
</details>

#### ChangeSet>>#classCommented: aClass

<details>
	<summary>See more</summary>
	
	classCommented: aClass

	self noteCommentClass: aClass
</details>

#### ChangeSet>>#assurePostscriptExists

Make sure there is a TextModel holding the postscript.


<details>
	<summary>See more</summary>
	
	assurePostscriptExists
	"Make sure there is a TextModel holding the postscript.  "

	"NOTE: FileIn recognizes the postscript by the line with Postscript: on it"
	postscript
		ifNil: [
			self postscriptString: '"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!).
Be sure to put any further comments in double-quotes, like this one."
']
</details>

#### ChangeSet>>#preambleTemplate

Answer a string that will form the default contents for a change set's preamble. Just a first stab at what the content should be.


<details>
	<summary>See more</summary>
	
	preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: Preferences defaultAuthorName.
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"
</details>

#### ChangeSet>>#noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil

<details>
	<summary>See more</summary>
	
	noteNewMethod: newMethod forClass: class selector: selector priorMethod: methodOrNil

	class wantsChangeSetLogging ifFalse: [^ self].
	(self changeRecorderFor: class)
		noteNewMethod: newMethod selector: selector priorMethod: methodOrNil.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#preambleString

Answer the string representing the preamble


<details>
	<summary>See more</summary>
	
	preambleString
	"Answer the string representing the preamble"

	^ preamble
		ifNotNil: [ preamble actualContents asString ]
</details>

#### ChangeSet>>#trimHistory

Drop non-essential history: methods added and then removed, as well as rename and reorganization of newly-added classes.


<details>
	<summary>See more</summary>
	
	trimHistory 
	"Drop non-essential history:  methods added and then removed, as well as rename and reorganization of newly-added classes."

	changeRecords do: [:chgRecord | chgRecord trimHistory]
</details>

#### ChangeSet>>#editPostscript

edit the receiver's postscript, in a separate window.


<details>
	<summary>See more</summary>
	
	editPostscript
	"edit the receiver's postscript, in a separate window."

	self assurePostscriptExists.
	postscript openLabel: 'Postscript for ChangeSet named ' , name
</details>

#### ChangeSet>>#fileOutMethodMovedToOtherPackagesFor: class on: stream

Write out removals and initialization for this class.


<details>
	<summary>See more</summary>
	
	fileOutMethodMovedToOtherPackagesFor: class on: stream 
	"Write out removals and initialization for this class."

	self 
		fileOutMethodRemovalsOf: #(movedToOtherPackage) 
		movedToOtherPackage: true
		for: class 
		on: stream

</details>

#### ChangeSet>>#fileOutMethodRemovalsOf: changeTypes movedToOtherPackage: moved for: class on: stream

Write out removals and initialization for this class.


<details>
	<summary>See more</summary>
	
	fileOutMethodRemovalsOf: changeTypes movedToOtherPackage: moved for: class on: stream 
	"Write out removals and initialization for this class."

	| classRecord methodChanges changeType |
	
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	methodChanges _ classRecord methodChanges.
	
	methodChanges keysAndValuesDo: [:selector :aMethodChange |
		changeType _ aMethodChange changeType.
		(changeTypes includes: changeType)
			ifTrue: [ self 
				fileOutMethodRemovalOf: selector 
				for: class 
				movedToOtherPackage: moved 
				on: stream 
				stamp: aMethodChange stamp ]].		 

</details>

#### ChangeSet>>#fileOutPreambleOn: stream

If the receiver has a preamble, put it out onto the stream.


<details>
	<summary>See more</summary>
	
	fileOutPreambleOn: stream 
	"If the receiver has a preamble, put it out onto the stream.  "

	| aString |
	aString _ self preambleString.
	(aString notNil and: [aString size > 0])
		ifTrue: [
			stream nextChunkPut: aString "surroundedBySingleQuotes".
			stream newLine; newLine]
</details>

#### ChangeSet>>#postscriptString: aString

<details>
	<summary>See more</summary>
	
	postscriptString: aString

	postscript _ TextModel withText: aString.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#removePreamble

<details>
	<summary>See more</summary>
	
	removePreamble
	preamble _ nil
</details>

#### ChangeSet>>#noteClassMoveToOtherPackage: class

The class is about to be moved to some other package, who will hold it. Adjust the receiver to reflect that fact.


<details>
	<summary>See more</summary>
	
	noteClassMoveToOtherPackage: class
	"The class is about to be moved to some other package, who will hold it.
	Adjust the receiver to reflect that fact."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #movedToOtherPackage.
	changeRecords removeKey: class class name ifAbsent: nil.
	self hasUnsavedChanges: true
</details>

#### ChangeSet>>#okayToRemove

Should suggest a fileOut if changeSet is dirty... Implement this!


<details>
	<summary>See more</summary>
	
	okayToRemove
	"Should suggest a fileOut if changeSet is dirty... Implement this!"
	^ "self okayToRemoveInforming: true" true
</details>

#### ChangeSet>>#sortedRemovedClassRecords

<details>
	<summary>See more</summary>
	
	sortedRemovedClassRecords

	^ self sortedClassRecords: self removedClassRecords
</details>

#### ChangeSet>>#messageListForChangesWhich: aBlock ifNone: ifEmptyBlock

<details>
	<summary>See more</summary>
	
	messageListForChangesWhich: aBlock ifNone: ifEmptyBlock

	| answer |

	answer _ self changedMessageListAugmented select: [ :each |
		aBlock value: each actualClass value: each methodSymbol
	].
	answer isEmpty ifTrue: [^ifEmptyBlock value].
	^answer

</details>

#### ChangeSet>>#classReorganized: aClass

<details>
	<summary>See more</summary>
	
	classReorganized: aClass

	self noteReorganizeClass: aClass
</details>

#### ChangeSet>>#fileOutPSFor: class on: stream

Write out removals and initialization for this class.


<details>
	<summary>See more</summary>
	
	fileOutPSFor: class on: stream 
	"Write out removals and initialization for this class."

	| dict classRecord currentDef |
	classRecord _ changeRecords at: class name ifAbsent: [^ self].
	dict _ classRecord methodChangeTypes.
	((dict includesKey:  #initialize) and: [ class isMeta ]) ifTrue: [
		stream nextChunkPut: class soleInstance name, ' initialize'; newLine].
	((classRecord includesChangeType: #change)
			and: [(currentDef _ class definition) ~= (self fatDefForClass: class)]) ifTrue: [
		stream
			nextPut: $!;
			nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: currentDef; newLine].
	(classRecord includesChangeType: #reorganize) ifTrue: [
		class fileOutOrganizationOn: stream.
		stream newLine]
</details>

#### ChangeSet>>#changedClassNames

Answer a OrderedCollection of the names of changed or edited classes. DOES include removed classes. Sort alphabetically.


<details>
	<summary>See more</summary>
	
	changedClassNames
	"Answer a OrderedCollection of the names of changed or edited classes.
	DOES include removed classes.  Sort alphabetically."

	^ changeRecords keysSortedSafely 
</details>

#### ChangeSet>>#removeSelectorChanges: selector class: class

Remove all memory of changes associated with the argument, selector, in this class.


<details>
	<summary>See more</summary>
	
	removeSelectorChanges: selector class: class 
	"Remove all memory of changes associated with the argument, selector, in 
	this class."

	| chgRecord |
	self hasUnsavedChanges: true.	"set the flag in any case"
	(chgRecord _ changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name]
</details>

#### ChangeSet>>#methodsWithoutClassifications

Return a collection representing methods in the receiver which have not been categorized


<details>
	<summary>See more</summary>
	
	methodsWithoutClassifications
	"Return a collection representing methods in the receiver which have not been categorized"

	| slips notClassified |

	notClassified _ {'as yet unclassified' asSymbol. #all}.
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | aSelector |
			(aClass selectors includes:  (aSelector _ mAssoc key)) ifTrue: [
				(notClassified includes: (aClass organization categoryOfElement: aSelector))
					ifTrue: [slips add: aClass name , ' ' , aSelector]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutClassifications) name: 'unclassified methods'
	"
</details>

#### ChangeSet>>#atClass: class add: changeType

<details>
	<summary>See more</summary>
	
	atClass: class add: changeType

	(self changeRecorderFor: class)
		noteChangeType: changeType fromClass: class
</details>

#### ChangeSet>>#noteClassForgotten: className

Remove from structures if class is not a superclass of some other one we are remembering


<details>
	<summary>See more</summary>
	
	noteClassForgotten: className
	"Remove from structures if class is not a superclass of some other one we are remembering"

	self hasUnsavedChanges: true.	"set the flag in any case"
	structures ifNil: [^ self].
	Smalltalk at: className ifPresent: [:cls |
		cls subclasses do: [:sub | (structures includesKey: sub) ifTrue: [
			^ self]]].  "No delete"
	structures removeKey: className ifAbsent: nil
</details>

#### ChangeSet>>#fileOutClassDefinition: class on: stream

Write out class definition for the given class on the given stream, if the class definition was added or changed.


<details>
	<summary>See more</summary>
	
	fileOutClassDefinition: class on: stream 
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename) ifTrue: [
		stream nextChunkPut: 'Smalltalk renameClassNamed: #', (self oldNameFor: class), ' as: #', class name; newLine].

	(self atClass: class includes: #change) ifTrue: [ "fat definition only needed for changes"
		stream
			nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
			nextChunkPut: (self fatDefForClass: class); newLine.
	] ifFalse: [
		(self atClass: class includes: #add) ifTrue: [ "use current definition for add"
			stream
				nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine.
		].
	].

	(self atClass: class includes: #comment) ifTrue: [
		class theNonMetaClass organization putCommentOnFile: stream numbered: 0 moveSource: false forClass: class theNonMetaClass.
		stream newLine].


</details>

#### ChangeSet>>#isEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	"Answer whether the receiver contains any elements."
	
	changeRecords ifNil: [^true].
	^ changeRecords isEmpty 
</details>

#### ChangeSet>>#methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

<details>
	<summary>See more</summary>
	
	methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	self
		noteNewMethod: aMethod
		forClass: aClass
		selector: aSymbol
		priorMethod: nil
</details>

#### ChangeSet>>#selectorsInClass: aClass

Used by a ChangeSorter to access the list methods.


<details>
	<summary>See more</summary>
	
	selectorsInClass: aClass
	"Used by a ChangeSorter to access the list methods."

	^ (changeRecords at: aClass ifAbsent: [^#()]) changedSelectors
</details>

#### ChangeSet>>#codePackage

<details>
	<summary>See more</summary>
	
	codePackage
	^codePackage
</details>

#### ChangeSet>>#noteClassStructure: aClass

Save the instance variable names of this class and all of its superclasses. Later we can tell how it changed and write a conversion method. The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp).


<details>
	<summary>See more</summary>
	
	noteClassStructure: aClass
	"Save the instance variable names of this class and all of its superclasses.  Later we can tell how it changed and write a conversion method.  The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp)."

	| clsName |
	aClass ifNil: [^ self].
	structures ifNil: [structures _ Dictionary new.
				superclasses _ Dictionary new].
	clsName _ (aClass name asLowercase beginsWith: 'anobsolete') 
		ifTrue: [(aClass name copyFrom: 11 to: aClass name size) asSymbol]
		ifFalse: [aClass name].
	(structures includesKey: clsName) ifFalse: [
		structures at: clsName put: 
			((Array with: aClass classVersion), (aClass allInstVarNames)).
		superclasses at: clsName put: aClass superclass name].
	"up the superclass chain"
	aClass superclass ifNotNil: [self noteClassStructure: aClass superclass].

</details>

#### ChangeSet>>#selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

<details>
	<summary>See more</summary>
	
	selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self noteReorganizeClass: aClass
</details>

#### ChangeSet>>#wither

The receiver is to be clobbered. Clear it out. 2/7/96 sw


<details>
	<summary>See more</summary>
	
	wither
	"The receiver is to be clobbered.  Clear it out.  2/7/96 sw"

	self clear.
	name _ nil
</details>

#### ChangeSet>>#canHavePreambleAndPostscript

Don't allow preambles and postscripts in changesets for Packages, because packages don't support them. Use prerequisites and class initialize methods instead.


<details>
	<summary>See more</summary>
	
	canHavePreambleAndPostscript
	"Don't allow preambles and postscripts in changesets for Packages,
	because packages don't support them. Use prerequisites and class initialize methods instead."
	
	^ self isForBaseSystem
</details>

#### ChangeSet>>#postscriptString

Answer the string representing the postscript.


<details>
	<summary>See more</summary>
	
	postscriptString
	"Answer the string representing the postscript.  "

	^ postscript
		ifNil: [postscript]
		ifNotNil: [postscript actualContents asString]
</details>

## ChangeSorter

I display a ChangeSet.

### Methods
#### ChangeSorter>>#currentSelector: messageName

<details>
	<summary>See more</summary>
	
	currentSelector: messageName

	currentSelector _ messageName.
	self changed: #currentSelector.
	self acceptedContentsChanged.
</details>

#### ChangeSorter>>#changeSet

<details>
	<summary>See more</summary>
	
	changeSet
	^ myChangeSet
</details>

#### ChangeSorter>>#removePrompting: doPrompt

Completely destroy my change set. Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first.


<details>
	<summary>See more</summary>
	
	removePrompting: doPrompt
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |

	"Tiene sentido? Preguntar cosas? Sugerir hacer fileOut?"
	self flag: #ojo.

	aName _ myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message _ 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg _ myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	false ifTrue: [
		changeSetNumber _ myChangeSet name initialIntegerOrNil.
		changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber]].
	ChangeSet removeChangeSet: myChangeSet.
</details>

#### ChangeSorter>>#addPreamble

<details>
	<summary>See more</summary>
	
	addPreamble
	myChangeSet assurePreambleExists.
	currentClassName _ nil.
	currentSelector _ nil.
	self showChangeSet: myChangeSet
</details>

#### ChangeSorter>>#removePostscript

<details>
	<summary>See more</summary>
	
	removePostscript

	myChangeSet removePostscript.
	self showChangeSet: myChangeSet
</details>

#### ChangeSorter>>#update

Recompute all of my lists.


<details>
	<summary>See more</summary>
	
	update
	"Recompute all of my lists."

	self updateIfNeeded
</details>

#### ChangeSorter>>#messageList

<details>
	<summary>See more</summary>
	
	messageList 

	| probe newSelectors |
	currentClassName ifNil: [^ #()].
	probe _ (currentClassName endsWith: ' class')
		ifTrue: [currentClassName]
		ifFalse: [currentClassName asSymbol].
	newSelectors _ myChangeSet selectorsInClass: probe.
	(newSelectors includes: currentSelector) ifFalse: [currentSelector _ nil].
	^ newSelectors asArray sort

</details>

#### ChangeSorter>>#forget

Drop this method from the changeSet


<details>
	<summary>See more</summary>
	
	forget
	"Drop this method from the changeSet"

	currentSelector ifNotNil: [
		myChangeSet removeSelectorChanges: self selectedMessageName 
			class: self selectedClassOrMetaClass.
		currentSelector _ nil.
		self showChangeSet: myChangeSet]
</details>

#### ChangeSorter>>#removePreamble

<details>
	<summary>See more</summary>
	
	removePreamble
	myChangeSet removePreamble.
	self showChangeSet: myChangeSet
</details>

#### ChangeSorter>>#currentClassName: aString

<details>
	<summary>See more</summary>
	
	currentClassName: aString

	currentClassName _ aString.
	currentSelector _ nil.	"fix by wod"
	self changed: #currentClassName.
	self changed: #messageList.
	self acceptedContentsChanged.
</details>

#### ChangeSorter>>#viewAffectedClassCategories

<details>
	<summary>See more</summary>
	
	viewAffectedClassCategories

	myChangeSet changedClassCategories explore
</details>

#### ChangeSorter>>#currentHasPreamble

<details>
	<summary>See more</summary>
	
	currentHasPreamble
	^ myChangeSet hasPreamble
</details>

#### ChangeSorter>>#showChangeSet: chgSet

<details>
	<summary>See more</summary>
	
	showChangeSet: chgSet

	myChangeSet == chgSet ifFalse: [
		myChangeSet _ chgSet.
		currentClassName _ nil.
		currentSelector _ nil].
	self changed: #relabel.
	self changed: #currentCngSet.	"new -- list of sets"
	self changed: #mainButtonName.	"old, button"
	self changed: #classList.
	self changed: #messageList.
	self acceptedContentsChanged.
</details>

#### ChangeSorter>>#myChangeSet

<details>
	<summary>See more</summary>
	
	myChangeSet
	^myChangeSet
</details>

#### ChangeSorter>>#currentSelector

<details>
	<summary>See more</summary>
	
	currentSelector

	^ currentSelector
</details>

#### ChangeSorter>>#updateIfNeeded

Recompute all of my lists.


<details>
	<summary>See more</summary>
	
	updateIfNeeded
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList ].

	newList _ self changeSetDirtyFlags.
	(priorDirtyFlags == nil or: [priorDirtyFlags ~= newList])
		ifTrue: [
			priorDirtyFlags _ newList.
			self changed: #changeSetDirtyFlags ].
	self acceptedContentsChanged
</details>

#### ChangeSorter>>#mainButtonName

<details>
	<summary>See more</summary>
	
	mainButtonName

	^ myChangeSet name
</details>

#### ChangeSorter>>#methodInfoFromRemoval: classAndSelector

<details>
	<summary>See more</summary>
	
	methodInfoFromRemoval: classAndSelector
	^myChangeSet methodInfoFromRemoval: classAndSelector
</details>

#### ChangeSorter>>#showChangeSetNamed: aName

<details>
	<summary>See more</summary>
	
	showChangeSetNamed: aName

	self showChangeSet: (ChangeSet changeSetNamed: aName) 
</details>

#### ChangeSorter>>#classList

Computed. View should try to preserve selections, even though index changes


<details>
	<summary>See more</summary>
	
	classList
	"Computed.  View should try to preserve selections, even though index changes"

	^ myChangeSet ifNotNil: [myChangeSet changedClassNames] ifNil: [OrderedCollection new]

</details>

#### ChangeSorter>>#selectedClass

Answer the currently-selected class. If there is no selection, or if the selection refers to a class no longer extant, return nil


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the currently-selected class.  If there is no selection, or if the selection refers to a class no longer extant, return nil"
	| c |
	^ currentClassName ifNotNil: [(c _ self selectedClassOrMetaClass)
		ifNotNil: [c theNonMetaClass]]
</details>

#### ChangeSorter>>#remove

Completely destroy my change set. Check if it's OK first


<details>
	<summary>See more</summary>
	
	remove
	"Completely destroy my change set.  Check if it's OK first"

	self removePrompting: true.
	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update
</details>

#### ChangeSorter>>#addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream

Add an annotation detailing the prior versions count. Specially handled here for the case of a selector no longer in the system, whose prior version is pointed to by the lost-method pointer in the change held on to by the changeset


<details>
	<summary>See more</summary>
	
	addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior version is pointed to by the lost-method pointer in the change held on to by the changeset"

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].
	aStream nextPutAll:
		((myChangeSet methodInfoFromRemoval: {aClass name. aSelector})
			ifNil:
				['no prior versions']
			ifNotNil:
				['version(s) retrievable here']), self annotationSeparator
</details>

#### ChangeSorter>>#changeSetList

Answer a list of ChangeSet names to be shown in the change sorter.


<details>
	<summary>See more</summary>
	
	changeSetList
	"Answer a list of ChangeSet names to be shown in the change sorter."

	^ (ChangeSet allChangeSets collect: [ :a | a name ]) reversed
</details>

#### ChangeSorter>>#toggleDiffing

Toggle whether diffs should be shown in the code pane


<details>
	<summary>See more</summary>
	
	toggleDiffing
	"Toggle whether diffs should be shown in the code pane"

	super toggleDiffing.
	self acceptedContentsChanged.
	self update
</details>

#### ChangeSorter>>#labelString

The label for my entire window. The large button that displays my name is gotten via mainButtonName


<details>
	<summary>See more</summary>
	
	labelString
	"The label for my entire window.  The large button that displays my name is gotten via mainButtonName"

	^ 'ChangeSet: ', myChangeSet name
</details>

#### ChangeSorter>>#fileOutAndRemove

File out the current change set.


<details>
	<summary>See more</summary>
	
	fileOutAndRemove
	"File out the current change set."

	myChangeSet fileOut.
	self removePrompting: false.

	self showChangeSet: ChangeSet changeSetForBaseSystem.
	self update
</details>

#### ChangeSorter>>#forgetClass

Remove all mention of this class from the changeSet


<details>
	<summary>See more</summary>
	
	forgetClass
	"Remove all mention of this class from the changeSet"

	currentClassName ifNotNil: [
		myChangeSet removeClassChanges: currentClassName.
		currentClassName _ nil.
		currentSelector _ nil.
		self showChangeSet: myChangeSet].

</details>

#### ChangeSorter>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	self currentSelector ifNil: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true
</details>

#### ChangeSorter>>#rename

Store a new name string into the selected ChangeSet. reject duplicate name; allow user to back out


<details>
	<summary>See more</summary>
	
	rename
	"Store a new name string into the selected ChangeSet.  reject duplicate name; allow user to back out"

	| newName |
	newName _ FillInTheBlankMorph request: 'New name for this change set'
						initialAnswer: myChangeSet name.
	(newName = myChangeSet name or: [newName size = 0]) ifTrue:
			[^ Smalltalk beep].

	(ChangeSet changeSetNamed: newName) ifNotNil:
			[^ Utilities inform: 'Sorry that name is already used'].

	myChangeSet name: newName.
	self update.
	self changed: #mainButtonName.
	self changed: #relabel.
</details>

#### ChangeSorter>>#fileOutAndKeep

File out the current change set.


<details>
	<summary>See more</summary>
	
	fileOutAndKeep
	"File out the current change set."

	myChangeSet fileOut.
	self update
</details>

#### ChangeSorter>>#currentHasPostscript

<details>
	<summary>See more</summary>
	
	currentHasPostscript
	^ myChangeSet hasPostscript
</details>

#### ChangeSorter>>#currentCngSet

<details>
	<summary>See more</summary>
	
	currentCngSet
	^ myChangeSet name
</details>

#### ChangeSorter>>#contents: aString notifying: aRequestor

Compile the code in aString. Notify aRequestor of any syntax errors. Create an error if the category of the selected message is unknown. Answer false if the compilation fails. Otherwise, if the compilation created a new method, deselect the current selection. Then answer true.


<details>
	<summary>See more</summary>
	
	contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |

	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [aString size = 0]) ifTrue: [ ^ false].
		(aString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: aString) size > 0 ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: aString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true
</details>

#### ChangeSorter>>#myChangeSet: anObject

<details>
	<summary>See more</summary>
	
	myChangeSet: anObject
	myChangeSet _ anObject
</details>

#### ChangeSorter>>#editPostscript

Allow the user to edit the receiver's change-set's postscript -- in a separate window


<details>
	<summary>See more</summary>
	
	editPostscript
	"Allow the user to edit the receiver's change-set's postscript -- in a separate window"

	myChangeSet editPostscript
</details>

#### ChangeSorter>>#trimHistory

Drop non-essential history (rename, reorg, method removals) from newly-added classes.


<details>
	<summary>See more</summary>
	
	trimHistory
	"Drop non-essential history (rename, reorg, method removals) from newly-added classes."

	myChangeSet trimHistory


</details>

#### ChangeSorter>>#acceptedStringOrText

return the source code that shows in the bottom pane


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class strm changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer ]
		ifNil: [
			strm _ WriteStream on: (String new: 100).
			(myChangeSet classChangeAt: currentClassName) do: [ :each |
				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
				each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
			].
			^ strm contents].
</details>

#### ChangeSorter>>#methodConflicts

Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such.


<details>
	<summary>See more</summary>
	
	methodConflicts
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |
	aList _ myChangeSet 
		messageListForChangesWhich: [ :aClass :aSelector |
			(ChangeSet allChangeSetsWithClass: aClass selector: aSelector) size > 1 ]
		ifNone: [ #() ].
	^aList
</details>

#### ChangeSorter>>#removeMessage

Remove the selected msg from the system. Real work done by the parent, a ChangeSorter


<details>
	<summary>See more</summary>
	
	removeMessage
	"Remove the selected msg from the system. Real work done by the 
	parent, a ChangeSorter"
	| confirmation sel |
	currentSelector
		ifNotNil: [
			confirmation _ Smalltalk
				confirmRemovalOf: (sel _ self selectedMessageName)
				on: self selectedClassOrMetaClass.
			confirmation = 3
				ifTrue: [^ self].
			self selectedClassOrMetaClass removeSelector: sel.
			self update.
			confirmation = 2
				ifTrue: [Smalltalk browseAllCallsOn: sel]]
</details>

#### ChangeSorter>>#changeSetDirtyFlags

<details>
	<summary>See more</summary>
	
	changeSetDirtyFlags

	^ (ChangeSet allChangeSets collect: [ :each |
		((each isForBaseSystem or: [ each codePackage notNil ]) and: [ each hasUnsavedChanges ])
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]) reversed
</details>

#### ChangeSorter>>#currentClassName

<details>
	<summary>See more</summary>
	
	currentClassName

	^ currentClassName
</details>

#### ChangeSorter>>#currentCanHavePreambleAndPostscript

<details>
	<summary>See more</summary>
	
	currentCanHavePreambleAndPostscript
	^ myChangeSet canHavePreambleAndPostscript
</details>

#### ChangeSorter>>#label

<details>
	<summary>See more</summary>
	
	label
	^ self labelString
</details>

#### ChangeSorter>>#selectedClassOrMetaClass

Careful, the class may have been removed!


<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	"Careful, the class may have been removed!"

	| cName |
	currentClassName ifNil: [^ nil].
	(currentClassName endsWith: ' class')
		ifTrue: [
			cName _ (currentClassName copyFrom: 1 to: currentClassName size-6) asSymbol.
			^ (Smalltalk at: cName ifAbsent: [^nil]) class]
		ifFalse: [
			cName _ currentClassName asSymbol.
			^ Smalltalk at: cName ifAbsent: nil]
</details>

#### ChangeSorter>>#selectedMessageName

<details>
	<summary>See more</summary>
	
	selectedMessageName

	currentSelector ifNil: [^ nil].
	^ currentSelector asSymbol
</details>

## ClassChangeRecord

A ClassChangeRecorder keeps track of most substantive changes premissible in a project, isolated or not. Structure: inForce a boolean Tells whether these changes are in effect. true for all changeSets in and above the current project. It should be sufficient only to record this for the changeSet as a whole, but this redundancy could help in error recovery. classIsLocal a boolean True if and only if this class is defined in this layer of the project structure. changeTypes an identitySet Summarizes which changes have been made in this class. Values include #comment, #reorganize, #rename, and the four more summarized below. thisName a string Retains the class name for this layer. priorName a string Preserves the prior name. thisComment a text Retains the class comment for this layer. priorComment a text Preserves the prior comment. thisOrganization a classOrganizer Retains the class organization for this layer. priorOrganization a classOrganizer Preserves the prior organization. thisMD a methodDictionary Used to prepare changes for nearly atomic invocation of this layer (see below). priorMD a methodDictionary Preserves the state of an altered class as it exists in the next outer layer of the project structure. methodChanges a dictionary of classChangeRecords Retains all the method changes for this layer. Four of the possible changeTypes are maintained in a mutually exclusive set, analogously to MethodChangeRecords. Here is a simple summary of the relationship between these four changeType symbols and the recording of prior state | prior == nil | prior not nil --------- |---------------------------- |-------------------- add | add | change --------- |---------------------------- |-------------------- remove | addedThenRemoved | remove A classChangeRecorder is notified of changes by the method noteMethodChange: <ClassChangeRecord>. ClassChangeRecorders are designed to invoke a set of changes relative to the definition of a class in an prior layer. It is important that both invocation and revocation of these changes take place in a nearly atomic fashion so that interdependent changes will be adopted as a whole, and so that only one flush of the method cache should be necessary. A further reason for revocation to be simple is that it may be requested as an attempt to recover from an error in a project that is failing.

### Methods
#### ClassChangeRecord>>#hasNoChanges

<details>
	<summary>See more</summary>
	
	hasNoChanges

	^ changeTypes isEmpty and: [methodChanges isEmpty]
</details>

#### ClassChangeRecord>>#checkCoherence

If I recreate the class then don't remove it


<details>
	<summary>See more</summary>
	
	checkCoherence
	"If I recreate the class then don't remove it"

	(changeTypes includes: #remove) ifTrue: [
		changeTypes remove: #remove.
		changeTypes add: #change ].
	(changeTypes includes: #addedThenRemoved) ifTrue: [
		changeTypes remove: #addedThenRemoved.
		changeTypes add: #add ].
	(changeTypes includes: #movedToOtherPackage) ifTrue: [
		changeTypes remove: #movedToOtherPackage.
		changeTypes add: #add ].
</details>

#### ClassChangeRecord>>#infoFromRemoval: selector

<details>
	<summary>See more</summary>
	
	infoFromRemoval: selector

	^ (methodChanges at: selector ifAbsent: [^ nil])
		methodInfoFromRemoval


</details>

#### ClassChangeRecord>>#priorDefinition

<details>
	<summary>See more</summary>
	
	priorDefinition

	^ priorDefinition
</details>

#### ClassChangeRecord>>#allChangeTypes

<details>
	<summary>See more</summary>
	
	allChangeTypes

	| chgs |
	(priorName notNil and: [changeTypes includes: #rename]) ifTrue:
		[(chgs _ changeTypes copy) add: 'oldName: ' , priorName.
		^ chgs].
	^ changeTypes
</details>

#### ClassChangeRecord>>#methodChanges

<details>
	<summary>See more</summary>
	
	methodChanges

	^ methodChanges
</details>

#### ClassChangeRecord>>#isClassMoveToOtherPackage

NOTE: there are other removals with changeType #addedThenRemoved, but this message is used to write out removals in fileOut, and those cases should not be written out.


<details>
	<summary>See more</summary>
	
	isClassMoveToOtherPackage
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ changeTypes includes: #movedToOtherPackage
</details>

#### ClassChangeRecord>>#noteNewMethod: newMethod selector: selector priorMethod: methodOrNil

<details>
	<summary>See more</summary>
	
	noteNewMethod: newMethod selector: selector priorMethod: methodOrNil

	| methodChange |
	methodChange _ self findOrMakeMethodChangeAt: selector priorMethod: methodOrNil.
	methodOrNil
		ifNil: [ methodChange noteChangeType: #add ]
		ifNotNil: [ methodChange noteChangeType: #change ].
	methodChange noteNewMethod: newMethod.

</details>

#### ClassChangeRecord>>#removeSelector: selector

Remove all memory of changes associated with the argument, selector, in this class.


<details>
	<summary>See more</summary>
	
	removeSelector: selector
	"Remove all memory of changes associated with the argument, selector, in this class."

	selector == #Comment
		ifTrue:
			[changeTypes remove: #comment ifAbsent: nil]
		ifFalse:
			[methodChanges removeKey: selector ifAbsent: nil]
</details>

#### ClassChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp
	
	^stamp
</details>

#### ClassChangeRecord>>#thisName

<details>
	<summary>See more</summary>
	
	thisName

	^ thisName
</details>

#### ClassChangeRecord>>#realClass

Return the actual class (or meta), as determined from my name.


<details>
	<summary>See more</summary>
	
	realClass
	"Return the actual class (or meta), as determined from my name."

	thisName ifNil: [^ nil].
	(thisName endsWith: ' class')
		ifTrue: [^ (Smalltalk at: (thisName copyFrom: 1 to: thisName size - 6) asSymbol
						ifAbsent: [^ nil]) class]
		ifFalse: [^ Smalltalk at: thisName ifAbsent: [^ nil]]
</details>

#### ClassChangeRecord>>#priorName

<details>
	<summary>See more</summary>
	
	priorName

	^ priorName
</details>

#### ClassChangeRecord>>#methodChangeTypes

Return an old-style dictionary of method change types.


<details>
	<summary>See more</summary>
	
	methodChangeTypes
	"Return an old-style dictionary of method change types."

	| dict |
	dict := IdentityDictionary new.
	methodChanges associationsDo:
		[:assn | | selector record |
		selector := assn key.
		record := assn value.
		dict at: selector put: record changeType].
	^ dict
</details>

#### ClassChangeRecord>>#noteRemoved: class fromCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	noteRemoved: class fromCategory: aCategoryName

	priorDefinition := class definitionReplacingCategoryWith: aCategoryName.
	self noteChangeType: #remove fromClass: class

</details>

#### ClassChangeRecord>>#atSelector: selector ifAbsent: absentBlock

<details>
	<summary>See more</summary>
	
	atSelector: selector ifAbsent: absentBlock

	^ (methodChanges at: selector ifAbsent: absentBlock)
		changeType
</details>

#### ClassChangeRecord>>#isClassRemoval

NOTE: there are other removals with changeType #addedThenRemoved, but this message is used to write out removals in fileOut, and those cases should not be written out.


<details>
	<summary>See more</summary>
	
	isClassRemoval
	"NOTE: there are other removals with changeType #addedThenRemoved,
	but this message is used to write out removals in fileOut, and those
	cases should not be written out."

	^ (changeTypes includes: #remove) or: [changeTypes includes: #removeClass]
</details>

#### ClassChangeRecord>>#compileAll: newClass from: oldClass

Something about this class has changed. Locally retained methods must be recompiled. NOTE: You might think that if this changeSet is in force, then we can just note the new methods but a lower change set may override and be in force which would mean that only the overriding copies go recompiled. Just do it.


<details>
	<summary>See more</summary>
	
	compileAll: newClass from: oldClass
	"Something about this class has changed.  Locally retained methods must be recompiled.
	NOTE:  You might think that if this changeSet is in force, then we can just note
	the new methods but a lower change set may override and be in force which
	would mean that only the overriding copies go recompiled.  Just do it."

	methodChanges associationsDo:
		[:assn | | sel changeType changeRecord newMethod |
		sel := assn key.
		changeRecord := assn value.
		changeType := changeRecord changeType.
		(changeType == #add or: [changeType == #change]) ifTrue:
			[newMethod := newClass
				recompileNonResidentMethod: changeRecord currentMethod
				atSelector: sel from: oldClass.
			changeRecord noteNewMethod: newMethod]]
</details>

#### ClassChangeRecord>>#atSelector: selector put: changeType

<details>
	<summary>See more</summary>
	
	atSelector: selector put: changeType

	(self findOrMakeMethodChangeAt: selector priorMethod: nil)
		noteChangeType: changeType
</details>

#### ClassChangeRecord>>#includesChangeType: changeType

<details>
	<summary>See more</summary>
	
	includesChangeType: changeType

	changeType == #new ifTrue: [^ changeTypes includes: #add].  "Backwd compat"
	^ changeTypes includes: changeType
</details>

#### ClassChangeRecord>>#noteNewName: newName

<details>
	<summary>See more</summary>
	
	noteNewName: newName

	thisName _ newName
</details>

#### ClassChangeRecord>>#changedSelectors

Return a set of the changed or removed selectors.


<details>
	<summary>See more</summary>
	
	changedSelectors
	"Return a set of the changed or removed selectors."

	^ methodChanges keys asSet
</details>

#### ClassChangeRecord>>#noteChangeType: changeSymbol

<details>
	<summary>See more</summary>
	
	noteChangeType: changeSymbol

	^ self noteChangeType: changeSymbol fromClass: nil
</details>

#### ClassChangeRecord>>#noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: infoOrNil

<details>
	<summary>See more</summary>
	
	noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: infoOrNil

	| methodChange |
	methodChange _ self findOrMakeMethodChangeAt: selector priorMethod: priorMethod.
	methodChange changeType == #add
		ifTrue: [methodChange noteChangeType: #addedThenRemoved]
		ifFalse: [methodChange noteChangeType: #remove].

	infoOrNil ifNotNil:
		["Save the source code pointer and category so can still browse old versions"
		methodChange noteMethodInfoFromRemoval: infoOrNil]


</details>

#### ClassChangeRecord>>#trimHistory

Drop non-essential history.


<details>
	<summary>See more</summary>
	
	trimHistory
	"Drop non-essential history."

	"Forget methods added and later removed"
	methodChanges keysAndValuesRemove:
		[:sel :chgRecord | chgRecord changeType == #addedThenRemoved].

	"Forget renaming and reorganization of newly-added classes."
	(changeTypes includes: #add) ifTrue:
		[changeTypes removeAllFoundIn: #(rename reorganize)].

</details>

#### ClassChangeRecord>>#noteMethodMoveToOtherPackage: selector

<details>
	<summary>See more</summary>
	
	noteMethodMoveToOtherPackage: selector

	| methodChange |
	methodChange _ self findOrMakeMethodChangeAt: selector priorMethod: nil.
	methodChange noteChangeType: #movedToOtherPackage
</details>

#### ClassChangeRecord>>#noteChangeType: changeSymbol fromClass: class

<details>
	<summary>See more</summary>
	
	noteChangeType: changeSymbol fromClass: class

	stamp _ Utilities changeStamp.
	
	changeSymbol == #movedToOtherPackage ifTrue: [
		^ changeTypes add: changeSymbol].
	"Any other change type meanse we're still here!"
	changeTypes remove: #movedToOtherPackage ifAbsent: nil.

	(changeSymbol == #new or: [changeSymbol == #add]) ifTrue: [
		changeTypes add: #add.
		changeTypes remove: #change ifAbsent: nil.
		^ self].
	changeSymbol == #change ifTrue: [
		(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue: [
		^ self].  "An entire class was added but then removed"
	changeSymbol == #comment ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #reorganize ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #rename ifTrue: [
		^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue: [
		"Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName _ changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol == #remove ifTrue: [
		(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'
</details>

#### ClassChangeRecord>>#notePriorDefinition: oldClass

<details>
	<summary>See more</summary>
	
	notePriorDefinition: oldClass

	oldClass ifNil: [^ self].
	priorDefinition ifNil: [priorDefinition _ oldClass definition]
</details>

#### ClassChangeRecord>>#initFor: className revertable: isRevertable

<details>
	<summary>See more</summary>
	
	initFor: className revertable: isRevertable

	inForce _ isRevertable.
	changeTypes _ IdentitySet new.
	methodChanges _ IdentityDictionary new.
	priorName _ thisName _ className
</details>

#### ClassChangeRecord>>#findOrMakeMethodChangeAt: selector priorMethod: priorMethod

<details>
	<summary>See more</summary>
	
	findOrMakeMethodChangeAt: selector priorMethod: priorMethod

	^ methodChanges 
		at: selector
		ifAbsentPut: [MethodChangeRecord new priorMethod: priorMethod]
</details>

## ClassCommentVersionsBrowser

A class-comment-versions-browser tool

### Methods
#### ClassCommentVersionsBrowser>>#compareToCurrentVersion

If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text


<details>
	<summary>See more</summary>
	
	compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	s1 _ classOfMethod organization classComment.
	s2 _ change string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].
	diffWords _ self shouldDiffWords.
	differDesc _ diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].
	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords))
				openLabel: 'Comparison to Current Version: ', differDesc
</details>

#### ClassCommentVersionsBrowser>>#priorSourceOrNil

If the currently-selected method has a previous version, return its source, else return nil


<details>
	<summary>See more</summary>
	
	priorSourceOrNil
	"If the currently-selected method has a previous version, return its source, else return nil"
	| changeRecords |
	self selectedClass ifNil: [^ nil].
	self selectedMessageName ifNil: [^ nil].
	changeRecords _  self class commentRecordsOf: self selectedClass.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [^ nil].
	^ (changeRecords at: 2) string 

</details>

#### ClassCommentVersionsBrowser>>#wantsPrettyDiffOption

Answer whether pretty-diffs are meaningful for this tool


<details>
	<summary>See more</summary>
	
	wantsPrettyDiffOption
	"Answer whether pretty-diffs are meaningful for this tool"

	^ false
</details>

#### ClassCommentVersionsBrowser>>#offerVersionsHelp

<details>
	<summary>See more</summary>
	
	offerVersionsHelp
	(TextModel new contents: self versionsHelpString)
		openLabel: 'Class Comment Versions Browsers'
</details>

#### ClassCommentVersionsBrowser>>#diffedVersionContents

Answer diffed version contents, maybe pretty maybe not


<details>
	<summary>See more</summary>
	
	diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change _ changeList at: listIndex.
	later _ change text.
	class _ self selectedClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier _ (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
</details>

#### ClassCommentVersionsBrowser>>#scanVersionsOf: class

Scan for all past versions of the class comment of the given class


<details>
	<summary>See more</summary>
	
	scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'commentStamp:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod field: #commentStamp: from: tokens ifAbsentOrNil: [ '' ].
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorRef ]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections
</details>

#### ClassCommentVersionsBrowser>>#contentsSymbolQuints

Answer a list of quintuplets representing information on the alternative views available in the code pane


<details>
	<summary>See more</summary>
	
	contentsSymbolQuints
	"Answer a list of quintuplets representing information on the alternative views available in the code pane"

	^ #(
(source		togglePlainSource 		showingPlainSourceString		'source'	'the textual source code as writen')
(lineDiffs		toggleLineDiffing		showingLineDiffsString	'showDiffs'	'the textual source diffed from its prior version')
	)
</details>

#### ClassCommentVersionsBrowser>>#updateIfNeeded

<details>
	<summary>See more</summary>
	
	updateIfNeeded
	| aComment |
	aComment _ classOfMethod organization commentRemoteStr.
	aComment == currentCompiledMethod
		ifFalse:
			["Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aComment last ~= 0 ifTrue: [self reformulateList]].
	^ true

</details>

#### ClassCommentVersionsBrowser>>#reformulateList

Some uncertainty about how to deal with lost methods here


<details>
	<summary>See more</summary>
	
	reformulateList

     classOfMethod organization classComment ifNil: [^ self].

	self scanVersionsOf: classOfMethod.
	self changed: #list. "for benefit of mvc"
	listIndex _ 1.
	self changed: #listIndex.
	self acceptedContentsChanged
</details>

#### ClassCommentVersionsBrowser>>#classCommentIndicated

Answer whether the receiver is pointed at a class comment


<details>
	<summary>See more</summary>
	
	classCommentIndicated
	"Answer whether the receiver is pointed at a class comment"

	^ true
</details>

#### ClassCommentVersionsBrowser>>#selectedClass

Answer the class currently selected in the browser. In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod
</details>

## ClassDeletionChangeRecord

Polymorphic with ChangeRecord. Used to represent classes that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Classes that will be deleted on install.

### Methods
#### ClassDeletionChangeRecord>>#doItOnlyIfInBaseSystem: aBoolean

<details>
	<summary>See more</summary>
	
	doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem _ aBoolean
</details>

#### ClassDeletionChangeRecord>>#string

<details>
	<summary>See more</summary>
	
	string
	
	^classDefinition ifNil: [ '' ]
</details>

#### ClassDeletionChangeRecord>>#changeClassName

<details>
	<summary>See more</summary>
	
	changeClassName
	^className
</details>

#### ClassDeletionChangeRecord>>#initializeClassName: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp

<details>
	<summary>See more</summary>
	
	initializeClassName: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp 
	
	className := aClassName.
	classDefinition := aClassDefinition.
	doItOnlyIfInBaseSystem := aDoItOnlyIfInBaseSystem.
	stamp := aStamp.
</details>

#### ClassDeletionChangeRecord>>#methodSelector

<details>
	<summary>See more</summary>
	
	methodSelector
	^nil
</details>

#### ClassDeletionChangeRecord>>#changeClass

<details>
	<summary>See more</summary>
	
	changeClass
	^Smalltalk at: className ifAbsent: nil
</details>

#### ClassDeletionChangeRecord>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ Smalltalk removeClassNamedIfInBaseSystem: className]
		ifFalse: [ self changeClass ifNotNil: [ :aClass | aClass removeFromSystem ] ]
</details>

#### ClassDeletionChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp

	^stamp
</details>

#### ClassDeletionChangeRecord>>#isClassDeletion

<details>
	<summary>See more</summary>
	
	isClassDeletion
	^true
</details>

#### ClassDeletionChangeRecord>>#fileOutOn: stream

File the receiver out on the given file stream


<details>
	<summary>See more</summary>
	
	fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |
		
	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'classMoveToSomePackage: #' ] ifFalse: ['classRemoval: #']);
			nextPutAll: className;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine; newLine.
	
</details>

#### ClassDeletionChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType

	^ #classRemoval
</details>

#### ClassDeletionChangeRecord>>#isMetaClassChange

<details>
	<summary>See more</summary>
	
	isMetaClassChange
	^false
</details>

#### ClassDeletionChangeRecord>>#className: aSymbol

<details>
	<summary>See more</summary>
	
	className: aSymbol

	className _ aSymbol
</details>

#### ClassDeletionChangeRecord>>#command

<details>
	<summary>See more</summary>
	
	command

	^doItOnlyIfInBaseSystem
		ifTrue: [ 'Smalltalk removeClassNamedIfInBaseSystem: #', className ]
		ifFalse: [ 'Smalltalk removeClassNamed: #', className ].

</details>

## ClassRenamedChangeRecord

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ClassRenamedChangeRecord>>#string

<details>
	<summary>See more</summary>
	
	string
	^ 'classRenamed: #', previousName, ' as: #', newName, stamp 
</details>

#### ClassRenamedChangeRecord>>#changeClassName

<details>
	<summary>See more</summary>
	
	changeClassName
	^ previousName 
</details>

#### ClassRenamedChangeRecord>>#initializeFrom: previousClassName to: newClassName stamp: aString

<details>
	<summary>See more</summary>
	
	initializeFrom: previousClassName to: newClassName stamp: aString 

	previousName := previousClassName.
	newName := newClassName.
	stamp := aString.
</details>

#### ClassRenamedChangeRecord>>#changeClass

<details>
	<summary>See more</summary>
	
	changeClass
	^ nil
</details>

#### ClassRenamedChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp
	^stamp
</details>

#### ClassRenamedChangeRecord>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn
	
</details>

#### ClassRenamedChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType
	^ #classRenamed
</details>

#### ClassRenamedChangeRecord>>#newClassName

<details>
	<summary>See more</summary>
	
	newClassName
	^ newName 
</details>

## FeatureChangeRecord

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FeatureChangeRecord>>#changeClass

<details>
	<summary>See more</summary>
	
	changeClass
	^nil
</details>

#### FeatureChangeRecord>>#fileIn

It does nothing - Hernan


<details>
	<summary>See more</summary>
	
	fileIn
	
	"It does nothing - Hernan"
</details>

#### FeatureChangeRecord>>#string

<details>
	<summary>See more</summary>
	
	string
	^ type, ': ', feature printString
</details>

#### FeatureChangeRecord>>#type: aSymbol feature: aFeature

<details>
	<summary>See more</summary>
	
	type: aSymbol feature: aFeature
	type _ aSymbol.
	feature _ aFeature
</details>

#### FeatureChangeRecord>>#changeClassName

<details>
	<summary>See more</summary>
	
	changeClassName
	^nil
</details>

#### FeatureChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType
	^ type
</details>

#### FeatureChangeRecord>>#fileOutOn: aFileStream

Nothing to file out - Hernan


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream

	"Nothing to file out - Hernan"
</details>

#### FeatureChangeRecord>>#feature

<details>
	<summary>See more</summary>
	
	feature
	^feature
</details>

#### FeatureChangeRecord>>#methodSelector

<details>
	<summary>See more</summary>
	
	methodSelector
	^nil
</details>

## MethodChangeRecord

MethodChangeRecords are used to record method changes. Here is a simple summary of the relationship between the changeType symbol and the recording of prior state | prior == nil | prior not nil --------- |---------------------------- |-------------------- add | add | change --------- |---------------------------- |-------------------- remove | addedThenRemoved | remove Structure: changeType symbol -- as summarized above currentMethod method This is the current version of the method. It can be used to assert this change upon entry to a layer. infoFromRemoval -- an array of size 2. The first element is the source index of the last version of the method. The second element is the category in which it was defined, so it can be put back there if re-accepted from a version browser. Note that the above states each have an associated revoke action: add --> remove change --> change back remove --> add back addedThenRemoved --> no change However all of these are accomplished trivially by restoring the original method dictionary.

### Methods
#### MethodChangeRecord>>#priorMethod: ignored

We do not save original versions of changed methods because we only revoke changes at the level of entire classes, and that is done by restoration of the entire methodDictionary.


<details>
	<summary>See more</summary>
	
	priorMethod: ignored

	"We do not save original versions of changed methods because we only
	revoke changes at the level of entire classes, and that is done by
	restoration of the entire methodDictionary."
</details>

#### MethodChangeRecord>>#methodInfoFromRemoval

Return an array with the source index of the last version of the method, and the category in which it was defined (so it can be put back there if re-accepted from a version browser).


<details>
	<summary>See more</summary>
	
	methodInfoFromRemoval
	"Return an array with the source index of the last version of the method,
	and the category in which it was defined (so it can be put back there if
	re-accepted from a version browser)."

	(changeType == #remove or: [changeType == #addedThenRemoved])
		ifTrue: [^ infoFromRemoval]
		ifFalse: [^ nil]
</details>

#### MethodChangeRecord>>#noteNewMethod: newMethod

<details>
	<summary>See more</summary>
	
	noteNewMethod: newMethod

	currentMethod _ newMethod
</details>

#### MethodChangeRecord>>#noteChangeType: newChangeType

<details>
	<summary>See more</summary>
	
	noteChangeType: newChangeType

	stamp _ Utilities changeStamp.
	
	"Change of an added method, is still an add"
	(changeType == #add and: [ newChangeType == #change ])
		ifTrue: [ ^self ].

	"Change of an added method, is still an add"
	(changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ 
			changeType _ #add.
			^self ].

	changeType _ newChangeType.
</details>

#### MethodChangeRecord>>#printOn: strm

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: strm

	super printOn: strm.
	strm nextPutAll: ' ('; print: changeType; nextPutAll: ')'
</details>

#### MethodChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp

	^stamp 
</details>

#### MethodChangeRecord>>#noteMethodInfoFromRemoval: info

Store an array with the source index of the last version of the method, and the category in which it was defined (so it can be put back there if re-accepted from a version browser).


<details>
	<summary>See more</summary>
	
	noteMethodInfoFromRemoval: info
	"Store an array with the source index of the last version of the method,
	and the category in which it was defined (so it can be put back there if
	re-accepted from a version browser)."

	infoFromRemoval _ info
</details>

#### MethodChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType

	^ changeType
</details>

#### MethodChangeRecord>>#performOn: aCodeFile

<details>
	<summary>See more</summary>
	
	performOn: aCodeFile

	 ^aCodeFile perform: (self changeType copyWith: $:) asSymbol with: self 
</details>

#### MethodChangeRecord>>#currentMethod

<details>
	<summary>See more</summary>
	
	currentMethod

	^ currentMethod
</details>

#### MethodChangeRecord>>#storeDataOn: aDataStream

Store myself on a DataStream. Answer self. This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream. NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects. readDataFrom:size: reads back what we write here.


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	| oldMethod |
	oldMethod _ currentMethod.
	currentMethod _ nil.
	super storeDataOn: aDataStream.
	currentMethod _ oldMethod.

</details>

## MethodDeletionChangeRecord

Polymorphic with ChangeRecord. Used to represent methods that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Methods that will be deleted on install.

### Methods
#### MethodDeletionChangeRecord>>#doItOnlyIfInBaseSystem: aBoolean

<details>
	<summary>See more</summary>
	
	doItOnlyIfInBaseSystem: aBoolean
	doItOnlyIfInBaseSystem _ aBoolean
</details>

#### MethodDeletionChangeRecord>>#string

<details>
	<summary>See more</summary>
	
	string

	^sourceCode ifNil: [ '' ]
</details>

#### MethodDeletionChangeRecord>>#changeClassName

<details>
	<summary>See more</summary>
	
	changeClassName
	^methodReference classIsMeta
		ifFalse: [ methodReference classSymbol ]
		ifTrue: [ methodReference classSymbol, ' class' ]
</details>

#### MethodDeletionChangeRecord>>#methodSelector

<details>
	<summary>See more</summary>
	
	methodSelector
	^methodReference selector
</details>

#### MethodDeletionChangeRecord>>#isMethodDeletion

<details>
	<summary>See more</summary>
	
	isMethodDeletion
	^true
</details>

#### MethodDeletionChangeRecord>>#changeClass

<details>
	<summary>See more</summary>
	
	changeClass
	^methodReference actualClass
</details>

#### MethodDeletionChangeRecord>>#fileIn

<details>
	<summary>See more</summary>
	
	fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [ methodReference actualClass removeSelectorIfInBaseSystem: self methodSelector ]
		ifFalse: [ methodReference actualClass removeSelector: self methodSelector ]
</details>

#### MethodDeletionChangeRecord>>#stamp

<details>
	<summary>See more</summary>
	
	stamp

	^stamp
</details>

#### MethodDeletionChangeRecord>>#fileOutOn: stream

File the receiver out on the given file stream


<details>
	<summary>See more</summary>
	
	fileOutOn: stream
	"File the receiver out on the given file stream"

	| record |

	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'methodMoveToSomePackage: ' ] ifFalse: ['methodRemoval: ']);
			nextPutAll: self changeClassName;
			space;
			nextPutAll: self methodSelector storeString;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine
	

</details>

#### MethodDeletionChangeRecord>>#changeType

<details>
	<summary>See more</summary>
	
	changeType
	^ #method
</details>

#### MethodDeletionChangeRecord>>#isMetaClassChange

<details>
	<summary>See more</summary>
	
	isMetaClassChange
	^methodReference classIsMeta
</details>

#### MethodDeletionChangeRecord>>#command

<details>
	<summary>See more</summary>
	
	command

	^String streamContents: [ :stream |
		stream
			nextPutAll: self changeClassName;
			space;
			nextPutAll: (doItOnlyIfInBaseSystem ifTrue: ['removeSelectorIfInBaseSystem:'] ifFalse: ['removeSelector:']);
			space;
			nextPutAll: self methodSelector storeString ]
</details>

#### MethodDeletionChangeRecord>>#performOn: aCodeFile

<details>
	<summary>See more</summary>
	
	performOn: aCodeFile

	^aCodeFile removedMethod: self command with: self 
</details>

#### MethodDeletionChangeRecord>>#methodReference: aMethodReference

<details>
	<summary>See more</summary>
	
	methodReference: aMethodReference
	methodReference _ aMethodReference
</details>

#### MethodDeletionChangeRecord>>#initializeMethodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp

<details>
	<summary>See more</summary>
	
	initializeMethodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp  
	
	methodReference := aMethodReference.
	doItOnlyIfInBaseSystem := aDoit.
	sourceCode := aSourceCode.
	stamp := aStamp 
</details>

## SingleSetChangeSorter

A Change Sorter limited to operate on a single ChangeSet.

### Methods
#### SingleSetChangeSorter>>#changeSetList

Answer a list of ChangeSet names to be shown in the change sorter.


<details>
	<summary>See more</summary>
	
	changeSetList

	^{ myChangeSet name }
</details>

#### SingleSetChangeSorter>>#changeSetDirtyFlags

<details>
	<summary>See more</summary>
	
	changeSetDirtyFlags

	^myChangeSet hasUnsavedChanges
			ifTrue: [ #('     --->')]
			ifFalse: [ #('       -') ]
</details>

## VersionsBrowser

VersionsBrowser shows all the versions of a particular method, and lets you compare them, revert to selected versions, and so on.

### Methods
#### VersionsBrowser>>#showsVersions

<details>
	<summary>See more</summary>
	
	showsVersions
	^ true
</details>

#### VersionsBrowser>>#selectedClassOrMetaClass

Answer the class or metaclass currently selected in the browser. In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane


<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass
	"Answer the class or metaclass currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod
</details>

#### VersionsBrowser>>#addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream

Add an annotation detailing the prior versions count. Specially handled here for the case of a selector no longer in the system, whose prior versions are seen in a versions browser -- in this case, the inherited version of this method will not work.


<details>
	<summary>See more</summary>
	
	addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"Add an annotation detailing the prior versions count.  Specially handled here for the case of a selector no longer in the system, whose prior versions are seen in a versions browser -- in this case, the inherited version of this method will not work."

	(aClass includesSelector: aSelector) ifTrue:
		[^ super addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].

	aStream nextPutAll: 
		((changeList size > 0
			ifTrue:
				[changeList size = 1
					ifTrue:
						['Deleted - one prior version']
					ifFalse:
						['Deleted - ', changeList size printString, ' prior versions']]
			ifFalse:
				['surprisingly, no prior versions']), self annotationSeparator)
</details>

#### VersionsBrowser>>#addedChangeRecord

<details>
	<summary>See more</summary>
	
	addedChangeRecord
	^addedChangeRecord
</details>

#### VersionsBrowser>>#scanVersionsOf: method class: class meta: meta category: category selector: selector

<details>
	<summary>See more</summary>
	
	scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp prevPos prevFileIndex preamble tokens sourceFilesCopy |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex _ nil.
		prevPos _ nil.
		stamp _ ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				stamp _ CompiledMethod stampFrom: tokens.
				(CompiledMethod priorReferenceFrom: tokens) ifNotNil: [ :priorMethodRef |
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections
</details>

#### VersionsBrowser>>#addedChangeRecord: aChangeRecord

<details>
	<summary>See more</summary>
	
	addedChangeRecord: aChangeRecord
	addedChangeRecord _ aChangeRecord.
	self reformulateList.
</details>

#### VersionsBrowser>>#offerVersionsHelp

<details>
	<summary>See more</summary>
	
	offerVersionsHelp
	(TextModel new contents: self versionsHelpString)
		openLabel: 'Versions Browsers'
</details>

#### VersionsBrowser>>#compareToOtherVersion

Prompt the user for a reference version, then spawn a window showing the diffs between the older and the newer of the current version and the reference version as text.


<details>
	<summary>See more</summary>
	
	compareToOtherVersion
	"Prompt the user for a reference version, then spawn a window 
	showing the diffs between the older and the newer of the current 
	version and the reference version as text."

	| change1 change2 s1 s2 differDesc diffWords targetChanges labels label1 |
	change1 _ changeList at: listIndex ifAbsent: [ ^self ].
	label1 _ list at: listIndex.

	labels _ list reject: [ :any | any = label1 ].
	targetChanges _ changeList reject: [ :any | any = change1 ].
	change2 _ ((SelectionMenu
				labels: labels
				selections: targetChanges) startUpMenu) ifNil: [ ^self ].
	
	"compare earlier -> later"
	"change1 timeStamp < change2 timeStamp
		ifFalse: [ | temp | temp _ change1. change1 _ change2. change2 _ temp ]."

	s1 _ change1 string.
	s2 _ change2 string.
	s1 = s2
		ifTrue: [^ self inform: 'Exact Match'].

	diffWords _ self shouldDiffWords.
	differDesc _ diffWords
		ifTrue: [ 'Words']
		ifFalse: [ 'Lines'].

	(TextModel new
		contents: (DifferenceFinder
			displayPatchFrom: s1 to: s2
			tryWords: diffWords
			prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [classOfMethod])))
		openLabel: 
			(('Comparison from {1} to {2}: ', differDesc, 
				(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])) 
					format: { change1 stamp. change2 stamp })
</details>

#### VersionsBrowser>>#fileInSelections

<details>
	<summary>See more</summary>
	
	fileInSelections 
	super fileInSelections.
	self reformulateList
</details>

#### VersionsBrowser>>#classCommentIndicated

Answer whether the receiver is pointed at a class comment


<details>
	<summary>See more</summary>
	
	classCommentIndicated
	"Answer whether the receiver is pointed at a class comment"

	^ false
</details>

#### VersionsBrowser>>#updateIfNeeded

<details>
	<summary>See more</summary>
	
	updateIfNeeded
	| aMethod |
	aMethod _ classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ false].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].
	^ true

</details>

#### VersionsBrowser>>#versionsHelpString

<details>
	<summary>See more</summary>
	
	versionsHelpString
	^ 'Each entry in the list pane represents a version of the source code for the same method; the topmost entry is the current version, the next entry is the next most recent, etc.

To revert to an earlier version, select it (in the list pane) and then do any of the following:
  *  Choose "revert to this version" from the list pane menu.
  *  Hit the "revert" button,
  *  Type Return in the code pane
  *  Type cmd-s (alt-s) in the code pane.

The code pane shows the source for the selected version.  If "diffing" is in effect, then differences betwen the selected version and the version before it are pointed out in the pane.  Turn diffing on and off by choosing "toggle diffing" from the list pane menu, or hitting the "diffs" button, or hitting cmd-D when the cursor is over the list pane.

To get a comparison between the selected version and the current version, choose "compare to current" from the list pane menu or hit the "compare to current" button.  (This is meaningless if the current version is selected, and is unnecessary if you''re interested in diffs from between the current version and the next-most-recent version, since the standard in-pane "diff" feature will give you that.)

You can also compare the selected version with any other version using the "compare to version..." menu choice.

If further versions of the method in question have been submitted elsewhere since you launched a particular Versions Browser, it will still stay nicely up-to-date if you''re in Morphic and have asked that smart updating be maintained; if you''re in mvc or in morphic but with smart-updating turned off, a versions browser is only brought up to date when you activate its window (and when you issue "revert" from within it, of course,) and you can also use the "update list" command to make certain the versions list is up to date.

Hit the "remove from changes" button, or choose the corresponding command in the list pane menu, to have the method in question deleted from the current change set.  This is useful if you''ve put debugging code into a method, and now want to strip it out and cleanse your current change set of all memory of the excursion.

Note:  the annotation pane in versions browsers shows information about the *current* version of the method in the image, not about the selected version.'
</details>

#### VersionsBrowser>>#reformulateList

Some uncertainty about how to deal with lost methods here


<details>
	<summary>See more</summary>
	
	reformulateList
	| aMethod |
	"Some uncertainty about how to deal with lost methods here"
	aMethod _ classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	
	self scanVersionsOf: aMethod class: classOfMethod theNonMetaClass meta: classOfMethod isMeta category: (classOfMethod whichCategoryIncludesSelector: selectorOfMethod) selector: selectorOfMethod.
	self changed: #list. "for benefit of mvc"
	listIndex _ 1.
	self changed: #listIndex.
	self acceptedContentsChanged

</details>

#### VersionsBrowser>>#selectedClass

Answer the class currently selected in the browser. In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ classOfMethod theNonMetaClass
</details>

#### VersionsBrowser>>#selectedMessageName

Answer the message name currently selected in the browser. In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane


<details>
	<summary>See more</summary>
	
	selectedMessageName
	"Answer the message name currently selected in the browser.  In the case of a VersionsBrowser, the class and selector are always the same, regardless of which version is selected and indeed whether or not any entry is selected in the list pane"

	^ selectorOfMethod
</details>

