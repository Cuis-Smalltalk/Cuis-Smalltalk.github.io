## AttributesReplaceCommand

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### AttributesReplaceCommand>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream
		nextPutAll: 'replaced attributes'
</details>

#### AttributesReplaceCommand>>#old: oldAttributes new: newAttributes start: startIndex stop: stopIndex

<details>
	<summary>See more</summary>
	
	old: oldAttributes new: newAttributes start: startIndex stop: stopIndex
	old _ oldAttributes.
	new _ newAttributes.
	start _ startIndex.
	stop _ stopIndex
</details>

#### AttributesReplaceCommand>>#undoOn: aTextModel

Perform the command, used for initial execution or for redo after undoing


<details>
	<summary>See more</summary>
	
	undoOn: aTextModel
	"Perform the command, used for initial execution or for redo after undoing"

	aTextModel basicReplaceAttributesFrom: start to: stop with: old.
	^nil
</details>

#### AttributesReplaceCommand>>#doOn: aTextModel

Perform the command, used for initial execution or for redo after undoing


<details>
	<summary>See more</summary>
	
	doOn: aTextModel
	"Perform the command, used for initial execution or for redo after undoing"

	aTextModel basicReplaceAttributesFrom: start to: stop with: new.
	^nil
</details>

## CodeProvider

An ancestor class for all models which can show code.

### Methods
#### CodeProvider>>#didCodeChangeElsewhere

Determine whether the code for the currently selected method and class has been changed somewhere else.


<details>
	<summary>See more</summary>
	
	didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass aSelector aCompiledMethod |
	currentCompiledMethod ifNil: [^ false].
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ false].
	(aSelector _ self selectedMessageName) ifNil: [^ false].

	aSelector == #Comment ifTrue:
		[^ currentCompiledMethod ~~ aClass organization commentRemoteStr].
	^ ((aCompiledMethod _ aClass compiledMethodAt: aSelector ifAbsent: [^ false]) ~~ currentCompiledMethod)
		and: [aCompiledMethod last ~= 0 "either not yet installed"
				or: [currentCompiledMethod last = 0 "or these methods don't have source pointers"]]
	
</details>

#### CodeProvider>>#acceptedContentsChanged

<details>
	<summary>See more</summary>
	
	acceptedContentsChanged

	self changed: #acceptedContents.
	self triggerEvent: #decorateButtons.
	self triggerEvent: #annotationChanged
</details>

#### CodeProvider>>#showingAnyKindOfDiffs

Answer whether the receiver is currently set to show any kind of diffs


<details>
	<summary>See more</summary>
	
	showingAnyKindOfDiffs
	"Answer whether the receiver is currently set to show any kind of diffs"

	^ #(lineDiffs prettyLineDiffs wordDiffs prettyWordDiffs) includes: self contentsSymbol
</details>

#### CodeProvider>>#showingLineDiffs

Answer whether the receiver is showing regular diffs of source code


<details>
	<summary>See more</summary>
	
	showingLineDiffs
	"Answer whether the receiver is showing regular diffs of source code"

	^ self contentsSymbol == #lineDiffs

</details>

#### CodeProvider>>#togglePlainSource

Toggle whether plain source shown in the code pane


<details>
	<summary>See more</summary>
	
	togglePlainSource
	"Toggle whether plain source shown in the code pane"
	self showingPlainSource
		ifTrue: [
			self contentsSymbol: #documentation]
		ifFalse: [
			self contentsSymbol: #source]
</details>

#### CodeProvider>>#timeStamp

Answer the time stamp for the chosen class and method, if any, else an empty string


<details>
	<summary>See more</summary>
	
	timeStamp
	"Answer the time stamp for the chosen class and method, if any, else an empty string"

	|  selector  |
	(selector _ self selectedMessageName) ifNotNil: [
		^self selectedClassOrMetaClass 
			ifNil: [
				String new]
			ifNotNil: [
				self selectedClassOrMetaClass stampAt: selector]].
	^ String new
</details>

#### CodeProvider>>#showingPrettyWordDiffsString

Answer a string representing whether I'm showing pretty diffs


<details>
	<summary>See more</summary>
	
	showingPrettyWordDiffsString
	"Answer a string representing whether I'm showing pretty diffs"

	^ (self showingPrettyWordDiffs
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'wordPrettyDiffs'
</details>

#### CodeProvider>>#sourceAndDiffsQuintsOnly

Answer a list of quintuplets representing information on the alternative views available in the code pane for the case where the only plausible choices are showing source or either of the two kinds of diffs


<details>
	<summary>See more</summary>
	
	sourceAndDiffsQuintsOnly
	"Answer a list of quintuplets representing information on the alternative views available in the code pane for the case where the only plausible choices are showing source or either of the two kinds of diffs"

	^ #(
(source				togglePlainSource 			showingPlainSourceString
														'source'			'the textual source code as writen')
(lineDiffs				toggleLineDiffing			showingLineDiffsString
														'lineDiffs'			'the textual source diffed from its prior version')
(wordDiffs			toggleWordDiffing			showingWordDiffsString
														'wordDiffs'			'the textual source words diffed from its prior version')
(prettyLineDiffs		togglePrettyLineDiffing	showingPrettyLineDiffsString
														'linePrettyDiffs'		'formatted source diffed from formatted prior version')
(prettyWordDiffs	togglePrettyWordDiffing	showingPrettyWordDiffsString
														'linePrettyDiffs'		'formatted source words diffed from prior version')
	)
</details>

#### CodeProvider>>#showingWordDiffs

Answer whether the receiver is showing regular diffs (alternative algorithm) of source code


<details>
	<summary>See more</summary>
	
	showingWordDiffs
	"Answer whether the receiver is showing regular diffs (alternative algorithm) of source code"

	^ self contentsSymbol == #wordDiffs

</details>

#### CodeProvider>>#selectedClass

<details>
	<summary>See more</summary>
	
	selectedClass
	^ nil
</details>

#### CodeProvider>>#sourceStringPrettifiedAndDiffed

Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies


<details>
	<summary>See more</summary>
	
	sourceStringPrettifiedAndDiffed
	"Answer a copy of the source code for the selected message, transformed by diffing and pretty-printing exigencies"
	| class selector sourceString |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	(class isNil or: [ selector isNil ]) ifTrue: [ ^ 'missing' ].
	sourceString _ class
		ultimateSourceCodeAt: selector
		ifAbsent: [ ^ 'error' ].
	(self showingPrettyPrint or: [ self showingAnyKindOfPrettyDiffs ]) ifTrue: [
		sourceString _ class compilerClass new
			format: sourceString
			in: class
			notifying: nil ].
	self showingAnyKindOfDiffs ifTrue: [
		sourceString _ self diffFromPriorSourceFor: sourceString ].
	^ sourceString
</details>

#### CodeProvider>>#priorSourceOrNil

If the currently-selected method has a previous version, return its source, else return nil


<details>
	<summary>See more</summary>
	
	priorSourceOrNil
	"If the currently-selected method has a previous version, return its source, else return nil"
	| aClass aSelector  changeRecords |
	(aClass _ self selectedClassOrMetaClass) ifNil: [^ nil].
	(aSelector _ self selectedMessageName) ifNil: [^ nil].
	changeRecords _ aClass changeRecordsAt: aSelector.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [^ nil].
	^ (changeRecords at: 2) string 

</details>

#### CodeProvider>>#addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream

add an annotation detailing the prior versions count


<details>
	<summary>See more</summary>
	
	addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream
	"add an annotation detailing the prior versions count"
	| versionsCount |

	versionsCount _ VersionsBrowser versionCountForSelector: aSelector class: aClass.
	aStream nextPutAll: 
				((versionsCount > 1
					ifTrue:
						[versionsCount = 2 ifTrue:
							['1 prior version']
							ifFalse:
								[versionsCount printString, ' prior versions']]
					ifFalse:
						['no prior versions']), self annotationSeparator)
</details>

#### CodeProvider>>#toggleDecompile

Toggle the setting of the showingDecompile flag, unless there are unsubmitted edits that the user declines to discard


<details>
	<summary>See more</summary>
	
	toggleDecompile
	"Toggle the setting of the showingDecompile flag, unless there are unsubmitted edits that the user declines to discard"

	self showDecompile: self showingDecompile not
</details>

#### CodeProvider>>#toggleLineDiffing

Toggle whether regular-diffing should be shown in the code pane


<details>
	<summary>See more</summary>
	
	toggleLineDiffing
	"Toggle whether regular-diffing should be shown in the code pane"

	self showLineDiffs: self showingLineDiffs not
</details>

#### CodeProvider>>#toggleDiffing

Toggle whether diffs should be shown in the code pane. If any kind of diffs were being shown, stop showing diffs. If no kind of diffs were being shown, start showing whatever kind of diffs are called for by default.


<details>
	<summary>See more</summary>
	
	toggleDiffing
	"Toggle whether diffs should be shown in the code pane.  If any kind of diffs were being shown, stop showing diffs.  If no kind of diffs were being shown, start showing whatever kind of diffs are called for by default."

	self showDiffs: self showingAnyKindOfDiffs not
</details>

#### CodeProvider>>#hierarchyBrowser

Create and schedule a new hierarchy browser on the currently selected class or meta.


<details>
	<summary>See more</summary>
	
	hierarchyBrowser
	"Create and schedule a new hierarchy browser on the currently selected class or meta."

	| newBrowser aSymbol aBehavior messageCatIndex selectedClassOrMetaClass |
	(selectedClassOrMetaClass _ self selectedClassOrMetaClass)
		ifNil: [^ nil].
	newBrowser _ HierarchyBrowser new initHierarchyForClass: selectedClassOrMetaClass.
	((aSymbol _ self selectedMessageName) notNil and: [(MessageSet isPseudoSelector: aSymbol) not])
		ifTrue: [
			aBehavior _ selectedClassOrMetaClass.
			messageCatIndex _ aBehavior organization numberOfCategoryOfElement: aSymbol.
			messageCatIndex = 0 ifFalse: [
				newBrowser messageCategoryListIndex: messageCatIndex + 1.
				newBrowser messageListIndex:
					((aBehavior organization listAtCategoryNumber: messageCatIndex) indexOf: aSymbol) ]].
	^newBrowser
</details>

#### CodeProvider>>#showingPlainSourceString

Answer a string telling whether the receiver is showing plain source


<details>
	<summary>See more</summary>
	
	showingPlainSourceString
	"Answer a string telling whether the receiver is showing plain source"

	^ (self showingPlainSource
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'source'
</details>

#### CodeProvider>>#showingDecompileString

Answer a string characerizing whether decompilation is showing


<details>
	<summary>See more</summary>
	
	showingDecompileString
	"Answer a string characerizing whether decompilation is showing"

	^ (self showingDecompile
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'decompile'
</details>

#### CodeProvider>>#defaultDiffsSymbol

Answer the code symbol to use when generically switching to diffing


<details>
	<summary>See more</summary>
	
	defaultDiffsSymbol
	"Answer the code symbol to use when generically switching to diffing"

	^ Preferences diffsWithPrettyPrint 
		ifTrue: [
			#prettyLineDiffs]
		ifFalse: [
			#lineDiffs]
</details>

#### CodeProvider>>#togglePrettyLineDiffing

Toggle whether pretty-diffing should be shown in the code pane


<details>
	<summary>See more</summary>
	
	togglePrettyLineDiffing
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showPrettyLineDiffs: self showingPrettyLineDiffs not
</details>

#### CodeProvider>>#selectedBytecodes

Answer text to show in a code pane when in showing-byte-codes mode


<details>
	<summary>See more</summary>
	
	selectedBytecodes
	"Answer text to show in a code pane when in showing-byte-codes mode"

	^ (self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName ifAbsent: [^ '' asText]) symbolic asText
</details>

#### CodeProvider>>#showDiffs: aBoolean

Set whether I'm showing diffs as indicated; use the global preference to determine which kind of diffs to institute.


<details>
	<summary>See more</summary>
	
	showDiffs: aBoolean
	"Set whether I'm showing diffs as indicated; use the global preference to determine which kind of diffs to institute."

	self showingAnyKindOfDiffs
		ifFalse: [
			aBoolean ifTrue: [
				self contentsSymbol: self defaultDiffsSymbol]]
		ifTrue: [
			aBoolean ifFalse: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#contentsSymbol: aSymbol

Set the contentsSymbol as indicated. #source means to show source code, #comment means to show the first comment found in the source code


<details>
	<summary>See more</summary>
	
	contentsSymbol: aSymbol
	"Set the contentsSymbol as indicated.  #source means to show source code, #comment means to show the first comment found in the source code"

	contentsSymbol _ aSymbol.
	self acceptedContentsChanged
</details>

#### CodeProvider>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass autoCompleterClass ].
	^SmalltalkCompleter
</details>

#### CodeProvider>>#selectedClassOrMetaClass

<details>
	<summary>See more</summary>
	
	selectedClassOrMetaClass

	^ self selectedClass	"I don't know any better"
</details>

#### CodeProvider>>#shouldDiffWords

Answer whether the receiver is currently set to use the word based differ


<details>
	<summary>See more</summary>
	
	shouldDiffWords
	"Answer whether the receiver is currently set to use the word based differ"

	^ #(wordDiffs prettyWordDiffs) includes: self contentsSymbol
</details>

#### CodeProvider>>#inspectCompiledMethod

Open an Inspector on the CompiledMethod itself


<details>
	<summary>See more</summary>
	
	inspectCompiledMethod
	"Open an Inspector on the CompiledMethod itself"

	self selectedMessageName ifNotNil: [
		(self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName)
			inspect ]
</details>

#### CodeProvider>>#annotationForClassCommentFor: aClass

Provide a line of content for an annotation pane, given that the receiver is pointing at the clas comment of the given class.


<details>
	<summary>See more</summary>
	
	annotationForClassCommentFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the clas comment of the given class."

	| aStamp nonMeta |
	aStamp _  (nonMeta _ aClass theNonMetaClass) organization commentStamp.
	^ aStamp
		ifNil:
			[nonMeta name, ' has no class comment']
		ifNotNil:
			['class comment for ', nonMeta name,
				(aStamp = '<historical>'
					ifFalse:
						[' - ', aStamp]
					ifTrue:
						[''])]
</details>

#### CodeProvider>>#showingSource

Answer whether the receiver is currently showing source code


<details>
	<summary>See more</summary>
	
	showingSource
	"Answer whether the receiver is currently showing source code"

	^ self contentsSymbol == #source

</details>

#### CodeProvider>>#instanceVariableRenamed

<details>
	<summary>See more</summary>
	
	instanceVariableRenamed

	self acceptedContentsChanged

</details>

#### CodeProvider>>#showDecompile: aBoolean

Set the decompile toggle as indicated


<details>
	<summary>See more</summary>
	
	showDecompile: aBoolean
	"Set the decompile toggle as indicated"

	self contentsSymbol: (aBoolean ifFalse: [#source] ifTrue: [#decompile])
</details>

#### CodeProvider>>#revertToPreviousVersion

Revert to the previous version of the current method


<details>
	<summary>See more</summary>
	
	revertToPreviousVersion
	"Revert to the previous version of the current method"
	| aClass aSelector  changeRecords |
	aClass _ self selectedClassOrMetaClass.
	aClass ifNil: [^ self changed: #flash].
	aSelector _ self selectedMessageName.
	changeRecords _ aClass changeRecordsAt: aSelector.
	(changeRecords == nil or: [changeRecords size <= 1]) ifTrue: [self changed: #flash.  ^ Smalltalk beep].
	changeRecords second fileIn.
	self acceptedContentsChanged
</details>

#### CodeProvider>>#showingLineDiffsString

Answer a string representing whether I'm showing regular diffs


<details>
	<summary>See more</summary>
	
	showingLineDiffsString
	"Answer a string representing whether I'm showing regular diffs"

	^ (self showingLineDiffs
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'lineDiffs'
</details>

#### CodeProvider>>#updateIfNeeded

<details>
	<summary>See more</summary>
	
	updateIfNeeded
	self didCodeChangeElsewhere
		ifTrue: [
			self acceptedContentsChanged]
</details>

#### CodeProvider>>#contentsSymbol

Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source. A nil value in the contentsSymbol slot will be set to #source by this method


<details>
	<summary>See more</summary>
	
	contentsSymbol
	"Answer a symbol indicating what kind of content should be shown for the method; for normal showing of source code, this symbol is #source.  A nil value in the contentsSymbol slot will be set to #source by this method"

	^ contentsSymbol ifNil: [
		contentsSymbol _ Preferences browseWithPrettyPrint
					ifTrue:
						[#prettyPrint]
					ifFalse:
						[#source]]
</details>

#### CodeProvider>>#okayToAccept

Answer whether it is okay to accept the receiver's input


<details>
	<summary>See more</summary>
	
	okayToAccept
	"Answer whether it is okay to accept the receiver's input"

	self showingByteCodes ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingDocumentation ifTrue: [
		self inform: 
'Sorry, you can only submit changes here 
when you are showing source.'.
		^ false].

	self showingAnyKindOfDiffs ifTrue: [
		^ SelectionMenu confirm: 
'Caution!  You are "showing diffs" here, so 
there is a danger that some of the text in the
code pane is contaminated by the "diff" display'
		trueChoice: 'accept anyway -- I''ll take my chances' falseChoice: 'um, let me reconsider' icons: #(acceptIcon cancelIcon)
	].

	^ true
</details>

#### CodeProvider>>#currentCompiledMethod

<details>
	<summary>See more</summary>
	
	currentCompiledMethod
	^currentCompiledMethod
</details>

#### CodeProvider>>#removeClass

Remove the selected class from the system, at interactive user request. Make certain the user really wants to do this, since it is not reversible. Answer true if removal actually happened.


<details>
	<summary>See more</summary>
	
	removeClass
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message  className classToRemove result |
	classToRemove _ self selectedClassOrMetaClass ifNil: [Smalltalk beep. ^ false].
	classToRemove _ classToRemove theNonMetaClass.
	className _ classToRemove name.
	message _ 'Are you certain that you
want to REMOVE the class ', className, '
from the system?'.
	(result _ self confirm: message)
		ifTrue: 
			[classToRemove subclasses size > 0
				ifTrue: [(self confirm: 'class has subclasses: ' , message)
					ifFalse: [^ false]].
			classToRemove removeFromSystem.
			self changed: #classList.
			true].
	^ result
</details>

#### CodeProvider>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Answer false if showing difs, to veto the styling.


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer false if showing difs, to veto the styling."
	
	^self showingAnyKindOfDiffs not
</details>

#### CodeProvider>>#showDocumentation: aBoolean

Set the showDocumentation toggle as indicated


<details>
	<summary>See more</summary>
	
	showDocumentation: aBoolean
	"Set the showDocumentation toggle as indicated"

	self contentsSymbol: (aBoolean ifFalse: [#source] ifTrue: [#documentation])
</details>

#### CodeProvider>>#categoryFromUserWithPrompt: aPrompt for: aClass

self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary


<details>
	<summary>See more</summary>
	
	categoryFromUserWithPrompt: aPrompt for: aClass
	"self new categoryFromUserWithPrompt: 'testing' for: SystemDictionary"

	|  labels myCategories reject lines newName menuIndex |
	labels _ OrderedCollection with: 'new...'.
	labels addAll: (myCategories _ aClass organization categories asArray copy sort:
		[ :a :b | a asLowercase < b asLowercase ]).
	reject _ myCategories asSet.
	reject
		add: ClassOrganizer nullCategory;
		add: ClassOrganizer default.
	lines _ OrderedCollection with: 1 with: (myCategories size + 1).

	aClass allSuperclasses do: [ :cls | | cats |
			cats _ cls organization categories reject: [ :cat | reject includes: cat].
			cats isEmpty ifFalse: [
				lines add: labels size.
				labels addAll: (cats asArray sort: [ :a :b | a asLowercase < b asLowercase]).
				reject addAll: cats]].

	(labels size = 1 or: [
		menuIndex _ (PopUpMenu labelArray: labels lines: lines)
		startUpWithCaption: aPrompt.
		menuIndex = 0 ifTrue: [^ nil].
		menuIndex = 1])
			ifTrue:[
				newName _ FillInTheBlankMorph request: 'Please type new category name' initialAnswer: 'category name'.
				newName isEmpty ifTrue: [ ^nil ]]
			ifFalse: [ newName _ labels at: menuIndex ].
	^ newName ifNotNil: [ newName asSymbol ]
</details>

#### CodeProvider>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	(#(acceptedContents classCommentText) statePointsTo: textGetter) ifFalse: [
		^super editorClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass editorClass ].
	^SmalltalkEditor
</details>

#### CodeProvider>>#commentContents

documentation for the selected method


<details>
	<summary>See more</summary>
	
	commentContents
	"documentation for the selected method"

	| poss aClass aSelector |
	^ (poss _ (aClass _ self selectedClassOrMetaClass)
						ifNil:
							['----']
						ifNotNil:
							[(aSelector _ self selectedMessageName)
								ifNil:
									['---']
								ifNotNil:
									[(aClass precodeCommentOrInheritedCommentFor: aSelector)", String crString, String crString, self timeStamp"
"which however misses comments that are between the temps  declaration and the body of the method; those are picked up by [aClass commentOrInheritedCommentFor: aSelector] but that method will get false positives from comments *anywhere* in the method source"]])
		isEmptyOrNil
			ifTrue:
				[aSelector
					ifNotNil:
						[((aClass methodHeaderFor: aSelector), '

Has no comment') ]
					ifNil:
						['Hamna']]
			ifFalse:	[aSelector
				ifNotNil: [((aClass methodHeaderFor: aSelector), '

', poss) ]
				ifNil: [poss]]
</details>

#### CodeProvider>>#showLineDiffs: aBoolean

Set whether I'm showing regular diffs as indicated


<details>
	<summary>See more</summary>
	
	showLineDiffs: aBoolean
	"Set whether I'm showing regular diffs as indicated"

	self showingLineDiffs
		ifFalse: [
			aBoolean ifTrue: [
				self contentsSymbol: #lineDiffs]]
		ifTrue: [
			aBoolean ifFalse: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#isThereAnOverride

Answer whether any subclass of my selected class implements my selected selector


<details>
	<summary>See more</summary>
	
	isThereAnOverride
	"Answer whether any subclass of my selected class implements my 
	selected selector"
	| aName aClass |
	aName := self selectedMessageName
				ifNil: [^ false].
	aClass := self selectedClassOrMetaClass ifNil: [ ^ false ].
	aClass allSubclassesDo: [ :cls | (cls includesSelector: aName) ifTrue: [ ^true ]].
	^ false
</details>

#### CodeProvider>>#annotationRequests

<details>
	<summary>See more</summary>
	
	annotationRequests
	^ Preferences defaultAnnotationRequests
</details>

#### CodeProvider>>#annotationSeparator

Answer the separator to be used between annotations


<details>
	<summary>See more</summary>
	
	annotationSeparator
	"Answer the separator to be used between annotations"

	^ ' ∞ '
</details>

#### CodeProvider>>#unusedMethods

<details>
	<summary>See more</summary>
	
	unusedMethods
	| classes unsent messageList cls |

	(cls _ self selectedClass) ifNil: [^ nil].
	classes _ Array with: cls with: cls class.
	unsent _ Set new.
	classes do: [:c | unsent addAll: c selectors].
	unsent _ Smalltalk allUnSentMessagesIn: unsent.
	messageList _ OrderedCollection new.
	classes do: [:c | (c selectors select: [:s | unsent includes: s]) asArray sort
					do: [:sel | messageList add: 
						(MethodReference class: c selector: sel) ]].
	^messageList
</details>

#### CodeProvider>>#canShowMultipleMessageCategories

Answer whether the receiver is capable of showing multiple message categories


<details>
	<summary>See more</summary>
	
	canShowMultipleMessageCategories
	"Answer whether the receiver is capable of showing multiple message categories"

	^ false
</details>

#### CodeProvider>>#togglePrettyPrint

Toggle whether pretty-print is in effectin the code pane


<details>
	<summary>See more</summary>
	
	togglePrettyPrint
	"Toggle whether pretty-print is in effectin the code pane"

	self showingPrettyPrint
		ifTrue: [
			self contentsSymbol: #source]
		ifFalse: [
			self contentsSymbol: #prettyPrint]
</details>

#### CodeProvider>>#textStylerClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	textStylerClassFor: textGetter
	textGetter = #acceptedContents ifFalse: [
		^super textStylerClassFor: textGetter ].
	currentCompiledMethod ifNotNil: [ :cm |
		^cm compilerClass textStylerClass ].
	^SHTextStylerST80
</details>

#### CodeProvider>>#prettyPrintString

Answer whether the receiver is showing pretty-print


<details>
	<summary>See more</summary>
	
	prettyPrintString
	"Answer whether the receiver is showing pretty-print"

	^ ((self contentsSymbol == #prettyPrint)
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'prettyPrint'
</details>

#### CodeProvider>>#methodCategoryChanged

<details>
	<summary>See more</summary>
	
	methodCategoryChanged
	self triggerEvent: #annotationChanged
</details>

#### CodeProvider>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock
	
</details>

#### CodeProvider>>#showByteCodes: aBoolean

Get into or out of bytecode-showoing mode


<details>
	<summary>See more</summary>
	
	showByteCodes: aBoolean
	"Get into or out of bytecode-showoing mode"

	aBoolean
		ifTrue: [
			self contentsSymbol: #byteCodes]
		ifFalse: [
			self contentsSymbol == #byteCodes ifTrue: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#isModeStyleable

determine if Shout can style in the current mode


<details>
	<summary>See more</summary>
	
	isModeStyleable
	"determine if Shout can style in the current mode"
	^ self showingSource or: [self showingPrettyPrint or: [self showingDecompile]]
</details>

#### CodeProvider>>#letUserReclassify: anElement in: aClass

Put up a list of categories and solicit one from the user. Answer true if user indeed made a change, else false


<details>
	<summary>See more</summary>
	
	letUserReclassify: anElement in: aClass
	"Put up a list of categories and solicit one from the user.  
	Answer true if user indeed made a change, else false"
	

	| currentCat newCat |
	currentCat _ aClass organization categoryOfElement: anElement.
	newCat _ self 
				categoryFromUserWithPrompt: 'choose category (currently "', currentCat, '")' 
				for: aClass.
	(newCat notNil and: [newCat ~= currentCat])
		ifTrue: [
			aClass organization classify: anElement under: newCat suppressIfDefault: false.
			^ true]
		ifFalse: [
			^ false]
</details>

#### CodeProvider>>#showWordDiffs: aBoolean

Set whether I'm showing regular diffs as indicated


<details>
	<summary>See more</summary>
	
	showWordDiffs: aBoolean
	"Set whether I'm showing regular diffs as indicated"

	self showingWordDiffs
		ifFalse: [
			aBoolean ifTrue: [
				self contentsSymbol: #wordDiffs]]
		ifTrue: [
			aBoolean ifFalse: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#releaseCachedState

Can always be found again. Don't write on a file.


<details>
	<summary>See more</summary>
	
	releaseCachedState
	"Can always be found again.  Don't write on a file."
	currentCompiledMethod _ nil.
</details>

#### CodeProvider>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #CodeProvider or: [ super is: aSymbol ]
</details>

#### CodeProvider>>#annotationForClassDefinitionFor: aClass

Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class.


<details>
	<summary>See more</summary>
	
	annotationForClassDefinitionFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."

	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Class definition for ';
			nextPutAll: aClass name;
			nextPutAll: '. '.
		aClass theNonMetaClass selectors size printOn: strm.
		strm nextPutAll: ' instance methods. '.
		aClass theMetaClass selectors size printOn: strm.
		strm nextPutAll: ' class methods. '.
		aClass theNonMetaClass linesOfCode printOn: strm.
		strm nextPutAll: ' total lines of code.' ]
</details>

#### CodeProvider>>#showingByteCodesString

Answer whether the receiver is showing bytecodes


<details>
	<summary>See more</summary>
	
	showingByteCodesString
	"Answer whether the receiver is showing bytecodes"

	^ (self showingByteCodes
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'byteCodes'
</details>

#### CodeProvider>>#categoryOfCurrentMethod

Answer the category that owns the current method. If unable to determine a category, answer nil.


<details>
	<summary>See more</summary>
	
	categoryOfCurrentMethod
	"Answer the category that owns the current method.  If unable to determine a category, answer nil."

	^ self selectedClassOrMetaClass ifNotNil: [ :cls | 
		self selectedMessageName ifNotNil: [ :sel |
			cls whichCategoryIncludesSelector: sel]]
</details>

#### CodeProvider>>#copySelector

Copy the selected selector to the clipboard


<details>
	<summary>See more</summary>
	
	copySelector
	"Copy the selected selector to the clipboard"

	| selector |
	(selector _ self selectedMessageName) ifNotNil: [
		Clipboard storeObject: selector asString]
</details>

#### CodeProvider>>#toggleShowingByteCodes

Toggle whether the receiver is showing bytecodes


<details>
	<summary>See more</summary>
	
	toggleShowingByteCodes
	"Toggle whether the receiver is showing bytecodes"

	self showByteCodes: self showingByteCodes not
</details>

#### CodeProvider>>#annotationForSelector: aSelector ofClass: aClass

Provide a line of content for an annotation pane, representing information about the given selector and class


<details>
	<summary>See more</summary>
	
	annotationForSelector: aSelector ofClass: aClass 
	"Provide a line of content for an annotation pane, representing  
	information about the given selector and class"
	| stamp sendersCount implementorsCount aCategory separator aString aList aComment stream requestList |
	aSelector == #Comment
		ifTrue: [^ self annotationForClassCommentFor: aClass].
	aSelector == #Definition
		ifTrue: [^ self annotationForClassDefinitionFor: aClass].
	aSelector == #Hierarchy
		ifTrue: [^ self annotationForHierarchyFor: aClass].
	stream _ WriteStream on: String new.
	requestList _ self annotationRequests.
	separator _ requestList size > 1
				ifTrue: [self annotationSeparator]
				ifFalse: [''].
	requestList
		do: [:aRequest | 
			aRequest == #firstComment
				ifTrue: [
					aComment _ aClass firstCommentAt: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #masterComment
				ifTrue: [
					aComment _ aClass supermostPrecodeCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #documentation
				ifTrue: [
					aComment _ aClass precodeCommentOrInheritedCommentFor: aSelector.
					aComment isEmptyOrNil
						ifFalse: [stream nextPutAll: aComment , separator]].
			aRequest == #timeStamp
				ifTrue: [
					stamp _ self timeStamp.
					stream
						nextPutAll: (stamp size > 0
								ifTrue: [stamp , separator]
								ifFalse: ['no timeStamp' , separator])].
			aRequest == #messageCategory
				ifTrue: [
					aCategory _ aClass organization categoryOfElement: aSelector.
					aCategory
						ifNotNil: ["woud be nil for a method no longer present,  
							e.g. in a recent-submissions browser"
							stream nextPutAll: aCategory , separator]].
			aRequest == #sendersCount
				ifTrue: [
					sendersCount _ Smalltalk numberOfSendersOf: aSelector.
					sendersCount _ sendersCount = 1
								ifTrue: ['1 sender']
								ifFalse: [sendersCount printString , ' senders'].
					stream nextPutAll: sendersCount , separator].
			aRequest == #implementorsCount
				ifTrue: [
					implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.
					implementorsCount _ implementorsCount = 1
								ifTrue: ['1 implementor']
								ifFalse: [implementorsCount printString , ' implementors'].
					stream nextPutAll: implementorsCount , separator].
			aRequest == #priorVersionsCount
				ifTrue: [
					self
						addPriorVersionsCountForSelector: aSelector
						ofClass: aClass
						to: stream].
			aRequest == #priorTimeStamp
				ifTrue: [
					stamp _ VersionsBrowser
								timeStampFor: aSelector
								class: aClass
								reverseOrdinal: 2.
					stamp
						ifNotNil: [stream nextPutAll: 'prior time stamp: ' , stamp , separator]].
			aRequest == #packages
				ifTrue: [
					(aClass compiledMethodAt: aSelector ifAbsent: nil) ifNotNil: [ :cm |
						(CodePackage packageOfMethod: cm methodReference ifNone: nil)
							ifNil: [ stream nextPutAll: 'part of base system (i.e. not in a package)' ]
							ifNotNil: [ :codePackage |
								stream nextPutAll: 'in package '; nextPutAll: codePackage packageName ].
						stream nextPutAll: separator]].
			aRequest == #changeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allChangeSets
				ifTrue: [
					aList _ ChangeSet allChangeSetsWithClass: aClass selector: aSelector.
					aList size > 0
						ifTrue: [aList size = 1
								ifTrue: [stream nextPutAll: 'only in change set ']
								ifFalse: [stream nextPutAll: 'in change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no change set'].
					stream nextPutAll: separator].
			aRequest == #allBaseSystemChangeSets
				ifTrue: [
					aList _ (ChangeSet allChangeSetsWithClass: aClass selector: aSelector) select: [ :it | it isForBaseSystem ].
					aList size > 0
						ifTrue: [
							aList size = 1
								ifTrue: [stream nextPutAll: 'only in base system change set']
								ifFalse: [stream nextPutAll: 'in base system change sets:'].
							aList
								do: [:aChangeSet | stream nextPut: Character space; nextPutAll: aChangeSet name ]
								separatedBy: [ stream nextPut: $, ]]
						ifFalse: [stream nextPutAll: 'in no base system change set'].
					stream nextPutAll: separator].
			aRequest == #closuresInfo
				ifTrue: [
					aString _ aClass closuresInfoAt: aSelector.
					aString size > 0
						ifTrue: [stream nextPutAll: aString , separator]].
		].
	^ stream contents
</details>

#### CodeProvider>>#showingWordDiffsString

Answer a string representing whether I'm showing regular diffs


<details>
	<summary>See more</summary>
	
	showingWordDiffsString
	"Answer a string representing whether I'm showing regular diffs"

	^ (self showingWordDiffs
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'wordDiffs'
</details>

#### CodeProvider>>#annotationForHierarchyFor: aClass

Provide a line of content for an annotation pane, given that the receiver is pointing at the hierarchy of the given class.


<details>
	<summary>See more</summary>
	
	annotationForHierarchyFor: aClass
	"Provide a line of content for an annotation pane, given that the receiver is pointing at the hierarchy of the given class."

	^ 'Hierarchy for ', aClass name
</details>

#### CodeProvider>>#showPrettyWordDiffs: aBoolean

Set whether I'm showing pretty diffs as indicated


<details>
	<summary>See more</summary>
	
	showPrettyWordDiffs: aBoolean
	"Set whether I'm showing pretty diffs as indicated"

	self showingPrettyWordDiffs
		ifFalse: [
			aBoolean ifTrue: [
				self contentsSymbol: #prettyWordDiffs]]
		ifTrue: [
			aBoolean ifFalse: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#selectedMessageName

<details>
	<summary>See more</summary>
	
	selectedMessageName

	^ nil
</details>

#### CodeProvider>>#isThisAnOverride

Answer whether any superclass of my selected class implements my selected selector


<details>
	<summary>See more</summary>
	
	isThisAnOverride
	"Answer whether any superclass of my selected class implements my selected selector"
	| aName aClass |
	aName _ self selectedMessageName ifNil: [^ false].
	aClass _ self selectedClassOrMetaClass ifNil: [^false ].
	aClass allSuperclassesDo: [ :cls | (cls includesSelector: aName) ifTrue: [ ^true ]].
	^ false
</details>

#### CodeProvider>>#showingDecompile

Answer whether the receiver should show decompile rather than, say, source code


<details>
	<summary>See more</summary>
	
	showingDecompile
	"Answer whether the receiver should show decompile rather than, say, source code"

	^ self contentsSymbol == #decompile

</details>

#### CodeProvider>>#showingDocumentationString

Answer a string characerizing whether documentation is showing


<details>
	<summary>See more</summary>
	
	showingDocumentationString
	"Answer a string characerizing whether documentation is showing"

	^ (self showingDocumentation
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'documentation'
</details>

#### CodeProvider>>#showingPrettyLineDiffsString

Answer a string representing whether I'm showing pretty diffs


<details>
	<summary>See more</summary>
	
	showingPrettyLineDiffsString
	"Answer a string representing whether I'm showing pretty diffs"

	^ (self showingPrettyLineDiffs
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'linePrettyDiffs'
</details>

#### CodeProvider>>#fileOutMessage

Put a description of the selected message on a file


<details>
	<summary>See more</summary>
	
	fileOutMessage
	"Put a description of the selected message on a file"

	self selectedMessageName ifNotNil: [
		self selectedClassOrMetaClass fileOutMethod: self selectedMessageName]
</details>

#### CodeProvider>>#annotation

Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver.


<details>
	<summary>See more</summary>
	
	annotation
	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."

	|  aSelector aClass |

	((aSelector _ self selectedMessageName) == nil or: [(aClass _ self selectedClassOrMetaClass) == nil])
		ifTrue: [^ ''].
	^ self annotationForSelector: aSelector ofClass: aClass
</details>

#### CodeProvider>>#showingByteCodes

Answer whether the receiver is showing bytecodes


<details>
	<summary>See more</summary>
	
	showingByteCodes
	"Answer whether the receiver is showing bytecodes"

	^ self contentsSymbol == #byteCodes
</details>

#### CodeProvider>>#showingPrettyLineDiffs

Answer whether the receiver is showing pretty diffs of source code


<details>
	<summary>See more</summary>
	
	showingPrettyLineDiffs
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyLineDiffs
</details>

#### CodeProvider>>#contentsSymbolQuints

Answer a list of quintuplets representing information on the alternative views available in the code pane first element: the contentsSymbol used second element: the selector to call when this item is chosen. third element: the selector to call to obtain the wording of the menu item. fourth element: the wording to represent this view fifth element: balloon help A hypen indicates a need for a seperator line in a menu of such choices


<details>
	<summary>See more</summary>
	
	contentsSymbolQuints
	"Answer a list of quintuplets representing information on the alternative views available in the code pane
		first element:	the contentsSymbol used
		second element:	the selector to call when this item is chosen.
		third element:	the selector to call to obtain the wording of the menu item.
		fourth element:	the wording to represent this view
		fifth element:	balloon help
	A hypen indicates a need for a seperator line in a menu of such choices"

	^ #(
(source				togglePlainSource 			showingPlainSourceString
														'source'			'the textual source code as writen')
(documentation		toggleShowDocumentation showingDocumentationString
														'documentation'	'the first comment in the method')
-
(prettyPrint			togglePrettyPrint 			prettyPrintString
														'prettyPrint'			'the method source presented in a standard text format')
-
(lineDiffs				toggleLineDiffing			showingLineDiffsString
														'lineDiffs'			'the textual source lines diffed from its prior version')
(wordDiffs			toggleWordDiffing			showingWordDiffsString
														'wordDiffs'			'the textual source words diffed from its prior version')
(prettyLineDiffs		togglePrettyLineDiffing	showingPrettyLineDiffsString
														'prettyLineDiffs'		'formatted source lines diffed from formatted prior version')
(prettyWordDiffs	togglePrettyWordDiffing	showingPrettyWordDiffsString
														'prettyWordDiffs'	'formatted source words diffed from prior version')
-
(decompile			toggleDecompile			showingDecompileString
														'decompile'			'source code decompiled from byteCodes')
(byteCodes			toggleShowingByteCodes	showingByteCodesString
														'byteCodes'		'the bytecodes that comprise the compiled method')
	)
</details>

#### CodeProvider>>#showingPlainSource

Answer whether the receiver is showing plain source


<details>
	<summary>See more</summary>
	
	showingPlainSource
	"Answer whether the receiver is showing plain source"

	^ self contentsSymbol == #source
</details>

#### CodeProvider>>#diffFromPriorSourceFor: sourceCode

If there is a prior version of source for the selected method, return a diff, else just return the source code


<details>
	<summary>See more</summary>
	
	diffFromPriorSourceFor: sourceCode 
	"If there is a prior version of source for the selected method, return a diff, else just return the source code"

	^ self priorSourceOrNil
		ifNil: [ sourceCode ]
		ifNotNil: [ :prior |
			DifferenceFinder
				displayPatchFrom: prior to: sourceCode
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [self selectedClass])]
</details>

#### CodeProvider>>#selectedMessage

Answer a copy of the source code for the selected message. This generic version is probably actually never reached, since every subclass probably reimplements and does not send to super. In time, ideally, most, or all, reimplementors would vanish and all would defer instead to a universal version right here. Everything in good time.


<details>
	<summary>See more</summary>
	
	selectedMessage
	"Answer a copy of the source code for the selected message.  This generic version is probably actually never reached, since every subclass probably reimplements and does not send to super.  In time, ideally, most, or all, reimplementors would vanish and all would defer instead to a universal version right here.  Everything in good time."

	| class selector method |

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class _ self selectedClassOrMetaClass.
	(class isNil or: [(selector _ self selectedMessageName) isNil]) ifTrue: [^ ''].
	method _ class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod _ method.

	^ (self showComment
		ifFalse: [self sourceStringPrettifiedAndDiffed]
		ifTrue:	[ self commentContents])
			copy
</details>

#### CodeProvider>>#toggleWordDiffing

Toggle whether regular-diffing should be shown in the code pane


<details>
	<summary>See more</summary>
	
	toggleWordDiffing
	"Toggle whether regular-diffing should be shown in the code pane"

	self showWordDiffs: self showingWordDiffs not
</details>

#### CodeProvider>>#instanceVariablePushedUp

<details>
	<summary>See more</summary>
	
	instanceVariablePushedUp

	self acceptedContentsChanged

</details>

#### CodeProvider>>#showComment

Answer whether the receiver should show documentation rather than, say, source code


<details>
	<summary>See more</summary>
	
	showComment
	"Answer whether the receiver should show documentation rather than, say, source code"

	^ self contentsSymbol == #documentation

</details>

#### CodeProvider>>#togglePrettyWordDiffing

Toggle whether pretty-diffing should be shown in the code pane


<details>
	<summary>See more</summary>
	
	togglePrettyWordDiffing
	"Toggle whether pretty-diffing should be shown in the code pane"

	self showPrettyWordDiffs: self showingPrettyWordDiffs not
</details>

#### CodeProvider>>#showingPrettyWordDiffs

Answer whether the receiver is showing pretty diffs of source code


<details>
	<summary>See more</summary>
	
	showingPrettyWordDiffs
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyWordDiffs

</details>

#### CodeProvider>>#selectedMessageCategoryName

Answer the name of the message category of the message of the currently selected context.


<details>
	<summary>See more</summary>
	
	selectedMessageCategoryName
	"Answer the name of the message category of the message of the currently selected context."

	^ self selectedClass organization categoryOfElement: self selectedMessageName
</details>

#### CodeProvider>>#showingDocumentation

Answer whether the receiver should show documentation rather than, say, source code


<details>
	<summary>See more</summary>
	
	showingDocumentation
	"Answer whether the receiver should show documentation rather than, say, source code"

	^ self contentsSymbol == #documentation

</details>

#### CodeProvider>>#showingPrettyPrint

Answer whether the receiver is showing pretty-print


<details>
	<summary>See more</summary>
	
	showingPrettyPrint
	"Answer whether the receiver is showing pretty-print"

	^ self contentsSymbol == #prettyPrint
</details>

#### CodeProvider>>#showingAnyKindOfPrettyDiffs

Answer whether the receiver is currently set to show any kind of pretty diffs


<details>
	<summary>See more</summary>
	
	showingAnyKindOfPrettyDiffs
	"Answer whether the receiver is currently set to show any kind of pretty diffs"

	^ #(prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol
</details>

#### CodeProvider>>#isEditingMethod

<details>
	<summary>See more</summary>
	
	isEditingMethod

	^false
</details>

#### CodeProvider>>#acceptedStringOrText

Answer the source code or documentation for the selected method


<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	"Answer the source code or documentation for the selected method"

	self showingByteCodes ifTrue: [
		^ self selectedBytecodes].

	self showingDocumentation ifTrue: [
		^ self commentContents].

	^ self selectedMessage
</details>

#### CodeProvider>>#showPrettyLineDiffs: aBoolean

Set whether I'm showing pretty diffs as indicated


<details>
	<summary>See more</summary>
	
	showPrettyLineDiffs: aBoolean
	"Set whether I'm showing pretty diffs as indicated"

	self showingPrettyLineDiffs
		ifFalse: [
			aBoolean ifTrue: [
				self contentsSymbol: #prettyLineDiffs]]
		ifTrue: [
			aBoolean ifFalse: [
				self contentsSymbol: #source]]
</details>

#### CodeProvider>>#decompiledSource

Obtain a source string by decompiling the method's code. Also return the string.


<details>
	<summary>See more</summary>
	
	decompiledSource
	"Obtain a source string by decompiling the method's code.
	Also return the string."
	| class selector method answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	"Was method deleted while in another project?"
	method _ class
		compiledMethodAt: selector
		ifAbsent: [ ^ '' ].
	currentCompiledMethod _ method.
	"decompile without temp names "
	answer _ (class decompilerClass new
		decompile: selector
		in: class
		method: method) decompileString.
	^ answer copy
</details>

#### CodeProvider>>#doItContext

Answer the context in which a text selection can be evaluated.


<details>
	<summary>See more</summary>
	
	doItContext
	"Answer the context in which a text selection can be evaluated."

	^nil
</details>

#### CodeProvider>>#changeCategory

Present a menu of the categories of messages for the current class, and let the user choose a new category for the current message


<details>
	<summary>See more</summary>
	
	changeCategory
	"Present a menu of the categories of messages for the current class, 
	and let the user choose a new category for the current message"

	self selectedClassOrMetaClass ifNotNil: [ :cls |
		self selectedMessageName ifNotNil: [ :sel |
			(self letUserReclassify: sel in: cls) ifTrue: [
				self methodCategoryChanged]]]
</details>

#### CodeProvider>>#toggleShowDocumentation

Toggle the setting of the showingDocumentation flag, unless there are unsubmitted edits that the user declines to discard


<details>
	<summary>See more</summary>
	
	toggleShowDocumentation
	"Toggle the setting of the showingDocumentation flag, unless there are unsubmitted edits that the user declines to discard"

	self showDocumentation: self showingDocumentation not
</details>

## Editor

New text editors. TextEditor provides most of the functionality that used to be in TextMorphEditor. SmalltalkEditor is has Smalltalk code specific features. SimpleEditor provides basic functionality for single line text editing. It does not handle fonts and styles, aligning and Smalltalk utilities. It handles one single line.

### Methods
#### Editor>>#help

<details>
	<summary>See more</summary>
	
	help
	^self class help
</details>

#### Editor>>#selectWord

Select delimited text or word--the result of double-clicking.


<details>
	<summary>See more</summary>
	
	selectWord
	"Select delimited text or word--the result of double-clicking."

	^self selectWordLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters 
</details>

#### Editor>>#selectionIntervalsDo: aBlock

Evaluate over all selections. For editors with a single selection, just evaluate over it. Subclasses with multi-selection redefine this method.


<details>
	<summary>See more</summary>
	
	selectionIntervalsDo: aBlock
	"Evaluate over all selections. For editors with a single selection, just evaluate over it.
	Subclasses with multi-selection redefine this method."

	aBlock value: self selectionInterval
</details>

#### Editor>>#morph

<details>
	<summary>See more</summary>
	
	morph
	^ morph
</details>

#### Editor>>#moveCursor: directionBlock forward: forward event: aKeyboardEvent

Private - Move cursor. directionBlock is a one argument Block that computes the new Position from a given one.


<details>
	<summary>See more</summary>
	
	moveCursor: directionBlock forward: forward event: aKeyboardEvent
	"Private - Move cursor.
	directionBlock is a one argument Block that computes the new Position from a given one."
	| shift indices newPosition |
	shift _ aKeyboardEvent shiftPressed.
	indices _ self
		setIndices: shift
		forward: forward.
	newPosition _ directionBlock value: (indices at: #moving).
	shift
		ifTrue: [
			self
				selectMark: (indices at: #fixed)
				point: newPosition - 1 ]
		ifFalse: [ self deselectAndPlaceCursorAt: newPosition ]
</details>

#### Editor>>#offerMenuFromEsc: aKeyboardEvent

The escape key was hit while the receiver has the keyboard focus; take action


<details>
	<summary>See more</summary>
	
	offerMenuFromEsc: aKeyboardEvent
	"The escape key was hit while the receiver has the keyboard focus; take action"

	^ aKeyboardEvent shiftPressed ifFalse: [
		morph mouseButton2Activity ]; not
</details>

#### Editor>>#wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters startingAt: pointIndex

Select delimited text or word--the result of double-clicking.


<details>
	<summary>See more</summary>
	
	wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters startingAt: pointIndex
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^1 to: 1].
	here _ pointIndex min: string size max: 2.
	initialDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			finalDelimiter _ rightDelimiters at: match]
		ifFalse: [
			initialDelimiter _ string at: here.
			match _ rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					finalDelimiter _ leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction _ -1]].
	level _ 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar _ string at: (here _ here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start _ 1.
									"go right if hit string start"
									direction _ 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start _ here + 1.
									"go right if hit non-token"
									direction _ 1]
								ifFalse: [level _ 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level _ level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		leftDelimiters size + rightDelimiters size = 0 ifFalse: [
			"If we failed to find final delimiter, then just select word."
			^self wordRangeLeftDelimiters: '' rightDelimiters: '' ].
		here _ here + direction ].
	^ direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			here + 1 to: stop]
</details>

#### Editor>>#pasteString

Paste the text's string from the shared buffer over the current selection and redisplay if necessary. Pasting a string means using destination current attributes if appropriate.


<details>
	<summary>See more</summary>
	
	pasteString
	"Paste the text's string from the shared buffer over the current selection and 
	redisplay if necessary.
	Pasting a string means using destination current attributes if appropriate."

	self replaceSelectionWith: self clipboardStringOrText asString
</details>

#### Editor>>#selectMark: mark point: point

Deselect, then select the specified characters inclusive. Be sure the selection is in view.


<details>
	<summary>See more</summary>
	
	selectMark: mark point: point
	"Deselect, then select the specified characters inclusive.
	 Be sure the selection is in view."

	(mark =  self markIndex and: [point + 1 = self pointIndex]) ifFalse: [
		self selectInvisiblyMark: mark point: point ]
</details>

#### Editor>>#enter: aKeyboardEvent

Enter / return key was pressed


<details>
	<summary>See more</summary>
	
	enter: aKeyboardEvent
	"Enter / return key was pressed"
	"Process the various Enter / Return keystrokes"
	"Not sure if this is ever called"
	
	^self returnKey: aKeyboardEvent
</details>

#### Editor>>#wordRangeUnder: aPositionInText

<details>
	<summary>See more</summary>
	
	wordRangeUnder: aPositionInText
	
	^self wordRangeLeftDelimiters: self wordLeftDelimiters rightDelimiters: self wordRightDelimiters startingAt: aPositionInText 
</details>

#### Editor>>#beginningOfNextParagraph: position

<details>
	<summary>See more</summary>
	
	beginningOfNextParagraph: position
	| s |
	s _ self privateCurrentString.
	^ (s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size ])
			+ 1
</details>

#### Editor>>#lineSelectAndEmptyCheck: returnBlock

If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.


<details>
	<summary>See more</summary>
	
	lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]
</details>

#### Editor>>#cursorTopHome: aKeyboardEvent

Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key.


<details>
	<summary>See more</summary>
	
	cursorTopHome: aKeyboardEvent
	"Put cursor at beginning of text -- invoked from cmd-H shortcut, useful for keyboards that have no home key."

	self deselectAndPlaceCursorAt: 1.
	^ true
</details>

#### Editor>>#normalCharacter: aKeyboardEvent

A nonspecial character is to be added to the stream of characters.


<details>
	<summary>See more</summary>
	
	normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self addString: aKeyboardEvent keyCharacter asString.
	^false
</details>

#### Editor>>#selectInterval: anInterval

Deselect, then select the specified characters inclusive. Be sure the selection is in view.


<details>
	<summary>See more</summary>
	
	selectInterval: anInterval
	"Deselect, then select the specified characters inclusive.
	 Be sure the selection is in view."

	self selectFrom: anInterval first to: anInterval last
</details>

#### Editor>>#beginningOfParagraph: position

<details>
	<summary>See more</summary>
	
	beginningOfParagraph: position
	| s |
	s _ self privateCurrentString.
	^ (s
		lastIndexOf: Character newLineCharacter
		startingAt: (position-1 min: s size)
		ifAbsent: [ 0 ])
			+ 1.
</details>

#### Editor>>#cursorPageUp: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	cursorPageUp: aKeyboardEvent 

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo - self pageHeight]
				forward: false]
		forward: false
		event: aKeyboardEvent.
	^true
</details>

#### Editor>>#paste: aKeyboardEvent

Replace the current text selection by the text in the shared buffer.


<details>
	<summary>See more</summary>
	
	paste: aKeyboardEvent 
	"Replace the current text selection by the text in the shared buffer."

	self paste.
	^true
</details>

#### Editor>>#cursorUp: aKeyboardEvent

Private - Move cursor from position in current line to same position in prior line. If prior line too short, put at end


<details>
	<summary>See more</summary>
	
	cursorUp: aKeyboardEvent 
	"Private - Move cursor from position in current line to same position in
	prior line. If prior line too short, put at end"

	self
		moveCursor: [ :position | | newPos |
			newPos _ self sameColumn: position newLine: [ :line | line - 1] forward: false.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [
						newPos _ self beginningOfParagraph: newPos-1 ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self beginningOfText ].
			newPos ]
		forward: false
		event: aKeyboardEvent.
	^true
</details>

#### Editor>>#userHasEdited

Note that my text is not free of user edits.


<details>
	<summary>See more</summary>
	
	userHasEdited
	"Note that my text is not free of user edits."

	morph hasUnacceptedEdits: true
</details>

#### Editor>>#endOfParagraph: position

<details>
	<summary>See more</summary>
	
	endOfParagraph: position
	| s |
	s _ self privateCurrentString.
	^ s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size + 1 ]
</details>

#### Editor>>#selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters

Select delimited text or word--the result of double-clicking.


<details>
	<summary>See more</summary>
	
	selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| wordRange |
	
	wordRange := self wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters.
	
	self selectFrom: wordRange first to: wordRange last
</details>

#### Editor>>#beginningOfText

<details>
	<summary>See more</summary>
	
	beginningOfText
	^1
</details>

#### Editor>>#selectAll: aKeyboardEvent

select everything, invoked by cmd-a. 1/17/96 sw


<details>
	<summary>See more</summary>
	
	selectAll: aKeyboardEvent 
	"select everything, invoked by cmd-a.  1/17/96 sw"

	self selectAll.
	^ true
</details>

#### Editor>>#paste

Paste the text from the shared buffer over the current selection and redisplay if necessary.


<details>
	<summary>See more</summary>
	
	paste
	"Paste the text from the shared buffer over the current selection and 
	redisplay if necessary."

	self replaceSelectionWith: self clipboardStringOrText
</details>

#### Editor>>#sameColumn: start newLine: lineBlock forward: isForward

See comment in other implementors.


<details>
	<summary>See more</summary>
	
	sameColumn: start newLine: lineBlock forward: isForward
	"See comment in other implementors."
	self subclassResponsibility
</details>

#### Editor>>#selectionInterval

Answer the interval that is currently selected.


<details>
	<summary>See more</summary>
	
	selectionInterval
	"Answer the interval that is currently selected."

	^self startIndex to: self stopIndex - 1 
</details>

#### Editor>>#endOfText

<details>
	<summary>See more</summary>
	
	endOfText
	^self privateCurrentString size + 1
</details>

#### Editor>>#nextWordStartFrom: aPosition goingForwards: goingForwardsBoolean

Answer the position of the start of the next word on the current line going forwards (or backwards). If the given position is the end (or beginning) of the line then answer the beginning (or end) of the next (or previous) line.


<details>
	<summary>See more</summary>
	
	nextWordStartFrom: aPosition goingForwards: goingForwardsBoolean
	
	"Answer the position of the start of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string _ self privateCurrentString.
	beginningOfLine _ self beginningOfLine: aPosition.
	endOfLine _ self endOfLine: aPosition.
	step _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset _ goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index _ aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index _ index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index _ index + step].
	
	newPosition _ index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1
</details>

#### Editor>>#cut: aKeyboardEvent

Cut out the current text selection.


<details>
	<summary>See more</summary>
	
	cut: aKeyboardEvent 
	"Cut out the current text selection."

	self cut.
	^true
</details>

#### Editor>>#cmdShortcuts

Same for all instances. A subclass could handle specific keyboard shortcuts for each instance, though.


<details>
	<summary>See more</summary>
	
	cmdShortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^self class cmdShortcuts
</details>

#### Editor>>#beginningOfLine: position

Redefined in subclasses using TextComposition


<details>
	<summary>See more</summary>
	
	beginningOfLine: position
	"Redefined in subclasses using TextComposition"
	^ self beginningOfParagraph: position
</details>

#### Editor>>#cursorLeft: aKeyboardEvent

Private - Move cursor left one character if nothing selected, otherwise move cursor to beginning of selection. If the shift key is down, start selecting or extending current selection. Don't allow cursor past beginning of text


<details>
	<summary>See more</summary>
	
	cursorLeft: aKeyboardEvent
	"Private - Move cursor left one character if nothing selected, otherwise move cursor to beginning of selection. If the shift key is down, start selecting or extending current selection.
	Don't allow cursor past beginning of text"

	self
		moveCursor: [ :position | | newPos |
			newPos _ position - 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos _ self previousWordStart: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self beginningOfLine: position ].
			newPos ]
		forward: false
		event: aKeyboardEvent.
	^ true
</details>

#### Editor>>#selectInvisiblyMark: mark point: point

Select the designated characters, inclusive. Make no visual changes.


<details>
	<summary>See more</summary>
	
	selectInvisiblyMark: mark point: point
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: mark pointIndex: point + 1
</details>

#### Editor>>#clipboardTextPut: textOrString

<details>
	<summary>See more</summary>
	
	clipboardTextPut: textOrString

	^ Clipboard storeObject: textOrString
</details>

#### Editor>>#wordRightDelimiters

<details>
	<summary>See more</summary>
	
	wordRightDelimiters

	^''
</details>

#### Editor>>#selectWord: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	selectWord: aKeyboardEvent

	self selectWord.
	^ true
</details>

#### Editor>>#copySelection: aKeyboardEvent

Copy the current text selection.


<details>
	<summary>See more</summary>
	
	copySelection: aKeyboardEvent
	"Copy the current text selection."

	self copySelection.
	^true
</details>

#### Editor>>#cursorPageDown: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	cursorPageDown: aKeyboardEvent 

	self 
		moveCursor: [ :position |
			self
				sameColumn: position
				newLine: [ :lineNo | lineNo + self pageHeight]
				forward: true]
		forward: true
		event: aKeyboardEvent.
	^true
</details>

#### Editor>>#newLine: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	newLine: aKeyboardEvent

	self addString: String newLineString.
	^false
</details>

#### Editor>>#setIndices: shiftPressed forward: forward

Little helper method that sets the moving and fixed indices according to some flags.


<details>
	<summary>See more</summary>
	
	setIndices: shiftPressed forward: forward
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices _ Dictionary new.
	(shiftPressed and:[Preferences selectionsMayShrink])
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices
</details>

#### Editor>>#wordSelectAndEmptyCheck: returnBlock

Ensure selecting the entire current word; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.


<details>
	<summary>See more</summary>
	
	wordSelectAndEmptyCheck: returnBlock
	"Ensure selecting the entire current word; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectWord.  "Select exactly a whole word"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]
</details>

#### Editor>>#wordLeftDelimiters

<details>
	<summary>See more</summary>
	
	wordLeftDelimiters

	^''
</details>

#### Editor>>#selectAll

<details>
	<summary>See more</summary>
	
	selectAll

	self selectFrom: 1 to: self privateCurrentString size
</details>

#### Editor>>#wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters

Select delimited text or word--the result of double-clicking.


<details>
	<summary>See more</summary>
	
	wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	^self wordRangeLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters  startingAt: self pointIndex 
</details>

#### Editor>>#deselectAndPlaceCursorAt: characterIndex

Deselect, then place the text cursor before the character at characterIndex. Be sure it is in view.


<details>
	<summary>See more</summary>
	
	deselectAndPlaceCursorAt: characterIndex 
	"Deselect, then place the text cursor before the character at characterIndex.
	 Be sure it is in view."

	self selectFrom: characterIndex to: characterIndex - 1
</details>

#### Editor>>#selectInvisiblyFrom: start to: stop

Select the designated characters, inclusive. Make no visual changes.


<details>
	<summary>See more</summary>
	
	selectInvisiblyFrom: start to: stop
	"Select the designated characters, inclusive.  Make no visual changes."

	self markIndex: start pointIndex: stop + 1
</details>

#### Editor>>#previousWordStart: aPosition

<details>
	<summary>See more</summary>
	
	previousWordStart: aPosition
	
	^self nextWordEndFrom: aPosition goingForwards: false
</details>

#### Editor>>#nextWordEnd: aPosition

<details>
	<summary>See more</summary>
	
	nextWordEnd: aPosition
	
	^self nextWordEndFrom: aPosition goingForwards: true
</details>

#### Editor>>#cursorDown: aKeyboardEvent

Private - Move cursor from position in current line to same position in next line. If next line too short, put at end. If shift key down, select.


<details>
	<summary>See more</summary>
	
	cursorDown: aKeyboardEvent
	"Private - Move cursor from position in current line to same position in
	next line. If next line too short, put at end. If shift key down,
	select."

	self
		moveCursor: [ :position | | newPos |
			newPos _ self sameColumn: position newLine: [ :line | line + 1 ] forward: true.
			"Mac standard keystroke"
			aKeyboardEvent rawMacOptionKeyPressed ifTrue: [
				newPos _ self endOfParagraph: position+1 ].
			"Windows / Linux standard keystroke"
			aKeyboardEvent controlKeyPressed ifTrue: [
				newPos _ self beginningOfNextParagraph: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self endOfText ].
			newPos ]
		forward: true
		event: aKeyboardEvent.
	^ true
</details>

#### Editor>>#backspace: aKeyboardEvent

Backspace over the last character.


<details>
	<summary>See more</summary>
	
	backspace: aKeyboardEvent
	"Backspace over the last character."
	"This is a user command, and generates undo"

	| startIndex |
	(aKeyboardEvent rawMacOptionKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [ ^ self backWord: aKeyboardEvent ].
	aKeyboardEvent shiftPressed
		ifTrue: [ ^ self forwardDelete: aKeyboardEvent ].
	startIndex _ self markIndex + (self hasSelection ifTrue: [1] ifFalse: [0]).
	startIndex _ 1 max: startIndex - 1.
	self backTo: startIndex.
	^ false
</details>

#### Editor>>#cursorRight: aKeyboardEvent

Private - Move cursor right one character if nothing selected, otherwise move cursor to end of selection. If the shift key is down, start selecting characters or extending already selected characters. Don't allow cursor past end of text


<details>
	<summary>See more</summary>
	
	cursorRight: aKeyboardEvent 
	"Private - Move cursor right one character if nothing selected, otherwise move cursor to end of selection. If the shift key is down, start selecting characters or extending already selected characters. 
	Don't allow cursor past end of text"

	self
		moveCursor: [ :position | | newPos |
			newPos _ position + 1.
			"Mac standard keystroke"
			(aKeyboardEvent rawMacOptionKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ newPos _ self nextWordEnd: position ].
			"Mac standard keystroke"
			aKeyboardEvent commandAltKeyPressed ifTrue: [
				newPos _ self endOfLine: position ].
			newPos ]
		forward: true
		event: aKeyboardEvent.
	^ true
</details>

#### Editor>>#clipboardStringOrText

<details>
	<summary>See more</summary>
	
	clipboardStringOrText

	| clipContents |
	clipContents _ Clipboard retrieveStringOrText.
	^ ((clipContents is: #Text) and: [ clipContents respondsTo: #asNonStyledText ])
		ifTrue: [ clipContents asNonStyledText ]
		ifFalse: [ clipContents ]
</details>

#### Editor>>#nextWordStart: aPosition

<details>
	<summary>See more</summary>
	
	nextWordStart: aPosition
	
	^self nextWordStartFrom: aPosition goingForwards: true
</details>

#### Editor>>#noop: aKeyboardEvent

Unimplemented keyboard command; just ignore it.


<details>
	<summary>See more</summary>
	
	noop: aKeyboardEvent 
	"Unimplemented keyboard command; just ignore it."

	^ true
</details>

#### Editor>>#nextWordEndFrom: aPosition goingForwards: goingForwardsBoolean

Answer the position of the end of the next word on the current line going forwards (or backwards). If the given position is the end (or beginning) of the line then answer the beginning (or end) of the next (or previous) line.


<details>
	<summary>See more</summary>
	
	nextWordEndFrom: aPosition goingForwards: goingForwardsBoolean
	
	"Answer the position of the end of the next word on the current line going
	forwards (or backwards).
	If the given position is the end (or beginning) of the line then answer the
	beginning (or end) of the next (or previous) line."
	
	| string beginningOfLine endOfLine step offset index newPosition |
	
	string _ self privateCurrentString.
	beginningOfLine _ self beginningOfLine: aPosition.
	endOfLine _ self endOfLine: aPosition.
	step _ goingForwardsBoolean ifTrue: [1] ifFalse: [-1].
	offset _ goingForwardsBoolean ifTrue: [0] ifFalse: [-1].
	
	index _ aPosition.
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers not]]
			whileTrue: [index _ index + step].
	[(index + step between: beginningOfLine and: endOfLine)
		and: [(string at: index + offset) isValidInIdentifiers]]
			whileTrue: [index _ index + step].
	
	newPosition _ index = aPosition ifTrue: [index + step] ifFalse: [index].
	^newPosition min: string size + 1 max: 1
</details>

#### Editor>>#currentAttributes

Redefined by subclasses that handle TextAttributes


<details>
	<summary>See more</summary>
	
	currentAttributes
	"Redefined by subclasses that handle TextAttributes"
	^nil
</details>

#### Editor>>#shortcuts

Same for all instances. A subclass could handle specific keyboard shortcuts for each instance, though.


<details>
	<summary>See more</summary>
	
	shortcuts
	"Same for all instances.
	A subclass could handle specific keyboard shortcuts for each instance, though."
	^self class shortcuts
</details>

#### Editor>>#endOfLine: position

Redefined in subclasses using TextComposition


<details>
	<summary>See more</summary>
	
	endOfLine: position
	"Redefined in subclasses using TextComposition"
	^self endOfParagraph: position
</details>

#### Editor>>#returnKey: aKeyboardEvent

Return / Enter / key was pressed


<details>
	<summary>See more</summary>
	
	returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: String crString.
			^false ].
		self addString: String crlfString.
		^false ].
	^ self newLine: aKeyboardEvent
</details>

#### Editor>>#morph: aMorph

Install a link back to the morph being edited (esp for text links)


<details>
	<summary>See more</summary>
	
	morph: aMorph
	"Install a link back to the morph being edited (esp for text links)"
	morph _ aMorph 
</details>

## PluggableTextModel

A TextModel whose contents are stored elsewhere (ivar textProvider)

### Methods
#### PluggableTextModel>>#textProvider

<details>
	<summary>See more</summary>
	
	textProvider
	^textProvider
</details>

#### PluggableTextModel>>#acceptContentsFrom: aTextModelMorph

Nothing to do here. Anyway, maybe this implementor should be removed...


<details>
	<summary>See more</summary>
	
	acceptContentsFrom: aTextModelMorph
	textSetter ifNil: [ ^ true ].
	^ textSetter numArgs = 2
		ifTrue: [
			textProvider
				perform: textSetter
				with: actualContents
				with: aTextModelMorph ]
		ifFalse: [
			textProvider
				perform: textSetter
				with: actualContents ].
</details>

#### PluggableTextModel>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName 
</details>

#### PluggableTextModel>>#getSelection

Answer the model's selection interval.


<details>
	<summary>See more</summary>
	
	getSelection
	"Answer the model's selection interval."

	^selectionGetter ifNotNil: [
		textProvider perform: selectionGetter ]
</details>

#### PluggableTextModel>>#computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName 
</details>

#### PluggableTextModel>>#autoCompleterClass

<details>
	<summary>See more</summary>
	
	autoCompleterClass
	^textProvider autoCompleterClassFor: textGetter
</details>

#### PluggableTextModel>>#update: aSymbol

We are being notified of a change in our provider. Notify our dependents


<details>
	<summary>See more</summary>
	
	update: aSymbol
	"We are being notified of a change in our provider.
	Notify our dependents"
	"
	self changed: aSymbol
	"
	aSymbol == textGetter ifTrue: [ ^self changed: #acceptedContents ].
	aSymbol == selectionGetter ifTrue: [ ^self changed: #initialSelection ].
	"do not try to autoselect on other than the main contents pane.
	I.e., in a 'senders' view, do not flash the annotations for not including the sent symbol"
	(aSymbol == #autoSelect and: [ textGetter ~~ #acceptedContents ])
		ifTrue: [ ^self ].
	self changed: aSymbol
</details>

#### PluggableTextModel>>#initWith: aTextProvider

aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream.


<details>
	<summary>See more</summary>
	
	initWith: aTextProvider
	"aTextProvider can be a kind of TextProvider, or perhaps a more exotic object, like an ObjectExplorer or a TranscriptStream."

	textProvider _ aTextProvider.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil
</details>

#### PluggableTextModel>>#textStylerClass

<details>
	<summary>See more</summary>
	
	textStylerClass
	^textProvider textStylerClassFor: textGetter
</details>

#### PluggableTextModel>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
</details>

#### PluggableTextModel>>#editorClass

<details>
	<summary>See more</summary>
	
	editorClass
	^textProvider editorClassFor: textGetter
</details>

#### PluggableTextModel>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #hasTextProvider or: [ super is: aSymbol ]
</details>

#### PluggableTextModel>>#refetch

Answer true if actualContents was actually fetched.


<details>
	<summary>See more</summary>
	
	refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ].
			^false ]
		ifNotNil: [
			self actualContents: (Text
				initialFont: Preferences standardCodeFont
				stringOrText: (textProvider perform: textGetter)).
			self changed: #refetched.
			^true ]
</details>

#### PluggableTextModel>>#autoSelectString

<details>
	<summary>See more</summary>
	
	autoSelectString
	^textProvider autoSelectString
</details>

#### PluggableTextModel>>#textGetter: symbol1 textSetter: symbol2 selectionGetter: symbol3

<details>
	<summary>See more</summary>
	
	textGetter: symbol1 textSetter: symbol2 selectionGetter: symbol3
	textGetter _ symbol1.
	textSetter _ symbol2.
	selectionGetter _ symbol3
</details>

#### PluggableTextModel>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	^textProvider shouldStyle: text with: anSHTextStyler
</details>

#### PluggableTextModel>>#classOfThisContext

<details>
	<summary>See more</summary>
	
	classOfThisContext

	^ textProvider classOfThisContext 
</details>

#### PluggableTextModel>>#wantsFrameAdornments

<details>
	<summary>See more</summary>
	
	wantsFrameAdornments

	^textSetter notNil
</details>

#### PluggableTextModel>>#classOfWorkspaceVarNamed: aName

<details>
	<summary>See more</summary>
	
	classOfWorkspaceVarNamed: aName

	^ textProvider classOfWorkspaceVarNamed: aName 
</details>

#### PluggableTextModel>>#actualContents

<details>
	<summary>See more</summary>
	
	actualContents
	actualContents ifNil: [ self refetch ].
	^actualContents
</details>

#### PluggableTextModel>>#computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	textProvider computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName 
</details>

#### PluggableTextModel>>#refusesToAccept

<details>
	<summary>See more</summary>
	
	refusesToAccept
	^textProvider is: #refusingToAccept
</details>

## SmalltalkEditor

The editor built specifically for Smalltalk code

### Methods
#### SmalltalkEditor>>#changeSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	changeSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

	aChangeSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector
</details>

#### SmalltalkEditor>>#refactoringMenu

<details>
	<summary>See more</summary>
	
	refactoringMenu

	^DynamicMenuBuilder
		buildTitled: 'More refactorings'
		targeting: self
		collectingMenuOptionsWith: #smalltalkEditorRefactoringMenuOptions.
</details>

#### SmalltalkEditor>>#argNext: aKeyboardEvent

Invoked by cmd-A. Search forward from the end of the selection for a colon followed by a space. Place the text cursor after the space. If none are found, place the text cursor at the end of the text..


<details>
	<summary>See more</summary>
	
	argNext: aKeyboardEvent
	"Invoked by cmd-A.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the end of the text.."

	| start t |
	t _ model actualContents.
	start _ t findString: ': ' startingAt: self startIndex.
	start = 0 ifTrue: [ start _ t size + 1].
	self deselectAndPlaceCursorAt: start + 2.
	^true
</details>

#### SmalltalkEditor>>#selectForInspection: aNodeUnderCursor in: aMethodNode

<details>
	<summary>See more</summary>
	
	selectForInspection: aNodeUnderCursor in: aMethodNode 
	
	(aNodeUnderCursor isLiteralNode or: [ aNodeUnderCursor isVariableNode ]) ifTrue: [ ^self selectNodeRange: aNodeUnderCursor in: aMethodNode ].
	aNodeUnderCursor isMessageNode ifTrue: [ ^self selectMessageNode: aNodeUnderCursor in: aMethodNode ].
</details>

#### SmalltalkEditor>>#contextualChangeSelectorInMethodUsing: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	contextualChangeSelectorInMethodUsing: aChangeSelectorApplier
	
	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualChangeSelectorOf: methodNode in: selectedClass using: aChangeSelectorApplier ]
		ifErrorsParsing: [ :anError | morph flash ]		
</details>

#### SmalltalkEditor>>#hasValidCurrentCompiledMethod

<details>
	<summary>See more</summary>
	
	hasValidCurrentCompiledMethod

	^ (self codeProvider respondsTo: #currentCompiledMethod)
		and: [ self codeProvider currentCompiledMethod notNil ]
</details>

#### SmalltalkEditor>>#compileSelectionFor: anObject in: evalContext ifFail: failBlock

<details>
	<summary>See more</summary>
	
	compileSelectionFor: anObject in: evalContext ifFail: failBlock

	| methodNode method sourceCode compiler |

	sourceCode := self selectionDoItSourceCodeIn: evalContext.
	
	methodNode _ [
		compiler _ Compiler new.
		compiler		
			compileDoIt: sourceCode
			in: anObject class
			context: evalContext
			notifying: self
			ifFail: [ ^ failBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method _ methodNode generate.
	method methodNode: methodNode.

	^{ #method -> method. #compiler -> compiler } asDictionary 

</details>

#### SmalltalkEditor>>#implementorsOfItWhenErrorsParsing

Open an implementors browser on the selected selector


<details>
	<summary>See more</summary>
	
	implementorsOfItWhenErrorsParsing

	"Open an implementors browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector _ self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllImplementorsOf: aSelector
</details>

#### SmalltalkEditor>>#extractMethod

To prevent the extract method to be evaluated on editors w/o methods like the workspace


<details>
	<summary>See more</summary>
	
	extractMethod

	"To prevent the extract method to be evaluated on editors w/o methods like the workspace"
	self hasValidCurrentCompiledMethod ifFalse: [ ^ nil ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractMethodApplier createAndValueHandlingExceptions: [
			ExtractMethodApplier for: self selectionInterval of: self codeProvider currentCompiledMethod ] ]
</details>

#### SmalltalkEditor>>#acceptAndTestAll: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	acceptAndTestAll: aKeyboardEvent

	^self acceptAndTestAll
</details>

#### SmalltalkEditor>>#changeSelectorTo: aSelector in: aClassToRefactor using: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	changeSelectorTo: aSelector in: aClassToRefactor using: aChangeSelectorApplier
	
	aChangeSelectorApplier 
		createAndValueHandlingExceptionsOn: model textProvider 
		for: aSelector 
		in: aClassToRefactor 
</details>

#### SmalltalkEditor>>#argPrev: aKeyboardEvent

Invoked by cmd-Q. Search backwards from the start of the selection for a colon followed by a space. Place the text cursor after the space. If none are found, place the text cursor at the start of the text..


<details>
	<summary>See more</summary>
	
	argPrev: aKeyboardEvent
	"Invoked by cmd-Q.
	 Search backwards from the start of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the start of the text.."

	| t i |
	t _ model actualContents.
	i _ self stopIndex.
	i > 1 ifTrue: [
		i _ i -2.
		[i > 0 and: [ (t at: i) ~= $  or: [(t at: i-1) ~= $: ]]] whileTrue: [
			i _ i -1 ].
		self deselectAndPlaceCursorAt: i + 1.
	].
	^true
</details>

#### SmalltalkEditor>>#exploreIt

<details>
	<summary>See more</summary>
	
	exploreIt

	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ morph flash ]
		profiled: false
</details>

#### SmalltalkEditor>>#explainCtxt: symbol

Is symbol a context variable?


<details>
	<summary>See more</summary>
	
	explainCtxt: symbol 
	"Is symbol a context variable?"

	| reply classes text cls provider |
	symbol = #nil ifTrue: [reply _ ' is a constant.  It is the only instance of class UndefinedObject.  nil is the initial value of all variables.'].
	symbol = #true ifTrue: [reply _ ' is a constant.  It is the only instance of class True and is the receiver of many control messages.'].
	symbol = #false ifTrue: [reply _ ' is a constant.  It is the only instance of class False and is the receiver of many control messages.'].
	symbol = #thisContext ifTrue: [reply _ ' is a context variable.  Its value is always the MethodContext which is executing this method.'].
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ reply].	  "no class known"
	symbol = #self ifTrue: 
			[classes _ cls withAllSubclasses.
			classes size > 12
				ifTrue: [text _ cls printString , ' or a subclass']
				ifFalse: 
					[classes _ classes printString.
					text _ 'one of these classes' , (classes copyFrom: 4 to: classes size)].
			reply _ ' is the receiver of this message; an instance of ' , text ].
	symbol = #super ifTrue: [reply _ 'is just like self.  Messages to super are looked up in the superclass (' , cls superclass printString , ')'].
	^reply ifNotNil: [symbol, reply]
</details>

#### SmalltalkEditor>>#sendersOfItWhenErrorsParsing

Open a senders browser on the selected selector


<details>
	<summary>See more</summary>
	
	sendersOfItWhenErrorsParsing

	"Open a senders browser on the selected selector"

	| aSelector |

	self lineSelectAndEmptyCheck: [^ self].
	(aSelector _ self selectedSelector) ifNil: [^ morph flash].
	Smalltalk browseAllCallsOn: aSelector
</details>

#### SmalltalkEditor>>#selectPrecedingIdentifier

Invisibly select the identifier that ends at the end of the selection, if any.


<details>
	<summary>See more</summary>
	
	selectPrecedingIdentifier
	"Invisibly select the identifier that ends at the end of the selection, if any."

	| string sep stop tok |
	tok _ false.
	string _ self privateCurrentString.
	stop _ self stopIndex - 1.
	[stop > 0 and: [(string at: stop) isSeparator]] whileTrue: [stop _ stop - 1].
	sep _ stop.
	[sep > 0 and: [(string at: sep) tokenish]] whileTrue: [tok _ true. sep _ sep - 1].
	tok ifTrue: [self selectInvisiblyFrom: sep + 1 to: stop]
</details>

#### SmalltalkEditor>>#explainChar: string

Does string start with a special character?


<details>
	<summary>See more</summary>
	
	explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float).'].
	char = $' ifTrue: [^'The characters between two single quotes are made into an instance of class String'].
	char = $" ifTrue: [^'Double quotes enclose a comment.  Smalltalk ignores everything between double quotes.'].
	char = $# ifTrue: [^'The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants.'].
	(char = $( or: [char = $)]) ifTrue: [^'Expressions enclosed in parenthesis are evaluated first'].
	(char = $[ or: [char = $]]) ifTrue: [^'The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockClosure and is usually passed as an argument.'].
	(char = ${ or: [char = $}]) ifTrue: [^ 'A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array'].
	(char = $< or: [char = $>]) ifTrue: [^'<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails.'].
	char = $^ ifTrue: [^'Up arrow means return from this method.  The value returned is the expression following the ^'].
	char = $| ifTrue: [^'Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code.'].
	char = $_ ifTrue: [^'Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it.'].
	char = $; ifTrue: [^'Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon.'].
	char = $: ifTrue: [^'A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon).'].
	char = $$ ifTrue: [^'The single character following a dollar sign is made into an instance of class Character'].
	char = $- ifTrue: [^'A minus sign in front of a number means a negative number.'].
	char = $e ifTrue: [^'An e in the middle of a number means that the exponent follows.'].
	char = $r ifTrue: [^'An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base.'].
	char = Character space ifTrue: [^'the space Character'].
	char = Character tab ifTrue: [^'the tab Character'].
	char = Character cr ifTrue: [^'the carriage return Character'].
	char = Character lf ifTrue: [^'newline. The line feed Character'].
	^nil
</details>

#### SmalltalkEditor>>#temporaryToInstanceVariable: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	temporaryToInstanceVariable: aKeyboardEvent

	self temporaryToInstanceVariable.

	^true.
</details>

#### SmalltalkEditor>>#explainInst: string

Is string an instance variable of this class?


<details>
	<summary>See more</summary>
	
	explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ nil].	  "no class known"
	classes _ (Array with: cls)
				, cls allSuperclasses.
	classes _ classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes _ classes printString.
	^ String streamContents: [:str |
		str
			nextPutAll: string;
			nextPutAll: ' is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]
</details>

#### SmalltalkEditor>>#renameGlobalOn: aBrowser for: anOldName

<details>
	<summary>See more</summary>
	
	renameGlobalOn: aBrowser for: anOldName

	(RenameGlobalApplier on: aBrowser for: anOldName) value
</details>

#### SmalltalkEditor>>#selectionDoItSourceCodeIn: evalContext

<details>
	<summary>See more</summary>
	
	selectionDoItSourceCodeIn: evalContext

	^String streamContents: [ :stream |
		Scanner selectionDoItSourceCodeHeaderWithContext: evalContext notNil into: stream.
		stream nextPutAll: self selectionAsStream upToEnd ]
</details>

#### SmalltalkEditor>>#selectedSymbol

Return the currently selected symbol, or nil if none. Spaces, tabs and returns are ignored


<details>
	<summary>See more</summary>
	
	selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasSelection ifFalse: [^ nil].
	aString _ self selectedString withoutSeparators.
	aString size = 0 ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil
</details>

#### SmalltalkEditor>>#paste

Paste the text from the shared buffer over the current selection and redisplay if necessary.


<details>
	<summary>See more</summary>
	
	paste
	| objectName |
	
	model canBindVariables ifTrue: [
		"Not a copy!!!"
		Clipboard contentsOriginalObject ifNotNil: [ :object |
			objectName _ model nameForObject: object.
			(model bindingOf: objectName) value: object.
			self replaceSelectionWith: objectName.
			^ self ]].
	^ super paste
</details>

#### SmalltalkEditor>>#temporaryToInstanceVariable

<details>
	<summary>See more</summary>
	
	temporaryToInstanceVariable

	self
		withNodeUnderCursorDo: [ :nodeUnderCursor |
			(nodeUnderCursor isTemp and: [nodeUnderCursor isArg not])
				ifTrue: [ TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name :: value ]
				ifFalse: [ morph flash ]]
		ifAbsent: [ morph flash ].
</details>

#### SmalltalkEditor>>#testSuiteForCategoryOf: aClass

<details>
	<summary>See more</summary>
	
	testSuiteForCategoryOf: aClass

	^TestSuite forSystemCategoryNamed: aClass category using: SystemOrganization 

</details>

#### SmalltalkEditor>>#implementorsOfIt

Open an implementors browser. If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line.


<details>
	<summary>See more</summary>
	
	implementorsOfIt
	"Open an implementors browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllImplementorsOf: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllImplementorsOf: selector ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ]
</details>

#### SmalltalkEditor>>#selection

Answer the text that is currently selected. Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string, answer that string.


<details>
	<summary>See more</summary>
	
	selection
	"Answer the text that is currently selected.
	Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string,
	answer that string."
	| t regularSelection allPartsEqual samePart firstIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.

	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].

	regularSelection _ ( t copyFrom: firstIndex to: lastIndex).
	allPartsEqual _ true.
	samePart _ nil.
	^Text streamContents: [ :strm |
		"Multiple selection"
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |
			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.
			toAppend size > 0 ifTrue: [
				samePart
					ifNil: [ samePart _ toAppend ]
					ifNotNil: [
						allPartsEqual _ allPartsEqual and: [ samePart = toAppend ]].
				strm nextPutAll: toAppend.
				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]].
			].
		(allPartsEqual and: [ regularSelection isEmpty ]) ifTrue: [
			^samePart ifNil: [ '' asText ]].
		"Regular selection"
		strm nextPutAll: regularSelection ]
</details>

#### SmalltalkEditor>>#explainGlobal: symbol

Is symbol a global variable?


<details>
	<summary>See more</summary>
	
	explainGlobal: symbol 
	"Is symbol a global variable?"
	| reply classes |
	reply _ Smalltalk at: symbol ifAbsent: [^nil].
	(reply class == Dictionary or:[reply isKindOf: SharedPool class])
		ifTrue: 
			[classes _ Set new.
			Smalltalk allBehaviorsDo: [:each | (each sharedPools detect: [:pool | pool == reply]
					ifNone: nil)
					ifNotNil: [classes add: each]].
			classes _ classes printString.
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a global variable.  It is a pool which is used by the following classes ';
					nextPutAll: (classes allButFirst: 5) ]].
	(reply isKindOf: Behavior)
		ifTrue: [^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a global variable.  ';
				nextPutAll: symbol;
				nextPutAll: ' is a class in category ';
				nextPutAll: reply category, '.', '\' withNewLines;
				nextPutAll: 'BrowserWindow fullOnClass: ';
				nextPutAll: symbol;
				nextPutAll: ' selector: nil';
				nextPut: $.]].
	symbol == #Smalltalk 
		ifTrue: [^ symbol, ' is a global.  Smalltalk is the only instance of SystemDictionary and holds all global variables.'].
	^ String streamContents: [:str |
		str
			nextPutAll: symbol;
			nextPutAll: ' is a global variable.  ';
			nextPutAll: symbol;
			nextPutAll: ' is ';
			nextPutAll: reply printString ]
</details>

#### SmalltalkEditor>>#contextualPushDownInClassDefinition

<details>
	<summary>See more</summary>
	
	contextualPushDownInClassDefinition 

	self inClassDefinitionContextuallyApply: [ :aSelectedClass | 
		(PushDownInstanceVariableApplier 
			on: self codeProvider 
			for: self wordUnderCursor 
			at: aSelectedClass ) value ]
</details>

#### SmalltalkEditor>>#renameTemporary: aTemporaryNode at: aMethodNode

<details>
	<summary>See more</summary>
	
	renameTemporary: aTemporaryNode at: aMethodNode

	self codeProvider isEditingMethod ifTrue: [ | applier |
		[ applier := RenameTemporaryApplier on: self for: aTemporaryNode at: aMethodNode ]
			on: SyntaxErrorNotification
			do: [:anError | ^self inform: (RenameTemporaryApplier errorMessageForCanNotParseMethod: anError) ].
		applier value ].

</details>

#### SmalltalkEditor>>#codeProvider

<details>
	<summary>See more</summary>
	
	codeProvider
	^ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ]
</details>

#### SmalltalkEditor>>#acceptAndDebugTest

<details>
	<summary>See more</summary>
	
	acceptAndDebugTest
	
	^self acceptAndWithMethodDo: [ :aPotencialTestMethod | 
		aPotencialTestMethod isTestMethod ifTrue: [ 
			aPotencialTestMethod methodClass debugAsFailure: aPotencialTestMethod selector ifCanNot: [ PopUpMenu inform: TestCase canNotDebugMethodErrorDescription ]]]
</details>

#### SmalltalkEditor>>#selectNodeRange: aNodeUnderCursor in: aMethodNode

<details>
	<summary>See more</summary>
	
	selectNodeRange: aNodeUnderCursor in: aMethodNode 
	
	| range ranges |
	
	ranges := aMethodNode rangeForNode: aNodeUnderCursor ifAbsent: [ ^ self ].
	range := (aMethodNode isMultipleRanges: ranges) 
		ifTrue: [ ranges detect: [ :aRange | aRange includes: self startIndex ] ifNone: [ ^self ]]
		ifFalse: [ ranges ].
		
	self selectFrom: range first to: range last

</details>

#### SmalltalkEditor>>#extractToTemporary

<details>
	<summary>See more</summary>
	
	extractToTemporary

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		ExtractToTemporaryApplier createAndValueHandlingExceptions: [
			ExtractToTemporaryApplier for: self selectionInterval of: self codeProvider currentCompiledMethod ] ]
</details>

#### SmalltalkEditor>>#classCommentsContainingIt

Open a browser class comments which contain the current selection somewhere in them.


<details>
	<summary>See more</summary>
	
	classCommentsContainingIt
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseClassCommentsWithString: self selectedString
</details>

#### SmalltalkEditor>>#wordRightDelimiters

<details>
	<summary>See more</summary>
	
	wordRightDelimiters

	^ ')]}>|''"`'
</details>

#### SmalltalkEditor>>#contextualRenameInMethod

<details>
	<summary>See more</summary>
	
	contextualRenameInMethod

	self
		withMethodNodeAndClassDo: [ :methodNode :selectedClass | self contextualRenameOf: methodNode in: selectedClass]
		ifErrorsParsing: [ :anError | morph flash ]		
</details>

#### SmalltalkEditor>>#extractToTemporary: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	extractToTemporary: aKeyboardEvent

	self extractToTemporary.
	^true
</details>

#### SmalltalkEditor>>#printIt: aKeyboardEvent

Print the results of evaluting the selection -- invoked via cmd-p. If there is no current selection, use the current line. 1/17/96 sw 2/29/96 sw: don't call selectLine now, since it's called by doIt


<details>
	<summary>See more</summary>
	
	printIt: aKeyboardEvent
	"Print the results of evaluting the selection -- invoked via cmd-p.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine now, since it's called by doIt"

	self printIt.
	^ true
</details>

#### SmalltalkEditor>>#explainDelimitor: string

Is string enclosed in delimitors?


<details>
	<summary>See more</summary>
	
	explainDelimitor: string
	"Is string enclosed in delimitors?"

	| str |
	(string at: 1) isLetter ifTrue: [^nil].  "only special chars"
	(string first = string last) ifTrue:
			[^ self explainChar: (String with: string first)]
		ifFalse:
			[(string first = $( and: [string last = $)]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $[ and: [string last = $]]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = ${ and: [string last = $}]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $< and: [string last = $>]) ifTrue:
				[^ self explainChar: (String with: string first)].
			(string first = $# and: [string last = $)]) ifTrue:
				[^'An instance of class Array.  The Numbers, Characters, or Symbols between the parenthesis are the elements of the Array.'].
			string first = $# ifTrue:
				[^'An instance of class Symbol.'].
			(string first = $$ and: [string size = 2]) ifTrue:
				[^'An instance of class Character.  This one is the character ', (String with: string last), '.'].
			(string first = $:) ifTrue:
				[str := string allButFirst.
				(self explainTemp: str) ifNotNil: [
					^'An argument to this block will be bound to the temporary variable ',
						str, '.']]].
	^ nil
</details>

#### SmalltalkEditor>>#sendersOfIt: aKeyboardEvent

Triggered by Cmd-n; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw


<details>
	<summary>See more</summary>
	
	sendersOfIt: aKeyboardEvent
	"Triggered by Cmd-n; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"

	self sendersOfIt.
	^ true
</details>

#### SmalltalkEditor>>#withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

<details>
	<summary>See more</summary>
	
	withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: model actualContents noPattern: true ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.
</details>

#### SmalltalkEditor>>#acceptAndTestAll

<details>
	<summary>See more</summary>
	
	acceptAndTestAll
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteForCategoryOf: aMethod methodClass ].
	^true
	
	
</details>

#### SmalltalkEditor>>#contextualRenameInClassDefinition

<details>
	<summary>See more</summary>
	
	contextualRenameInClassDefinition 

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		self
			withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
				self contextualRenameInClassDefinitionOf: classDefinitionNode in: selectedClass]
			ifErrorsParsing: [ :anError | morph flash ] ]
</details>

#### SmalltalkEditor>>#evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile

Treat the current selection as an expression; evaluate it and return the result 3 +4


<details>
	<summary>See more</summary>
	
	evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result receiver context methodAndCompiler |

	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver _ provider doItReceiver.
			context _ provider doItContext]
		ifFalse: [receiver _ context _ nil].

	methodAndCompiler _ self compileSelectionFor: receiver in: context ifFail: [^ failBlock value].

	result _ (methodAndCompiler at: #compiler)
		evaluateMethod: (methodAndCompiler at: #method)
		to: receiver
		logged: true
		profiled: doProfile.
		
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	
	^ aBlock value: result
</details>

#### SmalltalkEditor>>#inspectIt: aKeyboardEvent

Inspect the selection -- invoked via cmd-i. If there is no current selection, use the current line. 1/17/96 sw 2/29/96 sw: don't call selectLine; it's done by inspectIt now


<details>
	<summary>See more</summary>
	
	inspectIt: aKeyboardEvent
	"Inspect the selection -- invoked via cmd-i.  If there is no current selection, use the current line.  1/17/96 sw
	 2/29/96 sw: don't call selectLine; it's done by inspectIt now"

	self inspectIt.
	^ true
</details>

#### SmalltalkEditor>>#contextualRename: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	contextualRename: aKeyboardEvent

	self contextualRename.
	^true
</details>

#### SmalltalkEditor>>#renameSelectorFor: aSelector in: aClassToRefactor

<details>
	<summary>See more</summary>
	
	renameSelectorFor: aSelector in: aClassToRefactor

	RefactoringApplier renameSelectorApplier createAndValueHandlingExceptionsOn: model textProvider for: aSelector in: aClassToRefactor 
</details>

#### SmalltalkEditor>>#selectNodeUnderCursorForInspectionIn: aMethodNode

<details>
	<summary>See more</summary>
	
	selectNodeUnderCursorForInspectionIn: aMethodNode 
	
	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self selectForInspection: nodeUnderCursor in: aMethodNode ]
		ifAbsent: []



</details>

#### SmalltalkEditor>>#referencesToIt: aKeyboardEvent

Triggered by Cmd-N; browse references to the current selection


<details>
	<summary>See more</summary>
	
	referencesToIt: aKeyboardEvent
	"Triggered by Cmd-N; browse references to the current selection"

	self referencesToIt.
	^ true
</details>

#### SmalltalkEditor>>#debug: aCompiledMethod receiver: anObject in: evalContext

<details>
	<summary>See more</summary>
	
	debug: aCompiledMethod receiver: anObject in: evalContext

	| guineaPig debugger context |
	debugger _ Debugger new.
	guineaPig _ [
		aCompiledMethod
			valueWithReceiver: anObject
			arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ]).
		debugger sendProceeds.
	] newProcess name: 'debugIt'.
	context _ guineaPig suspendedContext.
	debugger process: guineaPig context: context.
	debugger openFullNoSuspendLabel: 'Debug it'.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]
</details>

#### SmalltalkEditor>>#notify: aString at: anInteger in: aStream

The compilation of text failed. The syntax error is noted as the argument, aString. Insert it in the text at starting character position anInteger.


<details>
	<summary>See more</summary>
	
	notify: aString at: anInteger in: aStream 
	"The compilation of text failed. The syntax error is noted as the argument, 
	aString. Insert it in the text at starting character position anInteger."
	"This is a user command, and generates undo"
	model startNewUndoRedoCommand.
	self insertAndSelect: aString at: (anInteger max: 1).
</details>

#### SmalltalkEditor>>#selectMessageNode: aMessageNodeUnderCursor in: aMethodNode

<details>
	<summary>See more</summary>
	
	selectMessageNode: aMessageNodeUnderCursor in: aMethodNode
	
	| messageRange |
	
	self 
		withReceiverRangeOf: aMessageNodeUnderCursor 
		in: aMethodNode 
		selectorPosition: self startIndex 
		do: [ :receiverRange |
			messageRange := aMethodNode rangeForNode: aMessageNodeUnderCursor ifAbsent: [ ^ self ].
			self selectFrom: receiverRange first to: messageRange last ]
		
	
</details>

#### SmalltalkEditor>>#explainTemp: string

Is string the name of a temporary variable (method or block argument or temporary)?


<details>
	<summary>See more</summary>
	
	explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])
</details>

#### SmalltalkEditor>>#withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock

	self withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: [ morph flash ]
</details>

#### SmalltalkEditor>>#pasteInitials: aKeyboardEvent

Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor.


<details>
	<summary>See more</summary>
	
	pasteInitials: aKeyboardEvent 
	"Replace the current text selection by an authorship name/date stamp; invoked by cmd-shift-v, easy way to put an authorship stamp in the comments of an editor."
	"This is a user command, and generates undo"

	| i |
	i _ self stopIndex.
	self replaceSelectionWith: (Text fromString: Utilities changeStamp).
	self deselectAndPlaceCursorAt: i.
	^ true
</details>

#### SmalltalkEditor>>#withNodeUnderCursorDo: aDoBlock ifAbsent: anAbsentBlock

<details>
	<summary>See more</summary>
	
	withNodeUnderCursorDo: aDoBlock ifAbsent: anAbsentBlock

	self 
		withMethodNodeAndClassDo: [ :currentMethodNode :currentClass |
			currentMethodNode 
				withParseNodeIncluding: self startIndex
				do: aDoBlock
				ifAbsent: anAbsentBlock ] 
		ifErrorsParsing: [ :arg1 | anAbsentBlock value ].
</details>

#### SmalltalkEditor>>#acceptAndWithMethodDo: aBlock

<details>
	<summary>See more</summary>
	
	acceptAndWithMethodDo: aBlock
	
	| potencialTestMethod |
	
	self acceptContents ifFalse: [ ^false ].
	
	potencialTestMethod _ self codeProvider currentCompiledMethod.
	^potencialTestMethod 
		ifNil: [ false ]
		ifNotNil: [
			aBlock value: potencialTestMethod.
			true]
</details>

#### SmalltalkEditor>>#browseIt: aKeyboardEvent

Triggered by Cmd-B; browse the thing represented by the current selection, if plausible. 1/18/96 sw


<details>
	<summary>See more</summary>
	
	browseIt: aKeyboardEvent
	"Triggered by Cmd-B; browse the thing represented by the current selection, if plausible.  1/18/96 sw"

	self browseIt.
	^ true
</details>

#### SmalltalkEditor>>#renameInstanceVariableOn: aBrowser for: anInstanceVariableName at: aClassToRefactor

<details>
	<summary>See more</summary>
	
	renameInstanceVariableOn: aBrowser for: anInstanceVariableName at: aClassToRefactor

	(RenameInstanceVariableApplier on: aBrowser for: anInstanceVariableName at: aClassToRefactor) value
</details>

#### SmalltalkEditor>>#referencesToSelectedLiteral

Evaluate the selected text and browse methods that reference the same literal


<details>
	<summary>See more</summary>
	
	referencesToSelectedLiteral
	"Evaluate the selected text and browse methods that reference the same literal"
	[
		self
			evaluateSelectionAndDo: [ :result |
				Smalltalk
					browseMessageList: (Smalltalk allReferencesToLiteral: result) asArray sort
					name: 'Users of literal: ' , result asString
					autoSelect: self selection ]
			ifFail: nil
			profiled: false ]
	on: UndeclaredVariableReference , UnknownSelector
	do: [ :ex |
		morph flash ]
</details>

#### SmalltalkEditor>>#compileSelectionFor: anObject in: evalContext

<details>
	<summary>See more</summary>
	
	compileSelectionFor: anObject in: evalContext

	^(self compileSelectionFor: anObject in: evalContext ifFail: [ ^ nil ]) at: #method

</details>

#### SmalltalkEditor>>#withReceiverRangeOf: aMessageNode in: aMethodNode selectorPosition: aSelectorPosition do: aBlock

If aMessageNode receiver isNil it means that it is a cascade receiver so this imposes the question on how to inspect a cascade message send. We could inspect the result of sending all the messages up to the cursor but the problem is that when looking for the cascade receiver range it does not find it because it is a different node that the used in the source ranges... we could do the trick of looking for printString in the sourceRanges keys, but that is too much - Hernan


<details>
	<summary>See more</summary>
	
	withReceiverRangeOf: aMessageNode in: aMethodNode selectorPosition: aSelectorPosition do: aBlock   
	
	| receiverRange receiverRangeOrRanges messageNodeReceiver |
	
	"If aMessageNode receiver isNil it means that it is a cascade receiver so this imposes the question on how to inspect
	a cascade message send. We could inspect the result of sending all the messages up to the cursor but the problem is
	that when looking for the cascade receiver range it does not find it because it is a different node that the used in the source
	ranges... we could do the trick of looking for printString in the sourceRanges keys, but that is too much - Hernan"
	aMessageNode isCascade ifFalse: [ 
		messageNodeReceiver := aMessageNode receiver.
		messageNodeReceiver isMessageNode ifTrue: [ 
			^self withReceiverRangeOf: messageNodeReceiver in: aMethodNode selectorPosition: (messageNodeReceiver keywordPositionAt: 1) first do: aBlock ].
		
		receiverRangeOrRanges := aMethodNode rangeForNode: messageNodeReceiver ifAbsent: [ ^ self ].
		
		receiverRange := (aMethodNode isMultipleRanges: receiverRangeOrRanges)
			ifTrue: [ | closestRange |
				closestRange := receiverRangeOrRanges first.
				receiverRangeOrRanges do: [ :aRange |  (aRange last < aSelectorPosition and: [ aRange last > closestRange last ]) ifTrue: [ closestRange := aRange ]].
				closestRange ]
			ifFalse: [ receiverRangeOrRanges ].

		aBlock value: receiverRange ]
</details>

#### SmalltalkEditor>>#debugIt

<details>
	<summary>See more</summary>
	
	debugIt

	| provider method receiver context |

	self lineSelectAndEmptyCheck: [^self].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
			receiver _ provider doItReceiver.
			context _ provider doItContext]
		ifFalse: [
			receiver _ context _ nil].

	method _ self compileSelectionFor: receiver in: context.
	method ifNotNil: [ self debug: method receiver: receiver in: context ]
</details>

#### SmalltalkEditor>>#referencesToIt

Open a references browser on the selected symbol: a variable name or class name


<details>
	<summary>See more</summary>
	
	referencesToIt
	"Open a references browser on the selected symbol: a variable name or class name"

	| selectedSymbol provider environment selectedString |
	self hasSelection ifFalse: [ self selectWord ].
	selectedSymbol _ self selectedSymbol.

	"convenient access to class variables, including those in SharedPools"
	provider _ self codeProvider.
	environment _ (provider respondsTo: #selectedClassOrMetaClass) ifTrue: [ provider selectedClassOrMetaClass ].
	environment _ environment ifNil: [ Smalltalk ].

	(selectedSymbol ifNotNil: [environment bindingOf: selectedSymbol]) ifNotNil: [ :reference |
		Smalltalk browseAllCallsOn: reference.
		^ self ].

	selectedString _ self selectedString withoutSeparators.
	(environment ~= Smalltalk and: [ environment definesInstanceVariableNamedInHierarchy: selectedString ]) ifTrue: [
		Smalltalk browseAllAccessesTo: selectedString from: environment.
		^ self ].

	self referencesToSelectedLiteral
</details>

#### SmalltalkEditor>>#browseClassFromIt

Launch a hierarchy browser for the class indicated by the current selection. If multiple classes matching the selection exist, let the user choose among them.


<details>
	<summary>See more</summary>
	
	browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [^ self].

	aClass _ Utilities
		classFromPattern: self selectedString withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash].

	HierarchyBrowserWindow
		onClass: aClass
		selector: nil
</details>

#### SmalltalkEditor>>#openMenu: aMenu

<details>
	<summary>See more</summary>
	
	openMenu: aMenu
	
	aMenu popUpInWorld: morph world
</details>

#### SmalltalkEditor>>#browseIt

Launch a browser for the current selection, if appropriate


<details>
	<summary>See more</summary>
	
	browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry browser |
	Preferences alternativeBrowseIt ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry _ anEntry class].
			browser _ Browser new.
			browser setClass: anEntry selector: nil.
			BrowserWindow open: browser label:'System Browser: ',  aSymbol]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]
</details>

#### SmalltalkEditor>>#renameSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector

<details>
	<summary>See more</summary>
	
	renameSelectorOf: aMessageNode in: aSelectedClass at: aSelectedSelector

	RefactoringApplier renameSelectorApplier
		on: aMessageNode
		createAndValueHandlingExceptionsOn: model textProvider
		in: aSelectedClass
		at: aSelectedSelector
</details>

#### SmalltalkEditor>>#contextualRename

<details>
	<summary>See more</summary>
	
	contextualRename

	self isEditingClassDefinition 
		ifTrue: [ self contextualRenameInClassDefinition ]
		ifFalse: [ self contextualRenameInMethod ]
</details>

#### SmalltalkEditor>>#getMenu

<details>
	<summary>See more</summary>
	
	getMenu
	
	^self createMenuCollectingOptionsWith: #smalltalkEditorMenuOptions
</details>

#### SmalltalkEditor>>#openMenu2

<details>
	<summary>See more</summary>
	
	openMenu2
	
	self openMenu: self getMenu2
	
</details>

#### SmalltalkEditor>>#explainMySel: symbol

Is symbol the selector of this method? Is it sent by this method? If not, then expalin will call (explainPartSel:) to see if it is a fragment of a selector sent here. If not, explain will call (explainAnySel:) to catch any selector.


<details>
	<summary>See more</summary>
	
	explainMySel: symbol 
	"Is symbol the selector of this method?  Is it sent by this method?  If 
	not, then expalin will call (explainPartSel:) to see if it is a fragment of a 
	selector sent here.  If not, explain will call (explainAnySel:) to catch any 
	selector. "

	| provider lits classes msg |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	(msg _ provider selectedMessageName) ifNil: [^nil].	"not in a message"
	classes _ Smalltalk allClassesImplementing: symbol.
	classes size > 12
		ifTrue: [classes _ 'many classes']
		ifFalse: [classes _ 'these classes ' , classes printString].
	msg = symbol
		ifTrue: [
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is the selector of this very method!  It is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]
		ifFalse: [
			lits _ (provider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(lits detect: [:each | each == symbol]
				ifNone: nil)
					ifNil: [^nil].
			^ String streamContents: [:str |
				str
					nextPut: $#;
					nextPutAll: symbol;
					nextPutAll: ' is a message selector which is defined in ';
					nextPutAll: classes;
					nextPutAll: self class plateA]]

</details>

#### SmalltalkEditor>>#withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan


<details>
	<summary>See more</summary>
	
	withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	"I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan"
	selectedClass := [ self codeProvider selectedClassOrMetaClass ] on: Error do: [ :anError | anError return: UndefinedObject ].
	[ methodNode := selectedClass methodNodeFor: model actualContents asString ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.
</details>

#### SmalltalkEditor>>#exploreIt: aKeyboardEvent

Explore the selection -- invoked via cmd-shift-I. If there is no current selection, use the current line.


<details>
	<summary>See more</summary>
	
	exploreIt: aKeyboardEvent
	"Explore the selection -- invoked via cmd-shift-I.  If there is no current selection, use the current line."

	self exploreIt.
	^ true
</details>

#### SmalltalkEditor>>#testSuiteOf: aClass

<details>
	<summary>See more</summary>
	
	testSuiteOf: aClass

	^TestSuite forClass: aClass

</details>

#### SmalltalkEditor>>#explainClass: symbol

Is symbol a class variable or a pool variable?


<details>
	<summary>See more</summary>
	
	explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes _ WriteStream on: Array new.
			Smalltalk
				allBehaviorsDo: [:each | (each sharedPools
						detect: 
							[:pool | 
							pool == reply]
						ifNone: nil)
						notNil ifTrue: [classes nextPut: each]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes contents printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil
</details>

#### SmalltalkEditor>>#contextualRemoveParameter: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	contextualRemoveParameter: aKeyboardEvent 
	
	self contextualRemoveParameter.
	^true
</details>

#### SmalltalkEditor>>#debugIt: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	debugIt: aKeyboardEvent

	self debugIt.
	^true
</details>

#### SmalltalkEditor>>#implementorsOfIt: aKeyboardEvent

Triggered by Cmd-m; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw


<details>
	<summary>See more</summary>
	
	implementorsOfIt: aKeyboardEvent
	"Triggered by Cmd-m; browse implementors of the selector represented by the current selection, if plausible. 2/1/96 sw"

	self implementorsOfIt.
	^ true
</details>

#### SmalltalkEditor>>#acceptAndTest: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	acceptAndTest: aKeyboardEvent

	^self acceptAndTest
</details>

#### SmalltalkEditor>>#doIt: aKeyboardEvent

Called when user hits cmd-d. Select the current line, if relevant, then evaluate and execute. 2/1/96 sw. 2/29/96 sw: don't call selectLine; it's done by doIt now


<details>
	<summary>See more</summary>
	
	doIt: aKeyboardEvent
	"Called when user hits cmd-d.  Select the current line, if relevant, then evaluate and execute.  2/1/96 sw.
	2/29/96 sw: don't call selectLine; it's done by doIt now"

	self doIt.
	^ true
</details>

#### SmalltalkEditor>>#acceptAndTest

<details>
	<summary>See more</summary>
	
	acceptAndTest
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteOf: aMethod methodClass ].
	^true
</details>

#### SmalltalkEditor>>#displayIfFalse: aKeyboardEvent

Replace the current text selection with the text 'ifFalse:'--initiated by cmd-F.


<details>
	<summary>See more</summary>
	
	displayIfFalse: aKeyboardEvent
	"Replace the current text selection with the text 'ifFalse:'--initiated by 
	cmd-F."

	self addString: 'ifFalse:'.
	^false
</details>

#### SmalltalkEditor>>#openSmalltalkEditorRefactoringMenu

<details>
	<summary>See more</summary>
	
	openSmalltalkEditorRefactoringMenu

	^self refactoringMenu popUpInWorld
</details>

#### SmalltalkEditor>>#sendersOfIt

Open a senders browser. If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line.


<details>
	<summary>See more</summary>
	
	sendersOfIt
	"Open a senders browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllCallsOn: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllCallsOn: selector ]
		otherwise: [ self sendersOfItWhenErrorsParsing ]
</details>

#### SmalltalkEditor>>#explainAnySel: symbol

Is this any message selector?


<details>
	<summary>See more</summary>
	
	explainAnySel: symbol 
	"Is this any message selector?"

	| list reply |
	list _ Smalltalk allClassesImplementing: symbol.
	list size = 0 ifTrue: [^nil].
	list size < 12
		ifTrue: [reply _ ' is a message selector which is defined in these classes ' , list printString]
		ifFalse: [reply _ ' is a message selector which is defined in many classes'].
	^'#' , symbol , reply , '.\' withNewLines, 'Smalltalk browseAllImplementorsOf: #' , symbol
</details>

#### SmalltalkEditor>>#nextTokenFrom: start direction: dir

simple token-finder for compiler automated corrections


<details>
	<summary>See more</summary>
	
	nextTokenFrom: start direction: dir
	"simple token-finder for compiler automated corrections"
	| loc str |
	loc _ start + dir.
	str _ self privateCurrentString.
	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]
		whileTrue: [loc _ loc + dir].
	^ loc
</details>

#### SmalltalkEditor>>#selectedSelector

Try to make a selector out of the current text selection


<details>
	<summary>See more</summary>
	
	selectedSelector
	"Try to make a selector out of the current text selection"

	^ self selectedString findSelector
</details>

#### SmalltalkEditor>>#doIt

Set the context to include pool vars of the model. Then evaluate. Print the result on the Transcript


<details>
	<summary>See more</summary>
	
	doIt
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			result print ]
		ifFail: nil
		profiled: false
</details>

#### SmalltalkEditor>>#contextualRemoveParameter

<details>
	<summary>See more</summary>
	
	contextualRemoveParameter

	self contextualChangeSelectorUsing: RefactoringApplier removeParameterApplier 
</details>

#### SmalltalkEditor>>#apply: aBlock inClassDefinitionOf: aClassDefinitionNode in: aSelectedClass

<details>
	<summary>See more</summary>
	
	apply: aBlock inClassDefinitionOf: aClassDefinitionNode in: aSelectedClass 
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ aBlock value: aSelectedClass ]
		ifFalse: [ morph flash ]
	

</details>

#### SmalltalkEditor>>#getMenu2

<details>
	<summary>See more</summary>
	
	getMenu2
	
	^self createMenuCollectingOptionsWith: #smalltalkEditorMenu2Options
</details>

#### SmalltalkEditor>>#withSelectorUnderCursorDo: aBlock otherwise: failBlock

<details>
	<summary>See more</summary>
	
	withSelectorUnderCursorDo: aBlock otherwise: failBlock

	self withSelectorUnderCursorDo: aBlock ifErrorsParsing: failBlock ifNoSelector: failBlock
</details>

#### SmalltalkEditor>>#clickAndHalf

<details>
	<summary>See more</summary>
	
	clickAndHalf

	| here |
	here _ self pointIndex.
	(here between: 2 and: model textSize)
		ifTrue: [
			super clickAndHalf ]
		ifFalse: [
			"if at beginning or end, select entire string"
			self selectAll ]
</details>

#### SmalltalkEditor>>#runTestSuite: aTestSuite

<details>
	<summary>See more</summary>
	
	runTestSuite: aTestSuite

	(ProgressiveTestRunner for: aTestSuite) value
	
	
</details>

#### SmalltalkEditor>>#contextualRenameOf: aMethodNode in: aSelectedClass

<details>
	<summary>See more</summary>
	
	contextualRenameOf: aMethodNode in: aSelectedClass

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self rename: nodeUnderCursor in: aSelectedClass at: aMethodNode ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self renameSelectorFor: aMethodNode selector in: aSelectedClass ]]
				ifFalse: [ morph flash ]]



</details>

#### SmalltalkEditor>>#renameClassOn: aBrowser for: aClassToRefactor

<details>
	<summary>See more</summary>
	
	renameClassOn: aBrowser for: aClassToRefactor

	(RenameClassApplier for: aClassToRefactor) value
</details>

#### SmalltalkEditor>>#acceptAndDebugTest: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	acceptAndDebugTest: aKeyboardEvent 
	
	^self acceptAndDebugTest 
</details>

#### SmalltalkEditor>>#contextualChangeSelectorOf: aMethodNode in: aSelectedClass using: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	contextualChangeSelectorOf: aMethodNode in: aSelectedClass using: aChangeSelectorApplier

	aMethodNode
		withParseNodeIncluding: self startIndex
		do: [ :nodeUnderCursor | self changeSelector: nodeUnderCursor in: aSelectedClass at: aMethodNode selector using: aChangeSelectorApplier ]
		ifAbsent: [
			self startIndex <= aMethodNode selectorLastPosition
				ifTrue: [ self changeSelectorTo: aMethodNode selector in: aSelectedClass using: aChangeSelectorApplier ]
				ifFalse: [ morph flash ]]

</details>

#### SmalltalkEditor>>#profileIt

Like #doit, but profiling


<details>
	<summary>See more</summary>
	
	profileIt
	"Like #doit, but profiling"
	^ self
		evaluateSelectionAndDo: [ :result | (result printString) print ]
		ifFail: nil
		profiled: true
</details>

#### SmalltalkEditor>>#explainNumber: string

Is string a Number?


<details>
	<summary>See more</summary>
	
	explainNumber: string 
	"Is string a Number?"

	| strm c |
	(c _ string at: 1) isDigit ifFalse: [(c = $- and: [string size > 1 and: [(string at: 2) isDigit]])
			ifFalse: [^nil]].
	strm _ ReadStream on: string.
	c _ Number readFrom: strm.
	strm atEnd ifFalse: [ ^nil ].
	c printString = string
		ifTrue: [ ^ string , ' is a ' , c class name ]
		ifFalse: [ ^ string , ' (= ' , c printString , ') is a ' , c class name ]
</details>

#### SmalltalkEditor>>#isEditingClassDefinition

<details>
	<summary>See more</summary>
	
	isEditingClassDefinition

	^(self codeProvider is: #Browser) and: [ self codeProvider isEditingExistingClass ]
</details>

#### SmalltalkEditor>>#openMenu

<details>
	<summary>See more</summary>
	
	openMenu
	
	self openMenu: self getMenu 
	
</details>

#### SmalltalkEditor>>#acceptThenTestMethodAndSuite: aSuiteBuilder

<details>
	<summary>See more</summary>
	
	acceptThenTestMethodAndSuite: aSuiteBuilder
	
	self acceptAndWithMethodDo: [ :aPotencialTestMethod |
		self runAndDebuggIfNecessary: aPotencialTestMethod.
		self runTestSuite: (aSuiteBuilder value: aPotencialTestMethod) ]
</details>

#### SmalltalkEditor>>#save: aKeyboardEvent

Submit the current text. Equivalent to 'accept' 1/18/96 sw


<details>
	<summary>See more</summary>
	
	save: aKeyboardEvent
	"Submit the current text.  Equivalent to 'accept' 1/18/96 sw"

	self acceptContents.
	^ true
</details>

#### SmalltalkEditor>>#inspectSelectionOrLine

<details>
	<summary>See more</summary>
	
	inspectSelectionOrLine

	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false
</details>

#### SmalltalkEditor>>#ifEditingClassDefinitionDoOrWarn: aBlock

<details>
	<summary>See more</summary>
	
	ifEditingClassDefinitionDoOrWarn: aBlock

	self isEditingClassDefinition 
		ifTrue: aBlock
		ifFalse: [ self informRefactoringCanOnlyBeAppliedInClassDefinition ]
</details>

#### SmalltalkEditor>>#displayIfTrue: aKeyboardEvent

Replace the current text selection with the text 'ifTrue:'--initiated by cmd-T.


<details>
	<summary>See more</summary>
	
	displayIfTrue: aKeyboardEvent
	"Replace the current text selection with the text 'ifTrue:'--initiated by 
	cmd-T."

	self addString: 'ifTrue:'.
	^false
</details>

#### SmalltalkEditor>>#contextualAddParameter

<details>
	<summary>See more</summary>
	
	contextualAddParameter
	
	self contextualChangeSelectorUsing: RefactoringApplier addParameterApplier 
</details>

#### SmalltalkEditor>>#methodSourceContainingIt

Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source). Slow!


<details>
	<summary>See more</summary>
	
	methodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!"

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithSourceString: self selectedString
</details>

#### SmalltalkEditor>>#newLine: aKeyboardEvent

Replace the current text selection with a newLine (i.e. LF) followed by as many tabs as there are leading tabs on the current line (+/- bracket count).


<details>
	<summary>See more</summary>
	
	newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString _ String streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false
</details>

#### SmalltalkEditor>>#printIt

Treat the current text selection as an expression; evaluate it. Insert the description of the result of evaluation after the selection and then make this description the new text selection.


<details>
	<summary>See more</summary>
	
	printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			self afterSelectionInsertAndSelect: 
				((' ', result printText, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false
</details>

#### SmalltalkEditor>>#wordLeftDelimiters

<details>
	<summary>See more</summary>
	
	wordLeftDelimiters

	^ '([{<|''"`'
</details>

#### SmalltalkEditor>>#rename: aNodeUnderCursor in: aSelectedClass at: aMethodNode

<details>
	<summary>See more</summary>
	
	rename: aNodeUnderCursor in: aSelectedClass at: aMethodNode

	aNodeUnderCursor isTempOrArg ifTrue: [ ^self renameTemporary: aNodeUnderCursor at: aMethodNode ].

	self ifSourceCodeRefactoringCanBeAppliedDo: [
		aNodeUnderCursor isMessageNode ifTrue: [
			^ self renameSelectorOf: aNodeUnderCursor in: aSelectedClass at: aMethodNode selector ].
		aNodeUnderCursor isInstanceVariableNode ifTrue: [
			^ self renameInstanceVariableOn: self codeProvider for: aNodeUnderCursor name at: aSelectedClass ].
		aNodeUnderCursor isLiteralVariableNode ifTrue: [ | variableName |
			variableName := aNodeUnderCursor key key.
			(Smalltalk classNamed: variableName) ifNotNil: [ :classToRename |
				^self renameClassOn: self codeProvider for: classToRename ].
			(Smalltalk bindingOf: variableName) ifNotNil: [
				^self renameGlobalOn: self codeProvider for: variableName ] ].

		^morph flash ]
</details>

#### SmalltalkEditor>>#contextualAddParameter: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	contextualAddParameter: aKeyboardEvent 
	
	self contextualAddParameter.
	^true
</details>

#### SmalltalkEditor>>#methodStringsContainingit: aKeyboardEvent

Triggered by Cmd-E


<details>
	<summary>See more</summary>
	
	methodStringsContainingit: aKeyboardEvent
	"Triggered by Cmd-E"

	self methodStringsContainingit.
	^ true
</details>

#### SmalltalkEditor>>#informRefactoringCanOnlyBeAppliedInClassDefinition

<details>
	<summary>See more</summary>
	
	informRefactoringCanOnlyBeAppliedInClassDefinition

	self inform: 'This refactoring can only be applied from the class definition'
</details>

#### SmalltalkEditor>>#methodStringsContainingit

Open a browser on methods which contain the current selection as part of a string constant.


<details>
	<summary>See more</summary>
	
	methodStringsContainingit
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithString: self selectedString
</details>

#### SmalltalkEditor>>#explainPartSel: string

Is this a fragment of a multiple-argument selector sent in this method?


<details>
	<summary>See more</summary>
	
	explainPartSel: string 
	"Is this a fragment of a multiple-argument selector sent in this method?"
	| lits whole reply classes s msg provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	(msg _ provider selectedMessageName) ifNil: [^ nil].  "not in a message"
	string last == $: ifFalse: [^ nil].
	"Name of this method"
	lits _ Array with: msg.
	(whole _ lits detect: [:each | (each keywords detect: [:frag | frag = string]
					ifNone: nil) notNil]
				ifNone: nil)
		ifNotNil: [
			reply _ ', which is the selector of this very method!'.
			s _ self class plateA]
		ifNil: [ 
			"Selectors called from this method"
			lits _ (provider selectedClassOrMetaClass compiledMethodAt: msg) messages.
			(whole _ lits detect: [:each | (each keywords detect: [ :frag | frag = string ]
							ifNone: nil) notNil]
						ifNone: nil) notNil
				ifFalse: [string = 'primitive:'
					ifTrue: [^self explainChar: '<']
					ifFalse: [^nil]].
			reply _ '.'.
			s _ self class plateB].
		classes _ Smalltalk allClassesImplementing: whole.
		classes size > 12
			ifTrue: [classes _ 'many classes']
			ifFalse: [classes _ 'these classes ' , classes printString].
		^ String streamContents: [:str |
			str
				nextPutAll: string;
				nextPutAll: ' is one part of the message selector ';
				nextPutAll: whole;
				nextPutAll: reply;
				nextPutAll: '  It is defined in ';
				nextPutAll: classes;
				nextPutAll: s]
</details>

#### SmalltalkEditor>>#runAndDebuggIfNecessary: aPotencialTestMethod

<details>
	<summary>See more</summary>
	
	runAndDebuggIfNecessary: aPotencialTestMethod

	aPotencialTestMethod isTestMethod ifTrue: [
		aPotencialTestMethod methodClass debug: aPotencialTestMethod selector ]
</details>

#### SmalltalkEditor>>#contextualChangeSelectorUsing: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	contextualChangeSelectorUsing: aChangeSelectorApplier
	
	self isEditingClassDefinition 
		ifTrue: [ morph flash ]
		ifFalse: [ self ifSourceCodeRefactoringCanBeAppliedDo: [ self contextualChangeSelectorInMethodUsing: aChangeSelectorApplier ]]
</details>

#### SmalltalkEditor>>#withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

<details>
	<summary>See more</summary>
	
	withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

	| methodNode nodeAtRange nodeUnderCursor alternativeAnswer failed |
	failed _ false. 
	methodNode := self codeProvider
		methodNodeOf: model actualContents
		ifErrorsParsing: [ :anError |
			alternativeAnswer _ aParsingErrorBlock valueWithPossibleArgument: anError.
			failed _ true ].
	failed ifTrue: [ ^alternativeAnswer ].

	self startIndex < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: self startIndex ifAbsent: [ ^ aNoSelectorBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].

	^ aNoSelectorBlock value 
</details>

#### SmalltalkEditor>>#contextualRenameInClassDefinitionOf: aClassDefinitionNode in: aSelectedClass

<details>
	<summary>See more</summary>
	
	contextualRenameInClassDefinitionOf: aClassDefinitionNode in: aSelectedClass 
	
	| analyzer cursorPosition |
	
	analyzer := ClassDefinitionNodeAnalyzer for: aClassDefinitionNode.
	cursorPosition := self startIndex.
	
	(analyzer isAtSuperclass: cursorPosition) 
		ifTrue: [ ^self renameClassOn: self codeProvider for: analyzer superclass ].
	
	(analyzer isAtClassName: cursorPosition)
		ifTrue: [ ^self renameClassOn: self codeProvider for: aSelectedClass ].
		
	(analyzer isAtInstanceVariables: cursorPosition) 
		ifTrue: [ |selection variableToRename|
			selection := self selectedString.
			variableToRename := selection isEmpty ifTrue: [ self wordUnderCursor ] ifFalse: [ selection ].
			^self renameInstanceVariableOn: self codeProvider for: variableToRename at: aSelectedClass ].
		
	(analyzer isAtCategory: cursorPosition)
		ifTrue: [ 
			"I'm sure codeProvider is a Browser - Hernan"
			^self codeProvider renameSystemCategory ].
		
	morph flash
	

</details>

#### SmalltalkEditor>>#ifSourceCodeRefactoringCanBeAppliedDo: aBlock

<details>
	<summary>See more</summary>
	
	ifSourceCodeRefactoringCanBeAppliedDo: aBlock

	^(self hasUnacceptedEdits or: [morph hasEditingConflicts ])
		ifTrue: [ self inform: 'This refactoring can not be applied when there are unsaved changes' ]
		ifFalse: aBlock
</details>

#### SmalltalkEditor>>#fileItIn

Make a Stream on the text selection and fileIn it.


<details>
	<summary>See more</summary>
	
	fileItIn
	"Make a Stream on the text selection and fileIn it."

	| selection |
	selection _ self selection.
	(ReadWriteStream on: selection string from: 1 to: selection size) fileIn
</details>

#### SmalltalkEditor>>#inspectIt

<details>
	<summary>See more</summary>
	
	inspectIt
	
	self hasSelection ifFalse: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass | self selectNodeUnderCursorForInspectionIn: methodNode ]
			ifErrorsParsing: [ :anError | ]].
			
	self inspectSelectionOrLine

	
</details>

#### SmalltalkEditor>>#createMenuCollectingOptionsWith: aMenuOptionsSelector

<details>
	<summary>See more</summary>
	
	createMenuCollectingOptionsWith: aMenuOptionsSelector
	
	^(DynamicMenuBuilder buildTitled: self class name targeting: self collectingMenuOptionsWith: aMenuOptionsSelector)
		addStayUpIcons;
		yourself
		

</details>

#### SmalltalkEditor>>#inClassDefinitionContextuallyApply: aBlock

<details>
	<summary>See more</summary>
	
	inClassDefinitionContextuallyApply: aBlock

	self ifEditingClassDefinitionDoOrWarn: [
		self ifSourceCodeRefactoringCanBeAppliedDo: [
			self
				withClassDefinitionNodeAndClassDo: [ :classDefinitionNode :selectedClass | 
					self apply: aBlock inClassDefinitionOf: classDefinitionNode in: selectedClass ]
				ifErrorsParsing: [ :anError | morph flash ]]]
</details>

#### SmalltalkEditor>>#fileItIn: aKeyboardEvent

File in the selection; invoked via a keyboard shortcut, -- for now, cmd-shift-G.


<details>
	<summary>See more</summary>
	
	fileItIn: aKeyboardEvent
	"File in the selection; invoked via a keyboard shortcut, -- for now, cmd-shift-G."

	self fileItIn.
	^ true
</details>

#### SmalltalkEditor>>#changeSelector: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

<details>
	<summary>See more</summary>
	
	changeSelector: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier

	aNodeUnderCursor isMessageNode 
		ifTrue: [ self changeSelectorOf: aNodeUnderCursor in: aSelectedClass at: aSelectedSelector using: aChangeSelectorApplier ]
		ifFalse: [ morph flash ]

</details>

#### SmalltalkEditor>>#contextualPushUpInClassDefinition

<details>
	<summary>See more</summary>
	
	contextualPushUpInClassDefinition 

	self inClassDefinitionContextuallyApply: [ :aSelectedClass | 
		(PushUpInstanceVariableApplier 
			on: self codeProvider 
			for: self wordUnderCursor 
			at: aSelectedClass) value ]
</details>

#### SmalltalkEditor>>#explain

Try to shed some light on what kind of entity the current selection is. The selection must be a single token or construct. Insert the answer after the selection. Send private messages whose names begin with 'explain' that return a string if they recognize the selection, else nil.


<details>
	<summary>See more</summary>
	
	explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string _ self selectedString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply size = 0 ifTrue: [reply _ sorry].

	morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print
</details>

#### SmalltalkEditor>>#extractMethod: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	extractMethod: aKeyboardEvent

	self extractMethod.
	^true
</details>

## Text

I represent a character string that has been marked with abstract changes in character appearance. Text associates a set of TextAttributes with each character in its character string. These attributes may be fonts, emphases such as bold or italic, or hyperling actions. Since most characters have the same attributes as their neighbors, the attributes are stored in a RunArray for efficiency. Each of my instances has string a String runs a RunArray From the comment at #deepCopy, written by Dan on 11/9/97: "Both string and runs are assumed to be read-only"

### Methods
#### Text>>#removeAttributes: attributesToRemove from: requestedStart to: requestedStop

Remove the attribute over the interval start to stop. Turned into a command to enable reuse by undo / redo


<details>
	<summary>See more</summary>
	
	removeAttributes: attributesToRemove from: requestedStart to: requestedStop
	"Remove the attribute over the interval start to stop.
	Turned into a command to enable reuse by undo / redo"
	
	(self
		commandForRemoveAttributes: attributesToRemove
		from: requestedStart
		to: requestedStop) doOn: self
</details>

#### Text>>#fontAt: characterIndex default: defaultFont

Answer the font for characters in the run beginning at characterIndex.


<details>
	<summary>See more</summary>
	
	fontAt: characterIndex default: defaultFont
	"Answer the font for characters in the run beginning at characterIndex."

	^self fontIfApplying: (self attributesAt: characterIndex) default: defaultFont
</details>

#### Text>>#copyReplaceTokens: oldSubstring with: newSubstring

Replace all occurrences of oldSubstring that are surrounded by non-alphanumeric characters


<details>
	<summary>See more</summary>
	
	copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"
</details>

#### Text>>#at: index put: character

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: character

	| answer prevChar |
	prevChar _ string at: index.
	answer _ string at: index put: character.

	"Only fix ParagraphAttributes if there is real danger of breaking the invariant"
	(prevChar isLineSeparator and: [
			(self attributesAt: index) anySatisfy: [ :attr | attr isParagraphAttribute]]) ifTrue: [
		self fixParagraphAttributesFrom: index to: index ].
	
	^answer
</details>

#### Text>>#attributesFrom: start to: stop do: aBlock

evaluate aBlock for each attribute in the specified range. Warning: aBlock might be evaluated several times for each attribute, but not as many as the characters that it applies to!.


<details>
	<summary>See more</summary>
	
	attributesFrom: start to: stop do: aBlock
	"evaluate aBlock for each attribute in the specified range.
	Warning: aBlock might be evaluated several times for each attribute, but not as many as the characters that it applies to!."
	runs runsFrom: start to: stop do: [ :attributes |
		attributes do: [ :attribute |
			aBlock value: attribute ]]
</details>

#### Text>>#reversed

Answer a copy of the receiver with element order reversed.


<details>
	<summary>See more</summary>
	
	reversed
	"Answer a copy of the receiver with element order reversed."
	
	| answer |
	answer _ self class string: string reversed runs: runs reversed.
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected.
	The way it is done could be considered to be wrong. In this case, instead of making the text to take
	the ParagraphAttributes from the cr characters, it could be done the other way, making the cr's take 
	the ParagraphAttributes of the preceeding char. This way, the attributes would be mostrly preserved.
	I don't know if this is of much use anyway"
	answer fixParagraphAttributesFrom: 1 to: answer size.
	^answer

  "  It is assumed that  self size = runs size  holds. "
</details>

#### Text>>#commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop

Set the attribute for characters in the interval start to stop.


<details>
	<summary>See more</summary>
	
	commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop

	"Set the attribute for characters in the interval start to stop."
	| intervalToFix start stop new old |
	start _ requestedStart.
	stop _ requestedStop.
	
	"If aTextAttribute must be applied to whole paragraphs, do so."
	aTextAttribute isParagraphAttribute ifTrue: [
		intervalToFix _ self encompassParagraph: (start to: stop).
		start _ intervalToFix first.
		stop _ intervalToFix last ].

	old _ runs copyFrom: start to: stop.
	new _ old copy mapValues: [ :attributes | 
			Text addAttribute: aTextAttribute toArray: attributes ].
	^AttributesReplaceCommand
		old: old
		new: new
		start: start
		stop: stop
</details>

#### Text>>#encompassParagraph: anInterval

<details>
	<summary>See more</summary>
	
	encompassParagraph: anInterval
	^string encompassParagraph: anInterval
</details>

#### Text>>#green

Stuff like 'Hello world' green edit


<details>
	<summary>See more</summary>
	
	green
	"Stuff like
	'Hello world' green edit
	"
	self color: Color green
</details>

#### Text>>#bold

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis bold
</details>

#### Text>>#black

Stuff like 'Hello world' black edit


<details>
	<summary>See more</summary>
	
	black
	"Stuff like
	'Hello world' black edit
	"
	self color: Color black
</details>

#### Text>>#rightFlush

Stuff like ('Hello world' rightFlush ) edit


<details>
	<summary>See more</summary>
	
	rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	self addAttribute: TextAlignment rightFlush
</details>

#### Text>>#asText

Answer the receiver itself.


<details>
	<summary>See more</summary>
	
	asText	
	"Answer the receiver itself."

	^self
</details>

#### Text>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream isText
		ifTrue: [aStream nextPutAll: self. ^ self].
	self printNameOn: aStream.
	aStream nextPutAll: ' for '; print: string
</details>

#### Text>>#privateSetParagraphAttributes: paragraphAttributes from: start to: stop

<details>
	<summary>See more</summary>
	
	privateSetParagraphAttributes: paragraphAttributes from: start to: stop

	self privateSetRuns: (runs
		copyReplaceFrom: start
		to: stop
		with: ((runs copyFrom: start to: stop) mapValues: [ :attributes |
			Text setParagraphAttributes: paragraphAttributes toArray: attributes]))
</details>

#### Text>>#size

Answer how many elements the receiver contains.


<details>
	<summary>See more</summary>
	
	size

	^string size
</details>

#### Text>>#attributesAt: characterIndex

Answer the code for characters in the run beginning at characterIndex.


<details>
	<summary>See more</summary>
	
	attributesAt: characterIndex
	"Answer the code for characters in the run beginning at characterIndex."

	self size = 0
		ifTrue: [^ #()].  "null text tolerates access"
	^runs at: characterIndex
</details>

#### Text>>#paragraphStyleOrNilIfApplying: textAttributes

Answer the ParagraphStyle for characters as specified by the argument.


<details>
	<summary>See more</summary>
	
	paragraphStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self
		withAttributeValues: textAttributes
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^paragraphStyle ]
</details>

#### Text>>#justified

Stuff like ('Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. ' justified ) edit


<details>
	<summary>See more</summary>
	
	justified
	"Stuff like
	('Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. Hello world. ' justified ) edit
	"
	self addAttribute: TextAlignment justified
</details>

#### Text>>#hasAnyAttribute

Return false if there are no emphasis (i.e., a String would not make a difference)


<details>
	<summary>See more</summary>
	
	hasAnyAttribute
	"Return false if there are no emphasis (i.e., a String would not make a difference)"

	^runs values anySatisfy: [ :emphArray | emphArray notEmpty ]
</details>

#### Text>>#paragraphStyleChunksDo: aBlock

Evaluate aBlock over each chunk (sequence of paragraphs) that have the same paragraphStyle


<details>
	<summary>See more</summary>
	
	paragraphStyleChunksDo: aBlock
	"Evaluate aBlock over each chunk (sequence of paragraphs) that have the same paragraphStyle"
	| start nextStart style |
	start _ 1.
	nextStart _ 1.
	[ start <= self size ] whileTrue: [
		style _ self paragraphStyleOrNilAt: start.
		[ nextStart <= self size and: [ (self paragraphStyleOrNilAt: nextStart) = style ]] whileTrue: [
			nextStart _ nextStart + 1 ].
		aBlock value: (start to: nextStart-1) value: style.
		start _ nextStart ]
</details>

#### Text>>#paragraphStyleOrNilAt: characterIndex

<details>
	<summary>See more</summary>
	
	paragraphStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ paragraphStyle ]
</details>

#### Text>>#commandForReplaceFrom: start to: stop with: replacement

<details>
	<summary>See more</summary>
	
	commandForReplaceFrom: start to: stop with: replacement

	^TextReplaceCommand
		old: (self copyFrom: start to: stop)
		new: replacement
		at: start
</details>

#### Text>>#alignmentAt: characterIndex

<details>
	<summary>See more</summary>
	
	alignmentAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ alignment ]
</details>

#### Text>>#yellow

Stuff like 'Hello world' yellow edit


<details>
	<summary>See more</summary>
	
	yellow
	"Stuff like
	'Hello world' yellow edit
	"
	self color: Color yellow
</details>

#### Text>>#replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart

This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. Do it to both the string and the runs. The size does not change


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| newRepRuns |
	string := string replaceFrom: start to: stop with: replacementString startingAt: repStart.
	newRepRuns _ RunArray new: stop-start+1 withAll: attributesArray.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacementString size - 1
</details>

#### Text>>#editLabel: labelString

<details>
	<summary>See more</summary>
	
	editLabel: labelString

	TextModel new contents: self; openLabel: labelString
</details>

#### Text>>#canJoin: attributes1 and: attributes2

<details>
	<summary>See more</summary>
	
	canJoin: attributes1 and: attributes2
	| s |
	s _ attributes1 size.
	^s = attributes2 size and: [
		(1 to: s) allSatisfy: [ :i |
			(attributes1 at: i) canBeJoinedWith: (attributes2 at: i) ]]
</details>

#### Text>>#findString: aString startingAt: start

Answer the index of subString within the receiver, starting at index start. If the receiver does not contain subString, answer 0.


<details>
	<summary>See more</summary>
	
	findString: aString startingAt: start 
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start
</details>

#### Text>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Text or: [ super is: aSymbol ]
</details>

#### Text>>#append: stringOrText

<details>
	<summary>See more</summary>
	
	append: stringOrText

	^ stringOrText appendToText: self
</details>

#### Text>>#gray

Stuff like 'Hello world' gray edit


<details>
	<summary>See more</summary>
	
	gray
	"Stuff like
	'Hello world' gray edit
	"
	self color: Color gray
</details>

#### Text>>#optimizedForMutationSpeed

Do not use RunArray. Optimized for extensive attribute modification.


<details>
	<summary>See more</summary>
	
	optimizedForMutationSpeed
	"Do not use RunArray. Optimized for extensive attribute modification."
	
	(runs is: #RunArray) ifTrue: [
		^Text string: string runs: (RunNotArray withAll: runs) ]
</details>

#### Text>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	string _ string copy.
	runs _ runs copy.
	runs mapValues: [ :attributes | attributes collect: [ :attr | attr copy ]]
</details>

#### Text>>#embeddedMorphsFrom: start to: stop

return the list of morphs embedded in me


<details>
	<summary>See more</summary>
	
	embeddedMorphsFrom: start to: stop 
	"return the list of morphs embedded in me"

	| morphs |
	morphs _ IdentitySet new.
	self attributesFrom: start to: stop do: [:attr |
		attr anchoredFormOrMorph ifNotNil: [ :m |
			(m is: #Morph) ifTrue: [
				morphs add: m]]].
	^morphs
</details>

#### Text>>#runLengthFor: characterIndex

Answer the count of characters remaining in run beginning with characterIndex.


<details>
	<summary>See more</summary>
	
	runLengthFor: characterIndex 
	"Answer the count of characters remaining in run beginning with 
	characterIndex."

	^runs runLengthAt: characterIndex
</details>

#### Text>>#copyReplaceFrom: start to: stop with: aText

Answer a copy of the receiver satisfying the following conditions: + stop is less than start, then this is an insertion; stop should be exactly start-1, + start = 1 means insert before the first character, + start = size+1 means append after last character. + Otherwise, this is a replacement; start and stop have to be within the receiver's bounds.


<details>
	<summary>See more</summary>
	
	copyReplaceFrom: start to: stop with: aText

	^self copy replaceFrom: start to: stop with: aText
</details>

#### Text>>#colorAt: characterIndex

Answer the color for characters in the run beginning at characterIndex.


<details>
	<summary>See more</summary>
	
	colorAt: characterIndex
	"Answer the color for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ color ]
</details>

#### Text>>#find: attribute

Return the first interval over which this attribute applies


<details>
	<summary>See more</summary>
	
	find: attribute
	"Return the first interval over which this attribute applies"
	^ runs find: attribute
</details>

#### Text>>#color: aColor

Stuff like 'Hello world' blue edit


<details>
	<summary>See more</summary>
	
	color: aColor
	"Stuff like
	'Hello world' blue edit
	"
	self addAttribute: (TextColor color: aColor)
</details>

#### Text>>#font: aFont

Apply aFont to the entire contents. Note: use #baseFont. If emphasis is desired, add it separatedly.


<details>
	<summary>See more</summary>
	
	font: aFont
	"Apply aFont to the entire contents.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	self addAttribute: (TextFontFamilyAndSize
			familyName: aFont familyName
			pointSize: aFont pointSize)
</details>

#### Text>>#struck

Stuff like ('Hello world' struck ) edit


<details>
	<summary>See more</summary>
	
	struck
	"Stuff like
	('Hello world' struck ) edit
	"
	self addAttribute: TextEmphasis struckThrough
</details>

#### Text>>#red

Stuff like 'Hello world' red edit


<details>
	<summary>See more</summary>
	
	red
	"Stuff like
	'Hello world' red edit
	"
	self color: Color red
</details>

#### Text>>#replaceFrom: start to: stop with: replacement startingAt: repStart

This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. Do it to both the string and the runs. The size does not change


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| rep newRepRuns |
	rep _ replacement asText.	"might be a string"
	string replaceFrom: start to: stop with: rep string startingAt: repStart.
	newRepRuns _ rep runs copyFrom: repStart to: repStart + stop - start.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacement size - 1
</details>

#### Text>>#asNumber

Answer the number created by interpreting the receiver as the textual representation of a number.


<details>
	<summary>See more</summary>
	
	asNumber
	"Answer the number created by interpreting the receiver as the textual 
	representation of a number."

	^string asNumber
</details>

#### Text>>#leftFlush

Stuff like ('Hello world' leftFlush ) edit


<details>
	<summary>See more</summary>
	
	leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	self addAttribute: TextAlignment leftFlush
</details>

#### Text>>#appendToText: aText

<details>
	<summary>See more</summary>
	
	appendToText: aText

	| textSize start stop textResult |
	textSize := aText size.
	start := textSize + 1.
	stop := textSize.
	textResult := Text fromString: aText string , self string.
	textResult privateSetRuns: (aText runs copyReplaceFrom: start to: stop with: self runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	textResult fixParagraphAttributesFrom: start to: start + self size - 1.
	^ textResult
	
</details>

#### Text>>#findString: aString startingAt: start caseSensitive: caseSensitive

Answer the index of subString within the receiver, starting at index start. If the receiver does not contain subString, answer 0.


<details>
	<summary>See more</summary>
	
	findString: aString startingAt: start caseSensitive: caseSensitive
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start caseSensitive: caseSensitive
</details>

#### Text>>#customizeExplorerContents

<details>
	<summary>See more</summary>
	
	customizeExplorerContents

	^ false
</details>

#### Text>>#initialFont: aFont

Apply aFont to those parts that are not already specifying one. Note: use #baseFont. If emphasis is desired, add it separatedly.


<details>
	<summary>See more</summary>
	
	initialFont: aFont
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttr |
	fontAttr _ TextFontFamilyAndSize
			familyName: aFont familyName
			pointSize: aFont pointSize.
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes copyWith: fontAttr ]]
</details>

#### Text>>#withCuisLineEndings

Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>


<details>
	<summary>See more</summary>
	
	withCuisLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <CF> have been changed to <LF>"

	| newText wrongLineEnd |
	wrongLineEnd _ String crlfString detect: [ :char | (char = Character newLineCharacter) not ].
	(string includes: wrongLineEnd) ifFalse: [ ^self copy ].
	newText _ self copyReplaceAll: String crlfString with: String newLineString asTokens: false.
	(newText asString includes: wrongLineEnd) ifFalse: [ ^newText ].
	^newText copyReplaceAll: wrongLineEnd asString with: String newLineString asTokens: false.
</details>

#### Text>>#emphasisAt: characterIndex

Answer the emphasis for characters in the run beginning at characterIndex.


<details>
	<summary>See more</summary>
	
	emphasisAt: characterIndex
	"Answer the emphasis for characters in the run beginning at characterIndex."

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ emphasis ]
</details>

#### Text>>#pointSize: pointSize

Stuff like ('Hello World' pointSize: 22) edit


<details>
	<summary>See more</summary>
	
	pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	self addAttribute: (TextFontFamilyAndSize pointSize: pointSize)
</details>

#### Text>>#characterStyleOrNilIfApplying: textAttributes

Answer the ParagraphStyle for characters as specified by the argument.


<details>
	<summary>See more</summary>
	
	characterStyleOrNilIfApplying: textAttributes
	"Answer the ParagraphStyle for characters as specified by the argument."
	
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		^characterStyle ]
</details>

#### Text>>#centered

Stuff like ('Hello world' centered ) edit


<details>
	<summary>See more</summary>
	
	centered
	"Stuff like
	('Hello world' centered ) edit
	"
	self addAttribute: TextAlignment centered
</details>

#### Text>>#allBold

prefer shorter selector


<details>
	<summary>See more</summary>
	
	allBold
	"prefer shorter selector"
	self bold
</details>

#### Text>>#fixParagraphAttributesFrom: start to: end

Helper method to ensure the invariant that TextAttributes that answer true to #isParagraphAttribute are only applied to whole paragraphs. See senders


<details>
	<summary>See more</summary>
	
	fixParagraphAttributesFrom: start to: end
	"Helper method to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	See senders"
	
	| paragraphEnd paragraphInterval paragraphStart paragraphAttributes|
	self hasAnyParagraphAttribute ifFalse: [ ^self ].
	paragraphEnd _ end max: start.	"end could be start-1 when new text is empty, for example, when backspacing"
	[
		paragraphInterval _ self encompassParagraph: (paragraphEnd to: paragraphEnd).
		paragraphStart _ paragraphInterval first.
		paragraphEnd _ paragraphInterval last.
	
		"We must honor the paragraph attributes as defined in the newline (Lf) Character that ends the paragraph"
		paragraphAttributes _ (self attributesAt: paragraphEnd) select: [ :attr | attr isParagraphAttribute ].

		"if paragraphEnd is inside the interval just modified, and it doesn't bring any paragraph attributes (i.e., it doesn't end in CR),
		then try to keep the paragraph attributes previously in use in this paragraph..
		This is needed, for example, when pasting an image  or a plain text at the end of the document"
		(paragraphEnd = end and: [ paragraphStart < start and: [ end > 0 and: [ (string at: end) isLineSeparator not ]]]) ifTrue: [
			paragraphAttributes _ (self attributesAt: paragraphStart) select: [ :attr | attr isParagraphAttribute ]].

		self privateSetParagraphAttributes: paragraphAttributes from: paragraphStart to: paragraphEnd.
		paragraphEnd _ paragraphStart - 1.
		paragraphStart > start ] whileTrue.
	runs coalesce
</details>

#### Text>>#runs

<details>
	<summary>See more</summary>
	
	runs

	^runs
</details>

#### Text>>#embeddedMorphs

return the list of morphs embedded in me


<details>
	<summary>See more</summary>
	
	embeddedMorphs
	"return the list of morphs embedded in me"
	^self embeddedMorphsFrom: 1 to: self size
</details>

#### Text>>#= other

Am I equal to the other Text or String? ***** Warning ***** Two Texts are considered equal if they have the same characters in them. They might have completely different emphasis, fonts, sizes, text actions, or embedded morphs. If you need to find out if one is a true copy of the other, you must do (text1 = text2 and: [text1 runs = text2 runs]).


<details>
	<summary>See more</summary>
	
	= other
	"Am I equal to the other Text or String?  
	***** Warning ***** Two Texts are considered equal if they have the same characters in them.  They might have completely different emphasis, fonts, sizes, text actions, or embedded morphs.  If you need to find out if one is a true copy of the other, you must do (text1 = text2 and: [text1 runs = text2 runs])."

	self == other ifTrue: [ ^ true ].
	(other is: #Text) ifTrue: [ "This is designed to run fast even for megabytes"
		^ string == other string or: [string = other string]].
	other isString ifTrue: [^ string == other or: [string = other]].
	^ false
</details>

#### Text>>#hash

#hash is implemented, because #= is implemented. We are now equal to a string with the same characters. Hash must reflect that.


<details>
	<summary>See more</summary>
	
	hash
	"#hash is implemented, because #= is implemented.  We are now equal to a string with the same characters.  Hash must reflect that."

	^ string hash
</details>

#### Text>>#lineCount

<details>
	<summary>See more</summary>
	
	lineCount

	^ string lineCount
</details>

#### Text>>#italic

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis italic
</details>

#### Text>>#replaceFrom: start to: stop with: replacement

newSize = oldSize - (stop-start-1) + aText size


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement
	"newSize = oldSize - (stop-start-1) + aText size"
	
	| rep |
	rep _ replacement asText.	"might be a string"
	string _ string copyReplaceFrom: start to: stop with: rep string.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: rep runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacement size - 1
</details>

#### Text>>#initialFontFrom: someAttributes

Apply aFont to those parts that are not already specifying one. Note: use #baseFont. If emphasis is desired, add it separatedly.


<details>
	<summary>See more</summary>
	
	initialFontFrom: someAttributes
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttr |
	fontAttr _ someAttributes detect: [ :any | any isFont ] ifNone: [ ^ self ].
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes copyWith: fontAttr ]]
</details>

#### Text>>#commandForRemoveAttributes: attributesToRemove from: requestedStart to: requestedStop

Remove the attribute over the interval start to stop.


<details>
	<summary>See more</summary>
	
	commandForRemoveAttributes: attributesToRemove from: requestedStart to: requestedStop
	"Remove the attribute over the interval start to stop."

	| intervalToFix start stop new old |
	start _ requestedStart.
	stop _ requestedStop.
	
	"If att must be applied to whole paragraphs, do so."
	(attributesToRemove anySatisfy: [ :att | att isParagraphAttribute ]) ifTrue: [
		intervalToFix _ self encompassParagraph: (start to: stop).
		start _ intervalToFix first.
		stop _ intervalToFix last ].

	old _ runs copyFrom: start to: stop.
	new _ old copy mapValues: [ :attributes | 
			attributes copyWithoutAll: attributesToRemove].
	^AttributesReplaceCommand
		old: old
		new: new
		start: start
		stop: stop
</details>

#### Text>>#isRemote

<details>
	<summary>See more</summary>
	
	isRemote
	^false
</details>

#### Text>>#copyFrom: start to: stop

Answer a copied subrange of the receiver.


<details>
	<summary>See more</summary>
	
	copyFrom: start to: stop 
	"Answer a copied subrange of the receiver."

	| realStart realStop |
	stop > self size
		ifTrue: [realStop _ self size]		"handle selection at end of string"
		ifFalse: [realStop _ stop].
	start < 1
		ifTrue: [realStart _ 1]			"handle selection before start of string"
		ifFalse: [realStart _ start].
	^Text
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)
</details>

#### Text>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index

	^string at: index
</details>

#### Text>>#fontIfApplying: textAttributes default: defaultFont

Answer the font for characters as specified by the argument.


<details>
	<summary>See more</summary>
	
	fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	| fn ps |
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		fn _ familyName ifNil: [ defaultFont ifNotNil: [ defaultFont familyName ] ifNil: [ FontFamily defaultFamilyName ]].
		ps _ pointSize ifNil: [ defaultFont ifNotNil: [ defaultFont pointSize ] ifNil: [ FontFamily defaultPointSize ]].
		^((AbstractFont familyName: fn pointSize: ps) ifNil: [
			AbstractFont familyName: fn aroundPointSize: ps])
			emphasized: emphasis ]
</details>

#### Text>>#addAttribute: att

<details>
	<summary>See more</summary>
	
	addAttribute: att 
	string size = 0 ifTrue: [ ^self ].
	^ self addAttribute: att from: 1 to: self size
</details>

#### Text>>#edit

<details>
	<summary>See more</summary>
	
	edit

	self editLabel: 'Text Editor'
</details>

#### Text>>#hasAnyParagraphAttribute

Return false if there are no emphasis (i.e., a String would not make a difference)


<details>
	<summary>See more</summary>
	
	hasAnyParagraphAttribute
	"Return false if there are no emphasis (i.e., a String would not make a difference)"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isParagraphAttribute ]]
</details>

#### Text>>#string

Answer the string representation of the receiver.


<details>
	<summary>See more</summary>
	
	string
	"Answer the string representation of the receiver."

	^string
</details>

#### Text>>#asString

Answer a String representation of the textual receiver.


<details>
	<summary>See more</summary>
	
	asString
	"Answer a String representation of the textual receiver."

	^string
</details>

#### Text>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)
</details>

#### Text>>#add: newObject

Adding to an Interval is not allowed.


<details>
	<summary>See more</summary>
	
	add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement
</details>

#### Text>>#blue

Stuff like 'Hello world' blue edit


<details>
	<summary>See more</summary>
	
	blue
	"Stuff like
	'Hello world' blue edit
	"
	self color: Color blue
</details>

#### Text>>#removeAttributesThat: aBlock

<details>
	<summary>See more</summary>
	
	removeAttributesThat: aBlock
	runs mapValues: [ :attributes |
		attributes reject: aBlock ]
</details>

#### Text>>#privateSetRuns: anArray

Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to #isParagraphAttribute are only applied to whole paragraphs. Use with care. Currently only used for Shout, that seems to know what it does. Also used for private use, replacing asignment to the ivar, to ensure that the RunArray is set to properly compare TextAttributes


<details>
	<summary>See more</summary>
	
	privateSetRuns: anArray
	"Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	Use with care. Currently only used for Shout, that seems to know what it does.
	Also used for private use, replacing asignment to the ivar, to ensure that the RunArray is set to properly compare TextAttributes"

	runs _ anArray.
	runs canJoinMessage: (MessageSend receiver: self selector: #canJoin:and:)
</details>

#### Text>>#super

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis superscript
</details>

#### Text>>#encompassLine: anInterval

<details>
	<summary>See more</summary>
	
	encompassLine: anInterval
	^string encompassLine: anInterval
</details>

#### Text>>#addAttribute: aTextAttribute from: requestedStart to: requestedStop

Set the attribute for characters in the interval start to stop. Turned into a command to enable reuse by undo / redo


<details>
	<summary>See more</summary>
	
	addAttribute: aTextAttribute from: requestedStart to: requestedStop
	"Set the attribute for characters in the interval start to stop.
	Turned into a command to enable reuse by undo / redo"
	(self
		commandForAddAttribute: aTextAttribute
		from: requestedStart
		to: requestedStop) doOn: self
</details>

#### Text>>#setString: aString setRuns: anArray

Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to #isParagraphAttribute are only applied to whole paragraphs. Use with care.


<details>
	<summary>See more</summary>
	
	setString: aString setRuns: anArray
	"Warning. No attempt is done to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	Use with care. "

	string _ aString.
	self privateSetRuns: anArray
</details>

#### Text>>#magenta

Stuff like 'Hello world' magenta edit


<details>
	<summary>See more</summary>
	
	magenta
	"Stuff like
	'Hello world' magenta edit
	"
	self color: Color magenta
</details>

#### Text>>#rangeOf: attribute startingAt: index

Answer an interval that gives the range of attribute at index position index. An empty interval with start value index is returned when the attribute is not present at position index.


<details>
	<summary>See more</summary>
	
	rangeOf: attribute startingAt: index
"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index.  "
   ^string size = 0
      ifTrue: [index to: index - 1]
	 ifFalse: [runs rangeOf: attribute startingAt: index]
</details>

#### Text>>#appendToString: aString

<details>
	<summary>See more</summary>
	
	appendToString: aString

	| stringSize |
	stringSize := aString size.
	^ (aString asText) 
			replaceFrom: stringSize + 1
			to: stringSize 
			with: self
</details>

#### Text>>#displayStringOrText

To be used in the UI


<details>
	<summary>See more</summary>
	
	displayStringOrText
	"To be used in the UI"
	"Answer the receiver itself."

	^self
</details>

#### Text>>#characterStyleOrNilAt: characterIndex

<details>
	<summary>See more</summary>
	
	characterStyleOrNilAt: characterIndex

	self
		withAttributeValues: (self attributesAt: characterIndex)
		do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
			^ characterStyle ]
</details>

#### Text>>#cyan

Stuff like 'Hello world' cyan edit


<details>
	<summary>See more</summary>
	
	cyan
	"Stuff like
	'Hello world' cyan edit
	"
	self color: Color cyan
</details>

#### Text>>#basicReplaceAttributesFrom: start to: stop with: replacement

Private. Does not enforce invariants. replacement size = (stop-start-1)


<details>
	<summary>See more</summary>
	
	basicReplaceAttributesFrom: start to: stop with: replacement
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	self privateSetRuns: (runs basicReplaceAttributesFrom: start to: stop with: replacement)
</details>

#### Text>>#prepend: stringOrText

<details>
	<summary>See more</summary>
	
	prepend: stringOrText

	self replaceFrom: 1 to: 0 with: stringOrText
</details>

#### Text>>#under

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis underlined
</details>

#### Text>>#withAttributeValues: attributes do: aBlock

Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order The order is (each overwriting the previous one) 1) basic defaults 2) ParagraphStyleReferene 3) CharacterStyleReference 4) TextFontReference 5) TextEmphasis


<details>
	<summary>See more</summary>
	
	withAttributeValues: attributes do: aBlock
	"Evaluate aBlock with the values of various attributes that affect text formatting, applied in the correct order
	The order is (each overwriting the previous one)
	1) basic defaults
	2) ParagraphStyleReferene
	3) CharacterStyleReference
	4) TextFontReference
	5) TextEmphasis"

	| paragraphStyle characterStyle familyName pointSize emphasis alignment color backgroundColor |
	paragraphStyle _ nil.
	characterStyle _ nil.
	familyName _ nil.
	pointSize _ nil.
	emphasis _ 0.
	alignment _ 0.
	color _ nil.
	backgroundColor _ nil.
	
	"ParagraphStyle is the first to set several values"
	attributes do: [ :attribute |
		attribute forParagraphStyleReferenceDo: [ :s |
			paragraphStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			alignment _ s alignment.
			s color ifNotNil: [ :c | color _ c ]]].

	"CharacterStyle, if present, can override font and color"
	attributes do: [ :attribute |
		attribute forCharacterStyleReferenceDo: [ :s |
			characterStyle _ s.
			familyName _ s familyName.
			pointSize _ s pointSize.
			emphasis _ s emphasis.
			s color ifNotNil: [ :c | color _ c ]]].

	"These will not interfere with each other, and all of them take precedence over previous values"
	attributes do: [ :attribute |
		attribute forFontFamilyAndSizeDo: [ :fn :ps | familyName _ fn. pointSize _ ps ].
		attribute forTextEmphasisDo: [ :e | emphasis _ emphasis bitOr: e ].
		attribute forTextColorDo: [ :c | color _ c ].
		attribute forTextBackgroundColorDo: [ :c | backgroundColor _ c ].
		attribute forTextAlignmentDo: [ :a | alignment _ a ].
	].
	
	"Done. Now evaluate the block."
	^aBlock valueWithArguments: { familyName. pointSize. emphasis. color. alignment. characterStyle. paragraphStyle. backgroundColor }
</details>

#### Text>>#sub

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	self addAttribute: TextEmphasis subscript
</details>

#### Text>>#, aStringOrText

Concatenate two Strings or Collections.


<details>
	<summary>See more</summary>
	
	, aStringOrText

	^ aStringOrText appendToText: self
</details>

## TextCommand

My instances are user commands that can be undone and redone.

### Methods
#### TextCommand>>#undoOn: aTextModel

Undo the command, bringing the text model to the state it had prior to doing it


<details>
	<summary>See more</summary>
	
	undoOn: aTextModel
	"Undo the command, bringing the text model to the state it had prior to doing it"
	^nil
</details>

#### TextCommand>>#doOn: aTextModel

Perform the command, used for initial execution or for redo after undoing


<details>
	<summary>See more</summary>
	
	doOn: aTextModel
	"Perform the command, used for initial execution or for redo after undoing"
	^nil
</details>

## TextEditor

See comment in Editor. My instances edit Text, this is, they support multiple lines and TextAttributes. They have no specific facilities for editing Smalltalk code. Those are found in SmalltalkEditor.

### Methods
#### TextEditor>>#bindingOf: aString

<details>
	<summary>See more</summary>
	
	bindingOf: aString
	^model bindingOf: aString
</details>

#### TextEditor>>#addCutAndPasteMenuSectionTo: aMenu

Adds typical cut and paste operations section to a menu


<details>
	<summary>See more</summary>
	
	addCutAndPasteMenuSectionTo: aMenu
	"Adds  typical cut and paste operations section to a menu"
	
	self hasUnacceptedEdits ifTrue: [
		aMenu
			add: 'Accept (s)'
			action: #acceptContents
			icon: #acceptIcon
	].
	
	aMenu
		add: 'Copy (c)'
		action: #copySelection
		icon: #copyIcon.
	
	aMenu
		add: 'Cut (x)'
		action: #cut
		icon: #cutIcon.

	aMenu
		add: 'Paste (v)'
		action: #paste
		icon: #pasteIcon.
		
	aMenu
		add: 'Paste without Format'
		action: #pasteString
		icon: #pasteIcon.
	
	aMenu
		add: 'Paste...'
		action: #pasteRecent
		icon: #worldIcon.
	
	^aMenu
</details>

#### TextEditor>>#changeEmphasisOrAlignment

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough superscript subscript withST80Glyphs) collect: [ :emph |
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) withIndexCollect: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough superscript subscript withST80Glyphs leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 8) selections: aList) startUpMenu.
	reply ifNotNil: [
		(#(leftFlush centered rightFlush justified) includes: reply)
			ifTrue: [
				attribute _ TextAlignment perform: reply]
			ifFalse: [
				attribute _ TextEmphasis perform: reply].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	^ true
</details>

#### TextEditor>>#offerColorMenu

Present a menu of available colors, and if one is chosen, apply it to the current selection.


<details>
	<summary>See more</summary>
	
	offerColorMenu
	"Present a menu of available colors, and if one is chosen, apply it to the current selection."
	"This is a user command, and generates undo"

	| attribute colors index thisSel |
	thisSel _ self selection.
	colors _ #(#black #magenta #red #yellow #green #blue #cyan #white ).
	index _ (PopUpMenu
		labelArray: colors , #('choose color...' )
		lines: (Array with: colors size + 1)) startUpMenu.
	index = 0 ifTrue: [ ^ true ].
	index <= colors size
		ifTrue: [ attribute _ TextColor color: (Color perform: (colors at: index)) ]
		ifFalse: [
			index _ index - colors size - 1.
			"Re-number!!!"
			index = 0 ifTrue: [ attribute _ self chooseColor ].
			thisSel ifNil: [ ^ true ]].
	attribute ifNotNil: [ self applyAttribute: attribute ].
	^ true.
</details>

#### TextEditor>>#pointIndex

<details>
	<summary>See more</summary>
	
	pointIndex
	pointBlock ifNil: [^1].
	^ pointBlock stringIndex
</details>

#### TextEditor>>#selectionIntervalsDo: aBlock

Evaluate over all selections. For editors with a single selection, just evaluate over it. Subclasses with multi-selection redefine this method.


<details>
	<summary>See more</summary>
	
	selectionIntervalsDo: aBlock
	"Evaluate over all selections. For editors with a single selection, just evaluate over it.
	Subclasses with multi-selection redefine this method."

	"multi-selection"
	selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
		aBlock value: (startBlock stringIndex to: stopBlock stringIndex - 1) ].

	"Simple (i.e. last) selection"
	aBlock value: self selectionInterval
</details>

#### TextEditor>>#addAttributesForPasting: replacement

<details>
	<summary>See more</summary>
	
	addAttributesForPasting: replacement
	^ (replacement is: #Text)
		ifTrue: [ replacement ]
		ifFalse: [
			Text
				string: replacement
				attributes: emphasisHere ]
</details>

#### TextEditor>>#doubleClickAndHalf

<details>
	<summary>See more</summary>
	
	doubleClickAndHalf

	| here interval |
	here _ self pointIndex.
	interval _ self privateCurrentString encompassParagraph: (here to: here).
	self selectFrom: interval first to: interval last.

	doWordSelection _ false.
	doParagraphSelection _ true.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock
</details>

#### TextEditor>>#moveCursor: directionBlock forward: forward event: aKeyboardEvent

Private - Move cursor. directionBlock is a one argument Block that computes the new Position from a given one.


<details>
	<summary>See more</summary>
	
	moveCursor: directionBlock forward: forward event: aKeyboardEvent

	super moveCursor: directionBlock forward: forward event: aKeyboardEvent.
	self setEmphasisHereFromTextForward: forward
</details>

#### TextEditor>>#messageSendsRanges: aRanges

aRanges must be notEmpty


<details>
	<summary>See more</summary>
	
	messageSendsRanges: aRanges
	"aRanges must be notEmpty"
	| lastRange |

	selectionStartBlocks := OrderedCollection new.
	selectionStopBlocks := OrderedCollection new.
	lastRange := nil.

	aRanges do: [ :range |
		selectionStartBlocks add: (textComposition characterBlockForIndex: range first).
		selectionStopBlocks add: (textComposition characterBlockForIndex: range last + 1).
		( lastRange isNil or: [ range first > lastRange first ]) ifTrue: [
			lastRange _ range ]].
	self selectFrom: lastRange first to: lastRange last
</details>

#### TextEditor>>#copyHiddenInfo

In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info. Copy that to the clipboard. You can paste it and see what it is. Usually enclosed in <>.


<details>
	<summary>See more</summary>
	
	copyHiddenInfo
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden
info.  Copy that to the clipboard.  You can paste it and see what it is.
Usually enclosed in <>."

	^ self clipboardTextPut: self hiddenInfo
</details>

#### TextEditor>>#unapplyAttributes: textAttributes

The user selected textAttributes to be removed. If there is a selection, unapply the attributes to the selection. In any case do not use the attribute for the user input (emphasisHere)


<details>
	<summary>See more</summary>
	
	unapplyAttributes: textAttributes
	"The user selected textAttributes to be removed.
	If there is a selection, unapply the attributes to the selection.
	In any case do not use the attribute for the user input (emphasisHere)"
	"This generates undo"

	| interval |
	emphasisHere _ emphasisHere copyWithoutAll: textAttributes.
	interval _ self selectionInterval.
	(interval isEmpty and: [ textAttributes noneSatisfy: [ :each | each isParagraphAttribute ]])
		ifTrue: [ ^self ].
	model logUndoAndRemoveAttributes: textAttributes from: interval first to: interval last.
	textComposition recomposeFrom: interval first to: interval last delta: 0.
	self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
	morph possiblyChanged
</details>

#### TextEditor>>#align: aKeyboardEvent

Triggered by Cmd-u; cycle through alignment alternatives. 8/11/96 sw


<details>
	<summary>See more</summary>
	
	align: aKeyboardEvent
	"Triggered by Cmd-u;  cycle through alignment alternatives.  8/11/96 sw"

	self align.
	^ true
</details>

#### TextEditor>>#lastFont

Answer the Font for to be used if positioned at the end of the text


<details>
	<summary>See more</summary>
	
	lastFont
	"Answer the Font for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents fontIfApplying: emphasisHere default: defaultFont ]
		ifFalse: [ model actualContents fontAt: model textSize +1 default: defaultFont ]
</details>

#### TextEditor>>#makeUppercase: aKeyboardEvent

Force the current selection to uppercase. Triggered by Cmd-Y.


<details>
	<summary>See more</summary>
	
	makeUppercase: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-Y."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asUppercase.
	^ true
</details>

#### TextEditor>>#blinkParen

Used if Shout


<details>
	<summary>See more</summary>
	
	blinkParen
	"Used if Shout"
	lastParenLocation ifNotNil: [
		model textSize >= lastParenLocation ifTrue: [
			model privateAddBoldAt: lastParenLocation ]]
</details>

#### TextEditor>>#removeMorph: aMorph

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	removeMorph: aMorph
	"This is a user command, and generates undo"

	| range |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	recreate the morph nor add it to the TextMorph! (See senders of #insertMorph:at:
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	range _ model actualContents find: (TextAnchor new anchoredFormOrMorph: aMorph).
	range ifNotNil: [
		model logUndoAndReplaceFrom: range first to: range last with: Text new.
		textComposition recomposeFrom: range first to: range first  -1 delta: range size negated ]
</details>

#### TextEditor>>#indent: aKeyboardEvent

Add a tab at the front of every line occupied by the selection. Invoked from keyboard via cmd-shift-R. 2/29/96 sw


<details>
	<summary>See more</summary>
	
	indent: aKeyboardEvent
	"Add a tab at the front of every line occupied by the selection. Invoked from keyboard via cmd-shift-R.  2/29/96 sw"

	^ self inOutdent: aKeyboardEvent delta: 1
</details>

#### TextEditor>>#makeLowercase: aKeyboardEvent

Force the current selection to lowercase. Triggered by Cmd-X.


<details>
	<summary>See more</summary>
	
	makeLowercase: aKeyboardEvent
	"Force the current selection to lowercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asLowercase.
	^ true
</details>

#### TextEditor>>#blinkPrevParen: aCharacter

Used if not Shout


<details>
	<summary>See more</summary>
	
	blinkPrevParen: aCharacter
	"Used if not Shout"
	| closingDelimiter openingDelimiter level string here hereChar |
	string _ self privateCurrentString.
	here _ pointBlock stringIndex.
	closingDelimiter _ aCharacter.
	openingDelimiter _ '([{' at: (')]}' indexOf: closingDelimiter).
	level _ 1.
	[ level > 0 and: [ here > 1 ]]
		whileTrue: [
			hereChar _ string at: (here _ here - 1).
			hereChar = openingDelimiter
				ifTrue: [
					level _ level - 1.
					level = 0
						ifTrue: [^ self blinkParenAt: here]]
				ifFalse: [
					hereChar = closingDelimiter
						ifTrue: [ level _ level + 1]]]
</details>

#### TextEditor>>#wrapOnOff

Toggle WordWrap


<details>
	<summary>See more</summary>
	
	wrapOnOff
	"Toggle WordWrap"

	morph wrapOnOff
</details>

#### TextEditor>>#recomputeSelection

The same characters are selected but their coordinates may have changed.


<details>
	<summary>See more</summary>
	
	recomputeSelection
	"The same characters are selected but their coordinates may have changed."

	selectionStartBlocks _ selectionStartBlocks collect: [ :block | textComposition characterBlockForIndex: block stringIndex ].
	selectionStopBlocks _ selectionStopBlocks collect: [ :block | textComposition characterBlockForIndex: block stringIndex ].
	self markIndex: self markIndex pointIndex: self pointIndex
</details>

#### TextEditor>>#lineSelectAndEmptyCheck: returnBlock

If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.


<details>
	<summary>See more</summary>
	
	lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	(self hasSelection or: [ selectionStartBlocks notEmpty]) ifFalse: [morph flash.  ^ returnBlock value]
</details>

#### TextEditor>>#backTo: startIndex

During typing, backspace to startIndex.


<details>
	<summary>See more</summary>
	
	backTo: startIndex
	"During typing, backspace to startIndex."
	"This is a user command, and generates undo"

	self markIndex: startIndex.
	self replaceSelectionWith: self nullText.
	markBlock _ pointBlock
</details>

#### TextEditor>>#dispatchOn: aKeyboardEvent

Carry out the action associated with this character, if any.


<details>
	<summary>See more</summary>
	
	dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| asciiValue c |
	self clearParens.
  	asciiValue _ aKeyboardEvent keyValue.
	"Control keys are handled by #shortcuts even if they have any modifiers"
	(self shouldHandleUsingCmdShortcuts: aKeyboardEvent) ifTrue: [
		^self perform: (self cmdShortcuts at: asciiValue + 1) with: aKeyboardEvent ].

	c _ aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	^ self perform: (self shortcuts at: asciiValue + 1) with: aKeyboardEvent
</details>

#### TextEditor>>#mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	aMouseButtonEvent shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(textComposition characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock _ textComposition characterBlockAtPoint: localEventPosition.
			cursorIndex _ cursorBlock stringIndex.
			startBlock _ self startBlock min: cursorBlock.
			startIndex _ startBlock stringIndex.
			stopBlock _ self stopBlock max: cursorBlock.
			stopIndex _ stopBlock stringIndex.
			(stopIndex - cursorIndex) < (cursorIndex - startIndex)
				ifTrue: [
					markBlock _ startBlock.
					pointBlock _ cursorBlock ]
				ifFalse: [
					markBlock _ stopBlock.
					pointBlock _ cursorBlock ]].
	self storeSelectionInComposition
</details>

#### TextEditor>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.
	defaultFont _ AbstractFont default
</details>

#### TextEditor>>#markIndex

<details>
	<summary>See more</summary>
	
	markIndex
	markBlock ifNil: [^1].
	^ markBlock stringIndex
</details>

#### TextEditor>>#outdent: aKeyboardEvent

Remove a tab from the front of every line occupied by the selection. Invoked from keyboard via cmd-shift-L. 2/29/96 sw


<details>
	<summary>See more</summary>
	
	outdent: aKeyboardEvent
	"Remove a tab from the front of every line occupied by the selection.
	Invoked from keyboard via cmd-shift-L.  2/29/96 sw"

	^ self inOutdent: aKeyboardEvent delta: -1
</details>

#### TextEditor>>#cursorEnd: aKeyboardEvent

Private - Move cursor end of current line.


<details>
	<summary>See more</summary>
	
	cursorEnd: aKeyboardEvent 

	"Private - Move cursor end of current line."

	self
		moveCursor: [ :position |
			"Mac standard keystrole"		
			(aKeyboardEvent commandAltKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ self endOfText ]
					ifFalse: [ self endOfLine: position ]]
		forward: true
		event: aKeyboardEvent.
	^true
</details>

#### TextEditor>>#findAgain: aKeyboardEvent

Find the desired text again. 1/24/96 sw


<details>
	<summary>See more</summary>
	
	findAgain: aKeyboardEvent 
	"Find the desired text again.  1/24/96 sw"

	self findAgain.
	^ true
</details>

#### TextEditor>>#addString: aString

Think of a better name


<details>
	<summary>See more</summary>
	
	addString: aString
	"Think of a better name"
	"This is a user command, and generates undo"

	self replaceSelectionWith: aString
</details>

#### TextEditor>>#find

Prompt the user for a string to search for, and search the receiver from the current selection onward for it. 1/26/96 sw


<details>
	<summary>See more</summary>
	
	find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	| reply |
	reply _ FillInTheBlankMorph request: 'Find what? ' initialAnswer: self class findText.
	"Set focus on our text morph, so that cmd-g does the search again"
	morph world activeHand newKeyboardFocus: morph.
	reply size = 0 ifTrue: [
		^ self].
	self setSearch: reply.
	ChangeText _ self class findText.  "Implies no replacement to againOnce: method"
	(self findAndReplaceMany: false)
		ifFalse: [ self flash ].

"	morph installEditorToReplace: self"
</details>

#### TextEditor>>#selectionAsStream

Answer a ReadStream on the text that is currently selected. Note: Only considers single selection. See #selection to see how we handle multiple selection.


<details>
	<summary>See more</summary>
	
	selectionAsStream
	"Answer a ReadStream on the text that is currently  selected.
	Note: Only considers single selection. See #selection to see how we handle multiple selection."

	^ReadWriteStream
		on: self privateCurrentString
		from: self startIndex
		to: self stopIndex - 1
</details>

#### TextEditor>>#redo: aKeyboardEvent

Redo the last edit.


<details>
	<summary>See more</summary>
	
	redo: aKeyboardEvent 
	"Redo the last edit."

	self redo.
	^true
</details>

#### TextEditor>>#selection

Answer the text that is currently selected.


<details>
	<summary>See more</summary>
	
	selection
	"Answer the text that is currently selected."

	| t firstIndex lastIndex |
	t _ model actualContents.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	(firstIndex = 1 and: [ lastIndex = t size ])
		ifTrue: [ ^t copy ].
	^Text streamContents: [ :strm |
		"Multiple selection"
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |
			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.
			toAppend size > 0 ifTrue: [
				strm nextPutAll: toAppend.
				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]]].
		"Regular selection"
		strm nextPutAll: ( t copyFrom: firstIndex to: lastIndex ) ]
</details>

#### TextEditor>>#cut

Cut out the current selection and redisplay if necessary.


<details>
	<summary>See more</summary>
	
	cut
	"Cut out the current selection and redisplay if necessary."
	"This is a user command, and generates undo"

	| multiSelection |
	selectionStartBlocks ifEmpty: [
		self lineSelectAndEmptyCheck: [^ self]].

	multiSelection _ self selection.
	self zapMultiSelection.
	self replaceSelectionWith: self nullText.
	self clipboardTextPut: multiSelection.
</details>

#### TextEditor>>#beginningOfLine: position

Redefined in subclasses using TextComposition


<details>
	<summary>See more</summary>
	
	beginningOfLine: position
	"Redefined in subclasses using TextComposition"
	^ (textComposition lines at: (textComposition lineIndexFor: position)) first
</details>

#### TextEditor>>#insertAndSelect: aString at: anInteger

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	insertAndSelect: aString at: anInteger
	"This is a user command, and generates undo"

	| newText |
	newText _ (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText.
	self selectFrom: anInteger to: anInteger + newText size - 1
</details>

#### TextEditor>>#insertMorph: aMorph at: relPt

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	insertMorph: aMorph at: relPt
	"This is a user command, and generates undo"
	| index newText |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	kill the morph nor send it to another owner! See implementors of #removeMorph: in TextMorphs.
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	index _ (textComposition characterBlockAtPoint: relPt) stringIndex.
	newText _ ' ', (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: aMorph)), ' '.
	model logUndoAndReplaceFrom: index to: index-1 with: newText.
	textComposition recomposeFrom: index to: index + newText size -1 delta: newText size.
</details>

#### TextEditor>>#makeCapitalized: aKeyboardEvent

Force the current selection to uppercase. Triggered by Cmd-X.


<details>
	<summary>See more</summary>
	
	makeCapitalized: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	| prev |
	prev _ $-.  "not a letter"
	self replaceSelectionWith: 
		(self selectedString collect:
			[:c | prev _ prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
	^ true
</details>

#### TextEditor>>#flushUndoRedoCommands

<details>
	<summary>See more</summary>
	
	flushUndoRedoCommands
	model flushUndoRedoCommands
</details>

#### TextEditor>>#mouseMove: aMouseMoveEvent localPosition: localEventPosition

Change the selection in response to mouse-down drag


<details>
	<summary>See more</summary>
	
	mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| newPointBlock goingBackwards newStartBlock newStopBlock interval i1 i2 |
	newPointBlock _ textComposition characterBlockAtPoint: localEventPosition.
	goingBackwards _ newPointBlock stringIndex < markBlock stringIndex.

	doWordSelection ifTrue: [
		pointBlock _ newPointBlock.
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		newStartBlock _ self startBlock min: initialSelectionStart.
		newStopBlock _ self stopBlock max: initialSelectionStop.
		markBlock _ goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock _ goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	doParagraphSelection ifTrue: [
		i1 _ newPointBlock stringIndex min: initialSelectionStart stringIndex.
		i2 _ newPointBlock stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		newStartBlock _ self startBlock min: initialSelectionStart.
		newStopBlock _ self stopBlock max: initialSelectionStop.
		markBlock _ goingBackwards ifTrue: [newStopBlock] ifFalse: [newStartBlock].
		pointBlock _ goingBackwards ifTrue: [newStartBlock] ifFalse: [newStopBlock].
		self storeSelectionInComposition.
		^self ].

	pointBlock _ newPointBlock.
	self storeSelectionInComposition
</details>

#### TextEditor>>#acceptContents

Save the current text of the text being edited as the current acceptable version for purposes of canceling. Allow my morph to take appropriate action


<details>
	<summary>See more</summary>
	
	acceptContents
	"Save the current text of the text being edited as the current acceptable version for purposes of canceling.  Allow my morph to take appropriate action"
	^morph acceptContents
</details>

#### TextEditor>>#inOutdent: aKeyboardEvent delta: delta

Add/remove a tab at the front of every line occupied by the selection. Derived from work by Larry Tesler back in December 1985. Now triggered by Cmd-L and Cmd-R. 2/29/96 sw


<details>
	<summary>See more</summary>
	
	inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection. 
	Derived from work by Larry Tesler back in December 1985.  Now triggered by Cmd-L and Cmd-R.  2/29/96 sw"
	"This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart indentation size numLines inStream newString outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	"Find the indentation of the least-indented non-blank line; never outdent more"
	indentation _ (startLine to: stopLine) inject: 1000 into: [ :previousValue :each |
		previousValue min: (textComposition indentationOfLineIndex: each ifBlank: [ :tabs | 1000 ])].			

	size _  stop + 1 - start.
	numLines _ stopLine + 1 - startLine.
	inStream _ ReadStream on: self privateCurrentString from: start to: stop.

	newString _ String new: size + ((numLines * delta) max: 0).
	outStream _ ReadWriteStream on: newString.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	indentation _ indentation + delta.
	size _ outStream position.
	inStream _ outStream setFrom: 1 to: size.

	outStream
		ifNil: [ 	"tried to outdent but some line(s) were already left flush"
			morph flash]
		ifNotNil: [
			self selectInvisiblyFrom: start to: stop.
			size = newString size ifFalse: [newString _ outStream contents].
			self replaceSelectionWith: newString].
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true
</details>

#### TextEditor>>#compareToClipboard: aKeyboardEvent

Compare the receiver to the text on the clipboard.


<details>
	<summary>See more</summary>
	
	compareToClipboard: aKeyboardEvent
	"Compare the receiver to the text on the clipboard."

	self compareToClipboard.
	^ true
</details>

#### TextEditor>>#wordUnderCursor

<details>
	<summary>See more</summary>
	
	wordUnderCursor
	
	^self wordUnder: self pointIndex
</details>

#### TextEditor>>#firstOfBeginningOfLineOrEndOfIndentationLeftOf: position

Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular). This way, if the given position is beginning-of-line then end-of-indentation is returned.


<details>
	<summary>See more</summary>
	
	firstOfBeginningOfLineOrEndOfIndentationLeftOf: position
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine _ textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine _ currentLine first.
	endOfIndentation _ self privateCurrentString
		skipDelimiters: (String with: Character tab)
		startingAt: beginningOfLine.
		
	stops _ OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]
</details>

#### TextEditor>>#pointIndex: anIndex

Called, for example, when selecting text with shift+arrow keys


<details>
	<summary>See more</summary>
	
	pointIndex: anIndex
	"Called, for example, when selecting text with shift+arrow keys"
	pointBlock _ textComposition characterBlockForIndex: anIndex
</details>

#### TextEditor>>#currentParagraphStyle

Answer the ParagraphStyle for the current selection or cursor location if any


<details>
	<summary>See more</summary>
	
	currentParagraphStyle
	"Answer the ParagraphStyle for the current selection or cursor location if any"
	
	| i |
	i _ self startIndex.
	^i > model textSize
		ifTrue: [ model actualContents paragraphStyleOrNilIfApplying: emphasisHere ]
		ifFalse: [ model actualContents paragraphStyleOrNilAt: i ]
</details>

#### TextEditor>>#markIndex: anIndex

Called, for example, when selecting text with shift+arrow keys


<details>
	<summary>See more</summary>
	
	markIndex: anIndex
	"Called, for example, when selecting text with shift+arrow keys"
	markBlock _ (textComposition characterBlockForIndex: anIndex)

</details>

#### TextEditor>>#enclose: aKeyboardEvent

Insert or remove bracket characters around the current selection.


<details>
	<summary>See more</summary>
	
	enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (String with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (String with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true
</details>

#### TextEditor>>#model: aModel

<details>
	<summary>See more</summary>
	
	model: aModel
	model _ aModel
</details>

#### TextEditor>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	| b |

	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.

	"Multiple selection of text.
	Windows uses Control, Mac uses Command (i.e. commandAlt)
	On the Mac, command-button1 is translated to command-button3 by the VM. do:
		Preferences disable: #commandClickOpensHalo
	to disable this behavior and make command-button1 work for multiple selection. "
	(aMouseButtonEvent controlKeyPressed or: [ aMouseButtonEvent commandAltKeyPressed ]) ifTrue: [
		self selectionInterval size > 0 ifTrue: [
			selectionStartBlocks _ selectionStartBlocks copyWith: self startBlock.
			selectionStopBlocks _ selectionStopBlocks copyWith: self stopBlock ]]
	ifFalse: [
		selectionStartBlocks _ #().
		selectionStopBlocks _ #() ].

	b _ textComposition characterBlockAtPoint: localEventPosition.

	(textComposition clickAt: localEventPosition) ifTrue: [
		markBlock _ b.
		pointBlock _ b.
		aMouseButtonEvent hand releaseKeyboardFocus: self.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			(self markIndex = b stringIndex and: [ self pointIndex = b stringIndex ])
				ifTrue: [
					markBlock _ b.
					pointBlock _ b ]
				ifFalse: [
					markBlock _ b.
					pointBlock _ b.	
					self setEmphasisHereFromText ]]
</details>

#### TextEditor>>#setSearch: aStringOrText

Set the FindText and ChangeText to seek aString; except if already seeking aString, leave ChangeText alone so again will repeat last replacement.


<details>
	<summary>See more</summary>
	
	setSearch: aStringOrText
	"Set the FindText and ChangeText to seek aString; except if already seeking aString, leave ChangeText alone so again will repeat last replacement."

	self class findText = aStringOrText
		ifFalse: [FindText _ ChangeText _ aStringOrText]
</details>

#### TextEditor>>#startBlock

<details>
	<summary>See more</summary>
	
	startBlock
	^ pointBlock min: markBlock
</details>

#### TextEditor>>#lastParagraphStyleOrNil

Answer the ParagraphStyle for to be used if positioned at the end of the text


<details>
	<summary>See more</summary>
	
	lastParagraphStyleOrNil
	"Answer the ParagraphStyle for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents paragraphStyleOrNilIfApplying: emphasisHere ]
		ifFalse: [ model actualContents paragraphStyleOrNilAt: model textSize +1 ]
</details>

#### TextEditor>>#characterBlockForIndex: anIndex

<details>
	<summary>See more</summary>
	
	characterBlockForIndex: anIndex

	^ textComposition characterBlockForIndex: anIndex 
</details>

#### TextEditor>>#markIndex: anIndex pointIndex: anotherIndex

Called, for example, when selecting text with shift+arrow keys


<details>
	<summary>See more</summary>
	
	markIndex: anIndex pointIndex: anotherIndex
	"Called, for example, when selecting text with shift+arrow keys"
	markBlock _ textComposition characterBlockForIndex: anIndex.
	pointBlock _ textComposition characterBlockForIndex: anotherIndex
</details>

#### TextEditor>>#nullText

<details>
	<summary>See more</summary>
	
	nullText

	^Text string: '' attributes: emphasisHere
</details>

#### TextEditor>>#tabKey: aKeyboardEvent

Add/remove a tab at the front of every line occupied by the selection if there is one; treat as a normal character otherwise.


<details>
	<summary>See more</summary>
	
	tabKey: aKeyboardEvent
	"Add/remove a tab at the front of every line occupied by the selection if there is one; treat as a normal character otherwise."
	
	aKeyboardEvent shiftPressed 
		ifTrue: [ ^ self outdent: aKeyboardEvent ].
	
	^ self hasSelection 
		ifTrue: [ self indent: aKeyboardEvent ]
		ifFalse: [ self normalCharacter: aKeyboardEvent ]
</details>

#### TextEditor>>#changeEmphasis: aKeyboardEvent

Change the emphasis of the current selection or prepare to accept characters with the change in emphasis.


<details>
	<summary>See more</summary>
	
	changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('01234567'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 7) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough #superscript #subscript #withST80Glyphs) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttributes: {attributeToRemove} ].
	^ true
</details>

#### TextEditor>>#setSearchString

Make the current selection, if any, be the current search string.


<details>
	<summary>See more</summary>
	
	setSearchString
	"Make the current selection, if any, be the current search string."
	self hasSelection ifFalse: [morph flash. ^ self].
	self setSearch: self selectedString
</details>

#### TextEditor>>#hiddenInfo

In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info. Return the entire string that was used by Cmd-6 to create this text attribute. Usually enclosed in < >.


<details>
	<summary>See more</summary>
	
	hiddenInfo
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info.  Return the entire string that was used by Cmd-6 to create this text attribute.  Usually enclosed in < >."

	| attrList |
	attrList _ model actualContents attributesAt: (self pointIndex + self markIndex)//2.
	attrList do: [:attr |
		attr forTextActionInfoDo: [ :info |
			^ self selectedString, '<', info, '>']].
	"If none of the above"
	attrList do: [:attr |
		attr forTextColorDo: [ :color |
			^ self selectedString, '<', color printString, '>']].
	^ self selectedString, '[No hidden info]'
</details>

#### TextEditor>>#processKeyStroke: aKeyboardEvent

Key struck on the keyboard. Find out which one and, if special, carry out the associated special action. Otherwise, add the character to the stream of characters.


<details>
	<summary>See more</summary>
	
	processKeyStroke: aKeyboardEvent
	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		self storeSelectionInComposition.
		^self].

	markBlock _ pointBlock.
	self storeSelectionInComposition
</details>

#### TextEditor>>#indent: delta fromStream: inStream toStream: outStream

Append the contents of inStream to outStream, adding or deleting delta or -delta tabs at the beginning, and after every NewLine except a final NewLine. Do not add tabs to totally empty lines, and be sure nothing but tabs are removed from lines.


<details>
	<summary>See more</summary>
	
	indent: delta fromStream: inStream toStream: outStream
	"Append the contents of inStream to outStream, adding or deleting delta or -delta
	 tabs at the beginning, and after every NewLine except a final NewLine.  Do not add tabs
	 to totally empty lines, and be sure nothing but tabs are removed from lines."

	| ch skip tab prev atEnd |
	tab _ Character tab.
	delta > 0
		ifTrue: [
			"shift right"
			prev _ Character newLineCharacter.
			[
			ch _ (atEnd _ inStream atEnd)
				ifTrue: [ Character newLineCharacter ]
				ifFalse: [ inStream next ].
			(prev isLineSeparator and: [ ch isLineSeparator not ]) ifTrue: [ delta timesRepeat: [ outStream nextPut: tab ]].
			atEnd ] whileFalse: [
				outStream nextPut: ch.
				prev _ ch ]]
		ifFalse: [
			"shift left"
			skip _ delta.
			"a negative number"
			[ inStream atEnd ] whileFalse: [
				((ch _ inStream next) == tab and: [ skip < 0 ]) ifFalse: [ outStream nextPut: ch ].
				skip _ ch isLineSeparator
					ifTrue: [ delta ]
					ifFalse: [ skip + 1 ]]]
</details>

#### TextEditor>>#findAndReplaceMany: doMany

Subroutine of search: and again. Use same FindText and ChangeText as before. If many is true, do it repeatedly. Created 1/26/96 sw by adding the many argument to #againOrSame.


<details>
	<summary>See more</summary>
	
	findAndReplaceMany: doMany
	"Subroutine of search: and again.  Use same FindText and ChangeText as before.  If many is true, do it repeatedly.  Created 1/26/96 sw by adding the many argument to #againOrSame."
	"jmvnote: We currently have no command for search&replace, but it is really easy to add, and this method supports it. See ChangeText"
	"This is a user command, and generates undo"

	|  indexStream |

	"Find and Change, recording start indices in the array"
	indexStream _ WriteStream on: (Array new: 20). "an array to store change locs"
	selectionStartBlocks _ OrderedCollection new.
	selectionStopBlocks _ OrderedCollection new.
	[(self findAndReplaceOnce: indexStream) & doMany] whileTrue. "<-- this does the work"
	"Last find was also stored in markBlock / pointBlock"
	selectionStartBlocks notEmpty ifTrue: [
		selectionStartBlocks removeLast.
		selectionStopBlocks removeLast ].
	"Answer whether something was found"
	^indexStream notEmpty
</details>

#### TextEditor>>#clearParens

<details>
	<summary>See more</summary>
	
	clearParens
	lastParenLocation ifNotNil: [
		model textSize >= lastParenLocation ifTrue: [
			model privateRemoveBoldAt: lastParenLocation ]].
	lastParenLocation _ nil
</details>

#### TextEditor>>#compareToClipboard

Check to see if whether the receiver's text is the same as the text currently on the clipboard, and inform the user.


<details>
	<summary>See more</summary>
	
	compareToClipboard
	"Check to see if whether the receiver's text is the same as the text currently on the clipboard, and inform the user."
	| s1 s2 |
	s1 _ self clipboardStringOrText string.
	s2 _ self selection ifEmpty: [self privateCurrentString].
	s1 = s2 ifTrue: [^ self inform: 'Exact match'].

	(TextModel new contents:
		(DifferenceFinder displayPatchFrom: s1 to: s2 tryWords: true))
			openLabel: 'Comparison to Clipboard Text'
</details>

#### TextEditor>>#selectFrom: start to: stop

Select the specified characters inclusive. I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2' If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:


<details>
	<summary>See more</summary>
	
	selectFrom: start to: stop
	"Select the specified characters inclusive.
	I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2'
	If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:"
	self selectInvisiblyFrom: start to: stop.
	self storeSelectionInComposition.
	"Preserve current emphasis if selection is empty"
	start > stop ifTrue: [
		self setEmphasisHereFromText ]
</details>

#### TextEditor>>#undo

<details>
	<summary>See more</summary>
	
	undo

	morph disablesEditing ifTrue: [
		^ self ].

	model undoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.				"this could be made more specific..."
			morph possiblyChanged ]]
</details>

#### TextEditor>>#currentAttributes

Redefined by subclasses that handle TextAttributes


<details>
	<summary>See more</summary>
	
	currentAttributes
	| i |
	i _ self startIndex.
	^i > model textSize
		ifTrue: [
			emphasisHere ]
		ifFalse: [
			"paragraph attributes at the current paragraph will later be applied.
			include them in the answer"
			(emphasisHere reject: [ :attr | attr isParagraphAttribute ]),
				((model actualContents attributesAt: i) select: [ :attr | attr isParagraphAttribute ]) ]
</details>

#### TextEditor>>#clearFont

Remove any Font attribute


<details>
	<summary>See more</summary>
	
	clearFont
	"Remove any Font attribute"
	"This is a user command, and generates undo"

	| attributesToRemove firstIndex lastIndex |
	attributesToRemove _ Set new.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	model actualContents attributesFrom: firstIndex to: lastIndex do: [ :attribute |
		attribute isFont ifTrue: [ attributesToRemove add: attribute ]].

	self unapplyAttributes: attributesToRemove.
	morph resetTextComposition.
	self recomputeSelection.
	morph updateFromTextComposition.
</details>

#### TextEditor>>#actualContents

<details>
	<summary>See more</summary>
	
	actualContents

	^model actualContents 
</details>

#### TextEditor>>#startIndex

<details>
	<summary>See more</summary>
	
	startIndex
	pointBlock ifNil: [^1].
	^ self startBlock stringIndex
</details>

#### TextEditor>>#endOfLine: position

Redefined in subclasses using TextComposition


<details>
	<summary>See more</summary>
	
	endOfLine: position
	"Redefined in subclasses using TextComposition"
	| targetLine |
	targetLine _ textComposition lines at: (textComposition lineIndexFor: position).
	^ targetLine = textComposition lastLine
		ifFalse: [ targetLine last ]
		ifTrue: [ targetLine last + 1 ]
</details>

#### TextEditor>>#addFinderMenuSectionTo: aMenu

Build a submenu with finding related operations


<details>
	<summary>See more</summary>
	
	addFinderMenuSectionTo: aMenu
	"Build a submenu with finding related operations"
	
	aMenu addItemsFromDictionaries:
	 `{
			{
				#label 			-> 		'Find...(f)'.
				#selector 		-> 		#find.
				#icon 			-> 		#findIcon
			} asDictionary.
			{
				#label 			-> 		'Find Again (g)'.
				#selector 		-> 		#findAgain.
				#icon 			-> 		#systemIcon
			} asDictionary.
			{
				#label 			-> 		'Use Selection for Find (j)'.
				#selector 		-> 		#setSearchString.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
		}`.
		^aMenu
</details>

#### TextEditor>>#stopIndex

<details>
	<summary>See more</summary>
	
	stopIndex
	pointBlock ifNil: [^1].
	^ self stopBlock stringIndex
</details>

#### TextEditor>>#chooseAlignment

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	chooseAlignment
	"This is a user command, and generates undo"

	| aList reply  |
	aList _ #(leftFlush centered justified rightFlush).
	reply _ (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNil: [ ^self ].
	self applyAttribute: (TextAlignment perform: reply).
	morph updateFromTextComposition.
	^ true
</details>

#### TextEditor>>#flash

Do nothing.


<details>
	<summary>See more</summary>
	
	flash
	^ morph flash
</details>

#### TextEditor>>#findAndReplaceOnce: indexStream

Find the next occurrence of FindText. If none, answer false. Append the start index of the occurrence to the stream indices, and, if ChangeText is not the same object as FindText, replace the occurrence by it. Note that the search is case-sensitive for replacements, otherwise not.


<details>
	<summary>See more</summary>
	
	findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: self class findText
		startingAt: self stopIndex
		caseSensitive: ((self class changeText ~~ self class findText) or: [Preferences caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	selectionStartBlocks add: (textComposition characterBlockForIndex: where).
	selectionStopBlocks add: (textComposition characterBlockForIndex: where + self class findText size).
	self selectFrom: where to: where + self class findText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	self class changeText ~~ self class findText ifTrue: [ self replaceSelectionWith: self class changeText ].
	indexStream nextPut: where.
	^ true
</details>

#### TextEditor>>#cursorHome: aKeyboardEvent

Private - Move cursor from position in current line to beginning of current line or end of indentation (see #firstOfBeginningOfLineOrEndOfIndentationLeftOf:). If control key is pressed put cursor at beginning of text


<details>
	<summary>See more</summary>
	
	cursorHome: aKeyboardEvent

	"Private - Move cursor from position in current line to beginning of current line or end of indentation (see #firstOfBeginningOfLineOrEndOfIndentationLeftOf:).
	If control key is pressed put cursor at beginning of text"

	self
		moveCursor: [ :position |
			"Mac standard keystrole"		
			(aKeyboardEvent commandAltKeyPressed or: [
				"Windows / Linux standard keystroke"
				aKeyboardEvent controlKeyPressed ])
					ifTrue: [ self beginningOfText ]
					ifFalse: [ self firstOfBeginningOfLineOrEndOfIndentationLeftOf: position ]]
		forward: false
		event: aKeyboardEvent.
	^true
</details>

#### TextEditor>>#help: aKeyboardEvent

Show a help screen


<details>
	<summary>See more</summary>
	
	help: aKeyboardEvent
	"Show a help screen"

	self openHelp.
	^ true
</details>

#### TextEditor>>#getMenu

<details>
	<summary>See more</summary>
	
	getMenu
	| aMenu | 
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		addTitle: self class name;
		addStayUpIcons.
		
	aMenu
		add: 'Help...'
		action: #openHelp
		icon: #helpIcon.
	aMenu addLine.
	
	self addFinderMenuSectionTo: aMenu.
	self addUndoMenuSectionTo: aMenu.
	aMenu addLine.
	
	self addCutAndPasteMenuSectionTo: aMenu.
	aMenu addLine.
	
	self addStyleMenuSectionTo: aMenu.
	
	^aMenu 
</details>

#### TextEditor>>#totalTextHeight

<details>
	<summary>See more</summary>
	
	totalTextHeight

	^textComposition lines last bottom
</details>

#### TextEditor>>#shouldHandleUsingCmdShortcuts: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	shouldHandleUsingCmdShortcuts: aKeyboardEvent 
	^ (aKeyboardEvent keyValue between: 32 and: 126) and: [ aKeyboardEvent commandAltKeyPressed ]
</details>

#### TextEditor>>#redo

<details>
	<summary>See more</summary>
	
	redo

	morph disablesEditing ifTrue: [
		^ self ].

	model redoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.			"this could be made more specific..."
			morph possiblyChanged ]]
</details>

#### TextEditor>>#selectInterval: anInterval

Deselect, then select the specified characters inclusive. Be sure the selection is in view.


<details>
	<summary>See more</summary>
	
	selectInterval: anInterval
	"Deselect, then select the specified characters inclusive.
	 Be sure the selection is in view."

	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	super selectInterval: anInterval
</details>

#### TextEditor>>#align

Align text according to the next greater alignment value, cycling among leftFlush, rightFlush, center, and justified.


<details>
	<summary>See more</summary>
	
	align
	"Align text according to the next greater alignment value,
	cycling among leftFlush, rightFlush, center, and justified."
	self chooseAlignment
</details>

#### TextEditor>>#pasteRecent

Paste an item chose from RecentClippings.


<details>
	<summary>See more</summary>
	
	pasteRecent
	"Paste an item chose from RecentClippings."

	| clipping |
	(clipping _ Clipboard chooseRecentClipping) ifNil: [^ self].
	self clipboardTextPut: clipping.
	^ self paste
</details>

#### TextEditor>>#actualContents: aString

<details>
	<summary>See more</summary>
	
	actualContents: aString

	model actualContents: aString 
</details>

#### TextEditor>>#setEmphasisHereFromTextForward: f

<details>
	<summary>See more</summary>
	
	setEmphasisHereFromTextForward: f

	| i forward delta prevIsLineSeparator nextIsLineSeparator prevIsSeparator nextIsSeparator |
	i _ self pointIndex.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsLineSeparator _  i > 1 and: [ (model actualContents at: i-1) isLineSeparator ].
	nextIsLineSeparator _ i <= model textSize and: [ (model actualContents at: i) isLineSeparator ].
	prevIsSeparator _  i > 1 and: [ (model actualContents at: i-1) isSeparator ].
	nextIsSeparator _ i <= model textSize and: [ (model actualContents at: i) isSeparator ].
	prevIsLineSeparator & nextIsLineSeparator
		ifTrue: [
			"Empty paragraph: take emphasis from the newLine character"
			forward _ false ]
		ifFalse: [
			prevIsSeparator == nextIsSeparator
				ifTrue: [
					"Space at both sides, or non-space at both sides, take emphasis used where the cursor comes from"
					forward _ f ]
				ifFalse: [
					"Space at one side and non-space at the other, take emphasis from the non-space character"
					forward _ nextIsSeparator ]].
	delta _ forward ifTrue: [ 1 ] ifFalse: [ 0 ].
	emphasisHere _ (model actualContents attributesAt: (i - delta max: 1))
					select: [:att | att mayBeExtended].
	morph possiblyChanged
</details>

#### TextEditor>>#backWord: aKeyboardEvent

If the selection is not empty, delete it and leave it in the backspace buffer. Else, delete the word before the text cursor.


<details>
	<summary>See more</summary>
	
	backWord: aKeyboardEvent
	"If the selection is not empty, delete it and leave it in the backspace buffer.
	 Else, delete the word before the text cursor."
	"This is a user command, and generates undo"

	| startIndex |
	self hasSelection ifFalse: [ "No selection, delete at least one character"
		startIndex _ 1 max: self markIndex - 1.
		[startIndex > 1 and:
			[(self privateCurrentString at: startIndex - 1) tokenish]]
				whileTrue: [
					startIndex _ startIndex - 1]]
	ifTrue: [ "some selection, just delete it"
		startIndex _ self markIndex].
	self backTo: startIndex.
	^false
</details>

#### TextEditor>>#changeLineEndsToLf: aKeyboardEvent

Replace all CRs and CrLfs by LFs. Triggered by Cmd-U -- useful when getting code from FTP sites


<details>
	<summary>See more</summary>
	
	changeLineEndsToLf: aKeyboardEvent
	"Replace all CRs and CrLfs by LFs.
	Triggered by Cmd-U -- useful when getting code from FTP sites"
	"This is a user command, and generates undo"
	
	self replaceSelectionWith: self selectedString withCuisLineEndings.
	^ true
</details>

#### TextEditor>>#applyAttribute: aTextAttribute

The user selected aTextAttribute via shortcut, menu or other means. If there is a selection, apply the attribute to the selection. In any case use the attribute for the user input (emphasisHere)


<details>
	<summary>See more</summary>
	
	applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone |

	morph disablesEditing ifTrue: [
		^ self ].

	anythingDone _ false.
	emphasisHere _ Text addAttribute: aTextAttribute toArray: emphasisHere.
	self selectionIntervalsDo: [ :interval |
		(interval notEmpty or: [ aTextAttribute isParagraphAttribute ])
			ifTrue: [
				anythingDone _ true.
				model logUndoAndAddAttribute: aTextAttribute from: interval first to: interval last.
				textComposition recomposeFrom: interval first to: interval last delta: 0 ]].
	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged
</details>

#### TextEditor>>#findAgain

Find the text-to-find again. 1/24/96 sw


<details>
	<summary>See more</summary>
	
	findAgain
	"Find the text-to-find again.  1/24/96 sw"

	(self findAndReplaceMany: false)
		ifFalse: [ self flash ].
</details>

#### TextEditor>>#addStyleMenuSectionTo: aMenu

Adds to the given menu text styiling related operations


<details>
	<summary>See more</summary>
	
	addStyleMenuSectionTo: aMenu
	"Adds to the given menu text styiling related operations"
	
	aMenu
		addItemsFromDictionaries: 
	`{
		{
				#label 			-> 		'Toggle WordWrap'.
				#selector 		-> 		#wrapOnOff.
				#icon 			-> 		#genericTextIcon
			} asDictionary.
			{
				#label 			-> 		'Set Font... (k)'.
				#selector 		-> 		#offerFontMenu.
				#icon 			-> 		#preferencesDesktopFontIcon
			} asDictionary.
			{
				#label 			-> 		'Clear Font'.
				#selector 		-> 		#clearFont.
				#icon 			-> 		#newIcon
			} asDictionary.
			{
				#label 			-> 		'Set Default Font...'.
				#selector 		-> 		#offerDefaultFontMenu.
				#icon 			-> 		#fontXGenericIcon
			} asDictionary.
			{
				#label 			-> 		'Set Alignment...'.
				#selector 		-> 		#chooseAlignment.
				#icon 			-> 		#formatJustifyLeftIcon
			} asDictionary.
		}`.
		^aMenu
</details>

#### TextEditor>>#forwardDelete: aKeyboardEvent

Delete forward over the next character.


<details>
	<summary>See more</summary>
	
	forwardDelete: aKeyboardEvent
	"Delete forward over the next character."
	"This is a user command, and generates undo"

	| startIndex stopIndex |

	"If there was a selection"
	self hasSelection ifTrue: [
		self replaceSelectionWith: self nullText.
		^ false].

	"Exit if at end"
	startIndex _ self markIndex.
	startIndex > model textSize ifTrue: [
		^ false].

	"Null selection - do the delete forward"
	stopIndex _ startIndex.
	(self shouldDeleteAWordForward: aKeyboardEvent)
		ifTrue: [stopIndex := (self nextWordStart: stopIndex) - 1].
	self selectFrom: startIndex to: stopIndex.
	self replaceSelectionWith: self nullText.
	self deselectAndPlaceCursorAt: startIndex.
	^false
</details>

#### TextEditor>>#hasUnacceptedEdits: aBoolean

<details>
	<summary>See more</summary>
	
	hasUnacceptedEdits: aBoolean

	^morph hasUnacceptedEdits: aBoolean 
</details>

#### TextEditor>>#addUndoMenuSectionTo: aMenu

Adds undo operations to the given menu


<details>
	<summary>See more</summary>
	
	addUndoMenuSectionTo: aMenu
	"Adds undo operations to the given menu"

	aMenu
		addItemsFromDictionaries: 
		`{	
			nil.
			{
				#label 			-> 		'Undo - multiple (z)'.
				#selector 		-> 		#undo.
				#icon 			-> 		#undoIcon
			} asDictionary.
			{
				#label 			-> 		'Redo - multiple (Z)'.
				#selector 		-> 		#redo.
				#icon 			-> 		#redoIcon
			} asDictionary.
			{
				#label 			-> 		'Undo / Redo history'.
				#selector 		-> 		#offerUndoHistory.
				#icon 			-> 		#changesIcon
			} asDictionary.
		}`.
		
	^aMenu.
</details>

#### TextEditor>>#visibleHeight

<details>
	<summary>See more</summary>
	
	visibleHeight

	^morph owner morphHeight
</details>

#### TextEditor>>#hasUnacceptedEdits

<details>
	<summary>See more</summary>
	
	hasUnacceptedEdits

	^morph hasUnacceptedEdits 
</details>

#### TextEditor>>#find: aKeyboardEvent

Prompt the user for what to find, then find it, searching from the current selection onward. 1/24/96 sw


<details>
	<summary>See more</summary>
	
	find: aKeyboardEvent
	"Prompt the user for what to find, then find it, searching from the current selection onward.  1/24/96 sw"

	self find.
	^ true
</details>

#### TextEditor>>#copySelection

Copy the current selection and store it in the Clipboard, unless empty.


<details>
	<summary>See more</summary>
	
	copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	| multiSelection |
	self lineSelectAndEmptyCheck: [^ self].

	multiSelection _ self selection.
	self clipboardTextPut: multiSelection
</details>

#### TextEditor>>#currentCharacterStyleOrNil

Answer the CharacterStyle for the current selection or cursor location if any


<details>
	<summary>See more</summary>
	
	currentCharacterStyleOrNil
	"Answer the CharacterStyle for the current selection or cursor location if any"

	^self hasSelection
		ifTrue: [ model actualContents characterStyleOrNilAt: self startIndex ]
		ifFalse: [ model actualContents characterStyleOrNilIfApplying: emphasisHere ]
</details>

#### TextEditor>>#offerFontMenu: aKeyboardEvent

The user typed the command key that requests a font change; Offer the font menu. 5/27/96 sw


<details>
	<summary>See more</summary>
	
	offerFontMenu: aKeyboardEvent 
	"The user typed the command key that requests a font change; Offer the font menu.  5/27/96 sw"

	self offerFontMenu.
	^ true
</details>

#### TextEditor>>#textComposition: aTextComposition

Install aTextComposition as the one to be edited by the receiver.


<details>
	<summary>See more</summary>
	
	textComposition: aTextComposition 
	"Install aTextComposition as the one to be edited by the receiver."

	textComposition _ aTextComposition
</details>

#### TextEditor>>#sameColumn: start newLine: lineBlock forward: isForward

Private - Compute the index in my text with the line number derived from lineBlock,


<details>
	<summary>See more</summary>
	
	sameColumn: start newLine: lineBlock forward: isForward
	"Private - Compute the index in my text
	with the line number derived from lineBlock,"
	" a one argument block accepting the old line number.
	The position inside the line will be preserved as good as possible"
	"The boolean isForward is used in the border case to determine if
	we should move to the beginning or the end of the line."
	| column currentLine offsetAtTargetLine targetEOL lines numberOfLines currentLineNumber targetLineNumber |
	lines _ textComposition lines.
	numberOfLines _ textComposition numberOfLines.
	currentLineNumber  _ textComposition lineIndexFor: start.
	currentLine _ lines at: currentLineNumber.
	column _ start - currentLine first.
	targetLineNumber _ ((lineBlock value: currentLineNumber) max: 1) min: numberOfLines.
	offsetAtTargetLine _ (lines at: targetLineNumber) first.
	targetEOL _ (lines at: targetLineNumber) last + (targetLineNumber = numberOfLines ifTrue:[1]ifFalse:[0]).
	targetLineNumber = currentLineNumber
	"No movement or movement failed. Move to beginning or end of line."
		ifTrue:[
			^isForward
				ifTrue:[targetEOL]
				ifFalse:[offsetAtTargetLine]].
	^offsetAtTargetLine + column min: targetEOL.
</details>

#### TextEditor>>#offerDefaultFontMenu

Present a menu of available fonts, and if one is chosen, apply it to the current selection. Note: use #baseFont. If emphasis is desired, add it separatedly.


<details>
	<summary>See more</summary>
	
	offerDefaultFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| newFont |
	newFont _ AbstractFont fromUser: defaultFont.
	newFont ifNil: [ ^self ].
	defaultFont _ newFont.
	morph resetTextComposition.
	self recomputeSelection.
	morph updateFromTextComposition.
</details>

#### TextEditor>>#defaultFont

<details>
	<summary>See more</summary>
	
	defaultFont
	^defaultFont
</details>

#### TextEditor>>#replaceSelectionWith: aTextOrString

Deselect, and replace the selection text by aText.


<details>
	<summary>See more</summary>
	
	replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |

	morph disablesEditing ifTrue: [
		^ self ].

	start _ self startIndex.
	stop _ self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere _ (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement _ self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		selectionStartBlocks _ #().
		selectionStopBlocks _ #().
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged
</details>

#### TextEditor>>#clickAndHalf

<details>
	<summary>See more</summary>
	
	clickAndHalf

	self selectWord.

	doWordSelection _ true.
	doParagraphSelection _ false.
	initialSelectionStart _ self startBlock.
	initialSelectionStop _ self stopBlock
</details>

#### TextEditor>>#blinkParenAt: parenLocation

<details>
	<summary>See more</summary>
	
	blinkParenAt: parenLocation

	model privateAddBoldAt: parenLocation.
	lastParenLocation _ parenLocation
</details>

#### TextEditor>>#storeSelectionInComposition

for proper display of selected text


<details>
	<summary>See more</summary>
	
	storeSelectionInComposition
	"for proper display of selected text"
	textComposition
		selectionStartBlocks: (selectionStartBlocks copyWith: self startBlock)
		selectionStopBlocks: (selectionStopBlocks copyWith: self stopBlock)
</details>

#### TextEditor>>#stopBlock

<details>
	<summary>See more</summary>
	
	stopBlock
	^ pointBlock max: markBlock
</details>

#### TextEditor>>#chooseColor

Make a new Text Color Attribute, let the user pick a color, and return the attribute


<details>
	<summary>See more</summary>
	
	chooseColor
	"Make a new Text Color Attribute, let the user pick a color, and return the attribute"

	| |
	"(ColorPickerMorph new)
		choseModalityFromPreference;
		sourceHand: morph activeHand;
		target: (attribute := TextColor color: Color black);
		selector: #color:;
		originalColor: Color black;
		putUpFor: morph near: morph morphFullBoundsInWorld.
	^attribute"
</details>

#### TextEditor>>#correctFrom: start to: stop with: aString

Make a correction in the model that the user has authorised from somewhere else in the system (such as from the compilier). The user's selection is not changed, only corrected.


<details>
	<summary>See more</summary>
	
	correctFrom: start to: stop with: aString
	"Make a correction in the model that the user has authorised from somewhere else in the system (such as from the compilier).  The user's selection is not changed, only corrected."
	"This is a user command, and generates undo"
	| userSelection delta loc str |
	aString = '#insert period' ifTrue: [
		loc _ start.
		str _ self privateCurrentString.
		[(loc _ loc-1)>0 and: [(str at: loc) isSeparator]]
			whileTrue: [loc _ loc-1].
		^ self correctFrom: loc+1 to: loc with: '.'].
	userSelection _ self selectionInterval.

	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: aString.

	delta _ aString size - (stop - start + 1).
	self
		selectInvisiblyFrom: userSelection first + (userSelection first > start ifFalse: [ 0 ] ifTrue: [ delta ])
		to: userSelection last + (userSelection last > start ifFalse: [ 0 ] ifTrue: [ delta ])

</details>

#### TextEditor>>#shouldDeleteAWordForward: aKeyboardEvent

<details>
	<summary>See more</summary>
	
	shouldDeleteAWordForward: aKeyboardEvent

	^ aKeyboardEvent isDelete and: [
		aKeyboardEvent rawMacOptionKeyPressed or: [
			aKeyboardEvent controlKeyPressed ] ]
</details>

#### TextEditor>>#offerColorMenu: aKeyboardEvent

The user typed the command key that requests a font change; Offer the color menu.


<details>
	<summary>See more</summary>
	
	offerColorMenu: aKeyboardEvent 
	"The user typed the command key that requests a font change; Offer the color menu."

	self offerColorMenu.
	^ true
</details>

#### TextEditor>>#offerUndoHistory

<details>
	<summary>See more</summary>
	
	offerUndoHistory
	| index labels current |
	current _ model undoRedoCommandsPosition.
	labels _ model undoRedoCommands withIndexCollect: [ :each :i | 
		(i = current ifTrue: [ '<on>' ] ifFalse: [ '<off>' ]), each printString ].
	labels isEmpty ifFalse: [
		index _ (PopUpMenu
			labelArray: labels
			lines: #()) startUpMenu.
		index = current ifTrue: [ ^self ].
		index = 0 ifTrue: [ ^self ].
		index < current
			ifTrue: [ current - index timesRepeat: [ self undo ]]
			ifFalse: [ index - current timesRepeat: [ self redo ]]]
</details>

#### TextEditor>>#pointBlock

<details>
	<summary>See more</summary>
	
	pointBlock
	^pointBlock
</details>

#### TextEditor>>#setSearchString: aKeyboardEvent

Establish the current selection as the current search string.


<details>
	<summary>See more</summary>
	
	setSearchString: aKeyboardEvent
	"Establish the current selection as the current search string."

	| aString |
	self lineSelectAndEmptyCheck: [^ true].
	aString _  self selectedString.
	aString size = 0
		ifTrue: [ self flash ]
		ifFalse: [ self setSearch: aString ].
	^ true
</details>

#### TextEditor>>#selectAll

<details>
	<summary>See more</summary>
	
	selectAll

	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	super selectAll
</details>

#### TextEditor>>#zapMultiSelection

This generates undo


<details>
	<summary>See more</summary>
	
	zapMultiSelection
	"This generates undo"

	| delta intervals mi pi start stop miAndPiUpdated |
	miAndPiUpdated _ false.
	mi _ self markIndex.
	pi _ self pointIndex.
	delta _ 0.
	intervals _ (selectionStartBlocks with: selectionStopBlocks
		collect: [ :strt :stp | strt stringIndex to: stp stringIndex ])
			sorted: [ :a :b | a first < b first ].
	intervals do: [ :interval |
		miAndPiUpdated not ifTrue: [
			interval first > mi
				ifTrue: [
					miAndPiUpdated _ true.
					mi _ mi + delta.
					pi _ pi + delta ]
				ifFalse: [
					interval last > mi ifTrue: [
						miAndPiUpdated _ true.
						mi _ interval first + delta.
						pi _ mi ]]].
		start _  interval first+delta.
		stop _ interval last - 1 + delta.
		model logUndoAndReplaceFrom: start to: stop with: ''.
		delta _ delta - (interval size-1).
		textComposition
			recomposeFrom: start
			to:  start - 1
			delta: delta ].

	miAndPiUpdated not ifTrue: [
		mi _ mi + delta.
		pi _ pi + delta ].
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	self markIndex: mi pointIndex: pi
</details>

#### TextEditor>>#selectLine

Make the receiver's selection, if currently empty, encompass the current line.


<details>
	<summary>See more</summary>
	
	selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	(self hasSelection or: [ selectionStartBlocks notEmpty]) ifTrue:[^self].
	self selectInterval: (model actualContents encompassLine: self selectionInterval)
</details>

#### TextEditor>>#openHelp

Show help screen


<details>
	<summary>See more</summary>
	
	openHelp
	"Show help screen"
	TextModel new contents: self help; openLabel: self name, ' Help'.
</details>

#### TextEditor>>#defaultFont: aFont

<details>
	<summary>See more</summary>
	
	defaultFont: aFont
	defaultFont _ aFont
</details>

#### TextEditor>>#offerFontMenu

Present a menu of available fonts, and if one is chosen, apply it to the current selection. Note: use #baseFont. If emphasis is desired, add it separatedly.


<details>
	<summary>See more</summary>
	
	offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| curFont newFont attr startIndex |
	startIndex _ self startIndex.
	curFont _ model actualContents fontAt: startIndex default: defaultFont.
	newFont _ AbstractFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr _ TextFontFamilyAndSize
			familyName: newFont familyName
			pointSize: newFont pointSize.
	self applyAttribute: attr.
	morph updateFromTextComposition.
</details>

#### TextEditor>>#resetState

Establish the initial conditions for editing the paragraph: place text cursor before first character and set the emphasis to that of the first character


<details>
	<summary>See more</summary>
	
	resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock _ textComposition defaultCharacterBlock.
	pointBlock _ markBlock.
	self setEmphasisHereFromText.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()
</details>

#### TextEditor>>#deselectAndPlaceCursorAt: characterIndex

Deselect, then place the text cursor before the character at characterIndex. Be sure it is in view.


<details>
	<summary>See more</summary>
	
	deselectAndPlaceCursorAt: characterIndex 
	"Deselect, then place the text cursor before the character at characterIndex.
	 Be sure it is in view."

	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	super deselectAndPlaceCursorAt: characterIndex
</details>

#### TextEditor>>#selectionRectangle

Answer a rectangle that encompasses single or multiple selection. If no selection, answer a rectangle that includes cursor.


<details>
	<summary>See more</summary>
	
	selectionRectangle
	"Answer a rectangle that encompasses single or multiple selection.
	If no selection, answer a rectangle that includes cursor."
	selectionStartBlocks notEmpty ifTrue: [
		^ selectionStartBlocks first quickMerge: selectionStopBlocks last].
	^ markBlock quickMerge: pointBlock
</details>

#### TextEditor>>#privateCurrentString

Answer the string I'm editing. Private. Just for internal Editor use.


<details>
	<summary>See more</summary>
	
	privateCurrentString
	"Answer the string I'm editing. Private. Just for internal Editor use."

	^model actualContents string
</details>

#### TextEditor>>#hasSelection

<details>
	<summary>See more</summary>
	
	hasSelection
	^ markBlock ~= pointBlock or: [ selectionStartBlocks notEmpty ]
</details>

#### TextEditor>>#afterSelectionInsertAndSelect: aString

This is a user command, and generates undo


<details>
	<summary>See more</summary>
	
	afterSelectionInsertAndSelect: aString
	"This is a user command, and generates undo"

	self insertAndSelect: aString at: self stopIndex 
</details>

#### TextEditor>>#setEmphasisHereFromText

<details>
	<summary>See more</summary>
	
	setEmphasisHereFromText

	self setEmphasisHereFromTextForward: true
</details>

#### TextEditor>>#wordUnder: aPositionInText

<details>
	<summary>See more</summary>
	
	wordUnder: aPositionInText
	
	| wordUnderCursorRange word indexOfSpace |
	
	wordUnderCursorRange := self wordRangeUnder: aPositionInText.
	word := (model actualContents copyFrom: wordUnderCursorRange first to: wordUnderCursorRange last) asString.
	
	"I have to handle the edge case where the cursor is for example between a ' and the first letter of the word.
	In that case the range will include words with spaces - Hernan"
	indexOfSpace := word indexOf: $  ifAbsent: [ ^word ].
	
	^word first: indexOfSpace -1 
	
			
</details>

#### TextEditor>>#selectedString

<details>
	<summary>See more</summary>
	
	selectedString

	^self selection string
</details>

#### TextEditor>>#isDisjointFrom: anInterval

Answer true if anInterval is empty and not touching or within the current interval, or if anInterval is a not empty but it does not overlap the current selection.


<details>
	<summary>See more</summary>
	
	isDisjointFrom: anInterval
	"Answer true if anInterval is empty and not touching or within the current
	 interval, or if anInterval is a not empty but it does not overlap the current
	 selection."

	| fudge |
	fudge _ anInterval size = 0 ifTrue: [1] ifFalse: [0].
	^(anInterval last + fudge < self startIndex or:
			[anInterval first - fudge >= self stopIndex])

</details>

#### TextEditor>>#undo: aKeyboardEvent

Undo the last edit.


<details>
	<summary>See more</summary>
	
	undo: aKeyboardEvent 
	"Undo the last edit."

	self undo.
	^true
</details>

#### TextEditor>>#text

The returned object should be treated as read-only, and never modified


<details>
	<summary>See more</summary>
	
	text
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
</details>

#### TextEditor>>#pageHeight

<details>
	<summary>See more</summary>
	
	pageHeight
	| howManyLines visibleHeight totalHeight ratio |
	howManyLines _ textComposition numberOfLines.
	visibleHeight _ self visibleHeight.
	totalHeight _ self totalTextHeight.
	ratio _ visibleHeight / totalHeight.
	^(ratio * howManyLines) rounded - 2
</details>

## TextModel

I am a kind of Model that includes a piece of text. Category 'pane menu' and 'message list menu' are messages that may be called by my menus. See also CodeProvider.

### Methods
#### TextModel>>#isTextEmpty

<details>
	<summary>See more</summary>
	
	isTextEmpty
	^actualContents isEmpty
</details>

#### TextModel>>#contents: aTextOrString

Does not update any view... The kind of stuff that needs to be cleaned some day...


<details>
	<summary>See more</summary>
	
	contents: aTextOrString
	"Does not update any view...
	The kind of stuff that needs to be cleaned some day..."
	self basicActualContents: aTextOrString
</details>

#### TextModel>>#flushUndoRedoCommands

<details>
	<summary>See more</summary>
	
	flushUndoRedoCommands

	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#formatAndStyleIfNeededWith: anSHTextStyler

<details>
	<summary>See more</summary>
	
	formatAndStyleIfNeededWith: anSHTextStyler
	anSHTextStyler ifNotNil: [
		(self shouldStyle: self actualContents with: anSHTextStyler) ifTrue: [
			anSHTextStyler formatAndStyle: self actualContents allowBackgroundStyleProcess: true.
			self basicActualContents: anSHTextStyler formattedText ]]
</details>

#### TextModel>>#logUndoAndAddAttribute: aTextAttribute from: requestedStart to: requestedStop

As requested.


<details>
	<summary>See more</summary>
	
	logUndoAndAddAttribute: aTextAttribute from: requestedStart to: requestedStop
	"As requested."

	| command |
	command _ actualContents commandForAddAttribute: aTextAttribute from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#basicActualContents: aTextOrString

Do not throw events... Not nice... Use with care... Clean some day...


<details>
	<summary>See more</summary>
	
	basicActualContents: aTextOrString
	"Do not throw events... Not nice... Use with care... Clean some day..."
	| prevContents |
	prevContents _ actualContents.
	actualContents _ aTextOrString ifNotNil: [ aTextOrString asText ].
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp _ nil ]
</details>

#### TextModel>>#undoAndEvaluate: aTwoArgBlock

<details>
	<summary>See more</summary>
	
	undoAndEvaluate: aTwoArgBlock
	| modelUpdated newCursorPos |
	modelUpdated _ false.
	undoRedoCommands position > 0 ifTrue: [
		undoRedoCommands skip: -1.
		newCursorPos _ undoRedoCommands peek undoOn: self.
		modelUpdated _ true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#textSize

<details>
	<summary>See more</summary>
	
	textSize
	^actualContents size
</details>

#### TextModel>>#privateRemoveBoldAt: idx

Just for shout. No undo.


<details>
	<summary>See more</summary>
	
	privateRemoveBoldAt: idx
	"Just for shout. No undo."

	actualContents 
		removeAttributes: { ShoutTextEmphasis bold }
		from: idx
		to: idx
</details>

#### TextModel>>#acceptContentsFrom: aTextModelMorph

Nothing to do here. Anyway, maybe this implementor should be removed...


<details>
	<summary>See more</summary>
	
	acceptContentsFrom: aTextModelMorph
	"Nothing to do here.
	Anyway, maybe this implementor should be removed..."
</details>

#### TextModel>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextModel>>#getSelection

Answer the model's selection interval.


<details>
	<summary>See more</summary>
	
	getSelection
	"Answer the model's selection interval."

	^ nil	"null selection"
</details>

#### TextModel>>#computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextModel>>#logUndoAndReplaceFrom: start to: stop with: replacement

As requested.


<details>
	<summary>See more</summary>
	
	logUndoAndReplaceFrom: start to: stop with: replacement
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	
	now _ Time millisecondClockValue.
	command _ self commandForReplaceFrom: start to: stop with: replacement.
	(stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 and: [start = undoRedoCommands last stopPosition] ]])
		ifTrue: [
			"Don't use the command we just built"
			undoRedoCommands last appendToNew: replacement
			]
		ifFalse: [
			undoRedoCommands
				nextPut: command;
				truncateAtPosition.	"To disable redo of previous commands, now invalidated."
			].
	command doOn: self.
	lastEditTimeStamp _ now
</details>

#### TextModel>>#autoCompleterClass

<details>
	<summary>See more</summary>
	
	autoCompleterClass
	^nil
</details>

#### TextModel>>#undoRedoCommands

<details>
	<summary>See more</summary>
	
	undoRedoCommands
	^undoRedoCommands contents
</details>

#### TextModel>>#startNewUndoRedoCommand

<details>
	<summary>See more</summary>
	
	startNewUndoRedoCommand

	lastEditTimeStamp _ nil
</details>

#### TextModel>>#privateAddBoldAt: idx

Just for shout. No undo.


<details>
	<summary>See more</summary>
	
	privateAddBoldAt: idx
	"Just for shout. No undo."

	actualContents 
		addAttribute: ShoutTextEmphasis bold
		from: idx
		to: idx
</details>

#### TextModel>>#textStylerClass

<details>
	<summary>See more</summary>
	
	textStylerClass
	^nil
</details>

#### TextModel>>#openLabel: aString

Create a standard system view of the model, me, and open it.


<details>
	<summary>See more</summary>
	
	openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	^SystemWindow editText: self label: aString wrap: true
</details>

#### TextModel>>#commandForReplaceFrom: start to: stop with: replacement

<details>
	<summary>See more</summary>
	
	commandForReplaceFrom: start to: stop with: replacement

	^ actualContents commandForReplaceFrom: start to: stop with: replacement
</details>

#### TextModel>>#saveOn: stream as: format

Saves the model to the given stream


<details>
	<summary>See more</summary>
	
	saveOn: stream as: format
	"Saves the model to the given stream"
	stream binary.
	stream nextPutAll: self actualContents asString.
</details>

#### TextModel>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextModel>>#initialize

Initialize the state of the receiver with its default contents.


<details>
	<summary>See more</summary>
	
	initialize
	"Initialize the state of the receiver with its default contents."

	actualContents _ '' asText.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#actualContents: aTextOrString

<details>
	<summary>See more</summary>
	
	actualContents: aTextOrString
	self basicActualContents: aTextOrString.
	self changed: #actualContents
</details>

#### TextModel>>#undoRedoCommandsPosition

<details>
	<summary>See more</summary>
	
	undoRedoCommandsPosition
	^undoRedoCommands position
</details>

#### TextModel>>#editorClass

<details>
	<summary>See more</summary>
	
	editorClass
	^TextEditor
</details>

#### TextModel>>#refetch

Nothing here. Answer true if actualContents was actually fetched.


<details>
	<summary>See more</summary>
	
	refetch
	"Nothing here. Answer true if actualContents was actually fetched."
	^false
</details>

#### TextModel>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #canSaveContents or: [ super is: aSymbol ]
</details>

#### TextModel>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	super postCopy.
	actualContents _ actualContents copy.
	undoRedoCommands _ ReadWriteStream on: Array new.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#shouldStyle: aText with: aSHTextStylerST80

<details>
	<summary>See more</summary>
	
	shouldStyle: aText with: aSHTextStylerST80 
	
	^true
</details>

#### TextModel>>#redoAndEvaluate: aTwoArgBlock

<details>
	<summary>See more</summary>
	
	redoAndEvaluate: aTwoArgBlock
	| modelUpdated newCursorPos |
	modelUpdated _ false.
	undoRedoCommands atEnd ifFalse: [
		newCursorPos _ undoRedoCommands next doOn: self.
		modelUpdated _ true ].
	aTwoArgBlock value: modelUpdated value: newCursorPos.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#classOfThisContext

<details>
	<summary>See more</summary>
	
	classOfThisContext

	^ MethodContext 
</details>

#### TextModel>>#logUndoAndRemoveAttributes: textAttributes from: requestedStart to: requestedStop

As requested.


<details>
	<summary>See more</summary>
	
	logUndoAndRemoveAttributes: textAttributes from: requestedStart to: requestedStop
	"As requested."

	| command |
	command _ actualContents commandForRemoveAttributes: textAttributes from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp _ nil
</details>

#### TextModel>>#wantsFrameAdornments

<details>
	<summary>See more</summary>
	
	wantsFrameAdornments
	^false
</details>

#### TextModel>>#basicReplaceFrom: start to: stop with: replacement

As requested. Basic service used by Undo / Redo. Does not genertate undo.


<details>
	<summary>See more</summary>
	
	basicReplaceFrom: start to: stop with: replacement
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	actualContents replaceFrom: start to: stop with: replacement
</details>

#### TextModel>>#classOfWorkspaceVarNamed: aName

<details>
	<summary>See more</summary>
	
	classOfWorkspaceVarNamed: aName

	^ nil
</details>

#### TextModel>>#actualContents

<details>
	<summary>See more</summary>
	
	actualContents
	^actualContents
</details>

#### TextModel>>#canBindVariables

<details>
	<summary>See more</summary>
	
	canBindVariables
	^ false
</details>

#### TextModel>>#basicReplaceAttributesFrom: start to: stop with: replacement

As requested. Basic service used by Undo / Redo. Does not genertate undo.


<details>
	<summary>See more</summary>
	
	basicReplaceAttributesFrom: start to: stop with: replacement
	"As requested. Basic service used by Undo / Redo. Does not genertate undo."

	actualContents basicReplaceAttributesFrom: start to: stop with: replacement
</details>

#### TextModel>>#computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextModel>>#convertToCurrentVersion: varDict refStream: smartRefStrm

Maybe old instances won't have this variable set.


<details>
	<summary>See more</summary>
	
	convertToCurrentVersion: varDict refStream: smartRefStrm

	"Maybe old instances won't have this variable set."
	undoRedoCommands ifNil: [
		undoRedoCommands _ ReadWriteStream on: Array new ]
</details>

## TextProvider

A superclass for text providing models. To be used together with a PluggableTextModel.

### Methods
#### TextProvider>>#contentsSelection

Return the interval of text in the code pane to select when I set the pane's contents


<details>
	<summary>See more</summary>
	
	contentsSelection
	"Return the interval of text in the code pane to select when I set the pane's contents"

	^ nil  "null selection"
</details>

#### TextProvider>>#acceptedContents

<details>
	<summary>See more</summary>
	
	acceptedContents
	^ Text
		initialFont: Preferences standardCodeFont
		stringOrText: self acceptedStringOrText
</details>

#### TextProvider>>#acceptedContentsChanged

<details>
	<summary>See more</summary>
	
	acceptedContentsChanged

	self changed: #acceptedContents
</details>

#### TextProvider>>#textStylerClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	textStylerClassFor: textGetter
	^nil
</details>

#### TextProvider>>#classOfThisContext

<details>
	<summary>See more</summary>
	
	classOfThisContext

	^ MethodContext 
</details>

#### TextProvider>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextProvider>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	^TextEditor
</details>

#### TextProvider>>#computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextProvider>>#acceptedStringOrText

<details>
	<summary>See more</summary>
	
	acceptedStringOrText
	^self subclassResponsibility
</details>

#### TextProvider>>#classOfWorkspaceVarNamed: aName

<details>
	<summary>See more</summary>
	
	classOfWorkspaceVarNamed: aName

	^ nil
</details>

#### TextProvider>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock

</details>

#### TextProvider>>#computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

#### TextProvider>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	^nil
</details>

#### TextProvider>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForUnknowClass
</details>

## TextReplaceCommand

My instances are text replace commands, such as typing, pasting, deleting, etc.

### Methods
#### TextReplaceCommand>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	old isEmpty ifTrue: [
		^aStream nextPutAll: 'typed: '; nextPutAll: new asString withDescriptiveLineEndings surroundedBySingleQuotes].
	new isEmpty ifTrue: [
		^aStream nextPutAll: 'deleted: '; nextPutAll: old asString withDescriptiveLineEndings surroundedBySingleQuotes ].
	aStream
		nextPutAll: 'replaced: ';
		nextPutAll: old asString withDescriptiveLineEndings surroundedBySingleQuotes;
		nextPutAll: ' with: ';
		nextPutAll: new asString withDescriptiveLineEndings surroundedBySingleQuotes 
</details>

#### TextReplaceCommand>>#appendToNew: aStringOrText

<details>
	<summary>See more</summary>
	
	appendToNew: aStringOrText
	new _ new, aStringOrText
</details>

#### TextReplaceCommand>>#undoOn: aTextModel

Undo the command, bringing the text model to the state it had prior to doing it. Answer a new position for the text cursor


<details>
	<summary>See more</summary>
	
	undoOn: aTextModel
	"Undo the command, bringing the text model to the state it had prior to doing it.
	Answer a new position for the text cursor"

	aTextModel basicReplaceFrom: position to: position + new size-1 with: old.
	^position + old size
</details>

#### TextReplaceCommand>>#old: oldStringOrText new: newStringOrText at: anInteger

<details>
	<summary>See more</summary>
	
	old: oldStringOrText new: newStringOrText at: anInteger
	old _ oldStringOrText.
	new _ newStringOrText.
	position _ anInteger
</details>

#### TextReplaceCommand>>#doOn: aTextModel

Perform the command, used for initial execution or for redo after undoing. Answer a new position for the text cursor


<details>
	<summary>See more</summary>
	
	doOn: aTextModel
	"Perform the command, used for initial execution or for redo after undoing.
	Answer a new position for the text cursor"

	aTextModel basicReplaceFrom: position to: position + old size-1 with: new.
	^position + new size
</details>

#### TextReplaceCommand>>#stopPosition

<details>
	<summary>See more</summary>
	
	stopPosition
	^position + new size.
</details>

## Trie

I am an efficient collection of Strings. I behave a bit like a Dictionary, with the restriction that keys are instances of String. Notes: - check for inclusion is extremely fast - iteration is always done in collation order, contents are always sorted without performance cost - Behaves both like a Set (#add:, #remove:, #includes, #do) and a Dictionary (#at:put:, #at:, #at:ifAbsent:) | t | t _ Trie new. t add: 'car'. t at: 'car' put: Float pi. t at: 'cat' put: Date today. t explore. (t includesKey: 'car') print. (t includes: 'cat') print | t | t _ Trie new. Smalltalk allImplementedMessages do: [ :s | t add: s ]. t explore

### Methods
#### Trie>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	"Refer to the comment in Object|storeOn:."
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self keysAndValuesDo: 
			[:each :val | 
			noneYet
				ifTrue: [noneYet _ false]
				ifFalse: [aStream nextPut: $;].
			aStream 
				nextPutAll: ' at: ';
				store: each;
				nextPutAll: ' put: ';
				store: val].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
</details>

#### Trie>>#add: aString

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: aString

	self at: aString put: aString
</details>

#### Trie>>#at: aString put: aValue

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: aString put: aValue

	rootNode ifNil: [
		rootNode _ TrieNode someKey: aString segmentStart: 1].
	^rootNode at: aString put: aValue characterIndex: 1
</details>

#### Trie>>#at: aString ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	at: aString ifAbsent: aBlock

	rootNode ifNil: [ ^aBlock value ].
	^ rootNode at: aString ifAbsent: aBlock characterIndex: 1
</details>

#### Trie>>#keysDo: aBlock

<details>
	<summary>See more</summary>
	
	keysDo: aBlock

	rootNode ifNotNil: [
		rootNode keysAndValuesDo: [ :k :v | aBlock value: k ]]
</details>

#### Trie>>#valuesDo: aBlock

<details>
	<summary>See more</summary>
	
	valuesDo: aBlock

	rootNode ifNotNil: [
		rootNode keysAndValuesDo: [ :k :v | aBlock value: v ]]
</details>

#### Trie>>#at: key ifPresent: aBlock

Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil.


<details>
	<summary>See more</summary>
	
	at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [^ nil].
	^ aBlock value: v
</details>

#### Trie>>#remove: aString ifAbsent: exceptionBlock

Consistent with Set


<details>
	<summary>See more</summary>
	
	remove: aString ifAbsent: exceptionBlock
	"Consistent with Set"
	self at: aString ifPresent: [ :v |
		v = aString ifTrue: [
			self removeKey: aString ifAbsent: exceptionBlock ]]
</details>

#### Trie>>#isEmpty

Answer whether the receiver contains any elements.


<details>
	<summary>See more</summary>
	
	isEmpty
	^rootNode isNil
</details>

#### Trie>>#removeKey: aString ifAbsent: aBlock

Remove key (and its associated value) from the receiver. If key is not in the receiver, answer the result of evaluating aBlock. Otherwise, answer the value externally named by key.


<details>
	<summary>See more</summary>
	
	removeKey: aString ifAbsent: aBlock
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| answer |
	rootNode ifNil: [ ^aBlock value ].
	answer _ rootNode removeKey: aString ifAbsent: aBlock characterIndex: 1.
	rootNode isEmpty ifTrue: [
		rootNode _ nil ].
	^answer
</details>

#### Trie>>#removeKey: aString

Remove key from the receiver. If key is not in the receiver, notify an error.


<details>
	<summary>See more</summary>
	
	removeKey: aString 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: aString ifAbsent: [self errorKeyNotFound]
</details>

#### Trie>>#errorKeyNotFound

<details>
	<summary>See more</summary>
	
	errorKeyNotFound

	self error: Dictionary keyNotFoundErrorDescription 
</details>

#### Trie>>#includes: aString

Consistent with Set, but not with Dictionary, as in Dictionary, #includes: finds a value regardless of the key. To get this behavior, use #includesValue:


<details>
	<summary>See more</summary>
	
	includes: aString
	"Consistent with Set, but not with Dictionary,  as in Dictionary, #includes:
	finds a value regardless of the key. To get this behavior, use #includesValue:"

	aString isString ifFalse: [ ^ false ].
	self at: aString ifPresent: [ :v | ^v = aString ].
	^false
</details>

#### Trie>>#at: key ifAbsentPut: aBlock

Return the value at the given key. If key is not included in the receiver store the result of evaluating aBlock as new value.


<details>
	<summary>See more</summary>
	
	at: key ifAbsentPut: aBlock 
	"Return the value at the given key.
	If key is not included in the receiver store the result
	of evaluating aBlock as new value."
	^self at: key ifAbsent:[self at: key put: aBlock value]
</details>

#### Trie>>#do: aBlock

Consistent both with Set (#add:) and Dictionary (#at:put:) protocols.


<details>
	<summary>See more</summary>
	
	do: aBlock
	"Consistent both with Set (#add:) and Dictionary (#at:put:) protocols."
	self valuesDo: aBlock
</details>

#### Trie>>#forPrefix: aString keysAndValuesDo: twoArgBlock

<details>
	<summary>See more</summary>
	
	forPrefix: aString keysAndValuesDo: twoArgBlock

	rootNode ifNotNil: [
		rootNode forPrefix: aString keysAndValuesDo: twoArgBlock characterIndex: 1]
</details>

#### Trie>>#usedMemory

<details>
	<summary>See more</summary>
	
	usedMemory
	^rootNode usedMemory + 12 "object header+ 1 ivar"
</details>

#### Trie>>#includesValue: anObject

Similar to Dictionary>>#includes:


<details>
	<summary>See more</summary>
	
	includesValue: anObject
	"Similar to Dictionary>>#includes:"
	self do: [:each | anObject = each ifTrue: [^true]].
	^false
</details>

#### Trie>>#at: key

Answer the value associated with the key.


<details>
	<summary>See more</summary>
	
	at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound]
</details>

#### Trie>>#keysAndValuesDo: twoArgBlock

<details>
	<summary>See more</summary>
	
	keysAndValuesDo: twoArgBlock

	rootNode ifNotNil: [
		rootNode keysAndValuesDo: twoArgBlock ]
</details>

#### Trie>>#includesKey: key

<details>
	<summary>See more</summary>
	
	includesKey: key
	
	self at: key ifAbsent: [^false].
	^true
</details>

## TrieLeaf

My instances hold the key/value pairs for Tries.

### Methods
#### TrieLeaf>>#at: aString ifAbsent: aBlock characterIndex: i

Unused argument i is just for polymorphism with TrieNode


<details>
	<summary>See more</summary>
	
	at: aString ifAbsent: aBlock characterIndex: i
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex |
	keys isArray ifTrue: [
		keyIndex _ keys indexOf: aString.
		^ keyIndex = 0
			ifTrue: [ aBlock value ]
			ifFalse: [ values at: keyIndex ]].
	keys = aString ifTrue: [ ^ values ].
	^ aBlock value
</details>

#### TrieLeaf>>#at: aString put: aValue characterIndex: i

Unused argument i is just for polymorphism with TrieNode


<details>
	<summary>See more</summary>
	
	at: aString put: aValue characterIndex: i
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex |
	keys ifNil: [
		keys _ aString.
		values _ aValue.
		^aValue ].
	keys isArray ifFalse: [
		keys = aString
			ifFalse: [
				keys _ {keys. aString }.
				values _ {values. aValue}]
			ifTrue: [ values _ aValue ].
		^ aValue ].
	keyIndex _ keys indexOf: aString.
	keyIndex = 0
		ifTrue: [
			keys _ keys copyWith: aString.
			values _ values copyWith: aValue ]
		ifFalse: [ values at: keyIndex put: aValue ].
	^ aValue
</details>

#### TrieLeaf>>#usedMemory

<details>
	<summary>See more</summary>
	
	usedMemory
	| s |
	s _ 4 "Object header for a compact class"
		+ (2*4). "2 instance variables"

	keys isArray ifTrue: [
		"Array is a compact class of words"
		s _ keys size * 4 + 4 + s.
		s _ values size * 4 + 4 + s ].
	^s
</details>

#### TrieLeaf>>#removeKey: aString ifAbsent: aBlock characterIndex: i

Unused argument i is just for polymorphism with TrieNode


<details>
	<summary>See more</summary>
	
	removeKey: aString ifAbsent: aBlock characterIndex: i
	"Unused argument i is just for polymorphism with TrieNode"
	| keyIndex answer |
	keys isArray ifTrue: [
		keyIndex _ keys indexOf: aString.
		^ keyIndex = 0
			ifTrue: [ aBlock value ]
			ifFalse: [
				answer _ values at: keyIndex.
				keys size = 2
					ifTrue: [
						keys _ keys at: 3-keyIndex.
						values _ values at: 3-keyIndex ]
					ifFalse: [
						keys _ (keys copyFrom: 1 to: keyIndex-1), (keys copyFrom: keyIndex+1 to: keys size).
						values _ (values copyFrom: 1 to: keyIndex-1), (values copyFrom: keyIndex+1 to: values size) ].
				answer]].
	keys = aString ifTrue: [
		answer _ values.
		keys _ nil.
		values _ nil.
		^ answer ].
	^ aBlock value
</details>

#### TrieLeaf>>#isEmpty

<details>
	<summary>See more</summary>
	
	isEmpty
	^keys isNil
</details>

#### TrieLeaf>>#keysAndValuesDo: twoArgBlock

<details>
	<summary>See more</summary>
	
	keysAndValuesDo: twoArgBlock

	keys isArray
		ifTrue: [ keys with: values do: twoArgBlock ]
		ifFalse: [ twoArgBlock value: keys value: values ]
</details>

## TrieNode

My instances form a Trie. They do path compression, meaning that they can represent a segment of seveal characters long.

### Methods
#### TrieNode>>#at: aString put: aValue characterIndex: i

<details>
	<summary>See more</summary>
	
	at: aString put: aValue characterIndex: i

	| child |
	childrenFirstChars ifNotNil: [
		self splitIfNeededFor: aString characterIndex: i ].
	child _ self childFor: aString characterIndex: i orAdd: true.
	^child at: aString put: aValue characterIndex: i + segmentSize
</details>

#### TrieNode>>#childFor1: aString characterIndex: i orAdd: doAdd

Answer child at childCode. If it is not there, and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it. otherwise, just answer nil. This method for the case where we currently have exactly one child.


<details>
	<summary>See more</summary>
	
	childFor1: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have exactly one child."
	| nextSegmentStart nextSegmentFirst answer |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars = nextSegmentFirst ifTrue: [ ^ children].

	doAdd ifFalse: [ ^ nil ].
	
	answer _ nextSegmentStart > aString size
		ifTrue: [ TrieLeaf new ]
		ifFalse: [ TrieNode someKey: aString segmentStart: nextSegmentStart ].

	"Just one child, if adding, convert references to a collection"
	nextSegmentFirst < childrenFirstChars
		ifTrue: [
			children _ {answer . children }.
			childrenFirstChars _ String with: nextSegmentFirst with: childrenFirstChars ]
		ifFalse: [
			children _ {children . answer}.
			childrenFirstChars _ String with: childrenFirstChars with: nextSegmentFirst ].
	^answer
</details>

#### TrieNode>>#setChildrenFirstChars: aString children: anArray

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	setChildrenFirstChars: aString children: anArray
	"Private. To be called only from parent node"
	childrenFirstChars _ aString.
	children _ anArray.
</details>

#### TrieNode>>#splitIfNeededFor: aString characterIndex: segmentStart

This method splits the self (the current node) into two nodes, such that the concatenation of their string segment equals our current string segment, and such that the string segment of the first node (i.e. us) equals or is a prefix of aString. If this latter condition is already met, do nothing.


<details>
	<summary>See more</summary>
	
	splitIfNeededFor: aString characterIndex: segmentStart
	"This method splits the self (the current node) into two nodes, such that the concatenation of their string segment equals our current string segment,
	and such that the string segment of the first node (i.e. us) equals or is a prefix of aString.
	If this latter condition is already met, do nothing."
	| oldSegmentStop newSegmentStop newNode |

	"Compute last index where both strings are equal"
	oldSegmentStop _ segmentStart + segmentSize - 1.
	newSegmentStop _ someKey commonPartWith: aString startAt: segmentStart stopAt: oldSegmentStop
		applying: [ :c | c asLowercase asUnaccented ].
			
	newSegmentStop = oldSegmentStop ifTrue: [
		"No need to split: our segment is already a prefix of aString"
		^self ].

	"Split node"
	newNode _ TrieNode someKey: someKey setSegmentSize: oldSegmentStop - newSegmentStop.
	newNode setChildrenFirstChars: childrenFirstChars children: children.

	"As this is a split point, these will be turned into arrays soon, when the new node addition (that called us) is done."
	childrenFirstChars _ (someKey at: newSegmentStop+1) asLowercase asUnaccented.
	children _ newNode.
	"Update our segment"
	segmentSize _ newSegmentStop - segmentStart + 1
</details>

#### TrieNode>>#at: aString ifAbsent: aBlock characterIndex: i

<details>
	<summary>See more</summary>
	
	at: aString ifAbsent: aBlock characterIndex: i

	| child |
	child _ self childFor: aString characterIndex: i orAdd: false.
	child ifNil: [ ^aBlock value ].
	^child at: aString ifAbsent: aBlock characterIndex: i + segmentSize
</details>

#### TrieNode>>#forPrefix: aString keysAndValuesDo: twoArgBlock characterIndex: i

<details>
	<summary>See more</summary>
	
	forPrefix: aString keysAndValuesDo: twoArgBlock characterIndex: i

	i + segmentSize > aString size
		ifTrue: [
			self keysAndValuesDo: twoArgBlock ]
		ifFalse: [
			(self childFor: aString characterIndex: i orAdd: false)
				ifNotNil: [ :node |
					node
						forPrefix: aString
						keysAndValuesDo: twoArgBlock
						characterIndex: i + segmentSize ]]
</details>

#### TrieNode>>#childForN: aString characterIndex: i orAdd: doAdd

Answer child at childCode. If it is not there, and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it. otherwise, just answer nil. This method for the case where we currently have more than one child.


<details>
	<summary>See more</summary>
	
	childForN: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have more than one child."
	| answer newChildren newFirstChars nextSegmentFirst nextSegmentStart s |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found | ^ children at: found ]
		ifNone: [ :a :b |
			"Already more than one child. If adding, convert to a bigger collection."
			doAdd
				ifFalse: [ ^nil]
				ifTrue: [
					 answer _ nextSegmentStart > aString size
						ifTrue: [ TrieLeaf new ]
						ifFalse: [  TrieNode someKey: aString segmentStart: nextSegmentStart ].
					s _ children size + 1.
					newChildren _ Array new: s.
					newChildren
						replaceFrom: 1 to: a with: children startingAt: 1;
						at: a+1 put: answer;
						replaceFrom: a+2 to: s with: children startingAt: a+1.
					newFirstChars _ String new: s.
					newFirstChars
						replaceFrom: 1 to: a with: childrenFirstChars startingAt: 1;
						at: a+1 put: nextSegmentFirst;
						replaceFrom: a+2 to: s with: childrenFirstChars startingAt: a+1.
					children _ newChildren.
					childrenFirstChars _ newFirstChars.
					^answer ]]
</details>

#### TrieNode>>#getChildrenFirstChars

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	getChildrenFirstChars
	"Private. To be called only from parent node"
	^childrenFirstChars
</details>

#### TrieNode>>#isEmpty

<details>
	<summary>See more</summary>
	
	isEmpty
	^childrenFirstChars isNil
</details>

#### TrieNode>>#childFor: aString characterIndex: i orAdd: doAdd

Answer child at childCode. If it is not there, and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it. otherwise, just answer nil.


<details>
	<summary>See more</summary>
	
	childFor: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."

	"No children yet"
	childrenFirstChars ifNil: [
		doAdd ifTrue: [
			children _ TrieLeaf new.
			childrenFirstChars _ TrieNode characterForLeaf ].
		^children ].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isString ifFalse: [
		^ self childFor1: aString characterIndex: i orAdd: doAdd ].

	"Already more than one child. If adding, convert to a bigger collection."
	^self childForN: aString characterIndex: i orAdd: doAdd
</details>

#### TrieNode>>#getSegmentSize

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	getSegmentSize
	"Private. To be called only from parent node"
	^segmentSize
</details>

#### TrieNode>>#getSomeKey

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	getSomeKey
	"Private. To be called only from parent node"
	^someKey
</details>

#### TrieNode>>#removeChildFor: aString characterIndex: i

Answer child at childCode. If it is not there, and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it. otherwise, just answer nil.


<details>
	<summary>See more</summary>
	
	removeChildFor: aString characterIndex: i
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."
	| nextSegmentStart nextSegmentFirst |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isString ifFalse: [
		childrenFirstChars = nextSegmentFirst
			ifTrue: [ childrenFirstChars _ nil. children _ nil ].
		^self ].
	
	"Already more than one child."
	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found |
			childrenFirstChars size = 2
				ifTrue: [		"Two children: remove the arrays"
					childrenFirstChars _ childrenFirstChars at: 3-found.
					children _ children at: 3-found ]
				ifFalse: [	"More than two children. condense the arrays"
					childrenFirstChars _ (childrenFirstChars copyFrom: 1 to: found-1), 
						(childrenFirstChars copyFrom: found+1 to: childrenFirstChars size).
					children _ (children copyFrom: 1 to: found-1), (children copyFrom: found+1 to: children size) ]]
		ifNone: [].
</details>

#### TrieNode>>#usedMemory

<details>
	<summary>See more</summary>
	
	usedMemory
	| s |
	s _ 4 "Object header for a compact class"
		+ (4*4). "4 instance variables"

	childrenFirstChars isString 
		ifTrue: [
			"String is a compact class of bytes"
			s _  childrenFirstChars size + 4 + s.
			"Array is a compact class of words"
			s _ children size * 4 + 4 + s.
			children do: [ :c | 
				s _ c usedMemory + s ]]
		ifFalse: [
			children ifNotNil: [ s _ s + children usedMemory ]].
	^s
</details>

#### TrieNode>>#getChildren

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	getChildren
	"Private. To be called only from parent node"
	^children
</details>

#### TrieNode>>#setSegmentSize: anInteger someKey: aString

Private. To be called only from parent node


<details>
	<summary>See more</summary>
	
	setSegmentSize: anInteger someKey: aString
	"Private. To be called only from parent node"
	segmentSize _ anInteger.
	someKey _ aString
</details>

#### TrieNode>>#keysAndValuesDo: twoArgBlock

<details>
	<summary>See more</summary>
	
	keysAndValuesDo: twoArgBlock

	children isArray ifFalse: [
		^children keysAndValuesDo: twoArgBlock ].

	children do: [ :child |
		child ifNotNil: [ child keysAndValuesDo: twoArgBlock ]]
</details>

#### TrieNode>>#removeKey: aString ifAbsent: aBlock characterIndex: i

<details>
	<summary>See more</summary>
	
	removeKey: aString ifAbsent: aBlock characterIndex: i

	| child answer |
	child _ self childFor: aString characterIndex: i orAdd: false.
	child ifNil: [ ^aBlock value ].
	answer _ child removeKey: aString ifAbsent: aBlock characterIndex: i + segmentSize.
	child isEmpty ifTrue: [
		self removeChildFor: aString characterIndex: i.
		"If child is not needed anymore, compress the path"	
		children class == TrieNode ifTrue: [
			someKey _ children getSomeKey.
			segmentSize _ segmentSize + children getSegmentSize.
			childrenFirstChars _ children getChildrenFirstChars.
			children _ children getChildren ]].
	^answer
</details>

## Workspace

A Workspace is a text area plus a lot of support for executable code. It is a great place to execute top-level commands to compute something useful, and it is a great place to develop bits of a program before those bits get put into class methods. To open a new workspace, execute: Workspace openLabel: 'a workspace' A workspace can have its own variables, called "workspace variables", to hold intermediate results. For example, if you type into a workspace "x := 5" and do-it, then later you could type in "y := x * 2" and y would become 10. Additionally, in Morphic, a workspace can gain access to morphs that are on the screen. If acceptDroppedMorphss is turned on, then whenever a morph is dropped on the workspace, a variable will be created which references that morph. This functionality is toggled with the window-wide menu of a workspace.

### Methods
#### Workspace>>#bindingOf: aString

<details>
	<summary>See more</summary>
	
	bindingOf: aString
	mustDeclareVariables ifTrue: [^ nil].
	(bindings includesKey: aString) ifFalse: [
		aString first isUppercase
			ifTrue: [^nil]
			ifFalse: [bindings at: aString put: nil]].
	^bindings associationAt: aString
</details>

#### Workspace>>#classOfBindingOf: aName

<details>
	<summary>See more</summary>
	
	classOfBindingOf: aName
												
	^ (self bindingOf: aName) value ifNotNil: [ :aValue | aValue class ] 
</details>

#### Workspace>>#initialize

Initialize the state of the receiver with its default contents.


<details>
	<summary>See more</summary>
	
	initialize
	
	super initialize.
	self initializeBindings.
	mustDeclareVariables := false
</details>

#### Workspace>>#editorClass

<details>
	<summary>See more</summary>
	
	editorClass
	^SmalltalkEditor
</details>

#### Workspace>>#bindingNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingNamesDo: aBlock

	bindings keysDo: aBlock
</details>

#### Workspace>>#hasBindingOf: aString

<details>
	<summary>See more</summary>
	
	hasBindingOf: aString
	^bindings includesKey: aString
</details>

#### Workspace>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
</details>

#### Workspace>>#hasBindingThatBeginsWith: aString

<details>
	<summary>See more</summary>
	
	hasBindingThatBeginsWith: aString 

	bindings keysDo: [ :each |
		(each beginsWith: aString) ifTrue: [ ^true ] ].
	^false
</details>

#### Workspace>>#toggleStylingLabel

<details>
	<summary>See more</summary>
	
	toggleStylingLabel

	^self shouldStyle 
		ifTrue: [ '<on> syntax highlighting' ]
		ifFalse: [ '<off> syntax highlighting' ]
</details>

#### Workspace>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text.


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self shouldStyle ifFalse: [ ^false ].
	anSHTextStyler 
		classOrMetaClass: nil;
		workspace: self.
	^true
</details>

#### Workspace>>#mustDeclareVariableWording

<details>
	<summary>See more</summary>
	
	mustDeclareVariableWording
	
	^ mustDeclareVariables not
		ifTrue: ['<yes> automatically create variable declaration']
		ifFalse: ['<no> automatically create variable declaration']
</details>

#### Workspace>>#nameForObject: object

Answer a name suitable for a Workspace variable


<details>
	<summary>See more</summary>
	
	nameForObject: object
	"Answer a name suitable for a Workspace variable"
	^ (object class name, object identityHash asString) asIdentifier: false
</details>

#### Workspace>>#initializeBindings

<details>
	<summary>See more</summary>
	
	initializeBindings
	
	bindings _ Dictionary new
</details>

#### Workspace>>#shouldStyle

<details>
	<summary>See more</summary>
	
	shouldStyle

	^shouldStyle ifNil: [ Preferences shoutInWorkspaces]
</details>

#### Workspace>>#autoCompleterClass

<details>
	<summary>See more</summary>
	
	autoCompleterClass
	^SmalltalkCompleter
</details>

#### Workspace>>#toggleStyling

<details>
	<summary>See more</summary>
	
	toggleStyling

	shouldStyle _ self shouldStyle not.
	actualContents _ actualContents asString asText.
	self changed: #actualContents
</details>

#### Workspace>>#canBindVariables

<details>
	<summary>See more</summary>
	
	canBindVariables
	^ true
</details>

#### Workspace>>#toggleVariableDeclarationMode

<details>
	<summary>See more</summary>
	
	toggleVariableDeclarationMode

	mustDeclareVariables := mustDeclareVariables not
</details>

#### Workspace>>#classOfWorkspaceVarNamed: aName

<details>
	<summary>See more</summary>
	
	classOfWorkspaceVarNamed: aName
												
	^ self classOfBindingOf: aName 
</details>

#### Workspace>>#textStylerClass

<details>
	<summary>See more</summary>
	
	textStylerClass
	^SHTextStylerST80
</details>

#### Workspace>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^[ UndefinedObject methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do: aParsingErrorBlock

</details>

#### Workspace>>#openLabel: aString

Create a standard system view of the model, me, and open it.


<details>
	<summary>See more</summary>
	
	openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	| win |
	win _ WorkspaceWindow editText: self label: aString wrap: true.
	self changed: #actualContents.
	^win
</details>

#### Workspace>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	anAutocompleter computeMessageEntriesForClassOrNil: (self classOfBindingOf: aName) 
</details>

