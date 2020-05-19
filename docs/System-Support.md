## AppLauncher

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## Clipboard

The Clipboard class implements a basic buffering scheme for text. The currently selected text is also exported to the OS so that text can be copied from and to other applications. Commonly only a single instance is used (the default clipboard) but applications are free to use other than the default clipboard if necessary.

### Methods
#### Clipboard>>#retrieveStringOrText

Answer a Text or a String. Appropriate for a Text Editor.


<details>
	<summary>See more</summary>
	
	retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ '' ] ifNotNil: [ answer printString ]
</details>

#### Clipboard>>#noteRecentClipping: text

Keep most recent clippings in a queue for pasteRecent (paste... command)


<details>
	<summary>See more</summary>
	
	noteRecentClipping: text
	"Keep most recent clippings in a queue for pasteRecent (paste... command)"
	(text isString or: [text is: #Text]) ifFalse: [^self ].
	text isEmpty ifTrue: [^ self].
	text size > 50000 ifTrue: [^ self].
	(recent includes: text) ifTrue: [^ self].
	recent addFirst: text.
	[recent size > 5] whileTrue: [recent removeLast].

</details>

#### Clipboard>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	contents _ nil.
	contentsOriginalObjectWeakly _ WeakArray new: 1.
	recent _ OrderedCollection new.
</details>

#### Clipboard>>#extendedClipboardInterface

Answer a subinstance of ExtendedClipboardInterface, if present and operational. Clipboard default extendedClipboardInterface


<details>
	<summary>See more</summary>
	
	extendedClipboardInterface
	"Answer a subinstance of ExtendedClipboardInterface, if present and operational.
	Clipboard default extendedClipboardInterface
	"
	| interface |
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		interface _ clipboardInterface current.
		interface isOperational ifTrue: [
			^ interface ]].
	"No operational extended clipboard."
	^nil
</details>

#### Clipboard>>#retrieveIdOrStringFromOS

Use a specific content type if ExtendedClipboard is active. Otherwise, use regular clipboard primitives


<details>
	<summary>See more</summary>
	
	retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| primitiveFormat |

	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].
		
	primitiveFormat _ self primitiveClipboardString asByteArray.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard.
	We prefer nil"
	primitiveFormat isEmpty ifTrue: [ ^nil ].

	"The VM uses UTF-8 for clipboard"
	^(String fromUtf8: primitiveFormat hex: false trimLastNull: true) withCuisLineEndings
</details>

#### Clipboard>>#primitiveClipboardString: aString

Set the current clipboard to the given string.


<details>
	<summary>See more</summary>
	
	primitiveClipboardString: aString
	"Set the current clipboard to the given string."

	<primitive: 141>
	"don't fail if the primitive is not implemented"
</details>

#### Clipboard>>#retrieveObject

Answer whatever was last stored in the clipboard


<details>
	<summary>See more</summary>
	
	retrieveObject
	"Answer whatever was last stored in the clipboard"
	| stringOrNil |

	"If the OS clipboard has the id for our contents, or the same characters, then answer the richer Smalltalk object.
	Note: if the (extended) clipboard contains a serialized object, it shouldn't contain an id, so
	it is deserialized even if ivar contents contains the object. This is done to guarantee consistency with pasting
	from another Cuis image."
	stringOrNil _ self retrieveIdOrStringFromOS.
	(stringOrNil = (self idFor: contents) or: [ stringOrNil = contents asString])
		ifTrue: [
			"We copy the object, because the result of each paste operation could be modified independently of the others afterwards
			(and the same clipboard contents might be pasted many times)"
			^contents copyForClipboard ].

	"If we have the ExtendedClipboardInterface, try to get an RTF or Form"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface retrieveObject ifNotNil: [ :object | ^object ]].

	"Otherwise answer the string brought by clipboard primitives,
	but if they are not present or fail, use the internal clipboard."
	^stringOrNil ifNil: [ contents copyForClipboard ]
</details>

#### Clipboard>>#retrieveMorph

Answer a Morph or nil.


<details>
	<summary>See more</summary>
	
	retrieveMorph
	"Answer a Morph or nil."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer is: #Morph) ifTrue: [
		^ answer ].

	"If clipboard contains a Form, prepare a Morph including it."
	(answer is: #Form) ifTrue: [
		^ ImageMorph new image: answer ].

	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^ (TextModelMorph withText: answer asText)
			embeddedInMorphicWindowLabeled: 'Text editor' ].
	
	"No useful contents"
	^nil
</details>

#### Clipboard>>#stringOrIdFor: anObject

Clipboard default stringOrIdFor: Smalltalk


<details>
	<summary>See more</summary>
	
	stringOrIdFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isString ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject
</details>

#### Clipboard>>#chooseRecentClipping

Clipboard chooseRecentClipping


<details>
	<summary>See more</summary>
	
	chooseRecentClipping
	"
	Clipboard chooseRecentClipping
	"
	"Choose by menu from among the recent clippings"
	recent isEmpty ifTrue: [ ^ nil ].
	^ (SelectionMenu
		labelList:
			(recent collect: [ :txt |
				((txt asString contractTo: 50)
					withLineEndings: '\') withBlanksCondensed ])
		selections: recent) startUpMenu
</details>

#### Clipboard>>#idFor: anObject

Clipboard default stringOrIdFor: Smalltalk


<details>
	<summary>See more</summary>
	
	idFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	^{anObject class. anObject hash} printString
</details>

#### Clipboard>>#storeObject: anObject

Set new contents on the clipboard. Also export to OS. anObject can be a: String Text Form Morph Object. OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative.


<details>
	<summary>See more</summary>
	
	storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject withCuisLineEndings.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8: true.
	self primitiveClipboardString: primitiveFormat
</details>

#### Clipboard>>#contentsOriginalObject

If not nil, the original object (not a copy!) of what was stored in the clipboard. See #storeObject: Use with care


<details>
	<summary>See more</summary>
	
	contentsOriginalObject
	"If not nil, the original object (not a copy!) of what was stored in the clipboard. See #storeObject: Use with care"

	^ contentsOriginalObjectWeakly at: 1
</details>

#### Clipboard>>#primitiveClipboardString

Get the current clipboard string. Return the empty string if the primitive fails.


<details>
	<summary>See more</summary>
	
	primitiveClipboardString
	"Get the current clipboard string. Return the empty string if the primitive fails."
	<primitive: 141>
	^ ''
</details>

## ExternalSemaphoreTable

By John M McIntosh johnmci@smalltalkconsulting.com This class was written to mange the external semaphore table. When I was writing a Socket test server I discovered various race conditions on the access to the externalSemaphore table. This new class uses class side methods to restrict access using a mutex semaphore. It seemed cleaner to deligate the reponsibility here versus adding more code and another class variable to SystemDictionary Note that in Smalltalk recreateSpecialObjectsArray we still directly play with the table.

### Methods
## ObjectFinalizer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ObjectFinalizer>>#receiver: aReceiver selector: aSelector argument: anObject

<details>
	<summary>See more</summary>
	
	receiver: aReceiver selector: aSelector argument: anObject
	receiver := aReceiver.
	selector := aSelector.
	arguments := Array with: anObject
</details>

#### ObjectFinalizer>>#finalize

Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority.


<details>
	<summary>See more</summary>
	
	finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO guarantee that the resource associated with the receiver hasn't been freed already, so take care that you don't run into trouble - this all may happen with interrupt priority."

	[receiver perform: selector withArguments: arguments] 
		on: Error do:[:ex| ex return].

</details>

## Preference

Represents a true/false flag that is under user control and which can be interrogated by a call to Preferences name a symbol, the formal name of the preference. value a boolean, the current value defaultValue the default value of the preference helpString string or text, constituting the help message localToProject boolean, whether each project holds its own version categoryList list of categories under which to offer this changeInformee whom, if anyone, to inform if the value changes: changeSelector what selector to send to the changeInformee when the value changes

### Methods
#### Preference>>#defaultValue

Answer this preference's defaultValue


<details>
	<summary>See more</summary>
	
	defaultValue
	"Answer this preference's defaultValue"

	^ defaultValue
</details>

#### Preference>>#changeInformee: informee changeSelector: aSelector

Set the changeInformee and changeSelector as specified


<details>
	<summary>See more</summary>
	
	changeInformee: informee changeSelector: aSelector
	"Set the changeInformee and changeSelector as specified"

	changeInformee _ informee.
	changeSelector _ aSelector
</details>

#### Preference>>#printOn: aStream

Print a string decribing the receiver to the given stream


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Print a string decribing the receiver to the given stream"

	super printOn: aStream.
	aStream nextPutAll: name storeString, ' ', value storeString
</details>

#### Preference>>#restoreDefaultValue

restore the default value to the preference


<details>
	<summary>See more</summary>
	
	restoreDefaultValue
	"restore the default value to the preference"

	value _ defaultValue
</details>

#### Preference>>#preferenceValue: aValue

set the value as indicated, and invoke the change selector if appropriate


<details>
	<summary>See more</summary>
	
	preferenceValue: aValue
	"set the value as indicated, and invoke the change selector if appropriate"

	| oldValue |
	oldValue _ value.
	value _ aValue.
	oldValue ~~ value ifTrue:
		[self notifyInformeeOfChange]
</details>

#### Preference>>#name: aName defaultValue: aValue helpString: aString categoryList: aList changeInformee: informee changeSelector:  aChangeSelector

Initialize the preference from the given values. There is an extra tolerence here for the symbols #true, #false, and #nil, which are interpreted, when appropriate, as meaning true, false, and nil


<details>
	<summary>See more</summary>
	
	name: aName defaultValue: aValue helpString: aString categoryList: aList changeInformee: informee changeSelector:  aChangeSelector
	"Initialize the preference from the given values.  There is an extra tolerence here for the symbols #true, #false, and #nil, which are interpreted, when appropriate, as meaning true, false, and nil"

	name _ aName asSymbol.
	defaultValue _ aValue.
	defaultValue = #true ifTrue: [ defaultValue _ true ].
	defaultValue = #false ifTrue: [ defaultValue _ false ].
	value _ defaultValue.
	helpString _ aString.
	categoryList _ (aList ifNil: [OrderedCollection with: #unclassified]) collect:
		[:elem | elem asSymbol].

	changeInformee _ (informee == nil or: [informee == #nil])
						ifTrue: [nil]
						ifFalse:	[(informee isSymbol)
							ifTrue: [
								Smalltalk at: informee]
							ifFalse: [
								informee]].
	changeSelector  _ aChangeSelector
</details>

#### Preference>>#notifyInformeeOfChange

If there is a changeInformee, notify her that I have changed value


<details>
	<summary>See more</summary>
	
	notifyInformeeOfChange
	"If there is a changeInformee, notify her that I have changed value"

	changeInformee ifNotNil: [changeInformee perform: changeSelector]
</details>

#### Preference>>#name

Answer this preference's name


<details>
	<summary>See more</summary>
	
	name
	"Answer this preference's name"

	^ name
</details>

#### Preference>>#preferenceValue

Answer the current value of the preference


<details>
	<summary>See more</summary>
	
	preferenceValue
	"Answer the current value of the preference"

	^ value
</details>

#### Preference>>#togglePreferenceValue

Toggle whether the value of the preference


<details>
	<summary>See more</summary>
	
	togglePreferenceValue
	"Toggle whether the value of the preference"

	value _ value not.
	self notifyInformeeOfChange
</details>

## Preferences

A general mechanism to store preference choices. The default setup treats any symbol as a potential boolean flag; flags unknown to the preference dictionary are always returned as false. To open the control panel: Preferences openFactoredPanel To read how to use the panel (and how to make a preference be per-project): Preferences giveHelpWithPreferences All messages are on the class side. To query a a preference: Preferences logDebuggerStackToFile or some people prefer the more verbose Preferences valueOfFlag: #logDebuggerStackToFile You can make up a new preference any time. Do not define a new message in Preferences class. Accessor methods are compiled automatically when you add a preference as illustrated below: To add a preference (e.g. in the Postscript of a fileout): Preferences addPreference: #samplePreference categories: #(general browsing) default: true balloonHelp: 'This is an example of a preference added by a do-it' projectLocal: false changeInformee: nil changeSelector: nil. To change a preference programatically: Preferences disable: #logDebuggerStackToFile. Or to turn it on, Preferences enable: #logDebuggerStackToFile.

### Methods
#### Preferences>>#seeClassSide

All the code for Preferences is on the class side


<details>
	<summary>See more</summary>
	
	seeClassSide
	"All the code for Preferences is on the class side"
</details>

## RealEstateAgent

Responsible for real-estate management on the screen, which is to say, controlling where new windows appear, with what sizes, etc. 5/20/96 sw

### Methods
## SerializableBlockClosure

A SerializableBlockClosure is a regular Smalltalk object (and hence serializable by ReferenceStream and SmartRefStream), that hold the source code for a BlockClosure, together with the state (originally in outer temps) that the BlockClosure might access. When sent #asEvaluable (even after materializing in the same or different Smalltalk image / machine), the result is a BlockClosure that can be evaluated with identical result as the original. By making BlockClosure>>objectForDataStream: call #asSerializable, we enable serialization and #veryDeepCopy of BlockClosures as if they were regular Smalltalk objects. (The only limitation is that we can't meaningfully handle non-local returns. This limitation also applies to regular BlockClosures if evaluation is attempted when there's nowhere to return to.)

### Methods
#### SerializableBlockClosure>>#asEvaluable

<details>
	<summary>See more</summary>
	
	asEvaluable

	^Compiler evaluate: sourceCode for: self logged: false
</details>

#### SerializableBlockClosure>>#onBlockClosure: aBlockClosure

<details>
	<summary>See more</summary>
	
	onBlockClosure: aBlockClosure

	| both blockNode methodNode indirectTempNames sortedOuterTemps ownNames usedOuterNames sortedUsedOuterNames |
	aBlockClosure hasNonLocalReturn ifTrue: [
		self error: 'Can not serialize closures with non-local returns.' ].
	aBlockClosure sendsToSuper ifTrue: [
		self error: 'Can not currently serialize closures with super sends.' ].
	both _ SerializableClosureDecompiler new  decompileBlockAndMethod: aBlockClosure.
	blockNode _ both first.
	methodNode _ both second.

	indirectTempNames _ methodNode temporaries
		detect: [ :node | node isIndirectTempVector ]
		ifFound: [ :node | node remoteTemps collect: [ :n | n name ]]
		ifNone: [#()].
	sortedOuterTemps _ OrderedCollection new.
	aBlockClosure outerContextsDo: [ :c | c closure ifNotNil: [ :cc |
		| ccn |
		ccn _ cc decompile.
		sortedOuterTemps addAll: ccn arguments; addAll: ccn temporaries ]].
	sortedOuterTemps addAll: methodNode temporaries; addAll: methodNode arguments.

	ownNames _ ((blockNode arguments, blockNode temporaries)
		collect: [ :node | node name ]) asSet.
	usedOuterNames _ Set new.
	blockNode nodesDo: [ :node | node isTemp ifTrue: [
		(ownNames includes: node name) | (indirectTempNames includes: node name) ifFalse: [
			usedOuterNames add: node name]]].

	sortedUsedOuterNames _ sortedOuterTemps select: [ :node |
		usedOuterNames includes: node name ]. "sort them"
	sortedUsedOuterNames _ sortedUsedOuterNames collect: [ :node | node name ].

	blockNode nodesDo: [ :node | node isTemp ifTrue: [ 
		node isRemote
			ifTrue: [node capturedIndex: (indirectTempNames indexOf: node name) ]
			ifFalse: [
				(sortedUsedOuterNames includes: node name)
					ifTrue: [node capturedIndex: (sortedUsedOuterNames indexOf: node name)]]]].

	theSelf _ aBlockClosure receiver.
	capturedValues _ aBlockClosure capturedValues.
	sourceCode _ blockNode decompileString.
</details>

#### SerializableBlockClosure>>#comeFullyUpOnReload: smartRefStream

Normally this read-in object is exactly what we want to store. 7/26/96 tk


<details>
	<summary>See more</summary>
	
	comeFullyUpOnReload: smartRefStream

	^ self asEvaluable
</details>

## SerializableClosureAssignmentNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SerializableClosureAssignmentNode>>#printOn: aStream indent: level

For temps and args local to a block


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level

	"For temps and args local to a block"
	(variable isTemp and: [variable isCapturedByClosure not]) ifTrue: [ ^super printOn: aStream indent: level ].

	"For outer temps, but also for ivars"
	aStream nextPutAll: '('.
	variable printIndirectOn: aStream indent: level.
	aStream nextPutAll: ' put: '.
	value printOn: aStream indent: level.
	aStream nextPutAll: ')'.
</details>

## SerializableClosureDecompiler

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SerializableClosureDecompiler>>#constructorForMethod: aMethod

<details>
	<summary>See more</summary>
	
	constructorForMethod: aMethod

	^SerializableClosureDecompilerConstructor new
</details>

#### SerializableClosureDecompiler>>#decompileBlockAndMethod: aBlock

Decompile aBlock, returning the result as a BlockNode, and the corresponding MethodNode.


<details>
	<summary>See more</summary>
	
	decompileBlockAndMethod: aBlock
	"Decompile aBlock, returning the result as a BlockNode, and the corresponding MethodNode."
	"Decompiler new decompileBlockAndMethod: [3 + 4]"

	| homeMethod methodNode home methodClass methodSelector |
	(home := aBlock home) ifNil: [^ nil].
	homeMethod := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue: [
		methodClass := homeMethod methodClass ifNil: [Object].
		methodSelector := homeMethod selector ifNil: [homeMethod defaultSelector].
		methodNode := self decompile: methodSelector in: methodClass method: homeMethod.
		methodNode
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^{node. methodNode}]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'
</details>

#### SerializableClosureDecompiler>>#pushReceiver

<details>
	<summary>See more</summary>
	
	pushReceiver

	stack addLast: (InstanceVariableNode new
		name: 'theSelf'
		index: 1)
</details>

## SerializableClosureDecompilerConstructor

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SerializableClosureDecompilerConstructor>>#codeTemp: index named: tempName

<details>
	<summary>See more</summary>
	
	codeTemp: index named: tempName

	^ SerializableClosureTempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0
</details>

#### SerializableClosureDecompilerConstructor>>#codeAssignTo: variable value: expression

<details>
	<summary>See more</summary>
	
	codeAssignTo: variable value: expression

	^ SerializableClosureAssignmentNode new variable: variable value: expression
</details>

#### SerializableClosureDecompilerConstructor>>#codeInst: index

<details>
	<summary>See more</summary>
	
	codeInst: index

	^ SerializableClosureInstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1
</details>

#### SerializableClosureDecompilerConstructor>>#codeTemp: index

<details>
	<summary>See more</summary>
	
	codeTemp: index

	^ SerializableClosureTempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0
</details>

## SerializableClosureInstanceVariableNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SerializableClosureInstanceVariableNode>>#printOn: aStream indent: level

If control gets here, avoid recursion loop.


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level 

	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).
</details>

#### SerializableClosureInstanceVariableNode>>#printIndirectOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printIndirectOn: aStream indent: level

	aStream nextPutAll: 'theSelf instVarNamed: ''', name, ''''
</details>

## SerializableClosureTempVariableNode

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SerializableClosureTempVariableNode>>#printOn: aStream indent: level

For temps local to the block


<details>
	<summary>See more</summary>
	
	printOn: aStream indent: level

	"For temps local to the block"
	self isCapturedByClosure ifFalse: [
		^super printOn: aStream indent: level ].

	"For outer temps"
	aStream nextPut: $(.
	self printIndirectOn: aStream indent: level.
	aStream nextPut: $).
</details>

#### SerializableClosureTempVariableNode>>#capturedIndex: idx

<details>
	<summary>See more</summary>
	
	capturedIndex: idx

	capturedIndex _ idx
</details>

#### SerializableClosureTempVariableNode>>#isCapturedByClosure

<details>
	<summary>See more</summary>
	
	isCapturedByClosure

	^capturedIndex notNil
</details>

#### SerializableClosureTempVariableNode>>#printIndirectOn: aStream indent: level

<details>
	<summary>See more</summary>
	
	printIndirectOn: aStream indent: level 
	
	self isRemote
		ifTrue: [ aStream nextPutAll: 'capturedValues last at: ', capturedIndex printString ]
		ifFalse: [ aStream nextPutAll: 'capturedValues at: ', capturedIndex printString ]
</details>

## SharedPool

A shared pool represents a set of bindings which are accessible to all classes which import the pool in its 'pool dictionaries'. SharedPool is NOT a dictionary but rather a name space. Bindings are represented by 'class variables' - as long as we have no better way to represent them at least.

### Methods
## StdIOReadStream

Standard Input Stream. A basic problem/restriction with this code is that currently the VM runs multiple VM threads within a single OS thread. This means that waiting on StdIn blocks the VM, suspending all Smalltalk code.

### Methods
#### StdIOReadStream>>#openOnHandle: aFileID name: streamName

Initialize the instance with the given file handle. N.B. Do _not_ register the stream. We do not want it to be closed implicitly (e.g. on GC). There may be multiple instances accessing the same stream. The stream is not a file.


<details>
	<summary>See more</summary>
	
	openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
	buffer1 _ String new: 1.
	peeked _ false
</details>

#### StdIOReadStream>>#printOn: aStream

Put a printed version of the receiver onto aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Put a printed version of the receiver onto aStream."

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
</details>

#### StdIOReadStream>>#primRead: id into: byteArray startingAt: startIndex count: count

Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read.


<details>
	<summary>See more</summary>
	
	primRead: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self error: 'File read failed'
</details>

#### StdIOReadStream>>#peek

Answer the next byte from this stream, or wait until one becomes available. Warning: all Smalltalk processes are essentially suspended until that happens! Do not advance the stream!


<details>
	<summary>See more</summary>
	
	peek
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!
	
	Do not advance the stream!"

	"Multiple calls to #peek don't make new reads"
	peeked ifFalse: [
		self privateRead.
		peeked _ true ].

	"peeked is always true on exit"
	^buffer1 at: 1
</details>

#### StdIOReadStream>>#next

Answer the next byte from this stream, or wait until one becomes available. Warning: all Smalltalk processes are essentially suspended until that happens!


<details>
	<summary>See more</summary>
	
	next
	"Answer the next byte from this stream, or wait until one becomes available.
	Warning: all Smalltalk processes are essentially suspended until that happens!"

	"If last call was #peek, not #next, then just answer cached value."
	peeked
		ifFalse: [ self privateRead ]
		ifTrue: [ peeked _ false ].

	"peeked is always false on exit"
	^buffer1 at: 1
</details>

#### StdIOReadStream>>#peekFor: aCharacter

Answer false and do not move over the next element if it is not equal to the argument, aCharacter Answer true and increment the position for accessing elements, if the next element is equal to anObject.


<details>
	<summary>See more</summary>
	
	peekFor: aCharacter 
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar _ self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false
</details>

#### StdIOReadStream>>#atEnd

Answer whether the receiver can access any more objects.


<details>
	<summary>See more</summary>
	
	atEnd
	^ false
</details>

#### StdIOReadStream>>#privateRead

Read one Character. Private.


<details>
	<summary>See more</summary>
	
	privateRead
	"Read one Character.
	Private."
	| count |
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	count = 1 ifFalse: [ buffer1 at: 1 put: nil ]
</details>

## StdIOWriteStream

Standard Output/Error Streams.

### Methods
#### StdIOWriteStream>>#nextPut: aCharacter

Write the given character or byte to this file. StdIOWriteStream stdout nextPut: $a; flush.


<details>
	<summary>See more</summary>
	
	nextPut: aCharacter
	"Write the given character or byte to this file.
		 StdIOWriteStream stdout nextPut: $a; flush.
	"
	self nextPutAll: aCharacter asString.
	^aCharacter
</details>

#### StdIOWriteStream>>#primWrite: id from: stringOrByteArray startingAt: startIndex count: count

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

#### StdIOWriteStream>>#openOnHandle: aFileID name: streamName

Initialize the instance with the given file handle. N.B. Do _not_ register the stream. We do not want it to be closed implicitly (e.g. on GC). There may be multiple instances accessing the same stream. The stream is not a file.


<details>
	<summary>See more</summary>
	
	openOnHandle: aFileID name: streamName
	"Initialize the instance with the given file handle.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."

	fileID _ aFileID.
	name _ streamName.
</details>

#### StdIOWriteStream>>#nextPutAll: aStringOrUTF8Bytes

Write the given String (possibly including NCRs) or ByteArray (with utf-8 contents) StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush. StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush. StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush. StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush. StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush. See at the end of this method for a larger example with NCRs for arbitrary Unicode


<details>
	<summary>See more</summary>
	
	nextPutAll: aStringOrUTF8Bytes
	"Write the given String (possibly including NCRs) or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes _ aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8: true ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: '
&#913;&#945; 	Alpha 	
&#914;&#946; 	Beta 	
&#915;&#947; 	Gamma 	
†&#948; 	Delta 	
&#917;„ 	Epsilon 	
&#918;… 	Zeta 	
&#919;&#951; 	Eta 	
&#920;&#952; 	Theta 	
&#921;&#953; 	Iota 	
&#922;&#954; 	Kappa 	
&#923;&#955; 	Lambda 	
&#924;&#956; 	Mu 	
&#925;&#957; 	Nu
&#926;&#958; 	Xi
&#927;&#959; 	Omicron
&#928;ƒ 	Pi
&#929;&#961; 	Rho
&#931;&#963;&#962; 	Sigma
&#932;&#964; 	Tau
&#933;&#965; 	Upsilon
&#934;&#966; 	Phi
&#935;&#967; 	Chi
&#936;&#968; 	Psi
‡&#969; 	Omega
&# 937;&# 969; 	Not a NCR, just regular ASCII chars!
'; flush
"
</details>

#### StdIOWriteStream>>#printOn: aStream

Put a printed version of the receiver onto aStream. 1/31/96 sw


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
</details>

#### StdIOWriteStream>>#primFlush: id

Flush pending changes to the disk


<details>
	<summary>See more</summary>
	
	primFlush: id
	"Flush pending changes to the disk"

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
</details>

#### StdIOWriteStream>>#flush

Flush pending changes


<details>
	<summary>See more</summary>
	
	flush
	"Flush pending changes"
	^self primFlush: fileID
</details>

#### StdIOWriteStream>>#newLine

Append a newLine character to the receiver. The Cuis convention is to use lf on output.


<details>
	<summary>See more</summary>
	
	newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter
</details>

#### StdIOWriteStream>>#space

Append a space character to the receiver.


<details>
	<summary>See more</summary>
	
	space
	"Append a space character to the receiver."

	self nextPut: Character space
</details>

## SystemChangeNotifier

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SystemChangeNotifier>>#evaluated: expression context: aContext

<details>
	<summary>See more</summary>
	
	evaluated: expression context: aContext

	self
		triggerEvent: #evaluated
		withArguments: { expression . aContext }
</details>

#### SystemChangeNotifier>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	silenceLevel _ 0
</details>

#### SystemChangeNotifier>>#classCommented: aClass

A class with the given name was commented in the system.


<details>
	<summary>See more</summary>
	
	classCommented: aClass
	"A class with the given name was commented in the system."

	self
		triggerEvent: #classCommented
		with: aClass
</details>

#### SystemChangeNotifier>>#classRecategorized: aClass from: oldCategory to: newCategory

<details>
	<summary>See more</summary>
	
	classRecategorized: aClass from: oldCategory to: newCategory

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }
</details>

#### SystemChangeNotifier>>#aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #aboutToRenameClass
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }
</details>

#### SystemChangeNotifier>>#classDefinitionChangedFrom: oldClass to: newClass

<details>
	<summary>See more</summary>
	
	classDefinitionChangedFrom: oldClass to: newClass

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass }
</details>

#### SystemChangeNotifier>>#classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }
</details>

#### SystemChangeNotifier>>#methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

<details>
	<summary>See more</summary>
	
	methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	self
		triggerEvent: #methodChanged
		withArguments: { oldMethod . newMethod . aSymbol . aClass . requestor }
</details>

#### SystemChangeNotifier>>#triggerEvent: anEventSelector

Evaluate all actions registered for <anEventSelector>. Return the value of the last registered action.


<details>
	<summary>See more</summary>
	
	triggerEvent: anEventSelector

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector ]
</details>

#### SystemChangeNotifier>>#methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

A method with the given selector was added to aClass in protocol aCategoryName.


<details>
	<summary>See more</summary>
	
	methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self
		triggerEvent: #methodAddedInProtocol
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . requestor }
</details>

#### SystemChangeNotifier>>#classRemoved: aClass fromCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classRemoved: aClass fromCategory: aCategoryName

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName }
</details>

#### SystemChangeNotifier>>#classAdded: aClass inCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classAdded: aClass inCategory: aCategoryName

	self
		triggerEvent: #classAdded
		withArguments: { aClass . aCategoryName }
</details>

#### SystemChangeNotifier>>#doSilently: aBlock

Perform the block, and ensure that no system notification are broadcasted while doing so.


<details>
	<summary>See more</summary>
	
	doSilently: aBlock
	"Perform the block, and ensure that no system notification are broadcasted while doing so."

	silenceLevel := silenceLevel + 1.
	^aBlock ensure: [silenceLevel > 0 ifTrue: [silenceLevel := silenceLevel - 1]]
</details>

#### SystemChangeNotifier>>#classReorganized: aClass

<details>
	<summary>See more</summary>
	
	classReorganized: aClass

	self triggerEvent: #classReorganized with: aClass
</details>

#### SystemChangeNotifier>>#methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

A method with the given selector was removed from the class.


<details>
	<summary>See more</summary>
	
	methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass 
	"A method with the given selector was removed from the class."

	self
		triggerEvent: #methodRemoved
		withArguments: { aMethod . aSymbol . protocol . aClass }
</details>

#### SystemChangeNotifier>>#selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

<details>
	<summary>See more</summary>
	
	selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }
</details>

#### SystemChangeNotifier>>#isBroadcasting

<details>
	<summary>See more</summary>
	
	isBroadcasting

	^ silenceLevel = 0
</details>

#### SystemChangeNotifier>>#triggerEvent: anEventSelector withArguments: anArgumentList

<details>
	<summary>See more</summary>
	
	triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]
</details>

## SystemDictionary

I represent a special dictionary that supports protocol for asking questions about the structure of the system. Other than class names, I contain (print this)... Smalltalk globals

### Methods
#### SystemDictionary>>#defaultSourcesName

Answer the default full path to the sources file corresponding to the image file name.


<details>
	<summary>See more</summary>
	
	defaultSourcesName
	"Answer the default full path to the sources file corresponding to the image file name."
	"
	Smalltalk defaultSourcesName
	"
	^ self imagePath, '/', SourceFileVersionString, '.sources'
</details>

#### SystemDictionary>>#shouldShowFileInErrors

<details>
	<summary>See more</summary>
	
	shouldShowFileInErrors
	
 	^self confirm: self restoringChangesHasErrorsCaption
	
</details>

#### SystemDictionary>>#primitiveErrorTable

Smalltalk primitiveErrorTable


<details>
	<summary>See more</summary>
	
	primitiveErrorTable
	"Smalltalk primitiveErrorTable"
	^self specialObjectsArray at: 52
</details>

#### SystemDictionary>>#lowSpaceWatcher

Wait until the low space semaphore is signalled, then take appropriate actions.


<details>
	<summary>See more</summary>
	
	lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	Debugger interruptProcess: Processor preemptedProcess label: 'Space is low'
</details>

#### SystemDictionary>>#knownInitialsAndNames

This list could include people who hasn't contributed code to the Cuis image, but to some optional package.


<details>
	<summary>See more</summary>
	
	knownInitialsAndNames
	"This list could include people who hasn't contributed code to the Cuis image, but to some optional package."
"
| all ok |
all _ Smalltalk allContributors asSet.
ok _ (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
(all copyWithoutAll: ok) print

		initials         				name"
^ #(
	#('ab' 						'Alexandre Bergel')
	#('abc' 						'Colin Putney')
	#('acg' 						'Andrew C. Greenberg')
	#('ads' 						'Adam Spitz')
	#('AFi' 						'Alain Fischer')
	#('ajh' 						'Anthony Hannan')
	#('al' 						'Adrian Lienhard')
	#('aoy' 						'Andres Otaduy')
	#('apb' 						'Andrew P. Black')
	#('ar' 						'Andreas Raab')
	#('asm' 						'Alejandro Magistrello')
	#('avi' 						'Avi Bryant')
	#('AY'						'Angel Yan')
	#('BenComan' 				'Ben Coman')
	#('bf' 						'Bert Freudenberg')
	#('BG' 						'Boris Gaertner')
	#('bgs' 						'Boris G. Shingarov')
	#('BJP' 						'Bijan Parsia')
	#('bkv' 						'Brent Vukmer')
	#('bolot' 					'Bolot Kerimbaev')
	#('bp' 						'Bernhard Pieber')
	#('BP' 						'Brent Pinkney') 
	#('brp' 						'Brent Pinkney')
	#('cbc' 						'Chris Cunningham')
	#('cbr'						'Casey Ransberger')
	#('ccn' 						'Chris Norton')
	#('cmm' 						'Chris Muller')
	#('crl' 						'Craig Latta')
	#('cwp' 						'Colin Putney')
	#('das' 						'David A Smith')
	#('dc' 						'Damien Cassou')
	#('dew' 						'Doug Way')
	#('dgd' 						'Diego Gomez Deck')
	#('dkh'						'Dale Henrichs')
	#('dhn'	 					'Dan Norton')
	#('dhhi' 					'Dan Ingalls')
	#('di' 						'Dan Ingalls')
	#('djp' 						'David J. Pennell')
	#('DKL'						'Daniel K Lyons')
	#('DM' 						'Duncan Mak')
	#('DSM' 						'Duane Maxwell')
	#('DSG'						'David Graham')
	#('dtl' 						'Dave Lewis')
	#('dvf'	 					'Daniel Vainsencher')
	#('eat' 						'Eric Arseneau Tremblay')
	#('EB'						'Eric Brandwein')
	#('eem'						'Eliot Emilio Miranda')
	#('eliot'					'Eliot Emilio Miranda')
	#('efc' 						'Eddie Cottongim')
	#('em' 						'Ernest Micklei?')
	#('emm' 						'Ernest Micklei')
	#('fbs' 						'Frank Shearar')
	#('FBS' 						'Frank Shearar')
	#('fc' 						'Frank Caggiano')
	#('fcs' 						'Frank Sergeant')
	#('FernandoOlivero' 		'Fernando Olivero')
	#('FernanodOlivero' 		'Fernando Olivero')
	#('FGJ'						'Fernando Gasperi Jabalera')
	#('FJG'				 		'Facundo Javier Gelatti')
	#('GabrielOmarCotelli' 	'Gabriel Omar Cotelli')
	#('GC' 						'Gastón Caruso')
	#('gera' 					'Gerardo Richarte')
	#('gh' 						'Goran Krampe (nee Hultgren)')
	#('gk' 						'Goran Krampe (nee Hultgren)')
	#('gm' 						'German Morales')
	#('go' 						'Georg Gollmann')
	#('gsa' 						'German Arduino')
	#('HAW' 						'Hernan Wilkinson')
	#('HB' 						'Hari Balaraman')
	#('hjh' 						'Hannes Hirzel')
	#('hmm' 						'Hans-Martin Mosner')
	#('hsj' 						'Henrik Sperre Johansen')
	#('Igor.Stasenko' 		'Igor Stasenko')
	#('ikp' 						'Ian Piumarta')
	#('Jb' 						'Jean Baptiste Arnaud')
	#('jcg' 						'Joshua Gargus')
	#('jdr' 						'Javier Diaz-Reinoso')
	#('je' 						'Joern Eyrich')
	#('jf' 						'Julian Fitzell')
	#('JF' 						'Julian Fitzell')
	#('jhm' 						'John Maloney')
	#('jk'						'Jonathan Kelly')
	#('jlb' 						'Jim Benson')
	#('jm' '						John Maloney')
	#('jmb' 						'Hans Baveco')
	#('JMG'						'Jeff Gonis')
	#('JMM' 						'John McIntosh')
	#('jmv' 						'Juan Vuletich')
	#('JMV' 						'Juan Vuletich')
	#('JO'						'Javier Olaechea')
	#('jp' 						'Joseph Pelrine')
	#('jrm' 						'John-Reed Maffeo')
	#('jrp' 						'John Pierce')
	#('jsp' 						'Jeff Pierce')
	#('KenD' 					'Ken Dickey')
	#('kfr' 						'Karl Ramberg')
	#('KLC'			 			'Ken Causey')
	#('KLG'			 			'Gerald Klix')
	#('kph'						'Keith Hodges')
	#('KTT' 						'Kurt Thams')
	#('laza' 					'Alexander Lazarevic')
	#('LC' 						'Leandro Caniglia')
	#('len' 						'Luciano Esteban Notarfrancesco')
	#('lpc'						'Laura Perez Cerrato')
	#('lr' 						'Lukas Renggli')
	#('Lukas Renggli' 		'Lukas Renggli')
	#('ls' 						'Lex Spoon')
	#('md' 						'Marcus Denker')
	#('MarcusDenker' 			'Marcus Denker')
	#('marcus.denker' 		'Marcus Denker')
	#('mdr' 						'Mike Rutenberg')
	#('mga' 						'Markus Galli')
	#('mha' 						'Michael Haupt')
	#('mir' 						'Michael Rueger')
	#('mjg' 						'Mark Guzdial')
	#('mk'	 					'Matej Kosik')
	#('MM'	 					'Mariano Montone')
	#('MPH'	 					'Michael Hewner')
	#('mpw' 						'Marcel Weiher')
	#('MPW' 						'Marcel Weiher')
	#('mrm' 						'Martin McClure')
	#('mtf' 						'Matthew Fulmer')
	#('mu' 						'Masashi Umezawa')
	#('nb' 						'Naala Brewer')
	#('nice'					 	'Nicolas Cellier')
	#('nk' 						'Ned Konz')
	#('nop' 						'Jay Carlson')
	#('NS' 						'Nathanael Schaerli')
	#('panda' 					'Michael Rueger')
	#('pb'						'Phil Bellalouna')
	#('pmon'						'Paolo Montrasi')
	#('PHK' 						'Peter Keeler')
	#('Pmm' 						'Philippe Marschall')
	#('pnm' 						'Paul McDonough')
	#('r++' 						'Gerardo Richarte')
	#('raa' 						'Bob Arning')
	#('RAA' 						'Bob Arning')
	#('raok' 					'Richard A. O''Keefe')
	#('rca' 						'Russell Allen')
	#('reThink'				 	'Paul McDonough')
	#('rew' 						'Roger Whitney')
	#('rhi' 						'Robert Hirschfeld')
	#('RJT' 						'Ron Teitelbaum')
	#('RNG' 						'Nahuel Garbezza')
	#('rr' 						'Romain Robbes')
	#('rss' 						'Ron Spengler')
	#('rw' 						'Robert Withers')
	#('rww' 						'Robert Withers')
	#('Sames' 					'Samuel S. Shuster')
	#('sbw' 						'Stephan B. Wessels')
	#('sd' 						'Stephane Ducasse')
	#('SD' 						'Stephane Ducasse')
	#('sge' 						'Steve Elkins')
	#('sjd' 						'Santiago Jose Dandois')
	#('SLD'						'Steve Davies')
	#('sma' 						'Stefan Matthias Aust')
	#('sps' 						'Steven Swerling')
	#('SqR' 						'Andres Valloud')
	#('sqr' 						'Andres Valloud')
	#('sr' 						'Stephan Rudlof')
	#('ss'						'Sebastian Sujarchuk')
	#('SSS' 						'Samuel S. Shuster')
	#('stephane.ducasse' 	'Stephane Ducasse')
	#('stephaneducasse'	 	'Stephane Ducasse')
	#('stp' 						'Stephen Travis Pope')
	#('sumim' 					'Masato Sumi')
	#('svp' 						'Stephen Vincent Pair')
	#('sw' 						'Scott Wallace')
	#('TAG' 						'Travis Griggs')
	#('tak' 						'Takashi Yamamiya')
	#('tao' 						'Tim Olson')
	#('TBn' 						'Torsten Bergmann')
	#('tfei' 					'The Fourth Estate, Inc.')
	#('tfel' 					'Tim Felgentreff')
	#('th' 						'Torge Husfeldt')
	#('tk' 						'Ted Kaehler')
	#('tlk' 						'Tom Koenig')
	#('tpr' 						'Tim Rowledge')
	#('TPR' 						'Tim Rowledge')
	#('tween' 					'Andy Tween')
	#('ul' 						'Levente Uzonyi')
	#('vb' 						'Vassili Bykov')
	#('ward' 					'Ward Cunningham')
	#('wiz' 						'Jerome Peace')
	#('wod' 						'Bill Dargel')
	#('yo' 						'Yoshiki Ohshima')
	#('zz' 						'Serge Stinckwich'))
</details>

#### SystemDictionary>>#obsoleteClasses

Smalltalk obsoleteClasses inspect


<details>
	<summary>See more</summary>
	
	obsoleteClasses   "Smalltalk obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"
	| obs c |
	obs _ OrderedCollection new.  Smalltalk garbageCollect.
	Metaclass allInstancesDo:
		[:m | c _ m soleInstance.
		(c notNil and: ['AnOb*' match: c name asString])
			ifTrue: [obs add: c]].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"
"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found _ false.
	1 to: m numLiterals do:
		[:i | (((l _ m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found _ true]].
	found]
"
</details>

#### SystemDictionary>>#internalizeSources

Smalltalk internalizeSources


<details>
	<summary>See more</summary>
	
	internalizeSources
		"
		Smalltalk internalizeSources
		"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles first) ifNil: [
		^ self halt: 'Cannot locate Sources file so cannot proceed.'].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	(aFile _ SourceFiles last) ifNil: [
		^ self halt: 'Cannot locate Changes so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'
</details>

#### SystemDictionary>>#logChange: aStringOrText preamble: preambleOrNil

Write the argument, aString, onto the changes file.


<details>
	<summary>See more</summary>
	
	logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString _ aStringOrText asString.
	aString firstNoBlankIndex = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString ]]
</details>

#### SystemDictionary>>#allMethodsWithString: aString

Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring. 2/1/96 sw. The search is case-sensitive, and does not dive into complex literals, confining itself to string constants. 5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser


<details>
	<summary>See more</summary>
	
	allMethodsWithString: aString
	"Answer a sorted Collection of all the methods that contain, in a string literal, aString as a substring.  2/1/96 sw.  The search is case-sensitive, and does not dive into complex literals, confining itself to string constants.
	5/2/96 sw: fixed so that duplicate occurrences of aString in the same method don't result in duplicated entries in the browser"
	| aStringSize list |
	aStringSize _ aString size.
	list _ Set new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(class compiledMethodAt: sel) literalsDo: [ :aLiteral |
				((aLiteral isMemberOf: String) and: [ aLiteral size >= aStringSize ]) ifTrue: [
					(aLiteral includesSubString: aString) ifTrue: [
						list add: 
							(MethodReference new
								setStandardClass: class 
								methodSymbol: sel) ]]]]].
	^ list asArray sort
</details>

#### SystemDictionary>>#allImplementorsOf: aSelector  localTo: aClass

Answer a sorted Collection of all the methods that implement the message aSelector in, above, or below the given class.


<details>
	<summary>See more</summary>
	
	allImplementorsOf: aSelector  localTo: aClass
	"Answer a sorted Collection of all the methods that implement the message 
	aSelector in, above, or below the given class."

	| aSet cls |
	aSet _ Set new.
	cls _ aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [
				aSet add: (MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector)]].
	cls class withAllSuperAndSubclassesDoGently: [ :class |
		(class includesSelector: aSelector)
			ifTrue: [
				aSet add: (MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector)]].
	^aSet asArray sort
</details>

#### SystemDictionary>>#tagTail

<details>
	<summary>See more</summary>
	
	tagTail

	^ self tagHeader
</details>

#### SystemDictionary>>#allUnSentMessagesIn: selectorSet

Answer the subset of selectorSet which are not sent anywhere in the system.


<details>
	<summary>See more</summary>
	
	allUnSentMessagesIn: selectorSet
	"Answer the subset of selectorSet which are not sent anywhere in the system."

	^ selectorSet copyWithoutAll: self allSentMessages
</details>

#### SystemDictionary>>#processCommandLineArguments

Smalltalk processCommandLineArguments


<details>
	<summary>See more</summary>
	
	processCommandLineArguments
	"
	Smalltalk processCommandLineArguments
	"
	| rawArgStream |
	startUpScriptArguments _ Array streamContents: [ :strm |
		rawArgStream _ startUpArguments readStream.
		[ rawArgStream atEnd ] whileFalse: [
			self processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: strm ]]
</details>

#### SystemDictionary>>#useUpMemoryWithArrays

For testing the low space handler...


<details>
	<summary>See more</summary>
	
	useUpMemoryWithArrays 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithArrays"

	| b |  "First use up most of memory."
	b _ String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b _ b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | Array new: 10000]
</details>

#### SystemDictionary>>#snapshotTagFor: save andQuit: quit

<details>
	<summary>See more</summary>
	
	snapshotTagFor: save andQuit: quit
		
	^save
		ifTrue: [ quit
			ifTrue: [ self quitTag ]
			ifFalse: [ self snapshotTag ]]
		ifFalse: [ quit
			ifTrue: [ self quitNoSaveTag ]
			ifFalse: [ self nopTag ]]
</details>

#### SystemDictionary>>#startUpScriptArguments

To make command line arguments to startup scripts available to them


<details>
	<summary>See more</summary>
	
	startUpScriptArguments
	"To make command line arguments to startup scripts available to them"

	^startUpScriptArguments
</details>

#### SystemDictionary>>#interpreterClass

Interpreter class (Cog VM only) nil for classic Interpreter VM


<details>
	<summary>See more</summary>
	
	interpreterClass
	"Interpreter class (Cog VM only)
	nil for classic Interpreter VM
	"
	^self getSystemAttribute: 1007
</details>

#### SystemDictionary>>#externalizeSources

Write the sources and changes streams onto external files.


<details>
	<summary>See more</summary>
	
	externalizeSources   
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName _ self defaultSourcesName.
	sourcesName asFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: sourcesName asFileEntry readStream.

	changesName _ self defaultChangesName.
	changesName  asFileEntry writeStreamDo: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: changesName asFileEntry appendStream.

	self inform: 'Sources successfully externalized'
</details>

#### SystemDictionary>>#timeStamp: aStream

Writes system version and current time on stream aStream.


<details>
	<summary>See more</summary>
	
	timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime _ DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: Smalltalk version;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream
</details>

#### SystemDictionary>>#restoreLostChangesAutomaticallyFrom: aChangesFile

<details>
	<summary>See more</summary>
	
	restoreLostChangesAutomaticallyFrom: aChangesFile

	| changeList |

	changeList := ChangeListWithFileInErrors new.
	changeList scanFile: aChangesFile from: LastQuitLogPosition to: aChangesFile size.
	changeList fileInAllKeepingErrors.
	(changeList hasFileInErrors and: [ self shouldShowFileInErrors ]) ifTrue: [ changeList showChangesWithFileInErrors ]

</details>

#### SystemDictionary>>#isLiveTypingInstalled

<details>
	<summary>See more</summary>
	
	isLiveTypingInstalled
	
	^(FeatureRequirement name: #LiveTyping) isAlreadySatisfied
</details>

#### SystemDictionary>>#storeDataOn: aDataStream

I don't get stored. Use a DiskProxy


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store me'
</details>

#### SystemDictionary>>#useUpMemory

For testing the low space handler...


<details>
	<summary>See more</summary>
	
	useUpMemory
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemory"

	| lst |
	lst _ nil.
	[true] whileTrue: [
		lst _ Link nextLink: lst.
	].
</details>

#### SystemDictionary>>#allUnSentMessages

Smalltalk allUnSentMessages


<details>
	<summary>See more</summary>
	
	allUnSentMessages   "Smalltalk allUnSentMessages"
	"Answer the set of selectors that are implemented by some object in the system but not sent by any."

	^ self allUnSentMessagesWithout: #(#() #())
</details>

#### SystemDictionary>>#profileSample

Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active. See also primitiveProfileStart.


<details>
	<summary>See more</summary>
	
	profileSample
	"Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSample'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#hasToRestoreChanges

<details>
	<summary>See more</summary>
	
	hasToRestoreChanges

	^Preferences checkLostChangesOnStartUp and: [ 
		self withChangesFileDo: [ :changesFile | self hasToRestoreChangesFrom: changesFile ]].
	
</details>

#### SystemDictionary>>#allClassesDo: aBlock

Evaluate the argument, aBlock, for each class in the system.


<details>
	<summary>See more</summary>
	
	allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system."

	(self classNames collect: [:name | self at: name]) do: aBlock
</details>

#### SystemDictionary>>#restoreLostChanges

<details>
	<summary>See more</summary>
	
	restoreLostChanges

 	| decision |
	
	decision := PopUpMenu withCaption: self lostChangesDetectedCaption chooseFrom: self restoreLostChangesOptions.

	decision = 1 ifTrue: [ ^self restoreLostChangesAutomatically ].
	decision = 2 ifTrue: [ ^self restoreLostChangesManually ]
</details>

#### SystemDictionary>>#removeFromStartUpList: aClass

<details>
	<summary>See more</summary>
	
	removeFromStartUpList: aClass

	StartUpList remove: aClass name ifAbsent: nil
</details>

#### SystemDictionary>>#unloadModule: aString

Primitive. Unload the given module. This primitive is intended for development only since some platform do not implement unloading of DLL's accordingly. Also, the mechanism for unloading may not be supported on all platforms.


<details>
	<summary>See more</summary>
	
	unloadModule: aString
	"Primitive. Unload the given module.
	This primitive is intended for development only since some
	platform do not implement unloading of DLL's accordingly.
	Also, the mechanism for unloading may not be supported
	on all platforms."
	<primitive: 571>
	^self primitiveFailed
</details>

#### SystemDictionary>>#browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise

Smalltalk browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise


<details>
	<summary>See more</summary>
	
	browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				hasIndirectTemps and: [  anyClosureDoesNonLocalReturn not and: [ anyClosureUsesSelf not ] ] ].
			])
		name: ' Closures that write to outer temps, but clean otherwise'
</details>

#### SystemDictionary>>#removeClassNamedIfInBaseSystem: aName

Invoked from fileouts: if there is currently a class in the system named aName, and it is not part of a package, then remove it. If anything untoward happens, report it in the Transcript.


<details>
	<summary>See more</summary>
	
	removeClassNamedIfInBaseSystem: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, and it is not part of a package, then remove it.
	If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
"		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'."
		^ self].

	CodePackage
		packageOfClass: oldClass
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			oldClass removeFromSystem ]
</details>

#### SystemDictionary>>#saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag

Save the image under a new name.


<details>
	<summary>See more</summary>
	
	saveAs: newName andQuit: aBoolean clearAllClassState: clearAllStateFlag
	"Save the image  under a new name."

	| newChangesName |
	self currentChangesName ifNotNil: [ :oldChangesName |
		self closeSourceFiles. "so copying the changes file will always work"
		newChangesName _ self fullNameForChangesNamed: newName.
		FileIOAccessor default copy: oldChangesName asFileEntry to: newChangesName asFileEntry.
		ChangesInitialFileSize ifNotNil: [
			oldChangesName asFileEntry appendStreamDo: [ :strm | strm truncate: ChangesInitialFileSize ]]].

	self 
		changeImageNameTo: (self fullNameForImageNamed: newName);
		closeSourceFiles; openSourceFiles;  "so SNAPSHOT appears in new changes file"
		snapshot: true andQuit: aBoolean
		clearAllClassState: clearAllStateFlag
</details>

#### SystemDictionary>>#classRemoved: aClass fromCategory: aCategoryName

<details>
	<summary>See more</summary>
	
	classRemoved: aClass fromCategory: aCategoryName

	| classDefinition |
	
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self 
				logChange: classDefinition 
				preamble: 'classRemoval: ', aClass name printString, Utilities changeStampField ]
</details>

#### SystemDictionary>>#profileSemaphore: aSemaphore

Primitive. Install the semaphore to be used for profiling, or nil if no semaphore should be used. See also primitiveProfileStart.


<details>
	<summary>See more</summary>
	
	profileSemaphore: aSemaphore
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#closeSourceFiles

Shut down the source files if appropriate. 1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file


<details>
	<summary>See more</summary>
	
	closeSourceFiles
	"Shut down the source files if appropriate.  1/29/96 sw: changed so that the closing and nilification only take place if the entry was a FileStream, thus allowing stringified sources to remain in the saved image file"

	1 to: 2 do: [ :i |
		(SourceFiles at: i) ifNotNil: [ :strm |
			strm isFileStream
				ifTrue: [
					strm close.
					SourceFiles at: i put: nil ]]]
</details>

#### SystemDictionary>>#imageFormatVersion

Answer an integer identifying the type of image in memory. The image version number may identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements of the image (e.g. block closure support required). This invokes an optional primitive that may not be available on all virtual machines. Answer nil if unknown.


<details>
	<summary>See more</summary>
	
	imageFormatVersion
	"Answer an integer identifying the type of image in memory. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines.
	Answer nil if unknown."

	"
	Smalltalk imageFormatVersion
	"

	<primitive: 'primitiveImageFormatVersion'>

	"Cog provides a VM parameter"
	^Smalltalk vmParameterAt: 41
</details>

#### SystemDictionary>>#quitTag

<details>
	<summary>See more</summary>
	
	quitTag
		
	^'QUIT'
</details>

#### SystemDictionary>>#allSentMessages

Answer the set of selectors which are sent somewhere in the system.


<details>
	<summary>See more</summary>
	
	allSentMessages
	"Answer the set of selectors which are sent somewhere in the system."

	^ self allSentMessagesWithout: #(#() #())
</details>

#### SystemDictionary>>#calcEndianness

What endian-ness is the current hardware? The String '1234' will be stored into a machine word. On BigEndian machines (the Mac), $1 will be the high byte if the word. On LittleEndian machines (the PC), $4 will be the high byte.


<details>
	<summary>See more</summary>
	
	calcEndianness
	| bytes word blt |
	"What endian-ness is the current hardware?  The String '1234' will be stored into a machine word.  On BigEndian machines (the Mac), $1 will be the high byte if the word.  On LittleEndian machines (the PC), $4 will be the high byte."
	"Smalltalk endianness"

	bytes _ ByteArray withAll: #(0 0 0 0).  "(1 2 3 4) or (4 3 2 1)"
	word _ WordArray with: 16r01020304.
	blt _ (BitBlt toForm: (Form new hackBits: bytes)) 
				sourceForm: (Form new hackBits: word).
	blt combinationRule: Form over.  "store"
	blt sourceY: 0; destY: 0; height: 1; width: 4.
	blt sourceX: 0; destX: 0.
	blt copyBits.  "paste the word into the bytes"
	bytes first = 1 ifTrue: [^ #big].
	bytes first = 4 ifTrue: [^ #little].
	self error: 'Ted is confused'.
</details>

#### SystemDictionary>>#stopProfiling

Stop profiling the virtual machine.


<details>
	<summary>See more</summary>
	
	stopProfiling
	"Stop profiling the virtual machine."

	<primitive: 253>

</details>

#### SystemDictionary>>#browseObsoleteReferences

Smalltalk browseObsoleteReferences


<details>
	<summary>See more</summary>
	
	browseObsoleteReferences   "Smalltalk browseObsoleteReferences"
	| references |
	references _ OrderedCollection new.
	(LookupKey allSubInstances select:
		[:x | ((x value isKindOf: Behavior) and: ['AnOb*' match: x value name]) or:
		['AnOb*' match: x value class name]]) 
		do: [:x | references addAll: (Smalltalk allCallsOn: x)].
	Smalltalk 
		browseMessageList: references 
		name: 'References to Obsolete Classes'
</details>

#### SystemDictionary>>#browseMethodsWithClosuresThatAccessOuterTemps

Smalltalk browseMethodsWithClosuresThatAccessOuterTemps


<details>
	<summary>See more</summary>
	
	browseMethodsWithClosuresThatAccessOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatAccessOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				anyClosureHasCopied ].
			])
		name: 'Closures that read or write to outer temps'
</details>

#### SystemDictionary>>#allPrimitiveMessages

Answer an OrderedCollection of all the methods that are implemented by primitives.


<details>
	<summary>See more</summary>
	
	allPrimitiveMessages
	"Answer an OrderedCollection of all the methods that are implemented by 
	primitives."

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel | 
			method _ class compiledMethodAt: sel.
			method primitive ~= 0 ifTrue: [
				aColl addLast: class name , ' ' , sel 
					, ' ' , method primitive printString]]].
	^aColl
</details>

#### SystemDictionary>>#longRunningPrimitive

Primitive. Answer an Array of the current long-running primitive method identified by the heartbeat, and the number of heartbeats it ran for, or nil if none. To use this, first install a semaphore to wait upon via SystemDictionary>>longRunningPrimitiveSemaphore:


<details>
	<summary>See more</summary>
	
	longRunningPrimitive
	"Primitive. Answer an Array of the current long-running primitive method identified by the
	 heartbeat, and the number of heartbeats it ran for, or nil if none.  To use this, first install a
	 semaphore to wait upon via SystemDictionary>>longRunningPrimitiveSemaphore:"
	<primitive: 'primitiveLongRunningPrimitive'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#compactClassesArray

Smalltalk compactClassesArray


<details>
	<summary>See more</summary>
	
	compactClassesArray  "Smalltalk compactClassesArray"
	"Return the array of 31 classes whose instances may be
	represented compactly"
	^ Smalltalk specialObjectsArray at: 29
</details>

#### SystemDictionary>>#browseAllCallsOn: literal1 and: literal2

Create and schedule a message browser on each method that calls on the two Symbols, literal1 and literal2. For example, Smalltalk browseAllCallsOn: #at: and: #at:put:.


<details>
	<summary>See more</summary>
	
	browseAllCallsOn: literal1 and: literal2 
	"Create and schedule a message browser on each method that calls on the 
	two Symbols, literal1 and literal2. For example, Smalltalk 
	browseAllCallsOn: #at: and: #at:put:."

	^self 
		browseMessageList: (self allCallsOn: literal1 and: literal2)
		name: literal1 printString , ' -and- ' , literal2 printString
</details>

#### SystemDictionary>>#isSnapshotRecord: chunk

<details>
	<summary>See more</summary>
	
	isSnapshotRecord: chunk

	^chunk beginsWith: self tagHeader, self snapshotTag 
	
</details>

#### SystemDictionary>>#removedUnusedClassesAndMethods

<details>
	<summary>See more</summary>
	
	removedUnusedClassesAndMethods
	[
		#hereWeGo print.
		Smalltalk unusedClasses do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		Smalltalk removeAllUnSentMessages > 0 or: [ Smalltalk unusedClasses notEmpty ]] whileTrue.
</details>

#### SystemDictionary>>#snapshot: save andQuit: quit clearAllClassState: clearAllStateFlag

<details>
	<summary>See more</summary>
	
	snapshot: save andQuit: quit clearAllClassState: clearAllStateFlag
	save
		ifTrue: [
			self okayToSave ifFalse: [ ^ self ].
			ChangeSet zapAllChangeSets ]
		ifFalse: [
			quit ifTrue: [
				self okayToDiscardUnsavedCode ifFalse: [ ^ self ]]].
	^ self
		snapshot: save
		andQuit: quit
		embedded: false
		clearAllClassState: clearAllStateFlag
</details>

#### SystemDictionary>>#processStartUpList: isARealStartup

Send #startUp to each class that needs to run initialization after a snapshot.


<details>
	<summary>See more</summary>
	
	processStartUpList: isARealStartup
	"Send #startUp to each class that needs to run initialization after a snapshot."

	EndianCache _ self calcEndianness.
	self send: #startUp: toClassesNamedIn: StartUpList with: isARealStartup
</details>

#### SystemDictionary>>#allClasses

Return all the class defines in the Smalltalk SystemDictionary


<details>
	<summary>See more</summary>
	
	allClasses  
	"Return all the class defines in the Smalltalk SystemDictionary"
	"
	Smalltalk allClasses
	"

	^ self classNames collect: [:name | self at: name]
</details>

#### SystemDictionary>>#imageName: newName

Set the the full path name for the current image. All further snapshots will use this.


<details>
	<summary>See more</summary>
	
	imageName: newName
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed
</details>

#### SystemDictionary>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^ 'Image'
</details>

#### SystemDictionary>>#allPrimitiveMethodsInCategories: aList

Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories. 1/26/96 sw


<details>
	<summary>See more</summary>
	
	allPrimitiveMethodsInCategories: aList
	"Answer an OrderedCollection of all the methods that are implemented by primitives in the given categories.  1/26/96 sw"

	| aColl method | 
	aColl _ OrderedCollection new: 200.
	self allBehaviorsDo: [ :aClass |
		(aList includes: (SystemOrganization categoryOfElement: aClass theNonMetaClass name asString) asString)
			ifTrue: [
				aClass selectorsDo: [ :sel | 
					method _ aClass compiledMethodAt: sel.
					method primitive ~= 0
						ifTrue: [
							aColl addLast: aClass name , ' ' , sel 
								, ' ' , method primitive printString]]]].
	^ aColl

"Smalltalk allPrimitiveMethodsInCategories: #('Collections-Streams' 'Files-Streams' 'Files-Abstract' 'Files-Macintosh')"
</details>

#### SystemDictionary>>#flushClassNameCache

Smalltalk flushClassNameCache


<details>
	<summary>See more</summary>
	
	flushClassNameCache
	"Smalltalk flushClassNameCache"
	"Forse recomputation of the cached list of class names."

	cachedClassNames _ nil.
	cachedNonClassNames _ nil
</details>

#### SystemDictionary>>#hasClassNamed: aString

Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned. 4/29/96 sw Smalltalk hasClassNamed: 'Morph' Smalltalk hasClassNamed: 'Display' Smalltalk hasClassNamed: 'xMorph'


<details>
	<summary>See more</summary>
	
	hasClassNamed: aString
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw
	Smalltalk hasClassNamed: 'Morph'
	Smalltalk hasClassNamed: 'Display'
	Smalltalk hasClassNamed: 'xMorph'
	"

	Symbol hasInterned: aString ifTrue: [ :aSymbol |
		self at: aSymbol ifPresent: [ :global | ^global class isMeta ]].
	^ false
</details>

#### SystemDictionary>>#clearProfile

Clear the profile database.


<details>
	<summary>See more</summary>
	
	clearProfile
	"Clear the profile database."

	<primitive: 250>

</details>

#### SystemDictionary>>#currentSourcesName

Answer the full path to the version-stable source code currently in use Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)


<details>
	<summary>See more</summary>
	
	currentSourcesName
	"Answer the full path to the version-stable source code currently in use
	Answer nil if not a file (i.e. if internalized with #internalizeSources, or closed with #closeSourceFiles)"
	| sources |
	sources _ SourceFiles first.
	^sources isFileStream
		ifTrue: [ sources name ]
</details>

#### SystemDictionary>>#allReferencesToLiteral: aLiteral

<details>
	<summary>See more</summary>
	
	allReferencesToLiteral: aLiteral
	| coll |
	coll := OrderedCollection new.
	Smalltalk allBehaviorsDo: [ :eaClass |
		eaClass
			addMethodsTo: coll
			thatReferenceTo: aLiteral
			special: false
			byte: nil ].
	^ coll.
</details>

#### SystemDictionary>>#readCommandLineArguments

Smalltalk readCommandLineArguments


<details>
	<summary>See more</summary>
	
	readCommandLineArguments
	"
	Smalltalk readCommandLineArguments
	"
	| arg i |
	startUpArguments _ Array streamContents: [ :strm |
		i _ 2.
		[ i <= 1000 and: [ (arg _ self getSystemAttribute: i) notNil ] ] whileTrue: [
			strm nextPut: arg.
			i _ i + 1 ]]
</details>

#### SystemDictionary>>#saveSession

<details>
	<summary>See more</summary>
	
	saveSession
	self snapshot: true andQuit: false clearAllClassState: false
</details>

#### SystemDictionary>>#primGetCurrentWorkingDirectoryUnix

Call getcwd() to get the current working directory. Shamelessly taken from OSProcess.pck.st and copied here. Answer nil on fail (i.e., non-Unix)


<details>
	<summary>See more</summary>
	
	primGetCurrentWorkingDirectoryUnix
	"Call getcwd() to get the current working directory.
	Shamelessly taken from OSProcess.pck.st and copied here.
	Answer nil on fail (i.e., non-Unix)"

	"
	Smalltalk primGetCurrentWorkingDirectoryUnix
	"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'UnixOSProcessPlugin'>
	^ nil
</details>

#### SystemDictionary>>#quitPrimitive: exitCode

Primitive. Exit with exitCode to another operating system on the host machine, if one exists. All state changes in the object space since the last snapshot are lost. Ignore exitCode if it's not supported by the VM.


<details>
	<summary>See more</summary>
	
	quitPrimitive: exitCode
	"Primitive. Exit with exitCode to another operating system on the host machine, if one exists.
	All state changes in the object space since the last snapshot are lost.
	Ignore exitCode if it's not supported by the VM."

	<primitive: 113>
	self quitPrimitive
</details>

#### SystemDictionary>>#allSymbolsIn: anArray do: aBlock

Recursively dig into anArray and evaluate aBlock on any Symbols found


<details>
	<summary>See more</summary>
	
	allSymbolsIn: anArray do: aBlock
	"Recursively dig into anArray and evaluate aBlock on any Symbols found"
	"allSymbols and variableBindings"
	
	anArray do: [ :elem |
		((elem isMemberOf: Symbol) or: [ elem isVariableBinding ])
			ifTrue: [ aBlock value: elem ]
			ifFalse: [
				(elem isMemberOf: Array) ifTrue: [ 
					self allSymbolsIn: elem do: aBlock ]]]
</details>

#### SystemDictionary>>#browseMethodsWithSourceString: aString

Smalltalk browseMethodsWithSourceString: 'SourceString'


<details>
	<summary>See more</summary>
	
	browseMethodsWithSourceString: aString
	"Smalltalk browseMethodsWithSourceString: 'SourceString' "
	"Launch a browser on all methods whose source code contains aString as a substring."

	| caseSensitive suffix selectString |
	Sensor controlKeyPressed ifTrue: [
		selectString _ ((aString findTokens: '*') sorted: [ :a :b | a size > b size ]) first.
		^ self 
			browseMessageList: (self allMethodsSourceStringMatching: '*', aString, '*')
			name: 'Methods matched by ' , aString printString , ' (no ctrl for exact match)'
			autoSelect: selectString
		].
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [' (case-sensitive. Use ctrl for $* pattern match.)']
		ifFalse: [' (shift for case-sensitive. ctrl for $* pattern match.)'].
	^ self 
		browseMessageList: (self allMethodsWithSourceString: aString matchCase: caseSensitive)
		name: 'Methods containing ' , aString printString , suffix
		autoSelect: aString
</details>

#### SystemDictionary>>#currentChangesName

Answer the full path to the version-stable source code currently in use. Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)


<details>
	<summary>See more</summary>
	
	currentChangesName
	"Answer the full path to the version-stable source code currently in use.
	Answer nil if not a file (i.e. if internalized with #internalizeChangeLog or #internalizeSources, or closed with #closeSourceFiles)"
	| changes |
	changes _ SourceFiles second.
	^changes isFileStream
		ifTrue: [ changes name ]
</details>

#### SystemDictionary>>#browseMethodsWithOnlyCleanClosures

Smalltalk browseMethodsWithOnlyCleanClosures


<details>
	<summary>See more</summary>
	
	browseMethodsWithOnlyCleanClosures
	"
	Smalltalk browseMethodsWithOnlyCleanClosures
	"
	self
		browseMessageList: (
			self allSelect: [ :m | 
				self eliotsClosureMeasurementsOn: m over: [ :closuresCount
						:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
					closuresCount > 0 and: [
						(anyClosureHasCopied or: [ anyClosureDoesNonLocalReturn or: [ anyClosureUsesSelf ]]) not ].
				]
			])
		name: 'Methods with only Clean Closures'
</details>

#### SystemDictionary>>#recompileAllFrom: firstName

Recompile all classes, starting with given name.


<details>
	<summary>See more</summary>
	
	recompileAllFrom: firstName 
	"Recompile all classes, starting with given name."

	self allClassesDo: [ :class |
		class name >= firstName
			ifTrue: [
				Transcript show: class name; newLine.
				class compileAll]]

	"Smalltalk recompileAllFrom: 'AAABodyShop'."

</details>

#### SystemDictionary>>#storeOn: aStream

Refer to the comment in Object|storeOn:.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	self == Smalltalk ifTrue: [
		^ aStream nextPutAll: 'Smalltalk'].
	^ super storeOn: aStream
</details>

#### SystemDictionary>>#vmVersion

Smalltalk vmVersion


<details>
	<summary>See more</summary>
	
	vmVersion	"Smalltalk vmVersion"
	"Return a string identifying the interpreter version"
	^self getSystemAttribute: 1004
</details>

#### SystemDictionary>>#setStartupStamp

<details>
	<summary>See more</summary>
	
	setStartupStamp

	| dateAndTime |
	dateAndTime _ DateAndTime now.
	StartupStamp _ String streamContents: [ :stream |
		stream nextPutAll: '----STARTUP---- ('.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream
			nextPutAll: ') as ';
			nextPutAll: Smalltalk imageName ] 

</details>

#### SystemDictionary>>#hasSpecialSelector: aLiteral ifTrueSetByte: aBlock

<details>
	<summary>See more</summary>
	
	hasSpecialSelector: aLiteral ifTrueSetByte: aBlock

	1 to: self specialSelectorSize do:
		[:index | 
		(self specialSelectorAt: index) == aLiteral
			ifTrue: [aBlock value: index + 16rAF. ^true]].
	^false
</details>

#### SystemDictionary>>#okayToSave

Answer true unless the user cancels saving because of some warning given.


<details>
	<summary>See more</summary>
	
	okayToSave
	"Answer true unless the user cancels saving because of some warning given."

	| wasCog isCog |
	isCog _ Smalltalk isRunningCog.
	[ wasCog _ self imageFormatVersionFromFile allMask: 1 ]
		on: Error
		do: [ :ignore |
			"probably save-as to non-existing file"
			^ true ].

	(isCog and: [wasCog not]) ifTrue: [
		(self confirm: 'You''re running with a Cog VM.', String newLineString,
			'Non-Cog VMs might not be able to open images saved under Cog!', String newLineString,
			'(If you choose "YES", you might only use this image under Cog VMs.)', String newLineString,
			'(If you choose "NO", you might save your work in some other way, and later exit Cuis without saving).', String newLineString,
			'Really save?')
				ifFalse: [ ^false ]].
		
	^ true
</details>

#### SystemDictionary>>#setGCParameters

Adjust the VM's default GC parameters to avoid premature tenuring.


<details>
	<summary>See more</summary>
	
	setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring."

	Smalltalk isSpur
		ifTrue: [
			| proportion edenSize survivorSize averageObjectSize numObjects |
			proportion := 0.9. "tenure when 90% of pastSpace is full"
			edenSize := self vmParameterAt: 44.
			survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
			averageObjectSize := 8 * self wordSize. "a good approximation"
			numObjects := (proportion * survivorSize / averageObjectSize) rounded.
			self vmParameterAt: 6 put: numObjects  "tenure when more than this many objects survive the GC"
			]
		ifFalse: [
			Smalltalk vmParameterAt: 5 put: 4000.  "do an incremental GC after this many allocations"
			Smalltalk vmParameterAt: 6 put: 2000.  "tenure when more than this many objects survive the GC"
			]
</details>

#### SystemDictionary>>#profileStart: counter

Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results). The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore. If the argument is less or equal to zero, it disables profiling.


<details>
	<summary>See more</summary>
	
	profileStart: counter
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	"Not an interrupt check-counter, but #primHighResClock"
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#getFileNameFromUser

<details>
	<summary>See more</summary>
	
	getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: self imageName asFileEntry name.
	newName isEmpty ifTrue: [ ^nil ].
	((self fullNameForImageNamed: newName) asFileEntry exists or: [
		(self fullNameForChangesNamed: newName) asFileEntry exists ] ) ifTrue: [
			(self confirm: ('{1} already exists. Overwrite?' format: {newName}))
				ifFalse: [ ^nil ]].
	^newName

</details>

#### SystemDictionary>>#imageName

Answer the full path name for the current image.


<details>
	<summary>See more</summary>
	
	imageName
	"Answer the full path name for the current image."
	"
	Smalltalk imageName
	"

	| answer |
	answer _ self primImageName.
	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer _ answer capitalized ].
	^answer ifNil: [ '' ]
</details>

#### SystemDictionary>>#version

Answer the version of this release.


<details>
	<summary>See more</summary>
	
	version
	"Answer the version of this release."

	^SystemVersion current version
</details>

#### SystemDictionary>>#allClassesImplementing: aSelector

Answer an Array of all classes that implement the message aSelector.


<details>
	<summary>See more</summary>
	
	allClassesImplementing: aSelector  
	"Answer an Array of all classes that implement the message aSelector."

	^ Array streamContents: [ :strm |
		self allBehaviorsDo: [ :class |
			(class includesSelector: aSelector)
				ifTrue: [ strm nextPut: class ]]]
</details>

#### SystemDictionary>>#closuresInfoStringForClass: aClass selector: aSelector

Smalltalk closuresInfoStringFor: PlayingWithClosures class >> #exp01Argument


<details>
	<summary>See more</summary>
	
	closuresInfoStringForClass: aClass selector: aSelector
	"
	Smalltalk closuresInfoStringFor: PlayingWithClosures class >> #exp01Argument
	"
	| answer all someDo noneDoes method |
	method _ aClass compiledMethodAt: aSelector ifAbsent: [ ^'' ].
	self eliotsClosureMeasurementsOn: method over: [ 
				:closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
			
		closuresCount > 0
			ifFalse: [ answer _ 'No real (non-optimized) Closures' ]
			ifTrue: [
				closuresCount = 1
					ifTrue: [
						answer _ '1 Closure: '.
						all _ ''.
						someDo _ 'does'. 
						noneDoes _ 'does not' ]
					ifFalse: [
						answer _ closuresCount printString, ' Closures: '.
						all _ 'all '.
						someDo _ 'some do'. 
						noneDoes _ 'none does' ].
				(anyClosureHasCopied or: [ anyClosureDoesNonLocalReturn or: [ anyClosureUsesSelf ]])
					ifFalse: [ answer _ answer, all, 'clean' ]
					ifTrue: [
						answer _ answer, (anyClosureHasCopied
							ifTrue: [
								hasIndirectTemps
									ifTrue: [ someDo, ' write (and maybe ', someDo, ' read)' ]
									ifFalse: [ someDo, ' read (but ', noneDoes, ' write)' ] ]
							ifFalse: [ noneDoes, ' access' ]), ' outer temps; '.
						answer _ answer, (anyClosureDoesNonLocalReturn
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' ^return; '.
						answer _ answer, (anyClosureUsesSelf
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' use self'
					].
			]
		].
		^answer
</details>

#### SystemDictionary>>#browseAllAccessesTo: instVarName from: aClass

Create and schedule a Message Set browser for all the receiver's methods or any methods of a subclass/superclass that refer to the instance variable name.


<details>
	<summary>See more</summary>
	
	browseAllAccessesTo: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."

	"self new browseAllAccessesTo: 'contents' from: Collection."
		
	^ self 
		browseMessageList: (aClass allAccessesTo: instVarName)
		name: 'Accesses to ' , instVarName 
		autoSelect: instVarName
</details>

#### SystemDictionary>>#obsoleteMethodReferences

Smalltalk obsoleteMethodReferences Smalltalk browseObsoleteMethodReferences Open a browser on all referenced behaviors that are obsolete


<details>
	<summary>See more</summary>
	
	obsoleteMethodReferences
	"
	Smalltalk obsoleteMethodReferences
	Smalltalk browseObsoleteMethodReferences
	Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during: [ :barBlock |
	obsClasses keysAndValuesDo: [ :index :each |
		barBlock value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do: [ :ref |
			"Figure out if it may be a global"
			(ref isVariableBinding and: [ ref key isString "or Symbol" ]) ifTrue: [
				(self pointersTo: ref) do: [ :meth |
					(meth is: #CompiledMethod) ifTrue: [
						meth methodReference ifNotNil: [ :mref |
							(mref isValid and: [ mref compiledMethod == meth]) ifTrue: [
								references nextPut: mref ]]]]]]].
	].
	^references contents
</details>

#### SystemDictionary>>#allUnSentMessagesWithout: classesAndMessagesPair

Answer the set of selectors that are implemented but not sent, computed in the absence of the supplied classes and messages.


<details>
	<summary>See more</summary>
	
	allUnSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors that are implemented but not sent, computed in the absence of the supplied classes and messages."

	^ (self allImplementedMessagesWithout: classesAndMessagesPair)
		copyWithoutAll: (self allSentMessagesWithout: classesAndMessagesPair)
</details>

#### SystemDictionary>>#at: aKey put: anObject

Override from Dictionary to check Undeclared and fix up references to undeclared variables.


<details>
	<summary>See more</summary>
	
	at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."

	| association |
	(self includesKey: aKey) ifFalse: [
		self flushClassNameCache.

		"Update existing association if there is one."
		(Undeclared includesKey: aKey) ifTrue: [
			association _ self declare: aKey from: Undeclared.
			association value: anObject.
			^ anObject ]].

	"Update existing association if there is one."
	^super at: aKey put: anObject
</details>

#### SystemDictionary>>#garbageCollectMost

Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space.


<details>
	<summary>See more</summary>
	
	garbageCollectMost
	"Primitive. Reclaims recently created garbage (which is usually most of it) fairly quickly and answers the number of bytes of available space."

	<primitive: 131>
	^ self primBytesLeft
</details>

#### SystemDictionary>>#tagHeader

<details>
	<summary>See more</summary>
	
	tagHeader

	^ '----'
</details>

#### SystemDictionary>>#isLiveSmalltalkImage

<details>
	<summary>See more</summary>
	
	isLiveSmalltalkImage
	^ true
</details>

#### SystemDictionary>>#specialSelectorAt: anInteger

Answer the special message selector from the interleaved specialSelectors array.


<details>
	<summary>See more</summary>
	
	specialSelectorAt: anInteger 
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1
</details>

#### SystemDictionary>>#extraVMMemory: extraBytesToReserve

Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter.


<details>
	<summary>See more</summary>
	
	extraVMMemory: extraBytesToReserve
	"Request that the given amount of extra memory be reserved for use by the virtual machine to leave extra C heap space available for things like plugins, network and file buffers, and so on. This request is stored when the image is saved and honored when the image is next started up. Answer the previous value of this parameter."

	extraBytesToReserve < 0
		ifTrue: [self error: 'VM memory reservation must be non-negative'].
	^ Smalltalk vmParameterAt: 23 put: extraBytesToReserve

</details>

#### SystemDictionary>>#osVersion

Return the version number string of the platform we're running on


<details>
	<summary>See more</summary>
	
	osVersion
	"Return the version number string of the platform we're running on"
	^(self getSystemAttribute: 1002) asString
</details>

#### SystemDictionary>>#removeKey: key ifAbsent: aBlock

Remove key (and its associated value) from the receiver. If key is not in the receiver, answer the result of evaluating aBlock. Otherwise, answer the value externally named by key.


<details>
	<summary>See more</summary>
	
	removeKey: key ifAbsent: aBlock
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."
	self flushClassNameCache.
	^super removeKey: key ifAbsent: aBlock
</details>

#### SystemDictionary>>#confirmRemovalOf: aSelector on: aClass

Determine if it is okay to remove the given selector. Answer 1 if it should be removed, 2 if it should be removed followed by a senders browse, and 3 if it should not be removed.


<details>
	<summary>See more</summary>
	
	confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu answer caption allCalls |
	allCalls _ self allCallsOn: aSelector.
	(count _ allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	aMenu _ PopUpMenu labels: 'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption _ 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption _ caption copyWith: $s].
	answer _ aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoSelect: aSelector keywords first].
	answer = 0
		ifTrue: [answer _ 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3
</details>

#### SystemDictionary>>#changeImageNameTo: aString

<details>
	<summary>See more</summary>
	
	changeImageNameTo: aString
	self imageName: aString.
	LastImageName _ self imageName
</details>

#### SystemDictionary>>#browseAllImplementorsOfList: selectorList title: aTitle

Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example, Smalltalk browseAllImplementorsOf: #(at:put: size).


<details>
	<summary>See more</summary>
	
	browseAllImplementorsOfList: selectorList title: aTitle
	"Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example,  Smalltalk browseAllImplementorsOf: #(at:put: size)."

	| implementorLists flattenedList |

	implementorLists _ selectorList collect: [:each | self allImplementorsOf: each].
	flattenedList _ Array streamContents: [ :stream |
		implementorLists do: [ :each | stream nextPutAll: each]].
	flattenedList sort.
	^ self browseMessageList: flattenedList name: aTitle
</details>

#### SystemDictionary>>#browseAllUnimplementedCalls

Create and schedule a message browser on each method that includes a message that is not implemented in any object in the system. Smalltalk browseAllUnimplementedCalls


<details>
	<summary>See more</summary>
	
	browseAllUnimplementedCalls
	"Create and schedule a message browser on each method that includes a 
	message that is not implemented in any object in the system.
	Smalltalk browseAllUnimplementedCalls
	"

	^self browseMessageList: self allUnimplementedCalls name: 'Unimplemented calls'
</details>

#### SystemDictionary>>#logSnapshot: save andQuit: quit

Log quitting to changes file


<details>
	<summary>See more</summary>
	
	logSnapshot: save andQuit: quit
	"Log quitting to changes file"
	| msg |
	self assureStartupStampLogged.
	msg _ self snapshotMessageFor: save andQuit: quit.
	(SourceFiles at: 2) ifNotNil: [
		save ifTrue: [
			LastQuitLogPosition _ (SourceFiles at: 2)
				 setToEnd;
				 position ]].
	self logChange: msg.
	Transcript
		 newLine;
		 show: msg;
		 newLine
</details>

#### SystemDictionary>>#browseMethodsWithClosuresThatWriteOuterTemps

Smalltalk browseMethodsWithClosuresThatWriteOuterTemps


<details>
	<summary>See more</summary>
	
	browseMethodsWithClosuresThatWriteOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				hasIndirectTemps ].
			])
		name: ' Closures that write to outer temps'
</details>

#### SystemDictionary>>#processPreemptionYields: aBoolean

The Cog VM can be instructed not to yield on process preemption, i.e. not to put a preempted process at the back of its run queue. By default preempting a process causes it to yield (Blue Book semantics) which can have unfortunate effects. This flag persists across snapshots, stored in the image header.


<details>
	<summary>See more</summary>
	
	processPreemptionYields: aBoolean
	"The Cog VM can be instructed not to yield on process preemption,
	 i.e. not to put a preempted process at the back of its run queue.  By
	 default preempting a process causes it to yield (Blue Book semantics)
	 which can have unfortunate effects.
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 4) + (aBoolean ifTrue: [0] ifFalse: [4])
</details>

#### SystemDictionary>>#openSourceFiles

<details>
	<summary>See more</summary>
	
	openSourceFiles
	self imageName = LastImageName ifFalse: [
		LastImageName _ self imageName.
		"Reset the author initials to blank when the image gets moved"
		Utilities clearAuthor.
		"And clear any absolute path that is most likely invalid now."
		CodePackage clearPaths ].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install
</details>

#### SystemDictionary>>#dumpProfile

Dump the profile database to a file.


<details>
	<summary>See more</summary>
	
	dumpProfile
	"Dump the profile database to a file."

	<primitive: 251>

</details>

#### SystemDictionary>>#globals

Smalltalk globals


<details>
	<summary>See more</summary>
	
	globals
	"
	Smalltalk  globals
	"
	
	| globals |
	globals _ IdentityDictionary new.
	self associationsDo: [ :assoc |
		assoc value class isMeta not ifTrue: [ globals add: assoc ]].
	^globals
</details>

#### SystemDictionary>>#startUpArguments

To make all raw command line arguments available to applications


<details>
	<summary>See more</summary>
	
	startUpArguments
	"To make all raw command line arguments available to applications"

	^startUpArguments
</details>

#### SystemDictionary>>#exitToDebugger

Primitive. Enter the machine language debugger, if one exists. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	exitToDebugger
	"Primitive. Enter the machine language debugger, if one exists. Essential.
	See Object documentation whatIsAPrimitive."

	<primitive: 114>
	self primitiveFailed
</details>

#### SystemDictionary>>#printStuffToCleanOnImageSave

So far, to serve experiments. Some day, when this is empty, we'll be closer to bootstrap from sources.


<details>
	<summary>See more</summary>
	
	printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n _ 0.
	""
	'--------' print.
	'Proceeses: ' print.
	((Process allSubInstances reject: [:each | each isTerminated ])
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars _ cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n _ n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars _ Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v _ cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m _ m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print
</details>

#### SystemDictionary>>#showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock

Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters. Evaluate choiceBlock if a message is chosen.


<details>
	<summary>See more</summary>
	
	showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock
	"Show a sorted menu of the given selectors, preceded by firstItem, and all
	abbreviated to 40 characters.  Evaluate choiceBlock if a message is chosen."

	^ self showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: nil
</details>

#### SystemDictionary>>#allMethodsInCategory: category

<details>
	<summary>See more</summary>
	
	allMethodsInCategory: category 
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :x |
		(x organization listAtCategoryNamed: category) do: [ :sel |
			aCollection add: (MethodReference new
					setStandardClass: x 
					methodSymbol: sel)]].
	^aCollection
</details>

#### SystemDictionary>>#okayToDiscardUnsavedCode

Answer true unless the user cancels quitting because of some warning given. Smalltalk okayToDiscardUnsavedCode


<details>
	<summary>See more</summary>
	
	okayToDiscardUnsavedCode
	"Answer true unless the user cancels quitting because of some warning given.
	Smalltalk okayToDiscardUnsavedCode
	"

	| baseCSdirty dirtyPackages |
	baseCSdirty _ ChangeSet allChangeSets anySatisfy: [ :any | any isForBaseSystem and: [ any hasUnsavedChanges and: [ any isEmpty not ]]].
	dirtyPackages _ CodePackage installedPackages anySatisfy: [ :pck | pck hasUnsavedChanges ].

	baseCSdirty & dirtyPackages ifTrue: [
		^self confirm: 'There are both unsaved Packages', String newLineString,
			'and unsaved Changes to Cuis core.', String newLineString,
			'If you continue, they will all be lost.', String newLineString,
			'Continue?' ].
	baseCSdirty ifTrue: [
		^self confirm: 'Some ChangeSet for Cuis core might have unsaved changes.', String newLineString,
			'If you continue, they would be lost.', String newLineString,
			'Continue?' ].
	dirtyPackages ifTrue: [
		^self confirm: 'There are unsaved Packages.', String newLineString,
			'If you continue, they will all be lost.', String newLineString,
			'Continue?' ].

	^true
</details>

#### SystemDictionary>>#methodsWithUnboundGlobals

Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty.


<details>
	<summary>See more</summary>
	
	methodsWithUnboundGlobals
	"Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty."
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
	"
	^ self allSelect: [ :m |
		m literals anySatisfy: [ :l |
			l isVariableBinding and: [
				l key isSymbol and: [
					"avoid class-side methodClass literals"
					(m methodClass bindingOf: l key)
						ifNil: [
							(Undeclared
								associationAt: l key
								ifAbsent: [ ]) ~~ l ]
						ifNotNil: [ :b |
							b ~~ l ]]]]]
</details>

#### SystemDictionary>>#isLittleEndian

<details>
	<summary>See more</summary>
	
	isLittleEndian
	^self endianness == #little
</details>

#### SystemDictionary>>#allContributors

Smalltalk allContributors


<details>
	<summary>See more</summary>
	
	allContributors
"
	Smalltalk allContributors
"
	| answer author |
	answer _ Set new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			author notEmpty ifTrue: [
				answer add: author ]]].
	^answer
</details>

#### SystemDictionary>>#restoringChangesHasErrorsCaption

<details>
	<summary>See more</summary>
	
	restoringChangesHasErrorsCaption
	
	^'There were errors filing in the lost changes. Do you want to see them?'
</details>

#### SystemDictionary>>#showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption

Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters. Use aCaption as the menu title, if it is not nil. Evaluate choiceBlock if a message is chosen.


<details>
	<summary>See more</summary>
	
	showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption
	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."
	| index menuLabels sortedList aMenu |
	sortedList _ selectorCollection asArray sort.
	menuLabels _ String streamContents: [ :strm |
		strm nextPutAll: (firstItem contractTo: 40).
		sortedList do: [ :sel |
			strm
				 newLine;
				 nextPutAll: (sel contractTo: 40) ]].
	aMenu _ PopUpMenu
		labels: menuLabels
		lines: #(1 ).
	index _ aCaption
		ifNil: [ aMenu startUpMenu ]
		ifNotNil: [ aMenu startUpWithCaption: aCaption ].
	index = 1 ifTrue: [ choiceBlock value: firstItem ].
	index > 1 ifTrue: [ choiceBlock value: (sortedList at: index - 1) ].
</details>

#### SystemDictionary>>#primVmPath

Answer the path for the directory containing the Smalltalk virtual machine. Return nil if this primitive is not implemented.


<details>
	<summary>See more</summary>
	
	primVmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return nil if this primitive is not implemented."
	"Smalltalk primVmPath"

	<primitive: 142>
</details>

#### SystemDictionary>>#stopLowSpaceWatcher

Stop the process to watch for low-space conditions.


<details>
	<summary>See more</summary>
	
	stopLowSpaceWatcher
	"Stop the process to watch for low-space conditions."
	"
	Smalltalk
		stopLowSpaceWatcher;
		installLowSpaceWatcher
	"

	self primSignalAtBytesLeft: 0.  "disable low-space interrupts"
	LowSpaceProcess ifNotNil: [LowSpaceProcess terminate].
	LowSpaceProcess _ nil
</details>

#### SystemDictionary>>#prepareToRenameClass: aClass from: oldName to: newName

Rename the class, aClass, to have the title newName.


<details>
	<summary>See more</summary>
	
	prepareToRenameClass: aClass from: oldName to: newName

	"Rename the class, aClass, to have the title newName."

	| oldref i category |

	category := SystemOrganization categoryOfElement: oldName.
	SystemOrganization classify: newName under: category.
	SystemOrganization removeElement: oldName.
	oldref _ self associationAt: oldName.
	self removeKey: oldName.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i _ list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	
	SystemChangeNotifier uniqueInstance aboutToRenameClass: aClass from: oldName to: newName inCategory: category.
	
</details>

#### SystemDictionary>>#description

Needed by CodeFileBrowser to use Smalltalk as the 'case' source


<details>
	<summary>See more</summary>
	
	description
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name
</details>

#### SystemDictionary>>#allMethodsSourceStringMatching: aString

Answer a sorted Collection of all the methods that contain, in source code, aString as a substring. Search the class comments also. Argument might include $*, that matches any subsequence. For example, try: ensure:*[*close*]


<details>
	<summary>See more</summary>
	
	allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring. 
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].
					
					(aString match: cl organization classComment asString) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.
</details>

#### SystemDictionary>>#printElementsOn: aStream

<details>
	<summary>See more</summary>
	
	printElementsOn: aStream
	aStream nextPutAll:'(lots of globals)'
</details>

#### SystemDictionary>>#imageFormatVersionFromFileAsIs

Answer an integer identifying the type of image on file. The image version number may identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements of the image (e.g. block closure support required). If the image file has a different endianness than the VM, the format version will appear byte-swapped.


<details>
	<summary>See more</summary>
	
	imageFormatVersionFromFileAsIs
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). If the image file has a different
	endianness than the VM, the format version will appear byte-swapped."
	"
	Smalltalk imageFormatVersionFromFileAsIs
	"
	^ Smalltalk imageName asFileEntry readStreamDo: [ :stream |
		(stream binary; next: 4)
			unsignedLongAt: 1
			bigEndian: Smalltalk isBigEndian ]
</details>

#### SystemDictionary>>#lowSpaceThreshold

Answer the low space threshold. When the amount of free memory (after garbage collection) falls below this limit, the system is in serious danger of completely exhausting memory and crashing. This limit should be made high enough to allow the user open a debugger to diagnose a problem or to save the image. In a stack-based VM such as Cog contexts for activations in the stack zone will have to be created as the debugger opens, requiring additional headroom.


<details>
	<summary>See more</summary>
	
	lowSpaceThreshold 
	"Answer the low space threshold. When the amount of free memory (after garbage collection)
	 falls below this limit, the system is in serious danger of completely exhausting memory and
	 crashing. This limit should be made high enough to allow the user open a debugger to diagnose
	 a problem or to save the image.  In a stack-based VM such as Cog contexts for activations in
	 the stack zone will have to be created as the debugger opens, requiring additional headroom."

	| slotsForDebugger slotsForContextsOnStackPages |
	slotsForDebugger := 65536. "Arbitrary guess"
	slotsForContextsOnStackPages :=
		(self vmParameterAt: 42)
			ifNil: [0]
			ifNotNil:
				[:numStackPages| | headerSize numActivationsPerPage maxContextSize |
				numActivationsPerPage := 40. "Design goal of the Cog VM"
				headerSize := 2. "64-bytes for Spur"
				maxContextSize := MethodContext instSize + CompiledMethod fullFrameSize + headerSize.
				numStackPages * numActivationsPerPage * maxContextSize].
	^slotsForDebugger + slotsForContextsOnStackPages * self wordSize
</details>

#### SystemDictionary>>#isThereAnImplementorOf: aSelector exceptFor: aCollectionOfBehaviors

Answer whether if there is at least some other implementor of the selector besides aBehavior.


<details>
	<summary>See more</summary>
	
	isThereAnImplementorOf: aSelector exceptFor: aCollectionOfBehaviors
	"Answer whether if there is at least some other implementor of the selector besides aBehavior."

	self allBehaviorsDo: [ :class |
		((aCollectionOfBehaviors includes: class) not and: [class includesSelector: aSelector])
			ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"
</details>

#### SystemDictionary>>#assureStartupStampLogged

If there is a startup stamp not yet actually logged to disk, do it now.


<details>
	<summary>See more</summary>
	
	assureStartupStampLogged
	"If there is a startup stamp not yet actually logged to disk, do it now."
	| changesFile directory oldUserChanges oldUserChangesName |
	StartupStamp ifNil: [^ self].
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			changesFile nextChunkPut: StartupStamp asString; newLine.
			self forceChangesToDisk ]].
	Preferences autoNumberUserChanges ifTrue: [
		oldUserChanges _ Smalltalk defaultUserChangesName asFileEntry.
		oldUserChanges exists ifTrue: [
			directory _ oldUserChanges parent.
			oldUserChangesName _ directory nextNameFor: oldUserChanges nameWithoutExtension extension: 'changes'.
			oldUserChanges rename: oldUserChangesName ]].
	Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
		stream newLine; newLine.
		stream nextChunkPut: StartupStamp asString; newLine ].
	StartupStamp _ nil
</details>

#### SystemDictionary>>#allMethodsWithSourceString: aString matchCase: caseSensitive

Answer a sorted Collection of all the methods that contain, in source code, aString as a substring. Search the class comments also


<details>
	<summary>See more</summary>
	
	allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment asString
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.
</details>

#### SystemDictionary>>#processPreemptionYields

Answer whether the VM causes a process to yield on process preemption, i.e. to put a preempted process at the back of its run queue. If the parameter is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields. Smalltalk processPreemptionYields


<details>
	<summary>See more</summary>
	
	processPreemptionYields
	"Answer whether the VM causes a process to yield on process preemption,
	 i.e. to put a preempted process at the back of its run queue.  If the parameter
	 is unavailable (non-Cog VMs) or bit 2 (4) is 0 then preemption yields.
	Smalltalk processPreemptionYields
	"

	^ (self vmParameterAt: 48)
		ifNil: [ true ]
		ifNotNil: [ :properties |
			(properties allMask: 4) not ]
</details>

#### SystemDictionary>>#browseMyChanges

Browse only the changes (in the changes file) by the current author. Smalltalk browseMyChanges


<details>
	<summary>See more</summary>
	
	browseMyChanges
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method fileIndex > 1 "only look at changes file"
               and: [ method timeStamp beginsWith: Utilities authorInitials, ' ' ]]
</details>

#### SystemDictionary>>#browseAllImplementorsOf: selector localTo: aClass

Create and schedule a message browser on each method in or below the given class that implements the message whose selector is the argument, selector. For example, Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary.


<details>
	<summary>See more</summary>
	
	browseAllImplementorsOf: selector localTo: aClass
	"Create and schedule a message browser on each method in or below the given class
	that implements the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put: localTo: Dictionary."

	aClass ifNil: [ ^self inform: 'no class selected' ].
	^self browseMessageList: (self allImplementorsOf: selector localTo: aClass)
		name: 'Implementors of ' , selector, ' local to ', aClass name
</details>

#### SystemDictionary>>#clearExternalObjects

Clear the array of objects that have been registered for use in non-Smalltalk code.


<details>
	<summary>See more</summary>
	
	clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Smalltalk clearExternalObjects"

	ExternalSemaphoreTable clearExternalObjects

</details>

#### SystemDictionary>>#renamedClass: aClass from: oldName

<details>
	<summary>See more</summary>
	
	renamedClass: aClass from: oldName 
	
	| newName |
	
	newName := aClass name.
	
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: aClass category.
	Smalltalk 
		logChange: 'Smalltalk renameClassNamed: #', oldName, ' as: #', newName
		preamble: 'classRenamed: #', oldName, ' as: #', newName, Utilities changeStampField
</details>

#### SystemDictionary>>#primBytesLeft

Primitive. Answer the number of bytes available for new object data. Not accurate unless preceded by Smalltalk garbageCollectMost (for reasonable accuracy), or Smalltalk garbageCollect (for real accuracy). See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	primBytesLeft
	"Primitive. Answer the number of bytes available for new object data.
	Not accurate unless preceded by
		Smalltalk garbageCollectMost (for reasonable accuracy), or
		Smalltalk garbageCollect (for real accuracy).
	See Object documentation whatIsAPrimitive."

	<primitive: 112>
	^ 0
</details>

#### SystemDictionary>>#specialSelectorSize

Answer the number of special selectors in the system.


<details>
	<summary>See more</summary>
	
	specialSelectorSize
	"Answer the number of special selectors in the system."

	^ (self specialObjectsArray at: 24) size // 2
</details>

#### SystemDictionary>>#listLoadedModule: index

Return the name of the n-th loaded module. This list is not sorted!


<details>
	<summary>See more</summary>
	
	listLoadedModule: index
	"Return the name of the n-th loaded module.
	This list is not sorted!"
	<primitive: 573>
	^self primitiveFailed
</details>

#### SystemDictionary>>#allSentMessagesWithout: classesAndMessagesPair

Answer the set of selectors which are sent somewhere in the system, computed in the absence of the supplied classes and messages.


<details>
	<summary>See more</summary>
	
	allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
		"The following may be sent without being in any literal frame"
		1 to: self specialSelectorSize do: [ :index | 
			sent add: (self specialSelectorAt: index)].
	Smalltalk presumedSentMessages do: [ :sel | sent add: sel].
	^ sent
</details>

#### SystemDictionary>>#browseClassCommentsWithString: aString

Smalltalk browseClassCommentsWithString: 'my instances'


<details>
	<summary>See more</summary>
	
	browseClassCommentsWithString: aString
	"Smalltalk browseClassCommentsWithString: 'my instances' "
	"Launch a message list browser on all class comments containing aString as a substring."
	| caseSensitive suffix list |
	suffix _ (caseSensitive _ Sensor shiftPressed)
		ifTrue: [ ' (case-sensitive)' ]
		ifFalse: [ ' (use shift for case-sensitive)' ].
	list _ Set new.
	Smalltalk allClassesDo: [ :class |
		(class organization classComment asString
			findString: aString
			startingAt: 1
			caseSensitive: caseSensitive) > 0 ifTrue: [
			list add:
				(MethodReference new
					setStandardClass: class
					methodSymbol: #Comment) ]].
	^ self
		browseMessageList: list asArray sort
		name: 'Class comments containing ', aString printString, suffix
		autoSelect: aString.
</details>

#### SystemDictionary>>#verifyChanges

Smalltalk verifyChanges


<details>
	<summary>See more</summary>
	
	verifyChanges		"Smalltalk verifyChanges"
	"Recompile all methods in the changes file."
	Smalltalk allBehaviorsDo: [:class | class recompileChanges].

</details>

#### SystemDictionary>>#longRunningPrimitiveSemaphore: aSemaphoreOrNil

Primitive. Install the semaphore to be used for collecting long-running primitives, or nil if no semaphore should be used. The semaphore will be signalled once the heartbeat detects a long-running primitive until the result is collected via primitiveLongRunningPrimitive (see SystemDictionary>>longRunningPrimitive)


<details>
	<summary>See more</summary>
	
	longRunningPrimitiveSemaphore: aSemaphoreOrNil
	"Primitive. Install the semaphore to be used for collecting long-running primitives, 
	 or nil if no semaphore should be used.  The semaphore will be signalled once the
	 heartbeat detects a long-running primitive until the result is collected via
	 primitiveLongRunningPrimitive (see SystemDictionary>>longRunningPrimitive)"
	<primitive: 'primitiveLongRunningPrimitiveSemaphore'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#abandonSources

Smalltalk abandonSources


<details>
	<summary>See more</summary>
	
	abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	Preferences disable: #warnIfNoChangesFile.
	Preferences disable: #warnIfNoSourcesFile
</details>

#### SystemDictionary>>#isSpur

Answer true if we are a Spur ObjectMemory. Spur introduces a new format of header for objects, new format for classes, etc. Smalltalk isSpur


<details>
	<summary>See more</summary>
	
	isSpur
	"Answer true if we are a Spur ObjectMemory.
	Spur introduces a new format of header for objects, new format for classes, etc.
	Smalltalk isSpur
	"

	^ self compactClassesArray isNil
</details>

#### SystemDictionary>>#snapshotEmbeddedPrimitive

<details>
	<summary>See more</summary>
	
	snapshotEmbeddedPrimitive
	<primitive: 247>
	^nil "indicates error writing embedded image file"
</details>

#### SystemDictionary>>#browseUndeclaredReferences

Smalltalk browseUndeclaredReferences


<details>
	<summary>See more</summary>
	
	browseUndeclaredReferences
	"
	Smalltalk browseUndeclaredReferences
	"
	| anythingToShow |
	Smalltalk cleanOutUndeclared.
	anythingToShow _ false.
	Undeclared keys do: [ :k |
		anythingToShow _ true.
		self
			browseMessageList: (Smalltalk allCallsOn: (Undeclared associationAt: k))
			name: 'References to Undeclared: ', k printString ].

	"undeclared not in Undeclared - "
	"This happened in Pharo. See http://lists.gforge.inria.fr/pipermail/pharo-project/2012-March/061270.html
	Maybe do something like this in Cuis too???
	lostUndeclared _ Smalltalk allSelect: [:m|
		m literals anySatisfy: [:l|
			l isVariableBinding
				and: [l key isSymbol ""avoid class-side methodClass literals""
				and: [(m methodClass bindingOf: l key) isNil
				and: [(Undeclared includesAssociation: l) not]]]]].
	"

	anythingToShow ifFalse: [
		 (PopUpMenu labels: ' OK ')
				startUpWithCaption: 'There are no Undeclared at all' ]
</details>

#### SystemDictionary>>#primitiveGarbageCollect

Primitive. Reclaims all garbage and answers the number of bytes of available space.


<details>
	<summary>See more</summary>
	
	primitiveGarbageCollect
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."

	<primitive: 130>
	^ self primBytesLeft
</details>

#### SystemDictionary>>#bytesLeftString

Return a string describing the amount of memory available


<details>
	<summary>See more</summary>
	
	bytesLeftString
	"Return a string describing the amount of memory available"
	| availInternal availPhysical availTotal |
	self garbageCollect.
	availInternal _ self primBytesLeft.
	availPhysical _ self bytesLeft: false.
	availTotal _ self bytesLeft: true.
	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"
		ifFalse:[^availInternal printStringWithCommas, ' bytes available'].
	^String streamContents:[:s|
		s nextPutAll: availInternal printStringWithCommas, 	' bytes (internal) '; newLine.
		s nextPutAll: availPhysical printStringWithCommas,	' bytes (physical) '; newLine.
		s nextPutAll: availTotal printStringWithCommas, 	' bytes (total)     '].
</details>

#### SystemDictionary>>#voidCogVMState

Void any internal caches the VM maintains other than the method lookup caches. These comprise - the stack zone, where method activations are stored, and - the machine code zone, where the machine code form of CompiledMethods is held.


<details>
	<summary>See more</summary>
	
	voidCogVMState
	"Void any internal caches the VM maintains other than the method lookup caches.
		 These comprise
				- the stack zone, where method activations are stored, and
				- the machine code zone, where the machine code form of CompiledMethods is held."
	<primitive: 214>
	^self primitiveFailed

	"Time millisecondsToRun: [Smalltalk voidCogVMState]"
	"(1 to: 10) collect: [:ign| Time millisecondsToRun: [Smalltalk voidCogVMState]]"
</details>

#### SystemDictionary>>#snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag

WARNING: Current process will be killed. UI Process will be restarted


<details>
	<summary>See more</summary>
	
	snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: clearAllStateFlag
	"WARNING: Current process will be killed.
UI Process will be restarted
"
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."
	"
	To test the full cleanup and startup procedures, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: true

	To test the cleanup done when saving the image, evaluate:
		Smalltalk snapshot: false andQuit: false embedded: false clearAllClassState: false
	"
	| activeProc |
	activeProc _ Processor activeProcess.
	[ | isARealStartup guiRootObject guiRootObjectClass |
	save not & quit
		ifTrue: [
			(SourceFiles at: 2) ifNotNil: [ :changes |
				ChangesInitialFileSize ifNotNil: [ changes truncate: ChangesInitialFileSize ]]]
		ifFalse: [
			self
				logSnapshot: save
				andQuit: quit ].
	clearAllStateFlag ifTrue: [
		TranscriptWindow allInstancesDo: [ :each |
			each isInWorld ifTrue: [
				each delete.]].
		UISupervisor ui tearDownDesktop.
		Transcript logToFile: false ].
	ActiveModel flushEventSystem.
	self processShutDownList: quit.
	"Lo que sigue aca podria ir al shutdown de alguien... (se levantan en startup!)"
	Smalltalk stopLowSpaceWatcher.
	WeakArray stopFinalizationProcess.
	ProcessorScheduler stopBackgroundProcess.
	"Cosas que levanto explicitamente abajo"
	guiRootObjectClass _ UISupervisor ui class.
	guiRootObject _ UISupervisor ui.
	"Replace with this to create a new world at startup after 'saveAsNewVersion'"
	"guiRootObject _ clearAllStateFlag ifFalse: [ UISupervisor ui ]."
	UISupervisor stopUIProcess.
	activeProc isTerminated ifFalse: [ activeProc terminate ].
	"Clean Globals"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: nil.
	Smalltalk closeSourceFiles.
	Smalltalk
		at: #SourceFiles
		put: nil.
	Smalltalk allClassesDo: [ :cls |
		cls releaseClassCachedState ].
	clearAllStateFlag ifTrue: [
		Smalltalk allClassesDo: [ :cls |
			cls releaseClassState ]].
	"Ojo con los pool dicts. Creo que no hay ninguno..."
	"To keep cleaning stuff that shouldn't be saved..."
	clearAllStateFlag ifTrue: [ Smalltalk printStuffToCleanOnImageSave ].
	"Do image save & quit as apropriate"
	(Cursor cursorAt: #writeCursor) activateCursor.
	save
		ifTrue: [
			"The snapshot primitive answers false if it was just called to do the snapshot.
			But image startup is resumed by returning (again) from the primitive, but this time answering true."
			isARealStartup _ embeddedFlag
				ifTrue: [ self snapshotEmbeddedPrimitive ]
				ifFalse: [ self snapshotPrimitive ]]
		ifFalse: [ isARealStartup _ false ].
	quit & (isARealStartup == false) ifTrue: [ self quitPrimitive ].
	"If starting from absolute scratch, this would be a good time to recreate Global names"
	Smalltalk
		at: #Sensor
		put: nil.
	Smalltalk
		at: #Display
		put: DisplayScreen new.
	Smalltalk
		at: #SourceFiles
		put: (Array new: 2).
	Smalltalk openSourceFiles.
	"Here, startup begins! (isARealStartup might be nil)"
	Smalltalk allClassesDo: [ :cls |
		cls initClassCachedState ].
	self doStartUp: isARealStartup == true.
	UISupervisor spawnNewMorphicProcessFor: (guiRootObject ifNil: [ guiRootObject _ guiRootObjectClass newWorld ]).
	Display triggerEvent: #screenSizeChanged.
	self restoreLostChangesIfNecessary.
	clearAllStateFlag ifTrue: [
		UISupervisor whenUIinSafeState: [
			guiRootObject recreateDefaultDesktop ]].
	"If system is coming up (VM and image just started)"
	isARealStartup == true ifTrue: [
		UISupervisor whenUIinSafeState: [
			self processCommandLineArguments.
			AppLauncher launchApp ]].
	"Now it's time to raise an error"
	isARealStartup ifNil: [ self error: 'Failed to write image file (disk full?)' ]]
		forkAt: Processor timingPriority - 1
		named: 'Startup process'.
</details>

#### SystemDictionary>>#allSelect: aBlock

Answer a SortedCollection of each method that, when used as the block argument to aBlock, gives a true result.


<details>
	<summary>See more</summary>
	
	allSelect: aBlock
	"Answer a SortedCollection of each method that, when used as the block 
	argument to aBlock, gives a true result."
	| aCollection |
	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		class selectorsDo: [ :sel |
			(aBlock value: (class compiledMethodAt: sel)) ifTrue: [
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel)
			]]].
	^ aCollection
</details>

#### SystemDictionary>>#browseMethodsWithString: aString matchCase: caseSensitive

Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter


<details>
	<summary>See more</summary>
	
	browseMethodsWithString: aString matchCase: caseSensitive
	"Launch a browser on all methods that contain string literals with aString as a substring. Make the search case-sensitive or insensitive as dictated by the caseSensitive boolean parameter"

	self browseAllSelect:
			[:method |
				method  hasLiteralSuchThat: [:lit |
					lit class == String and:
					[lit includesSubstring: aString caseSensitive: caseSensitive]]]
		name:  'Methods with string ', aString printString, (caseSensitive ifTrue: [' (case-sensitive)'] ifFalse: [' (case-insensitive)'])
		autoSelect: aString.

</details>

#### SystemDictionary>>#browseInstVarDefs: aClass

Copied from browseInstVarRefs. Should be consolidated some day. 7/29/96 di 7/30/96 sw: did the consolidation


<details>
	<summary>See more</summary>
	
	browseInstVarDefs: aClass
	"Copied from browseInstVarRefs.  Should be consolidated some day. 7/29/96 di
	7/30/96 sw: did the consolidation"

	aClass chooseInstVarThenDo:	
		[:aVar | self browseAllStoresInto: aVar from: aClass]
</details>

#### SystemDictionary>>#cleanOutUndeclared

<details>
	<summary>See more</summary>
	
	cleanOutUndeclared 
	Undeclared removeUnreferencedKeys
</details>

#### SystemDictionary>>#testFormatter2

Smalltalk testFormatter2


<details>
	<summary>See more</summary>
	
	testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldCodeString _ (cls sourceCodeAt: selector) asString.
					newCodeString _ cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens _ oldCodeString findTokens: Character separators.
					newTokens _ newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.
</details>

#### SystemDictionary>>#allCallsOn: aLiteral

Smalltalk browseAllCallsOn: #open:label:.


<details>
	<summary>See more</summary>
	
	allCallsOn: aLiteral   "Smalltalk browseAllCallsOn: #open:label:."
	"Answer a Collection of all the methods that call on aLiteral."
	| aCollection special byte |

	#(23 48 'fred' (new open:label:)) size.
	"Example above should find #open:label:, though it is deeply embedded here."

	aCollection _ OrderedCollection new.
	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class | class addMethodsTo: aCollection thatReferenceTo: aLiteral special: special byte: byte ].
	
	^ aCollection
</details>

#### SystemDictionary>>#getCurrentWorkingDirectory

Do our best effort to answer the path from wich Cuis was started. Smalltalk getCurrentWorkingDirectory On Linux, starting as juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$ coglinux/bin/squeak Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image we get: Smalltalk vmPath '/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/' Smalltalk imagePath '/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master' Smalltalk primGetCurrentWorkingDirectoryUnix '/media/sf_SharedWithLinux/testPayload03/payload' Smalltalk getCurrentWorkingDirectory '/media/sf_SharedWithLinux/testPayload03/payload' On Windows 7, starting as C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload> cogwin\squeak.exe Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image we get: Smalltalk vmPath 'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\' Smalltalk imagePath 'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master' Smalltalk primGetCurrentWorkingDirectoryWindows 'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master' Smalltalk getCurrentWorkingDirectory 'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload' Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.


<details>
	<summary>See more</summary>
	
	getCurrentWorkingDirectory
	"Do our best effort to answer the path from wich Cuis was started.

	Smalltalk getCurrentWorkingDirectory


On Linux, starting as
	juan@juandebian:/media/sf_SharedWithLinux/testPayload03/payload$      coglinux/bin/squeak        Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath                       					'/media/sf_SharedWithLinux/testPayload03/payload/coglinux/lib/squeak/4.5-3370/'
	Smalltalk imagePath   							'/media/sf_SharedWithLinux/testPayload03/payload/Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryUnix    '/media/sf_SharedWithLinux/testPayload03/payload'
	Smalltalk getCurrentWorkingDirectory 			'/media/sf_SharedWithLinux/testPayload03/payload'


On Windows 7, starting as
	C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload>        cogwin\squeak.exe            Cuis-Smalltalk-Dev-master/Cuis4.2-2367.image
we get:
	Smalltalk vmPath    										'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\cogwin\'
	Smalltalk imagePath   									'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk primGetCurrentWorkingDirectoryWindows       	'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload\Cuis-Smalltalk-Dev-master'
	Smalltalk getCurrentWorkingDirectory  					'C:\Users\Juan-Tuerca\SharedWithLinux\testPayload03\payload'

	Easy on Unix. Harder on Windows, because #primGetCurrentWorkingDirectoryWindows insists on lying and answers the directory containing the image.
	"
	
	| fullImagePathName imageSpecCmdLineArgument |

	"Easy on Unix"
	 self primGetCurrentWorkingDirectoryUnix ifNotNil: [ :cwd |
		"It seems on Mac, when dropping image on VM, we get '/', that is not really meaningful"
		(cwd = '/' and: [ Smalltalk platformName = 'Mac OS' ]) ifFalse: [
			^ cwd ]].

	"On Windows, extract shell path if image is in subtree"
	fullImagePathName _ Smalltalk imageName.
	imageSpecCmdLineArgument _ Smalltalk getSystemAttribute: 1.
	imageSpecCmdLineArgument ifNotNil: [
		(imageSpecCmdLineArgument size < fullImagePathName size and: [			"Not if they are equal, as if they are both fully qualified (absolute) paths"
			fullImagePathName asPathTokens endsWith: imageSpecCmdLineArgument asPathTokens]) ifTrue: [
				^ fullImagePathName copyFrom: 1 to: fullImagePathName size - imageSpecCmdLineArgument size - 1 ]].

	"If we don't know better, answer nil. If appropriate, senders do:
		Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]
	to have, at least, some valid directory
	"
	^ nil
</details>

#### SystemDictionary>>#browseObsoleteMethodReferences

Open a browser on all referenced behaviors that are obsolete Smalltalk browseObsoleteMethodReferences Remember that if no methods reference obsoletes, but Smalltalk obsoleteBehaviors inspect still finds them, maybe they are referenced by ChangeSets!


<details>
	<summary>See more</summary>
	
	browseObsoleteMethodReferences
	"Open a browser on all referenced behaviors that are obsolete
		Smalltalk browseObsoleteMethodReferences
	Remember that if no methods reference obsoletes, but
		Smalltalk obsoleteBehaviors inspect
	still finds them, maybe they are referenced by ChangeSets!
	"
	| list |
	list _ self obsoleteMethodReferences.
	self browseMessageList: list name:'Method referencing obsoletes' autoSelect: nil
</details>

#### SystemDictionary>>#allObjects

Answer an Array of all objects in the system. Fail if there isn't enough memory to instantiate the result.


<details>
	<summary>See more</summary>
	
	allObjects
	"Answer an Array of all objects in the system.  Fail if
	 there isn't enough memory to instantiate the result."

	<primitive: 178>
	^self primitiveFailed
</details>

#### SystemDictionary>>#isDevelopmentEnvironmentPresent

Or we can't open a Smalltalk debugger


<details>
	<summary>See more</summary>
	
	isDevelopmentEnvironmentPresent
	"Or we can't open a Smalltalk debugger"

	^ Smalltalk isHeadless not and: [Smalltalk includesKey: #Debugger]
</details>

#### SystemDictionary>>#maxIdentityHash

Answer the maximum identityHash value supported by the VM.


<details>
	<summary>See more</summary>
	
	maxIdentityHash
	"Answer the maximum identityHash value supported by the VM."
	<primitive: 176>
	^self primitiveFailed
</details>

#### SystemDictionary>>#doMixedArithmetic

If true, then primitives can handle the conversions: SmallInteger arithmeticOp: Float (Small or Boxed) SmallInteger compareOp: Float (Small or Boxed) Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side. See doMixedArithmetic: Note: OpenSmalltalk VMs after March, 2019 can set the option and will honor it. The comparison operation behaves as if the Float was converted #asTrueFraction. This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk doMixedArithmetic: false'. Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior.


<details>
	<summary>See more</summary>
	
	doMixedArithmetic
	"If true, then primitives can handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.
	See doMixedArithmetic:
	
	Note:
		OpenSmalltalk VMs after March, 2019 can set the option and will honor it. The comparison operation behaves as if the Float was converted #asTrueFraction. This means that some rather big SmallIntegers in 64 bit systems, that can not be represented exactly as a Float will not be equal to any Float. Squeak adopted this critera. Cuis follows the more conventional, Smalltalk-80 tradition to always convert to Float if any operand is Float. Therefore Cuis needs to do 'Smalltalk doMixedArithmetic: false'.
		Previous VMs can not set the option, and will answer true when queried. But these VMs did the conversion to Float, and the requested operation in Floats. So, with these VMs, Cuis will also have the desired behavior."

	^ ((Smalltalk vmParameterAt: 48) allMask: 64) not
</details>

#### SystemDictionary>>#listBuiltinModule: index

Return the name of the n-th builtin module. This list is not sorted!


<details>
	<summary>See more</summary>
	
	listBuiltinModule: index
	"Return the name of the n-th builtin module.
	This list is not sorted!"
	<primitive: 572>
	^self primitiveFailed
</details>

#### SystemDictionary>>#installLowSpaceWatcher

Start a process to watch for low-space conditions.


<details>
	<summary>See more</summary>
	
	installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self stopLowSpaceWatcher.
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess resume.


</details>

#### SystemDictionary>>#removeFromShutDownList: aClass

<details>
	<summary>See more</summary>
	
	removeFromShutDownList: aClass

	ShutDownList remove: aClass name ifAbsent: nil
</details>

#### SystemDictionary>>#maxExternalSemaphores

The size of table where external semaphores are registered. Only in Cog. nil elsewhere


<details>
	<summary>See more</summary>
	
	maxExternalSemaphores
	"The size of table where external semaphores are registered. Only in Cog.
	nil elsewhere"

	^self vmParameterAt: 49
</details>

#### SystemDictionary>>#getVMParameters

Smalltalk getVMParameters


<details>
	<summary>See more</summary>
	
	getVMParameters	"Smalltalk getVMParameters"
	"Answer an Array containing the current values of the VM's internal
	parameter/metric registers.  Each value is stored in the array at the
	index corresponding to its VM register.  (See #vmParameterAt: and
	#vmParameterAt:put:.)"

	<primitive: 254>
	self primitiveFailed
</details>

#### SystemDictionary>>#flagInterpretedMethods: aBoolean

The Cog VM can be instructed to set the flag bit of CompiledMethods that it executes but will only interpret. This can be used e.g. to profile startup. See CompiledMethod>>#flag & CompiledMethod>>#clearFlag. This flag persists across snapshots, stored in the image header.


<details>
	<summary>See more</summary>
	
	flagInterpretedMethods: aBoolean
	"The Cog VM can be instructed to set the flag bit of CompiledMethods that
	 it executes but will only interpret.  This can be used e.g. to profile startup.
	 See CompiledMethod>>#flag & CompiledMethod>>#clearFlag.  
	 This flag persists across snapshots, stored in the image header."

	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 2) + (aBoolean ifTrue: [2] ifFalse: [0])
</details>

#### SystemDictionary>>#addToShutDownList: aClass

This will add a ref to this class at the BEGINNING of the shutDown list.


<details>
	<summary>See more</summary>
	
	addToShutDownList: aClass
	"This will add a ref to this class at the BEGINNING of the shutDown list."

	self addToShutDownList: aClass after: nil
</details>

#### SystemDictionary>>#addToShutDownList: aClass after: predecessor

<details>
	<summary>See more</summary>
	
	addToShutDownList: aClass after: predecessor

	self add: aClass toList: ShutDownList after: predecessor
</details>

#### SystemDictionary>>#nonClassNames

Answer a sorted collection of all non-class names. Use the return value of #fillCaches to avoid concurrency issues.


<details>
	<summary>See more</summary>
	
	nonClassNames
	"Answer a sorted collection of all non-class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedNonClassNames ifNil: [ self fillCaches at: 2 ]
</details>

#### SystemDictionary>>#saveAsNewVersion

Save the image/changes using the next available version number.


<details>
	<summary>See more</summary>
	
	saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true
</details>

#### SystemDictionary>>#compactClassesArrayIncludes: aClass

No compact classes in Spur, but usually some in non-Spur


<details>
	<summary>See more</summary>
	
	compactClassesArrayIncludes: aClass
	"No compact classes in Spur, but usually some in non-Spur"
	^self compactClassesArray
		ifNil: [ false ]
		ifNotNil: [ :it | it identityIncludes: aClass ]
</details>

#### SystemDictionary>>#doMixedArithmetic: aBoolean

If set to true, then primitives can handle the conversions: SmallInteger arithmeticOp: Float (Small or Boxed) SmallInteger compareOp: Float (Small or Boxed) Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side. Please see comment at #doMixedArithmetic


<details>
	<summary>See more</summary>
	
	doMixedArithmetic: aBoolean
	"If set to true, then primitives can handle the conversions:
	SmallInteger arithmeticOp: Float (Small or Boxed)
	SmallInteger compareOp: Float (Small or Boxed)
	Else, the primitive fail in case of mixed arithmetic, and conversion will be performed at image side.

	Please see  comment at #doMixedArithmetic"

	"Ignore request if VM doesn't support it"
	[
		self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 64) + (aBoolean ifTrue: [0] ifFalse: [64]).
	] on: Error do: [].
</details>

#### SystemDictionary>>#browseEqSmallConstant

Dan Ingalls' search for arithmetic use of == Answer whether the receiver contains the pattern <expression> == <constant>, where constant is -1, 0, 1, or 2... Smalltalk browseEqSmallConstant


<details>
	<summary>See more</summary>
	
	browseEqSmallConstant

    "Dan Ingalls' search for arithmetic use of ==
Answer whether the receiver contains the pattern <expression> == <constant>,
where constant is -1, 0, 1, or 2...
	Smalltalk browseEqSmallConstant
"

	Smalltalk browseAllSelect: [:m | m scanForEqSmallConstant]
</details>

#### SystemDictionary>>#browseAllImplementorsOf: selector

Create and schedule a message browser on each method that implements the message whose selector is the argument, selector. For example, Smalltalk browseAllImplementorsOf: #at:put:.


<details>
	<summary>See more</summary>
	
	browseAllImplementorsOf: selector
	"Create and schedule a message browser on each method that implements 
	the message whose selector is the argument, selector. For example, 
	Smalltalk browseAllImplementorsOf: #at:put:."

	^self browseMessageList: (self allImplementorsOf: selector) name: 'Implementors of ' , selector
</details>

#### SystemDictionary>>#isQuitRecord: chunk

<details>
	<summary>See more</summary>
	
	isQuitRecord: chunk

	^chunk beginsWith: self tagHeader, self quitTag 
</details>

#### SystemDictionary>>#browseMethodsWithEmptyClosures

Smalltalk browseMethodsWithEmptyClosures


<details>
	<summary>See more</summary>
	
	browseMethodsWithEmptyClosures
	"
	Smalltalk browseMethodsWithEmptyClosures
	"
	| emptyClosurePattern |
	emptyClosurePattern _ ByteArray readHexFrom: '8F000002737D'.
	self
		browseAllSelect: [ :m | | s |
			s _ InstructionStream on: m.
			s scanFor: [ :bytecode |
				bytecode = emptyClosurePattern first and: [
					(1 to: emptyClosurePattern size) allSatisfy: [ :i |
						(s method at: s pc + i - 1) = (emptyClosurePattern at: i) ]]]]
		name:  'Methods with empty closures'
		autoSelect: '[]'
</details>

#### SystemDictionary>>#isRunningCogit

Returns true if we're running on the Cog JIT (vmParameterAt: 46 is the size of the machine code zone) Smalltalk isRunningCogit


<details>
	<summary>See more</summary>
	
	isRunningCogit
	"Returns true if we're running on the Cog JIT
	 (vmParameterAt: 46 is the size of the machine code zone)
	Smalltalk isRunningCogit
	"

	^(self vmParameterAt: 46)
		ifNotNil: [ :machineCodeZoneSize  | machineCodeZoneSize > 0 ]
		ifNil: [ false ]
</details>

#### SystemDictionary>>#evaluated: expression context: aContext

<details>
	<summary>See more</summary>
	
	evaluated: expression context: aContext

	self logChange: expression
</details>

#### SystemDictionary>>#platformName

Return the name of the platform we're running on


<details>
	<summary>See more</summary>
	
	platformName
	"Return the name of the platform we're running on"
	^self getSystemAttribute: 1001
</details>

#### SystemDictionary>>#browseAllStoresInto: instVarName from: aClass

Create and schedule a Message Set browser for all the receiver's methods or any methods of a subclass/superclass that refer to the instance variable name.


<details>
	<summary>See more</summary>
	
	browseAllStoresInto: instVarName from: aClass
	"Create and schedule a Message Set browser for all the receiver's methods 
	or any methods of a subclass/superclass that refer to the instance variable name."
	
	"self new browseAllStoresInto: 'contents' from: Collection."

	| coll |
	coll _ OrderedCollection new.
	aClass withAllSubAndSuperclassesDo: [:class | 
		(class whichSelectorsStoreInto: instVarName) do: [:sel |
			coll add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: sel)]].
	^ self
		browseMessageList: coll 
		name: 'Stores into ' , instVarName 
		autoSelect: instVarName
</details>

#### SystemDictionary>>#alternativeSourcesName

Answer the alternative full path to the sources file. If Sources not found at #defaultSourcesName (image folder), seek for them here (vm folder).


<details>
	<summary>See more</summary>
	
	alternativeSourcesName
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (image folder), seek for them here (vm folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , SourceFileVersionString , '.sources'
</details>

#### SystemDictionary>>#specialSelectors

Used by SystemTracer only.


<details>
	<summary>See more</summary>
	
	specialSelectors
	"Used by SystemTracer only."

	^SpecialSelectors
</details>

#### SystemDictionary>>#browseMethodsWithClosuresThatOnlyReadOuterTemps

Smalltalk browseMethodsWithClosuresThatOnlyReadOuterTemps


<details>
	<summary>See more</summary>
	
	browseMethodsWithClosuresThatOnlyReadOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatOnlyReadOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount
					:hasIndirectTemps :anyClosureHasCopiedValues :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				anyClosureHasCopiedValues & hasIndirectTemps not].
			])
		name: 'Closures that read but not write to outer temps'
</details>

#### SystemDictionary>>#profilePrimitive

Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active. See also primitiveProfileStart.


<details>
	<summary>See more</summary>
	
	profilePrimitive
	"Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfilePrimitive'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#primVmFileName

Answer the absolute file name of the the Smalltalk virtual machine


<details>
	<summary>See more</summary>
	
	primVmFileName
	"Answer the absolute file name of the the Smalltalk virtual machine"

	^ self getSystemAttribute: 0
</details>

#### SystemDictionary>>#removeClassNamed: aName

Invoked from fileouts: if there is currently a class in the system named aName, then remove it. If anything untoward happens, report it in the Transcript.


<details>
	<summary>See more</summary>
	
	removeClassNamed: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	oldClass _ self at: aName asSymbol ifAbsent: [
		Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.
		^ self].

	oldClass removeFromSystem
</details>

#### SystemDictionary>>#interruptChecksPerMSec

Smalltalk interruptChecksPerMSec


<details>
	<summary>See more</summary>
	
	interruptChecksPerMSec
"
Smalltalk interruptChecksPerMSec
"
	"Primitive. Answer the number of interrupt checks per milliseconds that we execute
	on this machine. This can be used to adjust the sub-msecs profiler to check (roughly) 
	n times per millisecond."
	<primitive: 'primitiveInterruptChecksPerMSec'>
	^self primitiveFailed
</details>

#### SystemDictionary>>#reduceCuis

Smalltalk reduceCuis


<details>
	<summary>See more</summary>
	
	reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoMenuIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"
</details>

#### SystemDictionary>>#openSourcesAndChanges

Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups.


<details>
	<summary>See more</summary>
	
	openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ [ entry readStream ] on: FileDoesNotExistException do: [ nil ]]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		[ entry appendStream ] on: FileWriteError do: [ nil ] ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].
	ChangesInitialFileSize _ changes ifNotNil: [ changes position ].

	SourceFiles _ Array with: sources with: changes
</details>

#### SystemDictionary>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self == Smalltalk
		ifTrue: [ aStream nextPutAll: 'Smalltalk' ]
		ifFalse: [ super printOn: aStream ]
</details>

#### SystemDictionary>>#classNamed: className

className is either a class name or a class name followed by ' class'. Answer the class or metaclass it names. Answer nil if no class by that name.


<details>
	<summary>See more</summary>
	
	classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names.
	Answer nil if no class by that name."
	"
	Smalltalk classNamed: #Point
	Smalltalk classNamed: 'Point'
	Smalltalk classNamed: 'Point class'
	Smalltalk classNamed: 'BogusClassName'
	Smalltalk classNamed: 'BogusClassName class'

	Smalltalk classNamed: #Display
	Smalltalk classNamed: 'Display'
	Smalltalk classNamed: 'Display class'
	"

	Smalltalk at: className asSymbol ifPresent: [ :found |
		^ found isBehavior ifTrue: [ found ]].

	(className withoutSuffix: ' class') ifNotNil: [ :baseName |
		Smalltalk at: baseName asSymbol ifPresent: [ :found |
			^ found isBehavior ifTrue: [ found class ]]].

	^ nil
</details>

#### SystemDictionary>>#removeAllUnSentMessages

Smalltalk removeAllUnSentMessages


<details>
	<summary>See more</summary>
	
	removeAllUnSentMessages
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels _ self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels size = 0
		ifTrue: [^ 0].
	n _ 0.
	Smalltalk
		allBehaviorsDo: [:x | n _ n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:barBlock | 
			n _ 0.
			self
				allBehaviorsDo: [:class | 
					barBlock value: (n _ n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size
</details>

#### SystemDictionary>>#pointersTo: anObject except: objectsToExclude

Find all occurrences in the system of pointers to the argument anObject. Remove objects in the exclusion list from the results.


<details>
	<summary>See more</summary>
	
	pointersTo: anObject except: objectsToExclude 
	"Find all occurrences in the system of pointers to the argument
	anObject. Remove objects in the exclusion list from the
	results. "
	^ anObject inboundPointersExcluding: objectsToExclude
</details>

#### SystemDictionary>>#lastQuitLogPosition

<details>
	<summary>See more</summary>
	
	lastQuitLogPosition
	^ LastQuitLogPosition
</details>

#### SystemDictionary>>#reportClassAndMethodRemovalsFor: collectionOfClassNames

Smalltalk reportClassAndMethodRemovalsFor: #(Celeste Scamper MailMessage)


<details>
	<summary>See more</summary>
	
	reportClassAndMethodRemovalsFor: collectionOfClassNames
	| initialClassesAndMethods finalClassesAndMethods |
	"Smalltalk reportClassAndMethodRemovalsFor: #(Celeste Scamper MailMessage)"

	initialClassesAndMethods _ self unusedClassesAndMethodsWithout: #(#() #()).
	finalClassesAndMethods _ self unusedClassesAndMethodsWithout: {collectionOfClassNames. #()}.
	^ {finalClassesAndMethods first copyWithoutAll: initialClassesAndMethods first.
		finalClassesAndMethods second copyWithoutAll: initialClassesAndMethods second}
</details>

#### SystemDictionary>>#unbindExternalPrimitives

Primitive. Force all external primitives to be looked up again afterwards. Since external primitives that have not found are bound for fast failure this method will force the lookup of all primitives again so that after adding some plugin the primitives may be found.


<details>
	<summary>See more</summary>
	
	unbindExternalPrimitives
	"Primitive. Force all external primitives to be looked up again afterwards. Since external primitives that have not found are bound for fast failure this method will force the lookup of all primitives again so that after adding some plugin the primitives may be found."
	<primitive: 570>
	"Do nothing if the primitive fails for compatibility with older VMs"
</details>

#### SystemDictionary>>#browseViewReferencesFromNonViews

Smalltalk browseViewReferencesFromNonViews


<details>
	<summary>See more</summary>
	
	browseViewReferencesFromNonViews
	"
	Smalltalk browseViewReferencesFromNonViews
	"
	| aLiteral aCollection |

	aCollection _ OrderedCollection new.

	"Tweak to look just for pluggables or also for menus (or maybe for all morphs)"
"	PopUpMenu withAllSubclasses , MenuMorph withAllSubclasses , PluggableMorph withAllSubclasses do: [ :view |"
	PluggableMorph withAllSubclassesDo: [ :view |
"	MenuMorph withAllSubclassesDo: [ :view |"
		
		aLiteral _ view name.
		
		"tweak to linclude refs to SysWindow subhierarchy or not"
		(view includesBehavior: SystemWindow) & false ifFalse: [
			Smalltalk allBehaviorsDo: [ :class |
				((class includesBehavior: Morph) or: [ class includesBehavior: Morph class ]) ifFalse: [
					class addMethodsTo: aCollection thatReferenceTo: aLiteral special: false byte: nil ]]]].
	
	Smalltalk
		browseMessageList: aCollection asSet asArray sort
		name: 'References to Views from non-Views'
		autoSelect: ''.
</details>

#### SystemDictionary>>#baseLabel

<details>
	<summary>See more</summary>
	
	baseLabel
	^ 'system'
</details>

#### SystemDictionary>>#beep

Smalltalk beep


<details>
	<summary>See more</summary>
	
	beep
	"
	Smalltalk beep
	"
	Preferences soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]
</details>

#### SystemDictionary>>#logChange: aStringOrText

Write the argument, aString, onto the changes file.


<details>
	<summary>See more</summary>
	
	logChange: aStringOrText 
	"Write the argument, aString, onto the changes file."
	^ self logChange: aStringOrText preamble: nil
</details>

#### SystemDictionary>>#bytesLeft

Answer the number of bytes of space available. Does a full garbage collection.


<details>
	<summary>See more</summary>
	
	bytesLeft
	"Answer the number of bytes of space available. Does a full garbage collection."

	^ self garbageCollect

</details>

#### SystemDictionary>>#browseAllCallsOn: aLiteral localTo: aClass

Create and schedule a message browser on each method in or below the given class that refers to aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:.


<details>
	<summary>See more</summary>
	
	browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key , ' local to ' , aClass name
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral , ' local to ' , aClass name
				autoSelect: aLiteral ].
</details>

#### SystemDictionary>>#eliotsClosureMeasurements2On: aMethod

A Couple of Clean Closures Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01Argument Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp Closures reading and writing to outer temps Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTemp Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempWithAssignment Closure doing an method return Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01UpArrowReturn Closures sending messages to self & super Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SelfSend Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SuperSend A couple of non-closures, i.e. blocks that are optimized by the compiler and a closure is never created Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimized Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimizedWithAssignment A remote temp whose declaration can not be moved inside the block Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCantBeMovedInside Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempAssignedTwice A remote temp whose declaration can be moved inside the block Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCanBeMovedInside A not-so remote temp. The declaration was moved inside the block, making it a clean block Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp


<details>
	<summary>See more</summary>
	
	eliotsClosureMeasurements2On: aMethod
	"
	A Couple of Clean Closures
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01Argument
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	
	Closures reading and writing to outer temps
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTemp
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempWithAssignment

	Closure doing an method return
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01UpArrowReturn

	Closures sending messages to self & super
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SelfSend
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SuperSend

	A couple of non-closures, i.e. blocks that are optimized by the compiler and a closure is never created
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimized
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimizedWithAssignment
	
	A remote temp whose declaration can not be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCantBeMovedInside
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempAssignedTwice
	A remote temp whose declaration can be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCanBeMovedInside
	A not-so remote temp. The declaration was moved inside the block, making it a clean block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesNonLocalReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesNonLocalReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self eliotsClosureMeasurementsOn: aMethod over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
		numMethods := numMethods + 1.
		closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
		hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
		anyClosureDoesNonLocalReturn ifTrue: [ anyClosureDoesNonLocalReturnCount := anyClosureDoesNonLocalReturnCount + 1].
		anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
		(anyClosureDoesNonLocalReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
		closuresCount > 0 ifTrue: [
			(anyClosureDoesNonLocalReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
				onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
		anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithNonLocalReturnsInClosures'. anyClosureDoesNonLocalReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}
</details>

#### SystemDictionary>>#obsoleteBehaviors

Smalltalk obsoleteBehaviors inspect Find all obsolete behaviors including meta classes


<details>
	<summary>See more</summary>
	
	obsoleteBehaviors
	"
	Smalltalk obsoleteBehaviors inspect
	Find all obsolete behaviors including meta classes
	"
	| obs |
	obs _ OrderedCollection new.
	Smalltalk garbageCollect.
	self allObjectsDo: [ :cl |
		(cl isBehavior and: [cl isObsolete]) ifTrue: [obs add: cl]].
	^ obs asArray
</details>

#### SystemDictionary>>#listLoadedModules

Smalltalk listLoadedModules


<details>
	<summary>See more</summary>
	
	listLoadedModules
	"Smalltalk listLoadedModules"
	"Return a list of all currently loaded modules (e.g., plugins). Loaded modules are those that currently in use (e.g., active). The list returned will contain all currently active modules regardless of whether they're builtin (that is compiled with the VM) or external (e.g., residing in some external shared library). Note that the returned list is not sorted!"
	| modules index name |
	modules _ WriteStream on: Array new.
	index _ 1.
	[true] whileTrue:[
		name _ self listLoadedModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].
</details>

#### SystemDictionary>>#snapshotTag

<details>
	<summary>See more</summary>
	
	snapshotTag
		
	^'SNAPSHOT'
</details>

#### SystemDictionary>>#browseClassesWithNamesContaining: aString caseSensitive: caseSensitive

Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true


<details>
	<summary>See more</summary>
	
	browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix _ caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList _ OrderedCollection new.
	Smalltalk allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList size > 0
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet sorted title: 'Classes whose names contain ' , aString , suffix]
</details>

#### SystemDictionary>>#saveAs

Put up the 'saveAs' prompt, obtain a name, and save the image under that new name.


<details>
	<summary>See more</summary>
	
	saveAs
	"Put up the 'saveAs' prompt, obtain a name, and save the image  under that new name."

	| newName |
	newName _ self getFileNameFromUser.
	newName ifNil: [^ self].
 	self okayToSave ifFalse: [^self].
	self saveAs: newName andQuit: false clearAllClassState: false
</details>

#### SystemDictionary>>#okayToProceedEvenIfSpaceIsLow

Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning.


<details>
	<summary>See more</summary>
	
	okayToProceedEvenIfSpaceIsLow
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'

</details>

#### SystemDictionary>>#isBigEndian

<details>
	<summary>See more</summary>
	
	isBigEndian
	^self endianness == #big
</details>

#### SystemDictionary>>#cleanCompactObsoleteClasses

No Compact Classes support in Spur


<details>
	<summary>See more</summary>
	
	cleanCompactObsoleteClasses

	| cct |
	"No Compact Classes support in Spur"
	Smalltalk isSpur ifTrue: [ ^ self ].

	cct _ Smalltalk compactClassesArray.
	cct do: [ :c |
		c ifNotNil: [
			c isObsolete ifTrue: [ 
				cct at: c indexIfCompact put: nil ]]]
</details>

#### SystemDictionary>>#classes

<details>
	<summary>See more</summary>
	
	classes

	^self classNames collect: [:each | self at: each ]
</details>

#### SystemDictionary>>#restoreLostChangesAutomatically

<details>
	<summary>See more</summary>
	
	restoreLostChangesAutomatically
	
	self withChangesFileDo: [ :aChangesFile | self restoreLostChangesAutomaticallyFrom: aChangesFile ]
</details>

#### SystemDictionary>>#restoreLostChangesIfNecessary

Smalltalk restoreLostChangesIfNecessary


<details>
	<summary>See more</summary>
	
	restoreLostChangesIfNecessary

	"
	Smalltalk restoreLostChangesIfNecessary
	"
	
	self hasToRestoreChanges ifTrue: [ UISupervisor whenUIinSafeState: [self restoreLostChanges ]].

</details>

#### SystemDictionary>>#snapshot: save andQuit: quit embedded: embeddedFlag

<details>
	<summary>See more</summary>
	
	snapshot: save andQuit: quit embedded: embeddedFlag

	self snapshot: save andQuit: quit embedded: embeddedFlag clearAllClassState: false
</details>

#### SystemDictionary>>#restoreLostChangesManually

<details>
	<summary>See more</summary>
	
	restoreLostChangesManually

	ChangeList browseFrom: LastQuitLogPosition on: self currentChangesName asFileEntry labeled: 'Lost changes'

</details>

#### SystemDictionary>>#allGlobalRefs

Answer a set of symbols that may be refs to Global names. Warning: Will not include references to a class from its own methods


<details>
	<summary>See more</summary>
	
	allGlobalRefs
	"Answer a set of symbols that may be refs to Global names.
	Warning: Will not include references to a class from its own methods"

	^ self allGlobalRefsWithout: #(#() #())
</details>

#### SystemDictionary>>#endianness

Endianness is cached. If it happens to be nil, we are in the midst of either shutdown or startup. In such cases, compute endiannes but DO NOT cache it. I.e. do not do lazy initialization.


<details>
	<summary>See more</summary>
	
	endianness
	"Endianness is cached. If it happens to be nil, we are in the midst of either shutdown or startup. In such cases, compute endiannes but DO NOT cache it. I.e. do not do lazy initialization."
	^EndianCache ifNil: [ self calcEndianness ]
</details>

#### SystemDictionary>>#lastUpdateString

Smalltalk lastUpdateString


<details>
	<summary>See more</summary>
	
	lastUpdateString
	"Smalltalk lastUpdateString"
	^'latest update: #', SystemVersion current highestUpdate printString
</details>

#### SystemDictionary>>#numberOfSendersOf: aSymbol

Answer the count of all the methods that call on aLiteral. [ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun [ (Smalltalk numberOfSendersOf: #==) ] timeToRun


<details>
	<summary>See more</summary>
	
	numberOfSendersOf: aSymbol
	"Answer the count of all the methods that call on aLiteral.
	[ (Smalltalk numberOfSendersOf: #open:label:) ] timeToRun
	[ (Smalltalk numberOfSendersOf: #==) ] timeToRun
	"
	| count specialFlag specialByte |

	count _ 0.
	specialFlag _ self hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | specialByte _ b ].
	self allBehaviorsDo: [ :class |
		class selectorsAndMethodsDo: [ :sel :method |
			((method hasLiteral: aSymbol) or: [specialFlag and: [(method scanFor: specialByte) and: [ ((class sourceCodeAt: sel) findString: aSymbol) > 0 ]]])
				ifTrue: [ count _ count + 1 ]]].
	^ count
</details>

#### SystemDictionary>>#hasToRestoreChangesFrom: changesFile

<details>
	<summary>See more</summary>
	
	hasToRestoreChangesFrom: changesFile

	| chunk  |
	
	changesFile position: self lastQuitLogPosition.
	[ changesFile atEnd ] whileFalse: [ chunk := changesFile nextChunk ].
	
	^chunk notNil and: [(self isSnapshotQuitOrQuitNoSaveRecord: chunk) not]

</details>

#### SystemDictionary>>#hasBindingThatBeginsWith: aString

Answer true if the receiver has a key that begins with aString, false otherwise


<details>
	<summary>See more</summary>
	
	hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	"
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'Obj' ]] timeToRun 
		14
	Smalltalk hasBindingThatBeginsWith: 'Obj'
	[ 5000 timesRepeat: [Smalltalk hasBindingThatBeginsWith: 'zxzxObj' ]] timeToRun 
		47
	Smalltalk hasBindingThatBeginsWith: 'zxzxObj'
	"
	
	"Use the cached class and non-class names for better performance."
	| searchBlock |
	searchBlock _ [ :element |
		(element beginsWith: aString)
			ifTrue: [ 0 ]
			ifFalse: [
				aString < element
					ifTrue: [ -1 ]
					ifFalse: [ 1 ] ] ].

	self classNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].
	
	self nonClassNames 
		findBinary: searchBlock
		do: [ :found | ^true ]
		ifNone: [ :a :b | ].

	^false
</details>

#### SystemDictionary>>#allUnusedClassesWithout: classesAndMessagesPair

Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global.


<details>
	<summary>See more</summary>
	
	allUnusedClassesWithout: classesAndMessagesPair
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or: (d) some instance is a global."
	"
	Smalltalk unusedClasses
	"

	| unused cl |
	Smalltalk garbageCollect.
	unused _ Smalltalk classNames asIdentitySet
				copyWithoutAll: (self allGlobalRefsWithout: classesAndMessagesPair).
				
	Smalltalk do: [ :global |
		unused remove: global class name ifAbsent: nil].
	
	^ unused reject: [ :cName |
		cl _ Smalltalk at: cName.
		cl subclasses notEmpty
			"or: [ cl someInstance notNil ]"
			"or: [cl inheritsFrom: FileDirectory]"]
</details>

#### SystemDictionary>>#isThereAnImplementorOf: aSelector

Answer true if there is at least one implementor of the selector found in the system, false if there are no implementors


<details>
	<summary>See more</summary>
	
	isThereAnImplementorOf: aSelector  
	"Answer true if there is at least one implementor of the selector found in the system, false if there are no implementors"

	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"
</details>

#### SystemDictionary>>#testFormatter

Smalltalk testFormatter


<details>
	<summary>See more</summary>
	
	testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					newCodeString _ cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode _ cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldMethod _ cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.
</details>

#### SystemDictionary>>#specialObjectsArray

Smalltalk specialObjectsArray at: 1


<details>
	<summary>See more</summary>
	
	specialObjectsArray  "Smalltalk specialObjectsArray at: 1"
	<primitive: 129>
	^ self primitiveFailed
</details>

#### SystemDictionary>>#imageFormatVersionFromFile

Answer an integer identifying the type of image on file. The image version number may identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements of the image (e.g. block closure support required)


<details>
	<summary>See more</summary>
	
	imageFormatVersionFromFile
	"Answer an integer identifying the type of image on file. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required)"

	"
	Smalltalk imageFormatVersionFromFile
	"

	| format |
	format := self imageFormatVersionFromFileAsIs.
	^format <= 16r00FFFFFF
		ifTrue: [  "same endianness as VM"
			format ]
		ifFalse: [ "convert endianness"
			((format bitAnd: 16rFF000000) >> 24)
			+ ((format bitAnd: 16r00FF0000) >> 8)
			+ ((format bitAnd: 16r0000FF00) << 8)
			+ ((format bitAnd: 16r000000FF) << 16)]
</details>

#### SystemDictionary>>#browseMessageList: messageList ofSize: messageListSize name: labelString autoSelect: autoSelectString

Create and schedule a MessageSet browser on the message list.


<details>
	<summary>See more</summary>
	
	browseMessageList: messageList ofSize: messageListSize name: labelString autoSelect: autoSelectString

	| title |

	"Create and schedule a MessageSet browser on the message list."

	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	MessageSetWindow
		openMessageList: messageList
		label: title
		autoSelect: autoSelectString
</details>

#### SystemDictionary>>#browseAllImplementorsOfList: selectorList

Create and schedule a message browser on each method that implements the message whose selector is in the argument selectorList. For example, Smalltalk browseAllImplementorsOf: #(at:put: size). 1/16/96 sw: defer to the titled version


<details>
	<summary>See more</summary>
	
	browseAllImplementorsOfList: selectorList
	"Create and schedule a message browser on each method that implements 
	the message whose selector is in the argument selectorList. For example, 
	Smalltalk browseAllImplementorsOf: #(at:put: size).
	1/16/96 sw: defer to the titled version"

	self browseAllImplementorsOfList: selectorList title: 'Implementors of all'
</details>

#### SystemDictionary>>#processHasThreadIdInstVar: aBoolean

The threaded VM needs to know if the 4th inst var of Process is threadId which it uses to control process-to-thread binding. This flag persists across snapshots, stored in the image header.


<details>
	<summary>See more</summary>
	
	processHasThreadIdInstVar: aBoolean
	"The threaded VM needs to know if the 4th inst var of Process
	 is threadId which it uses to control process-to-thread binding.
	 This flag persists across snapshots, stored in the image header."
	aBoolean ifTrue: [self assert: (Process instVarNames at: 4) ='threadId'].
	self vmParameterAt: 48 put: ((self vmParameterAt: 48) bitClear: 1) + (aBoolean ifTrue: [1] ifFalse: [0])
</details>

#### SystemDictionary>>#primSignalAtBytesLeft: numBytes

Tell the interpreter the low-space threshold in bytes. When the free space falls below this threshold, the interpreter will signal the low-space semaphore, if one has been registered. Disable low-space interrupts if the argument is zero. Fail if numBytes is not an Integer.


<details>
	<summary>See more</summary>
	
	primSignalAtBytesLeft: numBytes
	"Tell the interpreter the low-space threshold in bytes. When the free
	space falls below this threshold, the interpreter will signal the low-space
	semaphore, if one has been registered.  Disable low-space interrupts if the
	argument is zero.  Fail if numBytes is not an Integer."

	<primitive: 125>
	self primitiveFailed
</details>

#### SystemDictionary>>#add: aClass toList: startUpOrShutDownList after: predecessor

Add the name of aClass to the startUp or shutDown list. Add it after the name of predecessor, or at the end if predecessor is nil.


<details>
	<summary>See more</summary>
	
	add: aClass toList: startUpOrShutDownList after: predecessor
	"Add the name of aClass to the startUp or shutDown list.
	Add it after the name of predecessor, or at the end if predecessor is nil."

	| name earlierName |
	name _ aClass name.
	(self at: name ifAbsent: nil) == aClass ifFalse: [
		self error: name , ' cannot be found in Smalltalk dictionary.'].
	predecessor
		ifNil: ["No-op if alredy in the list."
				(startUpOrShutDownList includes: name) ifFalse:
					[startUpOrShutDownList == StartUpList
						ifTrue: ["Add to end of startUp list"
								startUpOrShutDownList addLast: name]
						ifFalse: ["Add to front of shutDown list"
								startUpOrShutDownList addFirst: name]]]
		ifNotNil: ["Add after predecessor, moving it if already there."
				earlierName _ predecessor name.
				(self at: earlierName) == predecessor ifFalse: [
					self error: earlierName , ' cannot be found in Smalltalk dictionary.'].
				(startUpOrShutDownList includes: earlierName) ifFalse: [
					self error: earlierName , ' cannot be found in the list.'].
				startUpOrShutDownList remove: name ifAbsent: nil.
				startUpOrShutDownList add: name after: earlierName]
</details>

#### SystemDictionary>>#fullNameForChangesNamed: aName

Smalltalk fullNameForChangesNamed: 'newChanges'


<details>
	<summary>See more</summary>
	
	fullNameForChangesNamed: aName
	"
	Smalltalk fullNameForChangesNamed: 'newChanges'
	"
	| newName |
	newName _ FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.changes'
</details>

#### SystemDictionary>>#processShutDownList: quitting

Send #shutDown to each class that needs to wrap up before a snapshot.


<details>
	<summary>See more</summary>
	
	processShutDownList: quitting
	"Send #shutDown to each class that needs to wrap up before a snapshot."

	self send: #shutDown: toClassesNamedIn: ShutDownList with: quitting.
	EndianCache _ nil
</details>

#### SystemDictionary>>#allCallsOn: firstLiteral and: secondLiteral

Answer a SortedCollection of all the methods that call on both aLiteral and secondLiteral.


<details>
	<summary>See more</summary>
	
	allCallsOn: firstLiteral and: secondLiteral
	"Answer a SortedCollection of all the methods that call on both aLiteral 
	and secondLiteral."

	| aCollection secondArray firstSpecial secondSpecial firstByte secondByte |
	aCollection _ SortedCollection new.
	firstSpecial _ self hasSpecialSelector: firstLiteral ifTrueSetByte: [:b | firstByte _ b].
	secondSpecial _ self hasSpecialSelector: secondLiteral ifTrueSetByte: [:b | secondByte _ b].
	self allBehaviorsDo: [ :class |
		secondArray _ class 
			whichSelectorsReferTo: secondLiteral
			special: secondSpecial
			byte: secondByte.
		((class whichSelectorsReferTo: firstLiteral special: firstSpecial byte: firstByte) select: [ :aSel |
			(secondArray includes: aSel)]) do: [ :sel | 
				aCollection add: (
					MethodReference new
						setStandardClass: class 
						methodSymbol: sel )]].
	^aCollection
</details>

#### SystemDictionary>>#snapshotMessageFor: save andQuit: quit

<details>
	<summary>See more</summary>
	
	snapshotMessageFor: save andQuit: quit

	| dateAndTime |
	dateAndTime _ DateAndTime now.
	^String streamContents: [ :stream |
		stream 
			nextPutAll: self tagHeader;
			nextPutAll: (self snapshotTagFor: save andQuit: quit);
			nextPutAll: self tagTail.
		stream nextPut: $(.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream nextPut: $).
		stream
			space;
			nextPutAll: self imageName asFileEntry name;
			nextPutAll: ' priorSource: ';
			print: LastQuitLogPosition ].
</details>

#### SystemDictionary>>#forceChangesToDisk

Just flush the buffer and trust the OS to do its job.


<details>
	<summary>See more</summary>
	
	forceChangesToDisk
	"Just flush the buffer and trust the OS to do its job."
	| changesFile |
	"Expensive and not worth doing, esp. in Windows with antivirus active, when installing large packages"
	ChangeSet notInstallOrTestRun ifTrue: [
		changesFile _ SourceFiles at: 2.
		changesFile isFileStream ifTrue: [ changesFile flush ]].
</details>

#### SystemDictionary>>#bytesLeft: aBool

Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently availabe within Squeak memory but not counting extra memory use #primBytesLeft.


<details>
	<summary>See more</summary>
	
	bytesLeft: aBool
	"Return the amount of available space. If aBool is true, include possibly available swap space. If aBool is false, include possibly available physical memory. For a report on the largest free block currently availabe within Squeak memory but not counting extra memory use #primBytesLeft."
	<primitive: 112>
	^self primBytesLeft
</details>

#### SystemDictionary>>#allBehaviorsDo: aBlock

Evaluate the argument, aBlock, for each kind of Behavior in the system (that is, Object and its subclasses). ar 7/15/1999: The code below will not enumerate any obsolete or anonymous behaviors for which the following should be executed: Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]]. but what follows is way faster than enumerating all objects.


<details>
	<summary>See more</summary>
	
	allBehaviorsDo: aBlock 
	"Evaluate the argument, aBlock, for each kind of Behavior in the system 
	(that is, Object and its subclasses).
	ar 7/15/1999: The code below will not enumerate any obsolete or anonymous
	behaviors for which the following should be executed:

		Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]].

	but what follows is way faster than enumerating all objects."

	Smalltalk do: [ :root |
		(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
			root withAllSubclassesDo: [ :class |
				class isMeta ifFalse: [ "The metaclasses are rooted at Class; don't include them twice."
					aBlock
						value: class;
						value: class class ]]]]
</details>

#### SystemDictionary>>#allImplementorsOf: aSelector

Answer a SortedCollection of all the methods that implement the message aSelector.


<details>
	<summary>See more</summary>
	
	allImplementorsOf: aSelector  
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| aCollection |

	aCollection _ SortedCollection new.
	self allBehaviorsDo: [ :class |
		(class includesSelector: aSelector) ifTrue: [
			aCollection add: (
				MethodReference new
					setStandardClass: class 
					methodSymbol: aSelector )]].
	^aCollection
</details>

#### SystemDictionary>>#unknownContributors

Answer a collection of authorInitials for whom there is code in the system (either in core or in loaded packages), but we don't knwo their full name. Smalltalk unknownContributors


<details>
	<summary>See more</summary>
	
	unknownContributors
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all _ Smalltalk allContributors asSet.
	ok _ (Smalltalk knownInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort
</details>

#### SystemDictionary>>#renameAndUpdateReferences: oldName as: newName

Dangerous. Simple search and replace could break stuff. Use only to generate change sets to later checked with care OldMorph withAllSubclasses do: [ :c | n _ c name. (n beginsWith: 'Old') ifTrue: [ Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]] OldBorderStyle withAllSubclasses do: [ :c | n _ c name. (n beginsWith: 'Old') ifTrue: [ Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]] #(#OldKeyboardBuffer #OldLayoutCell #OldLayoutFrame #OldLayoutPolicy #OldLayoutProperties #OldMorphExtension #OldMouseClickState #OldProportionalLayout #OldTableLayout #OldTableLayoutProperties #OldTextAnchor #OldTextComposer #OldTextContainer #OldTextLine #OldTheWorldMenu #OldWorldState #OldWorldTest) do: [ :n | Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]


<details>
	<summary>See more</summary>
	
	renameAndUpdateReferences: oldName as: newName
	"Dangerous. Simple search and replace could break stuff.
	Use only to generate change sets to later checked with care
	
OldMorph withAllSubclasses do: [ :c |
	n _ c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

OldBorderStyle withAllSubclasses do: [ :c |
	n _ c name.
	(n beginsWith: 'Old') ifTrue: [
		Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]]

#(#OldKeyboardBuffer #OldLayoutCell #OldLayoutFrame #OldLayoutPolicy #OldLayoutProperties #OldMorphExtension #OldMouseClickState #OldProportionalLayout #OldTableLayout #OldTableLayoutProperties #OldTextAnchor #OldTextComposer #OldTextContainer #OldTextLine #OldTheWorldMenu #OldWorldState #OldWorldTest) do: [ :n |
	Smalltalk renameAndUpdateReferences: n as: (n copyFrom: 4 to: n size) asSymbol ]
	"

	self renameClassNamed: oldName as: newName.
	(Smalltalk allCallsOn: (Smalltalk associationAt: newName)) do: [ :methodRef |
		methodRef updateReferencesTo: oldName toBe: newName ]
</details>

#### SystemDictionary>>#externalObjects

Return an array of objects that have been registered for use in non-Smalltalk code. Smalltalk objects should be referrenced by external code only via indirection through this array, thus allowing the objects to move during compaction. This array can be cleared when the VM re-starts, since variables in external code do not survive snapshots. Note that external code should not attempt to access a Smalltalk object, even via this mechanism, while garbage collection is in progress.


<details>
	<summary>See more</summary>
	
	externalObjects
	"Return an array of objects that have been registered for use in non-Smalltalk code. Smalltalk objects should be referrenced by external code only via indirection through this array, thus allowing the objects to move during compaction. This array can be cleared when the VM re-starts, since variables in external code do not survive snapshots. Note that external code should not attempt to access a Smalltalk object, even via this mechanism, while garbage collection is in progress."
	"Smalltalk externalObjects"

	^ ExternalSemaphoreTable externalObjects

</details>

#### SystemDictionary>>#browseMessageList: messageList name: labelString autoSelect: autoSelectString

<details>
	<summary>See more</summary>
	
	browseMessageList: messageList name: labelString autoSelect: autoSelectString
	
	^self browseMessageList: messageList ofSize: messageList size name: labelString autoSelect: autoSelectString
</details>

#### SystemDictionary>>#browseAllPrimitives

<details>
	<summary>See more</summary>
	
	browseAllPrimitives
	self browseAllSelect: [:each| each primitive ~= 0 and: [(each primitive between: 256 and: 291) not]]

</details>

#### SystemDictionary>>#vmParameterAt: parameterIndex put: newValue

parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers. Store newValue (a positive integer) into that register and answer with the previous value that was stored there. Fail if newValue is out of range, if parameterIndex has no corresponding register, or if the corresponding register is read-only. As of mid 2017 the parameters which can be set are 5 allocations between GCs (read-write; nil in Cog VMs) 6 survivor count tenuring threshold (read-write) 17 proportion of code zone available for use (Sista VMs only) 23 bytes of extra memory to reserve for VM buffers, plugins, etc. 24 memory threshold above whichto shrink object memory (read-write) 25 memory headroom when growing object memory (read-write) 26 interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write) 34 bytes allocated in total since start-up or reset (read-write) 43 desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil) 45 desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil) 47 desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only) 48 various properties of the Cog VM as an integer encoding an array of bit flags. Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList) Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods Bit 2: if set, preempting a process puts it to the head of its run queue, not the back, i.e. preempting a process by a higher priority one will not cause the preempted process to yield to others at the same priority. Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set) (on VMs that don't support it, those primitives will fail in those cases) 49 the size of the external semaphore table (read-write; Cog VMs only) 55 ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil) 67 the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur only).


<details>
	<summary>See more</summary>
	
	vmParameterAt: parameterIndex put: newValue
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Store newValue (a positive integer) into that
	register and answer with the previous value that was stored there.
	Fail if newValue is out of range, if parameterIndex has no corresponding
	register, or if the corresponding register is read-only.

	As of mid 2017 the parameters which can be set are
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		17	proportion of code zone available for use (Sista VMs only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		34	bytes allocated in total since start-up or reset (read-write)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	the size of the external semaphore table (read-write; Cog VMs only)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil)
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit (Spur only)."

	<primitive: 254>
	self primitiveFailed
</details>

#### SystemDictionary>>#unregisterExternalObject: anObject

Unregister the given object in the external objects array. Do nothing if it isn't registered.


<details>
	<summary>See more</summary>
	
	unregisterExternalObject: anObject
	"Unregister the given object in the external objects array. Do nothing if it isn't registered."

	ExternalSemaphoreTable unregisterExternalObject: anObject
</details>

#### SystemDictionary>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a reference to Smalltalk instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a reference to Smalltalk instead."

	dp _ DiskProxy global: #Smalltalk selector: #yourself
			args: #().
	refStrm replace: self with: dp.
	^ dp
</details>

#### SystemDictionary>>#browseMethodsWithString: aString

Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive.


<details>
	<summary>See more</summary>
	
	browseMethodsWithString: aString
	"Launch a browser on all methods that contain string literals with aString as a substring. The search is case-insensitive, unless the shift key is pressed, in which case the search is case-sensitive."

	^ self browseMethodsWithString: aString matchCase: false

	"Smalltalk browseMethodsWithString: 'Testing' matchCase: false"
	"Smalltalk browseMethodsWithString: 'Testing' matchCase: true"
</details>

#### SystemDictionary>>#systemInformationString

Identify software version


<details>
	<summary>See more</summary>
	
	systemInformationString
	"Identify software version"
	^ SystemVersion current version, String newLineString, self lastUpdateString
</details>

#### SystemDictionary>>#signalLowSpace

Signal the low-space semaphore to alert the user that space is running low.


<details>
	<summary>See more</summary>
	
	signalLowSpace
	"Signal the low-space semaphore to alert the user that space is running low."

	LowSpaceSemaphore signal.
</details>

#### SystemDictionary>>#defaultChangesName

Answer the default full path to the changes file corresponding to the image file name.


<details>
	<summary>See more</summary>
	
	defaultChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), '.changes'
</details>

#### SystemDictionary>>#allImplementedMessagesWithout: classesAndMessagesPair

Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages. Note this reports messages that are in the absent selectors set.


<details>
	<summary>See more</summary>
	
	allImplementedMessagesWithout: classesAndMessagesPair
	"Answer a Set of all the messages that are implemented in the system, computed in the absence of the supplied classes and messages.  Note this reports messages that are in the absent selectors set."
	| messages absentClasses |
	messages _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
			ifTrue: [#()]
			ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
				messages addAll: cl selectors]].
	^ messages
</details>

#### SystemDictionary>>#addToStartUpList: aClass after: predecessor

<details>
	<summary>See more</summary>
	
	addToStartUpList: aClass after: predecessor

	self add: aClass toList: StartUpList after: predecessor
</details>

#### SystemDictionary>>#isHeadless

Answer true if any of this VM options was specified in the commandline: -nodisplay -vm-display-null Smalltalk isHeadless


<details>
	<summary>See more</summary>
	
	isHeadless
	"Answer true if any of this VM options was specified in the commandline:
		-nodisplay
		-vm-display-null

	Smalltalk isHeadless
	"
	self vmOptionsDo: [ :vmOption :i |
		vmOption = '-vm-display-null' ifTrue: [ ^ true ].
		vmOption = '-nodisplay' ifTrue: [ ^ true ] ].
	^ false
</details>

#### SystemDictionary>>#eliotsClosureMeasurements

Smalltalk eliotsClosureMeasurements From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/ by Eliot Miranda


<details>
	<summary>See more</summary>
	
	eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         nonLocalReturnsInClosure closureUsesSelfs nonLocalReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        nonLocalReturnsInClosure := closureUsesSelfs := nonLocalReturnAndUsesSelfs := numClean := 0.
        self allSelect: [ :m |
                 | s hasClosure hasIndirectTemps blkPc blkSz doesNonLocalReturn usesSelf hasCopied sel |
                 sel _ false.
                 hasClosure := hasIndirectTemps := false.
                 s := InstructionStream on: m.
                 s scanFor:
                          [:b|
                          b = 143 "closure creation" ifTrue:
                                   [hasClosure := true.
                                   numClosures := numClosures + 1.
                                   s followingByte >= 16 ifTrue:
                                            [numClosuresWithCopiedValues := numClosuresWithCopiedValues + 1.
                                             numCopiedValuesForClosure := numCopiedValuesForClosure + (s followingByte >> 4)]].
                          (b = 138 "indirect temp vector creation"
                           and: [s followingByte <= 127]) ifTrue:
                                   [hasIndirectTemps := true.
                                    numScopesWithRemoteTemps := numScopesWithRemoteTemps + 1.
                                    numRemoteTemps := numRemoteTemps + s followingByte].
                          false].
                 numMethods := numMethods + 1.
                 hasClosure ifTrue:
                          [numMethodsWithClosure := numMethodsWithClosure + 1.
                           s pc: m initialPC; scanFor: [:b| b = 143].

"jmv-This looks like the correct place to do this"
                           hasCopied := s followingByte >= 16.

                           blkSz := s interpretNextInstructionFor: BlockStartLocator new.
                           blkPc := s pc.
                           doesNonLocalReturn := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesNonLocalReturn := doesNonLocalReturn or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesNonLocalReturn ifTrue:
                                   [nonLocalReturnsInClosure := nonLocalReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesNonLocalReturn and: [usesSelf]) ifTrue:
                                   [nonLocalReturnAndUsesSelfs := nonLocalReturnAndUsesSelfs + 1].
                           (doesNonLocalReturn or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithNonLocalReturnsInClosures'. nonLocalReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. nonLocalReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }
</details>

#### SystemDictionary>>#inspectGlobals

Smalltalk inspectGlobals


<details>
	<summary>See more</summary>
	
	inspectGlobals
	"
	Smalltalk  inspectGlobals
	"

	self globals inspectWithLabel: 'The Globals'
</details>

#### SystemDictionary>>#forgetClass: aClass logged: aBool

Delete the class, aClass, from the system. Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem.


<details>
	<summary>See more</summary>
	
	forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	| classCategory |
	
	"I have to keep the cateogory becuase it is nil after removing the class -Hernan"
	classCategory _ aClass category.
	
	SystemOrganization removeElement: aClass name.
	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: classCategory].		
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache
</details>

#### SystemDictionary>>#methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

<details>
	<summary>See more</summary>
	
	methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [ self 
			logChange: aMethod sourceCode 
			preamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField ]. 

</details>

#### SystemDictionary>>#quitPrimitive

Primitive. Exit to another operating system on the host machine, if one exists. All state changes in the object space since the last snapshot are lost. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	quitPrimitive
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed
</details>

#### SystemDictionary>>#vmFileName

Answer the absolute file name of the the Smalltalk virtual machine


<details>
	<summary>See more</summary>
	
	vmFileName
	"Answer the absolute file name of the the Smalltalk virtual machine"
	"
	Smalltalk vmFileName
	"

	^ self primVmFileName
</details>

#### SystemDictionary>>#condenseChanges

Move all the changes onto a compacted sources file.


<details>
	<summary>See more</summary>
	
	condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :barBlock |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					barBlock value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFileEntry appendStream.

	self inform: 'Changes file has been rewritten!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '
</details>

#### SystemDictionary>>#fullNameForImageNamed: aName

Smalltalk fullNameForImageNamed: 'newImage'


<details>
	<summary>See more</summary>
	
	fullNameForImageNamed: aName
	"
	Smalltalk fullNameForImageNamed: 'newImage'
	"
	| newName |
	newName _ FileIOAccessor default baseNameFor: ((DirectoryEntry smalltalkImageDirectory // aName) pathName).
	^newName , '.image'
</details>

#### SystemDictionary>>#isSnapshotQuitOrQuitNoSaveRecord: chunk

<details>
	<summary>See more</summary>
	
	isSnapshotQuitOrQuitNoSaveRecord: chunk

	^(self isSnapshotRecord: chunk) 
		or: [ (self isQuitRecord: chunk) 
		or: [ self isQuitNoSaveRecord: chunk ]]
</details>

#### SystemDictionary>>#primitiveBeep

Make a primitive beep. Not to be called directly.


<details>
	<summary>See more</summary>
	
	primitiveBeep
	"Make a primitive beep. Not to be called directly."
	"
	Smalltalk primitiveBeep
	"

	<primitive: 140>
	self primitiveFailed
</details>

#### SystemDictionary>>#unusedClasses

Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or (d) some instance is a global.


<details>
	<summary>See more</summary>
	
	unusedClasses
	"Enumerates all classes in the system and returns a list of those that are apparently unused. A class is considered in use if it (a) has subclasses or (b) is referred to by some method or (c) has its name in use as a literal (but not in the same class) or (d) some instance is a global."
	"Smalltalk unusedClasses asSortedCollection"

	^ self allUnusedClassesWithout: #(#() #())
</details>

#### SystemDictionary>>#garbageCollect

Primitive. Reclaims all garbage and answers the number of bytes of available space.


<details>
	<summary>See more</summary>
	
	garbageCollect
	"Primitive. Reclaims all garbage and answers the number of bytes of available space."
	ActiveModel flushEventSystem.
	^self primitiveGarbageCollect
</details>

#### SystemDictionary>>#systemCategoryFromUserWithPrompt: aString

Prompt the user to select an existing system category (i.e. the ones that appear in the top left pane in the Browser window)


<details>
	<summary>See more</summary>
	
	systemCategoryFromUserWithPrompt: aString
	"Prompt the user to select an existing system category (i.e. the ones that appear in the top left pane in the Browser window)"
	| allCats menuIndex |
	allCats := self organization categories sorted.
	menuIndex := (PopUpMenu labelArray: allCats) startUpWithCaption: aString.
	^ menuIndex = 0 ifTrue: [nil] ifFalse: [allCats at: menuIndex]
</details>

#### SystemDictionary>>#classNames

Answer a sorted collection of all class names. Use the return value of #fillCaches to avoid concurrency issues.


<details>
	<summary>See more</summary>
	
	classNames
	"Answer a sorted collection of all class names. Use the return value of #fillCaches to avoid concurrency issues."
	
	^cachedClassNames ifNil: [ self fillCaches at: 1 ]
</details>

#### SystemDictionary>>#macroBenchmark1

Smalltalk macroBenchmark1


<details>
	<summary>See more</summary>
	
	macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes _ Smalltalk allClasses select: [:c | c name < 'B3'].
	badOnes _ OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes sum: [:c | c selectors size] ifEmpty: [0])
during: [:barBlock | n _ 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | barBlock value: (n _ n+1).
			oldMethod _ cls compiledMethodAt: selector.
			oldCodeString _ (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size
</details>

#### SystemDictionary>>#aboutThisSystem

Identify software version


<details>
	<summary>See more</summary>
	
	aboutThisSystem 
	"Identify software version"

	^ self inform: self systemInformationString
</details>

#### SystemDictionary>>#internalizeChangeLog

Smalltalk internalizeChangeLog


<details>
	<summary>See more</summary>
	
	internalizeChangeLog    
		"
		Smalltalk internalizeChangeLog
		"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aFile |
	reply _ self confirm:  'CAUTION -- do not undertake this lightly!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile _ SourceFiles second) ifNil: [
		^ self halt: 'Cannot locate Changes file so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'
</details>

#### SystemDictionary>>#pointersTo: anObject

Find all occurrences in the system of pointers to the argument anObject.


<details>
	<summary>See more</summary>
	
	pointersTo: anObject
	"Find all occurrences in the system of pointers to the argument anObject."
	"(Smalltalk pointersTo: Browser) inspect."

	^ self pointersTo: anObject except: #()

</details>

#### SystemDictionary>>#processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs

Smalltalk processCommandLineArguments A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment): Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''


<details>
	<summary>See more</summary>
	
	processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data entry |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: UnhandledError do: [:ex | ex return] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: UnhandledError do: [:ex | ex return] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: UnhandledError do: [:ex | ex return]]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]
</details>

#### SystemDictionary>>#browseAllCallsOnClass: aClass

Create and schedule a message browser on each method that refers to aClass. For example, Smalltalk browseAllCallsOnClass: Object.


<details>
	<summary>See more</summary>
	
	browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, Smalltalk browseAllCallsOnClass: Object."
	self
		browseMessageList: aClass allCallsOn asArray sort
		name: 'Users of class ' , aClass theNonMetaClass name
		autoSelect: aClass theNonMetaClass name.
</details>

#### SystemDictionary>>#allImplementedMessages

Answer a Set of all the messages that are implemented in the system.


<details>
	<summary>See more</summary>
	
	allImplementedMessages
	"Answer a Set of all the messages that are implemented in the system."

	^ self allImplementedMessagesWithout:  #(#() #())
</details>

#### SystemDictionary>>#fillCaches

Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values.


<details>
	<summary>See more</summary>
	
	fillCaches
	"Fill cachedClassNames and cachedNonClassNames. Return an array with the calculated values."

	| classNames nonClassNames |
	classNames _ IdentitySet new: self size.
	nonClassNames _ IdentitySet new.
	self keysAndValuesDo: [ :key :value |
		value isInMemory ifTrue: [
			"The key == value name test below addresses two separate issues:
				1) Obsolete classes, where key = #Foo and value name = 'AnObsoleteFoo'
				2) Aliases, i.e., Smalltalk at: #OtherName put: aClass"
			(value class isMeta and: [ key == value name ])
				ifTrue: [ classNames add: key ]
				ifFalse: [ nonClassNames add: key ] ] ].
	classNames _ classNames asArray sort.
	nonClassNames _ nonClassNames asArray sort.
	cachedClassNames _ classNames.
	cachedNonClassNames _ nonClassNames.
	^{ classNames. nonClassNames }
</details>

#### SystemDictionary>>#isQuitNoSaveRecord: chunk

<details>
	<summary>See more</summary>
	
	isQuitNoSaveRecord: chunk

	^chunk beginsWith: self tagHeader, self quitNoSaveTag
	
</details>

#### SystemDictionary>>#poolUsers

Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information


<details>
	<summary>See more</summary>
	
	poolUsers
	"Answer a dictionary of pool name -> classes that refer to it. Also includes any globally know dictionaries (such as Smalltalk, Undeclared etc) which although not strictly accurate is potentially useful information "
	"Smalltalk poolUsers"
	| poolUsers pool refs |
	poolUsers _ Dictionary new.
	Smalltalk keys
		do: [ :k |
			 (((pool _ Smalltalk at: k) is: #Dictionary)
					or: [pool isKindOf: SharedPool class])
				ifTrue: [refs _ Smalltalk allClasses
								select: [:c | c sharedPools identityIncludes: pool]
								thenCollect: [:c | c name].
					refs _ refs asOrderedCollection.
					refs
						add: (Smalltalk
								allCallsOn: (Smalltalk associationAt: k)).
					poolUsers at: k put: refs]].
	^ poolUsers
</details>

#### SystemDictionary>>#isRunningCog

Returns true if we're running on a Cog VM (JIT or StackInterpreter) (vmParameterAt: 42 is the number of stack pages) Smalltalk isRunningCog


<details>
	<summary>See more</summary>
	
	isRunningCog
	"Returns true if we're running on a Cog VM (JIT or StackInterpreter)
	 (vmParameterAt: 42 is the number of stack pages)
	Smalltalk isRunningCog
	"

	^(self vmParameterAt: 42)
		ifNotNil: [ :stackPages | stackPages > 0 ]
		ifNil: [ false ]
</details>

#### SystemDictionary>>#macroBenchmark3

Smalltalk macroBenchmark3


<details>
	<summary>See more</summary>
	
	macroBenchmark3   "Smalltalk macroBenchmark3"
	| testBlock tallies prev receiver |
	"Runs the stepping simulator with the messageTally tree (like tallySends)."
	testBlock _
		['Running the context step simulator'
			displayProgressAt: Sensor mousePoint
			from: 0 to: 200
			during:
				[:barBlock |
				1 to: 200 do:
				[:x | barBlock value: x.
				Float pi printString.
				15 factorial printString]]].
	tallies _ MessageTally new class: testBlock receiver class
							method: testBlock method.
	receiver _ nil.
	prev _ testBlock.
	thisContext sender
		runSimulated: testBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [
				"call or return"
				prev sender ifNotNil: [
					"call only"
					(receiver == nil or: [current receiver == receiver])
						ifTrue: [tallies tally: current by: 1]].
				prev _ current]].

</details>

#### SystemDictionary>>#startProfiling

Start profiling the virtual machine.


<details>
	<summary>See more</summary>
	
	startProfiling
	"Start profiling the virtual machine."

	<primitive: 252>

</details>

#### SystemDictionary>>#wordSize

Answer the size in bytes of an object pointer or word in the object memory. The value does not change for a given image, but may be modified by a SystemTracer when converting the image to another format. The value is cached in WordSize to avoid the performance overhead of repeatedly consulting the VM.


<details>
	<summary>See more</summary>
	
	wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	The value does not change for a given image, but may be modified by a SystemTracer
	when converting the image to another format. The value is cached in WordSize to
	avoid the performance overhead of repeatedly consulting the VM."

	"Smalltalk wordSize"

	^ WordSize ifNil: [
		SmallInteger initMinValAndMaxVal.
		WordSize := [self vmParameterAt: 40] on: Error do: [4]]
</details>

#### SystemDictionary>>#hierarchySorted: classes do: aBlock

Sort by hierarchy. Among siblings, sort first by class category and (in same category) by name.


<details>
	<summary>See more</summary>
	
	hierarchySorted: classes do: aBlock
	"Sort by hierarchy. Among siblings, sort first by class category and (in same category) by name."
	| s |
	s _ classes asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ].
			(s includes: cls class) ifTrue: [
				aBlock value: cls class ].
			]
		startingLevel: 1
		sortByCategory: true
</details>

#### SystemDictionary>>#listBuiltinModules

Smalltalk listBuiltinModules


<details>
	<summary>See more</summary>
	
	listBuiltinModules
	"Smalltalk listBuiltinModules"
	"Return a list of all builtin modules (e.g., plugins). Builtin plugins are those that are compiled with the VM directly, as opposed to plugins residing in an external shared library. The list will include all builtin plugins regardless of whether they are currently loaded or not. Note that the list returned is not sorted!"
	| modules index name |
	modules _ WriteStream on: Array new.
	index _ 1.
	[true] whileTrue:[
		name _ self listBuiltinModule: index.
		name ifNil:[^modules contents].
		modules nextPut: name.
		index _ index + 1.
	].
</details>

#### SystemDictionary>>#registerExternalObject: anObject

Register the given object in the external objects array and return its index. If it is already there, just return its index.


<details>
	<summary>See more</summary>
	
	registerExternalObject: anObject
	"Register the given object in the external objects array and return its index. If it is already there, just return its index."

	^ExternalSemaphoreTable registerExternalObject: anObject
</details>

#### SystemDictionary>>#allUnimplementedCalls

Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system.


<details>
	<summary>See more</summary>
	
	allUnimplementedCalls
	"Answer an Array of each message that is sent by an expression in a 
	method but is not implemented by any object in the system."

	| aStream all  selectors |
	all _ self allImplementedMessages.
	aStream _ WriteStream on: (Array new: 50).
	self allBehaviorsDo: [ :cl |
		 cl selectorsDo: [ :sel |
			selectors _ OrderedCollection new.
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [selectors add: m ]].
			selectors notEmpty ifTrue: [
				aStream nextPut: (MethodReference class: cl selector: sel).
				(cl name , '>>#' , sel , (String new: (70-sel size * 15//10 max: 0) withAll: $ ), ' calls:          ', selectors asArray printString) print.
				"aStream nextPut: cl name , ' ' , sel , ' calls: ', secondStream contents" ]]].
	^aStream contents
</details>

#### SystemDictionary>>#allObjectsDo: aBlock

Evaluate the argument, aBlock, for each object in the system, excluding immediates such as SmallInteger and (in Spur) Character.


<details>
	<summary>See more</summary>
	
	allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger and (in Spur) Character."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:
			["Fall back on the old single object primitive code.  With closures, this needs
			  to use an end marker (lastObject) since activation of the block will create
			  new contexts and cause an infinite loop.  The lastObject must be created
			  before calling someObject, so that the VM can settle the enumeration (e.g.
			  by flushing new space) as a side effect of  someObject"
			| object lastObject |
			lastObject := Object new.
			object := self someObject.
			[lastObject == object or: [0 == object]] whileFalse: [
				| nextObject |
				nextObject := object nextObject.
				aBlock value: object.
				object := nextObject ]]
</details>

#### SystemDictionary>>#imagePath

Answer the path for the directory containing the image file.


<details>
	<summary>See more</summary>
	
	imagePath
	"Answer the path for the directory containing the image file."
	"
	Smalltalk imagePath
	"
	| pathName |
	pathName _ self imageName.
	^ pathName copyFrom: 1 to: pathName indexOfLastPathSeparator-1
</details>

#### SystemDictionary>>#unusedClassesAndMethodsWithout: classesAndMessagesPair

Accepts and returns a pair: {set of class names. set of selectors}. It is expected these results will be diff'd with the normally unused results.


<details>
	<summary>See more</summary>
	
	unusedClassesAndMethodsWithout: classesAndMessagesPair
	| classRemovals messageRemovals nClasses nMessages |
	"Accepts and returns a pair: {set of class names. set of selectors}.
	It is expected these results will be diff'd with the normally unused results."

	(classRemovals _ IdentitySet new) addAll: classesAndMessagesPair first.
	(messageRemovals _ IdentitySet new) addAll: classesAndMessagesPair second.
	nClasses _ nMessages _ -1.
	["As long as we keep making progress..."
	classRemovals size > nClasses or: [messageRemovals size > nMessages]]
		whileTrue:
			["...keep trying for bigger sets of unused classes and selectors."
			nClasses _ classRemovals size.
			nMessages _ messageRemovals size.
			Utilities informUser: 'Iterating removals ' ,
					(classesAndMessagesPair first isEmpty
						ifTrue: ['for baseline...']
						ifFalse: ['for ', classesAndMessagesPair first first, ' etc...']) , String newLineString ,
					nClasses printString , ' classes, ' , nMessages printString , ' messages.
|
|' "spacers move menu off cursor"
				during:
				[classRemovals addAll: (self allUnusedClassesWithout: {classRemovals. messageRemovals}).
				messageRemovals addAll: (self allUnSentMessagesWithout: {classRemovals. messageRemovals})]].
	^ {classRemovals. self allUnSentMessagesWithout: {classRemovals. messageRemovals}}
</details>

#### SystemDictionary>>#summary

Needed by CodeFileBrowser to use Smalltalk as the 'case' source


<details>
	<summary>See more</summary>
	
	summary
	"Needed by CodeFileBrowser to use Smalltalk as the 'case' source"
	^ self name
</details>

#### SystemDictionary>>#vmParameterAt: parameterIndex

parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers. Answer with the current value of that register. Answer nil if the VM doesn't provide the register, and the primitive fails. Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the same primitive, whose behavior depends on argument count: 0 args: return an Array of VM parameter values; 1 arg: return the indicated VM parameter; 2 args: set the VM indicated parameter. VM parameters are numbered as follows: 1 end (v3) / size(Spur) of old-space (0-based, read-only) 2 end of young-space (v3) / size of new-space (Spur) (read-only) 3 end (v3) / size(Spur) of heap (read-only) 4 nil (was allocationCount (read-only)) 5 nil (was allocations between GCs (read-write) 6 survivor count tenuring threshold (read-write) 7 full GCs since startup (read-only) 8 total milliseconds in full GCs since startup (read-only) 9 incremental GCs (v3) / scavenges (Spur) since startup (read-only) 10 total milliseconds in incremental GCs (v3) / scavenges (Spur) since startup (read-only) 11 tenures of surving objects since startup or reset (read-write) 12-20 were specific to ikp's JITTER VM, now 12-15 are open for use 16 total microseconds at idle since start-up (if non-zero) 17 fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback) 18 total milliseconds in compaction phase of full GC since start-up (Spur only) 19 scavenge threshold, the effective size of eden. When eden fills to the threshold a scavenge is scheduled. Newer Spur VMs only. 20 utc microseconds at VM start-up (actually at time initialization, which precedes image load). 21 root/remembered table size (occupancy) (read-only) 22 root table overflows since startup (read-only) 23 bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header). 24 memory threshold above which shrinking object memory (rw) 25 memory headroom when growing object memory (rw) 26 interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (rw) 27 number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking 28 number of times sweep loop iterated for current IGC/FGC (read-only) 29 number of times make forward loop iterated for current IGC/FGC (read-only) 30 number of times compact move loop iterated for current IGC/FGC (read-only) 31 number of grow memory requests (read-only) 32 number of shrink memory requests (read-only) 33 number of root table entries used for current IGC/FGC (read-only) 34 Spur: bytes allocated in total since start-up or reset (read-write) (Used to be number of allocations done before current IGC/FGC (read-only)) 35 number of survivor objects after current IGC/FGC (read-only) 36 millisecond clock when current IGC/FGC completed (read-only) 37 number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only) 38 milliseconds taken by current IGC (read-only) 39 Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only) 40 BytesPerOop for this image 41 imageFormatVersion for the VM 42 number of stack pages in use 43 desired number of stack pages (stored in image file header, max 65535) 44 size of eden, in bytes 45 desired size of eden, in bytes (stored in image file header) 46 machine code zone size, in bytes (Cog only; otherwise nil) 47 desired machine code zone size (stored in image file header; Cog only; otherwise nil) 48 various header flags. Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList) Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods Bit 2: if set, preempting a process puts it to the head of its run queue, not the back, i.e. preempting a process by a higher priority one will not cause the preempted process to yield to others at the same priority. Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set) (on VMs that don't support it, those primitives will fail in those cases) 49 max size the image promises to grow the external semaphore table to (0 sets to default, which is 256 as of writing) 50-51 nil; reserved for VM parameters that persist in the image (such as eden above) 52 root/remembered table capacity 53 number of segments (Spur only; otherwise nil) 54 total size of free old space (Spur only, otherwise nil) 55 ratio of growth and image size at or above which a GC will be performed post scavenge 56 number of process switches since startup (read-only) 57 number of ioProcessEvents calls since startup (read-only) 58 number of ForceInterruptCheck calls since startup (read-only) 59 number of check event calls since startup (read-only) 60 number of stack page overflows since startup (read-only) 61 number of stack page divorces since startup (read-only) 62 compiled code compactions since startup (read-only; Cog only; otherwise nil) 63 total milliseconds in compiled code compactions since startup (read-only; Cog only; otherwise nil) 64 the number of methods that currently have jitted machine-code 65 whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMUTABILITY is bit 1 66 the byte size of a stack page 67 the max allowed size of old space (Spur only; nil otherwise; 0 implies no limit except that of the underlying platform) 68 the average number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write) 69 the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write) 70 the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION) 71 the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION) 72 total milliseconds in full GCs Mark phase since startup (read-only) 73 total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors) 74 maximum pause time due to segment allocation


<details>
	<summary>See more</summary>
	
	vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers. 
	Answer with the current value of that register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	end (v3) / size(Spur) of old-space (0-based, read-only)
		2	end of young-space (v3) / size of new-space (Spur) (read-only)
		3	end (v3) / size(Spur) of heap (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	incremental GCs (v3) / scavenges (Spur) since startup (read-only)
		10	total milliseconds in incremental GCs (v3) / scavenges (Spur) since startup (read-only)
		11	tenures of surving objects since startup or reset (read-write)
		12-20 were specific to ikp's JITTER VM, now 12-15 are open for use
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in compaction phase of full GC since start-up (Spur only)
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled. Newer Spur VMs only.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which shrinking object memory (rw)
		25	memory headroom when growing object memory (rw)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (rw)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	Spur: bytes allocated in total since start-up or reset (read-write) (Used to be number of allocations done before current IGC/FGC (read-only))
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use
		43	desired number of stack pages (stored in image file header, max 65535)
		44	size of eden, in bytes
		45	desired size of eden, in bytes (stored in image file header)
		46	machine code zone size, in bytes (Cog only; otherwise nil)
		47	desired machine code zone size (stored in image file header; Cog only; otherwise nil)
		48	various header flags.  
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in a Spur vm, if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	max size the image promises to grow the external semaphore table to (0 sets to default, which is 256 as of writing)
		50-51 nil; reserved for VM parameters that persist in the image (such as eden above)
		52	root/remembered table capacity
		53	number of segments (Spur only; otherwise nil)
		54	total size of free old space (Spur only, otherwise nil)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of ForceInterruptCheck calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only)
		61	number of stack page divorces since startup (read-only)
		62	compiled code compactions since startup (read-only; Cog only; otherwise nil)
		63	total milliseconds in compiled code compactions since startup (read-only; Cog only; otherwise nil)
		64	the number of methods that currently have jitted machine-code
		65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMUTABILITY is bit 1
		66	the byte size of a stack page
		67	the max allowed size of old space (Spur only; nil otherwise; 0 implies no limit except that of the underlying platform)
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)
		71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)
		72  total milliseconds in full GCs Mark phase since startup (read-only)
		73  total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74  maximum pause time due to segment allocation"

	<primitive: 254>
	^nil
</details>

#### SystemDictionary>>#setPlatformPreferences

Set some platform specific preferences on system startup


<details>
	<summary>See more</summary>
	
	setPlatformPreferences
	"Set some platform specific preferences on system startup"
	| platform specs |
	Preferences automaticPlatformSettings ifFalse:[^self].
	platform _ self platformName.
	specs _ 	#(	
					(soundStopWhenDone false)
					(soundQuickStart false)
			).
	platform = 'Win32' ifTrue:[
		specs _ #(	
					(soundStopWhenDone true)
					(soundQuickStart false)
				)].
	platform = 'Mac OS' ifTrue:[
		specs _ #(	
					(soundStopWhenDone false)
					(soundQuickStart true)
				)].
	specs do:[:tuple|
		Preferences setPreference: tuple first toValue: (tuple last == #true).
	].

</details>

#### SystemDictionary>>#browseAllUnSentMessages

Create and schedule a message browser on each method whose message is not sent in any method in the system.


<details>
	<summary>See more</summary>
	
	browseAllUnSentMessages
	"Create and schedule a message browser on each method whose message is  not sent in any method in the system."
	
	self browseAllImplementorsOfList: self allUnSentMessages title: 'Messages implemented but not sent'

"Smalltalk browseAllUnSentMessages"
</details>

#### SystemDictionary>>#allObjectsOrNil

Answer an Array of all objects in the system. Fail if there isn't enough memory to instantiate the result and answer nil.


<details>
	<summary>See more</summary>
	
	allObjectsOrNil
	"Answer an Array of all objects in the system.  Fail if there isn't
	 enough memory to instantiate the result and answer nil."

	<primitive: 178>
	^nil
</details>

#### SystemDictionary>>#unusedBlocks

Answer all methods that contain a block that is not used (not sent a message, returned, passed as an argument, or assigned).


<details>
	<summary>See more</summary>
	
	unusedBlocks
	"Answer all methods that contain a block that is not used (not
	 sent a message, returned, passed as an argument, or assigned)."
	"Smalltalk unusedBlocks"
	"Smalltalk
		browseMessageList: Smalltalk unusedBlocks
		name: 'unused blocks'"
	^self allSelect:
		[:m| | is |
		is := InstructionStream on: m.
		is scanFor: [:b| b = 143 and: [(m at: is thirdByte * 256 + is fourthByte + is pc + 4) = 135]]]
</details>

#### SystemDictionary>>#quitNoSaveTag

<details>
	<summary>See more</summary>
	
	quitNoSaveTag

	^ 'QUIT/NOSAVE' 
</details>

#### SystemDictionary>>#doStartUp: isARealStartup

isARealStartup true: system is coming up (VM and image just started) false: we have just saved an image snapshot, but didn't quit.


<details>
	<summary>See more</summary>
	
	doStartUp: isARealStartup
	"
	isARealStartup
		true: system is coming up (VM and image just started)
		false: we have just saved an image snapshot, but didn't quit.
	"
	"Here, startup begins!"
	Cursor defaultCursor activateCursor.
	self setGCParameters.
	isARealStartup ifTrue: [ self clearExternalObjects ].

	"We need to do this on startup because we can not know if the image was saved with a pre March2019 VM,
	and started with a later VM that handles the option.
	Please see  comment at #doMixedArithmetic"
	self doMixedArithmetic: false.

	self readCommandLineArguments.
	self processStartUpList: isARealStartup.
	isARealStartup ifTrue: [
		self setPlatformPreferences.
		self setStartupStamp ].
</details>

#### SystemDictionary>>#maxExternalSemaphores: aSize

Changes the size of table where external semaphores are registered. The size can only grow, and will always be the next power of two larger than the parameter. Setting this at any time other than start-up can potentially lose requests. i.e. during the realloc new storage is allocated, t he old contents are copied and then pointers are switched. Requests occurring during copying won't be seen if they occur to indices already copied. The intended use is to set the table to some adequate maximum at start-up


<details>
	<summary>See more</summary>
	
	maxExternalSemaphores: aSize
	"Changes the size of table where external semaphores are registered. 
	The size can only grow, and will always be the next power of two larger than the parameter.
	
	Setting this at any time other than start-up can potentially lose requests.
	 i.e. during the realloc new storage is allocated, t
	he old contents are copied and then pointers are switched. 
	 Requests occurring during copying won't be seen if they occur to indices already copied. 
	The intended use is to set the table to some adequate maximum at start-up"
	
	self isRunningCog ifFalse: [^0].
	"The vm-header field is a short, maximum 64k entries. Well, on most platforms anyways "
	(aSize < 0 or: [aSize > 16rFFFF]) ifTrue: [^Error signal: 'Must be in the range (0 to: 16rFFFF)'].
	^self vmParameterAt: 49 put: aSize
</details>

#### SystemDictionary>>#numberOfImplementorsOf: aSelector

Answer a count of the implementors of the given selector found in the system


<details>
	<summary>See more</summary>
	
	numberOfImplementorsOf: aSelector  
	"Answer a count of the implementors of the given selector found in the system"

	| aCount |
	aCount _ 0.
	self allBehaviorsDo:
		[:class |
			(class includesSelector: aSelector)
				ifTrue: [aCount _ aCount + 1]].
	^ aCount
"
Smalltalk numberOfImplementorsOf: #contents.
Smalltalk numberOfImplementorsOf: #nobodyImplementsThis. 
Smalltalk numberOfimplementorsOf: #numberOfImplementorsOf:.
"
</details>

#### SystemDictionary>>#withChangesFileDo: aBlock

<details>
	<summary>See more</summary>
	
	withChangesFileDo: aBlock

	^self currentChangesName asFileEntry readStreamDo: aBlock
</details>

#### SystemDictionary>>#defaultUserChangesName

Answer the default full path to the changes file corresponding to the image file name.


<details>
	<summary>See more</summary>
	
	defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), 
		Preferences userChangesFileNameExtension 
</details>

#### SystemDictionary>>#getSystemAttribute: attributeID

Optional. Answer the string for the system attribute with the given integer ID. Answer nil if the given attribute is not defined on this platform. On platforms that support invoking programs from command lines (e.g., Unix), this mechanism can be used to pass command line arguments to programs written in Squeak. By convention, the first command line argument that is not a VM configuration option is considered a 'document' to be filed in. Such a document can add methods and classes, can contain a serialized object, can include code to be executed, or any combination of these. Currently defined attributes include: -1000 1000th command line argument that specify VM options ... -1 first command line argument that specify VM options 0 the full path name for currently executing VM (or, on some platforms, just the path name of the VM's directory) 1 path name of this image (better use #imageName instead) 2 command line argument See #processCommandLineArguments 3 command line argument ... 1000 last possible command line argument 1001 this platform's operating system 'Mac OS', 'Win32', 'unix', ... 1002 operating system version 1003 this platform's processor type 1004 vm version 1005 window system name 1006 vm build id 1007 Interpreter class (Cog VM only) 1008 Cogit class (Cog VM only) 1009 Platform source version (Cog VM only?) 1201 max filename length (Mac OS only) 1202 file last error (Mac OS only) 10001 hardware details (Win32 only) 10002 operating system details (Win32 only) 10003 graphics hardware details (Win32 only)


<details>
	<summary>See more</summary>
	
	getSystemAttribute: attributeID
  	"Optional. Answer the string for the system attribute with the given 
  	integer ID. Answer nil if the given attribute is not defined on this 
  	platform. On platforms that support invoking programs from command 
  	lines (e.g., Unix), this mechanism can be used to pass command line 
  	arguments to programs written in Squeak.
  
  	By convention, the first command line argument that is not a VM
  	configuration option is considered a 'document' to be filed in. Such a
  	document can add methods and classes, can contain a serialized object,
  	can include code to be executed, or any combination of these.
  
  	Currently defined attributes include: 
  	-1000	1000th command line argument that specify VM options
  	...
  	-1		first command line argument that specify VM options
  	0		the full path name for currently executing VM
  			(or, on some platforms, just the path name of the VM's directory) 
  	1		path name of this image (better use #imageName instead)
  	2		command line argument See #processCommandLineArguments
  	3		command line argument
  	...
  	1000	last possible command line argument
  	1001	this platform's operating system 'Mac OS', 'Win32', 'unix', ...
  	1002	operating system version
  	1003	this platform's processor type
  	1004	vm version
  	1005	window system name
  	1006	vm build id
  	1007	Interpreter class (Cog VM only)
  	1008	Cogit class (Cog VM only)
 	1009	Platform source version (Cog VM only?)
  	1201	max filename length (Mac OS only)
  	1202	file last error (Mac OS only)
  	10001	hardware details (Win32 only)
  	10002	operating system details (Win32 only)
  	10003	graphics hardware details (Win32 only)
  	"

	<primitive: 149>
	^ nil
</details>

#### SystemDictionary>>#vmOptionsDo: aBlock

Repeatedly evaluate aBlock for each vm option specified by the commandline that started Cuis. aBlock has two arguments: the vm option itself and the index (position) Smalltalk vmOptionsDo: [ :option :i | {i. option} print ]


<details>
	<summary>See more</summary>
	
	vmOptionsDo: aBlock
	"Repeatedly evaluate aBlock for each vm option specified by the commandline that started Cuis.
	aBlock has two arguments: the vm option itself and the index (position)
	
	Smalltalk vmOptionsDo: [ :option :i | {i. option} print ]
	"
	| i vmOption |
	i _ -1.
	[vmOption _ Smalltalk getSystemAttribute: i.
	vmOption notNil ] whileTrue: [
		aBlock value: vmOption value: i.
		i _ i-1 ]
</details>

#### SystemDictionary>>#logError: errMsg inContext: aContext to: baseFilename

Log the error message and a stack trace to the given file. Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'


<details>
	<summary>See more</summary>
	
	logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr.txt'
	"

	| localFilename file |
	localFilename _ Preferences debugLogTimestamp
		ifTrue: [ baseFilename, '-', Utilities dateTimeSuffix, '.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file _ DirectoryEntry smalltalkImageDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
	 	 	stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
</details>

#### SystemDictionary>>#browseInstVarRefs: aClass

1/16/96 sw: moved here from Browser so that it could be used from a variety of places. 7/30/96 sw: call chooseInstVarThenDo: to get the inst var choice


<details>
	<summary>See more</summary>
	
	browseInstVarRefs: aClass
	"1/16/96 sw: moved here from Browser so that it could be used from a variety of places.
	 7/30/96 sw: call chooseInstVarThenDo: to get the inst var choice"

	aClass chooseInstVarThenDo: 
		[:aVar | self browseAllAccessesTo: aVar from: aClass]
</details>

#### SystemDictionary>>#testDecompiler

Smalltalk testDecompiler


<details>
	<summary>See more</summary>
	
	testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod _ cls compiledMethodAt: selector.
					oldCodeString _ (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.
</details>

#### SystemDictionary>>#createStackOverflow

For testing the low space handler...


<details>
	<summary>See more</summary>
	
	createStackOverflow
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; createStackOverflow"

	self createStackOverflow.  "infinite recursion"
</details>

#### SystemDictionary>>#presumedSentMessages

In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent: Smalltalk presumedSentMessages


<details>
	<summary>See more</summary>
	
	presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome compactSymbolTable
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c cmdShortcuts asSet do: [ :sel | sent add: sel ]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent
</details>

#### SystemDictionary>>#recreateSpecialObjectsArray

Smalltalk recreateSpecialObjectsArray


<details>
	<summary>See more</summary>
	
	recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: MethodContext.
	newArray at: 11 put: (self at: #MethodContext ifAbsent: [self at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: ((self specialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character numericValue: ascii])."
" 	 This is nil in Spur, which has immediate Characters."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers; an empty array in Spur"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Prototype instances that can be copied for fast initialization"
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
"	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new."
	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization"
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.

	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 36 put: nil. "was the prototype MethodContext"

	newArray at: 37 put: BlockClosure.
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: []).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	
</details>

#### SystemDictionary>>#extraVMMemory

Answer the current setting of the 'extraVMMemory' VM parameter. See the comment in extraVMMemory: for details.


<details>
	<summary>See more</summary>
	
	extraVMMemory
	"Answer the current setting of the 'extraVMMemory' VM parameter. See the comment in extraVMMemory: for details."

	^ Smalltalk vmParameterAt: 23

</details>

#### SystemDictionary>>#profile: aBlock

Make a virtual machine profile of the given block.


<details>
	<summary>See more</summary>
	
	profile: aBlock
	"Make a virtual machine profile of the given block."
	"Note: Profiling support is provided so that VM implementors
	 can better understand and improve the efficiency of the virtual
	 machine. To use it, you must be running a version of the
	 virtual machine compiled with profiling enabled (which
	 makes it much slower than normal even when not profiling).
	 You will also need the CodeWarrior profile reader application."

	self stopProfiling.
	self clearProfile.
	self startProfiling.
	aBlock value.
	self stopProfiling.
	self dumpProfile.
</details>

#### SystemDictionary>>#snapshotPrimitive

Primitive. Write the current state of the object memory on a file in the same format as the Smalltalk-80 release. The file can later be resumed, returning you to this exact state. Return normally after writing the file. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	snapshotPrimitive
	"Primitive. Write the current state of the object memory on a file in the
	same format as the Smalltalk-80 release. The file can later be resumed,
	returning you to this exact state. Return normally after writing the file.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 97>
	^nil "indicates error writing image file"
</details>

#### SystemDictionary>>#snapshot: save andQuit: quit

<details>
	<summary>See more</summary>
	
	snapshot: save andQuit: quit

	self snapshot: save andQuit: quit embedded: false clearAllClassState: false
</details>

#### SystemDictionary>>#browseAllSelect: aBlock

Create and schedule a message browser on each method that, when used as the block argument to aBlock gives a true result. For example, Smalltalk browseAllSelect: [:method | method numLiterals > 10].


<details>
	<summary>See more</summary>
	
	browseAllSelect: aBlock
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result. For example, 
	Smalltalk browseAllSelect: [:method | method numLiterals > 10]."

	^self browseMessageList: (self allSelect: aBlock) name: 'selected messages'
</details>

#### SystemDictionary>>#addToStartUpList: aClass

This will add a ref to this class at the END of the startUp list.


<details>
	<summary>See more</summary>
	
	addToStartUpList: aClass
	"This will add a ref to this class at the END of the startUp list."

	self addToStartUpList: aClass after: nil
</details>

#### SystemDictionary>>#prepareToRenameClass: aClass as: newName

<details>
	<summary>See more</summary>
	
	prepareToRenameClass: aClass as: newName 

	^self prepareToRenameClass: aClass from: aClass name to: newName
</details>

#### SystemDictionary>>#cogitClass

<details>
	<summary>See more</summary>
	
	cogitClass
	^self getSystemAttribute: 1008
</details>

#### SystemDictionary>>#eliotsClosureMeasurementsOn: m over: aFiveArgBlock

See senders. Or try something like: Smalltalk eliotsClosureMeasurementsOn: FileList >> #defaultContents over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf | (Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesNonLocalReturn with: anyClosureUsesSelf)] From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/ by Eliot Miranda Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.


<details>
	<summary>See more</summary>
	
	eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesNonLocalReturn with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesNonLocalReturn anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesNonLocalReturn := anyClosureUsesSelf := false.
	s := InstructionStream on: m.
	s scanFor: [ :b |
		b = 16r8F "16r8F = 143 closure creation" ifTrue: [
			closuresCount := closuresCount + 1].
		(b = 16r8A "16r8A = 138indirect temp vector creation" and: [ s followingByte <= 127]) ifTrue: [
				hasIndirectTemps := true].
		false].
	nextScanStart := m initialPC.
	analyzedClosures := 0.
	[ analyzedClosures < closuresCount ] whileTrue: [
		s pc: nextScanStart; scanFor: [ :b | b = 16r8F ].	"16r8F = 143 Search for first closure"
		analyzedClosures := analyzedClosures + 1.
		thisClosureHasCopied := s followingByte >= 16r10.
		anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
		blkSz := s interpretNextInstructionFor: BlockStartLocator new.		"Findout size of first closure"
		blkPc := s pc.
		s scanFor: [ :b |
			s pc >= (blkPc + blkSz)
				ifTrue: [
					nextScanStart := s pc.
					true]
				ifFalse: [
					b = 16r8F ifTrue: [			
						thisClosureHasCopied := s followingByte >= 16r10.
						anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
						analyzedClosures := analyzedClosures + 1 ].
					anyClosureDoesNonLocalReturn := anyClosureDoesNonLocalReturn or: [s willReturn and: [s willBlockReturn not]].
					anyClosureUsesSelf := anyClosureUsesSelf or: [b = 16r70 "pushSelf"
										or: [b < 16r10 "pushInstVar"
										or: [(b = 16r80 and: [s followingByte <= 16r3F]) "pushInstVar"
										or: [(b between: 16r60 and: 16r60 + 7) "storePopInstVar"
										or: [(b = 16r82 and: [s followingByte <= 63]) "storePopInstVar"
										or: [(b = 16r81 and: [s followingByte <= 63]) "storeInstVar"
										or: [b = 16r84 and: [s followingByte = 160]]]]]]]].
					false]]].
	^aFiveArgBlock valueWithArguments: (Array
			with: closuresCount
			with: hasIndirectTemps
			with: anyClosureHasCopied
			with: anyClosureDoesNonLocalReturn
			with: anyClosureUsesSelf)
</details>

#### SystemDictionary>>#browseLikelyUnnededRedefinitions

Spot unneded method redefinitions: methods that are equal to what would be inherited if they weren't there Smalltalk browseLikelyUnnededRedefinitions Be careful with class side #initialize methods that set up instance class variables In general, class side #initialize methods can't be removed unless empty.


<details>
	<summary>See more</summary>
	
	browseLikelyUnnededRedefinitions

	"
	Spot unneded method redefinitions: methods that are equal to what would be inherited if they weren't there
	Smalltalk browseLikelyUnnededRedefinitions
	Be careful with class side #initialize methods that set up instance class variables
	In general, class side #initialize methods can't be removed unless empty.
	"

	Smalltalk browseAllSelect: [ :cm |
		(cm methodClass superclass ifNotNil: [ :sup | sup lookupSelector: cm selector]) = cm ]
</details>

#### SystemDictionary>>#restoreLostChangesOptions

<details>
	<summary>See more</summary>
	
	restoreLostChangesOptions
	
	^{'Restore lost changes automatically'. 'Restore lost changes manually'. 'Nothing'}.
</details>

#### SystemDictionary>>#removeSelector: descriptor

Safely remove a selector from a class (or metaclass). If the class or the method doesn't exist anymore, never mind and answer nil. This method should be used instead of 'Class removeSelector: #method' to omit global class references.


<details>
	<summary>See more</summary>
	
	removeSelector: descriptor
	"Safely remove a selector from a class (or metaclass). If the class
	or the method doesn't exist anymore, never mind and answer nil.
	This method should be used instead of 'Class removeSelector: #method'
	to omit global class references."

	| class sel |
	class _ Smalltalk at: descriptor first ifAbsent: [^ nil].
	(descriptor size > 2 and: [descriptor second == #class])
		ifTrue:
			[class _ class class.
			sel _ descriptor third]
		ifFalse: [sel _ descriptor second].
	^ class removeSelector: sel
</details>

#### SystemDictionary>>#useUpMemoryWithTinyObjects

For testing the low space handler...


<details>
	<summary>See more</summary>
	
	useUpMemoryWithTinyObjects 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithTinyObjects"

	| b |  "First use up most of memory."
	b _ String new: self bytesLeft - self lowSpaceThreshold - 100000.
	b _ b.  "Avoid unused value warning"
	(1 to: 10000) collect: [:i | BitBlt new]
</details>

#### SystemDictionary>>#primImageName

Answer the full path name for the current image.


<details>
	<summary>See more</summary>
	
	primImageName
	"Answer the full path name for the current image."
	"
	Smalltalk primImageName
	"

	<primitive: 121>
	self primitiveFailed
</details>

#### SystemDictionary>>#associationOrUndeclaredAt: key

return an association or install in undeclared. Used for mating up ImageSegments.


<details>
	<summary>See more</summary>
	
	associationOrUndeclaredAt: key 
	"return an association or install in undeclared. Used for mating up ImageSegments."

	^ self associationAt: key ifAbsent: [
		Undeclared at: key put: nil.
		Undeclared associationAt: key ]
</details>

#### SystemDictionary>>#nopTag

<details>
	<summary>See more</summary>
	
	nopTag

	^ 'NOP'
</details>

#### SystemDictionary>>#allGlobalRefsWithout: classesAndMessagesPair

Answer a set of symbols that may be refs to Global names. This method computes its result in the absence of specified classes and messages. Does not include references from a class to itself


<details>
	<summary>See more</summary>
	
	allGlobalRefsWithout: classesAndMessagesPair
	"Answer a set of symbols that may be refs to Global names. 
	This method computes its result in the absence of specified classes and messages.
	Does not include references from a class to itself"

	| globalRefs absentClasses absentSelectors |
	globalRefs _ IdentitySet new: CompiledMethod instanceCount.
	absentClasses _ classesAndMessagesPair first.
	absentSelectors _ classesAndMessagesPair second.
	self classNames do: [ :cName |
		((absentClasses includes: cName)
					ifTrue: [#()]
					ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl | 
			(absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				"Include all capitalized symbols for good measure"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m | 
					((m isMemberOf: Symbol) and: [ m size > 0] and: [m first isUppercase]) ifTrue: [
						m = cl name ifFalse: [
							globalRefs add: m]].
					(m isVariableBinding) ifTrue:
						[m key ifNotNil: [
							m key = cl name ifFalse: [
								globalRefs add: m key]]]]]]].
	^ globalRefs
</details>

#### SystemDictionary>>#browseAllReferencesToLiteral: aLiteral

Create and schedule a message browser on each method that references aLiteral. For example, Smalltalk browseAllReferencesToLiteral: 47. Smalltalk browseAllReferencesToLiteral: `0 @ 0`.


<details>
	<summary>See more</summary>
	
	browseAllReferencesToLiteral: aLiteral
	"Create and schedule a message browser on each method that references aLiteral. For example, 
	Smalltalk browseAllReferencesToLiteral: 47.
	Smalltalk browseAllReferencesToLiteral: `0  @      0`."
	^ self
		browseMessageList: (self allReferencesToLiteral: aLiteral)
		name: 'References to literal ' , aLiteral asString.
</details>

#### SystemDictionary>>#lostChangesDetectedCaption

<details>
	<summary>See more</summary>
	
	lostChangesDetectedCaption
	
	^
'Last changes may have been lost
(maybe the VM crashed or you had to kill it)
What do you want to do?'
</details>

#### SystemDictionary>>#browseMethodsWithMoreThanOneClosure

Smalltalk browseMethodsWithMoreThanOneClosure


<details>
	<summary>See more</summary>
	
	browseMethodsWithMoreThanOneClosure
	"
	Smalltalk browseMethodsWithMoreThanOneClosure
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				closuresCount > 1 ].
			])
		name: 'Methods with more than one Closure'
</details>

#### SystemDictionary>>#browseAllCallsOn: aLiteral

Create and schedule a message browser on each method that refers to aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:.


<details>
	<summary>See more</summary>
	
	browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Users of ' , aLiteral key
				autoSelect: aLiteral key ]
		ifFalse: [
			self
				browseMessageList: (self allCallsOn: aLiteral) asArray sort
				name: 'Senders of ' , aLiteral
				autoSelect: aLiteral ].
</details>

#### SystemDictionary>>#browseAllSelect: aBlock name: aName autoSelect: autoSelectString

Create and schedule a message browser on each method that, when used as the block argument to aBlock gives a true result. Do not return an #DoIt traces.


<details>
	<summary>See more</summary>
	
	browseAllSelect: aBlock name: aName autoSelect: autoSelectString
	"Create and schedule a message browser on each method that, when used 
	as the block argument to aBlock gives a true result.   Do not return an #DoIt traces."

	"Smalltalk browseAllSelect: [:method | method numLiterals > 10] name: 'Methods with more than 10 literals' autoSelect: 'isDigit'"

	^ self browseMessageList: (self allSelect: aBlock) name: aName autoSelect: autoSelectString
</details>

#### SystemDictionary>>#primLowSpaceSemaphore: aSemaphore

Primitive. Register the given Semaphore to be signalled when the number of free bytes drops below some threshold. Disable low-space interrupts if the argument is nil.


<details>
	<summary>See more</summary>
	
	primLowSpaceSemaphore: aSemaphore
	"Primitive. Register the given Semaphore to be signalled when the
	number of free bytes drops below some threshold. Disable low-space
	interrupts if the argument is nil."

	<primitive: 124>
	self primitiveFailed
</details>

#### SystemDictionary>>#browseAllMethodsInCategory: aSymbol

<details>
	<summary>See more</summary>
	
	browseAllMethodsInCategory: aSymbol 
	^self
		browseMessageList: (self allMethodsInCategory: aSymbol)
		name: aSymbol
</details>

#### SystemDictionary>>#useUpMemoryWithContexts

For testing the low space handler...


<details>
	<summary>See more</summary>
	
	useUpMemoryWithContexts 
	"For testing the low space handler..."
	"Smalltalk installLowSpaceWatcher; useUpMemoryWithContexts"

	self useUpMemoryWithContexts
</details>

#### SystemDictionary>>#renameClassNamed: oldName as: newName

Invoked from fileouts: if there is currently a class in the system named oldName, then rename it to newName. If anything untoward happens, report it in the Transcript.


<details>
	<summary>See more</summary>
	
	renameClassNamed: oldName as: newName
	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	(oldClass _ self at: oldName asSymbol ifAbsent: nil)
		ifNil: [
			Transcript newLine; show: 'Class-rename for ', oldName, ' ignored because ', oldName, ' does not exist.'.
			^ self].

	oldClass rename: newName
</details>

#### SystemDictionary>>#organization

Return the organizer for the receiver


<details>
	<summary>See more</summary>
	
	organization
	"Return the organizer for the receiver"
	^SystemOrganization
</details>

#### SystemDictionary>>#contributionsOf: aString

Smalltalk contributionsOf: 'JMV'


<details>
	<summary>See more</summary>
	
	contributionsOf: aString
	"
	Smalltalk contributionsOf: 'JMV'
	"
	| author answer |
	answer _ OrderedCollection new.
	Smalltalk allBehaviorsDo: [ :behavior |
		behavior methodsDo: [ :compiledMethod |
			author _ compiledMethod author.
			aString = author ifTrue: [
				answer add: {compiledMethod methodClass. compiledMethod selector}]]].
	^answer
</details>

#### SystemDictionary>>#eliotsClosureMeasurements2

Smalltalk eliotsClosureMeasurements2


<details>
	<summary>See more</summary>
	
	eliotsClosureMeasurements2
	"
	Smalltalk eliotsClosureMeasurements2
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesNonLocalReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesNonLocalReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self allSelect: [ :m | 
		self eliotsClosureMeasurementsOn: m over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
			numMethods := numMethods + 1.
			closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
			hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
			anyClosureDoesNonLocalReturn ifTrue: [ anyClosureDoesNonLocalReturnCount := anyClosureDoesNonLocalReturnCount + 1].
			anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
			(anyClosureDoesNonLocalReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
			closuresCount > 0 ifTrue: [
				(anyClosureDoesNonLocalReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
					onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
			anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
			false.
		]
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithNonLocalReturnsInClosures'. anyClosureDoesNonLocalReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}
</details>

#### SystemDictionary>>#unimplemented

Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system.


<details>
	<summary>See more</summary>
	
	unimplemented
	"Answer an Array of each message that is sent by an expression in a method but is not implemented by any object in the system."

	| all unimplemented entry |
	all _ IdentitySet new: Symbol instanceCount * 2.
	self allBehaviorsDo: [ :cl | cl selectorsDo: [ :aSelector | all add: aSelector]].

	unimplemented _ IdentityDictionary new.
	self allBehaviorsDo: [:cl |
		 cl selectorsDo: [:sel |
			(cl compiledMethodAt: sel) messages do: [ :m |
				(all includes: m) ifFalse: [
					entry _ unimplemented at: m ifAbsent: [Array new].
					entry _ entry copyWith: (cl name, '>', sel).
					unimplemented at: m put: entry]]]].

	"remove some clutter from the result:"
	#(doPrimitive: primitiveFail success:) do: [ :sel |
		unimplemented removeKey: sel ifAbsent: nil].

	^ unimplemented
</details>

#### SystemDictionary>>#primGetCurrentWorkingDirectoryWindows

Call getcwd() to get the current working directory. Shamelessly taken from OSProcess.pck.st and copied here. Answer nil on fail (i.e., non-Windows) Note: Unfortunately, yhis windows implementation seems to always answer the directory containing the image, and not the real Windows current directory. See #getCurrentWorkingDirectory


<details>
	<summary>See more</summary>
	
	primGetCurrentWorkingDirectoryWindows
	"Call getcwd() to get the current working directory.
	Shamelessly taken from OSProcess.pck.st and copied here.
	Answer nil on fail (i.e., non-Windows)
	Note:
		Unfortunately, yhis windows implementation seems to always answer the directory 
		containing the image, and not the real Windows current directory.
		See #getCurrentWorkingDirectory"

	"
	Smalltalk primGetCurrentWorkingDirectoryWindows
	"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'Win32OSProcessPlugin'>
	^ nil
</details>

#### SystemDictionary>>#isThereAReferenceTo: aLiteral

Answer a Collection of all the methods that call on aLiteral.


<details>
	<summary>See more</summary>
	
	isThereAReferenceTo: aLiteral
	"Answer a Collection of all the methods that call on aLiteral."
	"
	Smalltalk isThereAReferenceTo: #open:label:
	"
	| special byte |

	#(23 48 'fred' (new open:label:)) size.
"Example above should find #open:label:, though it is deeply embedded here."

	special _ self hasSpecialSelector: aLiteral ifTrueSetByte: [:b | byte _ b ].
	self allBehaviorsDo: [:class |
		(class whichSelectorsReferTo: aLiteral special: special byte: byte) do: [ :sel |
			^true ]].
	^ false
</details>

#### SystemDictionary>>#send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument

Send the message #startUp: or #shutDown: to each class named in the list. The argument indicates if the system is about to quit (for #shutDown:) or if the image is ia real startup (or just continue after image save) (for #startUp:). If any name cannot be found, then remove it from the list.


<details>
	<summary>See more</summary>
	
	send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is ia real startup (or just continue after image save) (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals class |
	removals _ OrderedCollection new.
	startUpOrShutDownList do:
		[:name |
		class _ self at: name ifAbsent: nil.
		class
			ifNil: [removals add: name]
			ifNotNil: [
				class isInMemory ifTrue: [
					class perform: startUpOrShutDown with: argument]]].

	"Remove any obsolete entries, but after the iteration"
	"Well, not. Better just ignore them. Maybe it is stuff, like SoundPlayer, that was moved to optional packages, and can be loaded again anytime."
	"startUpOrShutDownList removeAll: removals"
</details>

#### SystemDictionary>>#removeEmptyMessageCategories

Smalltalk removeEmptyMessageCategories


<details>
	<summary>See more</summary>
	
	removeEmptyMessageCategories
	"Smalltalk removeEmptyMessageCategories"
	Smalltalk garbageCollect.
	(ClassOrganizer allInstances copyWith: SystemOrganization) do:
		[:org | org removeEmptyCategories]
</details>

#### SystemDictionary>>#browseEqEqSentToSmallIntegerConstants

Smalltalk browseEqEqSentToSmallIntegerConstants


<details>
	<summary>See more</summary>
	
	browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch _ false.
	visitor _ ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantNumber or: [ node arguments first isConstantNumber ]]]) ifTrue: [
						hasMatch _ true ]]
		select: [ :node |
			hasMatch not ].
	Smalltalk browseAllSelect: [ :method |
		hasMatch _ false.
		method decompile accept: visitor.
		hasMatch ].
</details>

#### SystemDictionary>>#growMemoryByAtLeast: numBytes

Only for Spur!


<details>
	<summary>See more</summary>
	
	growMemoryByAtLeast: numBytes
	"Only for Spur!"
	"Grow memory by at least the requested number of bytes.
	 Primitive.  Fail if no memory is available.  Essential.
	 N.B. In Spur, the heap is composed of segments. Growing memory adds a new
	 segment; it does not extend existing segments. Growth is by at least that
	 established by SmalltalkImage current vmParameterAt: 25, which defaults to 16Mb."
	<primitive: 180>
	^(numBytes isInteger and: [numBytes > 0])
		ifTrue: [OutOfMemory signal]
		ifFalse: [self primitiveFailed]
</details>

#### SystemDictionary>>#browseMessageList: messageList name: label

Create and schedule a MessageSet browser on messageList.


<details>
	<summary>See more</summary>
	
	browseMessageList: messageList name: label 
	"Create and schedule a MessageSet browser on messageList."
	^ self browseMessageList: messageList name: label autoSelect: nil
</details>

#### SystemDictionary>>#vmPath

Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented.


<details>
	<summary>See more</summary>
	
	vmPath
	"Answer the path for the directory containing the Smalltalk virtual machine. Return the empty string if this primitive is not implemented."
	"
	Smalltalk vmPath
	"

	| answer |
	answer _ self primVmPath.
	"On some setups (Cog VM in Git Bash under Windows)
		Smalltalk primVmPath 
				answer begins with 'c:\' (lowercase)
	but 
		(FileDirectory on: '') primLookupEntryIn: '' index: 1
				answer is  #('C:' 0 0 true 0) (uppercase)
	Make Windows drive letters be capitalized!
	"
	(answer size > 1 and: [
		answer first isLowercase and: [
			answer second isDriveSeparator ]]) ifTrue: [
					answer _ answer capitalized ].
	^answer ifNil: [ '' ]
</details>

#### SystemDictionary>>#handleUserInterrupt

<details>
	<summary>See more</summary>
	
	handleUserInterrupt
	Preferences cmdDotEnabled ifTrue: [
		[ UISupervisor userInterrupt ] fork]
</details>

#### SystemDictionary>>#platformSubtype

Return the subType of the platform we're running on


<details>
	<summary>See more</summary>
	
	platformSubtype
	"Return the subType of the platform we're running on"
	^self getSystemAttribute: 1003
</details>

#### SystemDictionary>>#condenseSources

Move all the changes onto a compacted sources file.


<details>
	<summary>See more</summary>
	
	condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount newVersionString oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :barBlock |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					barBlock value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFileEntry delete.
		oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName _ self defaultChangesName.
	newChangesPathName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '
</details>

#### SystemDictionary>>#lowSpaceWatcherProcess

<details>
	<summary>See more</summary>
	
	lowSpaceWatcherProcess
	^LowSpaceProcess
</details>

#### SystemDictionary>>#hierachySortedAllClassesDo: aBlock

Evaluate the argument, aBlock, for each class in the system. Smalltalk hierachySortedAllClassesDo: [ :cls | cls print ]


<details>
	<summary>See more</summary>
	
	hierachySortedAllClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system.
	Smalltalk hierachySortedAllClassesDo: [ :cls | cls print ]
	"

	| s |
	s _ self allClasses asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ]]
		startingLevel: 1
		sortByCategory: true
</details>

#### SystemDictionary>>#specialNargsAt: anInteger

Answer the number of arguments for the special selector at: anInteger.


<details>
	<summary>See more</summary>
	
	specialNargsAt: anInteger 
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2
</details>

## SystemOrganizer

My instances provide an organization for the classes in the system, just as a ClassOrganizer organizes the messages within a class. The only difference is the methods for fileIn/Out.

### Methods
#### SystemOrganizer>>#fileOutCategory: category on: aFileStream initializing: aBool

Store on the file associated with aFileStream, all the classes associated with the category and any requested shared pools.


<details>
	<summary>See more</summary>
	
	fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes _ (self superclassOrderIn: category).
	poolSet _ Set new.
	classes do: 
		[:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet size > 0 ifTrue:
		[tempClass _ Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first _ true.
	classes do: 
		[:class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false].
	aBool ifTrue:[classes do:[:cls| cls fileOutInitializerOn: aFileStream]].
</details>

#### SystemOrganizer>>#superclassOrderIn: category

Answer an OrderedCollection containing references to the classes in the category whose name is the argument, category (a string). The classes are ordered with superclasses first so they can be filed in.


<details>
	<summary>See more</summary>
	
	superclassOrderIn: category 
	"Answer an OrderedCollection containing references to the classes in the 
	category whose name is the argument, category (a string). The classes 
	are ordered with superclasses first so they can be filed in."

	| list |
	list _ 
		(self listAtCategoryNamed: category asSymbol) 
			collect: [:title | Smalltalk at: title].
	^Array streamContents: [ :strm |
		Smalltalk hierarchySorted: list do: [ :cls | strm nextPut: cls ]].
</details>

#### SystemOrganizer>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

self == SystemOrganization ifTrue: [
	dp _ DiskProxy global: #SystemOrganization selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp].
^ self

</details>

#### SystemOrganizer>>#fileOutCategory: category

FileOut all the classes in the named system category.


<details>
	<summary>See more</summary>
	
	fileOutCategory: category
	"FileOut all the classes in the named system category."

	DirectoryEntry smalltalkImageDirectory // (category asFileName , '.st') writeStreamDo: [ :fileStream |
		self fileOutCategory: category on: fileStream initializing: true ]
</details>

#### SystemOrganizer>>#removeMissingClasses

Remove any class names that are no longer in the Smalltalk dictionary. Used for cleaning up after garbage collecting user-generated classes.


<details>
	<summary>See more</summary>
	
	removeMissingClasses
	"Remove any class names that are no longer in the Smalltalk dictionary. Used for cleaning up after garbage collecting user-generated classes."
	"SystemOrganization removeMissingClasses"

	elementArray copy do: [:el |
		(Smalltalk includesKey: el) ifFalse: [self removeElement: el]].

</details>

#### SystemOrganizer>>#superclassOrderInAll: categories

<details>
	<summary>See more</summary>
	
	superclassOrderInAll: categories

	| classes |

	classes := OrderedCollection new.
	categories do: [ :aCategory | classes addAll: (self classesAt: aCategory)].

	^Array streamContents: [ :stream | Smalltalk hierarchySorted: classes do: [ :aClass | stream nextPut: aClass ]].
</details>

#### SystemOrganizer>>#removeCategoriesMatching: matchString

Remove all matching categories with their classes


<details>
	<summary>See more</summary>
	
	removeCategoriesMatching: matchString
	"Remove all matching categories with their classes"
	(self categoriesMatching: matchString) do:
		[:c | self removeSystemCategory: c]
</details>

#### SystemOrganizer>>#fileOutAllCategories

Cursor write showWhile: [ SystemOrganization fileOutAllCategories ]


<details>
	<summary>See more</summary>
	
	fileOutAllCategories
	"
	Cursor write showWhile: [
		SystemOrganization fileOutAllCategories ]
	"
	((Smalltalk imageName withoutSuffix: '.image'), '-AllCode.st') asFileEntry writeStreamDo: [ :stream |
		self categories do: [ :category |
			self fileOutCategoryNoPoolsNoInit: category on: stream ]]
</details>

#### SystemOrganizer>>#fileOutCategoryNoPoolsNoInit: category on: aFileStream

Store on the file associated with aFileStream, all the classes associated with the category and any requested shared pools.


<details>
	<summary>See more</summary>
	
	fileOutCategoryNoPoolsNoInit: category on: aFileStream
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first classes |
	classes _ self superclassOrderIn: category.
	first _ true.
	classes do: [ :class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false]
</details>

#### SystemOrganizer>>#removeSystemCategory: category

remove all the classes associated with the category


<details>
	<summary>See more</summary>
	
	removeSystemCategory: category
	"remove all the classes associated with the category"

	(self superclassOrderIn: category) reverseDo: [ :class | class removeFromSystem].

	self removeCategory: category.

</details>

#### SystemOrganizer>>#removeSystemCategories: categories

<details>
	<summary>See more</summary>
	
	removeSystemCategories: categories

	(self superclassOrderInAll: categories) reverseDo: [ :class | class removeFromSystem].

	categories do: [ :aCategory | self removeCategory: aCategory].

</details>

#### SystemOrganizer>>#categoriesMatching: matchString

Return all matching categories


<details>
	<summary>See more</summary>
	
	categoriesMatching: matchString
	"Return all matching categories"
	^ self categories select: [:c | matchString match: c]
</details>

#### SystemOrganizer>>#hasCategory: aCategory

<details>
	<summary>See more</summary>
	
	hasCategory: aCategory

	^self categories includes: aCategory 
</details>

## SystemVersion

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SystemVersion>>#isCuis

Sure we are!! Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!


<details>
	<summary>See more</summary>
	
	isCuis
	"Sure we are!!
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^true
</details>

#### SystemVersion>>#date: newDate

<details>
	<summary>See more</summary>
	
	date: newDate
	date _ newDate
</details>

#### SystemVersion>>#isSqueak

Nope. We are Cuis. Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!


<details>
	<summary>See more</summary>
	
	isSqueak
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^false
</details>

#### SystemVersion>>#versionRevision

Answer an array of version.revision. Try to do something not completely unlike 'Semantic Versioning'. For example, for Cuis 4.1, updated to #1744, answer { 41 . 1744 } SystemVersion current versionRevision


<details>
	<summary>See more</summary>
	
	versionRevision
	"Answer an array of version.revision.
	Try to do something not completely unlike 'Semantic Versioning'.
	For example, for Cuis 4.1, updated to #1744, answer
	{ 41 . 1744 }
	SystemVersion current versionRevision
	"
	^{versionMajor * 10 + versionMinor . self highestUpdate}
</details>

#### SystemVersion>>#versionMajor

<details>
	<summary>See more</summary>
	
	versionMajor
	^versionMajor
</details>

#### SystemVersion>>#updates

<details>
	<summary>See more</summary>
	
	updates
	^updates
</details>

#### SystemVersion>>#unregisterUpdate: update

<details>
	<summary>See more</summary>
	
	unregisterUpdate: update
	updates remove: update ifAbsent: nil
</details>

#### SystemVersion>>#highestUpdate: anInteger

<details>
	<summary>See more</summary>
	
	highestUpdate: anInteger
	highestUpdate _ anInteger
</details>

#### SystemVersion>>#versionMinor

<details>
	<summary>See more</summary>
	
	versionMinor
	^versionMinor
</details>

#### SystemVersion>>#isPharo

Nope. We are Cuis. Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!


<details>
	<summary>See more</summary>
	
	isPharo
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^false
</details>

#### SystemVersion>>#printOn: stream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: stream
	stream
		nextPutAll: self version;
		nextPutAll: ' update ' , self highestUpdate printString
</details>

#### SystemVersion>>#versionMajor: aNumber versionMinor: anotherNumber

<details>
	<summary>See more</summary>
	
	versionMajor: aNumber versionMinor: anotherNumber
	versionMajor _ aNumber.
	versionMinor _ anotherNumber.
	date _ Date today.
	updates _ OrderedCollection new.

</details>

#### SystemVersion>>#version

<details>
	<summary>See more</summary>
	
	version
	^String streamContents: [ :strm |
		strm nextPutAll: 'Cuis '.
		versionMajor printOn: strm.
		strm nextPut: $. .
		versionMinor printOn: strm ]
</details>

#### SystemVersion>>#date

<details>
	<summary>See more</summary>
	
	date
	^date
</details>

#### SystemVersion>>#highestUpdate

<details>
	<summary>See more</summary>
	
	highestUpdate
	highestUpdate ifNil: [
		highestUpdate _ updates
			ifEmpty: [ 0 ]
			ifNotEmpty: [
				(updates detectMax: [ :updateName | updateName initialIntegerOrNil ifNil: [0]])
					initialIntegerOrNil ifNil: [0]]].
	^ highestUpdate
</details>

#### SystemVersion>>#registerUpdate: changeSetOrPackageName

<details>
	<summary>See more</summary>
	
	registerUpdate: changeSetOrPackageName

	changeSetOrPackageName initialIntegerOrNil ifNotNil: [ :number |
		highestUpdate _ self highestUpdate max: number ].
	updates add: changeSetOrPackageName
</details>

## Transcript

A new implementation of Transcript. - Thread safe. - Very fast. - Independent of Morphic or any other UI framework. - Immediate feedback. - Can log to file. - Not an editor. Only used for output. - All protocol is on the Class side

### Methods
## UISupervisor

UISupervisor is an interface to User Interface services not tied to any specific GUI. There could even be no GUI. All state and behavior is on the class side

### Methods
## Utilities

A repository for general and miscellaneous utilities; much of what is here are in effect global methods that don't naturally attach to anything else. 1/96 sw

### Methods
#### Utilities>>#seeClassSide

All the code for Utilitites is on the class side


<details>
	<summary>See more</summary>
	
	seeClassSide
	"All the code for Utilitites is on the class side"
</details>

