## AbstractHierarchicalList

Contributed by Bob Arning as part of the ObjectExplorer package.

### Methods
#### AbstractHierarchicalList>>#noteNewSelection: x

<details>
	<summary>See more</summary>
	
	noteNewSelection: x

	currentSelection _ x.
	self changed: #getCurrentSelection.
	currentSelection ifNil: [^self].
	currentSelection sendSettingMessageTo: self.

</details>

#### AbstractHierarchicalList>>#getCurrentSelection

<details>
	<summary>See more</summary>
	
	getCurrentSelection

	^currentSelection
</details>

## FileDirectoryWrapper

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FileDirectoryWrapper>>#settingSelector

<details>
	<summary>See more</summary>
	
	settingSelector

	^#setSelectedDirectoryTo:
</details>

#### FileDirectoryWrapper>>#balloonText

<details>
	<summary>See more</summary>
	
	balloonText

	^balloonText
</details>

#### FileDirectoryWrapper>>#balloonText: aStringOrNil

<details>
	<summary>See more</summary>
	
	balloonText: aStringOrNil

	balloonText _ aStringOrNil
</details>

#### FileDirectoryWrapper>>#contents

<details>
	<summary>See more</summary>
	
	contents

	^(item directoryNames asArray sort: [ :a :b | a caseInsensitiveLessOrEqual: b]) collect: [ :n | 
		FileDirectoryWrapper with: item / n name: n model: model ]
</details>

#### FileDirectoryWrapper>>#setItem: anObject name: aString model: aModel

<details>
	<summary>See more</summary>
	
	setItem: anObject name: aString model: aModel

	item _ anObject.
	model _ aModel.
	itemName _ aString.
	hasContents _ nil.

</details>

#### FileDirectoryWrapper>>#asString

Answer a string that represents the receiver. Don't include extra quotes for Strings. This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray, the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired). It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day. In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability. In any case, in your code, if possible, use a more specific method. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	asString
	| result |
	result := itemName.
	"
	('_*_' match: result)
		ifTrue: [ result := (result copyFrom: 2 to: result size - 1) ].
	"
	^ result 
</details>

#### FileDirectoryWrapper>>#hasContents

Return whether this directory has subfolders. The value is cached to avoid a performance penalty. Also for performance reasons, the code below will just assume that the directory does indeed have contents in a few of cases: 1. If the item is not a FileDirectory (thus avoiding the cost of refreshing directories that are not local) (jmv: REMOVED) 2. If it's the root directory of a given volume 3. If there is an error computing the FileDirectory's contents (jmvL: REMOVED)


<details>
	<summary>See more</summary>
	
	hasContents
	"Return whether this directory has subfolders. The value is cached to 
	avoid a performance penalty.	Also for performance reasons, the code 
	below will just assume that the directory does indeed have contents in a 
	few of cases:  
	1. If the item is not a FileDirectory (thus avoiding the cost 
	of refreshing directories that are not local)  (jmv: REMOVED)
	2. If it's the root directory of a given volume 
	3. If there is an error computing the FileDirectory's contents (jmvL: REMOVED)
	"
	hasContents
		ifNil: [
			hasContents := true. "default"
			item isRoot
				ifFalse: [hasContents := self contents notEmpty ]].
	^ hasContents
</details>

## ListItemWrapper

Contributed by Bob Arning as part of the ObjectExplorer package.

### Methods
#### ListItemWrapper>>#withoutListWrapper

<details>
	<summary>See more</summary>
	
	withoutListWrapper

	^item withoutListWrapper
</details>

#### ListItemWrapper>>#canBeDragged

<details>
	<summary>See more</summary>
	
	canBeDragged

	^true
</details>

#### ListItemWrapper>>#wantsDroppedObject: anotherItem

<details>
	<summary>See more</summary>
	
	wantsDroppedObject: anotherItem

	^false
</details>

#### ListItemWrapper>>#asString

Answer a string that represents the receiver. Don't include extra quotes for Strings. This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray, the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired). It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day. In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability. In any case, in your code, if possible, use a more specific method. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	asString

	^item asString
</details>

#### ListItemWrapper>>#item

<details>
	<summary>See more</summary>
	
	item
	^item
</details>

#### ListItemWrapper>>#hasEquivalentIn: aCollection

<details>
	<summary>See more</summary>
	
	hasEquivalentIn: aCollection

	aCollection detect: [ :each | 
		each withoutListWrapper = item withoutListWrapper
	] ifNone: [^false].
	^true
</details>

#### ListItemWrapper>>#setItem: anObject

<details>
	<summary>See more</summary>
	
	setItem: anObject

	item _ anObject
</details>

#### ListItemWrapper>>#settingSelector

<details>
	<summary>See more</summary>
	
	settingSelector

	^nil
</details>

#### ListItemWrapper>>#preferredColor

<details>
	<summary>See more</summary>
	
	preferredColor

	^nil
</details>

#### ListItemWrapper>>#sendSettingMessageTo: aModel

<details>
	<summary>See more</summary>
	
	sendSettingMessageTo: aModel

	aModel 
		perform: (self settingSelector ifNil: [^self])
		with: self withoutListWrapper

</details>

#### ListItemWrapper>>#balloonText

<details>
	<summary>See more</summary>
	
	balloonText

	^nil
</details>

#### ListItemWrapper>>#handlesMouseOver: evt

<details>
	<summary>See more</summary>
	
	handlesMouseOver: evt

	^false
</details>

#### ListItemWrapper>>#contents

<details>
	<summary>See more</summary>
	
	contents

	^Array new
</details>

#### ListItemWrapper>>#setItem: anObject model: aModel

<details>
	<summary>See more</summary>
	
	setItem: anObject model: aModel

	item _ anObject.
	model _ aModel.
</details>

#### ListItemWrapper>>#acceptDroppingObject: anotherItem

<details>
	<summary>See more</summary>
	
	acceptDroppingObject: anotherItem

	^item acceptDroppingObject: anotherItem
</details>

#### ListItemWrapper>>#hasContents

<details>
	<summary>See more</summary>
	
	hasContents

	^self contents notEmpty
</details>

## ObjectExplorer

ObjectExplorer provides a hierarchical alternative to #inspect. Simply evaluate an expression like: self runningWorld explore and enjoy.

### Methods
#### ObjectExplorer>>#basicMonitorList

<details>
	<summary>See more</summary>
	
	basicMonitorList
	^monitorList
</details>

#### ObjectExplorer>>#doItReceiver

Answer the object that should be informed of the result of evaluating a text selection.


<details>
	<summary>See more</summary>
	
	doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	currentSelection ifNil: [^rootObject].
	^currentSelection withoutListWrapper

</details>

#### ObjectExplorer>>#bindingNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingNamesDo: aBlock
	self doItReceiver class allInstVarNames do: aBlock
</details>

#### ObjectExplorer>>#object

<details>
	<summary>See more</summary>
	
	object
	^currentSelection ifNotNil: [ :cs | cs withoutListWrapper ]
</details>

#### ObjectExplorer>>#hasBindingOf: aString

<details>
	<summary>See more</summary>
	
	hasBindingOf: aString
	^ self doItReceiver class allInstVarNames includes: aString
</details>

#### ObjectExplorer>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^currentSelection ifNotNil: [ :cs | cs selector ]
</details>

#### ObjectExplorer>>#monitorList

<details>
	<summary>See more</summary>
	
	monitorList
	^monitorList ifNil: [ monitorList _ WeakIdentityKeyDictionary new ].
</details>

#### ObjectExplorer>>#stopMonitoring

<details>
	<summary>See more</summary>
	
	stopMonitoring
	monitorList _ nil
</details>

#### ObjectExplorer>>#stepAt: millisecondSinceLast

If there's anything in my monitor list, see if the strings have changed.


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast

	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes _ true ]]].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]
</details>

#### ObjectExplorer>>#textStylerClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	textStylerClassFor: textGetter

	^SHTextStylerST80
</details>

#### ObjectExplorer>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
</details>

#### ObjectExplorer>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer true to allow styling to proceed, or false to veto the styling"

	anSHTextStyler workspace: self.
	^true
</details>

#### ObjectExplorer>>#rootObject: anObject

<details>
	<summary>See more</summary>
	
	rootObject: anObject
	rootObject _ anObject
</details>

#### ObjectExplorer>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	^SmalltalkEditor
</details>

#### ObjectExplorer>>#parentObject

<details>
	<summary>See more</summary>
	
	parentObject
	currentSelection ifNil: [ ^nil ].
	currentSelection parent ifNil: [ ^rootObject ].
	^currentSelection parent withoutListWrapper
</details>

#### ObjectExplorer>>#inspectSelection

<details>
	<summary>See more</summary>
	
	inspectSelection
	self object inspect
</details>

#### ObjectExplorer>>#getList

<details>
	<summary>See more</summary>
	
	getList

	^Array with: (ObjectExplorerWrapper with: rootObject name: 'root' model: self parent: nil)

</details>

#### ObjectExplorer>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^aParsingErrorBlock value: nil
</details>

#### ObjectExplorer>>#doItContext

Answer the context in which a text selection can be evaluated.


<details>
	<summary>See more</summary>
	
	doItContext
	"Answer the context in which a text selection can be evaluated."

	^nil
</details>

#### ObjectExplorer>>#autoCompleterClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	autoCompleterClassFor: textGetter
	^SmalltalkCompleter
</details>

#### ObjectExplorer>>#selectedClass

Answer the class of the receiver's current selection


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class of the receiver's current selection"

	^self doItReceiver class

</details>

#### ObjectExplorer>>#rootObject

<details>
	<summary>See more</summary>
	
	rootObject
	^rootObject
</details>

## ObjectExplorerWrapper

Contributed by Bob Arning as part of the ObjectExplorer package.

### Methods
#### ObjectExplorerWrapper>>#refresh

hack to refresh item given an object and a string that is either an index or an instance variable name.


<details>
	<summary>See more</summary>
	
	refresh
	"hack to refresh item given an object and a string that is either an index or an instance variable name."
	[ | index |
		(model class allInstVarNames includes: itemName)
			ifTrue: [ item _ model instVarNamed: itemName ]
			ifFalse: [ index _ itemName asNumber.
				(index between: 1 and: model basicSize) ifTrue: [ item _ model basicAt: index]]
	] on: UnhandledError do: [ :ex | item _ nil.  ex return ]
</details>

#### ObjectExplorerWrapper>>#setItem: anObject name: aString model: aModel parent: itemParent

<details>
	<summary>See more</summary>
	
	setItem: anObject name: aString model: aModel parent: itemParent
	parent _ itemParent.
	self setItem: anObject name: aString model: aModel
</details>

#### ObjectExplorerWrapper>>#canBeDragged

<details>
	<summary>See more</summary>
	
	canBeDragged

	^false
</details>

#### ObjectExplorerWrapper>>#parent

<details>
	<summary>See more</summary>
	
	parent
	^parent
</details>

#### ObjectExplorerWrapper>>#asString

Answer a string that represents the receiver. Don't include extra quotes for Strings. This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray, the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired). It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day. In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability. In any case, in your code, if possible, use a more specific method. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	asString
	| explorerString |
	explorerString _ [ item printString ]
		on: UnhandledError 
		do: [:ex | ex return: '<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	^itemName , ': ' , explorerString :: withBlanksCondensed
</details>

#### ObjectExplorerWrapper>>#selector

<details>
	<summary>See more</summary>
	
	selector
	parent ifNil: [ ^nil ].
	^(parent withoutListWrapper class allInstVarNames includes: itemName) ifTrue: [ itemName asSymbol ]
</details>

#### ObjectExplorerWrapper>>#contents

<details>
	<summary>See more</summary>
	
	contents

	(item customizeExplorerContents) ifTrue: [^item explorerContents].
	"For all others, show named vars first, then indexed vars"
	^(item class allInstVarNames asOrderedCollection withIndexCollect: [:each :index |
		self class
			with: (item instVarAt: index)
			name: each
			model: item
			parent: self]) ,
	((1 to: item basicSize) collect: [:index |
		self class
			with: (item basicAt: index)
			name: index printString
			model: item
			parent: self])
</details>

#### ObjectExplorerWrapper>>#setItem: anObject name: aString model: aModel

<details>
	<summary>See more</summary>
	
	setItem: anObject name: aString model: aModel

	item _ anObject.
	model _ aModel.
	itemName _ aString.
</details>

#### ObjectExplorerWrapper>>#parent: anObject

<details>
	<summary>See more</summary>
	
	parent: anObject
	parent _ anObject
</details>

#### ObjectExplorerWrapper>>#itemName

<details>
	<summary>See more</summary>
	
	itemName
	^itemName
</details>

#### ObjectExplorerWrapper>>#hasContents

<details>
	<summary>See more</summary>
	
	hasContents

	^item hasContentsInExplorer
	

</details>

