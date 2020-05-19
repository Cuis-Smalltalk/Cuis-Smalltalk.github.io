## CodePackage

A CodePackage is a package that is currently loaded in the system. If saved (.pck.st), then it is stored in a file that can be dealt with as an instance of CodePackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged. CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable. We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like (CodePackage named: 'Collections' createIfAbsent: true registerIfNew: false) inspect; save This won't mean the system is actually partitioned in such way. (CodePackage named: 'TestPackage' createIfAbsent: true registerIfNew: false) inspect; save

### Methods
#### CodePackage>>#packageNameAndVersion

<details>
	<summary>See more</summary>
	
	packageNameAndVersion
	^String streamContents: [ :strm |
		self printNameAndVersionOn: strm ]
</details>

#### CodePackage>>#hasAnyExtensionCategoriesForClass: aClassOrMetaClass

Pass the class as the argument for instance side. Pass the metaclass as the argument for class side.


<details>
	<summary>See more</summary>
	
	hasAnyExtensionCategoriesForClass: aClassOrMetaClass
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization hasAnyCategoriesSuchThat: [ :cat |
		self isYourClassExtension: cat ]
</details>

#### CodePackage>>#writeCoreMethodsForFileinOf: aClass on: aStream

<details>
	<summary>See more</summary>
	
	writeCoreMethodsForFileinOf: aClass on: aStream

	self coreMethodsForFileinOf: aClass do: [ :methodReference |
		methodReference isValid
			ifTrue: [
				self writeMethod: methodReference on: aStream ]]
</details>

#### CodePackage>>#classNames

<details>
	<summary>See more</summary>
	
	classNames
	| classNames |
	classNames := Set new.
	self classesDo: [ :cls | classNames add: cls name ].
	^classNames
</details>

#### CodePackage>>#writeFeatureSpecOn: aStream

<details>
	<summary>See more</summary>
	
	writeFeatureSpecOn: aStream
	| provides |
	provides _ featureSpec provides.
	aStream
		nextPut: $!; 
		nextChunkPut: 'provides: ', provides name asString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!; 
			nextChunkPut: 'requires: ', requires name asString printString, ' ', requires minVersion printString, ' ', requires minRevision printString, ' ', requires maxVersion printString;
			newLine ]
</details>

#### CodePackage>>#methods

<details>
	<summary>See more</summary>
	
	methods
	^ (self extensionMethods, self coreMethods) select: [:method |
		method isValid ]
</details>

#### CodePackage>>#codePackageClass

Answer the specific CodePackage subclass to use.


<details>
	<summary>See more</summary>
	
	codePackageClass
	"Answer the specific CodePackage subclass to use."

	self class == CodePackage ifFalse: [
		^ self class ].
	self classesDo: [ :cls |
		(cls inheritsFrom: CodePackage)
			ifTrue: [ ^ cls ]].
	^ nil
</details>

#### CodePackage>>#packageName: aString

<details>
	<summary>See more</summary>
	
	packageName: aString
	packageName _ aString.
	description _ ''.
	featureSpec _ FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: 1 revision: 0).
	hasUnsavedChanges _ self includesAnyCode.
	"But reset revision if it was incremented because of marking it dirty!"
	featureSpec provides name: packageName version: 1 revision: 0
</details>

#### CodePackage>>#writeSystemCategoriesOn: aStream

<details>
	<summary>See more</summary>
	
	writeSystemCategoriesOn: aStream

	self systemCategories do: [ :categoryName |
		aStream
			nextChunkPut: 'SystemOrganization addCategory: ', categoryName printString;
			newLine ].
	aStream newLine; newLine
</details>

#### CodePackage>>#packageName

<details>
	<summary>See more</summary>
	
	packageName
	^ packageName
</details>

#### CodePackage>>#hasUnsavedChanges

Might be nil and breaks when a code package window is open and loading packages - Hernan This is not a lazy initialization, the variable is set to non nil value only when certain.


<details>
	<summary>See more</summary>
	
	hasUnsavedChanges

	"Might be nil and breaks when a code package window is open and loading packages - Hernan
	This is not a lazy initialization, the variable is set to non nil value only when certain."
	^hasUnsavedChanges = true
</details>

#### CodePackage>>#write: classes classCommentsOn: aStream

<details>
	<summary>See more</summary>
	
	write: classes classCommentsOn: aStream

	classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				numbered: 0
				moveSource: false
				forClass: class ]]
</details>

#### CodePackage>>#sortedExtensionMethodsDo: aBlock displayingProgress: aString

Include both class and instance methods we define, for classes we don't define.


<details>
	<summary>See more</summary>
	
	sortedExtensionMethodsDo: aBlock displayingProgress: aString
	"Include both class and instance methods we define, for classes we don't define."
	| externalClasses methods |
	externalClasses _ self externalClasses.
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: externalClasses size
		during: [ :barBlock |
			externalClasses withIndexDo: [ :classOrMetaClass :i |
				barBlock value: i.
				methods _ Array streamContents: [ :stream |
					(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
						self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
							stream nextPut: m ]]].
				methods sort: [ :a :b |
					a methodSymbol < b methodSymbol ].
				methods do: aBlock.
				]
			]
</details>

#### CodePackage>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream nextPut: $(.
	self printNameAndVersionOn: aStream.
	aStream nextPut: $)
</details>

#### CodePackage>>#selectorNeededForFilein: aSelector

Answer true if aSelector might be needed for proper filein of the remaing methods.


<details>
	<summary>See more</summary>
	
	selectorNeededForFilein: aSelector
	"Answer true if aSelector might be needed for proper filein of the remaing methods."

	^ Metaclass isScarySelector: aSelector
</details>

#### CodePackage>>#requirementOfMe

<details>
	<summary>See more</summary>
	
	requirementOfMe

	^ self provides requirementOfMe
</details>

#### CodePackage>>#provides

<details>
	<summary>See more</summary>
	
	provides

	^featureSpec provides
</details>

#### CodePackage>>#systemCategories

<details>
	<summary>See more</summary>
	
	systemCategories
	^ SystemOrganization categories select: [:cat | self includesSystemCategory: cat]
</details>

#### CodePackage>>#includesSystemCategory: categoryName

<details>
	<summary>See more</summary>
	
	includesSystemCategory: categoryName
	^ self category: categoryName matches: self systemCategoryPrefix
</details>

#### CodePackage>>#isYourClassExtension: categoryName

<details>
	<summary>See more</summary>
	
	isYourClassExtension: categoryName
	^ categoryName notNil and: [self category: categoryName matches: self methodCategoryPrefix]
</details>

#### CodePackage>>#linesOfCode

An approximate measure of lines of code. Includes comments, but excludes blank lines.


<details>
	<summary>See more</summary>
	
	linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [ :sum :each |
		sum + each compiledMethod linesOfCode]
</details>

#### CodePackage>>#description

<details>
	<summary>See more</summary>
	
	description

	^description
</details>

#### CodePackage>>#hasUnsavedChanges: aBoolean

Not during package install


<details>
	<summary>See more</summary>
	
	hasUnsavedChanges: aBoolean

	"Not during package install"
	(ChangeSet isInstalling: self) ifFalse: [
		hasUnsavedChanges = aBoolean ifFalse: [
			hasUnsavedChanges _ aBoolean.
			hasUnsavedChanges ifTrue: [
				featureSpec provides newRevision ].
			self triggerEvent: #dirtyFlagChanged ]]
</details>

#### CodePackage>>#methodsInCategory: aString ofClass: aClass do: aBlock

<details>
	<summary>See more</summary>
	
	methodsInCategory: aString ofClass: aClass do: aBlock
	((aClass organization listAtCategoryNamed: aString) ifNil: [^self])
		do: [:sel | aBlock value: (self referenceForMethod: sel ofClass: aClass)]
</details>

#### CodePackage>>#coreMethodsForFileinOf: aClass do: aBlock

Evaluate aBlock with the actual method objects in this package. Only enumerate those methods that might be needed for proper filein. such as #compilerClass.


<details>
	<summary>See more</summary>
	
	coreMethodsForFileinOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package.
	Only enumerate those methods that might be needed for proper filein.
	such as #compilerClass."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				(self selectorNeededForFilein: s) ifTrue: [
					aBlock value: (self referenceForMethod: s ofClass: aClass) ]]]]
</details>

#### CodePackage>>#requires

<details>
	<summary>See more</summary>
	
	requires

	^featureSpec requires
</details>

#### CodePackage>>#foreignExtensionMethodsForClass: aClass

<details>
	<summary>See more</summary>
	
	foreignExtensionMethodsForClass: aClass
	^Array streamContents: [ :stream |
		(self foreignExtensionCategoriesForClass: aClass) do: [ :cat |
			(aClass organization listAtCategoryNamed: cat) do: [ :sel |
				stream nextPut: (self referenceForMethod: sel ofClass: aClass) ]]]
</details>

#### CodePackage>>#write: classes methodsForFileinOn: aStream

<details>
	<summary>See more</summary>
	
	write: classes methodsForFileinOn: aStream

	classes
		do: [ :class |
			self 
				writeCoreMethodsForFileinOf: class class on: aStream;
				writeCoreMethodsForFileinOf: class on: aStream ]
		displayingProgress: 'Saving methods...'
</details>

#### CodePackage>>#packageFileName

<details>
	<summary>See more</summary>
	
	packageFileName
	^ packageName, '.pck.st'
</details>

#### CodePackage>>#coreMethodsOf: aClass do: aBlock

Evaluate aBlock with the actual method objects in this package. Leave out all the methods needed for filein.


<details>
	<summary>See more</summary>
	
	coreMethodsOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package.
	
	Leave out all the methods needed for filein."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				(self selectorNeededForFilein: s) ifFalse: [
					aBlock value: (self referenceForMethod: s ofClass: aClass) ]]]]
</details>

#### CodePackage>>#includesAnyCode

<details>
	<summary>See more</summary>
	
	includesAnyCode
	self classesDo: [ :cls |
		^true ].
	^self methods notEmpty
</details>

#### CodePackage>>#removeMethod: aMethodReference

<details>
	<summary>See more</summary>
	
	removeMethod: aMethodReference
</details>

#### CodePackage>>#fullFileName: aString

<details>
	<summary>See more</summary>
	
	fullFileName: aString

	fullFileName _ aString
</details>

#### CodePackage>>#sourceSystem: aString

<details>
	<summary>See more</summary>
	
	sourceSystem: aString

	sourceSystem _ aString
</details>

#### CodePackage>>#methodCategoryPrefix

<details>
	<summary>See more</summary>
	
	methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [
		methodCategoryPrefix _ '*', packageName asLowercase ]
</details>

#### CodePackage>>#fullFileName

<details>
	<summary>See more</summary>
	
	fullFileName

	^fullFileName ifNil: '---Never saved yet'
</details>

#### CodePackage>>#isForeignClassExtension: categoryName

<details>
	<summary>See more</summary>
	
	isForeignClassExtension: categoryName
	categoryName ifNil: [ ^false ].
	^ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]
</details>

#### CodePackage>>#write: classes methodsOn: aStream

<details>
	<summary>See more</summary>
	
	write: classes methodsOn: aStream

	classes
		do: [ :class |
			self 
				writeCoreMethodsOf: class on: aStream;
			 	writeCoreMethodsOf: class class on: aStream ]
		displayingProgress: 'Saving methods...'
</details>

#### CodePackage>>#description: aString

<details>
	<summary>See more</summary>
	
	description: aString

	description = aString
		ifFalse: [
			description _ aString.
			self hasUnsavedChanges: true ]
</details>

#### CodePackage>>#systemCategoriesWithExtensionMethods

<details>
	<summary>See more</summary>
	
	systemCategoriesWithExtensionMethods

	^ SystemOrganization categories select: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) anySatisfy: [ :className | | cls |
			cls _ Smalltalk at: className.
			(self hasAnyExtensionCategoriesForClass: cls) or: [
				self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]]
</details>

#### CodePackage>>#includesClass: aClass

<details>
	<summary>See more</summary>
	
	includesClass: aClass
	^ self includesSystemCategory: aClass category
</details>

#### CodePackage>>#featureSpec: aFeatureSpec

<details>
	<summary>See more</summary>
	
	featureSpec: aFeatureSpec

	featureSpec _ aFeatureSpec
</details>

#### CodePackage>>#category: categoryName matches: prefix

<details>
	<summary>See more</summary>
	
	category: categoryName matches: prefix
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) = $-
</details>

#### CodePackage>>#includesMethodCategory: categoryName ofClass: aClass

<details>
	<summary>See more</summary>
	
	includesMethodCategory: categoryName ofClass: aClass
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]
</details>

#### CodePackage>>#coreMethods

<details>
	<summary>See more</summary>
	
	coreMethods
	^Array streamContents: [ :strm |
		self classesDo: [ :cls |
			strm nextPutAll: (self coreMethodsForClass: cls).
			strm nextPutAll: (self coreMethodsForClass: cls class) ]]
</details>

#### CodePackage>>#printNameAndVersionOn: aStream

<details>
	<summary>See more</summary>
	
	printNameAndVersionOn: aStream
	aStream
		nextPutAll: packageName;
		nextPut: $ .
	featureSpec ifNotNil: [
		featureSpec provides printVersionRevisionOn: aStream ]
</details>

#### CodePackage>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other

	self == other ifTrue: [ ^ true ].
	^ self species == other species and: [ packageName = other packageName ]
</details>

#### CodePackage>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ packageName hash
</details>

#### CodePackage>>#externalClasses

Answer Classes and Metaclasses not defined in self.


<details>
	<summary>See more</summary>
	
	externalClasses
	"Answer Classes and Metaclasses not defined in self."
	| myClasses |
	myClasses _ Set new.
	self classesDo: [ :cls | myClasses add: cls ].
	^ Array streamContents: [ :s |
		ProtoObject
			allSubclassesWithLevelDo: [ :cls :l |
				(myClasses includes: cls) ifFalse: [
					s nextPut: cls; nextPut: cls class ]]
			startingLevel: 1
			sortByCategory: true ]
</details>

#### CodePackage>>#write: classes classDefinitionsOn: aStream

<details>
	<summary>See more</summary>
	
	write: classes classDefinitionsOn: aStream

	classes
		do: [ :class |
			aStream
				nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine;

				nextPut: $!; nextChunkPut: class class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class class definition; newLine;

				newLine ]
		displayingProgress: 'Saving class definitions...'.
</details>

#### CodePackage>>#sourceSystem

<details>
	<summary>See more</summary>
	
	sourceSystem
	^sourceSystem ifNil: ['']
</details>

#### CodePackage>>#writeOnStream: aStream

<details>
	<summary>See more</summary>
	
	writeOnStream: aStream

	| sortedClasses |
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self writeSystemCategoriesOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self
			write: {codePackageClass} classDefinitionsOn: aStream;
			write: {codePackageClass} classCommentsOn: aStream;
			write: {codePackageClass} methodsOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' prePackageInstall'; newLine ].

	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			cls == self class ifFalse: [
				strm nextPut: cls ]]].
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsForFileinOn: aStream;
		write: sortedClasses methodsOn: aStream.

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	self write: sortedClasses initializersOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self write: { codePackageClass } initializersOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' postPackageInstall'; newLine ]
</details>

#### CodePackage>>#extensionCategoriesForClass: aClassOrMetaClass

Pass the class as the argument for instance side. Pass the metaclass as the argument for class side.


<details>
	<summary>See more</summary>
	
	extensionCategoriesForClass: aClassOrMetaClass
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization categories select: [:cat |
		self isYourClassExtension: cat]
</details>

#### CodePackage>>#includesMethodReference: aMethodRef

<details>
	<summary>See more</summary>
	
	includesMethodReference: aMethodRef
	^ self includesMethod: aMethodRef methodSymbol ofClass: aMethodRef actualClass
</details>

#### CodePackage>>#extensionClassNamesIn: aSystemCategory

<details>
	<summary>See more</summary>
	
	extensionClassNamesIn: aSystemCategory

	^ (SystemOrganization listAtCategoryNamed: aSystemCategory) select: [ :className | | cls |
		cls _ Smalltalk at: className.
		(self hasAnyExtensionCategoriesForClass: cls) or: [
			self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]
</details>

#### CodePackage>>#classesInSafeOrderDo: aBlock

<details>
	<summary>See more</summary>
	
	classesInSafeOrderDo: aBlock

	| myClasses myPoolDicts |
	myPoolDicts _ Set new.
	myClasses _ Set new.
	self classesDo: [ :cls |
		(cls inheritsFrom: SharedPool)
			ifTrue: [ myPoolDicts add: cls ]
			ifFalse: [ myClasses add: cls ]].
	Smalltalk hierarchySorted: myPoolDicts do: aBlock.
	Smalltalk hierarchySorted: myClasses do: aBlock
</details>

#### CodePackage>>#selectors

<details>
	<summary>See more</summary>
	
	selectors

	^ self methods collect: [ :ea | ea methodSymbol ]
</details>

#### CodePackage>>#write: classes initializersOn: aStream

Write the call to #initialize method of classes defined in us.


<details>
	<summary>See more</summary>
	
	write: classes initializersOn: aStream
	"Write the call to #initialize method of classes defined in us."

	Smalltalk hierarchySorted: classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [
			aStream nextChunkPut: class name, ' initialize'; newLine ]]
</details>

#### CodePackage>>#systemCategoryPrefix

<details>
	<summary>See more</summary>
	
	systemCategoryPrefix
	^ packageName
</details>

#### CodePackage>>#foreignExtensionCategoriesForClass: aClass

<details>
	<summary>See more</summary>
	
	foreignExtensionCategoriesForClass: aClass
	^ aClass organization categories select: [:cat | self isForeignClassExtension: cat]
</details>

#### CodePackage>>#featureSpec

<details>
	<summary>See more</summary>
	
	featureSpec

	^ featureSpec
</details>

#### CodePackage>>#coreMethodsForClass: aClass

<details>
	<summary>See more</summary>
	
	coreMethodsForClass: aClass
	^ (aClass selectors difference:
		((self foreignExtensionMethodsForClass: aClass) collect: [:r | r methodSymbol]))
			asArray collect: [:sel | self referenceForMethod: sel ofClass: aClass]
</details>

#### CodePackage>>#includesMethod: aSymbol ofClass: aClass

<details>
	<summary>See more</summary>
	
	includesMethod: aSymbol ofClass: aClass
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass organization categoryOfElement: aSymbol)
										ifNil: [' '])
		ofClass: aClass
</details>

#### CodePackage>>#writeMethod: methodReference on: aStream

<details>
	<summary>See more</summary>
	
	writeMethod: methodReference on: aStream
	methodReference actualClass
		printMethodChunk: methodReference methodSymbol
		withPreamble: true
		on: aStream
		moveSource: false
		toFile: 0
</details>

#### CodePackage>>#save

If we can't save, find a new destination directory.


<details>
	<summary>See more</summary>
	
	save
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFileEntry parent exists ifFalse: [
			fullFileName _ nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared, then save to image directory."
	fullFileName ifNil: [
		fullFileName _
			(DirectoryEntry smalltalkImageDirectory // self packageFileName) pathName ].

	fullFileName asFileEntry forceWriteStreamDo: [ :stream |
		stream timeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)
</details>

#### CodePackage>>#extensionMethods

Include both class and instance methods we define, for classes we don't define.


<details>
	<summary>See more</summary>
	
	extensionMethods
	"Include both class and instance methods we define, for classes we don't define."
	^Array streamContents: [ :stream |
		self externalClasses do: [ :classOrMetaClass |
			(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
				self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
					stream nextPut: m ]]]]
</details>

#### CodePackage>>#referenceForMethod: aSymbol ofClass: aClass

<details>
	<summary>See more</summary>
	
	referenceForMethod: aSymbol ofClass: aClass
	^ MethodReference new setStandardClass: aClass methodSymbol: aSymbol
</details>

#### CodePackage>>#writeCoreMethodsOf: aClass on: aStream

<details>
	<summary>See more</summary>
	
	writeCoreMethodsOf: aClass on: aStream

	self coreMethodsOf: aClass do: [ :methodReference |
		methodReference isValid
			ifTrue: [
				self writeMethod: methodReference on: aStream ]]
</details>

#### CodePackage>>#classesDo: aBlock

<details>
	<summary>See more</summary>
	
	classesDo: aBlock
	self systemCategories do: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) do: [ :className |
			aBlock value: (Smalltalk at: className) ]]
</details>

## CodePackageFile

A CodePackageFile represents a file with code for a package, regardless of whether it is installed (as a CodePackage) or not. It supports Cuis' .pck.st.

### Methods
#### CodePackageFile>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	description _ ''.
	featureSpec _ FeatureSpec new
</details>

#### CodePackageFile>>#doIt: chgRec

See senders of #doIt


<details>
	<summary>See more</summary>
	
	doIt: chgRec
	| string |
	string := chgRec string.
	('''Description *'
		match: string) ifTrue:[^self possibleDescription: chgRec].
	super doIt: chgRec
</details>

#### CodePackageFile>>#requires

<details>
	<summary>See more</summary>
	
	requires

	^featureSpec requires
</details>

#### CodePackageFile>>#install

Create, install and answer a (sub)instance of CodePackage Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us.


<details>
	<summary>See more</summary>
	
	install
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS _ ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS _ nil ].
		pckClass _ CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				ee fileInDefinitionAndMetaclass.
				pckClass _ Smalltalk at: ee name ]].
		newCodePackage _ pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.

		fullName asFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Preferences transcriptLogVerbose ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage
</details>

#### CodePackageFile>>#packageName

<details>
	<summary>See more</summary>
	
	packageName
	^packageName
</details>

#### CodePackageFile>>#provides: aFeatureChangeRecord

<details>
	<summary>See more</summary>
	
	provides: aFeatureChangeRecord
	featureSpec provides: aFeatureChangeRecord feature
</details>

#### CodePackageFile>>#possibleDescription: chgRec

<details>
	<summary>See more</summary>
	
	possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) class == String ]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.
</details>

#### CodePackageFile>>#featureSpec

<details>
	<summary>See more</summary>
	
	featureSpec

	^ featureSpec
</details>

#### CodePackageFile>>#provides

<details>
	<summary>See more</summary>
	
	provides

	^featureSpec provides
</details>

#### CodePackageFile>>#classesToRemove

<details>
	<summary>See more</summary>
	
	classesToRemove
	^classesToRemove
</details>

#### CodePackageFile>>#summary

<details>
	<summary>See more</summary>
	
	summary
	^featureSpec printString, String newLineString, super summary
</details>

#### CodePackageFile>>#requires: aFeatureChangeRecord

<details>
	<summary>See more</summary>
	
	requires: aFeatureChangeRecord
	featureSpec requires: aFeatureChangeRecord feature
</details>

#### CodePackageFile>>#buildFileStream: aFileStream packageName: pkName fullName: fullFileName

Just build the CodePackageFile object. Don't install the code.


<details>
	<summary>See more</summary>
	
	buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"This will most likely be updated with the actual data from the file, calling #provides:"
	featureSpec provides: (Feature name: pkName version: 1 revision: 0).
	"Don't register a package!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"
</details>

#### CodePackageFile>>#description

<details>
	<summary>See more</summary>
	
	description
	^String streamContents: [ :s |
		s nextPutAll: 'Code Package File: '.
		s nextPutAll: self fullName; newLine; newLine.
		s nextPutAll: 'Provides: '.
		self provides printDetailsOn: s.
		s newLine.
		(self requires sorted: [:a :b | a name < b name]) do: [ :req |
			s nextPutAll: 'Requires: '.
			req printDetailsOn: s ].
		s newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ]
	]
</details>

#### CodePackageFile>>#methodsToRemove

<details>
	<summary>See more</summary>
	
	methodsToRemove
	^methodsToRemove
</details>

## Feature

A package has a ProvidedFeature which specifies its version and revision. See class FeatureSpec. name -- a symbol naming the feature version -- the integer version/ This increments with each incompatable interface change. revision -- the integer revision which increments each time a package is saved.

### Methods
#### Feature>>#printVersionRevisionOn: aStream

<details>
	<summary>See more</summary>
	
	printVersionRevisionOn: aStream
	version printOn: aStream.
	aStream nextPut: $..
	revision printOn: aStream
</details>

#### Feature>>#satisfies: featureRequirement

Does this provided Feature satisfy the FeatureRequirement?


<details>
	<summary>See more</summary>
	
	satisfies: featureRequirement
	"Does this provided Feature satisfy the FeatureRequirement?"
	
	"Must match name."
	^ (name sameAs: featureRequirement name) and: [

		"If no specific version req, we are done. Ok."
		featureRequirement minVersion isNil or: [

		"If our version is exactly the min req version, we must also satisfy minRevision"
		version = featureRequirement minVersion and: [
			featureRequirement minRevision isNil or: [ revision >= featureRequirement minRevision ]]] or: [
		
		"If we are past min req version, ignore minRevision, but check we are not beyond max req version"
		version > featureRequirement minVersion and: [
			featureRequirement maxVersion isNil or: [ version <= featureRequirement maxVersion ]]]]
</details>

#### Feature>>#requirementOfMe

<details>
	<summary>See more</summary>
	
	requirementOfMe

	^ FeatureRequirement 
		name: self name 
		minVersion: self version 
		minRevision: self revision 
		maxVersion: nil
</details>

#### Feature>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)
</details>

#### Feature>>#version

<details>
	<summary>See more</summary>
	
	version

	^ version

</details>

#### Feature>>#revision

<details>
	<summary>See more</summary>
	
	revision

	^ revision
</details>

#### Feature>>#newRevision

Increment revision number


<details>
	<summary>See more</summary>
	
	newRevision
	"Increment revision number"

	revision _ revision + 1
</details>

#### Feature>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	
	^ name
</details>

#### Feature>>#printDetailsOn: aStream

<details>
	<summary>See more</summary>
	
	printDetailsOn: aStream

	aStream
		nextPutAll: name;
		nextPut: $ .
	self printVersionRevisionOn: aStream
</details>

#### Feature>>#newVersion

Increment major version number.


<details>
	<summary>See more</summary>
	
	newVersion
	"Increment major version number."

	version _ version + 1.
	revision _ 0
</details>

#### Feature>>#name: theName version: theVersion revision: theRevision

<details>
	<summary>See more</summary>
	
	name: theName version: theVersion revision: theRevision

	name     := theName.
	version  := theVersion.
	revision := theRevision
</details>

## FeatureRequirement

A package has a (possibly empty) set of FeatureRequirement's which specifies acceptable version range and minimum revision. See class FeatureSpec. name -- a symbol naming the feature minVersion -- the minimum version required maxVersion -- the maximum version required minRevision -- the minumum revision of the minVersion (assumed zero if minVersion does not equal maxVersion path -- nil or the (cached) pathName of a file containing the package with this feature Usually use the convenience methods: Feature require: 'Sound' Feature require: 'Tests' version: 1.0 Try any of: (FeatureRequirement name: #'Sound') require. "Ask for whatever version of Sound is available" (FeatureRequirement name: #'Sound' version: 1) require. "Ask for a specific version. Any revision will do" (FeatureRequirement name: #'Sound' version: 1 revision: 0) require. "Ask for a specific version.revision" (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) require "Specific version.revision, or any greater version, up to max" This should load packages #Compression and #Sound (FeatureRequirement name: #'Sound') print (FeatureRequirement name: #'Sound' version: 1) print (FeatureRequirement name: #'Sound' version: 1 revision: 12) print (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: nil) print (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: nil maxVersion: 3) print (FeatureRequirement name: #'Sound' minVersion: 1 minRevision: 23 maxVersion: 3) print

### Methods
#### FeatureRequirement>>#minVersion

<details>
	<summary>See more</summary>
	
	minVersion 

	^minVersion 
</details>

#### FeatureRequirement>>#name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

<details>
	<summary>See more</summary>
	
	name: featureName minVersion: minVer minRevision: rev maxVersion: maxVer

	name := featureName asSymbol.
	minVersion := minVer.
	minRevision := rev.
	maxVersion := maxVer
</details>

#### FeatureRequirement>>#isAlreadySatisfied

Answer true if requirement is satisfied by some installed package, or by base system.


<details>
	<summary>See more</summary>
	
	isAlreadySatisfied
	"Answer true if requirement is satisfied by some installed package, or by base system."

	(Feature baseSystemFeature satisfies: self)
		ifTrue: [ ^true ].

	^CodePackage installedPackages anySatisfy: [ :package |
		package provides satisfies: self ]
</details>

#### FeatureRequirement>>#findPackageFileAsReqOf: mainFeatureOrNil

Look in known places for packages providing required feature. Answer wether search was successful.


<details>
	<summary>See more</summary>
	
	findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry _ directory // packageFileName.
		entry exists ifTrue: [
			"Try this one. If success, keep it."
			self pathName: entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			"Nope. Don't keep it."
			self pathName: nil ]].
	^ false
</details>

#### FeatureRequirement>>#require

See if all the transitive closure of requirements can be met and answer the load order if so


<details>
	<summary>See more</summary>
	
	require
	"See if all the transitive closure of requirements can be met and answer the load order if so"

	| packagesToLoad |
	
	"Preflight before load 
	  [1] Build transitive closure as load order where Feature comes 
	       before its required features."
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: nil requiringFeature: nil]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"Transcript show: 'packagesToLoad: '; newLine.
	packagesToLoad do: [ :x |Transcript show: x; newLine ].
	Transcript newLine."
	
	"[2] Check to see that each requirement is actually satisfied"
	[packagesToLoad do: [ :fReq | fReq checkRequirement ]]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].

	"[3] Load required packages before packages that require them"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self isAlreadySatisfied ifFalse: [
		self install ]
</details>

#### FeatureRequirement>>#checkRequirement

Answer if I am satisfied by package found at pathName


<details>
	<summary>See more</summary>
	
	checkRequirement
	"Answer if I am satisfied by package found at pathName"

	| featureSpec |
	featureSpec _ self codePackageFile featureSpec.
	^ featureSpec notNil and: [featureSpec provides satisfies: self ]
</details>

#### FeatureRequirement>>#install

Preconditions have been satisfied. Install the required package.


<details>
	<summary>See more</summary>
	
	install
	"Preconditions have been satisfied.  Install the required package."

	| existing |
	existing _ CodePackage named: self codePackageFile packageName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		self codePackageFile install.
	]
</details>

#### FeatureRequirement>>#requirements

Answer my requirements


<details>
	<summary>See more</summary>
	
	requirements
	"Answer my requirements"

	^self codePackageFile requires
</details>

#### FeatureRequirement>>#= another

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= another
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^false ].
	^name = another name and: [
		minVersion = another minVersion and: [
		minRevision = another minRevision and: [
		maxVersion = another maxVersion ]]]
</details>

#### FeatureRequirement>>#packageFileName

Answer a package name based on feature name


<details>
	<summary>See more</summary>
	
	packageFileName
	"Answer a package name based on feature name"

	^ name asString, '.pck.st'
</details>

#### FeatureRequirement>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^name hash
</details>

#### FeatureRequirement>>#placesToLookForPackagesDo: aBlock

Look inside my own folder


<details>
	<summary>See more</summary>
	
	placesToLookForPackagesDo: aBlock

	| myDir base packagesDirectory |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asFileEntry parent.
		aBlock value: myDir ].
	
	"Look in codePackageFile folder"
	codePackageFile ifNotNil: [
		myDir := codePackageFile fullName asFileEntry parent.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ DirectoryEntry smalltalkImageDirectory.
	aBlock value: base.
	packagesDirectory _ base / 'Packages'.
	aBlock value: packagesDirectory.
	packagesDirectory allRegularDirectoriesDo: aBlock.
	base regularDirectoriesDo: [ :child |
		child = packagesDirectory ifFalse: [
			aBlock value: child.
			child allRegularDirectoriesDo: aBlock]].
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub.
	First try directories including the word Cuis in the name. Then try others."
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifTrue: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifFalse: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].

	"Also look in host OS current directory"
	(base ~= DirectoryEntry currentDirectory and: [base parent ~= DirectoryEntry currentDirectory])
		ifTrue: [
			base _ DirectoryEntry currentDirectory.
			base allRegularDirectoriesDo: aBlock ]
</details>

#### FeatureRequirement>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)
</details>

#### FeatureRequirement>>#satisfyRequirementsAndInstall

Like #require, but install me even if already satisified (i.e. installed)


<details>
	<summary>See more</summary>
	
	satisfyRequirementsAndInstall
	"Like #require, but install me even if already satisified (i.e. installed)"

	| packagesToLoad |
		
	"Preflight before load"
	[packagesToLoad _ self requireUnlessIn: OrderedCollection new main: self requiringFeature: self]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"Check to see that each requirement is actually satisfied"
	[packagesToLoad do: [ :fReq | fReq checkRequirement ]]
		on: FeatureRequirementUnsatisfied  
		do: [ :error | error defaultAction. 
			^self "exit"
		].
	
	"All requirements are satisfied; do the deed"
	packagesToLoad reverseDo: [ :requirement |
		requirement isAlreadySatisfied ifFalse: [
			requirement install ]].
	self install
</details>

#### FeatureRequirement>>#sameNameAs: anotherFeatureRequirement

Answer true if anotherFeatureRequirement has same name as me


<details>
	<summary>See more</summary>
	
	sameNameAs: anotherFeatureRequirement

	"Answer true if anotherFeatureRequirement has same name as me"
	^(self name = anotherFeatureRequirement name)
</details>

#### FeatureRequirement>>#pathName: aPathNameString

<details>
	<summary>See more</summary>
	
	pathName: aPathNameString
	
	pathName _ aPathNameString.
	codePackageFile _ nil
</details>

#### FeatureRequirement>>#addToLoad: toLoad withMyRequirements: requirements

Add self to OrderedCollection 'toLoad' before any requirements I have


<details>
	<summary>See more</summary>
	
	addToLoad: toLoad withMyRequirements: requirements

	"Add self to OrderedCollection 'toLoad' before any requirements I have"
	| myRequirements |
	myRequirements := self requirements.
	toLoad do: [ :featureReq | 
		(myRequirements anySatisfy: [ :req | featureReq sameNameAs: req])
		ifTrue: [
			"Transcript show: ('adding ', self name asString, ' before: ', featureReq name asString); newLine."
			toLoad add: self before: featureReq.
			^ toLoad
		]
	].
	"Transcript show: ('adding ', self name asString); newLine."
	toLoad addLast: self.
	^ toLoad
		
</details>

#### FeatureRequirement>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	
	^ name
</details>

#### FeatureRequirement>>#codePackageFile

<details>
	<summary>See more</summary>
	
	codePackageFile
	codePackageFile isNil ifTrue: [
		codePackageFile _ CodePackageFile onFileEntry: pathName asFileEntry.
		pathName _ nil ].
	^codePackageFile
</details>

#### FeatureRequirement>>#minRevision

<details>
	<summary>See more</summary>
	
	minRevision

	^minRevision 
</details>

#### FeatureRequirement>>#requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil

See if all the requirements can be met and answer the load order


<details>
	<summary>See more</summary>
	
	requireUnlessIn: toLoad main: mainFeatureOrNil requiringFeature: requiringFeatureOrNil
	"See if all the requirements can be met and answer the load order"

	self isAlreadySatisfied ifFalse: [
		(toLoad anySatisfy: [ :featReq | featReq sameNameAs: self]) ifFalse: [
			(self findPackageFileAsReqOf: mainFeatureOrNil)
				ifTrue: [ | otherRequirements |
					otherRequirements := self requirements.
					otherRequirements ifNotNil: [
						otherRequirements do: [ :requires | 
							requires requireUnlessIn: toLoad main: (mainFeatureOrNil ifNil: [self]) requiringFeature: self ]].
					self == mainFeatureOrNil ifFalse: [
						self addToLoad: toLoad withMyRequirements: otherRequirements ].
					]
				ifFalse: [ | failureMessage |
					failureMessage := 'Could not find package supplying: ', self printString, String newLineString,
						'Required by: ', (requiringFeatureOrNil ifNil: [ self ]) printString, String newLineString,
						'For installing: ', (mainFeatureOrNil ifNil: [ self ]) printString, String newLineString.
					FeatureRequirementUnsatisfied signal: failureMessage.
				]]].

	^ toLoad
</details>

#### FeatureRequirement>>#printDetailsOn: aStream

<details>
	<summary>See more</summary>
	
	printDetailsOn: aStream
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersion
		ifNil: [
			aStream nextPutAll: '*.*)'.
			^self ]
		ifNotNil: [ minVersion printOn: aStream ].
	aStream nextPut: $..
	minRevision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ minRevision printOn: aStream ].
	(minRevision notNil or: [ maxVersion isNil or: [maxVersion > minVersion ]]) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersion
			ifNil: [ aStream nextPut: $* ]
			ifNotNil: [ maxVersion printOn: aStream ].
		maxVersion = minVersion
			ifTrue: [ aStream nextPutAll: '.999' ]
			ifFalse: [ aStream nextPutAll: '.*' ]
		]
</details>

#### FeatureRequirement>>#maxVersion

<details>
	<summary>See more</summary>
	
	maxVersion 

	^maxVersion 
</details>

## FeatureRequirementUnsatisfied

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FeatureRequirementUnsatisfied>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The default action taken if the exception is signaled."

	self messageText print.
	PopUpMenu inform: 
		self messageText, 
		String newLineString, String newLineString,
		'You can view loaded Packages and their requirements via',
		String newLineString,
		'        World menu > Open.. > Installed Packages',
		String newLineString
	

</details>

## FeatureSpec

This class just packages two pieces of information useful for packages: provides -- the Feature which our containing package supplies requires -- nil or a Set of Features required by our containing package

### Methods
#### FeatureSpec>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	requires _ Set new
</details>

#### FeatureSpec>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	provides printOn: aStream.
	aStream nextPutAll: ' requires: '.
	requires printOn: aStream.
	aStream nextPut: $)
</details>

#### FeatureSpec>>#provides

<details>
	<summary>See more</summary>
	
	provides

	^ provides
</details>

#### FeatureSpec>>#requirementOfMe

Answer a FeatureRequirement with suitable defaults


<details>
	<summary>See more</summary>
	
	requirementOfMe
	"Answer a FeatureRequirement with suitable defaults"
	
	^ self provides requirementOfMe
</details>

#### FeatureSpec>>#requires: aFeature

<details>
	<summary>See more</summary>
	
	requires: aFeature

	requires add: aFeature
</details>

#### FeatureSpec>>#requires

<details>
	<summary>See more</summary>
	
	requires

	^ requires
</details>

#### FeatureSpec>>#name

Answer the name of the Feature provided


<details>
	<summary>See more</summary>
	
	name
	"Answer the name of the Feature provided"

	^provides name
</details>

#### FeatureSpec>>#provides: providesFeatureSpec requires: setOfRequiresFeatureSpec

<details>
	<summary>See more</summary>
	
	provides: providesFeatureSpec requires: setOfRequiresFeatureSpec

	provides := providesFeatureSpec.
	requires := setOfRequiresFeatureSpec.
</details>

#### FeatureSpec>>#provides: aFeature

<details>
	<summary>See more</summary>
	
	provides: aFeature

	provides _ aFeature
</details>

