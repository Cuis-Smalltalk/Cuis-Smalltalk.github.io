## BasicClassOrganizer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### BasicClassOrganizer>>#classComment: aString

Store the comment, aString, associated with the object that refers to the receiver.


<details>
	<summary>See more</summary>
	
	classComment: aString 
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	aString ifNil: [ ^classComment _ nil ].
	
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [aString size = 0
			ifTrue: [classComment _ nil]
			ifFalse: [
				self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"
</details>

#### BasicClassOrganizer>>#hasClassComment

<details>
	<summary>See more</summary>
	
	hasClassComment

	^classComment notNil and: [^classComment text notNil]
</details>

#### BasicClassOrganizer>>#commentStamp

Answer the comment stamp for the class


<details>
	<summary>See more</summary>
	
	commentStamp
	"Answer the comment stamp for the class"

	^ commentStamp
</details>

#### BasicClassOrganizer>>#setSubject: aClassDescription

<details>
	<summary>See more</summary>
	
	setSubject: aClassDescription
	subject _ aClassDescription
</details>

#### BasicClassOrganizer>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	self hasSubject ifTrue: [
		(self subject class isMeta) ifTrue: [
			dp _ DiskProxy global: self subject name selector: #organization args: #().
			refStrm replace: self with: dp.
			^ dp]].
	^ self	"in desparation"

</details>

#### BasicClassOrganizer>>#commentStamp: aStamp

<details>
	<summary>See more</summary>
	
	commentStamp: aStamp
	commentStamp _ aStamp
</details>

#### BasicClassOrganizer>>#hasSubject

<details>
	<summary>See more</summary>
	
	hasSubject
	^ self subject notNil
</details>

#### BasicClassOrganizer>>#fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex

Copy the class comment to aFileStream. If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file.


<details>
	<summary>See more</summary>
	
	fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileComment _ RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment _ fileComment].
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]]
</details>

#### BasicClassOrganizer>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	super postCopy.

	"I guess not..."
	"subject _ subject copy."

	classComment _ classComment copy.
	commentStamp _ commentStamp copy
</details>

#### BasicClassOrganizer>>#putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass

Store the comment about the class onto file, aFileStream.


<details>
	<summary>See more</summary>
	
	putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!.
		header _ String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp _ '<historical>'].
				commentStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: '0'].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream
				moveSource: moveSource toFile: sourceIndex.
		aFileStream newLine]
</details>

#### BasicClassOrganizer>>#commentRemoteStr

<details>
	<summary>See more</summary>
	
	commentRemoteStr
	^ classComment
</details>

#### BasicClassOrganizer>>#dateCommentLastSubmitted

Answer a Date object indicating when my class comment was last submitted. If there is no date stamp, or one of the old-time <historical> guys, return nil


<details>
	<summary>See more</summary>
	
	dateCommentLastSubmitted
	"Answer a Date object indicating when my class comment was last submitted.  If there is no date stamp, or one of the old-time <historical>  guys, return nil"
	"RecentMessageSet organization dateCommentLastSubmitted"

	| aStamp tokens |
	(aStamp _ self commentStamp) isEmptyOrNil ifTrue: [^ nil].
	tokens _ aStamp findBetweenSubStrs: ' 
'.  "space is expected delimiter, but cr is sometimes seen, though of mysterious provenance"
	^ tokens size > 1
		ifTrue: [
			[tokens second asDate] ifError: nil]
</details>

#### BasicClassOrganizer>>#subject

<details>
	<summary>See more</summary>
	
	subject
	^ subject.
</details>

#### BasicClassOrganizer>>#classComment: aString  stamp: aStamp

Store the comment, aString, associated with the object that refers to the receiver.


<details>
	<summary>See more</summary>
	
	classComment: aString  stamp: aStamp
	"Store the comment, aString, associated with the object that refers to the receiver."

	self commentStamp: aStamp.
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [(aString == nil or: [aString size = 0])
			ifTrue: [classComment _ nil]
			ifFalse:
				[self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"
</details>

#### BasicClassOrganizer>>#hasNoComment

Answer whether the class classified by the receiver has a comment.


<details>
	<summary>See more</summary>
	
	hasNoComment
	"Answer whether the class classified by the receiver has a comment."

	^classComment == nil
</details>

#### BasicClassOrganizer>>#classComment

<details>
	<summary>See more</summary>
	
	classComment
	classComment
		ifNil: [^ ''].
	^ classComment text ifNil: ['']
</details>

## Behavior

My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass).

### Methods
#### Behavior>>#>> selector

Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, create an error notification.


<details>
	<summary>See more</summary>
	
	>> selector 
	"Answer the compiled method associated with the argument, selector (a 
	Symbol), a message selector in the receiver's method dictionary. If the 
	selector is not in the dictionary, create an error notification."

	^self compiledMethodAt: selector 

</details>

#### Behavior>>#isInstanceVariableNamedReferencedInHierarchy: anInstanceVariableName

Returns true if self or any subclass has one or more methods referencing anInstanceVariableName - Hernan


<details>
	<summary>See more</summary>
	
	isInstanceVariableNamedReferencedInHierarchy: anInstanceVariableName

	"Returns true if self or any subclass has one or more methods referencing anInstanceVariableName - Hernan"

	^self withAllSubclasses anySatisfy: [ :aClass | aClass hasReferencesToInstanceVariableNamed: anInstanceVariableName ]
</details>

#### Behavior>>#allSuperclassesDo: aBlock

Evaluate the argument, aBlock, for each of the receiver's superclasses.


<details>
	<summary>See more</summary>
	
	allSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	superclass ifNotNil: [
		aBlock value: superclass.
		superclass allSuperclassesDo: aBlock]
</details>

#### Behavior>>#subclasses

slow implementation since Behavior does not keep track of subclasses


<details>
	<summary>See more</summary>
	
	subclasses
	"slow implementation since Behavior does not keep track of subclasses"
	
	^ self class allInstances select: [:each | each superclass = self ]
</details>

#### Behavior>>#sourceMatchesBytecodeAt: selector

Answers true if the source code at the selector compiles to the bytecode at the selector, and false otherwise. Implemented to detect an error where Monticello did not recompile sources when the class shape changed


<details>
	<summary>See more</summary>
	
	sourceMatchesBytecodeAt: selector
	"Answers true if the source code at the selector compiles to the bytecode at the selector, and false otherwise. Implemented to detect an error where Monticello did not recompile sources when the class shape changed"
	"This code was copied from #recompile:from:, with few changes. Several methods would benefit from a method which turned a selector and class into a CompiledMethod, without  installing it into the methodDictionary"

	| method trailer methodNode |
	method := self compiledMethodAt: selector.
	trailer := method trailer.
	methodNode := self compilerClass new
				compile: (self sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ false].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	^ (methodNode generate: trailer) = method
</details>

#### Behavior>>#unreferencedInstanceVariables

Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of its subclasses Object unreferencedInstanceVariables


<details>
	<summary>See more</summary>
	
	unreferencedInstanceVariables
	"Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of its subclasses

	Object unreferencedInstanceVariables
	"

	^ self instVarNames reject: [ :instanceVariableName | self isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]
		
</details>

#### Behavior>>#methodsWithArgumentOrTemporaryNamed: instVarName

<details>
	<summary>See more</summary>
	
	methodsWithArgumentOrTemporaryNamed: instVarName

	^self methodsSelect: [:aMethod | aMethod hasArgumentOrTemporaryNamed: instVarName ]
</details>

#### Behavior>>#copyOfMethodDictionary

Return a copy of the receiver's method dictionary


<details>
	<summary>See more</summary>
	
	copyOfMethodDictionary
	"Return a copy of the receiver's method dictionary"

	^ self methodDict copy
</details>

#### Behavior>>#new: sizeRequested

Answer an initialized instance of this class with the number of indexable variables specified by the argument, sizeRequested.


<details>
	<summary>See more</summary>
	
	new: sizeRequested 
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  
</details>

#### Behavior>>#isDoubleBytes

Answer whether the receiver's instances indexed 16-bit integer instance variables. Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isDoubleBytes
	"Answer whether the receiver's instances indexed 16-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur and: [ self instSpec = 12 ]
</details>

#### Behavior>>#allInstVarNames

Answer an Array of the names of the receiver's instance variables. The Array ordering is the order in which the variables are stored and accessed by the interpreter.


<details>
	<summary>See more</summary>
	
	allInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter."

	| vars |
	superclass
		ifNil: [vars _ self instVarNames copy]	"Guarantee a copy is answered."
		ifNotNil: [vars _ superclass allInstVarNames , self instVarNames].
	^vars
</details>

#### Behavior>>#canUnderstand: selector

Answer whether the receiver can respond to the message whose selector is the argument. The selector can be in the method dictionary of the receiver's class or any of its superclasses.


<details>
	<summary>See more</summary>
	
	canUnderstand: selector 
	"Answer whether the receiver can respond to the message whose selector 
	is the argument. The selector can be in the method dictionary of the 
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	superclass ifNil: [^false].
	^superclass canUnderstand: selector
</details>

#### Behavior>>#withAllSubAndSuperclassesDo: aBlock

<details>
	<summary>See more</summary>
	
	withAllSubAndSuperclassesDo: aBlock

	self withAllSubclassesDo: aBlock.
	self allSuperclassesDo: aBlock.

</details>

#### Behavior>>#decompilerClass

Answer a decompiler class appropriate for compiled methods of this class.


<details>
	<summary>See more</summary>
	
	decompilerClass
	"Answer a decompiler class appropriate for compiled methods of this class."

	^self compilerClass decompilerClass
</details>

#### Behavior>>#postCopy

<details>
	<summary>See more</summary>
	
	postCopy

	self methodDict: self methodDict copy
</details>

#### Behavior>>#isEphemeronClass

Answer whether the receiver has ephemeral instance variables. The garbage collector will fire (queue for finalization) any ephemeron whose first instance variable is not referenced other than from the transitive closure of references from ephemerons. Hence referring to an object from the first inst var of an ephemeron will cause the ephemeron to fire when the rest of the system does not refer to the object and that object is ready to be collected. Since references from the remaining inst vars of an ephemeron will not prevent the ephemeron from firing, ephemerons may act as the associations in weak dictionaries such that the value (e.g. properties attached to the key) will not prevent firing when the key is no longer referenced other than from ephemerons. Ephemerons can therefore be used to implement instance-based pre-mortem finalization.


<details>
	<summary>See more</summary>
	
	isEphemeronClass
	"Answer whether the receiver has ephemeral instance variables.  The garbage collector will
	 fire (queue for finalization) any ephemeron whose first instance variable is not referenced
	 other than from the transitive closure of references from ephemerons. Hence referring to
	 an object from the first inst var of an ephemeron will cause the ephemeron to fire when
	 the rest of the system does not refer to the object and that object is ready to be collected.
	 Since references from the remaining inst vars of an ephemeron will not prevent the ephemeron
	 from firing, ephemerons may act as the associations in weak dictionaries such that the value
	 (e.g. properties attached to the key) will not prevent firing when the key is no longer referenced
	 other than from ephemerons.  Ephemerons can therefore be used to implement instance-based
	 pre-mortem finalization."
	^self instSpec = 5
</details>

#### Behavior>>#compileAll

<details>
	<summary>See more</summary>
	
	compileAll
	^ self compileAllFrom: self
</details>

#### Behavior>>#instVarNames

Answer an Array of the instance variable names. Behaviors must make up fake local instance variable names because Behaviors have instance variables for the purpose of compiling methods, but these are not named instance variables.


<details>
	<summary>See more</summary>
	
	instVarNames
	"Answer an Array of the instance variable names. Behaviors must make 
	up fake local instance variable names because Behaviors have instance 
	variables for the purpose of compiling methods, but these are not named 
	instance variables."

	| mySize superSize |
	mySize _ self instSize.
	superSize _ 
		superclass
			ifNil: [0]
			ifNotNil: [superclass instSize].
	mySize = superSize ifTrue: [^#()].	
	^(superSize + 1 to: mySize) collect: [:i | 'inst' , i printString]
</details>

#### Behavior>>#recompile: selector

Compile the method associated with selector in the receiver's method dictionary.


<details>
	<summary>See more</summary>
	
	recompile: selector
	"Compile the method associated with selector in the receiver's method dictionary."
	^self recompile: selector from: self
</details>

#### Behavior>>#handleFailingFailingBasicNew

This basicNew gets sent after handleFailingBasicNew: has done a full garbage collection and possibly grown memory. If this basicNew fails then the system really is low on space, so raise the OutOfMemory signal. Primitive. Answer an instance of this class with the number of indexable variables specified by the argument, sizeRequested. Fail if this class is not indexable or if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	handleFailingFailingBasicNew
	"This basicNew gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	"space must be low"
	OutOfMemory signal.
	^self basicNew  "retry if user proceeds"
</details>

#### Behavior>>#lookupSelector: selector

Look up the given selector in my methodDictionary. Return the corresponding method if found. Otherwise chase the superclass chain and try again. Return nil if no method is found.


<details>
	<summary>See more</summary>
	
	lookupSelector: selector
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Return nil if no method is found."
	| lookupClass |
	lookupClass _ self.
	[lookupClass == nil]
		whileFalse: 
			[(lookupClass includesSelector: selector)
				ifTrue: [^ lookupClass compiledMethodAt: selector].
			lookupClass _ lookupClass superclass].
	^ nil
</details>

#### Behavior>>#name

Answer a String that is the name of the receiver.


<details>
	<summary>See more</summary>
	
	name
	"Answer a String that is the name of the receiver."
	^'a subclass of ', superclass name
</details>

#### Behavior>>#isVariable

Answer whether the receiver has indexable variables.


<details>
	<summary>See more</summary>
	
	isVariable
	"Answer whether the receiver has indexable variables."
	^ Smalltalk isSpur
		ifTrue: [ self isVariableSpur ]
		ifFalse: [ self isVariablePreSpur ]
</details>

#### Behavior>>#allCallsOn

Answer a SortedCollection of all the methods that refer to me by name or as part of an association in a global dict.


<details>
	<summary>See more</summary>
	
	allCallsOn
	"Answer a SortedCollection of all the methods that refer to me by name or 
	as part of an association in a global dict."
	"
	^ (Smalltalk
		allCallsOn: (Smalltalk associationAt: self theNonMetaClass name))
		, (Smalltalk allCallsOn: self theNonMetaClass name)
	"

	^ Smalltalk allCallsOn: self theNonMetaClass name
</details>

#### Behavior>>#decompile: selector

Find the compiled code associated with the argument, selector, as a message selector in the receiver's method dictionary and decompile it. Answer the resulting source code as a string. Create an error notification if the selector is not in the receiver's method dictionary.


<details>
	<summary>See more</summary>
	
	decompile: selector 
	"Find the compiled code associated with the argument, selector, as a 
	message selector in the receiver's method dictionary and decompile it. 
	Answer the resulting source code as a string. Create an error notification 
	if the selector is not in the receiver's method dictionary."

	^self decompilerClass new decompile: selector in: self
</details>

#### Behavior>>#addSelectorSilently: selector withMethod: compiledMethod

Add the message selector with the corresponding compiled method to the receiver's method dictionary. Do this without sending system change notifications


<details>
	<summary>See more</summary>
	
	addSelectorSilently: selector withMethod: compiledMethod 
	"Add the message selector with the corresponding compiled method to the 
	receiver's method dictionary.
	Do this without sending system change notifications"

	| oldMethodOrNil |
	oldMethodOrNil _ self lookupSelector: selector.
	self methodDict at: selector put: compiledMethod.

	"Now flush Squeak's method cache, either by selector or by method"
	oldMethodOrNil ifNotNil: [oldMethodOrNil flushCache].
	selector flushCache.
</details>

#### Behavior>>#kindOfSubclassPreSpur

Answer a String that is the keyword that describes the receiver's kind of subclass, either a regular subclass, a variableSubclass, a variableByteSubclass, a variableWordSubclass, or a weakSubclass.


<details>
	<summary>See more</summary>
	
	kindOfSubclassPreSpur
	"Answer a String that is the keyword that describes the receiver's kind 
	of subclass, either a regular subclass, a variableSubclass, a  
	variableByteSubclass, a variableWordSubclass, or a weakSubclass."
	self isWeak
		ifTrue: [^ ' weakSubclass: '].
	^ self isVariable
		ifTrue: [self isBits
				ifTrue: [self isBytes
						ifTrue: [ ' variableByteSubclass: ']
						ifFalse: [ ' variableWordSubclass: ']]
				ifFalse: [ ' variableSubclass: ']]
		ifFalse: [ ' subclass: ']
</details>

#### Behavior>>#compileAllFrom: oldClass

Compile all the methods in the receiver's method dictionary. This validates sourceCode and variable references and forces all methods to use the current bytecode set


<details>
	<summary>See more</summary>
	
	compileAllFrom: oldClass
	"Compile all the methods in the receiver's method dictionary.
	This validates sourceCode and variable references and forces
	all methods to use the current bytecode set"
	"ar 7/10/1999: Use oldClass selectors not self selectors"
	oldClass selectorsDo: [:sel | self recompile: sel from: oldClass]
</details>

#### Behavior>>#isBytes

Answer whether the receiver contains just bits (not pointers). Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isBytes
	"Answer whether the receiver contains just bits (not pointers).
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec >= 16 ]
		ifFalse: [ self instSpec >= 8 ]
</details>

#### Behavior>>#addObsoleteSubclass: aClass

Weakly remember that aClass was a subclass of the receiver and is now obsolete


<details>
	<summary>See more</summary>
	
	addObsoleteSubclass: aClass
	"Weakly remember that aClass was a subclass of the receiver and is now obsolete"
	| obs |
	ObsoleteSubclasses ifNil: [
		ObsoleteSubclasses _ WeakIdentityKeyDictionary new ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[WeakArray new].
	(obs includes: aClass) ifTrue:[^self].
	obs _ obs copyWithout: nil.
	obs _ obs copyWith: aClass.
	ObsoleteSubclasses at: self put: obs.

</details>

#### Behavior>>#allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean

Walk the tree of subclasses, giving the class and its level. Sort nicely.


<details>
	<summary>See more</summary>
	
	allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: aBoolean
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	| aCatIndex bCatIndex |
	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order"
	self subclasses
		sort: [ :a :b |
			aBoolean
				ifTrue: [
					aCatIndex _ SystemOrganization numberOfCategoryOfElement: a name.
					bCatIndex _ SystemOrganization numberOfCategoryOfElement: b name.
					aCatIndex < bCatIndex
						or: [ aCatIndex = bCatIndex and: [ a name <= b name ]]]
				ifFalse: [ a name <= b name ]
			];
		do: [ :subclass |
			subclass
				allSubclassesWithLevelDo: classAndLevelBlock
				startingLevel: level + 1
				sortByCategory: aBoolean ]
</details>

#### Behavior>>#isWords

Answer whether the receiver's instances indexed 32-bit integer instance variables. Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isWords
	"Answer whether the receiver's instances indexed 32-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec = 10 ]
		ifFalse: [ self isBytes not ]
</details>

#### Behavior>>#literalScannedAs: scannedLiteral notifying: requestor

Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote). If scannedLiteral is not an association, answer it. Else, if it is of the form: nil->#NameOfMetaclass answer nil->theMetaclass, if any has that name, else report an error. Else, if it is of the form: #NameOfGlobalVariable->anythiEng answer the global, class, or pool association with that nameE, if any, else add it to Undeclared a answer the new Association.


<details>
	<summary>See more</summary>
	
	literalScannedAs: scannedLiteral notifying: requestor
	"Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
	If scannedLiteral is not an association, answer it.
	Else, if it is of the form:
		nil->#NameOfMetaclass
	answer nil->theMetaclass, if any has that name, else report an error.
	Else, if it is of the form:
		#NameOfGlobalVariable->anythiEng
	answer the global, class, or pool association with that nameE, if any, else
	add it to Undeclared a answer the new Association."

	| key value |
	(scannedLiteral isVariableBinding)
		ifFalse: [^ scannedLiteral].
	key _ scannedLiteral key.
	value _ scannedLiteral value.
	key ifNil: [
		(self bindingOf: value) ifNotNil: [ :assoc |
				 (assoc value isKindOf: Behavior)
					ifTrue: [^ nil->assoc value class]].
		requestor notify: 'No such metaclass'.
		^false].
	(key isMemberOf: Symbol)
		ifTrue: "##<global var name>"
			[(self bindingOf: key) ifNotNil:[ :assoc | ^assoc].
			Undeclared at: key put: nil.
			 ^Undeclared bindingOf: key].
	requestor notify: '## must be followed by a non-local variable name'.
	^false

"	Form literalScannedAs: 14 notifying: nil 14
	Form literalScannedAs: #OneBitForm notiEfying: nil  OneBitForm
	Form literalScannedAs: ##OneBitForm notifying: nil  OneBitForm->a Form
	Form literalScannedAs: ##Form notifying: nil   Form->Form
	Form literalScannedAs: ###Form notifying: nil   nilE->Form class
"
</details>

#### Behavior>>#methodsDo: aBlock

Evaluate aBlock for all the compiled methods in my method dictionary.


<details>
	<summary>See more</summary>
	
	methodsDo: aBlock
	"Evaluate aBlock for all the compiled methods in my method dictionary."

	^ self methodDict valuesDo: aBlock
</details>

#### Behavior>>#allInstancesOrNil

Answer all instances of the receiver, or nil if the primitive fails, which it may be due to being out of memory.


<details>
	<summary>See more</summary>
	
	allInstancesOrNil
	"Answer all instances of the receiver, or nil if the primitive
	 fails, which it may be due to being out of memory."
	<primitive: 177>
	^nil
</details>

#### Behavior>>#binding

<details>
	<summary>See more</summary>
	
	binding
	^ nil -> self
</details>

#### Behavior>>#allSubInstancesDo: aBlock

Evaluate the argument, aBlock, for each of the current instances of the receiver and all its subclasses.


<details>
	<summary>See more</summary>
	
	allSubInstancesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the current instances of the 
	receiver and all its subclasses."

	self allInstancesDo: aBlock.
	self allSubclassesDo: [:sub | sub allInstancesDo: aBlock]
</details>

#### Behavior>>#releaseClassCachedState

Will be called for each class on shutdown or snapshot. All class vars or class instVar vars that can be cheaply recreated lazily on demand, should be nilled. For more expensive stuff to clean and recreate, consider #releaseClassState that is not called on every image save. See implementors for examples


<details>
	<summary>See more</summary>
	
	releaseClassCachedState
	"Will be called for each class on shutdown or snapshot.
	All class vars or class instVar vars that can be cheaply recreated lazily on demand, should be nilled.
	For more expensive stuff to clean and recreate, consider #releaseClassState that is not called on every image save.
	See implementors for examples"
</details>

#### Behavior>>#withAllSuperclasses

Answer an OrderedCollection of the receiver and the receiver's superclasses. See also #allSuperclasses.


<details>
	<summary>See more</summary>
	
	withAllSuperclasses
	"Answer an OrderedCollection of the receiver and the receiver's 
	superclasses. See also #allSuperclasses."

	^self allSuperclasses addFirst: self; yourself
</details>

#### Behavior>>#allowsSubInstVars

Classes that allow instances to change classes among its subclasses will want to override this and return false, so inst vars are not accidentally added to its subclasses.


<details>
	<summary>See more</summary>
	
	allowsSubInstVars
	"Classes that allow instances to change classes among its subclasses will want to override this and return false, so inst vars are not accidentally added to its subclasses."

	^ true
</details>

#### Behavior>>#isDoubleWords

Answer whether the receiver's instances indexed 64-bit integer instance variables. Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isDoubleWords
	"Answer whether the receiver's instances indexed 64-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur and: [ self instSpec = 9 ]
</details>

#### Behavior>>#addMethodsTo: methodsReferencingLiteral thatReferenceTo: aLiteral special: specialFlag byte: specialByte

<details>
	<summary>See more</summary>
	
	addMethodsTo: methodsReferencingLiteral thatReferenceTo: aLiteral special: specialFlag byte: specialByte
	
	| selectors |
	
	selectors _ self whichSelectorsReferTo: aLiteral special: specialFlag byte: specialByte.
	selectors do: [ :sel | methodsReferencingLiteral add: (MethodReference class: self selector: sel) ]
</details>

#### Behavior>>#hasReferencesToInstanceVariableNamed: anInstanceVariableName

Returns true if only self has one or more methods referencing anInstanceVariableName - Hernan


<details>
	<summary>See more</summary>
	
	hasReferencesToInstanceVariableNamed: anInstanceVariableName

	"Returns true if only self has one or more methods referencing anInstanceVariableName - Hernan"

	^(self whichSelectorsAccess: anInstanceVariableName) notEmpty
</details>

#### Behavior>>#classDepth

<details>
	<summary>See more</summary>
	
	classDepth

	superclass ifNil: [^ 1].
	^ superclass classDepth + 1
</details>

#### Behavior>>#basicNew

Primitive. Answer an instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable. Essential. See Object documentation whatIsAPrimitive. If the primitive fails because space is low then the scavenger will run before the method is activated. Check that space was low and retry via handleFailingBasicNew if so.


<details>
	<summary>See more</summary>
	
	basicNew
	"Primitive. Answer an instance of the receiver (which is a class) with no 
	 indexable variables. Fail if the class is indexable. Essential. See Object 
	 documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run
	 before the method is activated.  Check that space was low and retry
	 via handleFailingBasicNew if so."

	<primitive: 70 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingBasicNew].
	self isVariable ifTrue: [^self basicNew: 0].
	self primitiveFailed
</details>

#### Behavior>>#initClassCachedState

Will be called for each class on startup. See implementors for examples Can be used for eagerly initializing stuff that was cleared with releaseClassCachedState


<details>
	<summary>See more</summary>
	
	initClassCachedState
	"Will be called for each class on startup.
	See implementors for examples

	Can be used for eagerly initializing stuff that was cleared with releaseClassCachedState"
</details>

#### Behavior>>#zapAllMethods

Remove all methods in this class which is assumed to be obsolete


<details>
	<summary>See more</summary>
	
	zapAllMethods
	"Remove all methods in this class which is assumed to be obsolete"

	methodDict _ MethodDictionary new.
	self class isMeta ifTrue: [self class zapAllMethods]
</details>

#### Behavior>>#isWeak

Answer whether the receiver has contains weak references.


<details>
	<summary>See more</summary>
	
	isWeak
	"Answer whether the receiver has contains weak references."
	^ self instSpec = 4
</details>

#### Behavior>>#changeRecordsAt: selector

Return a list of ChangeRecords for all versions of the method at selector. Source code can be retrieved by sending string to any one. Return nil if the method is absent.


<details>
	<summary>See more</summary>
	
	changeRecordsAt: selector
	"Return a list of ChangeRecords for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	"(Pen changeRecordsAt: #go:) collect: [:cRec | cRec string]"
	| aList |
	aList _ VersionsBrowser new
			scanVersionsOf: (self compiledMethodAt: selector ifAbsent: [^ nil])
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector.
	^ aList ifNotNil: [aList changeList]
</details>

#### Behavior>>#allSharedPools

Answer a Set of the names of the pools (Dictionaries or SharedPool subclasses) that the receiver and the receiver's ancestors share.


<details>
	<summary>See more</summary>
	
	allSharedPools
	"Answer a Set of the names of the pools (Dictionaries or SharedPool subclasses) that the receiver and the receiver's ancestors share."

	^superclass allSharedPools
</details>

#### Behavior>>#format

Answer an Integer that encodes the kinds and numbers of variables of instances of the receiver.


<details>
	<summary>See more</summary>
	
	format
	"Answer an Integer that encodes the kinds and numbers of variables of 
	instances of the receiver."

	^format
</details>

#### Behavior>>#sourceCodeTemplate

Answer an expression to be edited and evaluated in order to define methods in this class. Modified to be parseable for Shour


<details>
	<summary>See more</summary>
	
	sourceCodeTemplate
	"Answer an expression to be edited and evaluated in order to define 
	methods in this class.
	Modified to be parseable for Shour"

	^'messageSelectorAndArgumentNames
	"comment stating purpose of message"

	| temporary variable names |
	statements'
</details>

#### Behavior>>#supermostPrecodeCommentFor: selector

Answer a string representing the precode comment in the most distant superclass's implementation of the selector. Return nil if none found.


<details>
	<summary>See more</summary>
	
	supermostPrecodeCommentFor: selector 
	"Answer a string representing the precode comment in the most distant 
	superclass's implementation of the selector. Return nil if none found."
	| aSuper superComment |
	(self == Behavior
			or: [superclass == nil
					or: [(aSuper _ superclass whichClassIncludesSelector: selector) == nil]])
		ifFalse: ["There is a super implementor"
			superComment _ aSuper supermostPrecodeCommentFor: selector].
	^ superComment
		ifNil: [self firstPrecodeCommentFor: selector
			"ActorState supermostPrecodeCommentFor: #printOn:"]
</details>

#### Behavior>>#classBindingOf: varName

Answer the binding of some variable resolved in the scope of the receiver's class


<details>
	<summary>See more</summary>
	
	classBindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver's class"
	^self bindingOf: varName
</details>

#### Behavior>>#compile: code notifying: requestor

Compile the argument, code, as source code in the context of the receiver and install the result in the receiver's method dictionary. The second argument, requestor, is to be notified if an error occurs. The argument code is either a string or an object that converts to a string or a PositionableStream. This method also saves the source code.


<details>
	<summary>See more</summary>
	
	compile: code notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode |
	methodAndNode _ self
		basicCompile: code "a Text"
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	methodAndNode method putSource: code fromParseNode: methodAndNode node inFile: 2
			withPreamble: [:f | f newLine; nextPut: $!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: methodAndNode selector withMethod: methodAndNode method.
	^ methodAndNode selector
</details>

#### Behavior>>#allInstancesDo: aBlock

Evaluate aBlock with each of the current instances of the receiver.


<details>
	<summary>See more</summary>
	
	allInstancesDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	| instances inst next |
	instances := self allInstancesOrNil.
	instances ifNotNil:
		[instances do: aBlock.
		 ^self].
	"allInstancesOrNil can fail because memory is low.  If so, fall back on the old
	 enumeration code.  Because aBlock might change the class of inst (for example,
	 using become:), it is essential to compute next before aBlock value: inst."
	inst := self someInstance.
	[inst == nil] whileFalse:
		[next := inst nextInstance.
		 aBlock value: inst.
		 inst := next]
</details>

#### Behavior>>#highestClassImplementing: aSelector ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	highestClassImplementing: aSelector ifNone: aNoneBlock

	| highestImplementorClass |

	self withAllSuperclassesDo: [ :aBehavior | (aBehavior includesSelector: aSelector) ifTrue: [ highestImplementorClass := aBehavior ]].
	
	^ highestImplementorClass ifNil: aNoneBlock 
</details>

#### Behavior>>#removeObsoleteSubclass: aClass

Remove aClass from the weakly remembered obsolete subclasses


<details>
	<summary>See more</summary>
	
	removeObsoleteSubclass: aClass
	"Remove aClass from the weakly remembered obsolete subclasses"
	| obs |
	ObsoleteSubclasses ifNil: [
		^ self ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[^ self].
	(obs includes: aClass) ifFalse:[^self].
	obs _ obs copyWithout: aClass.
	obs _ obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ObsoleteSubclasses removeKey: self ifAbsent: nil]
		ifFalse: [ObsoleteSubclasses at: self put: obs]
</details>

#### Behavior>>#isFixed

Answer whether the receiver does not have a variable (indexable) part.


<details>
	<summary>See more</summary>
	
	isFixed
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not
</details>

#### Behavior>>#sourceCodeAt: selector

<details>
	<summary>See more</summary>
	
	sourceCodeAt: selector

	^ (self methodDict at: selector) getSourceFor: selector in: self
</details>

#### Behavior>>#withAllSuperAndSubclassesDoGently: aBlock

<details>
	<summary>See more</summary>
	
	withAllSuperAndSubclassesDoGently: aBlock
	self allSuperclassesDo: aBlock.
	aBlock value: self.
	self allSubclassesDoGently: aBlock
</details>

#### Behavior>>#compile: code

Compile the argument, code, as source code in the context of the receiver. Create an error notification if the code can not be compiled. The argument is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string.


<details>
	<summary>See more</summary>
	
	compile: code 
	"Compile the argument, code, as source code in the context of the 
	receiver. Create an error notification if the code can not be compiled. 
	The argument is either a string or an object that converts to a string or a 
	PositionableStream on an object that converts to a string."

	^self compile: code notifying: nil
</details>

#### Behavior>>#methodDict

<details>
	<summary>See more</summary>
	
	methodDict
	methodDict ifNil: [self recoverFromMDFaultWithTrace].
	^ methodDict
</details>

#### Behavior>>#isMeta

<details>
	<summary>See more</summary>
	
	isMeta
	^ false
</details>

#### Behavior>>#definesInstanceVariableNamed: anInstanceVariableName

<details>
	<summary>See more</summary>
	
	definesInstanceVariableNamed: anInstanceVariableName

	^self instVarNames includes: anInstanceVariableName
</details>

#### Behavior>>#methodsSelect: aCondition

<details>
	<summary>See more</summary>
	
	methodsSelect: aCondition

	^ self methodDict valuesSelect: aCondition
</details>

#### Behavior>>#allSuperclassesPreviousTo: aSuperclass

<details>
	<summary>See more</summary>
	
	allSuperclassesPreviousTo: aSuperclass
	
	| superclasses |
	
	superclasses _ self allSuperclassesUpTo: aSuperclass.
	
	^ superclasses allButLast
</details>

#### Behavior>>#isBehavior

Return true if the receiver is a behavior


<details>
	<summary>See more</summary>
	
	isBehavior
	"Return true if the receiver is a behavior"
	^true
</details>

#### Behavior>>#precodeCommentOrInheritedCommentFor: selector

Answer a string representing the first comment in the method associated with selector, considering however only comments that occur before the beginning of the actual code. If the version recorded in the receiver is uncommented, look up the inheritance chain. Return nil if none found.


<details>
	<summary>See more</summary>
	
	precodeCommentOrInheritedCommentFor: selector 
	"Answer a string representing the first comment in the method associated 
	with selector, considering however only comments that occur before the 
	beginning of the actual code. If the version recorded in the receiver is 
	uncommented, look up the inheritance chain. Return nil if none found."
	| aSuper aComment |
	^ (aComment _ self firstPrecodeCommentFor: selector) isEmptyOrNil
		ifTrue: [(self == Behavior
					or: [superclass == nil
							or: [(aSuper _ superclass whichClassIncludesSelector: selector) == nil]])
				ifFalse: [aSuper precodeCommentOrInheritedCommentFor: selector]
			"ActorState precodeCommentOrInheritedCommentFor: #printOn:"]
		ifFalse: [aComment]
</details>

#### Behavior>>#shouldNotBeRedefined

Return true if the receiver should not be redefined. The assumption is that compact classes, classes in Smalltalk specialObjects and Behaviors should not be redefined


<details>
	<summary>See more</summary>
	
	shouldNotBeRedefined
	"Return true if the receiver should not be redefined.
	The assumption is that compact classes,
	classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"
	^(Smalltalk compactClassesArrayIncludes: self)
		or:[(Smalltalk specialObjectsArray identityIncludes: self)
			or:[self isKindOf: self]]
</details>

#### Behavior>>#withAllSubclasses

Answer an OrderedCollection with the receiver, the receiver's descendents, and the receiver's descendents' subclasses.


<details>
	<summary>See more</summary>
	
	withAllSubclasses
	"Answer an OrderedCollection with the receiver, the receiver's descendents, and the  
	receiver's descendents' subclasses."

	^self allSubclasses addFirst: self; yourself
</details>

#### Behavior>>#startUp: isARealStartup

This message is sent to registered classes, with isARealStartup = true when the system is coming up, and with isARealStartup = false after a snapshot (image save, no quit). Classes caring about the difference should reimplement this method.


<details>
	<summary>See more</summary>
	
	startUp: isARealStartup
	"This message is sent to registered classes, with isARealStartup = true when the system is coming up,
	and with isARealStartup = false after a snapshot (image save, no quit).
	Classes caring about the difference should reimplement this method."

	^ self startUp
</details>

#### Behavior>>#recompileNonResidentMethod: method atSelector: selector from: oldClass

Recompile the method supplied in the context of this class.


<details>
	<summary>See more</summary>
	
	recompileNonResidentMethod: method atSelector: selector from: oldClass
	"Recompile the method supplied in the context of this class."

	| trailer methodNode |
	trailer _ method trailer.
	methodNode _ self compilerClass new
			compile: (method getSourceFor: selector in: oldClass)
			in: self
			notifying: nil
			ifFail: ["We're in deep doo-doo if this fails (syntax error).
				Presumably the user will correct something and proceed,
				thus installing the result in this methodDict.  We must
				retrieve that new method, and restore the original (or remove)
				and then return the method we retrieved."
				^ self error: 'see comment'].
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	^ methodNode generate: trailer

</details>

#### Behavior>>#classVarNames

Answer a Set of the receiver's class variable names.


<details>
	<summary>See more</summary>
	
	classVarNames
	"Answer a Set of the receiver's class variable names."

	^Set new
</details>

#### Behavior>>#withSuperclassThatIncludesSelector: aSelector do: aFoundClosure ifNone: aNoneClosure

<details>
	<summary>See more</summary>
	
	withSuperclassThatIncludesSelector: aSelector do: aFoundClosure ifNone: aNoneClosure

	^superclass
		ifNil: aNoneClosure
		ifNotNil: [
			(superclass whichClassIncludesSelector: aSelector)
				ifNil: aNoneClosure
				ifNotNil: aFoundClosure ]
</details>

#### Behavior>>#canZapMethodDictionary

Return true if it is safe to zap the method dictionary on #obsolete


<details>
	<summary>See more</summary>
	
	canZapMethodDictionary
	"Return true if it is safe to zap the method dictionary on #obsolete"
	^true
</details>

#### Behavior>>#addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte

<details>
	<summary>See more</summary>
	
	addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	
	self withAllSuperAndSubclassesDoGently: [ :class |
		class addMethodsTo: aSet thatReferenceTo: aSymbol special: special byte: byte ] 
		
</details>

#### Behavior>>#recompileChanges

Compile all the methods that are in the changes file. This validates sourceCode and variable references and forces methods to use the current bytecode set


<details>
	<summary>See more</summary>
	
	recompileChanges
	"Compile all the methods that are in the changes file.
	This validates sourceCode and variable references and forces
	methods to use the current bytecode set"

	self selectorsDo:
		[:sel | (self compiledMethodAt: sel) fileIndex > 1 ifTrue:
			[self recompile: sel from: self]]
</details>

#### Behavior>>#whichSelectorsAccess: instVarName

Answer a collection (an Array) of selectors whose methods access the argument, instVarName, as a named instance variable.


<details>
	<summary>See more</summary>
	
	whichSelectorsAccess: instVarName 
	"Answer a collection (an Array) of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	| instVarIndex |
	instVarIndex _ self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^ (self methodDict keys select: [ :sel | 
		((self methodDict at: sel)
			readsField: instVarIndex)
			or: [(self methodDict at: sel) writesField: instVarIndex]]) asSet

	"Point whichSelectorsAccess: 'x'."
</details>

#### Behavior>>#isImmediateClass

Answer whether the receiver has immediate instances. Immediate instances store their value in their object pointer, not in an object body. Hence immediates take no space and are immutable. The immediates are distinguished by tag bits in the pointer. They include SmallIntegers and Characters. Hence in the 32-bit system SmallIntegers are 31-bit signed integers and Characters are 30-bit unsigned character codes.


<details>
	<summary>See more</summary>
	
	isImmediateClass
	"Answer whether the receiver has immediate instances.  Immediate instances
	 store their value in their object pointer, not in an object body.  Hence immediates
	 take no space and are immutable.  The immediates are distinguished by tag bits
	 in the pointer. They include SmallIntegers and Characters.  Hence in the 32-bit
	 system SmallIntegers are 31-bit signed integers and Characters are 30-bit
	 unsigned character codes."
	^self instSpec = 7
</details>

#### Behavior>>#adoptInstance: anInstance

Change the class of anInstance to me. Primitive (found in Cog and new VMs) follows the same rules as primitiveChangeClassTo:, but returns the class rather than the modified instance


<details>
	<summary>See more</summary>
	
	adoptInstance: anInstance
	"Change the class of anInstance to me.
	Primitive (found in Cog and new VMs)  follows the same rules as primitiveChangeClassTo:, but returns the class rather than the modified instance"

	<primitive: 160 error: ec>
	anInstance primitiveChangeClassTo: self basicNew.
	^self
</details>

#### Behavior>>#parserClass

Answer a parser class to use for parsing method headers.


<details>
	<summary>See more</summary>
	
	parserClass
	"Answer a parser class to use for parsing method headers."

	^self compilerClass parserClass
</details>

#### Behavior>>#initializedInstance

Answer an instance of the receiver which in some sense is initialized. In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu. Return nil if the receiver is reluctant for some reason to return such a thing


<details>
	<summary>See more</summary>
	
	initializedInstance
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the 'new morph' menu.   Return nil if the receiver is reluctant for some reason to return such a thing"

	^ self new
</details>

#### Behavior>>#methodDictionary: aDictionary

Store the argument, aDictionary, as the method dictionary of the receiver.


<details>
	<summary>See more</summary>
	
	methodDictionary: aDictionary 
	"Store the argument, aDictionary, as the method dictionary of the 
	receiver."
	methodDict _ aDictionary.
</details>

#### Behavior>>#allLocalCallsOn: aSymbol

Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy.


<details>
	<summary>See more</summary>
	
	allLocalCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."

	| aSet special byte cls |
	
	aSet _ Set new.
	cls _ self theNonMetaClass.
	special _ Smalltalk hasSpecialSelector: aSymbol ifTrueSetByte: [ :b | byte _ b ].
	
	cls addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	cls class addTo: aSet methodsThatReferenceInHierarchyTo: aSymbol special: special byte: byte.
	
	^aSet
</details>

#### Behavior>>#rejectSelectorsFrom: selectors thatReferenceTo: aLiteral byte: specialByte

For special selectors, look for the literal in the source code. Otherwise, for example, searching for senders of #== will include senders of #ifNil. Except for #at:put:, because it has two arguments and won't find it in the source code like that.


<details>
	<summary>See more</summary>
	
	rejectSelectorsFrom: selectors thatReferenceTo: aLiteral byte: specialByte 

	"For special selectors, look for the literal in the source code.
	Otherwise, for example, searching for senders of #== will include senders of #ifNil.
	Except for #at:put:, because it has two arguments and won't find it in the source code like that."

	^ (specialByte isNil or: [ aLiteral = #at:put: ]) 
		ifTrue: [ selectors ]
		ifFalse: [ selectors select: [ :sel | ((self sourceCodeAt: sel) findString: aLiteral) > 0]]
</details>

#### Behavior>>#isObsolete

Return true if the receiver is obsolete.


<details>
	<summary>See more</summary>
	
	isObsolete
	"Return true if the receiver is obsolete."
	^self instanceCount = 0
</details>

#### Behavior>>#indexIfCompact

If these 5 bits are non-zero, then instances of this class will be compact. It is crucial that there be an entry in Smalltalk compactClassesArray for any class so optimized. See the msgs becomeCompact and becomeUncompact.


<details>
	<summary>See more</summary>
	
	indexIfCompact
	"If these 5 bits are non-zero, then instances of this class
	will be compact.  It is crucial that there be an entry in
	Smalltalk compactClassesArray for any class so optimized.
	See the msgs becomeCompact and becomeUncompact."

	"Spur doesn't allow Compact Classes"
	Smalltalk isSpur ifTrue: [ ^ 0 ].

	^ (format bitShift: -11) bitAnd: 16r1F
"
Smalltalk compactClassesArray withIndexDo: 
	[:c :i | c == nil ifFalse:
		[c indexIfCompact = i ifFalse: [self halt]]]
"
</details>

#### Behavior>>#whichSelectorsReferTo: literal

Answer a Set of selectors whose methods access the argument as a literal.


<details>
	<summary>See more</summary>
	
	whichSelectorsReferTo: literal 
	"Answer a Set of selectors whose methods access the argument as a literal."

	| special byte |
	special _ Smalltalk hasSpecialSelector: literal ifTrueSetByte: [:b | byte _ b].
	^self whichSelectorsReferTo: literal special: special byte: byte

	"Rectangle whichSelectorsReferTo: #+."
</details>

#### Behavior>>#basicRemoveSelector: selector

Assuming that the argument, selector (a Symbol), is a message selector in my method dictionary, remove it and its method.


<details>
	<summary>See more</summary>
	
	basicRemoveSelector: selector 
	"Assuming that the argument, selector (a Symbol), is a message selector 
	in my method dictionary, remove it and its method."

	| oldMethod |
	oldMethod _ self methodDict at: selector ifAbsent: [^ self].
	self methodDict removeKey: selector.

	"Now flush Squeak's method cache, either by selector or by method"
	oldMethod flushCache.
	selector flushCache.
</details>

#### Behavior>>#standardMethodHeaderFor: aSelector

<details>
	<summary>See more</summary>
	
	standardMethodHeaderFor: aSelector
	| args |
	args _ (1 to: aSelector numArgs)	collect:[:i| 'arg', i printString].
	args size = 0 ifTrue:[^aSelector asString].
	args size = 1 ifTrue:[^aSelector,' arg1'].
	^String streamContents:[:s|
		(aSelector findTokens: ':') with: args do: [ :tok :arg |
			s nextPutAll: tok; nextPutAll:': '; nextPutAll: arg; nextPutAll:' '.
		].
	].

</details>

#### Behavior>>#instVarNamesAndOffsetsDo: aBinaryBlock

This is part of the interface between the compiler and a class's instance or field names. The class should enumerate aBinaryBlock with the instance variable name strings and their integer offsets. The order is important. Names evaluated later will override the same names occurring earlier.


<details>
	<summary>See more</summary>
	
	instVarNamesAndOffsetsDo: aBinaryBlock
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	"Nothing to do here; ClassDescription introduces named instance variables"
	^self
</details>

#### Behavior>>#allUnsentMessages

Answer an array of all the messages defined by the receiver that are not sent anywhere in the system. 5/8/96 sw


<details>
	<summary>See more</summary>
	
	allUnsentMessages
	"Answer an array of all the messages defined by the receiver that are not sent anywhere in the system.  5/8/96 sw"

	^ Smalltalk allUnSentMessagesIn: self selectors
</details>

#### Behavior>>#allSubclassesDo: aBlock

Evaluate the argument, aBlock, for each of the receiver's subclasses.


<details>
	<summary>See more</summary>
	
	allSubclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDo: 
		[:cl | 
		aBlock value: cl.
		cl allSubclassesDo: aBlock]
</details>

#### Behavior>>#handleFailingFailingBasicNew: sizeRequested

This basicNew: gets sent after handleFailingBasicNew: has done a full garbage collection and possibly grown memory. If this basicNew: fails then the system really is low on space, so raise the OutOfMemory signal. Primitive. Answer an instance of this class with the number of indexable variables specified by the argument, sizeRequested. Fail if this class is not indexable or if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	handleFailingFailingBasicNew: sizeRequested
	"This basicNew: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails
	 then the system really is low on space, so raise the OutOfMemory signal.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."
	"space must be low."
	<primitive: 71>
	(sizeRequested isInteger and: [ sizeRequested > 0 ])
		ifTrue: [ OutOfMemory signal ]
		ifFalse: [ self error: 'sizeRequested must be a positive integer' ].
	^ self basicNew: sizeRequested"retry if user proceeds".
</details>

#### Behavior>>#includesBehavior: aClass

<details>
	<summary>See more</summary>
	
	includesBehavior: aClass
	^self == aClass or:[self inheritsFrom: aClass]
</details>

#### Behavior>>#bindingOf: varName

Answer the binding of some variable resolved in the scope of the receiver


<details>
	<summary>See more</summary>
	
	bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	^superclass bindingOf: varName
</details>

#### Behavior>>#storeLiteral: aCodeLiteral on: aStream

Store aCodeLiteral on aStream, changing an Association to ##GlobalName or ###MetaclassSoleInstanceName format if appropriate


<details>
	<summary>See more</summary>
	
	storeLiteral: aCodeLiteral on: aStream
	"Store aCodeLiteral on aStream, changing an Association to ##GlobalName
	 or ###MetaclassSoleInstanceName format if appropriate"
	| key value |
	(aCodeLiteral isVariableBinding)
		ifFalse:
			[aCodeLiteral storeOn: aStream.
			 ^self].
	key _ aCodeLiteral key.
	(key isNil and: [(value _ aCodeLiteral value) isMemberOf: Metaclass])
		ifTrue:
			[aStream nextPutAll: '###'; nextPutAll: value soleInstance name.
			 ^self].
	((key isMemberOf: Symbol) and: [(self bindingOf: key) notNil])
		ifTrue:
			[aStream nextPutAll: '##'; nextPutAll: key.
			 ^self].
	aCodeLiteral storeOn: aStream
</details>

#### Behavior>>#selectorsAndMethodsDo: aBlock

Evaluate selectorBlock for all the message selectors in my method dictionary.


<details>
	<summary>See more</summary>
	
	selectorsAndMethodsDo: aBlock
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^ self methodDict keysAndValuesDo: aBlock
</details>

#### Behavior>>#addReferencesOf: anInstVarName at: anInstVarIndex to: references

<details>
	<summary>See more</summary>
	
	addReferencesOf: anInstVarName at: anInstVarIndex to: references

	| reference |
	
	self methodsDo: [ :aMethod |
		(aMethod accessorDescriptionOf: anInstVarName at: anInstVarIndex) ifNotEmpty: [ :description |
			reference := MethodReference method: aMethod.
			reference prefixStringVersionWith: '[',description, '] - '.
			references add: reference ]].
	
</details>

#### Behavior>>#instanceCount

Answer the number of instances of the receiver that are currently in use.


<details>
	<summary>See more</summary>
	
	instanceCount
	"Answer the number of instances of the receiver that are currently in 
	use."

	| count |
	count _ 0.
	self allInstancesDo: [:x | count _ count + 1].
	^count
</details>

#### Behavior>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Refer to the comment in Object|printOn:." 

	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream
</details>

#### Behavior>>#removeAllObsoleteSubclasses

Remove all the obsolete subclasses of the receiver


<details>
	<summary>See more</summary>
	
	removeAllObsoleteSubclasses
	"Remove all the obsolete subclasses of the receiver"
	ObsoleteSubclasses ifNotNil: [
		ObsoleteSubclasses finalizeValues. "clean up if need be"
		ObsoleteSubclasses removeKey: self ifAbsent: nil ]
</details>

#### Behavior>>#byteSizeOfInstance

Only for Spur!


<details>
	<summary>See more</summary>
	
	byteSizeOfInstance
	"Only for Spur!"
	"Answer the total memory size of an instance of the receiver."

	<primitive: 181 error: ec>
	self isVariable ifTrue:
		[^self byteSizeOfInstanceOfSize: 0].
	self primitiveFailed
</details>

#### Behavior>>#thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte

Answer a set of selectors whose methods access the argument as a literal. Dives into the compact literal notation, making it slow but thorough


<details>
	<summary>See more</summary>
	
	thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a 
	literal. Dives into the compact literal notation, making it slow but 
	thorough "

	| who |
	who _ Set new.
	self selectorsAndMethodsDo:
		[:sel :method |
		((method hasLiteralThorough: literal) or: [specialFlag and: [method scanFor: specialByte]])
			ifTrue:
				[((literal isVariableBinding) not
					or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
					or: [(method indexOfLiteral: literal) ~= 0]])
						ifTrue: [who add: sel]]].
	^ who
</details>

#### Behavior>>#compress

Compact the method dictionary of the receiver.


<details>
	<summary>See more</summary>
	
	compress
	"Compact the method dictionary of the receiver."

	self methodDict rehash
</details>

#### Behavior>>#startUp

This message is sent to registered classes when the system is coming up, or after an image save.


<details>
	<summary>See more</summary>
	
	startUp
	"This message is sent to registered classes when the system is coming up, or after an image save."
</details>

#### Behavior>>#withAllSuperclassesDo: aBlock

Evaluate the argument, aBlock, for each of the receiver's superclasses.


<details>
	<summary>See more</summary>
	
	withAllSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."
	aBlock value: self.
	superclass
		ifNotNil: [superclass withAllSuperclassesDo: aBlock]
</details>

#### Behavior>>#allSubclasses

A breadth-first iterative algorithm. Significantly faster than a recursive, depth-first implementation.


<details>
	<summary>See more</summary>
	
	allSubclasses
	"A breadth-first iterative algorithm. Significantly faster than a recursive, depth-first implementation."

	| answer finger fingerLimit each |
	answer := OrderedCollection new.
	self subclassesDo: [:some | answer add: some].
	finger := 0.
	fingerLimit := answer size.
	[finger < fingerLimit] whileTrue:
		[
			finger + 1 to: fingerLimit do:
				[:index |
					each := answer at: index.
					each subclassesDo: [:some | answer add: some]
				].
			finger := fingerLimit.
			fingerLimit := answer size.
		].
	^answer
</details>

#### Behavior>>#withTestCaseClassDo: aFoundTestCaseClassBlock ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	withTestCaseClassDo: aFoundTestCaseClassBlock ifNone: aNoneBlock 
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass _ self testCaseClass.
	
	^potentialTestCaseClass ifNil: aNoneBlock ifNotNil: aFoundTestCaseClassBlock 
	
</details>

#### Behavior>>#withAllSuperclassesPreviousTo: aSuperclass

<details>
	<summary>See more</summary>
	
	withAllSuperclassesPreviousTo: aSuperclass

	| classes |
	
	classes _ self withAllSuperclassesUpTo: aSuperclass.
	
	^ classes allButLast
</details>

#### Behavior>>#methodDict: aDictionary

<details>
	<summary>See more</summary>
	
	methodDict: aDictionary
	methodDict := aDictionary
</details>

#### Behavior>>#superclass

Answer the receiver's superclass, a Class.


<details>
	<summary>See more</summary>
	
	superclass
	"Answer the receiver's superclass, a Class."

	^superclass
</details>

#### Behavior>>#methodNodeFor: aSourceCode

<details>
	<summary>See more</summary>
	
	methodNodeFor: aSourceCode

	^self methodNodeFor: aSourceCode noPattern: false
</details>

#### Behavior>>#stampAt: selector

<details>
	<summary>See more</summary>
	
	stampAt: selector
	^(self compiledMethodAt: selector ifAbsent: [ ^'' ]) timeStamp
</details>

#### Behavior>>#withAllSubclassesDo: aBlock

Evaluate the argument, aBlock, for the receiver and each of its subclasses.


<details>
	<summary>See more</summary>
	
	withAllSubclassesDo: aBlock 
	"Evaluate the argument, aBlock, for the receiver and each of its 
	subclasses."

	aBlock value: self.
	self allSubclassesDo: aBlock
</details>

#### Behavior>>#sharedPools

Answer a Set of the names of the pools (Dictionaries) that the receiver shares. 9/12/96 tk sharedPools have an order now


<details>
	<summary>See more</summary>
	
	sharedPools
	"Answer a Set of the names of the pools (Dictionaries) that the receiver 
	shares.
	9/12/96 tk  sharedPools have an order now"

	^ OrderedCollection new
</details>

#### Behavior>>#includesSelector: aSymbol

Answer whether the message whose selector is the argument is in the method dictionary of the receiver's class.


<details>
	<summary>See more</summary>
	
	includesSelector: aSymbol 
	"Answer whether the message whose selector is the argument is in the 
	method dictionary of the receiver's class."

	^ self methodDict includesKey: aSymbol
</details>

#### Behavior>>#allSuperclassesUpTo: aSuperclass

<details>
	<summary>See more</summary>
	
	allSuperclassesUpTo: aSuperclass
	
	| superclasses |
	
	^ superclass = aSuperclass
		ifTrue: [ OrderedCollection with: aSuperclass]
		ifFalse: [superclasses _ superclass allSuperclassesUpTo: aSuperclass.
			superclasses addFirst: superclass.
			superclasses]
</details>

#### Behavior>>#instSize

Answer the number of named instance variables (as opposed to indexed variables) of the receiver.


<details>
	<summary>See more</summary>
	
	instSize
	"Answer the number of named instance variables
	(as opposed to indexed variables) of the receiver."

	^ Smalltalk isSpur

		ifTrue: [
	 		"Above Cog Spur the class format is
				<5 bits inst spec><16 bits inst size>"
			format bitAnd: 16rFFFF ]

		ifFalse: [
			"NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
			When we revise the image format, it should become...
			^ ((format bitShift: -1) bitAnd: 16rFF) - 1
			Note also that every other method in this category will require
			2 bits more of right shift after the change."
			((format bitShift: -10) bitAnd: 16rC0) + ((format bitShift: -1) bitAnd: 16r3F) - 1 ]
</details>

#### Behavior>>#allSelectors

Answer all selectors understood by instances of the receiver


<details>
	<summary>See more</summary>
	
	allSelectors
	"Answer all selectors understood by instances of the receiver"

	| coll |
	coll _ OrderedCollection new.
	self withAllSuperclasses do:
		[:aClass | coll addAll: aClass selectors].
	^ coll asIdentitySet
</details>

#### Behavior>>#obsoleteSubclasses

Return all the weakly remembered obsolete subclasses of the receiver


<details>
	<summary>See more</summary>
	
	obsoleteSubclasses
	"Return all the weakly remembered obsolete subclasses of the receiver"
	| obs |
	ObsoleteSubclasses ifNil: [
		^#() ].
	ObsoleteSubclasses finalizeValues. "clean up if need be"
	obs _ ObsoleteSubclasses at: self ifAbsent:[^#()].
	obs _ obs copyWithout: nil.
	obs isEmpty
		ifTrue: [ ObsoleteSubclasses removeKey: self ifAbsent: nil ]
		ifFalse: [ ObsoleteSubclasses at: self put: obs].
	^obs
</details>

#### Behavior>>#isPointers

Answer whether the receiver contains just pointers (not bits).


<details>
	<summary>See more</summary>
	
	isPointers
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not
</details>

#### Behavior>>#compilerClass

Answer a compiler class appropriate for source methods of this class.


<details>
	<summary>See more</summary>
	
	compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler
</details>

#### Behavior>>#becomeCompact

Here are the restrictions on compact classes in order for export segments to work: A compact class index may not be reused. If a class was compact in a release of Squeak, no other class may use that index. The class might not be compact later, and there should be nil in its place in the array.


<details>
	<summary>See more</summary>
	
	becomeCompact
	"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct index |

	Smalltalk isSpur ifTrue: [^ self halt: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self halt: 'You must not make a weak class compact'].
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name , 'is already compact'].
	index _ cct indexOf: nil
		ifAbsent: [^ self halt: 'compact class table is full'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format _ format + (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Purge any old instances"
	Smalltalk garbageCollect.
</details>

#### Behavior>>#superclass: aClass methodDictionary: mDict format: fmt

Basic initialization of the receiver. Must only be sent to a new instance; else we would need Object flushCache.


<details>
	<summary>See more</summary>
	
	superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver.
	Must only be sent to a new instance; else we would need Object flushCache."
	superclass _ aClass.
	format _ fmt.
	methodDict _ mDict.
</details>

#### Behavior>>#methodDictionary

Convenience


<details>
	<summary>See more</summary>
	
	methodDictionary
	"Convenience"
	^self methodDict
</details>

#### Behavior>>#obsolete

Invalidate and recycle local messages, e.g., zap the method dictionary if can be done safely.


<details>
	<summary>See more</summary>
	
	obsolete
	"Invalidate and recycle local messages,
	e.g., zap the method dictionary if can be done safely."
	self canZapMethodDictionary
		ifTrue:[ methodDict _ MethodDictionary new ].
</details>

#### Behavior>>#compiledMethodAt: selector

Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, create an error notification.


<details>
	<summary>See more</summary>
	
	compiledMethodAt: selector 
	"Answer the compiled method associated with the argument, selector (a 
	Symbol), a message selector in the receiver's method dictionary. If the 
	selector is not in the dictionary, create an error notification."

	^ self methodDict at: selector
</details>

#### Behavior>>#printWithClosureAnalysisOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream 
	"Refer to the comment in Object|printOn:." 

	aStream nextPutAll: 'a descendent of '.
	superclass printWithClosureAnalysisOn: aStream
</details>

#### Behavior>>#firstPrecodeCommentFor:  selector

If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil


<details>
	<summary>See more</summary>
	
	firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MessageSet isPseudoSelector: selector)
		ifTrue:
			["Not really a selector"
			^ nil].
	source _ self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
	parser _ self parserClass new.
	tree _ 
		parser
			parse: (ReadStream on: source)
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first
</details>

#### Behavior>>#elementSize

Answer the size in bytes of an element in the receiver. The formats are 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	elementSize
	"Answer the size in bytes of an element in the receiver.  The formats are
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	instSpec < 9 ifTrue: [^Smalltalk wordSize].
	instSpec >= 16 ifTrue: [^1].
	instSpec >= 12 ifTrue: [^2].
	instSpec >= 10 ifTrue: [^4].
	^8
</details>

#### Behavior>>#indexOfInstanceVariable: aName

<details>
	<summary>See more</summary>
	
	indexOfInstanceVariable: aName

	^self allInstVarNames indexOf: aName
</details>

#### Behavior>>#shutDown: quitting

This message is sent on system shutdown to registered classes. If quitting is true, we are about to quit. If quitting is false, we are just snapshotting.


<details>
	<summary>See more</summary>
	
	shutDown: quitting
	"This message is sent on system shutdown to registered classes.
	If quitting is true, we are about to quit.
	If quitting is false, we are just snapshotting."

	^ self shutDown
</details>

#### Behavior>>#longPrintOn: aStream

Append to the argument, aStream, the names and values of all of the receiver's instance variables. But, not useful for a class with a method dictionary.


<details>
	<summary>See more</summary>
	
	longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  But, not useful for a class with a method dictionary."

	aStream nextPutAll: '<<too complex to show>>'; newLine
</details>

#### Behavior>>#inheritsFrom: aClass

Answer whether the argument, aClass, is on the receiver's superclass chain.


<details>
	<summary>See more</summary>
	
	inheritsFrom: aClass 
	"Answer whether the argument, aClass, is on the receiver's superclass 
	chain."

	| aSuperclass |
	aSuperclass _ superclass.
	[aSuperclass == nil]
		whileFalse: 
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass _ aSuperclass superclass].
	^false
</details>

#### Behavior>>#compiledMethodAt: selector ifAbsent: aBlock

Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock


<details>
	<summary>See more</summary>
	
	compiledMethodAt: selector ifAbsent: aBlock
	"Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock"

	^ self methodDict at: selector ifAbsent: aBlock
</details>

#### Behavior>>#methodNodeFor: aSourceCode noPattern: aBoolean

<details>
	<summary>See more</summary>
	
	methodNodeFor: aSourceCode noPattern: aBoolean

	|  parser methodNode |

	parser := self parserClass new.

	methodNode := parser parse: aSourceCode class: self noPattern: aBoolean.
	methodNode sourceText: aSourceCode.

	^methodNode
			
</details>

#### Behavior>>#allSuperclasses

Answer an OrderedCollection of the receiver's and the receiver's ancestor's superclasses. The first element is the receiver's immediate superclass, followed by its superclass and subsequent superclasses, and proceeding as long as there is a non-nil superclass.


<details>
	<summary>See more</summary>
	
	allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's  
	ancestor's superclasses. The first element is the receiver's immediate  
	superclass, followed by its superclass and subsequent superclasses,
	and proceeding as long as there is a non-nil superclass."

	| answer pivot |
	answer := OrderedCollection new.
	pivot := superclass.
	[pivot == nil] whileFalse:
		[
			answer add: pivot.
			pivot := pivot superclass
		].
	^answer
</details>

#### Behavior>>#isBits

Answer whether the receiver contains just bits (not pointers). Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isBits
	"Answer whether the receiver contains just bits (not pointers).
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec >= 7 ]
		ifFalse: [ self instSpec >= 6 ]
</details>

#### Behavior>>#hasMethods

Answer whether the receiver has any methods in its method dictionary.


<details>
	<summary>See more</summary>
	
	hasMethods
	"Answer whether the receiver has any methods in its method dictionary."

	^ self methodDict size > 0
</details>

#### Behavior>>#becomeUncompact

<details>
	<summary>See more</summary>
	
	becomeUncompact
	| cct index |
	cct _ Smalltalk compactClassesArray.
	(index _ self indexIfCompact) = 0
		ifTrue: [^ self].
	(cct includes: self)
		ifFalse: [^ self halt  "inconsistent state"].
	"Update instspec so future instances will not be compact"
	format _ format - (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Make sure there are no compact ones left around"
	Smalltalk garbageCollect.
	"Remove this class from the compact class table"
	cct at: index put: nil.

</details>

#### Behavior>>#kindOfSubclassSpur

Answer a String that is the keyword that describes the receiver's kind of subclass, either a regular subclass, a variableSubclass, a variableByteSubclass, a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass. c.f. typeOfClass & instSpec


<details>
	<summary>See more</summary>
	
	kindOfSubclassSpur
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass.
	 c.f. typeOfClass & instSpec"
	^(#(' subclass: '
		' subclass: '
		' variableSubclass: '
		' variableSubclass: '
		' weakSubclass: '
		' ephemeronSubclass: '
		nil
		' immediateSubclass: '
		nil
		' variableDoubleWordSubclass: '
		' variableWordSubclass: '		nil
		' variableDoubleByteSubclass: '	nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil )
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']
</details>

#### Behavior>>#whichSelectorsStoreInto: instVarName

Answer a Set of selectors whose methods access the argument, instVarName, as a named instance variable.


<details>
	<summary>See more</summary>
	
	whichSelectorsStoreInto: instVarName 
	"Answer a Set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."
	| instVarIndex |
	instVarIndex _ self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^ (self methodDict keys select: 
		[:sel | (self methodDict at: sel) writesField: instVarIndex]) asSet

	"Point whichSelectorsStoreInto: 'x'."
</details>

#### Behavior>>#whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

<details>
	<summary>See more</summary>
	
	whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock

	(self definesInstanceVariableNamed: aVariable) ifTrue: [ ^self ].

	^superclass whichClassDefinesInstanceVariable: aVariable ifNone: aNoneBlock
</details>

#### Behavior>>#becomeCompactSimplyAt: index

Make me compact, but don't update the instances. For importing segments.


<details>
	<summary>See more</summary>
	
	becomeCompactSimplyAt: index
	"Make me compact, but don't update the instances.  For importing segments."
"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct |

	Smalltalk isSpur ifTrue: [^ self halt: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self halt: 'You must not make a weak class compact'].
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name , 'is already compact'].
	(cct at: index) ifNotNil: [^ self halt: 'compact table slot already in use'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format _ format + (index bitShift: 11).
	"Caller must convert the instances"

</details>

#### Behavior>>#isVariablePreSpur

Answer whether the receiver has indexable variables.


<details>
	<summary>See more</summary>
	
	isVariablePreSpur
	"Answer whether the receiver has indexable variables."

	^ self instSpec >= 2
</details>

#### Behavior>>#selectSuperclasses: aBlock

Evaluate the argument, aBlock, with the receiver's superclasses as the argument. Collect into an OrderedCollection only those superclasses for which aBlock evaluates to true. In addition, evaluate aBlock for the superclasses of each of these successful superclasses and collect into the OrderedCollection ones for which aBlock evaluates to true. Answer the resulting OrderedCollection.


<details>
	<summary>See more</summary>
	
	selectSuperclasses: aBlock 
	"Evaluate the argument, aBlock, with the receiver's superclasses as the 
	argument. Collect into an OrderedCollection only those superclasses for 
	which aBlock evaluates to true. In addition, evaluate aBlock for the 
	superclasses of each of these successful superclasses and collect into the 
	OrderedCollection ones for which aBlock evaluates to true. Answer the 
	resulting OrderedCollection."

	| aSet |
	aSet _ Set new.
	self allSuperclasses do: 
		[:aSuperclass | 
		(aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
	^aSet
</details>

#### Behavior>>#superclass: aClass

Change the receiver's superclass to be aClass.


<details>
	<summary>See more</summary>
	
	superclass: aClass 
	"Change the receiver's superclass to be aClass."
	"Note: Do not use 'aClass isKindOf: Behavior' here
		in case we recompile from Behavior itself."
	(aClass == nil or: [aClass isBehavior])
		ifTrue: [superclass _ aClass.
				Object flushCache]
		ifFalse: [self error: 'superclass must be a class-describing object']
</details>

#### Behavior>>#testCaseClass

<details>
	<summary>See more</summary>
	
	testCaseClass

	self subclassResponsibility 
</details>

#### Behavior>>#handleFailingBasicNew

handleFailingBasicNew gets sent after basicNew has failed and allowed a scavenging garbage collection to occur. The scavenging collection will have happened as the VM is activating the (failing) basicNew. If handleFailingBasicNew fails then the scavenge failed to reclaim sufficient space and a global garbage collection is required. Retry after garbage collecting and growing memory if necessary. Primitive. Answer an instance of this class with the number of indexable variables specified by the argument, sizeRequested. Fail if this class is not indexable or if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	handleFailingBasicNew
	"handleFailingBasicNew gets sent after basicNew has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew.  If
	 handleFailingBasicNew fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	Smalltalk garbageCollect < 1048576 ifTrue:
		[Smalltalk growMemoryByAtLeast: 1048576].
	^self handleFailingFailingBasicNew "retry after global garbage collect"
</details>

#### Behavior>>#recompile: selector from: oldClass

Compile the method associated with selector in the receiver's method dictionary.


<details>
	<summary>See more</summary>
	
	recompile: selector from: oldClass
	"Compile the method associated with selector in the receiver's method dictionary."
	"ar 7/10/1999: Use oldClass compiledMethodAt: not self compiledMethodAt:"
	| method trailer methodNode |
	method _ oldClass compiledMethodAt: selector.
	trailer _ method trailer.
	methodNode _ self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ self].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	self addSelectorSilently: selector withMethod: (methodNode generate: trailer).

</details>

#### Behavior>>#selectorsDo: selectorBlock

Evaluate selectorBlock for all the message selectors in my method dictionary.


<details>
	<summary>See more</summary>
	
	selectorsDo: selectorBlock
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^ self methodDict keysDo: selectorBlock
</details>

#### Behavior>>#allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level

Walk the tree of subclasses, giving the class and its level. Sort nicely.


<details>
	<summary>See more</summary>
	
	allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level 
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	^ self allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level sortByCategory: false
</details>

#### Behavior>>#shutDown

This message is sent on system shutdown to registered classes


<details>
	<summary>See more</summary>
	
	shutDown
	"This message is sent on system shutdown to registered classes"

</details>

#### Behavior>>#instSpec

Answer the instance specification part of the format that defines what kind of object an instance of the receiver is.


<details>
	<summary>See more</summary>
	
	instSpec
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is."

	^ Smalltalk isSpur

		ifTrue: [
			"The formats are
					0	= 0 sized objects (UndefinedObject True False et al)
					1	= non-indexable objects with inst vars (Point et al)
					2	= indexable objects with no inst vars (Array et al)
					3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
					4	= weak indexable objects with inst vars (WeakArray et al)
					5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
					6	= unused
					7	= immediates (SmallInteger, Character)
					8	= unused
					9	= 64-bit indexable
				10-11	= 32-bit indexable (Bitmap)					(plus one odd bit, unused in 32-bits)
				12-15	= 16-bit indexable							(plus two odd bits, one unused in 32-bits)
				16-23	= 8-bit indexable							(plus three odd bits, one unused in 32-bits)
				24-31	= compiled methods (CompiledMethod)	(plus three odd bits, one unused in 32-bits)
			 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
			 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
			 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
			 has format 18 in 32-bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
			 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6)."
			(format bitShift: -16) bitAnd: 16r1F ]

		ifFalse: [
			(format bitShift: -7) bitAnd: 16rF ]
</details>

#### Behavior>>#setFormat: aFormatInstanceDescription

Needed for shout theme test.


<details>
	<summary>See more</summary>
	
	setFormat: aFormatInstanceDescription
	"Needed for shout theme test."

	"only use this method with extreme care since it modifies the format of the class 
     ie a description of the number of instance variables and whether the class is
     compact, variable sized"

	format := aFormatInstanceDescription


</details>

#### Behavior>>#allAccessesTo: instVarName

<details>
	<summary>See more</summary>
	
	allAccessesTo: instVarName 

	| references instVarIndex definingClass |
	
	definingClass _ self whichClassDefinesInstanceVariable: instVarName ifNone: [ ^#() ].
	instVarIndex _ self indexOfInstanceVariable: instVarName.
	references _ SortedCollection sortBlock: [ :left :right | left stringVersion <= right stringVersion ].
	
	definingClass withAllSubclassesDo: [ :class | class addReferencesOf: instVarName at: instVarIndex to: references ].
	
	^ references 
</details>

#### Behavior>>#allUnreferencedInstanceVariables

Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses


<details>
	<summary>See more</summary>
	
	allUnreferencedInstanceVariables

	"Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses"

	^ self allInstVarNames reject: [ :instanceVariableName | | definingClass |
		definingClass _ self classThatDefinesInstanceVariable: instanceVariableName.
		definingClass isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]
</details>

#### Behavior>>#basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock

Compile code without logging the source in the changes file


<details>
	<summary>See more</summary>
	
	basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^ CompiledMethodWithNode generateMethodFromNode: methodNode trailer: bytes.
</details>

#### Behavior>>#crossReference

Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included.


<details>
	<summary>See more</summary>
	
	crossReference
	"Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included."

	^self selectors asArray sort collect: [ :x |
		Array 
			with: String newLineString, x 
			with: (self whichSelectorsReferTo: x) ]

	"
	Point crossReference.
	"
</details>

#### Behavior>>#allClassVarNames

Answer a Set of the names of the receiver's and the receiver's ancestor's class variables.


<details>
	<summary>See more</summary>
	
	allClassVarNames
	"Answer a Set of the names of the receiver's and the receiver's ancestor's 
	class variables."

	^superclass allClassVarNames
</details>

#### Behavior>>#kindOfSubclass

Answer a String that is the keyword that describes the receiver's kind of subclass, either a regular subclass, a variableSubclass, a variableByteSubclass, a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass (the last 2, only if Spur). c.f. typeOfClass & instSpec


<details>
	<summary>See more</summary>
	
	kindOfSubclass
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass
	(the last 2, only if Spur).
	 c.f. typeOfClass & instSpec"

	^ Smalltalk isSpur
		ifTrue: [ self kindOfSubclassSpur ]
		ifFalse: [ self kindOfSubclassPreSpur ]
</details>

#### Behavior>>#variablesAndOffsetsDo: aBinaryBlock

This is the interface between the compiler and a class's instance or field names. The class should enumerate aBinaryBlock with the field definitions (with nil offsets) followed by the instance variable name strings and their integer offsets (1-relative). The order is important; names evaluated later will override the same names occurring earlier.


<details>
	<summary>See more</summary>
	
	variablesAndOffsetsDo: aBinaryBlock
	"This is the interface between the compiler and a class's instance or field names.  The
	 class should enumerate aBinaryBlock with the field definitions (with nil offsets) followed
	 by the instance variable name strings and their integer offsets (1-relative).  The order is
	 important; names evaluated later will override the same names occurring earlier."

	"Only need to do instance variables here.  CProtoObject introduces field definitions."
	self instVarNamesAndOffsetsDo: aBinaryBlock
</details>

#### Behavior>>#sourceCodeAt: selector ifAbsent: aBlock

<details>
	<summary>See more</summary>
	
	sourceCodeAt: selector ifAbsent: aBlock

	^ (self methodDict at: selector ifAbsent: [^ aBlock value]) getSourceFor: selector in: self
</details>

#### Behavior>>#lastUnderscoreMeansSubscript

Redefine this method on the class side of those classes where you want a_1 to be shown as 'a subscript 1'


<details>
	<summary>See more</summary>
	
	lastUnderscoreMeansSubscript
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false
</details>

#### Behavior>>#whichSelectorsReferTo: literal special: specialFlag byte: specialByte

Answer a set of selectors whose methods access the argument as a literal.


<details>
	<summary>See more</summary>
	
	whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |

	Preferences thoroughSenders 
		ifTrue: [ who _ self thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte ]
		ifFalse: [ 
			who _ Set new.
			self selectorsAndMethodsDo: [:sel :method |
				((method hasLiteral: literal) or: [specialFlag and: [method scanFor: specialByte]]) ifTrue: [
					((literal isVariableBinding) not or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
						or: [(method indexOfLiteral: literal) ~= 0]]) ifTrue: [who add: sel]]]].
		
	^self rejectSelectorsFrom: who thatReferenceTo: literal byte: specialByte 
</details>

#### Behavior>>#printHierarchy

Answer a description containing the names and instance variable names of all of the subclasses and superclasses of the receiver.


<details>
	<summary>See more</summary>
	
	printHierarchy
	"Answer a description containing the names and instance variable names 
	of all of the subclasses and superclasses of the receiver."

	| aStream index |
	index _ 0.
	aStream _ WriteStream on: (String new: 16).
	self allSuperclasses reverseDo: [ :aClass | 
		aStream newLineTab: index.
		index _ index + 1.
		aStream nextPutAll: aClass name.
		aStream space.
		aStream print: aClass instVarNames].
	aStream newLine.
	self printSubclassesOn: aStream level: index.
	^aStream contents
</details>

#### Behavior>>#defaultMethodTrailer

<details>
	<summary>See more</summary>
	
	defaultMethodTrailer
	^ #(0 0 0 0)
</details>

#### Behavior>>#allSubInstances

Answer a list of all current instances of the receiver and all of its subclasses.


<details>
	<summary>See more</summary>
	
	allSubInstances 
	"Answer a list of all current instances of the receiver and all of its subclasses."
	| aCollection |
	aCollection _ OrderedCollection new.
	self allSubInstancesDo:
		[:x | x == aCollection ifFalse: [aCollection add: x]].
	^ aCollection
</details>

#### Behavior>>#spaceUsed

Answer a rough estimate of number of bytes used by this class and its metaclass. Does not include space used by class variables.


<details>
	<summary>See more</summary>
	
	spaceUsed
	"Answer a rough estimate of number of bytes used by this class and its metaclass. Does not include space used by class variables."

	| space |
	space _ 0.
	self selectorsDo: [ :sel | | method |
		space _ space + 16.  "dict and org'n space"
		method _ self compiledMethodAt: sel.
		space _ space + (method size + 6 "hdr + avg pad").
		method literalsDo: [ :lit |
			(lit isMemberOf: Array) ifTrue: [ space _ space + ((lit size + 1) * 4)].
			(lit isMemberOf: Float) ifTrue: [ space _ space + 12].
			(lit isMemberOf: String) ifTrue: [ space _ space + (lit size + 6)].
			(lit isMemberOf: LargeNegativeInteger) ifTrue: [ space _ space + ((lit size + 1) * 4)].
			(lit isMemberOf: LargePositiveInteger) ifTrue: [ space _ space + ((lit size + 1) * 4)]]].
	^ space
</details>

#### Behavior>>#typeOfClassSpur

Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec


<details>
	<summary>See more</summary>
	
	typeOfClassSpur
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^(#(normal
		normal
		variable
		variable
		weak
		ephemeron
		nil
		immediate
		nil
		longs
		words				nil
		shorts				nil nil nil
		bytes				nil nil nil nil nil nil nil
		compiledMethod	nil nil nil nil nil nil nil)
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']
</details>

#### Behavior>>#withAllSuperclassesUpTo: aSuperclass

<details>
	<summary>See more</summary>
	
	withAllSuperclassesUpTo: aSuperclass

	| classes |
	
	classes _ self allSuperclassesUpTo: aSuperclass.
	classes addFirst: self.
	
	^ classes
</details>

#### Behavior>>#inspectSubInstances

Inspect all instances of the receiver and all its subclasses. CAUTION - don't do this for something as generic as Object! 1/26/96 sw


<details>
	<summary>See more</summary>
	
	inspectSubInstances 
	"Inspect all instances of the receiver and all its subclasses.  CAUTION - don't do this for something as generic as Object!  1/26/96 sw"

	| all allSize prefix |
	all _ self allSubInstances.
	(allSize _ all size) = 0 ifTrue: [^ self inform: 'There are no 
instances of ', self name, '
or any of its subclasses'].
	prefix _ allSize = 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].
	
	all asArray inspectWithLabel: (prefix, ' of ', self name, ' & its subclasses')
</details>

#### Behavior>>#typeOfClass

Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec


<details>
	<summary>See more</summary>
	
	typeOfClass
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^ Smalltalk isSpur
		ifTrue: [ self typeOfClassSpur ]
		ifFalse: [ self typeOfClassPreSpur ]
</details>

#### Behavior>>#whichClassIncludesSelector: aSymbol

Answer the class on the receiver's superclass chain where the argument, aSymbol (a message selector), will be found. Answer nil if none found.


<details>
	<summary>See more</summary>
	
	whichClassIncludesSelector: aSymbol 
	"Answer the class on the receiver's superclass chain where the 
	argument, aSymbol (a message selector), will be found. Answer nil if none found."
	"Rectangle whichClassIncludesSelector: #inspect."
	(self includesSelector: aSymbol)
		ifTrue: [^ self].
	superclass
		ifNil: [^ nil].
	^ superclass whichClassIncludesSelector: aSymbol
</details>

#### Behavior>>#allRegularInstVarNames

Answer an Array of the names of the receiver's instance variables. The Array ordering is the order in which the variables are stored and accessed by the interpreter. Quite like asking #allInstVarNames, but do not include Behavior state (i.e. Smalltalk internals)


<details>
	<summary>See more</summary>
	
	allRegularInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter.
	
	Quite like asking #allInstVarNames, but do not include Behavior state (i.e. Smalltalk internals)"

	| vars |
	self == ProtoObject class
		ifTrue: [vars _ self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [vars _ superclass allRegularInstVarNames , self instVarNames].
	^vars
</details>

#### Behavior>>#handleFailingBasicNew: sizeRequested

handleFailingBasicNew: gets sent after basicNew: has failed and allowed a scavenging garbage collection to occur. The scavenging collection will have happened as the VM is activating the (failing) basicNew:. If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient space and a global garbage collection is required. Retry after garbage collecting and growing memory if necessary. Primitive. Answer an instance of this class with the number of indexable variables specified by the argument, sizeRequested. Fail if this class is not indexable or if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	handleFailingBasicNew: sizeRequested
	"handleFailingBasicNew: gets sent after basicNew: has failed and allowed
	 a scavenging garbage collection to occur.  The scavenging collection
	 will have happened as the VM is activating the (failing) basicNew:.  If
	 handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.

	 Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	| bytesRequested |
	bytesRequested := self byteSizeOfInstanceOfSize: sizeRequested.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	"retry after global garbage collect and possible grow"
	^self handleFailingFailingBasicNew: sizeRequested
</details>

#### Behavior>>#someInstance

Primitive. Answer the first instance in the enumeration of all instances of the receiver. Fails if there are none. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	someInstance
	"Primitive. Answer the first instance in the enumeration of all instances 
	of the receiver. Fails if there are none. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 77>
	^nil
</details>

#### Behavior>>#allInstances

Answer all instances of the receiver.


<details>
	<summary>See more</summary>
	
	allInstances
	"Answer all instances of the receiver."
	<primitive: 177>
	"The primitive can fail because memory is low.  If so, fall back on the old
	 enumeration code, which gives the system a chance to GC and/or grow.
	 Because aBlock might change the class of inst (for example, using become:),
	 it is essential to compute next before aBlock value: inst."
	| inst insts next |
	insts := WriteStream on: (Array new: 64).
	inst := self someInstance.
	[inst == nil] whileFalse:
		[next := inst nextInstance.
		 (inst == insts or: [inst == insts originalContents]) ifFalse: [insts nextPut: inst].
		 inst := next].
	^insts contents
</details>

#### Behavior>>#definesClassVariableNamedInHierarchy: aClassVariableName

<details>
	<summary>See more</summary>
	
	definesClassVariableNamedInHierarchy: aClassVariableName

	^self allClassVarNames includes: aClassVariableName 
</details>

#### Behavior>>#removeSelector: selector

Assuming that the argument, selector (a Symbol), is a message selector in my method dictionary, remove it and its method.


<details>
	<summary>See more</summary>
	
	removeSelector: selector 
	"Assuming that the argument, selector (a Symbol), is a message selector 
	in my method dictionary, remove it and its method."

	^ self basicRemoveSelector: selector
</details>

#### Behavior>>#hasChangedComparedTo: anotherClass

<details>
	<summary>See more</summary>
	
	hasChangedComparedTo: anotherClass

	^self superclass ~~ anotherClass superclass
		or: [ self instVarNames ~= anotherClass instVarNames
		or: [ self classVarNames ~= anotherClass classVarNames
		or: [ self sharedPools ~= anotherClass sharedPools ]]]
</details>

#### Behavior>>#byteSizeOfInstanceOfSize: basicSize

Answer the total memory size of an instance of the receiver with the given number of indexable instance variables.


<details>
	<summary>See more</summary>
	
	byteSizeOfInstanceOfSize: basicSize
	"Answer the total memory size of an instance of the receiver
	 with the given number of indexable instance variables."
	"Only for Spur"

	<primitive: 181 error: ec>
	self isVariable
		ifTrue: "If the primitive overflowed answer a close approximation"
			[(basicSize isInteger
			  and: [basicSize >= 16r1000000]) ifTrue:
				[^2 * (self byteSizeOfInstanceOfSize: basicSize + 1 // 2)
				   - (self byteSizeOfInstanceOfSize: 0)]]
		ifFalse:
			[basicSize = 0 ifTrue:
				[^self byteSizeOfInstance]].
	self primitiveFailed
</details>

#### Behavior>>#closuresInfoAt: selector

<details>
	<summary>See more</summary>
	
	closuresInfoAt: selector
	^Smalltalk closuresInfoStringForClass: self selector: selector
</details>

#### Behavior>>#selectorAtMethod: method setClass: classResultBlock

Answer both the message selector associated with the compiled method and the class in which that selector is defined.


<details>
	<summary>See more</summary>
	
	selectorAtMethod: method setClass: classResultBlock 
	"Answer both the message selector associated with the compiled method 
	and the class in which that selector is defined."

	| sel |
	sel _ self methodDict keyAtIdentityValue: method
				ifAbsent: [ | sel2 |
					superclass
						ifNil: [
							classResultBlock value: self.
							^method defaultSelector].
					sel2 _ superclass selectorAtMethod: method setClass: classResultBlock.
					"Set class to be self, rather than that returned from superclass. "
					sel2 == method defaultSelector ifTrue: [ classResultBlock value: self ].
					^sel2].
	classResultBlock value: self.
	^sel
</details>

#### Behavior>>#identityHash

Answer a SmallInteger whose value is related to the receiver's identity. Behavior implements identityHash to allow the VM to use an object representation which does not include a direct reference to an object's class in an object. If the VM is using this implementation then classes are held in a class table and instances contain the index of their class in the table. A class's class table index is its identityHash so that an instance can be created without searching the table for a class's index. The VM uses this primitive to enter the class into the class table, assigning its identityHash with an as yet unused class table index. If this primitive fails it means that the class table is full. In Spur as of 2014 there are 22 bits of classTable index and 22 bits of identityHash per object. Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  If the VM is using
	 this implementation then classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.  In Spur as of
	 2014 there are 22 bits of classTable index and 22 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 175>
	self primitiveFailed
</details>

#### Behavior>>#allSubclassesDoGently: aBlock

Evaluate the argument, aBlock, for each of the receiver's subclasses.


<details>
	<summary>See more</summary>
	
	allSubclassesDoGently: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDoGently: 
		[:cl | 
		cl isInMemory ifTrue: [
			aBlock value: cl.
			cl allSubclassesDoGently: aBlock]]
</details>

#### Behavior>>#selectors

Answer a Set of all the message selectors specified in the receiver's method dictionary.


<details>
	<summary>See more</summary>
	
	selectors
	"Answer a Set of all the message selectors specified in the receiver's 
	method dictionary."

	^ self methodDict keys
	"
	Point selectors
	"
</details>

#### Behavior>>#firstCommentAt:  selector

Answer a string representing the first comment in the method associated with selector. Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment. Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote.


<details>
	<summary>See more</summary>
	
	firstCommentAt:  selector
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote."

	| sourceString commentStart  pos nextQuotePos |

	sourceString _ (self sourceCodeAt: selector) asString.
	sourceString size = 0 ifTrue: [^ ''].
	commentStart _ sourceString findString: '"' startingAt: 1.
	commentStart = 0 ifTrue: [^ ''].
	pos _ commentStart + 1.
	[(nextQuotePos _ sourceString findString: '"' startingAt: pos) = (sourceString findString: '""' startingAt: pos)]
		whileTrue:
			[pos _ nextQuotePos + 2].
	
	commentStart = nextQuotePos ifTrue: [^ ''].  "Must have been a quote in string literal"

	^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'


"Behavior firstCommentAt: #firstCommentAt:"
</details>

#### Behavior>>#basicNew: sizeRequested

Primitive. Answer an instance of this class with the number of indexable variables specified by the argument, sizeRequested. Fail if this class is not indexable or if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive. If the primitive fails because space is low then the scavenger will run before the method is activated. Check args and retry via handleFailingBasicNew: if they're OK.


<details>
	<summary>See more</summary>
	
	basicNew: sizeRequested
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 71 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingBasicNew: sizeRequested].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self primitiveFailed
</details>

#### Behavior>>#typeOfClassPreSpur

Answer a symbol uniquely describing the type of the receiver


<details>
	<summary>See more</summary>
	
	typeOfClassPreSpur
	"Answer a symbol uniquely describing the type of the receiver"
	self instSpec = CompiledMethod instSpec ifTrue:[^#compiledMethod]. "Very special!"
	self isBytes ifTrue:[^#bytes].
	(self isWords and:[self isPointers not]) ifTrue:[^#words].
	self isWeak ifTrue:[^#weak].
	self isVariable ifTrue:[^#variable].
	^#normal.
</details>

#### Behavior>>#flushCache

Tell the interpreter to remove the contents of its method lookup cache, if it has one. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has 
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed
</details>

#### Behavior>>#definesInstanceVariableNamedInHierarchy: anInstanceVariableName

<details>
	<summary>See more</summary>
	
	definesInstanceVariableNamedInHierarchy: anInstanceVariableName

	^self allInstVarNames includes: anInstanceVariableName
</details>

#### Behavior>>#new

Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable.


<details>
	<summary>See more</summary>
	
	new
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	^ self basicNew initialize

</details>

#### Behavior>>#methodHeaderFor: selector

Answer the string corresponding to the method header for the given selector


<details>
	<summary>See more</summary>
	
	methodHeaderFor: selector 
	"Answer the string corresponding to the method header for the given selector"

	| methodSource |
	methodSource _ self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
"

</details>

#### Behavior>>#releaseClassState

Will be called for each class on 'Save as new version'. All class vars or class instVar vars that can be recreated lazily on demand, should be nilled. This is done not only to save space, but more importantly, to prepare Cuis for a complete bootstrap from sources. For this, it should be possible to recreate all class state, at least with default values. See implementors for examples


<details>
	<summary>See more</summary>
	
	releaseClassState
	"Will be called for each class on 'Save as new version'.
	All class vars or class instVar vars that can be recreated lazily on demand, should be nilled. This is done not only to save space, but more importantly, to prepare Cuis for a complete bootstrap from sources. For this, it should be possible to recreate all class state, at least with default values.
	See implementors for examples"
</details>

#### Behavior>>#inspectAllInstances

Inpsect all instances of the receiver. 1/26/96 sw


<details>
	<summary>See more</summary>
	
	inspectAllInstances 
	"Inpsect all instances of the receiver.  1/26/96 sw"

	| all allSize prefix |
	all _ self allInstances.
	(allSize _ all size) = 0 ifTrue: [^ self inform: 'There are no 
instances of ', self name].
	prefix _ allSize = 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].
	
	all asArray inspectWithLabel: (prefix, ' of ', self name)
</details>

#### Behavior>>#typeName

If the class whishes to be shown in a different way in the selectors documentation. For example, DenotativeObject does not show it self as a metaclass but as a class - Hernan


<details>
	<summary>See more</summary>
	
	typeName

	"If the class whishes to be shown in a different way in the selectors documentation. 
	For example, DenotativeObject does not show it self as a metaclass but as a class - Hernan"
	
	^self name
</details>

#### Behavior>>#isCompiledMethodClass

Answer whether the receiver has compiled method instances that mix pointers and bytes.


<details>
	<summary>See more</summary>
	
	isCompiledMethodClass
	"Answer whether the receiver has compiled method instances that mix pointers and bytes."
	^self instSpec >= 24
</details>

#### Behavior>>#isVariableSpur

Answer whether the receiver has indexable variables. Above Cog Spur the class format is <5 bits inst spec><16 bits inst size> where the 5-bit inst spec is 0 = 0 sized objects (UndefinedObject True False et al) 1 = non-indexable objects with inst vars (Point et al) 2 = indexable objects with no inst vars (Array et al) 3 = indexable objects with inst vars (MethodContext AdditionalMethodState et al) 4 = weak indexable objects with inst vars (WeakArray et al) 5 = weak non-indexable objects with inst vars (ephemerons) (Ephemeron) 6 = unused 7 = immediates (SmallInteger, Character) 8 = unused 9 = 64-bit indexable 10-11 = 32-bit indexable (Bitmap) 12-15 = 16-bit indexable 16-23 = 8-bit indexable 24-31 = compiled methods (CompiledMethod)


<details>
	<summary>See more</summary>
	
	isVariableSpur
	"Answer whether the receiver has indexable variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := self instSpec.
	^instSpec >= 2 and: [instSpec <= 4 or: [instSpec >= 9]]
</details>

## Categorizer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### Categorizer>>#isEmptyCategoryNumber: anInteger

<details>
	<summary>See more</summary>
	
	isEmptyCategoryNumber: anInteger

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ true].
	firstIndex _ self firstIndexOfCategoryNumber: anInteger.
	lastIndex _  self lastIndexOfCategoryNumber: anInteger.
	^ firstIndex > lastIndex
</details>

#### Categorizer>>#addCategory: catString before: nextCategory

Add a new category named heading. If default category exists and is empty, remove it. If nextCategory is nil, then add the new one at the end, otherwise, insert it before nextCategory.


<details>
	<summary>See more</summary>
	
	addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	| index newCategory |
	newCategory _ catString .
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^self].	"heading already exists, so done"
	index _ categoryArray indexOf: nextCategory
		ifAbsent: [categoryArray size + 1].
	categoryArray _ categoryArray
		copyReplaceFrom: index
		to: index-1
		with: (Array with: newCategory).
	categoryStops _ categoryStops
		copyReplaceFrom: index
		to: index-1
		with: (Array with: (index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1])).
	"remove empty default category"
	(newCategory ~= Default
			and: [(self listAtCategoryNamed: Default) isEmpty])
		ifTrue: [self removeCategory: Default]
</details>

#### Categorizer>>#moveCategoryUp: catString

<details>
	<summary>See more</summary>
	
	moveCategoryUp: catString
	| category index newIndex newCategories |
	category _ catString asSymbol.
	newCategories _ categoryArray copy.
	index _ newCategories indexOf: category.
	newIndex _ 0.
	index > 1 ifTrue: [
		newIndex _ index-1.
		newCategories swap: index with: newIndex.
		self categories: newCategories].
	^newIndex
</details>

#### Categorizer>>#numberOfCategoryOfElement: element

Answer the index of the category with which the argument, element, is associated.


<details>
	<summary>See more</summary>
	
	numberOfCategoryOfElement: element 
	"Answer the index of the category with which the argument, element, is 
	associated."

	| categoryIndex elementIndex |
	categoryIndex _ 1.
	elementIndex _ 0.
	[(elementIndex _ elementIndex + 1) <= elementArray size]
		whileTrue: 
			["point to correct category"
			[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: [categoryIndex _ categoryIndex + 1].
			"see if this is element"
			element = (elementArray at: elementIndex) ifTrue: [^categoryIndex]].
	^0
</details>

#### Categorizer>>#classify: element under: heading suppressIfDefault: aBoolean

Store the argument, element, in the category named heading. If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein


<details>
	<summary>See more</summary>
	
	classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading |
	((heading = NullCategory) or: [heading == nil])
		ifTrue: [realHeading _ Default]
		ifFalse: [realHeading _ heading ].
	(catName _ self categoryOfElement: element) = realHeading
		ifTrue: [^ self].  "done if already under that category"

	catName ifNotNil: [
		(aBoolean and: [realHeading = Default])
				ifTrue: [^ self].	  "return if non-Default category already assigned in memory"
		self removeElement: element].	"remove if in another category"

	(categoryArray indexOf: realHeading) = 0 ifTrue: [self addCategory: realHeading].

	catIndex _ categoryArray indexOf: realHeading.
	elemIndex _ 
		catIndex > 1
			ifTrue: [categoryStops at: catIndex - 1]
			ifFalse: [0].
	[(elemIndex _ elemIndex + 1) <= (categoryStops at: catIndex) 
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray _ elementArray copyReplaceFrom: elemIndex to: elemIndex-1
						with: (Array with: element).

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: 
		[:i | categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: Default) size = 0 ifTrue: [self removeCategory: Default]
</details>

#### Categorizer>>#removeCategory: cat

Remove the category named, cat. Create an error notificiation if the category has any elements in it.


<details>
	<summary>See more</summary>
	
	removeCategory: cat 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index _ categoryArray indexOf: cat ifAbsent: [^self].
	lastStop _ 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category'].
	categoryArray _ categoryArray copyReplaceFrom: index to: index with: Array new.
	categoryStops _ categoryStops copyReplaceFrom: index to: index with: Array new.
	categoryArray size = 0
		ifTrue:
			[categoryArray _ Array with: Default.
			categoryStops _ Array with: 0]

</details>

#### Categorizer>>#changeFromCategorySpecs: categorySpecs

Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane.


<details>
	<summary>See more</summary>
	
	changeFromCategorySpecs: categorySpecs 
	"Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane."

	| oldElements newElements newCategories newStops currentStop temp cc catSpec |
	oldElements _ elementArray asSet.
	newCategories _ Array new: categorySpecs size.
	newStops _ Array new: categorySpecs size.
	currentStop _ 0.
	newElements _ WriteStream on: (Array new: 16).
	1 to: categorySpecs size do: [ :i | 
		catSpec _ categorySpecs at: i.
		newCategories at: i put: catSpec first asSymbol.
		catSpec allButFirst asArray sort do:
			[:elem |
			(oldElements remove: elem ifAbsent: nil) ifNotNil: [
				newElements nextPut: elem.
				currentStop _ currentStop+1]].
		newStops at: i put: currentStop].

	"Ignore extra elements but don't lose any existing elements!"
	oldElements _ oldElements collect: [ :elem | Array with: (self categoryOfElement: elem) with: elem].
	newElements _ newElements contents.
	categoryArray _ newCategories.
	(cc _ categoryArray asSet) size = categoryArray size ifFalse: [ "has duplicate element"
		temp _ categoryArray asOrderedCollection.
		temp removeAll: categoryArray asSet asOrderedCollection.
		temp do: [ :each | | dup ii |
			dup _ each.
			ii _ categoryArray indexOf: dup.
			[ dup _ (dup,' #2') asSymbol.  cc includes: dup ] whileTrue.
			cc add: dup.
			categoryArray at: ii put: dup]].
	categoryStops _ newStops.
	elementArray _ newElements.
	oldElements do: [:pair | self classify: pair last under: pair first]
</details>

#### Categorizer>>#allMethodSelectors

give a list of all method selectors.


<details>
	<summary>See more</summary>
	
	allMethodSelectors
	"give a list of all method selectors."

	^ elementArray copy sort
</details>

#### Categorizer>>#categories: anArray

Reorder my categories to be in order of the argument, anArray. If the resulting organization does not include all elements, then give an error.


<details>
	<summary>See more</summary>
	
	categories: anArray 
	"Reorder my categories to be in order of the argument, anArray. If the 
	resulting organization does not include all elements, then give an error."

	| newCategories newStops newElements catName list runningTotal | 
	newCategories _ Array new: anArray size.
	newStops _ Array new: anArray size.
	newElements _ #().
	runningTotal _ 0.
	1 to: anArray size do:
		[:i |
		catName _ (anArray at: i) asSymbol.
		list _ self listAtCategoryNamed: catName.
				newElements _ newElements, list.
				newCategories at: i put: catName.
				newStops at: i put: (runningTotal _ runningTotal + list size)].
	elementArray do: [ :element | "check to be sure all elements are included"
		(newElements includes: element)
			ifFalse: [^self error: 'New categories must match old ones']].
	"Everything is good, now update my three arrays."
	categoryArray _ newCategories.
	categoryStops _ newStops.
	elementArray _ newElements
</details>

#### Categorizer>>#categories

Answer an Array of categories (names).


<details>
	<summary>See more</summary>
	
	categories
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ nil].
	(categoryArray size = 1 
		and: [categoryArray first = Default & (elementArray size = 0)])
		ifTrue: [^Array with: NullCategory].
	^categoryArray
</details>

#### Categorizer>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Refer to the comment in Object|printOn:."

	| elementIndex |
	elementIndex _ 1.
	1 to: categoryArray size do: [ :i | 
		aStream nextPut: $(.
		(categoryArray at: i) asString printOn: aStream.
		[elementIndex <= (categoryStops at: i)]
			whileTrue: 
				[aStream space; nextPutAll: (elementArray at: elementIndex).
				elementIndex _ elementIndex + 1].
		aStream nextPut: $); newLine]
</details>

#### Categorizer>>#renameCategory: oldCatString toBe: newCatString

Rename a category. No action if new name already exists, or if old name does not exist.


<details>
	<summary>See more</summary>
	
	renameCategory: oldCatString toBe: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."
	| index oldCategory newCategory |
	oldCategory _ oldCatString asSymbol.
	newCategory _ newCatString asSymbol.
	(categoryArray indexOf: newCategory) > 0
		ifTrue: [^ self].	"new name exists, so no action"
	(index _ categoryArray indexOf: oldCategory) = 0
		ifTrue: [^ self].	"old name not found, so no action"
	categoryArray _ categoryArray copy.  "need to change identity so smart list update will notice the change"
	categoryArray at: index put: newCategory
</details>

#### Categorizer>>#addCategory: newCategory

<details>
	<summary>See more</summary>
	
	addCategory: newCategory
	^ self addCategory: newCategory before: nil 
</details>

#### Categorizer>>#testCaseClassesAt: aCategoryName

<details>
	<summary>See more</summary>
	
	testCaseClassesAt: aCategoryName

	^(self classesAt: aCategoryName) select: [ :aClass | aClass is: #TestCaseClass ]
</details>

#### Categorizer>>#moveCategoryDown: catString

<details>
	<summary>See more</summary>
	
	moveCategoryDown: catString
	| category index newIndex newCategories |
	category _ catString asSymbol.
	newCategories _ categoryArray copy.
	index _ newCategories indexOf: category.
	newIndex _ 0.
	(index > 0 and: [index < newCategories size]) ifTrue: [
		newIndex _ index+1.
		newCategories swap: index with: newIndex.
		self categories: newCategories].
	^newIndex
</details>

#### Categorizer>>#elementArray

<details>
	<summary>See more</summary>
	
	elementArray

	^ elementArray
</details>

#### Categorizer>>#categoryOfElement: element

Answer the category associated with the argument, element.


<details>
	<summary>See more</summary>
	
	categoryOfElement: element 
	"Answer the category associated with the argument, element."

	| index |
	index _ self numberOfCategoryOfElement: element.
	index = 0
		ifTrue: [^nil]
		ifFalse: [^categoryArray at: index]
</details>

#### Categorizer>>#lastIndexOfCategoryNumber: anInteger

<details>
	<summary>See more</summary>
	
	lastIndexOfCategoryNumber: anInteger
	anInteger > categoryStops size ifTrue: [^ nil].
	^ categoryStops at: anInteger
</details>

#### Categorizer>>#changeFromString: aString

Parse the argument, aString, and make this be the receiver's structure.


<details>
	<summary>See more</summary>
	
	changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs _ Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self setDefaultList: Array new].

	^ self changeFromCategorySpecs: categorySpecs
</details>

#### Categorizer>>#scanFrom: aStream

Reads in the organization from the next chunk on aStream. Categories or elements not found in the definition are not affected. New elements are ignored.


<details>
	<summary>See more</summary>
	
	scanFrom: aStream
	"Reads in the organization from the next chunk on aStream.
	Categories or elements not found in the definition are not affected.
	New elements are ignored."

	self changeFromString: aStream nextChunk
</details>

#### Categorizer>>#elementCategoryDict

<details>
	<summary>See more</summary>
	
	elementCategoryDict
	| dict firstIndex lastIndex |
	elementArray ifNil: [^ nil].
	dict _ Dictionary new: elementArray size.
	1to: categoryStops size do: [:cat |
		firstIndex _ self firstIndexOfCategoryNumber: cat.
		lastIndex _ self lastIndexOfCategoryNumber: cat.
		firstIndex to: lastIndex do: [:el |
			dict at: (elementArray at: el) put: (categoryArray at: cat)].
	].
	^ dict.
</details>

#### Categorizer>>#moveCategoryBottom: catString

<details>
	<summary>See more</summary>
	
	moveCategoryBottom: catString
	| category newCategories |
	category _ catString asSymbol.
	newCategories _ (categoryArray copyWithout: category), (Array with: category).
	self categories: newCategories.
	^ newCategories size
</details>

#### Categorizer>>#classesAt: aCategoryName

<details>
	<summary>See more</summary>
	
	classesAt: aCategoryName

	^(self listAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName ]
		
</details>

#### Categorizer>>#sortCategories

<details>
	<summary>See more</summary>
	
	sortCategories
	| privateCategories publicCategories newCategories |

	privateCategories _ self categories select: [ :one |
		(one findString: 'private' startingAt: 1 caseSensitive: false) = 1].
	publicCategories _ self categories copyWithoutAll: privateCategories.
	newCategories _ publicCategories asArray sort,
		privateCategories asArray sort.
	self categories: newCategories
</details>

#### Categorizer>>#removeEmptyCategories

Remove empty categories.


<details>
	<summary>See more</summary>
	
	removeEmptyCategories
	"Remove empty categories."

	| categoryIndex currentStop keptCategories keptStops |
	keptCategories _ WriteStream on: (Array new: 16).
	keptStops _ WriteStream on: (Array new: 16).
	currentStop _ categoryIndex _ 0.
	[(categoryIndex _ categoryIndex + 1) <= categoryArray size]
		whileTrue: 
			[(categoryStops at: categoryIndex) > currentStop
				ifTrue: 
					[keptCategories nextPut: (categoryArray at: categoryIndex).
					keptStops nextPut: (currentStop _ categoryStops at: categoryIndex)]].
	categoryArray _ keptCategories contents.
	categoryStops _ keptStops contents.
	categoryArray size = 0
		ifTrue:
			[categoryArray _ Array with: Default.
			categoryStops _ Array with: 0]

	"ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories]."
</details>

#### Categorizer>>#listAtCategoryNumber: anInteger

Answer the array of elements stored at the position indexed by anInteger. Answer nil if anInteger is larger than the number of categories.


<details>
	<summary>See more</summary>
	
	listAtCategoryNumber: anInteger 
	"Answer the array of elements stored at the position indexed by anInteger.  Answer nil if anInteger is larger than the number of categories."

	| firstIndex lastIndex |
	(anInteger < 1 or: [anInteger > categoryStops size])
		ifTrue: [^ nil].
	firstIndex _ self firstIndexOfCategoryNumber: anInteger.
	lastIndex _  self lastIndexOfCategoryNumber: anInteger.
	^elementArray copyFrom: firstIndex to: lastIndex
</details>

#### Categorizer>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	categoryArray _ categoryArray copy.
	categoryStops _ categoryStops copy. 
	elementArray _ elementArray copy
</details>

#### Categorizer>>#classifyAll: aCollection under: heading

<details>
	<summary>See more</summary>
	
	classifyAll: aCollection under: heading

	aCollection do:
		[:element | self classify: element under: heading]
</details>

#### Categorizer>>#listAtCategoryNamed: categoryName

Answer the array of elements associated with the name, categoryName.


<details>
	<summary>See more</summary>
	
	listAtCategoryNamed: categoryName
	"Answer the array of elements associated with the name, categoryName."

	| i |
	i _ categoryArray indexOf: categoryName ifAbsent: [^Array new].
	^self listAtCategoryNumber: i
</details>

#### Categorizer>>#classify: element under: heading

<details>
	<summary>See more</summary>
	
	classify: element under: heading 
	self classify: element under: heading suppressIfDefault: true
</details>

#### Categorizer>>#firstIndexOfCategoryNumber: anInteger

<details>
	<summary>See more</summary>
	
	firstIndexOfCategoryNumber: anInteger
	anInteger < 1 ifTrue: [^ nil].
	^ (anInteger > 1
			ifTrue: [(categoryStops at: anInteger - 1) + 1]
			ifFalse: [1]).
</details>

#### Categorizer>>#hasAnyCategoriesSuchThat: aBlock

Answer an Array of categories (names).


<details>
	<summary>See more</summary>
	
	hasAnyCategoriesSuchThat: aBlock
	"Answer an Array of categories (names)."
	categoryArray ifNil: [^ false].
	(categoryArray size = 1 
		and: [categoryArray first = Default & (elementArray size = 0)])
		ifTrue: [^false].
	^categoryArray anySatisfy: aBlock
</details>

#### Categorizer>>#setDefaultList: anArray

<details>
	<summary>See more</summary>
	
	setDefaultList: anArray

	categoryArray _ Array with: Default.
	categoryStops _ Array with: anArray size.
	elementArray _ anArray
</details>

#### Categorizer>>#removeElement: element

Remove the selector, element, from all categories.


<details>
	<summary>See more</summary>
	
	removeElement: element 
	"Remove the selector, element, from all categories."
	| categoryIndex elementIndex nextStop newElements |
	categoryIndex _ 1.
	elementIndex _ 0.
	nextStop _ 0.
	"nextStop keeps track of the stops in the new element array"
	newElements _ WriteStream on: (Array new: elementArray size).
	[(elementIndex _ elementIndex + 1) <= elementArray size]
		whileTrue: 
			[[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: 
					[categoryStops at: categoryIndex put: nextStop.
					categoryIndex _ categoryIndex + 1].
			(elementArray at: elementIndex) = element
				ifFalse: 
					[nextStop _ nextStop + 1.
					newElements nextPut: (elementArray at: elementIndex)]].
	[categoryIndex <= categoryStops size]
		whileTrue: 
			[categoryStops at: categoryIndex put: nextStop.
			categoryIndex _ categoryIndex + 1].
	elementArray _ newElements contents
</details>

#### Categorizer>>#moveCategoryTop: catString

<details>
	<summary>See more</summary>
	
	moveCategoryTop: catString
	| category newCategories |
	category _ catString asSymbol.
	newCategories _ (Array with: category), (categoryArray copyWithout: category).
	self categories: newCategories
</details>

## Class

I add a number of facilities to those in ClassDescription: A set of all my subclasses (defined in ClassDescription, but only used here and below) A name by which I can be found in a SystemDictionary A classPool for class variables shared between this class and its metaclass A list of sharedPools which probably should be supplanted by some better mechanism. My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription. The slot 'subclasses' is a redundant structure. It is never used during execution, but is used by the development system to simplify or speed certain operations.

### Methods
#### Class>>#bindingOf: varName

Answer the binding of some variable resolved in the scope of the receiver


<details>
	<summary>See more</summary>
	
	bindingOf: varName
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol |
	aSymbol _ varName asSymbol.

	"First look in receiver, and up the hierarchy. Inherited variables take precedence over globals."
	(self localBindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in globals."
	(Smalltalk bindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Fail at the end."
	^nil
</details>

#### Class>>#classPool: aDictionary

<details>
	<summary>See more</summary>
	
	classPool: aDictionary
	classPool := aDictionary
</details>

#### Class>>#testCaseClass

<details>
	<summary>See more</summary>
	
	testCaseClass
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass _ Smalltalk classNamed: self name, 'Test'.
	
	^potentialTestCaseClass 
	
 
</details>

#### Class>>#subclasses

Answer a Set containing the receiver's subclasses.


<details>
	<summary>See more</summary>
	
	subclasses
	"Answer a Set containing the receiver's subclasses."

	^ subclasses
		ifNil: [#()]
		ifNotNil: [subclasses copy]
</details>

#### Class>>#subclassesDoGently: aBlock

Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.


<details>
	<summary>See more</summary>
	
	subclassesDoGently: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	subclasses
		ifNotNil: [subclasses do: aBlock]
</details>

#### Class>>#safeRenameTo: newName

<details>
	<summary>See more</summary>
	
	safeRenameTo: newName

	| oldName |
	
	oldName := name.
	Smalltalk prepareToRenameClass: self as: newName.
	name _ newName.
	Smalltalk renamedClass: self from: oldName
</details>

#### Class>>#allSharedPools

Answer a Set of the pools the receiver shares, including those defined in the superclasses of the receiver.


<details>
	<summary>See more</summary>
	
	allSharedPools
	"Answer a Set of the pools the receiver shares, including those defined  
	in the superclasses of the receiver."
	| aSet | 
	^ superclass
		ifNil: [self sharedPools copy]
		ifNotNil: [
			aSet _ superclass allSharedPools.
			aSet addAll: self sharedPools.
			aSet]
</details>

#### Class>>#variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable pointer variables.


<details>
	<summary>See more</summary>
	
	variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable pointer variables."
	
	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Class>>#allClassVarNames

Answer a Set of the names of the receiver's class variables, including those defined in the superclasses of the receiver.


<details>
	<summary>See more</summary>
	
	allClassVarNames
	"Answer a Set of the names of the receiver's class variables, including those
	defined in the superclasses of the receiver."

	| aSet |
	superclass
		ifNil: [^ self classVarNames].	 "This is the keys so it is a new Set."
	aSet _ superclass allClassVarNames.
	aSet addAll: self classVarNames.
	^ aSet
</details>

#### Class>>#copy

Answer a copy of the receiver without a list of subclasses.


<details>
	<summary>See more</summary>
	
	copy 
	"Answer a copy of the receiver without a list of subclasses."
	| newClass |
	newClass := self class copy new
		superclass: superclass
		methodDict: self methodDict copy
		format: format
		name: name
		organization: self organization copy
		instVarNames: instanceVariables copy
		classPool: classPool copy
		sharedPools: sharedPools copy.
	Class instSize+1 to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	^ newClass
</details>

#### Class>>#removeFromSystem

Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.


<details>
	<summary>See more</summary>
	
	removeFromSystem
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	self removeFromSystem: true.
</details>

#### Class>>#variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable byte-sized nonpointer variables.


<details>
	<summary>See more</summary>
	
	variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	
	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableByteSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

</details>

#### Class>>#shouldFileOutPool: aPoolName

respond with true if the user wants to file out aPoolName


<details>
	<summary>See more</summary>
	
	shouldFileOutPool: aPoolName
	"respond with true if the user wants to file out aPoolName"
	^self confirm: ('FileOut the sharedPool ', aPoolName, '?')
</details>

#### Class>>#sharing: poolString

Set up sharedPools. Answer whether recompilation is advisable.


<details>
	<summary>See more</summary>
	
	sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Scanner new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(PoolDefinitionNotification signalNamed: poolName)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]
</details>

#### Class>>#addInstVarName: aString

Add the argument, aString, as one of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	addInstVarName: aString
	"Add the argument, aString, as one of the receiver's instance variables."
	
	| answer |
	answer _ ClassBuilder new
		name: self name
		subclassOf: superclass
		type: self typeOfClass
		instanceVariableNames: self instanceVariablesString, ' ', aString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

</details>

#### Class>>#removeFromSystemUnlogged

Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver. Do not log the removal either to the current change set nor to the system changes log


<details>
	<summary>See more</summary>
	
	removeFromSystemUnlogged
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"
	^self removeFromSystem: false
</details>

#### Class>>#storeDataOn: aDataStream

I don't get stored. Use a DiskProxy


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store a Class'
</details>

#### Class>>#fileOut

File a description of the receiver onto a new file whose base name is the name of the receiver.


<details>
	<summary>See more</summary>
	
	fileOut
	"File a description of the receiver onto a new file whose base name is the name of the receiver."

	DirectoryEntry smalltalkImageDirectory // (self name, '.st') writeStreamDo: [ :stream |
		stream timeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: stream ]].
		self fileOutOn: stream moveSource: false toFile: 0 ]
</details>

#### Class>>#removeSubclass: aSubclass

If the argument, aSubclass, is one of the receiver's subclasses, remove it.


<details>
	<summary>See more</summary>
	
	removeSubclass: aSubclass 
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."

	subclasses
		ifNotNil: [
			subclasses _ subclasses copyWithout: aSubclass.
			subclasses isEmpty
				ifTrue: [subclasses _ nil]]
</details>

#### Class>>#unload

Sent when a the class is removed. Does nothing, but may be overridden by (class-side) subclasses.


<details>
	<summary>See more</summary>
	
	unload
	"Sent when a the class is removed.  Does nothing, but may be overridden by (class-side) subclasses."
	""

</details>

#### Class>>#subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver).


<details>
	<summary>See more</summary>
	
	subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."
	
	| answer |
	answer _ ClassBuilder new
		superclass: self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

</details>

#### Class>>#spaceUsed

Object spaceUsed


<details>
	<summary>See more</summary>
	
	spaceUsed

	"Object spaceUsed"
	^ super spaceUsed + self class spaceUsed
</details>

#### Class>>#superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet

Answer an instance of me, a new class, using the arguments of the message as the needed information. Must only be sent to a new instance; else we would need Object flushCache.


<details>
	<summary>See more</summary>
	
	superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet 
	"Answer an instance of me, a new class, using the arguments of the 
	message as the needed information.
	Must only be sent to a new instance; else we would need Object flushCache."

	superclass _ sup.
	methodDict _ md.
	format _ ft.
	name _ nm.
	instanceVariables _ nilOrArray.
	classPool _ pool.
	sharedPools _ poolSet.
	self organization: org.
</details>

#### Class>>#addClassVarName: aString

Add the argument, aString, as a class variable of the receiver. Signal an error if the first character of aString is not capitalized, or if it is already a variable named in the class.


<details>
	<summary>See more</summary>
	
	addClassVarName: aString 
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	| symbol oldState |
	oldState _ self copy.
	aString first isLowercase
		ifTrue: [^self error: aString, ' class variable name should be capitalized; proceed to include anyway.'].
	symbol _ aString asSymbol.
	self withAllSubclasses do: 
		[:subclass | 
		(subclass bindingOf: symbol) ifNotNil:[
			^ self error: aString 
				, ' is already used as a variable name in class ' 
				, subclass name]].
	classPool ifNil: [classPool _ Dictionary new].
	(classPool includesKey: symbol) ifFalse: [
		"Pick up any refs in Undeclared"
		classPool declare: symbol from: Undeclared.
		SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: oldState to: self]
</details>

#### Class>>#fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool

File a description of the receiver on aFileStream. If the boolean argument, moveSource, is true, then set the trailing bytes to the position of aFileStream and to fileIndex in order to indicate where to find the source code.


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex
				initializing: aBool]
</details>

#### Class>>#fileOutInitializerOn: aStream

<details>
	<summary>See more</summary>
	
	fileOutInitializerOn: aStream
	^self class fileOutInitializerOn: aStream
</details>

#### Class>>#sharedPools

Answer a Set of the pool dictionaries declared in the receiver.


<details>
	<summary>See more</summary>
	
	sharedPools
	"Answer a Set of the pool dictionaries declared in the receiver."

	^sharedPools
		ifNil: [OrderedCollection new]
</details>

#### Class>>#name

Answer the name of the receiver.


<details>
	<summary>See more</summary>
	
	name
	"Answer the name of the receiver."

	^name ifNil: [super name]
</details>

#### Class>>#variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable double-byte-sized (16 bits) nonpointer variables.


<details>
	<summary>See more</summary>
	
	variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-byte-sized (16 bits) nonpointer variables."
	"Note: Only for Spur images"
	
	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableDoubleByteSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

</details>

#### Class>>#immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new immediate class as a subclass of an existing class (the receiver). In Spur, use no ivars and make it really immediate. In preSpur, use supplied ivars, and make it non-immediate. Any classes created like this should also implement #definition. See Character, SmallInteger and SmallFloat64


<details>
	<summary>See more</summary>
	
	immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new
	 immediate class as a subclass of an existing class (the receiver).
	
	In Spur, use no ivars and make it really immediate.
	In preSpur, use supplied ivars, and make it non-immediate.
	
	Any classes created like this should also implement #definition.
	See Character, SmallInteger and SmallFloat64"
	^Smalltalk isSpur ifTrue: [
		ClassBuilder new
			superclass: self
			immediateSubclass: t
			instanceVariableNames: ''
			classVariableNames: d
			poolDictionaries: s
			category: cat ]
	ifFalse: [
		ClassBuilder new
			superclass: self
			subclass: t
			instanceVariableNames: f
			classVariableNames: d
			poolDictionaries: s
			category: cat ]
</details>

#### Class>>#reformatAll

Reformat all methods in this class. Leaves old code accessible to version browsing


<details>
	<summary>See more</summary>
	
	reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	super reformatAll.		"me..."
	self class reformatAll	"...and my metaclass"
</details>

#### Class>>#removeSharedPool: aDictionary

Remove the pool dictionary, aDictionary, as one of the receiver's pool dictionaries. Create an error notification if the dictionary is not one of the pools. : Note that it removes the wrong one if there are two empty Dictionaries in the list.


<details>
	<summary>See more</summary>
	
	removeSharedPool: aDictionary 
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool 
	dictionaries. Create an error notification if the dictionary is not one of 
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |
	(self sharedPools includes: aDictionary)
		ifFalse: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [:class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [sharedPools remove: aDictionary.
				sharedPools isEmpty ifTrue: [sharedPools _ nil].
				^self]. 

	"second get all the subclasses that reference aDictionary through me rather than a 
	superclass that is one of my subclasses."

	workingSet _ self subclasses asOrderedCollection.
	satisfiedSet _ Set new.
	[workingSet isEmpty] whileFalse:
		[aSubclass _ workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse: 
				[satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because 
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: 
		[:sub | 
		aDictionary associationsDo: 
			[:aGlobal | 
			(sub whichSelectorsReferTo: aGlobal) isEmpty 
				ifFalse: [^self error: aGlobal key 
								, ' is still used in code of class '
								, sub name]]].
	sharedPools remove: aDictionary.
	sharedPools isEmpty ifTrue: [sharedPools _ nil]
</details>

#### Class>>#rename: aString

The new name of the receiver is the argument, aString.


<details>
	<summary>See more</summary>
	
	rename: aString
	"The new name of the receiver is the argument, aString."

	| newName |
	(newName _ aString asSymbol) ~= self name
		ifFalse: [^ self].
	(Smalltalk includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	(Undeclared includesKey: newName)
		ifTrue: [self inform: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.'].

	self safeRenameTo: newName.
</details>

#### Class>>#removeInstVarName: aString

Remove the argument, aString, as one of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."

	| newInstVarString answer |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newInstVarString _ ''.
	(self instVarNames copyWithout: aString) do: 
		[:varName | newInstVarString _ newInstVarString , ' ' , varName].
	answer _ ClassBuilder new
		name: self name
		subclassOf: superclass
		type: self typeOfClass
		instanceVariableNames: newInstVarString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Class>>#classVarNames

Answer a Set of the names of the class variables defined in the receiver.


<details>
	<summary>See more</summary>
	
	classVarNames
	"Answer a Set of the names of the class variables defined in the receiver."

	^self classPool keys asSet
</details>

#### Class>>#superclass: aClass methodDictionary: mDict format: fmt

Basic initialization of the receiver


<details>
	<summary>See more</summary>
	
	superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	subclasses _ nil. "Important for moving down the subclasses field into Class"

</details>

#### Class>>#obsolete

Change the receiver and all of its subclasses to an obsolete class.


<details>
	<summary>See more</summary>
	
	obsolete
	"Change the receiver and all of its subclasses to an obsolete class."
	self == Object 
		ifTrue:[^self error:'Object is NOT obsolete'].
	name _ 'AnObsolete' , name.
	Object class instSize + 1 to: self class instSize do:
		[:i | self instVarAt: i put: nil]. "Store nil over class instVars."
	classPool _ nil.
	sharedPools _ nil.
	self class obsolete.
	super obsolete.

</details>

#### Class>>#definitionReplacingCategoryWith: aNewCategory

<details>
	<summary>See more</summary>
	
	definitionReplacingCategoryWith: aNewCategory

	| definition categoryDefinitionIndex currentCategoryDefinition definitionWithNewCategory |
	
	definition := self definition.
	"category can be nil, that is why I sent asString to it - Hernan"
	currentCategoryDefinition := 'category: ''', self category asString, ''''.
	categoryDefinitionIndex := definition 
		indexOfSubCollection: currentCategoryDefinition 
		startingAt: 1
		ifAbsent: [ self error: 'Definition of category not found!' ].
	
	definitionWithNewCategory := definition first: categoryDefinitionIndex - 1.
	definitionWithNewCategory := definitionWithNewCategory, 'category: ''', aNewCategory asString, ''''.
	
	^definitionWithNewCategory		
</details>

#### Class>>#addSubclass: aSubclass

Make the argument, aSubclass, be one of the subclasses of the receiver. Create an error notification if the argument's superclass is not the receiver.


<details>
	<summary>See more</summary>
	
	addSubclass: aSubclass 
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."
	
	aSubclass superclass ~~ self 
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		subclasses _ Array with: aSubclass.
		^self].
	subclasses do:[:cl| cl == aSubclass ifTrue:[^self]]. "Already my subclass"
	subclasses _ subclasses copyWith: aSubclass.
</details>

#### Class>>#fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex

File a description of the receiver on aFileStream. If the boolean argument, moveSource, is true, then set the trailing bytes to the position of aFileStream and to fileIndex in order to indicate where to find the source code.


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."
	^self fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: true
</details>

#### Class>>#shouldFileOutPools

respond with true if the user wants to file out the shared pools


<details>
	<summary>See more</summary>
	
	shouldFileOutPools
	"respond with true if the user wants to file out the shared pools"
	^self confirm: 'FileOut selected sharedPools?'
</details>

#### Class>>#compileAllFrom: oldClass

Recompile all the methods in the receiver's method dictionary (not the subclasses). Also recompile the methods in the metaclass.


<details>
	<summary>See more</summary>
	
	compileAllFrom: oldClass
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self class compileAllFrom: oldClass class
</details>

#### Class>>#subclassesDo: aBlock

Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.


<details>
	<summary>See more</summary>
	
	subclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	subclasses
		ifNotNil: [subclasses do: aBlock]
</details>

#### Class>>#declare: varString

Declare class variables common to all instances. Answer whether recompilation is advisable.


<details>
	<summary>See more</summary>
	
	declare: varString 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."

	| newVars conflicts |
	newVars _ 
		(Scanner new scanFieldNames: varString)
			collect: [:x | x asSymbol].
	newVars do: [:var | 
		var first isLowercase
			ifTrue: [self error: var, ' class variable name should be capitalized; proceed to include anyway.']].
	conflicts _ false.
	classPool
		ifNotNil: [
			(classPool keys reject: [:x | newVars includes: x]) 
				do: [:var | self removeClassVarName: var]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var , ' is defined elsewhere'.
					conflicts _ true]].
	newVars size > 0
		ifTrue: [
			classPool _ self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared]].
	^conflicts
</details>

#### Class>>#setName: aSymbol

Private - set the name of the class


<details>
	<summary>See more</summary>
	
	setName: aSymbol
	"Private - set the name of the class"
	name _ aSymbol.
</details>

#### Class>>#binding

<details>
	<summary>See more</summary>
	
	binding

	^ Smalltalk associationAt: name ifAbsent: [nil -> self]
</details>

#### Class>>#variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable double-word-sized (64 bits) nonpointer variables.


<details>
	<summary>See more</summary>
	
	variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-word-sized (64 bits) nonpointer variables."
	"Note: Only for Spur images"

	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableDoubleWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Class>>#variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable word-sized nonpointer variables.


<details>
	<summary>See more</summary>
	
	variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable word-sized nonpointer variables."

	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Class>>#classPool

Answer the dictionary of class variables.


<details>
	<summary>See more</summary>
	
	classPool
	"Answer the dictionary of class variables."

	^classPool
		ifNil: [Dictionary new]
</details>

#### Class>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a reference to a class in Smalltalk instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."

	^ DiskProxy global: self theNonMetaClass name selector: #yourself args: #()
</details>

#### Class>>#addSharedPool: aSharedPool

Add the argument, aSharedPool, as one of the receiver's shared pools. Create an error if the shared pool is already one of the pools. This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses


<details>
	<summary>See more</summary>
	
	addSharedPool: aSharedPool 
	"Add the argument, aSharedPool, as one of the receiver's shared pools. 
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"

	(self sharedPools includes: aSharedPool)
		ifTrue: [^self error: 'This is already in my shared pool list'].
	sharedPools
		ifNil: [sharedPools _ OrderedCollection with: aSharedPool]
		ifNotNil: [sharedPools add: aSharedPool]
</details>

#### Class>>#isObsolete

Return true if the receiver is obsolete.


<details>
	<summary>See more</summary>
	
	isObsolete
	"Return true if the receiver is obsolete."
	^(Smalltalk at: name ifAbsent: nil) ~~ self
</details>

#### Class>>#localBindingOf: varNameSymbol

Answer the binding of some variable resolved in the scope of the receiver.


<details>
	<summary>See more</summary>
	
	localBindingOf: varNameSymbol
	"Answer the binding of some variable resolved in the scope of the receiver."

	"First look in classVar dictionary."
	(self classPool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		(pool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].
	].

	"Finally look higher up the superclass chain and fail at the end."
	^superclass ifNotNil: [ superclass localBindingOf: varNameSymbol ]
</details>

#### Class>>#hasMethods

Answer a Boolean according to whether any methods are defined for the receiver (includes whether there are methods defined in the receiver's metaclass).


<details>
	<summary>See more</summary>
	
	hasMethods
	"Answer a Boolean according to whether any methods are defined for the 
	receiver (includes whether there are methods defined in the receiver's 
	metaclass)."

	^super hasMethods or: [self class hasMethods]
</details>

#### Class>>#weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables.


<details>
	<summary>See more</summary>
	
	weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have weak indexable pointer variables."

	| answer |
	answer _ ClassBuilder new
		superclass: self
		weakSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Class>>#removeFromSystem: logged

Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.


<details>
	<summary>See more</summary>
	
	removeFromSystem: logged
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	
	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to unload itself"
	self unload.
	self superclass ifNotNil:
		["If we have no superclass there's nothing to be remembered"
		self superclass addObsoleteSubclass: self].
	Smalltalk forgetClass: self logged: logged.
	self obsolete.
</details>

#### Class>>#possibleVariablesFor: misspelled continuedFrom: oldResults

<details>
	<summary>See more</summary>
	
	possibleVariablesFor: misspelled continuedFrom: oldResults

	| results |
	results _ misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
	self sharedPools do: [:pool | 
		results _ misspelled correctAgainstDictionary: pool continuedFrom: results ].
	^ superclass
		ifNil: [misspelled correctAgainstDictionary: Smalltalk continuedFrom: results]
		ifNotNil: [superclass possibleVariablesFor: misspelled continuedFrom: results]
</details>

#### Class>>#ensureClassPool

<details>
	<summary>See more</summary>
	
	ensureClassPool

	classPool ifNil: [classPool _ Dictionary new].
</details>

#### Class>>#fileOutSharedPoolsOn: aFileStream

file out the shared pools of this class after prompting the user about each pool


<details>
	<summary>See more</summary>
	
	fileOutSharedPoolsOn: aFileStream
	"file out the shared pools of this class after prompting the user about each pool"
	| poolsToFileOut |
	poolsToFileOut _ self sharedPools select: 
		[:aPool | (self shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool))].
	poolsToFileOut do: [:aPool | self fileOutPool: aPool onFileStream: aFileStream].
	
</details>

#### Class>>#fileOutPool: aPool onFileStream: aFileStream

<details>
	<summary>See more</summary>
	
	fileOutPool: aPool onFileStream: aFileStream 

	| aPoolName |
	(aPool  isKindOf: SharedPool class) ifTrue:[^self notify: 'we do not fileout SharedPool type shared pools for now'].
	aPoolName _ Smalltalk keyAtIdentityValue: aPool.
	Transcript newLine; show: aPoolName.
	aFileStream nextPutAll: 'Transcript show: ''' , aPoolName , '''; newLine!'; newLine.
	aFileStream nextPutAll: 'Smalltalk at: #' , aPoolName , ' put: Dictionary new!'; newLine.
	aPool keys sort do: [ :aKey | | aValue |
		aValue _ aPool at: aKey.
		aFileStream nextPutAll: aPoolName , ' at: #''' , aKey asString , '''', ' put:  '.
		aValue isNumber
			ifTrue: [ aValue printOn: aFileStream ]
			ifFalse: [
				aFileStream nextPutAll: '('.
				aValue printOn: aFileStream.
				aFileStream nextPutAll: ')' ].
		aFileStream nextPutAll: '!'; newLine ].
	aFileStream newLine
</details>

#### Class>>#removeClassVarName: aString

Remove the class variable whose name is the argument, aString, from the names defined in the receiver, a class. Create an error notification if aString is not a class variable or if it is still being used in the code of the class.


<details>
	<summary>See more</summary>
	
	removeClassVarName: aString 
	"Remove the class variable whose name is the argument, aString, from 
	the names defined in the receiver, a class. Create an error notification if 
	aString is not a class variable or if it is still being used in the code of 
	the class."

	| aSymbol |
	aSymbol _ aString asSymbol.
	(classPool includesKey: aSymbol)
		ifFalse: [ ^self error: aString, ' is not a class variable'].
	self withAllSubclasses do:[:subclass |
		(Array with: subclass with: subclass class) do: [ :classOrMeta |
			(classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
				isEmpty ifFalse: [
					InMidstOfFileinNotification signal ifTrue: [
						Transcript newLine; show: self name, ' (' , aString , ' is Undeclared) '.
						^Undeclared declare: aSymbol from: classPool ].
					(self confirm: (aString,' is still used in code of class ', classOrMeta name,
						'.\Is it okay to move it to Undeclared?') withNewLines)
						ifTrue: [ ^Undeclared declare: aSymbol from: classPool ]
						ifFalse: [ ^self ]]]].
	classPool removeKey: aSymbol.
	classPool isEmpty ifTrue: [ classPool _ nil ]
</details>

#### Class>>#classPoolFrom: aClass

share the classPool with aClass.


<details>
	<summary>See more</summary>
	
	classPoolFrom: aClass
	"share the classPool with aClass."

	classPool := aClass classPool
</details>

## ClassBuilder

Responsible for creating a new class or changing the format of an existing class (from a class definition in a browser or a fileIn). This includes validating the definition, computing the format of instances, creating or modifying the accompanying Metaclass, setting up the class and metaclass objects themselves, registering the class as a global, recompiling methods, modifying affected subclasses, mutating existing instances to the new format, and more. You typically only need to use or modify this class, or even know how it works, when making fundamental changes to how the Smalltalk system and language works. Implementation notes: ClassBuilder relies on the assumption that it can see ALL subclasses of some class. If there are any existing subclasses of some class, regardless of whether they have instances or not, regardless of whether they are considered obsolete or not, ClassBuilder MUST SEE THEM.

### Methods
#### ClassBuilder>>#recordClass: oldClass replacedBy: newClass

Keep the changes up to date when we're moving instVars around


<details>
	<summary>See more</summary>
	
	recordClass: oldClass replacedBy: newClass
	"Keep the changes up to date when we're moving instVars around"
	(instVarMap includesKey: oldClass name) ifTrue:[
		SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: oldClass to: newClass.
	].
</details>

#### ClassBuilder>>#doesClassNameStartWithUppercase: aClassName

<details>
	<summary>See more</summary>
	
	doesClassNameStartWithUppercase: aClassName
	
	^ aClassName first isUppercase
</details>

#### ClassBuilder>>#newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass

Create a new subclass of the given superclass with the given specification.


<details>
	<summary>See more</summary>
	
	newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
	"Create a new subclass of the given superclass with the given specification."
	| newFormat newClass |
	"Compute the format of the new class"
	newFormat _ 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper 
			ccIndex: (oldClass ifNil:[0] ifNotNil:[oldClass indexIfCompact]).

	newFormat
		ifNil: [^ nil].

	(oldClass == nil or:[oldClass isMeta not]) 
		ifTrue:[newClass _ self privateNewSubclassOf: newSuper from: oldClass]
		ifFalse:[newClass _ oldClass shallowCopy].

	newClass 
		superclass: newSuper
		methodDictionary: MethodDictionary new
		format: newFormat;
		setInstVarNames: instVars.

	oldClass ifNotNil:[
		newClass organization: oldClass organization.
		"Recompile the new class"
		oldClass hasMethods 
			ifTrue:[newClass compileAllFrom: oldClass].
		self recordClass: oldClass replacedBy: newClass.
	].

	(oldClass == nil or:[oldClass isObsolete not]) 
		ifTrue:[newSuper addSubclass: newClass]
		ifFalse:[newSuper addObsoleteSubclass: newClass].

	^newClass
</details>

#### ClassBuilder>>#validateClassvars: classVarArray from: oldClass forSuper: newSuper

Check if any of the classVars of oldClass conflict with the new superclass


<details>
	<summary>See more</summary>
	
	validateClassvars: classVarArray from: oldClass forSuper: newSuper
	"Check if any of the classVars of oldClass conflict with the new superclass"
	| usedNames classVars temp |
	classVarArray isEmpty ifTrue:[^true]. "Okay"

	"Validate the class var names"
	usedNames _ classVarArray asSet.
	usedNames size = classVarArray size 
		ifFalse:[	classVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp _ var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp _ var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames _ newSuper allClassVarNames asSet.
		classVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl classVarNames includes: iv) ifTrue:[temp _ cl]].
				self error: iv, ' is already defined in ', temp name.
				^false]]].

	oldClass ifNotNil: [
		usedNames _ Set new: 20.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl classVarNames].
		classVars _ classVarArray.
		newSuper ifNotNil: [ classVars _ classVars, newSuper allClassVarNames asArray].
		classVars do: [ :iv |
			(usedNames includes: iv) ifTrue: [
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true
</details>

#### ClassBuilder>>#name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe

Define a new class. If unsafe is true do not run any validation checks. This facility is provided to implement important system changes.


<details>
	<summary>See more</summary>
	
	name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Scanner new scanFieldNames: instVarString.
	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category asSymbol.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: category].
	^newClass
</details>

#### ClassBuilder>>#update: oldClass to: newClass

Convert oldClass, all its instances and possibly its meta class into newClass, instances of newClass and possibly its meta class. The process is surprisingly simple in its implementation and surprisingly complex in its nuances and potentially bad side effects. We can rely on two assumptions (which are critical): #1: The method #updateInstancesFrom: will not create any lasting pointers to 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do a become of the old vs. the new instances and therefore it will not create pointers to *new* instances before the #become: which are *old* afterwards) #2: The non-preemptive execution of the critical piece of code guarantees that nobody can get a hold by 'other means' (such as process interruption and reflection) on the old instances. Given the above two, we know that after #updateInstancesFrom: there are no pointers to any old instances. After the forwarding become there will be no pointers to the old class or meta class either. <preSpur> Meaning that if we throw in a nice fat GC at the end of the critical block, everything will be gone (but see the comment right there). </preSpur> Andreas Raab, 2/27/2003 23:42


<details>
	<summary>See more</summary>
	
	update: oldClass to: newClass
	"Convert oldClass, all its instances and possibly its meta class into newClass,
	 instances of newClass and possibly its meta class. The process is surprisingly
	 simple in its implementation and surprisingly complex in its nuances and potentially
	 bad side effects.
	 We can rely on two assumptions (which are critical):
		#1: The method #updateInstancesFrom: will not create any lasting pointers to
			 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do
			 a become of the old vs. the new instances and therefore it will not create
			 pointers to *new* instances before the #become: which are *old* afterwards)
		#2: The non-preemptive execution of the critical piece of code guarantees that
			 nobody can get a hold by 'other means' (such as process interruption and
			 reflection) on the old instances.
	 Given the above two, we know that after #updateInstancesFrom: there are no pointers
	 to any old instances. After the forwarding become there will be no pointers to the old
	 class or meta class either.
	<preSpur> Meaning that if we throw in a nice fat GC at the end of the critical block, everything will 
	be gone (but see the comment right there). </preSpur> 
	 Andreas Raab, 2/27/2003 23:42"
	| meta |
	meta := oldClass isMeta.
	"Note: Everything from here on will run without the ability to get interrupted
	to prevent any other process to create new instances of the old class."
	["Note: The following removal may look somewhat obscure and needs an explanation.
	  When we mutate the class hierarchy we create new classes for any existing subclass.
	  So it may look as if we don't have to remove the old class from its superclass. However,
	  at the top of the hierarchy (the first class we reshape) that superclass itself is not newly
	  created so therefore it will hold both the oldClass and newClass in its (obsolete or not)
	  subclasses. Since the #become: below will transparently replace the pointers to oldClass
	  with newClass the superclass would have newClass in its subclasses TWICE. With rather
	  unclear effects if we consider that we may convert the meta-class hierarchy itself (which
	  is derived from the non-meta class hierarchy).
	  Due to this problem ALL classes are removed from their superclass just prior to converting
	  them. Here, breaking the superclass/subclass invariant really doesn't matter since we will
	  effectively remove the oldClass 
	(<spur>becomeForward:</spur> or <preSpur>become+GC</preSpur>) just a few lines below."

		"Convert the instances of oldClass into instances of newClass"
		newClass updateInstancesFrom: oldClass.

		oldClass superclass removeSubclass: oldClass.
		oldClass superclass removeObsoleteSubclass: oldClass.

		"make sure that the VM cache is clean"
		oldClass methodDict do: [:cm | cm flushCache].

		meta
			ifTrue:
				[oldClass becomeForward: newClass.
				 oldClass updateMethodBindingsTo: oldClass binding]
			ifFalse:
				[{oldClass. oldClass class} elementsForwardIdentityTo: {newClass. newClass class}.
				 oldClass updateMethodBindingsTo: oldClass binding.
				 oldClass class updateMethodBindingsTo: oldClass class binding].

 Smalltalk isSpur
	ifTrue: [
		"eem 5/31/2014 07:22 At this point there used to be a garbage collect whose purpose was
		 to ensure no old instances existed after the becomeForward:.  Without the GC it was possible
		 to resurrect old instances using e.g. allInstancesDo:.  This was because the becomeForward:
		 updated references from the old objects to new objects but didn't destroy the old objects.
		 But as of late 2013/early 2014 becomeForward: has been modified to free all the old objects."
		]

	ifFalse: [
		"jmv: Squeak 4.6 (pre Spur) includes the GC. So, do it if not Spur.
		Not really sure if needed on newer Cog and Stack non-Spur VMs.
		Not sure if needed for SqueakJS.
		Remove it when we are sure.
		
		Original note by Andreas Raab below."
		Smalltalk garbageCollect.
		"Warning: Read this before you even think about removing the GC. Yes, it slows us down. Quite heavily if you have a large image. However, there's no good and simple alternative here, since unfortunately, #become: does change class pointers. What happens is that after the above become all of the instances of the old class will have a class pointer identifying them as instances of newClass. If we get our hands on any of these instances we will break immediately since their expected instance layout (that of its class, e.g., newClass) will not match their actual instance layout (that of oldClass). And getting your hands on any of those instances is really simple - just reshaping one class two times in rapid succession will do it. Reflection techniques, interrupts, etc. will only add to this problem. In the case of Metaclass things get even worse since when we recompile the entire class hierarchy we will recompile both, Metaclass and its instances (and some of its instances will have the old and some the new layout).
		The only easy solution to this problem would be to 'fix up' the class pointers of the old instances to point to the old class (using primitiveChangeClassTo:). But this won't work either - as we do a one-way become we would have to search the entire object memory for the oldClass and couldn't even clearly identify it unless we give it some 'special token' which sounds quite error-prone. If you really need to get rid of the GC here are some alternatives:
		On the image level, one could create a copy of the oldClass before becoming it into the new class and, after becoming it, 'fix up' the old instances. That would certainly work but it sounds quite complex, as we need to make sure we're not breaking any of the superclass/subclass meta/non-meta class variants.
		Alternatively, fix up #becomeForward on the VM-level to 'dump the source objects' of #become. This would be quite doable (just 'convert' them into a well known special class such as bitmap) yet it has problems if (accidentally or not) one of the objects in #become: appears on 'both sides of the fence' (right now, this will work ... in a way ... even though the consequences are unclear).
		Another alternative is to provide a dedicated primitive for this (instead of using it implicitly in become) which would allow us to dump all the existing instances right here. This is equivalent to a more general primitiveChangeClassTo: and might be worthwhile but it would likely have to keep in mind the differences between bits and pointer thingies etc.
		Since all of the alternatives seem rather complex and magical compared to a straight-forward GC it seems best to stick with the GC solution for now. If someone has a real need to fix this problem, that person will likely be motivated enough to check out the alternatives. Personally I'd probably go for #1 (copy the old class and remap the instances to it) since it's a solution that could be easily reverted from within the image if there's any problem with it.
		 Andreas Raab, 2/27/2003 23:42"
		]
	] valueUnpreemptively
</details>

#### ClassBuilder>>#recompile: force from: oldClass to: newClass mutate: forceMutation

Do the necessary recompilation after changing oldClass to newClass. If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass and all its subclasses. If forceMutation is true force a mutation even if oldClass and newClass are the same.


<details>
	<summary>See more</summary>
	
	recompile: force from: oldClass to: newClass mutate: forceMutation
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex _ 0.
	maxClassIndex _ oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo:[:cl|
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].
	"Recompile and mutate oldClass to newClass"
	self informUserDuring:[
		self mutate: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"
</details>

#### ClassBuilder>>#validateClass: srcClass forMoving: iv upTo: dstClass

Make sure we don't have this instvar already


<details>
	<summary>See more</summary>
	
	validateClass: srcClass forMoving: iv upTo: dstClass
	"Make sure we don't have this instvar already"
	dstClass withAllSubclassesDo:[:cls|
		(cls == srcClass or:[cls inheritsFrom: srcClass]) ifFalse:[
			cls isPointers ifFalse:[
				self error: dstClass name, ' cannot have instance variables'.
				^false].
			cls instSize >= 254 ifTrue:[
				self error: cls name, ' has more than 254 instance variables'.
				^false].
			(cls instVarNames includes: iv) ifTrue:[
				self notify: (iv printString asText allBold),' is defined in ', cls name asText allBold,'
Proceed to move it up to ', dstClass name asText allBold,' as well'.
				instVarMap at: cls name put: (cls instVarNames copyWithout: iv)].
		].
	].
	^true
</details>

#### ClassBuilder>>#validateClassName: aString

Validate the new class name


<details>
	<summary>See more</summary>
	
	validateClassName: aString
	"Validate the new class name"
	
	(self doesClassNameStartWithUppercase: aString) ifFalse:[
		self error: 'Class names must be capitalized'.
		^false].
	Smalltalk at: aString ifPresent:[:old|
		(old isKindOf: Behavior) ifFalse:[
			self notify: aString asText allBold, 
						' already exists!\Proceed will store over it.' withNewLines]].
	^true
</details>

#### ClassBuilder>>#class: oldClass instanceVariableNames: instVarString

This is the basic initialization message to change the definition of an existing Metaclass


<details>
	<summary>See more</summary>
	
	class: oldClass instanceVariableNames: instVarString
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	oldClass isMeta ifFalse:[^self error: oldClass name, 'is not a Metaclass'].
	^self class: oldClass instanceVariableNames: instVarString unsafe: false
</details>

#### ClassBuilder>>#reshapeClass: oldClass toSuper: newSuper

Reshape the given class to the new super class. Recompile all the methods in the newly created class. Answer the new class.


<details>
	<summary>See more</summary>
	
	reshapeClass: oldClass toSuper: newSuper
	"Reshape the given class to the new super class. Recompile all the methods in the newly created class. Answer the new class."
	| instVars |
	instVars := instVarMap at: oldClass name ifAbsent: [oldClass instVarNames].

	^self newSubclassOf: newSuper 
			type: oldClass typeOfClass 
			instanceVariables: instVars 
			from: oldClass
</details>

#### ClassBuilder>>#superclass: aClass
	variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class in which the subclass is to have indexable byte-sized nonpointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	| oldClassOrNil actualType |
	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a byte subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isBytes not])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with 16, 32 or 64 bit fields'].
	oldClassOrNil := Smalltalk at: t ifAbsent: nil.
	actualType := (oldClassOrNil notNil
				   and: [oldClassOrNil typeOfClass == #compiledMethod])
					ifTrue: [#compiledMethod]
					ifFalse: [#bytes].
	^self 
		name: t
		subclassOf: aClass
		type: actualType
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass

Answer whether we need a new subclass to conform to the requested changes


<details>
	<summary>See more</summary>
	
	needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
	"Answer whether we need a new subclass to conform to the requested changes"
	| newFormat |
	"Compute the format of the new class"
	newFormat _ 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper 
			ccIndex: (oldClass ifNil:[0] ifNotNil:[oldClass indexIfCompact]).
	newFormat
		ifNil: [^ nil].

	"Check if we really need a new subclass"
	oldClass ifNil:[^true]. "yes, it's a new class"
	newSuper == oldClass superclass ifFalse:[^true]. "yes, it's a superclass change"
	newFormat = oldClass format ifFalse:[^true]. "yes, it's a format change"
	instVars = oldClass instVarNames ifFalse:[^true]. "yes, it's an iVar change"

	^false

</details>

#### ClassBuilder>>#superclass: aClass
	variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class in which the subclass is to have indexable double-word-sized (64 bit) nonpointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-word-sized (64 bit) nonpointer variables."
	"Note: Only for Spur images"

	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isDoubleWords not])
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with 8, 16 or 32 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #longs
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#informUserDuring: aBlock

<details>
	<summary>See more</summary>
	
	informUserDuring: aBlock
	self class isSilent ifTrue:[^aBlock value].
	Utilities informUserDuring:[:barBlock|
		progress _ barBlock.
		aBlock value].
	progress _ nil.
</details>

#### ClassBuilder>>#superclass: aClass
	variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class in which the subclass is to have indexable word-sized (32 bit) nonpointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable word-sized (32 bit) nonpointer variables."
	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isWords not])
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with 8, 16 or 64 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #words
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#privateNewSubclassOf: newSuper from: oldClass

Create a new meta and non-meta subclass of newSuper using oldClass as template


<details>
	<summary>See more</summary>
	
	privateNewSubclassOf: newSuper from: oldClass
	"Create a new meta and non-meta subclass of newSuper using oldClass as template"
	"WARNING: This method does not preserve the superclass/subclass invariant!"
	| newSuperMeta oldMeta newMeta |
	oldClass ifNil:[^self privateNewSubclassOf: newSuper].
	newSuperMeta _ newSuper ifNil:[Class] ifNotNil:[newSuper class].
	oldMeta _ oldClass class.
	newMeta _ oldMeta shallowCopy.
	newMeta 
		superclass: newSuperMeta
		methodDictionary: MethodDictionary new
		format: (self computeFormat: oldMeta typeOfClass 
					instSize: oldMeta instVarNames size 
					forSuper: newSuperMeta
					ccIndex: 0);
		setInstVarNames: oldMeta instVarNames;
		organization: oldMeta organization.
	"Recompile the meta class"
	oldMeta hasMethods 
		ifTrue:[newMeta compileAllFrom: oldMeta].
	"Record the meta class change"
	self recordClass: oldMeta replacedBy: newMeta.
	"And create a new instance"
	^newMeta adoptInstance: oldClass from: oldMeta
</details>

#### ClassBuilder>>#doneCompiling: aClass

The receiver has finished modifying the class hierarchy. Do any necessary cleanup.


<details>
	<summary>See more</summary>
	
	doneCompiling: aClass
	"The receiver has finished modifying the class hierarchy.
	Do any necessary cleanup."
	aClass doneCompiling.
	Behavior flushObsoleteSubclasses.
</details>

#### ClassBuilder>>#privateNewSubclassOf: newSuper

Create a new meta and non-meta subclass of newSuper


<details>
	<summary>See more</summary>
	
	privateNewSubclassOf: newSuper
	"Create a new meta and non-meta subclass of newSuper"
	"WARNING: This method does not preserve the superclass/subclass invariant!"
	| newSuperMeta newMeta |
	newSuperMeta _ newSuper ifNil:[Class] ifNotNil:[newSuper class].
	newMeta _ Metaclass new.
	newMeta 
		superclass: newSuperMeta 
		methodDictionary: MethodDictionary new 
		format: newSuperMeta format.
	^newMeta new

</details>

#### ClassBuilder>>#superclass: aClass
	weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have weak indexable pointer variables."
	aClass isBits 
		ifTrue: [^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	^self 
		name: t
		subclassOf: aClass
		type: #weak
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	instVarMap _ IdentityDictionary new.
</details>

#### ClassBuilder>>#showProgressFor: aClass

Announce that we're processing aClass


<details>
	<summary>See more</summary>
	
	showProgressFor: aClass
	"Announce that we're processing aClass"
	progress
		ifNil: [^ self].
	aClass isObsolete ifTrue:[^self].
	currentClassIndex _ currentClassIndex + 1.
	(aClass hasMethods and: [aClass wantsRecompilationProgressReported]) ifTrue: [
		[	"As we might be recompiling the  lasses involved in showing progress, ignore errors. After all, they are harmless."
			progress value: ('Recompiling ', aClass name,' (', currentClassIndex printString,'/', maxClassIndex printString,')')
		] on: Error do: nil]
</details>

#### ClassBuilder>>#reservedNames

Return a list of names that must not be used for variables


<details>
	<summary>See more</summary>
	
	reservedNames

	"Return a list of names that must not be used for variables"

	^self class reservedNames 
</details>

#### ClassBuilder>>#silentlyMoveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName

Move the instvar from srcClass to dstClass. Do not perform any checks.


<details>
	<summary>See more</summary>
	
	silentlyMoveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName
	"Move the instvar from srcClass to dstClass.
	Do not perform any checks."
	| srcVars dstVars dstIndex newClass copyOfSrcClass copyOfDstClass |
	copyOfSrcClass _ srcClass copy.
	copyOfDstClass _ dstClass copy.
	
	srcVars _ srcClass instVarNames copyWithout: instVarName.
	srcClass == dstClass
		ifTrue:[dstVars _ srcVars]
		ifFalse:[dstVars _ dstClass instVarNames].
	dstIndex _ dstVars indexOf: prevInstVarName.
	dstVars _ (dstVars copyFrom: 1 to: dstIndex),
				(Array with: instVarName),
				(dstVars copyFrom: dstIndex+1 to: dstVars size).
	instVarMap at: srcClass name put: srcVars.
	instVarMap at: dstClass name put: dstVars.
	(srcClass inheritsFrom: dstClass) ifTrue:[
		newClass _ self reshapeClass: dstClass toSuper: dstClass superclass.
		self recompile: false from: dstClass to: newClass mutate: true.
	] ifFalse:[
		(dstClass inheritsFrom: srcClass) ifTrue:[
			newClass _ self reshapeClass: srcClass toSuper: srcClass superclass.
			self recompile: false from: srcClass to: newClass mutate: true.
		] ifFalse:[ "Disjunct hierarchies"
			srcClass == dstClass ifFalse:[
				newClass _ self reshapeClass: dstClass toSuper: dstClass superclass.
				self recompile: false from: dstClass to: newClass mutate: true.
			].
			newClass _ self reshapeClass: srcClass toSuper: srcClass superclass.
			self recompile: false from: srcClass to: newClass mutate: true.
		].
	].
	self doneCompiling: srcClass.
	self doneCompiling: dstClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfSrcClass to: srcClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfDstClass to: dstClass.
</details>

#### ClassBuilder>>#superclass: aClass
	variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class in which the subclass is to have indexable pointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable pointer variables."
	aClass isBits 
		ifTrue: [^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	^self 
		name: t
		subclassOf: aClass
		type: #variable
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#validateSubclassFormat: newType from: oldClass forSuper: newSuper extra: newInstSize

Validate the # of instVars and the format of the subclasses


<details>
	<summary>See more</summary>
	
	validateSubclassFormat: newType from: oldClass forSuper: newSuper extra: newInstSize
	"Validate the # of instVars and the format of the subclasses"
	| deltaSize |
	oldClass ifNil: [^ true]. "No subclasses"
	"Compute the # of instvars needed for all subclasses"
	deltaSize _ newInstSize.
	oldClass
		ifNotNil: [deltaSize _ deltaSize - oldClass instVarNames size].
	newSuper
		ifNotNil: [deltaSize _ deltaSize + newSuper instSize].
	(oldClass notNil and: [oldClass superclass notNil]) 
		ifTrue: [deltaSize _ deltaSize - oldClass superclass instSize].
	oldClass
		ifNil: [deltaSize > 254
				ifTrue: [self error: 'More than 254 instance variables'.
					^ false].
			^ true].

	oldClass withAllSubclassesDo: [ :sub | 
		( sub instSize + deltaSize > 254 )
			ifTrue: [
				self error: sub name,' has more than 254 instance variables'.
				^ false].

		"If we get this far, check whether the immediate subclasses of oldClass can keep its layout."
		(newType ~~ #normal) 
			ifTrue: [ self validateSubclass: sub canKeepLayoutFrom: oldClass forSubclassFormat: newType ]].

	^ true
</details>

#### ClassBuilder>>#computeFormat: type instSize: newInstSize forSuper: newSuper ccIndex: ccIndex

Compute the new format for making oldClass a subclass of newSuper. Return the format or nil if there is any problem.


<details>
	<summary>See more</summary>
	
	computeFormat: type instSize: newInstSize forSuper: newSuper ccIndex: ccIndex
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	| instSize isVar isWords isPointers isWeak |

	"Spur uses this version"
	Smalltalk isSpur ifTrue: [
		^ self computeFormat: type instSize: newInstSize forSuper: newSuper ].

	"This for preSpur images"
	type == #compiledMethod
		ifTrue:[^CompiledMethod format].
	instSize := newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize]).
	instSize > 254 ifTrue:[
		self error: 'Class has too many instance variables (', instSize printString,')'.
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isWords := isPointers := true].
	type == #bytes ifTrue:[isVar := true. isWords := isPointers := isWeak := false].
	type == #words ifTrue:[isVar := isWords := true. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := isWords := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isWords := isPointers := true].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and:[instSize > 0]) ifTrue:[
		self error:'A non-pointer class cannot have instance variables'.
		^nil].
	^(self format: instSize 
		variable: isVar 
		words: isWords 
		pointers: isPointers 
		weak: isWeak) + (ccIndex bitShift: 11)
</details>

#### ClassBuilder>>#superclass: aClass
	ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have ephemeron semantics, i.e. where the object will be queued for finalization when the key (first) inst var is not reachable other than through the other fields of ephemerons with unreachable keys.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	"Pre Spur does not support Ephemerons."
	Smalltalk isSpur ifFalse: [
		self halt: 'Attempt to create a new Ephemeron Class in a PreSpur image!'.
		^ self superclass: aClass
			subclass: t instanceVariableNames: f 
			classVariableNames: d poolDictionaries: s category: cat ].

	aClass isPointers ifFalse:
		[^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	aClass instSize + f substrings size < 2 ifTrue:
		[^self error: 'cannot make an ephemeron class with less than two named instance varaibles'].
	^self 
		name: t
		subclassOf: aClass
		type: #ephemeron
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#validateInstvars: instVarArray from: oldClass forSuper: newSuper

Check if any of the instVars of oldClass conflict with the new superclass


<details>
	<summary>See more</summary>
	
	validateInstvars: instVarArray from: oldClass forSuper: newSuper
	"Check if any of the instVars of oldClass conflict with the new superclass"
	| instVars usedNames temp |
	instVarArray isEmpty ifTrue:[^true]. "Okay"
	newSuper allowsSubInstVars ifFalse: [
		self error: newSuper printString, ' does not allow subclass inst vars. See allowsSubInstVars.'. ^ false].

	"Validate the inst var names"
	usedNames _ instVarArray asSet.
	usedNames size = instVarArray size 
		ifFalse:[	instVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp _ var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp _ var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames _ newSuper allInstVarNames asSet.
		instVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl instVarNames includes: iv) ifTrue:[temp _ cl]].
				self error: iv,' is already defined in ', temp name.
				^false]]].
	oldClass ifNotNil: [
		usedNames _ Set new: 20.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl instVarNames].
		instVars _ instVarArray.
		newSuper ifNotNil: [instVars _ instVars, newSuper allInstVarNames].
		instVars do:[:iv|
			(usedNames includes: iv) ifTrue:[
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true
</details>

#### ClassBuilder>>#validateSubclass: subclass canKeepLayoutFrom: oldClass forSubclassFormat: newType

Returns whether the immediate subclasses of oldClass can keep its layout


<details>
	<summary>See more</summary>
	
	validateSubclass: subclass canKeepLayoutFrom: oldClass forSubclassFormat: newType 
	"Returns whether the immediate subclasses of oldClass can keep its layout"
	"Note: Squeak does not appear to model classFormat relationships.. so I'm putting some logic here. bkv 4/2/2003"
	
	"Only run this test for a real subclass - otherwise this prevents changing
	a class from #subclass: to #variableSubclass: etc."
	subclass = oldClass ifTrue:[^true].

	 "isWeak implies isVariant"					
	 (oldClass isVariable and: [ subclass isWeak ])
		ifFalse: [ "In general we discourage format mis-matches"
				  (subclass typeOfClass == newType) 
				   	ifFalse: [ self error: subclass name,' cannot be recompiled'.
							  ^ false ]].
	^ true
</details>

#### ClassBuilder>>#superclass: aClass
	immediateSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new immediate class as a subclass of an existing class.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	immediateSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a
	 new immediate class as a subclass of an existing class."

	"Pre Spur does not support creating new immediate classes."
	Smalltalk isSpur ifFalse: [
		^ self superclass: aClass
			subclass: t instanceVariableNames: f 
			classVariableNames: d poolDictionaries: s category: cat ].

	aClass instSize > 0
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with named fields'].
	aClass isVariable
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with indexed instance variables'].
	aClass isPointers
		ifFalse: [^self error: 'cannot make an immediate subclass of a class without pointer fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #immediate
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#superclass: newSuper
	subclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class.


<details>
	<summary>See more</summary>
	
	superclass: newSuper
	subclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class."
	^self 
		name: t
		subclassOf: newSuper
		type: newSuper typeOfClass
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#tooDangerousClasses

Return a list of class names which will not be modified in the public interface


<details>
	<summary>See more</summary>
	
	tooDangerousClasses
	"Return a list of class names which will not be modified in the public interface"
	^#(
		"Object will break immediately"
		ProtoObject Object
		"Contexts and their superclasses"
		InstructionStream ContextPart MethodContext BlockClosure
		"Superclasses of basic collections"
		Collection SequenceableCollection ArrayedCollection
		"Collections known to the VM"
		Array Bitmap String Symbol ByteArray CompiledMethod
		"Basic Numbers"
		Magnitude Number SmallInteger Float
		"Misc other"
		LookupKey Association Link Point Rectangle Behavior PositionableStream UndefinedObject
	)

</details>

#### ClassBuilder>>#format: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak

Only for preSpur!


<details>
	<summary>See more</summary>
	
	format: nInstVars variable: isVar words: isWords pointers: isPointers weak: isWeak

	"Only for preSpur!"

	"Compute the format for the given instance specfication."
	| cClass instSpec sizeHiBits fmt |
"
	NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
	For now the format word is...
		<2 bits=instSize//64><5 bits=cClass><4 bits=instSpec><6 bits=instSize\\64><1 bit=0>
	But when we revise the image format, it should become...
		<5 bits=cClass><4 bits=instSpec><8 bits=instSize><1 bit=0>
"
	sizeHiBits _ (nInstVars+1) // 64.
	cClass _ 0.  "for now"
	instSpec _ isWeak
		ifTrue:[4]
		ifFalse:[isPointers
				ifTrue: [isVar
						ifTrue: [nInstVars>0 ifTrue: [3] ifFalse: [2]]
						ifFalse: [nInstVars>0 ifTrue: [1] ifFalse: [0]]]
				ifFalse: [isWords ifTrue: [6] ifFalse: [8]]].
	fmt _ sizeHiBits.
	fmt _ (fmt bitShift: 5) + cClass.
	fmt _ (fmt bitShift: 4) + instSpec.
	fmt _ (fmt bitShift: 6) + ((nInstVars+1)\\64).  "+1 since prim size field includes header"
	fmt _ (fmt bitShift: 1). "This shift plus integer bit lets wordSize work like byteSize"
	^fmt
</details>

#### ClassBuilder>>#class: oldClass instanceVariableNames: instVarString unsafe: unsafe

This is the basic initialization message to change the definition of an existing Metaclass


<details>
	<summary>See more</summary>
	
	class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew _ self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class"
	copyOfOldClass _ oldClass copy.
	newClass _ self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.
		
	newClass _ self recompile: false from: oldClass to: newClass mutate: false.
	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	^newClass
</details>

#### ClassBuilder>>#moveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName

Move the given instVar from srcClass to dstClass


<details>
	<summary>See more</summary>
	
	moveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName
	"Move the given instVar from srcClass to dstClass"
	(srcClass instVarNames includes: instVarName)
		ifFalse:[^self error: instVarName,' is not an instance variable of ', srcClass name].
	(prevInstVarName isNil or:[dstClass instVarNames includes: prevInstVarName])
		ifFalse:[^self error: prevInstVarName, 'is not an instance variable of', dstClass name].
	(srcClass inheritsFrom: dstClass) ifTrue:[
		"Move the instvar up the hierarchy."
		(self validateClass: srcClass forMoving: instVarName upTo: dstClass)
			ifFalse:[^false].
	].
	(dstClass inheritsFrom: srcClass) ifTrue:[
		"Move the instvar down the hierarchy"
		(self validateClass: srcClass forMoving: instVarName downTo: dstClass)
			ifFalse:[^false].
	].
	^self silentlyMoveInstVarNamed: instVarName from: srcClass to: dstClass after: prevInstVarName
</details>

#### ClassBuilder>>#computeFormat: type instSize: newInstSize forSuper: newSuper

Compute the new format for making oldClass a subclass of newSuper. Answer the format or nil if there is any problem.


<details>
	<summary>See more</summary>
	
	computeFormat: type instSize: newInstSize forSuper: newSuper
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	"Only for Spur!"

	| instSize isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue:
		[newInstSize > 0 ifTrue:
			[self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	instSize := newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize]).
	instSize > 65535 ifTrue:
		[self error: 'Class has too many instance variables (', instSize printString,')'.
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isPointers := true].
	type == #bytes ifTrue:[isVar := true. bitsUnitSize := 1. isPointers := isWeak := false].
	type == #shorts ifTrue:[isVar := true. bitsUnitSize := 2. isPointers := isWeak := false].
	type == #words ifTrue:[isVar := true. bitsUnitSize := 4. isPointers := isWeak := false].
	type == #longs ifTrue:[isVar := true. bitsUnitSize := 8. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isPointers := true].
	type == #ephemeron ifTrue:[isVar := false. isWeak := isPointers := true].
	type == #immediate ifTrue:[isVar := isWeak := isPointers := false].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and: [instSize > 0]) ifTrue:
		[self error: 'A non-pointer class cannot have named instance variables'.
		^nil].
	^self format: instSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak
</details>

#### ClassBuilder>>#mutate: oldClass to: newClass

Mutate the old class and subclasses into newClass and subclasses. Note: This method is slightly different from: #mutate:toSuper: since here we are at the root of reshaping and have two distinct roots.


<details>
	<summary>See more</summary>
	
	mutate: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Note: This method is slightly different from: #mutate:toSuper: since
	here we are at the root of reshaping and have two distinct roots."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self mutate: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self mutate: oldSubclass to: newSubclass.
		].
	].
	self update: oldClass to: newClass.
	^newClass
</details>

#### ClassBuilder>>#validateSuperclass: aSuperClass forSubclass: aClass

Check if it is okay to use aSuperClass as the superclass of aClass


<details>
	<summary>See more</summary>
	
	validateSuperclass: aSuperClass forSubclass: aClass
	"Check if it is okay to use aSuperClass as the superclass of aClass"
	aClass ifNil: [ "New class"
		(aSuperClass == nil or:[aSuperClass isBehavior and:[aSuperClass isMeta not]])
			ifFalse:[self error: aSuperClass name,' is not a valid superclass'.
					^false].
		^true].
	aSuperClass == aClass superclass ifTrue:[^true]. "No change"
	(aClass isMeta) "Not permitted - meta class hierarchy is derived from class hierarchy"
		ifTrue:[^self error: aClass name, ' must inherit from ', aClass superclass name].
	"Check for circular references"
	(aSuperClass notNil and:[aSuperClass == aClass or:[aSuperClass inheritsFrom: aClass]])
		ifTrue:[self error: aSuperClass name,' inherits from ', aClass name.
				^false].
	^true
</details>

#### ClassBuilder>>#validateClass: srcClass forMoving: iv downTo: dstClass

Make sure that we don't have any accesses to the instVar left


<details>
	<summary>See more</summary>
	
	validateClass: srcClass forMoving: iv downTo: dstClass
	"Make sure that we don't have any accesses to the instVar left"
	srcClass withAllSubclassesDo:[:cls|
		(cls == dstClass or:[cls inheritsFrom: dstClass]) ifFalse:[
			(cls whichSelectorsAccess: iv) isEmpty ifFalse:[
				self notify: (iv printString asText allBold), ' is still used in ', cls name asText allBold,'.
Proceed to move it to Undeclared'.
			].
		].
	].
	^true
</details>

#### ClassBuilder>>#superclass: aClass
	variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat

This is the standard initialization message for creating a new class as a subclass of an existing class in which the subclass is to have indexable double-byte-sized (16 bit) nonpointer variables.


<details>
	<summary>See more</summary>
	
	superclass: aClass
	variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-byte-sized (16 bit) nonpointer variables."
	"Note: Only for Spur images"

	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isDoubleBytes not])
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with 8, 32 or 64 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #shorts
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
</details>

#### ClassBuilder>>#name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category

Define a new class


<details>
	<summary>See more</summary>
	
	name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category
	"Define a new class"
	^self 
		name: className 
		subclassOf: newSuper 
		type: type 
		instanceVariableNames: instVarString 
		classVariableNames: classVarString 
		poolDictionaries: poolString 
		category: category
		unsafe: false
</details>

#### ClassBuilder>>#format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak

Only for Spur!


<details>
	<summary>See more</summary>
	
	format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak

	"Only for Spur!"

	"Compute the format for the given instance specfication.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character, SmallFloat64)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap, WideString)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable (ByteString)
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := isWeak
					ifTrue:
						[isVar
							ifTrue: [4]
							ifFalse: [5]]
					ifFalse:
						[isPointers
							ifTrue:
								[isVar
									ifTrue: [nInstVars > 0 ifTrue: [3] ifFalse: [2]]
									ifFalse: [nInstVars > 0 ifTrue: [1] ifFalse: [0]]]
							ifFalse:
								[isVar
									ifTrue: [bitsUnitSize caseOf: {
											[1] -> [16].
											[2] -> [12].
											[4] -> [10].
											[8] -> [9] }]
									ifFalse: [7]]].
	^(instSpec bitShift: 16) + nInstVars
</details>

## ClassCategoryReader

I represent a mechanism for retrieving class descriptions stored on a file.

### Methods
#### ClassCategoryReader>>#theClass

<details>
	<summary>See more</summary>
	
	theClass

	^ class
</details>

#### ClassCategoryReader>>#setClass: aClass category: aCategory changeStamp: aString

<details>
	<summary>See more</summary>
	
	setClass: aClass category: aCategory changeStamp: aString

	class _ aClass.
	category _ aCategory.
	changeStamp _ aString

</details>

#### ClassCategoryReader>>#changeStamp

<details>
	<summary>See more</summary>
	
	changeStamp
	^changeStamp
</details>

#### ClassCategoryReader>>#setClass: aClass category: aCategory

<details>
	<summary>See more</summary>
	
	setClass: aClass category: aCategory
	^ self setClass: aClass category: aCategory changeStamp: String new

</details>

#### ClassCategoryReader>>#scanFrom: aStream

File in methods from the stream, aStream.


<details>
	<summary>See more</summary>
	
	scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodSource |
	[
		methodSource _ aStream nextChunk.
		methodSource size > 0] whileTrue: [
		class compile: methodSource classified: category
			withStamp: changeStamp
			notifying: nil ]
</details>

## ClassCommentReader

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ClassCommentReader>>#scanFrom: aStream

File in the class comment from aStream. Not string-i-fied, just a text, exactly as it is in the browser. Move to changes file.


<details>
	<summary>See more</summary>
	
	scanFrom: aStream 
	"File in the class comment from aStream.  Not string-i-fied, just a text, exactly as it is in the browser.  Move to changes file."

	class theNonMetaClass classComment: aStream nextChunk stamp: changeStamp
		"Writes it on the disk and saves a RemoteString ref"
</details>

## ClassDescription

I add a number of facilities to basic Behaviors: Named instance variables Category organization for methods The notion of a name of this class (implemented as subclass responsibility) The maintenance of a ChangeSet, and logging changes on a file Most of the mechanism for fileOut. I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass. The slots 'organization' and 'methodDict' should ONLY be accessed by message in order for things to work during ImageSegment>>discoverActiveClasses (q.v.).

### Methods
#### ClassDescription>>#compileSilently: code classified: category

Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed.


<details>
	<summary>See more</summary>
	
	compileSilently: code classified: category
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: category notifying: nil.
</details>

#### ClassDescription>>#compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource

<details>
	<summary>See more</summary>
	
	compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	| methodAndNode selector isExistingMethod |
	methodAndNode _ self basicCompile: text asString notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		self logMethodSource: text forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector
</details>

#### ClassDescription>>#addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor

<details>
	<summary>See more</summary>
	
	addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor
	| priorMethodOrNil priorProtocolOrNil |
	priorMethodOrNil _ self compiledMethodAt: selector ifAbsent: nil.
	priorProtocolOrNil _ self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod 
				selector: selector 
				inProtocol: category 
				class: self 
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.
			category = priorProtocolOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorProtocolOrNil
					to: category
					inClass: self ]]
</details>

#### ClassDescription>>#subclasses

slow implementation since Behavior does not keep track of subclasses


<details>
	<summary>See more</summary>
	
	subclasses
	^ Array new
</details>

#### ClassDescription>>#updateInstances: oldInstances from: oldClass isMeta: isMeta

Recreate any existing instances of the argument, oldClass, as instances of the receiver, which is a newly changed class. Permute variables as necessary, and forward old instances to new instances. Answer nil to defeat old clients that expect an array of old instances. The old behaviour, which necessitated a global GC, exchanged identities and answered the old instances. But no clients used the result. This way we avoid the unnecessary GC,


<details>
	<summary>See more</summary>
	
	updateInstances: oldInstances from: oldClass isMeta: isMeta
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver,
	 which is a newly changed class. Permute variables as necessary, and forward old instances
	 to new instances.  Answer nil to defeat old clients that expect an array of old instances.
	 The old behaviour, which necessitated a global GC, exchanged identities and answered
	 the old instances.  But no clients used the result.  This way we avoid the unnecessary GC,"
	| map variable instSize newInstances |

	oldInstances isEmpty ifTrue:
		[^nil]. "no instances to convert"
	isMeta ifTrue:
		[(oldInstances size = 1
		  and: [self soleInstance class == self
				or: [self soleInstance class == oldClass]]) ifFalse:
			[^self error: 'Metaclasses can only have one instance']].
	map := self instVarMappingFrom: oldClass.
	variable := self isVariable.
	instSize := self instSize.
	newInstances := Array new: oldInstances size.
	1 to: oldInstances size do:
		[:i|
		newInstances
			at: i
			put: (self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize map: map)].
	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsForwardIdentityTo: newInstances.
	^nil
</details>

#### ClassDescription>>#chooseInstVarThenDo: aBlock

Put up a menu of all the instance variables in the receiver, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter. If the list is 6 or larger, then offer an alphabetical formulation as an alternative. triggered by a 'show alphabetically' item at the top of the list.


<details>
	<summary>See more</summary>
	
	chooseInstVarThenDo: aBlock
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count _ self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars _ OrderedCollection new.
	lines _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).

	(offerAlpha _ count > 5)
		ifTrue: [
			lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first; newLine].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class instVarNames.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream skip: -1 "cut last CR".
	(lines size > 0 and: [ lines last = allVars size ]) ifTrue: [
		lines removeLast ].  "dispense with inelegant line beneath last item"
	index _ (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [
		^ self chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)
</details>

#### ClassDescription>>#printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex

Copy the source code for the method associated with selector onto the fileStream. If moveSource true, then also set the source code pointer of the method.


<details>
	<summary>See more</summary>
	
	printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble method oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asString printString]
		ifFalse: [preamble _ ''].
	method _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((method fileIndex = 0
		or: [(SourceFiles at: method fileIndex) == nil])
		or: [(oldPos _ method filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: method decompileString]
	ifFalse: [
		sourceFile _ SourceFiles at: method fileIndex.
		preamble size > 0
			ifTrue:    "Copy the preamble"
				[outStream copyPreamble: preamble from: sourceFile at: oldPos]
			ifFalse:
				[sourceFile position: oldPos].
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.
			method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine
</details>

#### ClassDescription>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 

	aStream nextPutAll: self name
</details>

#### ClassDescription>>#copyCategory: cat from: class

Specify that one of the categories of messages for the receiver is cat, as found in the class, class. Copy each message found in this category.


<details>
	<summary>See more</summary>
	
	copyCategory: cat from: class 
	"Specify that one of the categories of messages for the receiver is cat, as 
	found in the class, class. Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat
</details>

#### ClassDescription>>#category

Answer the system organization category for the receiver.


<details>
	<summary>See more</summary>
	
	category
	"Answer the system organization category for the receiver."

	^SystemOrganization categoryOfElement: self name
</details>

#### ClassDescription>>#updateMethodBindingsTo: aBinding

ClassBuilder support for maintaining valid method bindings.


<details>
	<summary>See more</summary>
	
	updateMethodBindingsTo: aBinding
	"ClassBuilder support for maintaining valid method bindings."
	methodDict do: [:method| method methodClassAssociation: aBinding]
</details>

#### ClassDescription>>#linesOfCode

Object linesOfCode


<details>
	<summary>See more</summary>
	
	linesOfCode

"
Object linesOfCode 
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel |
		lines _ lines + (self compiledMethodAt: sel) linesOfCode ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode]
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) detectSum: [:c |
	(SystemOrganization superclassOrderIn: c) detectSum: [:cl | cl linesOfCode]]
 23232
"
</details>

#### ClassDescription>>#addInstVarName: aString

Add the argument, aString, as one of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility
</details>

#### ClassDescription>>#category: cat

Categorize the receiver under the system category, cat, removing it from any previous categorization.


<details>
	<summary>See more</summary>
	
	category: cat 
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCat |
	oldCat _ self category.
	(cat isString)
		ifTrue: [SystemOrganization classify: self name under: cat asSymbol]
		ifFalse: [self errorCategoryName].
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCat to: cat asSymbol
</details>

#### ClassDescription>>#comment: aStringOrText

Set the receiver's comment to be the argument, aStringOrText.


<details>
	<summary>See more</summary>
	
	comment: aStringOrText
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asString.
</details>

#### ClassDescription>>#fileOutCategory: catName

FileOut the named category


<details>
	<summary>See more</summary>
	
	fileOutCategory: catName
	"FileOut the named category"

	DirectoryEntry smalltalkImageDirectory // (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ]
</details>

#### ClassDescription>>#sharedPoolsString

Answer a string of my shared pool names separated by spaces.


<details>
	<summary>See more</summary>
	
	sharedPoolsString
	"Answer a string of my shared pool names separated by spaces."

	^String streamContents: [ :stream |
		self sharedPools 
			do: [ :each |
				stream nextPutAll: (Smalltalk
					keyAtIdentityValue: each 
					ifAbsent: [ 'private' ]) ]
			separatedBy: [ stream space ] ]
</details>

#### ClassDescription>>#removeSelectorIfInBaseSystem: selector

Remove the message whose selector is given from the method dictionary of the receiver, if it is there and not part of a package. Answer nil otherwise.


<details>
	<summary>See more</summary>
	
	removeSelectorIfInBaseSystem: selector
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there and not part of a package. Answer nil otherwise."
	| priorProtocol | 

	self compiledMethodAt: selector ifAbsent: [^ nil].
	priorProtocol _ self whichCategoryIncludesSelector: selector.
	CodePackage
		packageOfMethodCategory: priorProtocol
		ofClass: self
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			self removeSelector: selector ]
</details>

#### ClassDescription>>#logMethodSource: aText forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor

<details>
	<summary>See more</summary>
	
	logMethodSource: aText forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |
	
	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ :aSuperclass | 
		aSuperclass lookupSelector: aCompiledMethodWithNode selector ].
	
	aCompiledMethodWithNode method 
		putSource: aText asString
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil 
		overridesMethod: overridenMethodOrNil.
</details>

#### ClassDescription>>#wantsRecompilationProgressReported

Answer whether the receiver would like progress of its recompilation reported interactively to the user.


<details>
	<summary>See more</summary>
	
	wantsRecompilationProgressReported
	"Answer whether the receiver would like progress of its recompilation reported interactively to the user."

	^ true
</details>

#### ClassDescription>>#acceptsLoggingOfCompilation

weird name is so that it will come lexically before #compile, so that a clean build can make it through. 7/7/96 sw


<details>
	<summary>See more</summary>
	
	acceptsLoggingOfCompilation
	"weird name is so that it will come lexically before #compile, so that a clean build can make it through.  7/7/96 sw"

	^ true
</details>

#### ClassDescription>>#withClassesThatDefineInHierarchyInstanceVariable: aName do: foundBlock ifNone: noneBlock

<details>
	<summary>See more</summary>
	
	withClassesThatDefineInHierarchyInstanceVariable: aName do: foundBlock ifNone: noneBlock

	^(self classThatDefinesInstanceVariable: aName)
		ifNil: [ self withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock ]
		ifNotNil: [ :definingClass | foundBlock value: (Array with: definingClass) ]
</details>

#### ClassDescription>>#fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex

File a description of the receiver's category, aString, onto aFileStream. If moveSource, is true, then set the method source pointer to the new file position. Note when this method is called with moveSource=true, it is condensing the .sources file, and should only write one preamble per method category.


<details>
	<summary>See more</summary>
	
	fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the receiver's category, aString, onto aFileStream. If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.sources file, and should only write one preamble per method category."

	| selectors |

	aFileStream newLine.
	selectors _ (aSymbol == ClassOrganizer allCategory)
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [:sel |
		self printMethodChunk: sel 
			withPreamble: true
			on: aFileStream 
			moveSource: moveSource 
			toFile: fileIndex].
	^ self
</details>

#### ClassDescription>>#printCategoryChunk: categoryName on: aFileStream

<details>
	<summary>See more</summary>
	
	printCategoryChunk: categoryName on: aFileStream
	^ self printCategoryChunk: categoryName withStamp: '' on: aFileStream
</details>

#### ClassDescription>>#instVarNames

Answer an Array of the receiver's instance variable names.


<details>
	<summary>See more</summary>
	
	instVarNames
	"Answer an Array of the receiver's instance variable names."

	^instanceVariables ifNil: [ #() ]
</details>

#### ClassDescription>>#printSubclassesOn: aStream level: level

As part of the algorithm for printing a description of the receiver, print the subclass on the file stream, aStream, indenting level times.


<details>
	<summary>See more</summary>
	
	printSubclassesOn: aStream level: level
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times."
	| subclassNames |
	aStream newLineTab: level.
	aStream nextPutAll: self name.
	aStream
		 space;
		 print: self instVarNames.
	self == Class ifTrue: [
		aStream
			 newLineTab: level + 1;
			 nextPutAll: '[ ... all the Metaclasses ... ]'.
		^ self ].
	subclassNames _ self subclasses asArray sort: [ :c1 :c2 |
		c1 name <= c2 name ].
	"Print subclasses in alphabetical order"
	subclassNames do: [ :subclass |
		subclass
			printSubclassesOn: aStream
			level: level + 1 ].
</details>

#### ClassDescription>>#whichCategoryIncludesSelector: aSelector

Answer the category of the argument, aSelector, in the organization of the receiver, or answer nil if the receiver does not inlcude this selector.


<details>
	<summary>See more</summary>
	
	whichCategoryIncludesSelector: aSelector 
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	(self includesSelector: aSelector)
		ifTrue: [^ self organization categoryOfElement: aSelector]
		ifFalse: [^nil]
</details>

#### ClassDescription>>#comment: aStringOrText stamp: aStamp

Set the receiver's comment to be the argument, aStringOrText.


<details>
	<summary>See more</summary>
	
	comment: aStringOrText stamp: aStamp
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asString stamp: aStamp.
</details>

#### ClassDescription>>#instanceVariablesString

Answer a string of my instance variable names separated by spaces.


<details>
	<summary>See more</summary>
	
	instanceVariablesString
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
</details>

#### ClassDescription>>#isOkToChangeMethod: selector isMeta: isMeta

A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed


<details>
	<summary>See more</summary>
	
	isOkToChangeMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed"
	^true
</details>

#### ClassDescription>>#copyAll: selArray from: class

Install all the methods found in the method dictionary of the second argument, class, as the receiver's methods. Classify the messages under -As yet not classified-.


<details>
	<summary>See more</summary>
	
	copyAll: selArray from: class 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	-As yet not classified-."

	self copyAll: selArray
		from: class
		classified: nil
</details>

#### ClassDescription>>#removeInstVarName: aString

Remove the argument, aString, as one of the receiver's instance variables. Create an error notification if the argument is not found.


<details>
	<summary>See more</summary>
	
	removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance 
	variables. Create an error notification if the argument is not found."

	self subclassResponsibility
</details>

#### ClassDescription>>#fileOutChangedMessages: aSet on: aFileStream

File a description of the messages of the receiver that have been changed (i.e., are entered into the argument, aSet) onto aFileStream.


<details>
	<summary>See more</summary>
	
	fileOutChangedMessages: aSet on: aFileStream 
	"File a description of the messages of the receiver that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	self fileOutChangedMessages: aSet
		on: aFileStream
		moveSource: false
		toFile: 0
</details>

#### ClassDescription>>#theNonMetaClass

Sent to a class or metaclass, always return the class


<details>
	<summary>See more</summary>
	
	theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^self
</details>

#### ClassDescription>>#superclass: aClass methodDictionary: mDict format: fmt

Basic initialization of the receiver


<details>
	<summary>See more</summary>
	
	superclass: aClass methodDictionary: mDict format: fmt
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	instanceVariables _ nil.
	self organization: nil.
</details>

#### ClassDescription>>#recoverFromMDFault

This method handles methodDict faults to support, eg, discoverActiveClasses (qv).


<details>
	<summary>See more</summary>
	
	recoverFromMDFault
	"This method handles methodDict faults to support, eg, discoverActiveClasses (qv)."
	(organization isMemberOf: Array) ifFalse: [^ self error: 'oops'].
	methodDict _ organization first.
	organization _ organization second.

</details>

#### ClassDescription>>#forceNewFrom: anArray

Create a new instance of the class and fill its instance variables up with the array.


<details>
	<summary>See more</summary>
	
	forceNewFrom: anArray
    "Create a new instance of the class and fill
    its instance variables up with the array."
    | object max |

    object _ self new.
    max _ self instSize.
    anArray withIndexDo: [:each :index |
        index > max ifFalse:
            [object instVarAt: index put: each]].
    ^ object
</details>

#### ClassDescription>>#obsolete

Make the receiver obsolete.


<details>
	<summary>See more</summary>
	
	obsolete
	"Make the receiver obsolete."
	superclass removeSubclass: self.
	self organization: nil.
	super obsolete.
</details>

#### ClassDescription>>#chooseInstVarAlphabeticallyThenDo: aBlock

Put up a menu of all the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter.


<details>
	<summary>See more</summary>
	
	chooseInstVarAlphabeticallyThenDo: aBlock
	| allVars index |
	"Put up a menu of all the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	allVars _ self allInstVarNames sorted.
	allVars isEmpty ifTrue: [^ self inform: 'There are no
instance variables'].

	index _ (PopUpMenu labelArray: allVars lines: #()) startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	aBlock value: (allVars at: index)
</details>

#### ClassDescription>>#fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex

File a description of the receiver on aFileStream. If the boolean argument, moveSource, is true, then set the trailing bytes to the position of aFileStream and to fileIndex in order to indicate where to find the source code.


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver on aFileStream. If the boolean 
	argument, moveSource, is true, then set the trailing bytes to the position 
	of aFileStream and to fileIndex in order to indicate where to find the 
	source code."

	aFileStream nextPut: $!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	self organization categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]
</details>

#### ClassDescription>>#printWithClosureAnalysisOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printWithClosureAnalysisOn: aStream 

	aStream nextPutAll: self name
</details>

#### ClassDescription>>#printCategoryChunk: category on: aFileStream priorMethod: priorMethod

<details>
	<summary>See more</summary>
	
	printCategoryChunk: category on: aFileStream priorMethod: priorMethod
	^ self printCategoryChunk: category on: aFileStream
		withStamp: Utilities changeStamp priorMethod: priorMethod
</details>

#### ClassDescription>>#classComment: aString stamp: aStamp

Store the comment, aString or Text or RemoteString, associated with the class we are organizing. Empty string gets stored only if had a non-empty one before.


<details>
	<summary>See more</summary>
	
	classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file _ SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!.	"directly"
			header _ String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
				stream newLine; nextPut: $!.	"directly"
				header _ String streamContents: [:strm | strm nextPutAll: self name;
					nextPutAll: ' commentStamp: '.
					aStamp storeOn: strm.
					strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
				stream nextChunkPut: header.
				stream newLine; nextChunkPut: aString.
			].
	].
	SystemChangeNotifier uniqueInstance classCommented: self
</details>

#### ClassDescription>>#classVariablesString

Answer a string of my class variable names separated by spaces.


<details>
	<summary>See more</summary>
	
	classVariablesString
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classPool keys sort 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
</details>

#### ClassDescription>>#commentStamp: changeStamp prior: indexAndOffset

Prior source link ignored when filing in.


<details>
	<summary>See more</summary>
	
	commentStamp: changeStamp prior: indexAndOffset
	"Prior source link ignored when filing in."

	^ ClassCommentReader new setClass: self
				category: #Comment
				changeStamp: changeStamp

</details>

#### ClassDescription>>#copyCategory: cat from: aClass classified: newCat

Specify that one of the categories of messages for the receiver is the third argument, newCat. Copy each message found in the category cat in class aClass into this new category.


<details>
	<summary>See more</summary>
	
	copyCategory: cat from: aClass classified: newCat 
	"Specify that one of the categories of messages for the receiver is the 
	third argument, newCat. Copy each message found in the category cat in 
	class aClass into this new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat
</details>

#### ClassDescription>>#definitionPreamble

<details>
	<summary>See more</summary>
	
	definitionPreamble

	^self definitionPreambleWithoutStamp, Utilities changeStampField
</details>

#### ClassDescription>>#definition

Answer a String that defines the receiver.


<details>
	<summary>See more</summary>
	
	definition
	"Answer a String that defines the receiver."

	^String streamContents: [ :strm |
		strm
			nextPutAll: (superclass ifNotNil: [ superclass name ] ifNil: [ 'ProtoObject' ]);
			nextPutAll: self kindOfSubclass;
			store: self name.
		strm
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
		strm
			newLine;
			tab;
			nextPutAll: 'category: ';
			store: self category asString.

		superclass ifNil: [ 
			strm nextPutAll: '.'; newLine.
			strm nextPutAll: self name.
			strm space; nextPutAll: 'superclass: nil' ]]
</details>

#### ClassDescription>>#classThatDefinesInstanceVariable: instVarName

<details>
	<summary>See more</summary>
	
	classThatDefinesInstanceVariable: instVarName
	(instanceVariables notNil and: [instanceVariables includes: instVarName asString]) ifTrue: [^ self]. 
	^ superclass ifNotNil: [superclass classThatDefinesInstanceVariable: instVarName]
</details>

#### ClassDescription>>#storeOn: aStream

Classes and Metaclasses have global names.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name
</details>

#### ClassDescription>>#newInstanceFrom: oldInstance variable: variable size: instSize map: map

Create a new instance of the receiver based on the given old instance. The supplied map contains a mapping of the old instVar names into the receiver's instVars


<details>
	<summary>See more</summary>
	
	newInstanceFrom: oldInstance variable: variable size: instSize map: map
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"
	| new |
	variable
		ifTrue: [new _ self basicNew: oldInstance basicSize]
		ifFalse: [new _ self basicNew].
	1 to: instSize do: 
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	variable 
		ifTrue: [1 to: oldInstance basicSize do: 
					[:offset |
					new basicAt: offset put: (oldInstance basicAt: offset)]].
	^new
</details>

#### ClassDescription>>#induceMDFault

Stache a copy of the methodDict in the organization slot (hack!), and set the methodDict to nil. This will induce an MD fault on any message send. See: ClassDescription>>recoverFromMDFault and ImageSegment>>discoverActiveClasses.


<details>
	<summary>See more</summary>
	
	induceMDFault
	"Stache a copy of the methodDict in the organization slot (hack!),
	and set the methodDict to nil.  This will induce an MD fault on any message send.
	See: ClassDescription>>recoverFromMDFault
	and ImageSegment>>discoverActiveClasses."

	organization _ Array with: methodDict with: organization.
	methodDict _ nil.
	self flushCache
</details>

#### ClassDescription>>#commentStamp: changeStamp

<details>
	<summary>See more</summary>
	
	commentStamp: changeStamp
	self organization commentStamp: changeStamp.
    ^ self commentStamp: changeStamp prior: 0
</details>

#### ClassDescription>>#isOkToRemoveMethod: selector isMeta: isMeta

A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed


<details>
	<summary>See more</summary>
	
	isOkToRemoveMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed"
	^true
</details>

#### ClassDescription>>#copyAllCategoriesFrom: aClass

Specify that the categories of messages for the receiver include all of those found in the class, aClass. Install each of the messages found in these categories into the method dictionary of the receiver, classified under the appropriate categories.


<details>
	<summary>See more</summary>
	
	copyAllCategoriesFrom: aClass 
	"Specify that the categories of messages for the receiver include all of 
	those found in the class, aClass. Install each of the messages found in 
	these categories into the method dictionary of the receiver, classified 
	under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]
</details>

#### ClassDescription>>#definitionPreambleWithoutStamp

<details>
	<summary>See more</summary>
	
	definitionPreambleWithoutStamp

	^'classDefinition: ', self name printString, ' category: ', self category printString
</details>

#### ClassDescription>>#browseClassVarRefs

Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable


<details>
	<summary>See more</summary>
	
	browseClassVarRefs 
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"

	| lines labelStream allVars index owningClasses |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	owningClasses _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [ lines add: allVars size ]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ self].
	Smalltalk browseAllCallsOn:
		((owningClasses at: index) classPool associationAt: (allVars at: index))
</details>

#### ClassDescription>>#chooseClassVarName

Present the user with a list of class variable names and answer the one selected, or nil if none


<details>
	<summary>See more</summary>
	
	chooseClassVarName 
	"Present the user with a list of class variable names and answer the one selected, or nil if none"

	| lines labelStream  allVars index |
	lines _ OrderedCollection new.
	allVars _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class classVarNames asArray sort.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	labelStream skip: -1 "cut last CR".
	index _ (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ nil].
	^ allVars at: index
</details>

#### ClassDescription>>#allMethodCategoriesIntegratedThrough: mostGenericClass

Answer a list of all the method categories of the receiver and all its superclasses, up through mostGenericClass


<details>
	<summary>See more</summary>
	
	allMethodCategoriesIntegratedThrough: mostGenericClass
	"Answer a list of all the method categories of the receiver and all its superclasses, up through mostGenericClass"

	| aColl |
	aColl _ OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		(aClass includesBehavior: mostGenericClass)
			ifTrue: [ aColl addAll: aClass organization categories ]].
	aColl remove: 'no messages' asSymbol ifAbsent: nil.

	^ aColl asSet asSortedCollection: [ :a :b | a asLowercase < b asLowercase ]

"ColorTileMorph allMethodCategoriesIntegratedThrough: TileMorph"
</details>

#### ClassDescription>>#fileOutOn: aFileStream

File a description of the receiver on aFileStream.


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream 
	"File a description of the receiver on aFileStream."

	self fileOutOn: aFileStream
		moveSource: false
		toFile: 0
</details>

#### ClassDescription>>#organization: aClassOrg

Install an instance of ClassOrganizer that represents the organization of the messages of the receiver.


<details>
	<summary>See more</summary>
	
	organization: aClassOrg
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	organization _ aClassOrg
</details>

#### ClassDescription>>#updateInstancesFrom: oldClass

Recreate any existing instances of the argument, oldClass, as instances of the receiver, which is a newly changed class. Permute variables as necessary, and forward old instances to new instances.. Answer nil to defeat any clients that expected the old behaviour of answering the array of old instances.


<details>
	<summary>See more</summary>
	
	updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| newMethod oldMethod selector |
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				selector _ context method selector.
				oldMethod _ oldClass lookupSelector: selector.
				newMethod _ self lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					MethodInCallStackToBecomeInvalid
						signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
		runningProcessSearchStart: nil.
	self updateInstances: oldClass allInstances asArray from: oldClass isMeta: self isMeta.
	^nil

"This attempts to crash the VM by stepping off the end of an instance.
 As the doctor says, do not do this."
"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock value"
</details>

#### ClassDescription>>#theMetaClass

Sent to a class or metaclass, always return the metaclass


<details>
	<summary>See more</summary>
	
	theMetaClass
	"Sent to a class or metaclass, always return the metaclass"

	^self class
</details>

#### ClassDescription>>#putClassCommentToCondensedChangesFile: aFileStream

Called when condensing changes. If the receiver has a class comment, and if that class comment does not reside in the .sources file, then write it to the given filestream, with the resulting RemoteString being reachable from the source file #2. Note that any existing backpointer into the .sources file is lost by this process -- a situation that maybe should be fixed someday.


<details>
	<summary>See more</summary>
	
	putClassCommentToCondensedChangesFile: aFileStream
	"Called when condensing changes.  If the receiver has a class comment, and if that class comment does not reside in the .sources file, then write it to the given filestream, with the resulting RemoteString being reachable from the source file #2.  Note that any existing backpointer into the .sources file is lost by this process -- a situation that maybe should be fixed someday."

	| header aCommentRemoteStr aStamp |
	self isMeta ifTrue: [^ self].  "bulletproofing only"
	((aCommentRemoteStr _ self organization commentRemoteStr) isNil or:
		[aCommentRemoteStr sourceFileNumber = 1]) ifTrue: [^ self].

	aFileStream newLine; nextPut: $!.
	header _ String streamContents: [:strm | 
		strm 
			nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
		(aStamp _ self organization commentStamp ifNil: ['<historical>']) storeOn: strm.
		strm nextPutAll: ' prior: 0'].
	aFileStream nextChunkPut: header.
	aFileStream newLine.
	self organization classComment: (RemoteString newString: self organization classComment onFileNumber: 2 toFile: aFileStream) stamp: aStamp
</details>

#### ClassDescription>>#methods

Answer a ClassCategoryReader for compiling messages that are not classified, as in fileouts made with Smalltalk/V


<details>
	<summary>See more</summary>
	
	methods
	"Answer a ClassCategoryReader for compiling messages that are not classified, as in fileouts made with Smalltalk/V"

	^ ClassCategoryReader new setClass: self category: ClassOrganizer default
</details>

#### ClassDescription>>#compile: text classified: category withStamp: changeStamp notifying: requestor

<details>
	<summary>See more</summary>
	
	compile: text classified: category withStamp: changeStamp notifying: requestor
	^ self compile: text classified: category withStamp: changeStamp notifying: requestor logSource: self acceptsLoggingOfCompilation
</details>

#### ClassDescription>>#copy: sel from: class classified: cat

Install the method associated with the first arugment, sel, a message selector, found in the method dictionary of the second argument, class, as one of the receiver's methods. Classify the message under the third argument, cat.


<details>
	<summary>See more</summary>
	
	copy: sel from: class classified: cat 
	"Install the method associated with the first arugment, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under the third 
	argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code _ class sourceCodeAt: sel.
	code ifNotNil: [
			category _ cat
				ifNil: [ class organization categoryOfElement: sel].
			(self methodDict includesKey: sel)
				ifTrue: [code = (self sourceCodeAt: sel) 
							ifFalse: [self error: self name 
										, ' ' 
										, sel 
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]
</details>

#### ClassDescription>>#classThatDefinesClassVariable: classVarName

Answer the class that defines the given class variable


<details>
	<summary>See more</summary>
	
	classThatDefinesClassVariable: classVarName
	"Answer the class that defines the given class variable"

	(self classPool includesKey: classVarName asSymbol) ifTrue: [^ self]. 
	^ superclass ifNotNil: [superclass classThatDefinesClassVariable: classVarName]
</details>

#### ClassDescription>>#checkForInstVarsOK: instVarString

Return true if instVarString does no include any names used in a subclass


<details>
	<summary>See more</summary>
	
	checkForInstVarsOK: instVarString
	"Return true if instVarString does no include any names used in a subclass"
	| instVarArray |
	instVarArray _ Scanner new scanFieldNames: instVarString.
	self allSubclasses do:
		[:cl | cl instVarNames do:
			[:n | (instVarArray includes: n)
				ifTrue: [self error: n , ' is already used in ' , cl name.
						^ false]]].
	^ true
</details>

#### ClassDescription>>#methodsFor: categoryName stamp: changeStamp prior: indexAndOffset

Prior source link ignored when filing in.


<details>
	<summary>See more</summary>
	
	methodsFor: categoryName stamp: changeStamp prior: indexAndOffset
	"Prior source link ignored when filing in."
	^ ClassCategoryReader new setClass: self
				category: categoryName asSymbol
				changeStamp: changeStamp

"Most importantly, return the new ClassCategoryReader, so a fileIn will let it seize control.  So method will be placed in the proper category.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"
</details>

#### ClassDescription>>#moveChangesTo: newFile

Used in the process of condensing changes, this message requests that the source code of all methods of the receiver that have been changed should be moved to newFile. Write the class's definition before the changes if there are any so that the changes file is a potential application recovery vehicle, not just a crash recovery vehicle.


<details>
	<summary>See more</summary>
	
	moveChangesTo: newFile 
	"Used in the process of condensing changes, this message requests that 
	the source code of all methods of the receiver that have been changed 
	should be moved to newFile.
	
	Write the class's definition before the changes if there are any so that the changes file is a potential application recovery vehicle, not just a crash recovery vehicle."

	| changes |
	changes _ self methodDict keys select: [:sel | (self methodDict at: sel) fileIndex > 1].
	
         changes isEmpty ifTrue:
                 [^self].
         newFile newLine; newLine; nextChunkPut: self definition; newLine.

	self fileOutChangedMessages: changes asSet
		on: newFile
		moveSource: true
		toFile: 2
</details>

#### ClassDescription>>#compile: text classified: category notifying: requestor

<details>
	<summary>See more</summary>
	
	compile: text classified: category notifying: requestor
	| stamp |
	stamp _ self acceptsLoggingOfCompilation ifTrue: [Utilities changeStamp] ifFalse: [nil].
	^ self compile: text classified: category
		withStamp: stamp notifying: requestor

 
</details>

#### ClassDescription>>#errorCategoryName

<details>
	<summary>See more</summary>
	
	errorCategoryName
	self error: 'Category name must be a String'
</details>

#### ClassDescription>>#setInstVarNames: instVarArray

Private - for class initialization only


<details>
	<summary>See more</summary>
	
	setInstVarNames: instVarArray
	"Private - for class initialization only"
	| required |
	required _ self instSize.
	superclass ifNotNil: [
		required _ required - superclass instSize].
	instVarArray size = required
		ifFalse:[
			^self error: required printString, ' instvar names are required'].
	instanceVariables _ instVarArray isEmpty
		ifFalse: [ instVarArray asArray]
</details>

#### ClassDescription>>#compile: code notifying: requestor

Refer to the comment in Behavior|compile:notifying:.


<details>
	<summary>See more</summary>
	
	compile: code notifying: requestor 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: ClassOrganizer default
		 notifying: requestor
</details>

#### ClassDescription>>#withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock

<details>
	<summary>See more</summary>
	
	withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock

	| definingSubclasses |

	definingSubclasses := self allSubclasses select: [ :aSubclass | aSubclass definesInstanceVariableNamed: aName ].

	^definingSubclasses isEmpty
		ifTrue: noneBlock
		ifFalse: [ foundBlock value: definingSubclasses ]
</details>

#### ClassDescription>>#instVarNameForIndex: index

Answer the named instance variable with index index or nil if none.


<details>
	<summary>See more</summary>
	
	instVarNameForIndex: index
	"Answer the named instance variable with index index or nil if none."

	| superInstSize |
	index > self instSize ifTrue: [^nil].
	superInstSize := superclass ifNil: [0] ifNotNil: [superclass instSize].
	index > superInstSize ifTrue: [
		^instanceVariables at: index - superInstSize].
	^superclass ifNotNil: [ superclass instVarNameForIndex: index ]
</details>

#### ClassDescription>>#fileOutMethod: selector

Write source code of a single method on a file. Make up a name for the file.


<details>
	<summary>See more</summary>
	
	fileOutMethod: selector
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	nameBody _ self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry smalltalkImageDirectory // (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream timeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ]
</details>

#### ClassDescription>>#classComment: aString

Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing. Empty string gets stored only if had a non-empty one before.


<details>
	<summary>See more</summary>
	
	classComment: aString
	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	^ self classComment: aString stamp: '<historical>'
</details>

#### ClassDescription>>#allMethodsInCategory: aSymbol

Answer a list of all the method categories of the receiver and all its superclasses


<details>
	<summary>See more</summary>
	
	allMethodsInCategory: aSymbol
	"Answer a list of all the method categories of the receiver and all its superclasses"

	| aColl |
	aColl _ OrderedCollection new.
	self withAllSuperclasses do:
		[:aClass | aColl addAll:
			(aSymbol == ClassOrganizer allCategory
				ifTrue:
					[aClass organization allMethodSelectors]
				ifFalse:
					[aClass organization listAtCategoryNamed: aSymbol])].
	^ aColl asSet sorted

"TileMorph allMethodsInCategory: #initialization"
</details>

#### ClassDescription>>#commentFollows

Answer a ClassCommentReader who will scan in the comment.


<details>
	<summary>See more</summary>
	
	commentFollows 
	"Answer a ClassCommentReader who will scan in the comment."

	^ ClassCommentReader new setClass: self category: #Comment

	"False commentFollows inspect"
</details>

#### ClassDescription>>#doneCompiling

A ClassBuilder has finished the compilation of the receiver. This message is a notification for a class that needs to do some cleanup / reinitialization after it has been recompiled.


<details>
	<summary>See more</summary>
	
	doneCompiling
	"A ClassBuilder has finished the compilation of the receiver.
	This message is a notification for a class that needs to do some
	cleanup / reinitialization after it has been recompiled."
</details>

#### ClassDescription>>#methodsFor: categoryName stamp: changeStamp

<details>
	<summary>See more</summary>
	
	methodsFor: categoryName stamp: changeStamp 
	^ self methodsFor: categoryName stamp: (Utilities fixStamp: changeStamp) prior: 0
</details>

#### ClassDescription>>#compile: code classified: heading

Compile the argument, code, as source code in the context of the receiver and install the result in the receiver's method dictionary under the classification indicated by the second argument, heading. nil is to be notified if an error occurs. The argument code is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string.


<details>
	<summary>See more</summary>
	
	compile: code classified: heading 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary under 
	the classification indicated by the second argument, heading. nil is to be 
	notified if an error occurs. The argument code is either a string or an 
	object that converts to a string or a PositionableStream on an object that 
	converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil
</details>

#### ClassDescription>>#isOkToAddMethod: selector isMeta: isMeta

A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed


<details>
	<summary>See more</summary>
	
	isOkToAddMethod: selector isMeta: isMeta
	"A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed"
	^true
</details>

#### ClassDescription>>#methodsFor: aString priorSource: sourcePosition inFile: fileIndex

Prior source pointer ignored when filing in.


<details>
	<summary>See more</summary>
	
	methodsFor: aString priorSource: sourcePosition inFile: fileIndex
	"Prior source pointer ignored when filing in."
	^ self methodsFor: aString
</details>

#### ClassDescription>>#classVersion

Default. Any class may return a later version to inform readers that use ReferenceStream. 8/17/96 tk


<details>
	<summary>See more</summary>
	
	classVersion
	"Default.  Any class may return a later version to inform readers that use ReferenceStream.  8/17/96 tk"
	"This method allows you to distinguish between class versions when the shape of the class 
	hasn't changed (when there's no change in the instVar names).
	In the conversion methods you usually can tell by the inst var names 
	what old version you have. In a few cases, though, the same inst var 
	names were kept but their interpretation changed (like in the layoutFrame).
	By changing the class version when you keep the same instVars you can 
	warn older and newer images that they have to convert."
	^ 0
</details>

#### ClassDescription>>#printCategoryChunk: categoryName withStamp: changeStamp on: aFileStream

<details>
	<summary>See more</summary>
	
	printCategoryChunk: categoryName withStamp: changeStamp on: aFileStream
	^ self printCategoryChunk: categoryName on: aFileStream withStamp: changeStamp
		priorMethod: nil
</details>

#### ClassDescription>>#reformatAll

Reformat all methods in this class. Leaves old code accessible to version browsing


<details>
	<summary>See more</summary>
	
	reformatAll 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	self selectorsDo: [:sel | self reformatMethodAt: sel]
</details>

#### ClassDescription>>#copyMethodDictionaryFrom: donorClass

Copy the method dictionary of the donor class over to the receiver


<details>
	<summary>See more</summary>
	
	copyMethodDictionaryFrom: donorClass
	"Copy the method dictionary of the donor class over to the receiver"

	methodDict _ donorClass copyOfMethodDictionary.
	self organization: donorClass organization copy
</details>

#### ClassDescription>>#instVarMappingFrom: oldClass

Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass.


<details>
	<summary>See more</summary>
	
	instVarMappingFrom: oldClass
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames _ oldClass allInstVarNames.
	^self allInstVarNames 
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].

</details>

#### ClassDescription>>#chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock

<details>
	<summary>See more</summary>
	
	chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock

	^self
		chooseDefiningInstanceVariableAlphabeticallyWith: aCaption
		thenDo: aBlock
		ifNone: [ ^ self inform: 'There are no instance variables defined in ', self name ] 
</details>

#### ClassDescription>>#allInstVarNamesEverywhere

Answer the set of inst var names used by the receiver, all superclasses, and all subclasses


<details>
	<summary>See more</summary>
	
	allInstVarNamesEverywhere
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList _ OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedRectMorph allInstVarNamesEverywhere"
</details>

#### ClassDescription>>#removeCategory: aString

Remove each of the messages categorized under aString in the method dictionary of the receiver. Then remove the category aString.


<details>
	<summary>See more</summary>
	
	removeCategory: aString 
	"Remove each of the messages categorized under aString in the method 
	dictionary of the receiver. Then remove the category aString."
	| categoryName |
	categoryName _ aString asSymbol.
	(self organization listAtCategoryNamed: categoryName) do:
		[:sel | self removeSelector: sel].
	self organization removeCategory: categoryName
</details>

#### ClassDescription>>#chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock ifNone: noneBlock

Put up a menu of the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter.


<details>
	<summary>See more</summary>
	
	chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock ifNone: noneBlock

	| vars index |
	"Put up a menu of the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	vars _ self instVarNames sorted.
	vars isEmpty ifTrue: [^ noneBlock value ].

	index _ (PopUpMenu labelArray: vars lines: #()) startUpWithCaption: aCaption.
	index = 0 ifTrue: [^ self].
	aBlock value: (vars at: index)
</details>

#### ClassDescription>>#printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod

<details>
	<summary>See more</summary>
	
	printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod 

	^self 
		printCategoryChunk: category 
		on: aFileStream 
		withStamp: changeStamp 
		priorMethod: priorMethod 
		overridesMethod: nil 
</details>

#### ClassDescription>>#methodsFor: categoryName

Answer a ClassCategoryReader for compiling the messages in the category, categoryName, of the receiver.


<details>
	<summary>See more</summary>
	
	methodsFor: categoryName 
	"Answer a ClassCategoryReader for compiling the messages in the category, categoryName, of the receiver."

	^ ClassCategoryReader new setClass: self category: categoryName asSymbol

	"(False methodsFor: 'logical operations') inspect"
</details>

#### ClassDescription>>#comment

Answer the receiver's comment. (If missing, supply a template)


<details>
	<summary>See more</summary>
	
	comment
	"Answer the receiver's comment. (If missing, supply a template) "
	| aString |
	aString _ self theNonMetaClass organization classComment.
	aString isEmpty ifFalse: [^ aString].
	^
'Main comment stating the purpose of this class and relevant relationship to other classes.

Possible useful expressions for doIt or printIt.

Structure:
 instVar1		type -- comment about the purpose of instVar1
 instVar2		type -- comment about the purpose of instVar2

Any further useful comments about the general approach of this implementation.'
</details>

#### ClassDescription>>#printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod overridesMethod: overridenMethod

Print a method category preamble. This must have a category name. It may have an author/date stamp, and it may have a prior source link. If it has a prior source link, it MUST have a stamp, even if it is empty.


<details>
	<summary>See more</summary>
	
	printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod overridesMethod: overridenMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream newLine; nextPut: $!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp notNil and: [
			changeStamp size > 0 or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		overridenMethod notNil ifTrue: [
			strm nextPutAll: ' overrides: '; print: overridenMethod sourcePointer]
		]).

</details>

#### ClassDescription>>#classesThatImplementAllOf: selectorSet

Return an array of any classes that implement all the messages in selectorSet.


<details>
	<summary>See more</summary>
	
	classesThatImplementAllOf: selectorSet
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	found _ OrderedCollection new.
	selectorSet do:
		[:sel | (self methodDict includesKey: sel) ifTrue: [found add: sel]].
	found isEmpty
		ifTrue: [^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: selectorSet)]]
		ifFalse: [remaining _ selectorSet copyWithoutAll: found.
				remaining isEmpty ifTrue: [^ Array with: self].
				^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: remaining)]]
</details>

#### ClassDescription>>#reorganize

During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization. See the transfer of control where ReadWriteStream fileIn calls scanFrom:


<details>
	<summary>See more</summary>
	
	reorganize
	"During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"

	^self organization
</details>

#### ClassDescription>>#removeSelector: selector

Remove the message whose selector is given from the method dictionary of the receiver, if it is there. Answer nil otherwise.


<details>
	<summary>See more</summary>
	
	removeSelector: selector 
	| priorMethod priorProtocol | 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there. Answer nil otherwise."

	priorMethod _ self compiledMethodAt: selector ifAbsent: [^ nil].
	(self theNonMetaClass isOkToRemoveMethod: selector isMeta: self isMeta)
		ifFalse: [self error: 'Method removal not allowed'].
	priorProtocol _ self whichCategoryIncludesSelector: selector.
	SystemChangeNotifier uniqueInstance doSilently: [
		self organization removeElement: selector].
	super removeSelector: selector.
	SystemChangeNotifier uniqueInstance 
			methodRemoved: priorMethod selector: selector inProtocol: priorProtocol class: self.
</details>

#### ClassDescription>>#zapOrganization

Remove the organization of this class by message categories. This is typically done to save space in small systems. Classes and methods created or filed in subsequently will, nonetheless, be organized


<details>
	<summary>See more</summary>
	
	zapOrganization
	"Remove the organization of this class by message categories.
	This is typically done to save space in small systems.  Classes and methods
	created or filed in subsequently will, nonetheless, be organized"

	self organization: nil.
	self isMeta ifFalse: [self class zapOrganization]
</details>

#### ClassDescription>>#subclassesDo: aBlock

Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.


<details>
	<summary>See more</summary>
	
	subclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	^self subclasses do: aBlock
</details>

#### ClassDescription>>#fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex

File a description of the messages of this class that have been changed (i.e., are entered into the argument, aSet) onto aFileStream. If moveSource, is true, then set the method source pointer to the new file position. Note when this method is called with moveSource=true, it is condensing the .changes file, and should only write a preamble for every method.


<details>
	<summary>See more</summary>
	
	fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the messages of this class that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream.  If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.changes file, and should only write a preamble for every method."
	| org |
	(org _ self organization) categories do: [ :cat |  | sels |
		sels _ (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
		sels do: [:sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream moveSource: moveSource toFile: fileIndex]]
</details>

#### ClassDescription>>#hasDefinition

<details>
	<summary>See more</summary>
	
	hasDefinition
	^ true
</details>

#### ClassDescription>>#fileOutOrganizationOn: aFileStream

File a description of the receiver's organization on aFileStream.


<details>
	<summary>See more</summary>
	
	fileOutOrganizationOn: aFileStream
	"File a description of the receiver's organization on aFileStream."

	aFileStream newLine; nextPut: $!.
	aFileStream nextChunkPut: self name, ' reorganize'; newLine.
	aFileStream nextChunkPut: self organization printString; newLine
</details>

#### ClassDescription>>#organization

Answer the instance of ClassOrganizer that represents the organization of the messages of the receiver.


<details>
	<summary>See more</summary>
	
	organization
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	organization ifNil:
		[self organization: (ClassOrganizer defaultList: self methodDict keys sort)].
	(organization isMemberOf: Array) ifTrue:
		[self recoverFromMDFaultWithTrace].
	
	"Making sure that subject is set correctly. It should not be necessary."
	organization ifNotNil: [organization setSubject: self].
	^ organization
</details>

#### ClassDescription>>#copy: sel from: class

Install the method associated with the first argument, sel, a message selector, found in the method dictionary of the second argument, class, as one of the receiver's methods. Classify the message under -As yet not classified-.


<details>
	<summary>See more</summary>
	
	copy: sel from: class 
	"Install the method associated with the first argument, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under -As yet not 
	classified-."

	self copy: sel
		from: class
		classified: nil
</details>

#### ClassDescription>>#recoverFromMDFaultWithTrace

This method handles emthodDict faults to support, eg, discoverActiveClasses (qv).


<details>
	<summary>See more</summary>
	
	recoverFromMDFaultWithTrace
	"This method handles emthodDict faults to support, eg, discoverActiveClasses (qv)."
	self recoverFromMDFault.
	Smalltalk at: #MDFaultDict ifPresent:
		[:faultDict | faultDict at: self name put:
			(String streamContents:
				[:strm | (thisContext stackOfSize: 20) do: [:item | strm print: item; newLine]])]

"Execute the following statement to induce MD fault tracing.  This means that, not only will all active classes be recorded but, after a test run, MDFaultDict will contain, for every class used, a stack trace showing how it came to be used.  This statement should be executed just prior to any such text, in order to clear the traces.

	Smalltalk at: #MDFaultDict put: Dictionary new.

"
</details>

#### ClassDescription>>#localBindingOf: varNameSymbol

<details>
	<summary>See more</summary>
	
	localBindingOf: varNameSymbol

	self subclassResponsibility 
</details>

#### ClassDescription>>#compileSilently: code classified: category notifying: requestor

Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed.


<details>
	<summary>See more</summary>
	
	compileSilently: code classified: category notifying: requestor
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemChangeNotifier uniqueInstance 
		doSilently: [self compile: code classified: category withStamp: nil notifying: requestor logSource: false].
</details>

#### ClassDescription>>#instVarNamesAndOffsetsDo: aBinaryBlock

This is part of the interface between the compiler and a class's instance or field names. The class should enumerate aBinaryBlock with the instance variable name strings and their integer offsets. The order is important. Names evaluated later will override the same names occurring earlier.


<details>
	<summary>See more</summary>
	
	instVarNamesAndOffsetsDo: aBinaryBlock
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	| superInstSize |
	(superInstSize := superclass ifNotNil: [superclass instSize] ifNil: [0]) > 0 ifTrue: [
		superclass instVarNamesAndOffsetsDo: aBinaryBlock].
	1 to: self instSize - superInstSize do: [ :i | 
		aBinaryBlock value: (instanceVariables at: i) value: i + superInstSize]
</details>

#### ClassDescription>>#methodsInCategory: aSymbol

Answer a list of the methods of the receiver that are in category named aName


<details>
	<summary>See more</summary>
	
	methodsInCategory: aSymbol 
	"Answer a list of the methods of the receiver that are in category named aName"
	
	| aColl |
	aColl _ Set withAll: (aSymbol == ClassOrganizer allCategory
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort
</details>

#### ClassDescription>>#moveInstVarNamed: instVarName to: anotherClass after: prevInstVarName

Move the given instance variable to another class.


<details>
	<summary>See more</summary>
	
	moveInstVarNamed: instVarName to: anotherClass after: prevInstVarName
	"Move the given instance variable to another class."
	| answer |
	self == anotherClass ifFalse:[
		self notify:'Warning:' asText allBold,' moving ', instVarName printString,' from ', self name,' to ', anotherClass name,' will not be recorded in the change set correctly.
Proceed to do it anyways.'].
	answer _ ClassBuilder new
		moveInstVarNamed: instVarName 
		from: self 
		to: anotherClass 
		after: prevInstVarName.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### ClassDescription>>#hasComment

return whether this class truly has a comment other than the default


<details>
	<summary>See more</summary>
	
	hasComment
	"return whether this class truly has a comment other than the default"
	| org |
	org := self theNonMetaClass organization.
	^org classComment notNil and: [
		org classComment isEmpty not ].

</details>

#### ClassDescription>>#allUnreferencedClassVariables

Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses


<details>
	<summary>See more</summary>
	
	allUnreferencedClassVariables
	"Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses"
	| aList |
	aList _ OrderedCollection new.
	self withAllSuperclasses reverseDo: [ :aClass |
		aClass classVarNames do: [ :var |
			(Smalltalk isThereAReferenceTo: (aClass classPool associationAt: var)) ifFalse: [ aList add: var ]]].
	^ aList
</details>

#### ClassDescription>>#ultimateSourceCodeAt: selector ifAbsent: aBlock

Return the source code at selector, deferring to superclass if necessary


<details>
	<summary>See more</summary>
	
	ultimateSourceCodeAt: selector ifAbsent: aBlock
	"Return the source code at selector, deferring to superclass if necessary"
	^ self sourceCodeAt: selector ifAbsent:
		[superclass
			ifNil:
				[aBlock value]
			 ifNotNil:
				[superclass ultimateSourceCodeAt: selector ifAbsent: aBlock]]
</details>

#### ClassDescription>>#copyAll: selArray from: class classified: cat

Install all the methods found in the method dictionary of the second argument, class, as the receiver's methods. Classify the messages under the third argument, cat.


<details>
	<summary>See more</summary>
	
	copyAll: selArray from: class classified: cat 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	the third argument, cat."

	selArray do: 
		[:s | self copy: s
				from: class
				classified: cat]
</details>

#### ClassDescription>>#reformatMethodAt: selector

<details>
	<summary>See more</summary>
	
	reformatMethodAt: selector 
	| newCodeString method | 
	newCodeString _ (self compilerClass new)
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method _ self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		inFile: 2 priorMethod: method
</details>

#### ClassDescription>>#wantsChangeSetLogging

Answer whether code submitted for the receiver should be remembered by the changeSet mechanism. 7/12/96 sw


<details>
	<summary>See more</summary>
	
	wantsChangeSetLogging
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.  7/12/96 sw"


	^ true
</details>

#### ClassDescription>>#removeUninstantiatedSubclassesSilently

Remove the classes of any subclasses that have neither instances nor subclasses. Answer the number of bytes reclaimed


<details>
	<summary>See more</summary>
	
	removeUninstantiatedSubclassesSilently
	"Remove the classes of any subclasses that have neither instances nor subclasses.  Answer the number of bytes reclaimed"
	"Player removeUninstantiatedSubclassesSilently"

	| candidatesForRemoval  oldFree |

	oldFree _ Smalltalk garbageCollect.
	candidatesForRemoval _
		self subclasses select: [:c |
			(c instanceCount = 0) and: [c subclasses size = 0]].
	candidatesForRemoval do: [:c | c removeFromSystem].
	^ Smalltalk garbageCollect - oldFree
</details>

## ClassOrganizer

I represent method categorization information for classes. The handling of class comments has gone through a tortuous evolution. Grandfathered class comments (before late aug 98) have no time stamps, and historically, fileouts of class comments always substituted the timestamp reflecting the author and date/time at the moment of fileout; and historically any timestamps in a filed out class comment were dropped on the floor, with the author & time prevailing at the moment of filein being substituted. Such grandfathered comments now go out on fileouts with '<historical>' timestamp; class comments created after the 8/98 changes will have their correct timestamps preserved, though there is not yet a decent ui for reading those stamps other than filing out and looking at the file; nor is there yet any ui for browsing and recovering past versions of such comments. Everything in good time!

### Methods
#### ClassOrganizer>>#addCategory: catString before: nextCategory

Add a new category named heading. If default category exists and is empty, remove it. If nextCategory is nil, then add the new one at the end, otherwise, insert it before nextCategory.


<details>
	<summary>See more</summary>
	
	addCategory: catString before: nextCategory
	| oldCategories |
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super addCategory: catString before: nextCategory].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#sortCategories

<details>
	<summary>See more</summary>
	
	sortCategories
	| oldCategories |
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super sortCategories].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#removeEmptyCategories

Remove empty categories.


<details>
	<summary>See more</summary>
	
	removeEmptyCategories
	| oldCategories |
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeEmptyCategories].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#notifyOfChangedSelector: element from: oldCategory to: newCategory

<details>
	<summary>See more</summary>
	
	notifyOfChangedSelector: element from: oldCategory to: newCategory
	(self hasSubject and: [(oldCategory ~= newCategory)]) ifTrue: [
		SystemChangeNotifier uniqueInstance selectorRecategorized: element from: oldCategory to: newCategory inClass: self subject
	].
</details>

#### ClassOrganizer>>#notifyOfChangedSelectorsOldDict: oldDictionaryOrNil newDict: newDictionaryOrNil

<details>
	<summary>See more</summary>
	
	notifyOfChangedSelectorsOldDict: oldDictionaryOrNil newDict: newDictionaryOrNil
	(oldDictionaryOrNil isNil and: [newDictionaryOrNil isNil])
		ifTrue: [^ self].
		
	oldDictionaryOrNil ifNil: [
		newDictionaryOrNil keysAndValuesDo: [ :el :cat |
			self notifyOfChangedSelector: el from: nil to: cat].
		^ self.
	].

	newDictionaryOrNil ifNil: [
		oldDictionaryOrNil keysAndValuesDo: [ :el :cat |
			self notifyOfChangedSelector: el from: cat to: nil].
		^ self.
	].
		
	oldDictionaryOrNil keysAndValuesDo: [ :el :cat | | newCat |
		newCat := newDictionaryOrNil at: el.
		self notifyOfChangedSelector: el from: cat to: newCat.
	]
</details>

#### ClassOrganizer>>#classify: element under: heading suppressIfDefault: aBoolean

Store the argument, element, in the category named heading. If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein


<details>
	<summary>See more</summary>
	
	classify: element under: heading suppressIfDefault: aBoolean
	| oldCat newCat |
	oldCat _ self categoryOfElement: element.
	SystemChangeNotifier uniqueInstance doSilently: [
		super classify: element under: heading suppressIfDefault: aBoolean].
	newCat _ self categoryOfElement: element.
	self notifyOfChangedSelector: element from: oldCat to: newCat.
</details>

#### ClassOrganizer>>#removeCategory: cat

Remove the category named, cat. Create an error notificiation if the category has any elements in it.


<details>
	<summary>See more</summary>
	
	removeCategory: cat 
	| oldCategories |
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeCategory: cat].
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#changeFromCategorySpecs: categorySpecs

Tokens is an array of categorySpecs as scanned from a browser 'reorganize' pane.


<details>
	<summary>See more</summary>
	
	changeFromCategorySpecs: categorySpecs
	| oldDict oldCategories |
	oldDict _ self elementCategoryDict.
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super changeFromCategorySpecs: categorySpecs].
	self notifyOfChangedSelectorsOldDict: oldDict newDict: self elementCategoryDict.
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#notifyOfChangedCategoriesFrom: oldCollectionOrNil to: newCollectionOrNil

<details>
	<summary>See more</summary>
	
	notifyOfChangedCategoriesFrom: oldCollectionOrNil to: newCollectionOrNil
	(self hasSubject and: [oldCollectionOrNil ~= newCollectionOrNil]) 
		ifTrue: [SystemChangeNotifier uniqueInstance classReorganized: self subject].
</details>

#### ClassOrganizer>>#notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil

<details>
	<summary>See more</summary>
	
	notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil
	(self hasSubject and: [oldNameOrNil ~= newNameOrNil]) 
		ifTrue: [SystemChangeNotifier uniqueInstance classReorganized: self subject].
</details>

#### ClassOrganizer>>#renameCategory: oldCatString toBe: newCatString

Rename a category. No action if new name already exists, or if old name does not exist.


<details>
	<summary>See more</summary>
	
	renameCategory: oldCatString toBe: newCatString
	| oldCat newCat oldElementsBefore oldElementsAfter |
	oldCat _ oldCatString asSymbol.
	newCat _ newCatString asSymbol.
	oldElementsBefore _ self listAtCategoryNamed: oldCat.
	SystemChangeNotifier uniqueInstance doSilently: [
		super renameCategory: oldCatString toBe: newCatString].
	oldElementsAfter _ (self listAtCategoryNamed: oldCat) asSet.
	oldElementsBefore do: [:each |
		(oldElementsAfter includes: each)
			ifFalse: [self notifyOfChangedSelector: each from: oldCat to: newCat].
	].
	self notifyOfChangedCategoryFrom: oldCat to: newCat.
</details>

#### ClassOrganizer>>#setDefaultList: anArray

<details>
	<summary>See more</summary>
	
	setDefaultList: anArray
	| oldDict oldCategories |
	oldDict _ self elementCategoryDict.
	oldCategories _ self categories copy.
	SystemChangeNotifier uniqueInstance doSilently: [
		super setDefaultList: anArray].
	self notifyOfChangedSelectorsOldDict: oldDict newDict: self elementCategoryDict.
	self notifyOfChangedCategoriesFrom: oldCategories to: self categories.
</details>

#### ClassOrganizer>>#removeElement: element

Remove the selector, element, from all categories.


<details>
	<summary>See more</summary>
	
	removeElement: element
	| oldCat |
	oldCat _ self categoryOfElement: element.
	SystemChangeNotifier uniqueInstance doSilently: [
		super removeElement: element].
	self notifyOfChangedSelector: element from: oldCat to: (self categoryOfElement: element).
</details>

## Metaclass

My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance. [Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus, Integer superclass == Number, and Integer class superclass == Number class. However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus, Object superclass == nil, and Object class superclass == Class.

### Methods
#### Metaclass>>#replaceObsoleteInstanceWith: newInstance

<details>
	<summary>See more</summary>
	
	replaceObsoleteInstanceWith: newInstance
	thisClass class == self ifTrue:[^self error:'I am fine, thanks'].
	newInstance class == self ifFalse:[^self error:'Not an instance of me'].
	thisClass _ newInstance.
</details>

#### Metaclass>>#bindingOf: varName

Answer the binding of some variable resolved in the scope of the receiver


<details>
	<summary>See more</summary>
	
	bindingOf: varName

	^thisClass classBindingOf: varName
</details>

#### Metaclass>>#testCaseClass

<details>
	<summary>See more</summary>
	
	testCaseClass

	^self soleInstance testCaseClass 
</details>

#### Metaclass>>#removeInstVarName: aString

Remove the argument, aString, as one of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."

	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray _ self instVarNames copyWithout: aString.
	newString _ ''.
	newArray do: [:aString2 | newString _ aString2 , ' ' , newString].
	self instanceVariableNames: newString
</details>

#### Metaclass>>#theNonMetaClass

Sent to a class or metaclass, always return the class


<details>
	<summary>See more</summary>
	
	theNonMetaClass
	"Sent to a class or metaclass, always return the class"

	^thisClass
</details>

#### Metaclass>>#subclasses

Answer the receiver's subclasses.


<details>
	<summary>See more</summary>
	
	subclasses
	"Answer the receiver's subclasses."
	thisClass
		ifNil: [^ #()].
	^thisClass subclasses 
		select:[:aSubclass| aSubclass isMeta not] 
		thenCollect:[:aSubclass| aSubclass class]

	"Metaclass allInstancesDo:
		[:m | Compiler evaluate: 'subclasses_nil' for: m logged: false]"
</details>

#### Metaclass>>#subclassesDoGently: aBlock

Evaluate aBlock for each of the receiver's immediate subclasses.


<details>
	<summary>See more</summary>
	
	subclassesDoGently: aBlock
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	thisClass subclassesDo: [:aSubclass |
		"The following test is for Class class which has to exclude
			the Metaclasses being subclasses of Class."
		aSubclass isInMemory ifTrue: [
			aSubclass isMeta ifFalse: [aBlock value: aSubclass class]]].
</details>

#### Metaclass>>#adoptInstance: oldInstance from: oldMetaClass

Recreate any existing instances of the argument, oldClass, as instances of the receiver, which is a newly changed class. Permute variables as necessary.


<details>
	<summary>See more</summary>
	
	adoptInstance: oldInstance from: oldMetaClass 
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary."
	thisClass class == self ifTrue:[^self error:'Metaclasses have only one instance'].
	oldMetaClass isMeta ifFalse:[^self error:'Argument must be Metaclass'].
	oldInstance class == oldMetaClass ifFalse:[^self error:'Not the class of argument'].
	^thisClass _ self 
		newInstanceFrom: oldInstance 
		variable: self isVariable 
		size: self instSize 
		map: (self instVarMappingFrom: oldMetaClass)
</details>

#### Metaclass>>#canZapMethodDictionary

Return true if it is safe to zap the method dictionary on #obsolete


<details>
	<summary>See more</summary>
	
	canZapMethodDictionary
	"Return true if it is safe to zap the method dictionary on #obsolete"
	^thisClass
		ifNil: [ true ]
		ifNotNil: [ thisClass canZapMethodDictionary ]
</details>

#### Metaclass>>#fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex

File a description of the receiver on aFileStream. If the boolean argument, moveSource, is true, then set the trailing bytes to the position of aFileStream and to fileIndex in order to indicate where to find the source code.


<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	^self fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: true
</details>

#### Metaclass>>#addSubclass: aClass

Do nothing.


<details>
	<summary>See more</summary>
	
	addSubclass: aClass
	"Do nothing."
</details>

#### Metaclass>>#allInstances

Answer all instances of the receiver.


<details>
	<summary>See more</summary>
	
	allInstances
	thisClass class == self ifTrue:[^Array with: thisClass].
	^super allInstances
</details>

#### Metaclass>>#subclassesDo: aBlock

Evaluate aBlock for each of the receiver's immediate subclasses.


<details>
	<summary>See more</summary>
	
	subclassesDo: aBlock
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	thisClass subclassesDo:[:aSubclass|
		"The following test is for Class class which has to exclude
		the Metaclasses being subclasses of Class."
		aSubclass isMeta ifFalse:[aBlock value: aSubclass class]].
</details>

#### Metaclass>>#category

Answer the system organization category for the receiver.


<details>
	<summary>See more</summary>
	
	category
	^thisClass category
</details>

#### Metaclass>>#addObsoleteSubclass: aClass

Do nothing.


<details>
	<summary>See more</summary>
	
	addObsoleteSubclass: aClass
	"Do nothing."
</details>

#### Metaclass>>#addInstVarName: aString

Add the argument, aString, as one of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString _ aString.
	self instVarNames do: [:aString2 | fullString _ aString2 , ' ' , fullString].
	self instanceVariableNames: fullString
</details>

#### Metaclass>>#allInstancesDo: aBlock

There should be only one


<details>
	<summary>See more</summary>
	
	allInstancesDo: aBlock
	"There should be only one"
	thisClass class == self ifTrue:[^aBlock value: thisClass].
	^super allInstancesDo: aBlock
</details>

#### Metaclass>>#storeDataOn: aDataStream

I don't get stored. Use a DiskProxy


<details>
	<summary>See more</summary>
	
	storeDataOn: aDataStream
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store a Class'
</details>

#### Metaclass>>#soleInstance

The receiver has only one instance. Answer it.


<details>
	<summary>See more</summary>
	
	soleInstance
	"The receiver has only one instance. Answer it."

	^thisClass
</details>

#### Metaclass>>#removeObsoleteSubclass: aClass

Do nothing.


<details>
	<summary>See more</summary>
	
	removeObsoleteSubclass: aClass
	"Do nothing."
</details>

#### Metaclass>>#definition

Refer to the comment in ClassDescription|definition.


<details>
	<summary>See more</summary>
	
	definition
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: [ :strm |
		strm print: self;
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString]
</details>

#### Metaclass>>#classPool

Answer the dictionary of class variables.


<details>
	<summary>See more</summary>
	
	classPool
	"Answer the dictionary of class variables."

	^thisClass classPool
</details>

#### Metaclass>>#removeSubclass: aClass

Do nothing.


<details>
	<summary>See more</summary>
	
	removeSubclass: aClass
	"Do nothing."
</details>

#### Metaclass>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a reference to a class in Smalltalk instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."

	dp _ DiskProxy global: self theNonMetaClass name selector: #class
			args: (Array new).
	refStrm replace: self with: dp.
	^ dp

</details>

#### Metaclass>>#isObsolete

Return true if the receiver is obsolete


<details>
	<summary>See more</summary>
	
	isObsolete
	"Return true if the receiver is obsolete"
	^thisClass == nil "Either no thisClass"
		or:[thisClass class ~~ self "or I am not the class of thisClass"
			or:[thisClass isObsolete]] "or my instance is obsolete"
</details>

#### Metaclass>>#localBindingOf: varNameSymbol

<details>
	<summary>See more</summary>
	
	localBindingOf: varNameSymbol

	^thisClass localBindingOf: varNameSymbol 
</details>

#### Metaclass>>#postCopy

Don't share the reference to the sole instance.


<details>
	<summary>See more</summary>
	
	postCopy
	"Don't share the reference to the sole instance."

	super postCopy.
	thisClass := nil.
</details>

#### Metaclass>>#possibleVariablesFor: misspelled continuedFrom: oldResults

<details>
	<summary>See more</summary>
	
	possibleVariablesFor: misspelled continuedFrom: oldResults

	^ thisClass possibleVariablesFor: misspelled continuedFrom: oldResults

</details>

#### Metaclass>>#fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool

<details>
	<summary>See more</summary>
	
	fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool
	super fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex.
	(aBool and:[moveSource not and: [self methodDict includesKey: #initialize]]) ifTrue: [
		aFileStream newLine.
		aFileStream newLine.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream newLine ]
</details>

#### Metaclass>>#new

The receiver can only have one instance. Create it or complain that one already exists.


<details>
	<summary>See more</summary>
	
	new
	"The receiver can only have one instance. Create it or complain that
	one already exists."

	thisClass class ~~ self
		ifTrue: [^thisClass _ self basicNew]
		ifFalse: [self error: 'A Metaclass should only have one instance!']
</details>

#### Metaclass>>#acceptsLoggingOfCompilation

Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set. The metaclass follows the rule of the class itself. 6/18/96 sw


<details>
	<summary>See more</summary>
	
	acceptsLoggingOfCompilation
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself.  6/18/96 sw"

	^ thisClass acceptsLoggingOfCompilation
</details>

#### Metaclass>>#isMeta

<details>
	<summary>See more</summary>
	
	isMeta
	^ true
</details>

#### Metaclass>>#fileOutInitializerOn: aStream

<details>
	<summary>See more</summary>
	
	fileOutInitializerOn: aStream
	(self methodDict includesKey: #initialize) ifTrue: [
		aStream newLine.
		aStream nextChunkPut: thisClass name , ' initialize'].
</details>

#### Metaclass>>#instanceVariableNames: instVarString

Declare additional named variables for my instance.


<details>
	<summary>See more</summary>
	
	instanceVariableNames: instVarString 
	"Declare additional named variables for my instance."
	| answer |
	answer _ ClassBuilder new
		class: self
		instanceVariableNames: instVarString.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
</details>

#### Metaclass>>#nonTrivial

Answer whether the receiver has any methods or instance variables.


<details>
	<summary>See more</summary>
	
	nonTrivial 
	"Answer whether the receiver has any methods or instance variables."

	^ self instVarNames size > 0 or: [self methodDict size > 0]
</details>

#### Metaclass>>#wantsRecompilationProgressReported

The metaclass follows the rule of the class itself.


<details>
	<summary>See more</summary>
	
	wantsRecompilationProgressReported
	"The metaclass follows the rule of the class itself."

	^ thisClass wantsRecompilationProgressReported
</details>

#### Metaclass>>#theMetaClass

Sent to a class or metaclass, always return the metaclass


<details>
	<summary>See more</summary>
	
	theMetaClass
	"Sent to a class or metaclass, always return the metaclass"

	^self
</details>

#### Metaclass>>#wantsChangeSetLogging

Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself. 7/12/96 sw


<details>
	<summary>See more</summary>
	
	wantsChangeSetLogging
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself.  7/12/96 sw"

	^ thisClass wantsChangeSetLogging
</details>

#### Metaclass>>#name

Answer a String that is the name of the receiver, either 'Metaclass' or the name of the receiver's class followed by ' class'.


<details>
	<summary>See more</summary>
	
	name
	"Answer a String that is the name of the receiver, either 'Metaclass' or 
	the name of the receiver's class followed by ' class'."

	^thisClass
		ifNil: [ 'a Metaclass']
		ifNotNil: [ thisClass name , ' class']
</details>

#### Metaclass>>#obsoleteSubclasses

Answer the receiver's subclasses.


<details>
	<summary>See more</summary>
	
	obsoleteSubclasses
	"Answer the receiver's subclasses."
	thisClass
		ifNil: [^ #()].
	^thisClass obsoleteSubclasses 
		select:[:aSubclass| aSubclass isMeta not] 
		thenCollect:[:aSubclass| aSubclass class]

	"Metaclass allInstancesDo:
		[:m | Compiler evaluate: 'subclasses_nil' for: m logged: false]"
</details>

## PoolDefinitionNotification

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### PoolDefinitionNotification>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: 'The pool dictionary ', name,' does not exist.',
						'\Do you want it automatically created?' withNewLines.
						
	^ self resume: shouldResume 
</details>

#### PoolDefinitionNotification>>#initializeNamed: aName

<details>
	<summary>See more</summary>
	
	initializeNamed: aName

	name := aName 
</details>

