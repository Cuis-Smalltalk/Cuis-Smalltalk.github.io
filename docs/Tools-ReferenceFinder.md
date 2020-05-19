## ClosureScanner

Closure scanners for Cuis. This package implements a Reference Finder and a Weight Tracer. The Reference Finder scans object memory to determine the locations from where an object is referenced. The Weight Tracer determines the closure of objects held solely from the given object. Reference Finder. Introduction. This tool traces the object reference graph recording how a given object is connected to a set of well known roots. This information can be useful to examine object relationships. In addition, the tool's results show why an object is not considered garbage by the system. Generally speaking, the only public use class is ReferenceFinder. The trace node classes are considered private implementation details. The reference explorer classes are provided to integrate the tool with the system's retrospection capabilities (e.g. the inspector). For the sake of convenience, you may also use the reference finder programmatically by evaluating expressions such as ReferenceFinder openExplorerOn: ReferenceFinder The result is a window with a tree that shows how the ReferenceFinder class is (eventually) referenced by the system's special object array. Advanced settings and usage. The reference finder implementation provides a few configuration switches available to the user. By default, the reference finder will scan using a breadth first strategy. This is the recommended mode of operation because of breadth first's usefulness compared to depth first. Moreover, the breadth first scan implementation has been optimized significantly, so in most cases it can go through an image in 1 second or less. However, if depth first is preferred, one can also write expressions such as (ReferenceFinder target: ReferenceFinder) useDepthFirst: true; scanClosure; openExplorerWindow In addition, the reference finder will scan through weak objects by default. If this it not desired, then one can also write expressions such as (ReferenceFinder target: ReferenceFinder) skipWeakObjects: true; scanClosure; openExplorerWindow Note that skipping weak objects will also skip scanning the instance variables of weak objects. The instance variables of weak objects are strong, as opposed to weak objects' indexed slots. Moreover, the reference finder can optionally skip references from certain objects. This is useful in situations such as when the reference finder's UI is open. At that point, invoking a rescan should not pick up references that exist solely because the UI is open. The functionality can be invoked programmatically with expressions such as (ReferenceFinder target: ReferenceFinder) scanClosureSkipping: (Array with: Smalltalk); openExplorerWindow Weight Tracer. The Weight Tracer works similarly to the Reference Finder. However, the answer is the subset of the given object's closure that is solely reachable from the given object. For example, in the following code, only 'inner' is reachable solely from the inner array (please run the code in a workspace). outer := Array with: nil with: 'outer' copy. inner := Array with: outer with: 'inner' copy. WeightTracer openExplorerOn: inner Implementation details. The reference finder generally traces through objects trying to find references to a given target. Currently, the reference finder does not scan references to classes via the object header. In the rare case that you trace a class object and find no references, try finding references to the class' instances. Starting with a root node, generally the reference finder dives through referents by creating additional nodes. Other than the root node, all nodes know their parent node. When a reference to the target node is found, a new node for the target backtracks through the parent chain recording the new path. Initially, paths are held in sets to avoid path information duplication. At the end of the trace, a clean up phase converts those sets into sorted arrays for faster, deterministic access. For the sake of efficiency, the implementation has several observable characteristics. Special care is taken to avoid repeatedly calling the mirror primitives on the same object. Nodes are created with the basicNew primitive directly to avoid megamorphic message sends (with the current implementation of new, that's two megamorphic message sends: basicNew, and initialize). The implementation has a special trace node for CompiledMethod so that class checks for CompiledMethod class are performed only once. The special instance of UndefinedObject, nil, is treated as a special case and is assumed not to refer to anything else. Similarly, instances of SmallInteger are treated as a special case. In some cases, the code tends to choose faster variants over simpler expressions. An example choice is the use of to:do: instead of do: and timesRepeat:. The breadth first scanning is optimized to avoid having a large scan queue around while references are being traced. This approach avoids having a very large object in the remember table during every incremental GC. Moreover, the queue segments can be recycled in some cases which leads to reduced garbage creation. This approach trades off some complexity in exchange for a significant performance improvement. The depth first scanning is implemented in a reasonably straightforward manner only. It is assumed that breadth first scanning is typically preferable over depth first scanning. Weight tracers work analogously.

### Methods
#### ClosureScanner>>#scanQueue

<details>
	<summary>See more</summary>
	
	scanQueue

	^scanQueue
</details>

#### ClosureScanner>>#storagePreallocatedCapacity

<details>
	<summary>See more</summary>
	
	storagePreallocatedCapacity

	^262144
</details>

#### ClosureScanner>>#scanQueueAddNewSegment

<details>
	<summary>See more</summary>
	
	scanQueueAddNewSegment

	| newSegment |
	self scanQueueRecycledSegment isNil
		ifTrue: [newSegment _ self newScanQueueSegment]
		ifFalse:
			[
				newSegment _ self scanQueueRecycledSegment.
				newSegment removeAll.
				self scanQueueRecycledSegment: nil
			].
	self scanQueue add: newSegment.
	self scanQueueLastSegment: newSegment
</details>

#### ClosureScanner>>#openExplorerWindow

Assumes closure scanning already done


<details>
	<summary>See more</summary>
	
	openExplorerWindow
	"Assumes closure scanning already done"

	| explorer window |
	explorer _ self explorerClass finder: self.
	window _ self explorerWindowClass
		open: explorer
		label: self explorerWindowPrefix, self target printString.
	window expandAll
</details>

#### ClosureScanner>>#rootNode

<details>
	<summary>See more</summary>
	
	rootNode

	^rootNode
</details>

#### ClosureScanner>>#explorerClass

<details>
	<summary>See more</summary>
	
	explorerClass

	self subclassResponsibility
</details>

#### ClosureScanner>>#scanQueueSegmentTargetSize

<details>
	<summary>See more</summary>
	
	scanQueueSegmentTargetSize

	^1024
</details>

#### ClosureScanner>>#scanQueueAdd: aNode

<details>
	<summary>See more</summary>
	
	scanQueueAdd: aNode

	self scanQueueLastSegment add: aNode.
	self scanQueueLastSegment size >= self scanQueueSegmentTargetSize
		ifTrue: [self scanQueueAddNewSegment]
</details>

#### ClosureScanner>>#shouldCloseOverCompiledMethod: anObject

Assume anObject is an instance of CompiledMethod, and further that compiled methods almost always have literals


<details>
	<summary>See more</summary>
	
	shouldCloseOverCompiledMethod: anObject
	"Assume anObject is an instance of CompiledMethod, and
	further that compiled methods almost always have literals"

	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true
</details>

#### ClosureScanner>>#breadthFirstScanClosure

Using to:do: is ~10% faster than timesRepeat:


<details>
	<summary>See more</summary>
	
	breadthFirstScanClosure
	"Using to:do: is ~10% faster than timesRepeat:"

	self scanQueueAdd: self rootNode.
	[self scanQueue first isEmpty] whileFalse:
		[
			| segment |
			segment _ self scanQueueRemoveFirstSegment.
			1 to: segment size do:
				[:some |
					| next |
					next _ segment at: some.
					next breadthFirstCloseOver: self target for: self.
				].
			self scanQueueRecycledSegment: segment
		]
</details>

#### ClosureScanner>>#skipClasses

For the time being


<details>
	<summary>See more</summary>
	
	skipClasses
	"For the time being"

	^true
</details>

#### ClosureScanner>>#scanClosure

<details>
	<summary>See more</summary>
	
	scanClosure

	self scanClosureSkipping: Array new
</details>

#### ClosureScanner>>#skipWeakObjects

Skipping weak objects will also skip objects referenced from weak objects' instance variables (which are strong)


<details>
	<summary>See more</summary>
	
	skipWeakObjects
	"Skipping weak objects will also skip objects referenced
	from weak objects' instance variables (which are strong)"

	^skipWeakObjects
</details>

#### ClosureScanner>>#newScanQueueSegment

Segment the scan queue, otherwise the GC will be forced to scan the whole queue every time (obviously, the scan queue will be in the remember table)


<details>
	<summary>See more</summary>
	
	newScanQueueSegment
	"Segment the scan queue, otherwise the GC will be forced to scan the whole
	queue every time (obviously, the scan queue will be in the remember table)"

	| newScanQueue |
	newScanQueue _ OrderedCollection new: self scanQueueSegmentTargetSize.
	newScanQueue resetTo: 1.
	^newScanQueue
</details>

#### ClosureScanner>>#defaultRootObject

<details>
	<summary>See more</summary>
	
	defaultRootObject

	^Smalltalk specialObjectsArray
</details>

#### ClosureScanner>>#useDepthFirst: aBoolean

<details>
	<summary>See more</summary>
	
	useDepthFirst: aBoolean

	useDepthFirst _ aBoolean
</details>

#### ClosureScanner>>#closureTraceNodeClass

<details>
	<summary>See more</summary>
	
	closureTraceNodeClass

	^ReferenceTraceNode
</details>

#### ClosureScanner>>#initializeScanQueue

<details>
	<summary>See more</summary>
	
	initializeScanQueue

	self scanQueueLastSegment: self newScanQueueSegment.
	self scanQueue: self newScanQueueSegment.
	self scanQueue add: self scanQueueLastSegment
</details>

#### ClosureScanner>>#rootNode: aNode

<details>
	<summary>See more</summary>
	
	rootNode: aNode

	rootNode _ aNode
</details>

#### ClosureScanner>>#useDepthFirst

<details>
	<summary>See more</summary>
	
	useDepthFirst

	^useDepthFirst
</details>

#### ClosureScanner>>#closureTraceCompiledMethodNodeClass

<details>
	<summary>See more</summary>
	
	closureTraceCompiledMethodNodeClass

	^ReferenceTraceCompiledMethodNode
</details>

#### ClosureScanner>>#skipWeakObjects: aBoolean

<details>
	<summary>See more</summary>
	
	skipWeakObjects: aBoolean

	skipWeakObjects _ aBoolean
</details>

#### ClosureScanner>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	self useDepthFirst: false.
	self skipWeakObjects: false
</details>

#### ClosureScanner>>#nodes: aDictionary

<details>
	<summary>See more</summary>
	
	nodes: aDictionary

	nodes _ aDictionary
</details>

#### ClosureScanner>>#explorerWindowPrefix

<details>
	<summary>See more</summary>
	
	explorerWindowPrefix

	^'Exploring '
</details>

#### ClosureScanner>>#nodes

<details>
	<summary>See more</summary>
	
	nodes

	^nodes
</details>

#### ClosureScanner>>#newRootNodeFor: rootObject
defaultNodeClass: traceNodeClass
compiledMethodNodeClass: compiledMethodNodeClass

<details>
	<summary>See more</summary>
	
	newRootNodeFor: rootObject
defaultNodeClass: traceNodeClass
compiledMethodNodeClass: compiledMethodNodeClass

	| rootObjectClass node |
	rootObjectClass _ ReferenceFinderMirror objectClass: rootObject.
	node _ CompiledMethod == rootObjectClass
		ifTrue: [compiledMethodNodeClass object: rootObject]
		ifFalse: [traceNodeClass object: rootObject].
	node classOfObject: rootObjectClass.
	^node
</details>

#### ClosureScanner>>#scanQueueRemoveFirstSegment

<details>
	<summary>See more</summary>
	
	scanQueueRemoveFirstSegment

	| answer |
	answer _ self scanQueue removeFirst.
	self scanQueue isEmpty ifTrue: [self scanQueueAddNewSegment].
	^answer
</details>

#### ClosureScanner>>#newRootNode

<details>
	<summary>See more</summary>
	
	newRootNode

	^self newRootNodeFor: self defaultRootObject
</details>

#### ClosureScanner>>#prepareToScanClosure

<details>
	<summary>See more</summary>
	
	prepareToScanClosure

	| mustCleanRootNode |
	mustCleanRootNode _ self rootNode notNil.
	self rootNode: self newRootNode.
	self nodes: self newNodes.
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifTrue: [Smalltalk garbageCollect]
</details>

#### ClosureScanner>>#scanQueueRecycledSegment: anOrderedCollection

<details>
	<summary>See more</summary>
	
	scanQueueRecycledSegment: anOrderedCollection

	scanQueueRecycledSegment _ anOrderedCollection
</details>

#### ClosureScanner>>#target: anObject

<details>
	<summary>See more</summary>
	
	target: anObject

	target _ anObject
</details>

#### ClosureScanner>>#cleanUpAfterScan

<details>
	<summary>See more</summary>
	
	cleanUpAfterScan

	self nodes: nil.
	self scanQueue: nil.
	self scanQueueLastSegment: nil.
	self scanQueueRecycledSegment: nil.
	self rootNode cleanUpAfterScan
</details>

#### ClosureScanner>>#shouldCloseOver: anObject instanceOf: aClass

Tracing assumes UndefinedObjects do not have referents. Also, note that sending isBits is faster than selecting the bits classes and doing linear search by a factor of about 3x. The SmallInteger is specialized because overall it achieves 5% performance gain. Finally, assume aClass is not CompiledMethod


<details>
	<summary>See more</summary>
	
	shouldCloseOver: anObject instanceOf: aClass
	"Tracing assumes UndefinedObjects do not have referents.  Also, note
	that sending isBits is faster than selecting the bits classes and doing linear
	search by a factor of about 3x.  The SmallInteger is specialized because
	overall it achieves 5% performance gain.  Finally, assume aClass is not
	CompiledMethod"

	nil == anObject ifTrue: [^false].
	(SmallInteger == aClass or: [aClass isBits]) ifTrue: [^false].
	((ReferenceFinderMirror objectSize: anObject) = 0
		and: [aClass instSize = 0])
			ifTrue: [^false].
	(self skipWeakObjects and: [aClass isWeak]) ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true
</details>

#### ClosureScanner>>#scanQueueRecycledSegment

<details>
	<summary>See more</summary>
	
	scanQueueRecycledSegment

	^scanQueueRecycledSegment
</details>

#### ClosureScanner>>#scanQueueLastSegment

<details>
	<summary>See more</summary>
	
	scanQueueLastSegment

	^scanQueueLastSegment
</details>

#### ClosureScanner>>#skipInternalNodesAnd: aCollection

Assumes no bitNodes required


<details>
	<summary>See more</summary>
	
	skipInternalNodesAnd: aCollection
	"Assumes no bitNodes required"

	self nodes
		add: self;
		add: self rootNode object;
		addAll: aCollection
</details>

#### ClosureScanner>>#scanQueueLastSegment: anOrderedCollection

<details>
	<summary>See more</summary>
	
	scanQueueLastSegment: anOrderedCollection

	scanQueueLastSegment _ anOrderedCollection
</details>

#### ClosureScanner>>#target

<details>
	<summary>See more</summary>
	
	target

	^target
</details>

#### ClosureScanner>>#newNodes

<details>
	<summary>See more</summary>
	
	newNodes

	^IdentitySet new: self storagePreallocatedCapacity
</details>

#### ClosureScanner>>#basicScanClosure

<details>
	<summary>See more</summary>
	
	basicScanClosure

	self useDepthFirst
		ifTrue: [self depthFirstScanClosure]
		ifFalse: [self breadthFirstScanClosure]
</details>

#### ClosureScanner>>#explorerWindowClass

<details>
	<summary>See more</summary>
	
	explorerWindowClass

	^ReferencesExplorerWindow
</details>

#### ClosureScanner>>#scanClosureSkipping: aCollection

<details>
	<summary>See more</summary>
	
	scanClosureSkipping: aCollection

	self prepareToScanClosure.
	self skipInternalNodesAnd: aCollection.
	self basicScanClosure.
	self cleanUpAfterScan
</details>

#### ClosureScanner>>#depthFirstScanClosure

<details>
	<summary>See more</summary>
	
	depthFirstScanClosure

	self rootNode depthFirstCloseOver: self target for: self
</details>

#### ClosureScanner>>#newRootNodeFor: rootObject

<details>
	<summary>See more</summary>
	
	newRootNodeFor: rootObject

	^self
		newRootNodeFor: rootObject
		defaultNodeClass: self closureTraceNodeClass
		compiledMethodNodeClass: self closureTraceCompiledMethodNodeClass
</details>

#### ClosureScanner>>#scanQueue: anOrderedCollection

<details>
	<summary>See more</summary>
	
	scanQueue: anOrderedCollection

	scanQueue _ anOrderedCollection
</details>

## ClosureTraceNode

See comments in ClosureScanner.

### Methods
#### ClosureTraceNode>>#recordPath

<details>
	<summary>See more</summary>
	
	recordPath

	self parent isNil ifTrue: [^self].
	self parent paths add: self.
	self parent recordPath
</details>

#### ClosureTraceNode>>#dumpOn: aStream depth: anInteger

<details>
	<summary>See more</summary>
	
	dumpOn: aStream depth: anInteger

	anInteger timesRepeat: [aStream tab].
	self printOn: aStream.
	aStream newLine.
	self paths do: [:each | each dumpOn: aStream depth: anInteger + 1]
</details>

#### ClosureTraceNode>>#object: anObject

<details>
	<summary>See more</summary>
	
	object: anObject

	object _ anObject
</details>

#### ClosureTraceNode>>#object

<details>
	<summary>See more</summary>
	
	object

	^object
</details>

#### ClosureTraceNode>>#classOfObject

<details>
	<summary>See more</summary>
	
	classOfObject

	^classOfObject
</details>

#### ClosureTraceNode>>#parent

<details>
	<summary>See more</summary>
	
	parent

	^parent
</details>

#### ClosureTraceNode>>#paths: aSet

<details>
	<summary>See more</summary>
	
	paths: aSet

	paths _ aSet
</details>

#### ClosureTraceNode>>#dumpString

<details>
	<summary>See more</summary>
	
	dumpString

	| answer |
	answer := String new writeStream.
	self dumpOn: answer.
	^answer contents
</details>

#### ClosureTraceNode>>#breadthFirstCloseOver: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
</details>

#### ClosureTraceNode>>#dumpOn: aStream

<details>
	<summary>See more</summary>
	
	dumpOn: aStream

	self dumpOn: aStream depth: 0
</details>

#### ClosureTraceNode>>#cleanUpAfterScan

<details>
	<summary>See more</summary>
	
	cleanUpAfterScan

	self paths: self paths asArray.
	self paths sort: [:x :y | self path: x comesBefore: y].
	self paths do: [:each | each cleanUpAfterScan]
</details>

#### ClosureTraceNode>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		nextPutAll: '> ';
		print: self object
</details>

#### ClosureTraceNode>>#classOfObject: anObject

<details>
	<summary>See more</summary>
	
	classOfObject: anObject

	classOfObject _ anObject
</details>

#### ClosureTraceNode>>#path: x comesBefore: y

<details>
	<summary>See more</summary>
	
	path: x comesBefore: y

	x object identityHash < y object identityHash ifTrue: [^true].
	x object identityHash > y object identityHash ifTrue: [^false].
	^(ReferenceFinderMirror objectClass: x) name
		<= (ReferenceFinderMirror objectClass: y) name
</details>

#### ClosureTraceNode>>#foundTargetAtReferent: aReferent

<details>
	<summary>See more</summary>
	
	foundTargetAtReferent: aReferent

	| node |
	node _ self class object: aReferent.
	node parent: self.
	node recordPath
</details>

#### ClosureTraceNode>>#parent: aTraceNode

<details>
	<summary>See more</summary>
	
	parent: aTraceNode

	parent _ aTraceNode
</details>

#### ClosureTraceNode>>#depthFirstCloseOver: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: anObject for: aFinder

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
</details>

#### ClosureTraceNode>>#paths

<details>
	<summary>See more</summary>
	
	paths

	paths isNil ifTrue: [self paths: Set new].
	^paths
</details>

## ReferenceFinder

See comments in ClosureScanner.

### Methods
#### ReferenceFinder>>#explorerWindowPrefix

<details>
	<summary>See more</summary>
	
	explorerWindowPrefix

	^'References to '
</details>

#### ReferenceFinder>>#explorerClass

<details>
	<summary>See more</summary>
	
	explorerClass

	^ReferencesExplorer
</details>

## ReferenceFinderMirror

This class exists solely to implement the mirror primitives in a more accessible place.

### Methods
## ReferenceTraceCompiledMethodNode

See comments in ClosureScanner.

### Methods
#### ReferenceTraceCompiledMethodNode>>#depthFirstCloseOver: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
</details>

#### ReferenceTraceCompiledMethodNode>>#breadthFirstCloseOver: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
</details>

## ReferenceTraceNode

See comments in ClosureScanner.

### Methods
#### ReferenceTraceNode>>#breadthFirstCloseOver: aTarget into: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	aFinder scanQueueAdd: node
</details>

#### ReferenceTraceNode>>#depthFirstCloseOver: aTarget into: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ ReferenceTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ ReferenceTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	node depthFirstCloseOver: aTarget for: aFinder
</details>

## ReferencesExplorer

See comments in ClosureScanner.

### Methods
#### ReferencesExplorer>>#rootObject: anObject

Assume a preloaded finder if the target matches


<details>
	<summary>See more</summary>
	
	rootObject: anObject
	"Assume a preloaded finder if the target matches"

	super rootObject: anObject.
	self finder target == anObject ifFalse:
		[
			self finder target: anObject.
			self rescan
		]
</details>

#### ReferencesExplorer>>#getList

<details>
	<summary>See more</summary>
	
	getList

	| wrapper |
	wrapper _ ReferencesExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash asString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper
</details>

#### ReferencesExplorer>>#finder

<details>
	<summary>See more</summary>
	
	finder

	finder isNil ifTrue: [self finder: ReferenceFinder new].
	^finder
</details>

#### ReferencesExplorer>>#rescan

<details>
	<summary>See more</summary>
	
	rescan

	self rescanSkipping: Array new
</details>

#### ReferencesExplorer>>#finder: aReferenceFinder

<details>
	<summary>See more</summary>
	
	finder: aReferenceFinder

	finder _ aReferenceFinder
</details>

#### ReferencesExplorer>>#rescanSkipping: aCollection

<details>
	<summary>See more</summary>
	
	rescanSkipping: aCollection

	self finder scanClosureSkipping: aCollection.
	self changed: #getList
</details>

## ReferencesExplorerWindow

See comments in ClosureScanner.

### Methods
#### ReferencesExplorerWindow>>#buildMorphicWindow

<details>
	<summary>See more</summary>
	
	buildMorphicWindow

	super buildMorphicWindow.
	listMorph autoExpand: true
</details>

## ReferencesExplorerWrapper

See comments in ClosureScanner.

### Methods
#### ReferencesExplorerWrapper>>#pathStringFrom: parentObject to: childObject count: anInteger

<details>
	<summary>See more</summary>
	
	pathStringFrom: parentObject to: childObject count: anInteger

	| answer referenceDescription |
	answer _ (String new: 32) writeStream.
	referenceDescription _ self referenceDescriptionFrom: parentObject to: childObject count: anInteger.
	self print: parentObject on: answer.
	answer
		nextPutAll: ' ';
		nextPutAll: referenceDescription;
		nextPutAll: ' --> '.
	self print: childObject on: answer.
	^answer contents
</details>

#### ReferencesExplorerWrapper>>#finderSkipsClasses

<details>
	<summary>See more</summary>
	
	finderSkipsClasses

	^finderSkipsClasses
</details>

#### ReferencesExplorerWrapper>>#referenceDescriptionFrom: parentObject to: childObject count: anInteger

<details>
	<summary>See more</summary>
	
	referenceDescriptionFrom: parentObject to: childObject count: anInteger

	| parentClass referencesSoFar |
	referencesSoFar _ 0.
	parentClass _ ReferenceFinderMirror objectClass: parentObject.
	(self finderSkipsClasses not and: [parentClass == childObject]) ifTrue:
		[
			referencesSoFar _ referencesSoFar + 1.
			referencesSoFar = anInteger ifTrue: [^'class']
		].
	parentClass == CompiledMethod
		ifTrue:
			[
				1 to: parentObject numLiterals do:
					[:eachIndex |
						| eachLiteral |
						eachLiteral _ parentObject literalAt: eachIndex.
						eachLiteral == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'literalAt: ', eachIndex printString]
							]
					]
			]
		ifFalse:
			[
				1 to: parentClass instSize do:
					[:eachIndex |
						| eachVariable |
						eachVariable _ ReferenceFinderMirror object: parentObject instVarAt: eachIndex.
						eachVariable == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^parentClass allInstVarNames at: eachIndex]
							]
					].
				1 to: (ReferenceFinderMirror objectSize: parentObject) do:
					[:eachIndex |
						| eachSlot |
						eachSlot _ ReferenceFinderMirror object: parentObject basicAt: eachIndex.
						eachSlot == childObject ifTrue:
							[
								referencesSoFar _ referencesSoFar + 1.
								referencesSoFar = anInteger ifTrue: [^'at: ', eachIndex printString]
							]
					]
			].
	"self halt."
	^'unknown (maybe the reference changed)'
</details>

#### ReferencesExplorerWrapper>>#contents

<details>
	<summary>See more</summary>
	
	contents

	| answer |
	answer _ Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name _ self pathStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer
</details>

#### ReferencesExplorerWrapper>>#finderSkipsClasses: aBoolean

<details>
	<summary>See more</summary>
	
	finderSkipsClasses: aBoolean

	finderSkipsClasses _ aBoolean
</details>

#### ReferencesExplorerWrapper>>#print: anObject on: aStream

<details>
	<summary>See more</summary>
	
	print: anObject on: aStream

	aStream
		nextPut: $(;
		print: anObject class;
		nextPut: $:;
		print: anObject identityHash;
		nextPut: $)
</details>

#### ReferencesExplorerWrapper>>#hasContents

<details>
	<summary>See more</summary>
	
	hasContents

	^model paths notEmpty
</details>

## WeightExplorer

See comments in ClosureScanner.

### Methods
#### WeightExplorer>>#getList

<details>
	<summary>See more</summary>
	
	getList

	| wrapper |
	wrapper _ WeightExplorerWrapper
		with: self finder rootNode object
		name: self rootObject identityHash asString
		model: self finder rootNode.
	wrapper finderSkipsClasses: self finder skipClasses.
	^Array with: wrapper
</details>

## WeightExplorerWrapper

See comments in ClosureScanner.

### Methods
#### WeightExplorerWrapper>>#contents

<details>
	<summary>See more</summary>
	
	contents

	| answer |
	answer _ Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name _ self weightStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer
</details>

#### WeightExplorerWrapper>>#weightStringFrom: parentObject to: childObject count: anInteger

<details>
	<summary>See more</summary>
	
	weightStringFrom: parentObject to: childObject count: anInteger

	| answer |
	answer _ (String new: 32) writeStream.
	self print: parentObject on: answer.
	^answer contents
</details>

## WeightTraceCompiledMethodNode

See comments in ClosureScanner.

### Methods
#### WeightTraceCompiledMethodNode>>#depthFirstCloseOver: anObject for: aFinder

Bit objects must be counted, but not traced into


<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
</details>

#### WeightTraceCompiledMethodNode>>#breadthFirstCloseOver: anObject for: aFinder

Bit objects must be counted, but not traced into


<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: anObject for: aFinder

	self object literalsDo:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: each
				for: aFinder
		]
</details>

## WeightTraceNode

See comments in ClosureScanner.

### Methods
#### WeightTraceNode>>#depthFirstCloseOver: aTarget into: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	node depthFirstCloseOver: aTarget for: aFinder
</details>

#### WeightTraceNode>>#depthFirstCloseOver: anObject for: aFinder

Bit objects must be counted, but not traced into


<details>
	<summary>See more</summary>
	
	depthFirstCloseOver: anObject for: aFinder
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				depthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
</details>

#### WeightTraceNode>>#breadthFirstCloseOver: anObject for: aFinder

Bit objects must be counted, but not traced into


<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: anObject for: aFinder
	"Bit objects must be counted, but not traced into"

	1 to: self classOfObject instSize do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object instVarAt: each)
				for: aFinder
		].
	self classOfObject isBits ifTrue: [^self].
	1 to: (ReferenceFinderMirror objectSize: self object) do:
		[:each |
			self
				breadthFirstCloseOver: anObject
				into: (ReferenceFinderMirror object: self object basicAt: each)
				for: aFinder
		]
</details>

#### WeightTraceNode>>#breadthFirstCloseOver: aTarget into: anObject for: aFinder

<details>
	<summary>See more</summary>
	
	breadthFirstCloseOver: aTarget into: anObject for: aFinder

	| node referentClass |
	anObject == aTarget ifTrue: [^self foundTargetAtReferent: anObject].
	referentClass _ ReferenceFinderMirror objectClass: anObject.
	CompiledMethod == referentClass
		ifTrue:
			[
				(aFinder shouldCloseOverCompiledMethod: anObject) ifFalse: [^self].
				node _ WeightTraceCompiledMethodNode object: anObject
			]
		ifFalse:
			[
				(aFinder shouldCloseOver: anObject instanceOf: referentClass) ifFalse: [^self].
				node _ WeightTraceNode object: anObject
			].
	node classOfObject: referentClass.
	node parent: self.
	self paths add: node.
	aFinder scanQueueAdd: node
</details>

## WeightTracer

See comments in ClosureScanner.

### Methods
#### WeightTracer>>#shouldCloseOver: anObject instanceOf: aClass

Bit objects must be counted, but not traced into


<details>
	<summary>See more</summary>
	
	shouldCloseOver: anObject instanceOf: aClass
	"Bit objects must be counted, but not traced into"

	nil == anObject ifTrue: [^false].
	SmallInteger == aClass ifTrue: [^false].
	(self nodes includes: anObject) ifTrue: [^false].
	self nodes add: anObject.
	^true
</details>

#### WeightTracer>>#explorerWindowPrefix

<details>
	<summary>See more</summary>
	
	explorerWindowPrefix

	^'Objects held solely from '
</details>

#### WeightTracer>>#prepareToWeighClosure

<details>
	<summary>See more</summary>
	
	prepareToWeighClosure

	| mustCleanRootNode |
	mustCleanRootNode _ self rootNode notNil.
	self rootNode: (self newRootNodeFor: self target).
	self useDepthFirst ifFalse: [self initializeScanQueue].
	mustCleanRootNode ifFalse: [Smalltalk garbageCollect]
</details>

#### WeightTracer>>#closureTraceNodeClass

<details>
	<summary>See more</summary>
	
	closureTraceNodeClass

	^WeightTraceNode
</details>

#### WeightTracer>>#explorerClass

<details>
	<summary>See more</summary>
	
	explorerClass

	^WeightExplorer
</details>

#### WeightTracer>>#scanClosureSkipping: anArray

<details>
	<summary>See more</summary>
	
	scanClosureSkipping: anArray

	self prepareToScanClosure.
	self skipInternalNodesAnd: Array new.
	self basicScanClosure.
	self prepareToWeighClosure.
	self skipInternalNodesAnd: anArray.
	self basicScanClosure.
	self cleanUpAfterScan
</details>

#### WeightTracer>>#closureTraceCompiledMethodNodeClass

<details>
	<summary>See more</summary>
	
	closureTraceCompiledMethodNodeClass

	^WeightTraceCompiledMethodNode
</details>

