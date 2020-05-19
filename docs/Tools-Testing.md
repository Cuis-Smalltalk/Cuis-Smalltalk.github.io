## ProgressiveTestRunner

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ProgressiveTestRunner>>#registerTestSuiteAction

<details>
	<summary>See more</summary>
	
	registerTestSuiteAction
		
	testSuite when: #changed: send: #testRun: to: self
</details>

#### ProgressiveTestRunner>>#informNoTestToRun

<details>
	<summary>See more</summary>
	
	informNoTestToRun
		
	 PopUpMenu inform: 'No test to run'
</details>

#### ProgressiveTestRunner>>#value

<details>
	<summary>See more</summary>
	
	value

	Utilities authorInitials.
	testsStream _ ReadStream on: testSuite tests.
	testsStream atEnd
		ifTrue: [ self informNoTestToRun ]
	 	ifFalse:[ self createProgressBarAndRun ]
</details>

#### ProgressiveTestRunner>>#showDeffects

<details>
	<summary>See more</summary>
	
	showDeffects
	
	 | defects |
			
	defects _ testResult defects.
	defects size = 1
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultWindow]
</details>

#### ProgressiveTestRunner>>#testRun: aTest

<details>
	<summary>See more</summary>
	
	testRun: aTest

	self updateProgressBarSubLabel.
	self updateDoneIncrement 
	
		
</details>

#### ProgressiveTestRunner>>#initializeFor: aTestSuite

<details>
	<summary>See more</summary>
	
	initializeFor: aTestSuite

	testSuite _ aTestSuite.
	
</details>

#### ProgressiveTestRunner>>#createProgressBar

<details>
	<summary>See more</summary>
	
	createProgressBar
		
	progressBar _ ProgressMorph label: testSuite name.
	self calculateTestRunIncrement.
	self updateProgressBarSubLabel.
	
</details>

#### ProgressiveTestRunner>>#updateDoneIncrement

<details>
	<summary>See more</summary>
	
	updateDoneIncrement 

 	progressBar incrDone: testRunIncrement 
	
</details>

#### ProgressiveTestRunner>>#updateProgressBarSubLabel

<details>
	<summary>See more</summary>
	
	updateProgressBarSubLabel

	testsStream atEnd ifFalse: [
		progressBar subLabel: testsStream next printString, ' (', testsStream position printString, '/', testsStream size printString, ')' ].
</details>

#### ProgressiveTestRunner>>#runSuiteShowingProgress

<details>
	<summary>See more</summary>
	
	runSuiteShowingProgress

	[ self registerTestSuiteAction. 
	progressBar openInWorld.
	self runSuite ] ensure: [
		self unregisterTestSuiteAction.
		UISupervisor whenUIinSafeState: [progressBar dismissMorph] ].
	
</details>

#### ProgressiveTestRunner>>#calculateTestRunIncrement

<details>
	<summary>See more</summary>
	
	calculateTestRunIncrement
	
	testRunIncrement _ 1/testsStream size
</details>

#### ProgressiveTestRunner>>#createProgressBarAndRun

<details>
	<summary>See more</summary>
	
	createProgressBarAndRun

	self createProgressBar.
	[ self runSuiteShowingProgress ] fork
</details>

#### ProgressiveTestRunner>>#runSuite

<details>
	<summary>See more</summary>
	
	runSuite
		
	testResult _ testSuite run.	
	testResult hasPassed 
		ifTrue: [ self informAllTestPassed ] 
		ifFalse: [self showDeffects ]
	
</details>

#### ProgressiveTestRunner>>#openTestResultWindow

<details>
	<summary>See more</summary>
	
	openTestResultWindow

	TestResultWindow openFor: testResult 
	
</details>

#### ProgressiveTestRunner>>#informAllTestPassed

<details>
	<summary>See more</summary>
	
	informAllTestPassed 
	
	PopUpMenu inform: testResult printString.
	
</details>

#### ProgressiveTestRunner>>#unregisterTestSuiteAction

<details>
	<summary>See more</summary>
	
	unregisterTestSuiteAction
	
	testSuite releaseActionMap 
</details>

## ResumableTestFailure

A ResumableTestFailure triggers a TestFailure, but lets execution of the TestCase continue. this is useful when iterating through collections, and #assert: ing on each element. in combination with methods like testcase>>#assert:description:, this lets you run through a whole collection and note which tests pass. here''s an example: (1 to: 30) do: [ :each | self assert: each odd description: each printString, ' is even' resumable: true] for each element where #odd returns <false>, the element will be printed to the Transcript.

### Methods
#### ResumableTestFailure>>#sunitExitWith: aValue

<details>
	<summary>See more</summary>
	
	sunitExitWith: aValue
	self resume: aValue
</details>

#### ResumableTestFailure>>#isResumable

Of course a ResumableTestFailure is resumable ;-)


<details>
	<summary>See more</summary>
	
	isResumable
	"Of course a ResumableTestFailure is resumable ;-)"

	^true
</details>

## SUnitNameResolver

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## TestCase

A TestCase is a Command representing the future running of a test case. Create one with the class method #selector: aSymbol, passing the name of the method to be run when the test case runs. When you discover a new fixture, subclass TestCase, declare instance variables for the objects in the fixture, override #setUp to initialize the variables, and possibly override# tearDown to deallocate any external resources allocated in #setUp. When you are writing a test case method, send #assert: aBoolean when you want to check for an expected value. For example, you might say "self assert: socket isOpen" to test whether or not a socket is open at a point in a test.

### Methods
#### TestCase>>#assert: aBoolean description: aStringOrBlock

Throw an assertion error if aBlock does not evaluates to true.


<details>
	<summary>See more</summary>
	
	assert: aBoolean description: aStringOrBlock
	aBoolean ifFalse: [
		failureString _ aStringOrBlock value.
		self logFailure: failureString.
		TestResult failure sunitSignalWith: failureString]
			
</details>

#### TestCase>>#debugAsFailureIfCanNot: handler

<details>
	<summary>See more</summary>
	
	debugAsFailureIfCanNot: handler
	
	self ifCanNotDebugDo: [ ^handler value].
	
	(self class selector: testSelector) runCaseAsFailure
</details>

#### TestCase>>#debug

<details>
	<summary>See more</summary>
	
	debug

	self setUpResources.
	
	[(self class selector: testSelector) runCase] ensure: [self tearDownResources]
			
</details>

#### TestCase>>#setTestSelector: aSymbol

<details>
	<summary>See more</summary>
	
	setTestSelector: aSymbol
	testSelector := aSymbol
			
</details>

#### TestCase>>#assert: aNumber isCloseTo: anotherNumber

<details>
	<summary>See more</summary>
	
	assert: aNumber isCloseTo: anotherNumber 

	self assert: aNumber isCloseTo: anotherNumber withPrecision: self defaultPrecision 
</details>

#### TestCase>>#is: aNumber closeTo: anotherNumber withPrecision: aPrecision

This way of comparing numbers could be useful for many tests, but there is no single correct way to do it for numerical algorithms. That's why this method is here and not at Float.


<details>
	<summary>See more</summary>
	
	is: aNumber closeTo: anotherNumber withPrecision: aPrecision
	"This way of comparing numbers could be useful for many tests, but there is no single correct way to do it for numerical algorithms. That's why this method is here and not at Float."

	aNumber = 0 ifTrue: [ ^ anotherNumber abs < aPrecision ].
	
	^ (aNumber - anotherNumber) abs < (aPrecision * (aNumber abs max: anotherNumber abs))
</details>

#### TestCase>>#assert: anAction changes: aCondition from: anInitialObject to: aFinalObject

<details>
	<summary>See more</summary>
	
	assert: anAction changes: aCondition from: anInitialObject to: aFinalObject 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: before equals: anInitialObject.
	self assert: after equals: aFinalObject
</details>

#### TestCase>>#assert: aNumber isCloseTo: anotherNumber withPrecision: aPrecision

<details>
	<summary>See more</summary>
	
	assert: aNumber isCloseTo: anotherNumber withPrecision: aPrecision 

	self assert: (self is: aNumber closeTo: anotherNumber withPrecision: aPrecision)
</details>

#### TestCase>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: (testSelector ifNil: [ 'unknown' ])
			
</details>

#### TestCase>>#tearDownResources

<details>
	<summary>See more</summary>
	
	tearDownResources
	
	self resources do: [:each | each reset]
</details>

#### TestCase>>#run: aResult

<details>
	<summary>See more</summary>
	
	run: aResult
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ]
			
</details>

#### TestCase>>#should: aBlock description: aString

<details>
	<summary>See more</summary>
	
	should: aBlock description: aString

	self assert: aBlock value description: aString
			
</details>

#### TestCase>>#shouldFail: aBlock

<details>
	<summary>See more</summary>
	
	shouldFail: aBlock
	
	self should: aBlock raise: Exception 
</details>

#### TestCase>>#signalFailure: aString

<details>
	<summary>See more</summary>
	
	signalFailure: aString

	failureString _ aString.
	TestResult failure sunitSignalWith: aString
</details>

#### TestCase>>#signalCanNotDebugMethod

<details>
	<summary>See more</summary>
	
	signalCanNotDebugMethod 

	self error: self canNotDebugMethodErrorDescription
</details>

#### TestCase>>#errored: anError

<details>
	<summary>See more</summary>
	
	errored: anError
	raisedError _ anError
</details>

#### TestCase>>#should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription

<details>
	<summary>See more</summary>
	
	should: aBlock raise: anExceptionHandlingCondition withExceptionDo: assertionsBlock description: aFailDescription

	| passed |
	passed := [aBlock value.  false]
		on: anExceptionHandlingCondition
		do: [:ex | assertionsBlock value: ex.  ex return: true].
	passed ifFalse: [self failWith: aFailDescription]
</details>

#### TestCase>>#assert: anAction changes: aCondition by: aDifference

<details>
	<summary>See more</summary>
	
	assert: anAction changes: aCondition by: aDifference 
	
	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before + aDifference
</details>

#### TestCase>>#should: aBlock raise: anExceptonHandlingCondition

<details>
	<summary>See more</summary>
	
	should: aBlock raise: anExceptonHandlingCondition 

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [ :anException | ]
			
</details>

#### TestCase>>#defaultFailDescription

<details>
	<summary>See more</summary>
	
	defaultFailDescription

	^'Test failed'
</details>

#### TestCase>>#shouldnt: aBlock raise: anExceptonHandlingCondition description: aFailDescription

<details>
	<summary>See more</summary>
	
	shouldnt: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	aBlock 
		on: anExceptonHandlingCondition 
		do: [ :anException | self failWith: aFailDescription ]

</details>

#### TestCase>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #TestCase or: [ super is: aSymbol ]
</details>

#### TestCase>>#addDependentToHierachy: anObject

an empty method. for Composite compability with TestSuite


<details>
	<summary>See more</summary>
	
	addDependentToHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			
</details>

#### TestCase>>#assert: aFloatNumber isNotCloseTo: anotherFloatNumber withPrecision: aPrecision

<details>
	<summary>See more</summary>
	
	assert: aFloatNumber isNotCloseTo: anotherFloatNumber withPrecision: aPrecision 

	self deny: (self is: aFloatNumber closeTo: anotherFloatNumber withPrecision: aPrecision)

</details>

#### TestCase>>#assert: aBooleanOrBlock

Throw an assertion error if aBlock does not evaluates to true.


<details>
	<summary>See more</summary>
	
	assert: aBooleanOrBlock

	aBooleanOrBlock value ifFalse: [self signalFailure: 'Assertion failed']
			
</details>

#### TestCase>>#deny: aBoolean description: aString resumable: resumableBoolean

<details>
	<summary>See more</summary>
	
	deny: aBoolean description: aString resumable: resumableBoolean 
	self
		assert: aBoolean not
		description: aString
		resumable: resumableBoolean
			
</details>

#### TestCase>>#shouldnt: aBlock

<details>
	<summary>See more</summary>
	
	shouldnt: aBlock

	self deny: aBlock value
			
</details>

#### TestCase>>#runCaseAsFailure

<details>
	<summary>See more</summary>
	
	runCaseAsFailure

	self setUpResources.
	self setUp.
	
	self openDebuggerOnFailingTestMethod
</details>

#### TestCase>>#runCase

<details>
	<summary>See more</summary>
	
	runCase

	[
		self setUp.
		self performTest] ensure: [self tearDown]
			
</details>

#### TestCase>>#removeDependentFromHierachy: anObject

an empty method. for Composite compability with TestSuite


<details>
	<summary>See more</summary>
	
	removeDependentFromHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"


			
</details>

#### TestCase>>#run

<details>
	<summary>See more</summary>
	
	run
	| result |
	result := TestResult new.
	self run: result.
	^result
			
</details>

#### TestCase>>#fail

<details>
	<summary>See more</summary>
	
	fail

	^self failWith: self defaultFailDescription 
</details>

#### TestCase>>#assert: aBoolean description: aString resumable: resumableBoolean

<details>
	<summary>See more</summary>
	
	assert: aBoolean description: aString resumable: resumableBoolean 
	| exception |
	aBoolean
		ifFalse: [		
			failureString _ aString.
			self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]
			
</details>

#### TestCase>>#failWith: aDescription

<details>
	<summary>See more</summary>
	
	failWith: aDescription

	self signalFailure: aDescription 
</details>

#### TestCase>>#assert: aNumber isNotCloseTo: anotherNumber

<details>
	<summary>See more</summary>
	
	assert: aNumber isNotCloseTo: anotherNumber
 
	self assert: aNumber isNotCloseTo: anotherNumber withPrecision: self defaultPrecision 
</details>

#### TestCase>>#assert: aCollection includes: anObject

<details>
	<summary>See more</summary>
	
	assert: aCollection includes: anObject 
	
	^ self assert: (aCollection includes: anObject) description: [ aCollection asString, ' does not include ', anObject asString ]
</details>

#### TestCase>>#shouldnt: aBlock raise: anExceptonHandlingCondition

<details>
	<summary>See more</summary>
	
	shouldnt: aBlock raise: anExceptonHandlingCondition 

	self shouldnt: aBlock raise: anExceptonHandlingCondition description: anExceptonHandlingCondition printString, ' was not expected to be raised'
</details>

#### TestCase>>#methodForTest

Can not call it testMethod because it will be detected as test - Hernan


<details>
	<summary>See more</summary>
	
	methodForTest

	"Can not call it testMethod because it will be detected as test - Hernan" 

	^self class lookupSelector: self selector
</details>

#### TestCase>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^testSelector
			
</details>

#### TestCase>>#failureLog

<details>
	<summary>See more</summary>
	
	failureLog	
	^Transcript	
</details>

#### TestCase>>#failureString

<details>
	<summary>See more</summary>
	
	failureString
	^failureString
</details>

#### TestCase>>#comparingStringBetween: expected and: actual

<details>
	<summary>See more</summary>
	
	comparingStringBetween: expected and: actual
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Expected ';
			nextPutAll: (expected printStringLimitedTo: 50);
			nextPutAll: ' but was ';
			nextPutAll: (actual printStringLimitedTo: 50);
			nextPutAll: '.'
		]
</details>

#### TestCase>>#canNotDebugMethodErrorDescription

<details>
	<summary>See more</summary>
	
	canNotDebugMethodErrorDescription

	^self class canNotDebugMethodErrorDescription
</details>

#### TestCase>>#ifCanNotDebugDo: handler

<details>
	<summary>See more</summary>
	
	ifCanNotDebugDo: handler

	^self methodForTest isQuick ifTrue: handler
</details>

#### TestCase>>#isSameAs: aTestCase

<details>
	<summary>See more</summary>
	
	isSameAs: aTestCase

	^self class = aTestCase class and: [ testSelector = aTestCase selector ]
</details>

#### TestCase>>#defaultPrecision

<details>
	<summary>See more</summary>
	
	defaultPrecision

	^ 0.0001
	
</details>

#### TestCase>>#should: aBlock

<details>
	<summary>See more</summary>
	
	should: aBlock

	self assert: aBlock value
			
</details>

#### TestCase>>#should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: anExpectedErrorMessageCreator

<details>
	<summary>See more</summary>
	
	should: aBlockToFail raise: anExceptionHandlingCondition withMessageText: anExpectedErrorMessageCreator

	self 
		should: aBlockToFail 
		raise: anExceptionHandlingCondition 
		withExceptionDo: [ :anException | self assert: anExpectedErrorMessageCreator value equals: anException messageText ]
</details>

#### TestCase>>#assert: expected equals: actual

<details>
	<summary>See more</summary>
	
	assert: expected equals: actual
	^ self
		assert: (expected = actual)
		description: (self comparingStringBetween: expected and: actual)

</details>

#### TestCase>>#deny: aBoolean

<details>
	<summary>See more</summary>
	
	deny: aBoolean

	self assert: aBoolean not
			
</details>

#### TestCase>>#setUp

<details>
	<summary>See more</summary>
	
	setUp
			
</details>

#### TestCase>>#should: aBlock raise: anExceptonHandlingCondition description: aFailDescription

<details>
	<summary>See more</summary>
	
	should: aBlock raise: anExceptonHandlingCondition description: aFailDescription

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: [:anException | ] description: aFailDescription
</details>

#### TestCase>>#setUpResources

<details>
	<summary>See more</summary>
	
	setUpResources
			
	self resources do: [:res | res isAvailable ifFalse: [^res signalInitializationError]].

</details>

#### TestCase>>#performTest

<details>
	<summary>See more</summary>
	
	performTest

	self perform: testSelector asSymbol
			
</details>

#### TestCase>>#should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock

<details>
	<summary>See more</summary>
	
	should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock

	self should: aBlock raise: anExceptonHandlingCondition withExceptionDo: assertionsBlock description: self defaultFailDescription
</details>

#### TestCase>>#openDebuggerOnFailingTestMethod

<details>
	<summary>See more</summary>
	
	openDebuggerOnFailingTestMethod

	| processToDebug context compiledMethod debugger |

	compiledMethod _ self methodForTest.
	
	processToDebug _ [ [ self performTest ] ensure: [
		self tearDown.
		self tearDownResources]] newProcess.
	context _ processToDebug suspendedContext.
	
	debugger _ Debugger new
		process: processToDebug 
		context: context.
	debugger openFullNoSuspendLabel: 'Debug failed test ', self printString.

	[debugger interruptedContext method == compiledMethod] whileFalse: [debugger send].

</details>

#### TestCase>>#shouldntFail: aBlock

<details>
	<summary>See more</summary>
	
	shouldntFail: aBlock

	self shouldnt: aBlock raise: TestResult exError
</details>

#### TestCase>>#resources

<details>
	<summary>See more</summary>
	
	resources
	| allResources resourceQueue |
	allResources := Set new.
	resourceQueue := OrderedCollection new.
	resourceQueue addAll: self class resources.
	[resourceQueue isEmpty] whileFalse: [
		| next |
		next := resourceQueue removeFirst.
		allResources add: next.
		resourceQueue addAll: next resources].
	^allResources
			
</details>

#### TestCase>>#tearDown

<details>
	<summary>See more</summary>
	
	tearDown
			
</details>

#### TestCase>>#assert: anAction doesNotChange: aCondition

<details>
	<summary>See more</summary>
	
	assert: anAction doesNotChange: aCondition 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self assert: after equals: before
</details>

#### TestCase>>#should: aClosure notTakeMoreThan: aLimit

<details>
	<summary>See more</summary>
	
	should: aClosure notTakeMoreThan: aLimit 

	| millisecondsLimit |
	
	millisecondsLimit := aLimit totalMilliseconds.
	self assert: aClosure timeToRun <= millisecondsLimit description: [ 'Took more than ', millisecondsLimit printString, ' milliseconds' ]
</details>

#### TestCase>>#debugAsFailure

<details>
	<summary>See more</summary>
	
	debugAsFailure
	
	^self debugAsFailureIfCanNot: [ self signalCanNotDebugMethod ]
</details>

#### TestCase>>#assert: anAction changes: aCondition

<details>
	<summary>See more</summary>
	
	assert: anAction changes: aCondition 

	| after before |
	
	before := aCondition value.
	anAction value.
	after := aCondition value.
	
	self deny: after = before
</details>

#### TestCase>>#isLogging

Log to Transcript


<details>
	<summary>See more</summary>
	
	isLogging
	"Log to Transcript"
	^true
</details>

#### TestCase>>#shouldnt: aBlock description: aString

<details>
	<summary>See more</summary>
	
	shouldnt: aBlock description: aString

	self deny: aBlock value description: aString
			
</details>

#### TestCase>>#deny: aBoolean description: aString

<details>
	<summary>See more</summary>
	
	deny: aBoolean description: aString

	self assert: aBoolean not description: aString
			
</details>

#### TestCase>>#logFailure: aString

<details>
	<summary>See more</summary>
	
	logFailure: aString
	| log |
	self isLogging ifTrue: [
		log _ self failureLog.
		log newLine.
		self printOn: log.
		log nextPutAll: ' -- '.
		log nextPutAll: aString ]
</details>

#### TestCase>>#raisedError

<details>
	<summary>See more</summary>
	
	raisedError
	^raisedError
</details>

## TestFailure

Signaled in case of a failed test (failure). The test framework distinguishes between failures and errors. A failure is anticipated and checked for with assertions. Errors are unanticipated problems like a division by 0 or an index out of bounds ...

### Methods
#### TestFailure>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	
	^ false
</details>

#### TestFailure>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self noHandler
</details>

## TestResource

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TestResource>>#isUnavailable

override to provide information on the readiness of the resource


<details>
	<summary>See more</summary>
	
	isUnavailable
	"override to provide information on the
	readiness of the resource"
	
	^self isAvailable not
			
</details>

#### TestResource>>#setUp

Does nothing. Subclasses should override this to initialize their resource


<details>
	<summary>See more</summary>
	
	setUp
	"Does nothing. Subclasses should override this
	to initialize their resource"
			
</details>

#### TestResource>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	self setUp

			
</details>

#### TestResource>>#description: aString

<details>
	<summary>See more</summary>
	
	description: aString

	description := aString
			
</details>

#### TestResource>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream nextPutAll: self class printString
			
</details>

#### TestResource>>#description

<details>
	<summary>See more</summary>
	
	description

	^description ifNil: [ '' ]
</details>

#### TestResource>>#resources

<details>
	<summary>See more</summary>
	
	resources
	^self class resources
			
</details>

#### TestResource>>#signalInitializationError

<details>
	<summary>See more</summary>
	
	signalInitializationError
	^self class signalInitializationError
			
</details>

#### TestResource>>#isAvailable

override to provide information on the readiness of the resource


<details>
	<summary>See more</summary>
	
	isAvailable
	"override to provide information on the
	readiness of the resource"
	
	^true
			
</details>

#### TestResource>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^name ifNil: [ self printString]
</details>

#### TestResource>>#tearDown

Does nothing. Subclasses should override this to tear down their resource


<details>
	<summary>See more</summary>
	
	tearDown
	"Does nothing. Subclasses should override this
	to tear down their resource"
			
</details>

#### TestResource>>#name: aString

<details>
	<summary>See more</summary>
	
	name: aString

	name := aString
			
</details>

## TestResult

This is a Collecting Parameter for the running of a bunch of tests. TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. Kent has seen TestResults that recorded coverage information and that sent email when they were done.

### Methods
#### TestResult>>#isPassed: aTestCase

<details>
	<summary>See more</summary>
	
	isPassed: aTestCase

	^self passed includes: aTestCase
			
</details>

#### TestResult>>#isError: aTestCase

<details>
	<summary>See more</summary>
	
	isError: aTestCase

	^self errors includes: aTestCase
			
</details>

#### TestResult>>#isFailure: aTestCase

<details>
	<summary>See more</summary>
	
	isFailure: aTestCase
	^self failures includes: aTestCase
			
</details>

#### TestResult>>#removeFromDefectsAndAddToPassed: aPassed

<details>
	<summary>See more</summary>
	
	removeFromDefectsAndAddToPassed: aPassed 

	errors 
		detect: [ :anError | anError isSameAs: aPassed ]
		ifFound: [ :anError | errors remove: anError ]
		ifNone: [ 
			failures 
				detect: [ :aFail | aFail isSameAs: aPassed ]
				ifFound: [ :aFail | failures remove: aFail ]
				ifNone: [ self error: aPassed printString, ' is not an error nor a failure' ]].
	passed add: aPassed

</details>

#### TestResult>>#correctCount

depreciated - use #passedCount


<details>
	<summary>See more</summary>
	
	correctCount
	"depreciated - use #passedCount"

	^self passedCount
			
</details>

#### TestResult>>#runCase: aTestCase

<details>
	<summary>See more</summary>
	
	runCase: aTestCase

	| testCasePassed |

	testCasePassed _
		[
			[
				self reportAboutToRun: aTestCase.
				aTestCase runCase.
				self reportPassed: aTestCase.
				true]
					on: self class failure
					do: [ :signal |
						self reportFailed: aTestCase because: signal.
						(self failures isEmpty or: [ failures last ~~ aTestCase ])
							ifTrue: [ failures add: aTestCase ].
						signal sunitExitWith: false ]]
							on: self class error
							do: [ :signal |
								self reportError: aTestCase because: signal.
								aTestCase errored: signal.
								self errors add: aTestCase.
								signal sunitExitWith: false ].

	testCasePassed
		ifTrue: [ self passed add: aTestCase ]
</details>

#### TestResult>>#passed

<details>
	<summary>See more</summary>
	
	passed

	passed
		ifNil: [passed := OrderedCollection new].

	^passed
			
</details>

#### TestResult>>#reportPassed: aTestCase

<details>
	<summary>See more</summary>
	
	reportPassed: aTestCase
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: 'finished.'; newLine]
</details>

#### TestResult>>#passedCount

<details>
	<summary>See more</summary>
	
	passedCount

	^self passed size
			
</details>

#### TestResult>>#failureCount

<details>
	<summary>See more</summary>
	
	failureCount

	^self failures size
			
</details>

#### TestResult>>#tests

<details>
	<summary>See more</summary>
	
	tests

	^(OrderedCollection new: self runCount)
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself
			
</details>

#### TestResult>>#reportError: aTestCase because: anException

<details>
	<summary>See more</summary>
	
	reportError: aTestCase because: anException

	Transcript print: anException; newLine.
</details>

#### TestResult>>#errors

<details>
	<summary>See more</summary>
	
	errors

	errors
		ifNil: [errors := OrderedCollection new].
	^errors
			
</details>

#### TestResult>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self correctCount printString;
		nextPutAll: ' passed, ';
		nextPutAll: self failureCount printString;
		nextPutAll: ' failed, ';
		nextPutAll: self errorCount printString;
		nextPutAll: ' error'.

	self errorCount ~= 1
		ifTrue: [aStream nextPut: $s]
			
</details>

#### TestResult>>#failures

<details>
	<summary>See more</summary>
	
	failures
	failures
		ifNil: [failures := OrderedCollection new].
	^failures
			
</details>

#### TestResult>>#hasPassed

<details>
	<summary>See more</summary>
	
	hasPassed

	^self hasErrors not and: [self hasFailures not]
			
</details>

#### TestResult>>#errorCount

<details>
	<summary>See more</summary>
	
	errorCount

	^self errors size
			
</details>

#### TestResult>>#runCount

<details>
	<summary>See more</summary>
	
	runCount

	^self passedCount + self failureCount + self errorCount
			
</details>

#### TestResult>>#reportFailed: aTestCase because: anException

<details>
	<summary>See more</summary>
	
	reportFailed: aTestCase because: anException
	Preferences transcriptLogVerbose ifTrue: [
		Transcript print: anException; newLine].
</details>

#### TestResult>>#reportAboutToRun: aTestCase

<details>
	<summary>See more</summary>
	
	reportAboutToRun: aTestCase
	Preferences transcriptLogVerbose ifTrue: [
		Transcript show: 'Will run: '; print: aTestCase; newLine]
</details>

#### TestResult>>#hasErrors

<details>
	<summary>See more</summary>
	
	hasErrors

	^self errors size > 0
			
</details>

#### TestResult>>#hasFailures

<details>
	<summary>See more</summary>
	
	hasFailures

	^self failures size > 0
			
</details>

#### TestResult>>#defects

<details>
	<summary>See more</summary>
	
	defects
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself
			
</details>

## TestResultWindow

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TestResultWindow>>#addButtonsTo: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButtonsTo: row color: buttonColor

	self addButton: self createDebugButton to: row color: buttonColor.
	self addButton: self createReRunButton to: row color: buttonColor.
	
</details>

#### TestResultWindow>>#debug

<details>
	<summary>See more</summary>
	
	debug

	model selection ifNotNil: [ :selection | | test |
		test := selection actualClass selector: selection selector.
		test debug.
		testResult removeFromDefectsAndAddToPassed: test.
		model removeMessageFromBrowserKeepingLabel. 
		self setLabel: testResult printString ]
</details>

#### TestResultWindow>>#isMessageSelected

<details>
	<summary>See more</summary>
	
	isMessageSelected

	^model selection notNil 
</details>

#### TestResultWindow>>#initializeFor: aTestResult

<details>
	<summary>See more</summary>
	
	initializeFor: aTestResult 

	testResult := aTestResult 
</details>

#### TestResultWindow>>#buildLowerPanes

<details>
	<summary>See more</summary>
	
	buildLowerPanes

	| codeAndButtons  |

	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: self buttonsRow fixedHeight: self defaultButtonPaneHeight;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	
	^codeAndButtons 
</details>

#### TestResultWindow>>#runSuite

<details>
	<summary>See more</summary>
	
	runSuite

	| suite |
	 
	suite := TestSuite new.
	suite addTests: testResult tests.
	self delete.
	(ProgressiveTestRunner for: suite) value.
	
</details>

#### TestResultWindow>>#addButton: button to: row color: buttonColor

<details>
	<summary>See more</summary>
	
	addButton: button to: row color: buttonColor

	button color: buttonColor.
	row addMorph: button proportionalWidth: 10
</details>

#### TestResultWindow>>#buttonsRow

<details>
	<summary>See more</summary>
	
	buttonsRow

	| buttonColor row |

	buttonColor := self buttonColor.
	row := LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: buttonColor.

	self addButtonsTo: row color: buttonColor.
	
	^row
	
	
</details>

#### TestResultWindow>>#createReRunButton

<details>
	<summary>See more</summary>
	
	createReRunButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #runSuite
		label: 'Run Suite'.

</details>

#### TestResultWindow>>#createDebugButton

<details>
	<summary>See more</summary>
	
	createDebugButton

	^PluggableButtonMorph 
		model: self
		stateGetter: #isMessageSelected
		action: #debug
		label: 'Debug'.

</details>

## TestRunner

This is a user interface for the SUnit TestCase and TestSuite classes. It lets you run tests in the background, and you can select subsets to run.

### Methods
#### TestRunner>>#failuresList

<details>
	<summary>See more</summary>
	
	failuresList

        ^self failures collect: [:failure | failure printString]
</details>

#### TestRunner>>#updatePassFail: aTestResult

<details>
	<summary>See more</summary>
	
	updatePassFail: aTestResult
        | message |
        message _ aTestResult hasPassed
                                ifTrue: ['Pass']
                                ifFalse: ['Fail'].
        self displayPassFail: message
</details>

#### TestRunner>>#refreshTR

<details>
	<summary>See more</summary>
	
	refreshTR
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: ''.
	self displayDetails: ''
</details>

#### TestRunner>>#listSelectionAt: index put: aBoolean

<details>
	<summary>See more</summary>
	
	listSelectionAt: index put: aBoolean
	^selectedSuites at: index put: aBoolean

</details>

#### TestRunner>>#selectedSuite: anInteger

<details>
	<summary>See more</summary>
	
	selectedSuite: anInteger
	anInteger > 0 ifTrue: [ | selected |
		selected _ selectedSuite ~= anInteger.
		selectedSuites at: anInteger put: selected.
	] ifFalse: [
		"selectedSuite > 0 ifTrue: [ selectedSuites at: selectedSuite put: false ]."
	].
	selectedSuite _ anInteger.
	selectedFailureTest _ 0.
	selectedErrorTest _ 0.
	self changed: #selectedFailureTest.             "added rew"
	self changed: #selectedErrorTest.               "added rew" 
	self changed: #selectedSuite.
	self changed: #allSelections.

</details>

#### TestRunner>>#testCases

<details>
	<summary>See more</summary>
	
	testCases
	^ TestCase allSubclasses reject: [:cls | cls isAbstract]
</details>

#### TestRunner>>#selectedSuite

<details>
	<summary>See more</summary>
	
	selectedSuite

        ^selectedSuite
</details>

#### TestRunner>>#setFilter

<details>
	<summary>See more</summary>
	
	setFilter
	filter _ FillInTheBlankMorph request: 'Pattern for added test cases (#* OK)' initialAnswer: '*'.
	(filter endsWith: '*') ifFalse: [ filter _ filter, '*' ].
	selectedSuites _ (tests asOrderedCollection with: selectedSuites collect: [ :ea :sel |
		sel or: [ filter match: ea asString ]
	]).
	selectedSuite _ selectedSuites indexOf: true ifAbsent: [0].
	self changed: #allSelections.

</details>

#### TestRunner>>#displayDetails: aString

<details>
	<summary>See more</summary>
	
	displayDetails: aString
        details := aString.
        self changed: #details
</details>

#### TestRunner>>#tests

<details>
	<summary>See more</summary>
	
	tests
        ^ tests
</details>

#### TestRunner>>#showResult

<details>
	<summary>See more</summary>
	
	showResult

	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.
	self
		showResultSummary;
		showResultDefects.
	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine.
</details>

#### TestRunner>>#result

<details>
	<summary>See more</summary>
	
	result
	^result

</details>

#### TestRunner>>#completedFraction

<details>
	<summary>See more</summary>
	
	completedFraction
	^completedTests / totalTests
</details>

#### TestRunner>>#refreshTests

<details>
	<summary>See more</summary>
	
	refreshTests
	| preselected |
	selectedSuite _ 0.
	selectedFailureTest _ 0.
	selectedErrorTest _ 0.
	preselected _ Set new.
	tests
		with: selectedSuites
		do: [ :t :f |
			f ifTrue: [ preselected add: t ]].
	tests _ self gatherTestNames.
	selectedSuites _ tests collect: [ :ea |
		preselected includes: ea ].
	self changed: #tests.
	self changed: #selectedFailureTest.		"added rew"
	self changed: #selectedErrorTest.			"added rew"
	self changed: #selectedSuite.
</details>

#### TestRunner>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	result := TestResult new.
	passFail := ''.
	details := ''.
	failures := OrderedCollection new.
	errors := OrderedCollection new.
	tests := self gatherTestNames.
	selectedSuite := 0.
	selectedFailureTest := 0.
	selectedErrorTest := 0.
	selectedSuites := tests collect: [:ea | true].
	running := nil.
	runSemaphore := Semaphore new
</details>

#### TestRunner>>#selectedErrorTest

<details>
	<summary>See more</summary>
	
	selectedErrorTest
        ^selectedErrorTest
</details>

#### TestRunner>>#runWindow

<details>
	<summary>See more</summary>
	
	runWindow
	
	self updateErrors: TestResult new.
	self updateFailures: TestResult new.
	self displayPassFail: 'Running...'.
	self displayDetails: '...'
</details>

#### TestRunner>>#selectedTests

<details>
	<summary>See more</summary>
	
	selectedTests
	| retval |
	retval _ OrderedCollection new.
	tests with: selectedSuites do: [ :str :sel | sel ifTrue: [ retval add: str ]].
	^retval

</details>

#### TestRunner>>#runSuiteProfiled: suite

<details>
	<summary>See more</summary>
	
	runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ MessageTally spyOn: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				UISupervisor whenUIinSafeState: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
	      resume.

</details>

#### TestRunner>>#invertSelections

<details>
	<summary>See more</summary>
	
	invertSelections
	selectedSuites _ selectedSuites collect: [ :ea | ea not ].
	selectedSuite _ selectedSuites indexOf: true ifAbsent: [0].
	self changed: #allSelections .

</details>

#### TestRunner>>#runOneTest

<details>
	<summary>See more</summary>
	
	runOneTest
	| testSuite |
	self runWindow.
	selectedSuite isZero ifTrue: [ ^ self displayPassFail: 'No Test Suite Selected' ].
	testSuite _  TestSuite new name: 'TestRunner Suite'.
	self addTestsFor: (tests at: selectedSuite) toSuite: testSuite.
	runSemaphore initSignals.
	self runSuite: testSuite
</details>

#### TestRunner>>#errors

<details>
	<summary>See more</summary>
	
	errors

        ^errors
</details>

#### TestRunner>>#failures

<details>
	<summary>See more</summary>
	
	failures

        ^failures
</details>

#### TestRunner>>#runTests

<details>
	<summary>See more</summary>
	
	runTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuite: suite
</details>

#### TestRunner>>#errorLog

<details>
	<summary>See more</summary>
	
	errorLog
	^Transcript
</details>

#### TestRunner>>#passFail

<details>
	<summary>See more</summary>
	
	passFail

        ^passFail
</details>

#### TestRunner>>#displayPassFail: aString

<details>
	<summary>See more</summary>
	
	displayPassFail: aString
        passFail := aString.
        self changed: #passFail
</details>

#### TestRunner>>#selectedFailureTest

<details>
	<summary>See more</summary>
	
	selectedFailureTest

        ^selectedFailureTest
</details>

#### TestRunner>>#showResultDefects

<details>
	<summary>See more</summary>
	
	showResultDefects

	(self result failureCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ----- Failures ----'.
			self result failures do: [:failure |
				self errorLog newLine; tab; show: failure printString]].
	(self result errorCount > 0)
		ifTrue: [
			self errorLog newLine; show: '---- SUnit ------- Errors ----'.
			self result errors do: [:error |
				self errorLog newLine; tab; show: error printString]].
</details>

#### TestRunner>>#deselectAll

<details>
	<summary>See more</summary>
	
	deselectAll
	selectedSuites _ tests collect: [ :ea | false ].
	selectedSuite _ 0.
      self changed: #allSelections.
 
</details>

#### TestRunner>>#updateErrors: aTestResult

<details>
	<summary>See more</summary>
	
	updateErrors: aTestResult

        self displayErrors: aTestResult errors
</details>

#### TestRunner>>#debugFailureTest: anInteger

<details>
	<summary>See more</summary>
	
	debugFailureTest: anInteger

        (anInteger ~= 0)
                ifTrue: [(self failures at: anInteger) debugAsFailure].

        selectedFailureTest _ anInteger.
        selectedErrorTest _ 0.
        self changed: #selectedErrorTest.
        self changed: #selectedFailureTest.

</details>

#### TestRunner>>#displayFailures: anOrderedCollection

<details>
	<summary>See more</summary>
	
	displayFailures: anOrderedCollection

        failures := anOrderedCollection.
        self changed: #failuresList
</details>

#### TestRunner>>#updateWindow: aTestResult

<details>
	<summary>See more</summary>
	
	updateWindow: aTestResult
	self updatePassFail: aTestResult.
	self updateDetails: aTestResult.
	self updateFailures: aTestResult.
	self updateErrors: aTestResult
</details>

#### TestRunner>>#details

<details>
	<summary>See more</summary>
	
	details

        ^details
</details>

#### TestRunner>>#displayErrors: anOrderedCollection

<details>
	<summary>See more</summary>
	
	displayErrors: anOrderedCollection

        errors := anOrderedCollection.
        self changed: #errorsList
</details>

#### TestRunner>>#selectAll

<details>
	<summary>See more</summary>
	
	selectAll
	| sel |
	sel _ self selectedSuite.
	selectedSuites _ selectedSuites collect: [ :ea | true ].
	selectedSuites size isZero ifFalse: [
		sel isZero ifTrue: [ self selectedSuite: 1 ]
			ifFalse: [ self changed: #allSelections ]].

</details>

#### TestRunner>>#update: aParameter

updates come in from another thread


<details>
	<summary>See more</summary>
	
	update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestCase)
		ifTrue: [
			UISupervisor whenUIinSafeState: [
				completedTests _ completedTests + 1.
				progressLabel _ aParameter printString.
				self changed: #progress ]]
		ifFalse: [ super update: aParameter ]
</details>

#### TestRunner>>#listSelectionAt: index

<details>
	<summary>See more</summary>
	
	listSelectionAt: index
	^selectedSuites at: index
</details>

#### TestRunner>>#updateDetails: aTestResult

<details>
	<summary>See more</summary>
	
	updateDetails: aTestResult 
	self displayDetails: aTestResult printString
			, (self timeSinceLastPassAsString: aTestResult).
	aTestResult hasPassed
		ifTrue: [lastPass _ DateAndTime now]
</details>

#### TestRunner>>#runButtonState

Appear 'presed' while running.


<details>
	<summary>See more</summary>
	
	runButtonState
	"Appear 'presed' while running."
	^ running notNil
</details>

#### TestRunner>>#gatherTestNames

<details>
	<summary>See more</summary>
	
	gatherTestNames
	^ (self testCases collect: [ :each |
		each name ]) asArray sort.
</details>

#### TestRunner>>#debugErrorTest: anInteger

<details>
	<summary>See more</summary>
	
	debugErrorTest: anInteger
        selectedErrorTest _ anInteger.  "added rew"
        selectedFailureTest _ 0.                        "added rew"
        self changed: #selectedFailureTest.             "added rew"
        self changed: #selectedErrorTest.               "added rew"
        (anInteger ~= 0)
                ifTrue: [(result errors at: anInteger) debug]
</details>

#### TestRunner>>#showResultSummary

<details>
	<summary>See more</summary>
	
	showResultSummary

	| message summary |
	message := (self result runCount = self result correctCount)
		ifTrue: ['succeeded']
		ifFalse: ['failed'].
	Transcript newLine; tab; show: message.
	summary :=
		self result runCount printString, ' run, ',
		self result failureCount printString, ' failed, ',
		self result errorCount printString, ' errors'.
	Transcript newLine; tab; show: summary.
</details>

#### TestRunner>>#stopButtonState

<details>
	<summary>See more</summary>
	
	stopButtonState
	^self runButtonState not

</details>

#### TestRunner>>#addTestsFor: testName toSuite: suite

<details>
	<summary>See more</summary>
	
	addTestsFor: testName toSuite: suite 
	| cls |
	(testName indexOf: $() > 0
		ifFalse: [
			cls _ SUnitNameResolver classNamed: testName asSymbol.
			cls isAbstract
				ifTrue: [cls allSubclasses
						do: [:each | each isAbstract
								ifFalse: [each addToSuiteFromSelectors: suite]]]
				ifFalse: [cls addToSuiteFromSelectors: suite]].
	^ suite
</details>

#### TestRunner>>#runProfiledTests

<details>
	<summary>See more</summary>
	
	runProfiledTests
	| suite |
	suite _ TestSuite new name: 'TestRunner Suite'.
	self selectedTests do: [ :ea | self addTestsFor: ea toSuite: suite ].
	runSemaphore initSignals.
	self runSuiteProfiled: suite
</details>

#### TestRunner>>#timeSinceLastPassAsString: aResult

<details>
	<summary>See more</summary>
	
	timeSinceLastPassAsString: aResult
        (lastPass isNil or: [aResult hasPassed not]) ifTrue: [^ ''].
        ^ ', ' , (DateAndTime now - lastPass) printString , '
since last Pass'
</details>

#### TestRunner>>#terminateRun

<details>
	<summary>See more</summary>
	
	terminateRun
	running ifNotNil: [ running terminate. running _ nil ].

</details>

#### TestRunner>>#runSuite: suite

<details>
	<summary>See more</summary>
	
	runSuite: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	running _ [
            [ result _ suite run ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				UISupervisor whenUIinSafeState: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
"	      priority: Processor userBackgroundPriority;"
	      resume.

</details>

#### TestRunner>>#errorsList

<details>
	<summary>See more</summary>
	
	errorsList

        ^self errors collect: [:error | error printString]
</details>

#### TestRunner>>#progressLabel

<details>
	<summary>See more</summary>
	
	progressLabel
	^progressLabel
</details>

#### TestRunner>>#updateFailures: aTestResult

<details>
	<summary>See more</summary>
	
	updateFailures: aTestResult

        self displayFailures: aTestResult failures
</details>

## TestSuite

This is a Composite of Tests, either TestCases or other TestSuites. The common protocol is #run: aTestResult and the dependencies protocol

### Methods
#### TestSuite>>#setUpResources

<details>
	<summary>See more</summary>
	
	setUpResources
			
	self resources do: [ :res | res isAvailable ifFalse: [^res signalInitializationError]].
	
</details>

#### TestSuite>>#defaultResources

<details>
	<summary>See more</summary>
	
	defaultResources
	^self tests 
		inject: Set new
		into: [:coll :testCase | 
			coll
				addAll: testCase resources;
				yourself]
			
</details>

#### TestSuite>>#resources: anObject

<details>
	<summary>See more</summary>
	
	resources: anObject
	resources := anObject
			
</details>

#### TestSuite>>#addTests: aCollection

<details>
	<summary>See more</summary>
	
	addTests: aCollection 
	aCollection do: [:eachTest | self addTest: eachTest]
			
</details>

#### TestSuite>>#addDependentToHierachy: anObject

<details>
	<summary>See more</summary>
	
	addDependentToHierachy: anObject
	self addDependent: anObject.
	self tests do: [ :each | each addDependentToHierachy: anObject]
</details>

#### TestSuite>>#name: aString

<details>
	<summary>See more</summary>
	
	name: aString

	name := aString
			
</details>

#### TestSuite>>#resources

<details>
	<summary>See more</summary>
	
	resources
	resources ifNil: [resources := self defaultResources].
	^resources
			
</details>

#### TestSuite>>#tests

<details>
	<summary>See more</summary>
	
	tests
	tests ifNil: [tests := OrderedCollection new].
	^tests
			
</details>

#### TestSuite>>#tearDownResources

<details>
	<summary>See more</summary>
	
	tearDownResources
			
	self resources do: [:each | each reset]
</details>

#### TestSuite>>#removeDependentFromHierachy: anObject

<details>
	<summary>See more</summary>
	
	removeDependentFromHierachy: anObject
	self removeDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]
			
</details>

#### TestSuite>>#run: aResult

<details>
	<summary>See more</summary>
	
	run: aResult 
	self tests do: [:each | 
		self changed: each.
		each run: aResult]
			
</details>

#### TestSuite>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^name
			
</details>

#### TestSuite>>#addTest: aTest

<details>
	<summary>See more</summary>
	
	addTest: aTest
	self tests add: aTest
			
</details>

#### TestSuite>>#run

<details>
	<summary>See more</summary>
	
	run

	| result |
 
	result := TestResult new.
	self setUpResources.	
	[self run: result] ensure: [self tearDownResources].
	
	^result
			
</details>

