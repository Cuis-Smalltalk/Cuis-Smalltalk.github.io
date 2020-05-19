## ArithmeticError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## ArithmeticMessageError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ArithmeticMessageError>>#receiver: aNumber selector: aSymbol argument: otherNumber

<details>
	<summary>See more</summary>
	
	receiver: aNumber selector: aSymbol argument: otherNumber
	self receiver: aNumber selector: aSymbol arguments: {otherNumber}
</details>

#### ArithmeticMessageError>>#signalReceiver: aNumber selector: aSymbol arguments: aCollection

<details>
	<summary>See more</summary>
	
	signalReceiver: aNumber selector: aSymbol arguments: aCollection

	^self
		receiver: aNumber selector: aSymbol arguments: aCollection;
		signal
</details>

#### ArithmeticMessageError>>#receiver: aNumber selector: aSymbol arguments: aCollection

<details>
	<summary>See more</summary>
	
	receiver: aNumber selector: aSymbol arguments: aCollection
	receiver _ aNumber.
	selector _ aSymbol.
	arguments _ aCollection
</details>

#### ArithmeticMessageError>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	(receiver isFloatOrFloatComplex or: [ arguments notNil and: [arguments anySatisfy: [ :a | a isFloatOrFloatComplex ]]]) ifTrue: [
		^self floatErrorValue ].
	^ super defaultAction
</details>

#### ArithmeticMessageError>>#signalReceiver: aNumber selector: aSymbol argument: otherNumber

<details>
	<summary>See more</summary>
	
	signalReceiver: aNumber selector: aSymbol argument: otherNumber

	^self
		receiver: aNumber selector: aSymbol argument: otherNumber;
		signal
</details>

#### ArithmeticMessageError>>#floatErrorValue

<details>
	<summary>See more</summary>
	
	floatErrorValue
	^ self subclassResponsibility
</details>

## AttemptToWriteReadOnlyGlobal

This is a resumable error you get if you try to assign a readonly variable a value. Name definitions in the module system can be read only and are then created using instances of ReadOnlyVariableBinding instead of Association. See also LookupKey>>beReadWriteBinding and LookupKey>>beReadOnlyBinding.

### Methods
#### AttemptToWriteReadOnlyGlobal>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	^true
</details>

#### AttemptToWriteReadOnlyGlobal>>#description

Return a textual description of the exception.


<details>
	<summary>See more</summary>
	
	description
	"Return a textual description of the exception."

	| desc |
	desc := 'Error'.
	^self messageText
		ifNil: [ desc]
		ifNotNil: [ :mt | desc, ': ', mt ]
</details>

## BlockCannotReturn

This class is private to the EHS implementation. Its use allows for ensured execution to survive code such as: [self doThis. ^nil] ensure: [self doThat] Signaling or handling this exception is not recommended.

### Methods
#### BlockCannotReturn>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable

	^true
</details>

#### BlockCannotReturn>>#result: r

<details>
	<summary>See more</summary>
	
	result: r

	result := r
</details>

#### BlockCannotReturn>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self messageText: 'Block cannot return'.
	^super defaultAction
</details>

#### BlockCannotReturn>>#deadHome

<details>
	<summary>See more</summary>
	
	deadHome

	^ deadHome
</details>

#### BlockCannotReturn>>#result

<details>
	<summary>See more</summary>
	
	result

	^result
</details>

#### BlockCannotReturn>>#deadHome: context

<details>
	<summary>See more</summary>
	
	deadHome: context

	deadHome _ context
</details>

## CannotDeleteFileException

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## DomainError

A DomainError is an error occuring when a mathematical function is used outside its domain of validity.

### Methods
## Error

>From the ANSI standard: This protocol describes the behavior of instances of class Error. These are used to represent error conditions that prevent the normal continuation of processing. Actual error exceptions used by an application may be subclasses of this class. As Error is explicitly specified to be subclassable, conforming implementations must implement its behavior in a non-fragile manner. Additional notes: Error>defaultAction uses an explicit test for the presence of the Debugger class to decide whether or not it is in development mode. In the future, TFEI hopes to enhance the semantics of #defaultAction to improve support for pluggable default handlers.

### Methods
#### Error>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	"Determine whether an exception is resumable."

	^ false
</details>

#### Error>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self noHandler
</details>

## Exception

This is the main class used to implement the exception handling system (EHS). It plays two distinct roles: that of the exception, and that of the exception handler. More specifically, it implements the bulk of the protocols laid out in the ANSI specification - those protocol names are reflected in the message categories. Exception is an abstract class. Instances should neither be created nor trapped. In most cases, subclasses should inherit from Error or Notification rather than directly from Exception. In implementing this EHS, The Fourth Estate Inc. incorporated some ideas and code from Craig Latta's EHS. His insights were crucial in allowing us to implement BlockClosure>>valueUninterruptably (and by extension, #ensure: and #ifCurtailed:), and we imported the following methods with little or no modification: ContextPart>>terminateTo: ContextPart>>terminate MethodContext>>receiver: MethodContext>>answer: Thanks, Craig!

### Methods
#### Exception>>#pushHandlerContext: aContextTag

<details>
	<summary>See more</summary>
	
	pushHandlerContext: aContextTag

	self handlerContexts add: aContextTag
</details>

#### Exception>>#sunitExitWith: aValue

<details>
	<summary>See more</summary>
	
	sunitExitWith: aValue

        self return: aValue
</details>

#### Exception>>#messageText

Return an exception's message text.


<details>
	<summary>See more</summary>
	
	messageText
	
"Return an exception's message text."

	^messageText
</details>

#### Exception>>#resume

Return from the message that signaled the receiver.


<details>
	<summary>See more</summary>
	
	resume
	"Return from the message that signaled the receiver."

	self resume: self defaultResumeValue
</details>

#### Exception>>#receiver

<details>
	<summary>See more</summary>
	
	receiver

	^ self signalerContext receiver
</details>

#### Exception>>#tag

Return an exception's tag value.


<details>
	<summary>See more</summary>
	
	tag
       "Return an exception's tag value."

	^tag ifNil: [ self messageText ]
</details>

#### Exception>>#retryUsing: alternativeBlock

Abort an exception handler and evaluate a new block in place of the handler's protected block.


<details>
	<summary>See more</summary>
	
	retryUsing: alternativeBlock
	"Abort an exception handler and evaluate a new block in place of the handler's protected block."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext restartWithNewReceiver: alternativeBlock

</details>

#### Exception>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The default action taken if the exception is signaled."

	self subclassResponsibility
</details>

#### Exception>>#printOn: stream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: stream

	stream nextPutAll: self description
</details>

#### Exception>>#noHandler

No one has handled this error, but now give them a chance to decide how to debug it. If none handle this either then open debugger (see UnhandedError-defaultAction)


<details>
	<summary>See more</summary>
	
	noHandler
	"No one has handled this error, but now give them a chance to decide how to debug it.  If none handle this either then open debugger (see UnhandedError-defaultAction)"

	UnhandledError signalForException: self
</details>

#### Exception>>#resignalAs: replacementException

Signal an alternative exception in place of the receiver.


<details>
	<summary>See more</summary>
	
	resignalAs: replacementException
	"Signal an alternative exception in place of the receiver."

	handlerBlockNotCurtailed _ true.
	signalContext resumeEvaluating: [replacementException signal]
</details>

#### Exception>>#signalIn: aContext

Ask ContextHandlers in the sender chain to handle this signal. The default is to execute and return my defaultAction.


<details>
	<summary>See more</summary>
	
	signalIn: aContext
	"Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	signalContext ifNotNil: [self error: 'This exception has already been signaled and its handler block is being executed.'].
	signalContext _ aContext contextTag.
	^ aContext nextHandlerContext handleSignal: self
</details>

#### Exception>>#topHandlerContext

<details>
	<summary>See more</summary>
	
	topHandlerContext

	^self handlerContexts isEmpty ifTrue: [nil] ifFalse: [self handlerContexts last]
</details>

#### Exception>>#outer

Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:).


<details>
	<summary>See more</summary>
	
	outer
	"Evaluate the enclosing exception action and return to here instead of signal if it resumes (see #resumeUnchecked:)."

	| prevOuterContext |
	handlerBlockNotCurtailed _ true.
	self isResumable ifTrue: [
		prevOuterContext _ outerContext.
		outerContext _ thisContext contextTag.
	].
	self topHandlerContext nextHandlerContext handleSignal: self
</details>

#### Exception>>#signal

<details>
	<summary>See more</summary>
	
	signal

	^self signalIn: thisContext
</details>

#### Exception>>#popHandlerContext

<details>
	<summary>See more</summary>
	
	popHandlerContext

	self handlerContexts removeLast
</details>

#### Exception>>#handlerContexts

<details>
	<summary>See more</summary>
	
	handlerContexts

	^handlerContexts
</details>

#### Exception>>#description

Return a textual description of the exception.


<details>
	<summary>See more</summary>
	
	description
	
"Return a textual description of the exception."

	| desc |
	desc := self class name asString.
	^ self messageText
		ifNil: [ desc]
		ifNotNil: [ :mt | desc , ': ' , mt ]
</details>

#### Exception>>#tag: t

This message is not specified in the ANSI protocol, but that looks like an oversight because #tag is specified, and the spec states that the signaler may store the tag value.


<details>
	<summary>See more</summary>
	
	tag: t
	"This message is not specified in the ANSI protocol, but that looks like an oversight because #tag is specified, and the spec states that the signaler may store the tag value."

	tag := t
</details>

#### Exception>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	self handlerContexts: OrderedCollection new
</details>

#### Exception>>#signal: signalerText

Signal the occurrence of an exceptional condition with a specified textual description.


<details>
	<summary>See more</summary>
	
	signal: signalerText
	"Signal the occurrence of an exceptional condition with a specified textual description."

	self messageText: signalerText.
	^ self signal
</details>

#### Exception>>#handlerContexts: aCollection

<details>
	<summary>See more</summary>
	
	handlerContexts: aCollection

	handlerContexts _ aCollection
</details>

#### Exception>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	"Determine whether an exception is resumable."

	^ true
</details>

#### Exception>>#isNested

Determine whether the current exception handler is within the scope of another handler for the same exception.


<details>
	<summary>See more</summary>
	
	isNested
	"Determine whether the current exception handler is within the scope of another handler for the same exception."

	^self topHandlerContext nextHandlerContext canHandleSignal: self
</details>

#### Exception>>#retry

Abort an exception handler and re-evaluate its protected block.


<details>
	<summary>See more</summary>
	
	retry
	"Abort an exception handler and re-evaluate its protected block."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext restart
</details>

#### Exception>>#signalerContext

Find the first sender of signal(:)


<details>
	<summary>See more</summary>
	
	signalerContext
	"Find the first sender of signal(:)"

	^ signalContext findContextSuchThat: [:ctxt |
		(ctxt receiver == self or: [ctxt receiver == self class]) not]
</details>

#### Exception>>#canSearchForSignalerContext

This method is /only/ to support the debugger's catching of exceptions in stepIntoBlock.


<details>
	<summary>See more</summary>
	
	canSearchForSignalerContext
	"This method is /only/ to support the debugger's catching of exceptions in stepIntoBlock."
	^signalContext isContext
</details>

#### Exception>>#defaultResumeValue

Answer the value that by default should be returned if the exception is resumed


<details>
	<summary>See more</summary>
	
	defaultResumeValue
	"Answer the value that by default should be returned if the exception is resumed"
	^nil
</details>

#### Exception>>#return: returnValue

Return the argument as the value of the block protected by the active exception handler.


<details>
	<summary>See more</summary>
	
	return: returnValue
	"Return the argument as the value of the block protected by the active exception handler."

	handlerBlockNotCurtailed _ true.
	self topHandlerContext return: returnValue
</details>

#### Exception>>#evaluateHandlerBlock: aBlock

<details>
	<summary>See more</summary>
	
	evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"Please see
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000800.html
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000809.html
					Also see the rest of the tread in detail.
					This is work in progress."
					Preferences allowNonLocalReturnsInExceptionHandlers
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							Preferences warnAboutNonLocalReturnsInExceptionHandlers
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed _ true ].
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]
</details>

#### Exception>>#resume: resumptionValue

Return resumptionValue as the value of the signal message.


<details>
	<summary>See more</summary>
	
	resume: resumptionValue
	"Return resumptionValue as the value of the signal message."

	handlerBlockNotCurtailed _ true.
	self isResumable ifFalse: [IllegalResumeAttempt signal].
	self resumeUnchecked: resumptionValue
</details>

#### Exception>>#return

Return nil as the value of the block protected by the active exception handler.


<details>
	<summary>See more</summary>
	
	return
	"Return nil as the value of the block protected by the active exception handler."

	self return: nil
</details>

#### Exception>>#resumeUnchecked: resumptionValue

Return resumptionValue as the value of #signal, unless this was called after an #outer message, then return resumptionValue as the value of #outer.


<details>
	<summary>See more</summary>
	
	resumeUnchecked: resumptionValue
	"Return resumptionValue as the value of #signal, unless this was called after an #outer message, then return resumptionValue as the value of #outer."

	| ctxt |
	handlerBlockNotCurtailed _ true.
	outerContext ifNil: [
		signalContext return: resumptionValue
	] ifNotNil: [
		ctxt _ outerContext.
		outerContext _ ctxt tempAt: 1. "prevOuterContext in #outer"
		ctxt return: resumptionValue
	].

</details>

#### Exception>>#handlerBlockNotCurtailed

<details>
	<summary>See more</summary>
	
	handlerBlockNotCurtailed

	^handlerBlockNotCurtailed
</details>

#### Exception>>#pass

Yield control to the enclosing exception action for the receiver.


<details>
	<summary>See more</summary>
	
	pass
	"Yield control to the enclosing exception action for the receiver."

	| nextHandler |
	handlerBlockNotCurtailed _ true.
	nextHandler := self topHandlerContext nextHandlerContext.
	self popHandlerContext.
	nextHandler handleSignal: self
</details>

#### Exception>>#messageText: signalerText

Set an exception's message text.


<details>
	<summary>See more</summary>
	
	messageText: signalerText
	
"Set an exception's message text."

	messageText := signalerText
</details>

## ExceptionHandlingCondition

I represent the protocol expected to be as condition on the exception handling message on:do: I also define the protocol to create and combine exceptions handling conditions. See methods #, and #- for a complemented documentation

### Methods
#### ExceptionHandlingCondition>>#handles: anException

Must return true if anException must be handle See also Exception class>>handles: anException


<details>
	<summary>See more</summary>
	
	handles: anException 
	
	"Must return true if anException must be handle
	See also Exception class>>handles: anException"
	
	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#createFilterConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#createOrConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithOrCondition: anOrExceptionHandlingCondition 

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#createOrConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createOrConditionWithExceptionType: anExceptionType

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#- anExceptionHandlingCondition

Creates a handling condition that will not handle exceptions that meet the right side of the condition The following example will not handle the exception [ 1/0 ] on: Error - ZeroDivide do: [ :anError | ... ] The following example will handle the exception: [ Error signal ] on: Error - ZeroDivide do: [ :anError | ... ] Due to inconsisties that can arrise with combining #, with #- the implementation orders the in such a way that 'or conditions' go first and 'filter conditions' go last. Doing so (Error - Notification) , (UnhandledError - ZeroDivide) is converted to Error, UnhandledError - Notification - ZeroDivide Inconsisties can arrise because ZeroDivide is a subclass of Error and therefore if the condition is not ordered correctly a ZeroDivide could be handled. This inconsisty can be found in Pharo where the condition (Error - Notification) , (UnhandledError - ZeroDivide) does not filter ZeroDivide but the condition Error, UnhandledError - Notification - ZeroDivide does filter it. That is the reason the implementation uses double dispatch


<details>
	<summary>See more</summary>
	
	- anExceptionHandlingCondition

	"Creates a handling condition that will not handle exceptions that meet the right side of the condition
	The following example will not handle the exception
	[ 1/0 ]
	   on:  Error - ZeroDivide 
	  do: [ :anError | ... ]
	
	The following example will handle the exception:
	[ Error signal ]
	   on:  Error - ZeroDivide 
	  do: [ :anError | ... ]
	
	Due to inconsisties that can arrise with combining #, with #- the implementation orders the in such a way that 'or conditions' go first 
	and 'filter conditions' go last. Doing so (Error - Notification) , (UnhandledError - ZeroDivide)  is converted to  Error, UnhandledError - Notification - ZeroDivide 
	Inconsisties can arrise because ZeroDivide is a subclass of Error and therefore if the condition is not ordered correctly a ZeroDivide could be handled.
	This inconsisty can be found in Pharo where the condition (Error - Notification) , (UnhandledError - ZeroDivide) does not filter ZeroDivide but 
	the condition Error, UnhandledError - Notification - ZeroDivide does filter it.
	That is the reason the implementation uses double dispatch
	"
	
	self subclassResponsibility 
	
</details>

#### ExceptionHandlingCondition>>#createFilterConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createFilterConditionWithExceptionType: anExceptionType

	self subclassResponsibility 
</details>

#### ExceptionHandlingCondition>>#, anExceptionHandlingCondition

Creates a handling condition that will return true if either part of the condition handles the exception. It behaves like an or The following example will handle the exception [ Error signal ] on: Error, Halt do: [ :anError | ... ] The following example will also handle the exception: [ Halt signal ] on: Error, Halt do: [ :anError | ... ]


<details>
	<summary>See more</summary>
	
	, anExceptionHandlingCondition

	"Creates a handling condition that will return true if either part of the condition handles the exception.
	It behaves like an or
	The following example will handle the exception
	[ Error signal ]
	   on:  Error, Halt 
	  do: [ :anError | ... ]
	
	The following example will also handle the exception:
	[ Halt signal ]
	   on:  Error, Halt
	  do: [ :anError | ... ]"
	
	self subclassResponsibility 
	
</details>

## FileDoesNotExistException

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FileDoesNotExistException>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	readOnly _ false
</details>

#### FileDoesNotExistException>>#readOnly: aBoolean

<details>
	<summary>See more</summary>
	
	readOnly: aBoolean
	readOnly _ aBoolean
</details>

#### FileDoesNotExistException>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The default action taken if the exception is signaled."


	^ readOnly
		ifTrue: [ StandardFileStream readOnlyFileDoesNotExistUserHandling: fileName ]
		ifFalse: [ StandardFileStream fileDoesNotExistUserHandling: fileName ]
</details>

## FileExistsException

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FileExistsException>>#fileClass: aClass

<details>
	<summary>See more</summary>
	
	fileClass: aClass
	fileClass _ aClass
</details>

#### FileExistsException>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The default action taken if the exception is signaled."

	^ fileClass fileExistsUserHandling: fileName
</details>

## FileStreamException

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FileStreamException>>#messageText

Return an exception's message text.


<details>
	<summary>See more</summary>
	
	messageText
	
	"Return an exception's message text."

	^messageText ifNil: [fileName printString]
</details>

#### FileStreamException>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	"Determine whether an exception is resumable."

	^true
</details>

#### FileStreamException>>#fileName: aFileName

<details>
	<summary>See more</summary>
	
	fileName: aFileName
	fileName _ aFileName
</details>

## FileWriteError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## FilterExceptionHandlingCondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### FilterExceptionHandlingCondition>>#handles: anException

Must return true if anException must be handle See also Exception class>>handles: anException


<details>
	<summary>See more</summary>
	
	handles: anException 
	
	^ (filterCondition handles: anException) not and: [ handleCondition handles: anException ]
</details>

#### FilterExceptionHandlingCondition>>#createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition - handleCondition, filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#createFilterConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^anOrExceptionHandlingCondition, filterCondition - handleCondition 
</details>

#### FilterExceptionHandlingCondition>>#createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition handleCondition,handleCondition - aFilterExceptionHandlingCondition filterCondition - filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#createOrConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^anOrExceptionHandlingCondition, handleCondition - filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#createOrConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createOrConditionWithExceptionType: anExceptionType

	^anExceptionType, handleCondition - filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream
		print: handleCondition ;
		nextPutAll: ' - ';
		print: filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#- anExceptionHandlingCondition

See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation


<details>
	<summary>See more</summary>
	
	- anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithFilterCondition: self
</details>

#### FilterExceptionHandlingCondition>>#filterCondition

<details>
	<summary>See more</summary>
	
	filterCondition
	
	^filterCondition 
</details>

#### FilterExceptionHandlingCondition>>#handleCondition

<details>
	<summary>See more</summary>
	
	handleCondition
	
	^handleCondition 
</details>

#### FilterExceptionHandlingCondition>>#createFilterConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createFilterConditionWithExceptionType: anExceptionType

	^self class 
		handling: anExceptionType, filterCondition 
		filtering: handleCondition 
</details>

#### FilterExceptionHandlingCondition>>#initializeHandling: aHandleCondition filtering: aFilterCondition

<details>
	<summary>See more</summary>
	
	initializeHandling: aHandleCondition filtering: aFilterCondition 

	handleCondition _ aHandleCondition.
	filterCondition _ aFilterCondition 
</details>

#### FilterExceptionHandlingCondition>>#, anExceptionHandlingCondition

See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation


<details>
	<summary>See more</summary>
	
	, anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithFilterCondition: self
</details>

## IllegalResumeAttempt

This class is private to the EHS implementation. An instance of it is signaled whenever an attempt is made to resume from an exception which answers false to #isResumable.

### Methods
#### IllegalResumeAttempt>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	
	^ false
</details>

#### IllegalResumeAttempt>>#readMe

Never handle this exception!


<details>
	<summary>See more</summary>
	
	readMe

	"Never handle this exception!"
</details>

#### IllegalResumeAttempt>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self noHandler
</details>

## InMidstOfFileinNotification

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### InMidstOfFileinNotification>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction

	self resume: false
</details>

## MessageNotUnderstood

This exception is provided to support Object>>doesNotUnderstand:.

### Methods
#### MessageNotUnderstood>>#message: aMessage

<details>
	<summary>See more</summary>
	
	message: aMessage

	message := aMessage
</details>

#### MessageNotUnderstood>>#message

Answer the selector and arguments of the message that failed.


<details>
	<summary>See more</summary>
	
	message
	"Answer the selector and arguments of the message that failed."

	^message
</details>

#### MessageNotUnderstood>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	reachedDefaultHandler := false
</details>

#### MessageNotUnderstood>>#messageText

Return an exception's message text.


<details>
	<summary>See more</summary>
	
	messageText
	"Return an exception's message text."

	^ messageText
		ifNil: [
			message
				ifNil: [super messageText]
				ifNotNil: [message lookupClass printString , '>>' , message selector asString]]
</details>

#### MessageNotUnderstood>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	"Determine whether an exception is resumable."

	^true
</details>

#### MessageNotUnderstood>>#receiver

Answer the receiver that did not understand the message


<details>
	<summary>See more</summary>
	
	receiver
	"Answer the receiver that did not understand the message"

	^ receiver
</details>

#### MessageNotUnderstood>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	reachedDefaultHandler := true.
	super defaultAction
</details>

#### MessageNotUnderstood>>#receiver: obj

<details>
	<summary>See more</summary>
	
	receiver: obj

	receiver _ obj
</details>

#### MessageNotUnderstood>>#reachedDefaultHandler

<details>
	<summary>See more</summary>
	
	reachedDefaultHandler
	^reachedDefaultHandler
</details>

## MethodInCallStackToBecomeInvalid

A become operation tries to mutate an object that is the receiver ('self') in a method currently in execution, and part of the stack of calls of some process. This would render the method invalid and is potentially catastrophic.

### Methods
#### MethodInCallStackToBecomeInvalid>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self noHandler
</details>

## NegativePowerError

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### NegativePowerError>>#floatErrorValue

<details>
	<summary>See more</summary>
	
	floatErrorValue
	^ receiver class nan
</details>

## NonBooleanReceiver

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### NonBooleanReceiver>>#object: anObject

<details>
	<summary>See more</summary>
	
	object: anObject
	object _ anObject
</details>

#### NonBooleanReceiver>>#object

<details>
	<summary>See more</summary>
	
	object
	^object
</details>

#### NonBooleanReceiver>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable

	^true
</details>

## NotYetImplemented

Sent by #notYetImplemented. Better than the age-old behavior of opening a notifier window, because this can be caught and handled.

### Methods
#### NotYetImplemented>>#selector

<details>
	<summary>See more</summary>
	
	selector
	^selector
</details>

#### NotYetImplemented>>#receiverClass: cls selector: sel

<details>
	<summary>See more</summary>
	
	receiverClass: cls selector: sel
	receiverClass := cls.
	selector := sel.
</details>

#### NotYetImplemented>>#receiverClass

<details>
	<summary>See more</summary>
	
	receiverClass
	^receiverClass
</details>

## Notification

A Notification is an indication that something interesting has occurred. If it is not handled, it will pass by without effect.

### Methods
#### Notification>>#withNewDebuggerLabelOn: aContext do: labelBlock ifNone: noneBlok

<details>
	<summary>See more</summary>
	
	withNewDebuggerLabelOn: aContext do: labelBlock ifNone: noneBlok

	^(self isToUpdateDebuggerOn: aContext)
		ifTrue: [ labelBlock value: tag second description ]
		ifFalse: noneBlok 
</details>

#### Notification>>#isResumable

Answer true. Notification exceptions by default are specified to be resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	"Answer true. Notification exceptions by default are specified to be resumable."

	^true
</details>

#### Notification>>#isToUpdateDebuggerOn: aContext

<details>
	<summary>See more</summary>
	
	isToUpdateDebuggerOn: aContext

	^tag isArray
		and: [tag size = 2
		and: [(tag first == aContext or: [tag first hasSender: aContext])]]
</details>

#### Notification>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction
	"No action is taken. The value nil is returned as the value of the message that signaled the exception."


	^nil
</details>

## OrExceptionHandlingCondition

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### OrExceptionHandlingCondition>>#rightCondition

<details>
	<summary>See more</summary>
	
	rightCondition

	^rightCondition 
</details>

#### OrExceptionHandlingCondition>>#handles: anException

Must return true if anException must be handle See also Exception class>>handles: anException


<details>
	<summary>See more</summary>
	
	handles: anException

	^ (leftCondition handles: anException) or: [ rightCondition handles: anException ]
</details>

#### OrExceptionHandlingCondition>>#initializeHandling: aLeftCondition or: aRightCondition

<details>
	<summary>See more</summary>
	
	initializeHandling: aLeftCondition or: aRightCondition

	leftCondition _ aLeftCondition.
	rightCondition _ aRightCondition 
</details>

#### OrExceptionHandlingCondition>>#createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^aFilterExceptionHandlingCondition - leftCondition - rightCondition 
</details>

#### OrExceptionHandlingCondition>>#createFilterConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createFilterConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^ anOrExceptionHandlingCondition - leftCondition - rightCondition 
</details>

#### OrExceptionHandlingCondition>>#createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithFilterCondition: aFilterExceptionHandlingCondition 
	
	^self, aFilterExceptionHandlingCondition 
</details>

#### OrExceptionHandlingCondition>>#createOrConditionWithOrCondition: anOrExceptionHandlingCondition

<details>
	<summary>See more</summary>
	
	createOrConditionWithOrCondition: anOrExceptionHandlingCondition 
	
	^self class handling: anOrExceptionHandlingCondition or: self
</details>

#### OrExceptionHandlingCondition>>#createOrConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createOrConditionWithExceptionType: anExceptionType

	^self class handling: anExceptionType or: self
</details>

#### OrExceptionHandlingCondition>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	
	aStream
		print: leftCondition;
		nextPutAll: ', ';
		print: rightCondition 
</details>

#### OrExceptionHandlingCondition>>#- anExceptionHandlingCondition

See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation


<details>
	<summary>See more</summary>
	
	- anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createFilterConditionWithOrCondition: self
	
</details>

#### OrExceptionHandlingCondition>>#createFilterConditionWithExceptionType: anExceptionType

<details>
	<summary>See more</summary>
	
	createFilterConditionWithExceptionType: anExceptionType

	^FilterExceptionHandlingCondition 
		handling: anExceptionType - leftCondition 
		filtering: rightCondition 
</details>

#### OrExceptionHandlingCondition>>#leftCondition

<details>
	<summary>See more</summary>
	
	leftCondition

	^leftCondition 
</details>

#### OrExceptionHandlingCondition>>#, anExceptionHandlingCondition

See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation


<details>
	<summary>See more</summary>
	
	, anExceptionHandlingCondition

	"See ExceptionHandlingCondition>>#- for an explanation of why double dispatch is used as implementation"
	
	^anExceptionHandlingCondition createOrConditionWithOrCondition: self
	
</details>

## OutOfMemory

OutOfMemory is signaled when an allocation fails due to not having enough memory. Its default action signals the low-space semaphore.

### Methods
#### OutOfMemory>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	^true
</details>

#### OutOfMemory>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction
	Smalltalk signalLowSpace.
</details>

## ProgressInitiationException

I provide a way to alter the behavior of the old-style progress notifier in String. See examples in: ProgressInitiationException testWithout. ProgressInitiationException testWith.

### Methods
#### ProgressInitiationException>>#display: argString at: argPoint from: argMinVal to: argMaxVal during: argWorkBlock

<details>
	<summary>See more</summary>
	
	display: argString at: argPoint from: argMinVal to: argMaxVal during: argWorkBlock

	progressTitle _ argString.
	aPoint _ argPoint.
	minVal _ argMinVal.
	maxVal _ argMaxVal.
	currentVal _ minVal.
	workBlock _ argWorkBlock.
	^self signal
</details>

#### ProgressInitiationException>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	
	^true
</details>

#### ProgressInitiationException>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 prevContents |
	f _ AbstractFont default.
	h _ f lineSpacing * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ `0@0` extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	prevContents _ Form fromDisplay: outerFrame.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + `2@2` extent: filledWidth@(h-4)).
			filledWidth -200 > oldFilledWidth2
				ifFalse: [
					"Usually just request an update, to be done asynchronously."
					DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [
					"Once in a while, force a real screen update (warning: really slow on MacOS if done too often)"
					Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	prevContents displayAt: outerFrame topLeft.
	self resume: result
</details>

#### ProgressInitiationException>>#sendNotificationsTo: aNewBlock

<details>
	<summary>See more</summary>
	
	sendNotificationsTo: aNewBlock

	self resume: (
		workBlock value: [ :barVal |
			aNewBlock value: minVal value: maxVal value: barVal
		]
	)

</details>

## RecompilationFailure

Recompilation of some existing method failed. The usual handling of this exception should be to cancel the action that triggered recompilation. If not handled, a debugger will open.

### Methods
#### RecompilationFailure>>#class: aClass messageText: aString

<details>
	<summary>See more</summary>
	
	class: aClass messageText: aString
	class _ aClass.
	messageText _ aString
</details>

## TimedOut

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
## UnhandledError

This exception represents an error exception that has gone unhandled. Unhandled error conditions are fundamentally different from error exceptions, because error exceptions may have default handlers that address the error condition (by e.g. retrying the operation). The job of unhandled errors is to report the original problem. This reporting can be done in a variety of ways. For example, in everyday practice, unhandled errors open the debugger. Note the common practice of "catching all errors" with code such as this: [some code] on: Error do: [:ex | ex return] is doubly problematic. First, there is no specificity to the expected exceptions arising from the protected block. Second, the handler block will prevent the exception's default handler from running, which may resolve or otherwise react to the error condition. If one really wants to catch unhandled errors, the code should read like this instead: [some code] on: UnhandledError do: [:ex | ex return]

### Methods
#### UnhandledError>>#standaloneAppDefaultAction

Dump the stack trace to a log file, then exit the program (image).


<details>
	<summary>See more</summary>
	
	standaloneAppDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."
	
	Smalltalk logError: self description inContext: self signalerContext to: 'CuisDebug'.
	Smalltalk quitPrimitive: 1
</details>

#### UnhandledError>>#devDefaultAction

<details>
	<summary>See more</summary>
	
	devDefaultAction

	Processor activeProcess
		debug: exception signalerContext
		title: exception description
</details>

#### UnhandledError>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	
	^ false
</details>

#### UnhandledError>>#exception

<details>
	<summary>See more</summary>
	
	exception

	^ exception
</details>

#### UnhandledError>>#defaultAction

The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	Smalltalk isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]
</details>

#### UnhandledError>>#exception: anError

<details>
	<summary>See more</summary>
	
	exception: anError

	exception _ anError
</details>

## Warning

A Warning is a Notification which by default should be brought to the attention of the user.

### Methods
#### Warning>>#defaultAction

The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message.


<details>
	<summary>See more</summary>
	
	defaultAction
	"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."

	Debugger
		openContext: thisContext
		label: 'Warning'
		contents: self messageText.
	self resume
</details>

## ZeroDivide

ZeroDivide may be signaled when a mathematical division by 0 is attempted.

### Methods
#### ZeroDivide>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable
	
"Determine whether an exception is resumable."

	^true
</details>

#### ZeroDivide>>#floatErrorValue

<details>
	<summary>See more</summary>
	
	floatErrorValue
	| answerClass answerSign |

	receiver isZero ifTrue: [
		answerClass _ receiver isComplex ifTrue: [ receiver class ] ifFalse: [ Float ].
		^ answerClass nan ].

	receiver isComplex ifFalse: [
		answerSign _ arguments first isComplex
			ifTrue: [ receiver sign ]
			ifFalse: [ (receiver * arguments first) sign ].
		^ answerSign = -1
			ifTrue: [ Float negativeInfinity ]
			ifFalse: [ Float infinity ]].

	^ receiver class infinity
</details>

