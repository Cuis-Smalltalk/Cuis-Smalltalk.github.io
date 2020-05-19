## BreakPoint

This exception is raised on executing a breakpoint. "BreakPoint signal" is called from "Object>>break".

### Methods
## BreakpointManager

This class manages methods that include breakpoints. It has several class methods to install and uninstall breakpoints. Evaluating "BreakpointManager clear" will remove all installed breakpoints in the system. Known issues: - currently, only break-on-entry type of breakpoints are supported - emphasis change not implemented for MVC browsers - uninstalling the breakpoint doesn't auto-update other browsers - uninstalling a breakpoint while debugging should restart-simulate the current method Ernest Micklei, 2002 Send comments to emicklei@philemonworks.com

### Methods
## ContextVariablesInspector

I represent a query path into the internal representation of a ContextPart. Typically this is a context at a point in the query path of a Debugger. As a TextProvider, the string I represent is the value of the currently selected variable of the observed temporary variable of the context.

### Methods
#### ContextVariablesInspector>>#fieldList

Refer to the comment in Inspector|fieldList.


<details>
	<summary>See more</summary>
	
	fieldList 
	"Refer to the comment in Inspector|fieldList."

	object
		ifNil: [^ Array with: 'thisContext'].
	^fieldList ifNil:[fieldList := (Array with: 'thisContext' with: 'stack top' with: 'all temp vars') , object tempNames]
</details>

#### ContextVariablesInspector>>#doItReceiver

Answer the object that should be informed of the result of evaluating a text selection.


<details>
	<summary>See more</summary>
	
	doItReceiver

	^object receiver
</details>

#### ContextVariablesInspector>>#bindingNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingNamesDo: aBlock
	fieldList do: aBlock
</details>

#### ContextVariablesInspector>>#contentsIsString

Hacked so contents empty when deselected and = long printString when item 3


<details>
	<summary>See more</summary>
	
	contentsIsString
	"Hacked so contents empty when deselected and = long printString when item 3"

	^ (selectionIndex = 3) | (selectionIndex = 0)
</details>

#### ContextVariablesInspector>>#hasBindingOf: aString

<details>
	<summary>See more</summary>
	
	hasBindingOf: aString
	^ fieldList includes: aString
</details>

#### ContextVariablesInspector>>#doItContext

Answer the context in which a text selection can be evaluated.


<details>
	<summary>See more</summary>
	
	doItContext

	^object
</details>

#### ContextVariablesInspector>>#selection

Refer to the comment in Inspector|selection.


<details>
	<summary>See more</summary>
	
	selection 
	"Refer to the comment in Inspector|selection."
	selectionIndex = 0 ifTrue:[^''].
	selectionIndex = 1 ifTrue: [^object].
	selectionIndex = 2 ifTrue: [^object stackPtr > 0 ifTrue: [object top]].
	selectionIndex = 3 ifTrue: [^object tempsAndValues].
	^object debuggerMap namedTempAt: selectionIndex - 3 in: object
</details>

#### ContextVariablesInspector>>#inspect: anObject

Initialize the receiver so that it is inspecting anObject. There is no current selection. Because no object's inspectorClass method answers this class, it is OK for this method to override Inspector >> inspect:


<details>
	<summary>See more</summary>
	
	inspect: anObject 
	"Initialize the receiver so that it is inspecting anObject. There is no 
	current selection.
	
	Because no object's inspectorClass method answers this class, it is OK for this method to
	override Inspector >> inspect: "
	fieldList := nil.
	object := anObject.
	self initialize.
	
</details>

#### ContextVariablesInspector>>#replaceSelectionValue: anObject

Refer to the comment in Inspector|replaceSelectionValue:.


<details>
	<summary>See more</summary>
	
	replaceSelectionValue: anObject 
	"Refer to the comment in Inspector|replaceSelectionValue:."

	^selectionIndex = 1
		ifTrue: [object]
		ifFalse: [object namedTempAt: selectionIndex - 3 put: anObject]
</details>

## Debugger

I represent the machine state at the time of an interrupted process. I also represent a query path into the state of the process. The debugger is typically viewed through a window that views the stack of suspended contexts, the code for, and execution point in, the currently selected message, and inspectors on both the receiver of the currently selected message, and the variables in the current context. Special note on recursive errors: Some errors affect Squeak's ability to present a debugger. This is normally an unrecoverable situation. However, if such an error occurs in an isolation layer, Squeak will attempt to exit from the isolation layer and then present a debugger. Here is the chain of events in such a recovery. * A recursive error is detected. * The current project is queried for an isolationHead * Changes in the isolationHead are revoked * The parent project of isolated project is returned to * The debugger is opened there and execution resumes. If the user closes that debugger, execution continues in the outer project and layer. If, after repairing some damage, the user proceeds from the debugger, then the isolationHead is re-invoked, the failed project is re-entered, and execution resumes in that world.

### Methods
#### Debugger>>#updateInspectors

Update the inspectors on the receiver's variables.


<details>
	<summary>See more</summary>
	
	updateInspectors 
	"Update the inspectors on the receiver's variables."

	receiverInspector
		ifNotNil: [receiverInspector update].
	contextVariablesInspector
		ifNotNil: [contextVariablesInspector update]
</details>

#### Debugger>>#stepIntoBlock

Send messages until you return to the present method context. Used to step into a block in the method.


<details>
	<summary>See more</summary>
	
	stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	self 
		handleLabelUpdatesIn: [interruptedProcess stepToHome: self selectedContext]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn
</details>

#### Debugger>>#process: aProcess context: aContext

<details>
	<summary>See more</summary>
	
	process: aProcess context: aContext

	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.
	externalInterrupt _ false.
	selectingPC _ true
</details>

#### Debugger>>#openFullAt: index

<details>
	<summary>See more</summary>
	
	openFullAt: index
	self triggerEvent: #closeViews.
	self toggleContextStackIndex: index.
	self openFullMorphicLabel: 'Debugger'
</details>

#### Debugger>>#down

move down the context stack to the previous (enclosing) context


<details>
	<summary>See more</summary>
	
	down
	"move down the context stack to the previous (enclosing) context"

	self toggleContextStackIndex: contextStackIndex+1
</details>

#### Debugger>>#receiver

Answer the receiver of the selected context, if any. Answer nil otherwise.


<details>
	<summary>See more</summary>
	
	receiver
	"Answer the receiver of the selected context, if any. Answer nil 
	otherwise."

	contextStackIndex = 0
		ifTrue: [^nil]
		ifFalse: [^self selectedContext receiver]
</details>

#### Debugger>>#overrideMethodOnSubclass

<details>
	<summary>See more</summary>
	
	overrideMethodOnSubclass

	| chosenClass lastPossibleClass message methodCategory methodClass receiverClass |

	methodCategory _ self interruptedContext method category.
	methodClass _ self interruptedContext method methodClass.
	receiverClass _ self interruptedContext receiver class.
	lastPossibleClass _ (receiverClass withAllSuperclassesPreviousTo: methodClass) last.
	message _ self interruptedContext messageForYourself.

	chosenClass _ self
		askForSuperclassOf: receiverClass
		upTo: lastPossibleClass
		toImplement: message selector
		withCaption: 'Override #', message selector, ' in which class?'
		ifCancel: [^self].
	
	^ self implement: message inCategory: methodCategory fromClass: chosenClass context: self interruptedContext
</details>

#### Debugger>>#restart

Proceed from the initial state of the currently selected context.


<details>
	<summary>See more</summary>
	
	restart
	"Proceed from the initial state of the currently selected context."

	| ctxt |
	self checkContextSelection.
	ctxt _ interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext ifTrue: [
		interruptedProcess restartTop; stepToSendOrReturn].
	self resetContext: ctxt
</details>

#### Debugger>>#handleLabelUpdatesIn: aBlock whenExecuting: aContext

Send the selected message in the accessed method, and regain control after the invoked method returns.


<details>
	<summary>See more</summary>
	
	handleLabelUpdatesIn: aBlock whenExecuting: aContext
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	^aBlock
		on: Notification
		do: [:aNotification|
			aNotification 
				withNewDebuggerLabelOn: aContext
				do: [ :aNewLabel | 
					self labelString: aNewLabel.
					aNotification resume]
				ifNone: [aNotification pass]]
</details>

#### Debugger>>#computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName  

	contextStackIndex = 0 
		ifTrue: [ anAutocompleter computeMessageEntriesForUnknowClass ]
		ifFalse: [ anAutocompleter computeMessageEntriesForClass: (self receiver instVarNamed: aName) class ]
</details>

#### Debugger>>#askForCategoryIn: aClass default: aDefaultCategory

<details>
	<summary>See more</summary>
	
	askForCategoryIn: aClass default: aDefaultCategory

	| categories index category classCategories |
	
	categories := OrderedCollection with: 'new ...'. 
	
	aClass isMeta ifTrue: [ categories add: Categorizer instanceCreation ].
	classCategories := aClass allMethodCategoriesIntegratedThrough: Object.
	aClass isMeta ifTrue: [ classCategories remove: Categorizer instanceCreation ifAbsent: []].
	
	categories addAll: classCategories.	
	index := PopUpMenu 
		withCaption: 'Please provide a good category for the new method!'
		chooseFrom: categories.
	index = 0 ifTrue: [^ aDefaultCategory].
	category := index = 1 
		ifTrue: [FillInTheBlankMorph request: 'Enter category name:']
		ifFalse: [categories at: index].
		
	^ category isEmpty ifTrue: [aDefaultCategory] ifFalse: [category]
</details>

#### Debugger>>#wasInterruptedOnSubclassResponsibility

<details>
	<summary>See more</summary>
	
	wasInterruptedOnSubclassResponsibility

	^self interruptedContext sender ifNil: [ false ] ifNotNil: [ :senderContext | senderContext selector == #subclassResponsibility ]
</details>

#### Debugger>>#openNotifierContents: msgString label: label

Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired.


<details>
	<summary>See more</summary>
	
	openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices , (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			forceOpen: self
			label: label
			message: msg ].
</details>

#### Debugger>>#contextStackIndex

Answer the index of the selected context.


<details>
	<summary>See more</summary>
	
	contextStackIndex
	"Answer the index of the selected context."

	^contextStackIndex
</details>

#### Debugger>>#interruptedProcessUI: aWorld

<details>
	<summary>See more</summary>
	
	interruptedProcessUI: aWorld
	interruptedProcessUI _ aWorld
</details>

#### Debugger>>#askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: aCaptionText ifCancel: cancelBlock

<details>
	<summary>See more</summary>
	
	askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: aCaptionText ifCancel: cancelBlock

	| classes chosenClassIndex |

	classes _ aClass = aSuperclass ifTrue: [ { aClass } ] ifFalse: [ aClass withAllSuperclassesUpTo: aSuperclass ].
	chosenClassIndex _ PopUpMenu
		withCaption: aCaptionText
		chooseFrom: (classes collect: [:c | c name]).
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].

	^ classes at: chosenClassIndex
</details>

#### Debugger>>#proceedValue: anObject

Set the value to be returned to the selected context when the interrupted process proceeds.


<details>
	<summary>See more</summary>
	
	proceedValue: anObject 
	"Set the value to be returned to the selected context when the interrupted 
	process proceeds."

	proceedValue _ anObject
</details>

#### Debugger>>#up

move up the context stack to the next (enclosed) context


<details>
	<summary>See more</summary>
	
	up
	"move up the context stack to the next (enclosed) context"

	contextStackIndex > 1 ifTrue: [self toggleContextStackIndex: contextStackIndex-1]
</details>

#### Debugger>>#receiverInspectorObject: obj context: ctxt

set context before object so it can refer to context when building field list


<details>
	<summary>See more</summary>
	
	receiverInspectorObject: obj context: ctxt

	"set context before object so it can refer to context when building field list"
	receiverInspector context: ctxt.
	receiverInspector object: obj.

</details>

#### Debugger>>#computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockTempVarNamed: aName  

	^self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
</details>

#### Debugger>>#resetContext: aContext

Used when a new context becomes top-of-stack, for instance when the method of the selected context is re-compiled, or the simulator steps or returns to a new method. There is room for much optimization here, first to save recomputing the whole stack list (and text), and secondly to avoid recomposing all that text (by editing the TextComposition instead of recreating it).


<details>
	<summary>See more</summary>
	
	resetContext: aContext 
	"Used when a new context becomes top-of-stack, for instance when the
	method of the selected context is re-compiled, or the simulator steps or
	returns to a new method. There is room for much optimization here, first
	to save recomputing the whole stack list (and text), and secondly to avoid
	recomposing all that text (by editing the TextComposition instead of recreating it)."

	| oldContext |
	oldContext := self selectedContext.
	contextStackTop := aContext.
	self newStack: contextStackTop contextStack.
	self changed: #contextStackList.
	self contextStackIndex: 1 oldContextWas: oldContext.
	self acceptedContentsChanged.

</details>

#### Debugger>>#fixReceiverInspector

Make receiver inspector work on current context receiver. Create a new inspector if needed


<details>
	<summary>See more</summary>
	
	fixReceiverInspector
	"Make receiver inspector work on current context receiver.
	Create a new inspector if needed"

	| currentReceiver requiredInspectorClass oldInspectorClass |
	currentReceiver _ self receiver.
	requiredInspectorClass _ currentReceiver inspectorClass.
	oldInspectorClass _ receiverInspector class.
	
	oldInspectorClass ~= requiredInspectorClass ifTrue: [
		oldInspectorClass format = requiredInspectorClass format
			ifTrue: [receiverInspector primitiveChangeClassTo: requiredInspectorClass basicNew]
			ifFalse: [receiverInspector becomeForward: (requiredInspectorClass basicNew copyFrom: receiverInspector)]].
	
	receiverInspector object: currentReceiver
</details>

#### Debugger>>#sendProceeds

<details>
	<summary>See more</summary>
	
	sendProceeds
	sendProceeds _ true
</details>

#### Debugger>>#toggleContextStackIndex: anInteger

If anInteger is the same as the index of the selected context, deselect it. Otherwise, the context whose index is anInteger becomes the selected context.


<details>
	<summary>See more</summary>
	
	toggleContextStackIndex: anInteger 
	"If anInteger is the same as the index of the selected context, deselect it. 
	Otherwise, the context whose index is anInteger becomes the selected 
	context."

	self contextStackIndex: 
		(contextStackIndex = anInteger
			ifTrue: [0]
			ifFalse: [anInteger])
		oldContextWas:
		(contextStackIndex = 0
			ifTrue: [nil]
			ifFalse: [contextStack at: contextStackIndex])
</details>

#### Debugger>>#selectedClass

Answer the class in which the currently selected context's method was found.


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class in which the currently selected context's method was 
	found."

	^self selectedContext ifNotNil: [ :ctx | ctx methodClass ]
</details>

#### Debugger>>#wantsSteps

Overridden by morphic classes whose instances want to be stepped all the time, or by model classes who want their morphic views to be stepped all the time. Some classes might answer false to this message, and call #startStepping #startSteppingStepTime: #stopStepping as appropriate


<details>
	<summary>See more</summary>
	
	wantsSteps
 
	^ true
</details>

#### Debugger>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	sendProceeds _ false
</details>

#### Debugger>>#fullStack

Change from displaying the minimal stack to a full one.


<details>
	<summary>See more</summary>
	
	fullStack
	"Change from displaying the minimal stack to a full one."

	self contextStackList size > 20 "Already expanded"
		ifTrue:
			[self changed: #flash]
		ifFalse:
			[self contextStackIndex = 0 ifFalse: [
				self toggleContextStackIndex: self contextStackIndex].
			self fullyExpandStack]
</details>

#### Debugger>>#doItReceiver

Answer the object that should be informed of the result of evaluating a text selection.


<details>
	<summary>See more</summary>
	
	doItReceiver
	"Answer the object that should be informed of the result of evaluating a
	text selection."

	^self receiver
</details>

#### Debugger>>#wasInterrupedOnDoesNotUnderstand

<details>
	<summary>See more</summary>
	
	wasInterrupedOnDoesNotUnderstand

	^self interruptedContext selector == #doesNotUnderstand:
</details>

#### Debugger>>#labelString

<details>
	<summary>See more</summary>
	
	labelString
	^labelString
</details>

#### Debugger>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
</details>

#### Debugger>>#runToSelection: selectionInterval

<details>
	<summary>See more</summary>
	
	runToSelection: selectionInterval
	| currentContext |
	self pc first >= selectionInterval first ifTrue: [ ^self ].
	currentContext _ self selectedContext.
	[ currentContext == self selectedContext and: [ self pc first < selectionInterval first ] ] whileTrue: [ self doStep ].
</details>

#### Debugger>>#wasInterruptedOnOverridableMethod

<details>
	<summary>See more</summary>
	
	wasInterruptedOnOverridableMethod
	| methodClass receiverClass |
	
	methodClass _ self interruptedContext method methodClass.
	receiverClass _ self interruptedContext receiver class.
	
	^ methodClass ~= receiverClass
</details>

#### Debugger>>#fullyExpandStack

Expand the stack to include all of it, rather than the first four or five contexts.


<details>
	<summary>See more</summary>
	
	fullyExpandStack
	"Expand the stack to include all of it, rather than the first four or five
	contexts."

	self newStack: contextStackTop contextStack.
	self changed: #contextStackList
</details>

#### Debugger>>#computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName  

	| context tempIndex |
							
	context := self selectedContext.
	tempIndex := context tempNames indexOf: aName ifAbsent: [ ^ anAutocompleter computeMessageEntriesForUnknowClass ].
	
	anAutocompleter computeMessageEntriesForClass: 
		(self debuggerMap namedTempAt: tempIndex in: context) class 
</details>

#### Debugger>>#proceedValue

Answer the value to return to the selected context when the interrupted process proceeds.


<details>
	<summary>See more</summary>
	
	proceedValue
	"Answer the value to return to the selected context when the interrupted 
	process proceeds."

	^proceedValue
</details>

#### Debugger>>#resetToSelectedContextWith: newMethod

<details>
	<summary>See more</summary>
	
	resetToSelectedContextWith: newMethod

	| ctxt |

	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	
</details>

#### Debugger>>#createMethodWhenDoesNotUnderstand

The doesNotUndertand context must be selected - Hernan


<details>
	<summary>See more</summary>
	
	createMethodWhenDoesNotUnderstand

	| message chosenClass interruptedContext |
	
	"The doesNotUndertand context must be selected - Hernan"
	contextStackIndex = 1 ifFalse: [ self contextStackIndex: 1 oldContextWas: self selectedContext ].
		
	interruptedContext _ self interruptedContext.
	message _ interruptedContext tempAt: 1.
	
	chosenClass _ self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	
	self implement: message inClass: chosenClass context: self selectedContext


</details>

#### Debugger>>#contents: aText notifying: aController

The retrieved information has changed and its source must now be updated. In this case, the retrieved information is the method of the selected context.


<details>
	<summary>See more</summary>
	
	contents: aText notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."

	| result selector classOfMethod category h newMethod |

	contextStackIndex = 0 ifTrue: [^false].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
	selector :=self selectedClass parserClass selectorFrom: aText.

	selector ~= self selectedMessageName ifTrue: [
		self inform: 'Can not change the selector in the debugger'.
		^false].	
 	(classOfMethod = UndefinedObject and: [ selector = Scanner doItSelector or: [ selector = Scanner doItInSelector ]]) ifTrue: [
		self inform: 'DoIt and DoItIn: methods can not be changed'.
	 	^false].

	self selectedContext isExecutingBlock ifTrue: [
		h := self selectedContext activeHome.
		h ifNil: [
			self inform: 'Method for block not found on stack, can''t edit and continue'.
			^false].
		(self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse: [^false].
		self resetContext: h.
		(result := self contents: aText notifying: aController) ifTrue: [self acceptedContentsChanged].
		^result].
		
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.

	newMethod isQuick ifTrue: [
		contextStackIndex + 1 > contextStack size ifTrue: [
			self inform: 'Can not compile a quick method in the stack base context'.
			^false].
		self down.
		self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].

	self resetToSelectedContextWith: newMethod.
	
	^true
</details>

#### Debugger>>#doStep

Send the selected message in the accessed method, and regain control after the invoked method returns.


<details>
	<summary>See more</summary>
	
	doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	| currentContext newContext |

	currentContext := self selectedContext.
	newContext := self handleLabelUpdatesIn: [interruptedProcess completeStep: currentContext]
						whenExecuting: currentContext.
	newContext == currentContext ifTrue:
		[newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse:
			[newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].

</details>

#### Debugger>>#lowSpaceChoices

Return a notifier message string to be presented when space is running low.


<details>
	<summary>See more</summary>
	
	lowSpaceChoices
	"Return a notifier message string to be presented when space is running low."

	^ 'Warning! Cuis is almost out of memory!

Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don''t panic, but do proceed with caution.

Here are some suggestions:

- If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.

- If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
   > Close any windows that are not needed.
   > Get rid of some large objects (e.g., images).
   > Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Squeak VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.

- If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!).

'

</details>

#### Debugger>>#pcRange

Answer the indices in the source code for the method corresponding to the selected context's program counter value.


<details>
	<summary>See more</summary>
	
	pcRange
	"Answer the indices in the source code for the method corresponding to 
	the selected context's program counter value."

	(selectingPC and: [contextStackIndex ~= 0]) ifFalse:
		[^1 to: 0].
	self selectedContext isDead ifTrue:
		[^1 to: 0].
	^self selectedContext debuggerMap
		rangeForPC: self selectedContext pc
		contextIsActiveContext: contextStackIndex = 1
</details>

#### Debugger>>#shouldBeAbleToCreateMethod

<details>
	<summary>See more</summary>
	
	shouldBeAbleToCreateMethod

	^self wasInterrupedOnDoesNotUnderstand or: [ self wasInterruptedOnSubclassResponsibility]
</details>

#### Debugger>>#interruptedProcess

Answer the interrupted process.


<details>
	<summary>See more</summary>
	
	interruptedProcess
	"Answer the interrupted process."

	^interruptedProcess
</details>

#### Debugger>>#implement: aMessage inClass: aClass context: aContext

<details>
	<summary>See more</summary>
	
	implement: aMessage inClass: aClass context: aContext 
	
	self implement: aMessage inCategory: (self askForCategoryIn: aClass default: Categorizer default) fromClass: aClass context: aContext
</details>

#### Debugger>>#askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector ifCancel: cancelBlock

<details>
	<summary>See more</summary>
	
	askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector ifCancel: cancelBlock

	^ self askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector withCaption: 'Define #', aSelector, ' in which class?' ifCancel: cancelBlock
</details>

#### Debugger>>#receiverInspector

Answer the instance of Inspector that is providing a view of the variables of the selected context's receiver.


<details>
	<summary>See more</summary>
	
	receiverInspector
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context's receiver."

	^receiverInspector
</details>

#### Debugger>>#contextStackIndex: anInteger oldContextWas: oldContext

Change the context stack index to anInteger, perhaps in response to user selection.


<details>
	<summary>See more</summary>
	
	contextStackIndex: anInteger oldContextWas: oldContext
	"Change the context stack index to anInteger, perhaps in response to user selection."

	| isNewMethod selectedContextSlotName index |
	contextStackIndex _ anInteger.
	anInteger = 0 ifTrue: [
		currentCompiledMethod _ nil.
		self changed: #contextStackIndex.
		self acceptedContentsChanged.
		contextVariablesInspector object: nil.
		self fixReceiverInspector.
		^ self ].
	selectedContextSlotName _ contextVariablesInspector selectedSlotName.
	isNewMethod _ oldContext == nil
					or: [ oldContext method ~~ (currentCompiledMethod _ self selectedContext method) ].
	isNewMethod ifTrue: [
		self acceptedContentsChanged.
		self pcRange ].
	self changed: #contextStackIndex.
	self triggerEvent: #decorateButtons.
	contextVariablesInspector object: self selectedContext.
	((index _ contextVariablesInspector fieldList indexOf: selectedContextSlotName) ~= 0 and: [
		index ~= contextVariablesInspector selectionIndex ]) ifTrue: [
			contextVariablesInspector toggleIndex: index ].
	self fixReceiverInspector.
	isNewMethod ifFalse: [ self changed: #contentsSelection ].
</details>

#### Debugger>>#selectedMessageName

Answer the message selector of the currently selected context. If the method is unbound we can still usefully answer its old selector.


<details>
	<summary>See more</summary>
	
	selectedMessageName
	"Answer the message selector of the currently selected context.
	 If the method is unbound we can still usefully answer its old selector."

	| selector |
	selector := (self selectedContext ifNil: [ ^nil ]) selector.
	^(selector ~~ self selectedContext method selector
	    and: [selector beginsWith: 'DoIt'])
		ifTrue: [self selectedContext method selector]
		ifFalse: [selector]
</details>

#### Debugger>>#isNotifier

Return true if this debugger has not been expanded into a full sized window


<details>
	<summary>See more</summary>
	
	isNotifier
	"Return true if this debugger has not been expanded into a full sized window"

	^ receiverInspector == nil
</details>

#### Debugger>>#externalInterrupt: aBoolean

<details>
	<summary>See more</summary>
	
	externalInterrupt: aBoolean

	externalInterrupt _ aBoolean 
</details>

#### Debugger>>#contextStackList

Answer the array of contexts.


<details>
	<summary>See more</summary>
	
	contextStackList
	"Answer the array of contexts."

	^contextStackList
</details>

#### Debugger>>#bindingNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	bindingNamesDo: aBlock

	self selectedContext tempNames do: aBlock
</details>

#### Debugger>>#contextVariablesInspector

Answer the instance of Inspector that is providing a view of the variables of the selected context.


<details>
	<summary>See more</summary>
	
	contextVariablesInspector
	"Answer the instance of Inspector that is providing a view of the 
	variables of the selected context."

	^contextVariablesInspector
</details>

#### Debugger>>#openFullNoSuspendLabel: aString

Create and schedule a full debugger with the given label. Do not terminate the current active process.


<details>
	<summary>See more</summary>
	
	openFullNoSuspendLabel: aString
	"Create and schedule a full debugger with the given label. Do not terminate the current active process."

	self openFullMorphicLabel: aString.
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess
</details>

#### Debugger>>#windowIsClosing

My window is being closed; clean up. Restart the low space watcher.


<details>
	<summary>See more</summary>
	
	windowIsClosing
	"My window is being closed; clean up. Restart the low space watcher."

	interruptedProcess
		ifNil: [^ self].
	interruptedProcess terminate.
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	Smalltalk installLowSpaceWatcher.  "restart low space handler"

</details>

#### Debugger>>#proceed

Proceed execution of the receiver's model, starting after the expression at which an interruption occurred.


<details>
	<summary>See more</summary>
	
	proceed
	"Proceed execution of the receiver's model, starting after the expression at 
	which an interruption occurred."

	Smalltalk okayToProceedEvenIfSpaceIsLow ifTrue: [
		"So we don't send #windowIsClosing, which acts like 'terminate'"
		self triggerEvent: #closeViews.
		"Resume instead"
		self resumeProcess ]
</details>

#### Debugger>>#interruptedContext

Answer the suspended context of the interrupted process.


<details>
	<summary>See more</summary>
	
	interruptedContext
	"Answer the suspended context of the interrupted process."

	^contextStackTop
</details>

#### Debugger>>#implement: aMessage inCategory: aCategory fromClass: aClass context: aContext

<details>
	<summary>See more</summary>
	
	implement: aMessage inCategory: aCategory fromClass: aClass context: aContext

	aClass
		compile: (aMessage createStubMethodFor: aClass)
		classified: aCategory.
		
	aContext privRefreshWith: (aClass lookupSelector: aMessage selector).
	aMessage arguments withIndexDo: [ :arg :index | aContext tempAt: index put: arg ].
	self resetContext: aContext
</details>

#### Debugger>>#returnValue: expression

Force a return of a given value to the previous context!


<details>
	<summary>See more</summary>
	
	returnValue: expression
	"Force a return of a given value to the previous context!"

	| previous selectedContext value |
	contextStackIndex = 0 ifTrue: [^Smalltalk beep].
	selectedContext _ self selectedContext.
	value _ Compiler new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous _ selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value
</details>

#### Debugger>>#instanceVariableRenamed

<details>
	<summary>See more</summary>
	
	instanceVariableRenamed

	| newMethod |
	
	newMethod := self selectedClass compiledMethodAt: self selectedContext selector.
	self resetToSelectedContextWith: newMethod 
</details>

#### Debugger>>#selectedMessage

Answer the source code of the currently selected context.


<details>
	<summary>See more</summary>
	
	selectedMessage
	"Answer the source code of the currently selected context."
	^self selectedContext debuggerMap sourceText
</details>

#### Debugger>>#toggleBreakOnEntry

Install or uninstall a halt-on-entry breakpoint


<details>
	<summary>See more</summary>
	
	toggleBreakOnEntry
	"Install or uninstall a halt-on-entry breakpoint"

	| selectedMethod |
	self selectedClassOrMetaClass ifNil: [ ^self].
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	selectedMethod hasBreakpoint
		ifTrue:
			[BreakpointManager unInstall: selectedMethod]
		ifFalse:
			[BreakpointManager 
				installInClass: self selectedClassOrMetaClass
				selector: self selectedMessageName].
</details>

#### Debugger>>#debuggerMap

<details>
	<summary>See more</summary>
	
	debuggerMap
	^self selectedContext debuggerMap
</details>

#### Debugger>>#selectedContext

<details>
	<summary>See more</summary>
	
	selectedContext
	contextStackIndex = 0
		ifTrue: [^contextStackTop]
		ifFalse: [^contextStack at: contextStackIndex]
</details>

#### Debugger>>#createMethodOnSubclassResponsibility

<details>
	<summary>See more</summary>
	
	createMethodOnSubclassResponsibility

	| message chosenClass subclassResponsibilityContext |
	
	subclassResponsibilityContext _ self interruptedContext sender sender.
	message _ subclassResponsibilityContext messageForYourself.
	
	chosenClass _ self
		askForSuperclassOf: subclassResponsibilityContext receiver class
		upTo: subclassResponsibilityContext method methodClass
		toImplement: message selector
		ifCancel: [^self].
		
	self implement: message inClass: chosenClass context: subclassResponsibilityContext 


</details>

#### Debugger>>#send

Send the selected message in the accessed method, and take control in the method invoked to allow further step or send.


<details>
	<summary>See more</summary>
	
	send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send."
	sendProceeds ifTrue: [ ^ self proceed ].
	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.

</details>

#### Debugger>>#createMethod

<details>
	<summary>See more</summary>
	
	createMethod

	self wasInterrupedOnDoesNotUnderstand ifTrue: [ ^self createMethodWhenDoesNotUnderstand ].
	self wasInterruptedOnSubclassResponsibility ifTrue: [ ^self createMethodOnSubclassResponsibility ].
	self wasInterruptedOnOverridableMethod ifTrue: [ ^self overrideMethodOnSubclass ].
	
	self inform: 'Only available to override methods or for #doesNotUnderstand: and #subclassResponsibility' 
</details>

#### Debugger>>#messageListIndex

Answer the index of the currently selected context.


<details>
	<summary>See more</summary>
	
	messageListIndex
	"Answer the index of the currently selected context."

	^contextStackIndex
</details>

#### Debugger>>#contentsSelection

If we are just creating the method in the debugger, help the user by selecting the method body.


<details>
	<summary>See more</summary>
	
	contentsSelection

	"If we are just creating the method in the debugger, help the user by selecting the method body."
	| sendInterval |
	currentCompiledMethod ifNotNil: [
		((currentCompiledMethod hasLiteral: #shouldBeImplemented) and: [
			(currentCompiledMethod abstractPCForConcretePC: self selectedContext pc) = 1]) ifTrue: [
				sendInterval _ (self selectedContext debuggerMap abstractSourceMap at: 2 ifAbsent: [nil]).
				sendInterval ifNotNil: [ ^ sendInterval first - 5 to: sendInterval last + 1 ]]].

	^self pcRange
</details>

#### Debugger>>#peelToFirst

Peel the stack back to the second occurance of the currently selected message. Very useful for an infinite recursion. Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning. Also frees a lot of space!


<details>
	<summary>See more</summary>
	
	peelToFirst
	"Peel the stack back to the second occurance of the currently selected message.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.  Also frees a lot of space!"

	| ctxt |
	contextStackIndex = 0 ifTrue: [^ Smalltalk beep].
	"self okToChange ifFalse: [^ self]."
	ctxt := interruptedProcess popTo: self selectedContext findSecondToOldestSimilarSender.
	self resetContext: ctxt.

</details>

#### Debugger>>#newStack: stack

<details>
	<summary>See more</summary>
	
	newStack: stack
	| oldStack diff |
	oldStack _ contextStack.
	contextStack _ stack.
	(oldStack == nil or: [oldStack last ~~ stack last])
		ifTrue: [contextStackList _ contextStack collect: [:ctx | ctx printString].
				^ self].
	"May be able to re-use some of previous list"
	diff _ stack size - oldStack size.
	contextStackList _ diff <= 0
		ifTrue: [contextStackList copyFrom: 1-diff to: oldStack size]
		ifFalse: [diff > 1
				ifTrue: [contextStack collect: [:ctx | ctx printString]]
				ifFalse: [(Array with: stack first printString) , contextStackList]]
</details>

#### Debugger>>#checkContextSelection

<details>
	<summary>See more</summary>
	
	checkContextSelection

	contextStackIndex = 0 ifTrue: [self contextStackIndex: 1 oldContextWas: nil].

</details>

#### Debugger>>#hasBindingOf: aString

<details>
	<summary>See more</summary>
	
	hasBindingOf: aString
	^self selectedContext tempNames includes: aString
</details>

#### Debugger>>#askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock

<details>
	<summary>See more</summary>
	
	askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock

	^self askForSuperclassOf: aClass upTo: ProtoObject toImplement: aSelector ifCancel: cancelBlock

</details>

#### Debugger>>#shouldStyle: text with: anSHTextStyler

This is a notification that anSHTextStyler is about to re-style its text. Set the classOrMetaClass in anSHTextStyler, so that identifiers will be resolved correctly. Answer true to allow styling to proceed, or false to veto the styling


<details>
	<summary>See more</summary>
	
	shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		disableFormatAndConvert;
		workspace: self.
	^true
</details>

#### Debugger>>#selectPC

Toggle the flag telling whether to automatically select the expression currently being executed by the selected context.


<details>
	<summary>See more</summary>
	
	selectPC
	"Toggle the flag telling whether to automatically select the expression 
	currently being executed by the selected context."

	selectingPC _ selectingPC not
</details>

#### Debugger>>#openFullMorphicLabel: aLabelString

Open a full morphic debugger with the given label


<details>
	<summary>See more</summary>
	
	openFullMorphicLabel: aLabelString
	"Open a full morphic debugger with the given label"

	| oldContextStackIndex |
	oldContextStackIndex _ contextStackIndex.
	self expandStack. "Sets contextStackIndex to zero."

	DebuggerWindow open: self label: aLabelString.
	self toggleContextStackIndex: oldContextStackIndex
</details>

#### Debugger>>#pc

<details>
	<summary>See more</summary>
	
	pc

	^ self pcRange
</details>

#### Debugger>>#classOfThisContext

<details>
	<summary>See more</summary>
	
	classOfThisContext
	
	^ self selectedContext class
</details>

#### Debugger>>#labelString: aString

<details>
	<summary>See more</summary>
	
	labelString: aString
	labelString _ aString.
	self changed: #relabel
</details>

#### Debugger>>#isEditingMethod

<details>
	<summary>See more</summary>
	
	isEditingMethod

	^true
</details>

#### Debugger>>#expandStack

A Notifier is being turned into a full debugger. Show a substantial amount of stack in the context pane.


<details>
	<summary>See more</summary>
	
	expandStack
	"A Notifier is being turned into a full debugger.  Show a substantial amount of stack in the context pane."

	self newStack: (contextStackTop stackOfSize: 20).
	contextStackIndex _ 0.
	receiverInspector _ Inspector inspect: nil.
	contextVariablesInspector _ ContextVariablesInspector inspect: nil.
	proceedValue _ nil
</details>

#### Debugger>>#resumeProcess

<details>
	<summary>See more</summary>
	
	resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		Processor activeProcess animatedUI = interruptedProcessUI ifTrue: [
			interruptedProcess animatedUI: interruptedProcessUI.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, then it will terminate and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]
</details>

#### Debugger>>#acceptedStringOrText

Depending on the current selection, different information is retrieved. Answer a string description of that information. This information is the method in the currently selected context.


<details>
	<summary>See more</summary>
	
	acceptedStringOrText 
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information.  This information is the
	method in the currently selected context."

	^ self selectedContext
			ifNotNil: [self selectedMessage]
			ifNil: [String new]
</details>

#### Debugger>>#computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName

<details>
	<summary>See more</summary>
	
	computeMessageEntriesIn: anAutocompleter ofBlockArgNamed: aName  

	self computeMessageEntriesIn: anAutocompleter ofTempVarNamed: aName 
</details>

#### Debugger>>#doItContext

Answer the context in which a text selection can be evaluated.


<details>
	<summary>See more</summary>
	
	doItContext
	"Answer the context in which a text selection can be evaluated."

	contextStackIndex = 0
		ifTrue: [^super doItContext]
		ifFalse: [^self selectedContext]
</details>

#### Debugger>>#where

Select the expression whose evaluation was interrupted.


<details>
	<summary>See more</summary>
	
	where
	"Select the expression whose evaluation was interrupted."

	selectingPC _ true.
	self contextStackIndex: contextStackIndex oldContextWas: self selectedContext

</details>

## DebuggerMethodMap

I am a place-holder for information needed by the Debugger to inspect method activations. I insulate the debugger from details of code generation such as exact bytecode offsets and temporary variable locations. I have two concreate subclasses, one for methods compiled using BlueBook blocks and one for methods compiled using Closures. These classes deal with temporary variable access. My function is to abstract the source map away from actual bytecode pcs to abstract bytecode pcs. To reduce compilation time I try and defer as much computation to access time as possible as instances of me will be created after each compilation. I maintain a WeakIdentityDictionary of method to DebuggerMethodMap to cache maps. I refer to my method through a WeakArray to keep the map cache functional. If the reference from a DebuggerMethodMap to its method were strong then the method would never be dropped from the cache because the reference from its map would keep it alive. Instance variables blockExtentsToTempsRefs <Dictionary of: Interval -> Array of: (Array with: String with: (Integer | (Array with: Integer with: Integer)))> maps a block extent to an Array of temp references for that block/method. Each reference is a pair of temp name and index, where the index can itself be a pair for a remote temp. startpcsToTempRefs <Dictionary of: Integer -> Array of: (Array with: String with: temp reference)> where temp reference ::= Integer | (Array with: Integer with: Integer) | (Array with: #outer with: temp reference)

### Methods
#### DebuggerMethodMap>>#method

<details>
	<summary>See more</summary>
	
	method
	^methodReference at: 1
</details>

#### DebuggerMethodMap>>#sourceText

<details>
	<summary>See more</summary>
	
	sourceText

	^methodNode sourceText
</details>

#### DebuggerMethodMap>>#privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents

<details>
	<summary>See more</summary>
	
	privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	| nameRefPair namesAndRefs |
	namesAndRefs _ self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: theContextsStartpcsToBlockExtents.
	^index <= namesAndRefs size
		ifTrue: [
			nameRefPair _ namesAndRefs
				at: index
				ifAbsent: [ aContext errorSubscriptBounds: index ].
			self privateDereference: nameRefPair last in: aContext ]
		ifFalse: [
			(aContext method xtraBindings at: index - namesAndRefs size) value ]
</details>

#### DebuggerMethodMap>>#tempsAndValuesForContext: aContext

Return a string of the temporary variabls and their current values


<details>
	<summary>See more</summary>
	
	tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"
	| aStream |
	aStream := WriteStream on: (String new: 100).
	(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
		aStream nextPutAll: title; nextPut: $:; space; tab.
		aContext print: (self namedTempAt: index in: aContext) on: aStream.
		aStream newLine].
	^aStream contents
</details>

#### DebuggerMethodMap>>#rangeForPC: contextsConcretePC contextIsActiveContext: contextIsActiveContext

Answer the indices in the source code for the supplied pc. If the context is the actve context (is at the hot end of the stack) then its pc is the current pc. But if the context isn't, because it is suspended sending a message, then its current pc is the previous pc.


<details>
	<summary>See more</summary>
	
	rangeForPC: contextsConcretePC contextIsActiveContext: contextIsActiveContext
	"Answer the indices in the source code for the supplied pc.
	 If the context is the actve context (is at the hot end of the stack)
	 then its pc is the current pc.  But if the context isn't, because it is
	 suspended sending a message, then its current pc is the previous pc."

	| pc end |
	pc _ self method abstractPCForConcretePC:
		(contextIsActiveContext
			ifTrue: [ contextsConcretePC ]
			ifFalse: [
				(self method pcPreviousTo: contextsConcretePC) ifNil: [ contextsConcretePC ]]).
	(self abstractSourceMap includesKey: pc) ifTrue: [
		^self abstractSourceMap at: pc].
	sortedSourceMap ifNil: [
		sortedSourceMap _ self abstractSourceMap.
		sortedSourceMap _ (sortedSourceMap keys
			collect: [ :key | key -> (sortedSourceMap at: key)])
				asSortedCollection ].
	sortedSourceMap
		findBinaryIndex: [ :assoc | pc - assoc key ]
		do: [ :i | ^(sortedSourceMap at: i) value ]
		ifNone: [ :lower :upper |
			lower = 0 ifTrue: [ ^1 to: 0 ].
			upper <= sortedSourceMap size ifTrue: [
				^(sortedSourceMap at: upper) value ].    "No match, but  a nearby element."
			end _ sortedSourceMap detectMax: [ :each |
				each value last ].
			end _ end value last.
			^end + 1 to: end ].

	"| method source scanner map |
	 method := DebuggerMethodMap compiledMethodAt: #rangeForPC:contextIsActiveContext:.
	method := Debugger >> #pcRange.
	source := method getSourceFromFile asString.
	scanner := InstructionStream on: method.
	map := method debuggerMap.
	Array streamContents:
		[:ranges|
		[scanner atEnd] whileFalse:
			[| range |
			 range := map rangeForPC: scanner pc contextIsActiveContext: true.
			 ((map abstractSourceMap includesKey: scanner abstractPC)
			  and: [range first ~= 0]) ifTrue:
				[ranges nextPut: (source copyFrom: range first to: range last)].
			scanner interpretNextInstructionFor: InstructionClient new]]"
</details>

#### DebuggerMethodMap>>#privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents

Answer the sequence of temps in scope in aContext in the natural order, outermost arguments and temporaries first, innermost last. Each temp is a pair of the temp's name followed by a reference. The reference can be integer - index of temp in aContext #( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext #( outer. temp reference ) - a temp reference in an outer context.


<details>
	<summary>See more</summary>
	
	privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := methodNode blockExtentsToTempsMap.
		 blockExtentsToTempRefs ifNil:
			["an empty method.  shouldn't be able to step into here but it
			  can happen in weird circumstances (i.e. with MethodWrapper)."
			blockExtentsToTempRefs := Dictionary new.
			blockExtentsToTempRefs
				at: (theContextsStartpcsToBlockExtents at: aContext startpc)
				put: {}].
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs outerTemps |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer|
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]
</details>

#### DebuggerMethodMap>>#tempNamesForContext: aContext

Answer an Array of all the temp names in scope in aContext starting with the home's first local (the first argument or first temporary if no arguments).


<details>
	<summary>See more</summary>
	
	tempNamesForContext: aContext
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^((self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents) collect: [ :pair |
			pair first]),
		(aContext method xtraBindings collect: [ :assoc | assoc key ])
</details>

#### DebuggerMethodMap>>#namedTempAt: index put: aValue in: aContext

Assign the value of the temp at index in aContext where index is relative to the array of temp names answered by tempNamesForContext:. If the value is a copied value we also need to set it along the lexical chain.


<details>
	<summary>See more</summary>
	
	namedTempAt: index put: aValue in: aContext
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	^self
		privateTempAt: index
		in: aContext
		put: aValue
		startpcsToBlockExtents: aContext method startpcsToBlockExtents
</details>

#### DebuggerMethodMap>>#abstractSourceMap

Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>.


<details>
	<summary>See more</summary>
	
	abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."

	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |

	abstractSourceRanges ifNotNil: [ ^abstractSourceRanges].

	methodNode encoder hasGeneratedMethod
		ifTrue: [
			rawSourceRanges := methodNode encoder rawSourceRanges.
		 	theMethodToScan := self method ]
		ifFalse: [
			"If the methodNode hasn't had a method generated it doesn't have pcs set in its
			 nodes so we must generate a new method and might as well use it for scanning."
			 [methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
				rawSourceRanges := ranges.
			 	theMethodToScan := method ]]
			on: UndeclaredVariableWarning
			do: [ :ex | ex resume ]].

	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [ | realRange |
			realRange := (methodNode isMultipleRanges: range) ifTrue: [ range last ] ifFalse: [ range ].
			concreteSourceRanges at: node pc put: realRange ]].

	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	
	^abstractSourceRanges
</details>

#### DebuggerMethodMap>>#markRecentlyUsed

<details>
	<summary>See more</summary>
	
	markRecentlyUsed
	timestamp := Time localSecondClock
</details>

#### DebuggerMethodMap>>#privateDereference: tempReference in: aContext

Fetch the temporary with reference tempReference in aContext. tempReference can be integer - direct temp reference #( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index #( outer. temp reference ) - a temp reference in an outer context.


<details>
	<summary>See more</summary>
	
	privateDereference: tempReference in: aContext
	"Fetch the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue:
			[aContext tempAt: tempReference]
		ifFalse:
			[tempReference first == #outer
				ifTrue:
					[self privateDereference: tempReference last
						in: aContext outerContext]
				ifFalse: "If stopped before indirection vectors are created they will be nil. Simply answer nil"
					[(aContext tempAt: tempReference first) ifNotNil:
						[:indirectionVector|
						indirectionVector at: tempReference second]]]
</details>

#### DebuggerMethodMap>>#privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents

<details>
	<summary>See more</summary>
	
	privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents
	| nameRefPair |
	nameRefPair := (self privateTempRefsForContext: aContext
						 startpcsToBlockExtents: theContextsStartpcsToBlockExtents)
						at: index
						ifAbsent: [aContext errorSubscriptBounds: index].
	^self privateDereference: nameRefPair last in: aContext put: aValue
</details>

#### DebuggerMethodMap>>#namedTempAt: index in: aContext

Answer the value of the temp at index in aContext where index is relative to the array of temp names answered by tempNamesForContext:


<details>
	<summary>See more</summary>
	
	namedTempAt: index in: aContext
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"
	^self
		privateTempAt: index
		in: aContext
		startpcsToBlockExtents: aContext method startpcsToBlockExtents
</details>

#### DebuggerMethodMap>>#privateDereference: tempReference in: aContext put: aValue

Assign the temporary with reference tempReference in aContext. tempReference can be integer - direct temp reference #( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index #( outer. temp reference ) - a temp reference in an outer context.


<details>
	<summary>See more</summary>
	
	privateDereference: tempReference in: aContext put: aValue
	"Assign the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue:
			[aContext tempAt: tempReference put: aValue]
		ifFalse:
			[tempReference first == #outer
				ifTrue:
					[self privateDereference: tempReference last
						in: aContext outerContext
						put: aValue]
				ifFalse: "If stopped before indirection vectors are created they will be nil."
					[(aContext tempAt: tempReference first)
						ifNil: [ self inform: 'Cannot assign remote temp because indirection vector is nil.\Too early in method execution?' withNewLines.
							nil]
						ifNotNil:
							[:indirectionVector|
							indirectionVector
								at: tempReference second
								put: aValue]]]
</details>

#### DebuggerMethodMap>>#forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode

<CompiledMethod>


<details>
	<summary>See more</summary>
	
	forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
	self markRecentlyUsed
</details>

#### DebuggerMethodMap>>#timestamp

<details>
	<summary>See more</summary>
	
	timestamp
	^timestamp
</details>

