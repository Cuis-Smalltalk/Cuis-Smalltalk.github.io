## OutOfScopeNotification

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### OutOfScopeNotification>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction

	self resume: false
</details>

## ParserNotification

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ParserNotification>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^name
</details>

#### ParserNotification>>#initializeNamed: aName

<details>
	<summary>See more</summary>
	
	initializeNamed: aName

	name _ aName
</details>

## SyntaxErrorNotification

A SyntaxErrorNotification is an Exception occuring when compiling a Smalltalk source code with incorrect syntax. Note that in interactive mode, this exception is not raised because the Compiler will interact directly with source code editor. The defaultAction is to raise a SyntaxError pop up window so as to enable interactive handling even in non interactive mode. Instance Variables category: <String | nil> code: <String | Text | Stream> doitFlag: <Boolean> errorMessage: <String> inClass: <Behavior> location: <Integer> newSource: <String | Text | Stream | nil> category - the category in which the method will be classified code - the source code to be compiled or evaluated doitFlag - true if this is a doIt (code to evaluate), false if this is a method (code of a method to be compiled) errorMessage - contains information about the syntax error inClass - target class in which to compile the method location - position in the source code where the syntax error occured newSource - eventually hold a source code replacement typically passed by the SyntaxError window

### Methods
#### SyntaxErrorNotification>>#errorClass

<details>
	<summary>See more</summary>
	
	errorClass
	^inClass
</details>

#### SyntaxErrorNotification>>#setClass: aClass category: aCategory code: codeString doitFlag: aBoolean errorMessage: errorString location: anInteger

<details>
	<summary>See more</summary>
	
	setClass: aClass category: aCategory code: codeString doitFlag: aBoolean errorMessage: errorString location: anInteger
	inClass := aClass.
	category := aCategory.
	code := codeString.
	doitFlag := aBoolean.
	errorMessage := errorString.
	location := anInteger
</details>

#### SyntaxErrorNotification>>#messageText

Return an exception's message text.


<details>
	<summary>See more</summary>
	
	messageText
	^ super messageText
		ifNil: [messageText := code]
</details>

#### SyntaxErrorNotification>>#setClass: aClass category: aCategory code: codeString doitFlag: aBoolean

<details>
	<summary>See more</summary>
	
	setClass: aClass category: aCategory code: codeString doitFlag: aBoolean
	inClass := aClass.
	category := aCategory.
	code := codeString.
	doitFlag := aBoolean 
</details>

#### SyntaxErrorNotification>>#doitFlag

<details>
	<summary>See more</summary>
	
	doitFlag
	^doitFlag
</details>

#### SyntaxErrorNotification>>#location

<details>
	<summary>See more</summary>
	
	location
	^location
</details>

#### SyntaxErrorNotification>>#errorCode

<details>
	<summary>See more</summary>
	
	errorCode
	^code
</details>

#### SyntaxErrorNotification>>#errorMessage

<details>
	<summary>See more</summary>
	
	errorMessage
	^errorMessage
</details>

#### SyntaxErrorNotification>>#category

<details>
	<summary>See more</summary>
	
	category
	^category
</details>

## UndeclaredVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UndeclaredVariable>>#addAlternativesTo: labels actions: actions icons: icons

<details>
	<summary>See more</summary>
	
	addAlternativesTo: labels actions: actions icons: icons
	
	| alternatives |
	
	alternatives _ parser possibleVariablesFor: name.
	alternatives do: [ :each |
		labels add: each.
		actions add: [ parser substituteVariable: each atInterval: interval ].
		icons add: nil ].

</details>

#### UndeclaredVariable>>#addLocalVariableOptionsTo: labels actions: actions

<details>
	<summary>See more</summary>
	
	addLocalVariableOptionsTo: labels actions: actions

	labels add: 'declare block-local temp'.
	actions add: self declareBlockTempAction.
			
	labels add: 'declare method temp'.
	actions add: self declareMethodTempAction.
			
	parser canDeclareInstanceVariable ifTrue: [
		labels add: 'declare instance'.
		actions add: self declareInstanceVariableAction]
</details>

#### UndeclaredVariable>>#declareBlockTempAction

<details>
	<summary>See more</summary>
	
	declareBlockTempAction
	
	^[ parser declareTemp: name at: #block ]
</details>

#### UndeclaredVariable>>#declareMethodTempAction

<details>
	<summary>See more</summary>
	
	declareMethodTempAction
	
	^[ parser declareTemp: name at: #method ].
</details>

#### UndeclaredVariable>>#addGlobalVariableOptionsTo: labels actions: actions

<details>
	<summary>See more</summary>
	
	addGlobalVariableOptionsTo: labels actions: actions
	
	labels add: 'define new class'.
	actions add: [ parser defineClass: name ].
			
	labels add: 'declare global'.
	actions add: [ parser declareGlobal: name ].
			
	parser canDeclareClassVariable ifTrue: [
		labels add: 'declare class variable'.
		actions add: [ parser declareClassVar: name ]]

</details>

#### UndeclaredVariable>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction
	
	| labels actions lines caption choice icons |
	
	labels _ OrderedCollection new.
	actions _ OrderedCollection new.
	lines _ OrderedCollection new.
	icons _ OrderedCollection new.
	
	self createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines.
	caption _ 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice _ (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	
	self resume: (actions at: choice ifAbsent:[ nil ]).
</details>

#### UndeclaredVariable>>#addCancelTo: labels actions: actions icons: icons

<details>
	<summary>See more</summary>
	
	addCancelTo: labels actions: actions icons: icons
	
	labels add: 'cancel'.
	actions add: nil.
	icons add: #cancelIcon.
	

</details>

#### UndeclaredVariable>>#createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines

<details>
	<summary>See more</summary>
	
	createMenuOptionsAddingTo: labels actions: actions icons: icons lines: lines
	
	self addOptionsTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addAlternativesTo: labels actions: actions icons: icons.
	lines add: labels size.
	self addCancelTo: labels actions: actions icons: icons.
</details>

#### UndeclaredVariable>>#declareInstanceVariableAction

<details>
	<summary>See more</summary>
	
	declareInstanceVariableAction

	^[ parser declareInstVar: name ]
</details>

#### UndeclaredVariable>>#setParser: aParser name: aString range: anInterval

<details>
	<summary>See more</summary>
	
	setParser: aParser name: aString range: anInterval 
	parser := aParser.
	name := aString.
	interval := anInterval
</details>

#### UndeclaredVariable>>#addOptionsTo: labels actions: actions icons: icons

<details>
	<summary>See more</summary>
	
	addOptionsTo: labels actions: actions icons: icons

	name first isLowercase
		ifTrue: [ self addLocalVariableOptionsTo: labels actions: actions ]
		ifFalse: [ self addGlobalVariableOptionsTo: labels actions: actions ].
	labels size timesRepeat: [ icons add: #listAddIcon ].
		

</details>

## UndefinedVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UndefinedVariable>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction

	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\undefined at this point.\Proceed anyway?' withNewLines.
	^ self resume: shouldResume 
</details>

## UnknownSelector

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UnknownSelector>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction 

	| alternatives labels lines caption choice icons |

	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents: [:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	icons := Array new: labels size.
	icons at: 1 put: #acceptIcon.
	icons at: icons size put: #cancelIcon.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withNewLines.
	
	choice := (PopUpMenu labelArray: labels lines: lines icons: icons) startUpWithCaption: caption.
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1 ifAbsent: [ nil ])
	

</details>

## UnusedVariable

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### UnusedVariable>>#defaultAction

No action is taken. The value nil is returned as the value of the message that signaled the exception.


<details>
	<summary>See more</summary>
	
	defaultAction
	
	| shouldResume |
	
	shouldResume := PopUpMenu confirm: name, ' appears to be\unused in this method.\OK to remove it?' withNewLines.
	self resume: shouldResume 
</details>

