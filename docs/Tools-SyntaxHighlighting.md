## SHParserST80

I am a Smalltalk method / expression parser. Rather than creating an Abstract Syntax Tree, I create a sequence of SHRanges (in my 'ranges' instance variable), which represent the tokens within the String I am parsing. I am used by a SHTextStylerST80 to parse method source strings. I am able to parse incomplete / incorrect methods, and so can be used to parse methods that are being edited. My 'source' instance variable should be set to the string to be parsed. My 'classOrMetaClass' instance var must be set to the class or metaClass for the method source so that I can correctly resolve identifiers within the source. If this is nil , I parse the source as an expression (i.e. a doIt expression). My 'workspace' instance variable can be set to a Workspace, so that I can resolve workspace variables. Example 1. ranges := SHParserST80 new classOrMetaClass: Object; source: 'testMethod ^self'; parse; ranges

### Methods
#### SHParserST80>>#isTokenExternalFunctionCallingConvention

<details>
	<summary>See more</summary>
	
	isTokenExternalFunctionCallingConvention
	| descriptorClass |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: nil.
	descriptorClass ifNil: [^false].
	^(descriptorClass callingConventionFor: currentToken) notNil
</details>

#### SHParserST80>>#workspaceNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	workspaceNamesDo: aBlock

	| title |
	title _ '-- workspace variables'.
	workspace ifNotNil: [
		workspace bindingNamesDo: [ :name | aBlock value: name value: title ] ]
</details>

#### SHParserST80>>#scanPast: rangeType

record rangeType for current token . record argument and temp declarations. scan and answer the next token


<details>
	<summary>See more</summary>
	
	scanPast: rangeType 
	"record rangeType for current token .
	record argument and temp declarations.
	scan and answer the next token"
	rangeType == #blockPatternArg ifTrue: [self pushArgument: currentToken].
	rangeType == #blockPatternTempVar ifTrue: [self pushTemporary: currentToken].
	rangeType == #patternArg ifTrue: [self pushArgument: currentToken].
	rangeType == #patternTempVar ifTrue: [self pushTemporary: currentToken].
	^self
		rangeType: rangeType;
		scanNext
</details>

#### SHParserST80>>#isIncompleteBlockArgName: aString

Answer true if aString is the start of the name of a block argument, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteBlockArgName: aString 
	"Answer true if aString is the start of the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#parsePragmaKeyword

<details>
	<summary>See more</summary>
	
	parsePragmaKeyword

	[self isKeyword]
		whileTrue:[
			self scanPast: #pragmaKeyword.
			self isName
				ifTrue:[self scanPast: (self resolvePragmaArgument: currentToken)] 
				ifFalse:[	self parseLiteral: false]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
</details>

#### SHParserST80>>#last3Ranges

<details>
	<summary>See more</summary>
	
	last3Ranges
	| r s lastI |
	s _ ranges size.
	s = 0 ifTrue: [ ^ #(nil nil nil) ].
	r _ ranges last.
	lastI _ r rangeType = #excessCode
		ifTrue: [ s - 1 ]
		ifFalse: [ s].
	^{ 
		(lastI > 2 ifTrue: [ranges at: lastI-2]).
		(lastI > 1 ifTrue: [ranges at: lastI-1]).
		(lastI > 0 ifTrue: [ranges at: lastI])
	}
</details>

#### SHParserST80>>#isMethodTempName: aString

Answer true if aString is the name of a method temporary, false otherwise. Does not check whether aString is also a block temporary or argument


<details>
	<summary>See more</summary>
	
	isMethodTempName: aString 
	"Answer true if aString is the name of a method temporary, false otherwise.
    Does not check whether aString is also a block temporary
    or argument"

	self methodTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#isName

<details>
	<summary>See more</summary>
	
	isName
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last isValidInIdentifiers ]
</details>

#### SHParserST80>>#isIncompleteWorkspaceVarName: aString

Answer true if aString is the start of the name of an workspace variable, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteWorkspaceVarName: aString 
	"Answer true if aString is the  start of the name of an workspace variable, false otherwise"

	self workspaceNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#initializeInstanceVariables

<details>
	<summary>See more</summary>
	
	initializeInstanceVariables
	instanceVariables := classOrMetaClass 
		ifNotNil: [classOrMetaClass allInstVarNames asArray]
		ifNil: [Set new]
</details>

#### SHParserST80>>#parseCharSymbol

<details>
	<summary>See more</summary>
	
	parseCharSymbol
	| s e |
	s := sourcePosition - 1.
	e := sourcePosition.
	self nextChar.
	self scanPast: #symbol start: s end: e
</details>

#### SHParserST80>>#parseBraceArray

<details>
	<summary>See more</summary>
	
	parseBraceArray
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace level: braceDepth.
	braceDepth := braceDepth - 1
</details>

#### SHParserST80>>#reservedNames

<details>
	<summary>See more</summary>
	
	reservedNames

	^Theme current pseudoVariables
</details>

#### SHParserST80>>#parseSymbolString

<details>
	<summary>See more</summary>
	
	parseSymbolString
	| first c last |
	first := sourcePosition.
	self nextChar.
	[
		(c := self currentChar) 
			ifNil: [
				self rangeType: #unfinishedString start: first end: source size.
				self error	": 'unfinished string'"].
		c ~~ $' or: [
			self peekChar == $' 
				ifTrue: [sourcePosition := sourcePosition + 1.true] 
				ifFalse: [false]]
	] whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self scanPast: #stringSymbol start: first - 1 end: last
</details>

#### SHParserST80>>#scanPast: rangeType level: level

first level adds no suffix to the rangeType. Suffix from 1 to 7 added in cycles , ((level-2) mod(7) + 1)


<details>
	<summary>See more</summary>
	
	scanPast: rangeType level: level
	"first level adds no suffix to the rangeType.
	Suffix from 1 to 7 added in cycles , ((level-2) mod(7) + 1)"
	| cycle typePlusCycle |
	
	cycle := level <= 1 
		ifTrue: [0]
		ifFalse:[ ((level - 2) \\ 7) + 1].
	typePlusCycle := cycle = 0 
		ifTrue:[rangeType]
		ifFalse:[(rangeType, cycle asString) asSymbol].
	^self scanPast: typePlusCycle

</details>

#### SHParserST80>>#parseTerm

<details>
	<summary>See more</summary>
	
	parseTerm
	self failWhen: currentToken isNil.
	currentTokenFirst == $( 
		ifTrue: [
			bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			bracketDepth := bracketDepth - 1.
			^self ].
	currentTokenFirst == $[ ifTrue: [^self parseBlock].
	currentTokenFirst == $` ifTrue: [^self parseBacktick].
	currentTokenFirst == ${ 
		ifTrue: [
			braceDepth := braceDepth + 1.
			self scanPast: #leftBrace level: braceDepth.
			self parseBraceArray.
			^self ].
	self isName ifTrue: [^self scanPast: (self resolve: currentToken)].
	self parseLiteral: false
</details>

#### SHParserST80>>#isIdentifier: aSymbol

<details>
	<summary>See more</summary>
	
	isIdentifier: aSymbol

	^ #(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) statePointsTo:aSymbol
</details>

#### SHParserST80>>#parseLiteralArrayElement

<details>
	<summary>See more</summary>
	
	parseLiteralArrayElement
	 currentTokenFirst isValidStartOfIdentifiers ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst == $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true
</details>

#### SHParserST80>>#parseExternalCall

<details>
	<summary>See more</summary>
	
	parseExternalCall
	self scanNext.
	self failWhen: currentToken isNil.
	self scanPast: #externalCallType.
	currentToken = '*' 
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: 	[
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)] 
		whileTrue: [
			self failWhen: currentToken isNil.
			self scanPast: #externalCallType.
			currentToken = '*' 
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:' 
		ifTrue: [
			self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
</details>

#### SHParserST80>>#parseMethodTemporaries

<details>
	<summary>See more</summary>
	
	parseMethodTemporaries
	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #methodTempBar.
			[self isName] 
				whileTrue: [self scanPast: #patternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #methodTempBar]
</details>

#### SHParserST80>>#parseBinary

<details>
	<summary>See more</summary>
	
	parseBinary 
	| binary type |
	self parseUnary.
	[ self isBinary ]
		whileTrue: [
			binary _ currentToken.
			type _ #binary.
			(binary isEmpty or: [ Symbol hasInternedAndImplemented: binary ])
				ifFalse: [
					type _ (Symbol thatStartsCaseSensitive: binary)
						ifNil: [ #undefinedBinary]
						ifNotNil: [ #incompleteBinary]].	
			self scanPast: type. 	
			self parseTerm.
            	self parseUnary ]
</details>

#### SHParserST80>>#resolvePartialPragmaArgument: aString

check if any valid pragma argument begins with aString


<details>
	<summary>See more</summary>
	
	resolvePartialPragmaArgument: aString 
	"check if any valid pragma argument begins with aString"
	
	(#('true' 'false' 'nil') anySatisfy: [:each | each beginsWith: aString]) 
		ifTrue: [^#incompleteIdentifier].
	"should really check that a matching binding is for a Class?"
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [:c | 
				(Smalltalk hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]]]
		ifFalse: [(Smalltalk hasBindingThatBeginsWith: aString) ifTrue: [^#incompleteIdentifier]].
	^#undefinedIdentifier
</details>

#### SHParserST80>>#poolConstantNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	poolConstantNamesDo: aBlock

	| title |
	title _ '-- pool variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [ :pool |
					pool bindingsDo: [ :assoc | aBlock value: assoc key value: title ]]]]
</details>

#### SHParserST80>>#nextChar

<details>
	<summary>See more</summary>
	
	nextChar
	sourcePosition := sourcePosition + 1.
	^source at: sourcePosition ifAbsent: [$ ]
</details>

#### SHParserST80>>#resolve: aString

<details>
	<summary>See more</summary>
	
	resolve: aString

	self reservedNames do: [ :symbol | aString = symbol ifTrue: [^symbol]].
	(self isBlockTempName: aString) ifTrue: [^#blockTempVar].
	(self isBlockArgName: aString) ifTrue: [^#blockArg].
	(self isMethodTempName: aString) ifTrue: [^#tempVar].
	(self isMethodArgName: aString) ifTrue: [^#methodArg].
	(self isInstVarName: aString) ifTrue: [^#instVar].
	(self isWorkspaceVarName: aString) ifTrue: [^#workspaceVar].
	Symbol hasInterned: aString ifTrue: [ :symbol |
		(self isClassVarName: symbol) ifTrue: [ ^#classVar ].
		(self isPoolConstantName: symbol) ifTrue: [ ^#poolConstant].
		(self isGlobal: symbol) ifTrue: [^#globalVar]].
	^self resolvePartial: aString
</details>

#### SHParserST80>>#enterBlock

<details>
	<summary>See more</summary>
	
	enterBlock
	blockDepth := blockDepth + 1.
	bracketDepth := bracketDepth + 1
</details>

#### SHParserST80>>#parseBlockTemporaries

<details>
	<summary>See more</summary>
	
	parseBlockTemporaries
	currentTokenFirst == $| 
		ifTrue: [
			self scanPast: #blockTempBar.
			[self isName] 
				whileTrue: [self scanPast: #blockPatternTempVar].
			self failUnless: currentToken = '|'.
			self scanPast: #blockTempBar]
</details>

#### SHParserST80>>#isPartialOrFullIdentifier: aSymbol

<details>
	<summary>See more</summary>
	
	isPartialOrFullIdentifier: aSymbol

	^(self isIdentifier: aSymbol) or: [ self isReservedName: aSymbol ]
</details>

#### SHParserST80>>#parseString

<details>
	<summary>See more</summary>
	
	parseString
	| first c last |
	first _ sourcePosition.
	[
		(c _ self currentChar) ifNil: [
			self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error": 'unfinished string'" ].
		c ~~ $' or: [
			self peekChar == $' and: [
				sourcePosition _ sourcePosition + 1.
				true ]]] whileTrue: [ sourcePosition _ sourcePosition + 1 ].
	last _ sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last
</details>

#### SHParserST80>>#isIncompleteClassVarName: aString

Answer true if aString is the start of the name of a class variable, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteClassVarName: aString 
	"Answer true if aString is the start of the name of a class variable, false otherwise"

	self classVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#namesBeginningWith: aString do: aBlock

Evaluate aBlock for all available names that start with aString


<details>
	<summary>See more</summary>
	
	namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name :kindOfIdentifierTitle |
		(name beginsWith: aString) ifTrue: [ aBlock value: name value: kindOfIdentifierTitle ]].
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- classes' ] in: Smalltalk classNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- globals' ] in: Smalltalk nonClassNames
</details>

#### SHParserST80>>#isPoolConstantName: aSymbol

Answer true if aString is the name of a pool constant, false otherwise


<details>
	<summary>See more</summary>
	
	isPoolConstantName: aSymbol 
	"Answer true if aString is the name of a pool constant, false otherwise"

	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [:p | (p bindingOf: aSymbol) ifNotNil: [ ^true ]]]].
	^false
</details>

#### SHParserST80>>#source: aString

<details>
	<summary>See more</summary>
	
	source: aString
    source := aString
</details>

#### SHParserST80>>#isIncompletePoolConstantName: aString

Answer true if aString is the start of the name of a pool constant, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompletePoolConstantName: aString 
	"Answer true if aString is the start of the name of a pool constant, false otherwise"

	self poolConstantNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#failUnless: aBoolean

<details>
	<summary>See more</summary>
	
	failUnless: aBoolean
	aBoolean ifFalse:[self error]

</details>

#### SHParserST80>>#parseSymbolSelector

<details>
	<summary>See more</summary>
	
	parseSymbolSelector
	| start end |
	start := sourcePosition - 1.
	end := sourcePosition.
	[self isBinarySelectorCharacter: self nextChar] 
		whileTrue: [end := sourcePosition].
	self scanPast: #symbol start: start end: end
</details>

#### SHParserST80>>#rangesWithoutExcessCode

<details>
	<summary>See more</summary>
	
	rangesWithoutExcessCode
	
	^ranges 
		ifEmpty: [ ranges ]
		ifNotEmpty: [ 
			ranges last rangeType = #excessCode
				ifTrue: [ ranges allButLast ]
				ifFalse: [ ranges ]]
</details>

#### SHParserST80>>#isBlockTempName: aString

Answer true if aString is the name of a block temporary. false otherwise


<details>
	<summary>See more</summary>
	
	isBlockTempName: aString 
	"Answer true if aString is the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#scanNext

<details>
	<summary>See more</summary>
	
	scanNext
	self scanWhitespace.
	currentTokenFirst _ self currentChar.
	currentTokenFirst 
		ifNil: [" end of input "
			currentTokenFirst _ $ .
			currentTokenSourcePosition _ nil.
			currentToken _ nil.
			^nil].
	currentTokenFirst isDigit ifTrue: [^self scanNumber].
	currentTokenFirst isValidStartOfIdentifiers ifTrue: [^self scanIdentifier].
	^self scanBinary
</details>

#### SHParserST80>>#rangeType: aSymbol

<details>
	<summary>See more</summary>
	
	rangeType: aSymbol 
	^self 
		rangeType: aSymbol
		start: currentTokenSourcePosition
		end: currentTokenSourcePosition + currentToken size - 1
</details>

#### SHParserST80>>#blockDepthsStartIndexes

<details>
	<summary>See more</summary>
	
	blockDepthsStartIndexes
	^blockDepthsStartIndexes
</details>

#### SHParserST80>>#parseUnary

<details>
	<summary>See more</summary>
	
	parseUnary
	| unary type |
	[ self isName ]
		whileTrue: [
			unary _ currentToken.
			type _ #unary.
			(unary isEmpty or: [ Symbol hasInternedAndImplemented: unary ])
				ifFalse:[
					type _ (Symbol thatStartsCaseSensitive: unary)
						ifNil: [ #undefinedUnary]
						ifNotNil: [ #incompleteUnary ]].
			self scanPast: type ]
</details>

#### SHParserST80>>#isBinary

<details>
	<summary>See more</summary>
	
	isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	"Special case: '::' is not a binary selector but the Chain operator"
	(sourcePosition - currentTokenSourcePosition = 1 and: [
		(source at: currentTokenSourcePosition ifAbsent: nil) = $: and: [		
			(source at: sourcePosition ifAbsent: nil) = $: ]])
				ifTrue: [^ false ].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		((self isBinarySelectorCharacter: c) or: [c == $:])
			ifFalse: [^false]].
	^true
</details>

#### SHParserST80>>#parseBlock

Just read $[


<details>
	<summary>See more</summary>
	
	parseBlock

	"Just read $["
	blockDepths add: blockDepth+1.
	blockDepthsStartIndexes add: sourcePosition-1.

	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $: ifTrue: [self parseBlockArguments].
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].

	"Just read $]"
	blockDepths add: blockDepth-1.
	blockDepthsStartIndexes add: sourcePosition.

	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock
</details>

#### SHParserST80>>#methodTempNamesDo: aBlock

Iterate over method temporary names


<details>
	<summary>See more</summary>
	
	methodTempNamesDo: aBlock
	"Iterate over method temporary names"

	| title |
	title _ '-- method variables'.
	^temporaries at: 0 ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]
</details>

#### SHParserST80>>#isClassVarName: aSymbol

Answer true if aString is the name of a class variable, false otherwise


<details>
	<summary>See more</summary>
	
	isClassVarName: aSymbol 
	"Answer true if aString is the name of a class variable, false otherwise"

	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c | 
				(c classPool bindingOf: aSymbol) ifNotNil: [^true]]].
	^false
</details>

#### SHParserST80>>#parseUnaryMessagePattern

<details>
	<summary>See more</summary>
	
	parseUnaryMessagePattern
	
	 self scanPast: #patternUnary

</details>

#### SHParserST80>>#scanBinary

<details>
	<summary>See more</summary>
	
	scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := c asString.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , d asString.
			self nextChar.
			^currentToken].
	(c == $| and: [d == $|])
		ifTrue: ["|| empty temp declaration"
			^currentToken].
	c _ d.
	[
		d _ self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken
</details>

#### SHParserST80>>#scanComment

<details>
	<summary>See more</summary>
	
	scanComment
	| c s e |
	s := sourcePosition.
	
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar) 
		ifNil: [
			self rangeType: #unfinishedComment start: s end: source size.
			^self error	": 'unfinished comment'"].
	c == $"] 
		whileFalse: [].
	e := sourcePosition.
	s < e ifTrue: [self rangeType: #comment start: s end: e].
	self nextChar.
	self scanWhitespace
</details>

#### SHParserST80>>#isBinarySelectorCharacter: aCharacter

<details>
	<summary>See more</summary>
	
	isBinarySelectorCharacter: aCharacter
	aCharacter = $: ifTrue: [^ false].
	^aCharacter isValidInBinarySelectors
</details>

#### SHParserST80>>#blockDepths

<details>
	<summary>See more</summary>
	
	blockDepths
	^blockDepths
</details>

#### SHParserST80>>#scanWhitespace

<details>
	<summary>See more</summary>
	
	scanWhitespace
	| c |
	
	[c := self currentChar.
	c notNil and: [c isSeparator]] 
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $" ifTrue: [self scanComment]
</details>

#### SHParserST80>>#isBlockArgName: aString

Answer true if aString is the name of a block argument, false otherwise


<details>
	<summary>See more</summary>
	
	isBlockArgName: aString 
	"Answer true if aString is the name of a block argument, false otherwise"

	self blockArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#nonGlobalNamesDo: aBlock

Evaluate aBlock over all available names, except for globals


<details>
	<summary>See more</summary>
	
	nonGlobalNamesDo: aBlock
	"Evaluate aBlock over all available names, except for globals"

	self
		blockArgNamesDo: aBlock;
		blockTempNamesDo: aBlock;
		methodArgNamesDo: aBlock;
		methodTempNamesDo: aBlock;
		instVarNamesDo: aBlock;
		classVarNamesDo: aBlock;
		poolConstantNamesDo: aBlock;
		workspaceNamesDo: aBlock.
	self
		reservedNames do: [ :name |
			aBlock value: name value: '-- pseudovariables'  ]
</details>

#### SHParserST80>>#classOrMetaClass

<details>
	<summary>See more</summary>
	
	classOrMetaClass
	
	^classOrMetaClass
</details>

#### SHParserST80>>#resolvePartial: aString

check if any identifier begins with aString


<details>
	<summary>See more</summary>
	
	resolvePartial: aString 
	"check if any identifier begins with aString"
	
	(self isIncompleteReservedName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteBlockTempName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteBlockArgName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteMethodTempName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteMethodArgName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteInstVarName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteWorkspaceVarName: aString) ifTrue: [^#incompleteIdentifier].
	(self isIncompleteClassVarName: aString) ifTrue: [ ^#incompleteIdentifier ].
	(self isIncompletePoolConstantName: aString) ifTrue: [ ^#incompleteIdentifier ].
	(self isIncompleteGlobal: aString) ifTrue: [^#incompleteIdentifier].
	^#undefinedIdentifier
</details>

#### SHParserST80>>#pushTemporary: aString

<details>
	<summary>See more</summary>
	
	pushTemporary: aString 
	(temporaries at: blockDepth ifAbsentPut: [OrderedCollection new: 10]) 
		add: aString
</details>

#### SHParserST80>>#scanNumber

<details>
	<summary>See more</summary>
	
	scanNumber
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	('rx' includes: c)
		ifTrue: [
			base := Integer readFrom: (ReadStream on: (source copyFrom: start to: sourcePosition - 1)).
			self peekChar == $- ifTrue:[self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $. 
				ifTrue: [
					(self isBigDigit: self nextChar base: base) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipBigDigits: base]].
			c := self currentChar.
			('deqp'includes: c)
				ifTrue: [
					((nc := self nextChar) isDigit or: [nc == $- and:[self peekChar isDigit]]) 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			c == $s 
				ifTrue: [
					self nextChar isDigit 
						ifFalse: [sourcePosition := sourcePosition - 1]
						ifTrue: [self skipDigits]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits.].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^currentTokenSourcePosition := start].
	c == $. 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [
					sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^currentTokenSourcePosition := start]
				ifTrue: [self skipDigits]].
	c := self currentChar.
	('deqp' includes: c)
		ifTrue: [
			((nc := self nextChar) isDigit or: [nc == $-  and:[self peekChar isDigit]]) 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	c == $s 
		ifTrue: [
			self nextChar isDigit 
				ifFalse: [sourcePosition := sourcePosition - 1]
				ifTrue: [self skipDigits]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^currentTokenSourcePosition := start
</details>

#### SHParserST80>>#isMessage: aSymbol

<details>
	<summary>See more</summary>
	
	isMessage: aSymbol

	^#(binary incompleteBinary keyword incompleteKeyword unary incompleteUnary) statePointsTo: aSymbol
</details>

#### SHParserST80>>#isGlobal: aSymbol

Answer true if aString is the name of a global variable, false otherwise


<details>
	<summary>See more</summary>
	
	isGlobal: aSymbol
	"Answer true if aString is the name of a global variable, false otherwise"

	(Smalltalk bindingOf: aSymbol) ifNotNil: [^true].
	^false
</details>

#### SHParserST80>>#isAnsiAssignment

<details>
	<summary>See more</summary>
	
	isAnsiAssignment
	^currentToken = ':='
</details>

#### SHParserST80>>#leaveBlock

<details>
	<summary>See more</summary>
	
	leaveBlock
	arguments removeKey: blockDepth ifAbsent: nil.
	temporaries removeKey: blockDepth ifAbsent: nil.
	blockDepth := blockDepth - 1.
	bracketDepth := bracketDepth - 1
</details>

#### SHParserST80>>#parseCascadeAndChain

<details>
	<summary>See more</summary>
	
	parseCascadeAndChain

	self parseKeyword.
	currentTokenFirst == $; ifTrue:
		[
			self scanPast: #cascadeSeparator.
			^self parseCascadeAndChain
		].
	currentTokenFirst = $: ifTrue:
		[
			self scanPast: #chainSeparator.

			"These lines implement double colon chains"
			currentTokenFirst = $: ifFalse: [^self].
			self scanPast: #chainSeparator.
			
			^self parseCascadeAndChain
		]
</details>

#### SHParserST80>>#parseStatementListForBraceArray

same as parseStatementList, but does not allow empty statements e.g {...$a...}. A single terminating . IS allowed e.g. {$a.}


<details>
	<summary>See more</summary>
	
	parseStatementListForBraceArray
	"same as parseStatementList, but does not allow empty statements e.g {...$a...}.
	A single terminating . IS allowed e.g. {$a.} "

	
	[currentTokenFirst ~~ $} ifTrue: [self parseStatement].
	currentTokenFirst == $.] 
		whileTrue: [self scanPast: #statementSeparator]
</details>

#### SHParserST80>>#allSource: aSourceCode

<details>
	<summary>See more</summary>
	
	allSource: aSourceCode

	allSource _ aSourceCode
</details>

#### SHParserST80>>#lastRange

<details>
	<summary>See more</summary>
	
	lastRange
	| r i s |
	s _ ranges size.
	s = 0 ifTrue: [ ^nil ].
	r _ ranges last.
	^r rangeType = #excessCode
		ifFalse: [ r ]
		ifTrue: [
			i _ s-1.
			i > 0 ifTrue: [ ranges at: i ]]
</details>

#### SHParserST80>>#resolvePragmaArgument: aString

<details>
	<summary>See more</summary>
	
	resolvePragmaArgument: aString 
	(#('true' 'false' 'nil') includes: aString) ifTrue: [^aString asSymbol].
	"should really check that global is a class?"
	Symbol hasInterned: aString ifTrue: [:sym | 
		classOrMetaClass isBehavior 
			ifTrue: [
				classOrMetaClass theNonMetaClass withAllSuperclasses do: [:c | 
					(Smalltalk bindingOf: sym) ifNotNil: [^#globalVar]]]
			ifFalse: [(Smalltalk bindingOf: sym) ifNotNil: [^#globalVar]]].
	^self resolvePartialPragmaArgument: aString
</details>

#### SHParserST80>>#methodArgNamesDo: aBlock

Iterate over method argument names


<details>
	<summary>See more</summary>
	
	methodArgNamesDo: aBlock
	"Iterate over method argument names"

	| title |
	title _ '-- method arguments'.
	^arguments at: 0  ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]
</details>

#### SHParserST80>>#isIncompleteInstVarName: aString

Answer true if aString is the start of the name of an instance variable, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteInstVarName: aString 
	"Answer true if aString is the start of the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#parseSymbol

<details>
	<summary>See more</summary>
	
	parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c _ self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [
		self parseSymbolString.
		^self ].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol
</details>

#### SHParserST80>>#blockTempNamesDo: aBlock

Iterate over block temp names valid at current blockDepth


<details>
	<summary>See more</summary>
	
	blockTempNamesDo: aBlock
	"Iterate over block temp names valid at current blockDepth"

	| title |
	title _ '-- block variables'.
	blockDepth to: 1 by: -1 do: [ :level |
		temporaries at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]
</details>

#### SHParserST80>>#parseLiteral: inArray

<details>
	<summary>See more</summary>
	
	parseLiteral: inArray 
	currentTokenFirst == $$ 
		ifTrue: [
			| pos |
			self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			self scanPast: #character start: pos end: pos.
			^self ].
	currentTokenFirst isDigit 
		ifTrue: [
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentToken = '-' 
		ifTrue: [
			| c |
			c := self currentChar.
			(inArray and: [c isNil or: [c isDigit not]]) 
				ifTrue: [
					"single - can be a symbol in an Array"
					self scanPast: #symbol.
					^self ].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			"token isNil ifTrue: [self error: 'Unexpected End Of Input']."
			"do not parse the number, can be time consuming"
			self scanPast: #number.
			^self ].
	currentTokenFirst == $' ifTrue: [
		self parseString.
		^self ].
	currentTokenFirst == $# ifTrue: [
		self parseSymbol.
		^self ].
	(inArray and: [currentToken notNil]) ifTrue: [
		self scanPast: #symbol.
		^self ].
	self failWhen: currentTokenFirst == $. .
	self error	": 'argument missing'"
</details>

#### SHParserST80>>#isIncompleteMethodTempName: aString

Answer true if aString is the start of the name of a method temporary, false otherwise.


<details>
	<summary>See more</summary>
	
	isIncompleteMethodTempName: aString 
	"Answer true if aString is the start of the name of a method temporary, false otherwise."

	self methodTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#parseStatementList

<details>
	<summary>See more</summary>
	
	parseStatementList
	
	[[currentTokenFirst == $.] whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil and: [currentTokenFirst ~~ $]]) 
		ifTrue: [self parseStatement].
	currentTokenFirst == $.] 
			whileTrue: [self scanPast: #statementSeparator]
</details>

#### SHParserST80>>#isInstVarName: aString

Answer true if aString is the name of an instance variable, false otherwise


<details>
	<summary>See more</summary>
	
	isInstVarName: aString 
	"Answer true if aString is the name of an instance variable, false otherwise"

	self instVarNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#failWhen: aBoolean

<details>
	<summary>See more</summary>
	
	failWhen: aBoolean
	aBoolean ifTrue:[self error]
</details>

#### SHParserST80>>#skipBigDigits: baseInteger

<details>
	<summary>See more</summary>
	
	skipBigDigits: baseInteger
	[self isBigDigit: self nextChar base: baseInteger] 
		whileTrue: []

</details>

#### SHParserST80>>#parseMessagePattern

<details>
	<summary>See more</summary>
	
	parseMessagePattern   

	self isName 
		ifTrue: [self parseUnaryMessagePattern]
		ifFalse: [
			self isBinary
				ifTrue:[self parseBinaryMessagePattern]
				ifFalse:[
					self failUnless: self isKeyword.
					self parseKeywordMessagePattern]]
</details>

#### SHParserST80>>#skipDigits

<details>
	<summary>See more</summary>
	
	skipDigits
	[self nextChar isDigit] 
		whileTrue: []
</details>

#### SHParserST80>>#workspace: aWorkspace

<details>
	<summary>See more</summary>
	
	workspace: aWorkspace
    workspace := aWorkspace
</details>

#### SHParserST80>>#parsePragmaBinary

<details>
	<summary>See more</summary>
	
	parsePragmaBinary

	self scanPast: #pragmaBinary.
	self isName
		ifTrue:[self scanPast: (self resolvePragmaArgument: currentToken)] 
		ifFalse:[	self parseLiteral: false].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
</details>

#### SHParserST80>>#ranges

<details>
	<summary>See more</summary>
	
	ranges
	^ ranges
</details>

#### SHParserST80>>#source

<details>
	<summary>See more</summary>
	
	source
	^source
</details>

#### SHParserST80>>#parse

Parse the receiver's text as a Smalltalk method


<details>
	<summary>See more</summary>
	
	parse
    "Parse the receiver's text as a Smalltalk method"

    self parse: (classOrMetaClass notNil)
</details>

#### SHParserST80>>#classVarNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	classVarNamesDo: aBlock

	| title |
	title _ '-- class variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c classPool keysDo: [ :name | aBlock value: name value: title ] ]]
</details>

#### SHParserST80>>#peekChar

<details>
	<summary>See more</summary>
	
	peekChar
	^source at: sourcePosition + 1 ifAbsent: [ $  ] "asking #value to a char gives ascii..."
</details>

#### SHParserST80>>#parseExpression

<details>
	<summary>See more</summary>
	
	parseExpression
	| assignType |
	self isName 
		ifTrue: [
			self scanPast: (self resolve: currentToken).
			self isAssignment 
				ifTrue: [
					assignType := self isAnsiAssignment 
						ifTrue: [#ansiAssignment]
						ifFalse: [#assignment].
					self scanPast: assignType.
					self parseExpression]
				ifFalse: [self parseCascadeAndChain]]
		ifFalse: [
			self parseTerm.
			self parseCascadeAndChain]
</details>

#### SHParserST80>>#isBigDigit: aCharacter base: anInteger

Answer true if aCharacter is a digit or a capital letter appropriate for base anInteger


<details>
	<summary>See more</summary>
	
	isBigDigit: aCharacter base: anInteger
    "Answer true if aCharacter is a digit or a capital
    letter appropriate for base anInteger"
	| digitValue |
	
	digitValue := aCharacter digitValue.
	anInteger = 1 ifTrue: [
		^digitValue = 1 ].
	^digitValue >= 0 and:[digitValue < anInteger]
</details>

#### SHParserST80>>#scanIdentifier

<details>
	<summary>See more</summary>
	
	scanIdentifier
	| c start |
	start _ sourcePosition.
	[ (c _ self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c == $: and: [(self isBinarySelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken _ source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition _ start
</details>

#### SHParserST80>>#isIncompleteMethodArgName: aString

Answer true if aString is the start of the name of a method argument, false otherwise. Does not check whether aString is also a blockArgName


<details>
	<summary>See more</summary>
	
	isIncompleteMethodArgName: aString 
	"Answer true if aString is the start of the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#isMethodArgName: aString

Answer true if aString is the name of a method argument, false otherwise. Does not check whether aString is also a blockArgName


<details>
	<summary>See more</summary>
	
	isMethodArgName: aString 
	"Answer true if aString is the name of a method argument, false otherwise.
    Does not check whether aString is also a blockArgName"

	self methodArgNamesDo: [ :arg :dummy | arg = aString ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#isKeyword

<details>
	<summary>See more</summary>
	
	isKeyword
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last == $: ]
</details>

#### SHParserST80>>#blockArgNamesDo: aBlock

Iterate over block argument names valid at current blockDepth


<details>
	<summary>See more</summary>
	
	blockArgNamesDo: aBlock
	"Iterate over block argument names valid at current blockDepth"

	| title |
	title _ '-- block arguments'.
	blockDepth to: 1 by: -1 do: [ :level |
		arguments at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]
</details>

#### SHParserST80>>#parseStatement

<details>
	<summary>See more</summary>
	
	parseStatement
	currentTokenFirst == $^ ifTrue: [self scanPast: #return].
	self parseExpression
</details>

#### SHParserST80>>#isReservedName: aSymbol

<details>
	<summary>See more</summary>
	
	isReservedName: aSymbol

	^ self reservedNames statePointsTo: aSymbol
</details>

#### SHParserST80>>#parseBacktick

<details>
	<summary>See more</summary>
	
	parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock
</details>

#### SHParserST80>>#isAssignment

<details>
	<summary>See more</summary>
	
	isAssignment
	^currentToken = ':=' or: [ currentToken = '_' ]
</details>

#### SHParserST80>>#parseBinaryMessagePattern

<details>
	<summary>See more</summary>
	
	parseBinaryMessagePattern   

   	self scanPast:  #patternBinary. 
	self failUnless: self isName.
	self scanPast: #patternArg.


</details>

#### SHParserST80>>#currentChar

<details>
	<summary>See more</summary>
	
	currentChar
	^source at: sourcePosition ifAbsent: nil
</details>

#### SHParserST80>>#isIncompleteReservedName: aString

Answer true if aString is the start of a reserved name, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteReservedName: aString 
	"Answer true if aString is the start of a reserved name, false otherwise"

	self reservedNames do: [ :arg | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#parsePragmaSequence

<details>
	<summary>See more</summary>
	
	parsePragmaSequence
	[currentToken = '<' ]
		whileTrue:[
			self scanPast: #primitiveOrExternalCallStart.
			currentToken = 'primitive:' 
				ifTrue: [
					self rangeType: #primitive.
					self parsePrimitive]
				ifFalse:[
					self isTokenExternalFunctionCallingConvention 
						ifTrue: [
							self rangeType: #externalFunctionCallingConvention.
							self parseExternalCall]
						ifFalse:[
							self isName
								ifTrue:[
									self scanPast: #pragmaUnary.
									self failUnless: currentToken = '>'.
									self scanPast: #primitiveOrExternalCallEnd]
								ifFalse:[
									self isKeyword
										ifTrue:[
											self parsePragmaKeyword]
										ifFalse:[
											self isBinary
												ifTrue:[self parsePragmaBinary]
												ifFalse:[	self error	": 'Invalid External Function Calling convention'" ]]]]]]
</details>

#### SHParserST80>>#parseKeyword

<details>
	<summary>See more</summary>
	
	parseKeyword 
    | keyword rangeIndices |
	self parseBinary.
	keyword _ ''.
	rangeIndices _ #().
	[
    		[ self isKeyword ]
        		whileTrue: [
				keyword _ keyword, currentToken. 
				self rangeType: #keyword.
				"remember where this keyword token is in ranges"
				rangeIndices _ rangeIndices copyWith: ranges size.
				self scanNext.
				self parseTerm.
				self parseBinary ]
	] ensure: [ | type |
		"do this in an ensure so that it happens even if the errorBlock evaluates before getting here"
		"patch up the keyword tokens, so that incomplete and undefined ones look different"
		(keyword isEmpty or: [ Symbol hasInternedAndImplemented: keyword ])
			ifFalse: [
				type _ (Symbol thatStartsCaseSensitive: keyword)
					ifNil: [ #undefinedKeyword]
					ifNotNil: [ #incompleteKeyword ].
				rangeIndices do: [ :i | (ranges at: i) type: type ]]]
</details>

#### SHParserST80>>#isIncompleteGlobal: aString

Answer true if aString is the start of the name of a global variable, false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteGlobal: aString
	"Answer true if aString is the start of the name of a global variable, false otherwise"

	^Smalltalk hasBindingThatBeginsWith: aString
</details>

#### SHParserST80>>#namesBeginningWith: aString do: aBlock in: aCollection

aCollection is sorted


<details>
	<summary>See more</summary>
	
	namesBeginningWith: aString do: aBlock in: aCollection
	"aCollection is sorted"
	"
	self new namesBeginningWith: 'O' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorer' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorerWrapper' do: [ :each | each print ] in: Smalltalk classNames
	"
	| count |

	"Find the first element  starting with aString"
	count _ aCollection size.
	aCollection
		findBinaryIndex: [ :element |
			element < aString
				ifFalse: [ -1 ]
				ifTrue: [ 1 ] ]
		do: [ :found | "Will never find any" ]
		ifNone: [ :a :b | | i n |
			i _ b.
			[ i <= count and: [ 
				n _ aCollection at: i.
				aString isEmpty or: [
					n beginsWith: aString ]]] whileTrue: [
				aBlock value: n.
				i _ i + 1 ]]
</details>

#### SHParserST80>>#classOrMetaClass: aClass

<details>
	<summary>See more</summary>
	
	classOrMetaClass: aClass
    classOrMetaClass := aClass
</details>

#### SHParserST80>>#parse: isAMethod

Parse the receiver's text. If isAMethod is true then treat text as a method, if false as an expression with no message pattern


<details>
	<summary>See more</summary>
	
	parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	| continue prevSourcePosition |
	self initializeInstanceVariables.
	sourcePosition _ 1.
	arguments _ Dictionary new.
	temporaries _ Dictionary new.
	blockDepth _ bracketDepth _ braceDepth _ 0.
	blockDepths _ OrderedCollection with: blockDepth.
	blockDepthsStartIndexes _ OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new: 100] ifNotNil: [ ranges reset].
	errorBlock _ [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		"Iterate once for methods, but pontentially several times for workspaces
		(to recover after errors, for possible good next lines or chunks)"
		continue _ true.
		[ continue ] whileTrue: [
			prevSourcePosition _ sourcePosition.
			self parseStatementList.
			continue _ sourcePosition > prevSourcePosition.
			isAMethod
				ifTrue: [
					"Only if we are parsing a method, consider everything after this point as error."
					currentToken ifNotNil: [ self error ].
					continue _ false]
				ifFalse: [
					sourcePosition > source size ifTrue: [continue _ false]]].
	] ensure: [errorBlock _ nil].
	^true
</details>

#### SHParserST80>>#parseArray

<details>
	<summary>See more</summary>
	
	parseArray
	[currentTokenFirst == $)] whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd
</details>

#### SHParserST80>>#parsePrimitive

<details>
	<summary>See more</summary>
	
	parsePrimitive
	self scanNext.
	currentTokenFirst isDigit 
		ifTrue: [self scanPast: #integer]
		ifFalse: [
			self failUnless: currentTokenFirst == $'.
			self parseString.
			currentToken = 'module:' 
				ifTrue: [
					self scanPast: #module.
					self failUnless: currentTokenFirst == $'.
					self parseString]].
	currentToken = 'error:' ifTrue: [
		self scanPast: #primitive. "there's no rangeType for error"
		self isName
			ifTrue: [ self scanPast: #patternTempVar ]
			ifFalse: [ self parseStringOrSymbol ] ].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
</details>

#### SHParserST80>>#isIncompleteBlockTempName: aString

Answer true if aString is the start of the name of a block temporary. false otherwise


<details>
	<summary>See more</summary>
	
	isIncompleteBlockTempName: aString 
	"Answer true if aString is the start of the name of a block temporary. false otherwise"

	self blockTempNamesDo: [ :arg :dummy | (arg beginsWith: aString) ifTrue: [ ^true ]].
	^false
</details>

#### SHParserST80>>#error

<details>
	<summary>See more</summary>
	
	error
	self 
		rangeType: #excessCode
		start: (ranges isEmpty ifTrue: [1] ifFalse: [ranges last end + 1])
		end: source size.
	errorBlock value
</details>

#### SHParserST80>>#allSource

<details>
	<summary>See more</summary>
	
	allSource

	^allSource
</details>

#### SHParserST80>>#parseKeywordMessagePattern

<details>
	<summary>See more</summary>
	
	parseKeywordMessagePattern   

	[self isKeyword]
		whileTrue: [ 
			self scanPast:  #patternKeyword. 
			self failUnless: self isName.
			self scanPast: #patternArg]


</details>

#### SHParserST80>>#isWorkspaceVarName: aString

Answer true if aString is the name of an workspace variable, false otherwise


<details>
	<summary>See more</summary>
	
	isWorkspaceVarName: aString 
	"Answer true if aString is the name of an workspace variable, false otherwise"
	workspace
		ifNotNil: [ ^(workspace hasBindingOf: aString) ].
	^false
</details>

#### SHParserST80>>#pushArgument: aString

<details>
	<summary>See more</summary>
	
	pushArgument: aString 
	(arguments at: blockDepth ifAbsentPut: [OrderedCollection new: 10]) 
		add: aString
</details>

#### SHParserST80>>#parseSymbolIdentifier

<details>
	<summary>See more</summary>
	
	parseSymbolIdentifier
	| c start end |
	c _ self currentChar.
	self failUnless: ( c isValidStartOfIdentifiers or: [ c == $: ]).
	start _ sourcePosition.
	[
		c _ self nextChar.
		c isValidInIdentifiers or: [ c == $: ]
	] whileTrue: [].
	end _ sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end
</details>

#### SHParserST80>>#scanPast: rangeType start: startInteger end: endInteger

record rangeType for current token from startInteger to endInteger, and scanNext token


<details>
	<summary>See more</summary>
	
	scanPast: rangeType start: startInteger end: endInteger
	"record rangeType for current token from startInteger to endInteger,
	 and scanNext token"

	^self 
		rangeType: rangeType start: startInteger end: endInteger;
		scanNext
	

</details>

#### SHParserST80>>#rangeType: aSymbol start: s end: e

<details>
	<summary>See more</summary>
	
	rangeType: aSymbol start: s end: e 
	^ranges add: (SHRange start: s end: e type: aSymbol)
</details>

#### SHParserST80>>#parseStringOrSymbol

<details>
	<summary>See more</summary>
	
	parseStringOrSymbol

	currentTokenFirst == $' ifTrue: [ ^self parseString ].
	currentTokenFirst == $# ifTrue: [ ^self parseSymbol ].
	self error
</details>

#### SHParserST80>>#parseBlockArguments

<details>
	<summary>See more</summary>
	
	parseBlockArguments
	[ currentTokenFirst == $: ]
		whileTrue: [
			self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg ].
	currentTokenFirst == $| 
		ifTrue: [ self scanPast: #blockArgsBar ]
</details>

#### SHParserST80>>#parseByteArray

Literal ByteArray or literal FloatArray


<details>
	<summary>See more</summary>
	
	parseByteArray
	"Literal ByteArray or literal FloatArray"
	[currentTokenFirst == $]] whileFalse: [
		currentTokenFirst isDigit | (currentTokenFirst = $-)
			ifTrue: [
				"do not parse the number, can be time consuming"
				self scanPast: #number]
			ifFalse: [
				self failWhen: currentTokenFirst == $. .
				self error]].
	self scanPast: #arrayEnd
</details>

#### SHParserST80>>#instVarNamesDo: aBlock

<details>
	<summary>See more</summary>
	
	instVarNamesDo: aBlock

	| title |
	title _ '-- instance variables'.
	instanceVariables do: [ :name | aBlock value: name value: title ]
</details>

## SHRange

I associate a type with a range of characters in a String I have these instance variables... start - the one based index of the first character of the range within the String. end - the one based index of the last character of the range within the String. type - a Symbol describing the type of the range A sequence of instances of me are created by an instance of SHParserST80 which can then used by an instance of SHTextStyler to style Text.

### Methods
#### SHRange>>#end: anInteger

<details>
	<summary>See more</summary>
	
	end: anInteger
	end := anInteger
</details>

#### SHRange>>#length

<details>
	<summary>See more</summary>
	
	length
	^end - start + 1
</details>

#### SHRange>>#start

<details>
	<summary>See more</summary>
	
	start
	^start
</details>

#### SHRange>>#rangeType

<details>
	<summary>See more</summary>
	
	rangeType
	^type
</details>

#### SHRange>>#type: aSymbol

<details>
	<summary>See more</summary>
	
	type: aSymbol
	type := aSymbol
</details>

#### SHRange>>#start: startInteger end: endInteger type: typeSymbol

<details>
	<summary>See more</summary>
	
	start: startInteger end: endInteger type: typeSymbol
	start := startInteger.
	end := endInteger.
	type := typeSymbol
</details>

#### SHRange>>#end

<details>
	<summary>See more</summary>
	
	end
	^end
</details>

#### SHRange>>#start: anInteger

<details>
	<summary>See more</summary>
	
	start: anInteger
	start := anInteger
</details>

## SHTextStyler

I am an Abstract class. Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them. They may perform their styling asynchronously, in a background process which I create and manage. My public interface is... view: aViewOrMorph - set the view that will receive notifications when styling has completed. format: aText - modifies aText's string style: aText - modifies the TextAttributes of aText, but does not change the string, then triggers #shoutStyled. styleInBackgroundProcess: aText - performs style: in a background process, then sends #stylerStylednBackground: to the view. styledTextFor: aText - answers a formatted and styled copy of aText unstyledTextFrom: aText - answers a copy of aText with all TextAttributes removed Subclasses of me should re-implement... privateFormat: aText - answer a formatted version of aText; the String may be changed privateStyle: aText - modify the TextAttributes of aText; but do not change the String

### Methods
#### SHTextStyler>>#mutex

<details>
	<summary>See more</summary>
	
	mutex
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex
</details>

#### SHTextStyler>>#formatAndStyle: text allowBackgroundStyleProcess: aBoolean

Do the styling on a copy of the model text. After finishing, send it to the model, by triggering #shoutStyled The model should grab the TextAttributes we added to the copy, as appropriate.


<details>
	<summary>See more</summary>
	
	formatAndStyle: text allowBackgroundStyleProcess: aBoolean
	"Do the styling on a copy of the model text.
	After finishing, send it to the model, by triggering #shoutStyled
	The model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.
	formattedText _ text.
	self privateFormatAndConvert.
	(aBoolean and: [formattedText size > 4096])
		ifTrue: [
			formattedText size < 65536 ifTrue: [
				self styleInBackgroundProcess ]]
		ifFalse: [
			self privateStyle.
			textModel changed: #shoutStyled ]
</details>

#### SHTextStyler>>#formattedText

<details>
	<summary>See more</summary>
	
	formattedText
	^ formattedText
</details>

#### SHTextStyler>>#privateFormatAndConvert

<details>
	<summary>See more</summary>
	
	privateFormatAndConvert
	self subclassResponsibility 
</details>

#### SHTextStyler>>#privateStyle

<details>
	<summary>See more</summary>
	
	privateStyle
	self subclassResponsibility 
</details>

#### SHTextStyler>>#terminateBackgroundStylingProcess

<details>
	<summary>See more</summary>
	
	terminateBackgroundStylingProcess

	self mutex critical: [
		backgroundProcess
			ifNotNil: [
				backgroundProcess terminate.
				backgroundProcess _ nil ].
	]
</details>

#### SHTextStyler>>#styleInBackgroundProcess

Do the styling on a copy of the provided text (and in a separate process). After finishing, send it to the model, by triggering #shoutStyled The the model should grab the TextAttributes we added to the copy, as appropriate.


<details>
	<summary>See more</summary>
	
	styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	self terminateBackgroundStylingProcess.

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]
</details>

#### SHTextStyler>>#textModel: aTextModel

<details>
	<summary>See more</summary>
	
	textModel: aTextModel
	textModel _ aTextModel
</details>

## SHTextStylerST80

I style Smalltalk methods and expressions. My 'styleTable' class instance var holds an array ofArrays which control how each token is styled/coloured. See my defaultStyleTable class method for its structure. My styleTable can be changed by either modifying the defaultStyleTable class method and then executing SHTextStylerST80 initialize ; or by giving me a new styleTable through my #styleTable: class method. My 'textAttributesByPixelSize' class instance var contains a dictionary of dictionaries. The key is a pixelSize and the value a Dictionary from token type Symbol to TextAttribute array. It is created/maintained automatically. I also install these 3 preferences when my class initialize method is executed.... #syntaxHighlightingAsYouType - controls whether methods are styled in browsers #syntaxHighlightingAsYouTypeAnsiAssignment - controls whether assignments are formatted to be := #syntaxHighlightingAsYouTypeLeftArrowAssignment - controls whether assignments are formatted to be _ I reimplement #unstyledTextFrom: so that TextActions are preserved in the unstyled text

### Methods
#### SHTextStylerST80>>#convertAssignmentsToLeftArrow

If the Preference is to show leftArrowAssignments then answer a copy of <aText> where each ansi assignment (:=) is replaced with a left arrow. A parser is used so that each ':=' is only replaced if it actually occurs within an assigment statement


<details>
	<summary>See more</summary>
	
	convertAssignmentsToLeftArrow
	"If the Preference is to show leftArrowAssignments then answer a copy of  <aText> where each ansi assignment (:=) is replaced with a left arrow. A parser is used so that each ':=' is only replaced if it actually occurs within an assigment statement"

	self replaceStringForRangesWithType: #ansiAssignment with: '_'
</details>

#### SHTextStylerST80>>#classOrMetaClass: aBehavior

<details>
	<summary>See more</summary>
	
	classOrMetaClass: aBehavior
	classOrMetaClass := aBehavior
</details>

#### SHTextStylerST80>>#attributesFor: aSymbol

<details>
	<summary>See more</summary>
	
	attributesFor: aSymbol 
	^self class attributesFor: aSymbol
	
</details>

#### SHTextStylerST80>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	disableFormatAndConvert _ false
</details>

#### SHTextStylerST80>>#replaceStringForRangesWithType: aSymbol with: aString

Answer aText if no replacements, or a copy of aText with each range with a type of aSymbol replaced by aString


<details>
	<summary>See more</summary>
	
	replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"
	| toReplace increaseInLength |

	"We don't handle format and conversion for debuggers"
	disableFormatAndConvert ifTrue: [ ^self ].

	self parseSetWorkspace: false.
	toReplace _ parser ranges select: [ :each |
		each rangeType = aSymbol ].
	toReplace isEmpty ifTrue: [ ^self ].
	increaseInLength := 0.

	(toReplace asArray sort: [ :a :b | a start <= b start ]) 
		do: [ :each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]
</details>

#### SHTextStylerST80>>#privateStyle

<details>
	<summary>See more</summary>
	
	privateStyle

	| alpha end start count startIndexes c hue |
	self parseSetWorkspace: true.
	parser ranges ifNotNil: [ :ranges |
		self setAttributesFromRanges: ranges ].

	Preferences highlightBlockNesting ifTrue: [
		startIndexes _ parser blockDepthsStartIndexes.
		count _ startIndexes size.
		parser blockDepths withIndexDo: [ :depth :idx |
			start _ startIndexes at: idx.
			end _ idx = count ifTrue: [formattedText size] ifFalse: [ (startIndexes at: idx+1)-1].
			alpha _ depth / 10.0 min: 1.0.
			hue _ depth * 60.
			c _ Color h: hue s: 0.2 v: 0.5 alpha: alpha.
			formattedText 
				addAttribute: (ShoutTextBackgroundColor color: c ) 
				from: start 
				to: end ]]
</details>

#### SHTextStylerST80>>#workspace: aWorkspace

<details>
	<summary>See more</summary>
	
	workspace: aWorkspace
	workspace := aWorkspace
</details>

#### SHTextStylerST80>>#convertAssignmentsToAnsi

If the Preference is to show ansiAssignments then answer a copy of <aText> where each left arrow assignment is replaced with a ':=' ansi assignment. A parser is used so that each left arrow is only replaced if it occurs within an assigment statement


<details>
	<summary>See more</summary>
	
	convertAssignmentsToAnsi
	"If the Preference is to show ansiAssignments then answer a copy of  <aText> where each  left arrow assignment is replaced with a ':=' ansi assignment. A parser is used so that each left arrow is only replaced if it occurs within an assigment statement"

	self replaceStringForRangesWithType: #assignment with: ':='
</details>

#### SHTextStylerST80>>#setAttributesFromRanges: ranges

<details>
	<summary>See more</summary>
	
	setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].

	"Optimize for mutation speed unless method is really large but with very few distict elements:
	Source code that includes just big literals is better served by conventional Text+RunArray"
	"Do it only if we are not breaking textModel! (for instance, StyledTextEditor asks for formatting just sections, not whole #actualContents)."
	formattedText == textModel actualContents ifTrue: [
		(formattedText size > 2000 and: [ ranges size < 50 ]) ifFalse: [
			formattedText _ formattedText optimizedForMutationSpeed.
			textModel basicActualContents: formattedText ]].

	ranges do: [ :range |

		"Smalltalk text styling"
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		"Show as subscripts if appropriate."
		classOrMetaClass ifNotNil: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]]
</details>

#### SHTextStylerST80>>#parseSetWorkspace: aBoolean

Answer a collection of SHRanges by parsing aText. When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace


<details>
	<summary>See more</summary>
	
	parseSetWorkspace: aBoolean
	"Answer a collection of SHRanges by parsing aText.
	When formatting it is not necessary to set the workspace, and this can make the parse take less time, so aBoolean specifies whether the parser should be given the workspace"
	parser ifNil: [ parser := SHParserST80 new ].
	parser
		workspace:
			(aBoolean ifTrue: [ workspace ]);
		classOrMetaClass: classOrMetaClass;
		source: formattedText asString.
	parser parse.
	^ parser ranges.
</details>

#### SHTextStylerST80>>#privateFormatAndConvert

Perform any formatting of formattedText necessary and store or a formatted copy in formattedText


<details>
	<summary>See more</summary>
	
	privateFormatAndConvert
	"Perform any formatting of formattedText necessary and store or a formatted copy in formattedText"
	Preferences syntaxHighlightingAsYouTypeAnsiAssignment ifTrue: [
		self convertAssignmentsToAnsi ].
	Preferences syntaxHighlightingAsYouTypeLeftArrowAssignment ifTrue: [
		self convertAssignmentsToLeftArrow ]
</details>

#### SHTextStylerST80>>#disableFormatAndConvert

<details>
	<summary>See more</summary>
	
	disableFormatAndConvert
	disableFormatAndConvert _ true
</details>

