## AbstractFont

AbstractFont defines the generic interface that all fonts need to implement.

### Methods
#### AbstractFont>>#boldItalic

<details>
	<summary>See more</summary>
	
	boldItalic
	^self emphasized: AbstractFont boldCode+AbstractFont italicCode
</details>

#### AbstractFont>>#releaseCachedState

<details>
	<summary>See more</summary>
	
	releaseCachedState
	
</details>

#### AbstractFont>>#characterToGlyphMap

Return the character to glyph mapping table. If the table is not provided the character scanner will query the font directly for the width of each individual character.


<details>
	<summary>See more</summary>
	
	characterToGlyphMap
	"Return the character to glyph mapping table. If the table is not provided the character scanner will query the font directly for the width of each individual character."
	^nil
</details>

#### AbstractFont>>#widthOfString: aString from: startIndex to: stopIndex

Measure the length of the given string between start and stop index


<details>
	<summary>See more</summary>
	
	widthOfString: aString from: startIndex to: stopIndex
	"Measure the length of the given string between start and stop index"
	| character resultX |
	resultX _ 0.
	startIndex to: stopIndex do:[:i | 
		character _ aString at: i.
		resultX _ resultX + (self widthOf: character)].
	^resultX
</details>

#### AbstractFont>>#widthOfString: aString

<details>
	<summary>See more</summary>
	
	widthOfString: aString
	aString ifNil:[^0].
	^self widthOfString: aString from: 1 to: aString size.
"
	AbstractFont default widthOfString: 'zort' 
"
</details>

#### AbstractFont>>#pointSize

<details>
	<summary>See more</summary>
	
	pointSize
	self subclassResponsibility.
</details>

#### AbstractFont>>#ascent

<details>
	<summary>See more</summary>
	
	ascent

	self subclassResponsibility 
</details>

#### AbstractFont>>#height

Answer the line spacing. Prefer calling #lineSpacing, that is more explicit. Besides, #height is false polymorphism, Fonts are not interchangeable with Form or Rectangle!


<details>
	<summary>See more</summary>
	
	height
	"Answer the line spacing. Prefer calling #lineSpacing, that is more explicit.
	Besides, #height is false polymorphism, Fonts are not interchangeable with Form or Rectangle!"

	self deprecatedMethod.
	^self lineSpacing
</details>

#### AbstractFont>>#bold

<details>
	<summary>See more</summary>
	
	bold
	^self emphasized: AbstractFont boldCode
</details>

#### AbstractFont>>#emphasized: code

<details>
	<summary>See more</summary>
	
	emphasized: code
	^self subclassResponsibility
</details>

#### AbstractFont>>#approxWidthOfText: aText

Return the width of aText -- quickly, and a little bit dirty. Used by lists morphs containing Text objects to get a quick, fairly accurate measure of the width of a list item.


<details>
	<summary>See more</summary>
	
	approxWidthOfText: aText
	"Return the width of aText -- quickly, and a little bit dirty.
	Used by lists morphs containing Text objects to get a quick,
	fairly accurate measure of the width of a list item."
	| w |
	(aText isNil or: [ aText size = 0 ]) ifTrue: [ ^ 0 ].
	w _ self
		widthOfString: aText string
		from: 1
		to: aText size.
	"If the text has no emphasis, just return the string size.  If it is empasized, 
	just approximate the width by adding about 20% to the width"
	^ ((aText runLengthFor: 1) = aText size and: [ (aText emphasisAt: 1) = 0 ])
		ifTrue: [ w ]
		ifFalse: [ w * 6 // 5 ]
</details>

#### AbstractFont>>#underlined

<details>
	<summary>See more</summary>
	
	underlined
	^self emphasized: AbstractFont underlinedCode
</details>

#### AbstractFont>>#widthOfStringOrText: aStringOrText

<details>
	<summary>See more</summary>
	
	widthOfStringOrText: aStringOrText
    aStringOrText ifNil:[^0].
    ^(aStringOrText is: #Text)
        ifTrue: [ self approxWidthOfText: aStringOrText ]
        ifFalse: [ self widthOfString: aStringOrText ] 
</details>

#### AbstractFont>>#italic

<details>
	<summary>See more</summary>
	
	italic
	^self emphasized: AbstractFont italicCode
</details>

#### AbstractFont>>#onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color

Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position
	Answer nil if nothing was done"

	self subclassResponsibility
</details>

#### AbstractFont>>#baseKern

Required to answer a number if #xTable is provided


<details>
	<summary>See more</summary>
	
	baseKern
	"Required to answer a number if #xTable is provided"
	^nil
</details>

#### AbstractFont>>#derivativeFonts

<details>
	<summary>See more</summary>
	
	derivativeFonts
	^#()
</details>

#### AbstractFont>>#xTable

Return the xTable for the font. The xTable defines the left x-value for each individual glyph in the receiver. If such a table is not provided, the character scanner will ask the font directly for the appropriate width of each individual character.


<details>
	<summary>See more</summary>
	
	xTable
	"Return the xTable for the font. The xTable defines the left x-value for each individual glyph in the receiver. If such a table is not provided, the character scanner will ask the font directly for the appropriate width of each individual character."
	^nil
</details>

#### AbstractFont>>#struckThrough

<details>
	<summary>See more</summary>
	
	struckThrough
	^self emphasized: AbstractFont struckThroughCode
</details>

#### AbstractFont>>#widthOf: aCharacter

Return the width of the given character


<details>
	<summary>See more</summary>
	
	widthOf: aCharacter
	"Return the width of the given character"
	^self subclassResponsibility
</details>

#### AbstractFont>>#normalizedWidthOf: aCharacter

Return the width of the given character, irrespective of point size.


<details>
	<summary>See more</summary>
	
	normalizedWidthOf: aCharacter
	"Return the width of the given character, irrespective of point size."
	^ (self widthOf: aCharacter) / self pointSize
</details>

## CharacterBlock

My instances contain information about displayed characters. They are used to return the results of methods: TextComposition characterBlockAtPoint: aPoint and TextComposition characterBlockForIndex: stringIndex. Any recomposition or movement of a TextComposition can make the instance obsolete. My instances are effectively inmutable, as the only method settings instance variables is private and used just to build new instances.

### Methods
#### CharacterBlock>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^stringIndex hash
</details>

#### CharacterBlock>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream nextPutAll: 'a CharacterBlock with index '.
	stringIndex printOn: aStream.
	(text notNil and: [text size> 0 and: [stringIndex between: 1 and: text size]])
		ifTrue: [
			aStream nextPutAll: ' and character '.
			(text at: stringIndex) printOn: aStream].
	aStream nextPutAll: ' and rectangle '.
	super printOn: aStream.
	textLine ifNotNil: [
		aStream newLine; nextPutAll: ' in '.
		textLine printOn: aStream].

</details>

#### CharacterBlock>>#textLine

<details>
	<summary>See more</summary>
	
	textLine
	^ textLine
</details>

#### CharacterBlock>>#setStringIndex: anInteger text: aText topLeft: topLeft extent: extent textLine: aLine

<details>
	<summary>See more</summary>
	
	setStringIndex: anInteger text: aText topLeft: topLeft extent: extent textLine: aLine

	stringIndex _ anInteger.
	text _ aText.
	textLine _ aLine.
	self setOrigin: topLeft corner: topLeft + extent
</details>

#### CharacterBlock>>#>= aCharacterBlock

Answer whether the string index of the receiver does not precede that of aCharacterBlock.


<details>
	<summary>See more</summary>
	
	>= aCharacterBlock 
	"Answer whether the string index of the receiver does not precede that of 
	aCharacterBlock."

	^(self < aCharacterBlock) not
</details>

#### CharacterBlock>>#min: aCharacterBlock

<details>
	<summary>See more</summary>
	
	min: aCharacterBlock
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock < self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
</details>

#### CharacterBlock>>#<= aCharacterBlock

Answer whether the string index of the receiver does not come after that of aCharacterBlock.


<details>
	<summary>See more</summary>
	
	<= aCharacterBlock 
	"Answer whether the string index of the receiver does not come after that 
	of aCharacterBlock."

	^(self > aCharacterBlock) not
</details>

#### CharacterBlock>>#> aCharacterBlock

Answer whether the string index of the receiver comes after that of aCharacterBlock.


<details>
	<summary>See more</summary>
	
	> aCharacterBlock 
	"Answer whether the string index of the receiver comes after that of 
	aCharacterBlock."

	^aCharacterBlock < self
</details>

#### CharacterBlock>>#< aCharacterBlock

Answer whether the string index of the receiver precedes that of aCharacterBlock.


<details>
	<summary>See more</summary>
	
	< aCharacterBlock 
	"Answer whether the string index of the receiver precedes that of 
	aCharacterBlock."

	^stringIndex < aCharacterBlock stringIndex
</details>

#### CharacterBlock>>#stringIndex

Answer the position of the receiver in the string it indexes.


<details>
	<summary>See more</summary>
	
	stringIndex
	"Answer the position of the receiver in the string it indexes."

	^stringIndex
</details>

#### CharacterBlock>>#= aCharacterBlock

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= aCharacterBlock

	self == aCharacterBlock ifTrue: [ ^ true ].

	self species == aCharacterBlock species
		ifFalse: [ ^false ].
		
	^ stringIndex = aCharacterBlock stringIndex
</details>

#### CharacterBlock>>#max: aCharacterBlock

<details>
	<summary>See more</summary>
	
	max: aCharacterBlock
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock > self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].
</details>

## CharacterBlockScanner

My instances are used to scan text to compute the CharacterBlock for a character specified by its index in the text or its proximity to the cursor location.

### Methods
#### CharacterBlockScanner>>#paddedSpace

When the line is justified, the spaces will not be the same as the font's space character. A padding of extra space must be considered in trying to find which character the cursor is pointing at. Answer whether the scanning has crossed the cursor.


<details>
	<summary>See more</summary>
	
	paddedSpace
	"When the line is justified, the spaces will not be the same as the font's 
	space character. A padding of extra space must be considered in trying 
	to find which character the cursor is pointing at. Answer whether the 
	scanning has crossed the cursor."

	| pad |
	spaceCount _ spaceCount + 1.
	pad _ line justifiedPadFor: spaceCount.
	lastSpaceOrTabWidth _ spaceWidth + pad.
	(destX + lastSpaceOrTabWidth) >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	lastIndex _ lastIndex + 1.
	destX _ destX + lastSpaceOrTabWidth.
	^ false
</details>

#### CharacterBlockScanner>>#tab

<details>
	<summary>See more</summary>
	
	tab
	| currentX |
	currentX _ self tabDestX.
	lastSpaceOrTabWidth _ currentX - destX max: 0.
	currentX >= characterPoint x
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth.
			^ self crossedX ].
	destX _ currentX.
	lastIndex _ lastIndex + 1.
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^false
</details>

#### CharacterBlockScanner>>#setStopConditions

Set the font and the stop conditions for the current run.


<details>
	<summary>See more</summary>
	
	setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]
</details>

#### CharacterBlockScanner>>#endOfRun

Before arriving at the cursor location, the selection has encountered an end of run. Answer false if the selection continues, true otherwise. Set up indexes for building the appropriate CharacterBlock.


<details>
	<summary>See more</summary>
	
	endOfRun
	"Before arriving at the cursor location, the selection has encountered an 
	end of run. Answer false if the selection continues, true otherwise. Set 
	up indexes for building the appropriate CharacterBlock."

	| runLength lineStop lastChar |
	((characterIndex notNil and: [
		runStopIndex < characterIndex and: [ runStopIndex < text size ]])
			or:	[ characterIndex isNil and: [ lastIndex < line last ]])
		ifTrue:	[
			"We're really at the end of a real run."
			runLength _ (text runLengthFor: (lastIndex _ lastIndex + 1)).
			lineStop _ characterIndex		"scanning for index"
				ifNil: [ line last ].			"scanning for point"
			(runStopIndex _ lastIndex + (runLength - 1)) > lineStop
				ifTrue: [ runStopIndex _ lineStop ].
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^ false].

	lastChar _ text at: lastIndex.
	characterPoint _ destX @ destY.
	(( lastChar = Character space and: [ alignment = CharacterScanner justifiedCode ])
		or: [ lastChar = Character tab and: [ lastSpaceOrTabWidth notNil ]])
		ifTrue: [
			lastCharacterWidth _ lastSpaceOrTabWidth ].
	characterIndex
		ifNotNil: [
			"If scanning for an index and we've stopped on that index,
			then we back destX off by the width of the character stopped on
			(it will be pointing at the right side of the character) and return"
			runStopIndex = characterIndex
				ifTrue: [
					characterPoint _ destX - lastCharacterWidth @ characterPoint y.
					^ true ].
			"Otherwise the requested index was greater than the length of the
			string.  Return string size + 1 as index, indicate further that off the
			string by setting character to nil and the extent to 0."
			lastIndex _  lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue: [
			"off end of string"
			lastIndex _  lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].
	"just off end of line without crossing x"
	lastIndex _ lastIndex + 1.
	^true
</details>

#### CharacterBlockScanner>>#setFont

Set the font and other emphasis. In fact set actual ParagraphStyle (or nil), alignment, font and emphasis


<details>
	<summary>See more</summary>
	
	setFont
	specialWidth _ nil.
	super setFont
</details>

#### CharacterBlockScanner>>#doNewLine

Answer a CharacterBlock that specifies the current location of the mouse relative to a newLine stop condition that has just been encountered. The convention is to denote selections by CharacterBlocks, sometimes including the newLine character (cursor is at the end) and sometimes not (cursor is in the middle of the text).


<details>
	<summary>See more</summary>
	
	doNewLine
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a newLine stop condition that has just been 
	encountered. The convention is to denote selections by 
	CharacterBlocks, sometimes including the newLine character (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue: [
			"When off end of string, give data for next character"
			destY _ destY +  line lineHeight.
			characterPoint _ leftMargin @ destY.
			lastIndex _ lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].
		characterPoint _ destX @ destY.
		lastCharacterWidth _ rightMargin - destX.
		^true
</details>

#### CharacterBlockScanner>>#characterBlockAtPoint: aPoint index: index in: textLine

This method is the Morphic characterBlock finder.


<details>
	<summary>See more</summary>
	
	characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastTabIndex _ lastIndex _ line first.
	self setFont.
	self setStopConditions.
	characterIndex _ index.  "nil means scanning for point"
	characterPoint _ aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint _ line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	lastTabX _ destX _ leftMargin _ line leftMarginForAlignment: alignment.
	destY _ line top.
	runLength _ text runLengthFor: line first.
	lineStop _ characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex _ lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth _ 0.
	spaceCount _ 0.

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: text string rightX: characterPoint x
			stopConditions: stopConditions kern: font baseKern.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth _ specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat
</details>

#### CharacterBlockScanner>>#placeEmbeddedObject: anchoredFormOrMorph

Place the anchoredMorph or return false if it cannot be placed. In any event, advance destX by its width.


<details>
	<summary>See more</summary>
	
	placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	specialWidth _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphWidth ]
		ifFalse: [ anchoredFormOrMorph width ].
	^ true
</details>

#### CharacterBlockScanner>>#crossedX

Text display has wrapping. The scanner just found a character past the x location of the cursor. We know that the cursor is pointing at a character or before one.


<details>
	<summary>See more</summary>
	
	crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| lastChar currentX |
	"Scanning for index"
	characterIndex ifNotNil: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex _ characterIndex.
			characterPoint _ leftMargin @ (destY + line lineHeight).
			^true ]].

	"Pointing before middle of a character"
	characterPoint x <= (destX + (lastCharacterWidth // 2)) ifTrue: [
		characterPoint _ destX - (font isBold ifTrue: [1] ifFalse: [0]) @ destY.
		^true ].

	"Pointing past the end"
	lastIndex >= line last ifTrue: [
		characterPoint _ destX @ destY.
		^true ].

	"Pointing past middle of a character, return the next character."
	lastIndex _ lastIndex + 1.
	currentX _ destX + lastCharacterWidth - (font isBold ifTrue: [2] ifFalse: [0]).
	lastChar _ text at: lastIndex.
	lastCharacterWidth _ font widthOf: lastChar.
	characterPoint _ currentX @ destY.
	lastChar = Character space ifFalse: [
		^ true].

	"Yukky if next character is space or tab."
	alignment = CharacterScanner justifiedCode ifTrue: [
		lastCharacterWidth _ lastCharacterWidth + (line justifiedPadFor: (spaceCount + 1)).
		^ true].

	^ true
</details>

## CharacterScanner

My instances hold the state associated with scanning text. My subclasses scan characters for specified purposes, such as computing a CharacterBlock or placing characters into Forms. My instances are usually transient, lasting only during the scanning operation, and therefore don't need a TextModel, but just a Text with the current contents.

### Methods
#### CharacterScanner>>#tab

<details>
	<summary>See more</summary>
	
	tab
	^self subclassResponsibility
</details>

#### CharacterScanner>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	lastTabIndex _ lastTabX _ destX _ destY _ leftMargin _ rightMargin _ 0.
</details>

#### CharacterScanner>>#tabDestX

This is the basic method of adjusting destX for a tab.


<details>
	<summary>See more</summary>
	
	tabDestX
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			"Add the width of a tab for every two chars since last tab, to last tab x position."
			(lastIndex - lastTabIndex // 3 + 1) * tabWidth + lastTabX min: rightMargin ]
</details>

#### CharacterScanner>>#textColor: ignored

Overridden in MorphicScanner


<details>
	<summary>See more</summary>
	
	textColor: ignored
	"Overridden in MorphicScanner"
</details>

#### CharacterScanner>>#scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernValue

Primitive. This is the inner loop of text display--but see scanCharactersFrom: to:rightX: which would get the string, stopConditions and displaying from the instance. March through source String from startIndex to stopIndex. If any character is flagged with a non-nil entry in stops, then return the corresponding value. Determine width of each character from xTable, indexed by map. If dextX would exceed rightX, then return stops at: 258. Advance destX by the width of the character. If stopIndex has been reached, then return stops at: 257. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	scanCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernValue
	"Primitive. This is the inner loop of text display--but see
	scanCharactersFrom: to:rightX: which would get the string,
	stopConditions and displaying from the instance. March through source
	String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value. Determine
	width of each character from xTable, indexed by map.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257. Optional.
	See Object documentation whatIsAPrimitive."
	| nextDestX char |
	<primitive: 103>
	lastIndex _ startIndex.
	[ lastIndex <= stopIndex ]
		whileTrue: [
			char _ sourceString at: lastIndex.
			"stops are only defined for the first 256 characters.
			If we (ever) handle Character like objects beyond those in ISO-8859-15,
			thenf #iso8859s15Code shound answer nil!"
			char iso8859s15Code ifNotNil: [ :code |
				(stops at: code + 1) ifNotNil: [ :stop | ^stop ]].
			nextDestX _ destX + (font widthOf: char).
			nextDestX > rightX ifTrue: [
				^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode
</details>

#### CharacterScanner>>#setFont

Set the font and other emphasis. In fact set actual ParagraphStyle (or nil), alignment, font and emphasis


<details>
	<summary>See more</summary>
	
	setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex default: defaultFont).
	attributes _ text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment _ alignmentInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	tabWidth _ font pointSize * 5 // 2.
	xTable _ font xTable
</details>

#### CharacterScanner>>#doNewLine

<details>
	<summary>See more</summary>
	
	doNewLine
	^self subclassResponsibility
</details>

#### CharacterScanner>>#placeEmbeddedObject: anchoredFormOrMorph

Place the anchoredMorph or return false if it cannot be placed. In any event, advance destX by its width.


<details>
	<summary>See more</summary>
	
	placeEmbeddedObject: anchoredFormOrMorph
	"Place the anchoredMorph or return false if it cannot be placed.
	In any event, advance destX by its width."

	| w |
	w _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphWidth ]
		ifFalse: [ anchoredFormOrMorph width ].
	destX _ destX + w.
	(destX > rightMargin and: [ lastIndex ~= line first ])
		"Won't fit, but  not at start of a line. Start a new line with it"
		ifTrue: [ ^ false].
	lastIndex _ lastIndex + 1.
	^ true
</details>

#### CharacterScanner>>#backgroundColor: ignored

Overridden in MorphicScanner


<details>
	<summary>See more</summary>
	
	backgroundColor: ignored
	"Overridden in MorphicScanner"
</details>

#### CharacterScanner>>#setActualFont: aFont

Set the basal font to an isolated font reference.


<details>
	<summary>See more</summary>
	
	setActualFont: aFont
	"Set the basal font to an isolated font reference."

	font _ aFont
</details>

#### CharacterScanner>>#placeEmbeddedObject

<details>
	<summary>See more</summary>
	
	placeEmbeddedObject
	(text attributesAt: lastIndex) do: [ :attr |
		attr anchoredFormOrMorph ifNotNil: [ :m |
			self placeEmbeddedObject: m ]].
</details>

#### CharacterScanner>>#defaultFont: aFont

<details>
	<summary>See more</summary>
	
	defaultFont: aFont
	defaultFont _ aFont
</details>

#### CharacterScanner>>#text: aText

<details>
	<summary>See more</summary>
	
	text: aText
	text _ aText.
	paragraphStyle _ nil
</details>

## CompositionScanner

CompositionScanners are used to measure text and determine where line breaks and space padding should occur.

### Methods
#### CompositionScanner>>#tab

Advance destination x according to tab settings in the current ParagraphStyle. Answer whether the character has crossed the right edge of the composition rectangle of the TextComposition.


<details>
	<summary>See more</summary>
	
	tab
	"Advance destination x according to tab settings in the current
	ParagraphStyle. Answer whether the character has crossed the right edge of 
	the composition rectangle of the TextComposition."

	destX _ self tabDestX.
	destX > rightMargin ifTrue: [^self crossedX].
	lastIndex _ lastIndex + 1.
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^false

</details>

#### CompositionScanner>>#setStopConditions

Set the font and the stop conditions for the current run.


<details>
	<summary>See more</summary>
	
	setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ self class stopConditionsWithSpace
</details>

#### CompositionScanner>>#endOfRun

Answer true if scanning has reached the end of the TextComposition. Otherwise step conditions (mostly install potential new font) and answer false.


<details>
	<summary>See more</summary>
	
	endOfRun
	"Answer true if scanning has reached the end of the TextComposition. 
	Otherwise step conditions (mostly install potential new font) and answer 
	false."

	| runLength |
	lastIndex = text size
		ifTrue:	[
			line stop: lastIndex.
			spaceX _ destX.
			line paddingWidth: rightMargin - destX - 1.
			^true ]
		ifFalse:	[
			runLength _ (text runLengthFor: (lastIndex _ lastIndex + 1)).
			runStopIndex _ lastIndex + (runLength - 1).
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^false ]
</details>

#### CompositionScanner>>#doNewLine

Answer true. Set up values for the text line interval currently being composed.


<details>
	<summary>See more</summary>
	
	doNewLine
	"Answer true. Set up values for the text line interval currently being 
	composed."

	line stop: lastIndex.
	spaceX _ destX.
	line paddingWidth: rightMargin - spaceX - 1.
	^true
</details>

#### CompositionScanner>>#placeEmbeddedObject: anchoredFormOrMorph

Place the anchoredMorph or return false if it cannot be placed. In any event, advance destX by its width.


<details>
	<summary>See more</summary>
	
	placeEmbeddedObject: anchoredFormOrMorph
	| descent h |

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [
		line stop: lastIndex-1.
		^ false].
	descent _ lineHeight - baseline.
	h _ (anchoredFormOrMorph is: #Morph)
		ifTrue: [ anchoredFormOrMorph morphHeight ]
		ifFalse: [ anchoredFormOrMorph height ].
	baseline _ baseline max: h.
	lineHeight _ baseline + descent.
	line stop: lastIndex.
	^ true
</details>

#### CompositionScanner>>#rightX

Meaningful only when a line has just been composed -- refers to the line most recently composed. This is a subtrefuge to allow for easy resizing of a composition rectangle to the width of the maximum line. Useful only when there is only one line in the form or when each line is terminated by a carriage return. Handy for sizing menus and lists.


<details>
	<summary>See more</summary>
	
	rightX
	"Meaningful only when a line has just been composed -- refers to the 
	line most recently composed. This is a subtrefuge to allow for easy 
	resizing of a composition rectangle to the width of the maximum line. 
	Useful only when there is only one line in the form or when each line 
	is terminated by a carriage return. Handy for sizing menus and lists."

	^spaceX
</details>

#### CompositionScanner>>#lastLineBreakingSpace

<details>
	<summary>See more</summary>
	
	lastLineBreakingSpace
	^lastLineBreakingSpace
</details>

#### CompositionScanner>>#setActualFont: aFont

Keep track of max height and ascent for auto lineheight


<details>
	<summary>See more</summary>
	
	setActualFont: aFont
	"Keep track of max height and ascent for auto lineheight"
	| descent |
	super setActualFont: aFont.
	lineHeight
		ifNil: [
			descent _ font descent.
			baseline _ font ascent.
			lineHeight _ baseline + descent]
		ifNotNil: [
			descent _ lineHeight - baseline max: font descent.
			baseline _ baseline max: font ascent.
			lineHeight _ lineHeight max: baseline + descent]
</details>

#### CompositionScanner>>#composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

Answer an instance of TextLineInterval that represents the next line in the paragraph.


<details>
	<summary>See more</summary>
	
	composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastTabIndex _ lastIndex _ startIndex.	"scanning sets last index"
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

	"Set up margins"
	leftMargin _ lineRectangle left.
	rightMargin _ lineRectangle right.
	xtraSpaceBefore _ 0.
	spaceAfterParagraph _ 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin _ leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin _ rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore _ paragraphStyle spaceBefore ].
		spaceAfterParagraph _ paragraphStyle spaceAfter ].
	lastTabX _ destX _ spaceX _ leftMargin.

	runLength _ text runLengthFor: startIndex.
	runStopIndex _ lastIndex + runLength - 1.
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	lastLineBreakingSpace _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: font baseKern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat
</details>

#### CompositionScanner>>#space

Record left x and character index of the space character just encounted. Used for wrap-around. Answer whether the character has crossed the right edge of the composition rectangle of the TextComposition.


<details>
	<summary>See more</summary>
	
	space
	"Record left x and character index of the space character just encounted. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the TextComposition."
	spaceX _ destX.
	destX _ spaceX + spaceWidth.
	spaceIndex _ lastIndex.
	lineHeightAtSpace _ lineHeight.
	baselineAtSpace _ baseline.
	lastIndex _ lastIndex + 1.
	spaceCount _ spaceCount + 1.
	destX > rightMargin ifTrue: [ ^ self crossedX ].
	^ false
</details>

#### CompositionScanner>>#crossedX

There is a word that has fallen across the right edge of the composition rectangle. This signals the need for wrapping which is done to the last space that was encountered, as recorded by the space stop condition.


<details>
	<summary>See more</summary>
	
	crossedX
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition."

	spaceCount >= 1 ifTrue: [
		"The common case. First back off to the space at which we wrap."
		line stop: spaceIndex.
		lastLineBreakingSpace _ spaceIndex.
		lineHeight _ lineHeightAtSpace.
		baseline _ baselineAtSpace.
		spaceCount _ spaceCount - 1.
		spaceIndex _ spaceIndex - 1.

		"Check to see if any spaces preceding the one at which we wrap.
			Double space after punctuation, most likely."
		[(spaceCount > 1 and: [(text at: spaceIndex) = Character space])]
			whileTrue: [
				spaceCount _ spaceCount - 1.
				"Account for backing over a run which might
					change width of space."
				font _ text fontAt: spaceIndex default: defaultFont.
				spaceIndex _ spaceIndex - 1.
				spaceX _ spaceX - (font widthOf: Character space)].
		line paddingWidth: rightMargin - spaceX - 1.
		line internalSpaces: spaceCount]
	ifFalse: [
		"Neither internal nor trailing spaces -- almost never happens."
		lastIndex _ lastIndex - 1.
		[ destX <= rightMargin or: [ lastIndex = 0]]
			whileFalse: [
				destX _ destX - (font widthOf: (text at: lastIndex)).
				lastIndex _ lastIndex - 1].
		lastLineBreakingSpace _ lastIndex.
		spaceX _ destX.
		line paddingWidth: rightMargin - destX - 1.
		line stop: (lastIndex max: line first)].
	^true
</details>

## FontFamily

Also called Typeface.

### Methods
#### FontFamily>>#familyName: aString

<details>
	<summary>See more</summary>
	
	familyName: aString
	familyName _ aString.
	baseFontBySizes _ Dictionary new
</details>

#### FontFamily>>#atPointSize: aNumber

<details>
	<summary>See more</summary>
	
	atPointSize: aNumber
	^baseFontBySizes at: aNumber ifAbsent: nil
</details>

#### FontFamily>>#familyName

<details>
	<summary>See more</summary>
	
	familyName
	^ familyName
</details>

#### FontFamily>>#pointSizes

<details>
	<summary>See more</summary>
	
	pointSizes
	^baseFontBySizes keys sort
</details>

#### FontFamily>>#aroundPointSize: aNumber

<details>
	<summary>See more</summary>
	
	aroundPointSize: aNumber
	^baseFontBySizes at: aNumber ifAbsent: [ |found |
		found _ nil.
		baseFontBySizes do: [ :font |
			(found isNil or: [ (found pointSize - aNumber) abs > (font pointSize - aNumber) abs ])
				ifTrue: [ found _ font ]].
			found
		]
</details>

## MorphicScanner

My instances are used to scan text and display it on the screen or in a hidden form.

### Methods
#### MorphicScanner>>#displayBulletTextLeft: texLeft number: bulletNumber

texLeft is relative to the morph currently being drawn


<details>
	<summary>See more</summary>
	
	displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character numericValue: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character numericValue: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor
</details>

#### MorphicScanner>>#paddedSpace

Each space is a stop condition when the alignment is right justified. Padding must be added to the base width of the space according to which space in the line this space is and according to the amount of space that remained at the end of the line when it was composed.


<details>
	<summary>See more</summary>
	
	paddedSpace
	"Each space is a stop condition when the alignment is right justified. 
	Padding must be added to the base width of the space according to 
	which space in the line this space is and according to the amount of 
	space that remained at the end of the line when it was composed."

	spaceCount _ spaceCount + 1.
	destX _ destX + spaceWidth + (line justifiedPadFor: spaceCount).
	lastIndex _ lastIndex + 1.
	^ false
</details>

#### MorphicScanner>>#tab

<details>
	<summary>See more</summary>
	
	tab
	destX _ self tabDestX.
	lastIndex _ lastIndex + 1.
	lastTabX _ destX.
	lastTabIndex _ lastIndex.
	^ false
</details>

#### MorphicScanner>>#textColor: textColor

Overridden in MorphicScanner


<details>
	<summary>See more</summary>
	
	textColor: textColor
	foregroundColor _ textColor
</details>

#### MorphicScanner>>#setStopConditions

Set the font and the stop conditions for the current run.


<details>
	<summary>See more</summary>
	
	setStopConditions
	"Set the font and the stop conditions for the current run."

	stopConditions _ alignment = CharacterScanner justifiedCode
		ifTrue: [ self class stopConditionsWithPaddedSpace ]
		ifFalse: [ self class defaultStopConditions ]
</details>

#### MorphicScanner>>#canvas: aCanvas

<details>
	<summary>See more</summary>
	
	canvas: aCanvas

	canvas _ aCanvas
</details>

#### MorphicScanner>>#endOfRun

The end of a run in the display case either means that there is actually a change in the style (run code) to be associated with the string or the end of this line has been reached.


<details>
	<summary>See more</summary>
	
	endOfRun
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastIndex = line last ifTrue: [^true].
	runLength _ text runLengthFor: (lastIndex _ lastIndex + 1).
	runStopIndex _ lastIndex + (runLength - 1) min: line last.
	self setFont.
	self setStopConditions.
	self placeEmbeddedObject.
	^ false
</details>

#### MorphicScanner>>#displayBulletIfAppropriateFor: textLine textLeft: texLeft

texLeft is relative to the morph currently being drawn


<details>
	<summary>See more</summary>
	
	displayBulletIfAppropriateFor: textLine textLeft: texLeft
	"texLeft is relative to the morph currently being drawn"

	| paragraphEnd count pattern |
	paragraphStyle ifNotNil: [
		(textLine isFirstLine and: [ paragraphStyle isListStyle ]) ifTrue: [
			pattern _ paragraphStyle listBulletPattern.
			"Count how many paragraphs before this one already used the pattern"
			count _ 0.
			paragraphEnd _ textLine first-1.
			[
			paragraphEnd > 0 and: [ ((text paragraphStyleOrNilAt: paragraphEnd) ifNotNil: [ :ps | ps listBulletPattern ]) = pattern ]] whileTrue: [
				count _ count + 1.
				paragraphEnd _ text string endOfParagraphBefore: paragraphEnd ].
			"Our number in the list, is one more than the count of previous contiguous paragraphs with this pattern"
			self
				displayBulletTextLeft: texLeft
				number: count + 1]]
</details>

#### MorphicScanner>>#setFont

Set the font and other emphasis. In fact set actual ParagraphStyle (or nil), alignment, font and emphasis


<details>
	<summary>See more</summary>
	
	setFont 
	foregroundColor _ defaultColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
</details>

#### MorphicScanner>>#text: t foreground: foreColor

<details>
	<summary>See more</summary>
	
	text: t foreground: foreColor
	text _ t.
	paragraphStyle _ nil.
	foregroundColor _ defaultColor _ foreColor
</details>

#### MorphicScanner>>#doNewLine

When a newLine is encountered, simply increment the pointer into the paragraph.


<details>
	<summary>See more</summary>
	
	doNewLine
	"When a newLine is encountered, simply increment the pointer 
	into the paragraph."

	lastIndex _ lastIndex + 1.
	^false
</details>

#### MorphicScanner>>#placeEmbeddedObject: anchoredFormOrMorph

Place the anchoredMorph or return false if it cannot be placed. In any event, advance destX by its width.


<details>
	<summary>See more</summary>
	
	placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					topLeft.
			anchoredFormOrMorph show.
			canvas fullDraw: anchoredFormOrMorph ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true
</details>

#### MorphicScanner>>#backgroundColor: aColor

Overridden in MorphicScanner


<details>
	<summary>See more</summary>
	
	backgroundColor: aColor
	backgroundColor _ aColor
</details>

#### MorphicScanner>>#displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun

The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated). leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions.


<details>
	<summary>See more</summary>
	
	displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastTabIndex _ lastIndex _ line first.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	lastTabX _ destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: font baseKern.
		backgroundColor ifNotNil: [
			x1 _ destX.
			(Preferences backgroundColorFillsAllBackground and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			(Preferences backgroundColorFillsAllBackground and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat
</details>

#### MorphicScanner>>#crossedX

This condition will sometimes be reached 'legally' during display, when, for instance the space that caused the line to wrap actually extends over the right boundary. This character is allowed to display, even though it is technically outside or straddling the clipping ectangle since it is in the normal case not visible and is in any case appropriately clipped by the scanner.


<details>
	<summary>See more</summary>
	
	crossedX
	"This condition will sometimes be reached 'legally' during display, when, 
	for instance the space that caused the line to wrap actually extends over 
	the right boundary. This character is allowed to display, even though it 
	is technically outside or straddling the clipping ectangle since it is in 
	the normal case not visible and is in any case appropriately clipped by 
	the scanner."

	^ true 
</details>

## StrikeFont

I represent a compact encoding of a set of Forms corresponding to characters in the ISO-8859-15 character set. All the forms are placed side by side in a large form whose height is the font height, and whose width is the sum of all the character widths. The xTable variable gives the left-x coordinates of the subforms corresponding to the glyphs. Characters are mapped to glyphs by using the characterToGlyphMap. Subclasses can have non-trivial mapping rules as well as different representations for glyphs sizes (e.g., not using an xTable). If so, these classes should return nil when queried for xTable and/or the characterToGlyphMap. This will cause the CharacterScanner primitive to fail and query the font for the width of a character (so that a more programatical approach can be implemented).

### Methods
#### StrikeFont>>#isBaseFont

<details>
	<summary>See more</summary>
	
	isBaseFont
	^emphasis = 0
</details>

#### StrikeFont>>#descent

Answer the receiver's maximum extent of characters below the baseline. Positive.


<details>
	<summary>See more</summary>
	
	descent
	"Answer the receiver's maximum extent of characters below the baseline. Positive."

	| answer |
	answer _ descent.
	self isSubscript ifTrue: [ answer _ answer * 2 ].
	^ answer
</details>

#### StrikeFont>>#baseFont: aStrikeFont

<details>
	<summary>See more</summary>
	
	baseFont: aStrikeFont

	baseFont _ aStrikeFont.
	baseFont ifNotNil: [
		derivativeFonts _ nil ]
</details>

#### StrikeFont>>#glyphs

Answer a Form containing the bits representing the characters of the receiver.


<details>
	<summary>See more</summary>
	
	glyphs
	"Answer a Form containing the bits representing the characters of the 
	receiver."

	^glyphs
</details>

#### StrikeFont>>#syntheticSubscript

Build and answer a derivative that is Subscript.


<details>
	<summary>See more</summary>
	
	syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"
</details>

#### StrikeFont>>#fillZeroWidthSlots

Note: this is slow because it copies the font once for every replacement.


<details>
	<summary>See more</summary>
	
	fillZeroWidthSlots
	| nullGlyph |
	"Note: this is slow because it copies the font once for every replacement."

	nullGlyph _ (Form extent: 1@glyphs height) fillGray.
	"Now fill the empty slots with narrow box characters."
	minAscii to: maxAscii do:
		[:i | (self widthOf: (Character numericValue: i)) = 0 ifTrue:
			[self glyphAt: (Character numericValue: i) put: nullGlyph]].

</details>

#### StrikeFont>>#checkCharacter: character

Answer a Character that is within the ascii range of the receiver--either character or the last character in the receiver.


<details>
	<summary>See more</summary>
	
	checkCharacter: character 
	"Answer a Character that is within the ascii range of the receiver--either 
	character or the last character in the receiver."

	| ascii |  
	ascii _ character numericValue.
	((ascii < minAscii) or: [ascii > maxAscii])
			ifTrue: [^maxAscii asCharacter]
			ifFalse:	[^character]

</details>

#### StrikeFont>>#syntheticST80Glyphs

Build and answer a derivative that includes ST-80 glyphs: a left arrow instead of the underscore, and an up arrow instead of the caret.


<details>
	<summary>See more</summary>
	
	syntheticST80Glyphs
	"Build and answer a derivative that includes ST-80 glyphs:
	a left arrow instead of the underscore, and an up arrow instead of the caret."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'ST80';
		perform: Preferences assignmentGlyphSelector.
	^ derivative
</details>

#### StrikeFont>>#ascent

Answer the receiver's maximum extent of characters above the baseline. Positive.


<details>
	<summary>See more</summary>
	
	ascent
	"Answer the receiver's maximum extent of characters above the baseline. Positive."

	self isSuperscript ifTrue: [ ^ ascent * 1.9 ].
	self isSubscript ifTrue: [ ^ ascent * 0.75 ].
	^ascent
</details>

#### StrikeFont>>#emphasized: code

Answer a copy of the receiver with emphasis set to include code.


<details>
	<summary>See more</summary>
	
	emphasized: code
	"Answer a copy of the receiver with emphasis set to include code."
	| derivative addedEmphasis derivedFrom |
	self isBaseFont ifFalse: [ ^self baseFont emphasized: (code bitOr: emphasis) ].
	code = 0 ifTrue: [ ^ self ].

	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	derivative _ derivativeFonts at: code ifPresent: [ :der | ^ der ].		"Already have this style"

	"Dont have it -- derive from another with one with less emphasis"
	addedEmphasis _ 1 bitShift: code highBit - 1.
	derivedFrom _ self emphasized: code - addedEmphasis.

	"Order is Bold(B), Italic(i), Underlined(U), StruckThrough(X), Superscript(Sup), Subscript(Sub), WithST80Glyphs(ST80)"
	derivative _ addedEmphasis caseOf: {
		[ 1 ] -> [ derivedFrom syntheticBold ].
		[ 2 ] -> [ derivedFrom syntheticItalic ].
		[ 4 ] -> [ derivedFrom syntheticUnderlined ].
		[ 8 ] -> [ derivedFrom syntheticStruckThrough ].
		[ 16 ] -> [ derivedFrom syntheticSuperscript ].
		[ 32 ] -> [ derivedFrom syntheticSubscript ].
		[ 64 ] -> [ derivedFrom syntheticST80Glyphs ] }.

	derivative baseFont: self.
	derivative emphasis: code.
	derivativeFonts at: code put: derivative.
	^ derivative
</details>

#### StrikeFont>>#name: aString

Set the receiver's name.


<details>
	<summary>See more</summary>
	
	name: aString
	"Set the receiver's name."

	name _ aString
</details>

#### StrikeFont>>#useLeftArrow

Use left arrow glyph instead of underscore, and up arrow glyph instead of caret


<details>
	<summary>See more</summary>
	
	useLeftArrow
	"Use left arrow glyph instead of underscore, and up arrow glyph instead of caret"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 28.
	characterToGlyphMap at: 95 put: 30
</details>

#### StrikeFont>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self name;
		space;
		print: self lineSpacing;
		nextPut: $)
</details>

#### StrikeFont>>#baseKern

Return the base kern value to be used for all characters. What follows is some 'random' text used to visually adjust this method. HaHbHcHdHeHfHgHhHiHjHkHlHmHnHoHpHqHrHsHtHuHvHwHxHyHzH HAHBHCHDHEHFHGHHHIHJHKHLHMHNHOHPHQHRHSHTHUHVHWHXHYHXZH wok yuyo wuwu vuvu rucu tucu WUWU VUVU huevo HUEVO to k y mate runico ridiculo ARABICO AAAAA TOMATE TUTU tatadalajafua abacadafagahaqawaearatayauaiaoapasadafagahajakalazaxacavabanama kUxUxa q?d?h?l?t?f?j?


<details>
	<summary>See more</summary>
	
	baseKern
	"Return the base kern value to be used for all characters.
	What follows is some 'random' text used to visually adjust this method.
	HaHbHcHdHeHfHgHhHiHjHkHlHmHnHoHpHqHrHsHtHuHvHwHxHyHzH
	HAHBHCHDHEHFHGHHHIHJHKHLHMHNHOHPHQHRHSHTHUHVHWHXHYHXZH
	wok yuyo	wuwu	vuvu	rucu	tucu	WUWU	VUVU	huevo	HUEVO	to
											k y mate	runico ridiculo	ARABICO	AAAAA	TOMATE
	TUTU
	tatadalajafua
	abacadafagahaqawaearatayauaiaoapasadafagahajakalazaxacavabanama
	kUxUxa
	q?d?h?l?t?f?j?"
	
	| italic baseKern |
	italic _ self isItalic.
	
	"Assume synthetic will not affect kerning (i.e. synthetic italics are not used)"
	"After all, DejaVu Sans are the only StrikeFonts used in Cuis..."
"	self familyName = 'DejaVu Sans'
		ifTrue: ["
			baseKern _ (italic or: [ pointSize < 9 ])
				ifTrue: [ -1 ]
				ifFalse: [ 0 ].
			pointSize >= 13 ifTrue: [
				baseKern _ baseKern +1 ].
			pointSize >= 20 ifTrue: [
				baseKern _ baseKern +1 ]"]
		ifFalse: [
			baseKern _ pointSize < 12
				ifTrue: [ -1 ]
				ifFalse: [ 0 ].
			italic ifTrue: [
				baseKern _ baseKern - 1]]".
	
	"If synthetic italic"
	"See makeItalicGlyphs"
	(self isSynthetic and: [ italic and: [ self isBold ]]) ifTrue: [
		baseKern _ baseKern - ((self lineSpacing-1-self ascent+4)//4 max: 0)  
		- (((self ascent-5+4)//4 max: 0)) ].
	^baseKern
</details>

#### StrikeFont>>#makeItalicGlyphs

Make an italic set of glyphs with same widths by skewing left and right. In the process, characters would overlap, so we widen them all first.


<details>
	<summary>See more</summary>
	
	makeItalicGlyphs
	"Make an italic set of glyphs with same widths by skewing left and right.
	In the process, characters would overlap, so we widen them all first.
	"
	| extraWidth newGlyphs newXTable x newX w extraOnLeft |  
	extraOnLeft _ (self lineSpacing-1-self ascent+4)//4 max: 0.
	extraWidth _ ((self ascent-5+4)//4 max: 0) + extraOnLeft.
	newGlyphs _ Form extent: (glyphs width + (maxAscii + 1 - minAscii*extraWidth)) @ glyphs height depth: glyphs depth.
	newGlyphs fillWhite.
	newXTable _ xTable copy.

	"Copy glyphs into newGlyphs with room on left and right for overlap."
	minAscii to: maxAscii+1 do:
		[:ascii | x _ xTable at: ascii+1.  w _ (xTable at: ascii+2) - x.
		newX _ newXTable at: ascii+1.
		newGlyphs copy: ((newX + extraOnLeft) @ 0 extent: w @ glyphs height)
			from: x @ 0 in: glyphs rule: Form over.
		newXTable at: ascii+2 put: newX + w + extraWidth].		
	glyphs _ newGlyphs. 
	xTable _ newXTable.
	"Slide the bitmaps left and right for synthetic italic effect."
	4 to: self ascent-1 by: 4 do:
		[:y | 		"Slide ascenders right..."
		glyphs copy: (`1@0` extent: glyphs width @ (self ascent - y))
			from: `0@0` in: glyphs rule: Form over].
	self ascent to: self lineSpacing-1 by: 4 do:
		[:y | 		"Slide descenders left..."
		glyphs copy: (0@y extent: glyphs width @ glyphs height)
			from: 1@y in: glyphs rule: Form over].
	self isSynthetic: true
</details>

#### StrikeFont>>#syntheticBold

Build and answer a derivative that is Bold.


<details>
	<summary>See more</summary>
	
	syntheticBold
	"Build and answer a derivative that is Bold."

	| derivative |
	derivative _ self copy.
	derivative
		ensureCleanBold;
		name: self name , 'B';
		makeBoldGlyphs.
	^ derivative
</details>

#### StrikeFont>>#widen: char by: delta

<details>
	<summary>See more</summary>
	
	widen: char by: delta
	| newForm |
	^ self alter: char formBlock:  "Make a new form, wider or narrower..."
		[:charForm | newForm _ Form extent: charForm extent + (delta@0).
		charForm displayOn: newForm.  "Copy this image into it"
		newForm]    "and substitute it in the font"
</details>

#### StrikeFont>>#emphasis: code

Set the integer code for emphasis. See senders.


<details>
	<summary>See more</summary>
	
	emphasis: code 
	"Set the integer code for emphasis. See senders."

	emphasis _ code
</details>

#### StrikeFont>>#makeLfInvisible

<details>
	<summary>See more</summary>
	
	makeLfInvisible
	| glyph |
	glyph _ self glyphAt: Character lf.
	glyph fillWhite.
	self glyphAt: Character lf put: glyph
</details>

#### StrikeFont>>#releaseCachedState

<details>
	<summary>See more</summary>
	
	releaseCachedState

	self reset.
</details>

#### StrikeFont>>#makeTabInvisible

<details>
	<summary>See more</summary>
	
	makeTabInvisible
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: (10 < minAscii ifFalse: [10] ifTrue: [maxAscii+1])
</details>

#### StrikeFont>>#makeCrInvisible

<details>
	<summary>See more</summary>
	
	makeCrInvisible
	| glyph |
	glyph _ self glyphAt: Character cr.
	glyph fillWhite.
	self glyphAt: Character cr put: glyph
</details>

#### StrikeFont>>#useRightArrow

Use right arrow glyph instead of underscore, and up arrow glyph instead of caret


<details>
	<summary>See more</summary>
	
	useRightArrow
	"Use right arrow glyph instead of underscore, and up arrow glyph instead of caret"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 29.
	characterToGlyphMap at: 95 put: 30
</details>

#### StrikeFont>>#isSubscript

<details>
	<summary>See more</summary>
	
	isSubscript
	^emphasis allMask: 32
</details>

#### StrikeFont>>#isSuperscript

<details>
	<summary>See more</summary>
	
	isSuperscript
	^emphasis allMask: 16
</details>

#### StrikeFont>>#lineSpacing

Answer the height of the receiver including any additional line gap.


<details>
	<summary>See more</summary>
	
	lineSpacing
	"Answer the height of the receiver including any additional line gap."

	^self ascent + self descent
</details>

#### StrikeFont>>#extendMaxAsciiTo: newMax

Extend the range of this font so that it can display glyphs up to newMax.


<details>
	<summary>See more</summary>
	
	extendMaxAsciiTo: newMax
	"Extend the range of this font so that it can display glyphs up to newMax."

	(newMax+3) <= xTable size ifTrue: [^ self].  "No need to extend."
	xTable size = (maxAscii+3) ifFalse:
		[^ self error: 'This font is not well-formed.'].

	"Insert a bunch of zero-width characters..."
	xTable _ (xTable copyFrom: 1 to: maxAscii+2) ,
			((maxAscii+1 to: newMax) collect: [:i | xTable at: maxAscii+2]) ,
			{ xTable at: maxAscii+3 }.
	maxAscii _ newMax.
	self fillZeroWidthSlots.
	characterToGlyphMap _ nil.
</details>

#### StrikeFont>>#takeGlyphFor: aCharacter from: sourceCharacter in: aFont

Copy characterForm over the glyph for the argument, character.


<details>
	<summary>See more</summary>
	
	takeGlyphFor: aCharacter from: sourceCharacter in: aFont
	"Copy characterForm over the glyph for the argument, character."
	| f r characterForm |
	characterForm _ aFont glyphAt: sourceCharacter.
	r _ 0@(0 + aFont ascent - self ascent) extent: characterForm width @ glyphs height.
	f _ characterForm copy: r.
	self glyphAt: aCharacter put: f
</details>

#### StrikeFont>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	characterToGlyphMap ifNotNil: [
		characterToGlyphMap _ characterToGlyphMap copy ]
</details>

#### StrikeFont>>#isBold

<details>
	<summary>See more</summary>
	
	isBold
	^emphasis allMask: 1
</details>

#### StrikeFont>>#ensureCleanBold

This ensures that all character glyphs have at least one pixel of white space on the right so as not to cause artifacts in neighboring characters in bold or italic.


<details>
	<summary>See more</summary>
	
	ensureCleanBold 
	"This ensures that all character glyphs have at least one pixel of white space on the right
	so as not to cause artifacts in neighboring characters in bold or italic."

	| wider glyph |
	emphasis = 0 ifFalse: [^ self].
	minAscii to: maxAscii do:
		[:i | glyph _ self glyphAt: (Character numericValue: i).
		(glyph copy: (glyph boundingBox topRight - (1@0)
					corner: glyph boundingBox bottomRight)) isAllWhite ifFalse: [
			wider _ Form extent: (glyph width + 1)@glyph height depth: glyph depth.
			glyph depth > 1 ifTrue: [wider fillWhite].
			glyph displayOn: wider.
			self glyphAt: (Character numericValue: i) put: wider]].
"
StrikeFont allInstancesDo: [:f | f ensureCleanBold].
(StrikeFont familyName: 'NewYork' size: 21) ensureCleanBold.
StrikeFont shutDown.  'Flush synthetic fonts'.
"

</details>

#### StrikeFont>>#familyName

<details>
	<summary>See more</summary>
	
	familyName
	| lastSpace n |
	n _ self name.
	lastSpace _ (n findLast: [ :m | m = $  ]).
	^ lastSpace > 0
		ifTrue: [ n copyFrom: 1 to: lastSpace -1 ]
		ifFalse: [ '' ]

</details>

#### StrikeFont>>#buildFromForm: allGlyphs data: data name: aString

<details>
	<summary>See more</summary>
	
	buildFromForm: allGlyphs data: data name: aString

	| x shortUnderscore firstGlyphInFiles |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.

	firstGlyphInFiles _ 257-(data size-3).
	minAscii _ 1.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	4 to: data size do: [ :i |
		x _ (data at: i) asNumber.
		xTable at: i+firstGlyphInFiles+1-4 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.

	self makeCrVisible.

	"Replace glyph for 127 (a box) with a short version of the underscore (used to optionally mark subscript in code)"
	shortUnderscore _ self glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (`0@0` extent: 1@shortUnderscore height).
	self glyphAt: (Character numericValue: 127) put: shortUnderscore
</details>

#### StrikeFont>>#makeControlCharsVisible

<details>
	<summary>See more</summary>
	
	makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: `Color blue`.
	self glyphAt: (Character numericValue: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]
</details>

#### StrikeFont>>#stripHighGlyphs

Remove glyphs for characters above 128


<details>
	<summary>See more</summary>
	
	stripHighGlyphs
	"Remove glyphs for characters above 128"
	| i |

	characterToGlyphMap _ nil.
	maxAscii _ 127.
	
	xTable _ xTable copyFrom: 1 to: maxAscii + 3.
	i _ xTable at: maxAscii + 2.
	xTable at: maxAscii + 3 put: i.
	glyphs _ glyphs copy: (`0@0` extent: i+1@glyphs height).
	maxWidth _ 0.
	2 to: xTable size do: [ :ii |
		maxWidth _ maxWidth max: (xTable at: ii) - (xTable at: ii-1)-1 ].
	self reset
</details>

#### StrikeFont>>#derivativeFonts

<details>
	<summary>See more</summary>
	
	derivativeFonts
	^derivativeFonts
</details>

#### StrikeFont>>#emphasis

Answer the integer code for synthetic bold, italic, underline, and strike-out.


<details>
	<summary>See more</summary>
	
	emphasis
	"Answer the integer code for synthetic bold, italic, underline, and 
	strike-out."

	^emphasis
</details>

#### StrikeFont>>#syntheticUnderlined

Build and answer a derivative that is Underlined.


<details>
	<summary>See more</summary>
	
	syntheticUnderlined
	"Build and answer a derivative that is Underlined."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'U';
		makeUnderlinedGlyphs.
	^ derivative
</details>

#### StrikeFont>>#fixDerivatives

<details>
	<summary>See more</summary>
	
	fixDerivatives
	self isBaseFont
		ifTrue: [
			baseFont _ nil.
			derivativeFonts ifNotNil: [
				derivativeFonts valuesDo: [ :der |
					der ifNotNil: [
						der baseFont: self.
						der fixDerivatives ]]]]
		ifFalse: [
			derivativeFonts _ nil ].
</details>

#### StrikeFont>>#name

Answer the receiver's name.


<details>
	<summary>See more</summary>
	
	name
	"Answer the receiver's name."

	^name ifNil: ['(unnamed)']
</details>

#### StrikeFont>>#useUnderscore

Sets underscore and caret glyphs for chars 95 and 94. ASCII standard glyphs


<details>
	<summary>See more</summary>
	
	useUnderscore
	"Sets underscore and caret glyphs for chars 95 and 94. 
	ASCII standard glyphs"
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 95.
	characterToGlyphMap at: 95 put: 94
</details>

#### StrikeFont>>#syntheticItalic

Build and answer a derivative that is Italic.


<details>
	<summary>See more</summary>
	
	syntheticItalic
	"Build and answer a derivative that is Italic."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'I';
		makeItalicGlyphs.
	^ derivative
</details>

#### StrikeFont>>#pointSize: anInteger

<details>
	<summary>See more</summary>
	
	pointSize: anInteger
	pointSize _ anInteger
</details>

#### StrikeFont>>#isSynthetic: aBoolean

<details>
	<summary>See more</summary>
	
	isSynthetic: aBoolean
	type _ aBoolean ifTrue: [3] ifFalse: [0]
</details>

#### StrikeFont>>#pointSize

<details>
	<summary>See more</summary>
	
	pointSize
	^ pointSize
</details>

#### StrikeFont>>#baseFont

<details>
	<summary>See more</summary>
	
	baseFont
	^self isBaseFont
		ifTrue: [ self ]
		ifFalse: [ baseFont ]
</details>

#### StrikeFont>>#maxWidth

Answer the integer that is the width of the receiver's widest character.


<details>
	<summary>See more</summary>
	
	maxWidth
	"Answer the integer that is the width of the receiver's widest character."

	^maxWidth
</details>

#### StrikeFont>>#alter: char formBlock: formBlock

<details>
	<summary>See more</summary>
	
	alter: char formBlock: formBlock
	self
		glyphAt: char 
		put: (formBlock value: (self glyphAt: char))
</details>

#### StrikeFont>>#onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color

Answer last affected pixel position. Answer nil if nothing was done.


<details>
	<summary>See more</summary>
	
	onBitBltCanvasEngine: engine displayString: aString from: firstIndex to: lastIndex at: p color: color
	"Answer last affected pixel position.
	Answer nil if nothing was done."

	^ engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p
		strikeFont: self
		color: color
</details>

#### StrikeFont>>#makeLfVisible

<details>
	<summary>See more</summary>
	
	makeLfVisible
	| glyph |
	glyph _ self glyphAt: (Character numericValue: 163).
	glyph border: glyph boundingBox width: 1 fillColor: `Color blue`.
"	glyph _ glyph reverse."
	self glyphAt: Character lf put: glyph
</details>

#### StrikeFont>>#isItalic

<details>
	<summary>See more</summary>
	
	isItalic
	^emphasis allMask: 2
</details>

#### StrikeFont>>#glyphAt: character put: characterForm

Copy characterForm over the glyph for the argument, character.


<details>
	<summary>See more</summary>
	
	glyphAt: character put: characterForm
	"Copy characterForm over the glyph for the argument, character."
	| ascii leftX rightX widthDif newGlyphs |
	ascii _ character numericValue.
	ascii < minAscii ifTrue: [^ self error: 'Cant store characters below min ascii'].
	ascii > maxAscii ifTrue:
		[(self confirm:
'This font does not accomodate ascii values higher than ' , maxAscii printString , '.
Do you wish to extend it permanently to handle values up to ' , ascii printString)
			ifTrue: [self extendMaxAsciiTo: ascii]
			ifFalse: [^ self error: 'No change made']].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	widthDif _ characterForm width - (rightX - leftX).
	widthDif ~= 0 ifTrue:
		["Make new glyphs with more or less space for this char"
		newGlyphs _ Form extent: (glyphs width + widthDif) @ glyphs height depth: glyphs depth.
		newGlyphs copy: (`0@0` corner: leftX@glyphs height)
			from: `0@0` in: glyphs rule: Form over.
		newGlyphs copy: ((rightX+widthDif)@0 corner: newGlyphs width@glyphs height)
			from: rightX@0 in: glyphs rule: Form over.
		glyphs _ newGlyphs.
		"adjust further entries on xTable"
		xTable _ xTable copy.
		ascii+2 to: xTable size
			do: [:i | xTable at: i put: (xTable at: i) + widthDif]].
	glyphs copy: (leftX @ 0 extent: characterForm extent)
		from: 0@0 in: characterForm rule: Form over
"
| f |  f _ AbstractFont default.
f glyphAt: $  put: (Form extent: (f widthOf: $ )+10@f lineSpacing)
"
</details>

#### StrikeFont>>#bonk: glyphForm with: bonkForm

Bonking means to run through the glyphs clearing out black pixels between characters to prevent them from straying into an adjacent character as a result of, eg, bolding or italicizing


<details>
	<summary>See more</summary>
	
	bonk: glyphForm with: bonkForm
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"
	"Uses the bonkForm to erase at every character boundary in glyphs."
	| bb offset |
	offset _ bonkForm offset x.
	bb _ BitBlt toForm: glyphForm.
	bb sourceForm: bonkForm; sourceRect: bonkForm boundingBox;
		combinationRule: Form erase; destY: 0.
	1 to: xTable size-1 do: [:i | bb destX: (xTable at: i) + offset; copyBits].

</details>

#### StrikeFont>>#derivativeFont: aStrikeFontOrNil at: index

Store aStrikeFontOrNil at index If arg is nil, then remove font at index. But if index = 0, then remove all derivatives.


<details>
	<summary>See more</summary>
	
	derivativeFont: aStrikeFontOrNil at: index
	"Store aStrikeFontOrNil at index
	If arg is nil, then remove font at index. But if index = 0, then remove all derivatives."

	(aStrikeFontOrNil isNil and: [ index = 0 ]) 
		ifTrue: [
			derivativeFonts _ nil.
			^ self].

	self isBaseFont ifFalse: [ 
		derivativeFonts _ nil.
		self error: 'Derivative fonts can not have derivatives' ].
	
	derivativeFonts ifNil: [ derivativeFonts _ Dictionary new ].
	aStrikeFontOrNil
		ifNil: [ derivativeFonts removeKey: index ]
		ifNotNil: [
			derivativeFonts at: index put: aStrikeFontOrNil.
			aStrikeFontOrNil baseFont: self ]
</details>

#### StrikeFont>>#useShortUnderscore

<details>
	<summary>See more</summary>
	
	useShortUnderscore
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 127
</details>

#### StrikeFont>>#isStruckThrough

<details>
	<summary>See more</summary>
	
	isStruckThrough
	^emphasis allMask: 8
</details>

#### StrikeFont>>#xTable

Answer an Array of the left x-coordinate of characters in glyphs.


<details>
	<summary>See more</summary>
	
	xTable
	"Answer an Array of the left x-coordinate of characters in glyphs."

	^xTable
</details>

#### StrikeFont>>#syntheticStruckThrough

Build and answer a derivative that is StruckThrough.


<details>
	<summary>See more</summary>
	
	syntheticStruckThrough
	"Build and answer a derivative that is StruckThrough."

	| derivative |
	derivative _ self copy.
	derivative
		name: self name , 'X';
		makeStruckThroughGlyphs.
	^ derivative
</details>

#### StrikeFont>>#widthOf: aCharacter

Answer the width of the argument as a character in the receiver.


<details>
	<summary>See more</summary>
	
	widthOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."

	| ascii |
	ascii _ characterToGlyphMap
		ifNil: [ aCharacter numericValue ]
		ifNotNil: [ characterToGlyphMap at: aCharacter numericValue + 1 ].
	(ascii >= minAscii and: [ ascii <= maxAscii ]) ifFalse: [ ascii _ maxAscii + 1 ].
	^ (xTable at: ascii + 2) - (xTable at: ascii + 1) + self baseKern
</details>

#### StrikeFont>>#syntheticSuperscript

Build and answer a derivative that is Superscript.


<details>
	<summary>See more</summary>
	
	syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"
</details>

#### StrikeFont>>#isSynthetic

<details>
	<summary>See more</summary>
	
	isSynthetic
	^type = 3
</details>

#### StrikeFont>>#makeBoldGlyphs

Make a bold set of glyphs with same widths by ORing 1 bit to the right (requires at least 1 pixel of intercharacter space)


<details>
	<summary>See more</summary>
	
	makeBoldGlyphs
	"Make a bold set of glyphs with same widths by ORing 1 bit to the right
		(requires at least 1 pixel of intercharacter space)"
	| g bonkForm |
	g _ glyphs copy.
	bonkForm _ (Form extent: 1@16) fillBlack offset: -1@0.
	self bonk: g with: bonkForm.
	glyphs depth = 1 ifTrue: [
		g copyBits: g boundingBox from: g at: (1@0)
			clippingBox: g boundingBox rule: Form under ]
		ifFalse: [
			0 to: g width - 2 do: [ :x | 0 to: g height-1 do: [ :y |
				(glyphs colorAt:  x@y) = `Color white` ifFalse: [
					g colorAt: x+1@y put: 
						((glyphs colorAt: x+1@y) = `Color white`
							ifTrue: [glyphs colorAt:  x@y]
							ifFalse: [`Color black`])]]]].
	glyphs _ g.
	self isSynthetic: true
</details>

#### StrikeFont>>#characterToGlyphMap

Return the character to glyph mapping table. If the table is not provided the character scanner will query the font directly for the width of each individual character.


<details>
	<summary>See more</summary>
	
	characterToGlyphMap
	^characterToGlyphMap ifNil:[characterToGlyphMap _ self createCharacterToGlyphMap].
</details>

#### StrikeFont>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a textual reference instead. Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:aroundPointSize:
		args: (Array with: self familyName with: self pointSize)
</details>

#### StrikeFont>>#superscript

Answer an integer that is the further vertical offset relative to the baseline for positioning characters as superscripts.


<details>
	<summary>See more</summary>
	
	superscript
	"Answer an integer that is the further vertical offset relative to the 
	baseline for positioning characters as superscripts."

	^superscript
</details>

#### StrikeFont>>#createCharacterToGlyphMap

Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes.


<details>
	<summary>See more</summary>
	
	createCharacterToGlyphMap
	"Private. Create the character to glyph mapping for a font that didn't have any before. This is basically equivalent to what the former setStopCondition did, only based on indexes."
	| map |
	map _ Array new: 256.
	0 to: minAscii - 1 do:[:i| map at: i + 1 put: maxAscii + 1].
	minAscii to: maxAscii do:[:i| map at: i + 1 put: i].
	maxAscii + 1 to: 255 do:[:i| map at: i + 1 put: maxAscii + 1].
	^map
</details>

#### StrikeFont>>#characterToGlyphMap: anArray

<details>
	<summary>See more</summary>
	
	characterToGlyphMap: anArray
	characterToGlyphMap _ anArray.
</details>

#### StrikeFont>>#makeTabVisible

<details>
	<summary>See more</summary>
	
	makeTabVisible
	self characterToGlyphMap.
	characterToGlyphMap at: 10 put: 172
</details>

#### StrikeFont>>#glyphAt: character

Answer a Form copied out of the glyphs for the argument, character.


<details>
	<summary>See more</summary>
	
	glyphAt: character 
	"Answer a Form copied out of the glyphs for the argument, character."
	| ascii leftX rightX |
	ascii _ character numericValue.
	(ascii between: minAscii and: maxAscii) ifFalse: [ascii _ maxAscii + 1].
	leftX _ xTable at: ascii + 1.
	rightX _ xTable at: ascii + 2.
	^ glyphs copy: (leftX @ 0 corner: rightX @ self lineSpacing)
</details>

#### StrikeFont>>#isUnderlined

<details>
	<summary>See more</summary>
	
	isUnderlined
	^emphasis allMask: 4
</details>

#### StrikeFont>>#setGlyphsDepthAtMost: aNumber

<details>
	<summary>See more</summary>
	
	setGlyphsDepthAtMost: aNumber
	glyphs depth > aNumber ifTrue: [
		glyphs _ glyphs asFormOfDepth: aNumber ]
</details>

#### StrikeFont>>#subscript

Answer an integer that is the further vertical offset relative to the baseline for positioning characters as subscripts.


<details>
	<summary>See more</summary>
	
	subscript
	"Answer an integer that is the further vertical offset relative to the 
	baseline for positioning characters as subscripts."

	^subscript
</details>

#### StrikeFont>>#reset

Reset the cache of derivative emphasized fonts StrikeFont allInstancesDo: [ :f | f reset ]


<details>
	<summary>See more</summary>
	
	reset
	"Reset the cache of derivative emphasized fonts
	StrikeFont allInstancesDo: [ :f | f reset ]
	"
	
	| newDict |
	derivativeFonts ifNotNil: [
		newDict _ Dictionary new.
		"all after 16 are synthetic, and can be recreated on demand"
		1 to: 15 do: [ :i |
			derivativeFonts at: i ifPresent: [ :der |
				newDict at: i put: der ]].
		derivativeFonts _ newDict ]
</details>

#### StrikeFont>>#makeCrVisible

<details>
	<summary>See more</summary>
	
	makeCrVisible
	| glyph |
	glyph _ self glyphAt: (Character numericValue: 182).
	glyph border: glyph boundingBox width: 1 fillColor: `Color blue`.
"	glyph _ glyph reverse."
	self glyphAt: Character cr put: glyph
</details>

#### StrikeFont>>#makeStruckThroughGlyphs

Make a struckThrough set of glyphs with same widths


<details>
	<summary>See more</summary>
	
	makeStruckThroughGlyphs
	"Make a struckThrough set of glyphs with same widths"
	| g |
	g _ glyphs copy.
	g fillBlack: (0 @ (self ascent - (self ascent//3)) extent: g width @ 1).
	glyphs _ g.
	self isSynthetic: true

</details>

#### StrikeFont>>#makeUnderlinedGlyphs

Make an underlined set of glyphs with same widths


<details>
	<summary>See more</summary>
	
	makeUnderlinedGlyphs
	"Make an underlined set of glyphs with same widths"
	| g |
	g _ glyphs copy.
	g fillBlack: (0 @ (self ascent+1) extent: g width @ 1).
	glyphs _ g.
	self isSynthetic: true

</details>

## StrikeFontFamily

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### StrikeFontFamily>>#atPointSize: aNumber put: aFontAndSize

aFontAndSize must have emphasis = 0, i.e. it is a base font


<details>
	<summary>See more</summary>
	
	atPointSize: aNumber put: aFontAndSize
	"aFontAndSize must have emphasis = 0, i.e. it is a base font"
	baseFontBySizes at: aNumber put: aFontAndSize 
</details>

