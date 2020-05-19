## EmptyLine

An EmptyLine is a special line that has actually no text. It is used to place the cursor below the last line if it ends with cr. It is also used in case of empty text.

### Methods
#### EmptyLine>>#isEmptyLine

<details>
	<summary>See more</summary>
	
	isEmptyLine
	^true
</details>

#### EmptyLine>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	internalSpaces _ 0.
	paddingWidth _ 0.
	isFirstLine _ true
</details>

#### EmptyLine>>#paragraphStyle: aParagraphtStyle

<details>
	<summary>See more</summary>
	
	paragraphStyle: aParagraphtStyle

	paragraphStyle _ aParagraphtStyle
</details>

#### EmptyLine>>#paragraphStyle

<details>
	<summary>See more</summary>
	
	paragraphStyle

	^paragraphStyle
</details>

## TextAnchor

TextAnchors support anchoring of images in text. A TextAnchor exists as an attribute of text emphasis, and it gets control like a FontReference, through the emphasizeScanner: message. Depending on whether its anchoredMorph is a Morph or a Form, it repositions the morph, or displays the form respectively. The coordination between composition, display and selection can best be understood by browsing the various implementations of placeEmbeddedObject:. In the morphic world, simply embed any morph in text. In the old world, you can create an image reference using code such as the following. "A Form" ('Hello', (Text withForm: (EllipseMorph new imageForm: 2)), 'world') edit ('Hello', (Text withForm: (EllipseMorph new imageForm: 32)), 'world') edit "A Morph" ((Text withAll: 'foo') , (Text string: '*' asString attribute: (TextAnchor new anchoredFormOrMorph: EllipseMorph new)) , (Text withAll: 'bar')) edit In this case you select a piece of the screen, and it gets anchored to a one-character text in the editor's past buffer. If you then paste into some other text, you will see the image as an embedded image.

### Methods
#### TextAnchor>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^anchoredFormOrMorph hash
</details>

#### TextAnchor>>#mayBeExtended

A textAnchor is designed to modify only a single character, and therefore must not be extended by the ParagraphEditor's emphasisHere facility


<details>
	<summary>See more</summary>
	
	mayBeExtended
	"A textAnchor is designed to modify only a single character, and therefore must not be extended by the ParagraphEditor's emphasisHere facility"
	^ false
</details>

#### TextAnchor>>#anchoredFormOrMorph: aFormOrMorph

<details>
	<summary>See more</summary>
	
	anchoredFormOrMorph: aFormOrMorph
	anchoredFormOrMorph _ aFormOrMorph
</details>

#### TextAnchor>>#isForFormatting

Answer false for those attributes that are not for text formatting. This is, those that are not considered in #withFormattingAttributeValues:do:


<details>
	<summary>See more</summary>
	
	isForFormatting
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^false
</details>

#### TextAnchor>>#anchoredFormOrMorph

If one hides here, return it


<details>
	<summary>See more</summary>
	
	anchoredFormOrMorph
	^anchoredFormOrMorph
</details>

#### TextAnchor>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy

	anchoredFormOrMorph _ anchoredFormOrMorph copy
</details>

#### TextAnchor>>#canBeJoinedWith: aTextAttribute

Never join in RunArray. Not even when self == aTextAttribute!


<details>
	<summary>See more</summary>
	
	canBeJoinedWith: aTextAttribute
	"Never join in RunArray. Not even when self == aTextAttribute!"
	^false
</details>

#### TextAnchor>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other anchoredFormOrMorph = anchoredFormOrMorph]
</details>

## TextComposer

Aux class for TextComposition. My instances are usually transient, and therefore don't need a TextModel, but just a Text with the current contents.

### Methods
#### TextComposer>>#checkIfReadyToSlide

Check whether we are now in sync with previously composed lines


<details>
	<summary>See more</summary>
	
	checkIfReadyToSlide

	"Check whether we are now in sync with previously composed lines"

	(possibleSlide and: [currCharIndex > stopCharIndex]) ifFalse: [^self].

	[prevIndex < prevLines size
		and: [(prevLines at: prevIndex) first < (currCharIndex - deltaCharIndex)]]
			whileTrue: [prevIndex _ prevIndex + 1].

	(prevLines at: prevIndex) first = (currCharIndex - deltaCharIndex) ifTrue: [
		"Yes -- next line will have same start as prior line."
		prevIndex _ prevIndex - 1.
		possibleSlide _ false.
		nowSliding _ true
	] ifFalse: [
		prevIndex = prevLines size ifTrue: [
			"Weve reached the end of prevLines, so no use to keep looking for lines to slide."
			possibleSlide _ false
		]
	]
</details>

#### TextComposer>>#rectanglesAt: y

<details>
	<summary>See more</summary>
	
	rectanglesAt: y
	(y+1) > extentForComposing y ifTrue: [^ Array new].
	^ Array with: (0 @ y corner: extentForComposing x @ (y+1))
</details>

#### TextComposer>>#composeOneLine

<details>
	<summary>See more</summary>
	
	composeOneLine
	| rectangles |
	rectangles := self rectanglesAt: currentY.
	rectangles notEmpty 
		ifTrue: [(self composeAllRectangles: rectangles) ifNil: [^nil]]
		ifFalse: [currentY := currentY + 1].
	self checkIfReadyToSlide
</details>

#### TextComposer>>#composeAllLines

<details>
	<summary>See more</summary>
	
	composeAllLines

	[currCharIndex <= theText size and: [
			currentY < extentForComposing y ]] whileTrue: [

		nowSliding ifTrue: [
			self slideOneLineDown ifNil: [^nil].
		] ifFalse: [
			self composeOneLine ifNil: [^nil].
		]
	].

</details>

#### TextComposer>>#composeEachRectangleIn: rectangles

<details>
	<summary>See more</summary>
	
	composeEachRectangleIn: rectangles 
	| myLine lastChar |
	1 to: rectangles size
		do: 
			[:i | 
			currCharIndex <= theText size ifFalse: [^false].
			myLine _ scanner 
						composeFrom: currCharIndex
						inRectangle: (rectangles at: i)
						firstLine: isFirstLine
						leftSide: i = 1
						rightSide: i = rectangles size.
			lines addLast: myLine.
			actualHeight := actualHeight max: myLine lineHeight.	"includes font changes"
			currCharIndex := myLine last + 1.
			lastChar _ theText at: myLine last.
			lastChar isLineSeparator ifTrue: [^#newLine]].
	^false
</details>

#### TextComposer>>#composeAllRectangles: rectangles

<details>
	<summary>See more</summary>
	
	composeAllRectangles: rectangles

	| charIndexBeforeLine numberOfLinesBefore |

	actualHeight _ 1.
	charIndexBeforeLine _ currCharIndex.
	numberOfLinesBefore _ lines size.
	self composeEachRectangleIn: rectangles.

	currentY _ currentY + actualHeight.
	currentY > extentForComposing y ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex _ charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [ lines removeLast ].
		^ self ].
	
	"It's OK -- the line still fits."
	maxRightX _ maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline ].

	anythingComposed _ true.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator]
</details>

#### TextComposer>>#editor: anEditor

<details>
	<summary>See more</summary>
	
	editor: anEditor
	editor _ anEditor
</details>

#### TextComposer>>#slideOneLineDown

Having detected the end of rippling recoposition, we are only sliding old lines


<details>
	<summary>See more</summary>
	
	slideOneLineDown
	"Having detected the end of rippling recoposition, we are only sliding old lines"

	| priorLine |
	prevIndex < prevLines size 
		ifFalse: 
			["There are no more prevLines to slide."

			^nowSliding := possibleSlide := false].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex) slideIndexBy: deltaCharIndex
				andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1
</details>

#### TextComposer>>#addEmptyTrailingLine: isANewParagraph

The line to add is usually the first line of a new paragraph (if last char in text was newLine), but it can be a new line in same paragraph (if enough spaces ended last line).


<details>
	<summary>See more</summary>
	
	addEmptyTrailingLine: isANewParagraph
	"The line to add is usually the first line of a new paragraph (if last char in text was newLine),
	but it can be a new line in same paragraph (if enough spaces ended last line)."
	| ts f h bs r lm rm w a leftMarginForAlignment s |
	s _ theText size+1.
	f _ editor
		ifNotNil: [ editor lastFont ]
		ifNil: [ theText fontAt: s default: self defaultFont ].
	ts _ editor
		ifNotNil: [ editor lastParagraphStyleOrNil ]
		ifNil: [ theText paragraphStyleOrNilAt: s].

	h _ f lineSpacing.
	bs _ f ascent.
	lm _ 0.
	rm _ 0.
	w _ extentForComposing x.
	a _ 0.
	ts ifNotNil: [
		isANewParagraph ifTrue: [
			h _ h + ts spaceBefore.
			bs _ bs + ts spaceBefore ].
		lm _ ((isANewParagraph and: [ ts isListStyle not ])
			ifTrue: [ ts firstIndent ]
			ifFalse: [ ts restIndent ]).
		rm _ ts rightIndent.
		a _ ts alignment ].
	
	leftMarginForAlignment _ a = CharacterScanner rightFlushCode
		ifTrue: [ w - rm]
		ifFalse: [
			a = CharacterScanner centeredCode
				ifTrue: [ (w - rm - lm) //2 + lm]
				ifFalse: [ lm ]].
	r _ leftMarginForAlignment @ currentY extent: 0@h.

	lines addLast: (
		EmptyLine new
			firstIndex: s lastIndex: s - 1;
			rectangle: r;
			lineHeight: h baseline: bs;
			paragraphStyle: ts)
</details>

#### TextComposer>>#defaultFont

<details>
	<summary>See more</summary>
	
	defaultFont
	^editor ifNil: [ AbstractFont default ] ifNotNil: [ editor defaultFont ]
</details>

#### TextComposer>>#composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

<details>
	<summary>See more</summary>
	
	composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

	lines _ argLinesCollection.
	theText _ argText.
	extentForComposing _ argExtentForComposing.
	deltaCharIndex _ argDelta.
	currCharIndex _ startCharIndex _ argStart.
	stopCharIndex _ argStop.
	prevLines _ argPriorLines.
	currentY _ argStartY.
	maxRightX _ 0.
	possibleSlide _ stopCharIndex < theText size.
	nowSliding _ false.
	prevIndex _ 1.
	scanner _ CompositionScanner new text: theText.
	scanner defaultFont: self defaultFont.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed _ false.
	self composeAllLines.
	(anythingComposed not or: [ theText last isLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine: true ]
		ifFalse: [
			scanner lastLineBreakingSpace = theText size ifTrue: [
				self addEmptyTrailingLine: false ]].
	^ {lines asArray. maxRightX}


</details>

## TextComposition

A TextComposition represents text that has been laid out, or composed, in some container. (This class was formerly known as Paragraph). text A Text with encoded per-character emphasis. lines An Array of TextLines comprising the final layout of the text after it has been composed within its container. Lines are ordered vertically. However, for a given y, there may be several lines in left to right order. Lines must never be empty, even if text is empty.

### Methods
#### TextComposition>>#recomposeFrom: startArg to: stopArg delta: delta

Recompose this text. The altered portion is between start and stop. Recomposition may continue to the end of the text, due to a ripple effect. Delta is the amount by which the current text is longer than it was when its current lines were composed. Expand the requested interval to include whole paragraphs because there could be paragraph attributes.


<details>
	<summary>See more</summary>
	
	recomposeFrom: startArg to: stopArg delta: delta
	"Recompose this text.  The altered portion is between start and stop.
	Recomposition may continue to the end of the text, due to a ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed.
	Expand the requested interval to include whole paragraphs because there could be paragraph attributes."
	| intervalToFix start stop startLine newLines partialMaxRightX |
	"Have to recompose line above in case a word-break was affected."
	intervalToFix _ model actualContents encompassParagraph: (startArg to: stopArg).
	start _ intervalToFix first.
	stop _ intervalToFix last.
	startLine _ (self lineIndexFor: start) - 1 max: 1.
	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]
		whileTrue: [startLine _ startLine - 1].  "Find leftmost of line pieces"
	newLines _ OrderedCollection new: lines size + 1.
	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].
	partialMaxRightX _ self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top.
	"Partial recomposition computes actual right border only of recompsed text,
	so, it could make maxRightX larger but not smaller.
	This means that if the longest line gets shortened, we won't know, and maxRightX will be erroneously large"
	maxRightX _ maxRightX max: partialMaxRightX
</details>

#### TextComposition>>#fastFindFirstLineSuchThat: lineBlock

Perform a binary search of the lines array and return the index of the first element for which lineBlock evaluates as true. This assumes the condition is one that goes from false to true for increasing line numbers (as, eg, yval > somey or start char > somex). If lineBlock is not true for any element, return size+1.


<details>
	<summary>See more</summary>
	
	fastFindFirstLineSuchThat: lineBlock
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."

	^lines
		findBinaryIndex: [ :each | 
			(lineBlock value: each)
				ifTrue: [ -1 ]
				ifFalse: [ 1 ] ]
		do: [ :found | found ]
		ifNone: [ :lower :upper | upper ]
</details>

#### TextComposition>>#extentForComposing: aPoint

<details>
	<summary>See more</summary>
	
	extentForComposing: aPoint
	extentForComposing _ aPoint
</details>

#### TextComposition>>#selectionRects

Return an array of rectangles representing the selection regions.


<details>
	<summary>See more</summary>
	
	selectionRects
	"Return an array of rectangles representing the selection regions."
	^ Array streamContents: [ :strm |
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
			self addSelectionRectsFrom: startBlock to: stopBlock to: strm ]]
</details>

#### TextComposition>>#addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: aStream

Return an array of rectangles representing the area between the two character blocks given as arguments.


<details>
	<summary>See more</summary>
	
	addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: aStream
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w line |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 _ characterBlock1.  cb2 _ characterBlock2]
		ifFalse: [cb2 _ characterBlock1.  cb1 _ characterBlock2].
	cb1 = cb2 ifTrue: [
		w _ 6.
		^ aStream nextPut: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 _ self lineIndexFor: cb1 stringIndex.
	line2 _ self lineIndexFor: cb2 stringIndex.
	line1 = line2 ifTrue: [
		^ aStream nextPut: (cb1 topLeft corner: cb2 bottomRight)].
	rects _ OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		line _ lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect _ rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].
	aStream nextPutAll: rects.
	aStream nextPut: ((lines at: line2) topLeft corner: cb2 bottomLeft)
</details>

#### TextComposition>>#lines

<details>
	<summary>See more</summary>
	
	lines
	^ lines
</details>

#### TextComposition>>#lastLine

<details>
	<summary>See more</summary>
	
	lastLine
	^lines last
</details>

#### TextComposition>>#numberOfLines

<details>
	<summary>See more</summary>
	
	numberOfLines

	^lines size
</details>

#### TextComposition>>#lineIndexFor: characterIndex

Answer the index of the line in which to select the character at index.


<details>
	<summary>See more</summary>
	
	lineIndexFor: characterIndex
	"Answer the index of the line in which to select the character at index."
	^ (self fastFindFirstLineSuchThat: [:line | line first > characterIndex]) - 1 max: 1
</details>

#### TextComposition>>#composeAll

Full recomposition computes actual right border of text, only limited by longest line and (extentForComposing x)


<details>
	<summary>See more</summary>
	
	composeAll
	"Full recomposition computes actual right border of text,
	only limited by longest line and (extentForComposing x)"
	maxRightX _ self
		composeLinesFrom: 1
		to: model textSize
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: 0
</details>

#### TextComposition>>#composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY

While the section from start to stop has changed, composition may ripple all the way to the end of the text. However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values


<details>
	<summary>See more</summary>
	
	composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY 
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult composer |
	composer _ TextComposer new.
	editor ifNotNil: [
		composer editor: editor ].
	newResult _ composer
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				text: model actualContents
				extentForComposing: extentForComposing.
	lines _ newResult first asArray.
	^newResult second "right border of recomposed lines (bounded by extentForComposing x)"
</details>

#### TextComposition>>#characterBlockAtPoint: aPoint

Answer a CharacterBlock for the character in the text at aPoint.


<details>
	<summary>See more</summary>
	
	characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line _ lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint index: nil
		in: line
</details>

#### TextComposition>>#selectionRectsFrom: characterBlock1 to: characterBlock2

Return an array of rectangles representing the area between the two character blocks given as arguments.


<details>
	<summary>See more</summary>
	
	selectionRectsFrom: characterBlock1 to: characterBlock2 
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	^ Array streamContents: [ :strm |
		self addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: strm ]
</details>

#### TextComposition>>#lastTextCursorRect

Warning: Could be not updated, it is the one for when the text cursor was last drawn.


<details>
	<summary>See more</summary>
	
	lastTextCursorRect
	"Warning: Could be not updated, it is the one for when the text cursor was last drawn."
	^lastTextCursorRect
</details>

#### TextComposition>>#usedWidth

<details>
	<summary>See more</summary>
	
	usedWidth
	^ maxRightX
</details>

#### TextComposition>>#characterBlockForIndex: index

Answer a CharacterBlock for the character in text at index.


<details>
	<summary>See more</summary>
	
	characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line _ lines at: (self lineIndexFor: index).
	t _ model actualContents.
	^ (CharacterBlockScanner new text: t)
		defaultFont: self defaultFont;
		characterBlockAtPoint: nil index: ((index max: line first) min: t size+1)
		in: line
</details>

#### TextComposition>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	showTextCursor _ false.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()
</details>

#### TextComposition>>#textComposed

Not named #text just to avoid false polyphormism


<details>
	<summary>See more</summary>
	
	textComposed
	"Not named #text just to avoid false polyphormism"
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents
</details>

#### TextComposition>>#clickAt: clickPoint

Give sensitive text a chance to fire. Display flash: (100@100 extent: 100@100).


<details>
	<summary>See more</summary>
	
	clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue: [
				(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
				]]].
	^ action
</details>

#### TextComposition>>#editor: anEditor

Can be nil. But if not nil, must be the same editor used in any TextMorph that references us.


<details>
	<summary>See more</summary>
	
	editor: anEditor
	"Can be nil.
	But if not nil, must be the same editor used in any TextMorph that references us."
	editor _ anEditor
</details>

#### TextComposition>>#displaySelectionInLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc

textTopLeft is relative to the morph currently being drawn


<details>
	<summary>See more</summary>
	
	displaySelectionInLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc

	"textTopLeft is relative to the morph currently being drawn"
	selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
		self
			displaySelectionStartBlock: startBlock
			stopBlock: stopBlock
			InLine: line
			on: aCanvas
			textTopLeft: textTopLeft
			selectionColor: sc ]
</details>

#### TextComposition>>#showTextCursor

<details>
	<summary>See more</summary>
	
	showTextCursor
	^showTextCursor
</details>

#### TextComposition>>#indentationOfLineIndex: lineIndex ifBlank: aBlock

Answer the number of leading tabs in the line at lineIndex. If there are no visible characters, pass the number of tabs to aBlock and return its value. If the line is word-wrap overflow, back up a line and recur.


<details>
	<summary>See more</summary>
	
	indentationOfLineIndex: lineIndex ifBlank: aBlock
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last str |
	str _ model actualContents string.
	arrayIndex _ lineIndex.
	[
		first _ (lines at: arrayIndex) first.
		 first > 1 and: [(str at: first - 1) isLineSeparator not ] ] whileTrue: [ "word wrap"
			arrayIndex _ arrayIndex - 1].
	last _ (lines at: arrayIndex) last.
	
	^(str copyFrom: first to: last) indentationIfBlank: aBlock
</details>

#### TextComposition>>#showTextCursor: aBool

<details>
	<summary>See more</summary>
	
	showTextCursor: aBool
	showTextCursor _ aBool
</details>

#### TextComposition>>#displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc

textTopLeft is relative to the morph currently being drawn


<details>
	<summary>See more</summary>
	
	displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc
	"textTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx textCursorFont t b textCursorAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show text cursor on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showTextCursor ifTrue: [
				leftX _ textTopLeft x + startBlock left.
				idx _ startBlock stringIndex.
				textCursorAttributes _ editor ifNotNil: [ editor currentAttributes ].
				textCursorFont _ textCursorAttributes
					ifNil: [ model actualContents fontAt: idx default: self defaultFont ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes default: self defaultFont ].
				b _ textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t _ textTopLeft y + line top + line baseline - textCursorFont ascent.
				lastTextCursorRect _ nil.
				self
					displayTextCursorAtX: leftX
					top: t
					bottom: b
					emphasis: textCursorFont emphasis
					on: aCanvas
					textLeft: textTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastTextCursorRect _ nil.
			leftX _  textTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  textTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  textTopLeft y) corner: rightX @ (line bottom +  textTopLeft y))
				color: sc ].	"Selection begins on line below"
</details>

#### TextComposition>>#lineIndexForPoint: aPoint

Answer the index of the line in which to select the character nearest to aPoint.


<details>
	<summary>See more</summary>
	
	lineIndexForPoint: aPoint
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py _ aPoint y truncated.

	"Find the first line at this y-value"
	i _ (self fastFindFirstLineSuchThat: [ :line | line bottom > py]) min: lines size.

	"Now find the first line at this x-value"
	[ i < lines size and: [ (lines at: i+1) top = (lines at: i) top
				and: [ aPoint x >= (lines at: i+1) left ]]]
		whileTrue: [ i _ i + 1 ].
	^ i
</details>

#### TextComposition>>#setModel: aTextModel

<details>
	<summary>See more</summary>
	
	setModel: aTextModel
	model _ aTextModel
</details>

#### TextComposition>>#selectionStartBlocks: startBlocks selectionStopBlocks: stopBlockBlocks

<details>
	<summary>See more</summary>
	
	selectionStartBlocks: startBlocks selectionStopBlocks: stopBlockBlocks
	selectionStartBlocks _ startBlocks.
	selectionStopBlocks _ stopBlockBlocks.
</details>

#### TextComposition>>#usedExtent

<details>
	<summary>See more</summary>
	
	usedExtent
	^ maxRightX @ self usedHeight
</details>

#### TextComposition>>#usedHeight

<details>
	<summary>See more</summary>
	
	usedHeight
	^ lines last bottom - lines first top
</details>

#### TextComposition>>#displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft

x, top, bottom, textLeft are relative to the morph currently being drawn.


<details>
	<summary>See more</summary>
	
	displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d extraW corner origin |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x - halfW.
			x1 _ x - halfW].
	x0-textLeft < -1 ifTrue: [
		x1 _ x1 - x0 + textLeft.
		x0 _ textLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-textLeft) ifTrue: [
		x0 _ x0 + r - x1+textLeft.
		x1 _ r +textLeft].
	extraW _ isBold
		ifTrue: [ 3 ]
		ifFalse: [ 2 ].
	origin _ x0-halfW@ top.
	corner _ x1+halfW+extraW @ (bottom+2).
	lastTextCursorRect
		ifNil: [ lastTextCursorRect _ origin corner: corner ]
		ifNotNil: [lastTextCursorRect
			setOrigin: (lastTextCursorRect origin min: origin)
			corner: (lastTextCursorRect corner max: corner)].
	aCanvas
		line: x0+halfW@(bottom-w-w+halfW) to: x1+halfW@top
		width: w color: textCursorColor
</details>

#### TextComposition>>#defaultFont

<details>
	<summary>See more</summary>
	
	defaultFont
	^editor ifNil: [ AbstractFont default ] ifNotNil: [ editor defaultFont ]
</details>

#### TextComposition>>#defaultCharacterBlock

<details>
	<summary>See more</summary>
	
	defaultCharacterBlock
	^ CharacterBlock
		stringIndex: 1
		text: model actualContents
		topLeft: lines first topLeft
		extent: `0 @ 0`
		textLine: lines first
</details>

## TextLine

A TextLine embodies the layout of a line of composed text. left right top bottom The full line rectangle firstIndex lastIndex Starting and stopping indices in the full text internalSpaces Number of spaces to share paddingWidth paddingWidth Number of pixels of extra space in full line baseline Distance of baseline below the top of the line isFirstLine A boolean indicating if it is the first line in a paragraph TextLine's rather verbose message protocol is required for compatibility with the old CharacterScanners.

### Methods
#### TextLine>>#slideIndexBy: delta andMoveTopTo: newTop

Relocate my character indices and y-values. Used to slide constant text up or down in the wake of a text replacement.


<details>
	<summary>See more</summary>
	
	slideIndexBy: delta andMoveTopTo: newTop
	"Relocate my character indices and y-values.
	Used to slide constant text up or down in the wake of a text replacement."

	firstIndex _ firstIndex + delta.
	lastIndex _ lastIndex + delta.
	bottom _ bottom + (newTop - top).
	top _ newTop.

</details>

#### TextLine>>#rectangle: lineRectangle

<details>
	<summary>See more</summary>
	
	rectangle: lineRectangle
	left _ lineRectangle left.
	right _ lineRectangle right.
	top _ lineRectangle top.
	bottom _ lineRectangle bottom
</details>

#### TextLine>>#leftMargin: lm

<details>
	<summary>See more</summary>
	
	leftMargin: lm
	left _ lm
</details>

#### TextLine>>#paddingWidth

Answer the amount of space to be added to the font.


<details>
	<summary>See more</summary>
	
	paddingWidth
	"Answer the amount of space to be added to the font."

	^paddingWidth
</details>

#### TextLine>>#baseline

<details>
	<summary>See more</summary>
	
	baseline
	^ baseline
</details>

#### TextLine>>#first

<details>
	<summary>See more</summary>
	
	first
	^ firstIndex
</details>

#### TextLine>>#top

<details>
	<summary>See more</summary>
	
	top
	^ top
</details>

#### TextLine>>#justifiedPadFor: spaceIndex

Compute the width of pad for a given space in a line of justified text.


<details>
	<summary>See more</summary>
	
	justifiedPadFor: spaceIndex 
	"Compute the width of pad for a given space in a line of justified text."

	| pad |
	internalSpaces = 0 ifTrue: [^0].
	pad _ paddingWidth // internalSpaces.
	spaceIndex <= (paddingWidth \\ internalSpaces)
		ifTrue: [^pad + 1]
		ifFalse: [^pad]
</details>

#### TextLine>>#hash

#hash is re-implemented because #= is re-implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is re-implemented because #= is re-implemented"
	^firstIndex hash bitXor: lastIndex hash
</details>

#### TextLine>>#lineHeight

<details>
	<summary>See more</summary>
	
	lineHeight
	^ bottom - top
</details>

#### TextLine>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream space; print: firstIndex; nextPutAll: ' to: '; print: lastIndex
</details>

#### TextLine>>#internalSpaces

Answer the number of spaces in the line.


<details>
	<summary>See more</summary>
	
	internalSpaces
	"Answer the number of spaces in the line."

	^internalSpaces
</details>

#### TextLine>>#= line

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= line
 	self == line ifTrue: [ ^ true ].

	self species == line species
		ifFalse: [ ^ false ].

	^((firstIndex = line first and: [ lastIndex = line last ])
		and: [ internalSpaces = line internalSpaces ])
		and: [ paddingWidth = line paddingWidth ]
</details>

#### TextLine>>#lineHeight: height baseline: ascent

<details>
	<summary>See more</summary>
	
	lineHeight: height baseline: ascent
	bottom _ top + height.
	baseline _ ascent
</details>

#### TextLine>>#bottom

<details>
	<summary>See more</summary>
	
	bottom
	^ bottom
</details>

#### TextLine>>#internalSpaces: spacesInteger

Set the number of spaces in the line to be spacesInteger.


<details>
	<summary>See more</summary>
	
	internalSpaces: spacesInteger 
	"Set the number of spaces in the line to be spacesInteger."

	internalSpaces _ spacesInteger
</details>

#### TextLine>>#paddingWidth: padWidthInteger

Set the amount of space to be added to the font to be padWidthInteger.


<details>
	<summary>See more</summary>
	
	paddingWidth: padWidthInteger 
	"Set the amount of space to be added to the font to be padWidthInteger."

	paddingWidth _ padWidthInteger
</details>

#### TextLine>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	isFirstLine _ false
</details>

#### TextLine>>#width

<details>
	<summary>See more</summary>
	
	width
	^ right - left
</details>

#### TextLine>>#stop: stopInteger

Set the stopping point in the string of the line to be stopInteger.


<details>
	<summary>See more</summary>
	
	stop: stopInteger 
	"Set the stopping point in the string of the line to be stopInteger."

	lastIndex _ stopInteger
</details>

#### TextLine>>#firstIndex: firstInteger lastIndex: lastInteger

<details>
	<summary>See more</summary>
	
	firstIndex: firstInteger lastIndex: lastInteger
	firstIndex _ firstInteger.
	lastIndex _ lastInteger
</details>

#### TextLine>>#isEmptyLine

<details>
	<summary>See more</summary>
	
	isEmptyLine
	^false
</details>

#### TextLine>>#bottomRight

<details>
	<summary>See more</summary>
	
	bottomRight
	^ right@bottom
</details>

#### TextLine>>#left

<details>
	<summary>See more</summary>
	
	left
	^ left
</details>

#### TextLine>>#isFirstLine: aBoolean

<details>
	<summary>See more</summary>
	
	isFirstLine: aBoolean
	isFirstLine _ aBoolean
</details>

#### TextLine>>#last

<details>
	<summary>See more</summary>
	
	last
	^ lastIndex
</details>

#### TextLine>>#rightMargin

This has to get fixed -- store during composition


<details>
	<summary>See more</summary>
	
	rightMargin
	"This has to get fixed -- store during composition"
	^ self right
</details>

#### TextLine>>#isFirstLine

<details>
	<summary>See more</summary>
	
	isFirstLine
	^isFirstLine
</details>

#### TextLine>>#internalSpaces: spacesInteger paddingWidth: padWidthInteger

<details>
	<summary>See more</summary>
	
	internalSpaces: spacesInteger paddingWidth: padWidthInteger

	internalSpaces _ spacesInteger.
	paddingWidth _ padWidthInteger
</details>

#### TextLine>>#right

<details>
	<summary>See more</summary>
	
	right
	^ right
</details>

#### TextLine>>#leftMarginForAlignment: alignmentCode

<details>
	<summary>See more</summary>
	
	leftMarginForAlignment: alignmentCode
	alignmentCode = CharacterScanner rightFlushCode ifTrue: [^ self left + paddingWidth].
	alignmentCode = CharacterScanner centeredCode ifTrue: [
		^ self left + (paddingWidth//2)].
	^ self left  "leftFlush and justified"
</details>

#### TextLine>>#rectangle

<details>
	<summary>See more</summary>
	
	rectangle
	^ self topLeft corner: self bottomRight
</details>

#### TextLine>>#topLeft

<details>
	<summary>See more</summary>
	
	topLeft
	^ left @ top
</details>

#### TextLine>>#leftMargin

This has to get fixed -- store during composition


<details>
	<summary>See more</summary>
	
	leftMargin
	"This has to get fixed -- store during composition"
	^ self left
</details>

