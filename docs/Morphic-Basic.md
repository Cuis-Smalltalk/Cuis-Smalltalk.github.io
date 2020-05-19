## EllipseMorph

A round BorderedMorph. Supports borderWidth and borderColor. EllipseMorph new borderWidth:10; borderColor: Color green; openInWorld.

### Methods
#### EllipseMorph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	^false
</details>

#### EllipseMorph>>#defaultColor

Return the default fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"Return the default fill style for the receiver"
	^ `Color yellow`
</details>

#### EllipseMorph>>#morphContainsPoint: aLocalPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	morphContainsPoint: aLocalPoint

	| radius other delta xOverY |
	(self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [^ false].  "quick elimination"
	extent > `1@1`
		ifFalse: [^ true].  "Degenerate case -- code below fails by a bit"

	radius _ extent y asFloat / 2.
	other _ extent x asFloat / 2.
	delta _ aLocalPoint - (other@radius).
	xOverY _ extent x asFloat / extent y asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared
</details>

#### EllipseMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas 

	| rx ry |
	self revisar. "The morph should be specified better!"
	rx _ extent x //2.
	ry _ extent y // 2.
	aCanvas ellipseCenterX: rx y: ry rx: rx ry: ry borderWidth: borderWidth borderColor: borderColor fillColor: color
</details>

#### EllipseMorph>>#defaultBorderWidth

answer the default border width for the receiver


<details>
	<summary>See more</summary>
	
	defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1
</details>

## ImageMorph

ImageMorph is a morph that displays a picture (Form). My extent is determined by the extent of my form. Use #image: to set my picture. Structure: instance var Type Description image Form The Form to use when drawing Code examples: ImageMorph new openInWorld; grabFromScreen ImageMorph new image: (Form fromFileNamed: 'myGraphicsFileName'); openInWorld. Relationship to SketchMorph: ImageMorph should be favored over SketchMorph, a parallel, legacy class -- see the Swiki FAQ for details ( http://minnow.cc.gatech.edu/squeak/1372 ).

### Methods
#### ImageMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize

	super initialize.
	self image: self class defaultForm.

</details>

#### ImageMorph>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent
	^image extent
</details>

#### ImageMorph>>#morphExtent: aPoint

Do nothing; my extent is determined by my image Form.


<details>
	<summary>See more</summary>
	
	morphExtent: aPoint
	"Do nothing; my extent is determined by my image Form."

	"A clear case of a morph that shouldn't have an 'extent' ivar..."
	self flag: #jmvVer2.
</details>

#### ImageMorph>>#grabFromScreen

<details>
	<summary>See more</summary>
	
	grabFromScreen

	self image: Form fromUser.

</details>

#### ImageMorph>>#readFromFile

<details>
	<summary>See more</summary>
	
	readFromFile
	| fileName |
	fileName _ FillInTheBlankMorph
		request: 'Please enter the image file name'
		initialAnswer: 'fileName'.
	fileName isEmpty ifTrue: [^ self].
	self image: (Form fromFileNamed: fileName).

</details>

#### ImageMorph>>#color: aColor

Set the receiver's color.


<details>
	<summary>See more</summary>
	
	color: aColor
        super color: aColor.
        (image depth = 1 and: [aColor is: #Color]) ifTrue: [
                image colors: {`Color transparent`. aColor}.
                self redrawNeeded]
</details>

#### ImageMorph>>#image: anImage

<details>
	<summary>See more</summary>
	
	image: anImage 
	| newExtent |
	self redrawNeeded.
	image _ anImage depth = 1 
		ifTrue: [ ColorForm mappingWhiteToTransparentFrom: anImage ]
		ifFalse: [ anImage ]. 
	newExtent _ image extent.
	extent = newExtent ifFalse: [
		self redrawNeeded.
		extent _ newExtent.
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].
</details>

#### ImageMorph>>#form

<details>
	<summary>See more</summary>
	
	form
	^image
</details>

#### ImageMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	aCanvas image: image at: self morphTopLeft
</details>

## StringMorph

StringMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextMorph. Structure: instance var Type Description font StrikeFont (normally nil; then the accessor #font gives back a Font or nil #defaultFont) emphasis SmallInteger bitmask determining character attributes (underline, bold, italics, struckThrough) contents String The text that will be displayed.

### Methods
#### StringMorph>>#emphasis: aNumber

Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format: bit attribute 1 bold 2 italic 4 underlined 8 struckThrough 16 withUnderscoreGlyphs


<details>
	<summary>See more</summary>
	
	emphasis: aNumber
	"Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:

	bit	attribute
	1	bold
	2	italic
	4	underlined
	8	struckThrough
	16	withUnderscoreGlyphs
	"

	"examples: 0 -> plain.  
	1 -> bold.  2 -> italic.  3 -> bold italic.  4 -> underlined  
	5 -> bold underlined.  6 -> italic underlined.   7 -> bold italic underlined   
	etc..."

	emphasis _ aNumber.
	^ self font: font emphasis: emphasis
</details>

#### StringMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	font _ nil.
	emphasis _ 0.
	self contents: 'String Morph'

</details>

#### StringMorph>>#changeFont

<details>
	<summary>See more</summary>
	
	changeFont
	| newFont |
	newFont _ AbstractFont fromUser: self fontToUse.
	newFont ifNotNil:[self font: newFont].
</details>

#### StringMorph>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent

	^ self measureContents 
</details>

#### StringMorph>>#contents: newContents

<details>
	<summary>See more</summary>
	
	contents: newContents
	contents _ (newContents is: #Text)
		ifTrue: [
			emphasis _ newContents emphasisAt: 1.
			 newContents string ]
		ifFalse: [
			contents = newContents ifTrue: [ ^self ].	"no substantive change"
			newContents].
	self fitContents.
	self redrawNeeded
</details>

#### StringMorph>>#addOptionalHandlesTo: aHalo box: box

<details>
	<summary>See more</summary>
	
	addOptionalHandlesTo: aHalo box: box
	self flag: #deferred.

	"Eventually...
	self addFontHandlesTo: aHalo box: box"
</details>

#### StringMorph>>#fitContents

Measures contents later at #minimumExtent


<details>
	<summary>See more</summary>
	
	fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: `0@0`
</details>

#### StringMorph>>#changeEmphasis

<details>
	<summary>See more</summary>
	
	changeEmphasis

	| reply aList |
	aList _ #(normal bold italic underlined struckThrough).
	reply _ (SelectionMenu labelList: aList selections: aList) startUpMenu.
	reply ifNotNil:[
		self emphasis: (TextEmphasis perform: reply) emphasisCode.
	].

</details>

#### StringMorph>>#initWithContents: aString font: aFont emphasis: emphasisCode

<details>
	<summary>See more</summary>
	
	initWithContents: aString font: aFont emphasis: emphasisCode 
	self initialize.
	
	font _ aFont.
	emphasis _ emphasisCode.
	self contents: aString
</details>

#### StringMorph>>#font

who came up with #fontToUse rather than font?!


<details>
	<summary>See more</summary>
	
	font
	"who came up with #fontToUse rather than font?!"
	^self fontToUse
</details>

#### StringMorph>>#defaultColor

answer the default color/fill style for the receiver


<details>
	<summary>See more</summary>
	
	defaultColor
	"answer the default color/fill style for the receiver"
	^ Theme current menuText
</details>

#### StringMorph>>#addCustomMenuItems: aCustomMenu hand: aHandMorph

Add morph-specific items to the given menu which was invoked by the given hand. This method provides is invoked both from the halo-menu and from the control-menu regimes.


<details>
	<summary>See more</summary>
	
	addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change font' action: #changeFont.
	aCustomMenu add: 'change emphasis' action: #changeEmphasis.

</details>

#### StringMorph>>#printOn: aStream

Add the identity of the receiver to a stream


<details>
	<summary>See more</summary>
	
	printOn: aStream

	super printOn: aStream.
	aStream print: contents.

</details>

#### StringMorph>>#font: aFont

Set the font my text will use. The emphasis remains unchanged.


<details>
	<summary>See more</summary>
	
	font: aFont 
	"Set the font my text will use. The emphasis remains unchanged."

	font _ aFont.
	^ self font: font emphasis: emphasis
</details>

#### StringMorph>>#font: aFont emphasis: emphasisCode

<details>
	<summary>See more</summary>
	
	font: aFont emphasis: emphasisCode
	font _ aFont.
	emphasis _ emphasisCode.
	self fitContents.
	self redrawNeeded
"
in inspector say,
	 self font: StrikeFont default emphasis: 1
"
</details>

#### StringMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self morphTopLeft 
		font: self fontToUse
		color: color
</details>

#### StringMorph>>#measureContents

<details>
	<summary>See more</summary>
	
	measureContents
	| f |
	f _ self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f lineSpacing
</details>

#### StringMorph>>#contents

<details>
	<summary>See more</summary>
	
	contents

	^ contents
</details>

#### StringMorph>>#fontToUse

<details>
	<summary>See more</summary>
	
	fontToUse
	| fontToUse |
	fontToUse := font ifNil: [AbstractFont default].
	^(emphasis isNil or: [emphasis = 0]) 
		ifTrue: [ fontToUse]
		ifFalse: [ fontToUse emphasized: emphasis]
</details>

