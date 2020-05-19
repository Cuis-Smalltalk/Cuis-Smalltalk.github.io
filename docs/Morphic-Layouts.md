## LayoutAdjustingMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### LayoutAdjustingMorph>>#isOpaqueMorph

Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:) but is not an opaque rectangle covering bounds MUST answer false to this message


<details>
	<summary>See more</summary>
	
	isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	^true
</details>

#### LayoutAdjustingMorph>>#stepAt: millisecondSinceLast

got the #mouseLeave: message


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	"got the #mouseLeave: message"
	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [ Cursor defaultCursor activateCursor ].
		^ self stopStepping ].
	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			(Preferences cheapWindowReframe or: [ millisecondSinceLast > 200]) ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]]]
		ifFalse: [
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor defaultCursor activateCursor ]].
</details>

#### LayoutAdjustingMorph>>#mouseEnter: anEvent

Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseEnter: anEvent
	super mouseEnter: anEvent.
	self cursor activateCursor.
	hand _ anEvent hand
</details>

#### LayoutAdjustingMorph>>#cursor

<details>
	<summary>See more</summary>
	
	cursor
	^ owner direction == #horizontal
		ifTrue: [ Cursor cursorAt: #resizeLeftCursor ]
		ifFalse: [ Cursor cursorAt: #resizeTopCursor ].
</details>

#### LayoutAdjustingMorph>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseButton messages ? - #mouseButton1Down:localPosition: - #mouseButton1Up:localPosition: - #mouseButton2Down:localPosition: - #mouseButton2Up:localPosition: - #mouseButton3Down:localPosition: - #mouseButton3Up:localPosition: - #mouseMove:localPosition: - #mouseButton2Activity NOTE: The default response is false. Subclasses that implement these messages directly should override this one to return true. Implementors could query the argument, and only answer true for (for example) button 2 up only.


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent

	^ true
</details>

#### LayoutAdjustingMorph>>#adoptWidgetsColor: paneColor

<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	self color: paneColor
</details>

#### LayoutAdjustingMorph>>#stepTime

Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!


<details>
	<summary>See more</summary>
	
	stepTime
	"Update very often. Very short steptimes should only be used for morphs that are not stepping all the time!"
	^ 20
</details>

#### LayoutAdjustingMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
</details>

#### LayoutAdjustingMorph>>#handlesMouseOver: evt

Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty? The default response is false.


<details>
	<summary>See more</summary>
	
	handlesMouseOver: evt

	^ true
</details>

#### LayoutAdjustingMorph>>#mouseLeave: anEvent

Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed.


<details>
	<summary>See more</summary>
	
	mouseLeave: anEvent
	super mouseLeave: anEvent.
	hand ifNotNil: [
		hand _ nil.
		Cursor defaultCursor activateCursor ].
</details>

#### LayoutAdjustingMorph>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Handle a mouse down event. This message will only be sent to Morphs that answer true to #handlesMouseDown:


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self cursor activateCursor.
	hand _ aMouseButtonEvent hand.
	self startStepping
</details>

#### LayoutAdjustingMorph>>#adjustOwnerAt: aPoint

<details>
	<summary>See more</summary>
	
	adjustOwnerAt: aPoint
	owner
		adjustBy: self
		at: aPoint
</details>

## LayoutMorph

A row or column of widgets, does layout by placing them either horizontally or vertically. Submorphs might specify a LayoutSpec. If some don't, then, for a column, the column width is taken as the width, and any morph height is kept. Same for rows: submorph width would be maintained, and submorph height would be made equal to row height.

### Methods
#### LayoutMorph>>#direction

<details>
	<summary>See more</summary>
	
	direction
	^direction
</details>

#### LayoutMorph>>#addMorph: aMorph fixedHeight: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph fixedHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSpec fixedHeight: aNumber)
</details>

#### LayoutMorph>>#addAdjusterAndMorph: aMorph layoutSpec: aLayoutSpec

Add a submorph, at the bottom or right, with aLayoutSpec


<details>
	<summary>See more</summary>
	
	addAdjusterAndMorph: aMorph layoutSpec: aLayoutSpec
	"Add a submorph, at the bottom or right, with aLayoutSpec"
	self
		addAdjusterMorph;
		addMorph: aMorph layoutSpec: aLayoutSpec
</details>

#### LayoutMorph>>#addMorph: aMorph

Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph
	"Add a submorph, at the bottom or right, with a default LayoutSpec if none was provided."

	aMorph layoutSpec.
	self addMorphFront: aMorph
</details>

#### LayoutMorph>>#adjustVerticallyBy: aLayoutAdjustMorph at: aPoint

<details>
	<summary>See more</summary>
	
	adjustVerticallyBy: aLayoutAdjustMorph at: aPoint
	| delta t ts b bs tNewHeight bNewHeight i tCurrentHeight bCurrentHeight |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	t _ self submorphs at: i +1.
	ts _ t layoutSpec.
	tCurrentHeight _ t morphHeight max: 1.	"avoid division by zero"
	b _ self submorphs at: i - 1.
	bs _ b layoutSpec.
	bCurrentHeight _ b morphHeight max: 1.	"avoid division by zero"
	delta _ aPoint y - aLayoutAdjustMorph referencePosition y.
	delta _ delta max: t minimumLayoutExtent y - tCurrentHeight.
	delta _ delta min: bCurrentHeight - b minimumLayoutExtent y.
	delta = 0 ifTrue: [ ^self ].
	tNewHeight _ tCurrentHeight + delta.
	bNewHeight _ bCurrentHeight - delta.
	(ts isProportionalHeight and: [ bs isProportionalHeight ])
		ifTrue: [ | bottomNewProportion toDistribute topNewProportion |	"If both proportional, update them"
			topNewProportion _ tNewHeight / (tNewHeight + bNewHeight).
			bottomNewProportion _ 1.0 - topNewProportion.
			toDistribute _ ts proportionaLayoutlHeight + bs proportionaLayoutlHeight.
			ts setProportionalHeight: topNewProportion * toDistribute.
			bs setProportionalHeight: bottomNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ts isProportionalHeight ifFalse: [
				ts fixedOrMorphHeight: tNewHeight ].
			bs isProportionalHeight ifFalse: [
				bs fixedOrMorphHeight: bNewHeight ]].
	self layoutSubmorphs
</details>

#### LayoutMorph>>#addMorphKeepMorphHeight: aMorph

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorphKeepMorphHeight: aMorph
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSpec new useMorphHeight)
</details>

#### LayoutMorph>>#layoutSubmorphsVerticallyIn: boundsForLayout

Compute a new layout based on the given layout bounds.


<details>
	<summary>See more</summary>
	
	layoutSubmorphsVerticallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableHeight sumOfFixedOrMinimum normalizationFactor availableForPropHeight 
		fractionalHeights integerHeights theTop usableWidth boundsLeft boundsBottom theLeft minHeight submorphsToLayout 
			nextMorph ht wd ls theBottom boundsRight theRight alternativeHeights count diff i |

	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableHeight := boundsForLayout height - ((submorphsToLayout size + 1) * ySep).
	sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent y max: m layoutSpec fixedOrMinimumLayoutHeight ].
	availableForPropHeight := usableHeight - sumOfFixedOrMinimum max: 0.
	normalizationFactor := self proportionalHeightNormalizationFactor.
	availableForPropHeight := availableForPropHeight * normalizationFactor.
	
	fractionalHeights := submorphsToLayout collect: [ :m | m layoutSpec heightFor: availableForPropHeight ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerHeights _ fractionalHeights collect: [ :w | w rounded ].
	diff _ integerHeights sum - fractionalHeights sum rounded.
	alternativeHeights _ diff > 0 ifTrue: [ fractionalHeights collect: [ :w | w floor ]] ifFalse: [ fractionalHeights collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerHeights at: i) = (alternativeHeights at: i) ifFalse: [
			integerHeights at: i put: (alternativeHeights at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minHeight := integerHeights sum.
	theTop := ((usableHeight - minHeight) * (padding ifNil: [0]) max: 0) + boundsForLayout top + ySep.
	usableWidth     := boundsForLayout width - (xSep * 2) max: 0.
	boundsLeft      := boundsForLayout left + xSep.
	boundsBottom := boundsForLayout bottom - ySep.
	boundsRight := boundsForLayout right - xSep.
	
	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		ht := integerHeights at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		wd := (ls widthFor: usableWidth) min: usableWidth.
		theLeft := ((usableWidth - wd) * ls minorDirectionPadding) floor + boundsLeft.
		theRight := (theLeft + wd) ceiling min: boundsRight.
		theBottom := (theTop + (ht min: minHeight)) "ceiling" min: boundsBottom.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theTop := theBottom + ySep
	]
</details>

#### LayoutMorph>>#beRow

<details>
	<summary>See more</summary>
	
	beRow
	direction _ #horizontal.
	padding ifNil: [self padding: #left].
	self refreshExtent.
</details>

#### LayoutMorph>>#beColumn

<details>
	<summary>See more</summary>
	
	beColumn
	direction _ #vertical.
	padding ifNil: [self padding: #center].
	self refreshExtent.
</details>

#### LayoutMorph>>#adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint

<details>
	<summary>See more</summary>
	
	adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint
	| delta l ls r rs lNewWidth rNewWidth i lCurrentWidth rCurrentWidth |
	i _ submorphs indexOf: aLayoutAdjustMorph.
	l _ self submorphs at: i +1.
	ls _ l layoutSpec.
	lCurrentWidth _ l morphWidth max: 1.	"avoid division by zero"
	r _ self submorphs at: i - 1.
	rs _ r layoutSpec.
	rCurrentWidth _ r morphWidth max: 1.	"avoid division by zero"
	delta _ aPoint x - aLayoutAdjustMorph referencePosition x.
	delta _ delta max: l minimumLayoutExtent x - lCurrentWidth.
	delta _ delta min: rCurrentWidth - r minimumLayoutExtent x.
	delta = 0 ifTrue: [ ^self ].
	rNewWidth _ rCurrentWidth - delta.
	lNewWidth _ lCurrentWidth + delta.
	(ls isProportionalWidth and: [ rs isProportionalWidth ])
		ifTrue: [ | leftNewProportion rightNewProportion toDistribute |	"If both proportional, update them"
			leftNewProportion _ lNewWidth / (lNewWidth + rNewWidth).
			rightNewProportion _ 1.0 - leftNewProportion.
			toDistribute _ ls proportionalLayoutWidth + rs proportionalLayoutWidth.
			ls setProportionalWidth: leftNewProportion * toDistribute.
			rs setProportionalWidth: rightNewProportion * toDistribute ]
		ifFalse: ["If at least one is fixed, update only the fixed"
			ls isProportionalWidth ifFalse: [
				ls fixedOrMorphWidth: lNewWidth ].
			rs isProportionalWidth ifFalse: [
				rs fixedOrMorphWidth: rNewWidth ]].
	self layoutSubmorphs.
</details>

#### LayoutMorph>>#defaultColor

<details>
	<summary>See more</summary>
	
	defaultColor
	^ `Color gray`
</details>

#### LayoutMorph>>#submorphsToLayout

Select those that will be layout


<details>
	<summary>See more</summary>
	
	submorphsToLayout
	"Select those that will be layout"

	^submorphs select: [ :m | m visible ]
</details>

#### LayoutMorph>>#layoutSpec

Layout specific. Return the layout spec describing where the receiver should appear in a proportional layout


<details>
	<summary>See more</summary>
	
	layoutSpec
	"Layout specific. Return the layout spec describing where the
	receiver should appear in a proportional layout"

	layoutSpec ifNotNil: [ :ls | ^ ls ].
	layoutSpec _ LayoutSpec useAll.
	layoutSpec morph: self.

	^ layoutSpec 
</details>

#### LayoutMorph>>#proportionalHeightNormalizationFactor

<details>
	<summary>See more</summary>
	
	proportionalHeightNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionaLayoutlHeight ].
	^1.0 / (sumOfProportional max: 1.0).
</details>

#### LayoutMorph>>#padding: aSymbolOrNumber

This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used. self new padding: #center self new padding: 0.9


<details>
	<summary>See more</summary>
	
	padding: aSymbolOrNumber
	"This sets how extra space is used when doing layout. For example, a column might have extra , unneded vertical space. #top means widgets are set close to the top, and extra space is at bottom. Conversely, #bottom means widgets are set close to the bottom, and extra space is at top. Valid values include #left and #right (for rows) and #center. Alternatively, any number between 0.0 and 1.0 might be used.
	self new padding: #center
	self new padding: 0.9
	"
	padding _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]
</details>

#### LayoutMorph>>#layoutBounds

Return the bounds for laying out children of the receiver


<details>
	<summary>See more</summary>
	
	layoutBounds
	"Return the bounds for laying out children of the receiver"

	^ self morphLocalBounds
</details>

#### LayoutMorph>>#addMorphUseAll: aMorph

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorphUseAll: aMorph
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: LayoutSpec useAll
</details>

#### LayoutMorph>>#addMorphs: morphs widthProportionalTo: widths

Widths can be in any arbitrary unit. The actual widths will be proportional to them.


<details>
	<summary>See more</summary>
	
	addMorphs: morphs widthProportionalTo: widths
	"Widths can be in any arbitrary unit. The actual widths will be proportional to them."
	morphs with: widths do: [ :m :w |
		self addMorph: m proportionalWidth: w ]
</details>

#### LayoutMorph>>#addMorphs: morphs

All morphs are made equal width


<details>
	<summary>See more</summary>
	
	addMorphs: morphs
	"All morphs are made equal width"
	morphs do: [ :m |
		self addMorph: m proportionalWidth: 1 ]
</details>

#### LayoutMorph>>#separation: aNumberOrPoint

<details>
	<summary>See more</summary>
	
	separation: aNumberOrPoint
	separation _ aNumberOrPoint
</details>

#### LayoutMorph>>#doAdoptWidgetsColor

<details>
	<summary>See more</summary>
	
	doAdoptWidgetsColor
	doAdoptWidgetsColor _ true
</details>

#### LayoutMorph>>#addMorph: aMorph proportionalHeight: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph proportionalHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSpec proportionalHeight: aNumber)
</details>

#### LayoutMorph>>#addMorph: aMorph proportionalWidth: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph proportionalWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSpec proportionalWidth: aNumber)
</details>

#### LayoutMorph>>#ySeparation

<details>
	<summary>See more</summary>
	
	ySeparation
	^separation isNumber
		ifTrue: [ separation ]
		ifFalse: [ separation y ]
</details>

#### LayoutMorph>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	separation _ 0.
	doAdoptWidgetsColor _ false
</details>

#### LayoutMorph>>#addAdjusterAndMorph: aMorph proportionalHeight: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addAdjusterAndMorph: aMorph proportionalHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSpec proportionalHeight: aNumber)
</details>

#### LayoutMorph>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #LayoutMorph or: [ super is: aSymbol ]
</details>

#### LayoutMorph>>#minimumExtent

Answer size sufficient to frame my submorphs.


<details>
	<summary>See more</summary>
	
	minimumExtent
	"Answer size sufficient to frame my submorphs."
	
	| width height |
	width := self ySeparation.
	height := self xSeparation.
	(self direction =  #vertical)
		ifTrue: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"use maximum width across submorphs"
				width := width max: (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth).
				"sum up submorph heights"
				height := height + (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight) + self ySeparation.
			].
		     width := width + self xSeparation.
		]
		ifFalse: [
			self submorphsDo: [ :sm | | smMinExtent |
				smMinExtent := sm minimumLayoutExtent.
				"sum up submorphs width"
				width := width + (smMinExtent x max: sm layoutSpec fixedOrMinimumLayoutWidth) + self xSeparation.
				"use maximum height across submorph"
				height := height max: (smMinExtent y max: sm layoutSpec fixedOrMinimumLayoutHeight).
			].
			height := height + self ySeparation.
		].

	^ (width @ height) + self extentBorder
</details>

#### LayoutMorph>>#layoutSubmorphs

Compute a new layout based on the given layout bounds.


<details>
	<summary>See more</summary>
	
	layoutSubmorphs
	"Compute a new layout based on the given layout bounds."

	submorphs isEmpty ifTrue: [
		layoutNeeded _ false.
		^self].

	direction == #horizontal ifTrue: [
		self layoutSubmorphsHorizontallyIn: self layoutBounds ].

	direction == #vertical ifTrue: [
		self layoutSubmorphsVerticallyIn: self layoutBounds ].

	layoutNeeded _ false
</details>

#### LayoutMorph>>#refreshExtent

Flush cache & recalculate


<details>
	<summary>See more</summary>
	
	refreshExtent
	"Flush cache & recalculate"
	(self isOwnedByWorld or: [self isOwnedByHand]) ifTrue: [
		self morphExtent: (self morphExtent max: self minimumExtent) ]
</details>

#### LayoutMorph>>#proportionalWidthNormalizationFactor

<details>
	<summary>See more</summary>
	
	proportionalWidthNormalizationFactor

	| sumOfProportional |
	sumOfProportional _ self submorphsToLayout sum: [ :m | m layoutSpec proportionalLayoutWidth ].
	^1.0 / (sumOfProportional max: 1.0).
</details>

#### LayoutMorph>>#addMorph: aMorph layoutSpec: aLayoutSpec

Add a submorph, at the bottom or right, with aLayoutSpec


<details>
	<summary>See more</summary>
	
	addMorph: aMorph layoutSpec: aLayoutSpec

	"Add a submorph, at the bottom or right, with aLayoutSpec"
	aMorph layoutSpec: aLayoutSpec.
	self addMorphFront: aMorph
</details>

#### LayoutMorph>>#adoptWidgetsColor: paneColor

<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) ]
		ifFalse: [ self color: `Color transparent` ]
</details>

#### LayoutMorph>>#addMorph: aMorph fixedWidth: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addMorph: aMorph fixedWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addMorph: aMorph layoutSpec: (LayoutSpec fixedWidth: aNumber)
</details>

#### LayoutMorph>>#adjustBy: aLayoutAdjustMorph at: aPoint

<details>
	<summary>See more</summary>
	
	adjustBy: aLayoutAdjustMorph at: aPoint

	direction == #horizontal ifTrue: [
		self adjustHorizontallyBy: aLayoutAdjustMorph at: aPoint ].

	direction == #vertical ifTrue: [
		self adjustVerticallyBy: aLayoutAdjustMorph at: aPoint ].
</details>

#### LayoutMorph>>#addAdjusterAndMorph: aMorph fixedHeight: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addAdjusterAndMorph: aMorph fixedHeight: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSpec fixedHeight: aNumber)
</details>

#### LayoutMorph>>#addAdjusterAndMorph: aMorph proportionalWidth: aNumber

Convenience method. Add others as necessary.


<details>
	<summary>See more</summary>
	
	addAdjusterAndMorph: aMorph proportionalWidth: aNumber
	"Convenience method.
	Add others as necessary."
	self addAdjusterAndMorph: aMorph layoutSpec: (LayoutSpec proportionalWidth: aNumber)
</details>

#### LayoutMorph>>#xSeparation

<details>
	<summary>See more</summary>
	
	xSeparation
	^separation isNumber
		ifTrue: [ separation ]
		ifFalse: [ separation x ]
</details>

#### LayoutMorph>>#addMorphFrontFromWorldPosition: aMorph

<details>
	<summary>See more</summary>
	
	addMorphFrontFromWorldPosition: aMorph

	super addMorphFrontFromWorldPosition: aMorph.
	self refreshExtent.

</details>

#### LayoutMorph>>#addAdjusterMorph

So the user can adjust layout


<details>
	<summary>See more</summary>
	
	addAdjusterMorph
	"So the user can adjust layout"
	"twekear para ui grande..."

	| thickness |
	thickness _ Theme current layoutAdjusterThickness.

	direction == #horizontal ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSpec fixedWidth: thickness) ].

	direction == #vertical ifTrue: [
		self
			addMorph: LayoutAdjustingMorph new
			layoutSpec: (LayoutSpec fixedHeight: thickness)]
</details>

#### LayoutMorph>>#layoutSubmorphsHorizontallyIn: boundsForLayout

Compute a new layout based on the given layout bounds.


<details>
	<summary>See more</summary>
	
	layoutSubmorphsHorizontallyIn: boundsForLayout
	"Compute a new layout based on the given layout bounds."
	| xSep ySep usableWidth sumOfFixedOrMinimum normalizationFactor availableForPropWidth 
		fractionalWidths integerWidths theLeft usableHeight boundsTop boundsRight theTop minWidth submorphsToLayout 
			nextMorph ht wd ls theRight boundsBottom theBottom alternativeWidths count diff i |
	
	boundsForLayout extent > `2@2` "self minimumExtent" 
		ifFalse: [ ^self ]. "Too small. Don't bother!"

	submorphsToLayout := self submorphsToLayout.
	xSep := self xSeparation.
	ySep := self ySeparation.
	usableWidth := boundsForLayout width - ((submorphsToLayout size + 1) * xSep).
	sumOfFixedOrMinimum := submorphsToLayout sum: [ :m | m minimumLayoutExtent x max: m layoutSpec fixedOrMinimumLayoutWidth ].
	availableForPropWidth := usableWidth - sumOfFixedOrMinimum max: 0.
	normalizationFactor := self proportionalWidthNormalizationFactor.
	availableForPropWidth := availableForPropWidth * normalizationFactor.
	
	fractionalWidths := submorphsToLayout collect: [ :m | m layoutSpec widthFor: availableForPropWidth ].
	"Compute integer widths, mostly rounding but with occasional #floor or #ceiling as needed to keep sum"
	integerWidths _ fractionalWidths collect: [ :w | w rounded ].
	diff _ integerWidths sum - fractionalWidths sum rounded.
	alternativeWidths _ diff > 0 ifTrue: [ fractionalWidths collect: [ :w | w floor ]] ifFalse: [ fractionalWidths collect: [ :w | w ceiling ]].
	count _ diff abs.
	i _ 1.
	[ count > 0] whileTrue: [
		(integerWidths at: i) = (alternativeWidths at: i) ifFalse: [
			integerWidths at: i put: (alternativeWidths at: i).
			count _ count -1 ].
		i _ i + 1 ].
	minWidth := integerWidths sum.
	theLeft := ((usableWidth - minWidth) * (padding ifNil: [0]) max: 0) + boundsForLayout left + xSep.
	usableHeight := boundsForLayout height - (ySep * 2) max: 0.
	boundsTop    := boundsForLayout top + ySep.
	boundsRight  := boundsForLayout right - xSep.
	boundsBottom := boundsForLayout bottom - ySep.

	submorphsToLayout size to: 1 by: -1 do: [ :index |
		nextMorph := submorphsToLayout at: index.
		"major direction"
		wd := integerWidths at: index.
		"minor direction"
		ls := nextMorph layoutSpec.
		ht := (ls heightFor: usableHeight) min: usableHeight.
		theTop := ((usableHeight - ht) * ls minorDirectionPadding) floor + boundsTop.
		theBottom := (theTop + ht) ceiling min: boundsBottom.
		theRight := (theLeft + (wd min: minWidth)) "ceiling "min: boundsRight.
		"Set bounds and adjust major direction for next step"
		self flag: #jmvVer2.	"should extent be set in m's coordinate system? what if its scale is not 1?"
		ls usesMorphExtent
			ifTrue: [
				nextMorph morphPosition: theLeft floor @ theTop floor ]
			ifFalse: [
				nextMorph morphPosition: theLeft floor @ theTop floor extent: theRight - theLeft @ (theBottom - theTop) ].
		theLeft := theRight + xSep
	]
</details>

## LayoutSpec

LayoutSpecs are the basis for the layout mechanism. Any Morph can be given a LayoutSpec, but in order to honor it, its owner must be a LayoutMorph. A LayoutSpec specifies how a morph wants to be layed out. It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible. Alternatives: - proportionalWidth notNil, fixedWidth notNil -> Use fraction of available space, take fixedWidth as minimum desired width - proportionalWidth isNil, fixedWidth isNil -> Use current morph width - proportionalWidth isNil, fixedWidth notNil -> Use fixedWidth - proportionalWidth notNil, fixedWidth isNil -> NOT VALID Same goes for proportionalHeight and fixedHeight

### Methods
#### LayoutSpec>>#minorDirectionPadding

<details>
	<summary>See more</summary>
	
	minorDirectionPadding
	^minorDirectionPadding
</details>

#### LayoutSpec>>#proportionalWidth: aNumberOrNil minimum: otherNumberOrNil

Alternatives: - proportionalWidth notNil, fixedWidth notNil -> Use fraction of available space, take fixedWidth as minimum desired width - proportionalWidth isNil, fixedWidth isNil -> Use current morph width - proportionalWidth isNil, fixedWidth notNil -> Use fixedWidth - proportionalWidth notNil, fixedWidth isNil -> NOT VALID


<details>
	<summary>See more</summary>
	
	proportionalWidth: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil 	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil  	->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil 	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil 	->		NOT VALID"
	proportionalWidth _ aNumberOrNil.
	fixedWidth _ otherNumberOrNil
</details>

#### LayoutSpec>>#fixedHeight: aNumber

aNumber is taken as the fixed height to use. No proportional part.


<details>
	<summary>See more</summary>
	
	fixedHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight _ aNumber.
	proportionalHeight _ nil
</details>

#### LayoutSpec>>#proportionalWidth: aNumber

<details>
	<summary>See more</summary>
	
	proportionalWidth: aNumber
	^self proportionalWidth: aNumber minimum: 0
</details>

#### LayoutSpec>>#usesMorphExtent

Does not attempt to layout width or height. Uses current morph extent.


<details>
	<summary>See more</summary>
	
	usesMorphExtent
	"Does not attempt to layout width or height. Uses current morph extent."

	^fixedWidth isNil and: [ proportionalWidth isNil 
		and: [ fixedHeight isNil and: [ proportionalHeight isNil ]]]
</details>

#### LayoutSpec>>#fixedOrMinimumLayoutHeight

<details>
	<summary>See more</summary>
	
	fixedOrMinimumLayoutHeight
	^fixedHeight ifNil: [ morph morphHeight ]
</details>

#### LayoutSpec>>#proportionalHeight: aNumber

<details>
	<summary>See more</summary>
	
	proportionalHeight: aNumber
	^self proportionalHeight: aNumber minimum: 0.0
</details>

#### LayoutSpec>>#fixedOrMorphWidth: aNumber

aNumber is taken as the fixed width to use. No proportional part.


<details>
	<summary>See more</summary>
	
	fixedOrMorphWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth
		ifNotNil: [ fixedWidth _ aNumber ]
		ifNil: [ morph morphWidth: aNumber ].
	proportionalWidth _ nil
</details>

#### LayoutSpec>>#useMorphWidth

Do not attempt to layout width. Use current morph width if at all possible.


<details>
	<summary>See more</summary>
	
	useMorphWidth
	"Do not attempt to layout width. Use current morph width if at all possible."
	fixedWidth _ nil.
	proportionalWidth _ nil
</details>

#### LayoutSpec>>#minimumLayoutWidth

Generally prefer asking the morph itself!


<details>
	<summary>See more</summary>
	
	minimumLayoutWidth
	"Generally prefer asking the morph itself!"

	proportionalWidth ifNil: [ ^0 ].
	^fixedWidth ifNil: [0]
</details>

#### LayoutSpec>>#usesMorphHeight

Does not attempt to layout height. Uses current morph height.


<details>
	<summary>See more</summary>
	
	usesMorphHeight
	"Does not attempt to layout height. Uses current morph height."

	^fixedHeight isNil and: [ proportionalHeight isNil ]
</details>

#### LayoutSpec>>#minimumLayoutHeight

Generally prefer asking the morph itself!


<details>
	<summary>See more</summary>
	
	minimumLayoutHeight
	"Generally prefer asking the morph itself!"

	proportionalHeight ifNil: [ ^0 ].
	^fixedHeight ifNil: [0]
</details>

#### LayoutSpec>>#proportionalLayoutWidth

<details>
	<summary>See more</summary>
	
	proportionalLayoutWidth

	^ proportionalWidth ifNil: [ 0 ]
</details>

#### LayoutSpec>>#setProportionalWidth: aNumberOrNil

Alternatives: - proportionalWidth notNil, fixedWidth notNil -> Use fraction of available space, take fixedWidth as minimum desired width - proportionalWidth isNil, fixedWidth isNil -> Use current morph width - proportionalWidth isNil, fixedWidth notNil -> Use fixedWidth - proportionalWidth notNil, fixedWidth isNil -> NOT VALID


<details>
	<summary>See more</summary>
	
	setProportionalWidth: aNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil		->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil		->		NOT VALID"
	proportionalWidth _ aNumberOrNil
</details>

#### LayoutSpec>>#privateFixedHeight

<details>
	<summary>See more</summary>
	
	privateFixedHeight

	^fixedHeight
</details>

#### LayoutSpec>>#heightFor: availableSpace

If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. Otherwise, we do proportional layout, and the stored extent is a minimum extent. If there is no minimum extent, it should be set to zero.


<details>
	<summary>See more</summary>
	
	heightFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalHeight isNil
		ifTrue: [fixedHeight ifNil: [morph morphHeight]]
		ifFalse: [proportionalHeight * availableSpace + morph minimumLayoutExtent y]
</details>

#### LayoutSpec>>#initialize

Just some reasonable defaults, use all available space


<details>
	<summary>See more</summary>
	
	initialize
	"Just some reasonable defaults, use all available space"
	minorDirectionPadding _ 0.5.
	fixedWidth _ 0.
	fixedHeight _ 0.
	proportionalWidth _ 1.0.
	proportionalHeight _ 1.0
</details>

#### LayoutSpec>>#usesMorphWidth

Does not attempt to layout width. Uses current morph width.


<details>
	<summary>See more</summary>
	
	usesMorphWidth
	"Does not attempt to layout width. Uses current morph width."

	^fixedWidth isNil and: [ proportionalWidth isNil ]
</details>

#### LayoutSpec>>#proportionaLayoutlHeight

<details>
	<summary>See more</summary>
	
	proportionaLayoutlHeight

	^ proportionalHeight ifNil: [ 0 ]
</details>

#### LayoutSpec>>#isProportionalWidth

<details>
	<summary>See more</summary>
	
	isProportionalWidth

	^ proportionalWidth notNil
</details>

#### LayoutSpec>>#useMorphHeight

Do not attempt to layout height. Use current morph height if at all possible.


<details>
	<summary>See more</summary>
	
	useMorphHeight
	"Do not attempt to layout height. Use current morph height if at all possible."
	fixedHeight _ nil.
	proportionalHeight _ nil
</details>

#### LayoutSpec>>#isProportionalHeight

<details>
	<summary>See more</summary>
	
	isProportionalHeight

	^ proportionalHeight notNil
</details>

#### LayoutSpec>>#setProportionalHeight: aNumberOrNil

Alternatives: same as in #proportionalWidth:minimum:, see comment there


<details>
	<summary>See more</summary>
	
	setProportionalHeight: aNumberOrNil
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight _ aNumberOrNil
</details>

#### LayoutSpec>>#privateProportionalHeight

<details>
	<summary>See more</summary>
	
	privateProportionalHeight

	^ proportionalHeight
</details>

#### LayoutSpec>>#fixedOrMorphHeight: aNumber

aNumber is taken as the fixed height to use. No proportional part.


<details>
	<summary>See more</summary>
	
	fixedOrMorphHeight: aNumber
	"aNumber is taken as the fixed height to use.
	No proportional part."
	fixedHeight
		ifNotNil: [ fixedHeight _ aNumber ]
		ifNil: [ morph morphHeight: aNumber ].
	proportionalHeight _ nil
</details>

#### LayoutSpec>>#widthFor: availableSpace

If proportional is zero, answer stored fixed extent, or actual morph extent if undefined. Otherwise, we do proportional layout, and the stored extent is a minimum extent. If there is no minimum extent, it should be set to zero.


<details>
	<summary>See more</summary>
	
	widthFor: availableSpace
	"If proportional is zero, answer stored fixed extent, or actual morph extent if undefined.
	Otherwise, we do proportional layout, and the stored extent is a minimum extent.
	If there is no minimum extent, it should be set to zero."

	^proportionalWidth isNil
		ifTrue: [fixedWidth ifNil: [morph morphWidth]]
		ifFalse: [proportionalWidth * availableSpace + morph minimumLayoutExtent x]
</details>

#### LayoutSpec>>#fixedOrMinimumLayoutWidth

<details>
	<summary>See more</summary>
	
	fixedOrMinimumLayoutWidth
	^fixedWidth ifNil: [ morph morphWidth ]
</details>

#### LayoutSpec>>#fixedWidth: aNumber

aNumber is taken as the fixed width to use. No proportional part.


<details>
	<summary>See more</summary>
	
	fixedWidth: aNumber
	"aNumber is taken as the fixed width to use.
	No proportional part."
	fixedWidth _ aNumber.
	proportionalWidth _ nil
</details>

#### LayoutSpec>>#minorDirectionPadding: aSymbolOrNumber

This sets how padding is done in the secondary direction. For instance, if the owning morph is set in a row, the row will control horizontal layout. But if there is unused vertical space, it will be used according to this parameter. For instance, #top sets the owning morph at the top. Same for #bottom and #center. If the owner is contained in a column, #left, #center or #right should be used. Alternatively, any number between 0.0 and 1.0 can be used. self new minorDirectionPadding: #center self new minorDirectionPadding: 0.9


<details>
	<summary>See more</summary>
	
	minorDirectionPadding: aSymbolOrNumber
	"This sets how padding is done in the secondary direction. For instance, if the owning morph is set in a row, the row will control horizontal layout. But if there is unused vertical space, it will be used according to this parameter. For instance, #top sets the owning morph at the top. Same for #bottom and #center. If the owner is contained in a column, #left, #center or #right should be used. Alternatively, any number between 0.0 and 1.0 can be used.
	self new minorDirectionPadding: #center
	self new minorDirectionPadding: 0.9
	"
	minorDirectionPadding _ aSymbolOrNumber
		caseOf: {
			[ #top ] -> [ 0.0 ].
			[ #left ] -> [ 0.0 ].
			[ #center ] -> [ 0.5 ].
			[ #right ] -> [ 1.0 ].
			[ #bottom ] -> [ 1.0 ]
		}
		otherwise: [ aSymbolOrNumber ]
</details>

#### LayoutSpec>>#morph: aMorph

<details>
	<summary>See more</summary>
	
	morph: aMorph
	morph _ aMorph
</details>

#### LayoutSpec>>#proportionalHeight: aNumberOrNil minimum: otherNumberOrNil

Alternatives: same as in #proportionalWidth:minimum:, see comment there


<details>
	<summary>See more</summary>
	
	proportionalHeight: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives: same as in #proportionalWidth:minimum:, see comment there"
	proportionalHeight _ aNumberOrNil.
	fixedHeight _ otherNumberOrNil
</details>

## WindowEdgeAdjustingMorph

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### WindowEdgeAdjustingMorph>>#isOpaqueMorph

Not really needed, as we also answer false to #isOrthoRectangularMorph


<details>
	<summary>See more</summary>
	
	isOpaqueMorph
	"Not really needed, as we also answer false to #isOrthoRectangularMorph"
	^false
</details>

#### WindowEdgeAdjustingMorph>>#initializeTop

<details>
	<summary>See more</summary>
	
	initializeTop
	selector _ #windowTop:.
	coordinateGetter _ #y.
	cursorKey _ #resizeTopCursor
</details>

#### WindowEdgeAdjustingMorph>>#isOrthoRectangularMorph

Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and specified by my #morphExtent. If true, #morphContainsPoint: can simply check #morphExtent.


<details>
	<summary>See more</summary>
	
	isOrthoRectangularMorph
	^false
</details>

#### WindowEdgeAdjustingMorph>>#cursor

<details>
	<summary>See more</summary>
	
	cursor
	^ Cursor cursorAt: cursorKey.
</details>

#### WindowEdgeAdjustingMorph>>#initializeRight

<details>
	<summary>See more</summary>
	
	initializeRight
	selector _ #windowRight:.
	coordinateGetter _ #x.
	cursorKey _ #resizeRightCursor
</details>

#### WindowEdgeAdjustingMorph>>#initializeBottom

<details>
	<summary>See more</summary>
	
	initializeBottom
	selector _ #windowBottom:.
	coordinateGetter _ #y.
	cursorKey _ #resizeBottomCursor
</details>

#### WindowEdgeAdjustingMorph>>#morphContainsPoint: aLocalPoint

If not visible, won't contain any point at all.


<details>
	<summary>See more</summary>
	
	morphContainsPoint: aLocalPoint
	| sensitiveBorder |
	( self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	sensitiveBorder _ 4.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]
</details>

#### WindowEdgeAdjustingMorph>>#initializeBottomLeft

<details>
	<summary>See more</summary>
	
	initializeBottomLeft
	selector _ #windowBottomLeft:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeBottomLeftCursor
</details>

#### WindowEdgeAdjustingMorph>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas
</details>

#### WindowEdgeAdjustingMorph>>#initializeTopRight

<details>
	<summary>See more</summary>
	
	initializeTopRight
	selector _ #windowTopRight:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeTopRightCursor
</details>

#### WindowEdgeAdjustingMorph>>#initializeLeft

<details>
	<summary>See more</summary>
	
	initializeLeft
	selector _ #windowLeft:.
	coordinateGetter _ #x.
	cursorKey _ #resizeLeftCursor
</details>

#### WindowEdgeAdjustingMorph>>#initializeTopLeft

<details>
	<summary>See more</summary>
	
	initializeTopLeft
	selector _ #windowTopLeft:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeTopLeftCursor
</details>

#### WindowEdgeAdjustingMorph>>#adjustOwnerAt: aPoint

<details>
	<summary>See more</summary>
	
	adjustOwnerAt: aPoint
	| p |
	owner ifNotNil: [
		p _ aPoint + 1.
		owner owner ifNotNil: [ :parent |
			p _ parent internalizeFromWorld: p ].
		owner perform: selector with: (p perform: coordinateGetter) ]
</details>

#### WindowEdgeAdjustingMorph>>#initializeBottomRight

<details>
	<summary>See more</summary>
	
	initializeBottomRight
	selector _ #windowBottomRight:.
	coordinateGetter _ #yourself.
	cursorKey _ #resizeBottomRightCursor
</details>

