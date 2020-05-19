## BitBltCanvas

origin is aPoint, and it is expressed relative to the form. Usually, the form doesn't cover the whole World, and origin is negative. For instance, if we just want to draw the part of the World in (100@100 extent: 50@50) to another form, then fom extent = 50@50, and origin = -100@-100. clipRect is relative to the form. For instance, if we only need to draw the part in (110@110 extent: 20@20) to the form above, then clipRect is (10@10 extent: 20@20) All public protocol and drawing services is relative to World. The idea is that we only care about origin/form when we set them. Afterwards, we don't need to care about them. All the operations are done as if the whole World was drawn on Display.

### Methods
#### BitBltCanvas>>#roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h

Display restore. BitBltCanvas releaseClassCachedState. Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35


<details>
	<summary>See more</summary>
	
	roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor
</details>

#### BitBltCanvas>>#image: aForm multipliedBy: aColor at: aPoint

Multiply aForm and aColor, then blend over destination. aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel Display getCanvas image: (Form makeStar asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen


<details>
	<summary>See more</summary>
	
	image: aForm multipliedBy: aColor at: aPoint
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	Display getCanvas image: (Form makeStar asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		self class accessProtect critical: [
			self buildAuxWith: aForm multipliedWith: aColor.
			self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]]
</details>

#### BitBltCanvas>>#roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1

Display restore. BitBltCanvas releaseClassCachedState. Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35


<details>
	<summary>See more</summary>
	
	roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		image: (self class topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (self class bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (self class bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		tilingWith: (self class verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.
</details>

#### BitBltCanvas>>#setClipRect: aRectangle

In targetForm coordinates


<details>
	<summary>See more</summary>
	
	setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle
</details>

#### BitBltCanvas>>#frameRectangle: r borderWidth: borderWidth color: borderColor

Display getCanvas frameRectangle: (10@10 extent: 300@200) borderWidth: 20 color: Color red. Display forceToScreen


<details>
	<summary>See more</summary>
	
	frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalizeRectangle: r) rounded.
		bw _ currentTransformation externalizeScalar: borderWidth.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]
</details>

#### BitBltCanvas>>#isCurrentMorphVisible

<details>
	<summary>See more</summary>
	
	isCurrentMorphVisible
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	"#clippingRectForCurrentMorph is valid even before drawing currentMorph, only in BitBltCanvas!"
	aRectangle := self clippingRectForCurrentMorph.
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true

</details>

#### BitBltCanvas>>#resetEngine

Private! Create a new BitBltCanvasEngine for a new copy.


<details>
	<summary>See more</summary>
	
	resetEngine
	"Private! Create a new BitBltCanvasEngine for a new copy."

	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0
</details>

#### BitBltCanvas>>#setForm: aForm

<details>
	<summary>See more</summary>
	
	setForm: aForm
	super setForm: aForm.
	self resetEngine.
	self newClipRect: nil.
</details>

#### BitBltCanvas>>#windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor

BitBltCanvas releaseClassCachedState. Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10 border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green. Display forceToScreen


<details>
	<summary>See more</summary>
	
	windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState.
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		image: (self class topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor
</details>

#### BitBltCanvas>>#buildAuxWith: aForm multipliedWith: aColor

<details>
	<summary>See more</summary>
	
	buildAuxWith: aForm multipliedWith: aColor
	| h w r |
	w _ aForm width.
	h _ aForm height.
	AuxForm
		ifNotNil: [
			w _ w max: AuxForm width.
			h _ h max: AuxForm height.
			(AuxForm width < w or: [ AuxForm height < h ]) ifTrue: [ AuxForm _ nil ]].
	AuxForm
		ifNil: [
			AuxForm _ Form extent: w@h depth: 32.
			AuxBlitter _ BitBlt toForm: AuxForm ].
	
	r _ aForm boundingBox.
	AuxForm fill: r fillColor: aColor.
	AuxBlitter
		sourceForm: aForm;
		combinationRule: Form rgbMul;
		sourceRect: r;
		copyBits.
</details>

#### BitBltCanvas>>#frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol

Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white. Display forceToScreen. Display getCanvas frameRectangle: (10@10 extent: 300@200) color: Color green borderWidth: 10 borderStyleSymbol: #raised. Display forceToScreen.


<details>
	<summary>See more</summary>
	
	frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white. Display forceToScreen.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: r) rounded.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: r) rounded.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self frameRectangle: r borderWidth: bw color: aColor
</details>

#### BitBltCanvas>>#fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor

Fill aRectangle with the equivalent of aForm multiplied by aColor aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel Similar to #image:multipliedBy:at: Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.


<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"

	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]
</details>

#### BitBltCanvas>>#roundRect: aRectangle color: aColor radius: r

Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10


<details>
	<summary>See more</summary>
	
	roundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"
	"radious is not scaled properly..."
	"top stripe"
	self
		image: (self class topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor
</details>

#### BitBltCanvas>>#image: aForm at: aPoint sourceRect: sourceRect

Draw a translucent image using the best available way of representing translucency. Note: This will be fixed in the future.


<details>
	<summary>See more</summary>
	
	image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(self depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]
</details>

#### BitBltCanvas>>#reverseRectangleBorder: r borderWidth: borderWidth

Display getCanvas reverseRectangleBorder: (10@10 extent: 300@200) borderWidth: 20. Display forceToScreen


<details>
	<summary>See more</summary>
	
	reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| rect |
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth
</details>

#### BitBltCanvas>>#fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol
</details>

#### BitBltCanvas>>#stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor

Flood this canvas with aColor wherever stencilForm has non-zero pixels


<details>
	<summary>See more</summary>
	
	stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ (currentTransformation transform: aPoint) rounded.
	self setPaintColor: aColor.
	engine colorMap: stencilForm maskingMap.
	engine stencil: stencilForm
		at: p
		sourceRect: sourceRect
</details>

#### BitBltCanvas>>#frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

<details>
	<summary>See more</summary>
	
	frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |

	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	bw _ currentTransformation externalizeScalar: borderWidth.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]
</details>

#### BitBltCanvas>>#stencil: stencilForm at: aPoint color: aColor

Flood this canvas with aColor wherever stencilForm has non-zero pixels Display getCanvas stencil: (Form makeStar asFormOfDepth: 1) at: 20@20 color: Color red. Display forceToScreen


<details>
	<summary>See more</summary>
	
	stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels
	
	Display getCanvas stencil: (Form makeStar asFormOfDepth: 1) at: 20@20 color: Color red. Display forceToScreen
	"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor
</details>

#### BitBltCanvas>>#fillRectangle: aRectangle color: aColor

Fill the given rectangle.


<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: `Color transparent` ]
</details>

#### BitBltCanvas>>#clippingRectForCurrentMorph

In targetForm coordinates


<details>
	<summary>See more</summary>
	
	clippingRectForCurrentMorph
	"In targetForm coordinates"

	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds
</details>

#### BitBltCanvas>>#frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor

rect and borderWidth are in targetForm coordinates. No transformation is done. Display getCanvas frameRectangle: (10@10 extent: 300@200) borderWidth: 20 topLeftColor: Color green bottomRightColor: Color red Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white


<details>
	<summary>See more</summary>
	
	frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in targetForm coordinates. No transformation is done.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white
	"
	| w h |

	rect area = 0 ifTrue: [^ self].

	self setPaintColor: topLeftColor.

	engine frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	engine 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h
</details>

#### BitBltCanvas>>#setPaintColor: aColor

Install a new color used for filling.


<details>
	<summary>See more</summary>
	
	setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ aColor ifNil: [ `Color transparent` ].
	(paintColor is: #Color) ifFalse: [
		^ self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	engine sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		engine fillColor: paintColor.
		engine combinationRule: Form paint.
		^ self ].

	"BitBlt setup for alpha mapped transfer"
	engine fillColor: paintColor.
	engine combinationRule: Form blend
</details>

#### BitBltCanvas>>#initializeWith: aForm origin: aPoint

<details>
	<summary>See more</summary>
	
	initializeWith: aForm origin: aPoint

	super initializeWith: aForm origin: aPoint.
	self resetEngine
</details>

#### BitBltCanvas>>#fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

	| displayRectangle  additionalOffset clippedEngine targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ (currentTransformation externalizeRectangle: aRectangle) rounded.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ `0@0`.
	clippedEngine _ engine clippedBy: displayRectangle.
	targetTopLeft _ clippedEngine clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedEngine clipRect.
	savedMap _ clippedEngine colorMap.
	clippedEngine sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedEngine destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedEngine destOrigin: x@y; copyBits]].
	clippedEngine colorMap: savedMap
</details>

#### BitBltCanvas>>#line: pt1 to: pt2 width: wp color: c

<details>
	<summary>See more</summary>
	
	line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ currentTransformation externalizeScalar: wp.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]
</details>

#### BitBltCanvas>>#drawButtonIconFromCurrentMorph

We assume that we are drawing a PluggableButtonMorph, or some morph that conforms the required protocol. Answer true if we were able to draw it.


<details>
	<summary>See more</summary>
	
	drawButtonIconFromCurrentMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	currentMorph magnifiedIcon ifNotNil: [ :theIcon |
		self
			image: theIcon
			multipliedBy: currentMorph iconColor
			at: (currentMorph morphExtent - theIcon extent //2).
		^true ].
	^false
</details>

#### BitBltCanvas>>#drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor
</details>

#### BitBltCanvas>>#image: aForm at: aPoint

Draw a translucent image using the best available way of representing translucency.


<details>
	<summary>See more</summary>
	
	image: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox
</details>

#### BitBltCanvas>>#ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

<details>
	<summary>See more</summary>
	
	ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw) ]]
</details>

## BitBltCanvasEngine

Add services for BitBltCanvas

### Methods
#### BitBltCanvasEngine>>#stencil: stencilForm at: aPoint sourceRect: aRect

Paint using aColor wherever stencilForm has non-zero pixels


<details>
	<summary>See more</summary>
	
	stencil: stencilForm at: aPoint sourceRect: aRect
	"Paint using aColor wherever stencilForm has non-zero pixels"
	self sourceForm: stencilForm;
		destOrigin: aPoint;
		sourceRect: aRect.
	self copyBits
</details>

#### BitBltCanvasEngine>>#frameOval: rect borderWidth: borderWidth

<details>
	<summary>See more</summary>
	
	frameOval: rect borderWidth: borderWidth
	| centerX centerY nextY yBias xBias wp outer inner nextOuterX nextInnerX fillAlpha |
	rect area <= 0 ifTrue: [^ self].
	height _ 1.
	wp _ borderWidth asPoint.
	yBias _ rect height odd ifTrue: [0] ifFalse: [-1].
	xBias _ rect width odd ifTrue: [1] ifFalse: [0].
	centerX _ rect center x.
	centerY _ rect center y.
	outer _ EllipseMidpointTracer new on: rect.
	inner _ EllipseMidpointTracer new on: (rect insetBy: wp).
	nextY _ rect height // 2.
	1 to: (wp y min: nextY) do:[:i|
		nextOuterX _ outer stepInY.
		width _ (nextOuterX bitShift: 1) + xBias.
		destX _ centerX - nextOuterX.
		destY _ centerY - nextY.
		self copyBits.
		destY _ centerY + nextY + yBias.
		self copyBits.
		nextY _ nextY - 1.
	].
	[nextY > 0] whileTrue:[
		nextOuterX _ outer stepInY.
		nextInnerX _ inner stepInY.
		destX _ centerX - nextOuterX.
		destY _ centerY - nextY.
		width _ nextOuterX - nextInnerX.
		self copyBits.
		destX _ centerX + nextInnerX + xBias.
		self copyBits.
		destX _ centerX - nextOuterX.
		destY _ centerY + nextY + yBias.
		self copyBits.
		destX _ centerX + nextInnerX + xBias.
		self copyBits.
		nextY _ nextY - 1.
	].
	destY _ centerY.
	height _ 1 + yBias.
	width _ wp x.
	destX _ rect left.
	self copyBits.
	destX _ rect right - wp x.
	self copyBits.

</details>

#### BitBltCanvasEngine>>#frameRectTopLeft: rect borderWidth: borderWidth

Paint the top and left edges of a border whose rectangular area is defined by rect. The width of the border of each side is borderWidth.


<details>
	<summary>See more</summary>
	
	frameRectTopLeft: rect borderWidth: borderWidth
	"Paint the top and left edges of a border whose rectangular area is defined by rect.
	 The width of the border of each side is borderWidth."

	sourceX _ 0.
	sourceY _ 0.
	
	"top"
	height _ borderWidth. 
	width _ rect width. 
	destX _ rect left.
	destY _ rect top.
	self copyBits.

	"left"
	height _ rect height. 
	width _ borderWidth. 
	destY _ rect top.
	destX _ rect left.
	self copyBits
</details>

#### BitBltCanvasEngine>>#colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix

Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same


<details>
	<summary>See more</summary>
	
	colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: keepSubPix
	"Note: The color converting map for sourceDepth=16 and for sourceDepth=32 are the same"
	
	| srcIndex dstIndex map mapsForSource mapsForSourceAndDest |
	ColorConvertingMaps 
		ifNil: [ColorConvertingMaps _ (1 to: 6) collect: [:i | Array new: 6]].

	srcIndex _ sourceDepth highBit.
	sourceDepth > 8 ifTrue: [ srcIndex _ keepSubPix ifTrue: [5] ifFalse: [6] ].
	dstIndex _ destDepth highBit.
	
	mapsForSource _ ColorConvertingMaps at: srcIndex.
	(mapsForSourceAndDest _ mapsForSource at: dstIndex) ifNil: [
		mapsForSourceAndDest _ mapsForSource at: dstIndex put: Dictionary new ].
	
	map _ mapsForSourceAndDest at: targetColor ifAbsentPut: [
		Color 
			computeColorConvertingMap: targetColor 
			from: sourceDepth 
			to: destDepth 
			keepSubPixelAA: keepSubPix ].

	^ map
</details>

#### BitBltCanvasEngine>>#frameRect: rect borderWidth: borderWidth

Paint a border whose rectangular area is defined by rect. The width of the border of each side is borderWidth.


<details>
	<summary>See more</summary>
	
	frameRect: rect borderWidth: borderWidth
	"Paint a border whose rectangular area is defined by rect. The
	width of the border of each side is borderWidth."

	rect area = 0 ifTrue: [^ self].

	sourceX _ 0.
	sourceY _ 0.
	
	"for top and bottom, the following are the same"
	height _ borderWidth. 
	width _ rect width. 
	destX _ rect left.

	"top"
	destY _ rect top.
	self copyBits.

	"bottom"
	destY _ rect bottom - borderWidth.
	self copyBits.

	"for left & right, the following are the same"
	height _ rect height-borderWidth-borderWidth.
	width _ borderWidth. 
	destY _ rect top+borderWidth.

	"left"
	destX _ rect left.
	self copyBits.

	"right"
	destX _ rect right - borderWidth.
	self copyBits
</details>

#### BitBltCanvasEngine>>#clippedBy: aRectangle

<details>
	<summary>See more</summary>
	
	clippedBy: aRectangle
	^ self copy clipBy: aRectangle
</details>

#### BitBltCanvasEngine>>#image: aForm at: aPoint sourceRect: sourceRect rule: rule

Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule.


<details>
	<summary>See more</summary>
	
	image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."

	sourceForm _ aForm.
	combinationRule _ rule.
	self sourceRect: sourceRect.
	self destOrigin: aPoint.
	self copyBits
</details>

#### BitBltCanvasEngine>>#fillRect: rect

<details>
	<summary>See more</summary>
	
	fillRect: rect

	rect area = 0 ifTrue: [^ self].
	destX _ rect left.
	destY _ rect top.
	sourceX _ 0.
	sourceY _ 0.
	width _ rect width.
	height _ rect height.
	self copyBits
</details>

#### BitBltCanvasEngine>>#installStrikeFont: aStrikeFont foregroundColor: foregroundColor

<details>
	<summary>See more</summary>
	
	installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	sourceForm _ aStrikeFont glyphs.
	sourceY _ 0.
	height _ sourceForm height.
	self setRuleAndMapFor: sourceForm depth foregroundColor: foregroundColor
</details>

#### BitBltCanvasEngine>>#setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

<details>
	<summary>See more</summary>
	
	setRuleAndMapFor: sourceDepth foregroundColor: foregroundColor

	| targetColor destDepth |
	destDepth _ destForm depth.
	halftoneForm _ nil.	"Don't use fillColor. Use a more powerful ColorMap"

	sourceDepth = 1 ifTrue: [
		self combinationRule: Form paint.
		"Set up color map for a different source depth (color font)"
		"Uses caching for reasonable efficiency"
		colorMap _ self cachedFontColormapFrom1BitTo: destDepth.
		colorMap at: 1 put: (destForm pixelValueFor: `Color transparent`).
		colorMap at: 2 put: (destForm pixelValueFor: foregroundColor) ]
	
	ifFalse: [
		"Enable subpixel rendering if requested, but never for translucent text:
		This technique always draws opaque text. This could be added, by using an extra colormap for the rgbMul phase...
		So far, no need arised for doing so."
		(sourceDepth > 8 and: [
			Preferences subPixelRenderFonts and: [ foregroundColor = `Color black` or: [ 
				Preferences subPixelRenderColorFonts and: [ foregroundColor isOpaque ]]]]) ifTrue: [
			destDepth > 8 ifTrue: [
				"rgbMul is equivalent to component alpha blend if text is black (only faster, hehe)"
				self combinationRule: 37.		"rgbMul"
				colorMap _ (foregroundColor ~= `Color black` or: [
						destDepth = 32 and: [ destForm ~~ Display or: [Preferences properDisplayAlphaForFonts] ]]) ifTrue: [
					"rgbMul / rgbAdd IS component alpha blend for any color of text (neat trick, eh!)"
					"This colorMap is to be used on the second pass with rule 20 (rgbAdd)
					See #displayString:from:to:at:strikeFont:color:"
					"Note: In 32bpp, if we want the correct alpha in the result, we need the second pass, as the destination could have transparent pixels, 
					and we need to add to the alpha channel"
					self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
			ifFalse: [
				self combinationRule: 25.		"Paint"
				targetColor _ foregroundColor = `Color black` ifFalse: [ foregroundColor ].
				colorMap _ self colorConvertingMap: targetColor from: sourceDepth to: destDepth keepSubPixelAA: true]]
		ifFalse: [
			"Do not use rule 34 for 16bpp display."
			self combinationRule: (destDepth = 32 ifTrue: [34 "alphaBlendScaled"] ifFalse: [25 "Paint"]).
			colorMap _ self colorConvertingMap: foregroundColor from: sourceDepth to: destDepth keepSubPixelAA: false]]
</details>

#### BitBltCanvasEngine>>#frameRectBottom: rect height: h

<details>
	<summary>See more</summary>
	
	frameRectBottom: rect height: h

	destX _ rect left + 1.
	destY _ rect bottom - 1.
	width _ rect width - 2.
	height _ 1.
	1 to: h do: [:i |
		self copyBits.
		destX _ destX + 1.
		destY _ destY - 1.
		width _ width - 2].

</details>

#### BitBltCanvasEngine>>#cachedFontColormapFrom1BitTo: destDepth

<details>
	<summary>See more</summary>
	
	cachedFontColormapFrom1BitTo: destDepth

	| map dstIndex |
	CachedFontColorMaps 
		ifNil: [CachedFontColorMaps _ Array new: 6].

	dstIndex _ destDepth highBit.
	(CachedFontColorMaps at: dstIndex) ifNotNil: [ :m | ^ m ].

	map _ (Color cachedColormapFrom: 1 to: destDepth) copy.
	CachedFontColorMaps at: dstIndex put: map.
	^ map
</details>

#### BitBltCanvasEngine>>#fillOval: rect

<details>
	<summary>See more</summary>
	
	fillOval: rect
	| centerX centerY nextY yBias xBias outer nextOuterX |
	rect area <= 0 ifTrue: [^ self].
	height _ 1.
	yBias _ rect height odd ifTrue: [0] ifFalse: [-1].
	xBias _ rect width odd ifTrue: [1] ifFalse: [0].
	centerX _ rect center x.
	centerY _ rect center y.
	outer _ EllipseMidpointTracer new on: rect.
	nextY _ rect height // 2.
	[nextY > 0] whileTrue:[
		nextOuterX _ outer stepInY.
		width _ (nextOuterX bitShift: 1) + xBias.
		destX _ centerX - nextOuterX.
		destY _ centerY - nextY.
		self copyBits.
		destY _ centerY + nextY + yBias.
		self copyBits.
		nextY _ nextY - 1.
	].
	destY _ centerY.
	height _ 1 + yBias.
	width _ rect width.
	destX _ rect left.
	self copyBits.

</details>

#### BitBltCanvasEngine>>#displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont color: foregroundColor

If required, do a second pass with new rule and colorMap. Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer last affected pixel position
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	self installStrikeFont: aStrikeFont foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
		combinationRule := prevRule ].
	^answer
</details>

#### BitBltCanvasEngine>>#basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font

Answer last affected pixel position


<details>
	<summary>See more</summary>
	
	basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font
	"Answer last affected pixel position"

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: font baseKern.
	^ destX@(destY+font lineSpacing)
</details>

#### BitBltCanvasEngine>>#frameRectRight: rect width: w

<details>
	<summary>See more</summary>
	
	frameRectRight: rect width: w

	width _ 1.
	height _ rect height - 1.
	destX _ rect right - 1.
	destY _ rect top + 1.
	1 to: w do: [:i |
		self copyBits.
		destX _ destX - 1.
		destY _ destY + 1.
		height _ height - 2].

</details>

## DamageRecorder

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DamageRecorder>>#invalidRectsFullBounds: aRectangle

Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle.


<details>
	<summary>See more</summary>
	
	invalidRectsFullBounds: aRectangle
	"Return a collection of damaged rectangles for the given canvas. If a total repaint has been requested, return the given rectangle."
	"The collection answered should not be modified outside this method. In addition, it could contain nil objects, that should be ignored."
	| answer |
	answer _ totalRepaint
		ifTrue: [ Array with: aRectangle ].
		self pvtAccessProtect critical: [
			answer ifNil: [answer := invalidRects reject: [ :r |
					r isNil ]].
			self pvtReset].
	^ answer.
</details>

#### DamageRecorder>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize .
	invalidRects _ OrderedCollection new: 15.
	totalRepaint _ false
</details>

#### DamageRecorder>>#doFullRepaint

Record that a full redisplay is needed. No further damage rectangles will be recorded until after the next reset.


<details>
	<summary>See more</summary>
	
	doFullRepaint
	"Record that a full redisplay is needed. No further damage rectangles will be recorded until after the next reset."

	^ totalRepaint _ true.

</details>

#### DamageRecorder>>#updateIsNeeded

Return true if the display needs to be updated. Note: This could give a false positive (i.e. answer true) if invalidRects is not empty but it only contains nils. Senders should be aware of this.


<details>
	<summary>See more</summary>
	
	updateIsNeeded
	"Return true if the display needs to be updated.
	Note: This could give a false positive (i.e. answer true) if invalidRects is not empty but it only contains nils.
	Senders should be aware of this."
	^ totalRepaint or: [ self pvtAccessProtect critical: [invalidRects notEmpty] ].
</details>

#### DamageRecorder>>#pvtInnerRecordInvalidRect: requestedRect

Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle. Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)


<details>
	<summary>See more</summary>
	
	pvtInnerRecordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."
	| newRect mergeRect indexToReuse |
	totalRepaint ifTrue: [ ^ self ].
	"planning full repaint; don't bother collecting damage"
	indexToReuse _ nil.
	newRect _ requestedRect truncated.
	invalidRects withIndexDo: [ :oldRect :index |
		oldRect
			ifNil: [ indexToReuse ifNil: [ indexToReuse _ index ]]
			ifNotNil: [
				"No two rectangles should intersect"
				(oldRect intersects: newRect) ifTrue: [
					"newRect already in. Nothing to do then."
					(oldRect containsRect: newRect) ifTrue: [ ^ self ].
					"Some oldRect included in newRect. Remove it and continue, as newRect could still intersect others."
					(newRect containsRect: oldRect) ifTrue: [
						invalidRects
							at: index
							put: nil.
						"Effectively like 'invalidRects remove: rect', but without performance penalty."
						indexToReuse ifNil: [ indexToReuse _ index ]].
					"Merge rectangles if they overlap significantly, i.e. if the merge is not much larger than separated rectangles."
					mergeRect _ (oldRect origin min: newRect origin) corner: (oldRect corner max: newRect corner).
					mergeRect area > (newRect area + oldRect area * 2)
						ifTrue: [
							"Avoid intersections!"
							newRect
								areasOutside: oldRect
								do: [ :nonIntersection |
									"We have brand new newRects. Start all over with each of them."
									self pvtInnerRecordInvalidRect: nonIntersection ].
							"newRect no longer needed, then."
							^ self ]
						ifFalse: [
							"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
							newRect _ mergeRect.
							invalidRects
								at: index
								put: nil.
							"Effectively like 'invalidRects remove: rect', but without performance penalty."
							indexToReuse ifNil: [ indexToReuse _ index ]]]]].
	invalidRects size >= 10 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self pvtReset.
		mergeRect ifNotNil: [ invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].
	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [ invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects
				at: indexToReuse
				put: newRect ].
</details>

#### DamageRecorder>>#pvtReset

Clear the damage list.


<details>
	<summary>See more</summary>
	
	pvtReset
	"Clear the damage list."
	invalidRects removeAll.
	totalRepaint _ false.
</details>

#### DamageRecorder>>#recordInvalidRect: requestedRect

<details>
	<summary>See more</summary>
	
	recordInvalidRect: requestedRect
	^ self pvtAccessProtect critical: [ self pvtInnerRecordInvalidRect: requestedRect ]
</details>

#### DamageRecorder>>#pvtAccessProtect

<details>
	<summary>See more</summary>
	
	pvtAccessProtect
	^ drSemaphore ifNil: [drSemaphore := Semaphore forMutualExclusion]
</details>

## EllipseMidpointTracer

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### EllipseMidpointTracer>>#stepInY

Step to the next y value


<details>
	<summary>See more</summary>
	
	stepInY
	"Step to the next y value"
	inFirstRegion ifTrue:[
		"In the upper region we must step until we reach the next y value"
		[(aSquared * (y-0.5)) > (bSquared * (x+1))] whileTrue:[
			d1 < 0.0
				ifTrue:[d1 _ d1 + (bSquared * (2*x+3)).
						x _ x + 1]
				ifFalse:[d1 _ d1 + (bSquared * (2*x+3)) + (aSquared * (-2*y+2)).
						y _ y - 1.
						^x _ x + 1]].
		"Stepping into second region"
		d2 _ (bSquared * (x + 0.5) squared) + (aSquared * (y-1) squared) - (aSquared * bSquared).
		inFirstRegion _ false.
	].
	"In the lower region each step is a y-step"
	d2 < 0.0
		ifTrue:[d2 _ d2 + (bSquared * (2*x+2)) + (aSquared * (-2*y+3)).
				x _ x + 1]
		ifFalse:[d2 _ d2 + (aSquared * (-2*y+3))].
	y _ y - 1.
	^x
</details>

#### EllipseMidpointTracer>>#on: aRectangle

<details>
	<summary>See more</summary>
	
	on: aRectangle
	rect _ aRectangle.
	a _ rect width // 2.
	b _ rect height // 2.
	x _ 0.
	y _ b.
	aSquared _ a * a.
	bSquared _ b * b.
	d1 _ bSquared - (aSquared * b) + (0.25 * aSquared).
	d2 _ nil.
	inFirstRegion _ true.
</details>

## MorphicCanvas

A MorphicCanvas offers 2D drawing services. It works on a 'form', usually the Display. These services are used, for example, in #drawOn: methods. Subclasses are specific implementations. BitBltCanvas is based on BitBlt, the raster operation invented by Dan Ingalls for Smalltalk, and included in Smalltalk-80 and Squeak. VectorCanvas is based on its VectorEngine, using a novel technique for the rasterization (sampling) of vector graphics, invented by Juan Vuletich.

### Methods
#### MorphicCanvas>>#fullDrawHand: aHandMorph

Draw the full Morphic structure on us


<details>
	<summary>See more</summary>
	
	fullDrawHand: aHandMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."
	aHandMorph visible ifFalse: [^ self].
	self into: aHandMorph.
	aHandMorph fullDrawHandOn: self.
	self outOfMorph
</details>

#### MorphicCanvas>>#image: aForm multipliedBy: aColor at: aPoint

<details>
	<summary>See more</summary>
	
	image: aForm multipliedBy: aColor at: aPoint
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h

<details>
	<summary>See more</summary>
	
	roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1

<details>
	<summary>See more</summary>
	
	roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#canvasOrigin

Return the current origin for drawing operations


<details>
	<summary>See more</summary>
	
	canvasOrigin
	"Return the current origin for drawing operations"
	^ transformations first translation
</details>

#### MorphicCanvas>>#clipRect

Return the currently active clipping rectangle


<details>
	<summary>See more</summary>
	
	clipRect
	"Return the currently active clipping rectangle"
	"In targetForm coordinates"
	^ clipRect
</details>

#### MorphicCanvas>>#isCurrentMorphVisible

<details>
	<summary>See more</summary>
	
	isCurrentMorphVisible
	self subclassResponsibility
</details>

#### MorphicCanvas>>#setForm: aForm

<details>
	<summary>See more</summary>
	
	setForm: aForm
	form _ aForm.
</details>

#### MorphicCanvas>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream nextPutAll:' on: '; print: form.
</details>

#### MorphicCanvas>>#fullDraw: aMorph

Draw the full Morphic structure on us


<details>
	<summary>See more</summary>
	
	fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	
	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph"
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self ].

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	currentMorph hasSubmorphs ifTrue: [
		currentMorph clippedSubmorph ifNotNil: [ :clipped |
			self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
		currentMorph unclippedSubmorphsReverseDo: [ :m |
			self fullDraw: m ].
		].
	self outOfMorph
</details>

#### MorphicCanvas>>#extent

<details>
	<summary>See more</summary>
	
	extent

	^ form extent
</details>

#### MorphicCanvas>>#fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor

<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#roundRect: aRectangle color: aColor radius: r

<details>
	<summary>See more</summary>
	
	roundRect: aRectangle color: aColor radius: r
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#showAt: pt invalidRects: updateRects

<details>
	<summary>See more</summary>
	
	showAt: pt invalidRects: updateRects
	| blt |
	blt _ (BitBlt toForm: Display)
		sourceForm: form;
		combinationRule: Form over.
	updateRects do:
		[:rect |
		blt sourceRect: rect;
			destOrigin: rect topLeft + pt;
			copyBits]
</details>

#### MorphicCanvas>>#image: aForm at: aPoint sourceRect: sourceRect

<details>
	<summary>See more</summary>
	
	image: aForm at: aPoint sourceRect: sourceRect
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#outOfMorph

<details>
	<summary>See more</summary>
	
	outOfMorph

	drawingMorphStack at: cti put: nil.			"Don't hold any morphs that could be collected"
	cti _ cti - 1.
	currentTransformation _ transformations at: cti.
	currentMorph _ drawingMorphStack at: cti
</details>

#### MorphicCanvas>>#fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder

<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor

<details>
	<summary>See more</summary>
	
	stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

<details>
	<summary>See more</summary>
	
	frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#clippingByCurrentMorphDo: aBlock

<details>
	<summary>See more</summary>
	
	clippingByCurrentMorphDo: aBlock
	| prevClipRect |

	prevClipRect _ self clipRect.
	self setClipRect: (prevClipRect intersect: self clippingRectForCurrentMorph).
	self clippingMorph: currentMorph.
	aBlock ensure: [
		self clippingMorph: nil.
		self setClipRect: prevClipRect. ]
</details>

#### MorphicCanvas>>#clippingRectForCurrentMorph

This rectangle is used for clipping submorphs in BitBltCanvas. In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn.


<details>
	<summary>See more</summary>
	
	clippingRectForCurrentMorph
	"This rectangle is used for clipping submorphs in BitBltCanvas.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn."

	self subclassResponsibility
</details>

#### MorphicCanvas>>#drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean

Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]
</details>

#### MorphicCanvas>>#initializeWith: aForm origin: aPoint

<details>
	<summary>See more</summary>
	
	initializeWith: aForm origin: aPoint
	self initialize.
	self setForm: aForm.

	"We currently set up these only on initialization.
	This is safe (wrt walkbacks during world redraw) because a new instance is created
	each time the world is redrawn. See #drawInvalidAreasWorld:submorphs:
	Maybe this cleanup should be in an aux method that can be called each time on an existing instance..."
	currentTransformation _ MorphicTranslation withTranslation: aPoint.
	cti _ 1.
	transformations
		ifNil: [ transformations _ OrderedCollection with: currentTransformation ]
		ifNotNil: [ transformations at: cti put: currentTransformation ].
	drawingMorphStack
		ifNil: [ drawingMorphStack _ OrderedCollection with: nil ]
		ifNotNil: [ drawingMorphStack at: cti put: nil ].
</details>

#### MorphicCanvas>>#displayBoundsInWorldOf: aMorph

Answer a rectangle that completely bounds aMorph when drawn on our form. Might be larger than strictly required. - In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because: a) Morphic 2 doesn't use scaling and rotation b) Most Morphic 2 morphs have rectangular shape. - In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts.


<details>
	<summary>See more</summary>
	
	displayBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: aMorph morphLocalBounds from: aMorph
</details>

#### MorphicCanvas>>#drawButtonIconFromCurrentMorph

<details>
	<summary>See more</summary>
	
	drawButtonIconFromCurrentMorph
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

<details>
	<summary>See more</summary>
	
	drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#displayFullBoundsInWorldOf: aMorph

Answer a rectangle that completely bounds aMorph and submorphs when drawn (if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world). Might be larger than strictly required. See comment at #displayBoundsInWorldOf:


<details>
	<summary>See more</summary>
	
	displayFullBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Might be larger than strictly required. See comment at #displayBoundsInWorldOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsInWorldOf: aMorph.
	aMorph submorphsDrawingOutsideReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsInWorldOf: m) ]].

	^r
</details>

#### MorphicCanvas>>#clippingMorph: aMorph

<details>
	<summary>See more</summary>
	
	clippingMorph: aMorph
</details>

#### MorphicCanvas>>#into: aMorph

<details>
	<summary>See more</summary>
	
	into: aMorph
	| location previousLast |
	location _ aMorph location.
	currentMorph _ aMorph.
	cti _ cti + 1.
	transformations size < cti
		ifTrue: [
			drawingMorphStack add: aMorph.
			currentTransformation _ currentTransformation composedWith: location.
			transformations add: currentTransformation ]
		ifFalse: [
			drawingMorphStack at: cti put: aMorph.
			previousLast _ currentTransformation.
			currentTransformation _ transformations at: cti.
			"reuse the instance if possible"
			(previousLast class == location class and: [ previousLast class == currentTransformation class ])
				ifTrue: [
					previousLast composedWith: location into: currentTransformation ]
				ifFalse: [
					currentTransformation _ previousLast composedWith: location.
					transformations at: cti put: currentTransformation ]]
</details>

#### MorphicCanvas>>#setClipRect: aRectangle

by convention, aRectangle includes left and top but does not include right and bottom. We do draw clipRight and clipBottom but not beyond.


<details>
	<summary>See more</summary>
	
	setClipRect: aRectangle
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipRect := aRectangle
</details>

#### MorphicCanvas>>#drawCurrentAsError

The morph (or one of its submorphs) had an error in its drawing method.


<details>
	<summary>See more</summary>
	
	drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBounds.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft to: r bottomRight-w width: w color: `Color yellow`.
	self line: r topRight -(w@0) to: r bottomLeft -(0@w)width: w color: `Color yellow`
</details>

#### MorphicCanvas>>#drawCurrentAsOutline

<details>
	<summary>See more</summary>
	
	drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		self
			reverseRectangleBorder: currentMorph morphLocalBounds
			borderWidth: 2 ]
</details>

#### MorphicCanvas>>#frameRectangle: r borderWidth: borderWidth color: borderColor

<details>
	<summary>See more</summary>
	
	frameRectangle: r borderWidth: borderWidth color: borderColor
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint
			font: fontOrNil
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		self
			drawString: aString
			from: firstIndex
			to: lastIndex
			at: aPoint  + `0@2`
			font: fontOrNil
			color: bottomColor ].
	^self
		drawString: aString
		from: firstIndex
		to: lastIndex
		at: aPoint  + `0@1`
		font: fontOrNil
		color: aColor
</details>

#### MorphicCanvas>>#windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor

<details>
	<summary>See more</summary>
	
	windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#contentsOfArea: aRectangle into: aForm

<details>
	<summary>See more</summary>
	
	contentsOfArea: aRectangle into: aForm
	| bb |
	bb _ BitBlt toForm: aForm.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^aForm
</details>

#### MorphicCanvas>>#frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol

<details>
	<summary>See more</summary>
	
	frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#form

<details>
	<summary>See more</summary>
	
	form

	^ form
</details>

#### MorphicCanvas>>#depth

<details>
	<summary>See more</summary>
	
	depth

	^ form depth
</details>

#### MorphicCanvas>>#reverseRectangleBorder: r borderWidth: borderWidth

<details>
	<summary>See more</summary>
	
	reverseRectangleBorder: r borderWidth: borderWidth
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#drawString: s at: pt font: fontOrNil color: aColor

Answer last affected pixel position Answer nil if nothing was done


<details>
	<summary>See more</summary>
	
	drawString: s at: pt font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	^self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor
</details>

#### MorphicCanvas>>#currentTransformation

Warning. Only valid inside a #drawOn: method


<details>
	<summary>See more</summary>
	
	currentTransformation
	"Warning. Only valid inside a #drawOn: method"
	^currentTransformation
</details>

#### MorphicCanvas>>#stencil: stencilForm at: aPoint color: aColor

<details>
	<summary>See more</summary>
	
	stencil: stencilForm at: aPoint color: aColor
	self subclassResponsibility.
</details>

#### MorphicCanvas>>#fillRectangle: aRectangle color: aColor

<details>
	<summary>See more</summary>
	
	fillRectangle: aRectangle color: aColor
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#externalizeDisplayBounds: r from: aMorph

r is a Rectangle, expressed in aMorph's coordinate system. Answer another rectangle, that bounds r when translated to World coordinates


<details>
	<summary>See more</summary>
	
	externalizeDisplayBounds: r from: aMorph
	"r is a Rectangle, expressed in aMorph's coordinate system.
	Answer another rectangle, that bounds r when translated to World coordinates"

	"Should translate only to whatever world or PasteUp we are displaying.
	Fix when implementing multiple Canvases (Displays) showing different
	(potentially nested Worlds)"
	| inOwners owner |
	self flag: #jmvVer2.

	inOwners _ aMorph location displayBoundsOfTransformOf: r.
	owner _ aMorph owner.
	^owner
		ifNotNil: [ self externalizeDisplayBounds: inOwners from: owner]
		ifNil: [ inOwners ]
</details>

#### MorphicCanvas>>#newClipRect: aRectangleOrNil

aRectangle is in world coordinates. But ivar clipRect is relative to the form, For example, if we had previously been built like aCanvas on: someForm over: (100@100 extent 200@100) then our origin would be -100 @ -100. Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting only (20@20 extent: 40@30) in our form


<details>
	<summary>See more</summary>
	
	newClipRect: aRectangleOrNil
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ `0@0` corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ])
</details>

#### MorphicCanvas>>#line: pt1 to: pt2 width: wp color: c

<details>
	<summary>See more</summary>
	
	line: pt1 to: pt2 width: wp color: c
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc

<details>
	<summary>See more</summary>
	
	textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (`0@0` max: self clipRect origin - boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: self clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun ]
</details>

#### MorphicCanvas>>#drawsOnDisplay

<details>
	<summary>See more</summary>
	
	drawsOnDisplay
	^form == Display
</details>

#### MorphicCanvas>>#image: aForm at: aPoint

<details>
	<summary>See more</summary>
	
	image: aForm at: aPoint
	self subclassResponsibility 
</details>

#### MorphicCanvas>>#ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

<details>
	<summary>See more</summary>
	
	ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor
	self subclassResponsibility 
</details>

## ScrollBar

A ScrollBar for general use in Morphic.

### Methods
#### ScrollBar>>#sliderClass

<details>
	<summary>See more</summary>
	
	sliderClass
	^DraggeableButtonMorph
</details>

#### ScrollBar>>#freeSliderRoom

Answer the length or height of the free slider area, i.e. subtract the slider itself. If we are really too short of room, lie a little bit. Answering at least 4, even when the free space might be actually negative, makes the scrollbar somewhat usable.


<details>
	<summary>See more</summary>
	
	freeSliderRoom
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	| buttonsRoom |
	buttonsRoom _ Theme current minimalWindows ifTrue: [0] ifFalse: [self buttonExtent * 2].
	^ ((self isHorizontal
		ifTrue: [ extent x - slider morphWidth]
		ifFalse: [ extent y - slider morphHeight])
			- (borderWidth * 2) - buttonsRoom) max: 4
</details>

#### ScrollBar>>#privateExtent: aPoint

Answer whether extent was actually changed. If some subclass may reject the update, answer false in those cases.


<details>
	<summary>See more</summary>
	
	privateExtent: aPoint
	| isH wasH |
	wasH _ self isHorizontal.
	^ (super privateExtent: aPoint)
		ifTrue: [
			"Doesn't move!"
			"upButton morphPosition: borderWidth@borderWidth."
			downButton morphPosition: extent - borderWidth - downButton morphExtent.
			isH _ self isHorizontal.
			isH = wasH ifFalse: [
				isH
					ifTrue: [
						upButton updateLeftButtonImage.
						downButton updateRightButtonImage ]
					ifFalse: [
						upButton updateUpButtonImage.
						downButton updateDownButtonImage ]].
			]; yourself
</details>

#### ScrollBar>>#scrollTo: handPositionRelativeToSlider

<details>
	<summary>See more</summary>
	
	scrollTo: handPositionRelativeToSlider
	| v handPositionRelativeToUs |
	grabPosition ifNotNil: [
		handPositionRelativeToUs _ slider externalize: handPositionRelativeToSlider.
		v _ (self isHorizontal
			ifTrue: [ handPositionRelativeToUs x - grabPosition x ]
			ifFalse: [ handPositionRelativeToUs y - grabPosition y ])
				- borderWidth - self buttonExtent * 1.0
					/ self freeSliderRoom.
		self internalScrollValue: v ]
</details>

#### ScrollBar>>#computeSlider

<details>
	<summary>See more</summary>
	
	computeSlider

	| delta |
	delta _ (Theme current minimalWindows ifTrue: [0] ifFalse: [self buttonExtent]) + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPosition: borderWidth @ (borderWidth + delta) ] 
</details>

#### ScrollBar>>#initializeSlider

initialize the receiver's slider


<details>
	<summary>See more</summary>
	
	initializeSlider
	"initialize the receiver's slider"

	sliderShadow _ RectangleLikeMorph new.
	self addMorph: sliderShadow.
	sliderShadow hide.
		
	slider _ self sliderClass new.
	slider model: self.
	slider grabSelector: #sliderGrabbedAt:.
	slider dragSelector: #scrollTo:.
	slider action: #sliderReleased.
	self addMorph: slider.

	self computeSlider
</details>

#### ScrollBar>>#mouseStillDown

Called from the stepping mechanism for morphs wanting continuously repeated 'yes the mouse is still down, yes it is still down, yes it has not changed yet, no the mouse is still not up, yes the button is down' etc messages


<details>
	<summary>See more</summary>
	
	mouseStillDown
	self scrollByPage
</details>

#### ScrollBar>>#scrollDown: count

<details>
	<summary>See more</summary>
	
	scrollDown: count
	self internalScrollValue: (value + (scrollDelta * count) + 0.000001 min: 1.0)
</details>

#### ScrollBar>>#scrollDelta: d1 pageDelta: d2

Supply optional increments for better scrolling of, eg, text


<details>
	<summary>See more</summary>
	
	scrollDelta: d1 pageDelta: d2
	"Supply optional increments for better scrolling of, eg, text"
	scrollDelta _ d1.
	pageDelta _ d2.
</details>

#### ScrollBar>>#handlesMouseStillDown: evt

Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp


<details>
	<summary>See more</summary>
	
	handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	^true
</details>

#### ScrollBar>>#scrollValue: newValue

Drive the slider position externally...


<details>
	<summary>See more</summary>
	
	scrollValue: newValue
	"Drive the slider position externally..."
	value _ newValue min: 1.0 max: 0.0.
	self computeSlider
</details>

#### ScrollBar>>#buttonClass

<details>
	<summary>See more</summary>
	
	buttonClass
	^PluggableButtonMorph
</details>

#### ScrollBar>>#scrollUp

<details>
	<summary>See more</summary>
	
	scrollUp
	self scrollUp: 1
</details>

#### ScrollBar>>#recreateSubmorphs

<details>
	<summary>See more</summary>
	
	recreateSubmorphs
	self removeAllMorphs.
	self
		initializeUpButton;
		initializeDownButton;
		initializeSlider.
	"Set color for submorphs"
	self color: color.
</details>

#### ScrollBar>>#totalSliderRoom

Answer the length or height of the slider area


<details>
	<summary>See more</summary>
	
	totalSliderRoom
	"Answer the length or height of the slider area"

	^ (self isHorizontal
		ifTrue: [ extent x ]
		ifFalse: [ extent y ])
			- (borderWidth * 2) - (self buttonExtent * 2).
</details>

#### ScrollBar>>#mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

Update visual feedback


<details>
	<summary>See more</summary>
	
	mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Update visual feedback"

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	self setNextDirectionFromEvent: aMouseButtonEvent.
	self scrollByPage
</details>

#### ScrollBar>>#expandSlider

Compute the new size of the slider (use the old sliderThickness as a minimum).


<details>
	<summary>See more</summary>
	
	expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."

	| e |
	e _ (self totalSliderRoom * interval) asInteger max: 7.
	slider morphExtent: (self isHorizontal
		ifTrue: [ e @ self buttonExtent ]
		ifFalse: [ self buttonExtent @ e ])
</details>

#### ScrollBar>>#internalScrollValue: newValue

Called internally for propagation to model


<details>
	<summary>See more</summary>
	
	internalScrollValue: newValue
	"Called internally for propagation to model"
	self scrollValue: newValue.
	setValueSelector ifNotNil: [
		model perform: setValueSelector with: value ]
</details>

#### ScrollBar>>#interval: d

Supply an optional floating fraction so slider can expand to indicate range


<details>
	<summary>See more</summary>
	
	interval: d
	"Supply an optional floating fraction so slider can expand to indicate range"
	interval _ d min: 1.0.
	self expandSlider.
	self computeSlider.
</details>

#### ScrollBar>>#initializeUpButton

initialize the receiver's upButton


<details>
	<summary>See more</summary>
	
	initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ upButton updateLeftButtonImage ]
		ifFalse: [ upButton updateUpButtonImage ].

</details>

#### ScrollBar>>#isHorizontal

<details>
	<summary>See more</summary>
	
	isHorizontal

	^extent x > extent y
</details>

#### ScrollBar>>#initialize

initialize the state of the receiver


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	extent _ self class scrollbarThickness @ 100.
	value _ 0.0.
	self recreateSubmorphs.
	scrollDelta _ 0.02.
	pageDelta _ 0.2
</details>

#### ScrollBar>>#setNextDirectionFromEvent: event

<details>
	<summary>See more</summary>
	
	setNextDirectionFromEvent: event

	nextPageDirection _ self isHorizontal
		ifTrue: [ event eventPosition x >= slider referencePosition x ]
		ifFalse: [ event eventPosition y >= slider referencePosition y ]
</details>

#### ScrollBar>>#scrollDown

<details>
	<summary>See more</summary>
	
	scrollDown
	self scrollDown: 1
</details>

#### ScrollBar>>#minimumExtent

This returns the minimum extent that the morph may be shrunk to. It is expressed in the morph own coordinates, like morphExtent.


<details>
	<summary>See more</summary>
	
	minimumExtent
	| t |
	t _ self class scrollbarThickness.
	^t@t
</details>

#### ScrollBar>>#sliderReleased

<details>
	<summary>See more</summary>
	
	sliderReleased

	grabPosition _ nil.
	sliderShadow hide
</details>

#### ScrollBar>>#handlesMouseDown: aMouseButtonEvent

Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?


<details>
	<summary>See more</summary>
	
	handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	^true
</details>

#### ScrollBar>>#sliderGrabbedAt: handPositionRelativeToSlider

<details>
	<summary>See more</summary>
	
	sliderGrabbedAt: handPositionRelativeToSlider

	grabPosition _ handPositionRelativeToSlider.
	sliderShadow
		morphPosition: slider morphPosition extent: slider morphExtent;
		show
</details>

#### ScrollBar>>#adoptWidgetsColor: aColor

Adopt the given pane color


<details>
	<summary>See more</summary>
	
	adoptWidgetsColor: aColor
	"Adopt the given pane color"
	self color: aColor.
</details>

#### ScrollBar>>#scrollByPage

Scroll automatically while mouse is down


<details>
	<summary>See more</summary>
	
	scrollByPage
	"Scroll automatically while mouse is down"
	nextPageDirection
		ifTrue: [self internalScrollValue: (value + pageDelta min: 1.0)]
		ifFalse: [self internalScrollValue: (value - pageDelta max: 0.0)]

</details>

#### ScrollBar>>#scrollUp: count

<details>
	<summary>See more</summary>
	
	scrollUp: count
	self internalScrollValue: (value - (scrollDelta * count) - 0.000001 max: 0.0)
</details>

#### ScrollBar>>#fontPreferenceChanged

Rescale


<details>
	<summary>See more</summary>
	
	fontPreferenceChanged
	"Rescale"
	
	self recreateSubmorphs
</details>

#### ScrollBar>>#color: aColor

Change the color of the scrollbar to go with aColor.


<details>
	<summary>See more</summary>
	
	color: aColor
	"Change the color of the scrollbar to go with aColor."
	| buttonColor |
	super color: aColor.
	buttonColor _ color alphaMixed: 0.7 with: Theme current scrollbarButtonColor.
	upButton color: buttonColor.
	downButton color: buttonColor.
	slider color: buttonColor slightlyLighter.
	sliderShadow color: (color alphaMixed: 0.45 with: Theme current scrollbarSliderShadowColor)
</details>

#### ScrollBar>>#scrollValue

<details>
	<summary>See more</summary>
	
	scrollValue
	^ value
</details>

#### ScrollBar>>#model: thang setValueSelector: aSymbol

<details>
	<summary>See more</summary>
	
	model: thang setValueSelector: aSymbol
	model _ thang.
	setValueSelector _ aSymbol
</details>

#### ScrollBar>>#drawOn: aCanvas

A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.


<details>
	<summary>See more</summary>
	
	drawOn: aCanvas

	aCanvas
		fillRectangle: self morphLocalBounds
		color: (color alphaMixed: 0.3 with: Theme current scrollbarColor)
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor
</details>

#### ScrollBar>>#buttonExtent

<details>
	<summary>See more</summary>
	
	buttonExtent

	^self class scrollbarThickness
</details>

#### ScrollBar>>#initializeDownButton

initialize the receiver's downButton


<details>
	<summary>See more</summary>
	
	initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	downButton morphExtent: e@e.
	Theme current minimalWindows ifTrue: [^ self].
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ downButton updateRightButtonImage ]
		ifFalse: [ downButton updateDownButtonImage ]
</details>

