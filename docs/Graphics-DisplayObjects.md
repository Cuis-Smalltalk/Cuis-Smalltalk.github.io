## ColorForm

ColorForm is a normal Form plus a color map of up to 2^depth Colors. Typically, one reserves one entry in the color map for transparent. This allows 1, 3, 15, or 255 non-transparent colors in ColorForms of depths 1, 2, 4, and 8 bits per pixel. ColorForms don't support depths greater than 8 bits because that would require excessively large color maps with little real benefit, since 16-bit and 32-bit depths already support thousands and millions of colors. ColorForms have several uses: 1) Precise colors. You can have up to 256 true colors, instead being limited to the 8-bit color palette. 2) Easy transparency. Just store (Color transparent) at the desired position in the color map. 3) Cheap color remapping by changing the color map. A color map is an Array of up to 2^depth Color objects. A Bitmap colorMap is automatically computed and cached for rapid display. Note that if you change the color map, you must resubmit it via the colors: method to flush this cache. ColorForms can be a bit tricky. Note that: a) When you BitBlt from one ColorForm to another, you must remember to copy the color map of the source ColorForm to the destination ColorForm. b) A ColorForm's color map is an array of depth-independent Color objects. BitBlt requires a BitMap of actual pixel values, adjusted to the destination depth. These are different things! ColorForms automatically maintain a cache of the BitBlt-style color map corresponding to the colors array for the last depth on which the ColorForm was displayed, so there should be little need for clients to work with BitBlt-style color maps. c) The default map for 8 bit depth has black in the first entry, not transparent. Say (cform colors at: 1 put: Color transparent).

### Methods
#### ColorForm>>#blankCopyOf: aRectangle scaledBy: scale

<details>
	<summary>See more</summary>
	
	blankCopyOf: aRectangle scaledBy: scale

        | newForm |
        newForm _ super blankCopyOf: aRectangle scaledBy: scale.
        colors ifNotNil: [newForm colors: colors copy].
        ^ newForm
</details>

#### ColorForm>>#asFormOfDepth: d

<details>
	<summary>See more</summary>
	
	asFormOfDepth: d
	| answer |
	d = depth ifTrue: [ ^self ].
	"Same depth, but opposite endianness."
	d abs = depth abs ifTrue: [
		answer _ self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d
</details>

#### ColorForm>>#setExtent: extent depth: bitsPerPixel

Create a virtual bit map with the given extent and bitsPerPixel.


<details>
	<summary>See more</summary>
	
	setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs > 8 ifTrue: [self error: 'ColorForms only support depths up to 8 bits'].
	super setExtent: extent depth: bitsPerPixel.

</details>

#### ColorForm>>#flippedBy: direction

Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation) Form lena display. (Form lena flippedBy: #vertical) display. (Form lena flippedBy: #horizontal) display. (Form lena flippedBy: #both) display.


<details>
	<summary>See more</summary>
	
	flippedBy: direction
	| oldColors newForm |
	oldColors _ colors.
	self colors: nil.
	newForm _ super flippedBy: direction.
	self colors: oldColors.
	newForm colors: oldColors.
	^newForm
</details>

#### ColorForm>>#colors

Return my color palette.


<details>
	<summary>See more</summary>
	
	colors
	"Return my color palette."

	self ensureColorArrayExists.
	^ colors

</details>

#### ColorForm>>#unusedColormapEntry

Return the index of an unused color map entry, or zero if there isn't one.


<details>
	<summary>See more</summary>
	
	unusedColormapEntry
	"Return the index of an unused color map entry, or zero if there isn't one."

	| tallies |
	tallies _ self tallyPixelValues.
	1 to: tallies size do: [:i |
		(tallies at: i) = 0 ifTrue: [^ i]].
	^ 0

</details>

#### ColorForm>>#indexOfColor: aColor

Return the index of aColor in my color array


<details>
	<summary>See more</summary>
	
	indexOfColor: aColor
	"Return the index of aColor in my color array"

	self ensureColorArrayExists.
	^ colors indexOf: aColor ifAbsent: [0]
</details>

#### ColorForm>>#colormapIfNeededForGray8bpp

Return a colormap for displaying the receiver at the given depth.


<details>
	<summary>See more</summary>
	
	colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth."

	| newMap |

	"cachedDepth used to be anumber but..."
	(#gray8 = cachedDepth and: [cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].

	self ensureColorArrayExists.
	newMap _ Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) luminance * 255) rounded].

	cachedDepth _ #gray8.
	^ cachedColormap _ newMap
</details>

#### ColorForm>>#isTransparentAt: aPoint

Return true if the receiver is transparent at the given point.


<details>
	<summary>See more</summary>
	
	isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	^ (self colorAt: aPoint) isTransparent

</details>

#### ColorForm>>#ensureColorArrayExists

Return my color palette.


<details>
	<summary>See more</summary>
	
	ensureColorArrayExists
	"Return my color palette."

	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		self colors: (Color indexedColors copyFrom: 1 to: (1 bitShift: self depth))].

</details>

#### ColorForm>>#mightBeTranslucent

Answer whether this form may be translucent


<details>
	<summary>See more</summary>
	
	mightBeTranslucent
	"Answer whether this form may be translucent"
	^true
</details>

#### ColorForm>>#asGrayScale

Return a grayscale ColorForm computed by mapping each color into its grayscale equivalent


<details>
	<summary>See more</summary>
	
	asGrayScale
	"Return a grayscale ColorForm computed by mapping each color into its grayscale equivalent"
	^ self copy colors:
		(colors collect:
			[:c | c isTransparent ifTrue: [c]
						ifFalse: [Color gray: c luminance]])
</details>

#### ColorForm>>#setColors: colorArray cachedColormap: aBitmap depth: anInteger

Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations.


<details>
	<summary>See more</summary>
	
	setColors: colorArray cachedColormap: aBitmap depth: anInteger
	"Semi-private. Set the color array, cached colormap, and cached colormap depth to avoid having to recompute the colormap when switching color palettes in animations."

	colors _ colorArray.
	cachedDepth _ anInteger.
	cachedColormap _ aBitmap.

</details>

#### ColorForm>>#copy: aRect

Return a new ColorForm containing the portion of the receiver delineated by aRect.


<details>
	<summary>See more</summary>
	
	copy: aRect
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm
</details>

#### ColorForm>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		newLine; tab;
		nextPutAll: 'colorsFromArray: #('.
	self colors do: [:color |
		color storeArrayOn: aStream].
	aStream nextPutAll: ' ))'.
</details>

#### ColorForm>>#colors: colorList

Set my color palette to the given collection.


<details>
	<summary>See more</summary>
	
	colors: colorList
	"Set my color palette to the given collection."

	| colorArray colorCount newColors |
	colorList ifNil: [
		colors _ cachedDepth _ cachedColormap _ nil.
		^ self].

	colorArray _ colorList asArray.
	colorCount _ colorArray size.
	newColors _ Array new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (colorArray at: i)]
			ifFalse: [newColors at: i put: `Color transparent` ]].

	colors _ newColors.
	cachedDepth _ nil.
	cachedColormap _ nil.

</details>

#### ColorForm>>#colormapIfNeededForDepth: destDepth

Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.


<details>
	<summary>See more</summary>
	
	colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	| newMap |
	colors ifNil: [
		"use the standard colormap"
		^ Color colorMapIfNeededFrom: self depth to: destDepth].

	(destDepth = cachedDepth and:[cachedColormap isColormap not]) 
		ifTrue: [^ cachedColormap].
	newMap _ Bitmap new: colors size.
	1 to: colors size do: [:i |
		newMap
			at: i
			put: ((colors at: i) pixelValueForDepth: destDepth)].

	cachedDepth _ destDepth.
	^ cachedColormap _ newMap.

</details>

#### ColorForm>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #ColorForm or: [ super is: aSymbol ]
</details>

#### ColorForm>>#clearColormapCache

<details>
	<summary>See more</summary>
	
	clearColormapCache

	cachedDepth _ nil.
	cachedColormap _ nil.

</details>

#### ColorForm>>#readAttributesFrom: aBinaryStream

<details>
	<summary>See more</summary>
	
	readAttributesFrom: aBinaryStream
	super readAttributesFrom: aBinaryStream.
	colors _ ColorArray new: (2 raisedTo: depth).
	1 to: colors size do: [:idx | 
		colors basicAt: idx put: (aBinaryStream nextUnsignedInt32BigEndian: false) ]
</details>

#### ColorForm>>#mapColor: oldColor to: newColor

Replace all occurances of the given color with the given new color in my color map.


<details>
	<summary>See more</summary>
	
	mapColor: oldColor to: newColor
	"Replace all occurances of the given color with the given new color in my color map."

	self ensureColorArrayExists.
	1 to: colors size do: [:i | 
		(colors at: i) = oldColor ifTrue: [colors at: i put: newColor]].
	self clearColormapCache.

</details>

#### ColorForm>>#postCopy

Just to make visible the fact that we are sharing the colors... Is it ok to do this?


<details>
	<summary>See more</summary>
	
	postCopy
	"Just to make visible the fact that we are sharing the colors... Is it ok to do this?"
	super postCopy.
	self colors: colors
</details>

#### ColorForm>>#colorAt: aPoint put: aColor

Store the given color into the pixel at aPoint. The given color must match one of the colors in the receiver's colormap.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint put: aColor
	"Store the given color into the pixel at aPoint. The given color must match one of the colors in the receiver's colormap."

	| i |
	i _ self colors indexOf: aColor
		ifAbsent: [^ self error: 'trying to use a color that is not in my colormap'].
	self pixelValueAt: aPoint put: i - 1.

</details>

#### ColorForm>>#replaceColorAtIndex: index with: newColor

Replace a color map entry with newColor.


<details>
	<summary>See more</summary>
	
	replaceColorAtIndex: index with: newColor
	"Replace a color map entry with newColor."

	self ensureColorArrayExists.
	colors at: index put: newColor.
	cachedDepth _ nil.
	cachedColormap _ nil.
</details>

#### ColorForm>>#colorAt: aPoint

Return the color of the pixel at aPoint.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint
	"Return the color of the pixel at aPoint."

	^ self colors at: (self pixelValueAt: aPoint) + 1

</details>

#### ColorForm>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm

	^ (self asFormOfDepth: 32) offset: offset; as: Form
</details>

#### ColorForm>>#asGrayForm

Build an optimal GrayForm, for any color palette in the receiver.


<details>
	<summary>See more</summary>
	
	asGrayForm
	"Build an optimal GrayForm,
	for any color palette in the receiver."
	| answer map |
	answer _ GrayForm extent: width@height.
	map _ self colormapIfNeededForGray8bpp.
	(BitBlt toForm: answer)
		colorMap: map;
		copy: self boundingBox
		from: `0@0` in: self
		fillColor: nil rule: Form over.
	^ answer
</details>

#### ColorForm>>#writeAttributesOn: file

<details>
	<summary>See more</summary>
	
	writeAttributesOn: file
	| colorArray |
	super writeAttributesOn: file.
	colorArray _ self colors asColorArray.
	1 to: (2 raisedTo: depth) do: [:idx |
		file nextUnsignedInt32Put: (colorArray basicAt: idx) bigEndian: false ]
</details>

#### ColorForm>>#maskingMap

Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero.


<details>
	<summary>See more</summary>
	
	maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	| maskingMap |
	maskingMap _ Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF.
	1 to: colors size do:[:i|
		(colors at: i) isTransparent ifTrue:[maskingMap at: i put: 0].
	].
	colors size+1 to: maskingMap size do:[:i| maskingMap at: i put: 0].
	^maskingMap
</details>

## Cursor

I am a Form that is a possible appearance for a mouse cursor. My size is always 16x16, ever since the original implementation on the Alto. Predefined cursors should accessed via #cursorAt: which caches Cursor instances. For example "Cursor cursorAt: #normalCursorWithMask". You can also dynamically add your own cursors or modify existing ones via #cursorAt:put: as desired.

### Methods
#### Cursor>>#showWhile: aBlock

While evaluating the argument, aBlock, make the receiver be the cursor shape.


<details>
	<summary>See more</summary>
	
	showWhile: aBlock 
	"While evaluating the argument, aBlock, make the receiver be the cursor 
	shape."

	| oldcursor |
	oldcursor _ Cursor currentCursor.
	self activateCursor.
	^aBlock ensure: [oldcursor activateCursor]

</details>

#### Cursor>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm
	| form |
	form _ Form extent: self extent depth: 8.
	form fillShape: self fillColor: `Color black` at: offset negated.
	^ form offset: offset
</details>

#### Cursor>>#withMask

<details>
	<summary>See more</summary>
	
	withMask
	^CursorWithMask derivedFrom: self
</details>

#### Cursor>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self storeOn: aStream base: 2
</details>

#### Cursor>>#hasMask

<details>
	<summary>See more</summary>
	
	hasMask
	^false
</details>

#### Cursor>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Cursor or: [ super is: aSymbol ]
</details>

#### Cursor>>#installCursorWithMask: maskForm

Primitive. Tell the interpreter to use the receiver as the current cursor image with the given mask Form. Both the receiver and the mask should have extent 16@16 and a depth of one. The mask and cursor bits are combined as follow: mask cursor effect 0 0 transparent (underlying pixel shows through) 1 1 opaque black 1 0 opaque white 0 1 invert the underlying pixel


<details>
	<summary>See more</summary>
	
	installCursorWithMask: maskForm
	"Primitive. Tell the interpreter to use the receiver as the current cursor image with the given mask Form. Both the receiver and the mask should have extent 16@16 and a depth of one. The mask and cursor bits are combined as follow:
			mask	cursor	effect
			 0		  0		transparent (underlying pixel shows through)
			 1		  1		opaque black
			 1		  0		opaque white
			 0		  1		invert the underlying pixel"
"Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed

</details>

#### Cursor>>#enlargedBy: scale

Big cursors are 32 bits deep (ARGB premultiplied)


<details>
	<summary>See more</summary>
	
	enlargedBy: scale
	"Big cursors are 32 bits deep (ARGB premultiplied)"
	| big |
	scale = 1 ifTrue: [^self].
	big := CursorWithAlpha extent: self extent * scale depth: 32.
	(self asCursorForm magnifyBy: scale) displayOn: big.
	big offset: (self offset - 0.5 * scale min: `0@0` max: big extent negated) asIntegerPoint.
	big fallback: self.
	^ big
</details>

#### Cursor>>#asBigCursor

Big cursors are 32 bits deep (ARGB premultiplied)


<details>
	<summary>See more</summary>
	
	asBigCursor
	"Big cursors are 32 bits deep (ARGB premultiplied)"
	(width = 16 and: [ height = 16 ])
		ifTrue:[ ^self enlargedBy: 2 ].
	^self
</details>

#### Cursor>>#activateCursor

Make the hardware's mouse cursor look like the receiver


<details>
	<summary>See more</summary>
	
	activateCursor
	"Make the hardware's mouse cursor look like the receiver"

	Cursor currentCursor: self
</details>

#### Cursor>>#installCursor

Primitive. Tell the interpreter to use the receiver as the current cursor image. Fail if the receiver does not match the size expected by the hardware. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	installCursor
	"Primitive. Tell the interpreter to use the receiver as the current cursor 
	image. Fail if the receiver does not match the size expected by the 
	hardware. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed
</details>

## CursorWithAlpha

A 32-bit ARGB Cursor of arbitrary extent (some platforms may limit the size). Compositing assumes alpha is pre-multiplied.

### Methods
#### CursorWithAlpha>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm

	^ self as: Form
</details>

#### CursorWithAlpha>>#fallback: aCursor

<details>
	<summary>See more</summary>
	
	fallback: aCursor
	fallback := aCursor
</details>

#### CursorWithAlpha>>#fallback

<details>
	<summary>See more</summary>
	
	fallback
	^fallback ifNil: [self class cursorAt: #normalCursorWithMask]
</details>

#### CursorWithAlpha>>#installCursor

Primitive. Tell the interpreter to use the receiver as the current cursor image. Fail if the receiver does not match the size expected by the hardware. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	installCursor
	<primitive: 101>
	self fallback installCursor
</details>

## CursorWithMask

A Cursor which additionally has a 16x16 transparency bitmap called a "mask". See the comment of beCursorWithMask: for details on how the mask is treated.

### Methods
#### CursorWithMask>>#storeOn: aStream base: anInteger

Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original.


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: anInteger

	aStream nextPut: $(.
	super storeOn: aStream base: anInteger.
	aStream nextPutAll: ' setMaskForm: '.
	maskForm storeOn: aStream base: anInteger.
	aStream nextPut: $)
</details>

#### CursorWithMask>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm
	| form |
	form _ Form extent: self extent depth: 8.
	form fillShape: maskForm fillColor: `Color white`.
	form fillShape: self fillColor: `Color black` at: offset negated.
	^ form offset: offset
</details>

#### CursorWithMask>>#withMask

<details>
	<summary>See more</summary>
	
	withMask
	^self
</details>

#### CursorWithMask>>#hasMask

<details>
	<summary>See more</summary>
	
	hasMask
	^true
</details>

#### CursorWithMask>>#maskForm

<details>
	<summary>See more</summary>
	
	maskForm
	^ maskForm
</details>

#### CursorWithMask>>#setMaskForm: aForm

<details>
	<summary>See more</summary>
	
	setMaskForm: aForm
	maskForm _ aForm
</details>

#### CursorWithMask>>#installCursor

Primitive. Tell the interpreter to use the receiver as the current cursor image. Fail if the receiver does not match the size expected by the hardware. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	installCursor

	^ self installCursorWithMask: maskForm
</details>

## DisplayScreen

There is only one instance of me, Display. It is a global and is used to handle general user requests to deal with the whole display screen. Although I offer no protocol, my name provides a way to distinguish this special instance from all other Forms. This is useful, for example, in dealing with saving and restoring the system. To change the depth of your Display... Display newDepth: 16. Display newDepth: 8. Display newDepth: 1. Valid display depths are 1, 2, 4, 8, 16 and 32. It is suggested that you run with your monitors setting the same, for better speed and color fidelity. Note that this can add up to 4Mb for the Display form. Finally, note that newDepth: ends by executing a 'ControlManager restore' which currently terminates the active process, so nothing that follows in the doit will get executed. Depths 1, 2, 4 and 8 bits go through a color map to put color on the screen, but 16 and 32-bit color use the pixel values directly for RGB color (5 and 8 bits per, respectivlely). The color choice an be observed by executing Color fromUser in whatever depth you are using.

### Methods
#### DisplayScreen>>#forceDamageToScreen: allDamage

Force all the damage rects to the screen.


<details>
	<summary>See more</summary>
	
	forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."

	"allDamage do: [ :r | 
		self forceToScreen: r ]."
	"Do it at once. Otherwise, some flicking with 'broken' morphs was visible."
	self forceToScreen: (Rectangle merging: allDamage)
</details>

#### DisplayScreen>>#setExtent: aPoint depth: bitsPerPixel

DisplayScreen startUp


<details>
	<summary>See more</summary>
	
	setExtent: aPoint depth: bitsPerPixel  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth = bitsPerPixel and: [aPoint = self extent and: 
					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits _ nil.  "Free up old bitmap in case space is low"
		(self supportsDisplayDepth: bitsPerPixel)
			ifTrue:[super setExtent: aPoint depth: bitsPerPixel]
			ifFalse:[(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[super setExtent: aPoint depth: bitsPerPixel negated]
				ifFalse:["Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth]].
	].
"	Transcript newLine; print: 'DisplayScreen trigger: #screenSizeChanged'. "

	"Let the world know"
	self triggerEvent: #screenSizeChanged.
</details>

#### DisplayScreen>>#copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule

Make up a BitBlt table and copy the bits.


<details>
	<summary>See more</summary>
	
	copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule
	super copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule.
	DisplayScreen screenUpdateRequired: clipRect
</details>

#### DisplayScreen>>#fill: aRectangle rule: anInteger fillColor: aForm

Replace a rectangular area of the receiver with the pattern described by aForm according to the rule anInteger.


<details>
	<summary>See more</summary>
	
	fill: aRectangle rule: anInteger fillColor: aForm 
	super fill: aRectangle rule: anInteger fillColor: aForm.
	DisplayScreen screenUpdateRequired: aRectangle
</details>

#### DisplayScreen>>#copyBits: sourceForm at: destOrigin translucent: factor

Make up a BitBlt table and copy the bits with the given colorMap.


<details>
	<summary>See more</summary>
	
	copyBits: sourceForm at: destOrigin translucent: factor
	super copyBits: sourceForm at: destOrigin translucent: factor.
	DisplayScreen screenUpdateRequired: (destOrigin extent: sourceForm extent)
</details>

#### DisplayScreen>>#fullScreenMode: aBoolean

On platforms that support it, set full-screen mode to the value of the argument. (Note: you'll need to restore the Display after calling this primitive.


<details>
	<summary>See more</summary>
	
	fullScreenMode: aBoolean
	"On platforms that support it, set full-screen mode to the value of the argument. (Note: you'll need to restore the Display after calling this primitive."
	"Display fullScreenMode: true. Display newDepth: Display depth"

	<primitive: 233>
	self primitiveFailed

</details>

#### DisplayScreen>>#flash: aRectangle with: aColor

<details>
	<summary>See more</summary>
	
	flash: aRectangle with: aColor

	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle
</details>

#### DisplayScreen>>#primShowRectLeft: l right: r top: t bottom: b

Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates.


<details>
	<summary>See more</summary>
	
	primShowRectLeft: l right: r top: t bottom: b
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates."

	<primitive: 127>
	"if this fails, coerce coordinates to integers and try again"
	self primRetryShowRectLeft: l truncated
		right: r rounded
		top: t truncated
		bottom: b rounded.

</details>

#### DisplayScreen>>#beDisplay

Primitive. Tell the interpreter to use the receiver as the current display image. Fail if the form is too wide to fit on the physical display. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	beDisplay
	"Primitive. Tell the interpreter to use the receiver as the current display 
	image. Fail if the form is too wide to fit on the physical display. 
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 102>
	self primitiveFailed
</details>

#### DisplayScreen>>#copy: destRectangle from: sourcePt in: sourceForm rule: rule

Make up a BitBlt table and copy the bits.


<details>
	<summary>See more</summary>
	
	copy: destRectangle from: sourcePt in: sourceForm rule: rule
	super copy: destRectangle from: sourcePt in: sourceForm rule: rule.
	DisplayScreen screenUpdateRequired: destRectangle
</details>

#### DisplayScreen>>#findAnyDisplayDepth

Return any display depth that is supported on this system.


<details>
	<summary>See more</summary>
	
	findAnyDisplayDepth
	"Return any display depth that is supported on this system."
	^self findAnyDisplayDepthIfNone: [
		"Ugh .... now this is a biggie - a system that does not support
		any of the Squeak display depths at all."
		Smalltalk
			logError: 'Fatal error: This system has no support for any display depth at all.'
			inContext: thisContext
			to: 'CuisDebug'.
		Smalltalk quitPrimitive. "There is no way to continue from here"
	]
</details>

#### DisplayScreen>>#flash: aRectangle

Flash the area of the screen defined by the given rectangle.


<details>
	<summary>See more</summary>
	
	flash: aRectangle 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceToScreen: aRectangle
</details>

#### DisplayScreen>>#primitiveDeferUpdates: aBoolean

Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails.


<details>
	<summary>See more</summary>
	
	primitiveDeferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails."

	<primitive: 126>
	^ nil  "answer nil if primitive fails"

</details>

#### DisplayScreen>>#deferUpdates: aBoolean

Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. To actually make them visible, call #forceToScreen: . If this underlying platform does not support deferred updates, this primitive will fail. Answer nil if it fails. Most platforms do support this functionality. You can turn it off for playing with Display directly.


<details>
	<summary>See more</summary>
	
	deferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does"
	"true ifTrue: [ ^nil ]."

	"Note: If we disable VM defer updates (with this &false), but answer notNil, the the Morphic workaround is not used,
	and you get a lot of flicking."
	^self primitiveDeferUpdates: aBoolean "& false"
</details>

#### DisplayScreen>>#forceToScreen

Force the entire display area to the screen


<details>
	<summary>See more</summary>
	
	forceToScreen
	"Force the entire display area to the screen"
	^self forceToScreen: self boundingBox
</details>

#### DisplayScreen>>#findAnyDisplayDepthIfNone: aBlock

Return any display depth that is supported on this system. If there is none, evaluate aBlock.


<details>
	<summary>See more</summary>
	
	findAnyDisplayDepthIfNone: aBlock
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock."
	#(32 16 8 4 2 1 -32 -16 -8 -4 -2 -1) do:[:bpp|
		(self supportsDisplayDepth: bpp) ifTrue:[^bpp].
	].
	^aBlock value
</details>

#### DisplayScreen>>#eraseShape: bwForm

use bwForm as a mask to clear all pixels where bwForm has 1's


<details>
	<summary>See more</summary>
	
	eraseShape: bwForm
	super eraseShape: bwForm.
	DisplayScreen screenUpdateRequired: nil
</details>

#### DisplayScreen>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	self
		setExtent: self class actualScreenSize
		depth: (self class actualScreenDepth ifNil: [ 32 ])
</details>

#### DisplayScreen>>#forceToScreen: aRectangle

Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:.


<details>
	<summary>See more</summary>
	
	forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.

</details>

#### DisplayScreen>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a reference to the Display in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a reference to the Display in the other system instead.  "

	"A path to me"
	dp _ DiskProxy global: #Display selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp

</details>

#### DisplayScreen>>#supportsDisplayDepth: pixelDepth

Return true if this pixel depth is supported on the current host platform. Primitive. Optional.


<details>
	<summary>See more</summary>
	
	supportsDisplayDepth: pixelDepth
	"Return true if this pixel depth is supported on the current host platform.
	Primitive. Optional."
	<primitive: 91>
	^#(1 2 4 8 16 32) includes: pixelDepth
</details>

#### DisplayScreen>>#copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map

Make up a BitBlt table and copy the bits. Use a colorMap.


<details>
	<summary>See more</summary>
	
	copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map
	super copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map.
	DisplayScreen screenUpdateRequired: clipRect
</details>

#### DisplayScreen>>#restoreAfter: aBlock

- Evaluate the block - Update host OS Display - Wait for a mouse click - And then restore the Morphic World


<details>
	<summary>See more</summary>
	
	restoreAfter: aBlock
	"
	- Evaluate the block
	- Update host OS Display
	- Wait for a mouse click
	- And then restore the Morphic World"

	aBlock value.
	self forceToScreen.
	Sensor waitButton.
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]
</details>

#### DisplayScreen>>#restore

<details>
	<summary>See more</summary>
	
	restore
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]
</details>

#### DisplayScreen>>#fillFromXYColorBlock: colorBlock

General Gradient Fill. Supply relative x and y in [0.0 ... 1.0] to colorBlock, and paint each pixel with the color that comes back


<details>
	<summary>See more</summary>
	
	fillFromXYColorBlock: colorBlock
	super fillFromXYColorBlock: colorBlock.
	DisplayScreen screenUpdateRequired: nil
</details>

#### DisplayScreen>>#newDepthNoRestore: pixelSize

Change depths. Check if there is enough space! , di


<details>
	<summary>See more</summary>
	
	newDepthNoRestore: pixelSize
	"Change depths.  Check if there is enough space!  , di"
	| area need |
	pixelSize = depth ifTrue: [^ self  "no change"].
	pixelSize abs < self depth ifFalse:
		["Make sure there is enough space"
		area _ Display boundingBox area. "pixels"
		need _ (area * (pixelSize abs - self depth) // 8)  "new bytes needed"
				+ Smalltalk lowSpaceThreshold.
		(Smalltalk garbageCollectMost <= need
			and: [Smalltalk garbageCollect <= need])
			ifTrue: [self error: 'Insufficient free space']].
	self setExtent: self extent depth: pixelSize.
	DisplayScreen startUp
</details>

#### DisplayScreen>>#primRetryShowRectLeft: l right: r top: t bottom: b

Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. Do nothing if it fails.


<details>
	<summary>See more</summary>
	
	primRetryShowRectLeft: l right: r top: t bottom: b
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. Do nothing if it fails. "

	<primitive: 127>
	"do nothing if primitive fails"

</details>

#### DisplayScreen>>#fillShape: aShapeForm fillColor: aColor at: location

Fill a region corresponding to 1 bits in aShapeForm with aColor


<details>
	<summary>See more</summary>
	
	fillShape: aShapeForm fillColor: aColor at: location
	super fillShape: aShapeForm fillColor: aColor at: location.
	DisplayScreen screenUpdateRequired: nil
</details>

#### DisplayScreen>>#forceDisplayUpdate

We stopped using it because it seems that with #deferUpdates: true (as supported by Linux / Windows / Mac OS) what we need is #forceToScreen instead. You might check code containing 'forceDisplayUpdate'


<details>
	<summary>See more</summary>
	
	forceDisplayUpdate
"We stopped using it because it seems that with #deferUpdates: true (as supported by Linux / Windows / Mac OS)
what we need is #forceToScreen instead. You might check code containing 'forceDisplayUpdate'"

	"On platforms that buffer screen updates, force the screen to be updated immediately. On other platforms, or if the primitive is not implemented, do nothing."

	<primitive: 231>
	"do nothing if primitive fails"
</details>

#### DisplayScreen>>#newDepth: pixelSize

Display newDepth: 8. Display newDepth: 1.


<details>
	<summary>See more</summary>
	
	newDepth: pixelSize
"
	Display newDepth: 8.
	Display newDepth: 1.
"
	(self supportsDisplayDepth: pixelSize)
		ifFalse: [ ^self inform:'Display depth ', pixelSize printString, ' is not supported on this system' ].
	self newDepthNoRestore: pixelSize.
	self runningWorld ifNotNil: [ :w | w  buildMagnifiedBackgroundImage ].
	self restore.
</details>

#### DisplayScreen>>#supportedDisplayDepths

Return all pixel depths supported on the current host platform.


<details>
	<summary>See more</summary>
	
	supportedDisplayDepths
	"Return all pixel depths supported on the current host platform."
	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]
</details>

## Form

A rectangular array of pixels, used for holding images. All pictures, including character images are Forms. The depth of a Form is how many bits are used to specify the color at each pixel. The actual bits are held in a Bitmap, whose internal structure is different at each depth. Class Color allows you to deal with colors without knowing how they are actually encoded inside a Bitmap. The supported depths (in bits) are 1, 2, 4, 8, 16, and 32. The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million. Forms are indexed starting at 0 instead of 1; thus, the top-left pixel of a Form has coordinates 0@0. Forms are combined using BitBlt. See the comment in class BitBlt. colorAt: x@y Returns the abstract Color at this location displayAt: x@y shows this form on the screen displayOn: aMedium at: x@y shows this form in a Window, a Form, or other DisplayMedium fillColor: aColor Set all the pixels to the color. edit launch an editor to change the bits of this form. pixelValueAt: x@y The encoded color. The encoding depends on the depth.

### Methods
#### Form>>#blankCopyOf: aRectangle scaledBy: scale

<details>
	<summary>See more</summary>
	
	blankCopyOf: aRectangle scaledBy: scale

        ^ self class extent: (aRectangle extent * scale) truncated depth: depth
</details>

#### Form>>#border: aRectangle width: borderWidth borderHeight: borderHeight fillColor: aColor

Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth@borderHeight. Uses aHalfTone for drawing the border.


<details>
	<summary>See more</summary>
	
	border: aRectangle width: borderWidth borderHeight: borderHeight fillColor: aColor
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth@borderHeight. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderHeight
				bottom: borderHeight)
		rule: Form over
		fillColor: aColor
</details>

#### Form>>#pixelCompare: aRect with: otherForm at: otherLoc

Compare the selected bits of this form (those within aRect) against those in a similar rectangle of otherFrom. Return the sum of the absolute value of the differences of the color values of every pixel. Obviously, this is most useful for rgb (16- or 32-bit) pixels but, in the case of 8-bits or less, this will return the sum of the differing bits of the corresponding pixel values (somewhat less useful)


<details>
	<summary>See more</summary>
	
	pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	"Just use 32 bits..."
	| pixPerWord temp |
	otherForm nativeDepth = 32 ifFalse: [
		^ self pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc ].
	self nativeDepth = 32 ifFalse: [
		^ (self asFormOfDepth: 32) pixelCompare: aRect with: otherForm at: otherLoc ].
	
	pixPerWord _ 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue: [
		"If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp _ self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff |
f _ Form fromUser.
[ Sensor isAnyButtonPressed ] whileFalse: [
	diff _ f
		pixelCompare: f boundingBox
		with: Display
		at: Sensor mousePoint.
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	diff printString , '        ' displayAt: 0@0 ]
"
</details>

#### Form>>#copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule

Make up a BitBlt table and copy the bits.


<details>
	<summary>See more</summary>
	
	copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule
	"Make up a BitBlt table and copy the bits."

	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) copyBits
</details>

#### Form>>#bits

Answer the receiver's Bitmap containing its bits.


<details>
	<summary>See more</summary>
	
	bits
	"Answer the receiver's Bitmap containing its bits."

	^ bits
</details>

#### Form>>#slideImage: otherImage at: topLeft delta: delta

Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40 delta: 3@-4


<details>
	<summary>See more</summary>
	
	slideImage: otherImage at: topLeft delta: delta
	"
	Display slideImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 3@-4
	"
	| bb nSteps clipRect |
	bb _ otherImage boundingBox.
	clipRect _ topLeft extent: otherImage extent.
	nSteps _ 1.
	delta x = 0 ifFalse: [nSteps _ nSteps max: (bb width//delta x abs) + 1].
	delta y = 0 ifFalse: [nSteps _ nSteps max: (bb height//delta y abs) + 1].
	1 to: nSteps do: [ :i | 
			self 
				copyBits: bb from: otherImage
				at: delta*(i-nSteps) + topLeft
				clippingBox: clipRect rule: Form paint.
			Display forceToScreen.
		]
</details>

#### Form>>#addDeltasFrom: previousForm

<details>
	<summary>See more</summary>
	
	addDeltasFrom: previousForm

	(BitBlt 
		destForm: self 
		sourceForm: previousForm 
		fillColor: nil 
		combinationRule: Form reverse
		destOrigin: `0@0`
		sourceOrigin: `0@0`
		extent: self extent 
		clipRect: self boundingBox) copyBits.
	^self
</details>

#### Form>>#displayAutoRangeAt: aPoint zoom: scale

Display receiver, compatibility with Matrix and subclasses such as FloatImage


<details>
	<summary>See more</summary>
	
	displayAutoRangeAt: aPoint zoom: scale
	"Display receiver, compatibility with Matrix and subclasses such as FloatImage"

	| form |
	form _ self.
	scale = 1 ifFalse: [
		form _ form magnifyBy: scale ].
	form displayAt: aPoint.
	^ form
</details>

#### Form>>#zoomInTo: otherImage at: topLeft

Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40


<details>
	<summary>See more</summary>
	
	zoomInTo: otherImage at: topLeft
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	^ self zoomIn: true orOutTo: otherImage at: topLeft
		vanishingPoint: otherImage extent//2+topLeft
</details>

#### Form>>#asFormAutoRange

In optional packages (LinearAlgebra, ImageProcessing) we might have #asFormAutoRange conversion methods for other kinds of objects.


<details>
	<summary>See more</summary>
	
	asFormAutoRange
	"In optional packages (LinearAlgebra, ImageProcessing) we might have #asFormAutoRange
	 conversion methods for other kinds of objects."
	^self
</details>

#### Form>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream isText
		ifTrue:
			[aStream withAttribute: (TextAnchor new anchoredFormOrMorph: self) do: [aStream nextPut: $*].
			^ self].
	aStream
		nextPutAll: self class name;
		nextPut: $(; print: width;
		nextPut: $x; print: height;
		nextPut: $x; print: depth;
		nextPut: $)
</details>

#### Form>>#size

Should no longer be used -- use bitsSize instead. length of variable part of instance.


<details>
	<summary>See more</summary>
	
	size
	"Should no longer be used -- use bitsSize instead.  length of variable part of instance."
	^ super size
</details>

#### Form>>#isTransparentAt: aPoint

Return true if the receiver is transparent at the given point.


<details>
	<summary>See more</summary>
	
	isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	| p d |
	d _ self depth.

	"no transparency at depth 1"
	d = 1 ifTrue: [^ false].
	
	"Check inside the palette"
	d <= 8 ifTrue: [ 
		^(self colorAt: aPoint) isTransparent ].

	p _ self pixelValueAt: aPoint.

	"32bpp, rgba. Pixels with alpha = 0 are transparent"
	d = 32 ifTrue: [
		^(p anyMask: 16rFF000000) not ].

	"pixel pattern 0 is transparent for RGB"
	^p = 0
</details>

#### Form>>#i: i j: j

Compatibility with Matrices


<details>
	<summary>See more</summary>
	
	i: i j: j
	"Compatibility with Matrices"
	^ self colorAt: j@i -1
</details>

#### Form>>#magnifyTo: extent

Answer a Form created as a scaling of the receiver.


<details>
	<summary>See more</summary>
	
	magnifyTo: extent
	"Answer a Form created as a scaling of the receiver."

	^ self
		magnify: self boundingBox
		to: extent
		smoothing: 3
</details>

#### Form>>#displayAt: aDisplayPoint

Display the receiver located at aDisplayPoint with default settings for the displayMedium, rule and halftone.


<details>
	<summary>See more</summary>
	
	displayAt: aDisplayPoint 
	"Display the receiver located at aDisplayPoint with default settings for 
	the displayMedium, rule and halftone."

	self displayOn: Display
		at: aDisplayPoint
		clippingBox: Display boundingBox
		rule: Form over
</details>

#### Form>>#copy: aRect

Return a new form which derives from the portion of the original form delineated by aRect.


<details>
	<summary>See more</summary>
	
	copy: aRect
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	^ newForm copyBits: aRect from: self at: `0@0`
		clippingBox: newForm boundingBox rule: Form over
</details>

#### Form>>#mightBeTranslucent

Answer whether this form may be translucent


<details>
	<summary>See more</summary>
	
	mightBeTranslucent
	"Answer whether this form may be translucent"
	^self depth = 32
</details>

#### Form>>#pixelValueFor: aColor

Return the pixel word for representing the given color on the receiver


<details>
	<summary>See more</summary>
	
	pixelValueFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor pixelValueForDepth: self depth
</details>

#### Form>>#writeJPEGfileNamed: fileName

Write a JPEG file to the given filename using default settings


<details>
	<summary>See more</summary>
	
	writeJPEGfileNamed: fileName 
	"Write a JPEG file to the given filename using default settings"

	self writeJPEGfileNamed: fileName progressive: false

"
Display writeJPEGfileNamed: 'display.jpeg'
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg'
"
</details>

#### Form>>#asForm

In optional packages (LinearAlgebra, ImageProcessing) we might have #asForm conversion methods for other kinds of objects.


<details>
	<summary>See more</summary>
	
	asForm
	"In optional packages (LinearAlgebra, ImageProcessing) we might have #asForm
	 conversion methods for other kinds of objects."
	^self
</details>

#### Form>>#pageWarp: otherImage at: topLeft forward: forward

Produce a page-turning illusion that gradually reveals otherImage located at topLeft in this form. forward == true means turn pages toward you, else away. [ignored for now]


<details>
	<summary>See more</summary>
	
	pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect _ otherImage boundingBox.
	oldPage _ self copy: (pageRect translatedBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: `Color black`.
	oldBottom _ self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps _ 8.
	buffer _ Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d _ pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
					p _ pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: `0@0` rule: Form over.
					p _ pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect _ pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad _ Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp _ (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceToScreen.
		].

	buffer copy: pageRect from: otherImage to: `0@0` rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceToScreen.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
</details>

#### Form>>#innerPixelRectFor: pv orNot: not

Return a rectangle describing the smallest part of me that includes all pixels of value pv. Note: If orNot is true, then produce a copy that includes all pixels that are DIFFERENT from the supplied (background) value


<details>
	<summary>See more</summary>
	
	innerPixelRectFor: pv orNot: not
	"Return a rectangle describing the smallest part of me that includes 
	all pixels of value pv.
	Note:  If orNot is true, then produce a copy that includes all pixels
	that are DIFFERENT from the supplied (background) value"

	| xTally yTally |
	xTally _ self xTallyPixelValue: pv orNot: not.
	yTally _ self yTallyPixelValue: pv orNot: not.
	^ ((xTally findFirst: [:t | t>0]) - 1) @ ((yTally findFirst: [:t | t>0]) - 1)
		corner:
			(xTally findLast: [:t | t>0])@(yTally findLast: [:t | t>0])
</details>

#### Form>>#copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map

Make up a BitBlt table and copy the bits. Use a colorMap.


<details>
	<summary>See more</summary>
	
	copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map
	"Make up a BitBlt table and copy the bits.  Use a colorMap."

	((BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: sourceRect origin
		extent: sourceRect extent
		clipRect: clipRect) colorMap: map) copyBits
</details>

#### Form>>#readAttributesFrom: aBinaryStream

<details>
	<summary>See more</summary>
	
	readAttributesFrom: aBinaryStream
	| offsetX offsetY |
	depth _ aBinaryStream next.
	(self depth isPowerOfTwo and: [self depth between: 1 and: 32])
		ifFalse: [self error: 'invalid depth; bad Form file?'].
	width _ aBinaryStream nextUnsignedInt16BigEndian: true.
	height _ aBinaryStream nextUnsignedInt16BigEndian: true.
	offsetX  _ aBinaryStream nextSignedInt16BigEndian: true.
	offsetY _ aBinaryStream nextSignedInt16BigEndian: true.
	offset _ Point x: offsetX y: offsetY
</details>

#### Form>>#writeFileNamed: filename

Display writeFileNamed: 'pp.bmp' Display writeFileNamed: 'pp.jpg' Display writeFileNamed: 'pp.png' Display writeFileNamed: 'ppp'


<details>
	<summary>See more</summary>
	
	writeFileNamed: filename 
	" 
	Display writeFileNamed: 'pp.bmp'
	Display writeFileNamed: 'pp.jpg'
	Display writeFileNamed: 'pp.png'
	Display writeFileNamed: 'ppp'
	"
	ImageReadWriter write: self onFileNamed: filename
</details>

#### Form>>#postCopy

self is a shallow copy, subclasses should copy fields as necessary to complete the full copy


<details>
	<summary>See more</summary>
	
	postCopy
	self
		 bits: bits copy;
		 offset: offset copy
</details>

#### Form>>#fillWhite: aRectangle

Set all bits in the receiver's area defined by aRectangle to white.


<details>
	<summary>See more</summary>
	
	fillWhite: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to white."

	self fill: aRectangle rule: Form over fillColor: `Color white`
</details>

#### Form>>#bitPatternFor: aColor

Return the pixel word for representing the given color on the receiver


<details>
	<summary>See more</summary>
	
	bitPatternFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForDepth: self depth
</details>

#### Form>>#makeAllPixelsAlpha: a

Fix the alpha channel if the receiver is 32bit Set alpha values to 255 for all pixels. Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16). See #fixAlpha


<details>
	<summary>See more</summary>
	
	makeAllPixelsAlpha: a
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 1.
	bitBlt fillBitmap: (Bitmap with: (16r00FFFFFF)).
	bitBlt copyBits.
	bitBlt combinationRule: 7.
	bitBlt fillBitmap: (Bitmap with: (a bitShift: 24)).
	bitBlt copyBits
</details>

#### Form>>#copy: sourceRectangle from: sourceForm to: destPt rule: rule

<details>
	<summary>See more</summary>
	
	copy: sourceRectangle from: sourceForm to: destPt rule: rule
	^ self copy: (destPt extent: sourceRectangle extent)
		from: sourceRectangle topLeft in: sourceForm rule: rule
</details>

#### Form>>#copyFromByteArray: bigEndianByteArray

This method should work with either byte orderings. See comment at Bitmap>>#asByteArray Also see #copyFromByteArray2:to:


<details>
	<summary>See more</summary>
	
	copyFromByteArray: bigEndianByteArray
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray2:to:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: `0 @ 0` rule: Form over
</details>

#### Form>>#isBigEndian

Return true if the receiver contains big endian pixels, meaning the left-most pixel is stored in the most significant bits of a word.


<details>
	<summary>See more</summary>
	
	isBigEndian
	"Return true if the receiver contains big endian pixels, meaning the left-most pixel is stored in the most significant bits of a word."
	^depth > 0
</details>

#### Form>>#wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

<details>
	<summary>See more</summary>
	
	wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectsOrNil waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [ clipRect _ clipRect intersect: clipBox ].
	[ rectsOrNil _ rectForIndexBlock value: (i _ i + 1) ]
		whileNotNil: [
			t _ Time localMillisecondClock.
			rectsOrNil do: [ :r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over ].
			Display forceToScreen.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]]
</details>

#### Form>>#fillBlack: aRectangle

Set all bits in the receiver's area defined by aRectangle to black (ones).


<details>
	<summary>See more</summary>
	
	fillBlack: aRectangle 
	"Set all bits in the receiver's area defined by aRectangle to black (ones)."

	self fill: aRectangle rule: Form over fillColor: `Color black`
</details>

#### Form>>#writeAttributesOn: file

<details>
	<summary>See more</summary>
	
	writeAttributesOn: file

	file nextPut: depth.
	file nextUnsignedInt16Put: width bigEndian: true.
	file nextUnsignedInt16Put: height bigEndian: true.
	file nextSignedInt16Put: self offset x bigEndian: true.
	file nextSignedInt16Put: self offset y bigEndian: true
</details>

#### Form>>#icon

Answer a 16 x 16 icon of myself


<details>
	<summary>See more</summary>
	
	icon
	"Answer a 16 x 16 icon of myself"
	
	^self magnifyTo: `16 @ 16`
</details>

#### Form>>#reverse

Change all the bits in the receiver that are white to black, and the ones that are black to white. Display reverse


<details>
	<summary>See more</summary>
	
	reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white.
	Display reverse
	"

	self fill: self boundingBox rule: Form reverse fillColor: `Color white`
</details>

#### Form>>#offset: aPoint

<details>
	<summary>See more</summary>
	
	offset: aPoint

	offset _ aPoint
</details>

#### Form>>#newColorMap

Return an uninitialized color map array appropriate to this Form's depth.


<details>
	<summary>See more</summary>
	
	newColorMap 
	"Return an uninitialized color map array appropriate to this Form's depth."

	^ Bitmap new: (1 bitShift: (self depth min: 15))

</details>

#### Form>>#pixelValueAt: aPoint

Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.


<details>
	<summary>See more</summary>
	
	pixelValueAt: aPoint
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	^ self primPixelValueAtX: aPoint x y: aPoint y
</details>

#### Form>>#magnifyBy: scale

Answer a Form created as a scaling of the receiver. Scale may be a Float, and may be greater or less than 1.0.


<details>
	<summary>See more</summary>
	
	magnifyBy: scale 
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."

	^ self magnify: self boundingBox by: scale
			smoothing: (scale magnitude < 1 ifTrue: [2] ifFalse: [1])
</details>

#### Form>>#fillGray

Set all bits in the receiver to gray.


<details>
	<summary>See more</summary>
	
	fillGray
	"Set all bits in the receiver to gray."

	self fill: self boundingBox fillColor: `Color gray`
</details>

#### Form>>#nativeDepth

Return the 'native' depth of the receiver, e.g., including the endianess


<details>
	<summary>See more</summary>
	
	nativeDepth
	"Return the 'native' depth of the receiver, e.g., including the endianess"
	^depth
</details>

#### Form>>#primPrintHScale: hScale vScale: vScale landscape: aBoolean

On platforms that support it, this primitive prints the receiver, assumed to be a Form, to the default printer.


<details>
	<summary>See more</summary>
	
	primPrintHScale: hScale vScale: vScale landscape: aBoolean
	"On platforms that support it, this primitive prints the receiver, assumed to be a Form, to the default printer."
	"(Form extent: 10@10) primPrintHScale: 1.0 vScale: 1.0 landscape: true"

	<primitive: 232>
	self primitiveFailed

</details>

#### Form>>#asFormOfDepth: d

<details>
	<summary>See more</summary>
	
	asFormOfDepth: d
	| newForm source |
	d = depth ifTrue: [ ^self ].
	source _ (self depth = 32 and: [ d abs < 32 ])
		ifTrue: [ self copy convertAlphaToZeroValueTransparency ]
		ifFalse: [ self ].
	newForm _ Form extent: source extent depth: d.
	(BitBlt toForm: newForm)
		colorMap: (source colormapIfNeededFor: newForm);
		copy: source boundingBox
		from: `0@0` in: source
		fillColor: nil rule: Form over.
	"If we build a 32bpp from one of smaller depth,
	it will have zero in the alpha channel (until BitBlt is fixed!)"
	(newForm depth = 32 and: [self depth < 32]) ifTrue: [
		newForm fixAlpha ].
	^ newForm
</details>

#### Form>>#setExtent: extent depth: bitsPerPixel

Create a virtual bit map with the given extent and bitsPerPixel.


<details>
	<summary>See more</summary>
	
	setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	width _ extent x asInteger.
	width < 0 ifTrue: [width _ 0].
	height _ extent y asInteger.
	height < 0 ifTrue: [height _ 0].
	depth _ bitsPerPixel.
	bits _ Bitmap new: self bitsSize
</details>

#### Form>>#fill: aRectangle rule: anInteger fillColor: aForm

Replace a rectangular area of the receiver with the pattern described by aForm according to the rule anInteger.


<details>
	<summary>See more</summary>
	
	fill: aRectangle rule: anInteger fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by aForm 
	according to the rule anInteger."
	(BitBlt toForm: self)
		copy: aRectangle
		from: `0@0` in: nil
		fillColor: aForm rule: anInteger
</details>

#### Form>>#display

Display the receiver on the Display at location 0,0.


<details>
	<summary>See more</summary>
	
	display 
	"Display the receiver on the Display at location 0,0."

	self displayOn: Display
</details>

#### Form>>#copyBits: sourceForm at: destOrigin translucent: factor

Make up a BitBlt table and copy the bits with the given colorMap.


<details>
	<summary>See more</summary>
	
	copyBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt 
		destForm: self
		sourceForm: sourceForm
		combinationRule: 30
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 copyBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"
</details>

#### Form>>#flippedBy: direction

Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation) Form lena display. (Form lena flippedBy: #vertical) display. (Form lena flippedBy: #horizontal) display. (Form lena flippedBy: #both) display.


<details>
	<summary>See more</summary>
	
	flippedBy: direction
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	Form lena display.
	(Form lena flippedBy: #vertical) display.
	(Form lena flippedBy: #horizontal) display.
	(Form lena flippedBy: #both) display.
	"
	| newForm quad |
	newForm _ self class extent: self extent depth: depth.
	quad _ self boundingBox innerCorners.
	quad _ (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 	-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"
</details>

#### Form>>#preMultiplyAlpha

Pre-multiply each pixel by its alpha, for proper alpha compositing (BitBlt rule 34). E.g., half-transparent green 16r7F00FF00 becomes 16r7F007F00


<details>
	<summary>See more</summary>
	
	preMultiplyAlpha
	"Pre-multiply each pixel by its alpha, for proper alpha compositing (BitBlt rule 34).
	E.g., half-transparent green 16r7F00FF00 becomes 16r7F007F00"

	| v a r g b |
	depth = 32 ifFalse: [^self].
	1 to: bits size do: [ :i |
		v := bits at: i.
		a := v bitShift: -24.
		r := ((v bitShift: -16) bitAnd: 255) * a // 255.
		g := ((v bitShift: -8) bitAnd: 255) * a // 255.
		b := (v bitAnd: 255) * a // 255.
		bits at: i put: (a bitShift: 24) + (r bitShift: 16) + (g bitShift: 8) + b].
</details>

#### Form>>#= other

Compare for equal contents. Expensive! See comment at #hash


<details>
	<summary>See more</summary>
	
	= other 
	"Compare for equal contents. Expensive!
	See comment at #hash"

	self == other ifTrue: [ ^ true ].

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = self depth
					and: [ other offset = self offset
						and: [ other bits = bits ]]]]]
</details>

#### Form>>#reverse: aRectangle

Change all the bits in the receiver's area that intersects with aRectangle that are white to black, and the ones that are black to white.


<details>
	<summary>See more</summary>
	
	reverse: aRectangle
	"Change all the bits in the receiver's area that intersects with aRectangle 
	that are white to black, and the ones that are black to white."

	self fill: aRectangle rule: Form reverse fillColor: `Color white`
</details>

#### Form>>#displayOn: aForm at: aDisplayPoint

Display the receiver located at aDisplayPoint with default settings for rule and halftone.


<details>
	<summary>See more</summary>
	
	displayOn: aForm at: aDisplayPoint
	"Display the receiver located at aDisplayPoint with default settings for 
	rule and halftone."

	| toBeDrawn rule |
	"Rule Form paint treats pixels with a value of zero as transparent"
	toBeDrawn _ self.
	(aForm depth = 32 and: [ self depth = 32 ]) 
		ifTrue: [ rule _ Form blend ] 	"Handle translucent pixels correctly. Requires both source and dest of 32bpp"
		ifFalse: [
			"Warning. Using 'Form paint' with a 32bpp source that includes
			traslucent or transparent alphas will give incorrect results (alpha values will be ignored).
			Doing this might be terribly slow. It is best to convert to lower depth on image load."
			"self depth = 32 ifTrue: [
				toBeDrawn _ self asFormOfDepth: aForm depth ]."
			rule _ Form paint ].
	toBeDrawn displayOn: aForm
		at: aDisplayPoint
		clippingBox: aForm boundingBox
		rule: rule
</details>

#### Form>>#fillFromXColorBlock: colorBlock

Horizontal Gradient Fill. Supply relative x in [0.0 ... 1.0] to colorBlock, and paint each pixel with the color that comes back


<details>
	<summary>See more</summary>
	
	fillFromXColorBlock: colorBlock
	"Horizontal Gradient Fill.
	Supply relative x in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| xRel |
	0 to: width-1 do:
		[:x |  xRel _ x asFloat / (width-1) asFloat.
		self fill: (x@0 extent: 1@height) 
			fillColor: (colorBlock value: xRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromXColorBlock: [:x | Color r: x g: 0.0 b: 0.5]) display
"
</details>

#### Form>>#wipeImage: otherImage at: topLeft delta: delta clippingBox: clipBox

<details>
	<summary>See more</summary>
	
	wipeImage: otherImage at: topLeft delta: delta clippingBox: clipBox

	| wipeRect bb nSteps |
	bb _ otherImage boundingBox.
	wipeRect _ delta x = 0
		ifTrue:
		[delta y = 0 ifTrue: [nSteps _ 1. bb "allow 0@0"] ifFalse: [
		nSteps _ bb height//delta y abs + 1.  "Vertical movement"
		delta y > 0
			ifTrue: [bb topLeft extent: bb width@delta y]
			ifFalse: [bb bottomLeft+delta extent: bb width@delta y negated]]]
		ifFalse:
		[nSteps _ bb width//delta x abs + 1.  "Horizontal movement"
		delta x > 0
			ifTrue: [bb topLeft extent: delta x@bb height]
			ifFalse: [bb topRight+delta extent: delta x negated@bb height]].
	^ self wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: [ :i |
		i <= nSteps
			ifTrue: [{wipeRect translatedBy: (delta* (i-1))}]
			ifFalse: [nil]]
</details>

#### Form>>#wordsPerLine

<details>
	<summary>See more</summary>
	
	wordsPerLine
	| pixPerWord |
	pixPerWord _ self pixelsPerWord.
	^ width + pixPerWord - 1 // pixPerWord
</details>

#### Form>>#displayAutoRangeAt: aPoint

Display receiver, mapping used range to available gray levels


<details>
	<summary>See more</summary>
	
	displayAutoRangeAt: aPoint
	"Display receiver, mapping used range to available gray levels"

	^ self displayAutoRangeAt: aPoint zoom: 1
</details>

#### Form>>#colormapIfNeededForGray8bpp

Return a colormap for displaying the receiver at the given depth. Note: Uses 5 bits per color component. 32bit Forms will lose information!


<details>
	<summary>See more</summary>
	
	colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth.
	Note: Uses 5 bits per color component. 32bit Forms will lose information!"

	^ Color cachedColormapForGrayFrom: self depth
</details>

#### Form>>#isAllWhite

Answer whether all bits in the receiver are white (=0).


<details>
	<summary>See more</summary>
	
	isAllWhite
	"Answer whether all bits in the receiver are white (=0)."

	1 to: bits size do: [:i | (bits at: i) = 0 ifFalse: [^ false]].
	^ true
</details>

#### Form>>#bitsSize

<details>
	<summary>See more</summary>
	
	bitsSize

	^ self wordsPerLine * height
</details>

#### Form>>#hackBits64: bitThing

This method provides an initialization so that BitBlt may be used, eg, to copy ByteArrays and other non-pointer objects efficiently. The resulting form looks 8 wide, 8 deep, and bitThing-size-in-words high.


<details>
	<summary>See more</summary>
	
	hackBits64: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 8 wide, 8 deep, and bitThing-size-in-words high."
	width _ 8.
	depth _ 8.
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	bitThing class isBytes
		ifTrue: [height _ bitThing basicSize // 8]
		ifFalse: [height _ bitThing basicSize // 2].
	bits _ bitThing
</details>

#### Form>>#copyFromByteArray2: bigEndianByteArray to: i

This method should work with either byte orderings. See comment at Bitmap>>#asByteArray Also see #copyFromByteArray:


<details>
	<summary>See more</summary>
	
	copyFromByteArray2: bigEndianByteArray to: i
	"This method should work with either byte orderings.
	See comment at Bitmap>>#asByteArray
	Also see #copyFromByteArray:"

	| myHack byteHack |
	myHack := Form new hackBits: bits.
	byteHack := Form new hackBits: bigEndianByteArray.
	"We are passing a ByteArray instead of a Words object. Will be accessed according to native endianness."
	Smalltalk isLittleEndian = self isLittleEndian ifFalse: [byteHack swapEndianness].
	byteHack displayOn: myHack at: 0@i rule: Form over
</details>

#### Form>>#pixelsPerWord

<details>
	<summary>See more</summary>
	
	pixelsPerWord
	^32 // self depth
</details>

#### Form>>#fillGray: aRectangle

Set all bits in the receiver's area defined by aRectangle to the gray mask.


<details>
	<summary>See more</summary>
	
	fillGray: aRectangle
	"Set all bits in the receiver's area defined by aRectangle to the gray mask."

	self fill: aRectangle rule: Form over fillColor: `Color gray`
</details>

#### Form>>#depth

<details>
	<summary>See more</summary>
	
	depth
	^ depth < 0 ifTrue:[0-depth] ifFalse:[depth]
</details>

#### Form>>#eraseShape: bwForm

use bwForm as a mask to clear all pixels where bwForm has 1's


<details>
	<summary>See more</summary>
	
	eraseShape: bwForm
	"use bwForm as a mask to clear all pixels where bwForm has 1's"
	((BitBlt destForm: self sourceForm: bwForm
		combinationRule: Form erase1bitShape	"Cut a hole in the picture with my mask"
		destOrigin: bwForm offset 
		sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits
</details>

#### Form>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	self storeOn: aStream base: 10
</details>

#### Form>>#colormapIfNeededForDepth: destDepth

Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.


<details>
	<summary>See more</summary>
	
	colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	self depth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"
	^ Color colorMapIfNeededFrom: self depth to: destDepth

</details>

#### Form>>#width

<details>
	<summary>See more</summary>
	
	width
	^ width
</details>

#### Form>>#colormapIfNeededFor: destForm

Return a ColorMap mapping from the receiver to destForm. double dispatch


<details>
	<summary>See more</summary>
	
	colormapIfNeededFor: destForm
	"Return a ColorMap mapping from the receiver to destForm.
	double dispatch"

	^destForm colormapIfNeededFrom: self
</details>

#### Form>>#border: aRectangle width: borderWidth

Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth. Uses black for drawing the border.


<details>
	<summary>See more</summary>
	
	border: aRectangle width: borderWidth 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses black for 
	drawing the border."

	self border: aRectangle width: borderWidth fillColor: `Color black`
</details>

#### Form>>#orderedDither32To16

Do an ordered dithering for converting from 32 to 16 bit depth.


<details>
	<summary>See more</summary>
	
	orderedDither32To16
	"Do an ordered dithering for converting from 32 to 16 bit depth."
	| ditherMatrix ii out inBits outBits index pv dmv r di dmi dmo g b pvOut outIndex |
	self depth = 32 ifFalse:[^self error:'Must be 32bit for this'].
	ditherMatrix _ #(	0	8	2	10
						12	4	14	6
						3	11	1	9
						15	7	13	5).
	ii _ (0 to: 31) collect:[:i| i].
	out _ Form extent: self extent depth: 16.
	inBits _ self bits.
	outBits _ out bits.
	index _ outIndex _ 0.
	pvOut _ 0.
	0 to: self height-1 do:[:y|
		0 to: self width-1 do:[:x|
			pv _ inBits at: (index _ index + 1).
			dmv _ ditherMatrix at: (y bitAnd: 3) * 4 + (x bitAnd: 3) + 1.
			r _ pv bitAnd: 255.	di _ r * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			r _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			g _ (pv bitShift: -8) bitAnd: 255.	di _ g * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			g _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			b _ (pv bitShift: -16) bitAnd: 255.	di _ b * 496 bitShift: -8.
			dmi _ di bitAnd: 15.	dmo _ di bitShift: -4.
			b _ dmv < dmi ifTrue:[ii at: 2+dmo] ifFalse:[ii at: 1+dmo].
			pvOut _ (pvOut bitShift: 16) + 
						(b bitShift: 10) + (g bitShift: 5) + r.
			(x bitAnd: 1) = 1 ifTrue:[
				outBits at: (outIndex _ outIndex+1) put: pvOut.
				pvOut _ 0].
		].
		(self width bitAnd: 1) = 1 ifTrue:[
			outBits at: (outIndex _ outIndex+1) put: (pvOut bitShift: -16).
			pvOut _ 0].
	].
	^out
</details>

#### Form>>#fillFromXYColorBlock: colorBlock

General Gradient Fill. Supply relative x and y in [0.0 ... 1.0] to colorBlock, and paint each pixel with the color that comes back


<details>
	<summary>See more</summary>
	
	fillFromXYColorBlock: colorBlock
	"General Gradient Fill.
	Supply relative x and y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| poker yRel xRel |
	poker _ BitBlt bitPokerToForm: self.
	0 to: height-1 do:
		[:y | yRel _ y asFloat / (height-1) asFloat.
		0 to: width-1 do:
			[:x |  xRel _ x asFloat / (width-1) asFloat.
			poker pixelAt: x@y
				put: ((colorBlock value: xRel value: yRel) pixelWordForDepth: self depth)]]
"
 | d |
((Form extent: 100@20 depth: Display depth)
	fillFromXYColorBlock:
	[:x :y | d _ 1.0 - (x - 0.5) abs - (y - 0.5) abs.
	Color r: d g: 0 b: 1.0-d]) display
"
</details>

#### Form>>#colorAt: aPoint put: aColor

Store a Color into the pixel at coordinate aPoint.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint put: aColor
	"Store a Color into the pixel at coordinate aPoint.  "

	self pixelValueAt: aPoint put: (self pixelValueFor: aColor).

"
[Sensor isAnyButtonPressed] whileFalse:
	[Display colorAt: Sensor mousePoint put: Color red]
"
</details>

#### Form>>#fromDisplay: aRectangle

Create a virtual bit map from a user specified rectangular area on the display screen. Reallocates bitmap only if aRectangle ~= the receiver's extent.


<details>
	<summary>See more</summary>
	
	fromDisplay: aRectangle 
	"Create a virtual bit map from a user specified rectangular area on the 
	display screen. Reallocates bitmap only if aRectangle ~= the receiver's 
	extent."

	(width = aRectangle width and: [height = aRectangle height])
		ifFalse: [self setExtent: aRectangle extent depth: depth].
	self
		copyBits: (aRectangle origin extent: self extent)
		from: Display
		at: `0 @ 0`
		clippingBox: self boundingBox
		rule: Form over
</details>

#### Form>>#offset

<details>
	<summary>See more</summary>
	
	offset
	^offset ifNil: [`0@0`]
</details>

#### Form>>#displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger

Display the receiver located at aPoint with default setting for the halftone and clippingBox.


<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium at: aDisplayPoint rule: ruleInteger
	"Display the receiver located at aPoint with default setting for the 
	halftone and clippingBox."

	self displayOn: aDisplayMedium
		at: aDisplayPoint
		clippingBox: aDisplayMedium boundingBox
		rule: ruleInteger
</details>

#### Form>>#maskingMap

Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero.


<details>
	<summary>See more</summary>
	
	maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	"Warning: The behavior is incorrect for 32bpp Forms with translucency.
	Color maps are RGB only, they don't map on alpha values. Alpha is ignored when using the color map. This means that the only value mapped as transparent is pixel value 0,
	that is R=0, G=0, B=0, Alpha=0.
	However, a 32bpp form could have, for instance R=255, G=0, B=0, Alpha=0, also meaning transparent. But this will be mapped as if the source was red, not transparent."
	^Color maskingMap: self depth
</details>

#### Form>>#displayOn: aDisplayMedium

Simple default display in order to see the receiver in the upper left corner of screen.


<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium
	"Simple default display in order to see the receiver in the upper left 
	corner of screen."

	self displayOn: aDisplayMedium at: `0 @ 0`
</details>

#### Form>>#readBitsFrom: aBinaryStream

<details>
	<summary>See more</summary>
	
	readBitsFrom: aBinaryStream
	
	bits _ Bitmap newFromStream: aBinaryStream.
	bits size = self bitsSize ifFalse: [self error: 'wrong bitmap size; bad Form file?'].
	^ self

</details>

#### Form>>#borderWidth: borderWidth borderHeight: borderHeight fillColor: aColor

<details>
	<summary>See more</summary>
	
	borderWidth: borderWidth borderHeight: borderHeight fillColor: aColor
	self border: self boundingBox width: borderWidth borderHeight: borderHeight fillColor: aColor
</details>

#### Form>>#primCountBits

Count the non-zero pixels of this form.


<details>
	<summary>See more</summary>
	
	primCountBits
	"Count the non-zero pixels of this form."
	self depth > 8 ifTrue: [
		^(self asFormOfDepth: 8) primCountBits].
	^ (BitBlt toForm: self)
		fillColor: (Bitmap with: 0);
		destRect: self boundingBox;
		combinationRule: 32;
		copyBits
</details>

#### Form>>#smear: dir distance: dist

Smear any black pixels in this form in the direction dir in Log N steps


<details>
	<summary>See more</summary>
	
	smear: dir distance: dist
	"Smear any black pixels in this form in the direction dir in Log N steps"
	| skew bb |
	bb _ BitBlt destForm: self sourceForm: self
		combinationRule: Form under destOrigin: `0@0` sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox.
	skew _ 1.
	[skew < dist] whileTrue: [
		bb destOrigin: dir*skew; copyBits.
		skew _ skew+skew]
</details>

#### Form>>#cgForPixelValue: pv orNot: not

Return the center of gravity for all pixels of value pv. Note: If orNot is true, then produce the center of gravity for all pixels that are DIFFERENT from the supplied (background) value


<details>
	<summary>See more</summary>
	
	cgForPixelValue: pv orNot: not
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY _ (Array with: (self xTallyPixelValue: pv orNot: not)
					with: (self yTallyPixelValue: pv orNot: not)) collect: [ :profile |	"For both x and y profiles..."
		pixCount _ 0.  weighted _ 0.
		profile withIndexDo: [ :t :i |
			pixCount _ pixCount + t.
			weighted _ weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r _ Sensor mousePoint extent: 50@50.
	f _ Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg _ f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"
</details>

#### Form>>#convertAlphaToZeroValueTransparency

For a 32bpp Form, for each pixel, take the alpha value, and if less than 128 make the pixel value zero (i.e. transparent for 2, 4, 8 and 16 bpp) Side effect: make value of alpha = 0 or 128 for all pixels. Not a problem if we're converting to lower bpp anyway... This method also loses the lsb of the color components. Again, not a problem for going to lower bpp.


<details>
	<summary>See more</summary>
	
	convertAlphaToZeroValueTransparency
	"For a 32bpp Form, for each pixel, take the alpha value, and if less than 128
	make the pixel value zero (i.e. transparent for 2, 4, 8 and 16 bpp)
	Side effect: make value of alpha = 0 or 128 for all pixels. Not a problem if we're converting to lower bpp anyway...
	This method also loses the lsb of the color components. Again, not a problem for going to lower bpp.
	"
	"
	| bananas1 |
	bananas1 _ Form fromFileNamed: 'bananas1.png'.
	bananas1 convertAlphaToZeroValueTransparency..
	bananas1 convertAlphaToZeroValueTransparency.
	(bananas1 asFormOfDepth: 16) display
	"
	| bitBlt map multiplier |
	self depth = 32 ifFalse: [
		^self ].
	
	"Prepare multiplier to multiply each pixel by 0.5 or 0 (according to alpha)"
	multiplier _ Form extent: self extent depth: 32.
	map _ ColorMap
		masks: { 16r80000000. 16r80000000. 16r80000000. 16r80000000}
		shifts:  { 0.  -8. -16. -24 }.
	bitBlt _ BitBlt new.
	bitBlt
		setDestForm: multiplier;
		sourceForm: self;
		colorMap: map;
		combinationRule: Form over;
		copyBits.
	"Now fix for the fact that using a ColorMap turns pixelValues 0's into 1's"
	bitBlt
		sourceForm: nil;
		colorMap: nil;
		fillBitmap: (Bitmap with: 16rFFFFFFFE);
		combinationRule: Form and;
		copyBits.
	"Apply multiplier to ourselves"
	bitBlt
		fillBitmap: nil;
		setDestForm: self;
		sourceForm: multiplier;
		combinationRule: Form rgbMul;
		copyBits.
	"Now correct for the fact that the multiplier had 0.5's and 0's, not 1's and 0's"
	bitBlt
		sourceForm: self;
		combinationRule: 20; "Form rgbAdd"
		copyBits
</details>

#### Form>>#fillShape: aShapeForm fillColor: aColor

Fill a region corresponding to 1 bits in aShapeForm with aColor


<details>
	<summary>See more</summary>
	
	fillShape: aShapeForm fillColor: aColor
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	^ self fillShape: aShapeForm fillColor: aColor at: `0@0`
</details>

#### Form>>#fillColor: aColor

Set all pixels in the receiver to the color. Must be a correct color for this depth of medium. TK 1 Jun 96


<details>
	<summary>See more</summary>
	
	fillColor: aColor
	"Set all pixels in the receiver to the color.  Must be a correct color for this depth of medium.  TK 1 Jun 96"

	self fill: self boundingBox fillColor: aColor
</details>

#### Form>>#storeBitsOn: aStream base: anInteger

<details>
	<summary>See more</summary>
	
	storeBitsOn: aStream base: anInteger
	bits do: [ :word | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream newLineTab: 2].
		anInteger = 2
			ifTrue: [
				"Print binary with radix, but padded, so the bit pattern is easy to see."
				aStream nextPut: $2. 
				aStream nextPut: $r. 
				word printOn: aStream base: 2 length: 32 padded: true ]
			ifFalse: [
				word storeOn: aStream base: anInteger ]]
</details>

#### Form>>#zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp

Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40. Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.


<details>
	<summary>See more</summary>
	
	zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp 
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.
	Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40."
	| nSteps j bigR lilR minTime startTime lead |
	nSteps _ 16.
	minTime _ 500.  "milliseconds"
	startTime _ Time localMillisecondClock.
	^ self wipeImage: otherImage at: topLeft clippingBox: nil rectForIndex: [ :i | "i runs from 1 to nsteps"
		i > nSteps
			ifTrue: [nil "indicates all done"]
			ifFalse: [
				"If we are going too fast, delay for a bit"
				lead _ startTime + (i-1*minTime//nSteps) - Time localMillisecondClock.
				lead > 10 ifTrue: [(Delay forMilliseconds: lead) wait].

				"Return an array with the difference rectangles for this step."
				j _ goingIn ifTrue: [i] ifFalse: [nSteps+1-i].
				bigR _ vp - (vp*(j)//nSteps) corner:
					vp + (otherImage extent-vp*(j)//nSteps).
				lilR _ vp - (vp*(j-1)//nSteps) corner:
					vp + (otherImage extent-vp*(j-1)//nSteps).
				bigR areasOutside: lilR ]]
</details>

#### Form>>#asGrayScaleAndTransparent: componentIndex

Native depth


<details>
	<summary>See more</summary>
	
	asGrayScaleAndTransparent: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayScaleAndTransparent: componentIndex ].

	result _ ColorForm grayScaleAndTransparentExtent: self extent.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		copyBits.

	"final BitBlt to zero-out pixels that were truely transparent in the original"
	map _ Bitmap new: 512.
	map at: 1 put: 16rFF.
	(BitBlt toForm: result)
		sourceForm: self;
		sourceRect: self boundingBox;
		destOrigin: `0@0`;
		combinationRule: Form erase;
		colorMap: map;
		copyBits.
	^ result
</details>

#### Form>>#paintBits: sourceForm at: destOrigin translucent: factor

Make up a BitBlt table and copy the bits with the given colorMap.


<details>
	<summary>See more</summary>
	
	paintBits: sourceForm at: destOrigin translucent: factor
	"Make up a BitBlt table and copy the bits with the given colorMap."
	(BitBlt destForm: self
		sourceForm: sourceForm
		combinationRule: 31
		destOrigin: destOrigin
		sourceOrigin: `0@0`
		extent: sourceForm extent
		clipRect: self boundingBox)
			copyBitsTranslucent: ((0 max: (factor*255.0) asInteger) min: 255)
"
 | f f2 f3 | f _ Form fromUser. f replaceColor: f dominantColor withColor: Color transparent.
f2 _ Form fromDisplay: (0@0 extent: f extent). f3 _ f2 copy.
0.0 to: 1.0 by: 1.0/32 do:
	[:t | f3 _ f2 copy. f3 paintBits: f at: 0@0 translucent: t.
	f3 displayAt: 0@0. (Delay forMilliseconds: 100) wait].
"
</details>

#### Form>>#border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone

Paint a border whose rectangular area is defined by aRectangle. The width of each edge of the border is determined by the four coordinates of insets. Uses aHalfTone and combinationRule for drawing the border.


<details>
	<summary>See more</summary>
	
	border: aRectangle widthRectangle: insets rule: combinationRule fillColor: aHalfTone
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of each edge of the border is determined by the four coordinates 
	of insets. Uses aHalfTone and combinationRule for drawing the border."

	aRectangle
		areasOutside: (aRectangle insetBy: insets)
		do: [ :edgeStrip |
			self fill: edgeStrip rule: combinationRule fillColor: aHalfTone ]
</details>

#### Form>>#tallyPixelValuesInRect: destRect into: valueTable

Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable.


<details>
	<summary>See more</summary>
	
	tallyPixelValuesInRect: destRect into: valueTable
	"Tally the selected pixels of this Form into valueTable, a Bitmap of depth 2^depth similar to a color map. Answer valueTable."

	(BitBlt toForm: self)
		sourceForm: self;  "src must be given for color map ops"
		sourceOrigin: `0@0`;
		colorMap: valueTable;
		combinationRule: 33;
		destRect: destRect;
		copyBits.
	^ valueTable

"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"
</details>

#### Form>>#copy: destRectangle from: sourcePt in: sourceForm rule: rule

Make up a BitBlt table and copy the bits.


<details>
	<summary>See more</summary>
	
	copy: destRectangle from: sourcePt in: sourceForm rule: rule 
	"Make up a BitBlt table and copy the bits."
	(BitBlt toForm: self)
		copy: destRectangle
		from: sourcePt in: sourceForm
		fillColor: nil rule: rule
</details>

#### Form>>#asGrayForm: componentIndex

Native depth


<details>
	<summary>See more</summary>
	
	asGrayForm: componentIndex
"Native depth"
	"Assume the receiver is a grayscale image. Return a GrayForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit GrayForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)

	If component = 1, take the alpha component
	If component = 2, take the red component
	If component = 3, take the green component
	If component = 4, take the blue component
	"
	| f32 result map mask shift |
	self depth = 32 ifFalse: [
		f32 _ Form extent: self extent depth: 32.
		self displayOn: f32.
		^ f32 asGrayForm: componentIndex ].
	
	result _ GrayForm extent: self extent.
	shift _ #(-24 -16 -8 0) at: componentIndex.
	mask _ #(16rFF000000 16rFF0000 16rFF00 16rFF) at: componentIndex.
	map _ ColorMap masks: { mask. 0. 0. 0 } shifts: { shift. 0. 0. 0 }.
	(BitBlt toForm: result)
		sourceForm: self;
		combinationRule: Form over;
		colorMap: map;
		sourceRect: self boundingBox ;
		destOrigin: `0@0`;
		copyBits.
	^ result
</details>

#### Form>>#xTallyPixelValue: pv orNot: not

Return an array of the number of pixels with value pv by x-value. Note that if not is true, then this will tally those different from pv.


<details>
	<summary>See more</summary>
	
	xTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by x-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice countBlt copyBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: 1@height.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: 1 @ slice height
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: width-1) collect: [ :x |
		copyBlt sourceOrigin: x@0; copyBits.
		countBlt copyBits]
</details>

#### Form>>#border: aRectangle width: borderWidth fillColor: aHalfTone

Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth. Uses aHalfTone for drawing the border.


<details>
	<summary>See more</summary>
	
	border: aRectangle width: borderWidth fillColor: aHalfTone 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: Form over
		fillColor: aHalfTone
</details>

#### Form>>#extent

<details>
	<summary>See more</summary>
	
	extent
	^ width @ height
</details>

#### Form>>#bits: aBitmap

Reset the Bitmap containing the receiver's bits.


<details>
	<summary>See more</summary>
	
	bits: aBitmap 
	"Reset the Bitmap containing the receiver's bits."

	bits _ aBitmap
</details>

#### Form>>#asGrayScale

Redefined in ColorForm as an optimization.


<details>
	<summary>See more</summary>
	
	asGrayScale
	"Redefined in ColorForm as an optimization."

	^ self asGrayForm
</details>

#### Form>>#boundingBox

<details>
	<summary>See more</summary>
	
	boundingBox
	^ Rectangle
		origin: `0 @ 0`
		corner: width @ height
</details>

#### Form>>#fillBlack

Set all bits in the receiver to black (ones).


<details>
	<summary>See more</summary>
	
	fillBlack
	"Set all bits in the receiver to black (ones)."

	self fill: self boundingBox fillColor: `Color black`
</details>

#### Form>>#dominantColor

<details>
	<summary>See more</summary>
	
	dominantColor
	| tally max maxi |
	self depth > 16 ifTrue:
		[^(self asFormOfDepth: 16) dominantColor].
	tally _ self tallyPixelValues.
	max _ maxi _ 0.
	tally withIndexDo: [:n :i | n > max ifTrue: [max _ n. maxi _ i]].
	^ Color colorFromPixelValue: maxi - 1 depth: self depth
</details>

#### Form>>#isLittleEndian

Return true if the receiver contains little endian pixels, meaning the left-most pixel is stored in the least significant bits of a word.


<details>
	<summary>See more</summary>
	
	isLittleEndian
	"Return true if the receiver contains little endian pixels, meaning the left-most pixel is stored in the least significant bits of a word."
	^depth < 0
</details>

#### Form>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Form or: [ super is: aSymbol ]
</details>

#### Form>>#fixAlpha

Fix the alpha channel if the receiver is 32bit For any pixel with zero alpha value, and not zero rgb, set alpha to 255. Note: assumes that a pixel with 0 is meant to be transparent, and won't make it opaque black. See #makeAllPixelsOpaque


<details>
	<summary>See more</summary>
	
	fixAlpha
	"Fix the alpha channel if the receiver is 32bit
	For any pixel with zero alpha value, and not zero rgb, set alpha to 255.
	Note: assumes that a pixel with 0 is meant to be transparent, and won't make it opaque black.
	See #makeAllPixelsOpaque"
	| bb |
	self depth = 32 ifFalse: [ ^self ].
	bb _ BitBlt toForm: self.
	bb combinationRule: 40. "fixAlpha:with:"
	bb copyBits
</details>

#### Form>>#couldBeTranslucent

Answer whether this form may be translucent


<details>
	<summary>See more</summary>
	
	couldBeTranslucent
	"Answer whether this form may be translucent"
	^self depth = 32
</details>

#### Form>>#colorInterpolatedAt: aPoint

Evaluate a bilinear (i.e. cheap) interpolation Like OpenCV's CV_INTER_LINEAR Answer Color transparent if outside image bounds. Copied almost verbatim from FloatImage. Answer Colors with float components. Will be rounded to be stored in, for example, a 32-bit Form.


<details>
	<summary>See more</summary>
	
	colorInterpolatedAt: aPoint
	"Evaluate a bilinear (i.e. cheap) interpolation
	Like OpenCV's CV_INTER_LINEAR
	Answer Color transparent if outside image bounds.
	Copied almost verbatim from FloatImage.
	Answer Colors with float components. Will be rounded to be stored in, for example, a 32-bit Form."

	| x y w interpolated xWeight1 xWeight0 yWeight1 yWeight0 xIndex0 xIndex1 yIndex0 yIndex1 |
	x _ aPoint x.
	y _ aPoint y.
	x < 0.0 ifTrue: [ ^ `Color transparent` ].
	xIndex0 _ x truncated. 		"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	xIndex0 > (width-1) ifTrue: [ ^ `Color transparent` ].
	(xIndex0 = (width-1) and: [ x > (width-1) ]) ifTrue: [ ^ `Color transparent` ].
	xIndex1 _ xIndex0 = (width-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ xIndex0 + 1 ]
		ifTrue: [ xIndex0 ].

	y < 0.0 ifTrue: [ ^ `Color transparent` ].
	yIndex0 _ y truncated.			"Could be #floor. But as we only care for values >=0, it is the same. But faster."
	yIndex0 > (height-1) ifTrue: [ ^ `Color transparent` ].
	(yIndex0 = (height-1) and: [ y > (height-1) ]) ifTrue: [ ^ `Color transparent` ].
	yIndex1 _ yIndex0 = (height-1) 	"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ yIndex0 + 1 ]
		ifTrue: [ yIndex0 ].

	xWeight1 _ x - xIndex0.
	xWeight0 _ 1.0 - xWeight1.

	yWeight1 _ y - yIndex0.
	yWeight0 _ 1.0 - yWeight1.

	"/* perform interpolation */"
	w _ ((self colorAt: xIndex0 @ yIndex0) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex0) * xWeight1).
	interpolated _ w * yWeight0.

	w _ ((self colorAt: xIndex0 @ yIndex1) * xWeight0) +
		((self colorAt: xIndex1 @ yIndex1) * xWeight1).
	interpolated _ w * yWeight1 + interpolated.

	^interpolated
</details>

#### Form>>#primPixelValueAtX: x y: y

Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color


<details>
	<summary>See more</summary>
	
	primPixelValueAtX: x y: y
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth.
	Typical clients use colorAt: to get a Color"

	<primitive: 'primitivePixelValueAt' module:'BitBltPlugin'>
	^(BitBlt bitPeekerFromForm: self) pixelAt: x@y
</details>

#### Form>>#isAnyPixel: pv

Return an array of the number of pixels with value pv by y-value. Note that if not is true, then this will tally those different from pv. Based on #yTallyPixelValue:orNot: Form lena isAnyPixel: 0 Form lena isAnyPixel: 100


<details>
	<summary>See more</summary>
	
	isAnyPixel: pv
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv.
	Based on #yTallyPixelValue:orNot:
	Form lena isAnyPixel: 0
	Form lena isAnyPixel: 100
	"
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	(0 to: height-1) do: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits > 0 ifTrue: [ ^ true ]].
	^ false
</details>

#### Form>>#rotatedBy: deg smoothing: cellSize

Rotate the receiver by the indicated number of degrees.


<details>
	<summary>See more</summary>
	
	rotatedBy: deg smoothing: cellSize
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."
	| side rot warp r1 pts p center |
	side _ 1 + ((width*width) + (height*height)) asFloat sqrt asInteger.
	rot _ Form extent: side@side depth: self depth.
	center _ rot extent // 2.

	"Now compute the sin and cos constants for the rotation angle." 
	warp _ (WarpBlt toForm: rot)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: rot);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form over.
	r1 _ rot boundingBox aligned: center with: self boundingBox center.

	pts _ r1 innerCorners collect: [ :pt |
		p _ pt - r1 center.
		(r1 center x asFloat + (p x asFloat*deg degreeCos) + (p y asFloat*deg degreeSin)) @
		(r1 center y asFloat - (p x asFloat*deg degreeSin) + (p y asFloat*deg degreeCos))].
	warp copyQuad: pts toRect: rot boundingBox.
	^ rot
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedBy: (a _ a+0.1) smoothing: 2) display].
f display
"
</details>

#### Form>>#colorAt: aPoint

Return the color in the pixel at the given point.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint
	"Return the color in the pixel at the given point.  "

	^ Color 
		colorFromPixelValue: (self pixelValueAt: aPoint)
		depth: self depth

</details>

#### Form>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm

	^ self
</details>

#### Form>>#asGrayScaleAndTransparent

Native depth


<details>
	<summary>See more</summary>
	
	asGrayScaleAndTransparent
"Native depth"
	"Assume the receiver is a grayscale image. Return a grayscale ColorForm computed by extracting the brightness levels of one color component. This technique allows a 32-bit Form to be converted to an 8-bit ColorForm to save space while retaining a full 255 levels of gray. (The usual colormapping technique quantizes to 8, 16, or 32 levels, which loses information.)"

	"By default, take the green component"
	^self asGrayScaleAndTransparent: 3
</details>

#### Form>>#writeOnFileNamed: fileName

Saves the receiver on the file fileName in the format: fileCode, depth, extent, offset, bits.


<details>
	<summary>See more</summary>
	
	writeOnFileNamed: fileName 
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStreamDo: [ :file |
		file binary.
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f _ Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"
</details>

#### Form>>#fillFromYColorBlock: colorBlock

Vertical Gradient Fill. Supply relative y in [0.0 ... 1.0] to colorBlock, and paint each pixel with the color that comes back


<details>
	<summary>See more</summary>
	
	fillFromYColorBlock: colorBlock
	"Vertical Gradient Fill.
	Supply relative y in [0.0 ... 1.0] to colorBlock,
	and paint each pixel with the color that comes back"
	| yRel |
	0 to: height-1 do:
		[:y |  yRel _ y asFloat / (height-1) asFloat.
		self fill: (0@y extent: width@1) 
			fillColor: (colorBlock value: yRel)]
"
((Form extent: 100@100 depth: Display depth)
	fillFromYColorBlock: [:y | Color r: y g: 0.0 b: 0.5]) display
"
</details>

#### Form>>#writeOn: file

Write the receiver on the file in the format depth, extent, offset, bits.


<details>
	<summary>See more</summary>
	
	writeOn: file
	"Write the receiver on the file in the format
		depth, extent, offset, bits."
	self writeAttributesOn: file.
	bits writeOn: file
</details>

#### Form>>#i: i j: j put: aColor

Compatibility with Matrices


<details>
	<summary>See more</summary>
	
	i: i j: j put: aColor
	"Compatibility with Matrices"
	^ self colorAt: j@i -1 put: aColor
</details>

#### Form>>#fillWhite

Set all bits in the form to white.


<details>
	<summary>See more</summary>
	
	fillWhite
	"Set all bits in the form to white."

	self fill: self boundingBox fillColor: `Color white`
</details>

#### Form>>#fill: aRectangle fillColor: aForm

Replace a rectangular area of the receiver with the pattern described by aForm according to the rule over.


<details>
	<summary>See more</summary>
	
	fill: aRectangle fillColor: aForm 
	"Replace a rectangular area of the receiver with the pattern described by 
	aForm according to the rule over."

	self fill: aRectangle rule: Form over fillColor: aForm
</details>

#### Form>>#displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: rule

<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: rule

	aDisplayMedium copyBits: self boundingBox
		from: self
		at: aDisplayPoint + self offset
		clippingBox: clipRectangle
		rule: rule
		map: (self colormapIfNeededFor: aDisplayMedium).

</details>

#### Form>>#as8BitColorForm

Simple conversion of zero pixels to transparent. Force it to 8 bits.


<details>
	<summary>See more</summary>
	
	as8BitColorForm
	"Simple conversion of zero pixels to transparent.  Force it to 8 bits."

	| f map |
	f _ ColorForm extent: self extent depth: 8.
	self displayOn: f at: self offset negated.
	map _ Color indexedColors copy.
	map at: 1 put: `Color transparent`.
	f colors: map.
	f offset: self offset.
	^ f

</details>

#### Form>>#writeJPEGfileNamed: fileName  progressive: aBoolean

Write a JPEG file to the given filename using default settings. Make it progressive or not, depending on the boolean argument


<details>
	<summary>See more</summary>
	
	writeJPEGfileNamed: fileName  progressive: aBoolean
	"Write a JPEG file to the given filename using default settings.  Make it progressive or not, depending on the boolean argument"

	JPEGReadWriter2 putForm: self quality: -1 "default" progressiveJPEG: aBoolean onFileNamed: fileName

"
Display writeJPEGfileNamed: 'display.jpeg' progressive: false.
Form fromUser writeJPEGfileNamed: 'yourPatch.jpeg' progressive: true
"
</details>

#### Form>>#center

Note that offset is ignored here. Are we really going to embrace offset?


<details>
	<summary>See more</summary>
	
	center
	"Note that offset is ignored here.  Are we really going to embrace offset?  "
	^ (width @ height) // 2
</details>

#### Form>>#tallyPixelValues

Return a Bitmap with tallies in it of the number of pixels in this Form that have each pixel value. Note that several Forms may be tallied into the same table by tallyPixelValuesPrimitive:into: with the same table. Also Forms of depth 16 or 32 can be tallied into a tables of size 512, 4096, or 32768 entries by making a direct call with a Bitmap of the given size.


<details>
	<summary>See more</summary>
	
	tallyPixelValues
	"Return a Bitmap with tallies in it of the number of pixels in this Form that have each pixel value. Note that several Forms may be tallied into the same table by tallyPixelValuesPrimitive:into: with the same table. Also Forms of depth 16 or 32 can be tallied into a tables of size 512, 4096, or 32768 entries by making a direct call with a Bitmap of the given size."

	^ self tallyPixelValuesInRect: self boundingBox
		into: (Bitmap new: (1 bitShift: (self depth min: 15)))
"
Move a little rectangle around the screen and print its tallies...
 | r tallies nonZero |
Cursor blank showWhile: [
[Sensor isAnyButtonPressed] whileFalse:
	[r _ Sensor mousePoint extent: 10@10.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil.
	tallies _ (Display copy: r) tallyPixelValues.
	nonZero _ (1 to: tallies size) select: [:i | (tallies at: i) > 0]
			thenCollect: [:i | (tallies at: i) -> (i-1)].
	Display fill: (0@0 extent: Display width@20) fillColor: Color white.
	nonZero printString , '          ' displayAt: 0@0.
	Display border: (r expandBy: 2) width: 2 rule: Form reverse fillColor: nil]]
"
</details>

#### Form>>#magnify: aRectangle to: extent smoothing: cellSize

Answer a Form created as a scaling of the receiver. Scale may be a Float, and may be greater or less than 1.0.


<details>
	<summary>See more</summary>
	
	magnify: aRectangle to: extent smoothing: cellSize
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm _ Form extent: extent depth: depth.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) to: 300@200 smoothing: 2) display]
"
</details>

#### Form>>#hash

Hash is re-implemented because #= is re-implemented. But it is expensive. Storing (sub)instances of Form in Sets, or using them as keys in Dictionaries is usually not a good idea, because #= and #hash are expensive, and because Forms are usually mutable. Maybe use an IdentitySet or IdentityDictionary instead?


<details>
	<summary>See more</summary>
	
	hash
	"Hash is re-implemented because #= is re-implemented.
	But it is expensive.

	Storing (sub)instances of Form in Sets, or using them as keys in Dictionaries is 
	usually not a good idea, because #= and #hash are expensive, and because Forms
	are usually mutable.
	Maybe use an IdentitySet or IdentityDictionary instead?"

	^bits hash
</details>

#### Form>>#yTallyPixelValue: pv orNot: not

Return an array of the number of pixels with value pv by y-value. Note that if not is true, then this will tally those different from pv.


<details>
	<summary>See more</summary>
	
	yTallyPixelValue: pv orNot: not
	"Return an array of the number of pixels with value pv by y-value.
	Note that if not is true, then this will tally those different from pv."
	| cm slice copyBlt countBlt |
	cm _ self newColorMap.		"Map all colors but pv to zero"
	not ifTrue: [cm atAllPut: 1].		"... or all but pv to one"
	cm at: pv+1 put: 1 - (cm at: pv+1).
	slice _ Form extent: width@1.
	copyBlt _ (BitBlt destForm: slice sourceForm: self
				combinationRule: Form over
				destOrigin: `0@0` sourceOrigin: `0@0` extent: slice width @ 1
				clipRect: slice boundingBox)
					colorMap: cm.
	countBlt _ (BitBlt toForm: slice)
				fillColor: (Bitmap with: 0);
				destRect: (`0@0` extent: slice extent);
				combinationRule: 32.
	^ (0 to: height-1) collect: [ :y |
		copyBlt sourceOrigin: 0@y; copyBits.
		countBlt copyBits]
</details>

#### Form>>#border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone

Paint a border whose rectangular area is defined by aRectangle. The width of the border of each side is borderWidth. Uses aHalfTone for drawing the border.


<details>
	<summary>See more</summary>
	
	border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: combinationRule
		fillColor: aHalfTone
</details>

#### Form>>#contentsOfArea: aRect into: newForm

Return a new form which derives from the portion of the original form delineated by aRect.


<details>
	<summary>See more</summary>
	
	contentsOfArea: aRect into: newForm
 	"Return a new form which derives from the portion of the original form delineated by aRect."
	^ newForm copyBits: aRect from: self at: `0@0`
		clippingBox: newForm boundingBox rule: Form over
</details>

#### Form>>#reverse: aRectangle fillColor: aMask

Change all the bits in the receiver's area that intersects with aRectangle according to the mask. Black does not necessarily turn to white, rather it changes with respect to the rule and the bit in a corresponding mask location. Bound to give a surprise.


<details>
	<summary>See more</summary>
	
	reverse: aRectangle fillColor: aMask	
	"Change all the bits in the receiver's area that intersects with aRectangle 
	according to the mask. Black does not necessarily turn to white, rather it 
	changes with respect to the rule and the bit in a corresponding mask 
	location. Bound to give a surprise."

	self fill: aRectangle rule: Form reverse fillColor: aMask
</details>

#### Form>>#pixelValueAt: aPoint put: pixelValue

Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color.


<details>
	<summary>See more</summary>
	
	pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue.

</details>

#### Form>>#zoomOutTo: otherImage at: topLeft

Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40


<details>
	<summary>See more</summary>
	
	zoomOutTo: otherImage at: topLeft
	"Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40"
	^ self zoomIn: false orOutTo: otherImage at: topLeft
		vanishingPoint: otherImage extent//2+topLeft
</details>

#### Form>>#hackBits: bitThing

This method provides an initialization so that BitBlt may be used, eg, to copy ByteArrays and other non-pointer objects efficiently. The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high.


<details>
	<summary>See more</summary>
	
	hackBits: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high."
	width _ 4.
	depth _ 8.
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	bitThing class isBytes
		ifTrue: [height _ bitThing basicSize // 4]
		ifFalse: [height _ bitThing basicSize].
	bits _ bitThing
</details>

#### Form>>#rotatedByDegrees: deg

Rotate the receiver by the indicated number of degrees.


<details>
	<summary>See more</summary>
	
	rotatedByDegrees: deg
	"Rotate the receiver by the indicated number of degrees."
	"rot is the destination form, bit enough for any angle."

	^ self rotatedBy: deg smoothing: 1
"
 | a f |  f _ Form fromDisplay: (0@0 extent: 200@200).  a _ 0.
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
		rotatedByDegrees: (a _ a+0.1)) display].
f display
"
</details>

#### Form>>#wipeImage: otherImage at: topLeft delta: delta

Display wipeImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40 delta: 0@-2


<details>
	<summary>See more</summary>
	
	wipeImage: otherImage at: topLeft delta: delta
	"Display wipeImage: (Form fromDisplay: (40@40 extent: 300@300)) reverse
		at: 40@40 delta: 0@-2"

	self wipeImage: otherImage at: topLeft delta: delta clippingBox: nil.

</details>

#### Form>>#magnify: aRectangle by: scale smoothing: cellSize

Answer a Form created as a scaling of the receiver. Scale may be a Float, and may be greater or less than 1.0.


<details>
	<summary>See more</summary>
	
	magnify: aRectangle by: scale smoothing: cellSize
        "Answer a Form created as a scaling of the receiver.
        Scale may be a Float, and may be greater or less than 1.0."
        | newForm |
        newForm _ self blankCopyOf: aRectangle scaledBy: scale.
        (WarpBlt toForm: newForm)
                sourceForm: self;
                colorMap: (self colormapIfNeededFor: newForm);
                cellSize: cellSize;  "installs a new colormap if cellSize > 1"
                combinationRule: 3;
                copyQuad: aRectangle innerCorners toRect: newForm boundingBox.
        ^ newForm

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
        [(Display magnify: (Sensor mousePoint extent: 131@81) by: 0.5 smoothing: 2) display]
"
"Scaling test...
| f cp | f _ Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
        [cp _ Sensor mousePoint.
        (f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent smoothing: 2) display]]
"
</details>

#### Form>>#getCanvas

Return a Canvas that can be used to draw onto the receiver


<details>
	<summary>See more</summary>
	
	getCanvas
	"Return a Canvas that can be used to draw onto the receiver"
	^BitBltCanvas onForm: self
</details>

#### Form>>#setExtent: extent depth: bitsPerPixel bits: bitmap

Create a virtual bit map with the given extent and bitsPerPixel.


<details>
	<summary>See more</summary>
	
	setExtent: extent depth: bitsPerPixel bits: bitmap
	"Create a virtual bit map with the given extent and bitsPerPixel."
	width _ extent x asInteger.
	width < 0 ifTrue: [ width _ 0 ].
	height _ extent y asInteger.
	height < 0 ifTrue: [ height _ 0 ].
	depth _ bitsPerPixel.
	bits _ bitmap
</details>

#### Form>>#mapColor: oldColor to: newColor

Make all pixels of the given color in this Form to the given new color.


<details>
	<summary>See more</summary>
	
	mapColor: oldColor to: newColor
	"Make all pixels of the given color in this Form to the given new color."
	"Warnings: This method modifies the receiver. It may lose some color accuracy on 32-bit Forms, since the transformation uses a color map with only 15-bit resolution."

	| map |
	map _ (Color cachedColormapFrom: self depth to: self depth) copy.
	map at: (oldColor indexInMap: map) put: (newColor pixelWordForDepth: self depth).
	(BitBlt toForm: self)
		sourceForm: self;
		sourceOrigin: `0@0`;
		combinationRule: Form over;
		destX: 0 destY: 0 width: width height: height;
		colorMap: map;
		copyBits
</details>

#### Form>>#height

<details>
	<summary>See more</summary>
	
	height
	^ height
</details>

#### Form>>#readFrom: aBinaryStream

Reads the receiver from the given binary stream with the format: depth, extent, offset, bits.


<details>
	<summary>See more</summary>
	
	readFrom: aBinaryStream
	"Reads the receiver from the given binary stream with the format:
		depth, extent, offset, bits."
	self readAttributesFrom: aBinaryStream.
	self readBitsFrom: aBinaryStream
</details>

#### Form>>#storeOn: aStream base: anInteger

Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original.


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: anInteger 
	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream newLineTab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'depth: '.
	self depth printOn: aStream.
	aStream newLineTab: 1.
	aStream nextPutAll: 'fromArray: #('.
	self storeBitsOn:aStream base:anInteger.
	aStream nextPut: $).
	aStream newLineTab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $).

</details>

#### Form>>#magnify: aRectangle by: scale

Answer a Form created as a scaling of the receiver. Scale may be a Float, and may be greater or less than 1.0.


<details>
	<summary>See more</summary>
	
	magnify: aRectangle by: scale 
	"Answer a Form created as a scaling of the receiver.
	Scale may be a Float, and may be greater or less than 1.0."
	^ self magnify: aRectangle by: scale smoothing: 1

"Dynamic test...
[Sensor isAnyButtonPressed] whileFalse:
	[(Display magnify: (Sensor mousePoint extent: 31@41) by: 5@3) display]
"
"Scaling test...
| f cp | f _ Form fromDisplay: (Rectangle originFromUser: 100@100).
Display restoreAfter: [Sensor waitNoButton.
[Sensor isAnyButtonPressed] whileFalse:
	[cp _ Sensor mousePoint.
	(f magnify: f boundingBox by: (cp x asFloat@cp y asFloat)/f extent) display]]
"
"Consistency test...
 | f f2 p | [Sensor isAnyButtonPressed] whileFalse:
	[f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f magnify: f boundingBox by: 5@3.
	(f2 shrink: f2 boundingBox by: 5@3) displayAt: p]
"

</details>

#### Form>>#shrink: aRectangle by: scale

<details>
	<summary>See more</summary>
	
	shrink: aRectangle by: scale 
	| scalePt |
	scalePt _ scale asPoint.
	^ self magnify: aRectangle by: (1.0 / scalePt x asFloat) @ (1.0 / scalePt y asFloat)
</details>

#### Form>>#makeAllPixelsOpaqueBlack

Make all pixels opaque black. Useful for preparing 32bpp forms for later stuff. Set alpha values to 255 for all pixels. Set r, g, b values to zero for all pixels.


<details>
	<summary>See more</summary>
	
	makeAllPixelsOpaqueBlack
	"Make all pixels opaque black.
	Useful for preparing 32bpp forms for later stuff.
	Set alpha values to 255 for all pixels.
	Set r, g, b values to zero for all pixels."
	| bitBlt |
	bitBlt _ BitBlt toForm: self.
	bitBlt combinationRule: 3.
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits
</details>

#### Form>>#initFromArray: array

Fill the bitmap from array. If the array is shorter, then cycle around in its contents until the bitmap is filled.


<details>
	<summary>See more</summary>
	
	initFromArray: array
	"Fill the bitmap from array.  If the array is shorter,
	then cycle around in its contents until the bitmap is filled."
	| ax aSize array32 i j word16 |
	ax _ 0.
	aSize _ array size.
	aSize > bits size ifTrue:
		["backward compatibility with old 16-bit bitmaps and their forms"
		array32 _ Array new: height * (width + 31 // 32).
		i _ j _ 0.
		1 to: height do:
			[:y | 1 to: width+15//16 do:
				[:x16 | word16 _ array at: (i _ i + 1).
				x16 odd ifTrue: [array32 at: (j _ j+1) put: (word16 bitShift: 16)]
						ifFalse: [array32 at: j put: ((array32 at: j) bitOr: word16)]]].
		^ self initFromArray: array32].
	1 to: bits size do:
		[:index |
		(ax _ ax + 1) > aSize ifTrue: [ax _ 1].
		bits at: index put: (array at: ax)]
</details>

#### Form>>#asGrayForm

For lower bit depths, the 15 bit ColorMap loses no information, and answers the real #luminance of each pixel.


<details>
	<summary>See more</summary>
	
	asGrayForm
	| answer map |

	"For lower bit depths, the 15 bit ColorMap loses no information,
	and answers the real #luminance of each pixel."
	self depth < 32 ifTrue: [
		answer _ GrayForm extent: width@height.
		map _ self colormapIfNeededForGray8bpp.
		(BitBlt toForm: answer)
			colorMap: map;
			copy: self boundingBox
			from: `0@0` in: self
			fillColor: nil rule: Form over.
		answer offset: self offset.
		^ answer ].

	"For 32bpp, the approach below would use just 5bits per component.
	Generally it is best to extract a component in full 8bpp and keep full dynamic range.
	Green usually is a good choice."
	^ self asGrayForm: 3
</details>

#### Form>>#writeBMPfileNamed: fName

Display writeBMPfileNamed: 'display.bmp'


<details>
	<summary>See more</summary>
	
	writeBMPfileNamed: fName  "Display writeBMPfileNamed: 'display.bmp'"
	BMPReadWriter putForm: self onFileNamed: fName
</details>

#### Form>>#makeAllPixelsOpaque

Fix the alpha channel if the receiver is 32bit Set alpha values to 255 for all pixels. Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16). See #fixAlpha


<details>
	<summary>See more</summary>
	
	makeAllPixelsOpaque
	"Fix the alpha channel if the receiver is 32bit
	Set alpha values to 255 for all pixels.
	Note: assumes that a pixel with 0 is meant to be black, not transparent (As Squeak / Cuis usually handles rgb=0 for bpp<=16).
	See #fixAlpha"
	| bitBlt |
	self depth = 32 ifFalse: [ ^self ].
	bitBlt := BitBlt toForm: self.
	bitBlt combinationRule: 7. "bitOr:with:"
	bitBlt fillBitmap: (Bitmap with: 16rFF000000).
	bitBlt copyBits
</details>

#### Form>>#fillShape: aShapeForm fillColor: aColor at: location

Fill a region corresponding to 1 bits in aShapeForm with aColor


<details>
	<summary>See more</summary>
	
	fillShape: aShapeForm fillColor: aColor at: location
	"Fill a region corresponding to 1 bits in aShapeForm with aColor"

	((BitBlt destForm: self sourceForm: aShapeForm fillColor: aColor
		combinationRule: Form paint
		destOrigin: location + aShapeForm offset sourceOrigin: `0@0`
		extent: self extent clipRect: self boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF))
		copyBits
</details>

#### Form>>#colormapIfNeededFrom: sourceForm

Return a ColorMap mapping from sourceForm to the receiver.


<details>
	<summary>See more</summary>
	
	colormapIfNeededFrom: sourceForm
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForDepth: self depth
</details>

#### Form>>#fillWithColor: aColor

Fill the receiver's bounding box with the given color.


<details>
	<summary>See more</summary>
	
	fillWithColor: aColor
	"Fill the receiver's bounding box with the given color."

	self fill: self boundingBox fillColor: aColor
</details>

#### Form>>#swapEndianness

Swap from big to little endian pixels and vice versa


<details>
	<summary>See more</summary>
	
	swapEndianness
	"Swap from big to little endian pixels and vice versa"
	depth := 0 - depth.
</details>

## GrayForm

GrayForms can only have depth 8 or -8. Each pixel is a byte, and it specifies a gray level.

### Methods
#### GrayForm>>#colormapIfNeededForDepth: destDepth

Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.


<details>
	<summary>See more</summary>
	
	colormapIfNeededForDepth: destDepth
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed."

	^ Color cachedColormapFromGrayTo: destDepth
</details>

#### GrayForm>>#asFormOfDepth: d

<details>
	<summary>See more</summary>
	
	asFormOfDepth: d
	| answer |
	d = depth ifTrue: [ ^self ].
	"Same depth, but opposite endianness."
	d abs = depth abs ifTrue: [
		answer _ self copy.
		answer swapEndianness.
		BitBlt swapBytesIn32BitWords: answer bits.
		^ answer ].
	^ super asFormOfDepth: d
</details>

#### GrayForm>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #GrayForm or: [ super is: aSymbol ]
</details>

#### GrayForm>>#setExtent: extent depth: bitsPerPixel

Create a virtual bit map with the given extent and bitsPerPixel.


<details>
	<summary>See more</summary>
	
	setExtent: extent depth: bitsPerPixel
	"Create a virtual bit map with the given extent and bitsPerPixel."

	bitsPerPixel abs = 8 ifFalse: [self error: 'GrayForms only support depths of +/-8 bits'].
	super setExtent: extent depth: bitsPerPixel
</details>

#### GrayForm>>#pixelCompare: aRect with: otherForm at: otherLoc

Use RGB comparison. Divide by 3 because we have one sample per pixel, not 3.


<details>
	<summary>See more</summary>
	
	pixelCompare: aRect with: otherForm at: otherLoc
	"Use RGB comparison. Divide by 3 because we have one sample per pixel, not 3."
	^ ((self asFormOfDepth: 32) pixelCompare: aRect with: (otherForm asFormOfDepth: 32) at: otherLoc) / 3
</details>

#### GrayForm>>#grayAt: aPoint put: aGrayColor

Store the given color into the pixel at aPoint. Convert to gray level.


<details>
	<summary>See more</summary>
	
	grayAt: aPoint put: aGrayColor
	"Store the given color into the pixel at aPoint.
	Convert to gray level."

	self pixelValueAt: aPoint put: (aGrayColor luminance * 255) rounded

</details>

#### GrayForm>>#mapColor: oldColor to: newColor

Replace all occurances of the given color with the given new color in my color map.


<details>
	<summary>See more</summary>
	
	mapColor: oldColor to: newColor
	"Replace all occurances of the given color with the given new color in my color map."

	^self error: 'Unsupported for GrayForms'
</details>

#### GrayForm>>#colorAt: aPoint put: aColor

Store the given color into the pixel at aPoint. Convert to gray level.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint put: aColor
	"Store the given color into the pixel at aPoint. Convert to gray level."

	self grayAt: aPoint put: aColor
</details>

#### GrayForm>>#colorAt: aPoint

Return the color of the pixel at aPoint.


<details>
	<summary>See more</summary>
	
	colorAt: aPoint
	"Return the color of the pixel at aPoint."

	^ self grayAt: aPoint
</details>

#### GrayForm>>#bitPatternFor: aColor

Return the pixel word for representing the given color on the receiver


<details>
	<summary>See more</summary>
	
	bitPatternFor: aColor
	"Return the pixel word for representing the given color on the receiver"
	^aColor bitPatternForGrayForm
</details>

#### GrayForm>>#asCursorForm

<details>
	<summary>See more</summary>
	
	asCursorForm

	^self error: 'Unsupported for GrayForms'
</details>

#### GrayForm>>#grayAt: aPoint

Return the color of the pixel at aPoint.


<details>
	<summary>See more</summary>
	
	grayAt: aPoint
	"Return the color of the pixel at aPoint."

	^Color gray: (self pixelValueAt: aPoint) asFloat / 255.0
</details>

#### GrayForm>>#asGrayForm

For lower bit depths, the 15 bit ColorMap loses no information, and answers the real #luminance of each pixel.


<details>
	<summary>See more</summary>
	
	asGrayForm

	^ self
</details>

#### GrayForm>>#isTransparentAt: aPoint

Return true if the receiver is transparent at the given point.


<details>
	<summary>See more</summary>
	
	isTransparentAt: aPoint 
	"Return true if the receiver is transparent at the given point."

	^ false
</details>

#### GrayForm>>#colormapIfNeededForGray8bpp

Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed. We are already Gray 8 bpp. No colormap needed.


<details>
	<summary>See more</summary>
	
	colormapIfNeededForGray8bpp
	"Return a colormap for displaying the receiver at the given depth, or nil if no colormap is needed.
	We are already Gray 8 bpp. No colormap needed."

	^nil
</details>

#### GrayForm>>#colormapIfNeededFrom: sourceForm

Return a ColorMap mapping from sourceForm to the receiver.


<details>
	<summary>See more</summary>
	
	colormapIfNeededFrom: sourceForm
	"Return a ColorMap mapping from sourceForm to the receiver."
	^sourceForm colormapIfNeededForGray8bpp
</details>

#### GrayForm>>#maskingMap

Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero.


<details>
	<summary>See more</summary>
	
	maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."

	^Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF
</details>

#### GrayForm>>#mightBeTranslucent

Answer whether this form may be translucent


<details>
	<summary>See more</summary>
	
	mightBeTranslucent
	"Answer whether this form may be translucent"
	^false
</details>

#### GrayForm>>#copy: aRect

Return a new instance containing the portion of the receiver delineated by aRect.


<details>
	<summary>See more</summary>
	
	copy: aRect
 	"Return a new instance containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm _ self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		combinationRule: Form over
		destOrigin: `0@0`
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	^ newForm
</details>

