## AffineTransformation

This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. The direct transformation is equivalent as multiplying the matrix by a column vector (with an extra element of value 1). The inverse transformation is multiplying the inverse of the matrix by a column vector (with an extra element of value 1). By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: . Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive. We can consider the matrix to be of 3x3 with an extra row with #( 0 0 1) at the bottom, especially for having a properly defined inverse matrix. Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol. Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore: - Don't assume the receiver will be modified. Always take the result. - Don't assume the answer will be a new instance. The receiver might be modified! (make a copy if needed)

### Methods
#### AffineTransformation>>#a13

<details>
	<summary>See more</summary>
	
	a13
	^self at: 3
</details>

#### AffineTransformation>>#isTranslation: aPoint

The position of 0@0 in the external coordinate system. It is the translation we apply when transforming points.


<details>
	<summary>See more</summary>
	
	isTranslation: aPoint
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^self a13 = aPoint x and: [ self a23 = aPoint y ]
</details>

#### AffineTransformation>>#at: index put: value

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
</details>

#### AffineTransformation>>#inverseTransform: aPoint

Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint. Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion.


<details>
	<summary>See more</summary>
	
	inverseTransform: aPoint
	"Apply the inverse transformation to aPoint, i.e. multiply our inverse by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	| x y det a11 a12 a21 a22 detX detY |

	x _ aPoint x - self a13.
	y _ aPoint y - self a23.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)
</details>

#### AffineTransformation>>#a21

<details>
	<summary>See more</summary>
	
	a21
	 ^self at: 4
</details>

#### AffineTransformation>>#printOn: aStream

Note: Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. degrees: '.
	self degrees printOn: aStream.
	aStream nextPutAll: '. translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '
</details>

#### AffineTransformation>>#a11: value

<details>
	<summary>See more</summary>
	
	a11: value
	 self at: 1 put: value
</details>

#### AffineTransformation>>#a11

<details>
	<summary>See more</summary>
	
	a11
	^self at: 1
</details>

#### AffineTransformation>>#a12

<details>
	<summary>See more</summary>
	
	a12
	^self at: 2
</details>

#### AffineTransformation>>#rotatedBy: radians

rotate the receiver by radians angle. Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11.
	a12 _ self a12.
	a13 _ self a13.
	a21 _ self a21.
	a22 _ self a22.
	a23 _ self a23.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^ self
</details>

#### AffineTransformation>>#externalizeScalar: aNumber

Externalize a distance (without a direction). Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions).


<details>
	<summary>See more</summary>
	
	externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scale
</details>

#### AffineTransformation>>#a12: value

<details>
	<summary>See more</summary>
	
	a12: value
	 self at: 2 put: value
</details>

#### AffineTransformation>>#doesNotRotate

Answer true if #radians would answer 0.0. Performance optimization.


<details>
	<summary>See more</summary>
	
	doesNotRotate
	"Answer true if #radians would answer 0.0. Performance optimization."

	^self a21 = 0.0 and: [ self a11 sign >= 0 ]
</details>

#### AffineTransformation>>#displayBoundsOfTransformOf: aRectangle

Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds). Primitive rounds and answers integers. Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here.


<details>
	<summary>See more</summary>
	
	displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Primitive rounds and answers integers.
	Warning: if answer from primitive is not strictly positive, it is off by one. Fix it here."

	| dstRect |
	dstRect _ Rectangle new.
	(self primDisplayBoundsOfTransformOf: aRectangle into: dstRect) ifNotNil: [
		dstRect topLeft > `0@0` ifTrue: [ ^dstRect  ]].
	^Rectangle encompassing: (aRectangle corners collect: [ :pt |
		(self transform: pt) rounded ])
</details>

#### AffineTransformation>>#setPointScale: scalePoint

Set the raw scale in x and y in the receiver. No rotation. private


<details>
	<summary>See more</summary>
	
	setPointScale: scalePoint
	"Set the raw scale in x and y in the receiver. No rotation. private"

	self a11: scalePoint x.
	self a12: 0.
	self a21: 0.
	self a22: scalePoint y
</details>

#### AffineTransformation>>#initialize

Initialize the receiver to the identity transformation (e.g., not affecting points)


<details>
	<summary>See more</summary>
	
	initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	self
		a11: 1.0;
		a22: 1.0
</details>

#### AffineTransformation>>#scaledByNumber: aNumber rotatedBy: radians

rotate the receiver by radians angle. Also scale by aNumber. Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions. This means that there is no difference between scaling then rotating and rotating then scaling. Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| s c a11 a12 a13 a21 a22 a23|
	s _ radians sin.
	c _ radians cos.
	a11 _ self a11 * aNumber.
	a12 _ self a12 * aNumber.
	a13 _ self a13 * aNumber.
	a21 _ self a21 * aNumber.
	a22 _ self a22 * aNumber.
	a23 _ self a23 * aNumber.
	self a11: (c * a11) - (s * a21).
	self a12: (c * a12) - (s * a22).
	self a13: (c * a13) - (s * a23).
	self a21: (s * a11) + (c * a21).
	self a22: (s * a12) + (c * a22).
	self a23: (s * a13) + (c * a23).
	^self
</details>

#### AffineTransformation>>#scaledBy: aPoint

Multiply by a scale. Argument can be a point, applying different scaling in x and in y directions. Keep the transformed position of 0@0, i.e. don't change offset. Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt sx sy |
	pt _ aPoint asPoint.
	sx _ pt x.
	sy _ pt y.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.
	^ self
</details>

#### AffineTransformation>>#isPureTranslation

Return true if the receiver specifies no rotation or scaling.


<details>
	<summary>See more</summary>
	
	isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	<primitive: 'primitiveIsPureTranslation' module: 'Matrix2x3Plugin'>
	^self a11 = 1.0 and:[self a12 = 0.0 and:[self a22 = 0.0 and:[self a21 = 1.0]]]
</details>

#### AffineTransformation>>#withTranslation: aPoint

set an offset in the receiver Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or sch) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y.
	^ self
</details>

#### AffineTransformation>>#matrixPrintString

(AffineTransformation withRadians: -3 scale: 12 position: 4.5@3) matrixPrintString


<details>
	<summary>See more</summary>
	
	matrixPrintString
	"
	(AffineTransformation withRadians: -3 scale: 12 position: 4.5@3) matrixPrintString
	"
	^String streamContents: [ :strm | self printMatrixOn: strm ]
</details>

#### AffineTransformation>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self basicSize * self bytesPerBasicElement
</details>

#### AffineTransformation>>#a21: value

<details>
	<summary>See more</summary>
	
	a21: value
	 self at: 4 put: value
</details>

#### AffineTransformation>>#setTranslation: aPoint

private. Set the raw offset in the receiver


<details>
	<summary>See more</summary>
	
	setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y
</details>

#### AffineTransformation>>#innerComposedWithTranslation: aMorphicTranslation

Return the composition of the receiver and the transformation passed in. We know the class of the argument (through double dispatching)


<details>
	<summary>See more</summary>
	
	innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^(AffineTransformation withTranslation: aMorphicTranslation translation) composedWith: self
</details>

#### AffineTransformation>>#withYAxisNegated

Swap inneer point Y sign. Make y increment upwards. This makes the any matrix transform from standard mathematical coordinates to standard display coordinates (in addition to the transform it was already doing) Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12  negated.
	self a22: self a22  negated
</details>

#### AffineTransformation>>#primDisplayBoundsOfTransformOf: srcRect into: dstRect

Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds). Store result into dstRect.


<details>
	<summary>See more</summary>
	
	primDisplayBoundsOfTransformOf: srcRect into: dstRect
	"Externalize srcRect, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).
	Store result into dstRect."

	"Warning: the answer is rounded to integers by the primitive
	Warning: if answer is not strictly positive, it is off by one. Evaluate:

	AffineTransformation new primDisplayBoundsOfTransformOf: (-2@ 2 extent: 10@10) into: Rectangle new
	AffineTransformation new primDisplayBoundsOfTransformOf: (-12@ 12 extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfTransformOf: (-4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: -4)  primDisplayBoundsOfTransformOf: (2@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 2)  primDisplayBoundsOfTransformOf: (-14@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfTransformOf: (-12@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: -4)  primDisplayBoundsOfTransformOf: (12@ 2  extent: 10@10) into: Rectangle new
	
	These are Ok (answer is positive)
	(AffineTransformation withTranslation: -2)  primDisplayBoundsOfTransformOf: (4@ 2  extent: 10@10) into: Rectangle new
	(AffineTransformation withTranslation: 4)  primDisplayBoundsOfTransformOf: (-2@ 2  extent: 10@10) into: Rectangle new
	"
	<primitive: 'primitiveTransformRectInto' module: 'Matrix2x3Plugin'>

	^nil
</details>

#### AffineTransformation>>#writeOn: aStream

<details>
	<summary>See more</summary>
	
	writeOn: aStream
	aStream nextWordsPutAll: self.
</details>

#### AffineTransformation>>#externalizeDelta: aPoint

Externalize a distance vector. A distance is not a position. It is a magnitude with a direction. It is usually used as a delta to be added to a position to obtain some other position.


<details>
	<summary>See more</summary>
	
	externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12).
	y _ (aPoint x * self a21) + (aPoint y * self a22).
	^x @ y
</details>

#### AffineTransformation>>#a22: value

<details>
	<summary>See more</summary>
	
	a22: value
	 self at: 5 put: value
</details>

#### AffineTransformation>>#customizeExplorerContents

<details>
	<summary>See more</summary>
	
	customizeExplorerContents

	^ true
</details>

#### AffineTransformation>>#isIdentity

Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself.


<details>
	<summary>See more</summary>
	
	isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]
</details>

#### AffineTransformation>>#bytesPerBasicElement

Answer the number of bytes that each of my basic elements requires. In other words: self basicSize * self bytesPerBasicElement should equal the space required on disk by my variable sized representation.


<details>
	<summary>See more</summary>
	
	bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]
</details>

#### AffineTransformation>>#a22

<details>
	<summary>See more</summary>
	
	a22
	 ^self at: 5
</details>

#### AffineTransformation>>#transform: aPoint

Apply the direct transformation to aPoint, i.e. multiply self by aPoint. Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion.


<details>
	<summary>See more</summary>
	
	transform: aPoint
	"Apply the direct transformation to aPoint, i.e. multiply self by aPoint.
	Use Smalltalk code, and not Matrix2x3Plugin, because we want Float conversion."
	"
	Faster approach: Keep 2 instances, to be able to call prim.
	Cons: who cares? This is jost for M3, that inlines most calls... Besides, immediate floats makes this bogus.
t _ AffineTransformation withRadians: -3 scale: 12 position: 4.5@3.
[ 100000 timesRepeat: [ t transform: 2@3 ]] timeToRun.
[ 100000 timesRepeat: [ 2 ]] timeToRun.
z1 _ AffineTransformation withTranslation: 2@3.
z2 _ AffineTransformation new.
[ 100000 timesRepeat: [ t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
[ 100000 timesRepeat: [ z1 _ AffineTransformation withTranslation: 2@3.z2 _ AffineTransformation new.t composedWith: z1 into: z2. z2 a13@z2 a23 ]] timeToRun.
"

	| x y |
	x _ (aPoint x * self a11) + (aPoint y * self a12) + self a13.
	y _ (aPoint x * self a21) + (aPoint y * self a22) + self a23.
	^x @ y
</details>

#### AffineTransformation>>#translation

Translation and position are the same. Use the word translation when thinking about coordinate transformation, but use the word position when thinking about morph locations


<details>
	<summary>See more</summary>
	
	translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^self a13 @ self a23
</details>

#### AffineTransformation>>#internalizeDelta: aPoint

Internalize a distance vector. A distance is not a position. It is a magnitude with a direction. It is usually used as a delta to be added to a position to obtain some other position.


<details>
	<summary>See more</summary>
	
	internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position."

	| x y det a11 a12 a21 a22 detX detY |
	x _ aPoint x.
	y _ aPoint y.
	a11 _ self a11.
	a12 _ self a12.
	a21 _ self a21.
	a22 _ self a22.
	det _ (a11 * a22) - (a12 * a21).
	det = 0.0 ifTrue: [ ^`0@0` ].		"So we have at least a valid result"
	det _ 1.0 / det.
	detX _ (x * a22) - (a12 * y).
	detY _ (a11 * y) - (x * a21).
	^ (detX * det) @ (detY * det)
</details>

#### AffineTransformation>>#innerComposedWithAffineTransformation: anAffineTransformation

Return the composition of the receiver and the transformation passed in. We know the class of the argument (through double dispatching)


<details>
	<summary>See more</summary>
	
	innerComposedWithAffineTransformation: anAffineTransformation
	"Return the composition of the receiver and the transformation passed in.
	 We know the class of the argument (through double dispatching)"

	^ anAffineTransformation composedWith: self into: self class new
</details>

#### AffineTransformation>>#composedWith: aTransformation into: result

Return the composition of the receiver and the transformation passed in. Store the composed matrix into result. Please see the comment at: #composedWith:


<details>
	<summary>See more</summary>
	
	composedWith: aTransformation into: result
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	| a11 a12 a13 a21 a22 a23 b11 b12 b13 b21 b22 b23 matrix |
	<primitive: 'primitiveComposeMatrix' module: 'Matrix2x3Plugin'>
	matrix := aTransformation.
	a11 := self a11.		b11 := matrix a11.
	a12 := self a12.		b12 := matrix a12.
	a13 := self a13.		b13 := matrix a13.
	a21 := self a21.		b21 := matrix a21.
	a22 := self a22.		b22 := matrix a22.
	a23 := self a23.		b23 := matrix a23.
	result a11: (a11 * b11) + (a12 * b21).
	result a12: (a11 * b12) + (a12 * b22).
	result a13: a13 + (a11 * b13) + (a12 * b23).
	result a21: (a21 * b11) + (a22 * b21).
	result a22: (a21 * b12) + (a22 * b22).
	result a23: a23 + (a21 * b13) + (a22 * b23).
	^result
</details>

#### AffineTransformation>>#= anAffineTransformation

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= anAffineTransformation 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self == anAffineTransformation ifTrue: [ ^ true ].
	self class == anAffineTransformation class ifFalse: [^ false].
	length := self size.
	length = anAffineTransformation size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (anAffineTransformation at: i) ifFalse: [^ false]].
	^ true
</details>

#### AffineTransformation>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	| result |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF
</details>

#### AffineTransformation>>#radians

Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self a21 arcTan: self a11
</details>

#### AffineTransformation>>#externalizePosition: aPoint

Answer coordinates for aPoint in the space we are in. aPoint is expressed in us.


<details>
	<summary>See more</summary>
	
	externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint
</details>

#### AffineTransformation>>#inspectorClass

Answer the class of the inspector to be used on the receiver. Called by inspect; use basicInspect to get a normal (less useful) type of inspector.


<details>
	<summary>See more</summary>
	
	inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector
</details>

#### AffineTransformation>>#explorerContents

<details>
	<summary>See more</summary>
	
	explorerContents

	^{
		ObjectExplorerWrapper
			with: self scale
			name: 'scale'
			model: self. 
		ObjectExplorerWrapper
			with: self degrees
			name: 'degrees'
			model: self. 
		ObjectExplorerWrapper
			with: self translation
			name: 'translation'
			model: self }
</details>

#### AffineTransformation>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
</details>

#### AffineTransformation>>#print

<details>
	<summary>See more</summary>
	
	print
	self printOn: Transcript.
	Transcript newLine
</details>

#### AffineTransformation>>#italizing

a little shear Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	italizing
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 + (self scale*0.2).
	^self
</details>

#### AffineTransformation>>#setRadians: radians scale: scaleNumber

Set the raw rotation angle and scale in the receiver. private


<details>
	<summary>See more</summary>
	
	setRadians: radians scale: scaleNumber
	"Set the raw rotation angle and scale in the receiver. private"

	| s c |
	s _ radians sin * scaleNumber.
	c _ radians cos * scaleNumber.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c
</details>

#### AffineTransformation>>#withRotation: radians scale: scale

Set rotation and scaling according to parameters. Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already an AffineTransformation.


<details>
	<summary>See more</summary>
	
	withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already an AffineTransformation."

	self setRadians: radians scale: scale.
	^self
</details>

#### AffineTransformation>>#composedWith: innerTransformation

Return the composition of the receiver and the transformation passed in. The result is a translation that has the following effect: self externalize: (innerTransformation externalize: aPoint) innerTransformation internalize: (self internalize: aPoint).


<details>
	<summary>See more</summary>
	
	composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a translation  that has the following effect:
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

 	^innerTransformation innerComposedWithAffineTransformation: self
</details>

#### AffineTransformation>>#scale

Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^(self a11 squared + self a21 squared) sqrt
</details>

#### AffineTransformation>>#a23

<details>
	<summary>See more</summary>
	
	a23
	 ^self at: 6
</details>

#### AffineTransformation>>#inverseTransformation

Return the inverse transformation of the receiver. The inverse transformation is computed by first calculating the inverse offset and then computing transformations for the two identity vectors (1@0) and (0@1)


<details>
	<summary>See more</summary>
	
	inverseTransformation
	"Return the inverse transformation of the receiver.
	The inverse transformation is computed by first calculating
	the inverse offset and then computing transformations
	for the two identity vectors (1@0) and (0@1)"
	| r1 r2 r3 m |
	r3 _ self inverseTransform: `0@0`.
	r1 _ (self inverseTransform: `1@0`) - r3.
	r2 _ (self inverseTransform: `0@1`) - r3.
	m _ self species new.
	m
		a11: r1 x; a12: r2 x; a13: r3 x;
		a21: r1 y; a22: r2 y; a23: r3 y.
	^ m
</details>

#### AffineTransformation>>#a23: value

<details>
	<summary>See more</summary>
	
	a23: value
	 self at: 6 put: value
</details>

#### AffineTransformation>>#printMatrixOn: aStream

<details>
	<summary>See more</summary>
	
	printMatrixOn: aStream
	aStream
		newLine;
		nextPutAll: '| '.
	self a11 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a12 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a13 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream nextPutAll: ' |'.

	aStream
		newLine;
		nextPutAll: '| '.
	self a21 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a22 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a23 printOn: aStream integerDigits: 1 fractionDigits: 3.

	aStream
		nextPutAll: ' |';
		newLine
</details>

#### AffineTransformation>>#internalizePosition: aPoint

Answer our coordinates for aPoint. aPoint is expressed in the space we are in.


<details>
	<summary>See more</summary>
	
	internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint
</details>

#### AffineTransformation>>#internalizeScalar: aNumber

Internalize a distance (without a direction). Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions).


<details>
	<summary>See more</summary>
	
	internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scale
</details>

#### AffineTransformation>>#degrees

Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	degrees
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self radians radiansToDegrees
</details>

#### AffineTransformation>>#a13: value

<details>
	<summary>See more</summary>
	
	a13: value
	 self at: 3 put: value
</details>

#### AffineTransformation>>#italizing2

a little shear Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	italizing2
	"a little shear
	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	self a12: self a12 - (self scale*0.2).
	self setTranslation: (self scale*0.2)@0 + self translation.
	^self
</details>

#### AffineTransformation>>#restoreEndianness

This word object was just read in from a stream. It was stored in Big Endian (Mac) format. Swap each pair of bytes (16-bit word), if the current machine is Little Endian. Why is this the right thing to do? We are using memory as a byteStream. High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory. Different from a Bitmap.


<details>
	<summary>See more</summary>
	
	restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	| w b1 b2 b3 b4 |
	Smalltalk  isLittleEndian ifTrue: [
		1 to: self basicSize do: [:i |
			w := self basicAt: i.
			b1 := w digitAt: 1.
			b2 := w digitAt: 2.
			b3 := w digitAt: 3.
			b4 := w digitAt: 4.
			w := (b1 << 24) + (b2 << 16) + (b3 << 8) + b4.
			self basicAt: i put: w.
		]
	].


</details>

#### AffineTransformation>>#translatedBy: aPoint

add an offset in the receiver Answer the modified object. In this implementation is self, but some classes of transformations, more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	translatedBy: aPoint
	"add an offset in the receiver

	Answer the modified object. In this implementation is self, but some classes of transformations,
	more restricted ones (like MorphicTranslation) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	self a13: self a13 + pt x.
	self a23: self a23 + pt y.
	^ self
</details>

## BitBlt

WARNING: BitBlt's shape cannot be modified since WarpBlt relies on the exact layout. Important primitives will break if you fail to heed this warning. I represent a block transfer (BLT) of pixels from one Form ( the sourceForm) into a rectangle (destX, destY, width, height) of the destinationForm, as modified by a combination rule, a possible halftoneForm and a possible color map. The source of pixels may be a similar rectangle (at sourceX, sourceY) in the sourceForm, or the halftoneForm, or both. If both are specified, their pixel values are combined by a logical AND function prior to any further combination rule processing. The halftoneForm may be an actual Form or a simple WordArray of 32 bit values usually intended to represent Color values. In either case the 'top' of the form is effectively aligned with the top of the destinationForm and for each scanline the destination y modulo the size of the halftoneForm gives the index of the word to use. This makes it easy to make horizontal stripes, for example. In any case, the pixels from the source (AND'd with the halftone, remember) are combined with those of the destination by as specified by the combinationRules below- name rule result 0 always 0 and 1 src AND dst 2 src AND not(dst) over 3 src only erase 4 not(src) AND dst 5 dst only reverse 6 src XOR dst under 7 src OR dst 8 not(src) AND not(dst) 9 not(src) XOR dst 10 not(dst) 11 src OR not(dst) 12 not(src) 13 not(src) OR dst 14 not(src) OR not(dst) 15 always 1 (You can find an interesting explanation of how this comes to be in http://dev-docs.atariforge.org/files/BLiTTER_1-25-1990.pdf - which interestingly fails to mention any connection to Smalltalk and PARC.) Forms may be of different depths, see the comment in class Form. In addition to the original 16 combination rules invented for monochrome Forms, this BitBlt supports 16 fails (to simulate paint bits) 17 fails (to simulate erase bits) 18 sourceWord + destinationWord 19 sourceWord - destinationWord 20 rgbAdd: sourceWord with: destinationWord. Sum of color components 21 rgbSub: sourceWord with: destinationWord. Difference of color components 22 OLDrgbDiff: sourceWord with: destinationWord. Sum of abs of differences in components 23 OLDtallyIntoMap: destinationWord. Tallies pixValues into a colorMap these old versions don't do bitwise dest clipping. Use 32 and 33 now. blend 24 alphaBlend: sourceWord with: destinationWord. 32-bit source and dest only. Blend sourceWord with destinationWord, assuming both are 32-bit pixels. The source is assumed to have 255*alpha in the high 8 bits of each pixel, while the high 8 bits of the destinationWord will be ignored. The blend produced is alpha*source + (1-alpha)*dest, with the computation being performed independently on each color component. The high byte of the result will be 0. paint 25 pixPaint: sourceWord with: destinationWord. Wherever the sourceForm is non-zero, it replaces the destination. Can be used with a 1-bit source color mapped to (0, FFFFFFFF), and a fillColor to fill the dest with that color wherever the source is 1. erase1BitShape 26 pixMask: sourceWord with: destinationWord. Like pixPaint, but fills with 0. 27 rgbMax: sourceWord with: destinationWord. Max of each color component. 28 rgbMin: sourceWord with: destinationWord. Min of each color component. 29 rgbMin: sourceWord bitInvert32 with: destinationWord. Min with (max-source) blendAlpha 30 alphaBlendConst: sourceWord with: destinationWord. alpha is an arg. works in 16 bits. Blend sourceWord with destinationWord using a constant alpha. Alpha is encoded as 0 meaning 0.0, and 255 meaning 1.0. The blend produced is alpha*source + (1.0-alpha)*dest, with the computation being performed independently on each color component. paintAlpha 31 alphaPaintConst: sourceWord with: destinationWord. alpha is an arg. works in 16 bits. 32 rgbDiff: sourceWord with: destinationWord. Sum of abs of differences in components 33 tallyIntoMap: destinationWord. Tallies pixValues into a colorMap - Those tallied are exactly those in the destination rectangle. Note that the source should be specified == destination, in order for the proper color map checks be performed at setup. blendAlphaScaled 34 alphaBlendScaled: srcWord with: dstWord. Alpha blend of scaled srcWord and destWord. In contrast to alphaBlend:with: the color produced is srcColor + (1-srcAlpha) * dstColor 35 & 36 not used rgbMul 37 rgbMul: srcWord with: dstWord. 38 pixSwap: srcWord with: dstWord. 39 pixClear: srcWord with: dstWord. Clear all pixels in destinationWord for which the pixels of sourceWord have the same values. Used to clear areas of some constant color to zero. 40 fixAlpha: srcWord with: dstWord. For any non-zero pixel value in destinationWord with zero alpha channel take the alpha from sourceWord and fill it in. Intended for fixing alpha channels left at zero during 16->32 bpp conversions. 41 rgbComponentAlpha: srcWord with: dstWord. Any transfer specified is further clipped by the specified clipping rectangle (clipX, clipY, clipWidth, clipHeight), and also by the bounds of the source and destination forms. To make a small Form repeat and fill a big form, use an InfiniteForm as the source. Pixels copied from a source to a destination whose pixels have a different depth are converted based on the optional colorMap. If colorMap is nil, then conversion to more bits is done by filling the new high-order bits with zero, and conversion to fewer bits is done by truncating the lost high-order bits. The colorMap, if specified, must be a either word array (ie Bitmap) with 2^n elements, where n is the pixel depth of the source, or a fully specified ColorMap which may contain a lookup table (ie Bitmap) and/or four separate masks and shifts which are applied to the pixels. For every source pixel, BitBlt will first perform masking and shifting and then index the lookup table, and select the corresponding pixelValue and mask it to the destination pixel size before storing. When blitting from a 32 or 16 bit deep Form to one 8 bits or less, the default is truncation. This will produce very strange colors, since truncation of the high bits does not produce the nearest encoded color. Supply a 512 long colorMap, and red, green, and blue will be shifted down to 3 bits each, and mapped. The message copybits...stdColors will use the best map to the standard colors for destinations of depths 8, 4, 2 and 1. Two other sized of colorMaps are allowed, 4096 (4 bits per color) and 32786 (five bits per color). Normal blits between 16 and 32 bit forms truncates or pads the colors automatically to provide the best preservation of colors. Colors can be remapped at the same depth. Sometimes a Form is in terms of colors that are not the standard colors for this depth, for example in a GIF file. Convert the Form to a MaskedForm and send colorMap: the list of colors that the picture is in terms of. (Note also that a Form can be copied to itself, and transformed in the process, if a non-nil colorMap is supplied.)

### Methods
#### BitBlt>>#colorMap: map

See last part of BitBlt comment. 6/18/96 tk


<details>
	<summary>See more</summary>
	
	colorMap: map
	"See last part of BitBlt comment. 6/18/96 tk"
	colorMap _ map.
</details>

#### BitBlt>>#sourceForm: aForm

Set the receiver's source form to be the argument, aForm.


<details>
	<summary>See more</summary>
	
	sourceForm: aForm 
	"Set the receiver's source form to be the argument, aForm."

	sourceForm _ aForm
</details>

#### BitBlt>>#roundVariables

<details>
	<summary>See more</summary>
	
	roundVariables

	| maxVal minVal |
	maxVal _ SmallInteger maxVal.
	minVal _ SmallInteger minVal.
	destX _ destX asInteger min: maxVal max: minVal.
	destY _ destY asInteger min: maxVal max: minVal.
	width _ width asInteger min: maxVal max: minVal.
	height _ height asInteger min: maxVal max: minVal.
	sourceX _ sourceX asInteger min: maxVal max: minVal.
	sourceY _ sourceY asInteger min: maxVal max: minVal.
	clipX _ clipX asInteger min: maxVal max: minVal.
	clipY _ clipY asInteger min: maxVal max: minVal.
	clipWidth _ clipWidth asInteger min: maxVal max: minVal.
	clipHeight _ clipHeight asInteger min: maxVal max: minVal.

</details>

#### BitBlt>>#setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin

Set up a BitBlt for copying a Rectangle as large as possible, with sourceOrigin and destOrigin. Bound by sourceForm and destForm extents. No additional clipping, colorMap or fillColor. sourceOrigin and destOrigin are 0-based points | blitter sf sourceOrigin df destOrigin | sf _ FloatImage lena asForm. sourceOrigin _ 30@30. df _ Form extent: 156@156 depth: 32. df fillColor: Color red. destOrigin _ 10@20. blitter _ BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin. blitter copyBits. df display


<details>
	<summary>See more</summary>
	
	setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin
	"Set up a BitBlt for copying a Rectangle as large as possible, with sourceOrigin and destOrigin. Bound by sourceForm and destForm extents. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf _ FloatImage lena asForm.
	sourceOrigin _ 30@30.
	df _ Form extent: 156@156 depth: 32.
	df fillColor: Color red.
	destOrigin _ 10@20.
	blitter _ BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin.
	blitter copyBits.
	df display
	"
	self
		setDestForm: df sourceForm: sf
		destOrigin: destOrigin sourceOrigin: sourceOrigin
		extent: (sf width - sourceOrigin x) @ (sf height - sourceOrigin y)
</details>

#### BitBlt>>#height: anInteger

Set the receiver's destination form height to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	height: anInteger 
	"Set the receiver's destination form height to be the argument, anInteger."

	height _ anInteger
</details>

#### BitBlt>>#clipRect

Answer the receiver's clipping area rectangle.


<details>
	<summary>See more</summary>
	
	clipRect
	"Answer the receiver's clipping area rectangle."

	^clipX @ clipY extent: clipWidth @ clipHeight
</details>

#### BitBlt>>#sourceY: anInteger

Set the receiver's source form top left y to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	sourceY: anInteger 
	"Set the receiver's source form top left y to be the argument, anInteger."

	sourceY _ anInteger
</details>

#### BitBlt>>#sourceForm

<details>
	<summary>See more</summary>
	
	sourceForm

	^ sourceForm
</details>

#### BitBlt>>#fillColor: aColor

The destForm will be filled with this color or pattern of colors. May be an old Color, a new type Color, a Bitmap (see BitBlt comment), a Pattern, or a Form. 6/18/96 tk


<details>
	<summary>See more</summary>
	
	fillColor: aColor 
	"The destForm will be filled with this color or pattern of colors.  May be an old Color, a new type Color, a Bitmap (see BitBlt comment), a Pattern, or a Form.  6/18/96 tk"

	aColor ifNil: [halftoneForm _ nil. ^ self].
	destForm ifNil: [self error: 'Must set destForm first'].
	halftoneForm _ destForm bitPatternFor: aColor 
</details>

#### BitBlt>>#copyForm: srcForm to: destPt rule: rule

<details>
	<summary>See more</summary>
	
	copyForm: srcForm to: destPt rule: rule
	^ self copyForm: srcForm to: destPt rule: rule
		colorMap: (srcForm colormapIfNeededFor: destForm)
</details>

#### BitBlt>>#pixelAt: aPoint put: pixelValue

Assumes this BitBlt has been set up specially (see the init message, BitBlt bitPokerToForm:. Overwrites the pixel at aPoint.


<details>
	<summary>See more</summary>
	
	pixelAt: aPoint put: pixelValue
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPokerToForm:.  Overwrites the pixel at aPoint."
	destX _ aPoint x.
	destY _ aPoint y.
	sourceForm bits at: 1 put: pixelValue.
	self copyBits
"
| bb |
bb _ (BitBlt bitPokerToForm: Display).
[Sensor isAnyButtonPressed] whileFalse:
	[bb pixelAt: Sensor mousePoint put: 55. Display forceToScreen]
"
</details>

#### BitBlt>>#combinationRule: anInteger

Set the receiver's combination rule to be the argument, anInteger, a number in the range 0-15.


<details>
	<summary>See more</summary>
	
	combinationRule: anInteger 
	"Set the receiver's combination rule to be the argument, anInteger, a 
	number in the range 0-15."

	combinationRule _ anInteger
</details>

#### BitBlt>>#sourceOrigin: aPoint

Set the receiver's source form coordinates to be those of the argument, aPoint.


<details>
	<summary>See more</summary>
	
	sourceOrigin: aPoint 
	"Set the receiver's source form coordinates to be those of the argument, 
	aPoint."

	sourceX _ aPoint x.
	sourceY _ aPoint y
</details>

#### BitBlt>>#setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent

Set up a BitBlt for copying a Rectangle of extent w@h , with sourceOrigin and destOrigin. No additional clipping, colorMap or fillColor. sourceOrigin and destOrigin are 0-based points | blitter sf sourceOrigin df destOrigin | sf _ FloatImage lena asForm. sourceOrigin _ 30@30. sourceOrigin _ 0@0. df _ Form extent: 256@256 depth: 32. df fillColor: Color red. destOrigin _ 0@0. blitter _ BitBlt new setSourceForm: sf sourceOrigin: sourceOrigin destForm: df destOrigin: destOrigin width: 256 height: 256. blitter copyBits. df display


<details>
	<summary>See more</summary>
	
	setDestForm: df sourceForm: sf destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent
	"Set up a BitBlt for copying a Rectangle of extent w@h , with sourceOrigin and destOrigin. No additional clipping, colorMap or fillColor.

	sourceOrigin and destOrigin are 0-based points

	| blitter sf sourceOrigin df destOrigin |
	sf _ FloatImage lena asForm.
	sourceOrigin _ 30@30.
	sourceOrigin _ 0@0.
	df _ Form extent: 256@256 depth: 32.
	df fillColor: Color red.
	destOrigin _ 0@0.
	blitter _ BitBlt new
		setSourceForm: sf sourceOrigin: sourceOrigin
		destForm: df destOrigin: destOrigin
		width: 256 height: 256.
	blitter copyBits.
	df display
	"
	self setDestForm: df.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceForm _ sf.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ extent x.
	height _ extent y.
	halftoneForm _ nil.
	combinationRule _ 3. 	"Form over"
	colorMap _ nil
</details>

#### BitBlt>>#clipRange

clip and adjust source origin and extent appropriately


<details>
	<summary>See more</summary>
	
	clipRange
	"clip and adjust source origin and extent appropriately"
	"first in x"
	| sx sy dx dy bbW bbH |
	"fill in the lazy state if needed"
	destX ifNil:[destX := 0].
	destY ifNil:[destY := 0].
	width ifNil:[width := destForm width].
	height ifNil:[height := destForm height].
	sourceX ifNil:[sourceX := 0].
	sourceY ifNil:[sourceY := 0].
	clipX ifNil:[clipX := 0].
	clipY ifNil:[clipY := 0].
	clipWidth ifNil:[clipWidth := destForm width].
	clipHeight ifNil:[clipHeight := destForm height].

	destX >= clipX
		ifTrue: [sx _ sourceX.
				dx _ destX.
				bbW _ width]
		ifFalse: [sx _ sourceX + (clipX - destX).
				bbW _ width - (clipX - destX).
				dx _ clipX].
	(dx + bbW) > (clipX + clipWidth)
		ifTrue: [bbW _ bbW - ((dx + bbW) - (clipX + clipWidth))].
	"then in y"
	destY >= clipY
		ifTrue: [sy _ sourceY.
				dy _ destY.
				bbH _ height]
		ifFalse: [sy _ sourceY + clipY - destY.
				bbH _ height - (clipY - destY).
				dy _ clipY].
	(dy + bbH) > (clipY + clipHeight)
		ifTrue: [bbH _ bbH - ((dy + bbH) - (clipY + clipHeight))].
	sourceForm ifNotNil:[
		sx < 0
			ifTrue: [dx _ dx - sx.
					bbW _ bbW + sx.
					sx _ 0].
		sx + bbW > sourceForm width
			ifTrue: [bbW _ bbW - (sx + bbW - sourceForm width)].
		sy < 0
			ifTrue: [dy _ dy - sy.
					bbH _ bbH + sy.
					sy _ 0].
		sy + bbH > sourceForm height
			ifTrue: [bbH _ bbH - (sy + bbH - sourceForm height)].
	].
	(bbW <= 0 or:[bbH <= 0]) ifTrue:[
		sourceX := sourceY := destX := destY := clipX := clipY := width := height := 0.
		^true].
	(sx = sourceX 
		and:[sy = sourceY 
		and:[dx = destX 
		and:[dy = destY 
		and:[bbW = width 
		and:[bbH = height]]]]]) ifTrue:[^false].
	sourceX := sx.
	sourceY := sy.
	destX := dx.
	destY := dy.
	width := bbW.
	height := bbH.
	^true
</details>

#### BitBlt>>#drawLoopX: xDelta Y: yDelta

Primitive. Implements the Bresenham plotting algorithm (IBM Systems Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and maintains a potential, P. When P's sign changes, it is time to move in the minor direction as well. This particular version does not write the first and last points, so that these can be called for as needed in client code. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	drawLoopX: xDelta Y: yDelta 
	"Primitive. Implements the Bresenham plotting algorithm (IBM Systems
	Journal, Vol. 4 No. 1, 1965). It chooses a principal direction, and
	maintains a potential, P. When P's sign changes, it is time to move in
	the minor direction as well. This particular version does not write the
	first and last points, so that these can be called for as needed in client code.
	Optional. See Object documentation whatIsAPrimitive."
	| dx dy px py P |
	<primitive: 'primitiveDrawLoop' module: 'BitBltPlugin'>
	dx _ xDelta sign.
	dy _ yDelta sign.
	px _ yDelta abs.
	py _ xDelta abs.
	"self copyBits."
	py > px
		ifTrue: 
			["more horizontal"
			P _ py // 2.
			1 to: py do: 
				[:i |
				destX _ destX + dx.
				(P _ P - px) < 0 ifTrue: 
						[destY _ destY + dy.
						P _ P + py].
				i < py ifTrue: [self copyBits]]]
		ifFalse: 
			["more vertical"
			P _ px // 2.
			1 to: px do:
				[:i |
				destY _ destY + dy.
				(P _ P - py) < 0 ifTrue: 
						[destX _ destX + dx.
						P _ P + px].
				i < px ifTrue: [self copyBits]]]
</details>

#### BitBlt>>#width: anInteger

Set the receiver's destination form width to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	width: anInteger 
	"Set the receiver's destination form width to be the argument, anInteger."

	width _ anInteger
</details>

#### BitBlt>>#primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta

<details>
	<summary>See more</summary>
	
	primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta
	| ascii glyph |
	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>
	startIndex to: stopIndex do:[:charIndex|
		ascii _ (aString at: charIndex) numericValue.
		glyph _ glyphMap at: ascii + 1.
		sourceX _ xTable at: glyph + 1.
		width _ (xTable at: glyph + 2) - sourceX.
		self copyBits.
		destX _ destX + width + kernDelta.
	].
</details>

#### BitBlt>>#pixelAt: aPoint

Assumes this BitBlt has been set up specially (see the init message, BitBlt bitPeekerFromForm:. Returns the pixel at aPoint.


<details>
	<summary>See more</summary>
	
	pixelAt: aPoint
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPeekerFromForm:.  Returns the pixel at aPoint."
	sourceX _ aPoint x.
	sourceY _ aPoint y.
	destForm bits at: 1 put: 0.  "Just to be sure"
	self copyBits.
	^ destForm bits at: 1
</details>

#### BitBlt>>#copyFrom: sourceRectangle in: srcForm to: destPt

<details>
	<summary>See more</summary>
	
	copyFrom: sourceRectangle in: srcForm to: destPt
	| sourceOrigin |
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ 3.  "store"
	destX _ destPt x.
	destY _ destPt y.
	sourceOrigin _ sourceRectangle origin.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ sourceRectangle width.
	height _ sourceRectangle height.
	colorMap _ srcForm colormapIfNeededFor: destForm.
	self copyBits
</details>

#### BitBlt>>#oldPaintBits

Perform the old paint operation, which requires two calls to BitBlt.


<details>
	<summary>See more</summary>
	
	oldPaintBits
	"Perform the old paint operation, which requires two calls to BitBlt."
	| requestedFill oldMap requestedRule |
	sourceForm depth = 1 ifFalse: [
		^ self halt: 'paint operation is only defined for 1-bit deep sourceForms'].

	requestedRule _ combinationRule.
	requestedFill _ halftoneForm.
	oldMap _ colorMap.

	halftoneForm _ nil.
	colorMap _ Bitmap with: 0 with: 16rFFFFFFFF.	"Map 1's to ALL ones, not just one"
	combinationRule _ Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"

	halftoneForm _ requestedFill.
	combinationRule _ Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap _ oldMap.
	combinationRule _ requestedRule

" | dot |
dot _ Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor mousePoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"
</details>

#### BitBlt>>#destRect: aRectangle

Set the receiver's destination form top left coordinates to be the origin of the argument, aRectangle, and set the width and height of the receiver's destination form to be the width and height of aRectangle.


<details>
	<summary>See more</summary>
	
	destRect: aRectangle 
	"Set the receiver's destination form top left coordinates to be the origin of 
	the argument, aRectangle, and set the width and height of the receiver's 
	destination form to be the width and height of aRectangle."

	destX _ aRectangle left.
	destY _ aRectangle top.
	width _ aRectangle width.
	height _ aRectangle height
</details>

#### BitBlt>>#copy: destRectangle from: sourcePt in: srcForm

<details>
	<summary>See more</summary>
	
	copy: destRectangle from: sourcePt in: srcForm
	| destOrigin |
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ 3.  "store"
	destOrigin _ destRectangle origin.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceX _ sourcePt x.
	sourceY _ sourcePt y.
	width _ destRectangle width.
	height _ destRectangle height.
	self copyBits
</details>

#### BitBlt>>#colorMap

<details>
	<summary>See more</summary>
	
	colorMap
	^ colorMap
</details>

#### BitBlt>>#drawFrom: startPoint to: stopPoint withFirstPoint: drawFirstPoint

Draw a line whose end points are startPoint and stopPoint. The line is formed by repeatedly calling copyBits at every point along the line. If drawFirstPoint is false, then omit the first point so as not to overstrike at line junctions.


<details>
	<summary>See more</summary>
	
	drawFrom: startPoint to: stopPoint withFirstPoint: drawFirstPoint
	"Draw a line whose end points are startPoint and stopPoint.
	The line is formed by repeatedly calling copyBits at every
	point along the line.  If drawFirstPoint is false, then omit
	the first point so as not to overstrike at line junctions."
	| offset point1 point2 forwards |
	"Always draw down, or at least left-to-right"
	forwards _ (startPoint y = stopPoint y and: [startPoint x < stopPoint x])
				or: [startPoint y < stopPoint y].
	forwards
		ifTrue: [point1 _ startPoint. point2 _ stopPoint]
		ifFalse: [point1 _ stopPoint. point2 _ startPoint].
	sourceForm
		ifNil: [
			destX := point1 x.
			destY := point1 y]
		ifNotNil: [
			width := sourceForm width.
			height := sourceForm height.
			offset := sourceForm offset.
			destX := (point1 x + offset x) rounded.
			destY := (point1 y + offset y) rounded].

	"Note that if not forwards, then the first point is the last and vice versa.
	We agree to always paint stopPoint, and to optionally paint startPoint."
	(drawFirstPoint or: [forwards == false  "ie this is stopPoint"])
		ifTrue: [self copyBits].
	self drawLoopX: (point2 x - point1 x) rounded 
				  Y: (point2 y - point1 y) rounded.
	(drawFirstPoint or: [forwards  "ie this is stopPoint"])
		ifTrue: [self copyBits].

</details>

#### BitBlt>>#getPluginName

Private. Return the name of the plugin representing BitBlt. Used for dynamically switching between different BB representations only.


<details>
	<summary>See more</summary>
	
	getPluginName
	"Private. Return the name of the plugin representing BitBlt.
	Used for dynamically switching between different BB representations only."
	^'BitBltPlugin'
</details>

#### BitBlt>>#setDestForm: df

<details>
	<summary>See more</summary>
	
	setDestForm: df

	destForm _ df.
	clipX _ 0.
	clipY _ 0.
	clipWidth _ df width.
	clipHeight _ df height
</details>

#### BitBlt>>#drawFrom: startPoint to: stopPoint

<details>
	<summary>See more</summary>
	
	drawFrom: startPoint to: stopPoint 
	
	 ^ self drawFrom: startPoint to: stopPoint withFirstPoint: true
</details>

#### BitBlt>>#copyBitsTranslucent: factor

This entry point to BitBlt supplies an extra argument to specify translucency for operations 30 and 31. The argument must be an integer between 0 and 255.


<details>
	<summary>See more</summary>
	
	copyBitsTranslucent: factor
	"This entry point to BitBlt supplies an extra argument to specify translucency
	for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	self primitiveFailed  "Later do nicer error recovery -- share copyBits recovery"
</details>

#### BitBlt>>#setDestForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect

<details>
	<summary>See more</summary>
	
	setDestForm: df sourceForm: sf fillColor: fc combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect

	| aPoint |
	destForm _ df.
	sourceForm _ sf.
	self fillColor: fc.
	combinationRule _ cr.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceX _ sourceOrigin x.
	sourceY _ sourceOrigin y.
	width _ extent x.
	height _ extent y.
	aPoint _ clipRect origin.
	clipX _ aPoint x.
	clipY _ aPoint y.
	aPoint _ clipRect corner.
	clipWidth _ aPoint x - clipX.
	clipHeight _ aPoint y - clipY.
	sourceForm ifNotNil: [
		colorMap _ sourceForm colormapIfNeededFor: destForm]
</details>

#### BitBlt>>#clipRect: aRectangle

Set the receiver's clipping area rectangle to be the argument, aRectangle.


<details>
	<summary>See more</summary>
	
	clipRect: aRectangle 
	"Set the receiver's clipping area rectangle to be the argument, aRectangle."

	clipX _ aRectangle left truncated.
	clipY _ aRectangle top truncated.
	clipWidth _ aRectangle right truncated - clipX.
	clipHeight _ aRectangle bottom truncated - clipY.
</details>

#### BitBlt>>#destOrigin: aPoint

Set the receiver's destination top left coordinates to be those of the argument, aPoint.


<details>
	<summary>See more</summary>
	
	destOrigin: aPoint 
	"Set the receiver's destination top left coordinates to be those of the 
	argument, aPoint."

	destX _ aPoint x.
	destY _ aPoint y
</details>

#### BitBlt>>#copy: destRectangle from: sourcePt in: srcForm fillColor: hf rule: rule

Specify a Color to fill, not a Form. 6/18/96 tk


<details>
	<summary>See more</summary>
	
	copy: destRectangle from: sourcePt in: srcForm fillColor: hf rule: rule
	"Specify a Color to fill, not a Form. 6/18/96 tk"  
	| destOrigin |
	sourceForm _ srcForm.
	self fillColor: hf.	"sets halftoneForm"
	combinationRule _ rule.
	destOrigin _ destRectangle origin.
	destX _ destOrigin x.
	destY _ destOrigin y.
	sourceX _ sourcePt x.
	sourceY _ sourcePt y.
	width _ destRectangle width.
	height _ destRectangle height.
	srcForm
		ifNotNil: [colorMap := srcForm colormapIfNeededFor: destForm].
	^ self copyBits
</details>

#### BitBlt>>#fillBitmap: aBitmap

Sets the receivers half tone form. See class commment.


<details>
	<summary>See more</summary>
	
	fillBitmap: aBitmap
	"Sets the receivers half tone form. See class commment."
	
	halftoneForm _ aBitmap
</details>

#### BitBlt>>#destY: anInteger

Set the top left y coordinate of the receiver's destination form to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	destY: anInteger 
	"Set the top left y coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destY _ anInteger
</details>

#### BitBlt>>#copyForm: srcForm to: destPt rule: rule fillColor: color

<details>
	<summary>See more</summary>
	
	copyForm: srcForm to: destPt rule: rule fillColor: color
	sourceForm _ srcForm.
	self fillColor: color.	"sets halftoneForm"
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	self copyBits
</details>

#### BitBlt>>#sourceX: anInteger

Set the receiver's source form top left x to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	sourceX: anInteger 
	"Set the receiver's source form top left x to be the argument, anInteger."

	sourceX _ anInteger
</details>

#### BitBlt>>#destX: x destY: y width: w height: h

<details>
	<summary>See more</summary>
	
	destX: x destY: y width: w height: h
	destX _ x.
	destY _ y.
	width _ w.
	height _ h.
</details>

#### BitBlt>>#sourceRect: aRectangle

Set the receiver's source form top left x and y, width and height to be the top left coordinate and extent of the argument, aRectangle.


<details>
	<summary>See more</summary>
	
	sourceRect: aRectangle 
	"Set the receiver's source form top left x and y, width and height to be 
	the top left coordinate and extent of the argument, aRectangle."

	sourceX _ aRectangle left.
	sourceY _ aRectangle top.
	width _ aRectangle width.
	height _ aRectangle height
</details>

#### BitBlt>>#destX: anInteger

Set the top left x coordinate of the receiver's destination form to be the argument, anInteger.


<details>
	<summary>See more</summary>
	
	destX: anInteger 
	"Set the top left x coordinate of the receiver's destination form to be the 
	argument, anInteger."

	destX _ anInteger
</details>

#### BitBlt>>#destForm

<details>
	<summary>See more</summary>
	
	destForm
	^ destForm
</details>

#### BitBlt>>#fillColor

Return the current fill color as a Color. Gives the wrong answer if the halftoneForm is a complex pattern of more than one word.


<details>
	<summary>See more</summary>
	
	fillColor
	"Return the current fill color as a Color.  
	 Gives the wrong answer if the halftoneForm is a complex pattern of more than one word."

	halftoneForm ifNil: [^ `Color black` ].
	^ Color colorFromPixelValue: halftoneForm first depth: destForm depth
</details>

#### BitBlt>>#clipBy: aRectangle

<details>
	<summary>See more</summary>
	
	clipBy: aRectangle
	| aPoint right bottom |
	right _ clipX + clipWidth.
	bottom _ clipY + clipHeight.
	aPoint _ aRectangle origin.
	aPoint x > clipX ifTrue:[clipX _ aPoint x].
	aPoint y > clipY ifTrue:[clipY _ aPoint y].
	aPoint _ aRectangle corner.
	aPoint x < right ifTrue:[right _ aPoint x].
	aPoint y < bottom ifTrue:[bottom _ aPoint y].
	clipWidth _ right - clipX.
	clipHeight _ bottom - clipY.
	clipWidth < 0 ifTrue:[clipWidth _ 0].
	clipHeight < 0 ifTrue:[clipHeight _ 0].
</details>

#### BitBlt>>#copyBitsAgain

Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	copyBitsAgain
	"Primitive. See BitBlt|copyBits, also a Primitive. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	self primitiveFailed
</details>

#### BitBlt>>#copyForm: srcForm to: destPt rule: rule colorMap: map

<details>
	<summary>See more</summary>
	
	copyForm: srcForm to: destPt rule: rule colorMap: map
	sourceForm _ srcForm.
	halftoneForm _ nil.		"No fillColor. Just srcForm"
	combinationRule _ rule.
	destX _ destPt x + sourceForm offset x.
	destY _ destPt y + sourceForm offset y.
	sourceX _ 0.
	sourceY _ 0.
	width _ sourceForm width.
	height _ sourceForm height.
	colorMap _ map.
	self copyBits
</details>

#### BitBlt>>#fill: destRect fillColor: grayForm rule: rule

Fill with a Color, not a Form. 6/18/96 tk


<details>
	<summary>See more</summary>
	
	fill: destRect fillColor: grayForm rule: rule
	"Fill with a Color, not a Form. 6/18/96 tk"
	sourceForm _ nil.
	self fillColor: grayForm.		"sets halftoneForm"
	combinationRule _ rule.
	destX _ destRect left.
	destY _ destRect top.
	sourceX _ 0.
	sourceY _ 0.
	width _ destRect width.
	height _ destRect height.
	self copyBits
</details>

#### BitBlt>>#copyBits

Primitive. Perform the movement of bits from the source form to the destination form. Fail if any variables are not of the right type (Integer, Float, or Form) or if the combination rule is not implemented. In addition to the original 16 combination rules, this BitBlt supports 16 fail (to simulate paint) 17 fail (to simulate mask) 18 sourceWord + destinationWord 19 sourceWord - destinationWord 20 rgbAdd: sourceWord with: destinationWord 21 rgbSub: sourceWord with: destinationWord 22 rgbDiff: sourceWord with: destinationWord 23 tallyIntoMap: destinationWord 24 alphaBlend: sourceWord with: destinationWord 25 pixPaint: sourceWord with: destinationWord 26 pixMask: sourceWord with: destinationWord 27 rgbMax: sourceWord with: destinationWord 28 rgbMin: sourceWord with: destinationWord 29 rgbMin: sourceWord bitInvert32 with: destinationWord


<details>
	<summary>See more</summary>
	
	copyBits
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>

	"No alpha specified -- re-run with alpha = 1.0"
	(combinationRule >= 30 and: [combinationRule <= 31]) ifTrue: [
		^ self copyBitsTranslucent: 255].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [^ self oldPaintBits].
	combinationRule = Form oldErase1bitShape ifTrue: [^ self oldErase1bitShapeBits].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and:[colorMap isColormap]) ifTrue:[
		colorMap _ colorMap colors.
		^self copyBits].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue:[self roundVariables. ^self copyBitsAgain].

	self error: 'Bad BitBlt arg (Fraction?); proceed to convert.'.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain
</details>

#### BitBlt>>#clipByX1: x1 y1: y1 x2: x2 y2: y2

<details>
	<summary>See more</summary>
	
	clipByX1: x1 y1: y1 x2: x2 y2: y2
	| right bottom |
	right _ clipX + clipWidth.
	bottom _ clipY + clipHeight.
	x1 > clipX ifTrue:[clipX _ x1].
	y1 > clipY ifTrue:[clipY _ y1].
	x2 < right ifTrue:[right _ x2].
	y2 < bottom ifTrue:[bottom _ y2].
	clipWidth _ right - clipX.
	clipHeight _ bottom - clipY.
	clipWidth < 0 ifTrue:[clipWidth _ 0].
	clipHeight < 0 ifTrue:[clipHeight _ 0].
</details>

#### BitBlt>>#oldErase1bitShapeBits

Perform the erase operation, which puts 0's in the destination wherever the source (which is assumed to be just 1 bit deep) has a 1. This requires the colorMap to be set in order to AND all 1's into the destFrom pixels regardless of their size.


<details>
	<summary>See more</summary>
	
	oldErase1bitShapeBits
	"Perform the erase operation, which puts 0's in the destination
	wherever the source (which is assumed to be just 1 bit deep)
	has a 1.  This requires the colorMap to be set in order to AND
	all 1's into the destFrom pixels regardless of their size."
	| oldFillBitmap oldMap |
	oldFillBitmap _ halftoneForm.
	halftoneForm _ nil.
	oldMap _ colorMap.
	colorMap _ Bitmap with: 0 with: 16rFFFFFFFF.
	combinationRule _ Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"
	halftoneForm _ oldFillBitmap.
	colorMap _ oldMap
</details>

## Bitmap

My instances provide contiguous storage of bits, primarily to hold the graphical data of Forms. Forms and their subclasses provide the additional structural information as to how the bits should be interpreted in two dimensions.

### Methods
#### Bitmap>>#pixelValueForDepth: depth

Self is being used to represent a single color. Answer bits that appear in ONE pixel of this color in a Bitmap of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Returns an integer. First pixel only.


<details>
	<summary>See more</summary>
	
	pixelValueForDepth: depth
	"Self is being used to represent a single color.  Answer bits that appear in ONE pixel of this color in a Bitmap of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32.  Returns an integer.  First pixel only.  "

	^ (self at: 1) bitAnd: (1 bitShift: depth) - 1
</details>

#### Bitmap>>#bitPatternForDepth: depth

The raw call on BitBlt needs a Bitmap to represent this color. I already am Bitmap like. I am already adjusted for a specific depth. Interpret me as an array of (32/depth) Color pixelValues. BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary. 6/18/96 tk


<details>
	<summary>See more</summary>
	
	bitPatternForDepth: depth
	"The raw call on BitBlt needs a Bitmap to represent this color.  I already am Bitmap like.  I am already adjusted for a specific depth.  Interpret me as an array of (32/depth) Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary. 6/18/96 tk"

	^ self
</details>

#### Bitmap>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self size * 4
</details>

#### Bitmap>>#byteAt: byteAddress bigEndian: isBigEndian

Extract a byte from a Bitmap. Note that this is a byte address and it is one-order. For repeated use, create an instance of BitBlt and use pixelAt:. See Form pixelAt: 7/1/96 tk


<details>
	<summary>See more</summary>
	
	byteAt: byteAddress bigEndian: isBigEndian
	"Extract a byte from a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:.  See Form pixelAt:  7/1/96 tk"
	| lowBits shift |
	lowBits _ byteAddress - 1 bitAnd: 3.
	shift _ isBigEndian 
		ifTrue: [ (lowBits - 3) * 8 ]
		ifFalse: [ (0 - lowBits) * 8 ].
	^((self at: byteAddress - 1 - lowBits // 4 + 1)
		bitShift: shift * 8)
		bitAnd: 16rFF
</details>

#### Bitmap>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self printNameOn: aStream.
	aStream nextPutAll: ' of length '; print: self size
</details>

#### Bitmap>>#atAllPut: value

Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays.


<details>
	<summary>See more</summary>
	
	atAllPut: value
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."
	<primitive: 145>
	super atAllPut: value.
</details>

#### Bitmap>>#defaultElement

Return the default element of the receiver


<details>
	<summary>See more</summary>
	
	defaultElement
	"Return the default element of the receiver"
	^0
</details>

#### Bitmap>>#bytesAt: index

Answer a ByteArray of 4 elements. The 32-bit word is split in 4 bytes, in little endian format WordArray with: 16rFF32791B :: bytesAt: 1 :: hex


<details>
	<summary>See more</summary>
	
	bytesAt: index
	"Answer a ByteArray of 4 elements.
	The 32-bit word is split in 4 bytes, in little endian format
	WordArray with: 16rFF32791B :: bytesAt: 1 :: hex
	"

	| bytes word |
	bytes _ ByteArray new: 4.
	word _ self at: index. "Usually a SmallInteger, but may be a Large Integer in 32-bit images"
	1 to: 4 do: [ :i | bytes at: i put: (word digitAt: i) ].
	^ bytes
</details>

#### Bitmap>>#primFill: aPositiveInteger

Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays.


<details>
	<summary>See more</summary>
	
	primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.
</details>

#### Bitmap>>#integerAt: index

Return the integer at the given index


<details>
	<summary>See more</summary>
	
	integerAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word _ self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]
</details>

#### Bitmap>>#integerAt: index put: anInteger

Store the integer at the given index


<details>
	<summary>See more</summary>
	
	integerAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self  basicAt: index put: word.
	^anInteger
</details>

#### Bitmap>>#byteAt: byteAddress put: byte bigEndian: isBigEndian

Insert a byte into a Bitmap. Note that this is a byte address and it is one-order. For repeated use, create an instance of BitBlt and use pixelAt:put:. See Form pixelAt:put: 7/1/96 tk


<details>
	<summary>See more</summary>
	
	byteAt: byteAddress put: byte bigEndian: isBigEndian
	"Insert a byte into a Bitmap.  Note that this is a byte address and it is one-order.  For repeated use, create an instance of BitBlt and use pixelAt:put:.  See Form pixelAt:put:  7/1/96 tk"
	| longWord shift lowBits longAddr |
	(byte < 0 or:[byte > 255]) ifTrue:[^self errorImproperStore].
	lowBits _ byteAddress - 1 bitAnd: 3.
	longWord _ self at: (longAddr _ (byteAddress - 1 - lowBits) // 4 + 1).
	shift _ isBigEndian 
		ifTrue: [ (3 - lowBits) * 8 ]
		ifFalse: [ (lowBits) * 8 ].
	longWord _ longWord - (longWord bitAnd: (16rFF bitShift: shift)) 
		+ (byte bitShift: shift).
	self at: longAddr put: longWord.
	^ byte
</details>

#### Bitmap>>#isColormap

Bitmaps were used as color maps for BitBlt. This method allows to recognize real color maps.


<details>
	<summary>See more</summary>
	
	isColormap
	"Bitmaps were used as color maps for BitBlt.
	This method allows to recognize real color maps."
	^false
</details>

#### Bitmap>>#bytesAt: index put: aByteArray

Takes a ByteArray of 4 elements. Store the 32-bit word made with those byes, in little endian format WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex


<details>
	<summary>See more</summary>
	
	bytesAt: index put: aByteArray
	"Takes a ByteArray of 4 elements.
	Store the 32-bit word made with those byes, in little endian format
	WordArray new: 1 :: bytesAt: 1 put: #[16r1B 16r79 16r32 16rFF] :: first hex
	"

	| word |
	word _ 0.
	4 to: 1 by: -1 do: [ :i | word _ word * 256 + (aByteArray at: i) ].
	self at: index put: word
</details>

#### Bitmap>>#asByteArray

Faster way to make a byte array from me. copyFromByteArray:, if receiver is BigEndian makes equal Bitmap. Assume receiver bytes-in-word mapping is BigEndian: Most significant bye of first word in self goes to first position in result. This means that for a BigEndian 8bpp Form, pixels are in the right order in the ByteArray Form lena asGrayForm bits asByteArray copyFrom: 1 to: 4. (Form lena asGrayForm asFormOfDepth: 8) bits asByteArray copyFrom: 1 to: 4. (0 to: 3) collect: [ :x | ((Form lena asGrayForm colorAt: x@0) luminance * 255) rounded ].


<details>
	<summary>See more</summary>
	
	asByteArray
	"Faster way to make a byte array from me.
	copyFromByteArray:, if receiver is BigEndian makes equal Bitmap.
	Assume receiver bytes-in-word mapping is BigEndian:
		Most significant bye of first word in self goes to first position in result.
	This means that for a BigEndian 8bpp Form, pixels are in the right order in the ByteArray
	
	Form lena asGrayForm bits asByteArray copyFrom: 1 to: 4.
	(Form lena asGrayForm asFormOfDepth: 8) bits asByteArray copyFrom: 1 to: 4.
	(0 to: 3) collect: [ :x | ((Form lena asGrayForm colorAt: x@0) luminance * 255) rounded ].
	"
	| f bytes hack |
	f _ Form extent: 4@self size depth: 8 bits: self.
	bytes _ ByteArray new: self size * 4.
	hack _ Form new hackBits: bytes.
	Smalltalk isLittleEndian ifTrue: [hack swapEndianness].
	hack copyBits: f boundingBox
		from: f
		at: `0@0`
		clippingBox: hack boundingBox
		rule: Form over.

	"f displayOn: hack."
	^ bytes
</details>

#### Bitmap>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

## Color

This class represents abstract color, regardless of the depth of bitmap it will be shown in. At the very last moment a Color is converted to a pixelValue that depends on the depth of the actual Bitmap inside the Form it will be used with. The supported depths (in bits) are 1, 2, 4, 8, 16, and 32. The number of actual colors at these depths are: 2, 4, 16, 256, 32768, and 16 million. (See comment in BitBlt.) To change the depth of the Display and set how many colors you can see, execute: (Display newDepth: 8). (See comment in DisplayMedium) Color is represented as the amount of light in red, green, and blue. White is (1.0, 1.0, 1.0) and black is (0, 0, 0). Pure red is (1.0, 0, 0). These colors are "additive". Think of Color's instance variables as: r amount of red, a Float between 0.0 and 1.0. g amount of green, a Float between 0.0 and 1.0. b amount of blue, a Float between 0.0 and 1.0. Many colors are named. You find a color by name by sending a message to class Color, for example (Color lightBlue). Also, (Color red: 0.2 green: 0.6 blue: 1.0) or (Color r: 0.2 g: 0.6 b: 1.0) creates a color. (see below) A color is essentially immutable. Once you set red, green, and blue, you cannot change them. Instead, create a new Color and use it. Applications such as contour maps and bar graphs will want to display one of a set of shades based on a number. Convert the range of this number to an integer from 1 to N. Then call (Color green lightShades: N) to get an Array of colors from white to green. Use the Array messages at:, atPin:, or atWrap: to pull out the correct color from the array. atPin: gives the first (or last) color if the index is out of range. atWrap: wraps around to the other end if the index is out of range. Here are some fun things to run in when your screen has color: Pen new mandala: 30 diameter: Display height-100. Pen new web "Draw with the mouse, opt-click to end" Display fillWhite. Pen new hilberts: 5. Form toothpaste: 30 "Draw with mouse, opt-click to end" Messages: mixed: proportion with: aColor Answer this color mixed with the given color additively. The proportion, a number between 0.0 and 1.0, determines what what fraction of the receiver to use in the mix. + add two colors - subtract two colors * multiply the values of r, g, b by a number or an Array of factors. ((Color named: #white) * 0.3) gives a darkish gray. (aColor * #(0 0 0.9)) gives a color with slightly less blue. / divide a color by a factor or an array of three factors. hue Returns the hue of the color. On a wheel from 0 to 360 with pure red at 0 and again at 360. saturation Returns the saturation of the color. 0.0 to 1.0 brightness Returns the brightness of the color. 0.0 to 1.0 name Look to see if this Color has a name. display Show a swatch of this color tracking the cursor. lightShades: thisMany An array of thisMany colors from white to the receiver. darkShades: thisMany An array of thisMany colors from black to the receiver. Array is of length num. mix: color2 shades: thisMany An array of thisMany colors from the receiver to color2. wheel: thisMany An array of thisMany colors around the color wheel starting and ending at the receiver. pixelValueForDepth: d Returns the bits that appear be in a Bitmap of this depth for this color. Represents the nearest available color at this depth. Normal users do not need to know which pixelValue is used for which color. Messages to Class Color. red: r green: g blue: b Return a color with the given r, g, and b components. r: g: b: Same as above, for fast typing. hue: h saturation: s brightness: b Create a color with the given hue, saturation, and brightness. pink blue red ... Many colors have messages that return an instance of Color. canUnderstand: #brown Returns true if #brown is a defined color. names An OrderedCollection of the names of the colors. named: #notAllThatGray put: aColor Add a new color to the list and create an access message and a class variable for it. fromUser Shows the palette of colors available at this display depth. Click anywhere to return the color you clicked on. hotColdShades: thisMany An array of thisMany colors showing temperature from blue to red to white hot. stdColorsForDepth: d An Array of colors available at this depth. For 16 bit and 32 bits, returns a ColorGenerator. It responds to at: with a Color for that index, simulating a very big Array. colorFromPixelValue: value depth: d Returns a Color whose bit pattern (inside a Bitmap) at this depth is the number specified. Normal users do not need to use this. (See also comments in these classes: Form, Bitmap, BitBlt) Default colorSpace is #sRGB. Subclasses might use other color spaces

### Methods
#### Color>>#closestPixelValue1

Return the nearest approximation to this color for a monochrome Form.


<details>
	<summary>See more</summary>
	
	closestPixelValue1
	"Return the nearest approximation to this color for a monochrome Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 0].  "white"

	self luminance > 0.5
		ifTrue: [^ 0]  "white"
		ifFalse: [^ 1].  "black"
</details>

#### Color>>#twiceLighter

Answer a significantly lighter shade of this color.


<details>
	<summary>See more</summary>
	
	twiceLighter
	"Answer a significantly lighter shade of this color."

	^ self adjustSaturation: -0.06 brightness: 0.15
</details>

#### Color>>#closestPixelValue8

Return the nearest approximation to this color for an 8-bit deep Form.


<details>
	<summary>See more</summary>
	
	closestPixelValue8
	"Return the nearest approximation to this color for an 8-bit deep Form."

	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 255].  "white"

	^self saturation < 0.2
		ifTrue: [
			^ self class grayToIndexMap at:(self green * 255) rounded + 1.  "nearest gray"
			]
		ifFalse: [
			"compute nearest entry in the color cube"
			40 + 
				((self red * 5) rounded * 36) +
				((self blue * 5) rounded * 6) +
				(self green * 5) rounded ]
</details>

#### Color>>#atLeastAsLuminentAs: aFloat

<details>
	<summary>See more</summary>
	
	atLeastAsLuminentAs: aFloat

	| revisedColor |
	revisedColor _ self.
	[revisedColor luminance < aFloat] whileTrue: [revisedColor _ revisedColor slightlyLighter].
	^revisedColor

</details>

#### Color>>#green

Return the green component of this color, a float in the range [0.0..1.0].


<details>
	<summary>See more</summary>
	
	green
	"Return the green component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 2
</details>

#### Color>>#pixelWordForDepth: depth

Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1.


<details>
	<summary>See more</summary>
	
	pixelWordForDepth: depth
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."

	| pixelValue |
	pixelValue _ self pixelValueForDepth: depth.
	^ self pixelWordFor: depth filledWith: pixelValue

</details>

#### Color>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	 self colorName ifNotNil: [ :name |
		^ aStream
			nextPutAll: 'Color ';
			nextPutAll: name].
	self storeOn: aStream.

</details>

#### Color>>#clipToValidValues

<details>
	<summary>See more</summary>
	
	clipToValidValues
	| v |
	1 to: self size do: [ :i |
		v _ self slotAt: i.
		v > 1 ifTrue: [self slotAt: i put: 1.0].
		v < 0 ifTrue: [self slotAt: i put: 0.0]]
</details>

#### Color>>#adjustBrightness: brightness

Adjust the relative brightness of this color. (lowest value is 0.005 so that hue information is not lost)


<details>
	<summary>See more</summary>
	
	adjustBrightness: brightness
	"Adjust the relative brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ Color
		h: self hue
		s: self saturation
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha
</details>

#### Color>>#hue

Return the hue of this color, an angle in the range [0.0..360.0]. Color orange hue


<details>
	<summary>See more</summary>
	
	hue
	"Return the hue of this color, an angle in the range [0.0..360.0].
	Color orange hue
	"

	| r g b max min span h |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	span _ (max - min) asFloat.
	span isZero ifTrue: [ ^ 0.0 ].

	r = max ifTrue: [
		h _ ((g - b) asFloat / span) * 60.0.
	] ifFalse: [
		g = max
			ifTrue: [ h _ 120.0 + (((b - r) asFloat / span) * 60.0). ]
			ifFalse: [ h _ 240.0 + (((r - g) asFloat / span) * 60.0). ].
	].

	^h mod: 360.0
</details>

#### Color>>#mightBeTranslucent

For Colors, answer if actually translucent or not.


<details>
	<summary>See more</summary>
	
	mightBeTranslucent
	"For Colors, answer if actually translucent or not."
	^self isOpaque not
</details>

#### Color>>#bitPatternForDepth: depth

Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps.


<details>
	<summary>See more</summary>
	
	bitPatternForDepth: depth
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"

	^ Bitmap with: (self pixelWordForDepth: depth)
</details>

#### Color>>#isWhite

Return true if the receiver represents white


<details>
	<summary>See more</summary>
	
	isWhite
	"Return true if the receiver represents white"
	(self slotAt: 1) = 1.0 ifFalse: [ ^ false ].
	(self slotAt: 2) = 1.0 ifFalse: [ ^ false ].
	(self slotAt: 3) = 1.0 ifFalse: [ ^ false ].
	^ true
</details>

#### Color>>#twiceDarker

Answer a significantly darker shade of this color.


<details>
	<summary>See more</summary>
	
	twiceDarker
	"Answer a significantly darker shade of this color."

	^ self adjustSaturation: 0.076 brightness: -0.15
</details>

#### Color>>#isTransparent

<details>
	<summary>See more</summary>
	
	isTransparent

	^ false

</details>

#### Color>>#brightness

Return the brightness of this color, a float in the range [0.0..1.0]. Color red brightness


<details>
	<summary>See more</summary>
	
	brightness
	"Return the brightness of this color, a float in the range [0.0..1.0].
	Color red brightness
	"

	"Do not include alpha if TranslucentColor"
	^ (self red max: self green) max: self blue
</details>

#### Color>>#isOpaque

<details>
	<summary>See more</summary>
	
	isOpaque
	^true
</details>

#### Color>>#isBlack

Return true if the receiver represents black


<details>
	<summary>See more</summary>
	
	isBlack
	"Return true if the receiver represents black"
	(self slotAt: 1) = 0.0 ifFalse: [ ^ false ].
	(self slotAt: 2) = 0.0 ifFalse: [ ^ false ].
	(self slotAt: 3) = 0.0 ifFalse: [ ^ false ].
	^ true
</details>

#### Color>>#- aColor

Answer aColor is subtracted from the given color in an additive color space.


<details>
	<summary>See more</summary>
	
	- aColor
	"Answer aColor is subtracted from the given color in an additive color space.  "
	"
	(Color white - Color red) display
	"
	^ Color new
		setRed: (self red - aColor red min: 1.0 max: 0.0)
		green: (self green - aColor green min: 1.0 max: 0.0)
		blue: (self blue - aColor blue min: 1.0 max: 0.0)
</details>

#### Color>>#veryMuchLighter

<details>
	<summary>See more</summary>
	
	veryMuchLighter

	^ self alphaMixed: 0.07 with: `Color white`
</details>

#### Color>>#icon

Answer a swatch to display in a menu or browser


<details>
	<summary>See more</summary>
	
	icon
	"Answer a swatch to display in a menu or browser"
	^self swatch
</details>

#### Color>>#red

Return the red component of this color, a float in the range [0.0..1.0].


<details>
	<summary>See more</summary>
	
	red
	"Return the red component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 1
</details>

#### Color>>#name

Return this color's name, or description if unnamed.


<details>
	<summary>See more</summary>
	
	name
	"Return this color's name, or description if unnamed."

	^ self printString

</details>

#### Color>>#storeArrayOn: aStream

<details>
	<summary>See more</summary>
	
	storeArrayOn: aStream

	aStream nextPutAll: '#('.
	self storeArrayValuesOn: aStream.
	aStream nextPutAll: ') '

</details>

#### Color>>#setRed: r green: g blue: b

<details>
	<summary>See more</summary>
	
	setRed: r green: g blue: b

	self basicSetRed: r green: g blue: b
</details>

#### Color>>#isRed

Am I considered Red ?


<details>
	<summary>See more</summary>
	
	isRed
	"Am I considered Red ?"

	^self red > (self green + 0.4)
		and: [self red > (self blue + 0.6)]
			and: [(self green - self blue) abs < 0.4]
</details>

#### Color>>#veryMuchDarker

<details>
	<summary>See more</summary>
	
	veryMuchDarker

	^ self alphaMixed: 0.25 with: `Color black`
</details>

#### Color>>#slightlyLighter

<details>
	<summary>See more</summary>
	
	slightlyLighter

	^ self adjustSaturation: -0.01 brightness: 0.03
</details>

#### Color>>#color

<details>
	<summary>See more</summary>
	
	color
	^ self
</details>

#### Color>>#wheel: thisMany

An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self. Array is of length thisMany. Very useful for displaying color based on a variable in your program.


<details>
	<summary>See more</summary>
	
	wheel: thisMany
	"An array of thisMany colors around the color wheel starting at self and ending all the way around the hue space just before self.  Array is of length thisMany.  Very useful for displaying color based on a variable in your program.  "

	| sat bri hue step c |
	sat _ self saturation.
	bri _ self brightness.
	hue _ self hue.
	step _ 360.0 / (thisMany max: 1).
	^ (1 to: thisMany) collect: [:num |
		c _ Color h: hue s: sat v: bri.  "hue is taken mod 360"
		hue _ hue + step.
		c].
"
(Color wheel: 8) withIndexDo: [:c :i | Display fill: (i*10@20 extent: 10@20) fillColor: c]
"
</details>

#### Color>>#= another

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= another 
	self == another ifTrue: [ ^ true ].
	self class == another class ifFalse: [ ^ false ].
	^self primitiveEqual: another
</details>

#### Color>>#alphaMixed: proportion with: aColor

Answer this color mixed with the given color. The proportion, a number between 0.0 and 1.0, determines what what fraction of the receiver to use in the mix. For example, 0.9 would yield a color close to the receiver. This method uses RGB interpolation; HSV interpolation can lead to surprises. Mixes the alphas (for transparency) also.


<details>
	<summary>See more</summary>
	
	alphaMixed: proportion with: aColor 
	"Answer this color mixed with the given color. The proportion, a number 
	between 0.0 and 1.0, determines what what fraction of the receiver to  
	use in the mix. For example, 0.9 would yield a color close to the  
	receiver. This method uses RGB interpolation; HSV interpolation can lead 
	to surprises.  Mixes the alphas (for transparency) also."

	| frac1 frac2 |
	frac1 _ proportion asFloat min: 1.0 max: 0.0.
	frac2 _ 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
		alpha: self alpha * frac1 + (aColor alpha * frac2)
</details>

#### Color>>#whiter

<details>
	<summary>See more</summary>
	
	whiter

	^ self alphaMixed: 0.8333 with: Color white

</details>

#### Color>>#alpha

Return the opacity ('alpha') value of opaque so that normal colors can be compared to TranslucentColors.


<details>
	<summary>See more</summary>
	
	alpha
	"Return the opacity ('alpha') value of opaque so that normal colors can be compared to TranslucentColors."

	^ 1.0

</details>

#### Color>>#/ aNumberOrColor

Answer this color with its RGB divided by the given number.


<details>
	<summary>See more</summary>
	
	/ aNumberOrColor
	"Answer this color with its RGB divided by the given number. "
	"
	(Color red / 2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ Color new
			setRed: (self red / aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green / aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue / aNumberOrColor blue min: 1.0 max: 0.0)
		].
	^ Color new
		setRed: (self red / aNumberOrColor min: 1.0 max: 0.0)
		green: (self green / aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue / aNumberOrColor min: 1.0 max: 0.0)
</details>

#### Color>>#muchDarker

<details>
	<summary>See more</summary>
	
	muchDarker

	^ self alphaMixed: 0.5 with: `Color black`

</details>

#### Color>>#orColorUnlike: theOther

If this color is a lot like theOther, then return its complement, otherwide, return self


<details>
	<summary>See more</summary>
	
	orColorUnlike: theOther
	"If this color is a lot like theOther, then return its complement, otherwide, return self"

	^ (self diff: theOther) < 0.3
		ifTrue: [theOther negated]
		ifFalse: [self]
</details>

#### Color>>#closestPixelValue4

Return the nearest approximation to this color for a 4-bit deep Form.


<details>
	<summary>See more</summary>
	
	closestPixelValue4
	"Return the nearest approximation to this color for a 4-bit deep Form."

	| bIndex |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	self isRed ifTrue: [^ 4].
	self isGreen ifTrue: [^ 5].
	self isBlue ifTrue: [^ 6].
	self isCyan ifTrue: [^ 7].
	self isYellow ifTrue: [^ 8].
	self isMagenta ifTrue: [^ 9].

	bIndex _ (self luminance * 8.0) rounded.  "bIndex in [0..8]"
	^ #(
		1	"black"
		10	"1/8 gray"
		11	"2/8 gray"
		12	"3/8 gray"
		3	"4/8 gray"
		13	"5/8 gray"
		14	"6/8 gray"
		15	"7/8 gray"
		2	"opaque white"
	) at: bIndex + 1.

</details>

#### Color>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPut: $)
</details>

#### Color>>#darkShades: thisMany

An array of thisMany colors from black to the receiver. Array is of length num. Very useful for displaying color based on a variable in your program.


<details>
	<summary>See more</summary>
	
	darkShades: thisMany
	"An array of thisMany colors from black to the receiver.  Array is of length num. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red darkShades: 12)"

	^ self class black mix: self shades: thisMany

</details>

#### Color>>#blue

Return the blue component of this color, a float in the range [0.0..1.0].


<details>
	<summary>See more</summary>
	
	blue
	"Return the blue component of this color, a float in the range [0.0..1.0]."

	^ self slotAt: 3
</details>

#### Color>>#isCyan

Am I considered Cyan ?


<details>
	<summary>See more</summary>
	
	isCyan
	"Am I considered Cyan ?"

	^self red < 0.05 
		and: [(self green min: self blue) > 0.5]					
			and: [(self green - self blue) abs < 0.2]
</details>

#### Color>>#blacker

<details>
	<summary>See more</summary>
	
	blacker

	^ self alphaMixed: 0.8333 with: Color black

</details>

#### Color>>#isBlue

Am I considered Blue?


<details>
	<summary>See more</summary>
	
	isBlue
	"Am I considered Blue?"

	^self blue > (self green + 0.3)
		and: [self blue > (self red + 0.3)]
			and: [(self green - self red) abs < 0.4]
</details>

#### Color>>#indexInMap: aColorMap

Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.


<details>
	<summary>See more</summary>
	
	indexInMap: aColorMap
	"Return the index corresponding to this color in the given color map. RGB colors are truncated to 3-, 4-, or 5-bits per color component when indexing into such a colorMap.  "

	aColorMap size = 2 ifTrue: [^ (self pixelValueForDepth: 1) + 1].
	aColorMap size = 4 ifTrue: [^ (self pixelValueForDepth: 2) + 1].
	aColorMap size = 16 ifTrue: [^ (self pixelValueForDepth: 4) + 1].
	aColorMap size = 256 ifTrue: [^ (self pixelValueForDepth: 8) + 1].
	aColorMap size = 512 ifTrue: [^ (self pixelValueForDepth: 9) + 1].
	aColorMap size = 4096 ifTrue: [^ (self pixelValueForDepth: 12) + 1].
	aColorMap size = 32768 ifTrue: [^ (self pixelValueForDepth: 15) + 1].
	self error: 'unknown pixel depth'.

</details>

#### Color>>#adjustSaturation: saturation brightness: brightness

Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)


<details>
	<summary>See more</summary>
	
	adjustSaturation: saturation brightness: brightness
	"Adjust the relative saturation and brightness of this color. (lowest value is 0.005 so that hue information is not lost)"

	^ Color
		h: self hue
		s: (self saturation + saturation min: 1.0 max: 0.005)
		v: (self brightness + brightness min: 1.0 max: 0.005)
		alpha: self alpha
</details>

#### Color>>#quiteWhiter

<details>
	<summary>See more</summary>
	
	quiteWhiter

	^ self alphaMixed: 0.6 with: `Color white`
</details>

#### Color>>#pixelValueForDepth: d

Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:


<details>
	<summary>See more</summary>
	
	pixelValueForDepth: d
	"Returns an integer representing the bits that appear in a single pixel of this color in a Form of the given depth. The depth must be one of 1, 2, 4, 8, 16, or 32. Contrast with pixelWordForDepth: and bitPatternForDepth:, which return either a 32-bit word packed with the given pixel value or a multiple-word Bitmap containing a pattern. The inverse is the class message colorFromPixelValue:depth:"
	"Details: For depths of 8 or less, the result is a colorMap index. For depths of 16 and 32, it is a direct color value with 5 or 8 bits per color component."
	"Transparency: The pixel value zero is reserved for transparent. For depths greater than 8 and less than 32 (no Indexed colors, no real alpha), black maps to the darkest possible blue.
	Note that 
		Color transparent class = TranslucentColor
	this special case is handled in TranslucentColor >> #pixelValueForDepth:
	"

	| bitBltFakeBlack val |
	"Most common case"
	"eight bits per component; top 8 bits set to all ones (opaque alpha)"
	d = 32 ifTrue: [
		^ 16rFF000000 bitOr:
			((((self slotAt: 1) * 255.999) truncated bitShift: 16) bitOr:
			((((self slotAt: 2) * 255.999) truncated bitShift: 8) bitOr: 
			(((self slotAt: 3) * 255.999) truncated))) ].

"Faster in 32 bit systems, but slower in 64 bits"
"	d = 32 ifTrue: [
		val _ LargePositiveInteger new: 4.
		val at: 3 put: ((self at: 1) * 255.999) truncated.
		val at: 2 put: ((self at: 2)  * 255.999) truncated.
		val at: 1 put: ((self at: 3)  * 255.999) truncated.
		val at: 4 put: 16rFF.
		^ val normalize]."

	d = 8 ifTrue: [^ self closestPixelValue8].  "common case"
	d < 8 ifTrue: [
		d = 4 ifTrue: [^ self closestPixelValue4].
		d = 2 ifTrue: [^ self closestPixelValue2].
		d = 1 ifTrue: [^ self closestPixelValue1]].

	"For the depth 16, pixelValue = 0 means transparent, black is represented as 16r8000 (rgb=0, pixelvalue != 0)."
	(d = 16) | (d = 15) ifTrue: [
		"five bits per component; top bits ignored"
		val _ ((self red * 31) rounded bitShift: 10) bitOr:
			(((self green * 31) rounded bitShift: 5) bitOr: 
			((self blue * 31) rounded)).
		^ val = 0
			ifTrue: [d = 16 ifTrue: [16r8000] ifFalse: [1]]
			ifFalse: [val]].

	"For the rest of the depths, pixelValue = 0 means transparent, and darkest blue is considered to be black."
	bitBltFakeBlack := 1.  "closest black that is not transparent in RGB - Not for depths <=8 (Indexed) or = 32 (RGBA)"
	d = 12 ifTrue: [  "for indexing a color map with 4 bits per color component"
		val _ ((self red * 15) rounded bitShift: 8) bitOr:
			(((self green * 15) rounded bitShift: 4) bitOr: 
			((self blue * 15) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	d = 9 ifTrue: [  "for indexing a color map with 3 bits per color component"
		val _ ((self red * 7) rounded bitShift: 6) bitOr:
			(((self green * 7) rounded bitShift: 3) bitOr: 
			((self blue * 7) rounded)).
		^ val = 0 ifTrue: [bitBltFakeBlack] ifFalse: [val]].

	self error: 'unknown pixel depth: ', d printString
</details>

#### Color>>#dansDarker

Return a darker shade of the same color. An attempt to do better than the current darker method. (now obsolete, since darker has been changed to do this. -dew)


<details>
	<summary>See more</summary>
	
	dansDarker
	"Return a darker shade of the same color.
	An attempt to do better than the current darker method.
	(now obsolete, since darker has been changed to do this. -dew)"
	^ Color h: self hue s: self saturation
		v: (self brightness - 0.16 max: 0.0)
</details>

#### Color>>#colorName

Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color. Return nil if named color support is not present


<details>
	<summary>See more</summary>
	
	colorName
	"Return this color's name, or nil if it has no name. Only returns a name if it exactly matches the named color.
	Return nil if named color support is not present"

	Color classPool 
		at: #ColorNamesDict
		ifPresent: [ :dict | ^dict keyAtValue: self ifAbsent: [nil]].
	^nil
</details>

#### Color>>#isYellow

Am I considered Yellow ?


<details>
	<summary>See more</summary>
	
	isYellow
	"Am I considered Yellow ?"

	^self blue < 0.1
		and: [(self red min: self green) > 0.5]					
			and: [(self red - self green) abs < 0.2]
</details>

#### Color>>#setHue: hue chroma: chroma luminance: luma

Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details. http://en.wikipedia.org/wiki/HSL_and_HSV hue belongs in [0.0, 360.0) chroma and luma belongs in [0.0, 1.0]


<details>
	<summary>See more</summary>
	
	setHue: hue chroma: chroma luminance: luma
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 

	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ luma - (0.299*r1) - (0.587*g1) - (0.114*b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0 
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1
</details>

#### Color>>#pixelWordFor: depth filledWith: pixelValue

Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1.


<details>
	<summary>See more</summary>
	
	pixelWordFor: depth filledWith: pixelValue
	"Return to a 32-bit word that concatenates enough copies of the given pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."
	| halfword |
	depth = 32 ifTrue: [^ pixelValue].
	depth = 16
		ifTrue: [halfword _ pixelValue]
		ifFalse: [halfword _ pixelValue * 
					(#(16rFFFF				"replicates at every bit"
						16r5555 -			"replicates every 2 bits"
						16r1111 - - -			"replicates every 4 bits"
						16r0101) at: depth)	"replicates every 8 bits"].
	^ halfword bitOr: (halfword bitShift: 16)
</details>

#### Color>>#lightShades: thisMany

An array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.


<details>
	<summary>See more</summary>
	
	lightShades: thisMany
	"An array of thisMany colors from white to self. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red lightShades: 12)"

	^ self class white mix: self shades: thisMany

</details>

#### Color>>#duller

<details>
	<summary>See more</summary>
	
	duller

	^ self adjustSaturation: -0.03 brightness: -0.2
</details>

#### Color>>#primitiveEqual: aColor

<details>
	<summary>See more</summary>
	
	primitiveEqual: aColor 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aColor class == self class ifFalse: [^ false].
	length _ self size.
	length = aColor size ifFalse: [^ false].
	1 to: self size do: [ :i |
		(self basicAt: i) = (aColor basicAt: i) ifFalse: [^ false]].
	^ true
</details>

#### Color>>#asNontranslucentColor

<details>
	<summary>See more</summary>
	
	asNontranslucentColor
	^ self
</details>

#### Color>>#slightlyDarker

<details>
	<summary>See more</summary>
	
	slightlyDarker

	^ self adjustBrightness: -0.03

</details>

#### Color>>#saturation

Return the saturation of this color, a value between 0.0 and 1.0. Color red saturation Color gray saturation


<details>
	<summary>See more</summary>
	
	saturation
	"Return the saturation of this color, a value between 0.0 and 1.0.
	Color red saturation
	Color gray saturation
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	max isZero ifTrue: [ ^0.0 ].
	^max - min / max
</details>

#### Color>>#luminance

Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity.


<details>
	<summary>See more</summary>
	
	luminance
	"Return the luminance of this color, a brightness value weighted by the human eye's color sensitivity."

	^ ((299 * self red) +
	   (587 * self green) +
	   (114 * self blue)) / 1000
</details>

#### Color>>#dominantColor

<details>
	<summary>See more</summary>
	
	dominantColor
	^ self
</details>

#### Color>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Color or: [ super is: aSymbol ]
</details>

#### Color>>#atMostAsLuminentAs: aFloat

<details>
	<summary>See more</summary>
	
	atMostAsLuminentAs: aFloat

	| revisedColor |
	revisedColor _ self.
	[revisedColor luminance > aFloat] whileTrue: [revisedColor _ revisedColor slightlyDarker].
	^revisedColor

</details>

#### Color>>#mix: color2 shades: thisMany

Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.


<details>
	<summary>See more</summary>
	
	mix: color2 shades: thisMany
	"Return an array of thisMany colors from self to color2. Very useful for displaying color based on a variable in your program.  "
	"Color showColors: (Color red mix: Color green shades: 12)"

	| redInc greenInc blueInc rr gg bb c out |
	thisMany = 1 ifTrue: [^ Array with: color2].
	redInc _ color2 red - self red / (thisMany-1).
	greenInc _ color2 green - self green / (thisMany-1).
	blueInc _ color2 blue - self blue / (thisMany-1).
	rr _ self red.  gg _ self green.  bb _ self blue.
	out _ (1 to: thisMany) collect: [:num |
		c _ Color r: rr g: gg b: bb.
		rr _ rr + redInc.
		gg _ gg + greenInc.
		bb _ bb + blueInc.
		c].
	out at: out size put: color2.	"hide roundoff errors"
	^ out

</details>

#### Color>>#isGreen

Am I considered Green ?


<details>
	<summary>See more</summary>
	
	isGreen
	"Am I considered Green ?"

	^self green > (self blue + 0.3)
		and: [self green > (self red + 0.3)]
</details>

#### Color>>#paler

Answer a paler shade of this color.


<details>
	<summary>See more</summary>
	
	paler
	"Answer a paler shade of this color."

	^ self adjustSaturation: -0.09 brightness: 0.09

</details>

#### Color>>#muchLighter

<details>
	<summary>See more</summary>
	
	muchLighter

	^ self alphaMixed: 0.233 with: `Color white`
</details>

#### Color>>#hexStringRGB

Color fromUser hexStringRGB


<details>
	<summary>See more</summary>
	
	hexStringRGB
	"
	Color fromUser hexStringRGB
	"
	^String streamContents: [ :strm |
		(self red * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self green * 255) rounded printOn: strm base: 16 length: 2 padded: true.
		(self blue * 255) rounded printOn: strm base: 16 length: 2 padded: true ]
</details>

#### Color>>#chroma

Return the chroma of this color, a value between 0.0 and 1.0, somewhat related to saturation. See http://en.wikipedia.org/wiki/HSL_and_HSV Color red chroma Color gray chroma


<details>
	<summary>See more</summary>
	
	chroma
	"Return the chroma of this color, a value between 0.0 and 1.0, somewhat related to saturation.
	See http://en.wikipedia.org/wiki/HSL_and_HSV
	Color red chroma
	Color gray chroma
	"

	| r g b max min |
	r _ self red.
	g _ self green.
	b _ self blue. 
	max _ (r max: g) max: b.
	min _ (r min: g) min: b.
	^max - min
</details>

#### Color>>#storeArrayValuesOn: aStream

<details>
	<summary>See more</summary>
	
	storeArrayValuesOn: aStream


	self red printOn: aStream fractionDigits: 3.
	aStream space.
	self green printOn: aStream fractionDigits: 3.
	aStream space.
	self blue printOn: aStream fractionDigits: 3


</details>

#### Color>>#* aNumberOrColor

Answer this color with its RGB multiplied by the given number.


<details>
	<summary>See more</summary>
	
	* aNumberOrColor
	"Answer this color with its RGB multiplied by the given number. "
	"
	(Color brown *2) display
	"
	(aNumberOrColor is: #Color) ifTrue: [
		^ (Color new
			setRed: (self red * aNumberOrColor red min: 1.0 max: 0.0)
			green: (self green * aNumberOrColor green min: 1.0 max: 0.0)
			blue: (self blue * aNumberOrColor blue min: 1.0 max: 0.0))
				alpha: self alpha * aNumberOrColor alpha
		].
	^ (Color new
		setRed: (self red * aNumberOrColor min: 1.0 max: 0.0)
		green: (self green * aNumberOrColor min: 1.0 max: 0.0)
		blue: (self blue * aNumberOrColor min: 1.0 max: 0.0))
			alpha: self alpha
</details>

#### Color>>#writeOn: aStream

Store the array of bits onto the argument, aStream. (leading byte ~= 16r80) identifies this as raw bits (uncompressed). Always store in Big Endian (Mac) byte order. Do the writing at BitBlt speeds. We only intend this for non-pointer arrays. Do nothing if I contain pointers.


<details>
	<summary>See more</summary>
	
	writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	aStream nextSignedInt32Put: self basicSize bigEndian: true.
	aStream nextWordsPutAll: self.
</details>

#### Color>>#makeForegroundColor

Make a foreground color contrasting with me


<details>
	<summary>See more</summary>
	
	makeForegroundColor
        "Make a foreground color contrasting with me"
        ^self luminance >= 0.5
                ifTrue: [`Color black`]
                ifFalse: [`Color white`]
</details>

#### Color>>#lighter

Answer a lighter shade of this color.


<details>
	<summary>See more</summary>
	
	lighter
	"Answer a lighter shade of this color."

	^ self adjustSaturation: -0.03 brightness: 0.08
</details>

#### Color>>#setHue: hue saturation: saturation brightness: brightness

Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details.


<details>
	<summary>See more</summary>
	
	setHue: hue saturation: saturation brightness: brightness
	"Initialize this color to the given hue, saturation, and brightness. See the comment in the instance creation method for details."

	| s v hf i f p q t | 
	s _ saturation asFloat min: 1.0 max: 0.0.
	v _ brightness asFloat min: 1.0 max: 0.0.
	hf _ hue \\ 360.
	i _ hf // 60.  			"integer part of hue"
	f _ (hf \\ 60) / 60.0.         	"fractional part of hue"

	p _ (1.0 - s) * v.
	q _ (1.0 - (s * f)) * v.
	t _ (1.0 - (s * (1.0 - f))) * v.

	0 = i ifTrue: [ ^ self setRed: v green: t blue: p ].
	1 = i ifTrue: [ ^ self setRed: q green: v blue: p ].
	2 = i ifTrue: [ ^ self setRed: p green: v blue: t ].
	3 = i ifTrue: [ ^ self setRed: p green: q blue: v ].
	4 = i ifTrue: [ ^ self setRed: t green: p blue: v ].
	5 = i ifTrue: [ ^ self setRed: v green: p blue: q ].

	self error: 'implementation error'
</details>

#### Color>>#quiteBlacker

<details>
	<summary>See more</summary>
	
	quiteBlacker

	^ self alphaMixed: 0.8 with: `Color black`

</details>

#### Color>>#setHue: hue chroma: chroma brightness: brightness

Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details. http://en.wikipedia.org/wiki/HSL_and_HSV hue belongs in [0.0, 360.0) chroma and luma belongs in [0.0, 1.0]


<details>
	<summary>See more</summary>
	
	setHue: hue chroma: chroma brightness: brightness
	"Initialize this color to the given hue, chroma, and luma. See the comment in the instance creation method for details.
	http://en.wikipedia.org/wiki/HSL_and_HSV
		hue belongs in [0.0, 360.0)
		chroma and luma belongs in [0.0, 1.0]
	"

	| x hf i r1 g1 b1 m | 
	hf _ hue \\ 360.
	i _ hf // 60.  				"integer part of hue"
	x _ (hf \\ 60) / 60.0 .         		"fractional part of hue"
	x _ x \\  2 .
	i \\ 2 = 1 ifTrue: [ x _ 1.0 - x ].
	x _ chroma  * x.

	0 = i ifTrue: [ r1 _ chroma. g1 _ x. b1 _ 0.0 ].
	1 = i ifTrue: [ r1 _ x. g1 _ chroma. b1 _ 0.0 ].
	2 = i ifTrue: [ r1 _ 0.0. g1 _ chroma. b1 _ x ].
	3 = i ifTrue: [ r1 _ 0.0. g1 _ x. b1 _ chroma ].
	4 = i ifTrue: [ r1 _ x. g1 _ 0.0. b1 _ chroma ].
	5 = i ifTrue: [ r1 _ chroma. g1 _ 0.0. b1 _ x ].

	m _ brightness - ((r1 max: g1) max: b1).
	m < 0.0
		ifTrue: [ ^nil ]. "No color exists with required parameters"
	r1 _ r1 + m.
	r1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	g1 _ g1 + m.
	g1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	b1 _ b1 + m.
	b1 > 1.0
		ifTrue: [ ^nil ]. "No color exists with required parameters".
	self setRed: r1 green: g1 blue: b1
</details>

#### Color>>#iconOrThumbnailOfSize: aNumberOrPoint

Answer an appropiate form to represent the receiver


<details>
	<summary>See more</summary>
	
	iconOrThumbnailOfSize: aNumberOrPoint 
	"Answer an appropiate form to represent the receiver"
	| form |
	form := Form extent: aNumberOrPoint asPoint asPoint depth: 32.
	form fillColor: self.
	^ form
</details>

#### Color>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	| hash |
	<primitive: 'primitiveHashArray' module: 'FloatArrayPlugin'>
	hash _ (self species hash + self size hash) hashMultiply.
	1 to: self size do: [ :i | hash _ (hash + (self basicAt: i)) hashMultiply].
	^hash
</details>

#### Color>>#setRed: r green: g blue: b range: range

Initialize this color's r, g, and b components to the given values in the range [0..r].


<details>
	<summary>See more</summary>
	
	setRed: r green: g blue: b range: range
	"Initialize this color's r, g, and b components to the given values in the range [0..r]."

	self basicSetRed: r/range green: g/range blue: b/range.
	self clipToValidValues
</details>

#### Color>>#negated

Return an RGB inverted color


<details>
	<summary>See more</summary>
	
	negated
	"Return an RGB inverted color"
	^Color
		r: 1.0 - self red
		g: 1.0 - self green
		b: 1.0 - self blue
</details>

#### Color>>#basicSetRed: r green: g blue: b

Initialize this color's r, g, and b components to the given values in the range [0.0..1.0].


<details>
	<summary>See more</summary>
	
	basicSetRed: r green: g blue: b
	"Initialize this color's r, g, and b components to the given values in the range [0.0..1.0]."

	self
		slotAt: 1 put: r;
		slotAt: 2 put: g;
		slotAt: 3 put: b
</details>

#### Color>>#slightlyWhiter

<details>
	<summary>See more</summary>
	
	slightlyWhiter

	^ self alphaMixed: 0.85 with: Color white

</details>

#### Color>>#mixed: proportion with: aColor

Mix with another color and do not preserve transpareny. Only use this for extracting the RGB value and mixing it. All other callers should use instead: aColor alphaMixed: proportion with: anotherColor


<details>
	<summary>See more</summary>
	
	mixed: proportion with: aColor 
	"Mix with another color and do not preserve transpareny.  Only use this for extracting the RGB value and mixing it.  All other callers should use instead: 
	aColor alphaMixed: proportion with: anotherColor
	"

	| frac1 frac2 |
	frac1 _ proportion asFloat min: 1.0 max: 0.0.
	frac2 _ 1.0 - frac1.
	^ Color
		r: self red * frac1 + (aColor red * frac2)
		g: self green * frac1 + (aColor green * frac2)
		b: self blue * frac1 + (aColor blue * frac2)
</details>

#### Color>>#bitPatternForGrayForm

Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps.


<details>
	<summary>See more</summary>
	
	bitPatternForGrayForm
	"Return a Bitmap, that best represents this color at the given depth. BitBlt calls this method to convert colors into Bitmaps."
	"See also:	pixelValueForDepth:	-- value for single pixel
				pixelWordForDepth:	-- a 32-bit word filled with the pixel value"
	"Details: The pattern for the most recently requested depth is cached."

	^Bitmap with: (self pixelWordFor: 8 filledWith: (self luminance * 255) rounded)
</details>

#### Color>>#isMagenta

Am I considered Magenta ?


<details>
	<summary>See more</summary>
	
	isMagenta
	"Am I considered Magenta ?"

	^self green < 0.05 
		and: [(self red min: self blue) > 0.4]					
			and: [(self red - self blue) abs < 0.3]
</details>

#### Color>>#swatch

Answer a swatch to display in a menu or browser


<details>
	<summary>See more</summary>
	
	swatch
	"Answer a swatch to display in a menu or browser"
	^self iconOrThumbnailOfSize: 16
</details>

#### Color>>#alpha: alphaValue

Return a new TranslucentColor with the given amount of opacity ('alpha').


<details>
	<summary>See more</summary>
	
	alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifFalse: [
		^ TranslucentColor new
			setRed: self red
			green: self green
			blue: self blue
			alpha: alphaValue ]
</details>

#### Color>>#closestPixelValue2

Return the nearest approximation to this color for a 2-bit deep Form.


<details>
	<summary>See more</summary>
	
	closestPixelValue2
	"Return the nearest approximation to this color for a 2-bit deep Form."

	| lum |
	"fast special cases"
	self isBlack ifTrue: [^ 1].  "black"
	self isWhite ifTrue: [^ 2].  "opaque white"

	lum _ self luminance.
	lum < 0.2 ifTrue: [^ 1].  "black"
	lum > 0.6 ifTrue: [^ 2].  "opaque white"
	^ 3  "50% gray"

</details>

#### Color>>#rgbDistance: otherColor

Compare two colors in distance


<details>
	<summary>See more</summary>
	
	rgbDistance: otherColor
	"Compare two colors in distance"
"
	^ (self hue - otherColor hue) abs +
	  (self saturation - otherColor saturation) abs *10 +
	  (self brightness - otherColor brightness) abs
"
	"See http://www.compuphase.com/cmetric.htm"
	| meanRed deltaRed deltaGreen deltaBlue |
	meanRed := (self red + otherColor red) abs / 2.
	deltaRed := (self red - otherColor red) abs.
	deltaGreen := (self green - otherColor green) abs.
	deltaBlue := (self blue - otherColor blue) abs.
	
	^ ( ((2 + (meanRed / 256)) * (deltaRed * deltaRed)) +
		(4 * deltaGreen) +
		((2 + ((255 - meanRed) / 256)) * deltaBlue)
	  ) sqrt
</details>

#### Color>>#diff: theOther

Returns a number between 0.0 and 1.0. Color gray diff: Color red


<details>
	<summary>See more</summary>
	
	diff: theOther
	"Returns a number between 0.0 and 1.0.
	Color gray diff: Color red
	"
	^(self - theOther ) abs sum / self size
</details>

#### Color>>#+ aColor

Answer this color mixed with the given color in an additive color space.


<details>
	<summary>See more</summary>
	
	+ aColor
	"Answer this color mixed with the given color in an additive color space.  "
	"
	(Color blue + Color green) display
	"
	^ Color new
		setRed: (self red + aColor red min: 1.0 max: 0.0)
		green: (self green + aColor green min: 1.0 max: 0.0)
		blue: (self blue + aColor blue min: 1.0 max: 0.0)
</details>

#### Color>>#attemptToMutateError

A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it.


<details>
	<summary>See more</summary>
	
	attemptToMutateError
	"A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it."

	self error: 'Color objects are immutable once created'

</details>

#### Color>>#isCollection

Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:


<details>
	<summary>See more</summary>
	
	isCollection
	^ false
</details>

#### Color>>#darker

Answer a darker shade of this color.


<details>
	<summary>See more</summary>
	
	darker
	"Answer a darker shade of this color."

	^ self adjustBrightness: -0.08
</details>

#### Color>>#convertToCurrentVersion: varDict refStream: smartRefStrm

subclasses should implement if they wish to convert old instances to modern ones


<details>
	<summary>See more</summary>
	
	convertToCurrentVersion: varDict refStream: smartRefStrm

	"subclasses should implement if they wish to convert old instances to modern ones"
	self size = 0 ifTrue: [
		^ Color new copyFrom: (varDict at: 'floatRGB') ].
	^ self
</details>

#### Color>>#restoreEndianness

This word object was just read in from a stream. It was stored in Big Endian (Mac) format. Reverse the byte order if the current machine is Little Endian. We only intend this for non-pointer arrays. Do nothing if I contain pointers.


<details>
	<summary>See more</summary>
	
	restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	Smalltalk isLittleEndian ifTrue: [
		BitBlt swapBytesIn32BitWords: self ]
</details>

#### Color>>#printString

Answer a String whose characters are a description of the receiver.


<details>
	<summary>See more</summary>
	
	printString
	"Answer a String whose characters are a description of the receiver."
	
	^ String streamContents: [ :stream | self printOn: stream ]
</details>

## ColorMap

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ColorMap>>#redShift

<details>
	<summary>See more</summary>
	
	redShift
	^shifts at: 1
</details>

#### ColorMap>>#isIndexed

Return true if the receiver uses a lookup mechanism for pixel mapping


<details>
	<summary>See more</summary>
	
	isIndexed
	"Return true if the receiver uses a lookup mechanism for pixel mapping"
	^colors notNil
</details>

#### ColorMap>>#redMask

<details>
	<summary>See more</summary>
	
	redMask
	^masks at: 1
</details>

#### ColorMap>>#at: index put: value

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: value
	^colors at: index put: value
</details>

#### ColorMap>>#pixelMap: pixelValue

Perform a reverse pixel mapping operation


<details>
	<summary>See more</summary>
	
	pixelMap: pixelValue
	"Perform a reverse pixel mapping operation"
	| pv |
	pv _ colors
		ifNil: [pixelValue]
		ifNotNil: [colors at: pixelValue].
	(shifts == nil and:[masks == nil]) 
		ifFalse:[pv _ (((pv bitAnd: self redMask) bitShift: self redShift) bitOr: 
				((pv bitAnd: self greenMask) bitShift: self greenShift)) bitOr:
					(((pv bitAnd: self blueMask) bitShift: self blueShift) bitOr: 
						((pv bitAnd: self alphaMask) bitShift: self alphaShift))].
	"Need to check for translucency else Form>>paint goes gaga"
	pv = 0 ifTrue:[pixelValue = 0 ifFalse:[pv _ 1]].
	^pv
</details>

#### ColorMap>>#blueMask

<details>
	<summary>See more</summary>
	
	blueMask
	^masks at: 3
</details>

#### ColorMap>>#colors

<details>
	<summary>See more</summary>
	
	colors
	^colors
</details>

#### ColorMap>>#greenShift: value

<details>
	<summary>See more</summary>
	
	greenShift: value
	shifts at: 2 put: value.
</details>

#### ColorMap>>#= aColorMap

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aColorMap

	"Any object is equal to itself"
	self == aColorMap ifTrue: [ ^ true ].

	"Return true if the receiver is equal to aColorMap"
	self species == aColorMap species ifFalse: [ ^false ].
	self isIndexed == aColorMap isIndexed ifFalse: [ ^false ].
	^self colors = aColorMap colors and: [
		self shifts = aColorMap shifts and: [
			self masks = aColorMap masks ]]
</details>

#### ColorMap>>#hash

Hash is re-implemented because #= is re-implemented


<details>
	<summary>See more</summary>
	
	hash
	"Hash is re-implemented because #= is re-implemented"
	^colors hash bitXor: (shifts hash bitXor: masks hash)
</details>

#### ColorMap>>#inverseMap

Return the inverse map of the receiver


<details>
	<summary>See more</summary>
	
	inverseMap
	"Return the inverse map of the receiver"
	| newMasks newShifts |
	colors ifNotNil:[^self error:'Not yet implemented'].
	newMasks _ WriteStream on: (Array new: 4).
	newShifts _ WriteStream on: (Array new: 4).
	masks with: shifts do:[:mask :shift|
		newMasks nextPut: (mask bitShift: shift).
		newShifts nextPut: shift negated].
	^ColorMap
		shifts: newShifts contents
		masks: newMasks contents
</details>

#### ColorMap>>#blueShift

<details>
	<summary>See more</summary>
	
	blueShift
	^shifts at: 3
</details>

#### ColorMap>>#masks

<details>
	<summary>See more</summary>
	
	masks
	^masks
</details>

#### ColorMap>>#isColormap

<details>
	<summary>See more</summary>
	
	isColormap
	^true
</details>

#### ColorMap>>#mapPixel: pixelValue

Perform a forward pixel mapping operation


<details>
	<summary>See more</summary>
	
	mapPixel: pixelValue
	"Perform a forward pixel mapping operation"
	| pv |
	(shifts == nil and:[masks == nil]) ifFalse:[
		pv _ (((pixelValue bitAnd: self redMask) bitShift: self redShift) bitOr:
			((pixelValue bitAnd: self greenMask) bitShift: self greenShift)) bitOr:
			(((pixelValue bitAnd: self blueMask) bitShift: self blueShift) bitOr:
			((pixelValue bitAnd: self alphaMask) bitShift: self alphaShift)).
	] ifTrue:[pv _ pixelValue].
	colors ifNotNil:[pv _ colors at: pv].
	"Need to check for translucency else Form>>paint goes gaga"
	pv = 0 ifTrue:[pixelValue = 0 ifFalse:[pv _ 1]].
	^pv
</details>

#### ColorMap>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	^colors at: index
</details>

#### ColorMap>>#alphaShift

<details>
	<summary>See more</summary>
	
	alphaShift
	^shifts at: 4
</details>

#### ColorMap>>#alphaMask

<details>
	<summary>See more</summary>
	
	alphaMask
	^masks at: 4
</details>

#### ColorMap>>#blueShift: value

<details>
	<summary>See more</summary>
	
	blueShift: value
	shifts at: 3 put: value
</details>

#### ColorMap>>#isFixed

Return true if the receiver does not use a lookup mechanism for pixel mapping


<details>
	<summary>See more</summary>
	
	isFixed
	"Return true if the receiver does not use a lookup mechanism for pixel mapping"
	^self isIndexed not
</details>

#### ColorMap>>#greenMask

<details>
	<summary>See more</summary>
	
	greenMask
	^masks at: 2
</details>

#### ColorMap>>#redShift: value

<details>
	<summary>See more</summary>
	
	redShift: value
	shifts at: 1 put: value
</details>

#### ColorMap>>#alphaShift: value

<details>
	<summary>See more</summary>
	
	alphaShift: value
	shifts at: 4 put: value
</details>

#### ColorMap>>#setShifts: shiftArray masks: maskArray colors: colorArray

<details>
	<summary>See more</summary>
	
	setShifts: shiftArray masks: maskArray colors: colorArray
	shiftArray ifNotNil:[shifts _ shiftArray asIntegerArray].
	maskArray ifNotNil:[masks _ maskArray asWordArray].
	colorArray ifNotNil:[colors _ colorArray asWordArray].
</details>

#### ColorMap>>#shifts

<details>
	<summary>See more</summary>
	
	shifts
	^shifts
</details>

#### ColorMap>>#greenShift

<details>
	<summary>See more</summary>
	
	greenShift
	^shifts at: 2
</details>

#### ColorMap>>#greenMask: value

<details>
	<summary>See more</summary>
	
	greenMask: value
	masks at: 2 put: value
</details>

#### ColorMap>>#blueMask: value

<details>
	<summary>See more</summary>
	
	blueMask: value
	masks at: 3 put: value
</details>

#### ColorMap>>#redMask: value

<details>
	<summary>See more</summary>
	
	redMask: value
	masks at: 1 put: value
</details>

#### ColorMap>>#alphaMask: value

<details>
	<summary>See more</summary>
	
	alphaMask: value
	masks at: 4 put: value
</details>

## GeometryTransformation

Superclass of several Geometry Transformations used mainly to specify locations of Morphs. Morphs specify a coordinate system in which they and their submorphs are expressed. A Morph's coordinate system is defined by a GeometryTransformation that is applied to points in inner space to convert them to points in outer space. Therefore #externalizePosition: is equivalent of #transform: and #internalizePosition: is equivalent to #inverseTransform:

### Methods
#### GeometryTransformation>>#externalizeRectangle: aRectangle

<details>
	<summary>See more</summary>
	
	externalizeRectangle: aRectangle
	^ (self transform: aRectangle origin) corner: (self transform: aRectangle corner)
</details>

#### GeometryTransformation>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #GeometryTransformation or: [ super is: aSymbol ]
</details>

#### GeometryTransformation>>#isPureTranslation

Return true if the receiver specifies no rotation or scaling.


<details>
	<summary>See more</summary>
	
	isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	^false
</details>

## Homography

An homography (or perspective transformation) for resampling images (for example). Can apply or correct for the perspective resulting from taking a photograph to a 2D object that is not perpendicular to the camera axis (for example, taking an image of the ground ahead of the camera). http://docs.opencv.org/modules/imgproc/doc/geometric_transformations.html#warpperspective To be of use, needs #map:to: in NumCuis/ImageProcessing.pck.st

### Methods
#### Homography>>#b2

<details>
	<summary>See more</summary>
	
	b2
	^self at: 6
</details>

#### Homography>>#a2

<details>
	<summary>See more</summary>
	
	a2
	^self at: 3
</details>

#### Homography>>#at: index put: value

Primitive. Assumes receiver is indexable. Store the argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
</details>

#### Homography>>#map: aPoint

<details>
	<summary>See more</summary>
	
	map: aPoint
	| xx yy zz |
	xx _ (self a0 * aPoint x) + (self a1 * aPoint y) + self a2.
	yy _ (self b0 * aPoint x) + (self b1 * aPoint y) + self b2.
	zz _ (self c0 * aPoint x) + (self c1 * aPoint y) + 1.
	^(xx / zz) @ (yy / zz)
</details>

#### Homography>>#c0

<details>
	<summary>See more</summary>
	
	c0
	^self at: 7
</details>

#### Homography>>#b0

<details>
	<summary>See more</summary>
	
	b0
	^self at: 4
</details>

#### Homography>>#a0

<details>
	<summary>See more</summary>
	
	a0
	^self at: 1
</details>

#### Homography>>#c1

<details>
	<summary>See more</summary>
	
	c1
	^self at: 8
</details>

#### Homography>>#b1

<details>
	<summary>See more</summary>
	
	b1
	^self at: 5
</details>

#### Homography>>#a1

<details>
	<summary>See more</summary>
	
	a1
	^self at: 2
</details>

#### Homography>>#at: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
</details>

## MorphicTranslation

Can replace an AffineTransformation when there is only a translation applied (no scale change, no rotation, no skew). Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore: - Don't assume the receiver will be modified. Always take the result. - Don't assume the answer will be a new instance. The receiver might be modified! (make a copy if needed) For consistency with AffineTransformation, always answer Float coordinates (Fraction coordinates are not handled by BitBlt or VectorEngine).

### Methods
#### MorphicTranslation>>#isTranslation: aPoint

The position of 0@0 in the external coordinate system. It is the translation we apply when transforming points.


<details>
	<summary>See more</summary>
	
	isTranslation: aPoint
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^deltaX = aPoint x and: [ deltaY = aPoint y ]
</details>

#### MorphicTranslation>>#isIdentity

Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself.


<details>
	<summary>See more</summary>
	
	isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."

	^deltaX = 0 and: [deltaY = 0]
</details>

#### MorphicTranslation>>#inverseTransform: aPoint

Apply the inverse transformation to aPoint.


<details>
	<summary>See more</summary>
	
	inverseTransform: aPoint
	"Apply the inverse transformation to aPoint."

	| x y |
	x _ aPoint x - deltaX.
	y _ aPoint y - deltaY.
	^x @ y
</details>

#### MorphicTranslation>>#transform: aPoint

Apply the direct transformation to aPoint.


<details>
	<summary>See more</summary>
	
	transform: aPoint
	"Apply the direct transformation to aPoint."

	| x y |
	x _ aPoint x + deltaX.
	y _ aPoint y + deltaY.
	^x @ y
</details>

#### MorphicTranslation>>#translation

Translation and position are the same. Use the word translation when thinking about coordinate transformation, but use the word position when thinking about morph locations


<details>
	<summary>See more</summary>
	
	translation
	"Translation and position are the same.
	Use the word translation when thinking about coordinate transformation, but use
	the word position when thinking about morph locations"

	^deltaX @ deltaY 
</details>

#### MorphicTranslation>>#innerComposedWithAffineTransformation: anAffineTransformation

Return the composition of the receiver and the transformation passed in. We know the class of the argument (through double dispatching)


<details>
	<summary>See more</summary>
	
	innerComposedWithAffineTransformation: anAffineTransformation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^anAffineTransformation composedWith: (AffineTransformation withTranslation: self translation)
" 	^ anAffineTransformation copy offsetBy: self translation" 
</details>

#### MorphicTranslation>>#internalizeDelta: aPoint

Internalize a distance vector. A distance is not a position. It is a magnitude with a direction. It is usually used as a delta to be added to a position to obtain some other position. We keep scale of stuff.


<details>
	<summary>See more</summary>
	
	internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint
</details>

#### MorphicTranslation>>#composedWith: aTransformation into: result

Return the composition of the receiver and the transformation passed in. Store the composed matrix into result. Please see the comment at: #composedWith:


<details>
	<summary>See more</summary>
	
	composedWith: aTransformation into: result
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	result setTranslation: self translation + aTransformation translation.
	^ result
</details>

#### MorphicTranslation>>#= aMorphicTranslation

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= aMorphicTranslation
	self == aMorphicTranslation ifTrue: [ ^ true ].
	(aMorphicTranslation is: #GeometryTransformation) ifFalse: [ ^false ].
	aMorphicTranslation isPureTranslation ifFalse: [ ^false ].
	^self translation = aMorphicTranslation translation
</details>

#### MorphicTranslation>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^self translation hash
</details>

#### MorphicTranslation>>#radians

Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0.0
</details>

#### MorphicTranslation>>#printOn: aStream

Note: Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(translation '.
	self translation printOn: aStream.
	aStream nextPutAll: ') '
</details>

#### MorphicTranslation>>#externalizePosition: aPoint

Answer coordinates for aPoint in the space we are in. aPoint is expressed in us.


<details>
	<summary>See more</summary>
	
	externalizePosition: aPoint
	"Answer coordinates for aPoint in the space we are in.
	 aPoint is expressed in us."

	^self transform: aPoint
</details>

#### MorphicTranslation>>#rotatedBy: radians

rotate the receiver by radians angle. Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	rotatedBy: radians
	"rotate the receiver by radians angle.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) rotatedBy: radians
</details>

#### MorphicTranslation>>#externalizeScalar: aNumber

Externalize a distance (without a direction). We keep scale of stuff.


<details>
	<summary>See more</summary>
	
	externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 We keep scale of stuff."

	^aNumber asFloat
</details>

#### MorphicTranslation>>#doesNotRotate

Answer true if #radians would answer 0.0. Performance optimization.


<details>
	<summary>See more</summary>
	
	doesNotRotate
	"Answer true if #radians would answer 0.0. Performance optimization."

	^true
</details>

#### MorphicTranslation>>#displayBoundsOfTransformOf: aRectangle

Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds and integer coordinates (i.e. adisplayBounds).


<details>
	<summary>See more</summary>
	
	displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^(aRectangle translatedBy: self translation) rounded
</details>

#### MorphicTranslation>>#italizing

a little shear Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	italizing
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing
</details>

#### MorphicTranslation>>#initialize

Initialize the receiver to the identity transformation (e.g., not affecting points)


<details>
	<summary>See more</summary>
	
	initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	deltaX _ 0.0.
	deltaY _ 0.0
</details>

#### MorphicTranslation>>#scaledByNumber: aNumber rotatedBy: radians

rotate the receiver by radians angle. Also scale by aNumber. Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions. This means that there is no difference between scaling then rotating and rotating then scaling. Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	scaledByNumber: aNumber rotatedBy: radians
	"rotate the receiver by radians angle. Also scale by aNumber.
	Note: the scale factor is a number, not a point. Therefore, the same scale is applied in all directions.
	This means that there is no difference between  scaling then rotating and rotating then scaling.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledByNumber: aNumber rotatedBy: radians
</details>

#### MorphicTranslation>>#withRotation: radians scale: scale

Set rotation and scaling according to parameters. Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	withRotation: radians scale: scale
	"Set rotation and scaling according to parameters.
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withRotation: radians scale: scale
</details>

#### MorphicTranslation>>#scaledBy: aPoint

Multiply by a scale. Argument can be a point, applying different scaling in x and in y directions. Keep the transformed position of 0@0, i.e. don't change offset. Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	scaledBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset.

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) scaledBy: aPoint
</details>

#### MorphicTranslation>>#isPureTranslation

Return true if the receiver specifies no rotation or scaling.


<details>
	<summary>See more</summary>
	
	isPureTranslation
	"Return true if the receiver specifies no rotation or scaling."
	^true
</details>

#### MorphicTranslation>>#withTranslation: aPoint

set an offset in the receiver Answer the modified object. I this implementation is self, but some classes of transformations, more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x asFloat.
	deltaY _ pt y asFloat.
	^self
</details>

#### MorphicTranslation>>#scale

Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	scale
	"Answer the *scalar* scale applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^1
</details>

#### MorphicTranslation>>#composedWith: innerTransformation

Return the composition of the receiver and the transformation passed in. The result is a translation that has the following effect: self externalize: (innerTransformation externalize: aPoint) innerTransformation internalize: (self internalize: aPoint).


<details>
	<summary>See more</summary>
	
	composedWith: innerTransformation
	"Return the composition of the receiver and the transformation passed in.
	The result is a translation  that has the following effect:
		self externalize: (innerTransformation externalize: aPoint)
		innerTransformation internalize: (self internalize: aPoint)."

	^innerTransformation innerComposedWithTranslation: self
</details>

#### MorphicTranslation>>#inverseTransformation

Return the inverse transformation of the receiver.


<details>
	<summary>See more</summary>
	
	inverseTransformation
	"Return the inverse transformation of the receiver."

	^MorphicTranslation withTranslation: self translation negated
</details>

#### MorphicTranslation>>#setTranslation: aPoint

private. Set the raw offset in the receiver


<details>
	<summary>See more</summary>
	
	setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x asFloat.
	deltaY _ pt y asFloat
</details>

#### MorphicTranslation>>#innerComposedWithTranslation: aMorphicTranslation

Return the composition of the receiver and the transformation passed in. We know the class of the argument (through double dispatching)


<details>
	<summary>See more</summary>
	
	innerComposedWithTranslation: aMorphicTranslation
	"Return the composition of the receiver and the transformation passed in.
	We know the class of the argument (through double dispatching)"

	^ self copy translatedBy: aMorphicTranslation translation
</details>

#### MorphicTranslation>>#withYAxisNegated

Swap inneer point Y sign. Make y increment upwards. This makes the any matrix transform from standard mathematical coordinates to standard display coordinates (in addition to the transform it was already doing) Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	withYAxisNegated
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)

	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withYAxisNegated
</details>

#### MorphicTranslation>>#internalizePosition: aPoint

Answer our coordinates for aPoint. aPoint is expressed in the space we are in.


<details>
	<summary>See more</summary>
	
	internalizePosition: aPoint
	"Answer our coordinates for aPoint.
	 aPoint is expressed in the space we are in."

	^self inverseTransform: aPoint
</details>

#### MorphicTranslation>>#degrees

Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y. Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these.


<details>
	<summary>See more</summary>
	
	degrees
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ 0
</details>

#### MorphicTranslation>>#internalizeScalar: aNumber

Internalize a distance (without a direction). We keep scale of stuff.


<details>
	<summary>See more</summary>
	
	internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	We keep scale of stuff."

	^aNumber asFloat
</details>

#### MorphicTranslation>>#italizing2

a little shear Answer the modified object. In this implementation this requires the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself, as if the receiver is already a AffineTransformation.


<details>
	<summary>See more</summary>
	
	italizing2
	"a little shear
	Answer the modified object. In this implementation this requires the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself,
	as if the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) italizing2
</details>

#### MorphicTranslation>>#externalizeDelta: aPoint

Externalize a distance vector. A distance is not a position. It is a magnitude with a direction. It is usually used as a delta to be added to a position to obtain some other position. We keep scale of stuff.


<details>
	<summary>See more</summary>
	
	externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint
</details>

#### MorphicTranslation>>#translatedBy: aPoint

add an offset in the receiver Answer the modified object. I this implementation is self, but some classes of transformations, more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance. Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself.


<details>
	<summary>See more</summary>
	
	translatedBy: aPoint
	"add an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."
	| pt |
	pt _ aPoint asPoint.
	deltaX _ deltaX + pt x.
	deltaY _ deltaY + pt y.
	^self
</details>

## Point

I represent an x-y pair of numbers usually designating a location on the screen. When dealing with display coordinates, the y axis is usually considered to increase downwards. However, the standard math convention is to consider it increasing upwards. Points don't need to know about this. In the first case, theta increases clockwise. In the second case, it increases counter-clockwise, also the standard math convention. Any method that doesn't follow this (because it assumes one specific convention) include this fact in the selector and in a comment. My instances are immutable. See #privateSetX:setY:

### Methods
#### Point>>#floor

Answer a Point that is the receiver's x and y floor. Answer the receiver if its coordinates are already integral.


<details>
	<summary>See more</summary>
	
	floor
	"Answer a Point that is the receiver's x and y floor. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x floor @ y floor

</details>

#### Point>>#eightNeighbors

<details>
	<summary>See more</summary>
	
	eightNeighbors
	^ (Array with: self + `1@0`
		with: self + `1@1`
		with: self + `0@1`
		with: self + `-1@1`) ,
	(Array with: self + `-1@0`
		with: self + `-1@-1`
		with: self + `0@-1`
		with: self + `1@-1`)
</details>

#### Point>>#nearestPointOnLineFrom: p1 to: p2

This will not give points beyond the endpoints


<details>
	<summary>See more</summary>
	
	nearestPointOnLineFrom: p1 to: p2
	"This will not give points beyond the endpoints"
	^ (self nearestPointAlongLineFrom: p1 to: p2)
		adhereTo: (p1 rect: p2)
</details>

#### Point>>#rho

Answer the receiver's radius in polar coordinate system.


<details>
	<summary>See more</summary>
	
	rho
	"Answer the receiver's radius in polar coordinate system."

	^(self dotProduct: self) sqrt
</details>

#### Point>>#barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: aBlock ifDegenerate: degeneratBlock

Return the barycentric coordinates for the receiver within the triangle defined by the three arguments.


<details>
	<summary>See more</summary>
	
	barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: aBlock ifDegenerate: degeneratBlock
	"Return  the barycentric coordinates for the receiver within the triangle defined by the three arguments."
	| p0 b0 b1 b2 b3 |
	p0 _ self.
	b0 _ ((p2 x - p1 x) * (p3 y - p1 y)) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero ifTrue: [ ^ degeneratBlock value ]. "degenerate"
	b0 _ 1.0 / b0.
	b1 _ (((p2 x - p0 x) * (p3 y - p0 y)) - ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 _ (((p3 x - p0 x) * (p1 y - p0 y)) - ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 _ (((p1 x - p0 x) * (p2 y - p0 y)) - ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	^aBlock value: b1 value: b2 value: b3
</details>

#### Point>>#isInsideCircle: a with: b with: c

Returns TRUE if self is inside the circle defined by the points a, b, c. See Guibas and Stolfi (1985) p.107


<details>
	<summary>See more</summary>
	
	isInsideCircle: a with: b with: c 
	"Returns TRUE if self is inside the circle defined by the     
	points a, b, c. See Guibas and Stolfi (1985) p.107"
	^ (a dotProduct: a)
		* (b triangleArea: c with: self) - ((b dotProduct: b)
			* (a triangleArea: c with: self)) + ((c dotProduct: c)
			* (a triangleArea: b with: self)) - ((self dotProduct: self)
			* (a triangleArea: b with: c)) > 0.0
</details>

#### Point>>#truncated

Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral.


<details>
	<summary>See more</summary>
	
	truncated
	"Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x truncated @ y truncated

</details>

#### Point>>#sideOf: otherPoint

Returns #left, #right or #center if the otherPoint lies to the left, right or on the line given by the vector from 0@0 to self


<details>
	<summary>See more</summary>
	
	sideOf: otherPoint 
	"Returns #left, #right or #center if the otherPoint lies to the left, right 
	or on the line given by the vector from 0@0 to self"
	| side |
	side _ (self crossProduct: otherPoint) sign.
	^ {#right. #center. #left} at: side + 2

</details>

#### Point>>#x

Answer the x coordinate.


<details>
	<summary>See more</summary>
	
	x
	"Answer the x coordinate."

	^x
</details>

#### Point>>#crossProduct: aPoint

Answer a number that is the cross product of the receiver and the argument, aPoint.


<details>
	<summary>See more</summary>
	
	crossProduct: aPoint 
	"Answer a number that is the cross product of the receiver and the 
	argument, aPoint."

	^ (x * aPoint y) - (y * aPoint x)
</details>

#### Point>>#between: min and: max

Answer whether the receiver is less than or equal to the argument, max, and greater than or equal to the argument, min.


<details>
	<summary>See more</summary>
	
	between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
</details>

#### Point>>#adhereTo: aRectangle

If the receiver lies outside aRectangle, return the nearest point on the boundary of the rectangle, otherwise return self.


<details>
	<summary>See more</summary>
	
	adhereTo: aRectangle
	"If the receiver lies outside aRectangle, return the nearest point on the boundary of the rectangle, otherwise return self."

	(aRectangle containsPoint: self) ifTrue: [^ self].
	^ ((x max: aRectangle left) min: aRectangle right)
		@ ((y max: aRectangle top) min: aRectangle bottom)
</details>

#### Point>>#printOn: aStream

The receiver prints on aStream in terms of infix notation.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y printOn: aStream
</details>

#### Point>>#rotatedBy: radians

<details>
	<summary>See more</summary>
	
	rotatedBy: radians

	| r theta |
	r _ self r.
	theta _ self theta +  radians asFloat.
	^ (r * theta cos) @ (r * theta sin)
</details>

#### Point>>#truncateTo: grid

Answer a Point that is the receiver's x and y truncated to grid x and grid y.


<details>
	<summary>See more</summary>
	
	truncateTo: grid
	"Answer a Point that is the receiver's x and y truncated to grid x and 
	grid y."
	| gridPoint |
	gridPoint _ grid asPoint.
	^(x truncateTo: gridPoint x) @ (y truncateTo: gridPoint y)
</details>

#### Point>>#dotProduct: aPoint

Answer a number that is the dot product of the receiver and the argument, aPoint. That is, the two points are multipled and the coordinates of the result summed.


<details>
	<summary>See more</summary>
	
	dotProduct: aPoint 
	"Answer a number that is the dot product of the receiver and the 
	argument, aPoint. That is, the two points are multipled and the 
	coordinates of the result summed."

	^ (x * aPoint x) + (y * aPoint y)
</details>

#### Point>>#onLineFrom: p1 to: p2

<details>
	<summary>See more</summary>
	
	onLineFrom: p1 to: p2
	^ self onLineFrom: p1 to: p2 within: 2
</details>

#### Point>>#quadrantOf: otherPoint

Return 1..4 indicating relative direction to otherPoint. 1 is downRight, 2=downLeft, 3=upLeft, 4=upRight


<details>
	<summary>See more</summary>
	
	quadrantOf: otherPoint
	"Return 1..4 indicating relative direction to otherPoint.
	1 is downRight, 2=downLeft, 3=upLeft, 4=upRight"
	^ x <= otherPoint x
		ifTrue: [y < otherPoint y ifTrue: [1] ifFalse: [4]]
		ifFalse: [y <= otherPoint y ifTrue: [2] ifFalse: [3]]
"
[Sensor isAnyButtonPressed] whileFalse: [
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	(Display boundingBox center quadrantOf: Sensor mousePoint) printString displayAt: 0@0]
"
</details>

#### Point>>#y

Answer the y coordinate.


<details>
	<summary>See more</summary>
	
	y
	"Answer the y coordinate."

	^y
</details>

#### Point>>#scaledBy: factor

Answer a Point scaled by factor (an instance of Point).


<details>
	<summary>See more</summary>
	
	scaledBy: factor 
	"Answer a Point scaled by factor (an instance of Point)."

	^(factor x * x) @ (factor y * y)
</details>

#### Point>>#rounded

Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral.


<details>
	<summary>See more</summary>
	
	rounded
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x rounded @ y rounded

</details>

#### Point>>#triangleArea: b with: c

Returns twice the area of the oriented triangle (a, b, c), i.e., the area is positive if the triangle is oriented counterclockwise


<details>
	<summary>See more</summary>
	
	triangleArea: b with: c
	"Returns twice the area of the oriented triangle (a, b, c), i.e., the   
	area is positive if the triangle is oriented counterclockwise"
	^ b x - self x * (c y - self y) - (b y - self y * (c x - self x))
</details>

#### Point>>#interpolateTo: end at: amountDone

Interpolate between the instance and end after the specified amount has been done (0 - 1).


<details>
	<summary>See more</summary>
	
	interpolateTo: end at: amountDone
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self + ((end - self) * amountDone).
</details>

#### Point>>#isPoint

Overridden to return true in Point.


<details>
	<summary>See more</summary>
	
	isPoint
	^ true
</details>

#### Point>>#asPoint

Answer the receiver itself.


<details>
	<summary>See more</summary>
	
	asPoint
	"Answer the receiver itself."

	^self
</details>

#### Point>>#adaptToNumber: rcvr andSend: selector

If I am involved in arithmetic with an Integer, convert it to a Point.


<details>
	<summary>See more</summary>
	
	adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Point."
	^ rcvr@rcvr perform: selector with: self
</details>

#### Point>>#initializePvtX: xValue y: yValue

Points are immutable.


<details>
	<summary>See more</summary>
	
	initializePvtX: xValue y: yValue
	"Points are immutable."
	x _ xValue.
	y _ yValue
</details>

#### Point>>#asIntegerPoint

<details>
	<summary>See more</summary>
	
	asIntegerPoint
	^ x asInteger @ y asInteger
</details>

#### Point>>#- arg

Answer a Point that is the difference of the receiver and arg.


<details>
	<summary>See more</summary>
	
	- arg 
	"Answer a Point that is the difference of the receiver and arg."

	arg isPoint ifTrue: [^ (x - arg x) @ (y - arg y)].
	^ arg adaptToPoint: self andSend: #-
</details>

#### Point>>#rect: aPoint

Answer a Rectangle that encompasses the receiver and aPoint. This is the most general infix way to create a rectangle.


<details>
	<summary>See more</summary>
	
	rect: aPoint 
	"Answer a Rectangle that encompasses the receiver and aPoint.
	This is the most general infix way to create a rectangle."

	^ Rectangle 
		origin: (self min: aPoint)
		corner: (self max: aPoint)
</details>

#### Point>>#to: end sideOf: otherPoint

Returns #left, #right, #center if the otherPoint lies to the left, right or on the line given by the vector from self to end


<details>
	<summary>See more</summary>
	
	to: end sideOf: otherPoint 
	"Returns #left, #right, #center if the otherPoint lies to the left, right or on the line given by the vector from self to end"
	^ end - self sideOf: otherPoint - self
</details>

#### Point>>#\\ arg

Answer a Point that is the mod of the receiver and arg.


<details>
	<summary>See more</summary>
	
	\\ arg 
	"Answer a Point that is the mod of the receiver and arg."

	arg isPoint ifTrue: [^ (x \\ arg x) @ (y \\ arg y)].
	^ arg adaptToPoint: self andSend: #\\
</details>

#### Point>>#* arg

Answer a Point that is the product of the receiver and arg.


<details>
	<summary>See more</summary>
	
	* arg 
	"Answer a Point that is the product of the receiver and arg."

	arg isPoint ifTrue: [^ (x * arg x) @ (y * arg y)].
	^ arg adaptToPoint: self andSend: #*
</details>

#### Point>>#enclosingRectangleWith: aPoint

Answer a Rectangle with integer coordinates that includes self and aPoint.


<details>
	<summary>See more</summary>
	
	enclosingRectangleWith: aPoint 
	"Answer a Rectangle with integer coordinates that includes self and aPoint."

	self flag: #revisarM3.
	^Rectangle
		origin: (x min: aPoint x) floor @ (y min: aPoint y) floor 
		corner: (x max: aPoint x) ceiling @ (y max: aPoint y ceiling) +1
</details>

#### Point>>#max: aPoint

Answer the lower right corner of the rectangle uniquely defined by the receiver and the argument, aPoint.


<details>
	<summary>See more</summary>
	
	max: aPoint 
	"Answer the lower right corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x max: aPoint x) @ (y max: aPoint y)
</details>

#### Point>>#shallowCopy

Immutable


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Immutable"
	^ self.
</details>

#### Point>>#transposed

<details>
	<summary>See more</summary>
	
	transposed
	^y@x
</details>

#### Point>>#>= aPoint

Answer whether the receiver is neither above nor to the left of aPoint.


<details>
	<summary>See more</summary>
	
	>= aPoint 
	"Answer whether the receiver is neither above nor to the left of aPoint."

	^x >= aPoint x and: [y >= aPoint y]
</details>

#### Point>>#// arg

Answer a Point that is the quotient of the receiver and arg.


<details>
	<summary>See more</summary>
	
	// arg 
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x // arg x) @ (y // arg y)].
	^ arg adaptToPoint: self andSend: #//
</details>

#### Point>>#abs

Answer a Point whose x and y are the absolute values of the receiver's x and y.


<details>
	<summary>See more</summary>
	
	abs
	"Answer a Point whose x and y are the absolute values of the receiver's x 
	and y."

	^ x abs @ y abs
</details>

#### Point>>#= aPoint

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= aPoint

	self == aPoint ifTrue: [ ^ true ].
	self species == aPoint species ifFalse: [ ^ false ].

	^ x = aPoint x and: [ y = aPoint y ]
</details>

#### Point>>#magnitude

<details>
	<summary>See more</summary>
	
	magnitude
	^self r
</details>

#### Point>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^(x hash hashMultiply + y hash) hashMultiply
</details>

#### Point>>#r

Answer the receiver's radius in polar coordinate system.


<details>
	<summary>See more</summary>
	
	r
	"Answer the receiver's radius in polar coordinate system."

	^self rho
</details>

#### Point>>#insideTriangle: p1 with: p2 with: p3

Return true if the receiver is within the triangle defined by the three coordinates. Note: This method computes the barycentric coordinates for the receiver and tests those coordinates.


<details>
	<summary>See more</summary>
	
	insideTriangle: p1 with: p2 with: p3
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."
	self
		barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: [ :b1 :b2 :b3 | 
			b1 < 0.0 ifTrue: [ ^false ].
			b2 < 0.0 ifTrue: [ ^false ].
			b3 < 0.0 ifTrue: [ ^false ].
			^true ]
		ifDegenerate: [ ^false ]
</details>

#### Point>>#corner: aPoint

Answer a Rectangle whose origin is the receiver and whose corner is aPoint. This is one of the infix ways of expressing the creation of a rectangle.


<details>
	<summary>See more</summary>
	
	corner: aPoint 
	"Answer a Rectangle whose origin is the receiver and whose corner is 
	aPoint. This is one of the infix ways of expressing the creation of a 
	rectangle."

	^Rectangle origin: self corner: aPoint
</details>

#### Point>>#asFloatPoint

<details>
	<summary>See more</summary>
	
	asFloatPoint
	^ x asFloat @ y asFloat
</details>

#### Point>>#onLineFrom: p1 to: p2 within: epsilon

Answer true if the receiver lies on the given line segment between p1 and p2 within a small epsilon.


<details>
	<summary>See more</summary>
	
	onLineFrom: p1 to: p2 within: epsilon
	"Answer true if the receiver lies on the given line segment between p1 and p2 within a small epsilon."

	"is this point within the box spanning p1 and p2 expanded by epsilon? (optimized)"
	p1 x < p2 x
		ifTrue: [
			((x < (p1 x - epsilon)) or: [x > (p2 x + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((x < (p2 x - epsilon)) or: [x > (p1 x + epsilon)]) ifTrue: [^ false]].
	p1 y < p2 y
		ifTrue: [
			((y < (p1 y - epsilon)) or: [y > (p2 y + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((y < (p2 y - epsilon)) or: [y > (p1 y + epsilon)]) ifTrue: [^ false]].

	"it's in the box; is it on the line?"
	^ (self dist: (self nearestPointAlongLineFrom: p1 to: p2)) <= epsilon
</details>

#### Point>>#negated

Answer a point whose x and y coordinates are the negatives of those of the receiver. 6/6/96 sw


<details>
	<summary>See more</summary>
	
	negated
	"Answer a point whose x and y coordinates are the negatives of those of the receiver.  6/6/96 sw"
	"Optimized for speed -- ar 8/26/2001"
	^ (0 - x) @ (0 - y)
</details>

#### Point>>#< aPoint

Answer whether the receiver is above and to the left of aPoint.


<details>
	<summary>See more</summary>
	
	< aPoint 
	"Answer whether the receiver is above and to the left of aPoint."

	^x < aPoint x and: [y < aPoint y]
</details>

#### Point>>#flipBy: direction centerAt: c

Answer a Point which is flipped according to the direction about the point c. Direction must be #vertical or #horizontal.


<details>
	<summary>See more</summary>
	
	flipBy: direction centerAt: c
	"Answer a Point which is flipped according to the direction about the point c.
	Direction must be #vertical or #horizontal."
	direction == #vertical ifTrue: [^ x @ (c y * 2 - y)].
	direction == #horizontal ifTrue: [^ (c x * 2 - x) @ y].
	self error: 'unrecognizable direction'
</details>

#### Point>>#theta

Answer the angle the receiver makes with origin in radians. Answer is between Zero and TwoPi If coordinates are in Display convention (y increases downwards), angles increase clockwise. If coordinates are in math convention (y increases upwards), angles increase counter-clockwise


<details>
	<summary>See more</summary>
	
	theta
	"Answer the angle the receiver makes with origin in radians.
	Answer is between Zero and TwoPi
	If coordinates are in Display convention (y increases downwards), angles increase clockwise.
	If coordinates are in math convention (y increases upwards), angles increase counter-clockwise 
	"

	| tan theta |
	^x = 0
		ifTrue: [
			y >= 0
				ifTrue: [ 1.570796326794897 	"90.0 degreesToRadians"]
				ifFalse: [ 4.71238898038469	"270.0 degreesToRadians"]]
		ifFalse: [
			tan _ y asFloat / x asFloat.
			theta _ tan arcTan.
			x >= 0
				ifTrue: [
					y >= 0
						ifTrue: [ theta ]
						ifFalse: [
							6.283185307179586 "360.0 degreesToRadians"
							+ theta ]]
				ifFalse: [
					3.141592653589793 "180.0 degreesToRadians"
					+ theta ]]
</details>

#### Point>>#ceiling

Answer a Point that is the receiver's x and y ceiling. Answer the receiver if its coordinates are already integral.


<details>
	<summary>See more</summary>
	
	ceiling
	"Answer a Point that is the receiver's x and y ceiling. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x ceiling @ y ceiling

</details>

#### Point>>#squaredDistanceTo: aPoint

Answer the distance between aPoint and the receiver.


<details>
	<summary>See more</summary>
	
	squaredDistanceTo: aPoint
	"Answer the distance between aPoint and the receiver."
	| delta |
	delta _ aPoint - self.
	^delta dotProduct: delta
</details>

#### Point>>#/ arg

Answer a Point that is the quotient of the receiver and arg.


<details>
	<summary>See more</summary>
	
	/ arg 
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x / arg x) @ (y / arg y)].
	^ arg adaptToPoint: self andSend: #/
</details>

#### Point>>#to: end1 intersects: start2 to: end2

Returns true if the linesegment from start1 (=self) to end1 intersects with the segment from start2 to end2, otherwise false.


<details>
	<summary>See more</summary>
	
	to: end1 intersects: start2 to: end2 
	"Returns true if the linesegment from start1 (=self) to end1 intersects      
	    with the segment from start2 to end2, otherwise false."
	| start1 sideStart sideEnd |
	start1 _ self.
	(((start1 = start2 or: [end1 = end2])
		or: [start1 = end2])
		or: [start2 = end1])
		ifTrue: [^ true].
	sideStart _ start1 to: end1 sideOf: start2.
	sideEnd _ start1 to: end1 sideOf: end2.
	sideStart = sideEnd ifTrue: [^ false].
	sideStart _ start2 to: end2 sideOf: start1.
	sideEnd _ start2 to: end2 sideOf: end1.
	sideStart = sideEnd ifTrue: [^ false].
	^ true
</details>

#### Point>>#storeOn: aStream

x@y printed form is good for storing too


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"x@y printed form is good for storing too"
	self printOn: aStream
</details>

#### Point>>#min: aPoint

Answer the upper left corner of the rectangle uniquely defined by the receiver and the argument, aPoint.


<details>
	<summary>See more</summary>
	
	min: aPoint 
	"Answer the upper left corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x min: aPoint x) @ (y min: aPoint y)
</details>

#### Point>>#extent: aPoint

Answer a Rectangle whose origin is the receiver and whose extent is aPoint. This is one of the infix ways of expressing the creation of a rectangle.


<details>
	<summary>See more</summary>
	
	extent: aPoint 
	"Answer a Rectangle whose origin is the receiver and whose extent is 
	aPoint. This is one of the infix ways of expressing the creation of a 
	rectangle."

	^Rectangle origin: self extent: aPoint
</details>

#### Point>>#dist: aPoint

Answer the distance between aPoint and the receiver.


<details>
	<summary>See more</summary>
	
	dist: aPoint 
	"Answer the distance between aPoint and the receiver."

	^(aPoint - self) r
</details>

#### Point>>#normalized

Optimized for speed -- ar 8/26/2001


<details>
	<summary>See more</summary>
	
	normalized
	"Optimized for speed -- ar 8/26/2001"
	| r |
	r _ ((x*x) + (y * y)) sqrt.
	^(x / r) @ (y / r)
</details>

#### Point>>#> aPoint

Answer whether the receiver is below and to the right of aPoint.


<details>
	<summary>See more</summary>
	
	> aPoint 
	"Answer whether the receiver is below and to the right of aPoint."

	^x > aPoint x and: [y > aPoint y]
</details>

#### Point>>#nearestPointAlongLineFrom: p1 to: p2

Note this will give points beyond the endpoints. Streamlined by Gerardo Richarte 11/3/97


<details>
	<summary>See more</summary>
	
	nearestPointAlongLineFrom: p1 to: p2
	"Note this will give points beyond the endpoints.
	Streamlined by Gerardo Richarte 11/3/97"
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [^ p1 x @ y].
	p1 y = p2 y ifTrue: [^ x @ p1 y].
	x1 _ p1 x asFloat.
	y1 _ p1 y asFloat.
	x21 _ p2 x asFloat - x1.
	y21 _ p2 y asFloat - y1.
	t _ ((y asFloat - y1 / x21) + (x asFloat - x1 / y21))
			/ ((x21 / y21) + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
"
	| old new |
	Pen new place: 200@100; goto: (old _ 500@300).
	Display reverse: (old extent: 10@10).
	[Sensor isAnyButtonPressed] whileFalse:
		[(new _ (Sensor mousePoint nearestPointAlongLineFrom: 200@100 to: 500@300) )
			= old ifFalse:
				[Display reverse: (old extent: 10@10).
				Display reverse: ((old _ new) extent: 10@10)]]
"

</details>

#### Point>>#normal

Answer a Point representing the unit vector rotated 90 deg clockwise.


<details>
	<summary>See more</summary>
	
	normal
	"Answer a Point representing the unit vector rotated 90 deg clockwise."

	| n |
	n _ y negated @ x.
	^n / (n x * n x + (n y * n y)) sqrt
</details>

#### Point>>#+ arg

Answer a Point that is the sum of the receiver and arg.


<details>
	<summary>See more</summary>
	
	+ arg 
	"Answer a Point that is the sum of the receiver and arg."

	arg isPoint ifTrue: [^ (x + arg x) @ (y + arg y)].
	^ arg adaptToPoint: self andSend: #+
</details>

#### Point>>#degrees

Answer the angle the receiver makes with origin in radians. If coordinates are in Display convention (y increases downwards), angles increase clockwise. If coordinates are in math convention (y increases upwards), angles increase counter-clockwise


<details>
	<summary>See more</summary>
	
	degrees
	"Answer the angle the receiver makes with origin in radians.
	If coordinates are in Display convention (y increases downwards), angles increase clockwise.
	If coordinates are in math convention (y increases upwards), angles increase counter-clockwise "
	^self theta radiansToDegrees
</details>

#### Point>>#<= aPoint

Answer whether the receiver is neither below nor to the right of aPoint.


<details>
	<summary>See more</summary>
	
	<= aPoint 
	"Answer whether the receiver is neither below nor to the right of aPoint."

	^x <= aPoint x and: [y <= aPoint y]
</details>

#### Point>>#min: aMin max: aMax

<details>
	<summary>See more</summary>
	
	min: aMin max: aMax 

	^ (self min: aMin) max: aMax
</details>

#### Point>>#isZero

<details>
	<summary>See more</summary>
	
	isZero
	^x isZero and:[y isZero]
</details>

#### Point>>#printStringFractionDigits: placesDesired

<details>
	<summary>See more</summary>
	
	printStringFractionDigits: placesDesired
	^(x printStringFractionDigits: placesDesired), '@', (y printStringFractionDigits: placesDesired)
</details>

#### Point>>#adaptToCollection: rcvr andSend: selector

If I am involved in arithmetic with a Collection, return a Collection of the results of each element combined with me in that expression.


<details>
	<summary>See more</summary>
	
	adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
</details>

#### Point>>#fourNeighbors

<details>
	<summary>See more</summary>
	
	fourNeighbors
	^ Array with: self + `1@0`
		with: self + `0@1`
		with: self + `-1@0`
		with: self + `0@-1`
</details>

#### Point>>#translatedBy: delta

Answer a Point translated by delta (an instance of Point).


<details>
	<summary>See more</summary>
	
	translatedBy: delta 
	"Answer a Point translated by delta (an instance of Point)."

	^(delta x + x) @ (delta y + y)
</details>

#### Point>>#inverseRotatedBy: angle about: center

This method considers angle to be in standard math convention (counter clock wise) while at the same time it considers the points to be in display convention (y axis increases downwards)


<details>
	<summary>See more</summary>
	
	inverseRotatedBy: angle about: center
	"This method considers angle to be in standard math convention (counter clock wise) while at the same time it considers the points to be in display convention (y axis increases downwards)"

	| p r theta |
	p _ self - center.
	r _ p r.
	theta _ angle asFloat - p theta.
	^ (center x asFloat + (r * theta cos)) @
	  (center y asFloat - (r * theta sin))
</details>

## Rectangle

I represent a rectangular area of the screen. Arithmetic functions take points as arguments and carry out scaling and translating operations to create new instances of me. Rectangle functions create new instances by determining intersections of rectangles with rectangles. Note 1: only rectangles parallel to reference frame (Screen) can be represented by this class. Note 2: the Rectangle is represented by two extremities of one diagonal. By convention, it must be the diagonal: from rectangle origin (the point having smallest coordinates in reference frame), to rectangle corner (the point having largest coordinates in reference frame). Note 3: Screen coordinates conventions are: x is horizontal axis, zero at left border, oriented toward right; y is vertical axis, zero at top border, oriented toward bottom. This corresponds to the latin convention for writing text from left to right and top to bottom. Note 4: the Rectangle extent is obtained by subtracting rectangle origin to rectangle corner coordinates. If this leads to a negative width (extent x coordinate) and/or a negative height (extent y coordinate), then the Rectangle is degenerated and considered empty. Instance variables: origin <Point> the coordinates of corner having smallest coordinates (top left in Screen coordinates) corner <Point> the coordinates of corner having largest coordinates (bottom right in Screen coordinates)

### Methods
#### Rectangle>>#bottomCenter

Answer the point at the center of the bottom horizontal line of the receiver.


<details>
	<summary>See more</summary>
	
	bottomCenter
	"Answer the point at the center of the bottom horizontal line of the 
	receiver."

	^self center x @ self bottom
</details>

#### Rectangle>>#left: aNumber

<details>
	<summary>See more</summary>
	
	left: aNumber
	^aNumber @ origin y corner: corner
</details>

#### Rectangle>>#encompass: aPoint

Answer a Rectangle that contains both the receiver and aPoint. 5/30/96 sw


<details>
	<summary>See more</summary>
	
	encompass: aPoint 
	"Answer a Rectangle that contains both the receiver and aPoint.  5/30/96 sw"

	^ Rectangle 
		origin: (origin min: aPoint)
		corner: (corner max:  aPoint)
</details>

#### Rectangle>>#leftCenter

Answer the point at the center of the receiver's left vertical line.


<details>
	<summary>See more</summary>
	
	leftCenter
	"Answer the point at the center of the receiver's left vertical line."

	^self left @ self center y
</details>

#### Rectangle>>#outsetBy: delta

Answer a Rectangle that is outset from the receiver by delta. delta is a Rectangle, Point, or scalar.


<details>
	<summary>See more</summary>
	
	outsetBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]
</details>

#### Rectangle>>#bottomLeft

Answer the point at the left edge of the bottom horizontal line of the receiver.


<details>
	<summary>See more</summary>
	
	bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the 
	receiver."

	^origin x @ corner y
</details>

#### Rectangle>>#truncated

Answer a Rectangle whose origin and corner have any fractional parts removed. Answer the receiver if its coordinates are already integral.


<details>
	<summary>See more</summary>
	
	truncated
	"Answer a Rectangle whose origin and corner have any fractional parts removed. Answer the receiver if its coordinates are already integral."

	(origin x isInteger and:
	[origin y isInteger and:
	[corner x isInteger and:
	[corner y isInteger]]])
		ifTrue: [^ self].

	^ Rectangle origin: origin truncated corner: corner truncated

</details>

#### Rectangle>>#top

Answer the position of the receiver's top horizontal line.


<details>
	<summary>See more</summary>
	
	top
	"Answer the position of the receiver's top horizontal line."

	^origin y
</details>

#### Rectangle>>#rectanglesAt: y

<details>
	<summary>See more</summary>
	
	rectanglesAt: y
	(y+1) > self bottom ifTrue: [^ Array new].
	^ Array with: (origin x @ y corner: corner x @ (y+1))
</details>

#### Rectangle>>#printOn: aStream

Refer to the comment in Object|printOn:.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Refer to the comment in Object|printOn:."

	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream
</details>

#### Rectangle>>#expandBy: delta

Answer a Rectangle that is outset from the receiver by delta. delta is a Rectangle, Point, or scalar.


<details>
	<summary>See more</summary>
	
	expandBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]
</details>

#### Rectangle>>#truncateTo: grid

Answer a Rectangle whose origin and corner are truncated to grid x and grid y.


<details>
	<summary>See more</summary>
	
	truncateTo: grid
	"Answer a Rectangle whose origin and corner are truncated to grid x and grid y."

	^Rectangle origin: (origin truncateTo: grid)
				corner: (corner truncateTo: grid)
</details>

#### Rectangle>>#extent

Answer with a rectangle with origin 0@0 and corner the receiver's width @ the receiver's height.


<details>
	<summary>See more</summary>
	
	extent
	"Answer with a rectangle with origin 0@0 and corner the receiver's 
	width @ the receiver's height."

	^corner - origin
</details>

#### Rectangle>>#withLeft: x

Return a copy of me with a different left x


<details>
	<summary>See more</summary>
	
	withLeft: x 
	"Return a copy of me with a different left x"
	^ x @ origin y corner: corner x @ corner y
</details>

#### Rectangle>>#areasOutside: aRectangle do: aBlock

Answer an Array of Rectangles comprising the parts of the receiver not intersecting aRectangle.


<details>
	<summary>See more</summary>
	
	areasOutside: aRectangle do: aBlock
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle."

	| yOrigin yCorner |
	"Make sure the intersection is non-empty"
     (self intersects: aRectangle)
		ifFalse: [
			aBlock value: self.
			^self ].
	aRectangle origin y > origin y
		ifTrue: [ aBlock value: (origin corner: corner x @ (yOrigin := aRectangle origin y))] 
		ifFalse: [ yOrigin _ origin y ].
	aRectangle corner y < corner y
		ifTrue: [ aBlock value: (origin x @ (yCorner := aRectangle corner y) corner: corner) ]
		ifFalse: [ yCorner _ corner y ].
	aRectangle origin x > origin x 
		ifTrue: [ aBlock value: (origin x @ yOrigin corner: aRectangle origin x @ yCorner) ].
	aRectangle corner x < corner x 
		ifTrue: [ aBlock value: (aRectangle corner x @ yOrigin corner: corner x @ yCorner) ]
</details>

#### Rectangle>>#areasOutside: aRectangle

Answer an Array of Rectangles comprising the parts of the receiver not intersecting aRectangle.


<details>
	<summary>See more</summary>
	
	areasOutside: aRectangle
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle."

	| areas |
	"Make sure the intersection is non-empty"
     (self intersects: aRectangle)
		ifFalse: [^Array with: self].
	areas := OrderedCollection new.
	self areasOutside: aRectangle do: [ :r |  areas add: r ].
	^areas
</details>

#### Rectangle>>#topCenter

Answer the point at the center of the receiver's top horizontal line.


<details>
	<summary>See more</summary>
	
	topCenter
	"Answer the point at the center of the receiver's top horizontal line."

	^self center x @ self top
</details>

#### Rectangle>>#boundingBox

<details>
	<summary>See more</summary>
	
	boundingBox
	^ self
</details>

#### Rectangle>>#insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint

Answer a Rectangle that is inset from the receiver by a given amount in the origin and corner.


<details>
	<summary>See more</summary>
	
	insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint 
	"Answer a Rectangle that is inset from the receiver by a given amount in 
	the origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint
</details>

#### Rectangle>>#withHeight: height

Return a copy of me with a different height


<details>
	<summary>See more</summary>
	
	withHeight: height 
	"Return a copy of me with a different height"
	^ origin corner: corner x @ (origin y + height)
</details>

#### Rectangle>>#scaledBy: scale

Answer a Rectangle scaled by scale, a Point or a scalar.


<details>
	<summary>See more</summary>
	
	scaledBy: scale 
	"Answer a Rectangle scaled by scale, a Point or a scalar."

	^Rectangle origin: origin * scale corner: corner * scale
</details>

#### Rectangle>>#rounded

Answer a Rectangle whose origin and corner are rounded.


<details>
	<summary>See more</summary>
	
	rounded
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin rounded corner: self corner rounded
</details>

#### Rectangle>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Rectangle or: [ super is: aSymbol ]
</details>

#### Rectangle>>#area

Answer the receiver's area, the product of width and height.


<details>
	<summary>See more</summary>
	
	area
	"Answer the receiver's area, the product of width and height."
	| w |
	(w _ self width) <= 0 ifTrue: [^ 0].
	^ w * self height max: 0
</details>

#### Rectangle>>#intersect: aRectangle

Answer a Rectangle that is the area in which the receiver overlaps with aRectangle. Optimized for speed; old code read: ^Rectangle origin: (origin max: aRectangle origin) corner: (corner min: aRectangle corner)


<details>
	<summary>See more</summary>
	
	intersect: aRectangle 
	"Answer a Rectangle that is the area in which the receiver overlaps with 
	aRectangle. Optimized for speed; old code read:
		^Rectangle 
			origin: (origin max: aRectangle origin)
			corner: (corner min: aRectangle corner)
	"
	| aPoint left right top bottom |
	aPoint _ aRectangle origin.
	aPoint x > origin x ifTrue:[left _ aPoint x] ifFalse:[left _ origin x].
	aPoint y > origin y ifTrue:[top _ aPoint y] ifFalse:[top _ origin y].
	aPoint _ aRectangle corner.
	aPoint x < corner x ifTrue:[right _ aPoint x] ifFalse:[right _ corner x].
	aPoint y < corner y ifTrue:[bottom _ aPoint y] ifFalse:[bottom _ corner y].
	^Rectangle
		origin: (left@top)
		corner: (right@bottom)

</details>

#### Rectangle>>#bottomRight

Answer the point at the right edge of the bottom horizontal line of the receiver.


<details>
	<summary>See more</summary>
	
	bottomRight
	"Answer the point at the right edge of the bottom horizontal line of the 
	receiver."

	^corner
</details>

#### Rectangle>>#amountToTranslateWithin: aRectangle

Answer a Point, delta, such that self + delta is forced within aRectangle.


<details>
	<summary>See more</summary>
	
	amountToTranslateWithin: aRectangle
	"Answer a Point, delta, such that self + delta is forced within aRectangle."
	"Altered so as to prefer to keep self topLeft inside when all of self
	cannot be made to fit 7/27/96 di"
	| dx dy |
	dx _ 0.  dy _ 0.
	self right > aRectangle right ifTrue: [dx _ aRectangle right - self right].
	self bottom > aRectangle bottom ifTrue: [dy _ aRectangle bottom - self bottom].
	(self left + dx) < aRectangle left ifTrue: [dx _ aRectangle left - self left].
	(self top + dy) < aRectangle top ifTrue: [dy _ aRectangle top - self top].
	^ dx@dy
</details>

#### Rectangle>>#left

Answer the position of the receiver's left vertical line.


<details>
	<summary>See more</summary>
	
	left
	"Answer the position of the receiver's left vertical line."

	^origin x
</details>

#### Rectangle>>#setOrigin: topLeft corner: bottomRight

<details>
	<summary>See more</summary>
	
	setOrigin: topLeft corner: bottomRight
	origin _ topLeft.
	corner _ bottomRight
</details>

#### Rectangle>>#innerCorners

Return an array of inner corner points, ie, the most extreme pixels included, in the order of a quadrilateral spec for WarpBlt


<details>
	<summary>See more</summary>
	
	innerCorners
	"Return an array of inner corner points,
	ie, the most extreme pixels included,
	in the order of a quadrilateral spec for WarpBlt"
	| r1 |
	r1 _ self topLeft corner: self bottomRight - `1@1`.
	^ Array with: r1 topLeft with: r1 bottomLeft with: r1 bottomRight with: r1 topRight
</details>

#### Rectangle>>#withRight: x

Return a copy of me with a different right x


<details>
	<summary>See more</summary>
	
	withRight: x 
	"Return a copy of me with a different right x"
	^ origin x @ origin y corner: x @ corner y
</details>

#### Rectangle>>#isWide

<details>
	<summary>See more</summary>
	
	isWide
	^ self width > self height
</details>

#### Rectangle>>#bottom: aNumber

<details>
	<summary>See more</summary>
	
	bottom: aNumber
	^origin corner: corner x @ aNumber
</details>

#### Rectangle>>#merge: aRectangle

Answer a Rectangle that contains both the receiver and aRectangle.


<details>
	<summary>See more</summary>
	
	merge: aRectangle 
	"Answer a Rectangle that contains both the receiver and aRectangle."

	^Rectangle 
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)
</details>

#### Rectangle>>#rightCenter

Answer the point at the center of the receiver's right vertical line.


<details>
	<summary>See more</summary>
	
	rightCenter
	"Answer the point at the center of the receiver's right vertical line."

	^self right @ self center y
</details>

#### Rectangle>>#intersects: aRectangle

Answer whether aRectangle intersects the receiver anywhere.


<details>
	<summary>See more</summary>
	
	intersects: aRectangle 
	"Answer whether aRectangle intersects the receiver anywhere."
	"Optimized; old code answered:
		(origin max: aRectangle origin) < (corner min: aRectangle corner)"

	| rOrigin rCorner |
	rOrigin := aRectangle origin.
	rCorner := aRectangle corner.
	rCorner x <= origin x	ifTrue: [^ false].
	rCorner y <= origin y	ifTrue: [^ false].
	rOrigin x >= corner x	ifTrue: [^ false].
	rOrigin y >= corner y	ifTrue: [^ false].
"None of the two rectangle shall be empty"
	corner x <= origin x	ifTrue: [^ false].
	corner y <= origin y	ifTrue: [^ false].
	rCorner x <= rOrigin x	ifTrue: [^ false].
	rCorner y <= rOrigin y	ifTrue: [^ false].
	^ true

</details>

#### Rectangle>>#center

Answer the point at the center of the receiver.


<details>
	<summary>See more</summary>
	
	center
	"Answer the point at the center of the receiver."

	^self topLeft + self bottomRight // 2
</details>

#### Rectangle>>#aligned: aPoint1 with: aPoint2

Answer a Rectangle that is a translated by aPoint2 - aPoint1.


<details>
	<summary>See more</summary>
	
	aligned: aPoint1 with: aPoint2
	"Answer a Rectangle that is a translated by aPoint2 - aPoint1."

	^self translatedBy: aPoint2 - aPoint1
</details>

#### Rectangle>>#top: aNumber

<details>
	<summary>See more</summary>
	
	top: aNumber
	^origin x @ aNumber corner: corner
</details>

#### Rectangle>>#containsRect: aRect

Answer whether aRect is within the receiver (OK to coincide).


<details>
	<summary>See more</summary>
	
	containsRect: aRect
	"Answer whether aRect is within the receiver (OK to coincide)."

	^ aRect origin >= origin and: [aRect corner <= corner]

</details>

#### Rectangle>>#= aRectangle

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= aRectangle 

	self == aRectangle ifTrue: [ ^ true ].

	"Answer true if the receiver's species, origin and corner match aRectangle's."
	self species == aRectangle species
		ifFalse: [ ^false ].

	^ origin = aRectangle origin and: [ corner = aRectangle corner ]
</details>

#### Rectangle>>#insetBy: delta

Answer a Rectangle that is inset from the receiver by delta. delta is a Rectangle, Point, or scalar.


<details>
	<summary>See more</summary>
	
	insetBy: delta 
	"Answer a Rectangle that is inset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin + delta origin 
				corner: corner - delta corner ]
		ifFalse: [
			Rectangle
				origin: origin + delta 
				corner: corner - delta ]
</details>

#### Rectangle>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^origin hash bitXor: corner hash
</details>

#### Rectangle>>#origin

Answer the point at the top left corner of the receiver.


<details>
	<summary>See more</summary>
	
	origin
	"Answer the point at the top left corner of the receiver."

	^origin
</details>

#### Rectangle>>#translatedToBeWithin: aRectangle

Answer a copy of the receiver that does not extend beyond aRectangle. 7/8/96 sw


<details>
	<summary>See more</summary>
	
	translatedToBeWithin: aRectangle
	"Answer a copy of the receiver that does not extend beyond aRectangle.  7/8/96 sw"

	^ self translatedBy: (self amountToTranslateWithin: aRectangle)
</details>

#### Rectangle>>#extendBy: delta

Answer a Rectangle with the same origin as the receiver, but whose corner is offset by delta. delta is a Rectangle, Point, or scalar.


<details>
	<summary>See more</summary>
	
	extendBy: delta 
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin
				corner: corner + delta ]
</details>

#### Rectangle>>#bottom

Answer the position of the receiver's bottom horizontal line.


<details>
	<summary>See more</summary>
	
	bottom
	"Answer the position of the receiver's bottom horizontal line."

	^corner y
</details>

#### Rectangle>>#newRectFrom: newRectBlock

Track the outline of a new rectangle until mouse button changes. newFrameBlock produces each new rectangle from the previous


<details>
	<summary>See more</summary>
	
	newRectFrom: newRectBlock
	"Track the outline of a new rectangle until mouse button changes.
	newFrameBlock produces each new rectangle from the previous"
	| rect newRect buttonStart buttonNow aHand delay |
	delay _ Delay forMilliseconds: 10.
	buttonStart _ buttonNow _ Sensor isAnyButtonPressed.
	rect _ self.
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	[buttonNow == buttonStart] whileTrue: 
		[delay wait.
		buttonNow _ Sensor isAnyButtonPressed.
		newRect _ newRectBlock value: rect.
		newRect = rect ifFalse:
			[Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
			Display border: newRect width: 2 rule: Form reverse fillColor: `Color gray`.
			rect _ newRect]].
	Display border: rect width: 2 rule: Form reverse fillColor: `Color gray`.
	" pay the price for reading the sensor directly ; get this party started "
	aHand _ self runningWorld activeHand.
	aHand
		newMouseFocus: nil;
		flushEvents.
	Sensor processSensorEvent: Sensor createMouseEvent discardingMouseEvents: false.
	^ rect
</details>

#### Rectangle>>#containsPoint: aPoint

Answer whether aPoint is within the receiver.


<details>
	<summary>See more</summary>
	
	containsPoint: aPoint 
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]
</details>

#### Rectangle>>#storeOn: aStream

printed form is good for storing too


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"printed form is good for storing too"
	
	aStream nextPut: $(.
	self printOn: aStream.
	aStream nextPut: $).
</details>

#### Rectangle>>#width

Answer the width of the receiver.


<details>
	<summary>See more</summary>
	
	width
	"Answer the width of the receiver."

	^corner x - origin x
</details>

#### Rectangle>>#right: aNumber

<details>
	<summary>See more</summary>
	
	right: aNumber
	^origin corner: aNumber @ corner y
</details>

#### Rectangle>>#quickMerge: aRectangle

Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't. THis method is an optimization to reduce extra rectangle creations.


<details>
	<summary>See more</summary>
	
	quickMerge: aRectangle 
	"Answer the receiver if it encloses the given rectangle or the merge of the two rectangles if it doesn't. THis method is an optimization to reduce extra rectangle creations."

	| useRcvr rOrigin rCorner minX maxX minY maxY |
	aRectangle ifNil: [ ^self ].

	useRcvr _ true.
	rOrigin _ aRectangle topLeft.
	rCorner _ aRectangle bottomRight.
	minX _ rOrigin x < origin x ifTrue: [ useRcvr _ false. rOrigin x ] ifFalse: [ origin x ].
	maxX _ rCorner x > corner x ifTrue: [ useRcvr _ false. rCorner x ] ifFalse: [ corner x ].
	minY _ rOrigin y < origin y ifTrue: [ useRcvr _ false. rOrigin y ] ifFalse: [ origin y ].
	maxY _ rCorner y > corner y ifTrue:  [useRcvr _ false. rCorner y ] ifFalse: [ corner y ].

	^useRcvr
		ifTrue: [ self ]
		ifFalse: [ Rectangle origin: minX@minY corner: maxX@maxY ].

</details>

#### Rectangle>>#height

Answer the height of the receiver.


<details>
	<summary>See more</summary>
	
	height
	"Answer the height of the receiver."

	^corner y - origin y
</details>

#### Rectangle>>#isTall

<details>
	<summary>See more</summary>
	
	isTall
	^ self height > self width
</details>

#### Rectangle>>#corner

Answer the point at the bottom right corner of the receiver.


<details>
	<summary>See more</summary>
	
	corner
	"Answer the point at the bottom right corner of the receiver."

	^corner
</details>

#### Rectangle>>#squishedWithin: aRectangle

Return an adjustment of the receiver that fits within aRectangle by reducing its size, not by changing its origin.


<details>
	<summary>See more</summary>
	
	squishedWithin: aRectangle
	"Return an adjustment of the receiver that fits within aRectangle by reducing its size, not by changing its origin.  "

	^ origin corner: (corner min: aRectangle bottomRight)

"(50 @ 50 corner: 160 @ 100) squishedWithin:  (20 @ 10 corner: 90 @ 85)"

</details>

#### Rectangle>>#flippedBy: direction centerAt: aPoint

Return a copy flipped #vertical or #horizontal, about aPoint.


<details>
	<summary>See more</summary>
	
	flippedBy: direction centerAt: aPoint 
	"Return a copy flipped #vertical or #horizontal, about aPoint."
	| futureOrigin futureCorner |
	direction == #horizontal
		ifTrue:
			[futureOrigin := self topRight.
			futureCorner := self bottomLeft]
		ifFalse: [direction == #vertical
			ifTrue:
				[futureOrigin := self bottomLeft.
				futureCorner := self topRight]
			ifFalse: [self error: 'unrecognizable direction']].
	^ (futureOrigin flipBy: direction centerAt: aPoint)
		corner: (futureCorner flipBy: direction centerAt: aPoint)
</details>

#### Rectangle>>#translatedAndSquishedToBeWithin: aRectangle

Return an adjustment of the receiver that fits within aRectangle by - translating it to be within aRectangle if necessary, then - reducing its size, if necessary


<details>
	<summary>See more</summary>
	
	translatedAndSquishedToBeWithin: aRectangle
	"Return an adjustment of the receiver that fits within aRectangle by
		- translating it to be within aRectangle if necessary, then
		- reducing its size, if necessary"

	^ (self translatedToBeWithin: aRectangle) squishedWithin: aRectangle
</details>

#### Rectangle>>#hasPositiveExtent

<details>
	<summary>See more</summary>
	
	hasPositiveExtent
	^ (corner x > origin x) and: [corner y > origin y]
</details>

#### Rectangle>>#right

Answer the position of the receiver's right vertical line.


<details>
	<summary>See more</summary>
	
	right
	"Answer the position of the receiver's right vertical line."

	^corner x
</details>

#### Rectangle>>#topLeft

Answer the point at the top left corner of the receiver's top horizontal line.


<details>
	<summary>See more</summary>
	
	topLeft
	"Answer the point at the top left corner of the receiver's top horizontal line."

	^origin

</details>

#### Rectangle>>#corners

Return an array of corner points in the order of a quadrilateral spec for WarpBlt.


<details>
	<summary>See more</summary>
	
	corners
	"Return an array of corner points in the order of a quadrilateral spec for WarpBlt."

	^ Array
		with: self topLeft
		with: self bottomLeft
		with: self bottomRight
		with: self topRight

</details>

#### Rectangle>>#topRight

Answer the point at the top right corner of the receiver's top horizontal line.


<details>
	<summary>See more</summary>
	
	topRight
	"Answer the point at the top right corner of the receiver's top horizontal 
	line."

	^corner x @ origin y
</details>

#### Rectangle>>#translatedBy: factor

Answer a Rectangle translated by factor, a Point or a scalar.


<details>
	<summary>See more</summary>
	
	translatedBy: factor 
	"Answer a Rectangle translated by factor, a Point or a scalar."

	^Rectangle origin: origin + factor corner: corner + factor
</details>

## TranslucentColor

A TranslucentColor behaves just like a normal color, except that it will pack its alpha value into the fourth position. This allows creating forms with translucency for use with the alpha blend function of BitBlt. An alpha of zero is transparent, and 1.0 is opaque.

### Methods
#### TranslucentColor>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	self isTransparent ifTrue: [^ aStream nextPutAll: '(Color transparent)'].
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' r: '.
	self red printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' g: '.
	self green printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' b: '.
	self blue printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ' alpha: '.
	self alpha printOn: aStream fractionDigits: 3.
	aStream nextPutAll: ')'
</details>

#### TranslucentColor>>#bitPatternForDepth: depth

Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency.


<details>
	<summary>See more</summary>
	
	bitPatternForDepth: depth
	"Return an appropriate bit pattern. This will almost never be meaningful for tranlucentColors, except for the degenerate case of tranparency."

	self isTransparent ifTrue: [
		^ Bitmap with: 0].
	^ super bitPatternForDepth: depth
</details>

#### TranslucentColor>>#pixelValueForDepth: d

Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths.


<details>
	<summary>See more</summary>
	
	pixelValueForDepth: d
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."
	| basicPixelWord |
	"In 32bpp, if alpha = 0, ignore any color components, and answer R=0, G=0, B=0, Alpha=0.
	In depths > 8 and < 32, pixelValue zero is the special value used by BitBlt to denote transparent."
	self isTransparent ifTrue: [
		^ 0].
	basicPixelWord _ super pixelValueForDepth: d.
	^d < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]
</details>

#### TranslucentColor>>#pixelWordForDepth: depth

Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths.


<details>
	<summary>See more</summary>
	
	pixelWordForDepth: depth
	"Return the pixel value for this color at the given depth. Translucency only works in RGB; this color will appear either opaque or transparent at all other depths."

	| basicPixelWord |
	self isTransparent ifTrue: [^ 0].
	basicPixelWord _ super pixelWordForDepth: depth.
	^depth < 32
		ifTrue: [ basicPixelWord ]
		ifFalse: [ (basicPixelWord bitAnd: 16rFFFFFF) bitOr:
			((self alpha*255.0) rounded
				bitShift: 24) ]
</details>

#### TranslucentColor>>#isTransparent

<details>
	<summary>See more</summary>
	
	isTransparent
	^ self alpha isZero
</details>

#### TranslucentColor>>#alpha: alphaValue

Return a new TranslucentColor with the given amount of opacity ('alpha').


<details>
	<summary>See more</summary>
	
	alpha: alphaValue
	"Return a new TranslucentColor with the given amount of opacity ('alpha')."
	alphaValue = 1.0 ifTrue: [
		^ Color new
			setRed: self red
			green: self green
			blue: self blue].
	^ super alpha: alphaValue
</details>

#### TranslucentColor>>#isOpaque

<details>
	<summary>See more</summary>
	
	isOpaque

	^self alpha = 1.0
</details>

#### TranslucentColor>>#asNontranslucentColor

<details>
	<summary>See more</summary>
	
	asNontranslucentColor
	^ self alpha: 1.0
</details>

#### TranslucentColor>>#storeArrayValuesOn: aStream

<details>
	<summary>See more</summary>
	
	storeArrayValuesOn: aStream

	self isTransparent ifTrue: [
		^ aStream space].
	super storeArrayValuesOn: aStream.
	aStream space.
	self alpha printOn: aStream fractionDigits: 3
</details>

#### TranslucentColor>>#setRed: r green: g blue: b alpha: alphaValue

<details>
	<summary>See more</summary>
	
	setRed: r green: g blue: b alpha: alphaValue

	self basicSetRed: r green: g blue: b.
	self slotAt: 4 put: alphaValue
</details>

#### TranslucentColor>>#alpha

Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque.


<details>
	<summary>See more</summary>
	
	alpha
	"Return my alpha value, a number between 0.0 and 1.0 where 0.0 is completely transparent and 1.0 is completely opaque."

	^ self slotAt: 4
</details>

## WarpBlt

WarpBlt is a little warp-drive added on to BitBlt. It takes a quadrilateral as its source specification, while its destination is traversed and combined just like any other call to copyBits. The source quadrilateral is specified as an array of points starting with the corner that wants to end up in the topLeft, and proceding to the successive points that want to follow CCW around the destination rectangle. Note that in specifying a plain old rectangle source, its non topLeft points must be actual pixels, not outside by 1, as with rectangle bottmRight, eg. See the method Rectangle asQuad. WarpBlt does a fast job of rotation, reflection and scaling, and it can even produce a semblance of perspective. Depth parameters are included for future improvements in this direction. but the primitve does not support this yet.

### Methods
#### WarpBlt>>#copyQuad: pts toRect: destRect

<details>
	<summary>See more</summary>
	
	copyQuad: pts toRect: destRect
	self sourceQuad: pts destRect: destRect.
	self warpBits
</details>

#### WarpBlt>>#rgbMap: sourcePixel from: nBitsIn to: nBitsOut

NOTE: This code is copied verbatim from BitBltSimulation so that it may be removed from the system


<details>
	<summary>See more</summary>
	
	rgbMap: sourcePixel from: nBitsIn to: nBitsOut
	"NOTE: This code is copied verbatim from BitBltSimulation so that it
	may be removed from the system"
	"Convert the given pixel value with nBitsIn bits for each color component to a pixel value with nBitsOut bits for each color component. Typical values for nBitsIn/nBitsOut are 3, 5, or 8."
	| mask d srcPix destPix |
	self inline: true.
	(d _ nBitsOut - nBitsIn) > 0
		ifTrue:
			["Expand to more bits by zero-fill"
			mask _ (1 << nBitsIn) - 1.  "Transfer mask"
			srcPix _ sourcePixel << d.
			mask _ mask << d.
			destPix _ srcPix bitAnd: mask.
			mask _ mask << nBitsOut.
			srcPix _ srcPix << d.
			^ destPix + (srcPix bitAnd: mask)
				 	+ (srcPix << d bitAnd: mask << nBitsOut)]
		ifFalse:
			["Compress to fewer bits by truncation"
			d = 0 ifTrue: [^ sourcePixel].  "no compression"
			sourcePixel = 0 ifTrue: [^ sourcePixel].  "always map 0 (transparent) to 0"
			d _ nBitsIn - nBitsOut.
			mask _ (1 << nBitsOut) - 1.  "Transfer mask"
			srcPix _ sourcePixel >> d.
			destPix _ srcPix bitAnd: mask.
			mask _ mask << nBitsOut.
			srcPix _ srcPix >> d.
			destPix _ destPix + (srcPix bitAnd: mask)
					+ (srcPix >> d bitAnd: mask << nBitsOut).
			destPix = 0 ifTrue: [^ 1].  "Dont fall into transparent by truncation"
			^ destPix]
</details>

#### WarpBlt>>#deltaFrom: x1 to: x2 nSteps: n

Utility routine for computing Warp increments. x1 is starting pixel, x2 is ending pixel; assumes n >= 1


<details>
	<summary>See more</summary>
	
	deltaFrom: x1 to: x2 nSteps: n
	"Utility routine for computing Warp increments.
	x1 is starting pixel, x2 is ending pixel;  assumes n >= 1"
	| fixedPtOne |
	fixedPtOne _ 16384.  "1.0 in fixed-pt representation"
	x2 > x1
		ifTrue: [^ x2 - x1 + fixedPtOne // (n+1) + 1]
		ifFalse: [x2 = x1 ifTrue: [^ 0].
				^ 0 - (x1 - x2 + fixedPtOne // (n+1) + 1)]
</details>

#### WarpBlt>>#startFrom: x1 to: x2 offset: sumOfDeltas

Utility routine for computing Warp increments.


<details>
	<summary>See more</summary>
	
	startFrom: x1 to: x2 offset: sumOfDeltas
	"Utility routine for computing Warp increments."
	x2 >= x1
		ifTrue: [^ x1]
		ifFalse: [^ x2 - sumOfDeltas]
</details>

#### WarpBlt>>#sourceForm: srcForm destRect: dstRectangle

Set up a WarpBlt from the entire source Form to the given destination rectangle.


<details>
	<summary>See more</summary>
	
	sourceForm: srcForm destRect: dstRectangle
	"Set up a WarpBlt from the entire source Form to the given destination rectangle."

	| w h |
	sourceForm _ srcForm.
	sourceX _ sourceY _ 0.
	destX _ dstRectangle left.
	destY _ dstRectangle top.
	width _ dstRectangle width.
	height _ dstRectangle height.
	w _ 16384 * (srcForm width - 1).
	h _ 16384 * (srcForm height - 1).
	p1x _ 0.
	p2x _ 0.
	p3x _ w.
	p4x _ w.
	p1y _ 0.
	p2y _ h.
	p3y _ h.
	p4y _ 0.
	p1z _ p2z _ p3z _ p4z _ 16384.  "z-warp ignored for now"

</details>

#### WarpBlt>>#warpBits

Move those pixels!


<details>
	<summary>See more</summary>
	
	warpBits
	"Move those pixels!"

	self warpBitsSmoothing: cellSize
		sourceMap: (sourceForm colormapIfNeededForDepth: 32).

</details>

#### WarpBlt>>#cellSize

<details>
	<summary>See more</summary>
	
	cellSize
	^ cellSize
</details>

#### WarpBlt>>#cellSize: s

Set the number of samples used for averaging


<details>
	<summary>See more</summary>
	
	cellSize: s
	"Set the number of samples used for averaging"
	cellSize := s.
	cellSize = 1 ifTrue: [^ self].
	"Install the colorMap to used for mapping the averaged RGBA 32bit pixels to the
	destination depth. Note that we need to install the 32->32 color map explicitly because
	the VM will substitute a colorMap derived from sourceForm->destForm mapping which
	is just plain wrong for <32 source and 32bit dest depth"
	(destForm depth = 32 and: [sourceForm notNil] and: [sourceForm depth < 32])
		ifTrue:[colorMap := ColorMap shifts: #(0 0 0 0) masks:#(16rFF0000 16rFF00 16rFF 16rFF000000) colors: nil]
		ifFalse:[colorMap := Color colorMapIfNeededFrom: 32 to: destForm depth].

</details>

#### WarpBlt>>#sourceQuad: pts destRect: aRectangle

<details>
	<summary>See more</summary>
	
	sourceQuad: pts destRect: aRectangle
	| fixedPt1 |
	sourceX _ sourceY _ 0.
	self destRect: aRectangle.
	fixedPt1 _ (pts at: 1) x isInteger ifTrue: [16384] ifFalse: [16384.0].
	p1x _ (pts at: 1) x * fixedPt1.
	p2x _ (pts at: 2) x * fixedPt1.
	p3x _ (pts at: 3) x * fixedPt1.
	p4x _ (pts at: 4) x * fixedPt1.
	p1y _ (pts at: 1) y * fixedPt1.
	p2y _ (pts at: 2) y * fixedPt1.
	p3y _ (pts at: 3) y * fixedPt1.
	p4y _ (pts at: 4) y * fixedPt1.
	p1z _ p2z _ p3z _ p4z _ 16384.  "z-warp ignored for now"

</details>

#### WarpBlt>>#warpBitsSmoothing: n sourceMap: sourceMap

<details>
	<summary>See more</summary>
	
	warpBitsSmoothing: n sourceMap: sourceMap
	| deltaP12 deltaP43 pA pB deltaPAB sp fixedPtOne picker poker pix nSteps |
	<primitive: 'primitiveWarpBits' module: 'BitBltPlugin'>

	(width < 1) | (height < 1) ifTrue: [^ self].
	fixedPtOne _ 16384.  "1.0 in fixed-pt representation"
	n > 1 ifTrue:
		[(destForm depth < 16 and: [colorMap == nil])
			ifTrue: ["color map is required to smooth non-RGB dest"
					^ self primitiveFail].
		pix _ Array new: n*n].

	nSteps _ height-1 max: 1.
	deltaP12 _ (self deltaFrom: p1x to: p2x nSteps: nSteps)
			@ (self deltaFrom: p1y to: p2y nSteps: nSteps).
	pA _ (self startFrom: p1x to: p2x offset: nSteps*deltaP12 x)
		@ (self startFrom: p1y to: p2y offset: nSteps*deltaP12 y).
	deltaP43 _ (self deltaFrom: p4x to: p3x nSteps: nSteps)
			@ (self deltaFrom: p4y to: p3y nSteps: nSteps).
	pB _ (self startFrom: p4x to: p3x offset: nSteps*deltaP43 x)
		@ (self startFrom: p4y to: p3y offset: nSteps*deltaP43 y).

	picker _ BitBlt bitPeekerFromForm: sourceForm.
	poker _ BitBlt bitPokerToForm: destForm.
	poker clipRect: self clipRect.
	nSteps _ width-1 max: 1.
	destY to: destY+height-1 do:
		[:y |
		deltaPAB _ (self deltaFrom: pA x to: pB x nSteps: nSteps)
				@ (self deltaFrom: pA y to: pB y nSteps: nSteps).
		sp _ (self startFrom: pA x to: pB x offset: nSteps*deltaPAB x)
			@ (self startFrom: pA y to: pB y offset: nSteps*deltaPAB x).
		destX to: destX+width-1 do:
			[:x | 
			n = 1
			ifTrue:
				[poker pixelAt: x@y
						put: (picker pixelAt: sp // fixedPtOne asPoint)]
			ifFalse:
				[0 to: n-1 do:
					[:dx | 0 to: n-1 do:
						[:dy |
						pix at: dx*n+dy+1 put:
								(picker pixelAt: sp
									+ (deltaPAB*dx//n)
									+ (deltaP12*dy//n)
										// fixedPtOne asPoint)]].
				poker pixelAt: x@y put: (self mixPix: pix
										sourceMap: sourceMap
										destMap: colorMap)].
			sp _ sp + deltaPAB].
		pA _ pA + deltaP12.
		pB _ pB + deltaP43]
</details>

#### WarpBlt>>#mixPix: pix sourceMap: sourceMap destMap: destMap

Average the pixels in array pix to produce a destination pixel. First average the RGB values either from the pixels directly, or as supplied in the sourceMap. Then return either the resulting RGB value directly, or use it to index the destination color map.


<details>
	<summary>See more</summary>
	
	mixPix: pix sourceMap: sourceMap destMap: destMap
	"Average the pixels in array pix to produce a destination pixel.
	First average the RGB values either from the pixels directly,
	or as supplied in the sourceMap.  Then return either the resulting
	RGB value directly, or use it to index the destination color map." 
	| r g b rgb nPix bitsPerColor d |
	nPix _ pix size.
	r _ 0. g _ 0. b _ 0.
	1 to: nPix do:
		[:i |   "Sum R, G, B values for each pixel"
		rgb _ sourceForm depth <= 8
				ifTrue: [sourceMap at: (pix at: i) + 1]
				ifFalse: [sourceForm depth = 32
						ifTrue: [pix at: i]
						ifFalse: [self rgbMap: (pix at: i) from: 5 to: 8]].
		r _ r + ((rgb bitShift: -16) bitAnd: 16rFF).
		g _ g + ((rgb bitShift: -8) bitAnd: 16rFF).
		b _ b + ((rgb bitShift: 0) bitAnd: 16rFF)].
	destMap
			ifNil: [
				bitsPerColor _ 3.  "just in case eg depth <= 8 and no map"
				destForm depth = 16 ifTrue: [bitsPerColor _ 5].
				destForm depth = 32 ifTrue: [bitsPerColor _ 8]]
			ifNotNil: [
				destMap size = 512 ifTrue: [bitsPerColor _ 3].
				destMap size = 4096 ifTrue: [bitsPerColor _ 4].
				destMap size = 32768 ifTrue: [bitsPerColor _ 5]].
	d _ bitsPerColor - 8.
	rgb _ ((r // nPix bitShift: d) bitShift: bitsPerColor*2)
		+ ((g // nPix bitShift: d) bitShift: bitsPerColor)
		+ ((b // nPix bitShift: d) bitShift: 0).
	^destMap
		ifNil: [rgb]
		ifNotNil: [destMap at: rgb+1]
</details>

