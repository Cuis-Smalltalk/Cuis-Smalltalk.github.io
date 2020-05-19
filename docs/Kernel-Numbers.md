## BoxedFloat64

My instances hold 64-bit Floats in heap objects. This is the only representation on 32-bit systems. But on 64-bit systems SmallFloat64 holds a subset of the full 64-bit double-precision range in immediate objects.

### Methods
#### BoxedFloat64>>#>= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is greater than or equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	>= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^ aNumber adaptToFloat: self andSend: #>=
</details>

#### BoxedFloat64>>#primSqrt

Answer the square root of the receiver. Optional. See Object documentation whatIsAPrimitive. Note: -0.0 primSqrt -0.0 sqrt both evaluate to -0.0


<details>
	<summary>See more</summary>
	
	primSqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive.
	Note: 
	-0.0 primSqrt 
	-0.0 sqrt
	both evaluate to -0.0
	"

	<primitive: 55>
	^Float nan
</details>

#### BoxedFloat64>>#sign

Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.


<details>
	<summary>See more</summary>
	
	sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self isNaN ifTrue: [ self error: 'Can not handle Not-a-Number' ].
	^super sign
</details>

#### BoxedFloat64>>#truncated

Answer with a SmallInteger equal to the value of the receiver without its fractional part. The primitive fails if the truncated value cannot be represented as a SmallInteger. In that case, the code below will compute a LargeInteger truncated value. Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]
</details>

#### BoxedFloat64>>#isInfinite

Return true if the receiver is positive or negative infinity.


<details>
	<summary>See more</summary>
	
	isInfinite
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [self = NegativeInfinity]
</details>

#### BoxedFloat64>>#= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is equal to the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=
</details>

#### BoxedFloat64>>#isFinite

simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity


<details>
	<summary>See more</summary>
	
	isFinite
	"simple, byte-order independent test for rejecting Not-a-Number and (Negative)Infinity"

	^(self - self) = 0.0
</details>

#### BoxedFloat64>>#< aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is less than the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	< aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^ aNumber adaptToFloat: self andSend: #<
</details>

#### BoxedFloat64>>#sin

Answer the sine of the receiver taken as an angle in radians. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 56>
	^ self sinNonPrimitive
</details>

#### BoxedFloat64>>#timesTwoPower: anInteger

Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	timesTwoPower: anInteger
	"Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>
	anInteger isInteger ifFalse: [ ^DomainError signal: '#timesTwoPower: only defined for Integer argument.'].
	self isFinite ifFalse: [^self].
	self isZero ifTrue: [^self].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue: [
			| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue: [
				"self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)
</details>

#### BoxedFloat64>>#raisedTo: exponent

Answer the receiver raised to aNumber.


<details>
	<summary>See more</summary>
	
	raisedTo: exponent
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedTo: exponent
</details>

#### BoxedFloat64>>#arcTan

Answer the angle in radians. Optional. See Object documentation whatIsAPrimitive. Note: If the purpose is to recover the angle of some vector, prefer #arcTan: See, for example, Complex>>#argument


<details>
	<summary>See more</summary>
	
	arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 57>
	^ self arcTanNonPrimitive
</details>

#### BoxedFloat64>>#/ aNumber

Primitive. Answer the result of dividing receiver by aNumber. Fail if the argument is not a Float. Essential. See Object clas >> whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 50>
	aNumber isZero ifTrue: [^ ZeroDivide new signalReceiver: self selector: #/ argument: aNumber].
	^ aNumber adaptToFloat: self andSend: #/
</details>

#### BoxedFloat64>>#ln

Answer the natural logarithm of the receiver. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 58>
	^ self lnNonPrimitive
</details>

#### BoxedFloat64>>#~= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is not equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	~= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^super ~= aNumber
</details>

#### BoxedFloat64>>#exponent

Primitive. Consider the receiver to be represented as a power of two multiplied by a mantissa between one and two or between -1 and -2 (#significand). Answer with the SmallInteger to whose power two is raised. Optional. See Object documentation whatIsAPrimitive. Note: invalid for infinities, NaN and zero. See also #signPart, #significandAsInteger and #exponentPart


<details>
	<summary>See more</summary>
	
	exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	| positive |
	<primitive: 53>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent
</details>

#### BoxedFloat64>>#isDenormalized

Denormalized numbers are only represented as BoxedFloat64


<details>
	<summary>See more</summary>
	
	isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		exponentBits = 0 and: [mantissaBits ~=0]]
</details>

#### BoxedFloat64>>#> aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is greater than the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	> aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^ aNumber adaptToFloat: self andSend: #>
</details>

#### BoxedFloat64>>#fractionPart

Primitive. Answer a Float whose value is the difference between the receiver and the receiver's asInteger value. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	fractionPart
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 52>
	^self - self truncated asFloat
</details>

#### BoxedFloat64>>#exp

Answer e raised to the receiver power. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	exp
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 59>
	^ self expNonPrimitive
</details>

#### BoxedFloat64>>#argument

Compatibility with Complex numbers.


<details>
	<summary>See more</summary>
	
	argument
	"Compatibility with Complex numbers."
	self isNaN ifTrue: [^self].
	^super argument
</details>

#### BoxedFloat64>>#storeOn: aStream base: base

Print the Number exactly so it can be interpreted back unchanged


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: base 

	self isFinite
		ifTrue: [ super storeOn: aStream base: base ]
		ifFalse: [ self isNaN
			ifTrue: [aStream nextPutAll: 'Float nan']
			ifFalse: [self > 0.0
					ifTrue: [aStream nextPutAll: 'Float infinity']
					ifFalse: [aStream nextPutAll: 'Float infinity negated']]]
</details>

#### BoxedFloat64>>#+ aNumber

Primitive. Answer the sum of the receiver and aNumber. Essential. Fail if the argument is not a Float. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ aNumber adaptToFloat: self andSend: #+
</details>

#### BoxedFloat64>>#- aNumber

Primitive. Answer the difference between the receiver and aNumber. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	- aNumber 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ aNumber adaptToFloat: self andSend: #-
</details>

#### BoxedFloat64>>#isNaN

simple, byte-order independent test for Not-a-Number


<details>
	<summary>See more</summary>
	
	isNaN
	"simple, byte-order independent test for Not-a-Number"

	^ self ~= self
</details>

#### BoxedFloat64>>#arg

Compatibility with Complex numbers.


<details>
	<summary>See more</summary>
	
	arg
	"Compatibility with Complex numbers."
	self isNaN ifTrue: [^self].
	^super arg
</details>

#### BoxedFloat64>>#<= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is less than or equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	<= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andSend: #<=
</details>

#### BoxedFloat64>>#* aNumber

Primitive. Answer the result of multiplying the receiver by aNumber. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	* aNumber 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ aNumber adaptToFloat: self andSend: #*
</details>

#### BoxedFloat64>>#raisedToInteger: exponent

Answer the receiver raised to aNumber.


<details>
	<summary>See more</summary>
	
	raisedToInteger: exponent
	"Answer the receiver raised to aNumber."

	self isNaN ifTrue: [ ^self ].
	^super raisedToInteger: exponent
</details>

## Float

A note About Floating Point numbers and Floating Point Arithmetic. The following is not specific to Cuis or Smalltalk at all. This is about the properties of Float numbers in any computer implementation. If you haven't done so already, read https://en.wikipedia.org/wiki/Floating-point_arithmetic But if you find the Wikipedia article too detailed, or hard to read, then try http://fabiensanglard.net/floating_point_visually_explained/ (get past "How Floating Point are usually explained" and read "A different way to explain..."). Other great reads are: "Why don't my numbers add up?": http://floating-point-gui.de/ and "What Every Computer Scientist Should Know About Floating-Point Arithmetic": http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html and also maybe "Comparing floating point numbers" https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/ Now that you read them, and we are on the same boat, some further comments (from jmv): Floats are (conceptually) approximate real numbers. That's why trig and other trascendental functions always answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could hopefully be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality. When doing mixed operations with Floats and Fractions, Cuis, as most other Smalltalks, converts all values to Floats. Some other systems, including Pharo Smalltalk, Scheme and Lisp have two rules: when the answer is a Number, they convert to Float. But when the answer is a boolean (#<, #=, #<=, etc.) they convert to Fraction. We think this is a mistake. There should never be implicit conversions from Float to Fraction. Fractions are to hold exact values, and people expect Fractions to be exact. On the other hand, Floats are to hold approximations (and people should be aware of that!). But an implicit conversion from Float to Fraction would give a Fraction that should not be considered an exact value (the value comes from an inexact Float), but that knowledge is lost, as it is an instance of Fraction. If you want exact arithmetic, usual mathematical properties (like transitivity of equality), can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead. Avoid trascendental functions and never convert to Float. In any case, most numeric computation is done on Float numbers. There are good reasons for that. One is that in most cases we don't need an exact answer. And in many cases we can't really have it: the inputs to algorithms already have a limited precision, or they use trascendental functions. And even when exact arithmetic possible, if we are doing sound synthesis, 24 bits of resolution is enough. For image processing and graphics, the result is never more than 16 bits per channel. So, these fields don't really need 64 bit Doubles. 32 bit Floats are enough. Other fields do need 64 bit Doubles, like physics simulations and geometry. Games usually prefer special, faster 32 bit Float operations in GPUs that have greater errors but are faster. There are some things that can be done to increase the confidence you can have on Float results. One is to do an error propagation analysis on the code you are running. This is not easy, but it is done for any widely used numerical method. Then, you can know real bounds and/or estimates of the errors made. So, understanding your inputs and your algorithms (for example error propagation, condition number, numeric stability), and using Float number if appropriate, is the usual advice. Perhaps you have heard about "interval arithmetic". It is a bit better than simple Float, but doesn't really fix the problems. The ultimate solution is to do Monte Carlo analysis, with random perturbation of inputs. After the Monte Carlo run, it is needed to do statistical analysis of possible correlations between the distributions of the random noise added to imputs and the result of the algorithm. Additional food for thought: http://www.cs.berkeley.edu/~wkahan/Mindless.pdf . According to this, doing Monte Carlo as described above attacks a slightly different problem. This might be yet another reason (besides performance) to try something like the next paragraph. I (jmv) came up with it, and I don't really know if it has been described and or tried before or not. Mhhh. Maybe a defensive publication is in order. A possibility that could be a practical solution, being much cheaper than Monte Carlo, but better than interval arithmetic, is to represent each value by 2 Floats: an estimation of the real value (i.e. an estimation of the mean value of the distribution of the corresponding Monte Carlo result), and an estimation of the error (i.e. an estimation of the standard deviation of the corresponding Monte Carlo result). Or perhaps even 3 of them. In addition to the estimation of the real value and an estimation of the error, we could add a hard bound on the error. In many cases it will be useless, because the error can not really be bound. But in those cases where it is possible to bound it, applications could really know about the quality of computed values. ======================================================================= My instances represent IEEE 754 floating-point double-precision numbers. They have about 16 decimal digits of accuracy and their range is between plus and minus 10^307. Some valid examples are: 8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12 Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point. It is actually possible to specify a radix for Float constants. This is great for teaching about numbers, but may be confusing to the average reader: 3r20.2 --> 6.66666666666667 8r20.2 --> 16.25 If you don't have access to the definition of IEEE754, you can figure out what is going on by printing various simple values in Float hex. It may help you to know that the basic format is... sign 1 bit exponent 11 bits with bias of 1023 (16r3FF), substracted to produce an actual exponent in the range -1022 .. +1023 - 16r000: significand = 0: Float zero significand ~= 0: Denormal number (actual exponent is -1022, not -1023. No implicit leading '1' bit in mantissa) - 16r7FF: significand = 0: Infinity significand ~= 0: Not A Number (NaN) representation mantissa 53 bits, but only 52 are stored (20 in the first word, 32 in the second). This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE 754 omits this redundant bit to gain an extra bit of precision instead. People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND. The single-precision format is... sign 1 bit exponent 8 bits with bias of 127 (16r7F, substracted to produce an actual exponent in the range -126 .. +127 - 16r00: significand = 0: Float zero significand ~= 0: Denormal number (actual exponent is -126, not -127. No implicit leading '1' bit in mantissa) - 16rFF: significand = 0: Infinity significand ~= 0: Not A Number (NaN) representation mantissa 24 bits, but only 23 are stored This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:. You might also check https://en.wikipedia.org/wiki/IEEE_754_revision Other great reads (covering broader but interesting issues): https://randomascii.wordpress.com/2013/07/16/floating-point-determinism/ http://www.ima.umn.edu/2010-2011/W1.10-14.11/activities/Leeser-Miriam/Leeser-GPU-IMA-Jan2011.pdf

### Methods
#### Float>>#significandAsInteger

The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate. See #mantissaPart


<details>
	<summary>See more</summary>
	
	significandAsInteger
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	See #mantissaPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart
</details>

#### Float>>#withNegativeSign

Same as super, but handle the subtle case of Float negativeZero


<details>
	<summary>See more</summary>
	
	withNegativeSign
	"Same as super, but handle the subtle case of Float negativeZero"
	
	self isZero ifTrue: [^self class negativeZero].  
	^super withNegativeSign
</details>

#### Float>>#exponentBits

Actual bits for the exponent part of the floating point representation. Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.


<details>
	<summary>See more</summary>
	
	exponentBits
	"
	Actual bits for the exponent part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | exponentBits ]
</details>

#### Float>>#smoothStep

A Function that is x <= -1 -> 1 -1 < x < 1 -> smooth differentiable transition from 1 to 0 1 <= x -> 0 It is a 'smooth' version of an #negative Has countless applications. For example in image and signal processing, but also in other fields.


<details>
	<summary>See more</summary>
	
	smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->			smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5
</details>

#### Float>>#at: index put: value

Avoid primitive in Object>>at:put:


<details>
	<summary>See more</summary>
	
	at: index put: value 
	"Avoid primitive in Object>>at:put:"

	^self basicAt: index put: value
</details>

#### Float>>#expNonPrimitive

Answer e raised to the receiver power.


<details>
	<summary>See more</summary>
	
	expNonPrimitive
	"Answer e raised to the receiver power."

	| base fract correction delta div |

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base _ E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract _ self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction _ 1.0 + fract.
	delta _ fract * fract / 2.0.
	div _ 2.0.
	[delta >= base ulp] whileTrue: [
		correction _ correction + delta.
		div _ div + 1.0.
		delta _ delta * fract / div].
	correction _ correction + delta.
	^ base * correction
</details>

#### Float>>#sign

Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0. Handle IEEE754 negative-zero by reporting a sign of -1 Warning! This makes Float negativeZero the only number in the system such that x sign negated = x negated sign evaluates to false! This precludes the simpler implementation in #signPart 0.0 sign -> 0 0.0 signPart -> 1 -0.0 sign -> -1 -0.0 signPart -> -1


<details>
	<summary>See more</summary>
	
	sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
	Handle IEEE754 negative-zero by reporting a sign of -1
	Warning! This makes Float negativeZero the only number in the system such that
		x sign negated = x negated sign
	evaluates to false!
	This precludes the simpler implementation in #signPart
	0.0 sign  ->  0
	0.0 signPart   ->  1
	-0.0 sign   ->  -1
	-0.0 signPart   ->  -1
	"

	"Negative number or -0.0"
	self signBit = 1 ifTrue: [ ^ -1 ].

	"Zero"
	self = 0.0 ifTrue: [ ^ 0 ].

	"Positive number otherwise"
	^ 1
</details>

#### Float>>#sinNonPrimitive

Answer the sine of the receiver taken as an angle in radians.


<details>
	<summary>See more</summary>
	
	sinNonPrimitive
	"Answer the sine of the receiver taken as an angle in radians."

	| sum delta self2 i |

	"Taylor series"
	"normalize to the range [0..Pi/2]"
	self < 0.0 ifTrue: [^ (0.0 - ((0.0 - self) sin))].
	self > Twopi ifTrue: [^ (self \\ Twopi) sin].
	self > Pi ifTrue: [^ (0.0 - (self - Pi) sin)].
	self > Halfpi ifTrue: [^ (Pi - self) sin].

	"unroll loop to avoid use of abs"
	sum _ delta _ self.
	self2 _ 0.0 - (self * self).
	i _ 2.0.
	[delta >= sum ulp] whileTrue: [
		"once"
		delta _ (delta * self2) / (i * (i + 1.0)).
		i _ i + 2.0.
		sum _ sum + delta.
		"twice"
		delta _ (delta * self2) / (i * (i + 1.0)).
		i _ i + 2.0.
		sum _ sum + delta].
	^ sum
</details>

#### Float>>#absPrintOn: aStream base: base digitCount: digitCount

Print me in the given base, using digitCount significant figures.


<details>
	<summary>See more</summary>
	
	absPrintOn: aStream base: base digitCount: digitCount 
	"Print me in the given base, using digitCount significant figures."

	| fuzz x exp q fBase scale logScale xi |
	self isInfinite ifTrue: [^ aStream nextPutAll: 'Inf'].
	fBase := base asFloat.
	"x is myself normalized to [1.0, fBase), exp is my exponent"
	exp := 
		self < 1.0
			ifTrue: [self reciprocalFloorLog: fBase]
			ifFalse: [self floorLog: fBase].
	scale := 1.0.
	logScale := 0.
	[(x := fBase raisedTo: (exp + logScale)) = 0]
		whileTrue:
			[scale := scale * fBase.
			logScale := logScale + 1].
	x := self * scale / x.
	fuzz := fBase raisedTo: 1 - digitCount.
	"round the last digit to be printed"
	x := 0.5 * fuzz + x.
	x >= fBase
		ifTrue: 
			["check if rounding has unnormalized x"
			x := x / fBase.
			exp := exp + 1].
	(exp < 6 and: [exp > -4])
		ifTrue: 
			["decimal notation"
			q := 0.
			exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000'
at: i)]]]
		ifFalse: 
			["scientific notation"
			q := exp.
			exp := 0].
	[x >= fuzz]
		whileTrue: 
			["use fuzz to track significance"
			xi := x asInteger.
			aStream nextPut: (Character digitValue: xi).
			x := x - xi asFloat * fBase.
			fuzz := fuzz * fBase.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	[exp >= -1]
		whileTrue: 
			[aStream nextPut: $0.
			exp := exp - 1.
			exp = -1 ifTrue: [aStream nextPut: $.]].
	q ~= 0
		ifTrue: 
			[aStream nextPut: $e.
			q printOn: aStream]
</details>

#### Float>>#nthRoot: aPositiveInteger

Answer the nth root of the receiver.


<details>
	<summary>See more</summary>
	
	nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger even
				ifTrue: [ NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger ]
				ifFalse: [ (self negated nthRoot: aPositiveInteger) negated ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]
</details>

#### Float>>#basicAt: index put: value

Primitive. Assumes receiver is indexable. Store the second argument value in the indexable element of the receiver indicated by index. Fail if the index is not an Integer or is out of bounds. Or fail if the value is not of the right type for this kind of collection. Answer the value that was stored. Essential. Do not override in a subclass. See Object documentation whatIsAPrimitive. This version of basicAt: is specifically for floats, answering the most significant word for index 1 and the least significant word for index 2. This alows the VM to store floats in whatever order it chooses while it appears to the image that they are always in big-endian/PowerPC order.


<details>
	<summary>See more</summary>
	
	basicAt: index put: value
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive.

	This version of basicAt: is specifically for floats, answering the most significant
	word for index 1 and the least significant word for index 2.  This alows the VM
	to store floats in whatever order it chooses while it appears to the image that
	they are always in big-endian/PowerPC order."

	<primitive: 39>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index put: value].
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Float>>#isLiteral

There is no literal representation of NaN. However, there are literal representations of Infinity, like 1.0e1000. But since they are not able to print properly, only case of finite Float is considered.


<details>
	<summary>See more</summary>
	
	isLiteral
	"There is no literal representation of NaN.
	However, there are literal representations of Infinity, like 1.0e1000.
	But since they are not able to print properly, only case of finite Float is considered."
	
	^self isFinite
</details>

#### Float>>#adaptToInteger: rcvr andSend: selector

If I am involved in arithmetic with an Integer, convert it to a Float.


<details>
	<summary>See more</summary>
	
	adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Float."
	^ rcvr asFloat perform: selector with: self
</details>

#### Float>>#isInfinite

Infinities are only represented as BoxedFloat64


<details>
	<summary>See more</summary>
	
	isInfinite
	"Infinities are only represented as BoxedFloat64"

	^ false

</details>

#### Float>>#arCosh

Answer receiver's area hyperbolic cosine. That is the inverse function of cosh.


<details>
	<summary>See more</summary>
	
	arCosh
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh."

	self < 1 
		ifTrue: [^ Float nan].
	^self + 1 = self 
		ifTrue: [self abs ln + 2 ln]
		ifFalse: [((self squared - 1) sqrt + self) ln]
</details>

#### Float>>#literalEqual: aFloat

Two float literals can be replaced by a single one only if their representation have the same bits. For example, zero and negativeZero are equal, but not literally equal.


<details>
	<summary>See more</summary>
	
	literalEqual: aFloat
	"Two float literals can be replaced by a single one only if their representation have the same bits.
	For example, zero and negativeZero are equal, but not literally equal."

	^self class == aFloat class and: [(self at: 1) = (aFloat at: 1) and: [(self at: 2) = (aFloat at: 2)]]
</details>

#### Float>>#asFloat

Answer the receiver itself.


<details>
	<summary>See more</summary>
	
	asFloat
	"Answer the receiver itself."

	^self
</details>

#### Float>>#radiansToDegrees

Answer the receiver in degrees. Assumes the receiver is in radians.


<details>
	<summary>See more</summary>
	
	radiansToDegrees
	"Answer the receiver in degrees. Assumes the receiver is in radians."

	^self / RadiansPerDegree
</details>

#### Float>>#arcTan: denominator

Answer the angle in radians. Implementation note: use sign in order to catch cases of negativeZero


<details>
	<summary>See more</summary>
	
	arcTan: denominator
	"Answer the angle in radians.
	Implementation note: use sign in order to catch cases of negativeZero"

	self isNaN ifTrue: [ ^self ].
	denominator isNaN ifTrue: [ ^denominator class nan ].	"if Complex, answer complex nan"
	^self = 0.0
		ifTrue: [denominator sign >= 0
			ifTrue: [ 0.0 ]
			ifFalse: [ self sign >= 0
				ifTrue: [ Pi ]
				ifFalse: [ Pi negated ]]]
		ifFalse: [denominator = 0.0
			ifTrue: [self > 0.0
				ifTrue: [ Halfpi ]
				ifFalse: [ Halfpi negated ]]
			ifFalse: [denominator > 0.0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0.0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]
</details>

#### Float>>#log: aNumber

Answer the log base aNumber of the receiver.


<details>
	<summary>See more</summary>
	
	log: aNumber
	"Answer the log base aNumber of the receiver."

	^ self ln / aNumber ln
</details>

#### Float>>#partBits: aThreeArgumentBlock

Extract the bits for Sign, Mantissa and Exponent parts of the floating point representation. Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not subtract exponent bias. Do nothing with infinites and NaN.


<details>
	<summary>See more</summary>
	
	partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the floating point representation.
	Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not subtract exponent bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	| signBit exponentBits mantissaBits leastSignificativeWord mostSignificativeWord |

	mostSignificativeWord _ self basicAt: 1.
	leastSignificativeWord _ self basicAt: 2.
	signBit _ mostSignificativeWord bitShift: -31 .
	exponentBits _ (mostSignificativeWord bitShift: -20 ) bitAnd: 16r7FF.
	mantissaBits _ ((mostSignificativeWord bitAnd: 16r000FFFFF) bitShift: 32) + leastSignificativeWord.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits
</details>

#### Float>>#degreeCos

If finite, allow for special values such as cos(60 degrees) = 1/2


<details>
	<summary>See more</summary>
	
	degreeCos
	"If finite, allow for special values such as cos(60 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeCos].
	^self degreesToRadians cos
</details>

#### Float>>#partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock

Float pi hex print Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ] 0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ] For 0.0, exponent will be the minimum possible, i.e. -1023, and mantissa will be 0.


<details>
	<summary>See more</summary>
	
	partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"
	| sign exponent mantissa |

	^ self partBits: [ :signBit :exponentBits :mantissaBits |

		"Extract the sign"
		sign _ signBit = 0 ifTrue: [1] ifFalse: [-1].

		"Special cases: infinites and NaN"
		exponentBits = 16r7FF ifTrue: [
			^mantissaBits = 0
				ifTrue: [ infinityBlock valueWithPossibleArgument: self ]
				ifFalse: [ nanBlock valueWithPossibleArgument: self and: mantissaBits ]].

		"Unbias exponent: 16r3FF is bias"
		exponent _ exponentBits - 16r3FF.
	
		exponentBits ~= 0
			ifTrue: [
				"Add back implicit leading 1 in fraction."
				mantissa _ 16r0010000000000000 bitOr: mantissaBits ]
			ifFalse: [
				"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa."
				mantissa _ mantissaBits.
				exponent _ exponent + 1 ].

		"Evaluate the block"
		aThreeArgumentBlock value: sign value: exponent value: mantissa
	]
</details>

#### Float>>#printOn: aStream base: base

Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:


<details>
	<summary>See more</summary>
	
	printOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self negated absPrintOn: aStream base: base]]
</details>

#### Float>>#log

Answer the base 10 logarithm of the receiver.


<details>
	<summary>See more</summary>
	
	log
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10
</details>

#### Float>>#mantissaPart

Equivalent to #significandAsInteger.


<details>
	<summary>See more</summary>
	
	mantissaPart
	"Equivalent to #significandAsInteger."
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	Does not include the sign.
	See #exponentPart and #signPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | mantissa ]
</details>

#### Float>>#isDenormalized

Denormalized numbers are only represented as BoxedFloat64


<details>
	<summary>See more</summary>
	
	isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ false
</details>

#### Float>>#isFloatOrFloatComplex

Overridden to return true in Float and Complex


<details>
	<summary>See more</summary>
	
	isFloatOrFloatComplex
	^ true
</details>

#### Float>>#ulp

Answer the unit of least precision of the receiver. Follow John Harrison's definition as described at https://en.wikipedia.org/wiki/Unit_in_the_last_place


<details>
	<summary>See more</summary>
	
	ulp
	"Answer the unit of least precision of the receiver.
	Follow John Harrison's definition as described at
	https://en.wikipedia.org/wiki/Unit_in_the_last_place"
	
	self isFinite ifFalse: [^self abs].
	self isZero ifTrue: [^0.0 nextAwayFromZero].
	^ (self - self nextTowardsZero) abs
</details>

#### Float>>#absPrintOn: aStream base: base

In Cuis, print Floats with enough digits to be able to recover later exactly the same Float.


<details>
	<summary>See more</summary>
	
	absPrintOn: aStream base: base
	"In Cuis, print Floats with enough digits to be able to recover later exactly the same Float."

	self absPrintExactlyOn: aStream base: base
</details>

#### Float>>#cosh

Answer receivers hyperbolic cosine.


<details>
	<summary>See more</summary>
	
	cosh
	"Answer receivers hyperbolic cosine."
	
	| ex |
	ex := self abs exp.
	^(ex + ex reciprocal) / 2
</details>

#### Float>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^ 8
</details>

#### Float>>#arcCos

Answer the angle in radians.


<details>
	<summary>See more</summary>
	
	arcCos
	"Answer the angle in radians."

	^ Halfpi - self arcSin
</details>

#### Float>>#asIEEE32BitPrecisionFloat

Answer a 64-bit Float, but using only the precision of a 32-bit Float, as used in FloatArray and 'float' in the C world.


<details>
	<summary>See more</summary>
	
	asIEEE32BitPrecisionFloat
	"Answer a 64-bit Float, but using only the precision of a 32-bit Float, as used in FloatArray and 'float' in the C world."
	
	^ Float fromIEEE32Bit: self asIEEE32BitWord
</details>

#### Float>>#lnNonPrimitive

Answer the natural logarithm of the receiver. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	lnNonPrimitive
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum |

	"Taylor series"
	self <= 0.0 ifTrue: [^ Float nan].

	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.

	"compute fine correction from mantinssa in Taylor series"
	"mant is in the range [0..2]"
	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.  "x <= 0"
	[delta > (n + sum) ulp] whileTrue: [
		"pass one: delta is positive"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta.
		"pass two: delta is negative"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta].

	^ n + sum

	"Float e ln 1.0"
</details>

#### Float>>#integerPart

Answer a Float whose value is the receiver's truncated value.


<details>
	<summary>See more</summary>
	
	integerPart
	"Answer a Float whose value is the receiver's truncated value."

	^self - self fractionPart
</details>

#### Float>>#sqrt

Answer the square root of the receiver. Use primitive if available, or Smalltalk code if primitive is unavailable or fails.


<details>
	<summary>See more</summary>
	
	sqrt
	"Answer the square root of the receiver. 
	 Use primitive if available, or Smalltalk code if primitive is unavailable or fails."

	| prim |
	prim _ self primSqrt.
	prim isNaN ifFalse: [ ^prim ].

	^ self sqrtNonPrimitive
</details>

#### Float>>#reciprocalFloorLog: radix

Quick computation of (self log: radix) floor, when self < 1.0. Avoids infinite recursion problems with denormalized numbers


<details>
	<summary>See more</summary>
	
	reciprocalFloorLog: radix 
	"Quick computation of (self log: radix) floor, when self < 1.0.
	Avoids infinite recursion problems with denormalized numbers"

	| adjust scale n |
	adjust _ 0.
	scale _ 1.0.
	[(n _ radix / (self * scale)) isInfinite]
		whileTrue:
			[scale _ scale * radix.
			adjust _ adjust + 1].
	^ ((n floorLog: radix) + adjust) negated
</details>

#### Float>>#hex

<details>
	<summary>See more</summary>
	
	hex
	^ String streamContents: [ :strm | | word nibble |
		1 to: 2 do: [ :i |
			word := self at: i.
			1 to: 8 do: [ :s |
				nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
				strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]]
"
(-2.0 to: 2.0) collect: [:f | f hex]
"
</details>

#### Float>>#predecessor

Answer the largest Float smaller than self


<details>
	<summary>See more</summary>
	
	predecessor
	"Answer the largest Float smaller than self"

	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self signBit = 1 ifTrue: [ "Negative or -0.0"
		^ self nextAwayFromZero ].
	self = 0.0 ifTrue: [
		^ -0.0 ].
	^ self nextTowardsZero.
</details>

#### Float>>#basicAt: index

Primitive. Assumes receiver is indexable. Answer the value of an indexable element in the receiver. Fail if the argument index is not an Integer or is out of bounds. Essential. Do not override in a subclass. See Object documentation whatIsAPrimitive. This version of basicAt: is specifically for floats, answering the most significant word for index 1 and the least significant word for index 2. This alows the VM to store floats in whatever order it chooses while it appears to the image that they are always in big-endian/PowerPC order.


<details>
	<summary>See more</summary>
	
	basicAt: index
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. Do not override in a subclass. See 
	Object documentation whatIsAPrimitive.

	This version of basicAt: is specifically for floats, answering the most significant
	word for index 1 and the least significant word for index 2.  This alows the VM
	to store floats in whatever order it chooses while it appears to the image that
	they are always in big-endian/PowerPC order."

	<primitive: 38>
	| ec |
	ec == nil ifTrue: "primitive not implemented; floats are in big-endian/PowerPC order."
		[^super basicAt: index].
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
</details>

#### Float>>#floorLog: radix

Answer the floor of the log base radix of the receiver. The result may be off by one due to rounding errors, except in base 2.


<details>
	<summary>See more</summary>
	
	floorLog: radix
	"Answer the floor of the log base radix of the receiver.
	The result may be off by one due to rounding errors, except in base 2."

	(radix = 2 and: [self > 0.0 and: [self isFinite]]) ifTrue: [^self exponent].
	^ (self log: radix) floor

</details>

#### Float>>#reciprocal

Returns the reciprocal of self. In case self is 0 the / signals ZeroDivide


<details>
	<summary>See more</summary>
	
	reciprocal
	^ 1.0 / self
</details>

#### Float>>#successor

Answer the smallest Float greater than self


<details>
	<summary>See more</summary>
	
	successor
	"Answer the smallest Float greater than self"

	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self signBit = 0 ifTrue: [
		^ self nextAwayFromZero ].
	self = -0.0 ifTrue: [
		^ 0.0 ].
	^ self nextTowardsZero.
</details>

#### Float>>#tan

Answer the tangent of the receiver taken as an angle in radians.


<details>
	<summary>See more</summary>
	
	tan
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos
</details>

#### Float>>#reciprocalLogBase2

optimized for self = 10, for use in conversion for printing


<details>
	<summary>See more</summary>
	
	reciprocalLogBase2
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [Ln2 / Ln10]
		ifFalse: [Ln2 / self ln]
</details>

#### Float>>#printOn: aStream fractionDigits: placesDesired

This implementation avoids rounding errors doue to #rounded or #roundTo: Round to a suitable integer and insert the decimal point in the appropriately between the digits.


<details>
	<summary>See more</summary>
	
	printOn: aStream fractionDigits: placesDesired
	"This implementation avoids rounding errors doue to #rounded or #roundTo:
	Round to a suitable integer and insert the decimal point in the appropriately between the digits."

	| i s scaled |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired > 0 ifFalse: [
		^self rounded printOn: aStream ].

	scaled _ self * (10 raisedTo: placesDesired).
	"If rounding could possibly print a sequence that is read back as a different float, then go the more expensive Fraction way.
	If the following line is commented, #testPrintShowingDecimalPlaces4 will fail!"
	scaled ulp > 1 ifTrue: [
		^ self asTrueFraction printOn: aStream fractionDigits: placesDesired ].
	i _ scaled rounded.
	i negative ifTrue: [
		aStream nextPut: $-.
		i _ i negated ].
	s _ i printString.
	placesDesired + 1 > s size
		ifTrue: [
			aStream nextPutAll: '0.'.
			placesDesired - s size timesRepeat: [ aStream nextPut: $0 ].
			aStream nextPutAll: s ]
		ifFalse: [
			aStream
				nextPutAll: (s copyFrom: 1 to: s size-placesDesired);
				nextPut: $.;
				nextPutAll: (s copyFrom: s size-placesDesired+1 to: s size) ]
</details>

#### Float>>#cos

Answer the cosine of the receiver taken as an angle in radians.


<details>
	<summary>See more</summary>
	
	cos
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin
</details>

#### Float>>#isFloat

Overridden to return true in Float, natch


<details>
	<summary>See more</summary>
	
	isFloat
	^ true
</details>

#### Float>>#sinh

Answer receivers hyperbolic sine


<details>
	<summary>See more</summary>
	
	sinh
	"Answer receivers hyperbolic sine"
	
	| ex |
	ex _ self abs exp.
	^ (ex - ex reciprocal) / 2 * self sign
</details>

#### Float>>#adaptToFraction: rcvr andSend: selector

If I am involved in arithmetic with a Fraction, convert it to a Float.


<details>
	<summary>See more</summary>
	
	adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a Float."
	^ rcvr asFloat perform: selector with: self
</details>

#### Float>>#abs

This is faster than using Number abs and works for negativeZero.


<details>
	<summary>See more</summary>
	
	abs
	"This is faster than using Number abs and works for negativeZero."
	self <= 0.0
		ifTrue: [^ 0.0 - self]
		ifFalse: [^ self]
</details>

#### Float>>#asFraction

<details>
	<summary>See more</summary>
	
	asFraction
	^ self asTrueFraction 
</details>

#### Float>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^false
</details>

#### Float>>#exponentPart

Exponent part of the floating point representation. Valid for any floating point number (except zeros, infinities and NaNs). Includes correction of stored exponent bits for denormals (where it acts as a label, not a real exponent).


<details>
	<summary>See more</summary>
	
	exponentPart
	"
	Exponent part of the floating point representation.
	Valid for any floating point number (except zeros, infinities and NaNs).
	Includes correction of stored exponent bits for denormals (where it acts as a label, not a real exponent).
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | exponent ]
</details>

#### Float>>#to: stop count: n do: aBlock

Quite like doing self to: stop by: (stop-self)/(n-1) do: aBlock but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced This means that 0.0 to: 1.0 by: 1.0/9.0 do: [ :each | (each@(each = 1.0)) print ] will print false for all (nine!!!) values, and 0.0 to: 1.0 by: 0.111111 do: [ :each | (each@(each = 1.0)) print ] will print false for all (ten!!!) values, but 0.0 to: 1.0 count: 10 do: [ :each | (each@(each = 1.0)) print ] will print true in the last iteration


<details>
	<summary>See more</summary>
	
	to: stop count: n do: aBlock
	"Quite like doing 
		self to: stop by: (stop-self)/(n-1) do: aBlock
	but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced
	This means that
		0.0 to: 1.0 by: 1.0/9.0 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all  (nine!!!) values, and
		0.0 to: 1.0 by: 0.111111 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all  (ten!!!) values, but
		0.0 to: 1.0 count: 10 do: [ :each | (each@(each = 1.0)) print ]
	will print true in the last iteration
	"
	self to: stop hops: n-1 do: aBlock
</details>

#### Float>>#asIEEE32BitWord

Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format. Used for conversion in FloatArrays only.


<details>
	<summary>See more</summary>
	
	asIEEE32BitWord
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format.
	Used for conversion in FloatArrays only."
	
	| word1 word2 sign mantissa exponent destWord truncatedBits mask roundToUpper |
	
	"quickly skip positive and negative zero"
	self isZero ifTrue: [^self basicAt: 1].
	
	"retrieve 64 bits of IEEE 754 double"
	word1 := self basicAt: 1.
	word2 := self basicAt: 2.
	
	"prepare sign exponent and mantissa of 32 bits float"
	sign := word1 bitAnd: 16r80000000.
	exponent := ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	mantissa := (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	truncatedBits := (word2 bitAnd: 16r1FFFFFFF).

	"We must now honour default IEEE rounding mode (round to nearest even)"
	
	"we are below gradual underflow, even if rounded to upper mantissa"
	exponent < -24 ifTrue: [^sign "this can be negative zero"].
	
	"BEWARE: rounding occurs on less than 23bits when gradual underflow"
	exponent <= 0
		ifTrue:
			[mask := 1 bitShift: exponent negated.
			mantissa := mantissa bitOr: 16r800000.
			roundToUpper := (mantissa bitAnd: mask) isZero not
				and: [truncatedBits isZero not
					or: [(mantissa bitAnd: mask - 1) isZero not
						or: [(mantissa bitAnd: mask*2) isZero not]]].
			mantissa := mantissa bitShift: exponent - 1.
			"exponent := exponent + 1"]
		ifFalse:
			[roundToUpper := (truncatedBits bitAnd: 16r10000000) isZero not
				and: [(mantissa bitAnd: 16r1) isZero not
					or: [(truncatedBits bitAnd: 16r0FFFFFFF) isZero not]]
			].
		
	"adjust mantissa and exponent due to IEEE rounding mode"
	roundToUpper
		ifTrue:
			[mantissa := mantissa + 1.
			mantissa > 16r7FFFFF
				ifTrue:
					[mantissa := 0.
					exponent := exponent+1]].

	exponent > 254 ifTrue: ["Overflow"
		exponent := 255.
		self isNaN
			ifTrue: [mantissa isZero
				ifTrue: ["BEWARE: do not convert a NaN to infinity due to truncatedBits"
					mantissa := 1]]
			ifFalse: [mantissa := 0]].
		
	"Encode the word"
	destWord := (sign bitOr: ((exponent max: 0) bitShift: 23)) bitOr: mantissa.
	^ destWord
</details>

#### Float>>#asTrueFraction

Answer a fraction that EXACTLY represents self, a double precision IEEE floating point number. By David N. Smith with significant performance improvements by Luciano Esteban Notarfrancesco. (Version of 11April97). Refactoring and simplification by jmv


<details>
	<summary>See more</summary>
	
	asTrueFraction
	" Answer a fraction that EXACTLY represents self,
	  a double precision IEEE floating point number.
	  By David N. Smith with significant performance
	  improvements by Luciano Esteban Notarfrancesco.
	  (Version of 11April97).
	Refactoring and simplification by jmv"
	
	^self
		partValues: [ :sign :exponent :mantissa | | zeroBitsCount |
			" Prepare result. If exponent is greater than mantissa size, result is an integer"
			(exponent >= 52 or: [
					zeroBitsCount _ mantissa lowBit - 1.
					exponent + zeroBitsCount >= 52 ])
				ifTrue: [
					"result is an integer number"
					 sign * mantissa bitShift: exponent - 52 ]
				ifFalse: [
					" This is the 'obvious' way. Better do Luciano's trick below:"
					"result := Fraction
						numerator: sign * mantissa
						denominator: (1 bitShift: 52 - exponent)."
					" Form the result. When exp>52, the exponent is adjusted by
					  the number of trailing zero bits in the mantissa to minimize
					  the (huge) time that could be spent in #gcd:. "
					Fraction
						numerator: (sign * (mantissa bitShift: 0 - zeroBitsCount))
						denominator: (1 bitShift: 52 - exponent - zeroBitsCount) ]
		]
		ifInfinite: [ self error: 'Cannot represent infinity as a fraction' ]
		ifNaN: [ self error: 'Cannot represent Not-a-Number as a fraction' ].
</details>

#### Float>>#signPart

The sign of the mantissa. 1 means positive number or 0.0 -1 means negative number or -0.0 See #mantissaPart and #exponentPart


<details>
	<summary>See more</summary>
	
	signPart
	"The sign of the mantissa.
	1 means positive number or 0.0
	-1 means negative number or -0.0
	See #mantissaPart and #exponentPart"
	"
	| f |
	f := -2.0.
	(f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat.
	"
	^self partValues: [ :sign :exponent :mantissa | sign ]
</details>

#### Float>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	| asInteger |
	self isFinite ifTrue: [
		asInteger _ self truncated.
		"See Integer>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
		"Very big integers will answer true when asked #= with their own conversion to Float,
		But that Float will #asInteger to a different integer. Use Float hash in those cases, to ensure equal hash value.
		In addition, there is a performance improvement: the hash in LargeIntegers could
		get very slow for very big numbers"
		(asInteger = self and: [ asInteger abs < Float maxExactInteger ]) ifTrue: [
			^ asInteger hash ]].
	"Both words of the float are used. (The bitShift:'s ensure that the intermediate results do not become a large integer.)
	Care is taken to answer same hash as an equal Integer."
	^ ((self basicAt: 1) bitShift: -4) +
	   ((self basicAt: 2) bitShift: -4)
</details>

#### Float>>#isFinite

Infinities and Not a Number are only represented as BoxedFloat64


<details>
	<summary>See more</summary>
	
	isFinite
	"Infinities and Not a Number are only represented as BoxedFloat64"

	^ true
</details>

#### Float>>#inMinusPiToPiRange

For angles in radians. Add or remove whole turns until we get to the (-Pi .. +Pi] range


<details>
	<summary>See more</summary>
	
	inMinusPiToPiRange
	"For angles in radians. Add or remove whole turns until we get to the (-Pi .. +Pi] range"
	| answer |
	answer _ self \\ Twopi.
	answer > Pi ifTrue: [
		answer _ answer - Twopi ].
	^ answer
</details>

#### Float>>#significand

Answers mantissa as a Float between one and two (or between -1 and -2). See #exponent. See also #signPart, #significandAsInteger and #exponentPart


<details>
	<summary>See more</summary>
	
	significand
	"Answers mantissa as a Float between one and two (or between -1 and -2). See #exponent.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"
	^ self timesTwoPower: (self exponent negated)
</details>

#### Float>>#negated

Answer a Number that is the negation of the receiver. Implementation note: this version cares of negativeZero.


<details>
	<summary>See more</summary>
	
	negated
	"Answer a Number that is the negation of the receiver.
	Implementation note: this version cares of negativeZero."

	^-1.0 * self
</details>

#### Float>>#nextTowardsZero

Answer the Float with largest magnitude but smaller than ours, with the same sign. Only for finite, non zero numbers.


<details>
	<summary>See more</summary>
	
	nextTowardsZero
	"Answer the Float with largest magnitude but smaller than ours, with the same sign.
	Only for finite, non zero numbers."

	| exponent mantissa |
	^ self partValues: [ :sign :myExponent :myMantissa |
		myMantissa isZero ifTrue: [ self error: 'Not for zero values' ].
		(myMantissa = 16r10000000000000 and: [myExponent > Float emin])
			ifTrue: [
				mantissa _ 16r1FFFFFFFFFFFFF.
				exponent _ myExponent -1 ]
			ifFalse: [
				mantissa _ myMantissa-1.
				exponent _ myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]
</details>

#### Float>>#at: index

Avoid primitive in Object>>at:


<details>
	<summary>See more</summary>
	
	at: index 
	"Avoid primitive in Object>>at:"

	^self basicAt: index
</details>

#### Float>>#printAsIEEE32BitPrecisionFloatOn: aStream base: base

Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits: Print as a 32 bit Float


<details>
	<summary>See more</summary>
	
	printAsIEEE32BitPrecisionFloatOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:mantissaSignificantBits:
	Print as a 32 bit Float" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base mantissaSignificantBits: 24]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0'. ^ self]
				ifFalse: [self negated absPrintOn: aStream base: base mantissaSignificantBits: 24]]
</details>

#### Float>>#smoothIsAbsBelow: threshold

A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable. It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold. Useful when we need to 'count' stuff, but still want continuous and differentiable stuff.


<details>
	<summary>See more</summary>
	
	smoothIsAbsBelow: threshold
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	| scaled |
	scaled _ self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]
</details>

#### Float>>#absPrintExactlyOn: aStream base: base

Print my value on a stream in the given base. Assumes that my value is strictly positive; negative numbers, zero, and NaNs have already been handled elsewhere. Based upon the algorithm outlined in: Robert G. Burger and R. Kent Dybvig Printing Floating Point Numbers Quickly and Accurately ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation June 1996. https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf This version guarantees that the printed representation exactly represents my value by using exact integer arithmetic.


<details>
	<summary>See more</summary>
	
	absPrintExactlyOn: aStream base: base
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic."

	| significand exp baseExpEstimate r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount slowbit shead |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	significand := self significandAsInteger.
	roundingIncludesLimits := significand even.
	"What follows is equivalent, but faster than
		exp := self exponentPart - 52."
	exp := (self exponent - 52) max: MinValLogBase2.
	baseExpEstimate := (self exponent * base asFloat reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[significand ~= 16r10000000000000
				ifTrue:
					[r := significand bitShift: 1 + exp.
					s := 2.
					mPlus := mMinus := 1 bitShift: exp]
				ifFalse:
					[r := significand bitShift: 2 + exp.
					s := 4.
					mPlus := 2 * (mMinus := 1 bitShift: exp)]]
		ifFalse:
			[(exp = MinValLogBase2 or: [significand ~= 16r10000000000000])
				ifTrue:
					[r := significand bitShift: 1.
					s := 1 bitShift: 1 - exp.
					mPlus := mMinus := 1]
				ifFalse:
					[r := significand bitShift: 2.
					s := 1 bitShift: 2 - exp.
					mPlus := 2.
					mMinus := 1]].
	baseExpEstimate >= 0
		ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := base raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	((r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * base.
			mPlus := mPlus * base.
			mMinus := mMinus * base].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	slowbit := 1 - s lowBit .
	shead := s bitShift: slowbit.
	[d := (r bitShift: slowbit) // shead.
	r := r - (d * s).
	(tc1 := (r > mMinus) not and: [roundingIncludesLimits or: [r < mMinus]]) |
	(tc2 := (r + mPlus < s) not and: [roundingIncludesLimits or: [r + mPlus > s]])] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * base.
		mPlus := mPlus * base.
		mMinus := mMinus * base.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[(tc1 not or: [r * 2 >= s]) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]
</details>

#### Float>>#tanh

Answer hyperbolic tangent of receiver. Trivial implementation is: ^self sinh/self cosh This implementation takes care not to overflow.


<details>
	<summary>See more</summary>
	
	tanh
	"Answer hyperbolic tangent of receiver.
	Trivial implementation is:
		^self sinh/self cosh
	This implementation takes care not to overflow."

	| ex emx |
	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self > 20.0 ifTrue: [^1.0].
	self < -20.0 ifTrue: [^-1.0].
	ex := self exp.
	emx := ex reciprocal.
	^(ex - emx) / (ex + emx)
</details>

#### Float>>#signBit

Actual sigh bit part of the floating point representation. 0 means positive number or 0.0 1 means negative number or -0.0 Just extract the bit. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.


<details>
	<summary>See more</summary>
	
	signBit
	"
	Actual sigh bit part of the floating point representation.
	0 means positive number or 0.0
	1 means negative number or -0.0
	Just extract the bit. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]
</details>

#### Float>>#log2

Answer the base 2 logarithm of the receiver.


<details>
	<summary>See more</summary>
	
	log2
	"Answer the base 2 logarithm of the receiver."

	^ self ln / Ln2
</details>

#### Float>>#degreesToRadians

Answer the receiver in radians. Assumes the receiver is in degrees.


<details>
	<summary>See more</summary>
	
	degreesToRadians
	"Answer the receiver in radians. Assumes the receiver is in degrees."

	^self * RadiansPerDegree
</details>

#### Float>>#arcSin

Answer the angle in radians.


<details>
	<summary>See more</summary>
	
	arcSin
	"Answer the angle in radians."

	((self < -1.0) or: [self > 1.0]) ifTrue: [^ Float nan].
	^((self = -1.0) or: [self = 1.0])
		ifTrue: [Halfpi * self]
		ifFalse: [(self / (1.0 - (self * self)) sqrt) arcTan]
</details>

#### Float>>#arSinh

Answer receiver's area hyperbolic sine. That is the inverse function of sinh.


<details>
	<summary>See more</summary>
	
	arSinh
	"Answer receiver's area hyperbolic sine.
	That is the inverse function of sinh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"  
	^self + 1 = self 
		ifTrue: [(self abs ln + 2 ln) * self sign]
		ifFalse: [((self squared + 1) sqrt + self) ln]
</details>

#### Float>>#nextAwayFromZero

Answer the Float with smallest magnitude but larger than ours, with the same sign Only for finite numbers.


<details>
	<summary>See more</summary>
	
	nextAwayFromZero
	"Answer the Float with smallest magnitude but larger than ours, with the same sign
	Only for finite numbers."

	| exponent mantissa |
	^ self partValues: [ :sign :myExponent :myMantissa |
		myMantissa = 16r1FFFFFFFFFFFFF
			ifTrue: [
				mantissa _ 16r10000000000000.
				exponent _ myExponent +1 ]
			ifFalse: [
				mantissa _ myMantissa+1.
				exponent _ myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]
</details>

#### Float>>#degreeSin

If finite, allow for special values such as cos(30 degrees) = 1/2


<details>
	<summary>See more</summary>
	
	degreeSin
	"If finite, allow for special values such as cos(30 degrees) = 1/2"
	
	self isFinite ifTrue: [^super degreeSin].
	^self degreesToRadians sin
</details>

#### Float>>#storeOn: aStream base: base

Print the Number exactly so it can be interpreted back unchanged


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: base 
	
	"Print the Number exactly so it can be interpreted back unchanged"
	
	self sign = -1 ifTrue: [aStream nextPutAll: '-'].
	base = 10 ifFalse: [aStream print: base; nextPut: $r].
	self isZero
		ifTrue: [aStream nextPutAll: '0.0']
		ifFalse: [self abs absPrintExactlyOn: aStream base: base]
</details>

#### Float>>#arTanh

Answer receiver's area hyperbolic tangent. That is the inverse function of tanh.


<details>
	<summary>See more</summary>
	
	arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	self = 0.0 ifTrue: [^self].	"Handle negativeZero"
	self = 1 ifTrue: [^ Float infinity].
	self = -1 ifTrue: [^Float negativeInfinity].
	self abs > 1 ifTrue: [^ Float nan].
	^((1 + self) / (1 - self)) ln / 2
</details>

#### Float>>#sqrtNonPrimitive

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrtNonPrimitive
	"Answer the square root of the receiver. "
	| exp guess delta |

	self = 0.0 ifTrue: [
		^self ].	"Answer 0.0 for 0.0, but -0.0 for -0.0. See IEEE 754 standard"

	self <= 0.0
		ifTrue: [
			^NegativePowerError new signalReceiver: self selector: #sqrtNonPrimitive arguments: {} ].

	"NaN and Infinity"
	self isFinite ifFalse: [
		^ self ].

	"Newton-Raphson"
	"first guess is half the exponent"
	exp _ self exponent // 2.
	guess _ self timesTwoPower: 0 - exp.
	[
		delta _ self - (guess * guess) / (guess * 2.0).
		delta abs >= guess ulp ]
	whileTrue: [
		guess _ guess + delta ].
	^ guess
</details>

#### Float>>#to: stop hops: n do: aBlock

Quite like doing self to: stop by: (stop-self)/n do: aBlock but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced. Besides, the way for calculating steps inside the interval minimizes rounding error (wrt to incrementing and accumulator) This means that 0.0 to: 1.0 by: 0.1 do: [ :each | (each@(each = 1.0)) print ] will print false for all values, but 0.0 to: 1.0 hops: 10 do: [ :each | (each@(each = 1.0)) print ] will print true in the last iteration Warning: Having 10 hops means evaluating aBlock 11 times!


<details>
	<summary>See more</summary>
	
	to: stop hops: n do: aBlock
	"Quite like doing 
		self to: stop by: (stop-self)/n do: aBlock
	but ensuring that the last time, the block is evaluated exactly for stop, i.e. no rounding error is introduced.
	Besides, the way for calculating steps inside the interval minimizes rounding error (wrt to incrementing and accumulator)
	This means that
		0.0 to: 1.0 by: 0.1 do: [ :each | (each@(each = 1.0)) print ]
	will print false for all values, but
		0.0 to: 1.0 hops: 10 do: [ :each | (each@(each = 1.0)) print ]
	will print true in the last iteration
	Warning: Having 10 hops means evaluating aBlock 11 times!
	"
"	self to: stop by: (stop-self)/n do: aBlock"

	| factor |
	factor _ (stop - self) / n.
	aBlock value: self.
	1 to: n-1 do: [ :i |
		aBlock value: i * factor + self ].
	aBlock value: stop
</details>

#### Float>>#floatsAwayFrom: aFloat

<details>
	<summary>See more</summary>
	
	floatsAwayFrom: aFloat

	| count2 count1 |
	(self isNaN or: [ aFloat isNaN ]) ifTrue: [ ^ Float nan ].
	self partBits: [:s :e :m | count2 := (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	aFloat partBits: [:s :e :m | count1 := (e bitShift: self class precision - 1) + m * (s * -2 + 1)].
	^count2 - count1
</details>

#### Float>>#isNaN

Not a Number are only represented as BoxedFloat64


<details>
	<summary>See more</summary>
	
	isNaN
	"Not a Number are only represented as BoxedFloat64"

	^ false
</details>

#### Float>>#mantissaBits

Actual bits for the mantissa part of the floating point representation. Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.


<details>
	<summary>See more</summary>
	
	mantissaBits
	"
	Actual bits for the mantissa part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	^ self partBits: [ :signBit :exponentBits :mantissaBits | mantissaBits ]
</details>

#### Float>>#isZero

<details>
	<summary>See more</summary>
	
	isZero
	^self = 0.0
</details>

#### Float>>#arcTanNonPrimitive

Answer the angle in radians.


<details>
	<summary>See more</summary>
	
	arcTanNonPrimitive
	"Answer the angle in radians."

	| theta delta sinTheta cosTheta |

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta _ (self * Halfpi) / (self + 1.0).

	"iterate"
	[
		sinTheta _ theta sin.
		cosTheta _ theta cos.
		delta _ (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		delta abs >= theta ulp ]
		whileTrue: [
			theta _ theta - delta ].
	^ theta
</details>

#### Float>>#partValues: aThreeArgumentBlock

<details>
	<summary>See more</summary>
	
	partValues: aThreeArgumentBlock
	^ self
		partValues: aThreeArgumentBlock
		ifInfinite: [ self error: 'Can not handle infinity' ]
		ifNaN: [ self error: 'Can not handle Not-a-Number' ].
</details>

#### Float>>#isWithin: anInteger floatsFrom: aNumber

<details>
	<summary>See more</summary>
	
	isWithin: anInteger floatsFrom: aNumber

	^self floatsAwayFrom: aNumber :: abs <= anInteger
</details>

#### Float>>#replaceWordsFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceWordsFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	self primitiveFailed
</details>

#### Float>>#copySignTo: aNumber

Return a number with same magnitude as aNumber and same sign as self. Implementation note: take care of Float negativeZero, which is considered as having a negative sign.


<details>
	<summary>See more</summary>
	
	copySignTo: aNumber
	"Return a number with same magnitude as aNumber and same sign as self.
	Implementation note: take care of Float negativeZero, which is considered as having a negative sign."

	self isNaN ifTrue: [ ^self ].
	(self > 0.0 or: [(self at: 1) = 0]) ifTrue: [^ aNumber abs].
	^aNumber withNegativeSign
</details>

#### Float>>#safeArcCos

Answer the angle in radians.


<details>
	<summary>See more</summary>
	
	safeArcCos
	"Answer the angle in radians."
	(self between: -1.0 and: 1.0)
		ifTrue: [^ self arcCos]
		ifFalse: [^ self sign arcCos]
</details>

#### Float>>#absPrintOn: aStream base: base mantissaSignificantBits: significantBits

Print my value on a stream in the given base. Assumes that my value is strictly positive; negative numbers, zero, and NaNs have already been handled elsewhere. Based upon the algorithm outlined in: Robert G. Burger and R. Kent Dybvig Printing Floating Point Numbers Quickly and Accurately ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation June 1996. https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf This version performs all calculations with Floats instead of LargeIntegers, and loses about 3 lsbs of accuracy compared to an exact conversion.


<details>
	<summary>See more</summary>
	
	absPrintOn: aStream base: base mantissaSignificantBits: significantBits
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	| fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
		ifFalse:
			[r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower:  significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus :=
				(exp = MinValLogBase2) | (self significand ~= 1.0)
					ifTrue: [mMinus]
					ifFalse: [mMinus * 2.0]].
	baseExpEstimate >= 0
		ifTrue:
			[exp = 1023
				ifTrue:   "scale down to prevent overflow to Infinity during conversion"
					[r := r / fBase.
					s := s * (fBase raisedToInteger: baseExpEstimate - 1).
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase]
				ifFalse:
					[s := s * (fBase raisedToInteger: baseExpEstimate)]]
		ifFalse:
			[exp < -1023
				ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
					[d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
				ifFalse:
				[scale := fBase raisedToInteger: baseExpEstimate negated].
			s := s / scale].
	(r + mPlus >= s)
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[s := s / fBase].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1].
	[d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * fBase.
		mPlus := mPlus * fBase.
		mMinus := mMinus * fBase.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]
</details>

## Fraction

Fraction provides methods for dealing with fractions like 1/3 as fractions (not as 0.33333...). All public arithmetic operations answer reduced fractions (see examples). instance variables: 'numerator denominator ' Examples: (note the parentheses required to get the right answers in Smalltalk): (2/3) + (2/3) (2/3) + (1/2) "answers shows the reduced fraction" (2/3) raisedToInteger: 5 "fractions also can have exponents"

### Methods
#### Fraction>>#printOn: aStream fractionDigits: placesDesired

Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation.


<details>
	<summary>See more</summary>
	
	printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired > 0
		ifFalse: [self rounded printOn: aStream]
		ifTrue: [
			scaling := 10 raisedToInteger: placesDesired.
			integerPart := numerator abs quo: denominator.
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2.
			roundedFractionPart = scaling
				ifTrue:
					[integerPart := integerPart + 1.
					roundedFractionPart := 0].
			"Don't print minus sign if result is rounded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].
</details>

#### Fraction>>#reduced

<details>
	<summary>See more</summary>
	
	reduced

	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd _ numerator gcd: denominator.
	numer _ numerator // gcd.
	denom _ denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom
</details>

#### Fraction>>#truncated

Refer to the comment in Number|truncated.


<details>
	<summary>See more</summary>
	
	truncated 
	"Refer to the comment in Number|truncated."

	^numerator quo: denominator
</details>

#### Fraction>>#asFraction

Answer the receiver itself.


<details>
	<summary>See more</summary>
	
	asFraction	
	"Answer the receiver itself."

	^self
</details>

#### Fraction>>#nthRoot: aPositiveInteger

Answer the nth root of the receiver.


<details>
	<summary>See more</summary>
	
	nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	| guess |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^ NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger  ].
	
	guess _ (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign
</details>

#### Fraction>>#adaptToInteger: rcvr andSend: selector

If I am involved in arithmetic with an Integer, convert it to a Fraction.


<details>
	<summary>See more</summary>
	
	adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a Fraction."
	^ rcvr asFraction perform: selector with: self
</details>

#### Fraction>>#isLiteral

1e-3 isLiteral (1/3) isLiteral


<details>
	<summary>See more</summary>
	
	isLiteral
	"
	1e-3 isLiteral
	(1/3) isLiteral
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			^true]
		otherwise: [
			^false]
</details>

#### Fraction>>#cubed

See Fraction (Number) | cubed


<details>
	<summary>See more</summary>
	
	cubed
	"See Fraction (Number) | cubed"
	^ Fraction numerator: numerator cubed denominator: denominator cubed
</details>

#### Fraction>>#= aNumber

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aNumber

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [ ^ false ].
	aNumber isFraction
		ifTrue: [numerator = 0 ifTrue: [^ aNumber numerator = 0].
				^ (numerator * aNumber denominator) =
					(aNumber numerator * denominator)
				"Note: used to just compare num and denom,
					but this fails for improper fractions"].
	^ aNumber adaptToFraction: self andSend: #=
</details>

#### Fraction>>#hash

Hash is reimplemented because = is implemented. Care is taken that a Fraction equal to a Float also have an equal hash


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented.
	Care is taken that a Fraction equal to a Float also have an equal hash"

	^ self asFloat hash
</details>

#### Fraction>>#asFloat

Answer a Float that closely approximates the value of the receiver. This implementation will answer the closest floating point number to the receiver. In case of a tie, it will use the IEEE 754 round to nearest even mode. In case of overflow, it will answer +/- Float infinity.


<details>
	<summary>See more</summary>
	
	asFloat
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent floatMantissa hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBit.
	hb := b highBit.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly as floating point number,
	float division is fastest."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	The first guess is approximate, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will lose bits.
	Keep at least one bit to let underflow preserve the sign of zero."
	lostBit := Float emin - (exponent + hm - 1).
	lostBit > 0 ifTrue: [n := n - lostBit max: 1].

	"Remove excess bits in the mantissa."
	hm > n
		ifTrue:
			[exponent := exponent + hm - n.
			hasTruncatedBits := hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - n].
			mantissa := mantissa bitShift: n - hm].

	"Check if mantissa must be rounded upward.
	The case of tie will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	floatMantissa := mantissa asFloat.
	self positive ifFalse: [floatMantissa := floatMantissa negated].
	^floatMantissa timesTwoPower: exponent
</details>

#### Fraction>>#squared

See Fraction (Number) | squared


<details>
	<summary>See more</summary>
	
	squared
	"See Fraction (Number) | squared"
	^ Fraction numerator: numerator squared denominator: denominator squared
</details>

#### Fraction>>#log: aNumber

Answer the log base aNumber of the receiver. (1/(3 raisedTo: 215)) log: 3 (1/((3 raisedTo: 215)+(3 raisedTo: 213))) log: 3


<details>
	<summary>See more</summary>
	
	log: aNumber
	"Answer the log base aNumber of the receiver.
	(1/(3 raisedTo: 215)) log: 3
	(1/((3 raisedTo: 215)+(3 raisedTo: 213))) log: 3
	"
	^numerator = 1
		ifTrue: [ (denominator log: aNumber) negated ]
		ifFalse: [ super log: aNumber ]
</details>

#### Fraction>>#negated

Refer to the comment in Number|negated.


<details>
	<summary>See more</summary>
	
	negated 
	"Refer to the comment in Number|negated."

	^ Fraction
		numerator: numerator negated
		denominator: denominator
</details>

#### Fraction>>#setNumerator: n denominator: d

<details>
	<summary>See more</summary>
	
	setNumerator: n denominator: d

	d = 0
		ifTrue: [^ZeroDivide new signalReceiver: self selector: #setNumerator:denominator: arguments: {n.d}]
		ifFalse: [
			numerator _ n asInteger.
			denominator _ d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator _ numerator negated]]
</details>

#### Fraction>>#< aNumber

Answer whether the receiver is less than the argument.


<details>
	<summary>See more</summary>
	
	< aNumber
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator < (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andSend: #<
</details>

#### Fraction>>#numerator

<details>
	<summary>See more</summary>
	
	numerator

	^numerator
</details>

#### Fraction>>#raisedToFraction: aFraction

Raise an exception if argument is not a Fraction


<details>
	<summary>See more</summary>
	
	raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root _ (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction
</details>

#### Fraction>>#printOn: aStream base: base

This method should print a representation of the number for the given base, excluding the base prefix (and the letter r for radix)


<details>
	<summary>See more</summary>
	
	printOn: aStream base: base
	aStream isText
		ifTrue:
			[aStream nextPutAll: (numerator printStringBase: base) super; nextPut: $/; nextPutAll: (denominator printStringBase: base) sub.
			^ self].
	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $).

</details>

#### Fraction>>#/ aNumber

Answer the result of dividing the receiver by aNumber.


<details>
	<summary>See more</summary>
	
	/ aNumber
	"Answer the result of dividing the receiver by aNumber."
	aNumber isFraction
		ifTrue: [^self * aNumber reciprocal].
	^ aNumber adaptToFraction: self andSend: #/
</details>

#### Fraction>>#ln

This function is defined because super ln might overflow.


<details>
	<summary>See more</summary>
	
	ln
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln
</details>

#### Fraction>>#storeOn: aStream

If possible, store as a literal of the form 9999e-99 If denominator's prime factors are just 2 and 5, then we can be printed as a literal of the form 999999e-99


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	| e f |
	
	"If possible, store as a literal of the form 9999e-99
	If denominator's prime factors are just 2 and 5, then we can be printed as a literal
	of the form 999999e-99"
	"
	123e-12 storeString
	123e-12 printString
	123e-12000 storeString
	123e-12000 printString
	(3/4) storeString
	(3/4) printString
	(-3/4) storeString
	(-3/4) printString
	(1/3) storeString
	(1/3) printString
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			exponent2 > exponent5
				ifTrue: [
					e _ exponent2.
					f _ 5 raisedToInteger: e-exponent5 ]
				ifFalse: [
					e _ exponent5.
					f _ 2 bitShift: e-exponent2-1 ].
			numerator*f storeOn: aStream base: 10.
			aStream nextPut: $e; nextPut: $-.
			e storeOn: aStream base: 10 ]
		otherwise: [
			super storeOn: aStream ]

</details>

#### Fraction>>#log

(1/(10 raisedTo: 215)) log (1/((10 raisedTo: 215)+(10 raisedTo: 213))) log


<details>
	<summary>See more</summary>
	
	log
	"
	(1/(10 raisedTo: 215)) log
	(1/((10 raisedTo: 215)+(10 raisedTo: 213))) log
	"
	| res |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].

	"Integer answer if possible!"
	numerator = 1
		ifTrue: [ ^denominator log negated ].

	"This because super log might overflow."
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [ ^self reciprocal log negated ].
	res := super log.
	res isFinite ifTrue: [^res].
	^numerator log - denominator log
</details>

#### Fraction>>#printAsLiteralOn: aStream

<details>
	<summary>See more</summary>
	
	printAsLiteralOn: aStream
	| n d base powers pow2 maximumBase |
	n := numerator.
	d := denominator.
	powers := Dictionary new.
	(pow2 := d lowBit - 1) = 0
		ifTrue: 
			[base := 1]
		ifFalse:
			[base := 2.
			powers at: 2 put: pow2.
			d := d >> pow2].
	maximumBase := 36.
	Integer largePrimesUpTo: (d sqrtFloor max: 2) do: [:p |
		| pow |
		(d rem: p) = 0
			ifTrue:
				[pow := 1.
				[((d := d quo: p) rem: p) = 0] whileTrue: [pow := pow + 1].
				base := base * p.
				base > maximumBase ifTrue: [self halt: 'cannot print in base > ' , maximumBase printString].
				powers at: p put: pow].
		p squared > d
			ifTrue:
				["Decomposition done, we can now print"
				| maximumPower b |
				d = 1 ifFalse: [base := base * d. powers at: d put: 1].
				base > maximumBase ifTrue: [self halt: 'cannot print in base > ' , maximumBase printString].
				base = 2 ifTrue: [base := 10. powers at: 5 put: 0].
				maximumPower := powers detectMax: [:each | each].
				powers keysAndValuesDo: [:k :v | n := n * (k raisedTo: maximumPower - v)].
				b := base.
				[maximumPower > 1 and: [b * base <= maximumBase]]
					whileTrue:
						[b := b * base.
						maximumPower := maximumPower - 1].
				n storeOn: aStream base: b.
				aStream nextPutAll: 'e-'.
				maximumPower storeOn: aStream.
				^self]]
</details>

#### Fraction>>#log2

(1/(2 raisedTo: 215)) log2 (1/((2 raisedTo: 215)+(2 raisedTo: 213))) log2


<details>
	<summary>See more</summary>
	
	log2
	"
	(1/(2 raisedTo: 215)) log2
	(1/((2 raisedTo: 215)+(2 raisedTo: 213))) log2
	"
	^self isPowerOfTwo
		ifTrue: [ 1-denominator highBitOfMagnitude ]
		ifFalse: [ super log2 ]
</details>

#### Fraction>>#is: aSymbol

Note: Senders might prefer #isFraction for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Note: Senders might prefer #isFraction for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Fraction = aSymbol or: [ super is: aSymbol ]
</details>

#### Fraction>>#isPowerOfTwo

<details>
	<summary>See more</summary>
	
	isPowerOfTwo
	^numerator = 1 and: [ denominator isPowerOfTwo ]
</details>

#### Fraction>>#isFraction

Answer true if the receiver is a Fraction.


<details>
	<summary>See more</summary>
	
	isFraction
	^ true
</details>

#### Fraction>>#sqrt

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrt
	| d n answer |
	n _ numerator sqrt.
	d _ denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	answer _ n / d.
	answer isNaN ifTrue: [
		^self asFloat sqrt ].
	^ answer
</details>

#### Fraction>>#storeOn: aStream base: base

This method should print a representation of the number for the given base, including the base prefix (with letter r for radix)


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: base

	aStream nextPut: $(.
	numerator storeOn: aStream base: base.
	aStream nextPut: $/.
	denominator storeOn: aStream base: base.
	aStream nextPut: $).

</details>

#### Fraction>>#negative

Answer whether the receiver is mathematically negative.


<details>
	<summary>See more</summary>
	
	negative

	^numerator negative
</details>

#### Fraction>>#+ aNumber

Answer the sum of the receiver and aNumber.


<details>
	<summary>See more</summary>
	
	+ aNumber 
	"Answer the sum of the receiver and aNumber."
	| n d d1 d2 |
	aNumber isFraction ifTrue: 
		[d _ denominator gcd: aNumber denominator.
		n _ numerator * (d1 _ aNumber denominator // d) + (aNumber numerator * (d2 _ denominator // d)).
		d1 _ d1 * d2.
		n _ n // (d2 _ n gcd: d).
		(d _ d1 * (d // d2)) = 1 ifTrue: [^ n].
		^ Fraction numerator: n denominator: d].
	^ aNumber adaptToFraction: self andSend: #+
</details>

#### Fraction>>#- aNumber

Answer the difference between the receiver and aNumber.


<details>
	<summary>See more</summary>
	
	- aNumber
	"Answer the difference between the receiver and aNumber."
	aNumber isFraction ifTrue:
		[^ self + aNumber negated].
	^ aNumber adaptToFraction: self andSend: #-
</details>

#### Fraction>>#reciprocal

Refer to the comment in Number|reciprocal.


<details>
	<summary>See more</summary>
	
	reciprocal 
	"Refer to the comment in Number|reciprocal."

	^denominator / numerator
</details>

#### Fraction>>#floorLog: radix

Unlike super, this version is exact when radix is integer


<details>
	<summary>See more</summary>
	
	floorLog: radix
	"Unlike super, this version is exact when radix is integer"
	
	| d n |
	radix isInteger ifFalse: [^super floorLog: radix].
	n := numerator floorLog: radix.
	d := denominator floorLog: radix.
	^(numerator * (radix raisedTo: d))
		< (denominator * (radix raisedTo: n))
		ifTrue: [n - d - 1]
		ifFalse: [n - d]
</details>

#### Fraction>>#<= aNumber

Answer whether the receiver is less than or equal to the argument.


<details>
	<summary>See more</summary>
	
	<= aNumber
	aNumber isFraction ifTrue:
		[^ numerator * aNumber denominator <= (aNumber numerator * denominator)].
	^ aNumber adaptToFraction: self andSend: #<=
</details>

#### Fraction>>#denominator

<details>
	<summary>See more</summary>
	
	denominator

	^denominator
</details>

#### Fraction>>#* aNumber

Answer the result of multiplying the receiver by aNumber.


<details>
	<summary>See more</summary>
	
	* aNumber 
	"Answer the result of multiplying the receiver by aNumber."
	| d1 d2 |
	aNumber isFraction ifTrue: 
		[d1 _ numerator gcd: aNumber denominator.
		d2 _ denominator gcd: aNumber numerator.
		(d2 = denominator and: [d1 = aNumber denominator])
			ifTrue: [^ numerator // d1 * (aNumber numerator // d2)].
		^ Fraction numerator: numerator // d1 * (aNumber numerator // d2)
				denominator: denominator // d2 * (aNumber denominator // d1)].
	^ aNumber adaptToFraction: self andSend: #*
</details>

#### Fraction>>#raisedToInteger: anInteger

See Number | raisedToInteger:


<details>
	<summary>See more</summary>
	
	raisedToInteger: anInteger 
	"See Number | raisedToInteger:"
	"Raise an exception if argument is not a anInteger"

	^ (numerator raisedToInteger: anInteger) / (denominator raisedToInteger: anInteger)
</details>

## Integer

I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger. Integer division consists of: / exact division, answers a fraction if result is not a whole integer // answers an Integer, rounded towards negative infinity \\ is modulo rounded towards negative infinity quo: truncated division, rounded towards zero

### Methods
#### Integer>>#>> shiftAmount

right shift


<details>
	<summary>See more</summary>
	
	>> shiftAmount  "right shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount negated
</details>

#### Integer>>#atRandom: aGenerator

Answer a random integer from 1 to self picked from aGenerator.


<details>
	<summary>See more</summary>
	
	atRandom: aGenerator
	"Answer a random integer from 1 to self picked from aGenerator."

	^ aGenerator nextInteger: self
</details>

#### Integer>>#isPrime

Answer true if the receiver is a prime number. See isProbablyPrime for a probabilistic implementation that is much faster for large integers, and that is correct to an extremely high statistical level of confidence (effectively deterministic).


<details>
	<summary>See more</summary>
	
	isPrime
	"Answer true if the receiver is a prime number. See isProbablyPrime for a probabilistic
	implementation that is much faster for large integers, and that is correct to an extremely
	high statistical level of confidence (effectively deterministic)."
	
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2].
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 ifTrue: [ ^false ] ].
	^true
</details>

#### Integer>>#asInteger

Answer with the receiver itself.


<details>
	<summary>See more</summary>
	
	asInteger
	"Answer with the receiver itself."

	^self


</details>

#### Integer>>#gcd: anInteger

See Knuth, Vol 2, 4.5.2, Algorithm L


<details>
	<summary>See more</summary>
	
	gcd: anInteger
	"See Knuth, Vol 2, 4.5.2, Algorithm L"
	"Initialize"
	| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |
	higher _ SmallInteger maxVal highBit.
	u _ self abs max: (v _ anInteger abs).
	v _ self abs min: v.
	[v class == SmallInteger]
		whileFalse: 
			[(uHat _ u bitShift: (k _ higher - u highBit)) class == SmallInteger
				ifFalse: 
					[k _ k - 1.
					uHat _ uHat bitShift: -1].
			vHat _ v bitShift: k.
			a _ 1.
			b _ 0.
			c _ 0.
			d _ 1.
			"Test quotient"
			[(vPrime _ vHat + d) ~= 0
				and: [(vPrimePrime _ vHat + c) ~= 0 and: [(q _ uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]
				whileTrue: 
					["Emulate Euclid"
					c _ a - (q * (a _ c)).
					d _ b - (q * (b _ d)).
					vHat _ uHat - (q * (uHat _ vHat))].
			"Multiprecision step"
			b = 0
				ifTrue: 
					[v _ u rem: (u _ v)]
				ifFalse: 
					[t _ u * a + (v * b).
					v _ u * c + (v * d).
					u _ t]].
	^ v gcd: u
</details>

#### Integer>>#printStringRoman

<details>
	<summary>See more</summary>
	
	printStringRoman
	^String streamContents: [:stream | | integer |
		integer := self negative ifTrue: [stream nextPut: $-. self negated] ifFalse: [self].
		integer // 1000 timesRepeat: [stream nextPut: $M].
		integer
			romanDigits: 'MDC' for: 100 on: stream;
			romanDigits: 'CLX' for: 10 on: stream;
			romanDigits: 'XVI' for: 1 on: stream]
</details>

#### Integer>>#truncated

Refer to the comment in Number|truncated.


<details>
	<summary>See more</summary>
	
	truncated 
	"Refer to the comment in Number|truncated."

	^self
</details>

#### Integer>>#integersTo: stop count: n

Answer an interval, up to stop, with n elements. All the elements will be integers.


<details>
	<summary>See more</summary>
	
	integersTo: stop count: n
	"Answer an interval, up to stop, with n elements. All the elements will be integers."
	
	^Interval integersFrom: self to: stop count: n
</details>

#### Integer>>#isProbablyPrime

See isProbablyPrimeWithK:andQ: for the algoritm description.


<details>
	<summary>See more</summary>
	
	isProbablyPrime
	"See isProbablyPrimeWithK:andQ: for the algoritm description."
	
	| k q |
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2 ].
	"Factor self into (2 raisedTo: k) * q + 1, where q odd"
	q := self bitShift: -1.
	k := q lowBit.
	q := q bitShift: 1 - k.
	"Repeat the probabilistic until false (the probability of false negative is null) or until probability is very low."
	25 timesRepeat: [ (self isProbablyPrimeWithK: k andQ: q) ifFalse: [ ^false ] ].
	"The probability of false positive after 25 iterations is less than (1/4 raisedTo: 25) < 1.0e-15"
	^true
</details>

#### Integer>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral

	^true
</details>

#### Integer>>#digitLogic: arg op: op length: len

<details>
	<summary>See more</summary>
	
	digitLogic: arg op: op length: len 
	| i result neg1 neg2 rneg z1 z2 rz b1 b2 b |
	neg1 := self negative.
	neg2 := arg negative.
	rneg := ((neg1
				ifTrue: [-1]
				ifFalse: [0])
				perform: op
				with: (neg2
						ifTrue: [-1]
						ifFalse: [0]))
				< 0.
	result := Integer new: len neg: rneg.
	rz := z1 := z2 := true.
	i := 0.
	[(i := i + 1) <= len
		or: ["mind a carry on result that might go past len digits"
			rneg and: [rz
				and: [result := result growby: 1.
					true]]]]
		whileTrue: [b1 := self digitAt: i.
			neg1
				ifTrue: [b1 := z1
								ifTrue: [b1 = 0
										ifTrue: [0]
										ifFalse: [z1 := false.
											256 - b1]]
								ifFalse: [255 - b1]].
			b2 := arg digitAt: i.
			neg2
				ifTrue: [b2 := z2
								ifTrue: [b2 = 0
										ifTrue: [0]
										ifFalse: [z2 := false.
											256 - b2]]
								ifFalse: [255 - b2]].
			b := b1 perform: op with: b2.
			result
				digitAt: i
				put: (rneg
						ifTrue: [rz
								ifTrue: [b = 0
										ifTrue: [0]
										ifFalse: [rz := false.
											256 - b]]
								ifFalse: [255 - b]]
						ifFalse: [b])].
	^ result normalize
</details>

#### Integer>>#copyto: x

<details>
	<summary>See more</summary>
	
	copyto: x
	| stop |
	stop _ self digitLength min: x digitLength.
	^ x replaceFrom: 1 to: stop with: self startingAt: 1
</details>

#### Integer>>#bitAnd: n

Answer an Integer whose bits are the logical AND of the receiver's bits and those of the argument, n.


<details>
	<summary>See more</summary>
	
	bitAnd: n 
	"Answer an Integer whose bits are the logical AND of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitAnd' module:'LargeIntegers'>
	norm _ n normalize.
	^ self
		digitLogic: norm
		op: #bitAnd:
		length: (self digitLength max: norm digitLength)
</details>

#### Integer>>#slidingLeftRightRaisedTo: n modulo: m

Private - compute (self raisedTo: n) \\ m, Note: this method has to be fast because it is generally used with large integers in cryptography. It thus operate on exponent bits from left to right by packets with a sliding window rather than bit by bit (see below).


<details>
	<summary>See more</summary>
	
	slidingLeftRightRaisedTo: n modulo: m
	"Private - compute (self raisedTo: n) \\ m,
	Note: this method has to be fast because it is generally used with large integers in cryptography.
	It thus operate on exponent bits from left to right by packets with a sliding window rather than bit by bit (see below)."
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self * self \\\ m.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: pow * square \\\ m].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := 1.
	[k > 0]
		whileTrue:
			[pow := pow * pow \\\ m.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found an odd bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow * pow \\\ m.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow * (oddPowersOfSelf at: index + 1) \\\ m].
			k := k - 1].
	^pow normalize
</details>

#### Integer>>#asFloat

Answer a Float that best approximates the value of the receiver.


<details>
	<summary>See more</summary>
	
	asFloat
	"Answer a Float that best approximates the value of the receiver."
	
	self subclassResponsibility
</details>

#### Integer>>#printStringAsBytes

Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes. Useful for file-browsers. 123 printStringAsBytes 1024 printStringAsBytes (12*1024) printStringAsBytes (1024*1024) printStringAsBytes (1024*1024*1024) printStringAsBytes (1024*1024*1024*1024) printStringAsBytes (30 factorial) printStringAsBytes See https://en.wikipedia.org/wiki/Kibibyte See #printStringAsBytesDecimal


<details>
	<summary>See more</summary>
	
	printStringAsBytes
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 printStringAsBytes
	1024 printStringAsBytes
	(12*1024) printStringAsBytes
	(1024*1024) printStringAsBytes
	(1024*1024*1024) printStringAsBytes
	(1024*1024*1024*1024) printStringAsBytes
	(30 factorial) printStringAsBytes
	
	See https://en.wikipedia.org/wiki/Kibibyte
	See #printStringAsBytesDecimal
	"
	self withBinaryUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
		^String streamContents: [ :strm |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixSymbol;
				nextPut: $B]]
</details>

#### Integer>>#reciprocalModulo: mod

Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if x does not exist. The algorithm is a non extended euclidean modular inversion called NINV. It is described in this article: 'Using an RSA Accelerator for Modular Inversion' by Martin Seysen. See http://www.iacr.org/archive/ches2005/017.pdf


<details>
	<summary>See more</summary>
	
	reciprocalModulo: mod
	"Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if x does not exist.
	The algorithm is a non extended euclidean modular inversion called NINV.
	It is described in this article:
		'Using an RSA Accelerator for Modular Inversion'
	by Martin Seysen. See http://www.iacr.org/archive/ches2005/017.pdf"

	| receiver u v f fPlusN b result result2 |
	mod > 1 ifFalse: [^nil].
	receiver := (self >= mod or: [self < 0])
		ifTrue: [self \\ mod] ifFalse: [self].
	b := mod highBit + 1.
	f := 1 bitShift: b.
	v := (receiver bitShift: b) + 1.
	u := mod bitShift: b.
	fPlusN := f + mod.
	[v >= fPlusN] whileTrue:
		[v := u \\\ (u := v)].
	result := v - f.
	result2 := result + mod.
	result2 > 0 ifFalse: [^nil].
	^result positive
		ifTrue: [result]
		ifFalse: [result2]
</details>

#### Integer>>#printOn: aStream length: minimum zeroPadded: zeroFlag

7 printOn: Transcript length: 4 zeroPadded: true. Transcript newLine.


<details>
	<summary>See more</summary>
	
	printOn: aStream length: minimum zeroPadded: zeroFlag
	"
	7 printOn: Transcript length: 4 zeroPadded: true. Transcript newLine.
	"
	self printOn: aStream base: 10 length: minimum padded: zeroFlag
</details>

#### Integer>>#romanDigits: digits for: base on: aStream

<details>
	<summary>See more</summary>
	
	romanDigits: digits for: base on: aStream
	| n |
	n _ self \\ (base * 10) // base.
	n = 9 ifTrue: [^ aStream nextPut: digits last; nextPut: digits first].
	n = 4 ifTrue: [^ aStream nextPut: digits last; nextPut: digits second].
	n > 4 ifTrue: [aStream nextPut: digits second].
	n \\ 5 timesRepeat: [aStream nextPut: digits last]
</details>

#### Integer>>#raisedToFraction: aFraction

Raise an exception if argument is not a Fraction


<details>
	<summary>See more</summary>
	
	raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	self = 0 ifTrue: [ ^0 ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction
</details>

#### Integer>>#asColorOfDepth: d

Return a color value representing the receiver as color of the given depth


<details>
	<summary>See more</summary>
	
	asColorOfDepth: d
	"Return a color value representing the receiver as color of the given depth"
	^Color colorFromPixelValue: self depth: d
</details>

#### Integer>>#log

This function is defined because super log might overflow. (10 raisedTo: 215) log ((10 raisedTo: 215)+(10 raisedTo: 213)) log Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.


<details>
	<summary>See more</summary>
	
	log
	"This function is defined because super log might overflow.
	(10 raisedTo: 215) log
	((10 raisedTo: 215)+(10 raisedTo: 213)) log
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self floatLog.
	roundedAnswer _ floatAnswer rounded.
	(10 raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer
</details>

#### Integer>>#digitDiv: arg neg: ng

Answer with an array of (quotient, remainder).


<details>
	<summary>See more</summary>
	
	digitDiv: arg neg: ng 
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	<primitive: 'primDigitDivNegative' module:'LargeIntegers'>
	arg = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #digitDiv:neg: arguments: {arg.ng}].
	"TFEI added this line"
	l _ self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^ Array with: 0 with: self].
	"shortcut against #highBit"
	d _ 8 - arg lastDigit highBitOfMagnitude.
	div _ arg digitLshift: d.
	div _ div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem _ self digitLshift: d.
	rem digitLength = self digitLength ifTrue: [rem _ rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo _ Integer new: l neg: ng.
	dl _ div digitLength - 1.
	"Last actual byte of data"
	ql _ l.
	dh _ div digitAt: dl.
	dnh _ dl = 1
				ifTrue: [0]
				ifFalse: [div digitAt: dl - 1].
	1 to: ql do: 
		[:k | 
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j _ rem digitLength + 1 - k.
		"r1 _ rem digitAt: j."
		(rem digitAt: j)
			= dh
			ifTrue: [qhi _ qlo _ 15
				"i.e. q=255"]
			ifFalse: 
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
				Note that r1,r2 are bytes, not nibbles.  
				Be careful not to generate intermediate results exceeding 13  
				bits."
				"r2 _ (rem digitAt: j - 1)."
				t _ ((rem digitAt: j)
							bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitShift: -4).
				qhi _ t // dh.
				t _ (t \\ dh bitShift: 4)
							+ ((rem digitAt: j - 1)
									bitAnd: 15).
				qlo _ t // dh.
				t _ t \\ dh.
				"Next compute (hi,lo) _ q*dnh"
				hi _ qhi * dnh.
				lo _ qlo * dnh + ((hi bitAnd: 15)
								bitShift: 4).
				hi _ (hi bitShift: -4)
							+ (lo bitShift: -8).
				lo _ lo bitAnd: 255.
				"Correct overestimate of q.  
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 _ j < 3
							ifTrue: [0]
							ifFalse: [rem digitAt: j - 2].
				[(t < hi
					or: [t = hi and: [r3 < lo]])
					and: 
						["i.e. (t,r3) < (hi,lo)"
						qlo _ qlo - 1.
						lo _ lo - dnh.
						lo < 0
							ifTrue: 
								[hi _ hi - 1.
								lo _ lo + 256].
						hi >= dh]]
					whileTrue: [hi _ hi - dh].
				qlo < 0
					ifTrue: 
						[qhi _ qhi - 1.
						qlo _ qlo + 16]].
		"Subtract q*div from rem"
		l _ j - dl.
		a _ 0.
		1 to: div digitLength do: 
			[:i | 
			hi _ (div digitAt: i)
						* qhi.
			lo _ a + (rem digitAt: l) - ((hi bitAnd: 15)
							bitShift: 4) - ((div digitAt: i)
							* qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).
			"sign-tolerant form of (lo bitAnd: 255)"
			a _ lo // 256 - (hi bitShift: -4).
			l _ l + 1].
		a < 0
			ifTrue: 
				["Add div back into rem, decrease q by 1"
				qlo _ qlo - 1.
				l _ j - dl.
				a _ 0.
				1 to: div digitLength do: 
					[:i | 
					a _ (a bitShift: -8)
								+ (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l _ l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4)
				+ qlo].
	rem _ rem
				digitRshift: d
				bytes: 0
				lookfirst: dl.
	^ Array with: quo with: rem
</details>

#### Integer>>#raisedToInteger: exp modulo: m

self deprecated: 'rather use #raisedTo:modulo: for efficiency'.


<details>
	<summary>See more</summary>
	
	raisedToInteger: exp modulo: m
	"
	self deprecated: 'rather use #raisedTo:modulo: for efficiency'.
	"
	(exp = 0) ifTrue: [^ 1].
	exp even
		ifTrue: [^ (self raisedToInteger: (exp // 2) modulo: m) squared \\ m]
		ifFalse: [^ (self * (self raisedToInteger: (exp - 1) modulo: m)) \\ m].
</details>

#### Integer>>#printOn: aStream thousandSeparator: aString includePlusSign: aBoolean

String streamContents: [ :strm | 123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ] String streamContents: [ :strm | -123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ]


<details>
	<summary>See more</summary>
	
	printOn: aStream thousandSeparator: aString includePlusSign: aBoolean
	"
	String streamContents: [ :strm | 123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ]
	String streamContents: [ :strm | -123456789 printOn: strm thousandSeparator: ',' includePlusSign: false ]
	"
	| digits |
	digits _ self abs printString.
	self sign = -1
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [
			aBoolean ifTrue: [ aStream nextPut: $+ ]].
	1 to: digits size do: [ :i |
		aStream nextPut: (digits at: i).
		(i < digits size and: [ i - digits size \\ 3 = 0 ]) ifTrue: [
			aStream nextPutAll: aString ]]
</details>

#### Integer>>#hex

Print the receiver as hex, prefixed with 16r. DO NOT CHANGE THIS! The Cog VMMaker depends on this. Consider using any of printStringBase: 16 printStringBase: 16 length: 8 padded: true storeStringBase: 16 storeStringBase: 16 length: 11 padded: true


<details>
	<summary>See more</summary>
	
	hex
	"Print the receiver as hex, prefixed with 16r.  DO NOT CHANGE THIS!  The Cog VMMaker depends on this.
	 Consider using any of
		printStringBase: 16
		printStringBase: 16 length: 8 padded: true
		storeStringBase: 16
		storeStringBase: 16 length: 11 padded: true"

	^ String streamContents: [ :strm | self storeOn: strm base: 16 ]
</details>

#### Integer>>#- aNumber

Refer to the comment in Number -


<details>
	<summary>See more</summary>
	
	- aNumber
	"Refer to the comment in Number - "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ self digitSubtract: aNumber]
			ifFalse: [^ (self digitAdd: aNumber) normalize]].
	^ aNumber adaptToInteger: self andSend: #-
</details>

#### Integer>>#lcm: anInteger

Answer the least common multiple of the receiver and anInteger. This is the smallest non-negative integer divisible by the receiver and the argument. If either the receiver or the argument is zero, the result is zero.


<details>
	<summary>See more</summary>
	
	lcm: anInteger
	"Answer the least common multiple of the receiver and anInteger.
	This is the smallest non-negative integer divisible by the receiver and the argument.
	If either the receiver or the argument is zero, the result is zero."

	(self = 0 or: [anInteger = 0]) ifTrue: [^ 0].
	^self abs // (self gcd: anInteger) * anInteger abs
</details>

#### Integer>>#xgcd: anInteger

Extended Euclidean algorithm. Answer an array {x. u. v} where self * u + (anInteger * v) = x, and x = (self gcd: anInteger).


<details>
	<summary>See more</summary>
	
	xgcd: anInteger
	"Extended Euclidean algorithm.
	Answer an array {x. u. v} where self * u + (anInteger * v) = x, and x = (self gcd: anInteger)."
	| a b s t sp tp r rp |
	a _ self. b _ anInteger.
	s _ 0. sp _ 1.
	t _ 1. tp _ 0.
	r _ a abs. rp _ b abs.
	[r == 0]
		whileFalse:
			[ | q temp |
			q _ rp // r.
			temp _ r. r _ rp - (q * r). rp _ temp.
			temp _ s. s _ sp - (q * s). sp _ temp.
			temp _ t. t _ tp - (q * t). tp _ temp].
	sp _ sp * b sign. tp _ tp * a sign.
	^ {rp. tp. sp}
</details>

#### Integer>>#floorLog: radix

Unlike super, this version is exact when radix is integer


<details>
	<summary>See more</summary>
	
	floorLog: radix
	"Unlike super, this version is exact when radix is integer"
	
	radix isInteger ifFalse: [^super floorLog: radix].
	self <= 0 ifTrue: [^DomainError signal: 'floorLog: is only defined for x > 0.0'].
	^(self numberOfDigitsInBase: radix) - 1
</details>

#### Integer>>#montgomeryRaisedTo: n times: y modulo: m mInvModB: mInv

Private - do a Montgomery exponentiation of self modulo m. The operation is equivalent to (self/y raisedTo: n)*y \\ m, with y is (256 raisedTo: m digitLength), with (m bitAnd: 255) * mInv \\ 256 = 255.


<details>
	<summary>See more</summary>
	
	montgomeryRaisedTo: n times: y modulo: m mInvModB: mInv
	"Private - do a Montgomery exponentiation of self modulo m.
	The operation is equivalent to (self/y raisedTo: n)*y \\ m,
	with y is (256 raisedTo: m digitLength),
	with (m bitAnd: 255) * mInv \\ 256 = 255."
	
	| pow j k w index oddPowersOfSelf square |
	
	"Precompute powers of self for odd bit patterns xxxx1 up to length w + 1.
	The width w is chosen with respect to the total bit length of n,
	such that each bit pattern will on average be encoutered P times in the whole bit sequence of n.
	This costs (2 raisedTo: w) multiplications, but more will be saved later (see below)."
	k := n highBit.
	w := (k highBit - 1 >> 1 min: 16) max: 1.
	oddPowersOfSelf := Array new: 1 << w.
	oddPowersOfSelf at: 1 put: (pow := self).
	square := self montgomeryTimes: self modulo: m mInvModB: mInv.
	2 to: oddPowersOfSelf size do: [:i | pow := oddPowersOfSelf at: i put: (pow montgomeryTimes: square modulo: m mInvModB: mInv)].
	
	"Now exponentiate by searching precomputed bit patterns with a sliding window"
	pow := y.
	[k > 0]
		whileTrue:
			[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
			"Skip bits set to zero (the sliding window)"
			(n bitAt: k) = 0
				ifFalse:
					["Find longest odd bit pattern up to window length (w + 1)"
					j := k - w max: 1.
					[j < k and: [(n bitAt: j) = 0]] whileTrue: [j := j + 1].
					"We found a bit pattern of length k-j+1;
					perform the square powers for each bit
					(same cost as bitwise algorithm);
					compute the index of this bit pattern in the precomputed powers."
					index := 0.
					[k > j] whileTrue:
						[pow := pow montgomeryTimes: pow modulo: m mInvModB: mInv.
						index := index << 1 + (n bitAt: k).
						k := k - 1].
					"Perform a single multiplication for the whole bit pattern.
					This saves up to (k-j) multiplications versus a naive algorithm operating bit by bit"
					pow := pow montgomeryTimes: (oddPowersOfSelf at: index + 1) modulo: m mInvModB: mInv].
			k := k - 1].
	^pow
</details>

#### Integer>>#printStringAsBytesDecimal

Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes. Useful for file-browsers. 123 printStringAsBytesDecimal (12*1000) printStringAsBytesDecimal (1000*1000) printStringAsBytesDecimal 1024 printStringAsBytesDecimal (12*1024) printStringAsBytesDecimal (1024*1024) printStringAsBytesDecimal (1024*1024*1024) printStringAsBytesDecimal (1024*1024*1024*1024) printStringAsBytesDecimal (30 factorial) printStringAsBytesDecimal See https://en.wikipedia.org/wiki/Kibibyte See #printStringAsBytes


<details>
	<summary>See more</summary>
	
	printStringAsBytesDecimal
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 printStringAsBytesDecimal
	(12*1000) printStringAsBytesDecimal
	(1000*1000) printStringAsBytesDecimal
	
	1024 printStringAsBytesDecimal
	(12*1024) printStringAsBytesDecimal
	(1024*1024) printStringAsBytesDecimal
	(1024*1024*1024) printStringAsBytesDecimal
	(1024*1024*1024*1024) printStringAsBytesDecimal
	(30 factorial) printStringAsBytesDecimal
	
	See https://en.wikipedia.org/wiki/Kibibyte
	See #printStringAsBytes
	"
	self withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
		^String streamContents: [ :strm |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixSymbol;
				nextPut: $B]]
</details>

#### Integer>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Catches failure if LgInt replace primitive fails


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart
	| j |  "Catches failure if LgInt replace primitive fails"
	j _ repStart.
	start to: stop do:
		[:i |
		self digitAt: i put: (replacement digitAt: j).
		j _ j+1]
</details>

#### Integer>>#bitInvert

Answer an Integer whose bits are the logical negation of the receiver's bits. Numbers are interpreted as having 2's-complement representation.


<details>
	<summary>See more</summary>
	
	bitInvert
	"Answer an Integer whose bits are the logical negation of the receiver's bits.
	Numbers are interpreted as having 2's-complement representation."

	^ -1 - self.
</details>

#### Integer>>#benchmark

Handy bytecode-heavy benchmark


<details>
	<summary>See more</summary>
	
	benchmark  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
    | size flags prime k count |
    size _ 8190.
    1 to: self do:
        [:iter |
        count _ 0.
        flags _ (Array new: size) atAllPut: true.
        1 to: size do:
            [:i | (flags at: i) ifTrue:
                [prime _ i+1.
                k _ i + prime.
                [k <= size] whileTrue:
                    [flags at: k put: false.
                    k _ k + prime].
                count _ count + 1]]].
    ^ count
</details>

#### Integer>>#montgomeryNumberOfDigits

Answer the number of montgomery digits required to represent the receiver.


<details>
	<summary>See more</summary>
	
	montgomeryNumberOfDigits
	"Answer the number of montgomery digits required to represent the receiver."
	^self digitLength * 8 + (self montgomeryDigitLength - 1) // self montgomeryDigitLength
</details>

#### Integer>>#>= aNumber

Answer whether the receiver is greater than or equal to the argument.


<details>
	<summary>See more</summary>
	
	>= aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) <= 0]
						ifFalse: [^(self digitCompare: aNumber) >= 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andSend: #>=
</details>

#### Integer>>#bitReverse: highBit

Reverse the bits of the receiver so that the lsb is the highBit'th bit of the answer. Translated from C code at: http://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious.


<details>
	<summary>See more</summary>
	
	bitReverse: highBit 
	"Reverse the bits of the receiver so that the lsb is the highBit'th bit of the answer.  Translated from C code at:  http://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious."
	| v r s |
	highBit < self highBit ifTrue: [ self error: 'Not enough bits.' ].
	v := self.
	r := v bitAnd: 1.
	s := highBit - 1.
	[ v := v bitShift: -1.
	v = 0 ] whileFalse:
		[ r := r bitShift: 1.
		r := r bitOr: (v bitAnd: 1).
		s := s - 1 ].
	^ r bitShift: s
</details>

#### Integer>>#asIntegerOrFloat

Convert to integer if integer, or to Float otherwhise.


<details>
	<summary>See more</summary>
	
	asIntegerOrFloat
	"Convert to integer if integer, or to Float otherwhise."

	^self
</details>

#### Integer>>#adaptToFraction: rcvr andSend: selector

If I am involved in arithmetic with a Fraction, convert me to a Fraction.


<details>
	<summary>See more</summary>
	
	adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert me to a Fraction."
	^ rcvr perform: selector with: self asFraction
</details>

#### Integer>>#asFraction

Answer a Fraction that represents the value of the receiver.


<details>
	<summary>See more</summary>
	
	asFraction
	"Answer a Fraction that represents the value of the receiver."

	^Fraction numerator: self denominator: 1
</details>

#### Integer>>#montgomeryDigitLength

Answer the number of bits composing a digit in Montgomery algorithm. Primitive use either 8 or 32 bits digits


<details>
	<summary>See more</summary>
	
	montgomeryDigitLength
	"Answer the number of bits composing a digit in Montgomery algorithm.
	Primitive use either 8 or 32 bits digits"
	<primitive: 'primMontgomeryDigitLength' module:'LargeIntegers'>
	^8 "Legacy plugin which did not have this primitive did use 8 bits digits"
</details>

#### Integer>>#bitOr: n

Answer an Integer whose bits are the logical OR of the receiver's bits and those of the argument, n.


<details>
	<summary>See more</summary>
	
	bitOr: n 
	"Answer an Integer whose bits are the logical OR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitOr' module:'LargeIntegers'>
	norm _ n normalize.
	^ self
		digitLogic: norm
		op: #bitOr:
		length: (self digitLength max: norm digitLength)
</details>

#### Integer>>#= aNumber

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aNumber

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue: [
		aNumber class == self class ifFalse: [ ^false ].
		^ (self digitCompare: aNumber) = 0].
	^ aNumber adaptToInteger: self andSend: #=
</details>

#### Integer>>#numberOfDigitsInBase: b

Return how many digits are necessary to print this number in base b. This does not count any place for minus sign, radix prefix or whatever. Note that this algorithm may cost a few operations on LargeInteger.


<details>
	<summary>See more</summary>
	
	numberOfDigitsInBase: b 
	"Return how many digits are necessary to print this number in base b.
	This does not count any place for minus sign, radix prefix or whatever.
	Note that this algorithm may cost a few operations on LargeInteger."

	| nDigits q total |
	self negative ifTrue: [^self negated numberOfDigitsInBase: b].
	self < b ifTrue: [^1].
	b isPowerOfTwo ifTrue: [^self highBit + b highBit - 2 quo: b highBit - 1].
	
	"A conversion from base 2 to base b has to be performed.
	This algorithm avoids Float computations like (self log: b) floor + 1,
	1) because they are inexact
	2) because LargeInteger might overflow
	3) because this algorithm might be cheaper than conversion"

	q := self.
	total := 0.
	["Make an initial nDigits guess that is lower than or equal to required number of digits"
	nDigits := b = 10
		ifTrue: [((q highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [q highBit quo: b highBit].
	total := total + nDigits.
	
	"See how many digits remains above these first nDigits guess"
	(q := q quo: (b raisedToInteger: nDigits)) < b] whileFalse.
	^q = 0
		ifTrue: [total]
		ifFalse: [total + 1]
</details>

#### Integer>>#bitClear: aMask

Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask.


<details>
	<summary>See more</summary>
	
	bitClear: aMask 
	"Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask."

	^ (self bitOr: aMask) - aMask
</details>

#### Integer>>#montgomeryDigitMax

Answer the maximum value of a digit used in Montgomery algorithm.


<details>
	<summary>See more</summary>
	
	montgomeryDigitMax
	"Answer the maximum value of a digit used in Montgomery algorithm."
	
	^1 << self montgomeryDigitLength - 1
</details>

#### Integer>>#printStringWithCommas

123456789 printStringWithCommas -123456789 printStringWithCommas


<details>
	<summary>See more</summary>
	
	printStringWithCommas
	"
	123456789 printStringWithCommas
	-123456789 printStringWithCommas
	"
	^String streamContents: [ :strm |
		self printOn: strm thousandSeparator: ',' includePlusSign: false ]
</details>

#### Integer>>#print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag

<details>
	<summary>See more</summary>
	
	print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag
	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
				ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
				ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
	
</details>

#### Integer>>#bitXor: n

Answer an Integer whose bits are the logical XOR of the receiver's bits and those of the argument, n.


<details>
	<summary>See more</summary>
	
	bitXor: n 
	"Answer an Integer whose bits are the logical XOR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitXor' module:'LargeIntegers'>
	norm _ n normalize.
	^ self
		digitLogic: norm
		op: #bitXor:
		length: (self digitLength max: norm digitLength)
</details>

#### Integer>>#digitCompare: arg

Compare the magnitude of self with that of arg. Return a code of 1, 0, -1 for self >, = , < arg


<details>
	<summary>See more</summary>
	
	digitCompare: arg 
	"Compare the magnitude of self with that of arg.   
	Return a code of 1, 0, -1 for self >, = , < arg"
	| len arglen argDigit selfDigit |
	<primitive: 'primDigitCompare' module:'LargeIntegers'>
	len _ self digitLength.
	(arglen _ arg digitLength) ~= len
		ifTrue: [arglen > len
				ifTrue: [^ -1]
				ifFalse: [^ 1]].
	[len > 0]
		whileTrue: 
			[(argDigit _ arg digitAt: len) ~= (selfDigit _ self digitAt: len)
				ifTrue: [argDigit < selfDigit
						ifTrue: [^ 1]
						ifFalse: [^ -1]].
			len _ len - 1].
	^ 0
</details>

#### Integer>>#ceiling

Refer to the comment in Number|ceiling.


<details>
	<summary>See more</summary>
	
	ceiling 
	"Refer to the comment in Number|ceiling."

	^self
</details>

#### Integer>>#explorerContents

<details>
	<summary>See more</summary>
	
	explorerContents

	^#(
		('hexadecimal' 16)
		('octal' 8)
		('binary' 2)) collect: [ :each |
			ObjectExplorerWrapper
				with: each first
				name: (self printStringBase: each second)
				model: self ]
</details>

#### Integer>>#/ aNumber

Refer to the comment in Number /


<details>
	<summary>See more</summary>
	
	/ aNumber
	"Refer to the comment in Number / "
	| quoRem |
	aNumber isInteger ifTrue:
		[quoRem _ self digitDiv: aNumber abs	"*****I've added abs here*****"
						neg: self negative ~~ aNumber negative.
		(quoRem at: 2) = 0
			ifTrue: [^ (quoRem at: 1) normalize]
			ifFalse: [^ (Fraction numerator: self denominator: aNumber) reduced]].
	^ aNumber adaptToInteger: self andSend: #/
</details>

#### Integer>>#benchFib

Handy send-heavy benchmark


<details>
	<summary>See more</summary>
	
	benchFib  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t _ Time millisecondsToRun: [r _ 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1] 
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]

</details>

#### Integer>>#digitRshift: anInteger bytes: b lookfirst: a

Shift right 8*b+anInteger bits, 0<=n<8. Discard all digits beyond a, and all zeroes at or below a.


<details>
	<summary>See more</summary>
	
	digitRshift: anInteger bytes: b lookfirst: a 
	 "Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."
	| n x r f m digit count i |
	n _ 0 - anInteger.
	x _ 0.
	f _ n + 8.
	i _ a.
	m _ 255 bitShift: 0 - f.
	digit _ self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x _ digit bitShift: f "Can't exceed 8 bits".
		i _ i - 1.
		digit _ self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r _ Integer new: i - b neg: self negative.
	count _ i.
	x _ (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit _ self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x) 
			"Avoid values > 8 bits".
		x _ digit bitShift: n].
	^r
</details>

#### Integer>>#factorial

<details>
	<summary>See more</summary>
	
	factorial

	self < 0 ifTrue: [self error: 'Undefined for negative integers'].
	^1 productTo: self
</details>

#### Integer>>#storeStringHex

<details>
	<summary>See more</summary>
	
	storeStringHex
	^self storeStringBase: 16
</details>

#### Integer>>#isPowerOfTwo

Return true if the receiver is an integral power of two.


<details>
	<summary>See more</summary>
	
	isPowerOfTwo
	"Return true if the receiver is an integral power of two."
	^ (self bitAnd: self-1) = 0
</details>

#### Integer>>#normalize

SmallInts OK; LgInts override


<details>
	<summary>See more</summary>
	
	normalize 
	"SmallInts OK; LgInts override"
	^ self
</details>

#### Integer>>#hex8

Print the receiver in base 16 with prefixed base, using at least 8 digits. DO NOT CHANGE THIS! The Cog VMMaker depends on this. Consider using storeStringBase: 16 length: 11 padded: true instead.


<details>
	<summary>See more</summary>
	
	hex8
	"Print the receiver in base 16 with prefixed base, using at least 8 digits.
	 DO NOT CHANGE THIS!  The Cog VMMaker depends on this.
	 Consider using storeStringBase: 16 length: 11 padded: true instead."
	  "16r3333 hex8"
	| hex |
	hex := self hex.  "16rNNN"
	^hex size < 11
		ifTrue: [hex copyReplaceFrom: 4 to: 3
						 with: ('00000000' copyFrom: 1 to: 11-hex size)]
		ifFalse: [hex]
</details>

#### Integer>>#bitAt: anInteger put: value

Answer a new Integer that has the bit of rank anInteger set to value. The bit value should be 0 or 1, otherwise raise an Error. The bits are indexed starting at 1 for the least significant bit. For negative integers, operate on 2-complement representation.


<details>
	<summary>See more</summary>
	
	bitAt: anInteger put: value
	"Answer a new Integer that has the bit of rank anInteger set to value.
	The bit value should be 0 or 1, otherwise raise an Error.
	The bits are indexed starting at 1 for the least significant bit.
	For negative integers, operate on 2-complement representation."
	
	| b |
	b := self bitAt: anInteger.
	b = value ifTrue: [^self].
	0 = value ifTrue: [^self bitAnd: (1 bitShift: anInteger - 1) bitInvert].
	1 = value ifTrue: [^self bitOr: (1 bitShift: anInteger - 1)].
	self error: 'bit value should be 0 or 1'
</details>

#### Integer>>#printStringRadix: anInteger

<details>
	<summary>See more</summary>
	
	printStringRadix: anInteger

	^anInteger printString, 'r', (self printStringBase: anInteger)
</details>

#### Integer>>#atRandom

Answer a random integer from 1 to self. This implementation uses a shared generator. Heavy users should their own implementation or use Interval>atRandom: directly.


<details>
	<summary>See more</summary>
	
	atRandom
	"Answer a random integer from 1 to self.  This implementation uses a
	shared generator. Heavy users should their own implementation or use
	Interval>atRandom: directly."
	"
	7 atRandom
	"

	self = 0 ifTrue: [ ^0 ].
	self < 0 ifTrue: [ ^self negated atRandom negated ].
	^ Random withDefaultDo: [ :random |
		self atRandom: random ]
</details>

#### Integer>>#take: anInteger

Answer the binomial coefficient (self anInteger)


<details>
	<summary>See more</summary>
	
	take: anInteger
	"Answer the binomial coefficient (self anInteger)"
	" 6 take: 3  "

	(anInteger < 0 or: [anInteger > self]) ifTrue: [^0].
	^(self - anInteger + 1 productTo: self) // anInteger factorial
</details>

#### Integer>>#printStringHex

<details>
	<summary>See more</summary>
	
	printStringHex
	^self printStringBase: 16
</details>

#### Integer>>#<= aNumber

Answer whether the receiver is less than or equal to the argument.


<details>
	<summary>See more</summary>
	
	<= aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) >= 0]
						ifFalse: [^ (self digitCompare: aNumber) <= 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andSend: #<=
</details>

#### Integer>>#digitMultiply: arg neg: ng

<details>
	<summary>See more</summary>
	
	digitMultiply: arg neg: ng 
	| prod prodLen carry digit k ab |
	<primitive: 'primDigitMultiplyNegative' module:'LargeIntegers'>
	(arg digitLength = 1 and: [(arg digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	(self digitLength = 1 and: [(self digitAt: 1)
			= 0])
		ifTrue: [^ 0].
	prodLen _ self digitLength + arg digitLength.
	prod _ Integer new: prodLen neg: ng.
	"prod starts out all zero"
	1 to: self digitLength do: [:i | (digit _ self digitAt: i) ~= 0
			ifTrue: 
				[k _ i.
				carry _ 0.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do: 
					[:j | 
					ab _ (arg digitAt: j)
								* digit + carry + (prod digitAt: k).
					carry _ ab bitShift: -8.
					prod digitAt: k put: (ab bitAnd: 255).
					k _ k + 1].
				prod digitAt: k put: carry]].
	^ prod normalize
</details>

#### Integer>>#asHexDigit

<details>
	<summary>See more</summary>
	
	asHexDigit
	^'0123456789ABCDEF' at: self+1
</details>

#### Integer>>#sumTo: anInteger

Answer self + self + 1 + ... + anInteger


<details>
	<summary>See more</summary>
	
	sumTo: anInteger
	"Answer self + self + 1 + ... + anInteger"
	
	| integers |
	integers := anInteger - self + 1.
	integers < 1 ifTrue: [^0].
	^self + anInteger * integers bitShift: -1
</details>

#### Integer>>#highBit

Answer the index of the high order bit of the receiver, or zero if the receiver is zero. Raise an error if the receiver is negative, since negative integers are defined to have an infinite number of leading 1's in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to get the highest bit of the magnitude.


<details>
	<summary>See more</summary>
	
	highBit
	"Answer the index of the high order bit of the receiver, or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to 
	get the highest bit of the magnitude."
	self < 0 ifTrue: [^ self error: 'highBit is not defined for negative integers'].
	^ self highBitOfMagnitude
</details>

#### Integer>>#floor

Refer to the comment in Number|floor.


<details>
	<summary>See more</summary>
	
	floor 
	"Refer to the comment in Number|floor."

	^self
</details>

#### Integer>>#noMask: mask

Treat the argument as a bit mask. Answer whether none of the bits that are 1 in the argument are 1 in the receiver.


<details>
	<summary>See more</summary>
	
	noMask: mask 
	"Treat the argument as a bit mask. Answer whether none of the bits that 
	are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask)
</details>

#### Integer>>#nthRoot: aPositiveInteger

Answer the nth root of the receiver. Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root.


<details>
	<summary>See more</summary>
	
	nthRoot: aPositiveInteger
	"Answer the nth root of the receiver.
	Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."
  
	| guess excess scaled nBits |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ DomainError signal: 'nth root only defined for positive Integer n.'].

	self = 0 ifTrue: [ ^0 ].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^  NegativePowerError new signalReceiver: self selector: #nthRoot: argument: aPositiveInteger  ].

	guess _ self nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - self.
	excess = 0 ifTrue: [ ^ guess ].

	nBits _ Float precision - guess highBitOfMagnitude.
	nBits <= 0 ifTrue: [ ^(Fraction numerator: guess * 4 - excess sign denominator: 4) asFloat].

	scaled _ self << (nBits * aPositiveInteger).
	guess _ scaled nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - scaled.
	^(Fraction numerator: guess * 4 - excess sign denominator: 1 << (nBits + 2)) asFloat
</details>

#### Integer>>#printStringBase: base length: minimum padded: zeroFlag

<details>
	<summary>See more</summary>
	
	printStringBase: base length: minimum padded: zeroFlag
	^String streamContents: [:s| self printOn: s base: base length: minimum padded: zeroFlag]
</details>

#### Integer>>#bitInvert32

Answer the 32-bit complement of the receiver.


<details>
	<summary>See more</summary>
	
	bitInvert32
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF
</details>

#### Integer>>#alignedTo: anInteger

Answer the smallest number not less than receiver that is a multiple of anInteger.


<details>
	<summary>See more</summary>
	
	alignedTo: anInteger
	"Answer the smallest number not less than receiver that is a multiple of anInteger."

	^(self+anInteger-1//anInteger)*anInteger

"5 alignedTo: 2"
"12 alignedTo: 3"
</details>

#### Integer>>#log: aNumber

Answer the log base aNumber of the receiver. (3 raisedTo: 215) log: 3 ((3 raisedTo: 215)+(3 raisedTo: 213)) log: 3 Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.


<details>
	<summary>See more</summary>
	
	log: aNumber
	"Answer the log base aNumber of the receiver.
	(3 raisedTo: 215) log: 3
	((3 raisedTo: 215)+(3 raisedTo: 213)) log: 3
	Answers an integer number if appropriate. Doing this is somewhat expensive. If you care about performance and not about using Floats, do 'aNumber asFloat log: another'.
	"
	| floatAnswer roundedAnswer |
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0'].
	floatAnswer _ self asFloat log: aNumber.
	roundedAnswer _ floatAnswer rounded.
	(aNumber raisedToInteger: roundedAnswer) = self
		ifTrue: [ ^roundedAnswer ].
	^floatAnswer
</details>

#### Integer>>#lowBit

Answer the index of the low order bit of this number


<details>
	<summary>See more</summary>
	
	lowBit
	"Answer the index of the low order bit of this number"

	| byte byteIndex byteSize |
	byteIndex _ 1.
	byteSize _ self digitLength.
	[ byteIndex <= byteSize ] whileTrue: [
		byte _ self at: byteIndex.
		byte > 0 ifTrue: [ ^ byteIndex - 1 * 8 + byte lowBit ].
		byteIndex _ byteIndex + 1 ].
	^ 0.
</details>

#### Integer>>#sqrtFloor

Return the integer part of the square root of self


<details>
	<summary>See more</summary>
	
	sqrtFloor
	"Return the integer part of the square root of self"

	| guess delta |
	guess := 1 bitShift: self highBit + 1 // 2.
	[
		delta := guess squared - self // (guess bitShift: 1).
		delta = 0 ] whileFalse: [
			guess := guess - delta ].
	^guess - 1
</details>

#### Integer>>#printStringWords

SmallInteger maxVal printStringWords


<details>
	<summary>See more</summary>
	
	printStringWords
	"
	SmallInteger maxVal printStringWords
	"
	| mils minus three num answer milCount |
	self = 0 ifTrue: [^'zero'].
	mils _ #('' ' thousand' ' million' ' billion' ' trillion' ' quadrillion' ' quintillion' ' sextillion' ' septillion' ' octillion' ' nonillion' ' decillion' ' undecillion' ' duodecillion' ' tredecillion' ' quattuordecillion' ' quindecillion' ' sexdecillion' ' septendecillion' ' octodecillion' ' novemdecillion' ' vigintillion').
	num _ self.
	minus _ ''.
	self < 0 ifTrue: [
		minus _ 'negative '.
		num _ num negated.
	].
	answer _ String new.
	milCount _ 1.
	[num > 0] whileTrue: [
		three _ (num \\ 1000) threeDigitName.
		num _ num // 1000.
		three isEmpty ifFalse: [
			answer isEmpty ifFalse: [
				answer _ ', ',answer
			].
			answer _ three,(mils at: milCount),answer.
		].
		milCount _ milCount + 1.
	].
	^minus,answer
</details>

#### Integer>>#<< shiftAmount

left shift


<details>
	<summary>See more</summary>
	
	<< shiftAmount  "left shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount
</details>

#### Integer>>#isInteger

True for all subclasses of Integer.


<details>
	<summary>See more</summary>
	
	isInteger
	"True for all subclasses of Integer."

	^ true
</details>

#### Integer>>#ifMultipleOf2And5Do: aBlock otherwise: anotherBlock

If our prime factorization consists only of 2's and 5's, evaluata aBlock with the exponents. Otherwise evaluate anotherBlock. Be fast!


<details>
	<summary>See more</summary>
	
	ifMultipleOf2And5Do: aBlock otherwise: anotherBlock
	"If our prime factorization consists only of 2's and 5's, evaluata aBlock with the exponents.
	Otherwise evaluate anotherBlock.
	Be fast!"

	| exponent2 exponent5 without2Factors |
	exponent2 _ self lowBit-1.
	without2Factors _ self bitShift: exponent2 negated.
	exponent5 _ ( 0.430676558073393 "2 ln / 5 ln" * without2Factors highBit) truncated.
	(5 raisedToInteger: exponent5) = without2Factors
		ifTrue: [
			aBlock value: exponent2 value: exponent5 ]
		ifFalse: [
			anotherBlock value ]
</details>

#### Integer>>#rounded

Refer to the comment in Number|rounded.


<details>
	<summary>See more</summary>
	
	rounded 
	"Refer to the comment in Number|rounded."

	^self
</details>

#### Integer>>#anyBitOfMagnitudeFrom: start to: stopArg

Tests for any magnitude bits in the interval from start to stopArg.


<details>
	<summary>See more</summary>
	
	anyBitOfMagnitudeFrom: start to: stopArg 
	"Tests for any magnitude bits in the interval from start to stopArg."
	"Primitive fixed in LargeIntegers v1.2. If you have an earlier version 
	comment out the primitive call (using this ST method then)."
	| magnitude firstDigitIx lastDigitIx rightShift leftShift stop |
	<primitive: 'primAnyBitFromTo' module:'LargeIntegers'>
	start < 1 | (stopArg < 1)
		ifTrue: [^ self error: 'out of range'].
	magnitude _ self abs.
	stop _ stopArg min: magnitude highBit.
	start > stop
		ifTrue: [^ false].
	firstDigitIx _ start - 1 // 8 + 1.
	lastDigitIx _ stop - 1 // 8 + 1.
	rightShift _ (start - 1 \\ 8) negated.
	leftShift _ 7 - (stop - 1 \\ 8).
	firstDigitIx = lastDigitIx
		ifTrue: [| digit mask | 
			mask _ (255 bitShift: rightShift negated)
						bitAnd: (255 bitShift: leftShift negated).
			digit _ magnitude digitAt: firstDigitIx.
			^ (digit bitAnd: mask)
				~= 0].
	((magnitude digitAt: firstDigitIx)
			bitShift: rightShift)
			~= 0
		ifTrue: [^ true].
	firstDigitIx + 1
		to: lastDigitIx - 1
		do: [:ix | (magnitude digitAt: ix)
					~= 0
				ifTrue: [^ true]].
	(((magnitude digitAt: lastDigitIx)
			bitShift: leftShift)
			bitAnd: 255)
			~= 0
		ifTrue: [^ true].
	^ false
</details>

#### Integer>>#is: aSymbol

Note: Senders might prefer #isInteger for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Note: Senders might prefer #isInteger for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Integer = aSymbol or: [ super is: aSymbol ]
</details>

#### Integer>>#bitShiftMagnitude: shiftCount

Answer an Integer whose value (in magnitude representation) is the receiver's value (in magnitude representation) shifted left by the number of bits indicated by the argument. Negative arguments shift right. Zeros are shifted in from the right in left shifts.


<details>
	<summary>See more</summary>
	
	bitShiftMagnitude: shiftCount 
	"Answer an Integer whose value (in magnitude representation) is  
	the receiver's value (in magnitude representation) shifted left by  
	the number of bits indicated by the argument. Negative arguments
	shift right. Zeros are shifted in from the right in left shifts."
	| rShift |
	<primitive: 'primDigitBitShiftMagnitude' module:'LargeIntegers'>
	shiftCount >= 0 ifTrue: [^ self digitLshift: shiftCount].
	rShift _ 0 - shiftCount.
	^ (self
		digitRshift: (rShift bitAnd: 7)
		bytes: (rShift bitShift: -3)
		lookfirst: self digitLength) normalize
</details>

#### Integer>>#printOn: aStream base: base length: minimum padded: zeroFlag

<details>
	<summary>See more</summary>
	
	printOn: aStream base: base length: minimum padded: zeroFlag
	| prefix |
	prefix := self negative ifTrue: ['-'] ifFalse: [String new].
	self print: (self abs printStringBase: base) on: aStream prefix: prefix length: minimum padded: zeroFlag

</details>

#### Integer>>#sqrt

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrt
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat _ self asFloat.
	floatResult _ selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess _ floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method."
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess _ self sqrtFloor.
			guess squared = self
				ifTrue: [ ^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we can which is the rounded sqrt."
			^ self sqrtRounded asFloat ].

	"We need an approximate result"
	^floatResult
</details>

#### Integer>>#montgomeryDigitBase

Answer the base used by Montgomery algorithm.


<details>
	<summary>See more</summary>
	
	montgomeryDigitBase
	"Answer the base used by Montgomery algorithm."
	^1 << self montgomeryDigitLength
</details>

#### Integer>>#nthRootTruncated: aPositiveInteger

Answer the integer part of the nth root of the receiver.


<details>
	<summary>See more</summary>
	
	nthRootTruncated: aPositiveInteger
          "Answer the integer part of the nth root of the receiver."
          | guess guessToTheNthMinusOne nextGuess |
          self = 0 ifTrue: [^0].
          self negative
                  ifTrue: [
                          aPositiveInteger even ifTrue: [ ^DomainError signal: 'Negative numbers don''t have even roots.' ].
                          ^(self negated nthRootTruncated: aPositiveInteger) negated].
          guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
          [
                  guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
                  nextGuess := (aPositiveInteger - 1 * guess * guessToTheNthMinusOne + self) // (guessToTheNthMinusOne * aPositiveInteger).
                 nextGuess >= guess ] whileFalse:
                          [ guess := nextGuess ].
          ( guess raisedTo: aPositiveInteger) > self  ifTrue:
                          [ guess := guess - 1 ].
          ^guess
</details>

#### Integer>>#productTo: anInteger

Answer self * (self + 1) * ... * anInteger


<details>
	<summary>See more</summary>
	
	productTo: anInteger
	"Answer self * (self + 1) * ... * anInteger"
	
	| integers answer |
	integers := anInteger - self + 1.
	integers < 1 ifTrue: [^1].
	integers > 7 ifTrue:
		[
			| half |
			half := integers bitShift: -1.
			^(self productTo: self + half)
				* (self + half + 1 productTo: anInteger)
		].
	answer := self.
	self + 1 to: anInteger do:
		[:each | answer := answer * each].
	^answer
</details>

#### Integer>>#bitShift: shiftCount

Answer an Integer whose value (in twos-complement representation) is the receiver's value (in twos-complement representation) shifted left by the number of bits indicated by the argument. Negative arguments shift right. Zeros are shifted in from the right in left shifts.


<details>
	<summary>See more</summary>
	
	bitShift: shiftCount 
	"Answer an Integer whose value (in twos-complement representation) is  
	the receiver's value (in twos-complement representation) shifted left by 
	the number of bits indicated by the argument. Negative arguments  
	shift right. Zeros are shifted in from the right in left shifts."
	| magnitudeShift |
	magnitudeShift _ self bitShiftMagnitude: shiftCount.
	^ ((self negative and: [shiftCount negative])
		and: [self anyBitOfMagnitudeFrom: 1 to: shiftCount negated])
		ifTrue: [magnitudeShift - 1]
		ifFalse: [magnitudeShift]
</details>

#### Integer>>#asCharacter

Answer the Character whose value is the receiver.


<details>
	<summary>See more</summary>
	
	asCharacter
	"Answer the Character whose value is the receiver."

	^Character numericValue: self
</details>

#### Integer>>#nextPrime

<details>
	<summary>See more</summary>
	
	nextPrime

	| pivot |
	self < 2 ifTrue: [^2].
	pivot := self + 1 bitOr: 1.
	[pivot isPrime] whileFalse: [pivot := pivot + 2].
	^pivot
</details>

#### Integer>>#* aNumber

Refer to the comment in Number *


<details>
	<summary>See more</summary>
	
	* aNumber
	"Refer to the comment in Number * " 
	aNumber isInteger ifTrue: [
		^ self
			digitMultiply: aNumber
			neg: (self negative xor: aNumber negative) ].
	^ aNumber adaptToInteger: self andSend: #*
</details>

#### Integer>>#reciprocalModulo2: n

Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if it doesn't exist.


<details>
	<summary>See more</summary>
	
	reciprocalModulo2: n
	"Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if it doesn't exist."
	| xgcd |
	self == 0 ifTrue: [^ nil].
	self == 1 ifTrue: [^ 1].
	xgcd _ self xgcd: n.
	^ (xgcd at: 1) == 1 ifTrue: [^ (xgcd at: 2) \\ n]
</details>

#### Integer>>#anyMask: mask

Treat the argument as a bit mask. Answer whether any of the bits that are 1 in the argument are 1 in the receiver.


<details>
	<summary>See more</summary>
	
	anyMask: mask 
	"Treat the argument as a bit mask. Answer whether any of the bits that 
	are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)
</details>

#### Integer>>#quo: aNumber

Refer to the comment in Number quo:


<details>
	<summary>See more</summary>
	
	quo: aNumber 
	"Refer to the comment in Number quo: "
	| ng quo |
	aNumber isInteger ifTrue: 
		[ng _ self negative == aNumber negative == false.
		quo _ (self digitDiv:
			(aNumber class == SmallInteger
				ifTrue: [aNumber abs]
				ifFalse: [aNumber])
			neg: ng) at: 1.
		^ quo normalize].
	^ aNumber adaptToInteger: self andSend: #quo:
</details>

#### Integer>>#printOn: aStream fractionDigits: placesDesired

Same as super, but provides a faster implementation because fraction part and rounding are trivial.


<details>
	<summary>See more</summary>
	
	printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."
	
	self printOn: aStream base: 10.

	placesDesired > 0
		ifTrue: [
			aStream nextPut: $..
			placesDesired timesRepeat: [
				aStream nextPut: $0 ]]
</details>

#### Integer>>#digitLshift: shiftCount

<details>
	<summary>See more</summary>
	
	digitLshift: shiftCount 
	| carry rShift mask len result digit byteShift bitShift highBit |
	(highBit _ self highBitOfMagnitude) = 0 ifTrue: [^ 0].
	len _ highBit + shiftCount + 7 // 8.
	result _ Integer new: len neg: self negative.
	byteShift _ shiftCount // 8.
	bitShift _ shiftCount \\ 8.
	bitShift = 0 ifTrue: ["Fast version for byte-aligned shifts"
		^ result
			replaceFrom: byteShift + 1
			to: len
			with: self
			startingAt: 1].
	carry _ 0.
	rShift _ bitShift - 8.
	mask _ 255 bitShift: 0 - bitShift.
	1 to: byteShift do: [:i | result digitAt: i put: 0].
	1 to: len - byteShift do: 
		[:i | 
		digit _ self digitAt: i.
		result digitAt: i + byteShift put: (((digit bitAnd: mask)
				bitShift: bitShift)
				bitOr: carry).
		carry _ digit bitShift: rShift].
	^ result
</details>

#### Integer>>#timesRepeat: aBlock

Evaluate the argument, aBlock, the number of times represented by the receiver.


<details>
	<summary>See more</summary>
	
	timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| count |
	count _ 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count _ count + 1]
</details>

#### Integer>>#bitAt: anInteger

Answer 1 if the bit at position anInteger is set to 1, 0 otherwise. self is considered an infinite sequence of bits, so anInteger can be any strictly positive integer. Bit at position 1 is the least significant bit. Negative numbers are in two-complements. This is a naive implementation that can be refined in subclass for speed


<details>
	<summary>See more</summary>
	
	bitAt: anInteger
	"Answer 1 if the bit at position anInteger is set to 1, 0 otherwise.
	self is considered an infinite sequence of bits, so anInteger can be any strictly positive integer.
	Bit at position 1 is the least significant bit.
	Negative numbers are in two-complements.
	
	This is a naive implementation that can be refined in subclass for speed"
	
	^(self bitShift: 1 - anInteger) bitAnd: 1
</details>

#### Integer>>#// aNumber

Integer division with truncation toward negative infinity. 9//4 = 2 -9//4 = -3 -0.9//0.4 = -3 #\\ answers the remainder from this division. See #//, #quo:, #div:


<details>
	<summary>See more</summary>
	
	// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ ZeroDivide new signalReceiver: self selector: #// argument: aNumber].
	aNumber isInteger ifFalse: [ ^super // aNumber ].
	self = 0 ifTrue: [^ 0].
	q _ self quo: aNumber.
	"Refer to the comment in Number>>#//."
	^(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
			ifTrue: [q - 1"Truncate towards minus infinity."]
			ifFalse: [q]
</details>

#### Integer>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer
	^true
</details>

#### Integer>>#digitAdd: arg

<details>
	<summary>See more</summary>
	
	digitAdd: arg 
	| len arglen accum sum |
	<primitive: 'primDigitAdd' module:'LargeIntegers'>
	accum _ 0.
	(len _ self digitLength) < (arglen _ arg digitLength) ifTrue: [len _ arglen].
	"Open code max: for speed"
	sum _ Integer new: len neg: self negative.
	1 to: len do: 
		[:i | 
		accum _ (accum bitShift: -8)
					+ (self digitAt: i) + (arg digitAt: i).
		sum digitAt: i put: (accum bitAnd: 255)].
	accum > 255
		ifTrue: 
			[sum _ sum growby: 1.
			sum at: sum digitLength put: (accum bitShift: -8)].
	^ sum
</details>

#### Integer>>#even

Refer to the comment in Number|even.


<details>
	<summary>See more</summary>
	
	even 
	"Refer to the comment in Number|even."

	^((self digitAt: 1) bitAnd: 1) = 0
</details>

#### Integer>>#\\\ anInteger

a modulo method for use in DSA. Be careful if you try to use this elsewhere.


<details>
	<summary>See more</summary>
	
	\\\ anInteger 
	"a modulo method for use in DSA. Be careful if you try to use this elsewhere."

	^self \\ anInteger
</details>

#### Integer>>#growby: n

<details>
	<summary>See more</summary>
	
	growby: n

	^self growto: self digitLength + n
</details>

#### Integer>>#tinyBenchmarks

Report the results of running the two tiny Squeak benchmarks. ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results Performance of early 80's experimental Smalltalk systems. Estimations from http://wiki.c2.com/?GreenBook http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1982-qtr4-magnolia-perf-graph.pdf http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1983-Magnolia-st-perf.pdf DEC PDP-11/23 5,000 bytecodes/sec (Green Book, p.128) Apple 5MHz 68000 9,000 to 11,000 bytecodes/sec (Green Book, p.187, awb) 5000 clocks/bytecode VAX-11/780 5MHz C HP Smalltalk 5,000 to 25,000 bytecodes/sec (Green Book, p.235) 330 clocks/bytecode VAX-11/780 5MHz C Berkeley Smalltalk 12,000 to 23000 bytecodes/sec (Green Book, p.203, awb) 300 clocks/bytecode DEC VAX-11/780 5MHz assembly 20,000 to 25,000 bytecodes/sec (Green Book, p.149, awb) 200 clocks/bytecode Xerox Dolphin µcode 20,000 to 40,000 bytecodes/sec (Green Book, p.44, p.203, awb) TEK Magnolia 10MHz 68000 50,000 bytecodes/sec (awb) 200 clocks/bytecode Xerox Dorado 14MHz µcode 400,000 to 500,000 bytecodes/sec (Green book, p.44, p.203, awb) 28 clocks/bytecode 0 tinyBenchmarks 292 MHz G3 Mac: 22,727,272 bytecodes/sec; 984,169 sends/sec 12.8 clocks/bytecode 400 MHz PII/Win98: 18,028,169 bytecodes/sec; 1,081,272 sends/sec 22.2 clocks/bytecode 900MHz RasPi2 - StackVM: 37,758,112 bytecodes/sec; 2,412,667 sends/sec 16.2 clocks/bytecode 900MHz RasPi2- CogSSpur: 157,441,574 bytecodes/sec; 10,946,039 sends/sec 5.7 clocks/bytecode 1GHz C.H.I.P. (*1) - StackVM: 55,315,471 bytecodes/sec; 3,348,667 sends/sec 18.1 clocks/bytecode 1GHz C.H.I.P. (*1) - CogSpur: 253,716,551 bytecodes/sec; 16,853,816 sends/sec 3.9 clocks/bytecode 1.2GHz RasPi3B - StackSpur 44,107,512 bytecodes/sec; 2,767,863 sends/sec 27.2 clocks/bytecode 1.2GHz RasPi3B - CogSpur 281,783,159 bytecodes/sec; 16,404,381 sends/sec 6.6 clocks/bytecode 1.66GHz Atom N450 - Cog: 244,274,809 bytecodes/sec; 28,795,277 sends/sec 6.8 clocks/bytecode 1.66GHz Atom N450 - CogSpur: 469,724,770 bytecodes/sec; 30,754,699 sends/sec 3.5 clocks/bytecode 1.33GHz Atom 3735G - Cog: 326,114,649 bytecodes/sec; 34,985,976 sends/sec 4.1 clocks/bytecode 1.33GHz Atom 3735G - CogSpur: 632,098,765 bytecodes/sec; 33,692,910 sends/sec 2.1 clocks/bytecode 1.5GHz AMD A4-5000 APU - Cog: 390,243,902 bytecodes/sec; 47,507,997 sends/sec 3.8 clocks/bytecode 1.5GHz AMD A4-5000 APU - CogSpur: 675,907,590 bytecodes/sec; 40,669,724 sends/sec 2.2 clocks/bytecode 1.5GHz AMD A4-5000 APU - CogSpur64: 659,368,963 bytecodes/sec; 50,338,916 sends/sec 2.2 clocks/bytecode 2.3GHz Tegra (*2) - StackVM: 258,847,320 bytecodes/sec; 13,014,759 sends/sec 8.9 clocks/bytecode 2.3GHz Tegra (*2) - CogSpur: 1,083,024,854 bytecodes/sec; 64,289,750 sends/sec 2.1 clocks/bytecode 3.1GHz Core i3-2100 - Cog: 1,203,290,246 bytecodes/sec; 165,723,327 sends/sec 2.6 clocks/bytecode 3.1GHz Core i3-2100 - CogSpur: 2,042,892,768 bytecodes/sec; 127,837,794 sends/sec 1.5 clocks/bytecode 2.70GHz Core i5-6400 -CogSpur64 3,162,934,362 bytecodes/sec; 243,321,293 sends/sec 0.85 clocks/bytecode (*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz (*2) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3' - 2.3 GHz It is interesting to note that Spur for the first time brings ARM hardware (RasPi and C.H.I.P.) into a 'Morphic is confortable and nice to use' level of performance.


<details>
	<summary>See more</summary>
	
	tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results


	Performance of early 80's experimental Smalltalk systems.
	Estimations from
		http://wiki.c2.com/?GreenBook
		http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1982-qtr4-magnolia-perf-graph.pdf
		http://www.wirfs-brock.com/allen/things/smalltalk-things/tektronix-smalltalk-document-archive/1983-Magnolia-st-perf.pdf

		DEC PDP-11/23 											  5,000 bytecodes/sec (Green Book, p.128)
		Apple 5MHz 68000 							   9,000 to 11,000 bytecodes/sec (Green Book, p.187, awb) 							5000 clocks/bytecode
		VAX-11/780 5MHz C HP Smalltalk 		  5,000 to 25,000 bytecodes/sec (Green Book, p.235) 								  330 clocks/bytecode
		VAX-11/780 5MHz C Berkeley Smalltalk 	 12,000 to 23000 bytecodes/sec (Green Book, p.203, awb) 							  300 clocks/bytecode
		DEC VAX-11/780 5MHz assembly 			20,000 to 25,000 bytecodes/sec (Green Book, p.149, awb) 				 			  200 clocks/bytecode
		Xerox Dolphin  µcode 					 	20,000 to 40,000 bytecodes/sec (Green Book, p.44, p.203, awb)
		TEK Magnolia 10MHz 68000 						      50,000 bytecodes/sec (awb) 												  200 clocks/bytecode
		Xerox Dorado 14MHz µcode			 400,000 to 500,000 bytecodes/sec (Green book, p.44, p.203, awb) 					    28 clocks/bytecode


	0 tinyBenchmarks
		
		292 MHz G3 Mac: 				        			 22,727,272 bytecodes/sec; 			    984,169 sends/sec 					12.8 clocks/bytecode
		400 MHz PII/Win98:  			        			 18,028,169 bytecodes/sec; 			1,081,272 sends/sec 					22.2 clocks/bytecode

		900MHz RasPi2 - StackVM: 						 37,758,112 bytecodes/sec; 		       2,412,667 sends/sec 					16.2 clocks/bytecode
		900MHz RasPi2- CogSSpur: 				     157,441,574 bytecodes/sec; 		     10,946,039 sends/sec 					   5.7 clocks/bytecode
		
		1GHz C.H.I.P. (*1) - StackVM: 					55,315,471 bytecodes/sec;  			3,348,667 sends/sec  					18.1 clocks/bytecode
		1GHz C.H.I.P. (*1) - CogSpur: 			 	    253,716,551 bytecodes/sec; 		    16,853,816 sends/sec 					   3.9 clocks/bytecode
		
		1.2GHz RasPi3B - StackSpur 					44,107,512 bytecodes/sec; 			2,767,863 sends/sec 					27.2 clocks/bytecode
		1.2GHz RasPi3B - CogSpur 					    281,783,159 bytecodes/sec; 		    16,404,381 sends/sec 					   6.6 clocks/bytecode

		1.66GHz Atom N450 - Cog: 					     244,274,809 bytecodes/sec; 		    28,795,277 sends/sec 					   6.8 clocks/bytecode
		1.66GHz Atom N450 - CogSpur: 			     469,724,770 bytecodes/sec; 		    30,754,699 sends/sec 					   3.5 clocks/bytecode
 
		1.33GHz Atom 3735G - Cog: 				     326,114,649 bytecodes/sec; 		    34,985,976 sends/sec 					   4.1 clocks/bytecode
		1.33GHz Atom 3735G - CogSpur: 			     632,098,765 bytecodes/sec; 		    33,692,910 sends/sec 					   2.1 clocks/bytecode

		1.5GHz AMD A4-5000 APU - Cog: 			    390,243,902 bytecodes/sec; 		    47,507,997 sends/sec 					   3.8 clocks/bytecode
		1.5GHz AMD A4-5000 APU - CogSpur: 		    675,907,590 bytecodes/sec; 		    40,669,724 sends/sec 					   2.2 clocks/bytecode
		1.5GHz AMD A4-5000 APU - CogSpur64:	    659,368,963 bytecodes/sec; 		    50,338,916 sends/sec 					   2.2 clocks/bytecode

		2.3GHz Tegra (*2) - StackVM: 				    258,847,320 bytecodes/sec; 		    13,014,759 sends/sec					   8.9 clocks/bytecode
		2.3GHz Tegra (*2) - CogSpur: 				1,083,024,854 bytecodes/sec; 		    64,289,750 sends/sec 					   2.1 clocks/bytecode

		3.1GHz Core i3-2100 - Cog:  				1,203,290,246 bytecodes/sec; 		165,723,327 sends/sec 					   2.6 clocks/bytecode
		3.1GHz Core i3-2100 - CogSpur:  			2,042,892,768 bytecodes/sec; 		127,837,794 sends/sec 					   1.5 clocks/bytecode

		2.70GHz Core i5-6400 -CogSpur64 		3,162,934,362 bytecodes/sec; 		243,321,293 sends/sec 					   0.85 clocks/bytecode
	
		(*1) C.H.I.P. $9 Computer [Next Thing Co], Allwinner R8 Single-Core ARM Cortex-A8 - 1 GHz
		(*2) ACER Chromebook CB5-311 - NVIDIA Tegra K1 Quad-Core ARM Cortex-A15 'r3'  - 2.3 GHz
		
	It is interesting to note that Spur for the first time brings ARM hardware (RasPi and C.H.I.P.) into a 'Morphic is confortable and nice to use' level of performance.
	"
	| t1 t2 r n1 n2 |
	n1 _ 1.
	[
		t1 _ Time millisecondsToRun: [n1 benchmark].
		t1 < 1000] 
			whileTrue: [n1 _ n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 _ 28.
	[
		t2 _ Time millisecondsToRun: [r _ n2 benchFib].
		t2 < 1000] 
			whileTrue: [n2 _ n2 + 1]. 
	"Note: #benchFib's runtime is about O(k^n),
		where k is the golden number = (1 + 5 sqrt) / 2 = 1.618...."

	^String streamContents: [ :strm |
		(n1 * 500000 * 1000) // t1 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Bytecodes/second; ' ].
		(r * 1000) // t2 withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixName;
				nextPutAll: 'Sends/second' ]]
</details>

#### Integer>>#hash

Hash is reimplemented because = is implemented. | s | s _ (1 to: 10000) asSet. [s includes: 123456] bench


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented.
	| s |
	s _ (1 to: 10000) asSet.
	[s includes: 123456] bench
	"

	"Can only happen in 64 bits images...
	See Float>>#hash, LargePositiveInteger>>#hash and LargeNegativeInteger>>#hash"
	self abs < Float maxExactInteger ifFalse: [
		^ self asFloat hash ].

	"In 32 bit image it will always go this way"
	^self hashMultiply
</details>

#### Integer>>#lastDigit

Answer the last digit of the integer base 256. LargePositiveInteger uses bytes of base two number, and each is a 'digit'.


<details>
	<summary>See more</summary>
	
	lastDigit
	"Answer the last digit of the integer base 256.  LargePositiveInteger uses bytes of base two number, and each is a 'digit'."

	^self digitAt: self digitLength
</details>

#### Integer>>#digitSubtract: arg

<details>
	<summary>See more</summary>
	
	digitSubtract: arg 
	| smaller larger z sum sl al ng |
	<primitive: 'primDigitSubtract' module:'LargeIntegers'>
	sl _ self digitLength.
	al _ arg digitLength.
	(sl = al
		ifTrue: 
			[[(self digitAt: sl)
				= (arg digitAt: sl) and: [sl > 1]]
				whileTrue: [sl _ sl - 1].
			al _ sl.
			(self digitAt: sl)
				< (arg digitAt: sl)]
		ifFalse: [sl < al])
		ifTrue: 
			[larger _ arg.
			smaller _ self.
			ng _ self negative == false.
			sl _ al]
		ifFalse: 
			[larger _ self.
			smaller _ arg.
			ng _ self negative].
	sum _ Integer new: sl neg: ng.
	z _ 0.
	"Loop invariant is -1<=z<=1"
	1 to: sl do: 
		[:i | 
		z _ z + (larger digitAt: i) - (smaller digitAt: i).
		sum digitAt: i put: z - (z // 256 * 256).
		"sign-tolerant form of (z bitAnd: 255)"
		z _ z // 256].
	^ sum normalize
</details>

#### Integer>>#< aNumber

Answer whether the receiver is less than the argument.


<details>
	<summary>See more</summary>
	
	< aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) > 0]
						ifFalse: [^ (self digitCompare: aNumber) < 0]]
			ifFalse: [^ self negative]].
	^ aNumber adaptToInteger: self andSend: #<
</details>

#### Integer>>#highBitOfMagnitude

Answer the position of the leading bit or zero if the receiver is zero. Receiver has to be positive!


<details>
	<summary>See more</summary>
	
	highBitOfMagnitude
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Receiver has to be positive!"

	| shifted bitNo |
	shifted := self < 0 ifTrue: [0 - self] ifFalse: [self].
	bitNo := 0.
	[shifted < 65536]
		whileFalse: 
			[shifted := shifted bitShift: -16.
			bitNo := bitNo + 16].
	shifted < 256
		ifFalse: 
			[shifted := shifted bitShift: -8.
			bitNo := bitNo + 8].
		
	"The high bits table can be obtained with:
	(1 to: 8) inject: #[0] into: [:highBits :rank | highBits , (highBits collect: [:e | rank])]."
	^bitNo + ( #[0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] at: shifted + 1)
</details>

#### Integer>>#raisedTo: n modulo: m

Answer the modular exponential. Note: this implementation is optimized for case of large integers raised to large powers.


<details>
	<summary>See more</summary>
	
	raisedTo: n modulo: m
	"Answer the modular exponential.
	Note: this implementation is optimized for case of large integers raised to large powers."
	| a s mInv |
	n = 0 ifTrue: [^1 \\ m].
	(self >= m or: [self < 0]) ifTrue: [^self \\ m raisedTo: n modulo: m].
	n < 0 ifTrue: [^(self reciprocalModulo: m) raisedTo: n negated modulo: m].
	(n < 4096 or: [m even])
		ifTrue:
			["Overhead of Montgomery method might cost more than naive divisions, use naive"
			^self slidingLeftRightRaisedTo: n modulo: m].
	
	mInv := self montgomeryDigitBase - ((m bitAnd: self montgomeryDigitMax) reciprocalModulo: self montgomeryDigitBase).
 
	"Initialize the result to R=self montgomeryDigitModulo raisedTo: m montgomeryNumberOfDigits"
	a := (1 bitShift: m montgomeryNumberOfDigits * m montgomeryDigitLength) \\ m.
	
	"Montgomerize self (multiply by R)"
	(s := self montgomeryTimes: (a*a \\ m) modulo: m mInvModB: mInv)
		ifNil:
			["No Montgomery primitive available ? fallback to naive divisions"
			^self slidingLeftRightRaisedTo: n modulo: m].

	"Exponentiate self*R"
	a := s montgomeryRaisedTo: n times: a modulo: m mInvModB: mInv.

	"Demontgomerize the result (divide by R)"
	^a montgomeryTimes: 1 modulo: m mInvModB: mInv
</details>

#### Integer>>#crossSumBase: aBase

Precondition


<details>
	<summary>See more</summary>
	
	crossSumBase: aBase
	|aResult|
	"Precondition"
	self assert:[aBase isInteger and: [aBase >=2]].

	self < 0 ifTrue: [^self negated crossSumBase: aBase].
	self < aBase ifTrue: [^ self].
	aResult := self \\ aBase + (self // aBase crossSumBase: aBase).

	"Postcondition
	E.g. 18 crossSumBase: 10 -> 9 => 18\\(10-1) = 0"
	self assert: [((aResult \\ (aBase - 1) = 0)) = ((self \\ (aBase - 1)) =0)].
	^aResult
</details>

#### Integer>>#allMask: mask

Treat the argument as a bit mask. Answer whether all of the bits that are 1 in the argument are 1 in the receiver.


<details>
	<summary>See more</summary>
	
	allMask: mask 
	"Treat the argument as a bit mask. Answer whether all of the bits that 
	are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)
</details>

#### Integer>>#ln

This function is defined because super ln might overflow.


<details>
	<summary>See more</summary>
	
	ln
	"This function is defined because super ln might overflow."
	| res h |
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0'].
	res := super ln.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 ln * h + (self / (1 << h)) asFloat ln
</details>

#### Integer>>#log2

(2 raisedTo: 215) log2 ((2 raisedTo: 215)+(2 raisedTo: 213)) log2 It would be cool to be able to give integer answers for other bases when appropriate...


<details>
	<summary>See more</summary>
	
	log2
	"
	(2 raisedTo: 215) log2
	((2 raisedTo: 215)+(2 raisedTo: 213)) log2
	It would be cool to be able to give integer answers for other bases when appropriate...
	"
	^self isPowerOfTwo
		ifTrue: [ self highBitOfMagnitude-1 ]
		ifFalse: [ super log2 ]
</details>

#### Integer>>#montgomeryTimes: a modulo: m mInvModB: mInv

Answer the result of a Montgomery multiplication self * a * (b raisedTo: m montgomeryNumberOfDigits) inv \\ m NOTE: it is assumed that: self montgomeryNumberOfDigits <= m montgomeryNumberOfDigits a montgomeryNumberOfDigits <= m montgomeryNumberOfDigits mInv * m \\ b = (-1 \\ b) = (b-1) (this implies m odd) where b = self montgomeryDigitBase Answer nil in case of absent plugin or other failure.


<details>
	<summary>See more</summary>
	
	montgomeryTimes: a modulo: m mInvModB: mInv
	"Answer the result of a Montgomery multiplication
	self * a * (b raisedTo: m montgomeryNumberOfDigits) inv \\ m
	NOTE: it is assumed that:
	self montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	a montgomeryNumberOfDigits <= m montgomeryNumberOfDigits
	mInv * m \\ b = (-1 \\ b) = (b-1) (this implies m odd)
	where b = self montgomeryDigitBase
	
	Answer nil in case of absent plugin or other failure."
	
	<primitive: 'primMontgomeryTimesModulo' module:'LargeIntegers'>
	^nil
</details>

#### Integer>>#nthRootRounded: aPositiveInteger

Answer the integer nearest the nth root of the receiver. http://stackoverflow.com/questions/39794338/precise-nth-root http://smallissimo.blogspot.com.ar/2011/09/clarifying-and-optimizing.html Ojo 32/64! Tambien http://smallissimo.blogspot.com.ar/2011/09/reviewing-fraction-asfloat.html


<details>
	<summary>See more</summary>
	
	nthRootRounded: aPositiveInteger
    "Answer the integer nearest the nth root of the receiver.
http://stackoverflow.com/questions/39794338/precise-nth-root

http://smallissimo.blogspot.com.ar/2011/09/clarifying-and-optimizing.html
Ojo 32/64!

Tambien
http://smallissimo.blogspot.com.ar/2011/09/reviewing-fraction-asfloat.html
"
    | guess |
    self = 0 ifTrue: [^0].
    self negative
        ifTrue: [
            aPositiveInteger even ifTrue: [ ^DomainError signal: 'Negative numbers don''t have even roots.' ].
            ^(self negated nthRootRounded: aPositiveInteger) negated].
    guess := self nthRootTruncated: aPositiveInteger.
    ^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
        ifTrue: [guess + 1]
        ifFalse: [guess]
</details>

#### Integer>>#sqrtRounded

<details>
	<summary>See more</summary>
	
	sqrtRounded

	| sqrtFloor |
	sqrtFloor := (self bitShift: 2) sqrtFloor.
	^(sqrtFloor bitShift: -1) + (sqrtFloor bitAnd: 1)
</details>

#### Integer>>#printOn: aStream base: b nDigits: n

Append a representation of this number in base b on aStream using nDigits. self must be positive.


<details>
	<summary>See more</summary>
	
	printOn: aStream base: b nDigits: n 
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self subclassResponsibility
</details>

#### Integer>>#> aNumber

Answer whether the receiver is greater than the argument.


<details>
	<summary>See more</summary>
	
	> aNumber
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^(self digitCompare: aNumber) < 0]
						ifFalse: [^(self digitCompare: aNumber) > 0]]
			ifFalse: [^ aNumber negative]].
	^ aNumber adaptToInteger: self andSend: #>
</details>

#### Integer>>#isProbablyPrimeWithK: k andQ: q

Algorithm P, probabilistic primality test, from Knuth, Donald E. 'The Art of Computer Programming', Vol 2, Third Edition, section 4.5.4, page 395, P1-P5 refer to Knuth description.. Note that this is a Miller Rabin test which may answer false positives (known as pseudoprimes) for at most 1/4 of the possible bases x.


<details>
	<summary>See more</summary>
	
	isProbablyPrimeWithK: k andQ: q 
	"Algorithm P, probabilistic primality test, from
	Knuth, Donald E. 'The Art of Computer Programming', Vol 2,
	Third Edition, section 4.5.4, page 395, P1-P5 refer to Knuth description..
	Note that this is a Miller Rabin test which may answer false positives (known as pseudoprimes) for at most 1/4 of the possible bases x."

	| x j y minusOne |
	"P1"
	x := (self - 2) atRandom + 1.
	"P2"
	j := 0.
	y := x raisedTo: q modulo: self.
	minusOne := self - 1.
	
	["P3"
	y = 1 ifTrue: [^j = 0].
	y = minusOne ifTrue: [^true].
	"P4"
	(j := j + 1) < k]
		whileTrue:
			[y := y squared \\ self].
	"P5"
	^false
</details>

#### Integer>>#storeOn: aStream base: base

Print a representation of the receiver on the stream <aStream> in base <base> where 2 <= <baseInteger> <= 16. If <base> is other than 10 it is written first separated by $r followed by the number like for example: 16rFCE2


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: base
	"Print a representation of the receiver on the stream
	<aStream> in base <base> where
	2 <= <baseInteger> <= 16. If <base> is other than 10
	it is written first separated by $r followed by the number
	like for example: 16rFCE2"

	| integer |
	integer := self negative
		ifTrue: [ aStream nextPut: $-. self negated ]
		ifFalse: [ self ].
	base = 10 ifFalse: [ aStream nextPutAll: base printString; nextPut: $r ].
	integer printOn: aStream base: base
</details>

#### Integer>>#+ aNumber

Refer to the comment in Number +


<details>
	<summary>See more</summary>
	
	+ aNumber
	"Refer to the comment in Number + "
	aNumber isInteger ifTrue:
		[self negative == aNumber negative
			ifTrue: [^ (self digitAdd: aNumber) normalize]
			ifFalse: [^ self digitSubtract: aNumber]].
	^ aNumber adaptToInteger: self andSend: #+
</details>

#### Integer>>#growto: n

<details>
	<summary>See more</summary>
	
	growto: n

	^self copyto: (self species new: n)
</details>

#### Integer>>#floatLog

Float, even if Integer result possible


<details>
	<summary>See more</summary>
	
	floatLog
	"Float, even if Integer result possible"
	| res h |
	res := super log.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 log * h + (self / (1 << h)) asFloat log
</details>

#### Integer>>#bitInvert16

Answer the 16-bit complement of the receiver.


<details>
	<summary>See more</summary>
	
	bitInvert16
	"Answer the 16-bit complement of the receiver."

	^ self bitXor: 16rFFFF
</details>

## LaggedFibonacciRandom

This class implements a two-tap Lagged Fibonacci random number generator, with lags 83 and 258.

### Methods
#### LaggedFibonacciRandom>>#ringAt: anInteger put: anObject

<details>
	<summary>See more</summary>
	
	ringAt: anInteger put: anObject

	| index |
	index := self clampRingIndex: anInteger.
	self ring at: index put: anObject
</details>

#### LaggedFibonacciRandom>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize

	self ring: self newRing.
	self last: 1.
	self initializeRingWith: ParkMiller93Random new
</details>

#### LaggedFibonacciRandom>>#newRing

<details>
	<summary>See more</summary>
	
	newRing

	^Array new: self majorLag
</details>

#### LaggedFibonacciRandom>>#last: anInteger

<details>
	<summary>See more</summary>
	
	last: anInteger

	last _ anInteger
</details>

#### LaggedFibonacciRandom>>#next

Answer a random Float in the interval [0, 1)


<details>
	<summary>See more</summary>
	
	next
	"Answer a random Float in the interval [0, 1)"

	| answer tap1 tap2 |
	tap1 _ self ringAt: self last.
	tap2 _ self ringAt: self last - self minorLag + 1.
	answer _ tap2 - tap1.
	answer < 0.0 ifTrue: [answer _ 1.0 + answer].
	self ringAt: self last put: answer.
	self setLast: self last + 1.
	^answer
</details>

#### LaggedFibonacciRandom>>#nextChunkBits

This method generates random instances of Integer in the interval 0 to 16r1FFFFFFFFFFFFF (53 bits).


<details>
	<summary>See more</summary>
	
	nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r1FFFFFFFFFFFFF (53 bits)."
	
	^self next timesTwoPower: self nextChunkSize :: truncated
</details>

#### LaggedFibonacciRandom>>#ring: aRing

<details>
	<summary>See more</summary>
	
	ring: aRing

	ring _ aRing
</details>

#### LaggedFibonacciRandom>>#nextChunkSize

Recall double precision floating point numbers in this generator's output range have an implicit mantissa bit


<details>
	<summary>See more</summary>
	
	nextChunkSize
	"Recall double precision floating point numbers in this
	generator's output range have an implicit mantissa bit"

	^53
</details>

#### LaggedFibonacciRandom>>#minorLag

<details>
	<summary>See more</summary>
	
	minorLag

	^83
</details>

#### LaggedFibonacciRandom>>#last

<details>
	<summary>See more</summary>
	
	last

	^last
</details>

#### LaggedFibonacciRandom>>#seed: anInteger

<details>
	<summary>See more</summary>
	
	seed: anInteger

	| random |
	random _ ParkMiller93Random seed: anInteger.
	self initializeRingWith: random.
	self last: 1
</details>

#### LaggedFibonacciRandom>>#ringAt: anInteger

<details>
	<summary>See more</summary>
	
	ringAt: anInteger

	| index |
	index := self clampRingIndex: anInteger.
	^self ring at: index
</details>

#### LaggedFibonacciRandom>>#setLast: anInteger

<details>
	<summary>See more</summary>
	
	setLast: anInteger

	anInteger > self majorLag
		ifTrue: [self last: anInteger - self majorLag]
		ifFalse: [self last: anInteger]
</details>

#### LaggedFibonacciRandom>>#clampRingIndex: anInteger

<details>
	<summary>See more</summary>
	
	clampRingIndex: anInteger

	^anInteger < 1
		ifTrue: [anInteger + self majorLag]
		ifFalse: [anInteger]
</details>

#### LaggedFibonacciRandom>>#ring

<details>
	<summary>See more</summary>
	
	ring

	^ring
</details>

#### LaggedFibonacciRandom>>#initializeRingWith: aRandom

The odds aRandom will produce self majorLag even integers in a row should be almost zero. So, if that effectively impossible event does happen, fail


<details>
	<summary>See more</summary>
	
	initializeRingWith: aRandom
	"The odds aRandom will produce self majorLag even integers in a row should be almost zero.
	So, if that effectively impossible event does happen, fail"

	| anyOdd |
	anyOdd := false.
	1 to: self ring size do:
		[:eachIndex |
			| nextInteger nextDouble |
			nextInteger _ aRandom nextBits: self nextChunkSize.
			anyOdd _ anyOdd or: [nextInteger odd].
			nextDouble _ nextInteger asFloat timesTwoPower: 0 - self nextChunkSize.
			self ring at: eachIndex put: nextDouble
		].
	anyOdd ifFalse: [self error: 'Initialization failed']
</details>

#### LaggedFibonacciRandom>>#majorLag

<details>
	<summary>See more</summary>
	
	majorLag

	^258
</details>

## LargeNegativeInteger

Just like LargePositiveInteger, but represents a negative number.

### Methods
#### LargeNegativeInteger>>#highBit

Answer the position of the leading bit or zero if the receiver is zero. Raise an error if the receiver is negative, since negative integers are defined to have an infinite number of leading 1's in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to get the highest bit of the magnitude.


<details>
	<summary>See more</summary>
	
	highBit
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."

	^self error: 'highBit is not defined for negative integers'
</details>

#### LargeNegativeInteger>>#sign

Optimization. Answer -1 since receiver is less than 0.


<details>
	<summary>See more</summary>
	
	sign
	"Optimization. Answer -1 since receiver is less than 0."

	^ -1

</details>

#### LargeNegativeInteger>>#bitAt: anInteger

super would not work because we have to pretend we are in two-complement. this has to be tricky...


<details>
	<summary>See more</summary>
	
	bitAt: anInteger
	"super would not work because we have to pretend we are in two-complement.
	this has to be tricky..."
	
	| digitIndex bitIndex i |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^1].
	bitIndex := (anInteger - 1 bitAnd: 2r111) + 1.

	i := 1.
	[i = digitIndex
		ifTrue:
			["evaluate two complement (bitInvert + 1) on the digit :
			(if digitIndex > 1, we must still add 1 due to the carry).
			but x bitInvert is -1-x, bitInvert+1 is just x negated..."
			^(self digitAt: digitIndex) negated bitAt: bitIndex].
	(self digitAt: i) = 0]
		whileTrue: [
			"two complement (bitInvert + 1) raises a carry:
			0 bitInvert -> 2r11111111.  2r11111111 + 1 -> 0 with carry...
			Thus we must inquire one digit forward"
			i := i + 1].
	
	"We escaped the while loop, because there is no more carry.
	Do a simple bitInvert without a carry"
	^1 - ((self digitAt: digitIndex) bitAt: bitIndex)
</details>

#### LargeNegativeInteger>>#abs

Answer a Number that is the absolute value (positive magnitude) of the receiver.


<details>
	<summary>See more</summary>
	
	abs
	^ self negated
</details>

#### LargeNegativeInteger>>#normalize

Check for leading zeroes and return shortened copy if so


<details>
	<summary>See more</summary>
	
	normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen minVal |
	<primitive: 'primNormalizeNegative' module: 'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.
	 Fast compute SmallInteger minVal digitLength"
	sLen := SmallInteger minVal < -16r80000000 "we're definitely on 64bit if we are smaller than (-2 raisedTo: 31)"
				ifTrue: [8]
				ifFalse: [4].
	len <= sLen ifTrue:
		[minVal := SmallInteger minVal.
		(len < sLen
		 or: [(self digitAt: sLen) < minVal lastDigit])
			ifTrue: ["If high digit less, then can be small"
					val := 0.
					len to: 1 by: -1 do:
						[:i | val := (val *256) - (self digitAt: i)].
					^ val].
		1 to: sLen do:  "If all digits same, then = minVal"
			[:i | (self digitAt: i) = (minVal digitAt: i)
					ifFalse: ["Not so; return self shortened"
							len < oldLen
								ifTrue: [^ self growto: len]
								ifFalse: [^ self]]].
		^ minVal].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]
</details>

#### LargeNegativeInteger>>#sqrt

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrt
	"Answer the square root of the receiver."
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}
</details>

#### LargeNegativeInteger>>#hash

Hash is reimplemented because = is implemented. | s | s _ (1 to: 10000) asSet. [s includes: 123456] bench


<details>
	<summary>See more</summary>
	
	hash

	self > `(2 raisedToInteger: Float emax+1) negated` ifFalse: [
		^ `Float negativeInfinity hash` ].

	self > Float maxExactInteger negated ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargePositiveInteger>>#hash"
		^ self asFloat hash ].

	"May normally only reach here in 32 bit images"

	"If could be a SmallInteger (regardless of the current word size, we want consistency between 32/64 bit systems)"
	self digitLength <= 8 ifTrue: [
		^ self hashMultiply ].

	^ByteArray
		hashBytes: self
		startingWith: self species hash
</details>

#### LargeNegativeInteger>>#negative

Answer whether the receiver is mathematically negative.


<details>
	<summary>See more</summary>
	
	negative
	"Answer whether the receiver is mathematically negative."

	^ true
</details>

#### LargeNegativeInteger>>#asFloat

Answer a Float that best approximates the value of the receiver. This algorithm is optimized to process only the significant digits of a LargeInteger. And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below).


<details>
	<summary>See more</summary>
	
	asFloat
	^self negated asFloat negated
</details>

#### LargeNegativeInteger>>#negated

Answer a Number that is the negation of the receiver.


<details>
	<summary>See more</summary>
	
	negated
	^ self copyto: (LargePositiveInteger new: self digitLength)
</details>

#### LargeNegativeInteger>>#positive

Answer whether the receiver is positive or equal to 0. (ST-80 protocol). See also strictlyPositive


<details>
	<summary>See more</summary>
	
	positive
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ false
</details>

#### LargeNegativeInteger>>#strictlyPositive

Answer whether the receiver is mathematically positive.


<details>
	<summary>See more</summary>
	
	strictlyPositive
	"Answer whether the receiver is mathematically positive."

	^ false
</details>

#### LargeNegativeInteger>>#printOn: aStream base: b

Append a representation of this number in base b on aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream base: b
	"Append a representation of this number in base b on aStream."
	
	aStream nextPut: $-.
	self abs printOn: aStream base: b
</details>

## LargePositiveInteger

I represent positive integers of more than 30 bits (ie, >= 1073741824). These values are beyond the range of SmallInteger, and are encoded here as an array of 8-bit digits. Care must be taken, when new values are computed, that any result that COULD BE a SmallInteger IS a SmallInteger (see normalize). Note that the bit manipulation primitives, bitAnd:, bitShift:, etc., = and ~= run without failure (and therefore fast) if the value fits in 32 bits. This is a great help to the simulator.

### Methods
#### LargePositiveInteger>>#highBit

Answer the position of the leading bit or zero if the receiver is zero. Raise an error if the receiver is negative, since negative integers are defined to have an infinite number of leading 1's in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to get the highest bit of the magnitude.


<details>
	<summary>See more</summary>
	
	highBit
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."
	^ self highBitOfMagnitude
</details>

#### LargePositiveInteger>>#mightBeASquare

In base 16, a square number can end only with 0,1,4 or 9 and - in case 0, only 0,1,4,9 can precede it, - in case 4, only even numbers can precede it. See http://en.wikipedia.org/wiki/Square_number So, in hex, the last byte must be one of: 00 10 40 90 x1 e4 x9 where x is any hex digit and e is any even digit Also, the receiver must be an aven power of two. This needs additional testing in the 00 suffix, as it implied in all the other conditions


<details>
	<summary>See more</summary>
	
	mightBeASquare
	"In base 16, a square number can end only with 0,1,4 or 9 and
	- in case 0, only 0,1,4,9 can precede it,
	- in case 4, only even numbers can precede it.
	See http://en.wikipedia.org/wiki/Square_number
	So, in hex, the last byte must be one of:
		00
		10
		40
		90
		x1
		e4
		x9
	where x is any hex digit and e is any even digit
	
	Also, the receiver must be an aven power of two.
	This needs additional testing in the 00 suffix,
	as it implied in all the other conditions"
	| lsb |
	lsb _ self digitAt: 1.
	^(lsb = 0 and: [ self lowBit odd ])		"00 (and even power of 2)"
		or: [ lsb = 16r40					"40"
		or: [ (lsb bitAnd: 16r7) = 1			"any|1 or any|9"
		or: [ (lsb bitAnd: 16r1F) = 4			"even|4"
		or: [ (lsb bitAnd: 16r7F) = 16 ]]]]	"10 or 90"
</details>

#### LargePositiveInteger>>#bitReverse: highBit

This implementation is faster than super


<details>
	<summary>See more</summary>
	
	bitReverse: highBit 
	"This implementation is faster than super"
	
	| digitSize reversed |
	highBit < self highBit ifTrue: [ self error: 'Not enough bits.' ].
	digitSize := highBit + 7 // 8.
	reversed := self class new: digitSize.
	1 to: self digitLength do: [:i |
		reversed digitAt: digitSize + 1 - i put: (self digitAt: i) byteReversed].
	^reversed bitShift: highBit - (digitSize * 8)
</details>

#### LargePositiveInteger>>#>= anInteger

Primitive. Compare the receiver with the argument and answer true if the receiver is greater than or equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	>= anInteger 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger or a LargePositiveInteger
	less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 26>
	^super >= anInteger
</details>

#### LargePositiveInteger>>#sign

Optimization. Answer 1 since receiver is greater than 0.


<details>
	<summary>See more</summary>
	
	sign
	"Optimization. Answer 1 since receiver is greater than 0."

	^ 1

</details>

#### LargePositiveInteger>>#bitAt: anInteger

Optimize super algorithm to avoid long bit operations. Instead work on digits which are known to be SmallInteger and fast. Note that this algorithm does not work for negative integers.


<details>
	<summary>See more</summary>
	
	bitAt: anInteger
	"Optimize super algorithm to avoid long bit operations.
	Instead work on digits which are known to be SmallInteger and fast.
	Note that this algorithm does not work for negative integers."
	
	| digitIndex bitIndex |
	digitIndex := anInteger - 1 // 8 + 1.
	digitIndex > self digitLength ifTrue: [^0].
	bitIndex := (anInteger - 1 bitAnd: 2r111) + 1.
	^(self digitAt: digitIndex) bitAt: bitIndex
</details>

#### LargePositiveInteger>>#// anInteger

Primitive. Divide the receiver by the argument and return the result. Round the result down towards negative infinity to make it a whole integer. Fail if the argument is 0. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	// anInteger 
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards negative infinity to make it a whole
	integer. Fail if the argument is 0. Fail if either the argument or the
	result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive. "

	<primitive: 32>
	^super // anInteger
</details>

#### LargePositiveInteger>>#abs

Answer a Number that is the absolute value (positive magnitude) of the receiver.


<details>
	<summary>See more</summary>
	
	abs
</details>

#### LargePositiveInteger>>#hashMultiply

Truncate to 28 bits and try again


<details>
	<summary>See more</summary>
	
	hashMultiply
	"Truncate to 28 bits and try again"

	^(self bitAnd: 16rFFFFFFF) hashMultiply
</details>

#### LargePositiveInteger>>#\\\ anInteger

a faster modulo method for use in DSA. Be careful if you try to use this elsewhere


<details>
	<summary>See more</summary>
	
	\\\ anInteger 
	"a faster modulo method for use in DSA. Be careful if you try to use this elsewhere"

	^(self digitDiv: anInteger neg: false) second
</details>

#### LargePositiveInteger>>#hash

Hash is reimplemented because = is implemented. | s | s _ (1 to: 10000) asSet. [s includes: 123456] bench


<details>
	<summary>See more</summary>
	
	hash

	self < `(2 raisedToInteger: Float emax+1)` ifFalse: [
		^ `Float infinity hash` ].

	self < Float maxExactInteger ifFalse: [
		"Will always enter here for 64 bits images.
		See Float>>#hash, Integer>>#hash and LargeNegativeInteger>>#hash"
		^ self asFloat hash ].

	"May normally only reach here in 32 bit images"

	"If could be a SmallInteger (regardless of the current word size, we want consistency between 32/64 bit systems)"
	self digitLength <= 8 ifTrue: [
		^ self hashMultiply ].

	^ByteArray
		hashBytes: self
		startingWith: self species hash
</details>

#### LargePositiveInteger>>#asFloat

Answer a Float that best approximates the value of the receiver. This algorithm is optimized to process only the significant digits of a LargeInteger. And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below).


<details>
	<summary>See more</summary>
	
	asFloat
	"Answer a Float that best approximates the value of the receiver.
	This algorithm is optimized to process only the significant digits of a LargeInteger.
	And it does honour IEEE 754 round to nearest even mode in case of excess precision (see details below)."
	
	"How numbers are rounded in IEEE 754 default rounding mode:
	A shift is applied so that the highest 53 bits are placed before the floating point to form a mantissa.
	The trailing bits form the fraction part placed after the floating point.
	This fractional number must be rounded to the nearest integer.
	If fraction part is 2r0.1, exactly between two consecutive integers, there is a tie.
	The nearest even integer is chosen in this case.
	Examples (First 52bits of mantissa are omitted for brevity):
	2r0.00001 is rounded downward to 2r0
	2r1.00001 is rounded downward to 2r1
	2r0.1 is a tie and rounded to 2r0 (nearest even)
	2r1.1 is a tie and rounded to 2r10 (nearest even)
	2r0.10001 is rounded upward to 2r1
	2r1.10001 is rounded upward to 2r10
	Thus, if the next bit after floating point is 0, the mantissa is left unchanged.
	If next bit after floating point is 1, an odd mantissa is always rounded upper.
	An even mantissa is rounded upper only if the fraction part is not a tie."
	
	"Algorihm details:
	Floating point hardware will correctly handle the rounding by itself with a single inexact operation if mantissa has one excess bit of precision.
	Except in the last case when extra bits are present after an even mantissa, we must round upper by ourselves.
	Note 1: the inexact flag in floating point hardware must not be trusted because it won't take into account the bits we truncated by ourselves.
	Note 2: the floating point hardware is presumed configured in default rounding mode."
	
	| mantissa shift sum excess |

	"Check how many bits excess the maximum precision of a Float mantissa."
	excess := self highBitOfMagnitude - Float precision.
	excess > 1
		ifTrue:
			["Remove the excess bits but one."
			mantissa := self bitShift: 1 - excess.
			shift := excess - 1.
			"Handle the case of extra bits truncated after an even mantissa."
			((mantissa bitAnd: 2r11) = 2r01 and: [self anyBitOfMagnitudeFrom: 1 to: shift])
				ifTrue: [mantissa := mantissa + 1]]
		ifFalse:
			[mantissa := self.
			shift := 0].

	"Now that mantissa has at most 1 excess bit of precision, let floating point operations perform the final rounding."
	sum := 0.0.
	1 to: mantissa digitLength do:
		[:byteIndex | 
		sum := sum + ((mantissa digitAt: byteIndex) asFloat timesTwoPower: shift).
		shift := shift + 8].
	^sum
</details>

#### LargePositiveInteger>>#digitAt: index put: value

Primitive. Store the second argument (value) in the indexable field of the receiver indicated by index. Fail if the value is negative or is larger than 255. Fail if the index is not an Integer or is out of bounds. Answer the value that was stored. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	digitAt: index put: value 
	"Primitive. Store the second argument (value) in the indexable field of 
	the receiver indicated by index. Fail if the value is negative or is larger 
	than 255. Fail if the index is not an Integer or is out of bounds. Answer 
	the value that was stored. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 61>
	^super at: index put: value
</details>

#### LargePositiveInteger>>#negated

Answer a Number that is the negation of the receiver.


<details>
	<summary>See more</summary>
	
	negated 
	^ (self copyto: (LargeNegativeInteger new: self digitLength))
		normalize  "Need to normalize to catch SmallInteger minVal"
</details>

#### LargePositiveInteger>>#< anInteger

Primitive. Compare the receiver with the argument and answer true if the receiver is less than the argument. Otherwise answer false. Fail if the argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	< anInteger 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 23>
	^super < anInteger
</details>

#### LargePositiveInteger>>#highBitOfMagnitude

Answer the position of the leading bit or zero if the receiver is zero. This method is used for LargeNegativeIntegers as well, since Squeak's LargeIntegers are sign/magnitude.


<details>
	<summary>See more</summary>
	
	highBitOfMagnitude
	"Answer the position of the leading bit or zero if the  
	receiver is zero. 
	This method is used for LargeNegativeIntegers as well,  
	since Squeak's LargeIntegers are sign/magnitude."
	| byteIndex msByte |
	byteIndex := self digitLength.
	[byteIndex > 0] whileTrue:
		[
			msByte  := self at: byteIndex.
			msByte  > 0 ifTrue: [^byteIndex - 1 * 8 + msByte  highBit].
			byteIndex := byteIndex - 1
		].
	^0
</details>

#### LargePositiveInteger>>#bitXor: arg

Primitive 36 deals with only 64-bit values (up to 8 byte LargeIntegers). The inherited <primitive: 'primDigitBitXor' module:'LargeIntegers'> deals with arbitrary sized large integers, but is much slower. This method gives a performance improvement for integers using 32 to 64 bits on 32 bit VMs, but only for 62 to 64 bits on 64 bits VMs. See http://forum.world.st/Integer-arithmetic-and-bit-operations-in-Squeak-and-Pharo-32bit-amp-64bit-tc4928994.html#none


<details>
	<summary>See more</summary>
	
	bitXor: arg
	"Primitive 36 deals with only 64-bit values (up to 8 byte LargeIntegers).
	The inherited <primitive: 'primDigitBitXor' module:'LargeIntegers'> deals with 
	arbitrary sized large integers, but is much slower.
	This method gives a performance improvement for integers using 32 to 64 bits on 32 bit VMs,
	but only for 62 to 64 bits on 64 bits VMs.
	See http://forum.world.st/Integer-arithmetic-and-bit-operations-in-Squeak-and-Pharo-32bit-amp-64bit-tc4928994.html#none
	"

    <primitive:36>
    ^super bitXor: arg
</details>

#### LargePositiveInteger>>#strictlyPositive

Answer whether the receiver is mathematically positive.


<details>
	<summary>See more</summary>
	
	strictlyPositive
	"Answer whether the receiver is mathematically positive."

	^ true
</details>

#### LargePositiveInteger>>#printOn: aStream base: b

Append a representation of this number in base b on aStream. In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits.


<details>
	<summary>See more</summary>
	
	printOn: aStream base: b
	"Append a representation of this number in base b on aStream.
	In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits."
	
	| halfDigits halfPower head tail nDigitsUnderestimate |
	"Don't engage any arithmetic if not normalized"
	(self digitLength = 0 or: [(self digitAt: self digitLength) = 0]) ifTrue: [^self normalize printOn: aStream base: b].
	
	nDigitsUnderestimate := b = 10
		ifTrue: [((self highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [self highBit quo: b highBit].
		
	"splitting digits with a whole power of two is more efficient"
	halfDigits := 1 bitShift: nDigitsUnderestimate highBit - 2.
	
	halfDigits <= 1
		ifTrue: ["Hmmm, this could happen only in case of a huge base b... Let lower level fail"
			^self printOn: aStream base: b nDigits: (self numberOfDigitsInBase: b)].
	
	"Separate in two halves, head and tail"
	halfPower := b raisedToInteger: halfDigits.
	head := self quo: halfPower.
	tail := self - (head * halfPower).
	
	"print head"
	head printOn: aStream base: b.
	
	"print tail without the overhead to count the digits"
	tail printOn: aStream base: b nDigits: halfDigits
</details>

#### LargePositiveInteger>>#/ anInteger

Primitive. Divide the receiver by the argument and answer with the result if the division is exact. Fail if the result is not a whole integer. Fail if the argument is 0. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	/ anInteger 
	"Primitive. Divide the receiver by the argument and answer with the
	result if the division is exact. Fail if the result is not a whole integer.
	Fail if the argument is 0. Fail if either the argument or the result is not
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 30>
	^super / anInteger
</details>

#### LargePositiveInteger>>#digitLength

Primitive. Answer the number of indexable fields in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	digitLength
	"Primitive. Answer the number of indexable fields in the receiver. This 
	value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed
</details>

#### LargePositiveInteger>>#printOn: aStream base: b nDigits: n

Append a representation of this number in base b on aStream using n digits. In order to reduce cost of LargePositiveInteger ops, split the number of digts approximatily in two Should be invoked with: 0 <= self < (b raisedToInteger: n)


<details>
	<summary>See more</summary>
	
	printOn: aStream base: b nDigits: n
	"Append a representation of this number in base b on aStream using n digits.
	In order to reduce cost of LargePositiveInteger ops, split the number of digts approximatily in two
	Should be invoked with: 0 <= self < (b raisedToInteger: n)"
	
	| halfPower half head tail |
	n <= 1 ifTrue: [
		n <= 0 ifTrue: [self error: 'Number of digits n should be > 0'].
		
		"Note: this is to stop an infinite loop if one ever attempts to print with a huge base
		This can happen because choice was to not hardcode any limit for base b
		We let Character>>#digitValue: fail"
		^aStream nextPut: (Character digitValue: self) ].
	halfPower := n bitShift: -1.
	half := b raisedToInteger: halfPower.
	head := self quo: half.
	tail := self - (head * half).
	head printOn: aStream base: b nDigits: n - halfPower.
	tail printOn: aStream base: b nDigits: halfPower
</details>

#### LargePositiveInteger>>#> anInteger

Primitive. Compare the receiver with the argument and answer true if the receiver is greater than the argument. Otherwise answer false. Fail if the argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	> anInteger 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 24>
	^super > anInteger
</details>

#### LargePositiveInteger>>#normalize

Check for leading zeroes and return shortened copy if so


<details>
	<summary>See more</summary>
	
	normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen |
	<primitive: 'primNormalizePositive' module:'LargeIntegers'>
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range.  Fast compute SmallInteger maxVal digitLength"
	sLen := SmallInteger maxVal > 16r7FFFFFFF "we're definitely on 64bit if we are larger than (2 raisedTo: 31) - 1"
				ifTrue: [8]
				ifFalse: [4].
	(len <= sLen
	 and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val := 0.
				len to: 1 by: -1 do:
					[:i | val := (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self]
</details>

#### LargePositiveInteger>>#sqrt

If we know for sure no exact solution exists, and we have a reasonable float approximation, then answer it without wasting time.


<details>
	<summary>See more</summary>
	
	sqrt
	"If we know for sure no exact solution exists, and we have a reasonable  float approximation,
	then answer it without wasting time."
	| selfAsFloat |
	self mightBeASquare ifFalse: [
		selfAsFloat _ self asFloat.
		selfAsFloat isFinite ifTrue: [
			^self asFloat sqrt ]].

	"If some exact solution might exist, or self asFloat isInfinite, call potentially expensive super"
	^super sqrt
</details>

#### LargePositiveInteger>>#negative

Answer whether the receiver is mathematically negative.


<details>
	<summary>See more</summary>
	
	negative
	"Answer whether the receiver is mathematically negative."

	^ false
</details>

#### LargePositiveInteger>>#+ anInteger

Primitive. Add the receiver to the argument and answer with an Integer result. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	+ anInteger 
	"Primitive. Add the receiver to the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 21>
	^super + anInteger
</details>

#### LargePositiveInteger>>#- anInteger

Primitive. Subtract the argument from the receiver and answer with an Integer result. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	- anInteger 
	"Primitive. Subtract the argument from the receiver and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 22>
	^super - anInteger
</details>

#### LargePositiveInteger>>#digitAt: index

Primitive. Answer the value of an indexable field in the receiver. LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256. Fail if the argument (the index) is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	digitAt: index 
	"Primitive. Answer the value of an indexable field in the receiver.   LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds. Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	self digitLength < index
		ifTrue: [^0]
		ifFalse: [^super at: index]
</details>

#### LargePositiveInteger>>#isLarge

<details>
	<summary>See more</summary>
	
	isLarge
	^true
</details>

#### LargePositiveInteger>>#withAtLeastNDigits: desiredLength

<details>
	<summary>See more</summary>
	
	withAtLeastNDigits: desiredLength

	| new |

	self size >= desiredLength ifTrue: [^self].
	new _ self class new: desiredLength.
	new
		replaceFrom: 1 
		to: self size 
		with: self 
		startingAt: 1.
	^new
</details>

#### LargePositiveInteger>>#<= anInteger

Primitive. Compare the receiver with the argument and answer true if the receiver is less than or equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	<= anInteger 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 25>
	^super <= anInteger
</details>

#### LargePositiveInteger>>#\\ anInteger

Primitive. Take the receiver modulo the argument. The result is the remainder rounded towards negative infinity, of the receiver divided by the argument. Fail if the argument is 0. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	\\ anInteger 
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided
	by the argument. Fail if the argument is 0. Fail if either the argument
	or the result is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 31>
	^super \\ anInteger
</details>

#### LargePositiveInteger>>#* anInteger

Primitive. Multiply the receiver by the argument and answer with an Integer result. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	* anInteger 
	"Primitive. Multiply the receiver by the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 29>
	^super * anInteger
</details>

#### LargePositiveInteger>>#quo: anInteger

Primitive. Divide the receiver by the argument and return the result. Round the result down towards zero to make it a whole integer. Fail if the argument is 0. Fail if either the argument or the result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	quo: anInteger 
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards zero to make it a whole integer. Fail if
	the argument is 0. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 33>
	^super quo: anInteger
</details>

#### LargePositiveInteger>>#positive

Answer whether the receiver is positive or equal to 0. (ST-80 protocol). See also strictlyPositive


<details>
	<summary>See more</summary>
	
	positive
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ true
</details>

#### LargePositiveInteger>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

## Number

Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity. All of Number's subclasses participate in a simple type coercion mechanism that supports mixed-mode arithmetic and comparisons. It works as follows: If self<typeA> op: arg<typeB> fails because of incompatible types, then it is retried in the following guise: (arg adaptTypeA: self) op: arg adaptToTypeA. This gives the arg of typeB an opportunity to resolve the incompatibility, knowing exactly what two types are involved. If self is more general, then arg will be converted, and viceVersa. This mechanism is extensible to any new number classes that one might wish to add to Squeak. The only requirement is that every subclass of Number must support a pair of conversion methods specific to each of the other subclasses of Number. Note: To write an hex number, in Cuis you can do 16rFE and not 16rfe. WRT some proposal to change this, Dan Ingalls said: at http://lists.squeakfoundation.org/pipermail/squeak-dev/2000-March/013368.html : I will have to admit to a bit of bigotry here. I was the originator of the numeric format, and I object to a couple of the ANSI decisions regarding the syntax of numeric constants. I have three reasons for this... 1. I much prefer 2r1e31 to 2r10000000000000000000000000000000 for the sign bit and 16rFe8 for the high nibble of a 32-bit integer and 10e6 for a million. Come on. (jmv note: it is actually 16rFe7 hex and 10e5) 2. I just feel that the current (Squeak) def, which is the original one, is a micro world of simplicity and generality. Radix, digits and exponent defined analogously over ints, largeInts, and Floats. That's all. 3. I have spent entire hours with inquisitive kids, just exploring this micro world. If they get bored with 2r1e10 then it's time to step out and explore the meaning of 3r11.2. If I were going to "fix" anything, I think it would be to allow 1r111111111111 = 12. This may be the issue that finally leads to a parting of the ways in the Squeak Community. Just Kidding - Dan Note: Cuis does support the Unary numeric system. So 1r111111111111 = 12 1r1 = 1 1r11 = 2 1r-11 = -2 1r111 = 3 1r = 0 See See http://en.wikipedia.org/wiki/Unary_numeral_system and #readBaseOneFrom:

### Methods
#### Number>>#withNegativeSign

Answer a number with same magnitude than receiver and negative sign.


<details>
	<summary>See more</summary>
	
	withNegativeSign
	"Answer a number with same magnitude than receiver and negative sign."
	^self abs negated
</details>

#### Number>>#withDecimalUnitPrefixAndValue: aBlock

As in https://en.wikipedia.org/wiki/Metric_prefix { 0.00000123456. 0.0000123456. 0.000123456. 0.00123456. 0.0123456. 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withDecimalUnitPrefixAndValue: [ :value :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]


<details>
	<summary>See more</summary>
	
	withDecimalUnitPrefixAndValue: aBlock
	"
	As in https://en.wikipedia.org/wiki/Metric_prefix
	{ 0.00000123456. 0.0000123456. 0.000123456. 0.00123456. 0.0123456. 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex _ self log floor // 3.
	prefixIndex _ prefixIndex min: 6 max: -6.
	factor _ 1000 raisedToInteger: prefixIndex.
	nameAndSymbol _ {
		{'atto'.		'a'}.
		{'femto'.	'f'}.
		{'pico'.		'p'}.
		{'nano'.		'n'}.
		{'micro'.	'µ'}.
		{'milli'.	'm'}.
		{''. 			''}.
		{'kilo'.		'k'}.
		{'mega'.		'M'}.
		{'giga'.		'G'}.
		{'tera'.		'T'}.
		{'peta'.		'P'}.
		{'exa'.		'E'}
	} at: prefixIndex+7.
	aBlock value: self asFloat / factor value: nameAndSymbol second value: nameAndSymbol first
</details>

#### Number>>#asInteger

Answer an Integer nearest the receiver toward zero.


<details>
	<summary>See more</summary>
	
	asInteger
	"Answer an Integer nearest the receiver toward zero."

	^self truncated
</details>

#### Number>>#adaptToPoint: rcvr andSend: selector

If I am involved in arithmetic with a Point, convert me to a Point.


<details>
	<summary>See more</summary>
	
	adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a Point, convert me to a Point."
	^ rcvr perform: selector with: self@self
</details>

#### Number>>#isNumber

Overridden to return true in Number, natch


<details>
	<summary>See more</summary>
	
	isNumber
	^ true
</details>

#### Number>>#truncated

Answer an integer nearest the receiver toward zero.


<details>
	<summary>See more</summary>
	
	truncated
	"Answer an integer nearest the receiver toward zero."

	^self quo: 1
</details>

#### Number>>#adaptToInteger: rcvr andSend: selector

If I am involved in arithmetic with a Integer, convert us and evaluate exprBlock.


<details>
	<summary>See more</summary>
	
	adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with a Integer, convert us and evaluate exprBlock."
	^ self subclassResponsibility
</details>

#### Number>>#cubed

Answer the receiver multipled by itself.


<details>
	<summary>See more</summary>
	
	cubed
	"Answer the receiver multipled by itself."

	^self * self * self
</details>

#### Number>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self printOn: aStream base: 10
</details>

#### Number>>#% another

Answer the relative distance between two number


<details>
	<summary>See more</summary>
	
	% another
	"Answer the relative distance between two number"
	^ (self - another) abs / (self abs + another abs / 2)
</details>

#### Number>>#to: stop

Answer an Interval from the receiver up to the argument, stop, incrementing by 1.


<details>
	<summary>See more</summary>
	
	to: stop
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by 1."

	^Interval from: self to: stop by: 1
</details>

#### Number>>#truncateTo: aNumber

Answer the next multiple of aNumber toward zero that is nearest the receiver.


<details>
	<summary>See more</summary>
	
	truncateTo: aNumber 
	"Answer the next multiple of aNumber toward zero that is nearest the 
	receiver."
	"Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category."
	"Examples:
		3.1479 truncateTo: 0.01 -> 3.14
		3.1479 truncateTo: 0.1 -> 3.1
		1923 truncateTo: 10 -> 1920
		3.1479 truncateTo: 0.005 -> 3.145
		-3.1479 truncateTo: 0.01 -> -3.14"

	^ (self quo: aNumber) * aNumber
</details>

#### Number>>#rem:  divisor

Modulo operation. Remainder of the integer division #quo: (division truncated to zero). Answer a Number with the same sign as dividend (i.e., self). (9 rem: 4) = 1. (-9 rem: 4) = -1. (0.9 rem: 0.4) = 0.1. See http://en.wikipedia.org/wiki/Modulo_operation See #\\, #rem: and #mod: See detailed discussion at http://mathforum.org/library/drmath/view/52343.html


<details>
	<summary>See more</summary>
	
	rem:  divisor
	"Modulo operation. Remainder of the integer division #quo: (division truncated to zero).
	Answer a Number with the same  sign as dividend (i.e.,  self).
	(9 rem: 4) = 1.
	(-9 rem: 4) = -1.
	(0.9 rem: 0.4) = 0.1.
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - ((self quo: divisor) * divisor)

	"Evaluate the following:"
"
| g d |
d _ 1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld
"
</details>

#### Number>>#sin

The receiver represents an angle measured in radians. Answer its sine.


<details>
	<summary>See more</summary>
	
	sin
	"The receiver represents an angle measured in radians. Answer its sine."

	^self asFloat sin
</details>

#### Number>>#raisedToFraction: aFraction

Raise an exception if argument is not a Fraction


<details>
	<summary>See more</summary>
	
	raisedToFraction: aFraction
	"Raise an exception if argument is not a Fraction"
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	self isZero ifTrue: [
		aFraction negative ifTrue: [^ (self raisedToFraction: aFraction negated) reciprocal].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated
</details>

#### Number>>#ifNotZero: aBlock

Useful for workarounding division by zero #(1.0 2.0 0.0) collect: [ :k | k ifNotZero: [100.0/k]]


<details>
	<summary>See more</summary>
	
	ifNotZero: aBlock
	"
	Useful for workarounding division by zero
	#(1.0 2.0 0.0) collect: [ :k | k ifNotZero: [100.0/k]]
	"
	^ self isZero ifFalse: aBlock ifTrue: [self]
</details>

#### Number>>#printOn: aStream base: base

This method should print a representation of the number for the given base, excluding the base prefix (and the letter r for radix)


<details>
	<summary>See more</summary>
	
	printOn: aStream base: base
	"This method should print a representation of the number for the given base,
	excluding the base prefix (and the letter r for radix)"
	
	^self subclassResponsibility
</details>

#### Number>>#log

Answer the base-10 log of the receiver. See comment at #log:


<details>
	<summary>See more</summary>
	
	log
	"Answer the base-10 log of the receiver.
	See comment at #log:"

	^self asFloat log
</details>

#### Number>>#cosh

Answer receivers hyperbolic cosine.


<details>
	<summary>See more</summary>
	
	cosh
	"Answer receivers hyperbolic cosine."
	
	^self asFloat cosh
</details>

#### Number>>#degreesToRadiansMinutes: minutes seconds: seconds

deg degreesToRadiansWithMinutes: min seconds: secs Take receiver and arguments as degrees, minutes and seconds. Answer radians


<details>
	<summary>See more</summary>
	
	degreesToRadiansMinutes: minutes seconds: seconds
	" deg degreesToRadiansWithMinutes: min seconds: secs
	Take receiver and arguments as degrees, minutes and seconds. Answer radians"

	^(seconds / 60.0 + minutes / 60.0 + self) degreesToRadians
</details>

#### Number>>#asPoint

Answer a Point with the receiver as both coordinates; often used to supply the same value in two dimensions, as with symmetrical gridding or scaling.


<details>
	<summary>See more</summary>
	
	asPoint
	"Answer a Point with the receiver as both coordinates; often used to 
	supply the same value in two dimensions, as with symmetrical gridding 
	or scaling."

	^self @ self
</details>

#### Number>>#integerPart

Added for ANSI compatibility


<details>
	<summary>See more</summary>
	
	integerPart
	"Added for ANSI compatibility"
	^self truncated
</details>

#### Number>>#toSelfPlus: aDelta

<details>
	<summary>See more</summary>
	
	toSelfPlus: aDelta

	^self to: self + aDelta
</details>

#### Number>>#- aNumber

Answer the difference between the receiver and aNumber.


<details>
	<summary>See more</summary>
	
	- aNumber 
	"Answer the difference between the receiver and aNumber."

	self subclassResponsibility
</details>

#### Number>>#arg

Answer the argument of the receiver (see Complex | arg). Note: #argument and #phase assume the convention of 0+0i having argument=0


<details>
	<summary>See more</summary>
	
	arg
	"Answer the argument of the receiver (see Complex | arg).
	Note: #argument and #phase assume the convention of 0+0i having argument=0"
	
	self isZero ifTrue: [self error: 'Zero (0 + 0 i) does not have an argument.'].
	^self < 0
		ifFalse: [0]
		ifTrue: [Float pi]
</details>

#### Number>>#floorLog: radix

Answer the floor of the log base radix of the receiver.


<details>
	<summary>See more</summary>
	
	floorLog: radix
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor
</details>

#### Number>>#reciprocal

Returns the reciprocal of self. In case self is 0 the / signals ZeroDivide


<details>
	<summary>See more</summary>
	
	reciprocal
	"Returns the reciprocal of self.
	In case self is 0 the / signals ZeroDivide"
	
	^1 / self
</details>

#### Number>>#tan

The receiver represents an angle measured in radians. Answer its tangent.


<details>
	<summary>See more</summary>
	
	tan
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self asFloat tan
</details>

#### Number>>#printOn: aStream integerDigits: placesLeftOfFractionPoint fractionDigits: placesRightOfFractionPoint

placesLeftOfFractionPoint is the minimum to be used (use more if required) placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate.


<details>
	<summary>See more</summary>
	
	printOn: aStream integerDigits: placesLeftOfFractionPoint fractionDigits: placesRightOfFractionPoint
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 123456.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float pi printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 fractionDigits: 5 ]
	"

	^self printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: nil fractionDigits: placesRightOfFractionPoint positiveIndicator: nil
</details>

#### Number>>#milliSeconds

<details>
	<summary>See more</summary>
	
	milliSeconds

	^ Duration milliSeconds: self

</details>

#### Number>>#real

Compatibility with Complex numbers


<details>
	<summary>See more</summary>
	
	real
	"Compatibility with Complex numbers"
	^ self
</details>

#### Number>>#to: stop count: n

Answer an Interval from the receiver up to the argument, stop, with n elements.


<details>
	<summary>See more</summary>
	
	to: stop count: n
	"Answer an Interval from the receiver up to the argument, stop, 
	with n elements."

	^Interval from: self to: stop count: n
</details>

#### Number>>#asIntegerOrFloat

Convert to integer if integer, or to Float otherwhise.


<details>
	<summary>See more</summary>
	
	asIntegerOrFloat
	"Convert to integer if integer, or to Float otherwhise."

	^self asFloat
</details>

#### Number>>#nanoSeconds

<details>
	<summary>See more</summary>
	
	nanoSeconds

	^ Duration nanoSeconds: self.
</details>

#### Number>>#detentBy: detent atMultiplesOf: grid snap: snap

Map all values that are within detent/2 of any multiple of grid to that multiple. Otherwise, if snap is true, return self, meaning that the values in the dead zone will never be returned. If snap is false, then expand the range between dead zones so that it covers the range between multiples of the grid, and scale the value by that factor.


<details>
	<summary>See more</summary>
	
	detentBy: detent atMultiplesOf: grid snap: snap
	"Map all values that are within detent/2 of any multiple of grid to that multiple.  Otherwise, if snap is true, return self, meaning that the values in the dead zone will never be returned.  If snap is false, then expand the range between dead zones so that it covers the range between multiples of the grid, and scale the value by that factor."
	| r1 r2 |
	r1 _ self roundTo: grid.  "Nearest multiple of grid"
	(self roundTo: detent) = r1 ifTrue: [^ r1].  "Snap to that multiple..."
	snap ifTrue: [^ self].  "...or return self"

	r2 _ self < r1  "Nearest end of dead zone"
		ifTrue: [r1 - (detent asFloat/2)]
		ifFalse: [r1 + (detent asFloat/2)].
	"Scale values between dead zones to fill range between multiples"
	^ r1 + ((self - r2) * grid asFloat / (grid - detent))
"
	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: true] 	(170 to: 190 by: 2) collect: [:a | a detentBy: 10 atMultiplesOf: 90 snap: false]
	(3.9 to: 4.1 by: 0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: true] 	(-3.9 to: -4.1 by: -0.02) collect: [:a | a detentBy: 0.1 atMultiplesOf: 1.0 snap: false]
"
</details>

#### Number>>#adaptToFraction: rcvr andSend: selector

If I am involved in arithmetic with a Fraction, convert us and evaluate exprBlock.


<details>
	<summary>See more</summary>
	
	adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert us and evaluate exprBlock."
	^ self subclassResponsibility
</details>

#### Number>>#phase

Compatibility with Complex numbers. Answer the argument of the receiver. Answer is in (-Pi .. +Pi] Note: Assume the convention of 0+0i having argument=0


<details>
	<summary>See more</summary>
	
	phase
	"Compatibility with Complex numbers.
	Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^ self argument
</details>

#### Number>>#to: stop by: step do: aBlock

Normally compiled in-line, and therefore not overridable. Evaluate aBlock for each element of the interval (self to: stop by: step). Warning: Might fail with Float numbers. For instance: 0 to: 2.4 by: 0.1 do: [ :each | each print ] is wrong, while (0 to: 2.4 by: 0.1) do: [ :each | each print ] is right. Prefer explicit Interval creation if using Floats!


<details>
	<summary>See more</summary>
	
	to: stop by: step do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: step).

	Warning: Might fail with Float numbers. For instance:
		0 to: 2.4 by: 0.1 do: [ :each | each print ]
	is wrong, while
		(0 to: 2.4 by: 0.1) do: [ :each | each print ]
	is right. Prefer explicit Interval creation if using Floats!
	"
	| nextValue |
	nextValue _ self.
	step = 0 ifTrue: [self error: 'step must be non-zero'].
	step < 0
		ifTrue: [
			[stop <= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue _ nextValue + step]]
		ifFalse: [
			[stop >= nextValue]
				whileTrue: [
					aBlock value: nextValue.
					nextValue _ nextValue + step]]
</details>

#### Number>>#= aNumber

Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes: (3 = 3.0) ifTrue: [3 hash = 3.0 hash] Therefore, consistency between #= and #hash for numeric classes is validated by specific tests


<details>
	<summary>See more</summary>
	
	= aNumber
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility
</details>

#### Number>>#seconds

<details>
	<summary>See more</summary>
	
	seconds

	^ Duration seconds: self
</details>

#### Number>>#magnitude

Compatibility with Complex numbers


<details>
	<summary>See more</summary>
	
	magnitude
	"Compatibility with Complex numbers"
	^self abs
</details>

#### Number>>#isFinite

<details>
	<summary>See more</summary>
	
	isFinite
	^ true
</details>

#### Number>>#days

<details>
	<summary>See more</summary>
	
	days

	^ Duration days: self
</details>

#### Number>>#raisedTo: exponent

Answer the receiver raised to aNumber.


<details>
	<summary>See more</summary>
	
	raisedTo: exponent
	"Answer the receiver raised to aNumber."

	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: exponent].
	self < 0 ifTrue: [
		^NegativePowerError new signalReceiver: self selector: #raisedTo: argument: exponent].
	0.0 = exponent ifTrue: [^ self class one]. 	"Special case of exponent=0"
	1.0 = exponent ifTrue: [^ self].					"Special case of exponent=1"
	0 = self ifTrue: [									"Special case of self = 0"
		exponent isNaN ifTrue: [ ^exponent ].
		^exponent < 0
			ifTrue: [(self raisedTo: exponent negated) reciprocal]
			ifFalse: [self]].
	^ (exponent * self ln) exp							"Otherwise use logarithms"
</details>

#### Number>>#roundDownTo: aNumber

Answer the next multiple of aNumber toward negative infinity that is nearest the receiver.


<details>
	<summary>See more</summary>
	
	roundDownTo: aNumber 
	"Answer the next multiple of aNumber toward negative infinity that is nearest the 
	receiver."
 	"Examples:
		3.1479 roundDownTo: 0.01 -> 3.14
		3.1479 roundDownTo: 0.1 -> 3.1
		1923 roundDownTo: 10 -> 1920
		3.1479 roundDownTo: 0.005 -> 3.145
		-3.1479 roundDownTo: 0.01 -> -3.15"
		
	^(self/aNumber) floor * aNumber
</details>

#### Number>>#ceiling

Answer the integer nearest the receiver toward infinity.


<details>
	<summary>See more</summary>
	
	ceiling
	"Answer the integer nearest the receiver toward  infinity."

	| truncation |
	truncation := self truncated.
	self <= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation + 1]
</details>

#### Number>>#/ aNumber

Answer the result of dividing the receiver by aNumber.


<details>
	<summary>See more</summary>
	
	/ aNumber 
	"Answer the result of dividing the receiver by aNumber."

	self subclassResponsibility
</details>

#### Number>>#tanh

Answer receivers hyperbolic tangent


<details>
	<summary>See more</summary>
	
	tanh
	"Answer receivers hyperbolic tangent"
	
	^self asFloat tanh
</details>

#### Number>>#mod: divisor

Modulo operation. Remainder of the integer division #div: (Euclidean division) Answer a Number that is never negative (it is positive or zero). (9 mod: 4) = 1 (-9 mod: 4) = 3 (9 mod: -4) = 1 (0.9 mod: 0.4) = 0.1 See http://en.wikipedia.org/wiki/Modulo_operation See #\\, #rem: and #mod: See detailed discussion at http://mathforum.org/library/drmath/view/52343.html And https://biblio.ugent.be/input/download?func=downloadFile&recordOId=314490&fileOId=452146


<details>
	<summary>See more</summary>
	
	mod: divisor
	"Modulo operation. Remainder of the integer division #div: (Euclidean division)
	Answer a Number that is never negative (it is positive or zero). 
	(9 mod: 4) = 1
	(-9 mod: 4) = 3
	(9 mod: -4) = 1
	(0.9 mod: 0.4) = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	And https://biblio.ugent.be/input/download?func=downloadFile&recordOId=314490&fileOId=452146
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

	"Evaluate the following:"
"
| g d |
d _ 1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"

</details>

#### Number>>#to: stop by: step

Answer an Interval from the receiver up to the argument, stop, incrementing by step.


<details>
	<summary>See more</summary>
	
	to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop, 
	incrementing by step."

	^Interval from: self to: stop by: step
</details>

#### Number>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	self storeOn: aStream base: 10
</details>

#### Number>>#degreesToRadians

The receiver is assumed to represent degrees. Answer the conversion to radians.


<details>
	<summary>See more</summary>
	
	degreesToRadians
	"The receiver is assumed to represent degrees. Answer the conversion to 
	radians."

	^self asFloat degreesToRadians
</details>

#### Number>>#roundUpTo: aNumber

Answer the next multiple of aNumber toward infinity that is nearest the receiver.


<details>
	<summary>See more</summary>
	
	roundUpTo: aNumber 
	"Answer the next multiple of aNumber toward infinity that is nearest the 
	receiver."
 	"Examples:
		3.1479 roundUpTo: 0.01 -> 3.15
		3.1479 roundUpTo: 0.1 -> 3.2
		1923 roundUpTo: 10 -> 1930
		3.1479 roundUpTo: 0.005 -> 3.15
		-3.1479 roundUpTo: 0.01 -> -3.14"

	^(self/aNumber) ceiling * aNumber
</details>

#### Number>>#arSinh

Answer receiver's area hyperbolic sine. That is the inverse function of sinh.


<details>
	<summary>See more</summary>
	
	arSinh
	"Answer receiver's area hyperbolic sine.
	That is the inverse function of sinh."

	^self asFloat arSinh
</details>

#### Number>>#withBinaryUnitPrefixAndValue: aBlock

As in https://en.wikipedia.org/wiki/Binary_prefix { 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withBinaryUnitPrefixAndValue: [ :value :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]


<details>
	<summary>See more</summary>
	
	withBinaryUnitPrefixAndValue: aBlock
	"
	As in https://en.wikipedia.org/wiki/Binary_prefix
	{ 0.123456. 1.23456. 12.3456. 123.456. 1234.56. 12345.6. 123456. 1234560. 12345600 } do: [ :n | n withBinaryUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName | {value printString, ' ', unitPrefixSymbol. unitPrefixName} print]]
	"
	| prefixIndex factor nameAndSymbol |
	prefixIndex _ self log floor // 3.
	prefixIndex _ prefixIndex min: 8 max: 0.
	factor _ 1024 raisedToInteger: prefixIndex.
	nameAndSymbol _ {
		{''. 		''}.
		{'kibi'.	'Ki'}.
		{'mebi'.	'Mi'}.
		{'gibi'.	'Gi'}.
		{'tebi'.	'Ti'}.
		{'pebi'.	'Pi'}.
		{'exbi'.	'Ei'}.
		{'zebi'.	'Zi'}.
		{'yobi'.	'Yi'}
	} at: prefixIndex+1.
	aBlock value: (self / factor) asIntegerOrFloat value: nameAndSymbol second value: nameAndSymbol first
</details>

#### Number>>#imaginary

Compatibility with Complex numbers


<details>
	<summary>See more</summary>
	
	imaginary
	"Compatibility with Complex numbers"
	^ 0
</details>

#### Number>>#exp

Answer the exponential of the receiver as a floating point number.


<details>
	<summary>See more</summary>
	
	exp
	"Answer the exponential of the receiver as a floating point number."

	^self asFloat exp
</details>

#### Number>>#fractionPart

Added for ANSI compatibility


<details>
	<summary>See more</summary>
	
	fractionPart
	
	"Added for ANSI compatibility"
	
	^self - self integerPart
</details>

#### Number>>#@ y

Primitive. Answer a Point whose x value is the receiver and whose y value is the argument. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	@ y 
	"Primitive. Answer a Point whose x value is the receiver and whose y 
	value is the argument. Optional. No Lookup. See Object documentation 
	whatIsAPrimitive."

	<primitive: 18>
	^Point x: self y: y
</details>

#### Number>>#argument

Compatibility with Complex numbers. Answer the argument of the receiver (see Complex | argument). Answer is in (-Pi .. +Pi] Note: Assume the convention of 0+0i having argument=0


<details>
	<summary>See more</summary>
	
	argument
	"Compatibility with Complex numbers.
	Answer the argument of the receiver (see Complex | argument).
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^self < 0
		ifFalse: [0]
		ifTrue: [Float pi]
</details>

#### Number>>#negative

Answer whether the receiver is mathematically negative.


<details>
	<summary>See more</summary>
	
	negative
	"Answer whether the receiver is mathematically negative."

	^ self < 0
</details>

#### Number>>#minutes

<details>
	<summary>See more</summary>
	
	minutes

	^ Duration minutes: self
</details>

#### Number>>#copySignTo: aNumber

Return a number with same magnitude as aNumber and same sign as self.


<details>
	<summary>See more</summary>
	
	copySignTo: aNumber
	"Return a number with same magnitude as aNumber and same sign as self."

	^ self positive
		ifTrue: [aNumber abs]
		ifFalse: [aNumber withNegativeSign].
</details>

#### Number>>#adaptToCollection: rcvr andSend: selector

If I am involved in arithmetic with a Collection, return a Collection of the results of each element combined with me in that expression.


<details>
	<summary>See more</summary>
	
	adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
</details>

#### Number>>#floor

Answer the integer nearest the receiver toward negative infinity.


<details>
	<summary>See more</summary>
	
	floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation _ self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]
</details>

#### Number>>#sign

Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.


<details>
	<summary>See more</summary>
	
	sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0
</details>

#### Number>>#nthRoot: aPositiveInteger

Answer the nth root of the receiver.


<details>
	<summary>See more</summary>
	
	nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."

	self subclassResponsibility
</details>

#### Number>>#isInfinite

<details>
	<summary>See more</summary>
	
	isInfinite

	^ false
</details>

#### Number>>#isDivisibleBy: aNumber

<details>
	<summary>See more</summary>
	
	isDivisibleBy: aNumber
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0
</details>

#### Number>>#arCosh

Answer receiver's area hyperbolic cosine. That is the inverse function of cosh.


<details>
	<summary>See more</summary>
	
	arCosh
	"Answer receiver's area hyperbolic cosine.
	That is the inverse function of cosh."

	^self asFloat arCosh
</details>

#### Number>>#asSmallAngleDegrees

Return the receiver normalized to lie within the range (-180, 180)


<details>
	<summary>See more</summary>
	
	asSmallAngleDegrees
	"Return the receiver normalized to lie within the range (-180, 180)"

	| pos |
	pos _ self \\ 360.
	pos > 180 ifTrue: [pos _ pos - 360].
	^ pos

"#(-500 -300 -150 -5 0 5 150 300 500 1200) collect: [:n | n asSmallAngleDegrees]"
</details>

#### Number>>#arcTan: denominator

The receiver is the tangent of an angle. Answer the angle measured in radians.


<details>
	<summary>See more</summary>
	
	arcTan: denominator
	"The receiver is the tangent of an angle. Answer the angle measured in 
	radians."

	^(self asFloat) arcTan: denominator.
</details>

#### Number>>#radiansToDegrees

The receiver is assumed to represent radians. Answer the conversion to degrees.


<details>
	<summary>See more</summary>
	
	radiansToDegrees
	"The receiver is assumed to represent radians. Answer the conversion to 
	degrees."

	^self asFloat radiansToDegrees
</details>

#### Number>>#log: aNumber

Answer the log base aNumber of the receiver. Note 1: Answer an Integer number if receiver and base are both integer (receiver might also be a fraction of the form 1/Integer), and the correct answer is integer too. Note2: There are several implementors of #log:, #log, #log2 and #ln in the Number hierarchy. Behavior is not consistent when receiver is a negative value. Some implementors answer Float nan, others raise a DomainError. Another possibility would be to answer the correct Complex number. It would be good to pick one and make it consistently followed.


<details>
	<summary>See more</summary>
	
	log: aNumber
	"Answer the log base aNumber of the receiver.
	Note 1:
		Answer an Integer number if receiver and base are both integer (receiver might also be a fraction of the form 1/Integer), and the correct answer is integer too.
	Note2:
		There are several implementors of #log:, #log, #log2 and #ln in the Number hierarchy. Behavior is not consistent when receiver is a negative value. Some implementors answer Float nan, others raise a DomainError. Another possibility would be to answer the correct Complex number.
		It would be good to pick one and make it consistently followed.
	"

	^self asFloat log: aNumber
</details>

#### Number>>#asSmallPositiveDegrees

Return the receiver normalized to lie within the range (0, 360)


<details>
	<summary>See more</summary>
	
	asSmallPositiveDegrees
	"Return the receiver normalized to lie within the range (0, 360)"

	^self \\ 360
</details>

#### Number>>#squared

Answer the receiver multipled by itself.


<details>
	<summary>See more</summary>
	
	squared
	"Answer the receiver multipled by itself."

	^self * self
</details>

#### Number>>#adaptToFloat: rcvr andSend: selector

If I am involved in arithmetic with a Float, convert me to a Float.


<details>
	<summary>See more</summary>
	
	adaptToFloat: rcvr andSend: selector 
	"If I am involved in arithmetic with a Float, convert me to a Float."
	^ rcvr perform: selector with: self asFloat
</details>

#### Number>>#odd

Answer whether the receiver is an odd number.


<details>
	<summary>See more</summary>
	
	odd
	"Answer whether the receiver is an odd number."

	^self even == false
</details>

#### Number>>#degreeCos

Answer the cosine of the receiver taken as an angle in degrees.


<details>
	<summary>See more</summary>
	
	degreeCos
	"Answer the cosine of the receiver taken as an angle in degrees."
	
	^ (90 - (180 + self \\ 360 - 180) abs) degreesToRadians sin
</details>

#### Number>>#strictlyPositive

Answer whether the receiver is mathematically positive.


<details>
	<summary>See more</summary>
	
	strictlyPositive
	"Answer whether the receiver is mathematically positive."

	^ self > 0
</details>

#### Number>>#printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: leftPaddingCharOrNil fractionDigits: placesRightOfFractionPoint positiveIndicator: aCharacterOrNil

placesLeftOfFractionPoint is the minimum to be used (use more if required) placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate.


<details>
	<summary>See more</summary>
	
	printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: leftPaddingCharOrNil fractionDigits: placesRightOfFractionPoint positiveIndicator: aCharacterOrNil
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: $  fractionDigits: 5 positiveIndicator: Character space ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	"

	| withoutSignAndPad zeroPad |
	withoutSignAndPad _ String streamContents: [ :strm |
		self abs printOn: strm fractionDigits: placesRightOfFractionPoint ].
	self < 0
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ aCharacterOrNil ifNotNil: [ aStream nextPut: aCharacterOrNil ]].
	zeroPad _ placesLeftOfFractionPoint - ((withoutSignAndPad indexOf: $. startingAt: 1 ifAbsent: [withoutSignAndPad size + 1]) - 1).
	zeroPad > 0 ifTrue: [
		zeroPad timesRepeat: [ aStream nextPut: (leftPaddingCharOrNil ifNil: [$0]) ]].
	aStream nextPutAll: withoutSignAndPad
</details>

#### Number>>#arcTan

The receiver is the tangent of an angle. Answer the angle measured in radians. Note: If the purpose is to recover the angle of some vector, prefer #arcTan: See, for example, Complex>>#argument


<details>
	<summary>See more</summary>
	
	arcTan
	"The receiver is the tangent of an angle. Answer the angle measured in  radians.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	^self asFloat arcTan
</details>

#### Number>>#rounded

Answer the integer nearest the receiver.


<details>
	<summary>See more</summary>
	
	rounded
	"Answer the integer nearest the receiver."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"

	| truncated fractionPartAbs |
	truncated _ self truncated.
	fractionPartAbs _ (self-truncated) abs.
	fractionPartAbs = (1/2)
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < (1/2)
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]
</details>

#### Number>>#is: aSymbol

Note: Senders might prefer #isNumber for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Note: Senders might prefer #isNumber for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Number = aSymbol or: [ super is: aSymbol]
</details>

#### Number>>#hours

<details>
	<summary>See more</summary>
	
	hours

	^ Duration hours: self
</details>

#### Number>>#interpolateTo: aNumber at: param

<details>
	<summary>See more</summary>
	
	interpolateTo: aNumber at: param
	^self + (aNumber - self * param)
</details>

#### Number>>#printStringBase: base

<details>
	<summary>See more</summary>
	
	printStringBase: base
	^ String streamContents:
		[:strm | self printOn: strm base: base]
</details>

#### Number>>#arcCos

The receiver is the cosine of an angle. Answer the angle measured in radians.


<details>
	<summary>See more</summary>
	
	arcCos 
	"The receiver is the cosine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcCos
</details>

#### Number>>#isOrAreStringWith: aNoun

<details>
	<summary>See more</summary>
	
	isOrAreStringWith: aNoun
	| result |
	result _ self = 1
		ifTrue:
			[' is one ']
		ifFalse:
			[self = 0
				ifTrue:
					[' are no ']
				ifFalse:
					[' are ', self printString, ' ']].
	result _ result, aNoun.
	self = 1 ifFalse: [result _ result, 's'].
	^ result

"
#(0 1 2 98.6) do:
	[:num | Transcript newLine; show: 'There', (num isOrAreStringWith: 'way'), ' to skin a cat'].
"
</details>

#### Number>>#sqrt

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrt
	"Answer the square root of the receiver."

	self subclassResponsibility
</details>

#### Number>>#lg

Answer the base 2 log of the receiver. Alternative notation. See https://en.wikipedia.org/wiki/Binary_logarithm


<details>
	<summary>See more</summary>
	
	lg
	"Answer the base 2 log of the receiver.
	Alternative notation. See https://en.wikipedia.org/wiki/Binary_logarithm"

	^self log2
</details>

#### Number>>#to: stop do: elementBlock separatedBy: separatorBlock

String streamContents: [ :strm | 1 to: 10 do: [ :i | i printOn: strm ] separatedBy: [ strm nextPutAll: ' -- ' ]]


<details>
	<summary>See more</summary>
	
	to: stop do: elementBlock separatedBy: separatorBlock
	"
	String streamContents: [ :strm |
		1 to: 10 do: [ :i | i printOn: strm ] separatedBy: [ strm nextPutAll: ' -- ' ]]
	"
	| beforeFirst | 
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	beforeFirst _ true.
	self to: stop do: [ :element |
		beforeFirst
			ifTrue: [beforeFirst _ false]
			ifFalse: [separatorBlock value].
		elementBlock value: element]
</details>

#### Number>>#\\ divisor

Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division) Answer a Number with the same sign as divisor. 9\\4 = 1 -9\\4 = 3 9\\-4 = -3 0.9\\0.4 = 0.1 See http://en.wikipedia.org/wiki/Modulo_operation See #\\, #rem: and #mod: See detailed discussion at http://mathforum.org/library/drmath/view/52343.html


<details>
	<summary>See more</summary>
	
	\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\-4 = -3
	0.9\\0.4 = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - (self // divisor * divisor)

	"Evaluate the following:"
"
| g d |
d _ 1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"
</details>

#### Number>>#* aNumber

Answer the result of multiplying the receiver by aNumber.


<details>
	<summary>See more</summary>
	
	* aNumber 
	"Answer the result of multiplying the receiver by aNumber."

	self subclassResponsibility
</details>

#### Number>>#quo: aNumber

Integer division with truncation toward zero. (-9 quo: 4) = -2 (-0.9 quo: 0.4) = -2 #rem: answers the remainder from this division. See #//, #quo:, #div:


<details>
	<summary>See more</summary>
	
	quo: aNumber
	"Integer division with truncation toward zero.
	(-9 quo: 4) = -2
	(-0.9 quo: 0.4) = -2
	#rem: answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) truncated
</details>

#### Number>>#asNumber

<details>
	<summary>See more</summary>
	
	asNumber
	^ self
</details>

#### Number>>#printOn: aStream fractionDigits: placesDesired

Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator.


<details>
	<summary>See more</summary>
	
	printOn: aStream fractionDigits: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator."

	| rounder rounded roundedFractionPart |
	placesDesired > 0 ifFalse: [ ^ self rounded printOn: aStream ].
	rounder _ 10 raisedToInteger: placesDesired.
	rounded _ self roundTo: rounder reciprocal.
	rounded negative ifTrue: [ aStream nextPut: $- ].
	rounded _ rounded abs.
	rounded integerPart truncated printOn: aStream.
	aStream nextPut: $..
	roundedFractionPart _ (rounded fractionPart * rounder) truncated.
	roundedFractionPart
		printOn: aStream
		base: 10
		length: placesDesired
		padded: true
</details>

#### Number>>#storeStringBase: base

<details>
	<summary>See more</summary>
	
	storeStringBase: base
	^ String streamContents: [:strm | self storeOn: strm base: base]
</details>

#### Number>>#cos

The receiver represents an angle measured in radians. Answer its cosine.


<details>
	<summary>See more</summary>
	
	cos
	"The receiver represents an angle measured in radians. Answer its cosine."

	^self asFloat cos
</details>

#### Number>>#sinh

Answer receivers hyperbolic sine


<details>
	<summary>See more</summary>
	
	sinh
	"Answer receivers hyperbolic sine"
	
	^self asFloat sinh
</details>

#### Number>>#to: stop do: aBlock

Normally compiled in-line, and therefore not overridable. Evaluate aBlock for each element of the interval (self to: stop by: 1).


<details>
	<summary>See more</summary>
	
	to: stop do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue _ self.
	[nextValue <= stop]
		whileTrue: 
			[aBlock value: nextValue.
			nextValue _ nextValue + 1]
</details>

#### Number>>#// aNumber

Integer division with truncation toward negative infinity. 9//4 = 2 -9//4 = -3 -0.9//0.4 = -3 #\\ answers the remainder from this division. See #//, #quo:, #div:


<details>
	<summary>See more</summary>
	
	// aNumber
	"Integer division with truncation toward negative infinity. 
	9//4 = 2
	-9//4 = -3
	-0.9//0.4 = -3
	#\\ answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) floor
</details>

#### Number>>#abs

Answer a Number that is the absolute value (positive magnitude) of the receiver.


<details>
	<summary>See more</summary>
	
	abs
	"Answer a Number that is the absolute value (positive magnitude) of the 
	receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]
</details>

#### Number>>#div: aNumber

Integer division with non-negative remainder. Euclidean division. (9 div:4) = 2 (-9 div: 4) = -3 (-0.9 div: 0.4) = -3 #mod: answers the remainder from this division. See comments and examples there. See #//, #quo:, #div:


<details>
	<summary>See more</summary>
	
	div: aNumber
	"Integer division with non-negative remainder. Euclidean division.
	(9 div:4) = 2
	(-9 div: 4) = -3
	(-0.9 div: 0.4) = -3
	#mod: answers the remainder from this division. See comments and examples there.
	See #//, #quo:, #div:"
	"Answer an integer q such that: 
		for some r, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	aNumber positive ifTrue: [ ^self // aNumber ].
	^ (self // aNumber abs) negated
</details>

#### Number>>#raisedToNegativeInteger: negativeExponent

<details>
	<summary>See more</summary>
	
	raisedToNegativeInteger: negativeExponent
	
	| firstTry positiveExponent exponent1 exponent2 |

	positiveExponent := negativeExponent negated.
	firstTry := self raisedToInteger: positiveExponent.
	^firstTry isInfinite
		ifFalse: [firstTry reciprocal]
		ifTrue: [
			exponent1 _ positiveExponent // 2.
			exponent2 _ positiveExponent - exponent1.
			(self raisedToInteger: exponent1) reciprocal * (self raisedToInteger: exponent2) reciprocal ]
</details>

#### Number>>#even

Answer whether the receiver is an even number.


<details>
	<summary>See more</summary>
	
	even
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0
</details>

#### Number>>#hash

Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes: (3 = 3.0) ifTrue: [3 hash = 3.0 hash] Therefore, consistency between #= and #hash for numeric classes is validated by specific tests


<details>
	<summary>See more</summary>
	
	hash
	"Note: Consistency between #= and #hash for numeric classes is not done in the usual way (redefining them together), because we also need #= and #hash consistency across numeric classes:
	(3 = 3.0) ifTrue: [3 hash = 3.0 hash]
	Therefore, consistency between #= and #hash for numeric classes is validated by specific tests"

	^self subclassResponsibility
</details>

#### Number>>#inMinusPiToPiRange

Avoid conversion to Float if already ok


<details>
	<summary>See more</summary>
	
	inMinusPiToPiRange
	"Avoid conversion to Float if already ok"
	(self > Float pi negated and: [self <= Float pi]) ifTrue: [
		^ self ].
	^ self asFloat inMinusPiToPiRange
</details>

#### Number>>#^ another

Covenient, usual idiom. 2 ^ 8


<details>
	<summary>See more</summary>
	
	^ another
	"Covenient, usual idiom.
	2 ^ 8
	"
	^ self raisedTo: another
</details>

#### Number>>#negated

Answer a Number that is the negation of the receiver.


<details>
	<summary>See more</summary>
	
	negated
	"Answer a Number that is the negation of the receiver."

	^0 - self
</details>

#### Number>>#weeks

<details>
	<summary>See more</summary>
	
	weeks

	^ Duration weeks: self
</details>

#### Number>>#ln

Answer the natural log of the receiver. See comment at #log:


<details>
	<summary>See more</summary>
	
	ln
	"Answer the natural log of the receiver.
	See comment at #log:"

	^self asFloat ln
</details>

#### Number>>#log2

Answer the base-2 logarithm of the receiver. See comment at #log:


<details>
	<summary>See more</summary>
	
	log2
	"Answer the base-2 logarithm of the receiver.
	See comment at #log:"

	^self asFloat log2
</details>

#### Number>>#arcSin

The receiver is the sine of an angle. Answer the angle measured in radians.


<details>
	<summary>See more</summary>
	
	arcSin
	"The receiver is the sine of an angle. Answer the angle measured in 
	radians."

	^self asFloat arcSin
</details>

#### Number>>#degreeSin

Answer the sine of the receiver taken as an angle in degrees.


<details>
	<summary>See more</summary>
	
	degreeSin
	"Answer the sine of the receiver taken as an angle in degrees."
	
	^(90 - self) degreeCos
</details>

#### Number>>#storeOn: aStream base: base

This method should print a representation of the number for the given base, including the base prefix (with letter r for radix)


<details>
	<summary>See more</summary>
	
	storeOn: aStream base: base
	"This method should print a representation of the number for the given base,
	including the base prefix (with letter r for radix)"
	
	^self subclassResponsibility
</details>

#### Number>>#roundTo: quantum

Answer the nearest number that is a multiple of quantum.


<details>
	<summary>See more</summary>
	
	roundTo: quantum 
	"Answer the nearest number that is a multiple of quantum."
	"Please use this method when you actually want a numeric result.
	If what you need is a string representation with certain precision,
	consider using #printOn:fractionDigits: or some other method in the 'printing' category."

	^ (self / quantum) rounded * quantum
</details>

#### Number>>#arTanh

Answer receiver's area hyperbolic tangent. That is the inverse function of tanh.


<details>
	<summary>See more</summary>
	
	arTanh
	"Answer receiver's area hyperbolic tangent.
	That is the inverse function of tanh."

	^self asFloat arTanh
</details>

#### Number>>#+ aNumber

Answer the sum of the receiver and aNumber.


<details>
	<summary>See more</summary>
	
	+ aNumber 
	"Answer the sum of the receiver and aNumber."

	self subclassResponsibility
</details>

#### Number>>#isNaN

<details>
	<summary>See more</summary>
	
	isNaN
	^ false
</details>

#### Number>>#printStringFractionDigits: placesDesired

Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator. Float pi printStringFractionDigits: 4


<details>
	<summary>See more</summary>
	
	printStringFractionDigits: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator.
	Float pi printStringFractionDigits: 4
	"
	^ String streamContents: [ :strm | self printOn: strm fractionDigits: placesDesired ]
</details>

#### Number>>#isZero

<details>
	<summary>See more</summary>
	
	isZero
	^self = 0
</details>

#### Number>>#sign: aNumber

Return a Number with the same sign as aNumber and same magnitude as self.


<details>
	<summary>See more</summary>
	
	sign: aNumber
	"Return a Number with the same sign as aNumber and same magnitude as self."

	^ aNumber copySignTo: self
</details>

#### Number>>#positive

Answer whether the receiver is positive or equal to 0. (ST-80 protocol). See also strictlyPositive


<details>
	<summary>See more</summary>
	
	positive
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ self >= 0
</details>

#### Number>>#printString

Answer a String whose characters are a description of the receiver. If you want to print without a character limit, use fullPrintString. This description is to be meaningful for a Smalltalk programmer and usually includes a hint on the class of the object. Usually you should not reimplement this method in subclasses, but #printOn: See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	printString

	^String streamContents: [:str | self printOn: str base: 10]
</details>

#### Number>>#raisedToInteger: exponent

The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must be handled as an indeterminate form. Maybe further discussion is required on this topic.


<details>
	<summary>See more</summary>
	
	raisedToInteger: exponent
	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	Maybe further discussion is required on this topic."
	
	| bitProbe result |

	exponent negative ifTrue: [^self raisedToNegativeInteger: exponent ].
	exponent = 0 ifTrue: [^ self class one].
	exponent = 1 ifTrue: [^ self].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 	[
		(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
		bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
			whileTrue: [
				result := result * result].
	^result
</details>

## ParkMiller88Random

This Random Number Generator graciously contributed by David N. Smith. It is an adaptation of the Park-Miller RNG which uses Floats to avoid the need for LargeInteger arithmetic. The algorithm is described in detail in Random Number Generators: Good Ones Are Hard To Find Stephen K. Park, Keith W. Miller (Communications of the ACM, 31(10):1192--1201, 1988) http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf See also https://en.wikipedia.org/wiki/Lehmer_random_number_generator.

### Methods
#### ParkMiller88Random>>#initialize

Output stabilization is the user's responsibility


<details>
	<summary>See more</summary>
	
	initialize
	"Output stabilization is the user's responsibility"

	[
		seed _ (Time localMillisecondClock + self identityHash) hashMultiply \\ self m.
		seed = 0  "zero seeds are unacceptable"
	] whileTrue.
	seed _ seed asFloat
</details>

#### ParkMiller88Random>>#seed: anInteger

<details>
	<summary>See more</summary>
	
	seed: anInteger

	seed _ anInteger - 1 \\  (self m - 1) truncated + 1.
	
</details>

#### ParkMiller88Random>>#privateNextChunk

This method generates random instances of Integer in the interval 1 to 16r7FFFFFFF (almost 31 bits). Note the calculations cannot result in seed = 0 because a is a primitive generator of the integers modulo m.


<details>
	<summary>See more</summary>
	
	privateNextChunk
	"This method generates random instances of Integer in the interval
	1 to 16r7FFFFFFF (almost 31 bits).  Note the calculations cannot
	result in seed = 0 because a is a primitive generator of the integers
	modulo m."

	seed _ seed asFloat * self a \\ self m :: truncated.
	^seed
</details>

#### ParkMiller88Random>>#next

Answer a random Float in the interval [0, 1). NOTE: this is not a uniformly distributed random Float, since only uses 31 bits out of 53 bits mantissa. Additionally, #privateNextChunk can not produce all zeros or all ones. A higher quality Float RNG is LaggedFibonacciRandom


<details>
	<summary>See more</summary>
	
	next
	"Answer a random Float in the interval [0, 1).
	NOTE: this is not a uniformly distributed random Float, since only uses 31 bits out of 53 bits mantissa.
	Additionally, #privateNextChunk can not produce all zeros or all ones.

	A higher quality Float RNG is LaggedFibonacciRandom"

	^ self privateNextChunk asFloat / self m
	
	"In any case, an alternative, slower but better answer could be:
	
	^(self nextBits: 53) asFloat  timesTwoPower: -53"
</details>

#### ParkMiller88Random>>#nextChunkBits

This method generates random instances of Integer in the interval 0 to 16r3FFFFFFF (30 bits).


<details>
	<summary>See more</summary>
	
	nextChunkBits
	"This method generates random instances of Integer in the interval
	0 to 16r3FFFFFFF (30 bits)."

	| answer |
	[
		answer _ self privateNextChunk.
		answer > 16r40000000
	] whileTrue.
	^answer - 1
</details>

#### ParkMiller88Random>>#a

See https://en.wikipedia.org/wiki/Lehmer_random_number_generator


<details>
	<summary>See more</summary>
	
	a
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^16807.0
</details>

#### ParkMiller88Random>>#nextChunkSize

#privateNextChunk generates almost 31 bits. #nextChunkBits generates all of just 30 bits.


<details>
	<summary>See more</summary>
	
	nextChunkSize
	"#privateNextChunk generates almost 31 bits.
	#nextChunkBits generates all of just 30 bits."

	^30
</details>

#### ParkMiller88Random>>#m

<details>
	<summary>See more</summary>
	
	m

	^2147483647.0
</details>

## ParkMiller93Random

The 1993 version of the Park-Miller RNG.

### Methods
#### ParkMiller93Random>>#a

See https://en.wikipedia.org/wiki/Lehmer_random_number_generator


<details>
	<summary>See more</summary>
	
	a
	"See https://en.wikipedia.org/wiki/Lehmer_random_number_generator"

	^48271.0
</details>

## Random

The abstract class for all random number generators. If you just want a quick random integer, use: 10 atRandom Every integer interval can give a random number: (6 to: 12) atRandom SequenceableCollections can give randomly selected elements: 'pick one of these letters randomly' atRandom SequenceableCollections also respond to shuffled, as in: ($A to: $Z) shuffled The correct way to use class Random is to store one in an instance or class variable: myGenerator _ Random new. Then use it every time you need another Float in the interval [0, 1) myGenerator next You can also generate a positive integer myGenerator nextInt: 10

### Methods
#### Random>>#nextInteger: anInteger

Answer a random integer in the interval [1, anInteger]


<details>
	<summary>See more</summary>
	
	nextInteger: anInteger
	"Answer a random integer in the interval [1, anInteger]"

	| answer |
	
	anInteger strictlyPositive ifFalse: [self error: 'invalid interval'].
	[(answer _ self nextBits: anInteger highBit) >= anInteger] whileTrue.

	^ answer + 1
</details>

#### Random>>#seed: anObject

<details>
	<summary>See more</summary>
	
	seed: anObject

	self subclassResponsibility
</details>

#### Random>>#nextBoolean

<details>
	<summary>See more</summary>
	
	nextBoolean
	^ (self nextBits: 1) = 1
</details>

#### Random>>#next

Answer a random Float in the interval [0, 1)


<details>
	<summary>See more</summary>
	
	next
	"Answer a random Float in the interval [0, 1)"

	self subclassResponsibility
</details>

#### Random>>#nextBits: anInteger

Answer a random integer in the interval [0, 2^anInteger - 1]


<details>
	<summary>See more</summary>
	
	nextBits: anInteger
	"Answer a random integer in the interval [0, 2^anInteger - 1]"

	| toGo remainder answer |
	
	anInteger negative ifTrue: [self error: 'invalid interval'].
	remainder := anInteger \\ self nextChunkSize.
	answer _ remainder > 0
		ifTrue: [self nextChunkBits bitShift: remainder - self nextChunkSize]
		ifFalse: [0].
	toGo := anInteger - self nextChunkSize.
	[toGo > 0] whileTrue:
		[
			answer _ answer bitShift: self nextChunkSize :: bitXor: self nextChunkBits.
			toGo _ toGo - self nextChunkSize
		].
	^answer
</details>

#### Random>>#nextChunkBits

<details>
	<summary>See more</summary>
	
	nextChunkBits

	self subclassResponsibility
</details>

#### Random>>#nextChunkSize

<details>
	<summary>See more</summary>
	
	nextChunkSize

	self subclassResponsibility
</details>

## SmallFloat64

My instances represent 64-bit Floats whose exponent fits in 8 bits as immediate objects. This representation is only available on 64-bit systems, not 32-bit systems.

### Methods
#### SmallFloat64>>#* aNumber

Primitive. Answer the result of multiplying the receiver by aNumber. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	* aNumber 
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 549>
	^ aNumber adaptToFloat: self andSend: #*
</details>

#### SmallFloat64>>#shallowCopy

Answer the receiver, because SmallFloat64s are unique.


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Answer the receiver, because SmallFloat64s are unique."
	^self
</details>

#### SmallFloat64>>#~= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is not equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	~= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 548>
	^super ~= aNumber
</details>

#### SmallFloat64>>#exponent

Primitive. Consider the receiver to be represented as a power of two multiplied by a mantissa between one and two (#significand). Answer with the SmallInteger to whose power two is raised. Optional. See Object documentation whatIsAPrimitive. Note: invalid for infinities, NaN and zero. See comment at BoxedFloat64


<details>
	<summary>See more</summary>
	
	exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	Note: invalid for infinities, NaN and zero.
	See comment at BoxedFloat64"

	| positive |
	<primitive: 553>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent
</details>

#### SmallFloat64>>#>= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is greater than or equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	>= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 546>
	^ aNumber adaptToFloat: self andSend: #>=
</details>

#### SmallFloat64>>#ln

Answer the natural logarithm of the receiver. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 558>
	^ self lnNonPrimitive
</details>

#### SmallFloat64>>#primSqrt

Answer the square root of the receiver. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	primSqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 555>
	^Float nan
</details>

#### SmallFloat64>>#truncated

Answer with a SmallInteger equal to the value of the receiver without its fractional part. The primitive fails if the truncated value cannot be represented as a SmallInteger. In that case, the code below will compute a LargeInteger truncated value. Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 551>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]
</details>

#### SmallFloat64>>#> aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is greater than the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	> aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 544>
	^ aNumber adaptToFloat: self andSend: #>
</details>

#### SmallFloat64>>#profilerFriendlyTimesTwoPower: anInteger

This is an example on proper primitive reporting in AndreasSystemProfiler. It is a reimplementation of #timesTwoPower: in a Profiler friendly way. Compare the results of AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]]. and AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]]. See #profilerFriendlyCall:


<details>
	<summary>See more</summary>
	
	profilerFriendlyTimesTwoPower: anInteger 

	"This is an example on proper primitive reporting in AndreasSystemProfiler.
	It is a reimplementation of #timesTwoPower: in a Profiler friendly way.

	Compare the results of 
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
	and
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	See #profilerFriendlyCall:
	"
	
	| primResult |
	primResult _ self profilerFriendlyCall: [
		self profilerFriendlyPrimTimesTwoPower: anInteger ].
	primResult ifNotNil: [ :result | ^result ].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue:
			[| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue:
				["self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)
</details>

#### SmallFloat64>>#profilerFriendlyPrimTimesTwoPower: anInteger

This is an example on proper primitive reporting in AndreasSystemProfiler. See senders.


<details>
	<summary>See more</summary>
	
	profilerFriendlyPrimTimesTwoPower: anInteger 
	"
	This is an example on proper primitive reporting in AndreasSystemProfiler.
	See senders.
	"

	<primitive: 554>
	^nil
</details>

#### SmallFloat64>>#= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is equal to the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 547>
	aNumber isNumber ifFalse: [^ false].
	^ aNumber adaptToFloat: self andSend: #=
</details>

#### SmallFloat64>>#fractionPart

Primitive. Answer a Float whose value is the difference between the receiver and the receiver's asInteger value. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	fractionPart
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 552>
	^self - self truncated asFloat
</details>

#### SmallFloat64>>#exp

Answer e raised to the receiver power. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	exp
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 559>
	^ self expNonPrimitive
</details>

#### SmallFloat64>>#+ aNumber

Primitive. Answer the sum of the receiver and aNumber. Essential. Fail if the argument is not a Float. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ aNumber adaptToFloat: self andSend: #+
</details>

#### SmallFloat64>>#- aNumber

Primitive. Answer the difference between the receiver and aNumber. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	- aNumber 
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 542>
	^ aNumber adaptToFloat: self andSend: #-
</details>

#### SmallFloat64>>#<= aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is less than or equal to the argument. Otherwise return false. Fail if the argument is not a Float. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	<= aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 545>
	^ aNumber adaptToFloat: self andSend: #<=
</details>

#### SmallFloat64>>#< aNumber

Primitive. Compare the receiver with the argument and return true if the receiver is less than the argument. Otherwise return false. Fail if the argument is not a Float. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	< aNumber 
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 543>
	^ aNumber adaptToFloat: self andSend: #<
</details>

#### SmallFloat64>>#sin

Answer the sine of the receiver taken as an angle in radians. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 556>
	^ self sinNonPrimitive
</details>

#### SmallFloat64>>#timesTwoPower: anInteger

Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	timesTwoPower: anInteger
	"Primitive. Answer with the receiver multiplied by 2 raised to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 554>
	anInteger isInteger ifFalse: [ ^DomainError signal: '#timesTwoPower: only defined for Integer argument.'].
	self isFinite ifFalse: [^self].
	self isZero ifTrue: [^self].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue: [
			| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue: [
				"self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)
</details>

#### SmallFloat64>>#identityHash

Answer an integer unique to the receiver.


<details>
	<summary>See more</summary>
	
	identityHash
	"Answer an integer unique to the receiver."
	<primitive: 171>
	^self primitiveFailed
</details>

#### SmallFloat64>>#/ aNumber

Primitive. Answer the result of dividing receiver by aNumber. Fail if the argument is not a Float. Essential. See Object clas >> whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	/ aNumber 
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float.
	Essential. See Object clas >> whatIsAPrimitive."

	<primitive: 550>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^ aNumber adaptToFloat: self andSend: #/
</details>

#### SmallFloat64>>#arcTan

Answer the angle in radians. Optional. See Object documentation whatIsAPrimitive. Note: If the purpose is to recover the angle of some vector, prefer #arcTan: See, for example, Complex>>#argument


<details>
	<summary>See more</summary>
	
	arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 557>
	^self arcTanNonPrimitive
</details>

## SmallInteger

My instances are 31-bit numbers, stored in twos complement form. The allowable range is approximately +- 1 billion (see SmallInteger minVal, maxVal). Of the various classes in the Number hierarchy, SmallInteger gives: - Maximum performance - Top precision - Restricted possible values LargePositive(Negative)Integer and Fraction give increasing generality (more possible values) at the expense of performance. Float gives more generality at the expense of precision. Please see the class comments of the other Number classes.

### Methods
#### SmallInteger>>#printStringBase: b nDigits: n

Return a string representation of this number in base b with n digits (left padded with 0). Should be invoked with: 0 <= self < (b raisedToInteger: n).


<details>
	<summary>See more</summary>
	
	printStringBase: b nDigits: n
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."
	
	| integer next result |
	result := String new: n.
	integer := self.
	n to: 1 by: -1 do: [:i |
		next := integer // b.
		result byteAt: i put: (Character digitValue: (integer - (next * b))).
		integer := next].
	^result
</details>

#### SmallInteger>>#printString

Highly optimized version for base 10 and that we know it is a SmallInteger.


<details>
	<summary>See more</summary>
	
	printString
	"Highly optimized version for base 10
	and that we know it is a SmallInteger."
	
	| integer next result len |
	self = 0 ifTrue: [^'0'].
	self < 0 ifTrue: [^'-', self negated printString].
	len := self decimalDigitLength.
	result := String new: len.
	integer := self.
	len to: 1 by: -1 do: [:i |
		next := integer // 10.
		result byteAt: i put: 48 + (integer - (next * 10)).
		integer := next].
	^result
</details>

#### SmallInteger>>#>= aNumber

Primitive. Compare the receiver with the argument and answer true if the receiver is greater than or equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	>= aNumber 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger. Optional. No Lookup.
	See Object documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber
</details>

#### SmallInteger>>#gcd: anInteger

See SmallInteger (Integer) | gcd:


<details>
	<summary>See more</summary>
	
	gcd: anInteger 
	"See SmallInteger (Integer) | gcd:"
	| n m |
	n _ self.
	m _ anInteger.
	[n = 0]
		whileFalse: 
			[n _ m \\ (m _ n)].
	^ m abs
</details>

#### SmallInteger>>#// aNumber

Primitive. Divide the receiver by the argument and answer with the result. Round the result down towards negative infinity to make it a whole integer. Fail if the argument is 0 or is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	// aNumber 
	"Primitive. Divide the receiver by the argument and answer with the
	result. Round the result down towards negative infinity to make it a
	whole integer. Fail if the argument is 0 or is not a SmallInteger.
	Essential. No Lookup. See Object documentation whatIsAPrimitive. "

	<primitive: 12>
	^ super // aNumber 	"Do with quo: if primitive fails"
</details>

#### SmallInteger>>#byteReversed

Answer the receiver with bits reversed in a byte. The receiver must be between 0 and 255. The constant has been obtained by this snippet: (0 to: 255) collect: [:e | | r | r := ((e bitAnd: 2r11110000) bitShift: -4) + ((e bitAnd: 2r00001111) bitShift: 4). r := ((r bitAnd: 2r11001100) bitShift: -2) + ((r bitAnd: 2r00110011) bitShift: 2). ((r bitAnd: 2r10101010) bitShift: -1) + ((r bitAnd: 2r01010101) bitShift: 1).] as: ByteArray


<details>
	<summary>See more</summary>
	
	byteReversed
	"Answer the receiver with bits reversed in a byte.
	The receiver must be between 0 and 255.
	The constant has been obtained by this snippet:
	(0 to: 255) collect: [:e |
		| r |
		r := ((e bitAnd: 2r11110000) bitShift: -4) + ((e bitAnd: 2r00001111) bitShift: 4).
		r := ((r bitAnd: 2r11001100) bitShift: -2) + ((r bitAnd: 2r00110011) bitShift: 2).
		((r bitAnd: 2r10101010) bitShift: -1) + ((r bitAnd: 2r01010101) bitShift: 1).] as: ByteArray"
	
	^#[0 128 64 192 32 160 96 224 16 144 80 208 48 176 112 240 8 136 72 200 40 168 104 232 24 152 88 216 56 184 120 248 4 132 68 196 36 164 100 228 20 148 84 212 52 180 116 244 12 140 76 204 44 172 108 236 28 156 92 220 60 188 124 252 2 130 66 194 34 162 98 226 18 146 82 210 50 178 114 242 10 138 74 202 42 170 106 234 26 154 90 218 58 186 122 250 6 134 70 198 38 166 102 230 22 150 86 214 54 182 118 246 14 142 78 206 46 174 110 238 30 158 94 222 62 190 126 254 1 129 65 193 33 161 97 225 17 145 81 209 49 177 113 241 9 137 73 201 41 169 105 233 25 153 89 217 57 185 121 249 5 133 69 197 37 165 101 229 21 149 85 213 53 181 117 245 13 141 77 205 45 173 109 237 29 157 93 221 61 189 125 253 3 131 67 195 35 163 99 227 19 147 83 211 51 179 115 243 11 139 75 203 43 171 107 235 27 155 91 219 59 187 123 251 7 135 71 199 39 167 103 231 23 151 87 215 55 183 119 247 15 143 79 207 47 175 111 239 31 159 95 223 63 191 127 255] at: 1 + self
</details>

#### SmallInteger>>#bitOr: arg

Primitive. Answer an Integer whose bits are the logical OR of the receiver's bits and those of the argument, arg. Numbers are interpreted as having 2's-complement representation. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	bitOr: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 15>
	self >= 0 ifTrue: [^ arg bitOr: self].
	^ arg < 0
		ifTrue: [(self bitInvert bitAnd: arg bitInvert) bitInvert]
		ifFalse: [(self bitInvert bitClear: arg) bitInvert]
</details>

#### SmallInteger>>#threeDigitName

<details>
	<summary>See more</summary>
	
	threeDigitName

	| units answer |

	self = 0 ifTrue: [^''].
	units _ #('one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' 'ten' 
		'eleven' 'twelve' 'thirteen' 'fourteen' 'fifteen' 'sixteen' 'seventeen' 
		'eighteen' 'nineteen').
	self > 99 ifTrue: [
		answer _ (units at: self // 100),' hundred'.
		(self \\ 100) = 0 ifFalse: [
			answer _ answer,' ',(self \\ 100) threeDigitName
		].
		^answer
	].
	self < 20 ifTrue: [
		^units at: self
	].
	answer _ #('twenty' 'thirty' 'forty' 'fifty' 'sixty' 'seventy' 'eighty' 'ninety')
			at: self // 10 - 1.
	(self \\ 10) = 0 ifFalse: [
		answer _ answer,'-',(units at: self \\ 10)
	].
	^answer
</details>

#### SmallInteger>>#hashMultiply

Multiply by 1664525, take lower 28 bits, do not use LargeIntegers (not even in 32 bit images)


<details>
	<summary>See more</summary>
	
	hashMultiply
	"Multiply by 1664525, take lower 28 bits, do not use LargeIntegers (not even in 32 bit images)"
	| low |

	low _ self bitAnd: 16383.
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))
			bitAnd: 16r0FFFFFFF
</details>

#### SmallInteger>>#bitAnd: arg

Primitive. Answer an Integer whose bits are the logical OR of the receiver's bits and those of the argument, arg. Numbers are interpreted as having 2's-complement representation. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	bitAnd: arg 
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 14>
	self >= 0 ifTrue: [^ arg bitAnd: self].
	^ arg < 0
		ifTrue: [(arg bitInvert bitOr: self bitInvert) bitInvert]
		ifFalse: [arg bitClear: self bitInvert]
</details>

#### SmallInteger>>#= aNumber

Primitive. Compare the receiver with the argument and answer true if the receiver is equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	= aNumber 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger. Essential. No Lookup. See Object
	documentation whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber
</details>

#### SmallInteger>>#numberOfDigitsInBase: b

Return how many digits are necessary to print this number in base b. Mostly same as super but an optimized version for base 10 case


<details>
	<summary>See more</summary>
	
	numberOfDigitsInBase: b 
	"Return how many digits are necessary to print this number in base b.
	Mostly same as super but an optimized version for base 10 case"
	
	b = 10 ifFalse: [^super numberOfDigitsInBase: b].
	self < 0 ifTrue: [^self negated numberOfDigitsInBase: b].
	^self decimalDigitLength
</details>

#### SmallInteger>>#asFloat

Primitive. Answer a Float that represents the value of the receiver. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	asFloat
	"Primitive. Answer a Float that represents the value of the receiver.
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 40>
	self primitiveFailed
</details>

#### SmallInteger>>#even

Refer to the comment in Number|even.


<details>
	<summary>See more</summary>
	
	even

	^(self bitAnd: 1) = 0
</details>

#### SmallInteger>>#odd

Answer whether the receiver is an odd number.


<details>
	<summary>See more</summary>
	
	odd

	^(self bitAnd: 1) = 1
</details>

#### SmallInteger>>#digitAt: n put: value

Fails. The digits of a small integer can not be modified.


<details>
	<summary>See more</summary>
	
	digitAt: n put: value 
	"Fails. The digits of a small integer can not be modified."

	self error: 'You can''t store in a SmallInteger'
</details>

#### SmallInteger>>#decimalDigitLength

Answer the number of digits printed out in base 10. Note that this only works for positive SmallIntegers up to 63-bits.


<details>
	<summary>See more</summary>
	
	decimalDigitLength
	"Answer the number of digits printed out in base 10.
	 Note that this only works for positive SmallIntegers up to 63-bits."
	
	self <= 99999999 "8" ifTrue: [
		self <= 9999 "4" ifTrue: [
			self <= 99 "2" ifTrue: [
				self <= 9 "1" ifTrue: [ ^1].
				^2 ].
			self <= 999 "3" ifTrue: [ ^3].
			^4 ].
		self <= 999999 "6" ifTrue: [
			self <= 99999 "5" ifTrue: [ ^5 ].
			^6 ].
		self <= 9999999 "7" ifTrue: [ ^7 ].
		^8 ].
	self <= 1073741823 "10" ifTrue: [ "This is here only to avoid LargeInteger comparisons in 32-bit VMs"
		self <= 999999999 "9" ifTrue: [ ^9 ].
		^10 ].
	self <= 999999999999999 "15" ifTrue: [
		self <= 9999999999999 "13" ifTrue: [
			self <= 99999999999 "11" ifTrue: [
				self <= 9999999999 "10" ifTrue: [ ^10 ].
				^11 ].
			self <= 999999999999 "12" ifTrue: [ ^12 ].
			^13 ].
		self <= 99999999999999 "14" ifTrue: [ ^14 ].
		^15 ].
	self <= 99999999999999999 "17" ifTrue: [
		self <= 9999999999999999 "16" ifTrue: [ ^16 ].
		^17 ].
	self <= 999999999999999999 "18" ifTrue: [ ^18 ].
	^19 "(1 << 60 - 1) asString size"
</details>

#### SmallInteger>>#< aNumber

Primitive. Compare the receiver with the argument and answer with true if the receiver is less than the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	< aNumber 
	"Primitive. Compare the receiver with the argument and answer with
	true if the receiver is less than the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger. Essential. No Lookup. See
	Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber
</details>

#### SmallInteger>>#lowBit

Answer the index of the low order one bit. 2r00101000 lowBit (Answers: 4) 2r-00101000 lowBit (Answers: 4)


<details>
	<summary>See more</summary>
	
	lowBit
	" Answer the index of the low order one bit.
		2r00101000 lowBit       (Answers: 4)
		2r-00101000 lowBit      (Answers: 4)"

	self = 0 ifTrue: [^0].
	^(self bitXor: self - 1) highBit
</details>

#### SmallInteger>>#bitXor: arg

Primitive. Answer an Integer whose bits are the logical XOR of the receiver's bits and those of the argument, arg. Numbers are interpreted as having 2's-complement representation. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	bitXor: arg 
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument, arg.
	Numbers are interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 16>
	self >= 0 ifTrue: [^ arg bitXor: self].
	^ arg < 0
		ifTrue: [self bitInvert bitXor: arg bitInvert]
		ifFalse: [(self bitInvert bitXor: arg) bitInvert].
</details>

#### SmallInteger>>#identityHash

Answer a SmallInteger whose value is related to the receiver's identity. This method must not be overridden, except by immediate classes such as SmallInteger, and in Spur systems, Character and SmallFloat64. Primitive. Fails if the receiver is a SmallInteger. Essential. See Object documentation whatIsAPrimitive. Do not override.


<details>
	<summary>See more</summary>
	
	identityHash

	^self
</details>

#### SmallInteger>>#/ aNumber

Primitive. This primitive (for /) divides the receiver by the argument and returns the result if the division is exact. Fail if the result is not a whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	/ aNumber 
	"Primitive. This primitive (for /) divides the receiver by the argument
	and returns the result if the division is exact. Fail if the result is not a
	whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 10>
	aNumber isZero ifTrue: [^ZeroDivide new signalReceiver: self selector: #/ argument: aNumber ].
	^(aNumber isMemberOf: SmallInteger)
		ifTrue: [(Fraction numerator: self denominator: aNumber) reduced]
		ifFalse: [super / aNumber]
</details>

#### SmallInteger>>#printOn: stream base: base

Append a representation of this number in base b on aStream.


<details>
	<summary>See more</summary>
	
	printOn: stream base: base 
	"Append a representation of this number in base b on aStream."

	self printOn: stream base: base length: 0 padded: false
</details>

#### SmallInteger>>#~= aNumber

Primitive. Compare the receiver with the argument and answer true if the receiver is not equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	~= aNumber 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is not equal to the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger. Essential. No Lookup. See Object
	documentation whatIsAPrimitive."

	<primitive: 8>
	^super ~= aNumber
</details>

#### SmallInteger>>#nextObject

SmallIntegers are immediate objects, and, as such, do not have successors in object memory.


<details>
	<summary>See more</summary>
	
	nextObject
	"SmallIntegers are immediate objects, and, as such, do not have successors in object memory."

	self shouldNotImplement 
</details>

#### SmallInteger>>#digitLength

Answer the number of indexable fields in the receiver. This value is the same as the largest legal subscript. Included so that a SmallInteger can behave like a LargePositiveInteger or LargeNegativeInteger.


<details>
	<summary>See more</summary>
	
	digitLength
	"Answer the number of indexable fields in the receiver. This value is the 
	 same as the largest legal subscript. Included so that a SmallInteger can 
	 behave like a LargePositiveInteger or LargeNegativeInteger."

	| value length |
	length := 1.
	value := self.
	value >= 0
		ifTrue:
			[[value > 255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]]
		ifFalse:
			[[value < -255] whileTrue:
				[value := value bitShift: -8.
				 length := length + 1]].
	^length
</details>

#### SmallInteger>>#printOn: aStream base: b nDigits: n

Append a representation of this number in base b on aStream using nDigits. self must be positive.


<details>
	<summary>See more</summary>
	
	printOn: aStream base: b nDigits: n 
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self printOn: aStream base: b length: n padded: true
</details>

#### SmallInteger>>#> aNumber

Primitive. Compare the receiver with the argument and answer true if the receiver is greater than the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	> aNumber 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger. Essential. No Lookup. See Object
	documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber
</details>

#### SmallInteger>>#printStringBase: b

Return a String representation of this number in base b. For SmallIntegers, it is more efficient to print directly in a String, rather than using a Stream like super.


<details>
	<summary>See more</summary>
	
	printStringBase: b 
	"Return a String representation of this number in base b.
	For SmallIntegers, it is more efficient to print directly in a String,
	rather than using a Stream like super."

	self < 0
		ifTrue: [^ '-'
				, (self negated printStringBase: b)].
	self < b
		ifTrue: [^ String
				with: (Character digitValue: self)].
	^ self printStringBase: b nDigits: (self numberOfDigitsInBase: b)
</details>

#### SmallInteger>>#printOn: stream base: base length: minimumLength padded: padWithZeroes

<details>
	<summary>See more</summary>
	
	printOn: stream base: base length: minimumLength padded: padWithZeroes

	| n numberOfDigits totalLength divisor |
	self < 0
		ifTrue: [
			n := self negated.
			totalLength := 1 ]
		ifFalse: [
			n := self.
			totalLength := 0 ].
	numberOfDigits := n numberOfDigitsInBase: base.
	totalLength := totalLength + numberOfDigits.
	padWithZeroes ifFalse: [
		[ totalLength < minimumLength ] whileTrue: [
			stream space.
			totalLength := totalLength + 1 ] ].
	n = self ifFalse: [ stream nextPut: $- ].
	padWithZeroes ifTrue: [
		[ totalLength < minimumLength ] whileTrue: [
			stream nextPut: $0.
			totalLength := totalLength + 1 ] ].
	divisor := (base raisedToInteger: numberOfDigits - 1).
	[ divisor > 0 ] whileTrue: [
		| digit |
		digit := n // divisor.
		stream nextPut: ('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' at: digit + 1).
		n := n - (digit * divisor).
		divisor := divisor // base ]
</details>

#### SmallInteger>>#sqrt

Answer the square root of the receiver.


<details>
	<summary>See more</summary>
	
	sqrt

	self positive ifTrue: [^super sqrt].
	^NegativePowerError new signalReceiver: self selector: #sqrt arguments: {}
</details>

#### SmallInteger>>#+ aNumber

Primitive. Add the receiver to the argument and answer with the result if it is a SmallInteger. Fail if the argument or the result is not a SmallInteger Essential No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	+ aNumber 
	"Primitive. Add the receiver to the argument and answer with the result
	if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 1>
	^ super + aNumber
</details>

#### SmallInteger>>#- aNumber

Primitive. Subtract the argument from the receiver and answer with the result if it is a SmallInteger. Fail if the argument or the result is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	- aNumber 
	"Primitive. Subtract the argument from the receiver and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation
	whatIsAPrimitive."

	<primitive: 2>
	^super - aNumber
</details>

#### SmallInteger>>#digitAt: n

Answer the value of an indexable field in the receiver. LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256. Fail if the argument (the index) is not an Integer or is out of bounds.


<details>
	<summary>See more</summary>
	
	digitAt: n 
	"Answer the value of an indexable field in the receiver.  LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds."
	n > Smalltalk wordSize ifTrue: [^ 0].
	self < 0
		ifTrue: 
			[self = SmallInteger minVal ifTrue: [
				"Can't negate minVal -- treat specially"
				^ Smalltalk wordSize = 4
					ifTrue: [ #(0 0 0 64) at: n ]
					ifFalse: [ #(0 0 0 0 0 0 0 16) at: n ]].
			^ ((0-self) bitShift: (1-n)*8) bitAnd: 16rFF]
		ifFalse: [^ (self bitShift: (1-n)*8) bitAnd: 16rFF]
</details>

#### SmallInteger>>#bitShift: arg

Primitive. Answer an Integer whose value is the receiver's value shifted left by the number of bits indicated by the argument. Negative arguments shift right. The receiver is interpreted as having 2's-complement representation. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	bitShift: arg 
	"Primitive. Answer an Integer whose value is the receiver's value shifted
	left by the number of bits indicated by the argument. Negative arguments
	shift right. The receiver is interpreted as having 2's-complement representation.
	Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 17>
	self >= 0 ifTrue: [^ super bitShift: arg].
	^ arg >= 0
		ifTrue: [(self negated bitShift: arg) negated]
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].
</details>

#### SmallInteger>>#<= aNumber

Primitive. Compare the receiver with the argument and answer true if the receiver is less than or equal to the argument. Otherwise answer false. Fail if the argument is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	<= aNumber 
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger. Optional. No Lookup.
	See Object documentation whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber
</details>

#### SmallInteger>>#isLarge

<details>
	<summary>See more</summary>
	
	isLarge
	^false
</details>

#### SmallInteger>>#\\ aNumber

Primitive. Take the receiver modulo the argument. The result is the remainder rounded towards negative infinity, of the receiver divided by the argument Fail if the argument is 0 or is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	\\ aNumber 
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided by
	the argument Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 11>
	^ super \\ aNumber 	"will use // to compute it if primitive fails"
</details>

#### SmallInteger>>#* aNumber

Primitive. Multiply the receiver by the argument and answer with the result if it is a SmallInteger. Fail if the argument or the result is not a SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	* aNumber 
	"Primitive. Multiply the receiver by the argument and answer with the
	result if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 9>
	^ super * aNumber
</details>

#### SmallInteger>>#quo: aNumber

Primitive. Divide the receiver by the argument and answer with the result. Round the result down towards zero to make it a whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	quo: aNumber 
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards zero to make it a whole integer. 
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object 
	documentation whatIsAPrimitive."
	<primitive: 13>
	aNumber = 0 ifTrue: [^ZeroDivide new signalReceiver: self selector: #quo: argument: aNumber ].
	(aNumber isMemberOf: SmallInteger)
		ifFalse: [^ super quo: aNumber].
	(aNumber = -1 and: [self = self class minVal])
		ifTrue: ["result is aLargeInteger" ^ self negated].
	self primitiveFailed
</details>

#### SmallInteger>>#shallowCopy

Answer a copy of the receiver which shares the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	shallowCopy

	^self
</details>

#### SmallInteger>>#nextInstance

SmallIntegers can't be enumerated this way. There are a finite number of them from from (SmallInteger minVal) to (SmallInteger maxVal), but you'll have to enumerate them yourself with: (SmallInteger minVal) to: (SmallInteger maxVal) do: [:integer | <your code here>].


<details>
	<summary>See more</summary>
	
	nextInstance
	"SmallIntegers can't be enumerated this way.  There are a finite number of them from from (SmallInteger minVal) to (SmallInteger maxVal), but you'll have to enumerate them yourself with:
	(SmallInteger minVal) to: (SmallInteger maxVal) do: [:integer | <your code here>].
	"

	self shouldNotImplement 
</details>

