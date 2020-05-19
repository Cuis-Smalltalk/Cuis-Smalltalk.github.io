## Character

I represent a character by storing its associated Latin-9 code (ISO 8859-15). My instances are created uniquely, so that all instances of a character ($R, for example) are identical.

### Methods
#### Character>>#isLowercase

Answer whether the receiver is a lowercase letter.


<details>
	<summary>See more</summary>
	
	isLowercase
	"Answer whether the receiver is a lowercase letter."
	^ LowercaseTruthTable at: self numericValue + 1.
</details>

#### Character>>#nonImmediateNumericValue

Answer the numeric value of the receiver, if instances happen to be regular (i.e. not in Spur)


<details>
	<summary>See more</summary>
	
	nonImmediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be regular (i.e. not in Spur)"

	^self instVarAt: 1
</details>

#### Character>>#codePoint

self assert: $A codePoint hex = '16r41'. self assert: $¤ codePoint hex = '16r20AC'.


<details>
	<summary>See more</summary>
	
	codePoint
	"
	self assert: $A codePoint hex = '16r41'.
	self assert: $¤ codePoint hex = '16r20AC'.
	"
	^self class unicodeCodePoints at: self numericValue + 1
</details>

#### Character>>#iso8859s15Code

Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding. Any implementation of Character like object beyond ISO-8859-15 should answer nil. Senders, please consider that this method might answer nil. See #iso8859s15CodeForUnicodeCodePoint:


<details>
	<summary>See more</summary>
	
	iso8859s15Code
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding.
	Any implementation of Character like object beyond ISO-8859-15 should answer nil.
	Senders, please consider that this method might answer nil.
	See #iso8859s15CodeForUnicodeCodePoint:"

	^ self numericValue
</details>

#### Character>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral

	^true
</details>

#### Character>>#asText

<details>
	<summary>See more</summary>
	
	asText
	^ self asString asText
</details>

#### Character>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream nextPut: $$.
	aStream nextPut: self
</details>

#### Character>>#asSymbol

Answer a Symbol consisting of the receiver as the only element.


<details>
	<summary>See more</summary>
	
	asSymbol 
	"Answer a Symbol consisting of the receiver as the only element."

	^Symbol internCharacter: self
</details>

#### Character>>#leadingChar

See Squeak if curious.


<details>
	<summary>See more</summary>
	
	leadingChar
	"See Squeak if curious."
	^ 0
</details>

#### Character>>#to: other

Answer with a collection in ascii order -- $a to: $z


<details>
	<summary>See more</summary>
	
	to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self numericValue to: other numericValue) collect:
				[:ascii | Character numericValue: ascii]
</details>

#### Character>>#isValidInIdentifiers

Can c be part of an identifier? (unary or keyword selector, or variable name)


<details>
	<summary>See more</summary>
	
	isValidInIdentifiers
	"Can c be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ #( $_ ) statePointsTo: self ]
</details>

#### Character>>#isUppercase

Answer whether the receiver is an uppercase letter.


<details>
	<summary>See more</summary>
	
	isUppercase
	"Answer whether the receiver is an uppercase letter."
	^ UppercaseTruthTable at: self numericValue + 1.
</details>

#### Character>>#canBeGlobalVarInitial

<details>
	<summary>See more</summary>
	
	canBeGlobalVarInitial
	^self isUppercase
</details>

#### Character>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #Character or: [ super is: aSymbol ]
</details>

#### Character>>#isPathSeparator

<details>
	<summary>See more</summary>
	
	isPathSeparator
	self = $\ ifTrue: [^true].
	self = $: ifTrue: [^true].
	self = $/ ifTrue: [^true].
	^false
</details>

#### Character>>#asciiValue

Answer the value of the receiver that represents its ISO 8859-15 encoding. This is the same as ASCII for the first 128 characters.


<details>
	<summary>See more</summary>
	
	asciiValue
	"Answer the value of the receiver that represents its ISO 8859-15 encoding.
	This is the same as ASCII for the first 128 characters."

	^self numericValue
</details>

#### Character>>#hex

<details>
	<summary>See more</summary>
	
	hex
	^ String
		with: ('0123456789ABCDEF' at: self numericValue//16+1)
		with: ('0123456789ABCDEF' at: self numericValue\\16+1)
</details>

#### Character>>#isValidStartOfIdentifiers

Can c be the first character of an identifier? (unary or keyword selector, or variable name)


<details>
	<summary>See more</summary>
	
	isValidStartOfIdentifiers
	"Can c be the first character of an identifier? (unary or keyword selector, or variable name)"

	^self isLetter or: [ self  = $_ ]
</details>

#### Character>>#immediateNumericValue

Answer the numeric value of the receiver, if instances happen to be immediate (i.e. as in Spur)


<details>
	<summary>See more</summary>
	
	immediateNumericValue
	"Answer the numeric value of the receiver, if instances happen to be immediate (i.e. as in Spur)"

	<primitive: 171>
	^self primitiveFailed
</details>

#### Character>>#asCharacter

Answer the receiver itself.


<details>
	<summary>See more</summary>
	
	asCharacter
	"Answer the receiver itself."

	^self
</details>

#### Character>>#isRightBracket

<details>
	<summary>See more</summary>
	
	isRightBracket

	^self = $]
</details>

#### Character>>#asLowercase

<details>
	<summary>See more</summary>
	
	asLowercase
	^ LowercaseMappingTable at: self numericValue + 1.
</details>

#### Character>>#shallowCopy

Answer the receiver, because Characters are unique.


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Answer the receiver, because Characters are unique."
	^self
</details>

#### Character>>#isLineSeparator

Answer whether the receiver is a line separator character: line feed, cr, or form feed.


<details>
	<summary>See more</summary>
	
	isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12) statePointsTo: self numericValue
</details>

#### Character>>#>= aCharacter

Answer whether the receiver is greater than or equal to the argument.


<details>
	<summary>See more</summary>
	
	>= aCharacter 
	"Answer whether the receiver is greater than or equal to the argument."

	^aCharacter <= self
</details>

#### Character>>#isVowel

Answer whether the receiver is one of the vowels, AEIOU, in upper or lower case, and with various diacritical marks.


<details>
	<summary>See more</summary>
	
	isVowel
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or 
	lower case, and with various diacritical marks."

	^Character vowels includes: self
</details>

#### Character>>#isUnaccented

<details>
	<summary>See more</summary>
	
	isUnaccented
	^self isLetter and: [ self asUnaccented == self ]
</details>

#### Character>>#isAlphaNumeric

Answer whether the receiver is a letter or a digit.


<details>
	<summary>See more</summary>
	
	isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."

	^self isLetter or: [self isDigit]
</details>

#### Character>>#isAccented

<details>
	<summary>See more</summary>
	
	isAccented
	^self isLetter and: [ self asUnaccented ~~ self ]
</details>

#### Character>>#= aCharacter

In Cuis, Characters are unique. Therefore #= and #== are equivalent.


<details>
	<summary>See more</summary>
	
	= aCharacter 
	"In Cuis, Characters are unique. Therefore #= and #== are equivalent."

"	<primitive: 110>"
	^self == aCharacter
</details>

#### Character>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^self numericValue hash
</details>

#### Character>>#isDriveSeparator

<details>
	<summary>See more</summary>
	
	isDriveSeparator
	^self == $:
	
</details>

#### Character>>#asUnaccented

$A asUnaccented $Α asUnaccented (0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented ] (0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented asLowercase]


<details>
	<summary>See more</summary>
	
	asUnaccented
	"
	$A asUnaccented
	$Α asUnaccented
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented  asLowercase]
	"
	^ Character
		numericValue: (self class unaccentedTable at: self numericValue + 1)
</details>

#### Character>>#< aCharacter

Compare with the standard case sensitive collation order. This stuff should be in Character and not in String...


<details>
	<summary>See more</summary>
	
	< aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	^String does: self collateBefore: aCharacter
</details>

#### Character>>#identityHash

Answer the receiver's character code.


<details>
	<summary>See more</summary>
	
	identityHash
	"Answer the receiver's character code."

	"For Spur"
	<primitive: 171>

	"For preSpur (non-immediate Characters)"
	^super identityHash
</details>

#### Character>>#numericValue

Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding.


<details>
	<summary>See more</summary>
	
	numericValue
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding."

	^ Character isImmediateClass
		ifTrue: [ self immediateNumericValue ]
		ifFalse: [ self nonImmediateNumericValue ]
</details>

#### Character>>#isLetter

Answer whether the receiver is a letter.


<details>
	<summary>See more</summary>
	
	isLetter
	"Answer whether the receiver is a letter."
	^ LetterTruthTable at: self numericValue + 1
</details>

#### Character>>#storeOn: aStream

Character literals are preceded by '$'.


<details>
	<summary>See more</summary>
	
	storeOn: aStream
	"Character literals are preceded by '$'."

	aStream nextPut: $$; nextPut: self
</details>

#### Character>>#digitValue

Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 otherwise. This is used to parse literal numbers of radix 2-36. $0 numericValue = 48 $9 numericValue = 57 $A numericValue = 65 $Z numericValue = 90 $7 digitValue = 7


<details>
	<summary>See more</summary>
	
	digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36.
	$0 numericValue = 48
	$9 numericValue = 57
	$A numericValue = 65
	$Z numericValue = 90 
	$7 digitValue = 7 
	"

	| nv |
	nv _ self numericValue.
	(nv between: 48 and: 57)
		ifTrue: [ ^ nv - 48 ].
	(nv between: 65 and: 90)
		ifTrue: [ ^ nv - 55 ].
	^ -1
</details>

#### Character>>#asString

Answer a string that represents the receiver. Don't include extra quotes for Strings. This message has may uses. Some of them call it to convert numbers to a string, and/or can be converted back to number. Other uses are for the UI. Some need conversion from ByteArray (where the result is not a description of the ByteArray, the same contents in a different class). Others need conversion from Text. Or from Character or Symbol. In many cases, the receiver might sometimes be a String (and the same String is desired). It would be great to check every sender and change them for a message with a more specific meaning. Maybe some day. In addition this message is used by code that also runs in other Smalltalks, and removing it would affect portability. In any case, in your code, if possible, use a more specific method. See the comments at: #printString #displayStringOrText #asString #storeString


<details>
	<summary>See more</summary>
	
	asString
	^ String with: self
</details>

#### Character>>#isDigit

Answer whether the receiver is a digit.


<details>
	<summary>See more</summary>
	
	isDigit
	"Answer whether the receiver is a digit."

	^ self numericValue between: 48 and: 57
</details>

#### Character>>#> aCharacter

Compare with the standard case sensitive collation order. This stuff should be in Character and not in String...


<details>
	<summary>See more</summary>
	
	> aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	^String does: self collateAfter: aCharacter
</details>

#### Character>>#tokenish

Answer whether the receiver is a valid token-character--letter, digit, or colon.


<details>
	<summary>See more</summary>
	
	tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isValidInIdentifiers or: [self = $:]
</details>

#### Character>>#withDiacriticalMark: anUnicodeCodePoint

Answer the character resulting from adding a diacritical mark (accent) to a letter. If the result is unsupported in ISO 8859-15, answer the receiver. Supported diacritical marks are: U+0300 COMBINING GRAVE ACCENT U+0301 COMBINING ACUTE ACCENT U+0302 COMBINING CIRCUMFLEX ACCENT U+0303 COMBINING TILDE U+0308 COMBINING DIAERESIS U+030A COMBINING RING ABOVE U+030C COMBINING CARON $a withDiacriticalMark: 16r301 $N withDiacriticalMark: $~ $Z withDiacriticalMark: $v invalid: $9 withDiacriticalMark:$v $A withDiacriticalMark: $v $Α withDiacriticalMark: $v $A withDiacriticalMark: 1244


<details>
	<summary>See more</summary>
	
	withDiacriticalMark: anUnicodeCodePoint
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-15, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$Α withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i _ ((anUnicodeCodePoint isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $° $v)]) indexOf: anUnicodeCodePoint
				) + 1.
	answer _ (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer = $- ifFalse: [answer] ifTrue: [self]
</details>

#### Character>>#comeFullyUpOnReload: smartRefStream

Use existing an Character. Don't use the new copy.


<details>
	<summary>See more</summary>
	
	comeFullyUpOnReload: smartRefStream
	"Use existing an Character.  Don't use the new copy."

	^ self class numericValue: self numericValue
</details>

#### Character>>#isDriveLetter

<details>
	<summary>See more</summary>
	
	isDriveLetter
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: self asUppercase
	
</details>

#### Character>>#isValidInBinarySelectors

Can be part of a binary selector? $< isValidInBinarySelectors $| isValidInBinarySelectors $^ isValidInBinarySelectors $: isValidInBinarySelectors


<details>
	<summary>See more</summary>
	
	isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	^#(verticalBar upArrow xColon xBinary) statePointsTo: (Scanner typeTable at: self numericValue)
</details>

#### Character>>#<= aCharacter

Answer whether the receiver is less than or equal to the argument.


<details>
	<summary>See more</summary>
	
	<= aCharacter 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aCharacter) not
</details>

#### Character>>#isSpecial

Answer whether the receiver is one of the special characters


<details>
	<summary>See more</summary>
	
	isSpecial
	"Answer whether the receiver is one of the special characters"

	^'+-/\*~<>=@,%|&?!' includes: self
</details>

#### Character>>#isSeparator

Answer whether the receiver is one of the separator characters space, tab, lf, cr, or form feed.


<details>
	<summary>See more</summary>
	
	isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."

	^ #(32 9 10 13 12) statePointsTo: self numericValue
</details>

#### Character>>#asUppercase

If the receiver is lowercase, answer its matching uppercase Character.


<details>
	<summary>See more</summary>
	
	asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	^ UppercaseMappingTable at: self numericValue + 1.
</details>

## CharacterSet

A set of characters. Lookups for inclusion are very fast.

### Methods
#### CharacterSet>>#hash

A default hash function for any collection. Note that this method is insensitive to contents when the size is greater than 10, so critical applications that compare many large collections of the same length will want to refine this behavior.


<details>
	<summary>See more</summary>
	
	hash
	^self byteArrayMap hash
</details>

#### CharacterSet>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	map _ ByteArray new: 256 withAll: 0.
</details>

#### CharacterSet>>#add: aCharacter

Include newObject as one of the receiver's elements. Answer newObject. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	add: aCharacter
	map at: aCharacter numericValue+1  put: 1.
</details>

#### CharacterSet>>#complement

return a character set containing precisely the characters the receiver does not


<details>
	<summary>See more</summary>
	
	complement
	"return a character set containing precisely the characters the receiver does not"
	| set |
	set _ CharacterSet allCharacters.
	self do: [ :c | set remove: c ].
	^set
</details>

#### CharacterSet>>#includes: aCharacter

Answer whether anObject is one of the receiver's elements.


<details>
	<summary>See more</summary>
	
	includes: aCharacter
	(aCharacter is: #Character) ifFalse: [ ^ false ].
	^(map at: aCharacter numericValue + 1) > 0
</details>

#### CharacterSet>>#do: aBlock

evaluate aBlock with each character in the set


<details>
	<summary>See more</summary>
	
	do: aBlock
	"evaluate aBlock with each character in the set"

	Character characterTable do: [ :c |
		(self includes: c) ifTrue: [ aBlock value: c ] ]

</details>

#### CharacterSet>>#byteArrayMap

return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't. Intended for use by primitives only


<details>
	<summary>See more</summary>
	
	byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"
	^map
</details>

#### CharacterSet>>#remove: aCharacter

Remove oldObject from the receiver's elements. Answer oldObject unless no element is equal to oldObject, in which case, raise an error. ArrayedCollections cannot respond to this message.


<details>
	<summary>See more</summary>
	
	remove: aCharacter
	map at: aCharacter numericValue + 1  put: 0
</details>

#### CharacterSet>>#= anObject

Default implementation. Usually redefined in subclasses.


<details>
	<summary>See more</summary>
	
	= anObject
	self == anObject ifTrue: [ ^ true ].
	^self species == anObject species and: [
		self byteArrayMap = anObject byteArrayMap ]
</details>

## String

A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-9 (ISO 8859-15) encoding. Each character takes 1 byte. String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code. Here are a few useful methods to look at... String match: String contractTo: String also inherits many useful methods from its hierarchy, such as SequenceableCollection , SequenceableCollection copyReplaceAll:with: Cuis includes limited support for Unicode. A ByteArray can hold any Unicode string in utf-8. There is support for converting to and from instances of String. As String can not hold all Unicode code points, (just the ISO 8859-15 subset), others are converted to NCRs. See #fromUtf8:hex:trimLastNull: and #asUtf8: .

### Methods
#### String>>#copyReplacing: ranges with: newString

Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan


<details>
	<summary>See more</summary>
	
	copyReplacing: ranges with: newString

	"Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan"

	^ self class streamContents: [ :replacementStream | self copyReplacing: ranges with: newString into: replacementStream ]
		
</details>

#### String>>#exceptionalPlural

<details>
	<summary>See more</summary>
	
	exceptionalPlural
	| singular plural index |
	singular := #(
		'addendum' 'aircraft' 'alga' 'alumnus' 'amoeba' 'antenna' 'appendix'
		'bacterium' 'barracks'
		'cactus' 'child' 'criterion' 'curriculum'
		'datum' 'deer' 'dwarf'
		'echo' 'ephemeris' 'embargo'
		'fish' 'focus' 'foot' 'forum' 'fungus'
		'gallows' 'genus' 'goose'
		'hero'
		'index' 'is'
		'larva' 'louse'
		'matrix' 'means' 'memorandum' 'mithos' 'money' 'mouse'
		'nucleus'
		'offspring' 'ox'
		'person' 'phenomenon' 'potato' 'proof'
		'roof'
		'series' 'sheep' 'species' 'spoof' 'stimulus' 'syllabus'
		'tomato' 'tooth' 'torpedo' 'trilby'
		'vertebra' 'veto'
		'was').
	plural := #(
		'addenda' 'aircraft' 'algae' 'alumni' 'amoebae' 'antennae' 'appendices'
		'bacteria' 'barracks'
		'cacti' 'children' 'criteria' 'curricula'
		'data' 'deer' 'dwarfs'
		'echoes' 'ephemerides' 'embargoes'
		'fish' 'foci' 'feet' 'fora' 'fungi'
		'gallows' 'genera' 'geese'
		'heroes'
		'indices'
		'are'
		'larvae' 'lice'
		'matrices' 'means' 'memoranda' 'mythoi' 'moneys' 'mice'
		'nuclei'
		'offspring' 'oxen'
		'people' 'phenomena' 'potatoes' 'proofs'
		'roofs'
		'series' 'sheep' 'species' 'spoofs' 'stimuli' 'syllabi'
		'tomatoes' 'teeth' 'torpedoes' 'trilbys'
		'vertebrae' 'vetoes'
		'were').
	index := singular indexOf: self.
	^index > 0 ifTrue: [plural at: index]
</details>

#### String>>#adaptToPoint: rcvr andSend: selector

If I am involved in arithmetic with a point, convert me to a number.


<details>
	<summary>See more</summary>
	
	adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a point, convert me to a number."

"	^ rcvr perform: selector with: self asNumber"
	self shouldNotImplement.
</details>

#### String>>#isLiteral

Answer whether the receiver has a literal text form recognized by the compiler. The literal form must be provided by #storeOn:


<details>
	<summary>See more</summary>
	
	isLiteral

	^true
</details>

#### String>>#black

Stuff like 'Hello world' black edit


<details>
	<summary>See more</summary>
	
	black
	"Stuff like
	'Hello world' black edit
	"
	^self asText black
</details>

#### String>>#findDelimiters: delimiters startingAt: start

Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.


<details>
	<summary>See more</summary>
	
	findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1
</details>

#### String>>#substrings

Answer an array of the substrings that compose the receiver.


<details>
	<summary>See more</summary>
	
	substrings
	"Answer an array of the substrings that compose the receiver."
	| result end beginning |

	result _ WriteStream on: (Array new: 10).



	end _ 0.
	"find one substring each time through this loop"
	[ 
		"find the beginning of the next substring"
		beginning _ self indexOfAnyOf: CSNonSeparators startingAt: end+1 ifAbsent: nil.
		beginning notNil ] 
	whileTrue: [
		"find the end"
		end _ self indexOfAnyOf: CSSeparators startingAt: beginning ifAbsent: [ self size + 1 ].
		end _ end - 1.

		result nextPut: (self copyFrom: beginning to: end).

	].

	^result contents
</details>

#### String>>#asSymbol

Answer the unique Symbol whose characters are the characters of the string.


<details>
	<summary>See more</summary>
	
	asSymbol
	"Answer the unique Symbol whose characters are the characters of the 
	string."

	^Symbol intern: self
</details>

#### String>>#justified

Stuff like ('Hello world' justified ) edit


<details>
	<summary>See more</summary>
	
	justified
	"Stuff like
	('Hello world' justified ) edit
	"
	^self asText justified
</details>

#### String>>#truncateTo: smallSize

return myself or a copy shortened to smallSize. 1/18/96 sw


<details>
	<summary>See more</summary>
	
	truncateTo: smallSize
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]
</details>

#### String>>#displayAt: aPoint

Display the receiver as a DisplayText at aPoint on the display screen.


<details>
	<summary>See more</summary>
	
	displayAt: aPoint 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint.
	DisplayScreen screenUpdateRequired: nil
</details>

#### String>>#pathAndLocalName

<details>
	<summary>See more</summary>
	
	pathAndLocalName
	| index path localName |
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^{''. self}].

	path := self copyFrom: 1 to: index-1.
	localName := self copyFrom: index+1 to: self size.

	^{path. localName}

	
</details>

#### String>>#commonPrefixWith: aString

Return the size of the longest common prefix with aString. Do a character-by-character comparison between the receiver and aString. Return the index of the final character that matched exactly. 'this is a string' commonPrefixWith: 'this is another' 'this is a string' commonPrefixWith: '' '' commonPrefixWith: 'this is another' 't' commonPrefixWith: 'this is another' 'txxxxxxxxx' commonPrefixWith: 'this is another'


<details>
	<summary>See more</summary>
	
	commonPrefixWith: aString
	"Return the size of the longest common prefix with aString.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly.
	'this is a string' commonPrefixWith: 'this is another'
	'this is a string' commonPrefixWith: ''
	'' commonPrefixWith: 'this is another'
	't' commonPrefixWith: 'this is another'
	'txxxxxxxxx' commonPrefixWith: 'this is another'
		"
	^ self commonPartWith: aString startAt: 1 stopAt: self size applying: [ :c | c ]
</details>

#### String>>#editLabel: labelString

<details>
	<summary>See more</summary>
	
	editLabel: labelString

	TextModel new contents: self; openLabel: labelString
</details>

#### String>>#fileContents: aString

<details>
	<summary>See more</summary>
	
	fileContents: aString
	self asFileEntry fileContents: aString.
	^aString
</details>

#### String>>#upToFirstPathSeparator

<details>
	<summary>See more</summary>
	
	upToFirstPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfFirstPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	
</details>

#### String>>#getEnclosedExpressionFrom: aStream

private - get the expression enclosed between '{' and '}' and remove all the characters from the stream


<details>
	<summary>See more</summary>
	
	getEnclosedExpressionFrom: aStream 
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"
	| result currentChar |
	result := String new writeStream.

	[aStream atEnd 
		or: [(currentChar := aStream next) == $}]]
		whileFalse: [result nextPut: currentChar].

	^ result contents withBlanksTrimmed
</details>

#### String>>#findString: subString startingAt: start

Answer the index of subString within the receiver, starting at start. If the receiver does not contain subString, answer 0.


<details>
	<summary>See more</summary>
	
	findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: self startingAt: start matchTable: CaseSensitiveOrder
</details>

#### String>>#append: aStringOrText

<details>
	<summary>See more</summary>
	
	append: aStringOrText

	^ aStringOrText appendToString: self
</details>

#### String>>#gray

Stuff like 'Hello world' gray edit


<details>
	<summary>See more</summary>
	
	gray
	"Stuff like
	'Hello world' gray edit
	"
	^self asText gray
</details>

#### String>>#withBlanksCondensed

Return a copy of the receiver with leading/trailing blanks removed and consecutive white spaces condensed as a single space.


<details>
	<summary>See more</summary>
	
	withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed as a single space."

	| trimmed lastWasBlank |
	trimmed _ self withBlanksTrimmed.
	^String streamContents: [ :stream |
		lastWasBlank _ false.
		trimmed do: [ :c |
			c isSeparator
				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]
				ifFalse: [ stream nextPut: c ].
			lastWasBlank _ c isSeparator ]].

	" 
	' abc  d   ' withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	"
</details>

#### String>>#asDriveName

Answer a real drive name, or else answer nil. (Original FileMan implementation would answer first token on Mac even if it is not a Drive Name, and self in any case in other Unix variants) Windows 'C:\' asDriveName 'C:' 'NotAChance' asDriveName nil Linux '/media/cdrom' asDriveName nil MacOsX '/SanDisk32-NTFS' asDriveName nil


<details>
	<summary>See more</summary>
	
	asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

MacOsX
    '/SanDisk32-NTFS' asDriveName nil
	
	"

	| candidate |
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FileIOAccessor default onMacClassic) ifTrue: [
		candidate _ self upToFirstPathSeparator.
		"Aparently on Mac Classic, 
			xxx/yyy means xxx must be a drive name
			/xxx/yyy means xxx could be any folder in root. Check to make sure!
		"
		('/' asDirectoryEntry directoryNames includes: candidate) ifTrue: [
			^candidate ]].
	
	^ nil
</details>

#### String>>#lowercasePlural

<details>
	<summary>See more</summary>
	
	lowercasePlural
	| last |
	self exceptionalPlural ifNotNil: [:pl | ^pl].
	self isUninflictedNoun ifTrue: [^self].
	last := self last.
	last = $y ifTrue: [
		#('ay' 'ey' 'oy' 'uy') do: [:t |
			(self endsWith: t) ifTrue: [^self , 's'].
			^self allButLast , 'ies']].
	#('zz' 'ch' 'sh') do: [:t | (self endsWith: t) ifTrue: [^self , 'es']].
	last = $s ifTrue: [
		self = 'its' ifTrue: [^'their'].
		#('bs' 'cs' 'ds' 'ks' 'ls' 'ms' 'rs' 'ts' 'ws')
			do: [:t | (self endsWith: t) ifTrue: [^self]].
		#('sis' 'xis')
			do: [:t | (self endsWith: t) ifTrue: [^(self allButLast: 2) , 'es']]].
	last = $z ifTrue: [^self , 'zes'].
	(last = $x or: [last = $s]) ifTrue: [^self , 'es'].
	(self endsWith: 'man') ifTrue: [^(self allButLast: 2) , 'en'].
	last = $f ifTrue: [^self allButLast , 'ves'].
	(self endsWith: 'fe') ifTrue: [^(self allButLast: 2) , 'ves'].
	^self , 's'
</details>

#### String>>#crc16

Compute a 16 bit cyclic redundancy check.


<details>
	<summary>See more</summary>
	
	crc16
	"Compute a 16 bit cyclic redundancy check."

	| crc |
	crc := 0.
	self do: [:c |
		crc := (crc bitShift: -8) bitXor: (
		 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
			16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
			16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
			16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
			16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
			16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
			16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
			16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
			16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
			16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
			16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
			16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
			16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
			16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
			16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
			16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
			16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
			16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
			16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
			16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
			16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
			16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
			16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
			16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
			16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
			16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
			16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
			16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
			16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
			16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
			16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
			16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c numericValue) bitAnd: 16rFF) + 1) ].
	^crc
</details>

#### String>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105 error: ec>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
</details>

#### String>>#upToLastPathSeparator

<details>
	<summary>See more</summary>
	
	upToLastPathSeparator
	| index |
	self ifEmpty: [^self].
	index := self indexOfLastPathSeparator.
	index = 0 ifTrue: [^self].
	^self copyFrom: 1 to: index - 1
	
</details>

#### String>>#appendToText: aText

<details>
	<summary>See more</summary>
	
	appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText
</details>

#### String>>#>= aString

Answer whether the receiver sorts after or equal to aString. The collation order is case sensitive.


<details>
	<summary>See more</summary>
	
	>= aString 
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	^ (self compare: self with: aString collated: CaseSensitiveOrder) >= 2
</details>

#### String>>#lines

Answer an array of lines composing this receiver without the line ending delimiters.


<details>
	<summary>See more</summary>
	
	lines
	"Answer an array of lines composing this receiver without the line ending delimiters."

	^Array
		streamContents: [ :lines | self linesDo: [ :aLine | lines nextPut: aLine ]]
		estimatedSize: (self size // 60 max: 16)
</details>

#### String>>#asHex

<details>
	<summary>See more</summary>
	
	asHex
	| stream |
	stream _ WriteStream on: (String new: self size * 2).
	self do: [ :ch | stream nextPutAll: ch hex ].
	^stream contents
</details>

#### String>>#romanNumber

<details>
	<summary>See more</summary>
	
	romanNumber
	| value v1 v2 |
	value _ v1 _ v2 _ 0.
	self reverseDo:
		[:each |
		v1 _ #(1 5 10 50 100 500 1000) at: ('IVXLCDM' indexOf: each).
		v1 >= v2
			ifTrue: [value _ value + v1]
			ifFalse: [value _ value - v1].
		v2 _ v1].
	^ value
</details>

#### String>>#= aString

Answer whether the receiver sorts equally as aString. This means same characters in same order.


<details>
	<summary>See more</summary>
	
	= aString 
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a String or Symbol, maybe it is a Text?"
	aString species == String ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].
	
	self size > 256 ifTrue: [
		self hashQuick = aString hashQuick ifFalse: [ ^false ]].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2
</details>

#### String>>#displayOn: aDisplayMedium at: aPoint

Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text.


<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	^self displayOn: aDisplayMedium at: aPoint textColor: `Color black`
	"
	'Display' displayOn: Display at: 10@10. Display forceToScreen.
	"
</details>

#### String>>#fileContents

<details>
	<summary>See more</summary>
	
	fileContents
	^self asFileEntry fileContents
</details>

#### String>>#skipDelimiters: delimiters startingAt: start

Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1. Assumes the delimiters to be a non-empty string.


<details>
	<summary>See more</summary>
	
	skipDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1
</details>

#### String>>#/ arg

If working with file paths, just use $/ Or better yet, use DirectoryEntry protocol


<details>
	<summary>See more</summary>
	
	/ arg
	"If working with file paths, just use $/
	Or better yet, use DirectoryEntry protocol"

	self shouldNotImplement
</details>

#### String>>#afterBlanksEndsWith: aTail

<details>
	<summary>See more</summary>
	
	afterBlanksEndsWith: aTail

	^(self endsWith: aTail) and: [ self firstNoBlankIndex = (self size - aTail size + 1) ]

</details>

#### String>>#storeOn: aStream

Print inside string quotes, doubling inbedded quotes.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x _ self at: i).
		x == $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'
</details>

#### String>>#asWeek

'2008-W52' asWeek. '2008-W53' asWeek. 'Invalid format!'. '2009-W01' asWeek '2009-W02' asWeek '2009-W53' asWeek '2010-W01' asWeek '2010-W02' asWeek '2008-W52' asWeek start. '2009-W01' asWeek start '2009-W02' asWeek start '2009-W53' asWeek start '2010-W01' asWeek start '2010-W02' asWeek start


<details>
	<summary>See more</summary>
	
	asWeek
	"
		'2008-W52' asWeek.
		'2008-W53' asWeek. 'Invalid format!'.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
		'2008-W52' asWeek start.
		'2009-W01' asWeek start
		'2009-W02' asWeek start
		'2009-W53' asWeek start
		'2010-W01' asWeek start
		'2010-W02' asWeek start
	"

	^ Week fromString: self
</details>

#### String>>#string

Compatibility with Text


<details>
	<summary>See more</summary>
	
	string
	"Compatibility with Text"
	^self
</details>

#### String>>#sameAs: aString

Answer whether the receiver sorts equal to aString. The collation sequence is ascii with case differences ignored.


<details>
	<summary>See more</summary>
	
	sameAs: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	^ (self compare: self with: aString collated: CaseInsensitiveOrder) = 2
</details>

#### String>>#withoutSeparators

<details>
	<summary>See more</summary>
	
	withoutSeparators
	
	^self reject: [ :aCharacter | aCharacter isSeparator ]
</details>

#### String>>#withoutLeadingBlanks

Return a copy of the receiver from which leading blanks have been trimmed.


<details>
	<summary>See more</summary>
	
	withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self firstNoBlankIndex.
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"

</details>

#### String>>#format: aCollection

format the receiver with aCollection simplest example: 'foo {1} bar' format: {Date today}. complete example: '\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.


<details>
	<summary>See more</summary>
	
	format: aCollection 
	"format the receiver with aCollection  
	 
	simplest example:  
	'foo {1} bar' format: {Date today}.
	 
	complete example:  
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.  
	"
	| result stream |
	result := String new writeStream.
	stream := self readStream.

	[stream atEnd]
		whileFalse: [| currentChar | 
			currentChar := stream next.
			currentChar == ${
				ifTrue: [| expression | 
					expression := self getEnclosedExpressionFrom: stream.
					result
						nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
				ifFalse: [
					currentChar == $\
						ifTrue: [stream atEnd
								ifFalse: [result nextPut: stream next]]
						ifFalse: [result nextPut: currentChar]]].

	^ result contents
</details>

#### String>>#indexOf: aCharacter

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	indexOf: aCharacter

	(aCharacter class == Character) ifFalse: [^ 0].
	^ String indexOfByte: aCharacter numericValue inString: self startingAt: 1
</details>

#### String>>#magenta

Stuff like 'Hello world' magenta edit


<details>
	<summary>See more</summary>
	
	magenta
	"Stuff like
	'Hello world' magenta edit
	"
	^self asText magenta
</details>

#### String>>#compare: string1 with: string2 collated: order

Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array.


<details>
	<summary>See more</summary>
	
	compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>
	self var: #string1 declareC: 'unsigned char *string1'.
	self var: #string2 declareC: 'unsigned char *string2'.
	self var: #order declareC: 'unsigned char *order'.

	len1 _ string1 size.
	len2 _ string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 _ order at: (string1 basicAt: i) + 1.
		c2 _ order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].

</details>

#### String>>#caseSensitiveLessOrEqual: aString

Answer whether the receiver sorts before or equal to aString. The collation order is case sensitive.


<details>
	<summary>See more</summary>
	
	caseSensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	^ (self compare: self with: aString collated: CaseSensitiveOrder) <= 2
</details>

#### String>>#compare: aString caseSensitive: aBool

Answer a comparison code telling how the receiver sorts relative to aString: 1 - before 2 - equal 3 - after.


<details>
	<summary>See more</summary>
	
	compare: aString caseSensitive: aBool
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	"
	| map |
	map := aBool ifTrue:[CaseSensitiveOrder] ifFalse:[CaseInsensitiveOrder].
	^self compare: self with: aString collated: map
</details>

#### String>>#displayOn: aDisplayMedium

Display the receiver on the given DisplayMedium. 5/16/96 sw


<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium
	"Display the receiver on the given DisplayMedium.  5/16/96 sw"

	self displayOn: aDisplayMedium at: `0 @ 0`
</details>

#### String>>#endOfParagraphBefore: aNumber

Return the index of the last Character newLineCharacter before position aNumber, or zero if this is the first paragraph. 'ddd' endOfParagraphBefore: 3 'dd d' endOfParagraphBefore: 4


<details>
	<summary>See more</summary>
	
	endOfParagraphBefore: aNumber
	"Return the index of the last Character newLineCharacter before position aNumber, or zero if this is the first paragraph.
	'ddd' endOfParagraphBefore: 3
	'dd
	d' endOfParagraphBefore: 4
	"
	^ self lastIndexOf: Character newLineCharacter startingAt: aNumber - 1 ifAbsent: [ 0 ]
</details>

#### String>>#isRelativePathName

<details>
	<summary>See more</summary>
	
	isRelativePathName
	self ifEmpty: [^false].
	self isRelativeMark ifTrue: [^true].
	^#('.' '..') includes: self upToFirstPathSeparator 
</details>

#### String>>#copyReplaceTokens: oldSubstring with: newSubstring

Replace all occurrences of oldSubstring that are surrounded by non-alphanumeric characters


<details>
	<summary>See more</summary>
	
	copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"
</details>

#### String>>#initialIntegerOrNil

Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit


<details>
	<summary>See more</summary>
	
	initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self size = 0 or: [self first isDigit not]) ifTrue: [^ nil].
	firstNonDigit _ (self findFirst: [:m | m isDigit not]).
	firstNonDigit = 0 ifTrue: [firstNonDigit _ self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"

</details>

#### String>>#at: index put: aCharacter

Primitive. Store the Character in the field of the receiver indicated by the index. Fail if the index is not an Integer or is out of bounds, or if the argument is not a Character. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	(aCharacter is: #Character)
		ifTrue: [	
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']
</details>

#### String>>#withDescriptiveLineEndings

<details>
	<summary>See more</summary>
	
	withDescriptiveLineEndings
	^ self withLineEndings: '[enter]'
</details>

#### String>>#correctAgainstDictionary: wordDict continuedFrom: oldCollection

Like correctAgainst:continuedFrom:. Use when you want to correct against a dictionary.


<details>
	<summary>See more</summary>
	
	correctAgainstDictionary: wordDict continuedFrom: oldCollection
	"Like correctAgainst:continuedFrom:.  Use when you want to correct against a dictionary."

	^ wordDict
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordDict keysDo: action ]
				continuedFrom: oldCollection ]
</details>

#### String>>#numArgs

Answer either the number of arguments that the receiver would take if considered a selector. Answer -1 if it couldn't be a selector. Note that currently this will answer -1 for anything begining with an uppercase letter even though the system will accept such symbols as selectors. It is intended mostly for the assistance of spelling correction.


<details>
	<summary>See more</summary>
	
	numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self size = 0 ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(self
			findSubstring: '~'
			in: self
			startingAt: 1
			matchTable: Tokenish) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[
		(ix _ self
			findSubstring: ':'
			in: self
			startingAt: start
			matchTable: CaseSensitiveOrder) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.
</details>

#### String>>#findLastOccurrenceOfString: subString startingAt: start

Answer the index of the last occurrence of subString within the receiver, starting at start. If the receiver does not contain subString, answer 0. Case-sensitive match used.


<details>
	<summary>See more</summary>
	
	findLastOccurrenceOfString: subString startingAt: start 
	"Answer the index of the last occurrence of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.  Case-sensitive match used."

	| last now |
	last := self findString: subString startingAt: start.
	last = 0 ifTrue: [^ 0].
	[last > 0] whileTrue:
		[now := last.
		last := self findString: subString startingAt: last + 1].

	^ now

</details>

#### String>>#rightFlush

Stuff like ('Hello world' rightFlush ) edit


<details>
	<summary>See more</summary>
	
	rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	^self asText rightFlush
</details>

#### String>>#asText

Answer a Text whose string is the receiver.


<details>
	<summary>See more</summary>
	
	asText
	"Answer a Text whose string is the receiver."

	^Text fromString: self
</details>

#### String>>#asPathComponents

<details>
	<summary>See more</summary>
	
	asPathComponents
	| tokens |

	self isRelativePathName ifTrue: [self error: 'relative form is invaild!'].

	tokens := self asPathTokens.
	^tokens
</details>

#### String>>#withoutEnclosing: aCharacter

'*Hello*' withoutEnclosing: $*


<details>
	<summary>See more</summary>
	
	withoutEnclosing: aCharacter
	"
	'*Hello*' withoutEnclosing: $*
	"
	| s |
	s _ self size.
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]
</details>

#### String>>#findSelector

Revised to use scanner for better removal of extraneous stuff


<details>
	<summary>See more</summary>
	
	findSelector
	"Revised to use scanner for better removal of extraneous stuff"
	| sel colonIndex |
	sel _ self withBlanksTrimmed.
	colonIndex _ sel indexOf: $:.
	"possible keyword selector"
	(colonIndex > 1 and: [ (self at: colonIndex - 1) isLetter ]) ifTrue: [
		sel _ Scanner findSelectorIn: sel ].
	sel isEmpty ifTrue: [ ^ nil ].
	Symbol
		hasInterned: sel
		ifTrue: [ :aSymbol |
			^ aSymbol ].
	^ nil.
</details>

#### String>>#asPlural

<details>
	<summary>See more</summary>
	
	asPlural
	| k trimmed plural n |
	k := self findFirst: [:ch | ch isSeparator not].
	k > 1
		ifTrue: [^(self copyFrom: 1 to: k - 1) , (self allButFirst: k - 1) asPlural].
	trimmed := self withBlanksTrimmed.
	trimmed isEmpty ifTrue: [^''].
	plural := trimmed asLowercase lowercasePlural.
	n := trimmed size min: plural size.
	1 to: n do: [:i |
		(trimmed at: i) isUppercase
			ifTrue: [plural at: i put: (plural at: i) asUppercase]].
	^plural
</details>

#### String>>#indexOfAnyOf: aCharacterSet

returns the index of the first character in the given set, starting from start


<details>
	<summary>See more</summary>
	
	indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from start"

	^String findFirstInString: self inSet: aCharacterSet byteArrayMap startingAt: 1
</details>

#### String>>#join: aCollection

'*' join: #('WWWWW' 'W EW' 'zzzz') -> 'WWWWW*W EW*zzzz'


<details>
	<summary>See more</summary>
	
	join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	| w |		
	w := WriteStream on: String new.
	aCollection do: [ :elem | w nextPutAll: elem asString] separatedBy: [w nextPutAll: self].
	^String fromString: w contents
</details>

#### String>>#flattenTo: flattenedStream

<details>
	<summary>See more</summary>
	
	flattenTo: flattenedStream

	flattenedStream nextPut: self
</details>

#### String>>#prefixAndSuffix: aCharacter

Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it. Answer nil if aCharacter is not in self '1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-


<details>
	<summary>See more</summary>
	
	prefixAndSuffix: aCharacter
	"Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it.
	Answer nil if aCharacter is not in self
	'1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-
	"
	| i |
	i _ self findLast: [ :c | c = aCharacter ].
	i = 0 ifTrue: [ ^ nil ].
	^ { self copyFrom: 1 to: i-1 . self copyFrom: i+1 to: self size }
</details>

#### String>>#translateFrom: start  to: stop  table: table

translate the characters in the string by the given table, in place


<details>
	<summary>See more</summary>
	
	translateFrom: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	String translate: self  from: start to: stop table: table
</details>

#### String>>#withBlanksTrimmed

Return a copy of the receiver from which leading and trailing blanks have been trimmed.


<details>
	<summary>See more</summary>
	
	withBlanksTrimmed
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first |
	
	first _ self firstNoBlankIndex.
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	
	^ self copyFrom: first to: self lastNoBlankIndex 

	" ' abc  d   ' withBlanksTrimmed"

</details>

#### String>>#indexOfLastPathSeparator

'pp.txt' indexOfLastPathSeparator '/pp.txt' indexOfLastPathSeparator 'a/pp.txt' indexOfLastPathSeparator 'b/a/pp.txt' indexOfLastPathSeparator '/b/a/pp.txt' indexOfLastPathSeparator


<details>
	<summary>See more</summary>
	
	indexOfLastPathSeparator
	"
	'pp.txt' indexOfLastPathSeparator
	'/pp.txt' indexOfLastPathSeparator
	'a/pp.txt' indexOfLastPathSeparator
	'b/a/pp.txt' indexOfLastPathSeparator
	'/b/a/pp.txt' indexOfLastPathSeparator
	"
	^self findLast: [ :char | char isPathSeparator ]
</details>

#### String>>#article

<details>
	<summary>See more</summary>
	
	article
	| article first letter second |
	self isEmpty ifTrue: [^self].
	article := self first isVowel ifTrue: ['an'] ifFalse: ['a'].
	first := self first asLowercase.
	letter := self size = 1.
	second := letter ifFalse: [self second asLowercase].
	(first = $f and: [letter orNot: ['aeiloru' includes: second]])
		ifTrue: [^'an'].
	first = $u ifTrue: [
		(letter or: ['ck' includes: second]) ifTrue: [^'a'].
		second = $n
			ifTrue: [(self size = 2 or: [self third isVowel]) ifTrue: [^'a']]].
	(first = $e and: [second = $u]) ifTrue: [^'a'].
	^article
</details>

#### String>>#match: text

Answer whether text matches the pattern in this string. Matching ignores upper/lower case differences. Where this string contains #, text may contain any character. Where this string contains *, text may contain any sequence of characters.


<details>
	<summary>See more</summary>
	
	match: text
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."

	^ self startingAt: 1 match: text startingAt: 1
"
	'*'			match: 'zort' true
	'*baz'		match: 'mobaz' true
	'*baz'		match: 'mobazo' false
	'*baz*'		match: 'mobazo' true
	'*baz*'		match: 'mozo' false
	'foo*'		match: 'foozo' true
	'foo*'		match: 'bozo' false
	'foo*baz'	match: 'foo23baz' true
	'foo*baz'	match: 'foobaz' true
	'foo*baz'	match: 'foo23bazo' false
	'foo'		match: 'Foo' true
	'foo*baz*zort' match: 'foobazort' false
	'foo*baz*zort' match: 'foobazzort' false
	'*foo#zort'	match: 'afoo3zortthenfoo3zort' true
	'*foo*zort'	match: 'afoodezortorfoo3zort' true
"
</details>

#### String>>#isPathSeparator

<details>
	<summary>See more</summary>
	
	isPathSeparator
	self = '\' ifTrue: [^true].
	self = ':' ifTrue: [^true].
	self = '/' ifTrue: [^true].
	^false
</details>

#### String>>#asAbsolutePathName

See comment at #isAbsolutePathName


<details>
	<summary>See more</summary>
	
	asAbsolutePathName
	"See comment at #isAbsolutePathName"

	| slash |
	slash _ FileIOAccessor default slash.
	^ String streamContents: [ :childPath |
		childPath nextPutAll: slash.
		(FileIOAccessor default absolutePathComponentsFor: self)
			do: [ :each | childPath nextPutAll: each]
			separatedBy: [childPath nextPutAll: slash]]
</details>

#### String>>#withoutSuffix: aString

'Elvis' withoutSuffix: 'vis' 'Elvis' withoutSuffix: 'Paul'


<details>
	<summary>See more</summary>
	
	withoutSuffix: aString
	"
	'Elvis' withoutSuffix: 'vis'
	'Elvis' withoutSuffix: 'Paul'
	"
	^(self endsWith: aString)
		ifTrue: [ self copyFrom: 1 to: self size - aString size ]
</details>

#### String>>#is: aString substringAt: index

Answer whether the receiver includes aString as a subcollection at position index. The comparison is case-sensitive.


<details>
	<summary>See more</summary>
	
	is: aString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."
	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ aString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (aString basicAt: 1) ifFalse: [ ^false ].
	^(self findSubstring: aString in: self startingAt: index matchTable: CaseSensitiveOrder) = index
</details>

#### String>>#* arg

<details>
	<summary>See more</summary>
	
	* arg

	self shouldNotImplement.
"	^ arg adaptToString: self andSend: #*"
</details>

#### String>>#asLowercase

Answer a String made up from the receiver whose characters are all lowercase.


<details>
	<summary>See more</summary>
	
	asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asString translateToLowercase
</details>

#### String>>#asNumber

Answer the Number created by interpreting the receiver as the string representation of a number.


<details>
	<summary>See more</summary>
	
	asNumber 
	"Answer the Number created by interpreting the receiver as the string 
	representation of a number."
	^Number readFrom: self readStream
</details>

#### String>>#pluralize: aBoolean

<details>
	<summary>See more</summary>
	
	pluralize: aBoolean
	^aBoolean ifTrue: [self asPlural] ifFalse: [self]
</details>

#### String>>#findString: key startingAt: start caseSensitive: caseSensitive

Answer the index in this String at which the substring key first occurs, at or beyond start. The match can be case-sensitive or not. If no match is found, zero will be returned.


<details>
	<summary>See more</summary>
	
	findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive
	ifTrue: [^ self findSubstring: key in: self startingAt: start matchTable: CaseSensitiveOrder]
	ifFalse: [^ self findSubstring: key in: self startingAt: start matchTable: CaseInsensitiveOrder]
</details>

#### String>>#// arg

<details>
	<summary>See more</summary>
	
	// arg

	self shouldNotImplement.
"	^ arg adaptToString: self andSend: #//"
</details>

#### String>>#withoutTrailingBlanks

Return a copy of the receiver from which trailing blanks have been trimmed.


<details>
	<summary>See more</summary>
	
	withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self lastNoBlankIndex.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	" ' abc  d   ' withoutTrailingBlanks"

</details>

#### String>>#lineNumber: anIndex

Answer a string containing the characters in the given line number.


<details>
	<summary>See more</summary>
	
	lineNumber: anIndex
	"Answer a string containing the characters in the given line number. "
	| i |
	i _ 1.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i = anIndex ifTrue: [
			^ self copyFrom: start to: endWithoutDelimiters ].
		i _ i + 1 ].
	^nil
	
"
'Fred
the
Bear' lineNumber: 3
".
</details>

#### String>>#indexOf: aCharacter  startingAt: start  ifAbsent: aBlock

Answer the index of anElement within the receiver. If the receiver does not contain anElement, answer the result of evaluating the argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	(aCharacter class == Character) ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter numericValue inString: self  startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]
</details>

#### String>>#hash

#hash is implemented, because #= is implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is implemented, because #= is implemented"
	self size > 256 ifTrue: [ ^ self hashQuick ].
	^ByteArray
		hashBytes: self
		startingWith: self species hash
</details>

#### String>>#isUninflictedNoun

<details>
	<summary>See more</summary>
	
	isUninflictedNoun
	| nouns |
	nouns := #(
		'bison' 'bream' 'breeches' 'britches'
		'carp' 'chassis' 'clippers' 'cod' 'contretemps' 'corps'
		'debris' 'diabetes' 'djinn'
		'eland' 'elk'
		'flounder'
		'gallows' 'graffiti'
		'headquarters' 'herpes' 'high-jinks' 'homework'
		'innings'
		'jackanapes'
		'mackerel' 'measles' 'mews' 'mumps'
		'news'
		'pincers' 'pliers' 'proceedings'
		'rabies'
		'salmon' 'scissors' 'sea-bass' 'series' 'shears' 'species' 'swine'
		'trout' 'tuna'
		'whiting' 'wildebeest').
	^nouns includes: self
</details>

#### String>>#asUnaccented

'Hello' asUnaccented 'αιξτσό' asUnaccented


<details>
	<summary>See more</summary>
	
	asUnaccented
	"
	'Hello' asUnaccented
	'αιξτσό' asUnaccented
	"
	^(self anySatisfy: [ :c | c isAccented])
		ifFalse: [ self ]
		ifTrue: [ self collect: [ :c | c asUnaccented ]]
</details>

#### String>>#< aString

Answer whether the receiver sorts before aString. The collation order is case sensitive.


<details>
	<summary>See more</summary>
	
	< aString 
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 1
</details>

#### String>>#includesSubstring: aString caseSensitive: caseSensitive

<details>
	<summary>See more</summary>
	
	includesSubstring: aString caseSensitive: caseSensitive
	
	^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0
</details>

#### String>>#at: index

Primitive. Answer the Character stored in the field of the receiver indexed by the argument. Fail if the index argument is not an Integer or is out of bounds. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character numericValue: (super at: index)
</details>

#### String>>#print

<details>
	<summary>See more</summary>
	
	print
	Transcript show: self; newLine
</details>

#### String>>#base64Decoded

Decode the receiver from base 64


<details>
	<summary>See more</summary>
	
	base64Decoded
	"Decode the receiver from base 64"
	"
	'SGVsbG8gV29ybGQ=' base64Decoded
	'SGVsbG8gV29ybGQ=' base64Decoded asString
	"
	^(Base64MimeConverter mimeDecode: self)
</details>

#### String>>#endsWith: suffix

Answer whether the tail end of the receiver is the same as suffix. The comparison is case-sensitive.


<details>
	<summary>See more</summary>
	
	endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substringAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
"
</details>

#### String>>#newFromAfterAppending: aNewString into: replacementStream keepingFrom: from skipping: aRange

<details>
	<summary>See more</summary>
	
	newFromAfterAppending: aNewString into: replacementStream keepingFrom: from skipping: aRange

	replacementStream
		nextPutAll: (self copyFrom: from to: aRange first - 1);
		nextPutAll: aNewString.

	^ aRange last + 1
</details>

#### String>>#edit

<details>
	<summary>See more</summary>
	
	edit

	self editLabel: 'Text Editor'
</details>

#### String>>#withNewLines

Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf).


<details>
	<summary>See more</summary>
	
	withNewLines
	"Return a copy of the receiver in which backslash (\) characters have been replaced with newLine (i.e. Lf)."

	^ self collect: [ :c | c = $\ ifTrue: [ Character newLineCharacter ] ifFalse: [ c ]].
</details>

#### String>>#asValidInstanceVariableName

<details>
	<summary>See more</summary>
	
	asValidInstanceVariableName
	| answer |
	answer _ self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer _ answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
"
</details>

#### String>>#asValidSelector

<details>
	<summary>See more</summary>
	
	asValidSelector
	^ self asIdentifier: false

"
'234znak 43 ) 2' asValidSelector
"
</details>

#### String>>#withLineEndings: lineEndingString

assume the string is textual, and that CR, LF, and CRLF are all valid line endings. Answer a new String, where each occurence with is replaced with lineEndingString


<details>
	<summary>See more</summary>
	
	withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr _ Character cr.
	lf _ Character lf.
	crlf _ CharacterSet new.
	crlf add: cr; add: lf.

	inPos _ 1.
	outPos _ 1.
	lineEndingSize _ lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString _ String new: self size * lineEndingSize.

	[
		lineEndPos _ self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos _ outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: self startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos _ newOutPos + 1 ].
			outPos _ newOutPos.
			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos _ lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos _ lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos _ outPos + (self size - inPos).
	outString replaceFrom: outPos to: newOutPos with: self startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos
</details>

#### String>>#uncapitalized

Answer an object like the receiver but with first character downshifted if necesary


<details>
	<summary>See more</summary>
	
	uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary"
	"'MElViN' uncapitalized"
	"#Will uncapitalized"
	| answer |
	self isEmpty ifTrue: [^ self].
	answer _ self isString
				ifTrue: ["don't modify receiver"
					self copy]
				ifFalse: [self asString].
	answer at: 1 put: (answer at: 1) asLowercase.
	^ self isString
		ifTrue: [answer]
		ifFalse: [answer as: self class]
</details>

#### String>>#copyReplacing: rangesToNewStrings

<details>
	<summary>See more</summary>
	
	copyReplacing: rangesToNewStrings

	^self class streamContents: [ :replacementStream | self copyReplacing: rangesToNewStrings into: replacementStream ].
</details>

#### String>>#withoutPrefix: aString

'Elvis' withoutPrefix: 'El' 'Elvis' withoutPrefix: 'Paul'


<details>
	<summary>See more</summary>
	
	withoutPrefix: aString
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]
</details>

#### String>>#asFileName

Answer a String made up from the receiver that is an acceptable file name.


<details>
	<summary>See more</summary>
	
	asFileName
	"Answer a String made up from the receiver that is an acceptable file 
	name."

	^FileIOAccessor default checkName: self fixErrors: true
</details>

#### String>>#isRelativeMark

<details>
	<summary>See more</summary>
	
	isRelativeMark
	
	self = '.' ifTrue: [^true].
	self = '..' ifTrue: [^true].

	^false
</details>

#### String>>#appendToString: aString

<details>
	<summary>See more</summary>
	
	appendToString: aString

	| stringSize |
	stringSize := aString size.
	^ aString copyReplaceFrom: stringSize + 1
		 	    to: stringSize
		   	    with: self
</details>

#### String>>#findString: subString

Answer the index of subString within the receiver, starting at start. If the receiver does not contain subString, answer 0.


<details>
	<summary>See more</summary>
	
	findString: subString
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."
	^self findString: subString startingAt: 1.
</details>

#### String>>#squeezedTo: n

Examples: Do nothing: 'This one is a rather long phrase' squeezedTo: 32 1-remove blanks (result can be shorter than asked): 'This one is a rather long phrase' squeezedTo: 30 2-remove necessary trailing vowels 'This one is a rather long phrase' squeezedTo: 24 3-truncate as needed (and add ellipsis) 'This one is a rather long phrase' squeezedTo: 15 4-avoid ellipsis 'This one is a rather long phrase' squeezedTo: 5


<details>
	<summary>See more</summary>
	
	squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str := self.
	desiredSize := n.
	str size <= n ifTrue: [^str].
	str := str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix := ''.
	desiredSize := n - postFix size.
	vowelCount := str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels := vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels := 0.
			postFix := '...'.
			n - postFix size < 5 ifTrue: [postFix := ''].
			desiredSize := n - postFix size].

	read := str readStream.
	write := String new writeStream.
	i := 0.
	j := 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char := read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j := j + 1]].
	str := write contents , postFix.
	^ str
</details>

#### String>>#asUppercase

Answer a String made up from the receiver whose characters are all uppercase.


<details>
	<summary>See more</summary>
	
	asUppercase
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^self copy asString translateToUppercase
</details>

#### String>>#byteAt: index

<details>
	<summary>See more</summary>
	
	byteAt: index
	<primitive: 60>
	^(self at: index) numericValue
</details>

#### String>>#asSmalltalkComment

return this string, munged so that it can be treated as a comment in Smalltalk code. Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled


<details>
	<summary>See more</summary>
	
	asSmalltalkComment
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^String streamContents:  [ :str |
		| quoteCount first |

		str nextPut: $".
	
		quoteCount := 0.
		first := true.
		self do: [ :char |
			char = $"
				ifTrue: [
					first ifFalse: [
						str nextPut: char.
						quoteCount := quoteCount + 1 ] ]
				ifFalse: [
					quoteCount odd ifTrue: [
						"add a quote to even the number of quotes in a row"
						str nextPut: $" ].
					quoteCount := 0.
					str nextPut: char ].
			first := false ]. 

		quoteCount odd ifTrue: [
			"check at the end"
			str nextPut: $". ].

		str nextPut: $".
	].
	
</details>

#### String>>#asInteger

Answer the Integer created by interpreting the receiver as the string representation of an integer. Answer nil if no digits, else find the first digit and then all consecutive digits after that


<details>
	<summary>See more</summary>
	
	asInteger 
	"Answer the Integer created by interpreting the receiver as the string representation of an integer.  Answer nil if no digits, else find the first digit and then all consecutive digits after that"

	| startPosition tail endPosition |
	startPosition _ self findFirst: [:ch | ch isDigit].
	startPosition = 0 ifTrue: [^ nil].
	tail _ self copyFrom: startPosition to: self size.
	endPosition _ tail findFirst: [:ch | ch isDigit not].
	endPosition = 0 ifTrue: [endPosition _ tail size + 1].
	^ Number readFrom: (tail copyFrom: 1 to: endPosition - 1) readStream

"
'1796exportFixes-tkMX' asInteger
'1848recentLogFile-sw'  asInteger
'donald' asInteger
'abc234def567' asInteger
"
</details>

#### String>>#findTokens: delimiters

Answer the collection of tokens that result from parsing self. Return strings between the delimiters. Any character in the Collection delimiters marks a border. Several delimiters in a row are considered as just one separation. Also, allow delimiters to be a single character.


<details>
	<summary>See more</summary>
	
	findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens _ OrderedCollection new.
	separators _ delimiters class == Character 
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: separators startingAt: keyStop.
		keyStop _ self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
</details>

#### String>>#translateToUppercase

Translate all characters to lowercase, in place


<details>
	<summary>See more</summary>
	
	translateToUppercase
	"Translate all characters to lowercase, in place"

	self translateWith: UppercasingTable
</details>

#### String>>#copyReplacing: ranges with: newString into: replacementStream

<details>
	<summary>See more</summary>
	
	copyReplacing: ranges with: newString into: replacementStream

	| lastFrom |

	lastFrom := ranges
		inject: 1
		into: [ :from :aRange |
			self newFromAfterAppending: newString into: replacementStream keepingFrom: from skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).
</details>

#### String>>#green

Stuff like 'Hello world' green edit


<details>
	<summary>See more</summary>
	
	green
	"Stuff like
	'Hello world' green edit
	"
	^self asText green
</details>

#### String>>#findSubstring: key in: body startingAt: start matchTable: matchTable

Answer the index in the string body at which the substring key first occurs, at or beyond start. The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches. If no match is found, zero will be returned. The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter.


<details>
	<summary>See more</summary>
	
	findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	self var: #key declareC: 'unsigned char *key'.
	self var: #body declareC: 'unsigned char *body'.
	self var: #matchTable declareC: 'unsigned char *matchTable'.

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body at: startIndex+index-1) numericValue + 1)
				= (matchTable at: (key at: index) numericValue + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"
</details>

#### String>>#capitalized

Return a copy with the first letter capitalized


<details>
	<summary>See more</summary>
	
	capitalized
	"Return a copy with the first letter capitalized"
	| cap |
	self isEmpty ifTrue: [ ^self copy ].
	cap _ self copy.
	cap at: 1 put: (cap at: 1) asUppercase.
	^ cap
</details>

#### String>>#printOn: aStream

Print inside string quotes, doubling inbedded quotes.


<details>
	<summary>See more</summary>
	
	printOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."

	self storeOn: aStream
</details>

#### String>>#truncateWithElipsisTo: maxLength

Return myself or a copy suitably shortened but with elipsis added


<details>
	<summary>See more</summary>
	
	truncateWithElipsisTo: maxLength
	"Return myself or a copy suitably shortened but with elipsis added"

	^ self size <= maxLength
		ifTrue:
			[self]
		ifFalse:
			[(self copyFrom: 1 to: (maxLength - 3)), '...']


	"'truncateWithElipsisTo:' truncateWithElipsisTo: 20"
</details>

#### String>>#asDate

Many allowed forms, see Date>>#readFrom: '2014/6/30' asDate. '70/12/30' asDate. '12/30/70' asDate. '30/12/70' asDate. '4/5/6' asDate. '15 April 1982' asDate.


<details>
	<summary>See more</summary>
	
	asDate
	"Many allowed forms, see Date>>#readFrom:
		'2014/6/30' asDate.
		'70/12/30' asDate.
		'12/30/70' asDate.
		'30/12/70' asDate.
		'4/5/6' asDate.
		'15 April 1982' asDate.
	"

	^ Date fromString: self
</details>

#### String>>#beginsWithWindowsDriveName

Answer doesn't depend on running on Windows or not


<details>
	<summary>See more</summary>
	
	beginsWithWindowsDriveName
	"Answer doesn't depend on running on Windows or not"
	self size < 2 ifTrue: [ ^false ].
	^self first isDriveLetter
		and: [ self second isDriveSeparator
			and: [ self size = 2 or: [ self third isPathSeparator ]]]
</details>

#### String>>#isDriveName

<details>
	<summary>See more</summary>
	
	isDriveName
	FileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	FileIOAccessor default onMacClassic ifTrue: [
		^'/' asDirectoryEntry directoryNames includes: self].

	^false
</details>

#### String>>#yellow

Stuff like 'Hello world' yellow edit


<details>
	<summary>See more</summary>
	
	yellow
	"Stuff like
	'Hello world' yellow edit
	"
	^self asText yellow
</details>

#### String>>#indexOf: aCharacter  startingAt: start

Answer the index of the first occurence of anElement after start within the receiver. If the receiver does not contain anElement, answer 0.


<details>
	<summary>See more</summary>
	
	indexOf: aCharacter  startingAt: start

	(aCharacter class == Character) ifFalse: [^ 0].
	^ String indexOfByte: aCharacter numericValue inString: self startingAt: start
</details>

#### String>>#translateWith: table

translate the characters in the string by the given table, in place


<details>
	<summary>See more</summary>
	
	translateWith: table
	"translate the characters in the string by the given table, in place"
	^self translateFrom: 1 to: self size table: table
</details>

#### String>>#translateToLowercase

Translate all characters to lowercase, in place


<details>
	<summary>See more</summary>
	
	translateToLowercase
	"Translate all characters to lowercase, in place"

	self translateWith: LowercasingTable
</details>

#### String>>#asCamelCase

Answer a new String, without any whitespace, and with words capitalized (Except for the first one) ' how do you do? ' asCamelCase


<details>
	<summary>See more</summary>
	
	asCamelCase
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	"
	^ String streamContents: [ :outStream | | inStream capitalize wroteSome |
		wroteSome _ false.
		capitalize _ false.
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [ | c |
			c _ inStream next.
			c isSeparator
				ifTrue: [ capitalize _ true ]
				ifFalse: [
					capitalize & wroteSome ifTrue: [ c _ c asUppercase ].
					outStream nextPut: c.
					wroteSome _ true.
					capitalize _ false ]]]
</details>

#### String>>#findAnySubStr: delimiters startingAt: start

Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters. delimiters is an Array of Strings (Characters are permitted also). If the receiver does not contain any of the delimiters, answer size + 1.


<details>
	<summary>See more</summary>
	
	findAnySubStr: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min ind |
	min _ self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		delim class == Character 
			ifTrue: [ind _ self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [ind _ self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
			min _ min min: ind].
	^ min
</details>

#### String>>#asIdentifier: shouldBeCapitalized

Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case. This will always return a legal identifier, even for an empty string


<details>
	<summary>See more</summary>
	
	asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	(aString size = 0 or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString _ 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"
</details>

#### String>>#- arg

<details>
	<summary>See more</summary>
	
	- arg

	self shouldNotImplement.
"	^ arg adaptToString: self andSend: #-"
</details>

#### String>>#struck

<details>
	<summary>See more</summary>
	
	struck
	^self asText struck
</details>

#### String>>#contractTo: smallSize

return myself or a copy shortened by ellipsis to smallSize


<details>
	<summary>See more</summary>
	
	contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize _ smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
"
</details>

#### String>>#findBetweenSubStrs: delimiters

Answer the collection of String tokens that result from parsing self. Tokens are separated by 'delimiters', which can be a collection of Strings, or a collection of Characters. Several delimiters in a row are considered as just one separation.


<details>
	<summary>See more</summary>
	
	findBetweenSubStrs: delimiters
	"Answer the collection of String tokens that result from parsing self.  Tokens are separated by 'delimiters', which can be a collection of Strings, or a collection of Characters.  Several delimiters in a row are considered as just one separation."

	| tokens keyStart keyStop |
	tokens _ OrderedCollection new.
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipAnySubStr: delimiters startingAt: keyStop.
		keyStop _ self findAnySubStr: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
</details>

#### String>>#red

Stuff like 'Hello world' red edit


<details>
	<summary>See more</summary>
	
	red
	"Stuff like
	'Hello world' red edit
	"
	^self asText red
</details>

#### String>>#leftFlush

Stuff like ('Hello world' leftFlush ) edit


<details>
	<summary>See more</summary>
	
	leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	^self asText leftFlush
</details>

#### String>>#asYear

'2008' asYear. '2008' asYear start.


<details>
	<summary>See more</summary>
	
	asYear
	"
		'2008' asYear.
		'2008' asYear start.
	"

	^ Year fromString: self
</details>

#### String>>#withCuisLineEndings

assume the string is textual, and that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf ('aLine', String crlfString, 'anotherOne') withCuisLineEndings


<details>
	<summary>See more</summary>
	
	withCuisLineEndings
	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Replace each occurence with a single Lf
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	"

	^ self withLineEndings: String newLineString
</details>

#### String>>#indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock

returns the index of the first character in the given set, starting from start


<details>
	<summary>See more</summary>
	
	indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start"

	| answer |
	answer _ String findFirstInString: self inSet: aCharacterSet byteArrayMap startingAt: start.

	^answer = 0 
		ifTrue: [ aBlock value ]
		ifFalse: [ answer]
</details>

#### String>>#pointSize: pointSize

Stuff like ('Hello World' pointSize: 22) edit


<details>
	<summary>See more</summary>
	
	pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	^self asText pointSize: pointSize
</details>

#### String>>#linesDo: aBlock

execute aBlock with each line in this string. The terminating CR's are not included in what is passed to aBlock


<details>
	<summary>See more</summary>
	
	linesDo: aBlock
	"execute aBlock with each line in this string.  The terminating CR's are not included in what is passed to aBlock"
	
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]
		
</details>

#### String>>#surroundedBySingleQuotes

Answer the receiver with leading and trailing quotes.


<details>
	<summary>See more</summary>
	
	surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ $' asString, self, $' asString
</details>

#### String>>#lineCount

Answer the number of lines represented by the receiver, where every cr adds one line. 5/10/96 sw


<details>
	<summary>See more</summary>
	
	lineCount
	"Answer the number of lines represented by the receiver, where every cr adds one line.  5/10/96 sw"


	| i |
	i _ 0.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i _ i + 1 ].
	^i

"
'Fred
the
Bear' lineCount
"
</details>

#### String>>#withArticle

<details>
	<summary>See more</summary>
	
	withArticle
	^self article , ' ' , self
</details>

#### String>>#correctAgainst: wordList continuedFrom: oldCollection

Like correctAgainst:. Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList.


<details>
	<summary>See more</summary>
	
	correctAgainst: wordList continuedFrom: oldCollection
	"Like correctAgainst:.  Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList."

	^ wordList
		ifNil: [
			self correctAgainstEnumerator: nil continuedFrom: oldCollection ]
		ifNotNil: [
			self
				correctAgainstEnumerator: [ :action | wordList do: action without: nil]
				continuedFrom: oldCollection ]
</details>

#### String>>#italic

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit
	"
	^self asText italic
</details>

#### String>>#asUtf8: convertEmbeddedNCRs

Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15) Answer a ByteArray. If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints. See http://en.wikipedia.org/wiki/Numeric_character_reference Note: The conversion of NCRs is reversible. See #fromUtf8:hex:trimLastNull: This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs. The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it. Copy the following and paste in your external text editor &#913;&#945; Alpha &#914;&#946; Beta &#915;&#947; Gamma &#916;&#948; Delta &#917;&#949; Epsilon &#918;&#950; Zeta &#919;&#951; Eta &#920;&#952; Theta &#921;&#953; Iota &#922;&#954; Kappa &#923;&#955; Lambda &#924;&#956; Mu &#925;&#957; Nu &#926;&#958; Xi &#927;&#959; Omicron &#928;&#960; Pi &#929;&#961; Rho &#931;&#963;&#962; Sigma &#932;&#964; Tau &#933;&#965; Upsilon &#934;&#966; Phi &#935;&#967; Chi &#936;&#968; Psi &#937;&#969; Omega &# 937;&# 969; Not a NCR, just regular ASCII chars!


<details>
	<summary>See more</summary>
	
	asUtf8: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8:hex:trimLastNull:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it.

Copy the following and paste in your external text editor
&#913;&#945; 	Alpha 	
&#914;&#946; 	Beta 	
&#915;&#947; 	Gamma 	
&#916;&#948; 	Delta 	
&#917;&#949; 	Epsilon 	
&#918;&#950; 	Zeta 	
&#919;&#951; 	Eta 	
&#920;&#952; 	Theta 	
&#921;&#953; 	Iota 	
&#922;&#954; 	Kappa 	
&#923;&#955; 	Lambda 	
&#924;&#956; 	Mu 	
&#925;&#957; 	Nu
&#926;&#958; 	Xi
&#927;&#959; 	Omicron
&#928;&#960; 	Pi
&#929;&#961; 	Rho
&#931;&#963;&#962; 	Sigma
&#932;&#964; 	Tau
&#933;&#965; 	Upsilon
&#934;&#966; 	Phi
&#935;&#967; 	Chi
&#936;&#968; 	Psi
&#937;&#969; 	Omega
&# 937;&# 969; 	Not a NCR, just regular ASCII chars!
	"

	| str |
	^ByteArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrEnd codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					ncrEnd _ 0.
					maybeUnicodeNCR _ inStream next: 9.
					(maybeUnicodeNCR notEmpty and: [ maybeUnicodeNCR first = $# ]) ifTrue: [
						ncrEnd _ maybeUnicodeNCR indexOf: $; ].
					ncrEnd = 0
						ifFalse: [
							maybeUnicodeNCR second = $x
								ifTrue: [
									str _ (maybeUnicodeNCR copyFrom: 3 to: ncrEnd) asUppercase.
									(str first isDigit or: [ str first asciiValue between: $A asciiValue and: $F asciiValue])
										ifTrue: [ codePoint _  ('16r', str) asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]
								ifFalse: [
									str _ maybeUnicodeNCR copyFrom: 2 to: ncrEnd.
									str first isDigit
										ifTrue: [ codePoint _  codePoint _ str asNumber ]
										ifFalse: [ ncrEnd _ 0 ]]].
					ncrEnd = 0
						ifTrue: [
							"Not an NCR after all. Just add the $& and continue from there"
							codePoint _ nextChar codePoint ].
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint.
					inStream position: prevPos + ncrEnd ]
				ifFalse: [
					codePoint _ nextChar codePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]
</details>

#### String>>#isAbsolutePathName

Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt' and even '/Users/Someone/file.txt' are considered an absolute pathName. This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice.


<details>
	<summary>See more</summary>
	
	isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	| upperName |
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	FileIOAccessor default onMacClassic ifTrue: [
		upperName := self asUppercase.
		^'/' asDirectoryEntry directoryNames anySatisfy: [ :each |
			(upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						(self at: nextPos) isPathSeparator ]]].

	^ false
</details>

#### String>>#correctAgainst: wordList

Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList. Depends on the scoring scheme of alike:


<details>
	<summary>See more</summary>
	
	correctAgainst: wordList
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results _ self correctAgainst: wordList continuedFrom: nil.
	results _ self correctAgainst: nil continuedFrom: results.
	^ results
</details>

#### String>>#blue

Stuff like 'Hello world' blue edit


<details>
	<summary>See more</summary>
	
	blue
	"Stuff like
	'Hello world' blue edit
	"
	^self asText blue
</details>

#### String>>#isString

Overridden to return true in String, natch


<details>
	<summary>See more</summary>
	
	isString
	^ true
</details>

#### String>>#correctAgainstEnumerator: wordBlock continuedFrom: oldCollection

The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it.


<details>
	<summary>See more</summary>
	
	correctAgainstEnumerator: wordBlock continuedFrom: oldCollection
	"The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it."

	| choices scoreMin results score maxChoices |
	scoreMin _ self size // 2 min: 3.
	maxChoices _ 10.
	choices _ oldCollection
		ifNil: [ SortedCollection sortBlock: [ :x :y | x value > y value ] ].
	wordBlock
		ifNil: [
			results _ OrderedCollection new.
			1 to: (maxChoices min: choices size) do: [ :i | results add: (choices at: i) key ] ]
		ifNotNil: [
			wordBlock value: [ :word |
				(score _ self alike: word) >= scoreMin ifTrue: [
					choices add: (Association key: word value: score).
						(choices size >= maxChoices) ifTrue: [ scoreMin _ (choices at: maxChoices) value] ] ].
			results _ choices ].
	^ results
</details>

#### String>>#cyan

Stuff like 'Hello world' cyan edit


<details>
	<summary>See more</summary>
	
	cyan
	"Stuff like
	'Hello world' cyan edit
	"
	^self asText cyan
</details>

#### String>>#<= aString

Answer whether the receiver sorts before or equal to aString. The collation order is case sensitive.


<details>
	<summary>See more</summary>
	
	<= aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	^ (self compare: self with: aString collated: CaseSensitiveOrder) <= 2
</details>

#### String>>#under

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit
	"
	^self asText under
</details>

#### String>>#sub

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit
	"
	^self asText sub
</details>

#### String>>#adaptToCollection: rcvr andSend: selector

If I am involved in arithmetic with a collection, convert me to a number.


<details>
	<summary>See more</summary>
	
	adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a collection, convert me to a number."

	^ rcvr perform: selector with: self asNumber
</details>

#### String>>#, aStringOrText

Concatenate two Strings or Collections.


<details>
	<summary>See more</summary>
	
	, aStringOrText
	^ aStringOrText appendToString: self
</details>

#### String>>#displayProgressAt: aPoint from: minVal to: maxVal during: workBlock

Display this string as a caption over a progress bar while workBlock is evaluated. EXAMPLE (Select next 6 lines and Do It) 'Now here''s some Real Progress' displayProgressAt: Sensor mousePoint from: 0 to: 100 during: [:barBlock | 1 to: 100 do: [:x | (Delay forMilliseconds: 10) wait. barBlock value: x.]]. ['Now here''s some Real Progress' displayProgressAt: Sensor mousePoint from: 0 to: 100 during: [:barBlock | 1 to: 100 do: [:x | barBlock value: x. (Delay forMilliseconds: 100) wait]].] fork ['Now here''s some Real Progress' displayProgressAt: Sensor mousePoint from: 0 to: 10 during: [:barBlock | 1 to: 30 do: [:x | barBlock value: x \\ 11. (Delay forMilliseconds: 100) wait]]] fork 'Now here''s some Real Progress' displayProgressAt: Sensor mousePoint from: 0 to: 10 during: [:barBlock | 1 to: 30 do: [:x | barBlock value: nil. (Delay forMilliseconds: 200) wait]]. HOW IT WORKS (Try this in any other language :-) Since your code (the last 2 lines in the above example) is in a block, this method gets control to display its heading before, and clean up the screen after, its execution. The key, though, is that the block is supplied with an argument, named 'bar' in the example, which will update the bar image every it is sent the message value: x, where x is in the from:to: range. The use of ProgressInitiationException allows for avoiding actual progress display, by catching the exception.


<details>
	<summary>See more</summary>
	
	displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].

['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
	1 to: 100 do: [:x | barBlock value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock
</details>

#### String>>#separateKeywords

<details>
	<summary>See more</summary>
	
	separateKeywords

	^self, ' '
</details>

#### String>>#beginsWithPathSeparator

<details>
	<summary>See more</summary>
	
	beginsWithPathSeparator
	self ifEmpty: [^false].
	^self first isPathSeparator
</details>

#### String>>#asDirectoryEntry

See examples in #asFileEntry method comment


<details>
	<summary>See more</summary>
	
	asDirectoryEntry
	"See examples in #asFileEntry method comment"
	^DirectoryEntry withPathName: self
</details>

#### String>>#firstNoBlankIndex

<details>
	<summary>See more</summary>
	
	firstNoBlankIndex 

	^self findFirst: [:aChar | aChar isSeparator not ]
</details>

#### String>>#encompassParagraph: anInterval

Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver. Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size). Look also for null characters. Never include null characters in the answer. See also #encompassLine:


<details>
	<summary>See more</summary>
	
	encompassParagraph: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| left rightCr rightNull |
	left _ (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	rightNull _ (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
</details>

#### String>>#bold

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit
	"
	^self asText bold
</details>

#### String>>#includesSubString: subString

<details>
	<summary>See more</summary>
	
	includesSubString: subString
	^ (self findString: subString startingAt: 1) > 0
</details>

#### String>>#is: aSymbol

Note: Senders might prefer #isString for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	"Note: Senders might prefer #isString for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#String = aSymbol or: [ super is: aSymbol]
</details>

#### String>>#byteSize

<details>
	<summary>See more</summary>
	
	byteSize
	^self size
</details>

#### String>>#indentationIfBlank: aBlock

Answer the number of leading tabs in the receiver. If there are no visible characters, pass the number of tabs to aBlock and return its value.


<details>
	<summary>See more</summary>
	
	indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab _ Character tab.
	reader _ ReadStream on: self.
	leadingTabs _ 0.
	[ reader atEnd not and: [ (ch _ reader next) == tab ]] whileTrue: [
		leadingTabs _ leadingTabs + 1 ].
	lastSeparator _ leadingTabs + 1.
	[ reader atEnd not and: [
		ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
			lastSeparator _ lastSeparator + 1.
			ch _ reader next ].
	lastSeparator = self size | (ch notNil and: [ch isLineSeparator]) ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs
</details>

#### String>>#adaptToNumber: rcvr andSend: selector

If I am involved in arithmetic with a number, convert me to a number.


<details>
	<summary>See more</summary>
	
	adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

"	^ rcvr perform: selector with: self asNumber"
	self shouldNotImplement.
</details>

#### String>>#startingAt: keyStart match: text startingAt: textStart

Answer whether text matches the pattern in this string. Matching ignores upper/lower case differences. Where this string contains #, text may contain any character. Where this string contains *, text may contain any sequence of characters.


<details>
	<summary>See more</summary>
	
	startingAt: keyStart match: text startingAt: textStart
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.

	"Check for any #'s"
	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].
	(self at: i) = $#] whileTrue:
		["# consumes one char of key and one char of text"
		j > text size ifTrue: [^ false "no more text"].
		i := i+1.  j := j+1].

	"Then check for *"
	(self at: i) = $*
		ifTrue: [i = self size ifTrue:
					[^ true "Terminal * matches all"].
				"* means next match string can occur anywhere"
				anyMatch := true.
				matchStart := i + 1]
		ifFalse: ["Otherwise match string must occur immediately"
				anyMatch := false.
				matchStart := i].

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '** not valid -- use * instead'].
		matchEnd := ii-1].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '*# not valid -- use #* instead'].
		matchEnd := matchEnd min: ii-1].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"
</details>

#### String>>#asUnHtml

Strip out all Html stuff (commands in angle brackets <>) and convert the characters &<> back to their real value. Leave actual cr and tab as they were in text.


<details>
	<summary>See more</summary>
	
	asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did inString |

	"Hack in some minimal workaround for Unicode stuff"
	inString _ self copyReplaceAll: 'β€™' with: $' asString.
	"Check if we can handle this in #safeValue: in some way..."
	inString = self ifFalse: [ self halt ].
	
	in _ ReadStream on: inString.
	out _ WriteStream on: (String new: self size).
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
						ifTrue: [rest _ in upTo: $;.
								did _ out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $΅].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest asInteger) ].
								did = out position ifTrue: [
									out nextPut: $&; nextPutAll: rest.
									"self error: 'unknown encoded HTML char'."
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents
</details>

#### String>>#\\ arg

<details>
	<summary>See more</summary>
	
	\\ arg

	self shouldNotImplement.
"	^ arg adaptToString: self andSend: #\\"
</details>

#### String>>#skipAnySubStr: delimiters startingAt: start

Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed). If the receiver is all delimiters, answer size + 1.


<details>
	<summary>See more</summary>
	
	skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| any this ind ii |
	ii _ start-1.
	[(ii _ ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any _ false.
		delimiters do: [:delim |
			delim class == Character 
				ifTrue: [(self at: ii) == delim ifTrue: [any _ true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: "Here's where the one-off error was."
						[ind _ 0.
						this _ true.
						delim do: [:dd | 
							dd == (self at: ii+ind) ifFalse: [this _ false].
							ind _ ind + 1].
						this ifTrue: [ii _ ii + delim size - 1.  any _ true]]
							ifTrue: [any _ false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1
</details>

#### String>>#indexOfFirstPathSeparator

<details>
	<summary>See more</summary>
	
	indexOfFirstPathSeparator
	self
		withIndexDo: [:each :idx | each isPathSeparator
				ifTrue: [^ idx]].
	^ 0
</details>

#### String>>#copyReplacing: rangesToNewStrings into: replacementStream

<details>
	<summary>See more</summary>
	
	copyReplacing: rangesToNewStrings into: replacementStream

	| lastFrom |

	lastFrom := rangesToNewStrings inject: 1 into: [ :from :aRangeToNewString |
		self
			newFromAfterAppending: aRangeToNewString value
			into: replacementStream
			keepingFrom: from
			skipping: aRangeToNewString key ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).

</details>

#### String>>#asFileEntry

Windows 'C:\Windows' asFileEntry exists false 'C:\Windows' asDirectoryEntry exists true '/' asFileEntry exists false '/' asDirectoryEntry exists false 'C:\' asFileEntry exists false 'C:\' asDirectoryEntry exists true ('C:' asDirectoryEntry // 'Windows') exists false ('C:' asDirectoryEntry / 'Windows') exists true Linux '/var' asFileEntry exists '/var' asDirectoryEntry exists true '/' asFileEntry exists false '/' asDirectoryEntry exists true '/media/cdrom' asFileEntry exists false '/media/cdrom' asDirectoryEntry exists true ('/bin' asDirectoryEntry / 'more') exists false ('/bin' asDirectoryEntry // 'more') exists true MacOsX '/var' asFileEntry exists false '/var' asDirectoryEntry exists true '/' asFileEntry exists false '/' asDirectoryEntry exists true '/Volumes/SanDisk32-NTFS' asFileEntry exists false '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true 'SanDisk32-NTFS' asFileEntry exists false 'SanDisk32-NTFS' asDirectoryEntry exists false


<details>
	<summary>See more</summary>
	
	asFileEntry
	"

Windows	
	'C:\Windows' asFileEntry exists false
	'C:\Windows' asDirectoryEntry exists true
	'/' asFileEntry exists false
	'/' asDirectoryEntry exists false
	'C:\' asFileEntry exists false
	'C:\' asDirectoryEntry exists true
	('C:' asDirectoryEntry // 'Windows') exists false
	('C:' asDirectoryEntry / 'Windows') exists true
	
Linux
    '/var' asFileEntry exists
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists true
    '/media/cdrom' asFileEntry exists false
    '/media/cdrom' asDirectoryEntry exists true
    ('/bin' asDirectoryEntry / 'more') exists false
    ('/bin' asDirectoryEntry // 'more') exists true

MacOsX
    '/var' asFileEntry exists false
    '/var' asDirectoryEntry exists true
    '/' asFileEntry exists false
    '/' asDirectoryEntry exists  true
    '/Volumes/SanDisk32-NTFS' asFileEntry exists false
    '/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
    'SanDisk32-NTFS' asFileEntry exists false
    'SanDisk32-NTFS' asDirectoryEntry exists false
	
	"
	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!' ].
	^FileEntry withPathName: self
</details>

#### String>>#beginsWith: prefix

Answer whether the receiver begins with the given prefix string. The comparison is case-sensitive.


<details>
	<summary>See more</summary>
	
	beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."

	^self is: prefix substringAt: 1
</details>

#### String>>#hasContentsInExplorer

<details>
	<summary>See more</summary>
	
	hasContentsInExplorer

	^false
</details>

#### String>>#indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock

Answer the index of the receiver's first element, such that that element equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at element start of the receiver. If no such match is found, answer the result of evaluating argument, exceptionBlock.


<details>
	<summary>See more</summary>
	
	indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	| index |
	index _ self findSubstring: sub in: self startingAt: start matchTable: CaseSensitiveOrder.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index
</details>

#### String>>#isAlphaNumeric

Answer true if the receiver contains only letters or digits.


<details>
	<summary>See more</summary>
	
	isAlphaNumeric
	"Answer true if the receiver contains only letters or digits."
	^ self allSatisfy: [:each| each isAlphaNumeric]
</details>

#### String>>#centered

Stuff like ('Hello world' centered ) edit


<details>
	<summary>See more</summary>
	
	centered
	"Stuff like
	('Hello world' centered ) edit
	"
	^self asText centered
</details>

#### String>>#padded: leftOrRight to: length with: char

<details>
	<summary>See more</summary>
	
	padded: leftOrRight to: length with: char
	leftOrRight = #left ifTrue:
		[^ (String new: (length - self size max: 0) withAll: char) , self].
	leftOrRight = #right ifTrue:
		[^ self , (String new: (length - self size max: 0) withAll: char)].
</details>

#### String>>#asUtf8

Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15) Answer a ByteArray. See #fromUtf8:


<details>
	<summary>See more</summary>
	
	asUtf8
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	See #fromUtf8: "

	^self asUtf8: false
</details>

#### String>>#asPathTokens

<details>
	<summary>See more</summary>
	
	asPathTokens

	^self beginsWithWindowsDriveName
		ifTrue: [
			(OrderedCollection with: (self copyFrom: 1 to: 2)), 
				((self copyFrom: 3 to: self size) findTokens: String pathSeparators) ]
		ifFalse: [
			self findTokens: String pathSeparators ]
</details>

#### String>>#lineIndicesDo: aBlock

execute aBlock with 3 arguments for each line: - start index of line - end index of line without line delimiter - end index of line including line delimiter(s) CR, LF or CRLF


<details>
	<summary>See more</summary>
	
	lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[
		end _ self indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters _ end _ self size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^self ].

		"Account for CrLf sequence"
		endWithoutDelimiters _ end - 1.
		(end < self size
			and: [(self at: end + 1) = Character lf
			and: [(self at: end) = Character cr ]])
				ifTrue: [ end _ end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ] repeat
</details>

#### String>>#isRemote

<details>
	<summary>See more</summary>
	
	isRemote
	^false
</details>

#### String>>#withoutWindowsDriveName

<details>
	<summary>See more</summary>
	
	withoutWindowsDriveName
	^self beginsWithWindowsDriveName 
		ifFalse: [ self ]
		ifTrue: [ self copyFrom: 3 to: self size ]
</details>

#### String>>#displayOn: aDisplayMedium at: aPoint textColor: aColor

Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color


<details>
	<summary>See more</summary>
	
	displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	^aDisplayMedium getCanvas drawString: self at: aPoint font: nil color: aColor
</details>

#### String>>#commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock

Return the size of the longest common subsequence with aString, only between startIndex and stopIndex. Apply aBlock to each character before comparing. Do a character-by-character comparison between the receiver and aString. Return the index of the final character that matched exactly.


<details>
	<summary>See more</summary>
	
	commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock
	"Return the size of the longest common subsequence with aString, only between startIndex and stopIndex.
	Apply aBlock to each character before comparing.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."
	| stopIndex |
	stopIndex _ stopIndexRequested min: aString size.
	startIndex
		to: stopIndex
		do: [ :i |
			(aBlock value: (self at: i)) = (aBlock value: (aString at: i)) ifFalse: [ ^ i - 1 ]].
	^ stopIndex
</details>

#### String>>#lastNoBlankIndex

<details>
	<summary>See more</summary>
	
	lastNoBlankIndex

	^ self findLast: [:aChar | aChar isSeparator not].
	
</details>

#### String>>#asMonth

Many allowed forms, see Month>>#readFrom: 'July 1998' asMonth. '1998/7'asMonth.


<details>
	<summary>See more</summary>
	
	asMonth
	"Many allowed forms, see Month>>#readFrom:
		'July 1998' asMonth.
		'1998/7'asMonth.
	"

	^ Month fromString: self
</details>

#### String>>#asString

Answer this string.


<details>
	<summary>See more</summary>
	
	asString
	"Answer this string."

	^ self

</details>

#### String>>#alike: aString

Answer some indication of how alike the receiver is to the argument, 0 is no match, twice aString size is best score. Case is ignored.


<details>
	<summary>See more</summary>
	
	alike: aString 
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize _ (j _ self size) min: (k _ aString size).
	bonus _ (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].
	i _ 1.
	[(i <= minSize) and: [((super at: i) bitAnd: 16rDF)  = ((aString at: i) numericValue bitAnd: 16rDF)]]
		whileTrue: [ i _ i + 1 ].
	[(j > 0) and: [(k > 0) and:
		[((super at: j) bitAnd: 16rDF) = ((aString at: k) numericValue bitAnd: 16rDF)]]]
			whileTrue: [ j _ j - 1.  k _ k - 1. ].
	^ i - 1 + self size - j + bonus. 
</details>

#### String>>#collect: aBlock

Refer to the comment in Collection|collect:.


<details>
	<summary>See more</summary>
	
	collect: aBlock 
	"Refer to the comment in Collection|collect:."
	| result value stillAString |
	result _ self species new: self size.
	stillAString _ true.
	1 to: self size do: [ :index |
		value _ aBlock value: (self at: index).
		(stillAString and: [ (value is: #Character) not]) ifTrue: [
			result _ result asArray.
			stillAString _ false ].
		result at: index put: value].
	^ result
</details>

#### String>>#compare: aString

Answer a comparison code telling how the receiver sorts relative to aString: 1 - before 2 - equal 3 - after. The collation sequence is ascii with case differences ignored. To get the effect of a <= b, but ignoring case, use (a compare: b) <= 2.


<details>
	<summary>See more</summary>
	
	compare: aString 
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	The collation sequence is ascii with case differences ignored.
	To get the effect of a <= b, but ignoring case, use (a compare: b) <= 2."

	^ self compare: self with: aString collated: CaseInsensitiveOrder
</details>

#### String>>#> aString

Answer whether the receiver sorts after aString. The collation order is case sensitive.


<details>
	<summary>See more</summary>
	
	> aString 
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 3
</details>

#### String>>#super

Stuff like ('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit


<details>
	<summary>See more</summary>
	
	super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  ' sub, '(1 / z)' ) edit
	"
	^self asText super
</details>

#### String>>#encompassLine: anInterval

Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver. Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size) See also encompassParagraph:


<details>
	<summary>See more</summary>
	
	encompassLine: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left _ (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	rightNull _ (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: (rightCr min: rightNull)
</details>

#### String>>#findTokens: delimiters keep: keepers

Answer the collection of tokens that result from parsing self. The tokens are seperated by delimiters, any of a string of characters. If a delimiter is also in keepers, make a token for it. (Very useful for carriage return. A sole return ends a line, but is also saved as a token so you can see where the line breaks were.)


<details>
	<summary>See more</summary>
	
	findTokens: delimiters keep: keepers
	"Answer the collection of tokens that result from parsing self.  The tokens are seperated by delimiters, any of a string of characters.  If a delimiter is also in keepers, make a token for it.  (Very useful for carriage return.  A sole return ends a line, but is also saved as a token so you can see where the line breaks were.)"

	| tokens keyStart keyStop |
	tokens _ OrderedCollection new.
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: delimiters startingAt: keyStop.
		keyStop to: keyStart-1 do: [:ii | 
			(keepers includes: (self at: ii)) ifTrue: [
				tokens add: (self copyFrom: ii to: ii)]].	"Make this keeper be a token"
		keyStop _ self findDelimiters: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
</details>

#### String>>#evaluateExpression: aString parameters: aCollection

private - evaluate the expression aString with aCollection as the parameters and answer the evaluation result as an string


<details>
	<summary>See more</summary>
	
	evaluateExpression: aString parameters: aCollection 
	"private - evaluate the expression aString with  
	aCollection as the parameters and answer the  
	evaluation result as an string"
	| index |
	index := ('0' , aString) asNumber.

	index isZero
		ifTrue: [^ '[invalid subscript: {1}]' format: {aString}].

	index > aCollection size
		ifTrue: [^ '[subscript is out of bounds: {1}]' format: {aString}].

	^ (aCollection at: index) asString
</details>

#### String>>#+ arg

<details>
	<summary>See more</summary>
	
	+ arg

	self shouldNotImplement.
"	^ arg adaptToString: self andSend: #+"
</details>

#### String>>#displayStringOrText

To be used in the UI


<details>
	<summary>See more</summary>
	
	displayStringOrText
	"To be used in the UI"
	"Answer this string."

	^ self

</details>

#### String>>#keywords

Answer an array of the keywords that compose the receiver.


<details>
	<summary>See more</summary>
	
	keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords isAllValidInIdentifiers |
	keywords _ Array streamContents: [ :kwds |
		kwd _ WriteStream on: (String new: 16).
		isAllValidInIdentifiers _ true.
		1
			to: self size
			do: [ :i |
				char _ self at: i.
				kwd nextPut: char.
				char = $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers _ true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers _ false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.
</details>

#### String>>#caseInsensitiveLessOrEqual: aString

Answer whether the receiver sorts before or equal to aString. The collation order is case insensitive.


<details>
	<summary>See more</summary>
	
	caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	^ (self compare: self with: aString collated: CaseInsensitiveOrder) <= 2
</details>

#### String>>#byteAt: index put: value

<details>
	<summary>See more</summary>
	
	byteAt: index put: value
	<primitive: 61>
	self at: index put: value asCharacter.
	^value
</details>

## Symbol

I represent Strings that are created uniquely. Thus, someString asSymbol == someString asSymbol.

### Methods
#### Symbol>>#separateKeywords

<details>
	<summary>See more</summary>
	
	separateKeywords

	^self isKeyword
		ifFalse: [ self, ' ' ]
		ifTrue: [
			String streamContents: [ :strm |
				self keywords
					do: [ :keyword | strm nextPutAll: keyword ]
					separatedBy: [ strm space; space ].
				strm space]]
</details>

#### Symbol>>#isPvtSelector

Answer whether the receiver is a private message selector, that is, begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash.


<details>
	<summary>See more</summary>
	
	isPvtSelector
	"Answer whether the receiver is a private message selector, that is,
	begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash."

	^ (self beginsWith: 'pvt') and: [self size >= 4 and: [(self at: 4) isUppercase]]
</details>

#### Symbol>>#at: anInteger put: anObject

You cannot modify the receiver.


<details>
	<summary>See more</summary>
	
	at: anInteger put: anObject 
	"You cannot modify the receiver."

	self errorNoModification
</details>

#### Symbol>>#isValidBinarySelector

<details>
	<summary>See more</summary>
	
	isValidBinarySelector

	^ self isInfix and: [ self allSatisfy: [ :character | character isValidInBinarySelectors ] ]
</details>

#### Symbol>>#errorNoModification

<details>
	<summary>See more</summary>
	
	errorNoModification

	self error: 'symbols can not be modified.'
</details>

#### Symbol>>#isValidKeywordSelector

<details>
	<summary>See more</summary>
	
	isValidKeywordSelector

	^ self isKeyword and: [ self keywords allSatisfy: [ :keywordString | keywordString allButLast asSymbol isValidSelector ] ]
</details>

#### Symbol>>#isKeyword

Answer whether the receiver is a message keyword.


<details>
	<summary>See more</summary>
	
	isKeyword
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3
</details>

#### Symbol>>#isLiteral

Answer whether the receiver is a valid Smalltalk literal.


<details>
	<summary>See more</summary>
	
	isLiteral
	"Answer whether the receiver is a valid Smalltalk literal."

	^ true
</details>

#### Symbol>>#= another

Use == between two symbols...


<details>
	<summary>See more</summary>
	
	= another

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another class == Symbol ifTrue: [
		^ false].  "Was not == "

	"Otherwise use string =..."
	^ super = another
</details>

#### Symbol>>#capitalized

Return a copy with the first letter capitalized


<details>
	<summary>See more</summary>
	
	capitalized
	^ self asString capitalized asSymbol
</details>

#### Symbol>>#isInfix

Answer whether the receiver is an infix message selector.


<details>
	<summary>See more</summary>
	
	isInfix
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2
</details>

#### Symbol>>#asSymbol

Refer to the comment in String|asSymbol.


<details>
	<summary>See more</summary>
	
	asSymbol 
	"Refer to the comment in String|asSymbol."
</details>

#### Symbol>>#string: aString

<details>
	<summary>See more</summary>
	
	string: aString

	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self  
</details>

#### Symbol>>#isValidSelector

<details>
	<summary>See more</summary>
	
	isValidSelector

	^ self isValidUnarySelector
		or: [ self isValidBinarySelector ]
		or: [ self isValidKeywordSelector ]
</details>

#### Symbol>>#print

<details>
	<summary>See more</summary>
	
	print
	Transcript show: self printString; newLine
</details>

#### Symbol>>#isSymbol

<details>
	<summary>See more</summary>
	
	isSymbol
	^ true 
</details>

#### Symbol>>#precedence

Answer the receiver's precedence, assuming it is a valid Smalltalk message selector or 0 otherwise. The numbers are 1 for unary, 2 for binary and 3 for keyword selectors.


<details>
	<summary>See more</summary>
	
	precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self size = 0 ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1
</details>

#### Symbol>>#storeOn: aStream

Print inside string quotes, doubling inbedded quotes.


<details>
	<summary>See more</summary>
	
	storeOn: aStream 

	aStream nextPut: $#.
	(Scanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]
</details>

#### Symbol>>#asString

Refer to the comment in String|asString.


<details>
	<summary>See more</summary>
	
	asString 
	"Refer to the comment in String|asString."

	| newString |
	newString _ String new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString
</details>

#### Symbol>>#numArgs: n

Answer a string that can be used as a selector with n arguments. TODO: need to be extended to support shrinking and for selectors like #+ Note: Unrelated, but remember that we inherit #numArgs


<details>
	<summary>See more</summary>
	
	numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| selector numArgs aStream offs |
	
	selector _ self.
	(numArgs _ selector numArgs) >= n ifTrue: [ ^self ].	
	aStream _ WriteStream on: (String new: 16).
	aStream nextPutAll: self.
	
	(numArgs = 0) ifTrue: [ aStream nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
	2 to: n - numArgs + offs do: [ :i | aStream nextPutAll: 'with:' ].	
	^aStream contents asSymbol
</details>

#### Symbol>>#isValidUnarySelector

<details>
	<summary>See more</summary>
	
	isValidUnarySelector

	^ self isUnary and: [ self allSatisfy: [ :character | character isValidInIdentifiers ] ]
</details>

#### Symbol>>#flushCache

Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one. This primitive must be called whenever a method is defined or removed. NOTE: Only one of the two selective flush methods needs to be used. Squeak 2.3 and later uses 116 (See CompiledMethod flushCache).


<details>
	<summary>See more</summary>
	
	flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>

</details>

#### Symbol>>#isUnary

Answer whether the receiver is an unary message selector.


<details>
	<summary>See more</summary>
	
	isUnary
	"Answer whether the receiver is an unary message selector."

	^ self precedence = 1
</details>

#### Symbol>>#isInitializePvtSelector

Answer whether the receiver is a private instance initialization message selector, that is, begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)


<details>
	<summary>See more</summary>
	
	isInitializePvtSelector
	"Answer whether the receiver is a private instance initialization message selector, that is,
	begins with 'initializePvt' (followed or not by additional stuff, as a unary message, or as keyword with arguments)"

	^ self beginsWith: 'initializePvt'
</details>

#### Symbol>>#species

Answer the preferred class for reconstructing the receiver. For example, collections create new collections whenever enumeration messages such as collect: or select: are invoked. The new kind of collection is determined by the species of the original collection. Species and class are not always the same. For example, the species of Interval is Array.


<details>
	<summary>See more</summary>
	
	species

	^String
</details>

#### Symbol>>#shallowCopy

Answer with the receiver, because Symbols are unique.


<details>
	<summary>See more</summary>
	
	shallowCopy
	"Answer with the receiver, because Symbols are unique."
</details>

#### Symbol>>#replaceFrom: start to: stop with: replacement startingAt: repStart

Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	replaceFrom: start to: stop with: replacement startingAt: repStart

	self errorNoModification
</details>

