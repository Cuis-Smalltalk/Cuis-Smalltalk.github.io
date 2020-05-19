## ShoutTextBackgroundColor

Just for code styler (Shout)

### Methods
#### ShoutTextBackgroundColor>>#isForShout

True if to be removed from code before styling


<details>
	<summary>See more</summary>
	
	isForShout
	"True if to be removed from code before styling"
	^true
</details>

## ShoutTextColor

Just for code styler (Shout)

### Methods
#### ShoutTextColor>>#isForShout

True if to be removed from code before styling


<details>
	<summary>See more</summary>
	
	isForShout
	"True if to be removed from code before styling"
	^true
</details>

## ShoutTextEmphasis

Just for code styler (Shout)

### Methods
#### ShoutTextEmphasis>>#isForShout

True if to be removed from code before styling


<details>
	<summary>See more</summary>
	
	isForShout
	"True if to be removed from code before styling"
	^true
</details>

## TextAction

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TextAction>>#validate: aString

any format is OK with me


<details>
	<summary>See more</summary>
	
	validate: aString
	"any format is OK with me"
	^ aString
</details>

#### TextAction>>#isForFormatting

Answer false for those attributes that are not for text formatting. This is, those that are not considered in #withFormattingAttributeValues:do:


<details>
	<summary>See more</summary>
	
	isForFormatting
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^false
</details>

#### TextAction>>#forTextActionInfoDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextActionInfoDo: aBlock
	aBlock value: self info
</details>

#### TextAction>>#dominatedByNormalEmphasis

TextEmphasis normal (i.e. cmd-0) should turn off active text


<details>
	<summary>See more</summary>
	
	dominatedByNormalEmphasis
	"TextEmphasis normal (i.e. cmd-0) should turn off active text"
	^ true
</details>

#### TextAction>>#analyze: aString

Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection). Does not return self!. May be of the form: 3+4 <3+4> Click Here<3+4> <3+4>Click Here


<details>
	<summary>See more</summary>
	
	analyze: aString
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 _ aString indexOf: $<.
	b2 _ aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param _ self validate: aString.
		^ Array with: param with: (param size = 0 ifTrue: [nil] ifFalse: [param])].
	"Two parts"
	trim _ aString withBlanksTrimmed.
	(trim at: 1) == $< 
		ifTrue: [(trim last) == $>
			ifTrue: ["only instructions" 
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [(trim last) == $>
			ifTrue: ["at the end"
				param _ self validate: (aString copyFrom: b1+1 to: b2-1).
				show _ param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show _ nil]].
	^ Array with: param with: show

</details>

#### TextAction>>#mayActOnClick

Subclasses may override to provide, eg, hot-spot actions


<details>
	<summary>See more</summary>
	
	mayActOnClick

	^ true
</details>

#### TextAction>>#info

<details>
	<summary>See more</summary>
	
	info
	^ 'no hidden info'
</details>

## TextAlignment

Warning: TextAlignment and ParagraphStyleReference should always be applied to whole 'paragraphs' in the text. See #isParagraphAttribute ( (Text string: 'This text has no tyle set', String crString), (Text string: 'This is centered', String crString attribute: TextAlignment centered), (Text string: 'This text has no tyle set', String crString) ) edit

### Methods
#### TextAlignment>>#hash

#hash is re-implemented because #= is re-implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is re-implemented because #= is re-implemented"
	^ alignment hash
</details>

#### TextAlignment>>#alignment: aNumber

<details>
	<summary>See more</summary>
	
	alignment: aNumber
	alignment _ aNumber.
</details>

#### TextAlignment>>#dominates: other

There can be only one...


<details>
	<summary>See more</summary>
	
	dominates: other
	"There can be only one..."
	^self class == other class
</details>

#### TextAlignment>>#alignment

<details>
	<summary>See more</summary>
	
	alignment
	^alignment
</details>

#### TextAlignment>>#isParagraphAttribute

Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)


<details>
	<summary>See more</summary>
	
	isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^true
</details>

#### TextAlignment>>#forTextAlignmentDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextAlignmentDo: aBlock
	aBlock value: alignment
</details>

#### TextAlignment>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other alignment = alignment]
</details>

## TextAttribute

Tells a piece of text to be a certain way.

### Methods
#### TextAttribute>>#forTextColorDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextColorDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#forFontFamilyAndSizeDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forFontFamilyAndSizeDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#isParagraphAttribute

Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)


<details>
	<summary>See more</summary>
	
	isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^false
</details>

#### TextAttribute>>#actOnClickFor: model in: aTextComposition at: clickPoint

<details>
	<summary>See more</summary>
	
	actOnClickFor: model in: aTextComposition at: clickPoint
	^self actOnClickFor: model in: aTextComposition
</details>

#### TextAttribute>>#forTextAlignmentDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextAlignmentDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#canBeJoinedWith: aTextAttribute

To be used for RunArray compaction


<details>
	<summary>See more</summary>
	
	canBeJoinedWith: aTextAttribute
	"To be used for RunArray compaction"
	^self = aTextAttribute
</details>

#### TextAttribute>>#isSet

Respond true to include this attribute in the text


<details>
	<summary>See more</summary>
	
	isSet
	"Respond true to include this attribute in the text"
	^ true
</details>

#### TextAttribute>>#isForShout

True if to be removed from code before styling


<details>
	<summary>See more</summary>
	
	isForShout
	"True if to be removed from code before styling"
	^false
</details>

#### TextAttribute>>#isFont

<details>
	<summary>See more</summary>
	
	isFont
	^false
</details>

#### TextAttribute>>#actOnClickFor: model in: aTextComposition at: clickPoint editor: editor

<details>
	<summary>See more</summary>
	
	actOnClickFor: model in: aTextComposition at: clickPoint editor: editor
	^self actOnClickFor: model in: aTextComposition at: clickPoint
</details>

#### TextAttribute>>#mayActOnClick

Subclasses may override to provide, eg, hot-spot actions


<details>
	<summary>See more</summary>
	
	mayActOnClick
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false
</details>

#### TextAttribute>>#anchoredFormOrMorph

If one hides here, return it


<details>
	<summary>See more</summary>
	
	anchoredFormOrMorph
	"If one hides here, return it"
	^nil
</details>

#### TextAttribute>>#actOnClickFor: model

Subclasses may override to provide, eg, hot-spot actions


<details>
	<summary>See more</summary>
	
	actOnClickFor: model
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false
</details>

#### TextAttribute>>#emphasisCode

Subclasses may override to add bold, italic, etc


<details>
	<summary>See more</summary>
	
	emphasisCode
	"Subclasses may override to add bold, italic, etc"
	^ 0
</details>

#### TextAttribute>>#mayBeExtended

A quality that may be overridden by subclasses, such as TextAnchors, that really only apply to a single character


<details>
	<summary>See more</summary>
	
	mayBeExtended
	"A quality that may be overridden by subclasses, such as TextAnchors, that really only apply to a single character"
	^ true
</details>

#### TextAttribute>>#forBaseFontDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forBaseFontDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#forTextActionInfoDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextActionInfoDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#dominatedByNormalEmphasis

Subclasses may override if TextEmphasis normal (i.e. cmd-0) should turn them off


<details>
	<summary>See more</summary>
	
	dominatedByNormalEmphasis
	"Subclasses may override if TextEmphasis normal (i.e. cmd-0) should turn them off"
	^ false
</details>

#### TextAttribute>>#forTextBackgroundColorDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextBackgroundColorDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#actOnClickFor: model in: aTextComposition

<details>
	<summary>See more</summary>
	
	actOnClickFor: model in: aTextComposition
	^self actOnClickFor: model
</details>

#### TextAttribute>>#forCharacterStyleReferenceDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forCharacterStyleReferenceDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#isForFormatting

Answer false for those attributes that are not for text formatting. This is, those that are not considered in #withFormattingAttributeValues:do:


<details>
	<summary>See more</summary>
	
	isForFormatting
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^true
</details>

#### TextAttribute>>#dominates: another

Subclasses may override condense multiple attributes


<details>
	<summary>See more</summary>
	
	dominates: another
	"Subclasses may override condense multiple attributes"
	^ false
</details>

#### TextAttribute>>#forParagraphStyleReferenceDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forParagraphStyleReferenceDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#forTextEmphasisDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextEmphasisDo: aBlock
	"No action is the default"
</details>

#### TextAttribute>>#reset

Allow subclasses to prepare themselves for merging attributes


<details>
	<summary>See more</summary>
	
	reset
	"Allow subclasses to prepare themselves for merging attributes"
</details>

## TextBackgroundColor

A TextBackgroundColor encodes a highlight (background) color change applicable over a given range of text.

### Methods
#### TextBackgroundColor>>#color

<details>
	<summary>See more</summary>
	
	color
	^ color
</details>

#### TextBackgroundColor>>#forTextBackgroundColorDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextBackgroundColorDo: aBlock
	aBlock value: color
</details>

#### TextBackgroundColor>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]
</details>

#### TextBackgroundColor>>#isSet

Do not include Color black, as it is the default color.


<details>
	<summary>See more</summary>
	
	isSet
	"Do not include Color black, as it is the default color."
	^color isTransparent not
</details>

#### TextBackgroundColor>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ color hash
</details>

#### TextBackgroundColor>>#printOn: strm

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color
</details>

#### TextBackgroundColor>>#color: aColor

<details>
	<summary>See more</summary>
	
	color: aColor
	color _ aColor
</details>

#### TextBackgroundColor>>#dominates: other

Subclasses may override condense multiple attributes


<details>
	<summary>See more</summary>
	
	dominates: other
	^ other class == self class
</details>

## TextColor

A TextColor encodes a text color change applicable over a given range of text.

### Methods
#### TextColor>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash
	^ color hash
</details>

#### TextColor>>#forTextColorDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextColorDo: aBlock
	aBlock value: color
</details>

#### TextColor>>#printOn: strm

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color
</details>

#### TextColor>>#dominates: other

Subclasses may override condense multiple attributes


<details>
	<summary>See more</summary>
	
	dominates: other
	^ other class == self class
</details>

#### TextColor>>#color: aColor

<details>
	<summary>See more</summary>
	
	color: aColor
	color _ aColor
</details>

#### TextColor>>#color

<details>
	<summary>See more</summary>
	
	color
	^ color
</details>

#### TextColor>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other color = color]
</details>

#### TextColor>>#isSet

Do not include Color black, as it is the default color.


<details>
	<summary>See more</summary>
	
	isSet
	"Do not include Color black, as it is the default color."
	^color ~= `Color black`
</details>

## TextDoIt

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TextDoIt>>#actOnClickFor: anObject

Note: evalString gets evaluated IN THE CONTEXT OF anObject -- meaning that self and all instVars are accessible


<details>
	<summary>See more</summary>
	
	actOnClickFor: anObject
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Compiler evaluate: evalString for: anObject logged: false.
	^ true 
</details>

#### TextDoIt>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^evalString hash
</details>

#### TextDoIt>>#analyze: aString

Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection). Does not return self!. May be of the form: 3+4 <3+4> Click Here<3+4> <3+4>Click Here


<details>
	<summary>See more</summary>
	
	analyze: aString

	| list |
	list _ super analyze: aString.
	evalString _ list at: 1.
	^ list at: 2
</details>

#### TextDoIt>>#evalString: str

<details>
	<summary>See more</summary>
	
	evalString: str
	evalString _ str 
</details>

#### TextDoIt>>#info

<details>
	<summary>See more</summary>
	
	info
	^ evalString
</details>

#### TextDoIt>>#evalString

<details>
	<summary>See more</summary>
	
	evalString
	^evalString
</details>

#### TextDoIt>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other evalString = evalString]
</details>

## TextEmphasis

A TextEmphasis, encodes a characteristic applicable to all fonts. The encoding is as follows: 1 bold 2 itallic 4 underlined 8 struck out 16 Superscript 32 Subscript 64 with ST-80 Glyphs

### Methods
#### TextEmphasis>>#hash

#hash is re-implemented because #= is re-implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is re-implemented because #= is re-implemented"
	^emphasisCode hash

</details>

#### TextEmphasis>>#printOn: strm

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: emphasisCode
</details>

#### TextEmphasis>>#emphasisCode

Subclasses may override to add bold, italic, etc


<details>
	<summary>See more</summary>
	
	emphasisCode
	^ emphasisCode
</details>

#### TextEmphasis>>#dominates: other

Subclasses may override condense multiple attributes


<details>
	<summary>See more</summary>
	
	dominates: other
	(emphasisCode = 0 and: [ other dominatedByNormalEmphasis ]) ifTrue: [^ true].
	^ (other class == self class)
		and: [ emphasisCode = other emphasisCode ]
</details>

#### TextEmphasis>>#dominatedByNormalEmphasis

TextEmphasis normal (i.e. cmd-0) should turn off emphasis


<details>
	<summary>See more</summary>
	
	dominatedByNormalEmphasis
	"TextEmphasis normal (i.e. cmd-0) should turn off emphasis"
	^ true
</details>

#### TextEmphasis>>#forTextEmphasisDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forTextEmphasisDo: aBlock
	aBlock value: emphasisCode
</details>

#### TextEmphasis>>#emphasisCode: int

<details>
	<summary>See more</summary>
	
	emphasisCode: int
	emphasisCode _ int
</details>

#### TextEmphasis>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other emphasisCode = emphasisCode]
</details>

#### TextEmphasis>>#isSet

Respond true to include this attribute in the text


<details>
	<summary>See more</summary>
	
	isSet
	^ emphasisCode ~= 0
</details>

## TextFontFamilyAndSize

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### TextFontFamilyAndSize>>#hash

#hash is re-implemented because #= is re-implemented


<details>
	<summary>See more</summary>
	
	hash
	"#hash is re-implemented because #= is re-implemented"
	^familyName hash bitXor: pointSize hash
</details>

#### TextFontFamilyAndSize>>#familyName

<details>
	<summary>See more</summary>
	
	familyName
	^familyName
</details>

#### TextFontFamilyAndSize>>#familyName: aString pointSize: aNumber

<details>
	<summary>See more</summary>
	
	familyName: aString pointSize: aNumber
	familyName _ aString.
	pointSize _ aNumber
</details>

#### TextFontFamilyAndSize>>#dominates: other

Subclasses may override condense multiple attributes


<details>
	<summary>See more</summary>
	
	dominates: other
	^ other class == self class
</details>

#### TextFontFamilyAndSize>>#forBaseFontDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forBaseFontDo: aBlock
	aBlock value: self font
</details>

#### TextFontFamilyAndSize>>#forFontFamilyAndSizeDo: aBlock

No action is the default


<details>
	<summary>See more</summary>
	
	forFontFamilyAndSizeDo: aBlock
	aBlock value: familyName value: pointSize
</details>

#### TextFontFamilyAndSize>>#isFont

<details>
	<summary>See more</summary>
	
	isFont
	^true
</details>

#### TextFontFamilyAndSize>>#pointSize

<details>
	<summary>See more</summary>
	
	pointSize
	^pointSize
</details>

#### TextFontFamilyAndSize>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [ other pointSize = pointSize
			and: [ other familyName = familyName ]]
</details>

#### TextFontFamilyAndSize>>#font

<details>
	<summary>See more</summary>
	
	font

	^ AbstractFont familyName: familyName pointSize: pointSize
</details>

## TextURL

('Some text ', (Text string: 'theLink' attributes: {TextURL new url: 'www.koko.kom'}), ' blah, blah.') edit

### Methods
#### TextURL>>#actOnClickFor: anObject

Do what you can with this URL. Later a web browser.


<details>
	<summary>See more</summary>
	
	actOnClickFor: anObject 
	"Do what you can with this URL.  Later a web browser."
	^true
</details>

#### TextURL>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^url hash
</details>

#### TextURL>>#url

<details>
	<summary>See more</summary>
	
	url
	^url
</details>

#### TextURL>>#analyze: aString

Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection). Does not return self!. May be of the form: 3+4 <3+4> Click Here<3+4> <3+4>Click Here


<details>
	<summary>See more</summary>
	
	analyze: aString

	| list |
	list _ super analyze: aString.
	url _ list at: 1.
	^ list at: 2
</details>

#### TextURL>>#url: aString

<details>
	<summary>See more</summary>
	
	url: aString
	url _ aString
</details>

#### TextURL>>#info

<details>
	<summary>See more</summary>
	
	info
	^ url
</details>

#### TextURL>>#= other

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= other
 	self == other ifTrue: [ ^ true ].
	^ (other class == self class) 
		and: [other url = url]
</details>

