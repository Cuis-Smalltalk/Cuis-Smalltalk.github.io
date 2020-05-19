## DifferenceFinder

Count := 0.

### Methods
#### DifferenceFinder>>#lcsAt: i at: j

<details>
	<summary>See more</summary>
	
	lcsAt: i at: j
	| lcs |
	(i = 0 or: [j = 0]) ifTrue: [^Set with: #() "EmptyLCS"].
	lcs := matrix i: i j: j.
	lcs ifNil: [
		lcs := self computeLcsAt: i at: j.
		matrix i: i j: j put: lcs].
	^lcs

</details>

#### DifferenceFinder>>#computeMapAt: i at: j

<details>
	<summary>See more</summary>
	
	computeMapAt: i at: j
	| xi yj left up t |
	xi := x at: i.
	yj := y at: j.
	xi = yj ifTrue: [
		t := ((j = 1 or: [i = 1])
			ifTrue: [0]
			ifFalse: [tally i: i - 1 j: j - 1])
			+ 1.
		tally i: i j: j put: t.
		^self class d].
	left := j = 1 ifTrue: [0] ifFalse: [tally i: i j: j - 1].
	up := i = 1 ifTrue: [0] ifFalse: [tally i: i - 1 j: j].
	left < up ifTrue: [
		tally i: i j: j put: up.
		^self class u].
	tally i: i j: j put: left.
	^up < left ifTrue: [self class l] ifFalse: [self class ul]

</details>

#### DifferenceFinder>>#similitudeProportion

<details>
	<summary>See more</summary>
	
	similitudeProportion
	^self maxLength / (x size + y size / 2)
</details>

#### DifferenceFinder>>#keywordsAndBlanksFrom: aString

<details>
	<summary>See more</summary>
	
	keywordsAndBlanksFrom: aString
	^Array streamContents: [:strm | | read keyword tail |
		read := aString readStream.
		[read atEnd] whileFalse: [
			keyword := read nextKeyword.
			keyword notEmpty ifTrue: [
				strm nextPut: keyword ].
			tail := read untilAnySatisfying: [:ch | ch isValidInIdentifiers].
			tail notEmpty ifTrue: [strm nextPut: tail]]]

</details>

#### DifferenceFinder>>#compute

<details>
	<summary>See more</summary>
	
	compute
	^self compute: false
</details>

#### DifferenceFinder>>#initializeMatrix

<details>
	<summary>See more</summary>
	
	initializeMatrix
	matrix _ Array2D height: x size width: y size
</details>

#### DifferenceFinder>>#computeMap

<details>
	<summary>See more</summary>
	
	computeMap
	| m |
	tally _ Array2D height: x size width: y size.
	1 to: x size do: [ :i | 
		1 to: y size do: [ :j | 
			m _ self computeMapAt: i at: j.
			map i: i j: j put: m ]]
</details>

#### DifferenceFinder>>#compareCharacters

<details>
	<summary>See more</summary>
	
	compareCharacters
	x := base.
	y := case

</details>

#### DifferenceFinder>>#base: aCollection case: anotherCollection

<details>
	<summary>See more</summary>
	
	base: aCollection case: anotherCollection
	base := aCollection.
	case := anotherCollection.
	x := aCollection.
	y := anotherCollection

</details>

#### DifferenceFinder>>#unfold: aPoint on: pending

<details>
	<summary>See more</summary>
	
	unfold: aPoint on: pending
	| i j mij |
	i := aPoint x.
	j := aPoint y.
	(i = 0 or: [j = 0]) ifTrue: [^self].
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		pending add: i - 1 @ (j - 1).
		^self].
	mij = self class u ifTrue: [
		pending add: i - 1 @ j.
		^self].
	mij = self class l ifTrue: [
		pending add: i @ (j - 1).
		^self].
	mij = self class ul ifTrue: [
		pending add: i - 1 @ j; add: i @ (j - 1).
		^self].
	self assert: false
</details>

#### DifferenceFinder>>#initializeMap

<details>
	<summary>See more</summary>
	
	initializeMap
	map _ Array2D height: x size width: y size
</details>

#### DifferenceFinder>>#maxLengthPoints

<details>
	<summary>See more</summary>
	
	maxLengthPoints
	| max points |
	max := self maxLength.
	max = 0 ifTrue: [^Array with: `0 @ 0`].
	points := OrderedCollection new.
	tally withIndexesDo: [:i :j :t | t = max ifTrue: [points add: i @ j]].
	^ points
</details>

#### DifferenceFinder>>#compareWords

<details>
	<summary>See more</summary>
	
	compareWords
	x := self keywordsAndBlanksFrom: base.
	y := self keywordsAndBlanksFrom: case

</details>

#### DifferenceFinder>>#differences

<details>
	<summary>See more</summary>
	
	differences
	^differences

</details>

#### DifferenceFinder>>#maxLength

<details>
	<summary>See more</summary>
	
	maxLength

	(tally width = 0 or: [ tally height = 0 ]) ifTrue: [ ^0 ].
	^tally i: x size j: y size
</details>

#### DifferenceFinder>>#unfold: pointCollection

<details>
	<summary>See more</summary>
	
	unfold: pointCollection
	| pending visited point |
	pending := OrderedCollection withAll: pointCollection.
	visited := OrderedCollection new.
	[pending notEmpty] whileTrue: [
		point := pending removeFirst.
		(visited includes: point) ifFalse: [
			self unfold: point on: pending.
			visited add: point]].
	^visited

</details>

#### DifferenceFinder>>#compute: abortIfTooExpensive

If abortIfTooExpensive, we might abort, and then differences could be nil.


<details>
	<summary>See more</summary>
	
	compute: abortIfTooExpensive
	"If abortIfTooExpensive, we might abort, and then differences could be nil."
	| longestSequences |
	self initializeMap; initializeMatrix; computeMap.
	longestSequences _ self longestSequences: abortIfTooExpensive.
	"If decided computation was too expensive..."
	longestSequences ifNil: [
		differences _ nil.
		^self ].
	differences _ longestSequences asArray collect: [ :lcs |
		SequenceDifference x: x y: y lcs: lcs].
	differences sort
</details>

#### DifferenceFinder>>#compareLines

<details>
	<summary>See more</summary>
	
	compareLines
	x := self linesIn: base.
	y := self linesIn: case

</details>

#### DifferenceFinder>>#longestSequences: abortIfTooExpensive

<details>
	<summary>See more</summary>
	
	longestSequences: abortIfTooExpensive
	| maxs points answer |
	maxs _ self maxLengthPoints.
	points _ self unfold: maxs.
	abortIfTooExpensive ifTrue: [
		points size > 500 ifTrue: [ ^nil ].	"maybe a bit too much..."
	].
	points
		sort: [:p :q | p x < q x or: [p x = q x and: [p y <= q y]]];
		do: [:p | self lcsAt: p x at: p y].
	answer _ Set new.
	maxs do: [ :p | | lcs |
		lcs _ self lcsAt: p x at: p y.
		lcs do: [ :s | 
			answer add: s]].
	^answer
</details>

#### DifferenceFinder>>#unfold

<details>
	<summary>See more</summary>
	
	unfold
	| points |
	points := OrderedCollection with: x size @ y size.
	^self unfold: points

</details>

#### DifferenceFinder>>#linesIn: aString

LongestCommonSequenceFinder linesIn: 'x y'


<details>
	<summary>See more</summary>
	
	linesIn: aString
	"
	LongestCommonSequenceFinder linesIn: 'x y'
	"
	^Array streamContents: [:strm | | cr read |
		cr := '
'.
		read := aString readStream.
		[read atEnd] whileFalse: [| line |
			line := read nextLine.
			read skip: -1.
			read peek = cr last ifTrue: [line := line , cr].
			read skip: 1.
			strm nextPut: line]]

</details>

#### DifferenceFinder>>#computeLcsAt: i at: j

<details>
	<summary>See more</summary>
	
	computeLcsAt: i at: j
	| mij cij pair left up |
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		cij := self lcsAt: i - 1 at: j - 1.
		pair := Array with: i with: j.
		^cij collect: [:s | s copyWith: pair]].
	mij = self class u ifTrue: [^self lcsAt: i - 1 at: j].
	mij = self class l ifTrue: [^self lcsAt: i at: j - 1].
	mij = self class ul ifTrue: [
		left := self lcsAt: i at: j - 1.
		up := self lcsAt: i - 1 at: j.
		^left copy addAll: up; yourself].
	self assert: false
</details>

## SequenceDifference

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SequenceDifference>>#asText

<details>
	<summary>See more</summary>
	
	asText
	^Text streamContents: [:rtf | self printTextOn: rtf]
</details>

#### SequenceDifference>>#attributesFor: condition

<details>
	<summary>See more</summary>
	
	attributesFor: condition 
	condition == #unchanged
		ifTrue: [ 
			^ {TextEmphasis normal} ].
	condition == #removed
		ifTrue: [ 
			^ {TextEmphasis struckThrough. TextColor red} ].
	condition == #inserted
		ifTrue: [ 
			^ {TextColor green} ]
</details>

#### SequenceDifference>>#x: aCollection y: anotherCollection lcs: pairCollection

<details>
	<summary>See more</summary>
	
	x: aCollection y: anotherCollection lcs: pairCollection
	x := aCollection.
	y := anotherCollection.
	lcs := pairCollection sort: [ :a :b | a first < b first ]
</details>

#### SequenceDifference>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size
	^lcs sum: [:pair | (x at: pair first) size]

</details>

#### SequenceDifference>>#do: aBlock

<details>
	<summary>See more</summary>
	
	do: aBlock
	| j i item |
	i := j := 1.
	lcs do: [:pair | 
		[i < pair first] whileTrue: [
			item := x at: i.
			aBlock value: item value: #removed.
			i := i + 1].
		[j < pair second] whileTrue: [
			item := y at: j.
			aBlock value: item value: #inserted.
			j := j + 1].
		item := x at: i.
		aBlock value: item value: #unchanged.
		i := i + 1.
		j := j + 1].
	i to: x size do: [:k | 
		item := x at: k.
		aBlock value: item value: #removed].
	j to: y size do: [:k | 
		item := y at: k.
		aBlock value: item value: #inserted]

</details>

#### SequenceDifference>>#lcsSize

<details>
	<summary>See more</summary>
	
	lcsSize
	^lcs size
</details>

#### SequenceDifference>>#partsSize

<details>
	<summary>See more</summary>
	
	partsSize
	| count last |
	count := 0.
	self do: [:item :condition | 
		last = condition ifFalse: [
			count := count + 1.
			last := condition]].
	^count

</details>

#### SequenceDifference>>#<= sequence

<details>
	<summary>See more</summary>
	
	<= sequence
	^lcs size <= sequence lcsSize

</details>

#### SequenceDifference>>#printTextOn: rtf

<details>
	<summary>See more</summary>
	
	printTextOn: rtf
	self do: [:item :condition | | attributes |
		attributes := self attributesFor: condition.
		rtf withAttributes: attributes do: [rtf nextPutAll: item asString]]
</details>

#### SequenceDifference>>#invert

<details>
	<summary>See more</summary>
	
	invert
	| swap |
	swap := x.
	x := y.
	y := swap.
	lcs := lcs collect: [:pair | pair copy swap: 1 with: 2]

</details>

