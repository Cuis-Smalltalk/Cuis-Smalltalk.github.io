## Date

Instances of Date are Timespans with duration of 1 day. Their default creation assumes a start of midnight in the local time zone.

### Methods
#### Date>>#julianDayNumber

<details>
	<summary>See more</summary>
	
	julianDayNumber

	^ start julianDayNumber
</details>

#### Date>>#isLeapYear

<details>
	<summary>See more</summary>
	
	isLeapYear

	^ start isLeapYear
</details>

#### Date>>#yyyymmdd

Format the date in ISO 8601 standard like '2002-10-22'.


<details>
	<summary>See more</summary>
	
	yyyymmdd
	"Format the date in ISO 8601 standard like '2002-10-22'."

	^ self printFormat: #(3 2 1 $- 1 1 2)
</details>

#### Date>>#year

<details>
	<summary>See more</summary>
	
	year

	^start year
</details>

#### Date>>#weekdayIndex

Sunday=1, ... , Saturday=7 Monday=1, ... , Sunday=7 '12 May 2017 ' asDate weekdayIndex = 5


<details>
	<summary>See more</summary>
	
	weekdayIndex
	"Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate weekdayIndex = 5
	"

	^ self dayOfWeek
</details>

#### Date>>#week

<details>
	<summary>See more</summary>
	
	week

	^start week
</details>

#### Date>>#dayOfMonth

Answer the day of the month represented by the receiver.


<details>
	<summary>See more</summary>
	
	dayOfMonth
	"Answer the day of the month represented by the receiver."

	^ start dayOfMonth
</details>

#### Date>>#daysLeftInYear

<details>
	<summary>See more</summary>
	
	daysLeftInYear
	^ start daysLeftInYear
</details>

#### Date>>#printFormat: formatArray

Answer a String describing the receiver using the format denoted by the argument, formatArray.


<details>
	<summary>See more</summary>
	
	printFormat: formatArray 
	"Answer a String describing the receiver using the format denoted by the 
	argument, formatArray."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream format: formatArray.
	^aStream contents
</details>

#### Date>>#weekday

Answer the name of the day of the week on which the receiver falls.


<details>
	<summary>See more</summary>
	
	weekday
	"Answer the name of the day of the week on which the receiver falls."

	^ self dayOfWeekName
</details>

#### Date>>#dayOfYear

Answer the day of the year represented by the receiver.


<details>
	<summary>See more</summary>
	
	dayOfYear
	"Answer the day of the year represented by the receiver."

	^ start dayOfYear
</details>

#### Date>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self printOn: aStream format: #(1 2 3 $  3 1 )
</details>

#### Date>>#secondsSinceSqueakEpoch

Answer the seconds since the Squeak epoch: 1 January 1901


<details>
	<summary>See more</summary>
	
	secondsSinceSqueakEpoch
	"Answer the seconds since the Squeak epoch: 1 January 1901"

	^ start secondsSinceSqueakEpoch
</details>

#### Date>>#previous: dayName

Answer the previous date whose weekday name is dayName.


<details>
	<summary>See more</summary>
	
	previous: dayName 
	"Answer the previous date whose weekday name is dayName."

	| n |
	n _ 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
	n = 0 ifTrue: [ n _ 7 ].
	^ self - n days
</details>

#### Date>>#dayOfWeek

Answer the day of the week represented by the receiver.


<details>
	<summary>See more</summary>
	
	dayOfWeek
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeek
</details>

#### Date>>#< aDate

<details>
	<summary>See more</summary>
	
	< aDate

	self assert: aDate class == Date.
	^ self start < aDate start
</details>

#### Date>>#month

<details>
	<summary>See more</summary>
	
	month

	^start month
</details>

#### Date>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream print: self printString; nextPutAll: ' asDate'
</details>

#### Date>>#monthIndex

<details>
	<summary>See more</summary>
	
	monthIndex

	^ start monthIndex
</details>

#### Date>>#mmddyyyy

Answer the receiver rendered in standard fmt mm/dd/yyyy. Good for avoiding year 2000 bugs. Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, so that for example feb 1 1996 is 2/1/96


<details>
	<summary>See more</summary>
	
	mmddyyyy
	"Answer the receiver rendered in standard fmt mm/dd/yyyy.  Good for avoiding year 2000 bugs.  Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, so that for example feb 1 1996 is 2/1/96"

	"Date today mmddyyyy"

	^ self printFormat: #(2 1 3 $/ 1 1)
</details>

#### Date>>#> aDate

<details>
	<summary>See more</summary>
	
	> aDate

	self assert: aDate class == Date.
	^ self start > aDate start
</details>

#### Date>>#monthName

<details>
	<summary>See more</summary>
	
	monthName

	^ start monthName
</details>

#### Date>>#dayOfWeekName

Answer the day of the week represented by the receiver.


<details>
	<summary>See more</summary>
	
	dayOfWeekName
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeekName
</details>

#### Date>>#firstDayOfMonth

<details>
	<summary>See more</summary>
	
	firstDayOfMonth

	^ start firstDayOfMonth
</details>

#### Date>>#monthAbbreviation

<details>
	<summary>See more</summary>
	
	monthAbbreviation

	^ start monthAbbreviation
</details>

#### Date>>#daysInYear

Answer the number of days in the month represented by the receiver.


<details>
	<summary>See more</summary>
	
	daysInYear
	"Answer the number of days in the month represented by the receiver."

	^ start daysInYear
</details>

#### Date>>#yearNumber

<details>
	<summary>See more</summary>
	
	yearNumber

	^ start yearNumber
</details>

#### Date>>#printOn: aStream format: formatArray

Print a description of the receiver on aStream using the format denoted the argument, formatArray: #(item item item sep monthfmt yearfmt twoDigits) items: 1=day 2=month 3=year will appear in the order given, separated by sep which is eaither an ascii code or character. monthFmt: 1=09 2=Sep 3=September yearFmt: 1=1996 2=96 digits: (missing or)1=9 2=09. See the examples in printOn: and mmddyy


<details>
	<summary>See more</summary>
	
	printOn: aStream format: formatArray 
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| gregorian twoDigits element monthFormat |
	gregorian _ self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits _ formatArray size > 6 and: [(formatArray at: 7) > 1].
	1 to: 3 do: 
		[ :i | 
			element := formatArray at: i.
			element = 1
				ifTrue: [twoDigits
						ifTrue: [aStream
								nextPutAll: (gregorian first asString
										padded: #left
										to: 2
										with: $0)]
						ifFalse: [gregorian first printOn: aStream]].
			element = 2
				ifTrue: [monthFormat := formatArray at: 5.
					monthFormat = 1
						ifTrue: [twoDigits
								ifTrue: [aStream
										nextPutAll: (gregorian middle asString
												padded: #left
												to: 2
												with: $0)]
								ifFalse: [gregorian middle printOn: aStream]].
					monthFormat = 2
						ifTrue: [aStream
								nextPutAll: ((Month nameOfMonth: gregorian middle)
										copyFrom: 1
										to: 3)].
					monthFormat = 3
						ifTrue: [aStream
								nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6)
							= 1
						ifTrue: [gregorian last printOn: aStream]
						ifFalse: [aStream
								nextPutAll: ((gregorian last \\ 100) asString
										padded: #left
										to: 2
										with: $0)]].
			i < 3
				ifTrue: [(formatArray at: 4)
							~= 0
						ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]]

</details>

#### Date>>#dayMonthYearDo: aBlock

Supply integers for day, month and year to aBlock and return the result


<details>
	<summary>See more</summary>
	
	dayMonthYearDo: aBlock 
	"Supply integers for day, month and year to aBlock and return the result"

	^ start dayMonthYearDo: aBlock
</details>

#### Date>>#daysInMonth

<details>
	<summary>See more</summary>
	
	daysInMonth


	^ start daysInMonth
</details>

## DateAndTime

I represent a point in UTC time as defined by ISO 8601. I have zero duration. My implementation uses three SmallIntegers and a Duration: jdn - julian day number. seconds - number of seconds since midnight. nanos - the number of nanoseconds since the second. offset - duration from UTC. The nanosecond attribute is almost always zero but it defined for full ISO compliance and is suitable for timestamping.

### Methods
#### DateAndTime>>#year

<details>
	<summary>See more</summary>
	
	year

	^Year including: self
</details>

#### DateAndTime>>#week

<details>
	<summary>See more</summary>
	
	week

	^Week including: self
</details>

#### DateAndTime>>#printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean

Print as per ISO 8601 section 5.4.2 If printLeadingSpaceToo is false, prints either: 'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years) If printLeadingSpaceToo is true, prints either: ' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)


<details>
	<summary>See more</summary>
	
	printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c numericValue > $0 numericValue ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs asString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs asString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated asString) ]]
</details>

#### DateAndTime>>#timeZoneName

<details>
	<summary>See more</summary>
	
	timeZoneName

	^ self class localTimeZone name

</details>

#### DateAndTime>>#dayOfWeekAbbreviation

<details>
	<summary>See more</summary>
	
	dayOfWeekAbbreviation

	^ self dayOfWeekName copyFrom: 1 to: 3
</details>

#### DateAndTime>>#printHMSOn: aStream separator: aChar

Print just hh:mm:ss


<details>
	<summary>See more</summary>
	
	printHMSOn: aStream separator: aChar
	"Print just hh:mm:ss"
	aStream
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self second asString padded: #left to: 2 with: $0).

</details>

#### DateAndTime>>#utcOffset: anOffset

Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset


<details>
	<summary>See more</summary>
	
	utcOffset: anOffset

	"Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset"

	| diff |
	self assert: anOffset class == Duration.

	diff _ anOffset - self offset.
	^ self class
		julianDayNumber: jdn
		seconds: seconds + diff totalSeconds
		nanoseconds: nanos + diff nanoSeconds
		offset: anOffset
</details>

#### DateAndTime>>#time

<details>
	<summary>See more</summary>
	
	time

	^Time seconds: seconds nanoSeconds: nanos
</details>

#### DateAndTime>>#hour24

<details>
	<summary>See more</summary>
	
	hour24


	^ (Duration seconds: seconds) hours

</details>

#### DateAndTime>>#setJdn: j seconds: s nano: n offset: o

<details>
	<summary>See more</summary>
	
	setJdn: j seconds: s nano: n offset: o

jdn := j.
seconds := s.
nanos :=  n.
offset :=  o

</details>

#### DateAndTime>>#printOn: aStream

Print as per ISO 8601 section 5.4.2 Prints either: 'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: true
</details>

#### DateAndTime>>#dayOfWeek

Sunday=1, ... , Saturday=7 Monday=1, ... , Sunday=7 '12 May 2017 ' asDate dayOfWeek = 5


<details>
	<summary>See more</summary>
	
	dayOfWeek

	"
	Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate dayOfWeek = 5
	"

	^ (jdn rem: 7) + 1
</details>

#### DateAndTime>>#to: anEnd

Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan


<details>
	<summary>See more</summary>
	
	to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	^ Timespan starting: self ending: anEnd
</details>

#### DateAndTime>>#filenamishPrintOn: aStream

String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]


<details>
	<summary>See more</summary>
	
	filenamishPrintOn: aStream
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName _ Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour asString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute asString padded: #left to: 2 with: $0);
		nextPut: $m
</details>

#### DateAndTime>>#unixTimeSeconds

Return the number of seconds since the Unix epoch as an integer number DateAndTime now unixTimeSeconds To check consistency with https://en.wikipedia.org/wiki/Unix_time (DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSeconds 1462118860


<details>
	<summary>See more</summary>
	
	unixTimeSeconds
	"Return the number of seconds since the Unix epoch as an integer number

	DateAndTime now  unixTimeSeconds
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSeconds
	1462118860
	"

	| elapsed |
	elapsed _ self - self class unixEpoch.
	^ elapsed totalSeconds
</details>

#### DateAndTime>>#nanoSecond

<details>
	<summary>See more</summary>
	
	nanoSecond


	^ nanos

</details>

#### DateAndTime>>#timeZoneAbbreviation

<details>
	<summary>See more</summary>
	
	timeZoneAbbreviation

	^ self class localTimeZone abbreviation

</details>

#### DateAndTime>>#printWithMsOn: aStream

Print with millisecond resolution, no leading space, no offset.


<details>
	<summary>See more</summary>
	
	printWithMsOn: aStream
	"Print with millisecond resolution, no leading space, no offset."

	| ps |
	self printYMDOn: aStream withLeadingSpace: false.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	ps _ (self nanoSecond // 1000000) printString padded: #left to: 3 with: $0.
	aStream nextPut: $..
	aStream nextPutAll: ps
</details>

#### DateAndTime>>#minute

<details>
	<summary>See more</summary>
	
	minute


	^ (Duration seconds: seconds) minutes

</details>

#### DateAndTime>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^ aSymbol == #DateAndTime or: [ super is: aSymbol ]
</details>

#### DateAndTime>>#monthName

<details>
	<summary>See more</summary>
	
	monthName


	^ Month nameOfMonth: self monthIndex

</details>

#### DateAndTime>>#second

<details>
	<summary>See more</summary>
	
	second


	^ (Duration seconds: seconds) seconds

</details>

#### DateAndTime>>#noon

Answer a DateAndTime starting at noon


<details>
	<summary>See more</summary>
	
	noon
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: [ :d :m :y |
		self class year: y month: m day: d hour: 12 minute: 0 second: 0 ]
</details>

#### DateAndTime>>#dayOfWeekName

'12 May 2017 ' asDate dayOfWeek = 5 '12 May 2017 ' asDate dayOfWeekName = #Friday


<details>
	<summary>See more</summary>
	
	dayOfWeekName
	"
	'12 May 2017 ' asDate dayOfWeek = 5 
	'12 May 2017 ' asDate dayOfWeekName = #Friday 
	"

	^ Week nameOfDay: self dayOfWeek

</details>

#### DateAndTime>>#subtractFrom: aDateAndTime

<details>
	<summary>See more</summary>
	
	subtractFrom: aDateAndTime

	^ aDateAndTime subtractDateAndtime: self
</details>

#### DateAndTime>>#hour12

Answer an <integer> between 1 and 12, inclusive, representing the hour of the day in the 12-hour clock of the local time of the receiver.


<details>
	<summary>See more</summary>
	
	hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	^ self hour24 - 1 \\ 12 + 1
</details>

#### DateAndTime>>#monthAbbreviation

<details>
	<summary>See more</summary>
	
	monthAbbreviation


	^ self monthName copyFrom: 1 to: 3

</details>

#### DateAndTime>>#- operand

operand is a DateAndTime or a Duration. Double dispatch


<details>
	<summary>See more</summary>
	
	- operand
	"operand is a DateAndTime or a Duration.
	Double dispatch"

	^ operand subtractFrom: self
</details>

#### DateAndTime>>#includingTimespanOf: aTimespanClass

<details>
	<summary>See more</summary>
	
	includingTimespanOf: aTimespanClass

	^ aTimespanClass includingDateAndTime: self
</details>

#### DateAndTime>>#daysInYear

Answer the number of days in the year represented by the receiver.


<details>
	<summary>See more</summary>
	
	daysInYear

	"Answer the number of days in the year represented by the receiver."

	^ (Year including: self) daysInYear
</details>

#### DateAndTime>>#meridianAbbreviation

<details>
	<summary>See more</summary>
	
	meridianAbbreviation

	^ self time meridianAbbreviation
</details>

#### DateAndTime>>#dayMonthYearDo: aBlock

Evaluation the block with three arguments: day month, year.


<details>
	<summary>See more</summary>
	
	dayMonthYearDo: aBlock
	"Evaluation the block with three arguments: day month, year."

	| l n i j dd mm yyyy |
	l := jdn + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	dd := l - (2447 * j // 80).
	l := j // 11.
	mm := j + 2 - (12 * l).
	yyyy := 100 * (n - 49) + i + l.

	^ aBlock
		value: dd
		value: mm
		value: yyyy.
</details>

#### DateAndTime>>#daysInMonth

Answer the number of days in the month represented by the receiver.


<details>
	<summary>See more</summary>
	
	daysInMonth
	"Answer the number of days in the month represented by the receiver."


	^ self month daysInMonth
</details>

#### DateAndTime>>#julianDayNumber

<details>
	<summary>See more</summary>
	
	julianDayNumber


	^ jdn

</details>

#### DateAndTime>>#isLeapYear

<details>
	<summary>See more</summary>
	
	isLeapYear


	^ Year isLeapYear: self yearNumber
</details>

#### DateAndTime>>#dayOfMonth

Answer which day of the month is represented by the receiver.


<details>
	<summary>See more</summary>
	
	dayOfMonth
	"Answer which day of the month is represented by the receiver."

	^ self
		dayMonthYearDo: [ :d :m :y | d ]
</details>

#### DateAndTime>>#daysLeftInYear

Answer the number of days in the year after the date of the receiver.


<details>
	<summary>See more</summary>
	
	daysLeftInYear
	"Answer the number of days in the year after the date of the receiver."

	^ self daysInYear - self dayOfYear

</details>

#### DateAndTime>>#to: anEnd by: aDuration do: aBlock

Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan


<details>
	<summary>See more</summary>
	
	to: anEnd by: aDuration do: aBlock
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	| schedule |
	schedule _ self to: anEnd by: aDuration.
	schedule scheduleDo: aBlock.
	"Timespan are open on the right side. But include requested end if appropriate, like implementation in Number
	See DateAndTimeTest>>#testToByDo
	"
	schedule end + DateAndTime clockPrecision = anEnd ifTrue: [
		aBlock value: anEnd ]
</details>

#### DateAndTime>>#= comparand

comparand is a DateAndTime.


<details>
	<summary>See more</summary>
	
	= comparand
	"comparand is a DateAndTime."
	| lvalue rvalue |

	"Any object is equal to itself"
	self == comparand ifTrue: [ ^ true ].

	self class == comparand class ifFalse: [ ^false ].

	offset = comparand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ comparand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ comparand asUTC ].
	^ lvalue julianDayNumber = rvalue julianDayNumber
		and: [ lvalue secondsSinceMidnight = rvalue secondsSinceMidnight
			and: [ lvalue nanoSecond = rvalue nanoSecond ] ]
</details>

#### DateAndTime>>#hash

Hash must be redefined whenever = is redefined.


<details>
	<summary>See more</summary>
	
	hash

	^(jdn bitXor: seconds) bitXor: nanos
</details>

#### DateAndTime>>#dayOfYear

This code was contributed by Dan Ingalls. It is equivalent to the terser ^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker.


<details>
	<summary>See more</summary>
	
	dayOfYear
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	| monthStart |
	^ self dayMonthYearDo:
		[ :d :m :y |
			monthStart _ #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]]
</details>

#### DateAndTime>>#secondsSinceSqueakEpoch

Return the number of seconds since the Squeak epoch


<details>
	<summary>See more</summary>
	
	secondsSinceSqueakEpoch
	"Return the number of seconds since the Squeak epoch"

	^ (self - (self class epoch)) totalSeconds
</details>

#### DateAndTime>>#asLocal

<details>
	<summary>See more</summary>
	
	asLocal
	

	^ (self offset = self class localOffset)

		ifTrue: [self]
		ifFalse: [self utcOffset: self class localOffset]

</details>

#### DateAndTime>>#date

<details>
	<summary>See more</summary>
	
	date

	^Date including: self
</details>

#### DateAndTime>>#printHMSOn: aStream

Print just hh:mm:ss


<details>
	<summary>See more</summary>
	
	printHMSOn: aStream
	"Print just hh:mm:ss"
	self printHMSOn: aStream separator: $:
</details>

#### DateAndTime>>#asUTC

<details>
	<summary>See more</summary>
	
	asUTC

	^ offset isZero
		ifTrue: [self]
		ifFalse: [self utcOffset: 0 hours]

</details>

#### DateAndTime>>#< comparand

comparand is a DateAndTime.


<details>
	<summary>See more</summary>
	
	< comparand
	"comparand is a DateAndTime."
	| lvalue rvalue |
	self assert: (comparand is: #DateAndTime).
	offset = comparand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ comparand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ comparand asUTC ].
	^ lvalue julianDayNumber < rvalue julianDayNumber
		or: [
			lvalue julianDayNumber > rvalue julianDayNumber
				ifTrue: [ false ]
				ifFalse: [
					lvalue secondsSinceMidnight < rvalue secondsSinceMidnight
						or: [
							lvalue secondsSinceMidnight > rvalue secondsSinceMidnight
								ifTrue: [ false ]
								ifFalse: [ lvalue nanoSecond < rvalue nanoSecond ]]]]
</details>

#### DateAndTime>>#printStringWithoutOffset

Print as per ISO 8601 section 5.4.2 Prints either: 'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)


<details>
	<summary>See more</summary>
	
	printStringWithoutOffset
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^String streamContents: [ :strm | self printWithoutOffsetOn: strm ]
</details>

#### DateAndTime>>#hour

<details>
	<summary>See more</summary>
	
	hour

	^ self hour24

</details>

#### DateAndTime>>#month

<details>
	<summary>See more</summary>
	
	month

	^Month including: self
</details>

#### DateAndTime>>#unixTimeSecondsFloat

Return the number of seconds since the Unix epoch. Answer an instance of Float, including fraction of a second DateAndTime now unixTimeSecondsFloat To check consistency with https://en.wikipedia.org/wiki/Unix_time (DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSecondsFloat 1.46211886e9


<details>
	<summary>See more</summary>
	
	unixTimeSecondsFloat
	"Return the number of seconds since the Unix epoch.
	Answer an instance of Float, including fraction of a second

	DateAndTime now  unixTimeSecondsFloat
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSecondsFloat
	1.46211886e9
	"

	"We know that DateAndTime unixEpoch has nanos set to zero"
	^ self unixTimeSeconds + (nanos * 1.0e-9)
</details>

#### DateAndTime>>#printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo

Print just the year, month, and day on aStream. If printLeadingSpaceToo is true, then print as: ' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative) otherwise print as: 'YYYY-MM-DD' or '-YYYY-MM-DD'


<details>
	<summary>See more</summary>
	
	printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo
	"Print just the year, month, and day on aStream.

	If printLeadingSpaceToo is true, then print as:
		' YYYY-MM-DD' (if the year is positive) or '-YYYY-MM-DD' (if the year is negative)
	otherwise print as:
		'YYYY-MM-DD' or '-YYYY-MM-DD' "

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ printLeadingSpaceToo ifTrue: [ aStream space ]].
	aStream
		nextPutAll: (year abs asString padded: #left to: 4 with: $0);
		nextPut: $-;
		nextPutAll: (month asString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (day asString padded: #left to: 2 with: $0)

</details>

#### DateAndTime>>#subtractDateAndtime: operand

operand is a DateAndTime or a Duration


<details>
	<summary>See more</summary>
	
	subtractDateAndtime: operand
	"operand is a DateAndTime or a Duration"

	| lvalue rvalue |
	offset = operand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ operand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ operand asUTC ].
	^ Duration
		seconds: (Time secondsInDay *(lvalue julianDayNumber - rvalue julianDayNumber)) + 
					(lvalue secondsSinceMidnight - rvalue secondsSinceMidnight)
		nanoSeconds: lvalue nanoSecond - rvalue nanoSecond
</details>

#### DateAndTime>>#to: anEnd by: aDuration

Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan


<details>
	<summary>See more</summary>
	
	to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #DateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.

</details>

#### DateAndTime>>#secondsSinceMidnight

<details>
	<summary>See more</summary>
	
	secondsSinceMidnight

	^ seconds
</details>

#### DateAndTime>>#subtractDuration: operand

operand is a DateAndTime or a Duration


<details>
	<summary>See more</summary>
	
	subtractDuration: operand
	"operand is a DateAndTime or a Duration"

	^self + operand negated
</details>

#### DateAndTime>>#monthIndex

<details>
	<summary>See more</summary>
	
	monthIndex
	^ self dayMonthYearDo:
		[ : d : m : y |  m ]
</details>

#### DateAndTime>>#julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset

secs could span more than one day. nsecs could span more than one second.


<details>
	<summary>See more</summary>
	
	julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset
	"secs could span more than one day. nsecs could span more than one second."
	
	| daysOverflow nanosSinceSec secsOverflow secsSinceMidnight totalSecs totalDays |
	"Add to totalSecs any whole seconds in nsecs"
	secsOverflow _ nsecs // Time nanosInSecond.
	nanosSinceSec _ nsecs - (secsOverflow * Time nanosInSecond).
	totalSecs _ secs + secsOverflow.

	"Add to totalDays any whole days in totalSecs"
	daysOverflow _ totalSecs // Time secondsInDay.
	secsSinceMidnight _ totalSecs - (daysOverflow * Time secondsInDay).
	totalDays _ day + daysOverflow.

	jdn _ totalDays.
	seconds _ secsSinceMidnight.
	nanos _ nanosSinceSec.
	offset _ utcOffset
</details>

#### DateAndTime>>#midnight

Answer a DateAndTime starting at midnight local time


<details>
	<summary>See more</summary>
	
	midnight
	"Answer a DateAndTime starting at midnight local time"

	^self class basicNew
		setJdn: jdn
		seconds: 0
		nano: 0
		offset: self class localOffset

</details>

#### DateAndTime>>#firstDayOfMonth

<details>
	<summary>See more</summary>
	
	firstDayOfMonth

	^ self month start dayOfYear
</details>

#### DateAndTime>>#printWithoutOffsetOn: aStream

Print as per ISO 8601 section 5.4.2 Prints either: 'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)


<details>
	<summary>See more</summary>
	
	printWithoutOffsetOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: false
</details>

#### DateAndTime>>#offset

<details>
	<summary>See more</summary>
	
	offset

	^ offset

</details>

#### DateAndTime>>#+ operand

operand conforms to protocol Duration


<details>
	<summary>See more</summary>
	
	+ operand
	"operand conforms to protocol Duration"

	self assert: operand class == Duration.

	^ self class
		julianDayNumber: jdn
		seconds: seconds + operand totalSeconds
		nanoseconds: nanos + operand nanoSeconds
		offset: offset
</details>

#### DateAndTime>>#yearNumber

<details>
	<summary>See more</summary>
	
	yearNumber
	^ self
		dayMonthYearDo: [ :d :m :y | y ]
</details>

#### DateAndTime>>#printYMDOn: aStream

Print just YYYY-MM-DD part. If the year is negative, prints out '-YYYY-MM-DD'.


<details>
	<summary>See more</summary>
	
	printYMDOn: aStream
	"Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	^self printYMDOn: aStream withLeadingSpace: false.

</details>

## Duration

I represent a duration of time. I have nanosecond precision

### Methods
#### Duration>>#// operand

operand is a Duration or a Number


<details>
	<summary>See more</summary>
	
	// operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds // operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds // operand totalNanoSeconds ]

</details>

#### Duration>>#nanoSeconds

<details>
	<summary>See more</summary>
	
	nanoSeconds


	^ nanos

</details>

#### Duration>>#abs

<details>
	<summary>See more</summary>
	
	abs

	^ self class seconds: seconds abs nanoSeconds: nanos abs

</details>

#### Duration>>#totalMinutes

Answer an integer number of minutes


<details>
	<summary>See more</summary>
	
	totalMinutes
	"Answer an integer number of minutes"
	^ seconds // 60
</details>

#### Duration>>#totalMilliseconds

<details>
	<summary>See more</summary>
	
	totalMilliseconds

	^ (seconds * 1000) + (nanos // 1e6)
</details>

#### Duration>>#= comparand

Answer whether the argument is a <Duration> representing the same period of time as the receiver.


<details>
	<summary>See more</summary>
	
	= comparand 
	"Answer whether the argument is a <Duration> representing the same 
	period of time as the receiver."

	self == comparand
		ifTrue: [ ^ true ].

	self species == comparand species 
		ifFalse: [ ^ false ].

	^ self totalNanoSeconds = comparand totalNanoSeconds
</details>

#### Duration>>#seconds

Answer the number of seconds the receiver represents.


<details>
	<summary>See more</summary>
	
	seconds
	"Answer the number of seconds the receiver represents."

	^seconds rem: Time secondsInMinute
</details>

#### Duration>>#hash

Hash must be redefined whenever = is redefined.


<details>
	<summary>See more</summary>
	
	hash
	^seconds hash bitXor: nanos hash
</details>

#### Duration>>#printOn: aStream

Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S] (Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString = '2:03:16:43'


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
	"
	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	m printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	s printOn: aStream length: 2 zeroPadded: true.
	n = 0 ifFalse: [
		| z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c digitValue > 0 ].
		z _ #(3 6 9) detect: [ :ez | ez >= z ].	"print either milliseconds, microseconds or nanoseconds"
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ]
</details>

#### Duration>>#days

Answer the number of days the receiver represents.


<details>
	<summary>See more</summary>
	
	days
	"Answer the number of days the receiver represents."

	^ seconds quo: Time secondsInDay
</details>

#### Duration>>#truncateTo: aDuration

e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes.


<details>
	<summary>See more</summary>
	
	truncateTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes."

	^ self class
		nanoSeconds: (self totalNanoSeconds truncateTo: aDuration totalNanoSeconds)


</details>

#### Duration>>#seconds: secondCount nanoSeconds: nanoCount

Private - only used by Duration class


<details>
	<summary>See more</summary>
	
	seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + Time nanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - Time nanosInSecond ]


</details>

#### Duration>>#< comparand

Answer whether the receiver is less than the argument.


<details>
	<summary>See more</summary>
	
	< comparand

	^ self totalNanoSeconds < comparand totalNanoSeconds

</details>

#### Duration>>#negated

<details>
	<summary>See more</summary>
	
	negated

	^ self class seconds: seconds negated nanoSeconds: nanos negated

</details>

#### Duration>>#/ operand

operand is a Duration or a Number


<details>
	<summary>See more</summary>
	
	/ operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds / operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds / operand totalNanoSeconds ]
.

</details>

#### Duration>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	self seconds: 0 nanoSeconds: 0.

</details>

#### Duration>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self className;
		nextPutAll: ' seconds: ';
		print: seconds;
		nextPutAll: ' nanoSeconds: ';
		print: nanos;
		nextPut: $).

</details>

#### Duration>>#asDelay

<details>
	<summary>See more</summary>
	
	asDelay

	^ Delay forDuration: self
</details>

#### Duration>>#hours

Answer the number of hours the receiver represents.


<details>
	<summary>See more</summary>
	
	hours
	"Answer the number of hours the receiver represents."


	^ (seconds rem: Time secondsInDay) quo: Time secondsInHour
</details>

#### Duration>>#totalSeconds

<details>
	<summary>See more</summary>
	
	totalSeconds

	^ seconds
</details>

#### Duration>>#subtractFrom: aDateAndTimeOrDate

<details>
	<summary>See more</summary>
	
	subtractFrom: aDateAndTimeOrDate

	^aDateAndTimeOrDate subtractDuration: self
</details>

#### Duration>>#totalNanoSeconds

<details>
	<summary>See more</summary>
	
	totalNanoSeconds

	^ (seconds * Time nanosInSecond) + nanos
</details>

#### Duration>>#roundTo: aDuration

e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes.


<details>
	<summary>See more</summary>
	
	roundTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."

	^ self class nanoSeconds: (self totalNanoSeconds roundTo: aDuration totalNanoSeconds)


</details>

#### Duration>>#+ operand

operand is a Duration


<details>
	<summary>See more</summary>
	
	+ operand

	"operand is a Duration" 	^ self class nanoSeconds: (self totalNanoSeconds + operand totalNanoSeconds)

</details>

#### Duration>>#- operand

operand is a Duration


<details>
	<summary>See more</summary>
	
	- operand
	"operand is a Duration" 	^ self + operand negated

</details>

#### Duration>>#negative

<details>
	<summary>See more</summary>
	
	negative


	^ self positive not

</details>

#### Duration>>#minutes

Answer the number of minutes the receiver represents.


<details>
	<summary>See more</summary>
	
	minutes
	"Answer the number of minutes the receiver represents."

	^ (seconds rem: Time secondsInHour) quo: Time secondsInMinute
</details>

#### Duration>>#isZero

<details>
	<summary>See more</summary>
	
	isZero

	^ seconds = 0 and: [ nanos = 0 ]

</details>

#### Duration>>#\\ operand

modulo. Remainder defined in terms of //. Answer a Duration with the same sign as aDuration. operand is a Duration or a Number.


<details>
	<summary>See more</summary>
	
	\\ operand

	"modulo. Remainder defined in terms of //. Answer a Duration with the 
	same sign as aDuration. operand is a Duration or a Number."

	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds \\ operand) ]
		ifFalse: [ self - (operand * (self // operand)) ]

</details>

#### Duration>>#* operand

operand is a Number


<details>
	<summary>See more</summary>
	
	* operand
	"operand is a Number" 	^ self class nanoSeconds: ( (self totalNanoSeconds * operand) asInteger).

</details>

#### Duration>>#positive

<details>
	<summary>See more</summary>
	
	positive


	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]

</details>

## Month

I represent a month.

### Methods
#### Month>>#isLeapYear

<details>
	<summary>See more</summary>
	
	isLeapYear

	^ start isLeapYear
</details>

#### Month>>#monthAbbreviation

<details>
	<summary>See more</summary>
	
	monthAbbreviation

	^ start monthAbbreviation
</details>

#### Month>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	aStream nextPutAll: self monthName, ' ', self yearNumber printString
</details>

#### Month>>#year

<details>
	<summary>See more</summary>
	
	year

	^start year
</details>

#### Month>>#subtractMonth: aMonth

Months can be subtracted even they have different length.


<details>
	<summary>See more</summary>
	
	subtractMonth: aMonth
	"Months can be subtracted even they have different length."

	^self start subtractDateAndtime: aMonth start
</details>

#### Month>>#daysInYear

Answer the number of days in the month represented by the receiver.


<details>
	<summary>See more</summary>
	
	daysInYear
	"Answer the number of days in the month represented by the receiver."

	^ start daysInYear
</details>

#### Month>>#yearNumber

<details>
	<summary>See more</summary>
	
	yearNumber

	^ start yearNumber
</details>

#### Month>>#monthIndex

<details>
	<summary>See more</summary>
	
	monthIndex

	^ start monthIndex
</details>

#### Month>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name
	^ self monthName
</details>

#### Month>>#daysInMonth

<details>
	<summary>See more</summary>
	
	daysInMonth

	^ self duration days
</details>

#### Month>>#monthName

<details>
	<summary>See more</summary>
	
	monthName

	^ start monthName
</details>

#### Month>>#subtractFrom: aTimespan

<details>
	<summary>See more</summary>
	
	subtractFrom: aTimespan

	^ aTimespan subtractMonth: self
</details>

## Schedule

I represent a powerful class for implementing recurring schedules.

### Methods
#### Schedule>>#dateAndTimes

<details>
	<summary>See more</summary>
	
	dateAndTimes

	| dateAndTimes |
	dateAndTimes _ OrderedCollection new.
	self scheduleDo: [ :e | dateAndTimes add: e ].
	^ dateAndTimes asArray.
</details>

#### Schedule>>#scheduleDuration

<details>
	<summary>See more</summary>
	
	scheduleDuration
	^schedule sum
</details>

#### Schedule>>#schedule

<details>
	<summary>See more</summary>
	
	schedule
	^ schedule

</details>

#### Schedule>>#includes: aDateAndTime

Operand might be a Timespan or a DateAndtime


<details>
	<summary>See more</summary>
	
	includes: aDateAndTime

	| dt |
	self assert: aDateAndTime class == DateAndTime.	"Or else understand and fix"
	dt _ aDateAndTime.
	self scheduleDo: [ :e | e = dt ifTrue: [^true] ].
	^ false.

</details>

#### Schedule>>#schedule: anArrayOfDurations

<details>
	<summary>See more</summary>
	
	schedule: anArrayOfDurations

	schedule _ anArrayOfDurations

</details>

#### Schedule>>#between: aStart and: anEnd do: aBlock

<details>
	<summary>See more</summary>
	
	between: aStart and: anEnd do: aBlock

	| element end i requestedStartDate |
	end _ self end min: anEnd.
	element _ self start.
	
	"Performance optimization. Avoid going through unnecesary days if easy."
	requestedStartDate _ aStart date.
	(requestedStartDate start > element and: [ self everyDayAtSameTimes ]) ifTrue: [
		element _ DateAndTime date: requestedStartDate time: element time ].

	i _ 1.
	[ element < aStart ] whileTrue: [
		element _ element + (schedule at: i).
		i _ i + 1.
		i > schedule size ifTrue: [i _ 1]].
	i _ 1.
	[ element <= end ] whileTrue: [
		aBlock value: element.
		element _ element + (schedule at: i).
		i _ i + 1.
		i > schedule size ifTrue: [i _ 1]]
</details>

#### Schedule>>#everyDayAtSameTimes

Answer false if unknown


<details>
	<summary>See more</summary>
	
	everyDayAtSameTimes
	"Answer false if unknown"
	
	| count |
	count _ (Duration days: 1) / self scheduleDuration.
	^count >=1 and: [ count isInteger ]
</details>

#### Schedule>>#scheduleDo: aBlock

<details>
	<summary>See more</summary>
	
	scheduleDo: aBlock

	self between: (self start) and: (self end) do: aBlock.

</details>

## Time

This represents a particular point in time during any given day. For example, '5:19:45 pm'. If you need a point in time on a particular day, use DateAndTime. If you need a duration of time, use Duration.

### Methods
#### Time>>#hhmm24

Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm), of exactly 4 digits


<details>
	<summary>See more</summary>
	
	hhmm24
	"Return a string of the form 1123 (for 11:23 am), 2154 (for 9:54 pm), of exactly 4 digits"

	^(String streamContents: 
		[ :aStream | self print24: true showSeconds: false on: aStream ])
			copyWithout: $:
</details>

#### Time>>#print24

Return as 8-digit string 'hh:mm:ss', with leading zeros if needed


<details>
	<summary>See more</summary>
	
	print24
	"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"

	^String streamContents: [ :aStream |
		self print24: true showSeconds: true on: aStream ]
</details>

#### Time>>#hour24

<details>
	<summary>See more</summary>
	
	hour24


	^ self asDurationSinceMidnight hours

</details>

#### Time>>#print24: hr24 showSeconds: showSeconds showSecondsFraction: showSecondsFraction on: aStream

Format is 'hh:mm:ss' or 'h:mm:ss am' or, if showSeconds is false, 'hh:mm' or 'h:mm am'


<details>
	<summary>See more</summary>
	
	print24: hr24 showSeconds: showSeconds showSecondsFraction: showSecondsFraction on: aStream
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h _ self hour.
	m _ self minute.
	s _ self second.
	hr24
		ifTrue: [
			h < 10 ifTrue: [ aStream nextPutAll: '0' ].
			h printOn: aStream ]
		ifFalse: [
			h > 12
				ifTrue: [h - 12 printOn: aStream]
				ifFalse: 
					[h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ]]].

	aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
	m printOn: aStream.

	showSeconds ifTrue: [
		aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
		(self nanoSecond = 0 or: [ showSecondsFraction not ])
			ifTrue: [s asInteger printOn: aStream]
			ifFalse: [(s + (1.0 * self nanoSecond / Time nanosInSecond)) printOn: aStream]].

	hr24 ifFalse: [
		aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ]
</details>

#### Time>>#= aTime

Any object is equal to itself


<details>
	<summary>See more</summary>
	
	= aTime

	"Any object is equal to itself"
	self == aTime ifTrue: [ ^ true ].

	self class == aTime class ifFalse: [ ^false ].
	^ seconds = aTime privateSeconds and: [ nanos = aTime privateNanos ]
</details>

#### Time>>#hash

Hash must be redefined whenever = is redefined.


<details>
	<summary>See more</summary>
	
	hash

	^ seconds hash bitXor: nanos hash
</details>

#### Time>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	self print24: false
		showSeconds: (seconds ~= 0
				or: [ nanos ~= 0])
		showSecondsFraction: nanos ~= 0
		on: aStream
</details>

#### Time>>#privateSeconds

<details>
	<summary>See more</summary>
	
	privateSeconds
	^seconds
</details>

#### Time>>#asDurationSinceMidnight

Answer the duration since midnight


<details>
	<summary>See more</summary>
	
	asDurationSinceMidnight

	"Answer the duration since midnight"

	^ Duration seconds: seconds nanoSeconds: nanos
</details>

#### Time>>#< aTime

Answer whether the receiver is less than the argument.


<details>
	<summary>See more</summary>
	
	< aTime

	^seconds < aTime privateSeconds or: [ 
		seconds = aTime privateSeconds and: [ nanos < aTime privateNanos ]]
</details>

#### Time>>#seconds: secondCount nanoSeconds: nanoCount

Private - only used by Time class.


<details>
	<summary>See more</summary>
	
	seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Time class."

	self assert: nanoCount < Time nanosInSecond.
	seconds _ secondCount.
	nanos _ nanoCount
</details>

#### Time>>#hour

<details>
	<summary>See more</summary>
	
	hour

	^ self hour24

</details>

#### Time>>#print24: hr24 showSecondsFraction: showSecondsFraction on: aStream

Format is 'hh:mm:ss.ssssss' or 'h:mm:ss.ssssss am' or, if showSecondsFraction is false, 'hh:mm:ss' or 'h:mm:ss am'


<details>
	<summary>See more</summary>
	
	print24: hr24 showSecondsFraction: showSecondsFraction on: aStream
	"Format is 'hh:mm:ss.ssssss' or 'h:mm:ss.ssssss am'  or, if showSecondsFraction is false, 'hh:mm:ss' or 'h:mm:ss am'"

	^self print24: hr24 showSeconds: true showSecondsFraction: showSecondsFraction on: aStream
</details>

#### Time>>#nanoSecond

<details>
	<summary>See more</summary>
	
	nanoSecond


	^ nanos

</details>

#### Time>>#storeOn: aStream

Append to the argument aStream a sequence of characters that is an expression whose evaluation creates an object similar to the receiver.


<details>
	<summary>See more</summary>
	
	storeOn: aStream

	aStream print: self printString; nextPutAll: ' asTime'
</details>

#### Time>>#minute

<details>
	<summary>See more</summary>
	
	minute

	^ self asDurationSinceMidnight minutes
</details>

#### Time>>#second

<details>
	<summary>See more</summary>
	
	second


	^ self asDurationSinceMidnight seconds
</details>

#### Time>>#print24: hr24 on: aStream

Format is 'hh:mm:ss' or 'h:mm:ss am'


<details>
	<summary>See more</summary>
	
	print24: hr24 on: aStream 
	"Format is 'hh:mm:ss' or 'h:mm:ss am' "

	self print24: hr24 showSeconds: true on: aStream 

</details>

#### Time>>#print24: hr24 showSeconds: showSeconds on: aStream

Format is 'hh:mm:ss' or 'h:mm:ss am' or, if showSeconds is false, 'hh:mm' or 'h:mm am'


<details>
	<summary>See more</summary>
	
	print24: hr24 showSeconds: showSeconds on: aStream
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	^self print24: hr24 showSeconds: showSeconds showSecondsFraction: false on: aStream
</details>

#### Time>>#hour12

Answer an <integer> between 1 and 12, inclusive, representing the hour of the day in the 12-hour clock of the local time of the receiver.


<details>
	<summary>See more</summary>
	
	hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	^ self hour24 - 1 \\ 12 + 1
</details>

#### Time>>#- aTime

<details>
	<summary>See more</summary>
	
	- aTime
	^ self asDurationSinceMidnight - aTime asDurationSinceMidnight
</details>

#### Time>>#meridianAbbreviation

<details>
	<summary>See more</summary>
	
	meridianAbbreviation

	^ self hour < 12 ifTrue: ['AM'] ifFalse: ['PM'].

</details>

#### Time>>#printMinutes

Return as string 'hh:mm pm'


<details>
	<summary>See more</summary>
	
	printMinutes
	"Return as string 'hh:mm pm'  "

	^String streamContents:
		[ :aStream | self print24: false showSeconds: false on: aStream ]


</details>

#### Time>>#privateNanos

<details>
	<summary>See more</summary>
	
	privateNanos
	^nanos
</details>

#### Time>>#seconds: secondCount

Private - only used by Time class.


<details>
	<summary>See more</summary>
	
	seconds: secondCount
	"Private - only used by Time class."

	seconds _ secondCount.
	nanos _ 0
</details>

## TimeZone

TimeZone is a simple class to colect the information identifying a UTC time zone. offset - Duration - the time zone's offset from UTC abbreviation - String - the abbreviated name for the time zone. name - String - the name of the time zone. TimeZone class >> #timeZones returns an array of the known time zones TimeZone class >> #default returns the default time zone (Grenwich Mean Time)

### Methods
#### TimeZone>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self abbreviation;
		nextPut: $).
</details>

#### TimeZone>>#offset

<details>
	<summary>See more</summary>
	
	offset

	^ offset
</details>

#### TimeZone>>#abbreviation

<details>
	<summary>See more</summary>
	
	abbreviation

	^ abbreviation

</details>

#### TimeZone>>#abbreviation: aString

<details>
	<summary>See more</summary>
	
	abbreviation: aString

	abbreviation _ aString

</details>

#### TimeZone>>#offset: aDuration

<details>
	<summary>See more</summary>
	
	offset: aDuration

	offset _ aDuration
</details>

#### TimeZone>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^ name

</details>

#### TimeZone>>#name: aString

<details>
	<summary>See more</summary>
	
	name: aString

	name _ aString

</details>

## Timespan

I represent a duration starting on a specific DateAndTime.

### Methods
#### Timespan>>#workDatesDo: aBlock

Exclude Saturday and Sunday


<details>
	<summary>See more</summary>
	
	workDatesDo: aBlock
	"Exclude Saturday and Sunday"

	self do: aBlock with: start date when: [ :d | d dayOfWeek < 6 ].

</details>

#### Timespan>>#do: aBlock with: aFirstElement

<details>
	<summary>See more</summary>
	
	do: aBlock with: aFirstElement

	self do: aBlock with: aFirstElement when: [ :t | true ].

</details>

#### Timespan>>#previous

(Month month: 10 year: 2017) previous (Year yearNumber: 2016) previous


<details>
	<summary>See more</summary>
	
	previous
	"
	(Month month: 10 year: 2017) previous
	(Year yearNumber: 2016) previous
	"
	^self class classDefinesDuration
		ifTrue: [ self class including: self end - duration ]
		ifFalse: [ self class starting: start - duration duration: duration ]
</details>

#### Timespan>>#subtractYear: aYear

<details>
	<summary>See more</summary>
	
	subtractYear: aYear

	^ self subtractTimespan: aYear
</details>

#### Timespan>>#= comparand

Answer whether the receiver and the argument represent the same object. If = is redefined in any subclass, consider also redefining the message hash.


<details>
	<summary>See more</summary>
	
	= comparand
 	self == comparand ifTrue: [ ^ true ].
	^ self class == comparand class 
		and: [ self start = comparand start ]
		and: [ self duration = comparand duration ]
.
</details>

#### Timespan>>#hash

Answer a SmallInteger whose value is related to the receiver's identity. May be overridden, and should be overridden in any classes that define =


<details>
	<summary>See more</summary>
	
	hash

	^ start hash bitXor: duration hash

</details>

#### Timespan>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream


	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: start;
		nextPut: $D;
		print: duration;
		nextPut: $).

</details>

#### Timespan>>#includes: operand

Operand might be a Timespan or a DateAndtime


<details>
	<summary>See more</summary>
	
	includes: operand
	"Operand might be a Timespan or a DateAndtime"

	^ (operand is: #Timespan)
			ifTrue: [ (self includes: operand start)
						and: [ self includes: operand end ] ]
			ifFalse: [ operand between: start and: self end ]
</details>

#### Timespan>>#do: aBlock with: aFirstElement when: aConditionBlock

<details>
	<summary>See more</summary>
	
	do: aBlock with: aFirstElement when: aConditionBlock

	| element end |
	element _ aFirstElement.
	end _ self end.
	[ element start <= end ] whileTrue:
	
	[(aConditionBlock value: element)
			ifTrue: [ aBlock value: element ].
		element _ element next. ]
</details>

#### Timespan>>#includesAllOf: aCollection

Answer whether all the elements of aCollection are in the receiver.


<details>
	<summary>See more</summary>
	
	includesAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."

	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true

</details>

#### Timespan>>#to: anEnd

Answer an Timespan. anEnd must be aDateAndTime


<details>
	<summary>See more</summary>
	
	to: anEnd
	"Answer an Timespan. anEnd must be aDateAndTime"

	self assert: (anEnd is: #DateAndTime).
	^ Timespan starting: self start ending: anEnd
</details>

#### Timespan>>#end

<details>
	<summary>See more</summary>
	
	end

	^ self duration totalNanoSeconds = 0
		ifTrue: [ self start ]
		ifFalse: [ self next start - DateAndTime clockPrecision ]
</details>

#### Timespan>>#includesAnyOf: aCollection

Answer whether any element of aCollection is included in the receiver


<details>
	<summary>See more</summary>
	
	includesAnyOf: aCollection 
	"Answer whether any element of aCollection is included in the receiver"

	aCollection do: [ :elem | (self includes: elem) ifTrue: [^ true]].
	^false

</details>

#### Timespan>>#duration

Answer the Duration of this timespan


<details>
	<summary>See more</summary>
	
	duration
	"Answer the Duration of this timespan"

	^ duration

</details>

#### Timespan>>#duration: aDuration

Set the Duration of this timespan


<details>
	<summary>See more</summary>
	
	duration: aDuration
	"Set the Duration of this timespan"

	duration _ aDuration

</details>

#### Timespan>>#every: aDuration do: aBlock

<details>
	<summary>See more</summary>
	
	every: aDuration do: aBlock

	| element end |
	element _ self start.
	end _ self end.
	[ element <= end ] whileTrue:
	
	[ aBlock value: element.
		element _ element + aDuration. ]

</details>

#### Timespan>>#subtractMonth: aYear

<details>
	<summary>See more</summary>
	
	subtractMonth: aYear

	^ self subtractTimespan: aYear
</details>

#### Timespan>>#is: aSymbol

A means for cleanly replacing isXXX like methods. Please use judiciously! aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc. A few comments: - Good for kernel tests - Good for tests defined in the same package as the receiver - Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases - In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching. - if a class happens to answer true for several Symbols, consider implementing it like: ^#(symbol1 symbol2 symbol3) statePointsTo: aSymbol


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #Timespan or: [ super is: aSymbol ]
</details>

#### Timespan>>#subtractDuration: aDuration

<details>
	<summary>See more</summary>
	
	subtractDuration: aDuration

	^self class classDefinesDuration
		ifTrue: [ self class including: start - aDuration ]
		ifFalse: [ self class starting: start - aDuration duration: duration ]
</details>

#### Timespan>>#next

<details>
	<summary>See more</summary>
	
	next

	^self class classDefinesDuration
		ifTrue: [ self class including: start + duration ]
		ifFalse: [ self class starting: start + duration duration: duration ]
</details>

#### Timespan>>#start

Answer the start DateAndTime of this timespan


<details>
	<summary>See more</summary>
	
	start
	"Answer the start DateAndTime of this timespan"

	^ start

</details>

#### Timespan>>#intersection: aTimespan

Return the Timespan both have in common, or nil


<details>
	<summary>See more</summary>
	
	intersection: aTimespan
	 "Return the Timespan both have in common, or nil"
	"Warning: There's something possibly wrong here. See 
	http://lists.gforge.inria.fr/pipermail/pharo-project/2011-May/048253.html
	
	It seems reasonable to expect this to answer true. However it answers false:
	|start end span|
	start :=DateAndTime now.
	end := start + 1 hour.
	span := Timespan starting: start ending: end.
	(span intersection: span) = span
	"

	 | aBegin anEnd |
	 aBegin _ self start max: aTimespan start.
	 anEnd _ self end min: aTimespan end.
	 anEnd < aBegin ifTrue: [^nil].

	 ^ Timespan starting: aBegin ending: anEnd
</details>

#### Timespan>>#subtractFrom: aTimespan

<details>
	<summary>See more</summary>
	
	subtractFrom: aTimespan

	^ aTimespan subtractTimespan: self
</details>

#### Timespan>>#includingTimespanOf: aTimespanClass

<details>
	<summary>See more</summary>
	
	includingTimespanOf: aTimespanClass

	^ aTimespanClass includingTimespan: self
</details>

#### Timespan>>#union: aTimespan

Return the Timespan spanned by both


<details>
	<summary>See more</summary>
	
	union: aTimespan
	 "Return the Timespan spanned by both"
	"
	| union timespan |
	timespan := Timespan starting:
					(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0)
						duration: (Duration hours: 100).
	union := timespan union: timespan.
	
	self 
		assert: (union start = timespan start);
		assert: (union duration = timespan duration)
	"

	| aBegin anEnd |

	aBegin _ self start min: aTimespan start.
	anEnd _ self end max: aTimespan end.
	^ Timespan starting: aBegin ending: anEnd + DateAndTime clockPrecision
</details>

#### Timespan>>#+ aDuration

<details>
	<summary>See more</summary>
	
	+ aDuration

	^self class classDefinesDuration
		ifTrue: [ self class including: start + aDuration ]
		ifFalse: [ self class starting: start + aDuration duration: duration ]
</details>

#### Timespan>>#- aDurationOrTimespan

<details>
	<summary>See more</summary>
	
	- aDurationOrTimespan

	^ aDurationOrTimespan subtractFrom: self
</details>

#### Timespan>>#subtractTimespan: aTimespan

(Month month: 'March' year: 2017) - (Month month: 'January' year: 2017) (Month month: 'February' year: 2017) - (Month month: 'January' year: 2017) (Year yearNumber: 2016) - (Year yearNumber: 2015). (Year yearNumber: 2017) - (Year yearNumber: 2016). (Year yearNumber: 2017) - (Year yearNumber: 2015). (Year yearNumber: 2018) - (Date today). 'Error'.


<details>
	<summary>See more</summary>
	
	subtractTimespan: aTimespan
	"
	(Month month: 'March' year: 2017) - (Month month: 'January' year: 2017)
	(Month month: 'February' year: 2017) - (Month month: 'January' year: 2017)

	(Year yearNumber: 2016) - (Year yearNumber: 2015).
	(Year yearNumber: 2017) - (Year yearNumber: 2016).
	(Year yearNumber: 2017) - (Year yearNumber: 2015).

	(Year yearNumber: 2018) - (Date today). 					'Error'.
	"
	aTimespan duration = self duration ifFalse: [
		self error: 'Can not subtract Timespans of different duration' ].
	
	^self start subtractDateAndtime: aTimespan start
</details>

#### Timespan>>#start: aDateAndTime

Store the start DateAndTime of this timespan


<details>
	<summary>See more</summary>
	
	start: aDateAndTime
	"Store the start DateAndTime of this timespan"

	self assert: (aDateAndTime is: #DateAndTime).
	start _ aDateAndTime
</details>

## Week

I represent a week.

### Methods
#### Week>>#yearNumber

<details>
	<summary>See more</summary>
	
	yearNumber

	| thursday |
	thursday _ self start + 3 days.
	^thursday yearNumber
</details>

#### Week>>#printOn: aStream

'2008-W52' asWeek. '2009-W01' asWeek '2009-W02' asWeek '2009-W53' asWeek '2010-W01' asWeek '2010-W02' asWeek


<details>
	<summary>See more</summary>
	
	printOn: aStream
	"
		'2008-W52' asWeek.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
	"
	self yearNumber printOn: aStream.
	aStream nextPutAll: '-W'.
	self weekNumber printOn: aStream length: 2 zeroPadded: true
</details>

#### Week>>#weekNumber

<details>
	<summary>See more</summary>
	
	weekNumber

	| thursday |
	thursday _ self start + 3 days.
	^thursday dayOfYear-1 // 7 + 1
</details>

## Year

I represent a year.

### Methods
#### Year>>#subtractFrom: aTimespan

<details>
	<summary>See more</summary>
	
	subtractFrom: aTimespan

	^ aTimespan subtractYear: self
</details>

#### Year>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	aStream nextPutAll: 'a Year ('.
	self start yearNumber printOn: aStream.

	aStream nextPutAll: ')'.

</details>

#### Year>>#daysInMonth

<details>
	<summary>See more</summary>
	
	daysInMonth


	self shouldNotImplement 

</details>

#### Year>>#subtractYear: aYear

Years can be subtracted even if one of them is leap and the other isn't.


<details>
	<summary>See more</summary>
	
	subtractYear: aYear
	"Years can be subtracted even if one of them is leap and the other isn't."

	^self start subtractDateAndtime: aYear start
</details>

#### Year>>#yearNumber

<details>
	<summary>See more</summary>
	
	yearNumber

	^ start yearNumber
</details>

#### Year>>#daysInYear

<details>
	<summary>See more</summary>
	
	daysInYear

	^ self duration days
</details>

