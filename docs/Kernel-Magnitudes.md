## Magnitude

I'm the abstract class Magnitude that provides common protocol for objects that have the ability to be compared along a linear dimension, such as dates or times. Subclasses of Magnitude include Date, ArithmeticValue, and Time, as well as LookupKey. My subclasses should implement < aMagnitude = aMagnitude hash Here are some example of my protocol: 3 > 4 5 = 6 100 max: 9 7 between: 5 and: 10

### Methods
#### Magnitude>>#hash

Hash must be redefined whenever = is redefined.


<details>
	<summary>See more</summary>
	
	hash
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility
</details>

#### Magnitude>>#between: min and: max

Answer whether the receiver is less than or equal to the argument, max, and greater than or equal to the argument, min.


<details>
	<summary>See more</summary>
	
	between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
</details>

#### Magnitude>>#>= aMagnitude

Answer whether the receiver is greater than or equal to the argument.


<details>
	<summary>See more</summary>
	
	>= aMagnitude 
	"Answer whether the receiver is greater than or equal to the argument."

	^aMagnitude <= self
</details>

#### Magnitude>>#min: aMagnitude

Answer the receiver or the argument, whichever has the lesser magnitude.


<details>
	<summary>See more</summary>
	
	min: aMagnitude 
	"Answer the receiver or the argument, whichever has the lesser 
	magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]
</details>

#### Magnitude>>#min: aMin max: aMax

<details>
	<summary>See more</summary>
	
	min: aMin max: aMax 

	^ (self min: aMin) max: aMax
</details>

#### Magnitude>>#<= aMagnitude

Answer whether the receiver is less than or equal to the argument.


<details>
	<summary>See more</summary>
	
	<= aMagnitude 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not
</details>

#### Magnitude>>#> aMagnitude

Answer whether the receiver is greater than the argument.


<details>
	<summary>See more</summary>
	
	> aMagnitude 
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self
</details>

#### Magnitude>>#< aMagnitude

Answer whether the receiver is less than the argument.


<details>
	<summary>See more</summary>
	
	< aMagnitude 
	"Answer whether the receiver is less than the argument."

	^self subclassResponsibility
</details>

#### Magnitude>>#= aMagnitude

Compare the receiver with the argument and answer with true if the receiver is equal to the argument. Otherwise answer false.


<details>
	<summary>See more</summary>
	
	= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility
</details>

#### Magnitude>>#max: aMagnitude

Answer the receiver or the argument, whichever has the greater magnitude.


<details>
	<summary>See more</summary>
	
	max: aMagnitude 
	"Answer the receiver or the argument, whichever has the greater 
	magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]
</details>

