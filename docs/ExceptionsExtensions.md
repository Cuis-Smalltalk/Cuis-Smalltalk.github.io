## AssertionFailure

AsssertionFailure is the exception signaled from Object>>assert: when the assertion block evaluates to false.

### Methods
## Halt

Halt is provided to support Object>>halt.

### Methods
#### Halt>>#isResumable

Determine whether an exception is resumable.


<details>
	<summary>See more</summary>
	
	isResumable

	^true
</details>

#### Halt>>#defaultAction

The default action taken if the exception is signaled.


<details>
	<summary>See more</summary>
	
	defaultAction

	self noHandler
</details>

