## DocGenerator

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### DocGenerator>>#writeCategoryFile: categoryName

<details>
	<summary>See more</summary>
	
	writeCategoryFile: categoryName

	(self docfileFor: categoryName) asFileEntry
	forceWriteStreamDo: [ :file |
		(self classesAt: categoryName) do: [ :className | |class|
			class _ Smalltalk at: className.
			
			file nextPutAll: '## ', className, Character newLineCharacter asString.
			file nextPutAll: Character newLineCharacter asString.
			
			file nextPutAll: class comment withBlanksCondensed, Character newLineCharacter asString.
			file nextPutAll: Character newLineCharacter asString.
			
			file nextPutAll: '### Methods', Character newLineCharacter asString.
			self writeClassMethods: class in: file.
		]
	]
</details>

#### DocGenerator>>#writeCategoriesFile

<details>
	<summary>See more</summary>
	
	writeCategoriesFile

	self categories do: [ :categoryName |
		self writeCategoryFile: categoryName
	]

</details>

#### DocGenerator>>#writeSidebarFile

<details>
	<summary>See more</summary>
	
	writeSidebarFile

	'/home/gaston/Code/Smalltalk/cuis-docs/docs/_sidebar.md' asFileEntry
	forceWriteStreamDo: [ :file |
		self categories do: [ :category | |categoryFile|
			categoryFile _ (self sanitizedCategoryName: category, '.md').
			
			file nextPutAll: ('- [', category, '](', categoryFile, ')', Character newLineCharacter asString).
		]
	]
</details>

#### DocGenerator>>#sanitizedCategoryName: categoryName

<details>
	<summary>See more</summary>
	
	sanitizedCategoryName: categoryName

	^ categoryName reject: [:c| c isSeparator ]
</details>

#### DocGenerator>>#docfileFor: categoryName

<details>
	<summary>See more</summary>
	
	docfileFor: categoryName

	^ '/home/gaston/Code/Smalltalk/cuis-docs/docs/', (self sanitizedCategoryName: categoryName), '.md'
</details>

#### DocGenerator>>#categories

<details>
	<summary>See more</summary>
	
	categories

	^ SystemOrganization categories
</details>

#### DocGenerator>>#writeClassMethods: class in: aStream

<details>
	<summary>See more</summary>
	
	writeClassMethods: class in: aStream

	class selectors do: [ :selector | |message messageComment|
		message _ class methodHeaderFor: selector.
		messageComment _ class precodeCommentOrInheritedCommentFor: selector.
		
		aStream nextPutAll: '#### ', class name, '>>#', message, Character newLineCharacter asString.
		aStream nextPut: Character newLineCharacter.
		
		messageComment ifNotNil: [
			aStream nextPutAll: messageComment withBlanksCondensed, Character newLineCharacter asString.
			aStream nextPut: Character newLineCharacter.
			aStream nextPut: Character newLineCharacter.
		].
	
		aStream nextPutAll: 
			'<details>', Character newLineCharacter asString,
				Character tab asString, '<summary>See more</summary>', Character newLineCharacter asString,
				Character tab asString, Character newLineCharacter asString,
				Character tab asString, (class sourceCodeAt: selector), Character newLineCharacter asString,
			'</details>'.
			
		aStream nextPut: Character newLineCharacter.
		aStream nextPut: Character newLineCharacter.	
	] 
</details>

#### DocGenerator>>#classesAt: categoryName

<details>
	<summary>See more</summary>
	
	classesAt: categoryName

	^ SystemOrganization listAtCategoryNamed: categoryName 
</details>

