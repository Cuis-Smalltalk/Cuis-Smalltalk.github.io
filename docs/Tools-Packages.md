## CodePackageList

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### CodePackageList>>#selectionIndex

<details>
	<summary>See more</summary>
	
	selectionIndex

	^ packages indexOf: selection
</details>

#### CodePackageList>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	self updatePackageList.
	CodePackage
		when: #installedPackagesChanged
		send: #updatePackageList
		to: self
</details>

#### CodePackageList>>#description: aText

<details>
	<summary>See more</summary>
	
	description: aText

	selection ifNotNil: [
		selection description: aText string ].
	^true
</details>

#### CodePackageList>>#packages

<details>
	<summary>See more</summary>
	
	packages
	^CodePackage installedPackages asArray sort: [ :a :b |
		 a packageName < b packageName ]
</details>

#### CodePackageList>>#packageDirtyFlags

<details>
	<summary>See more</summary>
	
	packageDirtyFlags

	^ packages collect: [ :each |
		each hasUnsavedChanges
			ifTrue: [ '     --->']
			ifFalse: [ '       -' ]]
</details>

#### CodePackageList>>#packageFullNames

<details>
	<summary>See more</summary>
	
	packageFullNames

	^ packages collect: [ :each | each fullFileName ]
</details>

#### CodePackageList>>#packageNames

<details>
	<summary>See more</summary>
	
	packageNames

	^ packages collect: [ :each | each packageNameAndVersion ]
</details>

#### CodePackageList>>#updateDirtyFlags

<details>
	<summary>See more</summary>
	
	updateDirtyFlags
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;		"if it was set dirty, revision number changed"
		changed: #packageFullNames	"if it was never saved and now it is, fileName changes"
</details>

#### CodePackageList>>#editorClassFor: textGetter

Enable any object to be the textProvider for a PluggableTextModel


<details>
	<summary>See more</summary>
	
	editorClassFor: textGetter
	textGetter = #description ifTrue: [
		^SmalltalkEditor ].		"To enable saving"
	^TextEditor
</details>

#### CodePackageList>>#methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

<details>
	<summary>See more</summary>
	
	methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock

	^aParsingErrorBlock value: nil
</details>

#### CodePackageList>>#summary

<details>
	<summary>See more</summary>
	
	summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method).' ]
</details>

#### CodePackageList>>#save

<details>
	<summary>See more</summary>
	
	save

	selection ifNotNil: [
		selection save ]
</details>

#### CodePackageList>>#selection

<details>
	<summary>See more</summary>
	
	selection

	^selection
</details>

#### CodePackageList>>#updatePackageList

<details>
	<summary>See more</summary>
	
	updatePackageList
	
	| newPackages oldPackages |
	oldPackages _ (packages ifNil: [#()]) asIdentitySet.
	newPackages _ CodePackage installedPackages asIdentitySet.
	oldPackages do: [ :old |
		(newPackages includes: old) ifFalse: [
			old removeActionsWithReceiver: self ]].
	newPackages do: [ :new |
		(oldPackages includes: new) ifFalse: [
			new when: #dirtyFlagChanged send:#updateDirtyFlags to: self ]].
	newPackages _ newPackages asArray sort: [ :a :b |
		 a packageName < b packageName ].
	packages _ newPackages.
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements
</details>

#### CodePackageList>>#selectionIndex: anInteger

<details>
	<summary>See more</summary>
	
	selectionIndex: anInteger

	selection _ anInteger = 0 ifFalse: [ packages at: anInteger ].
	self
		changed: #packageDirtyFlags;
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements
</details>

#### CodePackageList>>#description

<details>
	<summary>See more</summary>
	
	description

	selection ifNil: [ ^'' ].
	^selection description
</details>

## PackageRequirementsList

I model the requirements of a CodePackageList

### Methods
#### PackageRequirementsList>>#selectionIndex

<details>
	<summary>See more</summary>
	
	selectionIndex

	^ requirements indexOf: selection
</details>

#### PackageRequirementsList>>#requirementsStrings

<details>
	<summary>See more</summary>
	
	requirementsStrings

	^ (requirements collect: [ :req | req printString ]) asArray
</details>

#### PackageRequirementsList>>#deleteSelectedRequirement

<details>
	<summary>See more</summary>
	
	deleteSelectedRequirement

	| selectedPackage featureSpec |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	featureSpec 
		provides: featureSpec provides 
		requires: (featureSpec requires copyWithout: self selection).
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	
</details>

#### PackageRequirementsList>>#codePackageList: cpList

<details>
	<summary>See more</summary>
	
	codePackageList: cpList

	codePackageList := cpList.
	selection := 0.
	self updateRequirementsFromPackageList.
	codePackageList 
		when: #changed 
		send:  #updateRequirementsFromPackageList 
		to: self
</details>

#### PackageRequirementsList>>#updateRequirementsFromPackageList

<details>
	<summary>See more</summary>
	
	updateRequirementsFromPackageList
	
	| req |
	req := requirements.
	((codePackageList selectionIndex isNil) 
			or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ requirements := #() ]
		ifFalse: [ requirements := codePackageList selection requires asArray ].
		
	(req = requirements) ifFalse: [
		self changed: #requirements
	]
</details>

#### PackageRequirementsList>>#selection

<details>
	<summary>See more</summary>
	
	selection

	^selection
</details>

#### PackageRequirementsList>>#selectionIndex: anInteger

<details>
	<summary>See more</summary>
	
	selectionIndex: anInteger

	selection := anInteger = 0 ifFalse: [ requirements at: anInteger ].
</details>

#### PackageRequirementsList>>#updateSelectedRequirement

<details>
	<summary>See more</summary>
	
	updateSelectedRequirement

	| selectedPackage requiredPackage featureSpec requirementToUpdate updatedRequirement newRequires selectedName |
	self selectionIndex ifNil: [ ^self ].
	self selectionIndex isZero ifTrue: [ ^self ].
	((codePackageList selectionIndex isNil)  or: [ codePackageList selectionIndex isZero ])
		ifTrue: [ ^self ].
		
	selectedPackage := codePackageList selection.
	featureSpec := selectedPackage featureSpec.
	requirementToUpdate := self selection.
	updatedRequirement := (selectedName _ requirementToUpdate name) = Feature baseSystemFeature name
		ifTrue: [ Feature baseSystemFeature requirementOfMe ]
		ifFalse: [
			requiredPackage := CodePackage installedPackages at: selectedName.
			requiredPackage hasUnsavedChanges
				ifTrue: [ self notify: 'Please save package ', requiredPackage packageName, ' first. Requirement version of an unsaved package can not be updated.'. ^self ].
			requiredPackage requirementOfMe ].
	newRequires := (featureSpec requires copyWithout: requirementToUpdate) copyWith: updatedRequirement.
	featureSpec 
		provides: featureSpec provides 
		requires: newRequires.
	selectedPackage hasUnsavedChanges: true.
	requirements := codePackageList selection requires asArray.
	self changed: #requirements

	
	
</details>

## SinglePackageBrowser

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### SinglePackageBrowser>>#rawMessageCategoryList

Answer the selected category of messages.


<details>
	<summary>See more</summary>
	
	rawMessageCategoryList
	"Answer the selected category of messages."


	^ (package includesSystemCategory: selectedSystemCategory)
		ifTrue: [
			super rawMessageCategoryList reject: [ :cat | package isForeignClassExtension: cat ]]
		ifFalse: [
			super rawMessageCategoryList select: [ :cat | package isYourClassExtension: cat ]]
</details>

#### SinglePackageBrowser>>#messageList

Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range. Otherwise, answer an empty Array If messageCategoryListIndex is found to be larger than the number of categories (it happens!) (??), it is reset to zero.


<details>
	<summary>See more</summary>
	
	messageList
	"Answer an Array of the message selectors of the currently selected message category, provided that the messageCategoryListIndex is in proper range.  Otherwise, answer an empty Array  If messageCategoryListIndex is found to be larger than the number of categories (it happens!) (??), it is reset to zero."

	| answer |
	answer _ selectedMessageCategory
		ifNil: [ #() ]
		ifNotNil: [
			(self classOrMetaClassOrganizer listAtCategoryNamed: selectedMessageCategory) ifNil: [
				selectedMessageCategory _ nil.
				#() ]].
	selectedMessage ifNil: [
		answer size = 0 ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessage _ answer first.
				self editSelection: #editMessage ]]].
	^answer
</details>

#### SinglePackageBrowser>>#defaultBrowserTitle

<details>
	<summary>See more</summary>
	
	defaultBrowserTitle
	^ 'Browser for package: ', package packageName
</details>

#### SinglePackageBrowser>>#messageCategoryList

Answer the selected category of messages.


<details>
	<summary>See more</summary>
	
	messageCategoryList
	"Answer the selected category of messages."

	"Do not include the -- all -- category"
	| answer |
	answer _ self rawMessageCategoryList.
	selectedMessageCategory ifNil: [
		answer size = 0 ifFalse: [
			(package includesSystemCategory: selectedSystemCategory) ifFalse: [
				selectedMessageCategory _ answer first ]]].
	^answer
</details>

#### SinglePackageBrowser>>#defaultClassList

Answer an array of the class names of the selected category. Answer an empty array if no selection exists.


<details>
	<summary>See more</summary>
	
	defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer _ selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer listAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn: (selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer size = 0 ifFalse: [
			selectedClassName _ answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer
</details>

#### SinglePackageBrowser>>#systemCategoryList

Answer the class categories modelled by the receiver.


<details>
	<summary>See more</summary>
	
	systemCategoryList

	^package systemCategories,
		(package systemCategoriesWithExtensionMethods collect: [ :cat |
			'*', cat ])
</details>

#### SinglePackageBrowser>>#package: aCodePackage

<details>
	<summary>See more</summary>
	
	package: aCodePackage

	package _ aCodePackage
</details>

