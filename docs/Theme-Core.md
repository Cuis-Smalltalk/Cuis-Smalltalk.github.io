## BrightColorTheme

Default bright colored theme for Cuis.

### Methods
## ContentPack

ContentPack lets you read in and write out the (supported files in the) contents of a directory on your file system. It also allows you to trivially create "messenger" subclasses that capture the information containted in these directory trees, including any implicit communication that's there in the structure of the directory hierarchy itself, which are captured in your changes file. You can then file out a change set that contains a representation of the (supported file/object types and directory structurein) the stuff on your disk, or in your image. This subclass is a dummy which ContentPack compiles methods into containing base 64 encoded data. You can load this into another image, as long as that image has ContentPack loaded. The filed in class can then recreate the ContentPack on the other end with the media files and structure intact. The current implementation is based on #storeString, but the plan is to change that to SmartRefStream in the long run to support serializing things like morphs. ContentPack instances hang onto the actual tree of media objects. It has a nice simple EDSL that just interprets an array of strings from beginning to end as a "path" to a file (really a series of dictionary lookups to a Smalltalk object, wherin the dictionaries mirror the structure of what was on the disk, sans unsupported files.) This mechanism will likely change a little bit at some point, ContentPack came into the world a little faster than I expected, as I ended up using it to send some icons back in time to fix the Cuis update stream without having to sort my changes all over again. As such it had some unusual design pressures... it had to be able to carry information in and out of both the change set stream and the filesystem, as well as function in a slightly earlier (unreleased) version of Cuis than it was written in, and not break anything on it's way back up through the build to head. The code, in particular the way things are named, has not settled yet, and that's why this comment contains no code examples. Use with care and read the code first, for now. Currently, .bmp import and .png import are implemented, and both can be exported. Anything you can import, you can also shuffle into a change set. Plans are in the works to support audio, change sets, and text files. I'll support video if someone has a good importer, exporter, and player under the MIT license that'll work under Cuis. Currently, objects are serialized into single methods, which works for small icons, but likely doesn't work well (if at all) for larger files. My intent is to add some behavior that breaks up large objects into smaller chunks so that this becomes a non-issue. I'll likely get to that when I've removed most of the repetitive subtle variations of the same recursive tree walking visitor-trick from the code, and renamed everything. I think in essence this class is slightly smaller than it is as represented currently. Hopefully I will be able to explain all of this better once I've clarified the code a bit so that I can show off some examples. - cbr ----------------------------------- Alternative description (need to merge both!) Forms (and potentially other media types) can exist in three forms: 1) As external files, such as jpg, png, etc. This is the representation we need to use external tools (such as image processing apps, cameras, scanners, web, etc) to work on them. 2) As methods. Non human readable, base-64 encoded binary data. We need this to be able to include such stuff in the update stream, or in packages. After we update an image, we usually delete these methods, just keeping 3). 3) Live objects in the image, for example, stored in class variables. This is to make use of them in Cuis. Most of the time, we use 3). But we need 2) for the update stream. We also need 1) sometimes to work on them. ContentPack supports the conversion between these 3 formats. The implementation is quite simple. What is really great is that Casey realized we need some tool to move comfortably between these 3 representations. And he also implemented it. - jmv ------------------------------------------- Usage hints Feature require: 'Graphics-Files-Additional'. Theme content export. "Build 'Import' directory copying the structure of 'Exported', with stuff to be loaded" "Just build an instance from files" ContentPack import. "Build an instance and generate code" ContentPack generateCode: ContentPack import. "Build an instance from code generated" ContentPack decode. "Build and merge. Usually do this in the postscript of the change set that includes the generated code" Theme content merge: ContentPack decode. ContentPack removeCategory: ContentPack generatedMethodsCategory

### Methods
#### ContentPack>>#from: key get: anArray

Theme content from: #Theme get: #('16x16' 'actions' 'view-refresh.png' ) Answer nil when the object isn't found. --cbr


<details>
	<summary>See more</summary>
	
	from: key get: anArray

"
	Theme content from: #Theme get: #('16x16' 'actions' 'view-refresh.png' )
	Answer nil when the object isn't found.
		--cbr	"
		
	| object | object _ self at: key ifAbsent: [ ^ nil ].
	
	anArray do: [ :i | object _ object at: i ifAbsent: [ ^ nil ]].
	
	^ object
</details>

#### ContentPack>>#asContentPack: aDictionary

Recursively make ContentPacks of all objects which include behavior from Dictionary.


<details>
	<summary>See more</summary>
	
	asContentPack: aDictionary

	"Recursively make ContentPacks of all objects which include behavior from Dictionary."

	| associations |

	associations _ aDictionary keys collect: [ :key | 
		((aDictionary at: key) is: #Dictionary)
			ifTrue: [ key -> (self asContentPack: (aDictionary at: key)) ]
			ifFalse: [ key -> (aDictionary at: key) ]	
	].

	^ associations as: ContentPack
</details>

#### ContentPack>>#export

Answer true on success.


<details>
	<summary>See more</summary>
	
	export

	"Answer true on success."

	"self break."

	self class exportDirectory exists
		ifTrue: [ Utilities inform: 
			'Before you can export, you must move, rename or delete this directory: ' ,
			self exportDirectory pathName.
			
			^ false.
		].
	
	self exportInto: self class exportDirectory.
	
	^ true
</details>

#### ContentPack>>#is: aSymbol

Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum.


<details>
	<summary>See more</summary>
	
	is: aSymbol
	^aSymbol == #ContentPack or: [ super is: aSymbol ]
</details>

#### ContentPack>>#merge: aDictionaryOrContentPack

Merge aDictionaryOrContentPack into us


<details>
	<summary>See more</summary>
	
	merge: aDictionaryOrContentPack
	"Merge aDictionaryOrContentPack into us"

	aDictionaryOrContentPack keysAndValuesDo: [ :key :value |
		(value is: #ContentPack)
			ifFalse: [
				self at: key put: value ]
			ifTrue: [
				(self at: key ifAbsentPut: [ContentPack new])
					merge: value ]]
</details>

#### ContentPack>>#loadContentFrom: aDirectoryEntry

Load content in us, from files. Start with an empty instance!


<details>
	<summary>See more</summary>
	
	loadContentFrom: aDirectoryEntry
	"Load content in us, from files.
	Start with an empty instance!"

	(self supportedFilesIn: aDirectoryEntry) do: [ :filename |			
		self flag: #todo. "Add hook for other media types here. Also consider renaming this method. --cbr"
		self at: filename name 
			put: (Form fromFileEntry: filename)
	].

	aDirectoryEntry directoryNames do: [ :i |
		self at: i put: (ContentPack new loadContentFrom: aDirectoryEntry / i)
	]
</details>

#### ContentPack>>#decodeContentFrom: associationList

Load content in us, frin geberated code. Start with an empty instance!


<details>
	<summary>See more</summary>
	
	decodeContentFrom: associationList
	"Load content in us, frin geberated code.
	Start with an empty instance!"

	associationList do: [ :assoc |
		(assoc at: 2) class == Array
			ifTrue: [ 
				self at: (assoc at: 1) put: (ContentPack new decodeContentFrom: (assoc at: 2)) ]
			ifFalse: [ 
				self at: (assoc at: 1) put: (Compiler 
					evaluate: (self 
						perform: 
							('object' , (assoc at: 2) asString) asSymbol) base64Decoded)
			]
	]
</details>

#### ContentPack>>#asDictionary

Recursively make Dictionaries of all objects which include behavior from ContentPack.


<details>
	<summary>See more</summary>
	
	asDictionary

	"Recursively make Dictionaries of all objects which include behavior from ContentPack."

	| associations |

	associations _ self keys collect: [ :key | 
		((self at: key) is: #ContentPack)
			ifTrue: [ key -> (self at: key) asDictionary ]
			ifFalse: [ key -> (self at: key) ]	
	].

	^ associations as: Dictionary
</details>

#### ContentPack>>#fixFormKeysIn: aContentPack

Need to add file extension to the keys for the Forms; it made sense to strip them out before I realized that ContentPack could be used for other media types, which suddenly meant that file type was no longer implicit..


<details>
	<summary>See more</summary>
	
	fixFormKeysIn: aContentPack

	"Need to add file extension to the keys for the Forms; 
	it made sense to strip them out before I realized that 
	ContentPack could be used for other media types, 
	which suddenly meant that file type was no longer implicit.."

| associations |

	self flag: #todo. "Remove this in a later change set. This is only needed once to get the icons working right again."


	associations _ aContentPack keys collect: [ :key |
		((aContentPack at: key) is: #Dictionary)
			ifTrue: [ key -> (self fixFormKeysIn: (aContentPack at: key)) ] "Recurse."
			ifFalse: [ (key , '.png') -> (aContentPack at: key) ] "Fix leaf."
	].

	^ associations as: ContentPack
			
</details>

#### ContentPack>>#decodeContent

Load content in us. Start with an empty instance!


<details>
	<summary>See more</summary>
	
	decodeContent
	"Load content in us.
	Start with an empty instance!"

	self decodeContentFrom: self contentMap
</details>

#### ContentPack>>#exportInto: aDirectory

<details>
	<summary>See more</summary>
	
	exportInto: aDirectory

	| featureName |
	featureName _ 'Graphics-Files-Additional'.
	(FeatureRequirement name: featureName) isAlreadySatisfied
		ifFalse: [
			self error: 'Please load "', featureName, '".'].

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory / assoc key.
			fullPath _  aDirectory // assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writePNGfile: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writePNGfile: fullPath ]
	]
</details>

#### ContentPack>>#supportedFilesIn: aDirectoryEntry

<details>
	<summary>See more</summary>
	
	supportedFilesIn: aDirectoryEntry

	| fileTypes supportedFiles |
		fileTypes _ (self class mapping as: Dictionary) values.
		supportedFiles _ Set new.
	
	fileTypes do: [ :type | 
		supportedFiles _ supportedFiles
			union: (aDirectoryEntry files select: [ :entry |
				'*.' , type match: entry name ])
	].

	^ supportedFiles
</details>

## DarkTheme

A low contrast, darker gray theme.

### Methods
#### DarkTheme>>#textHighlight

A nice light blue.


<details>
	<summary>See more</summary>
	
	textHighlight
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^ `Color hue: 204 chroma: 0.29 luminance: 0.22`
</details>

#### DarkTheme>>#unfocusedTextHighlightFrom: aColor

<details>
	<summary>See more</summary>
	
	unfocusedTextHighlightFrom: aColor
	^ aColor adjustSaturation: -0.15 brightness: -0.07
</details>

#### DarkTheme>>#missingCommentTextColor

<details>
	<summary>See more</summary>
	
	missingCommentTextColor

	^ TextColor cyan
</details>

#### DarkTheme>>#shout

Color symbols as an association list.


<details>
	<summary>See more</summary>
	
	shout
	"Color symbols as an association list."

	^ {
		#selector  					-> '#B59F60'.
		#arguments 					-> '#289078'.
		#comment 					-> #(green duller).
		#tempBar 					-> #gray.
		#tempVars 					-> '#767836'.
		#instVar 					-> '#B3695A'.
		#pseudoVariables 			-> '#2070E0'.
		#literals 					-> #green.
		#messages 					-> '#6FB3BD'.
		#incompleteMessages  	-> '#F08060'.
		#blockLevelZero 			-> '#6FB3BD'.
		#blockLevelOne 			-> '#FFB0B0'.
		#blockLevelTwo 			-> '#B0FFB0'.
		#blockLevelThree 			-> '#B0B0FF'.
		#blockLevelFour 			-> '#00B0B0'.
		#blockLevelFive 			-> '#E03030'.
		#blockLevelSix 			-> '#30E030'.
		#blockLevelSeven 			-> '#3030E0'.
		#defaults 					-> '#A1AFBF'.
		#undefined 					-> '#E04020'.
		#methodTags 				-> #green.
	}
</details>

#### DarkTheme>>#menuText

<details>
	<summary>See more</summary>
	
	menuText
	^ self text
</details>

#### DarkTheme>>#paneBackgroundFrom: aColor

<details>
	<summary>See more</summary>
	
	paneBackgroundFrom: aColor
	^ aColor alphaMixed: 0.7 with: Color black
</details>

#### DarkTheme>>#textPane

<details>
	<summary>See more</summary>
	
	textPane
	^ `Color gray`
</details>

#### DarkTheme>>#buttonLabel

<details>
	<summary>See more</summary>
	
	buttonLabel
	^ `Color gray: 0.48`
</details>

#### DarkTheme>>#defaultWindowColor

<details>
	<summary>See more</summary>
	
	defaultWindowColor
	^ `Color hue: 212 chroma: 0.015 luminance: 0.25`
</details>

#### DarkTheme>>#background

<details>
	<summary>See more</summary>
	
	background
	^ `Color black`
</details>

#### DarkTheme>>#text

<details>
	<summary>See more</summary>
	
	text
	^ `Color veryLightGray`
</details>

#### DarkTheme>>#windowLabel

<details>
	<summary>See more</summary>
	
	windowLabel
	^ `Color gray: 0.55`
</details>

#### DarkTheme>>#menu

<details>
	<summary>See more</summary>
	
	menu
	^ `Color darkGray`
</details>

#### DarkTheme>>#useUniformColors

<details>
	<summary>See more</summary>
	
	useUniformColors
	^ true
</details>

## Theme

To set a theme: "Note that some parts of the UI will not update. You may need to reopen your browsers, etc." ThemeName beCurrent. "Actually now you can do this from the appearance menu too!" Themes are collections of methods that affect the way the system presents itself. They can be used to change the color scheme, as well as to modify other look and feel properties (e.g., round versus square window corners, embossed versus solid window labels.) Each of these methods answers a value like true or false, or a Color. Various parts of the system relating primarily to tools and menus query Theme class>>current to determine their configuration. Theme serves as base theme. You can make new color schemes by subclassing it, and you can see what's possible by browsing its methods. Shout's colors are configured using #shout. The method should be fairly self explanatory. The data structure answered by this method is used in #generateShoutConfig to create a large nested array, which is in turn requested by Shout to configure it's color scheme. The default scheme on systems that the present author has encountered thus far use only a subset of shout's full customizability, as does this facade for it's configuration. For more granular control of Shout, you can override #generateShoutConfig to answer the data structure directly. To get a sense of what it looks like, print: Theme new generateShoutConfig. *Be Warned:* If something goes wrong with this method, or the data isn't shaped correctly, weird and/or bad things can happen to Morphic. Be careful. I've supplied a test for it if you do decide to tinker with it. Send your theme #beCurrent in order to initialize Shout after modifying #shout. Icons can now be swapped in and out of the system using ContentPack. The actual system icons are kept in a ContentPack instance that Theme hangs onto in a class variable called Content. Access the data structure with: Theme content. ...and see the comment on ContentPack for more details. You can choose from three icon "moods;" in one almost every menu item has an icon, in another, the usual menu items have icons, and a few have been added, and in a third, icons are not shown, which is nice if you've done a theme using colors that don't work well with the built in icons and don't have a suitable set handy. MenuIcons has been removed from the system. It's duties have been absorbed by Theme and ContentPack. - cbr

### Methods
#### Theme>>#blockLevelTwo

<details>
	<summary>See more</summary>
	
	blockLevelTwo

	^ #(
		blockStart2
		blockEnd2
		leftParenthesis2
		rightParenthesis2
	)

</details>

#### Theme>>#worldIcon

<details>
	<summary>See more</summary>
	
	worldIcon

	^ self fetch: #( '16x16' 'smalltalk' 'world' )

</details>

#### Theme>>#debugIcon

<details>
	<summary>See more</summary>
	
	debugIcon

	^ self fetch: #( '16x16' 'smalltalk' 'debug' )

</details>

#### Theme>>#chatIcon

<details>
	<summary>See more</summary>
	
	chatIcon

	^ self fetch: #( '16x16' 'apps' 'internet-group-chat' )

</details>

#### Theme>>#goTopIcon

<details>
	<summary>See more</summary>
	
	goTopIcon

	^ self fetch: #( '16x16' 'actions' 'go-top' )

</details>

#### Theme>>#closeIcon

<details>
	<summary>See more</summary>
	
	closeIcon

	^ self fetch: #( '14x14' 'buttons' 'close' )

</details>

#### Theme>>#background

<details>
	<summary>See more</summary>
	
	background
	^ `Color r: 0.7 g: 0.72 b: 0.83`
</details>

#### Theme>>#resizeTopLeftIcon

Theme current resizeTopLeftIcon


<details>
	<summary>See more</summary>
	
	resizeTopLeftIcon
	"
	Theme current resizeTopLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-left' )
</details>

#### Theme>>#preferencesDesktopFontIcon

<details>
	<summary>See more</summary>
	
	preferencesDesktopFontIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-font' )

</details>

#### Theme>>#useWindowTitleGradient

<details>
	<summary>See more</summary>
	
	useWindowTitleGradient
	^true
</details>

#### Theme>>#versionsBrowser

<details>
	<summary>See more</summary>
	
	versionsBrowser
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `(Color r: 0.869 g: 0.753 b: 1.0) duller` ]
</details>

#### Theme>>#scrollbarThickness

<details>
	<summary>See more</summary>
	
	scrollbarThickness
	^ Preferences scrollbarThickness
</details>

#### Theme>>#unfocusedTextHighlightFrom: aColor

<details>
	<summary>See more</summary>
	
	unfocusedTextHighlightFrom: aColor
	^aColor adjustSaturation: -0.15 brightness: -0.13
</details>

#### Theme>>#textCursor

<details>
	<summary>See more</summary>
	
	textCursor
	^ Display depth <= 2
		ifTrue: [ `Color black` ]
		ifFalse: [ self text ]
</details>

#### Theme>>#classIcon

<details>
	<summary>See more</summary>
	
	classIcon

	^ self fetch: #( '16x16' 'categories' 'applications-other' )

</details>

#### Theme>>#pushPinIcon

<details>
	<summary>See more</summary>
	
	pushPinIcon

	^ self fetch: #( '16x16' 'smalltalk' 'push-pin' )

</details>

#### Theme>>#systemIcon

<details>
	<summary>See more</summary>
	
	systemIcon

	^ self fetch: #( '16x16' 'categories' 'applications-system' )

</details>

#### Theme>>#newIcon

<details>
	<summary>See more</summary>
	
	newIcon
	
	^ self fetch: #( '16x16' 'actions' 'document-new' )

</details>

#### Theme>>#genericTextIcon

<details>
	<summary>See more</summary>
	
	genericTextIcon

	^ self fetch: #( '16x16' 'mimetypes' 'text-x-generic' )

</details>

#### Theme>>#haloDragIcon

<details>
	<summary>See more</summary>
	
	haloDragIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-drag' )

</details>

#### Theme>>#resizeRightIcon

Theme current resizeRightIcon


<details>
	<summary>See more</summary>
	
	resizeRightIcon
	"
	Theme current resizeRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-right' )
</details>

#### Theme>>#textEmptyDisplayMessage

<details>
	<summary>See more</summary>
	
	textEmptyDisplayMessage
	^ `Color veryLightGray`
</details>

#### Theme>>#titleGradientBottomFactor

Will only be used for color themes that answer true to #useWindowTitleGradient


<details>
	<summary>See more</summary>
	
	titleGradientBottomFactor
	"Will only be used for color themes that answer true to #useWindowTitleGradient"
	^0.85
</details>

#### Theme>>#steButtons

<details>
	<summary>See more</summary>
	
	steButtons
	^false
</details>

#### Theme>>#roundedButtonRadius

Only effective if #roundButtons answers true. Provide a reasonable default for subclasses.


<details>
	<summary>See more</summary>
	
	roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^8
</details>

#### Theme>>#copyIcon

<details>
	<summary>See more</summary>
	
	copyIcon

	^ self fetch: #( '16x16' 'actions' 'edit-copy' )

</details>

#### Theme>>#speadsheetTemplateIcon

<details>
	<summary>See more</summary>
	
	speadsheetTemplateIcon

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-spreadsheet-template' )

</details>

#### Theme>>#developmentIcon

<details>
	<summary>See more</summary>
	
	developmentIcon
	
	^ self fetch: #( '16x16' 'categories' 'applications-development' )

</details>

#### Theme>>#resizeFullIcon

Theme current resizeFullIcon


<details>
	<summary>See more</summary>
	
	resizeFullIcon
	"
	Theme current resizeFullIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize' )
</details>

#### Theme>>#clockIcon

<details>
	<summary>See more</summary>
	
	clockIcon

	^ self fetch: #( '16x16' 'actions' 'appointment-new' )

</details>

#### Theme>>#pseudoVariables

<details>
	<summary>See more</summary>
	
	pseudoVariables

	^ ClassBuilder reservedNames 
</details>

#### Theme>>#resizeBottomIcon

Theme current resizeBottomIcon


<details>
	<summary>See more</summary>
	
	resizeBottomIcon
	"
	Theme current resizeBottomIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom' )
</details>

#### Theme>>#printerIcon

<details>
	<summary>See more</summary>
	
	printerIcon

	^ self fetch: #( '16x16' 'devices' 'printer' )

</details>

#### Theme>>#findIcon

<details>
	<summary>See more</summary>
	
	findIcon

	^ self fetch: #( '16x16' 'actions' 'edit-find' )

</details>

#### Theme>>#systemMonitorIcon

<details>
	<summary>See more</summary>
	
	systemMonitorIcon

	^ self fetch: #( '16x16' 'apps' 'utilities-system-monitor' )

</details>

#### Theme>>#minimalWindows

<details>
	<summary>See more</summary>
	
	minimalWindows
	^ false
</details>

#### Theme>>#windowIcon

<details>
	<summary>See more</summary>
	
	windowIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-system-windows'  )

</details>

#### Theme>>#mailMarkJunkIcon

<details>
	<summary>See more</summary>
	
	mailMarkJunkIcon

	^ self fetch: #( '16x16' 'actions' 'mail-mark-junk' )

</details>

#### Theme>>#resizeBottomLeftIcon

Theme current resizeBottomLeftIcon


<details>
	<summary>See more</summary>
	
	resizeBottomLeftIcon
	"
	Theme current resizeBottomLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-left' )
</details>

#### Theme>>#formatJustifyRightIcon

<details>
	<summary>See more</summary>
	
	formatJustifyRightIcon

	^ self fetch: #( '16x16' 'actions' 'format-justify-right' )

</details>

#### Theme>>#undefined

<details>
	<summary>See more</summary>
	
	undefined

	^ #(
		invalid
		excessCode
		#'$'
		undefinedKeyword
		undefinedBinary
		undefinedUnary
	)
</details>

#### Theme>>#haloColorIcon

<details>
	<summary>See more</summary>
	
	haloColorIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-color' )

</details>

#### Theme>>#formatJustifyLeftIcon

<details>
	<summary>See more</summary>
	
	formatJustifyLeftIcon

	^ self fetch: #( '16x16' 'actions' 'format-justify-left' )

</details>

#### Theme>>#browser

<details>
	<summary>See more</summary>
	
	browser
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.5 g: 0.7 b: 0.4`]
</details>

#### Theme>>#undoIcon

<details>
	<summary>See more</summary>
	
	undoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-undo' )

</details>

#### Theme>>#viewFullscreenIcon

<details>
	<summary>See more</summary>
	
	viewFullscreenIcon

	^ self fetch: #( '16x16' 'actions' 'view-fullscreen' )

</details>

#### Theme>>#testRunner

<details>
	<summary>See more</summary>
	
	testRunner
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `(Color r: 0.650 g: 0.753 b: 0.976) duller` ]
</details>

#### Theme>>#weatherFewCloudsIcon

<details>
	<summary>See more</summary>
	
	weatherFewCloudsIcon

	^ self fetch: #( '16x16' 'status' 'weather-few-clouds' )

</details>

#### Theme>>#prepend: aTheme toContentSpec: anArray

<details>
	<summary>See more</summary>
	
	prepend: aTheme toContentSpec: anArray

	^ { aTheme printString}, anArray
</details>

#### Theme>>#morphsIcon

<details>
	<summary>See more</summary>
	
	morphsIcon
	
	^ self fetch: #( '16x16' 'smalltalk' 'morphs' )

</details>

#### Theme>>#blockTemps

<details>
	<summary>See more</summary>
	
	blockTemps

	^ #(
		blockTempVar
		blockPatternTempVar
	)
</details>

#### Theme>>#goUpIcon

<details>
	<summary>See more</summary>
	
	goUpIcon

	^ self fetch: #( '16x16' 'actions' 'go-up' )

</details>

#### Theme>>#dualChangeSorter

<details>
	<summary>See more</summary>
	
	dualChangeSorter
	^ self changeSorter
</details>

#### Theme>>#deleteIcon

<details>
	<summary>See more</summary>
	
	deleteIcon

	^ self fetch: #( '16x16' 'places' 'user-trash' )

</details>

#### Theme>>#missingCommentTextColor

<details>
	<summary>See more</summary>
	
	missingCommentTextColor

	^ TextColor red
</details>

#### Theme>>#listRemoveIcon

<details>
	<summary>See more</summary>
	
	listRemoveIcon

	^ self fetch: #( '16x16' 'actions' 'list-remove' )

</details>

#### Theme>>#exploreIcon

<details>
	<summary>See more</summary>
	
	exploreIcon

	^ self fetch: #( '16x16' 'actions' 'document-properties' )

</details>

#### Theme>>#inspectIcon

<details>
	<summary>See more</summary>
	
	inspectIcon
	
	^ self fetch: #( '16x16' 'actions' 'system-search' )

</details>

#### Theme>>#literals

<details>
	<summary>See more</summary>
	
	literals

	^ #(
			character
			integer
			number
			-
	)
</details>

#### Theme>>#redoIcon

<details>
	<summary>See more</summary>
	
	redoIcon

	^ self fetch: #( '16x16' 'actions' 'edit-redo' )

</details>

#### Theme>>#resizeTopRightIcon

Theme current resizeTopRightIcon


<details>
	<summary>See more</summary>
	
	resizeTopRightIcon
	"
	Theme current resizeTopRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top-right' )
</details>

#### Theme>>#terminalIcon

<details>
	<summary>See more</summary>
	
	terminalIcon

	^ self fetch: #( '16x16' 'apps' 'utilities-terminal' )
</details>

#### Theme>>#quitIcon

<details>
	<summary>See more</summary>
	
	quitIcon

	^ self fetch: #( '16x16' 'actions' 'system-log-out' )

</details>

#### Theme>>#addressBookIcon

<details>
	<summary>See more</summary>
	
	addressBookIcon

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-address-book' )

</details>

#### Theme>>#systemFileManagerIcon

<details>
	<summary>See more</summary>
	
	systemFileManagerIcon

	^ self fetch: #( '16x16' 'apps' 'system-file-manager' )

</details>

#### Theme>>#removableMediaIcon

<details>
	<summary>See more</summary>
	
	removableMediaIcon

	^ self fetch: #( '16x16' 'devices' 'drive-removable-media' )

</details>

#### Theme>>#blockLevelFive

<details>
	<summary>See more</summary>
	
	blockLevelFive

	^ #(
		leftParenthesis5
		rightParenthesis5
		blockStart5
		blockEnd5
	)
</details>

#### Theme>>#assignment

#bold


<details>
	<summary>See more</summary>
	
	assignment
	"#bold"

	^ #(
		assignment
	)
</details>

#### Theme>>#globals

<details>
	<summary>See more</summary>
	
	globals

	^ #(
		primitiveOrExternalCallStart
		primitiveOrExternalCallEnd
		globalVar
		workspaceVar
	)
</details>

#### Theme>>#acceptButton

<details>
	<summary>See more</summary>
	
	acceptButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ `Color r: 0.2 g: 0.6 b: 0.1` ])
</details>

#### Theme>>#textHighlight

A nice light blue.


<details>
	<summary>See more</summary>
	
	textHighlight
	"A nice light blue."
	"
	^ Color r: 0.71 g: 0.835 b: 1.0
	^ Color hue: 214 chroma: 0.29 luminance: 0.816
	"
	^ `Color hue: 204 chroma: 0.29 luminance: 0.77`
</details>

#### Theme>>#haloScaleIcon

<details>
	<summary>See more</summary>
	
	haloScaleIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-scale' )

</details>

#### Theme>>#resizeTopIcon

Theme current resizeTopIcon


<details>
	<summary>See more</summary>
	
	resizeTopIcon
	"
	Theme current resizeTopIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-top' )
</details>

#### Theme>>#fileOutIcon

<details>
	<summary>See more</summary>
	
	fileOutIcon

	^ self fetch: #( '16x16' 'actions' 'document-print' )

</details>

#### Theme>>#pasteIcon

<details>
	<summary>See more</summary>
	
	pasteIcon

	^ self fetch: #( '16x16' 'actions' 'edit-paste' )

</details>

#### Theme>>#debugger

<details>
	<summary>See more</summary>
	
	debugger
	^ `Color h: 0.0 s: 0.6 v: 0.7`
</details>

#### Theme>>#fontXGenericIcon

<details>
	<summary>See more</summary>
	
	fontXGenericIcon

	^ self fetch: #( '16x16' 'mimetypes' 'font-x-generic' )

</details>

#### Theme>>#textEditor

<details>
	<summary>See more</summary>
	
	textEditor
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color gray: 0.6` ]
</details>

#### Theme>>#stillCameraIcon

<details>
	<summary>See more</summary>
	
	stillCameraIcon

	^ self fetch: #( '16x16' 'devices' 'camera-photo' )

</details>

#### Theme>>#menuTitleBar

<details>
	<summary>See more</summary>
	
	menuTitleBar
	Display depth = 1 ifTrue: [^ `Color white`].
	Display depth = 2 ifTrue: [^ `Color gray`].
	^ self menu darker
</details>

#### Theme>>#blockLevelSix

<details>
	<summary>See more</summary>
	
	blockLevelSix

	^ #(
		leftParenthesis6
		rightParenthesis6
		blockStart6
		blockEnd6
	)

</details>

#### Theme>>#scriptIcon

<details>
	<summary>See more</summary>
	
	scriptIcon

	^ self fetch: #( '16x16' 'mimetypes' 'text-x-script' )

</details>

#### Theme>>#resizeLeftIcon

Theme current resizeLeftIcon


<details>
	<summary>See more</summary>
	
	resizeLeftIcon
	"
	Theme current resizeLeftIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-left' )
</details>

#### Theme>>#haloDuplicateIcon

<details>
	<summary>See more</summary>
	
	haloDuplicateIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-duplicate' )

</details>

#### Theme>>#cutIcon

<details>
	<summary>See more</summary>
	
	cutIcon

	^ self fetch: #( '16x16' 'actions' 'edit-cut' )

</details>

#### Theme>>#transcript

<details>
	<summary>See more</summary>
	
	transcript
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.8 g: 0.6 b: 0.3` ]
</details>

#### Theme>>#goBottomIcon

<details>
	<summary>See more</summary>
	
	goBottomIcon

	^ self fetch: #( '16x16' 'actions' 'go-bottom' )

</details>

#### Theme>>#helpIcon

<details>
	<summary>See more</summary>
	
	helpIcon

	^ self fetch: #( '16x16' 'apps' 'help-browser' )

</details>

#### Theme>>#formatJustifyCenterIcon

<details>
	<summary>See more</summary>
	
	formatJustifyCenterIcon

	^ self fetch: #( '16x16' 'actions' 'format-justify-center' )

</details>

#### Theme>>#resizeBottomRightIcon

Theme current resizeBottomRightIcon


<details>
	<summary>See more</summary>
	
	resizeBottomRightIcon
	"
	Theme current resizeBottomRightIcon
	"
	^ self fetch: #( '16x16' 'resize' 'resize-bottom-right' )
</details>

#### Theme>>#cancelIcon

<details>
	<summary>See more</summary>
	
	cancelIcon

	^ self fetch: #( '16x16' 'smalltalk' 'cancel' )

</details>

#### Theme>>#haloGrabIcon

<details>
	<summary>See more</summary>
	
	haloGrabIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-grab' )

</details>

#### Theme>>#buttonGradientHeight

Only effective if #roundButtons answers true. Provide a reasonable default for subclasses.


<details>
	<summary>See more</summary>
	
	buttonGradientHeight
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^14
</details>

#### Theme>>#blockLevelFour

<details>
	<summary>See more</summary>
	
	blockLevelFour

	^ #(
		blockStart4 
		blockEnd4
		leftParenthesis4
		rightParenthesis4
	)

</details>

#### Theme>>#embossedTitles

<details>
	<summary>See more</summary>
	
	embossedTitles
	^true
</details>

#### Theme>>#scrollbarSliderShadowColor

<details>
	<summary>See more</summary>
	
	scrollbarSliderShadowColor
	^ `Color white`
</details>

#### Theme>>#successColor

<details>
	<summary>See more</summary>
	
	successColor
	^ `Color green lighter`
</details>

#### Theme>>#changesIcon

<details>
	<summary>See more</summary>
	
	changesIcon

	^ self fetch: #( '16x16' 'actions' 'view-refresh')

</details>

#### Theme>>#saveAndQuitIcon

<details>
	<summary>See more</summary>
	
	saveAndQuitIcon

	^ self fetch: #( '16x16' 'smalltalk' 'save-and-quit' )

</details>

#### Theme>>#roundWindowCorners

<details>
	<summary>See more</summary>
	
	roundWindowCorners
	^true
</details>

#### Theme>>#fullScreenDeskMargin

<details>
	<summary>See more</summary>
	
	fullScreenDeskMargin
	^ Preferences fullScreenLeavesDeskMargins ifTrue: [22] ifFalse: [0]
</details>

#### Theme>>#symbols

<details>
	<summary>See more</summary>
	
	symbols
	^ #(
		symbol
		stringSymbol
		literalArray
	)
</details>

#### Theme>>#haloHelpIcon

<details>
	<summary>See more</summary>
	
	haloHelpIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-help' )

</details>

#### Theme>>#listMultiHighlightFocused: aBoolean

<details>
	<summary>See more</summary>
	
	listMultiHighlightFocused: aBoolean
	^ (self listHighlightFocused: aBoolean) 
		adjustSaturation: 0.0 brightness: -0.08
</details>

#### Theme>>#blockLevelZero

<details>
	<summary>See more</summary>
	
	blockLevelZero

	^ #(
		blockStart
		blockEnd
		leftParenthesis
		rightParenthesis
	)
</details>

#### Theme>>#buttonGradientBottomFactor

Will only be used for color themes that answer true to #roundButtons


<details>
	<summary>See more</summary>
	
	buttonGradientBottomFactor
	"Will only be used for color themes that answer true to #roundButtons"
	^0.92
</details>

#### Theme>>#instanceIcon

<details>
	<summary>See more</summary>
	
	instanceIcon

	^ self fetch: #( '16x16' 'mimetypes' 'application-x-executable' )

</details>

#### Theme>>#useUniformColors

<details>
	<summary>See more</summary>
	
	useUniformColors
	^false
</details>

#### Theme>>#argumentTypes

<details>
	<summary>See more</summary>
	
	argumentTypes

	^ #(
		patternArg
		methodArg
		blockPatternArg
		blockArg
		argument
	)
</details>

#### Theme>>#blockLevelOne

<details>
	<summary>See more</summary>
	
	blockLevelOne

	^ #(
		blockStart1
		blockEnd1
		leftParenthesis1
		rightParenthesis1
	)
</details>

#### Theme>>#haloDismissIcon

<details>
	<summary>See more</summary>
	
	haloDismissIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-dismiss' )

</details>

#### Theme>>#scrollbarButtonColor

<details>
	<summary>See more</summary>
	
	scrollbarButtonColor
	^ `Color gray: 0.95`
</details>

#### Theme>>#object

<details>
	<summary>See more</summary>
	
	object
	^ `Color white duller`
</details>

#### Theme>>#sendReceiveIcon

<details>
	<summary>See more</summary>
	
	sendReceiveIcon

	^ self fetch: #( '16x16' 'actions' 'mail-send-receive' )

</details>

#### Theme>>#cancelButton

<details>
	<summary>See more</summary>
	
	cancelButton

	^ self buttonColorFrom: 
		(self useUniformColors
			ifTrue: [ self defaultWindowColor ]
			ifFalse: [ `Color r: 0.8 g: 0.2 b: 0.2` ])
</details>

#### Theme>>#scrollbarColor

<details>
	<summary>See more</summary>
	
	scrollbarColor
	^ `Color white`
</details>

#### Theme>>#packageList

<details>
	<summary>See more</summary>
	
	packageList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.63 g: 0.47 b: 0.08` ]
</details>

#### Theme>>#methodTags

<details>
	<summary>See more</summary>
	
	methodTags

	^ #(
		primitive
		pragmaKeyword
		pragmaUnary
		pragmaBinary
		externalFunctionCallingConvention
		module
	)
</details>

#### Theme>>#switchIcon

<details>
	<summary>See more</summary>
	
	switchIcon

	^ self fetch: #( '16x16' 'actions' 'system-shutdown' )

</details>

#### Theme>>#messageSet

<details>
	<summary>See more</summary>
	
	messageSet
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.45 g: 0.6 b: 0.85` ]
</details>

#### Theme>>#buttonGradientTopFactor

Will only be used for color themes that answer true to #roundButtons


<details>
	<summary>See more</summary>
	
	buttonGradientTopFactor
	"Will only be used for color themes that answer true to #roundButtons"
	^1.0
</details>

#### Theme>>#keyboardShortcutsIcon

<details>
	<summary>See more</summary>
	
	keyboardShortcutsIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-keyboard-shortcuts' )

</details>

#### Theme>>#formatJustifyFillIcon

<details>
	<summary>See more</summary>
	
	formatJustifyFillIcon

	^ self fetch: #( '16x16' 'actions' 'format-justify-fill' )

</details>

#### Theme>>#newFolderIcon

<details>
	<summary>See more</summary>
	
	newFolderIcon

	^ self fetch: #( '16x16' 'actions' 'folder-new' )

</details>

#### Theme>>#doItIcon

<details>
	<summary>See more</summary>
	
	doItIcon

	^ self fetch: #( '16x16' 'smalltalk' 'do-it' )
</details>

#### Theme>>#fileList

<details>
	<summary>See more</summary>
	
	fileList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.7 g: 0.55 b: 0.7` ]
</details>

#### Theme>>#newWindowIcon

<details>
	<summary>See more</summary>
	
	newWindowIcon

	^ self fetch: #( '16x16' 'actions' 'window-new' )

</details>

#### Theme>>#windowClosed: aSystemWindow

<details>
	<summary>See more</summary>
	
	windowClosed: aSystemWindow
	^ self
</details>

#### Theme>>#defaultWindowColor

<details>
	<summary>See more</summary>
	
	defaultWindowColor
	^ `Color lightGray`
</details>

#### Theme>>#spreadsheetIcon

<details>
	<summary>See more</summary>
	
	spreadsheetIcon

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-spreadsheet' )

</details>

#### Theme>>#titleGradientTopFactor

Will only be used for color themes that answer true to #useWindowTitleGradient


<details>
	<summary>See more</summary>
	
	titleGradientTopFactor
	"Will only be used for color themes that answer true to #useWindowTitleGradient"
	^1.0
</details>

#### Theme>>#graphicsIcon

<details>
	<summary>See more</summary>
	
	graphicsIcon

	^ self fetch: #( '16x16' 'categories' 'applications-graphics' )

</details>

#### Theme>>#saveAsIcon

<details>
	<summary>See more</summary>
	
	saveAsIcon

	^ self fetch: #( '16x16' 'actions' 'document-save-as' )

</details>

#### Theme>>#menu

<details>
	<summary>See more</summary>
	
	menu
	Display depth <= 2 ifTrue: [^ `Color white` ].
	^ `Color r: 0.75 g: 0.75 b: 0.75 alpha: 0.93`
</details>

#### Theme>>#saveIcon

<details>
	<summary>See more</summary>
	
	saveIcon

	^ self fetch: #( '16x16' 'actions' 'document-save' )

</details>

#### Theme>>#ansiAssignment

#bold


<details>
	<summary>See more</summary>
	
	ansiAssignment
	"#bold"

	^ #(
		ansiAssignment
	)
</details>

#### Theme>>#emblemImportantIcon

<details>
	<summary>See more</summary>
	
	emblemImportantIcon

	^ self fetch: #( '16x16' 'emblems' 'emblem-important' )
</details>

#### Theme>>#exitFullscreenIcon

<details>
	<summary>See more</summary>
	
	exitFullscreenIcon

	^ self fetch: #( '16x16' 'smalltalk' 'exit-fullscreen' )

</details>

#### Theme>>#layoutAdjusterThickness

<details>
	<summary>See more</summary>
	
	layoutAdjusterThickness
	^ 3
</details>

#### Theme>>#warningIcon

<details>
	<summary>See more</summary>
	
	warningIcon

	^ self fetch: #( '16x16' 'status' 'dialog-warning' )
</details>

#### Theme>>#blockLevelThree

<details>
	<summary>See more</summary>
	
	blockLevelThree

	^ #(
		blockStart3
		blockEnd3
		leftParenthesis3
		rightParenthesis3
	)

</details>

#### Theme>>#selectAllIcon

<details>
	<summary>See more</summary>
	
	selectAllIcon

	^ self fetch: #( '16x16' 'actions' 'edit-select-all' )

</details>

#### Theme>>#textPane

<details>
	<summary>See more</summary>
	
	textPane
	^ `Color white`
</details>

#### Theme>>#blockLevelSeven

<details>
	<summary>See more</summary>
	
	blockLevelSeven

	^ #(
		leftParenthesis7
		rightParenthesis7
		blockStart7
		blockEnd7
	)
</details>

#### Theme>>#buttonLabel

<details>
	<summary>See more</summary>
	
	buttonLabel
	^ `Color gray: 0.18`
</details>

#### Theme>>#italic

<details>
	<summary>See more</summary>
	
	italic
	^Preferences italicsInShout
		ifTrue: [ #italic ]
		ifFalse: [ #normal ]
</details>

#### Theme>>#haloDebugIcon

<details>
	<summary>See more</summary>
	
	haloDebugIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-debug' )

</details>

#### Theme>>#editFindReplaceIcon

<details>
	<summary>See more</summary>
	
	editFindReplaceIcon

	^ self fetch: #( '16x16' 'actions' 'edit-find-replace' )

</details>

#### Theme>>#appendExtensionToContentSpec: anArray

<details>
	<summary>See more</summary>
	
	appendExtensionToContentSpec: anArray

	^ ((anArray asOrderedCollection 
			copyReplaceFrom: 3 
			to: 3 
			with: 
				{ (anArray at: 3) , '.png' } ))
</details>

#### Theme>>#windowMenuIcon

<details>
	<summary>See more</summary>
	
	windowMenuIcon

	^ self fetch: #( '14x14' 'buttons' 'window-menu' )

</details>

#### Theme>>#embossedButtonLabels

Currently only apply to rounded buttons!


<details>
	<summary>See more</summary>
	
	embossedButtonLabels
	"Currently only apply to rounded buttons!"

	^true
</details>

#### Theme>>#tempBar

<details>
	<summary>See more</summary>
	
	tempBar
	
	^ #(
		methodTempBar
		blockTempBar
		blockArgsBar
	)
</details>

#### Theme>>#usersIcon

<details>
	<summary>See more</summary>
	
	usersIcon

	^ self fetch: #( '16x16' 'apps' 'system-users' )

</details>

#### Theme>>#shout

Color symbols as an association list. SHTextStylerST80 initialize


<details>
	<summary>See more</summary>
	
	shout
	"Color symbols as an association list.
	SHTextStylerST80 initialize
	"
	^ {
		#selector  					-> nil.
		#arguments 					-> #(cyan muchDarker).
		#comment 					-> #(green muchDarker).
		#tempBar 					-> #gray.
		#tempVars 					-> #(gray muchDarker).
		#instVar 					-> #(magenta muchDarker).
		#pseudoVariables 			-> #(red muchDarker).
		#literals 					-> #(green muchDarker).
		#messages 					-> #(blue darker).
		#incompleteMessages 		-> #(gray veryMuchDarker).
		#blockLevelZero 			-> #black.
		#blockLevelOne 			-> #brown.
		#blockLevelTwo 			-> #magenta.
		#blockLevelThree 			-> #red.
		#blockLevelFour 			-> #(orange darker).
		#blockLevelFive 			-> #(orange muchDarker).
		#blockLevelSix 			-> #(green muchDarker).
		#blockLevelSeven 			-> #blue.
		#defaults 					-> #black.
		#undefined 					-> #red.
		#methodTags 				-> #(green muchDarker).
	}
</details>

#### Theme>>#useButtonGradient

<details>
	<summary>See more</summary>
	
	useButtonGradient
	^false
</details>

#### Theme>>#haloFontSizeIcon

<details>
	<summary>See more</summary>
	
	haloFontSizeIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-font-size' )

</details>

#### Theme>>#mailForwardIcon

<details>
	<summary>See more</summary>
	
	mailForwardIcon
	
	^ self fetch: #( '16x16' 'actions' 'mail-forward' )

</details>

#### Theme>>#printIcon

<details>
	<summary>See more</summary>
	
	printIcon

	^ self fetch: #( '16x16' 'actions' 'document-print-preview' )

</details>

#### Theme>>#messages

<details>
	<summary>See more</summary>
	
	messages

	^ #(
		keyword
		binary
		unary
	)
</details>

#### Theme>>#packageIcon

<details>
	<summary>See more</summary>
	
	packageIcon

	^ self fetch: #( '16x16' 'mimetypes' 'package-x-generic' )
</details>

#### Theme>>#haloRotateIcon

<details>
	<summary>See more</summary>
	
	haloRotateIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-rotate' )

</details>

#### Theme>>#focusIndicator

<details>
	<summary>See more</summary>
	
	focusIndicator
	^ self textHighlight
</details>

#### Theme>>#appearanceIcon

<details>
	<summary>See more</summary>
	
	appearanceIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-theme' )

</details>

#### Theme>>#generateShoutConfig

<details>
	<summary>See more</summary>
	
	generateShoutConfig

	| styles colors |
	styles := OrderedCollection new.
	colors := self shout as: Dictionary.

	{
		{self undefined. colors at: #undefined}.
		{self defaults . colors at: #defaults}.
		{self pseudoVariables . colors at: #pseudoVariables}.
		{self literals . colors at: #literals}.
		{self instVar . colors at: #instVar}.
		{self messages . colors at: #messages}.
		{self blockLevelZero . colors at: #blockLevelZero}.
		{self blockLevelOne . colors at: #blockLevelOne}.
		{self blockLevelTwo . colors at: #blockLevelTwo}.
		{self blockLevelThree . colors at: #blockLevelThree}.
		{self blockLevelFour . colors at: #blockLevelFour}.
		{self blockLevelFive . colors at: #blockLevelFive}.
		{self blockLevelSix . colors at: #blockLevelSix}.
		{self blockLevelSeven . colors at: #blockLevelSeven}.
		{self tempBar . colors at: #tempBar}.
		{self methodTags . colors at: #methodTags . #bold}.
		{self globals . colors at: #defaults . #bold}.
		{self incompleteMessages . colors at: #incompleteMessages . #underlined}.
		{self argumentTypes . colors at: #arguments . self italic}.
		{self symbols . colors at: #messages . #bold}.
		{self pattern . colors at: #selector . #bold}.
		{self ansiAssignment . nil . #bold}.
		{self assignment . nil . #(#bold #withST80Glyphs)}.
		{self return . nil . #(#bold #withST80Glyphs)}.
		{self tempVars . colors at: #tempVars . self italic}.
		{self blockTemps . colors at: #tempBar . self italic}
	} do: [ :style |
		styles addAll:
			(style first
				collect: [ :category | | elements |
					elements _ style asOrderedCollection.
					elements at: 1 put: category.
					Array withAll: elements ])].

	"Miscellaneous remainder after factoring out commonality:"
	styles addAll: {
		{#unfinishedString . colors at: #undefined . #normal}.
		{#undefinedIdentifier . colors at: #undefined .#bold}.
		{#unfinishedComment . colors at: #pseudoVariables . self italic}.
		{#comment . colors at: #comment . self italic}.
		{#string . colors at: #instVar . #normal}.
		{#literal . nil . self italic}.
		{#incompleteIdentifier . colors at: #tempVars . {#italic. #underlined}}.
		{#classVar . colors at: #tempVars . #bold}.
	}.

	^ styles
</details>

#### Theme>>#haloCollapseIcon

<details>
	<summary>See more</summary>
	
	haloCollapseIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-collapse' )

</details>

#### Theme>>#buttonColorFrom: aColor

<details>
	<summary>See more</summary>
	
	buttonColorFrom: aColor
	^ Display depth <= 8
			ifTrue: [ `Color transparent` ]
			ifFalse: [ aColor paler ]
</details>

#### Theme>>#preferencesIcon

<details>
	<summary>See more</summary>
	
	preferencesIcon

	^ self fetch: #( '16x16' 'categories' 'preferences-system' )

</details>

#### Theme>>#menuHighlight

<details>
	<summary>See more</summary>
	
	menuHighlight
	^ Display depth < 8
		ifTrue: [ `Color veryLightGray` ]
		ifFalse: [ self textHighlight ]
</details>

#### Theme>>#halfRefreshIcon

<details>
	<summary>See more</summary>
	
	halfRefreshIcon

	^ self fetch: #( '16x16' 'smalltalk' 'half-refresh' )

</details>

#### Theme>>#tempVars

<details>
	<summary>See more</summary>
	
	tempVars

	^ #(
		tempVar
		patternTempVar
		poolConstant
	)
</details>

#### Theme>>#titleGradientExtraLightness

Will only be used for color themes that answer true to #useWindowTitleGradient


<details>
	<summary>See more</summary>
	
	titleGradientExtraLightness
	"Will only be used for color themes that answer true to #useWindowTitleGradient"
	"To compensate for the darkening effect of the gradient, if desired
	Default value is 1.0 / #titleGradientBottomFactor, so bottom of gradient is original title color,
	and top of gradient is a lighter shade."
	^1.1765
</details>

#### Theme>>#blankIcon

<details>
	<summary>See more</summary>
	
	blankIcon

	^ self fetch: #( '16x16' 'smalltalk' 'blank' )

</details>

#### Theme>>#wallpaperIcon

<details>
	<summary>See more</summary>
	
	wallpaperIcon

	^ self fetch: #( '16x16' 'apps' 'preferences-desktop-wallpaper' )

</details>

#### Theme>>#defaults

<details>
	<summary>See more</summary>
	
	defaults

	^ #(
		default
		arrayStart
		arrayEnd
		arrayStart1
		arrayEnd1
		leftBrace
		rightBrace
		cascadeSeparator
		chainSeparator
		statementSeparator
		externalCallType
		externalCallTypePointerIndicator
		blockArgColon
	)
</details>

#### Theme>>#openIcon

<details>
	<summary>See more</summary>
	
	openIcon

	^ self fetch: #( '16x16' 'actions' 'document-open' )

</details>

#### Theme>>#windowOpen: aSystemWindow

<details>
	<summary>See more</summary>
	
	windowOpen: aSystemWindow
	^ self
</details>

#### Theme>>#incompleteMessages

<details>
	<summary>See more</summary>
	
	incompleteMessages

	^ #(
		incompleteKeyword
		incompleteBinary
		incompleteUnary
	)
</details>

#### Theme>>#return

<details>
	<summary>See more</summary>
	
	return

	^ #(
		#return
	)
</details>

#### Theme>>#globeIcon

<details>
	<summary>See more</summary>
	
	globeIcon

	^ self fetch: #( '16x16' 'apps' 'internet-web-browser' )

</details>

#### Theme>>#instVar

<details>
	<summary>See more</summary>
	
	instVar
	^ #(
		instVar
	)
</details>

#### Theme>>#textHighlightFocused: focused

A nice light blue.


<details>
	<summary>See more</summary>
	
	textHighlightFocused: focused
	"A nice light blue."
	| textHighlight |
	Display depth = 1 ifTrue: [^ `Color veryLightGray` ].
	Display depth = 2 ifTrue: [^ `Color gray: 0.87` ].
	textHighlight _ self textHighlight.
	^focused
		ifTrue: [ textHighlight ]
		ifFalse: [ self unfocusedTextHighlightFrom: textHighlight ]
</details>

#### Theme>>#useTaskbar

<details>
	<summary>See more</summary>
	
	useTaskbar
	^true
</details>

#### Theme>>#listAddIcon

<details>
	<summary>See more</summary>
	
	listAddIcon

	^ self fetch: #( '16x16' 'actions' 'list-add' )

</details>

#### Theme>>#saveAsNewVersionIcon

<details>
	<summary>See more</summary>
	
	saveAsNewVersionIcon

	^ self fetch: #( '16x16' 'smalltalk' 'save-as-new-version' )

</details>

#### Theme>>#listHighlightFocused: aBoolean

<details>
	<summary>See more</summary>
	
	listHighlightFocused: aBoolean
	^ self textHighlightFocused: aBoolean
</details>

#### Theme>>#acceptIcon

#( 'resolution' 'context' 'filename' )


<details>
	<summary>See more</summary>
	
	acceptIcon

			"	#( 'resolution' 'context' 'filename' )	"
	^ self fetch: #( '16x16' 'smalltalk' 'accept' )
</details>

#### Theme>>#line

<details>
	<summary>See more</summary>
	
	line
	^ self textHighlight darker
</details>

#### Theme>>#buttonPaneHeight

Answer the user's preferred default height for button panes.


<details>
	<summary>See more</summary>
	
	buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^Preferences standardButtonFont lineSpacing * 14 // 8
</details>

#### Theme>>#textEditorIcon

<details>
	<summary>See more</summary>
	
	textEditorIcon

	^ self fetch: #( '16x16' 'apps' 'accessories-text-editor' )

</details>

#### Theme>>#errorColor

<details>
	<summary>See more</summary>
	
	errorColor
	^ `Color red lighter`
</details>

#### Theme>>#menuText

<details>
	<summary>See more</summary>
	
	menuText
	^ `Color black`
</details>

#### Theme>>#paneBackgroundFrom: aColor

<details>
	<summary>See more</summary>
	
	paneBackgroundFrom: aColor
	^ aColor veryMuchLighter
</details>

#### Theme>>#changeList

<details>
	<summary>See more</summary>
	
	changeList
	^self messageSet
</details>

#### Theme>>#junkIcon

<details>
	<summary>See more</summary>
	
	junkIcon

	^ self fetch: #( '16x16' 'actions' 'mail-mark-junk' )

</details>

#### Theme>>#haloMenuIcon

<details>
	<summary>See more</summary>
	
	haloMenuIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-menu' )

</details>

#### Theme>>#expandIcon

<details>
	<summary>See more</summary>
	
	expandIcon

	^ self fetch: #( '14x14' 'buttons' 'expand' )

</details>

#### Theme>>#messageNames

<details>
	<summary>See more</summary>
	
	messageNames

	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.53 g: 0.77 b: 0.382` ]
</details>

#### Theme>>#roundButtons

<details>
	<summary>See more</summary>
	
	roundButtons
	^true
</details>

#### Theme>>#goDownIcon

<details>
	<summary>See more</summary>
	
	goDownIcon
	
	^ self fetch: #( '16x16' 'actions' 'go-down' )

</details>

#### Theme>>#updateIcon

<details>
	<summary>See more</summary>
	
	updateIcon

	^ self fetch: #( '16x16' 'status' 'software-update-available' )

</details>

#### Theme>>#fetch: aTuple

#( 'resolution' 'context' 'filename' )


<details>
	<summary>See more</summary>
	
	fetch: aTuple "	#( 'resolution' 'context' 'filename' )	"

	"Get an icon from Content. See icons protocol."

	| contentSpecifier icon themeGuess |
	
	icon _ nil.
	themeGuess _ self class.
	contentSpecifier _ self appendExtensionToContentSpec: aTuple.

	[ icon isNil ] 
		whileTrue: [
			icon _ self class content
				from: themeGuess name
				get: contentSpecifier.
	
			icon ifNotNil: [ ^ icon ].
	
			themeGuess = Theme
				ifTrue: [ ^ nil "See comment in ContentPack>>from:get: --cbr" ].
	
			themeGuess _ themeGuess superclass
		]
</details>

#### Theme>>#haloFontEmphasisIcon

<details>
	<summary>See more</summary>
	
	haloFontEmphasisIcon

	^ self fetch: #( '16x16' 'smalltalk' 'halo-font-emphasis' )

</details>

#### Theme>>#failureColor

<details>
	<summary>See more</summary>
	
	failureColor
	^ `Color yellow lighter`
</details>

#### Theme>>#roundedWindowRadius

Only effective if #roundWindowCorners answers true. Provide a reasonable default for subclasses.


<details>
	<summary>See more</summary>
	
	roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^8
</details>

#### Theme>>#mediaPlaybackStartIcon

<details>
	<summary>See more</summary>
	
	mediaPlaybackStartIcon

	^ self fetch: #( '16x16' 'actions' 'media-playback-start' )

</details>

#### Theme>>#collapseIcon

<details>
	<summary>See more</summary>
	
	collapseIcon

	^ self fetch: #( '14x14' 'buttons' 'collapse' )

</details>

#### Theme>>#dateIcon

<details>
	<summary>See more</summary>
	
	dateIcon

	^ self fetch: #( '16x16' 'mimetypes' 'x-office-calendar'  )

</details>

#### Theme>>#fileContentsBrowser

<details>
	<summary>See more</summary>
	
	fileContentsBrowser
	^ `Color tan duller`
</details>

#### Theme>>#pattern

#bold


<details>
	<summary>See more</summary>
	
	pattern
	"#bold"

	^ #(
		patternKeyword
		patternBinary
		patternUnary
	)
</details>

#### Theme>>#changeSorter

<details>
	<summary>See more</summary>
	
	changeSorter
	^ self packageList
</details>

#### Theme>>#windowLabel

<details>
	<summary>See more</summary>
	
	windowLabel
	^ `Color gray: 0.3`
</details>

#### Theme>>#text

<details>
	<summary>See more</summary>
	
	text
	^ `Color black`
</details>

#### Theme>>#displayIcon

<details>
	<summary>See more</summary>
	
	displayIcon

	^ self fetch: #( '16x16' 'devices' 'video-display' )

</details>

#### Theme>>#workspace

<details>
	<summary>See more</summary>
	
	workspace
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color h: 60.0 s: 0.73 v: 0.72` ]
</details>

