## BMPReadWriter

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### BMPReadWriter>>#readIndexedBmpFile: colors

Read uncompressed pixel data of depth d from the given BMP stream, where d is 1, 4, 8, or 16


<details>
	<summary>See more</summary>
	
	readIndexedBmpFile: colors
	"Read uncompressed pixel data of depth d from the given BMP stream, where d is 1, 4, 8, or 16"
	| form bytesPerRow pixelData pixelLine startIndex map bitBlt mask |
	colors 
		ifNil:[form := Form extent: biWidth@biHeight depth: biBitCount]
		ifNotNil:[form := ColorForm extent: biWidth@biHeight nativeDepthFrom: biBitCount.
				form colors: colors].
	bytesPerRow := (((biBitCount* biWidth) + 31) // 32) * 4.
	pixelData := ByteArray new: bytesPerRow * biHeight.
	biHeight to: 1 by: -1 do: [:y |
		pixelLine := stream next: bytesPerRow.
		startIndex := ((y - 1) * bytesPerRow) + 1.
		pixelData 
			replaceFrom: startIndex 
			to: startIndex + bytesPerRow - 1 
			with: pixelLine 
			startingAt: 1].
	form copyFromByteArray: pixelData.
	biBitCount = 16 ifTrue:[
		map := ColorMap shifts: #(8 -8 0 0) masks: #(16rFF 16rFF00 0 0).
		mask := 16r80008000.
	].
	biBitCount = 32 ifTrue:[
		map := ColorMap shifts: #(24 8 -8 -24) masks: #(16rFF 16rFF00 16rFF0000 16rFF000000).
		mask := 16rFF000000.
	].
	map ifNotNil:[
		bitBlt := BitBlt toForm: form.
		bitBlt sourceForm: form.
		bitBlt colorMap: map.
		bitBlt combinationRule: Form over.
		bitBlt copyBits.
	].
	mask ifNotNil:[
		bitBlt := BitBlt toForm: form.
		bitBlt combinationRule: 7 "bitOr:with:".
		bitBlt fillBitmap: (Bitmap with: mask).
		bitBlt copyBits.
	].
	^ form

</details>

#### BMPReadWriter>>#store24BitBmpLine: pixelLine from: formBits startingAt: formBitsIndex width: width

Stores a single scanline containing 32bpp RGBA values in a 24bpp scanline. Swizzles the bytes as needed.


<details>
	<summary>See more</summary>
	
	store24BitBmpLine: pixelLine from: formBits startingAt: formBitsIndex width: width
	"Stores a single scanline containing 32bpp RGBA values in a 24bpp scanline.
	Swizzles the bytes as needed."

	| pixIndex rgb bitsIndex |
	<primitive: 'primitiveWrite24BmpLine' module:'BMPReadWriterPlugin'>
	pixIndex := 0. "pre-increment"
	bitsIndex := formBitsIndex-1. "pre-increment"
	1 to: width do: [:j |
		rgb := (formBits at: (bitsIndex := bitsIndex+1)) bitAnd: 16rFFFFFF.
		pixelLine at: (pixIndex := pixIndex+1) put: (rgb bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -8) bitAnd: 255).
		pixelLine at: (pixIndex := pixIndex+1) put: ((rgb bitShift: -16) bitAnd: 255).
	].

</details>

#### BMPReadWriter>>#readColorMap

Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors.


<details>
	<summary>See more</summary>
	
	readColorMap
	"Read colorCount BMP color map entries from the given binary stream. Answer an array of Colors."
	| colorCount colors maxLevel b g r ccStream |
	colorCount := (bfOffBits - 54) // 4.
	"Note: some programs (e.g. Photoshop 4.0) apparently do not set colorCount; assume that any data between the end of the header and the start of the pixel data is the color map"
	biBitCount >= 16 ifTrue:[^nil].
	colorCount = 0 ifTrue: [ "this BMP file does not have a color map"
		"default monochrome color map"
		biBitCount = 1 ifTrue: [^ `Array with: Color white with: Color black`].
		"default gray-scale color map"
		maxLevel := (2 raisedTo: biBitCount) - 1.
		^ (0 to: maxLevel) collect: [:level | Color gray: (level asFloat / maxLevel)]].
	ccStream := ReadStream on: (stream next: colorCount*4).
	colors := Array new: colorCount.
	1 to: colorCount do: [:i |
		b := ccStream next.
		g := ccStream next.
		r := ccStream next.
		ccStream next. "skip reserved"
		colors at: i put: (Color r: r g: g b: b range: 255)].
	^ colors

</details>

#### BMPReadWriter>>#read24BmpFile

Read 24-bit pixel data from the given a BMP stream.


<details>
	<summary>See more</summary>
	
	read24BmpFile
	"Read 24-bit pixel data from the given a BMP stream."
	| form formBits pixelLine bitsIndex |
	form := Form extent: biWidth@biHeight depth: 32.
	pixelLine := ByteArray new: (((24 * biWidth) + 31) // 32) * 4.
	bitsIndex := form height - 1 * biWidth + 1.
	formBits := form bits.
	1 to: biHeight do: [:i |
		pixelLine := stream nextInto: pixelLine.
		self read24BmpLine: pixelLine into: formBits startingAt: bitsIndex width: biWidth.
		bitsIndex := bitsIndex - biWidth.
	].
	form makeAllPixelsOpaque.
	^ form

</details>

#### BMPReadWriter>>#nextPutImage: aForm

Encoding anImage on stream.


<details>
	<summary>See more</summary>
	
	nextPutImage: aForm
	| bhSize rowBytes rgb data colorValues depth image scanLineLen pixline |
	depth := aForm depth.
	depth := #(1 4 8 32 ) detect: [ :each | each >= depth].
	image := aForm asFormOfDepth: depth.
	bhSize := 14.  "# bytes in file header"
	biSize := 40.  "info header size in bytes"
	biWidth := image width.
	biHeight := image height.
	biClrUsed := depth = 32 ifTrue: [0] ifFalse:[1 << depth].  "No. color table entries"
	bfOffBits := biSize + bhSize + (4*biClrUsed).
	rowBytes := ((depth min: 24) * biWidth + 31 // 32) * 4.
	biSizeImage := biHeight * rowBytes.

	"Write the file header"
	stream position: 0.
	stream nextUnsignedInt16Put: 19778 bigEndian: false.  "bfType = BM"
	stream nextUnsignedInt32Put: bfOffBits + biSizeImage bigEndian: false.  "Entire file size in bytes"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "bfReserved"
	stream nextUnsignedInt32Put: bfOffBits bigEndian: false.  "Offset of bitmap data from start of hdr (and file)"

	"Write the bitmap info header"
	stream position: bhSize.
	stream nextUnsignedInt32Put: biSize bigEndian: false.  "info header size in bytes"
	stream nextUnsignedInt32Put: image width bigEndian: false.  "biWidth"
	stream nextUnsignedInt32Put: image height bigEndian: false.  "biHeight"
	stream nextUnsignedInt16Put: 1 bigEndian: false.  "biPlanes"
	stream nextUnsignedInt16Put: (depth min: 24) bigEndian: false.  "biBitCount"
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biCompression"
	stream nextUnsignedInt32Put: biSizeImage bigEndian: false.  "size of image section in bytes"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biXPelsPerMeter"
	stream nextUnsignedInt32Put: 2800 bigEndian: false.  "biYPelsPerMeter"
	stream nextUnsignedInt32Put: biClrUsed bigEndian: false.
	stream nextUnsignedInt32Put: 0 bigEndian: false.  "biClrImportant"
	biClrUsed > 0 ifTrue: [
		"write color map; this works for ColorForms, too"
		colorValues := image colormapIfNeededForDepth: 32.
		1 to: biClrUsed do: [ :i |
			rgb := colorValues at: i.
			0 to: 24 by: 8 do: [ :j | stream nextPut: (rgb >> j bitAnd: 16rFF)]]].

	depth = 32 ifTrue: [
		"original depth 16 or 32"
		data _ image bits.
		pixline _ ByteArray new: (((biWidth * 3 + 3) // 4) * 4).
		1 to: biHeight do: [ :i |
			self store24BitBmpLine: pixline from: data startingAt: (biHeight-i)*biWidth+1 width: biWidth.
			stream nextPutAll: pixline.
		].
	] ifFalse: [
		"depth = 1, 4 or 8."
		data _ image bits asByteArray.
		scanLineLen _ image wordsPerLine * 4.  "# of bytes in line"
		1 to: biHeight do: [ :i |
			stream next: scanLineLen putAll: data startingAt: (biHeight-i)*scanLineLen+1.
		].
	].
	stream position = (bfOffBits + biSizeImage) ifFalse: [self error:'Write failure']
</details>

#### BMPReadWriter>>#readHeader

<details>
	<summary>See more</summary>
	
	readHeader
	| reserved |
	bfType _ stream nextUnsignedInt16BigEndian: false.
	bfSize _ stream nextUnsignedInt32BigEndian: false.
	reserved _ stream nextUnsignedInt32BigEndian: false.
	bfOffBits _ stream nextUnsignedInt32BigEndian: false.
	biSize _ stream nextUnsignedInt32BigEndian: false.
	biWidth _ stream nextUnsignedInt32BigEndian: false.
	biHeight _ stream nextUnsignedInt32BigEndian: false.
	biPlanes _ stream nextUnsignedInt16BigEndian: false.
	biBitCount _ stream nextUnsignedInt16BigEndian: false.
	biCompression _ stream nextUnsignedInt32BigEndian: false.
	biSizeImage _ stream nextUnsignedInt32BigEndian: false.
	biXPelsPerMeter _ stream nextUnsignedInt32BigEndian: false.
	biYPelsPerMeter _ stream nextUnsignedInt32BigEndian: false.
	biClrUsed _ stream nextUnsignedInt32BigEndian: false.
	biClrImportant _ stream nextUnsignedInt32BigEndian: false.

</details>

#### BMPReadWriter>>#read24BmpLine: pixelLine into: formBits startingAt: formBitsIndex width: width

Swizzles the bytes in a 24bpp scanline and fills in the given 32bpp form bits. Ensures that color black is represented as 16rFF000001 so that Form paint works properly.


<details>
	<summary>See more</summary>
	
	read24BmpLine: pixelLine into: formBits startingAt: formBitsIndex width: width
	"Swizzles the bytes in a 24bpp scanline and fills in the given 32bpp form bits.
	Ensures that color black is represented as 16rFF000001 so that Form paint
	works properly."

	| pixIndex rgb bitsIndex |
	<primitive: 'primitiveRead24BmpLine' module:'BMPReadWriterPlugin'>
	pixIndex := 0. "pre-increment"
	bitsIndex := formBitsIndex-1. "pre-increment"
	1 to: width do: [:j |
		rgb := 
			(pixelLine at: (pixIndex := pixIndex+1)) +
			((pixelLine at: (pixIndex := pixIndex+1)) bitShift: 8) +
			((pixelLine at: (pixIndex := pixIndex+1)) bitShift: 16).
		rgb = 0 ifTrue:[rgb := 16rFF000001] ifFalse:[rgb := rgb + 16rFF000000].
		formBits at: (bitsIndex := bitsIndex+1) put: rgb.
	].

</details>

#### BMPReadWriter>>#nextImage

Dencoding an image on stream and answer the image.


<details>
	<summary>See more</summary>
	
	nextImage
	| colors |
	self readHeader.
	biBitCount = 24 ifTrue:[^self read24BmpFile].
	"read the color map"
	colors := self readColorMap.
	^self readIndexedBmpFile: colors
</details>

#### BMPReadWriter>>#understandsImageFormat

Test to see if the image stream format is understood by this decoder. This should be implemented in each subclass of ImageReadWriter so that a proper decoder can be selected without ImageReadWriter having to know about all possible image file types.


<details>
	<summary>See more</summary>
	
	understandsImageFormat
	stream size < 54 ifTrue:[^false]. "min size = BITMAPFILEHEADER+BITMAPINFOHEADER"
	self readHeader.
	bfType = 19778 "BM" ifFalse:[^false].
	biSize = 40 ifFalse:[^false].
	biPlanes = 1 ifFalse:[^false].
	bfSize <= stream size ifFalse:[^false].
	biCompression = 0 ifFalse:[^false].
	^true
</details>

## ImageReadWriter

Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved. I am an abstract class to provide for encoding and/or decoding an image on a stream. Instance Variables: stream <ReadStream | WriteStream> stream for image storages Class Variables: ImageNotStoredSignal <Signal> image not stored error signal MagicNumberErrorSignal <Signal> magic number error signal Subclasses must implement the following messages: accessing nextImage nextPutImage: testing canUnderstand (added tao 10/26/97)

### Methods
#### ImageReadWriter>>#skip: anInteger

<details>
	<summary>See more</summary>
	
	skip: anInteger

	^stream skip: anInteger
</details>

#### ImageReadWriter>>#nextPut: aByte

<details>
	<summary>See more</summary>
	
	nextPut: aByte

	^stream nextPut: aByte
</details>

#### ImageReadWriter>>#position: anInteger

<details>
	<summary>See more</summary>
	
	position: anInteger

	^stream position: anInteger
</details>

#### ImageReadWriter>>#hasMagicNumber: aByteArray

<details>
	<summary>See more</summary>
	
	hasMagicNumber: aByteArray
	| position |
	position _ stream position.
	((stream size - position) >= aByteArray size and:
	[(stream next: aByteArray size)  = aByteArray])
		ifTrue: [^true].
	stream position: position.
	^false
</details>

#### ImageReadWriter>>#next

<details>
	<summary>See more</summary>
	
	next

	^stream next
</details>

#### ImageReadWriter>>#peekFor: aValue

<details>
	<summary>See more</summary>
	
	peekFor: aValue

	^stream peekFor: aValue
</details>

#### ImageReadWriter>>#atEnd

<details>
	<summary>See more</summary>
	
	atEnd

	^stream atEnd
</details>

#### ImageReadWriter>>#nextImage

Dencoding an image on stream and answer the image.


<details>
	<summary>See more</summary>
	
	nextImage
	"Dencoding an image on stream and answer the image."

	^self subclassResponsibility
</details>

#### ImageReadWriter>>#position

<details>
	<summary>See more</summary>
	
	position

	^stream position
</details>

#### ImageReadWriter>>#changePadOfBits: bits width: width height: height depth: depth from: oldPad
to: newPad

Change padding size of bits.


<details>
	<summary>See more</summary>
	
	changePadOfBits: bits width: width height: height depth: depth from: oldPad
to: newPad
	"Change padding size of bits."

	| srcRowByteSize dstRowByteSize newBits srcRowBase rowEndOffset |
	(#(8 16 32) includes: oldPad)
		ifFalse: [^self error: 'Invalid pad: ', oldPad printString].
	(#(8 16 32) includes: newPad)
		ifFalse: [^self error: 'Invalid pad: ', newPad printString].
	srcRowByteSize _ width * depth + oldPad - 1 // oldPad * (oldPad / 8).
	srcRowByteSize * height = bits size
		ifFalse: [^self error: 'Incorrect bitmap array size.'].
	dstRowByteSize _ width * depth + newPad - 1 // newPad * (newPad / 8).
	newBits _ ByteArray new: dstRowByteSize * height.
	srcRowBase _ 1.
	rowEndOffset _ dstRowByteSize - 1.
	1 to: newBits size by: dstRowByteSize do:
		[:dstRowBase |
		newBits replaceFrom: dstRowBase
			to: dstRowBase + rowEndOffset
			with: bits
			startingAt: srcRowBase.
		srcRowBase _ srcRowBase + srcRowByteSize].
	^newBits
</details>

#### ImageReadWriter>>#nextPutAll: aByteArray

<details>
	<summary>See more</summary>
	
	nextPutAll: aByteArray

	^stream nextPutAll: aByteArray
</details>

#### ImageReadWriter>>#binaryStream: aStream

<details>
	<summary>See more</summary>
	
	binaryStream: aStream
	stream _ aStream.
	fileSize _ stream size
</details>

#### ImageReadWriter>>#size

Primitive. Answer the number of indexable variables in the receiver. This value is the same as the largest legal subscript. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	size

	^stream size
</details>

#### ImageReadWriter>>#next: size

<details>
	<summary>See more</summary>
	
	next: size

	^stream next: size
</details>

#### ImageReadWriter>>#nextUnsignedInt32

Read a 32-bit unsigned quantity from the stream. Big Endian


<details>
	<summary>See more</summary>
	
	nextUnsignedInt32
	"Read a 32-bit unsigned quantity from the stream.
	Big Endian"

	^ stream nextUnsignedInt32BigEndian: true
</details>

#### ImageReadWriter>>#nextPutImage: anImage

Encoding anImage on stream.


<details>
	<summary>See more</summary>
	
	nextPutImage: anImage
	"Encoding anImage on stream."

	^self subclassResponsibility
</details>

#### ImageReadWriter>>#contents

<details>
	<summary>See more</summary>
	
	contents

	^stream contents
</details>

#### ImageReadWriter>>#nextUnsignedInt32Put: aNumber

Write out a 32-bit integer as 32 bits. Big Endian


<details>
	<summary>See more</summary>
	
	nextUnsignedInt32Put: aNumber
	"Write out a 32-bit integer as 32 bits.
	Big Endian"

	stream nextUnsignedInt32Put: aNumber bigEndian: true.
	^aNumber
</details>

#### ImageReadWriter>>#understandsImageFormat

Test to see if the image stream format is understood by this decoder. This should be implemented in each subclass of ImageReadWriter so that a proper decoder can be selected without ImageReadWriter having to know about all possible image file types.


<details>
	<summary>See more</summary>
	
	understandsImageFormat
	"Test to see if the image stream format is understood by this decoder.
	This should be implemented in each subclass of ImageReadWriter so that
	a proper decoder can be selected without ImageReadWriter having to know
	about all possible image file types."

	^ false
</details>

## JPEGReadWriter2

I provide fast JPEG compression and decompression. I require the VM pluginJPEGReadWriter2Plugin, which is typically stored in same directory as the Squeak virtual machine. JPEGReadWriter2Plugin is based on LIBJPEG library. This sentence applies to the plugin: "This software is based in part on the work of the Independent JPEG Group". The LIBJPEG license allows it to be used free for any purpose so long as its origin and copyright are acknowledged. You can read more about LIBJPEG and get the complete source code at www.ijg.org.

### Methods
#### JPEGReadWriter2>>#primJPEGDecompressStructSize

<details>
	<summary>See more</summary>
	
	primJPEGDecompressStructSize

	<primitive: 'primJPEGDecompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#resultFormWidth: width height: height components: components orReuse: aFormOrNil

<details>
	<summary>See more</summary>
	
	resultFormWidth: width height: height components: components orReuse: aFormOrNil

	aFormOrNil ifNotNil: [
		(aFormOrNil class == (components = 1 ifTrue: [GrayForm] ifFalse: [Form])) ifTrue: [
			aFormOrNil extent = (width@height) ifTrue: [
				^ aFormOrNil ]]].

	^components
			ifNil: [ Form extent: width@height depth: 32]		"Original version of the JPEG plugin"
			ifNotNil: [ 												"Enhanced version of the JPEG plugin as of 6/2016"
				components = 3
					ifTrue: [ Form extent: width@height nativeDepthFrom: 32 ]
					ifFalse: [ GrayForm extent: width@height ]]
</details>

#### JPEGReadWriter2>>#primJPEGPluginIsPresent

<details>
	<summary>See more</summary>
	
	primJPEGPluginIsPresent
	<primitive: 'primJPEGPluginIsPresent' module: 'JPEGReadWriter2Plugin'>
	^false
</details>

#### JPEGReadWriter2>>#primJPEGCompressStructSize

<details>
	<summary>See more</summary>
	
	primJPEGCompressStructSize

	<primitive: 'primJPEGCompressStructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct

<details>
	<summary>See more</summary>
	
	primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct

	<primitive: 'primJPEGReadImagefromByteArrayonFormdoDitheringerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#jpegWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct

<details>
	<summary>See more</summary>
	
	jpegWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct

	^ self primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct
</details>

#### JPEGReadWriter2>>#compress: aForm quality: quality progressiveJPEG: progressiveFlag

Encode the given Form and answer the compressed ByteArray. Quality goes from 0 (low) to 100 (high), where -1 means default. Usually progressiveFlag is false


<details>
	<summary>See more</summary>
	
	compress: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form and answer the compressed ByteArray.
	Quality goes from 0 (low) to 100 (high), where -1 means default.
	Usually progressiveFlag is false"
	"Creates buffer twice. Normally you would use a better optimization."
	
	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		^ buffer copyFrom: 1 to: byteCount ]
</details>

#### JPEGReadWriter2>>#supportsGrayForms

<details>
	<summary>See more</summary>
	
	supportsGrayForms
	<primitive: 'primSupports8BitGrayscaleJPEGs' module: 'JPEGReadWriter2Plugin'>
	^false
</details>

#### JPEGReadWriter2>>#jpegReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct

<details>
	<summary>See more</summary>
	
	jpegReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct

	^ self primJPEGReadImage: aJPEGDecompressStruct fromByteArray: source onForm: form doDithering: ditherFlag errorMgr: aJPEGErrorMgr2Struct
</details>

#### JPEGReadWriter2>>#primJPEGErrorMgr2StructSize

<details>
	<summary>See more</summary>
	
	primJPEGErrorMgr2StructSize

	<primitive: 'primJPEGErrorMgr2StructSize' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#primImageHeight: aJPEGCompressStruct

<details>
	<summary>See more</summary>
	
	primImageHeight: aJPEGCompressStruct

	<primitive: 'primImageHeight' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#nextImage

Decode and answer a Form from my stream. We can read RGB JPEGs into: * 32-bit Forms * -32-bit Forms *16-bit Forms (with or without dithering!) * -16-bit Forms (with or without dithering!) We can read grayscale JPEGs into: * 32-bit Forms * -32-bit Forms *16-bit Forms (with or without dithering!) * -16-bit Forms (with or without dithering!) * 8-bit GrayForms * -8-bit GrayForms


<details>
	<summary>See more</summary>
	
	nextImage
	"Decode and answer a Form from my stream. 
	We can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		*16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
		* 8-bit GrayForms
		* -8-bit GrayForms"

	| bytes |
"	bytes _ stream upToEnd."
	bytes _ stream contents.
	^self uncompress: bytes into: nil
</details>

#### JPEGReadWriter2>>#compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: aByteArrayOrNil into: aBlock

Encode the given Form with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG. Evaluate aBlock with two arguments. The first is a ByteArray with the data. Usually bigger than needed. The second argument is the actual maningful bytes. We can only compress: * 32-bit deep Forms * -32-bit deep Forms * 16-bit deep Forms * -16-bit deep Forms * 8-bit deep GrayForms * -8-bit deep GrayForms


<details>
	<summary>See more</summary>
	
	compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: aByteArrayOrNil into: aBlock
	"Encode the given Form with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
	
	Evaluate aBlock with two arguments. The first is a ByteArray with the data. Usually bigger than needed.
	The second argument is the actual maningful bytes.
	
	We can only compress:
		* 32-bit deep Forms 
		* -32-bit deep Forms
		* 16-bit deep Forms
		* -16-bit deep Forms
		* 8-bit deep GrayForms
		* -8-bit deep GrayForms"

	| sourceForm jpegCompressStruct jpegErrorMgr2Struct buffer byteCount |
	self supportsGrayForms
		ifTrue: [
			"Newer plugin supports 32bpp, 16bpp, GrayForms"
			sourceForm _ (aForm depth = 32) |  (aForm depth = 16) | (aForm is: #GrayForm)
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 16]]
		ifFalse: [
			"Original plugin supports 32bpp and even width big endian 16bpp"
			sourceForm _ (aForm depth = 32) | (aForm width even & (aForm nativeDepth = 16))
				ifTrue: [aForm]
				ifFalse: [aForm asFormOfDepth: 32]].

	jpegCompressStruct _ ByteArray new: self primJPEGCompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	"Most likely more than needed"
	buffer _ aByteArrayOrNil ifNil: [ByteArray new: sourceForm width * sourceForm height // 2+1024].
	[
		byteCount _ self jpegWriteImage: jpegCompressStruct 
			onByteArray: buffer
			form: sourceForm
			quality: quality
			progressiveJPEG: progressiveFlag
			errorMgr: jpegErrorMgr2Struct.
		byteCount = 0 ] whileTrue: [
			"But if not, ask for some more"
			buffer _ ByteArray new: buffer size * 14 // 10 ].
	
	aBlock value: buffer value: byteCount
</details>

#### JPEGReadWriter2>>#nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag

Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG. We can only compress: * 32-bit deep Forms * -32-bit deep Forms * 16-bit deep Forms * -16-bit deep Forms * 8-bit deep GrayForms * -8-bit deep GrayForms


<details>
	<summary>See more</summary>
	
	nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag
	"Encode the given Form on my stream with the given settings. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG.
	We can only compress:
		* 32-bit deep Forms 
		* -32-bit deep Forms
		* 16-bit deep Forms
		* -16-bit deep Forms
		* 8-bit deep GrayForms
		* -8-bit deep GrayForms"

	self compress: aForm quality: quality progressiveJPEG: progressiveFlag usingBuffer: nil into: [ :buffer :byteCount |
		stream next: byteCount putAll: buffer startingAt: 1 ]
</details>

#### JPEGReadWriter2>>#imageExtent: aByteArray

Answer the extent of the compressed image encoded in the given ByteArray.


<details>
	<summary>See more</summary>
	
	imageExtent: aByteArray 
	"Answer the extent of the compressed image encoded in the given ByteArray."

	| jpegDecompressStruct jpegErrorMgr2Struct w h |
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	w _ self primImageWidth: jpegDecompressStruct.
	h _ self primImageHeight: jpegDecompressStruct.
	^ w @ h

</details>

#### JPEGReadWriter2>>#primJPEGReadHeader: aJPEGDecompressStruct fromByteArray: source errorMgr: aJPEGErrorMgr2Struct

<details>
	<summary>See more</summary>
	
	primJPEGReadHeader: aJPEGDecompressStruct fromByteArray: source errorMgr: aJPEGErrorMgr2Struct

	<primitive: 'primJPEGReadHeaderfromByteArrayerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct

<details>
	<summary>See more</summary>
	
	primJPEGWriteImage: aJPEGCompressStruct onByteArray: destination form: form quality: quality progressiveJPEG: progressiveFlag errorMgr: aJPEGErrorMgr2Struct

	<primitive: 'primJPEGWriteImageonByteArrayformqualityprogressiveJPEGerrorMgr' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#nextPutImage: aForm

Encode the given Form on my stream with default quality.


<details>
	<summary>See more</summary>
	
	nextPutImage: aForm
	"Encode the given Form on my stream with default quality."

	^ self nextPutImage: aForm quality: -1 progressiveJPEG: false

</details>

#### JPEGReadWriter2>>#primImageNumComponents: aJPEGDecompressStruct

If primitive not present, answer nil.


<details>
	<summary>See more</summary>
	
	primImageNumComponents: aJPEGDecompressStruct
	"If primitive not present, answer nil."
	<primitive: 'primImageNumComponents' module: 'JPEGReadWriter2Plugin'>
	^nil
</details>

#### JPEGReadWriter2>>#isPluginPresent

<details>
	<summary>See more</summary>
	
	isPluginPresent
	^self primJPEGPluginIsPresent
</details>

#### JPEGReadWriter2>>#uncompress: aByteArray into: aFormOrNil

Uncompress an image from the given ByteArray. If we have a post 6/2016 version of the plugin, we can read RGB JPEGs into: * 32-bit Forms * -32-bit Forms * 16-bit Forms (with or without dithering!) * -16-bit Forms (with or without dithering!) We can read grayscale JPEGs into: * 32-bit Forms * -32-bit Forms * 16-bit Forms (with or without dithering!) * -16-bit Forms (with or without dithering!) * 8-bit GrayForms * -8-bit GrayForms aFormOrNil might be a Form (16 or 32 bpp) for 3 RGB JPEG or a GrayForm for Gray JPEG. If nil, an appropriate instance is created. Optional argument aFormOrNil might be useful to save memory or avoid allocation.


<details>
	<summary>See more</summary>
	
	uncompress: aByteArray into: aFormOrNil
	"Uncompress an image from the given ByteArray.
	If we have a post 6/2016 version of the plugin, we can read RGB JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
	We can read grayscale JPEGs into:
		* 32-bit Forms
		* -32-bit Forms
		* 16-bit Forms (with or without dithering!)
		* -16-bit Forms (with or without dithering!)
		* 8-bit GrayForms
		* -8-bit GrayForms
	
	aFormOrNil might be a Form (16 or 32 bpp) for 3 RGB JPEG or a GrayForm for Gray JPEG. 
	If nil, an appropriate instance is created.
	Optional argument aFormOrNil might be useful to save memory or avoid allocation.
	"

	| width height components form jpegDecompressStruct jpegErrorMgr2Struct |
	
	jpegDecompressStruct _ ByteArray new: self primJPEGDecompressStructSize.
	jpegErrorMgr2Struct _ ByteArray new: self primJPEGErrorMgr2StructSize.
	self 
		primJPEGReadHeader: jpegDecompressStruct 
		fromByteArray: aByteArray
		errorMgr: jpegErrorMgr2Struct.
	
	width _ self primImageWidth: jpegDecompressStruct.
	height _ self primImageHeight: jpegDecompressStruct.
	components _ self primImageNumComponents: jpegDecompressStruct.
	form _ self resultFormWidth: width height: height components: components orReuse: aFormOrNil.

	(width = 0 or: [height = 0]) ifTrue: [^ form].
	self
		jpegReadImage: jpegDecompressStruct
		fromByteArray: aByteArray
		onForm: form
		doDithering: true
		errorMgr: jpegErrorMgr2Struct.
	
	^ form
</details>

#### JPEGReadWriter2>>#primImageWidth: aJPEGCompressStruct

<details>
	<summary>See more</summary>
	
	primImageWidth: aJPEGCompressStruct

	<primitive: 'primImageWidth' module: 'JPEGReadWriter2Plugin'>
	self primitiveFailed

</details>

#### JPEGReadWriter2>>#understandsImageFormat

Answer true if the image stream format is understood by this decoder.


<details>
	<summary>See more</summary>
	
	understandsImageFormat
	"Answer true if the image stream format is understood by this decoder."
	self isPluginPresent ifFalse:[^false]. "cannot read it otherwise"
	self next = 16rFF ifFalse: [^ false].
	self next = 16rD8 ifFalse: [^ false].
	^ true

</details>

