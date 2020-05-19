## Delay

I am the main way that a process may pause for some amount of time. The simplest usage is like this: (Delay forSeconds: 5) wait. An instance of Delay responds to the message 'wait' by suspending the caller's process for a certain amount of time. The duration of the pause is specified when the Delay is created with the message forMilliseconds: or forSeconds:. A Delay can be used again when the current wait has finished. For example, a clock process might repeatedly wait on a one-second Delay. A delay in progress when an image snapshot is saved is resumed when the snapshot is re-started. Delays work across millisecond clock roll-overs. For a more complex example, see #testDelayOf:for:rect: . A word of advice: This is THE highest priority code which is run in Squeak, in other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system. In short: Don't fix it if it ain't broken! This code isn't supposed to be beautiful, it's supposed to be fast! The reason for duplicating code is to make it fast. The reason for not using ifNil:[]ifNotNil:[] is that the compiler may not inline those. Since the effect of changes are VERY hard to predict it is best to leave things as they are for now unless there is an actual need to change anything

### Methods
#### Delay>>#beingWaitedOn: aBool

Indicate whether this delay is currently scheduled, e.g., being waited on


<details>
	<summary>See more</summary>
	
	beingWaitedOn: aBool
	"Indicate whether this delay is currently scheduled, e.g., being waited on"
	beingWaitedOn := aBool
</details>

#### Delay>>#delayDuration

<details>
	<summary>See more</summary>
	
	delayDuration
	^delayDuration
</details>

#### Delay>>#isExpired

<details>
	<summary>See more</summary>
	
	isExpired

	^delaySemaphore isSignaled.

</details>

#### Delay>>#wait

Schedule this Delay, then wait on its semaphore. The current process will be suspended for the amount of time specified when this Delay was created.


<details>
	<summary>See more</summary>
	
	wait
	"Schedule this Delay, then wait on its semaphore. The current process will be suspended for the amount of time specified when this Delay was created."

	self schedule.
	[delaySemaphore wait] ifCurtailed:[self unschedule].

</details>

#### Delay>>#adjustResumptionTimeOldBase: oldBaseTime newBase: newBaseTime

Private! Adjust the value of the system's millisecond clock at which this Delay will be awoken. Used to adjust resumption times after a snapshot or clock roll-over.


<details>
	<summary>See more</summary>
	
	adjustResumptionTimeOldBase: oldBaseTime newBase: newBaseTime
	"Private! Adjust the value of the system's millisecond clock at which this Delay will be awoken. Used to adjust resumption times after a snapshot or clock roll-over."

	resumptionTime _ newBaseTime + (resumptionTime - oldBaseTime).

</details>

#### Delay>>#setDelay: millisecondCount forSemaphore: aSemaphore

Private! Initialize this delay to signal the given semaphore after the given number of milliseconds.


<details>
	<summary>See more</summary>
	
	setDelay: millisecondCount forSemaphore: aSemaphore
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration _ millisecondCount.
	delaySemaphore _ aSemaphore.
	beingWaitedOn _ false.

</details>

#### Delay>>#delaySemaphore

<details>
	<summary>See more</summary>
	
	delaySemaphore

	^ delaySemaphore
</details>

#### Delay>>#resumptionTime

Answer the value of the system's millisecondClock at which the receiver's suspended Process will resume.


<details>
	<summary>See more</summary>
	
	resumptionTime
	"Answer the value of the system's millisecondClock at which the receiver's suspended Process will resume."

	^ resumptionTime

</details>

#### Delay>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: '('; print: delayDuration; nextPutAll: ' msecs'.
	beingWaitedOn ifTrue:[
		aStream nextPutAll: '; '; print: resumptionTime - Time millisecondClockValue; nextPutAll: ' msecs remaining'.
	].
	aStream nextPutAll: ')'.
</details>

#### Delay>>#resumptionTime: anInteger

Private! Set the value of the system's millisecondClock at which the receiver's suspended Process will resumed. Must only be called from the class-side #scheduleDelay:.


<details>
	<summary>See more</summary>
	
	resumptionTime: anInteger
	"Private! Set the value of the system's millisecondClock at which the receiver's suspended Process will resumed.
	Must only be called from the class-side #scheduleDelay:."
	
	resumptionTime := anInteger
</details>

#### Delay>>#schedule

Schedule this delay.


<details>
	<summary>See more</summary>
	
	schedule
	"Schedule this delay."

	beingWaitedOn ifTrue: [ ^ self error: 'This Delay has already been scheduled.' ].
	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	ScheduledDelay == nil
		ifTrue: [
			"No other process will overwrite "
			ScheduledDelay _ self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			ScheduledDelayNilledSemaphore wait.
			ScheduledDelay _ self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action ScheduledDelay,
        set ScheduledDelay to nil and signal ScheduledDelayNilledSemaphore"
	TimingSemaphore signal

</details>

#### Delay>>#unschedule

Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted.


<details>
	<summary>See more</summary>
	
	unschedule

	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	FinishedDelay == nil
		ifTrue: [ FinishedDelay _ self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			FinishedDelayNilledSemaphore wait.
			FinishedDelay _ self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action FinishedDelay,
        set FinishedDelay to nil and signal FinishedDelayNilledSemaphore"
	TimingSemaphore signal

</details>

#### Delay>>#setDelay: millisecondCount

Private! Initialize this delay to signal the given semaphore after the given number of milliseconds.


<details>
	<summary>See more</summary>
	
	setDelay: millisecondCount 
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration _ millisecondCount

</details>

#### Delay>>#beingWaitedOn

Answer whether this delay is currently scheduled, e.g., being waited on


<details>
	<summary>See more</summary>
	
	beingWaitedOn
	"Answer whether this delay is currently scheduled, e.g., being waited on"
	^beingWaitedOn
</details>

#### Delay>>#signalWaitingProcess

The delay time has elapsed; signal the waiting process.


<details>
	<summary>See more</summary>
	
	signalWaitingProcess
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn _ false.
	delaySemaphore signal.

</details>

## DelayWaitTimeout

DelayWaitTimeout is a special kind of Delay used in waitTimeoutMSecs: to avoid signaling the underlying semaphore when the wait times out.

### Methods
#### DelayWaitTimeout>>#isExpired

Did this timeout fire before the associated semaphore was signaled?


<details>
	<summary>See more</summary>
	
	isExpired
	"Did this timeout fire before the associated semaphore was signaled?"
	^expired
</details>

#### DelayWaitTimeout>>#setDelay: anInteger forSemaphore: aSemaphore

Private! Initialize this delay to signal the given semaphore after the given number of milliseconds.


<details>
	<summary>See more</summary>
	
	setDelay: anInteger forSemaphore: aSemaphore
	super setDelay: anInteger forSemaphore: aSemaphore.
	process := Processor activeProcess.
	expired := false.
</details>

#### DelayWaitTimeout>>#signalWaitingProcess

Release the given process from the semaphore it is waiting on. This method relies on running at highest priority so that it cannot be preempted by the process being released.


<details>
	<summary>See more</summary>
	
	signalWaitingProcess
	"Release the given process from the semaphore it is waiting on.
	This method relies on running at highest priority so that it cannot be preempted
	by the process being released."
	beingWaitedOn := false.
	"Release the process but only if it is still waiting on its original list"
	process suspendingList == delaySemaphore ifTrue:[
		expired := true.
		process suspend; resume.
	].

</details>

#### DelayWaitTimeout>>#wait

Wait until either the semaphore is signaled or the delay times out


<details>
	<summary>See more</summary>
	
	wait
	"Wait until either the semaphore is signaled or the delay times out"
	[self schedule.
	"It is critical that the following has no suspension point so that
	the test and the wait primitive are atomic. In addition, if the delay
	is no longer being waited on while entering the way we know that it 
	is expired because the delay has already fired."
	beingWaitedOn 
		ifTrue:[delaySemaphore wait]
		ifFalse:[expired := true]] ensure:[self unschedule].
	^self isExpired

</details>

## EventSensor

EventSensor is a replacement for InputSensor based on a set of (optional) event primitives. An EventSensor updates its state when events are received so that all state based users of Sensor (e.g., Sensor keyboard, Sensor leftShiftDown, Sensor mouseButtons) will work exactly as before, by moving the current VM mechanisms into EventSensor itself. An optional input semaphore is part of the new design. For platforms that support true asynchronous event notification, the semaphore will be signaled to indicate pending events. On platforms that do not support asynchronous notifications about events, the UI will have to poll EventSensor periodically to read events from the VM. Instance variables: mouseButtons <Integer> - mouse button state as replacement for primMouseButtons mousePosition <Point> - mouse position as replacement for primMousePt keyboardBuffer <SharedQueue> - keyboard input buffer interruptKey <Integer> - currently defined interrupt key interruptSemaphore <Semaphore> - the semaphore signaled when the interruptKey is detected eventQueue <SharedQueue> - an optional event queue for event driven applications inputSemaphore <Semaphore>- the semaphore signaled by the VM if asynchronous event notification is supported lastEventPoll <Integer> - the last millisecondClockValue at which we called fetchMoreEvents hasInputSemaphore <Boolean> - true if my inputSemaphore has actually been signaled at least once. Class variables: EventPollPeriod <Integer> - the number of milliseconds to wait between polling for more events in the userInterruptHandler. EventTicklerProcess <Process> - the process that makes sure that events are polled for often enough (at least every EventPollPeriod milliseconds). Event format: The current event format is very simple. Each event is recorded into an 8 element array. All events must provide some SmallInteger ID (the first field in the event buffer) and a time stamp (the second field in the event buffer), so that the difference between the time stamp of an event and the current time can be reported. Currently, the following events are defined: Null event ============= The Null event is returned when the ST side asks for more events but no more events are available. Structure: [1] - event type 0 [2-8] - unused Mouse event structure ========================== Mouse events are generated when mouse input is detected. Structure: [1] - event type 1 [2] - time stamp [3] - mouse x position [4] - mouse y position [5] - button state; bitfield with the following entries: 1 - mouseButton2 (e.g., right) button 2 - mouseButton3 (e.g., middle) button 4 - mouseButton1 (e.g., left) button [all other bits are currently undefined] [6] - modifier keys; bitfield with the following entries: 1 - shift key 2 - ctrl key 4 - (Mac specific) option key 8 - Cmd/Alt key [all other bits are currently undefined] [7] - reserved. [8] - reserved. Keyboard events ==================== Keyboard events are generated when keyboard input is detected. [1] - event type 2 [2] - time stamp [3] - character code For now the character code is in Mac Roman encoding. [4] - press state; integer with the following meaning 0 - character 1 - key press (down) 2 - key release (up) [5] - modifier keys (same as in mouse events) [6] - reserved. [7] - reserved. [8] - reserved.

### Methods
#### EventSensor>>#peekEvent

Look ahead at the next event.


<details>
	<summary>See more</summary>
	
	peekEvent
	"Look ahead at the next event."

	self fetchMoreEvents.
	^eventQueue peek
</details>

#### EventSensor>>#peekMousePt

<details>
	<summary>See more</summary>
	
	peekMousePt
	^mousePosition
</details>

#### EventSensor>>#primSetInterruptKey: anInteger

Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>.


<details>
	<summary>See more</summary>
	
	primSetInterruptKey: anInteger
	"Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>."
	interruptKey _ anInteger.
	"backward compatibility: use the old primitive which is obsolete now"
	super primSetInterruptKey: anInteger
</details>

#### EventSensor>>#flushNonKbdEvents

<details>
	<summary>See more</summary>
	
	flushNonKbdEvents

	eventQueue flushAllSuchThat: [ :buf |
		(self isKbdEvent: buf) not ]
</details>

#### EventSensor>>#flushAllButDandDEvents

<details>
	<summary>See more</summary>
	
	flushAllButDandDEvents
	| newQueue oldQueue  |
	
	newQueue _ SharedQueue new.
	eventQueue ifNil: [
		eventQueue _ newQueue.
		^self].
	oldQueue _ eventQueue.
	[ oldQueue size > 0 ] whileTrue: [ | item type | 
		item _ oldQueue next.
		type _ item at: 1.
		type = EventSensor eventTypeDragDropFiles ifTrue: [ newQueue nextPut: item]].
	eventQueue _ newQueue.

</details>

#### EventSensor>>#fetchMoreEvents

Fetch more events from the VM


<details>
	<summary>See more</summary>
	
	fetchMoreEvents
	"Fetch more events from the VM"

	self fetchMoreEventsDiscardingMouseEvents: false
</details>

#### EventSensor>>#primKbdNext

Allows for use of old Sensor protocol to get at the keyboard, as when running kbdTest or the InterpreterSimulator in Morphic


<details>
	<summary>See more</summary>
	
	primKbdNext
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running kbdTest or the InterpreterSimulator in Morphic"
	| evtBuf |
	self fetchMoreEvents.
	evtBuf _ eventQueue nextOrNilSuchThat: [ :buf | self isKbdEvent: buf].
	self flushNonKbdEvents.
	^ evtBuf ifNotNil: [ evtBuf at: 3 ]
</details>

#### EventSensor>>#primKbdPeek

Allows for use of old Sensor protocol to get at the keyboard, as when running kbdTest or the InterpreterSimulator in Morphic


<details>
	<summary>See more</summary>
	
	primKbdPeek
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running kbdTest or the InterpreterSimulator in Morphic"
	| char |
	self fetchMoreEvents.
	char _ nil.
	"NOTE: must not return out of this block, so loop to end"
	eventQueue nextOrNilSuchThat: [ :buf |
		(self isKbdEvent: buf) ifTrue: [
			char ifNil: [ char _ buf at: 3 ]].
		false  "NOTE: block value must be false so Queue won't advance"].
	^ char
</details>

#### EventSensor>>#isKbdEvent: buf

<details>
	<summary>See more</summary>
	
	isKbdEvent: buf
	^ (buf at: 1) = EventSensor eventTypeKeyboard and: [(buf at: 4) = EventSensor eventKeyChar]
</details>

#### EventSensor>>#processKeyboardSensorEvent: evt

process a keyboard event, updating InputSensor state


<details>
	<summary>See more</summary>
	
	processKeyboardSensorEvent: evt
	"process a keyboard event, updating InputSensor state"

	mouseButtons _ (mouseButtons bitAnd: 7) bitOr: ((evt at: 5) bitShift: 3)
</details>

#### EventSensor>>#test

Sensor test


<details>
	<summary>See more</summary>
	
	test
	"
	Sensor test
	"
	| char lastStamp stamp |
	char _ nil.
	lastStamp _ 0.
	[ char = $x ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp _ evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ '' print ].
			self printEventBuffer: evt.
			Transcript show: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				char _ Character numericValue: evt third ].
			lastStamp _ stamp]]
</details>

#### EventSensor>>#primGetNextEvent: array

Store the next OS event available into the provided array. Essential. If the VM is not event driven the ST code will fall back to the old-style mechanism and use the state based primitives instead.


<details>
	<summary>See more</summary>
	
	primGetNextEvent: array
	"Store the next OS event available into the provided array.
	Essential. If the VM is not event driven the ST code will fall
	back to the old-style mechanism and use the state based
	primitives instead."
	| kbd buttons modifiers pos mapped |
	<primitive: 94>
	"Simulate the events"
	array at: 1 put: EventSensor eventTypeNone. "assume no more events"

	"First check for keyboard"
	kbd _ super primKbdNext.
	kbd ifNotNil: [
		"simulate keyboard event"
		array at: 1 put: EventSensor eventTypeKeyboard. "evt type"
		array at: 2 put: Time localMillisecondClock. "time stamp"
		array at: 3 put: (kbd bitAnd: 255). "char code"
		array at: 4 put: EventSensor eventKeyChar. "key press/release"
		array at: 5 put: (kbd bitShift: -8). "modifier keys"
		^self].

	"Then check for mouse"
	buttons _ super primMouseButtons.
	pos _ super primMousePt.
	modifiers _ buttons bitShift: -3.
	buttons _ buttons bitAnd: 7.
	mapped _ self mapButtons: buttons modifiers: modifiers.
	(pos = mousePosition and:[(mapped bitOr: (modifiers bitShift: 3)) = mouseButtons])
		ifTrue:[^self].
	array 
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time localMillisecondClock;
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.

</details>

#### EventSensor>>#eventTicklerProcess

Answer my event tickler process, if any


<details>
	<summary>See more</summary>
	
	eventTicklerProcess
	"Answer my event tickler process, if any"
	^EventTicklerProcess
</details>

#### EventSensor>>#peekPosition

<details>
	<summary>See more</summary>
	
	peekPosition
	self fetchMoreEvents.
	^mousePosition
</details>

#### EventSensor>>#nextEvent

Return the next event from the receiver.


<details>
	<summary>See more</summary>
	
	nextEvent
	"Return the next event from the receiver."
	eventQueue isEmpty ifTrue: [ self fetchMoreEvents ].
	^ eventQueue isEmpty
		ifFalse: [ eventQueue next ]
</details>

#### EventSensor>>#lastEventPoll

Answer the last clock value at which fetchMoreEvents was called.


<details>
	<summary>See more</summary>
	
	lastEventPoll
	"Answer the last clock value at which fetchMoreEvents was called."
	^lastEventPoll ifNil: [ lastEventPoll _ Time localMillisecondClock ]
</details>

#### EventSensor>>#printEventBuffer: evtBuf

<details>
	<summary>See more</summary>
	
	printEventBuffer: evtBuf

	| type buttons macRomanCode modifiers position pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time localMillisecondClock ].
	type = EventSensor eventTypeMouse
		ifTrue: [
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			Transcript
				newLine;
				show: type;
				show: ' macRomanCode:', macRomanCode printString, '-', 
					(Character numericValue: (Character macRomanToLatin1: macRomanCode)) asString, '-';
				show: ' unicodeCodePoint:', unicodeCodePoint printString.
			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |
				Transcript show: '-', (Character numericValue: latin15) asString, '-' ].
			Transcript
				show: ' modifiers:', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].
			].
</details>

#### EventSensor>>#installEventTickler

Initialize the event tickler process. Terminate the old process if any.


<details>
	<summary>See more</summary>
	
	installEventTickler
	"Initialize the event tickler process. Terminate the old process if any."
	"
	Sensor installEventTickler
	"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: 'Event Tickler'.
	EventTicklerProcess resume
</details>

#### EventSensor>>#queueEvent: evt

Queue the given event in the event queue (if any). Note that the event buffer must be copied since it will be reused later on.


<details>
	<summary>See more</summary>
	
	queueEvent: evt
	"Queue the given event in the event queue (if any).
	Note that the event buffer must be copied since it
	will be reused later on."

	eventQueue nextPut: evt copy
</details>

#### EventSensor>>#initialize

Run the I/O process


<details>
	<summary>See more</summary>
	
	initialize
	"Run the I/O process"
	mouseButtons _ 0.
	mousePosition _ `0@0`.
	self setInterruptKey: (interruptKey ifNil: [$. numericValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore _ Semaphore new.
	hasInputSemaphore _ false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals
</details>

#### EventSensor>>#primMousePt

Primitive. Poll the mouse to find out its position. Return a Point. Fail if event-driven tracking is used instead of polling. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	primMousePt
	self fetchMoreEvents.
	self flushNonKbdEvents.
	^ mousePosition
</details>

#### EventSensor>>#flushEvents

Do nothing


<details>
	<summary>See more</summary>
	
	flushEvents
	eventQueue flush
</details>

#### EventSensor>>#peekButtons

<details>
	<summary>See more</summary>
	
	peekButtons
	self fetchMoreEvents.
	^mouseButtons
</details>

#### EventSensor>>#primMouseButtons

<details>
	<summary>See more</summary>
	
	primMouseButtons
	self fetchMoreEvents.
	self flushNonKbdEvents.
	^ mouseButtons
</details>

#### EventSensor>>#primInterruptSemaphore: aSemaphore

Primitive. Install the argument as the semaphore to be signalled whenever the user presses the interrupt key. The semaphore will be signaled once each time the interrupt key is pressed.


<details>
	<summary>See more</summary>
	
	primInterruptSemaphore: aSemaphore 
	"Primitive. Install the argument as the semaphore to be signalled whenever the user presses the interrupt key. The semaphore will be signaled once each time the interrupt key is pressed."
	interruptSemaphore _ aSemaphore
</details>

#### EventSensor>>#mapButtons: buttons modifiers: modifiers

Map the buttons to mouseButton2 or mouseButton3 based on the given modifiers. If only the mouseButton1 is pressed, then map Cmd-mouseButton1 -> mouseButton2. Ctrl-mouseButton1 -> mouseButton3.


<details>
	<summary>See more</summary>
	
	mapButtons: buttons modifiers: modifiers
	"Map the buttons to mouseButton2 or mouseButton3 based on the given modifiers.
	If only the mouseButton1 is pressed, then map
		Cmd-mouseButton1 -> mouseButton2.
		Ctrl-mouseButton1 -> mouseButton3.
	"
	(buttons = InputSensor mouseButton1)
		ifFalse: [ ^ buttons ].
	(modifiers allMask: EventSensor macOptionKey) 
		ifTrue: [ ^ InputSensor mouseButton3 ].
	(modifiers allMask: EventSensor commandAltKey) 
		ifTrue: [ ^ InputSensor mouseButton2 ].
	^buttons
</details>

#### EventSensor>>#fetchMoreEventsDiscardingMouseEvents: discardMouseEvents

Fetch more events from the VM


<details>
	<summary>See more</summary>
	
	fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore _ true.
			inputSemaphore initSignals ].

	"Remember the last time that I checked for events."
	lastEventPoll _ Time localMillisecondClock.

	eventBuffer _ Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		type _ eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ]
</details>

#### EventSensor>>#createMouseEvent

create and return a new mouse event from the current mouse position; this is useful for restarting normal event queue processing after manual polling


<details>
	<summary>See more</summary>
	
	createMouseEvent
	"create and return a new mouse event from the current mouse 
	position; this is useful for restarting normal event queue 
	processing after manual polling"

	| buttons modifiers pos mapped eventBuffer |
	eventBuffer _ Array new: 8.
	buttons _ self primMouseButtons.
	pos _ self primMousePt.
	modifiers _ buttons bitShift: -3.
	buttons _ buttons bitAnd: 7.
	mapped _ self mapButtons: buttons modifiers: modifiers.
	eventBuffer
		at: 1 put: EventSensor eventTypeMouse;
		at: 2 put: Time localMillisecondClock;
		at: 3 put: pos x;
		at: 4 put: pos y;
		at: 5 put: mapped;
		at: 6 put: modifiers.
	^ eventBuffer
</details>

#### EventSensor>>#shutDownSensor

<details>
	<summary>See more</summary>
	
	shutDownSensor
	InterruptWatcherProcess ifNotNil: [
		InterruptWatcherProcess terminate.
		InterruptWatcherProcess _ nil ].
	EventTicklerProcess ifNotNil: [
		EventTicklerProcess terminate.
		EventTicklerProcess _ nil. ].
	inputSemaphore ifNotNil: [Smalltalk unregisterExternalObject: inputSemaphore]
</details>

#### EventSensor>>#eventTickler

If the UI process hasn't processed events in a while, do it here. This is needed to detect the interrupt key.


<details>
	<summary>See more</summary>
	
	eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."
	| delay delta |
	delay _ Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.
	"ensure not nil."
	[
	[
	delay wait.
	delta _ Time localMillisecondClock - lastEventPoll.
	delta > self class eventPollPeriod ifTrue: [
		"See #doOneCycleNowFor:"
		Cursor currentCursor = Cursor defaultCursor ifTrue: [ (Cursor cursorAt: #waitCursor) activateCursor ].
		"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
					interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
					In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
					running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
					Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
					events makes the 'processing' of those very quick."
		self fetchMoreEventsDiscardingMouseEvents: true ]]
		on: Error
		do: [ :ex |
			nil ]] repeat.
</details>

#### EventSensor>>#primSetInputSemaphore: semaIndex

Set the input semaphore the VM should use for asynchronously signaling the availability of events. Primitive. Optional.


<details>
	<summary>See more</summary>
	
	primSetInputSemaphore: semaIndex
	"Set the input semaphore the VM should use for asynchronously signaling the availability of events. Primitive. Optional."
	<primitive: 93>
	^nil
</details>

#### EventSensor>>#processSensorEvent: evt discardingMouseEvents: discardMouseEvents

Process a single event. This method is run at high priority.


<details>
	<summary>See more</summary>
	
	processSensorEvent: evt discardingMouseEvents: discardMouseEvents
	"Process a single event. This method is run at high priority."
	| type |
	type _ evt at: 1.

	"Check if the event is a user interrupt"
	(type = EventSensor eventTypeKeyboard and: [ (evt at: 4) = 0 and: [
		((evt at: 3) bitOr: (((evt at: 5) bitAnd: 8) bitShift: 8)) = interruptKey]])
			 ifTrue: [
				"interrupt key is meta - not reported as event"
				^interruptSemaphore signal].

	"Store the event in the queue if there's any"
	type = EventSensor eventTypeMouse ifTrue: [
		"Only swap secondary and tertiary buttons if there is no command or option modifier keys.
		This swap is done so a 3-button mouse  is
			left -> mouseButton1 (select)
			center -> mouseButton3 (halo)
			right -> mouseButton2 (menu).
		This is only needed on the Mac, Window VM does this mapping by default.
		We avoid ding the swap if there are modifier keys, because in that case the buttons were generated by the VM as follows:
			left -> mouseButton1
			macOption + left -> mouseButton3
			command + left -> mouseButton2,
		but Mac users are already used to 
			macOption + left -> menu
			command + left -> halo.
		See #installMouseDecodeTable"
		((evt at: 6) anyMask: 12) ifFalse: [
			evt at: 5 put: (ButtonDecodeTable at: (evt at: 5) + 1)]].
	
	(discardMouseEvents and: [ type = EventSensor eventTypeMouse ]) ifFalse: [
		self queueEvent: evt ].

	"Update state for InputSensor."
	type = EventSensor eventTypeMouse ifTrue: [
		self processMouseSensorEvent: evt ].
	type = EventSensor eventTypeKeyboard ifTrue: [
		self processKeyboardSensorEvent: evt ]
</details>

#### EventSensor>>#processMouseSensorEvent: evt

process a mouse event, updating InputSensor state


<details>
	<summary>See more</summary>
	
	processMouseSensorEvent: evt
	"process a mouse event, updating InputSensor state"
	| modifiers buttons mapped |
	mousePosition _ (evt at: 3) @ (evt at: 4).
	buttons _ evt at: 5.
	modifiers _ evt at: 6.
	mapped _ self mapButtons: buttons modifiers: modifiers.
	mouseButtons _ mapped bitOr: (modifiers bitShift: 3).
</details>

## InputSensor

An InputSensor is an interface to the user input devices. There is at least one (sub)instance of InputSensor named Sensor in the system. Class variables: ButtonDecodeTable <ByteArray> - maps mouse buttons as reported by the VM to ones reported in the events. KeyDecodeTable <Dictionary<SmallInteger->SmallInteger>> - maps some keys and their modifiers to other keys (used for instance to map Ctrl-X to Alt-X) InterruptSemaphore <Semaphore> - signalled by the the VM and/or the event loop upon receiving an interrupt keystroke. InterruptWatcherProcess <Process> - waits on the InterruptSemaphore and then responds as appropriate.

### Methods
#### InputSensor>>#kbdTest

Sensor kbdTest


<details>
	<summary>See more</summary>
	
	kbdTest
	"
	Sensor kbdTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char _ nil.
	[ char = $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char _ self keyboard.
		Display fill: `5@5 extent: 400@20` fillColor: `Color white`.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				print: char numericValue;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ]])
		displayAt: 10 @ 10 ].
</details>

#### InputSensor>>#tabletExtent

Answer the full tablet extent in tablet coordinates.


<details>
	<summary>See more</summary>
	
	tabletExtent
	"Answer the full tablet extent in tablet coordinates."

	| params |
	params _ self primTabletGetParameters: 1.
	params ifNil: [^ self error: 'no tablet available'].
	^ (params at: 1)@(params at: 2)

</details>

#### InputSensor>>#hasTablet

Answer true if there is a pen tablet available on this computer.


<details>
	<summary>See more</summary>
	
	hasTablet
	"Answer true if there is a pen tablet available on this computer."

	^ (self primTabletGetParameters: 1) notNil

</details>

#### InputSensor>>#primReadJoystick: index

Return the joystick input word for the joystick with the given index in the range [1..16]. Returns zero if the index does not correspond to a currently installed joystick.


<details>
	<summary>See more</summary>
	
	primReadJoystick: index
	"Return the joystick input word for the joystick with the given index in the range [1..16]. Returns zero if the index does not correspond to a currently installed joystick."

	<primitive: 'primitiveReadJoystick' module: 'JoystickTabletPlugin'>
	^ 0

	
</details>

#### InputSensor>>#commandAltKeyPressed

Answer whether the command (Mac) / alt(Windows) key on the keyboard is being held down.


<details>
	<summary>See more</summary>
	
	commandAltKeyPressed
	"Answer whether the command (Mac) / alt(Windows) key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor commandAltKey
</details>

#### InputSensor>>#setInterruptKey: anInteger

Register the given keycode as the user interrupt key.


<details>
	<summary>See more</summary>
	
	setInterruptKey: anInteger
	"Register the given keycode as the user interrupt key."

	self primSetInterruptKey: anInteger.

</details>

#### InputSensor>>#mousePoint

Answer a Point indicating the coordinates of the current mouse location.


<details>
	<summary>See more</summary>
	
	mousePoint
	"Answer a Point indicating the coordinates of the current mouse location."

	^self primMousePt
</details>

#### InputSensor>>#testJoystick: index

Sensor testJoystick: 3


<details>
	<summary>See more</summary>
	
	testJoystick: index
	"Sensor testJoystick: 3"

	| f pt buttons status |
	f _ Form extent: 110@50.
	[Sensor isAnyButtonPressed] whileFalse: [
		pt _ Sensor joystickXY: index.
		buttons _ Sensor joystickButtons: index.
		status _
'xy: ', pt printString, '
buttons: ', buttons hex.
		f fillWhite.
		status displayOn: f at: 10@10.
		f displayOn: Display at: 10@10.
	].

</details>

#### InputSensor>>#eventTicklerProcess

Answer my event tickler process, if any


<details>
	<summary>See more</summary>
	
	eventTicklerProcess
	"Answer my event tickler process, if any"
	^nil
</details>

#### InputSensor>>#buttons

Answer the result of primMouseButtons, but swap the mouse buttons if appropriate.


<details>
	<summary>See more</summary>
	
	buttons
	"Answer the result of primMouseButtons, but swap the mouse  buttons if appropriate."
	^ ButtonDecodeTable at: self primMouseButtons + 1
</details>

#### InputSensor>>#joystickOn: index

<details>
	<summary>See more</summary>
	
	joystickOn: index

	^ (((self primReadJoystick: index) bitShift: -27) bitAnd: 1) ~= 0
	
</details>

#### InputSensor>>#primTabletRead: cursorIndex

Answer the pen tablet data for the cursor having the given index. Answer nil if there is no pen tablet. The data is: 1. index of the cursor to which this data applies 2. timestamp of the last state chance for this cursor 3., 4., and 5. x, y, and z coordinates of the cursor (z is typically 0) 6. and 7. xTilt and yTilt of the cursor; (signed) 8. type of cursor (0 = unknown, 1 = pen, 2 = puck, 3 = eraser) 9. cursor buttons 10. cursor pressure, downward 11. cursor pressure, tangential 12. flags


<details>
	<summary>See more</summary>
	
	primTabletRead: cursorIndex
	"Answer the pen tablet data for the cursor having the given index. Answer nil if there is no pen tablet. The data is:
	1. index of the cursor to which this data applies
	2. timestamp of the last state chance for this cursor
	3., 4., and 5. x, y, and z coordinates of the cursor (z is typically 0)
	6. and 7. xTilt and yTilt of the cursor; (signed)
	8. type of cursor (0 = unknown, 1 = pen, 2 = puck, 3 = eraser)
	9. cursor buttons
	10. cursor pressure, downward
	11. cursor pressure, tangential
	12. flags"

	<primitive: 'primitiveReadTablet' module: 'JoystickTabletPlugin'>
	self primitiveFailed

</details>

#### InputSensor>>#rawMacOptionKeyPressed

Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific. Clients are discouraged from calling this directly, since it circumvents bert's attempt to eradicate option-key checks


<details>
	<summary>See more</summary>
	
	rawMacOptionKeyPressed
	"Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific.  Clients are discouraged from calling this directly, since it circumvents bert's attempt to eradicate option-key checks"

	^ self primMouseButtons anyMask: InputSensor macOptionKey
</details>

#### InputSensor>>#keyboardPeek

Answer the next character in the keyboard buffer without removing it, or nil if it is empty.


<details>
	<summary>See more</summary>
	
	keyboardPeek
	"Answer the next character in the keyboard buffer without removing it, or nil if it is empty."

	^ self characterForKeycode: self primKbdPeek
</details>

#### InputSensor>>#macOptionKeyPressed

Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific.


<details>
	<summary>See more</summary>
	
	macOptionKeyPressed
	"Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific."

	self notify: 'Portability note:
InputSensor>>macOptionKeyPressed is not portable.
Please use InputSensor>>mouseButton2Pressed instead!'.

	^ self rawMacOptionKeyPressed
</details>

#### InputSensor>>#joystickButtons: index

<details>
	<summary>See more</summary>
	
	joystickButtons: index

	^ ((self primReadJoystick: index) bitShift: -22) bitAnd: 16r71F
	
</details>

#### InputSensor>>#waitClickButton

Wait for the user to click (press and then release) any mouse button and then answer with the current location of the cursor.


<details>
	<summary>See more</summary>
	
	waitClickButton
	"Wait for the user to click (press and then release) any mouse button and 
	then answer with the current location of the cursor."

	self waitButton.
	^self waitNoButton
</details>

#### InputSensor>>#noButtonPressed

Answer whether any mouse button is not being pressed.


<details>
	<summary>See more</summary>
	
	noButtonPressed
	"Answer whether any mouse button is not being pressed."

	^self isAnyButtonPressed not

</details>

#### InputSensor>>#tabletTimestamp

Answer the time (in tablet clock ticks) at which the tablet's primary pen last changed state. This can be used in polling loops; if this timestamp hasn't changed, then the pen state hasn't changed either.


<details>
	<summary>See more</summary>
	
	tabletTimestamp
	"Answer the time (in tablet clock ticks) at which the tablet's primary pen last changed state. This can be used in polling loops; if this timestamp hasn't changed, then the pen state hasn't changed either."

	| data |
	data _ self primTabletRead: 1.  "state of first/primary pen"
	^ data at: 2

</details>

#### InputSensor>>#flushEvents

Do nothing


<details>
	<summary>See more</summary>
	
	flushEvents
	"Do nothing"
</details>

#### InputSensor>>#peekButtons

<details>
	<summary>See more</summary>
	
	peekButtons
	^self primMouseButtons
</details>

#### InputSensor>>#mousePoint: aPoint

Set aPoint to be the current cursor location.


<details>
	<summary>See more</summary>
	
	mousePoint: aPoint 
	"Set aPoint to be the current cursor location."

	^self primCursorLocPut: aPoint
</details>

#### InputSensor>>#shiftPressed

Answer whether the shift key on the keyboard is being held down.


<details>
	<summary>See more</summary>
	
	shiftPressed
	"Answer whether the shift key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor shiftKey
</details>

#### InputSensor>>#installInterruptWatcher

Initialize the interrupt watcher process. Terminate the old process if any. This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger.


<details>
	<summary>See more</summary>
	
	installInterruptWatcher
	"Initialize the interrupt watcher process. Terminate the old process if any.
	This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger."
	"
	Sensor installInterruptWatcher
	"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore _ Semaphore new.
	InterruptWatcherProcess _ [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor lowIOPriority.
	InterruptWatcherProcess name: 'User interrupt watcher'.
	InterruptWatcherProcess resume.
	self primInterruptSemaphore: InterruptSemaphore
</details>

#### InputSensor>>#isMouseButton3Pressed

Answer whether only the mouseButton3 is being pressed. This is the third mouse button, usually the wheel or button at the center, or cmd+click on the Mac.


<details>
	<summary>See more</summary>
	
	isMouseButton3Pressed
	"Answer whether only the mouseButton3 is being pressed. 
	This is the third mouse button, usually the wheel or button at the center, or cmd+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton3
</details>

#### InputSensor>>#waitButtonOrKeyboard

Wait for the user to press either any mouse button or any key. Answer the current cursor location or nil if a keypress occured.


<details>
	<summary>See more</summary>
	
	waitButtonOrKeyboard
	"Wait for the user to press either any mouse button or any key. 
	Answer the current cursor location or nil if a keypress occured."

	| delay |
	delay := Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ]
		whileFalse: [
			delay wait.
			self keyboardPressed
				ifTrue: [^ nil]].
	^ self mousePoint

</details>

#### InputSensor>>#primCursorLocPutAgain: aPoint

Do nothing if primitive is not implemented.


<details>
	<summary>See more</summary>
	
	primCursorLocPutAgain: aPoint
	"Do nothing if primitive is not implemented."

	<primitive: 91>
	^ self
</details>

#### InputSensor>>#isMouseButton1Pressed

Answer true if only the mouseButton1 is being pressed. This is the first mouse button, usually the one at the left.


<details>
	<summary>See more</summary>
	
	isMouseButton1Pressed
	"Answer true if only the mouseButton1 is being pressed.
	This is the first mouse button, usually the one at the left."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton1
</details>

#### InputSensor>>#peekMousePt

<details>
	<summary>See more</summary>
	
	peekMousePt
	^self primMousePt
</details>

#### InputSensor>>#primSetInterruptKey: anInteger

Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>.


<details>
	<summary>See more</summary>
	
	primSetInterruptKey: anInteger
	"Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>."

	<primitive: 133>
	^self primitiveFailed
"Note: This primitive is obsolete with the new event driven architecture in which EventSensor can handle the interrupts itself. However, for supporting older images running on newer VMs the primitive must still be implemented."
</details>

#### InputSensor>>#primKbdNext

<details>
	<summary>See more</summary>
	
	primKbdNext
	<primitive: 108>
	^ nil
</details>

#### InputSensor>>#mouseButtons

Answer a number from 0 to 7 that encodes the state of the three mouse buttons in its lowest 3 bits.


<details>
	<summary>See more</summary>
	
	mouseButtons
	"Answer a number from 0 to 7 that encodes the state of the three mouse buttons in its lowest 3 bits."

	^ self primMouseButtons bitAnd: 7

</details>

#### InputSensor>>#controlKeyPressed

Answer whether the control/ctrl key on the keyboard is being held down.


<details>
	<summary>See more</summary>
	
	controlKeyPressed
	"Answer whether the control/ctrl key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor controlKey
</details>

#### InputSensor>>#characterForKeycode: keycode

Map the given keycode to a Smalltalk character object. Encoding: A keycode is 12 bits: <4 modifer bits><8 bit ISO character> Modifier bits are: <command><option><control><shift>


<details>
	<summary>See more</summary>
	
	characterForKeycode: keycode
	"Map the given keycode to a Smalltalk character object. Encoding:
		A keycode is 12 bits:   <4 modifer bits><8 bit ISO character>
		Modifier bits are:       <command><option><control><shift>"

	"NOTE: the command and option keys are specific to the Macintosh and may not have equivalents on other platforms."

	keycode ifNil: [ ^nil ].
	^ Character numericValue: (keycode bitAnd: 16rFF)
</details>

#### InputSensor>>#primKbdPeek

<details>
	<summary>See more</summary>
	
	primKbdPeek
	<primitive: 109>
	^ nil
</details>

#### InputSensor>>#joystickXY: index

<details>
	<summary>See more</summary>
	
	joystickXY: index

	| inputWord x y |
	inputWord _ self primReadJoystick: index.
	x _ (inputWord bitAnd: 16r7FF) - 16r400.
	y _ ((inputWord bitShift: -11) bitAnd: 16r7FF) - 16r400.
	^ x@y
	
</details>

#### InputSensor>>#primCursorLocPut: aPoint

If the primitive fails, try again with a rounded point.


<details>
	<summary>See more</summary>
	
	primCursorLocPut: aPoint
	"If the primitive fails, try again with a rounded point."

	<primitive: 91>
	^ self primCursorLocPutAgain: aPoint rounded
</details>

#### InputSensor>>#isMouseButton2Pressed

Answer whether only the mouseButton2 is being pressed. This is the usually the right mouse button or option+click on the Mac.


<details>
	<summary>See more</summary>
	
	isMouseButton2Pressed
	"Answer whether only the mouseButton2 is being pressed. 
	This is the usually the right mouse button or option+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton2
</details>

#### InputSensor>>#primTabletGetParameters: cursorIndex

Answer the pen tablet parameters. For parameters that differ from cursor to cursor, answer those associated with the cursor having the given index. Answer nil if there is no pen tablet. The parameters are: 1. tablet width, in tablet units 2. tablet height, in tablet units 3. number of tablet units per inch 4. number of cursors (pens, pucks, etc; some tablets have more than one) 5. this cursor index 6. and 7. x scale and x offset for scaling tablet coordintes (e.g., to fit the screen) 8. and 9. y scale and y offset for scaling tablet coordintes (e.g., to fit the screen) 10. number of pressure levels 11. presure threshold needed close pen tip switch 12. number of pen tilt angles


<details>
	<summary>See more</summary>
	
	primTabletGetParameters: cursorIndex
	"Answer the pen tablet parameters. For parameters that differ from cursor to cursor, answer those associated with the cursor having the given index. Answer nil if there is no pen tablet. The parameters are:
	1. tablet width, in tablet units
	2. tablet height, in tablet units
	3. number of tablet units per inch
	4. number of cursors (pens, pucks, etc; some tablets have more than one)
	5. this cursor index
	6. and 7. x scale and x offset for scaling tablet coordintes (e.g., to fit the screen)
	8. and 9. y scale and y offset for scaling tablet coordintes  (e.g., to fit the screen)
	10. number of pressure levels
	11. presure threshold needed close pen tip switch 
	12. number of pen tilt angles"

	<primitive: 'primitiveGetTabletParameters' module: 'JoystickTabletPlugin'>
	^ nil

</details>

#### InputSensor>>#keyboard

Answer the next character from the keyboard.


<details>
	<summary>See more</summary>
	
	keyboard
	"Answer the next character from the keyboard."

	^ self characterForKeycode: self primKbdNext
</details>

#### InputSensor>>#waitNoButton

Wait for the user to release any mouse button and then answer the current location of the cursor.


<details>
	<summary>See more</summary>
	
	waitNoButton
	"Wait for the user to release any mouse button and then answer the current location of the cursor."

	| delay |
	delay _ Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileTrue: [ delay wait].
	^self mousePoint

</details>

#### InputSensor>>#isAnyButtonPressed

Answer whether at least one mouse button is currently being pressed.


<details>
	<summary>See more</summary>
	
	isAnyButtonPressed
	"Answer whether at least one mouse button is currently being pressed."

	^ self primMouseButtons anyMask: InputSensor anyMouseButton

</details>

#### InputSensor>>#anyModifierKeyPressed

ignore, however, the shift keys 'cause that's not REALLY a command key


<details>
	<summary>See more</summary>
	
	anyModifierKeyPressed
	"ignore, however, the shift keys 'cause that's not REALLY a command key"

	^ self primMouseButtons anyMask: InputSensor anyModifierKey
</details>

#### InputSensor>>#primMousePt

Primitive. Poll the mouse to find out its position. Return a Point. Fail if event-driven tracking is used instead of polling. Optional. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	primMousePt
	"Primitive. Poll the mouse to find out its position. Return a Point. Fail if
	event-driven tracking is used instead of polling. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 90>
	^ `0@0`
</details>

#### InputSensor>>#flushKeyboard

Remove all characters from the keyboard buffer.


<details>
	<summary>See more</summary>
	
	flushKeyboard
	"Remove all characters from the keyboard buffer."

	[self keyboardPressed]
		whileTrue: [self keyboard]
</details>

#### InputSensor>>#primMouseButtons

<details>
	<summary>See more</summary>
	
	primMouseButtons
	<primitive: 107>
	^ 0
</details>

#### InputSensor>>#userInterruptWatcher

Wait for user interrupts and open a notifier on the active process when one occurs.


<details>
	<summary>See more</summary>
	
	userInterruptWatcher
	"Wait for user interrupts and open a notifier on the active process when one occurs."

	[ true ] whileTrue: [
		InterruptSemaphore wait.
"		Display deferUpdates: false."
		Smalltalk at: #SoundPlayer ifPresent: [ :cls | cls shutDown ].
		Smalltalk handleUserInterrupt]
</details>

#### InputSensor>>#waitButton

Wait for the user to press any mouse button and then answer with the current location of the cursor.


<details>
	<summary>See more</summary>
	
	waitButton
	"Wait for the user to press any mouse button and then answer with the 
	current location of the cursor."

	| delay |
	delay _ Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileFalse: [ delay wait ].
	^self mousePoint

</details>

#### InputSensor>>#interruptWatcherProcess

Answer my interrupt watcher process, if any


<details>
	<summary>See more</summary>
	
	interruptWatcherProcess
	"Answer my interrupt watcher process, if any"
	^InterruptWatcherProcess
</details>

#### InputSensor>>#keyboardPressed

Answer true if keystrokes are available.


<details>
	<summary>See more</summary>
	
	keyboardPressed
	"Answer true if keystrokes are available."

	^self primKbdPeek notNil
</details>

#### InputSensor>>#tabletPoint

Answer the current position of the first tablet pointing device (pen, puck, or eraser) in tablet coordinates.


<details>
	<summary>See more</summary>
	
	tabletPoint
	"Answer the current position of the first tablet pointing device (pen, puck, or eraser) in tablet coordinates."

	| data |
	data _ self primTabletRead: 1.  "state of first/primary pen"
	^ (data at: 3) @ (data at: 4)

</details>

#### InputSensor>>#tabletPressure

Answer the current pressure of the first tablet pointing device (pen, puck, or eraser), a number between 0.0 (no pressure) and 1.0 (max pressure)


<details>
	<summary>See more</summary>
	
	tabletPressure
	"Answer the current pressure of the first tablet pointing device (pen, puck, or eraser), a number between 0.0 (no pressure) and 1.0 (max pressure)"

	| params data |
	params _ self primTabletGetParameters: 1.
	params ifNil: [^ self].
	data _ self primTabletRead: 1.  "state of first/primary pen"
	^ (data at: 10) asFloat / ((params at: 10) - 1)

</details>

## Monitor

A monitor provides process synchronization that is more high level than the one provided by a Semaphore. Similar to the classical definition of a Monitor it has the following properties: 1) At any time, only one process can execute code inside a critical section of a monitor. 2) A monitor is reentrant, which means that the active process in a monitor never gets blocked when it enters a (nested) critical section of the same monitor. 3) Inside a critical section, a process can wait for an event that may be coupled to a certain condition. If the condition is not fulfilled, the process leaves the monitor temporarily (in order to let other processes enter) and waits until another process signals the event. Then, the original process checks the condition again (this is often necessary because the state of the monitor could have changed in the meantime) and continues if it is fulfilled. 4) The monitor is fair, which means that the process that is waiting on a signaled condition the longest gets activated first. 5) The monitor allows you to define timeouts after which a process gets activated automatically. Basic usage: Monitor>>critical: aBlock Critical section. Executes aBlock as a critical section. At any time, only one process can execute code in a critical section. NOTE: All the following synchronization operations are only valid inside the critical section of the monitor! Monitor>>wait Unconditional waiting for the default event. The current process gets blocked and leaves the monitor, which means that the monitor allows another process to execute critical code. When the default event is signaled, the original process is resumed. Monitor>>waitWhile: aBlock Conditional waiting for the default event. The current process gets blocked and leaves the monitor only if the argument block evaluates to true. This means that another process can enter the monitor. When the default event is signaled, the original process is resumed, which means that the condition (argument block) is checked again. Only if it evaluates to false, does execution proceed. Otherwise, the process gets blocked and leaves the monitor again... Monitor>>waitUntil: aBlock Conditional waiting for the default event. See Monitor>>waitWhile: aBlock. Monitor>>signal One process waiting for the default event is woken up. Monitor>>signalAll All processes waiting for the default event are woken up. Using non-default (specific) events: Monitor>>waitFor: aSymbol Unconditional waiting for the non-default event represented by the argument symbol. Same as Monitor>>wait, but the process gets only reactivated by the specific event and not the default event. Monitor>>waitWhile: aBlock for: aSymbol Confitional waiting for the non-default event represented by the argument symbol. Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific event and not the default event. Monitor>>waitUntil: aBlock for: aSymbol Confitional waiting for the non-default event represented by the argument symbol. See Monitor>>waitWhile:for: aBlock. Monitor>>signal: aSymbol One process waiting for the given event is woken up. If there is no process waiting for this specific event, a process waiting for the default event gets resumed. Monitor>>signalAll: aSymbol All process waiting for the given event or the default event are woken up. Monitor>>signalReallyAll All processes waiting for any events (default or specific) are woken up. Using timeouts Monitor>>waitMaxMilliseconds: anInteger Monitor>>waitFor: aSymbol maxMilliseconds: anInteger Same as Monitor>>wait (resp. Monitor>>waitFor:), but the process gets automatically woken up when the specified time has passed. Monitor>>waitWhile: aBlock maxMilliseconds: anInteger Monitor>>waitWhile: aBlock for: aSymbol maxMilliseconds: anInteger Same as Monitor>>waitWhile: (resp. Monitor>>waitWhile:for:), but the process gets automatically woken up when the specified time has passed. Monitor>>waitUntil: aBlock maxMilliseconds: anInteger Monitor>>waitUntil: aBlock for: aSymbol maxMilliseconds: anInteger Same as Monitor>>waitUntil: (resp. Monitor>>waitUntil:for:), but the process gets automatically woken up when the specified time has passed.

### Methods
#### Monitor>>#signalAll: aSymbolOrNil

All process waiting for the given event or the default event are woken up.


<details>
	<summary>See more</summary>
	
	signalAll: aSymbolOrNil
	"All process waiting for the given event or the default event are woken up."

	| queue |
	self checkOwnerProcess.
	queue _ self queueFor: aSymbolOrNil.
	self signalAllInQueue: self defaultQueue.
	queue ~~ self defaultQueue ifTrue: [self signalAllInQueue: queue].
</details>

#### Monitor>>#wait

Unconditional waiting for the default event. The current process gets blocked and leaves the monitor, which means that the monitor allows another process to execute critical code. When the default event is signaled, the original process is resumed.


<details>
	<summary>See more</summary>
	
	wait
	"Unconditional waiting for the default event.
	The current process gets blocked and leaves the monitor, which means that the monitor
	allows another process to execute critical code. When the default event is signaled, the
	original process is resumed."

	^ self waitMaxMilliseconds: nil
</details>

#### Monitor>>#signalLock: aSemaphore inQueue: anOrderedCollection

<details>
	<summary>See more</summary>
	
	signalLock: aSemaphore inQueue: anOrderedCollection
	queuesMutex critical: [
		aSemaphore signal.
		anOrderedCollection remove: aSemaphore ifAbsent: nil
	]
</details>

#### Monitor>>#waitFor: aSymbolOrNil

Unconditional waiting for the non-default event represented by the argument symbol. Same as Monitor>>wait, but the process gets only reactivated by the specific event and not the default event.


<details>
	<summary>See more</summary>
	
	waitFor: aSymbolOrNil
	"Unconditional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>wait, but the process gets only reactivated by the specific event and 
	not the default event."

	^ self waitFor: aSymbolOrNil maxMilliseconds: nil
</details>

#### Monitor>>#critical: aBlock

Critical section. Executes aBlock as a critical section. At any time, only one process can be executing code in a critical section. NOTE: All the following synchronization operations are only valid inside the critical section of the monitor!


<details>
	<summary>See more</summary>
	
	critical: aBlock
	"Critical section.
	Executes aBlock as a critical section. At any time, only one process can be executing code 
	in a critical section.
	NOTE: All the following synchronization operations are only valid inside the critical section 
	of the monitor!"

	^[
	self enter.
	aBlock value]
		ensure: [self exit].
</details>

#### Monitor>>#exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil

<details>
	<summary>See more</summary>
	
	exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil

	[ 
		| lock |
		lock := queuesMutex critical: [ anOrderedCollection addLast: Semaphore new ].
		self exit.
		anIntegerOrNil 
			ifNil: [ lock wait 	]
			ifNotNil: [
				| delay |
				delay := MonitorDelay 
					signalLock: lock
					afterMSecs: anIntegerOrNil
					inMonitor: self
					queue: anOrderedCollection.
				[ lock wait ] ensure: [ delay unschedule ] ] ]
		ensure: [ self enter ]
</details>

#### Monitor>>#privateCleanup

<details>
	<summary>See more</summary>
	
	privateCleanup
	queuesMutex critical: [
		defaultQueue isEmpty ifTrue: [defaultQueue _ nil].
		queueDict ifNotNil: [
			queueDict copy keysAndValuesDo: [:id :queue | 
				queue isEmpty ifTrue: [queueDict removeKey: id]].
			queueDict isEmpty ifTrue: [queueDict _ nil].
		].
	].
</details>

#### Monitor>>#queueFor: aSymbol

<details>
	<summary>See more</summary>
	
	queueFor: aSymbol
	aSymbol ifNil: [^ self defaultQueue].
	^ self queueDict 
		at: aSymbol 
		ifAbsent: [self queueDict at: aSymbol put: OrderedCollection new].
</details>

#### Monitor>>#waitUntil: aBlock

Conditional waiting for the default event. See Monitor>>waitWhile: aBlock.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock
	"Conditional waiting for the default event.
	See Monitor>>waitWhile: aBlock."

	^ self waitUntil: aBlock for: nil
</details>

#### Monitor>>#cleanup

<details>
	<summary>See more</summary>
	
	cleanup
	self checkOwnerProcess.
	self critical: [self privateCleanup].
</details>

#### Monitor>>#waitUntil: aBlock for: aSymbolOrNil maxSeconds: aNumber

Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock for: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#defaultQueue

<details>
	<summary>See more</summary>
	
	defaultQueue
	defaultQueue ifNil: [defaultQueue _ OrderedCollection new].
	^ defaultQueue
</details>

#### Monitor>>#waitWhile: aBlock for: aSymbolOrNil

Confitional waiting for the non-default event represented by the argument symbol. Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific event and not the default event.


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock for: aSymbolOrNil
	"Confitional waiting for the non-default event represented by the argument symbol.
	Same as Monitor>>waitWhile:for:, but the process gets only reactivated by the specific 
	event and not the default event."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: nil
</details>

#### Monitor>>#enter

<details>
	<summary>See more</summary>
	
	enter
	self isOwnerProcess ifTrue: [
		nestingLevel _ nestingLevel + 1.
	] ifFalse: [
		mutex wait.
		ownerProcess _ Processor activeProcess.
		nestingLevel _ 1.
	].
</details>

#### Monitor>>#waitWhile: aBlock

Conditional waiting for the default event. The current process gets blocked and leaves the monitor only if the argument block evaluates to true. This means that another process can enter the monitor. When the default event is signaled, the original process is resumed, which means that the condition (argument block) is checked again. Only if it evaluates to false, does execution proceed. Otherwise, the process gets blocked and leaves the monitor again...


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock
	"Conditional waiting for the default event.
	The current process gets blocked and leaves the monitor only if the argument block
	evaluates to true. This means that another process can enter the monitor. When the 
	default event is signaled, the original process is resumed, which means that the condition
	(argument block) is checked again. Only if it evaluates to false, does execution proceed.
	Otherwise, the process gets blocked and leaves the monitor again..."

	^ self waitWhile: aBlock for: nil
</details>

#### Monitor>>#waitWhile: aBlock for: aSymbolOrNil maxSeconds: aNumber

Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock for: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil

Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitWhile: aBlock inQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.
</details>

#### Monitor>>#signal

One process waiting for the default event is woken up.


<details>
	<summary>See more</summary>
	
	signal
	"One process waiting for the default event is woken up."

	^ self signal: nil
</details>

#### Monitor>>#signalAllInQueue: anOrderedCollection

<details>
	<summary>See more</summary>
	
	signalAllInQueue: anOrderedCollection

	queuesMutex critical: [
		anOrderedCollection removeAllSuchThat: [ :each |
			each signal.
			true ] ]
</details>

#### Monitor>>#waitWhile: aBlock inQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil

<details>
	<summary>See more</summary>
	
	waitWhile: aBlock inQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil
	[aBlock value] whileTrue: [self exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil].
</details>

#### Monitor>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	mutex _ Semaphore forMutualExclusion.
	queuesMutex _ Semaphore forMutualExclusion.
	nestingLevel _ 0.
</details>

#### Monitor>>#waitUntil: aBlock for: aSymbolOrNil

Confitional waiting for the non-default event represented by the argument symbol. See Monitor>>waitWhile:for: aBlock.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock for: aSymbolOrNil
	"Confitional waiting for the non-default event represented by the argument symbol.
	See Monitor>>waitWhile:for: aBlock."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: nil
</details>

#### Monitor>>#signal: aSymbolOrNil

One process waiting for the given event is woken up. If there is no process waiting for this specific event, a process waiting for the default event gets resumed.


<details>
	<summary>See more</summary>
	
	signal: aSymbolOrNil
	"One process waiting for the given event is woken up. If there is no process waiting 
	for this specific event, a process waiting for the default event gets resumed."

	| queue |
	self checkOwnerProcess.
	queue _ self queueFor: aSymbolOrNil.
	queue isEmpty ifTrue: [queue _ self defaultQueue].
	self signalQueue: queue.
</details>

#### Monitor>>#signalAll

All processes waiting for the default event are woken up.


<details>
	<summary>See more</summary>
	
	signalAll
	"All processes waiting for the default event are woken up."

	^ self signalAll: nil
</details>

#### Monitor>>#waitWhile: aBlock maxSeconds: aNumber

Same as Monitor>>waitWhile:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock maxSeconds: aNumber
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock maxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#waitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil

<details>
	<summary>See more</summary>
	
	waitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil
	self exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil.
</details>

#### Monitor>>#waitUntil: aBlock maxSeconds: aNumber

Same as Monitor>>waitUntil:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock maxSeconds: aNumber
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock maxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil

Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: [aBlock value not] for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
</details>

#### Monitor>>#waitMaxSeconds: aNumber

Same as Monitor>>wait, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitMaxSeconds: aNumber
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitMaxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#waitFor: aSymbolOrNil maxSeconds: aNumber

Same as Monitor>>waitFor:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitFor: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger
</details>

#### Monitor>>#signalQueue: anOrderedCollection

<details>
	<summary>See more</summary>
	
	signalQueue: anOrderedCollection

	queuesMutex critical: [
		anOrderedCollection isEmpty ifFalse: [
			anOrderedCollection removeFirst signal ] ]
</details>

#### Monitor>>#waitMaxMilliseconds: anIntegerOrNil

Same as Monitor>>wait, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitMaxMilliseconds: anIntegerOrNil
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: nil maxMilliseconds: anIntegerOrNil
</details>

#### Monitor>>#checkOwnerProcess

<details>
	<summary>See more</summary>
	
	checkOwnerProcess
	self isOwnerProcess
		ifFalse: [self error: 'Monitor access violation'].
</details>

#### Monitor>>#waitWhile: aBlock maxMilliseconds: anIntegerOrNil

Same as Monitor>>waitWhile:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitWhile: aBlock maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: nil maxMilliseconds: anIntegerOrNil
</details>

#### Monitor>>#signalReallyAll

All processes waiting for any events (default or specific) are woken up.


<details>
	<summary>See more</summary>
	
	signalReallyAll
	"All processes waiting for any events (default or specific) are woken up."

	self checkOwnerProcess.
	self signalAll.
	self queueDict valuesDo: [:queue |
		self signalAllInQueue: queue].
</details>

#### Monitor>>#queueDict

<details>
	<summary>See more</summary>
	
	queueDict
	
	^queueDict ifNil: [ queueDict := IdentityDictionary new ]
</details>

#### Monitor>>#waitUntil: aBlock maxMilliseconds: anIntegerOrNil

Same as Monitor>>waitUntil:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitUntil: aBlock maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: nil maxMilliseconds: anIntegerOrNil
</details>

#### Monitor>>#exit

<details>
	<summary>See more</summary>
	
	exit
	nestingLevel _ nestingLevel - 1.
	nestingLevel < 1 ifTrue: [
		ownerProcess _ nil.
		mutex signal
	].
</details>

#### Monitor>>#waitFor: aSymbolOrNil maxMilliseconds: anIntegerOrNil

Same as Monitor>>waitFor:, but the process gets automatically woken up when the specified time has passed.


<details>
	<summary>See more</summary>
	
	waitFor: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitInQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.
</details>

#### Monitor>>#isOwnerProcess

<details>
	<summary>See more</summary>
	
	isOwnerProcess
	^ Processor activeProcess == ownerProcess
</details>

## MonitorDelay

This is a specialization of the class Delay that is used for the implementation of the class Monitor.

### Methods
#### MonitorDelay>>#setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection

<details>
	<summary>See more</summary>
	
	setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection
	monitor _ aMonitor.
	queue _ anOrderedCollection.
	self setDelay: anInteger forSemaphore: aSemaphore.
</details>

#### MonitorDelay>>#signalWaitingProcess

The delay time has elapsed; signal the waiting process.


<details>
	<summary>See more</summary>
	
	signalWaitingProcess
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn _ false.
	monitor signalLock: delaySemaphore inQueue: queue.

</details>

## Mutex

A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested. Nested (or recursive) calls to #critical: from a single process are allowed, and the process is not blocked for this: access is granted immediately. For this reason instances of Mutex should be used for resources that support this form of multiple access (i.e. multiple simultaneous access from within a single Process). Resources that don't support this, like access to private state that changes for each call, should use a Semaphore. See Semaphore's class comment Instance variables: semaphore <Semaphore> The (primitive) semaphore used for synchronization. owner <Process> The process owning the mutex.

### Methods
#### Mutex>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	semaphore := Semaphore forMutualExclusion.
</details>

#### Mutex>>#critical: aBlock

Evaluate aBlock protected by the receiver.


<details>
	<summary>See more</summary>
	
	critical: aBlock
	"Evaluate aBlock protected by the receiver."
	| activeProcess |
	activeProcess := Processor activeProcess.
	activeProcess == owner ifTrue:[^aBlock value].
	^semaphore critical:[
		owner := activeProcess.
		aBlock ensure:[owner := nil]].
</details>

## Process

I represent an independent path of control in the system. This path of control may be stopped (by sending the message suspend) in such a way that it can later be restarted (by sending the message resume). When any one of several paths of control can be advanced, the single instance of ProcessorScheduler named Processor determines which one will actually be advanced partly using the value of priority. The threadId variable is used by multi-threaded CogVMs to control process-to-thread binding. It's required to be the fourth instance variable. See SystemDictionary >> #processHasThreadIdInstVar: for further information. (If anyone ever makes a subclass of Process, be sure to use allSubInstances in anyProcessesAbove:.)

### Methods
#### Process>>#step: aContext

Resume self until aContext is on top, or if already on top, do next step


<details>
	<summary>See more</summary>
	
	step: aContext 
	"Resume self until aContext is on top, or if already on top, do next step"

	^ self basicSuspendedContext == aContext
		ifTrue: [self step]
		ifFalse: [self complete: (self calleeOf: aContext)]
</details>

#### Process>>#ifSuspendedContextIsUnhandledErrorDo: aBlock

<details>
	<summary>See more</summary>
	
	ifSuspendedContextIsUnhandledErrorDo: aBlock

	| unhandledError |
	
	self isSuspendedContextSignalUnhandledError ifTrue: [ 
		unhandledError := suspendedContext tempAt: 1.
		(self canSearchForSignalerContextOf: unhandledError) ifTrue: [ 
			aBlock value: unhandledError ]].
</details>

#### Process>>#isBlocked

Answer true if blocked on a semaphore.


<details>
	<summary>See more</summary>
	
	isBlocked
	"Answer true if blocked on a semaphore."
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == Semaphore
</details>

#### Process>>#popTo: aContext value: aValue

Replace the suspendedContext with aContext, releasing all contexts between the currently suspendedContext and it.


<details>
	<summary>See more</summary>
	
	popTo: aContext value: aValue
	"Replace the suspendedContext with aContext, releasing all contexts 
	between the currently suspendedContext and it."

	| callee |
	self == Processor activeProcess
		ifTrue: [^ self error: 'The active process cannot pop contexts'].
	callee := (self calleeOf: aContext) ifNil: [^ self].  "aContext is on top"
	self return: callee value: aValue
</details>

#### Process>>#step

<details>
	<summary>See more</summary>
	
	step

	^ suspendedContext _ suspendedContext step
</details>

#### Process>>#stepToCallee

Step until top context changes


<details>
	<summary>See more</summary>
	
	stepToCallee
	"Step until top context changes"

	| ctxt |
	ctxt _ suspendedContext.
	[ctxt == suspendedContext] whileTrue: [
		suspendedContext _ suspendedContext step].
	^ suspendedContext
</details>

#### Process>>#animatedUI

If we are an UI process, answer the root object of that UI. For a Morphic process, it is the Morphic World. Answer nil if not an UI process.


<details>
	<summary>See more</summary>
	
	animatedUI
	"If we are an UI process, answer the root object of that UI.
	For a Morphic process, it is the Morphic World.
	Answer nil if not an UI process."

	^self triggerEvent: #animatedUI
</details>

#### Process>>#resumeAt: aPriority

<details>
	<summary>See more</summary>
	
	resumeAt: aPriority
	self priority: aPriority.
	^self resume
</details>

#### Process>>#name: aString

<details>
	<summary>See more</summary>
	
	name: aString

	name _ aString
</details>

#### Process>>#completeTo: aContext

Resume self until aContext is on top


<details>
	<summary>See more</summary>
	
	completeTo: aContext 
	"Resume self until aContext is on top"

	self suspendedContext == aContext ifTrue: [^ aContext].
	^ self complete: (self calleeOf: aContext)
</details>

#### Process>>#suspendedContext

Answer the context the receiver has suspended. I am Ready to Run or Suspended. Answer the context I was running when last preempted. Otherwise, (Running or Terminated) answer nil.


<details>
	<summary>See more</summary>
	
	suspendedContext
	"Answer the context the receiver has suspended.
	I am Ready to Run or Suspended. Answer the context I was running when last preempted.

	Otherwise, (Running or Terminated) answer nil."

	^self isTerminated ifFalse: [ suspendedContext ]
</details>

#### Process>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	super printOn: aStream.
	aStream
		space;
		nextPutAll: (self browserPrintStringFull: false)
</details>

#### Process>>#canSearchForSignalerContextOf: anException

<details>
	<summary>See more</summary>
	
	canSearchForSignalerContextOf: anException

	^ (anException class includesBehavior: Exception)
		and: [anException canSearchForSignalerContext]
</details>

#### Process>>#priority

Answer the priority of the receiver.


<details>
	<summary>See more</summary>
	
	priority
	"Answer the priority of the receiver."

	^priority
</details>

#### Process>>#pvtSignal: anException list: aList

Private. This method is used to signal an exception from another process...the receiver must be the active process. If the receiver was previously waiting on a Semaphore, then return the process to the waiting state after signaling the exception and if the Semaphore has not been signaled in the interim


<details>
	<summary>See more</summary>
	
	pvtSignal: anException list: aList
	"Private. This method is used to signal an exception from another
	process...the receiver must be the active process.  If the receiver 
	was previously waiting on a Semaphore, then return the process
	to the waiting state after signaling the exception and if the Semaphore
	has not been signaled in the interim"

	"Since this method is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because I believe that could
	have the potential to push an unwanted object on the caller's stack)."

	| blocker |
	self isActiveProcess ifFalse: [^self].
	anException signal.
	blocker := Semaphore new.
	[self suspend.
	suspendedContext := suspendedContext swapSender: nil.
	aList class == Semaphore 
		ifTrue:
			[aList isSignaled
				ifTrue: 
					[aList wait.  "Consume the signal that would have restarted the receiver"
					self resume]
				ifFalse:
					["Add us back to the Semaphore's list (and remain blocked)"
					myList := aList.
					aList add: self]]
		ifFalse: [self resume]] fork.
	blocker wait.



</details>

#### Process>>#isReady

True if not running right now, but might be scheduled anytime. Useful to play with: | s | s _ Semaphore new. s inspect. ([ s wait . 10000 timesRepeat: [ 10000 factorial. (Delay forSeconds: 1) wait. 7 print ]] newProcess name: 'test5') inspect { self isRunning . self isReady . self isSuspended . self isBlocked . self isTerminated } print


<details>
	<summary>See more</summary>
	
	isReady
	"True if not running right now, but might be scheduled anytime.

	Useful to play with:

	| s |	
	s _ Semaphore new.
	s inspect.
	([ s wait . 10000 timesRepeat: [ 10000 factorial. (Delay forSeconds: 1) wait. 7 print ]] newProcess name: 'test5') inspect
	
	{ self isRunning . self isReady . self isSuspended . self isBlocked . self isTerminated } print
	"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == LinkedList
</details>

#### Process>>#terminate

Stop the process that the receiver represents forever. Unwind to execute pending ensure:/ifCurtailed: blocks before terminating.


<details>
	<summary>See more</summary>
	
	terminate 
	"Stop the process that the receiver represents forever.  Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."

	| ctxt unwindBlock oldList |
	self isRunning ifTrue: [
		ctxt := thisContext.
		[	ctxt := ctxt findNextUnwindContextUpTo: nil.
			ctxt isNil
		] whileFalse: [
			(ctxt tempAt: 2) ifNil:[
				ctxt tempAt: 2 put: nil.
				unwindBlock := ctxt tempAt: 1.
				thisContext terminateTo: ctxt.
				unwindBlock value].
		].
		thisContext terminateTo: nil.
		self suspend.
	] ifFalse: [
		"Always suspend the process first so it doesn't accidentally get woken up"
		oldList := self suspend.
		suspendedContext ifNotNil:[
			"Figure out if we are terminating the process while waiting in Semaphore>>critical:
			In this case, pop the suspendedContext so that we leave the ensure: block inside
			Semaphore>>critical: without signaling the semaphore."
			(oldList class == Semaphore and:[
				suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
					suspendedContext := suspendedContext home.
			].

			"If we are terminating a process halfways through an unwind, try
			to complete that unwind block first."
			(suspendedContext findNextUnwindContextUpTo: nil) ifNotNil:[:outer|
				(suspendedContext findContextSuchThat:[:c| c closure == (outer tempAt: 1)]) ifNotNil: [ :inner|
					"This is an unwind block currently under evaluation"
					suspendedContext runUntilErrorOrReturnFrom: inner.
				].
			].

			ctxt := self popTo: suspendedContext bottomContext.
			ctxt == suspendedContext bottomContext ifFalse: [
				self debug: ctxt title: 'Unwind error during termination']].
	].

</details>

#### Process>>#signal: anException

Signal an exception in the receiver process...if the receiver is currently suspended, the exception will get signaled when the receiver is resumed. If the receiver is blocked on a Semaphore, it will be immediately re-awakened and the exception will be signaled; if the exception is resumed, then the receiver will return to a blocked state unless the blocking Semaphore has excess signals


<details>
	<summary>See more</summary>
	
	signal: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"

	"If we are the active process, go ahead and signal the exception"
	self isRunning ifTrue: [^anException signal].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := MethodContext
		sender: suspendedContext
		receiver: self
		method: (self class methodDict at: #pvtSignal:list:)
		arguments: (Array with: anException with: myList).

	"If we are on a list to run, then suspend and restart the receiver 
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	myList ifNotNil: [self suspend; resume].
</details>

#### Process>>#restartTopWith: method

Rollback top context and replace with new method. Assumes self is suspended


<details>
	<summary>See more</summary>
	
	restartTopWith: method
	"Rollback top context and replace with new method.  Assumes self is suspended"

	method isQuick 
		ifTrue: [ self popTo: suspendedContext sender ]
		ifFalse: [ suspendedContext privRefreshWith: method ].

</details>

#### Process>>#popTo: aContext

Pop self down to aContext by remote returning from aContext's callee. Unwind blocks will be executed on the way. This is done by pushing a new context on top which executes 'aContext callee return' then resuming self until aContext is reached. This way any errors raised in an unwind block will get handled by senders in self and not by senders in the activeProcess. If an unwind block raises an error that is not handled then the popping stops at the error and the signalling context is returned, othewise aContext is returned.


<details>
	<summary>See more</summary>
	
	popTo: aContext 
	"Pop self down to aContext by remote returning from aContext's callee.  Unwind blocks will be executed on the way.
	This is done by pushing a new context on top which executes 'aContext callee return' then resuming self until aContext is reached.  This way any errors raised in an unwind block will get handled by senders in self and not by senders in the activeProcess.
	If an unwind block raises an error that is not handled then the popping stops at the error and the signalling context is returned, othewise aContext is returned."

	| callee |
	self == Processor activeProcess
		ifTrue: [^ self error: 'The active process cannot pop contexts'].
	callee _ (self calleeOf: aContext) ifNil: [^ aContext].  "aContext is on top"
	^ self return: callee value: callee receiver
</details>

#### Process>>#isSuspended

Answer true if I was never scheduled yet (new process, never been sent #resume) or paused (was sent #suspend)


<details>
	<summary>See more</summary>
	
	isSuspended
	"Answer true if I was never scheduled yet (new process, never been sent #resume) or paused (was sent #suspend)"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList isNil
</details>

#### Process>>#install: aContext

Replace the suspendedContext with aContext.


<details>
	<summary>See more</summary>
	
	install: aContext 
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext _ aContext
</details>

#### Process>>#browserPrintString

<details>
	<summary>See more</summary>
	
	browserPrintString
	^self browserPrintStringFull: true
</details>

#### Process>>#calleeOf: aContext

Return the context whose sender is aContext. Return nil if aContext is on top. Raise error if aContext is not in process chain.


<details>
	<summary>See more</summary>
	
	calleeOf: aContext
	"Return the context whose sender is aContext.  Return nil if aContext is on top.  Raise error if aContext is not in process chain."

	suspendedContext == aContext ifTrue: [^ nil].
	^ (suspendedContext findContextSuchThat: [:c | c sender == aContext])
		ifNil: [self error: 'aContext not in process chain']
</details>

#### Process>>#restartTop

Rollback top context and replace with new method. Assumes self is suspended


<details>
	<summary>See more</summary>
	
	restartTop
	"Rollback top context and replace with new method.  Assumes self is suspended"

	suspendedContext privRefresh
</details>

#### Process>>#name

Answer a name for the receiver. This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems. By default, we let the object just print itself out..


<details>
	<summary>See more</summary>
	
	name

	^name ifNil: [ 'unnamed' ]
</details>

#### Process>>#debug: context title: title

Open debugger on self with context shown on top


<details>
	<summary>See more</summary>
	
	debug: context title: title
	"Open debugger on self with context shown on top"

	self debug: context title: title full: false
</details>

#### Process>>#run

Suspend current process and execute self instead


<details>
	<summary>See more</summary>
	
	run
	"Suspend current process and execute self instead"

	| proc |
	proc _ Processor activeProcess.
	[	proc suspend.
		self resume.
	] forkAt: Processor highestPriority
</details>

#### Process>>#isTerminated

Answer true if terminated, i.e. can never be resumed again, because have nothing to run.


<details>
	<summary>See more</summary>
	
	isTerminated
	"Answer true if terminated, i.e. can never be resumed again, because have nothing to run."
	self isRunning ifTrue: [^ false].
	^suspendedContext isNil
		or: [ suspendedContext pc isNil
			or: ["If the suspendedContext is the bottomContext it is the block in Process>>newProcess.
		   		If so, and the pc is greater than the startpc, the bock has alrteady sent and returned
		   		from value and there is nothing more to do."
				suspendedContext isBottomContext
					and: [
						suspendedContext pc > suspendedContext startpc]]]
</details>

#### Process>>#debug: context title: title full: bool

Open debugger on self with context shown on top


<details>
	<summary>See more</summary>
	
	debug: context title: title full: bool
	"Open debugger on self with context shown on top"

	| topCtxt |
	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	topCtxt _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: bool.

</details>

#### Process>>#activateReturn: aContext value: value

Activate 'aContext return: value', so execution will return to aContext's sender


<details>
	<summary>See more</summary>
	
	activateReturn: aContext value: value
	"Activate 'aContext return: value', so execution will return to aContext's sender"

	^ suspendedContext _ suspendedContext activateReturn: aContext value: value
</details>

#### Process>>#browserPrintStringFull: aBoolean

<details>
	<summary>See more</summary>
	
	browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		aBoolean ifTrue: [
			stream
				nextPutAll: self statusString;
				space ].
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		self animatedUI ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext asString ]]
</details>

#### Process>>#resume

Allow the process that the receiver represents to continue. Put the receiver in line to become the activeProcess. Check for a nil suspendedContext, which indicates a previously terminated Process that would cause a vm crash if the resume attempt were permitted. This resumes the receiver if - It was never scheduled - it is suspended (was sent #suspend) - It is ready to run (was preempted). In this case there is no effect. - it is waiting on a semaphore. In this case, the semaphore is ignored.


<details>
	<summary>See more</summary>
	
	resume
	"Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Check for a nil 
	suspendedContext, which indicates a previously terminated Process that 
	would cause a vm crash if the resume attempt were permitted.
	
	This resumes the receiver if
		- It was never scheduled
		- it is suspended (was sent #suspend)
		- It is ready to run (was preempted). In this case there is no effect.
		- it is waiting on a semaphore. In this case, the semaphore is ignored.
	"

	suspendedContext ifNil: [^ self primitiveFailed].
	^ self primitiveResume
</details>

#### Process>>#priority: anInteger

Set the receiver's priority to anInteger.


<details>
	<summary>See more</summary>
	
	priority: anInteger 
	"Set the receiver's priority to anInteger."
	(anInteger >= Processor lowestPriority and:[anInteger <= Processor highestPriority])
		ifTrue: [priority _ anInteger]
		ifFalse: [self error: 'Invalid priority: ', anInteger printString]
</details>

#### Process>>#isActiveProcess

Are we the cunrrently running process? Just for compatibility.


<details>
	<summary>See more</summary>
	
	isActiveProcess
	"Are we the cunrrently running process?
	Just for compatibility."
	^self isRunning
</details>

#### Process>>#suspend

Primitive. Stop the process that the receiver represents in such a way that it can be restarted at a later time (by sending the receiver the message resume). If the receiver represents the activeProcess, suspend it. Otherwise remove the receiver from the list of waiting processes. The return value of this method is the list the receiver was previously on (if any).


<details>
	<summary>See more</summary>
	
	suspend
	"Primitive. Stop the process that the receiver represents in such a way 
	that it can be restarted at a later time (by sending the receiver the 
	message resume). If the receiver represents the activeProcess, suspend it. 
	Otherwise remove the receiver from the list of waiting processes.
	The return value of this method is the list the receiver was previously on (if any)."
	| oldList |
	<primitive: 88>
	"This is fallback code for VMs which only support the old primitiveSuspend which 
	would not accept processes that are waiting to be run."
	myList ifNil:[^nil]. "this allows us to use suspend multiple times"
	oldList := myList.
	myList := nil.
	oldList remove: self ifAbsent: nil.
	^oldList
</details>

#### Process>>#completeStepUpTo: aContext

<details>
	<summary>See more</summary>
	
	completeStepUpTo: aContext
	
	[aContext == suspendedContext] 
		whileFalse: [self completeStep: suspendedContext].
</details>

#### Process>>#basicSuspendedContext

Answer the context the receiver has suspended. The debugger needs access to this ivar even if the process appears to be terminated. To be better understood some day. This method was added when #suspendedContext was modified to answer nil for terminated processes.


<details>
	<summary>See more</summary>
	
	basicSuspendedContext
	"Answer the context the receiver has suspended.
	The debugger needs access to this ivar even if the process appears to be terminated.
	To be better understood some day.
	This method was added when #suspendedContext was modified to answer nil for terminated processes."
	^suspendedContext
</details>

#### Process>>#suspendPrimitivelyOrFail

Test support. Execute primitive 88, or fail.


<details>
	<summary>See more</summary>
	
	suspendPrimitivelyOrFail
	"Test support. Execute primitive 88, or fail."

	<primitive: 88>
	^self primitiveFailed
</details>

#### Process>>#isTheLastPreempted

Answer wether I am the last process that was preempted


<details>
	<summary>See more</summary>
	
	isTheLastPreempted
	"Answer wether I am the last process that was preempted"
	^self == Processor preemptedProcess
</details>

#### Process>>#longPrintOn: stream

Append to the argument, aStream, the names and values of all of the receiver's instance variables.


<details>
	<summary>See more</summary>
	
	longPrintOn: stream

	| ctxt |
	super printOn: stream.
	stream newLine.
	ctxt _ self suspendedContext.
	[ctxt == nil] whileFalse: [
		stream space.
		ctxt printOn: stream.
		stream newLine.
		ctxt _ ctxt sender.
	]
</details>

#### Process>>#completeStep: aContext

Resume self until aContext is on top, or if already on top, complete next step


<details>
	<summary>See more</summary>
	
	completeStep: aContext 
	"Resume self until aContext is on top, or if already on top, complete next step"

	| callee |
	self suspendedContext == aContext ifFalse: [
		^ self complete: (self calleeOf: aContext)].
	callee _ self step.
	callee == aContext ifTrue: [^ callee].
	aContext isDead ifTrue: [^ self suspendedContext].  "returned"
	^ self complete: callee  "finish send"
</details>

#### Process>>#suspendedContext: aContext

<details>
	<summary>See more</summary>
	
	suspendedContext: aContext

	suspendedContext _ aContext
</details>

#### Process>>#isRunning

Are we the cunrrently running process?


<details>
	<summary>See more</summary>
	
	isRunning
	"Are we the cunrrently running process?"
	^ self == Processor activeProcess
</details>

#### Process>>#animatedUI: anUIRoot

Let us know that we are running a certain UI. In Morphic, anUIRoot should be the World being run. We use the event system to avoid the need to add an ivar to us.


<details>
	<summary>See more</summary>
	
	animatedUI: anUIRoot
	"Let us know that we are running a certain UI.
	In Morphic, anUIRoot should be the World being run.
	We use the event system to avoid the need to add an ivar to us."

	self removeActionsForEvent: #animatedUI.
	anUIRoot ifNotNil: [
		self when: #animatedUI send: #yourself to: anUIRoot ]
</details>

#### Process>>#debugFullWithTitle: title

Open debugger on self


<details>
	<summary>See more</summary>
	
	debugFullWithTitle: title
	"Open debugger on self"

	| context |
	context _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	self debug: context title: title full: true
</details>

#### Process>>#suspendingList

Answer the list on which the receiver has been suspended.


<details>
	<summary>See more</summary>
	
	suspendingList
	"Answer the list on which the receiver has been suspended."

	"myList can be:
		- A LinkedList: The Processor queue we are in. This, if we are Ready to Run.
		- A Semaphore. This, if we are Blocked waiting on it.
		- nil otherwise, i.e. if we are Running, Suspended, Terminated."
	^myList
</details>

#### Process>>#stepToHome: aContext

Resume self until the home of top context is aContext. Top context may be a block context. Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext if so. Note that this will cause weird effects if using through to step through UnhandledError code, but as the doctor ordered, don't do that; use over or into instead.


<details>
	<summary>See more</summary>
	
	stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home |
	
	home := aContext home.
	[suspendedContext := suspendedContext step.
	home == suspendedContext home or: [home isDead]] whileFalse:
		[self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
			anError signalerContext ifNotNil: [:unhandledErrorSignalerContext|
				self completeStepUpTo: unhandledErrorSignalerContext.

				"Give a debugger a chance to update its title to reflect the new exception"
				 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
				^unhandledErrorSignalerContext]]].
		
	^suspendedContext
</details>

#### Process>>#objectForDataStream: refStrm

I am not allowed to be written on an object file.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	"I am not allowed to be written on an object file."

	refStrm replace: self with: nil.
	^ nil
</details>

#### Process>>#copyStack

<details>
	<summary>See more</summary>
	
	copyStack

	^ self copy install: suspendedContext copyStack
</details>

#### Process>>#isSuspendedContextSignalUnhandledError

<details>
	<summary>See more</summary>
	
	isSuspendedContextSignalUnhandledError

	^ suspendedContext selector == #signalForException:
		and: [suspendedContext receiver isBehavior 
		and: [suspendedContext receiver includesBehavior: UnhandledError]]
</details>

#### Process>>#primitiveResume

Primitive. Allow the process that the receiver represents to continue. Put the receiver in line to become the activeProcess. Fail if the receiver is already waiting in a queue (in a Semaphore or ProcessScheduler). Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	primitiveResume
	"Primitive. Allow the process that the receiver represents to continue. Put 
	the receiver in line to become the activeProcess. Fail if the receiver is 
	already waiting in a queue (in a Semaphore or ProcessScheduler). 
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 87>
	self primitiveFailed
</details>

#### Process>>#return: aContext value: value

Pop thread down to aContext's sender. Execute any unwind blocks on the way. See #popTo: comment and #runUntilErrorOrReturnFrom: for more details.


<details>
	<summary>See more</summary>
	
	return: aContext value: value
	"Pop thread down to aContext's sender.  Execute any unwind blocks on the way.  See #popTo: comment and #runUntilErrorOrReturnFrom: for more details."

	suspendedContext == aContext ifTrue: [
		^ suspendedContext _ aContext return: value from: aContext].
	self activateReturn: aContext value: value.
	^ self complete: aContext.

</details>

#### Process>>#complete: aContext

Run self until aContext is popped or an unhandled error is raised. Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger).


<details>
	<summary>See more</summary>
	
	complete: aContext 
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger)."
	
	| ctxt pair error |
	ctxt _ suspendedContext.
	suspendedContext _ nil.  "disable this process while running its stack in active process below"
	pair _ ctxt runUntilErrorOrReturnFrom: aContext.
	suspendedContext _ pair first.
	error _ pair second.
	error ifNotNil: [^ error signalerContext].
	^ suspendedContext
</details>

#### Process>>#signalException: anException

Signal an exception in the receiver process...if the receiver is currently suspended, the exception will get signaled when the receiver is resumed. If the receiver is blocked on a Semaphore, it will be immediately re-awakened and the exception will be signaled; if the exception is resumed, then the receiver will return to a blocked state unless the blocking Semaphore has excess signals


<details>
	<summary>See more</summary>
	
	signalException: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"
	| oldList |
	"If we are the active process, go ahead and signal the exception"
	self isRunning ifTrue: [^anException signal].

	"Suspend myself first to ensure that I won't run away in the
	midst of the following modifications."
	myList ifNotNil:[oldList := self suspend].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := MethodContext
		sender: suspendedContext
		receiver: self
		method: (self class lookupSelector: #pvtSignal:list:)
		arguments: (Array with: anException with: oldList).

	"If we are on a list to run, then suspend and restart the receiver 
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	oldList ifNotNil: [self resume].

</details>

#### Process>>#stepToSendOrReturn

<details>
	<summary>See more</summary>
	
	stepToSendOrReturn

	^ suspendedContext _ suspendedContext stepToSendOrReturn
</details>

#### Process>>#statusString

<details>
	<summary>See more</summary>
	
	statusString
	self isRunning ifTrue: [ ^ 'running' ].
	self isReady ifTrue: [ ^ 'ready' ].
	self isSuspended ifTrue: [ ^ 'suspended' ].
	self isBlocked ifTrue: [ ^ 'blocked' ].
	self isTerminated ifTrue: [ ^'terminated' ].
	^'unknown (bug?)'
</details>

## ProcessorScheduler

My single instance, named Processor, coordinates the use of the physical processor by all Processes requiring service.

### Methods
#### ProcessorScheduler>>#userBackgroundPriority

Answer the priority at which user background processes should run.


<details>
	<summary>See more</summary>
	
	userBackgroundPriority
	"Answer the priority at which user background processes should run."

	^self class userBackgroundPriority
</details>

#### ProcessorScheduler>>#timingPriority

Answer the priority at which the system processes keeping track of real time should run.


<details>
	<summary>See more</summary>
	
	timingPriority
	"Answer the priority at which the system processes keeping track of real 
	time should run."

	^self class timingPriority
</details>

#### ProcessorScheduler>>#userSchedulingPriority

Answer the priority at which the window scheduler should run.


<details>
	<summary>See more</summary>
	
	userSchedulingPriority
	"Answer the priority at which the window scheduler should run."

	^self class userSchedulingPriority
</details>

#### ProcessorScheduler>>#processesWithTopContextDo: aBlock runningProcessSearchStart: aContextOrNil

Iterate over processes that can run. Include top context in block arguments.


<details>
	<summary>See more</summary>
	
	processesWithTopContextDo: aBlock runningProcessSearchStart: aContextOrNil
	"Iterate over processes that can run. Include top context in block arguments."
	"
	Processor processesWithTopContextDo: [ :p :c | p print. ('------->', c printString) print ].
	"
	self processesDo: [ :process |
		aBlock
			value: process
			value: (process isRunning ifTrue: [ aContextOrNil ifNil: [thisContext] ] ifFalse: [ process suspendedContext ]) ]
</details>

#### ProcessorScheduler>>#activeProcess

Answer the currently running Process.


<details>
	<summary>See more</summary>
	
	activeProcess
	"Answer the currently running Process."

	^activeProcess
</details>

#### ProcessorScheduler>>#suspendFirstAt: aPriority ifNone: noneBlock

Suspend the first Process that is waiting to run with priority aPriority. If no Process is waiting, evaluate the argument, noneBlock.


<details>
	<summary>See more</summary>
	
	suspendFirstAt: aPriority ifNone: noneBlock 
	"Suspend the first Process that is waiting to run with priority aPriority. If 
	no Process is waiting, evaluate the argument, noneBlock."

	| aList |
	aList _ quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]
</details>

#### ProcessorScheduler>>#lowIOPriority

Answer the priority at which most input/output processes should run. Examples are the process handling input from the user (keyboard, pointing device, etc.) and the process distributing input from a network.


<details>
	<summary>See more</summary>
	
	lowIOPriority
	"Answer the priority at which most input/output processes should run. 
	Examples are the process handling input from the user (keyboard, 
	pointing device, etc.) and the process distributing input from a network."

	^self class lowIOPriority
</details>

#### ProcessorScheduler>>#highIOPriority

Answer the priority at which the most time critical input/output processes should run. An example is the process handling input from a network.


<details>
	<summary>See more</summary>
	
	highIOPriority
	"Answer the priority at which the most time critical input/output 
	processes should run. An example is the process handling input from a 
	network."

	^self class highIOPriority
</details>

#### ProcessorScheduler>>#backgroundProcess

Answer the background process


<details>
	<summary>See more</summary>
	
	backgroundProcess
	"Answer the background process"
	^ BackgroundProcess
</details>

#### ProcessorScheduler>>#preemptedProcess

Return the process that the currently active process just preempted.


<details>
	<summary>See more</summary>
	
	preemptedProcess
	"Return the process that the currently active process just preempted."
	
	self activeProcess priority to: 1 by: -1 do: [ :priority |
		(quiescentProcessLists at: priority) ifNotEmpty: [ :list |
			^ Smalltalk processPreemptionYields
				ifTrue: [ list last ]
				ifFalse: [ list first ]]].
	^ nil
	"
	Processor preemptedProcess
	"
</details>

#### ProcessorScheduler>>#nextReadyProcess

<details>
	<summary>See more</summary>
	
	nextReadyProcess
	quiescentProcessLists reverseDo: [ :list |
		list isEmpty ifFalse: [ | proc |
			proc _ list first.
			proc isReady ifTrue: [ ^proc ]]].
	^nil
</details>

#### ProcessorScheduler>>#suspendFirstAt: aPriority

Suspend the first Process that is waiting to run with priority aPriority.


<details>
	<summary>See more</summary>
	
	suspendFirstAt: aPriority 
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']
</details>

#### ProcessorScheduler>>#lowestPriority

Return the lowest priority that is allowed with the scheduler


<details>
	<summary>See more</summary>
	
	lowestPriority
	"Return the lowest priority that is allowed with the scheduler"
	^self class lowestPriority
</details>

#### ProcessorScheduler>>#waitingProcessesAt: aPriority

Return the list of processes at the given priority level.


<details>
	<summary>See more</summary>
	
	waitingProcessesAt: aPriority
	"Return the list of processes at the given priority level."
	^quiescentProcessLists at: aPriority
</details>

#### ProcessorScheduler>>#objectForDataStream: refStrm

I am about to be written on an object file. Write a path to me in the other system instead.


<details>
	<summary>See more</summary>
	
	objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	dp _ DiskProxy global: #Processor selector: #yourself args: #().
	refStrm replace: self with: dp.
	^ dp

</details>

#### ProcessorScheduler>>#processesDo: aBlock

Iterate over processes that can run


<details>
	<summary>See more</summary>
	
	processesDo: aBlock
	"Iterate over processes that can run"
	"
	Processor processesDo: [ :p | p print ].
	"
	Process allSubInstancesDo: [ :p |
		p isTerminated ifFalse: [
			aBlock value: p ]]
</details>

#### ProcessorScheduler>>#activePriority

Answer the priority level of the currently running Process.


<details>
	<summary>See more</summary>
	
	activePriority
	"Answer the priority level of the currently running Process."

	^activeProcess priority
</details>

#### ProcessorScheduler>>#processesDo: aBlock withStackFramestDo: twoArgBlock runningProcessSearchStart: aContextOrNil

Iterate over processes that can run. For each process, iterate over stack frames (i.e. contexts).


<details>
	<summary>See more</summary>
	
	processesDo: aBlock withStackFramestDo: twoArgBlock runningProcessSearchStart: aContextOrNil
	"Iterate over processes that can run.
	For each process, iterate over stack frames (i.e. contexts)."
	"
	Processor
		processesDo: [ :p |
			'--------------' print.
			p print. 
			'--------------' print ]
		withStackFramestDo: [ :p :c |
			('        ', c printString) print ].
	"
	self
		processesWithTopContextDo: [ :process :topContext | | context |
			aBlock value: process.
			context _ topContext.
			[ context notNil ] whileTrue: [
				twoArgBlock value: process value: context.
				context _ context sender ]]
		runningProcessSearchStart: aContextOrNil
</details>

#### ProcessorScheduler>>#userInterruptPriority

Answer the priority at which user processes desiring immediate service should run. Processes run at this level will preempt the window scheduler and should, therefore, not consume the processor forever.


<details>
	<summary>See more</summary>
	
	userInterruptPriority
	"Answer the priority at which user processes desiring immediate service 
	should run. Processes run at this level will preempt the window 
	scheduler and should, therefore, not consume the processor forever."

	^self class userInterruptPriority
</details>

#### ProcessorScheduler>>#remove: aProcess ifAbsent: aBlock

Remove aProcess from the list on which it is waiting for the processor and answer aProcess. If it is not waiting, evaluate aBlock.


<details>
	<summary>See more</summary>
	
	remove: aProcess ifAbsent: aBlock 
	"Remove aProcess from the list on which it is waiting for the processor 
	and answer aProcess. If it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess
</details>

#### ProcessorScheduler>>#highestPriority: newHighestPriority

Change the number of priority levels currently available for use.


<details>
	<summary>See more</summary>
	
	highestPriority: newHighestPriority
	"Change the number of priority levels currently available for use."

	| continue newProcessLists |
	(quiescentProcessLists size > newHighestPriority
		and: [self anyProcessesAbove: newHighestPriority])
			ifTrue: [self error: 'There are processes with priority higher than '
													,newHighestPriority printString].
	newProcessLists _ Array new: newHighestPriority.
	1 to: ((quiescentProcessLists size) min: (newProcessLists size)) do: 
		[:priority | newProcessLists at: priority put: (quiescentProcessLists at: priority)].
	quiescentProcessLists size to: newProcessLists size do: 
		[:priority | newProcessLists at: priority put: LinkedList new].
	quiescentProcessLists _ newProcessLists
</details>

#### ProcessorScheduler>>#anyProcessesAbove: highestPriority

Do any instances of Process exist with higher priorities?


<details>
	<summary>See more</summary>
	
	anyProcessesAbove: highestPriority 
	"Do any instances of Process exist with higher priorities?"

	^(Process allSubInstances select: [:aProcess | 
		aProcess priority > highestPriority]) isEmpty
		"If anyone ever makes a subclass of Process, be sure to use allSubInstances."
</details>

#### ProcessorScheduler>>#yield

Give other Processes at the current priority a chance to run.


<details>
	<summary>See more</summary>
	
	yield
	"Give other Processes at the current priority a chance to run."

	| semaphore |

	<primitive: 167>
	semaphore _ Semaphore new.
	[semaphore signal] fork.
	semaphore wait
</details>

#### ProcessorScheduler>>#tallyCPUUsageFor: seconds every: msec

Start a high-priority process that will tally the next ready process for the given number of seconds. Answer a Block that will return the tally (a Bag) after the task is complete


<details>
	<summary>See more</summary>
	
	tallyCPUUsageFor: seconds every: msec
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	| tally sem delay endDelay |
	tally _ IdentityBag new: 200.
	delay _ Delay forMilliseconds: msec truncated.
	endDelay _ Delay forSeconds: seconds.
	endDelay schedule.
	sem _ Semaphore new.
	[
		[ endDelay isExpired ] whileFalse: [
			delay wait.
			tally add: Processor nextReadyProcess
		].
		sem signal.
	] forkAt: self highestPriority named: 'Processor CPU Usage Tallier'.

	^[ sem wait. tally ]
</details>

#### ProcessorScheduler>>#terminateActive

Terminate the process that is currently running.


<details>
	<summary>See more</summary>
	
	terminateActive
	"Terminate the process that is currently running."

	activeProcess terminate
</details>

#### ProcessorScheduler>>#systemBackgroundPriority

Answer the priority at which system background processes should run. Examples are an incremental garbage collector or status checker.


<details>
	<summary>See more</summary>
	
	systemBackgroundPriority
	"Answer the priority at which system background processes should run. 
	Examples are an incremental garbage collector or status checker."

	^self class systemBackgroundPriority
</details>

#### ProcessorScheduler>>#highestPriority

Answer the number of priority levels currently available for use.


<details>
	<summary>See more</summary>
	
	highestPriority
	"Answer the number of priority levels currently available for use."

	^quiescentProcessLists size
</details>

#### ProcessorScheduler>>#anyReceiverInStackIn: anArray

Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument


<details>
	<summary>See more</summary>
	
	anyReceiverInStackIn: anArray
	"Iterate over all methods currently in execution. Answer true if in any of them 'self' an element of argument"
	"
	Processor anyReceiverInStackIn: { self runningWorld }
	Processor anyReceiverInStackIn: { Object new }
	"
	self
		processesDo: [ :p | ]
		withStackFramestDo: [ :p :c |
			(anArray statePointsTo: c receiver)
				ifTrue: [ ^ true ]]
		runningProcessSearchStart: nil.
	^ false
</details>

## Semaphore

I provide synchronized communication of a single bit of information (a "signal") between Processes. A signal is sent by sending the message signal and received by sending the message wait. If no signal has been sent when a wait message is sent, the sending Process will be suspended until a signal is sent. Beware that if a process calls 'aSemaphore critical: []' while already in a critical section for that semaphore, it will enter a deadlock. In some cases, a Mutex can be used instead. Refer to the Mutex class comment. More detail on the implementation as provided by Eliot Miranda: A semaphore is a queue (implemented as a linked list) and an excess signals count, which is a non-negative integer. On instance creation a new semaphore is empty and has a zero excess signals count. A semaphore created for mutual exclusion is empty and has an excess signals count of one. When a process waits on a semaphore, if the semaphore's excess signals count is non-zero, then the excess signal count is decremented, and the process proceeds. But if the semaphore has a zero excess signals count then the process is unscheduled and added to the end of the semaphore, after any other processes that are queued on the semaphore. When a semaphore is signaled, if it is not empty, the first process is removed from it and added to the runnable processes in the scheduler. If the semaphore is empty its excess signals count is incremented.

### Methods
#### Semaphore>>#waitTimeoutSeconds: anInteger

Wait on this semaphore for up to the given number of seconds, then timeout. Return true if the deadline expired, false otherwise.


<details>
	<summary>See more</summary>
	
	waitTimeoutSeconds: anInteger
	"Wait on this semaphore for up to the given number of seconds, then timeout.
	Return true if the deadline expired, false otherwise."
	^self waitTimeoutMSecs: anInteger * 1000.

</details>

#### Semaphore>>#critical: mutuallyExcludedBlock ifError: errorBlock

Evaluate mutuallyExcludedBlock only if the receiver is not currently in the process of running the critical: message. If the receiver is, evaluate mutuallyExcludedBlock after the other critical: message is finished.


<details>
	<summary>See more</summary>
	
	critical: mutuallyExcludedBlock ifError: errorBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	| blockValue hasError errMsg errRcvr |
	hasError := false.
	blockValue := self critical:[
		mutuallyExcludedBlock ifError: [ :msg :rcvr |
			hasError := true.
			errMsg := msg.
			errRcvr := rcvr
		].
	].
	hasError ifTrue:[ ^errorBlock value: errMsg value: errRcvr].
	^blockValue
</details>

#### Semaphore>>#wait

Primitive. The active Process must receive a signal through the receiver before proceeding. If no signal has been sent, the active Process will be suspended until one is sent. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	wait
	"Primitive. The active Process must receive a signal through the receiver 
	before proceeding. If no signal has been sent, the active Process will be 
	suspended until one is sent. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals _ excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"

</details>

#### Semaphore>>#critical: mutuallyExcludedBlock

Evaluate mutuallyExcludedBlock only if the receiver is not currently in the process of running the critical: message. If the receiver is, evaluate mutuallyExcludedBlock after the other critical: message is finished.


<details>
	<summary>See more</summary>
	
	critical: mutuallyExcludedBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in
	the process of running the critical: message. If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."
	
	| caught |
	"We need to catch eventual interruptions very carefully. 
	The naive approach of just doing, e.g.,:
		self wait.
		aBlock ensure:[self signal].
	will fail if the active process gets terminated while in the wait.
	However, the equally naive:
		[self wait.
		aBlock value] ensure:[self signal].
	will fail too, since the active process may get interrupted while
	entering the ensured block and leave the semaphore signaled twice.
	To avoid both problems we make use of the fact that interrupts only
	occur on sends (or backward jumps) and use an assignment (bytecode)
	right before we go into the wait primitive (which is not a real send and
	therefore not interruptable either)."

	caught := false.
	^[
		caught := true.
		self wait.
		mutuallyExcludedBlock value
	] ensure: [ caught ifTrue: [self signal] ]

</details>

#### Semaphore>>#= anObject

Answer true if the receiver is equivalent to the otherCollection. First test for identity, then rule out different species and sizes of collections. As a last resort, examine each element of the receiver and the otherCollection.


<details>
	<summary>See more</summary>
	
	= anObject
	^ self == anObject
</details>

#### Semaphore>>#isSignaled

Return true if this semaphore is currently signaled


<details>
	<summary>See more</summary>
	
	isSignaled
	"Return true if this semaphore is currently signaled"
	^excessSignals > 0
</details>

#### Semaphore>>#hash

Subclasses might use other methods. However #hashQuick is suggested for very large collections.


<details>
	<summary>See more</summary>
	
	hash
	^ self identityHash
</details>

#### Semaphore>>#critical: mutuallyExcludedBlock ifCurtailed: terminationBlock

Evaluate mutuallyExcludedBlock only if the receiver is not currently in the process of running the critical: message. If the receiver is, evaluate mutuallyExcludedBlock after the other critical: message is finished.


<details>
	<summary>See more</summary>
	
	critical: mutuallyExcludedBlock ifCurtailed: terminationBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, evaluate 
	mutuallyExcludedBlock after the other critical: message is finished."
	^self critical: [ mutuallyExcludedBlock ifCurtailed: terminationBlock ]

</details>

#### Semaphore>>#terminateProcess

Terminate the process waiting on this semaphore, if any.


<details>
	<summary>See more</summary>
	
	terminateProcess
	"Terminate the process waiting on this semaphore, if any."

	self isEmpty ifFalse: [ self removeFirst terminate ].
</details>

#### Semaphore>>#printOn: aStream

Append a sequence of characters that identify the receiver to aStream.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' with ';
		nextPutAll: excessSignals asString;
		space;
		nextPutAll: ('signal' pluralize: excessSignals ~= 1)
</details>

#### Semaphore>>#species

So we are never equal an Array


<details>
	<summary>See more</summary>
	
	species
	"So we are never equal an Array"

	^ self class
</details>

#### Semaphore>>#initSignals

Consume any excess signals the receiver may have accumulated.


<details>
	<summary>See more</summary>
	
	initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals _ 0.
</details>

#### Semaphore>>#critical: mutuallyExcludedBlock ifLocked: alternativeBlock

Evaluate mutuallyExcludedBlock only if the receiver is not currently in the process of running the critical: message. If the receiver is, then evaluate alternativeBlock and return.


<details>
	<summary>See more</summary>
	
	critical: mutuallyExcludedBlock ifLocked: alternativeBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in 
	the process of running the critical: message. If the receiver is, then evaluate 
	alternativeBlock and return."
	"See the comment of #critical: for the explanation how this pattern works
	before changing the code."

	| caught |
	caught := false.
	^[
		"Note: The following is tricky and depends on the fact that the VM will not switch between processes while executing byte codes (process switches happen only in real sends). The following test is written carefully so that it will result in bytecodes only.
	Do not change the following #== for #=, as #== is not a real message send, just a bytecode."
		excessSignals == 0
			ifTrue: [
				"If we come here, then the semaphore was locked when the test executed. 
				Evaluate the alternative block and answer its result."
				alternativeBlock value ]
			ifFalse: [
				excessSignals := excessSignals - 1.
				caught := true.
				mutuallyExcludedBlock value ] ]
		ensure: [ caught ifTrue: [ self signal ] ]
</details>

#### Semaphore>>#waitTimeoutMSecs: anInteger

Wait on this semaphore for up to the given number of milliseconds, then timeout. Return true if the deadline expired, false otherwise.


<details>
	<summary>See more</summary>
	
	waitTimeoutMSecs: anInteger
	"Wait on this semaphore for up to the given number of milliseconds, then timeout. 
	Return true if the deadline expired, false otherwise."
	| d |
	d := DelayWaitTimeout new setDelay: (anInteger max: 0) forSemaphore: self.
	^d wait
</details>

#### Semaphore>>#signal

Primitive. Send a signal through the receiver. If one or more processes have been suspended trying to receive a signal, allow the first one to proceed. If no process is waiting, remember the excess signal. Essential. See Object documentation whatIsAPrimitive.


<details>
	<summary>See more</summary>
	
	signal
	"Primitive. Send a signal through the receiver. If one or more processes 
	have been suspended trying to receive a signal, allow the first one to 
	proceed. If no process is waiting, remember the excess signal. Essential. 
	See Object documentation whatIsAPrimitive."

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals _ excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"


</details>

