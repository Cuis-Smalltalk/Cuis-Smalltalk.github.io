## AndreasSystemProfiler

AndreasSystemProfiler uses sub-msec VM supported PC sampling. In Memory of Andreas Raab. Author, Friend, Colleague. http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html Released by Ron, Julie and David Some technical details from Eliot Miranda ( http://forum.world.st/AndreasSystemProfiler-Released-MIT-tp4664958p4665182.html ): Both AndreasSystemProfiler and MessageTally are periodic sampling profilers. The essential difference between AndreasSystemProfiler and MessageTally is in how the current method is sampled. MessageTally is driven from a high-priority process in a loop waiting on a delay. When the delay fires the lower-priority process being profiled is interrupted, its stack is walked to determine the methods along the call chain, and that data is recorded. But since the sampling occurs when the high-priority process preempts the lower-priority process, a sample is only taken at a preemption point. In particular, primitives are *not* profiled because they are not suspension points. A process can only be suspended on method activation (a non-primitive method activation, or primitive failure) or on backward branch. The cost of primitives is charged to a caller and is inferred by subtracting the cost of children of the caller from the caller itself (subtracting the number of samples in children of the caller form the number of samples in the caller itself). Another problem is that using the clock that underlies Delay, which is typically the clock used by processes being profiled, causes sampling errors due to the sampling and sampled processes cohering. Delays are limited in resolution (at best 1 millisecond) so if the profiled process waits on a delay it'll fire immediately after the profiling process (because the profiling process is at higher priority) and so the sampling process may only ever see the sampled process in a wait state. If MessageTally is used to profile multiple processes then a third problem is that if a primitive causes a process switch then its cost will end up being charged to the process switched-to, not switched from. This is again because sampling can only occur after a primitive has completed (successfully or not). AndreasSystemProfiler is driven from a high-priority process in a loop waiting on a Semaphore known to the VM. The profiling process uses a primitive to schedule a sample some number of ticks of the VM's high-performance clock in the future. When the time is reached the VM samples the current method and the current process, *before any process preemption takes place*, and independently of the standard clock, and signals the semaphore. The profiling process then collects the method,process pair via primitives. So AndreasSystemProfiler provides much more accurate results. That said there are still limitations with primitives and Cog. Currently Cog only samples "interpreter" primitives. Those primitives it implements in machine code (integer and float arithmetic, closure evaluation, at:, identityHash) are not sampled and won't show up; they will be charged to the calling method. This is fixable, since Cog actually compiles the sampling direct into interpreter primitive invocation when profiling is in effect and not at other times, but sampling could be a significant cost in these simple and performance-critical primitives.

### Methods
#### AndreasSystemProfiler>>#reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: threshold

<details>
	<summary>See more</summary>
	
	reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: threshold
	| line |
	tallyRoot tally = 0
		ifTrue: [
			line _ ' - no tallies obtained'.
			textStream nextPutAll: line; newLine.
			linesStream nextPut: line.
			talliesStream nextPut: nil ]
		ifFalse: [
			line _ 'Reporting - ' , totalTally printStringWithCommas, ' tallies, ' , totalTime printStringWithCommas , ' msec.'.
			textStream
				nextPutAll: line; newLine;
				newLine.
			linesStream nextPut: line; nextPut: ''.
			talliesStream nextPut: nil; nextPut: nil.
			tallyRoot
				fullPrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream 
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream.
		self reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream ]
</details>

#### AndreasSystemProfiler>>#observedProcess: aProcess

<details>
	<summary>See more</summary>
	
	observedProcess: aProcess
	observedProcess _ aProcess
</details>

#### AndreasSystemProfiler>>#totalMillisecondsFor: aSelector

Answer the sum of the time spent in all appareances in aSelector in the tree


<details>
	<summary>See more</summary>
	
	totalMillisecondsFor: aSelector
	"Answer the sum of the time spent in all appareances in aSelector in the tree"
	| totalTallies |
	totalTallies _ 0.
	tallyRoot
		treeDFSDo: [ :eachTally :parentTally |
			(eachTally methodSymbol == aSelector and: [ eachTally blockNestingCount = 0]) ifTrue: [
				totalTallies _ eachTally tally + totalTallies ]]
		afterChildrenDo: [ :eachTally | ]
		threshold: 0.0
		parent: nil.
	^ totalTallies asFloat / tallyRoot tally * totalTime
</details>

#### AndreasSystemProfiler>>#reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream

<details>
	<summary>See more</summary>
	
	reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows line |
	upTime := totalTime.
	oldSpaceEnd		:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**Memory**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ ' old ', oldSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ ' young ', (youngSpaceEnd - oldSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  ' used ', youngSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _  ' free ', (memoryEnd - youngSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line _ '**GCs**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' full ', fullGCs printString,' totalling ', fullGCTime printStringWithCommas, 'ms (', (100.0 * fullGCTime / upTime) rounded printString, '% uptime)'.
	fullGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * fullGCTime / fullGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' incr ', incrGCs printString, ' totalling ', incrGCTime printStringWithCommas, 'ms (', (100.0 * incrGCTime / upTime) rounded printString, '% uptime)'.
	incrGCs = 0 ifFalse: [
		line _ line, ', avg ', (1.0 * incrGCTime / incrGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' tenures ', tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		line _ line, ' (avg ', (1.0 * incrGCs / tenureCount) asInteger printString, ' GCs/tenure)'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line _ ' root table ', rootOverflows printStringWithCommas, ' overflows'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
</details>

#### AndreasSystemProfiler>>#totalMilliseconds

<details>
	<summary>See more</summary>
	
	totalMilliseconds
	^ totalTime
</details>

#### AndreasSystemProfiler>>#startProfiling

Start the profiler process taking samplesPerMsec samples per *milli* second


<details>
	<summary>See more</summary>
	
	startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	semaphore _ Semaphore new.
	"Try to get 10 samples per msec... Not really sure how this parameter is used, nor the meaning and relevance of #interruptChecksPerMSec"
	"ticks _ Time highResTimerTicksPerMillisecond // Smalltalk interruptChecksPerMSec."
	ticks _ Time highResTimerTicksPerMillisecond // 10.
	vmStats _ Smalltalk getVMParameters.
	startTime _ Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume
</details>

#### AndreasSystemProfiler>>#runProfilerProcess

Run the profiler process


<details>
	<summary>See more</summary>
	
	runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks methodForPrimitiveWhileTakingSample parentNode contextToTally |
	totalTally _ 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks _ 0.
	[ true ]
		whileTrue: [
			tallyStart _ Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyTicks _ Time primHighResClock - tallyStart.
			"In the extremely unlikely event of high res clock rollover, just ignore this tally"
			tallyTicks > 0 ifTrue: [
				totalTicks _ totalTicks + tallyTicks.
				process _ Smalltalk profileSample.
				methodForPrimitiveWhileTakingSample _ Smalltalk profilePrimitive.
				totalTally _ totalTally + 1.
				process
					ifNotNil: [
						methodForPrimitiveWhileTakingSample
							ifNil: [
								tallyRoot
									tally: (process suspendedContext ifNil: [ thisContext ])
									inProcess: process
									by: tallyTicks.
								]
							ifNotNil: [
								"The intention of this code is record which primitive was running when the VM took the sample."
								"In Eliot Miranda's words: 
									AndreasSystemProfiler is more accurate because it uses VM support to tell it which primitive was running when it took a sample. 
									MessageTally simply ascribes a primitive's cost to the method at the next suspension point, which, in some contexts, 
									can yield wildly misleading results."
								"The problem is that knowing just the primitive and the process doesn't give us the complete call stack.
								So, this is, in a sense, approximate."
								"
									AndreasSystemProfiler spyOn: [
								 	       [ #((1 2 3)) do: [ :each |
 									               each findLast: [ :ea |
									                        ea squared = ea ] ] ] bench ].
								Without asking #sender to the context, for this example
									AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
								gave:
									  |  2.9% (7 ms) (Number>>#raisedToInteger:)
									  |    2.2% (5 ms) (Float>>#timesTwoPower: )
								but #raisedToInteger: does NOT send #timesTwoPower:
								Approach taken: Add to parent node, but print with a note that specifies this is primitives, and maybe parent node is missing.
								Additionally, add a note, suggesting #profilerFriendlyCall:

								For example
									AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
										Here, the real parent node is missing.

									AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 1000]].
										Here, the proper tree is shown.

								See profilerFriendlyCall:
								"
								contextToTally _ process suspendedContext ifNil: [ thisContext ].
								contextToTally method selector == #profilerFriendlyCall: ifFalse: [
									contextToTally _ contextToTally sender ].
								parentNode _ tallyRoot
									tally: contextToTally
									inProcess: process
									by: tallyTicks.
								parentNode
									tallyPrimInMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
								]]]]
</details>

#### AndreasSystemProfiler>>#reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream

<details>
	<summary>See more</summary>
	
	reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream
	| totalSwitches pageOverflows pageDivorces actualSwitches line |
	vmStats size >= 61 ifFalse:[^self]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.
	
	line _ '**Processes**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Total process switches: ', totalSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Without Profiler: ', actualSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Stack page overflows: ', pageOverflows printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line _ '	Stack page divorces: ', pageDivorces printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
</details>

#### AndreasSystemProfiler>>#doReport

Report the results of this profiler run


<details>
	<summary>See more</summary>
	
	doReport
	"Report the results of this profiler run"
	| report |
	report _ self report.
	UISupervisor whenUIinSafeState: [
		SystemWindow
			editText: (Workspace withText: report)
			label: 'Spy Results'
			wrap: false ]
</details>

#### AndreasSystemProfiler>>#stopProfiling

Stop the profiler process


<details>
	<summary>See more</summary>
	
	stopProfiling
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime _ Time localMillisecondClock - startTime.
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :value | 
		value isNumber ifTrue: [
			vmStats at: idx put: (value - (vmStats at: idx)) ]].
	profilerProcess ifNotNil: [
		profilerProcess terminate.
		profilerProcess _ nil ]
</details>

#### AndreasSystemProfiler>>#report

Answer a report, with cutoff percentage of each element of the tree


<details>
	<summary>See more</summary>
	
	report
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :textStream |
		self
			reportTextOn: textStream
			linesOn: (DummyStream on: nil)
			talliesOn: (DummyStream on: nil) ]
</details>

#### AndreasSystemProfiler>>#isProfiling

<details>
	<summary>See more</summary>
	
	isProfiling
	^ profilerProcess notNil
</details>

#### AndreasSystemProfiler>>#spyOn: aBlock

Profile system activity during execution of aBlock.


<details>
	<summary>See more</summary>
	
	spyOn: aBlock
	"Profile system activity during execution of aBlock."
"	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method nesting: 1."
	tallyRoot := QSystemTally new class: thisContext receiver class method: thisContext method nesting: 0.
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]
</details>

#### AndreasSystemProfiler>>#reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream

Print a report, with cutoff percentage of each element of the tree (leaves, roots, tree)=2, on the stream, strm.


<details>
	<summary>See more</summary>
	
	reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: 0.2
</details>

## CPUWatcher

CPUWatcher implements a simple runaway process monitoring tool that will suspend a process that is taking up too much of Squeak's time and allow user interaction. By default it watches for a Process that is taking more than 80% of the time; this threshold can be changed. CPUWatcher can also be used to show cpu percentages for each process from within the ProcessBrowser. CPUWatcher startMonitoring. "process period 20 seconds, sample rate 100 msec, threshold 80%" CPUWatcher startMonitoringPeriod: 10 rate: 20 threshold: 0.8 suspendPorcine: true CPUWatcher current threshold: 0.5. "change from 80% to 50%" CPUWatcher stopMonitoring.

### Methods
#### CPUWatcher>>#tally

<details>
	<summary>See more</summary>
	
	tally
	^tally
</details>

#### CPUWatcher>>#threshold: thresh

What fraction of the time can a process be the active process before we stop it?


<details>
	<summary>See more</summary>
	
	threshold: thresh
	"What fraction of the time can a process be the active process before we stop it?"
	threshold _ (thresh max: 0.02) min: 1.0
</details>

#### CPUWatcher>>#debugProcess: aProcess fromMenu: aMenuMorph

<details>
	<summary>See more</summary>
	
	debugProcess: aProcess fromMenu: aMenuMorph
	aMenuMorph delete.
	self debugProcess: aProcess.
</details>

#### CPUWatcher>>#debugProcess: aProcess

<details>
	<summary>See more</summary>
	
	debugProcess: aProcess
	| uiPriority oldPriority |
	uiPriority _ Processor activeProcess priority.
	aProcess priority >= uiPriority ifTrue: [
		oldPriority _ ProcessBrowser setProcess: aProcess toPriority: uiPriority - 1
	].
	ProcessBrowser debugProcess: aProcess.
</details>

#### CPUWatcher>>#terminateProcess: aProcess fromMenu: aMenuMorph

<details>
	<summary>See more</summary>
	
	terminateProcess: aProcess fromMenu: aMenuMorph
	aMenuMorph delete.
	ProcessBrowser terminateProcess: aProcess.
</details>

#### CPUWatcher>>#stopMonitoring

<details>
	<summary>See more</summary>
	
	stopMonitoring
	watcher ifNotNil: [
		ProcessBrowser terminateProcess: watcher.
		watcher _ nil.
	]
</details>

#### CPUWatcher>>#findThePig

tally has been updated. Look at it to see if there is a bad process. This runs at a very high priority, so make it fast


<details>
	<summary>See more</summary>
	
	findThePig
	"tally has been updated. Look at it to see if there is a bad process.
	This runs at a very high priority, so make it fast"
	| countAndProcess | 
	countAndProcess _ tally sortedCounts first.
	(countAndProcess key / tally size > threshold) ifTrue: [ | proc |
		proc _ countAndProcess value.
		proc == Processor backgroundProcess ifTrue: [ ^self ].	"idle process? OK"
		self catchThePig: proc
	].

</details>

#### CPUWatcher>>#isMonitoring

<details>
	<summary>See more</summary>
	
	isMonitoring
	^watcher notNil
</details>

#### CPUWatcher>>#catchThePig: aProcess

nickname, allow-stop, allow-debug


<details>
	<summary>See more</summary>
	
	catchThePig: aProcess
	| rule |
	"nickname, allow-stop, allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) first.

	aProcess animatedUI notNil
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [
			rule ifFalse: [ ^self ].
			ProcessBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]
</details>

#### CPUWatcher>>#openWindowForSuspendedProcess: aProcess

<details>
	<summary>See more</summary>
	
	openWindowForSuspendedProcess: aProcess 

	UISupervisor whenUIinSafeState: [self openMorphicWindowForSuspendedProcess: aProcess]
</details>

#### CPUWatcher>>#resumeProcess: aProcess fromMenu: aMenuMorph

<details>
	<summary>See more</summary>
	
	resumeProcess: aProcess fromMenu: aMenuMorph
	aMenuMorph delete.
	ProcessBrowser resumeProcess: aProcess.
</details>

#### CPUWatcher>>#monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean

<details>
	<summary>See more</summary>
	
	monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| delay |
	self stopMonitoring.
	watcher _ [
	delay _ Delay forMilliseconds: msecs truncated.
	[ | thisTally |
	thisTally _ IdentityBag new: 20.
	secs * 1000 // msecs timesRepeat: [
		delay wait.
		thisTally add: Processor nextReadyProcess ].
	tally _ thisTally.
	aBoolean ifTrue: [ self findThePig ]] repeat ] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.
</details>

#### CPUWatcher>>#watcherProcess

<details>
	<summary>See more</summary>
	
	watcherProcess
	^watcher
</details>

#### CPUWatcher>>#openMorphicWindowForSuspendedProcess: aProcess

<details>
	<summary>See more</summary>
	
	openMorphicWindowForSuspendedProcess: aProcess
	| menu rule |
	menu _ MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu action: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowserWindow action: #openProcessBrowser.
	menu add: 'Resume'
		target: self
		action: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		action: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			action: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash asString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld

</details>

## MessageTally

My instances observe and report the amount of time spent in methods. MessageTally provides two different strategies available for profiling: * tallySends: and friends use the interpreter simulator to run the block, recording every method call. It gives you accurate counts of how many times methods get called, and by exactly which route. If you're debugging, or trying to figure out if a given method is getting called too many times, this is your tool. * spyOn: and friends use a high-priority Process to interrupt the block or process being spied on at periodic intervals. The interrupted call stack is then examined for caller information. This was moved to AndreasSystemProfiler.

### Methods
#### MessageTally>>#method

<details>
	<summary>See more</summary>
	
	method

	^method
</details>

#### MessageTally>>#leavesPrintExactOn: aStream

<details>
	<summary>See more</summary>
	
	leavesPrintExactOn: aStream

	| dict |
	dict _ IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	dict asArray sort
		do: [ :node |
			node printOn: aStream.
			node printSenderCountsOn: aStream ]
</details>

#### MessageTally>>#bump: hitCount fromSender: senderTally

Add this hitCount to the total, and include a reference to the sender responsible for the increment


<details>
	<summary>See more</summary>
	
	bump: hitCount fromSender: senderTally
	"Add this hitCount to the total, and include a reference to the
	sender responsible for the increment"
	self bump: hitCount.
	senders ifNil: [senders _ OrderedCollection new].
	senderTally == nil
		ifFalse: [senders add: (senderTally copyWithTally: hitCount)]
</details>

#### MessageTally>>#printSenderCountsOn: aStream

<details>
	<summary>See more</summary>
	
	printSenderCountsOn: aStream

	| mergedSenders |
	mergedSenders _ IdentityDictionary new.
	senders do: [ :node | | mergedNode |
		mergedNode _ mergedSenders at: node method ifAbsent: nil .
		mergedNode
			ifNil: [ mergedSenders at: node method put: node ]
			ifNotNil: [ mergedNode bump: node tally ]].
	mergedSenders asArray sort do: [ :node | 
		10 to: node tally printString size by: -1 do: [ :i | aStream space ].
		node printOn: aStream ]
</details>

#### MessageTally>>#tally

Answer the receiver's number of tally.


<details>
	<summary>See more</summary>
	
	tally
	"Answer the receiver's number of tally."

	^tally
</details>

#### MessageTally>>#sonsOver: threshold

<details>
	<summary>See more</summary>
	
	sonsOver: threshold

	| hereTally sons |
	(receivers isNil or: [ receivers size = 0 ]) ifTrue: [ ^#() ].
	hereTally _ tally.
	sons _ receivers select: [ :son | "subtract subNode tallies for primitive hits here"
		hereTally _ hereTally - son tally.
		son tally > threshold ].
	hereTally > threshold
		ifTrue: [
			| last |
			last _ MessageTally new class: class method: method.
			^sons copyWith: (last primitives: hereTally)].
	^sons
</details>

#### MessageTally>>#> aMessageTally

Refer to the comment in Magnitude|>.


<details>
	<summary>See more</summary>
	
	> aMessageTally 
	"Refer to the comment in Magnitude|>."

	^tally < aMessageTally tally
</details>

#### MessageTally>>#bumpBy: count

<details>
	<summary>See more</summary>
	
	bumpBy: count

	tally _ tally + count
</details>

#### MessageTally>>#primitives: anInteger

<details>
	<summary>See more</summary>
	
	primitives: anInteger

	tally _ anInteger.
	receivers _ nil
</details>

#### MessageTally>>#tally: context by: count

Explicitly tally the specified context and its stack.


<details>
	<summary>See more</summary>
	
	tally: context by: count
	"Explicitly tally the specified context and its stack."
	| sender |
	
	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	(sender _ context sender)ifNil: [
		^ (self bumpBy: count) tallyPath: context by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender by: count) tallyPath: context by: count
</details>

#### MessageTally>>#copyWithTally: hitCount

<details>
	<summary>See more</summary>
	
	copyWithTally: hitCount

	^ (MessageTally new class: class method: method)
		bump: hitCount
</details>

#### MessageTally>>#= aMessageTally

Compare the receiver with the argument and answer with true if the receiver is equal to the argument. Otherwise answer false.


<details>
	<summary>See more</summary>
	
	= aMessageTally

	self == aMessageTally ifTrue: [ ^ true ].
	self species == aMessageTally species ifFalse: [^ false].
	^ aMessageTally method == method
</details>

#### MessageTally>>#hash

Hash is reimplemented because = is implemented.


<details>
	<summary>See more</summary>
	
	hash
	"Hash is reimplemented because = is implemented."

	^method hash
</details>

#### MessageTally>>#leavesInto: leafDict fromSender: senderTally

<details>
	<summary>See more</summary>
	
	leavesInto: leafDict fromSender: senderTally

	| rcvrs |
	rcvrs _ self sonsOver: 0.
	rcvrs size = 0
		ifTrue: [ self into: leafDict fromSender: senderTally ]
		ifFalse: [
			rcvrs do: [ :node |
				node isPrimitives
					ifTrue: [ node leavesInto: leafDict fromSender: senderTally ]
					ifFalse: [ node leavesInto: leafDict fromSender: self ]]]
</details>

#### MessageTally>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	| myTally aSelector aClass |
	myTally := tally.
	receivers
		ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
	aStream
		print: myTally;
		space.
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass == class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]
</details>

#### MessageTally>>#isPrimitives

Detect pseudo node used to carry tally of local hits


<details>
	<summary>See more</summary>
	
	isPrimitives
	"Detect pseudo node used to carry tally of local hits"
	^ receivers == nil
</details>

#### MessageTally>>#fullPrintExactOn: aStream

<details>
	<summary>See more</summary>
	
	fullPrintExactOn: aStream

	aStream nextPutAll: '**Tree**'; newLine.
	self
		treePrintExactOn: aStream
		tabs: OrderedCollection new
		thisTab: ''.
	aStream nextPut: Character newPage; newLine.
	aStream nextPutAll: '**Leaves**'; newLine.
	self leavesPrintExactOn: aStream
</details>

#### MessageTally>>#bump: hitCount

<details>
	<summary>See more</summary>
	
	bump: hitCount
	tally _ tally + hitCount
</details>

#### MessageTally>>#treePrintExactOn: aStream tabs: tabs thisTab: myTab

<details>
	<summary>See more</summary>
	
	treePrintExactOn: aStream tabs: tabs thisTab: myTab

	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue: [
			self printOn: aStream ].
	sons _ receivers.
	sons isEmpty 
		ifFalse: [
			tabs addLast: myTab.
			sons _ sons asArray sort.
			(1 to: sons size) do: [ :i | 
					sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
					(sons at: i)
						treePrintExactOn: aStream
						tabs: tabs
						thisTab: sonTab ].
			tabs removeLast]
</details>

#### MessageTally>>#class: aClass method: aMethod

<details>
	<summary>See more</summary>
	
	class: aClass method: aMethod

	class _ aClass.
	method _ aMethod.
	tally _ 0.
	receivers _ Array new: 0
</details>

#### MessageTally>>#tallyPath: context by: count

<details>
	<summary>See more</summary>
	
	tallyPath: context by: count

	| aMethod path |
	aMethod _ context method.
	
	"Find the correct child (if there)"
	receivers do: [ :oldTally | 
		oldTally method == aMethod ifTrue: [path _ oldTally]].
	
	"Add new child if needed"
	path ifNil: [
		path _ MessageTally new class: context receiver class method: aMethod.
		receivers _ receivers copyWith: path].
	
	^ path bumpBy: count
</details>

#### MessageTally>>#< aMessageTally

Refer to the comment in Magnitude|<.


<details>
	<summary>See more</summary>
	
	< aMessageTally 
	"Refer to the comment in Magnitude|<."

	^tally > aMessageTally tally
</details>

#### MessageTally>>#into: leafDict fromSender: senderTally

<details>
	<summary>See more</summary>
	
	into: leafDict fromSender: senderTally

	| leafNode |
	leafNode _ leafDict at: method
		ifAbsent: [
			leafDict at: method
				put: (MessageTally new class: class method: method)].
	leafNode bump: tally fromSender: senderTally
</details>

## ProcessBrowser

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### ProcessBrowser>>#changeStackListTo: aCollection

<details>
	<summary>See more</summary>
	
	changeStackListTo: aCollection 

        stackList _ aCollection.
        self changed: #stackNameList.
        self context: nil
</details>

#### ProcessBrowser>>#selectedMethod

<details>
	<summary>See more</summary>
	
	selectedMethod
	^ methodText ifNil: [methodText _ selectedContext
						ifNil: ['']
						ifNotNil: [| pcRange | 
							methodText _ [ selectedContext sourceCode ]
								ifError: [ :err :rcvr | 'error getting method text' ].
							pcRange _ self pcRange.
							methodText asText
								addAttribute: TextColor red
								from: pcRange first
								to: pcRange last;
								
								addAttribute: TextEmphasis bold
								from: pcRange first
								to: pcRange last]]
</details>

#### ProcessBrowser>>#stepAt: millisecondSinceLast

See comment at #wantsSteps


<details>
	<summary>See more</summary>
	
	stepAt: millisecondSinceLast
	self updateProcessList
</details>

#### ProcessBrowser>>#updateStackList: depth

<details>
	<summary>See more</summary>
	
	updateStackList: depth 
	| suspendedContext oldHighlight |
	selectedProcess
		ifNil: [ ^ self changeStackListTo: nil ].
	oldHighlight _ selectedContext.
	selectedProcess == Processor activeProcess
		ifTrue: [
			self changeStackListTo: (thisContext stackOfSize: depth)]
		ifFalse: [
			suspendedContext _ selectedProcess suspendedContext.
			self changeStackListTo: (suspendedContext ifNotNil: [suspendedContext stackOfSize: depth])].
	self context: ((stackList notNil and:  [(stackList includes: oldHighlight)]) ifTrue: [oldHighlight])
</details>

#### ProcessBrowser>>#processNameList

since processList is a WeakArray, we have to strengthen the result


<details>
	<summary>See more</summary>
	
	processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally _ CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent _ tally
				ifNotNil: [
					(((tally occurrencesOf: each) * 100.0 / tally size) rounded
						asString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (each browserPrintStringFull: false)
		]
</details>

#### ProcessBrowser>>#processListIndex: index

<details>
	<summary>See more</summary>
	
	processListIndex: index 
	self selectedProcess: 
		(processList at: index ifAbsent: nil)
</details>

#### ProcessBrowser>>#nextContext

<details>
	<summary>See more</summary>
	
	nextContext
	| initialProcessIndex initialStackIndex |

	searchString isEmpty ifTrue: [ ^false ].
	initialProcessIndex _ self processListIndex.
	initialStackIndex _ self stackListIndex.
	initialProcessIndex
		to: self processList size
		do: [ :pi | 
			self processListIndex: pi.
			self stackNameList withIndexDo: [ :name :si |
				(pi ~= initialProcessIndex or: [si > initialStackIndex])
					ifTrue: [
						(name includesSubString: searchString)
							ifTrue: [
								self stackListIndex: si.
								^true ]]]].
	self processListIndex: initialProcessIndex.
	self stackListIndex: initialStackIndex.
	^false
</details>

#### ProcessBrowser>>#signalSemaphore

<details>
	<summary>See more</summary>
	
	signalSemaphore
	(selectedProcess suspendingList isKindOf: Semaphore)
		ifFalse: [^ self].
	[selectedProcess suspendingList signal] fork.
	(Delay forMilliseconds: 300) wait.
	"Hate to make the UI wait, but it's convenient..."
	self updateProcessList
</details>

#### ProcessBrowser>>#stackListIndex: index

<details>
	<summary>See more</summary>
	
	stackListIndex: index 
	self context: (
		(stackList isNil or: [index = 0]) ifFalse: [
			stackList at: index ifAbsent: nil ])
</details>

#### ProcessBrowser>>#startStackSizeWatcher

<details>
	<summary>See more</summary>
	
	startStackSizeWatcher

	StackSizeWatcher isWatching ifFalse: [
		StackSizeWatcher startWatchingWithDefaults ]
</details>

#### ProcessBrowser>>#selectedContext

<details>
	<summary>See more</summary>
	
	selectedContext
	^selectedContext
</details>

#### ProcessBrowser>>#processListIndex

<details>
	<summary>See more</summary>
	
	processListIndex
	processList ifNil: [ ^0 ].
	selectedProcess ifNil: [ ^0 ].
	^processList indexOf: selectedProcess
</details>

#### ProcessBrowser>>#stackNameList

<details>
	<summary>See more</summary>
	
	stackNameList
	^ stackList
		ifNil: [#()]
		ifNotNil: [stackList
				collect: [:each | each asString]]
</details>

#### ProcessBrowser>>#context: aContext

<details>
	<summary>See more</summary>
	
	context: aContext 
	selectedContext := aContext.
	selectedClass := nil.
	selectedSelector := nil.
	methodText := nil.
	self changed: #stackListIndex.
	self changed: #selectedMethod
</details>

#### ProcessBrowser>>#selectedClass

Answer the class in which the currently selected context's method was found.


<details>
	<summary>See more</summary>
	
	selectedClass
	"Answer the class in which the currently selected context's method was  
	found."
	^ selectedClass
		ifNil: [selectedClass := selectedContext receiver
				ifNil: [selectedSelector := selectedContext method selector.
					   selectedContext method methodClass]
				ifNotNil: [selectedContext methodClass]]
</details>

#### ProcessBrowser>>#stopStackSizeWatcher

<details>
	<summary>See more</summary>
	
	stopStackSizeWatcher

	StackSizeWatcher stopWatching.
	self updateProcessList
</details>

#### ProcessBrowser>>#stackListIndex

<details>
	<summary>See more</summary>
	
	stackListIndex
	stackList ifNil: [ ^0 ].
	selectedContext ifNil: [ ^0 ].
	^stackList indexOf: selectedContext
</details>

#### ProcessBrowser>>#wasProcessSuspendedByProcessBrowser: aProcess

<details>
	<summary>See more</summary>
	
	wasProcessSuspendedByProcessBrowser: aProcess
	^self class suspendedProcesses includesKey: aProcess
</details>

#### ProcessBrowser>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	methodText _ ''.
	selectedContext _ nil.
	searchString _ ''.
	self updateProcessList; processListIndex: 1
</details>

#### ProcessBrowser>>#notify: errorString at: location in: aStream

A syntax error happened when I was trying to highlight my pc. Raise a signal so that it can be ignored.


<details>
	<summary>See more</summary>
	
	notify: errorString at: location in: aStream 
	"A syntax error happened when I was trying to highlight my pc. 
	Raise a signal so that it can be ignored."
	Warning signal: 'syntax error'
</details>

#### ProcessBrowser>>#processList

<details>
	<summary>See more</summary>
	
	processList
	^ processList
</details>

#### ProcessBrowser>>#selectedProcess

<details>
	<summary>See more</summary>
	
	selectedProcess
	^selectedProcess
</details>

#### ProcessBrowser>>#findContext: aString

<details>
	<summary>See more</summary>
	
	findContext: aString
	| initialProcess initialContext found |
	searchString _ aString.
	searchString isEmpty
		ifTrue: [^ false].
	initialProcess _ selectedProcess.
	initialContext _ selectedContext.
	self processListIndex: 1.
	self stackListIndex: 1.
	found _ self nextContext.
	found ifFalse: [
		self selectedProcess: initialProcess.
		self context: initialContext].
	^ found
</details>

#### ProcessBrowser>>#searchString

<details>
	<summary>See more</summary>
	
	searchString
	^searchString
</details>

#### ProcessBrowser>>#updateProcessList

<details>
	<summary>See more</summary>
	
	updateProcessList
	| oldSelectedProcess |
	oldSelectedProcess _ selectedProcess.
	processList _ selectedProcess _ selectedSelector _ nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList _ Process allSubInstances reject: [:each | each isTerminated ].
	processList _ processList sort: [ :a :b | a priority >= b priority ].
	processList _ WeakArray withAll: processList.
	self changed: #processNameList.
	self selectedProcess: ((processList includes: oldSelectedProcess)
		ifTrue: [oldSelectedProcess])
</details>

#### ProcessBrowser>>#stopCPUWatcher

<details>
	<summary>See more</summary>
	
	stopCPUWatcher

	CPUWatcher stopMonitoring.
	self updateProcessList
</details>

#### ProcessBrowser>>#terminateProcess

<details>
	<summary>See more</summary>
	
	terminateProcess
	| rule |
	rule _ (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t kill ' , selectedProcess name.
			^ self].
	self class terminateProcess: selectedProcess.	
	self updateProcessList
</details>

#### ProcessBrowser>>#stackList

<details>
	<summary>See more</summary>
	
	stackList
	^ stackList
</details>

#### ProcessBrowser>>#pcRange

Answer the indices in the source code for the method corresponding to the selected context's program counter value.


<details>
	<summary>See more</summary>
	
	pcRange
	"Answer the indices in the source code for the method corresponding to  
	the selected context's program counter value."
	(selectedContext isNil or: [methodText isEmptyOrNil])
		ifTrue: [^ 1 to: 0].
	^selectedContext debuggerMap
		rangeForPC: (selectedContext pc ifNotNil: [:pc| pc] ifNil: [selectedContext method endPC])
		contextIsActiveContext: stackList first == selectedContext
</details>

#### ProcessBrowser>>#updateStackList

<details>
	<summary>See more</summary>
	
	updateStackList
	self updateStackList: 20
</details>

#### ProcessBrowser>>#resumeProcess

<details>
	<summary>See more</summary>
	
	resumeProcess
	selectedProcess
		ifNil: [^ self].
	self class resumeProcess: selectedProcess.
	self updateProcessList
</details>

#### ProcessBrowser>>#selectedSelector

Answer the class in which the currently selected context's method was found.


<details>
	<summary>See more</summary>
	
	selectedSelector
	"Answer the class in which the currently selected context's method was  
	found."
	^ selectedSelector
		ifNil: [selectedSelector := selectedContext receiver
				ifNil: [selectedClass := selectedContext method methodClass
					   selectedContext method selector]
				ifNotNil: [selectedContext selector]]
</details>

#### ProcessBrowser>>#text

<details>
	<summary>See more</summary>
	
	text
	^methodText
</details>

#### ProcessBrowser>>#moreStack

<details>
	<summary>See more</summary>
	
	moreStack
	self updateStackList: 2000
</details>

#### ProcessBrowser>>#suspendProcess

<details>
	<summary>See more</summary>
	
	suspendProcess
	| rule |
	(selectedProcess isSuspended or: [ selectedProcess isTerminated ])
		ifTrue: [^ self].
	rule _ (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [
			PopUpMenu inform: 'Nope, won''t suspend ' , selectedProcess name.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList
</details>

#### ProcessBrowser>>#selectedProcess: aProcess

<details>
	<summary>See more</summary>
	
	selectedProcess: aProcess
	selectedProcess _ aProcess.
	self updateStackList.
	self changed: #processListIndex
</details>

#### ProcessBrowser>>#startCPUWatcher

Answers whether I started the CPUWatcher


<details>
	<summary>See more</summary>
	
	startCPUWatcher
	"Answers whether I started the CPUWatcher"

	CPUWatcher isMonitoring ifFalse: [
		CPUWatcher startMonitoringPeriod: 1 rate: 25 threshold: 0.85 suspendPorcine: false.
		^true
	].
	^false

</details>

## QAbstractTally

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### QAbstractTally>>#treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: parentTally

Deep First Search


<details>
	<summary>See more</summary>
	
	treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: parentTally
	"Deep First Search"
	| sons |
	aBlock value: self value: parentTally.
	sons _ self sonsOver: threshold.
	sons isEmpty ifFalse: [
		sons _ self sortCollectionFrom: sons.
		1 to: sons size do: [ :i |
			(sons at: i) treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: self ]].
	anotherBlock value: self
</details>

#### QAbstractTally>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	tally := 0
</details>

#### QAbstractTally>>#tally

Answer the tally count for this node


<details>
	<summary>See more</summary>
	
	tally
	"Answer the tally count for this node"
	^tally
</details>

#### QAbstractTally>>#addTallies: count

Bump this tally by the specified amount


<details>
	<summary>See more</summary>
	
	addTallies: count
	"Bump this tally by the specified amount"
	tally _ tally + count
</details>

#### QAbstractTally>>#sonsOver: threshold

Answer the sons with tallys over the given threshold


<details>
	<summary>See more</summary>
	
	sonsOver: threshold
	"Answer the sons with tallys over the given threshold"
	receivers ifNil:[^#()].
	^receivers asArray select:[:son | son tally >= threshold].
</details>

#### QAbstractTally>>#printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

<details>
	<summary>See more</summary>
	
	printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally
	self subclassResponsibility
</details>

#### QAbstractTally>>#process: aProcess

<details>
	<summary>See more</summary>
	
	process: aProcess
	process := aProcess
</details>

#### QAbstractTally>>#fullPrintOn: textStream linesOn: linesStream talliesOn: talliesStream threshold: perCent time: totalTime reportOnly: aProcessOrNil

<details>
	<summary>See more</summary>
	
	fullPrintOn: textStream linesOn: linesStream talliesOn: talliesStream threshold: perCent time: totalTime reportOnly: aProcessOrNil
	| threshold line |  
	threshold _ (perCent asFloat / 100 * tally) rounded.
	line _ '**Tree**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	self
		rootPrintOn: textStream
		linesOn: linesStream
		talliesOn: talliesStream 
		total: tally
		totalTime: totalTime
		threshold: threshold
		reportOnly: aProcessOrNil
</details>

#### QAbstractTally>>#sortCollectionFrom: aCollection

Create a sorted collection from the given input


<details>
	<summary>See more</summary>
	
	sortCollectionFrom: aCollection
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]
</details>

#### QAbstractTally>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	self subclassResponsibility
</details>

#### QAbstractTally>>#rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

<details>
	<summary>See more</summary>
	
	rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups dashes line tabsAndTreeLines thisTab |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g | | sons |
		sons _ self sortCollectionFrom: g.
		dashes _ '--------------------------------'.
		line _ 'Process: ', (g anyOne process browserPrintStringFull: false).
		textStream
			nextPutAll: dashes; newLine;
			nextPutAll: line; newLine;
			nextPutAll: dashes; newLine.
		linesStream
			nextPut: dashes;
			nextPut: line;
			nextPut: dashes.
		talliesStream
			nextPut: nil; nextPut: nil; nextPut: nil.
		sons do: [ :son |
			tabsAndTreeLines _ OrderedCollection new.
			thisTab _ '  '.
			son
				treeDFSDo: [ :eachTally :parentTally |
					eachTally
						printOn: textStream
						linesOn: linesStream talliesOn: talliesStream
						tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally.
					tabsAndTreeLines addLast: thisTab.
					thisTab := (eachTally sonsOver: threshold) size > 1 ifTrue: ['  |'] ifFalse: ['  '].
				]
				afterChildrenDo: [ :eachTally | tabsAndTreeLines removeLast ]
				threshold: threshold
				parent: self.
			].
		textStream newLine ]
</details>

#### QAbstractTally>>#asArray

esto es feo. normalmente los link esta en un LinkedList. Estamos de acuerdo que usualmente es el pedo... pero, es el elemento una coleccion o no????


<details>
	<summary>See more</summary>
	
	asArray
	"esto es feo. normalmente los link esta en un LinkedList.
	Estamos de acuerdo que usualmente es el pedo... pero, es el elemento una coleccion o no????"
	"(jmv) (style) Mhhh. Normally instances of Link are in a LinkedList, that provides collection protocol.
	Here, we allow a subInstance of Link to start behaving as a Collection.
	Not bad per se, but inconsistent with other uses of Link..."
	| link |
	^Array streamContents:[:s|
		link := self.
		[link == nil] whileFalse:[
			s nextPut: link.
			link := link nextLink.
		].
	].
</details>

#### QAbstractTally>>#tallyPrimInMethod: aMethod by: count

<details>
	<summary>See more</summary>
	
	tallyPrimInMethod: aMethod by: count
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		(node method == aMethod and: [ node wasInPrimitive ]) ifTrue: [
			^node addTallies: count ].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node wasInPrimitive: true.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count
</details>

#### QAbstractTally>>#process

<details>
	<summary>See more</summary>
	
	process
	^process
</details>

## QSystemTally

Tally for assembling system profiles. It's a subclass of Link so receivers actually holds the first element in a linked list. This is good for making the profiling overhead low and relatively constant (no OrderedCollections to grow).

### Methods
#### QSystemTally>>#method

Answer the CompiledMethod associated with this tally


<details>
	<summary>See more</summary>
	
	method
	"Answer the CompiledMethod associated with this tally"
	^method
</details>

#### QSystemTally>>#actualClass

<details>
	<summary>See more</summary>
	
	actualClass
	^method methodClass
</details>

#### QSystemTally>>#wasInPrimitive

<details>
	<summary>See more</summary>
	
	wasInPrimitive
	^ wasInPrimitive
</details>

#### QSystemTally>>#initialize

Subclasses should redefine this method to perform initializations on instance creation


<details>
	<summary>See more</summary>
	
	initialize
	super initialize.
	wasInPrimitive _ false
</details>

#### QSystemTally>>#class: aClass method: aCompiledMethod nesting: blockNestingCount

<details>
	<summary>See more</summary>
	
	class: aClass method: aCompiledMethod nesting: blockNestingCount
	class := aClass.
	method := aCompiledMethod.
	blockNesting := blockNestingCount
</details>

#### QSystemTally>>#tallyChild: context inProcess: aProcess by: count

Find a suitable child, or add a new one if needed.


<details>
	<summary>See more</summary>
	
	tallyChild: context inProcess: aProcess by: count
	"Find a suitable child, or add a new one if needed."
	| aMethod node nesting |
	aMethod _ context method.
	nesting _ self blockNestingCountOf: context.
	node _ receivers.
	[ node == nil ] whileFalse: [
		(context receiver class == node actualReceiverClass and: [aMethod == node method and: [ node process == aProcess and: [ nesting = node blockNestingCount ]]]) ifTrue: [
			node wasInPrimitive ifFalse: [
				^node addTallies: count ]].
		node _ node nextLink ].
	node _ QSystemTally new
				class: context receiver class
				method: aMethod
				nesting: nesting.
	node process: aProcess.
	node nextLink: receivers.
	receivers _ node.
	^node addTallies: count
</details>

#### QSystemTally>>#printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

<details>
	<summary>See more</summary>
	
	printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream nextPutAll: '% ('.
		percentage * totalTime / 100 printOn: lineStream fractionDigits: 1.
		lineStream nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [
			self flag: #profilerFriendlyCall:.
			parentTally methodSymbol == #profilerFriendlyCall:
				ifTrue: [
					lineStream nextPutAll: '   -- primitive (reported properly)' ]
				ifFalse: [
					lineStream nextPutAll: '   -- primitive (real sender possibly omitted, see #profilerFriendlyCall:)' ]
			].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self
</details>

#### QSystemTally>>#wasInPrimitive: aBoolean

<details>
	<summary>See more</summary>
	
	wasInPrimitive: aBoolean
	wasInPrimitive _ aBoolean
</details>

#### QSystemTally>>#actualReceiverClass

<details>
	<summary>See more</summary>
	
	actualReceiverClass
	^class
</details>

#### QSystemTally>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream
	blockNesting timesRepeat: [
		aStream nextPutAll: '[] in '].
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally
</details>

#### QSystemTally>>#blockNestingCountOf: context

<details>
	<summary>See more</summary>
	
	blockNestingCountOf: context
	| count nest |
	count := 0.
	nest := context.
	[nest closure notNil] whileTrue:
		[count := count + 1.
		 nest := nest closure outerContext].
	^count
</details>

#### QSystemTally>>#blockNestingCount

<details>
	<summary>See more</summary>
	
	blockNestingCount
	^blockNesting
</details>

#### QSystemTally>>#methodSymbol

<details>
	<summary>See more</summary>
	
	methodSymbol
	^method selector
</details>

#### QSystemTally>>#tally: context inProcess: aProcess by: count

Explicitly tally the specified context and its stack.


<details>
	<summary>See more</summary>
	
	tally: context inProcess: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender senderNode |

	"Add to this node if appropriate"
	(context receiver class == class and: [ context method == method and: [ aProcess == process | process isNil and: [ (self blockNestingCountOf: context) = blockNesting ]]]) ifTrue: [
		self wasInPrimitive ifFalse: [
			self addTallies: count.
			^self ]].

	sender _ context sender.
	"No sender means end of recursion. Add new root to the tree."
	sender ifNil: [
		self addTallies: count.
		^self tallyChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary).
	Then, add us."
	senderNode _ self tally: sender inProcess: aProcess by: count.
	^senderNode tallyChild: context inProcess: aProcess by: count
</details>

## SpaceTally

I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem try something like: ((SpaceTally new spaceTally: (Array with: Morph with: Point)) asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) SpaceTally new systemWideSpaceTally Also try: 'MemoryAnalysis.txt' asFileEntry forceWriteStreamContents: [ :stream | SpaceTally new printSpaceAnalysis: 1 on: stream ]

### Methods
#### SpaceTally>>#spaceTally: classes

Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables.


<details>
	<summary>See more</summary>
	
	spaceTally: classes 
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables. "
	"
	SpaceTally new spaceTally: (Array with: TextModelMorph with: Point)
	"
	self preAllocateResultsFor: classes.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results
		 sort: [ :a :b | a spaceForInstances > b spaceForInstances ];
		 yourself
</details>

#### SpaceTally>>#computeSpaceUsage

<details>
	<summary>See more</summary>
	
	computeSpaceUsage

	| entry c |
	1 to: results size do: [:i |
		entry := results at: i.
		c := Smalltalk at: entry analyzedClassName.
		entry codeSize: c spaceUsed.
		entry instanceCount: c instanceCount.
		entry spaceForInstances: (self spaceForInstancesOf: c).
		Smalltalk garbageCollectMost].
	

</details>

#### SpaceTally>>#preAllocateResultsFor: classes

<details>
	<summary>See more</summary>
	
	preAllocateResultsFor: classes

	results := OrderedCollection new: classes size.
	classes do: [:cl | results add: (SpaceTallyItem analyzedClassName: cl name)].
	results _ results asArray.

</details>

#### SpaceTally>>#compareTallyIn: beforeFileName to: afterFileName

SpaceTally new compareTallyIn: 'tally' to: 'tally2'


<details>
	<summary>See more</summary>
	
	compareTallyIn: beforeFileName to: afterFileName
	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"

	| answer beforeDict a afterDict allKeys before after diff |
	beforeDict _ Dictionary new.
	beforeFileName asFileEntry readStreamDo: [ :s |
		[s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			beforeDict at: a first put: a allButFirst ]].

	afterDict _ Dictionary new.
	afterFileName asFileEntry readStreamDo: [ :s |
		[ s atEnd ] whileFalse: [
			a _ Array readFrom: s nextLine.
			afterDict at: a first put: a allButFirst ]].

	answer _ String streamContents: [ :stream |
		allKeys _ (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.
		allKeys do: [ :each |
			before _ beforeDict at: each ifAbsent: [#(0 0 0)].
			after _ afterDict at: each ifAbsent: [#(0 0 0)].
			diff _ before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].
			diff = #(0 0 0) ifFalse: [
				stream nextPutAll: each, '  ', diff printString; newLine.
			].
		]].

	TextModel new contents: answer; openLabel: 'space diffs'
</details>

#### SpaceTally>>#spaceForInstancesOf: aClass

Answer a pair of the number of bytes consumed by all instances of the given class, including their object headers, and the number of instances.


<details>
	<summary>See more</summary>
	
	spaceForInstancesOf: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	^ Smalltalk isSpur
		ifTrue: [ self spaceForInstancesOfSpur: aClass ]
		ifFalse: [ self spaceForInstancesOfPreSpur: aClass ]
</details>

#### SpaceTally>>#saveTo: aFileName

| st | st := SpaceTally new. st spaceTally: (Array with: EllipseMorph with: Point). st saveTo: 'spaceTally2'


<details>
	<summary>See more</summary>
	
	saveTo: aFileName
	"
	| st |
	st := SpaceTally new.
	st spaceTally: (Array with: EllipseMorph with: Point).
	st saveTo: 'spaceTally2'
	"

	DirectoryEntry smalltalkImageDirectory // aFileName forceWriteStreamDo: [ :stream |
		results do: [ :each |
				stream nextPutAll: each analyzedClassName asString; 
						nextPutAll: ' '; nextPutAll: each codeSize printString; 
						nextPutAll: ' '; nextPutAll: each instanceCount printString; 
						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine ]]
</details>

#### SpaceTally>>#printSpaceAnalysis

SpaceTally new printSpaceAnalysis


<details>
	<summary>See more</summary>
	
	printSpaceAnalysis	
	"
	SpaceTally new printSpaceAnalysis
	"

	DirectoryEntry smalltalkImageDirectory // 'STspace.text' writeStreamDo: [ :stream |
		self printSpaceAnalysis: 1 on: stream ]
</details>

#### SpaceTally>>#spaceForInstancesOfPreSpur: aClass

Answer a pair of the number of bytes consumed by all instances of the given class, including their object headers, and the number of instances.


<details>
	<summary>See more</summary>
	
	spaceForInstancesOfPreSpur: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instCount isCompact instVarBytes bytesPerElement contentBytes headerBytes total |
	instCount _ aClass instanceCount.
	instCount = 0 ifTrue: [^ 0].
	isCompact _ aClass indexIfCompact > 0.
	instVarBytes _ aClass instSize * 4.
	aClass isVariable
		ifTrue: [
			bytesPerElement _ aClass isBytes ifTrue: [1] ifFalse: [4].
			total _ 0.
			aClass allInstancesDo: [:inst |
				contentBytes _ instVarBytes + (inst size * bytesPerElement).
				headerBytes _
					contentBytes > 255
						ifTrue: [12]
						ifFalse: [isCompact ifTrue: [4] ifFalse: [8]].
				total _ total + headerBytes + contentBytes].
			^ {total. instCount}]
		ifFalse: [
			headerBytes _
				instVarBytes > 255
					ifTrue: [12]
					ifFalse: [isCompact ifTrue: [4] ifFalse: [8]].
			^ {instCount * (headerBytes + instVarBytes). instCount}].

</details>

#### SpaceTally>>#printSpaceDifferenceFrom: fileName1 to: fileName2

For differential results, run printSpaceAnalysis twice with different fileNames, then run this method... 'STspace.text1' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ]. --- do something that uses space here --- 'STspace.text2' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ]. SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'


<details>
	<summary>See more</summary>
	
	printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		'STspace.text1' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
			--- do something that uses space here ---
		'STspace.text2' asFileEntry writeStreamDo: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| coll1 coll2 item |
	coll1 _ OrderedCollection new.
	DirectoryEntry smalltalkImageDirectory // fileName1 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [coll1 add: stream crLfNextLine]].
	
	coll2 _ OrderedCollection new.
	DirectoryEntry smalltalkImageDirectory // fileName2 readStreamDo: [ :stream |
		[stream atEnd] whileFalse: [
			item _ stream crLfNextLine.
			((coll1 includes: item) and: [(item endsWith: 'percent') not])
				ifTrue: [coll1 remove: item]
				ifFalse: [coll2 add: item]]].

	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.

</details>

#### SpaceTally>>#printSpaceAnalysis: threshold on: aStream

SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')


<details>
	<summary>See more</summary>
	
	printSpaceAnalysis: threshold on: aStream
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace _ totalInstCount _ totalInstSpace _ n _ 0.
	results _ OrderedCollection new: Smalltalk classNames size.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace _ cl spaceUsed.
				barBlock value: (n _ n+1).
				Smalltalk garbageCollectMost.
				instCount _ cl instanceCount.
				instSpace _ (cl indexIfCompact > 0 ifTrue: [4] ifFalse: [8]) * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!"
				cl isVariable
					ifTrue: [
						eltSize _ cl isBytes ifTrue: [1] ifFalse: [4].
						cl allInstancesDo: [ :x |
							instSpace _ instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace _ instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace _ totalCodeSpace + codeSpace.
				totalInstCount _ totalInstCount + instCount.
				totalInstSpace _ totalInstSpace + instSpace]].
	totalPercent _ 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results _ (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent _ s spaceForInstances*100.0/totalInstSpace.
		totalPercent _ totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil
</details>

#### SpaceTally>>#systemWideSpaceTally

Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed by the code and instances of each class in the system. Note that code sizes do not currently report memory consumed by class variables.


<details>
	<summary>See more</summary>
	
	systemWideSpaceTally
	"Answer a collection of SpaceTallyItems representing the memory space (in bytes) consumed 	by the code and instances of each class in the system. Note that code sizes do not currently 	report memory consumed by class variables. "

	"(SpaceTally new systemWideSpaceTally asSortedCollection: [:a :b | a last > b last]) asArray"

	self preAllocateResultsFor: Smalltalk allClasses.
	Smalltalk garbageCollect.
	self computeSpaceUsage.
	^ results

</details>

#### SpaceTally>>#results

<details>
	<summary>See more</summary>
	
	results

	^ results
</details>

#### SpaceTally>>#spaceForInstancesOfSpur: aClass

Answer a pair of the number of bytes consumed by all instances of the given class, including their object headers, and the number of instances.


<details>
	<summary>See more</summary>
	
	spaceForInstancesOfSpur: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instances total |
	instances := aClass allInstances.
	instances isEmpty ifTrue: [^#(0 0)].
	total := 0.
	aClass isVariable
		ifTrue:
			[instances do:
				[:i| total := total + (aClass byteSizeOfInstanceOfSize: i basicSize)]]
		ifFalse:
			[total := instances size * aClass byteSizeOfInstance].
	^{ total. instances size }
</details>

## SpaceTallyItem

I'm represent an entry in the spaceTally.

### Methods
#### SpaceTallyItem>>#codeSize

<details>
	<summary>See more</summary>
	
	codeSize

	^ codeSize
</details>

#### SpaceTallyItem>>#instanceCount

<details>
	<summary>See more</summary>
	
	instanceCount

	^ instanceCount
</details>

#### SpaceTallyItem>>#spaceForInstances

<details>
	<summary>See more</summary>
	
	spaceForInstances

	^ spaceForInstances
</details>

#### SpaceTallyItem>>#printOn: aStream

Append to the argument, aStream, a sequence of characters that identifies the receiver.


<details>
	<summary>See more</summary>
	
	printOn: aStream

	analyzedClassName
		ifNotNil: [ aStream nextPutAll: analyzedClassName asString]. 
	aStream nextPutAll: ' ('.
	codeSize
		ifNotNil: [ aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize asString]. 
	instanceCount
		ifNotNil: [ aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount asString]. 
	spaceForInstances
		ifNotNil: [ aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances asString]. 
	aStream nextPut: $).
	
</details>

#### SpaceTallyItem>>#spaceForInstances: aNumber

<details>
	<summary>See more</summary>
	
	spaceForInstances: aNumber

	spaceForInstances := aNumber
</details>

#### SpaceTallyItem>>#codeSize: aNumber

<details>
	<summary>See more</summary>
	
	codeSize: aNumber

	codeSize := aNumber
</details>

#### SpaceTallyItem>>#analyzedClassName: aClassName

<details>
	<summary>See more</summary>
	
	analyzedClassName: aClassName

	analyzedClassName := aClassName
</details>

#### SpaceTallyItem>>#instanceCount: aNumber

<details>
	<summary>See more</summary>
	
	instanceCount: aNumber

	instanceCount := aNumber
</details>

#### SpaceTallyItem>>#analyzedClassName

<details>
	<summary>See more</summary>
	
	analyzedClassName

	^ analyzedClassName
</details>

## StackSizeWatcher

Main comment stating the purpose of this class and relevant relationship to other classes. Possible useful expressions for doIt or printIt. Structure: instVar1 type -- comment about the purpose of instVar1 instVar2 type -- comment about the purpose of instVar2 Any further useful comments about the general approach of this implementation.

### Methods
#### StackSizeWatcher>>#startWatching

<details>
	<summary>See more</summary>
	
	startWatching

	self assertIsNotWatching.
	
	watcher _ [ [self watch] repeat ] newProcess.
	watcher priority: Processor lowIOPriority.
	watcher name: 'StackSizeWatcher monitor'.
	watcher resume.
	Processor yield
</details>

#### StackSizeWatcher>>#isWatching

<details>
	<summary>See more</summary>
	
	isWatching

	^watcher notNil 
</details>

#### StackSizeWatcher>>#watch

<details>
	<summary>See more</summary>
	
	watch

	| processToWatch |

	delayBetweenChecks wait.
	processToWatch := Processor nextReadyProcess.
	(self shouldStopAndDebug: processToWatch) ifTrue: [ self debug: processToWatch ]

</details>

#### StackSizeWatcher>>#isStackTooDeepAt: aProcess

aProcess suspendedContext should never be nil under this circunstances but checking that just in case - Hernan


<details>
	<summary>See more</summary>
	
	isStackTooDeepAt: aProcess

	"aProcess suspendedContext should never be nil under this circunstances but checking that just in case - Hernan"
	^aProcess suspendedContext 
		ifNil: [ false ] 
		ifNotNil: [ :topContext | topContext depthBelow > stackSizeThreashold ]
		
</details>

#### StackSizeWatcher>>#startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold

<details>
	<summary>See more</summary>
	
	startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold

	self assertIsNotWatching.
	
	self changeTimeBetweenChecksTo: aTimeBetweenChecks.
	self changeStackSizeThresholdTo: aThreshold.
	self startWatching 
</details>

#### StackSizeWatcher>>#changeTimeBetweenChecksTo: aTimeBetweenChecks

time in milliseconds - Hernan


<details>
	<summary>See more</summary>
	
	changeTimeBetweenChecksTo: aTimeBetweenChecks

	"time in milliseconds - Hernan"

	delayBetweenChecks _ Delay forMilliseconds: aTimeBetweenChecks.

</details>

#### StackSizeWatcher>>#assertIsWatching

<details>
	<summary>See more</summary>
	
	assertIsWatching
	
	self isWatching ifFalse: [ self error: 'It is not watching' ]
</details>

#### StackSizeWatcher>>#changeStackSizeThresholdTo: aThreshold

<details>
	<summary>See more</summary>
	
	changeStackSizeThresholdTo: aThreshold
	
	stackSizeThreashold _ aThreshold 
</details>

#### StackSizeWatcher>>#assertIsNotWatching

<details>
	<summary>See more</summary>
	
	assertIsNotWatching
	
	self isNotWatching ifFalse: [ self error: 'Already watching' ].
</details>

#### StackSizeWatcher>>#stopWatching

<details>
	<summary>See more</summary>
	
	stopWatching

	self assertIsWatching.
	
	watcher terminate.
	watcher _ nil
</details>

#### StackSizeWatcher>>#shouldStopAndDebug: aProcess

Verify the process can be debugged before #isStackTooDeepAt: to avoid loosing time in #isStackDeeperThan: that is more expensive - Hernan


<details>
	<summary>See more</summary>
	
	shouldStopAndDebug: aProcess

	"Verify the process can be debugged before #isStackTooDeepAt: to avoid
	loosing time in #isStackDeeperThan: that is more expensive - Hernan"

	^(self canDebug: aProcess) and: [self isStackTooDeepAt: aProcess]


</details>

#### StackSizeWatcher>>#debug: aProcess

<details>
	<summary>See more</summary>
	
	debug: aProcess

	aProcess debugFullWithTitle: 'Interrupted - Stack too deep'.

</details>

#### StackSizeWatcher>>#canDebug: aProcess

<details>
	<summary>See more</summary>
	
	canDebug: aProcess

	^(ProcessBrowser rulesFor: aProcess) second
	

</details>

#### StackSizeWatcher>>#isNotWatching

<details>
	<summary>See more</summary>
	
	isNotWatching

	^self isWatching not
</details>

## TimeProfileBrowser

A TimeProfileBrowser is a browser visualizing the runtime profile of an executed Smalltalk block. It is useful for finding performance bottlenecks in code. When optimizing code it can be hard to know what methods actually constitute the bulk of the execution time. Is it a few methods that take very long time to execute or is it perhaps a single method that gets executed a thousand times? The block is first spied on using a MessageTally instance (which has even more funtionality than used by the TimeProfileBrowser) which samples the block during it's execution and collects the amount of time approximately spent in the methods executed. Then the methods are shown in the browser with their relative execution time in percent. Example: TimeProfileBrowser onBlock: [20 timesRepeat: [Transcript show: 100 factorial printString]]

### Methods
#### TimeProfileBrowser>>#runBlock: aBlock

TimeProfileBrowser spyOn: [20 timesRepeat: [Transcript show: 100 factorial printString]]


<details>
	<summary>See more</summary>
	
	runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: Array new.
	talliesStream _ WriteStream on: Array new.
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self triggerEvent: #decorateButtons.
	^result
</details>

#### TimeProfileBrowser>>#messageListIndex: anInteger

Set the index of the selected item to be anInteger.


<details>
	<summary>See more</summary>
	
	messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."
	
	selectedMessage _ (talliesList isInBounds: anInteger) ifTrue: [ talliesList at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged
</details>

#### TimeProfileBrowser>>#initializeMessageList: anArray talliesList: anotherArray

<details>
	<summary>See more</summary>
	
	initializeMessageList: anArray talliesList: anotherArray

	messageList _ anArray.
	talliesList _ anotherArray.
	selectedMessage _ talliesList isEmpty ifFalse: [ talliesList first ]
</details>

#### TimeProfileBrowser>>#messageListIndex

Answer the index of the selected message selector into the currently selected message category.


<details>
	<summary>See more</summary>
	
	messageListIndex
	"Answer the index of the selected message selector into the currently 
	selected message category."

	selectedMessage ifNil: [ ^0 ].
	^talliesList indexOf: selectedMessage
</details>

