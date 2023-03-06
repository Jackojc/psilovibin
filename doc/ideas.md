# Ideas

### Design Principles
- Simplicity => Easy to use, terse and low brain-to-code impedence
- Low Latency => We're dealing with audio so latency from keystroke to sound is essential

### Future Features
- MIDI CC
- Arithmetic for literals
- Partial matching of identifiers/keywords
- Note literals
- Arpeggios

### Considerations
- How do we keep different channels in sync while also being realtime?
	- Scheduling a new track while one is already playing
	- Channels playing at different tempos
	- Can we just generate a list of instructions with a timestamp that we
	modulo the current time with to decide when to send the event?

### Grammar Ideas
##### Prop-like Nesting
The Prop languages uses notation like `2(...)` to set the ratio
of the nested sequence length to the parent. In PV we just multiply
by two implicitly but this isn't very flexible.

##### Rests/Empty notes
```
[ 65 65 - 67 [ 69 69 - ]]
```

### Semantic Ideas
- Mandate that a BPM is set for all channels during semantic analysis

### Architecture
- Make passes non-mutating and instead produce a new tree

### Scheduler
Use a back-buffer like approach where we flip between two trees. One tree
will be walked by the sequencer on another thread where it generates MIDI.
The other tree will be written to by the compiler as the user enters commands.
When the user enters a command, we compile and then swap the trees with the
sequencer picking up where it left off index-wise (so we need to be careful
with references being invalidated).

### Passes
- MIDI hex visualiser
