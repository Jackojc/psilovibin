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
