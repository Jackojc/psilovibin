# psilovibin
Interactive language for MIDI livecoding.

![c++](https://img.shields.io/badge/c%2B%2B-17-blue.svg?style=flat)
[![issues](https://img.shields.io/github/issues/Jackojc/psilovibin.svg?style=flat)](https://github.com/Jackojc/psilovibin/issues)
[![pull requests](https://img.shields.io/github/issues-pr/Jackojc/psilovibin?style=flat)](https://github.com/Jackojc/psilovibin/pulls)
[![license](https://img.shields.io/github/license/Jackojc/psilovibin.svg?style=flat)](./LICENSE)

### Examples
```
let synth midi "NTS-1" 0
let drums midi "TR-6S" 10

let note 65
let kick 60 snare 61 hat 62

synth [ note note + 5 note + 3 note + 7 ] bpm 140 go
drums [ kick hat (kick snare) hat kick hat (kick snare) hat ] bpm 140 go
```

### Features
> TODO

### Tutorial & Reference
> TODO

### Requirements
> TODO

### Compiling & Running
> TODO

### Design Notes
> TODO

### Acknowledgements
> TODO

### Resources
> TODO

### License
This project uses the GPL-2.0 license. (check [LICENSE](LICENSE))

