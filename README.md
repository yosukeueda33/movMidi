# movMidi

Midi to movie playback converter.

usage example:
https://youtube.com/shorts/Brvy3cwDJFk?feature=share

# Build.
```bash
stack build
```

# Execute.
```bash
stack run 3 ./configs/2.dhall
```

# Command for mpv.

```bash
mpv mov.mp4 --input-ipc-server=/tmp/mpvsocket --keep-open=yes --pause=yes --no-border
```
Resizing window is Alt+F8 for this on Linux.

