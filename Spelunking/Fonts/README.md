Put SadConsole `.font` files in this directory.

Set `Window:TileFontPath` in `Appsettings.json` to the copied font file, for example:

```json
"TileFontPath": "Fonts/MyTiles.font"
```

The path can be relative to the application output directory or absolute. Files in this
directory are copied to the output during build.
