# Png2Svg [![License](https://img.shields.io/badge/License-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT) 

Png to pixel-perfect svg conversion class for Delphi.

### Png
<img src="./Assets/Save_as.png" width="32" height="32"><img src="./Assets/Save_as.png" width="64" height="64"><img src="./Assets/Save_as.png" width="128" height="128"><img src="./Assets/Save_as.png" width="256" height="256">

### Svg
<img src="./Assets/Save_as.svg" width="32" height="32"><img src="./Assets/Save_as.svg" width="64" height="64"><img src="./Assets/Save_as.svg" width="128" height="128"><img src="./Assets/Save_as.svg" width="256" height="256">

## Command line 

### Usage

```
png2svg [-ia][-log][-nm][-m][-o][-p] <png filename|directory> [<svg filename|directory>]
```

### Params

|Param|Description|
|-----|-----------|
|-ia|Ignore alpha channel|
|-log|Show log|
|-nm|No pixel merge|
|-m|Minify|
|-o|Output svg|
|-p|Use path element|

## Examples

|Command|Description|
|-------|-----------|
|png2svg "C:\pngs\Example.png"|Converts "C:\pngs\Example.png" to "C:\pngs\Example.svg"|
|png2svg "C:\pngs\Example.png" "C:\svgs\Named.svg"|Converts "C:\pngs\Example.png" to "C:\svgs\Named.svg"|
|png2svg "C:\pngs\Example.png" Test.svg|Converts "C:\pngs\Example.png" to "C:\pngs\Test.svg"|
|png2svg Example.png|Converts Example.png in current directory to Example.svg|
|png2svg "C:\pngs\\"|Converts all png files in the "C:\pngs\\" directory with svg extension|
|png2svg "C:\pngs\\" svgs|Converts all png files from directory "C:\pngs\\" to directory "C:\pngs\svgs\\" with svg extension|
|png2svg "C:\pngs\\" "C:\svgs\\"|Converts all png files from directory "C:\pngs\\" to directory "C:\svgs\\" with svg extension|

## License

[MIT](https://github.com/TextEditorPro/Png2Svg/blob/main/LICENSE)

## Connect

https://www.linkedin.com/in/lassemarkusrautiainen/

## Donations

https://ko-fi.com/texteditorpro
