![alt tag](https://cloud.githubusercontent.com/assets/10290469/15780076/ef8afc0a-296d-11e6-862a-abc5fa1eb382.png)

# PPCL

This is a plugin and syntax for the PPCL langauge for Sublime Text 3.
It has a syntax highliting for .pcl files
It also has features specific to writing PPCL


Written by Brien Blandford at Smith Engineering, PLLC.
http://www.smith-eng.com/index.php
Contributions from Nathan Watkins


*LineChanger.py is a PPCL plugin for the Sublime Text 3 text editor.*

*Copyright (C) 2016  Brien Blandford*

*This program is free software: you can redistribute it and/or modify*
*it under the terms of the MIT license included in this package.*

*This program is distributed in the hope that it will be useful,*
*but WITHOUT ANY WARRANTY; without even the implied warranty of*
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the*
*MIT license for more details.*

*You should have received a copy of the license with this package,*
*but one can be found at <https://opensource.org/licenses/MIT>.*



## Installing
- **Ensure** [Sublime Text 3](https://www.sublimetext.com/3) is installed
	- You should really consider getting a license to support the awesomeness of ST3
- **via Package Control** *coming soon (hopefully) and preferred*
	- Download [Package Control](https://packagecontrol.io/) and follow their instructions
	- in ST3, access the console by typing <kbd>ctrl</kbd> + <kbd>`</kbd>
	- from [Package Control](https://packagecontrol.io/), look for the install link, and paste their code into the console.  Hit <kbd>enter</kbd>
	- to access the command palette, type  <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>p</kbd>
	- type *Package Control: Install Package* in the command palette. Then type *PPCL* and click on the link 
	- Using Package Control ensures the PPCL Editor will stay up to date automatically
- **Manual**
	- Click "Clone or Download" and (for windows) unzip the package to the following, where *user* is your username
		-C:\Users\ *user* \AppData\Roaming\Sublime Text 3\Packages\PPCL
	- Otherwise drop the package wherever you have ST3 Packages
		- ***or***
	- In ST3, go to *Preferences* and *Browse Packages*
	- This will open the directory containing the packages for ST3, and you can drop the unzipped contents there

## Usage
- **Note that some of these functions keymappings are different for Linux and Mac machines**
- **Open Help Window**
	- Press <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>h</kbd> to show some relevant native ST3 keyboard shortcuts and all the shortcuts for PPCL
- **Comment Toggling**
	- To toggle comments, select any text and use <kbd>ctrl</kbd> + <kbd>/</kbd>, like the Native ST3 command
- **Line Number Increment**
	- To increment line numbers, make a seletion of text, and press <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>l</kbd>
		- This will increment only the selected text, but will update all GOs and other branch statements in the whole document
		- If no text is selected, the default change will be the whole document
	- This will prompt for the starting line number followed by the increment
		- in the form of *1000:10*
	- This will automatically update any GOTO and GOSUB references
- **Add Line Numbers**
	- Pressing <kbd>enter</kbd> will increment lines automatically
	- The default increment is 1
	- In the *Tools*->*PPCL*->*Line Increment* menu, there is a user-selection for the line increment amount
	- if adding a line would increment => the following line, it won't progress
- **Toggling DEFINE statements**
	- <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>d</kbd> for toggling the off the DEFINE statements (i.e. %X% becomes "AH1.HHW.")
	- <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>r</kbd> for toggling the on the DEFINE statements (i.e. "AH1.HHW." becomes %X%)
- **Toggling Periods/Underscores in Point Names**
	- <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>p</kbd> changes all underscores in point names to periods
	- <kbd>ctrl</kbd> + <kbd>alt</kbd> + <kbd>u</kbd> changes all periods in point names to underscores
- **Copy a chunk of Code**
	- <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>c</kbd> prompts you for how many times you want the selected code copied, and pastes in where you have selected.
- **other features, such as the big cursor, regex searching, etc. are natively supported by ST3**
- *a similar funtion as [InsertNums](https://packagecontrol.io/packages/Insert%20Nums) has been added to the package.  Other packages cna work in tandem with the PPCL editor*


##Native ST3 Commands / Properties
- **Set Syntax**
	- in menu toolbar select "View" -> "Syntax" -> "PPCL"
- **Toggle Case**
	- Select the text you want to toggle, and press
		- <kbd>ctrl</kbd> + <kbd>KU</kbd> for upper case
		- <kbd>ctrl</kbd> + <kbd>LU</kbd> for lower case
- **Big Cursor**
	- This can be done many ways
	- <kbd>ctrl</kbd> + <kbd>F</kbd>, and find all the text, select it all, and now move around with the arrow keys
	- Highlight some text, press <kbd>alt</kbd> + <kbd>F3</kbd> to select all
	- Using the scroll wheel of the mouse, hold it down and drag for constant row selecting
	- Hold Ctrl and click the mouse in whatever locations
- **Find Next**
	- Select some text, press <kbd>ctrl</kbd> + <kbd>D</kbd> to select the next occurence of that text
	- Keep hitting <kbd>ctrl</kbd> + <kbd>D</kbd> until all instances are selected
	- <kbd>ctrl</kbd> + <kbd>U</kbd> goes back one selection
- **Find**
	- Press <kbd>ctrl</kbd> + <kbd>F</kbd>
		- buttons on the left allow search specifics to be toggled
- **Delete line, shift up**
	- navigate to the line you want to delete, press <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>del</kbd>
- **Shift entire line**
	- Hold <kbd>ctrl</kbd> + <kbd>shift</kbd> and move with the arrows

### Block Commenting for PPCL
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620019/c507b5c4-2425-11e6-9e0f-a3697ecbd0c0.gif)


### Insert lines and auto renumber
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620027/cf0ff4fa-2425-11e6-9f33-26dbe2314918.gif)


### Bulk edit the code, adding points
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620036/db199fe4-2425-11e6-8818-cf13ffe7d25f.gif)


### Big cursor, natively part of Sublime Text
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620038/dfa1561a-2425-11e6-9572-2213421cac3d.gif)

![alt tag](https://cloud.githubusercontent.com/assets/10290469/15780076/ef8afc0a-296d-11e6-862a-abc5fa1eb382.png)
