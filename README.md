# PPCL

This is a plugin and syntax for the PPCL langauge for Sublime Text 3.
It has a syntax highliting for .pcl files
It also has features specific to writing PPCL 

##Installing
- Ensure [Sublime Text 3](https://www.sublimetext.com/3) is installed
	- You should really consider getting a license to support the awesomeness of ST3
- **via Package Control** *coming soon (hopefully) and preferred*
	- Download [Package Control](https://packagecontrol.io/) and use the *Package Control: Install Package* command from the command palette. Using Package Control ensures the PPCL Editor will stay up to date automatically
- **Manual**
	- Click "Clone or Download" and  (for windows) unzip the package to the following, where user is your username
		-C:\Users\ *user* \AppData\Roaming\Sublime Text 3\Packages\PPCL
	- Otherwise drop the package wherever you have ST3 Packages
			-***or***
	- In ST3, go to *Preferences* and *Browse Packages*
	- This will open the directory containing the packages for ST3, and you can drop the unzipped contents there

##Usage
- **Comment Toggling**
	- To toggle comments, select any text and use *ctrl + /*, like the Native ST3 command
- **Line Number Increment**
	- To increment all the line numbers use *ctrl + shift + l*
	- This will prompt for the starting line number followed by the increment
		- in the form of *<1000:10>*
	- This will automatically update any GOTO and GOSUB references
	- There is a known bug for this where this will update on the second time commanding it
- **Add Line Numbers**
	- Pressing enter will increment lines automatically
	- The default increment is 10
	- if adding a line would increment => the following line, it won't progress
- **other features, such as the big cursor, regex searching, etc. are natively supported by ST3**
- **incrementing numbers inside the code (not the line numbers) can be done with another extension** [InsertNums](https://packagecontrol.io/packages/Insert%20Nums)

### Block Commenting for PPCL
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620019/c507b5c4-2425-11e6-9e0f-a3697ecbd0c0.gif)


### Insert lines and auto renumber
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620027/cf0ff4fa-2425-11e6-9f33-26dbe2314918.gif)


### Bulk edit the code, adding points
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620036/db199fe4-2425-11e6-8818-cf13ffe7d25f.gif)


### Big cursor, natively part of Sublime Text
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620038/dfa1561a-2425-11e6-9572-2213421cac3d.gif)
