# PPCL

This is a plugin and syntax for the PPCL langauge for Sublime Text 3.

It has a syntax highliting for .pcl files.

It also has a comment toggling plugin specific to PPCL 


##Usage
- **Comment Toggling**
	- To toggle comments, select any text and use (ctrl + /) like the Native ST3 command
- **Line Number Increment**
	- To increment all the line numbers use (ctrl + shift + l)
	- This will prompt for the starting line number followed by the increment
		- in the form of 1000:10
	- This will automatically update any GOTO and GOSUB references
	- There is a known bug for this where this will update on the second time commanding it
- **Add Line Numbers**
	- Pressing enter will increment lines automatically
	- The default increment is 10
	- if adding a line would increment => the following line, it won't progress


### Block Commenting for PPCL
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620019/c507b5c4-2425-11e6-9e0f-a3697ecbd0c0.gif)


### Insert lines and auto renumber
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620027/cf0ff4fa-2425-11e6-9f33-26dbe2314918.gif)


### Bulk edit the code, adding points
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620036/db199fe4-2425-11e6-8818-cf13ffe7d25f.gif)


### Big cursor, part of Sublime Text
![alt tag](https://cloud.githubusercontent.com/assets/10290469/15620038/dfa1561a-2425-11e6-9572-2213421cac3d.gif)
