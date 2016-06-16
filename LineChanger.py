'''
    LineChanger.py is a PPCL plugin for the Sublime Text 3 text editor.
    Copyright (C) 2016  Brien Blandford (Smith Engineering, PLLC)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''



import sublime, sublime_plugin
import re


class AdjustLineNumsCommand(sublime_plugin.TextCommand):
	'''
	This command is to readjust all the line numbers in the PPCL document.  It
	gives a user promt
	'''

	def __init__(self, view):
		self.view = view
		self.increment = 100
		self.start = 1000

	def run(self, edit, increment=100):
		# get the start number and the increment, defined by the user
		self.get_line_start_and_increment()
		# get the entire content of the code
		content = self.view.substr(sublime.Region(0, self.view.size()))
		# get the list of any GOTO statements
		# the shoulds are where they should point
		# the trues are where they actually point
		GOs_true, GOs_should = self.get_GOs(content)
		# get the list of any ONPWRT statements
		ONs_true, ONs_should = self.get_ONPWRT(content)
		newcontent = self.replace_line_nums(content, 
					GOs_true, GOs_should, ONs_true, ONs_should)

		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, newcontent)


	def get_GOs(self, content):
		'''
		Get all the GOs numbers in the document,
		to make sure those are changed appropriately.
		Returns two lists, where the first is the actual line number
		in the GO, and the second is where it should truly point.
		'''
		GOs_true = []
		GOs_should = []
		lineNums = self.get_LineNumbers(content)
		for i, line in enumerate(content.split('\n')):
			GO_nums = re.findall(r'(GO(TO|SUB) )([0-9]+)', line)
			try:
				for found in GO_nums:
					go_num = int(found[2])
					if go_num in lineNums:
						# the case where the GO references an existing linenum
						GOs_true.append(int(found[2]))
						GOs_should.append(int(found[2]))
					else:
						# the case where the GO doesn't reference an exisitng linenum
						index = lineNums.index(
							min(lineNums, key=lambda y:abs(y-go_num))) + 1
						GOs_should.append(lineNums[index])
						GOs_true.append(int(found[2]))
			except:
				pass
		return (GOs_true, GOs_should)


	def get_ONPWRT(self, content):
		'''
		Similar to the get_GOs method, this looks for ONPWRT(#####) to
		ensure its line numbers are appropriately changed.
		'''
		ONs_true = []
		ONs_should = []
		lineNums = self.get_LineNumbers(content)
		for i, line in enumerate(content.split('\n')):
			ON_nums = re.findall(r'(ONPWRT\()([0-9]+)(\))', line)
			try:
				for found in ON_nums:
					ON_num = int(found[1])
					if ON_num in lineNums:
						ONs_true.append(int(found[1]))
						ONs_should.append(int(found[1]))
					else:

						index = lineNums.index(
							min(lineNums, key=lambda y:abs(y-ON_num))) + 1
						ONs_should.append(lineNums[index])
						ONs_true.append(int(found[1]))
			except:
				pass
		return (ONs_true, ONs_should)


	def get_LineNumbers(self, content):
		'''
		get all the line numbers in the current document, convert to ints
		and return them as a list.
		'''
		lineNums = []
		for i, line in enumerate(content.split('\n')):
			num = re.search(r'(^[0-9]+)([\t]|[ ]+)', line)
			try:
				lineNums.append(int(num.group(1)))
			except:
				pass
		return lineNums



	def replace_line_nums(self, content, GOs_true, GOs_should, ONs_true, ONs_should):
		'''
		Replace all the content with the new line numbers, and return the updated content
		and GOTO and GOSUB replacements.
		Also replace all ONPWRT statements in the same way.
		There's probably a cleaner way to write this...
		'''

		# the new content is a string of all the content of code
		# start with it empty, and we are going to append to it
		newcontent = ''
		# GOs_should_new is a list to store all the lineNums the GOs should reference
		GOs_should_new = []
		# Go_map is a dictionary holding the current text's GO nums as the keys
		# for the lineNums they will end up refering to.
		GO_map = {}
		# ONs is the same, but for ONPWRT
		ONs_should_new = []
		ON_map = {}

		for i, line in enumerate(content.split('\n')):
			# try to find the lineNums in the start of each line of code
			try:
				lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)', line).group(1)
			except:
				pass
			# the lineNumReplace is the new line number, based on the start and increment
			lineNumReplace = self.add_leading_zeroes(int(self.start) +
													 (i * int(self.increment)))
			
			# the case where there is no text / line number
			# this could be a missing line number, or the start of a new document
			if lineNum == None:
				line = lineNumReplace + '\t' + line
			else:
				# check if line is a number associated with a GO, build a GO dict
				if int(lineNum) in GOs_should:
					index = GOs_should.index(int(lineNum))
					GO_map[GOs_true[index]] = int(lineNumReplace)
				# check if line is a number associated with a ONPWRT, build a ON dict
				if int(lineNum) in ONs_should:
					index = ONs_should.index(int(lineNum))
					ON_map[ONs_true[index]] = int(lineNumReplace)
				# proceed with the rest of the line
				if line.startswith('0'):
					line = line.replace(str(lineNum), str(lineNumReplace))
				else:
					line = line.replace(str(lineNum).lstrip('0'), str(lineNumReplace))
			# add the line to the newcontent, build it out
			if i < len(content.split('\n')) - 1:
				newcontent += line + '\n'
			else:
				newcontent += line

			# find and replace all the GOs/ONs with the int equivalent
			GO_num = re.findall(r'(GO(TO|SUB) )([0-9]+)', line)
			ON_num = re.findall(r'(ONPWRT\()([0-9]+)(\))', line)
			try:
				for number in GO_num:
					newcontent = newcontent.replace('GOTO ' + str(number[2]),
						'GOTO ' + str(int(number[2])))
			except:
				pass

			try:
				for number in ON_num:
					newcontent = newcontent.replace('ONPWRT(' + str(number[1]),
						'ONPWRT(' + str(int(number[2])))
			except:
				pass

		# this gets messy
		# for each line, search for all GOs and ONs
		# when they are found, we have to replace them with the
		# appropriate *new* line number reference
		# however, and blind string.reaplce() will replace
		# the 1000 in 10000 with its reference (say 8000),
		# making the new reference 80000 instead of 8000
		for line in content.split('\n'):
			GO_num = re.findall(r'(GO(TO|SUB) )([0-9]+)', line)
			ON_num = re.findall(r'(ONPWRT\()([0-9]+)(\))', line)
			for number in GO_num:
				if int(number[2]) in GO_map.keys():
					# match the GO reference only if it is the same integer
					# as the GO_map key
					newcontent = newcontent.replace('GOTO ' + number[2],
											 'GOTO ' + self.add_leading_zeroes(
											 	str(GO_map[int(number[2])])))
					newcontent = newcontent.replace('GOSUB ' + number[2],
											 'GOSUB ' + self.add_leading_zeroes(
											 	str(GO_map[int(number[2])])))
			for number in ON_num:
				if int(number[2]) in ON_map.keys():
					newcontent = newcontent.replace('ONPWRT(' + number[2],
									'ONPWRT(' + self.add_leading_zeroes(
										str(ON_map[int(number[2])])))
		
		return newcontent


	def on_done(self, user_input):
		'''
		this function just sets the start and increment selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		sublime.status_message("brien.blandford@gmail.com")
		self.start, self.increment = user_input.split(':')


	def on_change(self, user_input):
		'''
		this function just sets the start and increment selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		sublime.status_message("brien.blandford@gmail.com")
		self.start, self.increment = user_input.split(':')


	def get_line_start_and_increment(self):
		inputView = sublime.Window.show_input_panel(sublime.active_window(),
			'<Line Start>:<increment>', '1000:100', self.on_done, None, None)


	def add_leading_zeroes(self, linenum):
		'''
		add the leading zeroes to match the PPCL syntax of 5 characters.
		'''
		try:
			linenum = str(linenum).lstrip('0')
		except:
			pass

		while len(str(linenum)) < 5:
			linenum = '0' + str(linenum)
		return linenum


class InsertLinesCommand(sublime_plugin.TextCommand):
	'''
	This command will insert a line below the current line, in an increment defaulted
	to 10. I'm not sure yet if I want to spend the time to have it take into
	consideration the count if it ends up being the same as the line below it.
	'''
	def run(self, edit):
		increment = 10
		currentLine = self.view.substr(self.view.line(self.view.sel()[0]))
		rowandcol = self.view.rowcol(self.view.sel()[0].begin())
		if (int(rowandcol[0]) == 0) and (currentLine == ''):
			self.view.insert(edit, self.view.line(self.view.sel()[0]).end(),'01000\t')

		(row, col) = (int(rowandcol[0]) + 1, int(rowandcol[1])+1)
		nextLine = self.view.substr(self.view.line(
					sublime.Region(self.view.text_point(row, 0))))

		try:
			lineNum = self.add_leading_zeroes(re.search(r'(^[0-9]+)([\t]|[ ]+)',
												currentLine).group(1))
		except:
			lineNum = None

		try:
			nextLineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)', nextLine).group(1)
			nextLineNum = self.add_leading_zeroes(nextLineNum)
		except:
			# why did i make this a space?
			nextLineNum = ' '

		if lineNum is not None:
			newLineNum = self.add_leading_zeroes(int(lineNum) + increment)
		else:
			newLineNum = ''

		if ((int(newLineNum) < int(nextLineNum)) or
				(self.view.rowcol(self.view.size())[0] == row-1)):
			self.view.insert(edit, self.view.line(self.view.sel()[0]).end(),
							'\n'+str(newLineNum)+'\t')


	def add_leading_zeroes(self, linenum):
		'''
		add the leading zeroes to match the PPCL syntax.
		'''
		try:
			linenum = linenum.lstrip('0')
		except:
			pass
			
		while len(str(linenum)) < 5:
			linenum = '0' + str(linenum)
		return linenum


class InsertCommentCommand(sublime_plugin.TextCommand):
	'''
	This command will insert a comment (01000 tab C ) when ctrl+/ is pressed over all
	the selected lines. 
	'''
	def run(self, edit):
		rows = set()
		counter = 1

		# determine all the rows in the selection
		for region in self.view.sel():
			lines_tuple = self.view.split_by_newlines(region)
			for region in lines_tuple:
				rows.add(int(self.view.rowcol(region.begin())[0]))

		commented_rows = self.determine_toggle(rows)
		if all(commented_rows[0] == item for item in commented_rows):
			switch_all = True
		else:
			switch_all = False

		for row in rows:
			if (((switch_all==True) and (commented_rows[0]==False))
				or switch_all==False):
				line = self.view.substr(self.view.line(
						sublime.Region(self.view.text_point(row, 0))))
				commentedLine = self.toggle_on(line)
				self.view.replace(edit, self.view.line(
					sublime.Region(self.view.text_point(row, 0))),
									commentedLine)
			elif (switch_all==True) and (commented_rows[0]==True):
				line = self.view.substr(self.view.line(
						sublime.Region(self.view.text_point(row, 0))))
				commentedLine = self.toggle_off(line)
				self.view.replace(edit, self.view.line(
						sublime.Region(self.view.text_point(row, 0))),
										commentedLine)


	def determine_toggle(self, rows):
		'''
		make a boolean list to determine which lines need to have
		comments added and/or removed.
		'''
		commented_rows = []
		for row in rows:
			line = self.view.substr(self.view.line(
					sublime.Region(self.view.text_point(row, 0))))
			lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)?', line)
			if (lineNum.groups()[-1] == 'C') or (lineNum.groups()[-1] == 'C '):
				commented_rows.append(1)
			else:
				commented_rows.append(0)
		return commented_rows


	def toggle_on(self,line):
		'''
		add a comment to the beginning.
		'''
		try:
			lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)?', line)
			return (line.replace(lineNum.groups()[0] + lineNum.groups()[1],
						lineNum.groups()[0] + lineNum.groups()[1] + 'C '))
		except:
			pass


	def toggle_off(self,line):
		'''
		remove a comment from the beginning.
		'''
		try:
			lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)', line)
			tempstring = ''
			for group in lineNum.groups():
				tempstring += str(group)
			return (line.replace(tempstring, 
					tempstring.replace('C ', '').replace('C', '')))
		except:
			pass