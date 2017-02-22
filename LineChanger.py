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
from operator import itemgetter

enter_line_increment = 1


class SetIncrementCommand(sublime_plugin.WindowCommand):
	def run(self, increment):
		global enter_line_increment
		enter_line_increment = increment


class LoadPpclFileCommand(sublime_plugin.TextCommand):
	'''
	This class calls the user_input_window, gets the user response
	then calls the adjust_line_numbers command as an external command.
	'''
	def run(self, edit):
		newcontent = ''
		full_content = self.view.substr(sublime.Region(0, self.view.size()))
		line_regex = re.compile(r'(^[0-9]+)([\t]|[ ]+)', re.MULTILINE)
		full_content = re.sub(line_regex, lambda x: self.add_leading_zeroes(x.group(1),
														ending='\t'), full_content)

		GO_regex = re.compile(r'(GO(TO|SUB) )([0-9]+)', re.MULTILINE)
		full_content = re.sub(GO_regex, lambda x: self.add_leading_zeroes(x.group(3),
														beginning=x.group(1), ending='\t'),
														full_content)

		ON_regex = re.compile(r'(ONPWRT\()([0-9]+)(\))', re.MULTILINE)
		full_content = re.sub(ON_regex, lambda x: self.add_leading_zeroes(x.group(2),
														beginning='ONPWRT(', ending=')'),
														full_content)
		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, full_content)


	def add_leading_zeroes(self, linenum, ending='', beginning=''):
		'''
		add the leading zeroes to match the PPCL syntax of 5 characters.
		'''
		try:
			linenum = str(linenum).lstrip('0')
			while len(str(linenum)) < 5:
				linenum = '0' + str(linenum)
			return beginning + linenum + ending
		except:
			pass


class CallAdjustCommand(sublime_plugin.TextCommand):
	'''
	This class calls the user_input_window, gets the user response
	then calls the adjust_line_numbers command as an external command.
	'''
	def __init__(self, view):
		self.view = view
		self.adjust_line_increment = None
		self.adjust_line_start = None
		self.selections = None
		self.newcontent = None


	def run(self, edit):
		# get the start and end rows, even with multiple selections
		# beginning_row, ending_row = self.get_rows()
		# get the user input for the start and the increment
		self.edit = edit
		self.get_line_start_and_increment()
		

	def get_line_start_and_increment(self):
		inputView = sublime.Window.show_input_panel(sublime.active_window(),
			'<Line Start>:<increment>', '{}:{}'.format(self.adjust_line_start, self.adjust_line_increment),
			self.on_done, self.on_done, None)


	def on_done(self, text):
		'''
		this function just sets the start and increment selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		try:
			adjust_line_start, adjust_line_increment = text.split(':')
			self.adjust_line_increment = int(adjust_line_increment)
			self.adjust_line_start = int(adjust_line_start)
		except:
			self.adjust_line_increment = None
			self.adjust_line_start = None
		
		if self.adjust_line_increment != None and self.adjust_line_start != None:
			# self.main_functions()
			self.view.run_command("adjust_some_line_nums", 
									{'adjust_line_increment': self.adjust_line_increment,
									'adjust_line_start': self.adjust_line_start})


class AdjustSomeLineNumsCommand(sublime_plugin.TextCommand):
	'''
	adjust the line numbers in the selection according to the user input
	'''
	def run(self, edit, adjust_line_increment,adjust_line_start):
		'''
		break out the main functions to be called from the 
		get_line_start_and_increment() method, trying to see if this
		works to allow the user input to update on queue
		'''
		self.adjust_line_increment = adjust_line_increment
		self.adjust_line_start = adjust_line_start

		start_pos, end_pos = self.get_region()
		content = self.view.substr(sublime.Region(start_pos, end_pos))
		full_content = self.view.substr(sublime.Region(0, self.view.size()))

		# for renumbering gotos in the whole document, use the full_content
		# argument. to renumber only those in the current selection, use content

		GOs_true, GOs_should = self.get_GOs(content)
		ONs_true, ONs_should = self.get_ONPWRT(content)
		newcontent = self.replace_line_nums(content, 
						GOs_true, GOs_should, ONs_true, ONs_should)
		selections = sublime.Region(start_pos, end_pos)
		self.view.replace(edit, selections, newcontent)


	def get_region(self):
		'''
		return the beginning and ending row numbers of the selection.
		'''
		start_pos = None
		end_pos = 0
		for region in self.view.sel():
			selectedLines = self.view.lines(region)
			if start_pos is None:
				start_pos = selectedLines[0].begin()
			else:
				start_pos = min(start_pos, selectedLines[0].begin())
			end_pos = max(end_pos, selectedLines[-1].end())	
		return start_pos, end_pos

	def get_rows(self):
		'''
		return the beginning and ending row numbers of the selection.
		'''
		beginning_row = None
		ending_row = 0
		rows = set()
		selected_region = None
		for area in self.view.sel():
			lines_tuple = self.view.split_by_newlines(area)
			for region in lines_tuple:
				current_row = int(self.view.rowcol(region.begin())[0])
				if current_row is not None:
					if beginning_row is None:
						beginning_row = current_row
					elif current_row < beginning_row:
						beginning_row = current_row
					if current_row > ending_row:
						ending_row = current_row
		return beginning_row, ending_row


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
						GOs_should.append(int(lineNums[index]))
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
						ONs_should.append(int(lineNums[index]))
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
		lineNum = None

		for i, line in enumerate(content.split('\n')):
			# try to find the lineNums in the start of each line of code
			try:
				lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)', line).group(1)
			except:
				pass
			# the lineNumReplace is the new line number, based on the start and increment
			lineNumReplace = self.add_leading_zeroes(int(self.adjust_line_start) +
													 (i * int(self.adjust_line_increment)))
			
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
						'GOTO ' + self.add_leading_zeroes(str(int(number[2]))))
			except:
				pass

			try:
				for number in ON_num:
					newcontent = newcontent.replace('ONPWRT(' + str(number[1]),
						'ONPWRT(' + self.add_leading_zeroes(str(int(number[2]))))
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
	to 1. I'm not sure yet if I want to spend the time to have it take into
	consideration the count if it ends up being the same as the line below it.
	'''
	def run(self, edit):
		# increment = 1
		global enter_line_increment
		increment = enter_line_increment

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
		else:
			# popup warning if auto-increment fails
			self.view.show_popup('Auto-increment conflict! Renumber lines or \
				change line increment in "Tools>PPCL>Line Increment Amount".',
				sublime.HIDE_ON_MOUSE_MOVE_AWAY)

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