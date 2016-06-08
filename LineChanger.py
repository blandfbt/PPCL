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
		newcontent = self.replace_line_nums(content, GOs_true, GOs_should)

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
						GOs_true.append(self.add_leading_zeroes(found[2]))
						GOs_should.append(self.add_leading_zeroes(found[2]))
					else:

						index = lineNums.index(
							min(lineNums, key=lambda y:abs(y-go_num))) + 1
						GOs_should.append(
							self.add_leading_zeroes(str(lineNums[index]))
										)
						GOs_true.append(self.add_leading_zeroes(found[2]))

				else:
					# This min statement finds the value in the lineNums list
					# just below the GO's number, then increments to the next index
					# in the list. 
					index = lineNums.index(
						min(lineNums, key=lambda y:abs(y-go_num))) + 1
					GOs_should.append(self.add_leading_zeroes(str(lineNums[index])))
					GOs_true.append(self.add_leading_zeroes(found[2]))
			except:
				pass
		print (GOs_true, GOs_should)		
		return (GOs_true, GOs_should)


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


	def replace_line_nums(self, content, GOs_true, GOs_should):
		'''
		Replace all the content with the new line numbers, and return the updated content
		and GOTO and GOSUB replacements.
		'''
		newcontent = ''
		GOs_should_new = []
		GO_map = {}

		for i, line in enumerate(content.split('\n')):

			try:
				lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)', line).group(1)
			except:
				pass
			lineNumReplace = self.add_leading_zeroes(int(self.start) +
													 (i * int(self.increment)))
			if lineNum == None:
				line = lineNumReplace + '\t' + line
			else:
				if self.add_leading_zeroes(lineNum) in GOs_should:
					index = GOs_should.index(self.add_leading_zeroes(lineNum))
					GO_map[GOs_true[index]] = self.add_leading_zeroes(lineNumReplace)
				if line.startswith('0'):
					line = line.replace(str(lineNum), str(lineNumReplace))
				else:
					line = line.replace(str(lineNum).lstrip('0'), str(lineNumReplace))
			
			if i < len(content.split('\n')) - 1:
				newcontent += line + '\n'
			else:
				newcontent += line

			GO_num = re.search(r'(GO(TO|SUB) )([0-9]+)', line)
			try:
				newcontent = newcontent.replace('GOTO ' + str(GO_num.group(3)),
					'GOTO ' + self.add_leading_zeroes(GO_num.group(3)))
			except:
				pass

		for key in GO_map.keys():
			# print (key, ':', GO_map[key])
			newcontent = newcontent.replace('GOTO ' + str(key),
											 'GOTO ' + str(GO_map[key]))
			newcontent = newcontent.replace('GOTO ' + str(key).lstrip('0'),
											 'GOTO ' + str(GO_map[key]))
			newcontent = newcontent.replace('GOSUB ' + str(key),
											 'GOSUB ' + str(GO_map[key]))
			newcontent = newcontent.replace('GOSUB ' + str(key).lstrip('0'),
											 'GOSUB ' + str(GO_map[key]))
		return newcontent


	def on_done(self, user_input):
		'''
		this function just sets the start and increment selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		sublime.status_message("Brien Rules!")
		self.start, self.increment = user_input.split(':')


	def on_change(self, user_input):
		'''
		this function just sets the start and increment selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		sublime.status_message("Brien Rules!")
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