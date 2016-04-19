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
		GOTO_list = self.get_GOTO(content)
		newcontent = self.replace_line_nums(content, GOTO_list)

		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, newcontent)


	def get_GOTO(self, content):
		'''
		Get all the GOTO numbers in the document,
		to make sure those are changed appropriately.
		'''
		GOTOs = []
		for i, line in enumerate(content.split('\n')):
			GOTO_nums = re.search(r'(GOTO )([0-9]+)', line)
			try:
				GOTOs.append(self.add_leading_zeroes(GOTO_nums.group(2)))
			except:
				pass
		return GOTOs


	def replace_line_nums(self, content, GOTOs):
		'''
		Replace all the content with the new line numbers, and return the updated content
		and GOTO replacements.
		'''
		newcontent = ''
		GOTO_map = {}
		for i, line in enumerate(content.split('\n')):
			try:
				lineNum = self.add_leading_zeroes(re.search(r'(^[0-9]+)([ ]+)',
													line).group(1))
			except:
				pass
			lineNumReplace = self.add_leading_zeroes(int(self.start) +
													 (i * int(self.increment)))
			if lineNum == None:
				line = lineNumReplace + '  ' + line
			else:
				if lineNum in GOTOs:
					GOTO_map[lineNum] = self.add_leading_zeroes(lineNumReplace)
				if line.startswith('0'):
					line = line.replace(str(lineNum), str(lineNumReplace))
				else:
					line = line.replace(str(lineNum).lstrip('0'), str(lineNumReplace))
			
			if i < len(content.split('\n')) - 1:
				newcontent += line + '\n'
			else:
				newcontent += line

		for key in GOTO_map.keys():
			newcontent = newcontent.replace('GOTO ' + str(key),
											 'GOTO ' + str(GOTO_map[key]))
			newcontent = newcontent.replace('GOTO ' + str(key).lstrip('0'),
											 'GOTO ' + str(GOTO_map[key]))
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
		add the leading zeroes to match the PPCL syntax.
		'''
		try:
			linenum = linenum.lstrip('0')
		except:
			pass

		while len(str(linenum)) < 5:
			linenum = '0' + str(linenum)
			print (linenum)
		return linenum


class InsertLinesCommand(sublime_plugin.TextCommand):
	'''
	This command insert a line below the current line, in an increment defaulted to 10.
	I'm not sure yet if I want to spend the time to have it take into consideration the
	count if it ends up being the same as the line below it.
	'''
	def run(self, edit):
		increment = 10
		currentLine = self.view.substr(self.view.line(self.view.sel()[0]))
		rowandcol = self.view.rowcol(self.view.sel()[0].begin())
		if (int(rowandcol[0]) == 0) and (currentLine == ''):
			self.view.insert(edit, self.view.line(self.view.sel()[0]).end(),'01000  ')

		(row, col) = (int(rowandcol[0]) + 1, int(rowandcol[1])+1)
		nextLine = self.view.substr(self.view.line(sublime.Region(self.view.text_point(row, 0))))

		try:
			lineNum = self.add_leading_zeroes(re.search(r'(^[0-9]+)([ ]+)',
												currentLine).group(1))
		except:
			lineNum = None

		try:
			nextLineNum = re.search(r'(^[0-9]+)([ ]+)', nextLine).group(1)
			nextLineNum = self.add_leading_zeroes(nextLineNum)
		except:
			nextLineNum = ' '

		if lineNum is not None:
			newLineNum = self.add_leading_zeroes(int(lineNum) + increment)
		else:
			newLineNum = ''

		# print (newLineNum, nextLineNum)
		# print (self.view.rowcol(self.view.size())[0], row)
		if (int(newLineNum) < int(nextLineNum)) or (self.view.rowcol(self.view.size())[0] == row-1):
			self.view.insert(edit, self.view.line(self.view.sel()[0]).end(),
							'\n'+newLineNum+'  ')


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
			print (linenum)
		return linenum
