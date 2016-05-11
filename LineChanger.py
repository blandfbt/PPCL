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
		GO_list = self.get_GOs(content)
		newcontent = self.replace_line_nums(content, GO_list)

		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, newcontent)


	def get_GOs(self, content):
		'''
		Get all the GOs numbers in the document,
		to make sure those are changed appropriately.
		'''
		GOs = []
		for i, line in enumerate(content.split('\n')):
			GO_nums = re.search(r'(GOTO )([0-9]+)', line)
			try:
				GOs.append(self.add_leading_zeroes(GO_nums.group(2)))
			except:
				pass
			GO_nums = re.search(r'(GOSUB )([0-9]+)', line)
			try:
				print (GO_nums.groups())
				GOs.append(self.add_leading_zeroes(GO_nums.group(2)))
			except:
				pass
		return GOs


	def get_GOSUB(self, content):
		'''
		Get all the GOSUB numbers in the document,
		to make sure those are changed appropriately.
		'''
		GOSUBs = []
		for i, line in enumerate(content.split('\n')):
			GOSUB_nums = re.search(r'(GOSUB )([0-9]+)', line)
			try:
				GOSUBs.append(self.add_leading_zeroes(GOSUB_nums.group(2)))
			except:
				pass
		return GOSUBs


	def replace_line_nums(self, content, GOs):
		'''
		Replace all the content with the new line numbers, and return the updated content
		and GOTO and GOSBU replacements.
		'''
		newcontent = ''
		GO_map = {}
		for i, line in enumerate(content.split('\n')):
			try:
				lineNum = self.add_leading_zeroes(re.search(r'(^[0-9]+)([\t])',
													line).group(1))
			except:
				pass
			lineNumReplace = self.add_leading_zeroes(int(self.start) +
													 (i * int(self.increment)))
			if lineNum == None:
				line = lineNumReplace + '\t' + line
			else:
				if lineNum in GOs:
					GO_map[lineNum] = self.add_leading_zeroes(lineNumReplace)
				if line.startswith('0'):
					line = line.replace(str(lineNum), str(lineNumReplace))
				else:
					line = line.replace(str(lineNum).lstrip('0'), str(lineNumReplace))
			
			if i < len(content.split('\n')) - 1:
				newcontent += line + '\n'
			else:
				newcontent += line

		for key in GO_map.keys():
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
		add the leading zeroes to match the PPCL syntax.
		'''
		try:
			linenum = linenum.lstrip('0')
		except:
			pass

		while len(str(linenum)) < 5:
			linenum = '0' + str(linenum)
			# print (linenum)
		return linenum


class InsertLinesCommand(sublime_plugin.TextCommand):
	'''
	This command willinsert a line below the current line, in an increment defaulted
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
		nextLine = self.view.substr(self.view.line(sublime.Region(self.view.text_point(row, 0))))

		try:
			lineNum = self.add_leading_zeroes(re.search(r'(^[0-9]+)([\t])',
												currentLine).group(1))
		except:
			lineNum = None

		try:
			nextLineNum = re.search(r'(^[0-9]+)([\t])', nextLine).group(1)
			nextLineNum = self.add_leading_zeroes(nextLineNum)
		except:
			# why did i make this a space?
			nextLineNum = ' '

		if lineNum is not None:
			newLineNum = self.add_leading_zeroes(int(lineNum) + increment)
		else:
			newLineNum = ''

		if (int(newLineNum) < int(nextLineNum)) or (self.view.rowcol(self.view.size())[0] == row-1):
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
		# find a boolean array of rows with and without comments.
		# if none are commented (all 0's), then comment them all
		# if some are commented, add comments to them all
		# if all are commented, remove one comment from them all
		commented_rows = self.determine_toggle()
		# this truefalse list is to switch the terms
		true_false = [False, True]
		if all(commented_rows[0] == item for item in commented_rows):
			switch_all = True

		for region in self.view.sel():
			# Get the selected text 
			if region.empty():
				rows = set()
				# this is the case where there is just a cursor on the screen,
				# nothing selected, but there could be multiple cursors
				for pos in self.view.sel():
					rows.add(int(self.view.rowcol(pos.begin())[0]))

				for i,row in enumerate(rows):
					line = self.view.substr(self.view.line(
							sublime.Region(self.view.text_point(row, 0))))
					print (commented_rows[i], true_false[commented_rows[i]])
					if switch_all==True:
						commentedLine = self.toggle_comment_new(line, True)
					else:
						commentedLine = self.toggle_comment_new(line, true_false[commented_rows[i]])
					self.view.replace(edit, self.view.line(
							sublime.Region(self.view.text_point(row, commented_rows[i]))), commentedLine)
				return

			else:
				# this is the case where there is a region selected
				# this might not have selected the total lines in the
				# selected rows, so we have to iterate over each row in the selectoin.
				row_begin = int(self.view.rowcol(self.view.sel()[0].begin())[0])
				row_end = int(self.view.rowcol(self.view.sel()[0].end())[0])
				# iterate over all the rows in the selection
				for row in range(row_begin, row_end+1):
					line = self.view.substr(self.view.line(
							sublime.Region(self.view.text_point(row, 0))))
					commentedLine = self.toggle_comment_new(line)
					self.view.replace(edit, self.view.line(
							sublime.Region(self.view.text_point(row, 0))), commentedLine)

	
	def determine_toggle(self):
		'''
		make a boolean list to determine which lines need to have
		comments added and/or removed.
		'''
		commented_rows = []
		row_begin = int(self.view.rowcol(self.view.sel()[0].begin())[0])
		row_end = int(self.view.rowcol(self.view.sel()[0].end())[0])
		# iterate over all the rows in the selection
		for row in range(row_begin, row_end+1):
			line = self.view.substr(self.view.line(
					sublime.Region(self.view.text_point(row, 0))))
			lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)?', line)
			if (lineNum.groups()[-1] == 'C') or (lineNum.groups()[-1] == 'C '):
				commented_rows.append(1)
			else:
				commented_rows.append(0)
		# print (commented_rows)
		return commented_rows


	def toggle_comment_new(self, line, toggle):
		'''
		given a line, toggle the comment C at the beginning of the line.
		toggle true/false will add/remove comments
		'''
		if toggle:
			try:
				lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)?', line)
				if (lineNum.groups()[-1] == 'C') or (lineNum.groups()[-1] == 'C '):
					tempstring = ''
					for group in lineNum.groups():
						tempstring += str(group)
					return (line.replace(tempstring, tempstring.replace('C ', '').replace('C', '')))
				else:
					return (line.replace(lineNum.groups()[0] + lineNum.groups()[1], lineNum.groups()[0] + lineNum.groups()[1] + 'C '))
			except:
				pass
		else:
			pass

	def toggle_comment(self, line, toggle):
		'''
		given a line, toggle the comment C at the beginning of the line.
		'''
		try:
			lineNum = re.search(r'(^[0-9]+)([\t]|[ ]+)(C ?)?', line)
			if (lineNum.groups()[-1] == 'C') or (lineNum.groups()[-1] == 'C '):
				tempstring = ''
				for group in lineNum.groups():
					tempstring += str(group)
				return (line.replace(tempstring, tempstring.replace('C ', '').replace('C', '')))
			else:
				return (line.replace(lineNum.groups()[0] + lineNum.groups()[1], lineNum.groups()[0] + lineNum.groups()[1] + 'C '))
		except:
			pass