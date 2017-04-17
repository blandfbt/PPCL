'''
// Copyright 2017 Brieb Blandford

// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

'''


import sublime, sublime_plugin
import re


class GetHelpCommand(sublime_plugin.WindowCommand):
	help_dict = [
		' ',
		'**Note that these key mappings are for Windows machines.**',
		'**Linux and Mac mappings are included, but might differ from this menu.**'
		' ',
		' ',
		'Native Commands',
		' ',
		'Ctrl + X                Cut line',
		'Ctrl + Enter            Insert line after',
		'Ctrl + Shift + Enter    Insert line before',
		'Ctrl + Shift + ↑        Move line/selection up',
		'Ctrl + Shift + ↓        Move line/selection down',
		'Ctrl + L                Select line - Repeat to select next lines',
		'Ctrl + D                Select word - Repeat select others occurrences',
		'Ctrl + M                Jump to closing parentheses Repeat to jump to opening parentheses',
		'Ctrl + Shift + M        Select all contents of the current parentheses',
		'Ctrl + Shift + K        Delete Line',
		'Ctrl + KK               Delete from cursor to end of line',
		'Ctrl + ]                Indent current line(s)',
		'Ctrl + [                Un-indent current line(s)',
		'Ctrl + Shift + D        Duplicate line(s)',
		'Ctrl + J                Join line below to the end of the current line',
		'Ctrl + /                Comment/un-comment current line',
		'Ctrl + Y                Redo, or repeat last keyboard shortcut command',
		'Ctrl + Shift + V        Paste and indent correctly',
		'Ctrl + Space            Select next auto-complete suggestion',
		'Ctrl + U                soft undo; jumps to your last change before undoing change when repeated',
		'Shift + Del             delete current line, shift up',
		'Ctrl + ;                Goto word in current file',
		'Ctrl + F                Find',
		'Ctrl + H                Replace',
		'Ctrl + PgUp             Cycle up through tabs',
		'Ctrl + PgDn             Cycle down through tabs',
		'Ctrl + F2               Toggle bookmark',
		'F2                      Next bookmark',
		'Shift + F2              Previous bookmark',
		'Ctrl + Shift + F2       Clear bookmarks',
		'Ctrl + KU               Transform to Uppercase',
		'Ctrl + KL               Transform to Lowercase',
		' ',
		'PPCL Commands',
		' ',
		'Enter                   Add line number',
		'Ctrl + Shift + H        Open Help',
		'Ctrl + Shift + L        Line number increment',
		'Ctrl + Alt + D        	 Toggle off all DEFINE statements',
		'Ctrl + Alt + R        	 Toggle on all DEFINE statements',
		'Ctrl + Alt + U          Toggle point names with periods to underscores',
		'Ctrl + Alt + P          Toggle point names with underscores to periods',
		'Ctrl + Shift + C          Make a copy of the selected code a user-defined number of times',
		'Ctrl + Alt + N          Enumerate the selected text <Start>:<Interval>.  If start starts with a leading 0, it will make numbers leading with 0s',
			]

	def run(self,):
		self.window.show_quick_panel(self.help_dict, self._on_select)

	def _on_select(self, idx):
		pass
		# if idx > -1:
		# 	selected = self.help_dict[idx]
		# 	sublime.message_dialog(selected)


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


class ToggleDefineCommand(sublime_plugin.TextCommand):
	'''
	This function will toggle the DEFINE statement strings.
	Toggle OFF will replace all the %..% with the assocaited string.
	Toggle OFF will replace all occurences of the assocaited string with %..%.
	'''

	def run(self, edit, toggle):
		# print ("made it =", kwargs)
		content = self.view.substr(sublime.Region(0, self.view.size()))
		define_map, define_lines = self.get_defines(content)

		if toggle == 'off':
			newcontent = self.toggle_off(content, define_map)

		if toggle == 'on':
			newcontent = self.toggle_on(content, define_map, define_lines)

		# replace the existing text with the modified text
		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, newcontent)


	def get_defines(self, content):
		'''
		regex search for all the define statements, return a dict of the associations
		with the %X% as the key, return the line numbers associated
		'''
		# compile for regex, find DEFINE lines, parameters, and associated strings
		p = re.compile(r'(^[0-9]+)([\t]|[ ]+)(DEFINE[\t ]?\()([A-Z0-9]*)(\, ?")(.*)"',
						re.MULTILINE)
		define_terms = re.findall(p, content)
		print (define_terms)
		define_map = {}
		define_lines = []
		for item in define_terms:
			define_map['%'+item[3]+'%'] = item[5]
			define_lines.append(item[0])

		return define_map, define_lines


	def toggle_on(self, content, define_map, define_lines):
		'''
		toggle on the define statements, meaning put the %X% back
		'''
		newcontent = ''
		p = re.compile(r'(^[0-9]+)([\t]|[ ]+)')
		first_line=True
		for line in content.split('\n'):
			nextline = line
			# index the regex search to get just the number
			try:
				# this means the line has a line number
				linenum = re.findall(p, line)[0][0]
			except:
				break

			if any(word in nextline for word in define_map.values()) and linenum not in define_lines:
				for key in define_map.keys():
					nextline = nextline.replace(define_map[key], key)
			# this is silly, but necessary
			# we don't wanna add a \n to the end of the last line
			if first_line:
				newcontent += nextline
			else:
				newcontent += '\n' + nextline
			first_line=False

		return newcontent


	def toggle_off(self, content, define_map):
		'''
		toggle off the define statements, meaning replace %X%
		'''
		# define_map = self.get_defines()
		print (define_map)
		for key in define_map.keys():
			content = content.replace(key, define_map[key])
		return content


class ToggleUnderscoresAndDotsCommand(sublime_plugin.TextCommand):
	'''
	This function toggles the "." and the "_" in point names
	for Desigo CC-friendly programming

	If there are both underscores and periods in point names,
	the default behavior will be to return all point names to 
	period separation.
	'''

	def run(self, edit, toggle):
		# print ("made it =", kwargs)
		content = self.view.substr(sublime.Region(0, self.view.size()))
		points = self.get_points(content)

		newcontent = self.toggle_points(content, points, toggle)
		# replace the existing text with the modified text
		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, newcontent)


	def get_points(self, content):
		'''
		Get all the point names in the document, return the set
		'''
		points = set()
		p = re.compile(r'\"[A-Za-z0-9][A-Z0-9 :_\./-]+\"')
		for line in content.split('\n'):
			PointsFoundInLine = re.findall(p, line)
			for point in PointsFoundInLine:
				print (point)
				# if not ('.AND.' in point) not ('.LT.' in point)
				points.add(point)
		return list(points)


	def toggle_points(self, content, points, toggle):
		'''
		Toggle from "_" to "." or vice versa
			ptou means periods to underscores
			utop means underscores to periods
		Return the new content
		'''
		newcontent = content
		if toggle == 'ptou':
			new_points = [point.replace('.', '_') for point in points]
		if toggle == 'utop':
			new_points = [point.replace('_', '.') for point in points]
		for point, newpoint in zip(points, new_points):
			newcontent = newcontent.replace(point, newpoint)
		return newcontent




class CallCopyCodeCommand(sublime_plugin.TextCommand):
	'''
	This class calls the user_input_window, gets the user response
	then calls the adjust_line_numbers command as an external command.
	'''
	def __init__(self, view):
		self.view = view
		self.number_of_copies = 1


	def run(self, edit):
		# get the start and end rows, even with multiple selections
		# beginning_row, ending_row = self.get_rows()
		# get the user input for the start and the increment
		self.edit = edit
		self.get_number_of_copies()
		

	def get_number_of_copies(self):
		inputView = sublime.Window.show_input_panel(sublime.active_window(),
			'<Number of Copies>', '{}'.format(self.number_of_copies),
			self.on_done, None, None)


	def on_done(self, text):
		'''
		this function just sets the number of copies selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		try:
			number_of_copies = text
			self.number_of_copies = int(number_of_copies)
		except:
			self.number_of_copies = None
		
		if self.number_of_copies != None:
			# self.main_functions()
			self.view.run_command("copy_code", {'number_of_copies': self.number_of_copies})



class CopyCodeCommand(sublime_plugin.TextCommand):
	'''
	This function copies the selected code the number of specified times.
	'''

	def run(self, edit, number_of_copies):
		# print ("made it =", kwargs)
		start_pos, end_pos = self.get_region()
		selected_content = self.view.substr(sublime.Region(start_pos, end_pos))
		# print ('selected\n\n\n\n\n', selected_content)
		new_content = self.make_new_content(selected_content, number_of_copies)
		# print ('new\n\n\n\n\n', new_content)

		total_content = self.view.substr(sublime.Region(0, self.view.size()))
		replacement_content = total_content.replace(selected_content, new_content)
		# print ('replacement\n\n\n\n\n', replacement_content)

		# replace the existing text with the modified text
		selections = sublime.Region(0, self.view.size())
		self.view.replace(edit, selections, replacement_content)


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


	def make_new_content(self, content_to_copy, number_of_copies):
		'''this returns the selection copied the number of times desired.'''
		new_content = ''
		for number in range(0, number_of_copies):
			new_content += content_to_copy + '\n'

		return new_content



class CallEnumerateCommand(sublime_plugin.TextCommand):
	'''
	This class calls the user_input_window, gets the user response
	then calls the Enumerate command as an external command.
	'''
	def __init__(self, view):
		self.view = view
		self.start = 1
		self.interval = 1


	def run(self, edit):
		# get the start and interval for enumerating the selection
		self.get_start_and_interval()
		

	def get_start_and_interval(self):
		inputView = sublime.Window.show_input_panel(sublime.active_window(),
			'<Start>:<Interval>', '{}:{}'.format(self.start, self.interval),
			self.on_done, None, None)


	def on_done(self, text):
		'''
		this function just sets the start and interval selected by the user
		because I couldnt figure out how to do that in the show_input_panel
		function.
		'''
		try:
			self.start, self.interval = text.split(':')
			# self.start = float(start)
			# self.interval = float(interval)
		except:
			self.start = None
			self.interval = None
		
		if self.start!= None and self.interval!=None:
			self.view.run_command("enumerate", {'start': self.start, 'interval': self.interval})



class EnumerateCommand(sublime_plugin.TextCommand):
	'''
	This function enumerates the selection from a starting point by an interval
	'''

	def run(self, edit, start, interval):
		# print (start, interval)
		# initial_nums_list = []

		if start.startswith('0'):
			leading_zero = True
		else:
			leading_zero = False

		start_num = self.is_int(start)
		interval_num = self.is_int(interval)
		i_not_f = type(start_num)==type(interval_num)
		if not i_not_f:
			round_amount = self.round_amount(start, interval)

		for i, region in enumerate(self.view.sel()):
			# nums_list.append(self.view.substr(region))
			if leading_zero:
				num = self.add_leading_zeroes(str(start_num + i * interval_num))
				if not i_not_f:
					num = str(round(float(num), round_amount))
				self.view.replace(edit, region, num)
			else:
				num = str(start_num + i * interval_num)
				if not i_not_f:
					num = str(round(float(num), round_amount))
				self.view.replace(edit, region, num)


	def round_amount(self, start, interval):
		try:
			round_amount_i = len(interval.split('.'))
		except AttributeError:
			round_amount_i = 0
		try:
			round_amount_s = len(start.split('.'))
		except AttributeError:
			round_amount_s = 0

		return max(round_amount_s, round_amount_i)


	def is_int(self, num):
		'''determine if the number (string) is an int or float, return num'''
		try:
			i = int(num)
		except ValueError:
			i = 1
		try:
			f = float(num)
		except ValueError:
			f = None

		if f == i:
			return i
		else:
			return f


	def add_leading_zeroes(self, num):
		'''
		add the leading zeroes to match the PPCL syntax of 5 characters.
		'''
		try:
			num = str(num).lstrip('0')
		except:
			pass

		while len(str(num)) < 5:
			num = '0' + str(num)
		return num
