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
		# get the user input for the start and the increment
		self.edit = edit
		self.get_line_start_and_increment()
		

	def get_line_start_and_increment(self):
		inputView = sublime.Window.show_input_panel(sublime.active_window(),
			'<Line Start>:<increment>', '{}:{}'.format(self.adjust_line_start, self.adjust_line_increment),
			None, self.on_done, None) # Having an action triggered on both "done" and "change" unnecessarily double executes


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
			self.view.run_command("adjust_some_line_nums", 
									{'adjust_line_increment': self.adjust_line_increment,
									'adjust_line_start': self.adjust_line_start})


class AdjustSomeLineNumsCommand(sublime_plugin.TextCommand):
	'''
	adjust the line numbers in the selection according to the user input
	'''
	def run(self, edit, adjust_line_increment, adjust_line_start):
		'''
		break out the main functions to be called from the 
		get_line_start_and_increment() method, trying to see if this
		works to allow the user input to update on queue
		'''
		self.adjust_line_increment = adjust_line_increment
		self.adjust_line_start = adjust_line_start

		start_pos, end_pos, lineCount = self.get_region()

		# check whether requested renumbering values will exceed PPCL max
		if (lineCount * adjust_line_increment + adjust_line_start - adjust_line_increment) < 100000:
			selected_content = self.view.substr(sublime.Region(start_pos, end_pos))
			full_content = self.view.substr(sublime.Region(0, self.view.size()))

			# all GOs/ONs that point to renumbered lines should also be renumbered  
			# to avoid breaking PPCL code, whether in the selection or not.
			GOs_true, GOs_should, GOs_skipped = self.get_GOs(full_content)
			ONs_true, ONs_should, ONs_skipped = self.get_ONPWRT(full_content)
			newcontent = self.replace_line_nums(selected_content, full_content,
										GOs_true, GOs_should, GOs_skipped, 
										ONs_true, ONs_should, ONs_skipped)
			selections = sublime.Region(start_pos, end_pos)
			select_all = sublime.Region(0, self.view.size())
			self.view.replace(edit, select_all, newcontent)
		else:
			# popup warning if requested renumbering parameters will exceed max
			self.view.show_popup('<h3>Renumbering Error!</h3> \
				<p>Renumbering Start:Increment puts lines out of range. \
				The maximum allowed line number is <strong>99999</strong>.</p>',
				sublime.HIDE_ON_MOUSE_MOVE_AWAY,
				max_width=1000,
				max_height=500)


	def get_region(self):
		'''
		return the beginning and ending row numbers of the selection.
		'''
		start_pos = None
		end_pos = 0
		for region in self.view.sel():
			if region.empty() is True: # if nothing is selected, renumber entire file
				region = sublime.Region(0, self.view.size())
			selectedLines = self.view.lines(region)
			lineCount = len(selectedLines)
			if start_pos is None:
				start_pos = selectedLines[0].begin()
			else:
				start_pos = min(start_pos, selectedLines[0].begin())
			end_pos = max(end_pos, selectedLines[-1].end())	
		return start_pos, end_pos, lineCount


	def get_GOs(self, content):
		'''
		Get all the GOs numbers in the document,
		to make sure those are changed appropriately.
		Returns two lists, where the first is the actual line number
		in the GO, and the second is where it should truly point.
		'''
		GOs_true = []
		GOs_should = []
		GOs_skipped = []
		lineNums = self.get_LineNumbers(content)
		for i, line in enumerate(content.split('\n')):
			GO_nums = re.findall(r'(?:GO(?:TO|SUB) )([0-9]+)', line) # Changed regex so only the number is captured
			try:
				for found in GO_nums:
					go_num = int(found)
					if go_num in lineNums:
						# the case where the GO references an existing linenum
						GOs_true.append(int(found))
						GOs_should.append(int(found))
						GOs_skipped.append('')
					else:
						# the case where the GO doesn't reference an exisitng linenum
							# This could be extremely problematic. Probably better to 
							# note original number and *suggest* closest line number
						index = lineNums.index(
							min(lineNums, key=lambda y:abs(y-go_num))) + 1
						GOs_should.append(int(lineNums[index]))
						GOs_true.append(int(found))
						GOs_skipped.append('')
			except:
				GOs_should.append('')
				GOs_true.append('')
				GOs_skipped.append(int(found))
				pass
		return (GOs_true, GOs_should, GOs_skipped)


	def get_ONPWRT(self, content):
		'''
		Similar to the get_GOs method, this looks for ONPWRT(#####) to
		ensure its line numbers are appropriately changed.
		'''
		ONs_true = []
		ONs_should = []
		ONs_skipped = []
		lineNums = self.get_LineNumbers(content)
		for i, line in enumerate(content.split('\n')):
			ON_nums = re.findall(r'(?:ONPWRT\()([0-9]+)(?:\))', line) # Changed regex so only the number is captured
			try:
				for found in ON_nums:
					ON_num = int(found)
					if ON_num in lineNums:
						ONs_true.append(int(found))
						ONs_should.append(int(found))
						ONs_skipped.append('')
					else:

						index = lineNums.index(
							min(lineNums, key=lambda y:abs(y-ON_num))) + 1
						ONs_should.append(int(lineNums[index]))
						ONs_true.append(int(found))
						ONs_skipped.append('')
			except:
				ONs_should.append('')
				ONs_true.append('')
				ONs_skipped.append(int(found))				
				pass
		return (ONs_true, ONs_should, ONs_skipped)


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


	def replace_line_nums(self, selected_content, full_content, 
						GOs_true, GOs_should, GOs_skipped, 
						ONs_true, ONs_should, ONs_skipped):
		'''
		Replace all the content with the new line numbers, and return the updated content
		and GOTO and GOSUB replacements.
		Also replace all ONPWRT statements in the same way.
		There's probably a cleaner way to write this...
		'''

		# the newcontent is a string of all the content of code
		# start with it empty, and we are going to append to it
		original_content = ''
		renumbered_lines = ''
		# Go_true_map is a dictionary holding the current text's GO nums as the keys
		# for the lineNums they will end up refering to.
		GO_true_map = {}
		GO_suggested_map = {}
		# ONs is the same, but for ONPWRT
		ON_true_map = {}
		ON_suggested_map = {}
		lineNum = None

		for i, line in enumerate(selected_content.split('\n')):
			# record original contents of lines being modified. this will be 
			# used as the content to search for to replace with the renumbered lines 
			if i < len(selected_content.split('\n')) - 1:
				original_content += line + '\n'
			else:
				original_content += line

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
				# check if line is a number associated with a GO anywhere in the 
				# program, build a GO dict
				if int(lineNum) in GOs_true:
					index = int(lineNum)
					GO_true_map[index] = int(lineNumReplace)
				if int(lineNum) in GOs_should: # Changed elif -> if because a suggested number may point to the same line as a true number, but it would be skipped using elif
					index = GOs_should.index(int(lineNum))
					GO_suggested_map[GOs_true[index]] = int(lineNumReplace)				
				# check if line is a number associated with a ONPWRT anywhere in the 
				# program, build a ON dict
				if int(lineNum) in ONs_true:
					index = int(lineNum)
					ON_true_map[index] = int(lineNumReplace)
				if int(lineNum) in ONs_should: # Changed elif -> if because a suggested number may point to the same line as a true number, but it would be skipped using elif
					index = ONs_should.index(int(lineNum))
					ON_suggested_map[ONs_true[index]] = int(lineNumReplace)						
				# proceed with the rest of the line
				if line.startswith('0'):
					line = line.replace(str(lineNum), str(lineNumReplace))
				else:
					line = line.replace(str(lineNum).lstrip('0'), str(lineNumReplace)) # isn't the '.lstrip('0')' implicit in the else of the condition 'if line.startswith('0'):'?
			# add the line to the newcontent, build it out
			if i < len(selected_content.split('\n')) - 1:
				renumbered_lines += line + '\n'
			else:
				renumbered_lines += line
		# replace the original lines in the full pgm with renumbered lines.  
		newcontent = full_content.replace(original_content, renumbered_lines)
		newcontent = self.replace_gos_ons(newcontent, GO_true_map, GO_suggested_map, GOs_skipped, ON_true_map, ON_suggested_map, ONs_skipped)
		return newcontent


	def replace_gos_ons(self, newcontent, GO_true_map, GO_suggested_map, GOs_skipped, ON_true_map, ON_suggested_map, ONs_skipped):
		'''
		Replace GOTO, GOSUB, and ONPWRT targets with updated line numbers.
		'''

		GO_num = [] 
		ON_num = []
		for line in newcontent.split('\n'):
			for match in re.findall(r'(?:GO(?:TO|SUB) )([0-9]+)', line): # Changed regex so only the number is captured
				GO_num.append(int(match)) 
			for match in re.findall(r'(?:ONPWRT\()([0-9]+)(?:\))', line): # Changed regex so only the number is captured
				ON_num.append(int(match))

		# this gets messy
		# for each line, search for all GOs and ONs
		# when they are found, we have to replace them with the
		# appropriate *new* line number reference
		# however, and blind string.reaplce() will replace
		# the 1000 in 10000 with its reference (say 8000),
		# making the new reference 80000 instead of 8000
			# By "uniquefying" the lists of GOs/ONs before iterating through them, 
			# multiple replacements aren't made for the same value. So, repeated calls
			# to the same subroutine only result in one list entry. -NPW

		GO_num = list(set(GO_num))
		ON_num = list(set(ON_num))		
		for number in GO_num:
			if number in GO_true_map.keys():
				# match the GO reference only if it is the same integer as the GO_true_map key
				newcontent = newcontent.replace('GOTO ' + str(number),
										 'GOTO ' + str(GO_true_map[number]))
				newcontent = newcontent.replace('GOSUB ' + str(number),
										 'GOSUB ' + str(GO_true_map[number]))
			elif number in GO_suggested_map.keys():
				newcontent = newcontent.replace('GOTO ' + str(number),
										 'GOTO ' + str(GO_suggested_map[number]) + '[suggested number]')
				newcontent = newcontent.replace('GOSUB ' + str(number),
										 'GOSUB ' + str(GO_suggested_map[number]) + '[suggested number]')
			elif number in GOs_skipped:
				# moving the line number inside the square brackets ensures it isn't later matched and changed if later renumbering coincides with it.
				newcontent = newcontent.replace('GOTO ' + str(number),
										 'GOTO ' + '[' + str(number) + ' Not Found]')
				newcontent = newcontent.replace('GOSUB ' + str(number),
										 'GOSUB ' + '[' + str(number) + ' Not Found]')

		for number in ON_num:
			if number in ON_true_map.keys():
				newcontent = newcontent.replace('ONPWRT(' + str(number),
								'ONPWRT(' + str(ON_true_map[number]))
			elif number in ON_suggested_map.keys():
				newcontent = newcontent.replace('ONPWRT(' + str(number) + ')',
								'ONPWRT(' + str(ON_suggested_map[number]) + ')' + '[suggested number]')
			elif number in ONs_skipped:
				# moving the line number inside the square brackets ensures it isn't later matched and changed if later renumbering coincides with it.
				newcontent = newcontent.replace('ONPWRT(' + str(number) + ')',
								'ONPWRT(' + '[' + str(number) + ' Not Found]' + ')')												
		return newcontent

	def add_leading_zeroes(self, linenum):
		'''
		add the leading zeros to match the PPCL syntax of 5 characters.
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
			self.view.show_popup('<h3>Auto-increment conflict!</h3> \
				<p>Renumber lines or change line increment in <em><strong>Tools>PPCL>Line \
				Increment Amount</strong></em>.</p>',
				sublime.HIDE_ON_MOUSE_MOVE_AWAY,
				max_width=1000,
				max_height=500)

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