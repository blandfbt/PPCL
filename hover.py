'''
// Copyright 2017 Brieb Blandford

// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

'''


import sublime
import sublime_plugin
import json 
import os
import re


class EnableHelpCommand(sublime_plugin.WindowCommand):

    """Enables/Disables help popup setting"""

    def run(self, setting):
        s = sublime.load_settings('ppcl.sublime-settings')
        s.set(setting, not s.get(setting, False))
        sublime.save_settings('ppcl.sublime-settings')

    def is_checked(self, setting):
        s = sublime.load_settings('ppcl.sublime-settings')
        return s.get(setting, False)


class HoverOverCommand(sublime_plugin.EventListener):

    def on_hover(self, view, point, hover_zone):
        s = sublime.load_settings('ppcl.sublime-settings')
        word = s.get('enable_help_popup')
        if not word:
            return

        if hover_zone == sublime.HOVER_TEXT:
            syntax = view.scope_name(point).split(' ')[0]
            if syntax == 'source.PPCL':
                region = sublime.Region(point)
                word = self.get_user_selection(view, region)
                # find reserved words surrounded by dots (e.g. .AND. .OR. .ROOT. .GT. .EQ.).
                dot_word = re.search(r"\.(.+?)\.", word)
                if dot_word:
                    word = dot_word.group(1)
                # find reserved words and strip trailing digits (e.g. SECND7, LOC2)
                # this works but will also match incorrect syntax (e.g. SECND8, LOC16) 
                # num_word = re.search(r"([a-zA-Z]+)", word)
                # if num_word:
                #     word = num_word.group(1)
                print (word)
                helps = self.read_json(word)
                if helps == 'Not Defined':
                    return
                self.create_popup(view, word, helps, region.begin())


    def get_user_selection(self, view, region):
        '''
        from the selection or the mouse pointer, return the whole word.
        '''
        if region.begin() == region.end():
            word = view.word(region)
        else:
            word = region
        if not word.empty():
            keyword = view.substr(word)
        return keyword


    def read_json(self, func, filename='help.json'):
        '''
        read in the json file, return the appropriate function.
        '''
        dir_path = os.path.dirname(os.path.realpath(__file__))
        jsonfile = os.path.join(dir_path, filename)
        try:
            with open(jsonfile, 'r', encoding='utf-8') as helpjson:
                data = json.loads(helpjson.read())
            try:
                return data[func]
            except KeyError:
                return 'Not Defined'
        except (OSError, IOError) as e:
            print ('Error reading JSON')


    def create_popup(self, view, word, helps, location):
        '''
        make the pop up from helps, with appropriate html
        '''

        if view==None or word==None:
            return

        popup_html = '''
        <h2>Help for {0}</h2>
            <h3>
            <u style="color:#538b01">Format</u>
            </h3><p style="font-family: Courier New">{1}</p>
        <h3 >
            <u style="color:#538b01">Description</u>
        </h3><p style="word-wrap: break-word; width: 450px; padding: 25px">{2}</p>
        <h3 >
            <u style="color:#538b01">Example</u>
        </h3><p style="white-space: pre-line; width: 450px; padding: 25px; font-family: Courier New">{3}</p>
        '''.format(
            word,
            helps['function'],
            helps['description'],
            helps['example'],
            )


        view.show_popup(
                        popup_html,
                        flags=sublime.HIDE_ON_MOUSE_MOVE_AWAY,
                        location=location, 
                        max_width=800,
                        max_height = 500
                        )


