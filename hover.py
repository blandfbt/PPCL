import sublime
import sublime_plugin
import json 
import os



help_on = True


class EnableHelpCommand(sublime_plugin.WindowCommand):
    def run(self, enable_help):
        global help_on
        help_on = enable_help


class HoverOverCommand(sublime_plugin.EventListener):
    def on_hover(self, view, point, hover_zone):
        print (help_on)
        if not help_on:
            return

        if hover_zone == sublime.HOVER_TEXT:
            syntax = view.scope_name(point).split(' ')[0]
            if syntax == 'source.PPCL':
                region = sublime.Region(point)
                word = self.get_user_selection(view, region)
                print (word)
                helps = self.read_json(word)
                # print (helps)
                # print ('function: {0}\ndescription: {1}\nexample: {2}'.format(
                #                                             helps['function'],
                #                                             helps['description'],
                #                                             helps['example'],))
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
        jsonfile = os.path.join(sublime.packages_path(), 'PPCL', filename)
        try:
            with open(jsonfile, 'r') as helpjson:
                data = json.loads(helpjson.read())
            try:
                return data[func]
            except KeyError:
                print ('Key not in map')
        except (OSError, IOError) as e:
            print ('Error reading JSON')



    def create_popup(self, view, word, helps, location):
        '''
        make the pop up from helps, with appropriate html
        '''
        # {0} = word
        # 

        if view==None or word==None:
            return

        try:
            width = 3*max(255, max([len(item) for key, item in helps.items()]))
        except AttributeError:
            return

        

        popup_html = '''<h2>Help for {0}</h2>
        <p><h3><u>Format</u></h3> {1}</p>
        <h3><u>Description</u></h3> {2}
        <h3><u>Example</u></h3> {3}
        '''.format(
            word,
            helps['function'],
            helps['description'],
            helps['example'],
            )


        print (width)
        view.show_popup(
                        popup_html,
                        flags=sublime.HIDE_ON_MOUSE_MOVE_AWAY,
                        location=location, 
                        max_width=width
                        )


