import json
import subprocess

from copy import copy

TABLE = ('Table', )
CODE = ('Code', 'CodeBlock')
BLOCK = ('Div', 'Header')
IGNORED = ('Span', )


class MdExtractor:

    def __init__(self, warnings):
        self.tables = dict()
        self.text = list()
        self.plain_text = list()
        self.context = ''
        self.ancestor = ''
        self.content = ''
        self.warnings = warnings

    def get_content(self):
        content = copy(self.content)
        self.content = ''
        return content

    def add_content(self, addition):
        self.content = self.content + addition

    def save_ancestor(self, ancestor):
        self.context = copy(self.ancestor)
        self.ancestor = ancestor

    def save_text(self):
        # write collected text info
        content = self.get_content()
        if content != '':
            self.text.append((content, self.context, self.ancestor))
            self.plain_text.append(content)

    def write_code(self, code):
        """Saves code block that creates other elements in case ones were changed.
        Since, element with title 'Code' or 'CodeBlock' has special structure of 'c'(Content) field, that looks like:
        [[0], 'code']
        where:
            [0] - list of attributes: identifier, classes, key-value pairs.
            'code' - string with code.
        we should parse it especially.
        Args:
            code - element with title 'Code' or 'CodeBlock'.
        """

        self.save_text()
        self.save_ancestor(code['c'][1])

    def write_special_block(self, block, cell_content):
        if not cell_content:
            self.save_text()
        con = 1
        if block['t'] == 'Header':
            con = 2
        self.list_parse(block['c'][con])
        if not cell_content:
            self.save_text()

    def write_ignored(self, span):
        self.list_parse(span['c'][1])

    def write_table(self, tab):
        """Extracts table and saves them with code block they were made from.
        Table in pandoc's json has following structure:
        dict: { 't': 'Table'
                'c': [ [0] [1] [2] [3] [4] ]
              }
        Where:
        [0] - caption.
        [1] - is list of aligns by columns, looks like: [ { t: 'AlignDefault' }, ... ].
        [2] - widths of columns.
        [3] - is list of table's headers (top cell of every column), can be empty.
        [4] - list of rows, and row is list of cells.
        Since every cell's structure is the same as text's one, we just parse them as list and write one by one.
        Args:
            tab - dictionary with 't': 'Table".
        """
        self.save_text()

        table = list()
        row = list()
        headers = tab['c'][3]
        if headers:
            has_content = False
            for col in headers:
                self.list_parse(col, cell_content=True)
                cell_content = self.get_content()
                row.append(cell_content)
                if cell_content != '':
                    has_content = True
            if has_content:
                row = tuple(row)
                table.append(row)
        t_content = tab['c'][4]
        for line in t_content:
            row = list()
            for col in line:
                self.list_parse(col, cell_content=True)
                cell_content = self.get_content()
                row.append(cell_content)
            row = tuple(row)
            table.append(row)
        table = tuple(table)
        self.tables[((self.context, self.ancestor), len(self.tables))] = table

    def dict_parse(self, dictionary, cell_content=False):
        """Parse dictionaries.
        Dictionary represents some json-object. The kind of json object depends on its 't' (title) field.
        We will parse it differently depending on different titles.

        Args:
            dictionary - object with 't' and sometimes 'c' fields.
            cell_content - indicates either we inside or outside of table cell
        """
        try:
            if dictionary['t'] in TABLE and not cell_content:  # blocks that may have content
                self.write_table(dictionary)
            elif dictionary['t'] in BLOCK:
                self.write_special_block(dictionary, cell_content)
            elif dictionary['t'] in IGNORED:
                self.write_ignored(dictionary)
            elif dictionary['t'] in CODE and not cell_content:  # parse it only if it is outside table
                self.write_code(dictionary)
            elif 'c' in dictionary:
                if type(dictionary['c']) == str:
                    self.add_content(dictionary['c'])
                if type(dictionary['c']) == list:
                    self.list_parse(dictionary['c'], cell_content)
            else:  # blocks without content
                if dictionary['t'] == 'Space':
                    self.add_content(' ')
                elif dictionary['t'] == 'SoftBreak':
                    self.add_content(' ')
                elif dictionary['t'] == 'LineBreak':
                    self.add_content('\n')
            if dictionary['t'] == 'Para' or dictionary['t'] == 'Plain':
                if cell_content:
                    self.add_content('\n')
                else:
                    self.save_text()
        except KeyError:
            if self.warnings:
                print('Untypical block. Some information might be lost.')

    def list_parse(self, content_list, cell_content=False):
        """Parse list.

        Args:
            content_list - list with different parts of content from input-document.
            cell_content - indicates either we inside or outside of table cell
        """
        for item in content_list:
            if type(item) == dict:
                self.dict_parse(item, cell_content)
            elif type(item) == list:
                self.list_parse(item, cell_content)
            else:
                if self.warnings:
                    print('Untypical block. Some information might be lost.')

    def document_parse(self, document):
        """Main function.
        Gets JSON object from Pandoc, parses it and extracts tables.

        Args:
            document - json object as python dictionary or list.
                  In case of dictionary it has representation like:
                  { 'pandoc-version': ...
                    'meta': ...
                    'blocks': .......}
                  in blocks we have all file-content, we will parse doc['blocks'].
                  In case of list it has representation like:
                  [[info_list], [content_list]], so we will parse doc[1].
        """
        if type(document) == dict:
            self.list_parse(document['blocks'])
        elif type(document) == list:
            self.list_parse(document[1])
        else:
            print('Incompatible Pandoc version. Process failed.')

    def parse(self, source):
        command = 'pandoc ' + source + ' -t json'
        proc = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        res = proc.communicate()
        if res[1]:
            print('PROCESS FAILED. SEE BELOW:')
            print(str(res[1]))
            return None  # sending stderr output to user
        else:
            document = json.loads(res[0])
            self.document_parse(document)
            return self.tables, self.text, self.plain_text
