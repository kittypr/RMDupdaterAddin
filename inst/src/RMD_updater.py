#!/usr/bin/env python3
import argparse
import subprocess

import check
import mdparse


def check_token():  # left for future functionality
    command = 'RMD_updater_create_token.py'
    subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def write_changes_file(changes_string, filename):
    filename += '.changes'
    with open(filename, 'w') as changes_file:
        changes_file.write(changes_string)


def write_tchanges_file(tchanges_string, filename):
    filename += '.tchanges'
    with open(filename, 'w') as tchanges_file:
        tchanges_file.write(tchanges_string)


def main(input_echo_md, gdoc_id, filename, fair, warnings=False):
    extractor = mdparse.MdExtractor(warnings)
    tables, text, plain_text = extractor.parse(input_echo_md)
    fair_extractor = mdparse.MdExtractor(False)
    fair_tables, null, fair_plain_text = fair_extractor.parse(fair)
    # creating html diff table.
    check.create_diff(plain_text, fair_plain_text, filename)

    # creating *.tchanges file
    _, changed = check.run_local_text_comparison(plain_text, fair_plain_text)
    tchanges_string = ''
    if len(changed) > 0:
        for change in changed:
            if text[change][2] != '':
                tchanges_string += '~~ CONTEXT\n' + text[change][1] + '\n~~ CHANGED BLOCK\n' + text[change][2] + \
                                   '\n~~ TEXT\n' + text[change][0] + '\n~~ END\n'
            else:
                tchanges_string += '~~ CONTEXT\n\n~~ CHANGED BLOCK\n\n~~ TEXT\n' + text[change][0] + '\n~~ END\n'
    write_tchanges_file(tchanges_string, filename)

    # creating *.changes file
    result = check.run_local_comparison(tables, fair_tables)
    changes_string = ''
    if len(result) == 0:
        write_changes_file(changes_string, filename)
        print('UP TO DATE')
    else:
        print('OUTDATED BLOCKS FOUNDED')

    for index in tables.keys():
        if index[1] in result:
            changes_string += '~~ CONTEXT\n' + index[0][0] + '\n~~ CHANGED BLOCK\n' + index[0][1] +\
                                          '\n~~ END\n'
    write_changes_file(changes_string, filename)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='RMDupdater checks tables from given MD doc and Gdoc, '
                                                 'finds differences, logs code that generates outdated information.')
    parser.add_argument('input', help='*.md file generated from *.rmd with "echo=TRUE"', action='store')
    parser.add_argument('gdoc_id', help='Gdoc id.', action='store')  # left for future functionality
    parser.add_argument('name', help='Name for unique *.changes filename', action='store')
    parser.add_argument('fair', help='Actual fair version from gdoc.', action='store')
    args = parser.parse_args()

    main(args.input, args.gdoc_id, args.name, args.fair, warnings=False)

