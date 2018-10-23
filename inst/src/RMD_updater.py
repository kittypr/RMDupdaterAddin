#!/usr/bin/env python3
import argparse
import json
import subprocess

import check
import mdparse


def check_token():  # left for future functionality
    """Runs python script that creates token for google script API with documents scopes."""
    command = 'RMD_updater_create_token.py'
    subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def main(input_echo_md, gdoc_id, filename, fair, warnings=False):
    """Starts the comparing process.

    :param input_echo_md: string, path to .md document.
    :param gdoc_id: string, goodle document id.
    :param filename: string, unique prefix to all files.
    :param fair: string. path to downloaded clean copy of report.
    :param warnings: logical, indicates showing of additional information,
                     muted for correct protocol with RMDupdaterAddin.
    :return: -
    """
    extractor = mdparse.MdExtractor(warnings)
    tables, text = extractor.parse(input_echo_md)
    fair_extractor = mdparse.MdExtractor(False)
    fair_tables, fair_text = fair_extractor.parse(fair)
    # creating html diff table.
    check.create_diff(text, fair_text, filename)

    # creating text changes json object
    additions, changed = check.run_local_text_comparison(text, fair_text)
    text_json = {'text': list(),
                 'context': list(),
                 'ancestor': list()}
    if len(changed) > 0:
        for change in changed:
            text_json['text'].append(text[change][0])
            text_json['context'].append(text[change][1])
            text_json['ancestor'].append(text[change][2])

    # creating tables changes json object
    result = check.run_local_comparison(tables, fair_tables)
    if len(result) == 0 and len(changed) == 0 and not additions:
        print('ALL IS UP TO DATE')
    else:
        print('OUTDATED BLOCKS WERE FOUNDED')

    tables_json = {'context': list(),
                   'ancestor': list()}
    for different in result:
        tables_json['context'].append(different[0])
        tables_json['ancestor'].append(different[1])

    textj_filename = filename + '_text_changes.json'
    tablesj_filename = filename + '_tables_changes.json'
    with open(textj_filename, 'w') as text_file, open(tablesj_filename, 'w') as tables_file:
        json.dump(text_json, text_file)
        json.dump(tables_json, tables_file)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='RMDupdater checks tables from given MD doc and Gdoc, '
                                                 'finds differences, logs code that generates outdated information.')
    parser.add_argument('input', help='*.md file generated from *.rmd with "echo=TRUE".', action='store')
    parser.add_argument('gdoc_id', help='Gdoc id.', action='store')  # left for future functionality
    parser.add_argument('name', help='Name for unique json filenames.', action='store')
    parser.add_argument('fair', help='Actual fair version from gdoc.', action='store')
    args = parser.parse_args()

    main(args.input, args.gdoc_id, args.name, args.fair, warnings=False)