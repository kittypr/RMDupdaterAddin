#!/usr/bin/env python3

import os

from oauth2client import file as oauth_file, client, tools

SCOPES = ['https://www.googleapis.com/auth/documents']


def create():
    """Checks token existence, creates if it does not exist.

    :return: -
    """
    store = oauth_file.Storage('token.json')
    creds = store.get()
    path_to_script = os.path.dirname(os.path.abspath(__file__))
    client_secret_file = os.path.join(path_to_script, 'RMD_updater_credentials.json')
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets(client_secret_file, SCOPES)
        tools.run_flow(flow, store)


if __name__ == '__main__':
    create()
