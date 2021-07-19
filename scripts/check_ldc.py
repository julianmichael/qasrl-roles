#!/bin/python3
# Requires Python 3

from collections import namedtuple
import os.path
import urllib.request
import sys
import time
import tarfile

Artifact = namedtuple('Artifact', ['name', 'tarpath', 'dirpath', 'url', 'description'])


artifacts = [
    Artifact(
        name = 'Penn Treebank 2',
        tarpath = 'data/LDC95T7.tgz',
        dirpath = 'data/treebank2',
        url = 'https://catalog.ldc.upenn.edu/LDC95T7',
        description = "Required for reconstructing PropBank argument spans."
    ),
    Artifact(
        name = 'PropBank 1',
        tarpath = 'data/LDC2004T14.tgz',
        dirpath = 'data/propbank_1',
        url = 'https://catalog.ldc.upenn.edu/LDC2004T14',
        description = "Used to retrieve the original full PropBank argument spans.",
    ),
    Artifact(
        name = 'CoNLL 2008 Shared Task Data',
        tarpath = 'data/LDC2009T12.tgz',
        dirpath = 'data/conll08st',
        url = 'https://catalog.ldc.upenn.edu/LDC2009T12',
        description = "Evalaution dataset for semantic role induction."
    ),
]

a = ord('a')

def untar_item(artifact):
    print("Untarring {}.".format(artifact.name))
    tarpath = artifact.tarpath
    result = tarfile.open(tarpath)
    result.extractall(os.path.dirname(tarpath))
    result.close()
    # os.remove(tarpath)
    print("\nExtracted to {}".format(artifact.dirpath))


def get_artifact_option_prompt(num, artifact):
    if os.path.exists(artifact.dirpath) or os.path.exists(artifact.tarpath):
        if not os.path.exists(artifact.dirpath):
            untar_item(artifact)
        color = "\u001b[32m"
        icon  = "[extracted at {}]".format(artifact.dirpath)
        opt_done = True
    else:
        color = "\u001b[33m"
        icon  = "[please download to {}]".format(artifact.tarpath)
        opt_done = False

    letter = chr(num + a)

    desc = ("\n" + artifact.url + "\n" + artifact.description).replace("\n", "\n     ")

    return (u"  {}{} {}\u001b[0m ".format(color, artifact.name, icon) + desc + "\n"), opt_done


def construct_prompt():
    prompt = "Required LDC artifacts:"
    all_done = True
    for i, artifact in enumerate(artifacts):
        opt_prompt, opt_done = get_artifact_option_prompt(i, artifact)
        prompt += "\n" + opt_prompt
        all_done = all_done and opt_done
    if all_done:
        prompt += "\nAll items downloaded. Press enter to exit."
    else:
        prompt += "\nPress enter when you're done to refresh and untar the downloaded items (q to exit): "
    return prompt, all_done

done = False
while not done:
    prompt, done = construct_prompt()
    print(prompt, end='')
    optstr = "".join([chr(i) for i in range(a, a + len(artifacts))])
    response = input()
    if len(response) > 0 and "quit".startswith(response.lower()):
       done = True
