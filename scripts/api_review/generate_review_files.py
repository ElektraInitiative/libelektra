#!/usr/bin/env python3

import re
import argparse
import sys
import os

def print_error(message):
    print(message)
    sys.exit(1)

def is_file(path):
    if os.path.isfile(path):
        return path
    
    print_error('File {} does not exist!'.format(path))

def is_folder(path):
    if os.path.isdir(path):
        return path
    
    print_error('Folder {} does not exist!'.format(path))

parser = argparse.ArgumentParser(
    '''
    Generate review files from a markdown template
    '''
)

parser.add_argument(
    '-o',
    '--output-folder',
    help='Specify the output folder for the generated review files',
    type=is_folder,
    default='generated'
)

parser.add_argument(
    '-t',
    '--template-path',
    help='Specify the path for the template',
    type=is_file,
    default='template.c.md'
)

parser.add_argument(
    '-m',
    '--methods-path',
    help='Specify the path for the method list',
    type=is_file,
    default='methods.c.txt'
)

args = parser.parse_args()

def strip_method_name(method):
    method = method.strip().rstrip(';')
    method = re.sub('\s+', ' ', method)

    return method


def get_file_name(method, output_folder=args.output_folder):
    return "{}/{}.md".format(output_folder, method)


def get_method_name(method):
    method = method.split('(')[0]
    method = method.split()[-1].strip().lstrip('*')

    return method


with open(args.methods_path, 'r') as methods_file:
    methods = [strip_method_name(method) for method in methods_file]

with open(args.template_path) as template_file:
    template = template_file.read()

for method in methods:
    method_name = get_method_name(method)

    with open(get_file_name(method_name), 'w') as output_file:
        output_file.write(
            template.format(signature = method, name = method_name)
        )
