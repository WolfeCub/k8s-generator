#!/usr/bin/env python
import os
import subprocess
from pathlib import Path

import fire
import yaml
import sys
import colored
from colored import stylize
from jinja2 import Template, Environment, FileSystemLoader, DebugUndefined, StrictUndefined
from jinja2.meta import find_undeclared_variables

from providers.base_provider import BaseProvider
from render import Render
from generate import Generate
from models.file_data import FileData
from models.cli_error_exception import CliErrorException

class CliFunctions(object):
    def __init__(self):
        self.__config = {}
        self.__env = Environment(undefined=DebugUndefined)
        self.__env.undefined = StrictUndefined

        file_name = os.path.join(
            os.environ['HOME'],
            '.plateconfig'
        )

        if not os.path.isfile(file_name):
            return

        with open(file_name) as f:
            self.__config = yaml.safe_load(f.read())

    def deploy(self):
        process = subprocess.Popen(['kubectl', 'apply', '--all', '--prune', '-f', '-'], stdout=subprocess.PIPE)

        output = process.stdout.readline()
        while process.poll() is None:
            if output != b'':
                self.__format_deployment_line(output.decode('utf8').strip())

            output = process.stdout.readline()

    def __format_deployment_line(self, line):
        color = None

        if line.endswith('created'):
            color = colored.fg('green')
        elif line.endswith('deleted') or line.endswith('pruned\n'):
            color = colored.fg('red')

        print(stylize(line, color) if color else line)
        

    def build(self, *in_files, file_glob='*', output_dir=None, template_dir=None):
        has_std_data = not sys.stdin.isatty()

        if not has_std_data and len(in_files) <= 0:
            raise CliErrorException('You must specify at least one file or stdinput')

        generate = Generate(self.__config, self.__env, in_files, file_glob, template_dir)

        return Render(self.__config, self.__env, generate.build_user_templates(), output_dir)


if __name__ == '__main__':
    try:
        fire.Fire(CliFunctions)
    except CliErrorException as e:
        print(e.message)
        sys.exit(1)
