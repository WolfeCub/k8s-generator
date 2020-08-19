#!/usr/bin/env python
import os
from pathlib import Path

import fire
import yaml
import sys
from jinja2 import Template, Environment, FileSystemLoader, DebugUndefined, StrictUndefined
from jinja2.meta import find_undeclared_variables

from providers.base_provider import BaseProvider
from render import Render
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


    def render(self, *in_files, file_glob='*', output_dir=None):
        has_std_data = not sys.stdin.isatty()

        if not has_std_data and len(in_files) <= 0:
            raise CliErrorException('You must specify at least one file or stdinput')

        file_data_list = []
        for path in in_files:
            file_data_list.extend(self.__create_path_list_and_generate_file_data_models(path, file_glob))

        if has_std_data:
            user_input = sys.stdin.read()
            fd = FileData(user_input, self.__scan_str_for_variables(user_input), 'stdin')
            file_data_list.append(fd)

        return Render(self.__config, self.__env, file_data_list)


    def generate(self, in_file, template_dir, file_glob='*'):
        file_list = self.__create_path_list(in_file, file_glob)
        self.__env.loader = FileSystemLoader(template_dir)

        for file_path in file_list:
            with open(file_path) as f:
                body = yaml.safe_load(f)

                if 'template' not in body:
                    print('Missing template name', file=sys.stderr)
                    return
                if 'vars' not in body:
                    print('Missing variables', file=sys.stderr)
                    return

                templates = body['template'] if isinstance(body['template'], list) else [body['template']]
                for template in templates:
                    self.__render_and_output_template(template, body['vars'])


    def __render_and_output_template(self, template, variables):
        template = self.__env.get_template(template)
        rendered = template.render(variables)
        print(rendered)
        print('---')


    def __create_path_list(self, in_file, file_glob):
        return [Path(in_file)] if os.path.isfile(in_file) else list(Path(in_file).rglob(file_glob))


    def __create_path_list_and_generate_file_data_models(self, in_file, file_glob):
        paths = self.__create_path_list(in_file, file_glob)

        return list(self.__generate_file_data_models(paths))

    def __scan_str_for_variables(self, source):
        ast = self.__env.parse(source)
        return find_undeclared_variables(ast)

    def __generate_file_data_models(self, paths):
        for path in paths:
            if not os.path.isdir(path):
                with open(path, 'r') as src_file:
                    source = src_file.read()
                    undefined = self.__scan_str_for_variables(source)

                    yield FileData(source, undefined, path.as_posix())


if __name__ == '__main__':
    try:
        fire.Fire(CliFunctions)
    except CliErrorException as e:
        print(e.message)
        sys.exit(1)
