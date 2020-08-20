#!/usr/bin/env python
import os
import re
import subprocess
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
        self.__file_extension_pattern = re.compile('.plate(.yml|.yml)?$')

        file_name = os.path.join(
            os.environ['HOME'],
            '.plateconfig'
        )

        if not os.path.isfile(file_name):
            return

        with open(file_name) as f:
            self.__config = yaml.safe_load(f.read())

    def deploy(self):
        # ['kubectl', 'apply', '--all', '--prune', '-f', '-']
        process = subprocess.Popen(['kubectl'], stdout=subprocess.PIPE)

        output = process.stdout.readline()
        while process.poll() is None or output != b'':
            print(output)
            output = process.stdout.readline()

    def build(self, *in_files, file_glob='*', output_dir=None, template_dir=None):
        has_std_data = not sys.stdin.isatty()

        if not has_std_data and len(in_files) <= 0:
            raise CliErrorException('You must specify at least one file or stdinput')

        if template_dir is not None:
            self.__env.loader = FileSystemLoader(template_dir)

        file_data_list = []
        for path in in_files:
            models = self.__create_path_list_and_generate_file_data_models(path, file_glob, template_dir)
            file_data_list.extend(models)

        if has_std_data:
            user_input = sys.stdin.read()
            fd = FileData(user_input, self.__scan_str_for_variables(user_input), 'stdin')
            file_data_list.append(fd)

        return Render(self.__config, self.__env, file_data_list)


    def __process_user_template(self, file_content, file_path, template_dir):
        body = yaml.safe_load(file_content)

        if 'template' not in body:
            print(f'{file_path}: Missing template name', file=sys.stderr)
            return
        if 'vars' not in body:
            print(f'{file_path}: Missing variables', file=sys.stderr)
            return

        templates = body['template'] if isinstance(body['template'], list) else [body['template']]

        user_template_results = []
        for template in templates:
            if template_dir is None:
                target = Path(os.path.join(file_path.parent, template))
                self.__env.loader = FileSystemLoader(target.parent)

            user_template_results.append(self.__render_and_output_template(
                os.path.basename(template),
                body['vars']
            ))

        return '\n---\n'.join(user_template_results)

    def __render_and_output_template(self, template, variables):
        template = self.__env.get_template(template)
        return template.render(variables)


    def __create_path_list(self, in_file, file_glob):
        return [Path(in_file)] if os.path.isfile(in_file) else list(Path(in_file).rglob(file_glob))


    def __create_path_list_and_generate_file_data_models(self, in_file, file_glob, template_dir):
        paths = self.__create_path_list(in_file, file_glob)

        return list(self.__generate_file_data_models(paths, template_dir))

    def __scan_str_for_variables(self, source):
        ast = self.__env.parse(source)
        return find_undeclared_variables(ast)

    def __generate_file_data_models(self, paths, template_dir):
        for path in paths:
            if os.path.isdir(path):
                return

            with open(path, 'r') as src_file:
                source = src_file.read()

                if self.__file_extension_pattern.search(path.as_posix()):
                    source = self.__process_user_template(source, path, template_dir)

                undefined = self.__scan_str_for_variables(source)

                yield FileData(source, undefined, path.as_posix())


if __name__ == '__main__':
    try:
        fire.Fire(CliFunctions)
    except CliErrorException as e:
        print(e.message)
        sys.exit(1)
