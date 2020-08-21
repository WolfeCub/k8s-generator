import os
import re
import sys
from pathlib import Path

import yaml
from jinja2 import FileSystemLoader
from jinja2.meta import find_undeclared_variables

from models.file_data import FileData


class Generate:
    def __init__(self, config, environment, input_files, file_glob, template_dir):
        self.env = environment
        self.input_files = input_files
        self.file_glob = file_glob
        self.template_dir = template_dir

        self.__file_extension_pattern = re.compile('.plate(.yml|.yml)?$')

        if template_dir is not None:
            self.__env.loader = FileSystemLoader(template_dir)


    def build_user_templates(self):
        has_std_data = not sys.stdin.isatty()

        file_data_list = []
        for path in self.input_files:
            models = self.__create_path_list_and_generate_file_data_models(path)
            file_data_list.extend(models)

        if has_std_data:
            user_input = sys.stdin.read()
            fd = FileData(user_input, self.__scan_str_for_variables(user_input), 'stdin')
            file_data_list.append(fd)

        return file_data_list

    def __process_user_template(self, file_content, file_path):
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
            if self.template_dir is None:
                target = Path(os.path.join(file_path.parent, template))
                self.env.loader = FileSystemLoader(target.parent)

            user_template_results.append(self.__render_and_output_template(
                os.path.basename(template),
                body['vars']
            ))

        return '\n---\n'.join(user_template_results)

    def __render_and_output_template(self, template, variables):
        template = self.env.get_template(template)
        return template.render(variables)

    def __create_path_list(self, in_file):
        return [Path(in_file)] if os.path.isfile(in_file) else list(Path(in_file).rglob(self.file_glob))

    def __create_path_list_and_generate_file_data_models(self, in_file):
        paths = self.__create_path_list(in_file)

        return list(self.__generate_file_data_models(paths))

    def __scan_str_for_variables(self, source):
        ast = self.env.parse(source)
        return find_undeclared_variables(ast)

    def __generate_file_data_models(self, paths):
        for path in paths:
            if os.path.isdir(path):
                return

            with open(path, 'r') as src_file:
                source = src_file.read()

                if self.__file_extension_pattern.search(path.as_posix()):
                    source = self.__process_user_template(source, path)

                undefined = self.__scan_str_for_variables(source)

                yield FileData(source, undefined, path.as_posix())
