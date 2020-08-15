import os
from pathlib import Path

import fire
import yaml
import sys
from jinja2 import Template, Environment, DebugUndefined
from jinja2.meta import find_undeclared_variables

from providers.base_provider import BaseProvider
from render import Render
from models.file_data import FileData

class CliFunctions(object):
    def __init__(self):
        self.config = {}
        self.env = Environment(undefined=DebugUndefined)

        file_name = os.path.join(
            os.environ['HOME'],
            '.k8sgenerator'
        )

        if not os.path.isfile(file_name):
            return

        with open(file_name) as f:
            self.config = yaml.safe_load(f.read())


    def render(self, in_file, file_glob='*', output_dir=None):
        paths = [Path(in_file)] if os.path.isfile(in_file) else list(Path(in_file).rglob(file_glob))

        file_data_list = list(self.__generate_file_data_models(paths))

        return Render(self.config, self.env, file_data_list)


    def __generate_file_data_models(self, paths):
        for path in paths:
            if not os.path.isdir(path):
                with open(path, 'r') as src_file:
                    source = src_file.read()
                    ast = self.env.parse(source)
                    undefined = find_undeclared_variables(ast)

                    yield FileData(source, undefined)


if __name__ == '__main__':
    fire.Fire(CliFunctions)
