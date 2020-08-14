import os
from pathlib import Path

import fire
import yaml
import sys
from jinja2 import Template, Environment, DebugUndefined
from jinja2.meta import find_undeclared_variables

from providers.base_provider import BaseProvider
from providers.vault import VaultProvider

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

    def __render_file(self, input_path: str, provider: BaseProvider):
        if os.path.isdir(input_path):
            return None

        with open(input_path, 'r') as src_file:
            source = src_file.read()
            ast = self.env.parse(source)
            undefined = find_undeclared_variables(ast)

            provider_instance = provider(self.config)
            variables = provider_instance.fetch_variables(undefined)

            return self.env.from_string(source).render(variables)

    def render(self, in_file, file_glob='*', vault=False, output_dir=None):
        provider = None

        if vault:
            provider = VaultProvider

        if provider is None:
            print('No providers specified', file=sys.stderr)
            return

        paths = [Path(in_file)] if os.path.isfile(in_file) else list(Path(in_file).rglob(file_glob))

        if output_dir is not None:
            Path(output_dir).mkdir(parents=True, exist_ok=True)

        for path in paths:
            result = self.__render_file(path.absolute(), provider)

            if output_dir is not None:
                new_path = Path(output_dir).joinpath(path)
                if path.is_dir():
                    new_path.mkdir(parents=True, exist_ok=True)
                else:
                    with open(new_path, 'w') as f:
                        f.write(result)
            else:
                print(result)

if __name__ == '__main__':
    fire.Fire(CliFunctions)
