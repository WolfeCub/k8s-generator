import os
import sys
from pathlib import Path

from jinja2.exceptions import UndefinedError
from providers.vault import VaultProvider
from providers.yaml import YamlProvider
from models.file_data import FileData
from utils import recursive_dict_update, first_valid_or_default

class Render:
    def __init__(self, config, environment, file_data_list):
        self.__config = config
        self.__env = environment
        self.__file_data_list = file_data_list
        self.__vars_fetched_so_far = {}

        self.__provider = None


    def __str__(self):
        self.__render_all_paths()
        sys.exit(0)


    def __render_all_paths(self, output_dir=None):
        if output_dir is not None:
            Path(output_dir).mkdir(parents=True, exist_ok=True)

        has_encountered_error = False

        for file_data in self.__file_data_list:
            try:
                template = self.__env.from_string(file_data.content)
                result = template.render(self.__vars_fetched_so_far)
            except UndefinedError as e:
                print(e.message, file=sys.stderr)
                has_encountered_error = True

            if has_encountered_error:
                continue

            if output_dir is None:
                print(result)
                print('---')
                continue

            new_path = Path(output_dir).joinpath(file_data.path)
            if os.path.isdir(file_data.path):
                new_path.mkdir(parents=True, exist_ok=True)
                continue

            with open(new_path, 'w') as f:
                f.write(result)


    def __fetch_vars_from_provider(self) -> dict:
        variables_to_fetch = set()

        for file_data in self.__file_data_list:
            variables_to_fetch = variables_to_fetch.union(file_data.variables)

        return self.__provider.fetch_variables(variables_to_fetch)


    def __fetch_and_update_vars_with_provider(self, provider):
        self.__provider = provider
        var_dict = self.__fetch_vars_from_provider()

        self.__vars_fetched_so_far = recursive_dict_update(self.__vars_fetched_so_far, var_dict)

        return self


    def vault(self, url=None, mount=None, verify=True):
        config_section = self.__config['vault'] if 'vault' in self.__config else {}
        url = first_valid_or_default(url, config_section.get('url'))
        mount = first_valid_or_default(mount, config_section.get('mount_point'))
        verify = first_valid_or_default(config_section.get('verify'), verify)

        provider = VaultProvider(url, mount, verify)
        return self.__fetch_and_update_vars_with_provider(provider)


    def yaml(self, variables_file):
        provider = YamlProvider(variables_file)
        return self.__fetch_and_update_vars_with_provider(provider)
