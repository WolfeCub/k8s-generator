import os
from pathlib import Path

from providers.vault import VaultProvider
from providers.yaml import YamlProvider
from models.file_data import FileData
from utils import recursive_dict_update

class Render:
    def __init__(self, config, environment, file_data_list):
        self.__config = config
        self.__env = environment
        self.__file_data_list = file_data_list
        self.__vars_fetched_so_far = {}

        self.__provider = None


    def __str__(self):
        self.__render_all_paths()
        return '' # TODO: This adds an extra empty line to the output


    def __render_all_paths(self, output_dir=None):
        if output_dir is not None:
            Path(output_dir).mkdir(parents=True, exist_ok=True)

        for file_data in self.__file_data_list:
            result = self.__env.from_string(file_data.content).render(self.__vars_fetched_so_far)

            if output_dir is None:
                print(result)
                return

            new_path = Path(output_dir).joinpath(path)
            if path.is_dir():
                new_path.mkdir(parents=True, exist_ok=True)
                return

            with open(new_path, 'w') as f:
                f.write(result)

    
    def __fetch_vars_from_provider(self) -> dict:
        variables_to_fetch = set()

        for file_data in self.__file_data_list:
            variables_to_fetch = variables_to_fetch.union(file_data.variables)

        return self.__provider.fetch_variables(variables_to_fetch)

    def __fetch_and_update_vars_with_provider(self, provider, *vargs):
        self.__provider = provider(self.__config, *vargs)
        var_dict = self.__fetch_vars_from_provider()

        self.__vars_fetched_so_far = recursive_dict_update(self.__vars_fetched_so_far, var_dict)

        return self

    def vault(self):
        return self.__fetch_and_update_vars_with_provider(VaultProvider)

    def yaml(self, variables_file):
        return self.__fetch_and_update_vars_with_provider(YamlProvider, variables_file)