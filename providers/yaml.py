import yaml

from providers.base_provider import BaseProvider

class YamlProvider(BaseProvider):
    def __init__(self, config: dict, variables_file: str):
        with open(variables_file) as f:
            self.yaml_vars = yaml.safe_load(f.read())

    def fetch_variables(self, var_list: list) -> dict:
        return self.yaml_vars