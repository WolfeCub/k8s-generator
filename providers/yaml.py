import yaml

from providers.base_provider import BaseProvider

class YamlProvider(BaseProvider):
    def __init__(self, config: dict):
        raise NotImplementedError

    def fetch_variables(self, var_list: list) -> dict:
        raise NotImplementedError