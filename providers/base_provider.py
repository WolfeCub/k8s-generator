class BaseProvider:
    def __init__(self, config: dict):
        raise NotImplementedError

    def fetch_variables(self, var_list: list) -> dict:
        raise NotImplementedError