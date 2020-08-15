class BaseProvider:
    def fetch_variables(self, var_list: list) -> dict:
        raise NotImplementedError