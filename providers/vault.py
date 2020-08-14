import os
import hvac
from providers.base_provider import BaseProvider

import urllib3

class VaultProvider(BaseProvider):
    def __init__(self, config):
        config_section = config['vault']
        self.url = config_section['url']
        self.mount_point = config_section['mount_point']
        self.verify = True if 'verify' not in config_section else config_section['verify']

        if not self.verify:
            urllib3.disable_warnings()

        self.client = hvac.Client(
            url=self.url,
            token=os.environ['VAULT_TOKEN'],
            verify=self.verify
        )

    def fetch_variables(self, var_list):
        variables = {}
        for var in var_list:
            read_response = self.client.secrets.kv.v1.read_secret(
                mount_point=self.mount_point,
                path=var
            )
            variables[var] = read_response['data']

        return variables
