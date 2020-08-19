import os
import hvac
import urllib3

from providers.base_provider import BaseProvider
from models.cli_error_exception import CliErrorException

class VaultProvider(BaseProvider):
    def __init__(self, url, mount_point, verify):
        self.url = url
        self.mount_point = mount_point
        self.verify = verify

        if self.url is None:
            raise CliErrorException('Vault url cannot be empty')
        if self.mount_point is None:
            raise CliErrorException('Vault mount point cannot be empty')
        if 'VAULT_TOKEN' not in os.environ:
            raise CliErrorException('VAULT_TOKEN is not set in your environment')

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
