from pathlib import Path

class FileData:
    def __init__(self, content: str, variables: set, path: str):
        self.content = content
        self.variables = variables
        self.path = path