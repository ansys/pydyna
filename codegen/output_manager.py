# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
"""
OutputManager: Encapsulates all file and directory operations for codegen output.
"""
import os


class OutputManager:
    AUTO_DIR = "auto"
    AUTO_KEYWORDS_FILE = "auto_keywords.py"
    TYPE_MAPPING_FILE = "type_mapping.py"
    AUTODOC_INDEX_FILE = "index.rst"

    def __init__(self, base_path: str):
        self.base_path = base_path
        self.auto_path = os.path.join(base_path, self.AUTO_DIR)
        os.makedirs(self.auto_path, exist_ok=True)

    def _write_file(self, rel_path: str, content: str):
        path = os.path.join(self.base_path, rel_path)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(content)

    def write_auto_file(self, domain: str, filename: str, content: str):
        domain_path = os.path.join(self.auto_path, domain)
        os.makedirs(domain_path, exist_ok=True)
        file_path = os.path.join(domain_path, filename)
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(content)

    def write_autodoc(self, autodoc_output_path: str, content: str):
        os.makedirs(autodoc_output_path, exist_ok=True)
        file_path = os.path.join(autodoc_output_path, self.AUTODOC_INDEX_FILE)
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(content)

    def write_auto_keywords_file(self, content: str):
        self._write_file(self.AUTO_KEYWORDS_FILE, content)

    def write_type_mapping_file(self, content: str):
        self._write_file(self.TYPE_MAPPING_FILE, content)

    def write_domain_init_file(self, domain: str, content: str):
        self.write_auto_file(domain, "__init__.py", content)

    def write_main_init_file(self, content: str):
        self._write_file(os.path.join(self.AUTO_DIR, "__init__.py"), content)

    def clean(self):
        import shutil

        for fname in [self.AUTO_KEYWORDS_FILE, self.TYPE_MAPPING_FILE]:
            try:
                os.remove(os.path.join(self.base_path, fname))
            except FileNotFoundError:
                pass
        try:
            shutil.rmtree(self.auto_path)
        except FileNotFoundError:
            pass
