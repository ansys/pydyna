import os
import pathlib
import typing

from jinja2 import Environment

from keyword_generation.utils import fix_keyword, get_license_header


def generate_entrypoints(env: Environment, lib_path: str, keywords_list: typing.List[typing.Dict]) -> None:
    """use templates to write keywords/type_mapping.py, keywords/__init__.py and touch keywords/auto/__init__.py"""
    license_header = get_license_header()
    keywords_lists = {"license": license_header, "keywords": keywords_list}
    with open(os.path.join(lib_path, "auto_keywords.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("importer.j2").render(**keywords_lists))

    with open(os.path.join(lib_path, "type_mapping.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("type-mapping.j2").render(**keywords_lists))

    with open(pathlib.Path(lib_path) / "auto" / "__init__.py", "w", encoding="utf-8") as f:
        f.write(license_header)
