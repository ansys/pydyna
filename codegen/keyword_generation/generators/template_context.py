# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Template context objects for Jinja2 rendering.

This module provides structured context objects that replace manual dict building
for template rendering. Benefits include:
- Type safety and IDE autocomplete
- Clear documentation of template variables
- Easier maintenance and refactoring
"""

from dataclasses import dataclass, asdict
from typing import Dict, List, Optional

from keyword_generation.data_model.keyword_data import KeywordData


@dataclass
class KeywordTemplateContext:
    """
    Context object for keyword.j2 template rendering.

    This dataclass encapsulates all variables needed by the keyword template,
    replacing manual dict construction with a typed, documented structure.

    Template Variables (available in keyword.j2):
        license: Copyright header text
        keyword_data: KeywordData instance with all keyword metadata
        alias: Optional alias class name (e.g., "SectionShell" for "SECTION_SHELL_TITLE")
        alias_subkeyword: Optional alias subkeyword portion
        openbrace: Literal "{" character (for Jinja escaping)
        closebrace: Literal "}" character (for Jinja escaping)
        repeated_element_types: Type mappings for pandas repeated elements
            {"int": "pd.Int32Dtype()", "float": "np.float64", "str": "str"}
    """

    license: str
    keyword_data: KeywordData
    alias: Optional[str]
    alias_subkeyword: Optional[str]
    openbrace: str = "{"
    closebrace: str = "}"
    repeated_element_types: Dict[str, str] = None

    def __post_init__(self):
        """Set default repeated_element_types if not provided."""
        if self.repeated_element_types is None:
            self.repeated_element_types = {
                "int": "pd.Int32Dtype()",
                "float": "np.float64",
                "str": "str",
            }

    def to_dict(self) -> Dict:
        """Convert to dict for template.render(**context.to_dict())."""
        return asdict(self)


@dataclass
class EntrypointTemplateContext:
    """
    Context object for importer.j2 and type-mapping.j2 templates.

    Template Variables:
        license: Copyright header text
        keywords: List of keyword metadata dicts with 'classname', 'modulename', etc.
    """

    license: str
    keywords: List[Dict]

    def to_dict(self) -> Dict:
        """Convert to dict for template.render(**context.to_dict())."""
        return asdict(self)


@dataclass
class DocTemplateContext:
    """
    Context object for autodoc RST templates.

    Template Variables:
        categories: List of category names (for index template)
        category: Category name (for category template)
        keywords: List of keyword dicts for the category (for category template)
    """

    categories: Optional[List[str]] = None
    category: Optional[str] = None
    keywords: Optional[List[Dict]] = None

    def to_dict(self) -> Dict:
        """Convert to dict for template.render(**context.to_dict())."""
        result = {}
        if self.categories is not None:
            result["categories"] = self.categories
        if self.category is not None:
            result["category"] = self.category
        if self.keywords is not None:
            result["keywords"] = self.keywords
        return result
