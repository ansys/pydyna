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
Additional Imports Handler: Injects extra top-level imports into generated keyword modules.

Use this when a length-func, active-func, or other inline expression in the manifest
references a symbol from a module that is not imported by default (e.g. ``math.ceil``).

Example manifest entry::

    "generation-options": {
        "additional-imports": [
            {"name": "math"}
        ]
    }

This produces ``import math`` near the top of the generated ``.py`` file,
after the standard library ``import typing`` line.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict, List

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.handler_base import handler
import keyword_generation.handlers.handler_base

logger = logging.getLogger(__name__)


@dataclass
class AdditionalImportSettings:
    """Configuration for a single additional import.

    Attributes
    ----------
        name: The module name to import (e.g. ``"math"``).
    """

    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AdditionalImportSettings":
        """Create settings from a manifest dict entry.

        Args:
            data: Dict with a required ``"name"`` key.

        Returns
        -------
            AdditionalImportSettings instance.

        Raises
        ------
            KeyError: If ``"name"`` is missing.
        """
        return cls(name=data["name"])


@handler(
    name="additional-imports",
    description="Adds extra top-level imports (e.g. 'math') to the generated keyword module",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string",
                    "description": "Module name to import (e.g. 'math', 'functools')",
                },
            },
            "required": ["name"],
        },
    },
    output_description="Appends module name strings to kwd_data.additional_imports",
)
class AdditionalImportsHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """Adds extra top-level imports to the generated keyword module.

    This is needed when inline expressions in the manifest (``length-func``,
    ``active-func``, etc.) reference symbols from modules that are not part of the
    default import set produced by ``imports.j2``.

    Input Settings Example::

        [{"name": "math"}, {"name": "functools"}]

    Output Modification:
        Appends to ``kwd_data.additional_imports``, e.g. ``["math", "functools"]``.
        The ``imports.j2`` template iterates this list and emits one ``import <name>``
        statement per entry, placed after the standard ``import typing`` line.
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, Any]],
    ) -> None:
        """Populate kwd_data.additional_imports from manifest settings.

        Args:
            kwd_data: Keyword data being built for this keyword.
            settings: List of dicts, each with a ``"name"`` key.
        """
        typed_settings: List[AdditionalImportSettings] = [
            AdditionalImportSettings.from_dict(s) for s in settings
        ]

        for setting in typed_settings:
            if setting.name not in kwd_data.additional_imports:
                logger.debug("Adding additional import: %s", setting.name)
                kwd_data.additional_imports.append(setting.name)
            else:
                logger.debug("Skipping duplicate additional import: %s", setting.name)
