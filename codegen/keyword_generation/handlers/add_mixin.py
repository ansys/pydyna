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
Add Mixin Handler: Adds mixin classes to keyword classes without card replacement.

This handler enables keywords to inherit functionality from mixin classes by adding
them to the class inheritance list. Unlike external-card-implementation, this handler
does not require card references and is used purely for adding mixins.
"""

from dataclasses import dataclass
import logging
import typing
from typing import Any, Dict

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import MixinImport
from keyword_generation.handlers.handler_base import handler
import keyword_generation.handlers.handler_base

logger = logging.getLogger(__name__)


@dataclass
class AddMixinSettings:
    """Configuration for adding a mixin to a keyword class.

    Attributes
    ----------
        source: Full module path to import from (e.g., "ansys.dyna.core.lib.curve_plotting_mixin")
        name: Mixin class name to add to keyword
    """

    source: str
    name: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AddMixinSettings":
        return cls(
            source=data["source"],
            name=data["name"],
        )


@handler(
    name="add-mixin",
    description="Adds mixin classes to keyword classes for functionality extension",
    input_schema={
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "source": {
                    "type": "string",
                    "description": "Full module path to import mixin from",
                },
                "name": {
                    "type": "string",
                    "description": "Mixin class name to add to keyword",
                },
            },
            "required": ["source", "name"],
        },
    },
    output_description="Adds to 'mixins' and 'mixin_imports' lists in kwd_data",
)
class AddMixinHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    """
    Adds mixin classes to keyword classes without requiring card replacement.

    This handler enables code reuse by allowing keywords to inherit functionality
    from mixin classes. Unlike external-card-implementation, this handler does not
    require card references and is used purely for adding mixins to the class
    inheritance list.

    Input Settings Example:
        [
            {
                "source": "ansys.dyna.core.lib.curve_plotting_mixin",
                "name": "CurvePlottingMixin"
            },
            {
                "source": "ansys.dyna.core.lib.validation_mixin",
                "name": "ValidationMixin"
            }
        ]

    Output Modification:
        - Appends to kwd_data["mixins"] = ["CurvePlottingMixin", "ValidationMixin", ...]
        - Appends to kwd_data["mixin_imports"] = [
              {"source": "ansys.dyna.core.lib.curve_plotting_mixin", "names": ["CurvePlottingMixin"]},
              ...
          ]
    """

    def handle(
        self,
        kwd_data: KeywordData,
        settings: typing.List[typing.Dict[str, typing.Any]],
    ) -> None:
        """
        Configure mixin imports and add mixins to class inheritance.

        Args:
            kwd_data: Complete keyword data dictionary
            settings: List of mixin configurations
        """
        typed_settings = [AddMixinSettings.from_dict(s) for s in settings]

        # Initialize lists if not already present
        if not kwd_data.mixins:
            kwd_data.mixins = []
        if not kwd_data.mixin_imports:
            kwd_data.mixin_imports = []

        for setting in typed_settings:
            logger.debug(
                "Adding mixin: source=%s, name=%s",
                setting.source,
                setting.name,
            )
            kwd_data.mixins.append(setting.name)
            kwd_data.mixin_imports.append(
                MixinImport(source=setting.source, names=[setting.name])
            )
