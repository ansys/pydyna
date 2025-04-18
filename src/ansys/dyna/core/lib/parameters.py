# Copyright (C) 2021 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Parameter classes."""

import typing

from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.keyword_base import KeywordBase


class ParameterSet:
    """Deck parameters."""

    def __init__(self):
        self._params = dict()

    def get(self, param: str) -> typing.Any:
        """Get a parameter by name."""
        return self._params[param]

    def add(self, param: str, value: typing.Any) -> None:
        """Add a parameter."""
        self._params[param] = value


def _unpack_param(param: "kwd.Parameter.Parameter") -> typing.Union[type, str, typing.Any]:
    """Converts parameter into type, name, and value of the given type."""
    name_field = param.name
    type_code = name_field[0]
    if type_code == "R":
        t = float
    elif type_code == "I":
        t = int
    elif type_code == "C":
        t = str
    else:
        raise Exception(f"Parameter name {param_name} does not name a type")
    val = t(param.val.strip())
    name = name_field[1:].strip()
    return t, name, val


def _load_parameters(deck, parameter: "kwd.Parameter"):
    for p in parameter.parameters:
        t, name, val = _unpack_param(p)
        deck.parameters.add(name, val)


class ParameterHandler(ImportHandler):
    def __init__(self):
        pass

    def after_import(self, context: ImportContext, keyword: typing.Union["KeywordBase", str]) -> None:
        from ansys.dyna.core import keywords as kwd

        if isinstance(keyword, kwd.Parameter):
            _load_parameters(context.deck, keyword)

    def on_error(self, error):
        pass
