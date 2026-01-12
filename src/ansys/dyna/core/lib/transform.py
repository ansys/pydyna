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

"""Transformation handler for ``INCLUDE_TRANSFORM``."""

import typing
import warnings

from ansys.dyna.core.lib.import_handler import ImportContext, ImportHandler
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.transforms.base_transform import Transform
from ansys.dyna.core.lib.transforms.element_transform import TransformElement
from ansys.dyna.core.lib.transforms.node_transform import TransformNode


class TransformHandler(ImportHandler):
    def __init__(self):
        self._handlers: typing.Dict[typing.Union[str, typing.Tuple[str, str]], Transform] = {
            "NODE": TransformNode,
            "ELEMENT": TransformElement,
        }

    def register_transform_handler(
        self, identity: typing.Union[str, typing.Tuple[str, str]], handler: Transform
    ) -> None:
        self._handlers[identity] = handler

    def after_import(self, context: ImportContext, keyword: typing.Union[KeywordBase, str]) -> None:
        if not isinstance(keyword, KeywordBase):
            return
        if context.xform is None:
            return
        # first try to get the specialized handler for the keyword + subkeyword
        identity = (keyword.keyword, keyword.subkeyword)
        handler = self._handlers.get(identity, None)
        if handler is None:
            # then try to get the handler for the keyword
            identity = keyword.keyword
            handler = self._handlers.get(identity, None)
            if handler is None:
                warnings.warn()
                return

        # if a handler was found, initialize it with the xform from the context
        # and transform the keyword with it
        handler(context.xform).transform(keyword)

    def on_error(self, error):
        pass
