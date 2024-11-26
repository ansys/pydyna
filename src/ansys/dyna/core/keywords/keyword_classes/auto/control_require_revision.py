# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlRequireRevision(KeywordBase):
    """DYNA CONTROL_REQUIRE_REVISION keyword"""

    keyword = "CONTROL"
    subkeyword = "REQUIRE_REVISION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "release",
                        str,
                        0,
                        10,
                        kwargs.get("release")
                    ),
                    Field(
                        "svnrev",
                        int,
                        10,
                        10,
                        kwargs.get("svnrev")
                    ),
                    Field(
                        "gitrev",
                        int,
                        20,
                        10,
                        kwargs.get("gitrev")
                    ),
                ],
            ),
        ]

    @property
    def release(self) -> typing.Optional[str]:
        """Get or set the The release of code required. This should be a string such as "R6.1.0" or "R7.0".
        """ # nopep8
        return self._cards[0].get_value("release")

    @release.setter
    def release(self, value: str) -> None:
        self._cards[0].set_value("release", value)

    @property
    def svnrev(self) -> typing.Optional[int]:
        """Get or set the The minimum SVN revision required (ignored by executables made after our move to git). This corresponds to the “SVN Version” field in the d3hsp file for versions of LS-DYNA prior to our move to git for version control.  R12.0 was the last release version made while we were using SVN for version control.
        """ # nopep8
        return self._cards[0].get_value("svnrev")

    @svnrev.setter
    def svnrev(self, value: int) -> None:
        self._cards[0].set_value("svnrev", value)

    @property
    def gitrev(self) -> typing.Optional[int]:
        """Get or set the The minimum git revision required corresponding to the release of code (ignored by executables made prior to our move to git). This corresponds to part of the git hash given in the “Revision” field in the d3hsp file for versions of LS-DYNA after our move to git for version control. In d3hsp, the last string is “R[Release #]-[number]-[alphanumeric]”. If RELEASE is given, LS-DYNA compares GITREV to that [number]. If RELEASE is not given, see Remark 2.  R11.2 was the first release version made when we moved to git
        """ # nopep8
        return self._cards[0].get_value("gitrev")

    @gitrev.setter
    def gitrev(self, value: int) -> None:
        self._cards[0].set_value("gitrev", value)

