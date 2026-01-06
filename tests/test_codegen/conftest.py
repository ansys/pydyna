# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Shared fixtures for codegen tests."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData


@pytest.fixture
def sample_field():
    """Create a basic Field instance for testing."""
    return Field(
        name="pid",
        type="int",
        position=0,
        width=10,
        default=None,
        help="Part ID",
        used=True,
    )


@pytest.fixture
def sample_field_dict():
    """Create a field dictionary for testing from_dict conversion."""
    return {
        "name": "pid",
        "type": "integer",
        "position": 0,
        "width": 10,
        "default": 0,
        "help": "Part ID",
        "used": True,
    }


@pytest.fixture
def sample_card():
    """Create a basic Card instance for testing."""
    return Card(
        index=0,
        fields=[
            Field(name="pid", type="int", position=0, width=10, default=None, help="Part ID"),
            Field(name="secid", type="int", position=10, width=10, default=None, help="Section ID"),
        ],
    )


@pytest.fixture
def sample_card_dict():
    """Create a card dictionary for testing from_dict conversion."""
    return {
        "index": 0,
        "fields": [
            {
                "name": "pid",
                "type": "integer",
                "position": 0,
                "width": 10,
                "default": None,
                "help": "Part ID",
                "used": True,
            },
            {
                "name": "secid",
                "type": "integer",
                "position": 10,
                "width": 10,
                "default": None,
                "help": "Section ID",
                "used": True,
            },
        ],
    }


@pytest.fixture
def sample_keyword_data():
    """Create a basic KeywordData instance for testing."""
    return KeywordData(
        keyword="SECTION",
        subkeyword="SHELL",
        title="*SECTION_SHELL",
        cards=[
            Card(
                index=0,
                fields=[
                    Field(name="secid", type="int", position=0, width=10, default=None, help="Section ID"),
                    Field(name="elform", type="int", position=10, width=10, default=2, help="Element formulation"),
                ],
            ),
        ],
    )


@pytest.fixture
def sample_keyword_data_dict():
    """Create a keyword data dictionary for testing from_dict conversion."""
    return {
        "keyword": "SECTION",
        "subkeyword": "SHELL",
        "title": "*SECTION_SHELL",
        "cards": [
            {
                "index": 0,
                "title": "Card 1",
                "description": "Section card",
                "comment": "",
                "fields": [
                    {
                        "name": "secid",
                        "type": "integer",
                        "position": 0,
                        "width": 10,
                        "default": None,
                        "help": "Section ID",
                        "used": True,
                    },
                    {
                        "name": "elform",
                        "type": "integer",
                        "position": 10,
                        "width": 10,
                        "default": 2,
                        "help": "Element formulation",
                        "used": True,
                    },
                ],
            },
        ],
    }
