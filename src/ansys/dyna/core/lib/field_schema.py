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

"""Field schema definitions for efficient Card operations.

This module provides immutable schema definitions that can be shared across
all Card instances of the same type, avoiding repeated object creation.
"""

import typing
from typing import Any, Dict, NamedTuple, Tuple, Type

from ansys.dyna.core.lib.field import Field, Flag


class FieldSchema(NamedTuple):
    """Immutable schema for a single field in a Card.

    This is designed to be shared across all instances of a Card type.
    The `default` can be a value or a Flag instance for flag fields.
    """

    name: str
    type: Type
    offset: int
    width: int
    default: Any = None  # Can be a value or Flag instance

    def is_flag(self) -> bool:
        """Check if this field is a Flag type."""
        return isinstance(self.default, Flag)

    def to_field(self, value: Any = None) -> Field:
        """Create a Field instance from this schema with the given value.

        Parameters
        ----------
        value : Any, optional
            The value to set. If None, uses the default.

        Returns
        -------
        Field
            A new Field instance.
        """
        if self.is_flag():
            # For flag fields, create a copy of the Flag with the value set
            flag = Flag(
                value=value,
                true_value=self.default.true_value,
                false_value=self.default.false_value,
            )
            return Field(self.name, self.type, self.offset, self.width, flag)
        else:
            return Field(self.name, self.type, self.offset, self.width, value)

    @classmethod
    def from_field(cls, field: Field) -> "FieldSchema":
        """Create a FieldSchema from an existing Field.

        Parameters
        ----------
        field : Field
            The field to extract schema from.

        Returns
        -------
        FieldSchema
            An immutable schema representation.
        """
        if field._is_flag():
            # Preserve the Flag instance as the default
            return cls(
                name=field.name,
                type=field.type,
                offset=field.offset,
                width=field.width,
                default=field._value,  # The Flag instance
            )
        else:
            return cls(
                name=field.name,
                type=field.type,
                offset=field.offset,
                width=field.width,
                default=field._value,
            )


class CardSchema(NamedTuple):
    """Immutable schema for an entire Card.

    Contains the field schemas and a precomputed name-to-index mapping
    for fast value lookups.
    """

    fields: Tuple[FieldSchema, ...]
    name_to_index: Dict[str, int]

    @classmethod
    def from_fields(cls, fields: typing.List[Field]) -> "CardSchema":
        """Create a CardSchema from a list of Field objects.

        Parameters
        ----------
        fields : List[Field]
            The fields to extract schema from.

        Returns
        -------
        CardSchema
            An immutable schema representation.
        """
        field_schemas = tuple(FieldSchema.from_field(f) for f in fields)
        name_to_index = {fs.name: i for i, fs in enumerate(field_schemas)}
        return cls(fields=field_schemas, name_to_index=name_to_index)

    def to_fields(self, values: typing.List[Any]) -> typing.List[Field]:
        """Create Field instances from this schema with the given values.

        Parameters
        ----------
        values : List[Any]
            Values for each field.

        Returns
        -------
        List[Field]
            New Field instances with values set.
        """
        return [fs.to_field(v) for fs, v in zip(self.fields, values)]

    def get_index(self, name: str) -> int:
        """Get the index of a field by name.

        Parameters
        ----------
        name : str
            The field name.

        Returns
        -------
        int
            The field index.

        Raises
        ------
        KeyError
            If the field name is not found.
        """
        return self.name_to_index[name]

    def __len__(self) -> int:
        return len(self.fields)
