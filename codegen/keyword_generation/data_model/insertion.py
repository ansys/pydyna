from dataclasses import dataclass
import typing

@dataclass
class Insertion:
    target_index: int = None
    target_class: str = None
    card: typing.Dict = None
