import abc
import typing

class KeywordHandler(metaclass=abc.ABCMeta):
    """Abstract base class for keyword handlers."""

    @abc.abstractmethod
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        raise NotImplementedError

    @abc.abstractmethod
    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        raise NotImplementedError
