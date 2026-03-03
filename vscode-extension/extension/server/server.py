"""Minimal PyDyna LSP server - hello world."""

import logging
import re

from lsprotocol import types
from pygls.lsp.server import LanguageServer

logger = logging.getLogger(__name__)

server = LanguageServer("pydyna-lsp", "v0.1.0")


@server.feature(types.TEXT_DOCUMENT_HOVER)
def hover(params: types.HoverParams) -> types.Hover | None:
    """Return a hello-world hover for any word in a .k file."""
    doc = server.workspace.get_text_document(params.text_document.uri)
    line = doc.lines[params.position.line] if params.position.line < len(doc.lines) else ""

    # Find the word under the cursor
    col = params.position.character
    match = re.search(r"\b\w+\b", line)
    word = None
    for m in re.finditer(r"\b\w+\b", line):
        if m.start() <= col <= m.end():
            word = m.group()
            start_char = m.start()
            end_char = m.end()
            break

    if word is None:
        return None

    return types.Hover(
        contents=types.MarkupContent(
            kind=types.MarkupKind.Markdown,
            value=f"**Hello, World!** 👋\n\nYou hovered over `{word}`.\n\n*PyDyna LSP is working.*",
        ),
        range=types.Range(
            start=types.Position(line=params.position.line, character=start_char),
            end=types.Position(line=params.position.line, character=end_char),
        ),
    )


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(name)s: %(message)s")
    logger.info("Starting PyDyna LSP server")
    server.start_io()
