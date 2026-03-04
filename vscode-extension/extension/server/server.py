"""PyDyna LSP server."""

import logging

from lsprotocol import types
from pygls.lsp.server import LanguageServer

from documents import DocumentStore

logger = logging.getLogger(__name__)

server = LanguageServer("pydyna-lsp", "v0.1.0")

# Single shared document store — holds the source text for every open .k file.
_store = DocumentStore()


# ---------------------------------------------------------------------------
# Text document synchronisation
# ---------------------------------------------------------------------------


@server.feature(
    types.TEXT_DOCUMENT_DID_OPEN,
    types.TextDocumentSyncOptions(open_close=True, change=types.TextDocumentSyncKind.Full),
)
def did_open(params: types.DidOpenTextDocumentParams) -> None:
    """Track document text when a file is opened."""
    _store.open(params.text_document.uri, params.text_document.text)


@server.feature(types.TEXT_DOCUMENT_DID_CHANGE)
def did_change(params: types.DidChangeTextDocumentParams) -> None:
    """Update stored text on every full-document change notification."""
    # We request full-document sync (no incremental diffs), so there is always
    # exactly one change event whose ``text`` field contains the whole document.
    if params.content_changes:
        _store.change(params.text_document.uri, params.content_changes[-1].text)


@server.feature(types.TEXT_DOCUMENT_DID_CLOSE)
def did_close(params: types.DidCloseTextDocumentParams) -> None:
    """Remove document text when the editor closes the file."""
    _store.close(params.text_document.uri)


# ---------------------------------------------------------------------------
# Hover (hello-world stub — will be replaced in Step 3)
# ---------------------------------------------------------------------------


@server.feature(types.TEXT_DOCUMENT_HOVER)
def hover(params: types.HoverParams) -> types.Hover | None:
    """Return a hello-world hover for any word in a .k file."""
    import re

    doc = server.workspace.get_text_document(params.text_document.uri)
    line_text = doc.lines[params.position.line] if params.position.line < len(doc.lines) else ""

    col = params.position.character
    start_char = end_char = col
    word = None
    for m in re.finditer(r"\b\w+\b", line_text):
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
