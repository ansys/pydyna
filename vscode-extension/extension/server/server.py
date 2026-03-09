"""PyDyna LSP server."""

import logging

from lsprotocol import types
from pygls.lsp.server import LanguageServer

from documents import DocumentStore

logger = logging.getLogger(__name__)

server = LanguageServer(
    "pydyna-lsp",
    "v0.1.0",
    text_document_sync_kind=types.TextDocumentSyncKind.Full,
)

# Single shared document store — holds the source text for every open .k file.
_store = DocumentStore()


# ---------------------------------------------------------------------------
# Text document synchronisation
# ---------------------------------------------------------------------------


@server.feature(types.TEXT_DOCUMENT_DID_OPEN)
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
# Hover — narrow field documentation
# ---------------------------------------------------------------------------


@server.feature(types.TEXT_DOCUMENT_HOVER)
def hover(params: types.HoverParams) -> types.Hover | None:
    """Return LS-DYNA field documentation for the symbol under the cursor."""
    from hover import narrow_hover

    return narrow_hover(_store, params)


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(name)s: %(message)s")
    logger.info("Starting PyDyna LSP server")
    server.start_io()
