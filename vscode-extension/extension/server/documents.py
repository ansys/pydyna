"""Document text management for the PyDyna LSP server.

Responsibilities
----------------
- Track the source text of every open LS-DYNA document (open / change / close).
- Provide :func:`extract_keyword_block` which, given the full document text and a
  0-based line number, returns the raw text of the keyword block that contains
  that line together with the 0-based line number at which the block starts.

Keyword-block boundaries
------------------------
An LS-DYNA file is a sequence of keyword blocks.  Each block starts with a line
that begins with ``*`` (the keyword line itself) and ends just before the next
line that begins with ``*`` (or at end-of-file).  ``*END`` terminates the deck
and is not a keyword with a body.

The function returns only the text that belongs to a *real* keyword block — i.e.
everything from ``*KEYWORD_NAME`` up to (but not including) the next ``*`` line.
Lines before the first ``*`` keyword (e.g. a bare ``*KEYWORD`` header) and the
``*END`` sentinel are excluded from the returned block.

Example
-------
Given the text::

    *KEYWORD            ← line 0  (file header — not a data keyword)
    *DEFINE_CURVE       ← line 1  (block starts here)
    $# lcid             ← line 2
         1              ← line 3
    *MAT_ELASTIC        ← line 4  (new block)
    …

``extract_keyword_block(text, 3)`` returns ``(block_text, 1)``, where
``block_text`` is::

    *DEFINE_CURVE
    $# lcid
         1

"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from typing import Dict, Optional, Tuple

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Keyword-line detection
# ---------------------------------------------------------------------------

# A keyword line starts with * followed by at least one letter.
# This excludes *END (treated as a terminator, not a data keyword) and bare
# comment-style lines, but we handle *END explicitly below.
_KWD_LINE_RE = re.compile(r"^\*[A-Za-z]", re.ASCII)


def _is_keyword_line(line: str) -> bool:
    """Return True if *line* is a keyword header (starts with ``*<letter>``)."""
    return bool(_KWD_LINE_RE.match(line))


def _is_end_line(line: str) -> bool:
    """Return True if *line* is ``*END`` (case-insensitive)."""
    return line.rstrip().upper() == "*END"


# ---------------------------------------------------------------------------
# Block extraction
# ---------------------------------------------------------------------------


@dataclass
class KeywordBlock:
    """A keyword block extracted from an LS-DYNA document.

    Attributes
    ----------
    text : str
        The raw text of the block, from the ``*KEYWORD_NAME`` line to the end
        of its body (trailing newline stripped).
    start_line : int
        0-based line number of the ``*KEYWORD_NAME`` line within the full
        document text.
    end_line : int
        0-based line number of the last line belonging to this block (inclusive).
    """

    text: str
    start_line: int
    end_line: int


def extract_keyword_block(text: str, line: int) -> Optional[KeywordBlock]:
    """Return the keyword block that contains the given line.

    Parameters
    ----------
    text:
        Full document text (may contain ``\\r\\n`` or ``\\n`` line endings).
    line:
        0-based line number of the position of interest.

    Returns
    -------
    KeywordBlock or None
        ``None`` if *line* does not fall inside any keyword block (e.g. it is
        in the file header area before the first data keyword, or the file has
        no keyword blocks at all).
    """
    lines = text.splitlines()
    if line < 0 or line >= len(lines):
        return None

    # Walk backwards from *line* to find the start of the containing block.
    block_start = None
    for i in range(line, -1, -1):
        if _is_keyword_line(lines[i]):
            # *KEYWORD (bare header) and *END are not data-keyword blocks.
            stripped = lines[i].rstrip()
            upper = stripped.upper()
            if upper == "*KEYWORD" or upper.startswith("*KEYWORD ") or upper == "*END":
                # The cursor is in the header/trailer area — no data block.
                return None
            block_start = i
            break

    if block_start is None:
        # No keyword line found above the cursor: in the pre-keyword header.
        return None

    # Walk forward from block_start+1 to find the end of the block.
    block_end = len(lines) - 1
    for i in range(block_start + 1, len(lines)):
        if _is_keyword_line(lines[i]):
            block_end = i - 1
            break

    # Exclude trailing *END from the block body.
    while block_end > block_start and _is_end_line(lines[block_end]):
        block_end -= 1

    block_lines = lines[block_start : block_end + 1]
    return KeywordBlock(
        text="\n".join(block_lines),
        start_line=block_start,
        end_line=block_end,
    )


# ---------------------------------------------------------------------------
# Document store
# ---------------------------------------------------------------------------


@dataclass
class DocumentStore:
    """In-memory store of open document texts, keyed by URI.

    The store is the single source of truth for document content.  pygls also
    maintains its own ``workspace.text_documents``, but we keep our own copy so
    that :func:`extract_keyword_block` and the broad-index parser always operate
    on the same string regardless of which pygls workspace sync mode is in use.
    """

    _docs: Dict[str, str] = field(default_factory=dict)

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def open(self, uri: str, text: str) -> None:
        """Record *text* as the current content of *uri*."""
        logger.debug("document opened: %s (%d chars)", uri, len(text))
        self._docs[uri] = text

    def change(self, uri: str, text: str) -> None:
        """Replace the content of *uri* with *text* (full-document sync)."""
        logger.debug("document changed: %s (%d chars)", uri, len(text))
        self._docs[uri] = text

    def close(self, uri: str) -> None:
        """Remove *uri* from the store."""
        logger.debug("document closed: %s", uri)
        self._docs.pop(uri, None)

    # ------------------------------------------------------------------
    # Queries
    # ------------------------------------------------------------------

    def get_text(self, uri: str) -> Optional[str]:
        """Return the current text for *uri*, or ``None`` if not open."""
        return self._docs.get(uri)

    def get_keyword_block(self, uri: str, line: int) -> Optional[KeywordBlock]:
        """Return the keyword block at *line* in *uri*, or ``None``."""
        text = self._docs.get(uri)
        if text is None:
            return None
        return extract_keyword_block(text, line)

    def __contains__(self, uri: str) -> bool:
        return uri in self._docs

    def __len__(self) -> int:
        return len(self._docs)
