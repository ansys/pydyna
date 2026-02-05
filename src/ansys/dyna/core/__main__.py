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
"""CLI for PyDyna.

Usage:
    python -m ansys.dyna.core agent --env cursor
    python -m ansys.dyna.core agent --env vscode
    python -m ansys.dyna.core agent --env claude
    python -m ansys.dyna.core agent --print
"""

import argparse
from pathlib import Path
import sys


def find_workspace_root() -> Path | None:
    """Find workspace root by walking up from cwd.

    Returns
    -------
    Path | None
        Path to workspace root, or None if not found.
    """
    cwd = Path.cwd()
    for parent in [cwd, *cwd.parents]:
        if (parent / ".git").exists():
            return parent
        if (parent / "pyproject.toml").exists():
            return parent
    return None


def get_instructions_path() -> Path:
    """Get path to agent instructions file.

    Returns
    -------
    Path
        Path to AGENT.md file.
    """
    return Path(__file__).parent / "AGENT.md"


def get_instructions_content() -> str:
    """Read the agent instructions from installed package.

    Returns
    -------
    str
        Content of AGENT.md file.
    """
    return get_instructions_path().read_text(encoding="utf-8")


def format_for_cursor(content: str, source_path: Path) -> str:
    """Wrap content in Cursor rules MDC format.

    Parameters
    ----------
    content : str
        The instructions content.
    source_path : Path
        Path to the source file.

    Returns
    -------
    str
        Formatted content for Cursor.
    """
    return f"""---
description: PyDyna usage instructions for AI assistants
globs: ["**/*.py", "**/*.k", "**/*.key"]
alwaysApply: false
---

<!-- Auto-generated from: {source_path} -->

{content}
"""


def get_output_path(env: str, workspace: Path) -> Path:
    """Determine output path based on environment.

    For cursor: writes to .cursor/rules/pydyna.mdc (isolated file, no conflicts)
    For vscode/copilot/claude/generic: writes to .agent/ansys.dyna.core.md

    Parameters
    ----------
    env : str
        Target environment.
    workspace : Path
        Workspace root path.

    Returns
    -------
    Path
        Output file path for PyDyna instructions.
    """
    if env == "cursor":
        cursor_dir = workspace / ".cursor" / "rules"
        cursor_dir.mkdir(parents=True, exist_ok=True)
        return cursor_dir / "pydyna.mdc"

    # All other environments: write to .agent/ directory
    agent_dir = workspace / ".agent"
    agent_dir.mkdir(parents=True, exist_ok=True)
    return agent_dir / "ansys.dyna.core.md"


def ensure_gitignore(workspace: Path, env: str) -> None:
    """Add .agent/ to .gitignore if we write there.

    Parameters
    ----------
    workspace : Path
        Workspace root path.
    env : str
        Target environment.
    """
    # Cursor uses its own directory structure, no need to gitignore
    if env == "cursor":
        return

    gitignore = workspace / ".gitignore"
    marker = ".agent/"
    if gitignore.exists():
        content = gitignore.read_text()
        if marker not in content:
            with gitignore.open("a") as f:
                f.write(f"\n# Agent instructions (auto-generated)\n{marker}\n")
    else:
        gitignore.write_text(f"# Agent instructions (auto-generated)\n{marker}\n")


def _make_pointer_block(pydyna_ref: str, tool_name: str, regenerate_cmd: str) -> tuple[str, str, str]:
    """Create a pointer block for single-file instruction tools.

    Returns start marker, end marker, and block content.
    """
    start = "<!-- pydyna-agent-instructions:start -->"
    end = "<!-- pydyna-agent-instructions:end -->"

    block = f"""{start}
## PyDyna (ansys-dyna-core)

PyDyna ships agent instructions. To use them, read the file at:

- `{pydyna_ref}`

Regenerate this pointer with: `{regenerate_cmd}`
{end}"""

    return start, end, block


def _update_single_file_instructions(
    workspace: Path,
    pydyna_path: Path,
    target_file: Path,
    tool_name: str,
    regenerate_cmd: str,
    default_header: str,
) -> Path:
    """Append/refresh a pointer section in a single-file instructions file.

    This preserves user content and only updates the marked PyDyna section.

    Parameters
    ----------
    workspace : Path
        Workspace root path.
    pydyna_path : Path
        Path to the PyDyna instructions file.
    target_file : Path
        Path to the tool's instruction file.
    tool_name : str
        Name of the tool (for messages).
    regenerate_cmd : str
        Command to regenerate this pointer.
    default_header : str
        Header to use if creating a new file.

    Returns
    -------
    Path
        Path to the updated file.
    """
    try:
        rel = pydyna_path.relative_to(workspace)
        pydyna_ref = str(rel).replace("\\", "/")
    except ValueError:
        pydyna_ref = str(pydyna_path)

    start, end, block = _make_pointer_block(pydyna_ref, tool_name, regenerate_cmd)

    if target_file.exists():
        text = target_file.read_text(encoding="utf-8", errors="replace")

        # Check if our markers exist and are in the right order
        if start in text and end in text and text.index(start) < text.index(end):
            # Replace existing block
            pre = text[: text.index(start)].rstrip()
            post = text[text.index(end) + len(end) :].lstrip()
            if pre:
                new_text = f"{pre}\n\n{block}\n"
            else:
                new_text = f"{block}\n"
            if post:
                new_text += f"\n{post}"
            new_text = new_text.rstrip() + "\n"
        else:
            # Append block to end
            new_text = text.rstrip() + "\n\n" + block + "\n"
    else:
        # Create new file with header and block
        target_file.parent.mkdir(parents=True, exist_ok=True)
        new_text = f"{default_header}\n\n{block}\n"

    target_file.write_text(new_text, encoding="utf-8")
    return target_file


def update_copilot_instructions(workspace: Path, pydyna_path: Path) -> Path:
    """Update .github/copilot-instructions.md with a pointer to PyDyna instructions."""
    github_dir = workspace / ".github"
    github_dir.mkdir(parents=True, exist_ok=True)
    copilot_file = github_dir / "copilot-instructions.md"

    return _update_single_file_instructions(
        workspace=workspace,
        pydyna_path=pydyna_path,
        target_file=copilot_file,
        tool_name="GitHub Copilot",
        regenerate_cmd="python -m ansys.dyna.core agent --env vscode",
        default_header="# Copilot Instructions",
    )


def update_claude_instructions(workspace: Path, pydyna_path: Path) -> Path:
    """Update CLAUDE.md with a pointer to PyDyna instructions."""
    claude_file = workspace / "CLAUDE.md"

    return _update_single_file_instructions(
        workspace=workspace,
        pydyna_path=pydyna_path,
        target_file=claude_file,
        tool_name="Claude Code",
        regenerate_cmd="python -m ansys.dyna.core agent --env claude",
        default_header="# Claude Code Instructions",
    )


def cmd_agent(args: argparse.Namespace) -> int:
    """Handle 'agent' subcommand.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command-line arguments.

    Returns
    -------
    int
        Exit code (0 for success, 1 for error).
    """
    content = get_instructions_content()
    source_path = get_instructions_path()

    # Format for environment
    if args.env == "cursor":
        formatted = format_for_cursor(content, source_path)
    else:
        formatted = content

    # Print mode
    if args.print:
        # Handle Windows console encoding issues
        try:
            print(formatted)
        except UnicodeEncodeError:
            sys.stdout.buffer.write(formatted.encode("utf-8"))
        return 0

    # Find workspace
    if args.workspace:
        workspace = Path(args.workspace)
    else:
        workspace = find_workspace_root()
        if workspace is None:
            print("Error: Could not detect workspace root.", file=sys.stderr)
            print("Run from a git repo or specify --workspace PATH", file=sys.stderr)
            return 1

    # Write PyDyna instructions file
    output_path = get_output_path(args.env, workspace)
    output_path.write_text(formatted, encoding="utf-8")
    ensure_gitignore(workspace, args.env)

    print(f"[OK] PyDyna instructions written to: {output_path}")

    # For single-file tools, also update their instruction file with a pointer
    if args.env in ("vscode", "copilot"):
        pointer_file = update_copilot_instructions(workspace, output_path)
        print(f"[OK] Added pointer to: {pointer_file}")
        print("     (Your existing instructions were preserved)")

    elif args.env == "claude":
        pointer_file = update_claude_instructions(workspace, output_path)
        print(f"[OK] Added pointer to: {pointer_file}")
        print("     (Your existing instructions were preserved)")

    return 0


def main() -> int:
    """Main entry point for CLI.

    Returns
    -------
    int
        Exit code.
    """
    parser = argparse.ArgumentParser(prog="python -m ansys.dyna.core", description="PyDyna CLI utilities")
    subparsers = parser.add_subparsers(dest="command")

    # agent subcommand
    agent_parser = subparsers.add_parser("agent", help="Install agent instructions for AI assistants")
    agent_parser.add_argument(
        "--env",
        choices=["cursor", "vscode", "copilot", "claude", "generic"],
        default="generic",
        help="Target environment: cursor, vscode/copilot, claude, or generic (default: generic)",
    )
    agent_parser.add_argument("--print", action="store_true", help="Print to stdout instead of writing file")
    agent_parser.add_argument("--workspace", help="Workspace root (auto-detected if not specified)")

    args = parser.parse_args()

    if args.command == "agent":
        return cmd_agent(args)
    else:
        parser.print_help()
        return 0


if __name__ == "__main__":
    sys.exit(main())
