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
    python -m ansys.dyna.core agent --env vscode/copilot
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

    Parameters
    ----------
    env : str
        Target environment (cursor, vscode/copilot, or generic).
    workspace : Path
        Workspace root path.

    Returns
    -------
    Path
        Output file path.
    """
    if env == "cursor":
        cursor_dir = workspace / ".cursor" / "rules"
        cursor_dir.mkdir(parents=True, exist_ok=True)
        return cursor_dir / "pydyna.mdc"
    elif env in ("vscode", "copilot"):  # copilot is alias for vscode
        github_dir = workspace / ".github"
        github_dir.mkdir(parents=True, exist_ok=True)
        return github_dir / "copilot-instructions.md"
    else:  # generic
        agent_dir = workspace / ".agent"
        agent_dir.mkdir(parents=True, exist_ok=True)
        return agent_dir / "ansys.dyna.core.md"


def ensure_gitignore(workspace: Path, env: str) -> None:
    """Add .agent/ to .gitignore if using generic output.

    Parameters
    ----------
    workspace : Path
        Workspace root path.
    env : str
        Target environment.
    """
    if env != "generic":
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
            # Fall back to UTF-8 if console doesn't support Unicode
            import sys

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

    # Write file
    output_path = get_output_path(args.env, workspace)
    output_path.write_text(formatted, encoding="utf-8")
    ensure_gitignore(workspace, args.env)

    print(f"[OK] Agent instructions written to: {output_path}")
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
        choices=["cursor", "vscode", "copilot", "generic"],
        default="generic",
        help="Target environment: cursor, vscode/copilot, or generic (default: generic)",
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
