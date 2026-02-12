AI Assistant Integration
=========================

PyDyna includes built-in instructions for AI assistants (like GitHub Copilot, Cursor, 
Claude Code, etc.) to help them understand how to work with PyDyna code. This feature 
provides AI assistants with accurate, version-matched documentation without requiring 
web searches or dealing with outdated information.

.. contents:: Table of Contents
   :local:
   :depth: 2

Overview
--------

The AI agent instructions feature allows PyDyna to ship usage documentation directly 
with the package, making it discoverable by AI assistants through standard Python 
mechanisms. When you install PyDyna, these instructions are already included and ready 
to use with your preferred AI coding assistant.

**Key Benefits:**

- **Version-matched**: Instructions always match your installed PyDyna version
- **Local-first**: No web dependency or outdated online documentation
- **Multi-file structure**: Comprehensive coverage organized into focused guides
- **Cross-ecosystem compatible**: Works with multiple AI assistant tools
- **Standardized approach**: Uses Python entry points for discoverability

Quick Start
-----------

After installing PyDyna, enable AI assistant integration in your workspace:

**For GitHub Copilot (VS Code):**

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy

**For Cursor:**

.. code-block:: bash

   python -m ansys.dyna.core agent --env cursor --copy

**For Claude Code:**

.. code-block:: bash

   python -m ansys.dyna.core agent --env claude --copy

**For generic tools:**

.. code-block:: bash

   python -m ansys.dyna.core agent --copy

This copies PyDyna's usage instructions to your workspace where AI assistants can find them.

How it works
------------

Discovery mechanism
~~~~~~~~~~~~~~~~~~~

PyDyna registers its agent instructions using Python's standard entry points mechanism:

.. code-block:: python

   # In Python code
   import ansys.dyna.core
   print(ansys.dyna.core.AGENT_INSTRUCTIONS)
   # Output: /path/to/site-packages/ansys/dyna/core/AGENT.md

Tools that support Python introspection can automatically discover these instructions.


Installation modes
------------------

Copy mode (Default)
~~~~~~~~~~~~~~~~~~~

Copy mode duplicates instruction files into your workspace with working relative links:

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy

**Created structure:**

.. code-block:: text

   .agent/
   ├── manifest.json              # Machine-readable registry
   ├── README.md                  # Human-readable index
   ├── ansys.dyna.core.md        # Main instructions (links rewritten)
   └── ansys.dyna.core/          # Extended documentation
       ├── deck.md
       ├── keywords.md
       └── patterns.md
   .github/
   └── copilot-instructions.md   # Pointer to main instructions

**Use copy mode when:**

- Working with sandboxed AI assistants
- Running in CI/CD environments
- Agent doesn't have full file system access
- You want version-locked documentation in your repository

Pointer Mode
~~~~~~~~~~~~

Pointer mode adds a reference to your installed package without copying files:

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --pointer

This creates a minimal manifest and adds a pointer to your AI tool's instruction file 
pointing to the installed package location.

**Use pointer mode when:**

- Agent has full file system access
- You want to save disk space
- Instructions should update when you upgrade PyDyna
- Working in a single-user environment

Tool-Specific Integration
--------------------------

GitHub Copilot (VS Code)
~~~~~~~~~~~~~~~~~~~~~~~~

Updates ``.github/copilot-instructions.md`` with a pointer to PyDyna instructions:

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy

Your existing instructions are preserved. The PyDyna pointer is added in a clearly 
marked section that can be safely regenerated.

**What happens:**

1. Instructions copied to ``.agent/ansys.dyna.core.md``
2. Extended docs copied to ``.agent/ansys.dyna.core/``
3. Pointer added to ``.github/copilot-instructions.md``
4. Manifest updated in ``.agent/manifest.json``
5. ``.agent/`` added to ``.gitignore``

Cursor
~~~~~~

Creates a self-contained Cursor rules file:

.. code-block:: bash

   python -m ansys.dyna.core agent --env cursor

**Output:** ``.cursor/rules/pydyna.mdc``

Cursor's MDC format includes metadata for when to apply rules:

.. code-block:: text

   ---
   description: PyDyna usage instructions for AI assistants
   globs: ["**/*.py", "**/*.k", "**/*.key"]
   alwaysApply: false
   ---

   [PyDyna instructions here]

Claude Code
~~~~~~~~~~~

Updates ``CLAUDE.md`` with PyDyna instructions:

.. code-block:: bash

   python -m ansys.dyna.core agent --env claude --copy

Similar to VS Code integration but uses Claude's instruction file conventions.

Generic Mode
~~~~~~~~~~~~

Works with any AI assistant tool:

.. code-block:: bash

   python -m ansys.dyna.core agent --copy

Creates the ``.agent/`` directory structure with all files. You can then configure 
your AI tool to read from ``.agent/ansys.dyna.core.md``.

The Manifest System
-------------------

The ``.agent/`` directory maintains a cross-tool registry of all installed agent 
instructions.

manifest.json
~~~~~~~~~~~~~

Machine-readable registry tracking installed packages:

.. code-block:: json

   {
     "version": "1.0",
     "packages": [
       {
         "namespace": "ansys.dyna.core",
         "ecosystem": "pypi",
         "package_name": "ansys-dyna-core",
         "entry_file": "ansys.dyna.core.md",
         "extended_docs": [
           "ansys.dyna.core/deck.md",
           "ansys.dyna.core/keywords.md",
           "ansys.dyna.core/patterns.md"
         ],
         "mode": "copy",
         "source": "/path/to/site-packages/ansys/dyna/core/AGENT.md",
         "installed_at": "2026-02-05T16:20:01Z"
       }
     ]
   }

README.md
~~~~~~~~~

Human and agent-readable index of installed packages:

.. code-block:: text

   # Agent Instructions

   ## Installed Packages

   | Namespace | Ecosystem | Package | Entry File |
   |-----------|-----------|---------|------------|
   | ansys.dyna.core | pypi | ansys-dyna-core | [ansys.dyna.core.md](ansys.dyna.core.md) |

Command Reference
-----------------

Print Instructions
~~~~~~~~~~~~~~~~~~

View instructions without installing:

.. code-block:: bash

   python -m ansys.dyna.core agent --print

Install Commands
~~~~~~~~~~~~~~~~

.. code-block:: bash

   # Copy mode (default)
   python -m ansys.dyna.core agent --copy
   python -m ansys.dyna.core agent --env vscode --copy
   python -m ansys.dyna.core agent --env cursor
   
   # Pointer mode
   python -m ansys.dyna.core agent --pointer
   python -m ansys.dyna.core agent --env vscode --pointer

Specify Workspace
~~~~~~~~~~~~~~~~~

By default, the command auto-detects your workspace by looking for ``.git`` or 
``pyproject.toml``. Override with:

.. code-block:: bash

   python -m ansys.dyna.core agent --workspace /path/to/workspace --copy

Usage Examples
--------------

Example 1: Setting up Copilot
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # In your project directory
   cd my-pydyna-project
   python -m ansys.dyna.core agent --env vscode --copy

Now when you work on PyDyna code, GitHub Copilot will have access to:

- How to load and manipulate Deck objects
- Available keyword classes and their usage
- Common patterns for geometry transformations
- Best practices and important warnings

Example 2: Multiple Projects with Pointer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you work on multiple PyDyna projects and want them to share instructions:

.. code-block:: bash

   cd ~/projects/project-a
   python -m ansys.dyna.core agent --env cursor --pointer
   
   cd ~/projects/project-b
   python -m ansys.dyna.core agent --env cursor --pointer

Both projects now point to the same installed instructions, saving disk space.

Example 3: Inspecting Available Instructions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # View instructions in terminal
   python -m ansys.dyna.core agent --print | less
   
   # Or in Python
   python -c "import ansys.dyna.core; print(ansys.dyna.core.AGENT_INSTRUCTIONS)"

Example 4: CI/CD Integration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In your CI/CD pipeline, use copy mode to ensure consistent instructions:

.. code-block:: yaml

   # .github/workflows/test.yml
   steps:
     - uses: actions/checkout@v4
     - name: Install dependencies
       run: pip install ansys-dyna-core
     - name: Setup AI instructions
       run: python -m ansys.dyna.core agent --copy
     # Now AI coding assistants have context if reviewing code

Best Practices
--------------

Version Control
~~~~~~~~~~~~~~~

**Recommended approach:**

- Add ``.agent/`` to ``.gitignore`` (done automatically)
- Add ``.github/copilot-instructions.md`` to ``.gitignore`` if using VS Code integration
- Let each developer install instructions locally
- Instructions stay version-matched with installed package

Updating Instructions
~~~~~~~~~~~~~~~~~~~~~

When you upgrade PyDyna, reinstall agent instructions:

.. code-block:: bash

   pip install --upgrade ansys-dyna-core
   python -m ansys.dyna.core agent --copy

The manifest tracks installation time, helping you identify stale instructions.

Team Collaboration
~~~~~~~~~~~~~~~~~~

For teams using AI assistants:

1. Document which mode (copy/pointer) your team uses
2. Add installation step to onboarding documentation
3. Consider adding to project setup scripts
4. Keep instructions updated when upgrading PyDyna

Troubleshooting
---------------

Command Not Found
~~~~~~~~~~~~~~~~~

If ``python -m ansys.dyna.core agent`` fails:

.. code-block:: bash

   # Verify PyDyna is installed
   python -c "import ansys.dyna.core; print(ansys.dyna.core.__version__)"
   
   # Check if agent instructions are available
   python -c "import ansys.dyna.core; print(ansys.dyna.core.AGENT_INSTRUCTIONS)"

Workspace Not Detected
~~~~~~~~~~~~~~~~~~~~~~

If automatic workspace detection fails:

.. code-block:: bash

   # Specify workspace explicitly
   python -m ansys.dyna.core agent --workspace /path/to/project --copy

Instructions Not Loaded by AI
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**For VS Code/Copilot:**

- Verify ``.github/copilot-instructions.md`` contains PyDyna pointer
- Reload VS Code window (Command Palette → "Developer: Reload Window")
- Check that ``.agent/ansys.dyna.core.md`` exists

**For Cursor:**

- Verify ``.cursor/rules/pydyna.mdc`` exists
- Restart Cursor
- Check rules are enabled in Cursor settings

**For any tool:**

- Confirm files were created with ``ls -la .agent/``
- Check file permissions are readable
- Verify manifest with ``cat .agent/manifest.json``

Stale Instructions
~~~~~~~~~~~~~~~~~~

After upgrading PyDyna, reinstall instructions:

.. code-block:: bash

   # Remove old instructions
   rm -rf .agent/ansys.dyna.core*
   
   # Reinstall
   python -m ansys.dyna.core agent --copy

