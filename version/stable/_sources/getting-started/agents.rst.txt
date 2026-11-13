AI assistant integration
========================

PyDyna includes built-in usage instructions for AI coding assistants such as
GitHub Copilot, Cursor, and Claude Code. These instructions are distributed
with the package and are version-aligned with the exact PyDyna version
installed in your environment.

These instructions help AI assistants generate accurate PyDyna code without
relying on external documentation.


Overview
--------

PyDyna distributes structured AI instruction files with the package. These
files describe how to use core APIs, keyword classes, deck manipulation, and
common usage patterns.

The instructions cover:

- **Deck operations**: Loading, filtering, modifying, and writing keyword files
- **Keyword usage**: Creating and modifying material, section, element, and other keywords
- **Data access patterns**: Direct attributes, DataFrames for nodes/elements, and special cases
- **Common workflows**: Load → Modify → Save, building from scratch, geometry transforms
- **Best practices**: Filtering strategies, validation, format control

You can install these instructions into your workspace so supported AI tools
can read them.

Benefits:

- Version-aligned with your installed PyDyna
- No dependency on external documentation
- Organized into focused, modular files
- Compatible with multiple AI tools


Prerequisites
-------------

Before installing agent instructions, ensure:

- PyDyna is installed: ``pip install ansys-dyna-core``
- You have an active AI coding assistant (GitHub Copilot, Cursor, or Claude Code)
- You are in your project directory or can specify the path with ``--workspace``

.. note::
   The agent instructions are version-specific. They match the installed version
   of PyDyna and should be regenerated after package upgrades.

Quick start
-----------

Navigate to your project directory and run:

.. code-block:: bash

   python -m ansys.dyna.core agent --copy

To install for a specific tool:

**VS code (GitHub Copilot)**

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy

**Cursor**

.. code-block:: bash

   python -m ansys.dyna.core agent --env cursor --copy

**Claude code**

.. code-block:: bash

   python -m ansys.dyna.core agent --env claude --copy

.. tip::

   If you do not specify ``--env``, the command uses generic mode, which
   creates ``.agent/`` files that work with any AI tool capable of reading
   local documentation.

Installation output
-------------------

After running the installation command, local files are created in your workspace:

**Main instruction file**
  Contains quick reference, common operations, and links to detailed guides.
  Located at ``.agent/ansys.dyna.core.md`` (or tool-specific location).

**Extended documentation**
  Three detailed guides in ``.agent/ansys.dyna.core/``:

  - ``deck.md``: Complete Deck class operations
  - ``keywords.md``: Keyword creation and data access patterns
  - ``patterns.md``: Full workflow examples

**Manifest file**
  ``.agent/manifest.json`` tracks all installed agent instruction packages,
  allowing multiple packages to coexist.

**Tool-specific files**
  Depending on your ``--env`` choice:

  - **VS Code**: ``.github/copilot-instructions.md`` (pointer to main file)
  - **Cursor**: ``.cursor/rules/pydyna.mdc`` (single self-contained file)
  - **Claude**: ``CLAUDE.md`` (appends pointer section)

Operational details
-------------------

PyDyna exposes the location of its AI instructions through Python:

.. code-block:: python

   import ansys.dyna.core
   print(ansys.dyna.core.AGENT_INSTRUCTIONS)

This returns the path to the installed instruction file. Tools that support
Python introspection can use this path directly.


Installation modes
------------------

Copy mode (default)
~~~~~~~~~~~~~~~~~~~

Copy mode copies instruction files into your workspace. This is the **recommended
mode** for most users.

.. code-block:: bash

   python -m ansys.dyna.core agent --copy

Created structure:

.. code-block:: text

   .agent/
   ├── manifest.json              # Tracks installed instruction packages
   ├── README.md                  # Auto-generated index
   ├── ansys.dyna.core.md         # Main PyDyna instructions
   └── ansys.dyna.core/
       ├── deck.md                # Detailed Deck class guide
       ├── keywords.md            # Keyword creation and patterns
       └── patterns.md            # Complete workflow examples
   .github/
   ├── copilot-instructions.md    # Pointer for GitHub Copilot (VS Code)

**Advantages:**

- Works with cloud-based AI tools
- Instructions remain available even if package is uninstalled
- No external dependencies for AI tools
- Suitable for CI/CD environments

**Use copy mode when:**

- You're unsure which mode to use (safest choice)
- Your AI tool runs in a restricted environment
- You want instructions to persist independently of the package
- Working in containerized or cloud development environments


Pointer mode
~~~~~~~~~~~~

Pointer mode creates references to the installed package without copying files.
Instructions are read directly from the PyDyna installation directory.

.. code-block:: bash

   python -m ansys.dyna.core agent --pointer

**Advantages:**

- Automatically reflects package upgrades
- Minimal disk space usage
- Single source of truth

**Requirements:**

- AI tool must have filesystem access to Python site-packages
- PyDyna must remain installed

**Use pointer mode when:**

- Your AI tool has full local filesystem access
- You want instructions to auto-update after ``pip upgrade``
- You frequently upgrade PyDyna and want latest instructions
- Disk space is a concern (though files are small)

Tool-specific behavior
----------------------

VS code (GitHub copilot)
~~~~~~~~~~~~~~~~~~~~~~~~

Updates:

- ``.agent/`` directory
- ``.github/copilot-instructions.md``

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy


Cursor
~~~~~~

Creates:

``.cursor/rules/pydyna.mdc``

.. code-block:: bash

   python -m ansys.dyna.core agent --env cursor --copy


Claude code
~~~~~~~~~~~

Updates:

``CLAUDE.md``

.. code-block:: bash

   python -m ansys.dyna.core agent --env claude --copy


Manifest
--------

The ``.agent/manifest.json`` file tracks installed instruction sets from all
packages in your workspace. This allows multiple packages to install agent
instructions without conflicts.

The manifest includes:

- Package ``namespace`` and ecosystem (example, ``ansys.dyna.core``, ``pypi``)
- Installation mode (``copy`` or ``pointer``)
- Entry file and extended documentation locations
- Installation timestamp

Example:

.. code-block:: json

   {
     "version": "1.0",
     "packages": [
       {
         "namespace": "ansys.dyna.core",
         "ecosystem": "pypi",
         "package_name": "ansys-dyna-core",
         "mode": "copy",
         "entry_file": "ansys.dyna.core.md",
         "extended_docs": ["ansys.dyna.core/deck.md", "ansys.dyna.core/keywords.md"],
         "installed_at": "2026-02-12T10:30:00Z"
       }
     ]
   }


Command reference
-----------------

Print instructions:

.. code-block:: bash

   python -m ansys.dyna.core agent --print

Specify workspace:

.. code-block:: bash

   python -m ansys.dyna.core agent --workspace /path/to/project --copy


Updating instructions
---------------------

After upgrading PyDyna:

.. code-block:: bash

   pip install --upgrade ansys-dyna-core
   python -m ansys.dyna.core agent --copy


Troubleshooting
---------------

Verify installation:

.. code-block:: bash

   python -c "import ansys.dyna.core; print(ansys.dyna.core.AGENT_INSTRUCTIONS)"
   ls -la .agent/

If your AI tool does not detect instructions:

- Confirm files were created in the expected location
- Check ``.agent/manifest.json`` to verify the package is registered
- Restart the editor or AI tool to reload instructions
- Verify the correct ``--env`` option was used for your tool
- For VS code, check that ``.github/copilot-instructions.md`` was created
- For Cursor, verify ``.cursor/rules/pydyna.mdc`` exists
- For Claude, check that ``CLAUDE.md`` contains the PyDyna section