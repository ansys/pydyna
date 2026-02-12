AI assistant integration
========================

PyDyna includes built-in usage instructions for AI coding assistants such as
GitHub Copilot, Cursor, and Claude Code. These instructions are installed with
the package and match the exact version of PyDyna in your environment.

The instructions help AI assistants generate accurate PyDyna code without
relying on external documentation.

.. contents::
   :local:
   :depth: 2


Overview
--------

PyDyna distributes structured AI instructions with the package. These files
describe how to use core APIs, keyword classes, deck manipulation, and common
usage patterns.

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


Quick start
-----------

Run the following command inside your project:

.. code-block:: bash

   python -m ansys.dyna.core agent --copy

To install for a specific tool:

**VS Code (GitHub Copilot)**

.. code-block:: bash

   python -m ansys.dyna.core agent --env vscode --copy

**Cursor**

.. code-block:: bash

   python -m ansys.dyna.core agent --env cursor --copy

**Claude Code**

.. code-block:: bash

   python -m ansys.dyna.core agent --env claude --copy


How it works
------------

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

Copy mode copies instruction files into your workspace.

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

Use copy mode when:

- Working in CI/CD
- Using sandboxed AI tools
- You want project-local documentation


Pointer mode
~~~~~~~~~~~~

Pointer mode references the installed package without copying files.

.. code-block:: bash

   python -m ansys.dyna.core agent --pointer

Use pointer mode when:

- The AI tool has full filesystem access
- You want instructions to update automatically after upgrades


Tool-specific behavior
----------------------

VS Code (GitHub Copilot)
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


Claude Code
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

- Package namespace and ecosystem (e.g., ``ansys.dyna.core``, ``pypi``)
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
- For VS Code, check that ``.github/copilot-instructions.md`` was created
- For Cursor, verify ``.cursor/rules/pydyna.mdc`` exists
- For Claude, check that ``CLAUDE.md`` contains the PyDyna section
