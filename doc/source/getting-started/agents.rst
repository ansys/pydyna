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
   ├── manifest.json
   ├── README.md
   ├── ansys.dyna.core.md
   └── ansys.dyna.core/
       ├── deck.md
       ├── keywords.md
       └── patterns.md
   .github/
   ├── copilot-instructions.md

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

The ``.agent/manifest.json`` file tracks installed instruction sets.

Example:

.. code-block:: json

   {
     "version": "1.0",
     "packages": [
       {
         "namespace": "ansys.dyna.core",
         "mode": "copy",
         "entry_file": "ansys.dyna.core.md"
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

- Confirm files were created
- Restart the editor
- Verify the correct ``--env`` option was used
