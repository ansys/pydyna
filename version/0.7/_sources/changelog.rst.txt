Release notes
#############

This document contains the release notes for the PyDyna project.

.. vale off

.. towncrier release notes start

`0.7.1 <https://github.com/ansys/pydyna/releases/tag/v0.7.1>`_ - 2025-02-25
===========================================================================

Added
^^^^^

- feat: Add .remove method to deck class (#681) `#682 <https://github.com/ansys/pydyna/pull/682>`_
- fix: BOUNDARY_PRESCRIBED_MOTION_*_ID option and CONTROL_MPP_DECOMPOSITION_TRANSFORMATION (Issue #696) `#701 <https://github.com/ansys/pydyna/pull/701>`_
- fix: Mat 196 (#707) `#708 <https://github.com/ansys/pydyna/pull/708>`_


Dependencies
^^^^^^^^^^^^

- build(deps): bump numpy from 2.1.3 to 2.2.2 `#670 <https://github.com/ansys/pydyna/pull/670>`_


Documentation
^^^^^^^^^^^^^

- build(deps): bump pyvista from 0.44.1 to 0.44.2 `#651 <https://github.com/ansys/pydyna/pull/651>`_
- build(deps): bump jupyterlab from 4.3.1 to 4.3.4 `#652 <https://github.com/ansys/pydyna/pull/652>`_
- build(deps): bump nbsphinx from 0.9.5 to 0.9.6 `#655 <https://github.com/ansys/pydyna/pull/655>`_
- build(deps): bump ansys-sphinx-theme from 1.2.2 to 1.2.6 `#665 <https://github.com/ansys/pydyna/pull/665>`_
- [pre-commit.ci] pre-commit autoupdate `#666 <https://github.com/ansys/pydyna/pull/666>`_
- fix: Options api rework `#671 <https://github.com/ansys/pydyna/pull/671>`_
- build(deps): bump ipython from 8.29.0 to 8.31.0 `#673 <https://github.com/ansys/pydyna/pull/673>`_
- build(deps): bump imageio from 2.36.1 to 2.37.0 `#674 <https://github.com/ansys/pydyna/pull/674>`_
- build(deps): bump scikit-learn from 1.6.0 to 1.6.1 `#675 <https://github.com/ansys/pydyna/pull/675>`_
- build(deps): bump sphinx-autodoc-typehints from 2.5.0 to 3.0.1 `#676 <https://github.com/ansys/pydyna/pull/676>`_
- build(deps): bump pypandoc from 1.14 to 1.15 `#677 <https://github.com/ansys/pydyna/pull/677>`_
- chore: [pre-commit.ci] pre-commit autoupdate `#678 <https://github.com/ansys/pydyna/pull/678>`_, `#743 <https://github.com/ansys/pydyna/pull/743>`_
- build(deps): bump imageio-ffmpeg from 0.5.1 to 0.6.0 `#694 <https://github.com/ansys/pydyna/pull/694>`_
- fix: Support multiline include `#699 <https://github.com/ansys/pydyna/pull/699>`_
- fix: SET_PART_LIST_GENERATE `#702 <https://github.com/ansys/pydyna/pull/702>`_
- Rename variable card `#703 <https://github.com/ansys/pydyna/pull/703>`_
- chore: add dependabot groups `#704 <https://github.com/ansys/pydyna/pull/704>`_
- chore: bump the doc group with 5 updates `#705 <https://github.com/ansys/pydyna/pull/705>`_
- fix: Assign series card property `#706 <https://github.com/ansys/pydyna/pull/706>`_
- feat: Assign fields for duplicate cards in constructor `#716 <https://github.com/ansys/pydyna/pull/716>`_
- chore: bump numpy from 2.2.2 to 2.2.3 `#722 <https://github.com/ansys/pydyna/pull/722>`_
- chore: pre-commit autoupdate `#724 <https://github.com/ansys/pydyna/pull/724>`_
- chore: bump the doc group across 1 directory with 2 updates `#727 <https://github.com/ansys/pydyna/pull/727>`_
- fix: Read parameters `#728 <https://github.com/ansys/pydyna/pull/728>`_
- fix: add test scenario for bug 584 `#729 <https://github.com/ansys/pydyna/pull/729>`_
- fix: Rework defaults `#730 <https://github.com/ansys/pydyna/pull/730>`_
- chore: bump the doc group with 3 updates `#741 <https://github.com/ansys/pydyna/pull/741>`_
- fix: Contact mpp fix `#744 <https://github.com/ansys/pydyna/pull/744>`_
- feat: add api to get keyword names `#745 <https://github.com/ansys/pydyna/pull/745>`_


Fixed
^^^^^

- fix: Fix BOUNDARY_PRESCRIBED_MOTION and CONSTRAINED_BEAM_IN_SOLID `#668 <https://github.com/ansys/pydyna/pull/668>`_
- fix: *CONSTRAINED_ADAPTIVITY (#650) `#683 <https://github.com/ansys/pydyna/pull/683>`_
- fix: deck.get() in the presence of Encrypted keywords `#720 <https://github.com/ansys/pydyna/pull/720>`_
- fix: mat295 iso `#725 <https://github.com/ansys/pydyna/pull/725>`_


Miscellaneous
^^^^^^^^^^^^^

- chore: update CHANGELOG for v0.7.0 `#661 <https://github.com/ansys/pydyna/pull/661>`_
- Release/0.7 `#662 <https://github.com/ansys/pydyna/pull/662>`_
- support single path in *INCLUDE_PATH in the `expand` method `#697 <https://github.com/ansys/pydyna/pull/697>`_
- feat: Start to handle *INCLUDE_TRANSFORM in Deck.expand() `#709 <https://github.com/ansys/pydyna/pull/709>`_
- feat: Import encrypted file `#712 <https://github.com/ansys/pydyna/pull/712>`_
- fix: Allow setting option fields to None `#713 <https://github.com/ansys/pydyna/pull/713>`_
- ci: use main version of the action for doc-style `#715 <https://github.com/ansys/pydyna/pull/715>`_
- add active func to codegen for duplicate card group `#718 <https://github.com/ansys/pydyna/pull/718>`_
- Default duplicate card `#731 <https://github.com/ansys/pydyna/pull/731>`_
- fix: Rename cards `#732 <https://github.com/ansys/pydyna/pull/732>`_
- fix: Contact options `#733 <https://github.com/ansys/pydyna/pull/733>`_
- Refactor codegen `#734 <https://github.com/ansys/pydyna/pull/734>`_
- Fix issues `#742 <https://github.com/ansys/pydyna/pull/742>`_


Test
^^^^

- feat: get by subkeyword and add transform links `#735 <https://github.com/ansys/pydyna/pull/735>`_
- feat: begin to handle *DEFINE_TRANSFORMATION when expanding include decks `#740 <https://github.com/ansys/pydyna/pull/740>`_

`0.7.0 <https://github.com/ansys/pydyna/releases/tag/v0.7.0>`_ - 2025-01-10
===========================================================================

Added
^^^^^

- fix: CONTROL_TIMESTEP and CONTROL_TIME_STEP (#629) `#631 <https://github.com/ansys/pydyna/pull/631>`_


Dependencies
^^^^^^^^^^^^

- build(deps): bump ansys-sphinx-theme from 1.2.1 to 1.2.2 `#630 <https://github.com/ansys/pydyna/pull/630>`_
- build(deps): bump matplotlib from 3.9.2 to 3.10.0 `#640 <https://github.com/ansys/pydyna/pull/640>`_
- build(deps): bump scikit-learn from 1.5.2 to 1.6.0 `#642 <https://github.com/ansys/pydyna/pull/642>`_


Documentation
^^^^^^^^^^^^^

- chore: update CHANGELOG for v0.6.1 `#627 <https://github.com/ansys/pydyna/pull/627>`_
- fix: update launcher,add argument to define ansys version `#632 <https://github.com/ansys/pydyna/pull/632>`_
- build(deps): bump pytest from 8.3.3 to 8.3.4 `#633 <https://github.com/ansys/pydyna/pull/633>`_
- build(deps): bump sphinx-autoapi from 3.3.3 to 3.4.0 `#635 <https://github.com/ansys/pydyna/pull/635>`_
- build(deps): bump imageio from 2.36.0 to 2.36.1 `#636 <https://github.com/ansys/pydyna/pull/636>`_
- build(deps): bump ansys-api-dyna from 0.4.1 to 0.4.2 `#637 <https://github.com/ansys/pydyna/pull/637>`_
- [pre-commit.ci] pre-commit autoupdate `#638 <https://github.com/ansys/pydyna/pull/638>`_
- add parameter set to lib `#644 <https://github.com/ansys/pydyna/pull/644>`_
- feat: support 3.13 `#645 <https://github.com/ansys/pydyna/pull/645>`_


Fixed
^^^^^

- fix: reading *CONTROL_DEBUG `#643 <https://github.com/ansys/pydyna/pull/643>`_
- fix run_dyna `#658 <https://github.com/ansys/pydyna/pull/658>`_


Miscellaneous
^^^^^^^^^^^^^

- fix: warn out of bounds characters when reading lines `#647 <https://github.com/ansys/pydyna/pull/647>`_
- test: add test for deepcopy `#648 <https://github.com/ansys/pydyna/pull/648>`_
- feat: Option to disable LSPP defaults `#649 <https://github.com/ansys/pydyna/pull/649>`_

`0.6.1 <https://github.com/ansys/pydyna/releases/tag/v0.6.1>`_ - 2024-11-22
===========================================================================

Documentation
^^^^^^^^^^^^^

- build(deps): bump jupyterlab from 4.3.0 to 4.3.1 `#618 <https://github.com/ansys/pydyna/pull/618>`_
- build(deps): bump ansys-sphinx-theme from 1.2.0 to 1.2.1 `#619 <https://github.com/ansys/pydyna/pull/619>`_
- build: allow Numpy 1.X `#626 <https://github.com/ansys/pydyna/pull/626>`_


Miscellaneous
^^^^^^^^^^^^^

- chore: update CHANGELOG for v0.6.0 `#625 <https://github.com/ansys/pydyna/pull/625>`_

`0.6.0 <https://github.com/ansys/pydyna/releases/tag/v0.6.0>`_ - 2024-11-20
===========================================================================

Added
^^^^^

- feat: changelog actions `#613 <https://github.com/ansys/pydyna/pull/613>`_


Miscellaneous
^^^^^^^^^^^^^

- fix: make pr-check-title independent `#624 <https://github.com/ansys/pydyna/pull/624>`_

.. vale on
