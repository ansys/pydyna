Release notes
#############

This document contains the release notes for the PyDyna project.

.. vale off

.. towncrier release notes start

`0.9.1 <https://github.com/ansys/pydyna/releases/tag/v0.9.1>`_ - December 31, 2025
==================================================================================

.. tab-set::


  .. tab-item:: Fixed

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - Migrating to \`\`ansys-tools-common\`\`
          - `#951 <https://github.com/ansys/pydyna/pull/951>`_


`0.9.0 <https://github.com/ansys/pydyna/releases/tag/v0.9.0>`_ - July 15, 2025
==============================================================================

.. tab-set::


  .. tab-item:: Added

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - general updates
          - `#792 <https://github.com/ansys/pydyna/pull/792>`_

        * - add windows exe parameter to run dyna
          - `#801 <https://github.com/ansys/pydyna/pull/801>`_

        * - build docs in python 3.13
          - `#814 <https://github.com/ansys/pydyna/pull/814>`_

        * - add run and keyword markers to composite action and matrix for keyword test
          - `#819 <https://github.com/ansys/pydyna/pull/819>`_

        * - add autosummary using jinja to all auto keywords files
          - `#820 <https://github.com/ansys/pydyna/pull/820>`_

        * - add options to activate with setter
          - `#846 <https://github.com/ansys/pydyna/pull/846>`_


  .. tab-item:: Fixed

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - feat: add license header
          - `#787 <https://github.com/ansys/pydyna/pull/787>`_

        * - add changelog fragments in PRs
          - `#793 <https://github.com/ansys/pydyna/pull/793>`_

        * - change example data url
          - `#796 <https://github.com/ansys/pydyna/pull/796>`_

        * - hide some implementation from run package and migrate to theme autoapi
          - `#797 <https://github.com/ansys/pydyna/pull/797>`_

        * - add additional doc strings to auto keyword 'jinja' template
          - `#803 <https://github.com/ansys/pydyna/pull/803>`_

        * - The nan comparison in series card test
          - `#804 <https://github.com/ansys/pydyna/pull/804>`_

        * - run module subprocess exit properly on failing
          - `#826 <https://github.com/ansys/pydyna/pull/826>`_

        * - check the working directory path before run subprocess
          - `#831 <https://github.com/ansys/pydyna/pull/831>`_

        * - badge
          - `#840 <https://github.com/ansys/pydyna/pull/840>`_


  .. tab-item:: Documentation

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - chore: bump numpy from 2.2.3 to 2.2.4
          - `#770 <https://github.com/ansys/pydyna/pull/770>`_

        * - chore: bump pytest-cov from 6.0.0 to 6.1.1
          - `#780 <https://github.com/ansys/pydyna/pull/780>`_

        * - chore: bump ipywidgets from 8.1.5 to 8.1.6
          - `#784 <https://github.com/ansys/pydyna/pull/784>`_

        * - docs: revert beer can buckling example
          - `#790 <https://github.com/ansys/pydyna/pull/790>`_

        * - add keywords to autoapi docs
          - `#800 <https://github.com/ansys/pydyna/pull/800>`_

        * - improve getting started documentation
          - `#802 <https://github.com/ansys/pydyna/pull/802>`_

        * - Add deep wiki badge
          - `#854 <https://github.com/ansys/pydyna/pull/854>`_


  .. tab-item:: Dependencies

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - chore: bump ansys/actions from 8 to 9
          - `#785 <https://github.com/ansys/pydyna/pull/785>`_

        * - chore: bump numpy from 2.2.4 to 2.2.5
          - `#789 <https://github.com/ansys/pydyna/pull/789>`_


  .. tab-item:: Maintenance

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - bump pyvista from 0.44.2 to 0.45.0
          - `#788 <https://github.com/ansys/pydyna/pull/788>`_

        * - bump the doc group across 1 directory with 3 updates
          - `#798 <https://github.com/ansys/pydyna/pull/798>`_, `#812 <https://github.com/ansys/pydyna/pull/812>`_

        * - bump ansys/actions from 9.0.2 to 9.0.6
          - `#799 <https://github.com/ansys/pydyna/pull/799>`_

        * - bump ipywidgets from 8.1.6 to 8.1.7
          - `#806 <https://github.com/ansys/pydyna/pull/806>`_

        * - bump joblib from 1.4.2 to 1.5.0
          - `#808 <https://github.com/ansys/pydyna/pull/808>`_

        * - bump ansys/actions from 9.0.6 to 9.0.7
          - `#809 <https://github.com/ansys/pydyna/pull/809>`_

        * - bump ansys/actions from 9.0.7 to 9.0.15
          - `#810 <https://github.com/ansys/pydyna/pull/810>`_

        * - bump pyvista from 0.45.0 to 0.45.1
          - `#811 <https://github.com/ansys/pydyna/pull/811>`_

        * - bump matplotlib from 3.10.1 to 3.10.3
          - `#813 <https://github.com/ansys/pydyna/pull/813>`_

        * - bump pyvista from 0.45.1 to 0.45.2
          - `#815 <https://github.com/ansys/pydyna/pull/815>`_

        * - bump numpy from 2.2.5 to 2.2.6
          - `#817 <https://github.com/ansys/pydyna/pull/817>`_

        * - bump actions/setup-python from 5.5.0 to 5.6.0
          - `#818 <https://github.com/ansys/pydyna/pull/818>`_

        * - bump joblib from 1.5.0 to 1.5.1
          - `#822 <https://github.com/ansys/pydyna/pull/822>`_

        * - bump ansys/actions from 9.0.9 to 9.0.12
          - `#823 <https://github.com/ansys/pydyna/pull/823>`_

        * - bump ansys/actions from 9.0.12 to 9.0.13
          - `#828 <https://github.com/ansys/pydyna/pull/828>`_

        * - bump pytest-xdist from 3.6.1 to 3.7.0
          - `#829 <https://github.com/ansys/pydyna/pull/829>`_

        * - bump pandas from 2.2.3 to 2.3.0
          - `#834 <https://github.com/ansys/pydyna/pull/834>`_

        * - bump scikit-learn from 1.6.1 to 1.7.0
          - `#836 <https://github.com/ansys/pydyna/pull/836>`_

        * - bump pytest-cov from 6.1.1 to 6.2.1
          - `#837 <https://github.com/ansys/pydyna/pull/837>`_

        * - bump pytest from 8.3.5 to 8.4.1
          - `#838 <https://github.com/ansys/pydyna/pull/838>`_

        * - pre-commit automatic update
          - `#839 <https://github.com/ansys/pydyna/pull/839>`_

        * - Bump ansys/actions from 9.0.13 to 10.0.12
          - `#844 <https://github.com/ansys/pydyna/pull/844>`_

        * - bump the doc group across 1 directory with 5 updates
          - `#845 <https://github.com/ansys/pydyna/pull/845>`_

        * - Bump the doc group with 2 updates
          - `#849 <https://github.com/ansys/pydyna/pull/849>`_

        * - Bump pytest-xdist from 3.7.0 to 3.8.0
          - `#850 <https://github.com/ansys/pydyna/pull/850>`_

        * - Pre-commit automatic update
          - `#851 <https://github.com/ansys/pydyna/pull/851>`_


  .. tab-item:: Miscellaneous

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - [pre-commit.ci] pre-commit autoupdate
          - `#773 <https://github.com/ansys/pydyna/pull/773>`_

        * - chore: update CHANGELOG for v0.8.0
          - `#775 <https://github.com/ansys/pydyna/pull/775>`_

        * - build: bump dev version after 0.8.0 release
          - `#776 <https://github.com/ansys/pydyna/pull/776>`_

        * - docs: Update ``CONTRIBUTORS.md`` with the latest contributors
          - `#779 <https://github.com/ansys/pydyna/pull/779>`_

        * - fix: Fix define table keyword
          - `#782 <https://github.com/ansys/pydyna/pull/782>`_

        * - test: add test case for icfd_part
          - `#783 <https://github.com/ansys/pydyna/pull/783>`_


`0.8.0 <https://github.com/ansys/pydyna/releases/tag/v0.8.0>`_ - April 02, 2025
===============================================================================

.. tab-set::


  .. tab-item:: Dependencies

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - chore: bump pytest from 8.3.4 to 8.3.5
          - `#763 <https://github.com/ansys/pydyna/pull/763>`_


  .. tab-item:: Documentation

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - chore: update version
          - `#748 <https://github.com/ansys/pydyna/pull/748>`_

        * - chore: bump matplotlib from 3.10.0 to 3.10.1
          - `#762 <https://github.com/ansys/pydyna/pull/762>`_

        * - chore: [pre-commit.ci] pre-commit autoupdate
          - `#764 <https://github.com/ansys/pydyna/pull/764>`_

        * - [pre-commit.ci] pre-commit autoupdate
          - `#766 <https://github.com/ansys/pydyna/pull/766>`_


  .. tab-item:: Fixed

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - fix: handle initial_temperature_node/set correct with table-card
          - `#754 <https://github.com/ansys/pydyna/pull/754>`_

        * - fix: part set list generate gap
          - `#760 <https://github.com/ansys/pydyna/pull/760>`_

        * - feat: remove pyvista hard dependency
          - `#774 <https://github.com/ansys/pydyna/pull/774>`_


  .. tab-item:: Miscellaneous

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - chore: update CHANGELOG for v0.7.1
          - `#746 <https://github.com/ansys/pydyna/pull/746>`_

        * - fix: encrypted deck in expand
          - `#751 <https://github.com/ansys/pydyna/pull/751>`_

        * - fix: try to detect encoding when expanding decks
          - `#753 <https://github.com/ansys/pydyna/pull/753>`_

        * - fix pass **args to plotter object
          - `#755 <https://github.com/ansys/pydyna/pull/755>`_

        * - fix: try utf-8 before attempt encoding detection
          - `#756 <https://github.com/ansys/pydyna/pull/756>`_

        * - fix: issue reading series card with trailing whitespace
          - `#759 <https://github.com/ansys/pydyna/pull/759>`_

        * - feat: add include name property to keyword
          - `#768 <https://github.com/ansys/pydyna/pull/768>`_


  .. tab-item:: Test

    .. list-table::
        :header-rows: 0
        :widths: auto

        * - fix: remove print statement from test_initial_temperature
          - `#757 <https://github.com/ansys/pydyna/pull/757>`_


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
