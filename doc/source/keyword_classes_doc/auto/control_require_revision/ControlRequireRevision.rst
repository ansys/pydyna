





:class:`ControlRequireRevision`
===============================


.. py:class:: control_require_revision.ControlRequireRevision(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REQUIRE_REVISION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlRequireRevision

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~release`
            - Get or set the The release of code required. This should be a string such as "R6.1.0" or "R7.0".
          * - :py:attr:`~svnrev`
            - Get or set the The minimum SVN revision required (ignored by executables made after our move to git). This corresponds to the “SVN Version” field in the d3hsp file for versions of LS-DYNA prior to our move to git for version control.  R12.0 was the last release version made while we were using SVN for version control.
          * - :py:attr:`~gitrev`
            - Get or set the The minimum git revision required corresponding to the release of code (ignored by executables made prior to our move to git). This corresponds to part of the git hash given in the “Revision” field in the d3hsp file for versions of LS-DYNA after our move to git for version control. In d3hsp, the last string is “R[Release #]-[number]-[alphanumeric]”. If RELEASE is given, LS-DYNA compares GITREV to that [number]. If RELEASE is not given, see Remark 2.  R11.2 was the first release version made when we moved to git


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from control_require_revision import ControlRequireRevision

Property detail
---------------

.. py:property:: release
   :type: Optional[str]


   
   Get or set the The release of code required. This should be a string such as "R6.1.0" or "R7.0".
















   ..
       !! processed by numpydoc !!

.. py:property:: svnrev
   :type: Optional[int]


   
   Get or set the The minimum SVN revision required (ignored by executables made after our move to git). This corresponds to the “SVN Version” field in the d3hsp file for versions of LS-DYNA prior to our move to git for version control.  R12.0 was the last release version made while we were using SVN for version control.
















   ..
       !! processed by numpydoc !!

.. py:property:: gitrev
   :type: Optional[int]


   
   Get or set the The minimum git revision required corresponding to the release of code (ignored by executables made prior to our move to git). This corresponds to part of the git hash given in the “Revision” field in the d3hsp file for versions of LS-DYNA after our move to git for version control. In d3hsp, the last string is “R[Release #]-[number]-[alphanumeric]”. If RELEASE is given, LS-DYNA compares GITREV to that [number]. If RELEASE is not given, see Remark 2.  R11.2 was the first release version made when we moved to git
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REQUIRE_REVISION'






