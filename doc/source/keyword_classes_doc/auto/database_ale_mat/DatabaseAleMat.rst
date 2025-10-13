





:class:`DatabaseAleMat`
=======================


.. py:class:: database_ale_mat.DatabaseAleMat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_ALE_MAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseAleMat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Time interval between the outputs.
          * - :py:attr:`~boxlow`
            - Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
          * - :py:attr:`~boxup`
            - Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
          * - :py:attr:`~dtxy`
            - Get or set the Time interval between the extraction of *.xy files from datalemat.tmp


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

    from database_ale_mat import DatabaseAleMat

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Time interval between the outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxlow
   :type: Optional[int]


   
   Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
   the range while BOXUP is the upper bound. The series of
   volumes covered by the specified range of *DEFINE_BOX
   determines the mesh regions for which ALE material data are to be output.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxup
   :type: Optional[int]


   
   Get or set the Range of *DEFINE_BOX ids. BOXLOW is the lower bound for
   the range while BOXUP is the upper bound. The series of
   volumes covered by the specified range of *DEFINE_BOX
   determines the mesh regions for which ALE material data are to be output.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtxy
   :type: Optional[float]


   
   Get or set the Time interval between the extraction of *.xy files from datalemat.tmp
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'ALE_MAT'






