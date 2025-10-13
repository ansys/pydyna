





:class:`ControlImplicitResidualVector`
======================================


.. py:class:: control_implicit_residual_vector.ControlImplicitResidualVector(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_RESIDUAL_VECTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitResidualVector

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iresvec`
            - Get or set the Residual vector control flag:
          * - :py:attr:`~neig`
            - Get or set the Number of eigenmodes to compute for the purpose of orthogonalizing the computed load:
          * - :py:attr:`~iformat`
            - Get or set the Format for processing eigenmodes:
          * - :py:attr:`~rv_filenam`
            - Get or set the ENIG!=0 Name of the file for dumping the eigenvector


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

    from control_implicit_residual_vector import ControlImplicitResidualVector

Property detail
---------------

.. py:property:: iresvec
   :type: int


   
   Get or set the Residual vector control flag:
   EQ.0:   do not compute residual vectors.
   GT.0:   compute residual vectors.
















   ..
       !! processed by numpydoc !!

.. py:property:: neig
   :type: int


   
   Get or set the Number of eigenmodes to compute for the purpose of orthogonalizing the computed load:
   EQ.0:   read the eigenmodes from the file Eigen_‌Vectors which is the file used to for dumping eignevectors; see EVDUMP on *CONTROL_‌IMPLICIT_‌EIGENVALUE.
   GT.0:   compute NEIG eigenmodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: iformat
   :type: int


   
   Get or set the Format for processing eigenmodes:
   If NEIG = 0
   if IRESVEC > 0:
   read from Eigen_Vectors when NEIG = 0:
   LT.0:   file is in binary format.
   GT.0:   file is in ASCII format.
   Note that if IRSEVEC > 0 and NEIG = 0, IFORMAT = 0 is not allowed.
   If NEIG > 0:
   EQ.0:   do not dump the computed eigenmodes.
   LT.0:   dump the computed eigenmodes in binary format.
   GT.0:   dump the computed eignemodes in ASCII format.
















   ..
       !! processed by numpydoc !!

.. py:property:: rv_filenam
   :type: Optional[str]


   
   Get or set the ENIG!=0 Name of the file for dumping the eigenvector
   NEIG=0 Name of the file read to obtain the eigenvectors. See EVDUMP on *CONTROL_IMPLICIT_EIGENVALUE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_RESIDUAL_VECTOR'






