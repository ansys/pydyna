





:class:`ControlAcoustic`
========================


.. py:class:: control_acoustic.ControlAcoustic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ACOUSTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAcoustic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~macdvp`
            - Get or set the Calculate the nodal displacements and velocities of *MAT_‌ACOUSTIC volume elements for inclusion in d3plot and time-history files.


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

    from control_acoustic import ControlAcoustic

Property detail
---------------

.. py:property:: macdvp
   :type: int


   
   Get or set the Calculate the nodal displacements and velocities of *MAT_‌ACOUSTIC volume elements for inclusion in d3plot and time-history files.
   EQ.0:   Acoustic nodal motions will not be calculated
   EQ.1 : Acoustic nodal motions will be calculated
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ACOUSTIC'






