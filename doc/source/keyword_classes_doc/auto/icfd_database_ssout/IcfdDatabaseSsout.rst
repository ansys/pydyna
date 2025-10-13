





:class:`IcfdDatabaseSsout`
==========================


.. py:class:: icfd_database_ssout.IcfdDatabaseSsout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_SSOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseSsout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~out`
            - Get or set the Determines if the solver should retrieve the pressure loads and how to output it:
          * - :py:attr:`~outdt`
            - Get or set the Frequency of the pressure extraction. If left as 0., the solver will extract the pressure of the fluid on the FSI boundary at every timestep which is (not recommended due to its high memory and calculation cost).
          * - :py:attr:`~lcidsf`
            - Get or set the Option load curve ID to apply a scale factor on the fluid pressure output.
          * - :py:attr:`~poff`
            - Get or set the Optional pressure offset on the fluid pressure output


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

    from icfd_database_ssout import IcfdDatabaseSsout

Property detail
---------------

.. py:property:: out
   :type: int


   
   Get or set the Determines if the solver should retrieve the pressure loads and how to output it:
   EQ.0:   Inactive
   EQ.1 : The fluid solver will collect the segment sets (see * SET_SEGMENT) that are part of a FSI boundary and retrieve the pressure for subsequent print out in icfd_pressegand icfd_lcsegid
















   ..
       !! processed by numpydoc !!

.. py:property:: outdt
   :type: int


   
   Get or set the Frequency of the pressure extraction. If left as 0., the solver will extract the pressure of the fluid on the FSI boundary at every timestep which is (not recommended due to its high memory and calculation cost).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidsf
   :type: Optional[int]


   
   Get or set the Option load curve ID to apply a scale factor on the fluid pressure output.
















   ..
       !! processed by numpydoc !!

.. py:property:: poff
   :type: float


   
   Get or set the Optional pressure offset on the fluid pressure output
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_SSOUT'






