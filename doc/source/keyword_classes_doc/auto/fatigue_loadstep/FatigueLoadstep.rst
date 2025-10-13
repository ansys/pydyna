





:class:`FatigueLoadstep`
========================


.. py:class:: fatigue_loadstep.FatigueLoadstep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE_LOADSTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FatigueLoadstep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tstart`
            - Get or set the Start time of current load step
          * - :py:attr:`~tend`
            - Get or set the End time of current load step
          * - :py:attr:`~texpos`
            - Get or set the Exposure time of current load step


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

    from fatigue_loadstep import FatigueLoadstep

Property detail
---------------

.. py:property:: tstart
   :type: Optional[float]


   
   Get or set the Start time of current load step
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: Optional[float]


   
   Get or set the End time of current load step
















   ..
       !! processed by numpydoc !!

.. py:property:: texpos
   :type: float


   
   Get or set the Exposure time of current load step
   EQ.0.0: set to TEND-TSTART (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FATIGUE'


.. py:attribute:: subkeyword
   :value: 'LOADSTEP'






