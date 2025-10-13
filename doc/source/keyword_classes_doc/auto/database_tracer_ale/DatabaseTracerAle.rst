





:class:`DatabaseTracerAle`
==========================


.. py:class:: database_tracer_ale.DatabaseTracerAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_TRACER_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseTracerAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID defining the initial position of a tracer particle. See Remark 1
          * - :py:attr:`~track`
            - Get or set the Tracking option:
          * - :py:attr:`~ammgid`
            - Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 2
          * - :py:attr:`~hvbeg`
            - Get or set the The beginning index of element history variables to be output. See Remark 3
          * - :py:attr:`~hvend`
            - Get or set the The ending index of element history variables to be output. The number of extra history variables must be no more than 15, meaning HVEND-HVBEG=15.
          * - :py:attr:`~time`
            - Get or set the Start time for tracer particle activation


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

    from database_tracer_ale import DatabaseTracerAle

Property detail
---------------

.. py:property:: nid
   :type: int


   
   Get or set the Node ID defining the initial position of a tracer particle. See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: track
   :type: int


   
   Get or set the Tracking option:
   EQ.0:   particle follows material
   EQ.1: particle is fixed in space.
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgid
   :type: int


   
   Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: hvbeg
   :type: int


   
   Get or set the The beginning index of element history variables to be output. See Remark 3
















   ..
       !! processed by numpydoc !!

.. py:property:: hvend
   :type: int


   
   Get or set the The ending index of element history variables to be output. The number of extra history variables must be no more than 15, meaning HVEND-HVBEG=15.
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: float


   
   Get or set the Start time for tracer particle activation
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'TRACER_ALE'






