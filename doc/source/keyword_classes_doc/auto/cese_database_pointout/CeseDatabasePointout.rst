





:class:`CeseDatabasePointout`
=============================


.. py:class:: cese_database_pointout.CeseDatabasePointout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_DATABASE_POINTOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseDatabasePointout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Point Set ID.
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the CESE timestep will be used.
          * - :py:attr:`~pstype`
            - Get or set the Point Set type:
          * - :py:attr:`~vx`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~vy`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~vz`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~pid`
            - Get or set the Point ID.
          * - :py:attr:`~x`
            - Get or set the Point initial coordinates.
          * - :py:attr:`~y`
            - Get or set the Point initial coordinates.
          * - :py:attr:`~z`
            - Get or set the Point initial coordinates.


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

    from cese_database_pointout import CeseDatabasePointout

Property detail
---------------

.. py:property:: psid
   :type: int


   
   Get or set the Point Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the CESE timestep will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstype
   :type: int


   
   Get or set the Point Set type:
   EQ.0: Fixed points.
   EQ.1: Tracer points using prescribed velocity.
   EQ.2: Tracer points using fluid velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Point ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Point initial coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Point initial coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Point initial coordinates.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'DATABASE_POINTOUT'






