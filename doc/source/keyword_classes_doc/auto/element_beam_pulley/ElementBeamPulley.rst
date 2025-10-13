





:class:`ElementBeamPulley`
==========================


.. py:class:: element_beam_pulley.ElementBeamPulley(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEAM_PULLEY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBeamPulley

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~puid`
            - Get or set the Pulley ID. A unique number has to be used.
          * - :py:attr:`~bid1`
            - Get or set the Truss beam element 1 ID.
          * - :py:attr:`~bid2`
            - Get or set the Truss beam element 2 ID.
          * - :py:attr:`~pnid`
            - Get or set the Pulley node, NID.
          * - :py:attr:`~fd`
            - Get or set the Coulomb dynamic friction coefficient.
          * - :py:attr:`~fs`
            - Get or set the Optional Coulomb static friction coefficient.
          * - :py:attr:`~lmin`
            - Get or set the Minimum length, see notes below.
          * - :py:attr:`~dc`
            - Get or set the Optional decay constant to allow smooth transition between the static and dynamic friction coefficient.


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

    from element_beam_pulley import ElementBeamPulley

Property detail
---------------

.. py:property:: puid
   :type: int


   
   Get or set the Pulley ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: bid1
   :type: int


   
   Get or set the Truss beam element 1 ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: bid2
   :type: int


   
   Get or set the Truss beam element 2 ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pnid
   :type: int


   
   Get or set the Pulley node, NID.
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Coulomb dynamic friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Optional Coulomb static friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmin
   :type: float


   
   Get or set the Minimum length, see notes below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Optional decay constant to allow smooth transition between the static and dynamic friction coefficient.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEAM_PULLEY'






