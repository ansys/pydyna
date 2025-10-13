





:class:`ElementDiscreteSphere`
==============================


.. py:class:: element_discrete_sphere.ElementDiscreteSphere(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_DISCRETE_SPHERE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementDiscreteSphere

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID and Element ID are the same for the discrete shpher
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~mass`
            - Get or set the mass value.
          * - :py:attr:`~inertia`
            - Get or set the inertia value.
          * - :py:attr:`~radii`
            - Get or set the sphere radius.
          * - :py:attr:`~nid2`
            - Get or set the More than one element with the same PID, MASS, INERTIA, and RADIUS can be defined by setting this field without requiring additional cards. If set, NID2 is a node ID that must have a value greater than NID. Then, DES are defined for each node with an ID between NID and NID2 (including NID and NID2). If 0 or left blank, then only a DES for NID is specified.


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

    from element_discrete_sphere import ElementDiscreteSphere

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID and Element ID are the same for the discrete shpher
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: float


   
   Get or set the mass value.
















   ..
       !! processed by numpydoc !!

.. py:property:: inertia
   :type: float


   
   Get or set the inertia value.
















   ..
       !! processed by numpydoc !!

.. py:property:: radii
   :type: float


   
   Get or set the sphere radius.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the More than one element with the same PID, MASS, INERTIA, and RADIUS can be defined by setting this field without requiring additional cards. If set, NID2 is a node ID that must have a value greater than NID. Then, DES are defined for each node with an ID between NID and NID2 (including NID and NID2). If 0 or left blank, then only a DES for NID is specified.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'DISCRETE_SPHERE'






