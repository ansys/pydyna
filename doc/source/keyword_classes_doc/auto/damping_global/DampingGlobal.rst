





:class:`DampingGlobal`
======================


.. py:class:: damping_global.DampingGlobal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DAMPING_GLOBAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DampingGlobal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID which specifies node system damping:
          * - :py:attr:`~valdmp`
            - Get or set the System damping constant. Only used if LCID is set to zero.
          * - :py:attr:`~stx`
            - Get or set the Scale factor on global x translational damping forces.
          * - :py:attr:`~sty`
            - Get or set the Scale factor on global y translational damping forces.
          * - :py:attr:`~stz`
            - Get or set the Scale factor on global z translational damping forces.
          * - :py:attr:`~srx`
            - Get or set the Scale factor on global x rotational damping moments.
          * - :py:attr:`~sry`
            - Get or set the Scale factor on global y rotational damping moments.
          * - :py:attr:`~srz`
            - Get or set the Scale factor on global z rotational damping moments.


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

    from damping_global import DampingGlobal

Property detail
---------------

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID which specifies node system damping:
   EQ.0: a contact damping factor as defined by VALDMP is used,
   EQ.n: system damping is given by load curve n. The damping force applied to each node is f=-d(t)mv, where d(t) is defined by load curve n.
















   ..
       !! processed by numpydoc !!

.. py:property:: valdmp
   :type: float


   
   Get or set the System damping constant. Only used if LCID is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: stx
   :type: float


   
   Get or set the Scale factor on global x translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: sty
   :type: float


   
   Get or set the Scale factor on global y translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: stz
   :type: float


   
   Get or set the Scale factor on global z translational damping forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: srx
   :type: float


   
   Get or set the Scale factor on global x rotational damping moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: sry
   :type: float


   
   Get or set the Scale factor on global y rotational damping moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: srz
   :type: float


   
   Get or set the Scale factor on global z rotational damping moments.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DAMPING'


.. py:attribute:: subkeyword
   :value: 'GLOBAL'






