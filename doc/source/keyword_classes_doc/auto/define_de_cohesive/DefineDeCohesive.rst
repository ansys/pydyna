





:class:`DefineDeCohesive`
=========================


.. py:class:: define_de_cohesive.DefineDeCohesive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_COHESIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeCohesive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Node set ID, part set ID or part ID defining DES with cohesive force.
          * - :py:attr:`~styp`
            - Get or set the SID type:
          * - :py:attr:`~gamma`
            - Get or set the Liquid surface tension.
          * - :py:attr:`~vol`
            - Get or set the Volume fraction.
          * - :py:attr:`~ang`
            - Get or set the Contact angle.
          * - :py:attr:`~gap`
            - Get or set the Spatial limit for the existence of liquid bridge between particles.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_de_cohesive import DefineDeCohesive

Property detail
---------------

.. py:property:: sid
   :type: int


   
   Get or set the Node set ID, part set ID or part ID defining DES with cohesive force.
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the SID type:
   EQ.0:   Node set
   EQ.1:   Part set
   EQ.2: Part.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Liquid surface tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: vol
   :type: float


   
   Get or set the Volume fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang
   :type: float


   
   Get or set the Contact angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: float


   
   Get or set the Spatial limit for the existence of liquid bridge between particles.
   A liquid bridge will exist when the distance between two particles is less or equal to min(GAP, drup)
   where drup is the rupture distance of the bridge automatically calculated by LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'DE_COHESIVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





