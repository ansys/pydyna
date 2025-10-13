





:class:`Contact1D`
==================


.. py:class:: contact_1d.Contact1D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_1D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Contact1D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsidr`
            - Get or set the Nodal set ID for the rebar nodes that slide along the concrete; see* SET_NODE
          * - :py:attr:`~nsidc`
            - Get or set the Nodal set ID for the concrete nodes that the rebar nodes may slide along; see* SET_NODE
          * - :py:attr:`~err`
            - Get or set the External radius of rebar.
          * - :py:attr:`~sigc`
            - Get or set the Compressive strength of concrete.
          * - :py:attr:`~gb`
            - Get or set the Bond shear modulus.
          * - :py:attr:`~smax`
            - Get or set the Maximum shear strain displacement.
          * - :py:attr:`~exp`
            - Get or set the Exponent in damage curve.


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

    from contact_1d import Contact1D

Property detail
---------------

.. py:property:: nsidr
   :type: Optional[int]


   
   Get or set the Nodal set ID for the rebar nodes that slide along the concrete; see* SET_NODE
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidc
   :type: Optional[int]


   
   Get or set the Nodal set ID for the concrete nodes that the rebar nodes may slide along; see* SET_NODE
















   ..
       !! processed by numpydoc !!

.. py:property:: err
   :type: float


   
   Get or set the External radius of rebar.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigc
   :type: float


   
   Get or set the Compressive strength of concrete.
















   ..
       !! processed by numpydoc !!

.. py:property:: gb
   :type: float


   
   Get or set the Bond shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: smax
   :type: float


   
   Get or set the Maximum shear strain displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: exp
   :type: float


   
   Get or set the Exponent in damage curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: '1D'






