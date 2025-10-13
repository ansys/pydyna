





:class:`EmControlSwitch`
========================


.. py:class:: em_control_switch.EmControlSwitch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_SWITCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlSwitch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load Curve ID.Negative values switch the solver off, positive values switch it back on.
          * - :py:attr:`~femcomp`
            - Get or set the Determines if FEM matrices are recomputed each time the EM solver is turned back on:
          * - :py:attr:`~bemcomp`
            - Get or set the Determines if BEM matrices are recomputed each time the EM solver is turned back on:


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

    from em_control_switch import EmControlSwitch

Property detail
---------------

.. py:property:: lcid
   :type: int


   
   Get or set the Load Curve ID.Negative values switch the solver off, positive values switch it back on.
















   ..
       !! processed by numpydoc !!

.. py:property:: femcomp
   :type: int


   
   Get or set the Determines if FEM matrices are recomputed each time the EM solver is turned back on:
   EQ.0: FEM matrices are recomputed
   EQ.1: FEM matrices are not recomputed
















   ..
       !! processed by numpydoc !!

.. py:property:: bemcomp
   :type: int


   
   Get or set the Determines if BEM matrices are recomputed each time the EM solver is turned back on:
   EQ.0 : BEM matrices are recomputed
   EQ.1 : BEM matrices are not recomputed
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SWITCH'






