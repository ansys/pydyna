





:class:`DefineCurveFlc`
=======================


.. py:class:: define_curve_flc.DefineCurveFlc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_FLC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveFlc

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID.
          * - :py:attr:`~th`
            - Get or set the Sheet metal thickness
          * - :py:attr:`~n`
            - Get or set the Strain hardening value of the sheet metal, as in power law.
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

    from define_curve_flc import DefineCurveFlc

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: th
   :type: float


   
   Get or set the Sheet metal thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the Strain hardening value of the sheet metal, as in power law.
















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
   :value: 'CURVE_FLC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





