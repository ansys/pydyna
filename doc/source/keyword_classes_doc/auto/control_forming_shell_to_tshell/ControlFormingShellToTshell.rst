





:class:`ControlFormingShellToTshell`
====================================


.. py:class:: control_forming_shell_to_tshell.ControlFormingShellToTshell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_SHELL_TO_TSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingShellToTshell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the thin shell elements.
          * - :py:attr:`~thick`
            - Get or set the Thickness of the thick shell elements.
          * - :py:attr:`~midsf`
            - Get or set the TSHELL’s mid-plane position definition (see Figure 0-1 and Remark 4):
          * - :py:attr:`~idsegb`
            - Get or set the Set ID of the segments to be generated at the bottom layer of the TSHELLs, which can be used for segment-based contact.  The bottom layer of the TSHELLs has an outward normal that points in the opposite direction to the positive normal side of thin shells
          * - :py:attr:`~idsegt`
            - Get or set the Set ID of the segments to be generated at the top layer of the TSHELLs, which can be used for segment-based contact.  The top side of a TSHELL has an outward normal that points in the same direction as the positive normal side of the thin shells


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

    from control_forming_shell_to_tshell import ControlFormingShellToTshell

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the thin shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Thickness of the thick shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: midsf
   :type: float


   
   Get or set the TSHELL’s mid-plane position definition (see Figure 0-1 and Remark 4):
   EQ.0:   Mid - plane is at thin shell surface.
   EQ.1 : Mid - plane is at one half of THICK above thin shell surface.
   EQ. - 1 : Mid - plane is at one half of THICK below thin shell surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: idsegb
   :type: Optional[float]


   
   Get or set the Set ID of the segments to be generated at the bottom layer of the TSHELLs, which can be used for segment-based contact.  The bottom layer of the TSHELLs has an outward normal that points in the opposite direction to the positive normal side of thin shells
















   ..
       !! processed by numpydoc !!

.. py:property:: idsegt
   :type: Optional[float]


   
   Get or set the Set ID of the segments to be generated at the top layer of the TSHELLs, which can be used for segment-based contact.  The top side of a TSHELL has an outward normal that points in the same direction as the positive normal side of the thin shells
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_SHELL_TO_TSHELL'






