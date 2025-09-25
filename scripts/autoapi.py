"""Automatically generate reStructuredText files for Python modules and classes."""

import ast
from pathlib import Path
import textwrap
from typing import Union

from numpydoc.docscrape import NumpyDocString


class ManualRSTGenerator:
    @staticmethod
    def get_base_name(base):
        if hasattr(base, "id"):
            return base.id
        elif hasattr(base, "attr"):
            parts = []
            while isinstance(base, ast.Attribute):
                parts.append(base.attr)
                base = base.value
            if hasattr(base, "id"):
                parts.append(base.id)
            return ".".join(reversed(parts))
        return None
    """Generates reStructuredText files for Python modules and classes."""

    def __init__(self, core_namespace, module_dir, doc_dir):
        """Initialize the generator.

        Parameters
        ----------
        core_namespace : str
            The namespace for the modules.
        module_dir : pathlib.Path
            Path to the directory containing the Python modules.
        doc_dir : pathlib.Path
            Path to the directory where to save the generated RST files.

        """
        self.core_namespace = core_namespace
        self.module_dir = module_dir
        self.doc_dir = doc_dir

    def generate_rst_for_manual_modules(self, auto_files):
        """Generate RST files for Python modules.

        Parameters
        ----------
        auto_files : list
            List of files to be excluded from RST generation.

        """
        auto_file_paths = [Path(f).resolve() for f in auto_files]

        for path in Path(self.module_dir).rglob("*.py"):
            path_resolved = path.resolve()
            is_autofile = any(
                auto_path in path_resolved.parents or auto_path == path_resolved for auto_path in auto_file_paths
            )
            is_private_file = path.name.startswith("_") and ("__init__") not in path.name
            is_internal_file = "internal" in path.parts
            if not is_autofile and not is_internal_file and not is_private_file:
                self._generate_rst_for_pymodule(str(path))

    def _wrap_python_code_snippets(self, unformatted_docstring: str):
        """Wrap Python code snippets in the docstring with code-block directive.

        Parameters
        ----------
        unformatted_docstring : str
            The unformatted docstring.

        """
        lines = unformatted_docstring.splitlines()
        formatted_lines = []

        in_snippet = False
        for line in lines:
            stripped = line.lstrip()
            indent_level = len(line) - len(stripped)

            if not in_snippet and indent_level > 0 and stripped:
                in_snippet = True
                formatted_lines.append(".. code-block:: python\n")
                formatted_lines.append("")

            elif in_snippet and indent_level == 0 and stripped:
                in_snippet = False

            formatted_lines.append(line)

        return "\n".join(formatted_lines).rstrip()

    def _generate_rst_for_pymodule(self, path_to_src_file: Union[str, Path]):
        """Generate RST file for a Python module.

        Parameters
        ----------
        path_to_src_file : str or pathlib.Path
            Path to the source file.
        """
        path_to_src_file = Path(path_to_src_file).resolve()
        module_dir_path = Path(self.module_dir).resolve()
        relative_path = path_to_src_file.relative_to(module_dir_path)

        if path_to_src_file.name == "__init__.py" and path_to_src_file.parent == module_dir_path:
            return

        relative_namespace = ".".join(relative_path.with_suffix("").parts).replace(".__init__", "")
        full_namespace = self.core_namespace + "." + relative_namespace
        containing_namespace = ".".join(full_namespace.split(".")[:-1])
        module_name = full_namespace.split(".")[-1]

        if path_to_src_file.name == "__init__.py":
            out_file_path = Path(self.doc_dir) / relative_path.parent.with_suffix(".rst")
        else:
            out_file_path = Path(self.doc_dir) / relative_path.with_suffix(".rst")

        out_file_path.parent.mkdir(parents=True, exist_ok=True)

        with (
            path_to_src_file.open("r", encoding="utf-8") as in_file,
            out_file_path.open("w", encoding="utf-8") as out_file,
        ):
            tree = ast.parse(in_file.read())

            submodules = []
            subpackages = []
            functions = []

            if path_to_src_file.name == "__init__.py":
                for entry in path_to_src_file.parent.iterdir():
                    is_private_entry = entry.name.startswith("_")
                    if (
                        entry.is_file()
                        and entry.suffix == ".py"
                        and entry.name != "__init__.py"
                        and not is_private_entry
                    ):
                        submodules.append(entry.stem)
                    elif entry.is_dir() and entry.name != "__pycache__":
                        subpackages.append(entry.name)

            type_definitions = [node for node in tree.body if isinstance(node, ast.ClassDef)]
            enums = sorted(
                [
                    td
                    for td in type_definitions
                    if any(getattr(base, "id", "") in ("IntEnum", "IntFlag") for base in td.bases)
                ],
                key=lambda x: x.name,
            )
            interfaces = sorted(
                [td for td in type_definitions if td not in enums and td.name.startswith("I")], key=lambda x: x.name
            )
            classes = sorted(
                [
                    td
                    for td in type_definitions
                    if td not in enums and not td.name.startswith("_") and td not in interfaces
                ],
                key=lambda x: x.name,
            )

            function_definitions = [node for node in tree.body if isinstance(node, ast.FunctionDef)]
            functions = sorted(
                [func_def for func_def in function_definitions if not func_def.name.startswith("_")],
                key=lambda x: x.name,
            )

            def write_line(lines):
                out_file.writelines([line + "\n" for line in lines])

            write_line(
                [
                    f"The ``{module_name}`` module",
                    "=" * (len(module_name) + 20),
                    "",
                    f".. py:module:: {containing_namespace}.{module_name}",
                    "",
                ]
            )

            if any([subpackages, submodules, interfaces, classes, enums, functions]):
                write_line(["Summary", "-------", "", ".. tab-set::", ""])

                def write_list_tab(title, items, formatter):
                    write_line(
                        [
                            f"    .. tab-item:: {title}",
                            "",
                            "        .. list-table::",
                            "            :header-rows: 0",
                            "            :widths: auto",
                            "",
                        ]
                    )
                    for item in items:
                        write_line([f"            * - {formatter(item)}", ""])

                if subpackages:
                    write_list_tab(
                        "Subpackages", subpackages, lambda sp: f":py:obj:`~{containing_namespace}.{module_name}.{sp}`"
                    )

                if submodules:
                    write_list_tab(
                        "Submodules", submodules, lambda sm: f":py:obj:`~{containing_namespace}.{module_name}.{sm}`"
                    )

                for def_type, defs in [
                    ("Interfaces", interfaces),
                    ("Classes", classes),
                    ("Enums", enums),
                    ("Functions", functions),
                ]:
                    if defs:
                        tag = ":py:class:" if def_type != "Functions" else ":py:func:"
                        write_list_tab(
                            def_type,
                            defs,
                            lambda d: f"{tag}`~{containing_namespace}.{module_name}.{d.name}`"
                            + (
                                f"\n              - {ast.get_docstring(d).splitlines()[0]}"
                                if ast.get_docstring(d)
                                else ""
                            ),
                        )

            write_line(["Description", "-----------", ""])
            if (
                len(tree.body) > 0
                and isinstance(tree.body[0], ast.Expr)
                and isinstance(tree.body[0].value, ast.Constant)
            ):
                write_line([tree.body[0].value.value.strip(), ""])

            write_line([f".. py:currentmodule:: {containing_namespace}.{module_name}", "", ".. TABLE OF CONTENTS", ""])

            def write_toc_block(items, symbol, folder_name):
                if items:
                    write_line([".. toctree::", "    :titlesonly:", "    :maxdepth: 1", "    :hidden:", ""])
                    for item in items:
                        write_line([f"     <span class=\"{symbol}\"></span> {item}<{module_name}/{item}>"])

            write_toc_block(subpackages, "nf nf-md-package", "subpackage")
            write_toc_block(submodules, "nf nf-fa-file", "submodule")

            if any([classes, interfaces, enums, functions]):
                for def_type, defs in [
                    ("Interfaces", interfaces),
                    ("Classes", classes),
                    ("Enums", enums),
                    ("Functions", functions),
                ]:
                    if defs:
                        symbol = {"Interfaces": "nf nf-cod-symbol_class", "Classes": "nf nf-cod-symbol_class", "Enums": "nf nf-cod-symbol_enum", "Functions": "nf nf-md-function_variant"}[def_type]
                        write_line([".. toctree::", "    :titlesonly:", "    :maxdepth: 1", "    :hidden:", ""])
                        for d in defs:
                            write_line([f"     <span class=\"{symbol}\"></span> {d.name}<{module_name}/{d.name}>"])

            for obj in classes + interfaces:
                self._generate_rst_for_pyobj(obj, containing_namespace, module_name, str(out_file_path))

            for enum in enums:
                self._generate_rst_for_pyenum(enum, containing_namespace, module_name, str(out_file_path))
                

            for func in functions:
                self._generate_rst_for_pyfunc(func, containing_namespace, module_name, str(out_file_path))

    @staticmethod
    def _parse_args(method):
        args = []
        defaults = list(method.args.defaults or [])
        default_offset = len(method.args.args) - len(defaults)

        for i, arg in enumerate(method.args.args):
            arg_str = arg.arg
            annotation = getattr(arg, "annotation", None)
            if (
                isinstance(annotation, ast.Subscript)
                and hasattr(annotation.value, "attr")
                and annotation.value.attr == "Callable"
            ):
                # Defensive: handle ast.Name and ast.Attribute in callable arg types
                callable_args = []
                for elt in getattr(annotation.slice.dims[0], "elts", []):
                    if hasattr(elt, "id"):
                        callable_args.append(elt.id)
                    elif hasattr(elt, "attr"):
                        # ast.Attribute: get full name
                        parts = []
                        while isinstance(elt, ast.Attribute):
                            parts.append(elt.attr)
                            elt = elt.value
                        if hasattr(elt, "id"):
                            parts.append(elt.id)
                        callable_args.append(".".join(reversed(parts)))
                formatted_callable_arg_types = ", ".join(callable_args)
                callable_return_type = getattr(annotation.slice.dims[1], "id", None)
                arg_str += f": collections.abc.Callable[[{formatted_callable_arg_types}], {callable_return_type}]"
            elif isinstance(annotation, ast.Subscript):
                # Defensive: handle ast.Name and ast.Attribute
                type_id = getattr(annotation.value, "id", None)
                if not type_id and hasattr(annotation.value, "attr"):
                    parts = []
                    val = annotation.value
                    while isinstance(val, ast.Attribute):
                        parts.append(val.attr)
                        val = val.value
                    if hasattr(val, "id"):
                        parts.append(val.id)
                    type_id = ".".join(reversed(parts))
                arg_str += f": {type_id.lower() if type_id else ''}[{ManualRSTGenerator._parse_nested_type(annotation.slice)}]"
            elif annotation is not None:
                arg_str += f": {ManualRSTGenerator._parse_nested_type(annotation)}"
            if i >= default_offset and defaults:
                default = defaults[i - default_offset]
                if isinstance(default, ast.Constant):
                    arg_str += f" = {default.value!r}"
            args.append(arg_str)
        return ", ".join(args)

    @staticmethod
    def _parse_nested_type(annotation):
        type_string = ""
        while hasattr(annotation, "attr"):
            type_string = f"{annotation.attr}.{type_string}"
            if hasattr(annotation, "value"):
                annotation = annotation.value
            else:
                break
        if hasattr(annotation, "id"):
            type_string = f"{annotation.id}.{type_string}"
        return "~" + type_string.strip(".")

    @staticmethod
    def _parse_return_type(node):
        if isinstance(node, ast.Constant):
            return ["None"]
        if isinstance(node, ast.Subscript):
            elts = getattr(node.slice, "elts", None)
            if elts:
                return [ManualRSTGenerator._parse_nested_type(elt) for elt in elts]
        if node:
            return [ManualRSTGenerator._parse_nested_type(node)]
        return []

    def _generate_rst_for_pyobj(self, obj_definition, containing_namespace, module_name, module_rst_file_path):
        """Generate RST file for a Python object (class or interface).

        Parameters
        ----------
        obj_definition : ast.ClassDef
            Object definition in the AST.
        containing_namespace : str
            The namespace containing the object.
        module_name : str
            Name of the module.
        module_rst_file_path : str
            Path to the module RST file.

        Raises
        ------
        RuntimeError
            If a type hint does not have the proper structure.

        """
        out_dir = Path(module_rst_file_path).parent.resolve() / module_name
        out_path = out_dir / f"{obj_definition.name}.rst"
        out_dir.mkdir(parents=True, exist_ok=True)

        fq_name = f"{containing_namespace}.{module_name}.{obj_definition.name}"
        base_classes = ", ".join(filter(None, (ManualRSTGenerator.get_base_name(base) for base in obj_definition.bases)))

        def write_docstring(f, docstring, indent="   "):
            if docstring:
                formatted = self._wrap_python_code_snippets(docstring)
                f.write(textwrap.indent(formatted, indent) + "\n\n")

        def write_summary_table(f, title, items, ref_type):
            if not items:
                return
            f.writelines(
                [
                    ".. tab-set::\n\n",
                    f"    .. tab-item:: {title}\n\n",
                    "        .. list-table::\n",
                    "            :header-rows: 0\n",
                    "            :widths: auto\n\n",
                ]
            )
            for item in items:
                f.write(f"            * - :py:{ref_type}:`~{fq_name}.{item.name}`\n")
                doc = ast.get_docstring(item)
                if doc:
                    f.write(f"              - {doc.splitlines()[0]}\n")
            f.write("\n")

        with out_path.open("w", encoding="utf-8") as f:
            f.writelines(
                [
                    f"{obj_definition.name}\n",
                    "=" * len(obj_definition.name) + "\n\n",
                    f".. py:class:: {fq_name}\n\n",
                    f"   {base_classes}\n\n",
                ]
            )
            write_docstring(f, ast.get_docstring(obj_definition))
            f.write(f".. py:currentmodule:: {obj_definition.name}\n\n\n")

            methods = [m for m in obj_definition.body if isinstance(m, ast.FunctionDef) and not m.name.startswith("_")]
            props = [m for m in methods if any(getattr(d, "id", None) == "property" for d in m.decorator_list)]
            setters = [m for m in methods if any(getattr(d, "attr", None) == "setter" for d in m.decorator_list)]
            methods = [m for m in methods if m not in props and m not in setters]

            if props or methods:
                f.write("Overview\n--------\n\n")
            write_summary_table(f, "Methods", methods, "attr")
            write_summary_table(f, "Properties", props, "attr")

            f.writelines(
                [
                    "Import detail\n-------------\n\n",
                    ".. code-block:: python\n\n",
                    f"    from {containing_namespace}.{module_name} import {obj_definition.name}\n\n\n",
                ]
            )

            if props:
                f.write("Property detail\n---------------\n\n")
                for p in props:
                    try:
                        ret_type = ManualRSTGenerator._parse_return_type(p.returns)
                    except RuntimeError as e:
                        ret_type = "Unknown"
                    f.writelines(
                        [
                            f".. py:property:: {p.name}\n",
                            f"    :canonical: {fq_name}.{p.name}\n",
                            f"    :type: {ret_type}\n\n",
                        ]
                    )
                    write_docstring(f, ast.get_docstring(p), "    ")



            if methods:
                f.write("Method detail\n-------------\n\n")
                for m in methods:
                    arg_str = ManualRSTGenerator._parse_args(m)
                    ret_type = ManualRSTGenerator._parse_return_type(m.returns)
                    f.writelines(
                        [
                            f".. py:method:: {m.name}({arg_str})",
                            f"{' -> ' + ', '.join(ret_type) if ret_type else ''}\n",
                            f"    :canonical: {fq_name}.{m.name}\n\n",
                        ]
                    )
                    rawdocstring = ast.get_docstring(m)
                    docstring = None
                    if rawdocstring:
                        docstring = NumpyDocString(rawdocstring)

                    if docstring:
                        if "Summary" in docstring:
                            f.write(textwrap.indent("\n".join(docstring["Summary"]), "    ") + "\n\n")
                        if "Extended Summary" in docstring:
                            f.write(textwrap.indent("\n".join(docstring["Extended Summary"]), "    ") + "\n\n")

                    if m.args.args:
                        f.write("    :Parameters:\n\n")
                        if docstring and "Parameters" in docstring:
                            for param in docstring["Parameters"]:
                                if len(param.desc) > 0:
                                    if "of" in param.type:
                                        param_types = param.type.split()
                                        if len(param_types) == 3:
                                            f.write(
                                                f"        **{param.name}** : :obj:`~{param_types[0]}` of :obj:`~{param_types[2]}`\n"
                                            )
                                        else:
                                            raise RuntimeError(
                                                "Improper format for parameter containing 'of'- expecting `type` 'of' `type`."
                                            )
                                    else:
                                        f.write(f"        **{param.name}** : :obj:`~{param.type}`\n")
                                    f.write(textwrap.indent("\n".join(param.desc), "        ") + "\n")
                                    f.write("\n")
                            f.write("\n")
                        f.write("\n")

                    if ret_type:
                        f.write("    :Returns:\n\n")
                        # If multiple return types, output each separately
                        for i in range(len(ret_type)):
                            if docstring and "Returns" in docstring and len(docstring["Returns"]) > i:
                                ret = docstring["Returns"][i]
                                # Output each return value as a separate entry
                                f.write(f"        {ret.name} : :obj:`~{ret.type}`\n")
                                f.write(textwrap.indent("\n".join(ret.desc), "        ") + "\n")
                                f.write("\n")
                            else:
                                # Fallback if no name, just type
                                f.write(f"        :obj:`~{ret_type[i]}`\n\n")
            f.write("\n")

    def _generate_rst_for_pyfunc(self, func_def, namespace, module_name, module_rst_path):
        """Generate RST file for a Python function.

        Parameters
        ----------
        func_def : ast.FunctionDef
            Function definition in the AST.
        namespace : str
            Namespace containing the function.
        module_name : str
            Name of the module.
        module_rst_path : str
            Path to the module RST file.

        Raises
        ------
        RuntimeError
            If a type hint does not have the proper structure.
        """
        output_dir = Path(module_rst_path).parent.resolve() / module_name
        output_dir.mkdir(parents=True, exist_ok=True)

        out_path = output_dir / f"{func_def.name}.rst"
        with out_path.open("w", encoding="utf-8") as f:
            # Header and class declaration
            f.writelines(
                [
                    f"{func_def.name}\n",
                    f"{'=' * len(func_def.name)}\n\n",
                ]
            )

            # # For graphs, insert test image
            # graph_module_list = [
            #     "access_graphs",
            #     "aircraft_graphs",
            #     "antenna_graphs",
            #     "area_target_graphs",
            #     "chain_graphs",
            #     "comm_system_graphs",
            #     "coverage_definition_graphs",
            #     "facility_graphs",
            #     "figure_of_merit_graphs",
            #     "ground_vehicle_graphs",
            #     "launch_vehicle_graphs",
            #     "line_target_graphs",
            #     "missile_graphs",
            #     "place_graphs",
            #     "radar_graphs",
            #     "receiver_graphs",
            #     "satellite_graphs",
            #     "sensor_graphs",
            #     "ship_graphs",
            #     "target_graphs",
            #     "transmitter_graphs",
            #     "scenario_graphs",
            # ]
            # # Exclude images for untested graphs to avoid broken links
            # exclude_image_functions = [
            #     "tle_teme_residuals_line_chart",
            #     "radar_propagation_loss_line_chart",
            #     "flight_profile_by_downrange_line_chart",
            #     "flight_profile_by_time_line_chart",
            #     "angle_between_line_chart",
            #     "bentpipe_link_cno_line_chart",
            # ]
            # # Images generated once, but not generated as part of testing
            # shared_image_functions = [
            #     "model_area_line_chart",
            #     "solar_panel_area_line_chart",
            #     "solar_panel_power_line_chart",
            #     "obscuration_line_chart",
            # ]
            # # Substitute missing graphs of the same type
            # substitute_graph_key = {
            #     "sunlight_intervals_interval_pie_chart_launchvehicle": "sunlight_intervals_interval_pie_chart_satellite",
            #     "sunlight_intervals_interval_pie_chart_missile": "sunlight_intervals_interval_pie_chart_satellite",
            # }
            # class_name = module_name.replace("_graphs", "").replace("_", "")
            # if module_name in graph_module_list and func_def.name not in exclude_image_functions:
            #     func_module_path = f"{func_def.name}_{class_name}"
            #     if func_def.name in shared_image_functions:
            #         graph_image_path = f"/graph_images_temp/{func_def.name}.png"
            #     elif func_module_path in substitute_graph_key:
            #         substitute_path = substitute_graph_key[func_module_path]
            #         graph_image_path = f"/graph_images_temp/test_{substitute_path}.png"
            #     else:
            #         graph_image_path = f"/graph_images_temp/test_{func_module_path}.png"
            #     f.writelines(
            #         [
            #             f".. image:: {graph_image_path}\n",
            #             "  :width: 600\n",
            #             f"  :alt: image of output from {func_def.name}\n\n",
            #         ]
            #     )

            arg_str = ManualRSTGenerator._parse_args(func_def)
            ret_type = ManualRSTGenerator._parse_return_type(func_def.returns)
            fq_name = f"{namespace}.{module_name}.{func_def.name}"
            f.writelines(
                [
                    f".. py:function:: {fq_name}({arg_str})",
                    f"{' -> ' + ', '.join(ret_type) if ret_type else ''}\n",
                    f"    :canonical: {fq_name}\n\n",
                ]
            )

            # Function docstring
            rawdocstring = ast.get_docstring(func_def)
            docstring = None
            if rawdocstring:
                docstring = NumpyDocString(rawdocstring)

            if docstring:
                if "Summary" in docstring:
                    f.write(textwrap.indent("\n".join(docstring["Summary"]), "    ") + "\n\n")
                if "Extended Summary" in docstring:
                    f.write(textwrap.indent("\n".join(docstring["Extended Summary"]), "    ") + "\n\n")

            if func_def.args.args:
                f.write("    :Parameters:\n\n")
                if docstring and "Parameters" in docstring:
                    for param in docstring["Parameters"]:
                        if len(param.desc) > 0:
                            if "Callable" in param.type:
                                callable_return_type = param.type.rsplit(", ")[-1].strip("]")
                                callable_parameter_types = param.type.split("[[")[-1].split("]")[0].split(", ")
                                formatted_callable_parameter_types = ", ".join(
                                    [f":obj:`~{type}`" for type in callable_parameter_types]
                                )
                                f.write(
                                    f"        **{param.name}** : :obj:`~collections.abc.Callable` [[{formatted_callable_parameter_types}], :obj:`~{callable_return_type}`]\n"
                                )
                            elif "of" in param.type:
                                param_types = param.type.split()
                                if len(param_types) == 3:
                                    f.write(
                                        f"        **{param.name}** : :obj:`~{param_types[0]}` of :obj:`~{param_types[2]}`\n"
                                    )
                                else:
                                    raise RuntimeError(
                                        "Improper format for parameter containing 'of'- expecting `type` 'of' `type`."
                                    )
                            else:
                                f.write(f"        **{param.name}** : :obj:`~{param.type}`\n")
                            f.write(textwrap.indent("\n".join(param.desc), "        ") + "\n")
                            f.write("\n")
                    f.write("\n")
                f.write("\n")

            if ret_type:
                f.write("    :Returns:\n\n")
                for i in range(len(ret_type)):
                    ret = ret_type[i]
                    if docstring and "Returns" in docstring and len(docstring["Returns"]) >= i:
                        ret = docstring["Returns"][i]
                        f.write(f"        :obj:`~{ret.type}`\n")
                        f.write(textwrap.indent("\n".join(ret.desc), "        ") + "\n")
                    f.write("\n")

            if docstring and "Raises" in docstring and len(docstring["Raises"]) == 1:
                ret = docstring["Raises"][0]
                f.writelines(
                    [
                        "    :Raises:\n\n",
                        f"        :obj:`~{ret.type}`\n",
                        textwrap.indent("\n".join(ret.desc), "        ") + "\n",
                    ]
                )
            f.write("\n")

            if docstring and "Examples" in docstring and len(docstring["Examples"]) > 0:
                f.write("    :Examples:\n\n")
                in_code_block = False
                for example_line in docstring["Examples"]:
                    if in_code_block and not example_line.startswith(">>>"):
                        in_code_block = False
                        f.write("\n\n")
                    if not in_code_block and example_line.startswith(">>> "):
                        in_code_block = True
                        f.write("\n      .. code-block:: python\n\n")
                    if in_code_block:
                        f.write(f"{textwrap.indent(example_line[len('>>> ') :], '        ')}\n")
                    else:
                        f.write(f"{textwrap.indent(example_line, '      ')}\n")
                f.write("\n")

            # Set current module context
            f.writelines([f".. py:currentmodule:: {func_def.name}\n\n\n"])

            # Import statement
            f.writelines(
                [
                    "Import detail\n",
                    "-------------\n\n",
                    ".. code-block:: python\n\n",
                    f"    from {namespace}.{module_name} import {func_def.name}\n\n\n",
                ]
            )

    def _generate_rst_for_pyenum(self, enum_def, namespace, module_name, module_rst_path):
        """Generate RST file for a Python enum.

        Parameters
        ----------
        enum_def : ast.ClassDef
            Enum definition in the AST.
        namespace : str
            Namespace containing the enum.
        module_name : str
            Name of the module.
        module_rst_path : str
            Path to the module RST file.

        """
        output_dir = module_rst_path.parent.resolve() / module_name
        output_dir.mkdir(parents=True, exist_ok=True)

        out_path = output_dir / f"{enum_def.name}.rst"
        with out_path.open("w", encoding="utf-8") as f:
            # Header and class declaration
            f.writelines(
                [
                    f"{enum_def.name}\n",
                    f"{'=' * len(enum_def.name)}\n\n",
                    f".. py:class:: {namespace}.{module_name}.{enum_def.name}\n\n",
                    f"   {', '.join(base.id for base in enum_def.bases)}\n\n",
                ]
            )

            # Class docstring
            docstring = ast.get_docstring(enum_def)
            if docstring:
                f.write(f"{textwrap.indent(docstring, '   ')}\n\n")

            # Set current module context
            f.writelines([f".. py:currentmodule:: {enum_def.name}\n\n\n"])

            # Extract enum members and their docstrings
            members = []
            for i, node in enumerate(enum_def.body):
                if isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Name):
                    name = node.targets[0].id
                    value = node.value.value if isinstance(node.value, ast.Constant) else None
                    doc = None
                    if i + 1 < len(enum_def.body):
                        next_node = enum_def.body[i + 1]
                        if isinstance(next_node, ast.Expr) and isinstance(next_node.value, ast.Constant):
                            doc = next_node.value.value
                    members.append({"name": name, "value": value, "doc": doc})

            # Render enum member table if present
            if members:
                f.writelines(
                    [
                        "Overview\n",
                        "--------\n\n",
                        ".. tab-set::\n\n",
                        "    .. tab-item:: Members\n\n",
                        "        .. list-table::\n",
                        "            :header-rows: 0\n",
                        "            :widths: auto\n\n",
                    ]
                )
                for m in members:
                    f.write(f"            * - :py:attr:~{m['name']}\n")
                    if m["doc"]:
                        lines = m["doc"].splitlines()
                        f.write(f"              - {lines[0]}\n")
                        for line in lines[1:]:
                            f.write(f"                {line}\n")
                f.write("\n")

            # Import statement
            f.writelines(
                [
                    "Import detail\n",
                    "-------------\n\n",
                    ".. code-block:: python\n\n",
                    f"    from {namespace}.{module_name} import {enum_def.name}\n\n\n",
                ]
            )


def autodoc_extensions():
    """Automatically generate RST files for the extensions package."""
    namespace = "ansys.dyna.core"
    module_path = Path(__file__).resolve().parent.parent / "src" / "ansys" / "dyna" / "core"
    doc_path = Path(__file__).resolve().parent.parent / "doc" / "source" / "api" / "ansys" / "dyna" / "core"

    autoapi = ManualRSTGenerator(namespace, module_path, doc_path)
    print(f"Generating RST files in {doc_path} from modules in {module_path}"
          )
    # exit(1)
    autoapi.generate_rst_for_manual_modules(auto_files=[])


def main():
    """Entry point for the script."""
    autodoc_extensions()


if __name__ == "__main__":
    main()