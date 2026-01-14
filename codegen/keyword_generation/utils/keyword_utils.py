# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Utilities for keyword name processing and filtering.

Consolidates repeated logic for transforming keyword names and filtering by domain.
"""

from dataclasses import dataclass
from typing import Dict, List, Optional

from keyword_generation.utils import fix_keyword, get_classname
from keyword_generation.utils.domain_mapper import get_keyword_domain


@dataclass
class KeywordNames:
    """
    Processed keyword naming information.

    Consolidates the common pattern of transforming a keyword name into
    its various representations (classname, filename, domain, etc.).

    Attributes
    ----------
        keyword: Original keyword string (e.g., "SECTION_SHELL_TITLE")
        classname: Python class name (e.g., "SectionShellTitle")
        filename: Output filename stem (e.g., "section_shell_title")
        fixed_keyword: Normalized keyword (e.g., "SECTION_SHELL_TITLE")
        domain: Keyword domain (e.g., "section", "contact", "mat")
    """

    keyword: str
    classname: str
    filename: str
    fixed_keyword: str
    domain: str

    @classmethod
    def from_keyword(cls, keyword: str, keyword_options: Optional[Dict] = None) -> "KeywordNames":
        """
        Process a keyword string into all its name representations.

        Args:
            keyword: Raw keyword string
            keyword_options: Optional options dict that may contain custom classname

        Returns
        -------
            KeywordNames instance with all name variants
        """
        fixed_kwd = fix_keyword(keyword)

        # Get classname from options or generate it
        # Use original keyword (not fixed) to preserve hyphen distinction in classnames
        if keyword_options and "classname" in keyword_options:
            classname = keyword_options["classname"]
        else:
            classname = get_classname(keyword)

        filename = fixed_kwd.lower()
        domain = get_keyword_domain(keyword)

        return cls(
            keyword=keyword,
            classname=classname,
            filename=filename,
            fixed_keyword=fixed_kwd,
            domain=domain,
        )


def filter_keywords_by_domain(keywords: List[Dict], subset_domains: Optional[List[str]] = None) -> List[Dict]:
    """
    Filter keywords by domain membership.

    This eliminates the duplicated domain filtering logic that appears in multiple
    places in generate.py.

    Args:
        keywords: List of keyword dicts (each must have 'name' key)
        subset_domains: Optional list of domain names to include (e.g., ["contact", "section"])
                       If None, all keywords are returned.

    Returns
    -------
        Filtered list of keywords

    Example:
        >>> keywords = [{"name": "CONTACT_..."}, {"name": "SECTION_..."}]
        >>> filtered = filter_keywords_by_domain(keywords, ["contact"])
        >>> # Returns only CONTACT_ keywords
    """
    if subset_domains is None:
        return keywords

    filtered = []
    for kwd in keywords:
        domain = get_keyword_domain(kwd["name"])
        if domain in subset_domains:
            filtered.append(kwd)

    return filtered
