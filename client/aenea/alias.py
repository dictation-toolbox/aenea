# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (2014) David J. Rosenbaum
# David J. Rosenbaum <djr7c4@gmail.com>

import copy
import re

try:
    import dragonfly
except ImportError:
    import dragonfly_mock as dragonfly


def _product(choices_product):
    if len(choices_product) == 0:
        raise Exception("Error: choices_product is empty")
    elif len(choices_product) == 1:
        return choices_product[0]
    else:
        choices = []
        
        for choice in choices_product[0]:
            for text in _product(choices_product[1:]):
                choices.append(choice + " " + text)

        return choices

def product(choices_product):
    """Return concatenation of each element of the direct product."""
    choices_product = list(map(list, choices_product))
    return _product(choices_product)

def normalize_whitespace(text):
    return " ".join(text.split())

class Alias(object):
    """A mapping each string to a list of aliases."""
    def __init__(self, aliases = []):
        self._map = {}
        self._rmap = {}
        self._regex = None

        self.update(aliases)

    def contains_string(self, string):
        return string in self._map

    def strings(self):
        return self._map.keys()

    def aliases(self):
        return self._map.values()
    
    def alias(self, string):
        return copy.copy(self._map[string])

    def string(self, alias):
        return self._rmap[alias]
        
    def __contains__(self, string):
        return self.contains_string(string)

    def __iter__(self):
        for string in self._map:
            yield (string,) + tuple(self[string])
    
    def __getitem__(self, string):
        """Get the aliases for string."""
        return tuple(self._map[string])

    def __or__(self, other):
        obj = self.__class__()
        obj.update(self)
        obj.update(other)
        return obj

    def get(self, string, default = None):
        return self._map.get(string, default)
        
    def update(self, aliases):
        """aliases should be an iterable of tuples where the first element is the primary string and the others are aliases for it."""
        for strings in aliases:
            assert isinstance(strings, tuple)
            string = strings[0]
            alias_strings = strings[1:]
            self.add(string, alias_strings)

    def _cleanup(self, s):
        return " ".join(s.split())
    
    def add(self, string, alias_strings):
        """Add the iterable of aliases for string to this object."""
        self._regex = None

        if string not in self._map:
            self._map[string] = []

        for alias in alias_strings:
            if alias not in self._rmap:
                # Only add aliases that are not already present in order to avoid duplicate entries.
                self._map[string].append(alias)
                self._rmap[alias] = string
        
    def discard(self, string_or_alias):
        """Remove string_or_alias if it is present."""
        self._regex = None
        sora = string_or_alias
        
        if sora in self._map:
            for alias in self._map[sora]:
                del self._rmap[alias]
                
            del self._map[string_or_alias]

        if sora in self._rmap:
            string = self._rmap[sora]
            self._map[string].remove(sora)
            del self._rmap[sora]

    def _update_regex(self):
        if not self._regex:
            self._regex = re.compile(r"(?:^|[ \[\(])(?P<string>" + r"|".join(sorted(self.strings(), key = lambda s: (len(s.split()), s), reverse = True)) + r")(?:$|[ \]\)])")

    def spec_for_word(self, word):
        if word in self:
            return "(" + " | ".join([word] + list(self[word])) + ")"
        else:
            return word

    def spec_for_words(self, string):
        """Return a dragonfly spec string for any string that can be obtained from string by making substitutions for individual words in string."""
        return " ".join(map(self.spec_for_word, string.split()))
                
    def spec_for_string(self, string):
        """Return a dragonfly spec string for any string that can be obtained from string by making substitutions."""
        assert string in self
        alias_strings = self[string]

        if len(alias_strings) == 0:
            return string
        else:
            return "(" + " | ".join([string] + list(map(self.spec_for_words, alias_strings))) + ")"

    def split(self, text):
        """Find all substrings in text that are strings in this object.  Return an iterable of all such strings that the non-matching strings between them in the order they are encountered."""
        self._update_regex()
        k = 0 # The end of the previous match.
        open_angle_brackets = 0
        
        while True:
            m = self._regex.search(text[k:])

            if not m:
                if text[k:] != "":
                    yield text[k:]
                    
                break
            
            i, j = m.start("string"), m.end("string")
            i += k
            j += k
            open_angle_brackets += text[k:i].count("<") - text[k:i].count(">")
            assert open_angle_brackets >= 0

            # Ignore anything inside angle brackets
            if open_angle_brackets == 0:
                if text[k:i] != "":
                    yield text[k:i]

                if text[i:j] != "":
                    yield text[i:j]

            k = j
        
    def spec(self, spec):
        """Return a dragonfly spec string that allows aliases to be used instead of strings in spec."""
        new_spec = ""

        def ensure_space(s):
            if len(s) > 0 and s[-1] != " ":
                s += " "

            return s
        
        for substr in self.split(spec):
            new_spec = ensure_space(new_spec)

            if substr in self:
                new_spec += self.spec_for_string(substr)
            else:
                new_spec += substr

        return normalize_whitespace(new_spec)

    def make_mapping_spec(self, mapping):
        return {self.spec(spec) : value for spec, value in dict(mapping).items()}

    def choices_for_word(self, word):
        choices = [word]
        
        if word in self:
            choices += list(self[word])

        return choices
    
    def choices_for_words(self, string):
        choices_product = []
        
        for word in string.split():
            if word in self:
                choices_product.append(self.choices_for_word(word))
            else:
                choices_product.append([word])

        return product(choices_product)

    def choices_for_string(self, string):
        assert string in self
        choices = [string]

        for alias in self[string]:
            choices += self.choices_for_words(alias)

        return choices
        
    def substitute(self, text):
        """Return the strings that can be obtained from phrase by performing substitutions."""
        choices_product = []
            
        for substr in self.split(text):
            if substr in self:
                choices = list(self.choices_for_string(substr))
                assert choices
                choices_product.append(choices)
            else:
                choices_product.append([substr])
        
        return list(map(normalize_whitespace, product(choices_product)))
        
    def make_mapping(self, mapping):
        mapping = dict(mapping)
        new_mapping = dict(mapping)
        
        for string in mapping:
            for equivalent_text in self.substitute(string):
                new_mapping[equivalent_text] = mapping[string]

        return new_mapping

    def make_alternative(self, literal, **kwargs):
        return dragonfly.Alternative([dragonfly.Literal(equivalent_text) for equivalent_text in self.substitute(literal)] , **kwargs)
