import ast as parser, ast, re, sys

names = set()

for filename in sys.argv[1:]:
  print >> sys.stderr, "Processing ", filename
  tree = parser.parse(open(filename).read())
  for node in ast.walk(tree):
    if isinstance(node, ast.Name):
      names.add(node.id)
  
  parts = set()
  for name in names:
    for under_word in name.split("_"):
      if not under_word:
        continue
      under_word = under_word[0].lower() + under_word[1:]
      capitals = re.findall("[A-Z]", under_word)
      for letter in capitals:
        first, under_word =  under_word.split(letter, 1)
        parts.add(first.lower())
        under_word = letter + under_word
      parts.add(under_word.lower())
  
parts = list(parts)
parts.sort()
parts.sort(key=lambda value: len(value))
for part in parts:
  print part
