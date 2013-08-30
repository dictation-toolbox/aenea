import os

for name in os.listdir("E:\\aenea\\grammar"):
  if name.endswith(".py"):
    with open("E:\\aenea\\grammar\\%s" % name) as infd:
      with open("C:\\NatLink\\NatLink\\MacroSystem\\_%s" % name, "w") as outfd:
        outfd.write(infd.read())
for name in os.listdir("E:\\aenea\\util"):
  if name.endswith(".py"):
    with open("E:\\aenea\\util\\%s" % name) as infd:
      with open("C:\\NatLink\\NatLink\\MacroSystem\\%s" % name, "w") as outfd:
        outfd.write(infd.read())