import struct, json, socket, sys, time, config

class Pocket:
  def __init__(self, sock):
    self.sock = sock

  def send(self, data):
    data = json.dumps(data)
    self.sock.send(struct.pack("<i", len(data)) + data)

  def recv(self):
    l = self.sock.recv(4)
    if len(l) != 4:
      raise socket.error()
    l = struct.unpack("<i", l)[0]
    return json.loads(self.sock.recv(l))

class RPCProxy(object):
  def __init__(self, comsat):
    self.comsat = comsat

  def __getattr__(self, key):
    if key.startswith("call"):
      def rpc(*a, **kw):
        self.comsat.pocket.send((key, a, kw))
        return self.comsat.pocket.recv()
      return rpc
    else:
      raise AttributeError(key)

class ComSat(object):
  def __init__(self):
    self.handlers = []
    self.pocket = None

  def serverMainLoop(self):
    try:
      self.sock = socket.socket()
      self.sock.bind((config.HOST, config.PORT))
      self.sock.listen(100)
      self.sock.setblocking(True)
  
      while True:
        try:
          self.pocket = Pocket(self.sock.accept()[0])
#          sys.stderr.write("server: Client connected.\n")
          try:
            while True:
              self.process()
              yield
          except socket.error:
            pass
#            sys.stderr.write("Lost connection.\n")
        finally:
          self.pocket.sock.close()
    finally:
      try:
        self.sock.close()
      except:
        pass

      try:
        self.sock.close()
      except:
        pass

  def clientConnect(self, host=config.HOST):
      while not self.pocket:
        conn = socket.socket()
        conn.connect((host, config.PORT))
        if conn is not None:
#          sys.stderr.write("client: Client connected.\n")
          self.pocket = Pocket(conn)
        else:
          sys.stderr.write("Could not connect.\n")
          time.sleep(1)

  def process(self):
    tup = self.pocket.recv()
    args = []
    kw = {}
    if len(tup) == 1:
      cmd, = tup
    elif len(tup) == 2:
      cmd, args = tup
    else:
      cmd, args, kw = tup

    if not cmd.startswith("call"):
      sys.stderr.write("Illegal cmd %s.\n" % cmd)
    else:
      for hand in self.handlers:
        fxn = getattr(hand, cmd, None)
        if fxn is not None:
          self.pocket.send(fxn(*args, **kw))
          break
      else:
        sys.stderr.write("Unhandled cmd %s.\n" % cmd)

  def getRPCProxy(self):
    return RPCProxy(self)

  def __exit__(self, type, value, traceback):
    self.disconnect()

  def __enter__(self):
    self.clientConnect()
    return self

  def disconnect(self):
    try:
      self.sock.close()
    except:
      pass

    try:
      self.pocket.sock.close()
    except:
      pass

    self.pocket = None
    self.sock = None
