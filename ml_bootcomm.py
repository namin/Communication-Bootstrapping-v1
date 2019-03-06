nouns = "bob jim mary icepick shovel table lab".split()
verbs = "move approach retreat touch eat fear".split()
roles = "subject object instrument place".split()

# nouns = """bob jim mary icepick shovel table lab fred bill classroom
# leg cup butterfly dog cat turtle door window car hammer
# keyboard coffee danish pencil pen eraser wall socket book ed
# wheel chainsaw gun kite bedroom shower beach shoe light dark
# hat office house apple banana flea vampire stapler kim joe""".split()
# verbs = """move approach retreat touch eat fear zap feel fly throw
# catch push hit stab tickle hurt love hate want ignite""".split()

total_nouns = len(nouns)
total_verbs = len(verbs)
total_roles = len(roles)

input_size = total_verbs+(total_nouns*total_roles)
num_classes = input_size
num_epochs = 50000
learning_rate = 0.005
num_tests = 100

import random

def generate_features():
  features = []
  for i in range(random.randint(2, 4)):
    features.append((nouns[random.randint(0, total_nouns-1)]+" "+
                     roles[random.randint(0, total_roles-1)]))
  for i in range(random.randint(0, 2)):
    features.append((verbs[random.randint(0, total_verbs-1)]+" "+
                     "verb"))
  return features

def random_unused(m, x):
  k = random.randint(0, input_size-1)
  if m[k]:
    k = m.index(None)
  m[k] = x
  return k

def encode_sentence(m, s):
  t = torch.zeros(input_size)
  for x in s:
    try:
      k = m.index(x)
    except ValueError:
      k = random_unused(m, x)
    t[k] = 1.0
  return t

def init_map():
  return [None for i in range(input_size)]

import torch
import torch.nn as nn

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

class AutoEncoder(nn.Module):
  def __init__(self, input_size, num_classes):
    super(AutoEncoder, self).__init__()
    self.encoder = nn.Sequential(
      nn.Linear(input_size, num_classes),
      nn.Sigmoid())
    self.decoder = nn.Sequential(
      nn.Linear(num_classes, input_size),
      nn.Sigmoid())
    self.m = init_map()
    self.m_in = self.m
    self.m_out = self.m

  def forward(self, x):
    x = self.encoder(x)
    x = self.decoder(x)
    return x

class Comm(nn.Module):
  def __init__(self, a, b):
    super(Comm, self).__init__()
    self.encoder = a.encoder
    self.decoder = b.decoder
    self.m_in = a.m
    self.m_out = b.m

  def forward(self, x):
    x = self.encoder(x)
    x = self.decoder(x)
    return x

autoencoder1 = AutoEncoder(input_size, num_classes).to(device)
autoencoder2 = AutoEncoder(input_size, num_classes).to(device)
comm12 = Comm(autoencoder1, autoencoder2).to(device)
comm21 = Comm(autoencoder2, autoencoder1).to(device)

criterion = nn.MSELoss().to(device)
optimizer11 = torch.optim.Adam(autoencoder1.parameters(), lr=learning_rate)
optimizer22 = torch.optim.Adam(autoencoder2.parameters(), lr=learning_rate)
optimizer12 = torch.optim.Adam(comm12.parameters(), lr=learning_rate)
optimizer21 = torch.optim.Adam(comm21.parameters(), lr=learning_rate)

def opt(features, comm, optimizer):
  optimizer.zero_grad()
  input = encode_sentence(comm.m_in, features).to(device)
  output = comm(input)
  target = encode_sentence(comm.m_out, features).to(device)
  loss = criterion(target, output)
  loss.backward()
  optimizer.step()

def train1():
  features = generate_features()
  if random.randint(0, 1)==0:
    opt(features, comm12, optimizer12)
  else:
    opt(features, comm21, optimizer21)
  opt(features, autoencoder1, optimizer11)
  opt(features, autoencoder2, optimizer22)

def train(num_epochs=num_epochs):
  for epoch in range(num_epochs):
    train1()

def test1(net=None):
  correct = 0
  for i in range(num_tests):
    features = generate_features()
    comm = net
    if net is None:
      comm = comm12 if random.randint(0, 1)==0 else comm21
    input = encode_sentence(comm.m_in, features)
    output = comm(input)
    sample = torch.distributions.Binomial(probs=output).sample()
    target = encode_sentence(comm.m_out, features)
    loss = sum(abs(sample-target))
    if 0 <= loss and loss < 1:
      correct += 1
  return correct, num_tests

def test():
  correct, total = test1()
  print('Accuracy of the comm network: %d %%' % (100 * correct / total))
  correct, total = test1(autoencoder1)
  print('Auto-understanding 1: %d %%' % (100 * correct / total))
  correct, total = test1(autoencoder2)
  print('Auto-understanding 2: %d %%' % (100 * correct / total))

if __name__ == '__main__':
  train()
  test()
