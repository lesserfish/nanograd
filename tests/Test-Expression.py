import torch

# Forward Pass
a = torch.Tensor([4.0]).double()
b = torch.Tensor([7.0]).double()
c = torch.Tensor([16.0]).double()
a.requires_grad = True
b.requires_grad = True
c.requires_grad = True


d = torch.exp(a*b - c**2)
e = (d + 1/a).relu()
f = torch.sin(0.3 * e)


# Forward Pass
output = f.item()
print(output)

# Backpropagation
a = torch.Tensor([1.0]).double()
b = torch.Tensor([1.0]).double()
c = torch.Tensor([4.0]).double()
a.requires_grad = True
b.requires_grad = True
c.requires_grad = True
d = torch.sin(a + b - c**2)
e = (d + 1/a).relu()
f = torch.exp(0.3 * e)
f.backward()

(xa, xb, xc) = (a.grad.item(), b.grad.item(), c.grad.item())
print((xa, xb, xc))


# Backpropagation
a = torch.Tensor([0.3]).double()
b = torch.Tensor([0.1]).double()
c = torch.Tensor([1.0]).double()
a.requires_grad = True
b.requires_grad = True
c.requires_grad = True

d = torch.sinh(a +  torch.cosh(c**2))
e = (b + d + torch.sin(a)).relu()
f = torch.cos(e - 1/a)
g = torch.sin(f ** 3)
h = torch.log(torch.atanh(g))
h.backward()

(xa, xb, xc) = (a.grad.item(), b.grad.item(), c.grad.item())
print((xa, xb, xc))


# Backpropagation
a = torch.Tensor([0.3]).double()
b = torch.Tensor([0.5]).double()
a.requires_grad = True
b.requires_grad = True
c = torch.asin(a * b)
d = torch.cos (c ** 2)
e = torch.tanh (d)
e.backward()

(xa, xb) = (a.grad.item(), b.grad.item())
print((xa, xb))
