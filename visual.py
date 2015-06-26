from visual import *
scene.fullscreen=True
s=[]
params=open('boxparams.dat','r')
n=int(params.readline())
l=float(params.readline())
h=float(params.readline())
w=float(params.readline())
r=float(params.readline())
print n,l,w,h,r
num_lines = sum(1 for line in open('xyz.dat'))
loop=num_lines/n
f=open('xyz.dat','r')
box(pos=[l/2,h/2,w/2],length=l,height=h,width=w,opacity=0.3,color=color.white)
for i in range(0,n):
	line=f.readline()
	row = line.split()
	x = float(row[0])
	y = float(row[1])
	z = float(row[2])
	s.append(sphere(pos=[x,y,z],radius=r,color=color.cyan,make_trail=False))
s[1].color=color.red
s[2].color=color.black
s[3].color=color.green
s[4].color=color.blue
s[1].make_trail=True

for i in range(1,loop):
	rate(10000)
	for j in range(0,n):
		line=f.readline()
		row = line.split()
		x = float(row[0])
		y = float(row[1])
		z = float(row[2])
		s[j].pos = [x,y,z]
