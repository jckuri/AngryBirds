import math
from tkinter import *

class Point(object):
    def __init__(self,x=0,y=0):
        self.x=x
        self.y=y
    def __str__(self):
        return "("+str(self.x)+","+str(self.y)+")"
    def sum(self,point):
        return Point(self.x+point.x,self.y+point.y)
    def subtract(self,point):
        return Point(self.x-point.x,self.y-point.y)
    def magnitude(self):
        return math.sqrt(self.x**2+self.y**2)
    def angle(self):
        return math.atan2(self.y,self.x)
    def multiply(self,factor):
        return Point(factor*self.x,factor*self.y)
    def get_distance(self,point):
        return self.subtract(point).magnitude()
    def new_magnitude_same_angle(self,new_magnitude):
        angle=self.angle()
        return Point(math.cos(angle),math.sin(angle)).multiply(new_magnitude)

class Animated(object):
    explosion_time=20
    contact_radius=20
    def __init__(self,point,kind,tag):
        self.point=point
        self.kind=kind
        self.tag=tag
        self.counter=0
        self.status="normal"
    def move(self,game):
        pass
    def add_to_game(self,game):
        game.canvas.create_image(self.point.x,self.point.y,image=game.get_photo(self.kind),tags=self.tag)
    def iterate_destruction_variables(self):
        if self.status=="destroyed": return
        if self.counter>0:
            self.counter=self.counter-1
            self.status="explosion"
            if self.counter==0: self.status="destroyed"
    def repaint(self,game):
        game.canvas.coords(self.tag,self.point.x,self.point.y)
        if self.status=="explosion":
            game.canvas.itemconfig(self.tag,image=game.get_photo("E"))
        if self.status=="destroyed":
            game.canvas.itemconfig(self.tag,state="hidden")
    def is_touching(self,entity):
        return self.point.get_distance(entity.point)<=Animated.contact_radius
    def explode(self):
        self.counter=Animated.explosion_time

class Bird(Animated):
    gravity=0.1
    initial_point=Point(145,415)
    ground=475
    maximum_magnitude=100
    throwing_factor=0.15
    def __init__(self,point):
        super().__init__(point,"B","bird")
        self.point=point
        self.speed=Point()
        self.acceleration=Point()
        self.mouse_offset=None
        self.status="static"
        self.last_throw=None
    def move(self,game):
        if self.status=="normal":
            t=1
            self.point=self.point.sum(self.speed.multiply(t))
            self.speed=self.speed.sum(self.acceleration.multiply(t))
            if (self.point.y>Bird.ground)&(self.speed.y>0): self.explode()
        if self.status=="destroyed": self.reset_bird()
        self.iterate_destruction_variables()
    def touches_mouse(self,mouse):
        return self.point.get_distance(mouse)<=Animated.contact_radius/2.0
    def set_mouse_offset(self,mouse):
        self.mouse_offset=mouse.subtract(self.point)
    def get_new_point(self,mouse):
        if self.mouse_offset==None: return None
        new_point=mouse.subtract(self.mouse_offset)
        difference=Bird.initial_point.subtract(new_point)
        magnitude=difference.magnitude()
        if magnitude>Bird.maximum_magnitude:
            new_point=Bird.initial_point.subtract(difference.new_magnitude_same_angle(Bird.maximum_magnitude))
        return new_point
    def move_with_mouse(self,mouse):
        new_point=self.get_new_point(mouse)
        if new_point!=None: self.point=new_point
    def throw(self,mouse):
        new_point=self.get_new_point(mouse)
        if new_point==None: return
        difference=Bird.initial_point.subtract(new_point)
        self.status="normal"
        self.speed=difference.multiply(Bird.throwing_factor)
        self.acceleration=Point(0,Bird.gravity)
        self.last_throw=new_point
    def reset_bird(self):
        self.point=Bird.initial_point
        self.speed=Point()
        self.acceleration=Point()
        self.mouse_offset=None
        self.status="static"
    def repaint(self,game):
        game.canvas.coords(self.tag,self.point.x,self.point.y)
        if self.status=="explosion":
            game.canvas.itemconfig(self.tag,image=game.get_photo("E"))
        else:
            game.canvas.itemconfig(self.tag,image=game.get_photo(self.kind))
        if self.status=="destroyed":
            game.canvas.itemconfig(self.tag,state="hidden")
        else:
            game.canvas.itemconfig(self.tag,state="normal")
        if self.last_throw!=None:
            game.canvas.coords("last_throw",self.last_throw.x,self.last_throw.y)
    def add_to_game(self,game):
        game.canvas.create_image(self.point.x,self.point.y,image=game.get_photo(self.kind),tags=self.tag)
        game.canvas.create_image(self.point.x,self.point.y,image=game.get_photo(self.kind),tags="last_throw")

class Block(Animated):
    def __init__(self,i,j,kind):
        self.i=i
        self.j=j
        point=Point(i*30+18,j*30+18)
        tag="block"+str(i)+","+str(j)
        super().__init__(point,kind,tag)
    def move(self,game):
        if (self.status=="normal")&(game.bird.status=="normal")&(self.is_touching(game.bird)):
                if self.kind=="R": game.bird.explode()
                if self.kind=="W":
                    game.bird.explode()
                    self.explode()
                if (self.kind=="G")|(self.kind=="P"): self.explode()
        self.iterate_destruction_variables()

class Level(object):
    def __init__(self,level):
            self.blocks=[]
            self.read_level(self.get_file_name(level))
    def get_block(self,i,j,character):
        if character in ('P','G','R','W'):
            return Block(i,j,character)
        return None
    def process_line(self,line,j):
        n=min(len(line),34)
        for i in range(0,n):
            block=self.get_block(i,j,line[i])
            if block!=None:
                self.blocks=self.blocks+[block]
    def read_level(self,filename):
        file=open(filename)
        j=0
        line=file.readline()
        while line:
            self.process_line(line,j)
            j=j+1
            if j>=16: break
            line=file.readline()
        file.close()
    def get_level_string(self,level):
        level_string=str(level)
        length=len(level_string)
        if length==1: level_string="00"+level_string
        if length==2: level_string="0"+level_string
        return level_string
    def get_file_name(self,level):
        return "level-"+self.get_level_string(level)+".txt"

class Game(object):
    images=["angry-bird.gif","pig.gif","glass.gif","rock.gif","wood.gif","explosion.gif"]
    kinds={"B":0,"P":1,"G":2,"R":3,"W":4,"E":5}
    pause=1
    def __init__(self):
        self.root=Tk()
        self.background=PhotoImage(file="background.gif")
        self.width=self.background.width()
        self.height=self.background.height()        
        self.canvas=Canvas(width=self.width,height=self.height)
        self.canvas.pack()
        self.photos=[]
        for image in Game.images:
            self.photos=self.photos+[PhotoImage(file=image)]
        self.level=1
        self.load_level()        
        self.canvas.bind("<Button-1>",self.button1)
        self.canvas.bind("<B1-Motion>",self.motion1)
        self.canvas.bind("<ButtonRelease-1>",self.release1)
        self.tick()
        self.root.mainloop()
    def button1(self,event):
        mouse=Point(event.x,event.y)
        self.bird.mouse_offset=None
        if self.bird.touches_mouse(mouse):
            self.bird.set_mouse_offset(mouse)
    def motion1(self,event):
        mouse=Point(event.x,event.y)
        self.bird.move_with_mouse(mouse)
    def release1(self,event):
        mouse=Point(event.x,event.y)
        self.bird.throw(mouse)
    def tick(self):
        self.move_entities()
        self.repaint_entities()
        self.canvas.update()
        self.root.after(Game.pause,self.tick)
    def load_level(self,):
        self.canvas.delete(ALL)
        self.canvas.create_image(self.width/2.0,self.height/2.0,image=self.background,tags="background")
        level=Level(self.level)
        self.bird=Bird(Bird.initial_point)
        self.entities=level.blocks+[self.bird]
        for entity in self.entities:
            entity.add_to_game(self)
        self.canvas.update()
    def move_entities(self):
        for entity in self.entities:
            entity.move(self)
        if not(self.is_there_an_alive_pig()):
            self.level=self.level+1
            self.load_level()
    def repaint_entities(self):
        for entity in self.entities:
            entity.repaint(self)
    def get_photo(self,kind):
        return self.photos[Game.kinds[kind]]
    def is_there_an_alive_pig(self):
        exists=0
        for entity in self.entities:
            if (entity.kind=="P")&(entity.status!="destroyed"): exists=1
        return exists
                
game=Game()
